(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Tue May 21 16:16:31 PDT 1996 by mcjones *)

MODULE FromPS;

IMPORT Atom, BBoxSeq, Bundle, File, FileRd, FileWr, Fmt, FromPSBundle,
  FS, OCR_PS, OSError, OSUtils, Pipe, Process, Rd, TempFiles, TextSeq,
  Thread, Word, Wr;

CONST NSlots = 4;

REVEAL T = Public BRANDED OBJECT
    fifoPath, ocrPath, optionsPath, rotPath: TEXT;
    prGs: Process.T;
    imageRd, ocrRd: Rd.T;
    ocr_ps: OCR_PS.T;
    verbose: BOOLEAN;
    mu: MUTEX;
    cv: Thread.Condition;
    words: ARRAY [0..NSlots-1] OF TextSeq.T;
    bBoxes: ARRAY [0..NSlots-1] OF BBoxSeq.T;
    in: INTEGER; (* next slot for producer when (in+1)MOD NSlots # out *)
    out: INTEGER;(* next slot for consumer when in # out *)
  OVERRIDES
    init := Init;
    nextImage := NextImage;
    nextOCR := NextOCR
  END;

CONST ImageTypeName = ARRAY ImageType OF TEXT {
  "pbmraw", "pgmraw", "ppmraw"};

PROCEDURE Init(
    t: T;
    file: File.T;
    imageType := ImageType.PBMRAW;
    orientation := Orientation.Portrait;
    resolution := 300;
    ocr := TRUE;
    gs: TEXT := NIL;
    verbose := FALSE)
  : T RAISES {Error, Thread.Alerted} =
  VAR
    params := NEW(TextSeq.T).init();
    stdin, stdout, stderr: File.T;
    pipe1R, pipe1W: Pipe.T;
  BEGIN
    t.verbose := verbose;
    IF gs = NIL THEN gs := "gs" END;

    t.fifoPath := TempFiles.Get("/tmp", ",BuildLectern-fifo-");
    TRY OSUtils.CreateFifo(t.fifoPath)
    EXCEPT OSError.E => RAISE Error("Error creating temporary " & t.fifoPath)
    END;
    TempFiles.Note(t.fifoPath);

    (* Build Ghostscript parameter list. *)
(*  params.addhi("-dNOGC");*) (* ***** disable Level 2 GC until performance problems found *)
    params.addhi("-sDEVICE=" & ImageTypeName[imageType]);
    params.addhi("-r" & Fmt.Int(resolution));
    params.addhi("-dOpenOutputFile"); (* introduced in Ghostscript 3.21 *)
    params.addhi("-sOutputFile=" & t.fifoPath);
    params.addhi("-dNOPAUSE");
    params.addhi("-dSAFER");
    params.addhi("-q");
    TRY
      t.optionsPath := CopyTextToTemp(
        Bundle.Get(FromPSBundle.Get(), "options.ps"), "options.ps");
      params.addhi(t.optionsPath);
      IF ocr THEN
        t.ocrPath := CopyTextToTemp(
          Bundle.Get(FromPSBundle.Get(), "ocr.ps"), "ocr.ps");
        params.addhi(t.ocrPath);
      ELSE
         t.ocrPath := NIL
      END;
      IF orientation = Orientation.Landscape THEN
        t.rotPath := CopyTextToTemp(
          Bundle.Get(FromPSBundle.Get(), "rot270.ps"), "rot270.ps");
        params.addhi(t.rotPath)
      ELSIF orientation = Orientation.LandscapeOther THEN
        t.rotPath := CopyTextToTemp(
          Bundle.Get(FromPSBundle.Get(), "rot90.ps"), "rot90.ps");
        params.addhi(t.rotPath)
      ELSE
        t.rotPath := NIL
      END;
    EXCEPT OSError.E, Wr.Failure => 
      RAISE Error("Can't copy temporary file to /tmp")
    END;
    params.addhi("-c"); params.addhi("1000000"); params.addhi("setvmthreshold");
    params.addhi("-"); (* take input from stdin *)
    (* In principle, we should undo the save done in options.ps here. *)

    (* Start Ghostscript process. *)
    Process.GetStandardFileHandles(stdin, stdout, stderr);
    TRY Pipe.Open(pipe1R, pipe1W)
    EXCEPT OSError.E => RAISE Error("Can't create interprocess pipe")
    END;
    TRY
      t.prGs := Process.Create(
        cmd := gs,
        params := ToTextArray(params)^,
        stdin := file,
        stdout := pipe1W,
        stderr := stderr)
    EXCEPT OSError.E(code) =>
      RAISE Error("Can't create Ghostscript process: " 
                  & Atom.ToText(code.head))
    END;
    TRY pipe1W.close() EXCEPT OSError.E => END;

    IF ocr THEN
      TRY t.ocrRd := NEW(FileRd.T).init(pipe1R)
      EXCEPT OSError.E => RAISE Error("Can't open OCR reader")
      END;
      t.ocr_ps := NEW(OCR_PS.T).init(t.ocrRd, resolution);
      t.mu := NEW(MUTEX);
      t.cv := NEW(Thread.Condition);
      FOR i := 0 TO LAST(t.words) DO
        t.words[i] := NEW(TextSeq.T).init();
        t.bBoxes[i] := NEW(BBoxSeq.T).init()
      END;
      t.in := 0; t.out := 0;
      EVAL Thread.Fork(NEW(OCRThread, t := t))
    END;
    TRY
      t.imageRd := NEW(FileRd.T).init(OSUtils.OpenFifo())
    EXCEPT OSError.E => RAISE Error("Can't open image reader")
    END;

    RETURN t
  END Init;

TYPE OCRThread = Thread.Closure OBJECT
    t: T;
  OVERRIDES
    apply := OCRThreadApply
  END;

PROCEDURE NextImage(t: T; VAR (*OUT*) rd: Rd.T): BOOLEAN
  RAISES {Error, Thread.Alerted} =
  <*FATAL Rd.EndOfFile*>
  VAR e: INTEGER;
  BEGIN
    (* Skip initial white space, and terminate on EOF. *)
    TRY
      LOOP
        IF Rd.EOF(t.imageRd) THEN
          (* Finalize "t". *)
          Rd.Close(t.imageRd);
          (* Don't close "t.ocrRd" since another thread's reading it. *)
          TRY
            IF t.ocrPath # NIL THEN
              TempFiles.Forget(t.ocrPath); FS.DeleteFile(t.ocrPath)
            END;
            TempFiles.Forget(t.optionsPath); FS.DeleteFile(t.optionsPath);
            IF t.rotPath # NIL THEN
              TempFiles.Forget(t.rotPath); FS.DeleteFile(t.rotPath)
            END;
            TempFiles.Forget(t.fifoPath); OSUtils.DeleteFifo()
          EXCEPT OSError.E => RAISE Error("Can't delete temporary file")
          END;
          e := Process.Wait(t.prGs);
          IF e # 0 THEN
            RAISE Error("Ghostscript exited with code of " & Fmt.Int(e))
          END;
          RETURN FALSE
        END;
        WITH c = Rd.GetChar(t.imageRd) DO
          IF c # ' ' AND c # '\n' THEN
            Rd.UnGetChar(t.imageRd); EXIT
          END
        END
      END
    EXCEPT Rd.Failure => RAISE Error("Error reading image")
    END;
    rd := t.imageRd;
    RETURN TRUE
  END NextImage;

PROCEDURE NextOCR(
    t: T;
    wr: Wr.T;
    <*UNUSED*>height: CARDINAL;
    VAR (*OUT*) nWords, nWordsBytes: CARDINAL
    ) RAISES {Thread.Alerted, Wr.Failure} =
  VAR n0, nW: INTEGER;
  BEGIN
    LOCK t.mu DO
      WHILE t.out = t.in DO Thread.Wait(t.mu, t.cv) END
    END;
    WITH n = t.words[t.out].size() DO
      <*ASSERT n = t.bBoxes[t.out].size()*>
      n0 := Wr.Index(wr);
      nW := 0;
      FOR i := 0 TO n - 1 DO
        WITH w = t.words[t.out].remlo() DO
          Wr.PutText(wr, w); Wr.PutChar(wr, '\n');
          INC(nW)
        END
      END;
      nWords := nW;
      nWordsBytes := Wr.Index(wr) - n0;
      FOR i := 0 TO n - 1 DO
        WITH b = t.bBoxes[t.out].remlo() DO
          WriteInt4(wr, b.blx); WriteInt4(wr, b.bly);
          WriteInt4(wr, b.trx); WriteInt4(wr, b.try)
        END
      END
    END;
    LOCK t.mu DO
      INC(t.out); IF t.out = NUMBER(t.words) THEN t.out := 0 END;
      Thread.Signal(t.cv)
    END;
  END NextOCR;

PROCEDURE CopyTextToTemp(t: TEXT; ext: TEXT)
  : TEXT RAISES {OSError.E, Thread.Alerted, Wr.Failure} =
(* Create temporary file, copy "t" to it, and return its
   pathname. *)
  VAR
    p := TempFiles.Get(NIL, ",BuildLectern-" & ext & "-");
    wr := FileWr.Open(p);
  BEGIN
    TempFiles.Note(p);
    TRY
      Wr.PutText(wr, t & Wr.EOL)
    FINALLY Wr.Close(wr)
    END;
    RETURN p
  END CopyTextToTemp;

PROCEDURE OCRThreadApply(t: OCRThread): REFANY =
  VAR inNew: INTEGER;
  BEGIN
    LOOP
      LOCK t.t.mu DO
        inNew := t.t.in + 1; IF inNew = NUMBER(t.t.words) THEN inNew := 0 END;
        WHILE inNew = t.t.out DO Thread.Wait(t.t.mu, t.t.cv) END;
      END;
      IF NOT t.t.ocr_ps.nextPage(t.t.words[t.t.in], t.t.bBoxes[t.t.in]) THEN
        RETURN NIL
      END;
      LOCK t.t.mu DO
        t.t.in := inNew; Thread.Signal(t.t.cv)
      END
    END
  END OCRThreadApply;

PROCEDURE ToTextArray(s: TextSeq.T): REF ARRAY OF TEXT =
(* Return a new array containing "s.getlo()", ..., "s.gethi()". *)
  VAR r := NEW(REF ARRAY OF TEXT, s.size());
  BEGIN
    FOR i := 0 TO LAST(r^) DO r[i] := s.get(i) END;
    RETURN r
  END ToTextArray;

PROCEDURE WriteInt4(wr: Wr.T; n: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
(* Write an integer represented as four bytes on wr, L.S. first. *)
  BEGIN
    FOR i := 0 TO 3 DO
      Wr.PutChar(wr, VAL(Word.Extract(n, i*8, 8), CHAR));
    END;
  END WriteInt4;

BEGIN
END FromPS.
