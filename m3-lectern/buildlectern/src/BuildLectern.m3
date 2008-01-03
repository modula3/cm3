(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: a user interface for viewing documents stored as images *)
(* Program for building a multi-page file from single-page image files *)

(* Last modified on Fri Jun  7 14:18:52 PDT 1996 by mcjones   *)
(*      modified on Wed May  3 16:38:12 PDT 1995 by birrell   *)

MODULE BuildLectern EXPORTS Main;

IMPORT Atom, File, FileRd, FileWr, FloatMode, Fmt, FromPS, FS,
       ImageRd, Images, LecternDoc, Lex, LGM, OCR, OSError, OSUtils,
       Params, Pathname, Pipe, Process, Rd, RdClass, RegularFile, Scan, Stdio,
       TempFiles, Text, TextWr, Thread, Time, Wr;

(* *)
(******** Version ********)
(* *)

CONST Version = "BuildLectern 1.2.7 of 5th June 1996";


(* *)
(* Synchronization note:  there isn't any - there's only one thread *)
(* *)


(* *)
(* Subroutines *)
(* *)

EXCEPTION
  Error(TEXT);
  MissingParam;


PROCEDURE ForceLower(t: TEXT): TEXT =
  (* Return "t", with upper-case letters forced to lower-case. *)
  VAR wr := TextWr.New();
  <*FATAL Thread.Alerted, Wr.Failure*>
  BEGIN
    FOR i := 0 TO Text.Length(t) - 1 DO
      WITH c = Text.GetChar(t, i) DO
        IF 'A' <= c AND c <= 'Z' THEN
          Wr.PutChar(wr, VAL(ORD(c)-ORD('A')+ORD('a'), CHAR));
        ELSE
          Wr.PutChar(wr, c);
        END;
      END;
    END;
    RETURN TextWr.ToText(wr);
  END ForceLower;

PROCEDURE ParamInt(VAR p: INTEGER): INTEGER RAISES { Error, MissingParam } =
  (* Read parameter number "p" as an integer, and return it.  Increments "p" *)
  VAR t: TEXT;
  BEGIN
    IF p < Params.Count-1 THEN
      t := Params.Get(p);
      INC(p);
      TRY
        RETURN Scan.Int(t)
      EXCEPT Lex.Error, FloatMode.Trap =>
        RAISE Error("expected an integer instead of \"" & t & "\"");
      END;
    ELSE
      RAISE MissingParam;
    END;
  END ParamInt;

PROCEDURE ParamReal(VAR p: INTEGER): REAL RAISES { Error, MissingParam } =
  (* Read parameter number "p" as a real, and return it.  Increments "p" *)
  VAR t: TEXT;
  BEGIN
    IF p < Params.Count-1 THEN
      t := Params.Get(p);
      INC(p);
      TRY
        RETURN Scan.Real(t)
      EXCEPT Lex.Error, FloatMode.Trap =>
        RAISE Error("expected a number instead of \"" & t & "\"");
      END;
    ELSE
      RAISE MissingParam;
    END;
  END ParamReal;


(* *)
(* Converting one image to its components *)
(* *)

VAR
  unscaledImage: LGM.T := NIL; (* used only by PageToComponents *)
  ocrError := FALSE;

PROCEDURE PageToComponents(file: RegularFile.T;
                           from: LecternDoc.Component;
                           READONLY scales: LecternDoc.Scales;
                           gamma: REAL;
                           wr: Wr.T; VAR page: LecternDoc.DirPage;
                           verbose: BOOLEAN;
                           VAR (*OUT*) height: CARDINAL)
                           RAISES { Wr.Failure, Thread.Alerted, Error } =
  (* Write components for one image.  "file" is in LGM or PNM format.
     Components are written iff the corresponding entry of "scales" is
     non-zero; scales[OCRWords] controls OCR (1 => real OCR; 0 => no OCR; -1 => fakeOCR);
     scales[OCRRects] is ignored.
   *)
  PROCEDURE WriteOne(class: LecternDoc.Class;
                     thisGamma: REAL; levels: INTEGER; diffuse: BOOLEAN)
                    RAISES { LGM.Error, Wr.Failure, Thread.Alerted } =
    VAR
      scale := scales[class];
      start, end: Time.T;
      processed: Images.Contents;
    BEGIN
      IF scale = 0 THEN
        page[class] := LecternDoc.Component{};
      ELSE
        page[class].start := Wr.Index(wr);
        start := Time.Now();
        processed := LGM.Reduce(unscaledImage, scale,
                                thisGamma,
                                levels,
                                diffuse,
                                verbose);
        LGM.Compress(processed, scale, wr, verbose);
        end := Time.Now();
        page[class].length := Wr.Index(wr) - page[class].start;
        IF verbose THEN
          Wr.PutText(Stdio.stderr,
                     Fmt.Int(page[class].length) & " bytes, " &
                     Fmt.LongReal(end-start, Fmt.Style.Fix, 1) &
                     " seconds.\n");
          Wr.Flush(Stdio.stderr);
        END;
      END;
    END WriteOne;
  BEGIN
    TRY
      LGM.ReadImage(file, from.start, from.length, unscaledImage);
      WriteOne(LecternDoc.Class.Print, gamma, 4, TRUE);
      height := LGM.GetHeight(unscaledImage);
      WriteOne(LecternDoc.Class.Large, gamma, 4, TRUE);
      WriteOne(LecternDoc.Class.Normal, gamma, 4, TRUE);
      WriteOne(LecternDoc.Class.Small, gamma, 4, TRUE);
      WriteOne(LecternDoc.Class.Thumbnail, 0.2, 4, FALSE);
    EXCEPT LGM.Error(msg) => RAISE Error(msg);
    END;
    IF scales[LecternDoc.Class.OCRWords] # 1 OR NOT OCR.IsFromPBMImplemented() THEN
      page[LecternDoc.Class.OCRWords] := LecternDoc.Component{};
      page[LecternDoc.Class.OCRRects] := LecternDoc.Component{};
    ELSE
      WITH
	words = page[LecternDoc.Class.OCRWords],
	rects = page[LecternDoc.Class.OCRRects] DO
	VAR
	  nWords, nWordsBytes: CARDINAL;
	  start, end: Time.T;
	  format := LGM.GetFormat(unscaledImage);
	  cmd, cmd2: TEXT := NIL; (* filter commands *)
	  pipe1, pipe2: Pipe.T := NIL;
	  tempRd, ocrRd: Rd.T := NIL;
	BEGIN
	  CASE format OF
	  | LGM.Format.PBM => (* no filter needed *)
	  | LGM.Format.PGM => cmd := "pgmtopbm";
	  | LGM.Format.PPM => cmd := "ppmtopgm"; cmd2 := "pgmtopbm";
	  | LGM.Format.LGM => RAISE Error("Can't OCR from LGM image format");
	  END;
	  TRY
	    EVAL file.seek(RegularFile.Origin.Beginning, from.start);
	    CASE format OF
	    | LGM.Format.PBM =>
		(* no filter needed *)
		ocrRd := NEW(OSUtils.DupRd).init(file);
	    | LGM.Format.PGM =>
		ocrRd := OSUtils.RunFilter(ARRAY OF TEXT{"pgmtopbm",
							 "-threshold"},
					 file, pipe1);
	    | LGM.Format.PPM =>
		tempRd := OSUtils.RunFilter(ARRAY OF TEXT{"ppmtopgm"},
					    file, pipe1);
		ocrRd := OSUtils.RunFilter(ARRAY OF TEXT{"pgmtopbm",
							 "-threshold"},
					   pipe1, pipe2);
	    | LGM.Format.LGM =>
		RAISE Error("Can't OCR from LGM image format");
	    END;
	  EXCEPT
	    | OSError.E =>
	      IF format = LGM.Format.PBM THEN
		RAISE Error("Can't re-read image file");
	      ELSE
		RAISE Error("Can't run \"ppmtopgm\" or \"pgmtopbm\"");
	      END;
	  END;
	  TRY
	    IF verbose THEN
	      Wr.PutText(Stdio.stderr, "  OCR ... ");
	      Wr.Flush(Stdio.stderr);
	    END;
	    words.start := Wr.Index(wr);
	    start := Time.Now();
	    TRY
	      OCR.FromPBM(ocrRd, wr, nWords, nWordsBytes);
	    FINALLY
	      end := Time.Now();
	    END;
	  EXCEPT
	  | OCR.Error(code) =>
	      nWords := 0; nWordsBytes := 0; ocrError := TRUE;
	      Wr.PutText(Stdio.stderr,
		       "\nOCR failed: " & Atom.ToText(code.head) & "\n");
	  | Rd.Failure =>
	      RAISE Error("Can't read PBM file for OCR");
	  END;
	  TRY
	    IF pipe2 # NIL THEN Rd.Close(tempRd) END;
	    Rd.Close(ocrRd);
	  EXCEPT Rd.Failure => (* ignore *)
	  END;
	  words.length := nWordsBytes;
	  rects.start := words.start + words.length;
	  rects.length := Wr.Index(wr) - rects.start;
	  IF verbose THEN
	    Wr.PutText(Stdio.stderr,
		       Fmt.Int(rects.length DIV 16) &
		       " words, " & Fmt.Int(words.length) &
		       " bytes, " &
		       Fmt.LongReal(end-start, Fmt.Style.Fix, 1) &
		       " seconds.\n");
	    Wr.Flush(Stdio.stderr);
	  END;
	END;
      END;
    END;
  END PageToComponents;

PROCEDURE FileSize(file: RegularFile.T; name: TEXT): INTEGER RAISES { Error } =
  (* Returns the length of "file" *)
  BEGIN
    TRY
      RETURN file.status().size
    EXCEPT OSError.E => RAISE Error("Can't read \"" & name & "\"");
    END;
  END FileSize;

PROCEDURE FileToComponents(src: TEXT;
                           READONLY scales: LecternDoc.Scales;
                           gamma: REAL;
                           wr: Wr.T;
                           VAR page: LecternDoc.DirPage;
                           verbose: BOOLEAN)
                           RAISES { Rd.Failure, Wr.Failure, Thread.Alerted,
                                    Error, Rd.EndOfFile } =
  (* Write the LGM images and OCR data for the page in file "src"; converts
     TIFF files to PNM before calling PageToComponents. *)
  CONST
    tiffBigEndian = 'M';
    tiffLittleEndian = 'I';
  VAR
    srcFile: RegularFile.T;
    srcName: TEXT;
    tempFileName, tempFile2Name: TEXT := NIL;
    tempFile, tempFile2: RegularFile.T := NIL;
    c1, c2: CHAR;
    start, end: Time.T;
    height: CARDINAL;
  BEGIN
    TRY
      srcFile := FS.OpenFileReadonly(src);
      srcName := src;
      WITH tiffCheckRd = NEW(OSUtils.DupRd).init(srcFile) DO
        c1 := Rd.GetChar(tiffCheckRd);
        c2 := Rd.GetChar(tiffCheckRd);
        Rd.Close(tiffCheckRd);
      END;
    EXCEPT OSError.E =>
      RAISE Error("Can't open \"" & src & "\"");
    END;
    TRY
      IF (c1 = tiffBigEndian AND c2 = tiffBigEndian) OR
         (c1 = tiffLittleEndian AND c2 = tiffLittleEndian) THEN
        (* File is TIFF *)
        IF verbose THEN
          Wr.PutText(Stdio.stderr, "  Convert from TIFF ... ");
          Wr.Flush(Stdio.stderr);
        END;
        TRY
          tempFileName := TempFiles.Get(part := ",BuildLectern-tiff-");
          tempFile := FS.OpenFile(tempFileName);
          TempFiles.Note(tempFileName);
        EXCEPT OSError.E =>
          RAISE Error("Can't create \"" & tempFileName & "\"");
        END;
        TRY
          tempFile2Name := TempFiles.Get(part := ",BuildLectern-err-");
          tempFile2 := FS.OpenFile(tempFile2Name);
          TempFiles.Note(tempFile2Name);
        EXCEPT OSError.E =>
          RAISE Error("Can't create \"" & tempFile2Name & "\"");
        END;
        TRY
          EVAL srcFile.seek(RegularFile.Origin.Beginning, 0);
          start := Time.Now();
          IF Process.Wait(Process.Create(cmd := "tifftopnm",
                                         params := ARRAY OF TEXT{},
                                         stdin := srcFile,
                                         stdout := tempFile,
                                         stderr := tempFile2)) # 0 THEN
            RAISE Error("\"tifftopnm\" reported failure");
          END;
          end := Time.Now();
          IF verbose THEN
            Wr.PutText(Stdio.stderr,
                       Fmt.LongReal(end-start, Fmt.Style.Fix, 1) &
                       " seconds.\n");
            Wr.Flush(Stdio.stderr);
          END;
        EXCEPT OSError.E =>
          RAISE Error("Can't run \"tifftopnm\"");
        END;
        TRY
          TempFiles.Forget(tempFile2Name);
          tempFile2.close();
          FS.DeleteFile(tempFile2Name);
        EXCEPT OSError.E =>
          RAISE Error("Can't delete \"" & tempFile2Name & "\"");
        END;
        TRY
          srcFile.close();
          srcFile := tempFile;
          srcName := tempFileName;
        EXCEPT OSError.E =>
          RAISE Error("Failed to read converted TIFF image");
        END;
      END;
      PageToComponents(srcFile,
                       LecternDoc.Component{ start:=0,
                                             length:=FileSize(srcFile,
                                                              srcName)},
                       scales, gamma, wr, page, verbose, height);
    FINALLY
      TRY
        srcFile.close();
        IF tempFileName # NIL THEN TempFiles.Forget(tempFileName) END;
        IF tempFile # NIL THEN FS.DeleteFile(tempFileName) END;
      EXCEPT OSError.E =>
        RAISE Error("Can't delete \"" & tempFileName & "\"");
      END;
    END;
  END FileToComponents;

REVEAL RdClass.Private <: MUTEX; (* For kludge resetting rd.cur, below *)

PROCEDURE ReaderToComponents(rd: Rd.T;
                             READONLY scales: LecternDoc.Scales;
                             gamma: REAL;
                             wr: Wr.T;
                             VAR page: LecternDoc.DirPage;
                             verbose: BOOLEAN;
                             VAR (*OUT*) height: CARDINAL)
                             RAISES { Wr.Failure, Thread.Alerted, Error } =
  (* Write the LGM images and OCR data for a page in "rd", which is in
     some PNM format. Returns FALSE iff reach EOF instead of an image. *)
  VAR
    tempFileName := TempFiles.Get(part := ",BuildLectern-input-");
    tempFile: RegularFile.T := NIL;
    tempWr: Wr.T;
    start, end: Time.T;
  BEGIN
    (* The following is a kludge to get around the problem that reader
       indices are limited to 32 bits, whereas images streams can easily
       exceed that.  I read the FileRd.m3 source before writing this code. *)
    LOCK rd DO DEC(rd.cur, rd.lo); DEC(rd.hi, rd.lo); DEC(rd.lo, rd.lo) END;
    (* End kludge *)
    TRY
      IF verbose THEN
        Wr.PutText(Stdio.stderr, "  Copying image to temporary file ... ");
        Wr.Flush(Stdio.stderr);
      END;
      TRY
        tempFile := FS.OpenFile(tempFileName);
        TempFiles.Note(tempFileName);
        tempWr := NEW(FileWr.T).init(tempFile);
      EXCEPT OSError.E =>
        RAISE Error("Can't create \"" & tempFileName & "\"");
      END;
      TRY
        start := Time.Now();
        ImageRd.Copy(rd, tempWr); Wr.Flush(tempWr);
        end := Time.Now();
        IF verbose THEN
          Wr.PutText(Stdio.stderr,
                     Fmt.LongReal(end-start, Fmt.Style.Fix, 1) &
                     " seconds.\n");
          Wr.Flush(Stdio.stderr);
        END;
      EXCEPT
      | Rd.Failure => RAISE Error("Can't read input");
      | Wr.Failure => RAISE Error("Can't write \"" & tempFileName & "\"");
      | Images.Error(msg) => RAISE Error(msg);
      END;
      PageToComponents(tempFile,
                       LecternDoc.Component{ start:=0,
                                             length:=FileSize(tempFile,
                                                              tempFileName)},
                       scales, gamma, wr, page, verbose, height);
    FINALLY
      TRY
        TempFiles.Forget(tempFileName);
        IF tempFile # NIL THEN FS.DeleteFile(tempFileName) END;
        IF tempWr # NIL THEN Wr.Close(tempWr) END;
      EXCEPT OSError.E =>
        RAISE Error("Can't delete \"" & tempFileName & "\"");
      END;
    END;
  END ReaderToComponents;

PROCEDURE PSPageToComponents(
    fromPS: FromPS.T;
    rd: Rd.T;
    READONLY scales: LecternDoc.Scales;
    gamma: REAL;
    wr: Wr.T;
    VAR page: LecternDoc.DirPage;
    verbose: BOOLEAN)
  RAISES { Wr.Failure, Thread.Alerted, Error } =
  VAR height, nWords, nWordsBytes: CARDINAL;
  BEGIN
    ReaderToComponents(rd, scales, gamma, wr, page, verbose, height);
    WITH
      words = page[LecternDoc.Class.OCRWords],
      rects = page[LecternDoc.Class.OCRRects] DO
      IF scales[LecternDoc.Class.OCRWords] = -1 THEN
        words.start := Wr.Index(wr);
        fromPS.nextOCR(wr, height, nWords, nWordsBytes);
        words.length := nWordsBytes;
        rects.start := words.start + words.length;
        rects.length := Wr.Index(wr) - rects.start;
        IF verbose THEN
          Wr.PutText(Stdio.stderr,
                     "  OCR ... " &
                     Fmt.Int(rects.length DIV 16) &
                     " words, " & Fmt.Int(words.length) &
                     " bytes\n");
          Wr.Flush(Stdio.stderr);
        END;
      END;
    END;
  END PSPageToComponents;

PROCEDURE CopyDirPages(VAR dir: LecternDoc.Dir; newSize: INTEGER) =
  (* Extends or contracts dir.pages and dir.gammas to length "newSize" *)
  VAR
    oldCount := MIN(NUMBER(dir.pages^), newSize);
    newPages := NEW(LecternDoc.DirPages, newSize);
    newGammas := NEW(LecternDoc.Gammas, newSize);
  BEGIN
    SUBARRAY(newPages^, 0, oldCount) := SUBARRAY(dir.pages^, 0, oldCount);
    SUBARRAY(newGammas^, 0, oldCount) := SUBARRAY(dir.gammas^, 0, oldCount);
    FOR j := oldCount TO LAST(newPages^) DO
      FOR c := FIRST(LecternDoc.Class) TO LAST(LecternDoc.Class) DO
        newPages[j][c].start := 0;
        newPages[j][c].length := 0;
      END;
      newGammas[j] := 1.0;
    END;
    dir.pages := newPages;
    dir.gammas := newGammas;
  END CopyDirPages;

PROCEDURE CopyAttributes(VAR dir: LecternDoc.Dir; newSize: INTEGER) =
  (* Extends or contracts dir.attributes to length "newSize" *)
  VAR
    oldCount := MIN(NUMBER(dir.attributes^), newSize);
    newAttr := NEW(LecternDoc.Attributes, newSize);
  BEGIN
    SUBARRAY(newAttr^, 0, oldCount) := SUBARRAY(dir.attributes^, 0, oldCount);
    FOR i := oldCount TO LAST(newAttr^) DO
      newAttr[i].key := NIL;
      newAttr[i].value := NIL;
    END;
    dir.attributes := newAttr;
  END CopyAttributes;

PROCEDURE CheckAttribute(VAR dir: LecternDoc.Dir;
                         key: TEXT;
                         VAR totalAttrs: INTEGER;
                         remove: BOOLEAN): BOOLEAN =
  (* Returns TRUE iff there is an attribute with given "key".
     Iff "remove", also removes any such attribute. *)
  BEGIN
    FOR i := 0 TO totalAttrs-1 DO
      IF Text.Equal(dir.attributes[i].key, key) THEN
        IF remove THEN
          FOR j := i+1 TO totalAttrs-1 DO
            dir.attributes[j-1] := dir.attributes[j];
          END;
          DEC(totalAttrs);
        END;
        RETURN TRUE;
      END;
    END;
    RETURN FALSE
  END CheckAttribute;

PROCEDURE AddAttribute(VAR dir: LecternDoc.Dir;
                       key, value: TEXT;
                       VAR totalAttrs: INTEGER;
                       dominate: BOOLEAN) =
  (* If there is no attribute with the given "key", add one.
     Otherwise, iff "dominate", replace it with the given "value". *)
  BEGIN
    IF NOT CheckAttribute(dir, key, totalAttrs, dominate) OR dominate THEN
      IF totalAttrs >= NUMBER(dir.attributes^) THEN
        CopyAttributes(dir, NUMBER(dir.attributes^)*2+1);
      END;
      dir.attributes[totalAttrs].key := key;
      dir.attributes[totalAttrs].value := value;
      INC(totalAttrs);
    END;
  END AddAttribute;

VAR copyBuf: REF ARRAY [0..49999] OF File.Byte := NIL; (* for CopyFileToTemp *)

PROCEDURE CopyFileToTemp(
  in: File.T;
  VAR (*OUT*) out: RegularFile.T; VAR (*OUT*) name: TEXT)
  RAISES {OSError.E} =
  (* Create temporary file, copy "file" to it, and return its file
     handle (positioned to 0) and pathname. *)
  VAR
    n: INTEGER;
  BEGIN
    IF copyBuf = NIL THEN copyBuf := NEW(REF ARRAY [0..49999] OF File.Byte) END;
    name := TempFiles.Get(NIL, ",BuildLectern-ps-");
    out := FS.OpenFile(name, create := FS.CreateOption.Always);
    TempFiles.Note(name);
    LOOP
      n := in.read(copyBuf^);
      IF n = 0 THEN EXIT END;
      out.write(SUBARRAY(copyBuf^, 0, n))
    END;
    EVAL out.seek(RegularFile.Origin.Beginning, 0);
  END CopyFileToTemp;

PROCEDURE GetSignature(src: TEXT; VAR (*OUT*) sig: ARRAY [0..3] OF CHAR)
  : RegularFile.T
  RAISES {Error, Thread.Alerted} =
  (* Set "sig" to first four characters of "src", and return a file handle
     for "src", positioned at the beginning. *)
  VAR srcFile: RegularFile.T; n: INTEGER;
  BEGIN
    TRY
      srcFile := FS.OpenFileReadonly(src);
      WITH rd = NEW(OSUtils.DupRd).init(srcFile) DO
        n := Rd.GetSub(rd, sig);
        IF n < 4 THEN RAISE Error("Unexpected short file \"" & src & "\"") END;
        Rd.Close(rd);
      END;
    EVAL srcFile.seek(RegularFile.Origin.Beginning, 0);
    EXCEPT OSError.E, Rd.Failure =>
      RAISE Error("Can't read \"" & src & "\"");
    END;
    RETURN srcFile;
  END GetSignature;

PROCEDURE DoIt() RAISES { Error } =
  VAR
    start, end: Time.T;
    src, option, dest, destTemp: TEXT; (* file or option names *)
    thisParam: INTEGER;        (* Number of next command line parameter *)
    wr: Wr.T;                  (* Writer on destination document *)
    dir: LecternDoc.Dir;       (* Directory of destination document *)
    lastPage := -1;            (* Largest index in "dir" containing a page *)
    totalAttrs := 0;           (* Total number of attributes in "dir" *)
    thisPage := 0;             (* Index in "dir" of next page to be added *)
    pageIncr := 1;             (* 1 for simplex, 2 for duplex *)
    scales := LecternDoc.DefaultScales;
    scalesPS := scales;        (* modified below *)
    gamma: REAL := 1.0;        (* gamma adjustment for scaling *)
    gs: TEXT := NIL;           (* Ghostscript pathname *)
    verbose := FALSE;          (* level of chattiness *)
    originalDefined := FALSE;
    originDefined := FALSE;
    contentsDefined := FALSE;
    indexDefined := FALSE;
    (* Options applying to PostScript input: *)
    gammaPS: REAL := 0.454;    (* note different default than gamma above *)
    imageType := FromPS.ImageType.PBMRAW;
    includeOriginal := TRUE;
    orientation := FromPS.Orientation.Portrait;
    resolution := 300;         (* resolution of unscaled image in dots/inch *)
  PROCEDURE PrepareForPage(gamma: REAL) RAISES { Error } =
    BEGIN
      IF thisPage < 0 THEN
        RAISE Error("Too many images in \"-verso\" mode");
      END;
      IF thisPage >= NUMBER(dir.pages^) THEN
        CopyDirPages(dir, NUMBER(dir.pages^)*2+1);
      END;
      lastPage := MAX(lastPage, thisPage);
      dir.gammas[thisPage] := gamma;
    END PrepareForPage;
  PROCEDURE CompletePage() =
    BEGIN
      thisPage := thisPage + pageIncr; (* Might be negative *)
    END CompletePage;
  PROCEDURE DefineOrigin(n: INTEGER) =
    BEGIN
      dir.origin := n;
      originDefined := TRUE;
    END DefineOrigin;
  PROCEDURE DefineContents(n: INTEGER) =
    BEGIN
      dir.contents := n;
      contentsDefined := TRUE;
    END DefineContents;
  PROCEDURE DefineIndex(n: INTEGER) =
    BEGIN
      dir.index := n;
      indexDefined := TRUE;
    END DefineIndex;
  PROCEDURE LecternToComponents(
      file: RegularFile.T; src: TEXT;
      rescale, attributes: BOOLEAN;
      from, for: INTEGER)
    RAISES { Error, Rd.Failure, Thread.Alerted, Wr.Failure } =
    (* Copy or rescale some of the images from "file" (whose name is "src")
       to the destination. Leaves "file" open.  *)
    VAR rd: Rd.T; fromDir: LecternDoc.Dir; last: INTEGER;
    BEGIN
      TRY
        rd := NEW(OSUtils.DupRd).init(file);
        (* CAUTION: don't use "rd" after any explicit seek of "file" *)
        LecternDoc.ReadDir(rd, fromDir);
      EXCEPT
        | OSError.E => RAISE Error("Can't open \"" & src & "\"");
        | LecternDoc.NotLectern =>
           RAISE Error("\"" & src & "\" isn't a Lectern document");
      END;
      IF from < 0 THEN
        RAISE Error(
                "Invalid \"from\" image number when copying from \"" &
                src & "\"");
      END;
      last := MIN(from+for-1, LAST(fromDir.pages^));
      IF attributes THEN
        Wr.PutText(Stdio.stderr,
                   "Merging attributes, etc., from \"" &
                   src & "\"\n");
      END;
      IF last < from THEN
        IF NOT attributes THEN
          Wr.PutText(Stdio.stderr,
                     "Including nothing from \"" & src & "\"\n");
        END;
      ELSIF NOT rescale THEN
        WITH lastDest = thisPage+(last-from)*pageIncr DO
          Wr.PutText(Stdio.stderr,
                     "Images " & Fmt.Int(thisPage+1) &
                     " .. " & Fmt.Int(lastDest+1));
          IF pageIncr # 1 THEN
            Wr.PutText(Stdio.stderr," by " & Fmt.Int(pageIncr));
          END;
          Wr.PutText(Stdio.stderr,
                     ": including \"" & src &
                     "\" starting at its image " &
                     Fmt.Int(from+1) & "\n");
        END;
      END;
      Wr.Flush(Stdio.stderr);
      IF attributes THEN
        IF NOT originDefined THEN DefineOrigin(fromDir.origin) END;
        IF NOT contentsDefined THEN
          DefineContents(fromDir.contents);
        END;
        IF NOT indexDefined THEN DefineIndex(fromDir.index) END;
        indexDefined := TRUE;
        FOR i := 0 TO LAST(fromDir.attributes^) DO
          WITH attr = fromDir.attributes[i] DO
            AddAttribute(dir, attr.key, attr.value, totalAttrs, FALSE);
          END;
        END;
        dir.outline := LecternDoc.CopyComponent(rd,
                                                fromDir.outline,
                                                wr);
        IF NOT originalDefined THEN
          dir.original := LecternDoc.CopyComponent(rd,
                                                   fromDir.original,
                                                   wr);
          originalDefined := TRUE;
        END;
      END;
      FOR i := from TO last DO
        PrepareForPage(gamma);
        WITH
          from = fromDir.pages[i],
          to = dir.pages[thisPage] DO
          IF rescale THEN
            Wr.PutText(Stdio.stderr,
                       "Image " & Fmt.Int(thisPage+1) &
                       ": rescaling from \"" & src &
                       "\", image " & Fmt.Int(i+1) & "\n");
            Wr.Flush(Stdio.stderr);
            IF from[LecternDoc.Class.Print].start = 0 THEN
              RAISE Error("can't rescale - no unscaled image");
            END;
            VAR
              rescales := scales; height: CARDINAL;
            BEGIN
              (* Manufacture only the rescaled images; don't re-OCR *)
              rescales[LecternDoc.Class.OCRWords] := 0;
              rescales[LecternDoc.Class.OCRRects] := 0;
              PageToComponents(file, from[LecternDoc.Class.Print],
                               rescales, gamma,
                               wr, to, verbose, height);
            END;
            IF scales[LecternDoc.Class.OCRWords] # 0 THEN
              WITH
                words = LecternDoc.Class.OCRWords,
                rects = LecternDoc.Class.OCRRects DO
                to[words] := LecternDoc.CopyComponent(rd, from[words], wr);
                to[rects] := LecternDoc.CopyComponent(rd, from[rects], wr);
              END;
            END;
            FOR c := LecternDoc.Class.Hypertext TO LAST(LecternDoc.Class) DO
              to[c] := LecternDoc.CopyComponent(rd, from[c], wr);
            END;
          ELSE
            FOR c := FIRST(LecternDoc.Class) TO LAST(LecternDoc.Class) DO
              IF scales[c] # 0 THEN
                to[c] := LecternDoc.CopyComponent(rd, from[c], wr);
              END;
            END;
            dir.gammas[thisPage] := fromDir.gammas[i];
          END;
          CompletePage();
        END;
      END;
      TRY
        Rd.Close(rd);
      EXCEPT
        Rd.Failure => (* ignore *)
      END;
    END LecternToComponents;
  PROCEDURE PSStdinToComponents()
    RAISES { Error, Rd.Failure, Wr.Failure, Thread.Alerted } =
    VAR
      fileStdin, fileStdout, fileStderr: File.T;
      tempFile: RegularFile.T; tempFileName: TEXT; start, end: Time.T;
    BEGIN
      Process.GetStandardFileHandles(fileStdin, fileStdout, fileStderr);
      IF includeOriginal AND NOT originalDefined THEN
        IF verbose THEN
          Wr.PutText(Stdio.stderr, "  Copying stdin to temporary file ... ");
          Wr.Flush(Stdio.stderr);
        END;
        TRY
          start := Time.Now();
          CopyFileToTemp(fileStdin, tempFile, tempFileName);
          end := Time.Now();
          IF verbose THEN
            Wr.PutText(Stdio.stderr,
                       Fmt.LongReal(end-start, Fmt.Style.Fix, 1) &
                       " seconds.\n");
            Wr.Flush(Stdio.stderr);
          END;
        EXCEPT OSError.E =>
          RAISE Error("Can't create \"" & tempFileName & "\"");
        END;
        TRY
          PSFileToComponents(tempFile, tempFileName);
        FINALLY
          TRY
            TempFiles.Forget(tempFileName);
            IF tempFile # NIL THEN FS.DeleteFile(tempFileName) END;
          EXCEPT OSError.E =>
            RAISE Error("Can't delete \"" & tempFileName & "\"");
          END;
        END;
      ELSE
        PSFileToComponents(fileStdin, "(standard input)");
      END;
    END PSStdinToComponents;
  PROCEDURE PSFileToComponents(srcFile: File.T; src: TEXT)
    RAISES { Error, Rd.Failure, Thread.Alerted, Wr.Failure } =
    (* "srcFile" must be a "RegularFile.T" if "includeOriginal=TRUE". *)
    VAR fromPS: FromPS.T; rdImage, rd: Rd.T;
    BEGIN
      TRY
	fromPS := NEW(FromPS.T).init(
	  srcFile, imageType, orientation, resolution,
	  scalesPS[LecternDoc.Class.OCRWords] = -1, gs,
	  verbose);
	WHILE fromPS.nextImage(rdImage) DO
	  Wr.PutText(Stdio.stderr,
		     "Image " & Fmt.Int(thisPage+1) &
		     ": from PostScript\n");
	  Wr.Flush(Stdio.stderr);
	  PrepareForPage(gammaPS);
	  PSPageToComponents(fromPS, rdImage, scalesPS, gammaPS,
			     wr, dir.pages[thisPage], verbose);
	  CompletePage();
	END;
      EXCEPT FromPS.Error(text) => RAISE Error(text);
      END;
      IF includeOriginal AND NOT originalDefined THEN
	originalDefined := TRUE;
	Wr.PutText(Stdio.stderr, "Original PostScript: copying from input\n");
	Wr.Flush(Stdio.stderr);
	TRY
	  EVAL NARROW(srcFile, RegularFile.T).seek(
            RegularFile.Origin.Beginning, 0);
	  rd := NEW(OSUtils.DupRd).init(srcFile);
	EXCEPT OSError.E =>
	  RAISE Error("Can't reposition \"" & src & "\"");
	END;
	dir.original := LecternDoc.CopyRd(rd, Rd.Length(rd), wr);
	Rd.Close(rd);
      END;
    END PSFileToComponents;
  BEGIN
    Thread.IncDefaultStackSize(1000); (* avoid stack overflow on SOLgnu *)
    IF Params.Count < 3 THEN
      RAISE Error(
  "usage is \"" & Params.Get(0) &
                            " options... dest\", where \"options\" are:\n" &
  "  -contents <n>                ... first contents page is image <n>\n" &
  "  -gamma <r>                   ... apply gamma adjustment while scaling\n" &
  "  -image <n>                   ... next image is image number <n>\n" &
  "  -images <file> <from> <for>  ... copy images only from Lectern file\n" &
  "  -include <file> <from> <for> ... copy images & attributes from Lectern file\n" &
  "  -includeUnscaled             ... include unscaled images in output\n" &
  "  -index <n>                   ... first index page is page number <n>\n" &
  "  -noAttribute <key>           ... undefine attribute <key>\n" &
  "  -noOCR                       ... don't perform OCR\n" &
  "  -noUnscaled                  ... don't include unscaled images in output\n" &
  "  -only <n>                    ... produce only images scaled down by n\n" &
  "  -original <file>             ... copy original PostScript to output\n" &
  "  -page1 <n>                   ... page number 1 is image <n>\n" &
  "  -realOCR                     ... perform real OCR\n" &
  "  -recto                       ... following are odd pages, increasing\n" &
  "  -resolution <r>              ... assume <r> rather than 300 DPI for input\n" &
  "  -rescale <file> <from> <for> ... as \"include\", but rescale images\n" &
  "  -simplex                     ... following are consecutive pages\n" &
  "  -stdin                       ... take PNM images from standard input\n" &
  "  -verbose                     ... give more progress reports\n" &
  "  -verso                       ... following are even pages, decreasing\n" &
  "  -<key>: <value>              ... define attribute <key> = <value>\n" &
  "  <file>                       ... take an image from PNM or TIFF file,\n" &
  "                                   or a set of images from PostScript file\n" &
  "These options affect only input from (subsequent) PostScript file:\n" & 
  "  -PSblackAndWhite             ... generate black&white images\n" &
  "  -PScolor                     ... generate color images\n" &
  "  -PSfakeOCR                   ... extract OCR info from PostScript\n" &
  "  -PSgray                      ... generate grayscale images\n" &
  "  -PSgs                        ... specify Ghostscript pathname\n" &
  "  -PSincludeOriginal           ... include PostScript from output\n" &
  "  -PSlandscape                 ... rotate images 270 degrees\n" &
  "  -PSlandscapeOther            ... rotate images  90 degrees\n" &
  "  -PSportrait                  ... don't rotate images\n" &
  "  -PSnoOriginal                ... omit PostScript from output\n" &
  "  -PSscale <r>                 ... unscaled resolution is <r>*300 DPI\n" &
  "See the man page for details.");
    END;
    TRY
      Wr.PutText(Stdio.stderr, Version & "\n");
      dest := Params.Get(Params.Count-1);
      start := Time.Now();
      Wr.PutText(Stdio.stderr, "Constructing \"" & dest & "\"\n");
      Wr.Flush(Stdio.stderr);
      TempFiles.DefaultPrefix(Pathname.Prefix(dest));
      destTemp := TempFiles.Get(part := ",BuildLectern-out-");
      TRY
        wr := FileWr.Open(destTemp);
      EXCEPT OSError.E =>
        RAISE Error("Can't open \"" & destTemp & "\"");
      END;
      TempFiles.Note(destTemp);
      LecternDoc.WriteHeader(wr);
      (* If dest exists, only overwrite if it is apparently a Lectern file. *)
      VAR destRd: FileRd.T; junk: LecternDoc.Dir; BEGIN
        TRY
          destRd := FileRd.Open(dest);
          TRY
            LecternDoc.ReadDir(destRd, junk); 
          FINALLY Rd.Close(destRd);
          END;
        EXCEPT
        | OSError.E => (* probably doesn't exist *)
        | LecternDoc.NotLectern => RAISE Error("Destination exists and isn't Lectern file")
        END;
      END;
      dir.origin := -1;
      dir.contents := -1;
      dir.index := -1;
      dir.outline := LecternDoc.Component{};
      dir.original := LecternDoc.Component{};
      dir.pages := NEW(LecternDoc.DirPages, 0);
      dir.attributes := NEW(LecternDoc.Attributes, 0);
      dir.gammas := NEW(LecternDoc.Gammas, 0);
      scalesPS[LecternDoc.Class.OCRWords] := -1;
      scalesPS[LecternDoc.Class.Print] := 0;
      thisParam := 1;
      WHILE thisParam < Params.Count-1 DO
        src := Params.Get(thisParam);
        option := ForceLower(src);
        INC(thisParam);
        IF Text.Equal(option, "-attribute") THEN
          IF thisParam+1 < Params.Count-1 THEN
            WITH key = Params.Get(thisParam) DO
              INC(thisParam);
              AddAttribute(dir, key, Params.Get(thisParam), totalAttrs, TRUE);
              INC(thisParam);
            END;
          ELSE
            RAISE MissingParam
          END;
        ELSIF Text.Equal(option, "-contents") THEN
          DefineContents(ParamInt(thisParam)-1);
        ELSIF Text.Equal(option, "-gamma") THEN
          gamma := MAX(0.1, MIN(10.0, ParamReal(thisParam)));
          gammaPS := gamma;
        ELSIF Text.Equal(option, "-image") THEN
          thisPage := ParamInt(thisParam)-1;
          IF thisPage < 0 THEN
            RAISE Error("Negative image number \"" & Fmt.Int(thisPage+1) &
                        "\" in \"-image\" option");
          END;
        ELSIF Text.Equal(option, "-images") OR
              Text.Equal(option, "-include") OR
              Text.Equal(option, "-rescale") THEN
          IF thisParam+2 < Params.Count-1 THEN
            VAR
              rescale := Text.Equal(option, "-rescale");
              attributes := NOT Text.Equal(option, "-images");
              from, for: INTEGER;
              file: File.T;
            BEGIN
              src := Params.Get(thisParam);
              INC(thisParam);
              from := ParamInt(thisParam)-1;
              for := ParamInt(thisParam);
              TRY
                file := FS.OpenFileReadonly(src);
              EXCEPT
                | OSError.E => RAISE Error("Can't open \"" & src & "\"");
              END;
              LecternToComponents(file, src, rescale, attributes, from, for);
              TRY file.close() EXCEPT OSError.E => (* ignore *) END;
            END;
          ELSE
            RAISE MissingParam
          END;
        ELSIF Text.Equal(option, "-includeunscaled") THEN
          scales[LecternDoc.Class.Print] := 1;
          scalesPS[LecternDoc.Class.Print] := 1;
        ELSIF Text.Equal(option, "-index") THEN
          DefineIndex(ParamInt(thisParam) + dir.origin);
        ELSIF Text.Equal(option, "-noattribute") THEN
          IF thisParam < Params.Count-1 THEN
            WITH key = Params.Get(thisParam) DO
              INC(thisParam);
              EVAL CheckAttribute(dir, key, totalAttrs, TRUE);
            END;
          ELSE
            RAISE MissingParam
          END;
        ELSIF  Text.Equal(option, "-noocr") THEN
          scales[LecternDoc.Class.OCRWords] := 0;
          scales[LecternDoc.Class.OCRRects] := 0;
          scalesPS[LecternDoc.Class.OCRWords] := 0;
          scalesPS[LecternDoc.Class.OCRRects] := 0;
        ELSIF Text.Equal(option, "-nounscaled") THEN
          scales[LecternDoc.Class.Print] := 0;
          scalesPS[LecternDoc.Class.Print] := 0;
        ELSIF Text.Equal(option, "-only") THEN
          WITH n = ParamInt(thisParam) DO
            CASE n OF
            | 2, 3, 4 =>
              FOR c := LecternDoc.Class.Small TO LecternDoc.Class.Print DO
                IF LecternDoc.DefaultScales[c] = n THEN
                  scales[c] := n
                ELSE
                  scales[c] := 0;
                END;
              END;
              scales[LecternDoc.Class.OCRWords] := 0;
              scales[LecternDoc.Class.OCRRects] := 0;
              scalesPS := scales;
            ELSE
              RAISE Error("-only scale must be 2, 3, or 4");
            END;
          END;
        ELSIF Text.Equal(option, "-original") THEN
          IF thisParam < Params.Count-1 THEN
            src := Params.Get(thisParam);
            INC(thisParam);
            originalDefined := TRUE;
            Wr.PutText(Stdio.stderr, "Original PostScript: copying \"" &
                                     src & "\"\n");
            Wr.Flush(Stdio.stderr);
            VAR rd: Rd.T;
            BEGIN
              TRY
                rd := FileRd.Open(src);
              EXCEPT OSError.E =>
                RAISE Error("Can't open \"" & src & "\"");
              END;
              dir.original := LecternDoc.CopyRd(rd, Rd.Length(rd), wr);
              Rd.Close(rd);
            END;
          ELSE
            RAISE MissingParam
          END;
        ELSIF Text.Equal(option, "-page1") THEN
          DefineOrigin(ParamInt(thisParam)-2);
        ELSIF Text.Equal(option, "-psblackandwhite") THEN
          imageType := FromPS.ImageType.PBMRAW
        ELSIF Text.Equal(option, "-pscolor") THEN
          imageType := FromPS.ImageType.PPMRAW
        ELSIF Text.Equal(option, "-psfakeocr") THEN
          scalesPS[LecternDoc.Class.OCRWords] := -1;
          scalesPS[LecternDoc.Class.OCRRects] := 1;
        ELSIF Text.Equal(option, "-psgray") OR 
              Text.Equal(option, "-psgrey") THEN
          imageType := FromPS.ImageType.PGMRAW
        ELSIF Text.Equal(option, "-psgs") THEN
          gs := Params.Get(thisParam);
          INC(thisParam);
        ELSIF Text.Equal(option, "-psincludeoriginal") THEN
          includeOriginal := TRUE;
        ELSIF Text.Equal(option, "-pslandscape") THEN
          orientation := FromPS.Orientation.Landscape;
        ELSIF Text.Equal(option, "-pslandscapeother") THEN
          orientation := FromPS.Orientation.LandscapeOther;
        ELSIF Text.Equal(option, "-psportrait") THEN
          orientation := FromPS.Orientation.Portrait;
        ELSIF Text.Equal(option, "-psnooriginal") THEN
          includeOriginal := FALSE;
        ELSIF Text.Equal(option, "-psscale") THEN
          resolution := FLOOR(ParamReal(thisParam) * 300.0);
          IF resolution < 0 THEN
            RAISE Error("Negative value in \"-PSscale\" option");
          END;
        ELSIF Text.Equal(option, "-realocr") THEN
          IF NOT OCR.IsFromPBMImplemented() THEN
            RAISE Error("OCR isn't implemented on this platform." &
                   "  By default, \"-noOCR\" will be used for image input.");
          END;
          scales[LecternDoc.Class.OCRWords] := 1;
          scales[LecternDoc.Class.OCRRects] := 1;
          scalesPS[LecternDoc.Class.OCRWords] := 1;
          scalesPS[LecternDoc.Class.OCRRects] := 1;
        ELSIF Text.Equal(option, "-recto") THEN
          pageIncr := 2;
        ELSIF Text.Equal(option, "-resolution") THEN
          WITH n = ParamInt(thisParam) DO
            FOR c := LecternDoc.Class.Thumbnail TO LecternDoc.Class.Print DO
              scales[c] := ROUND(FLOAT(scales[c]) * FLOAT(n) / 300.0);
            END;
          END;
        ELSIF Text.Equal(option, "-simplex") THEN
          pageIncr := 1;
        ELSIF Text.Equal(option, "-stdin") THEN
          WHILE TRUE DO
            TRY
              (* Skip initial white space, and terminate on EOF *)
              WHILE TRUE DO
                WITH c = Rd.GetChar(Stdio.stdin) DO
                  IF c # ' ' AND c # '\n' THEN
                    Rd.UnGetChar(Stdio.stdin); EXIT;
                  END;
                END;
              END;
            EXCEPT
            | Rd.Failure => RAISE Error("Can't read stdin");
            | Rd.EndOfFile => EXIT;
            END;
            Wr.PutText(Stdio.stderr,
                       "Image " & Fmt.Int(thisPage+1) &
                       ": from stdin\n");
            Wr.Flush(Stdio.stderr);
            PrepareForPage(gamma);
            VAR height: CARDINAL;
            BEGIN
              ReaderToComponents(Stdio.stdin, scales, gamma,
                                 wr, dir.pages[thisPage], verbose, height);
            END;
            CompletePage();
          END;
        ELSIF Text.Equal(option, "-verbose") THEN
          verbose := TRUE;
        ELSIF Text.Equal(option, "-verso") THEN
          thisPage := thisPage - 1;
          pageIncr := -2;
        ELSIF Text.Length(option) > 0 AND Text.GetChar(src, 0) = '-' THEN
          IF Text.Length(option) = 1 THEN (* PostScript from standard input *)
            PSStdinToComponents();
          ELSIF Text.GetChar(src, Text.Length(src)-1) = ':' THEN
            IF thisParam < Params.Count-1 THEN
              WITH key = Text.Sub(src, 1, Text.Length(src)-2) DO
                AddAttribute(dir, key, Params.Get(thisParam), totalAttrs, TRUE);
                INC(thisParam);
              END;
            ELSE
              RAISE MissingParam
            END;
          ELSE
            RAISE Error("Unknown option \"" & src & "\"");
          END;
        ELSE (* <file> *)
          VAR 
            sig: ARRAY [0..3] OF CHAR;
            srcFile := GetSignature(src, sig);
          BEGIN
            TRY
              IF sig[0] = '%' AND sig[1] = '!'
                 OR sig[0] = '\004' AND sig[1] = '%' AND sig[2] = '!' THEN
                PSFileToComponents(srcFile, src);
              ELSIF sig = ARRAY OF CHAR {'l', 'e', 'c', 't'} THEN
                LecternToComponents(srcFile, src,
                  rescale := FALSE,
                  attributes := TRUE,
                  from := 0, for := LAST(INTEGER)
                  );
              ELSE
                Wr.PutText(Stdio.stderr,
                           "Image " & Fmt.Int(thisPage+1) &
                           ": reading \"" & src & "\"\n");
                Wr.Flush(Stdio.stderr);
                PrepareForPage(gamma);
                FileToComponents(src, scales, gamma,
                                 wr, dir.pages[thisPage], verbose);
                CompletePage();
              END;
            FINALLY
              TRY srcFile.close() EXCEPT OSError.E => (* ignore *) END;
            END;
          END;
        END;
      END;
      FOR i := 0 TO lastPage DO
        WITH page = dir.pages[i] DO
          VAR emptyPage := TRUE;
          BEGIN
            FOR j := FIRST(LecternDoc.Class) TO LAST(LecternDoc.Class) DO
              IF page[j].start > 0 THEN emptyPage := FALSE; EXIT END;
            END;
            IF emptyPage THEN
              RAISE Error("Image " & Fmt.Int(i+1) & " is missing");
            END;
          END;
        END;
      END;
      CopyDirPages(dir, lastPage+1);
      CopyAttributes(dir, totalAttrs);
      WITH dirStart = Wr.Index(wr), totalPages = NUMBER(dir.pages^) DO
        LecternDoc.WriteDir(wr, dir);
        LecternDoc.WriteTrailer(wr, dirStart);
        Wr.Close(wr);
        TRY
          TempFiles.Forget(destTemp);
          FS.Rename(destTemp, dest);
          destTemp := NIL;
        EXCEPT OSError.E =>
          RAISE Error("Can't rename \"" & destTemp & "\" to \"" & dest & "\"");
        END;
        end := Time.Now();
        Wr.PutText(Stdio.stderr, "Wrote \"" & dest & "\":\n");
        IF totalPages = 0 THEN
          Wr.PutText(Stdio.stderr, "  Contains no images\n");
        ELSE
          Wr.PutText(Stdio.stderr, "  Contains " & Fmt.Int(totalPages) &
                   " images (average of " &
                   Fmt.Int((((dirStart-dir.original.length) DIV totalPages) DIV
                             10240) * 10) &
                   " KBytes and " &
                   Fmt.Int(FLOOR(end-start) DIV totalPages) &
                   " seconds per image)\n");
        END;
        IF dir.original.start = 0 THEN
          Wr.PutText(Stdio.stderr, "  Contains no original PostScript\n");
        ELSE
          Wr.PutText(Stdio.stderr, "  Contains " &
                                   Fmt.Int(dir.original.length) &
                                   " bytes of original PostScript\n");
        END;
        Wr.PutText(Stdio.stderr, "  Page number 1 is at image " &
                                 Fmt.Int(dir.origin+2) & "\n");
        IF dir.contents < 0 THEN
          Wr.PutText(Stdio.stderr, "  No contents page specified\n");
        ELSE
          Wr.PutText(Stdio.stderr, "  First contents page is at image " &
                                   Fmt.Int(dir.contents+1) & "\n");
        END;
        IF dir.index < 0 THEN
          Wr.PutText(Stdio.stderr, "  No index page specified\n");
        ELSE
          Wr.PutText(Stdio.stderr, "  First index page is at image " &
                                   Fmt.Int(dir.index+1) &
                                   " (page number " &
                                   Fmt.Int(dir.index-dir.origin) & ")\n");
        END;
        IF NUMBER(dir.attributes^) = 0 THEN
          Wr.PutText(Stdio.stderr, "  No key/value attributes specified\n");
        ELSE
          FOR i := 0 TO LAST(dir.attributes^) DO
            Wr.PutText(Stdio.stderr, "  \"" &
                       dir.attributes[i].key & "\" = \"" &
                       dir.attributes[i].value & "\"\n");
          END;
        END;
        Wr.Flush(Stdio.stderr);
      END;
    EXCEPT
      | MissingParam => RAISE Error("Missing parameter after \"" & src & "\"")
      | Rd.EndOfFile => RAISE Error("Unexpected end of file")
      | Rd.Failure => RAISE Error("Error while reading file")
      | Wr.Failure => RAISE Error("Error while writing \"" & dest & "\"")
      | Thread.Alerted => RAISE Error("Unexpected Thread.Alerted")
    END
  END DoIt;

BEGIN
  TRY
    DoIt();
    IF ocrError THEN Process.Exit(2) END;
  EXCEPT Error(e) =>
    TRY
      Wr.PutText(Stdio.stderr, "\n" & Params.Get(0) & ": " & e & "\n");
      Wr.Flush(Stdio.stderr);
    EXCEPT Wr.Failure, Thread.Alerted =>
    END;
    Process.Exit(1);
  END;
END BuildLectern.
