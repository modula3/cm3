(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: interface for lectern document format *)

(* Last modified on Fri Apr  7 14:47:28 PDT 1995 by birrell   *)

MODULE LecternDoc;

IMPORT Rd, Text, TextF (* for ReadText *), Thread, Wr;

CONST
  Signature = "lect";
  RevisionLevel = 2; (* revision level of file formats having this sig. *)


(* *)
(* Subroutines *)
(* *)

PROCEDURE PutInt4(wr: Wr.T; n: INTEGER)
                  RAISES { Wr.Failure, Thread.Alerted } =
    (* Write an integer onto wr as four bytes, L.S. first. Uses IntBias to
       simplify representation of negative numbers machine-independently. *)
  VAR nn: CARDINAL := n+IntBias;
  BEGIN
    FOR j := 0 TO 3 DO
      Wr.PutChar(wr, VAL(nn MOD 256, CHAR));
      nn := nn DIV 256;
    END;
  END PutInt4;

PROCEDURE PutReal(wr: Wr.T; r: REAL)
                  RAISES { Wr.Failure, Thread.Alerted } =
    (* Write "r" onto wr, encoded by writing the integer ROUND(r*RealFactor).
       Note that r must be >= -IntBias/RealFactor *)
  BEGIN
    PutInt4(wr, ROUND(r*RealFactor));
  END PutReal;

PROCEDURE PutComponent(wr: Wr.T; component: Component)
                  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    PutInt4(wr, component.start);
    PutInt4(wr, component.length);
  END PutComponent;

PROCEDURE PutText(wr: Wr.T; t: TEXT) RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    PutInt4(wr, Text.Length(t));
    Wr.PutText(wr, t);
  END PutText;

PROCEDURE ReadInt4(rd: Rd.T): INTEGER RAISES { Rd.Failure, Thread.Alerted,
                                               Rd.EndOfFile } =
    (* Read an integer represented as four bytes on rd, L.S. first, and adjust
       for IntBias. *)
  VAR n := 0; mul := 1;
  BEGIN
    FOR i := 0 TO 3 DO
      INC(n, ORD(Rd.GetChar(rd)) * mul);
      IF i < 3 THEN mul := mul * 256 END;
    END;
    RETURN n-IntBias
  END ReadInt4;

PROCEDURE ReadReal(rd: Rd.T): REAL RAISES { Rd.Failure, Thread.Alerted,
                                               Rd.EndOfFile } =
    (* Read a real encoded as a positive integer, and adjust for IntBias and
       RealFactor. *)
  BEGIN
    RETURN FLOAT(ReadInt4(rd)) / RealFactor
  END ReadReal;

PROCEDURE VerifySignature(rd: Rd.T) RAISES { Rd.Failure, Thread.Alerted,
                                             Rd.EndOfFile, NotLectern } =
  BEGIN
    FOR i := 0 TO Text.Length(Signature)-1 DO
      IF Rd.GetChar(rd) # Text.GetChar(Signature, i) THEN RAISE NotLectern END;
    END;
  END VerifySignature;

PROCEDURE ReadText(rd: Rd.T): TEXT RAISES { Rd.Failure, Thread.Alerted,
                                            Rd.EndOfFile, NotLectern } =
  VAR
    len := ReadInt4(rd);
    t: TEXT;
  BEGIN
    IF len < 0 OR len > 100000 THEN RAISE NotLectern END;
    t := TextF.New(len);
    IF Rd.GetSub(rd, SUBARRAY(t^, 0, len)) # len THEN RAISE NotLectern END;
    RETURN t
  END ReadText;

(* *)
(* Copying *)
(* *)

VAR
  buffer: REF ARRAY OF CHAR := NIL; (* for CopyRd *)
  bufferMu := NEW(Thread.Mutex);

PROCEDURE CopyRd(rd: Rd.T; length: INTEGER; wr: Wr.T): Component
                 RAISES { Rd.Failure, Wr.Failure, Thread.Alerted } =
  VAR
    start, remaining: INTEGER;
  BEGIN
    LOCK bufferMu DO
      start := Wr.Index(wr);
      IF buffer = NIL THEN buffer := NEW(REF ARRAY OF CHAR, 50000) END;
      remaining := length;
      WHILE remaining > 0 DO
        WITH
          thisTime = MIN(remaining, NUMBER(buffer^)),
          done = Rd.GetSub(rd, SUBARRAY(buffer^, 0, thisTime)) DO
          IF done # thisTime THEN remaining := done (* early EOF *) END;
          Wr.PutString(wr, SUBARRAY(buffer^, 0, done));
          DEC(remaining, done);
        END;
      END;
    END;
    RETURN Component{start := start, length := Wr.Index(wr)-start };
  END CopyRd;

PROCEDURE CopyComponent(rd: Rd.T; from:Component; wr: Wr.T): Component
             RAISES { Rd.Failure, Wr.Failure, Thread.Alerted } =
  BEGIN
    IF from.start > 0 THEN
      Rd.Seek(rd, from.start);
      RETURN CopyRd(rd, MAX(from.length, 0), wr);
    ELSE
      RETURN Component{}
    END;
  END CopyComponent;


(* *)
(* Writing *)
(* *)

PROCEDURE WriteHeader(wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    Wr.PutText(wr, Signature);
    (* Write four #ff bytes, so it looks like binary to causal observers *)
    (* These bytes are ignored when reading *)
    FOR i := 0 TO 3 DO Wr.PutChar(wr, VAL(255,CHAR)) END;
  END WriteHeader;

PROCEDURE WriteDir(wr: Wr.T; READONLY dir: Dir) RAISES {Wr.Failure,
                                                        Thread.Alerted} =
  BEGIN
    PutInt4(wr, dir.origin);
    PutInt4(wr, dir.contents);
    PutInt4(wr, dir.index);
    PutInt4(wr, RevisionLevel);
    PutInt4(wr, 0); (* spare, for future expansion *)
    PutInt4(wr, 0); (* spare, for future expansion *)
    PutInt4(wr, 0); (* spare, for future expansion *)
    PutInt4(wr, 0); (* spare, for future expansion *)
    PutComponent(wr, dir.outline);
    PutComponent(wr, dir.original);
    IF dir.pages = NIL THEN
      PutInt4(wr, 0);
    ELSE
      PutInt4(wr, NUMBER(dir.pages^));
      FOR i := 0 TO LAST(dir.pages^) DO
        FOR j := FIRST(Class) TO Class.OCRRects DO
          PutComponent(wr, dir.pages[i][j]);
        END;
      END;
    END;
    IF dir.attributes = NIL THEN
      PutInt4(wr, 0);
    ELSE
      PutInt4(wr, NUMBER(dir.attributes^));
      FOR i := 0 TO LAST(dir.attributes^) DO
        PutText(wr, dir.attributes[i].key);
        PutText(wr, dir.attributes[i].value);
      END;
    END;
    IF dir.gammas = NIL THEN
      PutInt4(wr, 0);
    ELSE
      PutInt4(wr, NUMBER(dir.gammas^));
      FOR i := 0 TO LAST(dir.gammas^) DO
        PutReal(wr, dir.gammas[i]);
      END;
    END;
    FOR i := 0 TO LAST(dir.pages^) DO
      FOR j := Class.Hypertext TO LAST(Class) DO
        PutComponent(wr, dir.pages[i][j]);
      END;
      PutInt4(wr, 0); (* spare, for future expansion *)
      PutInt4(wr, 0); (* spare, for future expansion *)
    END;
  END WriteDir;

PROCEDURE WriteTrailer(wr: Wr.T; dirPos: CARDINAL) RAISES {Wr.Failure,
                                                           Thread.Alerted} =
  BEGIN
    PutInt4(wr, dirPos);
    Wr.PutText(wr, Signature);
  END WriteTrailer;


(* *)
(* Reading *)
(* *)

PROCEDURE ReadDir(rd: Rd.T; VAR dir: Dir) RAISES { Rd.Failure, Thread.Alerted,
                                                   NotLectern } =
  VAR
    dirStart: INTEGER;
    revLevel: INTEGER;
  BEGIN
    TRY
      Rd.Seek(rd, 0);
      VerifySignature(rd);
      Rd.Seek(rd, Rd.Length(rd) - (4+Text.Length(Signature)));
      dirStart := ReadInt4(rd);
      VerifySignature(rd);
      IF dirStart < 0 OR dirStart > Rd.Length(rd) THEN RAISE NotLectern END;
      Rd.Seek(rd, dirStart);
      dir.origin := ReadInt4(rd);
      dir.contents := ReadInt4(rd);
      dir.index := ReadInt4(rd);
      revLevel := ReadInt4(rd);
      EVAL ReadInt4(rd); (* space for future expansion *)
      EVAL ReadInt4(rd); (* space for future expansion *)
      EVAL ReadInt4(rd); (* space for future expansion *)
      EVAL ReadInt4(rd); (* space for future expansion *)
      dir.outline.start := ReadInt4(rd);
      dir.outline.length := ReadInt4(rd);
      dir.original.start := ReadInt4(rd);
      dir.original.length := ReadInt4(rd);
      dir.pages := NEW(DirPages, ReadInt4(rd));
      FOR i := 0 TO LAST(dir.pages^) DO
        FOR j := FIRST(Class) TO Class.OCRRects DO
          dir.pages[i][j].start := ReadInt4(rd);
          dir.pages[i][j].length := ReadInt4(rd);
        END;
      END;
      dir.attributes := NEW(Attributes, ReadInt4(rd));
      FOR i := 0 TO LAST(dir.attributes^) DO
        dir.attributes[i].key := ReadText(rd);
        dir.attributes[i].value := ReadText(rd);
      END;
      dir.gammas := NEW(Gammas, ReadInt4(rd));
      FOR i := 0 TO LAST(dir.gammas^) DO
        IF revLevel = 0 THEN
          dir.gammas[i] := 1.0 / (FLOAT(ReadInt4(rd)) / 1024.0);
        ELSE
          dir.gammas[i] := ReadReal(rd);
        END;
      END;
      FOR i := 0 TO LAST(dir.pages^) DO
        FOR j := Class.Hypertext TO LAST(Class) DO
          IF revLevel < 2 THEN
            dir.pages[i][j] := Component{};
          ELSE
            dir.pages[i][j].start := ReadInt4(rd);
            dir.pages[i][j].length := ReadInt4(rd);
          END;
        END;
        IF revLevel >= 2 THEN
          EVAL ReadInt4(rd); (* space for future expansion *)
          EVAL ReadInt4(rd); (* space for future expansion *)
        END;
      END;
    EXCEPT
    | Rd.EndOfFile => RAISE NotLectern;
    END;
  END ReadDir;


(* *)
(* Module initialization *)
(* *)

BEGIN
END LecternDoc.
