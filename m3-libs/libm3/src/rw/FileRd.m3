(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Fri Sep 23 09:32:52 PDT 1994 by kalsow     *)
(*      modified on Mon Jun 21 11:48:11 PDT 1993 by wobber     *)
(*      modified on Tue Jun 15 13:41:09 1993 by gnelson        *)
(*      modified on Wed May 26 06:46:37 PDT 1993 by swart      *)
(*      modified on Mon Apr 26 17:16:39 PDT 1993 by mcjones    *)

UNSAFE MODULE FileRd;

IMPORT File, FS, OSError, Pathname, Rd, RdClass, RegularFile;

PROCEDURE Open(p: Pathname.T): T RAISES {OSError.E} =
  BEGIN
    RETURN NEW(T).init(FS.OpenFileReadonly(p))
  END Open;

TYPE CharBuffer = REF ARRAY OF CHAR;

CONST BufferSize = 4096;

CONST BIG = 16_1000000; (* 2^24 => 16M *)

TYPE ByteArrayPtr = UNTRACED REF ARRAY [0..BIG-1] OF File.Byte;

REVEAL T = Public BRANDED "FileRd.T" OBJECT
    sourceH: File.T;
  OVERRIDES
    init := Init;
    seek := Seek;
    length := Length;
    close := Close;
    getSub := GetSub;
  END;
(* Q1: If "rd.sourceH" is a "RegularFile.T" then the current position
       of "rd.sourceH" is equal to "rd.hi". 

   Q2: If "rd.seekable", then "rd.sourceH" is a "RegularFile.T". *)

PROCEDURE Init(rd: T; h: File.T): T RAISES {OSError.E} =
  BEGIN
    rd.sourceH := h;
    IF (rd.buff = NIL) THEN
      rd.buff := NEW(CharBuffer, BufferSize);
    (*ELSE reuse the existing buffer*)
    END;
    rd.st := 0;
    rd.closed := FALSE;
    TYPECASE h OF
    | RegularFile.T(hRF) =>
      rd.seekable := TRUE;
      rd.intermittent := FALSE;
      rd.cur := hRF.seek(RegularFile.Origin.Current, 0);
    ELSE
      rd.seekable := FALSE;
      rd.intermittent := TRUE;
      rd.cur := 0
    END;
    rd.lo := rd.cur;
    rd.hi := rd.cur;
    RETURN rd
  END Init;

EXCEPTION Error; <*FATAL Error*>

PROCEDURE Seek (rd: T; pos: CARDINAL; dontBlock: BOOLEAN): RdClass.SeekResult
  RAISES {Rd.Failure} =
  VAR n: INTEGER; BEGIN
    TRY
      IF pos # rd.hi THEN
        IF NOT rd.seekable THEN RAISE Error; END;
        IF pos > rd.cur THEN pos := MIN(pos, rd.sourceH.status().size) END;
        EVAL NARROW(rd.sourceH, RegularFile.T).seek(
                            RegularFile.Origin.Beginning, pos);
        rd.cur := pos;
        rd.hi := pos
      ELSE
        rd.cur := pos;
      END;
      rd.lo := pos;
      (* Maintains Q1 if sourceH.read raises OS.Error or returns -1. *)
      n := rd.sourceH.read(
             SUBARRAY(LOOPHOLE(ADR(rd.buff[0]), ByteArrayPtr)^, 0,
                      MIN (NUMBER(rd.buff^), BIG)), mayBlock := NOT dontBlock)
    EXCEPT
    | OSError.E (code) => RAISE Rd.Failure(code)
    END;
    IF n < 0 THEN RETURN RdClass.SeekResult.WouldBlock END;
    IF n = 0 THEN RETURN RdClass.SeekResult.Eof END;
    INC(rd.hi, n);
    RETURN RdClass.SeekResult.Ready
  END Seek;

PROCEDURE GetSub (rd: T; VAR a: ARRAY OF CHAR): CARDINAL
  RAISES {Rd.Failure} =
  VAR toRead := NUMBER(a);
  BEGIN
    TRY
      WHILE toRead # 0 DO
        IF rd.cur # rd.hi THEN
          VAR n := MIN(toRead, rd.hi-rd.cur); BEGIN
            SUBARRAY(a, NUMBER(a) - toRead, n) := 
              SUBARRAY(rd.buff^, rd.cur-rd.lo, n);
            INC(rd.cur, n);
            DEC(toRead, n)
          END
        ELSE
          rd.lo := rd.cur;
          (* Maintains Q1 if sourceH.read raises OS.Error or returns -1. *)
          VAR n: INTEGER; BEGIN
            IF 2 * toRead < NUMBER(rd.buff^) THEN
              n := rd.sourceH.read(
                    SUBARRAY(LOOPHOLE(ADR(rd.buff[0]), ByteArrayPtr)^, 
                              0, MIN (NUMBER(rd.buff^), BIG)));
              INC(rd.hi, n)
            ELSE
              n := rd.sourceH.read(
                    SUBARRAY(LOOPHOLE(ADR(a[NUMBER(a)-toRead]), ByteArrayPtr)^,
                              0, MIN (toRead, BIG)));
              DEC(toRead, n);
              INC(rd.cur, n);
              rd.lo := rd.cur;
              rd.hi := rd.cur
            END;
            IF n = 0 THEN EXIT; END
          END
        END
      END
    EXCEPT
      OSError.E (code) => RAISE Rd.Failure(code)
    END;
    RETURN NUMBER(a) - toRead;
  END GetSub;

PROCEDURE Length(rd: T): INTEGER RAISES {Rd.Failure} =
  BEGIN
    TRY
      IF rd.seekable THEN
        RETURN rd.sourceH.status().size;
      ELSE
        RETURN -1;
      END
    EXCEPT
    | OSError.E(code) =>  RAISE Rd.Failure(code)
    END
  END Length;

PROCEDURE Close(rd: T) RAISES {Rd.Failure} =
  BEGIN
    TRY
      rd.sourceH.close()
    EXCEPT OSError.E(code) => RAISE Rd.Failure(code)
    END
  END Close;

BEGIN
END FileRd.
