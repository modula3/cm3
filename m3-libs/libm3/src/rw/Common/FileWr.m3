(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Tue Sep  3 18:54:00 PDT 1996 by heydon     *)
(*      modified on Fri Sep 23 11:12:07 PDT 1994 by kalsow     *)
(*      modified on Mon Jun 21 11:48:09 PDT 1993 by wobber     *)
(*      modified on Sun May 23 00:04:54 PDT 1993 by swart      *)
(*      modified on Wed Apr 28 12:01:41 PDT 1993 by mcjones    *)
(*      modified on Thu Jan 28 13:54:21 PST 1993 by mjordan    *)
(*      modified on Tue Mar 24 22:18:02 PST 1992 by muller     *)

UNSAFE MODULE FileWr;

IMPORT File, FS, Pathname, OSError, RegularFile, Wr, WrClass;

PROCEDURE Open(p: Pathname.T): T RAISES {OSError.E} =
  BEGIN
    RETURN NEW(T).init(FS.OpenFile(p))
  END Open;

PROCEDURE OpenAppend(p: Pathname.T): T RAISES {OSError.E} =
  VAR h: RegularFile.T := FS.OpenFile(p, truncate := FALSE);
  BEGIN
    EVAL h.seek(RegularFile.Origin.End, 0);
    RETURN NEW(T).init(h)
  END OpenAppend;

REVEAL T = Public BRANDED "FileWr.T" OBJECT
    targetH: File.T;
  OVERRIDES
    init := Init;
    seek := Seek;
    length := Length;
    flush := Flush;
    close := Close;
    putString := PutString;
  END;
(* Q1: If "wr.targetH" is a "RegularFile.T" then the current position
       of "wr.targetH" is equal to "wr.lo". 

   Q2: If "wr.seekable", then "wr.targetH" is a "RegularFile.T". *)


TYPE CharBuffer = REF ARRAY OF CHAR;

CONST BufferSize = 4096;

CONST BIG = 16_1000000; (* 2^24 => 16M *)

TYPE ByteArrayPtr = UNTRACED REF ARRAY [0 .. BIG-1] OF File.Byte;

PROCEDURE Init (wr: T; h: File.T; buffered: BOOLEAN := TRUE): T
  RAISES {OSError.E} =
  BEGIN
    wr.targetH := h;
    wr.st := 0;
    wr.closed := FALSE;
    IF (wr.buff = NIL) THEN
      wr.buff := NEW(CharBuffer, BufferSize);
    (*ELSE reuse the existing buffer *)
    END;
    TYPECASE h OF
    | RegularFile.T (hRF) =>     (* seekable *)
      wr.seekable := TRUE;
      wr.cur := hRF.seek(RegularFile.Origin.Current, 0);
      wr.buffered := TRUE;
    ELSE
      wr.seekable := FALSE;
      wr.cur := 0;
      wr.buffered := buffered;
    END;
    wr.lo := wr.cur;
    wr.hi := wr.cur + NUMBER(wr.buff^);
    RETURN wr
  END Init;

EXCEPTION Error; <*FATAL Error*>

PROCEDURE Seek(wr: T; n: CARDINAL) RAISES {Wr.Failure} =
  BEGIN
    IF NOT wr.seekable AND n # wr.hi THEN RAISE Error END;
    TRY
      EmptyBuffer (wr);
      (* Maintains V4 -- we hope that on a seek failure the file
                         position is unchanged, ensuring Q1 *)
      IF n # wr.cur THEN
        IF n > wr.cur THEN n := MIN(n, wr.targetH.status().size); END;
        EVAL NARROW(wr.targetH, RegularFile.T).seek(
                     RegularFile.Origin.Beginning, n);
        wr.cur := n;
        wr.lo := n;
        wr.hi := n + NUMBER(wr.buff^);
      END;
    EXCEPT
    | OSError.E(code) =>  RAISE Wr.Failure(code)
    END
  END Seek;

PROCEDURE Length(wr: T): CARDINAL RAISES {Wr.Failure} =
  BEGIN
    TRY
      IF wr.seekable THEN
        RETURN MAX(wr.cur, wr.targetH.status().size);
      ELSE
        RETURN wr.cur;
      END
    EXCEPT
    | OSError.E(code) =>  RAISE Wr.Failure(code)
    END
  END Length;

PROCEDURE Flush(wr: T) RAISES {Wr.Failure} =
  BEGIN
    IF wr.cur > wr.lo THEN
      TRY EmptyBuffer (wr);
      EXCEPT OSError.E(code) => RAISE Wr.Failure(code)
      END;
    END
  END Flush;

PROCEDURE EmptyBuffer(wr: T) RAISES {OSError.E} =
  VAR buffered := wr.cur - wr.lo;  start := 0;  n: INTEGER;
  BEGIN
    WHILE (buffered > 0) DO
      n := MIN (buffered, BIG);
      wr.targetH.write(
        SUBARRAY(LOOPHOLE(ADR(wr.buff[start]), ByteArrayPtr)^, 0, n));
      DEC (buffered, n);
      INC (start, n);
    END;
    (* the file position is now wr.cur *)
    wr.lo := wr.cur;
    wr.hi := wr.cur + NUMBER(wr.buff^);
  END EmptyBuffer;

PROCEDURE PutString (wr: T; READONLY buf: ARRAY OF CHAR)
  RAISES {Wr.Failure} =
  VAR toWrite, start, n: INTEGER;
  BEGIN
    IF NUMBER(buf) <= wr.hi - wr.cur THEN
      SUBARRAY(wr.buff^, wr.cur - wr.lo, NUMBER(buf)) := buf;
      INC(wr.cur, NUMBER(buf));
    ELSE
      Flush(wr);
      IF 2 * NUMBER(buf) < NUMBER(wr.buff^) THEN
        SUBARRAY(wr.buff^, 0, NUMBER(buf)) := buf;
        INC(wr.cur, NUMBER(buf));
      ELSE
        TRY
          toWrite := NUMBER (buf);
          start := 0;
          WHILE toWrite > 0 DO
            n := MIN (toWrite, BIG);
            wr.targetH.write(
              SUBARRAY(LOOPHOLE(ADR(buf[start]), ByteArrayPtr)^, 0, n));
            DEC (toWrite, n);
            INC (start, n);
          END;
          INC(wr.cur, NUMBER(buf));
          wr.lo := wr.cur;
          wr.hi := wr.cur + NUMBER(wr.buff^);
        EXCEPT
        | OSError.E (code) => RAISE Wr.Failure(code);
        END
      END 
    END
  END PutString;

PROCEDURE Close(wr: T) RAISES {Wr.Failure} =
  BEGIN
    TRY
      wr.targetH.close()
    EXCEPT OSError.E(code) => RAISE Wr.Failure(code)
    END;
  END Close;

BEGIN
END FileWr.
