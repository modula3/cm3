(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Jan 26 13:58:35 PST 1995 by kalsow     *)
(*      modified on Fri Jun 18 18:12:46 PDT 1993 by wobber     *)
(*      modified on Tue Jun 15 13:41:05 1993 by gnelson        *)
(*      modified on Mon May 31 06:25:34 PDT 1993 by swart      *)
(*      modified on Mon Apr 26 17:22:58 PDT 1993 by mcjones    *)
(*      modified on Tue Apr 21 15:56:06 PDT 1992 by muller     *)

(* This module is very similar to the WrMove module, so we will list
its code with only a few comments.  *)

MODULE RdMove EXPORTS Rd, RdClass, UnsafeRd;
IMPORT Thread;
FROM Thread IMPORT Alerted;

REVEAL
  Private = Thread.Mutex BRANDED OBJECT END;

(* FastGetChar and GetChar are identical except that GetChar acquires
   and releases the lock while FastGetChar assumes it is already held. *)

(* It is invariant that for a closed reader "rd", "rd.buff = NIL" and
   "rd.lo = rd.hi".  Therefore the check that "rd" is ready need
   not inspect "rd.closed" on the fast path. *)

<*INLINE*>
PROCEDURE GetChar (rd: T): CHAR RAISES {EndOfFile, Failure, Alerted} =
  VAR res: CHAR; BEGIN
    LOCK rd DO
      IF rd.cur = rd.hi THEN DoSeek(rd) END;
      res := rd.buff[rd.st + (rd.cur - rd.lo)];
      INC(rd.cur);
      RETURN res
    END
  END GetChar;

<*INLINE*>
PROCEDURE FastGetChar(rd: T): CHAR
    RAISES {EndOfFile, Failure, Alerted} =
    (* rd is locked *)
  VAR res: CHAR; BEGIN
    IF rd.cur = rd.hi THEN DoSeek(rd) END;
    res := rd.buff[rd.st + (rd.cur - rd.lo)];
    INC(rd.cur);
    RETURN res
  END FastGetChar;

PROCEDURE DoSeek(rd: T) RAISES {EndOfFile, Failure, Alerted} =
  BEGIN
    (* rd.cur = rd.hi here *)
    IF rd.closed THEN Die() END;
    IF rd.seek(rd.cur, FALSE) = SeekResult.Eof THEN
      RAISE EndOfFile
    END
  END DoSeek;
  
PROCEDURE GetSub (rd: T; VAR (*out*) str: ARRAY OF CHAR): CARDINAL
  RAISES {Failure, Alerted} =
  BEGIN
    LOCK rd DO
      IF rd.closed THEN Die() END;
      RETURN rd.getSub(str)
    END
  END GetSub;
  
PROCEDURE FastGetSub (rd: T; VAR (*out*) str: ARRAY OF CHAR): CARDINAL
  RAISES {Failure, Alerted} =
  BEGIN 
    IF rd.closed THEN Die() END;
    RETURN rd.getSub(str) 
  END FastGetSub;

PROCEDURE GetSubDefault (rd: T; VAR (*out*) str: ARRAY OF CHAR): CARDINAL
  RAISES {Failure, Alerted} =
  VAR i := 0; BEGIN
    LOOP
      (* i chars have been read into str *)
      IF i = NUMBER(str) THEN EXIT END;
      IF rd.cur = rd.hi THEN
        IF rd.seek(rd.cur, FALSE) = SeekResult.Eof THEN EXIT END
      END;
      (* rd.lo <= rd.cur < rd.hi *)
      VAR n := MIN(rd.hi - rd.cur, NUMBER(str) - i); BEGIN
        SUBARRAY(str, i, n) :=
          SUBARRAY(rd.buff^, rd.cur - rd.lo + rd.st, n);
        INC(i, n);
        INC(rd.cur, n)
      END
    END;
    RETURN i
  END GetSubDefault;


(* EOF and FastEOF are identical except that EOF acquires and releases
   the reader lock while FastEOF assumes it is already held. *)

<*INLINE*> 
PROCEDURE EOF (rd: T): BOOLEAN RAISES {Failure, Alerted} =
  (* rd is unlocked *)
  BEGIN
    LOCK rd DO
      IF rd.cur # rd.hi THEN
        RETURN FALSE
      ELSE
        IF rd.closed THEN Die() END;
        RETURN rd.seek(rd.cur, FALSE) = SeekResult.Eof
      END
    END
  END EOF;
  
<*INLINE*> 
PROCEDURE FastEOF (rd: T): BOOLEAN RAISES {Failure, Alerted} =
  BEGIN
    (* rd is locked *)
    IF rd.cur # rd.hi THEN
      RETURN FALSE
    ELSE
      IF rd.closed THEN Die() END;
      RETURN rd.seek(rd.cur, FALSE) = SeekResult.Eof
    END
  END FastEOF;

PROCEDURE UnGetChar(rd: T) RAISES {} =
  BEGIN
    LOCK rd DO FastUnGetChar (rd) END;
  END UnGetChar;

PROCEDURE FastUnGetChar(rd: T) RAISES {} =
  BEGIN
    IF rd.closed OR rd.cur = rd.lo THEN Die() END;
    DEC(rd.cur)
  END FastUnGetChar;

PROCEDURE CharsReady(rd: T): CARDINAL RAISES {Failure} =
  <*FATAL Thread.Alerted*>
  BEGIN
    LOCK rd DO
      IF rd.cur = rd.hi THEN
        IF rd.closed THEN Die() END;
        IF rd.seek(rd.cur, TRUE) = SeekResult.Eof THEN RETURN 1 END
      END;
      RETURN rd.hi - rd.cur;
    END
  END CharsReady;

PROCEDURE Index(rd: T): CARDINAL RAISES {} =
  BEGIN
    LOCK rd DO
      IF rd.closed THEN Die() END;
      RETURN rd.cur
    END
  END Index;

PROCEDURE Length(rd: T): INTEGER RAISES {Failure, Alerted} =
  BEGIN
    LOCK rd DO
      IF rd.closed THEN Die() END;
      RETURN rd.length()
    END
  END Length;

PROCEDURE Seek(rd: T; n: CARDINAL) RAISES {Failure, Alerted} =
  BEGIN
    LOCK rd DO
      IF rd.closed OR NOT rd.seekable THEN Die() END;
      IF n < rd.lo OR n > rd.hi THEN
        EVAL rd.seek(n, FALSE);
      ELSE
        rd.cur := n;
      END
    END
  END Seek;

PROCEDURE Close(rd: T) RAISES {Failure, Alerted} =
  BEGIN
    LOCK rd DO FastClose (rd); END;
  END Close;

PROCEDURE FastClose(rd: T) RAISES {Failure, Alerted} =
  BEGIN
    IF NOT rd.closed THEN
      TRY
        rd.close()
      FINALLY
        rd.closed := TRUE;
        rd.cur := rd.hi;
        rd.lo := rd.hi;
        rd.buff := NIL
      END
    END
  END FastClose;

PROCEDURE Lock (rd: T) RAISES {} =
  BEGIN
    Thread.Acquire (rd)
  END Lock;

PROCEDURE Unlock (rd: T) RAISES {} =
  BEGIN
    Thread.Release (rd)
  END Unlock;

PROCEDURE LengthDefault (<*UNUSED*> rd: T): INTEGER RAISES {} =
  BEGIN
    <*NOWARN*> Die()
  END LengthDefault;

PROCEDURE CloseDefault(<*UNUSED*> rd: T) RAISES {} =
  BEGIN END CloseDefault;


EXCEPTION FatalError;

PROCEDURE Die() =
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError;
  END Die;

BEGIN
END RdMove.
