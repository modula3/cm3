(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Aug 16 13:18:52 PDT 1996 by heydon     *)
(*      modified on Tue Jun 22 16:54:39 PDT 1993 by wobber     *)
(*      modified on Mon May 31 06:25:00 PDT 1993 by swart      *)
(*      modified on Mon Mar 09 13:46:38 PST 1992 by muller     *)
(*      modified on Sat Feb 29 08:20:22 PST 1992 by kalsow     *)

MODULE WrMove EXPORTS Wr, WrClass, UnsafeWr;
IMPORT Thread, Convert, Text, TextF;
FROM Thread IMPORT Alerted;

REVEAL
  Private = Thread.Mutex BRANDED OBJECT END;

(* FastPutChar and PutChar are identical except that PutChar acquires
   and releases the lock while FastPutChar assumes it is already held. *)

(* It is invariant that for a closed writer "wr", "wr.buff = NIL" and
   "wr.lo = wr.hi".  Therefore the check that "wr" is ready need
   not inspect "wr.closed" on the fast path. *)

PROCEDURE Lock(wr: T) RAISES {} =
  BEGIN 
    Thread.Acquire(wr);
  END Lock;

PROCEDURE Unlock(wr: T) =
  BEGIN
    Thread.Release(wr)
  END Unlock;

<*INLINE*>
PROCEDURE PutChar (wr: T; ch: CHAR) RAISES {Failure, Alerted} =
  BEGIN
    LOCK wr DO
      IF wr.cur = wr.hi THEN DoSeek(wr) END;
      wr.buff[wr.st + wr.cur - wr.lo] := ch;
      INC(wr.cur);
      IF NOT wr.buffered THEN wr.flush(); END;
    END;
  END PutChar;

<*INLINE*>
PROCEDURE FastPutChar (wr: T; ch: CHAR) RAISES {Failure, Alerted} =
  BEGIN
    IF wr.cur = wr.hi THEN DoSeek(wr) END;
    wr.buff[wr.st + wr.cur - wr.lo] := ch;
    INC(wr.cur);
    IF NOT wr.buffered THEN wr.flush(); END;
  END FastPutChar;

PROCEDURE DoSeek (wr: T) RAISES {Failure, Alerted} =
  BEGIN
    (* wr.cur = wr.hi here *)
    IF wr.closed THEN Die() END;
    wr.seek(wr.cur);
  END DoSeek;

PROCEDURE PutText (wr: T; t: TEXT) RAISES {Failure, Alerted} =
  BEGIN
    PutString (wr, SUBARRAY (t^, 0, Text.Length (t)));
  END PutText;

PROCEDURE FastPutText (wr:T; t: TEXT) RAISES {Failure, Alerted} =
  BEGIN
    FastPutString (wr, SUBARRAY (t^, 0, Text.Length (t)));
  END FastPutText;

(* PutString and FastPutString are identical except that PutString acquires
   and releases the lock while FastPutString assumes it is already held. *)

<*INLINE*>
PROCEDURE PutString (wr: T; READONLY a: ARRAY OF CHAR)
  RAISES {Failure, Alerted} =
  BEGIN
    LOCK wr DO
      IF wr.closed THEN Die() END;
      wr.putString(a);
      IF NOT wr.buffered THEN wr.flush(); END;
    END;
  END PutString;

<*INLINE*>
PROCEDURE FastPutString (wr: T; READONLY a: ARRAY OF CHAR)
  RAISES {Failure, Alerted} =
  BEGIN
    IF wr.closed THEN Die() END;
    wr.putString(a);
    IF NOT wr.buffered THEN wr.flush(); END;
  END FastPutString;

PROCEDURE PutStringDefault(wr: T; READONLY a: ARRAY OF CHAR)
    RAISES {Failure, Alerted} =
  VAR
    start: CARDINAL := 0;
    l               := NUMBER(a);
  BEGIN
    WHILE (l > 0) DO
      VAR n := MIN(wr.hi - wr.cur, l);
      BEGIN
        IF n > 0 THEN
          SUBARRAY(wr.buff^, wr.st + wr.cur - wr.lo, n) :=
                     SUBARRAY(a, start, n);
          INC(start, n);
          DEC(l, n);
          INC(wr.cur, n);
        END;
      END;
      IF l > 0 THEN wr.seek(wr.cur) END;
    END;
  END PutStringDefault;


PROCEDURE FastPutInt (wr: T; n: INTEGER; base: Convert.Base := 10)
  RAISES {Failure, Alerted} =
  <*FATAL Convert.Failed*>
  VAR
    chars: ARRAY [0..BITSIZE(INTEGER) + 3] OF CHAR;
    size:  INTEGER;
  BEGIN
    size := Convert.FromInt (chars, n, base);
    FastPutString (wr, SUBARRAY (chars, 0, size));  
  END FastPutInt;

PROCEDURE FastPutReal (wr: T; r: REAL; p: CARDINAL := 6;
                       s := Convert.Style.Mix)
  RAISES {Failure, Alerted} =
  <*FATAL Convert.Failed*>
  VAR
    chars: ARRAY [0..100] OF CHAR;
    size:  INTEGER;
  BEGIN
    size := Convert.FromFloat (chars, r, p, s);
    FastPutString (wr, SUBARRAY (chars, 0, size));
  END FastPutReal;

PROCEDURE FastPutLongReal (wr: T; r: LONGREAL; p: CARDINAL := 6;
                           s := Convert.Style.Mix)
  RAISES {Failure, Alerted} =
  <*FATAL Convert.Failed*>
  VAR
    chars: ARRAY [0..100] OF CHAR;
    size:  INTEGER;
  BEGIN
    size := Convert.FromLongFloat (chars, r, p, s);
    FastPutString (wr, SUBARRAY (chars, 0, size));
  END FastPutLongReal;

   
PROCEDURE Seek(wr: T; n: CARDINAL) RAISES {Failure, Alerted} =
  BEGIN
    LOCK wr DO
      IF wr.closed OR NOT wr.seekable THEN Die() END;
      wr.seek(n);
    END
  END Seek;

PROCEDURE Flush (wr: T) RAISES {Failure, Alerted} =
  BEGIN
    LOCK wr DO
      IF wr.closed THEN Die() END;
      wr.flush();
    END;
  END Flush;

PROCEDURE Index(wr: T): CARDINAL RAISES {} =
  BEGIN 
    LOCK wr DO 
      IF wr.closed THEN Die() END;
      RETURN wr.cur;
    END
  END Index;

PROCEDURE Length (wr: T): CARDINAL RAISES {Failure, Alerted} =
  BEGIN
    LOCK wr DO
      IF wr.closed THEN Die() END;
      RETURN wr.length ();
    END
  END Length;

PROCEDURE Close (wr: T) RAISES {Failure, Alerted} =
  BEGIN
    LOCK wr DO FastClose (wr); END;
  END Close;

PROCEDURE FastClose (wr: T) RAISES {Failure, Alerted} =
  BEGIN
    IF NOT wr.closed THEN
      TRY 
        TRY wr.flush() FINALLY wr.close() END
      FINALLY 
        wr.closed := TRUE;
        wr.cur := wr.hi;
        wr.lo := wr.hi;
        wr.buff := NIL
      END
    END
  END FastClose;

PROCEDURE Seekable (wr: T): BOOLEAN RAISES {} =
  BEGIN
    LOCK wr DO
      RETURN wr.seekable
    END
  END Seekable;

PROCEDURE Closed(wr: T): BOOLEAN RAISES {} =
  BEGIN
    LOCK wr DO
      RETURN wr.closed;
    END
  END Closed;

PROCEDURE Buffered(wr: T): BOOLEAN RAISES {} =
  BEGIN
    LOCK wr DO
      RETURN wr.buffered;
    END
  END Buffered;

PROCEDURE CloseDefault(<*UNUSED*> wr: T) RAISES {} =
  BEGIN 
  END CloseDefault;

PROCEDURE FlushDefault (<*UNUSED*> wr: T) RAISES {} =
  BEGIN
  END FlushDefault;

PROCEDURE LengthDefault(wr: T): CARDINAL RAISES {} =
  BEGIN
    RETURN wr.cur;
  END LengthDefault;


EXCEPTION FatalError;

PROCEDURE Die() =
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError;
  END Die;

BEGIN
END WrMove.

