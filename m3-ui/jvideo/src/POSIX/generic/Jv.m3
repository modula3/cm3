(* Copyright (C) 1989, 1990, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Tue Feb  7 11:22:30 PST 1995 by kalsow   *)
(*      modified on Fri Jan  7 14:56:04 PST 1994 by msm      *)
(*      modified on Thu Oct 21 17:03:16 PDT 1993 by sfreeman *)

UNSAFE MODULE Jv;

IMPORT Atom, AtomList, Ctypes, OSError, OSErrorPosix, Rd, Thread, Wr;

REVEAL
  T = Public BRANDED OBJECT
        rd: Rd.T;
        wr: Wr.T;
      OVERRIDES
        init  := Init;
        close := Close;
      END;

PROCEDURE Init (<*UNUSED*> t: T;
                <*UNUSED*> pipeName: TEXT): T RAISES {OSError.E} =
  (* open Unix domain connection to server. *)
  BEGIN
    OSErrorPosix.Raise();
    RETURN NIL;
  END Init;

PROCEDURE Close (t: T) =
  BEGIN
    TRY
      Wr.Close(t.wr);
      Rd.Close(t.rd);
    EXCEPT
    | Thread.Alerted, Wr.Failure, Rd.Failure => (* skip *)
    END;
  END Close;

(* -- send and receiving stuff, assume BYTESIZE(CHAR) = byte -- *)
TYPE
  LongArrayPtr =
    UNTRACED REF
      ARRAY [0 .. ((LAST(CARDINAL) DIV (BITSIZE(CHAR)*8)) - 1)] OF CHAR;
(* hack to get around type system.  The horrible formula for the length of
   the array gives the longest possible array *)

PROCEDURE Send (t: T; buf: ADDRESS; nbytes: CARDINAL)
  RAISES {OSError.E, Thread.Alerted} =
  BEGIN
    TRY
      WITH chars = LOOPHOLE(buf, LongArrayPtr) DO
        Wr.PutString(t.wr, SUBARRAY(chars^, 0, nbytes));
      END;
      Wr.Flush(t.wr);
    EXCEPT
    | Wr.Failure (e) =>
        RAISE OSError.E(AtomList.Cons(Atom.FromText("Write"), e));
    END;
  END Send;

PROCEDURE Recv (t: T; buf: ADDRESS; nbytes: CARDINAL)
  RAISES {OSError.E, Thread.Alerted} =
  VAR nchars: CARDINAL;
  BEGIN
    TRY
      WITH chars = LOOPHOLE(buf, LongArrayPtr) DO
        nchars := Rd.GetSub(t.rd, SUBARRAY(chars^, 0, nbytes));
      END;
      IF nchars < nbytes THEN
        RAISE
          OSError.E(AtomList.List2(ServerFailure, Atom.FromText("Read")));
      END;
    EXCEPT
    | Rd.Failure (e) =>
        RAISE OSError.E(AtomList.Cons(Atom.FromText("Read"), e));
    END;
  END Recv;

BEGIN
  <* ASSERT BYTESIZE(CHAR) = BYTESIZE(Ctypes.char) *>
  (* dumb consistency check *)
  ServerFailure := Atom.FromText("Jv ServerFailure");

END Jv.
