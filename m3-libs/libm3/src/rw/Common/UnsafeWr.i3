(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Jun 18 18:01:59 PDT 1993 by wobber     *)
(*      modified on Sat Feb 29 08:20:48 PST 1992 by kalsow     *)
(*      modified on Fri Feb  7 00:03:21 PST 1992 by muller     *)



(* The routines in the UnsafeWr and UnsafeRd interfaces are like the
corresponding routines in the Wr and Rd interfaces, but it is the
client's responsibility to lock the stream before calling them.  The
lock can be acquired once and held for several operations, which is
faster than acquiring the lock for each operation, and also makes the
whole group atomic.  Danger is the price of speed: it is an unchecked
runtime error to call one of these operations without locking the
stream.

The UnsafeWr interface also provides routines for formatted printing
of integers and reals.  Using them is more efficient but less
convenient than using the Fmt interface (described in the first
edition of the Modula-3 report.  For example, the statement

	Wr.PutText(wr, "Line " & Fmt.Int(n) & " of file " & f)

could be replaced with the following faster code:

	LOCK wr DO
	  FastPutText(wr, "Line ");
	  FastPutInt (wr, n);
	  FastPutText(wr, " of file ");
	  FastPutText(wr, f)
  	END

If several threads are writing characters concurrently to the same
writer, treating each PutChar as an atomic action is likely to produce
inscrutable output---it is usually preferable if the units of
interleaving are whole lines, or even larger.  It is therefore
convenient as well as efficient to import UnsafeWr and use LOCK
clauses like the one above to make small groups of output atomic.  But
don't forget to acquire the lock! If you call one of the routines in
this interface without it, then the unsafe code in WrRep might crash
your program in a rubble of bits.  A historical note: the main public
interface to Modula-2+ writers used the unsafe, unmonitored routines.
Errors were fairly frequent, mostly because of concurrent calls to
Wr.Flush or Wr.Close, which often occur as implicit finalization
actions when the programmer doesn't expect them.  In the Modula-3
design we have therefore made the main interfaces safe.

Here is the interface: *)

INTERFACE UnsafeWr;
IMPORT Wr, Thread, Convert; 
FROM Thread IMPORT Alerted;
FROM Wr IMPORT Failure;

REVEAL
  Wr.T <: Thread.Mutex;


(* Thus an importer of UnsafeWr can write code like LOCK wr DO ... END.*)


PROCEDURE FastPutChar(wr: Wr.T; ch: CHAR) RAISES {Failure, Alerted};
(* Like Wr.PutChar, but wr must be locked (as in all routines in
the interface). *)

PROCEDURE FastPutText(wr: Wr.T; t: TEXT) RAISES {Failure, Alerted};
(* Like Wr.PutText. *)

PROCEDURE FastPutString(wr: Wr.T; READONLY a: ARRAY OF CHAR)
  RAISES {Failure, Alerted};
(* Like Wr.PutString. *)

PROCEDURE FastPutInt(wr: Wr.T; n: INTEGER; base: Convert.Base := 10) 
  RAISES {Failure, Alerted};
(* Like Wr.PutText(wr, Fmt.Int(n, base)). *)

PROCEDURE FastPutReal(
  wr: Wr.T;
  r: REAL; 
  precision: CARDINAL := 6;
  style := Convert.Style.Mix)
  RAISES {Failure, Alerted};
(* Like Wr.PutText(wr, Fmt.Real(wr, precision, style)). *)

PROCEDURE FastPutLongReal(
  wr: Wr.T; 
  r: LONGREAL; 
  precision: CARDINAL := 6;
  style := Convert.Style.Mix)
  RAISES {Failure, Alerted};
(* Like Wr.PutText(wr, Fmt.LongReal(wr, precision, style)). *)

PROCEDURE FastClose (wr: Wr.T) RAISES {Failure, Alerted};

END UnsafeWr.
