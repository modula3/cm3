(* $Id$ *)

MODULE RdWrReset;
IMPORT Rd AS R, Wr AS W;
IMPORT RdClass, WrClass;
<*NOWARN*>IMPORT UnsafeWr, UnsafeRd;

(* Since we need to use the Mutex properties of Rd.T and Wr.T, we
   should actually import UnsafeWr and UnsafeRd. We need to add the
   following revelations, as the comment in UnsafeRd points out, if we
   want to include both the Unsafe* and *Class interfaces. *)
REVEAL RdClass.Private <: MUTEX;
REVEAL WrClass.Private <: MUTEX;

PROCEDURE Rd (rd: R.T) =
  BEGIN
    LOCK rd DO DEC(rd.cur, rd.lo); DEC(rd.hi, rd.lo); rd.lo := 0; END;
  END Rd;
          
PROCEDURE Wr (wr: W.T) =
  BEGIN
    LOCK wr DO DEC(wr.cur, wr.lo); DEC(wr.hi, wr.lo); wr.lo := 0; END;
  END Wr;

BEGIN END RdWrReset.
