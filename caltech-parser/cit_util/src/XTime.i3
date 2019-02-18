(* $Id$ *)

INTERFACE XTime;
IMPORT Time;

(* 

   XTime is a controllable version of the Time interface.

   The data type is the same, and the same routines are provided (Now).

   The difference is that XTime can be set to "fake" time, to do 
   system testing (e.g.).

   System can conveniently be initialized by providing an address for a 
   TCP XTime server on the command line:

   @M3xtime=<host>:<port>

*)

TYPE T = Time.T;

PROCEDURE Now() : T;

VAR (* CONST *) Grain : T;

PROCEDURE SetXTime(to : T; 
                   adjust := TRUE; absRate := 0.5d0; maxDelta := 30.0d0)
  RAISES { CantAdjust };
  (* can only raise CantAdjust if adjust is TRUE *)

PROCEDURE SetOffset(to : LONGREAL); 
  (* a step change *)

EXCEPTION CantAdjust;

PROCEDURE AdjustOffset(to : T; absRate := 0.5d0; maxDelta := 30.0d0) 
  RAISES { CantAdjust };
  (* change gradually, so that time doesn't run backwards.

     If the delta required is more than maxDelta seconds, raise CantAdjust.

     At this point the caller should either abort or call SetOffset, above.
  *)

END XTime.
