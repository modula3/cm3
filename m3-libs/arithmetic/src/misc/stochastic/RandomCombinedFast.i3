INTERFACE RandomCombinedFast;
(*Gnu CopyLefted.*)
(*
Abstract: Psuedo-random number generator by Warren D. Smith.
Runtimes (in 100 MHz pentium clock ticks per call, roughly):
    RandWord           470
    Uni01             1000
    FasterUni01         80
    FasterRandWord      80

Usage:  You can call these directly, or use the T OBJECTS:
    VAR
      myrand:=NEW(RandomCombinedFast.T).init();
    BEGIN
      value:=myrand.uniform();
    END;


3/23/96  Warren Smith    Initial version
*)

IMPORT Word,RandomBasic;

(** The below are faster than the slow variant
but less-random versions based on combining
only 2 generators, not 5. They should still be very random. If you
are doing a serious Monte-Carlo experiment, then just to be sure you
should probably try both the slow & fast versions to see if the
slow ones yield different statistics. Any such case is evidence that
the fast generator has a weakness. In the event you find any such
statistical evidence of flaws in any of these generators, please
tell me at wds@research.NJ.NEC.COM.
************************************************************)

TYPE
  T <: RandomBasic.T;


(*==========================*)
(*** Initializes all random number generators here. Quite slow.
If fixed=FALSE (the default) will incorporate the
   time into the seed.
If TRUE will use a particular fixed seed.
*************************************************************)
PROCEDURE New(fixed : BOOLEAN := FALSE):T;

(*==========================*)
END RandomCombinedFast.
