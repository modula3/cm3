INTERFACE RandomNumber02;
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
      myrand:=NEW(RandomNumber02.slow).init();
    BEGIN
      value:=myrand.uniform();
    END;

WARNING: RandomNumber02 uses global data.  Ordinally that could cause problems
with threads.  If race conditions did occur, and you needed to
replicate a sequences of numbers, then you would need to lock the
thread in which RNG02 was called.


3/23/96  Warren Smith    Initial version
*)

IMPORT Word,RandomBasic;

TYPE
  slow <: RandomBasic.T;
  fast <: RandomBasic.T;



(*==========================*)
(*** Initializes all random number generators here. Quite slow.
If NonReproducible=TRUE (the default) will incorporate the
   time into the seed.
If FALSE will use a particular fixed seed.
*************************************************************)
PROCEDURE Init(NonReproducible : BOOLEAN := TRUE);

(** The random words output by these generators ought to be extremely
random since RandWord is a combination of 5 generators, each pretty good by
itSELF, and all 5 work according to different principles. I think
they will be good enough for any application except cryptography, and
quite likely good enough even for that. Its only
disadvantage is it is too slow for applications which do
a very small amount of computing per random number.
**********************************************)
PROCEDURE RandWord() : Word.T;
PROCEDURE Uni01() : LONGREAL;

(** The below are faster but less-random versions based on combining
only 2 generators, not 5. They should still be very random. If you
are doing a serious Monte-Carlo experiment, then just to be sure you
should probably try both the slow & fast versions to see if the
slow ones yield different statistics. Any such case is evidence that
the fast generator has a weakness. In the event you find any such
statistical evidence of flaws in any of these generators, please
tell me at wds@research.NJ.NEC.COM.
************************************************************)
PROCEDURE FasterRandWord() : Word.T;
PROCEDURE FasterUni01() : LONGREAL;


(*-----------------------*)
PROCEDURE Test();
(*internal test loop for 10,000,000 samples*)
(*==========================*)
END RandomNumber02.
