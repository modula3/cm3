INTERFACE RandomCombinedSlow;
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
      myrand:=NEW(RandomCombinedSlow.T).init();
    BEGIN
      value:=myrand.uniform();
    END;


3/23/96  Warren Smith    Initial version
*)

IMPORT Word,RandomBasic;

TYPE
  T <: RandomBasic.T;



(*==========================*)
(*** Initializes all random number generators here. Quite slow.
If fixed=FALSE (the default) will incorporate the
   time into the seed.
If TRUE will use a particular fixed seed.
*************************************************************)
PROCEDURE New(fixed : BOOLEAN := FALSE):T;

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

(*-----------------------*)
PROCEDURE Test();
(*internal test loop for 10,000,000 samples*)
(*==========================*)
END RandomCombinedSlow.
