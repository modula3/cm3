INTERFACE RandomRep;
(*Copyright (c) 1996, m3na project

Abstract: Private interface to RandomBasic
Used to put object wrapper on RNG's.

3/23/96  Harry George    Initial version
*)
IMPORT xReal64 AS R;
FROM xReal64 IMPORT REAL64;
FROM RandomBasic IMPORT RandomGen,PublicRandomGen;

(*==========================*)
CONST
  TableSize = 32; (*for Bayes-Durham shuffle*)

REVEAL
  RandomGen <: PrivateRandomGen;

TYPE
  PrivateRandomGen = PublicRandomGen BRANDED OBJECT
    start:BOOLEAN;
    z1,z2,table_z:INTEGER;
    table:ARRAY[0..TableSize-1] OF INTEGER;
    gauss_y:REAL64;
  END;
(*-------------------*)
PROCEDURE Uniform(self:RandomGen;
                  min:REAL64:=0.0D0;  (*from min*)
                  max:REAL64:=1.0D0   (*up to but not including max*)
                  ):REAL64;           (*return uniform deviate*)

PROCEDURE Exponential(self:RandomGen):REAL64;

PROCEDURE NormalDev(self:RandomGen) : REAL64;

PROCEDURE GammaDev(self:RandomGen;
                   a : REAL64) : REAL64;

PROCEDURE Dirichlet(self:RandomGen;
                    p:R.Array);

PROCEDURE Poisson(self:RandomGen;
                     m:REAL64    (*mean*)
                     ):REAL64;
PROCEDURE Binomial(self:RandomGen;
                     p:REAL64;  (*probability*)
                     n:INTEGER  (*trials*)
                     ):REAL64;

(*==========================*)
END RandomRep.
