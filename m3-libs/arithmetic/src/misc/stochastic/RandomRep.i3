INTERFACE RandomRep;
(*Copyright (c) 1996, m3na project

Abstract: Private interface to RandomBasic
Used to put object wrapper on RNG's.

3/23/96  Harry George    Initial version
*)
IMPORT LongRealBasic AS R;
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
    gauss_y:R.T;
  END;
(*-------------------*)
PROCEDURE Uniform(self:RandomGen;
                  min:R.T:=0.0D0;  (*from min*)
                  max:R.T:=1.0D0   (*up to but not including max*)
                  ):R.T;           (*return uniform deviate*)

PROCEDURE Exponential(self:RandomGen):R.T;

PROCEDURE NormalDev(self:RandomGen) : R.T;

PROCEDURE GammaDev(self:RandomGen;
                   a : R.T) : R.T;

PROCEDURE Dirichlet(self:RandomGen;
                    p:R.Array);

PROCEDURE Poisson(self:RandomGen;
                     m:R.T    (*mean*)
                     ):R.T;
PROCEDURE Binomial(self:RandomGen;
                     p:R.T;  (*probability*)
                     n:INTEGER  (*trials*)
                     ):R.T;

(*==========================*)
END RandomRep.
