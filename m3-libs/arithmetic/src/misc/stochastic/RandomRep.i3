INTERFACE RandomRep;
(*Copyright (c) 1996, m3na project

Abstract: Private interface to RandomBasic
Used to put object wrapper on RNG's.

3/23/96  Harry George    Initial version
*)
IMPORT LongRealBasic AS R;
FROM RandomBasic IMPORT T,TPublic;

(*==========================*)
CONST
  TableSize = 32; (*for Bayes-Durham shuffle*)

REVEAL
  T <: TPrivate;

TYPE
  TPrivate = TPublic BRANDED OBJECT
    start:BOOLEAN;
    z1,z2,table_z:INTEGER;
    table:ARRAY[0..TableSize-1] OF INTEGER;
    gauss_y:R.T;
  END;
(*-------------------*)
PROCEDURE Uniform(SELF:T;
                  min:R.T:=R.Zero;  (*from min*)
                  max:R.T:=R.One   (*up to but not including max*)
                  ):R.T;           (*return uniform deviate*)

PROCEDURE Exponential(SELF:T):R.T;

PROCEDURE NormalDev(SELF:T) : R.T;

PROCEDURE GammaDev(SELF:T;
                   a : R.T) : R.T;

PROCEDURE Dirichlet(SELF:T;
                    p:R.Array);

(*
PROCEDURE Poisson(SELF:T;
                     m:R.T    (*mean*)
                     ):R.T;
*)
PROCEDURE Binomial(SELF:T;
                     p:R.T;
                     n:CARDINAL;
                     ):CARDINAL;

(*==========================*)
END RandomRep.
