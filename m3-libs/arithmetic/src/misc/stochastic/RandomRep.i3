INTERFACE RandomRep;
(* Arithmetic for Modula-3, see doc for details

   Abstract: Private interface to RandomBasic Used to put object wrapper on
   RNG's.

   3/23/96 Harry George Initial version *)

IMPORT LongRealBasic AS R;
FROM RandomBasic IMPORT T, TPublic;
FROM Arithmetic IMPORT Error;


REVEAL T <: TPrivate;

TYPE TPrivate = TPublic BRANDED OBJECT END;

PROCEDURE Uniform (SELF: T;
                   min : R.T := R.Zero; (* from min *)
                   max : R.T := R.One; (* up to but not including max *)
  ): R.T RAISES {Error};         (* return uniform deviate *)

PROCEDURE Exponential (SELF: T; ): R.T;

PROCEDURE NormalDev (SELF: T; ): R.T;

PROCEDURE GammaDev (SELF: T; a: R.T; ): R.T;

PROCEDURE Dirichlet (SELF: T; p: R.Array; );

(*
PROCEDURE Poisson(SELF:T;
                     m:R.T  ;  (* mean *)
                     ):R.T;
*)
PROCEDURE Binomial (SELF: T; p: R.T; n: CARDINAL; ): CARDINAL;


END RandomRep.
