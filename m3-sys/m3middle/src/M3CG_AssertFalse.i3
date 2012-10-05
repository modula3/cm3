INTERFACE M3CG_AssertFalse;

(* This asserts false in every function
 * and is a good basis for M3CG passes that must
 * override every method. *)

IMPORT M3CG;

TYPE T <: Public;
TYPE Public = M3CG.T OBJECT
END;

END M3CG_AssertFalse.
