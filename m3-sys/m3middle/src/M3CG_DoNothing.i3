INTERFACE M3CG_DoNothing;

(* This does nothing and is a good base for small M3CG passes. *)

IMPORT M3CG;

TYPE T <: Public;
TYPE Public = M3CG.T OBJECT
END;

END M3CG_DoNothing.
