INTERFACE M3CG_DoNothing;

(* This does nothing and is a good base for small M3CG passes. *)

IMPORT M3CG;

TYPE T <: Public;
TYPE Public = M3CG.T OBJECT
END;

PROCEDURE New (): T;
(* Return a fresh T with non-NIL varSentinel/procSentinel.
   Use this for direct-use backends (e.g. MSIRObj mode) where m3front
   assertions require non-NIL Var/Proc values from CG calls.
   Use NEW(T) / NEW(SubType) instead when NIL sentinels are needed
   (e.g. M3C.m3 multi-pass subtypes that rely on NIL for slot detection). *)

END M3CG_DoNothing.
