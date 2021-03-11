INTERFACE BoolImpl;
IMPORT Bool;

(* this interface is for Modula-3 routines that know the representation *)
(* of the bool_t's.  It should not be used lightly! *)
(* This interface is implemented by Bool.m3 *)

PROCEDURE IsLeaf(a : Bool.T) : BOOLEAN;

(* get the RHS child of a node *)
PROCEDURE Right(a : Bool.T) : Bool.T;

(* get the LHS child of a node *)
PROCEDURE Left (a: Bool.T) : Bool.T;

(* get the literal mentioned in a node *)
PROCEDURE NodeVar(a : Bool.T) : Bool.T;

END BoolImpl.
