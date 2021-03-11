(* $Id$ *)

INTERFACE BDDImpl;
IMPORT BDD;
(* this interface is for Modula-3 routines that know the representation *)
(* of the BDD_t's.  It should not be used lightly! *)
(* This interface is implemented by BDD.m3 *)

(* get the RHS child of a node *)
PROCEDURE Right(a : BDD.T) : BDD.T;

(* get the LHS child of a node *)
PROCEDURE Left (a: BDD.T) : BDD.T;

(* get the literal mentioned in a node *)
PROCEDURE NodeVar(v : BDD.T) : BDD.T;

END BDDImpl.
