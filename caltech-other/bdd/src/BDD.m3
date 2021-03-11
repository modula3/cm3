(* $Id$ *)
MODULE BDD;

(* this module implements higher-level BDD routines. *)
(* the lower-level implementations of And and Not are in BDDImpl.m3 *)

(*
PROCEDURE Or( a, b : T) : T =
  BEGIN RETURN Not(And(Not(a),Not(b))) END Or;
*)

PROCEDURE Implies(a, b : T) : T =
  BEGIN RETURN Not( And( a, Not(b) ) ) END Implies;

PROCEDURE Xor(a,b : T) : T =
  BEGIN RETURN Or(And(a,Not(b)), And(Not(a),b)) END Xor;

PROCEDURE Equivalent( a, b : T) : T =
  BEGIN RETURN Or(And(a,b), And(Not(a),Not(b))) END Equivalent;

<*INLINE*>PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal; 

BEGIN END BDD.
