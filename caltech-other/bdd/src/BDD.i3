(* $Id$ *)
(* simple BDD package *)

INTERFACE BDD;
IMPORT Word;

TYPE 
  T <: REFANY;

(* the boolean constants top and bottom *)
PROCEDURE True() : T;
PROCEDURE False() : T;

(* a new variable *)
PROCEDURE New(name : TEXT := NIL) : T;

(* unary ops *)
PROCEDURE Not(a : T) : T;

(* binary ops *)
PROCEDURE And(a, b : T) : T;
PROCEDURE Xor(a , b : T) : T;
PROCEDURE Equivalent(a, b : T) : T;
PROCEDURE Or( a, b : T) : T;
PROCEDURE Implies  (a , b : T) : T;

(* maketrue/makefalse *)

PROCEDURE MakeFalse(b, v : T) : T; (* make v false in b *)
PROCEDURE MakeTrue(b, v : T) : T; (* make v true in b *)


(* print with ids *)
PROCEDURE Format(a : T; symtab : REFANY (* BDDTextTbl.T *) := NIL; pfx := "") : TEXT;

(* the following procedures allow this interface to be used in generics *)
PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE Hash(a : T) : Word.T;

PROCEDURE Size(a : T) : CARDINAL;
  (* number of nodes in structure *)

CONST
  Brand = "BDD 0.1";

PROCEDURE GetId(a : T) : INTEGER; (* for debugging *)

END BDD.
