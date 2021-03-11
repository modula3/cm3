(* $Id$ *)

INTERFACE Bool;
IMPORT CPtr, Cbool;
IMPORT Word;
IMPORT BoolSet;
IMPORT BoolTextTbl;

CONST Brand = "Bool from Rajit";

TYPE 
  T = CPtr.T;

PROCEDURE New      () : T;
PROCEDURE And      (a , b : T) : T;
PROCEDURE Or       (a , b : T) : T;
PROCEDURE Xor      (a , b : T) : T;
PROCEDURE Equivalent(a, b : T) : T;
PROCEDURE Not      (a : T) : T;
PROCEDURE Copy     (a : T) : T;
PROCEDURE Implies  (a , b : T) : T;

(* make variable "v" true or false in the expression "a" *)
PROCEDURE MakeTrue (a , v : T) : T;
PROCEDURE MakeFalse(a , v : T) : T;

(* Boolean difference df/dx *)
PROCEDURE Difference(f, x : T) : T;

(* Boolean smoothing S_x f *)
PROCEDURE Smooth(f, x : T) : T;

(* choose "if" if c is false, "it" if c is true *)
PROCEDURE Choose(c : T; it, if : T) : T;

(* for debugging *)
PROCEDURE Print(a : T); 

(* for converting from a ( bool_t * ) from a C program *)
PROCEDURE FromC(cbool : Cbool.t) : T;

PROCEDURE True     () : T;
PROCEDURE False    () : T;

(* initialize global lattice *)
<*OBSOLETE*>PROCEDURE Init(); 

(* hint to garbage collect *)
PROCEDURE GC();

(* for debugging *)
PROCEDURE GetId(a : T) : INTEGER;

(* equal is for generics, not for generating Ts (use Equivalent for that) *)
PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE Hash(a : T) : Word.T;

(* find all the literals that a depends on *)
PROCEDURE Vars(a : T) : BoolSet.T;

(* format according to symtab *)
PROCEDURE Format(b : T; symTab : BoolTextTbl.T; pfx := "") : TEXT; 
VAR frees : CARDINAL;

END Bool.

