(* Copyright (c) 2000 Mika Nystrom.  All Rights Reserved. *)
(* $Id$ *)
INTERFACE Quantity;
IMPORT RefSet;

EXCEPTION 
  Recursive;
  IllegalOperands(T);

TYPE
  Vector = REF ARRAY OF T;
  Matrix = REF ARRAY OF ARRAY OF T;

  T <: Public;

  Public = OBJECT
  METHODS
    format(printValues : BOOLEAN := FALSE) : TEXT RAISES { Recursive } ;
    value() : LONGREAL RAISES { Recursive, IllegalOperands };  (* current value *)
    derivative(wrt : REF LONGREAL) : Public RAISES { Recursive };  (* analytic deriv. *)

    (* shouldnt really let the REF LONGREALs slip out like this *)
    variables(): RefSet.T RAISES { Recursive };                    (* RefSet of variables *)

    (* can set: only if a literal! *)
    set(newValue : LONGREAL);
  END;
  
PROCEDURE Mul(x,y:T) : T ;
PROCEDURE Add(x,y:T) : T ;
PROCEDURE Sub(x,y:T) : T ;
PROCEDURE Div(x,y:T) : T ;
PROCEDURE Pow(x,p:T) : T ;

PROCEDURE Neg(x:T) : T;
PROCEDURE Exp(x:T) : T;
PROCEDURE Log(x:T) : T;
PROCEDURE Sqrt(x:T) : T;
PROCEDURE Square(x:T): T ;

(* create new variable *)
PROCEDURE Variable(x : LONGREAL; name : TEXT := NIL) : T ;  

PROCEDURE Constant(x : LONGREAL) : T ;  (* create new constant *)

VAR Zero, One : T;

END Quantity.

