(* $Id$ *)

GENERIC INTERFACE SXNumOps(Elem);
IMPORT SXBool, SXInt;

PROCEDURE Abs(a : Elem.T) : Elem.T;    (* ABS(a) *)
PROCEDURE UMinus(a : Elem.T) : Elem.T; (*    -a  *)

PROCEDURE Times(a, b : Elem.T; shortCircuit := TRUE) : Elem.T;
PROCEDURE Plus(a, b : Elem.T) : Elem.T;
PROCEDURE Minus(a, b : Elem.T) : Elem.T;
PROCEDURE Div(a, b : Elem.T) : Elem.T;
PROCEDURE Mod(a, b : Elem.T) : Elem.T;

PROCEDURE Min(a, b : Elem.T) : Elem.T;
PROCEDURE Max(a, b : Elem.T) : Elem.T;

PROCEDURE Equal(a, b : Elem.T) : SXBool.T;
PROCEDURE Compare(a, b : Elem.T) : SXInt.T;

PROCEDURE GT(a, b : Elem.T) : SXBool.T;
PROCEDURE LT(a, b : Elem.T) : SXBool.T;
PROCEDURE GE(a, b : Elem.T) : SXBool.T;
PROCEDURE LE(a, b : Elem.T) : SXBool.T;

VAR (* CONST *) Zero, One, NegOne : Elem.Base; 

PROCEDURE Sum(READONLY a : ARRAY OF Elem.T) : Elem.T;
PROCEDURE Prod(READONLY a : ARRAY OF Elem.T) : Elem.T;

PROCEDURE WeightedSum(READONLY w : ARRAY OF Elem.Base; 
                      READONLY a : ARRAY OF Elem.T) : Elem.T;

PROCEDURE Average(READONLY a : ARRAY OF Elem.T; u : CARDINAL := 0) : Elem.T;
  (* performs DivB(Sum(a),NUMBER(a))

     -- this is perhaps not that useful for integers 
     
     u is the number of entries allowed to be undefined
  *)

(* helper routines *)

PROCEDURE SumB(READONLY a : ARRAY OF Elem.Base) : Elem.Base;
PROCEDURE ProdB(READONLY a : ARRAY OF Elem.Base) : Elem.Base;

PROCEDURE DivB(a, b : Elem.Base) : Elem.Base;
  (* N.B. DivB has different implementations for Real and Int *)

END SXNumOps.
