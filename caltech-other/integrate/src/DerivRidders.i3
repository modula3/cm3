(* $Id$ *)

INTERFACE DerivRidders;
IMPORT LRFunction AS Function;

(* take derivative of func using Ridders's method *)

PROCEDURE Deriv(func : Function.T; 
                x : LONGREAL;      (* point at which deriv is taken *)
                h : LONGREAL;      (* "big" step in x; must be nonzero *)
                VAR err : LONGREAL (* estimate of error *)
               ) : LONGREAL;

END DerivRidders.
