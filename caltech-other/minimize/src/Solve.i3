(* $Id$ *)
INTERFACE Solve;
IMPORT LRFunction;

(* Solve an equation using van Wijngaarden-Dekker-Brent method *)
(* WDB is threadsafe *)

PROCEDURE WDB(f : LRFunction.T; 
              x1, x2 : LONGREAL; tol : LONGREAL := 0.0001d0) : LONGREAL;

END Solve.
