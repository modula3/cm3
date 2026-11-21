(* $Id: Solve.i3,v 1.2 2002/04/04 10:10:19 mika Exp $ *)
INTERFACE Solve;
IMPORT LRFunction;

(* Solve an equation using van Wijngaarden-Dekker-Brent method *)
(* WDB is threadsafe *)

EXCEPTION Error(TEXT);

PROCEDURE WDB(f : LRFunction.T; 
              x1, x2 : LONGREAL; tol : LONGREAL := 0.0001d0) : LONGREAL
  RAISES { Error };

END Solve.
