(* $Id$ *)

INTERFACE Bracket;
IMPORT Fmt;
IMPORT LongReal AS LR;
IMPORT LRFunction AS Function;

TYPE Trio = RECORD a, b, c : LONGREAL END;

(* Starting with Trio { a, b, x }, Initial searches until a bracket is
   found s.t. { a, b, x } bracket the minimum.  Initial returns the
   values of the function at { a, b, x } *)
PROCEDURE Initial(VAR bracket : Trio; func : Function.T) : Trio;


PROCEDURE Brent(bracket : Trio; f : Function.T; tol : LONGREAL; 
                VAR xmin : LONGREAL) : LONGREAL;

PROCEDURE Format(bracket : Trio ; style := Fmt.Style.Auto;
                 prec: CARDINAL := LR.MaxSignifDigits - 1;
                 literal := FALSE) : TEXT;

END Bracket.

