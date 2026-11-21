(* $Id: Bracket.i3,v 1.4 2004/10/22 05:28:24 mika Exp $ *)

INTERFACE Bracket;
IMPORT Fmt;
IMPORT LongReal AS LR;
IMPORT LRFunction AS Function;

TYPE Trio = RECORD a, b, c : LONGREAL END;

(* 
   Starting with Trio { a, b, x }, Initial searches until a bracket is
   found s.t. { a, b, x } bracket the minimum.  Initial returns the
   values of the function at { a, b, x } 

   we should have Trio.a < Trio.b < Trio.c

   with minimum known to be in [ Trio.a , Trio.c ]

   See method dbrent, Numerical Recipes in Fortran Section 10.2
*)

PROCEDURE Initial(VAR bracket : Trio; func : Function.T) : Trio;


PROCEDURE Brent(bracket  : Trio;
                f        : Function.T;
                tol      : LONGREAL; 
                VAR xmin : LONGREAL) : LONGREAL;

PROCEDURE Format(bracket : Trio;
                 style              := Fmt.Style.Auto;
                 prec    : CARDINAL := LR.MaxSignifDigits - 1;
                 literal            := FALSE) : TEXT;

(* the following are wrappers for Scheme programming, easier than
   dealing with VAR parameters *)

TYPE XYTrio = RECORD x, y : Trio END;

PROCEDURE SchemeInitial(bracket : Trio; func : Function.T) : XYTrio;

TYPE Pair = RECORD x, y : LONGREAL END;

PROCEDURE SchemeBrent(bracket : Trio; f : Function.T; tol : LONGREAL) : Pair;


END Bracket.

