GENERIC INTERFACE FindZero(R);
(* Arithmetic for Modula-3, see doc for details

   Abstract: Root finders *)

FROM Arithmetic IMPORT Error;
(*==========================*)

TYPE
  Ftn = PROCEDURE (x: R.T): R.T;

  DerivativeArray = ARRAY OF R.T;
  DerivativeArray2 = ARRAY [0 .. 1] OF R.T;
  DifFtn = PROCEDURE (x: R.T): DerivativeArray2;

  Bracket = RECORD l, r: R.T END;


PROCEDURE AreBracketing (y1, y2: R.T; ): BOOLEAN;
(* Test if two function values have different signs.  If they have and the
   function is continuous then between the function points is a zero. *)


PROCEDURE BracketOut (func: Ftn;  (*find brackets for this function*)
                      VAR (*INOUT*) x: Bracket;  (*starting with these
                                                    points*)
                      maxiter: CARDINAL := 55 (*growing maxiter times*)
  ): BOOLEAN RAISES {Error};     (*true if successful*)
(* Given x.l,x.r, search for points (returned in x.l, x.r) for which
   func(x.l) is opposite sign from func(x.r).  Grow outward from the
   original x.l,x.r by golden ratio, for geometric growth.  Return true if
   a good x.l,x.r can be found before getting to maxiter, else return
   false.

   requires: x.l<x.r. *)

PROCEDURE BracketIn (func: Ftn;  (*find brackets for this function*)
                     READONLY x: Bracket;  (*starting with these points*)
                     n: [1 .. LAST(CARDINAL)];  (*using n equi-sized
                                                   segments*)
  ): REF ARRAY OF Bracket;       (*returning pairs here*)
(* Break up the x.l..x.r range into n equi-sized segments.  Select all
   pairs which allow bracketing.

   requires: x.l<x.r. *)


PROCEDURE Bisection (func: Ftn;  (*find root of this function*)
                     READONLY x  : Bracket;  (*between these brackets*)
                              tol: R.T;      (*to within +/- tolerance*)
                     maxiter := 45 (*but no more than maxiter cuts*)
  ): R.T RAISES {Error};         (*returning the root*)
(* Given brackets x.l,x.r, find a root via bisection, and refine it to
   within +/- tol *)

PROCEDURE Brent (func: Ftn;      (*find a root of this function*)
                 READONLY x  : Bracket;  (*between these bracket points*)
                          tol: R.T;      (*to this tolerance*)
                 maxiter := 100  (*with <= maxiter iterations*)
  ): R.T RAISES {Error};
(* Use Brent's algorithm to find the real root between the bracket points.
   x.l and x.r must be of opposite signs. *)

PROCEDURE NewtonRaphson (func: DifFtn;  (*this ftn*)
                         READONLY x: Bracket;  (*bracketed by these
                                                  points*)
                         xtol: R.T;  (*find root to this precision of x*)
                         maxiter := 25 (*with no more than maxiter loops*)
  ): R.T RAISES {Error};         (*returning root*)
(* Given a function which returns both f(x) and df(x), and brackets x.l and
   x.r, find the root to xtol precision.  Works via newton-raphson and
   bisection. *)

END FindZero.
