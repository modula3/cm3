GENERIC INTERFACE ResidueClassBasic(R);
(* Arithmetic for Modula-3, see doc for details

   Abstract: Generic residue class type

   Instantiate with integers, polynomials

   ToDo: Test cases.  Check isomorphy of the polynomial ring modulo (x^2+1)
   to the complex numbers. *)

FROM Arithmetic IMPORT Error;


CONST Brand = R.Brand & "ResidueClass";

TYPE
  T = RECORD r, d: R.T;  END;    (* Representative of a residue class,
                                    divisor (generator of the ideal) *)


(* You must not create residue classes with respect to the zero as
   divisor. *)

PROCEDURE NewZero (d: R.T; ): T; (* neutral additive element in the class
                                    'd' *)
PROCEDURE NewOne (d: R.T; ): T;  (* neutral multiplicative element in the
                                    class 'd' *)
PROCEDURE FromRepresentative (x, d: R.T; ):
  T;                             (* the residue class with respect to 'd'
                                    to which 'x' belongs *)
PROCEDURE ToRepresentative (READONLY x: T; ): R.T; (* a representative of
                                                      the residue class *)

(* the operands must belong to the same residue class *)
PROCEDURE Add (READONLY x, y: T; ): T; (* x+y *)
PROCEDURE Sub (READONLY x, y: T; ): T; (* x-y *)
PROCEDURE Neg (READONLY x: T; ): T; (* -x *)
PROCEDURE IsZero (READONLY x: T; ): BOOLEAN;
PROCEDURE Equal (READONLY x, y: T; ): BOOLEAN; (* x=y *)

PROCEDURE Mul (READONLY x, y: T; ): T; (* x*y *)
PROCEDURE Div (READONLY x, y: T; ): T
  RAISES {Error};                (* returns z with y*z=x, if the divisor is
                                    reducible then the ring of residue
                                    classes is not a field and some
                                    divisors 'y' have undefined (Error) or
                                    multiple results *)
PROCEDURE Rec (READONLY x: T; ): T RAISES {Error}; (* 1/x *)
PROCEDURE Mod (READONLY x, y: T; ): T RAISES {Error}; (* x mod y *)

PROCEDURE Square (READONLY x: T; ): T; (* x*x *)
PROCEDURE Scale (READONLY x: T; y: R.T; ): T; (* x*y *)

END ResidueClassBasic.
