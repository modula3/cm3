(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Wed May  5 08:13:31 PDT 1993 by mcjones    *)
(*      modified on Thu Apr 29 16:11:25 PDT 1993 by muller     *)
(*      modified on Fri Feb 28 20:01:18 PST 1992 by stolfi     *)
(*      modified on Wed Sep 25 00:33:01 1991 by kalsow         *)

INTERFACE Extended;

(* Properties of EXTENDED (for ANSI/IEEE Standard 754-1985).

   This package defines some basic properties of the 
   built-in float type EXTENDED.

   Index: EXTENDED; floating-point; generics
*)

TYPE T = EXTENDED;

CONST
  Base: INTEGER = 2;
  (* The radix of the floating-point representation for T *)

  Precision: INTEGER = 53;
  (* The number of digits of precision in the given Base for T. *)

  MaxFinite: T = 1.7976931348623157X+308;
  (* The maximum finite value in T.  For non-IEEE implementations,
     this is the same as LAST(T). *)

  MinPos: T = 4.9406564584124654X-324;
  (* The minimum positive value in T. *)

  MinPosNormal: T = 2.2250738585072014X-308;
  (* The minimum positive "normal" value in T; differs from MinPos
     only for implementations with denormalized numbers. *)

CONST
  MaxExpDigits = 3;
  MaxSignifDigits = 17;
(* "MaxExpDigits" is the smallest integer with the property that every
   finite number of type "T" can be written in base-10 scientific notation
   using an exponent with at most "MaxExpDigits".  "MaxSignifDigits"
   is the smallest integer with the property that floating-decimal
   numbers with "MaxSignifDigits" are more closely spaced, all along
   the number line, than are numbers of type "T".  Typically, *)
(*
| MaxExpDigits    = ceiling(log_10(log_10(MaxFinite)))
| MaxSignifDigits = ceiling(log_10(Base^Precision)) + 1.
*)
END Extended.
