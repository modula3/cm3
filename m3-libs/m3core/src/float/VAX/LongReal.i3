(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Sep 24 08:26:51 PDT 1993 by kalsow     *)
(*      modified on Thu Apr 29 16:10:31 PDT 1993 by muller     *)
(*      modified on Fri Feb 28 20:17:40 PST 1992 by stolfi     *)

INTERFACE LongReal; 

(* Basic properties OF LONGREAL (for the VAX).

   This package defines some basic properties of the 
   built-in float type LONGREAL, for the VAX architecture.

   Index: LONGREAL; floating-point; generics
*)

TYPE T = LONGREAL; 

CONST 
  Base: INTEGER = 2; 
  (* The radix of the floating-point representation for T *)

  Precision: INTEGER = 56;
  (* The number of digits of precision in the given Base for T. *)

  MaxFinite: T = 1.70141183460469230D+38;
  (* The maximum finite value in T.  For non-IEEE implementations,
     this is the same as LAST(T). *)

  MinPos: T = 2.93873587705571880D-39;
  (* The minimum positive value in T. *)

  MinPosNormal: T = MinPos;
  (* The minimum positive "normal" value in T; differs from MinPos
     only for implementations with denormalized numbers. *)

CONST
  MaxExpDigits = 2;
  MaxSignifDigits = 18;

(* "MaxExpDigits" is the smallest integer with the property that every
   finite number of type "T" can be written in base-10 scientific notation
   using an exponent with at most "MaxExpDigits".  "MaxSignifDigits"
   is the smallest integer with the property that floating-decimal
   numbers with "MaxSignifDigits" are more closely spaced, all along
   the number line, than are numbers of type "T".  Typically,

| MaxExpDigits    = ceiling(log_10(log_10(MaxFinite)))
| MaxSignifDigits = ceiling(log_10(Base^Precision)) + 1.

*)
END LongReal.
