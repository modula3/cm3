(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Fri Aug 16 15:06:21 PDT 1996 by heydon     *)
(*      modified on Fri Jul  9 16:47:34 PDT 1993 by mhb        *)
(*      modified on Wed May  5 08:57:07 PDT 1993 by mcjones    *)
(*      modified on Thu Apr 29 16:10:55 PDT 1993 by muller     *)
(*      modified on Fri Feb 28 20:50:12 PST 1992 by stolfi     *)
(*      modified on Wed Sep 25 00:02:34 1991 by kalsow         *)

INTERFACE Real;

(* Properties of REAL (for ANSI/IEEE Standard 754-1985).

   This package defines some basic properties of the 
   built-in float type REAL. If you want to instantiate
   a generic type with real numbers, you should import the
   "RealType" interface instead.

   Index: REAL; floating-point; generics
*)
  
TYPE T = REAL;

CONST 
  Base: INTEGER = 2; 
  (* The radix of the floating-point representation for T *)

  Precision: INTEGER = 24;
  (* The number of digits of precision in the given Base for T. *)

  MaxFinite: T = 3.40282347E+38;
  (* The maximum finite value in T.  For non-IEEE implementations,
     this is the same as LAST(T). *)

  MinPos: T = 1.40239846E-45;
  (* The minimum positive value in T. *)

  MinPosNormal: T = 1.17549435E-38;
  (* The minimum positive "normal" value in T; differs from MinPos
     only for implementations with denormalized numbers. *)

CONST
  MaxExpDigits = 2;
  MaxSignifDigits = 9;
(* "MaxExpDigits" is the smallest integer with the property that every
   finite number of type "T" can be written in base-10 scientific
   notation using an exponent with at most "MaxExpDigits".
   "MaxSignifDigits" is the smallest integer with the property that
   floating-decimal numbers with "MaxSignifDigits" are more closely
   spaced, all along the number line, than are numbers of type "T".
   Typically, *)
(*
| MaxExpDigits    = ceiling(log_10(log_10(MaxFinite)))
| MaxSignifDigits = ceiling(log_10(Base^Precision)) + 1.

*)

END Real.
