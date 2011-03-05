(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Nov 18 13:44:09 PST 1994 by detlefs                  *)

INTERFACE Rat;

(* A Rat.T is a rational number. *)

IMPORT Text;

TYPE 
  T = RECORD num, den: INTEGER END;

(* The value of the rational "q" is "q.num/q.den". 
   Any rational "q" satisfies three conditions: 

| q.den > 0 
| GCD(q.num, q.den) = 1
| q.num = 0 => q.den = 1

*)

CONST 
  Zero = T{0, 1};
  One = T{1, 1};
  NegOne = T{-1, 1};

PROCEDURE Plus(READONLY q0, q1: T): T;
(* Return "q0 + q1". *)

PROCEDURE Minus(READONLY q0, q1: T): T;
(* Return "q0 - q1". *)

PROCEDURE Times(READONLY q0, q1: T): T;
(* Return "q0 * q1". *)

PROCEDURE Div(READONLY q0, q1: T): T;
(* Return "q0 / q1". A checked runtime error if
   "q1.num = 0". *)

PROCEDURE Recip(READONLY q: T): T;
(* Return "1 / q". A checked runtime error if "q.num = 0". *)

PROCEDURE Abs(READONLY q: T): T;
(* Return the absolute value of "q". *)

PROCEDURE GT(READONLY q0, q1: T): BOOLEAN;
(* Return "q0 > q1". *)

PROCEDURE GE(READONLY q0, q1: T): BOOLEAN;
(* Return "q0 >= q1". *)

PROCEDURE LT(READONLY q0, q1: T): BOOLEAN;
(* Return "q0 > q1". *)

PROCEDURE LE(READONLY q0, q1: T): BOOLEAN;
(* Return "q0 >= q1". *)

PROCEDURE Floor(READONLY q: T): T;
(* Return the largest integer that is not larger than "q". *)

PROCEDURE ToText(READONLY q: T): Text.T;
(* Returns a text represention of "q". *)

<*PRAGMA SPEC *>

<*SPEC Plus(q0, q1) *>
<*SPEC Minus(q0, q1) *>
<*SPEC Times(q0, q1) *>
<*SPEC Div(q0, q1) *>
<*SPEC Recip(q) *>
<*SPEC Abs(q) *>
<*SPEC GT(q0, q1) *>
<*SPEC GE(q0, q1) *>
<*SPEC LT(q0, q1) *>
<*SPEC LE(q0, q1) *>
<*SPEC Floor(q) *>

END Rat.
