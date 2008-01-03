(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Tue Jun 13 13:47:53 PDT 1995 by detlefs    *)

(* An "RealType.T" is a "REAL".  This interface is intended to be
   used to instantiate generic interfaces and modules such as "Table"
   and "List". *)

INTERFACE RealType;

IMPORT Word;

TYPE T = REAL;

CONST Brand = "Real";

PROCEDURE Equal(a, b: T): BOOLEAN;
(* Return "a = b".  The result is undefined if either "a" or "b" is
   an "NaN" (not a number) value. *)

PROCEDURE Hash(a: T): Word.T;
(* Return a hash value derived from "a".  The result is undefined if
   either "a" or "b" is an "NaN" (not a number) value. *)

PROCEDURE Compare(a, b: T): [-1..1];
(* Return "-1" if "a < b", "0" if "a = b", or "+1" if "a > b".
   The result is undefined if either "a" or "b" is an "NaN" (not a
   number) value. *)

END RealType.
