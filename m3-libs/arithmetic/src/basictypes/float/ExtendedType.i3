(* copied from LongrealType.i3 *)

(* An "ExtendedType.T" is a "EXTENDED".  This interface is intended to be
   used to instantiate generic interfaces and modules such as "Table" and
   "List". *)

INTERFACE ExtendedType;

IMPORT Word;

TYPE T = EXTENDED;

CONST Brand = "Extended";

PROCEDURE Equal (a, b: T; ): BOOLEAN;
(* Return "a = b".  The result is undefined if either "a" or "b" is an
   "NaN" (not a number) value. *)

<* UNUSED *>
PROCEDURE Hash (a: T; ): Word.T;
(* Return a hash value derived from "a".  The result is undefined if either
   "a" or "b" is an "NaN" (not a number) value. *)

PROCEDURE Compare (a, b: T; ): [-1 .. 1];
(* Return "-1" if "a < b", "0" if "a = b", or "+1" if "a > b".  The result
   is undefined if either "a" or "b" is an "NaN" (not a number) value. *)

END ExtendedType.
