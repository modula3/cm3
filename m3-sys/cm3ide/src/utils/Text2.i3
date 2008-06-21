(* Copyright 1995-96 Critical Mass, Inc. All rights reserved. *)

(* This interface defines misc. TEXT manipulation routines. *)

INTERFACE Text2;

PROCEDURE CIEqual (a, b: TEXT): BOOLEAN;
(* Returns "TRUE" if "a" and "b" are equal when case is ignored. *)

PROCEDURE EqualSub (a: TEXT;  READONLY b: ARRAY OF CHAR): BOOLEAN;
(* Returns "TRUE" if "a" equals "b". *)

PROCEDURE PrefixMatch (a, b: TEXT;  len: CARDINAL): BOOLEAN;
(* Returns "Text.Equal (Text.Sub (a, 0, Text.Length(b)), b)". *)

PROCEDURE FindSubstring (a, b: TEXT): INTEGER;
(* Returns the index "x" of "a" such that "Text.Sub(a, x, Text.Length(b))"
   equals "b".  If "b" is not contained in "a", "-1" is returned. *)

PROCEDURE FindBufSubstring (READONLY a: ARRAY OF CHAR;  b: TEXT): INTEGER;
(* Returns the index "x" of "a" such that "SUBARRAY(a, x, Text.Length(b))"
   equals "b".  If "b" is not contained in "a", "-1" is returned. *)

PROCEDURE Trim (a: TEXT): TEXT;
(* Returns "a" with any leading or trailing whitespace removed. *)

PROCEDURE Escape (a: TEXT): TEXT;
(* Returns "a" with any backslashes or quotes escaped. *)

PROCEDURE EscapeHTML (a: TEXT): TEXT;
(* Returns "a" with any special HTML characters escaped. *)

PROCEDURE ConvertNBSP (a: TEXT): TEXT;
(* Returns "a" with any blanks turned into HTML non-breaking spaces *)

PROCEDURE FixExeName (a: TEXT): TEXT;
(* If "a" contains blanks, return it wrapped in quotes.  Otherwise,
   return "a" unchanged. *)

END Text2.
