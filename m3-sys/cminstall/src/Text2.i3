(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* This interface defines misc. TEXT manipulation routines. *)

INTERFACE Text2;

IMPORT ASCII;

PROCEDURE CIEqual (a, b: TEXT): BOOLEAN;
(* Returns "TRUE" if "a" and "b" are equal when case is ignored. *)

PROCEDURE Trim (a: TEXT): TEXT;
(* Returns "a" with any leading or trailing whitespace removed. *)

PROCEDURE EscapeString (a: TEXT): TEXT;
(* Returns "a" with any backslashes or quotes escaped, and the
   result wrapped in quotes. *)

PROCEDURE FindChars(t: TEXT; s: ASCII.Set := ASCII.Spaces) : BOOLEAN;
(* TRUE <=> t contains at least one element of s *)

PROCEDURE RemoveChars(t: TEXT; s: ASCII.Set := ASCII.Spaces) : TEXT;
(* Returns "t" without any whitespace (or other characters from s) *)


END Text2.
