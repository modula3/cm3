(* Copyright 1995-96 Critical Mass, Inc. All rights reserved. *)

(* This interface defines misc. TEXT manipulation routines. *)

INTERFACE Text2;

PROCEDURE CIEqual (a, b: TEXT): BOOLEAN;
(* Returns "TRUE" if "a" and "b" are equal when case is ignored. *)

PROCEDURE Trim (a: TEXT): TEXT;
(* Returns "a" with any leading or trailing whitespace removed. *)

PROCEDURE EscapeString (a: TEXT): TEXT;
(* Returns "a" with any backslashes or quotes escaped, and the
   result wrapped in quotes. *)

END Text2.
