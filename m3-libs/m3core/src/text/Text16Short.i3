(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

(* A "Text16Short.T" is a text containing at most "MaxLength"
   16-bit or "WIDECHAR" characters. *)

INTERFACE Text16Short;

CONST MaxLength = 15;

TYPE
  T <: Public;
  Public = TEXT OBJECT
    len     : CARDINAL;
    contents: ARRAY [0..MaxLength] OF WIDECHAR;
  END;
  (* The array contains the characters of the text followed by a '\000'. *)

PROCEDURE New (READONLY a: ARRAY OF WIDECHAR): T;
(* Return a new text containing the characters of "a".  It is a checked
   runtime error if "NUMBER (a) > MaxLength". *)

END Text16Short.

