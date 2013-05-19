(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* A "Text8.T" is a text containing only 8-bit or "CHAR" characters. *)

INTERFACE Text8;

TYPE
  T <: Public;
  Public = TEXT OBJECT
    contents: REF ARRAY OF CHAR;
  END;
  (* The array contains the characters of the text followed by a '\000'. *)

PROCEDURE New (READONLY a: ARRAY OF CHAR): TEXT;
(* Return a new text containing the characters of "a". *)

PROCEDURE Create (n: CARDINAL): T;
(* Create a new text capable of holding "n" characters. Note that the
   length of the "contents" array is "n+1" because of the null termination.
   The characters at positions "[0..n-1]" are left undefined. The character
   at position "n" is initialized to '\000' *)

END Text8.

