(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

(* A "Text16.T" is a text containing only 16-bit or "WIDECHAR" characters. *)

INTERFACE Text16;

TYPE
  T <: Public;
  Public = TEXT OBJECT
    contents: REF ARRAY OF WIDECHAR;
  END;
  (* The array contains the characters of the text followed by a '\x0000'. *)

PROCEDURE New (READONLY a: ARRAY OF WIDECHAR): TEXT;
(* Return a new text containing the characters of "a". *)

PROCEDURE Create (n: CARDINAL): T;
(* Create a new text capable of holding "n" characters. Note that the
   length of the "contents" array is "n+1" because of the null termination.
   The characters at positions "[0..n-1]" are left undefined. The character
   at position "n" is '\x0000' *)

END Text16.

