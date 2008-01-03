(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* A "Text8Cstring.T" is a text that references a null-terminated
   C string containing only 8-bit or "CHAR" characters. *)

UNSAFE INTERFACE Text8CString;

IMPORT Ctypes;

TYPE
  T <: Public;
  Public = TEXT OBJECT
    str: Ctypes.char_star;
  END;
  (* The array contains the characters of the text followed by a '\000'. *)

PROCEDURE New (s: Ctypes.char_star): TEXT;
(* Return a new text referring to the characters of "s".  It is
   an unchecked runtime error to reference the returned text
   after "s" has been freed. *)

END Text8CString.

