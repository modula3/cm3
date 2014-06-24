(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* A "Text16Short.T" is a text containing at most "MaxLength"
   characters of type WIDECHAR. *)

(* NOTE on naming.  This source file was named when WIDECHAR was always 16-bits.
   Today, it can be that, or 32 bits, with upper bound of 16_10FFFF, for full
   Unicode range.  It's tedious to change many source file names in CVS, so
   the "16" remains. *)  

INTERFACE Text16Short;

CONST MaxLength = 15;

TYPE
  T <: Public;
  Public = TEXT OBJECT
    len     : CARDINAL;
    contents: ARRAY [0..MaxLength] OF WIDECHAR;
  END;
  (* The array contains the characters of the text followed immediately 
     by one element containing VAL(0,WIDECHAR). *)

PROCEDURE New (READONLY a: ARRAY OF WIDECHAR): T;
(* Return a new text containing the characters of "a".  It is a checked
   runtime error if "NUMBER (a) > MaxLength". *)

END Text16Short.

