(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun 20 17:01:08 PDT 1996 by heydon                   *)
(*      modified on Mon Feb 24 14:01:19 PST 1992 by muller                   *)
<*PRAGMA LL*>

(* A "Font.T" is a screen-independent specification of a typeface.  There
   is one predefined "Font.T", which yields the built-in font of the
   screentype.  

   The locking level is "LL.sup <= VBT.mu" for all of the procedures
   in this interface. *)

INTERFACE Font;

CONST Brand = "Font 1.0";

TYPE T = RECORD fnt: INTEGER END; Predefined = [0..0];

CONST BuiltIn = T{0};  

PROCEDURE FromName(READONLY names: ARRAY OF TEXT): T;
(* Return the first available font of those named in the array "names". *)

(* The entries of "names" are font names as specified in the "ScrnFont"
   interface, possibly containing wild card characters.  On any
   particular screentype, "FromName(names)" iterates through "names"
   in order and returns an arbitrary match from the first name that
   matches anything.  If no name has any matches, it returns the
   built-in font.
   
   Standard X screentypes give fonts long ``names'' that encode their
   properties, so with X it is almost always desirable to include
   wild-card characters in the names.  For example,

| FromName(
|  ARRAY OF TEXT{"-*-times-medium-r-*-*-*-10?-*-*-*-*-*-*"})

  will return a font that, on an X server containing the standard fonts, 
  is some Times Roman medium-weight unslanted font sized 10 to 10.9 points, 
  and behaves like "Font.BuiltIn" on any screentye that doesn't have a font 
  whose name matches the pattern.  *)

END Font.
