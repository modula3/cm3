(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

INTERFACE ItemFont;

IMPORT Font;

TYPE T <: REFANY;

PROCEDURE FromName (name: TEXT): T;
(* Converts a long X name to an item font. *)

<*INLINE*> PROCEDURE Size (if: T): REAL;
(* Returns the point size. *)

PROCEDURE Scale (if: T; scale: REAL): T;
(* returns font with real size set to "if.size/scale" *)

PROCEDURE ToFont (if: T; scale: REAL): Font.T 
    RAISES {Invisible, TooSmall, TooBig};
(* Converts an item font to its long X name. *)

EXCEPTION Invisible; TooSmall; TooBig;

PROCEDURE SplitName (full: TEXT; 
                     VAR preSize, size, postSize: TEXT);
(* Splits the "full" X font name into 3 parts: "preSize", "size"
   giving the deci-point size, and "postSize". *)

END ItemFont.
