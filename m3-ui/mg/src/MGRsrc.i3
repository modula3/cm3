(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Tue Jun  2 00:46:03 1992 by steveg   *)

INTERFACE MGRsrc;

<* PRAGMA LL *>

IMPORT Font, ScrnFont;

PROCEDURE ScaleableFont (family   : TEXT             := NIL;
                         pointSize                   := 12.0;
                         slant    : ScrnFont.Slant;
                         weightName: TEXT := ScrnFont.AnyMatch;
                         version   : TEXT := "";
                         foundry   : TEXT := ScrnFont.AnyMatch;
                         width     : TEXT := ScrnFont.AnyMatch;
                         spacing: ScrnFont.Spacing := ScrnFont.Spacing.Any;
                         charsetRegistry: TEXT := "ISO8859";
                         charsetEncoding: TEXT := "1"        ): Font.T;
(* returns a scaleable font that matches the specified font name.  It works
   by finding a font on the server that matches the desired font
   characterstics and is close to the scaled size of the font. *)

CONST
  DefaultScaleableFont = Font.T{-8231955};
  (* a distinguisheable value for a default Font.T  An Mg.T with font =
     DefaultScaleableFont will actually use defaultScaleable as its font
  *)

VAR
  defaultScaleable: Font.T;

PROCEDURE ScaleFont(font: Font.T; scale: REAL);
  (* if "font" is a scaleable font, then ScaleFont(font, scale) will 
     adjust "font" to display in a font close to the scaled size of
     "font" *)

END MGRsrc.
