(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:57:54 PST 1992 by muller   *)
(*      modified on Thu Dec 12  1:43:15 PST 1991 by gnelson  *)
(*      modified on Fri Aug  3 16:59:33 PDT 1990 by steveg   *)
<*PRAGMA LL*>

(* A "ScreenType.T" represents a class of screens that have a common
   pixel depth, a common set of operations on the pixels, and common
   repositories for cursors, pixmaps, and fonts.  
   
   When the screentype of a "VBT" changes, any screen-dependent
   resources for the old screentype become useless.  The application
   must use the new screentype's {\it oracles} to look up resources
   that are valid for the new screentype.  This is all handled
   automatically if you use screen-independent resources that are
   predefined or defined by somebody else.  But you will need to use
   this interface if you are implementing your own screen-independent
   resources.  *)

INTERFACE ScreenType;

IMPORT ScrnCursor, VBT, ScrnColorMap, ScrnFont, 
  ScrnPaintOp, ScrnPixmap;

TYPE T = VBT.ScreenType;

REVEAL VBT.ScreenType <: Public;

TYPE
  Public = VBT.ScreenTypePublic OBJECT (*CONST*)
    bg, fg: ScrnPaintOp.Pixel;
    bits: T;
    op: ScrnPaintOp.Oracle;
    cursor: ScrnCursor.Oracle;
    pixmap: ScrnPixmap.Oracle;
    font: ScrnFont.Oracle;
    cmap: ScrnColorMap.Oracle;
  END;

(* For a screentype "st", the values "st.bg" and "st.fg" are the pixel
   values that represent the user's default background and foreground
   colors on "st". If the screen is color-mapped, these are appropriate
   for the default colormap.  For applications doing simple painting,
   "bg" is logical white and "fg" is logical black.  Depending on the
   screen and user preferences, the actual colors that the user sees
   might be different.

   The screentype "st.bits" is the screentype for 1-bit deep pixmap
   sources for painting on screens of type "st". It is guaranteed that
   "st.bits.bits=st.bits", "st.bits.fg=1", and "st.bits.bg=0".

   The oracles "st.op", "st.font", "st.cursor", and "st.pixmap"
   contains methods that provide screen-dependent resources appropriate
   for "st"---for example, "st.font" has a method that will look up 
   fonts by name.

   If "st.cmap # NIL", "st" is a color-mapped screen, which means that
   the color of a pixel is determined by looking up its value in a
   table.  The color map can be either readonly or writable. *)

END ScreenType.
