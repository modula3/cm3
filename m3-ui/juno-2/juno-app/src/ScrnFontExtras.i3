(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Dec 21 18:28:34 PST 1994 by heydon                   *)

INTERFACE ScrnFontExtras;

(* Extensions to the "ScrnFont" interface. *)

IMPORT ScrnFont, Rect;

PROCEDURE TightBoundingBox(txt: TEXT; fnt: ScrnFont.T): Rect.T;
(* Return the smallest rectangle enclosing the ink resulting from painting
   "txt" at the origin in the font "fnt". Requires "txt # NIL". If "fnt" is
   "NIL" or the font's metrics are "NIL", then the result is a rectangle
   whose southwest corner is the origin, and whose height is 1 and whose width
   is the length of "txt". *)

PROCEDURE TightBoundingBoxSub(READONLY txt: ARRAY OF CHAR; fnt: ScrnFont.T):
  Rect.T;
(* Like "TightBoundingBox", but the string is passed as an array of characters
   instead of as a "TEXT". *)

PROCEDURE TightBoundingBoxSubValid(
  READONLY txt: ARRAY OF CHAR;
  fnt: ScrnFont.T;
  VAR (*OUT*) valid: BOOLEAN)
  : Rect.T;
(* Like "TightBoundingBoxSub", but "valid" is set to "TRUE" iff all characters
   of "txt" are valid. If "fnt" is "NIL" or the font's metrics are "NIL", then
   "valid" will be "TRUE" independent of "txt". *)

END ScrnFontExtras.
