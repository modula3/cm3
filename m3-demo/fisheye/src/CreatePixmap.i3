(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 16:46:32 PDT 1992 by muller                   *)

INTERFACE CreatePixmap;

IMPORT Pixmap, Rect;

PROCEDURE Circle(radius: REAL; borderThickness: REAL := -1.0): Pixmap.T;

PROCEDURE CircleBox(r: REAL): Rect.T;
(* returns the boundingbox *)

PROCEDURE Rectangle(a, b: REAL; borderThickness: REAL := -1.0): Pixmap.T;

PROCEDURE RectangleBox(a, b: REAL): Rect.T;
(* returns the boundingbox *)

END CreatePixmap.
