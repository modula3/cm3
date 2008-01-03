(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Stephen Harrison and Greg Nelson *)
(* *)
(* Last modified on Tue Jul 21 20:28:24 PDT 1992 by harrison *)

INTERFACE CirclePixmap;

IMPORT Pixmap;

PROCEDURE New (width, height: CARDINAL; border: CARDINAL := 0; fill := TRUE):
  Pixmap.T;
(* Return an elliptical bitmap of the given "width" and "height".  It is a
   checked runtime error if either of these are negative. *)

END CirclePixmap.

