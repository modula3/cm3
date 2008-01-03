(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Jun  5 22:02:01 PDT 1994 by msm     *)
<*PRAGMA LL*>

INTERFACE JoinPixmap;

IMPORT ScrnPixmap, JoinScreen, Palette, Pixmap, Rect, ScreenType;

TYPE
  Oracle <: ScrnPixmap.Oracle;
  T <: ScrnPixmap.T;

PROCEDURE New(st: JoinScreen.T): Oracle;

PROCEDURE Apply (st: JoinScreen.T; cl: Palette.PixmapClosure; pm: Pixmap.T):
  ScrnPixmap.T;

PROCEDURE Create(st: JoinScreen.T; READONLY clip: Rect.T): T;
(* Create a pixmap which resolves to a to-be-specified list of "ScrnPixmap"
   on other screen types; the "free" method of such a pixmap frees all the
   attached pixmaps. *)

PROCEDURE AddPixmap(p: T; st: ScreenType.T; pm: ScrnPixmap.T);
(* Add (st, pm) to the list of resolvable pixmaps for p *)

PROCEDURE Resolve (jst: JoinScreen.T; pst: ScreenType.T; n: INTEGER):
  ScrnPixmap.T;
(* If "pm" was the result of a call to "Create" on "jst", and "n" is the id
   of "pm", and "(pst, res)" was added to "pm" using "AddPixmap", return
   "res".  Otherwise, return "NIL". *)

END JoinPixmap.
