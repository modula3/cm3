(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Nov  9 18:04:14 PST 1992 by msm     *)
<*PRAGMA LL*>

INTERFACE JoinCursor;

IMPORT ScrnCursor, JoinScreen, Palette, Cursor;

TYPE
  Oracle <: ScrnCursor.Oracle;

PROCEDURE New(st: JoinScreen.T): Oracle;

PROCEDURE Apply (st: JoinScreen.T; cl: Palette.CursorClosure; cs: Cursor.T):
  ScrnCursor.T;

END JoinCursor.
