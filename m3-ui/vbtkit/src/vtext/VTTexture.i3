(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last Modified On Tue Jun 16 13:12:37 PDT 1992 by muller *)
(*      Modified On Fri Dec 21 09:59:16 1990 by jdd *)

(* This module holds the textures for VTs. *)

INTERFACE VTTexture;

IMPORT Pixmap;

VAR
  gray, lightGray: Pixmap.T;
  turn: ARRAY BOOLEAN OF Pixmap.T;


PROCEDURE Init () RAISES {};

END VTTexture.
