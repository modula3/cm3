(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last Modified On Tue Jun 16 13:12:36 PDT 1992 by muller *)
(*      Modified On Fri Dec 21 10:04:35 1990 by jdd *)
(*      Modified On Tue May 15 17:06:34 PDT 1990 by mcjones *)

(* This module holds the textures for VTs. *)

MODULE VTTexture;

IMPORT Pixmap;

VAR
  initflag: BOOLEAN;


PROCEDURE Init () RAISES {} =
  BEGIN
    IF  NOT initflag THEN
      gray := Pixmap.Gray;
      lightGray := gray;
      turn[FALSE] := Pixmap.Empty;
      turn[TRUE] := gray;
      initflag := TRUE;
    END;
  END Init;

BEGIN
  initflag := FALSE;
END VTTexture.
