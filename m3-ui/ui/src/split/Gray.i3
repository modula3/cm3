(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Sep 25 15:37:25 PDT 1992 by msm  *)
<*PRAGMA LL*>

INTERFACE Gray;
IMPORT Pixmap;

PROCEDURE New3x3(intensity:[0..9]):Pixmap.T;
  (* return a 3x3 1-bit "Pixmap.T" with "intensity" pixels lit *)

PROCEDURE New4x4(intensity:[0..16]):Pixmap.T;
  (* return a 4x4 1-bit "Pixmap.T" with "intensity" pixels lit *)

END Gray.
