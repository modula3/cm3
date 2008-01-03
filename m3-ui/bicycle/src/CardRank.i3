(* Copyright (C) 1991, Digital Equipment Corporation                    *)
(* All rights reserved.                                                 *)
(* See the file COPYRIGHT for a full description.                       *)

(* Last modified on Mon Mar  2 15:56:15 1992 by msm     *)

INTERFACE CardRank;

IMPORT Pixmap;

FROM Card IMPORT Rank;

PROCEDURE Pix(r: Rank): Pixmap.T;
PROCEDURE Xip(r: Rank): Pixmap.T;
(* Xip returns the upside-down rank as a pixmap *)

END CardRank.
