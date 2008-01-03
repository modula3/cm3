(* Copyright (C) 1991, Digital Equipment Corporation                    *)
(* All rights reserved.                                                 *)
(* See the file COPYRIGHT for a full description.                       *)

(* Last modified on Sun Mar  1 17:44:46 1992 by msm     *)

INTERFACE CardSuit;

IMPORT Pixmap;

FROM Card IMPORT Suit;

PROCEDURE AcePix(r: Suit): Pixmap.T;
PROCEDURE RankPix(r: Suit): Pixmap.T;
PROCEDURE PipPix(r: Suit): Pixmap.T;
PROCEDURE RankXip(r: Suit): Pixmap.T;
PROCEDURE PipXip(r: Suit): Pixmap.T;
(* Xip returns the upside-down pip as a pixmap *)

VAR PixWidth, PixHeight: ARRAY Suit OF CARDINAL;

END CardSuit.
