(* Copyright (C) 1991, Digital Equipment Corporation                    *)
(* All rights reserved.                                                 *)
(* See the file COPYRIGHT for a full description.                       *)

(* Last modified on Sun Mar  1 17:44:36 1992 by msm     *)

INTERFACE FaceCards;

IMPORT Pixmap, Point;

FROM Card IMPORT Suit, Value;

TYPE
  FaceRank = [Value.Jack..Value.King];

PROCEDURE Pix(s: Suit; r: FaceRank): Pixmap.T;
PROCEDURE PixCenter(s: Suit; r: FaceRank): Point.T;
PROCEDURE XipCenter(s: Suit; r: FaceRank): Point.T;
(* Positions to center a pip in the card *)

END FaceCards.
