(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Mar 13 14:58:54 PST 1992 by muller                   *)
(*      modified on Tue Mar 13 11:54:39 1990 by kalsow                       *)

MODULE Bars;

FROM Config IMPORT PieceList, FreqList, Game, New3;

PROCEDURE Build () : Game =
  VAR p := NEW (PieceList, 2);  f := NEW (FreqList, 2);
  BEGIN
    p[0] := New3 (0,-1,  0,0,  0,1);     f[0] := 0.5;
    p[1] := New3 (-1,0,  0,0,  1,0);     f[1] := 0.5;
    RETURN NEW (Game,
                  name     := "Bars",
                  speed    := 400,
                  nRows    := 15,
                  nCols    := 6,
                  nMatches := 3,
                  nColors  := 6,
                  nPieces  := 2,
                  nTiles   := 3,
                  pieces   := p,
                  freq     := f);
  END Build;

BEGIN
END Bars.
