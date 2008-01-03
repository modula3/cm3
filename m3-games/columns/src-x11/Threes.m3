(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Mar 13 15:00:01 PST 1992 by muller                   *)
(*      modified on Fri Dec 22 15:52:12 1989 by kalsow                       *)

MODULE Threes;

FROM Config IMPORT PieceList, FreqList, Game, New3;

PROCEDURE Build () : Game =
  VAR p := NEW (PieceList, 6);  f := NEW (FreqList, 6);
  BEGIN
    p[0] := New3 (0,-1,  0,0,  0,1);  f[0] := 0.25;
    p[1] := New3 (-1,0,  0,0,  1,0);  f[1] := 0.25;
    p[2] := New3 (0,1,   0,0,  1,0);  f[2] := 0.125;
    p[3] := New3 (0,0,   1,0,  1,1);  f[3] := 0.125;
    p[4] := New3 (1,0,   1,1,  0,1);  f[4] := 0.125;
    p[5] := New3 (1,1,   0,1,  0,0);  f[5] := 0.125;
    RETURN NEW (Game,
                  name     := "Threes",
                  speed    := 500,
                  nRows    := 15,
                  nCols    := 6,
                  nMatches := 3,
                  nColors  := 6,
                  nPieces  := 6,
                  nTiles   := 3,
                  pieces   := p,
                  freq     := f);
  END Build;

BEGIN
END Threes.
