(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Mar 13 14:59:46 PST 1992 by muller                   *)
(*      modified on Fri Dec 22 15:52:12 1989 by kalsow                       *)

MODULE Squares;

FROM Config IMPORT PieceList, FreqList, Game, New4;

PROCEDURE Build () : Game =
  VAR p := NEW (PieceList, 1);  f := NEW (FreqList, 1);
  BEGIN
    p[0] := New4 (0,0, 0,1, 1,1, 1,0);     f[0] := 1.0;
    RETURN NEW (Game,
                  name     := "Squares",
                  speed    := 500,
                  nRows    := 16,
                  nCols    := 12,
                  nMatches := 3,
                  nColors  := 8,
                  nPieces  := 1,
                  nTiles   := 4,
                  pieces   := p,
                  freq     := f);
  END Build;

BEGIN
END Squares.
