(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Mar 13 14:59:03 PST 1992 by muller                   *)
(*      modified on Fri Dec 22 15:52:13 1989 by kalsow                       *)

MODULE Columns;

FROM Config IMPORT PieceList, FreqList, Game, New3;

PROCEDURE Build () : Game =
  VAR p := NEW (PieceList, 1);  f := NEW (FreqList, 1);
  BEGIN
    p[0] := New3 (0,-1,  0,0,  0,1);     f[0] := 1.0;
    RETURN NEW (Game,
                  name     := "Columns",
                  speed    := 400,
                  nRows    := 15,
                  nCols    := 6,
                  nMatches := 3,
                  nColors  := 6,
                  nPieces  := 1,
                  nTiles   := 3,
                  pieces   := p,
                  freq     := f);
  END Build;

BEGIN
END Columns.
