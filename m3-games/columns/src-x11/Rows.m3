(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Mar 13 14:59:27 PST 1992 by muller                   *)
(*      modified on Fri Dec 22 15:52:13 1989 by kalsow                       *)

MODULE Rows;

FROM Config IMPORT PieceList, FreqList, Game, New3;

PROCEDURE Build () : Game =
  VAR p := NEW (PieceList, 1);  f := NEW (FreqList, 1);
  BEGIN
    p[0] := New3 (-1,0,  0,0,  1,0);     f[0] := 1.0;
    RETURN NEW (Game,
                  name     := "Rows",
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
END Rows.
