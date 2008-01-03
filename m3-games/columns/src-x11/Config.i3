(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Tue Feb 11 17:40:36 PST 1992 by muller         *)
(*      modified on Fri Dec 22 15:26:58 1989 by kalsow         *)

INTERFACE Config;

(* Config defines the board configuration for columns *)

TYPE Point = RECORD x, y: INTEGER END;
TYPE Piece = REF ARRAY OF Point;

TYPE PieceList = REF ARRAY OF Piece;
TYPE FreqList  = REF ARRAY OF REAL;

TYPE Game = REF RECORD
       name     : TEXT;       (* game's name *)
       speed    : INTEGER;    (* initial speed of the game *)
       nRows    : INTEGER;    (* rows on the board *)
       nCols    : INTEGER;    (* columns on the board *)
       nMatches : INTEGER;    (* # of pieces required to "match" *)
       nColors  : INTEGER;    (* # of colors in the palette *)
       nPieces  : INTEGER;    (* # of pieces in the game *)
       nTiles   : INTEGER;    (* # of points per piece *)
       pieces   : PieceList;  (* the actual pieces *)
       freq     : FreqList;   (* initial piece probabilities *)
     END;

PROCEDURE New3 (x0,y0, x1,y1, x2,y2: INTEGER): Piece;
PROCEDURE New4 (x0,y0, x1,y1, x2,y2, x3,y3: INTEGER): Piece;

END Config.
