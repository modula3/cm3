(* Copyright (C) 1995, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Jun 13 16:19:43 PDT 1995 by kalsow     *)

INTERFACE Config;

IMPORT Point;

TYPE
  T = OBJECT
    name       : TEXT;
    nRows      : CARDINAL;
    nCols      : CARDINAL;
    nPieces    : CARDINAL;
    nTiles     : CARDINAL;
    delay      : LONGREAL;
    pieces     : PieceList;
  END;

CONST
  NRotations = 4;

TYPE
  PieceList = REF ARRAY (* 0..nPieces-1 *) OF Piece;
  Piece     = ARRAY [0..NRotations-1] OF RotatedPieceMap;
  TileList  = REF ARRAY (* 0..nTiles-1 *) OF Point.T;

  RotatedPieceMap = RECORD
    tiles   : TileList;
    hoffset : INTEGER;
    voffset : INTEGER;
  END;

PROCEDURE New (nSquares: [2..5]): T;

END Config.
