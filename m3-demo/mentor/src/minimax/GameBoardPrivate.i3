(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

INTERFACE GameBoardPrivate;

IMPORT GameBoard;
(* This is an interface which gives access to an unique integer key for
   each board.  This is not useful for any Modula-3 code, but is useful
   when using other languages *)

REVEAL GameBoard.T <: GameBoardPrivate;

TYPE
  GameBoardPrivate =
    GameBoard.Public OBJECT METHODS getKey (): INTEGER; END;

END GameBoardPrivate.
