(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

INTERFACE TypeFmt;

IMPORT GameBoard;

(* This module translates M3 types that occur as arguments to events into a
   form that obliq can use *)

PROCEDURE Board (board: GameBoard.T): TEXT;

(* This procedure just translates a board into its key *)
PROCEDURE BoardKey (board: GameBoard.T): TEXT;

END TypeFmt.
