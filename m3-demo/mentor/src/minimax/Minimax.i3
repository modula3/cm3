(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

INTERFACE Minimax;

IMPORT GameBoard, GamePlay;

CONST Infinity = 10;

TYPE
  Player = GamePlay.Player;
  Board = GameBoard.T;
  BoardValue = [-Infinity .. Infinity];

  Minimax <: MinimaxPlayerPublic;

  MinimaxPlayerPublic =
    Player BRANDED OBJECT

    METHODS
      (*Callbacks:*)
      (*CallHeuristic is a callback, called whenever we've called the
         heuristic *)
      CallHeuristic (board: Board; val: BoardValue) RAISES ANY;
      (* EvaluateNode is called whenever we start evaluating a board *)
      EvaluateNode (board: Board) RAISES ANY;
      (* UpdatedNodeValue is called whenever we've changed the value of the
         board *)
      UpdatedNodeValue (board: Board; newValue: BoardValue) RAISES ANY;
      (* FinishedEvalNode is called whenever we've finished evaluating the
         value of a board *)
      FinishedEvalNode (board:Board) RAISES ANY;

      (* SetHeuristic sets the heuristic that we're going to use *)
      SetHeuristic (heuristic: PROCEDURE (player: Minimax; board: Board): BoardValue);
      (* SetDepth sets the recursion depth that we'll use *)
      SetDepth (depth: CARDINAL);
    END;

END Minimax.
