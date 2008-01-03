(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

MODULE Minimax;

IMPORT GameBoard, MoveList;

TYPE Move = GameBoard.Move;

REVEAL
  Minimax = MinimaxPlayerPublic BRANDED OBJECT
              heuristicUsed: PROCEDURE (player: Minimax; board: Board):
                               BoardValue;
              depth: CARDINAL;
            OVERRIDES
              SetHeuristic := DoSetHeuristic;
              SetDepth     := DoSetDepth;
              GetMove      := DoGetMove;
            END;

PROCEDURE DoSetHeuristic (self: Minimax;
                          heuristic: PROCEDURE
                                       (player: Minimax; board: Board):
                                       BoardValue) =
  BEGIN
    self.heuristicUsed := heuristic;
  END DoSetHeuristic;

PROCEDURE DoSetDepth (self: Minimax; depth: CARDINAL) =
  BEGIN
    self.depth := depth;
  END DoSetDepth;

PROCEDURE DoGetMove (self: Minimax; board: Board): Move RAISES ANY =
  VAR
    move     : Move;
    moveValue: BoardValue;
  BEGIN
    EvaluateBoard(board, self.depth, self, move, moveValue);
    RETURN move;
  END DoGetMove;

PROCEDURE EvaluateBoard (            board       : Board;
                                     recurseDepth: CARDINAL;
                                     player      : Minimax;
                         VAR (*OUT*) bestMove    : Move;
                         VAR (*OUT*) bestMoveVal : BoardValue) RAISES ANY =

  PROCEDURE DoMinOrMax (val1, val2: BoardValue): BoardValue =
    BEGIN
      IF player.position() = board.toMove() THEN
        RETURN MAX(val1, val2);
      ELSE
        RETURN MIN(val1, val2);
      END;
    END DoMinOrMax;

  VAR
    movelist       : MoveList.T         := board.legalMoves();
    childboardvalue: BoardValue;
    childBoardMove : Move;
    winner         : GameBoard.PlayerId;
  BEGIN
    player.EvaluateNode(board);
    IF board.finished(winner) THEN
      IF winner = player.position() THEN
        bestMoveVal := Infinity;
      ELSIF winner = GameBoard.PlayerId.Neither THEN
        bestMoveVal := 0;
      ELSE
        bestMoveVal := -Infinity;
      END;
      player.UpdatedNodeValue(board, bestMoveVal);
    ELSE
      IF recurseDepth = 0 THEN
        (* call Heuristic *)
        bestMoveVal := player.heuristicUsed(player, board);
        player.CallHeuristic(board, bestMoveVal);
        (*bestMove is undefined *)
      ELSE
        <*ASSERT movelist # NIL*>
        bestMoveVal := -DoMinOrMax(Infinity, -Infinity);
        bestMove := movelist.head;
        WHILE movelist # NIL DO
          EvaluateBoard(board.Move(movelist.head), recurseDepth - 1,
                        player, childBoardMove, childboardvalue);
          player.UpdatedNodeValue(
            board, DoMinOrMax(bestMoveVal, childboardvalue));
          IF bestMoveVal # DoMinOrMax(bestMoveVal, childboardvalue) THEN
            bestMoveVal := childboardvalue;
            bestMove := movelist.head;
          END;
          movelist := movelist.tail;
        END;                     (*WHILE*)
      END;                       (*IF*)
    END;
    player.FinishedEvalNode(board);
  END EvaluateBoard;

BEGIN
END Minimax.
