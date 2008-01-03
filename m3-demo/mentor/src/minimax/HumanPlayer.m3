(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

MODULE HumanPlayer;

IMPORT MinimaxAlgClass, ZeusPanel, Thread, MinimaxIE;
IMPORT GameBoard, GamePlay, MoveList;

<*FATAL Thread.Alerted *>

REVEAL
  InteractivePlayer =
    GamePlay.Player BRANDED OBJECT OVERRIDES GetMove := DoGetMove; END;

VAR currentAlg: MinimaxAlgClass.T;
VAR moveAskedFor: BOOLEAN := FALSE;
VAR selectedSquare: GameBoard.Square;

PROCEDURE DoHumanSelection (<*UNUSED*> alg           : MinimaxAlgClass.T;
                                       xCoord, yCoord: INTEGER            ) =
  BEGIN
    IF NOT moveAskedFor THEN RETURN; END;
    selectedSquare := GameBoard.Square{xCoord, yCoord};
    ZeusPanel.EndFeedback(currentAlg);
  END DoHumanSelection;

PROCEDURE DoGetMove (<*UNUSED*> self: InteractivePlayer; board: GameBoard.T):
  GameBoard.Move RAISES {Thread.Alerted} =
  VAR move: GameBoard.Move;
  BEGIN
    REPEAT
      moveAskedFor := TRUE;
      ZeusPanel.StartFeedback(currentAlg);
      move.fromSquare := selectedSquare;
      moveAskedFor := FALSE;
      MinimaxIE.HumanCellSelected(
        currentAlg, move.fromSquare.x, move.fromSquare.y);
      moveAskedFor := TRUE;
      ZeusPanel.StartFeedback(currentAlg);
      move.toSquare := selectedSquare;
      moveAskedFor := FALSE;
      IF MoveList.Member(board.legalMoves(), move) THEN
        EXIT;
      ELSE
        MinimaxIE.HumanIllegalMove(currentAlg);
      END;
    UNTIL FALSE;
    RETURN move;
  END DoGetMove;

PROCEDURE InitModule (alg: MinimaxAlgClass.T) =
  BEGIN
    currentAlg := alg;
  END InitModule;

BEGIN
END HumanPlayer.

