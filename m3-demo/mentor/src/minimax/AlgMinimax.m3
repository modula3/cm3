(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Aug 19 21:01:24 PDT 1993 by weber *)
(* modified on Tue Jul 13 10:57:18 PDT 1993 by horning *)

<* PRAGMA LL *>
MODULE AlgMinimax;

IMPORT Algorithm, Thread, ZeusPanel, VBT, FormsVBT;
IMPORT MinimaxAlgClass, MinimaxIE;
IMPORT Text;
IMPORT GameBoard, GamePlay, Minimax, HumanPlayer;

TYPE
  T = MinimaxAlgClass.T BRANDED OBJECT
      OVERRIDES
        run              := Run;
        feHumanSelection := HumanPlayer.DoHumanSelection
      END;

VAR currentAlg: T;

PROCEDURE New (): Algorithm.T =
  BEGIN
    RETURN NEW(T, data := ZeusPanel.NewForm("MinimaxInput.fv")).init()
  END New;

TYPE
  InteractivePlayer = HumanPlayer.InteractivePlayer;

  NonPrintingMatch = GamePlay.Match OBJECT
                     OVERRIDES
                       MoveMade     := DoMoveMade;
                       MoveAskedFor := DoMoveAskedFor;
                     END;

  MinimaxPlayer = Minimax.Minimax OBJECT
                  OVERRIDES
                    CallHeuristic    := DoCallHeuristic;
                    EvaluateNode     := DoEvaluateNode;
                    UpdatedNodeValue := DoUpdatedNodeValue;
                    FinishedEvalNode := DoFinishedEvalNode;
                  END;

PROCEDURE DoMoveAskedFor (<*UNUSED*> self  : NonPrintingMatch;
                                     player: GameBoard.PlayerId;
                                     board : GameBoard.T         )
  RAISES {Thread.Alerted} =
  BEGIN
    MinimaxIE.PlayerThinking(currentAlg, ORD(player), board);
  END DoMoveAskedFor;

PROCEDURE DoFinishedEvalNode (<*UNUSED*> self : MinimaxPlayer;
                                         board: GameBoard.T    )
  RAISES {Thread.Alerted} =
  BEGIN
    MinimaxIE.FinishedEvalNode(currentAlg, board);
  END DoFinishedEvalNode;

PROCEDURE DoCallHeuristic (<*UNUSED*> self : MinimaxPlayer;
                                      board: GameBoard.T;
                                      val  : Minimax.BoardValue)
  RAISES {Thread.Alerted} =
  BEGIN
    MinimaxIE.BoardValueUpdated(currentAlg, board, val);
  END DoCallHeuristic;

PROCEDURE DoEvaluateNode (<*UNUSED*> self : MinimaxPlayer;
                                     board: GameBoard.T    )
  RAISES {Thread.Alerted} =
  BEGIN
    MinimaxIE.EvaluateNode(currentAlg, board);
  END DoEvaluateNode;

PROCEDURE DoUpdatedNodeValue (<*UNUSED*> self : MinimaxPlayer;
                                         board: GameBoard.T;
                                         val  : Minimax.BoardValue)
  RAISES {Thread.Alerted} =
  BEGIN
    MinimaxIE.BoardValueUpdated(currentAlg, board, val);
  END DoUpdatedNodeValue;

PROCEDURE Heuristic (<*UNUSED*> player: Minimax.Minimax;
                     <*UNUSED*> board : GameBoard.T    ):
  Minimax.BoardValue =
  BEGIN
    RETURN 0;
  END Heuristic;

PROCEDURE SmarterHPHeuristic (player: Minimax.Minimax; board: GameBoard.T):
  Minimax.BoardValue =
  VAR
    res      : INTEGER            := 0;
    myself   : GameBoard.PlayerId := player.position();
    cell     : GameBoard.PlayerId;
    cellvalue: INTEGER;
  BEGIN
    FOR y := 0 TO 2 DO
      cell := board.squareContents(GameBoard.Square{1, y});
      IF y = 1 THEN cellvalue := 2 ELSE cellvalue := 1; END;
      IF cell # GameBoard.PlayerId.Neither THEN
        IF cell = myself THEN
          res := res + cellvalue;
        ELSE
          res := res - cellvalue;
        END;
      END;
    END;
    RETURN 2 * res;
  END SmarterHPHeuristic;

PROCEDURE DoMoveMade (<*UNUSED*> self  : NonPrintingMatch;
                                 player: GameBoard.PlayerId;
                                 move  : GameBoard.Move;
                      <*UNUSED*> board : GameBoard.T         )
  RAISES {Thread.Alerted} =
  BEGIN
    MinimaxIE.PlayerMove(
      currentAlg, ORD(player), move.fromSquare.x, move.fromSquare.y,
      move.toSquare.x, move.toSquare.y);
  END DoMoveMade;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  VAR
    winner          : GameBoard.PlayerId;
    playerA, playerB: GamePlay.Player;
  BEGIN
    currentAlg := alg;
    HumanPlayer.InitModule(alg);
    GetInputs(playerA, playerB);
    MinimaxIE.Setup(alg);
    TRY
      winner := NEW(NonPrintingMatch).Init(playerA, playerB).Play();
    EXCEPT
      Thread.Alerted => RAISE Thread.Alerted;
    ELSE
    END;
    MinimaxIE.Finished(alg, ORD(winner));
  END Run;

PROCEDURE GetInputs (VAR playerA, playerB: GamePlay.Player) =

  PROCEDURE GetPlayer (READONLY name: TEXT): GamePlay.Player =
    <*FATAL FormsVBT.Error, FormsVBT.Unimplemented*>
    VAR
      humanComp: TEXT;
      depth    : INTEGER;
      heuristic: TEXT;
      mmplayer : MinimaxPlayer;
    BEGIN
      LOCK VBT.mu DO
        humanComp :=
          FormsVBT.GetChoice(currentAlg.data, name & "HumanComp");
      END;
      IF Text.Equal(humanComp, name & "Human") THEN
        RETURN NEW(InteractivePlayer);
      ELSE
        mmplayer := NEW(MinimaxPlayer);
        LOCK VBT.mu DO
          depth := FormsVBT.GetInteger(currentAlg.data, name & "Depth");
        END;
        mmplayer.SetDepth(depth);
        LOCK VBT.mu DO
          heuristic :=
            FormsVBT.GetChoice(currentAlg.data, name & "Hexpawn");
        END;
        IF Text.Equal(heuristic, name & "HPTrivial") THEN
          mmplayer.SetHeuristic(Heuristic);
        ELSE
          mmplayer.SetHeuristic(SmarterHPHeuristic);
        END;
        RETURN mmplayer;
      END;
    END GetPlayer;
  BEGIN
    playerA := GetPlayer("A");
    playerB := GetPlayer("B");
  END GetInputs;

BEGIN
  ZeusPanel.RegisterAlg(New, "Minimax", "Minimax");
END AlgMinimax.
