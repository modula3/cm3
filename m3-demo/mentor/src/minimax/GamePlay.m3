(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

MODULE GamePlay;

IMPORT GameBoard;

REVEAL
  Player = PlayerPublic BRANDED OBJECT
             (* positionPlaying is the position the player is playing *)
             positionPlaying: PlayerId;
           OVERRIDES
             Init     := DoPlayerInit;
             position := DoPlayerPosition;
           END;

  Match =
    MatchPublic BRANDED OBJECT
      (* players is an array of players *)
      players: ARRAY [PlayerId.PlayerA .. PlayerId.PlayerB] OF Player;
      (* The current board position *)
      board: GameBoard.T;
    OVERRIDES
      Init := DoMatchInit;
      Play := DoMatchPlay;
    END;

PROCEDURE DoPlayerInit (self: Player; position: PlayerId): Player =
  BEGIN
    self.positionPlaying := position;
    RETURN self;
  END DoPlayerInit;

PROCEDURE DoPlayerPosition (self: Player): PlayerId =
  BEGIN
    RETURN self.positionPlaying;
  END DoPlayerPosition;

PROCEDURE DoMatchInit (self: Match; playerA, playerB: Player): Match =
  BEGIN
    self.players := ARRAY [PlayerId.PlayerA .. PlayerId.PlayerB] OF
                      Player{playerA, playerB};
    EVAL playerA.Init(PlayerId.PlayerA);
    EVAL playerB.Init(PlayerId.PlayerB);
    self.board := NEW(GameBoard.T).Init();
    RETURN self;
  END DoMatchInit;

PROCEDURE DoMatchPlay (self: Match): PlayerId RAISES ANY =
  VAR
    winner  : PlayerId;
    move    : GameBoard.Move;
    newBoard: GameBoard.T;
  BEGIN
    WHILE NOT self.board.finished(winner) DO
      self.MoveAskedFor(self.board.toMove(), self.board);
      move := self.players[self.board.toMove()].GetMove(self.board);
      newBoard := self.board.Move(move);
      self.MoveMade(self.board.toMove(), move, newBoard);
      self.board := newBoard;
    END;
    RETURN winner;
  END DoMatchPlay;

BEGIN
END GamePlay.



