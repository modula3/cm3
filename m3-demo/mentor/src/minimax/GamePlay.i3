(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

INTERFACE GamePlay;

IMPORT GameBoard;

(* A Player represents one of the sides playing the game.  A Player is
   either PlayerId.PlayerA or PlayerId.PlayerB, and, when given a board
   position, gives back a move to make. *)

TYPE

  PlayerId = GameBoard.PlayerId;

  Player <: PlayerPublic;

  PlayerPublic =
    OBJECT
    METHODS
      (* Init(name) initializes the player to play position "position" *)
      Init (position: PlayerId): Player;
      (* position returns the position that the player is playing. *)
      position (): PlayerId;
      (* GetMove takes a board position and returns the move that should be
         made in that position.  This method might have side effects upon
         the player. *)
      GetMove (board: GameBoard.T): GameBoard.Move RAISES ANY;
    END;

  (* The Match object is used to play out a match between two players.  It
     is an object to make it easy to attack callbacks to. *)
  Match <: MatchPublic;

  MatchPublic =
    OBJECT
    METHODS
      (* Init initializes a match between two players.  It calls the Init
         method of the two players *)
      Init (playerA, playerB: Player): Match;
      (* MoveMade is a callback, which is called whenever a player makes a
         move. *)
      MoveMade (player  : PlayerId;
                move    : GameBoard.Move;
                newBoard: GameBoard.T     ) RAISES ANY;

      (* MoveAskedFor is a callback, which is called whenever a player is
         required to return a move *)
      MoveAskedFor (player : PlayerId; board: GameBoard.T) RAISES ANY;

      (* Play() plays out the match, and returns the name of the winner. *)
      Play (): PlayerId RAISES ANY;
    END;

END GamePlay.

