(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

INTERFACE GameBoard;

IMPORT GameMove, MoveList;

(* This interface defines the type representing the players, and the board
   position of the game *)

(* BoardSize is the size of the board *)
CONST BoardSize = 3;

TYPE
  (* There are two players, PlayerA and PlayerB.  Neither represents
     neither player, and is defined for convenience.  We will consider that
     every cell on the board has a value which is a Player -- the value
     PlayerId.PlayerA will indicate that one of player A's pieces on the
     square, while PlayerId.Neither will indicate that neither player
     occupies that square. *)
  PlayerId = {PlayerA, PlayerB, Neither};

  Square = GameMove.Square;

  Move = GameMove.T;

  (* T is the type of a board *)

  T <: Public;

  Public = OBJECT
           METHODS
             Init           (): T;
             squareContents (square: Square): PlayerId;
             moveNumber     (): CARDINAL;
             toMove         (): PlayerId;
             legalMoves     (): MoveList.T;
             finished       (VAR (*OUT*) winner: PlayerId): BOOLEAN;
             previous       (): T;
             Move           (move: Move): T;
           END;

(* The methods of T are as follows.  Methods whose names begin with small
   letters give information about the state of the board, while methods
   starting with capital letters perform some action.  No methods have
   side-effects upon the board except for Init. *)

(* Init initializes the board. *)

(* squareContents returns the player whose piece occupies the given square.
   PlayerId.Neither indicates the square is empty. *)

(* moveNumber indicates how many turns have passed, where the game begins
   with moveNumber 0 *)

(* toMove returns the player whose turn it is to move *)

(* legalMoves returns the list of legal moves that the moving player can
   make *)

(* finished returns TRUE iff the game is over.  If it returns TRUE, then
   winner is set to the player who won.  If winner = PlayerId.Neither, then
   the game ended in a tie. *)

(* previous returns the board position before the last move, or NIL IF
   this board is the first move of the game *)

(* Move takes a move, and returns a new board, reflecting that move *)

END GameBoard.
