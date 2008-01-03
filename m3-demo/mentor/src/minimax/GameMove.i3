(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

INTERFACE GameMove;

(* GameMove.T represents one move in the game.  It gives the square that the
   moved piece started from, and the square that it ended up in. *)

CONST Brand = "GameMove";

TYPE
  T = RECORD
        fromSquare: Square;
        toSquare  : Square;
      END;

(* A Square is simply a pair of x and y co-ordinates *)

TYPE
  Square = RECORD
           x: INTEGER;
           y: INTEGER
         END;

PROCEDURE Equal (move1, move2: T): BOOLEAN;

END GameMove.
