(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

MODULE TypeFmt;

IMPORT GameBoard, GameBoardPrivate, Fmt, Text;

(* This module translates M3 types that occur as arguments to events into a
   form that obliq can use *)

PROCEDURE Board (board: GameBoard.T): TEXT =
  VAR
    arrayRep: TEXT    := "";
    contents: INTEGER;
  BEGIN
    FOR x := 0 TO 2 DO
      FOR y := 0 TO 2 DO
        contents := ORD(board.squareContents(GameBoard.Square{x, y}));
        IF Text.Empty(arrayRep) THEN
          arrayRep := Fmt.Int(contents);
        ELSE
          arrayRep := arrayRep & "," & Fmt.Int(contents)
        END
      END;
    END;
    RETURN "[" & Fmt.Int(board.getKey()) & ", [" & arrayRep & "]]";
  END Board;

PROCEDURE BoardKey (board: GameBoard.T): TEXT =
  BEGIN
    RETURN Fmt.Int(board.getKey());
  END BoardKey;

BEGIN
END TypeFmt.


