(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Ident.m3                                              *)
(* Last Modified On Fri Jun 24 15:19:34 PDT 1994 By kalsow     *)

MODULE Ident;

IMPORT Token;
FROM Scanner IMPORT MatchID, GetToken, cur;

PROCEDURE ParseList (): INTEGER =
  VAR start: INTEGER;
  BEGIN
    IF (stack = NIL) THEN Init () END;
    start := top;
    LOOP
      IF (LAST (stack^) < top) THEN ExpandLists () END;
      offset[top] := cur.offset;
      stack[top] := MatchID ();
      INC (top);
      IF (cur.token # Token.T.tCOMMA) THEN EXIT;  END;
      GetToken (); (* , *)
    END;
    RETURN (top - start);
  END ParseList;

PROCEDURE Reset () =
  BEGIN
    top := 0;
  END Reset;

PROCEDURE Init () =
  BEGIN
    stack  := NEW (StringList, 100);
    offset := NEW (IntegerList, 100);
  END Init;

PROCEDURE ExpandLists () =
  VAR newStack  := NEW (StringList, 2 * NUMBER (stack^));
  VAR newOffset := NEW (IntegerList, 2 * NUMBER (offset^));
  BEGIN
    SUBARRAY (newStack^, 0, NUMBER (stack^)) := stack^;
    SUBARRAY (newOffset^, 0, NUMBER (offset^)) := offset^;
    stack  := newStack;
    offset := newOffset;
  END ExpandLists;

BEGIN
END Ident.
