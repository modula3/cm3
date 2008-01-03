(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Tue Jan 31 15:40:33 PST 1995 by kalsow *)
(*      modified on Thu Apr 28 16:13:41 PDT 1994 by najork *)
(*      modified on Thu Sep 24 12:44:22 PDT 1992 by mhb    *)

MODULE A_RecDescent;

IMPORT Algorithm, Thread, ZeusPanel, ZeusCodeView, RefList;
IMPORT Parse, ParseIE, ParseAlgClass, Token;

TYPE 
  T = ParseAlgClass.T BRANDED OBJECT
        state  : Parse.State;
        input  : REF ARRAY OF Token.T;
        tok    : Token.T;
        cursor : INTEGER;
        next_id: INTEGER;
      OVERRIDES
        run := Run;
      END;

EXCEPTION
  SyntaxError;

PROCEDURE Run (t: T) RAISES {Thread.Alerted} =
  BEGIN
    t.state   := Parse.Init (t.data);
    t.input   := t.state.input;
    t.cursor  := 0;
    t.next_id := t.state.n_tokens + 1;
    ParseIE.Setup (t, t.state);
PEnter (t, "Parse");
    TRY
At(t, 1);
      Scan (t); (* prime the input stream *)
At(t, 2);
      Program (t);
    EXCEPT SyntaxError =>
      ParseIE.NoteError (t);
    END;
PExit (t);
  END Run;

PROCEDURE Program (t: T) RAISES {Thread.Alerted, SyntaxError} =
  VAR self := t.next_id;
  BEGIN
    INC (t.next_id);
PEnter (t, "Program");
    ParseIE.NewNode (t, self, "<program>");
    ParseIE.UpdateDone (t);
    ParseIE.Push (t, self, "<program>");
    LOOP
At (t, 1);
      Stmt (t, self);
At (t, 2);
      IF (t.tok # Token.T.Semi) THEN EXIT END;
At (t, 3);
      Match (t, Token.T.Semi, self);
    END;
At (t, 4);
    Match (t, Token.T.EOF, self);
    ParseIE.Pop (t, self);
PExit (t);
  END Program;

PROCEDURE Stmt (t: T;  parent: INTEGER) RAISES {Thread.Alerted, SyntaxError} =
  VAR self := t.next_id;
  BEGIN
    INC (t.next_id);
PEnter (t, "Stmt");
    ParseIE.NewNode (t, self, "<stmt>");
    ParseIE.NewEdge (t, self, parent);
    ParseIE.UpdateDone (t);
    ParseIE.Push (t, self, "<stmt>");
At (t, 1);
    Match (t, Token.T.Id, self);
At (t, 2);
    Match (t, Token.T.Assign, self);
At (t, 3);
    Expr (t, self);
    ParseIE.Pop (t, self);
PExit (t);
  END Stmt;

PROCEDURE Expr (t: T;  parent: INTEGER) RAISES {Thread.Alerted, SyntaxError} =
  VAR self := t.next_id;
  BEGIN
    INC (t.next_id);
PEnter (t, "Expr");
    ParseIE.NewNode (t, self, "<expr>");
    ParseIE.NewEdge (t, self, parent);
    ParseIE.UpdateDone (t);
    ParseIE.Push (t, self, "<expr>");
At (t, 1);
    Term (t, self);
At (t, 2);
    WHILE (t.tok = Token.T.Plus) DO
At (t, 3);
      Match (t, Token.T.Plus, self);
At (t, 4);
      Term (t, self);
    END;
At (t, 2);
    ParseIE.Pop (t, self);
PExit (t);
  END Expr;

PROCEDURE Term (t: T;  parent: INTEGER) RAISES {Thread.Alerted, SyntaxError} =
  VAR self := t.next_id;
  BEGIN
    INC (t.next_id);
PEnter (t, "Term");
    ParseIE.NewNode (t, self, "<term>");
    ParseIE.NewEdge (t, self, parent);
    ParseIE.UpdateDone (t);
    ParseIE.Push (t, self, "<term>");
At (t, 1);
    Factor (t, self);
At (t, 2);
    WHILE (t.tok = Token.T.Star) DO
At (t, 3);
      Match (t, Token.T.Star, self);
At (t, 4);
      Factor (t, self);
    END;
At (t, 2);
    ParseIE.Pop (t, self);
PExit (t);
  END Term;

PROCEDURE Factor (t: T;  parent: INTEGER) RAISES {Thread.Alerted, SyntaxError}=
  VAR self := t.next_id;
  BEGIN
    INC (t.next_id);
PEnter (t, "Factor");
    ParseIE.NewNode (t, self, "<factor>");
    ParseIE.NewEdge (t, self, parent);
    ParseIE.UpdateDone (t);
    ParseIE.Push (t, self, "<factor>");
At (t, 1);
    IF (t.tok = Token.T.LParen) THEN
At (t, 4);
      Match (t, Token.T.LParen, self);
At (t, 5);
      Expr (t, self);
At (t, 6);
      Match (t, Token.T.RParen, self);
    ELSE
At (t, 2);
      Match (t, Token.T.Id, self);
    END;
    ParseIE.Pop (t, self);
PExit (t);
  END Factor;

PROCEDURE Match (t: T;  tok: Token.T;  parent: INTEGER)
  RAISES {Thread.Alerted, SyntaxError} =
  BEGIN
    IF (t.tok # tok) THEN RAISE SyntaxError END;
    ParseIE.NewEdge (t, t.cursor-1, parent);
    ParseIE.UpdateDone (t);
    Scan (t);
  END Match;

PROCEDURE Scan (t: T) RAISES {Thread.Alerted} =
  VAR x := MIN (t.cursor, t.state.n_tokens-1);
  BEGIN
    t.tok := t.state.input [x];
    ParseIE.Scan (t, t.state.tokens [x]);
    INC (t.cursor);
  END Scan;

PROCEDURE At (t: T;  line: INTEGER) RAISES {Thread.Alerted} =
  BEGIN ZeusCodeView.At (t, line) END At;

PROCEDURE PEnter (t: T;  proc: TEXT) RAISES {Thread.Alerted} =
  BEGIN ZeusCodeView.Enter (t, procedureName := proc) END PEnter;

PROCEDURE PExit (t: T) RAISES {Thread.Alerted} =
  BEGIN ZeusCodeView.Exit (t) END PExit;

PROCEDURE New (): Algorithm.T =
  VAR fv := ZeusPanel.NewForm("Parse.fv");
      cv := RefList.List1 (RefList.List2 ("code view", "A_RecDescent.code"));
  BEGIN
    RETURN NEW (T, data := fv, codeViews := cv).init ()
  END New;

BEGIN
  ZeusPanel.RegisterAlg(New, "recursive descent", "Parse");
END A_RecDescent.

