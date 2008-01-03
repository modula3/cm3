(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Tue Jan 31 15:40:32 PST 1995 by kalsow *)
(*      modified on Thu Apr 28 16:23:48 PDT 1994 by najork *)
(*      modified on Thu Sep 24 12:43:53 PDT 1992 by mhb    *)

MODULE A_BottomUp;

IMPORT Algorithm, Thread, ZeusPanel, ZeusCodeView, RefList;
IMPORT Parse, ParseIE, ParseAlgClass;
IMPORT Wr, Stdio, Fmt; (* debugging *)

VAR DEBUG := FALSE;

TYPE 
  T = ParseAlgClass.T BRANDED OBJECT
        input   : Parse.State;
        cursor  : INTEGER;
        next_id : INTEGER;
        state   : INTEGER;
        sym     : Symbol;
        sym_node: INTEGER;
        tos     : INTEGER;
        stack   : ARRAY [0..99] OF StackEntry;
      OVERRIDES
        run := Run;
      END;

TYPE
  Action = { Shift, Reduce, Goto, Accept, Error };
  Symbol = { LParen, RParen, Plus, Star, Assign, Id, Semi, EOF,
             Program, StmtList, Stmt, Expr, Term, Factor };

CONST
  ActNames = ARRAY Action OF TEXT {
    "shift", "reduce", "goto", "accept", "error"
  };

CONST
  Names = ARRAY Symbol OF TEXT {
    "(", ")", "+", "*", "=", "<ID>", ";", "$",
    "<program>", "<stmt list>", "<stmt>", "<expr>", "<term>", "<factor>"
  };

TYPE
  StackEntry = RECORD state: INTEGER;  node: INTEGER END;
  Entry = RECORD state: INTEGER;  sym: Symbol;  act: Action;  op: INTEGER END;
  Production = RECORD lhs: Symbol;  rhs_len: INTEGER END;

CONST
  X = ARRAY [0..31] OF Entry {
    (* 0 *)
    Entry {  0, Symbol.Id,        Action.Shift,    3 },
    Entry {  0, Symbol.StmtList,  Action.Goto,     1 },
    Entry {  0, Symbol.Stmt,      Action.Goto,     2 },
    Entry {  0, Symbol.Program,   Action.Goto,     0 },
    (* 4 *)
    Entry {  1, Symbol.EOF,       Action.Accept,   0 },
    (* 5 *)
    Entry {  2, Symbol.Semi,      Action.Shift,    4 },
    (* 6 *)
    Entry {  3, Symbol.Assign,    Action.Shift,    5 },
    (* 7 *)
    Entry {  4, Symbol.Id,        Action.Shift,    3 },
    Entry {  4, Symbol.StmtList,  Action.Goto,     6 },
    Entry {  4, Symbol.Stmt,      Action.Goto,     2 },
    (* 10 *)
    Entry {  5, Symbol.Id,        Action.Shift,   10 },
    Entry {  5, Symbol.LParen,    Action.Shift,   11 },
    Entry {  5, Symbol.Expr,      Action.Goto,     7 },
    Entry {  5, Symbol.Term,      Action.Goto,     8 },
    Entry {  5, Symbol.Factor,    Action.Goto,     9 },
    (* 15 *)
    (* -- state 6 only has a default action -- *)
    (* 15 *)
    Entry {  7, Symbol.Plus,      Action.Shift,   12 },
    (* 16 *)
    Entry {  8, Symbol.Star,      Action.Shift,   13 },
    (* 17 *)
    (* -- state 9 only has a default action -- *)
    (* 17 *)
    (* -- state 10 only has a default action -- *)
    (* 17 *)
    Entry { 11, Symbol.Id,        Action.Shift,   10 },
    Entry { 11, Symbol.LParen,    Action.Shift,   11 },
    Entry { 11, Symbol.Expr,      Action.Goto,    14 },
    Entry { 11, Symbol.Term,      Action.Goto,     8 },
    Entry { 11, Symbol.Factor,    Action.Goto,     9 },
    (* 22 *)
    Entry { 12, Symbol.Id,        Action.Shift,   10 },
    Entry { 12, Symbol.LParen,    Action.Shift,   11 },
    Entry { 12, Symbol.Term,      Action.Goto,    15 },
    Entry { 12, Symbol.Factor,    Action.Goto,     9 },
    (* 26 *)
    Entry { 13, Symbol.Id,        Action.Shift,   10 },
    Entry { 13, Symbol.LParen,    Action.Shift,   11 },
    Entry { 13, Symbol.Factor,    Action.Goto,    16 },
    (* 29 *)
    Entry { 14, Symbol.RParen,    Action.Shift,   17 },
    Entry { 14, Symbol.Plus,      Action.Shift,   12 },
    (* 31 *)
    Entry { 15, Symbol.Star,      Action.Shift,   13 }
    (* 32 *)
    (* -- state 16 only has a default action -- *)
    (* 32 *)
    (* -- state 17 only has a default action -- *)
    (* 32 *)
  };

CONST
  Default_action = ARRAY [0..17] OF Entry {
    Entry {  0, Symbol.EOF,       Action.Error,   0 },
    Entry {  1, Symbol.EOF,       Action.Error,   0 },
    Entry {  2, Symbol.EOF,       Action.Reduce,  1 },
    Entry {  3, Symbol.EOF,       Action.Error,   0 },
    Entry {  4, Symbol.EOF,       Action.Error,   0 },
    Entry {  5, Symbol.EOF,       Action.Error,   0 },
    Entry {  6, Symbol.EOF,       Action.Reduce,  2 },
    Entry {  7, Symbol.EOF,       Action.Reduce,  3 },
    Entry {  8, Symbol.EOF,       Action.Reduce,  4 },
    Entry {  9, Symbol.EOF,       Action.Reduce,  6 },
    Entry { 10, Symbol.EOF,       Action.Reduce,  8 },
    Entry { 11, Symbol.EOF,       Action.Error,   0 },
    Entry { 12, Symbol.EOF,       Action.Error,   0 },
    Entry { 13, Symbol.EOF,       Action.Error,   0 },
    Entry { 14, Symbol.EOF,       Action.Error,   0 },
    Entry { 15, Symbol.EOF,       Action.Reduce,  5 },
    Entry { 16, Symbol.EOF,       Action.Reduce,  7 },
    Entry { 17, Symbol.EOF,       Action.Reduce,  9 }
  };

CONST
  State_Start = ARRAY [0..18] OF INTEGER {
    0, 4, 5, 6, 7, 10, 15, 15, 16, 17, 17, 17, 22, 26, 29, 31, 32, 32, 32
  };

CONST
  Reduce = ARRAY [0..9] OF Production {
    Production { Symbol.Program,  2 },  (* <pgm>    ::= <s-list> $        *)
    Production { Symbol.StmtList, 1 },  (* <s-list> ::= <stmt>            *)
    Production { Symbol.StmtList, 3 },  (* <s-list> ::= <stmt> ; <s-list> *)
    Production { Symbol.Stmt,     3 },  (* <stmt>   ::= <Id> = <expr>     *)
    Production { Symbol.Expr,     1 },  (* <expr>   ::= <term>            *)
    Production { Symbol.Expr,     3 },  (* <expr>   ::= <expr> + <term>   *)
    Production { Symbol.Term,     1 },  (* <term>   ::= <factor>          *)
    Production { Symbol.Term,     3 },  (* <term>   ::= <term> * <factor> *)
    Production { Symbol.Factor,   1 },  (* <factor> ::= <Id>              *)
    Production { Symbol.Factor,   3 }   (* <factor> ::= ( <expr> )        *)
  };

PROCEDURE Run (t: T) RAISES {Thread.Alerted} =
  VAR
    act   : Action;
    info  : INTEGER;
  BEGIN
    t.input   := Parse.Init (t.data);
    t.cursor  := 0;
    t.next_id := t.input.n_tokens + 1;
    t.tos     := 0;
    t.state   := 0;
    ParseIE.Setup (t, t.input);
PEnter (t, "Parse");

At (t, 1);
At (t, 2);
    Scan (t); (* prime the input stream *)
    LOOP
Debug ("state ", Fmt.Int (t.state));
Debug ("sym   ", Names [t.sym]);
      LookUp (t.state, t.sym, act, info);
Debug ("  action ", ActNames[act], " ", Fmt.Int (info));
At (t, 3);
      CASE act OF

      | Action.Shift  =>
At (t, 4);
At (t, 5);
          DoShift (t);
At (t, 6);
          t.state := info;
At (t, 7);
          Scan (t);

      | Action.Goto  =>
At (t, 8);
At (t, 9);
          DoGoto (t);
At (t, 10);
          t.state := info;

      | Action.Reduce =>
At (t, 11);
          DoReduce (t, Reduce [info]);

      | Action.Accept =>
At (t, 15);
          DoShift (t); (* for its animation effect *)
          DoReduce (t, Reduce [0]);
At (t, 16);
          EXIT;

      | Action.Error  =>
At (t, 17);
At (t, 18);
          ParseIE.NoteError (t);
At (t, 19);
          EXIT;
      END;
    END;
At (t, 20);
PExit (t);
  END Run;

PROCEDURE DoShift (t: T) RAISES {Thread.Alerted} =
  VAR x := t.next_id;
  BEGIN
    INC (t.next_id);  (* allocate a new "terminal" node *)
    ParseIE.NewTerm (t, x, Names [t.sym]);
    ParseIE.NewEdge (t, t.sym_node, x);
    ParseIE.UpdateDone (t);
    t.sym_node := x;
    DoGoto (t);
  END DoShift;

PROCEDURE DoGoto (t: T) RAISES {Thread.Alerted} =
  BEGIN
    ParseIE.Push (t, t.sym_node, Names [t.sym]);
    t.stack [t.tos].node  := t.sym_node;
    t.stack [t.tos].state := t.state;
    INC (t.tos);
  END DoGoto;

PROCEDURE DoReduce (t: T;  p: Production) RAISES {Thread.Alerted} =
  VAR x := t.next_id;  act: Action;  info: INTEGER;
  BEGIN
    INC (t.next_id); (* allocate a new parse node *)
    ParseIE.NewNode (t, x, Names [p.lhs]);

    (* link the subtrees and pop the stack *)
    FOR i := t.tos - p.rhs_len TO t.tos - 1 DO
      ParseIE.NewEdge (t, t.stack[i].node, x);
    END;
At (t, 12);
    FOR i := t.tos - 1 TO t.tos - p.rhs_len BY -1 DO
      ParseIE.Pop (t, t.stack[i].node);
    END;
    DEC (t.tos, p.rhs_len);

    (* and redraw the graph *)
    ParseIE.UpdateDone (t);

At (t, 13);
    (* recover the old state *)
    IF (t.tos >= 0)
      THEN t.state := t.stack [t.tos].state;
      ELSE t.state := 0;
    END;

At (t, 14);
    (* push the lhs on the stack from the old state*)
    ParseIE.Push (t, x, Names [p.lhs]);
    t.stack [t.tos].node  := x;
    t.stack [t.tos].state := t.state;
    INC (t.tos);

    (* consult the goto table to find the new state *)
    LookUp (t.state, p.lhs, act, info);
    IF (act = Action.Goto)
      THEN t.state := info;
      ELSE ParseIE.NoteError (t);
    END;
  END DoReduce;

PROCEDURE LookUp (state: INTEGER;  s: Symbol;
                  VAR act: Action;  VAR info: INTEGER) =
  BEGIN
    FOR i := State_Start [state] TO State_Start [state+1] - 1 DO
      IF X[i].sym = s THEN act := X[i].act;  info := X[i].op; RETURN END;
    END;
    act  := Default_action [state].act;
    info := Default_action [state].op;
  END LookUp;

PROCEDURE Scan (t: T) RAISES {Thread.Alerted} =
  VAR x := MIN (t.cursor, t.input.n_tokens - 1);
  BEGIN
    ParseIE.Scan (t, t.input.tokens[x]);
    t.sym := VAL (ORD (t.input.input[x]), Symbol);
    t.sym_node := x;
    INC (t.cursor);
  END Scan;

PROCEDURE Debug (a, b, c, d: TEXT := NIL) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  BEGIN
    IF NOT DEBUG THEN RETURN END;
    IF (a # NIL) THEN Wr.PutText (Stdio.stdout, a) END;
    IF (b # NIL) THEN Wr.PutText (Stdio.stdout, b) END;
    IF (c # NIL) THEN Wr.PutText (Stdio.stdout, c) END;
    IF (d # NIL) THEN Wr.PutText (Stdio.stdout, d) END;
    Wr.PutText (Stdio.stdout, "\n");
    Wr.Flush (Stdio.stdout);
  END Debug;

PROCEDURE At (t: T;  line: INTEGER) RAISES {Thread.Alerted} =
  BEGIN ZeusCodeView.At (t, line) END At;

PROCEDURE PEnter (t: T;  proc: TEXT) RAISES {Thread.Alerted} =
  BEGIN ZeusCodeView.Enter (t, procedureName := proc) END PEnter;

PROCEDURE PExit (t: T) RAISES {Thread.Alerted} =
  BEGIN ZeusCodeView.Exit (t) END PExit;

PROCEDURE New (): Algorithm.T =
  VAR fv := ZeusPanel.NewForm("Parse.fv");
      cv := RefList.List1 (RefList.List2 ("code view", "A_BottomUp.code"));
  BEGIN
    RETURN NEW (T, data := fv, codeViews := cv).init ()
  END New;

BEGIN
  ZeusPanel.RegisterAlg(New, "bottom up", "Parse");
END A_BottomUp.

