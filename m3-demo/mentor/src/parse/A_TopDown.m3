(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Tue Jan 31 15:40:31 PST 1995 by kalsow *)
(*      modified on Thu Apr 28 16:24:26 PDT 1994 by najork *)
(*      modified on Thu Sep 24 12:44:38 PDT 1992 by mhb    *)

MODULE A_TopDown;

IMPORT Algorithm, Thread, ZeusPanel, ZeusCodeView, RefList;
IMPORT Parse, ParseIE, ParseAlgClass;
IMPORT Wr, Stdio, Fmt; (* for debugging *)

VAR DEBUG := FALSE;

TYPE 
  T = ParseAlgClass.T BRANDED OBJECT
        input   : Parse.State;
        cursor  : INTEGER;
        next_id : INTEGER;
        sym     : Symbol;
        tos     : INTEGER;
        stack   : ARRAY [0..99] OF StackEntry;
      OVERRIDES
        run := Run;
      END;

TYPE
  Symbol = { LParen, RParen, Plus, Star, Assign, Id, Semi, EOF,
             Program, StmtList, StmtTail, Stmt,
             Expr, ExprTail, Term, TermTail, Factor };

CONST
  First_Terminal = Symbol.LParen;
  Last_Terminal  = Symbol.EOF;

CONST
  Names = ARRAY Symbol OF TEXT {
    "(", ")", "+", "*", "=", "<ID>", ";", "$",
    "<program>", "<stmt list>", "<s-tail>", "<stmt>",
    "<expr>", "<e-tail>", "<term>", "<t-tail>", "<factor>"
  };

TYPE
  StackEntry = RECORD sym: Symbol;  node: INTEGER END;
  Entry = RECORD stack, input: Symbol;  production: INTEGER END;

TYPE
  RHS = ARRAY [0..2] OF Symbol;
  Prod = RECORD
    lhs: Symbol;
    len: INTEGER;
    rhs: RHS;
  END;

CONST
  Production = ARRAY [0..12] OF Prod {
     Prod { Symbol.Program,  2, RHS{Symbol.StmtList, Symbol.EOF, .. }},
     Prod { Symbol.StmtList, 2, RHS{Symbol.Stmt, Symbol.StmtTail, .. }},
     Prod { Symbol.StmtTail, 0, RHS{Symbol.EOF, .. }},
     Prod { Symbol.StmtTail, 3, RHS{Symbol.Semi,Symbol.Stmt,Symbol.StmtTail}},
     Prod { Symbol.Stmt,     3, RHS{Symbol.Id,Symbol.Assign,Symbol.Expr}},
     Prod { Symbol.Expr,     2, RHS{Symbol.Term, Symbol.ExprTail, .. }},
     Prod { Symbol.ExprTail, 0, RHS{Symbol.EOF, .. }},
     Prod { Symbol.ExprTail, 2, RHS{Symbol.Plus, Symbol.Expr, ..}},
     Prod { Symbol.Term,     2, RHS{Symbol.Factor, Symbol.TermTail, .. }},
     Prod { Symbol.TermTail, 0, RHS{Symbol.EOF, .. }},
     Prod { Symbol.TermTail, 2, RHS{Symbol.Star, Symbol.Term, ..}},
     Prod { Symbol.Factor,   1, RHS{Symbol.Id, ..}},
     Prod { Symbol.Factor,   3, RHS{Symbol.LParen, Symbol.Expr, Symbol.RParen}}
  };

CONST
  Prediction = ARRAY [0..19] OF Entry {
    Entry { Symbol.Program,  Symbol.Id,      0  },
    Entry { Symbol.StmtList, Symbol.Id,      1  },
    Entry { Symbol.StmtTail, Symbol.EOF,     2  },
    Entry { Symbol.StmtTail, Symbol.Semi,    3  },
    Entry { Symbol.Stmt,     Symbol.Id,      4  },
    Entry { Symbol.Expr,     Symbol.Id,      5  },
    Entry { Symbol.Expr,     Symbol.LParen,  5  },
    Entry { Symbol.ExprTail, Symbol.Semi,    6  },
    Entry { Symbol.ExprTail, Symbol.EOF,     6  },
    Entry { Symbol.ExprTail, Symbol.RParen,  6  },
    Entry { Symbol.ExprTail, Symbol.Plus,    7  },
    Entry { Symbol.Term,     Symbol.Id,      8  },
    Entry { Symbol.Term,     Symbol.LParen,  8  },
    Entry { Symbol.TermTail, Symbol.Semi,    9  },
    Entry { Symbol.TermTail, Symbol.Plus,    9  },
    Entry { Symbol.TermTail, Symbol.EOF,     9  },
    Entry { Symbol.TermTail, Symbol.RParen,  9  },
    Entry { Symbol.TermTail, Symbol.Star,    10 },
    Entry { Symbol.Factor,   Symbol.Id,      11 },
    Entry { Symbol.Factor,   Symbol.LParen,  12 }
  };

PROCEDURE Run (t: T) RAISES {Thread.Alerted} =
  VAR x: Symbol;  node, p: INTEGER;
  BEGIN
    t.input   := Parse.Init (t.data);
    t.cursor  := 0;
    t.next_id := t.input.n_tokens + 1;
    t.tos     := 0;
    ParseIE.Setup (t, t.input);
PEnter (t, "Parse");

At (t, 1);
    Push (t, Symbol.Program, NewNode (t, Symbol.Program, -1));
    ParseIE.UpdateDone (t);
At (t, 2);
    Scan (t); (* prime the input stream *)
    WHILE (t.tos > 0) DO
At (t, 13);
At (t, 3);
      Pop (t, x, node);
Debug ("tok ", Names[t.sym]);
Debug ("pop ", Names[x]);
At (t, 4);
      IF (First_Terminal <= x) AND (x <= Last_Terminal) THEN
Debug (" match ", Names[x]);
        DoMatch (t, x, node);
      ELSIF LookUp (x, t.sym, p) THEN (* a non-term *)
At (t, 8);
At (t, 9);
At (t, 10);
At (t, 11);
Debug (" predict ", Fmt.Int(p));
        DoPredict (t, node, Production[p]);
      ELSE
At (t, 8);
At (t, 9);
At (t, 10);
At (t, 12);
        ParseIE.NoteError (t);
        EXIT;
      END;
    END;
At (t, 13);
    IF (t.cursor < t.input.n_tokens) THEN ParseIE.NoteError (t) END;
At (t, 14);
PExit (t);
  END Run;

PROCEDURE Push (t: T;  sym: Symbol;  self: INTEGER)
  RAISES {Thread.Alerted} =
  BEGIN
    ParseIE.Push (t, self, Names [sym]);
    t.stack [t.tos].sym  := sym;
    t.stack [t.tos].node := self;
    INC (t.tos);
  END Push;

PROCEDURE NewNode (t: T;  sym: Symbol;  parent: INTEGER): INTEGER
  RAISES {Thread.Alerted} =
  VAR self := t.next_id;
  BEGIN
    INC (t.next_id);  (* allocate a parse node for this non-term *)
    IF (First_Terminal <= sym) AND (sym <= Last_Terminal)
      THEN ParseIE.NewTerm (t, self, Names [sym]);
      ELSE ParseIE.NewNode (t, self, Names [sym]);
    END;
    IF (parent >= 0) THEN ParseIE.NewEdge (t, self, parent) END;
    RETURN self;
  END NewNode;

PROCEDURE Pop (t: T; VAR sym: Symbol; VAR n: INTEGER)
  RAISES {Thread.Alerted} =
  BEGIN
    DEC (t.tos);
    sym := t.stack [t.tos].sym;
    n   := t.stack [t.tos].node;
    ParseIE.Pop (t, n);
  END Pop;

PROCEDURE DoMatch (t: T;  sym: Symbol;  parent: INTEGER)
  RAISES {Thread.Alerted} =
  BEGIN
    IF (t.sym # sym) THEN ParseIE.NoteError (t) END;
    ParseIE.NewEdge (t, t.cursor-1, parent);
    ParseIE.UpdateDone (t);
    Scan (t);
  END DoMatch;

PROCEDURE DoPredict (t: T;  parent: INTEGER;  READONLY p: Prod)
  RAISES {Thread.Alerted} =
  VAR x: ARRAY [0..9] OF INTEGER;
  BEGIN
    IF (p.len > 0) THEN
      (* allocate the leaves left-to-right *)
      FOR i := 0 TO p.len - 1 DO
        x[i] := NewNode (t, p.rhs[i], parent);
      END;
      ParseIE.UpdateDone (t);
      (* but, push them on the stack right-to-left *)
      FOR i := p.len - 1 TO 0 BY -1 DO
        Push (t, p.rhs[i], x[i]);
      END;
    ELSE (* epsilon production -> delete parent *)
      ParseIE.DeleteLeaf (t, parent);
      ParseIE.UpdateDone (t);
    END;
  END DoPredict;

PROCEDURE Scan (t: T) RAISES {Thread.Alerted} =
  VAR x := MIN (t.cursor, t.input.n_tokens - 1);
  BEGIN
    ParseIE.Scan (t, t.input.tokens[x]);
    t.sym := VAL (ORD (t.input.input[x]), Symbol);
    INC (t.cursor);
  END Scan;

PROCEDURE LookUp (stack, input: Symbol;
                  VAR prediction: INTEGER): BOOLEAN =
  BEGIN
    FOR i := FIRST (Prediction) TO LAST (Prediction) DO
      WITH z = Prediction[i] DO
        IF (z.stack = stack) AND (z.input = input) THEN
          prediction := z.production;  RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END LookUp;

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
      cv := RefList.List1 (RefList.List2 ("code view", "A_TopDown.code"));
  BEGIN
    RETURN NEW (T, data := fv, codeViews := cv).init ()
  END New;

BEGIN
  ZeusPanel.RegisterAlg(New, "top down", "Parse");
END A_TopDown.

