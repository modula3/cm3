(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Mar 28 14:47:44 PST 1996 by heydon                   *)
(*      modified on Tue Aug 31 22:16:59 PDT 1993 by gnelson                  *)
(*      modified on Fri Aug  7 21:54:02 PDT 1992 by myers                    *)

MODULE JunoUnparse;

IMPORT JunoAST AS AST, JunoValue, IndexedNF;
IMPORT Atom, Formatter, Wr, Text, Thread, Fmt, Stdio;

(* Provides procedures for unparsing Juno units (interfaces and modules),
   commands, and expressions.  A number of valid tokens may be specified so
   that incorrect programs may be unparsed up to the point of failure. *)

CONST
  IdIndent = 2;                    (* indentation for identifiers *)
  CmdIndent = 2;                   (* indentation for decl commands *)
  PredIndent = 2;		   (* indentation for decl predicates *)
  
PROCEDURE Block(
    wr: Wr.T;
    ast: AST.Block;
    tokens, indent, width, prec: CARDINAL;
    debug, private: BOOLEAN)
  RAISES {Wr.Failure} =
  VAR f := Formatter.New(wr, width); BEGIN
    Unparse(f, ast, tokens, indent, prec, debug, private);
    Formatter.Close(f)
  END Block;

PROCEDURE Cmd(
    wr: Wr.T;
    ast: AST.Cmd;
    tokens, indent, width, prec: CARDINAL;
    debug: BOOLEAN)
  RAISES {Wr.Failure} =
  VAR f := Formatter.New(wr, width); BEGIN
    Unparse(f, ast, tokens, indent, prec, debug, private := TRUE);
    Formatter.Close(f)
  END Cmd;

PROCEDURE Expr(
    wr: Wr.T;
    ast: AST.Expr;
    tokens, indent, width, prec: CARDINAL;
    debug: BOOLEAN)
  RAISES {Wr.Failure} =
  VAR f := Formatter.New(wr, width); BEGIN
    Unparse(f, ast, tokens, indent, prec, debug, private := TRUE);
    Formatter.Close(f)
  END Expr;

PROCEDURE P(wr: Wr.T; ast: AST.T; indent: CARDINAL := 6;
  width: CARDINAL := 75; prec := Prec; debug := FALSE; private := TRUE;
  errast: AST.T := NIL) RAISES {Wr.Failure} =
  VAR f := Formatter.New(wr, width); BEGIN
    Unparse(f, ast, LAST(INTEGER), indent, prec, debug, private, errast);
    Formatter.Close(f)
  END P;

PROCEDURE ToFmt(fmt: Formatter.T; ast: AST.T; indent, prec: CARDINAL;
  debug, private: BOOLEAN; errast: AST.T := NIL) RAISES {Wr.Failure} =
  BEGIN
    Unparse(fmt, ast, LAST(INTEGER), indent, prec, debug, private, errast)
  END ToFmt;

PROCEDURE Debug(r: REFANY) =
<* FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    TYPECASE r OF <*NOWARN*>
    | AST.T (ast) => P(Stdio.stderr, ast, indent := 4, debug := TRUE)
    | JunoValue.T (v) => JunoValue.Unparse(Stdio.stderr, v, Prec)
    END;
    Wr.PutChar(Stdio.stderr, '\n')
  END Debug;

VAR (*CONST*) refReal := NEW(REF JunoValue.Real); (* Used by "RefReal" *)

PROCEDURE RefReal(r: JunoValue.Real): REF JunoValue.Real =
  BEGIN refReal^ := r; RETURN refReal END RefReal;

EXCEPTION TokenLimit;           (* only used inside "Unparse" *)

(* Here is the real unparsing machinery.  All of the unparsing procedures
   have been encapsulated in this big "Unparse" procedure, which holds the
   global state of the formatter. *)

PROCEDURE Unparse(
    fmt: Formatter.T;
    ast: AST.T;
    tokens, indent, prec: INTEGER;
    debug, private: BOOLEAN;
    errast: AST.T := NIL)
  RAISES {Wr.Failure} =

  (* Formatter state: We need a little more information than just the
     formatter itself.  "count" is the number of tokens output so far, and
     each of the unparsing routines bumps "count" when it sends a token to
     the formatter.  The invariant that "count <= tokens" is maintained,
     and the exception "TokenLimit" is raised if "count = tokens". *)

  VAR
    count := 0;

  PROCEDURE Block(ast: AST.Block) RAISES {TokenLimit, Wr.Failure} =
    VAR appendSemi := TRUE; BEGIN
      CheckStart(ast);
      TYPECASE ast OF
      | AST.Module (m) =>
          Token("MODULE ");
          Id(m.name)
      | AST.Import (i) =>
	  Token("IMPORT ");
	  IdList(i.idList)
      | AST.Comment (c) =>
          Comment(c);
          appendSemi := FALSE
      | AST.UIDecl (d) =>
          UIDecl(d)
      | AST.Decl (d) =>
          Decl(d)
      ELSE
	Token("<UNRECOGNIZED DECLARATION FORM>")
      END;
      IF appendSemi THEN Token(";") END;
      CheckFinish(ast)
    END Block;

  PROCEDURE Comment(c: AST.Comment) RAISES {TokenLimit, Wr.Failure} =
    VAR
      str := Text.Sub(c.txt, 2, Text.Length(c.txt)-4);
      len := Text.Length(str);		 (* length of comment string *)
      i := 0;				 (* current index into comment *)

    PROCEDURE SkipWhiteSpace(): BOOLEAN =
    (* Returns TRUE iff the whitespace should be interpretted as starting a
       new paragraph. *)
      VAR c: CHAR; newlines := 0; BEGIN
	WHILE i < len DO
	  c := Text.GetChar(str, i);
          IF c = '\n' THEN INC(newlines)
	  ELSIF c # ' ' AND c # '\t' THEN EXIT
          END;
	  INC(i)
	END;
        RETURN newlines > 1
      END SkipWhiteSpace;

    PROCEDURE SkipWord() RAISES {Wr.Failure} =
      VAR c: CHAR; init := i; BEGIN
        WHILE i < len DO
          c := Text.GetChar(str, i);
          IF c = ' ' OR c = '\n' OR c = '\t' THEN EXIT END;
          INC(i)
        END;
	Print(Text.Sub(str, init, i-init))
      END SkipWord;

    PROCEDURE SkipVerbatim(newP: BOOLEAN) RAISES {Wr.Failure} =
      VAR init: INTEGER; BEGIN
        IF newP THEN
          Formatter.NewLine(fmt, offset := 0, freshLine := FALSE)
        END;
        REPEAT
          Formatter.NewLine(fmt, offset := -3, freshLine := FALSE);
          init := i;
          i := Text.FindChar(str, '\n', start := init+1);
          IF i = -1 THEN i := len END;
          Print(Text.Sub(str, init, i-init));
          INC(i)
        UNTIL i >= len OR Text.GetChar(str, i) # '|';
        DEC(i);
        IF i < len THEN
          Formatter.NewLine(fmt, offset := 0, freshLine := FALSE);
          IF SkipWhiteSpace() THEN
            Formatter.NewLine(fmt, offset := 0, freshLine := FALSE)
          END
        END
      END SkipVerbatim;

    (* Comment *)
    VAR newP: BOOLEAN; BEGIN
      Begin(indent := 0);
      (* Print open-comment characters *)
      Token(Text.Sub(c.txt, 0, 2));
      Print(" ");
      (* Indent the body of the comment in its own group *)
      Begin(indent := 0);
      newP := SkipWhiteSpace();
      WHILE i < len DO
        IF Text.GetChar(str, i) = '|' AND i > 0
           AND Text.GetChar(str, i-1) = '\n' THEN
          (* start of verbatim lines *)
          SkipVerbatim(newP)
        ELSIF newP THEN
          (* paragraph break -- leave a blank line *)
          Formatter.NewLine(fmt, offset := 0, freshLine := FALSE);
          Formatter.NewLine(fmt, offset := 0, freshLine := FALSE)
        ELSE
          (* word break *)
          Break(indent := 0);
        END;
        SkipWord();
        Print(" ");
        newP := SkipWhiteSpace();
      END;
      End();
      (* Print close-comment characters *)
      Break(indent := 0);
      Print(Text.Sub(c.txt, Text.Length(c.txt)-2, 2));
      End()
    END Comment;

  PROCEDURE UIDecl(ast: AST.UIDecl) RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      Token("UI ");
      CheckStart(ast);
      Begin();
      Id(ast.name);
      Token("(");
      ExprList(ast.args);
      Token(")");
      End();
      CheckFinish(ast)
    END UIDecl;

  PROCEDURE Decl(ast: AST.Decl) RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      IF ast.private THEN
        Token("PRIVATE ")
      END;
      TYPECASE ast OF <* NOWARN *>
      | AST.ConstDecl (cd) =>
	  Token("CONST ");
          UnitedBreak(indent := IdIndent);
          Begin();
          ConstDeclItem(cd.head);
          VAR cnt := cd.size - 1; curr := cd.head.next; BEGIN
            WHILE cnt # 0 DO
              OpL2(",", united := TRUE);
              ConstDeclItem(curr);
              DEC(cnt);
              curr := curr.next
            END
          END;
          End()
      | AST.VarDecl (vd) =>
          Token("VAR ");
          UnitedBreak(indent := IdIndent);
          Begin();
          VarDeclItem(vd.head);
          VAR cnt := vd.size - 1; curr := vd.head.next; BEGIN
            WHILE cnt # 0 DO
              OpL2(",", united := TRUE);
              VarDeclItem(curr);
              DEC(cnt);
              curr := curr.next
            END
          END;
          End()
      | AST.PredDecl (pd) =>
	  Token("PRED ");
          PredHeader(pd.header);
          IF private THEN
            Token(" IS ");
            UnitedBreak(PredIndent);
            Expr(pd.body, TYPECODE(NULL));
            Print(" ");
            UnitedBreak(0);
	    Token("END")
          END
      | AST.FuncDecl (fd) =>
	  Token("FUNC ");
          FuncHeader(fd.header);
          IF private THEN
            Token(" IS ");
            UnitedBreak(PredIndent);
            Expr(fd.body, TYPECODE(NULL));
            Print(" ");
            UnitedBreak(0);
	    Token("END")
          END
      | AST.ProcDecl (pd) =>
	  Token("PROC ");
          ProcHeader(pd.header);
          IF private THEN
            Token(" IS ");
            UnitedBreak(CmdIndent);
            Cmd(pd.body, TYPECODE(NULL));
            Print(" ");
            UnitedBreak(0);
	    Token("END")
          END
      END
    END Decl;

  PROCEDURE ConstDeclItem(cdi: AST.ConstDeclItem)
      RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      Begin();
      Id(cdi.name);
      Token(" = ");
      Expr(cdi.value, TYPECODE(NULL));
      End();
    END ConstDeclItem;

  PROCEDURE VarDeclItem(vdi: AST.VarDeclItem)
      RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      Begin();
      Id(vdi.name);
      IF vdi.value # AST.NilExpr THEN
        Token(" := ");
        Expr(vdi.value, TYPECODE(NULL))
      END;
      End();
    END VarDeclItem;

  PROCEDURE PredHeader(header: AST.PredHeader)
      RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      CheckStart(header);
      Break(2);
      Begin();
      Id(header.name);
      Token("(");
      IdList(header.ins);
      Token(")");
      End();
      CheckFinish(header)
    END PredHeader;

  PROCEDURE FuncHeader(header: AST.FuncHeader)
      RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      CheckStart(header);
      Break(2);
      Begin();
      Id(header.result);
      Print(" ");
      Token("=");
      Print(" ");
      Break(4);
      Id(header.name);
      Token("(");
      IdList(header.ins);
      Token(")");
      End();
      CheckFinish(header)
    END FuncHeader;

  PROCEDURE ProcHeader(header: AST.ProcHeader)
      RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      CheckStart(header);
      Break(2);
      Begin();
      IF header.outs # NIL AND header.outs.size # 0 THEN
        Begin();
        IdList(header.outs); Op3(":=")
      END;
      IF header.inouts # NIL AND header.inouts.size # 0 THEN
        IF header.inout_prens THEN Token("(") END;
        IdList(header.inouts);
        IF header.inout_prens THEN Token(")") END;
        Token(":")
      END;
      Id(header.name);
      Token("(");
      IdList(header.ins);
      Token(")");
      IF header.outs # NIL AND header.outs.size # 0 THEN
        End()
      END;
      End();
      CheckFinish(header)
    END ProcHeader;

  PROCEDURE Cmd(ast: AST.Cmd; typeCode: CARDINAL)
      RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      CheckStart(ast);
      IF TYPECODE(ast) # typeCode THEN Begin() END;
      TYPECASE ast OF
        AST.Skip => Token("SKIP")
      | AST.Abort => Token("ABORT")
      | AST.Halt => Token("HALT")
      | AST.Fail => Token("FAIL")
      | AST.Assign (a) =>
          Begin();
          ExprList(a.vars);
          Op3(":=");
          ExprList(a.exprs);
          End()
      | AST.ProcCall (pc) =>
          Begin(indent := 2);
          IF pc.outs.size # 0 THEN
            ExprList(pc.outs);
            Op2(":=")
          END;
          IF pc.inouts.size # 0 THEN
            IF (pc.inout_parens) THEN Token("(") END;
            ExprList(pc.inouts);
            IF (pc.inout_parens) THEN Token(")") END;
            Token(":")
          END;
          QId(pc.name);
          Token("(");
          ExprList(pc.ins);
          Token(")");
          End()
      | AST.If (ifCmd) =>
          Token("IF");
          Print(" ");
          UnitedBreak(indent := 2);
          Cmd(ifCmd.body, TYPECODE(NULL));
          Print(" ");
          UnitedBreak(indent := 0);
          Token("FI");
      | AST.Do (doCmd) =>
          Token("DO");
          Print(" ");
          UnitedBreak(indent := 2);
          Cmd(doCmd.body, TYPECODE(NULL));
          Print(" ");
          UnitedBreak(indent := 0);
          Token("OD");
      | AST.Flip (flip) =>
	  Print("FLIP(");
	  Cmd(flip.body, TYPECODE(NULL));
	  Print(")")
      | AST.Safe (safe) =>
	  Print("SAFE(");
          VAR cmd := safe.body; BEGIN
            LOOP
              TYPECASE cmd OF
                AST.Safe (s) => cmd := s.body
              ELSE EXIT
              END
            END;
	    Cmd(cmd, TYPECODE(NULL))
          END;
	  Print(")")
      | AST.Save (s) =>
          Token("SAVE ");
          QId(s.nm);
          Op("IN");
	  UnitedBreak(2);
          Cmd(s.body, TYPECODE(NULL));
          Print(" ");
	  UnitedBreak(0);
          Token("END")
      | AST.Proj (p) =>
          Begin(indent := 0);
            Token("VAR ");
	    UnitedBreak(2);
	    NearVarList(p.vars);
	    Print(" ");
	    UnitedBreak(0);
	  End();
	  Token("IN ");
	  UnitedBreak(2);
	  Cmd(p.body, TYPECODE(NULL));
	  Print(" ");
	  UnitedBreak(0);
          Token("END")
      | AST.Seq (seq) =>
          Cmd(seq.c1, TYPECODE(seq));
          OpL2(";", united := TRUE);
          Cmd(seq.c2, TYPECODE(seq))
      | AST.Guard (grd) =>
          Begin();
          Expr(grd.grd, TYPECODE(grd));
          Op3("->");
          Cmd(grd.body, TYPECODE(grd));
          End()
      | AST.Else (e) =>
          Cmd(e.c1, TYPECODE(e));
          Op("|");
          Cmd(e.c2, TYPECODE(e))
      | AST.Query (q) =>
	  Print("(");
	  Expr(q.f, TYPECODE(q));
	  Print(")");
          Op("?");
          Print("(");
          IdList2(q.vars);
          Print(")")
      | AST.ConjQuery (cq) =>
	  Print("(");
	  Conj(cq.conj^);
	  Print(")");
          Op("?");
          Print("(");
          Vars(cq.var^);
          Print(")")
      | AST.GroupedCmd (gc) =>
          Token("{");
	  Begin();
          Print(" ");
          Cmd(gc.body, TYPECODE(gc));
          Print(" ");
          UnitedBreak();
	  End();
          Token("}")
      ELSE
        Token("<UNRECOGNIZED CMD>")
      END;
      IF TYPECODE(ast) # typeCode THEN End() END;
      CheckFinish(ast);
    END Cmd;

  PROCEDURE QId(qid: AST.QId) RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      CheckStart(qid);
      IF qid.id0 # AST.NilId THEN Id(qid.id0); Token(".") END;
      Id(qid.id1);
      IF debug AND qid.type # AST.IdType.None THEN
        Print("["); Print(Fmt.Int(qid.index)); Print("]")
      END;
      CheckFinish(qid);
    END QId;

  PROCEDURE Id(a: Atom.T) RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      Check();
      Print(Atom.ToText(a));
      INC(count)
    END Id;


  PROCEDURE NearVarList(nvl: AST.NearVarList)
    RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      CheckStart(nvl);
      VAR curr := nvl.head; BEGIN
        Begin();
        WHILE curr # NIL DO
          NearVarLink(curr);
          IF curr.next # NIL THEN OpL2(",", united := FALSE) END;
          curr := curr.next
        END;
        End()
      END;
      CheckFinish(nvl);
    END NearVarList;

  PROCEDURE NearVarLink(nv: AST.NearVarLink) RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      Begin();
      Id(nv.id);
      IF debug AND nv.index # 0 THEN
        Print("["); Print(Fmt.Int(nv.index)); Print("]")
      END;
      IF nv.hint # AST.NilExpr THEN
        IF nv.frozen
          THEN Op("=")
          ELSE Op("~")
        END;
        Expr(nv.hint, TYPECODE(NULL))
      END;
      End()
    END NearVarLink;

  PROCEDURE IdList(ids: AST.IdList) RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      CheckStart(ids);
      VAR id := ids.head; BEGIN
        Begin(indent := 0);
        WHILE id # NIL DO
          Id(id.id);
          IF debug AND id.index # 0 THEN
            Print("["); Print(Fmt.Int(id.index)); Print("]")
          END;
          IF id.next # NIL THEN OpL2(",") END;
          id := id.next
        END;
        End()
      END;
      CheckFinish(ids);
    END IdList;

  PROCEDURE IdList2(ids: AST.NearVarList) RAISES {TokenLimit, Wr.Failure} =
  (* Unparse a near-var list as if it were an id-list. Don't unparse hints, if
     any, but do print annotations to indicate which variables are hinted and
     which are E-quantified. *)
    BEGIN
      CheckStart(ids);
      VAR id := ids.head; BEGIN
        Begin(indent := 0);
        WHILE id # NIL DO
          IF id.evar THEN Print("<") END;
          IF id.frozen THEN Print("~") END;
          Id(id.id);
          IF debug AND id.index # 0 THEN
            Print("["); Print(Fmt.Int(id.index)); Print("]")
          END;
          IF id.evar THEN Print(">") END;
          IF id.next # NIL THEN OpL2(",") END;
          id := id.next
        END;
        End()
      END;
      CheckFinish(ids);
    END IdList2;

  PROCEDURE Vars(READONLY vars: AST.Vars; unquantified := TRUE)
      RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      Begin(indent := 0);
      FOR i := FIRST(vars) TO LAST(vars) DO
        IF unquantified AND vars[i].evar THEN Print("<") END;
        IF vars[i].frozen THEN Print("~") END;
        Id(vars[i].id);
        IF debug AND vars[i].index # 0 THEN
          Print("["); Print(Fmt.Int(vars[i].index)); Print("]")
        END;
        IF unquantified AND vars[i].evar THEN Print(">") END;
        IF i < LAST(vars) THEN OpL2(",") END
      END;
      End()
    END Vars;

  PROCEDURE Conj(READONLY conj: AST.Formulas)
      RAISES {TokenLimit, Wr.Failure} =
  (* Unparse a conjunction of formulas. We must be careful to parenthesize OR
     nodes, since OR has weaker binding power than AND. *)
    VAR expr: AST.Expr; BEGIN
      Begin();
      IF NUMBER(conj) = 0 THEN
        Print("TRUE")
      ELSE
	FOR i := FIRST(conj) TO LAST(conj) DO
          IF FIRST(conj) < LAST(conj) AND ISTYPE(conj[i], AST.Or)
            THEN expr := NEW(AST.GroupedExpr, expr := conj[i])
            ELSE expr := conj[i]
          END;
	  Expr(expr, TYPECODE(NULL));
	  IF i < LAST(conj) THEN Op2("AND", united := TRUE) END
        END
      END;
      End()
    END Conj;

  PROCEDURE ExprList(exprs: AST.ExprList)
      RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      CheckStart(exprs);
      VAR e := exprs.head; BEGIN
        Begin(indent := 0);
        WHILE e # NIL DO
          Expr(e.expr, TYPECODE(NULL));
          IF e.next # NIL THEN OpL2(",") END;
          e := e.next
        END;
        End()
      END;
      CheckFinish(exprs);
    END ExprList;

  PROCEDURE Expr(expr: AST.Expr; typeCode: CARDINAL)
      RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      CheckStart(expr);
      IF typeCode # TYPECODE(expr) THEN Begin() END;
      TYPECASE expr OF
        AST.Call (c) =>
          Begin(indent := 2);
          IF c.inouts.size # 0 THEN
            IF (c.inout_parens) THEN Token("(") END;
            ExprList(c.inouts);
            IF (c.inout_parens) THEN Token(")") END;
            Token(":")
          END;
          QId(c.name);
          Token("(");
          ExprList(c.ins);
          Token(")");
          End()
      | AST.True => Token("TRUE")
      | AST.False => Token("FALSE")
      | AST.Number (n) => Value(RefReal(n.val))
      | AST.Text (t) => Value(t.val)
      | AST.QId (qid) => QId(qid)
      | AST.Nil => Token("NIL")
      | AST.And (f) =>
          Expr(f.f1, TYPECODE(f));
          Op2("AND", united := TRUE);
          Expr(f.f2, TYPECODE(f))
      | AST.Or (f) =>
          Expr(f.f1, TYPECODE(f));
          Op("OR", united := TRUE);
          Expr(f.f2, TYPECODE(f))
      | AST.Not (f) =>
          Token("NOT ");
          Expr(f.f, TYPECODE(f))
      | AST.Exists (f) =>
          Token("(");
          Token("E ");
          NearVarList(f.vars);
          Print(" ");
          Token("::");
          Print(" ");
	  UnitedBreak(2);
          Expr(f.f, TYPECODE(f));
          Token(")")
      | IndexedNF.T (inf) =>
          NormalForm(SUBARRAY(inf.var^,  0, inf.v_cnt),
                     SUBARRAY(inf.conj^, 0, inf.c_cnt))
      | AST.NormalForm (nf) =>
          NormalForm(nf.var^, nf.conj^)
      | AST.BIUPred (p) => BIUPred(p)
      | AST.Relation (r) => Relation(r)
      | AST.UMinus (e) => Token("-"); Expr(e.e, TYPECODE(e))
      | AST.BIUFunc (f) => BIUFunc(f)
      | AST.BuiltInAddFunc (e) =>
          Expr(e.e1, TYPECODE(e));
          TYPECASE e OF
            AST.Plus => Op2("+")
          | AST.Minus => Op2("-")
          | AST.Concat => Op2("&")
          ELSE
            Token("<UNRECOGNIZED ADDFUNC>")
          END;
          Expr(e.e2, TYPECODE(e))
      | AST.BuiltInMulFunc (e) =>
          Expr(e.e1, TYPECODE(e));
          TYPECASE e OF
            AST.Times => Op2("*")
          | AST.Divide => Op2("/")
          | AST.Div => Op2("DIV")
          | AST.Mod => Op2("MOD")
          ELSE
            Token("<UNRECOGNIZED MULFUNC>")
          END;
          Expr(e.e2, TYPECODE(e))
      | AST.Pair (e) =>
          Token("("); Begin();
          Expr(e.e1, TYPECODE(e));
          OpL2(",");
          Expr(e.e2, TYPECODE(e));
          Token(")"); End()
      | AST.Rel (e) =>
          Expr(e.e1, TYPECODE(e));
          Op2("REL");
          Expr(e.e2, TYPECODE(e))
      | AST.BIBFunc(e) =>	 (* MAX, MIN, ATAN only *)
          TYPECASE e OF <* NOWARN *>
            AST.Max => Token("MAX")
          | AST.Min => Token("MIN")
          | AST.Atan => Token("ATAN")
          END;
          Token("(");
          Expr(e.e1, TYPECODE(e));
          OpL2(",");
          Expr(e.e2, TYPECODE(e));
          Token(")");
      | AST.List (e) =>
          Token("[");
          ExprList(e.elts);
          Token("]")
      | AST.GroupedExpr (e) =>
          Token("(");
          Expr(e.expr, TYPECODE(e));
          Break(1);
          Token(")")
      ELSE
        Token("<UNIMPLEMENTED EXPRESSION FORM>")
      END;
      IF typeCode # TYPECODE(expr) THEN End() END;
      CheckFinish(expr);
    END Expr;

  PROCEDURE BIUPred(up: AST.BIUPred)
      RAISES {TokenLimit, Wr.Failure} =
    VAR name: TEXT; BEGIN
      CheckStart(up);
      TYPECASE up OF <* NOWARN *>
        AST.IsReal => name := "REAL"
      | AST.IsText => name := "TEXT"
      | AST.IsPair => name := "PAIR"
      | AST.IsInt  => name := "INT"
      END;
      Token(name);
      Token("(");
      Expr(up.e, TYPECODE(up));
      Token(")");
      CheckFinish(up);
    END BIUPred;

  PROCEDURE Relation(r: AST.Relation)
      RAISES {TokenLimit, Wr.Failure} =
    VAR name: TEXT; BEGIN
      CheckStart(r);
      TYPECASE (r) OF
        AST.Equals(e) => IF e.near THEN name := "~" ELSE name := "=" END
      | AST.Differs => name := "#"
      | AST.Less    => name := "<"
      | AST.Greater => name := ">"
      | AST.AtMost  => name := "<="
      | AST.AtLeast => name := ">="
      | AST.Cong    => name := "CONG"
      | AST.Para    => name := "PARA"
      | AST.Hor     => name := "HOR"
      | AST.Ver     => name := "VER"
      ELSE
        name := "<UNIMPLEMENTED RELATION>"
      END;
      Begin();
      Expr(r.e1, TYPECODE(r));
      Op3(name);
      Expr(r.e2, TYPECODE(r));
      End();
      CheckFinish(r);
    END Relation;

  PROCEDURE BIUFunc(uf: AST.BIUFunc)
      RAISES {TokenLimit, Wr.Failure} =
  (* NOTE: This procedure does *not* handle the case where "uf" is a
     AST.UMinus. *)
    VAR name: TEXT; BEGIN
      CheckStart(uf);
      TYPECASE uf OF <* NOWARN *>
        AST.Floor   => name := "FLOOR"
      | AST.Ceiling => name := "CEILING"
      | AST.Round   => name := "ROUND"
      | AST.Abs     => name := "ABS"
      | AST.Sin     => name := "SIN"
      | AST.Cos     => name := "COS"
      | AST.Exp     => name := "EXP"
      | AST.Ln      => name := "LN"
      | AST.Car     => name := "CAR"
      | AST.Cdr     => name := "CDR"
      END;
      Token(name);
      Token("(");
      Expr(uf.e, TYPECODE(uf));
      Token(")");
      CheckFinish(uf)
    END BIUFunc;

  PROCEDURE NormalForm(READONLY vars: AST.Vars; READONLY conj: AST.Formulas)
      RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      IF NUMBER(vars) = 0 THEN
        Conj(conj)
      ELSE
        Token("("); Token("E ");
        Vars(vars, unquantified := FALSE);
        Op("::");
        UnitedBreak(2);
        Conj(conj);
        Token(")")
      END
    END NormalForm;

  PROCEDURE CheckStart(ast: AST.T) RAISES {TokenLimit, Wr.Failure} =
  (* Check to see if "ast" is the start of "errast"; if so, write '\001' to
     the output stream and set "foundStart". *)
    BEGIN
      Check();
      IF ast = errast AND NOT foundStart AND ast # NIL THEN
        Print("\001");
        foundStart := TRUE
      END
    END CheckStart;

  PROCEDURE CheckFinish(ast: AST.T) RAISES {Wr.Failure} =
    BEGIN
      IF ast = errast AND NOT foundFinish AND ast # NIL THEN
        Print("\002");
        foundFinish := TRUE
      END
    END CheckFinish;

  PROCEDURE Check() RAISES {TokenLimit} =
    BEGIN
      <* ASSERT count <= tokens *>
      IF count = tokens THEN RAISE TokenLimit END
    END Check;

  PROCEDURE Token(t: TEXT) RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      Check();
      Print(t);
      INC(count)
    END Token;

  PROCEDURE Value(v: JunoValue.T) RAISES {TokenLimit, Wr.Failure} =
    BEGIN
      Check();
      JunoValue.UnparseToFmt(fmt, v, prec);
      INC(count)
    END Value;

  PROCEDURE Print(t: TEXT) RAISES {Wr.Failure} =
    BEGIN
      Formatter.PutText(fmt, t, raw := TRUE)
    END Print;

  <* INLINE *>
  PROCEDURE OpNoSpaces(t: TEXT; united := FALSE)
      RAISES {TokenLimit, Wr.Failure} =
    VAR len := - (Text.Length(t) + 1); BEGIN
      IF united
        THEN UnitedBreak(indent := len)
        ELSE Break(indent := len)
      END;
      Token(t);
    END OpNoSpaces;

  PROCEDURE Op(t: TEXT; united := FALSE) RAISES {TokenLimit, Wr.Failure} =
  (* Introduces an ordinary operation into the stream, with spaces on either
     side.  A break is introduced before the op so that the resulting output
     looks like 
|         A
|      op B
     if the operation is broken or
|         A op B
     if it is not. If "united = TRUE", then all siblings of the break will
     also break if this one does. *)
    BEGIN
      Print(" ");
      OpNoSpaces(t, united);
      Print(" ")
    END Op;

  PROCEDURE Op2(t: TEXT; united := FALSE) RAISES {TokenLimit, Wr.Failure} =
  (* Like "Op", but puts the op at the end of the first line instead of
    the start of the second. *)
  BEGIN
    Print(" ");
    Token(t);
    Print(" ");
    IF united THEN
      UnitedBreak(indent := 0)
    ELSE
      Break(indent := 0)
    END
  END Op2;

  PROCEDURE Op3(t: TEXT) RAISES {TokenLimit, Wr.Failure} =
  (* Format
|  LeftArg op
|    RightArg
*)
  BEGIN
    Print(" ");
    Token(t);
    Print(" ");
    Break(indent := 2);
  END Op3;

  PROCEDURE OpL2(t: TEXT; united := FALSE) RAISES {TokenLimit, Wr.Failure} =
  (* Like OpL, only the output text is broken to look like this:
|
|    Aop
|    B
  *)
    BEGIN
      Token(t);
      Print(" ");
      IF united
        THEN UnitedBreak(indent := 0)
        ELSE Break(indent := 0)
      END
    END OpL2;

  PROCEDURE Begin(indent: INTEGER := 0) RAISES {Wr.Failure} =
    BEGIN
      Formatter.Begin(fmt, indent)
    END Begin;

  PROCEDURE End() RAISES {Wr.Failure} =
    BEGIN
      Formatter.End(fmt)
    END End;

  PROCEDURE Break(indent: INTEGER := 0) RAISES {Wr.Failure} =
    BEGIN
      (* Formatter.PartialBreak(fmt, offset := indent); *)
      Formatter.Break(fmt, offset := indent,
        type := Formatter.BreakType.NonOptimal)
    END Break;

  PROCEDURE UnitedBreak(indent: INTEGER := 0) RAISES {Wr.Failure} =
    BEGIN
      Formatter.UnitedBreak(fmt, offset := indent)
    END UnitedBreak;

  VAR foundStart, foundFinish := FALSE; BEGIN (* Unparse *)
    IF errast # NIL THEN
      (* find predecessor; crash if predecessor undefined *)
      WHILE errast.bp # AST.End DO errast := errast.bp END
    END;
    FOR i := 1 TO indent DO Print(" ") END;
    Begin();
    TRY
      TYPECASE ast OF
        AST.IdList(l)      => IdList(l)
      | AST.NearVarList(l) => NearVarList(l)
      | AST.ExprList(l)    => ExprList(l)
      | AST.Expr(e)        => Expr(e, TYPECODE(NULL))
      | AST.Cmd(c)         => Cmd(c, TYPECODE(NULL))
      | AST.Block(b)       => Block(b)
      ELSE
        Token("<UNRECOGNIZED AST TYPE>")
      END
    EXCEPT
      TokenLimit => (* SKIP *)
    END;
    End();
    Formatter.Flush(fmt)
  END Unparse;

BEGIN
END JunoUnparse.
