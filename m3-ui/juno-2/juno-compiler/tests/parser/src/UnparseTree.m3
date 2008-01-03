(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last modified on Fri Jun 26 14:44:14 PDT 1992 by heydon                   *)

MODULE UnparseTree;

IMPORT JunoAST, Atom;
IMPORT Wr, Fmt;
FROM Thread IMPORT Alerted;

<* FATAL Wr.Failure, Alerted *>

CONST Tab = 3;				 (* amount to indent each new line *)

PROCEDURE Unit(wr: Wr.T; READONLY ast: JunoAST.Unit; cnt: CARDINAL; in := 0) =
  BEGIN
    Wr.PutText(wr, "Parsed a total of " & Fmt.Int(cnt) & " tokens.\n");
    DeclList(wr, ast, in);
  END Unit;

PROCEDURE Cmd(wr: Wr.T; READONLY ast: JunoAST.Cmd; cnt: CARDINAL; in := 0) =
  BEGIN
    Wr.PutText(wr, "Parsed a total of " & Fmt.Int(cnt) & " tokens.\n");
    Command(wr, ast, in);
  END Cmd;

PROCEDURE Expr(wr: Wr.T; READONLY ast: JunoAST.Expr; cnt: CARDINAL; in := 0) =
  BEGIN
    Wr.PutText(wr, "Parsed a total of " & Fmt.Int(cnt) & " tokens.\n");
    Expression(wr, ast, in);
  END Expr;

PROCEDURE DeclList(wr: Wr.T; READONLY ast: JunoAST.Unit; in := 0) =
  BEGIN
    IF ast = NIL THEN Empty(wr, in) ELSE
      VAR curr := ast.head; BEGIN
        Indent(wr, in); Wr.PutText(wr, "(Unit\n");
        FOR i := 1 TO ast.size DO
          Decl(wr, curr.decl, in + Tab);
          curr := curr.next;
        END;
        Indent(wr, in); Wr.PutText(wr, ")\n");
      END
    END
  END DeclList;

PROCEDURE Decl(wr: Wr.T; READONLY ast: JunoAST.Decl; in := 0) =
  BEGIN
    TYPECASE ast OF <* NOWARN *>
    | NULL => Empty(wr, in);
    | JunoAST.Interface(d) => Interface(wr, d, in)
    | JunoAST.Module(d)    => Module(wr, d, in)
    | JunoAST.Import(d)    => Import(wr, d, in)
    | JunoAST.Comment(d)   => Comment(wr, d, in)
    | JunoAST.ConstDecl(d) => ConstDecl(wr, d, in)
    | JunoAST.VarDecl(d)   => VarDecl(wr, d, in)
    | JunoAST.PredDecl(d)  => PredDecl(wr, d, in)
    | JunoAST.FuncDecl(d)  => FuncDecl(wr, d, in)
    | JunoAST.ProcDecl(d)  => ProcDecl(wr, d, in)
    END
  END Decl;

PROCEDURE Interface(wr: Wr.T; READONLY ast: JunoAST.Interface; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(INTERFACE\n");
    Id(wr, ast.name, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Interface;

PROCEDURE Module(wr: Wr.T; READONLY ast: JunoAST.Module; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(MODULE\n");
    Id(wr, ast.name, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Module;

PROCEDURE Import(wr: Wr.T; READONLY ast: JunoAST.Import; in := 0) =
  BEGIN
    TYPECASE ast OF <* NOWARN *>
    | JunoAST.CompleteImport(d)  => Complete(wr, d, in)
    | JunoAST.SelectiveImport(d) => Selective(wr, d, in)
    END
  END Import;

PROCEDURE Complete(wr: Wr.T; READONLY ast: JunoAST.CompleteImport; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(IMPORT\n");
    IdList(wr, ast.idList, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Complete;

PROCEDURE Selective(wr: Wr.T; READONLY ast: JunoAST.SelectiveImport; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(FROM\n");
    Id(wr, ast.id, in + Tab);
    IdList(wr, ast.idList, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Selective;

PROCEDURE Comment(wr: Wr.T; READONLY ast: JunoAST.Comment; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(Comment\n");
    Txt(wr, ast.txt, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Comment;

PROCEDURE ConstDecl(wr: Wr.T; READONLY ast: JunoAST.ConstDecl; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(CONST\n");
    Id(wr, ast.name, in + Tab);
    Expression(wr, ast.value, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END ConstDecl;

PROCEDURE VarDecl(wr: Wr.T; READONLY ast: JunoAST.VarDecl; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(VAR\n");
    IdList(wr, ast.names, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END VarDecl;

PROCEDURE PredDecl(wr: Wr.T; READONLY ast: JunoAST.PredDecl; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(PREDICATE\n");
    Id(wr, ast.name, in + Tab);
    IdList(wr, ast.ins, in + Tab);
    IF ast.body # JunoAST.NilExpr THEN Expression(wr, ast.body, in + Tab) END;
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END PredDecl;

PROCEDURE FuncDecl(wr: Wr.T; READONLY ast: JunoAST.FuncDecl; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(FUNCTION\n");
    Id(wr, ast.name, in + Tab);
    IdList(wr, ast.ins, in + Tab);
    Id(wr, ast.result, in + Tab);
    IF ast.body # JunoAST.NilExpr THEN Expression(wr, ast.body, in + Tab) END;
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END FuncDecl;

PROCEDURE ProcDecl(wr: Wr.T; READONLY ast: JunoAST.ProcDecl; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(PROCEDURE\n");
    IdList(wr, ast.outs, in + Tab);
    IdList(wr, ast.inouts, in + Tab);
    Id(wr, ast.name, in + Tab);
    IdList(wr, ast.ins, in + Tab);
    IF ast.body # JunoAST.NilCmd THEN Command(wr, ast.body, in + Tab) END;
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END ProcDecl;

PROCEDURE Command(wr: Wr.T; READONLY ast: JunoAST.Cmd; in := 0) =
  BEGIN
    TYPECASE ast OF <* NOWARN *>
    | NULL => Empty(wr, in);
    | JunoAST.Skip          => Skip(wr, in);
    | JunoAST.Abort         => Abort(wr, in);
    | JunoAST.Assign(c)     => Assign(wr, c, in);
    | JunoAST.ProcCall(c)   => ProcCall(wr, c, in);
    | JunoAST.If(c)         => If(wr, c, in);
    | JunoAST.Do(c)         => Do(wr, c, in);
    | JunoAST.Save(c)       => Save(wr, c, in);
    | JunoAST.Proj(c)       => Proj(wr, c, in);
    | JunoAST.Seq(c)        => Seq(wr, c, in);
    | JunoAST.Guard(c)      => Guard(wr, c, in);
    | JunoAST.Else(c)       => Else(wr, c, in);
    | JunoAST.GroupedCmd(c) => GroupedCmd(wr, c, in);
    END
  END Command;

PROCEDURE Skip(wr: Wr.T; in := 0) =
  BEGIN Indent(wr, in); Wr.PutText(wr, "SKIP\n"); END Skip;

PROCEDURE Abort(wr: Wr.T; in := 0) =
  BEGIN Indent(wr, in); Wr.PutText(wr, "ABORT\n"); END Abort;

PROCEDURE Assign(wr: Wr.T; READONLY ast: JunoAST.Assign; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(:=\n");
    ExprList(wr, ast.vars, in + Tab);
    ExprList(wr, ast.exprs, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Assign;

PROCEDURE ProcCall(wr: Wr.T; READONLY ast: JunoAST.ProcCall; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(ProcCall\n");
    ExprList(wr, ast.inouts, in + Tab);
    QId(wr, ast.name, in + Tab);
    ExprList(wr, ast.ins, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END ProcCall;

PROCEDURE If(wr: Wr.T; READONLY ast: JunoAST.If; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(IF\n");
    Command(wr, ast.body, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END If;

PROCEDURE Do(wr: Wr.T; READONLY ast: JunoAST.Do; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(DO\n");
    Command(wr, ast.body, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Do;

PROCEDURE Save(wr: Wr.T; READONLY ast: JunoAST.Save; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(SAVE\n");
    Id(wr, ast.id, in + Tab);
    Command(wr, ast.body, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Save;

PROCEDURE Proj(wr: Wr.T; READONLY ast: JunoAST.Proj; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(::\n");
    NearVarList(wr, ast.vars, in + Tab);
    Command(wr, ast.body, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Proj;

PROCEDURE Seq(wr: Wr.T; READONLY ast: JunoAST.Seq; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(;\n");
    Command(wr, ast.c1, in + Tab);
    Command(wr, ast.c2, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Seq;

PROCEDURE Guard(wr: Wr.T; READONLY ast: JunoAST.Guard; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(->\n");
    Expression(wr, ast.grd, in + Tab);
    Command(wr, ast.body, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Guard;

PROCEDURE Else(wr: Wr.T; READONLY ast: JunoAST.Else; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(|\n");
    Command(wr, ast.c1, in + Tab);
    Command(wr, ast.c2, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Else;

PROCEDURE GroupedCmd(wr: Wr.T; READONLY ast: JunoAST.GroupedCmd; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(GroupedCmd\n");
    Command(wr, ast.cmd, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END GroupedCmd;

PROCEDURE Expression(wr: Wr.T; READONLY ast: JunoAST.Expr; in := 0) =
  BEGIN
    TYPECASE ast OF <* NOWARN *>
    | NULL => Empty(wr, in);
    | JunoAST.Call(e) => Call(wr, e, in);
    | JunoAST.GroupedExpr(e) => GroupedExpr(wr, e, in);
    | JunoAST.BuiltIn(e) => BuiltIn(wr, e, in);
    END
  END Expression;

PROCEDURE Call(wr: Wr.T; READONLY ast: JunoAST.Call; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(Call\n");
    ExprList(wr, ast.inouts, in + Tab);
    QId(wr, ast.name, in + Tab);
    ExprList(wr, ast.ins, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Call;

PROCEDURE BuiltIn(wr: Wr.T; READONLY ast: JunoAST.BuiltIn; in := 0) =
  BEGIN
    TYPECASE ast OF <* NOWARN *>
    | NULL => Empty(wr, in);
    | JunoAST.Literal(e) => Literal(wr, e, in)
    | JunoAST.BuiltInPred(e) => BuiltInPred(wr, e, in)
    | JunoAST.BuiltInFunc(e) => BuiltInFunc(wr, e, in)
    END
  END BuiltIn;

PROCEDURE Literal(wr: Wr.T; READONLY ast: JunoAST.Literal; in := 0) =
  BEGIN
    TYPECASE ast OF <* NOWARN *>
    | NULL => Empty(wr, in);
    | JunoAST.LitPred(e) => LitPred(wr, e, in);
    | JunoAST.LitValue(e) => LitValue(wr, e, in);
    END
  END Literal;

PROCEDURE LitPred(wr: Wr.T; READONLY ast: JunoAST.LitPred; in := 0) =
  BEGIN
    TYPECASE ast OF <* NOWARN *>
    | NULL          => Empty(wr, in);
    | JunoAST.True  => True(wr, in)
    | JunoAST.False => False(wr, in)
    END
  END LitPred;

PROCEDURE True(wr: Wr.T; in := 0) =
  BEGIN Indent(wr, in); Wr.PutText(wr, "TRUE\n"); END True;

PROCEDURE False(wr: Wr.T; in := 0) =
  BEGIN Indent(wr, in); Wr.PutText(wr, "FALSE\n"); END False;

PROCEDURE LitValue(wr: Wr.T; READONLY ast: JunoAST.LitValue; in := 0) =
  BEGIN
    TYPECASE ast OF <* NOWARN *>
    | NULL              => Empty(wr, in);
    | JunoAST.Number(e) => Number(wr, e, in);
    | JunoAST.Text(e)   => Text(wr, e, in);
    | JunoAST.QId(e)    => QId(wr, e, in);
    | JunoAST.Nil       => Nil(wr, in);
    END
  END LitValue;

PROCEDURE Number(wr: Wr.T; ast: JunoAST.Number; in: CARDINAL) =
  BEGIN Indent(wr, in); Wr.PutText(wr, Fmt.Real(ast.val^) & "\n") END Number;

PROCEDURE Text(wr: Wr.T; ast: JunoAST.Text; in: CARDINAL) =
  BEGIN Indent(wr, in); Wr.PutText(wr, "\"" & ast.val & "\"\n") END Text;

PROCEDURE QId(wr: Wr.T; ast: JunoAST.QId; in: CARDINAL) =
  BEGIN
    IF ast = NIL THEN Empty(wr, in) ELSE
      Indent(wr, in);
      IF ast.id0 # NIL THEN
        Wr.PutText(wr, Atom.Name(ast.id0));
      ELSE
        Wr.PutText(wr, "EMPTY")
      END;
      IF ast.id1 # JunoAST.NilId THEN
        Wr.PutText(wr, ".");
        IF ast.id1 # NIL THEN
          Wr.PutText(wr, Atom.Name(ast.id1));
        ELSE
          Wr.PutText(wr, "EMPTY")
        END;
      END;
      Wr.PutChar(wr, '\n');
    END;
  END QId;

PROCEDURE Nil(wr: Wr.T; in: CARDINAL) =
  BEGIN Indent(wr, in); Wr.PutText(wr, "NIL\n") END Nil;

PROCEDURE BuiltInPred(wr: Wr.T; READONLY ast: JunoAST.BuiltInPred; in := 0) =
  BEGIN
    TYPECASE ast OF <* NOWARN *>
    | NULL => Empty(wr, in);
    | JunoAST.And(e) => And(wr, e, in);
    | JunoAST.Or(e) => Or(wr, e, in);
    | JunoAST.Not(e) => Not(wr, e, in);
    | JunoAST.Exists(e) => Exists(wr, e, in);
    | JunoAST.Relation(e) => Relation(wr, e, in);
    END
  END BuiltInPred;

PROCEDURE And(wr: Wr.T; ast: JunoAST.And; in: CARDINAL) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(AND\n");
    Expression(wr, ast.f1, in + Tab);
    Expression(wr, ast.f2, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END And;

PROCEDURE Or(wr: Wr.T; ast: JunoAST.Or; in: CARDINAL) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(OR\n");
    Expression(wr, ast.f1, in + Tab);
    Expression(wr, ast.f2, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Or;

PROCEDURE Not(wr: Wr.T; ast: JunoAST.Not; in: CARDINAL) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(NOT\n");
    Expression(wr, ast.f, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Not;

PROCEDURE Exists(wr: Wr.T; ast: JunoAST.Exists; in: CARDINAL) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(E\n");
    IdList(wr, ast.vars, in + Tab);
    Expression(wr, ast.f, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Exists;

PROCEDURE Relation(wr: Wr.T; ast: JunoAST.Relation; in: CARDINAL) =
  BEGIN
    Indent(wr, in); Wr.PutChar(wr, '(');
    TYPECASE ast OF <* NOWARN *>
    | JunoAST.Equals  => Wr.PutText(wr, "=\n");
    | JunoAST.Differs => Wr.PutText(wr, "#\n");
    | JunoAST.Less    => Wr.PutText(wr, "<\n");
    | JunoAST.Greater => Wr.PutText(wr, ">\n");
    | JunoAST.AtMost  => Wr.PutText(wr, "<=\n");
    | JunoAST.AtLeast => Wr.PutText(wr, ">=\n");
    | JunoAST.Cong    => Wr.PutText(wr, "CONG\n");
    | JunoAST.Para    => Wr.PutText(wr, "PARA\n");
    | JunoAST.Near    => Wr.PutText(wr, "NEAR\n");
    END;
    Expression(wr, ast.e1, in + Tab);
    Expression(wr, ast.e2, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Relation;

PROCEDURE BuiltInFunc(wr: Wr.T; READONLY ast: JunoAST.BuiltInFunc; in := 0) =
  BEGIN
    TYPECASE ast OF <* NOWARN *>
    | NULL => Empty(wr, in);
    | JunoAST.BuiltInAddFunc(e) => BuiltInAddFunc(wr, e, in);
    | JunoAST.BuiltInMulFunc(e) => BuiltInMulFunc(wr, e, in);
    | JunoAST.Pair(e)           => Pair(wr, e, in);
    | JunoAST.List(e)           => List(wr, e, in);
    | JunoAST.Rel(e)            => Rel(wr, e, in);
    | JunoAST.UMinus(e)         => UMinus(wr, e, in);
    END
  END BuiltInFunc;

PROCEDURE BuiltInAddFunc(wr: Wr.T; ast: JunoAST.BuiltInAddFunc; in: CARDINAL) =
  BEGIN
    Indent(wr, in); Wr.PutChar(wr, '(');
    TYPECASE ast OF <* NOWARN *>
    | JunoAST.Plus   => Wr.PutText(wr, "+\n");
    | JunoAST.Minus  => Wr.PutText(wr, "-\n");
    | JunoAST.Concat => Wr.PutText(wr, "&\n");
    END;
    Expression(wr, ast.e1, in + Tab);
    Expression(wr, ast.e2, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END BuiltInAddFunc;

PROCEDURE BuiltInMulFunc(wr: Wr.T; ast: JunoAST.BuiltInMulFunc; in: CARDINAL) =
  BEGIN
    Indent(wr, in); Wr.PutChar(wr, '(');
    TYPECASE ast OF <* NOWARN *>
    | JunoAST.Times  => Wr.PutText(wr, "*\n");
    | JunoAST.Divide => Wr.PutText(wr, "/\n");
    | JunoAST.Div    => Wr.PutText(wr, "DIV\n");
    | JunoAST.Mod    => Wr.PutText(wr, "MOD\n");
    END;
    Expression(wr, ast.e1, in + Tab);
    Expression(wr, ast.e2, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END BuiltInMulFunc;

PROCEDURE Pair(wr: Wr.T; READONLY ast: JunoAST.Pair; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(Pair\n");
    Expression(wr, ast.e1, in + Tab);
    Expression(wr, ast.e2, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Pair;

PROCEDURE List(wr: Wr.T; READONLY ast: JunoAST.List; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "([]\n");
    ExprList(wr, ast.elts, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END List;

PROCEDURE Rel(wr: Wr.T; ast: JunoAST.Rel; in: CARDINAL) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(REL\n");
    Expression(wr, ast.e1, in + Tab);
    Expression(wr, ast.e2, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END Rel;

PROCEDURE UMinus(wr: Wr.T; ast: JunoAST.UMinus; in: CARDINAL) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(U-\n");
    Expression(wr, ast.e, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END UMinus;

PROCEDURE GroupedExpr(wr: Wr.T; READONLY ast: JunoAST.GroupedExpr; in := 0) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "(GroupedExpr\n");
    Expression(wr, ast.expr, in + Tab);
    Indent(wr, in); Wr.PutText(wr, ")\n");
  END GroupedExpr;

PROCEDURE IdList(wr: Wr.T; ast: JunoAST.IdList; in: CARDINAL) =
  BEGIN
    IF ast = NIL THEN Empty(wr, in) ELSE
      VAR curr := ast.head; BEGIN
        Indent(wr, in); Wr.PutText(wr, "(IdList\n");
        FOR i := 1 TO ast.size DO
          Id(wr, curr.id, in + Tab);
          curr := curr.next;
        END;
        Indent(wr, in); Wr.PutText(wr, ")\n");
      END
    END
  END IdList;

PROCEDURE ExprList(wr: Wr.T; ast: JunoAST.ExprList; in: CARDINAL) =
  BEGIN
    IF ast = NIL THEN Empty(wr, in) ELSE
      VAR curr := ast.head; BEGIN
        Indent(wr, in); Wr.PutText(wr, "(ExprList\n");
        FOR i := 1 TO ast.size DO
          Expression(wr, curr.expr, in + Tab);
          curr := curr.next;
        END;
        Indent(wr, in); Wr.PutText(wr, ")\n");
      END
    END
  END ExprList;

PROCEDURE NearVarList(wr: Wr.T; ast: JunoAST.NearVarList; in: CARDINAL) =
  BEGIN
    IF ast = NIL THEN Empty(wr, in) ELSE
      VAR curr := ast.head; BEGIN
        Indent(wr, in); Wr.PutText(wr, "(NearVarList\n");
        FOR i := 1 TO ast.size DO
          NearVar(wr, curr.nv, in + Tab);
          curr := curr.next;
        END;
        Indent(wr, in); Wr.PutText(wr, ")\n");
      END
    END
  END NearVarList;

PROCEDURE NearVar(wr: Wr.T; ast: JunoAST.NearVar; in: CARDINAL) =
  BEGIN
    IF ast = NIL THEN Empty(wr, in) ELSE
      Indent(wr, in); Wr.PutText(wr, "(~\n");
      Id(wr, ast.var, in + Tab);
      IF ast.init # JunoAST.NilExpr THEN
        Expression(wr, ast.init, in + Tab)
      END;
      Indent(wr, in); Wr.PutText(wr, ")\n");
    END
  END NearVar;

PROCEDURE Id(wr: Wr.T; ast: JunoAST.Id; in: CARDINAL) =
  BEGIN
    IF ast = NIL THEN Empty(wr, in) ELSE
      Indent(wr, in); Wr.PutText(wr, Atom.Name(ast) & "\n")
    END
  END Id;

PROCEDURE Txt(wr: Wr.T; txt: TEXT; in: CARDINAL) =
  BEGIN
    IF txt = NIL THEN Empty(wr, in) ELSE
      Indent(wr, in); Wr.PutText(wr, txt & "\n")
    END
  END Txt;

PROCEDURE Empty(wr: Wr.T; in: CARDINAL) =
  BEGIN
    Indent(wr, in); Wr.PutText(wr, "EMPTY\n");
  END Empty;

PROCEDURE Indent(wr: Wr.T; in: CARDINAL) =
  BEGIN
    FOR i := 1 TO in DO Wr.PutChar(wr, ' '); END;
  END Indent;

BEGIN END UnparseTree.
