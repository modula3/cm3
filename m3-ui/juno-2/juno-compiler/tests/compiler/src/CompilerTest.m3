(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun 26 11:59:00 PDT 1997 by heydon                   *)

(* This program implements a simple test of the front-end of the Juno
   compiler. The command-line syntax of the program is:

       CompilerTest [-disassem] [-bptrs] [infile]

   By default, the program reads Juno commands from standard input. If you
   specify "infile", it will read from that file. If you specify "-disassem",
   then the disassembly of the compiled command is shown; if you specify
   "-bptrs", then all nodes in the compiled result AST that have defined
   predecessors in the input AST are pretty-printed.

   The program compiles commands under a test scope containing constants,
   variables, predicates, functions, and procedures with the following
   signatures. Commands are compiled as if they appeared in Main, so
   references to identifiers appearing in Main must be unqualified.

   MODULE Main;
   VAR v;
   PRED p0() END;
   PRED p1(x) END;
   PRED p2(x,y) END ;
   FUNC res := f0() END;
   FUNC res := f1(x) END;
   FUNC res := f2(x,y) END;
   FUNC c := _MINUS(a, b) IS b + c = a END;
   FUNC b := _CAR(a) IS (E c:: (b, c) = a) END;
   FUNC b := _CDR(a) IS (E c:: (c, b) = a) END;

   MODULE PS;
   VAR LineWidth;
   PRED Point(x) END;
   PROC MoveTo(p) END;
   PROC (p):Show(txt) END;
   PROC pos := CPos() END;
*)

MODULE CompilerTest EXPORTS Main;

IMPORT BackPtr;
IMPORT JunoAST, JunoParse, JunoLex, JunoToken, JunoUnparse, JunoScope;
IMPORT   JunoCompile, JunoCompileErr, JunoCompileRep, JunoChkBNF;
IMPORT   JunoAssemble, StackTbl, JunoASTUtils; 
IMPORT JunoDisassem;
IMPORT Atom, Text, Rd, Wr, TextRd, FileRd, Params, Process, OSError;
FROM Stdio IMPORT stdin, stdout, stderr;
FROM Thread IMPORT Alerted;

<* FATAL Rd.Failure, Wr.Failure, Alerted *>
<* FATAL Rd.EndOfFile *>

EXCEPTION BadFormat;

PROCEDURE BuildTestScope(): JunoScope.T =
  VAR
    top := JunoScope.New(NIL);
    main, ps := JunoScope.New(top);
    main_ent := NEW(JunoScope.Mod, public_scp := main, scp := main);
    ps_ent := NEW(JunoScope.Mod, public_scp := ps, scp := ps);
    p0, p1, p2, point := NEW(JunoScope.Pred, formals := NIL, index := 0);
    f0, f1, f2 := NEW(JunoScope.Func, formals := NIL, index := 0);
    moveto, show, cpos := NEW(JunoScope.Proc, formals := NIL, index := 0);
    v, linewidth := NEW(JunoScope.Var, index := 0);
  VAR
    idA := JunoASTUtils.QIdFromText("a");
    idB := JunoASTUtils.QIdFromText("b");
    idC := JunoASTUtils.QIdFromText("c");
    idListA := NEW(JunoAST.IdList, size := 1,
      head := NEW(JunoAST.IdLink, id := idA.id1));
    idListAB := NEW(JunoAST.IdList, size := 2, head := NEW(JunoAST.IdLink,
      id := idA.id1, next := NEW(JunoAST.IdLink, id := idB.id1)));
    nvListC := NEW(JunoAST.NearVarList, size := 1, head :=
      NEW(JunoAST.NearVarLink, id := idC.id1,
        hint := JunoAST.NilExpr, index := 0));

  PROCEDURE Install(
      name: JunoAST.QId;
      formals: JunoAST.IdList;
      result: JunoAST.QId;
      f: JunoAST.Formula) RAISES {JunoCompileErr.Error} =
    (* Install a function named "name.id1", IN parameters "formals", result
       named "result.id1", and body "f" in the scope "main". *)
    VAR
      header := NEW(JunoAST.FuncHeader, bp := JunoAST.End,
        name := name.id1, ins := formals, result := result.id1);
      decl := NEW(JunoAST.FuncDecl, bp := JunoAST.End,
        header := header, body := f);
      func := JunoScope.NewFunc(decl, mod := JunoAST.NilId);
    BEGIN
      JunoCompile.FuncDecl(name.id1, func, main);
      JunoScope.Bind(main, name.id1, func)
    END Install;

  PROCEDURE InstallEmptyProc(scp: JunoScope.T; nm: Atom.T) =
    (* Bind "nm" in "scp" to a procedure named "nm" with no arguments and
       "SKIP" as its body. *)
    <* FATAL JunoScope.NameClash *>
    VAR procDecl: JunoAST.ProcDecl; proc: JunoScope.Proc; BEGIN
      procDecl := NEW(JunoAST.ProcDecl, bp := JunoAST.End, private := FALSE,
        header := NEW(JunoAST.ProcHeader, bp := JunoAST.End, name := nm,
          ins := JunoAST.EmptyIdList, outs := JunoAST.EmptyIdList,
          inouts := JunoAST.EmptyIdList),
        body := JunoAST.SkipVal);
      proc := JunoScope.NewProc(procDecl, mod := NIL);
      JunoScope.Bind(scp, nm, proc)
    END InstallEmptyProc;

  <* FATAL JunoScope.NameClash, JunoCompileErr.Error *>
  BEGIN
    (* Install entities for CLOSE and APPLY *)
    InstallEmptyProc(top, Atom.FromText("CLOSE"));
    InstallEmptyProc(top, Atom.FromText("APPLY"));

    (* Initialize functions and predicates *)
    (* "c = Minus(a, b)" => "b + c = a" *)
    Install(JunoAST.MinusName, idListAB, idC, NEW(JunoAST.Equals,
      e2 := JunoASTUtils.QIdFromText("a"), e1 := NEW(JunoAST.Plus,
        e1 := JunoASTUtils.QIdFromText("b"),
        e2 := JunoASTUtils.QIdFromText("c"))));
    (* "b = Car(a)" => "(E c: (b, c) = a)" *)
    Install(JunoAST.CarName, idListA, idB, NEW(JunoAST.Exists, vars := nvListC,
      f := NEW(JunoAST.Equals, e2 := JunoASTUtils.QIdFromText("a"),
        e1 := NEW(JunoAST.Pair, e1 := JunoASTUtils.QIdFromText("b"),
          e2 := JunoASTUtils.QIdFromText("c")))));
    (* "b = Cdr(a)" => "(E c: (c, b) = a)" *)
    Install(JunoAST.CdrName, idListA, idB, NEW(JunoAST.Exists, vars := nvListC,
      f := NEW(JunoAST.Equals, e2 := JunoASTUtils.QIdFromText("a"),
        e1 := NEW(JunoAST.Pair, e1 := JunoASTUtils.QIdFromText("c"),
          e2 := JunoASTUtils.QIdFromText("b")))));

    (* Initialize entities *)
    p0.in_cnt := 0; p1.in_cnt := 1; p2.in_cnt := 2;
    f0.in_cnt := 0; f1.in_cnt := 1; f2.in_cnt := 2;
    point.in_cnt := 1;
    moveto.in_cnt := 1; moveto.inout_cnt := 0; moveto.out_cnt := 0;
    show.in_cnt := 1; show.inout_cnt := 1; show.out_cnt := 0;
    cpos.in_cnt := 0; cpos.inout_cnt := 0; cpos.out_cnt := 1;

    (* Install entities in scopes *)
    JunoScope.Bind(top, Atom.FromText("Main"), main_ent);
    JunoScope.Bind(top, Atom.FromText("PS"), ps_ent);
    JunoScope.Bind(main, Atom.FromText("p0"), p0);
    JunoScope.Bind(main, Atom.FromText("p1"), p1);
    JunoScope.Bind(main, Atom.FromText("p2"), p2);
    JunoScope.Bind(main, Atom.FromText("f0"), f0);
    JunoScope.Bind(main, Atom.FromText("f1"), f1);
    JunoScope.Bind(main, Atom.FromText("f2"), f2);
    JunoScope.Bind(main, Atom.FromText("v"), v);
    JunoScope.Bind(ps, Atom.FromText("Point"), point);
    JunoScope.Bind(ps, Atom.FromText("MoveTo"), moveto);
    JunoScope.Bind(ps, Atom.FromText("Show"), show);
    JunoScope.Bind(ps, Atom.FromText("CPos"), cpos);
    JunoScope.Bind(ps, Atom.FromText("LineWidth"), linewidth);
    RETURN main;
  END BuildTestScope;

VAR
  cmd: JunoAST.Cmd;			 (* current command *)
  scp: JunoScope.T;			 (* scope for compilations *)
  line: TEXT;				 (* current input line *)
  in := stdin;				 (* input stream *)
  tokenCnt: CARDINAL;			 (* total number of tokens parsed *)
  res: JunoCompileRep.Result;		 (* compilation result *)
  tempCnt: CARDINAL;			 (* number of local variables *)
  rd := NEW(TextRd.T);

CONST
  Usage = "CompilerTest [-disassem] [-bptrs] [infile]";

VAR (* command-line variables *)
  arg := 1;				 (* command-line argument index *)
  disassemble := FALSE;			 (* disassemble? *)
  backpointers := FALSE;		 (* show back pointers; *)

PROCEDURE Dummy(x: INTEGER): INTEGER = BEGIN RETURN x+1 END Dummy;

BEGIN
  (* Parse command-line *)
  TRY
    WHILE arg < Params.Count AND Text.GetChar(Params.Get(arg), 0) = '-' DO
      IF    Text.Equal(Params.Get(arg), "-disassem") THEN disassemble := TRUE
      ELSIF Text.Equal(Params.Get(arg), "-bptrs") THEN backpointers := TRUE
      ELSE RAISE BadFormat
      END;
      INC(arg)
    END;
    IF arg < Params.Count THEN in := FileRd.Open(Params.Get(arg)) END;
  EXCEPT
    BadFormat =>
      Wr.PutText(stderr, "Usage: " & Usage & "\n");
      Process.Exit(1);
  | OSError.E =>
      Wr.PutText(stderr, "Unable to open '"
        & Params.Get(1) & "' for reading.\n");
      Process.Exit(2);
  END;

  (* Add to top-level scope *)
  scp := BuildTestScope();

  (* Read AST's *)
  Wr.PutText(stdout, "Each line you enter will be parsed and compiled as ");
  Wr.PutText(stdout, "a total command.\n\n");
  Wr.PutText(stdout, "> ");
  Wr.Flush(stdout);
  WHILE NOT Rd.EOF(in) DO
    TRY
      TRY
        (* Read the input *)
        line := Rd.GetLine(in);
        IF NOT Rd.Intermittent(in) THEN
          Wr.PutText(stdout, line & "\n");
        END;
        Wr.PutChar(stdout, '\n');
        Wr.Flush(stdout);

        (* Parse and compile the input *)
        res := NIL;
        JunoParse.Command(rd.init(line), cmd, tokenCnt);
        JunoChkBNF.TotalCmd(cmd);
        VAR tbl := NEW(StackTbl.T).init(); BEGIN
          res := JunoCompileRep.Cmd(cmd, scp, tbl);
          cmd := res.cmd;
          tempCnt := tbl.next_index - 1
        END;
        tokenCnt := LAST(INTEGER)
      EXCEPT
      | JunoLex.Error(err) =>
          Wr.PutText(stdout, "*** Lex error: ");
          Wr.PutText(stdout, JunoLex.ErrorText(err.kind) & ": ");
          Wr.PutText(stdout, "Found '" & err.initialChars & "'\n");
          Wr.Flush(stdout)
      | JunoParse.Error(err) =>
          EVAL Dummy(0); (* necessary to work around an M3 compiler bug *)
          Wr.PutText(stdout, "*** Parse error: ");
          Wr.PutText(stdout, "Found '" &  JunoToken.ToText(err.found) & "'");
          IF err.expected # JunoToken.Kind.Unknown THEN
            VAR e := NEW(JunoToken.T, kind := err.expected); BEGIN
              Wr.PutText(stdout, "; Expected '" & JunoToken.ToText(e) & "'");
            END
          END;
          Wr.PutChar(stdout, '\n');
          Wr.Flush(stdout)
      END;

      (* Unparse the compilation result *)
      JunoUnparse.Cmd(stdout, cmd, tokenCnt, debug := TRUE);
      Wr.PutText(stdout, "\n\n");
      Wr.Flush(stdout);
 
      (* Print AST nodes with predecessors *)
      IF backpointers THEN BackPtr.ShowAll(cmd, stdout) END;

      (* Assemble *)
      IF res # NIL THEN
        VAR
          bs := JunoAssemble.Cmd(res, scp, tempCnt,
            type := JunoAssemble.CmdType.Proc);
        BEGIN
          IF disassemble THEN
            JunoDisassem.P(bs, stdout);
            Wr.PutChar(stdout, '\n');
            Wr.Flush(stdout)
          END
        END
      END
    EXCEPT
    | JunoCompileErr.Error(err) =>
        Wr.PutText(stdout, "*** Compilation error: " & err.msg & "\n");
        JunoUnparse.P(stdout, err.ast, debug := TRUE);
        Wr.PutText(stdout, "\n\n");
        Wr.Flush(stdout);
    | Rd.Failure =>
        Wr.PutText(stderr, "*** Read failure!\n");
        Wr.Flush(stderr);
        EXIT;
    END;
    Wr.PutText(stdout, "> ");
    Wr.Flush(stdout)
  END;
  IF in # stdin THEN Rd.Close(in) END;
  IF NOT Rd.Intermittent(in) THEN Wr.PutText(stdout, "^D") END;
  Wr.PutChar(stdout, '\n');
  Wr.Flush(stdout)
END CompilerTest. 
