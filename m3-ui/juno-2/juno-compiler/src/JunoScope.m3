(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Nov 12 18:54:18 PST 1994 by heydon *)
(*      modified on Fri Aug  7 21:54:05 PDT 1992 by myers *)

MODULE JunoScope;

IMPORT JunoAST, JunoASTUtils, StackTbl, JunoUnparse, JunoCompileErr;
IMPORT JunoRT;
IMPORT Atom, AtomRefTbl;
IMPORT Wr, Fmt, Stdio;
FROM Thread IMPORT Alerted;

<* FATAL Wr.Failure, Alerted *>

REVEAL
  T = AtomRefTbl.Default BRANDED "JunoScope.T" OBJECT
    parent: T := NIL;
  END;

REVEAL
  Entity = BRANDED "Entity" OBJECT END;
  Temp = LocalValue BRANDED "Temp" OBJECT END;
  Arg = LocalParam BRANDED "Arg" OBJECT END;
  Const = Value BRANDED "Const" OBJECT END;
  Var = Value BRANDED "Var" OBJECT END;
  Pred = PredCode BRANDED "Pred" OBJECT END;
  Func = PredCode BRANDED "Func" OBJECT END;
  Proc = ProcCode BRANDED "Proc" OBJECT END;
  Mod = Unit BRANDED "ModEntity" OBJECT END;

PROCEDURE New(p: T; size: CARDINAL := 1): T =
  BEGIN
    RETURN NEW(T, parent := p).init(size)
  END New;

PROCEDURE Parent(scp: T): T =
  BEGIN RETURN scp.parent END Parent;

PROCEDURE SetParent(scp, parent: T) =
  BEGIN scp.parent := parent END SetParent;

PROCEDURE Lookup(scp: T; id: Atom.T; localOnly := FALSE): Entity =
  VAR ref: REFANY; BEGIN
    WHILE scp # NIL DO
      IF scp.get(id, ref) THEN
        RETURN NARROW(ref, Entity)
      ELSIF localOnly
        THEN EXIT
        ELSE scp := scp.parent
      END
    END;
    RETURN NIL;
  END Lookup;

PROCEDURE LookupQId(
    scp: T;
    qid: JunoAST.QId;
    VAR (*OUT*) unit: Entity): Entity =
  BEGIN
    IF qid.id0 = JunoAST.NilId THEN
      unit := NIL;
      RETURN Lookup(scp, qid.id1)
    ELSE
      unit := Lookup(scp, qid.id0);
      TYPECASE unit OF
        NULL => RETURN NIL
      | Mod(m) => RETURN Lookup(m.public_scp, qid.id1)
      ELSE RETURN NIL
      END
    END
  END LookupQId;

PROCEDURE Names(scp: T): REF ARRAY OF Atom.T =
  VAR
    it := scp.iterate();
    key: Atom.T; dummy: REFANY;
    res := NEW(REF ARRAY OF Atom.T, scp.size());
    i := 0;
  BEGIN
    WHILE it.next(key, dummy) DO
      res[i] := key; INC(i)
    END;
    RETURN res
  END Names;

PROCEDURE LocalArgs(scp: T; kinds: SET OF ArgKind): JunoAST.IdList =
  VAR
    it := scp.iterate();
    key: Atom.T; value: REFANY;
    res := NEW(JunoAST.IdList);
  BEGIN
    WHILE it.next(key, value) DO
      TYPECASE value OF Arg (arg) =>
        IF arg.kind IN kinds THEN
          VAR link := NEW(JunoAST.IdLink, id := key,
            index := arg.offset, next := res.head);
          BEGIN
            res.head := link;
            INC(res.size)
          END
        END
      ELSE (* SKIP *)
      END;
    END;
    RETURN res
  END LocalArgs;

PROCEDURE Bind(scp: T; id: Atom.T; e: Entity) RAISES {NameClash} =
  VAR dummy: REFANY; BEGIN
    IF NOT scp.get(id, dummy)
      THEN EVAL scp.put(id, e)
      ELSE RAISE NameClash
    END
  END Bind;

PROCEDURE Rebind(scp: T; id: Atom.T; e: Entity) =
  BEGIN
    EVAL scp.put(id, e)
  END Rebind;

PROCEDURE Unbind(scp: T; id: Atom.T): Entity RAISES { NotFound } =
  VAR res: REFANY; BEGIN
    IF NOT scp.delete(id, res) THEN RAISE NotFound END;
    RETURN NARROW(res, Entity)
  END Unbind;

CONST Tab = 2;

PROCEDURE Indent(wr: Wr.T; indent: CARDINAL) =
  BEGIN
    WHILE indent > 0 DO Wr.PutChar(wr, ' '); DEC(indent) END
  END Indent;

PROCEDURE Debug(scp: T; level: CARDINAL := 0) =
  BEGIN
    Print(Stdio.stderr, scp, level, 2)
  END Debug;

PROCEDURE Print(wr: Wr.T; scp: T; level, indent: CARDINAL := 0) =
  BEGIN
    IF scp = NIL THEN
      Indent(wr, indent);
      Wr.PutText(wr, "<NIL scope>\n")
    ELSE
      VAR key := Names(scp); BEGIN
        FOR i := 0 TO NUMBER(key^) - 1 DO
          Indent(wr, indent);
          Wr.PutText(wr, "\"" & Atom.ToText(key[i]) & "\": ");
          PrintEntity(wr, Lookup(scp, key[i]), level, indent + Tab);
        END
      END
    END;

    Wr.Flush(wr)
  END Print;

PROCEDURE PrintEntity(wr: Wr.T; ent: Entity; level, indent: CARDINAL) =

  PROCEDURE PrintInt(wr: Wr.T; field: TEXT; val: INTEGER; in: CARDINAL) =
    BEGIN
      Indent(wr, in); Wr.PutText(wr, field & " = " & Fmt.Int(val) & "\n");
    END PrintInt;

  PROCEDURE PrintArgKind(wr: Wr.T;
      field: TEXT; val: ArgKind; in: CARDINAL) =
    BEGIN
      Indent(wr, in); Wr.PutText(wr, field & " = ");
      CASE val OF
      | ArgKind.Out => Wr.PutText(wr, "OUT\n")
      | ArgKind.InOut => Wr.PutText(wr, "INOUT\n")
      | ArgKind.In => Wr.PutText(wr, "IN\n")
      END
    END PrintArgKind;

  PROCEDURE PrintExpr(wr: Wr.T; field: TEXT; e: JunoAST.Expr; in: CARDINAL) =
    BEGIN
      Indent(wr, in); Wr.PutText(wr, field & " = ");
      JunoUnparse.Expr(wr, e, LAST(INTEGER), indent := 0);
      Wr.PutChar(wr, '\n');
    END PrintExpr;

  PROCEDURE PrintScope(wr: Wr.T; field: TEXT; val: T; in: CARDINAL) =
    BEGIN
      Indent(wr, in); Wr.PutText(wr, field);
      IF level > 0
        THEN Wr.PutText(wr, " =\n"); Print(wr, val, level - 1, in + Tab)
        ELSE Wr.PutText(wr, " (elided)\n")
      END
    END PrintScope;

  (* PrintEntity *)
  BEGIN
    TYPECASE ent OF <* NOWARN *>
    | Temp (e) =>
        Wr.PutText(wr, "Temp\n");
        PrintInt(wr, "Offset", e.offset, indent)
    | Arg (e) =>
        Wr.PutText(wr, "Arg\n");
        PrintInt(wr, "Offset", e.offset, indent);
        PrintArgKind(wr, "Kind", e.kind, indent)
    | Const (e) =>
        Wr.PutText(wr, "CONST\n");
        PrintInt(wr, "Index", e.index, indent);
        PrintExpr(wr, "Init",  e.init,  indent)
    | Var (e) =>
        Wr.PutText(wr, "VAR\n");
        PrintInt(wr, "Index", e.index, indent);
        IF e.init # JunoAST.NilExpr THEN
          PrintExpr(wr, "Init",  e.init,  indent)
        END
    | Pred (e) =>
        Wr.PutText(wr, "PREDICATE\n");
        PrintInt(wr, "Index", e.index, indent);
        PrintInt(wr, "In-Cnt", e.in_cnt, indent);
        PrintScope(wr, "Argument Scope", e.formals, indent);
    | Func (e) =>
        Wr.PutText(wr, "FUNCTION\n");
        PrintInt(wr, "Index", e.index, indent);
        PrintInt(wr, "In-Cnt", e.in_cnt, indent);
        PrintScope(wr, "Argument Scope", e.formals, indent);
    | Proc (e) =>
        Wr.PutText(wr, "PROCEDURE\n");
        PrintInt(wr, "Index", e.index, indent);
        PrintInt(wr, "In-Cnt", e.in_cnt, indent);
        PrintInt(wr, "InOut-Cnt", e.inout_cnt, indent);
        PrintInt(wr, "Out-Cnt", e.out_cnt, indent);
        PrintScope(wr, "Argument Scope", e.formals, indent);
    | Mod (e) =>
        Wr.PutText(wr, "MODULE\n");
        PrintScope(wr, "Public Scope", e.public_scp, indent);
        PrintScope(wr, "Private Scope", e.scp, indent);
    END
  END PrintEntity;

PROCEDURE NewPred(pred: JunoAST.PredDecl; mod: JunoAST.Id): Pred
    RAISES {JunoCompileErr.Error} =
  VAR
    tbl := NEW(StackTbl.T).init();
    header := pred.header;
    slot := JunoRT.GetCodeIndex(JunoRT.ProcAttr{
      mod, header.name, JunoRT.Sig{0, 0, header.ins.size}});
  BEGIN
    RETURN NEW(Pred, index := slot, body := pred.body, tbl := tbl,
      formals := ArgScope(tbl, header, header.ins, NIL, NIL),
      in_cnt := header.ins.size, normal_form := NIL)
  END NewPred;

PROCEDURE NewFunc(func: JunoAST.FuncDecl; mod: JunoAST.Id): Func
    RAISES {JunoCompileErr.Error} =
  VAR
    tbl := NEW(StackTbl.T).init();
    header := func.header;
    slot := JunoRT.GetCodeIndex(JunoRT.ProcAttr{
      mod, header.name, JunoRT.Sig{1, 0, header.ins.size}});
    outs := JunoASTUtils.NewIdList(header.result,
      index := -(header.ins.size + 1));
  BEGIN
    RETURN NEW(Func, index := slot, body := func.body, tbl := tbl,
      formals := ArgScope(tbl, header, header.ins, NIL, outs),
      in_cnt := header.ins.size, normal_form := NIL)
  END NewFunc;

PROCEDURE NewProc(proc: JunoAST.ProcDecl; mod: JunoAST.Id): Proc
    RAISES {JunoCompileErr.Error} =
  VAR
    tbl := NEW(StackTbl.T).init();
    header := proc.header;
    slot := JunoRT.GetCodeIndex(JunoRT.ProcAttr{mod, header.name, 
      JunoRT.Sig{header.outs.size, header.inouts.size, header.ins.size}});
  BEGIN
    RETURN NEW(Proc, index := slot, body := proc.body,
      formals := ArgScope(tbl, header, header.ins, header.inouts, header.outs),
      in_cnt := header.ins.size, inout_cnt := header.inouts.size,
      out_cnt := header.outs.size, tbl := tbl)
  END NewProc;

PROCEDURE ArgScope(
    tbl: StackTbl.T;
    header: JunoAST.Header;
    ins, inouts, outs: JunoAST.IdList): T
    RAISES {JunoCompileErr.Error} =
(* Return a new "JunoScope.T" containing one "JunoScope.Arg" for each formal
   in the lists "ins", "inouts", and "outs". This procedure has the
   side-effect of assigning each formal an index according to
   "tbl.next_formal"; it also adds the formals to "tbl". The indices are
   assigned first to "ins", then to "inouts", and finally to "outs", but
   within each group, indices are assigned in reverse order. Therefore, if
   "tbl" is a newly initialized table, the indices start at -1 and decrease
   from right to left in the order in which the arguments appear in the
   declaration. For example, in the declaration:

|    PROCEDURE a,b := (c,d):P(e,f)

   the arguments are assigned indices as follows: f => -1, e => -2, d => -3, c
   => -4, b => -5, and a => -6.

   Raises "JunoCompileErr.Error" with argument AST "header" in the event that
   the same name is used for more than one formal in the header. *)
  VAR res := New(NIL);
  PROCEDURE AddArgs(ids: JunoAST.IdLink; kind: ArgKind) RAISES {NameClash} =
    (* IMPLEMENTATION: Use recursion to add arguments in reverse order. *)
    BEGIN
      IF ids # NIL THEN
        AddArgs(ids.next, kind);
        Bind(res, ids.id, NEW(Arg, kind := kind, offset := tbl.next_formal));
        ids.index := tbl.next_formal;
        StackTbl.PushFormal(tbl, ids.id)
      END
    END AddArgs;
  BEGIN
    TRY
      AddArgs(ins.head, ArgKind.In);
      IF inouts # NIL THEN AddArgs(inouts.head, ArgKind.InOut) END;
      IF outs # NIL THEN AddArgs(outs.head, ArgKind.Out) END
    EXCEPT
      NameClash =>
        JunoCompileErr.Raise(
          "This declaration contains duplicate formal names", header)
    END;
    RETURN res
  END ArgScope;

BEGIN END JunoScope.
