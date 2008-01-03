MODULE M3CGenIns;

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


IMPORT PropertyV;
IMPORT AST, M3AST_AS;
IMPORT M3Error, M3CUnit, M3CPragma, M3CPragmaF, ASTCopy, ASTWalk;

IMPORT M3AST_AS_F, M3AST_FE_F, M3AST_TL_F;

IMPORT SeqM3AST_AS_F_Interface_id, SeqM3AST_AS_Used_interface_id,
    SeqM3AST_AS_IMPORTED, SeqM3AST_AS_Import_item;

PROCEDURE Set(
    cu_ins, cu_def: M3AST_AS.Compilation_Unit
    ): M3AST_AS.Compilation_Unit
    RAISES {}=
  VAR
    unit_ins := NARROW(cu_ins.as_root, M3AST_AS.UNIT_GEN_INS);
    unit_def := NARROW(cu_def.as_root, M3AST_AS.UNIT_GEN_DEF);
    n_formals := SeqM3AST_AS_F_Interface_id.Length(unit_def.as_id_s);
    n_actuals := SeqM3AST_AS_Used_interface_id.Length(unit_ins.as_id_s);
    cu_res: M3AST_AS.Compilation_Unit;
  BEGIN
    IF n_formals # n_actuals THEN
      IF n_formals < n_actuals THEN
        M3Error.Report(unit_ins.as_gen_id,
            "too many actual parameters for generic");
      ELSE
        M3Error.Report(unit_ins.as_gen_id,
            "not enough actual parameters for generic");
      END; (* if *)
      M3CUnit.InclState(cu_ins.fe_status, M3CUnit.State.SErrors);
      RETURN NIL;
    END; (* if *)

    (* Prevent instantiation if parse errors in generic definition,
       and propagate error status *)
    IF M3CUnit.State.PErrors IN cu_def.fe_status THEN
      M3CUnit.InclState(cu_ins.fe_status, M3CUnit.State.PErrors);
      RETURN NIL;
    END; (* if *)

    (* A temporary hack to copy the pragma store, if it exists *)
    IF cu_def.lx_pragmas.last # NIL THEN
      <*FATAL ANY*> BEGIN
        cu_res := ASTCopy.ModeNodes(cu_def,
            NEW(CopyClosure, ps := cu_def.lx_pragmas).init(),
            ASTWalk.OnExit);
      END;
      ClonePragmas(cu_def, cu_res);
    ELSE
      cu_res := ASTCopy.Nodes(cu_def);
    END;
    M3CUnit.InclState(cu_res.fe_status, M3CUnit.State.Parsed);
    (* change the root node to be a UNIT_NORMAL *)
    TYPECASE unit_ins OF <*NOWARN*>
    | M3AST_AS.Interface_gen_ins(int_ins) =>
        VAR int_res: M3AST_AS.Interface := NEW(M3AST_AS.Interface).init();
        BEGIN
          int_res.as_unsafe := ASTCopy.Nodes(int_ins.as_unsafe);
          int_res.as_id := ASTCopy.Nodes(int_ins.as_id);
          CopyUNIT_WITH_BODY(cu_res.as_root, int_res);
          cu_res.as_root := int_res;
        END;
    | M3AST_AS.Module_gen_ins(mod_ins) =>
        VAR mod_res: M3AST_AS.Module := NEW(M3AST_AS.Module).init();
        BEGIN
          mod_res.as_unsafe := ASTCopy.Nodes(mod_ins.as_unsafe);
          mod_res.as_id := ASTCopy.Nodes(mod_ins.as_id);
          CopyUNIT_WITH_BODY(cu_res.as_root, mod_res);
          mod_res.as_export_s := mod_ins.as_export_s;
          cu_res.as_root := mod_res;
        END;
    END;

    (* Add the all important IMPORT Ai AS Fi ... nodes *)

    VAR
      iter_formals := SeqM3AST_AS_F_Interface_id.NewIter(unit_def.as_id_s);
      iter_actuals := SeqM3AST_AS_Used_interface_id.NewIter(unit_ins.as_id_s);
      si: M3AST_AS.Simple_import := NEW(M3AST_AS.Simple_import).init();
      fi: M3AST_AS.F_Interface_id;
      ai: M3AST_AS.Used_interface_id;
      im: M3AST_AS.Import_item;
    BEGIN
      WHILE SeqM3AST_AS_F_Interface_id.Next(iter_formals, fi) DO
        EVAL SeqM3AST_AS_Used_interface_id.Next(iter_actuals, ai);
        im := NEW(M3AST_AS.Import_item).init();
        im.as_intf_id := NEW(M3AST_AS.Used_interface_id).init();
        im.as_intf_id.lx_symrep := ai.lx_symrep;
        im.as_id := NEW(M3AST_AS.Interface_AS_id).init();
        im.as_id.lx_symrep := fi.lx_symrep;
        SeqM3AST_AS_Import_item.AddRear(si.as_import_item_s, im);
      END; (* while *)
      SeqM3AST_AS_IMPORTED.AddFront(
        NARROW(cu_res.as_root, M3AST_AS.UNIT_NORMAL).as_import_s,
        si);
    END;
    RETURN cu_res;
  END Set;

PROCEDURE CopyUNIT_WITH_BODY(
    unit_def, unit_res: M3AST_AS.UNIT_WITH_BODY)
    RAISES {}=
  BEGIN
    unit_res.as_import_s := unit_def.as_import_s;
    unit_res.as_block := unit_def.as_block;  
  END CopyUNIT_WITH_BODY;

TYPE
  CopyClosure = ASTCopy.Closure OBJECT
    ps: M3CPragma.Store;
  OVERRIDES
    callback := PragmaSupport;
  END;

PROCEDURE PragmaSupport(cl: CopyClosure; def, ins: AST.NODE;
                        <*UNUSED*> vm: ASTWalk.VisitMode)=
  VAR t := cl.ps.first;
  BEGIN
    WHILE t # NIL DO
      IF t.precedingNode = def OR t.followingNode = def OR
         t.precedingStmOrDecl = def THEN
        (* save a forwarding pointer from "def" to "ins" *)
        PropertyV.Put(NARROW(def, M3AST_AS.SRC_NODE).tl_pset, ins);
      END;
      t := t.next;
    END;
  END PragmaSupport;

PROCEDURE ClonePragmas(cu_def, cu_res: M3AST_AS.Compilation_Unit)=
  VAR t_def := cu_def.lx_pragmas.first;
    last: M3CPragma.Iter := NIL;
  BEGIN
    cu_res.lx_pragmas := NEW(M3CPragma.Store);
    WHILE t_def # NIL DO
      WITH t_ins = NEW(M3CPragma.Iter, pos := t_def.pos,
          body := t_def.body,
          precedingNode := Forward(t_def.precedingNode),
          followingNode := Forward(t_def.followingNode),
          precedingStmOrDecl := Forward(t_def.precedingStmOrDecl)) DO
        IF last = NIL THEN
          cu_res.lx_pragmas.first := t_ins;
        ELSE
          last.next := t_ins;
          t_ins.prev := last;
        END;
        last := t_ins;
      END;
      t_def := t_def.next;
    END;
    cu_res.lx_pragmas.last := last;
  END ClonePragmas;

PROCEDURE Forward(n: M3AST_AS.SRC_NODE): M3AST_AS.SRC_NODE=
  BEGIN
    IF n = NIL THEN RETURN NIL ELSE
      RETURN PropertyV.GetSub(n.tl_pset, TYPECODE(M3AST_AS.SRC_NODE));
    END;
  END Forward;

BEGIN
END M3CGenIns.
