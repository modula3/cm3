(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3LProgContext;

IMPORT RefList, Text;
IMPORT M3Context, M3CUnit, M3AST_AS, M3CId;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_PL_F;

IMPORT M3LMain, M3LDepends;
IMPORT SeqM3AST_AS_Module, SeqM3AST_AS_Used_interface_id,
    SeqM3AST_AS_Interface;

PROCEDURE BuildInContext(
    c: M3Context.T;
    m: M3AST_AS.Module): M3Context.T
    RAISES {}=
  VAR
    iter: SeqM3AST_AS_Module.Iter;
    depends: M3AST_AS.Module;
  BEGIN
    RecordImportsAndExports(c, m);
    (* record imports/exports for dependent modules *)       
    iter := SeqM3AST_AS_Module.NewIter(m.pl_dependson_s);
    WHILE SeqM3AST_AS_Module.Next(iter, depends) DO
      RecordImportsAndExports(c, depends);
    END; (* while *)
    RETURN c;
  END BuildInContext;

PROCEDURE Build(m: M3AST_AS.Module): M3Context.T RAISES {}=
  BEGIN
    WITH c = M3Context.New() DO RETURN BuildInContext(c, m) END;
  END Build;

PROCEDURE BuildC(c: M3Context.T; name: TEXT := NIL): M3Context.T RAISES {}=
  VAR
    cu_s: RefList.T := M3LMain.Module(c);
  BEGIN
    IF cu_s = NIL THEN
      RETURN NIL;  (* no main program modules at all *)
    ELSE
      VAR cu: M3AST_AS.Compilation_Unit := cu_s.head;
      BEGIN
        IF name # NIL THEN
          WHILE cu_s # NIL DO
            cu := cu_s.head;
            IF Text.Equal(name, M3CId.ToText(cu.as_root.as_id.lx_symrep)) THEN
              EXIT;
            END;
            cu_s := cu_s.tail;
          END;
          IF cu_s = NIL THEN RETURN NIL END;
        END;
        M3LDepends.Set(c, M3LDepends.Default());
        RETURN Build(cu.as_root);
      END;
    END; (* if *)    
  END BuildC;


TYPE
  Closure = M3Context.Closure OBJECT
    list: SeqM3AST_AS_Interface.T;
    exporters := SeqM3AST_AS_Module.Null;
  OVERRIDES
    callback := FindExporters
  END;

PROCEDURE BuildPartial(
    c: M3Context.T;
    list: SeqM3AST_AS_Interface.T)
    : M3Context.T RAISES {}=
  VAR
    iter: SeqM3AST_AS_Module.Iter;
    module: M3AST_AS.Module;
    nc := M3Context.New();
    cl := NEW(Closure, list := list);
  BEGIN
    <*FATAL ANY*> BEGIN
      M3Context.ApplyToSet(c, cl,
          M3CUnit.TypeSet{M3CUnit.Type.Module, M3CUnit.Type.Module_gen_ins});
    END;
    iter := SeqM3AST_AS_Module.NewIter(cl.exporters);
    M3LDepends.Set(c, M3LDepends.Default());
    WHILE SeqM3AST_AS_Module.Next(iter, module) DO
      EVAL BuildInContext(nc, module);
    END; (* while *)
    RETURN nc;
  END BuildPartial;

PROCEDURE FindExporters(
    cl: Closure;
    ut: M3CUnit.Type;
    <*UNUSED*> name: TEXT;
    cu: M3AST_AS.Compilation_Unit)
    RAISES {}=
  VAR
    iter := SeqM3AST_AS_Interface.NewIter(cl.list);
    iter2: SeqM3AST_AS_Used_interface_id.Iter;
    intf: M3AST_AS.Interface;
    used_intf_id: M3AST_AS.Used_interface_id;
    module: M3AST_AS.Module;
  BEGIN
    cu := M3CUnit.ToGenIns(cu, ut);
    module := cu.as_root;
    iter2 := SeqM3AST_AS_Used_interface_id.NewIter(module.sm_export_s);
    WHILE SeqM3AST_AS_Used_interface_id.Next(iter2, used_intf_id) DO
      WHILE SeqM3AST_AS_Interface.Next(iter, intf) DO
        IF used_intf_id.sm_def= intf.as_id THEN
          SeqM3AST_AS_Module.AddFront(cl.exporters, module);
        END;
      END; (* while *)
    END; (* while *)
  END FindExporters;


PROCEDURE RecordImportsAndExports(
    c: M3Context.T; depends: M3AST_AS.Module)
    RAISES {}=
  VAR
    iter2: SeqM3AST_AS_Used_interface_id.Iter;
    used_intf_id: M3AST_AS.Used_interface_id;
  BEGIN
    CheckEnter(c, depends, M3CUnit.Type.Module);
    (* record which interfaces this module uses/exports *)
    iter2 := SeqM3AST_AS_Used_interface_id.NewIter(depends.sm_import_s);
    WHILE SeqM3AST_AS_Used_interface_id.Next(iter2, used_intf_id) DO
      CheckEnterId(c, used_intf_id.sm_def);
    END; (* while *)
    iter2 := SeqM3AST_AS_Used_interface_id.NewIter(depends.sm_export_s);
    WHILE SeqM3AST_AS_Used_interface_id.Next(iter2, used_intf_id) DO
      CheckEnterId(c, used_intf_id.sm_def);
    END; (* while *)
    END RecordImportsAndExports;

PROCEDURE CheckEnter(c: M3Context.T; u: M3AST_AS.UNIT;
    ut: M3CUnit.Type) RAISES {}=
  BEGIN
    TRY
      M3Context.Add(c, M3CId.ToText(u.as_id.lx_symrep), ut, u.sm_comp_unit);
    EXCEPT
    | M3Context.Duplicate => (* thats ok *)
    END; (* try *)
  END CheckEnter;

PROCEDURE CheckEnterId(c: M3Context.T; d: M3AST_AS.UNIT_ID) RAISES {}=
  VAR ut: M3CUnit.Type;
  BEGIN
    IF d # NIL THEN
      IF ISTYPE(d, M3AST_AS.Interface_id) THEN ut := M3CUnit.Type.Interface
      ELSE ut := M3CUnit.Type.Module;
      END;
      CheckEnter(c, d.sm_spec, ut);
    END; (* if *)
  END CheckEnterId;

BEGIN

END M3LProgContext.








