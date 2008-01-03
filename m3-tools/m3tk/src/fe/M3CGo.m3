(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3CGo;

IMPORT Text, Rd, RefList;

IMPORT M3AST_AS, M3Time; 
IMPORT M3CSrcPos, M3CLiteral;

IMPORT M3AST_LX_F, M3AST_AS_F, M3AST_SM_F, M3AST_FE_F;

IMPORT SeqM3AST_AS_Used_interface_id, SeqM3AST_AS_IMPORTED,
       SeqM3AST_AS_Import_item;

IMPORT M3CId, M3Context, M3Error, M3CParse, M3CUnit, M3CUnitRep,
    M3Conventions;
IMPORT M3CMkStd, M3CSM, M3CSpec, M3CGenIns;


VAR
  current_g: M3AST_AS.Compilation_Unit;
  parser_g: M3CParse.T := NIL;


PROCEDURE CompileInterface(
    context: M3Context.T;
    used_id: M3AST_AS.Used_interface_id;
    caller: M3AST_AS.Compilation_Unit;
    isexport: BOOLEAN;
    p: ImportedUnitProc)
    RAISES {}=
  VAR
    interface: M3AST_AS.Compilation_Unit;
    ens: Text.T;
  BEGIN
    IF used_id.lx_symrep # NIL AND
      p(M3CId.ToText(used_id.lx_symrep), 
        M3CUnit.Type.Interface, context, interface) THEN
      used_id.sm_def := interface.as_root.as_id;
      IF ISTYPE(interface.as_root, M3AST_AS.UNIT_GEN_DEF) THEN
        M3Error.ReportWithId(used_id,
            "illegal import of GENERIC interface \'%s\'", used_id.lx_symrep);
      ELSIF NOT (M3CUnit.State.ImportsResolved IN interface.fe_status) AND
          (caller # interface) THEN
        (* cycle in imports! This can never happen if the 'lx_symrep' of either
         is NIL *)
        M3Error.ReportWithId(used_id,
            "import cycle between interfaces \'%s\' and \'%s\'",
            used_id.lx_symrep, caller.as_root.as_id.lx_symrep);
        M3CUnit.InclState(interface.fe_status, M3CUnit.State.IErrors);
        M3CUnit.InclState(caller.fe_status, M3CUnit.State.IErrors);
      ELSE
        (* if there were IErrors in interface, propagate this fact *)
        IF M3CUnit.State.IErrors IN interface.fe_status THEN
          M3CUnit.InclState(caller.fe_status, M3CUnit.State.IErrors);
        END;
        IF Safe(caller) THEN
          IF NOT Safe(interface) THEN
            IF isexport THEN
              ens := "illegal export of unsafe interface \'%s\'"
            ELSE
              ens := "illegal import of unsafe interface \'%s\'"
            END;
            M3Error.ReportWithId(used_id, ens, used_id.lx_symrep);
          END; (* if *)
        END; (* if *)
        RETURN (* OK *)
      END; (* if *)
    END;
    M3CUnit.InclState(current_g.fe_status, M3CUnit.State.IErrors);
  END CompileInterface;


<*INLINE*> PROCEDURE Safe(cu: M3AST_AS.Compilation_Unit): BOOLEAN RAISES {}=
  BEGIN
    RETURN NARROW(cu.as_root, M3AST_AS.UNIT_NORMAL).as_unsafe = NIL;
  END Safe;

PROCEDURE CompileImports(
    context: M3Context.T;
    cu: M3AST_AS.Compilation_Unit;
    p: ImportedUnitProc)
    RAISES {}=
  VAR
    iterImported: SeqM3AST_AS_IMPORTED.Iter;
    imported: M3AST_AS.IMPORTED;
    iterImport_item: SeqM3AST_AS_Import_item.Iter;
    import_item: M3AST_AS.Import_item;
    local_from_name: M3AST_AS.Import_item;
    from_name: M3AST_AS.Used_interface_id;
    unit_normal := NARROW(cu.as_root, M3AST_AS.UNIT_NORMAL);
  BEGIN
    iterImported := SeqM3AST_AS_IMPORTED.NewIter(unit_normal.as_import_s);
    WHILE SeqM3AST_AS_IMPORTED.Next(iterImported, imported) DO
      TYPECASE imported OF <*NOWARN*>
      | M3AST_AS.Simple_import(si) =>
          iterImport_item := SeqM3AST_AS_Import_item.NewIter(si.as_import_item_s);
          WHILE SeqM3AST_AS_Import_item.Next(
              iterImport_item, import_item) DO
            CompileInterface(context, import_item.as_intf_id, cu, FALSE, p);
          END; (* while *)
      | M3AST_AS.From_import(fi) =>
          local_from_name := CheckForLocalName(unit_normal, fi.as_intf_id);
          IF local_from_name = NIL THEN from_name := fi.as_intf_id
          ELSE from_name := local_from_name.as_intf_id;
          END;
          CompileInterface(context, from_name, cu, FALSE, p);
          (* fix things up; CompileInterface will have set from_name.sm_def
             to the Interface_id of the imported AST. However, if
             local_from_name # NIL, we need to bind to its Interface_AS_id.
          *)
          IF local_from_name # NIL THEN
            fi.as_intf_id.sm_def := local_from_name.as_id;
          END; (* if *)
      END; (* typecase *)
    END;
  END CompileImports;

PROCEDURE CheckForLocalName(
    unit_normal: M3AST_AS.UNIT_NORMAL;
    intf_id: M3AST_AS.Used_interface_id)
    : M3AST_AS.Import_item
    RAISES {}=
  VAR
    iterImported: SeqM3AST_AS_IMPORTED.Iter;
    imported: M3AST_AS.IMPORTED;
    iterImport_item: SeqM3AST_AS_Import_item.Iter;
    import_item: M3AST_AS.Import_item;
  BEGIN
    iterImported := SeqM3AST_AS_IMPORTED.NewIter(unit_normal.as_import_s);
    WHILE SeqM3AST_AS_IMPORTED.Next(iterImported, imported) DO
      TYPECASE imported OF
      | M3AST_AS.Simple_import(si) =>
          iterImport_item := SeqM3AST_AS_Import_item.NewIter(si.as_import_item_s);
          WHILE SeqM3AST_AS_Import_item.Next(
              iterImport_item, import_item) DO
            IF import_item.as_id # NIL AND
               import_item.as_id.lx_symrep = intf_id.lx_symrep THEN
              RETURN import_item;
            END;
          END; (* while *)
      ELSE (* ignore From_import *)
      END; (* typecase *)
    END;
    RETURN NIL;
  END CheckForLocalName;


PROCEDURE CompileExports(
    context: M3Context.T;
    cu: M3AST_AS.Compilation_Unit;
    p: ImportedUnitProc)
    RAISES {}=
  VAR
    moduleId: M3AST_AS.Module_id;
    exports: SeqM3AST_AS_Used_interface_id.T;
    iterExports: SeqM3AST_AS_Used_interface_id.Iter;
    export: M3AST_AS.Used_interface_id;
  BEGIN
    TYPECASE cu.as_root OF
    | M3AST_AS.Module(module) =>
        moduleId := module.as_id;
        exports := module.as_export_s;
        IF SeqM3AST_AS_Used_interface_id.Empty(exports) THEN
          (* implicit export of interface with same name as module *)
          export := NEW(M3AST_AS.Used_interface_id).init();
          export.lx_symrep := moduleId.lx_symrep;
          export.lx_srcpos := moduleId.lx_srcpos;
          SeqM3AST_AS_Used_interface_id.AddRear(module.sm_export_s, export);
          CompileInterface(context, export, cu, TRUE, p);
        ELSE
          iterExports := SeqM3AST_AS_Used_interface_id.NewIter(exports);
          WHILE SeqM3AST_AS_Used_interface_id.Next(iterExports, export) DO
            SeqM3AST_AS_Used_interface_id.AddRear(module.sm_export_s, export);
            CompileInterface(context, export, cu, TRUE, p);
          END; (* while *)
        END; (* if *)
    ELSE
      (* interface - does not have any exports *)
    END; (* typecase *)
  END CompileExports;


PROCEDURE ResolveImportsAndExports(
    context: M3Context.T;
    cu: M3AST_AS.Compilation_Unit;
    p: ImportedUnitProc)
    RAISES {}=
  BEGIN
    IF NOT (M3CUnit.State.ImportsResolved IN cu.fe_status) THEN
      CompileExports(context, cu, p);
      CompileImports(context, cu, p);
      M3CUnit.InclState(cu.fe_status, M3CUnit.State.ImportsResolved);
    END; (* if *)
  END ResolveImportsAndExports;


PROCEDURE ErrorHandler(
    <*UNUSED*> h: M3CParse.ErrorHandler;
    pos: M3CSrcPos.T;
    msg: TEXT)
    RAISES {}=
  BEGIN
    M3CUnit.InclState(current_g.fe_status, M3CUnit.State.PErrors);
    M3Error.ReportAtPos(pos, msg);
  END ErrorHandler;


PROCEDURE Parse(
    input: Rd.T;
    cu: M3AST_AS.Compilation_Unit;
    headerOnly: BOOLEAN;
    ) RAISES {Rd.Failure}=
  VAR
    tcu: M3AST_AS.Compilation_Unit;
  BEGIN
    tcu := NewParser(input).compilation(headerOnly);
    cu.as_root := tcu.as_root;
    cu.lx_pragmas := tcu.lx_pragmas;
    cu.lx_comments := tcu.lx_comments;

    (* Unless we have no tree at all, mark as parsed *)
    IF cu.as_root # NIL THEN
      M3CUnit.InclState(cu.fe_status, M3CUnit.State.Parsed);
    END; (* if *)
  END Parse;

<*INLINE*> PROCEDURE NewParser(input: Rd.T): M3CParse.T RAISES {}=
  VAR
    errorHandler: M3CParse.ErrorHandler;
  BEGIN
    (* reuse global parser *)
    IF parser_g = NIL THEN
      errorHandler := NEW(M3CParse.ErrorHandler, handle := ErrorHandler);
      parser_g := NEW(M3CParse.T).init(input, M3CId.Table(),
                                       M3CLiteral.Table(), errorHandler);
    ELSE
      parser_g.reset(rd := input);
    END; (* if *)
    RETURN parser_g;
  END NewParser;

PROCEDURE CompileUnit(
    cu: M3AST_AS.Compilation_Unit;
    context: M3Context.T;
    stream: Rd.T;
    p: ImportedUnitProc;
    VAR (*inout*) phases: M3CUnit.Status;
    compTime: M3Conventions.CompTime;
    headerOnly := FALSE
    ) RAISES {}=
  <*FATAL Rd.Failure*>
  VAR
    saveCu: M3AST_AS.Compilation_Unit := current_g;
    startTime: M3Time.T;
    checkedSem := FALSE;
  BEGIN
   current_g := cu;
   TRY
    (* PARSING *)
    IF M3CUnit.State.Parsed IN phases THEN
      IF compTime # NIL THEN startTime := M3Time.Now(); END;
      Parse(stream, cu, headerOnly);
      IF compTime # NIL THEN compTime.parse := M3Time.Interval(startTime) END;
      phases := phases + (cu.fe_status * M3CUnit.Errors);
    END;

    (* GENERICS *)
    IF ISTYPE(cu.as_root, M3AST_AS.UNIT_GEN_DEF) THEN
      (* nothing more to do for generic definitions *)
      M3CSpec.Set(cu);
      RETURN
    ELSIF ISTYPE(cu.as_root, M3AST_AS.UNIT_GEN_INS) THEN
      (* ok, do the instantiation, and compile it *)
      VAR cu_res := CompileGenericInstantiation(context, cu, p);
      BEGIN
        IF cu_res = NIL THEN
          RETURN
        ELSE
          (* continue with instantiated unit *)
          cu := cu_res;
          current_g := cu;
          M3Error.SetCu(cu);
        END; (* if *)
      END;
    END; (* if *)

    (* IMPORT RESOLUTION *)
    IF M3CUnit.State.ImportsResolved IN phases THEN
      (* precondition *)
      IF M3CUnit.State.Parsed IN cu.fe_status THEN
        IF cu = M3Context.Standard() THEN M3CMkStd.TransForm(cu); END;
        ResolveImportsAndExports(context, cu, p);
        IF M3CUnit.State.IErrors IN cu.fe_status THEN
          M3CUnit.InclState(phases, M3CUnit.State.IErrors);
        END; (* if *)
      ELSE
        M3CUnit.InclState(phases, M3CUnit.State.IErrors);
      END;
    END;

    (* SEMANTIC ANALYSIS *)
    IF M3CUnit.State.SemChecked IN phases THEN
      (* precondition *)
      IF M3CUnit.State.ImportsResolved IN cu.fe_status THEN
        (* IMPORT errors make checking semantics worthless/dangerous *)
        IF M3CUnit.State.IErrors IN cu.fe_status THEN
          M3Error.Report(cu.as_root,
              "semantic analysis suppressed due to import errors");
        ELSE
          IF compTime # NIL THEN startTime := M3Time.Now(); END;
          M3CSM.Check(cu);
          checkedSem := TRUE;
          M3CUnit.InclState(cu.fe_status, M3CUnit.State.SemChecked); 
          IF compTime # NIL THEN 
            compTime.semantic := M3Time.Interval(startTime) 
          END;
        END; (* if *)
      ELSE
        M3CUnit.InclState(phases, M3CUnit.State.SErrors);
      END; (* if *)
    END;
    FINALLY
      LOCK extensions_m DO
        VAR list: RefList.T := extensions_g;
        BEGIN
          WHILE list # NIL DO
           NARROW(list.head, ExtensionElem).e.extend(context, cu, phases);
            list := list.tail;
          END;
        END;
      END;
      IF checkedSem THEN
        M3CSM.FinishUp(cu);
      END;
      current_g := saveCu;
    END;
  END CompileUnit;

PROCEDURE CompileGenericInstantiation(
    context: M3Context.T;
    cu_ins: M3AST_AS.Compilation_Unit;
    p: ImportedUnitProc): M3AST_AS.Compilation_Unit
    RAISES {}=
  VAR
    unit_ins := NARROW(cu_ins.as_root, M3AST_AS.UNIT_GEN_INS);
    gen_id := unit_ins.as_gen_id;
    cu_def: M3AST_AS.Compilation_Unit;
    ut: M3CUnit.Type;
  BEGIN
    IF ISTYPE(unit_ins, M3AST_AS.Interface_gen_ins) THEN
      ut := M3CUnit.Type.Interface;
    ELSE
      ut := M3CUnit.Type.Module;
    END; (* if *)
    IF gen_id.lx_symrep # NIL AND
       p(M3CId.ToText(gen_id.lx_symrep), ut, context, cu_def) THEN
      IF ut = M3CUnit.Type.Interface AND
         NOT ISTYPE(cu_def.as_root, M3AST_AS.Interface_gen_def) THEN
        M3Error.ReportWithId(gen_id,
            "\'%s\' is not an GENERIC interface", gen_id.lx_symrep);
      ELSIF ut = M3CUnit.Type.Module AND  
          NOT ISTYPE(cu_def.as_root, M3AST_AS.Module_gen_def) THEN
        M3Error.ReportWithId(gen_id,
            "\'%s\' is not an GENERIC module", gen_id.lx_symrep);
      ELSE
        gen_id.sm_def := cu_def.as_root.as_id;
        unit_ins.sm_ins_comp_unit := M3CGenIns.Set(cu_ins, cu_def);
        WITH cu_res = unit_ins.sm_ins_comp_unit DO
          IF cu_res # NIL THEN
            cu_res.fe_uid := NEW(M3CUnit.Uid);
            cu_res.fe_uid.filename := cu_def.fe_uid.filename &
                "[" & cu_ins.fe_uid.filename & "]";
            cu_res.fe_uid.stamp := cu_ins.fe_uid.stamp;
            IF M3Conventions.PrimarySource IN cu_ins.fe_status THEN
              M3CUnit.InclState(cu_res.fe_status, M3Conventions.PrimarySource);
            END; (* if *)
          END;
        END; (* with *)
        RETURN unit_ins.sm_ins_comp_unit;
      END; (* if *)
    END; (* if *)
    M3CUnit.InclState(cu_ins.fe_status, M3CUnit.State.IErrors);
    RETURN NIL;
  END CompileGenericInstantiation;


PROCEDURE Current(): M3AST_AS.Compilation_Unit RAISES {}=
  BEGIN
    RETURN current_g;
  END Current;

REVEAL
  Extension = Extension_public BRANDED OBJECT END;

TYPE ExtensionElem = REF RECORD e: Extension END;

VAR
  extensions_g: RefList.T := NIL;
  extensions_m := NEW(MUTEX);

PROCEDURE AddExtension(e: Extension) RAISES {}=
  BEGIN
    LOCK extensions_m DO
      extensions_g := RefList.AppendD(extensions_g,
                             RefList.List1(NEW(ExtensionElem, e := e)));
    END; (* lock *)
  END AddExtension;

PROCEDURE RemoveExtension(e: Extension) RAISES {}=
  VAR l, prev: RefList.T := extensions_g;
  BEGIN
    LOCK extensions_m DO
      WHILE l # NIL DO
        IF NARROW(l.head, ExtensionElem).e = e THEN
          IF prev = NIL THEN extensions_g := l.tail
          ELSE prev.tail := l.tail;
          END;
          RETURN
        END;
        prev := l; l := l.tail;
      END;
    END; (* lock *)
  END RemoveExtension;


BEGIN
  current_g := NIL;
END M3CGo.
