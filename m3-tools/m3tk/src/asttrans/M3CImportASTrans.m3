(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3CImportASTrans;

IMPORT ASTWalk;
IMPORT AST, M3AST_AS, M3CUnit;
IMPORT SeqM3AST_AS_F_Interface_id, SeqM3AST_AS_Import_item;
IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

TYPE
  WalkClosure = ASTWalk.Closure OBJECT
    formals: SeqM3AST_AS_F_Interface_id.T;
    any := TRUE;
  OVERRIDES
    callback := DoSubstitute;
  END; (* object *)

PROCEDURE Set(cu: M3AST_AS.Compilation_Unit; genericFormalsOnly := FALSE)=
  BEGIN
    WITH cl = NEW(WalkClosure) DO
      IF genericFormalsOnly THEN
        cl.any := FALSE;
      END; (* if *)
      TYPECASE cu.as_root OF
      | M3AST_AS.UNIT_GEN_INS(unit_ins) =>
          cu := unit_ins.sm_ins_comp_unit;
          WITH gen_id = NARROW(unit_ins.as_gen_id.sm_def, M3AST_AS.UNIT_ID) DO
            cl.formals :=
              NARROW(gen_id.sm_spec, M3AST_AS.UNIT_GEN_DEF).as_id_s
          END;
      ELSE    
      END; (* typecase *)
      ASTWalk.VisitNodes(cu, cl);
    END; (* with *)
  END Set;

PROCEDURE DoSubstitute(cl: WalkClosure; an: AST.NODE;
    vm: ASTWalk.VisitMode) RAISES {}=
  VAR 
    nids: TEXT;
    id: M3AST_AS.USED_ID;
  BEGIN
    IF M3AST_AS.IsA_USED_ID(an, id) AND id.sm_def # NIL THEN
      TYPECASE id.sm_def OF
      | M3AST_AS.Interface_AS_id(as_id) =>
          IF cl.any OR IsGenericFormal(cl.formals, as_id) THEN
            id.lx_symrep := as_id.tmp_used_id.lx_symrep;
            id.sm_def := as_id.tmp_used_id.sm_def;
          END;
      ELSE (* not interested *)
      END; (* typecase *)
    END; (* if *)
    TYPECASE an OF
    | M3AST_AS.Simple_import(si) =>
        VAR
          iter := SeqM3AST_AS_Import_item.NewIter(si.as_import_item_s);
          ii: M3AST_AS.Import_item;
        BEGIN
          WHILE SeqM3AST_AS_Import_item.Next(iter, ii) DO
            IF cl.any OR IsGenericFormal(cl.formals, ii.as_id) THEN
              ii.as_id := NIL;
            END; (* if *)
          END; (* while *)
        END;
    ELSE
    END; (* typecase *)
  END DoSubstitute;

PROCEDURE IsGenericFormal(
    formals: SeqM3AST_AS_F_Interface_id.T;
    id: M3AST_AS.ID): BOOLEAN RAISES {}=
  VAR iter := SeqM3AST_AS_F_Interface_id.NewIter(formals);
      f_used_id: M3AST_AS.F_Interface_id;
  BEGIN
    WHILE SeqM3AST_AS_F_Interface_id.Next(iter, f_used_id) DO
      IF id.lx_symrep = f_used_id.lx_symrep THEN
        RETURN TRUE;
      END;
    END; (* while *)
    RETURN FALSE;
  END IsGenericFormal;


BEGIN

END M3CImportASTrans.
