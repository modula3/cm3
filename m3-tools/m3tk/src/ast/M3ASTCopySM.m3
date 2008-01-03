MODULE M3ASTCopySM;

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

IMPORT M3AST, M3AST_AS, M3AST_SM, M3AST_SM_F, ASTWalk, M3Assert, ASTCopy;

PROCEDURE Attrs(
    <*UNUSED*> cl: ASTCopy.Closure;
    n, ncopy: M3AST.NODE; vm: ASTWalk.VisitMode) RAISES {}=
  VAR
    init_id, init_id_copy: M3AST_SM.INIT_ID;
    ccv_id, ccv_id_copy: M3AST_SM.CCV_ID;
    redef_id, redef_id_copy: M3AST_SM.REDEF_ID;
    recobj_id, recobj_id_copy: M3AST_SM.RECOBJ_ID;
    used_id, used_id_copy: M3AST_AS.USED_ID;
    exp_copy: M3AST_AS.EXP;
    type_spec_copy: M3AST_AS.TYPE_SPEC;
    typed_id_copy: M3AST_AS.TYPED_ID;
    unit_copy: M3AST_AS.UNIT;
    unit_copy_wb: M3AST_AS.UNIT_WITH_BODY;
    unit_id_copy: M3AST_AS.UNIT_ID;
    proc_id_copy: M3AST_AS.Proc_id;
    named_type_copy: M3AST_AS.Named_type;
    subrange_type_copy: M3AST_AS.Subrange_type;
    enumeration_type_copy: M3AST_AS.Enumeration_type;
    array_type_copy: M3AST_AS.Array_type;
    opaque_type_copy: M3AST_AS.Opaque_type;
    object_type_copy: M3AST_AS.Object_type;
    procedure_type_copy: M3AST_AS.Procedure_type;
    module_copy: M3AST_AS.Module;
    call_copy: M3AST_AS.Call;
    constructor_copy: M3AST_AS.Constructor;
  BEGIN
    IF vm = ASTWalk.VisitMode.Exit THEN
      TYPECASE n OF
      | M3AST_AS.EXP(exp) =>
          exp_copy := NARROW(ncopy, M3AST_AS.EXP);
          exp_copy.sm_exp_type_spec := exp.sm_exp_type_spec;
          exp_copy.sm_exp_value := exp.sm_exp_value;
      | M3AST_AS.TYPE_SPEC(type_spec) =>
          type_spec_copy := NARROW(ncopy, M3AST_AS.TYPE_SPEC);
          type_spec_copy.sm_bitsize := type_spec.sm_bitsize;
          type_spec_copy.sm_align := type_spec.sm_align;
      | M3AST_AS.TYPED_ID(typed_id) =>
          typed_id_copy := NARROW(ncopy, M3AST_AS.TYPED_ID);
          typed_id_copy.sm_type_spec := typed_id.sm_type_spec;
      | M3AST_AS.UNIT(unit) =>
          unit_copy := NARROW(ncopy, M3AST_AS.UNIT);
          unit_copy.sm_comp_unit := unit.sm_comp_unit;
          TYPECASE unit OF <*NOWARN*>
          | M3AST_AS.UNIT_WITH_BODY(unit_wb) =>
              unit_copy_wb := NARROW(unit_copy, M3AST_AS.UNIT_WITH_BODY);
              unit_copy_wb.sm_import_s := unit_wb.sm_import_s;
              unit_copy_wb.sm_type_spec_s := unit_wb.sm_type_spec_s;
              unit_copy_wb.sm_reveal_s := unit_wb.sm_reveal_s; 
          | M3AST_AS.UNIT_GEN_INS(unit_gen_ins) =>
              VAR unit_gen_ins_copy :=
                  NARROW(unit_copy, M3AST_AS.UNIT_GEN_INS);
              BEGIN
                unit_gen_ins_copy.sm_ins_comp_unit :=
                  unit_gen_ins.sm_ins_comp_unit;
              END;
          END; (* typecase *)
     | M3AST_AS.UNIT_ID(unit_id) =>
          unit_id_copy := NARROW(ncopy, M3AST_AS.UNIT_ID);
          unit_id_copy.sm_spec := unit_id.sm_spec;
      ELSE
      END; (* typecase *)

      IF n.IsA_INIT_ID(init_id) THEN
        M3Assert.Check(ncopy.IsA_INIT_ID(init_id_copy));
        init_id_copy.sm_init_exp := init_id.sm_init_exp;
      END; 
      IF n.IsA_CCV_ID(ccv_id) THEN
        M3Assert.Check(ncopy.IsA_CCV_ID(ccv_id_copy));
        ccv_id_copy.sm_exp_value := ccv_id.sm_exp_value;
      END;
      IF n.IsA_REDEF_ID(redef_id) THEN
        M3Assert.Check(ncopy.IsA_REDEF_ID(redef_id_copy));
        redef_id_copy.sm_int_def := redef_id.sm_int_def;
      END;
      IF n.IsA_RECOBJ_ID(recobj_id) THEN
        M3Assert.Check(ncopy.IsA_RECOBJ_ID(recobj_id_copy));
        recobj_id_copy.sm_enc_type_spec := recobj_id.sm_enc_type_spec;
      END;
      IF n.IsA_USED_ID(used_id) THEN
        M3Assert.Check(ncopy.IsA_USED_ID(used_id_copy));
        used_id_copy.sm_def := used_id.sm_def;
      END; (* if *)
      
      TYPECASE n OF
      | M3AST_AS.Named_type(named_type) =>
          named_type_copy := NARROW(ncopy, M3AST_AS.Named_type);
          named_type_copy.sm_type_spec := named_type.sm_type_spec;
      | M3AST_AS.Subrange_type(subrange_type) =>
          subrange_type_copy := NARROW(ncopy, M3AST_AS.Subrange_type);
          subrange_type_copy.sm_base_type_spec := 
              subrange_type.sm_base_type_spec;
      | M3AST_AS.Enumeration_type(enumeration_type) =>
          enumeration_type_copy := NARROW(ncopy, M3AST_AS.Enumeration_type);
          enumeration_type_copy.sm_num_elements := 
              enumeration_type.sm_num_elements;
      | M3AST_AS.Array_type(array_type) =>
          array_type_copy := NARROW(ncopy, M3AST_AS.Array_type);
          array_type_copy.sm_norm_type := array_type.sm_norm_type;
      | M3AST_AS.Opaque_type(opaque_type) =>
          opaque_type_copy := NARROW(ncopy, M3AST_AS.Opaque_type);
          opaque_type_copy.sm_concrete_type_spec := 
             opaque_type.sm_concrete_type_spec;
          opaque_type_copy.sm_type_spec_s :=
             opaque_type.sm_type_spec_s;
      | M3AST_AS.Object_type(object_type) =>
          object_type_copy := NARROW(ncopy, M3AST_AS.Object_type);
          object_type_copy.sm_rf_bitsize := object_type.sm_rf_bitsize;
          object_type_copy.sm_rf_align := object_type.sm_rf_align;
      | M3AST_AS.Procedure_type(procedure_type) =>
          procedure_type_copy := NARROW(ncopy, M3AST_AS.Procedure_type);
          procedure_type_copy.sm_def_id := procedure_type.sm_def_id;
      | M3AST_AS.Module(module) =>
          module_copy := NARROW(ncopy, M3AST_AS.Module);
          module_copy.sm_export_s := module.sm_export_s;
      | M3AST_AS.Proc_id(proc_id) =>
          proc_id_copy := NARROW(ncopy, M3AST_AS.Proc_id);
          proc_id_copy.sm_spec := proc_id.sm_spec;
          proc_id_copy.sm_concrete_proc_id := proc_id.sm_concrete_proc_id;
      | M3AST_AS.Call(call) =>
          call_copy := NARROW(ncopy, M3AST_AS.Call);
          call_copy.sm_actual_s := call.sm_actual_s;
      | M3AST_AS.Constructor(constructor) =>
          constructor_copy := NARROW(ncopy, M3AST_AS.Constructor);
          constructor_copy.sm_actual_s := constructor.sm_actual_s
      ELSE
      END;
    END; (* if *)
  END Attrs;

BEGIN

END M3ASTCopySM.
