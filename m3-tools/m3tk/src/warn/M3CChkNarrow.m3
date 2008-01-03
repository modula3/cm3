MODULE M3CChkNarrow;

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

IMPORT AST, M3AST_AS;

IMPORT M3AST_AS_F, M3AST_SM_F;

IMPORT ASTWalk;
IMPORT SeqM3AST_AS_EXP, SeqM3AST_AS_Var_id, M3ASTNext;
IMPORT M3Assert, M3Error, M3CStdProcs, M3CTypesMisc, M3CTypeRelation;

REVEAL
  Handle = ASTWalk.Closure BRANDED OBJECT
  OVERRIDES callback := Node;
  END; (* record *)

PROCEDURE Node(
    <*UNUSED*> h: Handle;
    n: AST.NODE;
    vm: ASTWalk.VisitMode)
    RAISES {}=
  BEGIN
    IF vm = ASTWalk.VisitMode.Entry THEN
      TYPECASE n OF
      | M3AST_AS.Call(call) =>
          VAR st_call: M3CStdProcs.T;
          BEGIN
            IF M3CStdProcs.IsStandardCall(call, st_call) THEN
            ELSE
              (* not a builtin call *)
              VAR iter_a := SeqM3AST_AS_EXP.NewIter(call.sm_actual_s);
                exp: M3AST_AS.EXP;
                proc_type := NARROW(call.as_callexp.sm_exp_type_spec,
                    M3AST_AS.Procedure_type);
                iter_f: M3ASTNext.IterFormal;
                hidden_formal: BOOLEAN;
                formal_param: M3AST_AS.Formal_param;
                formal_id: M3AST_AS.FORMAL_ID;
                formal_type: M3AST_AS.TYPE_SPEC;
              BEGIN
                IF proc_type = NIL THEN RETURN END;
                hidden_formal := proc_type.sm_def_id # NIL AND
                    ISTYPE(proc_type.sm_def_id, M3AST_AS.Type_id);
                iter_f := M3ASTNext.NewIterFormal(proc_type.as_formal_param_s);

                WHILE SeqM3AST_AS_EXP.Next(iter_a, exp) DO
                  IF hidden_formal THEN (* T.m *)
                    hidden_formal := FALSE;
                    formal_type := NARROW(proc_type.sm_def_id,
                        M3AST_AS.Type_id).sm_type_spec;
                  ELSE
                    M3Assert.Check(M3ASTNext.Formal(iter_f, formal_param,
                        formal_id));
                    formal_type := formal_id.sm_type_spec
                  END; (* if *)
                  CheckImplicitNarrow(exp, exp.sm_exp_type_spec,
                      formal_type);
                END; (* while *)
              END;
            END;
          END;

      | M3AST_AS.Var_decl(vd) =>
          IF vd.as_type # NIL AND vd.as_default # NIL THEN
            CheckImplicitNarrow(vd, vd.as_default.sm_exp_type_spec,
                SeqM3AST_AS_Var_id.First(vd.as_id_s).sm_type_spec);
          END; (* if *)

      | M3AST_AS.Assign_st(as_st) =>
          CheckImplicitNarrow(as_st, as_st.as_rhs_exp.sm_exp_type_spec,
              as_st.as_lhs_exp.sm_exp_type_spec);
      ELSE
      END;
    END;
  END Node;


PROCEDURE NewHandle(<*UNUSED*> cu: M3AST_AS.Compilation_Unit): Handle RAISES {}=
  BEGIN
    RETURN NEW(Handle);
  END NewHandle;

PROCEDURE CheckImplicitNarrow(
    node: AST.NODE;
    ts_from, ts_to: M3AST_AS.TYPE_SPEC)=
  CONST W = "implicit NARROW";
  BEGIN
    ts_from := M3CTypesMisc.Reveal(ts_from);
    ts_to := M3CTypesMisc.Reveal(ts_to);
    TYPECASE ts_to OF
    | M3AST_AS.Ref_type, M3AST_AS.Root_type =>
        IF ISTYPE(ts_from, M3AST_AS.RefAny_type) THEN
          M3Error.Warn(node, W);
        END; (* if *)

    | M3AST_AS.Object_type(ot_to) =>
        IF ISTYPE(ts_from, M3AST_AS.Root_type) OR
           ISTYPE(ts_from, M3AST_AS.RefAny_type) OR
           (NOT M3CTypeRelation.Identical(ts_from, ts_to) AND
            M3CTypeRelation.SubType(ot_to, ts_from)) THEN
          M3Error.Warn(node, W);
        END; (* if *)
    ELSE
    END; (* typecase *)
  END CheckImplicitNarrow;


BEGIN

END M3CChkNarrow.
