MODULE M3LExports;

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

IMPORT Text;
IMPORT AST, M3AST_AS, M3AST_SM, M3AST_PG, M3Context, M3CUnit;

IMPORT M3AST_AS_F, M3AST_PG_F, M3AST_SM_F;


IMPORT M3ASTWalk;

REVEAL
  Closure = Closure_public BRANDED OBJECT
  OVERRIDES
    callback := CheckUnit
  END;

TYPE
  WalkClosure = M3ASTWalk.Closure OBJECT
    unit: M3AST_AS.Compilation_Unit;
    ccl: Closure;
  OVERRIDES
    callback := CheckNode;
  END;

PROCEDURE Check(c: M3Context.T; cl: Closure) RAISES {}=
  <*FATAL ANY*>
  BEGIN
    M3Context.Apply(c, cl, findStandard := FALSE);
  END Check;

PROCEDURE CheckUnit(cl: Closure; ut: M3CUnit.Type; <*UNUSED*> name: Text.T; 
    cu: M3AST_AS.Compilation_Unit) RAISES {}=
  <*FATAL ANY*>
  BEGIN
    IF ut = M3CUnit.Type.Interface THEN
      M3ASTWalk.VisitNodes(cu, NEW(WalkClosure, unit := cu, ccl := cl));
    END; (* if *)
  END CheckUnit;

PROCEDURE CheckNode(wcl: WalkClosure; 
    an: AST.NODE; <*UNUSED*> vm: M3ASTWalk.VisitMode) RAISES {}=
  VAR 
    ts: M3AST_SM.TYPE_SPEC_UNSET;
    external_id: M3AST_PG.EXTERNAL_ID;
  BEGIN
    IF M3AST_PG.IsA_EXTERNAL_ID(an, external_id) THEN
      IF external_id.pg_external = NIL THEN
        TYPECASE an OF
        | M3AST_AS.Proc_id(proc_id) =>
            IF proc_id.sm_concrete_proc_id = NIL THEN
              wcl.ccl.report(wcl.unit, an);
            END; (* if *)

        | M3AST_AS.Type_id(type_id) =>
            ts := type_id.sm_type_spec;
            TYPECASE ts OF
            | M3AST_AS.Opaque_type(opaque_type) =>
              IF opaque_type.sm_concrete_type_spec = NIL THEN
                wcl.ccl.report(wcl.unit, an);
              END; (* if *)
            ELSE (* who cares *)
            END; (* typecase *)
        ELSE
        END; (* typecase *)
      END; (* if *)
    END; (* if *)
  END CheckNode;

BEGIN
END M3LExports.
