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
(**)
(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3CTmpAtt EXPORTS M3CTmpAtt, M3ASTOp_SM;

IMPORT IntRefTbl;
IMPORT AST, M3AST_AS, M3ASTOp_SM;
IMPORT M3CId; (* to reveal an M3AST_LX.Symrep *)

IMPORT M3AST_AS_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_Override, SeqM3AST_AS_Method;

IMPORT ASTWalk;

CONST
  TmpDefIdTableSize = 64;

<*INLINE*> PROCEDURE Set(n: AST.NODE; id: M3AST_AS.UNIT_ID)=
  BEGIN
    n.set_tmp(id);
  END Set;

<*INLINE*> PROCEDURE SetTmpUnitId(n: AST.NODE; id: M3AST_AS.UNIT_ID)=
  BEGIN
    n.set_tmp_unit_id(id);
  END SetTmpUnitId;

<*INLINE*> PROCEDURE DefId(
    defId: M3AST_AS.DEF_ID;
    unitId: M3AST_AS.UNIT_ID)
    RAISES {}=
  BEGIN
    defId.tmp_unit_id := unitId;
    defId.tmp_recursive := FALSE;
  END DefId;


<*INLINE*> PROCEDURE TypeSpec(
    ts: M3AST_AS.TYPE_SPEC;
    id: M3AST_AS.UNIT_ID)
    RAISES {}=
  BEGIN
    ts.tmp_unit_id := id;
  END TypeSpec;


TYPE
  Closure =
    ASTWalk.Closure OBJECT
      table: IntRefTbl.T;
    OVERRIDES
      callback := RecordId;
    END;


PROCEDURE RecordId(
    c: Closure;
    an: AST.NODE;
    <*UNUSED*> vm: ASTWalk.VisitMode)
    RAISES {}=
  BEGIN
    TYPECASE an OF
    | M3AST_AS.DEF_ID(defId) =>
        IF defId.lx_symrep # NIL THEN
          EVAL c.table.put(defId.lx_symrep.atom, defId);
        END; (* if *)
    | M3AST_AS.M3TYPE, M3AST_AS.EXP =>
        ASTWalk.IgnoreChildren(c);
    ELSE
    END;
  END RecordId;


PROCEDURE SetTmpDefIdTable(int: M3AST_AS.Interface) RAISES {}=
  <*FATAL ANY*>
  VAR
    c: Closure;
    t: IntRefTbl.T;
  BEGIN
    t := NEW(IntRefTbl.Default).init(TmpDefIdTableSize);
    int.tmp_def_id_table := t;
    c := NEW(Closure, table := t);
    ASTWalk.VisitNodes(int.as_block, c);
  END SetTmpDefIdTable;


PROCEDURE Set_tmp(an: M3ASTOp_SM.NODE; id: M3AST_AS.UNIT_ID) RAISES {} =
  VAR
    iter_m: SeqM3AST_AS_Method.Iter;
    iter_o: SeqM3AST_AS_Override.Iter;
    override: M3AST_AS.Override;
    method: M3AST_AS.Method;
  BEGIN
    TYPECASE an OF
    | M3AST_AS.Import_item(im) =>
        IF im.as_id # NIL THEN
          im.as_id.tmp_used_id := im.as_intf_id;
        END; (* if *)
    | M3AST_AS.Concrete_decl(concreteDecl) =>
        concreteDecl.as_id.tmp_type := concreteDecl.as_type;
    | M3AST_AS.Subtype_decl(subtypeDecl) =>
        subtypeDecl.as_id.tmp_type := subtypeDecl.as_type;
    | M3AST_AS.Exc_decl(excDecl) =>
        excDecl.as_id.tmp_type := excDecl.as_type;
    | M3AST_AS.Object_type(objectType) =>
        iter_m := SeqM3AST_AS_Method.NewIter(objectType.as_method_s);
        WHILE SeqM3AST_AS_Method.Next(iter_m, method) DO
          method.tmp_type := objectType;
        END; (* while *)
        iter_o := SeqM3AST_AS_Override.NewIter(objectType.as_override_s);
        WHILE SeqM3AST_AS_Override.Next(iter_o, override) DO
          override.tmp_type := objectType;
        END; (* while *)
        TypeSpec(objectType, id);
    | M3AST_AS.Opaque_type(opaqueType) =>
        opaqueType.tmp_rev_type_spec := NIL;
        TypeSpec(opaqueType, id);
    | M3AST_AS.Interface(interface) =>
        SetTmpDefIdTable(interface);
    | M3AST_AS.DEF_ID(defId) =>
        DefId(defId, id);
    | M3AST_AS.TYPE_SPEC(typeSpec) =>
        TypeSpec(typeSpec, id);
    ELSE   
    END;
  END Set_tmp;


PROCEDURE Set_tmp_unit_id(an: M3ASTOp_SM.NODE; id: M3AST_AS.UNIT_ID) RAISES {}=
  BEGIN
    TYPECASE an OF
    | M3AST_AS.DEF_ID(defId) =>
        DefId(defId, id);
    | M3AST_AS.TYPE_SPEC(typeSpec) =>
        TypeSpec(typeSpec, id);
    ELSE
    END;
  END Set_tmp_unit_id;


BEGIN
END M3CTmpAtt.
