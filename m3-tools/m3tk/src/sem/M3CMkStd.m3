MODULE M3CMkStd;

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

IMPORT Text;
IMPORT AST, M3AST_AS;

IMPORT M3AST_LX_F, M3AST_AS_F, M3AST_SM_F;

IMPORT SeqM3AST_AS_TYPE_SPEC;

IMPORT M3Conventions;
IMPORT M3CId, M3CSrcPos;
IMPORT M3Assert;
IMPORT M3CStdTypes;
IMPORT M3CBitSize;
IMPORT ASTWalk;

VAR
  any_g, m3type_g: M3CId.T;


PROCEDURE CheckAnyOrType(VAR type: M3AST_AS.M3TYPE_NULL) RAISES {}=
  VAR
    sp: M3CSrcPos.T;
  BEGIN
    TYPECASE type OF
    | NULL =>
    | M3AST_AS.Named_type(t) =>
        WITH defId = t.as_qual_id.as_id DO
          IF defId.lx_symrep = any_g THEN
            sp := defId.lx_srcpos;
            type := M3CStdTypes.Any();
	    type.lx_srcpos := sp;
          ELSIF defId.lx_symrep = m3type_g THEN
            sp := defId.lx_srcpos;
            type := M3CStdTypes.Type();
	    type.lx_srcpos := sp;
          END;
        END;
    ELSE
    END;
  END CheckAnyOrType;


PROCEDURE TransStandard(
    <*UNUSED*> cl: ASTWalk.Closure;
    an: AST.NODE;
    <*UNUSED*> vm: ASTWalk.VisitMode)=
  BEGIN
    TYPECASE an OF
    | M3AST_AS.Procedure_type(procType) =>
        CheckAnyOrType(procType.as_result_type);
    | M3AST_AS.Formal_param(formalParam) =>
        CheckAnyOrType(formalParam.as_formal_type);
    ELSE
      (* not interesting *)
    END; (* case *)
  END TransStandard;


PROCEDURE TransForm(cu: M3AST_AS.Compilation_Unit) RAISES {}=
  TYPE TF_Closure = ASTWalk.Closure OBJECT OVERRIDES
      callback := TransStandard END;
  <*FATAL ANY*>
  BEGIN
    M3Assert.Check(
       ISTYPE(cu.as_root, M3AST_AS.Interface) AND
       Text.Equal(M3CId.ToText(
             NARROW(cu.as_root.as_id, M3AST_AS.Interface_id).lx_symrep),
             M3Conventions.Standard)
    );
    any_g := M3CId.Enter("ANY_TYPE");
    m3type_g := M3CId.Enter("M3TYPE");
    ASTWalk.VisitNodes(cu, NEW(TF_Closure).init());
  END TransForm;


PROCEDURE FindBuiltIns(
    <*UNUSED*> cl: ASTWalk.Closure;
    n: AST.NODE;
    <*UNUSED*> vm: ASTWalk.VisitMode)
    RAISES {}=
  VAR
    t: Text.T;
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Concrete_decl(concreteDecl) =>
        t := M3CId.ToText(concreteDecl.as_id.lx_symrep);
        IF Text.Equal(t, "CHAR") THEN
          M3CStdTypes.RegisterChar(concreteDecl.as_type);
        ELSIF Text.Equal(t, "BOOLEAN") THEN
          M3CStdTypes.RegisterBoolean(concreteDecl.as_type);
        ELSIF Text.Equal(t, "CARDINAL") THEN
          M3CStdTypes.RegisterCardinal(concreteDecl.as_type);
        END;
    | M3AST_AS.Subtype_decl(subtypeDecl) =>
        t := M3CId.ToText(subtypeDecl.as_id.lx_symrep);
        IF Text.Equal(t, "TEXT") THEN
          M3CStdTypes.RegisterText(subtypeDecl.as_type);
        ELSIF Text.Equal(t, "MUTEX") THEN
          M3CStdTypes.RegisterMutex(subtypeDecl.as_type);
        END; (* if *)
    ELSE
    END; (* if *)
  END FindBuiltIns;

PROCEDURE MakeBuiltInRefTypesGlobal(cu: M3AST_AS.Compilation_Unit) RAISES {}=
  VAR
    unit_wb := NARROW(cu.as_root, M3AST_AS.UNIT_WITH_BODY);
    iter := SeqM3AST_AS_TYPE_SPEC.NewIter(unit_wb.sm_type_spec_s);
    ns := SeqM3AST_AS_TYPE_SPEC.Null;
    ts: M3AST_AS.TYPE_SPEC;
  BEGIN
    (* copy existing list, removing Null etc if on it (unpickled), then
    add the values from M3CStdTypes back on. *)
    WHILE SeqM3AST_AS_TYPE_SPEC.Next(iter, ts) DO
      TYPECASE ts OF
      | M3AST_AS.Null_type, M3AST_AS.Root_type, M3AST_AS.RefAny_type =>
      ELSE
        SeqM3AST_AS_TYPE_SPEC.AddRear(ns, ts)    
      END; (* typecase *)
    END; (* while *)
    WITH ts = M3CStdTypes.Null() DO
      SeqM3AST_AS_TYPE_SPEC.AddRear(ns, ts)
    END;
    WITH ts = M3CStdTypes.RefAny() DO
      SeqM3AST_AS_TYPE_SPEC.AddRear(ns, ts)
    END;
    WITH ts = M3CStdTypes.Root() DO
      SeqM3AST_AS_TYPE_SPEC.AddRear(ns, ts)
    END;
    unit_wb.sm_type_spec_s := ns;
  END MakeBuiltInRefTypesGlobal;


PROCEDURE RegisterBuiltIns(cu: M3AST_AS.Compilation_Unit) RAISES {}=
  TYPE FBI_Closure = ASTWalk.Closure OBJECT
      OVERRIDES callback := FindBuiltIns END;
  <*FATAL ANY*>
  BEGIN
    ASTWalk.VisitNodes(cu, NEW(FBI_Closure).init());
    (* Now an nasty hack to add Root, Null and Refany to sm_type_spec_s. *)
    MakeBuiltInRefTypesGlobal(cu);
    (* Now initialise their sizes *) 
    M3CBitSize.Set(M3CStdTypes.Integer());
    M3CBitSize.Set(M3CStdTypes.Real());
    M3CBitSize.Set(M3CStdTypes.LongReal());
    M3CBitSize.Set(M3CStdTypes.Extended());
    M3CBitSize.Set(M3CStdTypes.RefAny());
    M3CBitSize.Set(M3CStdTypes.Address());
    M3CBitSize.Set(M3CStdTypes.Root());
    M3CBitSize.Set(M3CStdTypes.Untraced_Root());
    M3CBitSize.Set(M3CStdTypes.Null());
  END RegisterBuiltIns;


BEGIN
END M3CMkStd.
