MODULE M3CTypeChkUtil;

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

(* To do:
-- Can texts screw up simple assumptions about checking whether a constant
-- expression is a member of a type?
*)

IMPORT M3AST_AS, M3AST_SM;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_DECL_REVL;

IMPORT M3CTypeRelation, M3CTypesMisc, M3CStdTypes, M3CExpsMisc,
    M3COrdinal, M3CStdProcs;


PROCEDURE IsBoolean(type: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {}=
  BEGIN
    RETURN M3CTypeRelation.Identical(
      M3CTypesMisc.CheckedUnpack(type), M3CStdTypes.Boolean());
  END IsBoolean;


PROCEDURE IsSubTypeOfInteger(
    type: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    RETURN M3CTypeRelation.SubType(type, M3CStdTypes.Integer());
  END IsSubTypeOfInteger;


PROCEDURE IsSubTypeOfBoolean(
    type: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    RETURN M3CTypeRelation.SubType(type, M3CStdTypes.Boolean());
  END IsSubTypeOfBoolean;


PROCEDURE IsSubTypeOfCardinal(
    type: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    RETURN M3CTypeRelation.SubType(type, M3CStdTypes.Cardinal());
  END IsSubTypeOfCardinal;


PROCEDURE IsSubTypeOfText(type: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {}=
  BEGIN
    RETURN M3CTypeRelation.SubType(type, M3CStdTypes.Text());
  END IsSubTypeOfText;


PROCEDURE IsSubTypeOfRefany(
    type: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    RETURN M3CTypeRelation.SubType(type, M3CStdTypes.RefAny());
  END IsSubTypeOfRefany;


PROCEDURE IsSubTypeOfAddress(
    type: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    RETURN M3CTypeRelation.SubType(type, M3CStdTypes.Address());
  END IsSubTypeOfAddress;


PROCEDURE IsTopLevel(id: M3AST_AS.Proc_id): BOOLEAN RAISES {}=
  VAR
    i: SeqM3AST_AS_DECL_REVL.Iter;
    decl: M3AST_AS.DECL_REVL;
    unit_id := NARROW(id.tmp_unit_id, M3AST_AS.UNIT_ID);
  BEGIN
    i := SeqM3AST_AS_DECL_REVL.NewIter(
        NARROW(unit_id.sm_spec, M3AST_AS.UNIT_NORMAL).as_block.as_decl_s);
    WHILE SeqM3AST_AS_DECL_REVL.Next(i, decl) DO
      TYPECASE decl OF
      | M3AST_AS.Proc_decl(procDecl) =>
          IF procDecl.as_id = id THEN RETURN TRUE END;
      ELSE
      END; (* if *)
    END; (* while *)
    RETURN FALSE;
  END IsTopLevel;


PROCEDURE ClassifyProc(exp: M3AST_AS.EXP): Proc RAISES {}=
  VAR
    def: M3AST_SM.DEF_ID_UNSET;
    pf: M3CStdProcs.T;
  BEGIN
    TYPECASE exp.sm_exp_type_spec OF
    | NULL =>
    | M3AST_AS.Procedure_type(pt) =>
        def := pt.sm_def_id;
        TYPECASE def OF <*NOWARN*>
        | NULL => (* a procedure type declaration *)
        | M3AST_AS.Proc_id(procId) =>
          IF M3CStdProcs.IsStandard(procId, pf) THEN
            RETURN Proc.Standard;
          ELSIF IsTopLevel(def) THEN
            RETURN Proc.TopLevel;
          ELSE
            RETURN Proc.Nested;
          END;
        | M3AST_AS.METHOD_OVERRIDE_ID, M3AST_AS.Type_id =>
            RETURN Proc.Method;
        END;
    ELSE (* not a procedure *)
    END;
    RETURN Proc.Variable;
  END ClassifyProc;


(* A note on the EXPAssignable procedure. If 'exp' is a constant it also checks
that the constant is a member of 'type'. This check looks too simple at first -
if 'type' is a subrange and 'exp' is constant then check that 'exp' is within
the bounds of the subrange. What about the other cases, when 'type' is not a
subrange? Well remember that the assignability of 'exp' to 'type' has already
been checked; that rules out a lot of weird cases. The assignment rule means
that we can assert:

  a)  E <: T OR
  b)  E is a ref type or array type and T <: E OR
  c)  E and T are ordinal types with at least one member in common

where T is 'type' and E is the type of 'exp'. If E <: T then, by the definition
of subtyping, 'exp' must be a member of T. There are no ref constants
(caveat: what about Text) so we can ignore the ref part of clause (b). We can
also ignore the part of clause (b) referring to arrays because if an array
constant (i.e. a constructor) is assignable to T it will certainly also
be a member of T. This is because the values in the constructors are the
element type of the array (ultimately - there may be nested array constructors
for multi dimensional open arrays) and the element types of E and T must be
identical for E <: T or T <: E.
  So we are left with clause (c). Obviously if T is a base ordinal (i.e. an
enumeration or INTEGER) then 'exp' must be a member in order to be assignable.
Hence subranges are the only case we worry about *)

PROCEDURE EXPAssignable(
    type: M3AST_SM.TYPE_SPEC_UNSET;
    exp: M3AST_AS.EXP;
    safe: BOOLEAN)
    : BOOLEAN
    RAISES {}=
  CONST
    PossiblyTopLevel =
        ProcSet{Proc.TopLevel, Proc.Method, Proc.Variable};
  VAR
    unpacked: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF NOT M3CTypeRelation.Assignable(type, exp.sm_exp_type_spec, safe) THEN
      RETURN FALSE;
    END;
    unpacked := M3CTypesMisc.CheckedUnpack(type);
    TYPECASE unpacked OF
    | NULL =>
    | M3AST_AS.Subrange_type(subrangeType) =>
        IF exp.sm_exp_value # NIL THEN
          RETURN M3COrdinal.IsMemberOf(subrangeType, exp);
        END;
    | M3AST_AS.Procedure_type =>
        RETURN ClassifyProc(exp) IN PossiblyTopLevel;
    ELSE
    END; (* if *)
    RETURN TRUE;
  END EXPAssignable;


PROCEDURE IsExpectedClass(
    exp: M3AST_AS.EXP;
    classes: M3CExpsMisc.ClassSet)
    : BOOLEAN
    RAISES {}=
  VAR
    class := M3CExpsMisc.Classify(exp);
  BEGIN
    IF class IN classes THEN
      RETURN TRUE;
    ELSE
      M3CExpsMisc.WrongClass(exp, class);
      RETURN FALSE;
    END; (* if *)
  END IsExpectedClass;


PROCEDURE IsNormalEXP(exp: M3AST_AS.EXP): BOOLEAN RAISES {}=
  BEGIN
    RETURN IsExpectedClass(exp, M3CExpsMisc.NormalOnly);
  END IsNormalEXP;


BEGIN
END M3CTypeChkUtil.
