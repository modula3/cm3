MODULE M3CNEWActualS;

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

IMPORT M3AST_LX,M3AST_AS, M3AST_SM, M3ASTNext;

IMPORT M3AST_AS_F, M3AST_SM_F;

IMPORT SeqM3AST_AS_EXP;

IMPORT M3Error, M3CTypesMisc, M3CExpsMisc, M3CTypeChkUtil, M3CActualUtil,
    M3COrdinal;


PROCEDURE NewOpenArray(
    call: M3AST_AS.Call;
    actuals: M3CActualUtil.List;
    array: M3AST_AS.Array_type)
    RAISES {}=
  VAR
    norm: M3AST_AS.Array_type;
    exp: M3AST_AS.EXP;
    base, elementType: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    norm := array.sm_norm_type;
    FOR pos := 1 TO M3CActualUtil.PositionalActuals(actuals) - 1 DO
      IF norm = NIL THEN
        M3CActualUtil.TooManyArguments(call);
        RETURN;
      ELSE
        exp := M3CActualUtil.ActualAt(actuals, pos, NIL);
        IF exp # NIL THEN
          IF ISTYPE(exp, M3AST_SM.TypeActual) THEN
            M3CExpsMisc.WrongClass(M3CActualUtil.OriginalActual(call, pos),
                M3CExpsMisc.Class.Type);
          ELSE
            SeqM3AST_AS_EXP.AddRear(call.sm_actual_s, exp);
            IF M3COrdinal.Is(exp.sm_exp_type_spec, base) AND
                (base = NIL OR NOT ISTYPE(base, M3AST_AS.Enumeration_type)) THEN
              (* i.e. is INTEGER or unset meaning ok or previous error *)
            ELSE
              M3CActualUtil.ArgumentIsWrongType(exp);
            END;
          END;
        ELSE
          (* actual was wrong class; error message already given *)
        END;
        M3CTypesMisc.GetTYPE_SPECFromM3TYPE(
            norm.as_elementtype, elementType);
        IF M3CTypesMisc.IsOpenArray(elementType) THEN
          norm := NARROW(elementType, M3AST_AS.Array_type).sm_norm_type;
        ELSE
          norm := NIL;
        END; (* if *)
      END; (* if 'norm = NIL' *)
    END; (* for *)
    IF norm # NIL THEN M3CActualUtil.TooFewArguments(call) END;
  END NewOpenArray;


TYPE
  FieldAndMethodIterRec = RECORD
    record: M3AST_AS.Record_type; (* one of 'record' and 'object' is NIL *)
    object: M3AST_AS.Object_type;
    iterR: M3ASTNext.IterField;
    iterO: M3ASTNext.IterFieldOrMethod;
    field_id: M3AST_AS.Field_id;
  END; (* record *)


PROCEDURE InitFieldAndMethodIter(
    o: M3AST_AS.Object_type;
    r: M3AST_AS.Record_type;
    VAR i: FieldAndMethodIterRec)
    RAISES {}=
  BEGIN
    i.object := o;
    i.record := r;
    IF r # NIL THEN
      i.iterR := M3ASTNext.NewIterField(r.as_fields_s);
    ELSE
      i.iterO := M3ASTNext.NewIterFieldOrMethod(o);
    END;
    i.field_id := NIL;
  END InitFieldAndMethodIter;


PROCEDURE CheckFieldActual(
    i: FieldAndMethodIterRec;
    exp: M3AST_AS.EXP;
    safe: BOOLEAN)
    RAISES {}=
  BEGIN
    IF exp # NIL THEN
        IF NOT M3CTypeChkUtil.EXPAssignable(
            i.field_id.sm_type_spec, exp, safe) THEN
          M3Error.Report(exp,
              "argument to NEW not assignable to record or object field");
        END; (* if *)
    ELSE
      (* was illegal use of type; error message already given *)
    END; (* if *)
  END CheckFieldActual;


PROCEDURE NextField(
    VAR i: FieldAndMethodIterRec)
    : BOOLEAN
    RAISES {}=
  VAR
    field_id: M3AST_AS.Field_id;
  BEGIN
    IF i.object # NIL  THEN
      VAR
        method: M3AST_AS.Method;
        symrep: M3AST_LX.Symbol_rep;
      BEGIN
        LOOP
          IF M3ASTNext.FieldOrMethod(i.iterO, field_id, method, symrep) THEN
            IF method = NIL THEN
              i.field_id := field_id;
              RETURN TRUE;
            END;
          ELSE
            RETURN FALSE;
          END;
        END;
      END;
    ELSE
      IF M3ASTNext.Field(i.iterR, field_id) THEN
        i.field_id := field_id;
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    END;      
  END NextField;


PROCEDURE NewObjectOrRecord(
    call: M3AST_AS.Call;
    actuals: M3CActualUtil.List;
    object: M3AST_AS.Object_type;
    record: M3AST_AS.Record_type;
    safe: BOOLEAN)
    RAISES {}=
  VAR
    iter: FieldAndMethodIterRec;
    exp: M3AST_AS.EXP;
  BEGIN
    InitFieldAndMethodIter(object, record, iter);

    IF M3CActualUtil.PositionalActuals(actuals) > 1 THEN
      M3Error.Report(call,
          "positional binding not allowed for field or method");
    END;

    (* See if there is a keyword actual for each field. If there is,
     use its expression.  If not, use the default. *)
    WHILE NextField(iter) DO
      IF M3CActualUtil.ActualByKeyword(actuals, iter.field_id, exp) THEN
        CheckFieldActual(iter, exp, safe);
      END; (* if *)
    END; (* while *)

  END NewObjectOrRecord;


PROCEDURE NewRef(
    call: M3AST_AS.Call;
    actuals: M3CActualUtil.List;
    ref: M3AST_AS.Ref_type;
    safe: BOOLEAN)
    RAISES {}=
  VAR
    referent: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(ref.as_type, referent);
    IF referent # NIL THEN
      IF NOT M3CTypesMisc.IsEmpty(referent) THEN
        IF M3CTypesMisc.IsOpenArray(referent) THEN
          NewOpenArray(call, actuals, referent);
        ELSIF ISTYPE(referent, M3AST_AS.Record_type) THEN
          NewObjectOrRecord(call, actuals, NIL, referent, safe);
        ELSE
          IF M3CActualUtil.PositionalActuals(actuals) > 1 THEN
            M3CActualUtil.TooManyArguments(call);
          END; (* if *)
        END; (* if *)
      ELSE
        M3Error.Report(call, "NEW of empty type");
      END; (* if *)
    ELSE
      (* referent is unset - forget it *)
    END; (* if *)
  END NewRef;


PROCEDURE SetAndTypeCheck(call: M3AST_AS.Call; safe: BOOLEAN) RAISES {}=
  VAR
    actuals := M3CActualUtil.ActualList(call, TRUE);
    exp: M3AST_AS.EXP;
    ts: M3AST_SM.TYPE_SPEC_UNSET := NIL;
  BEGIN
    IF M3CActualUtil.PositionalActuals(actuals) > 0 THEN
      exp := M3CActualUtil.ActualAt(actuals, 0, NIL);
      IF exp # NIL THEN
        VAR
          error: Text.T := NIL;
        BEGIN
          IF ISTYPE(exp, M3AST_SM.TypeActual) THEN
            SeqM3AST_AS_EXP.AddRear(call.sm_actual_s, exp);
            ts := M3CTypesMisc.Reveal(exp.sm_exp_type_spec);
            TYPECASE ts OF
            | NULL =>
            | M3AST_AS.Object_type(objectType) =>
                NewObjectOrRecord(call, actuals, objectType, NIL, safe);
            | M3AST_AS.Ref_type(refType) =>
                NewRef(call, actuals, refType, safe);
            | M3AST_AS.RefAny_type =>
                error := "cannot NEW an opaque type known only as <: REFANY";
            | M3AST_AS.Root_type =>
               IF M3CActualUtil.TotalActuals(actuals) > 1 THEN
                 M3CActualUtil.TooManyArguments(call);
               END;
            ELSE
              error := "first argument to NEW must be reference type";
            END; (* if *)
          ELSE
            error := "first argument to NEW must be a reference type";
          END;
          IF error # NIL THEN
            M3Error.Report(
                M3CActualUtil.OriginalActual(call, 0), error);
          END;
        END;
      ELSE
        (* argument was not a normal expression, error message already given *)
      END;
    ELSE
      IF M3CActualUtil.TotalActuals(actuals) = 0 THEN
        M3CActualUtil.TooFewArguments(call);
      ELSE
        M3Error.Report(call, "NEW must have at least one positional argument");
      END; (* if *)
    END;
    IF ts # NIL THEN M3CActualUtil.FindUnmatched(actuals) END;
  END SetAndTypeCheck;


BEGIN
END M3CNEWActualS.
