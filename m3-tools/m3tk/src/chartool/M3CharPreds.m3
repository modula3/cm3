(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

MODULE M3CharPreds;

IMPORT AST, ASTWalk;
IMPORT M3AST_AS;
IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;
IMPORT M3CStdProcs, M3CStdTypes, M3CTypesMisc, M3ASTNext;
IMPORT SeqM3AST_AS_EXP, SeqM3AST_AS_M3TYPE;
IMPORT SeqM3AST_AS_Fields, SeqM3AST_AS_Field_id;
IMPORT SeqM3AST_AS_Override, SeqM3AST_AS_Method;
IMPORT SeqM3AST_AS_FORMAL_ID, SeqM3AST_AS_Formal_param;
IMPORT M3Error, M3CSrcPos, M3CGo;

PROCEDURE Es (e: M3AST_AS.EXP): BOOLEAN RAISES {} =
  BEGIN
    TYPECASE e OF
    | M3AST_AS.Call (call) =>
        VAR st_call: M3CStdProcs.T;
        BEGIN
          IF M3CStdProcs.IsStandardCall(call, st_call) THEN
            CASE st_call OF
            | M3CStdProcs.T.First, M3CStdProcs.T.Last,
                M3CStdProcs.T.Number =>
                WITH ta = SeqM3AST_AS_EXP.First(call.sm_actual_s) DO
                  RETURN Tn(ta.sm_exp_type_spec); END;

            | M3CStdProcs.T.BitSize, M3CStdProcs.T.ByteSize,
                M3CStdProcs.T.AdrSize =>
                WITH ta = SeqM3AST_AS_EXP.First(call.sm_actual_s) DO
                  RETURN TC(ta.sm_exp_type_spec, Ts); END;
            ELSE END; END;
        END;
    ELSE END;
    RETURN FALSE;
  END Es;

TYPE
  EcsClosure = ASTWalk.Closure OBJECT
                 ecs := FALSE;
               OVERRIDES
                 callback := EcsHelper; END;

PROCEDURE Ecs (e: M3AST_AS.EXP): BOOLEAN =
  <*FATAL ANY*>
  BEGIN
    WITH cl = NEW(EcsClosure) DO
      EVAL cl.init();
      ASTWalk.VisitNodes(e, cl);
      RETURN cl.ecs;
    END;
  END Ecs;

PROCEDURE EcsHelper (            cl: EcsClosure;
                                 n : AST.NODE;
                     <*UNUSED *> vm: ASTWalk.VisitMode)
  RAISES {ASTWalk.Aborted} =
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Exp_used_id (e) =>
        WITH def = e.vUSED_ID.sm_def DO
          TYPECASE def OF
          | M3AST_AS.Const_id (c) =>
              IF Ecs(c.vINIT_ID.sm_init_exp) THEN
                cl.ecs := TRUE;
                ASTWalk.Abort() END;
          ELSE END;              (* typecase *)
          END;                   (* with *)
    | M3AST_AS.TYPE_SPEC => ASTWalk.IgnoreChildren(cl);
    | M3AST_AS.EXP (e) =>
        IF Es(e) THEN cl.ecs := TRUE; ASTWalk.Abort() END;
    ELSE END;                    (* typecase *)
  END EcsHelper;


PROCEDURE Tn (ts: M3AST_AS.TYPE_SPEC): BOOLEAN =
  BEGIN
    TYPECASE ts OF
    | M3AST_AS.Enumeration_type (x) =>
        RETURN x = M3CStdTypes.Char();

    | M3AST_AS.Array_type (x) => RETURN ArrayTnOf(x);

    | M3AST_AS.Subrange_type (x) =>
        RETURN Ecs(x.as_range.as_exp1) OR Ecs(x.as_range.as_exp2);
    ELSE END;                    (* typecase *)
    RETURN FALSE;
  END Tn;

PROCEDURE ArrayTnOf (ts: M3AST_AS.TYPE_SPEC): BOOLEAN =
  BEGIN
    TYPECASE ts OF
    | M3AST_AS.Array_type (x) =>
        VAR
          iter := SeqM3AST_AS_M3TYPE.NewIter(x.as_indextype_s);
          y: M3AST_AS.M3TYPE;
        BEGIN
          WHILE SeqM3AST_AS_M3TYPE.Next(iter, y) DO
            IF Tn(M3TYPE_To_TYPE_SPEC(y)) THEN RETURN TRUE END; END; (* while *)
        END;
    ELSE END;                    (* typecase *)
    RETURN FALSE;
  END ArrayTnOf;

PROCEDURE Th (ts: M3AST_AS.TYPE_SPEC): BOOLEAN RAISES {} =
  BEGIN
    TYPECASE ts OF
    | M3AST_AS.Array_type (x) => RETURN ArrayTnOf(x);

    | M3AST_AS.Set_type (x) =>
        RETURN Tn(M3TYPE_To_TYPE_SPEC(x.as_type));

    ELSE END;
    RETURN FALSE;
  END Th;

PROCEDURE Tr (ts: M3AST_AS.TYPE_SPEC): BOOLEAN RAISES {} =
  BEGIN
    TYPECASE ts OF
    | M3AST_AS.Array_type, M3AST_AS.Set_type => RETURN Th(ts);

    | M3AST_AS.Packed_type (x) =>
        RETURN
          Ecs(x.as_exp) OR TC(M3TYPE_To_TYPE_SPEC(x.as_type), Ts);
    ELSE END;
    RETURN FALSE;
  END Tr;

PROCEDURE Ts (ts: M3AST_AS.TYPE_SPEC): BOOLEAN RAISES {} =
  BEGIN
    RETURN Tn(ts) OR Tr(ts);
  END Ts;

PROCEDURE TC (ts: M3AST_AS.TYPE_SPEC; tp: PredTypeProc): BOOLEAN
  RAISES {} =
  BEGIN
    IF tp(ts) THEN
      RETURN TRUE
    ELSE
      TYPECASE ts OF
      | M3AST_AS.Array_type (x) =>
          RETURN TC(M3TYPE_To_TYPE_SPEC(x.as_elementtype), tp);

      | M3AST_AS.Packed_type (x) =>
          RETURN TC(M3TYPE_To_TYPE_SPEC(x.as_type), tp);

      | M3AST_AS.Record_type (x) =>
          VAR
            iter := M3ASTNext.NewIterField(x.as_fields_s);
            field_id: M3AST_AS.Field_id;
          BEGIN
            WHILE M3ASTNext.Field(iter, field_id) DO
              IF TC(field_id.sm_type_spec, tp) THEN
                RETURN TRUE END; END;
          END;
      ELSE END;                  (* typecase *)
      END;                       (* if *)
    RETURN FALSE;
  END TC;

PROCEDURE TCO (ts: M3AST_AS.TYPE_SPEC; tp: PredTypeProc): BOOLEAN
  RAISES {} =
  VAR rts := M3CTypesMisc.Reveal(ts);
  BEGIN
    TYPECASE rts OF
    | NULL =>
        M3Error.ReportAtPos(
          M3CSrcPos.Null, "M3CTypesMisc.Reveal(...)=NIL");
    | M3AST_AS.Object_type (x) =>
        VAR
          iter := M3ASTNext.NewIterObjectField(x);
          field_id: M3AST_AS.Field_id;
        BEGIN
          WHILE M3ASTNext.ObjectField(iter, field_id) DO
            IF field_id.sm_type_spec = NIL THEN
              M3Error.ReportWithId(
                field_id, "No sm_type_spec for field %s", field_id.lx_symrep);
            ELSIF TC(field_id.sm_type_spec, tp) THEN
              RETURN TRUE END; END;
        END
    ELSE END;
    RETURN FALSE
  END TCO;

TYPE
  RefStack = REF RECORD
                   len : INTEGER               := 0;
                   elts: REF ARRAY OF REFANY;
                   next: RefStack              := NIL END;

PROCEDURE RefStack_Push (rs: RefStack; elt: REFANY) RAISES {} =
  BEGIN
    IF rs.len = NUMBER(rs.elts^) THEN
      WITH na = NEW(REF ARRAY OF REFANY, rs.len * 2) DO
        FOR i := 0 TO rs.len - 1 DO na[i] := rs.elts[i] END;
        rs.elts := na; END END;
    rs.elts[rs.len] := elt;
    INC(rs.len);
  END RefStack_Push;

PROCEDURE RefStack_Pop (rs: RefStack; elt: REFANY) RAISES {} =
  BEGIN
    IF rs.elts[rs.len - 1] # elt THEN
      M3Error.ReportAtPos(M3CSrcPos.Null, "RefStack_Pop fails")
    ELSE
      DEC(rs.len) END
  END RefStack_Pop;

PROCEDURE RefStack_Has (rs: RefStack; elt: REFANY): BOOLEAN
  RAISES {} =
  VAR elts := rs.elts;
  BEGIN
    FOR i := 0 TO rs.len - 1 DO
      IF elts[i] = elt THEN RETURN TRUE END; END;
    RETURN FALSE;
  END RefStack_Has;

VAR
  stackPool           := NEW(MUTEX);
  stacks   : RefStack := NIL;

PROCEDURE Grade (ts: M3AST_AS.TYPE_SPEC): Char_Grade RAISES {} =
  BEGIN
    IF NOT Tm(ts) THEN
      RETURN Char_Grade.No
    ELSIF TC(ts, Ts) THEN
      RETURN Char_Grade.TcTs
    ELSE
      RETURN Char_Grade.Td END
  END Grade;

PROCEDURE Tm (ts: M3AST_AS.TYPE_SPEC): BOOLEAN RAISES {} =
  VAR rs: RefStack;
  BEGIN
    LOCK stackPool DO
      IF stacks = NIL THEN
        rs := NEW(RefStack, elts := NEW(REF ARRAY OF REFANY, 100));
      ELSE
        rs := stacks;
        stacks := stacks.next END; END;
    TRY
      RETURN TmW(ts, rs)
    FINALLY
      LOCK stackPool DO rs.next := stacks; stacks := rs; END END
  END Tm;

PROCEDURE TmW (ts: M3AST_AS.M3TYPE; rs: RefStack): BOOLEAN
  RAISES {} =
  BEGIN
    IF RefStack_Has(rs, ts) THEN RETURN FALSE END;
    RefStack_Push(rs, ts);
    TRY
      TYPECASE ts OF
      | M3AST_AS.Named_type (x) => RETURN TmW(x.sm_type_spec, rs);
      | M3AST_AS.FLOAT_TYPE, M3AST_AS.Integer_type,
          M3AST_AS.Null_type, M3AST_AS.RefAny_type,
          M3AST_AS.Address_type, M3AST_AS.Root_type =>
          RETURN FALSE;
      | M3AST_AS.Packed_type (x) =>
          RETURN Ecs(x.as_exp) OR TmW(x.as_type, rs);
      | M3AST_AS.Array_type (x) =>
          RETURN
            TmW(x.as_elementtype, rs)
              OR SeqTmW(x.as_indextype_s, rs);
      | M3AST_AS.Enumeration_type (x) =>
          RETURN x = M3CStdTypes.Char();
      | M3AST_AS.Set_type (x) => RETURN TmW(x.as_type, rs);
      | M3AST_AS.Subrange_type (x) =>
          IF x.sm_base_type_spec # NIL
               AND TmW(x.sm_base_type_spec, rs) THEN
            RETURN TRUE END;
          RETURN
            Ecs(x.as_range.as_exp1) OR Ecs(x.as_range.as_exp2);
      | M3AST_AS.Record_type (x) =>
          RETURN FieldsTmW(x.as_fields_s, rs);
      | M3AST_AS.Ref_type (x) => RETURN TmW(x.as_type, rs);
      | M3AST_AS.Object_type (x) =>
          IF x.as_ancestor # NIL AND TmW(x.as_ancestor, rs) THEN
            RETURN TRUE END;
          IF FieldsTmW(x.as_fields_s, rs) THEN RETURN TRUE END;
          IF MethodsTmW(x.as_method_s, rs) THEN RETURN TRUE END;
          IF OverridesTmW(x.as_override_s, rs) THEN
            RETURN TRUE END;
          RETURN FALSE;
      | M3AST_AS.Procedure_type (x) =>
          IF x.as_result_type # NIL AND TmW(x.as_result_type, rs) THEN
            RETURN TRUE END;
          IF FormalsTmW(x.as_formal_param_s, rs) THEN
            RETURN TRUE END;
          RETURN FALSE;
      | M3AST_AS.Opaque_type (x) => RETURN TmW(x.as_type, rs);
      | M3AST_AS.TYPE_SPEC(x) =>
          ReportInUnit(x.tmp_unit_id, ts, "Unexpected case of TYPE_SPEC");
          RETURN FALSE;
      ELSE
        M3Error.ReportAtPos(M3CSrcPos.Null, "Unexpected case of M3TYPE");
        RETURN FALSE END;
    FINALLY
      RefStack_Pop(rs, ts); END;
  END TmW;

PROCEDURE SeqTmW (seq: SeqM3AST_AS_M3TYPE.T; rs: RefStack): BOOLEAN
  RAISES {} =
  VAR i := SeqM3AST_AS_M3TYPE.NewIter(seq);
  VAR t: M3AST_AS.M3TYPE;
  BEGIN
    WHILE SeqM3AST_AS_M3TYPE.Next(i, t) DO
      IF TmW(t, rs) THEN RETURN TRUE END; END;
    RETURN FALSE;
  END SeqTmW;

PROCEDURE FieldsTmW (seq: SeqM3AST_AS_Fields.T; rs: RefStack):
  BOOLEAN RAISES {} =
  VAR i := SeqM3AST_AS_Fields.NewIter(seq);
  VAR j: SeqM3AST_AS_Field_id.Iter;
  VAR fs: M3AST_AS.Fields;
  VAR fid: M3AST_AS.Field_id;
  BEGIN
    WHILE SeqM3AST_AS_Fields.Next(i, fs) DO
      j := SeqM3AST_AS_Field_id.NewIter(fs.as_id_s);
      WHILE SeqM3AST_AS_Field_id.Next(j, fid) DO
        IF fid.sm_type_spec # NIL AND TmW(fid.sm_type_spec, rs) THEN
          RETURN TRUE
        ELSE
          EXIT END END END;
    RETURN FALSE;
  END FieldsTmW;

PROCEDURE MethodsTmW (seq: SeqM3AST_AS_Method.T; rs: RefStack):
  BOOLEAN RAISES {} =
  VAR i := SeqM3AST_AS_Method.NewIter(seq);
  VAR m: M3AST_AS.Method;
  BEGIN
    WHILE SeqM3AST_AS_Method.Next(i, m) DO
      IF TmW(m.as_type, rs) THEN RETURN TRUE END; END;
    RETURN FALSE;
  END MethodsTmW;

PROCEDURE OverridesTmW (seq: SeqM3AST_AS_Override.T; rs: RefStack):
  BOOLEAN RAISES {} =
  VAR i := SeqM3AST_AS_Override.NewIter(seq);
  VAR o: M3AST_AS.Override;
  BEGIN
    WHILE SeqM3AST_AS_Override.Next(i, o) DO
      IF o.as_id.sm_type_spec # NIL
           AND TmW(o.as_id.sm_type_spec, rs) THEN
        RETURN TRUE END; END;
    RETURN FALSE;
  END OverridesTmW;

PROCEDURE FormalsTmW (seq: SeqM3AST_AS_Formal_param.T; rs: RefStack):
  BOOLEAN RAISES {} =
  VAR i := SeqM3AST_AS_Formal_param.NewIter(seq);
  VAR j: SeqM3AST_AS_FORMAL_ID.Iter;
  VAR fp: M3AST_AS.Formal_param;
  VAR fid: M3AST_AS.FORMAL_ID;
  BEGIN
    WHILE SeqM3AST_AS_Formal_param.Next(i, fp) DO
      j := SeqM3AST_AS_FORMAL_ID.NewIter(fp.as_id_s);
      WHILE SeqM3AST_AS_FORMAL_ID.Next(j, fid) DO
        IF fid.sm_type_spec # NIL AND TmW(fid.sm_type_spec, rs) THEN
          RETURN TRUE
        ELSE
          EXIT END END (*do*) END (*do*);
    RETURN FALSE;
  END FormalsTmW;

PROCEDURE M3TYPE_To_TYPE_SPEC (m: M3AST_AS.M3TYPE):
  M3AST_AS.TYPE_SPEC =
  VAR ts: M3AST_AS.TYPE_SPEC;
  BEGIN
    IF m=NIL THEN RETURN NIL END;
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m, ts);
    RETURN ts;
  END M3TYPE_To_TYPE_SPEC;

PROCEDURE ReportInUnit(unit   : M3AST_AS.UNIT_ID;
                       n      : M3Error.ERROR_NODE;
                       message: TEXT) RAISES {} =
	BEGIN
	M3Error.SetCu(unit.sm_spec.sm_comp_unit);
	M3Error.Report(n, message);
	M3Error.SetCu(M3CGo.Current());
	END ReportInUnit;

BEGIN

END M3CharPreds.
