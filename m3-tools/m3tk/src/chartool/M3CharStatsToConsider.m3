(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

MODULE M3CharStatsToConsider;

IMPORT AST, ASTWalk;
IMPORT M3AST_AS, M3AST_LX;
IMPORT M3AST_AS_F, M3AST_SM_F;
IMPORT M3AST_PG, M3AST_PG_F;
IMPORT SeqM3AST_AS_EXP, SeqM3AST_AS_Var_id, SeqM3AST_AS_Field_id;
IMPORT SeqM3AST_AS_Actual, SeqM3AST_AS_M3TYPE,
       SeqM3AST_AS_FORMAL_ID;
IMPORT M3Error, M3CStdProcs, M3ASTNext, M3Assert, M3CTypesMisc;
IMPORT M3CSrcPos;
IMPORT M3CharPreds;

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

VAR
  stackPool           := NEW(MUTEX);
  stacks   : RefStack := NIL;

REVEAL
  Handle = Public BRANDED OBJECT
             results: RefStack
           OVERRIDES
             callback := Node; END;

PROCEDURE NewHandle (): Handle RAISES {} =
  VAR rs: RefStack;
  BEGIN
    LOCK stackPool DO
      IF stacks = NIL THEN
        rs := NEW(RefStack, elts := NEW(REF ARRAY OF REFANY, 100));
      ELSE
        rs := stacks;
        stacks := stacks.next END; END;
    TRY
      RETURN NEW(Handle, results := rs).init()
    FINALLY
      LOCK stackPool DO rs.next := stacks; stacks := rs; END END
  END NewHandle;

CONST noteDullDecls = FALSE;

PROCEDURE Node (h: Handle; n: AST.NODE; vm: ASTWalk.VisitMode)
  RAISES {} =
  VAR e: M3AST_PG.EXTERNAL_ID;
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Proc_decl (pd) =>
        CASE vm OF
        | ASTWalk.VisitMode.Entry =>
            RefStack_Push(h.results, pd.as_type.as_result_type);
        | ASTWalk.VisitMode.Exit =>
            RefStack_Pop(h.results, pd.as_type.as_result_type); END;
    ELSE END;
    IF vm = ASTWalk.VisitMode.Entry THEN
      IF n # NIL AND M3AST_PG.IsA_EXTERNAL_ID(n, e) THEN
        IF e # NIL AND e.pg_external # NIL THEN
          TYPECASE n OF
          | M3AST_AS.TYPED_ID (tid) =>
              IF tid.sm_type_spec # NIL
                   AND M3CharPreds.Tm(tid.sm_type_spec) THEN
                M3Error.WarnWithId(
                  n, "Decl of EXTERNAL Tm item %s", tid.lx_symrep)
              ELSIF noteDullDecls THEN
                M3Error.WarnWithId(
                  n, "Decl of EXTERNAL non-Tm item %s",
                  tid.lx_symrep) END
          ELSE END END END;

      TYPECASE n OF
      | M3AST_AS.NEWCall (call) =>
          IF call.as_param_s = NIL THEN
            M3Error.Report(call, "NIL call.as_param_s");
          ELSE
            VAR
              a0      := SeqM3AST_AS_Actual.First(call.as_param_s);
              rfcType := EXP_TYPE_To_TS(call, a0.as_exp_type);
              rfcTS := M3CTypesMisc.CheckedUnpack(
                         M3CharPreds.M3TYPE_To_TYPE_SPEC(rfcType));
              nOpen := CountOpen(rfcTS);
              iter_a := SeqM3AST_AS_Actual.NewIter(call.as_param_s);
              a_len := SeqM3AST_AS_Actual.Length(call.as_param_s);
              actual: M3AST_AS.Actual;
            BEGIN
              IF nOpen + 1 > a_len THEN
                M3Error.Warn(
                  call, "Fewer actuals than levels of open array");
                RETURN END;
              IF nOpen + 1 = a_len THEN RETURN END;
              FOR i := 0 TO nOpen DO
                IF NOT SeqM3AST_AS_Actual.Next(iter_a, actual) THEN
                  M3Error.Report(
                    call, "Alleged open actual fails to enumerate"); END; END;
              FOR i := nOpen + 1 TO a_len - 1 DO
                IF NOT SeqM3AST_AS_Actual.Next(iter_a, actual) THEN
                  M3Error.Report(
                    call,
                    "SeqM3AST_AS_Actual.Next exhausted before alleged length");
                  RETURN END;
                TYPECASE actual.as_exp_type OF
                | M3AST_AS.M3TYPE =>
                    M3Error.Report(actual, "Actual is a TYPE");
                | M3AST_AS.EXP (ae) =>
                    TYPECASE actual.as_id OF
                    | NULL =>
                        M3Error.Warn(actual, "Actual.as_id=NIL");
                    | M3AST_AS.Exp_used_id (e) =>
                        TYPECASE e.vUSED_ID.sm_def OF
                        | NULL =>
                            M3Error.Warn(
                              actual,
                              "actual.as_id.vUSED_ID.sm_def=NIL");
                        | M3AST_AS.Field_id (fid) =>
                            IF ae.sm_exp_type_spec # NIL THEN
                              CheckAssign(
                                actual, ae.sm_exp_type_spec,
                                fid.sm_type_spec); END
                        ELSE
                          M3Error.Warn(
                            actual,
                            "Weird actual.as_id.vUSED_ID.sm_def"); END
                    ELSE
                      M3Error.Warn(actual, "Weird actual.as_id"); END;
                ELSE END (*typecase*); END (*do*);
            END END;
      | M3AST_AS.Call (call) =>
          VAR st_call: M3CStdProcs.T;
          BEGIN
            IF M3CStdProcs.IsStandardCall(call, st_call) THEN
              CASE st_call OF
              | M3CStdProcs.T.Inc, M3CStdProcs.T.Dec =>
                  WITH ta = SeqM3AST_AS_EXP.First(call.sm_actual_s) DO
                    IF M3CharPreds.Tn(ta.sm_exp_type_spec) THEN
                      M3Error.Warn(
                        call,
                        "INC/DEC of a NUM(CHAR)-dependent type"); END; END;
              ELSE END;
            ELSE
              (* not a builtin call *)
              VAR
                iter_a := SeqM3AST_AS_EXP.NewIter(call.sm_actual_s);
                exp: M3AST_AS.EXP;
                proc_type := NARROW(
                               call.as_callexp.sm_exp_type_spec,
                               M3AST_AS.Procedure_type);
                iter_f       : M3ASTNext.IterFormal;
                hidden_formal: BOOLEAN;
                formal_param : M3AST_AS.Formal_param;
                formal_id    : M3AST_AS.FORMAL_ID;
                formal_type  : M3AST_AS.TYPE_SPEC;
                hidden_tid   : M3AST_AS.Type_id;
                formal_sym   : M3AST_LX.Symbol_rep;
                check_copy   : BOOLEAN;
              BEGIN
                IF proc_type = NIL THEN RETURN END;
                hidden_formal :=
                  proc_type.sm_def_id # NIL
                    AND ISTYPE(
                          proc_type.sm_def_id, M3AST_AS.Type_id);
                iter_f :=
                  M3ASTNext.NewIterFormal(
                    proc_type.as_formal_param_s);

                WHILE SeqM3AST_AS_EXP.Next(iter_a, exp) DO
                  IF hidden_formal THEN (* T.m *)
                    hidden_formal := FALSE;
                    hidden_tid :=
                      NARROW(proc_type.sm_def_id, M3AST_AS.Type_id);
                    formal_type := hidden_tid.sm_type_spec;
                    formal_sym := hidden_tid.lx_symrep;
                    check_copy := TRUE;
                  ELSE
                    M3Assert.Check(
                      M3ASTNext.Formal(
                        iter_f, formal_param, formal_id));
                    formal_sym := formal_id.lx_symrep;
                    formal_type := formal_id.sm_type_spec;
                    TYPECASE formal_id OF <*NOWARN*>
                    | M3AST_AS.F_Value_id => check_copy := TRUE;
                    | M3AST_AS.F_Var_id, M3AST_AS.F_Readonly_id =>
                        check_copy := FALSE; END; END; (* if *)
                  CheckAssign(
                    call, exp.sm_exp_type_spec, formal_type,
                    formal_sym, check_copy := check_copy); END; (* while *)
              END; END;
          END;

      | M3AST_AS.Raise_st (r) =>
          IF r.as_exp_void # NIL THEN
            TYPECASE r.as_qual_id.as_id.sm_def OF
            | NULL =>
                M3Error.Report(r, "r.as_qual_id.as_id.sm_def=NIL");
            | M3AST_AS.TYPED_ID (tid) =>
                CheckAssign(
                  r, r.as_exp_void.sm_exp_type_spec,
                  tid.sm_type_spec, check_copy := FALSE);
            ELSE
              M3Error.Report(r, "Weird r.as_qual_id.as_id.sm_def"); END END;

      | M3AST_AS.Handler_id (hid) =>
          IF hid.sm_type_spec # NIL THEN
            CheckAssign(hid, hid.sm_type_spec, hid.sm_type_spec);
          ELSE
            M3Error.Report(hid, "Unset Handler_id.sm_type_spec"); END;

      | M3AST_AS.Return_st (r) =>
          IF h.results.len = 0 THEN
            M3Error.Report(n, "RETURN outside PROC decl")
          ELSIF r.as_exp = NIL THEN
            EVAL 0
          ELSE
            TYPECASE h.results.elts[h.results.len - 1] OF
            | M3AST_AS.M3TYPE (t) =>
                CheckAssign(
                  r, r.as_exp.sm_exp_type_spec,
                  M3CharPreds.M3TYPE_To_TYPE_SPEC(t), check_copy := FALSE);
            ELSE
              M3Error.Report(r, "RETURN to non-M3TYPE") END END;

      | M3AST_AS.Var_decl (vd) =>
          IF vd.as_default # NIL THEN
            CheckAssign(
              vd, vd.as_default.sm_exp_type_spec,
              SeqM3AST_AS_Var_id.First(vd.as_id_s).sm_type_spec); END;

      | M3AST_AS.Formal_param (fp) =>
          VAR
            ff  := SeqM3AST_AS_FORMAL_ID.First(fp.as_id_s);
            fts := ff.sm_type_spec;
          BEGIN
            IF fp.as_default # NIL THEN
              CheckAssign(
                fp, fp.as_default.sm_exp_type_spec, fts,
                check_copy := FALSE); END;
            TYPECASE ff OF <*NOWARN*>
            | M3AST_AS.F_Value_id =>
                CheckAssign(fp, fts, fts, check_assy := FALSE);
            | M3AST_AS.F_Var_id, M3AST_AS.F_Readonly_id => EVAL 0; END;

          END;

      | M3AST_AS.Fields (fs) =>
          WITH fid = SeqM3AST_AS_Field_id.First(fs.as_id_s) DO
            TYPECASE fid.vRECOBJ_ID.sm_enc_type_spec OF
            | NULL =>
                M3Error.Report(
                  fs, "fid.vRECOBJ_ID.sm_enc_type_spec=NIL");
            | M3AST_AS.Record_type => EVAL 0;
            | M3AST_AS.Object_type =>
                IF M3CharPreds.TC(fid.sm_type_spec, M3CharPreds.Tr) THEN
                  M3Error.Warn(
                    fs, "Object field containing changing type") END;
            ELSE
              M3Error.Report(
                fs, "Weird fid.vRECOBJ_ID.sm_enc_type_spec") END;
            IF fs.as_default # NIL THEN
              CheckAssign(
                fs, fs.as_default.sm_exp_type_spec,
                fid.sm_type_spec); END; END;

      | M3AST_AS.Assign_st (as_st) =>
          CheckAssign(
            as_st, as_st.as_rhs_exp.sm_exp_type_spec,
            as_st.as_lhs_exp.sm_exp_type_spec);
      ELSE END;                  (* typecase *)
      END;                       (* if *)
  END Node;

PROCEDURE CheckAssign (n             : AST.NODE;
                       ts_from, ts_to: M3AST_AS.TYPE_SPEC;
                       formal: M3AST_LX.Symbol_rep := NIL;
                       check_assy, check_copy := TRUE) =
  BEGIN
    IF ts_from = NIL OR ts_to = NIL THEN
      M3Error.Report(n, "ts_from or ts_to is NIL")
    ELSE
      IF check_assy AND ts_from # ts_to
           AND (M3CharPreds.TC(ts_from, M3CharPreds.ArrayTnOf)
                  OR M3CharPreds.TC(ts_to, M3CharPreds.ArrayTnOf)) THEN
        IF formal # NIL THEN
          M3Error.WarnWithId(
            n,
            "assignment to/from NUM(CHAR)-dependent array at formal %s",
            formal);
        ELSE
          M3Error.Warn(
            n, "assignment to/from NUM(CHAR)-dependent array"); END; END; END;
    IF check_copy AND ts_to # NIL AND M3CharPreds.TC(ts_to, M3CharPreds.Th) THEN
      IF formal # NIL THEN
        M3Error.WarnWithId(
          n, "Copy of becoming-huge value at formal %s", formal);
      ELSE
        M3Error.Warn(n, "Copy of becoming-huge value"); END; END;
  END CheckAssign;

PROCEDURE CountOpen (ts: M3AST_AS.TYPE_SPEC): INTEGER =
  VAR rts := M3CTypesMisc.Reveal(ts);
  PROCEDURE PerArray (t: M3AST_AS.M3TYPE): INTEGER =
    VAR ts := M3CharPreds.M3TYPE_To_TYPE_SPEC(t);
    BEGIN
      TYPECASE ts OF
      | NULL => RETURN 0;
      | M3AST_AS.Array_type (at) =>
          WITH nat = at.sm_norm_type DO
            IF SeqM3AST_AS_M3TYPE.Empty(nat.as_indextype_s)
                 OR SeqM3AST_AS_M3TYPE.First(nat.as_indextype_s)
                      = NIL THEN
              RETURN 1 + PerArray(nat.as_elementtype) END; END
      ELSE END;
      RETURN 0;
    END PerArray;
  BEGIN
    TYPECASE rts OF
    | NULL => RETURN 0;
    | M3AST_AS.Ref_type (rt) => RETURN PerArray(rt.as_type);
    ELSE
      RETURN 0 END;
  END CountOpen;

PROCEDURE EXP_TYPE_To_TS (call: M3AST_AS.NEWCall;
                          et  : M3AST_AS.EXP_TYPE ):
  M3AST_AS.TYPE_SPEC =
  PROCEDURE UI (ui: M3AST_AS.USED_ID): M3AST_AS.TYPE_SPEC =
    BEGIN
      TYPECASE ui.sm_def OF
      | NULL => M3Error.Report(call, "ui.sm_def=NIL");
      | M3AST_AS.Type_id (tid) =>
          IF tid.sm_type_spec # NIL THEN
            RETURN tid.sm_type_spec
          ELSE
            M3Error.Report(call, "ui.sm_def._type_spec=NIL"); END;
      ELSE
        M3Error.Report(call, "Weird ui.sm_def") END;
      RETURN NIL;
    END UI;
  BEGIN
    TYPECASE et OF
    | M3AST_AS.M3TYPE (t) => RETURN M3CharPreds.M3TYPE_To_TYPE_SPEC(t);
    | M3AST_AS.Exp_used_id (eui) => RETURN UI(eui.vUSED_ID);
    | M3AST_AS.Select (b) =>
        TYPECASE b.as_id OF
        | NULL =>
            M3Error.Report(
              call, "1st arg to NEW() is Select(.., NIL)");
        | M3AST_AS.Exp_used_id (eui) => RETURN UI(eui.vUSED_ID);
        (*********************
        ELSE
          M3Error.Report(
            call, "1st arg to NEW() is Select(.., weird)")
        **********************)
        END;
        RETURN NIL;
    | M3AST_AS.Bad_EXP =>
        M3Error.Report(call, "1st arg to NEW() is Bad_EXP");
    | M3AST_AS.EXP =>
        M3Error.Report(call, "1st arg to NEW() is an expr");
    ELSE
      M3Error.Report(call, "Weird 1st arg to NEW()"); END;
    RETURN NIL;
  END EXP_TYPE_To_TS;

BEGIN

END M3CharStatsToConsider.
