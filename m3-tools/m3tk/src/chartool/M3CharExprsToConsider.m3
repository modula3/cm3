(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

MODULE M3CharExprsToConsider;

IMPORT AST, ASTWalk;
IMPORT M3AST_AS, M3AST_PG;
IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_PG_F;
IMPORT SeqM3AST_AS_EXP;
IMPORT M3CStdProcs;
IMPORT M3Error;
IMPORT M3CharPreds;

TYPE
  BitStack = REF RECORD
                   head: AST.NODE;
                   tail: BitStack   END;
    (* head=NIL is where Es warnings should be skipped *)

PROCEDURE Cons (head: AST.NODE; tail: BitStack): BitStack
  RAISES {} =
  BEGIN
    RETURN NEW(BitStack, head := head, tail := tail)
  END Cons;

REVEAL
  Handle = Public BRANDED OBJECT
             consider, distant: BOOLEAN;
             inType           : BitStack  := NIL
           OVERRIDES
             callback := Node; END;

PROCEDURE NewHandle (consider, distant: BOOLEAN): Handle
  RAISES {} =
  BEGIN
    RETURN
      NEW(Handle, consider := consider, distant := distant).init();
  END NewHandle;

PROCEDURE Node (h: Handle; n: AST.NODE; vm: ASTWalk.VisitMode)
  RAISES {} =
  PROCEDURE NoteUsedId (u: M3AST_AS.USED_ID) =
    VAR
      e: M3AST_PG.EXTERNAL_ID;
      d                       := u.sm_def;
    BEGIN
      IF d # NIL AND M3AST_PG.IsA_EXTERNAL_ID(d, e) THEN
        IF e # NIL AND e.pg_external # NIL THEN
          TYPECASE d OF
          | M3AST_AS.TYPED_ID (tid) =>
              IF tid.sm_type_spec # NIL
                   AND M3CharPreds.Tm(tid.sm_type_spec) THEN
                M3Error.WarnWithId(
                  n, "Use of EXTERNAL Tm item %s", u.lx_symrep) END
          ELSE END END END;
    END NoteUsedId;
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Subrange_type, M3AST_AS.Packed_type =>
        CASE vm OF
        | ASTWalk.VisitMode.Entry =>
            h.inType := Cons(NIL, h.inType);
        | ASTWalk.VisitMode.Exit =>
            IF h.inType # NIL AND h.inType.head = NIL THEN
              h.inType := h.inType.tail
            ELSE
              M3Error.Report(n, "inType broken") END (*if*); END (*case*);
    | M3AST_AS.TYPE_SPEC =>
        CASE vm OF
        | ASTWalk.VisitMode.Entry =>
            IF h.inType # NIL AND h.inType.head = NIL THEN
              h.inType := Cons(n, h.inType) END;
        | ASTWalk.VisitMode.Exit =>
            IF h.inType # NIL AND h.inType.head = n THEN
              h.inType := h.inType.tail END; END (*case*);
    ELSE END (*typecase*);
    IF vm = ASTWalk.VisitMode.Entry THEN
      TYPECASE n OF
      | M3AST_AS.Exp_used_id (u) => NoteUsedId(u.vUSED_ID);
      | M3AST_AS.USED_ID (u) => NoteUsedId(u);
      | M3AST_AS.Call (call) =>
          VAR
            st_call: M3CStdProcs.T;
            ta, tb : M3AST_AS.EXP;
            tat    : M3AST_AS.TYPE_SPEC;
          PROCEDURE Grade (ts: M3AST_AS.TYPE_SPEC):
            M3CharPreds.Char_Grade =
            VAR g: M3CharPreds.Char_Grade;
            BEGIN
              IF h.consider AND NOT h.distant THEN
                IF M3CharPreds.TC(ts, M3CharPreds.Ts) THEN
                  RETURN M3CharPreds.Char_Grade.TcTs
                ELSE
                  RETURN M3CharPreds.Char_Grade.No END END;
              g := M3CharPreds.Grade(ts);
              IF g = M3CharPreds.Char_Grade.TcTs AND NOT h.consider THEN
                g := M3CharPreds.Char_Grade.No END;
              RETURN g;
            END Grade;
          BEGIN
            IF NOT (h.inType # NIL AND h.inType.head = NIL) THEN
              IF M3CharPreds.Es(call) THEN
                M3Error.Warn(call, "Expr depends on NUM(CHAR)") END; END;
            IF M3CStdProcs.IsStandardCall(call, st_call) THEN
              ta := SeqM3AST_AS_EXP.First(call.sm_actual_s);
              tat := ta.sm_exp_type_spec;
              CASE st_call OF
              | M3CStdProcs.T.Subarray =>
                  IF M3CharPreds.Tr(tat) THEN
                    M3Error.Warn(
                      call, "SUBARRAY of a changing array"); END;

              | M3CStdProcs.T.Ord =>
                  IF ta.sm_exp_value = NIL AND M3CharPreds.Tn(tat) THEN
                    M3Error.Warn(
                      call,
                      "ORD(var in NUM(CHAR)-dependent type)"); END;

              | M3CStdProcs.T.Val =>
                  tb := SeqM3AST_AS_EXP.Ith(call.sm_actual_s, 1);
                  IF M3CharPreds.Tn(tb.sm_exp_type_spec) THEN
                    M3Error.Warn(
                      call, "VAL(..., NUM(CHAR)-dependent type)"); END;

              | M3CStdProcs.T.Loophole =>
                  CASE Grade(tat) OF
                  | M3CharPreds.Char_Grade.No =>
                  | M3CharPreds.Char_Grade.Td =>
                      M3Error.Warn(
                        call,
                        "LOOPHOLE from a type related to CHAR");
                  | M3CharPreds.Char_Grade.TcTs =>
                      M3Error.Warn(
                        call,
                        "LOOPHOLE from a CHAR-size-dependent type"); END (*case*);
                  tb := SeqM3AST_AS_EXP.Ith(call.sm_actual_s, 1);
                  CASE Grade(tb.sm_exp_type_spec) OF
                  | M3CharPreds.Char_Grade.No =>
                  | M3CharPreds.Char_Grade.Td =>
                      M3Error.Warn(
                        call, "LOOPHOLE to a type related to CHAR");
                  | M3CharPreds.Char_Grade.TcTs =>
                      M3Error.Warn(
                        call,
                        "LOOPHOLE to a CHAR-size-dependent type"); END (*case*);

              | M3CStdProcs.T.Adr =>
                  CASE Grade(tat) OF
                  | M3CharPreds.Char_Grade.No =>
                  | M3CharPreds.Char_Grade.Td =>
                      M3Error.Warn(
                        call, "ADR of a type related to CHAR");
                  | M3CharPreds.Char_Grade.TcTs =>
                      M3Error.Warn(
                        call, "ADR of a CHAR-size-dependent type"); END (*case*);

              ELSE END; END;
          END;

      ELSE END (* typecase *);

      END (* if *);

  END Node;
BEGIN

END M3CharExprsToConsider.
