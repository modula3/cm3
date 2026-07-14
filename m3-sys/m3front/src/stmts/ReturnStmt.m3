(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ReturnStmt.m3                                         *)
(* Last modified on Fri Jun 24 12:30:34 PDT 1994 by kalsow     *)
(*      modified on Thu Dec  5 17:22:32 PST 1991 by muller     *)

MODULE ReturnStmt;

IMPORT Expr, Error, Type, AssignStmt, Token, Scanner;
IMPORT Variable, Marker, Stmt, StmtRep, ArrayExpr, Target, TInt;
IMPORT MSIR, MSIRBuilder, CaptureAnalysis;

TYPE
  P = Stmt.T OBJECT
        expr    : Expr.T;
      OVERRIDES
        check       := Check;
        compile     := Compile;
        outcomes    := GetOutcome;
        compileMSIR := CompileMSIR;
        capture  := Capture;
      END;

PROCEDURE Parse (): Stmt.T =
  VAR p := NEW (P);
  BEGIN
    StmtRep.Init (p);
    p.expr := NIL;
    Scanner.Match (Token.T.tRETURN);
    IF (Scanner.cur.token IN Token.ExprStart) THEN
      p.expr := Expr.Parse ();
    END;
    RETURN p;
  END Parse;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR t: Type.T;  v: Variable.T;
  BEGIN
    Expr.TypeCheck (p.expr, cs);
    IF NOT Marker.ReturnOK () THEN
      Error.Msg ("RETURN not in a procedure");
      RETURN ;
    END;
    Marker.ReturnVar (t, v);
    IF (p.expr = NIL) THEN
      IF (t # NIL) THEN Error.Msg ("missing return result") END;
    ELSIF (t = NIL) THEN
      Error.Msg ("procedure does not have a return result");
    ELSE
      ArrayExpr.NoteUseTargetVar (p.expr);
      (* ^Marker.EmitReturn will either build result value directly or
         provide a temp. *)
      AssignStmt.Check (t, p.expr, cs);
    END;
  END Check;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  BEGIN
    Marker.EmitReturn (p.expr, fromFinally := FALSE);
    RETURN Stmt.Outcomes {Stmt.Outcome.Returns};
  END Compile;

PROCEDURE GetOutcome (<*UNUSED*> p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.Outcomes {Stmt.Outcome.Returns};
  END GetOutcome;

PROCEDURE CompileMSIR (p: P) =
  VAR v: MSIR.Value := NIL;  endCatch: MSIR.Proc;
  BEGIN
    IF NOT MSIRBuilder.InProc ()              THEN RETURN END;
    IF MSIRBuilder.CurrentBlockTerminated ()  THEN RETURN END;
    IF p.expr # NIL THEN
      v := Expr.CompileMSIR (p.expr);
      IF v = NIL THEN RETURN END;
      (* Coerce the return value to match the declared procedure result type when
         M3 subtyping allows assignment but MSIR requires exact kind match:
         - NIL constant → any non-void result type (e.g. RETURN NIL from ADDRESS proc)
         - gc_ref X → gc_ref void when the proc result is REFANY / gc_ref void
           (REF T <: REFANY is valid M3 but MSIR types are not structurally equal)
         For large-result procs the LLVM result type is void; use CurrentResultType()
         to get the actual M3-level type for coercion purposes. *)
      VAR resultT := MSIRBuilder.CurrentResultType ();
      BEGIN
        IF resultT = NIL THEN
          resultT := MSIR.ProcResultType (MSIRBuilder.CurrentProc ());
        END;
        IF resultT # NIL AND MSIR.Kind (resultT) # MSIR.TypeKind.Void AND
           NOT MSIR.Equal (MSIR.ValueType (v), resultT) THEN
          IF MSIR.GetValueKind (v) = MSIR.ValueKind.ConstNil OR
             (MSIR.Kind (MSIR.ValueType (v)) = MSIR.Kind (resultT) AND
              (MSIR.Kind (resultT) = MSIR.TypeKind.GcRef OR
               MSIR.Kind (resultT) = MSIR.TypeKind.Ptr)) THEN
            v := MSIR.RetypeValue (v, resultT);
          ELSIF MSIR.Kind (MSIR.ValueType (v))  = MSIR.TypeKind.OpenArray AND
                MSIR.OpenArrayRank (MSIR.ValueType (v)) = 1              AND
                MSIR.Kind (resultT) = MSIR.TypeKind.FixedArray           AND
                MSIR.Equal (MSIR.OpenArrayElt  (MSIR.ValueType (v)),
                             MSIR.FixedArrayElt (resultT)) THEN
            (* RETURN open_array_var from a fixed-array-result proc.
               Extract the data pointer from the dope vector, retype it to
               ptr([N]T) so the verifier accepts the load, then load the
               fixed-array value. *)
            VAR blk  := MSIRBuilder.CurrentBlock ();
                zero := MSIR.ConstInt (MSIR.TI (Target.Integer.size), 0);
                dPtr := MSIR.BuildOpenArrayElemAddr (blk, "", v,
                          ARRAY OF MSIR.Value {zero});
                tPtr := MSIR.RetypeValue (dPtr, MSIR.TPtr (resultT));
            BEGIN
              v := MSIR.BuildLoad (MSIRBuilder.CurrentBlock (), "", resultT, tPtr);
            END;
          ELSIF MSIR.Kind (MSIR.ValueType (v)) >= MSIR.TypeKind.I1 AND
                MSIR.Kind (MSIR.ValueType (v)) <= MSIR.TypeKind.W64 AND
                MSIR.Kind (resultT) >= MSIR.TypeKind.I1 AND
                MSIR.Kind (resultT) <= MSIR.TypeKind.W64 AND
                MSIR.BitWidth (MSIR.ValueType (v)) < MSIR.BitWidth (resultT) THEN
            (* Packed integer subrange (e.g. BITS 8 FOR [0..255]) widened to INTEGER.
               Use SExt if the subrange lower bound is negative, ZExt otherwise. *)
            VAR lo, hi: Target.Int;
                doSExt := (p.expr # NIL) AND
                           Type.GetBounds (Type.StripPacked (Expr.TypeOf (p.expr)),
                                           lo, hi) AND
                           TInt.LT (lo, TInt.Zero);
            BEGIN
              IF doSExt
                THEN v := MSIR.BuildSExt (MSIRBuilder.CurrentBlock (), "", v, resultT);
                ELSE v := MSIR.BuildZExt (MSIRBuilder.CurrentBlock (), "", v, resultT);
              END;
            END;
          ELSIF MSIR.Kind (MSIR.ValueType (v)) >= MSIR.TypeKind.I1 AND
                MSIR.Kind (MSIR.ValueType (v)) <= MSIR.TypeKind.W64 AND
                MSIR.Kind (resultT) >= MSIR.TypeKind.I1 AND
                MSIR.Kind (resultT) <= MSIR.TypeKind.W64 AND
                MSIR.BitWidth (MSIR.ValueType (v)) > MSIR.BitWidth (resultT) THEN
            (* INTEGER (i64) returned into a packed subrange result (e.g. [-1..+1] → i8).
               Truncate to the narrower type. *)
            v := MSIR.BuildTrunc (MSIRBuilder.CurrentBlock (), "", v, resultT);
          ELSIF MSIR.Kind (MSIR.ValueType (v)) = MSIR.TypeKind.Enum AND
                MSIR.Kind (resultT) >= MSIR.TypeKind.I1 AND
                MSIR.Kind (resultT) <= MSIR.TypeKind.W64 THEN
            (* Enum value (e.g. ST.Missing, VAL(x, RuntimeError)) returned from
               an i64-result proc (TranslateResult returns i64 for ordinals).
               TEnum is emitted as iN in LLVM IR, so ZExt/Trunc is valid. *)
            VAR blk := MSIRBuilder.CurrentBlock ();
                srcW := MSIR.BitWidth (MSIR.ValueType (v));
                dstW := MSIR.BitWidth (resultT);
            BEGIN
              IF srcW < dstW THEN
                v := MSIR.BuildZExt (blk, "", v, resultT);
              ELSIF srcW > dstW THEN
                v := MSIR.BuildTrunc (blk, "", v, resultT);
              ELSE
                v := MSIR.RetypeValue (v, resultT);
              END;
            END;
          ELSE
            (* Unhandled type mismatch.  Array-copy with memcpy is not yet
               implemented for all cases (e.g. multi-rank open arrays). *)
            MSIRBuilder.Abandon ("return type mismatch not yet supported in MSIR");
            RETURN;
          END;
        END;
      END;
    END;
    (* If returning from inside a catch handler, release the exception first. *)
    endCatch := MSIRBuilder.CurrentCatchEndProc ();
    IF endCatch # NIL THEN
      EVAL MSIR.BuildCall (MSIRBuilder.CurrentBlock (), "", endCatch,
                           ARRAY OF MSIR.Value {});
    END;
    (* Route the RETURN through any enclosing TRY/FINALLY cleanup frame.
       A RETURN inside a TRY body must execute the FINALLY before leaving.
       EmitReturnThroughFinally stores v into the frame's retSlot, sets
       selector = Sel_Return, branches to finBody, and returns TRUE.
       When it returns FALSE there is no enclosing finally and we emit a
       plain ret.  (p020: QQ's RETURN j+10 must pass through FINALLY
       j := 4; RETURN j+1, which overrides the return value to 5.) *)
    IF MSIRBuilder.EmitReturnThroughFinally (v) THEN
      RETURN;
    END;
    VAR resultPtr := MSIRBuilder.CurrentResultPtr ();  blk := MSIRBuilder.CurrentBlock ();
    BEGIN
      IF resultPtr # NIL AND v # NIL THEN
        MSIR.BuildStore (blk, v, resultPtr);
        MSIR.BuildRet (blk, NIL);
      ELSE
        MSIR.BuildRet (blk, v);
      END;
    END;
  END CompileMSIR;

PROCEDURE Capture (p: P;  ca: CaptureAnalysis.T) =
  BEGIN
    Expr.Capture (p.expr, ca);
  END Capture;

BEGIN
END ReturnStmt.
