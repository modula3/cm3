MODULE MSIREH;

IMPORT MSIR, M3RT, Target, Fmt, RefSeq;
FROM MSIR IMPORT Op, ScopeKind;

(* Selector codes stored before entering FINALLY code. *)
CONST
  Sel_Exc     = 1;
  Sel_Return  = 2;
  FirstTarget = 3;           (* + k: the k-th distinct unwind_to target *)

TYPE
  ScopeInfo = REF RECORD
    s        : MSIR.Scope;
    lpad     : MSIR.Block := NIL;    (* protected scopes *)
    hdr, act : MSIR.Value := NIL;    (* Except: from its landing pad *)
    selector : MSIR.Value := NIL;    (* Finally *)
    actSlot  : MSIR.Value := NIL;
    retSlot  : MSIR.Value := NIL;
    targets  : RefSeq.T := NIL;      (* Finally: MSIR.Block, code FirstTarget+k *)
    returns  := FALSE;
  END;

  ArgSlot = REF RECORD               (* a handler entry's argument slot *)
    entry : MSIR.Block;
    slot  : MSIR.Value;
  END;

  Pending = REF RECORD               (* an end_finally, lowered in phase 2 *)
    block : MSIR.Block;
    scope : MSIR.Scope;              (* its FinallyCode scope *)
  END;

VAR
  mod        : MSIR.Module;
  proc       : MSIR.Proc;
  info       : REF ARRAY OF ScopeInfo;
  seq        : INTEGER;
  ptrT, i32, i64 : MSIR.T;
  getPtr, beginCatch, endCatch : MSIR.Proc;
  stubModule : MSIR.Module := NIL;   (* the module the stubs were made for *)
  argSlots   : RefSeq.T;             (* ArgSlot, per handler entry *)
  allocaBlk  : MSIR.Block;           (* the entry, where slots go *)

(*---------------------------------------------------------------- helpers *)

PROCEDURE Stubs (m: MSIR.Module) =
  VAR hdrParam := ARRAY [0..0] OF MSIR.Param {
                    MSIR.Param {"exc_header", MSIR.TPtr (MSIR.TVoid ()),
                                MSIR.ParamMode.ByValue} };
  BEGIN
    IF m = stubModule THEN RETURN END;
    stubModule := m;
    ptrT := MSIR.TPtr (MSIR.TVoid ());
    i32  := MSIR.TI (32);
    i64  := MSIR.TI (64);
    getPtr     := MSIR.NewProc ("__cxa_get_exception_ptr", hdrParam, ptrT);
    beginCatch := MSIR.NewProc ("__cxa_begin_catch", hdrParam, ptrT);
    endCatch   := MSIR.NewProc ("__cxa_end_catch", ARRAY OF MSIR.Param {},
                                MSIR.TVoid ());
  END Stubs;

PROCEDURE Info (s: MSIR.Scope): ScopeInfo =
  BEGIN
    RETURN info[MSIR.ScopeIndex (s)];
  END Info;

PROCEDURE IsProtected (s: MSIR.Scope): BOOLEAN =
  BEGIN
    RETURN MSIR.ScopeKindOf (s) IN SET OF ScopeKind {ScopeKind.Except,
                                                     ScopeKind.Finally};
  END IsProtected;

(* The landing pad an exception raised in scope s goes to, or NIL. *)
PROCEDURE Unwind (s: MSIR.Scope): MSIR.Block =
  BEGIN
    WHILE s # NIL DO
      IF IsProtected (s) THEN RETURN Info (s).lpad END;
      s := MSIR.ScopeParent (s);
    END;
    RETURN NIL;
  END Unwind;

(* The Except scope whose handler code s is in, for handler.activation. *)
PROCEDURE HandlerOwner (s: MSIR.Scope): MSIR.Scope =
  BEGIN
    WHILE s # NIL DO
      IF MSIR.ScopeKindOf (s) = ScopeKind.Handler THEN
        RETURN MSIR.ScopeOwner (s);
      END;
      s := MSIR.ScopeParent (s);
    END;
    RETURN NIL;
  END HandlerOwner;

PROCEDURE Depth (s: MSIR.Scope): INTEGER =
  VAR d := 0;
  BEGIN
    WHILE s # NIL DO INC (d);  s := MSIR.ScopeParent (s) END;
    RETURN d;
  END Depth;

PROCEDURE NewBlk (tag: TEXT): MSIR.Block =
  VAR b := MSIR.NewBlock ("eh." & tag & "." & Fmt.Int (seq),
                          ARRAY OF MSIR.BlockParam {});
  BEGIN
    INC (seq);
    MSIR.ProcAddBlock (proc, b);
    RETURN b;
  END NewBlk;

PROCEDURE Args (i: MSIR.Insn): REF ARRAY OF MSIR.Value =
  VAR r := NEW (REF ARRAY OF MSIR.Value, MSIR.InsnBrArgCount (i, 0));
  BEGIN
    FOR j := 0 TO LAST (r^) DO r[j] := MSIR.InsnBrArg (i, 0, j) END;
    RETURN r;
  END Args;

CONST NoArgs = ARRAY OF MSIR.Value {};

(* Re-raise the activation saved in actSlot: invoked to the enclosing landing
   pad u in this frame, or called (propagating to the caller) if none. *)
PROCEDURE Reraise (b: MSIR.Block;  act: MSIR.Value;  u: MSIR.Block) =
  VAR hook := MSIR.ModuleRtHook(mod, MSIR.RtHook.ResumeRaiseEx);  c: MSIR.Block;
  BEGIN
    <* ASSERT hook # NIL *>
    IF u # NIL THEN
      c := NewBlk ("reraised");
      EVAL MSIR.BuildInvoke (b, "", hook, ARRAY OF MSIR.Value {act}, c, u);
      MSIR.BuildUnreachable (c);
    ELSE
      EVAL MSIR.BuildCall (b, "", hook, ARRAY OF MSIR.Value {act});
      MSIR.BuildUnreachable (b);
    END;
  END Reraise;

(*----------------------------------------------------------- leaving scopes *)

(* Leave scopes from scope 'from', ending block b: for a branch to target
   (with args), or a return of v.  Handler code left calls __cxa_end_catch;
   the first Finally scope left takes the outcome and runs its FINALLY code,
   whose end_finally continues it. *)
PROCEDURE Leave (b: MSIR.Block;  from: MSIR.Scope;  target: MSIR.Block;
                 READONLY args: ARRAY OF MSIR.Value;  isReturn: BOOLEAN;
                 v: MSIR.Value) =
  VAR s := from;  fi: ScopeInfo;  code: INTEGER;
  BEGIN
    LOOP
      IF NOT isReturn AND s = MSIR.BlockScope (target) THEN
        MSIR.BuildBr (b, target, args);
        RETURN;
      END;
      CASE MSIR.ScopeKindOf (s) OF
      | ScopeKind.Proc =>
          <* ASSERT isReturn *>
          MSIR.BuildRet (b, v);
          RETURN;
      | ScopeKind.Handler =>
          EVAL MSIR.BuildCall (b, "", endCatch, NoArgs);
      | ScopeKind.Except, ScopeKind.FinallyCode =>
      | ScopeKind.Finally =>
          fi := Info (s);
          IF isReturn THEN
            code := Sel_Return;
            fi.returns := TRUE;
            IF v # NIL AND fi.retSlot # NIL THEN MSIR.BuildStore (b, v, fi.retSlot) END;
          ELSE
            <* ASSERT NUMBER (args) = 0 *>  (* no block arguments through FINALLY *)
            code := TargetCode (fi, target);
          END;
          MSIR.BuildStore (b, MSIR.ConstInt (i32, code), fi.selector);
          MSIR.BuildBr (b, MSIR.ScopeFinally (s), NoArgs);
          RETURN;
      END;
      s := MSIR.ScopeParent (s);
    END;
  END Leave;

PROCEDURE TargetCode (fi: ScopeInfo;  target: MSIR.Block): INTEGER =
  BEGIN
    FOR k := 0 TO fi.targets.size () - 1 DO
      IF fi.targets.get (k) = target THEN RETURN FirstTarget + k END;
    END;
    fi.targets.addhi (target);
    RETURN FirstTarget + fi.targets.size () - 1;
  END TargetCode;

(*----------------------------------------------------------- landing pads *)

(* lpad of a Finally scope: save the activation, select Sel_Exc, run the
   FINALLY code. *)
PROCEDURE FinallyPad (s: MSIR.Scope) =
  VAR fi := Info (s);  b := fi.lpad;  lp, hdr, obj, act: MSIR.Value;
  BEGIN
    lp  := MSIR.BuildLandingPad (b, "", isCleanup := FALSE);
    hdr := MSIR.BuildExtractValue (b, "", lp, 0);
    obj := MSIR.BuildCall (b, "", getPtr, ARRAY OF MSIR.Value {hdr});
    act := MSIR.BuildLoad (b, "", ptrT, obj);
    MSIR.BuildStore (b, act, fi.actSlot);
    MSIR.BuildStore (b, MSIR.ConstInt (i32, Sel_Exc), fi.selector);
    MSIR.BuildBr (b, MSIR.ScopeFinally (s), NoArgs);
  END FinallyPad;

(* lpad of an Except scope: find the handler by the exception's uid. *)
PROCEDURE ExceptPad (s: MSIR.Scope) =
  VAR fi := Info (s);  b := fi.lpad;  lp, obj, desc, uid: MSIR.Value;
      h: MSIR.Handler;  yes, no: MSIR.Block;
  BEGIN
    lp      := MSIR.BuildLandingPad (b, "", isCleanup := FALSE);
    fi.hdr  := MSIR.BuildExtractValue (b, "", lp, 0);
    obj     := MSIR.BuildCall (b, "", getPtr, ARRAY OF MSIR.Value {fi.hdr});
    fi.act  := MSIR.BuildLoad (b, "", ptrT, obj);
    desc    := MSIR.BuildLoad (b, "", ptrT, fi.act);
    uid     := MSIR.BuildLoad (b, "", i64, desc);
    FOR k := 0 TO MSIR.ScopeHandlerCount (s) - 1 DO
      h := MSIR.ScopeHandler (s, k);
      IF h.exc = NIL THEN                (* ELSE: the last handler *)
        yes := NewBlk ("else");
        MSIR.BuildBr (b, yes, NoArgs);
        ToHandler (yes, h, fi.act);
        RETURN;
      END;
      yes := NewBlk ("catch");
      no  := NewBlk ("next");
      MSIR.BuildCondBr (b, MSIR.BuildICmp (b, "", MSIR.CmpPred.Eq, uid,
                             MSIR.ConstInt (i64, MSIR.ExcDescUID (h.exc))),
                        yes, NoArgs, no, NoArgs);
      ToHandler (yes, h, fi.act);
      b := no;
    END;
    (* No handler matched: re-raise to the enclosing landing pad in this
       frame (resume would skip it), or resume to the caller. *)
    VAR u := Unwind (MSIR.ScopeParent (s));
    BEGIN
      IF u # NIL THEN Reraise (b, fi.act, u) ELSE MSIR.BuildResume (b, lp) END;
    END;
  END ExceptPad;

(* Branch from b to handler h's code with the exception's argument, read
   from the activation's argument word (by value or by reference).  Block
   arguments are not lowered (MSIRToLLVM has no phis), so the argument goes
   through a slot the handler entry loads (BlockParamsToLoads, phase 1). *)
PROCEDURE ToHandler (b: MSIR.Block;  READONLY h: MSIR.Handler;
                     act: MSIR.Value) =
  VAR fld, raw, val: MSIR.Value;  t: MSIR.T;
  BEGIN
    IF MSIR.BlockParamCount (h.entry) = 1 THEN
      t   := MSIR.ExcDescArgType (h.exc);
      fld := MSIR.BuildPtrAdd (b, "", act, M3RT.EA_arg DIV Target.Char.size);
      raw := MSIR.BuildLoad (b, "", ptrT, fld);
      IF MSIR.ExcDescArgByRef (h.exc)
        THEN val := MSIR.BuildLoad (b, "", t, raw);
        ELSE val := MSIR.BuildConvert (b, "", raw, t);
      END;
      MSIR.BuildStore (b, val, ArgSlotOf (h.entry, t));
    END;
    MSIR.BuildBr (b, h.entry, NoArgs);
  END ToHandler;

PROCEDURE ArgSlotOf (entry: MSIR.Block;  t: MSIR.T): MSIR.Value =
  VAR a: ArgSlot;
  BEGIN
    FOR k := 0 TO argSlots.size () - 1 DO
      a := argSlots.get (k);
      IF a.entry = entry THEN RETURN a.slot END;
    END;
    a := NEW (ArgSlot, entry := entry, slot := MSIR.BuildAlloca (allocaBlk, "", t));
    argSlots.addhi (a);
    RETURN a.slot;
  END ArgSlotOf;

(*------------------------------------------------------------ end_finally *)

(* After FINALLY code: continue the outcome its selector records. *)
PROCEDURE EndFinally (b: MSIR.Block;  code: MSIR.Scope) =
  VAR s := MSIR.ScopeOwner (code);  fi := Info (s);  out := MSIR.ScopeParent (s);
      sel := MSIR.BuildLoad (b, "", i32, fi.selector);
      arm, next: MSIR.Block;
  BEGIN
    arm  := NewBlk ("fin.raise");
    next := NewBlk ("fin.next");
    MSIR.BuildCondBr (b, Is (b, sel, Sel_Exc), arm, NoArgs, next, NoArgs);
    Reraise (arm, MSIR.BuildLoad (arm, "", ptrT, fi.actSlot), Unwind (out));
    b := next;
    FOR k := 0 TO fi.targets.size () - 1 DO
      arm  := NewBlk ("fin.exit");
      next := NewBlk ("fin.next");
      MSIR.BuildCondBr (b, Is (b, sel, FirstTarget + k), arm, NoArgs, next, NoArgs);
      Leave (arm, out, fi.targets.get (k), NoArgs, FALSE, NIL);
      b := next;
    END;
    IF fi.returns THEN
      VAR v: MSIR.Value := NIL;
      BEGIN
        IF fi.retSlot # NIL THEN
          v := MSIR.BuildLoad (b, "", MSIR.ProcResultType (proc), fi.retSlot);
        END;
        Leave (b, out, NIL, NoArgs, TRUE, v);
      END;
    ELSE
      MSIR.BuildUnreachable (b);
    END;
  END EndFinally;

PROCEDURE Is (b: MSIR.Block;  sel: MSIR.Value;  code: INTEGER): MSIR.Value =
  BEGIN
    RETURN MSIR.BuildICmp (b, "", MSIR.CmpPred.Eq, sel, MSIR.ConstInt (i32, code));
  END Is;

PROCEDURE Prepend (a, b: REF ARRAY OF MSIR.Insn): REF ARRAY OF MSIR.Insn =
  VAR r := NEW (REF ARRAY OF MSIR.Insn, NUMBER (a^) + NUMBER (b^));
  BEGIN
    SUBARRAY (r^, 0, NUMBER (a^)) := a^;
    SUBARRAY (r^, NUMBER (a^), NUMBER (b^)) := b^;
    RETURN r;
  END Prepend;

(*-------------------------------------------------------------- the pass *)

PROCEDURE LowerProc (m: MSIR.Module;  p: MSIR.Proc) =
  VAR
    nScopes := MSIR.ProcScopeCount (p);
    nBlocks := MSIR.ProcBlockCount (p);
    orig    := NEW (REF ARRAY OF MSIR.Block, nBlocks);
    entry   : MSIR.Block;
    entryInsns : REF ARRAY OF MSIR.Insn;
    pending := NEW (RefSeq.T).init ();
    resultT := MSIR.ProcResultType (p);
    s       : MSIR.Scope;
  BEGIN
    IF nBlocks = 0 OR nScopes = 1 THEN RETURN END;
    Stubs (m);
    mod := m;  proc := p;  seq := 0;
    FOR k := 0 TO nBlocks - 1 DO orig[k] := MSIR.ProcBlock (p, k) END;
    entry := orig[0];
    entryInsns := MSIR.BlockTakeInsns (entry);
    allocaBlk := entry;
    argSlots := NEW (RefSeq.T).init ();

    (* Scope data; FINALLY state lives in allocas at the head of the entry. *)
    info := NEW (REF ARRAY OF ScopeInfo, nScopes);
    FOR k := 0 TO nScopes - 1 DO
      s := MSIR.ProcScopeAt (p, k);
      info[k] := NEW (ScopeInfo, s := s);
      IF IsProtected (s) THEN info[k].lpad := NewBlk ("lpad") END;
      IF MSIR.ScopeKindOf (s) = ScopeKind.Finally THEN
        info[k].selector := MSIR.BuildAlloca (entry, "", i32);
        info[k].actSlot  := MSIR.BuildAlloca (entry, "", ptrT);
        IF resultT # NIL AND MSIR.Kind (resultT) # MSIR.TypeKind.Void THEN
          info[k].retSlot := MSIR.BuildAlloca (entry, "", resultT);
        END;
        info[k].targets := NEW (RefSeq.T).init ();
      END;
    END;
    FOR k := 0 TO nScopes - 1 DO
      CASE MSIR.ScopeKindOf (info[k].s) OF
      | ScopeKind.Except  => ExceptPad (info[k].s);
      | ScopeKind.Finally => FinallyPad (info[k].s);
      ELSE
      END;
    END;

    (* Phase 1: calls in protected blocks become invokes; scope exits are
       lowered; end_finally is left for phase 2. *)
    FOR k := 0 TO nBlocks - 1 DO
      VAR b := orig[k];  sc := MSIR.BlockScope (b);  cur := b;
          u := Unwind (sc);  insns: REF ARRAY OF MSIR.Insn;
      BEGIN
        IF k = 0 THEN insns := entryInsns ELSE insns := MSIR.BlockTakeInsns (b) END;
        IF MSIR.BlockParamCount (b) = 1 THEN
          (* a handler entry: its argument arrives in a slot *)
          VAR t := MSIR.ValueType (MSIR.BlockParamValue (b, 0));
          BEGIN
            MSIR.BlockParamsToLoads (b, ARRAY OF MSIR.Value {ArgSlotOf (b, t)});
            insns := Prepend (MSIR.BlockTakeInsns (b), insns);
          END;
        END;
        IF MSIR.ScopeKindOf (sc) = ScopeKind.Handler AND MSIR.ScopeEntry (sc) = b THEN
          EVAL MSIR.BuildCall (cur, "", beginCatch,
                               ARRAY OF MSIR.Value {Info (MSIR.ScopeOwner (sc)).hdr});
        END;
        FOR j := 0 TO LAST (insns^) DO
          VAR i := insns[j];
          BEGIN
            CASE MSIR.InsnOp (i) OF
            | Op.Call, Op.CallIndirect =>
                MSIR.BlockAppendInsn (cur, i);
                IF u # NIL THEN
                  VAR cont := NewBlk ("cont");
                  BEGIN
                    MSIR.InsnMakeInvoke (i, cont, u);
                    cur := cont;
                  END;
                END;
            | Op.HandlerActivation =>
                MSIR.InsnMakeCopy (i, Info (HandlerOwner (sc)).act);
                MSIR.BlockAppendInsn (cur, i);
            | Op.UnwindTo =>
                Leave (cur, sc, MSIR.InsnBrTarget (i, 0), Args (i)^, FALSE, NIL);
            | Op.RetThroughEnvelope =>
                VAR v: MSIR.Value := NIL;
                BEGIN
                  IF MSIR.InsnOperandCount (i) > 0 THEN v := MSIR.InsnOperand (i, 0) END;
                  Leave (cur, sc, NIL, NoArgs, TRUE, v);
                END;
            | Op.EndFinally =>
                pending.addhi (NEW (Pending, block := cur, scope := sc));
            | Op.Raise =>
                <* ASSERT FALSE *>  (* MSIRRuntime.Expand lowered it (D35) *)
            ELSE
              MSIR.BlockAppendInsn (cur, i);
            END;
          END;
        END;
      END;
    END;

    (* Phase 2: end_finally, inner FINALLYs first, since continuing an
       outcome from an inner one can add outcomes to an outer one. *)
    VAR n := pending.size ();  a := NEW (REF ARRAY OF Pending, n);  t: Pending;
    BEGIN
      FOR k := 0 TO n - 1 DO a[k] := pending.get (k) END;
      FOR k := 1 TO n - 1 DO
        t := a[k];
        VAR j := k;
        BEGIN
          WHILE j > 0 AND Depth (a[j-1].scope) < Depth (t.scope) DO
            a[j] := a[j-1];  DEC (j);
          END;
          a[j] := t;
        END;
      END;
      FOR k := 0 TO n - 1 DO EndFinally (a[k].block, a[k].scope) END;
    END;

    MSIR.ProcClearScopes (p);
    info := NIL;  proc := NIL;  mod := NIL;
  END LowerProc;

PROCEDURE LowerModule (m: MSIR.Module) =
  BEGIN
    FOR i := 0 TO MSIR.ModuleProcCount (m) - 1 DO
      LowerProc (m, MSIR.ModuleProc (m, i));
    END;
  END LowerModule;

BEGIN
END MSIREH.
