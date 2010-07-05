(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 28 11:01:06 PST 1997 by heydon                   *)
(*      modified on Sun Jun  5 15:47:38 PDT 1994 by gnelson                  *)
(*      modified on Sat Aug 22 22:10:45 PDT 1992 by myers                    *)
<* PRAGMA SPEC *>

UNSAFE MODULE JunoRT EXPORTS JunoRT, JunoArgs;

IMPORT JunoRTError, JunoMarshal, JunoSolve, JunoValue, RTVal;
IMPORT   QId, QIdIntTbl, JunoByteCode AS BC;
IMPORT Atom, RefIntTbl, Word, Thread;

(* For debugging output *)
IMPORT Wr, Fmt, JunoDisassem;
FROM Stdio IMPORT stderr;
<* FATAL Wr.Failure, Thread.Alerted *>

(* ----------------------------- TABLE TYPE -------------------------------- *)

TYPE
  JVIntTbl = RefIntTbl.Default BRANDED "JunoRT.JVIntTbl" OBJECT OVERRIDES
    keyEqual := JVEqual;
    keyHash := JVHash
  END;

PROCEDURE JVEqual(<*UNUSED*> t: JVIntTbl; READONLY k1, k2: REFANY): BOOLEAN =
  BEGIN RETURN JunoValue.Equal(k1, k2) END JVEqual;

PROCEDURE JVHash(<*UNUSED*> t: JVIntTbl; READONLY k: REFANY): Word.T =
  BEGIN RETURN JunoValue.Hash(k) END JVHash;

(* ---------------------- VALUE/CODE TABLE OPERATIONS  --------------------- *)

VAR
  val_idx_tbl := NEW(JVIntTbl).init(sizeHint := 20);
  var_idx_tbl, code_idx_tbl, ext_idx_tbl :=
    NEW(QIdIntTbl.Default).init(sizeHint:=20);
  next_var_idx := 1;
  next_code_idx := 1;
  next_ext_idx := 0;
  procAttrs: REF ARRAY OF ProcAttr;
  extProcAttrs: REF ARRAY OF ProcAttr;

PROCEDURE GetValueIndex(val: JunoValue.T): CARDINAL =
  VAR res: INTEGER; BEGIN
    IF NOT val_idx_tbl.get(val, res) THEN
      res := next_var_idx;
      INC(next_var_idx);
      EVAL val_idx_tbl.put(val, res);
      VAR n := NUMBER(value_tbl^); BEGIN
        IF next_var_idx = n THEN
          VAR new := NEW(REF ARRAY OF JunoValue.T, 2 * n); BEGIN
            SUBARRAY(new^, 0, n) := value_tbl^;
            value_tbl := new
          END
        END
      END;
      value_tbl[res] := val
    END;
    RETURN res
  END GetValueIndex;

PROCEDURE GetVarIndex(md, nm: Atom.T): CARDINAL =
  VAR res: INTEGER; qid := NEW(QId.T, id0 := md, id1 := nm); BEGIN
    IF NOT var_idx_tbl.get(qid, res) THEN
      res := next_var_idx;
      INC(next_var_idx);
      EVAL var_idx_tbl.put(qid, res);
      VAR n := NUMBER(value_tbl^); BEGIN
        IF next_var_idx = n THEN
          VAR new := NEW(REF ARRAY OF JunoValue.T, 2 * n); BEGIN
            SUBARRAY(new^, 0, n) := value_tbl^;
            value_tbl := new
          END
        END
      END
    END;
    RETURN res
  END GetVarIndex;

PROCEDURE GetCodeIndex(READONLY pa: ProcAttr): CARDINAL =
  VAR res: INTEGER; qid := NEW(QId.T, id0 := pa.modName, id1 := pa.name); BEGIN
    IF NOT code_idx_tbl.get(qid, res) THEN
      res := next_code_idx; INC(next_code_idx);
      EVAL code_idx_tbl.put(qid, res);
      VAR n := NUMBER(code_tbl^); BEGIN
        IF next_code_idx = n THEN
          VAR new := NEW(REF ARRAY OF ByteStream, 2 * n); BEGIN
            SUBARRAY(new^, 0, n) := code_tbl^;
            code_tbl := new
          END;
          VAR new := NEW(REF ARRAY OF ProcAttr, 2 * n); BEGIN
            SUBARRAY(new^, 0, n) := procAttrs^;
            procAttrs := new
          END
        END
      END
    END;
    procAttrs[res] := pa;
    RETURN res
  END GetCodeIndex;

PROCEDURE GetExtCodeIndex(READONLY pa: ProcAttr): CARDINAL =
  VAR res: INTEGER; qid := NEW(QId.T, id0 := pa.modName, id1 := pa.name); BEGIN
    IF NOT ext_idx_tbl.get(qid, res) THEN
      res := next_ext_idx; INC(next_ext_idx);
      EVAL ext_idx_tbl.put(qid, res);
      VAR n := NUMBER(ext_code_tbl^); BEGIN
        IF next_ext_idx = n THEN
          VAR new := NEW(REF ARRAY OF ExternalCode, 2 * n); BEGIN
            SUBARRAY(new^, 0, n) := ext_code_tbl^;
            ext_code_tbl := new
          END;
          VAR new := NEW(REF ARRAY OF ProcAttr, 2 * n); BEGIN
            SUBARRAY(new^, 0, n) := extProcAttrs^;
            extProcAttrs := new
          END
        END
      END
    END;
    extProcAttrs[res] := pa;
    RETURN res
  END GetExtCodeIndex;

PROCEDURE GetProcAttr(s: CARDINAL): ProcAttr =
  BEGIN RETURN procAttrs[s] END GetProcAttr;

PROCEDURE GetExtProcAttr(s: CARDINAL): ProcAttr =
  BEGIN RETURN extProcAttrs[s] END GetExtProcAttr;

(* ------------------------ STACK FRAME OPERATIONS  ------------------------ *)

TYPE
  CallState = RTVal.T OBJECT
    pc: PC;
    fp: CARDINAL;
    clIns: CARDINAL;
    nextAvail: CallState;
  END;

(* A "CallState" encapsulates the state of the machine into an object
   that can be Push()ed onto the stack when a CALL bytecode is performed.
   The procedure "MakeCallState" must be used to create objects of this type,
   and "DisposeCallState" should be called to recycle them. *)

TYPE
  BytePtr = JunoMarshal.BytePtr;
  Real = JunoValue.Real;
  StackValue = RTVal.T;

(* The values on the stack are of type "CallState" or one of
   the value types declared in the "RTVal" interface. *)

CONST
  InitialProc = 0;
  BaseFP = LAST(CARDINAL);
  NoTrapLoc = PC{InitialProc, 0};
  NoErrorCode = JunoRTError.Code.None;

(* The "InitialProc" slot of the global code table is reserved by the
   run-time. The value "BaseFP" is a distinguished fp value for the base of
   the frame stack.  It must never be used for anything, and making it
   "LAST(CARDINAL)" ensures that it will cause problems if it is used.

   The value "NoErrorCode" is assigned to the "errorCode" field of an
   "ExecRes" record in the event of a non-error trap-code. *)

VAR
  (* state variables *)
  stackSize: CARDINAL := 10000;
  stack := NEW(REF ARRAY OF StackValue, stackSize);
  pc := PC{InitialProc, 0};		 (* program counter *)
  fp: CARDINAL := BaseFP;		 (* frame pointer (into stack) *)
  sp: CARDINAL := 0;			 (* stack pointer (into stack) *)
  ext_fp: CARDINAL;			 (* frame pointer for external proc *)
  cond: BOOLEAN;			 (* condition flag *)

VAR
  (* saved state *)
  saved_pc: PC;
  saved_fp, saved_sp: CARDINAL;
  saved_stack: REF ARRAY OF StackValue;
  saved_cond: BOOLEAN;

REVEAL
  Frame = FramePublic BRANDED "JunoRT.Frame" OBJECT
    fp: CARDINAL;
    downF, upF: Frame;
  OVERRIDES
    down     := FrameDown;
    up       := FrameUp;
    getLocal := GetLocal;
    setLocal := SetLocal;
    pc       := GetPC;
    setPC    := SetPC
  END;

VAR
  topFrame: Frame;
  baseFrame: Frame;

(* The "topFrame" and "baseFrame" variables are valid only after executing
   either the "ResetMachine" or "RecomputeFrames" procedure. *)

PROCEDURE FrameDown(f: Frame): Frame =
  BEGIN
    RETURN f.downF
  END FrameDown;

PROCEDURE FrameUp(f: Frame): Frame =
  BEGIN
    RETURN f.upF
  END FrameUp;

PROCEDURE GetLocal(f: Frame; i: INTEGER): JunoValue.T =
  BEGIN
    <* ASSERT i # 0 *>
    RETURN RTVal.ToJV(stack[f.fp + i])
  END GetLocal;

PROCEDURE SetLocal(f: Frame; i: INTEGER; v: JunoValue.T) =
  BEGIN
    <* ASSERT i # 0 *>
    stack[f.fp + i] := RTVal.FromJV(v)
  END SetLocal;

PROCEDURE GetPC(f: Frame): PC =
  BEGIN
    IF f = topFrame
      THEN RETURN pc
      ELSE RETURN NARROW(stack[f.upF.fp], CallState).pc
    END
  END GetPC;

PROCEDURE SetPC(f: Frame; newPC: PC) =
  BEGIN
    IF f = topFrame
      THEN pc := newPC
      ELSE NARROW(stack[f.upF.fp], CallState).pc := newPC
    END
  END SetPC;

PROCEDURE BaseFrame(): Frame =
  BEGIN
    RETURN baseFrame
  END BaseFrame;

PROCEDURE TopFrame(): Frame =
  BEGIN
    RETURN topFrame
  END TopFrame;

PROCEDURE PushFrame(new_pc: PC; size: CARDINAL) =
  VAR st := MakeCallState(pc, fp); BEGIN
    WHILE sp + size + 1 > stackSize DO EnlargeStack() END;
    <* FATAL StackOverflow, PushedNIL *>
    BEGIN
      FOR i := 1 TO size DO Push(RTVal.nil) END;
      Push(st)
    END;
    fp := sp - 1;
    topFrame.upF := NEW(Frame, downF := topFrame, upF := NIL, fp := fp);
    topFrame := topFrame.upF;
    pc := new_pc
  END PushFrame;

PROCEDURE PopFrame() =
(* "PopFrame" pops the frame, but leaves any arguments on the stack, so
   that it appears to the code in the frame above that the procedure has
   genuinely returned. *)
  VAR st: CallState := stack[fp]; BEGIN
    sp := fp;
    fp := st.fp;
    pc := st.pc;
    DisposeCallState(st);
    topFrame := topFrame.downF
  END PopFrame;

PROCEDURE RecomputeFrames() =
(* Create a new chain of stack frames corresponding to the current frames on
   the run-time stack beginning at "fp". Set "topFrame" and "baseFrame" to
   the bottom and top stack frames, respectively. *)
  VAR curr_fp := fp; BEGIN
    topFrame := NEW(Frame, downF := NIL, upF := NIL, fp := curr_fp);
    VAR f := topFrame; BEGIN
      WHILE curr_fp # BaseFP DO
        curr_fp := NARROW(stack[f.fp], CallState).fp;
        f.downF := NEW(Frame, upF := f, downF := NIL, fp := curr_fp);
        f := f.downF
      END;
      baseFrame := f
    END
  END RecomputeFrames;

EXCEPTION StackOverflow; PushedNIL;

(* "StackOverflow" is raised in the event that pushing onto the stack would
   cause a stack overflow. "PushedNIL" is raised when the machine
   attempts to push Modula-3 "NIL" onto the stack. This indicates the use of
   an uninitialized variable, since unhinted variables are initialized to
   Modula-3 "NIL" by the "PUSHM3NIL" instruction at the start of every
   procedure. *)

PROCEDURE Push(v: StackValue) RAISES { StackOverflow, PushedNIL } =
  BEGIN
    IF v = NIL THEN RAISE PushedNIL END;
    IF sp = stackSize THEN RAISE StackOverflow END;
    stack[sp] := v;
    INC(sp)
  END Push;

PROCEDURE Pop(): StackValue =
  BEGIN
    DEC(sp);
    RETURN stack[sp]
  END Pop;

PROCEDURE PopNum(VAR (*INOUT*) err: BOOLEAN): RTVal.Real =
  BEGIN
    DEC(sp);
    TYPECASE stack[sp] OF
      NULL => (* SKIP *)
    | RTVal.Number (r) => RETURN r.val
    ELSE (* SKIP *)
    END;
    err := TRUE;
    RETURN 0.0
  END PopNum;

PROCEDURE PopText(VAR (*INOUT*) err: BOOLEAN): TEXT =
  BEGIN
    DEC(sp);
    TYPECASE stack[sp] OF
      NULL => (* SKIP *)
    | RTVal.Text (r)  => RETURN r.val
    ELSE (* SKIP *)
    END;
    err := TRUE;
    RETURN NIL
  END PopText;

PROCEDURE PopPair(VAR (*INOUT*) err: BOOLEAN): RTVal.Pair =
  BEGIN
    DEC(sp);
    TYPECASE stack[sp] OF
      NULL => (* SKIP *)
    | RTVal.Pair (r)  => RETURN r
    ELSE (* SKIP *)
    END;
    err := TRUE;
    RETURN NIL
  END PopPair;

PROCEDURE InsertList(l: RTVal.T; lSz, numBelow: CARDINAL)
  RAISES {StackOverflow} =
(* Insert the "lSz" values of "l" onto the stack in order under the top
   "numBelow" elements. *)
  VAR oldSp := sp; BEGIN
    IF sp + lSz >= stackSize THEN RAISE StackOverflow END;
    INC(sp, lSz);
    SUBARRAY(stack^, sp - numBelow, numBelow) :=
      SUBARRAY(stack^, oldSp - numBelow, numBelow);
    VAR i := oldSp - numBelow; BEGIN
      WHILE l # RTVal.nil DO
      	VAR pr: RTVal.Pair := l; BEGIN
      	  stack[i] := pr.car;
      	  l := pr.cdr
      	END;
        INC(i)
      END
    END
  END InsertList;

PROCEDURE PopList(n: CARDINAL): RTVal.T =
(* Pop and return the list of the top "n" elements of the stack, where the
   top of the stack becomes the last element of the list. *)
  VAR
    res: RTVal.T := RTVal.nil;
  BEGIN
    FOR i := 1 TO n DO
      res := RTVal.FromPair(Pop(), res)
    END;
    RETURN res
  END PopList;

(* The operations in the JunoArgs interface: *)

PROCEDURE ReadValue(i: CARDINAL): RTVal.T =
  BEGIN RETURN stack[ext_fp - i] END ReadValue;

PROCEDURE ReadInt(i: CARDINAL; VAR err: BOOLEAN): INTEGER =
  BEGIN
    TYPECASE stack[ext_fp - i] OF
      NULL => (* SKIP *)
    | RTVal.Number (n) => RETURN ROUND(n.val)
    ELSE (* SKIP *)
    END;
    err := TRUE;
    RETURN 0
  END ReadInt;

PROCEDURE ReadReal(i: CARDINAL; VAR err: BOOLEAN): JunoValue.Real =
  BEGIN
    TYPECASE stack[ext_fp - i] OF
      NULL => (* SKIP *)
    | RTVal.Number (n) => RETURN n.val
    ELSE (* SKIP *)
    END;
    err := TRUE;
    RETURN 0.0
  END ReadReal;

PROCEDURE ReadText(i: CARDINAL; VAR err: BOOLEAN): TEXT =
  BEGIN
    TYPECASE stack[ext_fp - i] OF
      NULL => (* SKIP *)
    | RTVal.Text (t) => RETURN t.val
    ELSE (* SKIP *)
    END;
    err := TRUE;
    RETURN NIL
  END ReadText;

PROCEDURE ReadPair(i: CARDINAL; VAR err: BOOLEAN): RTVal.Pair =
  BEGIN
    TYPECASE stack[ext_fp - i] OF
      NULL => (* SKIP *)
    | RTVal.Pair (p) => RETURN p
    ELSE (* SKIP *)
    END;
    err := TRUE;
    RETURN NIL
  END ReadPair;

PROCEDURE WriteValue(i: CARDINAL; val: RTVal.T) =
  BEGIN stack[ext_fp - i] := val END WriteValue;

PROCEDURE WriteInt(i: CARDINAL; int: INTEGER) =
  BEGIN stack[ext_fp - i] := RTVal.FromInt(int) END WriteInt;

PROCEDURE WriteReal(i: CARDINAL; r: JunoValue.Real) =
  BEGIN stack[ext_fp - i] := RTVal.FromReal(r) END WriteReal;

PROCEDURE WriteText(i: CARDINAL; t: TEXT) =
  BEGIN stack[ext_fp - i] := RTVal.FromText(t) END WriteText;

PROCEDURE PushValue(v: RTVal.T) =
  <* FATAL StackOverflow, PushedNIL *>
  BEGIN Push(v) END PushValue;

(* --------------------------- MACHINE STATE ------------------------------- *)

VAR (* CONST *)
  emptyFrame := NEW(Frame, downF := NIL, upF := NIL, fp := BaseFP);

PROCEDURE ResetMachine() =
  BEGIN
    sp := 0;
    fp := BaseFP;
    pc := PC{InitialProc, 0};
    baseFrame := emptyFrame;
    topFrame := baseFrame
  END ResetMachine;

PROCEDURE Save() =
  BEGIN
    saved_fp := fp;
    saved_sp := sp;
    saved_cond := cond;
    saved_pc := pc;
    saved_stack := NEW(REF ARRAY OF StackValue, NUMBER(stack^));
    saved_stack^ := stack^
  END Save;

PROCEDURE Restore() =
  BEGIN
    fp := saved_fp;
    sp := saved_sp;
    cond := saved_cond;
    pc := saved_pc;
    stack := NEW(REF ARRAY OF StackValue, NUMBER(saved_stack^));
    stack^ := saved_stack^
  END Restore;

PROCEDURE GetStackSize(): CARDINAL =
  BEGIN
    RETURN stackSize
  END GetStackSize;

PROCEDURE EnlargeStack() =
  VAR newStack := NEW(REF ARRAY OF StackValue, NUMBER(stack^) * 2); BEGIN
    SUBARRAY(newStack^, 0, NUMBER(stack^)) := stack^;
    stack := newStack;
    stackSize := NUMBER(stack^)
  END EnlargeStack;

(* -------------------------------- EXEC ----------------------------------- *)

TYPE
  Point = RECORD x, y: Real END;
  Segment = RECORD a, b: Point END;

PROCEDURE ExtractSegment(seg: RTVal.T; VAR (* OUT *) out: Segment):
    BOOLEAN =
(* If "seg" is a pair of points, then set "out" to contain the four
   coordinates and return "FALSE". Otherwise, return "TRUE". *)
  BEGIN
    TYPECASE seg OF
    | NULL => RETURN TRUE
    | RTVal.Pair (p) =>
        IF ExtractPoint(p.car, out.a) THEN RETURN TRUE END;
        IF ExtractPoint(p.cdr, out.b) THEN RETURN TRUE END;
    ELSE RETURN TRUE
    END;
    RETURN FALSE
  END ExtractSegment;

PROCEDURE ExtractPoint(pt: RTVal.T; VAR (* OUT *) out: Point): BOOLEAN =
(* If "pt" is a pair of real numbers, then set "out" to contain its
   coordinates and return "FALSE". Otherwise, return "TRUE". *)
  BEGIN
    TYPECASE pt OF
    | NULL => RETURN TRUE
    | RTVal.Pair (p) =>
        IF ExtractReal(p.car, out.x) THEN RETURN TRUE END;
        IF ExtractReal(p.cdr, out.y) THEN RETURN TRUE END;
    ELSE RETURN TRUE
    END;
    RETURN FALSE
  END ExtractPoint;

PROCEDURE ExtractReal(r: RTVal.T; VAR (* OUT *) out: Real): BOOLEAN =
(* If "r" is a number, set "out" to its value and return "FALSE". Otherwise,
   return "TRUE". *)
  BEGIN
    TYPECASE r OF
    | NULL => RETURN TRUE
    | RTVal.Number (v) => out := v.val
    ELSE RETURN TRUE
    END;
    RETURN FALSE
  END ExtractReal;

PROCEDURE AppendClosure(l1, l2: RTVal.T): RTVal.T =
(* Return the result of appending the list "l2" to the end of the closure "l1",
   or NIL if "l1" is not a list. *)
  BEGIN
    IF ListLen(l1) = -1
      THEN RETURN NIL
      ELSE RETURN Append(l1, l2)
    END
  END AppendClosure;

PROCEDURE ListLen(t: RTVal.T): INTEGER =
(* Returns the length of the list "t", or -1 if "t" is not a list. *)
  VAR res := 0; BEGIN
    LOOP
      TYPECASE t OF
      | NULL => EXIT
      | RTVal.Pair (p) =>
        INC(res);
        t := p.cdr
      ELSE EXIT
      END
    END;
    IF t = RTVal.nil
      THEN RETURN res
      ELSE RETURN -1
    END
  END ListLen;

PROCEDURE Append(l1, l2: RTVal.T): RTVal.T =
(* Requires "l1" and "l2" to be lists; returns the result of appending them. *)
  BEGIN
    TYPECASE l1 OF <* NOWARN *>
      RTVal.Null => RETURN l2
    | RTVal.Pair (p) =>
        RETURN RTVal.FromPair(p.car, Append(p.cdr, l2))
    END
  END Append;

VAR
  machine := NEW(MUTEX);
  isRunning := FALSE;
  intPending := FALSE;
  intSeen := NEW(Thread.Condition);

PROCEDURE Interrupt() =
(* Send an interrupt; block until the machine signals that the interrupt has
   been seen. *)
  BEGIN
    LOCK machine DO
      IF NOT isRunning THEN RETURN END;
      intPending := TRUE;
      WHILE intPending DO Thread.Wait(machine, intSeen) END
    END
  END Interrupt;

VAR instrCount: INTEGER;

PROCEDURE EtpLogExecStep(<*UNUSED*> bc: ByteCode) =
  BEGIN END EtpLogExecStep;

PROCEDURE EtpLogExecInstrCount(<*UNUSED*> cnt: INTEGER) =
  BEGIN END EtpLogExecInstrCount;

PROCEDURE Exec(): ExecRes =
  VAR
    a: BytePtr := ADR(code_tbl[pc.proc][pc.offset]);
    bc: ByteCode;
    offset: JunoMarshal.Short;
    index, extSlot: CARDINAL;
    a_init: BytePtr;
    (* "a_init" always contains the value "a" had at the beginning of the
       instruction, so that the instruction can be restarted in case of a
       stack overflow.  See the "EXCEPT" clause below. *)
    a_ut: BytePtr;
    (* "a_ut" is the address of the most recent instruction that branches
       to its offset because one of its argument is an undefined term *)

  PROCEDURE HandleError(ec: JunoRTError.Code): ExecRes =
  (* Return a "ExecRes" in the "TrapCode.Error" case with error code "ec". On
     entry to this routine, "a" should be pointing to the first byte of the
     instruction just after the one causing the error. *)
    VAR startLoc := ADR(code_tbl[pc.proc][0]); eLoc: PC; BEGIN
      pc.offset := a - startLoc;
      IF ec = JunoRTError.Code.UndefTerm
        THEN eLoc := PC{pc.proc, a_ut - startLoc}
        ELSE eLoc := PC{pc.proc, a_init - startLoc}
      END;
      RecomputeFrames();
      RETURN ExecRes{TrapCode.Error, eLoc, extSlot, ec}
    END HandleError;

  PROCEDURE HandleIntr(): ExecRes =
  <* SPEC machine IN LL *>
    BEGIN
      intPending := FALSE;
      Thread.Broadcast(intSeen);
      pc.offset := a - ADR(code_tbl[pc.proc][0]);
      RecomputeFrames();
      RETURN ExecRes{TrapCode.Interrupt, pc, extSlot, NoErrorCode}
    END HandleIntr;

  PROCEDURE MkName(m, n: Atom.T): TEXT =
    VAR res := Atom.ToText(n); BEGIN
      IF m # NIL THEN res := Atom.ToText(m) & "." & res END;
      RETURN res
    END MkName;

  PROCEDURE MakeClosure(s: INTEGER) RAISES {StackOverflow}=
  (* Push an empty closure for slot "s" *)
    <* FATAL PushedNIL *>
    VAR nm: TEXT; BEGIN
      IF s > 0
        THEN nm := MkName(procAttrs[s].modName, procAttrs[s].name)
        ELSE nm := MkName(extProcAttrs[-s].modName, extProcAttrs[-s].name)
      END;
      Push(RTVal.FromPair(
        RTVal.FromPair(RTVal.FromText(nm), RTVal.FromInt(s)),
        RTVal.nil))
    END MakeClosure;

  PROCEDURE DoCall(slot: CARDINAL) RAISES {StackOverflow} =
    <* FATAL PushedNIL *>
    VAR
      firstaddr := ADR(code_tbl[pc.proc][0]);
      iS := MakeCallState(
        pc := PC{proc := pc.proc, offset := a - firstaddr},
        fp := fp);
    BEGIN
      Push(iS);
      fp := sp - 1;   (* Do this here in case "Push" fails *)
      pc.proc := slot;
      pc.offset := 0;
      a := ADR(code_tbl[slot][0]);
      WITH sig = procAttrs[slot].sig DO
        IF sig.outs = 0 AND sig.inouts = 0 THEN
          RTVal.Mark()
        END
      END
    END DoCall;

  PROCEDURE DoExtCall(slot: CARDINAL) =
    BEGIN
      ext_fp := sp;
      extSlot := slot;
      cond := ext_code_tbl[slot].invoke();
    END DoExtCall;

  (* Exec *)
  BEGIN
   instrCount := 0;
   LOCK machine DO
     <* ASSERT NOT isRunning *>
     isRunning := TRUE
   END;
   TRY
    IF pc.proc = InitialProc THEN
      RecomputeFrames();
      RETURN ExecRes{TrapCode.NormalHalt, NoTrapLoc, 0, NoErrorCode}
    END;
    TRY
      LOOP
        a_init := a;
        bc := a^;
        INC(a);
        EtpLogExecStep(bc); INC(instrCount);
        CASE bc OF
        | BC.PUSHL =>
            offset := JunoMarshal.ReadShort(a);
            Push(stack[fp + offset])
        | BC.POPL =>
            offset := JunoMarshal.ReadShort(a);
            stack[fp + offset] := Pop()
        | BC.PUSHG =>
            index := JunoMarshal.ReadULong(a);
            Push(RTVal.FromJV(value_tbl[index]))
        | BC.POPG =>
            index := JunoMarshal.ReadULong(a);
            value_tbl[index] := RTVal.ToJV(Pop())
        | BC.INCSP =>
            IF sp + a^ >= stackSize THEN RAISE StackOverflow END;
            INC(sp, a^);
            INC(a)
        | BC.DECSP =>
            DEC(sp, a^); INC(a)
        | BC.PUSHM3NIL =>
            IF sp + a^ >= stackSize THEN RAISE StackOverflow END;
            FOR i := 1 TO a^ DO stack[sp] := NIL; INC(sp) END;
            INC(a)
        | BC.PUSHNIL =>
            Push(RTVal.nil)
        | BC.PUSHNUM =>
            Push(RTVal.FromReal(JunoMarshal.ReadReal(a)))
        | BC.C_OFF =>
            cond := FALSE
        | BC.C_ON =>
            cond := TRUE
        | BC.JUMP =>
            offset := JunoMarshal.ReadShort(a);
            IF offset <= 0 THEN
              LOCK machine DO
                IF intPending THEN RETURN HandleIntr() END
              END
            END;
            INC(a, offset)
        | BC.TJUMP =>
            offset := JunoMarshal.ReadShort(a);
            IF cond THEN INC(a, offset) END
        | BC.FJUMP =>
            offset := JunoMarshal.ReadShort(a);
            IF NOT cond THEN INC(a, offset) END
        | BC.UJUMP =>
            offset := JunoMarshal.ReadShort(a);
            IF NOT cond THEN
              DEC(sp);			 (* skip past single OUT parameter *)
              a_ut := a_init;		 (* set "undef term" address *)
              INC(a, offset)
            END
        | BC.CALL =>
            LOCK machine DO
              IF intPending THEN
                RETURN HandleIntr()
              END
            END;
            DoCall(JunoMarshal.ReadULong(a))
        | BC.CALLEXT =>
            DEC(instrCount);
            DoExtCall(JunoMarshal.ReadULong(a));
            IF NOT cond THEN
              RETURN HandleError(JunoRTError.Code.FailedExtProc)
            END
        | BC.RET =>
            WITH sig = procAttrs[pc.proc].sig DO
              IF sig.outs = 0 AND sig.inouts = 0 THEN
                RTVal.Dispose()
              END
            END;
            sp := fp + 1;
            VAR s: CallState := Pop(); BEGIN
              pc := s.pc;
              fp := s.fp;
              DisposeCallState(s)
            END;
            IF pc.proc = InitialProc THEN
              RecomputeFrames();
              RETURN ExecRes{TrapCode.NormalHalt, NoTrapLoc, 0, NoErrorCode}
            END;
            a := ADR(code_tbl[pc.proc][pc.offset])
        | BC.ERROR =>
            VAR ec := a^; BEGIN
              INC(a); RETURN HandleError(VAL(ec, JunoRTError.Code))
            END
        | BC.FERROR =>
            IF cond THEN INC(a) ELSE
              VAR ec := a^; BEGIN
                INC(a); RETURN HandleError(VAL(ec, JunoRTError.Code))
              END
            END
        | BC.ADD =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t2, t1 := PopNum(err); BEGIN
              IF NOT err
                THEN Push(RTVal.FromReal(t1 + t2))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.SUBTRACT =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t2, t1 := PopNum(err); BEGIN
              IF NOT err
                THEN Push(RTVal.FromReal(t1 - t2))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.MULTIPLY =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t2, t1 := PopNum(err); BEGIN
              IF NOT err
                THEN Push(RTVal.FromReal(t1 * t2))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.DIVIDE =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t2, t1 := PopNum(err); BEGIN
              IF NOT err AND t2 # 0.0
                THEN Push(RTVal.FromReal(t1 / t2))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.DIV_ =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t2, t1 := PopNum(err); BEGIN
              IF NOT err AND t2 # 0.0
                THEN Push(RTVal.FromInt(FLOOR(t1 / t2)))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.MOD_ =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t2, t1 := PopNum(err); BEGIN
              IF NOT err AND t2 # 0.0
                THEN Push(RTVal.FromReal(t1 MOD t2))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.NEGATE =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t1 := PopNum(err); BEGIN
              IF NOT err
                THEN Push(RTVal.FromReal(-t1))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.ABS_ =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t1 := PopNum(err); BEGIN
              IF NOT err
                THEN Push(RTVal.FromReal(ABS(t1)))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.FLOOR_ =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t1 := PopNum(err); BEGIN
              IF NOT err
                THEN Push(RTVal.FromInt(FLOOR(t1)))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.CEILING_ =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t1 := PopNum(err); BEGIN
              IF NOT err
                THEN Push(RTVal.FromInt(CEILING(t1)))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.ROUND_ =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t1 := PopNum(err); BEGIN
              IF NOT err
                THEN Push(RTVal.FromInt(ROUND(t1)))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.MAX_ =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t2, t1 := PopNum(err); BEGIN
              IF NOT err
                THEN Push(RTVal.FromReal(MAX(t1, t2)))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.MIN_ =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t2, t1 := PopNum(err); BEGIN
              IF NOT err
                THEN Push(RTVal.FromReal(MIN(t1, t2)))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.ATAN =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t2, t1 := PopNum(err); BEGIN
              IF NOT err
                THEN Push(RTVal.FromReal(JunoValue.Atan(t1, t2)))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.SIN =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t1 := PopNum(err); BEGIN
              IF NOT err
                THEN Push(RTVal.FromReal(JunoValue.Sin(t1)))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.COS =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t1 := PopNum(err); BEGIN
              IF NOT err
                THEN Push(RTVal.FromReal(JunoValue.Cos(t1)))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.LN =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t1 := PopNum(err); BEGIN
              IF NOT err
                THEN Push(RTVal.FromReal(JunoValue.Ln(t1)))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.EXP =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t1 := PopNum(err); BEGIN
              IF NOT err
                THEN Push(RTVal.FromReal(JunoValue.Exp(t1)))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.REL =>           (* Evaluate (x, y) REL ((ax, ay), (bx, by)) *)
            offset := JunoMarshal.ReadShort(a);
            VAR t2, t1 := Pop(); p: Point; s: Segment; BEGIN
              IF ExtractPoint(t1, p) OR ExtractSegment(t2, s) THEN
                a_ut := a_init;
                INC(a, offset)
              ELSE
                WITH a = s.a, b = s.b DO
                  Push(RTVal.FromPair(
                    RTVal.FromReal(a.x + (b.x-a.x)*p.x - (b.y-a.y)*p.y),
                    RTVal.FromReal(a.y + (b.x-a.x)*p.y + (b.y-a.y)*p.x)))
                END
              END
            END
        | BC.CAR =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; pr := PopPair(err); BEGIN
              IF NOT err
                THEN Push(pr.car)
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.CDR =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; pr := PopPair(err); BEGIN
              IF NOT err
                THEN Push(pr.cdr)
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.CAR_CDR =>
            offset := JunoMarshal.ReadShort(a);
            (* This is the only place we test "sp" because this is the
               only bytecode that pops stuff off the stack and pushes
               even more stuff back *)
            IF sp = stackSize THEN
              VAR err := FALSE; pr := PopPair(err); BEGIN
            	IF NOT err
            	  THEN Push(pr.cdr); Push(pr.car)
            	  ELSE a_ut := a_init; INC(a, offset)
            	END
              END
            ELSE
              RAISE StackOverflow
            END
        | BC.CONS =>
            VAR t2, t1 := Pop(); BEGIN
              Push(RTVal.FromPair(t1, t2))
            END
        | BC.LIST =>
            Push(PopList(JunoMarshal.ReadUShort(a)))
        | BC.CONCAT =>
            offset := JunoMarshal.ReadShort(a);
            VAR err := FALSE; t2, t1 := PopText(err); BEGIN
              IF NOT err
                THEN Push(RTVal.FromText(t1 & t2))
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.IS_REAL =>
            VAR t := stack[sp - 1]; BEGIN
              cond := t # NIL AND ISTYPE(t, RTVal.Number)
            END
        | BC.IS_INT =>
            TYPECASE stack[sp - 1] OF
              NULL => cond := FALSE
            | RTVal.Number (n) => cond := (n.val = FLOAT(ROUND(n.val)))
            ELSE cond := FALSE
            END
        | BC.IS_TEXT =>
            VAR t := stack[sp - 1]; BEGIN
              cond := t # NIL AND ISTYPE(t, RTVal.Text)
            END
        | BC.IS_PAIR =>
            VAR t := stack[sp - 1]; BEGIN
              cond := t # NIL AND ISTYPE(t, RTVal.Pair)
            END
        | BC.EQUAL =>
            VAR t2, t1 := Pop(); BEGIN
              cond := RTVal.Equal(t1, t2)
            END
        | BC.LESS =>
            VAR err := FALSE; t2, t1 := PopNum(err); BEGIN
              cond := (NOT err) AND (t1 < t2)
            END
        | BC.AT_MOST =>
            VAR err := FALSE; t2, t1 := PopNum(err); BEGIN
              cond := (NOT err) AND (t1 <= t2)
            END
        | BC.CONG =>
            VAR t2, t1 := Pop(); s1, s2: Segment; BEGIN
              IF ExtractSegment(t1, s1) OR ExtractSegment(t2, s2) THEN
                cond := FALSE
              ELSE
                PROCEDURE LenSq(READONLY s: Segment): Real =
                  VAR dx := s.b.x - s.a.x; dy := s.b.y - s.a.y; BEGIN
                    RETURN dx * dx + dy * dy
                  END LenSq;
                CONST Epsilon = 2.0E-3;
                VAR len1 := LenSq(s1); len2 := LenSq(s2); BEGIN
                  cond := ABS(len2 - len1) < Epsilon
                END
              END
            END
        | BC.PARA =>
            VAR t2, t1 := Pop(); s1, s2: Segment; BEGIN
              IF ExtractSegment(t1, s1) OR ExtractSegment(t2, s2) THEN
                cond := FALSE
              ELSE
                CONST Epsilon = 1.0E-4;
                VAR
                  dx1 := s1.b.x - s1.a.x; dy1 := s1.b.y - s1.a.y;
                  dx2 := s2.b.x - s2.a.x; dy2 := s2.b.y - s2.a.y;
                BEGIN
                  cond := ABS(dx2 * dy1 - dx1 * dy2) < Epsilon
                END
              END
            END
        | BC.HOR =>
            VAR t2, t1 := Pop(); p1, p2: Point; BEGIN
              IF ExtractPoint(t1, p1) OR ExtractPoint(t2, p2)
                THEN cond := FALSE
                ELSE cond := p1.y = p2.y
              END
            END
        | BC.VER =>
            VAR t2, t1 := Pop(); p1, p2: Point; BEGIN
              IF ExtractPoint(t1, p1) OR ExtractPoint(t2, p2)
                THEN cond := FALSE
                ELSE cond := p1.x = p2.x
              END
            END
        | BC.NEWCL =>    MakeClosure(JunoMarshal.ReadULong(a))
        | BC.NEWEXTCL => MakeClosure(-JunoMarshal.ReadULong(a))
        | BC.CLOSE =>
            VAR
              l := PopList(JunoMarshal.ReadUShort(a));
              cl := Pop();
              offset := JunoMarshal.ReadShort(a);
            BEGIN
              cl := AppendClosure(cl, l);
              IF cl # NIL
                THEN Push(cl)
                ELSE a_ut := a_init; INC(a, offset)
              END
            END
        | BC.APPLY =>
            VAR
              outs := JunoMarshal.ReadUShort(a);
              inouts := JunoMarshal.ReadUShort(a);
              ins := JunoMarshal.ReadUShort(a);
              offset := JunoMarshal.ReadShort(a);
              clRec := UnpackClosure(Pop());
            BEGIN
              IF clRec.valid AND ins + clRec.argsLen = clRec.sig.ins
                 AND outs = clRec.sig.outs AND inouts = clRec.sig.inouts THEN
                NARROW(stack[fp], CallState).clIns := clRec.argsLen;
                InsertList(clRec.args, clRec.argsLen, ins);
                IF clRec.slot > 0 THEN
                  DoCall(clRec.slot)
                ELSE
                  DoExtCall(-clRec.slot);
                  IF NOT cond THEN
                    RETURN HandleError(JunoRTError.Code.FailedExtProc)
                  END
                END
              ELSE
                a_ut := a_init;
                INC(a, offset)
              END
            END
        | BC.CLDECSP =>
            DEC(sp, NARROW(stack[fp], CallState).clIns)
        | BC.SOLVE =>
            DEC(instrCount);
            DoSolve(a)
        ELSE
            Wr.PutText(stderr, "Fatal error in JunoRT.Exec:\n");
            Wr.PutText(stderr, "  Unknown byte code " & Fmt.Int(bc) & "\n");
            pc.offset := a_init - ADR(code_tbl[pc.proc][0]);
            Wr.PutText(stderr, "  PC = [" & PCToProcName(pc)
              & ", " & Fmt.Int(pc.offset) & "]\n\n");
            Wr.PutText(stderr, "Procedure disassembly:\n");
            Wr.Flush(stderr);
            JunoDisassem.P(code_tbl[pc.proc], stderr);
            Wr.Flush(stderr);
            <* ASSERT FALSE *>
        END
      END
    EXCEPT
    | StackOverflow =>
        pc.offset := a_init - ADR(code_tbl[pc.proc][0]);
        RecomputeFrames();
        RETURN ExecRes{TrapCode.StackOverflow, pc, 0, NoErrorCode}
    | PushedNIL =>
        RETURN HandleError(JunoRTError.Code.UsedUninitialized)
    END
   FINALLY
    LOCK machine DO
      isRunning := FALSE
    END;
    EtpLogExecInstrCount(instrCount);
    RTVal.Dispose()
   END
  END Exec;

TYPE
  ClosureRec = RECORD
    valid: BOOLEAN;
    slot: INTEGER;
    args: JunoValue.T;
    argsLen: CARDINAL;
    sig: Sig;
  END;

PROCEDURE UnpackClosure(cl: RTVal.T): ClosureRec =
  VAR res: ClosureRec; BEGIN
    TYPECASE cl OF RTVal.Pair (p) =>
      TYPECASE p.car OF RTVal.Pair (q) =>
        TYPECASE q.cdr OF RTVal.Number (s) =>
          res.slot := ROUND(s.val);
          IF (res.slot > 0 AND res.slot < next_code_idx) THEN
            res.sig := procAttrs[res.slot].sig
          ELSIF res.slot <= 0 AND -res.slot < next_ext_idx THEN
            res.sig := extProcAttrs[-res.slot].sig
          ELSE
            res.valid := FALSE; RETURN res
          END;
          res.argsLen := ListLen(p.cdr);
          IF res.argsLen >= 0 THEN
            res.args := p.cdr;
            res.valid := TRUE;
            RETURN res
          END
        ELSE (*SKIP*)
        END
      ELSE (*SKIP*)
      END
    ELSE (*SKIP*)
    END;
    res.valid := FALSE;
    RETURN res
  END UnpackClosure;

PROCEDURE ExecFromSlot(slot: CARDINAL; reset := TRUE): ExecRes =
  BEGIN
    IF reset THEN ResetMachine() END;
    PushFrame(PC{slot, 0}, size := 0);
    RETURN Exec()
  END ExecFromSlot;

PROCEDURE AttrsToProcName(READONLY p: ProcAttr): TEXT =
  VAR res := Atom.ToText(p.name); BEGIN
    IF p.modName # NIL THEN res := Atom.ToText(p.modName) & "." & res END;
    RETURN res
  END AttrsToProcName;

PROCEDURE PCToProcName(READONLY pc: PC): TEXT =
  BEGIN RETURN AttrsToProcName(procAttrs[pc.proc]) END PCToProcName;

PROCEDURE ExtSlotToProcName(slot: CARDINAL): TEXT =
  BEGIN RETURN AttrsToProcName(extProcAttrs[slot]) END ExtSlotToProcName;

PROCEDURE RunTimeError(READONLY execRes: ExecRes): TEXT =
  BEGIN
    IF execRes.errorCode = JunoRTError.Code.FailedExtProc THEN
      RETURN "Built-in procedure \"" & ExtSlotToProcName(execRes.extSlot)
        & "\" failed"
    ELSE
      RETURN JunoRTError.names[execRes.errorCode]
    END
  END RunTimeError;

PROCEDURE TrapMessage(READONLY execRes: ExecRes): TEXT =
  PROCEDURE Msg(prefix: TEXT; READONLY eRes: ExecRes): TEXT =
    BEGIN RETURN prefix & " in \"" & PCToProcName(eRes.trapLoc) & "\"" END Msg;
  BEGIN
    CASE execRes.trapCode OF
      TrapCode.NormalHalt =>    RETURN "Execution completed successfully"
    | TrapCode.BreakPoint =>    RETURN Msg("Hit breakpoint", execRes)
    | TrapCode.Interrupt =>     RETURN Msg("Juno stopped", execRes)
    | TrapCode.StackOverflow => RETURN Msg("Stack Overflow", execRes)
    | TrapCode.Error =>
        RETURN Msg("Run-time error", execRes)
          & ":\n" & RunTimeError(execRes)
    END
  END TrapMessage;

VAR
  con := NEW(JunoSolve.Constraints, 100);
  var := NEW(JunoSolve.Vars, 100);

PROCEDURE DoSolve(VAR a: BytePtr) =
  VAR
    inouts := JunoMarshal.ReadUShort(a);
    ins := JunoMarshal.ReadUShort(a);
    nc := JunoMarshal.ReadUShort(a);
    nv := ins + inouts;
    params: CARDINAL := sp - nv;
    c: CARDINAL := 0;
  BEGIN
    (* check that "con" and "var" are large enough *)
    IF NUMBER(con^) < nc THEN
      con := NEW(JunoSolve.Constraints, MAX(nc, 2 * NUMBER(con^)))
    END;
    IF NUMBER(var^) < nv THEN
      var := NEW(JunoSolve.Vars, MAX(nv, 2 * NUMBER(var^)))
    END;

    (* create variables *)
    FOR i := 0 TO nv - 1 DO
      var[i] := JunoSolve.New(
        known := (i >= inouts),
        val := stack[params + i])
    END;

    (* process constraints *)
    WHILE c < nc DO
      VAR n: BC.ConRange := a^; x, y, z: JunoMarshal.Short := 0; BEGIN
        INC(a);
        x := JunoMarshal.ReadUShort(a);
        IF n < BC.REAL_C  THEN y := JunoMarshal.ReadUShort(a) END;
        IF n < BC.EQUAL_C THEN z := JunoMarshal.ReadUShort(a) END;
        CASE n OF
        | BC.CONS_C => con[c] := JunoSolve.NewCons(var[x], var[y], var[z])
        | BC.SUM_C =>  con[c] := JunoSolve.NewPlus(var[x], var[y], var[z])
        | BC.PROD_C => con[c] := JunoSolve.NewTimes(var[x], var[y], var[z])
        | BC.ATAN_C => con[c] := JunoSolve.NewAtan(var[x], var[y], var[z])
        | BC.EQUAL_C =>con[c] := JunoSolve.NewEqual(var[x], var[y])
        | BC.SIN_C =>  con[c] := JunoSolve.NewSin(var[x], var[y])
        | BC.COS_C =>  con[c] := JunoSolve.NewCos(var[x], var[y])
        | BC.EXP_C =>  con[c] := JunoSolve.NewExp(var[x], var[y])
        | BC.REAL_C => con[c] := JunoSolve.NewReal(var[x])
        | BC.TEXT_C => con[c] := JunoSolve.NewText(var[x])
        END;
        INC(c)
      END
    END;

    (* solve constraints *)
    cond := JunoSolve.P(SUBARRAY(con^, 0, nc));

    (* if successful, store results on the stack *)
    IF cond THEN
      FOR i := 0 TO inouts - 1 DO
        stack[params + i] := var[i].val
      END
    END;
    JunoSolve.Dispose();
    sp := sp - ins
  END DoSolve;

(* ------------------ "CallState" OPERATIONS --------------------------- *)

VAR csAvail: CallState := NIL;

PROCEDURE MakeCallState(pc: PC; fp: CARDINAL): CallState =
  VAR res: CallState; BEGIN
    IF csAvail = NIL THEN
      res := NEW(CallState)
    ELSE
      res := csAvail;
      csAvail := csAvail.nextAvail
    END;
    res.pc := pc;
    res.fp := fp;
    RETURN res
  END MakeCallState;

PROCEDURE DisposeCallState(st: CallState) =
  BEGIN
    st.nextAvail := csAvail;
    csAvail := st
  END DisposeCallState;

BEGIN
  value_tbl := NEW(REF ARRAY OF JunoValue.T, 1000);
  code_tbl := NEW(REF ARRAY OF ByteStream, 1000);
  procAttrs := NEW(REF ARRAY OF ProcAttr, 1000);
  ext_code_tbl := NEW(REF ARRAY OF ExternalCode, 100);
  extProcAttrs := NEW(REF ARRAY OF ProcAttr, 100);
  ResetMachine()
END JunoRT.
