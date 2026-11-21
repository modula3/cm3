MODULE MSIRToM3CG;

IMPORT MSIR, MSIRPrinter, M3CG, M3CG_Ops, M3ID, M3RT, Target, TInt, TextRefTbl, Fmt,
       Text, Word, Wr, Thread;

FROM MSIR IMPORT Op, TypeKind, ValueKind;

CONST
  (* Ops whose M3CG lowering is direct: one or a few M3CG ops each. *)
  Direct = SET OF Op {
    Op.Alloca, Op.Load, Op.Store, Op.GcLoad, Op.GcStore, Op.GcDirty,
    Op.FieldAddr,
    Op.IAdd, Op.ISub, Op.IMul, Op.IDiv, Op.IMod, Op.IUDiv, Op.IURem,
    Op.IAnd, Op.IOr, Op.IXor, Op.IShl, Op.ILShr, Op.IAShr, Op.IRotL, Op.IRotR,
    Op.Select,
    Op.FAdd, Op.FSub, Op.FMul, Op.FDiv, Op.FNeg, Op.FPAbs,
    Op.ICmp, Op.FCmp,
    Op.Br, Op.CondBr, Op.Ret, Op.Unreachable,
    Op.Call, Op.CallIndirect, Op.Dispatch,
    Op.Convert, Op.SIToFP, Op.FPToSI, Op.FPExt, Op.FPTrunc,
    Op.ZExt, Op.SExt, Op.Trunc,
    Op.PtrAdd, Op.GepByte, Op.ArrayElemAddr,
    Op.SubscriptCheck, Op.NilCheck, Op.RangeCheck,
    Op.OpenArraySize, Op.OpenArrayElemAddr,
    Op.AtomicFence, Op.AtomicLoad, Op.AtomicStore, Op.AtomicRMW,
    Op.AtomicCmpXchg };

  EH = SET OF Op {
    Op.Invoke, Op.InvokeIndirect, Op.LandingPad, Op.ExtractValue,
    Op.Resume, Op.Raise, Op.UnwindTo, Op.RetThroughEnvelope };

  (* Kinds that are not a single M3CG stack value. *)
  Aggregate = SET OF TypeKind {
    TypeKind.IWide, TypeKind.Struct, TypeKind.Object, TypeKind.OpenArray,
    TypeKind.HeapArray, TypeKind.FixedArray, TypeKind.Set };

  KindText = ARRAY TypeKind OF TEXT {
    "void", "i1", "i8", "i16", "i32", "i64", "iwide", "w8", "w16", "w32",
    "w64", "f32", "f64", "f128", "ptr", "gcref", "gcslot", "struct", "object",
    "openarray", "heaparray", "fixedarray", "subrange", "set", "enum",
    "proctype" };

TYPE
  Features = REF RECORD
    n    : INTEGER := 0;
    list : ARRAY [0..63] OF TEXT;
  END;

PROCEDURE Note (f: Features;  what: TEXT) =
  BEGIN
    FOR i := 0 TO f.n - 1 DO
      IF Text.Equal (f.list[i], what) THEN RETURN END;
    END;
    IF f.n < NUMBER (f.list) THEN f.list[f.n] := what;  INC (f.n) END;
  END Note;

PROCEDURE NoteType (f: Features;  prefix: TEXT;  t: MSIR.T) =
  VAR k: TypeKind;
  BEGIN
    IF t = NIL THEN RETURN END;
    k := MSIR.Kind (t);
    IF k IN Aggregate THEN Note (f, prefix & KindText[k]) END;
  END NoteType;

PROCEDURE HasPrefix (t, p: TEXT): BOOLEAN =
  BEGIN
    RETURN Text.Length (t) >= Text.Length (p)
       AND Text.Equal (Text.Sub (t, 0, Text.Length (p)), p);
  END HasPrefix;

PROCEDURE HasSuffix (t, s: TEXT): BOOLEAN =
  VAR n := Text.Length (t);  k := Text.Length (s);
  BEGIN
    RETURN n >= k AND Text.Equal (Text.Sub (t, n - k), s);
  END HasSuffix;

PROCEDURE NoteValue (f: Features;  ownInfo: TEXT;  v: MSIR.Value) =
  VAR name: TEXT;
  BEGIN
    IF v = NIL THEN RETURN END;
    name := MSIR.ValueName (v);
    CASE MSIR.GetValueKind (v) OF
    | ValueKind.ConstTextLit => Note (f, "textlit");
    | ValueKind.ConstStruct, ValueKind.ConstAggArray => Note (f, "const-agg");
    | ValueKind.GlobalRef => Note (f, "global-standalone");
    | ValueKind.StructFieldRef =>
        IF NOT Text.Equal (name, ownInfo) THEN Note (f, "global-imported") END;
    | ValueKind.InsnResult =>
        IF MSIR.ValueDefInsn (v) = NIL AND name # NIL
           AND HasPrefix (name, "@") THEN
          IF HasPrefix (name, "@tc_") THEN Note (f, "sym-typecell")
          ELSIF HasPrefix (name, "@constarray") THEN Note (f, "sym-constarray")
          ELSIF HasPrefix (name, "@textlit") THEN Note (f, "sym-textlit")
          ELSIF HasSuffix (name, "_info") THEN Note (f, "sym-modinfo")
          ELSE Note (f, "sym-other")
          END;
        END;
    ELSE
    END;
  END NoteValue;

PROCEDURE NoteInsn (f: Features;  ownInfo: TEXT;  i: MSIR.Insn) =
  VAR op := MSIR.InsnOp (i);
  BEGIN
    IF op IN EH THEN
      Note (f, "eh")
    ELSIF NOT op IN Direct THEN
      Note (f, "op:" & MSIRPrinter.OpText (op))
    END;
    IF MSIR.InsnResult (i) # NIL THEN
      NoteType (f, "ssa:", MSIR.ValueType (MSIR.InsnResult (i)));
    END;
    FOR k := 0 TO MSIR.InsnOperandCount (i) - 1 DO
      NoteValue (f, ownInfo, MSIR.InsnOperand (i, k));
    END;
    IF op = Op.Br OR op = Op.CondBr THEN
      FOR k := 0 TO ORD (op = Op.CondBr) DO
        FOR j := 0 TO MSIR.InsnBrArgCount (i, k) - 1 DO
          NoteValue (f, ownInfo, MSIR.InsnBrArg (i, k, j));
        END;
      END;
    END;
  END NoteInsn;

PROCEDURE Census (wr: Wr.T;  m: MSIR.Module) =
  VAR
    mod     := MSIR.ModuleName (m);
    ownInfo := "@" & MSIR.ModuleInfoName (m);
    p       : MSIR.Proc;
    b       : MSIR.Block;
    f       : Features;
    line    : TEXT;
  BEGIN
    FOR pi := 0 TO MSIR.ModuleProcCount (m) - 1 DO
      p := MSIR.ModuleProc (m, pi);
      IF MSIR.ProcBlockCount (p) > 0 THEN  (* skip bodyless declarations *)
        f := NEW (Features);
        IF MSIR.ProcItemCount (p) # MSIR.ProcBlockCount (p) THEN
          Note (f, "envelope");
        END;
        NoteType (f, "result:", MSIR.ProcResultType (p));
        FOR k := 0 TO MSIR.ProcParamCount (p) - 1 DO
          NoteType (f, "param:", MSIR.ValueType (MSIR.ProcParam (p, k)));
        END;
        FOR bi := 0 TO MSIR.ProcBlockCount (p) - 1 DO
          b := MSIR.ProcBlock (p, bi);
          FOR k := 0 TO MSIR.BlockParamCount (b) - 1 DO
            NoteType (f, "blockparam:",
                      MSIR.ValueType (MSIR.BlockParamValue (b, k)));
          END;
          FOR ii := 0 TO MSIR.BlockInsnCount (b) - 1 DO
            NoteInsn (f, ownInfo, MSIR.BlockInsn (b, ii));
          END;
        END;
        line := "msir-census: " & mod & " " & MSIR.ProcName (p);
        IF f.n = 0 THEN
          line := line & " ok";
        ELSE
          line := line & " blk";
          FOR k := 0 TO f.n - 1 DO line := line & " " & f.list[k] END;
        END;
        TRY
          Wr.PutText (wr, line & "\n");
        EXCEPT Wr.Failure, Thread.Alerted =>
        END;
      END;
    END;
  END Census;


(*=================================================================== Lower *)

(* The lowering follows CG's data conventions, so a lowered unit links with
   units compiled through the front end's own M3CG stream:
   - one data segment ("M_<Mod>" / "I_<Mod>"): RT0.ModuleInfo, then the
     user globals at the front end's offsets, then the RT0.ImportInfo
     records;
   - one constant segment: procedure names, the RT0.ProcInfo table, the
     source file name and the global GC map.
   SSA values live in locals.  Integers are held as 64-bit values,
   sign-extended from their MSIR width (i1 as 0/1), so every operation is
   computed at 64 bits and re-canonicalised. *)

TYPE
  Loc = REF RECORD seg: M3CG.Var;  off: INTEGER END;

  ValKind = {Temp, AddrOf, Param};
  ValLoc = REF RECORD
    kind : ValKind;
    var  : M3CG.Var;
    t    : MSIR.T;
  END;

  ProcRef = REF RECORD p: M3CG.Proc END;
  LabelRef = REF RECORD l: M3CG.Label END;

VAR
  (* State of one Lower call; the compiler is single-threaded here. *)
  cg        : M3CG_Ops.Public;
  mod       : MSIR.Module;
  modName   : TEXT;
  isIface   : BOOLEAN;
  constSeg  : M3CG.Var;
  dataSeg   : M3CG.Var;
  syms      : TextRefTbl.T;   (* symbol (no '@') -> Loc *)
  procs     : TextRefTbl.T;   (* C symbol -> ProcRef *)
  vals      : TextRefTbl.T;   (* SSA name -> ValLoc, per procedure *)
  blocks    : TextRefTbl.T;   (* block label -> LabelRef, per procedure *)
  binder    : M3CG.Proc;
  curLine   : INTEGER;

CONST
  Int64 = M3CG.Type.Int64;
  Word64 = M3CG.Type.Word64;
  Addr = M3CG.Type.Addr;

(*--------------------------------------------------------------- reasons *)

TYPE Why = OBJECT wr: Wr.T;  ok := TRUE;  mod: TEXT END;

PROCEDURE Fail (w: Why;  what: TEXT) =
  BEGIN
    IF w.ok AND w.wr # NIL THEN
      TRY
        Wr.PutText (w.wr, "msir-cg: " & w.mod & " fallback: " & what & "\n");
      EXCEPT Wr.Failure, Thread.Alerted =>
      END;
    END;
    w.ok := FALSE;
  END Fail;

(*------------------------------------------------------------------ types *)

PROCEDURE Base (t: MSIR.T): MSIR.T =
  BEGIN
    WHILE t # NIL AND MSIR.Kind (t) = TypeKind.Subrange DO
      t := MSIR.SubrangeParent (t);
    END;
    RETURN t;
  END Base;

PROCEDURE IsInt (t: MSIR.T): BOOLEAN =
  BEGIN
    t := Base (t);
    RETURN t # NIL AND MSIR.Kind (t) IN SET OF TypeKind {
             TypeKind.I1, TypeKind.I8, TypeKind.I16, TypeKind.I32,
             TypeKind.I64, TypeKind.W8, TypeKind.W16, TypeKind.W32,
             TypeKind.W64, TypeKind.Enum };
  END IsInt;

PROCEDURE IsPtr (t: MSIR.T): BOOLEAN =
  BEGIN
    t := Base (t);
    RETURN t # NIL AND MSIR.Kind (t) IN SET OF TypeKind {
             TypeKind.Ptr, TypeKind.GcRef, TypeKind.GcSlot, TypeKind.ProcType };
  END IsPtr;

PROCEDURE IsFloat (t: MSIR.T): BOOLEAN =
  BEGIN
    t := Base (t);
    RETURN t # NIL AND MSIR.IsFloat (MSIR.Kind (t));
  END IsFloat;

PROCEDURE IsScalar (t: MSIR.T): BOOLEAN =
  BEGIN
    RETURN IsInt (t) OR IsPtr (t) OR IsFloat (t);
  END IsScalar;

(* Bit width of an integer type; i1 is 1. *)
PROCEDURE IntBits (t: MSIR.T): INTEGER =
  BEGIN
    t := Base (t);
    IF MSIR.Kind (t) = TypeKind.Enum THEN RETURN MSIR.BitWidth (t) END;
    CASE MSIR.Kind (t) OF
    | TypeKind.I1 => RETURN 1;
    | TypeKind.I8, TypeKind.W8 => RETURN 8;
    | TypeKind.I16, TypeKind.W16 => RETURN 16;
    | TypeKind.I32, TypeKind.W32 => RETURN 32;
    ELSE RETURN 64;
    END;
  END IntBits;

PROCEDURE RType (t: MSIR.T): M3CG.RType =
  BEGIN
    CASE MSIR.Kind (Base (t)) OF
    | TypeKind.F32 => RETURN M3CG.Type.Reel;
    | TypeKind.F64 => RETURN M3CG.Type.LReel;
    ELSE RETURN M3CG.Type.XReel;
    END;
  END RType;

(* The operator type of a value of type t on the M3CG stack. *)
PROCEDURE ZType (t: MSIR.T): M3CG.ZType =
  BEGIN
    IF IsFloat (t) THEN RETURN RType (t) END;
    IF IsPtr (t) THEN RETURN Addr END;
    RETURN Int64;
  END ZType;

(* The memory type for loading or storing a value of type t.  Integers load
   signed, so the stack holds them sign-extended. *)
PROCEDURE MType (t: MSIR.T): M3CG.MType =
  BEGIN
    IF IsFloat (t) THEN RETURN RType (t) END;
    IF IsPtr (t) THEN RETURN Addr END;
    CASE IntBits (t) OF
    | 1 => RETURN M3CG.Type.Word8;
    | 8 => RETURN M3CG.Type.Int8;
    | 16 => RETURN M3CG.Type.Int16;
    | 32 => RETURN M3CG.Type.Int32;
    ELSE RETURN Int64;
    END;
  END MType;

(* The type a procedure's parameter or result is declared with. *)
PROCEDURE SigType (t: MSIR.T): M3CG.Type =
  BEGIN
    IF t = NIL OR MSIR.Kind (t) = TypeKind.Void THEN
      RETURN M3CG.Type.Void
    END;
    RETURN MType (t);
  END SigType;

(* M3RT's layout constants are in bits. *)
PROCEDURE B (bits: INTEGER): INTEGER =
  BEGIN
    RETURN bits DIV Target.Byte;
  END B;

PROCEDURE RoundUp (n, a: INTEGER): INTEGER =
  BEGIN
    IF a <= 1 THEN RETURN n END;
    RETURN (n + a - 1) DIV a * a;
  END RoundUp;

(* Byte size and alignment of t, or -1 if unknown. *)
PROCEDURE Size (t: MSIR.T;  VAR align: INTEGER): INTEGER =
  VAR n, a, sz, end: INTEGER;  f: MSIR.Field;
  BEGIN
    align := 1;
    IF t = NIL THEN RETURN -1 END;
    t := Base (t);
    IF IsPtr (t) THEN align := Target.Address.bytes; RETURN align END;
    IF IsFloat (t) THEN
      CASE MSIR.Kind (t) OF
      | TypeKind.F32 => align := 4;
      | TypeKind.F64 => align := 8;
      ELSE align := 16;
      END;
      RETURN align;
    END;
    IF IsInt (t) THEN
      align := MAX (1, IntBits (t) DIV 8);
      RETURN align;
    END;
    CASE MSIR.Kind (t) OF
    | TypeKind.FixedArray =>
        sz := Size (MSIR.FixedArrayElt (t), align);
        IF sz < 0 THEN RETURN -1 END;
        RETURN sz * MSIR.FixedArrayLen (t);
    | TypeKind.Struct =>
        end := 0;
        FOR i := 0 TO MSIR.StructFieldCount (t) - 1 DO
          f := MSIR.StructField (t, i);
          sz := Size (f.type, a);
          IF sz < 0 OR f.offset MOD 8 # 0 THEN RETURN -1 END;
          align := MAX (align, a);
          end := MAX (end, f.offset DIV 8 + sz);
        END;
        RETURN RoundUp (end, align);
    ELSE
      n := -1;
    END;
    RETURN n;
  END Size;

(*----------------------------------------------------------- C symbols *)

PROCEDURE ContainsDunder (n: TEXT): BOOLEAN =
  VAR i: INTEGER;
  BEGIN
    IF n = NIL THEN RETURN FALSE END;
    i := Text.FindChar (n, '_');
    WHILE i >= 0 AND i + 1 < Text.Length (n) DO
      IF Text.GetChar (n, i + 1) = '_' THEN RETURN TRUE END;
      i := Text.FindChar (n, '_', i + 1);
    END;
    RETURN FALSE;
  END ContainsDunder;

PROCEDURE DotsToUnderscore (n: TEXT): TEXT =
  VAR result := "";  start, dot: INTEGER;
  BEGIN
    start := 0;
    LOOP
      dot := Text.FindChar (n, '.', start);
      IF dot < 0 THEN RETURN result & Text.Sub (n, start) END;
      result := result & Text.Sub (n, start, dot - start) & "__";
      start := dot + 1;
    END;
  END DotsToUnderscore;

PROCEDURE IsOwnProc (p: MSIR.Proc): BOOLEAN =
  BEGIN
    FOR i := 0 TO MSIR.ModuleProcCount (mod) - 1 DO
      IF MSIR.ModuleProc (mod, i) = p THEN RETURN TRUE END;
    END;
    RETURN FALSE;
  END IsOwnProc;

PROCEDURE IsOwnProcName (n: TEXT): BOOLEAN =
  BEGIN
    FOR i := 0 TO MSIR.ModuleProcCount (mod) - 1 DO
      IF Text.Equal (MSIR.ProcName (MSIR.ModuleProc (mod, i)), n) THEN
        RETURN TRUE
      END;
    END;
    RETURN FALSE;
  END IsOwnProcName;

(* The linker name of p, as MSIRToLLVM.LLSymbol chooses it. *)
PROCEDURE CSymbol (p: MSIR.Proc): TEXT =
  VAR n := MSIR.ProcName (p);
  BEGIN
    IF n = NIL THEN n := "" END;
    IF IsOwnProc (p) THEN
      IF ContainsDunder (n) THEN RETURN n END;
      RETURN modName & "__" & n;
    ELSIF Text.FindChar (n, '.') >= 0 THEN
      RETURN DotsToUnderscore (n);
    ELSIF IsOwnProcName (n) THEN
      IF ContainsDunder (n)
         OR Text.Equal (n, modName & "_M3")
         OR Text.Equal (n, modName & "_I3") THEN
        RETURN n;
      END;
      RETURN modName & "__" & n;
    ELSE
      RETURN n;
    END;
  END CSymbol;

PROCEDURE ID (t: TEXT): M3ID.T =
  BEGIN
    RETURN M3ID.Add (t);
  END ID;

(*------------------------------------------------------------- checking *)

CONST
  Supported = SET OF Op {
    Op.Alloca, Op.Load, Op.Store, Op.GcLoad, Op.GcStore, Op.GcDirty,
    Op.FieldAddr,
    Op.IAdd, Op.ISub, Op.IMul, Op.IDiv, Op.IMod, Op.IUDiv, Op.IURem,
    Op.IAnd, Op.IOr, Op.IXor, Op.IShl, Op.ILShr, Op.IAShr, Op.IRotL, Op.IRotR,
    Op.Select,
    Op.FAdd, Op.FSub, Op.FMul, Op.FDiv, Op.FNeg, Op.FPAbs,
    Op.ICmp, Op.FCmp,
    Op.Br, Op.CondBr, Op.Ret, Op.Unreachable,
    Op.Call, Op.CallIndirect,
    Op.Convert, Op.SIToFP, Op.FPToSI, Op.FPExt, Op.FPTrunc,
    Op.ZExt, Op.SExt, Op.Trunc,
    Op.PtrAdd, Op.GepByte, Op.ArrayElemAddr };

PROCEDURE CheckSigType (w: Why;  what: TEXT;  t: MSIR.T) =
  BEGIN
    IF t = NIL OR MSIR.Kind (t) = TypeKind.Void THEN RETURN END;
    IF NOT IsScalar (t) THEN
      Fail (w, what & " " & KindText[MSIR.Kind (Base (t))]);
    END;
  END CheckSigType;

PROCEDURE CheckProcSig (w: Why;  p: MSIR.Proc) =
  BEGIN
    CheckSigType (w, "result", MSIR.ProcResultType (p));
    FOR k := 0 TO MSIR.ProcParamCount (p) - 1 DO
      CheckSigType (w, "param", MSIR.ValueType (MSIR.ProcParam (p, k)));
    END;
  END CheckProcSig;

PROCEDURE CheckValue (w: Why;  v: MSIR.Value) =
  VAR name: TEXT;
  BEGIN
    IF v = NIL THEN RETURN END;
    name := MSIR.ValueName (v);
    CASE MSIR.GetValueKind (v) OF
    | ValueKind.ConstInt, ValueKind.ConstFloat, ValueKind.ConstNil,
      ValueKind.Param, ValueKind.BlockParam =>
    | ValueKind.ConstProc => CheckProcSig (w, MSIR.GetConstProc (v));
    | ValueKind.StructFieldRef, ValueKind.InsnResult =>
        IF MSIR.ValueDefInsn (v) = NIL AND name # NIL
           AND HasPrefix (name, "@") THEN
          VAR r: REFANY;
          BEGIN
            IF NOT syms.get (Text.Sub (name, 1), r) THEN
              Fail (w, "symbol " & name);
            END;
          END;
        END;
    | ValueKind.GlobalRef => Fail (w, "global " & name);
    | ValueKind.ConstTextLit => Fail (w, "text literal");
    ELSE Fail (w, "constant aggregate");
    END;
    IF NOT IsScalar (MSIR.ValueType (v)) THEN
      Fail (w, "value " & KindText[MSIR.Kind (Base (MSIR.ValueType (v)))]);
    END;
  END CheckValue;

PROCEDURE CheckInsn (w: Why;  i: MSIR.Insn) =
  VAR op := MSIR.InsnOp (i);  res := MSIR.InsnResult (i);  a: INTEGER;
      t: MSIR.T;
  BEGIN
    IF NOT op IN Supported THEN
      Fail (w, "op " & MSIRPrinter.OpText (op));
      RETURN;
    END;
    IF res # NIL AND op # Op.Alloca AND NOT IsScalar (MSIR.ValueType (res)) THEN
      Fail (w, MSIRPrinter.OpText (op) & " result "
                 & KindText[MSIR.Kind (Base (MSIR.ValueType (res)))]);
    END;
    FOR k := 0 TO MSIR.InsnOperandCount (i) - 1 DO
      CheckValue (w, MSIR.InsnOperand (i, k));
    END;
    CASE op OF
    | Op.Br, Op.CondBr =>
        FOR k := 0 TO ORD (op = Op.CondBr) DO
          FOR j := 0 TO MSIR.InsnBrArgCount (i, k) - 1 DO
            CheckValue (w, MSIR.InsnBrArg (i, k, j));
          END;
        END;
    | Op.Alloca =>
        IF Size (MSIR.InsnTargetType (i), a) < 0 THEN
          Fail (w, "alloca of unsized type");
        END;
    | Op.Call => CheckProcSig (w, MSIR.InsnCallee (i));
    | Op.CallIndirect => CheckSigType (w, "result", MSIR.InsnTargetType (i));
    | Op.FieldAddr =>
        t := MSIR.ValueType (MSIR.InsnOperand (i, 0));
        IF IsPtr (t) THEN t := MSIR.EltType (Base (t)) END;
        IF t = NIL OR MSIR.Kind (t) # TypeKind.Struct THEN
          Fail (w, "field_addr on non-record");
        END;
    | Op.ArrayElemAddr =>
        t := MSIR.ValueType (MSIR.InsnOperand (i, 0));
        IF IsPtr (t) THEN t := MSIR.EltType (Base (t)) END;
        IF t = NIL OR MSIR.Kind (t) # TypeKind.FixedArray
           OR Size (MSIR.FixedArrayElt (t), a) < 0 THEN
          Fail (w, "array.elem_addr on unsized array");
        END;
    | Op.IRotL, Op.IRotR =>
        IF IntBits (MSIR.ValueType (res)) # 64 THEN Fail (w, "narrow rotate") END;
    | Op.FCmp =>
        IF NOT MSIR.InsnFCmpPred (i) IN SET OF MSIR.FCmpPred {
                 MSIR.FCmpPred.OEq, MSIR.FCmpPred.OLt, MSIR.FCmpPred.OLe,
                 MSIR.FCmpPred.OGt, MSIR.FCmpPred.OGe, MSIR.FCmpPred.UNe } THEN
          Fail (w, "fcmp predicate");
        END;
    | Op.Convert =>
        VAR s := MSIR.ValueType (MSIR.InsnOperand (i, 0));
            d := MSIR.InsnTargetType (i);
        BEGIN
          IF d = NIL THEN d := MSIR.ValueType (res) END;
          IF IsFloat (s) # IsFloat (d) AND (IsPtr (s) OR IsPtr (d)
             OR IntBits (s) # 64 OR IntBits (d) # 64
             OR MSIR.Kind (Base (s)) = TypeKind.F32
             OR MSIR.Kind (Base (d)) = TypeKind.F32) THEN
            Fail (w, "convert between float and non-64-bit");
          END;
        END;
    ELSE
    END;
  END CheckInsn;

PROCEDURE Lowerable (m: MSIR.Module;  why: Wr.T := NIL): BOOLEAN =
  VAR w := NEW (Why, wr := why, mod := MSIR.ModuleName (m));
      p: MSIR.Proc;  b: MSIR.Block;  v: MSIR.Value;
  BEGIN
    Setup (m);
    IF MSIR.ModuleTypeDescCount (m) > 0 THEN Fail (w, "typecells") END;
    IF MSIR.ModuleTypeLinkCount (m) > 0 THEN Fail (w, "type links") END;
    IF MSIR.ModuleRevelationCount (m) > 0 THEN Fail (w, "revelations") END;
    IF MSIR.ModuleExcDescCount (m) > 0 THEN Fail (w, "exceptions") END;
    IF MSIR.ModuleTextLitCount (m) > 0 THEN Fail (w, "text literals") END;
    IF MSIR.ModuleConstArrayCount (m) > 0 THEN Fail (w, "constant arrays") END;
    FOR i := 0 TO MSIR.ModuleGlobalCount (m) - 1 DO
      VAR g := MSIR.ModuleGlobal (m, i);
      BEGIN
        IF MSIR.GlobalIsExternal (g) OR MSIR.GlobalByteOffset (g) < 0 THEN
          Fail (w, "global " & MSIR.GlobalName (g));
        END;
      END;
    END;
    FOR i := 0 TO MSIR.ModuleGlobalInitCount (m) - 1 DO
      v := MSIR.ModuleGlobalInitValue (m, i);
      IF NOT MSIR.GetValueKind (v) IN SET OF ValueKind {
               ValueKind.ConstInt, ValueKind.ConstFloat, ValueKind.ConstNil,
               ValueKind.ConstProc } THEN
        Fail (w, "global initializer");
      END;
    END;
    FOR pi := 0 TO MSIR.ModuleProcCount (m) - 1 DO
      p := MSIR.ModuleProc (m, pi);
      IF MSIR.ProcBlockCount (p) > 0 THEN
        IF MSIR.ProcItemCount (p) # MSIR.ProcBlockCount (p) THEN
          Fail (w, "envelope");
        END;
        CheckProcSig (w, p);
        FOR bi := 0 TO MSIR.ProcBlockCount (p) - 1 DO
          b := MSIR.ProcBlock (p, bi);
          FOR k := 0 TO MSIR.BlockParamCount (b) - 1 DO
            CheckValue (w, MSIR.BlockParamValue (b, k));
          END;
          FOR ii := 0 TO MSIR.BlockInsnCount (b) - 1 DO
            CheckInsn (w, MSIR.BlockInsn (b, ii));
          END;
        END;
      END;
    END;
    IF w.ok AND w.wr # NIL THEN
      TRY
        Wr.PutText (w.wr, "msir-cg: " & w.mod & " lowered\n");
      EXCEPT Wr.Failure, Thread.Alerted =>
      END;
    END;
    RETURN w.ok;
  END Lowerable;

(*---------------------------------------------------------------- layout *)

VAR
  dataSize, constSize: INTEGER;
  impOff, fileOff, procInfoOff, gcMapOff: INTEGER;
  nameOff: REF ARRAY OF INTEGER;

(* Choose the module's symbols and segment offsets. *)
PROCEDURE Setup (m: MSIR.Module) =
  VAR nImports, nProcs: INTEGER;  gm: MSIR.GcMapBytes;  src: TEXT;
  BEGIN
    mod := m;
    modName := MSIR.ModuleName (m);
    isIface := MSIR.ModuleIsInterface (m);
    syms := NEW (TextRefTbl.Default).init ();
    nImports := MSIR.ModuleImportBinderCount (m);
    nProcs := MSIR.ModuleProcCount (m);

    dataSize := MAX (B (M3RT.MI_SIZE), MSIR.ModuleGlobalStructSize (m));
    impOff := RoundUp (dataSize, Target.Address.bytes);
    dataSize := impOff + nImports * B (M3RT.II_SIZE);
    EVAL syms.put (MSIR.ModuleInfoName (m), NEW (Loc, off := 0));
    FOR k := 0 TO nImports - 1 DO
      EVAL syms.put (modName & "_M3_imp." & Fmt.Int (k),
                     NEW (Loc, off := impOff + k * B (M3RT.II_SIZE)));
    END;

    constSize := 0;
    nameOff := NEW (REF ARRAY OF INTEGER, nProcs);
    FOR i := 0 TO nProcs - 1 DO
      nameOff[i] := constSize;
      INC (constSize, Text.Length (MSIR.ProcName (MSIR.ModuleProc (m, i))) + 1);
    END;
    procInfoOff := RoundUp (constSize, Target.Address.bytes);
    constSize := procInfoOff + (nProcs + 1) * B (M3RT.PI_SIZE);
    fileOff := constSize;
    src := SourceFile ();
    INC (constSize, Text.Length (src) + 1);
    gm := MSIR.ModuleGcMap (m);
    gcMapOff := -1;
    IF gm # NIL AND NUMBER (gm^) > 0 THEN
      gcMapOff := constSize;
      INC (constSize, NUMBER (gm^));
    END;
  END Setup;

PROCEDURE SourceFile (): TEXT =
  VAR src := MSIR.ModuleSrcFile (mod);
  BEGIN
    IF src = NIL OR Text.Equal (src, "") THEN src := modName END;
    RETURN src;
  END SourceFile;

(*------------------------------------------------------------ procedures *)

PROCEDURE ProcFor (p: MSIR.Proc): M3CG.Proc =
  VAR r: REFANY;  sym := CSymbol (p);
  BEGIN
    IF procs.get (sym, r) THEN RETURN NARROW (r, ProcRef).p END;
    RETURN ImportProc (sym, p);
  END ProcFor;

PROCEDURE ImportProc (sym: TEXT;  p: MSIR.Proc): M3CG.Proc =
  VAR n := MSIR.ProcParamCount (p);  q: M3CG.Proc;  t: MSIR.T;  a: INTEGER;
  BEGIN
    q := cg.import_procedure (ID (sym), n, SigType (MSIR.ProcResultType (p)),
                              Target.DefaultCall);
    FOR k := 0 TO n - 1 DO
      t := MSIR.ValueType (MSIR.ProcParam (p, k));
      EVAL cg.declare_param (ID (ParamName (p, k)), Size (t, a), a,
                             SigType (t), 0, FALSE, FALSE, M3CG.Maybe);
    END;
    EVAL procs.put (sym, NEW (ProcRef, p := q));
    RETURN q;
  END ImportProc;

(* A binder: PROCEDURE (mode: INTEGER): ADDRESS. *)
PROCEDURE ImportBinder (sym: TEXT): M3CG.Proc =
  VAR r: REFANY;  q: M3CG.Proc;
  BEGIN
    IF procs.get (sym, r) THEN RETURN NARROW (r, ProcRef).p END;
    q := cg.import_procedure (ID (sym), 1, Addr, Target.DefaultCall);
    EVAL cg.declare_param (ID ("mode"), Target.Integer.bytes,
                           Target.Integer.bytes, Int64, 0, FALSE, FALSE,
                           M3CG.Maybe);
    EVAL procs.put (sym, NEW (ProcRef, p := q));
    RETURN q;
  END ImportBinder;

PROCEDURE ParamName (p: MSIR.Proc;  k: INTEGER): TEXT =
  VAR n := MSIR.ProcParamName (p, k);
  BEGIN
    IF n = NIL OR Text.Equal (n, "") THEN RETURN "p" & Fmt.Int (k) END;
    IF HasPrefix (n, "a.") THEN n := Text.Sub (n, 2) END;  (* MSIR's "a.x" *)
    IF Text.FindChar (n, '.') >= 0 THEN n := "_" & DotsToUnderscore (n) END;
    RETURN n;
  END ParamName;

(* The body procedure of the unit, if the unit has one. *)
PROCEDURE BodyProc (): MSIR.Proc =
  VAR p: MSIR.Proc;  n: TEXT;
  BEGIN
    IF isIface THEN n := modName & "_I3" ELSE n := modName & "_M3" END;
    FOR i := 0 TO MSIR.ModuleProcCount (mod) - 1 DO
      p := MSIR.ModuleProc (mod, i);
      IF Text.Equal (MSIR.ProcName (p), n) AND MSIR.ProcBlockCount (p) > 0 THEN
        RETURN p;
      END;
    END;
    RETURN NIL;
  END BodyProc;

(*------------------------------------------------------------- the unit *)

PROCEDURE Lower (m: MSIR.Module;  c: M3CG.T) =
  VAR p: MSIR.Proc;  binderName: TEXT;  modeVar: M3CG.Var;
  BEGIN
    <* ASSERT Target.Integer.size = 64 AND Target.Address.size = 64 *>
    Setup (m);
    cg := c;  (* M3CG.T's methods are revealed by M3CG_Ops *)
    procs := NEW (TextRefTbl.Default).init ();
    curLine := -1;

    cg.begin_unit (0);
    cg.widechar_size (Target.WideCharSize ());
    cg.set_source_file (SourceFile ());
    constSeg := cg.declare_segment (M3ID.NoID, -1, TRUE);
    IF isIface
      THEN dataSeg := cg.declare_segment (ID ("I_" & modName), -1, FALSE);
      ELSE dataSeg := cg.declare_segment (ID ("M_" & modName), -1, FALSE);
    END;
    VAR it := syms.iterate ();  k: TEXT;  r: REFANY;
    BEGIN
      WHILE it.next (k, r) DO NARROW (r, Loc).seg := dataSeg END;
    END;

    (* The unit's binder. *)
    IF isIface THEN binderName := modName & "_I3"
    ELSE binderName := modName & "_M3"
    END;
    binder := cg.declare_procedure (ID (binderName), 1, Addr, 0,
                                    Target.DefaultCall, TRUE, NIL);
    modeVar := cg.declare_param (ID ("mode"), Target.Integer.bytes,
                                 Target.Integer.bytes, Int64, 0, FALSE, FALSE,
                                 M3CG.Always);
    EVAL procs.put (binderName, NEW (ProcRef, p := binder));

    (* The unit's own procedures. *)
    FOR i := 0 TO MSIR.ModuleProcCount (m) - 1 DO
      p := MSIR.ModuleProc (m, i);
      IF MSIR.ProcBlockCount (p) > 0 THEN DeclareProc (p) END;
    END;
    FOR k := 0 TO MSIR.ModuleImportBinderCount (m) - 1 DO
      EVAL ImportBinder (MSIR.ModuleImportBinder (m, k));
    END;

    GenBinder (modeVar);
    FOR i := 0 TO MSIR.ModuleProcCount (m) - 1 DO
      p := MSIR.ModuleProc (m, i);
      IF MSIR.ProcBlockCount (p) > 0 THEN GenProc (p) END;
    END;

    GenConstSeg ();
    GenDataSeg ();
    cg.end_unit ();
    cg := NIL;
    mod := NIL;
  END Lower;

PROCEDURE DeclareProc (p: MSIR.Proc) =
  VAR n := MSIR.ProcParamCount (p);  q: M3CG.Proc;  t: MSIR.T;  a: INTEGER;
      sym := CSymbol (p);
  BEGIN
    q := cg.declare_procedure (ID (sym), n, SigType (MSIR.ProcResultType (p)),
                               0, Target.DefaultCall, TRUE, NIL);
    EVAL procs.put (sym, NEW (ProcRef, p := q));
    (* The params' Vars are recorded when the body is generated: M3CG
       declares them here, so keep them in a per-proc table. *)
    VAR pv := NEW (REF ARRAY OF M3CG.Var, n);
    BEGIN
      FOR k := 0 TO n - 1 DO
        t := MSIR.ValueType (MSIR.ProcParam (p, k));
        pv[k] := cg.declare_param (ID (ParamName (p, k)), Size (t, a), a,
                                   SigType (t), 0, FALSE, FALSE, M3CG.Maybe);
      END;
      EVAL paramVars.put (sym, pv);
    END;
  END DeclareProc;

VAR paramVars := NEW (TextRefTbl.Default).init ();

PROCEDURE GenBinder (modeVar: M3CG.Var) =
  VAR done: M3CG.Label;  body := BodyProc ();  q: M3CG.Proc;
  BEGIN
    cg.begin_procedure (binder);
    IF body # NIL THEN
      done := cg.next_label ();
      cg.load (modeVar, 0, Int64, Int64);
      cg.if_false (Int64, done, M3CG.Maybe);
      q := ProcFor (body);
      cg.start_call_direct (q, 0, M3CG.Type.Void);
      cg.call_direct (q, M3CG.Type.Void);
      cg.set_label (done);
    END;
    cg.load_address (dataSeg, 0);
    cg.exit_proc (Addr);
    cg.end_procedure (binder);
  END GenBinder;

(*---------------------------------------------------------- the segments *)

PROCEDURE InitInt (off, v: INTEGER;  t: M3CG.Type) =
  VAR i: Target.Int;
  BEGIN
    EVAL TInt.FromInt (v, i);
    cg.init_int (off, i, t);
  END InitInt;

PROCEDURE GenConstSeg () =
  VAR nProcs := MSIR.ModuleProcCount (mod);  p: MSIR.Proc;
      gm := MSIR.ModuleGcMap (mod);  off: INTEGER;
  BEGIN
    cg.bind_segment (constSeg, RoundUp (MAX (constSize, 1), 16), 16,
                     M3CG.Type.Struct, FALSE, TRUE);
    cg.begin_init (constSeg);
    FOR i := 0 TO nProcs - 1 DO
      cg.init_chars (nameOff[i], MSIR.ProcName (MSIR.ModuleProc (mod, i)));
    END;
    off := procInfoOff;
    FOR i := 0 TO nProcs - 1 DO
      p := MSIR.ModuleProc (mod, i);
      cg.init_proc (off + B (M3RT.PI_proc), InfoProc (p));
      cg.init_var (off + B (M3RT.PI_name), constSeg, nameOff[i]);
      INC (off, B (M3RT.PI_SIZE));
    END;
    cg.init_chars (fileOff, SourceFile ());
    IF gcMapOff >= 0 THEN
      FOR k := 0 TO LAST (gm^) DO
        InitInt (gcMapOff + k, gm[k], M3CG.Type.Word8);
      END;
    END;
    cg.end_init (constSeg);
  END GenConstSeg;

(* The procedure a ProcInfo entry names: the binder for the unit's body. *)
PROCEDURE InfoProc (p: MSIR.Proc): M3CG.Proc =
  BEGIN
    IF p = BodyProc () THEN RETURN binder END;
    RETURN ProcFor (p);
  END InfoProc;

TYPE GInit = RECORD off: INTEGER;  v: MSIR.Value END;

PROCEDURE GenDataSeg () =
  VAR nImports := MSIR.ModuleImportBinderCount (mod);
      nInits := MSIR.ModuleGlobalInitCount (mod);
      inits := NEW (REF ARRAY OF GInit, nInits);
      tmp: GInit;  off: INTEGER;  f: Target.Float;
  BEGIN
    cg.bind_segment (dataSeg, RoundUp (dataSize, 16), 16, M3CG.Type.Struct,
                     FALSE, TRUE);
    cg.begin_init (dataSeg);
    cg.init_var (B (M3RT.MI_file), constSeg, fileOff);
    IF MSIR.ModuleProcCount (mod) > 0 THEN
      cg.init_var (B (M3RT.MI_proc_info), constSeg, procInfoOff);
    END;
    IF gcMapOff >= 0 THEN
      cg.init_var (B (M3RT.MI_var_map), constSeg, gcMapOff);
      cg.init_var (B (M3RT.MI_gc_map), constSeg, gcMapOff);
    END;
    IF nImports > 0 THEN cg.init_var (B (M3RT.MI_imports), dataSeg, impOff) END;
    cg.init_proc (B (M3RT.MI_binder), binder);
    InitInt (B (M3RT.MI_gc_flags), 3, Int64);

    (* Initial values of user globals, in offset order. *)
    FOR i := 0 TO nInits - 1 DO
      inits[i].off := MSIR.ModuleGlobalInitOffset (mod, i);
      inits[i].v := MSIR.ModuleGlobalInitValue (mod, i);
    END;
    FOR i := 1 TO nInits - 1 DO
      tmp := inits[i];
      VAR j := i;
      BEGIN
        WHILE j > 0 AND inits[j-1].off > tmp.off DO
          inits[j] := inits[j-1];  DEC (j);
        END;
        inits[j] := tmp;
      END;
    END;
    FOR i := 0 TO nInits - 1 DO
      VAR v := inits[i].v;
      BEGIN
        CASE MSIR.GetValueKind (v) OF
        | ValueKind.ConstInt =>
            InitInt (inits[i].off, MSIR.GetIntVal (v), MType (MSIR.ValueType (v)));
        | ValueKind.ConstFloat =>
            MSIR.GetFloatVal (v, f);
            cg.init_float (inits[i].off, f);
        | ValueKind.ConstProc =>
            cg.init_proc (inits[i].off, ProcFor (MSIR.GetConstProc (v)));
        ELSE (* NIL: zero *)
        END;
      END;
    END;

    off := impOff;
    FOR k := 0 TO nImports - 1 DO
      cg.init_proc (off + B (M3RT.II_binder),
                    ImportBinder (MSIR.ModuleImportBinder (mod, k)));
      IF k < nImports - 1 THEN
        cg.init_var (off + B (M3RT.II_next), dataSeg, off + B (M3RT.II_SIZE));
      END;
      INC (off, B (M3RT.II_SIZE));
    END;
    cg.end_init (dataSeg);
  END GenDataSeg;

(*--------------------------------------------------------------- values *)

PROCEDURE TempFor (t: MSIR.T): M3CG.Var =
  VAR z := ZType (t);  sz: INTEGER;
  BEGIN
    CASE z OF
    | M3CG.Type.Reel => sz := 4;
    | M3CG.Type.XReel => sz := 16;
    ELSE sz := 8;
    END;
    RETURN cg.declare_local (M3ID.NoID, sz, sz, z, 0, FALSE, FALSE,
                             M3CG.Maybe);
  END TempFor;

(* The ValLoc of an SSA value defined in the current procedure, creating
   its temporary on first use. *)
PROCEDURE Loc0 (v: MSIR.Value): ValLoc =
  VAR r: REFANY;  l: ValLoc;  name := MSIR.ValueName (v);
  BEGIN
    IF vals.get (name, r) THEN RETURN r END;
    l := NEW (ValLoc, kind := ValKind.Temp, t := MSIR.ValueType (v));
    l.var := TempFor (l.t);
    EVAL vals.put (name, l);
    RETURN l;
  END Loc0;

PROCEDURE PushInt (v: INTEGER;  t: M3CG.IType := Int64) =
  VAR i: Target.Int;
  BEGIN
    EVAL TInt.FromInt (v, i);
    cg.load_integer (t, i);
  END PushInt;

(* Sign-extend the integer on the stack from its width in t. *)
PROCEDURE Canon (t: MSIR.T) =
  VAR bits := IntBits (t);
  BEGIN
    IF bits = 1 THEN
      PushInt (1);  cg.and (Int64);
    ELSIF bits < 64 THEN
      cg.extract_mn (Int64, TRUE, 0, bits);
    END;
  END Canon;

(* Zero-extend the integer on the stack from its width in t. *)
PROCEDURE Mask (t: MSIR.T) =
  VAR bits := IntBits (t);
  BEGIN
    IF bits < 64 THEN cg.extract_mn (Int64, FALSE, 0, bits) END;
  END Mask;

PROCEDURE CanonInt (v, bits: INTEGER): INTEGER =
  BEGIN
    IF bits = 1 THEN RETURN Word.And (v, 1) END;
    IF bits >= 64 THEN RETURN v END;
    v := Word.And (v, Word.Minus (Word.Shift (1, bits), 1));
    IF Word.And (v, Word.Shift (1, bits - 1)) # 0 THEN
      v := Word.Or (v, Word.Not (Word.Minus (Word.Shift (1, bits), 1)));
    END;
    RETURN v;
  END CanonInt;

PROCEDURE Push (v: MSIR.Value) =
  VAR t := MSIR.ValueType (v);  r: REFANY;  f: Target.Float;
      name := MSIR.ValueName (v);
  BEGIN
    CASE MSIR.GetValueKind (v) OF
    | ValueKind.ConstInt =>
        IF IsPtr (t) THEN
          IF MSIR.GetIntVal (v) = 0 THEN cg.load_nil ()
          ELSE PushInt (MSIR.GetIntVal (v));  cg.loophole (Int64, Addr)
          END;
        ELSE
          PushInt (CanonInt (MSIR.GetIntVal (v), IntBits (t)));
        END;
    | ValueKind.ConstFloat =>
        MSIR.GetFloatVal (v, f);
        cg.load_float (RType (t), f);
    | ValueKind.ConstNil => cg.load_nil ();
    | ValueKind.ConstProc => cg.load_procedure (ProcFor (MSIR.GetConstProc (v)));
    | ValueKind.StructFieldRef =>
        EVAL syms.get (Text.Sub (name, 1), r);
        cg.load_address (NARROW (r, Loc).seg,
                         NARROW (r, Loc).off + MSIR.GetStructFieldOffset (v));
    ELSE
      IF MSIR.ValueDefInsn (v) = NIL AND name # NIL AND HasPrefix (name, "@")
         AND syms.get (Text.Sub (name, 1), r) THEN
        cg.load_address (NARROW (r, Loc).seg, NARROW (r, Loc).off);
        RETURN;
      END;
      VAR l := Loc0 (v);
      BEGIN
        CASE l.kind OF
        | ValKind.AddrOf => cg.load_address (l.var, 0);
        | ValKind.Param => cg.load (l.var, 0, MType (l.t), ZType (l.t));
                           IF IsInt (l.t) THEN Canon (l.t) END;
        | ValKind.Temp => cg.load (l.var, 0, ZType (l.t), ZType (l.t));
        END;
      END;
    END;
  END Push;

(* Store the value on the stack as the result of insn i. *)
PROCEDURE Def (v: MSIR.Value) =
  VAR l := Loc0 (v);
  BEGIN
    cg.store (l.var, 0, ZType (l.t), ZType (l.t));
  END Def;

PROCEDURE LabelOf (b: MSIR.Block): M3CG.Label =
  VAR r: REFANY;
  BEGIN
    IF blocks.get (MSIR.BlockLabel (b), r) THEN RETURN NARROW (r, LabelRef).l END;
    VAR l := NEW (LabelRef, l := cg.next_label ());
    BEGIN
      EVAL blocks.put (MSIR.BlockLabel (b), l);
      RETURN l.l;
    END;
  END LabelOf;

(*------------------------------------------------------------ procedures *)

PROCEDURE GenProc (p: MSIR.Proc) =
  VAR sym := CSymbol (p);  r: REFANY;  pv: REF ARRAY OF M3CG.Var;
      q := ProcFor (p);  b: MSIR.Block;  i: MSIR.Insn;
  BEGIN
    vals := NEW (TextRefTbl.Default).init ();
    blocks := NEW (TextRefTbl.Default).init ();
    EVAL paramVars.get (sym, r);
    pv := r;
    cg.begin_procedure (q);
    FOR k := 0 TO MSIR.ProcParamCount (p) - 1 DO
      VAR v := MSIR.ProcParam (p, k);
      BEGIN
        EVAL vals.put (MSIR.ValueName (v),
                       NEW (ValLoc, kind := ValKind.Param, var := pv[k],
                            t := MSIR.ValueType (v)));
      END;
    END;
    (* Allocas become in-memory locals, wherever they occur. *)
    FOR bi := 0 TO MSIR.ProcBlockCount (p) - 1 DO
      b := MSIR.ProcBlock (p, bi);
      FOR ii := 0 TO MSIR.BlockInsnCount (b) - 1 DO
        i := MSIR.BlockInsn (b, ii);
        IF MSIR.InsnOp (i) = Op.Alloca THEN
          VAR a: INTEGER;
              sz := Size (MSIR.InsnTargetType (i), a)
                      * MAX (1, MSIR.InsnExtractIdx (i));
              res := MSIR.InsnResult (i);
          BEGIN
            EVAL vals.put (MSIR.ValueName (res), NEW (ValLoc,
                   kind := ValKind.AddrOf, t := MSIR.ValueType (res),
                   var := cg.declare_local (ID (LocalName (res)),
                            RoundUp (MAX (sz, 1), a), a, M3CG.Type.Struct,
                            0, TRUE, FALSE, M3CG.Maybe)));
          END;
        END;
      END;
    END;
    FOR bi := 0 TO MSIR.ProcBlockCount (p) - 1 DO
      b := MSIR.ProcBlock (p, bi);
      IF bi > 0 THEN cg.set_label (LabelOf (b)) END;
      FOR ii := 0 TO MSIR.BlockInsnCount (b) - 1 DO
        GenInsn (MSIR.BlockInsn (b, ii));
      END;
    END;
    cg.end_procedure (q);
    vals := NIL;
    blocks := NIL;
  END GenProc;

(* "%x.slot" -> "x". *)
PROCEDURE LocalName (v: MSIR.Value): TEXT =
  VAR n := MSIR.ValueName (v);  len: INTEGER;
  BEGIN
    IF n = NIL THEN RETURN "_t" END;
    IF HasPrefix (n, "%") THEN n := Text.Sub (n, 1) END;
    len := Text.Length (n);
    IF len > 5 AND Text.Equal (Text.Sub (n, len - 5), ".slot") THEN
      n := Text.Sub (n, 0, len - 5);
    END;
    IF Text.FindChar (n, '.') >= 0 OR Text.Equal (n, "") THEN
      RETURN "_" & DotsToUnderscore (n);
    END;
    RETURN n;
  END LocalName;

(* Copy branch arguments to the target block's parameters. *)
PROCEDURE PassArgs (i: MSIR.Insn;  k: INTEGER;  target: MSIR.Block) =
  VAR n := MSIR.InsnBrArgCount (i, k);
  BEGIN
    FOR j := 0 TO n - 1 DO Push (MSIR.InsnBrArg (i, k, j)) END;
    FOR j := n - 1 TO 0 BY -1 DO Def (MSIR.BlockParamValue (target, j)) END;
  END PassArgs;

PROCEDURE Binop (i: MSIR.Insn) =
  BEGIN
    Push (MSIR.InsnOperand (i, 0));
    Push (MSIR.InsnOperand (i, 1));
  END Binop;

PROCEDURE CmpOp (p: MSIR.CmpPred): M3CG.CompareOp =
  BEGIN
    CASE p OF
    | MSIR.CmpPred.Eq => RETURN M3CG.CompareOp.EQ;
    | MSIR.CmpPred.Ne => RETURN M3CG.CompareOp.NE;
    | MSIR.CmpPred.Slt, MSIR.CmpPred.Ult => RETURN M3CG.CompareOp.LT;
    | MSIR.CmpPred.Sle, MSIR.CmpPred.Ule => RETURN M3CG.CompareOp.LE;
    | MSIR.CmpPred.Sgt, MSIR.CmpPred.Ugt => RETURN M3CG.CompareOp.GT;
    | MSIR.CmpPred.Sge, MSIR.CmpPred.Uge => RETURN M3CG.CompareOp.GE;
    END;
  END CmpOp;

PROCEDURE FCmpOp (p: MSIR.FCmpPred): M3CG.CompareOp =
  BEGIN
    CASE p OF
    | MSIR.FCmpPred.OEq => RETURN M3CG.CompareOp.EQ;
    | MSIR.FCmpPred.OLt => RETURN M3CG.CompareOp.LT;
    | MSIR.FCmpPred.OLe => RETURN M3CG.CompareOp.LE;
    | MSIR.FCmpPred.OGt => RETURN M3CG.CompareOp.GT;
    | MSIR.FCmpPred.OGe => RETURN M3CG.CompareOp.GE;
    ELSE RETURN M3CG.CompareOp.NE;
    END;
  END FCmpOp;

PROCEDURE HookCall (hook: MSIR.Proc;  fallback: TEXT;  arg: MSIR.Value) =
  VAR q: M3CG.Proc;  r: REFANY;
  BEGIN
    IF hook # NIL THEN
      q := ProcFor (hook);
    ELSIF procs.get (fallback, r) THEN
      q := NARROW (r, ProcRef).p;
    ELSE
      q := cg.import_procedure (ID (fallback), 1, M3CG.Type.Void,
                                Target.DefaultCall);
      EVAL cg.declare_param (ID ("r"), Target.Address.bytes,
                             Target.Address.bytes, Addr, 0, FALSE, FALSE,
                             M3CG.Maybe);
      EVAL procs.put (fallback, NEW (ProcRef, p := q));
    END;
    cg.start_call_direct (q, 0, M3CG.Type.Void);
    Push (arg);
    cg.pop_param (Addr);
    cg.call_direct (q, M3CG.Type.Void);
  END HookCall;

(* Push the RefHeader word of the heap object v. *)
PROCEDURE PushHeader (v: MSIR.Value) =
  BEGIN
    Push (v);
    cg.load_indirect (-Target.Address.bytes, Int64, Int64);
  END PushHeader;

(* The CM3 write barrier: call CheckStoreTraced unless already dirty. *)
PROCEDURE WriteBarrier (container: MSIR.Value) =
  VAR skip := cg.next_label ();
  BEGIN
    PushHeader (container);
    PushInt (Word.Shift (1, M3RT.RH_dirty_offset));
    cg.and (Int64);
    cg.if_true (Int64, skip, M3CG.Likely);
    HookCall (MSIR.ModuleGCStoreBarrier (mod), "RTHooks__CheckStoreTraced",
              container);
    cg.set_label (skip);
  END WriteBarrier;

(* The CM3 read barrier on a loaded reference v. *)
PROCEDURE ReadBarrier (v: MSIR.Value) =
  VAR skip := cg.next_label ();
  BEGIN
    Push (v);
    cg.load_nil ();
    cg.if_compare (Addr, M3CG.CompareOp.EQ, skip, M3CG.Maybe);
    Push (v);
    cg.loophole (Addr, Int64);
    PushInt (1);
    cg.and (Int64);
    cg.if_true (Int64, skip, M3CG.Never);
    PushHeader (v);
    PushInt (Word.Shift (1, M3RT.RH_gray_offset));
    cg.and (Int64);
    cg.if_false (Int64, skip, M3CG.Likely);
    HookCall (MSIR.ModuleGCLoadBarrier (mod), "RTHooks__CheckLoadTracedRef", v);
    cg.set_label (skip);
  END ReadBarrier;

PROCEDURE LineOf (i: MSIR.Insn) =
  VAR line := MSIR.InsnSrcLine (i);
  BEGIN
    IF line > 0 AND line # curLine THEN
      cg.set_source_line (line);
      curLine := line;
    END;
  END LineOf;

PROCEDURE GenInsn (i: MSIR.Insn) =
  VAR op := MSIR.InsnOp (i);  res := MSIR.InsnResult (i);
      nOps := MSIR.InsnOperandCount (i);  rt: MSIR.T;  a: INTEGER;
  BEGIN
    LineOf (i);
    IF res # NIL THEN rt := MSIR.ValueType (res) END;
    CASE op OF
    | Op.Alloca => (* declared in GenProc *)

    | Op.Load =>
        Push (MSIR.InsnOperand (i, 0));
        cg.load_indirect (0, MType (rt), ZType (rt));
        Def (res);
    | Op.GcLoad =>
        Push (MSIR.InsnOperand (i, 0));
        cg.load_indirect (0, Addr, Addr);
        Def (res);
        ReadBarrier (res);
    | Op.Store, Op.GcStore =>
        VAR val := MSIR.InsnOperand (i, 0);  vt := MSIR.ValueType (val);
        BEGIN
          IF op = Op.GcStore AND nOps = 3 THEN
            WriteBarrier (MSIR.InsnOperand (i, 2));
          END;
          Push (MSIR.InsnOperand (i, 1));
          Push (val);
          cg.store_indirect (0, ZType (vt), MType (vt));
        END;
    | Op.GcDirty =>
        WriteBarrier (MSIR.InsnOperand (i, 0));

    | Op.FieldAddr =>
        VAR t := MSIR.ValueType (MSIR.InsnOperand (i, 0));
            fname := MSIR.InsnSelector (i);  f: MSIR.Field;  off := 0;
        BEGIN
          IF IsPtr (t) THEN t := MSIR.EltType (Base (t)) END;
          FOR k := 0 TO MSIR.StructFieldCount (t) - 1 DO
            f := MSIR.StructField (t, k);
            IF Text.Equal (f.name, fname) THEN off := f.offset DIV 8;  EXIT END;
          END;
          Push (MSIR.InsnOperand (i, 0));
          IF off # 0 THEN cg.add_offset (off) END;
          Def (res);
        END;
    | Op.ArrayElemAddr =>
        VAR t := MSIR.ValueType (MSIR.InsnOperand (i, 0));
        BEGIN
          IF IsPtr (t) THEN t := MSIR.EltType (Base (t)) END;
          Push (MSIR.InsnOperand (i, 0));
          Push (MSIR.InsnOperand (i, 1));
          cg.index_address (Int64, Size (MSIR.FixedArrayElt (t), a));
          Def (res);
        END;
    | Op.PtrAdd =>
        Push (MSIR.InsnOperand (i, 0));
        IF MSIR.InsnExtractIdx (i) # 0 THEN
          cg.add_offset (MSIR.InsnExtractIdx (i));
        END;
        Def (res);
    | Op.GepByte =>
        Push (MSIR.InsnOperand (i, 0));
        Push (MSIR.InsnOperand (i, 1));
        cg.index_address (Int64, 1);
        Def (res);

    | Op.IAdd => Binop (i);  cg.add (Int64);  Canon (rt);  Def (res);
    | Op.ISub => Binop (i);  cg.subtract (Int64);  Canon (rt);  Def (res);
    | Op.IMul => Binop (i);  cg.multiply (Int64);  Canon (rt);  Def (res);
    | Op.IDiv =>
        Binop (i);
        cg.div (Int64, M3CG.Sign.Unknown, M3CG.Sign.Unknown);
        Canon (rt);  Def (res);
    | Op.IMod =>
        Binop (i);
        cg.mod (Int64, M3CG.Sign.Unknown, M3CG.Sign.Unknown);
        Canon (rt);  Def (res);
    | Op.IUDiv, Op.IURem =>
        Push (MSIR.InsnOperand (i, 0));  Mask (rt);
        Push (MSIR.InsnOperand (i, 1));  Mask (rt);
        IF op = Op.IUDiv
          THEN cg.div (Word64, M3CG.Sign.Positive, M3CG.Sign.Positive);
          ELSE cg.mod (Word64, M3CG.Sign.Positive, M3CG.Sign.Positive);
        END;
        Canon (rt);  Def (res);
    | Op.IAnd => Binop (i);  cg.and (Int64);  Def (res);
    | Op.IOr => Binop (i);  cg.or (Int64);  Def (res);
    | Op.IXor => Binop (i);  cg.xor (Int64);  Def (res);
    | Op.IShl =>
        Binop (i);  cg.shift_left (Word64);  Canon (rt);  Def (res);
    | Op.ILShr, Op.IAShr =>
        (* extract (x, n, 64 - n), sign-extended for ashr *)
        Push (MSIR.InsnOperand (i, 0));
        IF op = Op.ILShr THEN Mask (rt) END;
        Push (MSIR.InsnOperand (i, 1));
        PushInt (64);
        Push (MSIR.InsnOperand (i, 1));
        cg.subtract (Int64);
        cg.extract (Int64, op = Op.IAShr);
        Canon (rt);  Def (res);
    | Op.IRotL => Binop (i);  cg.rotate_left (Word64);  Def (res);
    | Op.IRotR => Binop (i);  cg.rotate_right (Word64);  Def (res);
    | Op.Select =>
        VAR else := cg.next_label ();  done := cg.next_label ();
        BEGIN
          Push (MSIR.InsnOperand (i, 0));
          cg.if_false (Int64, else, M3CG.Maybe);
          Push (MSIR.InsnOperand (i, 1));  Def (res);
          cg.jump (done);
          cg.set_label (else);
          Push (MSIR.InsnOperand (i, 2));  Def (res);
          cg.set_label (done);
        END;

    | Op.FAdd => Binop (i);  cg.add (RType (rt));  Def (res);
    | Op.FSub => Binop (i);  cg.subtract (RType (rt));  Def (res);
    | Op.FMul => Binop (i);  cg.multiply (RType (rt));  Def (res);
    | Op.FDiv => Binop (i);  cg.divide (RType (rt));  Def (res);
    | Op.FNeg => Push (MSIR.InsnOperand (i, 0));  cg.negate (RType (rt));  Def (res);
    | Op.FPAbs => Push (MSIR.InsnOperand (i, 0));  cg.abs (RType (rt));  Def (res);

    | Op.ICmp =>
        VAR pred := MSIR.InsnCmpPred (i);
            t := MSIR.ValueType (MSIR.InsnOperand (i, 0));
            unsigned := pred IN SET OF MSIR.CmpPred {
                          MSIR.CmpPred.Ult, MSIR.CmpPred.Ule,
                          MSIR.CmpPred.Ugt, MSIR.CmpPred.Uge };
            z: M3CG.ZType;
        BEGIN
          IF IsPtr (t) THEN
            z := Addr;
            Binop (i);
          ELSIF unsigned THEN
            z := Word64;
            Push (MSIR.InsnOperand (i, 0));  Mask (t);
            Push (MSIR.InsnOperand (i, 1));  Mask (t);
          ELSE
            z := Int64;
            Binop (i);
          END;
          cg.compare (z, Int64, CmpOp (pred));
          Def (res);
        END;
    | Op.FCmp =>
        Binop (i);
        cg.compare (RType (MSIR.ValueType (MSIR.InsnOperand (i, 0))), Int64,
                    FCmpOp (MSIR.InsnFCmpPred (i)));
        Def (res);

    | Op.Br =>
        VAR target := MSIR.InsnBrTarget (i, 0);
        BEGIN
          PassArgs (i, 0, target);
          cg.jump (LabelOf (target));
        END;
    | Op.CondBr =>
        VAR t := MSIR.InsnBrTarget (i, 0);  f := MSIR.InsnBrTarget (i, 1);
            edge := cg.next_label ();
        BEGIN
          Push (MSIR.InsnOperand (i, 0));
          cg.if_false (Int64, edge, M3CG.Maybe);
          PassArgs (i, 0, t);
          cg.jump (LabelOf (t));
          cg.set_label (edge);
          PassArgs (i, 1, f);
          cg.jump (LabelOf (f));
        END;
    | Op.Ret =>
        IF nOps = 0 THEN
          cg.exit_proc (M3CG.Type.Void);
        ELSE
          VAR v := MSIR.InsnOperand (i, 0);
          BEGIN
            Push (v);
            cg.exit_proc (SigType (MSIR.ValueType (v)));
          END;
        END;
    | Op.Unreachable =>
        cg.abort (M3CG.RuntimeError.UnhandledException);

    | Op.Call =>
        VAR callee := MSIR.InsnCallee (i);  q := ProcFor (callee);
            rtype := SigType (MSIR.ProcResultType (callee));
        BEGIN
          cg.start_call_direct (q, 0, rtype);
          FOR k := 0 TO nOps - 1 DO
            Push (MSIR.InsnOperand (i, k));
            cg.pop_param (SigType (MSIR.ValueType (MSIR.ProcParam (callee, k))));
          END;
          cg.call_direct (q, rtype);
          IF res # NIL THEN
            IF IsInt (rt) THEN Canon (rt) END;
            Def (res);
          END;
        END;
    | Op.CallIndirect =>
        VAR rtype := SigType (MSIR.InsnTargetType (i));
        BEGIN
          cg.start_call_indirect (rtype, Target.DefaultCall);
          FOR k := 1 TO nOps - 1 DO
            VAR v := MSIR.InsnOperand (i, k);
            BEGIN
              Push (v);
              cg.pop_param (SigType (MSIR.ValueType (v)));
            END;
          END;
          Push (MSIR.InsnOperand (i, 0));
          cg.call_indirect (rtype, Target.DefaultCall);
          IF res # NIL THEN
            IF IsInt (rt) THEN Canon (rt) END;
            Def (res);
          ELSIF rtype # M3CG.Type.Void THEN
            cg.pop (rtype);
          END;
        END;

    | Op.Convert =>
        VAR src := MSIR.InsnOperand (i, 0);  st := MSIR.ValueType (src);
            dt := MSIR.InsnTargetType (i);
        BEGIN
          IF dt = NIL THEN dt := rt END;
          Push (src);
          IF IsPtr (st) AND IsPtr (dt) THEN
            (* nothing *)
          ELSIF IsPtr (st) THEN
            cg.loophole (Addr, Int64);  Canon (dt);
          ELSIF IsPtr (dt) THEN
            cg.loophole (Int64, Addr);
          ELSIF IsFloat (st) AND IsFloat (dt) THEN
            IF RType (st) # RType (dt) THEN cg.cvt_float (RType (st), RType (dt)) END;
          ELSIF IsFloat (st) THEN
            cg.loophole (RType (st), Int64);
          ELSIF IsFloat (dt) THEN
            cg.loophole (Int64, RType (dt));
          ELSIF IntBits (dt) < IntBits (st) THEN
            Canon (dt);
          END;
          Def (res);
        END;
    | Op.SIToFP =>
        Push (MSIR.InsnOperand (i, 0));
        cg.cvt_float (Int64, RType (rt));
        Def (res);
    | Op.FPToSI =>
        VAR src := MSIR.InsnOperand (i, 0);
        BEGIN
          Push (src);
          cg.cvt_int (RType (MSIR.ValueType (src)), Int64, M3CG.ConvertOp.Trunc);
          Canon (rt);
          Def (res);
        END;
    | Op.FPExt, Op.FPTrunc =>
        VAR src := MSIR.InsnOperand (i, 0);
        BEGIN
          Push (src);
          cg.cvt_float (RType (MSIR.ValueType (src)), RType (rt));
          Def (res);
        END;
    | Op.ZExt =>
        VAR src := MSIR.InsnOperand (i, 0);
        BEGIN
          Push (src);
          Mask (MSIR.ValueType (src));
          Def (res);
        END;
    | Op.SExt =>
        VAR src := MSIR.InsnOperand (i, 0);
        BEGIN
          Push (src);
          IF IntBits (MSIR.ValueType (src)) = 1 THEN cg.negate (Int64) END;
          Def (res);
        END;
    | Op.Trunc =>
        Push (MSIR.InsnOperand (i, 0));
        Canon (rt);
        Def (res);
    ELSE
      <* ASSERT FALSE *>
    END;
  END GenInsn;

BEGIN
END MSIRToM3CG.
