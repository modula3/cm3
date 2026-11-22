MODULE MSIRToM3CG;

IMPORT MSIR, MSIRPrinter, M3CG, M3CG_Ops, M3ID, RefSeq, M3RT, Target, TInt, TextRefTbl, Fmt,
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
    Op.FPFloor, Op.FPCeil, Op.FPRound, Op.Typecase,
    Op.SetUnion, Op.SetIntersect, Op.SetDifference, Op.SetSymDifference,
    Op.SetMember, Op.SetSingleton, Op.SetRange,
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
    Op.Resume, Op.Raise, Op.UnwindTo, Op.RetThroughEnvelope, Op.EndFinally,
    Op.HandlerActivation };

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
        IF MSIR.ProcScopeCount (p) > 1 THEN Note (f, "eh-scope") END;
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
  Loc = REF RECORD off: INTEGER;  s: Seg END;

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
  syms      : TextRefTbl.T;   (* symbol (no '@') -> Loc *)
  procs     : TextRefTbl.T;   (* C symbol -> ProcRef *)
  vals      : TextRefTbl.T;   (* SSA name -> ValLoc, per procedure *)
  blocks    : TextRefTbl.T;   (* block label -> LabelRef, per procedure *)
  binder    : M3CG.Proc;
  curLine   : INTEGER;

CONST
  Int64 = M3CG.Type.Int64;
  RoundOp = ARRAY [Op.FPFloor .. Op.FPRound] OF M3CG.ConvertOp {
    M3CG.ConvertOp.Floor, M3CG.ConvertOp.Ceiling, M3CG.ConvertOp.Round };
  Word64 = M3CG.Type.Word64;
  Addr = M3CG.Type.Addr;

(*--------------------------------------------------------------- reasons *)

TYPE Why = OBJECT wr: Wr.T;  ok := TRUE;  mod: TEXT;  seen: Features END;

PROCEDURE Fail (w: Why;  what: TEXT) =
  VAR n: INTEGER;
  BEGIN
    IF w.seen = NIL THEN w.seen := NEW (Features) END;
    n := w.seen.n;
    Note (w.seen, what);
    IF w.seen.n > n AND w.wr # NIL THEN
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
    RETURN t # NIL AND (MSIR.Kind (t) IN SET OF TypeKind {
             TypeKind.I1, TypeKind.I8, TypeKind.I16, TypeKind.I32,
             TypeKind.I64, TypeKind.W8, TypeKind.W16, TypeKind.W32,
             TypeKind.W64, TypeKind.Enum } OR IsOddInt (t));
  END IsInt;

(* An integer narrower than a word whose width is not 8, 16 or 32 (e.g. the
   i11 of a packed field); MSIR gives it kind IWide.  It is a word on the
   M3CG stack and takes LLVM's size in a struct, but has no memory type,
   so it is not loaded or stored directly. *)
PROCEDURE IsOddInt (t: MSIR.T): BOOLEAN =
  BEGIN
    t := Base (t);
    RETURN t # NIL AND MSIR.Kind (t) = TypeKind.IWide
       AND MSIR.BitWidth (t) < 64;
  END IsOddInt;

PROCEDURE IsPtr (t: MSIR.T): BOOLEAN =
  BEGIN
    t := Base (t);
    RETURN t # NIL AND MSIR.Kind (t) IN SET OF TypeKind {
             TypeKind.Ptr, TypeKind.GcRef, TypeKind.GcSlot, TypeKind.ProcType,
             TypeKind.Object, TypeKind.HeapArray };
  END IsPtr;

PROCEDURE IsFloat (t: MSIR.T): BOOLEAN =
  BEGIN
    t := Base (t);
    RETURN t # NIL AND MSIR.IsFloat (MSIR.Kind (t));
  END IsFloat;

(* Aggregates live in memory; their SSA values are addresses of temps. *)
PROCEDURE IsAgg (t: MSIR.T): BOOLEAN =
  BEGIN
    t := Base (t);
    RETURN t # NIL AND (MSIR.Kind (t) IN SET OF TypeKind {
             TypeKind.Struct, TypeKind.FixedArray, TypeKind.OpenArray }
             OR IsWideSet (t));
  END IsAgg;

(* A set wider than a word: m3front types it as an integer iN and works on
   it with the set intrinsics (D19).  Here it is an aggregate of N/64 words,
   word-aligned as CG lays sets out, operated on by M3CG's set_* ops. *)
PROCEDURE IsWideSet (t: MSIR.T): BOOLEAN =
  BEGIN
    t := Base (t);
    RETURN t # NIL AND MSIR.Kind (t) = TypeKind.IWide
       AND MSIR.BitWidth (t) > 64;
  END IsWideSet;

PROCEDURE SetBytes (t: MSIR.T): INTEGER =
  BEGIN
    RETURN RoundUp (MSIR.BitWidth (Base (t)), 64) DIV 8;
  END SetBytes;

PROCEDURE IsValue (t: MSIR.T): BOOLEAN =
  VAR a: INTEGER;
  BEGIN
    RETURN IsScalar (t) OR IsAgg (t) AND Size (t, a) >= 0;
  END IsValue;

PROCEDURE IsScalar (t: MSIR.T): BOOLEAN =
  BEGIN
    RETURN IsInt (t) OR IsPtr (t) OR IsFloat (t);
  END IsScalar;

(* Bit width of an integer type; i1 is 1. *)
PROCEDURE IntBits (t: MSIR.T): INTEGER =
  BEGIN
    t := Base (t);
    IF MSIR.Kind (t) = TypeKind.Enum OR IsOddInt (t) THEN
      RETURN MSIR.BitWidth (t);
    END;
    CASE MSIR.Kind (t) OF
    | TypeKind.I1 => RETURN 1;
    | TypeKind.I8, TypeKind.W8 => RETURN 8;
    | TypeKind.I16, TypeKind.W16 => RETURN 16;
    | TypeKind.I32, TypeKind.W32 => RETURN 32;
    ELSE RETURN 64;
    END;
  END IntBits;

(* The width of a float type, in bits. *)
PROCEDURE FloatBits (t: MSIR.T): INTEGER =
  BEGIN
    CASE MSIR.Kind (Base (t)) OF
    | TypeKind.F32 => RETURN 32;
    | TypeKind.F64 => RETURN 64;
    ELSE RETURN Target.Extended.size;
    END;
  END FloatBits;

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
    IF IsAgg (t) THEN RETURN Addr END;
    RETURN MType (t);
  END SigType;

(* The declared result type: CG returns aggregates through a hidden
   "_result" address parameter, and the procedure returns nothing. *)
PROCEDURE RetType (t: MSIR.T): M3CG.Type =
  BEGIN
    IF t # NIL AND IsAgg (t) THEN RETURN M3CG.Type.Void END;
    RETURN SigType (t);
  END RetType;

PROCEDURE HasAggResult (p: MSIR.Proc): BOOLEAN =
  VAR t := MSIR.ProcResultType (p);
  BEGIN
    RETURN t # NIL AND IsAgg (t);
  END HasAggResult;

(* Where the hidden result parameter goes, as CG's DeclareFormals. *)
PROCEDURE ResultFirst (): BOOLEAN =
  BEGIN
    RETURN Target.DefaultCall.results_on_left;
  END ResultFirst;

(* The size and alignment a parameter of type t is declared with. *)
PROCEDURE ParamSize (t: MSIR.T;  VAR a: INTEGER): INTEGER =
  BEGIN
    IF IsAgg (t) THEN a := Target.Address.bytes;  RETURN a END;
    RETURN Size (t, a);
  END ParamSize;

(* M3RT's layout constants are in bits. *)
PROCEDURE B (bits: INTEGER): INTEGER =
  BEGIN
    RETURN bits DIV Target.Byte;
  END B;

(* The byte offset of field i of struct t: the natural layout MSIRToLLVM's
   struct types get from LLVM.  (MSIR.Field.offset is debug information,
   set only for structs translated from M3 records.) *)
PROCEDURE FieldOffset (t: MSIR.T;  i: INTEGER): INTEGER =
  VAR off := 0;  a, sz: INTEGER;
  BEGIN
    t := Base (t);
    FOR k := 0 TO i DO
      sz := Size (MSIR.StructField (t, k).type, a);
      off := RoundUp (off, a);
      IF k < i THEN INC (off, sz) END;
    END;
    RETURN off;
  END FieldOffset;

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
    IF IsWideSet (t) THEN align := 8;  RETURN SetBytes (t) END;
    IF IsOddInt (t) THEN
      (* LLVM's allocation size: the next power-of-two number of bytes *)
      align := 1;
      WHILE align * 8 < IntBits (t) DO align := align * 2 END;
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
        (* natural layout, as LLVM gives MSIRToLLVM's struct types *)
        end := 0;
        FOR i := 0 TO MSIR.StructFieldCount (t) - 1 DO
          f := MSIR.StructField (t, i);
          sz := Size (f.type, a);
          IF sz < 0 THEN RETURN -1 END;
          align := MAX (align, a);
          end := RoundUp (end, a) + sz;
        END;
        RETURN RoundUp (end, align);
    | TypeKind.OpenArray =>
        align := Target.Address.bytes;
        RETURN Target.Address.bytes
                 + MSIR.OpenArrayRank (t) * Target.Integer.bytes;
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

(* "tc_obj_-123" -> "tc_obj_m123": an LLVM symbol made a C identifier. *)
PROCEDURE MinusToM (n: TEXT): TEXT =
  VAR i := Text.FindChar (n, '-');
  BEGIN
    WHILE i >= 0 DO
      n := Text.Sub (n, 0, i) & "m" & Text.Sub (n, i + 1);
      i := Text.FindChar (n, '-', i + 1);
    END;
    RETURN n;
  END MinusToM;

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
    Op.FPFloor, Op.FPCeil, Op.FPRound, Op.Typecase,
    Op.SetUnion, Op.SetIntersect, Op.SetDifference, Op.SetSymDifference,
    Op.SetMember, Op.SetSingleton, Op.SetRange,
    Op.ICmp, Op.FCmp,
    Op.Br, Op.CondBr, Op.Ret, Op.Unreachable,
    Op.UnwindTo, Op.RetThroughEnvelope, Op.EndFinally, Op.HandlerActivation,
    Op.Call, Op.CallIndirect,
    Op.Convert, Op.SIToFP, Op.FPToSI, Op.FPExt, Op.FPTrunc,
    Op.ZExt, Op.SExt, Op.Trunc,
    Op.PtrAdd, Op.GepByte, Op.ArrayElemAddr,
    Op.OpenArraySize, Op.OpenArrayElemAddr, Op.AllocaDyn };

PROCEDURE CheckSigType (w: Why;  what: TEXT;  t: MSIR.T) =
  BEGIN
    IF t = NIL OR MSIR.Kind (t) = TypeKind.Void THEN RETURN END;
    IF NOT IsValue (t) THEN
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
    | ValueKind.GlobalRef =>
        VAR r: REFANY;  key := name;
        BEGIN
          IF key # NIL AND HasPrefix (key, "@") THEN key := Text.Sub (key, 1) END;
          IF FindGlobal (key) = NIL OR NOT syms.get (key, r) THEN
            Fail (w, "global " & name);
          END;
        END;
    | ValueKind.ConstTextLit =>
    | ValueKind.ConstStruct, ValueKind.ConstAggArray =>
        IF NOT IsInitConst (v) THEN Fail (w, "constant aggregate") END;
    END;
    IF NOT IsValue (MSIR.ValueType (v)) THEN
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
    IF res # NIL AND op # Op.Alloca THEN
      t := MSIR.ValueType (res);
      IF NOT IsScalar (t) AND NOT (IsAgg (t) AND IsValue (t) AND op IN SET OF Op {
               Op.Load, Op.Call, Op.CallIndirect, Op.Select,
               Op.SetUnion, Op.SetIntersect, Op.SetDifference,
               Op.SetSymDifference, Op.SetSingleton, Op.SetRange }) THEN
        Fail (w, MSIRPrinter.OpText (op) & " result "
                   & KindText[MSIR.Kind (Base (t))]);
      END;
    END;
    FOR k := 0 TO MSIR.InsnOperandCount (i) - 1 DO
      VAR v := MSIR.InsnOperand (i, k);
      BEGIN
        CheckValue (w, v);
        (* constant aggregates only as a value to store *)
        IF v # NIL AND IsAgg (MSIR.ValueType (v))
           AND MSIR.GetValueKind (v) IN SET OF ValueKind {
                 ValueKind.ConstNil, ValueKind.ConstStruct,
                 ValueKind.ConstAggArray }
           AND NOT (op = Op.Store AND k = 0) THEN
          Fail (w, "constant aggregate operand of " & MSIRPrinter.OpText (op));
        END;
      END;
    END;
    CASE op OF
    | Op.Br, Op.CondBr, Op.UnwindTo =>
        FOR k := 0 TO ORD (op = Op.CondBr) DO
          FOR j := 0 TO MSIR.InsnBrArgCount (i, k) - 1 DO
            CheckValue (w, MSIR.InsnBrArg (i, k, j));
          END;
        END;
    | Op.ICmp =>
        IF IsWideSet (MSIR.ValueType (MSIR.InsnOperand (i, 0)))
           AND NOT MSIR.InsnCmpPred (i) IN SET OF MSIR.CmpPred {
                     MSIR.CmpPred.Eq, MSIR.CmpPred.Ne } THEN
          Fail (w, "ordered icmp on a multi-word set");
        END;
    | Op.Typecase =>
        IF MSIR.ModuleScanTypecase (mod) = NIL THEN
          Fail (w, "typecase without the ScanTypecase hook");
        END;
    | Op.Load =>
        IF IsOddInt (MSIR.ValueType (res)) THEN Fail (w, "load of odd width") END;
    | Op.Store =>
        IF IsOddInt (MSIR.ValueType (MSIR.InsnOperand (i, 0))) THEN
          Fail (w, "store of odd width");
        END;
    | Op.Alloca =>
        IF Size (MSIR.InsnTargetType (i), a) < 0 THEN
          Fail (w, "alloca of unsized type");
        END;
    | Op.Call =>
        IF MemOp (MSIR.InsnCallee (i)) = Mem.Zero THEN
          VAR c := MSIR.InsnOperand (i, 1);
          BEGIN
            IF MSIR.GetValueKind (c) # ValueKind.ConstInt
               OR MSIR.GetIntVal (c) # 0 THEN
              Fail (w, "memset with a non-zero fill");
            END;
            c := MSIR.InsnOperand (i, 2);
            IF MSIR.GetValueKind (c) # ValueKind.ConstInt THEN
              Fail (w, "memset of a non-constant size");
            END;
          END;
        ELSIF MemOp (MSIR.InsnCallee (i)) = Mem.None THEN
          CheckProcSig (w, MSIR.InsnCallee (i));
        END;
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
          (* A float <-> integer convert is a bitcast, the integer resized
             (truncated or zero-extended) to the float's width, as in
             MSIRToLLVM.  M3CG's loophole reinterprets only between a float
             and an integer of its size (Reel/Int32, LReel/Int64), so the
             float must be 32 or 64 bits. *)
          IF IsFloat (s) # IsFloat (d) AND (IsPtr (s) OR IsPtr (d)
             OR (IsFloat (s) AND NOT FloatBits (s) IN SET OF [0..64] {32, 64})
             OR (IsFloat (d) AND NOT FloatBits (d) IN SET OF [0..64] {32, 64})) THEN
            Fail (w, "convert between float and integer: float of 128 bits");
          END;
        END;
    ELSE
    END;
  END CheckInsn;

(* Scopes are lowered to CG's ex_stack scheme, or to its ex_frame scheme on
   a target without a stack walker. *)
PROCEDURE CheckScopes (w: Why;  m: MSIR.Module;  p: MSIR.Proc) =
  VAR s: MSIR.Scope;  h: MSIR.Handler;  b: MSIR.Block;  i: MSIR.Insn;
  BEGIN
    IF NOT Target.Has_stack_walker
       AND (MSIR.ModulePushEFrame (m) = NIL OR MSIR.ModulePopEFrame (m) = NIL) THEN
      Fail (w, "EH scopes (ex_frame): no frame hooks");
    END;
    IF MSIR.ModuleResumeRaise (m) = NIL THEN
      Fail (w, "EH scopes: no re-raise hook");
    END;
    FOR k := 0 TO MSIR.ProcScopeCount (p) - 1 DO
      s := MSIR.ProcScopeAt (p, k);
      IF MSIR.ScopeKindOf (s) = MSIR.ScopeKind.Except THEN
        FOR j := 0 TO MSIR.ScopeHandlerCount (s) - 1 DO
          h := MSIR.ScopeHandler (s, j);
          IF MSIR.BlockParamCount (h.entry) = 1
             AND NOT MSIR.ExcDescArgByRef (h.exc)
             AND IsFloat (MSIR.ValueType (MSIR.BlockParamValue (h.entry, 0))) THEN
            Fail (w, "exception argument: float by value");
          END;
        END;
      END;
    END;
    FOR bi := 0 TO MSIR.ProcBlockCount (p) - 1 DO
      b := MSIR.ProcBlock (p, bi);
      FOR ii := 0 TO MSIR.BlockInsnCount (b) - 1 DO
        i := MSIR.BlockInsn (b, ii);
        IF MSIR.InsnOp (i) = Op.UnwindTo AND MSIR.InsnBrArgCount (i, 0) > 0
           AND FirstFinally (MSIR.BlockScope (b), MSIR.InsnBrTarget (i, 0),
                             FALSE) # NIL THEN
          Fail (w, "unwind_to with arguments through FINALLY");
        END;
      END;
    END;
  END CheckScopes;

PROCEDURE Lowerable (m: MSIR.Module;  why: Wr.T := NIL): BOOLEAN =
  VAR w := NEW (Why, wr := why, mod := MSIR.ModuleName (m));
      p: MSIR.Proc;  b: MSIR.Block;  v: MSIR.Value;
  BEGIN
    Setup (m);
    IF MSIR.ModuleIncomplete (m) # NIL THEN
      Fail (w, "incomplete: " & MSIR.ModuleIncomplete (m));
    END;
    FOR i := 0 TO MSIR.ModuleConstArrayCount (m) - 1 DO
      VAR ca := MSIR.ModuleConstArray (m, i);  a: INTEGER;
      BEGIN
        IF Size (MSIR.ConstArrayEltType (ca), a) < 0 THEN
          Fail (w, "constant array of unsized type");
        END;
        FOR j := 0 TO MSIR.ConstArrayEltCount (ca) - 1 DO
          IF NOT IsInitConst (MSIR.ConstArrayElt (ca, j)) THEN
            Fail (w, "constant array element");
          END;
        END;
      END;
    END;
    FOR i := 0 TO MSIR.ModuleTypeDescCount (m) - 1 DO
      VAR d := MSIR.ModuleTypeDesc (m, i);  r: REFANY;
      BEGIN
        IF MSIR.TypeDescKind (d) = TK_Obj
           AND MethodMode (d) = LinkMode.Inits THEN
          FOR j := 0 TO MSIR.TypeDescMethodInitCount (d) - 1 DO
            IF NOT MSIR.TypeDescMethodInitBaseKnown (d, j)
               AND NOT syms.get ("tl_obj_" & Fmt.Int (
                         MSIR.TypeDescMethodInitDeclUID (d, j)), r) THEN
              Fail (w, "method of unlinked type");
            END;
          END;
        END;
      END;
    END;
    FOR i := 0 TO MSIR.ModuleGlobalCount (m) - 1 DO
      VAR g := MSIR.ModuleGlobal (m, i);
      BEGIN
        IF NOT MSIR.GlobalIsExternal (g) THEN
          IF MSIR.GlobalByteOffset (g) < 0 AND NOT IsBacking (g)
             AND NOT IsStandalone (g) THEN
            Fail (w, "standalone global " & MSIR.GlobalName (g));
          END;
        ELSE
          (* External globals are <*EXTERNAL*> C variables (D28); an
             interface variable is reached through the import chain. *)
          VAR a: INTEGER;
          BEGIN
            IF Size (MSIR.GlobalType (g), a) < 0 THEN
              Fail (w, "external global of unsized type " & MSIR.GlobalName (g));
            END;
          END;
        END;
      END;
    END;
    FOR i := 0 TO MSIR.ModuleGlobalInitCount (m) - 1 DO
      v := MSIR.ModuleGlobalInitValue (m, i);
      IF NOT IsInitConst (v) THEN Fail (w, "global initializer") END;
    END;
    FOR pi := 0 TO MSIR.ModuleProcCount (m) - 1 DO
      p := MSIR.ModuleProc (m, pi);
      IF MSIR.ProcBlockCount (p) > 0 THEN
        IF MSIR.ProcScopeCount (p) > 1 THEN CheckScopes (w, m, p) END;
        IF MSIR.ProcChecksRaises (p) AND NOT Target.Has_stack_walker
           AND (MSIR.ModulePushEFrame (m) = NIL
                OR MSIR.ModulePopEFrame (m) = NIL) THEN
          Fail (w, "RAISES check (ex_frame): no frame hooks");
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

(* Each segment is a list of items at increasing offsets; an item emits its
   own init_* calls when the segment is generated. *)

TYPE
  Seg = REF RECORD
    var   : M3CG.Var := NIL;
    size  : INTEGER := 0;
    items : RefSeq.T;
  END;

  Item = OBJECT
    off : INTEGER;
  METHODS
    emit ();
  END;

TYPE
  (* A large global (size > Max_zero_global) lives in its own zeroed
     global; the module struct's slot points to it (a global initializer).
     The CG path does the same (Variable.Declare, t.bss_var). *)
  Backing = REF RECORD
    seg: Seg;  name: TEXT;  align: INTEGER;
    external := FALSE;   (* an <*EXTERNAL*> C variable: import_global *)
    type: M3CG.Type := M3CG.Type.Struct;
  END;

VAR
  cseg, dseg: Seg;   (* constant and data segments *)
  typecases: RefSeq.T;  (* TypecaseItem *)
  backing: RefSeq.T;    (* Backing: large globals' separate storage *)
  procInfoOff, gcMapOff, fileOff: INTEGER;
  firstTC, lastTL, fullRevOff: INTEGER;

PROCEDURE Alloc (s: Seg;  size, align: INTEGER;  it: Item): INTEGER =
  BEGIN
    it.off := RoundUp (s.size, MAX (align, 1));
    s.size := it.off + size;
    s.items.addhi (it);
    RETURN it.off;
  END Alloc;

PROCEDURE NewSym (name: TEXT;  s: Seg;  off: INTEGER) =
  BEGIN
    EVAL syms.put (name, NEW (Loc, off := off, s := s));
  END NewSym;

(*----- items *)

TYPE
  CharsItem = Item OBJECT chars: TEXT OVERRIDES emit := EmitChars END;
  BytesItem = Item OBJECT b: MSIR.GcMapBytes OVERRIDES emit := EmitBytes END;
  ProcInfoItem = Item OBJECT names: REF ARRAY OF INTEGER
                 OVERRIDES emit := EmitProcInfo END;
  ModuleInfoItem = Item OBJECT OVERRIDES emit := EmitModuleInfo END;
  GlobalsItem = Item OBJECT OVERRIDES emit := EmitGlobalInits END;
  SpaceItem = Item OBJECT OVERRIDES emit := EmitNothing END;
  ImportsItem = Item OBJECT OVERRIDES emit := EmitImports END;
  TypecellItem = Item OBJECT
    d: MSIR.TypeDesc;
    next, name, typeMap, gcMap, desc, brand, methods := -1;
    linkProc: M3CG.Proc := NIL;
  OVERRIDES emit := EmitTypecell END;
  BrandItem = Item OBJECT d: MSIR.TypeDesc OVERRIDES emit := EmitBrand END;
  MethodsItem = Item OBJECT d: MSIR.TypeDesc OVERRIDES emit := EmitMethods END;
  TypeLinkItem = Item OBJECT tl: MSIR.TypeLink;  prev := -1
                 OVERRIDES emit := EmitTypeLink END;
  RevItem = Item OBJECT OVERRIDES emit := EmitRevs END;
  ExcItem = Item OBJECT d: MSIR.ExcDesc;  name := -1
            OVERRIDES emit := EmitExc END;
  TextMethodsItem = Item OBJECT OVERRIDES emit := EmitTextMethods END;
  TextLitItem = Item OBJECT uid: INTEGER;  methods: INTEGER
                OVERRIDES emit := EmitTextLit END;
  ConstArrayItem = Item OBJECT ca: MSIR.ConstArray
                   OVERRIDES emit := EmitConstArray END;
  (* RTHooks.TypecaseCell array of a TYPECASE: defn (resolved lazily by
     ScanTypecase), uid; the ELSE clause's uid 0 ends it. *)
  TypecaseItem = Item OBJECT insn: MSIR.Insn
                 OVERRIDES emit := EmitTypecase END;
  (* ex_frame: the exceptions an Except scope handles (s), or that a
     procedure's RAISES clause allows (p), as the frame's NIL-terminated list
     of uids (ESet's layout). *)
  HandlesItem = Item OBJECT
                  s: MSIR.Scope := NIL;  p: MSIR.Proc := NIL;
                  uids: REF ARRAY OF INTEGER
                OVERRIDES emit := EmitHandles END;

CONST TEXT_typecode = 1;

(* Choose the module's symbols and segment offsets. *)
PROCEDURE Setup (m: MSIR.Module) =
  VAR nImports, nProcs, nTC, nTL, k: INTEGER;  src: TEXT;
      ptr := Target.Address.bytes;
      tcs: REF ARRAY OF TypecellItem;
  BEGIN
    mod := m;
    modName := MSIR.ModuleName (m);
    isIface := MSIR.ModuleIsInterface (m);
    syms := NEW (TextRefTbl.Default).init ();
    cseg := NEW (Seg, items := NEW (RefSeq.T).init ());
    dseg := NEW (Seg, items := NEW (RefSeq.T).init ());
    nImports := MSIR.ModuleImportBinderCount (m);
    nProcs := MSIR.ModuleProcCount (m);
    nTC := MSIR.ModuleTypeDescCount (m);
    nTL := MSIR.ModuleTypeLinkCount (m);

    (* data: ModuleInfo, user globals, imports *)
    NewSym (MSIR.ModuleInfoName (m), dseg,
            Alloc (dseg, B (M3RT.MI_SIZE), ptr, NEW (ModuleInfoItem)));
    VAR n := MSIR.ModuleGlobalStructSize (m) - B (M3RT.MI_SIZE);
    BEGIN
      IF n > 0 THEN EVAL Alloc (dseg, n, 1, NEW (GlobalsItem)) END;
    END;
    IF nImports > 0 THEN
      k := Alloc (dseg, nImports * B (M3RT.II_SIZE), ptr, NEW (ImportsItem));
      FOR i := 0 TO nImports - 1 DO
        NewSym (modName & "_M3_imp." & Fmt.Int (i), dseg,
                k + i * B (M3RT.II_SIZE));
      END;
    END;

    (* constants: procedure names and table, file name, global GC map *)
    VAR names := NEW (REF ARRAY OF INTEGER, nProcs);
    BEGIN
      FOR i := 0 TO nProcs - 1 DO
        VAR n := MSIR.ProcName (MSIR.ModuleProc (m, i));
        BEGIN
          names[i] := Alloc (cseg, Text.Length (n) + 1, 1,
                             NEW (CharsItem, chars := n));
        END;
      END;
      procInfoOff := -1;
      IF nProcs > 0 THEN
        procInfoOff := Alloc (cseg, (nProcs + 1) * B (M3RT.PI_SIZE), ptr,
                              NEW (ProcInfoItem, names := names));
      END;
    END;
    src := SourceFile ();
    fileOff := Alloc (cseg, Text.Length (src) + 1, 1, NEW (CharsItem, chars := src));
    gcMapOff := Bytes (MSIR.ModuleGcMap (m));

    (* typecells *)
    tcs := NEW (REF ARRAY OF TypecellItem, nTC);
    firstTC := -1;
    FOR i := 0 TO nTC - 1 DO
      VAR d := MSIR.ModuleTypeDesc (m, i);  sz: INTEGER;
          it := NEW (TypecellItem, d := d);
      BEGIN
        CASE MSIR.TypeDescKind (d) OF
        | TK_Obj => sz := B (M3RT.OTC_SIZE);
        | TK_Array => sz := B (M3RT.ATC_SIZE);
        ELSE sz := B (M3RT.TC_SIZE);
        END;
        tcs[i] := it;
        NewSym (MSIR.TypeDescName (d), dseg, Alloc (dseg, sz, ptr, it));
        IF i = 0 THEN firstTC := it.off END;
        IF i > 0 THEN tcs[i-1].next := it.off END;
        IF MSIR.TypeDescUserName (d) # NIL THEN
          it.name := Alloc (cseg, Text.Length (MSIR.TypeDescUserName (d)) + 1,
                            1, NEW (CharsItem, chars := MSIR.TypeDescUserName (d)));
        END;
        it.typeMap := Bytes (MSIR.TypeDescTypeMap (d));
        it.gcMap := Bytes (MSIR.TypeDescGcMap (d));
        it.desc := Bytes (MSIR.TypeDescPickleDesc (d));
        VAR cb: INTEGER;  chars := MSIR.TypeDescBrand (d, cb);
        BEGIN
          IF chars # NIL THEN
            it.brand := Alloc (cseg, Target.Integer.bytes + NUMBER (chars^) + cb,
                               ptr, NEW (BrandItem, d := d));
          END;
        END;
        IF MSIR.TypeDescKind (d) = TK_Obj AND MethodMode (d) = LinkMode.Table THEN
          it.methods := Alloc (cseg, MSIR.TypeDescMethodCount (d) * ptr, ptr,
                               NEW (MethodsItem, d := d));
        END;
      END;
    END;

    (* type links, chained through defn *)
    lastTL := -1;
    FOR i := 0 TO nTL - 1 DO
      VAR tl := MSIR.ModuleTypeLink (m, i);
          it := NEW (TypeLinkItem, tl := tl, prev := lastTL);
      BEGIN
        lastTL := Alloc (dseg, 2 * ptr, ptr, it);
        NewSym (MSIR.TypeLinkName (tl), dseg, lastTL);
      END;
    END;

    fullRevOff := -1;
    IF MSIR.ModuleRevelationCount (m) > 0 THEN
      fullRevOff := Alloc (dseg, (MSIR.ModuleRevelationCount (m) + 1)
                                   * B (M3RT.RV_SIZE), ptr, NEW (RevItem));
    END;

    FOR i := 0 TO MSIR.ModuleExcDescCount (m) - 1 DO
      VAR d := MSIR.ModuleExcDesc (m, i);  it := NEW (ExcItem, d := d);
          disp := MSIR.ExcDescDisplay (d);
      BEGIN
        IF disp # NIL AND NOT Text.Equal (disp, "") THEN
          it.name := Alloc (cseg, Text.Length (disp) + 1, 1,
                            NEW (CharsItem, chars := disp));
        END;
        NewSym (MSIR.ExcDescName (d), dseg,
                Alloc (dseg, B (M3RT.ED_SIZE), ptr, it));
      END;
    END;

    IF MSIR.ModuleTextLitCount (m) > 0 THEN
      k := Alloc (cseg, 5 * ptr, ptr, NEW (TextMethodsItem));
      FOR uid := 0 TO MSIR.ModuleTextLitCount (m) - 1 DO
        NewSym ("textlit_" & Fmt.Int (uid), cseg,
                Alloc (cseg, 3 * ptr + TextLitBytes (uid), ptr,
                       NEW (TextLitItem, uid := uid, methods := k)));
      END;
    END;

    backing := NEW (RefSeq.T).init ();
    FOR i := 0 TO MSIR.ModuleGlobalCount (m) - 1 DO
      VAR g := MSIR.ModuleGlobal (m, i);  a: INTEGER;  bs: Backing;
      BEGIN
        IF MSIR.GlobalIsExternal (g) THEN
          (* An <*EXTERNAL*> variable (D28): a C symbol, named as declared. *)
          VAR t := MSIR.GlobalType (g);  sz := Size (t, a);  ty := M3CG.Type.Struct;
          BEGIN
            IF IsScalar (t) AND NOT IsAgg (t) THEN ty := MType (t) END;
            bs := NEW (Backing, name := MSIR.GlobalName (g), align := MAX (a, 1),
                       external := TRUE, type := ty,
                       seg := NEW (Seg, size := MAX (sz, 0),
                                   items := NEW (RefSeq.T).init ()));
            backing.addhi (bs);
            NewSym (bs.name, bs.seg, 0);
          END;
        ELSIF IsBacking (g) THEN
          EVAL Size (MSIR.GlobalType (g), a);
          bs := NEW (Backing, name := MSIR.GlobalName (g), align := MAX (a, 1),
                     seg := NEW (Seg, size := MSIR.GlobalBackingBytes (g),
                                 items := NEW (RefSeq.T).init ()));
          backing.addhi (bs);
          NewSym (bs.name, bs.seg, 0);
        ELSIF IsStandalone (g) THEN
          VAR sz := Size (MSIR.GlobalType (g), a);
          BEGIN
            NewSym (MSIR.GlobalName (g), dseg,
                    Alloc (dseg, sz, MAX (a, 1), NEW (SpaceItem)));
          END;
        END;
      END;
    END;

    exFrame := NOT Target.Has_stack_walker;
    allocaProc := NIL;  setjmpProc := NIL;  jmpbufSize := NIL;
    handlesLists := NEW (RefSeq.T).init ();
    IF exFrame THEN
      FOR pi := 0 TO nProcs - 1 DO
        VAR p := MSIR.ModuleProc (m, pi);  sc: MSIR.Scope;  n: INTEGER;
            it: HandlesItem;  hasElse: BOOLEAN;
        BEGIN
          n := MSIR.ProcRaisesCount (p);
          IF MSIR.ProcChecksRaises (p) AND n > 0 THEN
            it := NEW (HandlesItem, p := p,
                       uids := NEW (REF ARRAY OF INTEGER, n));
            FOR j := 0 TO n - 1 DO
              it.uids[j] := MSIR.ExcDescUID (MSIR.ProcRaises (p, j));
            END;
            EVAL Alloc (cseg, (n + 1) * Target.Integer.bytes, ptr, it);
            handlesLists.addhi (it);
          END;
          FOR k := 0 TO MSIR.ProcScopeCount (p) - 1 DO
            sc := MSIR.ProcScopeAt (p, k);
            IF MSIR.ScopeKindOf (sc) = MSIR.ScopeKind.Except THEN
              n := MSIR.ScopeHandlerCount (sc);  hasElse := FALSE;
              FOR j := 0 TO n - 1 DO
                IF MSIR.ScopeHandler (sc, j).exc = NIL THEN hasElse := TRUE END;
              END;
              IF NOT hasElse THEN
                it := NEW (HandlesItem, s := sc,
                           uids := NEW (REF ARRAY OF INTEGER, n));
                FOR j := 0 TO n - 1 DO
                  it.uids[j] := MSIR.ExcDescUID (MSIR.ScopeHandler (sc, j).exc);
                END;
                EVAL Alloc (cseg, (n + 1) * Target.Integer.bytes, ptr, it);
                handlesLists.addhi (it);
              END;
            END;
          END;
        END;
      END;
    END;

    typecases := NEW (RefSeq.T).init ();
    FOR pi := 0 TO nProcs - 1 DO
      VAR p := MSIR.ModuleProc (m, pi);  b: MSIR.Block;  i: MSIR.Insn;
          it: TypecaseItem;
      BEGIN
        FOR bi := 0 TO MSIR.ProcBlockCount (p) - 1 DO
          b := MSIR.ProcBlock (p, bi);
          FOR ii := 0 TO MSIR.BlockInsnCount (b) - 1 DO
            i := MSIR.BlockInsn (b, ii);
            IF MSIR.InsnOp (i) = Op.Typecase THEN
              it := NEW (TypecaseItem, insn := i);
              EVAL Alloc (dseg, MSIR.InsnTypecaseClauseCount (i) * 2 * ptr,
                          ptr, it);
              typecases.addhi (it);
            END;
          END;
        END;
      END;
    END;

    FOR i := 0 TO MSIR.ModuleConstArrayCount (m) - 1 DO
      VAR ca := MSIR.ModuleConstArray (m, i);  a: INTEGER;
          sz := Size (MSIR.ConstArrayEltType (ca), a);
      BEGIN
        NewSym (MSIR.ConstArrayName (ca), cseg,
                Alloc (cseg, MAX (sz, 0) * MSIR.ConstArrayEltCount (ca),
                       MAX (a, ptr), NEW (ConstArrayItem, ca := ca)));
      END;
    END;
  END Setup;

CONST TK_Obj = 2;  TK_Array = 3;

(* A private, untraced global with no place in the module's global struct
   and no backing storage of its own (Formal's constdope_N): it gets space
   in the data segment. *)
PROCEDURE IsStandalone (g: MSIR.Global): BOOLEAN =
  VAR a: INTEGER;
  BEGIN
    RETURN NOT MSIR.GlobalIsExternal (g) AND MSIR.GlobalByteOffset (g) < 0
       AND NOT IsBacking (g) AND NOT MSIR.GlobalIsTraced (g)
       AND Size (MSIR.GlobalType (g), a) > 0;
  END IsStandalone;

PROCEDURE IsBacking (g: MSIR.Global): BOOLEAN =
  BEGIN
    RETURN NOT MSIR.GlobalIsExternal (g) AND MSIR.GlobalByteOffset (g) < 0
       AND MSIR.GlobalBackingBytes (g) > 0;
  END IsBacking;

PROCEDURE Bytes (b: MSIR.GcMapBytes): INTEGER =
  BEGIN
    IF b = NIL OR NUMBER (b^) = 0 THEN RETURN -1 END;
    RETURN Alloc (cseg, NUMBER (b^), 1, NEW (BytesItem, b := b));
  END Bytes;

PROCEDURE FindGlobal (name: TEXT): MSIR.Global =
  VAR g: MSIR.Global;
  BEGIN
    FOR i := 0 TO MSIR.ModuleGlobalCount (mod) - 1 DO
      g := MSIR.ModuleGlobal (mod, i);
      IF Text.Equal (MSIR.GlobalName (g), name) THEN RETURN g END;
    END;
    RETURN NIL;
  END FindGlobal;



PROCEDURE TextLitBytes (uid: INTEGER): INTEGER =
  VAR cnt := MSIR.ModuleTextLitCnt (mod, uid);  w := 1;
  BEGIN
    IF cnt < 0 THEN w := Target.WideCharSize () DIV Target.Char.size END;
    RETURN (ABS (cnt) + 1) * w;
  END TextLitBytes;

PROCEDURE SourceFile (): TEXT =
  VAR src := MSIR.ModuleSrcFile (mod);
  BEGIN
    IF src = NIL OR Text.Equal (src, "") THEN src := modName END;
    RETURN src;
  END SourceFile;

(*----- emission *)

PROCEDURE GenSeg (s: Seg) =
  BEGIN
    cg.bind_segment (s.var, RoundUp (MAX (s.size, 1), 16), 16,
                     M3CG.Type.Struct, FALSE, TRUE);
    cg.begin_init (s.var);
    FOR i := 0 TO s.items.size () - 1 DO
      NARROW (s.items.get (i), Item).emit ();
    END;
    cg.end_init (s.var);
  END GenSeg;

PROCEDURE InitInt (off, v: INTEGER;  t: M3CG.Type) =
  VAR i: Target.Int;
  BEGIN
    IF v = 0 THEN RETURN END;
    EVAL TInt.FromInt (v, i);
    cg.init_int (off, i, t);
  END InitInt;

PROCEDURE InitPtr (off: INTEGER;  s: Seg;  target: INTEGER) =
  BEGIN
    IF target >= 0 THEN cg.init_var (off, s.var, target) END;
  END InitPtr;

PROCEDURE EmitChars (it: CharsItem) =
  VAR printable := TRUE;  c: CHAR;
  BEGIN
    FOR i := 0 TO Text.Length (it.chars) - 1 DO
      c := Text.GetChar (it.chars, i);
      IF c < ' ' OR c > '~' THEN printable := FALSE END;
    END;
    IF printable THEN
      cg.init_chars (it.off, it.chars);
    ELSE
      FOR i := 0 TO Text.Length (it.chars) - 1 DO
        InitInt (it.off + i, ORD (Text.GetChar (it.chars, i)), M3CG.Type.Word8);
      END;
    END;
  END EmitChars;

PROCEDURE EmitBytes (it: BytesItem) =
  BEGIN
    FOR k := 0 TO LAST (it.b^) DO
      InitInt (it.off + k, it.b[k], M3CG.Type.Word8);
    END;
  END EmitBytes;

PROCEDURE EmitProcInfo (it: ProcInfoItem) =
  VAR off := it.off;
  BEGIN
    FOR i := 0 TO MSIR.ModuleProcCount (mod) - 1 DO
      VAR p := MSIR.ModuleProc (mod, i);
      BEGIN
        IF MSIR.ProcBlockCount (p) > 0 THEN
          cg.init_proc (off + B (M3RT.PI_proc), InfoProc (p));
          InitPtr (off + B (M3RT.PI_name), cseg, it.names[i]);
          INC (off, B (M3RT.PI_SIZE));
        END;
      END;
    END;
  END EmitProcInfo;

(* The procedure a ProcInfo entry names: the binder for the unit's body. *)
PROCEDURE InfoProc (p: MSIR.Proc): M3CG.Proc =
  BEGIN
    IF p = BodyProc () THEN RETURN binder END;
    RETURN ProcFor (p);
  END InfoProc;

PROCEDURE EmitModuleInfo (it: ModuleInfoItem) =
  BEGIN
    InitPtr (it.off + B (M3RT.MI_file), cseg, fileOff);
    InitPtr (it.off + B (M3RT.MI_type_cells), dseg, firstTC);
    InitPtr (it.off + B (M3RT.MI_type_cell_ptrs), dseg, lastTL);
    InitPtr (it.off + B (M3RT.MI_full_rev), dseg, fullRevOff);
    InitPtr (it.off + B (M3RT.MI_proc_info), cseg, procInfoOff);
    InitPtr (it.off + B (M3RT.MI_var_map), cseg, gcMapOff);
    InitPtr (it.off + B (M3RT.MI_gc_map), cseg, gcMapOff);
    IF MSIR.ModuleImportBinderCount (mod) > 0 THEN
      VAR r: REFANY;
      BEGIN
        EVAL syms.get (modName & "_M3_imp.0", r);
        InitPtr (it.off + B (M3RT.MI_imports), dseg, NARROW (r, Loc).off);
      END;
    END;
    cg.init_proc (it.off + B (M3RT.MI_binder), binder);
    InitInt (it.off + B (M3RT.MI_gc_flags), 3, Int64);
  END EmitModuleInfo;

TYPE GInit = RECORD off: INTEGER;  v: MSIR.Value END;

(* Initial values of user globals, in offset order. *)
(* Zero-filled space: a standalone private global the code initializes
   itself (e.g. Formal's constdope_N, a constant open array's dope). *)
PROCEDURE EmitNothing (<*UNUSED*> it: SpaceItem) =
  BEGIN
  END EmitNothing;

PROCEDURE EmitGlobalInits (<*UNUSED*> it: GlobalsItem) =
  VAR nInits := MSIR.ModuleGlobalInitCount (mod);
      inits := NEW (REF ARRAY OF GInit, nInits);  tmp: GInit;
  BEGIN
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
      InitConst (inits[i].off, inits[i].v, MSIR.ValueType (inits[i].v));
    END;
  END EmitGlobalInits;

(* Initialise the bytes at off in the segment being generated to the
   constant v of type t. *)
PROCEDURE InitConst (off: INTEGER;  v: MSIR.Value;  t: MSIR.T) =
  VAR f: Target.Float;  r: REFANY;  name := MSIR.ValueName (v);
  BEGIN
    CASE MSIR.GetValueKind (v) OF
    | ValueKind.ConstInt =>
        IF IsWideSet (t) THEN
          FOR k := 0 TO SetBytes (t) DIV 8 - 1 DO
            IF WideWord (MSIR.GetIntVal (v), k) # 0 THEN
              InitInt (off + k * 8, WideWord (MSIR.GetIntVal (v), k), Int64);
            END;
          END;
        ELSIF IsPtr (t) THEN
          InitInt (off, MSIR.GetIntVal (v), Addr);
        ELSE
          InitInt (off, CanonInt (MSIR.GetIntVal (v), IntBits (t)), MType (t));
        END;
    | ValueKind.ConstFloat =>
        MSIR.GetFloatVal (v, f);
        cg.init_float (off, f);
    | ValueKind.ConstNil =>
    | ValueKind.ConstProc =>
        cg.init_proc (off, ProcFor (MSIR.GetConstProc (v)));
    | ValueKind.ConstTextLit =>
        EVAL syms.get ("textlit_" & Fmt.Int (MSIR.GetTextLitUID (v)), r);
        InitPtr (off, NARROW (r, Loc).s, NARROW (r, Loc).off + Target.Address.bytes);
    | ValueKind.ConstStruct =>
        t := Base (t);
        FOR i := 0 TO MSIR.GetConstStructFieldCount (v) - 1 DO
          VAR fld := MSIR.StructField (t, i);
          BEGIN
            InitConst (off + FieldOffset (t, i), MSIR.GetConstStructField (v, i),
                       fld.type);
          END;
        END;
    | ValueKind.ConstAggArray =>
        VAR elt := MSIR.FixedArrayElt (Base (t));  a: INTEGER;
            sz := Size (elt, a);
        BEGIN
          FOR i := 0 TO MSIR.GetConstStructFieldCount (v) - 1 DO
            InitConst (off + i * sz, MSIR.GetConstStructField (v, i), elt);
          END;
        END;
    | ValueKind.StructFieldRef =>
        EVAL syms.get (Text.Sub (name, 1), r);
        InitPtr (off, NARROW (r, Loc).s,
                 NARROW (r, Loc).off + MSIR.GetStructFieldOffset (v));
    ELSE
      IF name # NIL AND HasPrefix (name, "@")
         AND syms.get (Text.Sub (name, 1), r) THEN
        InitPtr (off, NARROW (r, Loc).s, NARROW (r, Loc).off);
      END;
    END;
  END InitConst;

(* TRUE if InitConst can lay out v. *)
PROCEDURE IsInitConst (v: MSIR.Value): BOOLEAN =
  VAR r: REFANY;  name := MSIR.ValueName (v);
  BEGIN
    CASE MSIR.GetValueKind (v) OF
    | ValueKind.ConstInt, ValueKind.ConstFloat, ValueKind.ConstNil,
      ValueKind.ConstProc, ValueKind.ConstTextLit => RETURN TRUE;
    | ValueKind.ConstStruct, ValueKind.ConstAggArray =>
        FOR i := 0 TO MSIR.GetConstStructFieldCount (v) - 1 DO
          IF NOT IsInitConst (MSIR.GetConstStructField (v, i)) THEN
            RETURN FALSE;
          END;
        END;
        RETURN TRUE;
    ELSE
      RETURN name # NIL AND HasPrefix (name, "@")
             AND syms.get (Text.Sub (name, 1), r);
    END;
  END IsInitConst;

PROCEDURE EmitImports (it: ImportsItem) =
  VAR off := it.off;  n := MSIR.ModuleImportBinderCount (mod);
  BEGIN
    FOR k := 0 TO n - 1 DO
      cg.init_proc (off + B (M3RT.II_binder),
                    ImportBinder (MSIR.ModuleImportBinder (mod, k)));
      IF k < n - 1 THEN
        InitPtr (off + B (M3RT.II_next), dseg, off + B (M3RT.II_SIZE));
      END;
      INC (off, B (M3RT.II_SIZE));
    END;
  END EmitImports;

PROCEDURE EmitTypecell (it: TypecellItem) =
  VAR d := it.d;  o := it.off;  ipName := MSIR.TypeDescInitProcName (d);
  BEGIN
    InitInt (o + B (M3RT.TC_selfID), MSIR.TypeDescUID (d), Int64);
    FOR i := 0 TO 7 DO
      InitInt (o + B (M3RT.TC_fp) + i, MSIR.TypeDescFPByte (d, i),
               M3CG.Type.Word8);
    END;
    InitInt (o + B (M3RT.TC_traced), ORD (MSIR.TypeDescTraced (d)),
             M3CG.Type.Word8);
    InitInt (o + B (M3RT.TC_kind), MSIR.TypeDescKind (d), M3CG.Type.Word8);
    InitInt (o + B (M3RT.TC_dataAlignment), MSIR.TypeDescAlign (d),
             M3CG.Type.Word8);
    InitInt (o + B (M3RT.TC_dataSize), MSIR.TypeDescSize (d), Int64);
    InitPtr (o + B (M3RT.TC_type_map), cseg, it.typeMap);
    InitPtr (o + B (M3RT.TC_gc_map), cseg, it.gcMap);
    InitPtr (o + B (M3RT.TC_type_desc), cseg, it.desc);
    IF ipName # NIL THEN
      cg.init_proc (o + B (M3RT.TC_initProc), ProcByName (ipName));
    END;
    InitPtr (o + B (M3RT.TC_brand), cseg, it.brand);
    InitPtr (o + B (M3RT.TC_name), cseg, it.name);
    InitPtr (o + B (M3RT.TC_next), dseg, it.next);
    CASE MSIR.TypeDescKind (d) OF
    | TK_Obj =>
        InitInt (o + B (M3RT.OTC_parentID), MSIR.TypeDescParentUID (d), Int64);
        IF it.linkProc # NIL THEN
          cg.init_proc (o + B (M3RT.OTC_linkProc), it.linkProc);
        END;
        InitInt (o + B (M3RT.OTC_methodSize), MSIR.TypeDescMethodBytes (d), Int64);
        InitPtr (o + B (M3RT.OTC_defaultMethods), cseg, it.methods);
    | TK_Array =>
        InitInt (o + B (M3RT.ATC_nDimensions), MSIR.TypeDescNDimensions (d), Int64);
        InitInt (o + B (M3RT.ATC_elementSize), MSIR.TypeDescElementSize (d), Int64);
    ELSE
    END;
  END EmitTypecell;

(* The RT0 brand record, as Brand.Compile lays it out: an INTEGER count of
   the characters' bytes, then the characters and a NUL character. *)
PROCEDURE EmitBrand (it: BrandItem) =
  VAR cb: INTEGER;  chars := MSIR.TypeDescBrand (it.d, cb);
  BEGIN
    InitInt (it.off, NUMBER (chars^), Int64);
    FOR i := 0 TO LAST (chars^) DO
      InitInt (it.off + Target.Integer.bytes + i, chars[i], M3CG.Type.Word8);
    END;
  END EmitBrand;

PROCEDURE EmitMethods (it: MethodsItem) =
  BEGIN
    FOR j := 0 TO MSIR.TypeDescMethodCount (it.d) - 1 DO
      cg.init_proc (it.off + j * Target.Address.bytes,
                    ProcByName (MSIR.TypeDescMethod (it.d, j)));
    END;
  END EmitMethods;

PROCEDURE EmitTypecase (it: TypecaseItem) =
  VAR ptr := Target.Address.bytes;
  BEGIN
    FOR k := 0 TO MSIR.InsnTypecaseClauseCount (it.insn) - 1 DO
      VAR uid := MSIR.InsnTypecaseClause (it.insn, k).uid;
      BEGIN
        IF uid # 0 THEN InitInt (it.off + k * 2 * ptr + ptr, uid, Int64) END;
      END;
    END;
  END EmitTypecase;

PROCEDURE EmitHandles (it: HandlesItem) =
  BEGIN
    FOR k := 0 TO LAST (it.uids^) DO
      InitInt (it.off + k * Target.Integer.bytes, it.uids[k], Int64);
    END;
  END EmitHandles;

PROCEDURE HandlesOff (sc: MSIR.Scope): INTEGER =
  BEGIN
    FOR k := 0 TO handlesLists.size () - 1 DO
      WITH it = NARROW (handlesLists.get (k), HandlesItem) DO
        IF it.s = sc AND sc # NIL THEN RETURN it.off END;
      END;
    END;
    <* ASSERT FALSE *>
  END HandlesOff;

PROCEDURE RaisesOff (p: MSIR.Proc): INTEGER =
  BEGIN
    FOR k := 0 TO handlesLists.size () - 1 DO
      WITH it = NARROW (handlesLists.get (k), HandlesItem) DO
        IF it.p = p THEN RETURN it.off END;
      END;
    END;
    <* ASSERT FALSE *>
  END RaisesOff;

PROCEDURE TypecaseOff (i: MSIR.Insn): INTEGER =
  BEGIN
    FOR k := 0 TO typecases.size () - 1 DO
      WITH it = NARROW (typecases.get (k), TypecaseItem) DO
        IF it.insn = i THEN RETURN it.off END;
      END;
    END;
    <* ASSERT FALSE *>
  END TypecaseOff;

PROCEDURE EmitTypeLink (it: TypeLinkItem) =
  BEGIN
    InitPtr (it.off, dseg, it.prev);
    InitInt (it.off + Target.Address.bytes, MSIR.TypeLinkUID (it.tl), Int64);
  END EmitTypeLink;

PROCEDURE EmitRevs (it: RevItem) =
  VAR off := it.off;
  BEGIN
    FOR i := 0 TO MSIR.ModuleRevelationCount (mod) - 1 DO
      VAR r := MSIR.ModuleRevelation (mod, i);
      BEGIN
        InitInt (off + B (M3RT.RV_lhs_id), MSIR.RevelationLhsUID (r), Int64);
        InitInt (off + B (M3RT.RV_rhs_id), MSIR.RevelationRhsUID (r), Int64);
        INC (off, B (M3RT.RV_SIZE));
      END;
    END;
  END EmitRevs;

PROCEDURE EmitExc (it: ExcItem) =
  BEGIN
    InitInt (it.off + B (M3RT.ED_uid), MSIR.ExcDescUID (it.d), Int64);
    InitPtr (it.off + B (M3RT.ED_name), cseg, it.name);
  END EmitExc;

PROCEDURE EmitTextMethods (it: TextMethodsItem) =
  BEGIN
    FOR i := 0 TO 4 DO
      cg.init_proc (it.off + i * Target.Address.bytes,
                    ProcFor (MSIR.ModuleGetTextLitHook (mod, i)));
    END;
  END EmitTextMethods;

PROCEDURE EmitTextLit (it: TextLitItem) =
  VAR ptr := Target.Address.bytes;
      chars := MSIR.ModuleTextLitChars (mod, it.uid);
      cnt := MSIR.ModuleTextLitCnt (mod, it.uid);
  BEGIN
    InitInt (it.off, Word.Shift (TEXT_typecode, M3RT.RH_typecode_offset), Int64);
    InitPtr (it.off + ptr, cseg, it.methods);
    InitInt (it.off + 2 * ptr, cnt, Int64);
    EmitChars (NEW (CharsItem, off := it.off + 3 * ptr, chars := chars));
  END EmitTextLit;

PROCEDURE EmitConstArray (it: ConstArrayItem) =
  VAR ca := it.ca;  t := MSIR.ConstArrayEltType (ca);  a: INTEGER;
      sz := Size (t, a);
  BEGIN
    FOR j := 0 TO MSIR.ConstArrayEltCount (ca) - 1 DO
      InitConst (it.off + j * sz, MSIR.ConstArrayElt (ca, j), t);
    END;
  END EmitConstArray;

(*------------------------------------------------------------ link procs *)

TYPE LinkMode = {None, Inits, Dynamic, Slots, Table};

(* How an object typecell's methods reach its method suite, as in
   MSIRToLLVM.EmitTypeCells. *)
PROCEDURE MethodMode (d: MSIR.TypeDesc): LinkMode =
  VAR n := MSIR.TypeDescMethodCount (d);
  BEGIN
    IF MSIR.TypeDescUseLinkProc (d) AND MSIR.TypeDescMethodInitCount (d) > 0 THEN
      RETURN LinkMode.Inits;
    ELSIF n = 0 THEN
      RETURN LinkMode.None;
    ELSIF MSIR.TypeDescDynamicMethOff (d) THEN
      RETURN LinkMode.Dynamic;
    END;
    FOR j := 0 TO n - 1 DO
      IF MSIR.TypeDescMethod (d, j) = NIL THEN RETURN LinkMode.Slots END;
    END;
    RETURN LinkMode.Table;
  END MethodMode;

(* Generate the link procedures of the unit's object typecells.  Each
   stores method procedures into the vtable RTLinker allocated:
   PROCEDURE (tp: ADDRESS (* the typecell *)). *)
PROCEDURE GenLinkProcs () =
  VAR ptr := Target.Address.bytes;
  BEGIN
    FOR i := 0 TO dseg.items.size () - 1 DO
      TYPECASE dseg.items.get (i) OF
      | TypecellItem (it) =>
          VAR d := it.d;  mode := LinkMode.None;  q: M3CG.Proc;  tp: M3CG.Var;
          BEGIN
            IF MSIR.TypeDescKind (d) = TK_Obj THEN mode := MethodMode (d) END;
            IF mode IN SET OF LinkMode {LinkMode.Inits, LinkMode.Dynamic,
                                        LinkMode.Slots} THEN
              q := cg.declare_procedure (
                     ID (MinusToM (MSIR.TypeDescName (d)) & "_linkproc"), 1,
                     M3CG.Type.Void, 0, Target.DefaultCall, FALSE, NIL);
              tp := cg.declare_param (ID ("tp"), ptr, ptr, Addr, 0, FALSE,
                                      FALSE, M3CG.Always);
              it.linkProc := q;
              cg.begin_procedure (q);
              CASE mode OF
              | LinkMode.Inits =>
                  FOR j := 0 TO MSIR.TypeDescMethodInitCount (d) - 1 DO
                    VAR mname := MSIR.TypeDescMethodInitProc (d, j);
                        off := MSIR.TypeDescMethodInitOff (d, j);
                        r: REFANY;
                    BEGIN
                      IF mname # NIL THEN
                        PushMethods (tp);
                        IF MSIR.TypeDescMethodInitBaseKnown (d, j) THEN
                          cg.add_offset (MSIR.TypeDescMethodInitBase (d, j) + off);
                        ELSE
                          (* + the declaring type's OTC_methodOffset *)
                          EVAL syms.get ("tl_obj_" & Fmt.Int (
                                 MSIR.TypeDescMethodInitDeclUID (d, j)), r);
                          cg.load_address (NARROW (r, Loc).s.var, NARROW (r, Loc).off);
                          cg.load_indirect (0, Addr, Addr);
                          cg.load_indirect (B (M3RT.OTC_methodOffset), Int64, Int64);
                          cg.index_address (Int64, 1);
                          cg.add_offset (off);
                        END;
                        cg.load_procedure (ProcByName (mname));
                        cg.store_indirect (0, Addr, Addr);
                      END;
                    END;
                  END;
              | LinkMode.Dynamic, LinkMode.Slots =>
                  FOR j := 0 TO MSIR.TypeDescMethodCount (d) - 1 DO
                    VAR mname := MSIR.TypeDescMethod (d, j);
                    BEGIN
                      IF mname # NIL THEN
                        PushMethods (tp);
                        IF mode = LinkMode.Dynamic THEN
                          cg.load (tp, 0, Addr, Addr);
                          cg.load_indirect (B (M3RT.OTC_methodOffset), Int64, Int64);
                          cg.index_address (Int64, 1);
                        END;
                        cg.add_offset (j * ptr);
                        cg.load_procedure (ProcByName (mname));
                        cg.store_indirect (0, Addr, Addr);
                      END;
                    END;
                  END;
              ELSE
              END;
              cg.exit_proc (M3CG.Type.Void);
              cg.end_procedure (q);
            END;
          END;
      ELSE
      END;
    END;
  END GenLinkProcs;

(* Push tp.defaultMethods, the vtable. *)
PROCEDURE PushMethods (tp: M3CG.Var) =
  BEGIN
    cg.load (tp, 0, Addr, Addr);
    cg.load_indirect (B (M3RT.OTC_defaultMethods), Addr, Addr);
  END PushMethods;

(* The procedure with linker name sym: the unit's own, or imported as
   PROCEDURE () if not already known. *)
PROCEDURE ProcByName (sym: TEXT): M3CG.Proc =
  VAR r: REFANY;  q: M3CG.Proc;
  BEGIN
    IF procs.get (sym, r) THEN RETURN NARROW (r, ProcRef).p END;
    q := cg.import_procedure (ID (sym), 0, M3CG.Type.Void, Target.DefaultCall);
    EVAL procs.put (sym, NEW (ProcRef, p := q));
    RETURN q;
  END ProcByName;

(*------------------------------------------------------------ procedures *)

PROCEDURE ProcFor (p: MSIR.Proc): M3CG.Proc =
  VAR r: REFANY;  sym := CSymbol (p);
  BEGIN
    IF procs.get (sym, r) THEN RETURN NARROW (r, ProcRef).p END;
    RETURN ImportProc (sym, p);
  END ProcFor;

PROCEDURE ImportProc (sym: TEXT;  p: MSIR.Proc): M3CG.Proc =
  VAR n := MSIR.ProcParamCount (p);  q: M3CG.Proc;
  BEGIN
    q := cg.import_procedure (ID (sym), n + ORD (HasAggResult (p)),
                              RetType (MSIR.ProcResultType (p)),
                              Target.DefaultCall);
    EVAL DeclareParams (p);
    EVAL procs.put (sym, NEW (ProcRef, p := q));
    RETURN q;
  END ImportProc;

(* Declare p's parameters, including CG's hidden result parameter; return
   the formals' Vars, and the result parameter's in the last element. *)
PROCEDURE DeclareParams (p: MSIR.Proc): REF ARRAY OF M3CG.Var =
  VAR n := MSIR.ProcParamCount (p);  t: MSIR.T;  a: INTEGER;
      pv := NEW (REF ARRAY OF M3CG.Var, n + 1);
      ptr := Target.Address.bytes;
  BEGIN
    IF HasAggResult (p) AND ResultFirst () THEN
      pv[n] := cg.declare_param (ID ("_result"), ptr, ptr, Addr, 0, FALSE,
                                 FALSE, M3CG.Maybe);
    END;
    FOR k := 0 TO n - 1 DO
      t := MSIR.ValueType (MSIR.ProcParam (p, k));
      pv[k] := cg.declare_param (ID (ParamName (p, k)), ParamSize (t, a), a,
                                 SigType (t), 0, FALSE, FALSE, M3CG.Maybe);
    END;
    IF HasAggResult (p) AND NOT ResultFirst () THEN
      pv[n] := cg.declare_param (ID ("_result"), ptr, ptr, Addr, 0, FALSE,
                                 FALSE, M3CG.Maybe);
    END;
    RETURN pv;
  END DeclareParams;

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
    paramVars := NEW (TextRefTbl.Default).init ();
    generated := NEW (TextRefTbl.Default).init ();
    curLine := -1;

    cg.begin_unit (0);
    cg.widechar_size (Target.WideCharSize ());
    cg.set_source_file (SourceFile ());
    cseg.var := cg.declare_segment (M3ID.NoID, -1, TRUE);
    IF isIface
      THEN dseg.var := cg.declare_segment (ID ("I_" & modName), -1, FALSE);
      ELSE dseg.var := cg.declare_segment (ID ("M_" & modName), -1, FALSE);
    END;
    IF exFrame THEN
      FOR pi := 0 TO MSIR.ModuleProcCount (m) - 1 DO
        IF MSIR.ProcScopeCount (MSIR.ModuleProc (m, pi)) > 1 THEN
          DeclareFrameImports ();
        END;
      END;
    END;
    IF UsesAllocaDyn (m) THEN DeclareAllocaImport () END;
    FOR k := 0 TO backing.size () - 1 DO
      WITH bs = NARROW (backing.get (k), Backing) DO
        IF bs.external THEN
          bs.seg.var := cg.import_global (ID (bs.name), bs.seg.size, bs.align,
                                          bs.type, 0);
        ELSE
          bs.seg.var := cg.declare_global (ID (bs.name), bs.seg.size, bs.align,
                                           M3CG.Type.Struct, 0, FALSE, FALSE);
        END;
      END;
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

    (* The unit's own procedures, then everything they call, so each
       import carries its real signature. *)
    FOR i := 0 TO MSIR.ModuleProcCount (m) - 1 DO
      p := MSIR.ModuleProc (m, i);
      IF MSIR.ProcBlockCount (p) > 0 THEN DeclareProc (p) END;
    END;
    FOR i := 0 TO MSIR.ModuleProcCount (m) - 1 DO
      ImportCallees (MSIR.ModuleProc (m, i));
    END;
    FOR k := 0 TO MSIR.ModuleImportBinderCount (m) - 1 DO
      EVAL ImportBinder (MSIR.ModuleImportBinder (m, k));
    END;

    GenBinder (modeVar);
    FOR i := 0 TO MSIR.ModuleProcCount (m) - 1 DO
      p := MSIR.ModuleProc (m, i);
      IF MSIR.ProcBlockCount (p) > 0 AND NOT Generated (p) THEN GenProc (p) END;
    END;
    GenLinkProcs ();

    GenSeg (cseg);
    GenSeg (dseg);
    cg.end_unit ();
    cg := NIL;
    mod := NIL;
  END Lower;

PROCEDURE ImportCallees (p: MSIR.Proc) =
  VAR b: MSIR.Block;  i: MSIR.Insn;  v: MSIR.Value;
  BEGIN
    IF MSIR.ProcScopeCount (p) > 1 THEN
      EVAL ProcFor (MSIR.ModuleResumeRaise (mod));
    END;
    IF exFrame
       AND (MSIR.ProcScopeCount (p) > 1 OR MSIR.ProcChecksRaises (p)) THEN
      EVAL ProcFor (MSIR.ModulePushEFrame (mod));
      EVAL ProcFor (MSIR.ModulePopEFrame (mod));
    END;
    FOR bi := 0 TO MSIR.ProcBlockCount (p) - 1 DO
      b := MSIR.ProcBlock (p, bi);
      FOR ii := 0 TO MSIR.BlockInsnCount (b) - 1 DO
        i := MSIR.BlockInsn (b, ii);
        IF MSIR.InsnOp (i) = Op.Call AND MemOp (MSIR.InsnCallee (i)) = Mem.None THEN
          EVAL ProcFor (MSIR.InsnCallee (i));
        END;
        IF MSIR.InsnOp (i) = Op.Typecase THEN
          EVAL ProcFor (MSIR.ModuleScanTypecase (mod));
        END;
        FOR k := 0 TO MSIR.InsnOperandCount (i) - 1 DO
          v := MSIR.InsnOperand (i, k);
          IF v # NIL AND MSIR.GetValueKind (v) = ValueKind.ConstProc THEN
            EVAL ProcFor (MSIR.GetConstProc (v));
          END;
        END;
      END;
    END;
  END ImportCallees;

(* The unit may list a procedure twice (a hook both compiled here and
   looked up again); generate it once. *)
VAR generated: TextRefTbl.T;

PROCEDURE Generated (p: MSIR.Proc): BOOLEAN =
  VAR r: REFANY;
  BEGIN
    IF generated.get (CSymbol (p), r) THEN RETURN TRUE END;
    EVAL generated.put (CSymbol (p), p);
    RETURN FALSE;
  END Generated;

PROCEDURE DeclareProc (p: MSIR.Proc) =
  VAR n := MSIR.ProcParamCount (p);  q: M3CG.Proc;
      sym := CSymbol (p);  r: REFANY;
  BEGIN
    IF procs.get (sym, r) THEN RETURN END;
    q := cg.declare_procedure (ID (sym), n + ORD (HasAggResult (p)),
                               RetType (MSIR.ProcResultType (p)),
                               0, Target.DefaultCall, TRUE, NIL);
    EVAL procs.put (sym, NEW (ProcRef, p := q));
    EVAL paramVars.put (sym, DeclareParams (p));
  END DeclareProc;

VAR paramVars: TextRefTbl.T;
    resultVar: M3CG.Var;  (* the current procedure's "_result", or NIL *)
    curProc: MSIR.Proc;   (* the procedure being generated *)

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
    cg.load_address (dseg.var, 0);
    cg.exit_proc (Addr);
    cg.end_procedure (binder);
  END GenBinder;

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
    IF IsAgg (l.t) THEN
      l.kind := ValKind.AddrOf;
      l.var := AggTemp (l.t);
    ELSE
      l.var := TempFor (l.t);
    END;
    EVAL vals.put (name, l);
    RETURN l;
  END Loc0;

PROCEDURE AggTemp (t: MSIR.T): M3CG.Var =
  VAR a: INTEGER;  sz := Size (t, a);
  BEGIN
    RETURN cg.declare_local (M3ID.NoID, RoundUp (MAX (sz, 1), a), a,
                             M3CG.Type.Struct, 0, TRUE, FALSE, M3CG.Maybe);
  END AggTemp;

(* Copy an aggregate of type t: s1 is the destination, s0 the source. *)
PROCEDURE CopyAgg (t: MSIR.T) =
  VAR a: INTEGER;  sz := Size (t, a);
  BEGIN
    IF a >= 8 AND sz MOD 8 = 0 THEN cg.copy (sz DIV 8, Int64, FALSE)
    ELSIF a >= 4 AND sz MOD 4 = 0 THEN cg.copy (sz DIV 4, M3CG.Type.Int32, FALSE)
    ELSE cg.copy (sz, M3CG.Type.Word8, FALSE)
    END;
  END CopyAgg;

(* Store the constant aggregate v at the address on the stack. *)
PROCEDURE StoreConstAgg (v: MSIR.Value;  t: MSIR.T;  addr: MSIR.Value) =
  VAR a: INTEGER;  sz := Size (t, a);
  BEGIN
    Push (addr);
    cg.zero (sz, M3CG.Type.Word8);
    IF MSIR.GetValueKind (v) # ValueKind.ConstNil THEN
      StoreConstAt (addr, 0, v, t);
    END;
  END StoreConstAgg;

PROCEDURE StoreConstAt (addr: MSIR.Value;  off: INTEGER;  v: MSIR.Value;
                        t: MSIR.T) =
  BEGIN
    CASE MSIR.GetValueKind (v) OF
    | ValueKind.ConstNil =>
    | ValueKind.ConstStruct =>
        t := Base (t);
        FOR i := 0 TO MSIR.GetConstStructFieldCount (v) - 1 DO
          VAR fld := MSIR.StructField (t, i);
          BEGIN
            StoreConstAt (addr, off + FieldOffset (t, i),
                          MSIR.GetConstStructField (v, i), fld.type);
          END;
        END;
    | ValueKind.ConstAggArray =>
        VAR elt := MSIR.FixedArrayElt (Base (t));  a: INTEGER;
            sz := Size (elt, a);
        BEGIN
          FOR i := 0 TO MSIR.GetConstStructFieldCount (v) - 1 DO
            StoreConstAt (addr, off + i * sz, MSIR.GetConstStructField (v, i), elt);
          END;
        END;
    ELSE
      IF MSIR.GetValueKind (v) = ValueKind.ConstInt AND MSIR.GetIntVal (v) = 0 THEN
        RETURN;  (* already zeroed *)
      END;
      Push (addr);
      IF off # 0 THEN cg.add_offset (off) END;
      Push (v);
      cg.store_indirect (0, ZType (t), MType (t));
    END;
  END StoreConstAt;

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

(* A multi-word set constant: the integer v of type iN, i.e. v in the low
   word and its sign in the others (m3front uses 0 and -1). *)
PROCEDURE WideWord (v, k: INTEGER): INTEGER =
  BEGIN
    IF k = 0 THEN RETURN v END;
    IF v < 0 THEN RETURN -1 END;
    RETURN 0;
  END WideWord;

(* Push the address of a fresh temporary holding the constant v of type t. *)
PROCEDURE PushWideConst (v: INTEGER;  t: MSIR.T) =
  VAR tmp := AggTemp (t);
  BEGIN
    FOR k := 0 TO SetBytes (t) DIV 8 - 1 DO
      PushInt (WideWord (v, k));
      cg.store (tmp, k * 8, Int64, Int64);
    END;
    cg.load_address (tmp, 0);
  END PushWideConst;

PROCEDURE PushAddr (v: M3CG.Var;  off: INTEGER) =
  BEGIN
    cg.load_address (v, off);
  END PushAddr;

PROCEDURE Push (v: MSIR.Value) =
  VAR t := MSIR.ValueType (v);  r: REFANY;  f: Target.Float;
      name := MSIR.ValueName (v);
  BEGIN
    CASE MSIR.GetValueKind (v) OF
    | ValueKind.ConstInt =>
        IF IsWideSet (t) THEN
          PushWideConst (MSIR.GetIntVal (v), t);
        ELSIF IsPtr (t) THEN
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
    | ValueKind.ConstTextLit =>
        EVAL syms.get ("textlit_" & Fmt.Int (MSIR.GetTextLitUID (v)), r);
        PushAddr (NARROW (r, Loc).s.var,
                  NARROW (r, Loc).off + Target.Address.bytes);
    | ValueKind.StructFieldRef =>
        EVAL syms.get (Text.Sub (name, 1), r);
        PushAddr (NARROW (r, Loc).s.var,
                  NARROW (r, Loc).off + MSIR.GetStructFieldOffset (v));
    | ValueKind.GlobalRef =>
        (* the address of a global with its own M3CG variable: an
           <*EXTERNAL*> C variable or a large global's storage *)
        VAR key := name;
        BEGIN
          IF HasPrefix (key, "@") THEN key := Text.Sub (key, 1) END;
          EVAL syms.get (key, r);
          PushAddr (NARROW (r, Loc).s.var, NARROW (r, Loc).off);
        END;
    ELSE
      IF MSIR.ValueDefInsn (v) = NIL AND name # NIL AND HasPrefix (name, "@")
         AND syms.get (Text.Sub (name, 1), r) THEN
        PushAddr (NARROW (r, Loc).s.var, NARROW (r, Loc).off);
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
    IF l.kind = ValKind.AddrOf THEN
      (* an aggregate: the stack holds the address of its new value *)
      VAR src := cg.declare_temp (Target.Address.bytes, Target.Address.bytes,
                                  Addr, FALSE);
      BEGIN
        cg.store (src, 0, Addr, Addr);
        cg.load_address (l.var, 0);
        cg.load (src, 0, Addr, Addr);
        CopyAgg (l.t);
        cg.free_temp (src);
      END;
      RETURN;
    END;
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

(*------------------------------------------------- EH scopes (ex_stack) *)

(* D26 scopes are lowered to CG's ex_stack scheme, as TryStmt.Compile1 and
   TryFinStmt.Compile1 emit it: each protected scope's blocks go between
   start_try and end_try, entered only through its entry; calls in them
   are invokes naming the scope's landing pad, which follows end_try.
   M3C turns these into a C++ try block whose catch jumps to the landing
   pad with the activation, so handler and FINALLY code, emitted outside
   the try block, run after the C++ catch has finished.  Leaving a scope
   needs code only for a Finally scope: record the outcome in its selector
   and run its FINALLY code, whose end_finally continues the outcome. *)

CONST
  Sel_Exc     = 1;
  Sel_Return  = 2;
  FirstTarget = 3;              (* + k: the k-th distinct unwind_to target *)

TYPE
  EHScope = REF RECORD
    s       : MSIR.Scope;
    pad     : M3CG.Label := M3CG.No_label;  (* protected scopes *)
    act     : M3CG.Var := NIL;              (* protected: the activation *)
    sel     : M3CG.Var := NIL;              (* Finally: the outcome *)
    targets : RefSeq.T := NIL;              (* Finally: MSIR.Block per code *)
    returns := FALSE;                       (* Finally: a RETURN passes it *)
    placed  := FALSE;                       (* its blocks have been emitted *)
    frame   : M3CG.Var := NIL;              (* ex_frame: its EF1 frame *)
    jbuf    : M3CG.Var := NIL;              (* ex_frame: its jmp_buf's address *)
  END;

(* On a target without ex_stack (Target.Has_stack_walker FALSE) the scopes
   are lowered to CG's ex_frame scheme instead, as TryStmt.Compile2 and
   TryFinStmt.Compile3 emit it: entering a protected scope links an EF1
   frame (class Except, ExceptElse or Finally) with RTHooks.PushEFrame and
   calls setjmp on a jmp_buf allocated at the procedure's entry; a raise
   longjmps back, the runtime having already unlinked the frame, to the
   scope's "pad" label, where the activation is the frame's info field.
   Calls are plain calls.  Leaving a protected scope normally unlinks its
   frame with RTHooks.PopEFrame; the rest (handler dispatch, the Finally
   selector, end_finally) is as for ex_stack. *)

VAR
  exFrame   := FALSE;                       (* lowering to ex_frame *)
  allocaProc, setjmpProc: M3CG.Proc := NIL; (* ex_frame imports, per unit *)
  jmpbufSize: M3CG.Var := NIL;
  handlesLists: RefSeq.T := NIL;  (* HandlesItem, per Except scope or RAISES *)
  raisesFrame: M3CG.Var := NIL;   (* ex_frame: the procedure's RAISES frame *)

VAR
  eh        : REF ARRAY OF EHScope := NIL;  (* per scope; NIL: no scopes *)
  curScope  : MSIR.Scope;                   (* the block being emitted's *)
  curPad    : M3CG.Label := M3CG.No_label;  (* its invokes' landing pad *)
  retSlot   : M3CG.Var := NIL;              (* a RETURN's scalar result *)
  placedBlk : REF ARRAY OF BOOLEAN;

PROCEDURE EHOf (s: MSIR.Scope): EHScope =
  BEGIN
    RETURN eh[MSIR.ScopeIndex (s)];
  END EHOf;

PROCEDURE IsProtected (s: MSIR.Scope): BOOLEAN =
  BEGIN
    RETURN MSIR.ScopeKindOf (s) IN SET OF MSIR.ScopeKind {
             MSIR.ScopeKind.Except, MSIR.ScopeKind.Finally };
  END IsProtected;

(* The landing pad an exception raised in scope s goes to, or No_label. *)
PROCEDURE PadOf (s: MSIR.Scope): M3CG.Label =
  BEGIN
    IF exFrame THEN RETURN M3CG.No_label END;
    WHILE s # NIL DO
      IF IsProtected (s) THEN RETURN EHOf (s).pad END;
      s := MSIR.ScopeParent (s);
    END;
    RETURN M3CG.No_label;
  END PadOf;

(* The Except scope whose handler code s is in. *)
PROCEDURE HandlerOwner (s: MSIR.Scope): MSIR.Scope =
  BEGIN
    WHILE s # NIL DO
      IF MSIR.ScopeKindOf (s) = MSIR.ScopeKind.Handler THEN
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

(* The Finally scope that leaving 'from' for target (or returning) enters
   first, or NIL if the exit leaves no Finally scope. *)
PROCEDURE FirstFinally (from: MSIR.Scope;  target: MSIR.Block;
                        isReturn: BOOLEAN): MSIR.Scope =
  VAR s := from;
  BEGIN
    LOOP
      IF s = NIL THEN RETURN NIL END;
      IF NOT isReturn AND s = MSIR.BlockScope (target) THEN RETURN NIL END;
      IF MSIR.ScopeKindOf (s) = MSIR.ScopeKind.Finally THEN RETURN s END;
      s := MSIR.ScopeParent (s);
    END;
  END FirstFinally;

PROCEDURE TargetCode (f: EHScope;  target: MSIR.Block): INTEGER =
  BEGIN
    FOR k := 0 TO f.targets.size () - 1 DO
      IF f.targets.get (k) = target THEN RETURN FirstTarget + k END;
    END;
    f.targets.addhi (target);
    RETURN FirstTarget + f.targets.size () - 1;
  END TargetCode;

PROCEDURE NoteExit (from: MSIR.Scope;  target: MSIR.Block;  isReturn: BOOLEAN) =
  VAR s := FirstFinally (from, target, isReturn);
  BEGIN
    IF s = NIL THEN RETURN END;
    IF isReturn THEN EHOf (s).returns := TRUE
    ELSE EVAL TargetCode (EHOf (s), target)
    END;
  END NoteExit;

(* Before emitting anything: every outcome each Finally scope's end_finally
   must continue, inner FINALLYs first, since continuing an outcome from an
   inner one can add outcomes to an outer one. *)
PROCEDURE PlanExits (p: MSIR.Proc) =
  VAR b: MSIR.Block;  i: MSIR.Insn;  codes := NEW (RefSeq.T).init ();
  BEGIN
    FOR bi := 0 TO MSIR.ProcBlockCount (p) - 1 DO
      b := MSIR.ProcBlock (p, bi);
      FOR ii := 0 TO MSIR.BlockInsnCount (b) - 1 DO
        i := MSIR.BlockInsn (b, ii);
        CASE MSIR.InsnOp (i) OF
        | Op.UnwindTo =>
            NoteExit (MSIR.BlockScope (b), MSIR.InsnBrTarget (i, 0), FALSE);
        | Op.RetThroughEnvelope => NoteExit (MSIR.BlockScope (b), NIL, TRUE);
        | Op.EndFinally => codes.addhi (MSIR.BlockScope (b));
        ELSE
        END;
      END;
    END;
    VAR n := codes.size ();  a := NEW (REF ARRAY OF MSIR.Scope, n);
        t: MSIR.Scope;  f: EHScope;  out: MSIR.Scope;
    BEGIN
      FOR k := 0 TO n - 1 DO a[k] := codes.get (k) END;
      FOR k := 1 TO n - 1 DO
        t := a[k];
        VAR j := k;
        BEGIN
          WHILE j > 0 AND Depth (a[j-1]) < Depth (t) DO a[j] := a[j-1];  DEC (j) END;
          a[j] := t;
        END;
      END;
      FOR k := 0 TO n - 1 DO
        f := EHOf (MSIR.ScopeOwner (a[k]));
        out := MSIR.ScopeParent (f.s);
        FOR j := 0 TO f.targets.size () - 1 DO
          NoteExit (out, f.targets.get (j), FALSE);
        END;
        IF f.returns THEN NoteExit (out, NIL, TRUE) END;
      END;
    END;
  END PlanExits;

(* Set up the scopes of p for GenProc; FALSE if it has none. *)
PROCEDURE SetupScopes (p: MSIR.Proc): BOOLEAN =
  VAR n := MSIR.ProcScopeCount (p);  s: MSIR.Scope;  e: EHScope;
      rt := MSIR.ProcResultType (p);  returns := FALSE;
  BEGIN
    eh := NIL;  retSlot := NIL;  curPad := M3CG.No_label;
    IF n <= 1 THEN RETURN FALSE END;
    eh := NEW (REF ARRAY OF EHScope, n);
    FOR k := 0 TO n - 1 DO
      s := MSIR.ProcScopeAt (p, k);
      e := NEW (EHScope, s := s);
      eh[k] := e;
      IF IsProtected (s) THEN
        e.pad := cg.next_label ();
        e.act := cg.declare_local (M3ID.NoID, Target.Address.bytes,
                                   Target.Address.bytes, Addr, 0, TRUE,
                                   FALSE, M3CG.Never);
        IF exFrame THEN
          e.frame := cg.declare_local (M3ID.NoID, B (M3RT.EF1_SIZE),
                                       Target.Address.bytes,
                                       M3CG.Type.Struct, 0, TRUE, FALSE,
                                       M3CG.Never);
          e.jbuf := cg.declare_local (M3ID.NoID, Target.Address.bytes,
                                      Target.Address.bytes, Addr, 0, TRUE,
                                      FALSE, M3CG.Never);
        END;
      END;
      IF MSIR.ScopeKindOf (s) = MSIR.ScopeKind.Finally THEN
        e.sel := cg.declare_local (M3ID.NoID, Target.Integer.bytes,
                                   Target.Integer.bytes, Int64, 0, TRUE,
                                   FALSE, M3CG.Never);
        e.targets := NEW (RefSeq.T).init ();
      END;
    END;
    PlanExits (p);
    FOR k := 0 TO n - 1 DO returns := returns OR eh[k].returns END;
    IF returns AND rt # NIL AND MSIR.Kind (rt) # TypeKind.Void
       AND NOT IsAgg (rt) THEN
      retSlot := TempFor (rt);
    END;
    placedBlk := NEW (REF ARRAY OF BOOLEAN, MSIR.ProcBlockCount (p));
    FOR k := 0 TO LAST (placedBlk^) DO placedBlk[k] := FALSE END;
    RETURN TRUE;
  END SetupScopes;

(* Emit the blocks of scope s in their original order, each child scope's
   blocks together where its first block falls. *)
PROCEDURE PlaceScope (p: MSIR.Proc;  s: MSIR.Scope) =
  VAR b: MSIR.Block;  bs, c: MSIR.Scope;
  BEGIN
    FOR bi := 0 TO MSIR.ProcBlockCount (p) - 1 DO
      IF NOT placedBlk[bi] THEN
        b := MSIR.ProcBlock (p, bi);
        bs := MSIR.BlockScope (b);
        IF bs = s THEN
          PlaceBlock (p, bi);
        ELSIF MSIR.ScopeIsAncestor (s, bs) THEN
          c := bs;
          WHILE MSIR.ScopeParent (c) # s DO c := MSIR.ScopeParent (c) END;
          IF NOT EHOf (c).placed THEN PlaceChild (p, c) END;
        END;
      END;
    END;
  END PlaceScope;

PROCEDURE PlaceChild (p: MSIR.Proc;  c: MSIR.Scope) =
  VAR e := EHOf (c);  entry: MSIR.Block;
  BEGIN
    e.placed := TRUE;
    IF NOT IsProtected (c) THEN PlaceScope (p, c);  RETURN END;
    (* The try block is entered only by falling into it; branches to the
       entry go to a label before it. *)
    entry := MSIR.ScopeEntry (c);
    cg.set_label (LabelOf (entry), barrier := TRUE);
    IF exFrame THEN
      LinkFrame (e);
    ELSE
      cg.start_try ();
    END;
    PlaceBlock (p, BlockIndex (p, entry), label := FALSE);
    PlaceScope (p, c);
    IF NOT exFrame THEN cg.end_try () END;
    curScope := MSIR.ScopeParent (c);
    curPad := PadOf (curScope);
    cg.set_label (e.pad, barrier := TRUE);
    IF exFrame THEN
      (* reached by longjmp: the activation is the frame's info field *)
      cg.load_address (e.frame, B (M3RT.EF1_info));
      cg.store (e.act, 0, Addr, Addr);
      IF MSIR.ScopeKindOf (c) = MSIR.ScopeKind.Except
        THEN Dispatch (e);
        ELSE SetSel (e, Sel_Exc);
             cg.jump (LabelOf (MSIR.ScopeFinally (e.s)));
      END;
    ELSIF MSIR.ScopeKindOf (c) = MSIR.ScopeKind.Except THEN
      ExceptPad (e);
    ELSE
      FinallyPad (e);
    END;
  END PlaceChild;

PROCEDURE BlockIndex (p: MSIR.Proc;  b: MSIR.Block): INTEGER =
  BEGIN
    FOR bi := 0 TO MSIR.ProcBlockCount (p) - 1 DO
      IF MSIR.ProcBlock (p, bi) = b THEN RETURN bi END;
    END;
    <* ASSERT FALSE *>
  END BlockIndex;

PROCEDURE PlaceBlock (p: MSIR.Proc;  bi: INTEGER;  label := TRUE) =
  VAR b := MSIR.ProcBlock (p, bi);
  BEGIN
    placedBlk[bi] := TRUE;
    IF label AND bi > 0 THEN cg.set_label (LabelOf (b)) END;
    curScope := MSIR.BlockScope (b);
    curPad := PadOf (curScope);
    FOR ii := 0 TO MSIR.BlockInsnCount (b) - 1 DO
      GenInsn (MSIR.BlockInsn (b, ii));
    END;
  END PlaceBlock;

(*----- ex_frame *)

(* The imports the ex_frame scheme calls, as Module.GetAlloca, GetSetjmp
   and GetJmpbufSize declare them for the CG path. *)
PROCEDURE DeclareFrameImports () =
  BEGIN
    IF setjmpProc # NIL THEN RETURN END;
    DeclareAllocaImport ();
    setjmpProc := cg.import_procedure (ID (Target.Setjmp),
                                       1 + ORD (Target.Sigsetjmp), Int64,
                                       Target.DefaultCall);
    EVAL cg.declare_param (ID ("jmpbuf"), Target.Address.bytes,
                           Target.Address.bytes, Addr, 0, FALSE, FALSE,
                           M3CG.Always);
    IF Target.Sigsetjmp THEN
      EVAL cg.declare_param (ID ("save_mask"), 4, 4, M3CG.Type.Int32, 0,
                             FALSE, FALSE, M3CG.Always);
    END;
    jmpbufSize := cg.import_global (ID ("m3_jmpbuf_size"), Target.Word.bytes,
                                    Target.Word.bytes, Word64, 0,
                                    ID ("const_INTEGER"));
  END DeclareFrameImports;

(* The C library's alloca, for ex_frame's jmp_bufs and for alloca.dyn, as
   Module.GetAlloca declares it for the CG path. *)
PROCEDURE DeclareAllocaImport () =
  BEGIN
    IF allocaProc # NIL THEN RETURN END;
    allocaProc := cg.import_procedure (ID ("alloca"), 1, Addr,
                                       Target.DefaultCall);
    EVAL cg.declare_param (M3ID.NoID, Target.Word.bytes, Target.Word.bytes,
                           Word64, 0, FALSE, FALSE, M3CG.Always);
  END DeclareAllocaImport;

(* TRUE if some procedure of m allocates on the stack a byte count known
   only at run time (open array temporaries, D13). *)
PROCEDURE UsesAllocaDyn (m: MSIR.Module): BOOLEAN =
  VAR p: MSIR.Proc;  b: MSIR.Block;
  BEGIN
    FOR pi := 0 TO MSIR.ModuleProcCount (m) - 1 DO
      p := MSIR.ModuleProc (m, pi);
      FOR bi := 0 TO MSIR.ProcBlockCount (p) - 1 DO
        b := MSIR.ProcBlock (p, bi);
        FOR ii := 0 TO MSIR.BlockInsnCount (b) - 1 DO
          IF MSIR.InsnOp (MSIR.BlockInsn (b, ii)) = Op.AllocaDyn THEN
            RETURN TRUE;
          END;
        END;
      END;
    END;
    RETURN FALSE;
  END UsesAllocaDyn;

(* At the procedure's entry: a jmp_buf for each protected scope, as
   Jmpbufs.CompileProcAllocateJmpbufs allocates them. *)
PROCEDURE AllocJmpbufs () =
  BEGIN
    FOR k := 0 TO LAST (eh^) DO
      IF eh[k].frame # NIL THEN
        cg.start_call_direct (allocaProc, 0, Addr);
        cg.load (jmpbufSize, 0, Word64, Word64);
        cg.pop_param (Word64);
        cg.call_direct (allocaProc, Addr);
        cg.store (eh[k].jbuf, 0, Addr, Addr);
      END;
    END;
  END AllocJmpbufs;

(* Entering protected scope e: link its frame and capture the machine state;
   a raise comes back from setjmp to e's pad. *)
PROCEDURE LinkFrame (e: EHScope) =
  VAR class: M3RT.HandlerClass;  hasElse := FALSE;
      q := ProcFor (MSIR.ModulePushEFrame (mod));
  BEGIN
    IF MSIR.ScopeKindOf (e.s) = MSIR.ScopeKind.Finally THEN
      class := M3RT.HandlerClass.Finally;
      cg.load_nil ();
      cg.store (e.frame, B (M3RT.EF1_info) + B (M3RT.EA_exception), Addr, Addr);
    ELSE
      FOR k := 0 TO MSIR.ScopeHandlerCount (e.s) - 1 DO
        IF MSIR.ScopeHandler (e.s, k).exc = NIL THEN hasElse := TRUE END;
      END;
      IF hasElse THEN
        class := M3RT.HandlerClass.ExceptElse;
      ELSE
        class := M3RT.HandlerClass.Except;
        cg.load_address (cseg.var, HandlesOff (e.s));
        cg.store (e.frame, B (M3RT.EF1_handles), Addr, Addr);
      END;
    END;
    PushInt (ORD (class));
    cg.store (e.frame, B (M3RT.EF_class), Int64, Int64);
    cg.start_call_direct (q, 0, M3CG.Type.Void);
    cg.load_address (e.frame, 0);
    cg.pop_param (Addr);
    cg.call_direct (q, M3CG.Type.Void);
    cg.load (e.jbuf, 0, Addr, Addr);
    cg.store (e.frame, B (M3RT.EF1_jmpbuf), Addr, Addr);
    cg.start_call_direct (setjmpProc, 0, Int64);
    cg.load (e.jbuf, 0, Addr, Addr);
    cg.pop_param (Addr);
    IF Target.Sigsetjmp THEN
      PushInt (0, M3CG.Type.Int32);
      cg.pop_param (M3CG.Type.Int32);
    END;
    cg.call_direct (setjmpProc, Int64);
    cg.if_true (Int64, e.pad, M3CG.Never);
  END LinkFrame;

(* Leaving scope 'from' for scope 'stop' (NIL: the procedure): unlink the
   frames of the protected scopes left, all at once, by restoring the
   handler stack to the outermost one's predecessor. *)
PROCEDURE PopFrames (from, stop: MSIR.Scope) =
  VAR x := from;  outer: EHScope := NIL;  q: M3CG.Proc;
  BEGIN
    WHILE x # NIL AND x # stop DO
      IF IsProtected (x) THEN outer := EHOf (x) END;
      x := MSIR.ScopeParent (x);
    END;
    IF outer = NIL THEN RETURN END;
    q := ProcFor (MSIR.ModulePopEFrame (mod));
    cg.start_call_direct (q, 0, M3CG.Type.Void);
    cg.load (outer.frame, B (M3RT.EF_next), Addr, Addr);
    cg.pop_param (Addr);
    cg.call_direct (q, M3CG.Type.Void);
  END PopFrames;

(* At p's entry: the frame that enforces its RAISES clause, as CG's
   Procedure.StartRaises pushes it.  RAISES {} needs only the class; otherwise
   the frame points at the clause's uids.  An exception that reaches the
   frame and is not in the list is reported as blocked by the runtime. *)
PROCEDURE PushRaisesFrame (p: MSIR.Proc) =
  VAR q := ProcFor (MSIR.ModulePushEFrame (mod));
      class := M3RT.HandlerClass.Raises;  size := M3RT.EF3_SIZE;
  BEGIN
    IF MSIR.ProcRaisesCount (p) = 0 THEN
      class := M3RT.HandlerClass.RaisesNone;  size := M3RT.EF_SIZE;
    END;
    raisesFrame := cg.declare_local (M3ID.NoID, B (size), Target.Address.bytes,
                                     M3CG.Type.Struct, 0, TRUE, FALSE,
                                     M3CG.Never);
    IF class = M3RT.HandlerClass.Raises THEN
      cg.load_address (cseg.var, RaisesOff (p));
      cg.store (raisesFrame, B (M3RT.EF3_raises), Addr, Addr);
    END;
    PushInt (ORD (class));
    cg.store (raisesFrame, B (M3RT.EF_class), Int64, Int64);
    cg.start_call_direct (q, 0, M3CG.Type.Void);
    cg.load_address (raisesFrame, 0);
    cg.pop_param (Addr);
    cg.call_direct (q, M3CG.Type.Void);
  END PushRaisesFrame;

(* Before each return: unlink the RAISES frame, and so every frame the
   procedure pushed after it. *)
PROCEDURE PopRaisesFrame () =
  VAR q: M3CG.Proc;
  BEGIN
    IF raisesFrame = NIL THEN RETURN END;
    q := ProcFor (MSIR.ModulePopEFrame (mod));
    cg.start_call_direct (q, 0, M3CG.Type.Void);
    cg.load (raisesFrame, B (M3RT.EF_next), Addr, Addr);
    cg.pop_param (Addr);
    cg.call_direct (q, M3CG.Type.Void);
  END PopRaisesFrame;

(* The landing pad of a Finally scope: save the activation, select Sel_Exc,
   run the FINALLY code. *)
PROCEDURE FinallyPad (e: EHScope) =
  VAR catches := ARRAY [0..0] OF M3CG.TypeUID {0};
  BEGIN
    cg.landing_pad (Addr, e.pad, catches);
    cg.store (e.act, 0, Addr, Addr);
    SetSel (e, Sel_Exc);
    cg.jump (LabelOf (MSIR.ScopeFinally (e.s)));
  END FinallyPad;

(* The landing pad of an Except scope: find the handler by the exception's
   uid, or re-raise. *)
PROCEDURE ExceptPad (e: EHScope) =
  VAR n := MSIR.ScopeHandlerCount (e.s);  h: MSIR.Handler;
      catches := NEW (REF ARRAY OF M3CG.TypeUID, n);  nc := 0;
  BEGIN
    FOR k := 0 TO n - 1 DO
      h := MSIR.ScopeHandler (e.s, k);
      IF h.exc # NIL THEN catches[nc] := MSIR.ExcDescUID (h.exc);  INC (nc) END;
    END;
    cg.landing_pad (Addr, e.pad, SUBARRAY (catches^, 0, nc));
    cg.store (e.act, 0, Addr, Addr);
    Dispatch (e);
  END ExceptPad;

(* Find the handler of Except scope e by the uid of the exception in its
   activation, or re-raise. *)
PROCEDURE Dispatch (e: EHScope) =
  VAR n := MSIR.ScopeHandlerCount (e.s);  h: MSIR.Handler;  skip: M3CG.Label;
  BEGIN
    FOR k := 0 TO n - 1 DO
      h := MSIR.ScopeHandler (e.s, k);
      IF h.exc = NIL THEN ToHandler (e, h);  RETURN END;
      skip := cg.next_label ();
      cg.load (e.act, 0, Addr, Addr);
      cg.load_indirect (B (M3RT.EA_exception), Addr, Addr);
      cg.load_indirect (0, Int64, Int64);
      PushInt (MSIR.ExcDescUID (h.exc));
      cg.if_compare (Int64, M3CG.CompareOp.NE, skip, M3CG.Maybe);
      ToHandler (e, h);
      cg.set_label (skip);
    END;
    Reraise (e);
  END Dispatch;

(* Go to handler h's code, passing the exception's argument, read from the
   activation's argument word (by value or by reference). *)
PROCEDURE ToHandler (e: EHScope;  READONLY h: MSIR.Handler) =
  VAR v: MSIR.Value;  t: MSIR.T;
  BEGIN
    IF MSIR.BlockParamCount (h.entry) = 1 THEN
      v := MSIR.BlockParamValue (h.entry, 0);
      t := MSIR.ValueType (v);
      IF IsAgg (t) THEN
        cg.load_address (Loc0 (v).var, 0);
        PushArgWord (e);
        CopyAgg (t);
      ELSE
        PushArgWord (e);
        IF MSIR.ExcDescArgByRef (h.exc) THEN
          cg.load_indirect (0, MType (t), ZType (t));
        ELSIF NOT IsPtr (t) THEN
          cg.loophole (Addr, ZType (t));
        END;
        IF IsInt (t) THEN Canon (t) END;
        Def (v);
      END;
    END;
    cg.jump (LabelOf (h.entry));
  END ToHandler;

PROCEDURE PushArgWord (e: EHScope) =
  BEGIN
    cg.load (e.act, 0, Addr, Addr);
    cg.load_indirect (B (M3RT.EA_arg), Addr, Addr);
  END PushArgWord;

(* Re-raise the activation of e through the runtime's hook; an invoke if
   the code is itself protected. *)
PROCEDURE Reraise (e: EHScope) =
  VAR q := ProcFor (MSIR.ModuleResumeRaise (mod));
  BEGIN
    cg.start_call_direct (q, 0, M3CG.Type.Void);
    cg.load (e.act, 0, Addr, Addr);
    cg.pop_param (Addr);
    CallDirect (q, M3CG.Type.Void);
    cg.abort (M3CG.RuntimeError.UnhandledException);
  END Reraise;

PROCEDURE SetSel (e: EHScope;  code: INTEGER) =
  BEGIN
    PushInt (code);
    cg.store (e.sel, 0, Int64, Int64);
  END SetSel;

(* Leave scopes from 'from': for a branch to target (with the arguments of
   unwind_to i), or a return of v (NIL: already in retSlot or the result
   area).  The first Finally scope left takes the outcome. *)
PROCEDURE Leave (from: MSIR.Scope;  target: MSIR.Block;  i: MSIR.Insn;
                 isReturn: BOOLEAN;  v: MSIR.Value) =
  VAR s := FirstFinally (from, target, isReturn);  f: EHScope;
  BEGIN
    IF exFrame THEN
      IF s # NIL THEN PopFrames (from, MSIR.ScopeParent (s))
      ELSIF isReturn THEN
        (* the return pops the RAISES frame, and every frame above it *)
        IF raisesFrame = NIL THEN PopFrames (from, NIL) END
      ELSE PopFrames (from, MSIR.BlockScope (target))
      END;
    END;
    IF s = NIL THEN
      IF isReturn THEN
        IF v # NIL THEN GenRet (v) ELSE RetFromSlot () END;
      ELSE
        IF i # NIL THEN PassArgs (i, 0, target) END;
        cg.jump (LabelOf (target));
      END;
      RETURN;
    END;
    f := EHOf (s);
    IF isReturn THEN
      IF v # NIL THEN SaveResult (v) END;
      SetSel (f, Sel_Return);
    ELSE
      <* ASSERT i = NIL OR MSIR.InsnBrArgCount (i, 0) = 0 *>
      SetSel (f, TargetCode (f, target));
    END;
    cg.jump (LabelOf (MSIR.ScopeFinally (s)));
  END Leave;

(* Keep a RETURN's value while FINALLY code runs. *)
PROCEDURE SaveResult (v: MSIR.Value) =
  VAR t := MSIR.ValueType (v);
  BEGIN
    IF IsAgg (t) THEN
      cg.load (resultVar, 0, Addr, Addr);
      Push (v);
      CopyAgg (t);
    ELSIF retSlot # NIL THEN
      Push (v);
      cg.store (retSlot, 0, ZType (t), ZType (t));
    END;
  END SaveResult;

PROCEDURE RetFromSlot () =
  BEGIN
    PopRaisesFrame ();
    IF retSlot = NIL THEN
      cg.exit_proc (M3CG.Type.Void);
    ELSE
      VAR t := MSIR.ProcResultType (curProc);
      BEGIN
        cg.load (retSlot, 0, ZType (t), ZType (t));
        cg.exit_proc (SigType (t));
      END;
    END;
  END RetFromSlot;

(* After FINALLY code: continue the outcome its selector records. *)
PROCEDURE EndFinally (code: MSIR.Scope) =
  VAR f := EHOf (MSIR.ScopeOwner (code));  out := MSIR.ScopeParent (f.s);
      next: M3CG.Label;
  BEGIN
    next := cg.next_label ();
    TestSel (f, Sel_Exc, next);
    Reraise (f);
    cg.set_label (next);
    FOR k := 0 TO f.targets.size () - 1 DO
      next := cg.next_label ();
      TestSel (f, FirstTarget + k, next);
      Leave (out, f.targets.get (k), NIL, FALSE, NIL);
      cg.set_label (next);
    END;
    IF f.returns
      THEN Leave (out, NIL, NIL, TRUE, NIL);
      ELSE cg.abort (M3CG.RuntimeError.UnhandledException);
    END;
  END EndFinally;

(* Go to 'skip' unless e's selector is code. *)
PROCEDURE TestSel (e: EHScope;  code: INTEGER;  skip: M3CG.Label) =
  BEGIN
    cg.load (e.sel, 0, Int64, Int64);
    PushInt (code);
    cg.if_compare (Int64, M3CG.CompareOp.NE, skip, M3CG.Maybe);
  END TestSel;

(* A call: an invoke naming the landing pad if the code is protected. *)
PROCEDURE CallDirect (q: M3CG.Proc;  t: M3CG.Type) =
  BEGIN
    IF curPad = M3CG.No_label THEN
      cg.call_direct (q, t);
    ELSE
      VAR next := cg.next_label ();
      BEGIN
        cg.invoke_direct (q, t, next, curPad);
        cg.set_label (next);
      END;
    END;
  END CallDirect;

PROCEDURE CallIndirect (t: M3CG.Type) =
  BEGIN
    IF curPad = M3CG.No_label THEN
      cg.call_indirect (t, Target.DefaultCall);
    ELSE
      VAR next := cg.next_label ();
      BEGIN
        cg.invoke_indirect (t, Target.DefaultCall, next, curPad);
        cg.set_label (next);
      END;
    END;
  END CallIndirect;

PROCEDURE GenProc (p: MSIR.Proc) =
  VAR sym := CSymbol (p);  r: REFANY;  pv: REF ARRAY OF M3CG.Var;
      q := ProcFor (p);  b: MSIR.Block;  i: MSIR.Insn;
  BEGIN
    vals := NEW (TextRefTbl.Default).init ();
    blocks := NEW (TextRefTbl.Default).init ();
    curProc := p;
    EVAL paramVars.get (sym, r);
    pv := r;
    cg.begin_procedure (q);
    resultVar := pv[MSIR.ProcParamCount (p)];
    FOR k := 0 TO MSIR.ProcParamCount (p) - 1 DO
      VAR v := MSIR.ProcParam (p, k);  t := MSIR.ValueType (v);
      BEGIN
        IF IsAgg (t) THEN
          (* passed by address: the callee copies, as CG's do *)
          VAR l := Loc0 (v);
          BEGIN
            cg.load_address (l.var, 0);
            cg.load (pv[k], 0, Addr, Addr);
            CopyAgg (t);
          END;
        ELSE
          EVAL vals.put (MSIR.ValueName (v),
                         NEW (ValLoc, kind := ValKind.Param, var := pv[k], t := t));
        END;
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
    IF exFrame AND MSIR.ProcChecksRaises (p) THEN PushRaisesFrame (p) END;
    IF SetupScopes (p) THEN
      <* ASSERT MSIR.BlockScope (MSIR.ProcBlock (p, 0)) = MSIR.ProcScope (p) *>
      IF exFrame THEN AllocJmpbufs () END;
      PlaceScope (p, MSIR.ProcScope (p));
    ELSE
      FOR bi := 0 TO MSIR.ProcBlockCount (p) - 1 DO
        b := MSIR.ProcBlock (p, bi);
        IF bi > 0 THEN cg.set_label (LabelOf (b)) END;
        FOR ii := 0 TO MSIR.BlockInsnCount (b) - 1 DO
          GenInsn (MSIR.BlockInsn (b, ii));
        END;
      END;
    END;
    cg.end_procedure (q);
    vals := NIL;
    blocks := NIL;
    eh := NIL;  placedBlk := NIL;  retSlot := NIL;  raisesFrame := NIL;
    curScope := NIL;  curPad := M3CG.No_label;  curProc := NIL;
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

(* Pass the address of res's temp as the hidden result parameter. *)
PROCEDURE PassResult (res: MSIR.Value) =
  BEGIN
    cg.load_address (Loc0 (res).var, 0);
    cg.pop_param (Addr);
  END PassResult;

PROCEDURE LineOf (i: MSIR.Insn) =
  VAR line := MSIR.InsnSrcLine (i);
  BEGIN
    IF line > 0 AND line # curLine THEN
      cg.set_source_line (line);
      curLine := line;
    END;
  END LineOf;

(* m3front's MSIRBuilder calls the C library's memcpy and memset (always
   with fill 0 and a constant size) for aggregate copies and zeroing.
   Declaring them as procedures gives C prototypes that clash with the
   compiler's built-ins (gcc warns), so they become M3CG's copy_n and zero,
   as the CG path would emit; the call's result, the destination, is the
   first operand. *)
TYPE Mem = {None, Copy, Move, Zero};

PROCEDURE MemOp (p: MSIR.Proc): Mem =
  VAR n := MSIR.ProcName (p);
  BEGIN
    IF MSIR.ProcBlockCount (p) > 0 OR n = NIL THEN RETURN Mem.None END;
    IF Text.Equal (n, "memcpy") THEN RETURN Mem.Copy END;
    IF Text.Equal (n, "memmove") THEN RETURN Mem.Move END;
    IF Text.Equal (n, "memset") THEN RETURN Mem.Zero END;
    RETURN Mem.None;
  END MemOp;

PROCEDURE GenMemCall (i: MSIR.Insn;  m: Mem) =
  VAR res := MSIR.InsnResult (i);
  BEGIN
    Push (MSIR.InsnOperand (i, 0));
    IF m = Mem.Zero THEN
      (* a constant count (the census checks): M3C has no zero_n *)
      cg.zero (MSIR.GetIntVal (MSIR.InsnOperand (i, 2)), M3CG.Type.Word8);
    ELSE
      Push (MSIR.InsnOperand (i, 1));
      Push (MSIR.InsnOperand (i, 2));
      cg.copy_n (Int64, M3CG.Type.Word8, m = Mem.Move);
    END;
    IF res # NIL THEN
      Push (MSIR.InsnOperand (i, 0));
      Def (res);
    END;
  END GenMemCall;

PROCEDURE GenRet (v: MSIR.Value) =
  VAR t := MSIR.ValueType (v);
  BEGIN
    PopRaisesFrame ();
    IF IsAgg (t) THEN
      cg.load (resultVar, 0, Addr, Addr);
      Push (v);
      CopyAgg (t);
      cg.exit_proc (M3CG.Type.Void);
    ELSE
      Push (v);
      cg.exit_proc (SigType (t));
    END;
  END GenRet;

PROCEDURE GenInsn (i: MSIR.Insn) =
  VAR op := MSIR.InsnOp (i);  res := MSIR.InsnResult (i);
      nOps := MSIR.InsnOperandCount (i);  rt: MSIR.T;  a: INTEGER;
  BEGIN
    LineOf (i);
    IF res # NIL THEN rt := MSIR.ValueType (res) END;
    CASE op OF
    | Op.Alloca => (* declared in GenProc *)

    | Op.AllocaDyn =>
        (* The C library's alloca: like LLVM's alloca with a run-time count,
           the memory lives until the procedure returns, and the
           conservative stack scan sees any traced references in it. *)
        cg.start_call_direct (allocaProc, 0, Addr);
        Push (MSIR.InsnOperand (i, 0));
        cg.loophole (Int64, Word64);
        cg.pop_param (Word64);
        cg.call_direct (allocaProc, Addr);
        Def (res);

    | Op.Load =>
        IF IsAgg (rt) THEN
          cg.load_address (Loc0 (res).var, 0);
          Push (MSIR.InsnOperand (i, 0));
          CopyAgg (rt);
        ELSE
          Push (MSIR.InsnOperand (i, 0));
          cg.load_indirect (0, MType (rt), ZType (rt));
          Def (res);
        END;
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
          IF NOT IsAgg (vt) THEN
            Push (MSIR.InsnOperand (i, 1));
            Push (val);
            cg.store_indirect (0, ZType (vt), MType (vt));
          ELSIF MSIR.GetValueKind (val) IN SET OF ValueKind {
                  ValueKind.ConstNil, ValueKind.ConstStruct,
                  ValueKind.ConstAggArray } THEN
            StoreConstAgg (val, vt, MSIR.InsnOperand (i, 1));
          ELSE
            Push (MSIR.InsnOperand (i, 1));
            Push (val);
            CopyAgg (vt);
          END;
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
            IF Text.Equal (f.name, fname) THEN off := FieldOffset (t, k);  EXIT END;
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
    | Op.OpenArraySize =>
        Push (MSIR.InsnOperand (i, 0));
        cg.load_indirect (Target.Address.bytes
                            + MSIR.GetIntVal (MSIR.InsnOperand (i, 1))
                              * Target.Integer.bytes, Int64, Int64);
        Def (res);
    | Op.OpenArrayElemAddr =>
        (* data + ((i0*n1 + i1)*n2 + ...) * sizeof(elt), over the row-major
           data the open-array dope describes *)
        VAR oa := MSIR.InsnOperand (i, 0);  t := Base (MSIR.ValueType (oa));
        BEGIN
          Push (oa);
          cg.load_indirect (0, Addr, Addr);
          Push (MSIR.InsnOperand (i, 1));
          FOR k := 1 TO MSIR.OpenArrayRank (t) - 1 DO
            Push (oa);
            cg.load_indirect (Target.Address.bytes + k * Target.Integer.bytes,
                              Int64, Int64);
            cg.multiply (Int64);
            Push (MSIR.InsnOperand (i, 1 + k));
            cg.add (Int64);
          END;
          cg.index_address (Int64, Size (MSIR.OpenArrayElt (t), a));
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
    | Op.ILShr =>
        Push (MSIR.InsnOperand (i, 0));
        Mask (rt);
        Push (MSIR.InsnOperand (i, 1));
        cg.shift_right (Word64);
        Canon (rt);  Def (res);
    | Op.IAShr =>
        (* x < 0: NOT (NOT x >> n); else x >> n.  The value is sign-extended,
           so this is correct for every width. *)
        VAR neg := cg.next_label ();  done := cg.next_label ();
        BEGIN
          Push (MSIR.InsnOperand (i, 0));
          PushInt (0);
          cg.if_compare (Int64, M3CG.CompareOp.LT, neg, M3CG.Maybe);
          Push (MSIR.InsnOperand (i, 0));
          Push (MSIR.InsnOperand (i, 1));
          cg.shift_right (Word64);
          Def (res);
          cg.jump (done);
          cg.set_label (neg);
          Push (MSIR.InsnOperand (i, 0));
          cg.not (Int64);
          Push (MSIR.InsnOperand (i, 1));
          cg.shift_right (Word64);
          cg.not (Int64);
          Def (res);
          cg.set_label (done);
        END;
    | Op.IRotL =>
        (* Word.Rotate's signed count, as the CG path uses: rotate_left's
           count is a word, so a negative constant count is a huge
           unsigned one, which M3C's C rejects.  Rotations are mod 64. *)
        Binop (i);  cg.rotate (Word64);  Def (res);
    | Op.IRotR =>
        Push (MSIR.InsnOperand (i, 0));
        Push (MSIR.InsnOperand (i, 1));
        cg.negate (Int64);
        cg.rotate (Word64);
        Def (res);
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
    | Op.FPFloor, Op.FPCeil, Op.FPRound =>
        (* Float to float, as the CG path does it: through an integer. *)
        Push (MSIR.InsnOperand (i, 0));
        cg.cvt_int (RType (rt), Int64, RoundOp[op]);
        cg.cvt_float (Int64, RType (rt));
        Def (res);

    | Op.ICmp =>
      IF IsWideSet (MSIR.ValueType (MSIR.InsnOperand (i, 0))) THEN
        Binop (i);
        cg.set_compare (SetBytes (MSIR.ValueType (MSIR.InsnOperand (i, 0))),
                        CmpOp (MSIR.InsnCmpPred (i)), Int64);
        Def (res);
      ELSE
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
      END;
    | Op.FCmp =>
        Binop (i);
        cg.compare (RType (MSIR.ValueType (MSIR.InsnOperand (i, 0))), Int64,
                    FCmpOp (MSIR.InsnFCmpPred (i)));
        Def (res);

    (* Set intrinsics (D19) on a multi-word set: the result is the address
       of its temporary, filled by M3CG's set_* ops. *)
    | Op.SetUnion, Op.SetIntersect, Op.SetDifference, Op.SetSymDifference =>
        VAR n := SetBytes (rt);
        BEGIN
          cg.load_address (Loc0 (res).var, 0);
          Binop (i);
          CASE op OF
          | Op.SetUnion         => cg.set_union (n);
          | Op.SetIntersect     => cg.set_intersection (n);
          | Op.SetDifference    => cg.set_difference (n);
          | Op.SetSymDifference => cg.set_sym_difference (n);
          ELSE <* ASSERT FALSE *>
          END;
        END;
    | Op.SetMember =>
        Binop (i);
        cg.set_member (SetBytes (MSIR.ValueType (MSIR.InsnOperand (i, 0))), Int64);
        Def (res);
    | Op.SetSingleton, Op.SetRange =>
        VAR n := SetBytes (rt);  var := Loc0 (res).var;
        BEGIN
          cg.load_address (var, 0);
          cg.zero (n DIV 8, Int64);
          cg.load_address (var, 0);
          Push (MSIR.InsnOperand (i, 0));
          IF op = Op.SetSingleton THEN
            cg.set_singleton (n, Int64);
          ELSE
            Push (MSIR.InsnOperand (i, 1));
            cg.set_range (n, Int64);
          END;
        END;

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
        IF nOps = 0 THEN PopRaisesFrame ();  cg.exit_proc (M3CG.Type.Void);
        ELSE GenRet (MSIR.InsnOperand (i, 0));
        END;
    | Op.Typecase =>
        (* idx := ScanTypecase (ref, table); jump to clause idx, the last
           (ELSE) clause by default. *)
        VAR q := ProcFor (MSIR.ModuleScanTypecase (mod));
            n := MSIR.InsnTypecaseClauseCount (i);
            idx := cg.declare_temp (8, 8, Int64, FALSE);
        BEGIN
          cg.start_call_direct (q, 0, Int64);
          Push (MSIR.InsnOperand (i, 0));
          cg.pop_param (Addr);
          cg.load_address (dseg.var, TypecaseOff (i));
          cg.pop_param (Addr);
          CallDirect (q, Int64);
          cg.store (idx, 0, Int64, Int64);
          FOR k := 0 TO n - 2 DO
            cg.load (idx, 0, Int64, Int64);
            PushInt (k);
            cg.if_compare (Int64, M3CG.CompareOp.EQ,
                           LabelOf (MSIR.InsnTypecaseClause (i, k).block),
                           M3CG.Maybe);
          END;
          cg.free_temp (idx);
          cg.jump (LabelOf (MSIR.InsnTypecaseClause (i, n - 1).block));
        END;
    | Op.UnwindTo =>
        Leave (curScope, MSIR.InsnBrTarget (i, 0), i, FALSE, NIL);
    | Op.RetThroughEnvelope =>
        IF nOps = 0 THEN Leave (curScope, NIL, NIL, TRUE, NIL);
        ELSE Leave (curScope, NIL, NIL, TRUE, MSIR.InsnOperand (i, 0));
        END;
    | Op.EndFinally => EndFinally (curScope);
    | Op.HandlerActivation =>
        cg.load (EHOf (HandlerOwner (curScope)).act, 0, Addr, Addr);
        Def (res);
    | Op.Unreachable =>
        cg.abort (M3CG.RuntimeError.UnhandledException);

    | Op.Call =>
      IF MemOp (MSIR.InsnCallee (i)) # Mem.None THEN
        GenMemCall (i, MemOp (MSIR.InsnCallee (i)));
      ELSE
        VAR callee := MSIR.InsnCallee (i);  q := ProcFor (callee);
            rtype := RetType (MSIR.ProcResultType (callee));
            agg := HasAggResult (callee);
        BEGIN
          cg.start_call_direct (q, 0, rtype);
          IF agg AND ResultFirst () THEN PassResult (res) END;
          FOR k := 0 TO nOps - 1 DO
            Push (MSIR.InsnOperand (i, k));
            cg.pop_param (SigType (MSIR.ValueType (MSIR.ProcParam (callee, k))));
          END;
          IF agg AND NOT ResultFirst () THEN PassResult (res) END;
          CallDirect (q, rtype);
          IF agg THEN
            (* the result is already in place *)
          ELSIF res # NIL THEN
            IF IsInt (rt) THEN Canon (rt) END;
            Def (res);
          ELSIF rtype # M3CG.Type.Void THEN
            cg.pop (rtype);
          END;
        END;
      END;
    | Op.CallIndirect =>
        VAR rt0 := MSIR.InsnTargetType (i);  rtype := RetType (rt0);
            agg := rt0 # NIL AND IsAgg (rt0);
        BEGIN
          cg.start_call_indirect (rtype, Target.DefaultCall);
          IF agg AND ResultFirst () THEN PassResult (res) END;
          FOR k := 1 TO nOps - 1 DO
            VAR v := MSIR.InsnOperand (i, k);
            BEGIN
              Push (v);
              cg.pop_param (SigType (MSIR.ValueType (v)));
            END;
          END;
          IF agg AND NOT ResultFirst () THEN PassResult (res) END;
          Push (MSIR.InsnOperand (i, 0));
          CallIndirect (rtype);
          IF agg THEN
          ELSIF res # NIL THEN
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
            (* bitcast: reinterpret as the integer of the float's size, then
               widen to the stack's Int64, zero-extending if the result is
               wider than the float *)
            IF FloatBits (st) = 32 THEN
              cg.loophole (RType (st), M3CG.Type.Int32);
              cg.loophole (M3CG.Type.Int32, Int64);
              IF IntBits (dt) > 32 THEN cg.extract_mn (Int64, FALSE, 0, 32) END;
            ELSE
              cg.loophole (RType (st), Int64);
            END;
            Canon (dt);
          ELSIF IsFloat (dt) THEN
            (* an integer narrower than the float is zero-extended to it; a
               wider one is truncated (by the Int64 -> Int32 loophole) *)
            IF IntBits (st) < FloatBits (dt) THEN Mask (st) END;
            IF FloatBits (dt) = 32 THEN
              cg.loophole (Int64, M3CG.Type.Int32);
              cg.loophole (M3CG.Type.Int32, RType (dt));
            ELSE
              cg.loophole (Int64, RType (dt));
            END;
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
