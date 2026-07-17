(* See MSIR.i3 *)

MODULE MSIR;

IMPORT RefSeq, Fmt, Text, Target, TFloat;

(*------------------------------------------------------------------- Types *)

REVEAL T = BRANDED "MSIR.T" REF RECORD
  kind:       TypeKind;
  bits:       INTEGER     := 0;        (* scalar widths *)
  elt:        T           := NIL;      (* Ptr / GcRef / GcSlot / OpenArray *)
  structName: TEXT        := NIL;
  fields:     REF ARRAY OF Field := NIL;
  procParams: REF ARRAY OF T      := NIL;
  procResult: T           := NIL;
  objSuper:         T                  := NIL;   (* Object: super; NIL if root *)
  objMethods:       REF ARRAY OF Method := NIL;
  objDescriptorSym: TEXT                := NIL;
  openArrayRank:    INTEGER             := 0;    (* OpenArray *)
  arrayLen:         INTEGER              := 0;    (* FixedArray length *)
  subrLo, subrHi:   INTEGER              := 0;    (* Subrange / Set bounds *)
  typeUID:          INTEGER              := 0;    (* CM3 GlobalUID fingerprint; 0 = unset *)
  labels:           REF ARRAY OF TEXT   := NIL;  (* Enum label names, index = ordinal *)
END;

PROCEDURE NewType(k: TypeKind): T =
  VAR t := NEW(T);
  BEGIN
    t.kind := k;
    RETURN t;
  END NewType;

PROCEDURE TVoid(): T =
  BEGIN RETURN NewType(TypeKind.Void) END TVoid;

PROCEDURE TI1(): T =
  BEGIN RETURN NewType(TypeKind.I1) END TI1;

PROCEDURE TI(bits: INTEGER): T =
  VAR t := NEW(T);
  BEGIN
    CASE bits OF
    | 8  => t.kind := TypeKind.I8;
    | 16 => t.kind := TypeKind.I16;
    | 32 => t.kind := TypeKind.I32;
    | 64 => t.kind := TypeKind.I64;
    ELSE   t.kind := TypeKind.IWide;   (* arbitrary-width integer: i128, i256, etc. *)
    END;
    t.bits := bits;
    RETURN t;
  END TI;

PROCEDURE TW(bits: INTEGER): T =
  VAR t := NEW(T);
  BEGIN
    CASE bits OF
    | 8  => t.kind := TypeKind.W8;
    | 16 => t.kind := TypeKind.W16;
    | 32 => t.kind := TypeKind.W32;
    | 64 => t.kind := TypeKind.W64;
    ELSE  <* ASSERT FALSE, "TW: invalid bit width" *>
    END;
    t.bits := bits;
    RETURN t;
  END TW;

PROCEDURE TF(bits: INTEGER): T =
  VAR t := NEW(T);
  BEGIN
    CASE bits OF
    | 32  => t.kind := TypeKind.F32;
    | 64  => t.kind := TypeKind.F64;
    | 128 => t.kind := TypeKind.F128;
    ELSE   <* ASSERT FALSE, "TF: invalid bit width" *>
    END;
    t.bits := bits;
    RETURN t;
  END TF;

PROCEDURE TPtr(elt: T): T =
  VAR t := NewType(TypeKind.Ptr);
  BEGIN t.elt := elt; RETURN t END TPtr;

PROCEDURE TGcRef(elt: T): T =
  VAR t := NewType(TypeKind.GcRef);
  BEGIN t.elt := elt; RETURN t END TGcRef;

PROCEDURE TGcSlot(elt: T): T =
  VAR t := NewType(TypeKind.GcSlot);
  BEGIN t.elt := elt; RETURN t END TGcSlot;

PROCEDURE TStruct(name: TEXT;  READONLY fields: ARRAY OF Field): T =
  VAR
    t := NewType(TypeKind.Struct);
    fs := NEW(REF ARRAY OF Field, NUMBER(fields));
  BEGIN
    t.structName := name;
    FOR i := FIRST(fields) TO LAST(fields) DO fs[i] := fields[i] END;
    t.fields := fs;
    RETURN t;
  END TStruct;

PROCEDURE TProc(READONLY params: ARRAY OF T;  result: T): T =
  VAR
    t := NewType(TypeKind.ProcType);
    ps := NEW(REF ARRAY OF T, NUMBER(params));
  BEGIN
    FOR i := FIRST(params) TO LAST(params) DO ps[i] := params[i] END;
    t.procParams := ps;
    t.procResult := result;
    RETURN t;
  END TProc;

PROCEDURE TObject(name: TEXT;
                  super: T;
                  READONLY fields:  ARRAY OF Field;
                  READONLY methods: ARRAY OF Method;
                  descriptorSym: TEXT): T =
  VAR
    t  := NewType(TypeKind.Object);
    fs := NEW(REF ARRAY OF Field,  NUMBER(fields));
    ms := NEW(REF ARRAY OF Method, NUMBER(methods));
  BEGIN
    t.structName       := name;
    t.objSuper         := super;
    t.objDescriptorSym := descriptorSym;
    FOR i := FIRST(fields)  TO LAST(fields)  DO fs[i] := fields[i]  END;
    FOR i := FIRST(methods) TO LAST(methods) DO ms[i] := methods[i] END;
    t.fields     := fs;
    t.objMethods := ms;
    RETURN t;
  END TObject;

PROCEDURE TOpenArray(rank: INTEGER;  elt: T): T =
  VAR t := NewType(TypeKind.OpenArray);
  BEGIN
    <* ASSERT rank >= 1, "TOpenArray: rank must be >= 1" *>
    t.openArrayRank := rank;
    t.elt           := elt;
    RETURN t;
  END TOpenArray;

PROCEDURE THeapArray(rank: INTEGER;  elt: T): T =
  VAR t := NewType(TypeKind.HeapArray);
  BEGIN
    <* ASSERT rank >= 1, "THeapArray: rank must be >= 1" *>
    t.openArrayRank := rank;
    t.elt           := elt;
    RETURN t;
  END THeapArray;

PROCEDURE TFixedArray(len: INTEGER;  elt: T): T =
  VAR t := NewType(TypeKind.FixedArray);
  BEGIN
    <* ASSERT len >= 0, "TFixedArray: length must be >= 0" *>
    t.arrayLen := len;
    t.elt      := elt;
    RETURN t;
  END TFixedArray;

PROCEDURE TSubrange(parent: T;  lo, hi: INTEGER): T =
  VAR t := NewType(TypeKind.Subrange);
  BEGIN
    t.elt := parent;
    t.subrLo := lo;
    t.subrHi := hi;
    RETURN t;
  END TSubrange;

PROCEDURE TLandingPad(): T =
  VAR t := NewType(TypeKind.Struct);
      fs := NEW(REF ARRAY OF Field, 2);
  BEGIN
    fs[0].name := "excobj"; fs[0].type := TPtr(TVoid()); fs[0].offset := 0;
    fs[1].name := "sel";    fs[1].type := TI(32);        fs[1].offset := 0;
    t.structName := "lpad_t";
    t.fields     := fs;
    RETURN t;
  END TLandingPad;

PROCEDURE TSet(elt: T;  lo, hi: INTEGER): T =
  VAR t := NewType(TypeKind.Set);
  BEGIN
    t.elt := elt;
    t.subrLo := lo;
    t.subrHi := hi;
    RETURN t;
  END TSet;

PROCEDURE Kind(t: T): TypeKind = BEGIN RETURN t.kind END Kind;

PROCEDURE Equal(a, b: T): BOOLEAN =
  BEGIN
    IF a = b THEN RETURN TRUE END;
    IF a = NIL OR b = NIL THEN RETURN FALSE END;
    IF a.kind # b.kind THEN
      (* I-kind and W-kind of the same bit-width lower to the same LLVM iN type.
         Treat them as equal so that mixed signed/unsigned usage of the same
         storage width (e.g. CHAR=W8 value returned from a proc declared before
         CHAR→TW8, or passed to a formal typed with TI8) does not cause spurious
         type mismatches.  The signedness is only relevant for SExt vs ZExt in
         CoerceToMSIR; at the LLVM IR level iN is iN regardless of sign. *)
      IF BitWidth(a) > 0 AND BitWidth(a) = BitWidth(b) AND
         ((a.kind >= TypeKind.I1  AND a.kind <= TypeKind.I64 AND
           b.kind >= TypeKind.W8  AND b.kind <= TypeKind.W64) OR
          (a.kind >= TypeKind.W8  AND a.kind <= TypeKind.W64 AND
           b.kind >= TypeKind.I1  AND b.kind <= TypeKind.I64)) THEN
        RETURN TRUE;
      END;
      RETURN FALSE;
    END;
    CASE a.kind OF
    | TypeKind.Ptr, TypeKind.GcRef, TypeKind.GcSlot =>
        RETURN Equal(a.elt, b.elt);
    | TypeKind.Struct =>
        (* Always compare structurally: struct names are cosmetic.
           BITS X FOR T produces a different name than T but identical layout. *)
        IF a.fields = NIL AND b.fields = NIL THEN RETURN TRUE END;
        IF a.fields = NIL OR b.fields = NIL THEN RETURN FALSE END;
        IF NUMBER(a.fields^) # NUMBER(b.fields^) THEN RETURN FALSE END;
        FOR i := 0 TO LAST(a.fields^) DO
          IF NOT Text.Equal(a.fields[i].name, b.fields[i].name) THEN
            RETURN FALSE
          END;
          IF NOT Equal(a.fields[i].type, b.fields[i].type) THEN
            RETURN FALSE
          END;
        END;
        RETURN TRUE;
    | TypeKind.Object =>
        RETURN Text.Equal(a.structName, b.structName);
    | TypeKind.OpenArray, TypeKind.HeapArray =>
        IF a.openArrayRank # b.openArrayRank THEN RETURN FALSE END;
        RETURN Equal(a.elt, b.elt);
    | TypeKind.FixedArray =>
        IF a.arrayLen # b.arrayLen THEN RETURN FALSE END;
        RETURN Equal(a.elt, b.elt);
    | TypeKind.Subrange, TypeKind.Set =>
        IF a.subrLo # b.subrLo OR a.subrHi # b.subrHi THEN RETURN FALSE END;
        RETURN Equal(a.elt, b.elt);
    | TypeKind.ProcType =>
        IF NUMBER(a.procParams^) # NUMBER(b.procParams^) THEN
          RETURN FALSE
        END;
        FOR i := 0 TO LAST(a.procParams^) DO
          IF NOT Equal(a.procParams[i], b.procParams[i]) THEN
            RETURN FALSE
          END;
        END;
        RETURN Equal(a.procResult, b.procResult);
    ELSE
        RETURN a.bits = b.bits;
    END;
  END Equal;

PROCEDURE EltType(t: T): T = BEGIN RETURN t.elt END EltType;
PROCEDURE StructName(t: T): TEXT = BEGIN RETURN t.structName END StructName;
PROCEDURE StructFieldCount(t: T): INTEGER =
  BEGIN
    IF t.fields = NIL THEN RETURN 0 END;
    RETURN NUMBER(t.fields^);
  END StructFieldCount;
PROCEDURE StructField(t: T; i: INTEGER): Field =
  BEGIN RETURN t.fields[i] END StructField;
PROCEDURE BitWidth(t: T): INTEGER =
  BEGIN
    CASE t.kind OF
    | TypeKind.I1 => RETURN 1;
    | TypeKind.I8, TypeKind.I16, TypeKind.I32, TypeKind.I64,
      TypeKind.IWide,
      TypeKind.W8, TypeKind.W16, TypeKind.W32, TypeKind.W64,
      TypeKind.F32, TypeKind.F64, TypeKind.F128,
      TypeKind.Enum => RETURN t.bits;
    ELSE RETURN -1
    END;
  END BitWidth;

PROCEDURE TEnum(name: TEXT;  bits: INTEGER;  READONLY labels: ARRAY OF TEXT): T =
  VAR t  := NewType(TypeKind.Enum);
      ls := NEW(REF ARRAY OF TEXT, NUMBER(labels));
  BEGIN
    t.structName := name;
    t.bits       := bits;
    FOR i := FIRST(labels) TO LAST(labels) DO ls[i] := labels[i] END;
    t.labels := ls;
    RETURN t;
  END TEnum;

PROCEDURE EnumLabelCount(t: T): INTEGER =
  BEGIN
    IF t.labels = NIL THEN RETURN 0 END;
    RETURN NUMBER(t.labels^);
  END EnumLabelCount;

PROCEDURE EnumLabel(t: T;  i: INTEGER): TEXT =
  BEGIN RETURN t.labels[i] END EnumLabel;

PROCEDURE SetTypeUID(t: T; uid: INTEGER) = BEGIN t.typeUID := uid END SetTypeUID;
PROCEDURE TypeUID(t: T): INTEGER = BEGIN RETURN t.typeUID END TypeUID;

PROCEDURE ObjectName(t: T): TEXT = BEGIN RETURN t.structName END ObjectName;
PROCEDURE ObjectSuper(t: T): T = BEGIN RETURN t.objSuper END ObjectSuper;
PROCEDURE ObjectFieldCount(t: T): INTEGER =
  BEGIN
    IF t.fields = NIL THEN RETURN 0 END;
    RETURN NUMBER(t.fields^);
  END ObjectFieldCount;
PROCEDURE ObjectField(t: T;  i: INTEGER): Field =
  BEGIN RETURN t.fields[i] END ObjectField;
PROCEDURE ObjectMethodCount(t: T): INTEGER =
  BEGIN
    IF t.objMethods = NIL THEN RETURN 0 END;
    RETURN NUMBER(t.objMethods^);
  END ObjectMethodCount;
PROCEDURE ObjectMethod(t: T;  i: INTEGER): Method =
  BEGIN RETURN t.objMethods[i] END ObjectMethod;
PROCEDURE ObjectDescriptorSym(t: T): TEXT =
  BEGIN RETURN t.objDescriptorSym END ObjectDescriptorSym;

PROCEDURE OpenArrayRank(t: T): INTEGER = BEGIN RETURN t.openArrayRank END OpenArrayRank;
PROCEDURE OpenArrayElt(t: T): T        = BEGIN RETURN t.elt           END OpenArrayElt;
PROCEDURE HeapArrayRank(t: T): INTEGER = BEGIN RETURN t.openArrayRank END HeapArrayRank;
PROCEDURE HeapArrayElt(t: T): T        = BEGIN RETURN t.elt           END HeapArrayElt;
PROCEDURE FixedArrayLen(t: T): INTEGER = BEGIN RETURN t.arrayLen      END FixedArrayLen;
PROCEDURE FixedArrayElt(t: T): T       = BEGIN RETURN t.elt           END FixedArrayElt;
PROCEDURE FixedArrayLo(t: T): INTEGER  = BEGIN RETURN t.subrLo        END FixedArrayLo;
PROCEDURE SetFixedArrayLo(t: T; lo: INTEGER) = BEGIN t.subrLo := lo   END SetFixedArrayLo;

PROCEDURE SubrangeParent(t: T): T   = BEGIN RETURN t.elt    END SubrangeParent;
PROCEDURE SubrangeLo(t: T): INTEGER = BEGIN RETURN t.subrLo END SubrangeLo;
PROCEDURE SubrangeHi(t: T): INTEGER = BEGIN RETURN t.subrHi END SubrangeHi;
PROCEDURE SetElt(t: T): T           = BEGIN RETURN t.elt    END SetElt;
PROCEDURE SetLo(t: T): INTEGER      = BEGIN RETURN t.subrLo END SetLo;
PROCEDURE SetHi(t: T): INTEGER      = BEGIN RETURN t.subrHi END SetHi;

(* Look up a method by selector, walking the super chain. *)
PROCEDURE ObjectMethodIndex(t: T;  selector: TEXT): INTEGER =
  VAR cur := t;  base := 0;
  BEGIN
    (* Walk to root to compute total slot offset, then look in this type's
       own methods first. For v0 we use simple unification: each type's
       methods are appended; selector resolution scans in declaration order
       through the chain root→leaf, returning the first match. *)
    (* Simpler: just scan local methods, then super. Caller can rewalk. *)
    WHILE cur # NIL DO
      IF cur.objMethods # NIL THEN
        FOR i := 0 TO LAST(cur.objMethods^) DO
          IF Text.Equal(cur.objMethods[i].name, selector) THEN
            RETURN base + i;
          END;
        END;
        INC(base, NUMBER(cur.objMethods^));
      END;
      cur := cur.objSuper;
    END;
    RETURN -1;
  END ObjectMethodIndex;

PROCEDURE ObjectFieldIndex(t: T;  name: TEXT): INTEGER =
  VAR cur := t;  base := 0;
  BEGIN
    WHILE cur # NIL DO
      IF cur.fields # NIL THEN
        FOR i := 0 TO LAST(cur.fields^) DO
          IF Text.Equal(cur.fields[i].name, name) THEN
            RETURN base + i;
          END;
        END;
        INC(base, NUMBER(cur.fields^));
      END;
      cur := cur.objSuper;
    END;
    RETURN -1;
  END ObjectFieldIndex;

(*----------------------------------------------------------------- Values *)

REVEAL Value = BRANDED "MSIR.Value" REF RECORD
  type:        T          := NIL;
  name:        TEXT       := NIL;
  vKind:       ValueKind;
  intVal:      INTEGER    := 0;
  floatVal:    Target.Float;           (* ConstFloat *)
  proc:        Proc       := NIL;     (* Param, ConstProc *)
  paramIdx:    INTEGER    := -1;
  block:       Block      := NIL;     (* BlockParam *)
  bparamIdx:   INTEGER    := -1;
  insn:        Insn       := NIL;     (* InsnResult *)
  textUid:     INTEGER    := -1;      (* ConstTextLit *)
  textChars:   TEXT       := NIL;     (* ConstTextLit *)
  textCnt:     INTEGER    := 0;       (* ConstTextLit *)
  structOff:   INTEGER    := -1;      (* StructFieldRef: byte offset in @Mod_M3_info *)
  constFields: REF ARRAY OF Value := NIL;  (* ConstStruct *)
END;

PROCEDURE ConstInt(t: T;  v: INTEGER): Value =
  VAR
    val := NEW(Value);
  BEGIN
    val.type   := t;
    val.vKind  := ValueKind.ConstInt;
    val.intVal := v;
    val.name   := Fmt.Int(v);
    RETURN val;
  END ConstInt;

PROCEDURE ConstBool(v: BOOLEAN): Value =
  BEGIN
    IF v THEN RETURN ConstInt(TI1(), 1) ELSE RETURN ConstInt(TI1(), 0) END;
  END ConstBool;

PROCEDURE ConstNil(t: T): Value =
  VAR val := NEW(Value);
  BEGIN
    val.type  := t;
    val.vKind := ValueKind.ConstNil;
    val.name  := "nil";
    RETURN val;
  END ConstNil;

PROCEDURE ConstFloat(t: T;  READONLY v: Target.Float): Value =
  VAR val := NEW(Value);
      buf : ARRAY [0..63] OF CHAR;
      len : INTEGER;
  BEGIN
    val.type     := t;
    val.vKind    := ValueKind.ConstFloat;
    val.floatVal := v;
    len := TFloat.ToChars(v, buf);
    val.name := Text.FromChars(SUBARRAY(buf, 0, len));
    RETURN val;
  END ConstFloat;

PROCEDURE GetFloatVal(v: Value;  VAR f: Target.Float) =
  BEGIN
    f := v.floatVal;
  END GetFloatVal;

PROCEDURE ConstProcRef(p: Proc): Value =
  VAR val := NEW(Value);
  BEGIN
    val.type  := TPtr(TVoid());
    val.vKind := ValueKind.ConstProc;
    val.proc  := p;
    val.name  := "@" & ProcName(p);
    RETURN val;
  END ConstProcRef;

PROCEDURE GetConstProc(v: Value): Proc =
  BEGIN
    RETURN v.proc;
  END GetConstProc;

PROCEDURE ConstZero(t: T): Value =
  BEGIN
    CASE Kind(t) OF
    | TypeKind.I1                        => RETURN ConstBool(FALSE);
    | TypeKind.I8, TypeKind.I16,
      TypeKind.I32, TypeKind.I64,
      TypeKind.IWide,
      TypeKind.W8, TypeKind.W16,
      TypeKind.W32, TypeKind.W64        => RETURN ConstInt(t, 0);
    | TypeKind.Enum                      => RETURN ConstInt(t, 0);
    | TypeKind.Ptr, TypeKind.GcRef,
      TypeKind.GcSlot                   => RETURN ConstNil(t);
    | TypeKind.F32                       => RETURN ConstFloat(t, TFloat.ZeroR);
    | TypeKind.F64                       => RETURN ConstFloat(t, TFloat.ZeroL);
    | TypeKind.FixedArray,
      TypeKind.Struct                    => RETURN ConstNil(t);
      (* Emitter outputs "zeroinitializer" for ConstNil with aggregate types. *)
    ELSE                                   RETURN NIL;
    END;
  END ConstZero;

PROCEDURE RetypeValue(v: Value;  t: T): Value =
  VAR w := NEW(Value);
  BEGIN
    w.type        := t;
    w.name        := v.name;
    w.vKind       := v.vKind;
    w.intVal      := v.intVal;
    (* floatVal intentionally omitted: RetypeValue is never called on ConstFloat values *)
    w.proc        := v.proc;
    w.paramIdx    := v.paramIdx;
    w.block       := v.block;
    w.bparamIdx   := v.bparamIdx;
    w.insn        := v.insn;
    w.textUid     := v.textUid;
    w.textChars   := v.textChars;
    w.textCnt     := v.textCnt;
    w.structOff   := v.structOff;
    w.constFields := v.constFields;
    RETURN w;
  END RetypeValue;

PROCEDURE StructFieldRef(infoName: TEXT;  byteOffset: INTEGER;  t: T): Value =
  VAR v := NEW(Value);
  BEGIN
    v.type      := t;
    v.vKind     := ValueKind.StructFieldRef;
    v.name      := "@" & infoName;   (* base global name *)
    v.structOff := byteOffset;
    RETURN v;
  END StructFieldRef;

PROCEDURE GetStructFieldOffset(v: Value): INTEGER =
  BEGIN RETURN v.structOff END GetStructFieldOffset;

PROCEDURE ConstTextLit(uid: INTEGER;  chars: TEXT;  cnt: INTEGER): Value =
  VAR val := NEW(Value);
  BEGIN
    val.type      := TGcRef(TVoid());  (* TEXT = traced ref *)
    val.vKind     := ValueKind.ConstTextLit;
    val.textUid   := uid;
    val.textChars := chars;
    val.textCnt   := cnt;
    val.name      := "@textlit_" & Fmt.Int(uid);  (* LLVM global name for emit *)
    RETURN val;
  END ConstTextLit;

PROCEDURE GetTextLitUID  (v: Value): INTEGER = BEGIN RETURN v.textUid   END GetTextLitUID;
PROCEDURE GetTextLitChars(v: Value): TEXT    = BEGIN RETURN v.textChars  END GetTextLitChars;
PROCEDURE GetTextLitCnt  (v: Value): INTEGER = BEGIN RETURN v.textCnt   END GetTextLitCnt;

PROCEDURE ConstStruct(t: T; READONLY fields: ARRAY OF Value): Value =
  VAR val := NEW(Value);
      n   := NUMBER(fields);
  BEGIN
    val.type        := t;
    val.vKind       := ValueKind.ConstStruct;
    val.name        := "<const-struct>";
    val.constFields := NEW(REF ARRAY OF Value, n);
    FOR i := 0 TO n - 1 DO val.constFields[i] := fields[i] END;
    RETURN val;
  END ConstStruct;

PROCEDURE ConstAggArray(t: T; READONLY elts: ARRAY OF Value): Value =
  VAR val := NEW(Value);
      n   := NUMBER(elts);
  BEGIN
    val.type        := t;
    val.vKind       := ValueKind.ConstAggArray;
    val.name        := "<const-array>";
    val.constFields := NEW(REF ARRAY OF Value, n);
    FOR i := 0 TO n - 1 DO val.constFields[i] := elts[i] END;
    RETURN val;
  END ConstAggArray;

PROCEDURE GetConstStructField(v: Value; i: INTEGER): Value =
  BEGIN RETURN v.constFields[i] END GetConstStructField;

PROCEDURE GetConstStructFieldCount(v: Value): INTEGER =
  BEGIN
    IF v.constFields = NIL THEN RETURN 0 END;
    RETURN NUMBER(v.constFields^);
  END GetConstStructFieldCount;

PROCEDURE ValueType(v: Value): T = BEGIN RETURN v.type END ValueType;
PROCEDURE ValueName(v: Value): TEXT = BEGIN RETURN v.name END ValueName;

PROCEDURE GetValueKind(v: Value): ValueKind = BEGIN RETURN v.vKind END GetValueKind;
PROCEDURE GetIntVal(v: Value): INTEGER = BEGIN RETURN v.intVal END GetIntVal;

(*------------------------------------------------------------------- Insn *)

REVEAL Insn = BRANDED "MSIR.Insn" REF RECORD
  op:        Op;
  block:     Block := NIL;
  result:    Value := NIL;
  operands:  REF ARRAY OF Value := NIL;

  (* opcode-specific extras *)
  cmpPred:   CmpPred;
  fcmpPred:  FCmpPred;
  br0Tgt:    Block := NIL;
  br0Args:   REF ARRAY OF Value := NIL;
  br1Tgt:    Block := NIL;
  br1Args:   REF ARRAY OF Value := NIL;
  callee:    Proc  := NIL;
  targetType: T    := NIL;        (* Alloca, New, Narrow, Istype, Convert *)
  selector:   TEXT := NIL;        (* Dispatch method name; FieldAddr field name *)
  typecaseClauses: REF ARRAY OF TypecaseClause := NIL;
  extractIdx: INTEGER := 0;       (* ExtractValue: field index *)
  isCleanup:  BOOLEAN := FALSE;   (* LandingPad: cleanup vs. catch *)
  memOrder:  MemOrder    := MemOrder.SeqCst;   (* atomic ops *)
  memOrder2: MemOrder    := MemOrder.SeqCst;   (* AtomicCmpXchg failure ordering *)
  atomicOp:  AtomicRMWOp := AtomicRMWOp.Xchg; (* AtomicRMW op code *)
  srcLine:   INTEGER     := 0;                 (* source line when emitted; 0 = unknown *)
END;

PROCEDURE InsnOp(i: Insn): Op = BEGIN RETURN i.op END InsnOp;
PROCEDURE InsnResult(i: Insn): Value = BEGIN RETURN i.result END InsnResult;
PROCEDURE InsnOperandCount(i: Insn): INTEGER =
  BEGIN
    IF i.operands = NIL THEN RETURN 0 END;
    RETURN NUMBER(i.operands^);
  END InsnOperandCount;
PROCEDURE InsnOperand(i: Insn; k: INTEGER): Value =
  BEGIN RETURN i.operands[k] END InsnOperand;
PROCEDURE InsnCmpPred(i: Insn): CmpPred =
  BEGIN RETURN i.cmpPred END InsnCmpPred;
PROCEDURE InsnFCmpPred(i: Insn): FCmpPred =
  BEGIN RETURN i.fcmpPred END InsnFCmpPred;
PROCEDURE InsnBrTarget(i: Insn; k: INTEGER): Block =
  BEGIN
    IF k = 0 THEN RETURN i.br0Tgt ELSE RETURN i.br1Tgt END;
  END InsnBrTarget;
PROCEDURE InsnBrArgCount(i: Insn; k: INTEGER): INTEGER =
  VAR a: REF ARRAY OF Value;
  BEGIN
    IF k = 0 THEN a := i.br0Args ELSE a := i.br1Args END;
    IF a = NIL THEN RETURN 0 ELSE RETURN NUMBER(a^) END;
  END InsnBrArgCount;
PROCEDURE InsnBrArg(i: Insn; k, j: INTEGER): Value =
  BEGIN
    IF k = 0 THEN RETURN i.br0Args[j] ELSE RETURN i.br1Args[j] END;
  END InsnBrArg;
PROCEDURE InsnCallee(i: Insn): Proc = BEGIN RETURN i.callee END InsnCallee;
PROCEDURE InsnTargetType(i: Insn): T = BEGIN RETURN i.targetType END InsnTargetType;
PROCEDURE InsnSelector(i: Insn): TEXT = BEGIN RETURN i.selector END InsnSelector;
PROCEDURE InsnExtractIdx(i: Insn): INTEGER = BEGIN RETURN i.extractIdx END InsnExtractIdx;
PROCEDURE InsnIsCleanup(i: Insn): BOOLEAN = BEGIN RETURN i.isCleanup END InsnIsCleanup;
PROCEDURE InsnMemOrder(i: Insn): MemOrder = BEGIN RETURN i.memOrder END InsnMemOrder;
PROCEDURE InsnMemOrder2(i: Insn): MemOrder = BEGIN RETURN i.memOrder2 END InsnMemOrder2;
PROCEDURE InsnAtomicOp(i: Insn): AtomicRMWOp = BEGIN RETURN i.atomicOp END InsnAtomicOp;
PROCEDURE SetCurrentSrcLine(line: INTEGER) =
  BEGIN currentSrcLine := line END SetCurrentSrcLine;
PROCEDURE InsnSrcLine(i: Insn): INTEGER =
  BEGIN RETURN i.srcLine END InsnSrcLine;
PROCEDURE InsnTypecaseClauseCount(i: Insn): INTEGER =
  BEGIN
    IF i.typecaseClauses = NIL THEN RETURN 0 END;
    RETURN NUMBER(i.typecaseClauses^);
  END InsnTypecaseClauseCount;
PROCEDURE InsnTypecaseClause(i: Insn;  k: INTEGER): TypecaseClause =
  BEGIN RETURN i.typecaseClauses[k] END InsnTypecaseClause;

(*------------------------------------------------------------------ Block *)

REVEAL Block = BRANDED "MSIR.Block" REF RECORD
  label:       TEXT;
  params:      REF ARRAY OF BlockParam := NIL;
  paramValues: REF ARRAY OF Value      := NIL;
  insns:       RefSeq.T;                          (* elements: Insn *)
  proc:        Proc := NIL;
END;

PROCEDURE pctName(name: TEXT): TEXT =
  BEGIN
    IF Text.Length(name) > 0 AND Text.GetChar(name, 0) = '%' THEN
      RETURN name;
    END;
    RETURN "%" & name;
  END pctName;

PROCEDURE NewBlock(label: TEXT;
                   READONLY params: ARRAY OF BlockParam): Block =
  VAR
    b  := NEW(Block);
    n  := NUMBER(params);
    ps := NEW(REF ARRAY OF BlockParam, n);
    vs := NEW(REF ARRAY OF Value,       n);
  BEGIN
    b.label := label;
    b.insns := NEW(RefSeq.T).init();
    FOR i := 0 TO n - 1 DO
      ps[i] := params[i];
      ps[i].name := pctName(params[i].name);
      VAR v := NEW(Value);
      BEGIN
        v.type      := params[i].type;
        v.name      := ps[i].name;
        v.vKind     := ValueKind.BlockParam;
        v.block     := b;
        v.bparamIdx := i;
        vs[i] := v;
      END;
    END;
    b.params      := ps;
    b.paramValues := vs;
    RETURN b;
  END NewBlock;

PROCEDURE BlockLabel(b: Block): TEXT = BEGIN RETURN b.label END BlockLabel;
PROCEDURE BlockParamCount(b: Block): INTEGER =
  BEGIN
    IF b.params = NIL THEN RETURN 0 END;
    RETURN NUMBER(b.params^);
  END BlockParamCount;
PROCEDURE BlockParamValue(b: Block; i: INTEGER): Value =
  BEGIN RETURN b.paramValues[i] END BlockParamValue;
PROCEDURE BlockInsnCount(b: Block): INTEGER =
  BEGIN RETURN b.insns.size() END BlockInsnCount;
PROCEDURE BlockInsn(b: Block; i: INTEGER): Insn =
  BEGIN RETURN b.insns.get(i) END BlockInsn;

(*-------------------------------------------------------------- Procedure *)

REVEAL Proc = BRANDED "MSIR.Proc" REF RECORD
  name:        TEXT;
  params:      REF ARRAY OF Param := NIL;
  paramValues: REF ARRAY OF Value := NIL;
  result:      T := NIL;
  items:       RefSeq.T;                          (* elements: Block or Envelope *)
  nameCounter: INTEGER := 0;
  raises:      REF ARRAY OF TEXT := NIL;
  noReturn:    BOOLEAN := FALSE;
  linkage:     Linkage := Linkage.External;
  cc:          CallingConvention := CallingConvention.M3;
  srcFile:     TEXT    := NIL;   (* source filename; NIL = no location *)
  srcLine:     INTEGER := 0;     (* 1-based line number; 0 = no location *)
END;

(*--------------------------------------------------------------- Envelope *)

REVEAL Envelope = BRANDED "MSIR.Envelope" REF RECORD
  isFinally: BOOLEAN := FALSE;
  body:      Block := NIL;
  handlers:  REF ARRAY OF Handler := NIL;          (* try-except *)
  finally:   Block := NIL;                         (* try-finally *)
END;

PROCEDURE NewTryExcept(body: Block;
                       READONLY handlers: ARRAY OF Handler): Envelope =
  VAR
    e := NEW(Envelope);
    n := NUMBER(handlers);
    hs := NEW(REF ARRAY OF Handler, n);
  BEGIN
    e.isFinally := FALSE;
    e.body      := body;
    FOR i := 0 TO n - 1 DO hs[i] := handlers[i] END;
    e.handlers := hs;
    RETURN e;
  END NewTryExcept;

PROCEDURE NewTryFinally(body: Block;  finallyBlock: Block): Envelope =
  VAR e := NEW(Envelope);
  BEGIN
    e.isFinally := TRUE;
    e.body      := body;
    e.finally   := finallyBlock;
    RETURN e;
  END NewTryFinally;

PROCEDURE IsTryExcept(e: Envelope): BOOLEAN =
  BEGIN RETURN NOT e.isFinally END IsTryExcept;
PROCEDURE IsTryFinally(e: Envelope): BOOLEAN =
  BEGIN RETURN e.isFinally END IsTryFinally;
PROCEDURE EnvelopeBody(e: Envelope): Block = BEGIN RETURN e.body END EnvelopeBody;
PROCEDURE EnvelopeHandlerCount(e: Envelope): INTEGER =
  BEGIN
    IF e.handlers = NIL THEN RETURN 0 END;
    RETURN NUMBER(e.handlers^);
  END EnvelopeHandlerCount;
PROCEDURE EnvelopeHandler(e: Envelope;  i: INTEGER): Handler =
  BEGIN RETURN e.handlers[i] END EnvelopeHandler;
PROCEDURE EnvelopeFinally(e: Envelope): Block =
  BEGIN RETURN e.finally END EnvelopeFinally;

PROCEDURE NewProc(name: TEXT;
                  READONLY params: ARRAY OF Param;
                  result: T): Proc =
  VAR
    p  := NEW(Proc);
    n  := NUMBER(params);
    ps := NEW(REF ARRAY OF Param, n);
    vs := NEW(REF ARRAY OF Value, n);
  BEGIN
    p.name   := name;
    p.result := result;
    p.items  := NEW(RefSeq.T).init();
    FOR i := 0 TO n - 1 DO
      ps[i] := params[i];
      VAR v := NEW(Value);
      BEGIN
        v.type     := params[i].type;
        v.name     := params[i].name;
        v.vKind    := ValueKind.Param;
        v.proc     := p;
        v.paramIdx := i;
        vs[i] := v;
      END;
    END;
    p.params      := ps;
    p.paramValues := vs;
    RETURN p;
  END NewProc;

PROCEDURE ProcSetParams(p: Proc;  READONLY params: ARRAY OF Param) =
(* Replace p's parameter list (and rebuild its param Values).  Used when a
   bodyless forward stub — created by a call site that knew only the source
   formals — is reused as the real definition, which also has lambda-lifted
   capture params: the stub's narrower param list must be widened to match. *)
  VAR
    n  := NUMBER(params);
    ps := NEW(REF ARRAY OF Param, n);
    vs := NEW(REF ARRAY OF Value, n);
  BEGIN
    FOR i := 0 TO n - 1 DO
      ps[i] := params[i];
      VAR v := NEW(Value);
      BEGIN
        v.type     := params[i].type;
        v.name     := params[i].name;
        v.vKind    := ValueKind.Param;
        v.proc     := p;
        v.paramIdx := i;
        vs[i] := v;
      END;
    END;
    p.params      := ps;
    p.paramValues := vs;
  END ProcSetParams;

PROCEDURE ProcName(p: Proc): TEXT = BEGIN RETURN p.name END ProcName;
PROCEDURE ProcParamCount(p: Proc): INTEGER =
  BEGIN
    IF p.params = NIL THEN RETURN 0 END;
    RETURN NUMBER(p.params^);
  END ProcParamCount;
PROCEDURE ProcParam    (p: Proc; i: INTEGER): Value    = BEGIN RETURN p.paramValues[i]       END ProcParam;
PROCEDURE ProcParamName(p: Proc; i: INTEGER): TEXT     = BEGIN RETURN p.params[i].name        END ProcParamName;
PROCEDURE ProcParamMode(p: Proc; i: INTEGER): ParamMode =
  BEGIN RETURN p.params[i].mode END ProcParamMode;
PROCEDURE ProcResultType(p: Proc): T = BEGIN RETURN p.result END ProcResultType;
PROCEDURE ProcEntry(p: Proc): Block =
  VAR x: REFANY;
  BEGIN
    <* ASSERT p.items.size() > 0, "ProcEntry: no items" *>
    x := p.items.get(0);
    <* ASSERT ISTYPE(x, Block), "ProcEntry: first item is not a block" *>
    RETURN NARROW(x, Block);
  END ProcEntry;
PROCEDURE ProcBlockCount(p: Proc): INTEGER =
  VAR n: INTEGER := 0;
  BEGIN
    FOR i := 0 TO p.items.size() - 1 DO
      IF ISTYPE(p.items.get(i), Block) THEN INC(n) END;
    END;
    RETURN n;
  END ProcBlockCount;
PROCEDURE ProcBlock(p: Proc; i: INTEGER): Block =
  VAR seen: INTEGER := 0;  x: REFANY;
  BEGIN
    FOR k := 0 TO p.items.size() - 1 DO
      x := p.items.get(k);
      IF ISTYPE(x, Block) THEN
        IF seen = i THEN RETURN NARROW(x, Block) END;
        INC(seen);
      END;
    END;
    <* ASSERT FALSE, "ProcBlock: index out of range" *>
    RETURN NIL;
  END ProcBlock;
PROCEDURE ProcAddBlock(p: Proc; b: Block) =
  BEGIN
    b.proc := p;
    p.items.addhi(b);
  END ProcAddBlock;
PROCEDURE ProcSetRaises(p: Proc;  READONLY exceptionSyms: ARRAY OF TEXT) =
  VAR n := NUMBER(exceptionSyms);  rs := NEW(REF ARRAY OF TEXT, n);
  BEGIN
    FOR i := 0 TO n - 1 DO rs[i] := exceptionSyms[i] END;
    p.raises := rs;
  END ProcSetRaises;

PROCEDURE ProcRaisesCount(p: Proc): INTEGER =
  BEGIN
    IF p.raises = NIL THEN RETURN 0 END;
    RETURN NUMBER(p.raises^);
  END ProcRaisesCount;

PROCEDURE ProcRaises(p: Proc;  i: INTEGER): TEXT =
  BEGIN RETURN p.raises[i] END ProcRaises;

PROCEDURE ProcSetNoReturn(p: Proc;  v: BOOLEAN) =
  BEGIN p.noReturn := v END ProcSetNoReturn;
PROCEDURE ProcIsNoReturn(p: Proc): BOOLEAN =
  BEGIN RETURN p.noReturn END ProcIsNoReturn;

PROCEDURE ProcSetLinkage(p: Proc;  l: Linkage) =
  BEGIN p.linkage := l END ProcSetLinkage;
PROCEDURE ProcGetLinkage(p: Proc): Linkage =
  BEGIN RETURN p.linkage END ProcGetLinkage;

PROCEDURE ProcSetCallingConvention(p: Proc;  cc: CallingConvention) =
  BEGIN p.cc := cc END ProcSetCallingConvention;
PROCEDURE ProcGetCallingConvention(p: Proc): CallingConvention =
  BEGIN RETURN p.cc END ProcGetCallingConvention;

PROCEDURE ProcSetSrcLoc(p: Proc;  file: TEXT;  line: INTEGER) =
  BEGIN p.srcFile := file; p.srcLine := line END ProcSetSrcLoc;
PROCEDURE ProcSrcFile(p: Proc): TEXT =
  BEGIN RETURN p.srcFile END ProcSrcFile;
PROCEDURE ProcSrcLine(p: Proc): INTEGER =
  BEGIN RETURN p.srcLine END ProcSrcLine;

PROCEDURE ProcAddEnvelope(p: Proc;  e: Envelope) =
  BEGIN
    (* Tag interior blocks with their containing proc so name generation
       and so on work consistently. *)
    e.body.proc := p;
    IF e.handlers # NIL THEN
      FOR k := 0 TO LAST(e.handlers^) DO
        e.handlers[k].block.proc := p;
      END;
    END;
    IF e.finally # NIL THEN e.finally.proc := p END;
    p.items.addhi(e);
  END ProcAddEnvelope;
PROCEDURE ProcItemCount(p: Proc): INTEGER =
  BEGIN RETURN p.items.size() END ProcItemCount;
PROCEDURE ProcItem(p: Proc; i: INTEGER): REFANY =
  BEGIN RETURN p.items.get(i) END ProcItem;
PROCEDURE ProcItemIsBlock(p: Proc; i: INTEGER): BOOLEAN =
  BEGIN RETURN ISTYPE(p.items.get(i), Block) END ProcItemIsBlock;
PROCEDURE ProcItemIsEnvelope(p: Proc; i: INTEGER): BOOLEAN =
  BEGIN RETURN ISTYPE(p.items.get(i), Envelope) END ProcItemIsEnvelope;

(*--------------------------------------------------------------- Module *)

REVEAL Module = BRANDED "MSIR.Module" REF RECORD
  name:        TEXT;
  isInterface: BOOLEAN := FALSE;
  triple:      TEXT := NIL;
  datalayout:  TEXT := NIL;
  imports:    RefSeq.T;                            (* elements: TEXT *)
  procs:      RefSeq.T;                            (* elements: Proc *)
  globals:    RefSeq.T;                            (* elements: Global *)
  globalInits: RefSeq.T;                           (* elements: GlobalInit —
     compile-time-constant initial values for embedded user globals, applied by
     an early global constructor (see MSIRToLLVM); needed because @Mod_M3_info's
     user region is a zero blob and some globals (e.g. RTType's InfoMap tables)
     are used before any module body runs. *)
  excDescs:      RefSeq.T;                         (* elements: ExcDesc *)
  importBinders: RefSeq.T;                         (* elements: TEXT binder names *)
  typeDescs:     RefSeq.T;                         (* elements: TypeDesc *)
  textLiterals:  RefSeq.T;                         (* elements: TextLit *)
  constArrays:   RefSeq.T;                         (* elements: ConstArray *)
  nextGlobalOff: INTEGER := 0;  (* byte offset for next embedded user global *)
  (* Hook proc stubs set by MSIREmit via RunTyme lookup.  NIL = use
     fallback hardcoded names in the LLVM emitter. *)
  gcLoadBarrierProc  : Proc := NIL;   (* RTHooks__CheckLoadTracedRef *)
  gcStoreBarrierProc : Proc := NIL;   (* RTHooks__CheckStoreTraced   *)
  scanTypecaseProc   : Proc := NIL;   (* RTHooks__ScanTypecase       *)
  textLitHooks : ARRAY [0..4] OF Proc;
  (* 0=TextLitInfo 1=TextLitGetChar 2=TextLitGetWideChar
     3=TextLitGetChars 4=TextLitGetWideChars — NIL = not yet registered *)
  typeLinks    : REF ARRAY OF TypeLink    := NIL;
  nTypeLinks   : INTEGER                  := 0;
  revelations  : REF ARRAY OF Revelation := NIL;
  nRevelations : INTEGER                  := 0;
END;


REVEAL Global = BRANDED "MSIR.Global" REF RECORD
  name:         TEXT;
  type:         T;
  isTraced:     BOOLEAN;
  isExternal:   BOOLEAN  := FALSE;
  byteOffset:   INTEGER  := -1;   (* -1 = standalone global; >=0 = offset in @Mod_M3_info *)
  refValue:     Value    := NIL;
  backingBytes: INTEGER  := 0;    (* >0: emit as [N x i8] zeroinitializer backing storage *)
  isWeak:       BOOLEAN  := FALSE; (* TRUE => emit weak linkage: an interface-
                                      exported var re-defined by its same-name
                                      implementation module, so the interface
                                      unit's strong def wins at link.  Module-
                                      private globals stay strong (FALSE). *)
END;

PROCEDURE NewGlobal(name: TEXT;  type: T;  isTraced: BOOLEAN;
                    isExternal: BOOLEAN := FALSE): Global =
  VAR
    g := NEW(Global);
    v := NEW(Value);
  BEGIN
    g.name       := name;
    g.type       := type;
    g.isTraced   := isTraced;
    g.isExternal := isExternal;
    v.name       := name;
    v.vKind      := ValueKind.GlobalRef;
    IF isTraced THEN
      v.type := TGcSlot(type);
    ELSE
      v.type := TPtr(type);
    END;
    g.refValue := v;
    RETURN g;
  END NewGlobal;

PROCEDURE GlobalName       (g: Global): TEXT    = BEGIN RETURN g.name       END GlobalName;
PROCEDURE GlobalType       (g: Global): T       = BEGIN RETURN g.type       END GlobalType;
PROCEDURE GlobalSetWeak    (g: Global) = BEGIN g.isWeak := TRUE END GlobalSetWeak;
PROCEDURE GlobalIsWeak     (g: Global): BOOLEAN = BEGIN RETURN g.isWeak     END GlobalIsWeak;
PROCEDURE GlobalIsTraced   (g: Global): BOOLEAN = BEGIN RETURN g.isTraced   END GlobalIsTraced;
PROCEDURE GlobalIsExternal    (g: Global): BOOLEAN = BEGIN RETURN g.isExternal    END GlobalIsExternal;
PROCEDURE GlobalByteOffset    (g: Global): INTEGER = BEGIN RETURN g.byteOffset END GlobalByteOffset;
PROCEDURE GlobalSetStructField(g: Global;  byteOff: INTEGER;  ref: Value) =
  BEGIN g.byteOffset := byteOff; g.refValue := ref END GlobalSetStructField;
PROCEDURE GlobalValue(g: Global): Value         = BEGIN RETURN g.refValue       END GlobalValue;
PROCEDURE GlobalSetBackingBytes(g: Global; n: INTEGER) = BEGIN g.backingBytes := n END GlobalSetBackingBytes;
PROCEDURE GlobalBackingBytes   (g: Global): INTEGER    = BEGIN RETURN g.backingBytes END GlobalBackingBytes;
PROCEDURE GlobalAddrValue      (g: Global): Value =
  (* Return a ptr-typed Value whose LLVM lowering is @GlobalName.  Uses the
     StructFieldRef kind with byteOffset=0, which MSIRToLLVM lowers as
     getelementptr i8, ptr @Name, i64 0 — equivalent to @Name as a ptr. *)
  VAR v := NEW(Value);
  BEGIN
    v.type      := TPtr(TVoid());
    v.vKind     := ValueKind.StructFieldRef;
    v.name      := "@" & g.name;   (* the info-global name: "@Name" *)
    v.structOff := 0;
    RETURN v;
  END GlobalAddrValue;

PROCEDURE ModuleAddGlobal(m: Module;  g: Global) =
  BEGIN
    FOR i := 0 TO m.globals.size() - 1 DO
      IF m.globals.get(i) = g THEN RETURN END;
    END;
    m.globals.addhi(g)
  END ModuleAddGlobal;

TYPE GlobalInitRec = BRANDED "MSIR.GlobalInit" REF RECORD
  byteOff : INTEGER;
  val     : Value;
END;

(* One entry per object method that has a proc default (own methods AND
   overrides of inherited methods).  Mirrors C's ObjectType.GenLinkProc: the
   emitted linkProc stores `proc` into the object's defaultMethods vtable at
   byte offset `declBase + offBytes`, where declBase is the DECLARING type's
   method-region base — a compile-time constant when known, else loaded at
   runtime from the declaring type's typecell OTC_methodOffset (declUID). *)
TYPE MethodInitRec = BRANDED "MSIR.MethodInit" REF RECORD
  proc          : TEXT;
  offBytes      : INTEGER;   (* method.offset within its declaring type, bytes *)
  declBaseKnown : BOOLEAN;
  declBase      : INTEGER;   (* declaring type method base (bytes), if known *)
  declUID       : INTEGER;   (* declaring type UID (runtime methodOffset load) *)
END;

PROCEDURE AddTypeDescMethodInit(d: TypeDesc;  proc: TEXT;  offBytes: INTEGER;
                                declBaseKnown: BOOLEAN;  declBase: INTEGER;
                                declUID: INTEGER) =
  BEGIN
    IF d.methodInits = NIL THEN d.methodInits := NEW(RefSeq.T).init() END;
    d.methodInits.addhi(NEW(MethodInitRec, proc := proc, offBytes := offBytes,
                            declBaseKnown := declBaseKnown, declBase := declBase,
                            declUID := declUID));
  END AddTypeDescMethodInit;

PROCEDURE TypeDescMethodInitCount(d: TypeDesc): INTEGER =
  BEGIN IF d.methodInits = NIL THEN RETURN 0 END; RETURN d.methodInits.size() END
  TypeDescMethodInitCount;

PROCEDURE TypeDescMethodInitProc(d: TypeDesc; i: INTEGER): TEXT =
  BEGIN RETURN NARROW(d.methodInits.get(i), MethodInitRec).proc END
  TypeDescMethodInitProc;

PROCEDURE TypeDescMethodInitOff(d: TypeDesc; i: INTEGER): INTEGER =
  BEGIN RETURN NARROW(d.methodInits.get(i), MethodInitRec).offBytes END
  TypeDescMethodInitOff;

PROCEDURE TypeDescMethodInitBaseKnown(d: TypeDesc; i: INTEGER): BOOLEAN =
  BEGIN RETURN NARROW(d.methodInits.get(i), MethodInitRec).declBaseKnown END
  TypeDescMethodInitBaseKnown;

PROCEDURE TypeDescMethodInitBase(d: TypeDesc; i: INTEGER): INTEGER =
  BEGIN RETURN NARROW(d.methodInits.get(i), MethodInitRec).declBase END
  TypeDescMethodInitBase;

PROCEDURE TypeDescMethodInitDeclUID(d: TypeDesc; i: INTEGER): INTEGER =
  BEGIN RETURN NARROW(d.methodInits.get(i), MethodInitRec).declUID END
  TypeDescMethodInitDeclUID;

PROCEDURE SetTypeDescUseLinkProc(d: TypeDesc; b: BOOLEAN) =
  BEGIN d.useLinkProc := b END SetTypeDescUseLinkProc;

PROCEDURE TypeDescUseLinkProc(d: TypeDesc): BOOLEAN =
  BEGIN RETURN d.useLinkProc END TypeDescUseLinkProc;

PROCEDURE ModuleAddGlobalInit(m: Module;  byteOff: INTEGER;  val: Value) =
  BEGIN
    FOR i := 0 TO m.globalInits.size() - 1 DO
      IF NARROW(m.globalInits.get(i), GlobalInitRec).byteOff = byteOff THEN
        RETURN  (* already recorded at this offset *)
      END;
    END;
    m.globalInits.addhi(NEW(GlobalInitRec, byteOff := byteOff, val := val));
  END ModuleAddGlobalInit;

PROCEDURE ModuleGlobalInitCount(m: Module): INTEGER =
  BEGIN RETURN m.globalInits.size() END ModuleGlobalInitCount;

PROCEDURE ModuleGlobalInitOffset(m: Module;  i: INTEGER): INTEGER =
  BEGIN RETURN NARROW(m.globalInits.get(i), GlobalInitRec).byteOff END
  ModuleGlobalInitOffset;

PROCEDURE ModuleGlobalInitValue(m: Module;  i: INTEGER): Value =
  BEGIN RETURN NARROW(m.globalInits.get(i), GlobalInitRec).val END
  ModuleGlobalInitValue;
PROCEDURE ModuleGlobalCount(m: Module): INTEGER =
  BEGIN RETURN m.globals.size() END ModuleGlobalCount;
PROCEDURE ModuleGlobal(m: Module;  i: INTEGER): Global =
  BEGIN RETURN m.globals.get(i) END ModuleGlobal;

(*---------------------------------------------- ConstArray *)

REVEAL ConstArray = BRANDED "MSIR.ConstArray" REF RECORD
  name:    TEXT;
  eltType: T;
  elts:    REF ARRAY OF Value;
  ptrVal:  Value;
END;

PROCEDURE NewConstArray(name: TEXT; eltType: T;
                        READONLY elts: ARRAY OF Value): ConstArray =
  VAR ca := NEW(ConstArray);  v := NEW(Value);  n := NUMBER(elts);
  BEGIN
    ca.name    := name;
    ca.eltType := eltType;
    ca.elts    := NEW(REF ARRAY OF Value, n);
    ca.elts^   := elts;
    v.type     := TPtr(TFixedArray(n, eltType));
    v.vKind    := ValueKind.InsnResult;
    v.name     := "@" & name;
    ca.ptrVal  := v;
    RETURN ca;
  END NewConstArray;

PROCEDURE ConstArrayName    (ca: ConstArray): TEXT    = BEGIN RETURN ca.name    END ConstArrayName;
PROCEDURE ConstArrayEltType (ca: ConstArray): T       = BEGIN RETURN ca.eltType END ConstArrayEltType;
PROCEDURE ConstArrayEltCount(ca: ConstArray): INTEGER =
  BEGIN RETURN NUMBER(ca.elts^) END ConstArrayEltCount;
PROCEDURE ConstArrayElt (ca: ConstArray; i: INTEGER): Value =
  BEGIN RETURN ca.elts^[i] END ConstArrayElt;
PROCEDURE ConstArrayValue(ca: ConstArray): Value = BEGIN RETURN ca.ptrVal END ConstArrayValue;

PROCEDURE ModuleAddConstArray(m: Module; ca: ConstArray) =
  BEGIN m.constArrays.addhi(ca) END ModuleAddConstArray;
PROCEDURE ModuleConstArrayCount(m: Module): INTEGER =
  BEGIN RETURN m.constArrays.size() END ModuleConstArrayCount;
PROCEDURE ModuleConstArray(m: Module; i: INTEGER): ConstArray =
  BEGIN RETURN m.constArrays.get(i) END ModuleConstArray;

PROCEDURE ModuleAllocGlobal(m: Module;  byteSize: INTEGER;
                             byteAlign: INTEGER): INTEGER =
  VAR off: INTEGER;
  BEGIN
    (* Initialise on first call: start right after MI_SIZE *)
    IF m.nextGlobalOff = 0 THEN
      m.nextGlobalOff := MI_nFields * Target.AddressBytes();
    END;
    (* Round up to alignment *)
    off := m.nextGlobalOff;
    IF byteAlign > 1 THEN
      off := (off + byteAlign - 1) - ((off + byteAlign - 1) MOD byteAlign);
    END;
    m.nextGlobalOff := off + byteSize;
    RETURN off;
  END ModuleAllocGlobal;

PROCEDURE ModuleNoteGlobal(m: Module;  endByteOff: INTEGER) =
(* Record that a user global extends to endByteOff (= byteOffset + byteSize) in
   the module struct, growing the struct to contain it.  Used when globals are
   placed at the front-end's canonical Variable offset (so cross-module
   importers, which read at that offset, agree) rather than MSIR's dense
   ModuleAllocGlobal packing. *)
  BEGIN
    IF endByteOff > m.nextGlobalOff THEN m.nextGlobalOff := endByteOff END;
  END ModuleNoteGlobal;

PROCEDURE ModuleGlobalStructSize(m: Module): INTEGER =
  BEGIN
    IF m.nextGlobalOff = 0 THEN
      RETURN MI_nFields * Target.AddressBytes();
    END;
    RETURN m.nextGlobalOff;
  END ModuleGlobalStructSize;

(*---------------------------------------------- exception descriptors *)

REVEAL ExcDesc = BRANDED "MSIR.ExcDesc" REF RECORD
  name    : TEXT;
  uid     : INTEGER;
  display : TEXT := NIL;  (* human-readable qualified name, e.g. "Main.a" *)
  ptrVal  : Value := NIL;  (* cached ptr value *)
END;

PROCEDURE NewExcDesc(name: TEXT;  uid: INTEGER;  display: TEXT := NIL): ExcDesc =
  VAR d := NEW(ExcDesc);  v := NEW(Value);
  BEGIN
    d.name    := name;
    d.uid     := uid;
    d.display := display;
    (* Use a bare InsnResult-style value with the full @symbol name so
       LLOpVal emits it as-is without going through LLGlobalSym. *)
    v.type  := TPtr(TVoid());
    v.name  := "@" & name;
    v.vKind := ValueKind.InsnResult;
    d.ptrVal := v;
    RETURN d;
  END NewExcDesc;

PROCEDURE ExcDescName (d: ExcDesc): TEXT    = BEGIN RETURN d.name   END ExcDescName;
PROCEDURE ExcDescUID  (d: ExcDesc): INTEGER = BEGIN RETURN d.uid    END ExcDescUID;
PROCEDURE ExcDescDisplay (d: ExcDesc): TEXT = BEGIN RETURN d.display END ExcDescDisplay;
PROCEDURE ExcDescValue(d: ExcDesc): Value   = BEGIN RETURN d.ptrVal END ExcDescValue;

PROCEDURE ModuleAddExcDesc  (m: Module;  d: ExcDesc) = BEGIN m.excDescs.addhi(d) END ModuleAddExcDesc;
PROCEDURE ModuleExcDescCount(m: Module): INTEGER     = BEGIN RETURN m.excDescs.size() END ModuleExcDescCount;
PROCEDURE ModuleExcDesc     (m: Module;  i: INTEGER): ExcDesc =
  BEGIN RETURN m.excDescs.get(i) END ModuleExcDesc;

(*---------------------------------------------- type descriptors *)

REVEAL TypeDesc = BRANDED "MSIR.TypeDesc" REF RECORD
  name          : TEXT;
  userName      : TEXT := NIL;  (* OBJ: user-visible name for TC_name field *)
  initProcName  : TEXT := NIL;  (* OBJ: field-default init proc LLVM symbol *)
  uid           : INTEGER;
  fp            : ARRAY [0..7] OF [0..255];  (* Fingerprint2.T bytes, little-endian *)
  isTraced      : BOOLEAN;
  kind          : INTEGER;  (* ORD(RT0.TypeKind) *)
  dataSize      : INTEGER;  (* bytes *)
  dataAlignment : INTEGER;  (* bits *)
  parentUID     : INTEGER;  (* OBJ: parent fingerprint *)
  dataOffset    : INTEGER;  (* OBJ: field region byte offset *)
  methodOffset  : INTEGER;  (* OBJ: vtable byte offset where own methods start *)
  methods       : REF ARRAY OF TEXT;  (* OBJ: vtable function names *)
  methodBytes   : INTEGER;  (* OBJ: vtable byte size; -1 = derive from methods *)
  dynamicMethOff: BOOLEAN  := FALSE;  (* OBJ: method names at LOCAL indices; linkProc reads OTC_methodOffset at runtime *)
  useLinkProc   : BOOLEAN  := FALSE;  (* OBJ: emit defaultMethods=NIL + a C-GenLinkProc-style
                                         linkProc from methodInits (handles overrides of
                                         inherited methods; RTLinker copies parent first) *)
  methodInits   : RefSeq.T := NIL;    (* OBJ: elements MethodInitRec (see below) *)
  nDimensions   : INTEGER := 0;  (* Array: open array rank *)
  elementSize   : INTEGER := 0;  (* Array: element byte size *)
  ptrVal        : Value := NIL;
END;

PROCEDURE NewTypeDesc(name: TEXT; uid: INTEGER; isTraced: BOOLEAN;
                      kind: INTEGER; dataSize: INTEGER;
                      dataAlignment: INTEGER;
                      parentUID: INTEGER := 0;
                      dataOffset: INTEGER := 0;
                      READONLY methods: ARRAY OF TEXT := ARRAY OF TEXT{};
                      methodBytes: INTEGER := -1): TypeDesc =
  VAR d := NEW(TypeDesc);  v := NEW(Value);
      ms := NEW(REF ARRAY OF TEXT, NUMBER(methods));
  BEGIN
    FOR i := 0 TO NUMBER(methods) - 1 DO ms[i] := methods[i] END;
    d.name          := name;
    d.uid           := uid;
    d.isTraced      := isTraced;
    d.kind          := kind;
    d.dataSize      := dataSize;
    d.dataAlignment := dataAlignment;
    d.parentUID     := parentUID;
    d.dataOffset    := dataOffset;
    d.methods       := ms;
    d.methodBytes   := methodBytes;
    v.type  := TPtr(TVoid());
    v.name  := "@" & name;
    v.vKind := ValueKind.InsnResult;  (* emitted as @name *)
    d.ptrVal := v;
    RETURN d;
  END NewTypeDesc;

PROCEDURE TypeDescName       (d: TypeDesc): TEXT    = BEGIN RETURN d.name          END TypeDescName;
PROCEDURE TypeDescValue      (d: TypeDesc): Value   = BEGIN RETURN d.ptrVal        END TypeDescValue;
PROCEDURE TypeDescUID        (d: TypeDesc): INTEGER = BEGIN RETURN d.uid           END TypeDescUID;
PROCEDURE SetTypeDescFP(d: TypeDesc; READONLY b: ARRAY OF [0..255]) =
  BEGIN
    FOR i := 0 TO MIN(7, LAST(b)) DO d.fp[i] := b[i] END;
  END SetTypeDescFP;
PROCEDURE TypeDescFPByte(d: TypeDesc; i: INTEGER): INTEGER =
  BEGIN RETURN d.fp[i] END TypeDescFPByte;
PROCEDURE TypeDescTraced     (d: TypeDesc): BOOLEAN = BEGIN RETURN d.isTraced      END TypeDescTraced;
PROCEDURE TypeDescKind       (d: TypeDesc): INTEGER = BEGIN RETURN d.kind          END TypeDescKind;
PROCEDURE TypeDescSize       (d: TypeDesc): INTEGER = BEGIN RETURN d.dataSize      END TypeDescSize;
PROCEDURE TypeDescAlign      (d: TypeDesc): INTEGER = BEGIN RETURN d.dataAlignment END TypeDescAlign;
PROCEDURE TypeDescParentUID  (d: TypeDesc): INTEGER = BEGIN RETURN d.parentUID     END TypeDescParentUID;
PROCEDURE TypeDescDataOffset    (d: TypeDesc): INTEGER = BEGIN RETURN d.dataOffset    END TypeDescDataOffset;
PROCEDURE TypeDescMethodOffset  (d: TypeDesc): INTEGER = BEGIN RETURN d.methodOffset  END TypeDescMethodOffset;
PROCEDURE SetTypeDescMethodOffset(d: TypeDesc; mo: INTEGER) = BEGIN d.methodOffset := mo END SetTypeDescMethodOffset;
PROCEDURE SetTypeDescUserName(d: TypeDesc; name: TEXT) = BEGIN d.userName := name      END SetTypeDescUserName;
PROCEDURE TypeDescUserName   (d: TypeDesc): TEXT    = BEGIN RETURN d.userName          END TypeDescUserName;
PROCEDURE SetTypeDescInitProc (d: TypeDesc; procName: TEXT) = BEGIN d.initProcName := procName   END SetTypeDescInitProc;
PROCEDURE TypeDescInitProcName(d: TypeDesc): TEXT            = BEGIN RETURN d.initProcName        END TypeDescInitProcName;
PROCEDURE TypeDescMethodBytes(d: TypeDesc): INTEGER =
  BEGIN
    IF d.methodBytes >= 0 THEN RETURN d.methodBytes END;
    IF d.methods = NIL THEN RETURN 0 END;
    RETURN NUMBER(d.methods^) * Target.AddressBytes();
  END TypeDescMethodBytes;
PROCEDURE TypeDescMethodCount(d: TypeDesc): INTEGER =
  BEGIN
    IF d.methods = NIL THEN RETURN 0 END;
    RETURN NUMBER(d.methods^);
  END TypeDescMethodCount;
PROCEDURE TypeDescMethod(d: TypeDesc; i: INTEGER): TEXT =
  BEGIN RETURN d.methods[i] END TypeDescMethod;

PROCEDURE TypeDescDynamicMethOff(d: TypeDesc): BOOLEAN =
  BEGIN RETURN d.dynamicMethOff END TypeDescDynamicMethOff;
PROCEDURE SetTypeDescDynamicMethOff(d: TypeDesc; b: BOOLEAN) =
  BEGIN d.dynamicMethOff := b END SetTypeDescDynamicMethOff;

PROCEDURE TypeDescSetArrayInfo(d: TypeDesc; nDimensions, elementSize: INTEGER) =
  BEGIN d.nDimensions := nDimensions;  d.elementSize := elementSize END TypeDescSetArrayInfo;
PROCEDURE TypeDescNDimensions(d: TypeDesc): INTEGER =
  BEGIN RETURN d.nDimensions END TypeDescNDimensions;
PROCEDURE TypeDescElementSize(d: TypeDesc): INTEGER =
  BEGIN RETURN d.elementSize END TypeDescElementSize;

PROCEDURE TypeCellRef (name: TEXT): Value =
  VAR v := NEW(Value);
  BEGIN
    v.type  := TPtr(TVoid());
    v.name  := "@" & name;
    v.vKind := ValueKind.InsnResult;
    RETURN v;
  END TypeCellRef;

PROCEDURE ModuleAddTypeDesc  (m: Module;  d: TypeDesc) = BEGIN m.typeDescs.addhi(d) END ModuleAddTypeDesc;
PROCEDURE ModuleTypeDescCount(m: Module): INTEGER      = BEGIN RETURN m.typeDescs.size() END ModuleTypeDescCount;
PROCEDURE ModuleTypeDesc     (m: Module;  i: INTEGER): TypeDesc =
  BEGIN RETURN m.typeDescs.get(i) END ModuleTypeDesc;

TYPE TextLit = REF RECORD  (* populated by MSIREmit.EndUnit from TextExpr *)
  chars : TEXT;
  cnt   : INTEGER;
END;

PROCEDURE ModuleAddTextLit(m: Module;  chars: TEXT;  cnt: INTEGER): INTEGER =
  VAR lit: TextLit;  n := m.textLiterals.size();
  BEGIN
    lit := NEW(TextLit, chars := chars, cnt := cnt);
    m.textLiterals.addhi(lit);
    RETURN n;
  END ModuleAddTextLit;
PROCEDURE ModuleTextLitCount(m: Module): INTEGER =
  BEGIN RETURN m.textLiterals.size() END ModuleTextLitCount;
PROCEDURE ModuleTextLitChars(m: Module;  i: INTEGER): TEXT =
  BEGIN RETURN NARROW(m.textLiterals.get(i), TextLit).chars END ModuleTextLitChars;
PROCEDURE ModuleTextLitCnt(m: Module;  i: INTEGER): INTEGER =
  BEGIN RETURN NARROW(m.textLiterals.get(i), TextLit).cnt END ModuleTextLitCnt;

PROCEDURE ModuleSetTextLitHooks(m: Module;
                                 READONLY hooks: ARRAY [0..4] OF Proc) =
  BEGIN m.textLitHooks := hooks END ModuleSetTextLitHooks;
PROCEDURE ModuleGetTextLitHook(m: Module;  i: INTEGER): Proc =
  BEGIN RETURN m.textLitHooks[i] END ModuleGetTextLitHook;

PROCEDURE BuildTextLiteralRef(b: Block;  uid: INTEGER): Value =
  (* Kept for API compat; callers should prefer ConstTextLit directly. *)
  <*UNUSED*> VAR dummy := b;
  BEGIN
    RETURN ConstTextLit(uid, NIL, 0);
  END BuildTextLiteralRef;

(*---------------------------------------------- type links (cell_ptrs) *)

REVEAL TypeLink = BRANDED "MSIR.TypeLink" REF RECORD
  name : TEXT;
  uid  : INTEGER;
END;

PROCEDURE NewTypeLink(name: TEXT; uid: INTEGER): TypeLink =
  VAR tl := NEW(TypeLink);
  BEGIN tl.name := name; tl.uid := uid; RETURN tl END NewTypeLink;

PROCEDURE TypeLinkName(tl: TypeLink): TEXT =
  BEGIN RETURN tl.name END TypeLinkName;

PROCEDURE TypeLinkUID(tl: TypeLink): INTEGER =
  BEGIN RETURN tl.uid END TypeLinkUID;

PROCEDURE ModuleAddTypeLink(m: Module; tl: TypeLink) =
  BEGIN
    IF m.typeLinks = NIL OR m.nTypeLinks >= NUMBER(m.typeLinks^) THEN
      VAR n  := MAX(8, 2 * m.nTypeLinks);
          nb := NEW(REF ARRAY OF TypeLink, n);
      BEGIN
        IF m.typeLinks # NIL THEN
          SUBARRAY(nb^, 0, m.nTypeLinks) := SUBARRAY(m.typeLinks^, 0, m.nTypeLinks);
        END;
        m.typeLinks := nb;
      END;
    END;
    m.typeLinks[m.nTypeLinks] := tl;
    INC(m.nTypeLinks);
  END ModuleAddTypeLink;

PROCEDURE ModuleTypeLinkCount(m: Module): INTEGER =
  BEGIN RETURN m.nTypeLinks END ModuleTypeLinkCount;

PROCEDURE ModuleTypeLink(m: Module; i: INTEGER): TypeLink =
  BEGIN RETURN m.typeLinks[i] END ModuleTypeLink;

(*----------------------------------------------- Revelation --------------------*)

REVEAL Revelation = BRANDED "MSIR.Revelation" REF RECORD
  lhsUID: INTEGER;
  rhsUID: INTEGER;
END;

PROCEDURE NewRevelation(lhsUID, rhsUID: INTEGER): Revelation =
  VAR r := NEW(Revelation);
  BEGIN r.lhsUID := lhsUID; r.rhsUID := rhsUID; RETURN r END NewRevelation;

PROCEDURE RevelationLhsUID(r: Revelation): INTEGER =
  BEGIN RETURN r.lhsUID END RevelationLhsUID;

PROCEDURE RevelationRhsUID(r: Revelation): INTEGER =
  BEGIN RETURN r.rhsUID END RevelationRhsUID;

PROCEDURE ModuleAddRevelation(m: Module; r: Revelation) =
  BEGIN
    IF m.revelations = NIL OR m.nRevelations >= NUMBER(m.revelations^) THEN
      VAR n  := MAX(4, 2 * m.nRevelations);
          nb := NEW(REF ARRAY OF Revelation, n);
      BEGIN
        IF m.revelations # NIL THEN
          SUBARRAY(nb^, 0, m.nRevelations) := SUBARRAY(m.revelations^, 0, m.nRevelations);
        END;
        m.revelations := nb;
      END;
    END;
    m.revelations[m.nRevelations] := r;
    INC(m.nRevelations);
  END ModuleAddRevelation;

PROCEDURE ModuleRevelationCount(m: Module): INTEGER =
  BEGIN RETURN m.nRevelations END ModuleRevelationCount;

PROCEDURE ModuleRevelation(m: Module; i: INTEGER): Revelation =
  BEGIN RETURN m.revelations[i] END ModuleRevelation;

PROCEDURE ModuleAddImportBinder  (m: Module;  binder: TEXT) =
  BEGIN
    (* Deduplicate: skip if already registered. *)
    FOR i := 0 TO m.importBinders.size() - 1 DO
      IF Text.Equal(m.importBinders.get(i), binder) THEN RETURN END;
    END;
    m.importBinders.addhi(binder);
  END ModuleAddImportBinder;
PROCEDURE ModuleImportBinderCount(m: Module): INTEGER =
  BEGIN RETURN m.importBinders.size() END ModuleImportBinderCount;
PROCEDURE ModuleImportBinder(m: Module;  i: INTEGER): TEXT =
  BEGIN RETURN m.importBinders.get(i) END ModuleImportBinder;

PROCEDURE NewModule(name: TEXT): Module =
  VAR m := NEW(Module);
  BEGIN
    m.name          := name;
    m.imports       := NEW(RefSeq.T).init();
    m.procs         := NEW(RefSeq.T).init();
    m.globals       := NEW(RefSeq.T).init();
    m.globalInits   := NEW(RefSeq.T).init();
    m.excDescs      := NEW(RefSeq.T).init();
    m.importBinders := NEW(RefSeq.T).init();
    m.typeDescs      := NEW(RefSeq.T).init();
    m.textLiterals   := NEW(RefSeq.T).init();
    m.constArrays    := NEW(RefSeq.T).init();
    m.nextGlobalOff  := 0;  (* lazily initialised to MI_SIZE on first allocation *)
    RETURN m;
  END NewModule;

PROCEDURE ModuleName(m: Module): TEXT = BEGIN RETURN m.name END ModuleName;
PROCEDURE ModuleSetIsInterface(m: Module;  isInterface: BOOLEAN) =
  BEGIN m.isInterface := isInterface END ModuleSetIsInterface;
PROCEDURE ModuleIsInterface(m: Module): BOOLEAN =
  BEGIN RETURN m.isInterface END ModuleIsInterface;

PROCEDURE SetModuleTarget(m: Module;  triple, datalayout: TEXT) =
  BEGIN m.triple := triple;  m.datalayout := datalayout END SetModuleTarget;
PROCEDURE ModuleTriple(m: Module): TEXT =
  BEGIN RETURN m.triple END ModuleTriple;
PROCEDURE ModuleDataLayout(m: Module): TEXT =

  BEGIN RETURN m.datalayout END ModuleDataLayout;

PROCEDURE SetModuleHooks(m: Module;
                          gcLoad, gcStore, scanTypecase: Proc) =
  BEGIN
    m.gcLoadBarrierProc  := gcLoad;
    m.gcStoreBarrierProc := gcStore;
    m.scanTypecaseProc   := scanTypecase;
  END SetModuleHooks;
PROCEDURE ModuleGCLoadBarrier (m: Module): Proc =
  BEGIN RETURN m.gcLoadBarrierProc  END ModuleGCLoadBarrier;
PROCEDURE ModuleGCStoreBarrier(m: Module): Proc =
  BEGIN RETURN m.gcStoreBarrierProc END ModuleGCStoreBarrier;
PROCEDURE ModuleScanTypecase  (m: Module): Proc =
  BEGIN RETURN m.scanTypecaseProc   END ModuleScanTypecase;

PROCEDURE ModuleAddImport(m: Module;  name: TEXT) =
  BEGIN m.imports.addhi(name) END ModuleAddImport;
PROCEDURE ModuleImportCount(m: Module): INTEGER =
  BEGIN RETURN m.imports.size() END ModuleImportCount;
PROCEDURE ModuleImport(m: Module; i: INTEGER): TEXT =
  BEGIN RETURN m.imports.get(i) END ModuleImport;
PROCEDURE ModuleAddProc(m: Module;  p: Proc) =
  BEGIN m.procs.addhi(p) END ModuleAddProc;
PROCEDURE ModuleProcCount(m: Module): INTEGER =
  BEGIN RETURN m.procs.size() END ModuleProcCount;
PROCEDURE ModuleProc(m: Module; i: INTEGER): Proc =
  BEGIN RETURN m.procs.get(i) END ModuleProc;

(*---------------------------------------------------------------- Builders *)

VAR currentSrcLine: INTEGER := 0;

PROCEDURE freshName(p: Proc): TEXT =
  BEGIN
    INC(p.nameCounter);
    (* Use "t" prefix so LLVM does not treat these as unnamed-temporary slot
       numbers, which must be assigned sequentially counting all values. *)
    RETURN "%t" & Fmt.Int(p.nameCounter);
  END freshName;

PROCEDURE finalName(b: Block;  name: TEXT): TEXT =
  BEGIN
    IF Text.Length(name) = 0 THEN
      IF b = NIL OR b.proc = NIL THEN RETURN "%anon" END;
      RETURN freshName(b.proc)
    END;
    IF Text.GetChar(name, 0) = '%' THEN RETURN name END;
    RETURN "%" & name;
  END finalName;

PROCEDURE makeResult(b: Block;  type: T;  name: TEXT;  i: Insn): Value =
  VAR v := NEW(Value);
  BEGIN
    v.type  := type;
    v.name  := finalName(b, name);
    v.vKind := ValueKind.InsnResult;
    v.insn  := i;
    RETURN v;
  END makeResult;

PROCEDURE addInsn(b: Block;  i: Insn) =
  BEGIN
    i.srcLine := currentSrcLine;
    i.block := b;
    b.insns.addhi(i);
  END addInsn;

PROCEDURE binOpType(x, y: Value): T =
  BEGIN
    <* ASSERT Equal(x.type, y.type), "MSIR: binop operand type mismatch" *>
    RETURN x.type;
  END binOpType;

PROCEDURE buildBin(b: Block;  op: Op;  name: TEXT;  x, y: Value): Value =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 2);
  BEGIN
    i.op := op;
    ops[0] := x; ops[1] := y;
    i.operands := ops;
    i.result := makeResult(b, binOpType(x, y), name, i);
    addInsn(b, i);
    RETURN i.result;
  END buildBin;

PROCEDURE BuildIAdd(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.IAdd, name, x, y) END BuildIAdd;
PROCEDURE BuildISub(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.ISub, name, x, y) END BuildISub;
PROCEDURE BuildIMul(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.IMul, name, x, y) END BuildIMul;
PROCEDURE BuildIDiv(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.IDiv, name, x, y) END BuildIDiv;
PROCEDURE BuildIMod(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.IMod, name, x, y) END BuildIMod;

PROCEDURE BuildIAnd(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.IAnd, name, x, y) END BuildIAnd;
PROCEDURE BuildIOr(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.IOr, name, x, y) END BuildIOr;
PROCEDURE BuildIXor(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.IXor, name, x, y) END BuildIXor;
PROCEDURE BuildIShl(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.IShl, name, x, y) END BuildIShl;
PROCEDURE BuildILShr(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.ILShr, name, x, y) END BuildILShr;
PROCEDURE BuildIAShr(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.IAShr, name, x, y) END BuildIAShr;
PROCEDURE BuildIUDiv(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.IUDiv, name, x, y) END BuildIUDiv;
PROCEDURE BuildIURem(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.IURem, name, x, y) END BuildIURem;
PROCEDURE BuildIRotL(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.IRotL, name, x, y) END BuildIRotL;
PROCEDURE BuildIRotR(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.IRotR, name, x, y) END BuildIRotR;

PROCEDURE BuildSelect(b: Block; name: TEXT; cond, ifTrue, ifFalse: Value): Value =
  VAR
    i   := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 3);
  BEGIN
    i.op := Op.Select;
    ops[0] := cond; ops[1] := ifTrue; ops[2] := ifFalse;
    i.operands := ops;
    i.result := makeResult(b, ifTrue.type, name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildSelect;

PROCEDURE BuildICmp(b: Block; name: TEXT; pred: CmpPred; x, y: Value): Value =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 2);
  BEGIN
    i.op := Op.ICmp;
    i.cmpPred := pred;
    ops[0] := x; ops[1] := y;
    i.operands := ops;
    i.result := makeResult(b, TI1(), name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildICmp;

PROCEDURE BuildFAdd(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.FAdd, name, x, y) END BuildFAdd;
PROCEDURE BuildFSub(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.FSub, name, x, y) END BuildFSub;
PROCEDURE BuildFMul(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.FMul, name, x, y) END BuildFMul;
PROCEDURE BuildFDiv(b: Block; name: TEXT; x, y: Value): Value =
  BEGIN RETURN buildBin(b, Op.FDiv, name, x, y) END BuildFDiv;

PROCEDURE BuildFNeg(b: Block; name: TEXT; x: Value): Value =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 1);
  BEGIN
    i.op := Op.FNeg;
    ops[0] := x;
    i.operands := ops;
    i.result := makeResult(b, x.type, name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildFNeg;

PROCEDURE BuildFCmp(b: Block; name: TEXT; pred: FCmpPred; x, y: Value): Value =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 2);
  BEGIN
    i.op := Op.FCmp;
    i.fcmpPred := pred;
    ops[0] := x; ops[1] := y;
    i.operands := ops;
    i.result := makeResult(b, TI1(), name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildFCmp;

PROCEDURE BuildAtomicFence(b: Block; order: MemOrder) =
  VAR i := NEW(Insn);
  BEGIN
    i.op       := Op.AtomicFence;
    i.memOrder := order;
    i.result   := NIL;
    addInsn(b, i);
  END BuildAtomicFence;

PROCEDURE BuildAtomicLoad(b: Block; name: TEXT; elemType: T; ptr: Value; order: MemOrder): Value =
  VAR i := NEW(Insn); ops := NEW(REF ARRAY OF Value, 1);
  BEGIN
    i.op         := Op.AtomicLoad;
    ops[0]       := ptr;
    i.operands   := ops;
    i.targetType := elemType;
    i.memOrder   := order;
    i.result     := makeResult(b, elemType, name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildAtomicLoad;

PROCEDURE BuildAtomicStore(b: Block; value: Value; ptr: Value; order: MemOrder;
                            container: Value := NIL) =
  VAR i    := NEW(Insn);
      n    : INTEGER;
      ops  : REF ARRAY OF Value;
  BEGIN
    IF container = NIL THEN n := 2 ELSE n := 3 END;
    ops        := NEW(REF ARRAY OF Value, n);
    i.op       := Op.AtomicStore;
    ops[0]     := value;
    ops[1]     := ptr;
    IF container # NIL THEN ops[2] := container END;
    i.operands := ops;
    i.memOrder := order;
    i.result   := NIL;
    addInsn(b, i);
  END BuildAtomicStore;

PROCEDURE BuildAtomicRMW(b: Block; name: TEXT; op: AtomicRMWOp; ptr: Value;
                          val: Value; order: MemOrder;
                          container: Value := NIL): Value =
  VAR i        := NEW(Insn);
      elemType := ValueType(val);
      n        : INTEGER;
      ops      : REF ARRAY OF Value;
  BEGIN
    IF container = NIL THEN n := 2 ELSE n := 3 END;
    ops          := NEW(REF ARRAY OF Value, n);
    i.op         := Op.AtomicRMW;
    ops[0]       := ptr;
    ops[1]       := val;
    IF container # NIL THEN ops[2] := container END;
    i.operands   := ops;
    i.atomicOp   := op;
    i.targetType := elemType;
    i.memOrder   := order;
    i.result     := makeResult(b, elemType, name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildAtomicRMW;

PROCEDURE BuildAtomicCmpXchg(b: Block; name: TEXT; elemType: T; varPtr: Value;
                              expectedPtr: Value; desired: Value;
                              succOrder: MemOrder; failOrder: MemOrder;
                              container: Value := NIL): Value =
  VAR i    := NEW(Insn);
      n    : INTEGER;
      ops  : REF ARRAY OF Value;
  BEGIN
    IF container = NIL THEN n := 3 ELSE n := 4 END;
    ops          := NEW(REF ARRAY OF Value, n);
    i.op         := Op.AtomicCmpXchg;
    ops[0]       := varPtr;
    ops[1]       := expectedPtr;
    ops[2]       := desired;
    IF container # NIL THEN ops[3] := container END;
    i.operands   := ops;
    i.targetType := elemType;
    i.memOrder   := succOrder;
    i.memOrder2  := failOrder;
    i.result     := makeResult(b, TI1(), name, i);  (* returns i1 success flag *)
    addInsn(b, i);
    RETURN i.result;
  END BuildAtomicCmpXchg;

PROCEDURE buildCast(b: Block;  op: Op;  name: TEXT;  x: Value;  dstType: T): Value =
  VAR
    i   := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 1);
  BEGIN
    i.op      := op;
    ops[0]    := x;
    i.operands := ops;
    i.result  := makeResult(b, dstType, name, i);
    addInsn(b, i);
    RETURN i.result;
  END buildCast;

PROCEDURE BuildSIToFP (b: Block; name: TEXT; x: Value; dstType: T): Value =
  BEGIN RETURN buildCast(b, Op.SIToFP,  name, x, dstType) END BuildSIToFP;
PROCEDURE BuildFPToSI (b: Block; name: TEXT; x: Value; dstType: T): Value =
  BEGIN RETURN buildCast(b, Op.FPToSI,  name, x, dstType) END BuildFPToSI;
PROCEDURE BuildFPExt  (b: Block; name: TEXT; x: Value; dstType: T): Value =
  BEGIN RETURN buildCast(b, Op.FPExt,   name, x, dstType) END BuildFPExt;
PROCEDURE BuildFPTrunc(b: Block; name: TEXT; x: Value; dstType: T): Value =
  BEGIN RETURN buildCast(b, Op.FPTrunc, name, x, dstType) END BuildFPTrunc;
PROCEDURE BuildZExt   (b: Block; name: TEXT; x: Value; dstType: T): Value =
  BEGIN
    IF Equal(x.type, dstType) THEN RETURN x END;
    (* Same-width different-sign (e.g. W8→I8): no LLVM cast needed; retype. *)
    IF BitWidth(x.type) = BitWidth(dstType) THEN RETURN RetypeValue(x, dstType) END;
    RETURN buildCast(b, Op.ZExt,    name, x, dstType)
  END BuildZExt;
PROCEDURE BuildSExt   (b: Block; name: TEXT; x: Value; dstType: T): Value =
  BEGIN
    IF Equal(x.type, dstType) THEN RETURN x END;
    IF BitWidth(x.type) = BitWidth(dstType) THEN RETURN RetypeValue(x, dstType) END;
    RETURN buildCast(b, Op.SExt,    name, x, dstType)
  END BuildSExt;
PROCEDURE BuildTrunc  (b: Block; name: TEXT; x: Value; dstType: T): Value =
  BEGIN
    IF Equal(x.type, dstType) THEN RETURN x END;
    IF BitWidth(x.type) = BitWidth(dstType) THEN RETURN RetypeValue(x, dstType) END;
    RETURN buildCast(b, Op.Trunc,   name, x, dstType)
  END BuildTrunc;

PROCEDURE BuildFPFloor    (b: Block; name: TEXT; x: Value): Value =
  BEGIN RETURN buildCast(b, Op.FPFloor,     name, x, x.type) END BuildFPFloor;
PROCEDURE BuildFPCeil     (b: Block; name: TEXT; x: Value): Value =
  BEGIN RETURN buildCast(b, Op.FPCeil,      name, x, x.type) END BuildFPCeil;
PROCEDURE BuildFPRound    (b: Block; name: TEXT; x: Value): Value =
  BEGIN RETURN buildCast(b, Op.FPRound,     name, x, x.type) END BuildFPRound;
PROCEDURE BuildFPAbs      (b: Block; name: TEXT; x: Value): Value =
  BEGIN RETURN buildCast(b, Op.FPAbs,       name, x, x.type) END BuildFPAbs;

PROCEDURE BuildLoad(b: Block; name: TEXT; type: T; addr: Value): Value =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 1);
  BEGIN
    i.op := Op.Load;
    ops[0] := addr;
    i.operands := ops;
    i.result := makeResult(b, type, name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildLoad;

PROCEDURE BuildStore(b: Block; value: Value; addr: Value) =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 2);
  BEGIN
    <* ASSERT value # NIL *>
    <* ASSERT addr  # NIL *>
    i.op := Op.Store;
    ops[0] := value; ops[1] := addr;
    i.operands := ops;
    addInsn(b, i);
  END BuildStore;

PROCEDURE copyArgs(READONLY args: ARRAY OF Value): REF ARRAY OF Value =
  VAR r := NEW(REF ARRAY OF Value, NUMBER(args));
  BEGIN
    FOR k := FIRST(args) TO LAST(args) DO r[k] := args[k] END;
    RETURN r;
  END copyArgs;

PROCEDURE BuildBr(b: Block; target: Block;
                  READONLY args: ARRAY OF Value) =
  VAR i := NEW(Insn);
  BEGIN
    i.op := Op.Br;
    i.br0Tgt  := target;
    i.br0Args := copyArgs(args);
    addInsn(b, i);
  END BuildBr;

PROCEDURE BuildCondBr(b: Block; cond: Value;
                      ifTrue: Block;   READONLY trueArgs:  ARRAY OF Value;
                      ifFalse: Block;  READONLY falseArgs: ARRAY OF Value) =
  (* LLVM requires i1 for `br i1`. Auto-truncate any wider integer (e.g. i64
     from a widened BOOLEAN variable) to i1 before emitting the branch. *)
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 1);
    condI1 := cond;
  BEGIN
    IF Kind(ValueType(cond)) # TypeKind.I1 THEN
      condI1 := BuildTrunc(b, "", cond, TI1());
    END;
    i.op := Op.CondBr;
    ops[0] := condI1;
    i.operands := ops;
    i.br0Tgt  := ifTrue;
    i.br0Args := copyArgs(trueArgs);
    i.br1Tgt  := ifFalse;
    i.br1Args := copyArgs(falseArgs);
    addInsn(b, i);
  END BuildCondBr;

PROCEDURE BuildRet(b: Block; value: Value) =
  VAR i := NEW(Insn);
  BEGIN
    i.op := Op.Ret;
    IF value # NIL THEN
      i.operands := NEW(REF ARRAY OF Value, 1);
      i.operands[0] := value;
    END;
    addInsn(b, i);
  END BuildRet;

PROCEDURE BuildUnreachable(b: Block) =
  VAR i := NEW(Insn);
  BEGIN
    i.op := Op.Unreachable;
    addInsn(b, i);
  END BuildUnreachable;

PROCEDURE BuildCall(b: Block; name: TEXT; callee: Proc;
                    READONLY args: ARRAY OF Value): Value =
  VAR i := NEW(Insn);
  BEGIN
    i.op := Op.Call;
    i.callee := callee;
    i.operands := copyArgs(args);
    IF Kind(callee.result) # TypeKind.Void THEN
      i.result := makeResult(b, callee.result, name, i);
    END;
    addInsn(b, i);
    RETURN i.result;
  END BuildCall;

(*-------------------------------------------------- Object / RTTI builders *)

PROCEDURE BuildAlloca(b: Block;  name: TEXT;  type: T): Value =
  VAR i := NEW(Insn);  entry := b;
  BEGIN
    IF b = NIL THEN RETURN NIL END;
    i.op := Op.Alloca;
    i.targetType := type;
    i.extractIdx := 1;   (* count = 1 initially *)
    (* Hoist the alloca to the entry block.  A fixed-size alloca depends only on
       its type, so it is safe to define once at function entry — and necessary:
       an alloca left in a loop body reserves fresh stack on EVERY iteration
       (LLVM does not reclaim it until the function returns), overflowing the
       stack in long-running loops (p227: a ~4M-iteration loop with per-body
       temps SIGSEGV'd).  Stores/loads through the pointer stay in place; only
       the stack reservation moves.  Prepend so it precedes the entry block's
       terminator. *)
    IF b.proc # NIL AND b.proc.items.size() > 0
       AND ISTYPE(b.proc.items.get(0), Block) THEN
      entry := NARROW(b.proc.items.get(0), Block);
    END;
    i.result := makeResult(entry, TPtr(type), name, i);
    i.srcLine := currentSrcLine;
    i.block := entry;
    entry.insns.addlo(i);
    RETURN i.result;
  END BuildAlloca;

PROCEDURE BuildAllocaDyn(b: Block;  name: TEXT;  byteCount: Value): Value =
  VAR i    := NEW(Insn);
      ops  := NEW(REF ARRAY OF Value, 1);
  BEGIN
    IF b = NIL THEN RETURN NIL END;
    ops[0]       := byteCount;
    i.op         := Op.AllocaDyn;
    i.operands   := ops;
    i.result     := makeResult(b, TPtr(TVoid()), name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildAllocaDyn;

PROCEDURE AllocaSetCount(v: Value;  count: INTEGER) =
  (* v must be the result of BuildAlloca.  Patches extractIdx to the new count. *)
  BEGIN
    IF v = NIL OR v.insn = NIL OR v.insn.op # Op.Alloca THEN RETURN END;
    v.insn.extractIdx := MAX(1, count);
  END AllocaSetCount;

PROCEDURE BuildGcLoad(b: Block;  name: TEXT;  slot: Value): Value =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 1);
    slotT := slot.type;
    eltT  : T := NIL;
  BEGIN
    <* ASSERT slotT # NIL AND Kind(slotT) = TypeKind.GcSlot,
       "BuildGcLoad: operand must be gc_slot" *>
    eltT := EltType(slotT);
    i.op := Op.GcLoad;
    ops[0] := slot;
    i.operands := ops;
    i.result := makeResult(b, TGcRef(eltT), name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildGcLoad;

PROCEDURE BuildGcStore(b: Block;  slot: Value;  value: Value;
                        container: Value := NIL) =
  VAR
    i    := NEW(Insn);
    nOps := 2 + (ORD(container # NIL));
    ops  := NEW(REF ARRAY OF Value, nOps);
  BEGIN
    <* ASSERT Kind(slot.type) = TypeKind.GcSlot,
       "BuildGcStore: slot operand must be gc_slot" *>
    i.op := Op.GcStore;
    (* ops[0]=value, ops[1]=slot — consistent with Store.
       ops[2]=container (the heap object), present only when non-NIL. *)
    ops[0] := value;
    ops[1] := slot;
    IF container # NIL THEN ops[2] := container END;
    i.operands := ops;
    addInsn(b, i);
  END BuildGcStore;

PROCEDURE objectOrStruct(t: T): T =
  (* Strip a single layer of GcRef/Ptr/GcSlot to expose the underlying
     Object or Struct type for field lookup. *)
  BEGIN
    CASE Kind(t) OF
    | TypeKind.GcRef, TypeKind.Ptr, TypeKind.GcSlot => RETURN EltType(t);
    ELSE RETURN t
    END;
  END objectOrStruct;

PROCEDURE fieldType(objT: T;  name: TEXT): T =
  VAR cur := objT;
  BEGIN
    WHILE cur # NIL DO
      IF cur.fields # NIL THEN
        FOR i := 0 TO LAST(cur.fields^) DO
          IF Text.Equal(cur.fields[i].name, name) THEN
            RETURN cur.fields[i].type;
          END;
        END;
      END;
      IF Kind(cur) = TypeKind.Object THEN cur := cur.objSuper
                                     ELSE cur := NIL END;
    END;
    RETURN NIL;
  END fieldType;

PROCEDURE BuildFieldAddr(b: Block;  name: TEXT;
                         obj: Value;  fieldName: TEXT): Value =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 1);
    objT := objectOrStruct(obj.type);
    ftype := fieldType(objT, fieldName);
  BEGIN
    <* ASSERT ftype # NIL, "BuildFieldAddr: field not found" *>
    i.op := Op.FieldAddr;
    i.selector := fieldName;
    ops[0] := obj;
    i.operands := ops;
    i.result := makeResult(b, TPtr(ftype), name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildFieldAddr;

PROCEDURE BuildArrayElemAddr(b: Block;  name: TEXT;
                             arr: Value;  idx: Value): Value =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 2);
    arrT := arr.type;
    arrayT: T := NIL;
    eltT:   T := NIL;
  BEGIN
    <* ASSERT arrT # NIL, "BuildArrayElemAddr: arr has no type" *>
    (* Accept either `ptr FixedArray` (alloca'd local) or a FixedArray
       value (e.g., VAR-mode formal whose value already represents the
       address). *)
    IF Kind(arrT) = TypeKind.Ptr THEN
      arrayT := arrT.elt;
    ELSE
      arrayT := arrT;
    END;
    <* ASSERT Kind(arrayT) = TypeKind.FixedArray,
       "BuildArrayElemAddr: arr must be FixedArray or ptr FixedArray" *>
    eltT := arrayT.elt;
    i.op := Op.ArrayElemAddr;
    ops[0] := arr;
    ops[1] := idx;
    i.operands := ops;
    i.result := makeResult(b, TPtr(eltT), name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildArrayElemAddr;

PROCEDURE BuildNew(b: Block;  name: TEXT;  type: T): Value =
  VAR i := NEW(Insn);
  BEGIN
    <* ASSERT Kind(type) = TypeKind.Object,
       "BuildNew: type must be Object" *>
    i.op := Op.New;
    i.targetType := type;
    i.result := makeResult(b, TGcRef(type), name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildNew;

PROCEDURE BuildDispatch(b: Block;  name: TEXT;
                        obj: Value;  methodName: TEXT;
                        READONLY args: ARRAY OF Value): Value =
  VAR
    i := NEW(Insn);
    objT := objectOrStruct(obj.type);
    midx: INTEGER;
    cur: T;
    methodSig: T := NIL;
    resultT: T := NIL;
    nArgs := NUMBER(args);
    ops := NEW(REF ARRAY OF Value, 1 + nArgs);
  BEGIN
    <* ASSERT Kind(objT) = TypeKind.Object,
       "BuildDispatch: object operand must be Object-typed" *>
    midx := ObjectMethodIndex(objT, methodName);
    <* ASSERT midx >= 0, "BuildDispatch: method not found in type chain" *>
    (* find the actual method record in the chain to get its signature *)
    cur := objT;
    WHILE cur # NIL AND methodSig = NIL DO
      IF cur.objMethods # NIL THEN
        FOR k := 0 TO LAST(cur.objMethods^) DO
          IF Text.Equal(cur.objMethods[k].name, methodName) THEN
            methodSig := cur.objMethods[k].procType;
            EXIT;
          END;
        END;
      END;
      cur := cur.objSuper;
    END;
    <* ASSERT methodSig # NIL *>
    resultT := methodSig.procResult;

    i.op := Op.Dispatch;
    i.selector := methodName;
    ops[0] := obj;
    FOR k := 0 TO nArgs - 1 DO ops[1 + k] := args[k] END;
    i.operands := ops;
    IF Kind(resultT) # TypeKind.Void THEN
      i.result := makeResult(b, resultT, name, i);
    END;
    addInsn(b, i);
    RETURN i.result;
  END BuildDispatch;

PROCEDURE BuildNarrow(b: Block;  name: TEXT;
                      obj: Value;  targetType: T): Value =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 1);
  BEGIN
    i.op := Op.Narrow;
    i.targetType := targetType;
    ops[0] := obj;
    i.operands := ops;
    i.result := makeResult(b, TGcRef(targetType), name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildNarrow;

PROCEDURE BuildIstype(b: Block;  name: TEXT;
                      obj: Value;  targetType: T): Value =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 1);
  BEGIN
    i.op := Op.Istype;
    i.targetType := targetType;
    ops[0] := obj;
    i.operands := ops;
    i.result := makeResult(b, TI1(), name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildIstype;

PROCEDURE BuildTypecase(b: Block;  value: Value;
                        clauses: REF ARRAY OF TypecaseClause) =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 1);
    sawElse := FALSE;
  BEGIN
    i.op := Op.Typecase;
    ops[0] := value;
    i.operands := ops;
    FOR k := 0 TO LAST(clauses^) DO
      IF clauses[k].isElse THEN sawElse := TRUE END;
    END;
    <* ASSERT sawElse, "BuildTypecase: ELSE clause is mandatory" *>
    i.typecaseClauses := clauses;
    addInsn(b, i);
  END BuildTypecase;

(*--------------------------------------------------- EH builders / control *)

PROCEDURE BuildInvoke(b: Block;  name: TEXT;  callee: Proc;
                      READONLY args: ARRAY OF Value;
                      normalBlock: Block;  unwindBlock: Block): Value =
  VAR i := NEW(Insn);
  BEGIN
    i.op      := Op.Invoke;
    i.callee  := callee;
    i.operands := copyArgs(args);
    i.br0Tgt  := normalBlock;
    i.br1Tgt  := unwindBlock;
    IF callee.result # NIL AND Kind(callee.result) # TypeKind.Void THEN
      i.result := makeResult(b, callee.result, name, i);
    END;
    addInsn(b, i);
    RETURN i.result;
  END BuildInvoke;

PROCEDURE BuildLandingPad(b: Block;  name: TEXT;  isCleanup: BOOLEAN): Value =
  VAR i := NEW(Insn);
  BEGIN
    i.op        := Op.LandingPad;
    i.isCleanup := isCleanup;
    i.result    := makeResult(b, TLandingPad(), name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildLandingPad;

PROCEDURE BuildExtractValue(b: Block;  name: TEXT;
                             aggregate: Value;  idx: INTEGER): Value =
  VAR
    i    := NEW(Insn);
    ops  := NEW(REF ARRAY OF Value, 1);
    aggT := aggregate.type;
    fldT : T;
  BEGIN
    <* ASSERT aggT # NIL AND Kind(aggT) = TypeKind.Struct,
       "BuildExtractValue: aggregate must have Struct type" *>
    fldT := StructField(aggT, idx).type;
    i.op         := Op.ExtractValue;
    i.extractIdx := idx;
    ops[0]       := aggregate;
    i.operands   := ops;
    i.result     := makeResult(b, fldT, name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildExtractValue;

PROCEDURE BuildResume(b: Block;  lp: Value) =
  VAR
    i   := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 1);
  BEGIN
    i.op       := Op.Resume;
    ops[0]     := lp;
    i.operands := ops;
    addInsn(b, i);
  END BuildResume;

PROCEDURE BuildPtrAdd(b: Block;  name: TEXT;  base: Value;  idx: INTEGER): Value =
  VAR i := NEW(Insn);
  BEGIN
    i.op         := Op.PtrAdd;
    i.operands   := NEW(REF ARRAY OF Value, 1);
    i.operands[0] := base;
    i.extractIdx := idx;   (* reuse extractIdx for the constant index *)
    (* Byte arithmetic yields an opaque pointer; element type is unknown. *)
    i.result     := makeResult(b, TPtr(TVoid()), name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildPtrAdd;

PROCEDURE BuildGepByte(b: Block;  name: TEXT;  base, offset: Value): Value =
  VAR i := NEW(Insn);
      ops := NEW(REF ARRAY OF Value, 2);
  BEGIN
    i.op := Op.GepByte;
    ops[0] := base;
    ops[1] := offset;
    i.operands := ops;
    i.result   := makeResult(b, TPtr(TVoid()), name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildGepByte;

PROCEDURE BuildCallIndirect(b: Block;  name: TEXT;  fn: Value;  rtype: T;
                             READONLY args: ARRAY OF Value): Value =
  VAR
    i    := NEW(Insn);
    nOps := 1 + NUMBER(args);
    ops  := NEW(REF ARRAY OF Value, nOps);
  BEGIN
    i.op       := Op.CallIndirect;
    ops[0]     := fn;    (* ops[0] = function pointer *)
    FOR k := 0 TO NUMBER(args) - 1 DO ops[1 + k] := args[k] END;
    i.operands  := ops;
    i.targetType := rtype;   (* return type stored in targetType *)
    IF rtype # NIL AND Kind(rtype) # TypeKind.Void THEN
      i.result := makeResult(b, rtype, name, i);
    END;
    addInsn(b, i);
    RETURN i.result;
  END BuildCallIndirect;

PROCEDURE BuildInvokeIndirect(b: Block;  name: TEXT;  fn: Value;  rtype: T;
                               READONLY args: ARRAY OF Value;
                               normalBlock: Block;  unwindBlock: Block): Value =
  VAR
    i    := NEW(Insn);
    nOps := 1 + NUMBER(args);
    ops  := NEW(REF ARRAY OF Value, nOps);
  BEGIN
    i.op       := Op.InvokeIndirect;
    ops[0]     := fn;
    FOR k := 0 TO NUMBER(args) - 1 DO ops[1 + k] := args[k] END;
    i.operands   := ops;
    i.targetType  := rtype;
    i.br0Tgt     := normalBlock;
    i.br1Tgt     := unwindBlock;
    IF rtype # NIL AND Kind(rtype) # TypeKind.Void THEN
      i.result := makeResult(b, rtype, name, i);
    END;
    addInsn(b, i);
    RETURN i.result;
  END BuildInvokeIndirect;

PROCEDURE BuildRaise(b: Block;  exceptionSym: TEXT;  value: Value) =
  VAR i := NEW(Insn);
  BEGIN
    i.op := Op.Raise;
    i.selector := exceptionSym;
    IF value # NIL THEN
      i.operands := NEW(REF ARRAY OF Value, 1);
      i.operands[0] := value;
    END;
    addInsn(b, i);
  END BuildRaise;

PROCEDURE BuildUnwindTo(b: Block;  target: Block;
                        READONLY args: ARRAY OF Value) =
  VAR i := NEW(Insn);
  BEGIN
    i.op := Op.UnwindTo;
    i.br0Tgt  := target;
    i.br0Args := copyArgs(args);
    addInsn(b, i);
  END BuildUnwindTo;

PROCEDURE BuildRetThroughEnvelope(b: Block;  value: Value) =
  VAR i := NEW(Insn);
  BEGIN
    i.op := Op.RetThroughEnvelope;
    IF value # NIL THEN
      i.operands := NEW(REF ARRAY OF Value, 1);
      i.operands[0] := value;
    END;
    addInsn(b, i);
  END BuildRetThroughEnvelope;

(*-------------------------------------- Open arrays / runtime-check builders *)

PROCEDURE BuildOpenArraySize(b: Block;  name: TEXT;
                             oa: Value;  dim: INTEGER): Value =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 2);
    dimVal := ConstInt(TI(Target.IntegerSize()), dim);
  BEGIN
    <* ASSERT Kind(oa.type) = TypeKind.OpenArray,
       "BuildOpenArraySize: operand must be openarray" *>
    <* ASSERT 0 <= dim AND dim < oa.type.openArrayRank,
       "BuildOpenArraySize: dim out of range" *>
    i.op := Op.OpenArraySize;
    ops[0] := oa;
    ops[1] := dimVal;
    i.operands := ops;
    i.result := makeResult(b, TI(Target.IntegerSize()), name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildOpenArraySize;

PROCEDURE BuildOpenArrayElemAddr(b: Block;  name: TEXT;
                                 oa: Value;
                                 READONLY indices: ARRAY OF Value): Value =
  VAR
    i := NEW(Insn);
    nIdx := NUMBER(indices);
    ops := NEW(REF ARRAY OF Value, 1 + nIdx);
  BEGIN
    <* ASSERT Kind(oa.type) = TypeKind.OpenArray,
       "BuildOpenArrayElemAddr: operand must be openarray" *>
    <* ASSERT nIdx = oa.type.openArrayRank,
       "BuildOpenArrayElemAddr: index count must equal rank" *>
    i.op := Op.OpenArrayElemAddr;
    ops[0] := oa;
    FOR k := 0 TO nIdx - 1 DO ops[1 + k] := indices[k] END;
    i.operands := ops;
    i.result := makeResult(b, TPtr(oa.type.elt), name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildOpenArrayElemAddr;

PROCEDURE BuildSubarray(b: Block;  name: TEXT;
                        oa: Value;  lo, count: Value): Value =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 3);
  BEGIN
    <* ASSERT Kind(oa.type) = TypeKind.OpenArray,
       "BuildSubarray: operand must be openarray" *>
    <* ASSERT oa.type.openArrayRank = 1,
       "BuildSubarray: M3 SUBARRAY is 1D only" *>
    i.op := Op.Subarray;
    ops[0] := oa;  ops[1] := lo;  ops[2] := count;
    i.operands := ops;
    i.result := makeResult(b, oa.type, name, i);   (* same openarray<1> elt *)
    addInsn(b, i);
    RETURN i.result;
  END BuildSubarray;

PROCEDURE BuildSubscriptCheck(b: Block;  idx, bound: Value) =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 2);
  BEGIN
    i.op := Op.SubscriptCheck;
    ops[0] := idx; ops[1] := bound;
    i.operands := ops;
    addInsn(b, i);
  END BuildSubscriptCheck;

PROCEDURE BuildNilCheck(b: Block;  ref: Value) =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 1);
  BEGIN
    i.op := Op.NilCheck;
    ops[0] := ref;
    i.operands := ops;
    addInsn(b, i);
  END BuildNilCheck;

PROCEDURE BuildRangeCheck(b: Block;  value, lo, hi: Value) =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 3);
  BEGIN
    i.op := Op.RangeCheck;
    ops[0] := value;  ops[1] := lo;  ops[2] := hi;
    i.operands := ops;
    addInsn(b, i);
  END BuildRangeCheck;

(*--------------------------------------------- Conversion / set builders *)

PROCEDURE BuildConvert(b: Block;  name: TEXT;
                       value: Value;  targetType: T): Value =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 1);
  BEGIN
    i.op := Op.Convert;
    i.targetType := targetType;
    ops[0] := value;
    i.operands := ops;
    i.result := makeResult(b, targetType, name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildConvert;

PROCEDURE buildSetBin(b: Block;  op: Op;  name: TEXT;  x, y: Value): Value =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 2);
  BEGIN
    <* ASSERT Equal(x.type, y.type),
       "set binary op: operand type mismatch" *>
    <* ASSERT Kind(x.type) = TypeKind.Set,
       "set binary op: operands must have set type" *>
    i.op := op;
    ops[0] := x;  ops[1] := y;
    i.operands := ops;
    i.result := makeResult(b, x.type, name, i);
    addInsn(b, i);
    RETURN i.result;
  END buildSetBin;

PROCEDURE BuildSetUnion(b: Block;  name: TEXT;  x, y: Value): Value =
  BEGIN RETURN buildSetBin(b, Op.SetUnion, name, x, y) END BuildSetUnion;
PROCEDURE BuildSetIntersect(b: Block;  name: TEXT;  x, y: Value): Value =
  BEGIN RETURN buildSetBin(b, Op.SetIntersect, name, x, y) END BuildSetIntersect;
PROCEDURE BuildSetDifference(b: Block;  name: TEXT;  x, y: Value): Value =
  BEGIN RETURN buildSetBin(b, Op.SetDifference, name, x, y) END BuildSetDifference;

PROCEDURE BuildSetMember(b: Block;  name: TEXT;
                         set: Value;  elt: Value): Value =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 2);
  BEGIN
    <* ASSERT Kind(set.type) = TypeKind.Set,
       "BuildSetMember: first operand must be set" *>
    i.op := Op.SetMember;
    ops[0] := set;  ops[1] := elt;
    i.operands := ops;
    i.result := makeResult(b, TI1(), name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildSetMember;

PROCEDURE BuildOpenArrayNew(b: Block;  name: TEXT;
                            elt: T;
                            READONLY sizes: ARRAY OF Value): Value =
  VAR
    i    := NEW(Insn);
    n    := NUMBER(sizes);
    ops  := NEW(REF ARRAY OF Value, n);
    heap := THeapArray(n, elt);
  BEGIN
    <* ASSERT n >= 1, "BuildOpenArrayNew: at least one size required" *>
    i.op := Op.OpenArrayNew;
    i.targetType := elt;                 (* element type for printer *)
    FOR k := 0 TO n - 1 DO ops[k] := sizes[k] END;
    i.operands := ops;
    i.result := makeResult(b, TGcRef(heap), name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildOpenArrayNew;

PROCEDURE BuildOpenArrayDeref(b: Block;  name: TEXT;  ref: Value): Value =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 1);
    refT := ref.type;  heapT: T;  resultT: T;
  BEGIN
    <* ASSERT Kind(refT) = TypeKind.GcRef,
       "BuildOpenArrayDeref: operand must be gc_ref" *>
    heapT := EltType(refT);
    <* ASSERT Kind(heapT) = TypeKind.HeapArray,
       "BuildOpenArrayDeref: operand must be gc_ref to a heap array" *>
    resultT := TOpenArray(heapT.openArrayRank, heapT.elt);
    i.op := Op.OpenArrayDeref;
    ops[0] := ref;
    i.operands := ops;
    i.result := makeResult(b, resultT, name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildOpenArrayDeref;

PROCEDURE BlockIsTerminated(b: Block): BOOLEAN =
  VAR n := BlockInsnCount(b);  ri: Insn;
  BEGIN
    IF n = 0 THEN RETURN FALSE END;
    ri := BlockInsn(b, n-1);
    CASE ri.op OF
    | Op.Ret, Op.Br, Op.CondBr, Op.Unreachable,
      Op.UnwindTo, Op.RetThroughEnvelope, Op.Typecase,
      Op.Resume, Op.Invoke, Op.InvokeIndirect =>
        RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END BlockIsTerminated;

BEGIN
END MSIR.
