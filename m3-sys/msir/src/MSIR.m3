(* See MSIR.i3 *)

MODULE MSIR;

IMPORT RefSeq, Fmt, Text;

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
  subrLo, subrHi:   LONGINT              := 0L;   (* Subrange / Set bounds *)
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
    ELSE  <* ASSERT FALSE, "TI: invalid bit width" *>
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

PROCEDURE TSubrange(parent: T;  lo, hi: LONGINT): T =
  VAR t := NewType(TypeKind.Subrange);
  BEGIN
    t.elt := parent;
    t.subrLo := lo;
    t.subrHi := hi;
    RETURN t;
  END TSubrange;

PROCEDURE TSet(elt: T;  lo, hi: LONGINT): T =
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
    IF a.kind # b.kind THEN RETURN FALSE END;
    CASE a.kind OF
    | TypeKind.Ptr, TypeKind.GcRef, TypeKind.GcSlot =>
        RETURN Equal(a.elt, b.elt);
    | TypeKind.Struct =>
        RETURN Text.Equal(a.structName, b.structName);
    | TypeKind.Object =>
        RETURN Text.Equal(a.structName, b.structName);
    | TypeKind.OpenArray, TypeKind.HeapArray =>
        IF a.openArrayRank # b.openArrayRank THEN RETURN FALSE END;
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
      TypeKind.W8, TypeKind.W16, TypeKind.W32, TypeKind.W64,
      TypeKind.F32, TypeKind.F64, TypeKind.F128 => RETURN t.bits;
    ELSE RETURN -1
    END;
  END BitWidth;

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

PROCEDURE SubrangeParent(t: T): T   = BEGIN RETURN t.elt    END SubrangeParent;
PROCEDURE SubrangeLo(t: T): LONGINT = BEGIN RETURN t.subrLo END SubrangeLo;
PROCEDURE SubrangeHi(t: T): LONGINT = BEGIN RETURN t.subrHi END SubrangeHi;
PROCEDURE SetElt(t: T): T           = BEGIN RETURN t.elt    END SetElt;
PROCEDURE SetLo(t: T): LONGINT      = BEGIN RETURN t.subrLo END SetLo;
PROCEDURE SetHi(t: T): LONGINT      = BEGIN RETURN t.subrHi END SetHi;

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
  type:      T          := NIL;
  name:      TEXT       := NIL;
  vKind:     ValueKind;
  intVal:    LONGINT    := 0L;
  proc:      Proc       := NIL;     (* Param *)
  paramIdx:  INTEGER    := -1;
  block:     Block      := NIL;     (* BlockParam *)
  bparamIdx: INTEGER    := -1;
  insn:      Insn       := NIL;     (* InsnResult *)
END;

PROCEDURE ConstInt(t: T;  v: LONGINT): Value =
  VAR
    val := NEW(Value);
  BEGIN
    val.type   := t;
    val.vKind  := ValueKind.ConstInt;
    val.intVal := v;
    val.name   := Fmt.LongInt(v);
    RETURN val;
  END ConstInt;

PROCEDURE ConstBool(v: BOOLEAN): Value =
  BEGIN
    IF v THEN RETURN ConstInt(TI1(), 1L) ELSE RETURN ConstInt(TI1(), 0L) END;
  END ConstBool;

PROCEDURE ConstNil(t: T): Value =
  VAR val := NEW(Value);
  BEGIN
    val.type  := t;
    val.vKind := ValueKind.ConstNil;
    val.name  := "nil";
    RETURN val;
  END ConstNil;

PROCEDURE ValueType(v: Value): T = BEGIN RETURN v.type END ValueType;
PROCEDURE ValueName(v: Value): TEXT = BEGIN RETURN v.name END ValueName;

PROCEDURE GetValueKind(v: Value): ValueKind = BEGIN RETURN v.vKind END GetValueKind;
PROCEDURE GetIntVal(v: Value): LONGINT = BEGIN RETURN v.intVal END GetIntVal;

(*------------------------------------------------------------------- Insn *)

REVEAL Insn = BRANDED "MSIR.Insn" REF RECORD
  op:        Op;
  block:     Block := NIL;
  result:    Value := NIL;
  operands:  REF ARRAY OF Value := NIL;

  (* opcode-specific extras *)
  cmpPred:   CmpPred;
  br0Tgt:    Block := NIL;
  br0Args:   REF ARRAY OF Value := NIL;
  br1Tgt:    Block := NIL;
  br1Args:   REF ARRAY OF Value := NIL;
  callee:    Proc  := NIL;
  targetType: T    := NIL;        (* Alloca, New, Narrow, Istype, Convert *)
  selector:   TEXT := NIL;        (* Dispatch method name; FieldAddr field name *)
  typecaseClauses: REF ARRAY OF TypecaseClause := NIL;
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

PROCEDURE ProcName(p: Proc): TEXT = BEGIN RETURN p.name END ProcName;
PROCEDURE ProcParamCount(p: Proc): INTEGER =
  BEGIN
    IF p.params = NIL THEN RETURN 0 END;
    RETURN NUMBER(p.params^);
  END ProcParamCount;
PROCEDURE ProcParam(p: Proc; i: INTEGER): Value =
  BEGIN RETURN p.paramValues[i] END ProcParam;
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
  name:    TEXT;
  imports: RefSeq.T;                               (* elements: TEXT *)
  procs:   RefSeq.T;                               (* elements: Proc *)
  globals: RefSeq.T;                               (* elements: Global *)
END;

REVEAL Global = BRANDED "MSIR.Global" REF RECORD
  name:     TEXT;
  type:     T;
  isTraced: BOOLEAN;
  refValue: Value := NIL;
END;

PROCEDURE NewGlobal(name: TEXT;  type: T;  isTraced: BOOLEAN): Global =
  VAR
    g := NEW(Global);
    v := NEW(Value);
  BEGIN
    g.name     := name;
    g.type     := type;
    g.isTraced := isTraced;
    v.name  := name;
    v.vKind := ValueKind.GlobalRef;
    IF isTraced THEN
      v.type := TGcSlot(type);
    ELSE
      v.type := TPtr(type);
    END;
    g.refValue := v;
    RETURN g;
  END NewGlobal;

PROCEDURE GlobalName(g: Global): TEXT       = BEGIN RETURN g.name     END GlobalName;
PROCEDURE GlobalType(g: Global): T          = BEGIN RETURN g.type     END GlobalType;
PROCEDURE GlobalIsTraced(g: Global): BOOLEAN= BEGIN RETURN g.isTraced END GlobalIsTraced;
PROCEDURE GlobalValue(g: Global): Value     = BEGIN RETURN g.refValue END GlobalValue;

PROCEDURE ModuleAddGlobal(m: Module;  g: Global) =
  BEGIN m.globals.addhi(g) END ModuleAddGlobal;
PROCEDURE ModuleGlobalCount(m: Module): INTEGER =
  BEGIN RETURN m.globals.size() END ModuleGlobalCount;
PROCEDURE ModuleGlobal(m: Module;  i: INTEGER): Global =
  BEGIN RETURN m.globals.get(i) END ModuleGlobal;

PROCEDURE NewModule(name: TEXT): Module =
  VAR m := NEW(Module);
  BEGIN
    m.name    := name;
    m.imports := NEW(RefSeq.T).init();
    m.procs   := NEW(RefSeq.T).init();
    m.globals := NEW(RefSeq.T).init();
    RETURN m;
  END NewModule;

PROCEDURE ModuleName(m: Module): TEXT = BEGIN RETURN m.name END ModuleName;
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

PROCEDURE freshName(p: Proc): TEXT =
  BEGIN
    INC(p.nameCounter);
    RETURN "%" & Fmt.Int(p.nameCounter);
  END freshName;

PROCEDURE finalName(b: Block;  name: TEXT): TEXT =
  BEGIN
    IF Text.Length(name) = 0 THEN RETURN freshName(b.proc) END;
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
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 1);
  BEGIN
    i.op := Op.CondBr;
    ops[0] := cond;
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
  VAR i := NEW(Insn);
  BEGIN
    i.op := Op.Alloca;
    i.targetType := type;
    i.result := makeResult(b, TPtr(type), name, i);
    addInsn(b, i);
    RETURN i.result;
  END BuildAlloca;

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

PROCEDURE BuildGcStore(b: Block;  slot: Value;  value: Value) =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 2);
  BEGIN
    <* ASSERT Kind(slot.type) = TypeKind.GcSlot,
       "BuildGcStore: slot operand must be gc_slot" *>
    i.op := Op.GcStore;
    ops[0] := slot;
    ops[1] := value;
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
                        READONLY clauses: ARRAY OF TypecaseClause) =
  VAR
    i := NEW(Insn);
    ops := NEW(REF ARRAY OF Value, 1);
    n := NUMBER(clauses);
    cs := NEW(REF ARRAY OF TypecaseClause, n);
    sawElse := FALSE;
  BEGIN
    i.op := Op.Typecase;
    ops[0] := value;
    i.operands := ops;
    FOR k := 0 TO n - 1 DO
      cs[k] := clauses[k];
      IF clauses[k].isElse THEN sawElse := TRUE END;
    END;
    <* ASSERT sawElse, "BuildTypecase: ELSE clause is mandatory" *>
    i.typecaseClauses := cs;
    addInsn(b, i);
  END BuildTypecase;

(*--------------------------------------------------- EH builders / control *)

PROCEDURE BuildInvoke(b: Block;  name: TEXT;  callee: Proc;
                      READONLY args: ARRAY OF Value): Value =
  VAR i := NEW(Insn);
  BEGIN
    i.op := Op.Invoke;
    i.callee := callee;
    i.operands := copyArgs(args);
    IF Kind(callee.result) # TypeKind.Void THEN
      i.result := makeResult(b, callee.result, name, i);
    END;
    addInsn(b, i);
    RETURN i.result;
  END BuildInvoke;

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
    dimVal := ConstInt(TI(64), VAL(dim, LONGINT));
  BEGIN
    <* ASSERT Kind(oa.type) = TypeKind.OpenArray,
       "BuildOpenArraySize: operand must be openarray" *>
    <* ASSERT 0 <= dim AND dim < oa.type.openArrayRank,
       "BuildOpenArraySize: dim out of range" *>
    i.op := Op.OpenArraySize;
    ops[0] := oa;
    ops[1] := dimVal;
    i.operands := ops;
    i.result := makeResult(b, TI(64), name, i);
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
      Op.UnwindTo, Op.RetThroughEnvelope, Op.Typecase =>
        RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END BlockIsTerminated;

BEGIN
END MSIR.
