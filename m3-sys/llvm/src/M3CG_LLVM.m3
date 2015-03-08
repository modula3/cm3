(*      Add copyright notice      *)

UNSAFE MODULE M3CG_LLVM;

IMPORT LLVM;
FROM M3CG IMPORT Name, ByteOffset, TypeUID, CallingConvention;
FROM M3CG IMPORT BitSize, ByteSize, Alignment, Frequency;
FROM M3CG IMPORT Var, Proc, Label, Sign, BitOffset;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;
FROM M3CG IMPORT CompareOp, ConvertOp, AtomicOp, RuntimeError;
FROM M3CG IMPORT MemoryOrder;
IMPORT M3ID, M3Buf, M3CG, M3CG_Ops;
IMPORT Target, TInt, TFloat;
IMPORT Wr, IntRefTbl, RefSeq;
IMPORT Ctypes, M3toC, Fmt;
IMPORT Text, IO;

REVEAL

  U = Public BRANDED "M3CG_LLVM.T" OBJECT
    wr            : Wr.T := NIL;
    buf           : M3Buf.T := NIL;
    buf_len       : INTEGER := 0;
    abortFunc     : LLVM.ValueRef;
    
    exprStack     : RefSeq.T := NIL;
    callStack     : RefSeq.T := NIL;
    curVar        : LvVar;
    curProc       : LvProc;     

    structTypeTable : IntRefTbl.T := NIL; 
    procStack     : RefSeq.T := NIL;
    declStack     : RefSeq.T := NIL;
    labelTable     : IntRefTbl.T := NIL;
    
    next_label_id := 1;
    next_var      := 1;
    next_proc     := 1;
    next_scope    := 1;
    blockLevel    := 0;
    widecharSize  := 16;
METHODS
    ifCommon (t: IType; l: Label; f: Frequency; if_true : BOOLEAN) := IfCommon;
    buildFunc(p : Proc) := BuildFunc;
    getLabel(l : Label; name : TEXT) : LabelObj := GetLabel;
OVERRIDES    
    next_label := next_label;
    set_error_handler := set_error_handler;
    begin_unit := begin_unit;
    end_unit := end_unit;
    import_unit := import_unit;
    export_unit := export_unit;
    set_source_file := set_source_file;
    set_source_line := set_source_line;
    declare_typename := declare_typename;
    declare_array := declare_array;
    declare_open_array := declare_open_array;
    declare_enum := declare_enum;
    declare_enum_elt := declare_enum_elt;
    declare_packed := declare_packed;
    declare_record := declare_record;
    declare_field := declare_field;
    declare_set := declare_set;
    declare_subrange := declare_subrange;
    declare_pointer := declare_pointer;
    declare_indirect := declare_indirect;
    declare_proctype := declare_proctype;
    declare_formal := declare_formal;
    declare_raises := declare_raises;
    declare_object := declare_object;
    declare_method := declare_method;
    declare_opaque := declare_opaque;
    reveal_opaque := reveal_opaque;
    declare_exception := declare_exception;
    widechar_size := widechar_size; 
    set_runtime_proc := set_runtime_proc;
    import_global := import_global;
    declare_segment := declare_segment;
    bind_segment := bind_segment;
    declare_global := declare_global;
    declare_constant := declare_constant;
    declare_local := declare_local;
    declare_param := declare_param;
    declare_temp :=declare_temp;
    free_temp := free_temp;
    begin_init := begin_init;
    end_init := end_init;
    init_int := init_int;
    init_proc := init_proc;
    init_label := init_label;
    init_var := init_var;
    init_offset := init_offset;
    init_chars := init_chars;
    init_float := init_float;
    import_procedure := import_procedure;
    declare_procedure := declare_procedure;
    begin_procedure := begin_procedure;
    end_procedure := end_procedure;
    begin_block := begin_block;
    end_block := end_block;
    note_procedure_origin := note_procedure_origin;
    set_label := set_label;
    jump := jump;
    if_true := if_true;
    if_false := if_false;
    if_compare := if_compare;
    case_jump := case_jump;
    exit_proc := exit_proc;
    load := load;
    load_address := load_address;
    load_indirect := load_indirect;
    store := store;
    store_indirect := store_indirect;
    load_nil := load_nil;
    load_integer := load_integer;
    load_float := load_float;
    compare := compare;
    add := add;
    subtract := subtract;
    multiply := multiply;
    divide := divide;
    negate := negate;
    abs := abs;
    max := max;
    min := min;
    cvt_int := cvt_int;
    cvt_float := cvt_float;
    div := div;
    mod := mod;
    set_union := set_union;
    set_difference := set_difference;
    set_intersection := set_intersection;
    set_sym_difference := set_sym_difference;
    set_member := set_member;
    set_compare := set_compare;
    set_range := set_range;
    set_singleton := set_singleton;
    not := not;
    and := and;
    or := or;
    xor := xor;
    shift := shift;
    shift_left := shift_left;
    shift_right := shift_right;
    rotate := rotate;
    rotate_left := rotate_left;
    rotate_right := rotate_right;
    widen := widen;
    chop := chop;
    extract := extract;
    extract_n := extract_n;
    extract_mn := extract_mn;
    insert := insert;
    insert_n := insert_n;
    insert_mn := insert_mn;
    swap := swap;
    pop := pop;
    copy_n := copy_n;
    copy := copy;
    zero_n := zero_n;
    zero := zero;
    loophole := loophole;
    abort := abort;
    check_nil := check_nil;
    check_lo := check_lo;
    check_hi := check_hi;
    check_range := check_range;
    check_index := check_index;
    check_eq := check_eq;
    add_offset := add_offset;
    index_address := index_address;
    start_call_direct := start_call_direct;
    call_direct := call_direct;
    start_call_indirect := start_call_indirect;
    call_indirect := call_indirect;
    pop_param := pop_param;
    pop_struct := pop_struct;
    pop_static_link := pop_static_link;
    load_procedure := load_procedure;
    load_static_link := load_static_link;
    comment := comment;
    store_ordered := store_ordered;
    load_ordered := load_ordered;
    exchange := exchange;
    compare_exchange := compare_exchange;
    fence := fence;
    fetch_and_op := fetch_and_op;

    dumpLLVMIR := DumpLLVMIR;
  END;
  
TYPE 

  VarType = {Local,Global,Param,Temp};

  LvVar = Var OBJECT 
    tag: INTEGER;
    name : Name;
    size : ByteSize;
    type : Type;
    isConst : BOOLEAN;
    align : Alignment;
    varType : VarType;
    m3t : TypeUID;
    in_memory : BOOLEAN;
    up_level : BOOLEAN;
    lvType : LLVM.TypeRef;
    lv : LLVM.ValueRef;  (* llvm var definition *)
    inits : RefSeq.T;
  END;
  
  LvProc = Proc OBJECT 
    tag: INTEGER;
    name : Name;
    returnType : Type;
    numParams : CARDINAL;
    lev : INTEGER;
    cc : CallingConvention;
    exported : BOOLEAN := FALSE;
    lvProc : LLVM.ValueRef;  (* llvm procedure definition *)
    procTy : LLVM.TypeRef;    
    parent : LvProc := NIL;
    saveBB : LLVM.BasicBlockRef; (* for nested procs save the bb *)
    localStack  : RefSeq.T := NIL;
    paramStack  : RefSeq.T := NIL;
    imported : BOOLEAN := FALSE; (* if this is an import *)
    defined : BOOLEAN := FALSE; (* set when we build the declaration for real *)
  END;

  LvExpr = OBJECT
    lVal : LLVM.ValueRef;
    type : Type;
  END;

  LvStruct = OBJECT
    struct : LLVM.TypeRef;
  END;
  
  (* objects for the global segment *)
  
  BaseVar = OBJECT
    offset : INTEGER;
    size : INTEGER;
    type : Type;
    lvTy : LLVM.TypeRef;
    lvVal : LLVM.ValueRef;
  END;

  IntVar = BaseVar OBJECT
    value : Target.Int;
  END;

  FloatVar = BaseVar OBJECT
    value : Target.Float;
    prec : RType;
  END;

  TextVar = BaseVar OBJECT
    value : TEXT;
  END;

  VarVar = BaseVar OBJECT
    value : Var;
    bias : INTEGER;
  END;
  
  ProcVar = BaseVar OBJECT
    value : Proc;
  END;

  (* only for typecase *)
  FillerVar = BaseVar OBJECT
  END;
  
  (* objects for set label jmp and cmp *)

  BranchObj = OBJECT
    branchLVal : LLVM.ValueRef; (* not sure we need this not referenced use eval *)
    branchBB : LLVM.BasicBlockRef; (* the bb where the branch resides ie this is the bb where we found the jmp *)
  END;

  LabelObj = OBJECT
    labBB : LLVM.BasicBlockRef;  (* jmps goto this bb *)
    branchLVal : LLVM.ValueRef; (* when a branch is created store its value here but see above comment for same item *)

    cmpInstr : LLVM.ValueRef; (* saved cmp instr used to move the bb *)
    branchList : RefSeq.T; (* list of branches to this label *)
    
    elseBB : LLVM.BasicBlockRef; (* else bb for compares *)
    condLv : LLVM.ValueRef;      (* condtion branch lv could use eval and del *)
  END;

  (* template object for common if-then-else construction *)
  
  IfThenObj = OBJECT
    curProc : LvProc;
    cmpVal : LLVM.ValueRef;
    tmpLv : LLVM.ValueRef;
    curBB,thenBB,elseBB,exitBB : LLVM.BasicBlockRef;
    opName : TEXT;
  METHODS
    genInit() := GenInitSection;
    genThen() := GenThenSection;
    genExit() := GenExitSection;
  END;
  
VAR
  modRef : LLVM.ModuleRef;
  builderIR : LLVM.BuilderRef;
  globContext : LLVM.ContextRef;
  moduleID : Ctypes.const_char_star;
  faultVal : LLVM.ValueRef;  (* the abort function *)
  targetData : LLVM.TargetDataRef;
  
(* need to get these from front end somehow *)
  targetTriple := "x86_64-pc-linux-gnu";
  dataRep := "e-m:e-i64:64-f80:128-n8:16:32:64-S128";

  (* const once read *)
   ptrBytes : INTEGER; (* target pointer size in bytes *)
   ptrBits : LONGINT; (* target pointer size in bits *)
   IntPtrTy : LLVM.TypeRef; (* int type having same size as pointer *)
   PtrTy : LLVM.TypeRef; (*= LLVM.LLVMPointerType(IntPtrTy);*)
   AdrTy : LLVM.TypeRef; (* Pointer to i8 type *)
   AdrAdrTy : LLVM.TypeRef; (* Pointer to Pointer to i8 type *)
   
   i8Type := LLVM.LLVMInt8Type(); (* Byte type *)
   wordSize : LLVM.ValueRef; (* no of bits in word as llvm value *)
   byteSize : LLVM.ValueRef; (* no of bytes in word as llvm value *)

   (* external set functions *)
   setUnion : LLVM.ValueRef;
   setIntersection : LLVM.ValueRef;
   setDifference : LLVM.ValueRef;
   setSymDifference : LLVM.ValueRef;
   setMember : LLVM.ValueRef;
   setEq : LLVM.ValueRef;
   setNe : LLVM.ValueRef;
   setLe : LLVM.ValueRef;
   setLt : LLVM.ValueRef;
   setGe : LLVM.ValueRef;
   setGt : LLVM.ValueRef;
   setRange : LLVM.ValueRef;
   setSingleton : LLVM.ValueRef;
   
   (* dummy filename for output - fixme and use redirection or pipes *)
   outFile := "./m3test.ll";


(*---------------------------------------------------------------------------*)

PROCEDURE New (output: Wr.T): M3CG.T =
VAR mbuf := M3Buf.New ();
BEGIN
  M3Buf.AttachDrain (mbuf, output);
  RETURN NEW (U, wr := output, buf := mbuf, buf_len := 0,
              structTypeTable := NEW (IntRefTbl.Default).init (20),
              labelTable := NEW (IntRefTbl.Default).init (20),
              exprStack := NEW(RefSeq.T).init(),
              callStack := NEW(RefSeq.T).init(),
              procStack := NEW(RefSeq.T).init(),
              declStack := NEW(RefSeq.T).init());
END New;

PROCEDURE NewVar (self: U; name : Name; size : ByteSize; align : Alignment; type : Type; isConst : BOOLEAN; m3t : TypeUID; in_memory : BOOLEAN; up_level : BOOLEAN; varType : VarType): Var =
VAR 
  v := NEW (LvVar, tag := self.next_var, name := name, size := size, type := type, isConst := isConst, align := align, m3t := m3t, in_memory := in_memory, up_level := up_level, varType := varType);
BEGIN
  INC (self.next_var);
  IF varType = VarType.Global THEN
    v.inits := NEW(RefSeq.T).init();
  END;
  SetLvType(self,v);
  RETURN v;
END NewVar;

PROCEDURE NewProc (self: U; name : Name; numParams : INTEGER; returnType : Type; lev : INTEGER; cc : CallingConvention; exported : BOOLEAN; parent : Proc): Proc =
VAR 
  p := NEW (LvProc, tag := self.next_proc, name := name, numParams := numParams, returnType := returnType, lev := lev, cc := cc, exported := exported, parent := parent);
BEGIN
  INC (self.next_proc);
  RETURN p;
END NewProc;

PROCEDURE Pop(stack : RefSeq.T; n: CARDINAL := 1) =
BEGIN
  FOR i := 1 TO n DO
    EVAL stack.remlo();
  END;
END Pop;

PROCEDURE Push(stack : RefSeq.T; value : REFANY) =
BEGIN
  stack.addlo(value);
END Push;

PROCEDURE PushRev(stack : RefSeq.T; value : REFANY) =
BEGIN
  stack.addhi(value);
END PushRev;

PROCEDURE Get(stack : RefSeq.T; n: CARDINAL := 0) : REFANY =
BEGIN
  RETURN stack.get(n);
END Get;

PROCEDURE Put(stack : RefSeq.T; n: CARDINAL; e : REFANY) =
BEGIN
  stack.put(n,e);
END Put;

PROCEDURE PopDecl(self : U) =
BEGIN
  IF self.declStack.size() > 0 THEN
    Pop(self.declStack);
  END;
END PopDecl;

PROCEDURE PushDecl(self : U; p : LvProc) =
BEGIN
  Push(self.declStack,p);    
END PushDecl;

PROCEDURE VarName(var : Var) : Ctypes.char_star =
BEGIN
  RETURN LT("v." & Fmt.Int(NARROW(var,LvVar).tag));
END VarName;

PROCEDURE WordTypes(t : MType) : BOOLEAN =
BEGIN
  IF t = Type.Word8 OR t = Type.Word16 OR t = Type.Word32 OR t = Type.Word64 THEN
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END WordTypes;

PROCEDURE TypeSize(t : MType) : CARDINAL =
  BEGIN
    (* returns size in bytes maybe use llvm func storesizeoftype after llvmtype*)
    IF t = Type.Word8 OR t = Type.Int8 THEN RETURN 1;
    ELSIF t = Type.Word16 OR t = Type.Int16 THEN RETURN 2;
    ELSIF t = Type.Word32 OR t = Type.Int32 THEN RETURN 4;
    ELSIF t = Type.Word64 OR t = Type.Int64 THEN RETURN 8;
    ELSIF t = Type.Addr THEN RETURN ptrBytes; (* target dependant *)
    ELSIF t = Type.Reel THEN RETURN 4;
    ELSIF t = Type.LReel THEN RETURN 8;
    ELSIF t = Type.XReel THEN RETURN 10;
    ELSE
      <*ASSERT FALSE *>
    END;
  END TypeSize;
  
PROCEDURE LLvmType(t : Type) : LLVM.TypeRef =
BEGIN
  CASE t OF
  | Type.Int8   => RETURN LLVM.LLVMInt8Type();
  | Type.Word8  => RETURN LLVM.LLVMInt8Type();
  | Type.Int16  => RETURN LLVM.LLVMInt16Type();
  | Type.Word16 => RETURN LLVM.LLVMInt16Type();
  | Type.Int32  => RETURN LLVM.LLVMInt32Type();
  | Type.Word32 => RETURN LLVM.LLVMInt32Type();
  | Type.Int64  => RETURN LLVM.LLVMInt64Type();
  | Type.Word64 => RETURN LLVM.LLVMInt64Type();
  | Type.Reel   => RETURN LLVM.LLVMFloatType();
  | Type.LReel  => RETURN LLVM.LLVMDoubleType();
  | Type.XReel  => RETURN LLVM.LLVMX86FP80Type();
  | Type.Addr   => RETURN AdrTy;
  | Type.Struct => RETURN AdrTy;  (* fixme use setlvtype *)
  | Type.Void  => RETURN LLVM.LLVMVoidType();
  END;
END LLvmType;

(* combine this proc with llvmtype *)
PROCEDURE SetLvType(self : U; v : LvVar) =
VAR
  varTy,arrTy,structTy : LLVM.TypeRef;
  elem : UNTRACED REF ARRAY OF LLVM.TypeRef;
  elemVal : UNTRACED REF LLVM.TypeRef := NIL;  
  typeExists : BOOLEAN;
  structRef : REFANY;
BEGIN
  IF v.type # Type.Struct THEN
    varTy := LLvmType(v.type);
  ELSE
    typeExists := self.structTypeTable.get(v.size,structRef);
    IF typeExists THEN
      structTy := NARROW(structRef,LvStruct).struct;
    ELSE
      elem := NEW(UNTRACED REF ARRAY OF LLVM.TypeRef, 1);
      elemVal := LOOPHOLE(ADR(elem[0]), UNTRACED REF LLVM.TypeRef);
      arrTy := LLVM.LLVMArrayType(i8Type,v.size);
      elem[0] := arrTy;    
      structTy := LLVM.LLVMStructCreateNamed(globContext, LT("struct"));    
      LLVM.LLVMStructSetBody(structTy, elemVal, 1, FALSE);
      (* save the type *)
      structRef := NEW(LvStruct,struct := structTy);
      EVAL self.structTypeTable.put(v.size,structRef);  
    END;
    varTy := structTy;
  END;
  v.lvType := varTy;
END SetLvType;
    
PROCEDURE Zero(t : LLVM.TypeRef) : LLVM.ValueRef =
BEGIN
  RETURN LLVM.LLVMConstNull(t);   
END Zero;

PROCEDURE One(t : LLVM.TypeRef) : LLVM.ValueRef =
BEGIN
  RETURN LLVM.LLVMConstInt(t, 1L, TRUE);   
END One;

(* avoid the m3toc stuff everywhere *)
PROCEDURE LT(t : TEXT) : Ctypes.char_star =
BEGIN
  RETURN M3toC.CopyTtoS(t);
END LT;

PROCEDURE GenInitSection(self : IfThenObj) =
VAR
  lVal : LLVM.ValueRef;
  BEGIN
    self.curBB := LLVM.LLVMGetInsertBlock(builderIR);
    self.thenBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT(self.opName));
    self.elseBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT("not" & self.opName));
    self.exitBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT("endif"));
    LLVM.LLVMPositionBuilderAtEnd(builderIR,self.curBB);
    lVal := LLVM.LLVMBuildCondBr(builderIR,self.cmpVal,self.thenBB,self.elseBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,self.thenBB);
 END GenInitSection;
 
PROCEDURE GenThenSection(self : IfThenObj) =
  BEGIN
    EVAL LLVM.LLVMBuildBr(builderIR,self.exitBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,self.elseBB);
  END GenThenSection;

PROCEDURE GenExitSection(self : IfThenObj) =
  BEGIN
    EVAL LLVM.LLVMBuildBr(builderIR,self.exitBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,self.exitBB);    
  END GenExitSection;
  
  (* half works so far *)
PROCEDURE TestMetadata() =
VAR
   strVal,strVal2,lastInstr,mdNode : LLVM.ValueRef;
   curBB : LLVM.BasicBlockRef;
   kindId : CARDINAL;
   paramsVal : UNTRACED REF ARRAY OF LLVM.ValueRef;
   vals : UNTRACED REF LLVM.ValueRef := NIL;   
 BEGIN
  curBB := LLVM.LLVMGetInsertBlock(builderIR);
  lastInstr := LLVM.LLVMGetLastInstruction(curBB);
  kindId := LLVM.LLVMGetMDKindID(LT("0"),1);

  strVal := LLVM.LLVMMDString(LT("llvm.loop"),9);
  strVal2 := LLVM.LLVMMDString(LT("alvm.loop"),9);

  LLVM.LLVMSetMetadata(strVal, kindId, lastInstr);
  
  (* test add named md works *)
  paramsVal := NEW(UNTRACED REF ARRAY OF LLVM.ValueRef, 2);
  vals := LOOPHOLE(ADR(paramsVal[0]), UNTRACED REF LLVM.ValueRef);
  paramsVal[0] := strVal;
  paramsVal[1] := strVal2;

  mdNode := LLVM.LLVMMDNode(vals,2);
  
  LLVM.LLVMAddNamedMetadataOperand(modRef, LT("myloop"), mdNode);  
END TestMetadata;

PROCEDURE GetIntrinsicId(intrinsicName : TEXT) : INTEGER =
VAR
  id : INTEGER;
  fn : LLVM.ValueRef;
  procTy : LLVM.TypeRef;
  BEGIN
    (* this is a real hack. we should be able to access the c++ enum value
       for the intrinsic id *)
    procTy := LLVM.LLVMFunctionType(LLvmType(Type.Void), NIL, 0, FALSE);
    fn := LLVM.LLVMAddFunction(modRef, LT(intrinsicName), procTy); 
    id := LLVM.LLVMGetIntrinsicID(fn);
    LLVM.LLVMDeleteFunction(fn);
    RETURN id;
  END GetIntrinsicId;

PROCEDURE CheckIntrinsics() =
BEGIN
  EVAL MemMoveFn();
  EVAL MemCopyFn();
  EVAL MemSetFn();
  
  EVAL RoundFn(Type.Reel);  
  EVAL RoundFn(Type.LReel);    
  EVAL RoundFn(Type.XReel);  

  EVAL FloorFn(Type.Reel);  
  EVAL FloorFn(Type.LReel);    
  EVAL FloorFn(Type.XReel); 
  
  EVAL TruncFn(Type.Reel);  
  EVAL TruncFn(Type.LReel);    
  EVAL TruncFn(Type.XReel); 
  
  EVAL CeilFn(Type.Reel);  
  EVAL CeilFn(Type.LReel);    
  EVAL CeilFn(Type.XReel);   
END CheckIntrinsics;

(* declare an external set function *)
PROCEDURE DeclSet(name : TEXT; fn : LLVM.ValueRef; numParams : INTEGER; hasReturn : BOOLEAN; setRange : BOOLEAN := FALSE) : LLVM.ValueRef =
VAR
  proc : LLVM.ValueRef;
  retTy,procTy : LLVM.TypeRef;
  paramsTy : UNTRACED REF ARRAY OF LLVM.TypeRef;
  args : UNTRACED REF LLVM.TypeRef := NIL;
BEGIN
  IF fn # NIL THEN RETURN fn; END;
  IF hasReturn THEN
    retTy := IntPtrTy;  
  ELSE
    retTy := LLvmType(Type.Void);
  END;
  
  paramsTy := NEW(UNTRACED REF ARRAY OF LLVM.TypeRef, numParams+1);
  args := LOOPHOLE(ADR(paramsTy[0]), UNTRACED REF LLVM.TypeRef);
  
  paramsTy[0] := IntPtrTy;
  FOR i := 1 TO numParams - 1 DO
    paramsTy[i] := PtrTy;  
  END;
  IF setRange THEN
    paramsTy[1] := IntPtrTy;  
  END;
 
  procTy := LLVM.LLVMFunctionType(retTy, args, numParams, FALSE);
  proc := LLVM.LLVMAddFunction(modRef, LT(name), procTy);
  RETURN proc;
END DeclSet;

(* Declare this procedure and all its locals and parameters. *)
PROCEDURE BuildFunc(<*UNUSED*> self : U; p : Proc) =
VAR
  param : LvVar;
  proc : LvProc;
  retTy : LLVM.TypeRef;
  paramsTy : UNTRACED REF ARRAY OF LLVM.TypeRef;
  args : UNTRACED REF LLVM.TypeRef := NIL;
  lVal : LLVM.ValueRef;
  numParams : CARDINAL;
  name : TEXT;
  arg : REFANY;
BEGIN
  proc := NARROW(p,LvProc);
  IF proc.defined THEN RETURN; END; 
  numParams := proc.numParams;
 
  IF proc.imported THEN
    (* delete the temp function and define the real one *)
    LLVM.LLVMDeleteFunction(proc.lvProc);
  END;
  proc.defined := TRUE;
  <*ASSERT proc.paramStack.size() = numParams *>

  (* create the param types from the stack *)
  retTy := LLvmType(proc.returnType);

  IF numParams > 0 THEN
    paramsTy := NEW(UNTRACED REF ARRAY OF LLVM.TypeRef, numParams);
    FOR i := 0 TO numParams - 1 DO
      arg := Get(proc.paramStack,i);
      param := NARROW(arg,LvVar);
      IF param.type = Type.Struct THEN    
        param.lvType := LLVM.LLVMPointerType(param.lvType);       
      END;       
      paramsTy[i] := param.lvType; 
    END;
    args := LOOPHOLE(ADR(paramsTy[0]), UNTRACED REF LLVM.TypeRef);
  END;

  (* create the function sig *)
  proc.procTy := LLVM.LLVMFunctionType(retTy, args, numParams, FALSE);

  (* create the function *)
  name := M3ID.ToText(proc.name);
  proc.lvProc := LLVM.LLVMAddFunction(modRef, LT(name), proc.procTy);

  IF proc.exported THEN
(* dont seem to need this
    LLVM.LLVMSetLinkage(proc.lvProc,LLVM.Linkage.External);
    (*LLVM.LLVMSetLinkage(proc.lvProc,LLVM.Linkage.AvailableExternally);*)
*)
  END;

  (* c funcs seem to have these attrs ?? *)
  LLVM.LLVMAddFunctionAttr(proc.lvProc, LLVM.NoUnwindAttribute);
  LLVM.LLVMAddFunctionAttr(proc.lvProc, LLVM.UWTable);

  <*ASSERT LLVM.LLVMCountParams(proc.lvProc) = numParams *>

  (* add names attributes and alignment *)
  lVal := LLVM.LLVMGetFirstParam(proc.lvProc);
  FOR i := 0 TO numParams - 1 DO
    arg := Get(proc.paramStack,i);
    param := NARROW(arg,LvVar);
    IF param.name = M3ID.NoID THEN
      name := "parm";
    ELSE
      name := M3ID.ToText(param.name);
    END;
    (* set a name for the param - doesnt work for externals *)    
    LLVM.LLVMSetValueName(lVal, LT(name));
    
    (* this sets the byval attribute by which the caller make a copy *)
    IF param.type = Type.Struct THEN    
      LLVM.LLVMAddAttribute(lVal, LLVM.ByValAttribute);    
    END;
    (* set the alignment not sure we need it except for struct *)
    LLVM.LLVMSetParamAlignment(lVal, param.align);
    lVal := LLVM.LLVMGetNextParam(lVal);
  END;
END BuildFunc;

<*NOWARN*>PROCEDURE DumpLLVMIR(self : U) =
VAR
  msg : Ctypes.char_star_star;
BEGIN
(* use this to stream to stderr when called from frontend
   which needs redirect but need all llvmdumpvalue calls removed
*)
(*
  LLVM.LLVMDumpModule(modRef);
*)

  EVAL LLVM.LLVMPrintModuleToFile(modRef, LT(outFile), msg);
  (* or to bitcode *)
  (*
  EVAL LLVM.LLVMWriteBitcodeToFile(modRef, LT(outFile));
  *)

END DumpLLVMIR;

PROCEDURE AssertFalse() =
BEGIN
  <* ASSERT FALSE *>
END AssertFalse;


(*----------------------------------------------------------- ID counters ---*)

PROCEDURE next_label (self: U;  n: INTEGER := 1): Label =
VAR x := self.next_label_id;
  BEGIN
    INC (self.next_label_id, n);
    RETURN x;
  END next_label;

(*------------------------------------------------ READONLY configuration ---*)

<*NOWARN*>PROCEDURE set_error_handler(self: U; p: M3CG_Ops.ErrorHandler) = 
BEGIN 
  AssertFalse(); 
END set_error_handler;

(*----------------------------------------------------- compilation units ---*)

<*NOWARN*> PROCEDURE begin_unit (self: U;  optimize : INTEGER) =
  BEGIN
    targetData := LLVM.LLVMCreateTargetData(LT(dataRep));
    ptrBytes := LLVM.LLVMPointerSize(targetData);  
    IntPtrTy := LLVM.LLVMIntPtrType(targetData);
    PtrTy := LLVM.LLVMPointerType(IntPtrTy);
    AdrTy := LLVM.LLVMPointerType(LLVM.LLVMInt8Type());  
    AdrAdrTy := LLVM.LLVMPointerType(AdrTy);    
    ptrBits := LLVM.LLVMSizeOfTypeInBits(targetData, PtrTy);
    wordSize := LLVM.LLVMConstInt(IntPtrTy, VAL(ptrBits,LONGINT), TRUE);
    byteSize := LLVM.LLVMConstInt(IntPtrTy, VAL(ptrBytes,LONGINT), TRUE);
(*
IO.PutInt(ptrBytes);
IO.PutLongInt(ptrBits);
*) 
  END begin_unit;

<*NOWARN*> PROCEDURE end_unit (self: U) =
  BEGIN
(* not used - possibly write the unit *)
(*
    CheckIntrinsics();
*)
  END end_unit;

<*NOWARN*> PROCEDURE import_unit (self: U;  n: Name) =
  BEGIN
(* not used *) 
  END import_unit;

<*NOWARN*> PROCEDURE export_unit (self: U;  n: Name) =
  BEGIN
(* not used *) 
  END export_unit;

(*------------------------------------------------ debugging line numbers ---*)

<*NOWARN*> PROCEDURE set_source_file (self: U;  file: TEXT) =
  BEGIN
    moduleID := LT(file);
    modRef := LLVM.LLVMModuleCreateWithNameInContext(moduleID,globContext);
    LLVM.LLVMSetDataLayout(modRef,LT(dataRep));  
    LLVM.LLVMSetTarget(modRef,LT(targetTriple));     
  END set_source_file;

<*NOWARN*> PROCEDURE set_source_line (self: U; line: INTEGER) =
  BEGIN
(* fix me get the comments working and metadata *) 
  END set_source_line;

(*------------------------------------------- debugging type declarations ---*)

(* no debug support yet needs metadata working. see the llvm document on
  debug support *)
<*NOWARN*> PROCEDURE declare_typename (self: U;  t: TypeUID;  n: Name) =
  BEGIN
(* fix me *) 
  END declare_typename;

<*NOWARN*> PROCEDURE declare_array (self: U;  t, index, elt: TypeUID;  s: BitSize) =
  BEGIN
(* fix me *) 
  END declare_array;

<*NOWARN*> PROCEDURE declare_open_array (self: U;  t, elt: TypeUID;  s: BitSize) =
  BEGIN
(* fix me *) 
  END declare_open_array;

<*NOWARN*> PROCEDURE declare_enum (self: U;  t: TypeUID;  n_elts: INTEGER;  s: BitSize) =
  BEGIN
(* fix me *) 
  END declare_enum;

<*NOWARN*> PROCEDURE declare_enum_elt (self: U;  n: Name) =
  BEGIN
(* fix me *) 
  END declare_enum_elt;

<*NOWARN*> PROCEDURE declare_packed (self: U;  t: TypeUID;  s: BitSize;  base: TypeUID) =
  BEGIN
(* fix me *) 
  END declare_packed;

<*NOWARN*> PROCEDURE declare_record (self: U; t: TypeUID;  s: BitSize;                          n_fields: INTEGER)=
  BEGIN
(* fix me *) 
  END declare_record;

<*NOWARN*> PROCEDURE declare_field (self: U;  n: Name;  o: BitOffset;  s: BitSize;                         t: TypeUID)=
  BEGIN
(* fix me *) 
  END declare_field;

<*NOWARN*> PROCEDURE declare_set (self: U;  t, domain: TypeUID;  s: BitSize) =
  BEGIN
(* fix me *) 
  END declare_set;

<*NOWARN*> PROCEDURE declare_subrange (self: U; t, domain: TypeUID;                            READONLY min, max: Target.Int; s: BitSize) =
  BEGIN
(* fix me *) 
  END declare_subrange;

<*NOWARN*> PROCEDURE declare_pointer (self: U;  t, target: TypeUID;  brand: TEXT; traced: BOOLEAN) =
  BEGIN
(* fix me *) 
  END declare_pointer;

<*NOWARN*> PROCEDURE declare_indirect (self: U;  t, target: TypeUID) =
  BEGIN
(* fix me *) 
  END declare_indirect;

<*NOWARN*> PROCEDURE declare_proctype (self: U; t: TypeUID; n_formals: INTEGER; result: TypeUID;  n_raises: INTEGER; cc: CallingConvention) =
  BEGIN
(* fix me *) 
  END declare_proctype;

<*NOWARN*> PROCEDURE declare_formal (self: U;  n: Name;  t: TypeUID) =
  BEGIN
(* fix me *) 
  END declare_formal;

<*NOWARN*> PROCEDURE declare_raises (self: U;  n: Name) =
  BEGIN
(* fix me *) 
  END declare_raises;

<*NOWARN*> PROCEDURE declare_object (self: U; t, super: TypeUID; brand: TEXT;  traced: BOOLEAN;n_fields, n_methods: INTEGER;field_size: BitSize) =
  BEGIN
(* fix me *) 
  END declare_object;

<*NOWARN*> PROCEDURE declare_method (self: U;  n: Name;  signature: TypeUID) =
  BEGIN
(* fix me *) 
  END declare_method;

<*NOWARN*> PROCEDURE declare_opaque (self: U;  t, super: TypeUID) =
  BEGIN
(* fix me *) 
  END declare_opaque;

<*NOWARN*> PROCEDURE reveal_opaque (self: U;  lhs, rhs: TypeUID) =
  BEGIN
(* fix me *) 
  END reveal_opaque;

<*NOWARN*> PROCEDURE declare_exception (self: U;  n: Name;  arg_type: TypeUID;raise_proc: BOOLEAN;  base: Var;  offset: INTEGER) =
  BEGIN
(* fix me *) 
  END declare_exception;

<*NOWARN*> PROCEDURE widechar_size (self: U; size: INTEGER) = 
  BEGIN
    self.widecharSize := size;
  END widechar_size;

(*--------------------------------------------------------- runtime hooks ---*)

PROCEDURE set_runtime_proc (self: U;  n: Name;  p: Proc) =
  VAR 
    proc : LvProc;
  BEGIN 
    (* declare a runtime proc *)
    self.buildFunc(p);  
    proc := NARROW(p,LvProc);
    IF Text.Compare(M3ID.ToText(n),"ReportFault") = 0 THEN
      (* save the fault proc *)
      self.abortFunc := proc.lvProc;
    END;
    PopDecl(self);   
  END set_runtime_proc;

(*------------------------------------------------- variable declarations ---*)

<*NOWARN*> PROCEDURE import_global (self: U;  n: Name;  s: ByteSize;  a: Alignment; t: Type;  m3t: TypeUID): Var =
  BEGIN
    (* fixme - ever generated ?? *)
    AssertFalse();
    RETURN NIL;
  END import_global;

PROCEDURE declare_segment (self: U;  n: Name;  m3t: TypeUID; is_const: BOOLEAN): Var =
VAR
  v : LvVar := NewVar(self,n,0,0,Type.Struct,is_const,m3t,TRUE,FALSE,VarType.Global);
  segName : TEXT;
  BEGIN
 (* fixme *)
    IF is_const THEN
      (* the name will be nil so create a const name *)    
      segName := "M_Const";
    ELSE
      segName := M3ID.ToText(n);
    END;
    
    (* create an opaque struct type *)
    v.lvType := LLVM.LLVMStructCreateNamed(globContext, LT(segName & "_struct"));
    v.lv := LLVM.LLVMAddGlobal(modRef, v.lvType, LT(segName));
    IF is_const THEN
      LLVM.LLVMSetGlobalConstant(v.lv,TRUE);
    ELSE
      (* save the global for abort procedure *)
      faultVal := v.lv;
    END;
    (* this global is internal *)
    LLVM.LLVMSetLinkage(v.lv,LLVM.Linkage.Internal);      
    RETURN v;
  END declare_segment;

<*NOWARN*> PROCEDURE bind_segment (self: U;  seg: Var;  s: ByteSize;  a: Alignment; t: Type;  exported, inited: BOOLEAN) =
VAR v : LvVar;
  BEGIN
    v := NARROW(seg,LvVar);
    v.align := a;
  END bind_segment;

<*NOWARN*> PROCEDURE declare_global (self: U;  n: Name;  s: ByteSize;  a: Alignment; t: Type;  m3t: TypeUID;  exported, inited: BOOLEAN): Var =
VAR
  v : LvVar := NewVar(self,n,s,a,t,FALSE,m3t,TRUE,FALSE,VarType.Global);
  globName : TEXT;
  BEGIN
 (* fixme *)
    IF v.name = M3ID.NoID THEN
      globName := "m3global";
    ELSE
      globName := M3ID.ToText(v.name);
    END;   
  
    v.lv := LLVM.LLVMAddGlobal(modRef, v.lvType, LT(globName));
    (* need to add exported and check external
      LLVM.LLVMSetLinkage(glob,LLVM.Linkage.External);
    *)
    RETURN v;
  END declare_global;

<*NOWARN*> PROCEDURE declare_constant (self: U;  n: Name;  s: ByteSize;  a: Alignment; t: Type;  m3t: TypeUID;  exported, inited: BOOLEAN): Var =
  BEGIN
  (* fixme ever generated ??*)
    AssertFalse();
    RETURN NIL;
  END declare_constant;

PROCEDURE AllocVar(v : LvVar) =
VAR
  name : TEXT;
  BEGIN
    IF v.name = M3ID.NoID THEN
      name := "tmp";
    ELSE
      name := M3ID.ToText(v.name);
    END;
    IF v.varType = VarType.Param THEN
      name := name & ".addr";
    END;
    v.lv := LLVM.LLVMBuildAlloca(builderIR, v.lvType, LT(name));
    LLVM.LLVMSetAlignment(v.lv,v.align);
  END AllocVar;

<*NOWARN*> PROCEDURE declare_local (self: U;  n: Name;  s: ByteSize;  a: Alignment; t: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN; f: Frequency): Var =
VAR
(* need to add freq, think in_memory is false for real locals and true for the temp locals for exceptions etc declared in body of procedure so guess the blocklevel test could be replaced with in_memory test below *)
  v : LvVar := NewVar(self,n,s,a,t,FALSE,m3t,in_memory,up_level,VarType.Local); 
  proc : LvProc;
  BEGIN
  (* locals are declared after declare_procedure or within a begin_procedure
     which are anonymous or inside begin_block. Since begin_procedure implies
     a begin_block, checking for blockLevel > 0 is sufficient to allocate now *)
     
    (* get the current procs local stack *)
    IF self.blockLevel = 0 THEN
      <*ASSERT self.declStack.size() = 1 *>
      proc := Get(self.declStack);
      PushRev(proc.localStack, v);
    ELSE
      (* inside a local block or begin_procedure so allocate it now *)  
      AllocVar(v);
    END;
    RETURN v;
  END declare_local;

<*NOWARN*> PROCEDURE declare_param (self: U;  n: Name;  s: ByteSize;  a: Alignment; t: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN; f: Frequency): Var =
VAR
(* should add freq, llvm metadata for freq could be used *)
  v : LvVar := NewVar(self,n,s,a,t,FALSE,m3t,in_memory,up_level,VarType.Param);
  proc : LvProc;
  BEGIN
    (* params declared after either declare_procedure or import_procedure
       (which could be in a begin_procedure), either way the procDecl should 
        be set from the previous import or declare *)
    <*ASSERT self.declStack.size() = 1 *>
    proc := Get(self.declStack);
    PushRev(proc.paramStack, v);    
    RETURN v;
  END declare_param;

PROCEDURE declare_temp (self: U;  s: ByteSize;  a: Alignment;  t: Type; in_memory:BOOLEAN): Var =
VAR
  v  : LvVar := NewVar(self,M3ID.NoID,s,a,t,FALSE,0,in_memory,FALSE,VarType.Temp);
  BEGIN
    (* temps are always declared inside a begin_procedure so curProc always
       non nil - actually dont think we need the assert anymore *)
    <*ASSERT self.curProc # NIL *>       
    AllocVar(v);
    RETURN v;
  END declare_temp;

<*NOWARN*> PROCEDURE free_temp (self: U;  v: Var) =
  BEGIN
    (* nothing to do *)
  END free_temp;

(*---------------------------------------- static variable initialization ---*)

(* Global variables may be initialized only once.  All of their init_*
   calls must be bracketed by begin_init and end_init.  Within a begin/end
   pair, init_* calls must be made in ascending offset order.  Begin/end
   pairs may not be nested.  Any space in a global variable that's not
   explicitly initialized is zeroed.  *)
   
PROCEDURE begin_init (self: U;  v: Var) =
  BEGIN
    (*The curvar is just to track which var is being initd in init_chars et al*)
    self.curVar := v;
  END begin_init;
  
(* Now we have all the global vars we can construct the body of the segment
   type and initialise the global. This could be cleaned up to generate the
   filler with each init_xx call so that we dont need to regenerate the inits
   here which would mean only looping through the inits once. *)
PROCEDURE end_init (self: U;  v: Var) =
VAR
  baseObj : BaseVar;
  int,numGlobs : INTEGER;
  realStr : TEXT;
  thisVar : LvVar;
  types : UNTRACED REF ARRAY OF LLVM.TypeRef;
  typesVal : UNTRACED REF LLVM.TypeRef := NIL;
  init : UNTRACED REF ARRAY OF LLVM.ValueRef;
  initVal : UNTRACED REF LLVM.ValueRef := NIL;
  structVal,varVal : LLVM.ValueRef;
  proc : LvProc;
  var : LvVar;
  newInits : RefSeq.T;
  fillVar : FillerVar;
  thisOfs,ofsTot,fillLen : LONGINT;
  BEGIN
(* test me *)
    (* generate the struct and the global segment var which holds all globals *)
    thisVar := NARROW(v,LvVar);    
    numGlobs := thisVar.inits.size();
    
    IF numGlobs = 0 THEN RETURN; END;

    (* keep running total of offsets for filler calcs *)
    thisOfs := 0L;
    ofsTot := 0L;

    (* need to build a new inits stack to handle fillers *)
    newInits := NEW(RefSeq.T).init();

    FOR i := 0 TO numGlobs - 1 DO
      baseObj := Get(thisVar.inits,i);
      TYPECASE baseObj OF
      | IntVar(v) =>
         v.lvTy := LLvmType(v.type);
      | ProcVar(v) =>
         proc := NARROW(v.value,LvProc);
         v.lvTy := LLVM.LLVMTypeOf(proc.lvProc);  
(* implement label here *)
      | VarVar(v) =>
         v.lvTy := AdrTy;
(* implement offset here *)        
      | TextVar(v) =>    
         v.lvVal := LLVM.LLVMConstString(LT(v.value),Text.Length(v.value),FALSE);
         v.lvTy := LLVM.LLVMTypeOf(v.lvVal);               
      | FloatVar(v) =>    
         v.lvTy := LLvmType(v.prec);
      ELSE
        <*ASSERT FALSE*>
      END;
      ofsTot := VAL(baseObj.offset,LONGINT);   
      fillLen := ofsTot - thisOfs;

      thisOfs := thisOfs + LLVM.LLVMStoreSizeOfType(targetData,baseObj.lvTy);

      (* add a filler *)
      IF fillLen > 0L THEN
        fillVar := NEW(FillerVar);
        fillVar.lvTy := LLVM.LLVMArrayType(i8Type,VAL(fillLen,INTEGER));
        thisOfs := thisOfs + fillLen;
        PushRev(newInits,fillVar);    
      END;
      PushRev(newInits,baseObj);        
    END;
  
    (* update numbe of globals *)
    numGlobs := newInits.size();
    
    (* allocate the arrays for llvm *)
    types := NEW(UNTRACED REF ARRAY OF LLVM.TypeRef, numGlobs);
    init := NEW(UNTRACED REF ARRAY OF LLVM.ValueRef, numGlobs);
    typesVal := LOOPHOLE(ADR(types[0]), UNTRACED REF LLVM.TypeRef);
    initVal := LOOPHOLE(ADR(init[0]), UNTRACED REF LLVM.ValueRef);
    
    (* setup the types array *)
    FOR i := 0 TO numGlobs - 1 DO
      baseObj := Get(newInits,i);
      types[i] := baseObj.lvTy;    
    END;
  
    (* fill in the body of our opaque global struct now we know the types *)
    LLVM.LLVMStructSetBody(thisVar.lvType, typesVal, numGlobs, FALSE);

    (* calc the initialisers *)
    FOR i := 0 TO numGlobs  - 1 DO
      baseObj := Get(newInits,i);
      TYPECASE baseObj OF
      | IntVar(v) =>
          EVAL TInt.ToInt (v.value, int);
          v.lvVal := LLVM.LLVMConstInt(v.lvTy, VAL(int,LONGINT), TRUE);
      | ProcVar(v) =>
          proc := NARROW(v.value,LvProc);
          v.lvVal := proc.lvProc;
(* implement label here *)
      | VarVar(v) => 
          var := NARROW(v.value,LvVar);
          varVal := LLVM.LLVMBuildBitCast(builderIR, var.lv, AdrTy, LT("var_toadr"));
          v.lvVal := BuildConstGep(varVal,v.bias);
(* implement offset here *)
      | TextVar(<*UNUSED*> v) => (* aready done in construct type *)
      | FloatVar(v) =>  
          realStr := ConvertFloat(v.value);
          v.lvVal := LLVM.LLVMConstRealOfString(v.lvTy, LT(realStr));    
      | FillerVar(v) =>
          v.lvVal := LLVM.LLVMConstNull(v.lvTy);
      ELSE
        <*ASSERT FALSE*>    
      END;
      init[i] := baseObj.lvVal;   
    END;    

    (* save the initialisers *)
    structVal := LLVM.LLVMConstNamedStruct(thisVar.lvType, initVal, numGlobs);

    LLVM.LLVMSetInitializer(thisVar.lv, structVal);
    LLVM.LLVMSetAlignment(thisVar.lv, thisVar.align);

    thisVar.inits := newInits; (* keep a ref *)
    self.curVar := NIL;
  END end_init;


PROCEDURE init_int (self: U;  o: ByteOffset;  READONLY value: Target.Int; t: Type) =
(* initializes the integer static variable at 'ADR(v)+o' with
   the low order bits of 'value' which is of integer type 't'. *)
VAR
  intObj : IntVar;
  BEGIN
    intObj := NEW(IntVar, offset := o, type := t, value := value);
    PushRev(self.curVar.inits,intObj);    
  END init_int;

PROCEDURE init_proc (self: U;  o: ByteOffset;  value: Proc) =
(* initializes the static variable at 'ADR(v)+o' with the address
   of procedure 'value'. *)
VAR
  procObj : ProcVar;
  BEGIN
    procObj := NEW(ProcVar, offset := o, value := value);
    PushRev(self.curVar.inits,procObj);
  END init_proc;

<*NOWARN*> PROCEDURE init_label (self: U;  o: ByteOffset;  value: Label) =
(* initializes the static variable at 'ADR(v)+o' with the address
   of the label 'value'.  *)
  BEGIN
(* ever generated ?? *)
(* could use basicblockaddress ?? *)
  END init_label;

PROCEDURE init_var (self: U;  o: ByteOffset;  value: Var;  bias: ByteOffset) =
(* initializes the static variable at 'ADR(v)+o' with the address
   of 'value+bias'.  *)
VAR
  varObj : VarVar;   
  BEGIN
    varObj := NEW(VarVar, offset := o, value := value, bias := bias);
    PushRev(self.curVar.inits,varObj);    
  END init_var;

<*NOWARN*> PROCEDURE init_offset (self: U;  o: ByteOffset;  value: Var) =
(* initializes the static variable at 'ADR(v)+o' with the integer
   frame offset of the local variable 'var' relative to the frame
   pointers returned at runtime in RTStack.Frames *)
  BEGIN
(* ever generated ?? *)
  END init_offset;

PROCEDURE init_chars (self: U;  o: ByteOffset;  value: TEXT) =
(* initializes the static variable at 'ADR(v)+o' with the characters
   of 'value' *)
  VAR
  textObj : TextVar;
  BEGIN
    textObj := NEW(TextVar, offset := o, value := value);
    PushRev(self.curVar.inits,textObj);
  END init_chars;

PROCEDURE init_float (self: U;  o: ByteOffset;  READONLY f: Target.Float) =
(* initializes the static variable at 'ADR(v)+o' with the floating point value 'f' *)
VAR
  floatObj : FloatVar;
  prec : RType;
  BEGIN
    CASE TFloat.Prec(f) OF
    |  Target.Precision.Short => prec := Type.Reel;
    |  Target.Precision.Long => prec := Type.LReel;
    |  Target.Precision.Extended => prec := Type.XReel;    
    END;
    floatObj := NEW(FloatVar, offset := o, prec := prec, value := f);
    PushRev(self.curVar.inits,floatObj);
  END init_float;

(*------------------------------------------------------------ procedures ---*)

PROCEDURE import_procedure (self: U;  n: Name;  n_params: INTEGER; return_type: Type;  cc: CallingConvention): Proc =
VAR
  p : LvProc := NewProc(self,n,n_params,return_type,-1,cc,FALSE,NIL);
  name : TEXT;
  retTy : LLVM.TypeRef;
  BEGIN
 (* test me *)
     
    p.imported := TRUE; 
    (* dont need local stack since its imported but need a paramstack *)
    p.paramStack := NEW(RefSeq.T).init();
    
    PopDecl(self);

    (* create a dummy llvm proc which we can replace later if called and we
     have the params *)
    retTy := LLvmType(p.returnType);
    p.procTy := LLVM.LLVMFunctionType(retTy, NIL, 0, FALSE);

    name := M3ID.ToText(p.name);

    p.lvProc := LLVM.LLVMAddFunction(modRef, LT(name), p.procTy);
    (* has no effect LLVM.LLVMSetLinkage(p.lvProc,LLVM.Linkage.External); *)

    PushDecl(self,p);
    RETURN p;
  END import_procedure;

PROCEDURE declare_procedure (self: U;  n: Name;  n_params: INTEGER;
                             return_type: Type;  lev: INTEGER;
                             cc: CallingConvention;
                             exported: BOOLEAN;  parent: Proc): Proc =
VAR
  p : LvProc := NewProc(self,n,n_params,return_type,lev,cc,exported,parent);
  BEGIN  
    PopDecl(self); 
    p.imported := FALSE;    
    p.localStack := NEW(RefSeq.T).init();
    p.paramStack := NEW(RefSeq.T).init();
    PushDecl(self,p);  
    RETURN p;
  END declare_procedure;

(* begin blocks can be nested so need to keep a stack of procedures so we
  are referring to the current proc for the bb's *)
PROCEDURE begin_procedure (self: U;  p: Proc) =
(* begin generating code for the procedure 'p'.  Sets "current procedure"
   to 'p'.  Implies a begin_block.  *)
VAR
  local,param : LvVar;
  proc : LvProc;
  bbRef : LLVM.BasicBlockRef;
  storeVal,lVal : LLVM.ValueRef;
  numParams : CARDINAL;
  name : TEXT;
  arg : REFANY;
  BEGIN
    (*  Declare this procedure and all its locals and parameters.*)
    self.curProc := p;
    INC(self.blockLevel);

    proc := NARROW(p,LvProc);
    numParams := proc.numParams;

    (* top of stack is current proc *)
    proc.saveBB := LLVM.LLVMGetInsertBlock(builderIR);
    Push(self.procStack,proc);

    (* create the function *)
    self.buildFunc(p);

    (* create the entry basic block *)
    bbRef := LLVM.LLVMAppendBasicBlockInContext(globContext, self.curProc.lvProc,  LT("entry"));
    LLVM.LLVMPositionBuilderAtEnd(builderIR,bbRef);

    (* allocate the params if not a struct *)
    lVal := LLVM.LLVMGetFirstParam(self.curProc.lvProc);

    FOR i := 0 TO numParams - 1 DO
      arg := Get(proc.paramStack);
      param := NARROW(arg,LvVar);
      IF param.type # Type.Struct THEN
        AllocVar(param);
       (* do the stores for the parameters *)
        storeVal := LLVM.LLVMBuildStore(builderIR, lVal, param.lv);
        LLVM.LLVMSetAlignment(storeVal,param.align);
      ELSE
        (* refer directly to the param *)
        param.lv := lVal;
      END;
      lVal := LLVM.LLVMGetNextParam(lVal);
      Pop(proc.paramStack);
    END;

    (* allocate the locals *)
    WHILE proc.localStack.size() > 0 DO
      arg := Get(proc.localStack,0);
      local := NARROW(arg,LvVar);
      name := M3ID.ToText(local.name);
      (* ignore the _result parm llvm takes care of it *)
      IF Text.Compare(name,"_result") # 0 THEN 
        AllocVar(local);
      END;
      Pop(proc.localStack);
    END;  
  END begin_procedure;

<*NOWARN*> PROCEDURE end_procedure (self: U;  p: Proc) =
(* marks the end of the code for procedure 'p'.  Sets "current procedure"
   to NIL.  Implies an end_block.  *)
  VAR
    proc : LvProc;
    prevInstr : LLVM.ValueRef;
    opCode : LLVM.Opcode;
    curBB : LLVM.BasicBlockRef;
  BEGIN
    proc := NARROW(p,LvProc);
    (* its possible a no return warning will generate an abort and no return
      but llvm has mandatory return so if last instruction is not a return then
      add a dummy one, could possible add return to front end in this case *)
    curBB := LLVM.LLVMGetInsertBlock(builderIR);
    prevInstr := LLVM.LLVMGetLastInstruction(curBB);
    opCode := LLVM.LLVMGetInstructionOpcode(prevInstr);
    IF opCode # LLVM.Opcode.Ret THEN
      IF proc.returnType = Type.Void THEN
        EVAL LLVM.LLVMBuildRetVoid(builderIR);
      ELSE
        EVAL LLVM.LLVMBuildRet(builderIR,LLVM.LLVMGetUndef(LLvmType(proc.returnType)));
(*
        EVAL LLVM.LLVMBuildRet(builderIR,Zero(LLvmType(proc.returnType)));     
*)
      END;
    END;
     
    self.curProc := NIL;
    DEC(self.blockLevel);    
    
    IF self.procStack.size() > 0 THEN
      self.curProc := Get(self.procStack);
      Pop(self.procStack);
      LLVM.LLVMPositionBuilderAtEnd(builderIR,self.curProc.saveBB);
    END
  END end_procedure;

PROCEDURE begin_block (self: U) =
  BEGIN
    INC(self.blockLevel);
  END begin_block;

PROCEDURE end_block (self: U) =
  BEGIN
    DEC(self.blockLevel);
  END end_block;

<*NOWARN*> PROCEDURE note_procedure_origin (self: U;  p: Proc) =
  BEGIN
  (* not used *)
  END note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

<*NOWARN*> PROCEDURE DumpExprStack(self: U; from : TEXT) =
(* debug delete this when tested *)
(*
    The expression stack must be empty at each label, jump, or call.
    The stack must contain exactly one value prior to a conditional
    or indexed jump.
*)
(*
VAR
  tmpVal : LLVM.ValueRef;
*)
BEGIN
(*
  IO.Put("Expr stack from " & from & " size ");
  IO.PutInt(self.exprStack.size());
  IO.Put("\n");
  (* if from = jump stacksize could be 0 or 1 *)  
  FOR i := 0 TO self.exprStack.size() -1 DO
    tmpVal :=  NARROW(self.exprStack.get(i),LvExpr).lVal;
    LLVM.LLVMDumpValue(tmpVal);
  END;
  IO.Put("\nEnd stack dump\n");
*)
END DumpExprStack;


(* not using barrier thus far *)
<*NOWARN*> PROCEDURE set_label (self: U;  l: Label;  barrier: BOOLEAN) =
(* define 'l' to be at the current pc, if 'barrier', 'l' bounds an exception
   scope and no code is allowed to migrate past it. *)
VAR
  curBB : LLVM.BasicBlockRef;
  label : LabelObj;
  branch : BranchObj;
  terminator : LLVM.ValueRef;
BEGIN
  DumpExprStack(self,"set_label");

  curBB := LLVM.LLVMGetInsertBlock(builderIR);
  label := self.getLabel(l,"L");
  
  IF label.cmpInstr # NIL THEN
    LLVM.LLVMMoveBasicBlockAfter(label.labBB,curBB);  
  END;

  (* terminate the previous BB if it hasnt already been done *)
  terminator := LLVM.LLVMGetBasicBlockTerminator(curBB);
  IF terminator = NIL THEN
    LLVM.LLVMPositionBuilderAtEnd(builderIR,curBB);
    label.branchLVal := LLVM.LLVMBuildBr(builderIR,label.labBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,label.labBB);
  END;

  (* check if need to fix previous jmp instructions *)
  IF label.branchList # NIL THEN
    FOR i := 0 TO label.branchList.size() - 1 DO
      branch := label.branchList.get(i);
      (* the terminate previous bb could have added a branch but this is the correct branch so delete the old one*)
      terminator := LLVM.LLVMGetBasicBlockTerminator(branch.branchBB);
      IF terminator # NIL THEN
        LLVM.LLVMInstructionEraseFromParent(terminator);
      END;
      LLVM.LLVMPositionBuilderAtEnd(builderIR,branch.branchBB);     
      branch.branchLVal := LLVM.LLVMBuildBr(builderIR,label.labBB);
    END;
  END;
  LLVM.LLVMPositionBuilderAtEnd(builderIR,label.labBB);
END set_label;


(*
if we ever implement freq we could generate this metadata stuff

    !0 = metadata !{
  metadata !"branch_weights",
  i32 <TRUE_BRANCH_WEIGHT>,
  i32 <FALSE_BRANCH_WEIGHT>
}

this is the func to get the element of a type recusivley if you wish
til you reach the fundametal type i8 or whatever
PROCEDURE LLVMGetElementType(Ty: TypeRef): TypeRef;
*)

PROCEDURE jump (self: U; l: Label) =
VAR
  curBB : LLVM.BasicBlockRef;
  labRef : REFANY;
  label : LabelObj;
  branch : BranchObj;
  labelExists : BOOLEAN;
BEGIN
  DumpExprStack(self,"jump");

  curBB := LLVM.LLVMGetInsertBlock(builderIR);
  labelExists := self.labelTable.get(l,labRef);

  IF NOT labelExists THEN 
    label := NEW(LabelObj,branchList := NEW(RefSeq.T).init());
    EVAL self.labelTable.put(l,label);
  ELSE
    label := NARROW(labRef,LabelObj);
  END;

  branch := NEW(BranchObj);
  branch.branchBB := curBB;
  (* add the branch to our label *)
  label.branchList.addlo(branch);
  IF label.labBB # NIL THEN
    (* must have seen the label at some point so insert the branch *)
    branch.branchLVal := LLVM.LLVMBuildBr(builderIR,label.labBB);
  END;
  (* else the branches will be fixed up in set_label *)
END jump;

<*NOWARN*> PROCEDURE IfCommon(self: U;  t: IType;  l: Label;  f: Frequency; if_true : BOOLEAN) =
VAR
  s0 := Get(self.exprStack);
  curBB,negBB,posBB,exitBB : LLVM.BasicBlockRef;  
  a,cmpVal,zeroVal,lVal : LLVM.ValueRef;
  intType : LLVM.TypeRef;
  opName : TEXT;  
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
    intType := LLvmType(t);
    zeroVal := Zero(intType);

    IF if_true THEN
      opName := "if_true_cmp";
      cmpVal := LLVM.LLVMBuildICmp(builderIR,  LLVM.IntPredicate.NE, zeroVal, a, LT(opName));
    ELSE
      opName := "if_false_cmp";
      cmpVal := LLVM.LLVMBuildICmp(builderIR,  LLVM.IntPredicate.EQ, zeroVal, a, LT(opName));    
    END;

    curBB := LLVM.LLVMGetInsertBlock(builderIR);
    negBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT(opName));
    posBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT("not" & opName));
    exitBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT("exit"));

    LLVM.LLVMPositionBuilderAtEnd(builderIR,curBB);
    lVal := LLVM.LLVMBuildCondBr(builderIR,cmpVal,negBB,posBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,negBB);
 
    self.jump(l);
 
    lVal := LLVM.LLVMBuildBr(builderIR,exitBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,posBB);
    lVal := LLVM.LLVMBuildBr(builderIR,exitBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,exitBB);

    Pop(self.exprStack);
  END IfCommon;

<*NOWARN*> PROCEDURE if_true (self: U;  t: IType;  l: Label;  f: Frequency) =
(* tmp := s0.t; pop; IF (tmp # 0) GOTO l *)
  BEGIN
    self.ifCommon(t,l,f,TRUE);
  END if_true;

<*NOWARN*> PROCEDURE if_false (self: U;  t: IType;  l: Label;  f: Frequency) =
(* tmp := s0.t; pop; IF (tmp = 0) GOTO l *)
  BEGIN
    self.ifCommon(t,l,f,FALSE);
  END if_false;

<*NOWARN*> PROCEDURE if_compare (self: U;  t: ZType;  op: CompareOp;  l: Label;  f: Frequency) =
(*== compare(t, Int32, op); if_true(Int32, l,f)*)
(*alt  IF (s1.t op s0.t) GOTO l ; pop(2) *)
VAR
  (* using alt version *)
  s0 := Get(self.exprStack,0);
  s1 := Get(self.exprStack,1);
  a,b,cmpVal : LLVM.ValueRef;
  label : LabelObj;
  BEGIN
    a := NARROW(s1,LvExpr).lVal;
    b := NARROW(s0,LvExpr).lVal;

    cmpVal := CompareVal(a,b,op,t);

    label := self.getLabel(l,"IF_L");
    label.cmpInstr := cmpVal;
  
    label.elseBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT("ELSE_L" & Fmt.Int(l)));    
    label.condLv := LLVM.LLVMBuildCondBr(builderIR,cmpVal,label.labBB, label.elseBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,label.elseBB);
    
    Pop(self.exprStack,2);
  END if_compare;

PROCEDURE GetLabel(self : U; l : Label; name : TEXT) : LabelObj =
VAR
  label : LabelObj;
  labRef : REFANY;
  labelExists : BOOLEAN;
  BEGIN
    labelExists := self.labelTable.get(l,labRef);
    IF NOT labelExists THEN 
      label := NEW(LabelObj,branchList := NEW(RefSeq.T).init());
      EVAL self.labelTable.put(l,label);
    ELSE
      label := NARROW(labRef,LabelObj);    
    END;
    IF label.labBB = NIL THEN
      label.labBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT(name & Fmt.Int(l)));
    END;
    RETURN label;
  END GetLabel;

<*NOWARN*> PROCEDURE case_jump (self: U;  t: IType;  READONLY labels: ARRAY OF Label) =
(* tmp := s0.t; pop; GOTO labels[tmp]  (NOTE: no range checking on s0.t) *)
VAR
  s0 := Get(self.exprStack,0);
  a : LLVM.ValueRef;
  numCases,elseLab : CARDINAL;
  switchLVal,intVal : LLVM.ValueRef;
  label : LabelObj;
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
  
    (* this depends on case values normalised to range from zero up and also that the default label is the one before labels, which the front end seems to do *)
    elseLab := labels[0] - 1;
    label := self.getLabel(elseLab,"CASE_ELSE");
    numCases := NUMBER(labels);
    switchLVal := LLVM.LLVMBuildSwitch(builderIR, a, label.labBB, numCases);
    FOR i := FIRST(labels) TO LAST(labels) DO
      label := self.getLabel(labels[i],"CASE_L");
      intVal := LLVM.LLVMConstInt(IntPtrTy, VAL(i,LONGINT), TRUE);
      LLVM.LLVMAddCase(switchLVal,intVal,label.labBB);
    END;
    Pop(self.exprStack);
  END case_jump;

PROCEDURE exit_proc (self: U;  t: Type) =
(* Returns s0.t if t is not Void, otherwise returns no value. *)
VAR
  s0 : REFANY;
  expr : LvExpr;
  stackVal,retRef : LLVM.ValueRef;
  destTy : LLVM.TypeRef;
  BEGIN
    IF t = Type.Void THEN
      <*ASSERT self.exprStack.size() = 0 *> 
      retRef := LLVM.LLVMBuildRetVoid(builderIR);
    ELSE
      <*ASSERT self.exprStack.size() = 1 *>  

      s0 := Get(self.exprStack);
      expr := NARROW(s0,LvExpr);
      stackVal := expr.lVal;

      IF t < Type.Addr THEN
        (* return char in decl and exit t is abi need to trunc *)
        IF TypeSize(self.curProc.returnType) < TypeSize(t) THEN
          destTy := LLvmType(self.curProc.returnType);
          stackVal := LLVM.LLVMBuildTrunc(builderIR,stackVal,destTy,
                                      LT("exit_trunc"));
        END;
      END;

      IF t = Type.Addr THEN
        stackVal := LLVM.LLVMBuildBitCast(builderIR, stackVal, AdrTy, LT("exit_toadr"));
      END;

      retRef := LLVM.LLVMBuildRet(builderIR,stackVal);
     (* need it to keep expression stack at 0 for labels calls and jmp *)
      Pop(self.exprStack);
    END;
  END exit_proc;


(*------------------------------------------------------------ load/store ---*)

(* getelementptr functions *)
PROCEDURE Gep(src,ofs : LLVM.ValueRef; const : BOOLEAN) : LLVM.ValueRef =
VAR
  gepVal : LLVM.ValueRef;
  indexTy : UNTRACED REF ARRAY OF LLVM.ValueRef;  
  indexRef : UNTRACED REF LLVM.ValueRef;    
  BEGIN
    indexTy := NEW(UNTRACED REF ARRAY OF LLVM.ValueRef, 1);
    indexRef := LOOPHOLE(ADR(indexTy[0]), UNTRACED REF LLVM.ValueRef);
    indexTy[0] := ofs;
    (* inbounds ?? *)
    IF NOT const THEN
      gepVal := LLVM.LLVMBuildGEP(builderIR, src, indexRef, 1, LT("gepIdx"));
    ELSE
      gepVal := LLVM.LLVMConstGEP(src, indexRef, 1);
    END;
    RETURN gepVal;
  END Gep;

PROCEDURE BuildGep(src : LLVM.ValueRef; o : ByteOffset) : LLVM.ValueRef =
VAR
  int8Val,gepVal,ofs : LLVM.ValueRef;
  BEGIN
    ofs := LLVM.LLVMConstInt(LLVM.LLVMInt64Type(), VAL(o,LONGINT), TRUE);  
    (* cast to i8* value if not already *)
    int8Val := LLVM.LLVMBuildBitCast(builderIR, src, AdrTy, LT("gep_toi8"));
    gepVal := Gep(int8Val,ofs,FALSE);
    RETURN gepVal;
  END BuildGep;

PROCEDURE BuildConstGep(src : LLVM.ValueRef; o : ByteOffset) : LLVM.ValueRef =
VAR
  int8Val,gepVal,ofs : LLVM.ValueRef;
  BEGIN
    ofs := LLVM.LLVMConstInt(LLVM.LLVMInt64Type(), VAL(o,LONGINT), TRUE);  
    int8Val := LLVM.LLVMConstBitCast(src, AdrTy);
    gepVal := Gep(int8Val,ofs,TRUE);
    RETURN gepVal;
  END BuildConstGep;

(* load and particularly store need testing and a clean up or redesign.
   had more grief with these 2 than any others. All that strong typing in llvm
*)
PROCEDURE load (self: U;  v: Var;  o: ByteOffset;  t: MType;  u: ZType) =
(* push; s0.u := Mem [ ADR(v) + o ].t ; *)
VAR
  src : LvVar;
  srcVal,destVal : LLVM.ValueRef;
  destTy : LLVM.TypeRef;
  BEGIN
    src := NARROW(v,LvVar);
    srcVal := src.lv;

    IF src.type < Type.Addr THEN
      (* nothing *)
    ELSIF src.type = Type.Struct THEN 
      IF o > 0 THEN
        srcVal := BuildGep(srcVal,o);
      END;
      IF t # Type.Addr THEN
        srcVal := LLVM.LLVMBuildBitCast(builderIR, srcVal, PtrTy, LT("load_toptr"));
      ELSE
        srcVal := LLVM.LLVMBuildBitCast(builderIR, srcVal, AdrAdrTy, LT("load_adradr"));
      END;
    ELSIF src.type = Type.Addr THEN
      IF o > 0 THEN
        srcVal := BuildGep(srcVal,o);
(* maybe delete this case never seems to be called*)
        <*ASSERT FALSE *>
      END;
    ELSE
      <*ASSERT FALSE *>
    END;

    destVal := LLVM.LLVMBuildLoad(builderIR, srcVal, VarName(v)); 
    LLVM.LLVMSetAlignment(destVal,src.align);
  
    (* only load 64 or 32 bit sizes *)
    IF TypeSize(t) < TypeSize(u) THEN
      destTy := LLvmType(u);
      IF WordTypes(t) THEN
        destVal := LLVM.LLVMBuildZExt(builderIR,destVal,destTy, LT("load_zext"));
      ELSE
        destVal := LLVM.LLVMBuildSExt(builderIR,destVal,destTy, LT("load_sext"));    
      END;
    END;  

    Push(self.exprStack,NEW(LvExpr,lVal := destVal));
  END load;

PROCEDURE store (self: U;  v: Var;  o: ByteOffset;  t: ZType;  u: MType) =
(* Mem [ ADR(v) + o ].u := s0.t; pop *)
VAR
  s0 := Get(self.exprStack);
  src := NARROW(s0,LvExpr);
  dest := NARROW(v,LvVar);
  srcTy,destTy,srcPtrTy : LLVM.TypeRef;
  srcVal,destVal,storeVal : LLVM.ValueRef;
  
(* debug *)
  destty : LLVM.TypeRef;
  styKind,dtyKind : LLVM.TypeKind;
  stypeSize,dtypeSize : LONGCARD;
  BEGIN 
    srcVal := src.lVal;
    destVal := dest.lv;
    <*ASSERT srcVal # NIL *>
    <*ASSERT destVal # NIL *>    
    srcTy := LLVM.LLVMTypeOf(srcVal);

(* debug 
    IO.Put("STORE debug\n");
    IO.PutInt(ORD(dest.type));
*) 
  
styKind := LLVM.LLVMGetTypeKind(srcTy);
stypeSize := LLVM.LLVMStoreSizeOfType(targetData, srcTy);
destty := LLVM.LLVMTypeOf(destVal);
dtyKind := LLVM.LLVMGetTypeKind(destty);
dtypeSize := LLVM.LLVMStoreSizeOfType(targetData, destty);
  
(*
  IO.Put("store src type " & Fmt.Int(ORD(styKind)) & " " & Fmt.LongInt(stypeSize) & "\n");
  IO.Put("store dest type " & Fmt.Int(ORD(dtyKind)) & " " & Fmt.LongInt(dtypeSize) & "\n");

    LLVM.LLVMDumpValue(srcVal);
    LLVM.LLVMDumpValue(destVal);
    IO.Put("END Store debug\n");
*)

    IF dest.type < Type.Addr THEN
      <*ASSERT t < Type.Addr *>
  
      (* if typesize dest < typesize src then trunc the src to match the dest
         but only if int or word not reel which havent done yet*)
      IF TypeSize(u) < TypeSize(t) THEN
        destTy := LLvmType(u);
        srcVal := LLVM.LLVMBuildTrunc(builderIR,srcVal,destTy,
                                      LT("store_trunc"));
      ELSE
        destTy := LLvmType(u);
        srcVal := LLVM.LLVMBuildSExt(builderIR,srcVal,destTy,
                                      LT("store_sext"));
      END;
    ELSIF dest.type = Type.Addr THEN 
      (* check this. is it needed for addr? think so but need test case *)
      IF o > 0 THEN
        destVal := BuildGep(destVal,o);
        <*ASSERT FALSE *>      
      END;
      srcVal := LLVM.LLVMBuildBitCast(builderIR, srcVal, AdrTy, LT("store_toadr"));
    ELSIF dest.type = Type.Struct THEN
      IF o > 0 THEN
        destVal := BuildGep(destVal,o);
      END;

      IF styKind = LLVM.TypeKind.Pointer THEN
        srcPtrTy := LLVM.LLVMPointerType(srcTy);  
        destVal := LLVM.LLVMBuildBitCast(builderIR, destVal, srcPtrTy (* was AdrAdrTy*), LT("store_toptrptr"));            
      ELSE
        destVal := LLVM.LLVMBuildBitCast(builderIR, destVal, PtrTy, LT("store_toptr"));        
      END;
    ELSE
      <*ASSERT FALSE*>
    END;
  
    storeVal := LLVM.LLVMBuildStore(builderIR, srcVal, destVal);
    LLVM.LLVMSetAlignment(storeVal,dest.align);
  
    Pop(self.exprStack);
  END store;
  
  
PROCEDURE load_address (self: U;  v: Var;  o: ByteOffset) =
(* push; s0.A := ADR(v) + o *)
VAR
  srcVar := NARROW(v,LvVar);
  srcVal : LLVM.ValueRef;
  BEGIN
    srcVal := srcVar.lv;
    IF o > 0 THEN
      srcVal := BuildGep(srcVal,o);
    END;  
    (* srcVar.lv := srcVal not a good idea changing the def of the var *)
    Push(self.exprStack,NEW(LvExpr,lVal := srcVal));
  END load_address;

PROCEDURE load_indirect (self: U;  o: ByteOffset;  t: MType;  u: ZType) =
(* s0.u := Mem [s0.A + o].t  *)
VAR
  s0 := Get(self.exprStack);
  src := NARROW(s0,LvExpr);  
  srcVal : LLVM.ValueRef;
  srcTy,destTy : LLVM.TypeRef;
  BEGIN
    srcVal := src.lVal;
    IF o > 0 THEN
      srcVal := BuildGep(srcVal,o);
    END;

    srcTy := LLvmType(t);
    srcTy := LLVM.LLVMPointerType(srcTy);  

    srcVal := LLVM.LLVMBuildBitCast(builderIR, srcVal, srcTy, LT("load_ind_toptr"));
   
    src.lVal := LLVM.LLVMBuildLoad(builderIR, srcVal, LT("load_ind"));
   
    (* only load 64 or 32 bit sizes 
       and can make this a proc since in load as well *)
   
    IF TypeSize(t) < TypeSize(u) THEN
      destTy := LLvmType(u);
      IF WordTypes(t) THEN
        src.lVal := LLVM.LLVMBuildZExt(builderIR,src.lVal,destTy,
                              LT("load_ind_zext"));
      ELSE
        src.lVal := LLVM.LLVMBuildSExt(builderIR,src.lVal,destTy,
                              LT("load_ind_sext"));    
      END;
    END;  
  
(* fixme set alignment on load where is the value?? unless its the size of
the type *)
(*
  LLVM.LLVMSetAlignment(src.lVal,src.align);
*)         
    Put(self.exprStack,0,s0);
  END load_indirect;

PROCEDURE store_indirect (self: U;  o: ByteOffset;  t: ZType;  u: MType) =
(* Mem [s1.A + o].u := s0.t; pop (2) *)
VAR
  s0 := Get(self.exprStack,0);
  s1 := Get(self.exprStack,1);
  src := NARROW(s0,LvExpr);  
  dest := NARROW(s1,LvExpr); 
  srcVal,destVal : LLVM.ValueRef;
  destTy,destPtrTy : LLVM.TypeRef;
  BEGIN
    srcVal := src.lVal;
    destVal := dest.lVal;
    IF o > 0 THEN
      destVal := BuildGep(destVal,o);  
    END;

    destTy := LLvmType(u);
    destPtrTy := LLVM.LLVMPointerType(destTy);  
   
    IF TypeSize(u) < TypeSize(t) THEN
      srcVal := LLVM.LLVMBuildTrunc(builderIR,srcVal,destTy,
                                      LT("store_ind_trunc"));
    END;
   
    (* casting all values assuming the dest type is i8* *)
    destVal := LLVM.LLVMBuildBitCast(builderIR, destVal, destPtrTy, LT("store_ind_toptr"));   
   
    dest.lVal := LLVM.LLVMBuildStore(builderIR, srcVal, destVal); 
(*    
 LLVM.LLVMDumpValue(destVal);
*)    
(* fixme set alignment on store where is the value?? unless its the size of
the type *)
(*
  LLVM.LLVMSetAlignment(dest.lVal,dest.align);
*)        
    Pop(self.exprStack,2);
  END store_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil (self: U) =
 (*push; s0.A := NIL*)
VAR
  lVal : LLVM.ValueRef;
BEGIN
  lVal := LLVM.LLVMConstNull(AdrTy); (* all zeroes *)
  Push(self.exprStack,NEW(LvExpr,lVal := lVal));
  END load_nil;

PROCEDURE load_integer (self: U;  t: IType;  READONLY i: Target.Int) =
  (*push; s0.t := i *)
VAR
  intTy : LLVM.TypeRef;
  lVal : LLVM.ValueRef;
  int : INTEGER;
  res : BOOLEAN;
  BEGIN
    intTy := LLvmType(t);
    res := TInt.ToInt (i, int);
    <*ASSERT res = TRUE *>
    lVal := LLVM.LLVMConstInt(intTy, VAL(int,LONGINT), TRUE);
    Push(self.exprStack,NEW(LvExpr,lVal := lVal)); 
  END load_integer;

PROCEDURE ConvertFloat (f : Target.Float) : TEXT =
  VAR
    lastCh : INTEGER;
    buf : ARRAY[0..100] OF CHAR;
    result : TEXT;
  BEGIN
    lastCh := TFloat.ToChars(f,buf);
    buf[lastCh] := '\000';
    (* llvm uses E as exponent char *)
    FOR i := 0 TO lastCh - 1 DO
      IF buf[i] = 'X' THEN buf[i] := 'E'; END;
    END;
(* this prob work for real and longreal but need to test extended conversions
and limits etc with proper numbers *)
    result := Text.FromChars(SUBARRAY(buf,0,lastCh));
    RETURN result;
  END ConvertFloat;
  
PROCEDURE load_float (self: U;  t: RType;  READONLY f: Target.Float) =
  (*push; s0.t := f *)
VAR
  realTy : LLVM.TypeRef;
  lVal : LLVM.ValueRef;
  realStr : TEXT;
  BEGIN
    realTy := LLvmType(t);
    realStr := ConvertFloat(f);
    lVal := LLVM.LLVMConstRealOfString(realTy, LT(realStr));
    Push(self.exprStack,NEW(LvExpr,lVal := lVal));
  END load_float;

(*------------------------------------------------------------ arithmetic ---*)

TYPE
  BinOps = {add,sub,mul,div,mod,and,or,xor,shl,shr};
  BuildProc = PROCEDURE(B: LLVM.BuilderRef; LHS, RHS: LLVM.ValueRef;
                   Name: Ctypes.const_char_star): LLVM.ValueRef;

PROCEDURE binop(self: U; type: AType; op : BinOps) = 
VAR
  s0 := Get(self.exprStack,0);
  s1 := Get(self.exprStack,1);
  proc : BuildProc;
  a,b,lVal : LLVM.ValueRef;
  opName : Ctypes.char_star;
BEGIN
  a := NARROW(s1,LvExpr).lVal;
  b := NARROW(s0,LvExpr).lVal;
  IF type < Type.Reel THEN
    IF WordTypes(type) THEN
      CASE op OF
      | BinOps.add => proc := LLVM.LLVMBuildNUWAdd; opName := LT("uadd"); 
      | BinOps.sub => proc := LLVM.LLVMBuildNUWSub; opName := LT("usub");
      | BinOps.mul => proc := LLVM.LLVMBuildNUWMul; opName := LT("umul");
      | BinOps.div => proc := LLVM.LLVMBuildUDiv; opName := LT("udiv");
      | BinOps.mod => proc := LLVM.LLVMBuildURem; opName := LT("umod");
      | BinOps.and => proc := LLVM.LLVMBuildAnd; opName := LT("uand");
      | BinOps.or  => proc := LLVM.LLVMBuildOr; opName := LT("uor");
      | BinOps.xor => proc := LLVM.LLVMBuildXor; opName := LT("uxor");
      | BinOps.shl => proc := LLVM.LLVMBuildShl; opName := LT("ushl");
      | BinOps.shr => proc := LLVM.LLVMBuildLShr; opName := LT("ushr");
      END;      
    ELSE
      CASE op OF
      | BinOps.add => proc := LLVM.LLVMBuildNSWAdd; opName := LT("add");
      | BinOps.sub => proc := LLVM.LLVMBuildNSWSub; opName := LT("sub");
      | BinOps.mul => proc := LLVM.LLVMBuildNSWMul; opName := LT("mul");
      | BinOps.div => proc := LLVM.LLVMBuildSDiv; opName := LT("div");
      | BinOps.mod => proc := LLVM.LLVMBuildSRem; opName := LT("mod");
      | BinOps.and => proc := LLVM.LLVMBuildAnd; opName := LT("and");
      | BinOps.or  => proc := LLVM.LLVMBuildOr; opName := LT("or");
      | BinOps.xor => proc := LLVM.LLVMBuildXor; opName := LT("xor");
      | BinOps.shl => proc := LLVM.LLVMBuildShl; opName := LT("shl");
      | BinOps.shr => proc := LLVM.LLVMBuildLShr; opName := LT("shr");
      END;
    END; (* not word types *) 
  ELSE  (* real types *)
    CASE op OF
    | BinOps.add => proc := LLVM.LLVMBuildFAdd; opName := LT("fadd");
    | BinOps.sub => proc := LLVM.LLVMBuildFSub; opName := LT("fsub");
    | BinOps.mul => proc := LLVM.LLVMBuildFMul; opName := LT("fmul");
    | BinOps.div => proc := LLVM.LLVMBuildFDiv; opName := LT("fdiv");
    | BinOps.mod => proc := LLVM.LLVMBuildFRem; opName := LT("fmod");
    ELSE (* error *)
      <*ASSERT FALSE *>
    END;
  END;
  lVal := proc(builderIR,a,b,opName);
  NARROW(s1,LvExpr).lVal := lVal;
  Pop(self.exprStack);
END binop;

PROCEDURE CompareVal(a,b : LLVM.ValueRef; op : CompareOp; t : Type) : LLVM.ValueRef =
VAR
  cmpVal : LLVM.ValueRef;
  top : LLVM.IntPredicate;
  topr : LLVM.RealPredicate;
BEGIN
  IF t < Type.Reel OR t = Type.Addr THEN
    IF WordTypes(t) THEN
      CASE op OF
      | CompareOp.EQ => top := LLVM.IntPredicate.EQ;        
      | CompareOp.NE => top := LLVM.IntPredicate.NE;          
      | CompareOp.GT => top := LLVM.IntPredicate.UGT;
      | CompareOp.GE => top := LLVM.IntPredicate.UGE;
      | CompareOp.LT => top := LLVM.IntPredicate.ULT;
      | CompareOp.LE => top := LLVM.IntPredicate.ULE;
      END;      
    ELSE
      CASE op OF
      | CompareOp.EQ => top := LLVM.IntPredicate.EQ;        
      | CompareOp.NE => top := LLVM.IntPredicate.NE;  
      | CompareOp.GT => top := LLVM.IntPredicate.SGT;
      | CompareOp.GE => top := LLVM.IntPredicate.SGE;
      | CompareOp.LT => top := LLVM.IntPredicate.SLT;
      | CompareOp.LE => top := LLVM.IntPredicate.SLE;
      END;
    END;
    cmpVal := LLVM.LLVMBuildICmp(builderIR, top, a, b, LT("icmp"));
  ELSE
(* check if these are correct ie does ordered make any difference ??*)
    CASE op OF
    | CompareOp.EQ => topr := LLVM.RealPredicate.OEQ;
    | CompareOp.NE => topr := LLVM.RealPredicate.ONE;
    | CompareOp.GT => topr := LLVM.RealPredicate.OGT;
    | CompareOp.GE => topr := LLVM.RealPredicate.OGE;
    | CompareOp.LT => topr := LLVM.RealPredicate.OLT;
    | CompareOp.LE => topr := LLVM.RealPredicate.OLE;
    END;
    cmpVal := LLVM.LLVMBuildFCmp(builderIR, topr, a, b, LT("fcmp"));
  END;
  RETURN cmpVal;
END CompareVal;

<*NOWARN*> PROCEDURE compare (self: U;  t: ZType;  u: IType;  op: CompareOp) =
 (* s1.u := (s1.t op s0.t); pop   *)
VAR
  s1 := Get(self.exprStack,1);
  s0 := Get(self.exprStack,0);
  a,b,cmpVal,res : LLVM.ValueRef;
  genIf : IfThenObj;
  BEGIN
    a := NARROW(s1,LvExpr).lVal;
    b := NARROW(s0,LvExpr).lVal;

    cmpVal := CompareVal(a,b,op,t);

    genIf := NEW(IfThenObj,cmpVal := cmpVal,opName := "comp", curProc := self.curProc);
    genIf.tmpLv := LLVM.LLVMBuildAlloca(builderIR, i8Type, LT("cmptmp"));   
  
    genIf.genInit();

    EVAL LLVM.LLVMBuildStore(builderIR, One(i8Type), genIf.tmpLv);
    genIf.genThen();
    EVAL LLVM.LLVMBuildStore(builderIR, Zero(i8Type), genIf.tmpLv);
    genIf.genExit();
    res := LLVM.LLVMBuildLoad(builderIR, genIf.tmpLv, LT("compare"));

    NARROW(s1,LvExpr).lVal := res;
    Pop(self.exprStack);
  END compare;

PROCEDURE add (self: U;  t: AType) =
  (* s1.t := s1.t + s0.t ; pop *)
  BEGIN  
    binop(self,t,BinOps.add);
  END add;

PROCEDURE subtract (self: U;  t: AType) =
  (* s1.t := s1.t - s0.t ; pop *)
  BEGIN
    binop(self,t,BinOps.sub);
  END subtract;

PROCEDURE multiply (self: U;  t: AType) =
  (* s1.t := s1.t * s0.t ; pop *)
  BEGIN 
    binop(self,t,BinOps.mul);
  END multiply;

PROCEDURE divide (self: U;  t: RType) =
 (* s1.t := s1.t / s0.t; pop *)
  BEGIN
    binop(self,t,BinOps.div);
  END divide;

PROCEDURE negate (self: U;  t: AType) =
 (* s0.t := - s0.t *)
VAR
  s0 := Get(self.exprStack,0);
  a,lVal : LLVM.ValueRef;
  BEGIN 
    a := NARROW(s0,LvExpr).lVal;
    IF t < Type.Reel THEN
      lVal := LLVM.LLVMBuildNSWNeg(builderIR, a, LT("neg"));
    ELSE
      lVal := LLVM.LLVMBuildFNeg(builderIR, a, LT("fneg"));
    END;
    NARROW(s0,LvExpr).lVal := lVal;
  END negate;

PROCEDURE abs (self: U;  t: AType) =
VAR
  s0 := Get(self.exprStack,0);
  opType : LLVM.TypeRef;
  a,cmpVal,negRef : LLVM.ValueRef;
  genIf : IfThenObj;
  res : LLVM.ValueRef;
  intType : BOOLEAN;  
  BEGIN
    IF NOT WordTypes(t) THEN
      a := NARROW(s0,LvExpr).lVal;
      opType := LLvmType(t);
      intType := t < Type.Reel;

      IF intType THEN
        cmpVal := LLVM.LLVMBuildICmp(builderIR,  LLVM.IntPredicate.SLT, a, Zero(opType), LT("icmp"));
      ELSE
        cmpVal := LLVM.LLVMBuildFCmp(builderIR,  LLVM.RealPredicate.OLT, a, Zero(opType), LT("fcmp"));      
      END;

      genIf := NEW(IfThenObj,cmpVal := cmpVal,opName := "abs", curProc := self.curProc);

      genIf.tmpLv := LLVM.LLVMBuildAlloca(builderIR, opType, LT("tmp"));   

      genIf.genInit();
      IF intType THEN
        negRef := LLVM.LLVMBuildNSWNeg(builderIR, a, LT("ineg"));
      ELSE
        negRef := LLVM.LLVMBuildFNeg(builderIR, a, LT("fneg"));      
      END;      
      EVAL LLVM.LLVMBuildStore(builderIR, negRef, genIf.tmpLv);

      genIf.genThen();
      EVAL LLVM.LLVMBuildStore(builderIR, a, genIf.tmpLv);
      genIf.genExit();
      res := LLVM.LLVMBuildLoad(builderIR, genIf.tmpLv, LT("abs"));

      NARROW(s0,LvExpr).lVal := res;
    END;  
  END abs;
    
PROCEDURE MinMax (self: U;  t: ZType; doMin : BOOLEAN) =
VAR
  s0 := Get(self.exprStack,0);
  s1 := Get(self.exprStack,1);
  opType : LLVM.TypeRef;
  a,b,cmpVal,res : LLVM.ValueRef;
  genIf : IfThenObj;
  intType : BOOLEAN;  
  opName : TEXT;
  ipred : LLVM.IntPredicate;
  rpred : LLVM.RealPredicate;
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
    b := NARROW(s1,LvExpr).lVal;
    opType := LLvmType(t);
    intType := t < Type.Reel;
    
    IF doMin THEN 
      opName := "min";
      ipred := LLVM.IntPredicate.SLT;
      rpred := LLVM.RealPredicate.OLT 
    ELSE
      opName := "max";
      ipred := LLVM.IntPredicate.SGT;
      rpred := LLVM.RealPredicate.OGT
    END;

    IF intType THEN
      cmpVal := LLVM.LLVMBuildICmp(builderIR,  ipred , a, b, LT("icmp"));
    ELSE
      cmpVal := LLVM.LLVMBuildFCmp(builderIR,  rpred , a, b, LT("fcmp"));      
    END;
    
    genIf := NEW(IfThenObj,cmpVal := cmpVal,opName := opName, curProc := self.curProc);   
    genIf.tmpLv := LLVM.LLVMBuildAlloca(builderIR, opType, LT("tmp"));   
 
    genIf.genInit();
    EVAL LLVM.LLVMBuildStore(builderIR, a, genIf.tmpLv);
    genIf.genThen();
    EVAL LLVM.LLVMBuildStore(builderIR, b, genIf.tmpLv);
    genIf.genExit();
    res := LLVM.LLVMBuildLoad(builderIR, genIf.tmpLv, LT(opName));

    NARROW(s1,LvExpr).lVal := res;
    Pop(self.exprStack);  
  END MinMax;

PROCEDURE max (self: U;  t: ZType) =
  (* s1.t := MAX (s1.t, s0.t); pop *)
  BEGIN
    MinMax(self,t,FALSE);
  END max;

PROCEDURE min (self: U;  t: ZType) =
 (* s1.t := MIN (s1.t, s0.t); pop *)
  BEGIN
    MinMax(self,t,TRUE);  
  END min;

PROCEDURE FExt(t : RType) : TEXT =
  BEGIN
    CASE t OF
    |  Type.Reel  => RETURN ".f32"; 
    |  Type.LReel => RETURN ".f64";
    |  Type.XReel => RETURN ".f80";
    END; 
  END FExt;

PROCEDURE IntrinsicRealTypes(t : RType) : UNTRACED REF LLVM.TypeRef =
VAR
  indexTy := NEW(UNTRACED REF ARRAY OF LLVM.TypeRef, 1);
  Types := LOOPHOLE(ADR(indexTy[0]), UNTRACED REF LLVM.TypeRef);  
  BEGIN
    CASE t OF
    |  Type.Reel  => indexTy[0] := LLVM.LLVMFloatType();  
    |  Type.LReel => indexTy[0] := LLVM.LLVMDoubleType();
    |  Type.XReel => indexTy[0] := LLVM.LLVMX86FP80Type();
    END; 
    RETURN Types;
  END IntrinsicRealTypes;

PROCEDURE IntrinsicMemTypes(p1,p2,p3 : LLVM.TypeRef) : UNTRACED REF LLVM.TypeRef =
VAR
  indexTy := NEW(UNTRACED REF ARRAY OF LLVM.TypeRef, 3);
  Types := LOOPHOLE(ADR(indexTy[0]), UNTRACED REF LLVM.TypeRef);  
  BEGIN
    indexTy[0] := p1;
    indexTy[1] := p2;   
    indexTy[2] := p3;      
    RETURN Types;
  END IntrinsicMemTypes;
   
PROCEDURE MemSetFn() : LLVM.ValueRef =
CONST
(*delme  memset = 1279; *)
  iName = "llvm.memset.p0i8.i64";  
VAR
  memsetId := GetIntrinsicId(iName);
  (* 2 types for overloaded memset *)
  Types := IntrinsicMemTypes(AdrTy,IntPtrTy,NIL);  
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,memsetId,Types,2);  
  END MemSetFn;

PROCEDURE MemCopyFn() : LLVM.ValueRef =
CONST
(*delme  memcpy = 1277; *)
  iName = "llvm.memcpy.p0i8.p0i8.i64";
VAR
  memcpyId := GetIntrinsicId(iName);
  (* 3 types for overloaded memcpy *)
  Types := IntrinsicMemTypes(AdrTy,AdrTy,IntPtrTy);  
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,memcpyId,Types,3);  
  END MemCopyFn;

PROCEDURE MemMoveFn() : LLVM.ValueRef =
CONST
(*delme  memmov = 1278; *)
  iName = "llvm.memmove.p0i8.p0i8.i64";
VAR
  memmovId := GetIntrinsicId(iName);
  (* 3 types for overloaded memmov *)
  Types := IntrinsicMemTypes(AdrTy,AdrTy,IntPtrTy);  
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,memmovId,Types,3);  
  END MemMoveFn;

PROCEDURE RoundFn(t : RType) : LLVM.ValueRef =
CONST
(*delme  round = 3053; *)
  iName = "llvm.round";
VAR
  roundId := GetIntrinsicId(iName & FExt(t));
  Types := IntrinsicRealTypes(t);  
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,roundId,Types,1);  
  END RoundFn;

PROCEDURE TruncFn(t : RType) : LLVM.ValueRef =
CONST
(*delme  trunc = 3067; *)
  iName = "llvm.trunc";   
VAR
  truncId := GetIntrinsicId(iName & FExt(t));
  Types := IntrinsicRealTypes(t);  
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,truncId,Types,1);  
  END TruncFn;

PROCEDURE FloorFn(t : RType) : LLVM.ValueRef =
CONST
(*delme  floor = 407; *)
  iName = "llvm.floor";  
VAR
  floorId := GetIntrinsicId(iName & FExt(t));
  Types := IntrinsicRealTypes(t);  
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,floorId,Types,1);  
  END FloorFn;

PROCEDURE CeilFn(t : RType) : LLVM.ValueRef =
CONST
(*delme  ceil = 367; *)
  iName = "llvm.ceil";
VAR
  ceilId := GetIntrinsicId(iName & FExt(t));
  Types := IntrinsicRealTypes(t);  
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,ceilId,Types,1);  
  END CeilFn;

PROCEDURE DoCvtInt(var : LLVM.ValueRef; op : ConvertOp; t : RType) : LLVM.ValueRef =
CONST
  numParams = 1;
VAR
  res,fn : LLVM.ValueRef;
  callArgs: UNTRACED REF ARRAY OF LLVM.ValueRef;  
  paramRef : UNTRACED REF LLVM.ValueRef := NIL;
  BEGIN
  (* test me *)
    callArgs := NEW(UNTRACED REF ARRAY OF LLVM.ValueRef, numParams);
    callArgs[0] := var; 
    paramRef := LOOPHOLE(ADR(callArgs[0]), UNTRACED REF LLVM.ValueRef);

    CASE op OF
    | ConvertOp.Round => fn := RoundFn(t);
    | ConvertOp.Trunc => fn := TruncFn(t);
    | ConvertOp.Floor => fn := FloorFn(t);
    | ConvertOp.Ceiling => fn := CeilFn(t);
    END;
    res := LLVM.LLVMBuildCall(builderIR, fn, paramRef, numParams, LT("cvtint"));    
    RETURN res;
  END DoCvtInt;

PROCEDURE cvt_int (self: U;  t: RType;  u: IType;  op: ConvertOp) =
(* s0.u := op (s0.t) *)
VAR
  s0 := Get(self.exprStack,0);
  a,lVal : LLVM.ValueRef;
  destTy : LLVM.TypeRef;
  BEGIN
    a := NARROW(s0,LvExpr).lVal;  
    destTy := LLvmType(u);
    lVal := DoCvtInt(a,op,t);
    lVal := LLVM.LLVMBuildFPToSI(builderIR, lVal, destTy, LT("cvt_toint"));   
    NARROW(s0,LvExpr).lVal := lVal;
  END cvt_int;

PROCEDURE cvt_float (self: U;  t: AType;  u: RType) =
 (* s0.u := FLOAT (s0.t, u) *)
VAR
  a,lVal : LLVM.ValueRef;
  realTy : LLVM.TypeRef;
  s0 := Get(self.exprStack,0);
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
    realTy := LLvmType(u);
    IF WordTypes(t) THEN
      lVal := LLVM.LLVMBuildUIToFP(builderIR, a, realTy, LT("ufloat"));
    ELSE
      lVal := LLVM.LLVMBuildSIToFP(builderIR, a, realTy, LT("sfloat"));  
    END;
    NARROW(s0,LvExpr).lVal := lVal;
  END cvt_float;

<*NOWARN*> PROCEDURE div (self: U;  t: IType;  a, b: Sign) =
 (* s1.t := s1.t DIV s0.t;pop*)
   VAR
     s0 : REFANY;
     res,one : LLVM.ValueRef;
  BEGIN
    (* check sign *)
    binop(self,t,BinOps.div);
    (* if result is neg then subtract 1 to agree with m3 floor def for DIV *)
    (* get the result from s0 and use code like docheck *)
    IF a = Sign.Negative OR b = Sign.Negative THEN
      s0 := Get(self.exprStack,0);
      res := NARROW(s0,LvExpr).lVal;
      one := LLVM.LLVMConstInt(LLvmType(t), VAL(1,LONGINT), TRUE);  
      res := LLVM.LLVMBuildNSWSub(builderIR, res, one, LT("div_sub"));   
      NARROW(s0,LvExpr).lVal := res;      
    END;
    IF a = Sign.Unknown OR b = Sign.Unknown THEN
      (* todo build runtime check and sub*)
    END;
  END div;

<*NOWARN*> PROCEDURE mod (self: U;  t: IType;  a, b: Sign) =
 (* s1.t := s1.t MOD s0.t;pop*)
  BEGIN
    binop(self,t,BinOps.mod);
    (* fixme for neg values see m3 def for mod *)
  END mod;

(*------------------------------------------------------------------ sets ---*)

(* common function to get 1,2 or 3 stack values for some set methods *)
PROCEDURE GetSetStackVals(self : U; all : BOOLEAN; VAR s0,s1,s2 : LLVM.ValueRef) =
VAR st0,st1,st2 : REFANY;
  BEGIN
    st0 := Get(self.exprStack,0);
    st1 := Get(self.exprStack,1);
    s0 := NARROW(st0,LvExpr).lVal;  
    s1 := NARROW(st1,LvExpr).lVal;    
    s0 := LLVM.LLVMBuildBitCast(builderIR, s0, PtrTy, LT("set_toptr"));
    s1 := LLVM.LLVMBuildBitCast(builderIR, s1, PtrTy, LT("set_toptr"));  
    IF all THEN
      st2 := Get(self.exprStack,2);
      s2 := NARROW(st2,LvExpr).lVal;      
      s2 := LLVM.LLVMBuildBitCast(builderIR, s2, PtrTy, LT("set_toptr"));    
    END;
  END GetSetStackVals;

PROCEDURE SetCall(fn : LLVM.ValueRef; numParams : INTEGER; p1,p2,p3,p4 : LLVM.ValueRef := NIL) : LLVM.ValueRef =
CONST maxParams = 4;
VAR
  callArgs: UNTRACED REF ARRAY OF LLVM.ValueRef;  
  paramRef : UNTRACED REF LLVM.ValueRef := NIL;
  res : LLVM.ValueRef;
  BEGIN
    callArgs := NEW(UNTRACED REF ARRAY OF LLVM.ValueRef, maxParams);
    callArgs[0] := p1; 
    callArgs[1] := p2; 
    callArgs[2] := p3; 
    callArgs[3] := p4; 
  
    paramRef := LOOPHOLE(ADR(callArgs[0]), UNTRACED REF LLVM.ValueRef);
    res := LLVM.LLVMBuildCall(builderIR, fn, paramRef, numParams, 
                          LT("")); 
    RETURN res;
  END SetCall;

PROCEDURE SetCommon(self : U; name : TEXT; VAR fn : LLVM.ValueRef; s : ByteSize) = 
VAR
  s0,s1,s2,sizeVal : LLVM.ValueRef;
  BEGIN
    fn := DeclSet(name,fn,4,FALSE);
    sizeVal := LLVM.LLVMConstInt(IntPtrTy, VAL(s,LONGINT), TRUE);  
    GetSetStackVals(self,TRUE,s0,s1,s2);
    EVAL SetCall(fn,4,sizeVal,s0,s1,s2);
    Pop(self.exprStack,3);    
  END SetCommon;

PROCEDURE set_union (self: U;  s: ByteSize) =
(* s2.B := s1.B + s0.B; pop(3) *)
  BEGIN
    SetCommon(self,"set_union",setUnion,s);
  END set_union;

PROCEDURE set_difference (self: U;  s: ByteSize) =
(* s2.B := s1.B - s0.B; pop(3) *)
  BEGIN
    SetCommon(self,"set_difference",setDifference,s);
  END set_difference;

PROCEDURE set_intersection (self: U;  s: ByteSize) =
(* s2.B := s1.B * s0.B; pop(3) *)
  BEGIN
   SetCommon(self,"set_intersection",setIntersection,s);
  END set_intersection;

PROCEDURE set_sym_difference (self: U;  s: ByteSize) =
(* s2.B := s1.B / s0.B; pop(3) *)
  BEGIN
    SetCommon(self,"set_sym_difference",setSymDifference,s);
  END set_sym_difference;

<*NOWARN*> PROCEDURE set_member (self: U;  s: ByteSize;  t: IType) =
(* s1.t := (s0.t IN s1.B); pop *)
VAR
  st0 := Get(self.exprStack,0);
  st1 := Get(self.exprStack,1);
  s0,s1,res : LLVM.ValueRef;  
  BEGIN
    s0 := NARROW(st0,LvExpr).lVal;  
    s1 := NARROW(st1,LvExpr).lVal;
    s1 := LLVM.LLVMBuildBitCast(builderIR, s1, PtrTy, LT("set_toptr")); 
    setMember := DeclSet("set_member",setMember,2,TRUE);
    res := SetCall(setMember,2,s0,s1);
    NARROW(st1,LvExpr).lVal := res;
    Pop(self.exprStack); 
  END set_member;
  
<*NOWARN*> PROCEDURE set_compare (self: U;  s: ByteSize;  op: CompareOp;  t: IType) =
(* s1.t := (s1.B op s0.B); pop *)
  VAR
    s0,s1,s2,res : LLVM.ValueRef;
    st1 := Get(self.exprStack,1);
    fn,size : LLVM.ValueRef;
    name : TEXT;
  BEGIN
    size := LLVM.LLVMConstInt(IntPtrTy, VAL(s,LONGINT), TRUE);  
    CASE op OF
    | CompareOp.EQ => fn := setEq; name := "set_eq";        
    | CompareOp.NE => fn := setNe; name := "set_ne";          
    | CompareOp.GT => fn := setGt; name := "set_gt";
    | CompareOp.GE => fn := setGe; name := "set_ge";
    | CompareOp.LT => fn := setLt; name := "set_lt";
    | CompareOp.LE => fn := setLe; name := "set_le";
    END;  
    fn := DeclSet(name,fn,3,TRUE);      
    GetSetStackVals(self,FALSE,s0,s1,s2);
    res := SetCall(fn,3,size,s0,s1);
    NARROW(st1,LvExpr).lVal := res;
    Pop(self.exprStack);  
  END set_compare;

<*NOWARN*> PROCEDURE set_range (self: U;  s: ByteSize;  t: IType) =
(* s2.A[s1.t..s0.t] := 1; pop(3) *)
VAR
  s0,s1,s2,res : LLVM.ValueRef;
  st0 := Get(self.exprStack,0);  
  st1 := Get(self.exprStack,1);
  st2 := Get(self.exprStack,2); 
  BEGIN
    s0 := NARROW(st0,LvExpr).lVal;  
    s1 := NARROW(st1,LvExpr).lVal;
    s2 := NARROW(st2,LvExpr).lVal;    
    s2 := LLVM.LLVMBuildBitCast(builderIR, s2, PtrTy, LT("set_toptr"));     
    setRange := DeclSet("set_range",setRange,3,TRUE);  
    res := SetCall(setRange,3,s1,s0,s2); (* check this order *)
    Pop(self.exprStack,3);  
  END set_range;

<*NOWARN*> PROCEDURE set_singleton (self: U;  s: ByteSize;  t: IType) =
 (* s1.A [s0.t] := 1; pop(2) *)
VAR
  st0 := Get(self.exprStack,0);
  st1 := Get(self.exprStack,1);
  s0,s1,res : LLVM.ValueRef;  
  BEGIN
    s0 := NARROW(st0,LvExpr).lVal;  
    s1 := NARROW(st1,LvExpr).lVal;
    s1 := LLVM.LLVMBuildBitCast(builderIR, s1, PtrTy, LT("set_toptr"));
    setSingleton := DeclSet("set_singleton",setSingleton,2,FALSE);      
    res := SetCall(setSingleton,2,s0,s1); 
    Pop(self.exprStack,2);  
  END set_singleton;
  
(*------------------------------------------------- Word.T bit operations ---*)

<*NOWARN*> PROCEDURE not (self: U;  t: IType) =
 (* s0.t := Word.Not (s0.t) *)
VAR
  s0 := Get(self.exprStack,0);
  a,lVal : LLVM.ValueRef;
  BEGIN 
    a := NARROW(s0,LvExpr).lVal;
    lVal := LLVM.LLVMBuildNot(builderIR, a, LT("not"));
    NARROW(s0,LvExpr).lVal := lVal;
  END not;

PROCEDURE and (self: U;  t: IType) =
  BEGIN
    binop(self,t,BinOps.and);
  END and;

PROCEDURE or (self: U;  t: IType) =
  BEGIN
    binop(self,t,BinOps.or);
  END or;

PROCEDURE xor (self: U;  t: IType) =
  BEGIN
    binop(self,t,BinOps.xor);
  END xor;
  
PROCEDURE shift (self: U;  t: IType) =
 (* s1.t := Word.Shift  (s1.t, s0.t); pop *)
VAR
  s0 := Get(self.exprStack,0);
  s1 := Get(self.exprStack,1);     
  a,shift,cmpVal,res : LLVM.ValueRef;
  opType : LLVM.TypeRef;  
  genIf : IfThenObj;
  BEGIN
    a := NARROW(s1,LvExpr).lVal;
    shift := NARROW(s0,LvExpr).lVal;
  
    opType := LLvmType(t);
  
    cmpVal := LLVM.LLVMBuildICmp(builderIR,  LLVM.IntPredicate.SGE, shift, Zero(i8Type), LT("sge"));

    genIf := NEW(IfThenObj,cmpVal := cmpVal,opName := "shft", curProc := self.curProc);
    genIf.tmpLv := LLVM.LLVMBuildAlloca(builderIR, opType, LT("tmp"));   
  
    genIf.genInit();
    res := LLVM.LLVMBuildShl(builderIR, a, shift, LT("shl"));   
    EVAL LLVM.LLVMBuildStore(builderIR, res, genIf.tmpLv);
    genIf.genThen();
  
    (* make the shift positive  *)
    shift := LLVM.LLVMBuildNSWNeg(builderIR, shift, LT("neg"));  
    res := LLVM.LLVMBuildLShr(builderIR, a, shift, LT("shr"));   
    EVAL LLVM.LLVMBuildStore(builderIR, res, genIf.tmpLv);
    genIf.genExit();

    res := LLVM.LLVMBuildLoad(builderIR, genIf.tmpLv, LT("shift"));
    NARROW(s1,LvExpr).lVal := res;

    Pop(self.exprStack);    
  END shift;

PROCEDURE shift_left (self: U;  t: IType) =
 (* s1.t := Word.Shift  (s1.t, s0.t); pop *)
  BEGIN
    binop(self,t,BinOps.shl);
  END shift_left;

PROCEDURE shift_right (self: U;  t: IType) =
 (* s1.t := Word.Shift  (s1.t, -s0.t); pop *)
  BEGIN
    binop(self,t,BinOps.shr);
  END shift_right;

(*
rotl( unsigned int value, int shift)
return (value << shift) | (value >> (sizeof(value) * CHAR_BIT - shift));

rotr(usnigned int value, int shift)
return (value >> shift) | (value << (sizeof(value) * CHAR_BIT - shift));
*)

PROCEDURE DoRotate(value,shift : LLVM.ValueRef; rotLeft : BOOLEAN) : LLVM.ValueRef =
VAR
  t1,t2,t3,t4,wordSize : LLVM.ValueRef;
  BEGIN
    wordSize := LLVM.LLVMConstInt(IntPtrTy, VAL(ptrBits,LONGINT), TRUE);
    t1 := LLVM.LLVMBuildNUWSub(builderIR, wordSize, shift, LT("sub"));

    IF rotLeft THEN
      t2 := LLVM.LLVMBuildLShr(builderIR, value, t1, LT("shr"));
      t3 := LLVM.LLVMBuildShl(builderIR, value, shift, LT("shl"));
    ELSE (* rot right *)
      t2 := LLVM.LLVMBuildShl(builderIR, value, t1, LT("shr"));  
      t3 := LLVM.LLVMBuildLShr(builderIR, value, shift, LT("shr"));   
    END;

    t4 := LLVM.LLVMBuildOr(builderIR, t2, t3, LT("or"));
    RETURN t4;
  END DoRotate;

PROCEDURE rotate (self: U;  t: IType) =
(* s1.t := Word.Rotate (s1.t, s0.t); pop *)
VAR
  s0 := Get(self.exprStack,0);
  s1 := Get(self.exprStack,1);     
  a,shift,cmpVal,res : LLVM.ValueRef;
  opType : LLVM.TypeRef;  
  genIf : IfThenObj;
  BEGIN
    a := NARROW(s1,LvExpr).lVal;
    shift := NARROW(s0,LvExpr).lVal;
    opType := LLvmType(t);

    cmpVal := LLVM.LLVMBuildICmp(builderIR,  LLVM.IntPredicate.SGE, shift, Zero(i8Type), LT("sge"));

    genIf := NEW(IfThenObj,cmpVal := cmpVal,opName := "rot", curProc := self.curProc);
    genIf.tmpLv := LLVM.LLVMBuildAlloca(builderIR, opType, LT("tmp"));   
  
    genIf.genInit();
    res := DoRotate(a,shift,TRUE);
  
    EVAL LLVM.LLVMBuildStore(builderIR, res, genIf.tmpLv);
    genIf.genThen();
  
    (* make the shift positive  *)
    shift := LLVM.LLVMBuildNSWNeg(builderIR, shift, LT("neg"));  
    res := DoRotate(a,shift,FALSE);
    EVAL LLVM.LLVMBuildStore(builderIR, res, genIf.tmpLv);

    genIf.genExit();

    res := LLVM.LLVMBuildLoad(builderIR, genIf.tmpLv, LT("rotate"));
    NARROW(s1,LvExpr).lVal := res;

    Pop(self.exprStack);  
  END rotate;

<*NOWARN*> PROCEDURE rotate_left (self: U;  t: IType) =
(* s1.t := Word.Rotate (s1.t, s0.t); pop *)
VAR
  s0 := Get(self.exprStack,0);
  s1 := Get(self.exprStack,1);
  a,shift,res : LLVM.ValueRef;   
  BEGIN
    a := NARROW(s1,LvExpr).lVal;
    shift := NARROW(s0,LvExpr).lVal;
    res := DoRotate(a,shift,TRUE);
    NARROW(s1,LvExpr).lVal := res;
    Pop(self.exprStack);
  END rotate_left;

<*NOWARN*> PROCEDURE rotate_right (self: U;  t: IType) =
(* s1.t := Word.Rotate (s1.t, -s0.t); pop *)
VAR
  s0 := Get(self.exprStack,0);
  s1 := Get(self.exprStack,1);
  a,shift,res : LLVM.ValueRef;   
  BEGIN
    a := NARROW(s1,LvExpr).lVal;
    shift := NARROW(s0,LvExpr).lVal;
    res := DoRotate(a,shift,FALSE);
    NARROW(s1,LvExpr).lVal := res;
    Pop(self.exprStack);
  END rotate_right;

PROCEDURE widen (self: U;  sign: BOOLEAN) =
(* s0.I64 := s0.I32;  IF sign THEN SignExtend s0;  *)
VAR
  s0 := Get(self.exprStack,0);
  a,lVal : LLVM.ValueRef;
  Int64Ty := LLVM.LLVMInt64Type();
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
    IF sign THEN
      lVal := LLVM.LLVMBuildSExt(builderIR, a, Int64Ty, LT("widen"));
    ELSE
      lVal := LLVM.LLVMBuildZExt(builderIR, a, Int64Ty, LT("widen"));
    END;
    NARROW(s0,LvExpr).lVal := lVal;
  END widen;

PROCEDURE chop (self: U) =
(* s0.I32 := Word.And (s0.I64, 16_ffffffff); *)
VAR
  s0 := Get(self.exprStack,0);
  a,lVal : LLVM.ValueRef;
  Int32Ty := LLVM.LLVMInt32Type();
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
    lVal := LLVM.LLVMBuildTrunc(builderIR, a, Int32Ty, LT("chop"));
    NARROW(s0,LvExpr).lVal := lVal;
  END chop;
    
PROCEDURE DoExtract(val,count,offset : LLVM.ValueRef; sign : BOOLEAN) : LLVM.ValueRef =
VAR
  t1,t2,t3,t4,wordSize : LLVM.ValueRef;
  BEGIN
    wordSize := LLVM.LLVMConstInt(IntPtrTy, VAL(ptrBits,LONGINT), TRUE);
    t1 := LLVM.LLVMBuildNSWSub(builderIR, wordSize, count, LT("len"));
    t2 := LLVM.LLVMBuildNSWSub(builderIR, t1, offset, LT("dist"));
    t3 := LLVM.LLVMBuildShl(builderIR, val, t2, LT("shl"));
    (* is this LShr or AShr ?? *)
    t4 := LLVM.LLVMBuildAShr(builderIR, t3, t1, LT("shr"));
    IF sign THEN
      t4 := LLVM.LLVMBuildSExt(builderIR, t4, IntPtrTy, LT("sext"));
    END;
    RETURN t4;
  END DoExtract;

<*NOWARN*> PROCEDURE extract (self: U;  t: IType;  sign: BOOLEAN) =
(* s2.t := Word.Extract(s2.t, s1.t, s0.t); IF sign THEN SignExtend s2; pop(2) *)
VAR
  s0 := Get(self.exprStack,0);
  s1 := Get(self.exprStack,1);
  s2 := Get(self.exprStack,2);
  a,offset,count,res : LLVM.ValueRef;
  BEGIN
    a := NARROW(s2,LvExpr).lVal;
    count := NARROW(s0,LvExpr).lVal;
    offset := NARROW(s1,LvExpr).lVal;
    res := DoExtract(a,count,offset,sign);
    NARROW(s2,LvExpr).lVal := res;
    Pop(self.exprStack,2);
  END extract;

<*NOWARN*> PROCEDURE extract_n (self: U;  t: IType;  sign: BOOLEAN;  n: CARDINAL) =
(* s1.t := Word.Extract(s1.t, s0.t, n);
   IF sign THEN SignExtend s1; pop(1) *)
VAR
  s0 := Get(self.exprStack,0);
  s1 := Get(self.exprStack,1);
  a,offset,count,res : LLVM.ValueRef;   
  BEGIN
    a := NARROW(s1,LvExpr).lVal;
    offset := NARROW(s0,LvExpr).lVal;
    count := LLVM.LLVMConstInt(IntPtrTy, VAL(n,LONGINT), TRUE);
    res := DoExtract(a,count,offset,sign);
    NARROW(s1,LvExpr).lVal := res;
    Pop(self.exprStack);
  END extract_n;

<*NOWARN*> PROCEDURE extract_mn (self: U;  t: IType;  sign: BOOLEAN;  m, n: CARDINAL) =
(* s0.t := Word.Extract(s0.t, m, n); IF sign THEN SignExtend s0 *)
VAR
  s0 := Get(self.exprStack,0);
  a,offset,count,res : LLVM.ValueRef;
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
    count := LLVM.LLVMConstInt(IntPtrTy, VAL(n,LONGINT), TRUE);
    offset := LLVM.LLVMConstInt(IntPtrTy, VAL(m,LONGINT), TRUE);
    res := DoExtract(a,count,offset,sign);
    NARROW(s0,LvExpr).lVal := res; 
  END extract_mn;

(* insert pseudo
result = ( ~(widthmask << offset) & target) |
          ( ( value & widthmask) << offset);
  
  where offset is the bit location from the left field in target and widthmask is its size so eg if have 3 fields A B C like AABBBCCC and want to replace B with contents of value then
  
  result = ( ~(0x7 << 3) & target) | (( value & 0x07) << 3);

*)
PROCEDURE DoInsert(value,target,widthMask,offset : LLVM.ValueRef) : LLVM.ValueRef =
VAR
  t1,t2,t3,t4,t5,t6 : LLVM.ValueRef;
  BEGIN  
    t1 := LLVM.LLVMBuildShl(builderIR, widthMask, offset, LT("ishl"));
    t2 := LLVM.LLVMBuildNot(builderIR, t1, LT("inot")); 
    t3 := LLVM.LLVMBuildAnd(builderIR, t2, target, LT("iand"));
    t4 := LLVM.LLVMBuildAnd(builderIR, widthMask, value, LT("iand"));
    t5 := LLVM.LLVMBuildShl(builderIR, t4, offset, LT("ishl"));
    t6 := LLVM.LLVMBuildOr(builderIR, t3, t5, LT("ior"));
    RETURN t6;
  END DoInsert;

<*NOWARN*> PROCEDURE insert (self: U;  t: IType) =
(* s3.t := Word.Insert (s3.t, s2.t, s1.t, s0.t); pop(3) *)
VAR
  s0 := Get(self.exprStack,0);
  s1 := Get(self.exprStack,1);
  s2 := Get(self.exprStack,2);  
  s3 := Get(self.exprStack,3);    
  value,target,offset,mask,ones,res,count,wordsize,width : LLVM.ValueRef;   
  BEGIN
    (* testme *)
    value := NARROW(s2,LvExpr).lVal;
    offset := NARROW(s1,LvExpr).lVal;  
    count := NARROW(s0,LvExpr).lVal;    
    target := NARROW(s3,LvExpr).lVal;
  
    (* here we produce a type with count bits a hack of sorts *)
    ones := LLVM.LLVMConstAllOnes(IntPtrTy);
    wordsize := LLVM.LLVMConstInt(IntPtrTy, ptrBits, TRUE);
    width := LLVM.LLVMBuildSub(builderIR, wordsize, count, LT("width"));
  
    ones := LLVM.LLVMBuildShl(builderIR, ones, width, LT("allones"));
    mask := LLVM.LLVMBuildLShr(builderIR, ones, width, LT("masklshr"));
  
    res := DoInsert(value,target,mask,offset);
    NARROW(s3,LvExpr).lVal := res;
    Pop(self.exprStack,3);
  END insert;

<*NOWARN*> PROCEDURE insert_n (self: U;  t: IType;  n: CARDINAL) =
(* s2.t := Word.Insert (s2.t, s1.t, s0.t, n); pop(2) *)
VAR
  s0 := Get(self.exprStack,0);
  s1 := Get(self.exprStack,1);
  s2 := Get(self.exprStack,2);  
  value,target,offset,mask,ones,res : LLVM.ValueRef;   
  maskTy : LLVM.TypeRef;
  BEGIN
    value := NARROW(s1,LvExpr).lVal;
    offset := NARROW(s0,LvExpr).lVal;  
    target := NARROW(s2,LvExpr).lVal;
    maskTy := LLVM.LLVMIntType(n);
    ones := LLVM.LLVMConstAllOnes(maskTy);
    mask := LLVM.LLVMConstZExt(ones, IntPtrTy);
    res := DoInsert(value,target,mask,offset);
    NARROW(s2,LvExpr).lVal := res;
    Pop(self.exprStack,2);
  END insert_n;

<*NOWARN*> PROCEDURE insert_mn (self: U;  t: IType;  m, n: CARDINAL) =
(* s1.t := Word.Insert (s1.t, s0.t, m, n); pop(1) *)
VAR
  s0 := Get(self.exprStack,0);
  s1 := Get(self.exprStack,1);
  value,target,offset,mask,ones,res : LLVM.ValueRef;   
  maskTy : LLVM.TypeRef;
  BEGIN
    value := NARROW(s0,LvExpr).lVal;
    target := NARROW(s1,LvExpr).lVal;
    offset := LLVM.LLVMConstInt(IntPtrTy, VAL(m,LONGINT), TRUE);
    maskTy := LLVM.LLVMIntType(n);
    ones := LLVM.LLVMConstAllOnes(maskTy);
    mask := LLVM.LLVMConstZExt(ones, IntPtrTy);
    res := DoInsert(value,target,mask,offset);
    NARROW(s1,LvExpr).lVal := res;
    Pop(self.exprStack);
  END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

<*NOWARN*> PROCEDURE swap (self: U;  a, b: Type) =
(* tmp := s1.a; s1.b := s0.b; s0.a := tmp *)
VAR
  s0 := Get(self.exprStack,0);
  s1 := Get(self.exprStack,1);
  tmp : REFANY; 
  BEGIN
    tmp := s1;
    Put(self.exprStack,1,s0);
    Put(self.exprStack,0,tmp);
  END swap;

<*NOWARN*> PROCEDURE pop (self: U;  t: Type) =
(* pop(1) discard s0, not its side effects *)
  BEGIN
    Pop(self.exprStack);
  END pop;

PROCEDURE DoMemCopy(src,dest,len : LLVM.ValueRef; align : INTEGER; overlap : BOOLEAN) =
CONST
  numParams = 5;
VAR
  alignVal,volatile : LLVM.ValueRef;
  callArgs: UNTRACED REF ARRAY OF LLVM.ValueRef;  
  paramRef : UNTRACED REF LLVM.ValueRef := NIL;
  BEGIN
    callArgs := NEW(UNTRACED REF ARRAY OF LLVM.ValueRef, numParams);
    paramRef := LOOPHOLE(ADR(callArgs[0]), UNTRACED REF LLVM.ValueRef);
    alignVal := LLVM.LLVMConstInt(LLVM.LLVMInt32Type(), VAL(align,LONGINT), TRUE);
    (* not sure about volatile do we ever use it? *)
    volatile := LLVM.LLVMConstInt(LLVM.LLVMInt1Type(), VAL(0,LONGINT), TRUE);
    src := LLVM.LLVMBuildBitCast(builderIR, src, AdrTy, LT("src_toi8"));
    dest := LLVM.LLVMBuildBitCast(builderIR, dest, AdrTy, LT("dest_toi8"));
    callArgs[0] := dest; 
    callArgs[1] := src;   
    callArgs[2] := len;  
    callArgs[3] := alignVal; 
    callArgs[4] := volatile; 

    IF overlap THEN
      EVAL LLVM.LLVMBuildCall(builderIR, MemMoveFn(), paramRef, numParams, LT(""));    
    ELSE
      EVAL LLVM.LLVMBuildCall(builderIR, MemCopyFn(), paramRef, numParams, LT(""));  
    END;
  END DoMemCopy;

<*NOWARN*> PROCEDURE copy_n (self: U;  u: IType;  t: MType;  overlap: BOOLEAN) =
(* copy s0.u units with 't's size and alignment from s1.A to s2.A; pop(3).
   'overlap' is true if the source and destination may partially overlap
   (ie. you need memmove, not just memcpy). *)
  VAR
    s0 := Get(self.exprStack,0);
    s1 := Get(self.exprStack,1);
    s2 := Get(self.exprStack,2);   
    src,dest,len,sizeVal : LLVM.ValueRef;   
    align : INTEGER;
  BEGIN
    src := NARROW(s1,LvExpr).lVal;
    dest := NARROW(s2,LvExpr).lVal;
    len := NARROW(s0,LvExpr).lVal;    
    align := TypeSize(t);    
    sizeVal := LLVM.LLVMConstInt(LLVM.LLVMInt64Type(), VAL(align,LONGINT), TRUE);
    len := LLVM.LLVMBuildNSWMul(builderIR,len,sizeVal,LT("copy_mul"));    
    DoMemCopy(src,dest,len,align,overlap);
    Pop(self.exprStack,3);                               
  END copy_n;

<*NOWARN*> PROCEDURE copy (self: U;  n: INTEGER;  t: MType;  overlap: BOOLEAN) =
(* copy 'n' units with 't's size and alignment from s0.A to s1.A; pop(2).
   'overlap' is true if the source and destination may partially overlap
   (ie. you need memmove, not just memcpy). *)
  VAR
    s0 := Get(self.exprStack,0);
    s1 := Get(self.exprStack,1);
    src,dest,len : LLVM.ValueRef;
    byteLen,align : INTEGER;
  BEGIN
    src := NARROW(s0,LvExpr).lVal;
    dest := NARROW(s1,LvExpr).lVal;
    byteLen := n * TypeSize(t);
    align := TypeSize(t);
    len := LLVM.LLVMConstInt(IntPtrTy, VAL(byteLen,LONGINT), TRUE);
    DoMemCopy(src,dest,len,align,overlap);
    Pop(self.exprStack,2);   
  END copy;

PROCEDURE DoMemZero(dest,len : LLVM.ValueRef; align : INTEGER) =
CONST
  numParams = 5;
VAR
  alignVal,volatile : LLVM.ValueRef;
  callArgs: UNTRACED REF ARRAY OF LLVM.ValueRef;  
  paramRef : UNTRACED REF LLVM.ValueRef := NIL;
  BEGIN
  (* testme *)
    callArgs := NEW(UNTRACED REF ARRAY OF LLVM.ValueRef, numParams);
    paramRef := LOOPHOLE(ADR(callArgs[0]), UNTRACED REF LLVM.ValueRef);   
    alignVal := LLVM.LLVMConstInt(LLVM.LLVMInt32Type(), VAL(align,LONGINT), TRUE);
    (* not sure about volatile do we ever use it? *)
    volatile := LLVM.LLVMConstInt(LLVM.LLVMInt1Type(), VAL(0,LONGINT), TRUE);
    dest := LLVM.LLVMBuildBitCast(builderIR, dest, AdrTy, LT("dest_toi8"));
    callArgs[0] := dest; 
    callArgs[1] := Zero(i8Type);   
    callArgs[2] := len;  
    callArgs[3] := alignVal; 
    callArgs[4] := volatile; 
    EVAL LLVM.LLVMBuildCall(builderIR, MemSetFn(), paramRef, numParams, LT(""));
  END DoMemZero;

<*NOWARN*> PROCEDURE zero_n (self: U;  u: IType;  t: MType) =
(* zero s0.u units with 't's size and alignment starting at s1.A; pop(2) *)
  VAR
    s0 := Get(self.exprStack,0);
    s1 := Get(self.exprStack,1);
    dest,len,sizeVal : LLVM.ValueRef;
    align : INTEGER;
  BEGIN
    dest := NARROW(s1,LvExpr).lVal;
    len := NARROW(s0,LvExpr).lVal;
    align := TypeSize(t);    
    sizeVal := LLVM.LLVMConstInt(LLVM.LLVMInt64Type(), VAL(align,LONGINT), TRUE);
    len := LLVM.LLVMBuildNSWMul(builderIR,len,sizeVal,LT("zero_mul"));     
    DoMemZero(dest,len,align);
    Pop(self.exprStack,2);   
  END zero_n;

<*NOWARN*> PROCEDURE zero (self: U;  n: INTEGER;  t: MType) =
(* zero 'n' units with 't's size and alignment starting at s0.A; pop(1) *)
  VAR
    s0 := Get(self.exprStack,0);
    dest,len : LLVM.ValueRef;
    byteLen,align : INTEGER;    
  BEGIN
    dest := NARROW(s0,LvExpr).lVal;
    byteLen := n * TypeSize(t);
    align := TypeSize(t);
    len := LLVM.LLVMConstInt(IntPtrTy, VAL(byteLen,LONGINT), TRUE);    
    DoMemZero(dest,len,align);
    Pop(self.exprStack);   
  END zero;

(*----------------------------------------------------------- conversions ---*)

<*NOWARN*> PROCEDURE loophole (self: U;  from, two: ZType) =
(* s0.two := LOOPHOLE(s0.from, two) *)
VAR
  s0 := Get(self.exprStack,0);
  a,b : LLVM.ValueRef;
  destTy : LLVM.TypeRef;
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
    destTy := LLvmType(two);
    b := LLVM.LLVMBuildBitCast(builderIR, a, destTy, LT("loophole"));
    NARROW(s0,LvExpr).lVal := b;
  END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

<*NOWARN*> PROCEDURE abort (self: U;  code: RuntimeError) =
(* generate a checked runtime error for "code" *)
CONST
  numParams = 2;
VAR
  lVal,codeVal,modVal : LLVM.ValueRef;
  paramRef : UNTRACED REF LLVM.ValueRef := NIL;
  callArgs: UNTRACED REF ARRAY OF LLVM.ValueRef;
  BEGIN
    callArgs := NEW(UNTRACED REF ARRAY OF LLVM.ValueRef, numParams);
    paramRef := LOOPHOLE(ADR(callArgs[0]), UNTRACED REF LLVM.ValueRef);
    codeVal := LLVM.LLVMConstInt(LLVM.LLVMInt64Type(), VAL(ORD(code),LONGINT), TRUE);

    modVal := LLVM.LLVMBuildBitCast(builderIR, faultVal, AdrTy, LT("fault_toadr"));
    callArgs[0] := modVal; 
    callArgs[1] := codeVal;  
    lVal := LLVM.LLVMBuildCall(builderIR, self.abortFunc, paramRef, numParams, LT(""));                               
  END abort;

PROCEDURE DoCheck(self : U; a,b : LLVM.ValueRef; pred : LLVM.IntPredicate; code : RuntimeError) =
VAR
  cmpVal,brVal : LLVM.ValueRef;
  curBB,errorBB,exitBB : LLVM.BasicBlockRef;
  BEGIN
    cmpVal := LLVM.LLVMBuildICmp(builderIR, pred, a, b, LT("checkcmp"));
    curBB := LLVM.LLVMGetInsertBlock(builderIR);
    errorBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT("abort"));
    exitBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT("checkok"));

    LLVM.LLVMPositionBuilderAtEnd(builderIR,curBB);
    brVal := LLVM.LLVMBuildCondBr(builderIR,cmpVal,errorBB,exitBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,errorBB);
  
    self.abort(code);
  
    brVal := LLVM.LLVMBuildBr(builderIR,exitBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,exitBB);
  END DoCheck;

PROCEDURE GetErrVal(i : Target.Int; t : IType) : LLVM.ValueRef =
VAR
  intTy : LLVM.TypeRef;
  int : INTEGER;
  res : BOOLEAN;
  BEGIN
    intTy := LLvmType(t);
    res := TInt.ToInt (i, int);
    <*ASSERT res = TRUE *>
    RETURN LLVM.LLVMConstInt(intTy, VAL(int,LONGINT), TRUE);
  END GetErrVal;

PROCEDURE check_nil (self: U;  code: RuntimeError) =
(* IF (s0.A = NIL) THEN abort(code) *)
VAR
  s0 := Get(self.exprStack,0);
  a,b : LLVM.ValueRef;
  BEGIN
(* fixme may not work since addresses and need casts *)
    a := NARROW(s0,LvExpr).lVal;
    b := LLVM.LLVMConstNull(PtrTy); (* all zeroes *)
    DoCheck(self,a,b,LLVM.IntPredicate.EQ,code);
  END check_nil;

PROCEDURE check_lo (self: U;  t: IType;  READONLY i: Target.Int; code: RuntimeError) =
(* IF (s0.t < i) THEN abort(code) *)
VAR
  s0 := Get(self.exprStack,0);
  a,b : LLVM.ValueRef;
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
    b := GetErrVal(i,t);
    DoCheck(self,a,b,LLVM.IntPredicate.SLT,code);
  END check_lo;

PROCEDURE check_hi (self: U;  t: IType;  READONLY i: Target.Int; code: RuntimeError) =(* IF (i < s0.t) THEN abort(code) *)
VAR
  s0 := Get(self.exprStack,0);
  a,b : LLVM.ValueRef;
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
    b := GetErrVal(i,t);
    DoCheck(self,b,a,LLVM.IntPredicate.SLT,code);
  END check_hi;

PROCEDURE check_range (self: U;  t: IType;  READONLY i, j: Target.Int; code: RuntimeError) =
(* IF (s0.t < i) OR (j < s0.t) THEN abort(code) *)
VAR
  s0 := Get(self.exprStack,0);
  a,b,c : LLVM.ValueRef;
  BEGIN
(* fixme check the order of these *)
    a := NARROW(s0,LvExpr).lVal;
    b := GetErrVal(i,t);
    c := GetErrVal(j,t);
    DoCheck(self,a,b,LLVM.IntPredicate.SLT,code);
    DoCheck(self,c,a,LLVM.IntPredicate.SLT,code);    
  END check_range;

<*NOWARN*> PROCEDURE check_index (self: U;  t: IType;  code: RuntimeError) =
(* IF NOT (0 <= s1.t < s0.t) THEN abort(code) END; pop
   s0.t is guaranteed to be positive so the unsigned
   check (s0.W < s1.W) is sufficient. *)
VAR
  s0 := Get(self.exprStack,0);
  s1 := Get(self.exprStack,1);  
  a,b : LLVM.ValueRef;   
  BEGIN
(* using comment recommend for unsigned single test *)
    a := NARROW(s0,LvExpr).lVal;
    b := NARROW(s1,LvExpr).lVal;
    DoCheck(self,a,b,LLVM.IntPredicate.ULT,code);
    Pop(self.exprStack);
  END check_index;

<*NOWARN*> PROCEDURE check_eq (self: U;  t: IType;  code: RuntimeError) =
(* IF (s0.t # s1.t) THEN abort(code);  Pop (2) *)
VAR
  s0 := Get(self.exprStack,0);
  s1 := Get(self.exprStack,1);  
  a,b : LLVM.ValueRef;
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
    b := NARROW(s1,LvExpr).lVal;
    DoCheck(self,a,b,LLVM.IntPredicate.NE,code);
    Pop(self.exprStack,2);
  END check_eq;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE add_offset (self: U; i: INTEGER) =
(* s0.A := s0.A + i bytes *)
VAR
  s0 := Get(self.exprStack,0);
  a,b,lVal : LLVM.ValueRef;
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
    b := LLVM.LLVMConstInt(LLVM.LLVMInt64Type(), VAL(i,LONGINT), TRUE);
    lVal := LLVM.LLVMBuildNSWAdd(builderIR,a,b,LT("addOfs"));
    NARROW(s0,LvExpr).lVal := lVal;
  END add_offset;


PROCEDURE index_address (self: U;  t: IType;  size: INTEGER) =
(* s1.A := s1.A + s0.t * size; pop  -- where 'size' is in bytes *)
VAR
  s0 := Get(self.exprStack,0);
  s1 := Get(self.exprStack,1);
  a,b,sizeVal,gepVal,mulVal,adrVal : LLVM.ValueRef;  
  srcTy : LLVM.TypeRef;
  BEGIN
(* fixme check if correct any sign ext or conversions needed ??*)
    a := NARROW(s0,LvExpr).lVal;
    b := NARROW(s1,LvExpr).lVal;
    srcTy := LLVM.LLVMPointerType(LLvmType(t));

    sizeVal := LLVM.LLVMConstInt(IntPtrTy, VAL(size,LONGINT), TRUE);
    mulVal := LLVM.LLVMBuildNSWMul(builderIR,a,sizeVal,LT("idxadr_mul"));

    adrVal := LLVM.LLVMBuildBitCast(builderIR, b, AdrTy, LT("idxadr_toadr"));
    gepVal := Gep(adrVal,mulVal,FALSE);
    
    (* cast from i8* to the source type i32* or i64* correct?? *)
    gepVal := LLVM.LLVMBuildBitCast(builderIR, gepVal, srcTy, LT("idxadr_toptr"));
   
    NARROW(s1,LvExpr).lVal := gepVal;
    Pop(self.exprStack);
  END index_address;

  
(*------------------------------------------------------- procedure calls ---*)


<*NOWARN*> PROCEDURE start_call_direct (self: U;  p: Proc;  lev: INTEGER;  t: Type) =
(* begin a procedure call to procedure 'p' at static level 'lev' that
   will return a value of type 't'. *)
  VAR
    proc : LvProc;
  BEGIN
(* fixme think for the case of imported procedures we
complete the declaration and emit the proc for llvm which was started in import_procedure. the local procs were completed in begin_procedure*)

(* not sure that the lev parm is the same as lev declaration. anyway should
save this lev since it implies the static link has to be passed if its > 0
so that in the call_direct we test lev and 
add the static link parm even though the call stack could be empty *)
    proc := NARROW(p,LvProc);
    proc.lev := lev; (* maybe have a new var to distinguish from declared lev *)
    
    self.buildFunc(p);  
    PopDecl(self);
  END start_call_direct;


<*NOWARN*> PROCEDURE call_direct (self: U; p: Proc;  t: Type) =
(* call the procedure 'p'.  It returns a value of type t. *)
VAR
  proc : LvProc;
  fn,lVal : LLVM.ValueRef;
  args: UNTRACED REF ARRAY OF LLVM.ValueRef;
  paramRef : UNTRACED REF LLVM.ValueRef := NIL;
  arg : LvExpr;
  returnName : TEXT := "";
  numParams,stackParams,procParams : INTEGER;
  BEGIN
    DumpExprStack(self,"call_direct");

   <*ASSERT self.callStack # NIL *>
    proc := NARROW(p,LvProc);
    fn := proc.lvProc;
    <*ASSERT fn # NIL *>    
    procParams := proc.numParams;  
    stackParams := self.callStack.size();
    numParams := stackParams;
  
(* debug 
IO.Put("Calling Proc id, numParams "); IO.PutInt(proc.tag); IO.Put(" " ); IO.PutInt(numParams); IO.Put("\n");
  LLVM.LLVMDumpValue(fn);
*)

(* this isnt always the case when there are nested procs and or try finally
  <*ASSERT stackParams = procParams *>
*)

(* check the lev var and add the static link ?? somehow 
    IF proc.lev > 0 THEN
       (* fixme nested procedure *)
    END;
*)
  
    (* create the param types from the callstack *)
    IF numParams > 0 THEN  
      args := NEW(UNTRACED REF ARRAY OF LLVM.ValueRef, numParams);
      paramRef := LOOPHOLE(ADR(args[0]), UNTRACED REF LLVM.ValueRef);
    END;

    FOR i := 0 TO numParams - 1 DO
      arg := Get(self.callStack); 
      (* possibly add call attributes here *)
      args[i] := arg.lVal;
      Pop(self.callStack);
    END;
    <*ASSERT self.callStack.size() = 0 *>

    IF proc.returnType # Type.Void THEN
      returnName := "result";
    END;
    (* else void returns need null string *)
    
    lVal := LLVM.LLVMBuildCall(builderIR, fn, paramRef, numParams, 
                                 LT(returnName));

    IF proc.returnType # Type.Void THEN
      (* push the return val onto stack*)
      Push(self.exprStack,NEW(LvExpr,lVal := lVal));
    END;  
  END call_direct;

<*NOWARN*> PROCEDURE start_call_indirect (self: U;  t: Type;  cc: CallingConvention) =
(* begin an indirect procedure call that will return a value of type 't'. *)
  BEGIN
(* nothing to do *)
  END start_call_indirect;

(* construct a procedure signature type *)
PROCEDURE FuncType(retType : Type; paramStack : RefSeq.T) : LLVM.TypeRef =
VAR
  retTy,procTy : LLVM.TypeRef;
  numParams : INTEGER;
  param : LvExpr;
  paramsTy : UNTRACED REF ARRAY OF LLVM.TypeRef;
  args : UNTRACED REF LLVM.TypeRef;
  BEGIN
    retTy := LLvmType(retType);
    numParams := paramStack.size();

    IF numParams > 0 THEN
      paramsTy := NEW(UNTRACED REF ARRAY OF LLVM.TypeRef, numParams);
      FOR i := 0 TO numParams - 1 DO
        param := Get(paramStack,i);
        paramsTy[i] := LLVM.LLVMTypeOf(param.lVal); 
      END;
      args := LOOPHOLE(ADR(paramsTy[0]), UNTRACED REF LLVM.TypeRef);
    END;

    procTy := LLVM.LLVMFunctionType(retTy, args, numParams, FALSE);
    RETURN procTy;
  END FuncType;

<*NOWARN*> PROCEDURE call_indirect (self: U;  t: Type;  cc: CallingConvention) =
(* call the procedure whose address is in s0.A and pop s0.  The
   procedure returns a value of type t. *)
VAR
  s0 := Get(self.exprStack);
  expr,arg : LvExpr;
  proc,callVal,resultVal : LLVM.ValueRef;
  args: UNTRACED REF ARRAY OF LLVM.ValueRef;
  paramRef : UNTRACED REF LLVM.ValueRef := NIL; 
  funcTy,funcPtrTy : LLVM.TypeRef;  
  numParams : INTEGER;
  returnName : TEXT := "";  
  BEGIN
    DumpExprStack(self,"call_indirect");
  
    expr := NARROW(s0,LvExpr);
    proc := expr.lVal;
    (* get the function sig from the callstack *)
    funcTy := FuncType(t,self.callStack);
 
    numParams := self.callStack.size();
    IF numParams > 0 THEN  
      args := NEW(UNTRACED REF ARRAY OF LLVM.ValueRef, numParams);
      paramRef := LOOPHOLE(ADR(args[0]), UNTRACED REF LLVM.ValueRef);
    END;
    
    FOR i := 0 TO numParams - 1 DO
      arg := Get(self.callStack);
      args[i] := arg.lVal;
      Pop(self.callStack);
    END;
    <*ASSERT self.callStack.size() = 0 *>
     
    IF t # Type.Void THEN
      returnName := "result";
    END;

    (* need a pointer to function type for the call *)
    funcPtrTy := LLVM.LLVMPointerType(funcTy);    
    callVal := LLVM.LLVMBuildBitCast(builderIR, proc, funcPtrTy, LT("callind"));
    resultVal := LLVM.LLVMBuildCall(builderIR, callVal, paramRef, numParams, 
                               LT(returnName));

    (* discard the proc address *)
    Pop(self.exprStack);

    IF t # Type.Void THEN
      (* push the return val onto stack*)
      Push(self.exprStack,NEW(LvExpr,lVal := resultVal));
    END;
  END call_indirect;

<*NOWARN*> PROCEDURE pop_param (self: U;  t: MType) =
(* pop s0.t and make it the "next" parameter in the current call. *)
VAR
  s0 := Get(self.exprStack,0);
  expr : LvExpr;
  destTy : LLVM.TypeRef;
  BEGIN 
    expr := NARROW(s0,LvExpr);

    (* test if arg is ptr type then convert 
    test is exceptiontest and rthooks__raise   *)
    IF t = Type.Addr THEN
      expr.lVal := LLVM.LLVMBuildBitCast(builderIR, expr.lVal, AdrTy, LT("pop_toadr"));
    ELSE
      IF ptrBytes > TypeSize(t) THEN
        destTy := LLvmType(t);
        expr.lVal := LLVM.LLVMBuildTrunc(builderIR,expr.lVal,destTy,
                                LT("pop_trunc"));
      END;
    END;

    expr.type := t;
    PushRev(self.callStack,s0);
    Pop(self.exprStack);
  END pop_param;

<*NOWARN*> PROCEDURE pop_struct (self: U;  t: TypeUID;  s: ByteSize;  a: Alignment) =
(* pop s0.A, it's a pointer to a structure occupying 's' bytes that's
  'a' byte aligned; pass the structure by value as the "next" parameter
  in the current call. *)
VAR
  s0 := Get(self.exprStack,0);
  expr : LvExpr;
  BEGIN
    expr := NARROW(s0,LvExpr);
    expr.type := Type.Struct;
    PushRev(self.callStack,s0);
    Pop(self.exprStack);
  END pop_struct;

<*NOWARN*> PROCEDURE pop_static_link (self: U) =
(* pop s0.A for the current indirect procedure call's static link  *)
VAR
  s0 := Get(self.exprStack,0);
  BEGIN
(* fix me dont think we have ever tested this *)
    PushRev(self.callStack,s0);    
    Pop(self.exprStack);
  END pop_static_link;

(*------------------------------------------- procedure and closure types ---*)

<*NOWARN*> PROCEDURE load_procedure (self: U;  p: Proc) =
(* push; s0.A := ADDR (p's body) *)
VAR
  proc : LvProc;
  srcVal : LLVM.ValueRef;
BEGIN
    proc := NARROW(p,LvProc);
    self.buildFunc(p);
    srcVal := proc.lvProc;
    Push(self.exprStack,NEW(LvExpr,lVal := srcVal));
  END load_procedure;

<*NOWARN*> PROCEDURE load_static_link (self: U;  p: Proc) =
(* push; s0.A := (static link needed to call p, NIL for top-level procs) *)
VAR
  proc : LvProc;
  link : LLVM.ValueRef;
  BEGIN
(* fix me *)
    proc := NARROW(p,LvProc);
    (* do we load the p or p's parent should be the parent *)
    link := proc.parent.lvProc;  
    Push(self.exprStack,NEW(LvExpr,lVal := link));  
  END load_static_link;

(*----------------------------------------------------------------- misc. ---*)

<*NOWARN*> PROCEDURE comment (self: U;  a, b, c, d: TEXT := NIL) =
(* annotate the output with a&b&c&d as a comment.  Note that any of a,b,c or d
   may be NIL. *)
  VAR s : TEXT := "";
  BEGIN
(* fix me *)
    IF a # NIL THEN s := s & a; END;
    IF b # NIL THEN s := s & b; END;
    IF c # NIL THEN s := s & c; END;
    IF d # NIL THEN s := s & d; END;
    IO.Put("Comment -- " & s &  "\n");
  END comment;

(*--------------------------------------------------------------- atomics ---*)
(* These all operate atomically and affect memory as per "o". *)

PROCEDURE GetOrder(order : MemoryOrder) : LLVM.AtomicOrdering =
VAR
  ordering : LLVM.AtomicOrdering;
BEGIN
  CASE order OF
  | MemoryOrder.Relaxed => ordering := LLVM.AtomicOrdering.LLVMAtomicOrderingMonotonic;
  | MemoryOrder.Release => ordering := LLVM.AtomicOrdering.LLVMAtomicOrderingRelease;
  | MemoryOrder.Acquire => ordering := LLVM.AtomicOrdering.LLVMAtomicOrderingAcquire;
  | MemoryOrder.AcquireRelease => ordering := LLVM.AtomicOrdering.LLVMAtomicOrderingAcquireRelease;
  | MemoryOrder.Sequential => ordering := LLVM.AtomicOrdering.LLVMAtomicOrderingSequentiallyConsistent;
  END;
  RETURN ordering;
END GetOrder;

<*NOWARN*> PROCEDURE store_ordered (self: U;  t: ZType;  u: MType;  order: MemoryOrder) =
(* Mem [s1.A].u := s0.t; pop (2) *)
  BEGIN
(* fix me - corresponds to store atomic ordered, needs extra instr in c api see instruction.h setatomic or another store instr for the overloaded store *)
  END store_ordered;

<*NOWARN*> PROCEDURE load_ordered (self: U;  t: MType;  u: ZType;  order: MemoryOrder) =
(* s0.u := Mem [s0.A].t  *)
  BEGIN
(* fix me see store_ordered *)
  END load_ordered;
    
<*NOWARN*> PROCEDURE exchange (self: U;  t: MType;  u: ZType;  order: MemoryOrder) =
(* tmp := Mem [s1.A].t;  Mem [s1.A].t := s0.u;  s0.u := tmp;  pop *)
 VAR
   s0 := Get(self.exprStack,0);
   s1 := Get(self.exprStack,1);  
   atomicOp : LLVM.AtomicRMWBinOp;
   ordering : LLVM.AtomicOrdering;
   a,b,lVal : LLVM.ValueRef;
  BEGIN
(* fix me *)
    a := NARROW(s0,LvExpr).lVal;
    b := NARROW(s1,LvExpr).lVal;
  
    ordering := GetOrder(order);
    atomicOp := LLVM.AtomicRMWBinOp.LLVMAtomicRMWBinOpXchg;
  
    lVal := LLVM.LLVMBuildAtomicRMW(builderIR, atomicOp,
                                    a,b,
                                    ordering,
                                    TRUE);
    NARROW(s1,LvExpr).lVal := lVal;          
    Pop(self.exprStack);
  END exchange;

<*NOWARN*> PROCEDURE compare_exchange (self: U;  t: MType;  u: ZType;  r: IType; success, failure: MemoryOrder) =
(* tmp := Mem[s2.A].t;
   IF (tmp = s1.u)
   THEN Mem[s2.A].t := s0.u; s2.r := 1; pop(1);
   ELSE s1.u := tmp;         s2.r := 0; pop(1);
   END;
   This is permitted to fail spuriously, leaving s1 unchanged.
*)

(*
  VAR
   s0 := Get(self.exprStack,0);
   s1 := Get(self.exprStack,1); 
   s2 := Get(self.exprStack,2);
   ptr,cmp,new,lVal : LLVM.ValueRef;
   successOrdering,failureOrdering : LLVM.AtomicOrdering;
*)
  BEGIN
(* fix me use cmpxch instr but need c api addition *)
(* possible implementation not sure values are correct *)
(*
    ptr := NARROW(s0,LvExpr).lVal;
    cmp := NARROW(s1,LvExpr).lVal;
    new := NARROW(s2,LvExpr).lVal;
     
    successOrdering := GetOrder(success);  
    failureOrdering := GetOrder(failure);  

    lVal := LLVM.LLVMBuildAtomicCmpXchg(builderIR,
                                ptr,cmp,new,
                                successOrdering,
                                failureOrdering,                                
                                TRUE);   
    NARROW(s1,LvExpr).lVal := lVal;          
    Pop(self.exprStack,2);                                
*)
  END compare_exchange;

<*NOWARN*> PROCEDURE fence (self: U;  order: MemoryOrder) =
(* Memory is affected as per o *)
VAR
 ordering : LLVM.AtomicOrdering;
  BEGIN
    (* fixme is singlethreaded significant *)
    ordering := GetOrder(order);  
    EVAL LLVM.LLVMBuildFence(builderIR, ordering, TRUE,  LT("fence")); 
  END fence;

<*NOWARN*> PROCEDURE fetch_and_op (self: U;  op: AtomicOp;  t: MType;  u: ZType; order: MemoryOrder) =
(* tmp := Mem [s1.A].t;
   Mem [s1.A].t := tmp op s0.u;
   s1.u := tmp; pop *)
 VAR
   s0 := Get(self.exprStack,0);
   s1 := Get(self.exprStack,1);  
   atomicOp : LLVM.AtomicRMWBinOp;
   ordering : LLVM.AtomicOrdering;
   a,b,lVal : LLVM.ValueRef;
  BEGIN
(* fix me *)
    a := NARROW(s0,LvExpr).lVal;
    b := NARROW(s1,LvExpr).lVal;

    CASE op OF
    | AtomicOp.Add => atomicOp := LLVM.AtomicRMWBinOp.LLVMAtomicRMWBinOpAdd;
    | AtomicOp.Sub => atomicOp := LLVM.AtomicRMWBinOp.LLVMAtomicRMWBinOpSub;
    | AtomicOp.Or  => atomicOp := LLVM.AtomicRMWBinOp.LLVMAtomicRMWBinOpOr;
    | AtomicOp.And => atomicOp := LLVM.AtomicRMWBinOp.LLVMAtomicRMWBinOpAnd;
    | AtomicOp.Xor => atomicOp := LLVM.AtomicRMWBinOp.LLVMAtomicRMWBinOpXor;
    END;

    ordering := GetOrder(order);
  
    lVal := LLVM.LLVMBuildAtomicRMW(builderIR, atomicOp,
                                    a,b,
                                    ordering,
                                    TRUE);
    NARROW(s1,LvExpr).lVal := lVal;          
    Pop(self.exprStack);
  END fetch_and_op;

BEGIN
  globContext := LLVM.LLVMGetGlobalContext();
  builderIR := LLVM.LLVMCreateBuilderInContext(globContext);
END M3CG_LLVM.
