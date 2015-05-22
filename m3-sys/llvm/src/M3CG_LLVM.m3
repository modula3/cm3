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
IMPORT Ctypes, M3toC;
IMPORT Text,Fmt,Pathname;
IMPORT IO; (* debug this module *)

REVEAL

  U = Public BRANDED "M3CG_LLVM.T" OBJECT
    wr            : Wr.T := NIL;
    buf           : M3Buf.T := NIL;
    buf_len       : INTEGER := 0;

    exprStack     : RefSeq.T := NIL;
    callStack     : RefSeq.T := NIL;
    curVar        : LvVar;
    curProc       : LvProc;
    abortFunc     : LLVM.ValueRef;

    procStack     : RefSeq.T := NIL;
    declStack     : RefSeq.T := NIL;
    allocStack    : RefSeq.T := NIL; (* optimise to remove unused allocs -not working *)
    labelTable    : IntRefTbl.T := NIL;
    structTable   : IntRefTbl.T := NIL;

    next_label_id := 1;
    next_var      := 1;
    next_proc     := 1;
    next_scope    := 1;
    blockLevel    := 0;
    widecharSize  := 16;

    (* debug stuff *)
    genDebug      := FALSE ;
    curFile       := "";
    curLine       := 0;
    debFile       := "";
    debDir        := "";
    debugTable    : IntRefTbl.T := NIL;
    debugRef      : LLVM.DIBuilderRef;
    cuRef         : LLVM.ValueRef;
    fileRef       : LLVM.ValueRef;
    funcRef       : LLVM.ValueRef;
    debugObj      : ROOT;
METHODS
    allocVar(v : LvVar) := AllocVar;
    buildFunc(p : Proc) := BuildFunc;
    getLabel(l : Label; name : TEXT) : LabelObj := GetLabel;
    ifCommon(t: IType; l: Label; f: Frequency; op : CompareOp) := IfCommon;
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
    declare_temp := declare_temp;
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
    segLen : INTEGER;
    in_memory : BOOLEAN;
    up_level : BOOLEAN;
    exported : BOOLEAN;
    frequency : Frequency;
    inProc : LvProc;  (* for static link *)
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
    linkStack   : RefSeq.T := NIL;
    imported : BOOLEAN := FALSE; (* if this is an import *)
    defined : BOOLEAN := FALSE; (* set when we build the declaration for real *)
    trampTy : LLVM.TypeRef := NIL; (* array of addresses for static link *)
    staticLv : LLVM.ValueRef := NIL; (* i8* type for the static link parm *)
    linkAdrLv : LLVM.ValueRef := NIL; (* i8* type for the static link adr *)
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
    size : INTEGER;
    offset : INTEGER;
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
    id : Label;
    labBB : LLVM.BasicBlockRef;  (* jmps goto this bb *)
    branchLVal : LLVM.ValueRef; (* when a branch is created store its value here but see above comment for same item *)

    cmpInstr : LLVM.ValueRef; (* saved cmp instr used to move the bb *)
    branchList : RefSeq.T; (* list of branches to this label *)

    elseBB : LLVM.BasicBlockRef; (* else bb for compares *)
    condLv : LLVM.ValueRef;      (* condtion branch lv could use eval and del *)
  END;

  (* template object for common If-Then-Else construction *)

  ITEObj = OBJECT
    curProc : LvProc;
    cmpVal : LLVM.ValueRef;
    opType : LLVM.TypeRef;
    tmpLv : LLVM.ValueRef;
    curBB,thenBB,elseBB,exitBB : LLVM.BasicBlockRef;
    opName : TEXT;
  METHODS
    init() : ITEObj := ITEInit;
    block(storeVal : LLVM.ValueRef; endBB : BOOLEAN) : LLVM.ValueRef := ITEBlock;
  END;

  (* debug uids for basic types *)
CONST
  NO_UID = 16_FFFFFFFF;
  UID_INTEGER = 16_195C2A74;
  UID_LONGINT = 16_05562176;
  UID_WORD = 16_FFFFFFFF97E237E2;
  UID_LONGWORD = 16_FFFFFFFF9CED36E7;
  UID_REEL = 16_48E16572;
  UID_LREEL = 16_FFFFFFFF94FE32F6;
  UID_XREEL = 16_FFFFFFFF9EE024E3;
  UID_BOOLEAN = 16_1E59237D;
  UID_CHAR = 16_56E16863;
  UID_WIDECHAR = 16_88F439FC;
  UID_MUTEX = 16_1541F475;
  UID_TEXT = 16_50F86574;
  UID_UNTRACED_ROOT = 16_898EA789;
  UID_ROOT = 16_FFFFFFFF9D8FB489;
  UID_REFANY = 16_1C1C45E6;
  UID_ADDR = 16_08402063;
  UID_RANGE_0_31 = 16_2DA6581D;
  UID_RANGE_0_63 = 16_2FA3581D;
(*
  UID_PROC1 = 16_9C9DE465;
  UID_PROC2 = 16_20AD399F;
  UID_PROC3 = 16_3CE4D13B;
  UID_PROC4 = 16_FA03E372;
  UID_PROC5 = 16_509E4C68;
  UID_PROC6 = 16_DC1B3625;
  UID_PROC7 = 16_EE17DF2C;
  UID_PROC8 = 16_B740EFD0;
*)
  UID_NULL = 16_48EC756E;

  (* debug encoding - see dwarf.h *)
  DW_ATE_boolean = 16_2;
  DW_ATE_signed = 16_5;
  DW_ATE_unsigned = 16_7;
  DW_ATE_signed_char = 16_6;
  DW_ATE_unsigned_char = 16_8;
  DW_ATE_float = 16_4;
  DW_ATE_address = 16_1;

  (* debug var type *)
  DW_TAG_auto_variable = 16_100;
  DW_TAG_arg_variable = 16_101;

  (* debug objects *)
TYPE

  BaseDebug = OBJECT
    bitSize,align : LONGINT;
    typeName : Name;
    encoding : CARDINAL;
  END;

  ArrayDebug = BaseDebug OBJECT
    index,elt : TypeUID;
  END;

  SetDebug = BaseDebug OBJECT
    domain : TypeUID;
  END;
(* can this be base on arraydebug ?? *)
  OpenArrayDebug = BaseDebug OBJECT
    elt : TypeUID;
  END;

  EnumDebug = BaseDebug OBJECT
    numElts,index : CARDINAL;
    elts : REF ARRAY OF Name;
  END;

  SubrangeDebug = BaseDebug OBJECT
    domain : TypeUID;
    min,max : LONGINT;
  END;

  FieldDebug = BaseDebug OBJECT
    name : Name;
    bitOffset : LONGINT;
    type : TypeUID;
  END;

  PackedDebug = BaseDebug OBJECT
    base : TypeUID;
  END;

  MethodDebug = BaseDebug OBJECT
    name : Name;
    signature : TypeUID;
  END;

  RecordDebug = BaseDebug OBJECT
    numFields,fieldIndex : CARDINAL;
    global := FALSE;
    fields : REF ARRAY OF FieldDebug;
  END;

  ObjectDebug = RecordDebug OBJECT
    superType : TypeUID;
    fieldSize : LONGINT;
    numMethods,methodIndex : CARDINAL;
    brand : TEXT;
    traced : BOOLEAN;
    methods : REF ARRAY OF MethodDebug;
  END;

  OpaqueDebug = BaseDebug OBJECT
    superType : TypeUID;
  END;

  PointerDebug = BaseDebug OBJECT
    target : TypeUID;
    brand : TEXT;
    traced : BOOLEAN;
  END;

  IndirectDebug = BaseDebug OBJECT
    target : TypeUID;
  END;

  ProcTypeDebug = BaseDebug OBJECT
    result : TypeUID;
    numFormals,numRaises : INTEGER;
    cc : CallingConvention;
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

(* simplify all the untraced array allocs for llvm *)
TYPE
  ValueArrType = UNTRACED REF ARRAY OF LLVM.ValueRef;
  ValueRefType = UNTRACED REF LLVM.ValueRef;
  TypeArrType = UNTRACED REF ARRAY OF LLVM.TypeRef;
  TypeRefType = UNTRACED REF LLVM.TypeRef;

PROCEDURE NewValueArr(VAR paramsArr : ValueArrType; numParams : CARDINAL) : ValueRefType =
  BEGIN
    IF numParams = 0 THEN RETURN NIL; END;
    paramsArr := NEW(ValueArrType, numParams);
    RETURN LOOPHOLE(ADR(paramsArr[0]), ValueRefType);
  END NewValueArr;

PROCEDURE NewTypeArr(VAR paramsArr : TypeArrType; numParams : CARDINAL) : TypeRefType =
  BEGIN
    IF numParams = 0 THEN RETURN NIL; END;
    paramsArr := NEW(TypeArrType, numParams);
    RETURN LOOPHOLE(ADR(paramsArr[0]), TypeRefType);
  END NewTypeArr;

PROCEDURE New (output: Wr.T): M3CG.T =
  VAR mbuf := M3Buf.New ();
  BEGIN
    M3Buf.AttachDrain (mbuf, output);
    RETURN NEW (U, wr := output, buf := mbuf, buf_len := 0,
                structTable := NEW (IntRefTbl.Default).init (20),
                debugTable := NEW (IntRefTbl.Default).init (20),
                labelTable := NEW (IntRefTbl.Default).init (20),
                exprStack := NEW(RefSeq.T).init(),
                callStack := NEW(RefSeq.T).init(),
                procStack := NEW(RefSeq.T).init(),
                allocStack := NEW(RefSeq.T).init(),
                declStack := NEW(RefSeq.T).init());
  END New;

PROCEDURE NewVar (self: U; name : Name; size : ByteSize; align : Alignment; type : Type; isConst : BOOLEAN; m3t : TypeUID; in_memory : BOOLEAN; up_level : BOOLEAN; exported : BOOLEAN; frequency : Frequency; varType : VarType): Var =
  VAR
    v := NEW (LvVar, tag := self.next_var, name := name, size := size, type := type, isConst := isConst, align := align, m3t := m3t, in_memory := in_memory, up_level := up_level, exported := exported, frequency := frequency, varType := varType);
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
    FOR i := 1 TO n DO EVAL stack.remlo(); END;
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
    IF t = Type.Word8 OR t = Type.Word16 OR
       t = Type.Word32 OR t = Type.Word64 THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END WordTypes;

PROCEDURE TypeSize(t : Type) : CARDINAL =
  BEGIN
    (* returns size in bytes maybe use llvm func storesizeoftype after llvmtype*)
    CASE t OF
    | Type.Int8,Type.Word8   => RETURN 1;
    | Type.Int16,Type.Word16  => RETURN 2;
    | Type.Int32,Type.Word32  => RETURN 4;
    | Type.Int64,Type.Word64  => RETURN 8;
    | Type.Reel   => RETURN 4;
    | Type.LReel  => RETURN 8;
    | Type.XReel  => RETURN 10;
    | Type.Addr   => RETURN ptrBytes; (* target dependant *)
    | Type.Struct => RETURN 0; (* not used - could fix using size *)
    | Type.Void  => RETURN 0;
    END;
  END TypeSize;

PROCEDURE LLvmType(t : Type) : LLVM.TypeRef =
  BEGIN
    CASE t OF
    | Type.Int8,Type.Word8   => RETURN LLVM.LLVMInt8Type();
    | Type.Int16,Type.Word16  => RETURN LLVM.LLVMInt16Type();
    | Type.Int32,Type.Word32  => RETURN LLVM.LLVMInt32Type();
    | Type.Int64,Type.Word64  => RETURN LLVM.LLVMInt64Type();
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
  CONST numElems = 1;
  VAR
    varTy,arrTy,structTy : LLVM.TypeRef;
    elemArr : TypeArrType;
    elemRef : TypeRefType;
    typeExists : BOOLEAN;
    structRef : REFANY;
  BEGIN
    IF v.type # Type.Struct THEN
      varTy := LLvmType(v.type);
    ELSE
      typeExists := self.structTable.get(v.size,structRef);
      IF typeExists THEN
        structTy := NARROW(structRef,LvStruct).struct;
      ELSE
        elemRef := NewTypeArr(elemArr,numElems);
        arrTy := LLVM.LLVMArrayType(i8Type,v.size);
        elemArr[0] := arrTy;
        structTy := LLVM.LLVMStructCreateNamed(globContext, LT("struct"));
        LLVM.LLVMStructSetBody(structTy, elemRef, numElems, FALSE);
        (* save the type *)
        structRef := NEW(LvStruct,struct := structTy);
        EVAL self.structTable.put(v.size,structRef);
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
    RETURN LLVM.LLVMConstInt(t, VAL(1,LONGINT), TRUE);
  END One;

(* avoid the m3toc stuff everywhere *)
PROCEDURE LT(t : TEXT) : Ctypes.char_star =
  BEGIN
    RETURN M3toC.CopyTtoS(t);
  END LT;

PROCEDURE ITEInit(self : ITEObj) : ITEObj =
  BEGIN
    self.tmpLv := LLVM.LLVMBuildAlloca(builderIR, self.opType, LT("itetmp"));
    self.curBB := LLVM.LLVMGetInsertBlock(builderIR);
    self.thenBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT(self.opName & "_then"));
    self.elseBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT(self.opName & "_else"));
    self.exitBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT(self.opName & "_end"));
    LLVM.LLVMPositionBuilderAtEnd(builderIR,self.curBB);
    EVAL LLVM.LLVMBuildCondBr(builderIR,self.cmpVal,self.thenBB,self.elseBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,self.thenBB);
    RETURN self;
 END ITEInit;

PROCEDURE ITEBlock(self : ITEObj; storeVal : LLVM.ValueRef; endBB : BOOLEAN) : LLVM.ValueRef =
  VAR res : LLVM.ValueRef := NIL;
  BEGIN
    EVAL LLVM.LLVMBuildStore(builderIR, storeVal, self.tmpLv);
    EVAL LLVM.LLVMBuildBr(builderIR,self.exitBB);
    IF endBB THEN
      LLVM.LLVMPositionBuilderAtEnd(builderIR,self.exitBB);
      res := LLVM.LLVMBuildLoad(builderIR, self.tmpLv, LT(self.opName & "_load"));
    ELSE (* elseBB *)
      LLVM.LLVMPositionBuilderAtEnd(builderIR,self.elseBB);
    END;
    RETURN res;
  END ITEBlock;

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
    paramsArr : TypeArrType;
    paramsRef : TypeRefType;
  BEGIN
    IF fn # NIL THEN RETURN fn; END;
    IF hasReturn THEN
      retTy := IntPtrTy;
    ELSE
      retTy := LLvmType(Type.Void);
    END;

    paramsRef := NewTypeArr(paramsArr,numParams+1);

    paramsArr[0] := IntPtrTy;
    FOR i := 1 TO numParams - 1 DO
      paramsArr[i] := PtrTy;
    END;
    IF setRange THEN
      paramsArr[1] := IntPtrTy;
    END;

    procTy := LLVM.LLVMFunctionType(retTy, paramsRef, numParams, FALSE);
    proc := LLVM.LLVMAddFunction(modRef, LT(name), procTy);
    RETURN proc;
  END DeclSet;

PROCEDURE SizeStaticLink(self : U; proc : LvProc) : INTEGER =
  VAR
    tp : LvProc;
    linkSize : CARDINAL := 0;
  BEGIN
    tp := proc.parent;
    WHILE tp # NIL DO
      IF tp.linkStack # NIL THEN (*importeds are nil *)
        INC(linkSize,tp.linkStack.size());
      END;
      tp := tp.parent;
    END;
    RETURN linkSize;
  END SizeStaticLink;

PROCEDURE LinkExists(proc : LvProc) : BOOLEAN =
VAR arg : REFANY; param : LvVar;
BEGIN
  (* check if this is compiler generated func with a static link.
     Making a pretty big assumption here *)
  IF proc.paramStack.size() = 1 THEN
    arg := Get(proc.paramStack);
    param := NARROW(arg,LvVar);
    IF param.name = M3ID.NoID THEN
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END LinkExists;

(* Declare this procedure and all its locals and parameters. *)
PROCEDURE BuildFunc(self : U; p : Proc) =
  VAR
    param,v : LvVar;
    proc : LvProc;
    retTy : LLVM.TypeRef;
    paramsArr : TypeArrType;
    paramsRef : TypeRefType;
    lVal : LLVM.ValueRef;
    numParams,linkSize : CARDINAL := 0;
    name : TEXT;
    arg : REFANY;
  BEGIN
    proc := NARROW(p,LvProc);
    IF proc.defined THEN RETURN; END;

    IF proc.lev > 0 THEN
      (* get the size of the trampoline area for the static link *)
      linkSize := SizeStaticLink(self,proc);
      IF linkSize > 0 THEN
        proc.trampTy := LLVM.LLVMArrayType(AdrTy,linkSize);
        (* check if this is a try-finally parm or other internal static link *)
        IF NOT LinkExists(proc) THEN
          (* create the static link param var and push it onto the param stack *)
          v := NewVar(self,M3ID.Add("_link"),ptrBytes,ptrBytes,Type.Addr,FALSE,UID_ADDR,TRUE,FALSE,FALSE,50,VarType.Param);
          PushRev(proc.paramStack, v);
          INC(proc.numParams);
        END;
      END;
    END;

    numParams := proc.numParams;

    IF proc.imported THEN
      (* delete the temp function and define the real one *)
      LLVM.LLVMDeleteFunction(proc.lvProc);
    END;
    proc.defined := TRUE;
    <*ASSERT proc.paramStack.size() = numParams *>

    (* create the param types from the param stack *)
    paramsRef := NewTypeArr(paramsArr,numParams);
    IF numParams > 0 THEN
      FOR i := 0 TO numParams - 1 DO
        arg := Get(proc.paramStack,i);
        param := NARROW(arg,LvVar);
        IF param.type = Type.Struct THEN
          param.lvType := LLVM.LLVMPointerType(param.lvType);
        END;
        paramsArr[i] := param.lvType;
      END;
    END;

    (* create the return type *)
    retTy := LLvmType(proc.returnType);

    (* create the function sig *)
    proc.procTy := LLVM.LLVMFunctionType(retTy, paramsRef, numParams, FALSE);

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
        name := "_link";
        (* the only place the parm is noid seems to be the generated internal
        procs for try-finally and maybe for runtime linker and the purpose of this
        parm is the static link. Set the name so we can check in begin_procedure *)
        param.name := M3ID.Add(name);
      ELSE
        name := M3ID.ToText(param.name);
      END;
      (* set a name for the param - doesnt work for externals *)
      LLVM.LLVMSetValueName(lVal, LT(name));

      (* this sets the byval attribute by which the caller makes a copy *)
      IF param.type = Type.Struct THEN
        LLVM.LLVMAddAttribute(lVal, LLVM.ByValAttribute);
      END;
      (* set the alignment not sure we need it except for struct *)
      LLVM.LLVMSetParamAlignment(lVal, param.align);
      lVal := LLVM.LLVMGetNextParam(lVal);
    END;

    DebugFunc(self,p);
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
    (* not used *)
  END set_error_handler;

(*----------------------------------------------------- compilation units ---*)

<*NOWARN*> PROCEDURE begin_unit (self: U;  optimize : INTEGER) =
  BEGIN
    globContext := LLVM.LLVMGetGlobalContext();
    builderIR := LLVM.LLVMCreateBuilderInContext(globContext);

    targetData := LLVM.LLVMCreateTargetData(LT(dataRep));
    ptrBytes := LLVM.LLVMPointerSize(targetData);
    IntPtrTy := LLVM.LLVMIntPtrType(targetData);
    PtrTy := LLVM.LLVMPointerType(IntPtrTy);
    AdrTy := LLVM.LLVMPointerType(LLVM.LLVMInt8Type());
    AdrAdrTy := LLVM.LLVMPointerType(AdrTy);
    ptrBits := LLVM.LLVMSizeOfTypeInBits(targetData, PtrTy);
    wordSize := LLVM.LLVMConstInt(IntPtrTy, VAL(ptrBits,LONGINT), TRUE);
    byteSize := LLVM.LLVMConstInt(IntPtrTy, VAL(ptrBytes,LONGINT), TRUE);
  END begin_unit;

<*NOWARN*> PROCEDURE end_unit (self: U) =
  VAR
    iter : IntRefTbl.Iterator;
    key : INTEGER;
    lab : REFANY;
    label : LabelObj;
    terminator : LLVM.ValueRef;
    curBB : LLVM.BasicBlockRef;
  BEGIN

(*
    CheckIntrinsics();
*)

(* test running a pass - not working c api missing pass addition procs
  VAR
    passRef : LLVM.PassManagerRef;
    modified : BOOLEAN;
  BEGIN
    passRef := LLVM.LLVMCreatePassManager();
    modified := LLVM.LLVMRunPassManager(passRef,modRef);
    IF modified THEN
      IO.Put("pass modified\n");
    END;
  END;
*)

    (* could be a label after an exit_proc which created a bb or a loop with no exit, either way the bb must have a terminator so 
    add an unreachable stmt *)
    iter := self.labelTable.iterate();
    WHILE iter.next(key, lab) DO
      label := NARROW(lab,LabelObj);
      terminator := LLVM.LLVMGetBasicBlockTerminator(label.labBB);
      IF terminator = NIL THEN
        <*ASSERT LLVM.LLVMGetFirstInstruction(label.labBB) = NIL *>
        <*ASSERT label.branchList.size() = 0 *>
        curBB := LLVM.LLVMGetInsertBlock(builderIR);
        LLVM.LLVMPositionBuilderAtEnd(builderIR,label.labBB);
        EVAL LLVM.LLVMBuildUnreachable(builderIR);
        LLVM.LLVMPositionBuilderAtEnd(builderIR,curBB);
      END;
    END;
    DebugFinalise(self);
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

PROCEDURE set_source_file (self: U;  file: TEXT) =
  BEGIN
    self.curFile := file;
    moduleID := LT(file);
    modRef := LLVM.LLVMModuleCreateWithNameInContext(moduleID,globContext);
    LLVM.LLVMSetDataLayout(modRef,LT(dataRep));
    LLVM.LLVMSetTarget(modRef,LT(targetTriple));

    DebugInit(self);
  END set_source_file;

PROCEDURE set_source_line (self: U; line: INTEGER) =
  BEGIN
    self.curLine := line;
(* debug *)
    IO.Put("LINE ------------------------ " & Fmt.Int(line) & "------------\n");
(*debug
    IF self.curProc # NIL THEN
      IO.Put("the cur proc " & M3ID.ToText(self.curProc.name) & "\n");
    END;
*)
    (* set the debugloc for this line *)
    DebugLine(self);
  END set_source_line;

(*------------------------------------------- debugging type declarations ---*)

PROCEDURE declare_typename (self: U; t: TypeUID; n: Name) =
  VAR
    baseObj : BaseDebug;
    typeRef : REFANY;
    tidExists : BOOLEAN;
  BEGIN
    tidExists := self.debugTable.get(t,typeRef);
    IF tidExists THEN
      baseObj := NARROW(typeRef,BaseDebug);
      baseObj.typeName := n;
    END;
  END declare_typename;

PROCEDURE declare_array (self: U; t,index,elt: TypeUID; s: BitSize) =
  VAR
    arrayRef : ArrayDebug;
  BEGIN
    arrayRef := NEW(ArrayDebug, index := index, elt := elt, bitSize := VAL(s,LONGINT), align := ptrBits);
    EVAL self.debugTable.put(t,arrayRef);
  END declare_array;

PROCEDURE declare_open_array (self: U; t, elt: TypeUID; s: BitSize) =
  VAR
    arrayRef : OpenArrayDebug;
  BEGIN
    arrayRef := NEW(OpenArrayDebug, elt := elt, bitSize := VAL(s,LONGINT), align := ptrBits);
    EVAL self.debugTable.put(t,arrayRef);
  END declare_open_array;

PROCEDURE declare_enum (self: U; t: TypeUID; n_elts: INTEGER; s: BitSize) =
  VAR
    enumRef : EnumDebug;
  BEGIN
    enumRef := NEW(EnumDebug, numElts := n_elts, bitSize := VAL(s,LONGINT), align := VAL(s,LONGINT));
    enumRef.elts := NEW(REF ARRAY OF Name,n_elts);
    enumRef.index := 0;
    EVAL self.debugTable.put(t,enumRef);
    self.debugObj := enumRef; (* keep for the elements *)
  END declare_enum;

PROCEDURE declare_enum_elt (self: U; n: Name) =
  VAR
    enumRef : EnumDebug;
  BEGIN
    enumRef := self.debugObj;
    <*ASSERT ISTYPE(enumRef,EnumDebug) *>
    enumRef.elts[enumRef.index] := n;
    INC(enumRef.index);
  END declare_enum_elt;

<*NOWARN*> PROCEDURE declare_packed (self: U; t: TypeUID; s: BitSize; base: TypeUID) =
  VAR
    packedRef : PackedDebug;
  BEGIN
    packedRef := NEW(PackedDebug, base := base, bitSize := VAL(s,LONGINT), align := ptrBits);
    EVAL self.debugTable.put(t,packedRef);
  END declare_packed;

PROCEDURE declare_record (self: U; t: TypeUID; s: BitSize; n_fields: INTEGER) =
  VAR
    recordRef : RecordDebug;
  BEGIN
    recordRef := NEW(RecordDebug, numFields := n_fields, bitSize := VAL(s,LONGINT), align := ptrBits);
    recordRef.fields := NEW(REF ARRAY OF FieldDebug,n_fields);
    recordRef.fieldIndex := 0;
    IF t = -1 THEN recordRef.global := TRUE; END;
    EVAL self.debugTable.put(t,recordRef);
    self.debugObj := recordRef; (* keep for the fields *)
  END declare_record;

PROCEDURE declare_field (self: U; n: Name; o: BitOffset; s: BitSize; t: TypeUID)=
  VAR
    recordRef : RecordDebug;
  BEGIN
    recordRef := self.debugObj;
    <*ASSERT ISTYPE(recordRef,RecordDebug) *>
    recordRef.fields[recordRef.fieldIndex] := NEW(FieldDebug,name := n, bitOffset := VAL(o,LONGINT), type := t, bitSize := VAL(s,LONGINT), align := VAL(s,LONGINT));
    INC(recordRef.fieldIndex);
  END declare_field;

PROCEDURE declare_set (self: U; t,domain: TypeUID;  s: BitSize) =
  VAR
    setRef : SetDebug;
  BEGIN
    setRef := NEW(SetDebug, domain := domain, bitSize := VAL(s,LONGINT), align := VAL(s,LONGINT));
    EVAL self.debugTable.put(t,setRef);
  END declare_set;

PROCEDURE declare_subrange (self: U; t,domain: TypeUID;                            READONLY min, max: Target.Int; s: BitSize) =
  VAR
    subRef : SubrangeDebug;
    minR,maxR : INTEGER;
  BEGIN
    EVAL TInt.ToInt (min, minR);
    EVAL TInt.ToInt (max, maxR);
    subRef := NEW(SubrangeDebug, domain := domain, min := VAL(minR,LONGINT), max := VAL(maxR,LONGINT), bitSize := VAL(s,LONGINT), align := VAL(s,LONGINT));
    EVAL self.debugTable.put(t,subRef);
  END declare_subrange;

PROCEDURE declare_pointer (self: U; t,target: TypeUID;  brand: TEXT; traced: BOOLEAN) =
  VAR
    ptrRef : PointerDebug;
  BEGIN
    ptrRef := NEW(PointerDebug, target := target, brand := brand, traced := traced, bitSize := ptrBits, align := ptrBits);
    EVAL self.debugTable.put(t,ptrRef);
  END declare_pointer;

PROCEDURE declare_indirect (self: U; t, target: TypeUID) =
  VAR
    indirectRef : IndirectDebug;
  BEGIN
    indirectRef := NEW(IndirectDebug, target := target);
    EVAL self.debugTable.put(t,indirectRef);
  END declare_indirect;

<*NOWARN*> PROCEDURE declare_proctype (self: U; t: TypeUID; n_formals: INTEGER; result: TypeUID;  n_raises: INTEGER; cc: CallingConvention) =
  VAR
    procRef : ProcTypeDebug;
  BEGIN
    procRef := NEW(ProcTypeDebug, numFormals := n_formals, numRaises := n_raises, result := result, cc := cc, bitSize := ptrBits, align := ptrBits);
    EVAL self.debugTable.put(t,procRef);
  END declare_proctype;

<*NOWARN*> PROCEDURE declare_formal (self: U; n: Name;  t: TypeUID) =
  BEGIN
(* fix me *)
  END declare_formal;

<*NOWARN*> PROCEDURE declare_raises (self: U; n: Name) =
  BEGIN
(* fix me *)
  END declare_raises;

PROCEDURE declare_object (self: U; t, super: TypeUID; brand: TEXT;  traced: BOOLEAN; n_fields, n_methods: INTEGER; field_size: BitSize) =
  VAR
    objectRef : ObjectDebug;
  BEGIN
    objectRef := NEW(ObjectDebug, superType := super, brand := brand, traced := traced, numFields := n_fields, numMethods := n_methods, fieldSize := VAL(field_size,LONGINT), bitSize := VAL(field_size,LONGINT), align := ptrBits);
    objectRef.fields := NEW(REF ARRAY OF FieldDebug,n_fields);
    objectRef.methods := NEW(REF ARRAY OF MethodDebug,n_methods);
    objectRef.fieldIndex := 0;
    objectRef.methodIndex := 0;
    EVAL self.debugTable.put(t,objectRef);
    self.debugObj := objectRef; (* keep for the fields and methods *)
  END declare_object;

<*NOWARN*> PROCEDURE declare_method (self: U; n: Name;  signature: TypeUID) =
  VAR
    objectRef : ObjectDebug;
  BEGIN
    objectRef := self.debugObj;
    <*ASSERT ISTYPE(objectRef,ObjectDebug) *>
    objectRef.methods[objectRef.methodIndex] := NEW(MethodDebug, name := n, signature := signature);
    INC(objectRef.methodIndex);
  END declare_method;

<*NOWARN*> PROCEDURE declare_opaque (self: U; t, super: TypeUID) =
  VAR
    opaqueRef : OpaqueDebug;
  BEGIN
    opaqueRef := NEW(OpaqueDebug, superType := super);
    EVAL self.debugTable.put(t,opaqueRef);
  END declare_opaque;

<*NOWARN*> PROCEDURE reveal_opaque (self: U; lhs, rhs: TypeUID) =
  BEGIN
(* fix me *)
  END reveal_opaque;

<*NOWARN*> PROCEDURE declare_exception (self: U;  n: Name;  arg_type: TypeUID; raise_proc: BOOLEAN; base: Var; offset: INTEGER) =
  BEGIN
(* fix me *)
  END declare_exception;

PROCEDURE widechar_size (self: U; size: INTEGER) =
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
  <* ASSERT FALSE *>
  END import_global;

PROCEDURE declare_segment (self: U;  n: Name;  m3t: TypeUID; is_const: BOOLEAN): Var =
  VAR
    v : LvVar := NewVar(self,n,0,0,Type.Struct,is_const,m3t,TRUE,FALSE,FALSE,M3CG.Maybe,VarType.Global);
    segName : TEXT;
  BEGIN
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
    v.segLen := s;
    v.exported := exported; (* check this in end_init *)
  END bind_segment;

<*NOWARN*> PROCEDURE declare_global (self: U;  n: Name;  s: ByteSize;  a: Alignment; t: Type;  m3t: TypeUID;  exported, inited: BOOLEAN): Var =
  VAR
    v : LvVar := NewVar(self,n,s,a,t,FALSE,m3t,TRUE,FALSE,exported,M3CG.Maybe,VarType.Global);
    globName : TEXT;
  BEGIN
    IF v.name = M3ID.NoID THEN
      globName := "m3global";
    ELSE
      globName := M3ID.ToText(v.name);
    END;

    v.lv := LLVM.LLVMAddGlobal(modRef, v.lvType, LT(globName));
    LLVM.LLVMSetInitializer(v.lv, LLVM.LLVMConstNull(v.lvType));
    LLVM.LLVMSetAlignment(v.lv, v.align);

    (* need to check linkage *)
    IF exported THEN
      (* check this maybe its available externally *)
      LLVM.LLVMSetLinkage(v.lv,LLVM.Linkage.External);
    ELSE
      LLVM.LLVMSetLinkage(v.lv,LLVM.Linkage.Internal);
    END;

    RETURN v;
  END declare_global;

<*NOWARN*> PROCEDURE declare_constant (self: U;  n: Name;  s: ByteSize;  a: Alignment; t: Type;  m3t: TypeUID;  exported, inited: BOOLEAN): Var =
  BEGIN
  (* fixme ever generated ??*)
  <* ASSERT FALSE *>
  END declare_constant;

PROCEDURE VName(v : LvVar; debug := FALSE) : TEXT =
  VAR
    name : TEXT;
  BEGIN
    IF v.name = M3ID.NoID THEN
      name := "tmp." & Fmt.Int(v.tag);
    ELSE
      name := M3ID.ToText(v.name);
    END;
    IF v.varType = VarType.Param AND NOT debug THEN
      name := name & ".addr";
    END;
    RETURN name;
  END VName;

PROCEDURE AllocVar(self : U; v : LvVar) =
  BEGIN
    v.lv := LLVM.LLVMBuildAlloca(builderIR, v.lvType, LT(VName(v)));
    LLVM.LLVMSetAlignment(v.lv,v.align);
    (* add to stack of allocs for this proc. Optimisation so we can
    remove those not referenced at the end *)
    Push(self.allocStack, v);
  END AllocVar;

<*NOWARN*> PROCEDURE declare_local (self: U;  n: Name;  s: ByteSize;  a: Alignment; t: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN; f: Frequency): Var =
  VAR
(*  in_memory is false for real locals and true for the temp locals for exceptions etc declared in body of procedure so guess the blocklevel test could be replaced with in_memory test below *)
    v : LvVar := NewVar(self,n,s,a,t,FALSE,m3t,in_memory,up_level,FALSE,f,VarType.Local);
    proc : LvProc;
  BEGIN
    (* locals are declared after declare_procedure or within a begin_procedure which are anonymous or inside begin_block. Since begin_procedure implies a begin_block, checking for blockLevel > 0 is sufficient to allocate now *)
    IF self.blockLevel = 0 THEN
      <*ASSERT self.declStack.size() = 1 *>
      (* get the current procs local stack *)
      proc := Get(self.declStack);
      PushRev(proc.localStack, v);
      v.inProc := proc;
      IF up_level THEN
        Push(proc.linkStack, v);
      END;
    ELSE
      (* inside a local block or begin_procedure so allocate it now *)
      self.allocVar(v);
      v.inProc := self.curProc;
    END;
    RETURN v;
  END declare_local;

<*NOWARN*> PROCEDURE declare_param (self: U;  n: Name;  s: ByteSize;  a: Alignment; t: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN; f: Frequency): Var =
  VAR
    v : LvVar := NewVar(self,n,s,a,t,FALSE,m3t,in_memory,up_level,FALSE,f,VarType.Param);
    proc : LvProc;
  BEGIN
    (* params declared after either declare_procedure or import_procedure
       (which could be in a begin_procedure), either way the procDecl should be set from the previous import or declare *)

    <*ASSERT self.declStack.size() = 1 *>
    proc := Get(self.declStack);
    PushRev(proc.paramStack, v);
    v.inProc := proc;
    IF up_level THEN
      Push(proc.linkStack, v);
    END;
    RETURN v;
  END declare_param;

PROCEDURE declare_temp (self: U; s: ByteSize; a: Alignment; t: Type; in_memory: BOOLEAN): Var =
  VAR
    v : LvVar := NewVar(self,M3ID.NoID,s,a,t,FALSE,0,in_memory,TRUE,FALSE,M3CG.Maybe,VarType.Temp);
    entryBB,curBB : LLVM.BasicBlockRef;
    firstInstr : LLVM.ValueRef;
  BEGIN
    (* temps are always declared inside a begin_procedure. However we
       allocate them in the entry BB to avoid dominate all uses problems *)
    curBB := LLVM.LLVMGetInsertBlock(builderIR);
    entryBB := LLVM.LLVMGetEntryBasicBlock(self.curProc.lvProc);

    IF entryBB = curBB THEN
      self.allocVar(v);
    ELSE
      (* alloc the temp in the entry BB *)
      firstInstr := LLVM.LLVMGetFirstInstruction(entryBB);
      LLVM.LLVMPositionBuilderBefore(builderIR, firstInstr);
      self.allocVar(v);
      LLVM.LLVMPositionBuilderAtEnd(builderIR, curBB);
    END;

    (* test - seems some code puts temps into the static link eg text
    pointers from concat referred to in a finally clause. Problem is there
    is no up_level flag so this puts all temps on the static link and have
    changed the uplevel flag to true in v declare above which could be
    very inefficient*)
    v.inProc := self.curProc;
    Push(self.curProc.linkStack, v);

    RETURN v;
  END declare_temp;

<*NOWARN*> PROCEDURE free_temp (self: U; v: Var) =
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
   filler with each init_xx call so that we dont need to regenerate the inits here which would mean only looping through the inits once. *)
PROCEDURE end_init (self: U;  v: Var) =
  VAR
    baseObj : BaseVar;
    int,numGlobs,typeSize,thisOfs,fillLen,segSize : INTEGER;
    realStr : TEXT;
    typesArr : TypeArrType;
    typesRef : TypeRefType;
    initsArr : ValueArrType;
    initsRef : ValueRefType;
    structVal,varVal : LLVM.ValueRef;
    proc : LvProc;
    var,thisVar : LvVar;
    newInits : RefSeq.T;
    fillVar : FillerVar;
  BEGIN
    (* generate the struct and the global segment var which holds all globals *)
    thisVar := NARROW(v,LvVar);
    numGlobs := thisVar.inits.size();

    IF numGlobs = 0 THEN RETURN; END;

    (* keep running total of offsets for filler calcs *)
    thisOfs := 0;

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
      fillLen := baseObj.offset - thisOfs;

      typeSize := VAL(LLVM.LLVMStoreSizeOfType(targetData,baseObj.lvTy),INTEGER);
      thisOfs := thisOfs + typeSize;

      (* add a filler *)
      IF fillLen > 0 THEN
        fillVar := NEW(FillerVar);
        fillVar.lvTy := LLVM.LLVMArrayType(i8Type,fillLen);
        thisOfs := thisOfs + fillLen;
        PushRev(newInits,fillVar);
      END;
      PushRev(newInits,baseObj);
    END;

(*
IO.Put("this " & Fmt.Int(thisOfs) & " " & Fmt.Int(baseObj.offset) & "\n");
*)
(* if we dont add the final zero in to agree with the bind size the rtlinker
crashes, also there is some mismatch with global longreal and external
the sizes seem to be out between gcc and llvm which causes the
segment sizes to mismatch and if you add a filler the s file can get it
twice to compensate - check this
*)
    IF thisOfs < thisVar.segLen THEN
      fillLen := thisVar.segLen - thisOfs;
      fillVar := NEW(FillerVar);
      fillVar.lvTy := LLVM.LLVMArrayType(i8Type,fillLen);
      PushRev(newInits,fillVar);
    END;

    (* update number of globals *)
    numGlobs := newInits.size();

    (* allocate the arrays for llvm *)
    typesRef := NewTypeArr(typesArr,numGlobs);
    initsRef := NewValueArr(initsArr,numGlobs);

    (* setup the types array *)
    FOR i := 0 TO numGlobs - 1 DO
      baseObj := Get(newInits,i);
      typesArr[i] := baseObj.lvTy;
    END;

    (* fill in the body of our opaque global struct now we know the types *)
    LLVM.LLVMStructSetBody(thisVar.lvType, typesRef, numGlobs, FALSE);

    segSize := VAL(LLVM.LLVMStoreSizeOfType(targetData,thisVar.lvType),INTEGER);
    <*ASSERT segSize = thisVar.segLen *>
(*
IO.Put("Store size of global " & Fmt.Int(segSize) & "\n");
*)
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
      | TextVar(v) =>  EVAL v; (* aready done in construct type *)
      | FloatVar(v) =>
          realStr := ConvertFloat(v.value);
          v.lvVal := LLVM.LLVMConstRealOfString(v.lvTy, LT(realStr));
      | FillerVar(v) =>
          v.lvVal := LLVM.LLVMConstNull(v.lvTy);
      ELSE
        <*ASSERT FALSE*>
      END;
      initsArr[i] := baseObj.lvVal;
    END;

    (* save the initialisers *)
    structVal := LLVM.LLVMConstNamedStruct(thisVar.lvType, initsRef, numGlobs);

    LLVM.LLVMSetInitializer(thisVar.lv, structVal);
    LLVM.LLVMSetAlignment(thisVar.lv, thisVar.align);

    thisVar.inits := newInits; (* keep a ref *)
    self.curVar := NIL;

    DebugGlobals(self,thisVar);
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
    p.linkStack := NEW(RefSeq.T).init();
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
    numParams,numLocals : CARDINAL;
    name : TEXT;
    arg : REFANY;
  BEGIN
    (*  Declare this procedure and all its locals and parameters.*)
    self.curProc := p;
    INC(self.blockLevel);

    proc := NARROW(p,LvProc);

    (* top of stack is current proc *)
    proc.saveBB := LLVM.LLVMGetInsertBlock(builderIR);
    Push(self.procStack,proc);

    (* create the function *)
    self.buildFunc(p);
    numParams := proc.numParams;

    (* set debug loc to nul here to run over prologue instructions *)
    DebugClearLoc(self);

    (* create the entry basic block *)
    bbRef := LLVM.LLVMAppendBasicBlockInContext(globContext, self.curProc.lvProc,  LT("entry"));
    LLVM.LLVMPositionBuilderAtEnd(builderIR,bbRef);

    (* allocate the params if not a struct *)
    lVal := LLVM.LLVMGetFirstParam(self.curProc.lvProc);

    FOR i := 0 TO numParams - 1 DO
      arg := Get(proc.paramStack,i);
      param := NARROW(arg,LvVar);
      IF param.type # Type.Struct THEN
        self.allocVar(param);
        (* static link support - save the lv for this proc *)
        IF Text.Equal("_link", M3ID.ToText(param.name)) THEN
          self.curProc.linkAdrLv := param.lv;
        END;
       (* do the stores for the parameters *)
        storeVal := LLVM.LLVMBuildStore(builderIR, lVal, param.lv);
        LLVM.LLVMSetAlignment(storeVal,param.align);
      ELSE
        (* refer directly to the param *)
        param.lv := lVal;
      END;
      lVal := LLVM.LLVMGetNextParam(lVal);
    END;

    (* allocate the locals *)
    numLocals := proc.localStack.size();
    FOR i := 0 TO numLocals - 1 DO
      arg := Get(proc.localStack,i);
      local := NARROW(arg,LvVar);
      name := M3ID.ToText(local.name);
      self.allocVar(local);
    END;

   (* debug for locals and params here, need the stacks intact *)
    DebugLocalsParams(self,proc);
  END begin_procedure;

<*NOWARN*> PROCEDURE end_procedure (self: U;  p: Proc) =
(* marks the end of the code for procedure 'p'.  Sets "current procedure"
   to NIL.  Implies an end_block.  *)
  VAR
    proc : LvProc;
    v : LvVar;
    prevInstr : LLVM.ValueRef;
    opCode : LLVM.Opcode;
    curBB : LLVM.BasicBlockRef;
    use : LLVM.UseRef;
    numAllocs : CARDINAL;
  BEGIN
    proc := NARROW(p,LvProc);
    (* its possible a no return warning will generate an abort and no return
    but llvm has mandatory return so if last instruction is not a return then add a dummy one, could possible add return to front end in this case *)
    curBB := LLVM.LLVMGetInsertBlock(builderIR);
    prevInstr := LLVM.LLVMGetLastInstruction(curBB);
    IF prevInstr # NIL THEN
      opCode := LLVM.LLVMGetInstructionOpcode(prevInstr);
      IF opCode # LLVM.Opcode.Ret THEN
        IF proc.returnType = Type.Void THEN
          EVAL LLVM.LLVMBuildRetVoid(builderIR);
        ELSE
          EVAL LLVM.LLVMBuildRet(builderIR,LLVM.LLVMGetUndef(LLvmType(proc.returnType)));
        END;
      END;
    END;

    (* check uses and remove allocs that have none *)
(* temp disable. stuffs the static link allocs somehow. - check*)
(*
    IF self.allocStack.size() > 0 THEN
      numAllocs := self.allocStack.size();
      FOR i := 0 TO numAllocs - 1 DO
        v := Get(self.allocStack);
        use := LLVM.LLVMGetFirstUse(v.lv);
        IF use = NIL THEN
          LLVM.LLVMInstructionEraseFromParent(v.lv);
        END;
        Pop(self.allocStack);
      END;
    END;
*)
    DEC(self.blockLevel);

    Pop(self.procStack);
    IF self.procStack.size() > 0 THEN
      LLVM.LLVMPositionBuilderAtEnd(builderIR,self.curProc.saveBB);
      self.curProc := Get(self.procStack);
    ELSE
      self.curProc := NIL;
    END; 
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
(* note that nested procedure 'p's body occured at the current location
   in the source.  In particular, nested in whatever procedures,
   anonymous blocks, or exception scopes surround this point. *)
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
    label := self.getLabel(l,"label_");

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
    <*ASSERT label.branchList # NIL *>
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
    LLVM.LLVMPositionBuilderAtEnd(builderIR,label.labBB);
  END set_label;

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
      label := NEW(LabelObj, id := l, branchList := NEW(RefSeq.T).init());
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

PROCEDURE BuildCmp(self: U; a,b : LLVM.ValueRef; t: ZType; op : CompareOp; l: Label) =
  VAR
    cmpVal : LLVM.ValueRef;
    label : LabelObj;
  BEGIN
    cmpVal := CompareVal(a,b,op,t);

    label := self.getLabel(l,"if_");

    label.cmpInstr := cmpVal;
    label.elseBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT("else_" & Fmt.Int(l)));
    label.condLv := LLVM.LLVMBuildCondBr(builderIR,cmpVal,label.labBB, label.elseBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,label.elseBB);
  END BuildCmp;

<*NOWARN*> PROCEDURE IfCommon(self: U;  t: IType;  l: Label;  f: Frequency; op : CompareOp) =
  VAR
    s0 := Get(self.exprStack);
    a,b : LLVM.ValueRef;
    intType : LLVM.TypeRef;
  BEGIN
    intType := LLvmType(t);
    a := NARROW(s0,LvExpr).lVal;
    b := Zero(intType);
    BuildCmp(self,a,b,t,op,l);
    Pop(self.exprStack);
  END IfCommon;

<*NOWARN*> PROCEDURE if_true (self: U;  t: IType;  l: Label;  f: Frequency) =
(* tmp := s0.t; pop; IF (tmp # 0) GOTO l *)
  BEGIN
    self.ifCommon(t,l,f,CompareOp.NE);
  END if_true;

<*NOWARN*> PROCEDURE if_false (self: U;  t: IType;  l: Label;  f: Frequency) =
(* tmp := s0.t; pop; IF (tmp = 0) GOTO l *)
  BEGIN
    self.ifCommon(t,l,f,CompareOp.EQ);
  END if_false;

<*NOWARN*> PROCEDURE if_compare (self: U;  t: ZType;  op: CompareOp;  l: Label;  f: Frequency) =
(*== compare(t, Int32, op); if_true(Int32, l,f)*)
(*alt  IF (s1.t op s0.t) GOTO l ; pop(2) *)
  VAR
    (* using alt version *)
    s0 := Get(self.exprStack,0);
    s1 := Get(self.exprStack,1);
    a,b : LLVM.ValueRef;
  BEGIN
    a := NARROW(s1,LvExpr).lVal;
    b := NARROW(s0,LvExpr).lVal;

    BuildCmp(self,a,b,t,op,l);

    Pop(self.exprStack,2);
  END if_compare;

(* get or create a new label and the bb that goes with it *)
PROCEDURE GetLabel(self : U; l : Label; name : TEXT) : LabelObj =
  VAR
    label : LabelObj;
    labRef : REFANY;
    labelExists : BOOLEAN;
  BEGIN
    labelExists := self.labelTable.get(l,labRef);
    IF NOT labelExists THEN
      label := NEW(LabelObj, id := l, branchList := NEW(RefSeq.T).init());
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

    (* this depends on case values normalised to range from zero up.
    Also since m3 handles the else case we dont have a default label
    so we arbitrarily use the first.*)
    elseLab := labels[0];
    label := self.getLabel(elseLab,"case_");
    numCases := NUMBER(labels);
    switchLVal := LLVM.LLVMBuildSwitch(builderIR, a, label.labBB, numCases);
    FOR i := FIRST(labels) TO LAST(labels) DO
      label := self.getLabel(labels[i],"case_");
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
  CONST numParams = 1;
  VAR
    gepVal : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
  BEGIN
    paramsRef := NewValueArr(paramsArr,numParams);
    paramsArr[0] := ofs;
    (* inbounds ?? *)
    IF NOT const THEN
      gepVal := LLVM.LLVMBuildGEP(builderIR, src, paramsRef, 1, LT("gepIdx"));
    ELSE
      gepVal := LLVM.LLVMConstGEP(src, paramsRef, 1);
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

PROCEDURE SearchStaticLink(proc : LvProc; var : LvVar) : INTEGER =
  VAR
    tp : LvProc;
    v : LvVar;
    index : INTEGER := 0;
  BEGIN
    tp := proc.parent;
    WHILE tp # NIL DO
      IF tp.linkStack # NIL THEN (*importeds are nil *)
        FOR i := 0 TO tp.linkStack.size() - 1 DO
          v := Get(tp.linkStack,i);
          IF v = var THEN
            RETURN(index);
          END;
          INC(index);
        END;
      END;
      tp := tp.parent;
    END;
    RETURN -1;
  END SearchStaticLink;

PROCEDURE FindLinkVar(self : U; var : LvVar) : LLVM.ValueRef =
  VAR
    linkIdx,lv : LLVM.ValueRef;
    idx : INTEGER := -1;
  BEGIN
    idx := SearchStaticLink(self.curProc, var);
    <*ASSERT idx >= 0 *>
    linkIdx := LLVM.LLVMBuildLoad(builderIR, self.curProc.linkAdrLv, LT("link_adr"));
    lv := BuildGep(linkIdx,idx * ptrBytes);
    lv := LLVM.LLVMBuildBitCast(builderIR, lv, AdrAdrTy, LT("link_adradr"));
    lv := LLVM.LLVMBuildLoad(builderIR, lv, LT("link_ofs"));
    RETURN lv;
  END FindLinkVar;

PROCEDURE LoadExtend(val : LLVM.ValueRef; t : MType; u : ZType) : LLVM.ValueRef =
  VAR destTy : LLVM.TypeRef;
  BEGIN
    destTy := LLvmType(u);
    IF TypeSize(t) < TypeSize(u) THEN
      IF WordTypes(t) THEN
        val := LLVM.LLVMBuildZExt(builderIR,val,destTy, LT("load_zext"));
      ELSE
        val := LLVM.LLVMBuildSExt(builderIR,val,destTy, LT("load_sext"));
      END;
    END;
    RETURN val;
  END LoadExtend;

PROCEDURE load (self: U;  v: Var;  o: ByteOffset;  t: MType;  u: ZType) =
(* push; s0.u := Mem [ ADR(v) + o ].t ; *)
  VAR
    src : LvVar;
    srcVal,destVal : LLVM.ValueRef;
    srcTy,srcPtrTy : LLVM.TypeRef;
  BEGIN
    src := NARROW(v,LvVar);
    srcVal := src.lv;
    srcTy := LLvmType(t);
    srcPtrTy := LLVM.LLVMPointerType(srcTy);

    (* check if a static link var to load *)
    IF src.up_level THEN
      (* if the var is not local to this proc then search all the static links *)
      IF src.inProc # self.curProc THEN
        srcVal := FindLinkVar(self,src);
        srcVal := LLVM.LLVMBuildBitCast(builderIR, srcVal, srcPtrTy, LT("link_cast"));
      END;
    END;

    IF src.type = Type.Struct THEN
      IF o # 0 THEN
        srcVal := BuildGep(srcVal,o);
      END;
      srcVal := LLVM.LLVMBuildBitCast(builderIR, srcVal, srcPtrTy, LT("load_toptr"));
    END;

    destVal := LLVM.LLVMBuildLoad(builderIR, srcVal, VarName(v));
    LLVM.LLVMSetAlignment(destVal,src.align);

    (* only load 64 or 32 bit sizes *)
    destVal := LoadExtend(destVal,t,u);

    Push(self.exprStack,NEW(LvExpr,lVal := destVal));
  END load;

PROCEDURE GetBaseType(ty : LLVM.TypeRef) : LLVM.TypeRef =
  VAR
    tyKind : LLVM.TypeKind;
  BEGIN
    LOOP
      tyKind := LLVM.LLVMGetTypeKind(ty);
      IF tyKind # LLVM.TypeKind.Pointer THEN
        RETURN ty;
      END;
      ty := LLVM.LLVMGetElementType(ty);
    END;
  END GetBaseType;
  
PROCEDURE store (self: U;  v: Var;  o: ByteOffset;  t: ZType;  u: MType) =
(* Mem [ ADR(v) + o ].u := s0.t; pop *)
  VAR
    s0 := Get(self.exprStack);
    src : LvExpr;
    dest : LvVar;
    srcTy,destTy,destPtrTy,destEltTy,srcBaseTy,destBaseTy : LLVM.TypeRef;
    srcVal,destVal,storeVal : LLVM.ValueRef;
  BEGIN
    src := NARROW(s0,LvExpr);
    dest := NARROW(v,LvVar);
    srcVal := src.lVal;
    destVal := dest.lv;

    srcTy := LLvmType(t);
    destTy := LLvmType(u);
    destPtrTy := LLVM.LLVMPointerType(destTy);

    IF TypeSize(u) # TypeSize(t) THEN
      IF TypeSize(u) < TypeSize(t) THEN
        srcVal := LLVM.LLVMBuildTrunc(builderIR,srcVal,destTy,
                                      LT("store_trunc"));
      ELSE
        srcVal := LLVM.LLVMBuildSExt(builderIR,srcVal,destTy,
                                      LT("store_sext"));
      END;
    END;

    (* check if static link var to store *)
    IF dest.up_level THEN
      IF dest.inProc # self.curProc THEN
        destVal := FindLinkVar(self,dest);
        destVal := LLVM.LLVMBuildBitCast(builderIR, destVal, destPtrTy, LT("link_cast"));
      END;
    END;

    IF o # 0 THEN
      destVal := BuildGep(destVal,o);
    END;

    IF dest.type = Type.Addr THEN
      srcBaseTy := GetBaseType(LLVM.LLVMTypeOf(srcVal));
      destBaseTy := GetBaseType(LLVM.LLVMTypeOf(destVal));
      IF srcBaseTy # destBaseTy THEN
        (* remove the first pointer *)
        destEltTy := LLVM.LLVMGetElementType(LLVM.LLVMTypeOf(destVal));
        srcVal := LLVM.LLVMBuildBitCast(builderIR, srcVal, destEltTy, LT("store_ptr"));
      END;
    ELSIF dest.type = Type.Struct THEN
      (* get pointer to u type bit cast dest to that then bitcast src to u type *)
      srcVal := LLVM.LLVMBuildBitCast(builderIR, srcVal, destTy, LT("store_srcptr"));
      destVal := LLVM.LLVMBuildBitCast(builderIR, destVal, destPtrTy, LT("store_destptr"));
    ELSE
      destVal := LLVM.LLVMBuildBitCast(builderIR, destVal, destPtrTy, LT("store_ptr"));
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
    IF o # 0 THEN
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
    srcTy : LLVM.TypeRef;
  BEGIN
    srcVal := src.lVal;
    srcTy := LLvmType(t);
    srcTy := LLVM.LLVMPointerType(srcTy);

    IF o # 0 THEN
      srcVal := BuildGep(srcVal,o);
    END;

    srcVal := LLVM.LLVMBuildBitCast(builderIR, srcVal, srcTy, LT("load_ind_toptr"));
    srcVal := LLVM.LLVMBuildLoad(builderIR, srcVal, LT("load_ind"));
(* fixme set alignment on load where is the value?? unless its the size of
the type. need to check if its worth doing and correct. Only seems to
change 1 or 2 instructions in testing. *)
(*
    LLVM.LLVMSetAlignment(srcVal,TypeSize(u));
*)
    (* only load 64 or 32 bit sizes *)
    srcVal := LoadExtend(srcVal,t,u);

    src.lVal := srcVal;
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
    destTy := LLvmType(u);
    destPtrTy := LLVM.LLVMPointerType(destTy);

    IF o # 0 THEN
      destVal := BuildGep(destVal,o);
    END;

    IF TypeSize(u) < TypeSize(t) THEN
      srcVal := LLVM.LLVMBuildTrunc(builderIR,srcVal,destTy,
                                      LT("store_ind_trunc"));
    END;

    destVal := LLVM.LLVMBuildBitCast(builderIR, destVal, destPtrTy, LT("store_ind_toptr"));    
    dest.lVal := LLVM.LLVMBuildStore(builderIR, srcVal, destVal);
(* fixme set alignment on store where is the value?? unless its the size of
the type
    LLVM.LLVMSetAlignment(dest.lVal,Typeize(u));
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
    buf : ARRAY[0..30] OF CHAR;
    result : TEXT;
  BEGIN
    lastCh := TFloat.ToChars(f,buf);
    buf[lastCh] := '\000';
    (* llvm uses E as exponent char *)
    FOR i := 0 TO lastCh - 1 DO
      IF buf[i] = 'X' THEN buf[i] := 'E'; END;
    END;
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
    opType : LLVM.TypeRef;
    ite : ITEObj;
  BEGIN
    a := NARROW(s1,LvExpr).lVal;
    b := NARROW(s0,LvExpr).lVal;
    opType := LLvmType(t);

    cmpVal := CompareVal(a,b,op,t);

    ite := NEW(ITEObj,cmpVal := cmpVal,opName := "cmp", opType := IntPtrTy, curProc := self.curProc).init();
    EVAL ite.block(One(IntPtrTy),FALSE);
    res := ite.block(Zero(IntPtrTy),TRUE);

    (* convert to IType - probably not needed with IntPtrTy 
    destTy := LLvmType(u);
    res := LLVM.LLVMBuildSExt(builderIR,res,destTy, LT("cmp_sext"));
    *)
    
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
    ite : ITEObj;
    res : LLVM.ValueRef;
    intType : BOOLEAN;
  BEGIN
    IF WordTypes(t) THEN RETURN; END;
    a := NARROW(s0,LvExpr).lVal;
    opType := LLvmType(t);
    intType := t < Type.Reel;

    IF intType THEN
      cmpVal := LLVM.LLVMBuildICmp(builderIR,  LLVM.IntPredicate.SLT, a, Zero(opType), LT("abs_icmp"));
    ELSE
      cmpVal := LLVM.LLVMBuildFCmp(builderIR,  LLVM.RealPredicate.OLT, a, Zero(opType), LT("abs_fcmp"));
    END;

    ite := NEW(ITEObj,cmpVal := cmpVal,opName := "abs", opType := opType, curProc := self.curProc).init();
    IF intType THEN
      negRef := LLVM.LLVMBuildNSWNeg(builderIR, a, LT("abs_ineg"));
    ELSE
      negRef := LLVM.LLVMBuildFNeg(builderIR, a, LT("abs_fneg"));
    END;
    EVAL ite.block(negRef,FALSE);
    res := ite.block(a,TRUE);
    NARROW(s0,LvExpr).lVal := res;
  END abs;

PROCEDURE MinMax (self: U;  t: ZType; doMin : BOOLEAN) =
  VAR
    s0 := Get(self.exprStack,0);
    s1 := Get(self.exprStack,1);
    opType : LLVM.TypeRef;
    a,b,cmpVal,res : LLVM.ValueRef;
    ite : ITEObj;
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
      cmpVal := LLVM.LLVMBuildICmp(builderIR,  ipred , a, b, LT(opName & "_cmp"));
    ELSE
      cmpVal := LLVM.LLVMBuildFCmp(builderIR,  rpred , a, b, LT(opName & "_cmp"));
    END;

    ite := NEW(ITEObj, cmpVal := cmpVal, opName := opName, opType := opType, curProc := self.curProc).init();
    EVAL ite.block(a,FALSE);
    res := ite.block(b,TRUE);
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
    typesArr : TypeArrType;
    typesRef : TypeRefType;
  BEGIN
    typesRef := NewTypeArr(typesArr,1);
    CASE t OF
    |  Type.Reel  => typesArr[0] := LLVM.LLVMFloatType();
    |  Type.LReel => typesArr[0] := LLVM.LLVMDoubleType();
    |  Type.XReel => typesArr[0] := LLVM.LLVMX86FP80Type();
    END;
    RETURN typesRef;
  END IntrinsicRealTypes;

PROCEDURE IntrinsicMemTypes(p1,p2,p3 : LLVM.TypeRef) : UNTRACED REF LLVM.TypeRef =
  VAR
    typesArr : TypeArrType;
    typesRef : TypeRefType;
  BEGIN
    typesRef := NewTypeArr(typesArr,3);
    typesArr[0] := p1;
    typesArr[1] := p2;
    typesArr[2] := p3;
    RETURN typesRef;
  END IntrinsicMemTypes;

PROCEDURE MemSetFn() : LLVM.ValueRef =
  CONST iName = "llvm.memset.p0i8.i64";
  VAR
    memsetId := GetIntrinsicId(iName);
    (* 2 types for overloaded memset *)
    Types := IntrinsicMemTypes(AdrTy,IntPtrTy,NIL);
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,memsetId,Types,2);
  END MemSetFn;

PROCEDURE MemCopyFn() : LLVM.ValueRef =
  CONST iName = "llvm.memcpy.p0i8.p0i8.i64";
  VAR
    memcpyId := GetIntrinsicId(iName);
    (* 3 types for overloaded memcpy *)
    Types := IntrinsicMemTypes(AdrTy,AdrTy,IntPtrTy);
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,memcpyId,Types,3);
  END MemCopyFn;

PROCEDURE MemMoveFn() : LLVM.ValueRef =
  CONST iName = "llvm.memmove.p0i8.p0i8.i64";
  VAR
    memmovId := GetIntrinsicId(iName);
    (* 3 types for overloaded memmov *)
    Types := IntrinsicMemTypes(AdrTy,AdrTy,IntPtrTy);
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,memmovId,Types,3);
  END MemMoveFn;

PROCEDURE RoundFn(t : RType) : LLVM.ValueRef =
  CONST iName = "llvm.round";
  VAR
    roundId := GetIntrinsicId(iName & FExt(t));
    Types := IntrinsicRealTypes(t);
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,roundId,Types,1);
  END RoundFn;

PROCEDURE TruncFn(t : RType) : LLVM.ValueRef =
  CONST iName = "llvm.trunc";
  VAR
    truncId := GetIntrinsicId(iName & FExt(t));
    Types := IntrinsicRealTypes(t);
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,truncId,Types,1);
  END TruncFn;

PROCEDURE FloorFn(t : RType) : LLVM.ValueRef =
  CONST iName = "llvm.floor";
  VAR
    floorId := GetIntrinsicId(iName & FExt(t));
    Types := IntrinsicRealTypes(t);
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,floorId,Types,1);
  END FloorFn;

PROCEDURE CeilFn(t : RType) : LLVM.ValueRef =
  CONST iName = "llvm.ceil";
  VAR
    ceilId := GetIntrinsicId(iName & FExt(t));
    Types := IntrinsicRealTypes(t);
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,ceilId,Types,1);
  END CeilFn;

PROCEDURE DoCvtInt(var : LLVM.ValueRef; op : ConvertOp; t : RType) : LLVM.ValueRef =
  CONST numParams = 1;
  VAR
    res,fn : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
  BEGIN
    paramsRef := NewValueArr(paramsArr,numParams);
    paramsArr[0] := var;
    CASE op OF
    | ConvertOp.Round => fn := RoundFn(t);
    | ConvertOp.Trunc => fn := TruncFn(t);
    | ConvertOp.Floor => fn := FloorFn(t);
    | ConvertOp.Ceiling => fn := CeilFn(t);
    END;
    res := LLVM.LLVMBuildCall(builderIR, fn, paramsRef, numParams, LT("cvtint"));
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
    s0 := Get(self.exprStack,0);
    a,lVal : LLVM.ValueRef;
    realTy : LLVM.TypeRef;
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
    realTy := LLvmType(u);
    IF t < Type.Reel THEN
      IF WordTypes(t) THEN
        lVal := LLVM.LLVMBuildUIToFP(builderIR, a, realTy, LT("ufloat"));
      ELSE
        lVal := LLVM.LLVMBuildSIToFP(builderIR, a, realTy, LT("sfloat"));
      END;
    ELSE
      IF TypeSize(u) = TypeSize(t) THEN
        lVal := a; (* noop *)
      ELSE
        IF TypeSize(u) < TypeSize(t) THEN
          lVal := LLVM.LLVMBuildFPTrunc(builderIR, a, realTy, LT("ftrunc"));
        ELSE
          lVal := LLVM.LLVMBuildFPExt(builderIR, a, realTy, LT("fext"));
        END;
      END;
    END;
    NARROW(s0,LvExpr).lVal := lVal;
  END cvt_float;

(* helper function for div and mod to add or subtract a value in case
   where one of the operands is negative.
   if mod # 0 then
     if div < 0 then
        add fixup to div or mod
*)

PROCEDURE GenDivMod(self : U; t : IType; isDiv : BOOLEAN; divVal,modVal,fixVal : LLVM.ValueRef) : LLVM.ValueRef =
  VAR
    curBB,thenBB,elseBB,exitBB : LLVM.BasicBlockRef;
    cmpVal,storeVal,tmpLv,res : LLVM.ValueRef;
    opType : LLVM.TypeRef;
  BEGIN
    opType := LLvmType(t);
    tmpLv := LLVM.LLVMBuildAlloca(builderIR, opType, LT("divmod_tmp"));
    IF isDiv THEN storeVal := divVal; ELSE storeVal := modVal; END;
    EVAL LLVM.LLVMBuildStore(builderIR, storeVal, tmpLv);

    curBB := LLVM.LLVMGetInsertBlock(builderIR);
    thenBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT("div_then"));
    elseBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT("div_else"));
    exitBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT("div_end"));
    LLVM.LLVMPositionBuilderAtEnd(builderIR,curBB);
    (* check if mod is zero *)
    cmpVal := LLVM.LLVMBuildICmp(builderIR,  LLVM.IntPredicate.EQ, modVal, Zero(opType), LT("mod_cmp"));

    EVAL LLVM.LLVMBuildCondBr(builderIR,cmpVal,exitBB,thenBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,thenBB);
    (* check if div is < 0 *)
    cmpVal := LLVM.LLVMBuildICmp(builderIR,  LLVM.IntPredicate.SGE, divVal, Zero(opType), LT("div_cmp"));

    EVAL LLVM.LLVMBuildCondBr(builderIR,cmpVal,exitBB,elseBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,elseBB);
    (* add fix to div or mod *)
    res := LLVM.LLVMBuildNSWAdd(builderIR, storeVal, fixVal, LT("divmod_add"));

    EVAL LLVM.LLVMBuildStore(builderIR, res, tmpLv);
    EVAL LLVM.LLVMBuildBr(builderIR,exitBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,exitBB);
    res := LLVM.LLVMBuildLoad(builderIR, tmpLv, LT("divmod_load"));
    RETURN res;
  END GenDivMod;

PROCEDURE DivMod(self : U; t : IType; isDiv : BOOLEAN) : LLVM.ValueRef=
  VAR
    res,fixup : LLVM.ValueRef;
    divRes,modRes,num,div : LLVM.ValueRef;
  BEGIN
    (* save numerator and denominator *)
    div := NARROW(Get(self.exprStack,0),LvExpr).lVal;
    num := NARROW(Get(self.exprStack,1),LvExpr).lVal;
    (* do a div to get the sign of the result *)
    binop(self,t,BinOps.div);
    divRes := NARROW(Get(self.exprStack,0),LvExpr).lVal;
    (* restore stack for mod *)
    Pop(self.exprStack);
    Push(self.exprStack,NEW(LvExpr,lVal := num));
    Push(self.exprStack,NEW(LvExpr,lVal := div));
    (* do the mod *)
    binop(self,t,BinOps.mod);
    modRes := NARROW(Get(self.exprStack,0),LvExpr).lVal;
    IF isDiv THEN
      (* adjust div for neg operands *)
      fixup := LLVM.LLVMConstInt(LLvmType(t), VAL(-1,LONGINT), TRUE);
    ELSE
      (* same for mod *)
      fixup := div;
    END;
    res := GenDivMod(self, t, isDiv, divRes, modRes, fixup);
    RETURN res;
  END DivMod;

<*NOWARN*> PROCEDURE div (self: U;  t: IType;  a, b: Sign) =
 (* s1.t := s1.t DIV s0.t;pop*)
  VAR
    s0 : REFANY;
    res : LLVM.ValueRef;
  BEGIN
    res := DivMod(self,t,TRUE);
    s0 := Get(self.exprStack,0);
    NARROW(s0,LvExpr).lVal := res;
  END div;

<*NOWARN*> PROCEDURE mod (self: U;  t: IType;  a, b: Sign) =
 (* s1.t := s1.t MOD s0.t;pop*)
  VAR
    s0 : REFANY;
    res : LLVM.ValueRef;
  BEGIN
    res := DivMod(self,t,FALSE);
    s0 := Get(self.exprStack,0);
    NARROW(s0,LvExpr).lVal := res;
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
    res : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
  BEGIN
    paramsRef := NewValueArr(paramsArr,maxParams);
    paramsArr[0] := p1;
    paramsArr[1] := p2;
    paramsArr[2] := p3;
    paramsArr[3] := p4;
    res := LLVM.LLVMBuildCall(builderIR, fn, paramsRef, numParams, LT(""));
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
    res := SetCall(setRange,3,s1,s0,s2);
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
    ite : ITEObj;
  BEGIN
    a := NARROW(s1,LvExpr).lVal;
    shift := NARROW(s0,LvExpr).lVal;
    opType := LLvmType(t);
    cmpVal := LLVM.LLVMBuildICmp(builderIR,  LLVM.IntPredicate.SGE, shift, Zero(opType), LT("shift_cmp"));

    ite := NEW(ITEObj, cmpVal := cmpVal, opName := "shift", opType := opType, curProc := self.curProc).init();
    res := LLVM.LLVMBuildShl(builderIR, a, shift, LT("shl"));
    EVAL ite.block(res,FALSE);
    (* make the shift positive  *)
    shift := LLVM.LLVMBuildNSWNeg(builderIR, shift, LT("neg"));
    res := LLVM.LLVMBuildLShr(builderIR, a, shift, LT("shr"));
    res := ite.block(res,TRUE);
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
    t1 := LLVM.LLVMBuildNUWSub(builderIR, wordSize, shift, LT("rsub"));
    IF rotLeft THEN
      t2 := LLVM.LLVMBuildLShr(builderIR, value, t1, LT("rshr"));
      t3 := LLVM.LLVMBuildShl(builderIR, value, shift, LT("rshl"));
    ELSE (* rot right *)
      t2 := LLVM.LLVMBuildShl(builderIR, value, t1, LT("rshr"));
      t3 := LLVM.LLVMBuildLShr(builderIR, value, shift, LT("rshr"));
    END;
    t4 := LLVM.LLVMBuildOr(builderIR, t2, t3, LT("ror"));
    RETURN t4;
  END DoRotate;

PROCEDURE rotate (self: U;  t: IType) =
  (* s1.t := Word.Rotate (s1.t, s0.t); pop *)
  VAR
    s0 := Get(self.exprStack,0);
    s1 := Get(self.exprStack,1);
    a,shift,cmpVal,res : LLVM.ValueRef;
    opType : LLVM.TypeRef;
    ite : ITEObj;
  BEGIN
    a := NARROW(s1,LvExpr).lVal;
    shift := NARROW(s0,LvExpr).lVal;
    opType := LLvmType(t);
    cmpVal := LLVM.LLVMBuildICmp(builderIR,  LLVM.IntPredicate.SGE, shift, Zero(opType), LT("rotate_cmp"));

    ite := NEW(ITEObj, cmpVal := cmpVal, opName := "rotate", opType := opType, curProc := self.curProc).init();
    res := DoRotate(a,shift,TRUE);
    EVAL ite.block(res,FALSE);
    (* make the shift positive  *)
    shift := LLVM.LLVMBuildNSWNeg(builderIR, shift, LT("neg"));
    res := DoRotate(a,shift,FALSE);
    res := ite.block(res,TRUE);
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
    Int32Ty := LLVM.LLVMInt32Type();
    a,lVal : LLVM.ValueRef;
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
    t1 := LLVM.LLVMBuildNSWSub(builderIR, wordSize, count, LT("elen"));
    t2 := LLVM.LLVMBuildNSWSub(builderIR, t1, offset, LT("edist"));
    t3 := LLVM.LLVMBuildShl(builderIR, val, t2, LT("eshl"));
    t4 := LLVM.LLVMBuildLShr(builderIR, t3, t1, LT("eshr"));
    IF sign THEN
      t4 := LLVM.LLVMBuildSExt(builderIR, t4, IntPtrTy, LT("esext"));
    END;
    RETURN t4;
  END DoExtract;

<*NOWARN*> PROCEDURE extract (self: U;  t: IType;  sign: BOOLEAN) =
  (* s2.t := Word.Extract(s2.t, s1.t, s0.t);
     IF sign THEN SignExtend s2; pop(2) *)
  VAR
    s0 := Get(self.exprStack,0);
    s1 := Get(self.exprStack,1);
    s2 := Get(self.exprStack,2);
    a,offset,count,res,cmpVal : LLVM.ValueRef;
    opType : LLVM.TypeRef;
    ite : ITEObj;
  BEGIN
    a := NARROW(s2,LvExpr).lVal;
    count := NARROW(s0,LvExpr).lVal;
    offset := NARROW(s1,LvExpr).lVal;
    opType := LLvmType(t);
    cmpVal := LLVM.LLVMBuildICmp(builderIR,  LLVM.IntPredicate.EQ, count, Zero(opType), LT("extract_cmp"));
    (* if count zero return zero *)
    ite := NEW(ITEObj, cmpVal := cmpVal, opName := "extract", opType := opType, curProc := self.curProc).init();
    res := Zero(opType);
    EVAL ite.block(res,FALSE);
    res := DoExtract(a,count,offset,sign);
    res := ite.block(res,TRUE);
    NARROW(s2,LvExpr).lVal := res;
    Pop(self.exprStack,2);
  END extract;

PROCEDURE extract_n (self: U;  t: IType;  sign: BOOLEAN;  n: CARDINAL) =
  (* s1.t := Word.Extract(s1.t, s0.t, n);
     IF sign THEN SignExtend s1; pop(1) *)
  VAR
    s0 := Get(self.exprStack,0);
    s1 := Get(self.exprStack,1);
    a,offset,count,res : LLVM.ValueRef;
  BEGIN
    IF n = 0 THEN
      res := Zero(LLvmType(t));
    ELSE
      a := NARROW(s1,LvExpr).lVal;
      offset := NARROW(s0,LvExpr).lVal;
      count := LLVM.LLVMConstInt(IntPtrTy, VAL(n,LONGINT), TRUE);
      res := DoExtract(a,count,offset,sign);
    END;
    NARROW(s1,LvExpr).lVal := res;
    Pop(self.exprStack);
  END extract_n;

PROCEDURE extract_mn (self: U;  t: IType;  sign: BOOLEAN;  m, n: CARDINAL) =
  (* s0.t := Word.Extract(s0.t, m, n);
     IF sign THEN SignExtend s0 *)
  VAR
    s0 := Get(self.exprStack,0);
    a,offset,count,res : LLVM.ValueRef;
  BEGIN
    IF n = 0 THEN
      res := Zero(LLvmType(t));
    ELSE
      a := NARROW(s0,LvExpr).lVal;
      count := LLVM.LLVMConstInt(IntPtrTy, VAL(n,LONGINT), TRUE);
      offset := LLVM.LLVMConstInt(IntPtrTy, VAL(m,LONGINT), TRUE);
      res := DoExtract(a,count,offset,sign);
    END;
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
  CONST numParams = 5;
  VAR
    alignVal,volatile : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
  BEGIN
    paramsRef := NewValueArr(paramsArr,numParams);
    alignVal := LLVM.LLVMConstInt(LLVM.LLVMInt32Type(), VAL(align,LONGINT), TRUE);
    (* not sure about volatile do we ever use it? *)
    volatile := LLVM.LLVMConstInt(LLVM.LLVMInt1Type(), VAL(0,LONGINT), TRUE);
    src := LLVM.LLVMBuildBitCast(builderIR, src, AdrTy, LT("src_toi8"));
    dest := LLVM.LLVMBuildBitCast(builderIR, dest, AdrTy, LT("dest_toi8"));
    paramsArr[0] := dest;
    paramsArr[1] := src;
    paramsArr[2] := len;
    paramsArr[3] := alignVal;
    paramsArr[4] := volatile;

    IF overlap THEN
      EVAL LLVM.LLVMBuildCall(builderIR, MemMoveFn(), paramsRef, numParams, LT(""));
    ELSE
      EVAL LLVM.LLVMBuildCall(builderIR, MemCopyFn(), paramsRef, numParams, LT(""));
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
  CONST numParams = 5;
  VAR
    alignVal,volatile : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
  BEGIN
    paramsRef := NewValueArr(paramsArr,numParams);
    alignVal := LLVM.LLVMConstInt(LLVM.LLVMInt32Type(), VAL(align,LONGINT), TRUE);
    (* not sure about volatile do we ever use it? *)
    volatile := LLVM.LLVMConstInt(LLVM.LLVMInt1Type(), VAL(0,LONGINT), TRUE);
    dest := LLVM.LLVMBuildBitCast(builderIR, dest, AdrTy, LT("zmem_toadr"));
    paramsArr[0] := dest;
    paramsArr[1] := Zero(i8Type);
    paramsArr[2] := len;
    paramsArr[3] := alignVal;
    paramsArr[4] := volatile;
    EVAL LLVM.LLVMBuildCall(builderIR, MemSetFn(), paramsRef, numParams, LT(""));
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

PROCEDURE loophole (self: U;  from, two: ZType) =
  (* s0.two := LOOPHOLE(s0.from, two) *)
  VAR
    s0 := Get(self.exprStack,0);
    a,b : LLVM.ValueRef;
    destTy : LLVM.TypeRef;
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
    destTy := LLvmType(two);
(* fixme think there are other types that have not been handled
LLVM.LLVMDumpValue(a);
LLVM.LLVMDumpType(destTy);
*)
    IF from = Type.Addr THEN
      b := LLVM.LLVMBuildPtrToInt(builderIR, a, destTy, LT("loophole"));
    ELSIF two = Type.Addr THEN
      b := LLVM.LLVMBuildIntToPtr(builderIR, a, destTy, LT("loophole"));
    ELSE
      b := LLVM.LLVMBuildBitCast(builderIR, a, destTy, LT("loophole"));
    END;
    NARROW(s0,LvExpr).lVal := b;
  END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

<*NOWARN*> PROCEDURE abort (self: U;  code: RuntimeError) =
  (* generate a checked runtime error for "code" *)
  CONST numParams = 2;
  VAR
    lVal,codeVal,modVal : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
  BEGIN
    paramsRef := NewValueArr(paramsArr,numParams);
    codeVal := LLVM.LLVMConstInt(LLVM.LLVMInt64Type(), VAL(ORD(code),LONGINT), TRUE);
    modVal := LLVM.LLVMBuildBitCast(builderIR, faultVal, AdrTy, LT("fault_toadr"));
    paramsArr[0] := modVal;
    paramsArr[1] := codeVal;
    lVal := LLVM.LLVMBuildCall(builderIR, self.abortFunc, paramsRef, numParams, LT(""));
  END abort;

PROCEDURE DoCheck(self : U; a,b : LLVM.ValueRef; pred : LLVM.IntPredicate; code : RuntimeError) =
  VAR
    cmpVal,brVal : LLVM.ValueRef;
    curBB,errorBB,exitBB : LLVM.BasicBlockRef;
  BEGIN
    cmpVal := LLVM.LLVMBuildICmp(builderIR, pred, a, b, LT("checkcmp"));
    curBB := LLVM.LLVMGetInsertBlock(builderIR);
    errorBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT("abort_"));
    exitBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT("checkok_"));

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
    a := NARROW(s0,LvExpr).lVal;
    b := LLVM.LLVMConstNull(AdrTy); (* all zeroes *)
    a := LLVM.LLVMBuildBitCast(builderIR, a, AdrTy, LT("checknil_toadr"));
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

PROCEDURE check_hi (self: U;  t: IType;  READONLY i: Target.Int; code: RuntimeError) =
(* IF (i < s0.t) THEN abort(code) *)
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
    a,b,adrVal,gepVal : LLVM.ValueRef;
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
    b := LLVM.LLVMConstInt(LLVM.LLVMInt64Type(), VAL(i,LONGINT), TRUE);
    adrVal := LLVM.LLVMBuildBitCast(builderIR, a, AdrTy, LT("addOfs_toadr"));
    gepVal := Gep(adrVal,b,FALSE);
    NARROW(s0,LvExpr).lVal := gepVal;
  END add_offset;


<*NOWARN*> PROCEDURE index_address (self: U;  t: IType;  size: INTEGER) =
  (* s1.A := s1.A + s0.t * size; pop  -- where 'size' is in bytes *)
  VAR
    s0 := Get(self.exprStack,0);
    s1 := Get(self.exprStack,1);
    a,b,sizeVal,gepVal,mulVal,adrVal : LLVM.ValueRef;
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
    b := NARROW(s1,LvExpr).lVal;

    sizeVal := LLVM.LLVMConstInt(IntPtrTy, VAL(size,LONGINT), TRUE);
    mulVal := LLVM.LLVMBuildNSWMul(builderIR,a,sizeVal,LT("idxadr_mul"));

    adrVal := LLVM.LLVMBuildBitCast(builderIR, b, AdrTy, LT("idxadr_toadr"));
    gepVal := Gep(adrVal,mulVal,FALSE);

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
(* fixme *)
    proc := NARROW(p,LvProc);
    self.buildFunc(p);
    PopDecl(self);
  END start_call_direct;

PROCEDURE BuildStaticLink(self : U; proc : LvProc) : CARDINAL =
  VAR
    tp : LvProc;
    v : LvVar;
    varLv,storeLv : LLVM.ValueRef;
    linkSize,index : CARDINAL := 0;
  BEGIN
    tp := proc.parent;
    WHILE tp # NIL DO
      IF tp.linkStack # NIL THEN (*importeds are nil *)
        FOR i := 0 TO tp.linkStack.size() - 1 DO
          INC(linkSize);
          v := Get(tp.linkStack,i);
          IF v.inProc = self.curProc THEN
            varLv := LLVM.LLVMBuildBitCast(builderIR,v.lv,AdrTy,LT("ToAdr"));
          ELSE
            (* so have to get the lv from the link passed in *)
            varLv := FindLinkVar(self,v);
          END;
          storeLv := BuildGep(proc.staticLv,index * ptrBytes);
          storeLv := LLVM.LLVMBuildBitCast(builderIR,storeLv,AdrAdrTy,LT("ToAdrAdr"));
          EVAL LLVM.LLVMBuildStore(builderIR,varLv,storeLv);
          INC(index);
        END;
      END;
      tp := tp.parent;
    END;
IO.Put("linksize " & Fmt.Int(linkSize) & "\n");
    RETURN linkSize;
  END BuildStaticLink;

<*NOWARN*> PROCEDURE call_direct (self: U; p: Proc;  t: Type) =
  (* call the procedure 'p'.  It returns a value of type t. *)
  VAR
    proc : LvProc;
    fn,lVal,trampLv : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
    arg : LvExpr;
    returnName : TEXT := "";
    numParams,paramsCnt,stackParams,procParams,linkSize : INTEGER := 0;
    linkCount : [0..1]  := 0;
  BEGIN
    DumpExprStack(self,"call_direct");

    <*ASSERT self.callStack # NIL *>
    proc := NARROW(p,LvProc);
    fn := proc.lvProc;
    <*ASSERT fn # NIL *>
    procParams := proc.numParams;
    stackParams := self.callStack.size();
    numParams := stackParams;

(* this isnt always the case when there are nested procs and or try finally
    <*ASSERT stackParams = procParams *>
*)
    IF proc.lev > 0 THEN
      (* its possible the staticSize is zero in the case of no locals
         so the trampty is nil in which case we dont need a static link at all *)
      IF proc.trampTy # NIL THEN
        (* build the alloc for the static link *)
        IF proc.staticLv = NIL THEN
          trampLv := LLVM.LLVMBuildAlloca(builderIR, proc.trampTy, LT("_link"));
          proc.staticLv := LLVM.LLVMBuildBitCast(builderIR, trampLv, AdrTy, LT("_link_adr"));
        END;
        linkSize := BuildStaticLink(self,proc);
        IF linkSize > 0 THEN
          linkCount := 1; (* to correct the array size below *)
        END;
      ELSE
       (* if stackparms is 0 and procparms is 1 its a try-finally link
          so push an undef on the call stack *)
        IF procParams = 1 THEN
          lVal := LLVM.LLVMGetUndef(AdrTy);
          PushRev(self.callStack, NEW(LvExpr,lVal := lVal));
          numParams := procParams;
        END;
      END;
    END;

    (* create the param types from the callstack *)
    paramsCnt := numParams + linkCount;
    paramsRef := NewValueArr(paramsArr,paramsCnt);

    FOR i := 0 TO numParams - 1 DO
      arg := Get(self.callStack);
      (* possibly add call attributes here *)
      paramsArr[i] := arg.lVal;
      Pop(self.callStack);
    END;
    <*ASSERT self.callStack.size() = 0 *>

    IF linkCount > 0 THEN
      paramsArr[paramsCnt - 1] := proc.staticLv;
    END;
    
    IF proc.returnType # Type.Void THEN
      returnName := "result";
    END;
    (* else void returns need null string *)

    lVal := LLVM.LLVMBuildCall(builderIR, fn, paramsRef, paramsCnt, LT(returnName));

    IF proc.returnType # Type.Void THEN
      (* push the return val onto stack *)
      Push(self.exprStack,NEW(LvExpr,lVal := lVal));
    END;
  END call_direct;

<*NOWARN*> PROCEDURE start_call_indirect (self: U;  t: Type;  cc: CallingConvention) =
  (* begin an indirect procedure call that will return a value of type 't'. *)
  BEGIN
  (* nothing to do *)
  END start_call_indirect;

(* construct a procedure signature type for indirect calls *)
PROCEDURE FuncType(retType : Type; paramStack : RefSeq.T) : LLVM.TypeRef =
  VAR
    retTy,procTy : LLVM.TypeRef;
    numParams : INTEGER;
    param : LvExpr;
    typesArr : TypeArrType;
    typesRef : TypeRefType;
  BEGIN
    retTy := LLvmType(retType);
    numParams := paramStack.size();

    IF numParams > 0 THEN
      typesRef := NewTypeArr(typesArr,numParams);
      FOR i := 0 TO numParams - 1 DO
        param := Get(paramStack,i);
        typesArr[i] := LLVM.LLVMTypeOf(param.lVal);
      END;
    END;
    procTy := LLVM.LLVMFunctionType(retTy, typesRef, numParams, FALSE);
    RETURN procTy;
  END FuncType;

<*NOWARN*> PROCEDURE call_indirect (self: U;  t: Type;  cc: CallingConvention) =
  (* call the procedure whose address is in s0.A and pop s0.  The
     procedure returns a value of type t. *)
  VAR
    s0 := Get(self.exprStack);
    expr,arg : LvExpr;
    proc,callVal,resultVal : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
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
      paramsRef := NewValueArr(paramsArr,numParams);
      FOR i := 0 TO numParams - 1 DO
        arg := Get(self.callStack);
        paramsArr[i] := arg.lVal;
        Pop(self.callStack);
      END;
    END;
    <*ASSERT self.callStack.size() = 0 *>

    IF t # Type.Void THEN
      returnName := "result";
    END;

    (* need a pointer to function type for the call *)
    funcPtrTy := LLVM.LLVMPointerType(funcTy);
    callVal := LLVM.LLVMBuildBitCast(builderIR, proc, funcPtrTy, LT("call_ind"));
    resultVal := LLVM.LLVMBuildCall(builderIR, callVal, paramsRef, numParams, LT(returnName));

    (* discard the proc address *)
    Pop(self.exprStack);

    IF t # Type.Void THEN
      (* push the return val onto stack*)
      Push(self.exprStack,NEW(LvExpr,lVal := resultVal));
    END;
  END call_indirect;

PROCEDURE pop_param (self: U;  t: MType) =
  (* pop s0.t and make it the "next" parameter in the current call. *)
  VAR
    s0 := Get(self.exprStack,0);
    expr : LvExpr;
    destTy : LLVM.TypeRef;
  BEGIN
    expr := NARROW(s0,LvExpr);

    (* test if arg is ptr type then convert *)
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

PROCEDURE pop_static_link (self: U) =
  (* pop s0.A for the current indirect procedure call's static link  *)
  VAR
    s0 := Get(self.exprStack,0);
  BEGIN
(* fix me dont think we have ever tested this *)
    PushRev(self.callStack,s0);
    Pop(self.exprStack);
  END pop_static_link;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE load_procedure (self: U;  p: Proc) =
  (* push; s0.A := ADDR (p's body) *)
  VAR
    proc : LvProc;
    srcVal : LLVM.ValueRef;
  BEGIN
    proc := NARROW(p,LvProc);
    self.buildFunc(p);
    srcVal := proc.lvProc;
    (* convert to address *)
    srcVal:= LLVM.LLVMBuildBitCast(builderIR, srcVal, AdrTy, LT("loadproc_toadr"));

    Push(self.exprStack,NEW(LvExpr,lVal := srcVal));
  END load_procedure;

PROCEDURE load_static_link (self: U;  p: Proc) =
  (* push; s0.A := (static link needed to call p, NIL for top-level procs) *)
  VAR
    proc : LvProc;
    link : LLVM.ValueRef;
  BEGIN
    proc := NARROW(p,LvProc);
    link := proc.parent.lvProc;
    Push(self.exprStack,NEW(LvExpr,lVal := link));
  END load_static_link;

(*----------------------------------------------------------------- misc. ---*)

<*NOWARN*> PROCEDURE comment (self: U;  a, b, c, d: TEXT := NIL) =
(* annotate the output with a&b&c&d as a comment.  Note that any of a,b,c or d
   may be NIL. *)
  VAR s : TEXT := "";
  BEGIN
    IF a # NIL THEN s := s & a; END;
    IF b # NIL THEN s := s & b; END;
    IF c # NIL THEN s := s & c; END;
    IF d # NIL THEN s := s & d; END;
    (* debug *)
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
    a := NARROW(s0,LvExpr).lVal;
    b := NARROW(s1,LvExpr).lVal;

    ordering := GetOrder(order);
    atomicOp := LLVM.AtomicRMWBinOp.LLVMAtomicRMWBinOpXchg;

    lVal := LLVM.LLVMBuildAtomicRMW(builderIR, atomicOp, a, b, ordering, TRUE);
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

    lVal := LLVM.LLVMBuildAtomicCmpXchg(builderIR, ptr,cmp,new, successOrdering,
                                failureOrdering, TRUE);
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
    EVAL LLVM.LLVMBuildFence(builderIR, ordering, TRUE, LT("fence"));
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

    lVal := LLVM.LLVMBuildAtomicRMW(builderIR, atomicOp, a, b, ordering, TRUE);
    NARROW(s1,LvExpr).lVal := lVal;
    Pop(self.exprStack);
  END fetch_and_op;

(* Debug support - *)
(*
   This is very tentative. Maybe move this stuff to its own module
   Also it depends on the DiBuilder class which needs c api bindings which
   should be supplied with llvm but are not at present. There could be other
   ways of supporting debug information maybe via direct metadata calls.

  thus far simple scalar locals and params work
  enumerations, records and arrays seem to work with scalar types

  problems with debug so far
  set types dont work - not implemented type in dibuilder
  object types work except cant see base type info
  global variables dont work gdb says optimised out.
  boolean types only display 0 or 1
  subranges not working entirely
*)

PROCEDURE DebugInit(self: U) =
  CONST
   (* from Dwarf.h*)
    DW_LANG_Modula3 = 16_17;
    DW_LANG_C99 = 16_0C;
    DWARF_VERSION = 4;
    DWARF_INFO_VERSION = 1;
    lang = DW_LANG_Modula3;
  VAR
    mdNode : LLVM.ValueRef;
    valsMD : ARRAY[0..2] OF REFANY;
    identMD : ARRAY[0..0] OF REFANY;
    ident := "cm3 version 5.8"; (* fixme get it from front end *)
  BEGIN
    IF NOT self.genDebug THEN RETURN; END;

    (* should be able to use "." as dir and pathname.last as filename *)
    self.debDir := Pathname.Prefix(self.curFile);
    self.debFile := Pathname.Last(self.curFile);

    (* the dwarf version we generate
       these numbers can be gotten from dwarf.h in the enum at top
       should be able to import those into llvm.i3*)
    (* also this init is really messy *)
    valsMD[0] := NEW(REF INTEGER);
    NARROW(valsMD[0],REF INTEGER)^ := 2;
    valsMD[1] := "Dwarf Version";
    valsMD[2] := NEW(REF INTEGER);
    NARROW(valsMD[2],REF INTEGER)^ := DWARF_VERSION;

    mdNode := GetMDNode(valsMD);
    LLVM.LLVMAddNamedMetadataOperand(modRef, LT("llvm.module.flags"), mdNode);
    valsMD[1] := "Debug Info Version";
    NARROW(valsMD[2],REF INTEGER)^ := DWARF_INFO_VERSION;
    mdNode := GetMDNode(valsMD);
    LLVM.LLVMAddNamedMetadataOperand(modRef, LT("llvm.module.flags"), mdNode);

    identMD[0] := ident;
    mdNode := GetMDNode(identMD);
    LLVM.LLVMAddNamedMetadataOperand(modRef, LT("llvm.ident"), mdNode);

    self.debugRef := LLVM.LLVMNewDIBuilder(modRef);

    self.cuRef := LLVM.LLVMDIBuilderCreateCompileUnit(
                self.debugRef,
                lang,
                LT(self.debFile),
                LT(self.debDir),
                LT("cm3"), (* Producer *)
                FALSE, (* Optimized - can we check *)
                LT(""), (* Flags *)
                0 (*RuntimeVersion*) );

    self.fileRef := LLVM.LLVMDIBuilderCreateFile(self.debugRef, LT(self.debFile), LT(self.debDir));

    InitUids(self);
  END DebugInit;

PROCEDURE InitUids(self : U) =
  BEGIN
    (* create the basic tUid entries - needs testing *)

    EVAL self.debugTable.put(UID_INTEGER,NEW(BaseDebug, bitSize := ptrBits, align := ptrBits, typeName := M3ID.Add("INTEGER"), encoding := DW_ATE_signed));
    EVAL self.debugTable.put(UID_LONGINT,NEW(BaseDebug, bitSize := ptrBits, align := ptrBits, typeName := M3ID.Add("LONGINT"), encoding := DW_ATE_signed));
    EVAL self.debugTable.put(UID_WORD,NEW(BaseDebug, bitSize := ptrBits, align := ptrBits, typeName := M3ID.Add("WORD"), encoding := DW_ATE_unsigned));
    EVAL self.debugTable.put(UID_LONGWORD,NEW(BaseDebug, bitSize := ptrBits, align := ptrBits, typeName := M3ID.Add("LONGWORD"), encoding := DW_ATE_unsigned));
    EVAL self.debugTable.put(UID_REEL,NEW(BaseDebug, bitSize := 32L, align := 32L, typeName := M3ID.Add("REAL"), encoding := DW_ATE_float));
    EVAL self.debugTable.put(UID_LREEL,NEW(BaseDebug, bitSize := 64L, align := 64L, typeName := M3ID.Add("LONGREAL"), encoding := DW_ATE_float));
    EVAL self.debugTable.put(UID_XREEL,NEW(BaseDebug, bitSize := 80L, align := 80L, typeName := M3ID.Add("EXTENDED"), encoding := DW_ATE_float));
    EVAL self.debugTable.put(UID_BOOLEAN,NEW(BaseDebug, bitSize := 8L, align := 8L,
    typeName := M3ID.Add("BOOLEAN"), encoding := DW_ATE_unsigned_char));
    EVAL self.debugTable.put(UID_CHAR,NEW(BaseDebug, bitSize := 8L, align := 8L, typeName := M3ID.Add("CHAR"), encoding := DW_ATE_unsigned_char));
    EVAL self.debugTable.put(UID_WIDECHAR,NEW(BaseDebug, bitSize := 16L, align := 16L, typeName := M3ID.Add("WIDECHAR"), encoding := DW_ATE_signed_char));

    EVAL self.debugTable.put(UID_ROOT,NEW(BaseDebug, bitSize := ptrBits, align := ptrBits, typeName := M3ID.Add("ROOT"), encoding := DW_ATE_address));
    EVAL self.debugTable.put(UID_UNTRACED_ROOT,NEW(BaseDebug, bitSize := ptrBits, align := ptrBits, typeName := M3ID.Add("UNTRACED_ROOT"), encoding := DW_ATE_address));
    EVAL self.debugTable.put(UID_ADDR,NEW(BaseDebug, bitSize := ptrBits, align := ptrBits, typeName := M3ID.Add("ADDR"), encoding := DW_ATE_address));

    EVAL self.debugTable.put(UID_TEXT,NEW(BaseDebug, bitSize := ptrBits, align := ptrBits, typeName := M3ID.Add("TEXT"), encoding := DW_ATE_address));
    EVAL self.debugTable.put(UID_REFANY,NEW(BaseDebug, bitSize := ptrBits, align := ptrBits, typeName := M3ID.Add("REFANY"), encoding := DW_ATE_address));
    EVAL self.debugTable.put(UID_MUTEX,NEW(BaseDebug, bitSize := ptrBits, align := ptrBits, typeName := M3ID.Add("MUTEX"), encoding := DW_ATE_address));

    EVAL self.debugTable.put(UID_RANGE_0_31,NEW(BaseDebug, bitSize := 32L, align := 32L, typeName := M3ID.Add("RANGE_0_31"), encoding := DW_ATE_signed));
    EVAL self.debugTable.put(UID_RANGE_0_63,NEW(BaseDebug, bitSize := 64L, align := 64L, typeName := M3ID.Add("RANGE_0_63"), encoding := DW_ATE_signed));

    EVAL self.debugTable.put(UID_NULL,NEW(BaseDebug, bitSize := ptrBits, align := ptrBits, typeName := M3ID.Add("NULL"), encoding := DW_ATE_address));
(*
  NO_UID ??
*)
  END InitUids;

PROCEDURE DebugFinalise(self : U) =
  BEGIN
    IF self.genDebug THEN
      LLVM.LLVMDIBuilderFinalize(self.debugRef);
    END;
  END DebugFinalise;

PROCEDURE DebugLine(self : U) =
  VAR
    loc : LLVM.ValueRef;
  BEGIN
    IF self.genDebug THEN
      IF self.funcRef # NIL THEN
        loc := LLVM.LLVMGetDebugLoc(self.curLine,0,self.funcRef);
        LLVM.LLVMSetCurrentDebugLocation(builderIR, loc);
      END;
    END;
  END DebugLine;

PROCEDURE DebugClearLoc(self : U) =
  BEGIN
    IF self.genDebug THEN
      LLVM.LLVMSetCurrentDebugLocation(builderIR, NIL);
    END;
  END DebugClearLoc;

PROCEDURE DebugFunc(self : U; p : Proc) =
  VAR
    proc : LvProc;
    param : LvVar;
    funcTy,tyVal,paramTypes : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
    arg : REFANY;
    numParams,tSize,tAlign : CARDINAL;
    procName,typeName : TEXT;
  BEGIN
    IF NOT self.genDebug THEN RETURN; END;

    proc := NARROW(p,LvProc);
    numParams := proc.numParams;

    (* extra one since return type is first param *)
    paramsRef := NewValueArr(paramsArr,numParams + 1);
    IF proc.returnType = Type.Void THEN
      tyVal := NIL;
    ELSE
      tSize := TypeSize(proc.returnType) * ptrBytes;
      tAlign := tSize;
      tyVal := LLVM.LLVMDIBuilderCreateBasicType(self.debugRef, LT("return_t"),           VAL(tSize,LONGINT), VAL(tAlign,LONGINT), 0);
    END;
    paramsArr[0] := tyVal;

    IF numParams > 0 THEN
      FOR i := 0 TO numParams - 1 DO
        arg := Get(proc.paramStack,i);
        param := NARROW(arg,LvVar);
        typeName := M3ID.ToText(param.name) & "_t";
        tSize := param.size * ptrBytes;
        tAlign := param.align * ptrBytes;
        tyVal := LLVM.LLVMDIBuilderCreateBasicType(self.debugRef, LT(typeName),             VAL(tSize,LONGINT), VAL(tAlign,LONGINT), 0);
        paramsArr[i+1] := tyVal;
      END;
    END;

    paramTypes := LLVM.LLVMDIBuilderGetOrCreateTypeArray(self.debugRef, paramsRef, numParams + 1);
    funcTy := LLVM.LLVMDIBuilderCreateSubroutineType(self.debugRef,self.fileRef, paramTypes);
    procName := M3ID.ToText(proc.name);

    self.funcRef := LLVM.LLVMDIBuilderCreateFunction(self.debugRef,
       self.cuRef,     (* Scope - problem with nested ??*)
       LT(procName),   (* Name *)
       LT(""),         (* LinkageName *)
       self.fileRef,   (* File *)
       self.curLine,   (* LineNo *)
       funcTy,         (* CompositeType *)
       FALSE,          (* IsLocaltoUnit *)
       TRUE,           (* IsDefinition *)
       self.curLine,   (* ScopeLine ?? maybe 0 *)
       0,              (* flags *)
       FALSE,          (* isOptimized *)
       proc.lvProc);   (* Func *)

END DebugFunc;

PROCEDURE DebugSubrangeLookup(self : U; tUid : TypeUID) : SubrangeDebug =
  VAR
    debugObj : REFANY;
    tidExists : BOOLEAN;
  BEGIN
    tidExists := self.debugTable.get(tUid,debugObj);
    IF tidExists THEN
      RETURN NARROW(debugObj,SubrangeDebug);
    ELSE
      RETURN NIL;
    END;
  END DebugSubrangeLookup;

PROCEDURE DebugSubrange(self : U; s : SubrangeDebug) : LLVM.ValueRef =
  BEGIN
IO.Put("subrange debug\n");

    RETURN LLVM.LLVMDIBuilderGetOrCreateSubrange(self.debugRef, s.min, s.max - s.min + 1L);
  END DebugSubrange;

PROCEDURE DebugArray(self : U; a : ArrayDebug) : LLVM.ValueRef =
  VAR
    eltVal,subsVal,paramTypes : LLVM.ValueRef;
    subrange : SubrangeDebug;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
  BEGIN
IO.Put("array debug\n");

    eltVal := DebugLookup(self,a.elt);
    subrange := DebugSubrangeLookup(self,a.index);
    subsVal := LLVM.LLVMDIBuilderGetOrCreateSubrange(self.debugRef, subrange.min, subrange.max - subrange.min + 1L);
    paramsRef := NewValueArr(paramsArr,1);
    paramsArr[0] := subsVal;
    paramTypes := LLVM.LLVMDIBuilderGetOrCreateArray(self.debugRef, paramsRef, 1);
    RETURN LLVM.LLVMDIBuilderCreateArrayType(self.debugRef, a.bitSize, a.align, eltVal, paramTypes);
  END DebugArray;

PROCEDURE DebugOpenArray(self : U; a : OpenArrayDebug) : LLVM.ValueRef =
  VAR
    eltVal,subsVal,paramTypes : LLVM.ValueRef;
(*    subrange : SubrangeDebug;*)
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
  BEGIN
IO.Put("openarray debug\n");

    eltVal := DebugLookup(self,a.elt);
    (* open arrays dont have a range so just fake it for now 0 - last(val)
        fix this later
    subrange := DebugSubrangeLookup(self,a.index);
    *)
    subsVal := LLVM.LLVMDIBuilderGetOrCreateSubrange(self.debugRef, 0L, 100L);
    paramsRef := NewValueArr(paramsArr,1);
    paramsArr[0] := subsVal;
    paramTypes := LLVM.LLVMDIBuilderGetOrCreateArray(self.debugRef, paramsRef, 1);
    RETURN LLVM.LLVMDIBuilderCreateArrayType(self.debugRef, a.bitSize, a.align, eltVal, paramTypes);
  END DebugOpenArray;

PROCEDURE DebugSet(self : U; s : SetDebug) : LLVM.ValueRef =
  VAR
    eltVal,subsVal,paramTypes : LLVM.ValueRef;
    subrange : SubrangeDebug;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
  BEGIN
IO.Put("set debug\n");

    eltVal := LLVM.LLVMDIBuilderCreateBasicType(self.debugRef,LT("basic_type" ), 1L, 8L, DW_ATE_unsigned_char);

    (*eltVal := DebugLookup(self,d.elt);*)
    (*
    subsVal := DebugLookup(self,d.domain);
    paramsRef := NewValueArr(paramsArr,1);
    paramsArr[0] := subsVal;
    paramTypes := LLVM.LLVMDIBuilderGetOrCreateArray(self.debugRef, paramsRef, 1);
    *)
    subrange := DebugSubrangeLookup(self,s.domain);
    (*
    subsCount := VAL(subrange.max - subrange.min + 1L,INTEGER);
    *)
    subsVal := LLVM.LLVMDIBuilderGetOrCreateSubrange(self.debugRef, subrange.min, subrange.max - subrange.min + 1L);

    paramsRef := NewValueArr(paramsArr,1);
    paramsArr[0] := subsVal;
    paramTypes := LLVM.LLVMDIBuilderGetOrCreateArray(self.debugRef, paramsRef, 1);

    RETURN LLVM.LLVMDIBuilderCreateArrayType(self.debugRef, s.bitSize, s.align, eltVal, paramTypes );

(* attempt with set type - this creates the new metadata tag for sets
  and generates proper metadata i think but llc complains the tag is not
  implemented, think the DW set tag has to be implemented in llvm
  Also you should be able to implement this with packed array of bits
  havent tried that yet. *)
(*
    eltVal := LLVM.LLVMDIBuilderCreateBasicType(self.debugRef,LT("basic_type" ), 1L, 8L, DW_ATE_unsigned_char);

    lVal := LLVM.LLVMDIBuilderCreateSetType(
             self.debugRef,
             eltVal,
             d.bitSize,
             d.align,
             LT(M3ID.ToText(d.typeName)));
*)
  END DebugSet;

PROCEDURE DebugEnum(self : U; e : EnumDebug) : LLVM.ValueRef =
  VAR
    eltVal,paramTypes : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
  BEGIN
IO.Put("enum debug\n");

    paramsRef := NewValueArr(paramsArr,e.numElts);
    FOR i := 0 TO e.numElts - 1 DO
      eltVal := LLVM.LLVMDIBuilderCreateEnumerator(self.debugRef,LT(M3ID.ToText(e.elts[i])),i);
      paramsArr[i] := eltVal;
    END;
    paramTypes := LLVM.LLVMDIBuilderGetOrCreateArray(self.debugRef, paramsRef, e.numElts);

    RETURN LLVM.LLVMDIBuilderCreateEnumerationType(
               self.debugRef,
               self.funcRef,
               LT(M3ID.ToText(e.typeName)),
               self.fileRef,
               self.curLine,
               e.bitSize,
               e.align,
               paramTypes,
               NIL);
  END DebugEnum;

PROCEDURE DebugPacked(self : U; p : PackedDebug) : LLVM.ValueRef =
  BEGIN
  (* fixme *)
IO.Put("packed debug\n");
    RETURN NIL;
END DebugPacked;

PROCEDURE DebugOpaque(self : U; o : OpaqueDebug) : LLVM.ValueRef =
  BEGIN
  (* fixme *)
IO.Put("opaque debug\n");
    RETURN NIL;
END DebugOpaque;

PROCEDURE DebugProcType(self : U; p : ProcTypeDebug) : LLVM.ValueRef =
(*  VAR
    eltVal,paramTypes : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
    *)
  BEGIN
  (* fixme *)
IO.Put("proctype debug\n");
    RETURN NIL;
END DebugProcType;

PROCEDURE DebugObject(self : U; o : ObjectDebug) : LLVM.ValueRef =
  VAR
    eltVal,memberVal,cVal,derivedVal,inheritVal,forwardRef,paramTypes : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;

    (* test finding the root obj *)
    paramCount : CARDINAL;
    tidExists,isSub : BOOLEAN;
    debugObj : REFANY;
    baseObj : BaseDebug;
  BEGIN
IO.Put("object debug\n");

    tidExists := self.debugTable.get(o.superType,debugObj);
    baseObj := NARROW(debugObj,BaseDebug);
    paramCount := 0;
    isSub := FALSE;
    paramsRef := NewValueArr(paramsArr,o.numFields+1);

    IF NOT Text.Equal(M3ID.ToText(baseObj.typeName), "ROOT") THEN
      isSub := TRUE;
      derivedVal := DebugLookup(self,o.superType);

    (*
     build a forward type decl for the cval then create an inherit using the
     derived val and the cval and put it as first param
  DW_TAG_class_type = 0x02, for forward ref below
    *)
      forwardRef := LLVM.LLVMDIBuilderCreateReplaceableForwardDecl(
                    self.debugRef,
                    2,
                    LT("forward1"),
                    self.funcRef,
                    self.fileRef,
                    self.curLine,
                    0,
                    0L,
                    0L,
                    LT(M3ID.ToText(o.typeName)));

      inheritVal := LLVM.LLVMDIBuilderCreateInheritance(self.debugRef,
                      forwardRef, (* cVal, *)(* Ty;*)
                      derivedVal, (*BaseTy;*)
                      0L, (*BaseOffset,*)
                      0);


      paramsArr[0] := inheritVal;
      INC(paramCount);
    END;

    FOR i := 0 TO o.numFields - 1 DO
      eltVal := DebugLookup(self,o.fields[i].type);
      memberVal := LLVM.LLVMDIBuilderCreateMemberType(
                     self.debugRef,
                    forwardRef,
                    (* self.funcRef,*) (* think this is obj we are part of as str *)
                     LT(M3ID.ToText(o.fields[i].name)),
                     self.fileRef,
                     self.curLine,
                     o.fields[i].bitSize,
                     o.fields[i].align,
                     o.fields[i].bitOffset + 64L, (* for vtable ptr *)
                     0,
                     eltVal);
      paramsArr[paramCount] := memberVal;
      INC(paramCount);
    END;
    paramTypes := LLVM.LLVMDIBuilderGetOrCreateArray(self.debugRef, paramsRef, paramCount);

    cVal := LLVM.LLVMDIBuilderCreateClassType(
              self.debugRef,
              NIL, (*self.funcRef, *)
              LT(M3ID.ToText(o.typeName)),
              self.fileRef,
              self.curLine,
              o.fieldSize + 64L, (* for vtable ptr *)
              o.align,
              0L, (*ptrBits, (* offset *) *)
              0,
              NIL, (*derivedVal,*)
              paramTypes,
              NIL,
              NIL,
              LT("someid"));

(* not working *)
IF isSub THEN

LLVM.LLVMReplaceAllUsesWith(forwardRef,cVal);

END;

    RETURN LLVM.LLVMDIBuilderCreatePointerType(
              self.debugRef,
              cVal,
              ptrBits,
              ptrBits,
              LT(M3ID.ToText(o.typeName)));
  END DebugObject;

PROCEDURE DebugRecord(self : U; r : RecordDebug) : LLVM.ValueRef =
  VAR
    eltVal,memberVal,paramTypes : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
  BEGIN
IO.Put("record debug\n");

    paramsRef := NewValueArr(paramsArr,r.numFields);
    FOR i := 0 TO r.numFields - 1 DO
      eltVal := DebugLookup(self,r.fields[i].type);
      memberVal := LLVM.LLVMDIBuilderCreateMemberType(
                     self.debugRef,
                     self.funcRef,
                     LT(M3ID.ToText(r.fields[i].name)),
                     self.fileRef,
                     self.curLine,
                     r.fields[i].bitSize,
                     r.fields[i].align,
                     r.fields[i].bitOffset,
                     0,
                     eltVal);

      paramsArr[i] := memberVal;
    END;
    paramTypes := LLVM.LLVMDIBuilderGetOrCreateArray(self.debugRef, paramsRef, r.numFields);

    RETURN LLVM.LLVMDIBuilderCreateStructType(
               self.debugRef,
               self.funcRef,
               LT(M3ID.ToText(r.typeName)),
               self.fileRef,
               self.curLine,
               r.bitSize,
               r.align,
               0,
               NIL,
               paramTypes);
  END DebugRecord;

PROCEDURE DebugPointer(self : U; p : PointerDebug) : LLVM.ValueRef =
  VAR
    eltVal : LLVM.ValueRef;
  BEGIN
IO.Put("pointer debug\n");

    eltVal := DebugLookup(self,p.target);
    RETURN LLVM.LLVMDIBuilderCreatePointerType(
              self.debugRef,
               eltVal,
               p.bitSize,
               p.align,
               LT(M3ID.ToText(p.typeName)));
  END DebugPointer;

PROCEDURE DebugLookup(self : U; tUid : TypeUID) : LLVM.ValueRef =
  VAR
    debugObj : REFANY;
    tidExists : BOOLEAN;
  BEGIN
    IO.Put("tid>>"); IO.PutInt(tUid); IO.Put("<< ");
    (* exceptions have 0 tUid *)
    IF tUid = 0 THEN RETURN NIL; END;

    tidExists := self.debugTable.get(tUid,debugObj);
    IF NOT tidExists THEN
IO.Put("tid does not exist\n");
      <*ASSERT FALSE*>
    ELSE

      TYPECASE debugObj OF
      | ArrayDebug(d) => RETURN DebugArray(self,d);
      | OpenArrayDebug(d) => RETURN DebugOpenArray(self,d);
      | SetDebug(d) =>  RETURN DebugSet(self,d);
      | EnumDebug(d) => RETURN DebugEnum(self,d);
      | OpaqueDebug(d) => RETURN DebugOpaque(self,d);
      | PackedDebug(d) => RETURN DebugPacked(self,d);
      | ObjectDebug(d) => RETURN DebugObject(self,d);
      | RecordDebug(d) => RETURN DebugRecord(self,d);
      | PointerDebug(d) => RETURN DebugPointer(self,d);
      | ProcTypeDebug(d) => RETURN DebugProcType(self,d);
      | IndirectDebug(d) => RETURN DebugLookup(self,d.target);
      | SubrangeDebug(d) => RETURN (*DebugSubrange(self,d);*) DebugLookup(self,d.domain);
      | BaseDebug(d) =>
         IO.Put("base debug\n");
         RETURN LLVM.LLVMDIBuilderCreateBasicType(self.debugRef,LT(M3ID.ToText(d.typeName)), d.bitSize, d.align, d.encoding);
      ELSE
      <*ASSERT FALSE*>
      END;
    END;
  END DebugLookup;

PROCEDURE DebugVar(self : U; v : LvVar; argNum : CARDINAL := 0) =
  VAR
    tyVal,decl,loc,lvDebug : LLVM.ValueRef;
    tag,flags : CARDINAL := 0;
    name : TEXT;
  BEGIN
    IF NOT self.genDebug THEN RETURN; END;

    name := VName(v,TRUE);
    (* Dont debug temps and the _result *)
    IF v.name = M3ID.NoID OR Text.Compare(name,"_result") = 0 THEN RETURN; END;

    IF v.varType = VarType.Param THEN
      tag := DW_TAG_arg_variable;
    ELSE
      tag := DW_TAG_auto_variable;
    END;

    tyVal := DebugLookup(self,v.m3t);

    lvDebug := LLVM.LLVMDIBuilderCreateLocalVariable(self.debugRef, tag, self.funcRef, LT(name), self.fileRef, self.curLine, tyVal, FALSE, flags, argNum);

    (* we need this since setinstdebuglocation has to have a current loc *)
    loc := LLVM.LLVMGetDebugLoc(self.curLine, 0, self.funcRef);
    LLVM.LLVMSetCurrentDebugLocation(builderIR, loc);

    decl := LLVM.LLVMDIBuilderInsertDeclareAtEnd(self.debugRef, v.lv,lvDebug,LLVM.LLVMGetInsertBlock(builderIR));

    LLVM.LLVMSetInstDebugLocation(builderIR, decl);
  END DebugVar;

PROCEDURE DebugLocalsParams(self : U; proc : LvProc) =
  VAR
    local,param : LvVar;
    numParams,numLocals : CARDINAL;
    arg : REFANY;
    loc : LLVM.ValueRef;
  BEGIN
    IF NOT self.genDebug THEN RETURN; END;

    numParams := proc.numParams;
    FOR i := 0 TO numParams - 1 DO
      arg := Get(proc.paramStack,i);
      param := NARROW(arg,LvVar);
      DebugVar(self, param, i + 1);
    END;
    numLocals := proc.localStack.size();
    FOR i := 0 TO numLocals - 1 DO
      arg := Get(proc.localStack,i);
      local := NARROW(arg,LvVar);
      DebugVar(self, local);
    END;

    loc := LLVM.LLVMGetDebugLoc(self.curLine,0,self.funcRef);
    LLVM.LLVMSetCurrentDebugLocation(builderIR, loc);
  END DebugLocalsParams;

PROCEDURE DebugGlobals(self : U; var : LvVar) =
  VAR
    iter : IntRefTbl.Iterator;
    key : INTEGER;
    debugObj : REFANY;
    glob : RecordDebug;
    ty,gep,lVal,globAlias : LLVM.ValueRef;
    tref,fltPtrTy : LLVM.TypeRef;
  BEGIN
(* not working at all - seems to create the correct metadata but gdb
says its optimized out *)

    IF NOT self.genDebug THEN RETURN; END;

    iter := self.debugTable.iterate();
    WHILE iter.next(key, debugObj) DO
      IF ISTYPE(debugObj,RecordDebug) THEN
        glob := NARROW(debugObj,RecordDebug);
        IF glob.global THEN

(*
the global segment could be created first as a struct with members
possibly then create aliases to the members and they are the globals
*)
          ty := DebugRecord(self,glob);

          lVal := LLVM.LLVMDIBuilderCreateGlobalVariable(
                      self.debugRef,
                      LT(M3ID.ToText(var.name)),
                      self.fileRef,
                      0,
                      ty,
                      TRUE,
                      var.lv);

          FOR i := 0 TO glob.numFields - 1 DO
(*
            IO.Put(M3ID.ToText(glob.fields[i].name) & "\n");
*)
            (* need proper type not just i8* *)
            gep := BuildGep(var.lv,VAL(glob.fields[i].bitOffset,INTEGER) DIV 8);

(* aliases dont seem to work
           tref := LLVM.LLVMTypeOf(gep);
           globAlias := LLVM.LLVMAddAlias(modRef, tref, gep, LT(M3ID.ToText(glob.fields[i].name)));
*)

            ty := DebugLookup(self,glob.fields[i].type);
            lVal := LLVM.LLVMDIBuilderCreateGlobalVariable(
                      self.debugRef,
                      LT(M3ID.ToText(glob.fields[i].name)),
                      self.fileRef,
                      0, (*self.curLine,*)
                      ty,
                      TRUE,
                      gep (*globAlias*));
          END;
        END;
      END;
    END;
  END DebugGlobals;

(* Metadata support *)

PROCEDURE GetMDNode(READONLY refs : ARRAY OF REFANY) : LLVM.ValueRef =
  VAR
    mdNode,lVal : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
    numRefs : CARDINAL;
  BEGIN
    numRefs := NUMBER(refs);
    paramsRef := NewValueArr(paramsArr,numRefs);

    FOR i := 0 TO numRefs - 1 DO
      TYPECASE refs[i] OF
      |  REF INTEGER(x) =>
           lVal :=  LLVM.LLVMConstInt(IntPtrTy, VAL(x^,LONGINT), TRUE);
      |  REF REAL(x) =>
           lVal := LLVM.LLVMConstReal(LLVM.LLVMFloatType(), FLOAT(x^,LONGREAL));
      |  TEXT(x) =>
           lVal := LLVM.LLVMMDString(LT(x),Text.Length(x));
      ELSE
        <*ASSERT FALSE *>
      END;
      paramsArr[i] := lVal;
    END;
    mdNode := LLVM.LLVMMDNode(paramsRef,numRefs);
    RETURN mdNode;
  END GetMDNode;

PROCEDURE AddInstrMD(name,val : TEXT) =
  VAR
    mdNode,strVal,lastInstr,md : LLVM.ValueRef;
    curBB : LLVM.BasicBlockRef;
    kindId : CARDINAL;
  BEGIN
    curBB := LLVM.LLVMGetInsertBlock(builderIR);
    lastInstr := LLVM.LLVMGetLastInstruction(curBB);

    kindId := LLVM.LLVMGetMDKindID(LT(name),Text.Length(name));
    strVal := LLVM.LLVMMDString(LT(val),Text.Length(val));

    md := LLVM.LLVMGetMetadata(lastInstr,kindId);
    <*ASSERT md # NIL*>
    LLVM.LLVMSetMetadata(lastInstr, kindId, mdNode);
  END AddInstrMD;

BEGIN
END M3CG_LLVM.
