(*      Add copyright notice      *)

UNSAFE MODULE M3CG_LLVM;

(* NOTE: This module contains code that passes parameters of type
         LONGINT to C/C++ code that declares them as unsigned long long,
         across unchecked bindings.  Check that this is right. *) 

IMPORT Ctypes;
IMPORT Fmt;
IMPORT IntRefTbl;
IMPORT IO; (* debug this module *)
IMPORT M3toC;
IMPORT Pathname;
IMPORT RefSeq;
IMPORT Text;
IMPORT Word;
IMPORT Wr;

IMPORT DwarfConst AS DC; 
IMPORT LLVM;
IMPORT LLVMTypes; 
FROM LLVMTypes IMPORT int64_t , uint64_t , unsigned;
FROM LLVMTypes IMPORT Bool , False , True;   
FROM LLVMTypes IMPORT ArrayRefOfMetadataRef, MDNodeRef, MetadataRef, StringRef;
IMPORT M3Buf;
IMPORT M3CG;
FROM M3CG IMPORT Name, ByteOffset, TypeUID, CallingConvention;
FROM M3CG IMPORT BitSize, ByteSize, Alignment, Frequency;
FROM M3CG IMPORT Var, Proc, Label, Sign, BitOffset;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;
FROM M3CG IMPORT CompareOp, ConvertOp, AtomicOp, RuntimeError;
FROM M3CG IMPORT MemoryOrder;
IMPORT M3CG_Ops;
IMPORT M3DIBuilder AS M3DIB;
IMPORT M3ID;
IMPORT Target;
IMPORT TargetMap;
IMPORT TFloat;
IMPORT TInt;

(* Pervasive ASSUMPTION: Modula-3 NIL = C++ null pointer. *) 

TYPE INT32 = Ctypes.int; 

TYPE callStateTyp 
  = { outside,         (* Not in a call. *) 
      insideDirect,    (* Inside a call_direct sequence. *) 
      insideIndirect,  (* Inside a call_indirect sequence, but 
                          before any pop_static_link. *) 
      indirectAfterSL  (* Need I belabor this one? *) 
    }; 
CONST callStateSetIndirect 
  = SET OF callStateTyp 
      { callStateTyp.insideIndirect, callStateTyp.indirectAfterSL }; 

REVEAL

  U = Public BRANDED "M3CG_LLVM.T" OBJECT
    wr            : Wr.T := NIL;
    buf           : M3Buf.T := NIL;
    buf_len       : INTEGER := 0;

    exprStack     : RefSeq.T := NIL;
    callStack     : RefSeq.T := NIL;
    curVar        : LvVar;
    curProc       : LvProc; (* Procedure whose body we are in. *) 
    curParamOwner : LvProc; (* Most recent signature introducer, either
                               declare_procedure or import_procedure. *) 
    curLocalOwner : LvProc; (* Most recent procedure, introduced either 
                               by declare_procedure or begin_procedure. *)
    abortFunc     : LLVM.ValueRef;

    procStack     : RefSeq.T := NIL;

    labelTable    : IntRefTbl.T := NIL;
    structTable   : IntRefTbl.T := NIL;

    next_label_id := 1;
    next_var      := 1;
    next_proc     := 1;
    next_scope    := 1;
    blockLevel    := 0;
    widecharSize  := 16; (* May change to 32. *) 
    optLevel      := 0; (* optimize level - not used yet *)

    allocaName    : Name;
      (* "alloca", for detecting CG-generated library call on it. *) 

    (* State for generating calls. *) 
    callState     : callStateTyp; 
    callResultType : Type; (* Meaningful when inside a call. *) 
    indirCallCC    : CallingConvention; (* Meaningful when inside an indirect call. *)
    staticLinkBB   : LLVM.BasicBlockRef; (* BB that contains a pop_static_link. *)

   (* external set functions *)
    setUnion, setIntersection, setDifference, setSymDifference,
    setMember, setEq, setNe, setLe, setLt, setGe, setGt,
    setRange, setSingleton : LLVM.ValueRef;

    (* Debugging m3llvm: *) 
    m3llvmDebugLev : m3llvmDebugLevTyp;

    (* Generating debug output in the code being compiled. *)
    genDebug      := FALSE;
    curFile       := "";
    curLine       := 0;
    debFile       := "";
    debDir        := "";
    debugTable    : IntRefTbl.T := NIL;
    debugLexStack : RefSeq.T := NIL;
    debugRef      : M3DIB.DIBuilderRef; 
    cuRef         : M3DIB.LLVMDICompileUnit;
    fileRef       : M3DIB.LLVMDIFile;
    funcRef       : M3DIB.LLVMDISubprogram;
    segConst      : LvVar;
    segVar        : LvVar;
    seenConst     := FALSE;
    debugObj      : ROOT;
METHODS
    allocVar(v : LvVar) := AllocVar;
    allocVarInEntryBlock(v : LvVar) := AllocVarInEntryBlock;
    buildFunc(p : Proc) := BuildFunc;
    getLabel(lab : Label; name : TEXT) : LabelObj := GetLabel;
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
    align : Alignment;
    varType : VarType;
    m3t : TypeUID;
    frequency : Frequency;
    inProc : LvProc;  (* for static link *)
    lvType : LLVM.TypeRef;
    lv : LLVM.ValueRef;  (* llvm var definition *)
    lvSsa : LLVM.ValueRef;
      (* ^For a parameter, the llvm parameter SSA variable.  Needed? *)
    locDisplayIndex : INTEGER := -1;
    (* ^Index within the display provided by containing proc.  Nonnegative 
       only if labelled by front end as up_level. *)  
    inits : RefSeq.T;
    isConst : BOOLEAN; (* As of 2017-07-23, set but not used. *) 
    in_memory : BOOLEAN;
    up_level : BOOLEAN; (* Maintained but not used. *) 
    exported : BOOLEAN;
    inited : BOOLEAN := FALSE;
  END;

  procState = {uninit, decld, built, begun, complete};
  (* ^Used to assert some assumptions about the order things occur in CG IR. *)

  LvProc = Proc OBJECT
    tag: INTEGER;
    name : Name;
    state : procState := procState.uninit; 
    returnType : Type;
    numParams : CARDINAL;
    lev : INTEGER;
    cc : CallingConvention;
    exported : BOOLEAN := FALSE;
    lvProc : LLVM.ValueRef;  (* llvm procedure definition *)
    procTy : LLVM.TypeRef;
    parent : LvProc := NIL;
    entryBB : LLVM.BasicBlockRef; 
    (* ^For stored static link, params, vars, temps, display construction. *) 
    secondBB : LLVM.BasicBlockRef; 
    (* ^For other M3-coded stuff.  There are two separate basic blocks
       here, because we need to be able to intersperse adding things at the
       ends of each.  This seems easier than shuffling insertion points in
       one BB.  secondBB is the unconditional successor of entryBB. *)
    (* NOTE: It is hard to tell from the header files, but apparently, there
             is only one insertion point globally, not one per BB. *) 
    saveBB : LLVM.BasicBlockRef; (* for nested procs save the bb *)
    localStack  : RefSeq.T := NIL;
    paramStack  : RefSeq.T := NIL;
    uplevelRefdStack  : RefSeq.T := NIL;
      (* ^List of params and locals that are uplevel-referenced. *) 
    cumUplevelRefdCt : CARDINAL := 0; 
      (* ^Between the declare_procedure and begin_procedure, the number of
          params and locals declared so far that are uplevel-referenced.
          After begin_procedure, also includes those of all static ancestor
          procs as well. *)
    staticLinkFormal : LvVar := NIL; (* i8* *)
      (* ^For most procedures, CG emits neither a static link formal nor an
          actual for it in a call.  We provide these, for a nested procedure.
          For an internally-generated FINALLY procedure, CG emits an explicit 
          formal for a static link, which we just use, but CG does not emit an 
          actual parameter for in it a call, so we provide that too.  CG does, 
          however, explicitly pass a SL value to the runtime, when pushing a 
          FINALLY frame, and this SL will be passed by the runtime when it calls 
          the FINALLY procedure.
          This really would be more consistend as i8**, but CG makes assignments
          between this and things CG types as Addr, that are not easily
          identifiable as static link values. So we have to type static links
          as i8* 
      *)     
    storedStaticLink : LvVar := NIL; (* i8** *) 
      (* ^A memory copy of staticLinkFormal, always stored as 1st alloca of
          a nested procedure, so the debugger can find it. *) 
    imported : BOOLEAN := FALSE; (* if this is an import *)
    defined : BOOLEAN := FALSE; (* set when we build the declaration for real *)
    displayLty : LLVM.TypeRef := NIL; (* i8** *) 
      (* ^llvm type for a display, an array of addresses to all up-level- 
         referenced variables in this proc and its static ancestors.  If it 
         calls a procedure nested one deeper than itself and that wants a
         display, this proc will create it in a local of this type (only once)
         and pass its address as an added static link parameter to the callee. *)
    outgoingDisplayLv : LLVM.ValueRef := NIL; (* i8** *) 
    (* ^Address of the display this proc will pass to one-level deeper nested
        procs.  We store the display itself in the AR, but this pointer to it
        is an llvm SSA variable that we don't explicitly store. *)
    needsDisplay : BOOLEAN := FALSE; 
  END;

  LvExpr = OBJECT
    lVal : LLVM.ValueRef;
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
    branchBB : LLVM.BasicBlockRef; (* the bb where the branch resides ie this is the bb where we found the jmp *)
  END;

  LabelObj = OBJECT
    id : Label;
    barrier : BOOLEAN := FALSE; (* unused *)
    labBB : LLVM.BasicBlockRef;  (* jmps goto this bb *)
    elseBB : LLVM.BasicBlockRef; (* else bb for compares *)
    cmpInstr : LLVM.ValueRef; (* saved cmp instr used to move the bb *)
    branchList : RefSeq.T; (* list of branches to this label *)
  END;

  (* template object for common If-Then-Else construction *)

  ITEObj = OBJECT
    curObj : U;
    opType : Type;
    tmpVar : LvVar;
    cmpVal : LLVM.ValueRef;
    curBB,thenBB,elseBB,exitBB,beforeBB : LLVM.BasicBlockRef := NIL;
    opName : TEXT;
  METHODS
    init() : ITEObj := ITEInit;
    block(storeVal : LLVM.ValueRef; endBB : BOOLEAN) : LLVM.ValueRef := ITEBlock;
  END;

PROCEDURE SignExtend(a, b: INTEGER): INTEGER =
BEGIN
    b := Word.LeftShift(-1, b - 1);
    IF Word.And(a, b) # 0 THEN
        a := Word.Or(a, b);
    END;
    RETURN a;
END SignExtend;

PROCEDURE SignExtend32(a: INTEGER): INT32 =
BEGIN
    RETURN SignExtend(a, 32);
END SignExtend32;

CONST IntegerToTypeid = SignExtend32;

  (* debug uids for basic types *)
CONST NO_UID = 16_FFFFFFFF;

VAR UID_INTEGER := IntegerToTypeid(16_195C2A74);
CONST UID_LONGINT = 16_05562176;
VAR UID_CARDINAL := IntegerToTypeid(16_97E237E2); (* CARDINAL *)
VAR UID_LONGCARD := IntegerToTypeid(16_9CED36E7); (* LONGCARD *)
CONST UID_REEL = 16_48E16572; (* REAL *)
VAR UID_LREEL := IntegerToTypeid(16_94FE32F6); (* LONGREAL *)
VAR UID_XREEL := IntegerToTypeid(16_9EE024E3); (* EXTENDED *)
CONST UID_BOOLEAN = 16_1E59237D; (* BOOLEAN [0..1] *)
CONST UID_CHAR = 16_56E16863; (* CHAR [0..255] *)
VAR UID_WIDECHAR := IntegerToTypeid(16_88F439FC);
CONST UID_MUTEX = 16_1541F475; (* MUTEX *)
CONST UID_TEXT = 16_50F86574; (* TEXT *)
VAR UID_UNTRACED_ROOT := IntegerToTypeid(16_898EA789); (* UNTRACED ROOT *)
VAR UID_ROOT := IntegerToTypeid(16_9D8FB489); (* ROOT *)
CONST UID_REFANY = 16_1C1C45E6; (* REFANY *)
CONST UID_ADDR = 16_08402063; (* ADDRESS *)
CONST UID_RANGE_0_31 = 16_2DA6581D; (* [0..31] *)
CONST UID_RANGE_0_63 = 16_2FA3581D; (* [0..63] *)
CONST UID_NULL = 16_48EC756E; (* NULL *) (* Occurs in elego/graphicutils/src/RsrcFilter.m3 *)

(* not used
  UID_PROC1 = 16_9C9DE465;
  UID_PROC2 = 16_20AD399F;
  UID_PROC3 = 16_3CE4D13B;
  UID_PROC4 = 16_FA03E372;
  UID_PROC5 = 16_509E4C68;
  UID_PROC6 = 16_DC1B3625;
  UID_PROC7 = 16_EE17DF2C;
  UID_PROC8 = 16_B740EFD0;
*) 

(* debug objects *)
TYPE

  BaseDebug = OBJECT
    tUid : TypeUID;
    bitSize,align : LONGINT;
    typeName : Name;
    encoding : unsigned;
    DIDescr : M3DIB.LLVMDIDescriptor := NIL; 
  END;

  ArrayDebug = BaseDebug OBJECT
    index,elt : TypeUID;
  END;

  SetDebug = BaseDebug OBJECT
    domain : TypeUID;
  END;

  OpenArrayDebug = BaseDebug OBJECT
    elt : TypeUID;
  END;

  EnumDebug = BaseDebug OBJECT
    numElts, index : CARDINAL;
    elts : REF ARRAY OF Name;
  END;

  SubrangeDebug = BaseDebug OBJECT
    domain : TypeUID;
    min, count : TInt.Int;
  END;

  FieldDebug = BaseDebug OBJECT
    name : Name;
    bitOffset : LONGINT;
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
    var : LvVar; (* for global debug *)
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

  FormalDebug = BaseDebug OBJECT
    name : Name;
  END;

  RaisesDebug = BaseDebug OBJECT
    name : Name;
  END;

  ProcTypeDebug = BaseDebug OBJECT
    result : TypeUID;
    numFormals,numRaises,formalIdx,raisesIdx : INTEGER; (* numRaises can be neg *)
    cc : CallingConvention;
    formals : REF ARRAY OF FormalDebug;
    raises : REF ARRAY OF RaisesDebug;
  END;

  (* debug lexical blocks *)
  BlockDebug = OBJECT
    value : M3DIB.LLVMDISubprogram;
  END;

VAR
  modRef : LLVM.ModuleRef := NIL;
  builderIR : LLVM.BuilderRef;
  globContext : LLVM.ContextRef;
  moduleID : Ctypes.char_star;
  faultVal : LLVM.ValueRef;  (* the abort function *)
  targetData : LLVM.TargetDataRef;

(* need to get these from front end somehow *)
  targetTriple := "x86_64-pc-linux-gnu";
  dataRep := "e-m:e-i64:64-f80:128-n8:16:32:64-S128";

  (* const once read *)
   ptrBytes : INTEGER; (* target pointer size in bytes *)
   ptrBits : LONGINT;  (* target pointer size in bits *)
   intBytes : INTEGER; (* target integer size in bytes *)
   intBits : LONGINT;  (* target integer size in bits *)
   widecharBytes : INTEGER; (* WIDECHAR size in bytes *)
   widecharBits : LONGINT;  (* WIDECHAR size in bits *)

   IntPtrTy : LLVM.TypeRef; (* int type having same size as pointer *)
   PtrTy : LLVM.TypeRef; (*= LLVM.LLVMPointerType(IntPtrTy);*)
   AdrTy : LLVM.TypeRef; (* llvm i8* *)
   AdrAdrTy : LLVM.TypeRef; (* llvm i8** *)
   AdrAdrAdrTy : LLVM.TypeRef; (* llvm i8*** *)

   i8Type := LLVM.LLVMInt8Type(); (* Byte type *)
   wordSize : LLVM.ValueRef; (* no of bits in word as llvm value *)
   byteSize : LLVM.ValueRef; (* no of bytes in word as llvm value *)

   (* Keep EXTENDED type compatible with front end which is double. Later
   we could change it to a 128 bit quad precision floating point *)
   ExtendedType := LLVM.LLVMDoubleType();
   (* ExtendedType := LLVM.LLVMFP128Type(); *)

(*--------------------------------------------------------------- Utility ---*)

PROCEDURE TIntToint64_t(Val: TInt.Int) : int64_t = 
  VAR IVal: INTEGER; 
  BEGIN  
    EVAL TInt.ToInt(Val, IVal);
(* FIXME: This will not cross compile 32- to 64-bits. *) 
(* We need a TInt.ToLongInt. *) 
    RETURN VAL(IVal, int64_t); 
  END TIntToint64_t; 

(* For use from m3gdb: *) 
PROCEDURE LvType ( Val: LLVM.ValueRef) : LLVM.TypeRef =
  BEGIN
    RETURN LLVM.LLVMTypeOf(Val); 
  END LvType;

PROCEDURE DumpLvType ( TR: LLVM.TypeRef ) =
  BEGIN
    LLVM.LLVMDumpType(TR); 
  END DumpLvType;

PROCEDURE DumpLvVal ( VR: LLVM.ValueRef ) =
  BEGIN
    LLVM.LLVMDumpValue(VR); 
  END DumpLvVal;

PROCEDURE GetBBLastInstruction ( BB: LLVM.BasicBlockRef; expectedOp: LLVM.Opcode )
  : LLVM.ValueRef =
  VAR inst : LLVM.ValueRef;
  VAR Op: LLVM.Opcode;
  BEGIN
    inst := LLVM.LLVMGetLastInstruction(BB);
    <* ASSERT inst # NIL *> 
    Op := LLVM.LLVMGetInstructionOpcode(inst);
    <* ASSERT Op = expectedOp *>
    RETURN inst; 
  END GetBBLastInstruction;

(*-------------------------simplify all the untraced array allocs for llvm ---*)

(* simplify all the untraced array allocs for llvm *)
TYPE
  ValueArrType = UNTRACED REF ARRAY OF LLVM.ValueRef;
  ValueRefType = UNTRACED REF LLVM.ValueRef;
  TypeArrType = UNTRACED REF ARRAY OF LLVM.TypeRef;
  TypeRefType = UNTRACED REF LLVM.TypeRef;
  BBArrType = UNTRACED REF ARRAY OF LLVM.BasicBlockRef;
  BBRefType = UNTRACED REF LLVM.BasicBlockRef;

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

PROCEDURE NewBBArr(VAR BBsArr : BBArrType; numBBs : CARDINAL) : BBRefType =
  BEGIN
    IF numBBs = 0 THEN RETURN NIL; END;
    BBsArr := NEW(BBArrType, numBBs);
    RETURN LOOPHOLE(ADR(BBsArr[0]), BBRefType);
  END NewBBArr;

PROCEDURE NewArrayRefOfMetadataRef 
  ( ElemCt : CARDINAL; 
    VAR (*OUT*) Open : REF ARRAY OF MetadataRef; 
        (* ^As Rodney Bates has been told by llvm folk, llvm will immediately 
           copy everything passed in and take responsibility for memory-
           managing the copy, so we can let this be GCed, once done giving 
           ArrRef to llvm. *) 
    VAR (*OUT*) ArrRef : ArrayRefOfMetadataRef 
    (* Open and ArrRef denote the same array of MetadataRef, in different ways.
       Open is a Modula-3 open array, to be used by Modula-3 code. 
       ArrRef is a C/C++ {pointer,elementCount} pair that llvm expects.
       Both lead to the same actual array.  
    *) 
  ) =
  BEGIN 
    IF ElemCt > 0 THEN
      Open := NEW ( REF ARRAY OF MetadataRef , ElemCt );
      ArrRef . Data := ADR (Open[0]);
    ELSE 
      Open := NIL; 
      ArrRef . Data := NIL; (* Llvm might choke on this? *) 
    END;
    ArrRef . Length := ElemCt;    
  END NewArrayRefOfMetadataRef; 

(* EXPORTED: *) 
PROCEDURE New 
  (output: Wr.T; m3llvmDebugLev: m3llvmDebugLevTyp; genDebug: BOOLEAN)
: M3CG.T =
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
                m3llvmDebugLev := m3llvmDebugLev,
                genDebug := genDebug,
                debugLexStack := NEW(RefSeq.T).init(),
                allocaName := M3ID.Add("alloca"));
  END New;

PROCEDURE NewVar 
  (self: U; name : Name; size : ByteSize; align : Alignment; type : Type; 
   isConst : BOOLEAN; m3t : TypeUID; in_memory : BOOLEAN; up_level : BOOLEAN; 
   exported : BOOLEAN; inited : BOOLEAN; frequency : Frequency; varType : VarType)
: Var =
  VAR
    v := NEW (LvVar, tag := self.next_var, name := name, size := size, type := type, 
              isConst := isConst, align := align, m3t := m3t, in_memory := in_memory, 
              up_level := up_level, exported := exported, inited := inited, 
              frequency := frequency, varType := varType);
  BEGIN
    INC (self.next_var);
    IF varType = VarType.Global THEN
      v.inits := NEW(RefSeq.T).init();
    END;
    IF v.type = Type.Struct THEN
      v.lvType := StructType(self,v.size);
    ELSE
      v.lvType := LLvmType(v.type);
    END;
    RETURN v;
  END NewVar;

PROCEDURE NewProc (self: U; name : Name; numParams : INTEGER; returnType : Type; lev : INTEGER; cc : CallingConvention; exported : BOOLEAN; parent : Proc): Proc =
  VAR
    p := NEW (LvProc, tag := self.next_proc, name := name, numParams := numParams, returnType := returnType, lev := lev, cc := cc, exported := exported, parent := parent);
  BEGIN
    INC (self.next_proc);
    RETURN p;
  END NewProc;

PROCEDURE MakeRefSeqEmpty (stack : RefSeq.T ) = 
  BEGIN
    FOR i := 1 TO stack.size () DO EVAL stack.remlo(); END;
  END MakeRefSeqEmpty; 

PROCEDURE Pop(stack : RefSeq.T; n: CARDINAL := 1) =
  BEGIN
    FOR i := 1 TO n DO EVAL stack.remlo(); END;
  END Pop;

PROCEDURE Push(stack : RefSeq.T; value : REFANY) =
  BEGIN
    stack.addlo(value);
  END Push;

(* unused now static link is first parm
PROCEDURE PopRev(stack : RefSeq.T; n: CARDINAL := 1) =
  BEGIN
    FOR i := 1 TO n DO EVAL stack.remhi(); END;
  END PopRev;
*)

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
    RETURN VAL(LLVM.LLVMStoreSizeOfType(targetData,LLvmType(t)),CARDINAL);
  END TypeSize;

PROCEDURE LLvmType(t : Type) : LLVM.TypeRef =
  BEGIN
    CASE t OF
    | Type.Int8,Type.Word8   => RETURN LLVM.LLVMInt8Type();
    | Type.Int16,Type.Word16  => RETURN LLVM.LLVMInt16Type();
    | Type.Int32,Type.Word32  => RETURN LLVM.LLVMInt32Type();
    | Type.Int64,Type.Word64  => RETURN LLVM.LLVMInt64Type();
(* CHECK: Doesn't llvm have unsigned 8,16,32,64 types?  Not in LLVM, at least. *) 
    | Type.Reel   => RETURN LLVM.LLVMFloatType();
    | Type.LReel  => RETURN LLVM.LLVMDoubleType();
    | Type.XReel  => RETURN ExtendedType;
    | Type.Addr   => RETURN AdrTy;
    | Type.Struct => RETURN AdrTy;  (* never called *)
    | Type.Void  => RETURN LLVM.LLVMVoidType();
    END;
  END LLvmType;

PROCEDURE StructType(self : U; size : ByteSize) : LLVM.TypeRef =
(* A uniqued llvm array type of size bytes. *) 
  CONST numElems = 1;
  VAR    
    arrTy,structTy : LLVM.TypeRef;
    elemArr : TypeArrType;
    elemRef : TypeRefType;
    typeExists : BOOLEAN;
    structRef : REFANY;
  BEGIN
    typeExists := self.structTable.get(size,structRef);
    IF typeExists THEN
      structTy := NARROW(structRef,LvStruct).struct;
    ELSE
      elemRef := NewTypeArr(elemArr,numElems);
      arrTy := LLVM.LLVMArrayType(i8Type,size);
      elemArr[0] := arrTy;
      structTy := LLVM.LLVMStructCreateNamed(globContext, LT("struct"));
      LLVM.LLVMStructSetBody(structTy, elemRef, numElems, FALSE);
      (* save the type *)
      structRef := NEW(LvStruct,struct := structTy);
      EVAL self.structTable.put(size,structRef);
    END;
    RETURN structTy;    
  END StructType;

PROCEDURE Zero(t : LLVM.TypeRef) : LLVM.ValueRef =
  BEGIN
    RETURN LLVM.LLVMConstNull(t);
(* CHECK: ^Is the resule of every execution here unique?
   We need it to be, in places. *) 
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

(* For strings passed to stuff in DIBuilder: *) 
PROCEDURE LTD(t : TEXT) : StringRef =
  VAR LResult : StringRef;
  BEGIN
    LResult . Length := Text.Length (t); 
    LResult . Data := M3toC.CopyTtoS(t);
    RETURN LResult;
  END LTD;
  
PROCEDURE ITEInit(self : ITEObj) : ITEObj =
  VAR
    size : CARDINAL;
  BEGIN
    size := TypeSize(self.opType);
    self.curBB := LLVM.LLVMGetInsertBlock(builderIR);    
    self.tmpVar := self.curObj.declare_temp (size, size, self.opType, TRUE);

    WITH cp = self.curObj.curProc.lvProc DO
      IF self.beforeBB = NIL THEN
        self.exitBB := LLVM.LLVMAppendBasicBlock(cp, LT(self.opName & "_end"));
      ELSE
        (* nested if-then-else blocks *)
        self.exitBB := LLVM.LLVMInsertBasicBlock(self.beforeBB, LT(self.opName & "_end"));
      END;
      self.elseBB := LLVM.LLVMInsertBasicBlock(self.exitBB, LT(self.opName & "_else"));
      self.thenBB := LLVM.LLVMInsertBasicBlock(self.elseBB, LT(self.opName & "_then"));
    END;  
    LLVM.LLVMPositionBuilderAtEnd(builderIR,self.curBB);
    EVAL LLVM.LLVMBuildCondBr(builderIR,self.cmpVal,self.thenBB,self.elseBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,self.thenBB);
    RETURN self;
 END ITEInit;

PROCEDURE ITEBlock(self : ITEObj; storeVal : LLVM.ValueRef; endBB : BOOLEAN) : LLVM.ValueRef =
  VAR res : LLVM.ValueRef := NIL;
  BEGIN
    EVAL LLVM.LLVMBuildStore(builderIR, storeVal, self.tmpVar.lv);
    EVAL LLVM.LLVMBuildBr(builderIR,self.exitBB);
    IF endBB THEN
      LLVM.LLVMPositionBuilderAtEnd(builderIR,self.exitBB);
      res := LLVM.LLVMBuildLoad(builderIR, self.tmpVar.lv, LT(self.opName & "_load"));
    ELSE (* elseBB *)
      LLVM.LLVMPositionBuilderAtEnd(builderIR,self.elseBB);
    END;
    RETURN res;
  END ITEBlock;

(* debug
PROCEDURE CheckIntrinsics() =
BEGIN
  EVAL MemMoveFn();
  EVAL MemCopyFn();
  EVAL MemSetFn();

  EVAL RealIntrinsic(Type.Reel, LLVM.M3Intrinsic.m3round);
  EVAL RealIntrinsic(Type.LReel, LLVM.M3Intrinsic.m3round);
  EVAL RealIntrinsic(Type.XReel, LLVM.M3Intrinsic.m3round);

  EVAL RealIntrinsic(Type.Reel, LLVM.M3Intrinsic.m3floor);
  EVAL RealIntrinsic(Type.LReel, LLVM.M3Intrinsic.m3floor);
  EVAL RealIntrinsic(Type.XReel, LLVM.M3Intrinsic.m3floor);
  
  EVAL RealIntrinsic(Type.Reel, LLVM.M3Intrinsic.m3trunc);
  EVAL RealIntrinsic(Type.LReel, LLVM.M3Intrinsic.m3trunc);
  EVAL RealIntrinsic(Type.XReel, LLVM.M3Intrinsic.m3trunc);
  
  EVAL RealIntrinsic(Type.Reel, LLVM.M3Intrinsic.m3ceil);
  EVAL RealIntrinsic(Type.LReel, LLVM.M3Intrinsic.m3ceil);
  EVAL RealIntrinsic(Type.XReel, LLVM.M3Intrinsic.m3ceil);
  
  EVAL RealIntrinsic(Type.Reel, LLVM.M3Intrinsic.m3fabs);
  EVAL RealIntrinsic(Type.LReel, LLVM.M3Intrinsic.m3fabs);
  EVAL RealIntrinsic(Type.XReel, LLVM.M3Intrinsic.m3fabs);

  EVAL RealIntrinsic(Type.Reel, LLVM.M3Intrinsic.m3minnum);
  EVAL RealIntrinsic(Type.LReel, LLVM.M3Intrinsic.m3minnum);
  EVAL RealIntrinsic(Type.XReel, LLVM.M3Intrinsic.m3minnum);

  EVAL RealIntrinsic(Type.Reel, LLVM.M3Intrinsic.m3maxnum);
  EVAL RealIntrinsic(Type.LReel, LLVM.M3Intrinsic.m3maxnum);
  EVAL RealIntrinsic(Type.XReel, LLVM.M3Intrinsic.m3maxnum);
     
END CheckIntrinsics;
*)

(* embed version info into generated llvm and hence assembly 
   Need to rationalise the version stuff. Good to have the shipped version
   of cm3 plus the llvm library version we are using. Then there is the
   version of this file. 
*)
PROCEDURE EmbedVersion() =
  VAR
    mdNode : LLVM.ValueRef;
    identMD : ARRAY[0..0] OF REFANY;
    cm3Ver,llvmVer,ident : TEXT;
    llmajor,llminor : Ctypes.int;
  BEGIN
    cm3Ver := "5.8";
    LLVM.GetLLVMVersion(llmajor,llminor);
    llvmVer := Fmt.Int(llmajor) & "." & Fmt.Int(llminor);
    ident := "versions- cm3: " & cm3Ver & " llvm: " & llvmVer;
    
    identMD[0] := ident;
    mdNode := GetMDNode(identMD);
    LLVM.LLVMAddNamedMetadataOperand(modRef, LT("llvm.ident"), mdNode);
  END EmbedVersion;


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

PROCEDURE CGProvidedStaticLinkFormal(proc : LvProc) : LvVar =
(* Return the LvVar of the front-end-generated static link formal parameter
   of proc, if it has one, otherwise NIL.  This happens only for an internally
   generated FINALLY procedure, by CG.   
   Making a pretty big assumption here.  The criterion is proc is nested,
   has exactly one formal, and the formal is nameless. *)
  VAR arg : REFANY; param : LvVar;
  BEGIN
    IF proc.lev > 0 AND proc.paramStack.size() = 1 THEN
      arg := Get(proc.paramStack);
      param := NARROW(arg,LvVar);
      IF param.name = M3ID.NoID THEN
        RETURN param;
      END;
    END;
    RETURN NIL;
  END CGProvidedStaticLinkFormal;

(* Create Llvm declarations for this procedure and all its parameters.
   This is enough to handle calls and assignments of it, without requiring
   that its body have been seen. *)
PROCEDURE BuildFunc(self : U; p : Proc) =
  VAR
    param : LvVar;
    mlProc : LvProc;
    retTy : LLVM.TypeRef;
    paramsArr : TypeArrType;
    paramsRef : TypeRefType;
    lVal : LLVM.ValueRef;
    numParams : CARDINAL := 0;
    procTextName, paramTextName : TEXT;
    name : Name;
    arg : REFANY;
  BEGIN
    mlProc := NARROW(p,LvProc);
    IF mlProc.defined THEN RETURN; END; (* Only do this once per function. *)
    <* ASSERT mlProc.state = procState.decld *>

    procTextName := M3ID.ToText(mlProc.name);
    IF mlProc.lev > 0 THEN (* 'mlProc' is nested. *)
      (* Always give a nested proc a static link. We won't know whether it
        will be used until we finish its body, but there can be calls on
        it before that.  Besides, decent debugger behavour needs it,
        even if generated code does not.*) 
      param := CGProvidedStaticLinkFormal (mlProc); 
      IF param # NIL 
      THEN (* CG already provided it. Give it a name and note its identity. *)
        paramTextName := "__CG_StaticLinkFormal";
        param.name := M3ID.Add(paramTextName);
        mlProc.staticLinkFormal := param         
      ELSE (* Create a static link formal. *) 
        paramTextName := "__m3llvm_StaticLinkFormal";
        name := M3ID.Add(paramTextName); 
        mlProc.staticLinkFormal (* i8* *) 
          := NewVar (self,name,ptrBytes,ptrBytes,
                    Type.Addr, isConst:=TRUE,m3t:=UID_ADDR,in_memory:=TRUE,
                    up_level:=FALSE,exported:=FALSE,inited:=FALSE,
                    frequency:=M3CG.Maybe,varType:=VarType.Param); 
        Push(mlProc.paramStack, mlProc.staticLinkFormal); (* Make it leftmost formal. *)
        INC(mlProc.numParams);
      END;
      (* Here, we have not necessarily seen mlProc's body, thus don't know the
         display size or the true type of the static link.  We just make all
         static link formals i8** and bitcast uses, as needed. *) 
    END;

    numParams := mlProc.numParams;

    IF mlProc.imported THEN
      (* delete the temp function before defining the real one *)
      LLVM.LLVMDeleteFunction(mlProc.lvProc);
    END;
    mlProc.defined := TRUE;
    <*ASSERT mlProc.paramStack.size() = numParams *>

    (* create the llvm param types from the param stack *)
    paramsRef := NewTypeArr(paramsArr,numParams);
    FOR i := 0 TO numParams - 1 DO
      arg := Get(mlProc.paramStack,i);
      param := NARROW(arg,LvVar);
      IF param.type = Type.Struct THEN
        param.lvType := LLVM.LLVMPointerType(param.lvType);
(* NOTE: NewVar already set this to StructType(size). *)
(* REVIEW: We always pass structs by reference at IR code level.
           If M3 mode is VALUE, call code will use pop_struct, which
           makes a call-site copy before passing the address.
           But what type do we want here? *) 
      END;
      paramsArr[i] := param.lvType;
    END;

    (* create the return type *)
    retTy := LLvmType(mlProc.returnType);

    (* create the function signature llvm type. *)
    mlProc.procTy
      := LLVM.LLVMFunctionType(retTy, paramsRef, numParams, IsVarArg := FALSE);

    (* create the llvm function *)
    mlProc.lvProc := LLVM.LLVMAddFunction(modRef, LT(procTextName), mlProc.procTy);

    (* c funcs seem to have these attrs ?? *)
    LLVM.LLVMAddFunctionAttr(mlProc.lvProc, LLVM.NoUnwindAttribute);
    LLVM.LLVMAddFunctionAttr(mlProc.lvProc, LLVM.UWTable);

    <*ASSERT LLVM.LLVMCountParams(mlProc.lvProc) = numParams *>

    (* add names, attributes, and alignment to llvm formals. *)
    lVal := LLVM.LLVMGetFirstParam(mlProc.lvProc);
    FOR i := 0 TO numParams - 1 DO
      arg := Get(mlProc.paramStack,i);
      param := NARROW(arg,LvVar);
      IF param.name = M3ID.NoID THEN (* Can this happen? *) 
        paramTextName := procTextName & "__AnonFormal_" & Fmt.Int(i);
        param.name := M3ID.Add(paramTextName);
      ELSE
        paramTextName := M3ID.ToText(param.name);
      END;
      (* set 'nest' attribute of staticLinkFormal. This makes for ABI
         compatibility with m3cc, and allows interoperation between
         llvm-compiled code and an m3cc-compiled runtime . *)
      IF param = mlProc.staticLinkFormal THEN
        <* ASSERT i = 0 *> (* For leftmost SL. *)
        LLVM.LLVMAddAttribute(lVal, LLVM.NestAttribute);
      END; 
      (* set a name for the param - doesn't work for externals *)
      LLVM.LLVMSetValueName(lVal, LT(paramTextName));

      (* Don't use byval attribute, because it is too hard to know when to
         apply it to an indirect call.  Instead, we generate explicit copy code
         at the call site, in response to the pop_struct operator. Eli Friedman,
         says, on llvm-dev@lists.llvm.org at 11/03/2017 05:44 PM, that this may
         allow better optimizations anyway. *) 
   (* IF param.type = Type.Struct THEN
        LLVM.LLVMAddAttribute(lVal, LLVM.ByValAttribute);
      END; *) 
      (* set the alignment not sure we need it except for struct *)
      LLVM.LLVMSetParamAlignment(lVal, param.align);
      IF param = mlProc.staticLinkFormal THEN param.lvSsa := lVal; END; 
      lVal := LLVM.LLVMGetNextParam(lVal);
    END;
    mlProc.state := procState.built;

  END BuildFunc;

PROCEDURE DumpLLVMIR(<*UNUSED*> self : U; BitcodeFileName, AsmFileName: TEXT) =
  VAR
    msg : Ctypes.char_star_star := NIL;
  BEGIN
    (* Write Assembly format 1st, in case of obscure failures during write. *) 
    IF AsmFileName # NIL THEN
      EVAL LLVM.LLVMPrintModuleToFile(modRef, LT(AsmFileName), msg);
    END (*IF*); 
    IF BitcodeFileName # NIL THEN
      EVAL LLVM.LLVMWriteBitcodeToFile(modRef, LT(BitcodeFileName));
    END (*IF*); 
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

PROCEDURE begin_unit (self: U;  optimize : INTEGER) =
  BEGIN
    self.optLevel := optimize;
    globContext := LLVM.LLVMGetGlobalContext();
    builderIR := LLVM.LLVMCreateBuilderInContext(globContext);

    targetData := LLVM.LLVMCreateTargetData(LT(dataRep));
    ptrBytes := LLVM.LLVMPointerSize(targetData);
    intBytes := ptrBytes; (* ASSUMPTION!! *) 
    IntPtrTy := LLVM.LLVMIntPtrType(targetData);
    PtrTy := LLVM.LLVMPointerType(IntPtrTy);
    AdrTy := LLVM.LLVMPointerType(LLVM.LLVMInt8Type());
    AdrAdrTy := LLVM.LLVMPointerType(AdrTy);
    AdrAdrAdrTy := LLVM.LLVMPointerType(AdrAdrTy);
    ptrBits := LLVM.LLVMSizeOfTypeInBits(targetData, PtrTy);
    intBits := LLVM.LLVMSizeOfTypeInBits(targetData, IntPtrTy);
    widecharBytes := 2; (* May change. *)
    widecharBits := 16L; (* May change. *) 
    wordSize := LLVM.LLVMConstInt(IntPtrTy, VAL(ptrBits,LONGINT), TRUE);
    byteSize := LLVM.LLVMConstInt(IntPtrTy, VAL(ptrBytes,LONGINT), TRUE);
  END begin_unit;

PROCEDURE end_unit (self: U) =
  VAR
    iter : IntRefTbl.Iterator;
    key : INTEGER;
    lab : REFANY;
    label : LabelObj;
    terminator : LLVM.ValueRef;
    curBB : LLVM.BasicBlockRef;
  BEGIN
(* debug
    CheckIntrinsics();
*)

    (* could be a label after an exit_proc which created a bb or a loop with no
    exit, either way the bb must have a terminator so add an unreachable stmt*)
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
    IF modRef = NIL THEN  
      self.curFile := file;
      moduleID := LT(file);
      modRef := LLVM.LLVMModuleCreateWithNameInContext(moduleID,globContext);
      LLVM.LLVMSetDataLayout(modRef,LT(dataRep));
      LLVM.LLVMSetTarget(modRef,LT(targetTriple));
      EmbedVersion();         
      DebugInit(self);
    END;
  END set_source_file;

PROCEDURE set_source_line (self: U; line: INTEGER) =
  BEGIN
    self.curLine := line;
    IF self.m3llvmDebugLev > 0 THEN 
      IO.Put("LINE ------------------------ " & Fmt.Int(line) & "------------\n");
    END; 
    IF self.curProc # NIL AND self.m3llvmDebugLev > 0 THEN
      IO.Put("the cur proc " & M3ID.ToText(self.curProc.name) & "\n");
    END;
    (* set the debugloc for this line *)
    DebugLine(self);
  END set_source_line;

(*------------------------------------------- debugging type declarations ---*)

PROCEDURE declare_typename (self: U; t: TypeUID; n: Name) =
  VAR
    baseObj : BaseDebug;
    typeRef : REFANY;
    tUidExists : BOOLEAN;
  BEGIN
    tUidExists := self.debugTable.get(t, (*OUT*)typeRef);
    IF tUidExists THEN
      baseObj := NARROW(typeRef,BaseDebug);
      baseObj.typeName := n;
    ELSE
     <*ASSERT FALSE *>
    END;
    (* fixme is it possible for the tid to not exist ? *)
  END declare_typename;

PROCEDURE declare_array (self: U; t,index,elt: TypeUID; s: BitSize) =
  VAR
    arrayRef : ArrayDebug;
  BEGIN
    arrayRef := NEW(ArrayDebug, tUid := t, index := index, elt := elt, bitSize := VAL(s,LONGINT), align := ptrBits);
    EVAL self.debugTable.put(t,arrayRef);
  END declare_array;

PROCEDURE declare_open_array (self: U; t, elt: TypeUID; s: BitSize) =
  VAR
    arrayRef : OpenArrayDebug;
  BEGIN
    arrayRef := NEW(OpenArrayDebug, tUid := t, elt := elt, bitSize := VAL(s,LONGINT), align := ptrBits);
    EVAL self.debugTable.put(t,arrayRef);
  END declare_open_array;

PROCEDURE declare_enum (self: U; t: TypeUID; n_elts: INTEGER; s: BitSize) =
  VAR
    enumRef : EnumDebug;
  BEGIN
    enumRef := NEW(EnumDebug, tUid := t, numElts := n_elts, bitSize := VAL(s,LONGINT), align := VAL(s,LONGINT));
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

PROCEDURE declare_packed (self: U; t: TypeUID; s: BitSize; base: TypeUID) =
  VAR
    packedRef : PackedDebug;
  BEGIN
    packedRef := NEW(PackedDebug, tUid := t, base := base, bitSize := VAL(s,LONGINT), align := ptrBits);
    EVAL self.debugTable.put(t,packedRef);
  END declare_packed;

PROCEDURE declare_record (self: U; t: TypeUID; s: BitSize; n_fields: INTEGER) =
  VAR
    recordRef : RecordDebug;
  BEGIN
    recordRef := NEW(RecordDebug, tUid := t, numFields := n_fields, bitSize := VAL(s,LONGINT), align := ptrBits);
    recordRef.fields := NEW(REF ARRAY OF FieldDebug,n_fields);
    recordRef.fieldIndex := 0;
    IF t = -1 THEN
      recordRef.global := TRUE;
      (* need to track the global rec for debug & const seems to be first*)
      IF NOT self.seenConst THEN
        recordRef.var := self.segConst;
        self.seenConst := TRUE;
      ELSE
        recordRef.var := self.segVar;
      END;
    END;
    EVAL self.debugTable.put(t,recordRef);
    self.debugObj := recordRef; (* keep for the fields *)
  END declare_record;

PROCEDURE Align(s : BitSize) : LONGINT =
  BEGIN
    IF s <= 8 THEN RETURN 8L;
    ELSIF s <= 32 THEN RETURN 32L;
    ELSE RETURN ptrBits;
    END;
  END Align;

PROCEDURE declare_field (self: U; n: Name; o: BitOffset; s: BitSize; t: TypeUID) =
  VAR
    recordRef : RecordDebug;
    align : LONGINT;
  BEGIN
    recordRef := self.debugObj;
    <*ASSERT ISTYPE(recordRef,RecordDebug) *>
    align := Align(s);
    recordRef.fields[recordRef.fieldIndex] := NEW(FieldDebug,name := n, bitOffset := VAL(o,LONGINT), tUid := t, bitSize := VAL(s,LONGINT), align := align);
    INC(recordRef.fieldIndex);
  END declare_field;

PROCEDURE declare_set (self: U; t,domain: TypeUID;  s: BitSize) =
  VAR
    setRef : SetDebug;
  BEGIN
    setRef := NEW(SetDebug, tUid := t, domain := domain, bitSize := VAL(s,LONGINT), align := VAL(s,LONGINT));
    EVAL self.debugTable.put(t,setRef);
  END declare_set;

PROCEDURE declare_subrange 
  (self: U; t,domain: TypeUID; READONLY min, max: Target.Int(*=Tint.Int*); s: BitSize) 
= 
  VAR
    LDebug : SubrangeDebug;
    encoding: unsigned; 
    nameText: TEXT := "";
  BEGIN
    encoding := DC.DW_ATE_signed;
    (* domain could be a forward reference, so we can't necessarily compute 
       encoding now.  Make an assumption, but fix it later, in SubrangeDebug,
       when we can lookup domain. *) 
(* TODO: Get something better for nameText. *) 
    LDebug 
      := BuildSubrangeDebug
           (self, t, domain, min, max, 
            bitSize := VAL(s,LONGINT), align := VAL(s,LONGINT), 
            encoding := encoding, 
            nameText := nameText); 
  END declare_subrange;

PROCEDURE declare_pointer (self: U; t,target: TypeUID;  brand: TEXT; traced: BOOLEAN) =
  VAR
    ptrRef : PointerDebug;
  BEGIN
    ptrRef := NEW(PointerDebug, tUid := t, target := target, brand := brand, traced := traced, bitSize := ptrBits, align := ptrBits);
    EVAL self.debugTable.put(t,ptrRef);
  END declare_pointer;

PROCEDURE declare_indirect (self: U; t, target: TypeUID; <*UNUSED*>target_typename: Name) =
  VAR
    indirectRef : IndirectDebug;
  BEGIN
    indirectRef := NEW(IndirectDebug, tUid := t, target := target);
    EVAL self.debugTable.put(t,indirectRef);
  END declare_indirect;

PROCEDURE declare_proctype (self: U; t: TypeUID; n_formals: INTEGER; result: TypeUID;  n_raises: INTEGER; cc: CallingConvention; <*UNUSED*>result_typename: Name) =
  VAR
    procRef : ProcTypeDebug;
  BEGIN
    procRef := NEW(ProcTypeDebug, tUid := t, numFormals := n_formals, numRaises := n_raises, result := result, cc := cc, bitSize := ptrBits, align := ptrBits, formalIdx := 0, raisesIdx := 0);
    IF n_formals > 0 THEN
      procRef.formals := NEW(REF ARRAY OF FormalDebug,n_formals);
    END;
    IF n_raises > 0 THEN
      procRef.raises := NEW(REF ARRAY OF RaisesDebug,n_raises);
    END;
    EVAL self.debugTable.put(t,procRef);
    self.debugObj := procRef; (* keep for the formals and raises *)
  END declare_proctype;

PROCEDURE declare_formal (self: U; n: Name;  t: TypeUID; <*UNUSED*>typename: Name) =
  (* A formal parameter of a procedure type. *) 
  VAR
    procRef : ProcTypeDebug;
  BEGIN
    procRef := self.debugObj;
    <*ASSERT ISTYPE(procRef,ProcTypeDebug) *>
    procRef.formals[procRef.formalIdx] := NEW(FormalDebug, name := n, tUid := t);
    INC(procRef.formalIdx);
  END declare_formal;

PROCEDURE declare_raises (self: U; n: Name) =
  VAR
    procRef : ProcTypeDebug;
  BEGIN
    procRef := self.debugObj;
    <*ASSERT ISTYPE(procRef,ProcTypeDebug) *>
    procRef.raises[procRef.raisesIdx] := NEW(RaisesDebug, name := n);
    INC(procRef.raisesIdx);
  END declare_raises;

PROCEDURE declare_object (self: U; t, super: TypeUID; brand: TEXT;  traced: BOOLEAN; n_fields, n_methods: INTEGER; field_size: BitSize) =
  VAR
    objectRef : ObjectDebug;
  BEGIN
    objectRef := NEW(ObjectDebug, tUid :=t, superType := super, brand := brand, traced := traced, numFields := n_fields, numMethods := n_methods, fieldSize := VAL(field_size,LONGINT), bitSize := VAL(field_size,LONGINT), align := ptrBits);
    objectRef.fields := NEW(REF ARRAY OF FieldDebug,n_fields);
    objectRef.methods := NEW(REF ARRAY OF MethodDebug,n_methods);
    objectRef.fieldIndex := 0;
    objectRef.methodIndex := 0;
    EVAL self.debugTable.put(t,objectRef);
    self.debugObj := objectRef; (* keep for the fields and methods *)
  END declare_object;

PROCEDURE declare_method (self: U; n: Name;  signature: TypeUID) =
  VAR
    objectRef : ObjectDebug;
  BEGIN
    objectRef := self.debugObj;
    <*ASSERT ISTYPE(objectRef,ObjectDebug) *>
    objectRef.methods[objectRef.methodIndex] := NEW(MethodDebug, name := n, signature := signature);
    INC(objectRef.methodIndex);
  END declare_method;

PROCEDURE declare_opaque (self: U; t, super: TypeUID) =
  VAR
    opaqueRef : OpaqueDebug;
  BEGIN
    opaqueRef := NEW(OpaqueDebug, tUid := t, superType := super);
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
    <* ASSERT size = 16 OR size = 32 *> 
    self.widecharSize := size;
  END widechar_size;

(*--------------------------------------------------------- runtime hooks ---*)

PROCEDURE set_runtime_proc (self: U;  n: Name;  p: Proc) =
  VAR
    proc : LvProc;
  BEGIN
    (* declare a runtime proc *)
    proc := NARROW(p,LvProc);
    proc.state := procState.decld;
    self.buildFunc(p);
    IF Text.Equal(M3ID.ToText(n),"ReportFault") THEN
      (* save the fault proc *)
      self.abortFunc := proc.lvProc;
    END;
  END set_runtime_proc;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE import_global (self: U;  n: Name;  s: ByteSize;  a: Alignment; t: Type;  m3t: TypeUID): Var =
  VAR
    v : LvVar := NewVar(self,n,s,a,t,FALSE,m3t,TRUE,FALSE,FALSE,FALSE,M3CG.Maybe,VarType.Global);
    globName : TEXT;
  BEGIN
    IF v.name = M3ID.NoID THEN
      globName := "m3global_ext";
    ELSE
      globName := M3ID.ToText(v.name);
    END;

    v.lv := LLVM.LLVMAddGlobal(modRef, v.lvType, LT(globName));
    (* no initialisers for external globals *)
    LLVM.LLVMSetAlignment(v.lv, v.align);

    RETURN v;
  END import_global;

PROCEDURE declare_segment (self: U;  n: Name;  m3t: TypeUID; is_const: BOOLEAN): Var =
  VAR
    v : LvVar := NewVar(self,n,0,0,Type.Struct,is_const,m3t,TRUE,FALSE,FALSE,FALSE,M3CG.Maybe,VarType.Global);
    segName : TEXT;
  BEGIN
    IF is_const THEN
      (* the name will be nil so create a const name *)
      segName := "M_Const";
      v.name := M3ID.Add(segName); 
      (* keep the value for globals debugging *)
      self.segConst := v;
    ELSE
      segName := M3ID.ToText(n);
      (* RMB: The name of the global data segment is the only name we can depend 
         on getting to identify the module, so use it here. *) 
      IF modRef = NIL THEN 
         modRef := LLVM.LLVMModuleCreateWithNameInContext(LT(segName),globContext);
      END; 
      self.segVar := v;
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

PROCEDURE bind_segment (<*UNUSED*> self: U;  seg: Var;  s: ByteSize;  a: Alignment; <*UNUSED*> t: Type;  exported, inited: BOOLEAN) =
  VAR v : LvVar;
  BEGIN
    v := NARROW(seg,LvVar);
    v.align := a;
    v.size := s;
    v.exported := exported; (* check this in end_init *)
    v.inited := inited; (* not used yet *)
  END bind_segment;

PROCEDURE declare_global (self: U;  n: Name;  s: ByteSize;  a: Alignment; t: Type;  m3t: TypeUID;  exported, inited: BOOLEAN): Var =
  VAR
    v : LvVar := NewVar(self,n,s,a,t,FALSE,m3t,TRUE,FALSE,exported,inited,M3CG.Maybe,VarType.Global);
    globName : TEXT;
  BEGIN
    IF v.name = M3ID.NoID THEN
      globName := "m3global";
    ELSE
      globName := M3ID.ToText(v.name);
    END;

    IF inited THEN
      (* this global is more like a segment and can expect inits *)
      v.lvType := LLVM.LLVMStructCreateNamed(globContext, LT(globName & "_struct"));
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

(* Generate llvm code to allocate v in the current basic block. *) 
PROCEDURE AllocVar(<*UNUSED*>self : U; v : LvVar) =
  BEGIN
    v.lv := LLVM.LLVMBuildAlloca(builderIR, v.lvType, LT(VName(v)));
    LLVM.LLVMSetAlignment(v.lv,v.align);
  END AllocVar;
  
(* PRE: We are inside a procedure body, possibly deeply inside nested blocks. *)
(* Allocate a temp or local in the entry BB of the procedure. 
   It could be M3-coded and have a name, or be a true temp. *)
PROCEDURE AllocVarInEntryBlock(self : U; v : LvVar) =
  VAR
    curBB : LLVM.BasicBlockRef;
  BEGIN
    IF v.name = M3ID.NoID THEN 
      DebugClearLoc(self); (* suspend debugging of temp allocs *)
    END; 

    (* Position at end of entry BB. *)
    curBB := LLVM.LLVMGetInsertBlock(builderIR);
    LLVM.LLVMPositionBuilderAtEnd(builderIR, self.curProc.entryBB);

    (* alloc the variable in the entry BB *)
    self.allocVar(v);

    (* Back to regular insertion point. *) 
    LLVM.LLVMPositionBuilderAtEnd(builderIR, curBB);
    IF v.name = M3ID.NoID THEN 
      DebugLine(self); (* resume debugging *)
    END; 
  END AllocVarInEntryBlock;
  
PROCEDURE declare_local 
  (self: U;  n: Name;  s: ByteSize;  a: Alignment; t: Type;  m3t: TypeUID; 
    in_memory, up_level: BOOLEAN; f: Frequency): Var =
  VAR
    v : LvVar := NewVar
          (self,n,s,a,t,isConst:=FALSE,m3t:=m3t,in_memory:=in_memory,
           up_level:=up_level,exported:=FALSE,inited:=FALSE,frequency:=f,
           varType:=VarType.Local);
    proc : LvProc;
  BEGIN
    (* Locals are declared either within a procedure signature, i.e., after
       declare_procedure, or within a body, i.e., within a begin_procedure/
       end_procedure pair.  In the former case, we can't allocate them yet,
       so just save them in localStack, to be allocated in begin_procedure.
       In the latter case,  allocate them now.  
       Since begin_procedure implies a begin_block, checking for blockLevel > 0
       is sufficient to allocate now. *)
    proc := self.curLocalOwner;
    IF self.blockLevel = 0 THEN (* We are in a signature. *) 
    (* NOTE: If n is "_result", we are in the signature of a function procedure
             with a scalar result, and this is a compiler-generated local to 
             hold the result. *) 
        (* ^The proc belonging to the most recent declare_procedure. *)
      PushRev(proc.localStack, v); (* Left-to-right. *)  
      (* ^The local will be allocated later, in the proc body. *) 
      v.inProc := proc;
      IF up_level THEN
        v.locDisplayIndex := proc.uplevelRefdStack.size(); 
        PushRev(proc.uplevelRefdStack, v);
        INC(proc.cumUplevelRefdCt);
      END;
    ELSE (* We are in the body of the procedure. *) 
      <* ASSERT proc = self.curProc *> 
      self.allocVarInEntryBlock(v); 
        (* ^Which flattens it from an inner block into the locals of
            the containing proc. *) 
      v.inProc := proc;
      (* Could be up-level if M3 decl is in an inner block. *) 
      IF up_level THEN
        v.locDisplayIndex := self.curProc.uplevelRefdStack.size(); 
        PushRev(self.curProc.uplevelRefdStack, v);
        INC(proc.cumUplevelRefdCt);
      END;
      DebugVar(self, v);
    END;
    RETURN v;
  END declare_local;

PROCEDURE ImportedStructSize(self : U; m3t : TypeUID) : ByteSize =
  VAR
    typeObj : REFANY;
    tUidExists : BOOLEAN;
    size,baseSize : ByteSize := 0;
    tc : CARDINAL;
  BEGIN
    IF m3t = 0 THEN RETURN size; END;
    tUidExists := self.debugTable.get(m3t, (*OUT*)typeObj);
    <*ASSERT tUidExists *>
    tc := TYPECODE(typeObj);
    baseSize := VAL(NARROW(typeObj,BaseDebug).bitSize,INTEGER) DIV 8;
    IF tc = TYPECODE(RecordDebug) OR tc = TYPECODE(ArrayDebug) OR 
      (tc = TYPECODE(SetDebug) AND baseSize > ptrBytes) THEN
       size := baseSize;
(* REVIEW: This once also had "OR tc = TYPECODE(SetDebug)", but that 
           assert-failed on a SET OF a 7-member enumeration, in LLVMBuildCall, 
           over a type mismatch.  Could there be some SET cases (e.g., medium
           or large sets) where we need to do a type correction? *)
(* Re Review: Imported value sets less than word size are passed via pop-param
whereas larger sets behave like records and are structs. Added test case c142
to check this. Perhaps the front end could supply the correct type. *)           
    END;
    RETURN size;    
  END ImportedStructSize;

PROCEDURE declare_param (self: U;  n: Name;  s: ByteSize;  a: Alignment; t: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN; f: Frequency; <*UNUSED*>typename: Name): Var =
  (* A formal parameter of a procedure, not of a procedure type, (which
     is given by declare_formal). *) 
  VAR
    v : LvVar;
    proc : LvProc;
    size : ByteSize;
  BEGIN
    (* This appears after either import_procedure (which can occur inside
       the body of a different procedure, i.e., between begin_procedure and 
       end_procedure), or after declare_procedure.  Either way, the LvProc
       this parameter belongs to is self.curParamOwner. *)

    (* NOTE: If n is "_result", we are in the signature of a function procedure
             with a nonscalar result, and this is a compiler-generated VAR
             parameter used to return the result. *) 
    proc := self.curParamOwner; (* Get the current proc. *)
    
    IF proc.imported THEN
      (* Imported procs could have a value rec or array param classed as address
         param, but a call on it will use a pop_struct to pass the actual,   
         whereupon LLVMBuildCall will assert-fail with type mismatch.   
         Here we correct the param def if it is a struct by searching the 
         debug table for record or array *)
      size := ImportedStructSize(self,m3t);
      IF size > 0 THEN
        s := size;
        t := Type.Struct;
      END;
    END;
    v := NewVar
           (self,n,s,a,t,isConst:=FALSE,m3t:=m3t,in_memory:=in_memory,
            up_level:=up_level,exported:=FALSE,inited:=FALSE,frequency:=f,
            varType:=VarType.Param);
    
    PushRev(proc.paramStack, v); (* Left-to-right. *) 
    (* ^Postpone allocating and storing the formal until begin_procedure. *) 
    v.inProc := proc;
    IF up_level THEN
      v.locDisplayIndex := proc.uplevelRefdStack.size(); 
      PushRev(proc.uplevelRefdStack, v); (* Left-to-right. *)
      INC(proc.cumUplevelRefdCt);
    END;
    RETURN v;
  END declare_param;

PROCEDURE declare_temp (self: U; s: ByteSize; a: Alignment; t: Type; in_memory: BOOLEAN): Var =
  VAR
    v : LvVar := NewVar(self,M3ID.NoID,s,a,t,FALSE,0,in_memory,FALSE,FALSE,FALSE,M3CG.Maybe,VarType.Temp);
  BEGIN
    (* temps are always declared inside a begin_procedure. However we
       allocate them in the entry BB to avoid dominate all uses problems,
       also temps declared inside loops could overflow stack. *)  
    self.allocVarInEntryBlock(v);
    v.inProc := self.curProc;
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
   filler with each init_xx call so that we dont need to regenerate the inits
   here which would mean only looping through the inits once. *)
PROCEDURE end_init (self: U;  v: Var) =
  VAR
    baseObj : BaseVar;
    int,numGlobs,typeSize,thisOfs,fillLen,segSize : INTEGER;
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
         (* dont zero terminate the string *)
         v.lvVal := LLVM.LLVMConstString(LT(v.value),Text.Length(v.value),TRUE);
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

    (* if we dont add the final zero in to agree with the bind size
       the rtlinker crashes. *)
    IF thisOfs < thisVar.size THEN
      fillLen := thisVar.size - thisOfs;
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
    <*ASSERT segSize = thisVar.size *>

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
          v.lvVal := ConvertFloat(v.prec, v.value);
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
    DebugGlobals(self, thisVar);
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
    <*ASSERT FALSE *>
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
    <*ASSERT FALSE *>
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

PROCEDURE import_procedure (self: U;  n: Name;  n_params: INTEGER; return_type: Type;  cc: CallingConvention; <*UNUSED*>return_typeid: TypeUID; <*UNUSED*>return_typename: Name): Proc =
  VAR
    p : LvProc := NewProc(self,n,n_params,return_type,-1,cc,FALSE,NIL);
    name : TEXT;
    retTy : LLVM.TypeRef;
  BEGIN
    p.imported := TRUE;
    (* Don't need local stack or an up-level since its imported, but need a 
       paramstack. *)
    p.paramStack := NEW(RefSeq.T).init();

    (* create a dummy llvm proc which we can replace later if called and we
     have the params *)
    retTy := LLvmType(p.returnType);
    p.procTy := LLVM.LLVMFunctionType(retTy, NIL, 0, FALSE);

    name := M3ID.ToText(p.name);
    p.lvProc := LLVM.LLVMAddFunction(modRef, LT(name), p.procTy);

    p.state := procState.decld; 
    self.curParamOwner := p;
    (* ^Until further notice, occurences of declare_param belong to p. *)
(* REVIEW: Hopefully, a declare_local can't occur belonging to import_procedure.
   Otherwise, declare_local's ownership is ambiguous, since an
   import_procedure, with its signature items, can occur inside a procedure
   body, with its locals.  This is undocumented and hard to ferret out from CG.
   Sometimes, there are declare_local's interspersed with declar_param's in a
   signature, for certain, after declare_procedure.  Could this happen after an
   import_procedure?  When inside a body, it would be ambiguous which the parameter
   belonged to. *) 

    RETURN p;
  END import_procedure;

PROCEDURE declare_procedure (self: U;  n: Name;  n_params: INTEGER;
                             return_type: Type;  lev: INTEGER;
                             cc: CallingConvention; exported: BOOLEAN;
                             parent: Proc;
                             <*UNUSED*>return_typeid: TypeUID;
                             <*UNUSED*>return_typename: Name): Proc =
  VAR
    p : LvProc := NewProc(self,n,n_params,return_type,lev,cc,exported,parent);
  BEGIN
    p.imported := FALSE;
    p.localStack := NEW(RefSeq.T).init();
    p.paramStack := NEW(RefSeq.T).init();
    p.uplevelRefdStack := NEW(RefSeq.T).init();
    p.cumUplevelRefdCt := 0; (* This is not cumlative yet. *)
    p.state := procState.decld; 
    self.curParamOwner := p;
    self.curLocalOwner := p;
    (* ^Until further notice, both occurences of declare_param and of 
        declare_local belong to this procedure. *) 
    RETURN p;
  END declare_procedure;

PROCEDURE begin_procedure (self: U;  p: Proc) =
(* begin generating code for the body of procedure 'p'.  Sets "current procedure"
   to 'p'.  Implies a begin_block.  *)
  VAR
    local,param : LvVar;
    memVar : LvVar; 
    proc : LvProc;
    storeVal,lVal : LLVM.ValueRef;
    numParams,numLocals : CARDINAL;
    arg : REFANY;
    paramNo : INTEGER; 
  BEGIN
    (* Declare this procedure and all its locals and parameters.*)
    proc := NARROW(p,LvProc);

    (* create the function *)
    self.buildFunc(p);
    <* ASSERT proc.state = procState.built *>

    self.curProc := proc;

    (* Make proc.cumUplevelRefdCt cumulative. *) 
    IF proc.parent # NIL THEN 
      <* ASSERT proc.parent.state = procState.complete *>
      INC (proc.cumUplevelRefdCt, proc.parent.cumUplevelRefdCt);
    END; 

    proc.saveBB := LLVM.LLVMGetInsertBlock(builderIR);
    (* begin blocks can be nested so need to keep a stack of procedures so we
       are referring to the current proc for the BB's *)
    Push(self.procStack,proc);
    (* top of procStack is current proc *)

    numParams := proc.numParams;

    (* generate debug code for the function *)
    DebugFunc(self,p);
    (* set debug loc to nul here to run over prologue instructions *)
    DebugClearLoc(self);

    (* Create the entry and second basic blocks. *)
    proc.entryBB := LLVM.LLVMAppendBasicBlockInContext
                      (globContext, self.curProc.lvProc,  LT("entry"));
    (* ^For stuff we generate: alloca's, display build, etc. *) 
    proc.secondBB := LLVM.LLVMAppendBasicBlockInContext
                       (globContext, self.curProc.lvProc,  LT("second"));
    (* ^For m3-coded operations. *) 
    LLVM.LLVMPositionBuilderAtEnd(builderIR,proc.entryBB);

    (* Allocate and store parameters to memory. *)
    paramNo := 0; 
    lVal := LLVM.LLVMGetFirstParam(self.curProc.lvProc);

    (* If needed, allocate storedStaticLink, first alloca in AR. *)
    IF proc.staticLinkFormal # NIL (* i8* *) 
    THEN
      arg := Get(proc.paramStack,paramNo);
      param := NARROW(arg,LvVar);
      <* ASSERT param = proc.staticLinkFormal *> 
      memVar (* i8* *) 
        := NewVar (self,M3ID.Add("__StoredStaticLink"),size:=ptrBytes,
                   align:=ptrBytes,type:=Type.Addr, isConst:=TRUE,m3t:=UID_ADDR,
                   in_memory:=TRUE,up_level:=FALSE,exported:=FALSE,inited:=FALSE,
                   frequency:=M3CG.Maybe,varType:=VarType.Local );
      self.allocVar (memVar); (* memVar.lv is i8** *) 
      (* ^This will be the first alloca of the proc, so debugger can find it
         at fixed offset. *)
      storeVal := LLVM.LLVMBuildStore(builderIR, lVal, memVar.lv);
      LLVM.LLVMSetAlignment(storeVal,ptrBytes);
      proc.storedStaticLink := memVar;
      param.lv := memVar.lv; 
      paramNo := 1;
      lVal := LLVM.LLVMGetNextParam(lVal);
    END;

    (* Allocate the M3-declared params, except structs. *)
    WHILE paramNo < numParams DO
      arg := Get(proc.paramStack,paramNo);
      param := NARROW(arg,LvVar);
      param.lv := lVal; 
      IF param.type # Type.Struct THEN

        memVar
          := NewVar (self,param.name,ptrBytes,ptrBytes,
                     Type.Addr, TRUE,UID_ADDR,TRUE,FALSE,FALSE,FALSE,M3CG.Maybe,
                     VarType.Local );
        memVar.lvType := param.lvType;
        self.allocVar (memVar); (* memVar.lv is param.lvType* *) 
        (* do the stores for the parameters *)
        storeVal := LLVM.LLVMBuildStore(builderIR, lVal, memVar.lv);
        LLVM.LLVMSetAlignment(storeVal,param.align);
        param.lv := memVar.lv; 
      ELSE (* refer directly to the param *)
        param.lv := lVal;
      END;
      INC (paramNo);
      lVal := LLVM.LLVMGetNextParam(lVal);
    END;

    (* Allocate locals that were earlier declared in the signature. *)
    numLocals := proc.localStack.size();
    FOR i := 0 TO numLocals - 1 DO
      arg := Get(proc.localStack,i);
      local := NARROW(arg,LvVar);
      self.allocVar(local);
    END;

    (* A temporary placeholder for display, to be replaced at end_procedure.
       Do this unconditionally now, but we may delete it in end_procedure
       if it turns out not to be needed *)
    proc.outgoingDisplayLv (* i8** *)
      := LLVM.LLVMBuildAlloca
           (builderIR, AdrTy, LT("__TempDisplay"));
    (* Don't try to use a constant here.  Some of its uses would't be replaced
       by LLVMReplaceAllUsesWith.  Also, it will be uniqued, so genuine uses
       could be replaced incorrectly. *) 

    LLVM.LLVMPositionBuilderAtEnd(builderIR,proc.secondBB);
    (* ^This is where compiled-from-Modula3 code will be inserted. *) 
    self.curLocalOwner := p;
    (* ^Until further notice, occurences of declare_local belong to p. *) 
    self.begin_block();

    (* debug for locals and params here, need the stacks intact *)
    DebugLocalsParams(self,proc);
    proc.state := procState.begun;
  END begin_procedure;

PROCEDURE BuildDisplay(self : U; DisplayLv: LLVM.ValueRef) 
  : CARDINAL (* Display element count. *) =
(* Generate code, in the entry block of self.curProc, that will initialize the
   display area needed by calls to procedures immediately nested inside self.curProc. *)  
(* PRE: DisplayLv points to the place to build the display. *) 
  VAR
    v : LvVar;
    varLv,storeLv : LLVM.ValueRef;
    index : CARDINAL := 0;
    textName : TEXT;  

  PROCEDURE Recurse(ancestorProc: LvProc ) = 
  (* Do the ancestors outside-in, so a display works to pass to a procedure
     farther out than it was created for. *)
    BEGIN
      IF ancestorProc # NIL THEN
        Recurse (ancestorProc.parent); 
        IF ancestorProc.uplevelRefdStack # NIL THEN (* importeds are nil *)
          FOR i := 0 TO ancestorProc.uplevelRefdStack.size() - 1 DO
            v := Get(ancestorProc.uplevelRefdStack,i);
            textName := M3ID.ToText (v.name); 
            varLv (* i8* *) := GetAddrOfUplevelVar(self,v);
            storeLv (* i8** *)  
              := BuildDisplayGep (DisplayLv,
                           index , "__CopyToDisplaySlot.i8**");
            EVAL LLVM.LLVMBuildStore(builderIR,varLv,storeLv);
            INC(index);
          END;
        END;
      END;
    END Recurse; 

  BEGIN (*BuildDisplay*) 
    Recurse(self.curProc.parent); 
    (* ^First go through callee's proper static ancestors, outside inward. *) 
    IF self.curProc.uplevelRefdStack # NIL THEN (* importeds are nil *)
      FOR i := 0 TO self.curProc.uplevelRefdStack.size() - 1 DO
        v := Get(self.curProc.uplevelRefdStack,i);
        textName := M3ID.ToText (v.name); 
        varLv (* i8* *)
          := LLVM.LLVMBuildBitCast (builderIR,v.lv,AdrTy,LT(textName & ".addr"));
        storeLv (* i8** *)
          := BuildDisplayGep(DisplayLv,index,
                      "__NewDisplaySlot.i8**");

        storeLv (* i8** *)
          := LLVM.LLVMBuildBitCast
               (builderIR,storeLv,AdrAdrTy,LT(textName & ".display.bitcast"));
(* CHECK ^Why is this necessary? it seems storeLv would already have AdrAdrTy. *)
        EVAL LLVM.LLVMBuildStore(builderIR,varLv,storeLv);
        INC(index);
      END;
    END; 
    RETURN index;
  END BuildDisplay;

PROCEDURE end_procedure (self: U;  p: Proc) =
(* marks the end of the code for procedure 'p'.  Sets "current procedure"
   to NIL.  Implies an end_block.  *)
  VAR
    proc : LvProc;
    prevInstr : LLVM.ValueRef;
    opCode : LLVM.Opcode;
    curBB : LLVM.BasicBlockRef;
    newDisplayLv : LLVM.ValueRef;
    textName : TEXT;
    linkSize : CARDINAL;   
  BEGIN
    proc := NARROW(p,LvProc);
    <* ASSERT proc = self.curProc *> 
    <* ASSERT proc.state = procState.begun *> 

    curBB := LLVM.LLVMGetInsertBlock(builderIR);
    LLVM.LLVMPositionBuilderAtEnd(builderIR, proc.entryBB);

    (* Final setup of this proc's display, which it can pass to any 
       one-level more deeply nested procedure. *) 
    IF proc.needsDisplay THEN 
      (* ^proc contained a call on a deeper nested proc. *) 
      (* We need an llvm type for a local display area that this proc
         can pass as static link to deeper-nested procedures. *) 
      proc.displayLty := LLVM.LLVMArrayType(AdrTy,proc.cumUplevelRefdCt);
      textName := "__Display";
      newDisplayLv
        := LLVM.LLVMBuildAlloca
             (builderIR, proc.displayLty, LT(textName));
      newDisplayLv (* i8** *)
        := LLVM.LLVMBuildBitCast
             (builderIR,newDisplayLv,AdrAdrTy,
              LT(textName & ".i8**"));

      LLVM.LLVMReplaceAllUsesWith (proc.outgoingDisplayLv, newDisplayLv);
      linkSize := BuildDisplay(self, newDisplayLv);
    END;
    (* Remove the temporary proc.outgoingDisplayLv. *)
    LLVM.LLVMInstructionEraseFromParent(proc.outgoingDisplayLv);
    (* Here, proc.outgoingDisplayLv is dead. *) 

    (* Give entry BB a terminating  unconditional branch to secondBB. *) 
    EVAL LLVM.LLVMBuildBr(builderIR, proc.secondBB); 

    LLVM.LLVMPositionBuilderAtEnd(builderIR, curBB);
    (* ^Back to the regular code insertion site. *) 

    (* its possible a no return warning will generate an abort and no return
    but llvm has mandatory return so if last instruction is not a return
    then add a dummy one, could possibly add return to front end in this case *)
    curBB := LLVM.LLVMGetInsertBlock(builderIR);
    prevInstr := LLVM.LLVMGetLastInstruction(curBB);
    IF prevInstr # NIL THEN
      opCode := LLVM.LLVMGetInstructionOpcode(prevInstr);
(* CHECK: what about other terminators ? *)
      IF opCode # LLVM.Opcode.Ret THEN
        IF proc.returnType = Type.Void THEN
          EVAL LLVM.LLVMBuildRetVoid(builderIR);
        ELSE
          EVAL LLVM.LLVMBuildRet(builderIR,LLVM.LLVMGetUndef(LLvmType(proc.returnType)));
        END;
      END;
    END;

    self.curProc.state := procState.complete;
    Pop(self.procStack);
    IF self.procStack.size() > 0 THEN
      LLVM.LLVMPositionBuilderAtEnd(builderIR,proc.saveBB);
      self.curProc := Get(self.procStack);
    ELSE
      self.curProc := NIL;
    END;

    self.end_block();

  END end_procedure;

PROCEDURE begin_block (self: U) =
  BEGIN
    INC(self.blockLevel);
    DebugPushBlock(self);
  END begin_block;

PROCEDURE end_block (self: U) =
  BEGIN
    DEC(self.blockLevel);
    DebugPopBlock(self);
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

VAR
  tmpVal : LLVM.ValueRef;

BEGIN
  IF self.m3llvmDebugLev > 0 THEN
    IO.Put("Expr stack from " & from & " size ");
    IO.PutInt(self.exprStack.size());
    IO.Put("\n");
    (* if from = jump stacksize could be 0 or 1 *)
    FOR i := 0 TO self.exprStack.size() -1 DO
      tmpVal :=  NARROW(self.exprStack.get(i),LvExpr).lVal;
      LLVM.LLVMDumpValue(tmpVal);
    END;
    IO.Put("\nEnd stack dump\n");
  END; 
END DumpExprStack;

PROCEDURE set_label (self: U;  lab: Label;  barrier: BOOLEAN) =
(* define 'lab' to be at the current pc, if 'barrier', 'l' bounds an exception
   scope and no code is allowed to migrate past it. *)
  VAR
    curBB : LLVM.BasicBlockRef;
    label : LabelObj;
    branch : BranchObj;
    terminator : LLVM.ValueRef;
  BEGIN
    DumpExprStack(self,"set_label");

    curBB := LLVM.LLVMGetInsertBlock(builderIR);
    label := self.getLabel(lab,"label_");
    label.barrier := barrier;  (* not using barrier thus far *)

    IF label.cmpInstr # NIL THEN
      LLVM.LLVMMoveBasicBlockAfter(label.labBB,curBB);
    END;

    (* terminate the previous BB if it hasnt already been done *)
    terminator := LLVM.LLVMGetBasicBlockTerminator(curBB);
    IF terminator = NIL THEN
      LLVM.LLVMPositionBuilderAtEnd(builderIR,curBB);
      EVAL LLVM.LLVMBuildBr(builderIR,label.labBB);
      LLVM.LLVMPositionBuilderAtEnd(builderIR,label.labBB);
    END;

    (* check if need to fix previous jmp instructions *)
    <*ASSERT label.branchList # NIL *>
    FOR i := 0 TO label.branchList.size() - 1 DO
      branch := label.branchList.get(i);
      (* the terminate previous BB could have added a branch but this is the correct branch so delete the old one*)
      terminator := LLVM.LLVMGetBasicBlockTerminator(branch.branchBB);
      IF terminator # NIL THEN
        LLVM.LLVMInstructionEraseFromParent(terminator);
      END;
      LLVM.LLVMPositionBuilderAtEnd(builderIR,branch.branchBB);
      EVAL LLVM.LLVMBuildBr(builderIR,label.labBB);
    END;
    LLVM.LLVMPositionBuilderAtEnd(builderIR,label.labBB);
  END set_label;

PROCEDURE jump (self: U; lab: Label) =
  VAR
    labRef : REFANY;
    label : LabelObj;
    branch : BranchObj;
  BEGIN
    DumpExprStack(self,"jump");

    IF self.labelTable.get(lab,labRef) THEN
      label := NARROW(labRef,LabelObj);
    ELSE
      label := NEW(LabelObj, id := lab, branchList := NEW(RefSeq.T).init());
      EVAL self.labelTable.put(lab,label);
    END;

    branch := NEW(BranchObj);
    branch.branchBB := LLVM.LLVMGetInsertBlock(builderIR);
    (* add the branch to our label *)
    label.branchList.addlo(branch);
    IF label.labBB # NIL THEN
      (* must have seen the label at some point so insert the branch *)
      EVAL LLVM.LLVMBuildBr(builderIR,label.labBB);
    END;
    (* else the branches will be fixed up in set_label *)
  END jump;

PROCEDURE BuildCmp(self: U; a,b : LLVM.ValueRef; t: ZType; op : CompareOp; l: Label; <*UNUSED*> f : Frequency) =
  VAR
    cmpVal : LLVM.ValueRef;
    label : LabelObj;
  BEGIN
    cmpVal := CompareVal(a,b,op,t);

    label := self.getLabel(l,"if_");

    label.cmpInstr := cmpVal;
    label.elseBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT("else_" & Fmt.Int(l)));
    EVAL LLVM.LLVMBuildCondBr(builderIR,cmpVal,label.labBB, label.elseBB);
    (* if mdbuilder::createBranchWeights is an api and condbr is extended to
        have weights as the last parm then could use frequency to do this
        weights := LLVM.LLVMCreateBranchWeights(f, Frequency.Always - f);
        EVAL LLVM.LLVMBuildCondBr(builderIR,cmpVal,label.labBB, label.elseBB,weights);
    *)
    LLVM.LLVMPositionBuilderAtEnd(builderIR,label.elseBB);
  END BuildCmp;

PROCEDURE IfCommon(self: U;  t: IType;  l: Label; f: Frequency; op : CompareOp) =
  VAR
    s0 := Get(self.exprStack);
    a,b : LLVM.ValueRef;
    intType : LLVM.TypeRef;
  BEGIN
    intType := LLvmType(t);
    a := NARROW(s0,LvExpr).lVal;
    b := Zero(intType);
    BuildCmp(self,a,b,t,op,l,f);
    Pop(self.exprStack);
  END IfCommon;

PROCEDURE if_true (self: U;  t: IType;  l: Label;  f: Frequency) =
(* tmp := s0.t; pop; IF (tmp # 0) GOTO l *)
  BEGIN
    self.ifCommon(t,l,f,CompareOp.NE);
  END if_true;

PROCEDURE if_false (self: U;  t: IType;  l: Label;  f: Frequency) =
(* tmp := s0.t; pop; IF (tmp = 0) GOTO l *)
  BEGIN
    self.ifCommon(t,l,f,CompareOp.EQ);
  END if_false;

PROCEDURE if_compare (self: U;  t: ZType;  op: CompareOp;  l: Label; f: Frequency) =
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
    BuildCmp(self,a,b,t,op,l,f);
    Pop(self.exprStack,2);
  END if_compare;

(* get or create a new label and the BB that goes with it *)
PROCEDURE GetLabel(self : U; l : Label; name : TEXT) : LabelObj =
  VAR
    label : LabelObj;
    labRef : REFANY;
  BEGIN
    IF self.labelTable.get(l,labRef) THEN
      label := NARROW(labRef,LabelObj);
    ELSE
      label := NEW(LabelObj, id := l, branchList := NEW(RefSeq.T).init());
      EVAL self.labelTable.put(l,label);
    END;
    IF label.labBB = NIL THEN
      label.labBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT(name & Fmt.Int(l)));
    END;
    RETURN label;
  END GetLabel;

PROCEDURE case_jump (self: U; <*UNUSED*> t: IType;  READONLY labels: ARRAY OF Label) =
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
        (* truncate return val if needs be *)
        IF TypeSize(self.curProc.returnType) < TypeSize(t) THEN
          destTy := LLvmType(self.curProc.returnType);
          stackVal := LLVM.LLVMBuildTrunc(builderIR,stackVal,destTy, LT("exit_trunc"));
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
PROCEDURE Gep(src,ofs : LLVM.ValueRef; const : BOOLEAN; textName : TEXT := "") 
  : LLVM.ValueRef =
  CONST numParams = 1;
  VAR
    gepVal : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
  BEGIN
    paramsRef := NewValueArr(paramsArr,numParams);
    paramsArr[0] := ofs;
    (* Front end will always have precluded out-of-bounds references, so 
       we can assert inbounds to llvm. *)
    IF NOT const THEN
      gepVal := LLVM.LLVMBuildInBoundsGEP
                  (builderIR, src, paramsRef, numParams, LT(textName));
    ELSE (* Can't use textName here. *)
      gepVal := LLVM.LLVMConstInBoundsGEP(src, paramsRef, numParams);
    END;
    RETURN gepVal;
  END Gep;

PROCEDURE BuildGep(src : LLVM.ValueRef (* any* *) ; o : ByteOffset; textName : TEXT := "") 
  : LLVM.ValueRef (* i8* *) =
  VAR
    int8Val,gepVal,ofs : LLVM.ValueRef;
  BEGIN
    ofs := LLVM.LLVMConstInt(LLVM.LLVMInt64Type(), VAL(o,LONGINT), TRUE);
    (* cast to i8* value if not already *)
    int8Val := LLVM.LLVMBuildBitCast(builderIR, src, AdrTy, LT("_.gep_toAdr"));
    gepVal := Gep(int8Val,ofs,FALSE,textName);
    RETURN gepVal;
  END BuildGep;

PROCEDURE BuildDisplayGep
  (src : LLVM.ValueRef (* i8** *) ; o : ByteOffset; textName : TEXT := "")
(* Do the GEP entirely in i8**. *) 
  : LLVM.ValueRef (* i8** *) =
  VAR
    gepVal,ofs : LLVM.ValueRef;
  BEGIN
    ofs := LLVM.LLVMConstInt(LLVM.LLVMInt64Type(), VAL(o,LONGINT), TRUE);
    gepVal := Gep(src,ofs,FALSE,textName);
    RETURN gepVal;
  END BuildDisplayGep;

PROCEDURE BuildConstGep(src : LLVM.ValueRef; o : ByteOffset) : LLVM.ValueRef =
  VAR
    int8Val,gepVal,ofs : LLVM.ValueRef;
  BEGIN
    ofs := LLVM.LLVMConstInt(LLVM.LLVMInt64Type(), VAL(o,LONGINT), TRUE);
    int8Val := LLVM.LLVMConstBitCast(src, AdrTy);
    gepVal := Gep(int8Val,ofs,TRUE);
    RETURN gepVal;
  END BuildConstGep;

PROCEDURE GetAddrOfUplevelVar(self : U; var : LvVar)
: LLVM.ValueRef (* i8* *) =
(* Generate an llvm ValueRef that contains the address of 'var'.
   PRE: 'var' is being up-level referenced from within 'self.curProc', which 
        implies 'self.curProc' is nested, and thus has a staticLinkFormal. 
   PRE: The cumUplevelRefdCt fields of the proc containing the decl of the
   referenced variable and its ancestor procs are computed, which happens
   by the end of its body. *) 
  VAR
    displayPtrLv (* i8** *), lv (* i8*** *) : LLVM.ValueRef;
    displayIndex : INTEGER := -1;
    localto_Proc : LvProc; 
    textName : TEXT; 
  BEGIN
    <*ASSERT self.curProc.lev > 0 *>
    <*ASSERT var.locDisplayIndex >= 0 *> 
    IF var.name = M3ID.NoID THEN 
      textName := ""; 
(* CHECK: Is this case really necessary?  If so, we need it lots of other places too. *) 
    ELSE
      textName := M3ID.ToText ( var.name ); 
    END; 
    localto_Proc := var.inProc;
    <*ASSERT localto_Proc.state = procState.complete *> 
    displayIndex := var.locDisplayIndex;
    IF localto_Proc.parent # NIL
    THEN
      <* ASSERT localto_Proc.parent.state = procState.complete *> 
      INC (displayIndex, localto_Proc.parent.cumUplevelRefdCt) 
    END; 
    (* ^Index into the display. *) 
    (* Load the incoming static link pointer of the current procedure. *)
    <*ASSERT self.curProc.storedStaticLink.lv # NIL *>
    displayPtrLv (* i8* *) 
      := LLVM.LLVMBuildLoad
           (builderIR, self.curProc.storedStaticLink.lv, (* i8** *) 
            LT("stored_static_link"));
    displayPtrLv := LLVM.LLVMBuildBitCast (* i8** *)
      (builderIR, displayPtrLv, AdrAdrTy, LT("__static_link_to_use.i8**"));
    lv := BuildDisplayGep (* i8** *)
            (displayPtrLv, displayIndex, textName & "__RefDisplaySlotAddr");
    (* Now load the address of 'var' from the display. *)
    lv (* i8* *)
      := LLVM.LLVMBuildLoad (builderIR, lv, LT(textName & "__addr.i8*"));
    RETURN lv;
  END GetAddrOfUplevelVar;
  
PROCEDURE Extend(val : LLVM.ValueRef; t : MType; destTy : LLVM.TypeRef) : LLVM.ValueRef =
  BEGIN
    IF WordTypes(t) THEN
      val := LLVM.LLVMBuildZExt(builderIR,val,destTy, LT("zext"));
    ELSE
      val := LLVM.LLVMBuildSExt(builderIR,val,destTy, LT("sext"));
    END;
    RETURN val;
  END Extend;
  
PROCEDURE LoadExtend(val : LLVM.ValueRef; t : MType; u : ZType) : LLVM.ValueRef =
  VAR destTy : LLVM.TypeRef;
  BEGIN
    destTy := LLvmType(u);
    IF TypeSize(t) < TypeSize(u) THEN
      val := Extend(val,t,destTy);
    END;
    RETURN val;
  END LoadExtend;

(* consider adding range metadata if subrange - from debug info *)
PROCEDURE load (self: U;  v: Var;  o: ByteOffset;  t: MType;  u: ZType) =
(* push; s0.u := Mem [ ADR(v) + o ].t ; *)
  VAR
    src : LvVar;
    srcVal,destVal : LLVM.ValueRef;
    srcTy,srcPtrTy : LLVM.TypeRef;
    textName : TEXT; 
  BEGIN
    src := NARROW(v,LvVar);
    srcTy := LLvmType(t); 
    srcPtrTy := LLVM.LLVMPointerType(srcTy); (* .srcTy* *)
    srcVal := src.lv; (* .srcPtrTy *) 
    IF src.name = M3ID.NoID THEN textName := ""; 
(* CHECK: ^Is this case really necessary?  If so, we need it lots of other places too. *) 
    ELSE textName := M3ID.ToText ( src.name ); 
    END; 

    (* check if a static link var to load *)
    IF src.locDisplayIndex >= 0 THEN
      (* If var is nonlocal to this proc then use the static link. *)
      IF src.inProc # self.curProc THEN
        srcVal (* .i8* *) := GetAddrOfUplevelVar(self,src);
        srcVal := LLVM.LLVMBuildBitCast
                    (builderIR, srcVal, srcPtrTy, LT(textName & "__addr"));
      END;
    END;

    IF src.type = Type.Struct THEN
      IF o # 0 THEN
        (* srcVal is a pointer, of size ptrBytes, but o is a byte offset.  Must
           bitcast to i8*, do the GEP, and bitcast back. *) 
        srcVal := LLVM.LLVMBuildBitCast (builderIR, srcVal, AdrTy, LT(".i8*"));
        srcVal := BuildGep(srcVal,o,"_struct_member");
        srcVal := LLVM.LLVMBuildBitCast (builderIR, srcVal, srcPtrTy, LT(".VarType*"));
      END;
      srcVal 
        := LLVM.LLVMBuildBitCast
            (builderIR, srcVal, srcPtrTy, LT(textName & "__load"));
    END;

    destVal := LLVM.LLVMBuildLoad(builderIR, srcVal, VarName(v)); (* .srcTy *)
    LLVM.LLVMSetAlignment(destVal,src.align);

    (* only load 64 or 32 bit sizes *)
    destVal := LoadExtend(destVal,t,u);

    Push(self.exprStack,NEW(LvExpr,lVal := destVal));
  END load;
  
PROCEDURE store (self: U;  v: Var;  o: ByteOffset;  t: ZType;  u: MType) =
(* Mem [ ADR(v) + o ].u := s0.t; pop *)
  VAR
    s0 := Get(self.exprStack);
    src : LvExpr;
    dest : LvVar;
    srcTy,destTy,destPtrTy,destEltTy : LLVM.TypeRef;
    srcVal,destVal,storeVal : LLVM.ValueRef;
    textName: TEXT;
  BEGIN
    srcTy := LLvmType(t);
    destTy := LLvmType(u);
    destPtrTy := LLVM.LLVMPointerType(destTy);

    src := NARROW(s0,LvExpr);  
    dest := NARROW(v,LvVar); 
    srcVal := src.lVal; (* .srcTy *)
    destVal := dest.lv; (* .destPtrTy *) 

    IF dest.name = M3ID.NoID THEN textName := ""; 
(* CHECK: Is this case really necessary?  If so, we need it lots of other places too. *) 
    ELSE textName := M3ID.ToText ( dest.name ); 
    END; 

    IF TypeSize(u) # TypeSize(t) THEN
      IF TypeSize(u) < TypeSize(t) THEN
        srcVal := LLVM.LLVMBuildTrunc(builderIR,srcVal,destTy,
                                      LT(textName & "store_trunc"));
      ELSE
        srcVal := LLVM.LLVMBuildSExt(builderIR,srcVal,destTy,
                                      LT(textName & "store_sext"));
      END;
    END;

    (* check if storing into uplevel variable. *)
    IF dest.locDisplayIndex >= 0 THEN
      IF dest.inProc # self.curProc THEN
        destVal (* i8* *) := GetAddrOfUplevelVar(self,dest);
        destVal := LLVM.LLVMBuildBitCast
          (builderIR, destVal, destPtrTy, LT(textName & "storeDest"));
      END;
    END;

    IF o # 0 THEN
      (* destVal is a pointer, of size ptrBytes, but o is a byte offset.  Must
           bitcast to i8*, do the GEP, and bitcast back. *) 
        destVal := LLVM.LLVMBuildBitCast (builderIR, destVal, AdrTy, LT("storeBase.i8*"));
        destVal := BuildGep(destVal,o,"storeDest.i8*");
        destVal := LLVM.LLVMBuildBitCast (builderIR, destVal, destPtrTy, LT("storeDest"));
    END;

    IF dest.type = Type.Addr THEN
      (* Remove the first pointer from destination type. *)
      destEltTy := LLVM.LLVMGetElementType(LLVM.LLVMTypeOf(destVal));
      IF u = Type.Addr THEN      
        srcVal := LLVM.LLVMBuildBitCast(builderIR, srcVal, destEltTy, LT("store_val"));
      ELSE
        srcVal := LLVM.LLVMBuildIntToPtr(builderIR, srcVal, destEltTy, LT("store_val"));      
      END;
    ELSIF dest.type = Type.Struct THEN
      (* get pointer to u type, bit cast dest to that, then bitcast src to u type *)
      srcVal := LLVM.LLVMBuildBitCast(builderIR, srcVal, destTy, LT("store_srcptr"));
      destVal := LLVM.LLVMBuildBitCast(builderIR, destVal, destPtrTy, LT("store_destptr"));
    ELSE
      destVal := LLVM.LLVMBuildBitCast
        (builderIR, destVal, destPtrTy, LT("__store_shouldBeRedundant"));
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
    srcVal := srcVar.lv; (* LLVM.LLVMTypeOf(srcVal)* *)

    (* check if address of an uplevel variable to load *)
    IF srcVar.locDisplayIndex >= 0 THEN
      IF srcVar.inProc # self.curProc THEN
        srcVal (* i8* *) := GetAddrOfUplevelVar(self,srcVar);
      END;
    END;

    IF o # 0 THEN
      srcVal := BuildGep(srcVal,o);
    END;
(* CHECK: Do we really want to return an llvm i8*, or do we want a pointer
          to the llvm type of var? *) 
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

    IF t = Type.Addr THEN
      srcVal := LLVM.LLVMBuildBitCast(builderIR, srcVal, AdrTy, LT("store_ind_srcptr"));
    END;

    destVal := LLVM.LLVMBuildBitCast(builderIR, destVal, destPtrTy, LT("store_ind_destptr"));
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
(* FIXME: ^Cross compile problem here. *) 
    <*ASSERT res = TRUE *>
    lVal := LLVM.LLVMConstInt(intTy, VAL(int,LONGINT), TRUE);
    Push(self.exprStack,NEW(LvExpr,lVal := lVal));
  END load_integer;

PROCEDURE ConvertFloat(t : RType; f : Target.Float) : LLVM.ValueRef =
  VAR 
    result : LLVM.ValueRef;
    realTy : LLVM.TypeRef;
  BEGIN
    realTy := LLvmType(t);  
    IF t = Type.Reel OR t = Type.LReel THEN 
      result := LLVM.LLVMConstReal(realTy,FLOAT(f.fraction,LONGREAL));
    ELSE (*Type.XReel*)
      result := LLVM.LLVMConstRealOfString(realTy, LT(Fmt.Extended(f.fraction)));    
    END;
    RETURN result;
  END ConvertFloat;

PROCEDURE load_float (self: U;  t: RType;  READONLY f: Target.Float) =
  (*push; s0.t := f *)
  VAR lVal : LLVM.ValueRef;
  BEGIN
    lVal := ConvertFloat(t,f);    
    Push(self.exprStack,NEW(LvExpr,lVal := lVal));
  END load_float;

(*------------------------------------------------------------ arithmetic ---*)

TYPE
  BinOps = {add,sub,mul,div,mod,and,or,xor,shl,shr};
  BuildProc = PROCEDURE(B: LLVM.BuilderRef; LHS, RHS: LLVM.ValueRef;
                   Name: Ctypes.char_star): LLVM.ValueRef;

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
      (* consider adding fast math flags *)
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
      (* consider adding fast math flags *)
      cmpVal := LLVM.LLVMBuildFCmp(builderIR, topr, a, b, LT("fcmp"));
    END;
    RETURN cmpVal;
  END CompareVal;

PROCEDURE compare (self: U;  t: ZType; u: IType;  op: CompareOp) =
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
    opType := LLvmType(u);

    cmpVal := CompareVal(a,b,op,t);

    ite := NEW(ITEObj, cmpVal := cmpVal, opName := "cmp", opType := u, curObj := self).init();
    EVAL ite.block(One(opType),FALSE);
    res := ite.block(Zero(opType),TRUE);
    
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

PROCEDURE IntAbs(a : LLVM.ValueRef) : LLVM.ValueRef =
  VAR shiftLen,ashr,xor,res : LLVM.ValueRef;
  BEGIN
    shiftLen := LLVM.LLVMConstInt(IntPtrTy, ptrBits - 1L, TRUE);
    ashr := LLVM.LLVMBuildAShr(builderIR, a, shiftLen, LT("abs_ashr"));
    xor := LLVM.LLVMBuildXor(builderIR, a, ashr, LT("abs_xor"));
    res := LLVM.LLVMBuildNSWSub(builderIR, xor, ashr, LT("abs_sub"));
    RETURN res;
  END IntAbs;
  
PROCEDURE abs(self: U;  t: AType) =
  (* s0.t := ABS (s0.t) *)
  CONST numParams = 1;
  VAR
    s0 := Get(self.exprStack,0);
    opType : LLVM.TypeRef;
    a,res,fn : LLVM.ValueRef;
    intType : BOOLEAN;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
  BEGIN
    IF WordTypes(t) THEN RETURN; END;
    a := NARROW(s0,LvExpr).lVal;
    opType := LLvmType(t);
    intType := t < Type.Reel;

    IF intType THEN
      res := IntAbs(a);
    ELSE
      paramsRef := NewValueArr(paramsArr,numParams);
      paramsArr[0] := a;
      fn := RealIntrinsic(t, LLVM.M3Intrinsic.m3fabs);
      res := LLVM.LLVMBuildCall(builderIR, fn, paramsRef, numParams, LT("fabs"));
    END;
    NARROW(s0,LvExpr).lVal := res;
  END abs;
  
PROCEDURE MinMax (self: U;  t: ZType; doMin : BOOLEAN) =
  CONST numParams = 2;
  VAR
    s0 := Get(self.exprStack,0);
    s1 := Get(self.exprStack,1);
    a,b,res,fn : LLVM.ValueRef;
    shiftLen,diff,ashr,xor,and,da,db : LLVM.ValueRef;
    intType : BOOLEAN;
    m3id : LLVM.M3Intrinsic;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
    b := NARROW(s1,LvExpr).lVal;
    intType := t < Type.Reel;

    IF intType THEN
      IF doMin THEN da := a; db := b; ELSE da := b; db := a; END;
      shiftLen := LLVM.LLVMConstInt(IntPtrTy, ptrBits - 1L, TRUE);
      diff := LLVM.LLVMBuildNSWSub(builderIR, a, b, LT("mm_sub"));
      ashr := LLVM.LLVMBuildAShr(builderIR, diff, shiftLen, LT("mm_ashr"));
      xor := LLVM.LLVMBuildXor(builderIR, da, db, LT("mm_xor"));
      and := LLVM.LLVMBuildAnd(builderIR, xor, ashr, LT("mm_and"));
      res := LLVM.LLVMBuildXor(builderIR, and, db, LT("mm_res"));
    ELSE
      paramsRef := NewValueArr(paramsArr,numParams);
      paramsArr[0] := a;
      paramsArr[1] := b;
      IF doMin THEN
        m3id := LLVM.M3Intrinsic.m3minnum;
      ELSE
        m3id := LLVM.M3Intrinsic.m3maxnum;
      END;
      fn := RealIntrinsic(t, m3id);
      res := LLVM.LLVMBuildCall(builderIR, fn, paramsRef, numParams, LT("fminmax"));
    END;  
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

PROCEDURE IntrinsicRealTypes(t : RType) : UNTRACED REF LLVM.TypeRef =
  VAR
    typesArr : TypeArrType;
    typesRef : TypeRefType;
  BEGIN
    typesRef := NewTypeArr(typesArr,1);
    CASE t OF
    |  Type.Reel  => typesArr[0] := LLVM.LLVMFloatType();
    |  Type.LReel => typesArr[0] := LLVM.LLVMDoubleType();
    |  Type.XReel => typesArr[0] := ExtendedType;
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
  VAR
    memsetId := LLVM.GetM3IntrinsicId(LLVM.M3Intrinsic.m3memset);    
    (* 2 types for overloaded memset *)
    Types := IntrinsicMemTypes(AdrTy,IntPtrTy,NIL);
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,memsetId,Types,2);
  END MemSetFn;

PROCEDURE MemCopyFn() : LLVM.ValueRef =
  VAR
    memcpyId := LLVM.GetM3IntrinsicId(LLVM.M3Intrinsic.m3memcpy);    
    (* 3 types for overloaded memcpy *)
    Types := IntrinsicMemTypes(AdrTy,AdrTy,IntPtrTy);
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,memcpyId,Types,3);
  END MemCopyFn;

PROCEDURE MemMoveFn() : LLVM.ValueRef =
  VAR
    memmovId := LLVM.GetM3IntrinsicId(LLVM.M3Intrinsic.m3memmov);  
    (* 3 types for overloaded memmov *)
    Types := IntrinsicMemTypes(AdrTy,AdrTy,IntPtrTy);
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,memmovId,Types,3);
  END MemMoveFn;

PROCEDURE RealIntrinsic(t : RType; m3id : LLVM.M3Intrinsic) : LLVM.ValueRef =
  VAR
    id := LLVM.GetM3IntrinsicId(m3id);
    types := IntrinsicRealTypes(t);
  BEGIN
    RETURN LLVM.LLVMGetDeclaration(modRef,id,types,1);
  END RealIntrinsic;
  
PROCEDURE DoCvtInt(var : LLVM.ValueRef; op : ConvertOp; t : RType) : LLVM.ValueRef =
  CONST numParams = 1;
  VAR
    res,fn : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
    m3id : LLVM.M3Intrinsic;    
  BEGIN
    paramsRef := NewValueArr(paramsArr,numParams);
    paramsArr[0] := var;
    CASE op OF
    | ConvertOp.Round => m3id := LLVM.M3Intrinsic.m3round;
    | ConvertOp.Trunc => m3id := LLVM.M3Intrinsic.m3trunc;
    | ConvertOp.Floor => m3id := LLVM.M3Intrinsic.m3floor;
    | ConvertOp.Ceiling => m3id := LLVM.M3Intrinsic.m3ceil;
    END;    
    fn := RealIntrinsic(t, m3id);
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
     if quotient < 0 then
        add fixup to div or mod
*)

PROCEDURE GenDivMod(self : U; t : IType; isDiv : BOOLEAN; numVal,denVal,divVal,modVal,fixVal : LLVM.ValueRef) : LLVM.ValueRef =
  VAR
    curBB,thenBB,elseBB,exitBB : LLVM.BasicBlockRef;
    cmpVal,storeVal,res : LLVM.ValueRef;
    opType : LLVM.TypeRef;
    tmpVar : LvVar;
    size : CARDINAL;    
  BEGIN
    opType := LLvmType(t);
    size := TypeSize(t);
    IF isDiv THEN storeVal := divVal; ELSE storeVal := modVal; END;

    curBB := LLVM.LLVMGetInsertBlock(builderIR);    
    tmpVar := self.declare_temp (size, size, t, TRUE);
    res := LLVM.LLVMBuildStore(builderIR, storeVal, tmpVar.lv);

    thenBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT("divmod_then"));
    elseBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT("divmod_else"));
    exitBB := LLVM.LLVMAppendBasicBlock(self.curProc.lvProc, LT("divmod_end"));
    LLVM.LLVMPositionBuilderAtEnd(builderIR,curBB);
    (* check if mod is zero *)
    cmpVal := LLVM.LLVMBuildICmp(builderIR,  LLVM.IntPredicate.EQ, modVal, Zero(opType), LT("divmod_cmp"));

    EVAL LLVM.LLVMBuildCondBr(builderIR,cmpVal,exitBB,thenBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,thenBB);
    
    (* check if quotient < 0 *)
    res := LLVM.LLVMBuildXor(builderIR, numVal, denVal, LT("divmod_xor"));
    cmpVal := LLVM.LLVMBuildICmp(builderIR,  LLVM.IntPredicate.SGE, res, Zero(opType), LT("divmod_cmp"));

    EVAL LLVM.LLVMBuildCondBr(builderIR,cmpVal,exitBB,elseBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,elseBB);
    (* add fix to div or mod *)
    res := LLVM.LLVMBuildNSWAdd(builderIR, storeVal, fixVal, LT("divmod_add"));
    res := LLVM.LLVMBuildStore(builderIR, res, tmpVar.lv);

    EVAL LLVM.LLVMBuildBr(builderIR,exitBB);
    LLVM.LLVMPositionBuilderAtEnd(builderIR,exitBB);
    
    res := LLVM.LLVMBuildLoad(builderIR, tmpVar.lv, LT("divmod_load"));
    RETURN res;
  END GenDivMod;

PROCEDURE DivMod(self : U; t : IType; isDiv : BOOLEAN) : LLVM.ValueRef=
  VAR
    res,fixup : LLVM.ValueRef;
    divRes,modRes,num,den : LLVM.ValueRef;
  BEGIN
    (* save numerator and denominator *)
    num := NARROW(Get(self.exprStack,1),LvExpr).lVal;
    den := NARROW(Get(self.exprStack,0),LvExpr).lVal;
    (* do a div first *)
    binop(self,t,BinOps.div);
    divRes := NARROW(Get(self.exprStack,0),LvExpr).lVal;
    (* restore stack for mod *)
    Pop(self.exprStack);
    Push(self.exprStack,NEW(LvExpr,lVal := num));
    Push(self.exprStack,NEW(LvExpr,lVal := den));
    (* do the mod *)
    binop(self,t,BinOps.mod);
    modRes := NARROW(Get(self.exprStack,0),LvExpr).lVal;
    IF isDiv THEN
      (* adjust div for neg operands *)
      fixup := LLVM.LLVMConstInt(LLvmType(t), VAL(-1,LONGINT), TRUE);
    ELSE
      (* same for mod *)
      fixup := den;
    END;
    res := GenDivMod(self, t, isDiv, num, den, divRes, modRes, fixup);
    RETURN res;
  END DivMod;

PROCEDURE div (self: U;  t: IType; a, b: Sign) =
 (* s1.t := s1.t DIV s0.t;pop*)
  VAR
    s0 : REFANY;
    res : LLVM.ValueRef;
  BEGIN
    IF (a = Sign.Positive AND b = Sign.Positive) OR
       (a = Sign.Negative AND b = Sign.Negative) THEN
      binop(self,t,BinOps.div);
    ELSE
      res := DivMod(self,t,TRUE);
      s0 := Get(self.exprStack,0);
      NARROW(s0,LvExpr).lVal := res;
    END;
  END div;

PROCEDURE mod (self: U;  t: IType; a, b: Sign) =
 (* s1.t := s1.t MOD s0.t;pop*)
  VAR
    s0 : REFANY;
    res : LLVM.ValueRef;
  BEGIN
    IF (a = Sign.Positive AND b = Sign.Positive) OR
       (a = Sign.Negative AND b = Sign.Negative) THEN
      binop(self,t,BinOps.mod);
    ELSE  
      res := DivMod(self,t,FALSE);
      s0 := Get(self.exprStack,0);
      NARROW(s0,LvExpr).lVal := res;
    END;
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

PROCEDURE SetBinopCommon
  (self : U; name : TEXT; VAR fn : LLVM.ValueRef; s : ByteSize) =
VAR
  s0,s1,s2,sizeVal : LLVM.ValueRef;
  BEGIN
    fn := DeclSet(name,fn,4,FALSE);
    sizeVal := LLVM.LLVMConstInt(IntPtrTy, VAL(s * 8,LONGINT), TRUE);
    GetSetStackVals(self,TRUE,s0,s1,s2);
    EVAL SetCall(fn,4,sizeVal,s0,s1,s2);
    Pop(self.exprStack,3);
  END SetBinopCommon;

PROCEDURE set_union (self: U;  s: ByteSize) =
  (* s2.B := s1.B + s0.B; pop(3) *)
  BEGIN
    SetBinopCommon(self,"set_union",self.setUnion,s);
  END set_union;

PROCEDURE set_difference (self: U;  s: ByteSize) =
  (* s2.B := s1.B - s0.B; pop(3) *)
  BEGIN
    SetBinopCommon(self,"set_difference",self.setDifference,s);
  END set_difference;

PROCEDURE set_intersection (self: U; s: ByteSize) =
  (* s2.B := s1.B * s0.B; pop(3) *)
  BEGIN
   SetBinopCommon(self,"set_intersection",self.setIntersection,s);
  END set_intersection;

PROCEDURE set_sym_difference (self: U; s: ByteSize) =
  (* s2.B := s1.B / s0.B; pop(3) *)
  BEGIN
    SetBinopCommon(self,"set_sym_difference",self.setSymDifference,s);
  END set_sym_difference;

PROCEDURE set_member (self: U; <*UNUSED*> s: ByteSize; <*UNUSED*> t: IType) =
  (* s1.t := (s0.t IN s1.B); pop *)
  VAR
    st0 := Get(self.exprStack,0);
    st1 := Get(self.exprStack,1);
    s0,s1,res : LLVM.ValueRef;
  BEGIN
    s0 := NARROW(st0,LvExpr).lVal;
    s1 := NARROW(st1,LvExpr).lVal;
    s1 := LLVM.LLVMBuildBitCast(builderIR, s1, PtrTy, LT("set_toptr"));
    self.setMember := DeclSet("set_member",self.setMember,2,TRUE);
    res := SetCall(self.setMember,2,s0,s1);
    NARROW(st1,LvExpr).lVal := res;
    Pop(self.exprStack);
  END set_member;

PROCEDURE SetCompareCommon 
  (self: U;  s: ByteSize; VAR fn : LLVM.ValueRef; name: TEXT) =
  (* s1.t := (fn(s1.B, s0.B)); pop *)
  VAR
    s0,s1,s2,res : LLVM.ValueRef;
    st1 := Get(self.exprStack,1);
    size : LLVM.ValueRef;
  BEGIN
    size := LLVM.LLVMConstInt(IntPtrTy, VAL(s * 8,LONGINT), TRUE);
    fn := DeclSet(name,fn,3,TRUE);
    GetSetStackVals(self,FALSE,s0,s1,s2);
    res := SetCall(fn,3,size,s0,s1);
    NARROW(st1,LvExpr).lVal := res;
    Pop(self.exprStack);
  END SetCompareCommon;

PROCEDURE set_compare 
 (self: U;  s: ByteSize;  op: CompareOp; <*UNUSED*> t: IType) =
  (* s1.t := (s1.B op s0.B); pop *)
  BEGIN
    CASE op OF
    | CompareOp.EQ 
    => SetCompareCommon(self, s, fn := self.setEq, name := "set_eq");
    | CompareOp.NE 
    => SetCompareCommon(self, s, fn := self.setNe, name := "set_ne");
    | CompareOp.GT 
    => SetCompareCommon(self, s, fn := self.setGt, name := "set_gt");
    | CompareOp.GE 
    => SetCompareCommon(self, s, fn := self.setGe, name := "set_ge");
    | CompareOp.LT 
    => SetCompareCommon(self, s, fn := self.setLt, name := "set_lt");
    | CompareOp.LE 
    => SetCompareCommon(self, s, fn := self.setLe, name := "set_le");
    END;
  END set_compare;

PROCEDURE set_range (self: U; <*UNUSED*> s: ByteSize; <*UNUSED*> t: IType) =
(* s2.A[s1.t..s0.t] := 1; pop(3) *)
  VAR
    s0,s1,s2 : LLVM.ValueRef;
    st0 := Get(self.exprStack,0);
    st1 := Get(self.exprStack,1);
    st2 := Get(self.exprStack,2);
  BEGIN
    s0 := NARROW(st0,LvExpr).lVal;
    s1 := NARROW(st1,LvExpr).lVal;
    s2 := NARROW(st2,LvExpr).lVal;
    s2 := LLVM.LLVMBuildBitCast(builderIR, s2, PtrTy, LT("set_toptr"));
    self.setRange := DeclSet("set_range",self.setRange,3,TRUE,TRUE);
    EVAL SetCall(self.setRange,3,s0,s1,s2);
    Pop(self.exprStack,3);
  END set_range;

PROCEDURE set_singleton (self: U; <*UNUSED*> s: ByteSize; <*UNUSED*> t: IType) =
 (* s1.A [s0.t] := 1; pop(2) *)
  VAR
    st0 := Get(self.exprStack,0);
    st1 := Get(self.exprStack,1);
    s0,s1 : LLVM.ValueRef;
  BEGIN
    s0 := NARROW(st0,LvExpr).lVal;
    s1 := NARROW(st1,LvExpr).lVal;
    s1 := LLVM.LLVMBuildBitCast(builderIR, s1, PtrTy, LT("set_toptr"));
    self.setSingleton := DeclSet("set_singleton",self.setSingleton,2,FALSE);
    EVAL SetCall(self.setSingleton,2,s0,s1);
    Pop(self.exprStack,2);
  END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not (self: U; <*UNUSED*> t: IType) =
  (* s0.t := Word.Not (s0.t) *)
  VAR
    s0 := Get(self.exprStack,0);
    a,lVal : LLVM.ValueRef;
  BEGIN
    a := NARROW(s0,LvExpr).lVal;
    lVal := LLVM.LLVMBuildNot(builderIR, a, LT("not"));
    NARROW(s0,LvExpr).lVal := lVal;
  END not;

PROCEDURE and (self: U; t: IType) =
  BEGIN
    binop(self,t,BinOps.and);
  END and;

PROCEDURE or (self: U; t: IType) =
  BEGIN
    binop(self,t,BinOps.or);
  END or;

PROCEDURE xor (self: U; t: IType) =
  BEGIN
    binop(self,t,BinOps.xor);
  END xor;
  
PROCEDURE shift (self: U; t: IType) =
  (* s1.t := Word.Shift  (s1.t, s0.t); pop *)
  VAR
    s0 := Get(self.exprStack,0);
    s1 := Get(self.exprStack,1);
    a,shift,cmpVal,cmpVal2,res,res2,absShift,shiftLen : LLVM.ValueRef;
    opType : LLVM.TypeRef;
    ite,ite2 : ITEObj;
    shiftBits : LONGINT;
  BEGIN
    a := NARROW(s1,LvExpr).lVal;
    shift := NARROW(s0,LvExpr).lVal;
    opType := LLvmType(t);
    (* avoid branches if shift is constant *)
    IF LLVM.LLVMIsConstant(shift) THEN
      shiftBits := LLVM.LLVMConstIntGetSExtValue(shift);
      IF ABS(shiftBits) >= ptrBits THEN
        res := Zero(opType);
      ELSE
        IF shiftBits >= 0L THEN
          res := LLVM.LLVMBuildShl(builderIR, a, shift, LT("shl"));
        ELSE
          absShift := LLVM.LLVMConstInt(opType, -shiftBits, TRUE);
          res := LLVM.LLVMBuildLShr(builderIR, a, absShift, LT("shr"));
        END;
      END;
    ELSE
      (* generate runtime check for range *)
      absShift := IntAbs(shift);
      shiftLen := LLVM.LLVMConstInt(IntPtrTy, ptrBits, TRUE);
      (* check if shift out of range *)
      cmpVal := LLVM.LLVMBuildICmp(builderIR,  LLVM.IntPredicate.SGE, absShift, shiftLen, LT("shift_abs_cmp"));
      ite := NEW(ITEObj, cmpVal := cmpVal, opName := "shift_abs", opType := t, curObj := self).init();

      res := Zero(opType);
      EVAL ite.block(res,FALSE);
        (* nested ite to check which shift left or right *)
        cmpVal2 := LLVM.LLVMBuildICmp(builderIR,  LLVM.IntPredicate.SGE, shift, Zero(opType), LT("shift_cmp"));
        ite2 := NEW(ITEObj, cmpVal := cmpVal2, opName := "shift", opType := t, beforeBB := ite.exitBB, curObj := self).init();
        res2 := LLVM.LLVMBuildShl(builderIR, a, absShift, LT("shl"));
        EVAL ite2.block(res2,FALSE);
        res2 := LLVM.LLVMBuildLShr(builderIR, a, absShift, LT("shr"));
        res := ite2.block(res2,TRUE);
      res := ite.block(res,TRUE);
    END;
    NARROW(s1,LvExpr).lVal := res;
    Pop(self.exprStack);
  END shift;

PROCEDURE shift_left (self: U; t: IType) =
  (* s1.t := Word.Shift  (s1.t, s0.t); pop *)
  BEGIN
    binop(self,t,BinOps.shl);
  END shift_left;

PROCEDURE shift_right (self: U; t: IType) =
  (* s1.t := Word.Shift  (s1.t, -s0.t); pop *)
  BEGIN
    binop(self,t,BinOps.shr);
  END shift_right;

(* rotate pseudo
rotl( unsigned int value, int shift)
return (value << shift) | (value >> (sizeof(value) * CHAR_BIT - shift));

rotr(unsigned int value, int shift)
return (value >> shift) | (value << (sizeof(value) * CHAR_BIT - shift));
*)

PROCEDURE DoRotate(value,shift : LLVM.ValueRef; rotLeft : BOOLEAN) : LLVM.ValueRef =
  VAR
    t1,t2,t3,t4,wordSize,maskSize : LLVM.ValueRef;
  BEGIN
    (* avoid undefined behaviour by masking out rotates greater then
     word length. This pattern is optimised to a single rotate instruction
     on some architectures. *)
    maskSize := LLVM.LLVMConstInt(IntPtrTy, ptrBits - 1L, TRUE);
    shift := LLVM.LLVMBuildAnd(builderIR, maskSize, shift, LT("rmask"));
    wordSize := LLVM.LLVMConstInt(IntPtrTy, ptrBits, TRUE);
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
    a,rot,cmpVal,res : LLVM.ValueRef;
    opType : LLVM.TypeRef;
    ite : ITEObj;
    rotBits : LONGINT;
  BEGIN
    a := NARROW(s1,LvExpr).lVal;
    rot := NARROW(s0,LvExpr).lVal;
    opType := LLvmType(t);
    (* avoid branches if rotate is constant *)
    IF LLVM.LLVMIsConstant(rot) THEN
      rotBits := LLVM.LLVMConstIntGetSExtValue(rot);
      IF rotBits >= 0L THEN
        res := DoRotate(a,rot,TRUE);
      ELSE
        rot := LLVM.LLVMConstInt(opType, -rotBits, TRUE);
        res := DoRotate(a,rot,FALSE);
      END;
    ELSE
      (* generate runtime check for rotate *)
      cmpVal := LLVM.LLVMBuildICmp(builderIR,  LLVM.IntPredicate.SGE, rot, Zero(opType), LT("rotate_cmp"));

      ite := NEW(ITEObj, cmpVal := cmpVal, opName := "rotate", opType := t, curObj := self).init();
      res := DoRotate(a,rot,TRUE);
      EVAL ite.block(res,FALSE);
      (* make the rotate positive  *)
      rot := LLVM.LLVMBuildNSWNeg(builderIR, rot, LT("neg"));
      res := DoRotate(a,rot,FALSE);
      res := ite.block(res,TRUE);
    END;
    NARROW(s1,LvExpr).lVal := res;
    Pop(self.exprStack);
  END rotate;

PROCEDURE rotate_left (self: U; <*UNUSED*> t: IType) =
  (* s1.t := Word.Rotate (s1.t, s0.t); pop *)
  VAR
    s0 := Get(self.exprStack,0);
    s1 := Get(self.exprStack,1);
    a,rot,res : LLVM.ValueRef;
  BEGIN
    a := NARROW(s1,LvExpr).lVal;
    rot := NARROW(s0,LvExpr).lVal;
    res := DoRotate(a,rot,TRUE);
    NARROW(s1,LvExpr).lVal := res;
    Pop(self.exprStack);
  END rotate_left;

PROCEDURE rotate_right (self: U; <*UNUSED*> t: IType) =
  (* s1.t := Word.Rotate (s1.t, -s0.t); pop *)
  VAR
    s0 := Get(self.exprStack,0);
    s1 := Get(self.exprStack,1);
    a,rot,res : LLVM.ValueRef;
  BEGIN
    a := NARROW(s1,LvExpr).lVal;
    rot := NARROW(s0,LvExpr).lVal;
    res := DoRotate(a,rot,FALSE);
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
    wordSize := LLVM.LLVMConstInt(IntPtrTy, ptrBits, TRUE);
    t1 := LLVM.LLVMBuildNSWSub(builderIR, wordSize, count, LT("elen"));
    t2 := LLVM.LLVMBuildNSWSub(builderIR, t1, offset, LT("edist"));
    t3 := LLVM.LLVMBuildShl(builderIR, val, t2, LT("eshl"));
    IF sign THEN
      t4 := LLVM.LLVMBuildAShr(builderIR, t3, t1, LT("eshr"));
    ELSE
      t4 := LLVM.LLVMBuildLShr(builderIR, t3, t1, LT("eshr"));
    END;    
    RETURN t4;
  END DoExtract;

PROCEDURE extract (self: U;  t: IType;  sign: BOOLEAN) =
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
    ite := NEW(ITEObj, cmpVal := cmpVal, opName := "extract", opType := t, curObj := self).init();
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

PROCEDURE insert (self: U; <*UNUSED*> t: IType) =
  (* s3.t := Word.Insert (s3.t, s2.t, s1.t, s0.t); pop(3) *)
  VAR
    s0 := Get(self.exprStack,0);
    s1 := Get(self.exprStack,1);
    s2 := Get(self.exprStack,2);
    s3 := Get(self.exprStack,3);
    value,target,offset,mask,ones,res,count : LLVM.ValueRef;
  BEGIN
    value := NARROW(s2,LvExpr).lVal;
    offset := NARROW(s1,LvExpr).lVal;
    count := NARROW(s0,LvExpr).lVal;
    target := NARROW(s3,LvExpr).lVal;
    ones := LLVM.LLVMConstAllOnes(IntPtrTy);
    ones := LLVM.LLVMBuildShl(builderIR, ones, count, LT("allones"));
    mask := LLVM.LLVMBuildNot(builderIR, ones, LT("masknot"));
    res := DoInsert(value,target,mask,offset);
    NARROW(s3,LvExpr).lVal := res;
    Pop(self.exprStack,3);
  END insert;

PROCEDURE insert_n (self: U; t: IType; n: CARDINAL) =
  (* s2.t := Word.Insert (s2.t, s1.t, s0.t, n); pop(2) *)
  VAR
    s0 := Get(self.exprStack,0);
    s1 := Get(self.exprStack,1);
    s2 := Get(self.exprStack,2);
    value,target,offset,mask,res : LLVM.ValueRef;
    maskTy,intTy : LLVM.TypeRef;
  BEGIN
    IF n > 0 THEN
      intTy := LLvmType(t);    
      value := NARROW(s1,LvExpr).lVal;
      offset := NARROW(s0,LvExpr).lVal;
      target := NARROW(s2,LvExpr).lVal;
      maskTy := LLVM.LLVMIntType(n);
      mask := LLVM.LLVMConstAllOnes(maskTy);
      mask := LLVM.LLVMConstZExtOrBitCast(mask, intTy);      
      res := DoInsert(value,target,mask,offset);
      NARROW(s2,LvExpr).lVal := res;
    END;
    (* else n = 0 is a noop *)
    Pop(self.exprStack,2);
  END insert_n;

PROCEDURE insert_mn (self: U; t: IType; m,n : CARDINAL) =
  (* s1.t := Word.Insert (s1.t, s0.t, m, n); pop(1) *)
  VAR
    s0 := Get(self.exprStack,0);
    s1 := Get(self.exprStack,1);
    value,target,offset,mask,res : LLVM.ValueRef;
    maskTy,intTy : LLVM.TypeRef;
  BEGIN
    IF n > 0 THEN
      intTy := LLvmType(t);    
      value := NARROW(s0,LvExpr).lVal;
      target := NARROW(s1,LvExpr).lVal;
      offset := LLVM.LLVMConstInt(IntPtrTy, VAL(m,LONGINT), TRUE);
      maskTy := LLVM.LLVMIntType(n);
      mask := LLVM.LLVMConstAllOnes(maskTy);
      mask := LLVM.LLVMConstZExtOrBitCast(mask, intTy);      
      res := DoInsert(value,target,mask,offset);
      NARROW(s1,LvExpr).lVal := res;
    END;
    (* else n = 0 is a noop *)
    Pop(self.exprStack);
  END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE swap (self: U;  <*UNUSED*> a, b: Type) =
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

PROCEDURE pop (self: U; <*UNUSED*> t: Type) =
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

PROCEDURE copy_n (self: U; <*UNUSED*> u: IType;  t: MType;  overlap: BOOLEAN) =
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

PROCEDURE copy (self: U;  n: INTEGER;  t: MType;  overlap: BOOLEAN) =
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

PROCEDURE zero_n (self: U; <*UNUSED*> u: IType; t: MType) =
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

PROCEDURE zero (self: U;  n: INTEGER; t: MType) =
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

(* The cm3 loophole IR operator was misdocumented as equivalent to Modula-3 
   LOOPHOLE, but this is not so.  

   LOOPHOLE always requires equal sizes, but the front end emits loophole 
   operators that call for various conversions involving different integer, 
   address and real sizes and different signednesses.  

   Inferring the complete specification of the loophole operator is difficult
   and probably ambiguous, and involves extensive vetting of the front end and
   all back ends.  This implementation supports a very liberal set of conversions.
   Hopefully, they do what is right, at least for all the cases that can happen.

   Any ZType can be converted to any other.  Shortening is always done by 
   truncation of high bits.  Extending is by sign extension when the final
   type is signed, or zero extension otherwise.  One exception is converting
   32-bit integer type to a 64-bit real.  Here, the final type gives no clue 
   whether to sign extend, so the decision is taken from the signedness of the 
   initial type.  This probably can't happen, would be irrelevant if it could,
   or just doesn't make much sense, but it's there for hopeful completeness.        

   Meanwhile, llvm has several different operators for different cases.
   bitcast requires identical sizes and either both types are pointers or 
   neither is a pointer.  PtrToInt and IntToPtr must be used for any conversion
   to/from a pointer, but will handle size changes with zero extend or truncate.
   trunc, zext, and sext work only on nonequal sized integer types.  trunc
   requires converting to a properly smaller size, and zext and sext require
   converting to a properly larger size.    

*) 

PROCEDURE SignedType ( fromType: Type ) : Type = 
  (* If fromType is an unsigned integer type, its same-sized signed counterpart.
     Otherwise, identity.
  *) 

  (* One would expect this function already exists elsewhere in the front end,
     but I can't find it. *) 

  BEGIN 
    CASE fromType OF
    | Type.Word8 => RETURN Type.Int8; 
    | Type.Word16 => RETURN Type.Int16; 
    | Type.Word32 => RETURN Type.Int32; 
    | Type.Word64 => RETURN Type.Int64; 
    ELSE RETURN fromType; 
    END; 
  END SignedType; 

PROCEDURE SameSizedWordType ( fromRType : RType) : IType = 
  (* The Word<n> CG type with same size as real type fromRType. *) 
  (* If we ever get a real type whose bit size does not match one of
     IType, there could be pervasive problems, as CG expects to be
     able to loophole between a real type and some IType or Addr.
     e.g., storing the argument of an exception expects to put it
     in an address, and does so by value if the argument type is
     scalar (including real types.) *) 

  VAR size: INTEGER; 
  BEGIN 
    size := TargetMap.CG_Size[fromRType]; 
    IF size = 32 THEN RETURN Type.Word32;
    ELSIF size = 64 THEN RETURN Type.Word64;
    ELSE <*ASSERT FALSE*>
    END; 
  END SameSizedWordType; 

PROCEDURE SizeNSignedness 
  (val: LLVM.ValueRef; fromIType, toIType: IType) : LLVM.ValueRef = 
  (* The Llvm type system does not distinguish signedness of integer types.
     Only the llvm operators applied make this distinction.  So CG types with
    the same size but different-signedness types require no Llvm operation. *) 

  VAR 
    destLlvmType : LLVM.TypeRef;
  BEGIN 
    CASE fromIType 
    OF Type.Word32, Type.Int32 => 
      CASE toIType 
      OF Type.Word64 => (* Zero extend. *) 
        destLlvmType := LLvmType(toIType);
        RETURN LLVM.LLVMBuildZExt(builderIR,val,destLlvmType, LT("loophole-zext"));
      | Type.Int64 => (* Sign extend. *) 
        destLlvmType := LLvmType(toIType);
        RETURN LLVM.LLVMBuildSExt(builderIR,val,destLlvmType, LT("loophole-sext"));
      ELSE RETURN val; 
      END; 
    | Type.Word64, Type.Int64 =>
      CASE toIType 
      OF Type.Word32, Type.Int32 => (* truncate. *) 
        destLlvmType := LLvmType(toIType);
        RETURN LLVM.LLVMBuildTrunc(builderIR,val,destLlvmType, LT("loophole-trunc"));
      ELSE RETURN val; 
      END; 
    END; 
  END SizeNSignedness;

PROCEDURE loophole (self: U;  fromCGType, toCGType: ZType) =
  (* s0.toCGType := LOOPHOLE(s0.fromCGType, toCGType) *)
  VAR
    s0 : REFANY; 
    s0AsExpr : LvExpr; 
    fromIType, toIType : IType; 
    initial, second, third, final : LLVM.ValueRef;
    fromLlvmType, destLlvmType : LLVM.TypeRef;
  BEGIN
    IF fromCGType = toCGType THEN RETURN END; 

    s0 := Get(self.exprStack,0);
    s0AsExpr := NARROW(s0,LvExpr);
    initial := s0AsExpr.lVal;
    destLlvmType := LLvmType(toCGType);
    IF fromCGType <= Type.Int64 THEN (* Word32, Int32, Word64, Int64 *) 
      IF toCGType <= Type.Int64 THEN (* word/int->word/int *) 
        final := SizeNSignedness (initial, fromCGType, toCGType); 
      ELSIF toCGType <= Type.XReel THEN (* word/int->real *)
        toIType := SameSizedWordType (toCGType); 
        IF Target.SignedType[fromCGType] THEN
          toIType := SignedType(toIType); 
        END; 
        second := SizeNSignedness (initial, fromCGType, toIType); 
        final := LLVM.LLVMBuildBitCast
                   (builderIR, second, destLlvmType, LT("loophole-word_real"));
      ELSE (* word/int->addr *) 
        final := LLVM.LLVMBuildIntToPtr
                   (builderIR, initial, destLlvmType, LT("loophole-addr_word"));
      END; 
    ELSIF fromCGType <= Type.XReel THEN (* Reel, LReel, XReel *) 
      IF toCGType <= Type.Int64 THEN (* real->word/int *)
        fromIType := SameSizedWordType (fromCGType); 
        second := LLVM.LLVMBuildBitCast
                   (builderIR, initial, LLvmType(fromIType), 
                    LT("loophole-real_word"));
        final := SizeNSignedness (second, fromIType, toCGType); 
      ELSIF toCGType <= Type.XReel THEN (* real->real *) 
        fromLlvmType := LLvmType(fromCGType); 
        IF fromLlvmType = destLlvmType THEN
          final := initial; 
        ELSE 
          fromIType := SameSizedWordType (fromCGType); 
          toIType := SameSizedWordType (toCGType); 
          second := LLVM.LLVMBuildBitCast
                     (builderIR, initial, LLvmType(fromIType), 
                      LT("loophole-real_real1"));
          third := SizeNSignedness (second, fromIType, toIType); 
          final := LLVM.LLVMBuildBitCast
                     (builderIR, third, destLlvmType, LT("loophole-real_real2"));
        END; 
      ELSE (* real->addr *) 
        fromIType := SameSizedWordType (fromCGType); 
        second := LLVM.LLVMBuildBitCast
                   (builderIR, initial, LLvmType(fromIType), 
                    LT("loophole-real_addr"));
        final := LLVM.LLVMBuildIntToPtr
                   (builderIR, second, destLlvmType, LT("loophole-IntToPtr"));
      END; 
    ELSE (* fromCGType = Type.Addr *) 
      IF toCGType <= Type.Int64 THEN (* addr->word/int *) 
        final := LLVM.LLVMBuildPtrToInt
                   (builderIR, initial, destLlvmType, LT("loophole-addr_word"));
      ELSIF toCGType <= Type.XReel THEN (* addr->real *) 
        toIType := SameSizedWordType (toCGType); 
        second := LLVM.LLVMBuildPtrToInt
                   (builderIR, initial, LLvmType(toIType), 
                    LT("loophole-addr_real"));
        final := LLVM.LLVMBuildBitCast
                   (builderIR, second, destLlvmType, LT("loophole-addr_real"));
      ELSE (* addr->addr *)
        (* This won't happen, but it is easier to handle it than assert false. *)  
        final := initial; 
      END; 
    END; 

    s0AsExpr.lVal := final;
  END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE abort (self: U;  code: RuntimeError) =
  (* generate a checked runtime error for "code" *)
  CONST numParams = 2;
  VAR
    codeVal,modVal : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
    codeAndLine : Word.T;
  BEGIN
    paramsRef := NewValueArr(paramsArr,numParams);
    (* encode the current line number with the error *)
    codeAndLine := Word.LeftShift(self.curLine,5);
    codeAndLine := Word.Or(codeAndLine,ORD(code));
    codeVal := LLVM.LLVMConstInt(LLVM.LLVMInt64Type(), VAL(ORD(codeAndLine),LONGINT), TRUE);    
    modVal := LLVM.LLVMBuildBitCast(builderIR, faultVal, AdrTy, LT("fault_toadr"));
    paramsArr[0] := modVal;
    paramsArr[1] := codeVal;
    EVAL LLVM.LLVMBuildCall(builderIR, self.abortFunc, paramsRef, numParams, LT(""));
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

PROCEDURE check_index (self: U; <*UNUSED*> t: IType;  code: RuntimeError) =
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

PROCEDURE check_eq (self: U; <*UNUSED*> t: IType; code: RuntimeError) =
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

PROCEDURE index_address (self: U; <*UNUSED*> t: IType; size: INTEGER) =
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


PROCEDURE start_call_direct 
  (self: U;  p: Proc; <*UNUSED*> lev: INTEGER; t: Type) =
  (* begin a procedure call to procedure 'p' at static level 'lev' that
     will return a value of type 't'. *)
  VAR
    proc : LvProc;
  BEGIN
    <* ASSERT self.callState = callStateTyp.outside *>
    self.callResultType := t; (* For completeness.  Won't be used. *) 
    proc := NARROW(p,LvProc);
    self.buildFunc(p);
    self.callState := callStateTyp.insideDirect; 
  END start_call_direct;

PROCEDURE Is_alloca (self: U; p: LvProc) : BOOLEAN =
  (* 'p' describes library function 'alloca'. *) 
  BEGIN 
    RETURN p.name = self . allocaName
           AND p.numParams = 1;  
  END Is_alloca; 

PROCEDURE call_direct (self: U; p: Proc; <*UNUSED*> t: Type) =
  (* call the procedure 'p'.  It returns a value of type t. *)
  VAR
    calleeProc : LvProc;
    fn, lVal, staticLinkActualLv (* i8* *) : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType;
    arg : LvExpr;
    returnName : TEXT := NIL;
    passedParamsCt, codedActualsCt : INTEGER := 0;
    staticLinkCount : [0..1]  := 0;
  BEGIN
    DumpExprStack(self,"call_direct");

    <* ASSERT self.callState = callStateTyp.insideDirect *>
    <* ASSERT self.callStack # NIL *>
    calleeProc := NARROW(p,LvProc);
    fn := calleeProc.lvProc;
    <*ASSERT fn # NIL *>
    codedActualsCt := self.callStack.size();

    IF Is_alloca (self, calleeProc) THEN 
      (* Front end encoded this as a call on library function 'alloca'.  
         Convert to an llvm 'alloca' instruction. *)  
      (* As of 2015-09-03, the only way a library call on 'alloca' appears
         in the input is front-end-generated for a jmpbuf. *) 
      arg := Get(self.callStack);
      Pop(self.callStack);
      lVal := LLVM.LLVMBuildArrayAlloca
                (builderIR, i8Type, arg.lVal, LT("m3_jmpbuf_size"));
      Push(self.exprStack,NEW(LvExpr,lVal := lVal));
      self.callState := callStateTyp.outside; 
      RETURN; 
    END; 
(* SEE ALSO: load_static_link. *) 
    IF calleeProc.staticLinkFormal # NIL 
    THEN 
      <* ASSERT calleeProc.lev > 0 *> (* Callee is nested. *)  
      staticLinkCount := 1; 
      (* ^Always pass a SL actual to a nested procedure. *)
      IF calleeProc.lev = self.curProc.lev + 1 THEN 
      (* Calling a nested procedure one level deeper than caller. *) 
        self.curProc.needsDisplay := TRUE; 
        (* ^Cause the code to build the display to end up in the entry BB of
           the caller.  We can't generate it until we get to its end_procedure,
           since there could still be more locals of inner blocks flattened
           into the caller's AR after this point. *)  
        staticLinkActualLv := self.curProc.outgoingDisplayLv (* i8** *);
        staticLinkActualLv := LLVM.LLVMBuildBitCast (* i8* *)
          (builderIR, staticLinkActualLv, AdrTy, LT("static_link_to_pass"))
      ELSE (* Nested callee procedure is nested no deeper than caller, which 
              is therefore also nested and thus has a static link formal.
              For this direct call, the static parent of the callee will be a
              proper static ancestor of the caller, and a prefix of the display
              passed to the caller in its SL will contain what's needed by the 
              callee.  The rest of it won't be used and is harmless.*)
        staticLinkActualLv (* i8* *) 
          := LLVM.LLVMBuildLoad
               (builderIR, self.curProc.storedStaticLink.lv (* i8** *), 
                LT("fetched_static_link"));
      END; (* Callee's nesting depth relative to caller's. *) 
    END; (* Is callee nested? *)

    (* create the param types from the callstack *)
    passedParamsCt := codedActualsCt + staticLinkCount;
    paramsRef := NewValueArr(paramsArr,passedParamsCt);

    IF staticLinkCount > 0 THEN
      paramsArr[0] := staticLinkActualLv; 
    END;
    
    FOR i := staticLinkCount TO passedParamsCt - 1 DO
      arg := Get(self.callStack);
      (* possibly add call attributes here like sext *)
      paramsArr[i] := arg.lVal;
      Pop(self.callStack);
    END;
    <*ASSERT self.callStack.size() = 0 *>

    IF calleeProc.returnType # Type.Void THEN
      returnName := "result";
    END;
    (* else void returns need null string *)

    lVal := LLVM.LLVMBuildCall
              (builderIR, fn, paramsRef, passedParamsCt, LT(returnName));

    IF calleeProc.returnType # Type.Void THEN
      (* push the return val onto stack *)
      Push(self.exprStack,NEW(LvExpr,lVal := lVal));
    END;
    self.callState := callStateTyp.outside; 
  END call_direct;

<*NOWARN*> 
PROCEDURE start_call_indirect 
  (self: U; t: Type; cc: CallingConvention) =
  (* begin an indirect procedure call that will return a value of type 't'. *)
  BEGIN
    <* ASSERT self.callState = callStateTyp.outside *>
    self.callResultType := t; 
    self.indirCallCC := cc;
    (* Basic blocks for separate top-level and nested call forms. *) 
    self.callState := callStateTyp.insideIndirect; 
  END start_call_indirect;

(* Construct a procedure signature type for use by an indirect call,
   using the actual parameters on the call stack (here, paramStack). *)
PROCEDURE IndirectFuncType
   (retType : Type; paramStack : RefSeq.T; Nested: BOOLEAN) : LLVM.TypeRef =
  VAR
    retTy,funcTy : LLVM.TypeRef;
    numFormals : INTEGER;
    param : LvExpr;
    typesArr : TypeArrType;
    typesRef : TypeRefType;
  BEGIN
    retTy := LLvmType(retType);
    numFormals := paramStack.size(); 

    IF numFormals > 0 THEN
      typesRef := NewTypeArr(typesArr,numFormals);
      FOR paramNo := 0 TO numFormals - 1 DO
        param := Get(paramStack, paramNo);
(* CHECK: ^Looks like contents of paramStack are all LvVar.  Narrow failure? *) 
        typesArr[paramNo] := LLVM.LLVMTypeOf(param.lVal);
      END;
    END;
    funcTy := LLVM.LLVMFunctionType(retTy, typesRef, numFormals, FALSE);
    RETURN funcTy;
  END IndirectFuncType;

PROCEDURE InnerCallIndirect 
  (self: U; proc: LLVM.ValueRef; t: Type; <*UNUSED*> cc: CallingConvention;
   Nested: BOOLEAN) 
  : LLVM.ValueRef =
  (* Call the procedure whose llvm address is proc.
     The procedure returns a value of CG type t. 
     Use the actual parameters on self.callStack (which will include a
     static link, if appropriate. 
     Do not pop the parameters. *)
  VAR
    callVal : LLVM.ValueRef;
    resultVal : LLVM.ValueRef;
    paramsArr : ValueArrType;
    paramsRef : ValueRefType := NIL;
    actual : LvExpr; 
    funcTy, funcPtrTy : LLVM.TypeRef;
    numFormals : INTEGER;
    formalName : TEXT;
    returnName : TEXT := "";
  BEGIN
    numFormals := self.callStack.size(); 

    (* Concoct a function signature from the actual parameters. *)
    funcTy := IndirectFuncType(t,self.callStack,Nested);

    (* Build the actual parameter list. *) 
    IF numFormals > 0 THEN
      paramsRef := NewValueArr(paramsArr,numFormals);
      FOR paramNo := 0 TO numFormals - 1 DO
        actual := NARROW(Get(self.callStack, paramNo), LvExpr);

        paramsArr[paramNo] := actual.lVal;
      END;
    END;
    IF t # Type.Void THEN
      returnName := "indir_call_result";
    END;

    (* Need a pointer to function type for the call. *)
    funcPtrTy := LLVM.LLVMPointerType(funcTy);
    callVal := LLVM.LLVMBuildBitCast(builderIR, proc, funcPtrTy, LT("call_ind"));
    resultVal := LLVM.LLVMBuildCall
      (builderIR, callVal, paramsRef, numFormals, LT(returnName));

    (* Set 'nest' attribute of static link formal. *) 
    IF Nested THEN
      LLVM.LLVMAddInstrAttribute
        (resultVal, 1 (*The parameter no. Zero is func return*), LLVM.NestAttribute);
    END;

    (* If we knew whether to, we would set the 'byval' attribute for structs
       passed by value here, but it is too hard to know whether, since we don't
       have the procedure type here. Instead, we forget byval and generate
       explicit copy code at the call site, in response to the pop_struct
       operator. *)

    RETURN resultVal; 
  END InnerCallIndirect;

PROCEDURE call_indirect (self: U;  t: Type; cc: CallingConvention) =
  (* call the procedure whose address is in s0.A and pop s0.  The
     procedure returns a value of type t. *)
  VAR 
    s0 := Get(self.exprStack,0);
    procExpr : LvExpr; 
    currentBB, mergeBB : LLVM.BasicBlockRef;
    resultVal1, resultVal2, resultVal3, mergePhi : LLVM.ValueRef;
    resultsArr: ValueArrType;
    resultsRef: ValueRefType;
    BBsArr: BBArrType;
    BBsRef: BBRefType;
    branchInst, loadInst : LLVM.ValueRef; 
  BEGIN
    DumpExprStack(self,"call_indirect_top_level");
    procExpr := NARROW(s0,LvExpr);
    Pop(self.exprStack);
    currentBB := LLVM.LLVMGetInsertBlock (builderIR); (* Save. *) 
    IF self.callState = callStateTyp . insideIndirect
       (* ^There was no pop_static_link => statically known to call a global proc. *)
    THEN 
      resultVal3 := InnerCallIndirect (self, procExpr.lVal, t, cc, Nested:=FALSE);
      IF t # Type.Void THEN (* push the return val onto exprStack*)
        Push(self.exprStack,NEW(LvExpr,lVal := resultVal3));
      END
    ELSIF self.staticLinkBB = currentBB
          (* same BB as the pop_static_link => statically known to call
             a nested proc. *)
    THEN 
      resultVal3 := InnerCallIndirect (self, procExpr.lVal, t, cc, Nested:=TRUE);
      IF t # Type.Void THEN (* push the return val onto exprStack*)
        Push(self.exprStack,NEW(LvExpr,lVal := resultVal3));
      END;

    ELSE (* Big semantic mismatch between CG IR and llvm IR.  The former can have
            a call with dynamically differing parameter list (here, with/without a
            static link), as long as it matches what the (also dynamically differing)
            procedure's code expects.  In llvm, a call instruction has to have a
            static function type, including its parameter list.  So we need two
            call instructions, in different basic blocks.

            Here, currentBB is on an execution path that does not leave a static
            link on paramStack.  Code in self.staticLinkBB is on a path that 
            contains the pop_static_link and statically does have one on 
            paramStack.  On both paths, the code address is on top of exprStack
            and is non-NIL, and paramStack, minus the SL if any, is otherwise correct
            for the call. *) 

      mergeBB := LLVM.LLVMAppendBasicBlock 
        (self.curProc.lvProc,LT("indir_sl_merge"));

      (* Let's switch to self.staticLinkBB first and take care of that case while
         exprStack is right for it. *)

      (* We have to use some inside knowledge of the code sequences CG is producing.
         Specifically, staticLinkBB ends with load (of code address), a store (which
         we don't use) and a branch (which we don't want to follow. *) 

      (* staticLinkBB already will have a terminating unconditional branch, because
         it is followed by label for not-a-closure-call.  We have to remove it.  
         The Load instruction 3rd from the end of staticLinkBB will be the code addr. *) 
      branchInst := GetBBLastInstruction(self.staticLinkBB, LLVM.Opcode.Br);
      loadInst := LLVM.LLVMGetPreviousInstruction(branchInst);
      <* ASSERT LLVM.LLVMGetInstructionOpcode(loadInst) = LLVM.Opcode.Store*> 
      loadInst := LLVM.LLVMGetPreviousInstruction(loadInst);
      <* ASSERT LLVM.LLVMGetInstructionOpcode(loadInst) = LLVM.Opcode.Load*> 
      LLVM.LLVMInstructionEraseFromParent(branchInst);
      
      LLVM.LLVMPositionBuilderAtEnd (builderIR, self.staticLinkBB);
 (* REVIEW: The CG IR code contains another nil-check at this point on the code
            address that was taken from the closure (having earlier nil-checked
            the procedure pointer itself).  I think nil here can't happen.
            If it ever does, just insert nil-check and abort on loadInst here.
            
            In the CG code, this check is also in the non-closure path, where it
            duplicates the earlier check, in the non-closure case, on the same
            pointer.  But it is needed in that case, because only it does an
            abort when it fails.  The CG code could really be reviewed here. *) 
      resultVal1 := InnerCallIndirect (self, loadInst, t, cc, Nested:=TRUE);
      EVAL LLVM.LLVMBuildBr(builderIR, mergeBB);

      (* If it has a result, prepare for a Phi at the top of the mergedBB. *) 
      IF t # Type.Void THEN
        resultsRef := NewValueArr (resultsArr, 2); 
        resultsArr^[0] := resultVal1; 
        BBsRef := NewBBArr ( BBsArr, 2);
        BBsArr^[0] := self.staticLinkBB; 
      END;

      (* Now switch back to currentBB, remove the SL from exprStack, and take care
         of the no-SL version. *)   
      LLVM.LLVMPositionBuilderAtEnd (builderIR, currentBB); 
      Pop(self.callStack); (* Remove SL from left of actual param list. *) 
   (* PopRev(self.callStack); If the SL were on the right of actual param list this,
                              is how we would remove it. *)
      (* But leave the rest of the params on self.call_stack.
         They will be needed by InnerCallIndirect. *) 
      resultVal2 := InnerCallIndirect (self, procExpr.lVal, t, cc, Nested:=FALSE);
      EVAL LLVM.LLVMBuildBr(builderIR, mergeBB); 
      LLVM.LLVMPositionBuilderAtEnd (builderIR, mergeBB); 

      (* If it has a result, complete the Phi. *) 
      IF t # Type.Void THEN
        resultsArr^[1] := resultVal2; 
        BBsArr^[1] := currentBB; 
        mergePhi := LLVM.LLVMBuildPhi 
          (builderIR, LLvmType(t), LT("indir_call_result"));  
        LLVM.LLVMAddIncoming(mergePhi, resultsRef, BBsRef, 2); 
        (* push the return val onto exprStack*)
        Push(self.exprStack,NEW(LvExpr,lVal := mergePhi));
      END;
    END;
    
    MakeRefSeqEmpty(self.callStack); 
    <*ASSERT self.callStack.size() = 0 *>

    self.callState := callStateTyp.outside; 
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

    PushRev(self.callStack,s0);
    Pop(self.exprStack);
  END pop_param;

PROCEDURE pop_struct 
  (self: U; t: TypeUID; s: ByteSize; <*UNUSED*>  a: Alignment) =
  (* pop s0.A, it's a pointer to a structure occupying 's' bytes that's
    'a' byte aligned;  It is passed by value in M3, but llvm code passes
    the *address* of the structure, so we first make a copy here. *)
  VAR
    s0 := Get(self.exprStack,0);
    expr : LvExpr;
    structTy, refTy : LLVM.TypeRef;
    origRef_lVal, copyRef_lVal, len_lVal : LLVM.ValueRef;
    curBB : LLVM.BasicBlockRef;
  BEGIN
    expr := NARROW(s0,LvExpr);
 
    (* This parm needs to agree with its declared type. All structs
       should be in the struct table indexed by their byte size. 
       Find, or create, the type and cast the parm to its pointer type. *)
    structTy := StructType(self,s);
    refTy := LLVM.LLVMPointerType(structTy);
    (* this is the proper type for the call *)
    origRef_lVal := LLVM.LLVMBuildBitCast
      (builderIR, expr.lVal, refTy, LT("pop_struct-reftype"));

    (* Allocate a temp for the copy in the entry BB *)
    curBB := LLVM.LLVMGetInsertBlock(builderIR);
    LLVM.LLVMPositionBuilderAtEnd(builderIR, self.curProc.entryBB);
    copyRef_lVal := LLVM.LLVMBuildAlloca(builderIR, structTy, LT("ValueFormalCopyRef"));
    LLVM.LLVMSetAlignment
      (copyRef_lVal, LLVM.LLVMPreferredAlignmentOfType(targetData, refTy));
    LLVM.LLVMPositionBuilderAtEnd(builderIR, curBB);
(* CHECK ^Is this really necessary? *)

    (* Generate the copy. *) 
    len_lVal := LLVM.LLVMConstInt(IntPtrTy, VAL(s,LONGINT), TRUE);
    DoMemCopy(expr.lVal,copyRef_lVal,len_lVal,align:=1,overlap:=FALSE);

    (* Update things. *) 
    expr.lVal := copyRef_lVal;
    Pop(self.exprStack);
    PushRev(self.callStack,expr);
  END pop_struct;

PROCEDURE pop_static_link (self: U) =
  (* pop s0.A and pass it as the current indirect procedure call's 
     static link  *)
  VAR
    s0 := Get(self.exprStack,0);
  BEGIN
    <* ASSERT self.callState = callStateTyp . insideIndirect *> 
    Push(self.callStack,s0); (* Make SL the leftmost actual. *) 
 (* PushRev(self.callStack,s0);  If we ever want to make SL rightmost, this is how. *)

    Pop(self.exprStack); 
    self.staticLinkBB := LLVM.LLVMGetInsertBlock (builderIR);
    (* ^So can see if it changes by the time we see the call_indirect. *) 
    self.callState := callStateTyp . indirectAfterSL; 

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
    link (* I8** *): LLVM.ValueRef;
  BEGIN
(* SEE ALSO: call_direct. *) 
    proc := NARROW(p,LvProc);
    IF proc.lev = 0 THEN 
      link := LLVM.LLVMConstPointerNull(AdrTy);
    ELSIF self.curProc.lev + 1 = proc.lev THEN (* One level deeper. *) 
      self.curProc.needsDisplay := TRUE; 
        (* ^Cause the code to build the display to end up in the entry BB of
           the caller.  We can't generate it until we get to its end_procedure,
           since there could still be more locals of inner blocks flattened
           into the caller's AR after this point. *)  
      link := self.curProc.outgoingDisplayLv; (* i8** *) 
      link := LLVM.LLVMBuildBitCast (* i8* *)
            (builderIR, link, AdrTy, LT("__static_link_from_display"))
    ELSE (* Nested callee procedure is nested no deeper than caller, which 
            is therefore also nested and thus has a static link formal.
            For this direct call, the static parent of the callee will be a
            proper static ancestor of the caller, and a prefix of the display
            passed to the caller in its SL will contain what's needed by the 
            callee.  The rest of it won't be used and is harmless.*)
      link := LLVM.LLVMBuildLoad (* i8* *) 
                (builderIR, self.curProc.storedStaticLink.lv (* i8** *),
                 LT("__fetched_static_link.i8*"));
    END; (*IF*) 
    Push(self.exprStack,NEW(LvExpr,lVal := link));
  END load_static_link;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE comment (self: U;  a, b, c, d: TEXT := NIL) =
(* annotate the output with a&b&c&d as a comment.  Note that any of a,b,c or d
   may be NIL. *)
  VAR s : TEXT := "";
  BEGIN
    IF a # NIL THEN s := s & a; END;
    IF b # NIL THEN s := s & b; END;
    IF c # NIL THEN s := s & c; END;
    IF d # NIL THEN s := s & d; END;
(* FIXME: Somehow get the comment into llvm IR. *)     
    IF self.m3llvmDebugLev > 0 THEN 
      IO.Put("Comment -- " & s &  "\n");
    END; 
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

PROCEDURE exchange (self: U; <*UNUSED*> t: MType; <*UNUSED*> u: ZType;  order: MemoryOrder) =
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

    lVal := LLVM.LLVMBuildAtomicCmpXchg(builderIR, ptr,cmp,new, successOrdering, failureOrdering, TRUE);
    NARROW(s1,LvExpr).lVal := lVal;
    Pop(self.exprStack,2);
*)
  END compare_exchange;

PROCEDURE fence (<*UNUSED*> self: U;  order: MemoryOrder) =
  (* Memory is affected as per o *)
  VAR
    ordering : LLVM.AtomicOrdering;
  BEGIN
    (* fixme is singlethreaded significant *)
    ordering := GetOrder(order);
    EVAL LLVM.LLVMBuildFence(builderIR, ordering, TRUE, LT("fence"));
  END fence;

PROCEDURE fetch_and_op (self: U;  op: AtomicOp; <*UNUSED*> t: MType; <*UNUSED*> u: ZType; order: MemoryOrder) =
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
  booleans, enumerations work
  records, arrays and objects seem to work
  block level vars work with for and with stmts
  refs work
  recursive types work
  packed records and objects work
  large global variables work

  problems with debug so far
  packed arrays dont work
  set types dont work - not implemented type in dibuilder
  global variables in data segment dont work gdb says optimised out.
  subranges dont enforce the subrange they are just the base type
  open arrays display the first 2 words of the allocation and are limited
  to 100 items. Need dibuilder support to set the range.
  text dont work same prob as open array i think.
*)

PROCEDURE DebugInit(self: U) =
  VAR
    mdNode : LLVM.ValueRef;
    valsMD : ARRAY[0..2] OF REFANY;
  BEGIN
    InitUids(self);
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
    NARROW(valsMD[2],REF INTEGER)^ := DC.DWARF_VERSION;

    mdNode := GetMDNode(valsMD);
    LLVM.LLVMAddNamedMetadataOperand(modRef, LT("llvm.module.flags"), mdNode);
    valsMD[1] := "Debug Info Version";
    NARROW(valsMD[2],REF INTEGER)^ := LLVMTypes.DEBUG_METADATA_VERSION;
    mdNode := GetMDNode(valsMD);
    LLVM.LLVMAddNamedMetadataOperand(modRef, LT("llvm.module.flags"), mdNode);

    self.debugRef := M3DIB.DIBBuilderCreate(modRef);

    self.cuRef := M3DIB.DIBcreateCompileUnit(
                self.debugRef,
                DC.DW_LANG_Modula3,
                LTD(self.debFile),
                LTD(self.debDir),
                LTD("cm3"), (* Producer *)
                FALSE, (* isOptimized - can we check *)
                LTD(""), (* Flags *)
                0 (*RuntimeVersion*) );

    self.fileRef := M3DIB.DIBcreateFile(self.debugRef, LTD(self.debFile), LTD(self.debDir));
  END DebugInit;

PROCEDURE BuildSubrangeDebug 
  (self : U; UID, baseUID: TypeUID; 
   min, max: TInt.Int; 
   bitSize, align : LONGINT; 
   encoding: unsigned; 
   nameText: TEXT := NIL)
: SubrangeDebug =
  VAR LDebug : SubrangeDebug; 
    temp, count: TInt.Int; 
  BEGIN
    IF TInt.LT(max,min) THEN
      count := TInt.Zero;
    ELSE  
      EVAL TInt.Subtract(max, min, (*OUT*)temp); 
      EVAL TInt.Add(temp, TInt.One, (*OUT*)count); 
    END; 

    LDebug 
      := NEW (SubrangeDebug,
              tUid := UID,
              bitSize := bitSize,
              align := align,
              typeName := M3ID.Add (nameText),
              encoding := encoding,
              DIDescr := NIL,
              domain := baseUID, (* Self-referential. *) 
              min := min,
              count := count);
    EVAL self.debugTable.put(UID, LDebug); 
    RETURN LDebug; 
  END BuildSubrangeDebug;  

PROCEDURE BuiltinOrdinalDebug
  (self : U; UID: TypeUID; 
   bitSize, align: LONGINT; 
   nameText: TEXT; 
   encoding: unsigned; 
   min, max: TInt.Int)  
: SubrangeDebug =
  BEGIN
    RETURN 
      BuildSubrangeDebug
        (self, 
         UID, UID (*Self referential*), 
         min, max, 
         bitSize := bitSize, align := align,
         encoding := encoding, 
         nameText := nameText); 
  END BuiltinOrdinalDebug; 

(* Must call InitUids after the possible widechar_size operator has been
   seen.  set_source_file operator is the earliest place that satisfies
   the requirement. *) 
PROCEDURE InitUids(self : U) =
  VAR wordMin, wordMax, widecharMax: TInt.Int;
    intBits, widecharBits: LONGINT; 
  BEGIN
  (* create the basic tUid entries - needs testing *)

    (* Builtin ordinal types: *) 
    IF ptrBytes = 4
    THEN 
      wordMin := TInt.Min32; wordMax := TInt.Max32; 
      intBits := 32L;
    ELSE 
      wordMin := TInt.Min64; wordMax := TInt.Max64;
      intBits := 64L;
    END;
    widecharBits := VAL(self.widecharSize,LONGINT); 
    IF widecharBits = 16L 
    THEN widecharMax := TInt.Max16U;
    ELSE <* ASSERT widecharBits = 32L *> 
         EVAL TInt.FromInt(16_10FFFF, (*OUT*)widecharMax);
    END;  

    EVAL BuiltinOrdinalDebug
           (self, UID_INTEGER, bitSize := intBits, align := intBits, 
            nameText := "INTEGER", encoding := DC.DW_ATE_signed,
            min := wordMin, max := wordMax);
    EVAL BuiltinOrdinalDebug
           (self, UID_CARDINAL, bitSize := intBits, align := intBits, 
            nameText := "CARDINAL", encoding := DC.DW_ATE_signed,
            min := TInt.Zero, max := wordMax);
    (* We don't want CARDINAL and LONGCARD to be unsigned.  They are 
       nonnegative subranges of signed types, and when involved in
       arithmetic, should use signed operations and intermediate 
       results. *) 
    EVAL BuiltinOrdinalDebug
           (self, UID_LONGINT, bitSize := 64L, align := intBits, 
            nameText := "LONGINT", encoding := DC.DW_ATE_signed,
            min := TInt.Min64, max := TInt.Max64);
    EVAL BuiltinOrdinalDebug
           (self, UID_LONGCARD, bitSize := 64L, align := intBits, 
            nameText := "LONGCARD", encoding := DC.DW_ATE_signed,
            min := TInt.Zero, max := TInt.Max64);
    EVAL BuiltinOrdinalDebug
           (self, UID_BOOLEAN, bitSize := 8L, align := 8L, 
            nameText := "BOOLEAN", encoding := DC.DW_ATE_boolean,
            min := TInt.Zero, max := TInt.One);
    EVAL BuiltinOrdinalDebug
           (self, UID_CHAR, bitSize := 8L, align := 8L, 
            nameText := "CHAR", encoding := DC.DW_ATE_unsigned_char,
            min := TInt.Zero, max := TInt.Max8U);
    EVAL BuiltinOrdinalDebug
           (self, UID_WIDECHAR, bitSize := widecharBits, align := widecharBits, 
            nameText := "WIDECHAR", encoding := DC.DW_ATE_unsigned,
            min := TInt.Zero, max := widecharMax);

    (* Real types: *) 
    EVAL self.debugTable.put(UID_REEL,NEW(BaseDebug, bitSize := 32L, align := 32L, typeName := M3ID.Add("REAL"), encoding := DC.DW_ATE_float));
    EVAL self.debugTable.put(UID_LREEL,NEW(BaseDebug, bitSize := 64L, align := 64L, typeName := M3ID.Add("LONGREAL"), encoding := DC.DW_ATE_float));
    (* change this if ever upgrade to 128 bit floats or change to store sizeof type*)
    EVAL self.debugTable.put(UID_XREEL,NEW(BaseDebug, bitSize := 64L, align := 64L, typeName := M3ID.Add("EXTENDED"), encoding := DC.DW_ATE_float));

    (* Reference types: *) 
(* check this type and size*)
    EVAL self.debugTable.put(UID_ROOT,NEW(ObjectDebug, bitSize := 0L(*ptrBits*), align := ptrBits, typeName := M3ID.Add("ROOT"), encoding := DC.DW_ATE_address));
(* fix this type *)
    EVAL self.debugTable.put(UID_UNTRACED_ROOT,NEW(ObjectDebug, bitSize := 0L(*ptrBits*), align := ptrBits, typeName := M3ID.Add("UNTRACED_ROOT"), encoding := DC.DW_ATE_address));
    EVAL self.debugTable.put(UID_ADDR,NEW(BaseDebug, bitSize := ptrBits, align := ptrBits, typeName := M3ID.Add("ADDR"), encoding := DC.DW_ATE_address));

    EVAL self.debugTable.put(UID_TEXT,NEW(ObjectDebug, superType := UID_REFANY, bitSize := ptrBits, align := ptrBits, typeName := M3ID.Add("TEXT"), encoding := DC.DW_ATE_address));
(* check this type and size*)
    EVAL self.debugTable.put(UID_REFANY,NEW(ObjectDebug, superType := UID_ROOT, bitSize := ptrBits, align := ptrBits, typeName := M3ID.Add("REFANY"), encoding := DC.DW_ATE_address));
(* check this type and size*)
    EVAL self.debugTable.put(UID_MUTEX,NEW(ObjectDebug, superType := UID_ROOT, bitSize := ptrBits, align := ptrBits, typeName := M3ID.Add("MUTEX"), encoding := DC.DW_ATE_address));

    (* Other: *) 
    EVAL self.debugTable.put(UID_RANGE_0_31,NEW(BaseDebug, bitSize := 32L, align := 32L, typeName := M3ID.Add("RANGE_0_31"), encoding := DC.DW_ATE_signed));
    EVAL self.debugTable.put(UID_RANGE_0_63,NEW(BaseDebug, bitSize := 64L, align := 64L, typeName := M3ID.Add("RANGE_0_63"), encoding := DC.DW_ATE_signed));

    EVAL self.debugTable.put(UID_NULL,NEW(BaseDebug, bitSize := ptrBits, align := ptrBits, typeName := M3ID.Add("NULL"), encoding := DC.DW_ATE_address));
    EVAL self.debugTable.put(NO_UID,NEW(BaseDebug, bitSize := 0L, align := 0L, typeName := M3ID.Add("NO_UID"), encoding := DC.DW_ATE_address));
  END InitUids;

PROCEDURE EnsureDebugTypeName(debug: BaseDebug) =
  BEGIN
    IF debug.typeName = M3ID.NoID THEN
      debug.typeName := M3ID.Add(M3CG.FormatUID(debug.tUid)); 
    END; 
  END EnsureDebugTypeName; 

PROCEDURE DebugFinalise(self : U) =
  BEGIN
    IF self.genDebug THEN
      M3DIB.DIBfinalize(self.debugRef);
    END;
  END DebugFinalise;

PROCEDURE DebugLine(self : U) =
  VAR
    loc : LLVM.ValueRef;
    scope : M3DIB.LLVMDISubprogram; 
    blockRef : BlockDebug;
  BEGIN
    IF self.genDebug THEN
      IF self.funcRef # NIL THEN
        IF self.debugLexStack.size() > 0 THEN
          blockRef := Get(self.debugLexStack);
          scope := blockRef.value;
        ELSE
          scope := self.funcRef;
        END;
        loc := M3DIB.DIBgetDebugLoc(self.curLine,0,self.funcRef);
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
    funcTy : M3DIB.LLVMDISubroutineType;
    paramsArr : REF ARRAY OF MetadataRef; 
    paramsMetadata : LLVMTypes.ArrayRefOfMetadataRef; 
    paramsDIArr : M3DIB.LLVMDIArray;
    tyVal : M3DIB.LLVMDIBasicType; 
    arg : REFANY;
    numParams,tSize,tAlign : CARDINAL;
    procName,typeName : TEXT;
  BEGIN
    IF NOT self.genDebug THEN RETURN; END;
    proc := NARROW(p,LvProc);
    numParams := proc.numParams;

    (* extra one since return type is first param *)
    NewArrayRefOfMetadataRef
      (numParams + 1, (*OUT*)paramsArr, (*OUT*)paramsMetadata);
    IF proc.returnType = Type.Void THEN
      tyVal := LOOPHOLE(M3DIB.LLVMDIDescriptorEmpty,M3DIB.LLVMDIBasicType);
(* CHECK       ^Or do we need some representation of void type? *) 
    ELSE
      tSize := TypeSize(proc.returnType) * ptrBytes;
      tAlign := tSize;
      tyVal := M3DIB.DIBcreateBasicType(self.debugRef, LTD("return_t"), VAL(tSize,uint64_t), VAL(tAlign,uint64_t), 0);
    END;
    paramsArr[0] := tyVal;

    IF numParams > 0 THEN
      FOR i := 0 TO numParams - 1 DO
        arg := Get(proc.paramStack,i);
        param := NARROW(arg,LvVar);
        typeName := M3ID.ToText(param.name) & "_t";
        tSize := param.size * ptrBytes;
        tAlign := param.align * ptrBytes;
        tyVal := M3DIB.DIBcreateBasicType
                   (self.debugRef, LTD(typeName),
                    VAL(tSize,uint64_t), VAL(tAlign,uint64_t),  DC.DW_ATE_address);
        paramsArr[i+1] := tyVal;
      END;
    END;

    paramsDIArr := M3DIB.DIBgetOrCreateArray(self.debugRef, paramsMetadata);
    funcTy := M3DIB.DIBcreateSubroutineType(self.debugRef,self.fileRef, paramsDIArr);
    procName := M3ID.ToText(proc.name);

    self.funcRef := M3DIB.DIBcreateFunction(self.debugRef,
       self.cuRef,     (* Scope - problem with nested ??*)
       LTD(procName),  (* Name *)
       LTD(""),        (* LinkageName *)
       self.fileRef,   (* File *)
       self.curLine,   (* LineNo *)
       funcTy,         (* CompositeType *)
       FALSE,          (* IsLocaltoUnit *)
       TRUE,           (* IsDefinition *)
       self.curLine,   (* ScopeLine ?? maybe 0 *)
       0,              (* flags *)
       FALSE,          (* isOptimized *)
       LOOPHOLE(proc.lvProc,LLVMTypes.FunctionRef));   (* Func *)

  END DebugFunc;

PROCEDURE DebugPushBlock(self : U) =
  VAR
    DILexBlock : M3DIB.LLVMDILexicalBlock; 
    parent : M3DIB.LLVMDISubprogram;
    blockRef : BlockDebug;
  BEGIN
    IF NOT self.genDebug THEN RETURN; END;
    IF self.debugLexStack.size() = 0 THEN
      blockRef := NEW(BlockDebug, value:= self.funcRef);
      Push(self.debugLexStack, blockRef);
    ELSE
      blockRef := Get(self.debugLexStack);
      parent := blockRef.value;
      (* this val has to be referred to by the var as the scope *)
      DILexBlock := M3DIB.DIBcreateLexicalBlock
         (self.debugRef, parent, self.fileRef, self.curLine, 0);
      blockRef := NEW(BlockDebug,value:= DILexBlock);
      Push(self.debugLexStack, blockRef);
    END;
  END DebugPushBlock;

PROCEDURE DebugPopBlock(self : U) =
  BEGIN
    IF NOT self.genDebug THEN RETURN; END;
    Pop(self.debugLexStack);
  END DebugPopBlock;

PROCEDURE DebugLookupOrdinalBounds(self : U; tUid : TypeUID; VAR min, count : TInt.Int) = 
  VAR
    debugObj : REFANY;
    tUidExists : BOOLEAN;
  BEGIN
    min := TInt.Zero; (* Default to an empty range. *) 
    count := TInt.Zero; 
    tUidExists := self.debugTable.get(tUid, (*OUT*)debugObj);
    IF tUidExists THEN
      TYPECASE debugObj 
      OF NULL => 
      | SubrangeDebug (SD) 
      => min := SD.min;
         count := SD.count; 
      | EnumDebug (ED)
      => min := TInt.Zero;
         EVAL TInt.FromInt(ED.numElts, (*OUT*)count);
      ELSE 
      END; 
    END;
  END DebugLookupOrdinalBounds;

PROCEDURE DebugSubrange(self : U; subrange : SubrangeDebug) 
: M3DIB.LLVMDISubrange =
  VAR Result : M3DIB.LLVMDISubrange;
      MLBase : BaseDebug;  
  BEGIN
    IF self.m3llvmDebugLev > 0 THEN
      IO.Put("subrange debug\n");
    END; 

    IF subrange.domain # NO_UID AND subrange.domain # subrange.tUid
    THEN (* It has a non-self-referential base type. *) 
      MLBase := DebugLookupML(self, subrange.domain);
      TYPECASE MLBase 
      OF NULL =>
      | SubrangeDebug (baseSD) 
        => subrange.domain := baseSD.domain; 
           (* ^Shortcut thru' transitive subranges. *)   
           subrange.encoding := MLBase.encoding; (* Now that it's available. *)
      ELSE 
        subrange.encoding := MLBase.encoding; (* Now that it's available. *)
      END; 
    ELSE MLBase := NIL;
    END; 

    Result 
      := M3DIB.DIBgetOrCreateSubrange
           (self.debugRef, 
            TIntToint64_t(subrange.min), TIntToint64_t(subrange.count));
(* REVIEW: How does llvm handle subranges for different signedness? *) 
(* FIXME: Someday, when llvm supports it, put MLBase into the subrange. *) 
    RETURN Result; 
  END DebugSubrange;

PROCEDURE DebugArray(self : U; a : ArrayDebug) : M3DIB.LLVMDICompositeType =
  VAR
    eltVal : M3DIB.LLVMDIType; 
    subsVal : M3DIB.LLVMDISubrange; 
    min, count : TInt.Int; 
    subrange : SubrangeDebug;
    paramsArr : REF ARRAY OF MetadataRef; 
    paramsMetadata : LLVMTypes.ArrayRefOfMetadataRef; 
    paramsDIArr : M3DIB.LLVMDIArray;
    Result : M3DIB.LLVMDICompositeType; 
  BEGIN
    IF self.m3llvmDebugLev > 0 THEN
      IO.Put("array debug\n");
    END; 

    eltVal := DebugLookupLL(self,a.elt);
    DebugLookupOrdinalBounds (self, a.index, (*OUT*)min, (*OUT*)count);  
    subsVal 
      := M3DIB.DIBgetOrCreateSubrange
           (self.debugRef, TIntToint64_t(min), TIntToint64_t(count));
    NewArrayRefOfMetadataRef(1, (*OUT*)paramsArr, (*OUT*)paramsMetadata);
    paramsArr[0] := subsVal;
    paramsDIArr := M3DIB.DIBgetOrCreateArray(self.debugRef, paramsMetadata);
    Result 
      := M3DIB.DIBcreateArrayType
           (self.debugRef, 
            VAL(a.bitSize,uint64_t), VAL(a.align,uint64_t), 
            eltVal, paramsDIArr);
    RETURN Result; 
  END DebugArray;

PROCEDURE DebugOpenArray
  (self : U; a : OpenArrayDebug) : M3DIB.LLVMDICompositeType =
  VAR
    eltVal : M3DIB.LLVMDIType; 
    subsVal : M3DIB.LLVMDISubrange; 
    paramsArr : REF ARRAY OF MetadataRef; 
    paramsMetadata : LLVMTypes.ArrayRefOfMetadataRef; 
    paramsDIArr : M3DIB.LLVMDIArray;
  BEGIN
    IF self.m3llvmDebugLev > 0 THEN
      IO.Put("openarray debug\n");
    END; 

    eltVal := DebugLookupLL(self,a.elt);
    (* open arrays dont have a range so just fake it for now 0 - last(val)
        fix this later
    subrange := DebugSubrangeLookup(self,a.index);
    *)
    subsVal := M3DIB.DIBgetOrCreateSubrange(self.debugRef, VAL(0L,int64_t), VAL(100L,int64_t));
    NewArrayRefOfMetadataRef(1, (*OUT*)paramsArr, (*OUT*)paramsMetadata);
    paramsArr[0] := subsVal;
    paramsDIArr := M3DIB.DIBgetOrCreateArray(self.debugRef, paramsMetadata);
    RETURN M3DIB.DIBcreateArrayType(self.debugRef, VAL(a.bitSize,uint64_t), VAL(a.align,uint64_t), eltVal, paramsDIArr);
  END DebugOpenArray;

PROCEDURE DebugSet(self : U; s : SetDebug) : M3DIB.LLVMDICompositeType =
  VAR
    eltVal : M3DIB.LLVMDIBasicType; 
    subsVal : M3DIB.LLVMDISubrange; 
    subrange : SubrangeDebug;
    min, count : TInt.Int; 
    paramsArr : REF ARRAY OF MetadataRef; 
    paramsMetadata : LLVMTypes.ArrayRefOfMetadataRef; 
    paramsDIArr : M3DIB.LLVMDIArray;

  BEGIN
    IF self.m3llvmDebugLev > 0 THEN
      IO.Put("set debug\n");
    END; 

    eltVal := M3DIB.DIBcreateBasicType(self.debugRef,LTD("basic_type" ), VAL(1L,uint64_t), VAL(8L,uint64_t), DC.DW_ATE_unsigned_char);


    (*eltVal := DebugLookupLL(self,d.elt);*)
    (*
    subsVal := DebugLookupLL(self,d.domain);
    NewArrayRefOfMetadataRef(1, (*OUT*)paramsArr, (*OUT*)paramsMetadata);
    paramsArr[0] := subsVal;
    paramsDIArr := M3DIB.DIBgetOrCreateArray(self.debugRef, paramsMetadata);
    *)
    (* subrange := DebugSubrangeLookup(self,s.domain); *) 
(* FIXME: domain could be CHAR or BOOLEAN, not a subrange. *) 
    (*
    subsCount := VAL(subrange.max - subrange.min + 1L,INTEGER);
    *)


    DebugLookupOrdinalBounds (self, s.domain, (*OUT*)min, (*OUT*)count);  
    subsVal := M3DIB.DIBgetOrCreateSubrange
      (self.debugRef, TIntToint64_t(min), TIntToint64_t(count));

    NewArrayRefOfMetadataRef(1, (*OUT*)paramsArr, (*OUT*)paramsMetadata);
    paramsArr[0] := subsVal;
    paramsDIArr := M3DIB.DIBgetOrCreateArray(self.debugRef, paramsMetadata);

    RETURN M3DIB.DIBcreateArrayType(
             self.debugRef, 
             VAL(s.bitSize,uint64_t), 
             VAL(s.align,uint64_t), 
             LOOPHOLE(eltVal,M3DIB.LLVMDIType), 
             paramsDIArr );

(* attempt with set type - this creates the new metadata tag for sets
  and generates proper metadata i think but llc complains the tag is not
  implemented, think the DW set tag has to be implemented in llvm
  Also you should be able to implement this with packed array of bits
  havent tried that yet. *)
(*
    eltVal := M3DIB.DIBcreateBasicType(self.debugRef,LTD("basic_type" ), 1L, 8L, DC.DW_ATE_unsigned_char);

    EnsureDebugTypeName(d); 
    lVal := M3DIB.DIBcreateSetType(
             self.debugRef,
             eltVal,
             d.bitSize,
             d.align,
             LTD(M3ID.ToText(d.typeName)));
*)
  END DebugSet;

PROCEDURE DebugEnum(self : U; e : EnumDebug) : M3DIB.LLVMDICompositeType =
  VAR
    eltVal : M3DIB.LLVMDIEnumerator; 
    paramsArr : REF ARRAY OF MetadataRef; 
    paramsMetadata : LLVMTypes.ArrayRefOfMetadataRef; 
    paramsDIArr : M3DIB.LLVMDIArray;
  BEGIN
    NewArrayRefOfMetadataRef
      (e.numElts, (*OUT*)paramsArr, (*OUT*)paramsMetadata);
    FOR i := 0 TO e.numElts - 1 DO
      eltVal := M3DIB.DIBcreateEnumerator(self.debugRef,LTD(M3ID.ToText(e.elts[i])),i);
      paramsArr[i] := eltVal;
    END;
    paramsDIArr := M3DIB.DIBgetOrCreateArray(self.debugRef, paramsMetadata);

    EnsureDebugTypeName(e); 
    RETURN M3DIB.DIBcreateEnumerationType(
               self.debugRef,
               self.funcRef,
               LTD(M3ID.ToText(e.typeName)),
               self.fileRef,
               self.curLine,
               VAL(e.bitSize,uint64_t),
               VAL(e.align,uint64_t),
               paramsDIArr,
               M3DIB.LLVMDIDescriptorEmpty);
   
    
    
  END DebugEnum;

PROCEDURE DebugPacked(self : U; p : PackedDebug) : M3DIB.LLVMDIType =
  BEGIN
(* FICME ? *) 
    RETURN DebugLookupLL(self,p.base);
END DebugPacked;

PROCEDURE DebugOpaque(self : U; o : OpaqueDebug) : M3DIB.LLVMDIType =
  BEGIN
  (* fixme *)
    IF self.m3llvmDebugLev > 0 THEN
      IO.Put("opaque debug\n");
    END; 
    RETURN M3DIB.LLVMDIDescriptorEmpty;
END DebugOpaque;

PROCEDURE DebugProcType(self : U; p : ProcTypeDebug) 
: M3DIB.LLVMDISubroutineType =
  VAR
    LDIDescr : M3DIB.LLVMDIDescriptor; 
    LDISubprogTy : M3DIB.LLVMDISubroutineType; 
    paramsArr : REF ARRAY OF MetadataRef; 
    paramsMetadata : LLVMTypes.ArrayRefOfMetadataRef; 
    paramsDIArr : M3DIB.LLVMDIArray;


  BEGIN
  (* fixme *)
    NewArrayRefOfMetadataRef
      (p.numFormals + 1, (*OUT*)paramsArr, (*OUT*)paramsMetadata);

    IF p.result = 0 THEN
      LDIDescr := M3DIB.LLVMDIDescriptorEmpty;
    ELSE
      LDIDescr := DebugLookupLL(self,p.result);
    END;
    paramsArr[0] := LDIDescr;

    IF p.numFormals > 0 THEN
      FOR i := 0 TO p.numFormals - 1 DO
        LDIDescr := DebugLookupLL(self,p.formals[i].tUid);
        paramsArr[i+1] := LDIDescr;
      END;
    END;

    paramsDIArr := M3DIB.DIBgetOrCreateArray (self.debugRef, paramsMetadata);

    LDISubprogTy 
      := M3DIB.DIBcreateSubroutineType
           (self.debugRef, self.fileRef, paramsDIArr);

    RETURN LDISubprogTy;
  END DebugProcType;

(* calc the size of all the supertypes *)
PROCEDURE BaseSize(self : U; o : ObjectDebug) : LONGINT =
VAR
  debugObj : REFANY;
  tUidExists : BOOLEAN;
  tid : TypeUID;
  size : LONGINT := 0L;
BEGIN
  tid := o.superType;
  REPEAT
    tUidExists := self.debugTable.get(tid, (*OUT*)debugObj);
    (* all supertypes should be objects or opaques. If opaque then
       cant tell size and its supertype may not exist in this module*)
    IF NOT tUidExists THEN RETURN size; END;

    TYPECASE debugObj OF
    | ObjectDebug(d) => INC(size,d.bitSize); tid := d.superType;
    | OpaqueDebug(d) => tid := d.superType; (* nothing dont know size *)
    ELSE
      <*ASSERT FALSE*>
    END;
  UNTIL tid = UID_ROOT;
  RETURN size;
END BaseSize;

PROCEDURE DebugIsObject(self : U; tUid : TypeUID) : ObjectDebug =
  VAR
    debugObj : REFANY;
    tUidExists : BOOLEAN;
  BEGIN
    tUidExists := self.debugTable.get(tUid, (*OUT*)debugObj);
    <*ASSERT tUidExists*>
    IF ISTYPE(debugObj,ObjectDebug) THEN
      RETURN NARROW(debugObj,ObjectDebug);
    ELSE
      RETURN NIL;
    END;
  END DebugIsObject;

PROCEDURE DebugObject(self : U; o : ObjectDebug) : M3DIB.LLVMDIDerivedType =
  VAR
    forwardClassDIT, heapObjectDIT, objectPtrDIT : M3DIB.LLVMDICompositeType; 
    supertypeDIT : M3DIB.LLVMDIDescriptor; 
    fieldDIT : M3DIB.LLVMDIType; 
    inheritDIT : M3DIB.LLVMDIDerivedType; 
    memberDINode : M3DIB.LLVMDIDerivedType; 
    (* ^Not really a type.  It contains lots of other info 
        about the member besides its type. *)  
    paramsArr : REF ARRAY OF MetadataRef; 
    paramsMetadata : LLVMTypes.ArrayRefOfMetadataRef; 
    paramsDIArr : M3DIB.LLVMDIArray;
    uniqueId : StringRef; 

    (* test finding the root obj *)
    nextMemberNo : CARDINAL := 0;
    tUidExists, hasSupertype : BOOLEAN;
    debugObj : REFANY;
    baseObj : BaseDebug;
  BEGIN
    IF self.m3llvmDebugLev > 0 THEN
      IO.Put("object debug\n");
    END; 

    tUidExists := self.debugTable.get(o.superType, (*OUT*)debugObj);
    <* ASSERT tUidExists *> (* Even if it's [UNTRACED]ROOT. *) 
    baseObj := NARROW(debugObj,BaseDebug);
    hasSupertype := FALSE;
    uniqueId := LTD(M3CG.FormatUID(o.tUid));
    NewArrayRefOfMetadataRef
      (o.numFields+1 (* +1 for supertype*), 
       (*OUT*) paramsArr, 
       (*OUT*) paramsMetadata);

    EnsureDebugTypeName(o); 
    EnsureDebugTypeName(baseObj); 

    (* Build a forward DIType decl for the eventual class DIType. *)
    forwardClassDIT 
      := M3DIB.DIBcreateReplaceableForwardDecl
           (self.debugRef,
            DC.DW_TAG_class_type,
            LTD(M3ID.ToText(o.typeName) & "__Forward"),
            self.funcRef,
            self.fileRef,
            self.curLine,
            RuntimeLang :=0,
            SizeInBits := VAL(0L,uint64_t),
            AlignInBits := VAL(0L,uint64_t),
            UniqueIdentifier := uniqueId);

    hasSupertype 
       := o.superType # NO_UID 
          AND o.superType # UID_ROOT 
          AND o.superType # UID_UNTRACED_ROOT;  
    IF hasSupertype 
    THEN (* o has a nontrivial supertype. *) 
      supertypeDIT := DebugLookupLL(self,o.superType);

      (* Create a DIBuilder inherit node connecting to the supertype
         and make it the first "param". *) 
      inheritDIT 
        := M3DIB.DIBcreateInheritance
             (self.debugRef,
              Ty := forwardClassDIT, 
              BaseTy := supertypeDIT,
              BaseOffset := VAL(0L,uint64_t),
              Flags := 0);
      paramsArr[0] := inheritDIT;
    ELSE 
      paramsArr[0] := NIL; (* Is this truly a C++ null? *)  
    END;
    nextMemberNo:= 1;

    FOR i := 0 TO o.numFields - 1 DO
      fieldDIT := DebugLookupLL(self,o.fields[i].tUid);
      memberDINode 
        := M3DIB.DIBcreateMemberType
             (self.debugRef,
              Scope := forwardClassDIT,
              Name := LTD(M3ID.ToText(o.fields[i].name)),
              File := self.fileRef,
              LineNo := self.curLine,
              SizeInBits := VAL(o.fields[i].bitSize,uint64_t),
              AlignInBits := VAL(o.fields[i].align,uint64_t),
              OffsetInBits 
                := VAL(o.fields[i].bitOffset + 64L(*For typecell pointer *),
                       uint64_t), 
              Flags := 0,
              Ty := fieldDIT);
      paramsArr[nextMemberNo] := memberDINode;
      INC(nextMemberNo);
    END;
    paramsDIArr 
      := M3DIB.DIBgetOrCreateArray(self.debugRef, paramsMetadata);

    heapObjectDIT 
      := M3DIB.DIBcreateClassType
           ( self.debugRef,
             Scope := self.funcRef, 
             Name := LTD(M3ID.ToText(o.typeName) & "__heapObj"),
             File := self.fileRef,
             LineNumber := self.curLine,
             SizeInBits 
                := VAL(o.fieldSize + 64L(*For typecell pointer *),uint64_t), 
             AlignInBits := VAL(o.align,uint64_t),
             OffsetInBits := VAL(0L,uint64_t), 
             Flags := 0,
             DerivedFrom := supertypeDIT,
             Elements := paramsDIArr,
             VTableHolder := M3DIB.LLVMDITypeEmpty,
             TemplateParms := NIL,
             UniqueIdentifier := uniqueId);
    M3DIB.replaceAllUsesWith(forwardClassDIT, heapObjectDIT); 

    objectPtrDIT 
       := M3DIB.DIBcreatePointerType
            ( self.debugRef,
              PointeeTy := heapObjectDIT,
              SizeInBits := VAL(ptrBits,uint64_t),
              AlignInBits := VAL(ptrBits,uint64_t),
              Name := LTD(M3ID.ToText(o.typeName)));
    RETURN objectPtrDIT;

  END DebugObject;
 
PROCEDURE DebugRecord(self : U; r : RecordDebug) : M3DIB.LLVMDICompositeType =
  VAR
    forwardStructDIT, structDIT : M3DIB.LLVMDICompositeType; 
    fieldDIT : M3DIB.LLVMDIType; 
    memberDIT : M3DIB.LLVMDIDerivedType; 
    paramsArr : REF ARRAY OF MetadataRef; 
    paramsMetadata : LLVMTypes.ArrayRefOfMetadataRef; 
    paramsDIArr : M3DIB.LLVMDIArray;
    uniqueId : StringRef; 
  BEGIN
    IF self.m3llvmDebugLev > 0 THEN
      IO.Put("record debug\n");
    END; 

    uniqueId := LTD(M3CG.FormatUID(r.tUid));
    NewArrayRefOfMetadataRef
      (r.numFields, (*OUT*)paramsArr, (*OUT*)paramsMetadata);
    EnsureDebugTypeName(r); 
    forwardStructDIT 
      := M3DIB.DIBcreateReplaceableForwardDecl
           (self.debugRef,
            DC.DW_TAG_structure_type,
            LTD(M3ID.ToText(r.typeName) & "__Forward"),
            self.funcRef,
            self.fileRef,
            self.curLine,
            RuntimeLang :=0,
            SizeInBits := VAL(0L,uint64_t),
            AlignInBits := VAL(0L,uint64_t),
            UniqueIdentifier := uniqueId);
    FOR i := 0 TO r.numFields - 1 DO
      fieldDIT := DebugLookupLL(self,r.fields[i].tUid);
      memberDIT := M3DIB.DIBcreateMemberType(
                     self.debugRef,
                     Scope := forwardStructDIT,
                     Name := LTD(M3ID.ToText(r.fields[i].name)),
                     File := self.fileRef,
                     LineNo := self.curLine,
                     SizeInBits := VAL(r.fields[i].bitSize,uint64_t),
                     AlignInBits := VAL(r.fields[i].align,uint64_t),
                     OffsetInBits := VAL(r.fields[i].bitOffset,uint64_t),
                     Flags := 0,
                     Ty := fieldDIT);

      paramsArr[i] := memberDIT;
    END;
    paramsDIArr := M3DIB.DIBgetOrCreateArray(self.debugRef, paramsMetadata);

    structDIT 
      := M3DIB.DIBcreateStructType
           ( self.debugRef,
             Scope := self.funcRef,
             Name := LTD(M3ID.ToText(r.typeName)),
             File := self.fileRef,
             LineNumber := self.curLine,
             SizeInBits := VAL(r.bitSize,uint64_t),
             AlignInBits := VAL(r.align,uint64_t),
             Flags := 0,
             DerivedFrom := M3DIB.LLVMDITypeEmpty,
             Elements := paramsDIArr,
             RunTimeLang := 0,
             VTableHolder := M3DIB.LLVMDITypeEmpty,
             UniqueIdentifier := uniqueId);
    RETURN structDIT; 
  END DebugRecord;

PROCEDURE DebugPointer(self : U; p : PointerDebug) : M3DIB.LLVMDIDerivedType =
  VAR
    referentDIType : M3DIB.LLVMDIDescriptor;
  BEGIN
    referentDIType := DebugLookupLL(self,p.target);
    EnsureDebugTypeName(p); 
    RETURN M3DIB.DIBcreatePointerType(
              self.debugRef,
               referentDIType,
               VAL(p.bitSize,uint64_t),
               VAL(p.align,uint64_t),
               LTD(M3ID.ToText(p.typeName)));
  END DebugPointer;

PROCEDURE DebugBasic(self : U; b : BaseDebug) : M3DIB.LLVMDIBasicType =
  BEGIN
    EnsureDebugTypeName(b); 
    RETURN M3DIB.DIBcreateBasicType
      (self.debugRef,
       LTD(M3ID.ToText(b.typeName)), 
       VAL(b.bitSize,uint64_t), 
       VAL(b.align,uint64_t), 
       b.encoding);
  END DebugBasic;

PROCEDURE DebugLookupML(self : U; tUid : TypeUID) : BaseDebug =
  VAR
    debugObj : REFANY;
    baseObj : BaseDebug;
    tUidExists : BOOLEAN;
    LDIDescr : M3DIB.LLVMDIDescriptor := NIL; 
    (* lVal : LLVM.ValueRef; *) 
  BEGIN
    IF self.m3llvmDebugLev > 0 THEN
      IO.Put("tid>>"); IO.PutInt(tUid); IO.Put(" "); IO.Put("<< ");
    END; 

    (* exceptions have 0 tUid *)
    IF tUid = 0 THEN RETURN NIL; END;

    tUidExists := self.debugTable.get(tUid, (*OUT*)debugObj);
    <*ASSERT tUidExists*>
    baseObj := NARROW(debugObj,BaseDebug);
    LDIDescr := baseObj.DIDescr;

    IF LDIDescr # M3DIB.LLVMDIDescriptorEmpty 
    THEN (* This ML node already has been completed. *) 
      RETURN baseObj;
    ELSE (* Complete this ML node's dependencies, then itself. *) 
      TYPECASE baseObj OF
      | ArrayDebug(d) => LDIDescr := DebugArray(self,d);
      | OpenArrayDebug(d) => LDIDescr := DebugOpenArray(self,d);
      | SetDebug(d) =>  LDIDescr := DebugSet(self,d);
      | EnumDebug(d) => LDIDescr := DebugEnum(self,d);
      | OpaqueDebug(d) => LDIDescr := DebugOpaque(self,d);
      | PackedDebug(d) => LDIDescr := DebugPacked(self,d);
      | ObjectDebug(d) => LDIDescr := DebugObject(self,d);
      | RecordDebug(d) => LDIDescr := DebugRecord(self,d);
      | PointerDebug(d) => LDIDescr := DebugPointer(self,d);
      | ProcTypeDebug(d) => LDIDescr := DebugProcType(self,d);
      | IndirectDebug(d) => LDIDescr := DebugLookupLL(self,d.target);
      | SubrangeDebug(d) => LDIDescr := DebugSubrange(self,d);
      | BaseDebug(d) => LDIDescr := DebugBasic(self,d); 
      END;
      <* ASSERT LDIDescr # NIL *> 
      baseObj.DIDescr := LDIDescr;
      RETURN baseObj;
    END;
  END DebugLookupML;

PROCEDURE DebugLookupLL(self : U; tUid : TypeUID) : M3DIB.LLVMDIDescriptor =
  VAR MLDebug : BaseDebug;
  BEGIN
    MLDebug := DebugLookupML(self, tUid); 
    IF MLDebug = NIL THEN RETURN NIL;
    ELSE RETURN MLDebug.DIDescr; 
    END; 
  END DebugLookupLL;

(* debug for locals and parms *)
PROCEDURE DebugVar(self : U; v : LvVar; argNum : CARDINAL := 0) =
  VAR
    loc : LLVM.ValueRef;
    scope : M3DIB.LLVMDISubprogram; 
    blockRef : BlockDebug;
    decl : LLVMTypes.InstructionRef;
    lvDebug : M3DIB.LLVMDIVariable; 
    tyVal : M3DIB.LLVMDIType; 
    dwarfTag,flags : CARDINAL := 0;
    name : TEXT;
    debugObj : ObjectDebug;
  BEGIN
    IF NOT self.genDebug THEN RETURN; END;

    name := VName(v,TRUE);
    (* Dont debug temps or _result or the static link *)
(* Review: Actually probably so for static link, and maybe result. *) 
    IF v.name = M3ID.NoID OR
       Text.Equal(name,"_result") OR
       Text.Equal(name,"_link") THEN RETURN;
    END;

    IF v.varType = VarType.Param THEN
      dwarfTag := DC.DW_TAG_arg_variable;
    ELSE
      dwarfTag := DC.DW_TAG_auto_variable;
    END;

    tyVal := DebugLookupLL(self,v.m3t);

    debugObj := DebugIsObject(self,v.m3t);
    IF debugObj # NIL THEN
      EnsureDebugTypeName(debugObj); 
      tyVal := M3DIB.DIBcreatePointerType(
                                  self.debugRef,
                                  tyVal,
                                  VAL(ptrBits,uint64_t),
                                  VAL(ptrBits,uint64_t),
                                  LTD(M3ID.ToText(debugObj.typeName)));
    END;

    blockRef := Get(self.debugLexStack);
    scope := blockRef.value;

    lvDebug := M3DIB.DIBcreateLocalVariable
       (self.debugRef, dwarfTag, scope, LTD(name), self.fileRef, self.curLine, tyVal, FALSE, flags, argNum);

    (* we need this since setinstdebuglocation has to have a current loc *)
    decl := M3DIB.DIBinsertDeclareAtEnd(
              self.debugRef, 
              v.lv,
              lvDebug,
              M3DIB.LLVMDIExpressionEmpty, 
              LLVM.LLVMGetInsertBlock(builderIR)
              );

    LLVM.LLVMSetInstDebugLocation
      (builderIR, LOOPHOLE (decl, LLVM.ValueRef));
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
    DebugLine(self);
    loc := M3DIB.DIBgetDebugLoc(self.curLine,0,self.funcRef);
    LLVM.LLVMSetCurrentDebugLocation(builderIR, loc);
  END DebugLocalsParams;

PROCEDURE FindGlobal(var : LvVar; ofs : CARDINAL) : LLVM.ValueRef =
  VAR
    numGlobs : CARDINAL;
    baseObj : BaseVar;
  BEGIN
    (* search the segment (var) for a global at offset ofs *)
    numGlobs := var.inits.size();
    FOR i := 0 TO numGlobs - 1 DO
      baseObj := Get(var.inits,i);
      IF baseObj.offset = ofs THEN
        TYPECASE baseObj OF
          (* this is a large global not in the global segment *)
          VarVar(v) =>
            RETURN NARROW(v.value,LvVar).lv;
        ELSE
        END;
      END;
    END;
    RETURN NIL;
  END FindGlobal;

PROCEDURE DebugGlobals(self : U; var : LvVar) =
  VAR
    globLv : LLVM.ValueRef;
    globType : M3DIB.LLVMDIDescriptor; 
    iter : IntRefTbl.Iterator;
    globRec : RecordDebug;
    key,byteOfs : INTEGER;
    debugObj : REFANY;
    ty : M3DIB.LLVMDIDescriptor;
  BEGIN
(* large globals ok but segment globals not working at all - seems to create the correct metadata but gdb says its optimized out *)
    IF NOT self.genDebug THEN RETURN; END;

    iter := self.debugTable.iterate();
    WHILE iter.next(key, debugObj) DO
      IF ISTYPE(debugObj,RecordDebug) THEN
        globRec := NARROW(debugObj,RecordDebug);
        IF globRec.global THEN

          FOR i := 0 TO globRec.numFields - 1 DO
            byteOfs := VAL(globRec.fields[i].bitOffset,INTEGER) DIV 8;

            globLv := FindGlobal(globRec.var,byteOfs);
            IF globLv = NIL THEN
              (* think we need proper type not just i8* *)
              globLv := BuildGep(globRec.var.lv, byteOfs);
            END;
            (* else its a proper llvm global *)

            globType := DebugLookupLL(self,globRec.fields[i].tUid);
            IF globType # M3DIB.LLVMDIDescriptorEmpty THEN
              EVAL M3DIB.DIBcreateGlobalVariable(
                      self.debugRef,
                      self.cuRef, 
                      LTD(M3ID.ToText(var.name)), (*Name*) 
                      LTD(M3ID.ToText(var.name)), (*LinkageName*) (*Correct?*) 
                      self.fileRef,
                      0,
                      globType,
                      NOT var.exported, (*IsLocalToUnit*)
                      LOOPHOLE(var.lv,LLVM.ConstantRef),
                      NIL (*Corresponding (forward?) decl, an MDNodeRef.*) ); 

(*
this does not help either
              EVAL LLVM.LLVMDIBuilderCreateStaticVariable(
            lVal := M3DIB.DIBcreateGlobalVariable(
                      self.debugRef,
                      self.cuRef, 
                      LTD(M3ID.ToText(glob.fields[i].name)), (*Name*)
                      LTD(M3ID.ToText(glob.fields[i].name)), (*LinkageName*) (*Correct?*) 
                      self.fileRef,
                      0, (*self.curLine,*)
                      ty,
                      NOT var.exported, (*IsLocalToUnit*)
                      LOOPHOLE(gep,LLVM.ConstantRef) (*globAlias*),
                      NIL(*Corresponding (forward?) decl, an MDNodeRef.*) );
*)
            END;
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

(* not used yet
PROCEDURE AddInstrMD(name,val : TEXT) =
  VAR
    mdNode,lastInstr : LLVM.ValueRef;
    curBB : LLVM.BasicBlockRef;
    kindId : CARDINAL;
    strMD : ARRAY[0..0] OF REFANY;
  BEGIN
    curBB := LLVM.LLVMGetInsertBlock(builderIR);
    IF curBB = NIL THEN RETURN; END;
    lastInstr := LLVM.LLVMGetLastInstruction(curBB);
    IF lastInstr = NIL THEN RETURN; END;

    kindId := LLVM.LLVMGetMDKindID(LT(name),Text.Length(name));
    strMD[0] := val;
    mdNode := GetMDNode(strMD);

    LLVM.LLVMSetMetadata(lastInstr, kindId, mdNode);
  END AddInstrMD;
*)
BEGIN
END M3CG_LLVM.
