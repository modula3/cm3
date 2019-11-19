(* File: LLVM.i3                                                              *|
|* Modula 3 code.                                                             *|
|* Derived from:                                                              *)
(*===-- llvm-c/Core.h - Core Library C Interface ------------------*- C -*-===*\
|*                                                                            *|
|*                     The LLVM Compiler Infrastructure                       *|
|*                                                                            *|
|* This file is distributed under the University of Illinois Open Source      *|
|* License. See LICENSE.TXT for details.                                      *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This header declares the C interface to libLLVMCore.a, which implements    *|
|* the LLVM intermediate representation.                                      *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*)

<*EXTERNAL*> INTERFACE LLVM;

FROM Cstdint IMPORT uint8_t, uint32_t, uint64_t;
FROM Ctypes IMPORT int, unsigned, char_star, 
                   const_char_star, char_star_star,
                   unsigned_long_long, double, long_long;
IMPORT Word;

FROM LLVMTypes IMPORT Bool, False, MetadataRef; 
(**
 * @defgroup LLVMC LLVM-C: C interface to LLVM
 *
 * This module exposes parts of the LLVM library as a C API.
 *
 * @{
 *)

(**
 * @defgroup LLVMCTransforms Transforms
 *)

(**
 * @defgroup LLVMCCore Core
 *
 * This modules provide an interface to libLLVMCore, which implements
 * the LLVM intermediate representation as well as other related types
 * and utilities.
 *
 * LLVM uses a polymorphic type hierarchy which C cannot represent, therefore
 * parameters must be passed as base types. Despite the declared types, most
 * of the functions provided operate only on branches of the type hierarchy.
 * The declared parameter names are descriptive and specify which type is
 * required. Additionally, each type hierarchy is documented along with the
 * functions that operate upon it. For more detail, refer to LLVM's C++ code.
 * If in doubt, refer to Core.cpp, which performs parameter downcasts in the
 * form unwrap<RequiredType>(Param).
 *
 * Many exotic languages can interoperate with C code but have a harder time
 * with C++ due to name mangling. So in addition to C, this interface enables
 * tools written in such languages.
 *
 * When included into a C++ source file, also declares 'wrap' and 'unwrap'
 * helpers to perform opaque reference<-->pointer conversions. These helpers
 * are shorter and more tightly typed than writing the casts by hand when
 * authoring bindings. In assert builds, they will do runtime type checking.
 *
 * @{
 *)

(**
 * @defgroup LLVMCCoreTypes Types and Enumerations
 *
 * @{
 *)

(* Opaque types. *)

TYPE Opaque = RECORD END;

(**
 * The top-level container for all LLVM global data. See the LLVMContext class.
 *)
TYPE ContextRef = UNTRACED BRANDED "LLVMOpaqueContext" REF Opaque;

(**
 * The top-level container for all other LLVM Intermediate Representation (IR)
 * objects.
 *
 * @see llvm::Module
 *)
TYPE ModuleRef = UNTRACED BRANDED "LLVMOpaqueModule" REF Opaque;

(**
 * Each value in the LLVM IR has a type, an LLVMTypeRef.
 *
 * @see llvm::Type
 *)
TYPE TypeRef = UNTRACED BRANDED "LLVMOpaqueType" REF Opaque;

(**
 * Represents an individual value in LLVM IR.
 *
 * This models llvm::Value.
 *)
TYPE ValueRef = UNTRACED BRANDED "LLVMOpaqueValue" REF Opaque;

(**
 * Represents an individual attribute in LLVM IR.
 *
 * This models llvm::Attribute.
 *)
TYPE AttributeRef = UNTRACED BRANDED "LLVMOpaqueAttribute" REF Opaque;

(* Currently unused ---- : 
(* From llvm/ADT/ArrayRef.h: *) 

(* class ArrayRef<Value *> (NOTE: this is not a pointer). *) 
; TYPE ArrayRefOfValueRef 
    = RECORD 
        Data : UNTRACED REF ValueRef (* 0th element, a la C/C++ *) 
      ; Length : size_t
      END 
; CONST ArrayRefOfValueRefEmpty 
    = ArrayRefOfValueRef { Data := NIL , Length := 0 }

---- unused. *) 

(**
 * Represents a constant in LLVM IR.
 *
 * This models llvm::Constant.
 *)
TYPE ConstantRef = UNTRACED BRANDED "LLVMOpaqueConstant" REF Opaque;

(**
 * Represents a basic block of instructions in LLVM IR.
 *
 * This models llvm::BasicBlock.
 *)
TYPE BasicBlockRef = UNTRACED BRANDED "LLVMOpaqueBasicBlock" REF Opaque;

(**
 * Represents an LLVM basic block builder.
 *
 * This models llvm::IRBuilder.
 *)
TYPE BuilderRef = UNTRACED BRANDED "LLVMOpaqueBuilder" REF Opaque;

(**
 * Interface used to provide a module to JIT or interpreter.
 * This is now just a synonym for llvm::Module, but we have to keep using the
 * different type to keep binary compatibility.
 *)
TYPE ModuleProviderRef = UNTRACED BRANDED "LLVMOpaqueModuleProvider" REF Opaque;

(**
 * Used to provide a module to JIT or interpreter.
 *
 * @see llvm::MemoryBuffer
 *)
TYPE MemoryBufferRef = UNTRACED BRANDED "LLVMOpaqueMemoryBuffer" REF Opaque;

(** @see llvm::PassManagerBase *)
TYPE PassManagerRef = UNTRACED BRANDED "LLVMOpaquePassManager" REF Opaque;

(** @see llvm::PassRegistry *)
TYPE PassRegistryRef = UNTRACED BRANDED "LLVMOpaquePassRegistry" REF Opaque;

(**
 * Used to get the users and usees of a Value.
 *
 * @see llvm::Use *)
TYPE UseRef = UNTRACED BRANDED "LLVMOpaqueUse" REF Opaque;

(* 8.0 *)
TYPE ModuleFlagEntryRef = UNTRACED BRANDED "LLVMModulFlagEntry" REF Opaque;
TYPE NamedMDNodeRef = UNTRACED BRANDED "LLVMNamedMDNode" REF Opaque;
TYPE ValueMetadataEntryRef = UNTRACED BRANDED "LLVMValueMetadataEntry" REF Opaque;
TYPE DiagnosticInfoRef = UNTRACED BRANDED "LLVMDiagnosticInfo" REF Opaque;

(* for Debug *)
TYPE LLVMDIScope = MetadataRef;

TYPE Opcode = {
  (* Terminator Instructions *)
  XXXXX0,               (*  0 *)
  Ret,                  (*  1 *)
  Br,                   (*  2 *)
  Switch,               (*  3 *)
  IndirectBr,           (*  4 *)
  Invoke,               (*  5 *)
  XXXXX6,               (* removed 6 due to API changes *)
  Unreachable,          (*  7 *)

  (* Standard Binary Operators *)
  Add,                  (*  8 *)
  FAdd,                 (*  9 *)
  Sub,                  (* 10 *)
  FSub,                 (* 11 *)
  Mul,                  (* 12 *)
  FMul,                 (* 13 *)
  UDiv,                 (* 14 *)
  SDiv,                 (* 15 *)
  FDiv,                 (* 16 *)
  URem,                 (* 17 *)
  SRem,                 (* 18 *)
  FRem,                 (* 19 *)

  (* Logical Operators *)
  Shl,                  (* 20 *)
  LShr,                 (* 21 *)
  AShr,                 (* 22 *)
  And,                  (* 23 *)
  Or,                   (* 24 *)
  Xor,                  (* 25 *)

  (* Memory Operators *)
  Alloca,               (* 26 *)
  Load,                 (* 27 *)
  Store,                (* 28 *)
  GetElementPtr,        (* 29 *)

  (* Cast Operators *)
  Trunc,                (* 30 *)
  ZExt,                 (* 31 *)
  SExt,                 (* 32 *)
  FPToUI,               (* 33 *)
  FPToSI,               (* 34 *)
  UIToFP,               (* 35 *)
  SIToFP,               (* 36 *)
  FPTrunc,              (* 37 *)
  FPExt,                (* 38 *)
  PtrToInt,             (* 39 *)
  IntToPtr,             (* 40 *)
  BitCast,              (* 41 *)

  (* Other Operators *)
  ICmp,                 (* 42 *)
  FCmp,                 (* 43 *)
  PHI,                  (* 44 *)
  Call,                 (* 45 *)
  Select,               (* 46 *)
  UserOp1,              (* 47 *)
  UserOp2,              (* 48 *)
  VAArg,                (* 49 *)
  ExtractElement,       (* 50 *)
  InsertElement,        (* 51 *)
  ShuffleVector,        (* 52 *)
  ExtractValue,         (* 53 *)
  InsertValue,          (* 54 *)

  (* Atomic operators *)
  Fence,                (* 55 *)
  AtomicCmpXchg,        (* 56 *)
  AtomicRMW,            (* 57 *)

  (* Exception Handling Operators *)
  Resume,               (* 58 *)
  LandingPad,           (* 59 *)
  
  (* Added *)
  AddrSpaceCast,        (* 60 *)
  
  (* Exception Handling Operators again *)  
  CleanupRet,           (* 61 *)
  CatchRet,             (* 62 *)
  CatchPad,             (* 63 *)
  CleanupPad,           (* 64 *)
  CatchSwitch,          (* 65 *) 
  
  (* Standard Unary Operators *)
  FNeg,                 (* 66 *)

  (* Added *)
  CallBr                (* 67 *)
  
};

TYPE TypeKind = {
  Void,        (**< type with no size *)
  Half,        (**< 16 bit floating point type *)
  Float,       (**< 32 bit floating point type *)
  Double,      (**< 64 bit floating point type *)
  X86_FP80,    (**< 80 bit floating point type (X87) *)
  FP128,       (**< 128 bit floating point type (112-bit mantissa)*)
  PPC_FP128,   (**< 128 bit floating point type (two 64-bits) *)
  Label,       (**< Labels *)
  Integer,     (**< Arbitrary bit width integers *)
  Function,    (**< Functions *)
  Struct,      (**< Structures *)
  Array,       (**< Arrays *)
  Pointer,     (**< Pointers *)
  Vector,      (**< SIMD 'packed' format, or other vector type *)
  Metadata,    (**< Metadata *)
  X86_MMX,     (**< X86 MMX *)
  Token        (**< Tokens *)  
};

TYPE Linkage = {
  External,    (**< Externally visible function *)
  AvailableExternally,
  LinkOnceAny, (**< Keep one copy of function when linking (inline)*)
  LinkOnceODR, (**< Same, but only replaced by something equivalent. *)
  LinkOnceODRAutoHide, (**< Like LinkOnceODR, but possibly hidden. *)
  WeakAny,     (**< Keep one copy of function when linking (weak) *)
  WeakODR,     (**< Same, but only replaced by something equivalent. *)
  Appending,   (**< Special purpose, only applies to global arrays *)
  Internal,    (**< Rename collisions when linking (static functions) *)
  Private,     (**< Like Internal, but omit from symbol table *)
  DLLImport,   (**< Function to be imported from DLL *)
  DLLExport,   (**< Function to be accessible from DLL *)
  ExternalWeak,(**< ExternalWeak linkage description *)
  Ghost,       (**< Obsolete *)
  Common,      (**< Tentative definitions *)
  LinkerPrivate, (**< Like Private, but linker removes. *)
  LinkerPrivateWeak (**< Like LinkerPrivate, but is weak. *)
};

TYPE Visibility = {
  Default,  (**< The GV is visible *)
  Hidden,   (**< The GV is hidden *)
  Protected (**< The GV is protected *)
};

TYPE UnnamedAddr = {
 No,
 Local,
 Global};
 
TYPE DLLStorageClass = {
  Default,
  DLLImport, (**< Function to be imported from DLL. *)
  DLLExport  (**< Function to be accessible from DLL. *)
};

CONST
  CCallConv           = 0;
  FastCallConv        = 8;
  ColdCallConv        = 9;
  WebKitJSCallConv    = 12;
  AnyRegCallConv      = 13;  
  X86StdcallCallConv  = 64;
  X86FastcallCallConv = 65;
  
TYPE ValueKind = {
  Argument,
  BasicBlock,
  MemoryUse,
  MemoryDef,
  MemoryPhi,

  Function,
  GlobalAlias,
  GlobalIFunc,
  GlobalVariable,
  BlockAddress,
  ConstantExpr,
  ConstantArray,
  ConstantStruct,
  ConstantVector,

  UndefValue,
  ConstantAggregateZero,
  ConstantDataArray,
  ConstantDataVector,
  ConstantInt,
  ConstantFP,
  ConstantPointerNull,
  ConstantTokenNone,

  MetadataAsValue,
  InlineAsm,

  Instruction
};

TYPE IntPredicate = {
  XXX00, XXX01, XXX02, XXX03, XXX04, XXX05, XXX06, XXX07,
  XXX08, XXX09, XXX10, XXX11, XXX12, XXX13, XXX14, XXX15,
  XXX16, XXX17, XXX18, XXX19, XXX20, XXX21, XXX22, XXX23,
  XXX24, XXX25, XXX26, XXX27, XXX28, XXX29, XXX30, XXX31,
  (* 32 *)
  EQ,  (**< equal *)
  NE,  (**< not equal *)
  UGT, (**< unsigned greater than *)
  UGE, (**< unsigned greater or equal *)
  ULT, (**< unsigned less than *)
  ULE, (**< unsigned less or equal *)
  SGT, (**< signed greater than *)
  SGE, (**< signed greater or equal *)
  SLT, (**< signed less than *)
  SLE  (**< signed less or equal *)
};

TYPE RealPredicate = {
  PredicateFalse, (**< Always false (always folded) *)
  OEQ,            (**< True if ordered and equal *)
  OGT,            (**< True if ordered and greater than *)
  OGE,            (**< True if ordered and greater than or equal *)
  OLT,            (**< True if ordered and less than *)
  OLE,            (**< True if ordered and less than or equal *)
  ONE,            (**< True if ordered and operands are unequal *)
  Ord,            (**< True if ordered (no nans) *)
  Uno,            (**< True if unordered: isnan(X) | isnan(Y) *)
  UEQ,            (**< True if unordered or equal *)
  UGT,            (**< True if unordered or greater than *)
  UGE,            (**< True if unordered, greater than, or equal *)
  ULT,            (**< True if unordered or less than *)
  ULE,            (**< True if unordered, less than, or equal *)
  UNE,            (**< True if unordered or not equal *)
  PredicateTrue   (**< Always true (always folded) *)
};

TYPE LandingPadClauseTy = {
  Catch,    (**< A catch clause   *)
  Filter    (**< A filter clause  *)
};

TYPE ThreadLocalMode = {
  NotThreadLocal,
  GeneralDynamicTLSModel,
  LocalDynamicTLSModel,
  InitialExecTLSModel,
  LocalExecTLSModel
};

TYPE AtomicOrdering = {
  LLVMAtomicOrderingNotAtomic, (** 0 < A load or store which is not atomic *)
  LLVMAtomicOrderingUnordered, (** 1 < Lowest level of atomicity, guarantees
                                     somewhat sane results, lock free. *)

  LLVMAtomicOrderingMonotonic, (** 2 < guarantees that if you take all the
                                     operations affecting a specific address,
                                     a consistent ordering exists *)
  XXXXX3,  
  LLVMAtomicOrderingAcquire, (** 4 < Acquire provides a barrier of the sort
                                   necessary to acquire a lock to access other
                                   memory with normal loads and stores. *)
  LLVMAtomicOrderingRelease, (** 5 < Release is similar to Acquire, but with
                                   a barrier of the sort necessary to release
                                   a lock. *)
  LLVMAtomicOrderingAcquireRelease, (** 6 < provides both an Acquire and a
                                          Release barrier (for fences and
                                          operations which both read and write
                                           memory). *)
  LLVMAtomicOrderingSequentiallyConsistent (** 7 < provides Acquire semantics
                                                 for loads and Release
                                                 semantics for stores.
                                                 Additionally, it guarantees
                                                 that a total ordering exists
                                                 between all
                                                 SequentiallyConsistent
                                                 operations. *)
};

TYPE AtomicRMWBinOp = {
    LLVMAtomicRMWBinOpXchg, (** < Set the new value and return the one old *)
    LLVMAtomicRMWBinOpAdd, (** < Add a value and return the old one *)
    LLVMAtomicRMWBinOpSub, (** < Subtract a value and return the old one *)
    LLVMAtomicRMWBinOpAnd, (** < And a value and return the old one *)
    LLVMAtomicRMWBinOpNand, (** < Not-And a value and return the old one *)
    LLVMAtomicRMWBinOpOr, (** < OR a value and return the old one *)
    LLVMAtomicRMWBinOpXor, (** < Xor a value and return the old one *)
    LLVMAtomicRMWBinOpMax, (** < Sets the value if it's greater than the
                             original using a signed comparison and return
                             the old one *)
    LLVMAtomicRMWBinOpMin, (** < Sets the value if it's Smaller than the
                             original using a signed comparison and return
                             the old one *)
    LLVMAtomicRMWBinOpUMax, (** < Sets the value if it's greater than the
                             original using an unsigned comparison and return
                             the old one *)
    LLVMAtomicRMWBinOpUMin (** < Sets the value if it's greater than the
                             original using an unsigned comparison  and return
                             the old one *)
};

TYPE DiagnosticSeverity = {
    DSError,
    DSWarning,
    DSRemark,
    DSNote
};

TYPE InlineAsmDialect = {
    InlineAsmDialectATT,
    InlineAsmDialectIntel
};

TYPE ModuleFlagBehavior = {
  (**
   * Emits an error if two values disagree, otherwise the resulting value is
   * that of the operands.
   *
   * @see Module::ModFlagBehavior::Error
   *)
  ModuleFlagBehaviorError,
  (**
   * Emits a warning if two values disagree. The result value will be the
   * operand for the flag from the first module being linked.
   *
   * @see Module::ModFlagBehavior::Warning
   *)
  ModuleFlagBehaviorWarning,
  (**
   * Adds a requirement that another module flag be present and have a
   * specified value after linking is performed. The value must be a metadata
   * pair, where the first element of the pair is the ID of the module flag
   * to be restricted, and the second element of the pair is the value the
   * module flag should be restricted to. This behavior can be used to
   * restrict the allowable results (via triggering of an error) of linking
   * IDs with the **Override** behavior.
   *
   * @see Module::ModFlagBehavior::Require
   *)
  ModuleFlagBehaviorRequire,
  (**
   * Uses the specified value, regardless of the behavior or value of the
   * other module. If both modules specify **Override**, but the values
   * differ, an error will be emitted.
   *
   * @see Module::ModFlagBehavior::Override
   *)
  ModuleFlagBehaviorOverride,
  (**
   * Appends the two values, which are required to be metadata nodes.
   *
   * @see Module::ModFlagBehavior::Append
   *)
  ModuleFlagBehaviorAppend,
  (**
   * Appends the two values, which are required to be metadata
   * nodes. However, duplicate entries in the second list are dropped
   * during the append operation.
   *
   * @see Module::ModFlagBehavior::AppendUnique
   *)
  ModuleFlagBehaviorAppendUnique
};


(**
 * Attribute index are either LLVMAttributeReturnIndex,
 * LLVMAttributeFunctionIndex or a parameter number from 1 to N.
 *)
CONST
  AttributeReturnIndex = 0;
  (* ISO C restricts enumerator values to range of 'int'
  // (4294967295 is too large)
  // LLVMAttributeFunctionIndex = ~0U *)
  AttributeFunctionIndex = -1;

TYPE
  AttributeIndex = Word.T;

(**
 * @}
 *)

PROCEDURE LLVMInitializeCore(R: PassRegistryRef);

(** Deallocate and destroy all ManagedStatic variables.
    @see llvm::llvm_shutdown
    @see ManagedStatic *)
PROCEDURE LLVMShutdown();

(*===-- Error handling ----------------------------------------------------===*)

PROCEDURE LLVMCreateMessage(Message : const_char_star) : char_star;
PROCEDURE LLVMDisposeMessage(Message: char_star);


(**
 * @defgroup LLVMCCoreContext Contexts
 *
 * Contexts are execution states for the core LLVM IR system.
 *
 * Most types are tied to a context instance. Multiple contexts can
 * exist simultaneously. A single context is not thread safe. However,
 * different contexts can execute on different threads simultaneously.
 *
 * @{
 *)

(**
 * Create a new context.
 *
 * Every call to this function should be paired with a call to
 * LLVMContextDispose() or the context will leak memory.
 *)
PROCEDURE LLVMContextCreate(): ContextRef;

(**
 * Obtain the global context instance.
 *)
PROCEDURE LLVMGetGlobalContext(): ContextRef;

TYPE
  DiagnosticHandler = PROCEDURE(info : DiagnosticInfoRef; parm : ADDRESS) : ADDRESS;
  YieldCallback  = PROCEDURE(info : ContextRef; parm : ADDRESS) : ADDRESS;

(**
 * Set the diagnostic handler for this context.
 *)
PROCEDURE LLVMContextSetDiagnosticHandler(C : ContextRef;
                                     Handler : DiagnosticHandler;
                                     DiagnosticContext : ADDRESS);

(**
 * Get the diagnostic handler of this context.
 *)
PROCEDURE LLVMContextGetDiagnosticHandler(C : ContextRef) : DiagnosticHandler;

(**
 * Get the diagnostic context of this context.
 *)
PROCEDURE LLVMContextGetDiagnosticContext(C : ContextRef) : ADDRESS;

(**
 * Set the yield callback function for this context.
 *
 * @see LLVMContext::setYieldCallback()
 *)
PROCEDURE LLVMContextSetYieldCallback(C : ContextRef; 
                                      Callback : YieldCallback;
                                      OpaqueHandle : ADDRESS);

(**
 * Retrieve whether the given context is set to discard all value names.
 *
 * @see LLVMContext::shouldDiscardValueNames()
 *)
PROCEDURE LLVMContextShouldDiscardValueNames(C : ContextRef) : Bool;

(**
 * Set whether the given context discards all value names.
 *
 * If true, only the names of GlobalValue objects will be available in the IR.
 * This can be used to save memory and runtime, especially in release mode.
 *
 * @see LLVMContext::setDiscardValueNames()
 *)
PROCEDURE LLVMContextSetDiscardValueNames(C : ContextRef; Discard : Bool);


(**
 * Destroy a context instance.
 *
 * This should be called for every call to LLVMContextCreate() or memory
 * will be leaked.
 *)
PROCEDURE LLVMContextDispose(C: ContextRef);

(**
 * Return a string representation of the DiagnosticInfo. Use
 * LLVMDisposeMessage to free the string.
 *
 * @see DiagnosticInfo::print()
 *)
PROCEDURE LLVMGetDiagInfoDescription (DI: DiagnosticInfoRef;
): char_star;

(**
 * Return an enum DiagnosticSeverity.
 *
 * @see DiagnosticInfo::getSeverity()
 *)
PROCEDURE LLVMGetDiagInfoSeverity (DI: DiagnosticInfoRef;
): DiagnosticSeverity;

PROCEDURE LLVMGetMDKindIDInContext(
    C: ContextRef; Name: const_char_star; SLen: unsigned): unsigned;
PROCEDURE LLVMGetMDKindID(Name: const_char_star; SLen: unsigned): unsigned;

(**
 * Return an unique id given the name of a enum attribute,
 * or 0 if no attribute by that name exists.
 *
 * See http://llvm.org/docs/LangRef.html#parameter-attributes
 * and http://llvm.org/docs/LangRef.html#function-attributes
 * for the list of available attributes.
 *
 * NB: Attribute names and/or id are subject to change without
 * going through the C API deprecation cycle.
 *)
PROCEDURE LLVMGetEnumAttributeKindForName(Name : const_char_star; SLen : unsigned) : unsigned;
PROCEDURE LLVMGetLastEnumAttributeKind() : unsigned;

(**
 * Create an enum attribute.
 *)
PROCEDURE LLVMCreateEnumAttribute(C : ContextRef; KindID : unsigned; Val : uint64_t) : AttributeRef;
                                        
(**
 * Get the unique id corresponding to the enum attribute
 * passed as argument.
 *)
PROCEDURE LLVMGetEnumAttributeKind(A : AttributeRef) : unsigned;

(**
 * Get the enum attribute's value. 0 is returned if none exists.
 *)
PROCEDURE LLVMGetEnumAttributeValue(A : AttributeRef) : uint64_t;

(**
 * Create a string attribute.
 *)
PROCEDURE LLVMCreateStringAttribute(C : ContextRef;
                                    K : const_char_star;
                                    KLength : unsigned;
                                    V : const_char_star;
                                    VLength : unsigned) : AttributeRef;

(**
 * Get the string attribute's kind.
 *)
PROCEDURE LLVMGetStringAttributeKind(A : AttributeRef; VAR Length : unsigned) : const_char_star;

(**
 * Get the string attribute's value.
 *)
PROCEDURE LLVMGetStringAttributeValue(A : AttributeRef; VAR Length : unsigned) : const_char_star;

(**
 * Check for the different types of attributes.
 *)
PROCEDURE LLVMIsEnumAttribute(A : AttributeRef) : Bool;
PROCEDURE LLVMIsStringAttribute(A : AttributeRef) : Bool;

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreModule Modules
 *
 * Modules represent the top-level structure in a LLVM program. An LLVM
 * module is effectively a translation unit or a collection of
 * translation units merged together.
 *
 * @{
 *)

(**
 * Create a new, empty module in the global context.
 *
 * This is equivalent to calling LLVMModuleCreateWithNameInContext with
 * LLVMGetGlobalContext() as the context parameter.
 *
 * Every invocation should be paired with LLVMDisposeModule() or memory
 * will be leaked.
 *)
PROCEDURE LLVMModuleCreateWithName(ModuleID: const_char_star): ModuleRef;
  (* PRE: ModuleID # NIL *) 

(**
 * Create a new, empty module in a specific context.
 *
 * Every invocation should be paired with LLVMDisposeModule() or memory
 * will be leaked.
 *)
PROCEDURE LLVMModuleCreateWithNameInContext(
    ModuleID: const_char_star; C: ContextRef): ModuleRef;
  (* PRE: ModuleID # NIL *) 

(**
 * Return an exact copy of the specified module.
 *)
PROCEDURE LLVMCloneModule(M : ModuleRef) : ModuleRef;

(**
 * Destroy a module instance.
 *
 * This must be called for every created module or memory will be
 * leaked.
 *)
PROCEDURE LLVMDisposeModule(M: ModuleRef);

(**
 * Obtain the module identifer for a module.
 *
 * @param M Module to obtain identifier of
 * @param Len Out parameter which holds the length of the returned string.
 * @return The identifier of M.
 * @see Module::getModuleIdentifier()
 *)
PROCEDURE LLVMGetModuleIdentifier(M: ModuleRef; VAR Len : unsigned): const_char_star;

(**
 * Set the identifier of a module to a string Ident with length Len.
 *
 * @param M The module to set identifier
 * @param Ident The string to set M's identifier to
 * @param Len Length of Ident
 * @see Module::setModuleIdentifier()
 *)
PROCEDURE LLVMSetModuleIdentifier(M: ModuleRef; ID: const_char_star; Len : unsigned);

(**
 * Obtain the module's original source file name.
 *
 * @param M Module to obtain the name of
 * @param Len Out parameter which holds the length of the returned string
 * @return The original source file name of M
 * @see Module::getSourceFileName()
 *)
PROCEDURE LLVMGetSourceFileName(M : ModuleRef; VAR Len : unsigned) : const_char_star;

(**
 * Set the original source file name of a module to a string Name with length
 * Len.
 *
 * @param M The module to set the source file name of
 * @param Name The string to set M's source file name to
 * @param Len Length of Name
 * @see Module::setSourceFileName()
 *)
PROCEDURE LLVMSetSourceFileName(M : ModuleRef; Name : const_char_star; Len : unsigned);

(**
 * Obtain the data layout for a module.
 *
 * @see Module::getDataLayout()
 *)
PROCEDURE LLVMGetDataLayoutStr(M: ModuleRef): const_char_star;

(**
 * Set the data layout for a module.
 *
 * @see Module::setDataLayout()
 *)
PROCEDURE LLVMSetDataLayout(M: ModuleRef; DataLayoutStr: const_char_star);

(**
 * Obtain the target triple for a module.
 *
 * @see Module::getTargetTriple()
 *)
PROCEDURE LLVMGetTarget(M: ModuleRef): const_char_star;

(**
 * Set the target triple for a module.
 *
 * @see Module::setTargetTriple()
 *)
PROCEDURE LLVMSetTarget(M: ModuleRef; Triple: const_char_star);

(**
 * Returns the module flags as an array of flag-key-value triples.  The caller
 * is responsible for freeing this array by calling
 * \c LLVMDisposeModuleFlagsMetadata.
 *
 * @see Module::getModuleFlagsMetadata()
 *)
 
 (* FIXME - check ModuleFlagEntryRef whether correct type 
 may need to by untraced ref ModuleflagEntryRef *)
PROCEDURE LLVMCopyModuleFlagsMetadata(M : ModuleRef; VAR Len : unsigned) : ModuleFlagEntryRef;

(**
 * Destroys module flags metadata entries.
 *)
PROCEDURE LLVMDisposeModuleFlagsMetadata(Entries : ModuleFlagEntryRef);

(**
 * Returns the flag behavior for a module flag entry at a specific index.
 *
 * @see Module::ModuleFlagEntry::Behavior
 *)

PROCEDURE LLVMModuleFlagEntriesGetFlagBehavior(
                                     Entries : ModuleFlagEntryRef;
                                     Index : unsigned) : ModuleFlagBehavior;

(**
 * Returns the key for a module flag entry at a specific index.
 *
 * @see Module::ModuleFlagEntry::Key
 *)
PROCEDURE LLVMModuleFlagEntriesGetKey(Entries : ModuleFlagEntryRef;
                                        Index : unsigned; VAR Len : unsigned) : char_star;

(**
 * Returns the metadata for a module flag entry at a specific index.
 *
 * @see Module::ModuleFlagEntry::Val
 *)
PROCEDURE LLVMModuleFlagEntriesGetMetadata(Entries : ModuleFlagEntryRef;
                                                 Index : unsigned) : MetadataRef;

(**
 * Add a module-level flag to the module-level flags metadata if it doesn't
 * already exist.
 *
 * @see Module::getModuleFlag()
 *)
PROCEDURE LLVMGetModuleFlag (M: ModuleRef;
 Key: char_star;
 KeyLen: uint32_t;
): MetadataRef;

(**
 * Add a module-level flag to the module-level flags metadata if it doesn't
 * already exist.
 *
 * @see Module::addModuleFlag()
 *)
PROCEDURE LLVMAddModuleFlag (M: ModuleRef;
Behavior: ModuleFlagBehavior;
 Key: char_star;
KeyLen: uint32_t;
Val: MetadataRef;
);

(**
 * Dump a representation of a module to stderr.
 *
 * @see Module::dump()
 *)
PROCEDURE LLVMDumpModule(M: ModuleRef);

(**
 * Print a representation of a module to a file. The ErrorMessage needs to be
 * disposed with LLVMDisposeMessage. Returns 0 on success, 1 otherwise.
 *
 * @see Module::print()
 *)
PROCEDURE LLVMPrintModuleToFile(M: ModuleRef; Filename: const_char_star;
    ErrorMessage: char_star_star): Bool;

(**
 * Return a string representation of the module. Use
 * LLVMDisposeMessage to free the string.
 *
 * @see Module::print()
 *)
PROCEDURE LLVMPrintModuleToString(M : ModuleRef) : char_star;

(**
 * Get inline assembly for a module.
 *
 * @see Module::getModuleInlineAsm()
 *)
PROCEDURE LLVMGetModuleInlineAsm(M : ModuleRef; VAR Len : unsigned) : const_char_star;

(**
 * Set inline assembly for a module.
 *
 * @see Module::setModuleInlineAsm()
 *)
PROCEDURE LLVMSetModuleInlineAsm2(M: ModuleRef; Asm: const_char_star; Len : unsigned);

(**
 * Append inline assembly to a module.
 *
 * @see Module::appendModuleInlineAsm()
 *)
PROCEDURE LLVMAppendModuleInlineAsm(M : ModuleRef; Asm : const_char_star; Len : unsigned);


(**
 * Create the specified uniqued inline asm string.
 *
 * @see InlineAsm::get()
 *)
PROCEDURE LLVMGetInlineAsm(Ty : TypeRef;
                           AsmString : char_star;
                           AsmStringSize : unsigned;
                           Constraints : char_star;
                           ConstraintsSize : unsigned;
                           HasSideEffects : Bool; 
                           IsAlignStack : Bool;
                           Dialect : InlineAsmDialect) : ValueRef;
                              
(**
 * Obtain the context to which this module is associated.
 *
 * @see Module::getContext()
 *)
PROCEDURE LLVMGetModuleContext(M: ModuleRef): ContextRef;

(**
 * Obtain a Type from a module by its registered name.
 *)
PROCEDURE LLVMGetTypeByName(M: ModuleRef; Name: const_char_star): TypeRef;

(**
 * Obtain an iterator to the first NamedMDNode in a Module.
 *
 * @see llvm::Module::named_metadata_begin()
 *)
PROCEDURE LLVMGetFirstNamedMetadata (M: ModuleRef;
): NamedMDNodeRef;

(**
 * Obtain an iterator to the last NamedMDNode in a Module.
 *
 * @see llvm::Module::named_metadata_end()
 *)
PROCEDURE LLVMGetLastNamedMetadata (M: ModuleRef;
): NamedMDNodeRef;

(**
 * Advance a NamedMDNode iterator to the next NamedMDNode.
 *
 * Returns NULL if the iterator was already at the end and there are no more
 * named metadata nodes.
 *)
PROCEDURE LLVMGetNextNamedMetadata (NamedMDNode: NamedMDNodeRef;
): NamedMDNodeRef;

(**
 * Decrement a NamedMDNode iterator to the previous NamedMDNode.
 *
 * Returns NULL if the iterator was already at the beginning and there are
 * no previous named metadata nodes.
 *)
PROCEDURE LLVMGetPreviousNamedMetadata (NamedMDNode: NamedMDNodeRef;
): NamedMDNodeRef;

(**
 * Retrieve a NamedMDNode with the given name, returning NULL if no such
 * node exists.
 *
 * @see llvm::Module::getNamedMetadata()
 *)
PROCEDURE LLVMGetNamedMetadata (M: ModuleRef; 
                                Name: char_star;
                                NameLen: uint32_t;
): NamedMDNodeRef;

(**
 * Retrieve a NamedMDNode with the given name, creating a new node if no such
 * node exists.
 *
 * @see llvm::Module::getOrInsertNamedMetadata()
 *)
PROCEDURE LLVMGetOrInsertNamedMetadata (M: ModuleRef;
                                    Name: char_star;
                                    NameLen: uint32_t;
): NamedMDNodeRef;                                                
                  
(**
 * Retrieve the name of a NamedMDNode.
 *
 * @see llvm::NamedMDNode::getName()
 *)
PROCEDURE LLVMGetNamedMetadataName (NamedMD: NamedMDNodeRef;
VAR NameLen: uint32_t;
): char_star;

(**
 * Obtain the number of operands for named metadata in a module.
 *
 * @see llvm::Module::getNamedMetadata()
 *)
PROCEDURE LLVMGetNamedMetadataNumOperands(M: ModuleRef; 
                                          name: const_char_star): unsigned;

(**
 * Obtain the named metadata operands for a module.
 *
 * The passed LLVMValueRef pointer should refer to an array of
 * LLVMValueRef at least LLVMGetNamedMetadataNumOperands long. This
 * array will be populated with the LLVMValueRef instances. Each
 * instance corresponds to a llvm::MDNode.
 *
 * @see llvm::Module::getNamedMetadata()
 * @see llvm::MDNode::getOperand()
 *)
PROCEDURE LLVMGetNamedMetadataOperands(
    M: ModuleRef; name: const_char_star; Dest: UNTRACED REF ValueRef);

(**
 * Add an operand to named metadata.
 *
 * @see llvm::Module::getNamedMetadata()
 * @see llvm::MDNode::addOperand()
 *)
PROCEDURE LLVMAddNamedMetadataOperand(M: ModuleRef; name: const_char_star;
                                  Val: ValueRef);


(**
 * Return the directory of the debug location for this value, which must be
 * an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.
 *
 * @see llvm::Instruction::getDebugLoc()
 * @see llvm::GlobalVariable::getDebugInfo()
 * @see llvm::Function::getSubprogram()
 *)
PROCEDURE LLVMGetDebugLocDirectory(Val : ValueRef; VAR Length : unsigned) : char_star;

(**
 * Return the filename of the debug location for this value, which must be
 * an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.
 *
 * @see llvm::Instruction::getDebugLoc()
 * @see llvm::GlobalVariable::getDebugInfo()
 * @see llvm::Function::getSubprogram()
 *)
PROCEDURE LLVMGetDebugLocFilename(Val : ValueRef; VAR Length : unsigned) : char_star;


(**
 * Return the line number of the debug location for this value, which must be
 * an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.
 *
 * @see llvm::Instruction::getDebugLoc()
 * @see llvm::GlobalVariable::getDebugInfo()
 * @see llvm::Function::getSubprogram()
 *)
PROCEDURE LLVMGetDebugLocLine(Val : ValueRef) : unsigned;


(**
 * Return the column number of the debug location for this value, which must be
 * an llvm::Instruction.
 *
 * @see llvm::Instruction::getDebugLoc()
 *)
PROCEDURE LLVMGetDebugLocColumn(Val : ValueRef) : unsigned;


(**
 * Add a function to a module under a specified name.
 *
 * @see llvm::Function::Create()
 *)
PROCEDURE LLVMAddFunction(M: ModuleRef; Name: const_char_star;
                      FunctionTy: TypeRef): ValueRef;

<*EXTERNAL LLVMAddFunction*>
PROCEDURE LLVMAddFunctionCHAR(M: ModuleRef; Name: UNTRACED REF CHAR;
                      FunctionTy: TypeRef): ValueRef;

(**
 * Obtain a Function value from a Module by its name.
 *
 * The returned value corresponds to a llvm::Function value.
 *
 * @see llvm::Module::getFunction()
 *)
PROCEDURE LLVMGetNamedFunction(M: ModuleRef; Name: const_char_star): ValueRef;

(**
 * Obtain an iterator to the first Function in a Module.
 *
 * @see llvm::Module::begin()
 *)
PROCEDURE LLVMGetFirstFunction(M: ModuleRef): ValueRef;

(**
 * Obtain an iterator to the last Function in a Module.
 *
 * @see llvm::Module::end()
 *)
PROCEDURE LLVMGetLastFunction(M: ModuleRef): ValueRef;

(**
 * Advance a Function iterator to the next Function.
 *
 * Returns NULL if the iterator was already at the end and there are no more
 * functions.
 *)
PROCEDURE LLVMGetNextFunction(Fn: ValueRef): ValueRef;

(**
 * Decrement a Function iterator to the previous Function.
 *
 * Returns NULL if the iterator was already at the beginning and there are
 * no previous functions.
 *)
PROCEDURE LLVMGetPreviousFunction(Fn: ValueRef): ValueRef;

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreType Types
 *
 * Types represent the type of a value.
 *
 * Types are associated with a context instance. The context internally
 * deduplicates types so there is only 1 instance of a specific type
 * alive at a time. In other words, a unique type is shared among all
 * consumers within a context.
 *
 * A Type in the C API corresponds to llvm::Type.
 *
 * Types have the following hierarchy:
 *
 *   types:
 *     integer type
 *     real type
 *     function type
 *     sequence types:
 *       array type
 *       pointer type
 *       vector type
 *     void type
 *     label type
 *     opaque type
 *
 * @{
 *)

(**
 * Obtain the enumerated type of a Type instance.
 *
 * @see llvm::Type:getTypeID()
 *)
PROCEDURE LLVMGetTypeKind(Ty: TypeRef): TypeKind;

(**
 * Whether the type has a known size.
 *
 * Things that don't have a size are abstract types, labels, and void.a
 *
 * @see llvm::Type::isSized()
 *)
PROCEDURE LLVMTypeIsSized(Ty: TypeRef): Bool;

(**
 * Obtain the context to which this type instance is associated.
 *
 * @see llvm::Type::getContext()
 *)
PROCEDURE LLVMGetTypeContext(Ty: TypeRef): ContextRef;

(**
 * Dump a representation of a type to stderr.
 *
 * @see llvm::Type::dump()
 *)
PROCEDURE LLVMDumpType(Val : TypeRef);

(**
 * Return a string representation of the type. Use
 * LLVMDisposeMessage to free the string.
 *
 * @see llvm::Type::print()
 *)
PROCEDURE LLVMPrintTypeToString(Val : TypeRef) : char_star;

(**
 * @defgroup LLVMCCoreTypeInt Integer Types
 *
 * Functions in this section operate on integer types.
 *
 * @{
 *)

(**
 * Obtain an integer type from a context with specified bit width.
 *)
PROCEDURE LLVMInt1TypeInContext(C: ContextRef): TypeRef;
PROCEDURE LLVMInt8TypeInContext(C: ContextRef): TypeRef;
PROCEDURE LLVMInt16TypeInContext(C: ContextRef): TypeRef;
PROCEDURE LLVMInt32TypeInContext(C: ContextRef): TypeRef;
PROCEDURE LLVMInt64TypeInContext(C: ContextRef): TypeRef;
PROCEDURE LLVMInt128TypeInContext(C: ContextRef): TypeRef;
PROCEDURE LLVMIntTypeInContext(C: ContextRef; NumBits: unsigned): TypeRef;

(**
 * Obtain an integer type from the global context with a specified bit
 * width.
 *)
PROCEDURE LLVMInt1Type(): TypeRef;
PROCEDURE LLVMInt8Type(): TypeRef;
PROCEDURE LLVMInt16Type(): TypeRef;
PROCEDURE LLVMInt32Type(): TypeRef;
PROCEDURE LLVMInt64Type(): TypeRef;
PROCEDURE LLVMInt128Type(): TypeRef;
PROCEDURE LLVMIntType(NumBits: unsigned): TypeRef;
PROCEDURE LLVMGetIntTypeWidth(IntegerTy: TypeRef): unsigned;

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreTypeFloat Floating Point Types
 *
 * @{
 *)

(**
 * Obtain a 16-bit floating point type from a context.
 *)
PROCEDURE LLVMHalfTypeInContext(C: ContextRef): TypeRef;

(**
 * Obtain a 32-bit floating point type from a context.
 *)
PROCEDURE LLVMFloatTypeInContext(C: ContextRef): TypeRef;

(**
 * Obtain a 64-bit floating point type from a context.
 *)
PROCEDURE LLVMDoubleTypeInContext(C: ContextRef): TypeRef;

(**
 * Obtain a 80-bit floating point type (X87) from a context.
 *)
PROCEDURE LLVMX86FP80TypeInContext(C: ContextRef): TypeRef;

(**
 * Obtain a 128-bit floating point type (112-bit mantissa) from a
 * context.
 *)
PROCEDURE LLVMFP128TypeInContext(C: ContextRef): TypeRef;

(**
 * Obtain a 128-bit floating point type (two 64-bits) from a context.
 *)
PROCEDURE LLVMPPCFP128TypeInContext(C: ContextRef): TypeRef;

(**
 * Obtain a floating point type from the global context.
 *
 * These map to the functions in this group of the same name.
 *)
PROCEDURE LLVMHalfType(): TypeRef;
PROCEDURE LLVMFloatType(): TypeRef;
PROCEDURE LLVMDoubleType(): TypeRef;
PROCEDURE LLVMX86FP80Type(): TypeRef;
PROCEDURE LLVMFP128Type(): TypeRef;
PROCEDURE LLVMPPCFP128Type(): TypeRef;

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreTypeFunction Function Types
 *
 * @{
 *)

(**
 * Obtain a function type consisting of a specified signature.
 *
 * The function is defined as a tuple of a return Type, a list of
 * parameter types, and whether the function is variadic.
 *)
PROCEDURE LLVMFunctionType(ReturnType: TypeRef;
                       ParamTypes: UNTRACED REF TypeRef; 
                       (* ^Actually, an array of TypeRef. *) 
                       ParamCount: unsigned;
                       IsVarArg: Bool := False): TypeRef;
(* PRE: ReturnType # NIL *)
(* PRE: ParamCount = 0 OR ParamCount elements of ParamTypes # NIL *) 
(* PRE: All existing elements of ParamTypes are not llvm void. *) 

<*EXTERNAL LLVMFunctionType*>
PROCEDURE LLVMFunctionTypeSafe(ReturnType: TypeRef;
                       READONLY ParamTypes: TypeRef; 
                       (* ^Actually, an array of TypeRef. *) 
                       (* READONLY, so it is passed by reference. *) 
                       ParamCount: unsigned;
                       IsVarArg: Bool := False): TypeRef;
(* "Safe" means only that the unsafety is moved outside of Modula-3
   code, where it can be inconvenient to deal with, to the unchecked matchup
   between this binding and the external C code it links to.
*) 

(**
 * Returns whether a function type is variadic.
 *)
PROCEDURE LLVMIsFunctionVarArg(FunctionTy: TypeRef): Bool;

(**
 * Obtain the Type this function Type returns.
 *)
PROCEDURE LLVMGetReturnType(FunctionTy: TypeRef): TypeRef;

(**
 * Obtain the number of parameters this function accepts.
 *)
PROCEDURE LLVMCountParamTypes(FunctionTy: TypeRef): unsigned;

(**
 * Obtain the types of a function's parameters.
 *
 * The Dest parameter should point to a pre-allocated array of
 * LLVMTypeRef at least LLVMCountParamTypes() large. On return, the
 * first LLVMCountParamTypes() entries in the array will be populated
 * with LLVMTypeRef instances.
 *
 * @param FunctionTy The function type to operate on.
 * @param Dest Memory address of an array to be filled with result.
 *)
PROCEDURE LLVMGetParamTypes(FunctionTy: TypeRef; Dest: UNTRACED REF TypeRef);

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreTypeStruct Structure Types
 *
 * These functions relate to LLVMTypeRef instances.
 *
 * @see llvm::StructType
 *
 * @{
 *)

(**
 * Create a new structure type in a context.
 *
 * A structure is specified by a list of inner elements/types and
 * whether these can be packed together.
 *
 * @see llvm::StructType::create()
 *)
PROCEDURE LLVMStructTypeInContext(C: ContextRef; ElementTypes: UNTRACED REF TypeRef;
                              ElementCount: unsigned; Packed: Bool): TypeRef;

(**
 * Create a new structure type in the global context.
 *
 * @see llvm::StructType::create()
 *)
PROCEDURE LLVMStructType(ElementTypes: UNTRACED REF TypeRef; ElementCount: unsigned;
                     Packed: Bool): TypeRef;

(**
 * Create an empty structure in a context having a specified name.
 *
 * @see llvm::StructType::create()
 *)
PROCEDURE LLVMStructCreateNamed(C: ContextRef;
                            Name: const_char_star := NIL): TypeRef;

(**
 * Obtain the name of a structure.
 *
 * @see llvm::StructType::getName()
 *)
PROCEDURE LLVMGetStructName(Ty: TypeRef): const_char_star;

(**
 * Set the contents of a structure type.
 *
 * @see llvm::StructType::setBody()
 *)
PROCEDURE LLVMStructSetBody(StructTy: TypeRef; 
                        ElementTypes: UNTRACED REF TypeRef;
                        (* ^Actually, an array of TypeRef. *) 
                        ElementCount: unsigned; Packed: Bool := False);

<*EXTERNAL LLVMStructSetBody*>
PROCEDURE LLVMStructSetBodySafe(StructTy: TypeRef; 
                        READONLY ElementTypes: TypeRef;
                        (* ^Actually, an array of TypeRef. *) 
                        (* READONLY, so it is passed by reference. *) 
                        ElementCount: unsigned; Packed: Bool := False);
(* "Safe" means only that the unsafety is moved outside of Modula-3
   code, where it can be inconvenient to deal with, to the unchecked matchup
   between this binding and the external C code it links to.
*) 

(**
 * Get the number of elements defined inside the structure.
 *
 * @see llvm::StructType::getNumElements()
 *)
PROCEDURE LLVMCountStructElementTypes(StructTy: TypeRef): unsigned;

(**
 * Get the elements within a structure.
 *
 * The function is passed the address of a pre-allocated array of
 * LLVMTypeRef at least LLVMCountStructElementTypes() long. After
 * invocation, this array will be populated with the structure's
 * elements. The objects in the destination array will have a lifetime
 * of the structure type itself, which is the lifetime of the context it
 * is contained in.
 *)
PROCEDURE LLVMGetStructElementTypes(StructTy: TypeRef; Dest: UNTRACED REF TypeRef);

(**
 * Get the type of the element at a given index in the structure.
 *
 * @see llvm::StructType::getTypeAtIndex()
 *)
PROCEDURE LLVMStructGetTypeAtIndex(StructTy : TypeRef; i : unsigned) : TypeRef;

(**
 * Determine whether a structure is packed.
 *
 * @see llvm::StructType::isPacked()
 *)
PROCEDURE LLVMIsPackedStruct(StructTy: TypeRef): Bool;

(**
 * Determine whether a structure is opaque.
 *
 * @see llvm::StructType::isOpaque()
 *)
PROCEDURE LLVMIsOpaqueStruct(StructTy: TypeRef): Bool;

(**
 * Determine whether a structure is literal.
 *
 * @see llvm::StructType::isLiteral()
 *)
PROCEDURE LLVMIsLiteralStruct(StructTy : TypeRef) : Bool;

(**
 * @}
 *)


(**
 * @defgroup LLVMCCoreTypeSequential Sequential Types
 *
 * Sequential types represents "arrays" of types. This is a super class
 * for array, vector, and pointer types.
 *
 * @{
 *)

(**
 * Obtain the type of elements within a sequential type.
 *
 * This works on array, vector, and pointer types.
 *
 * @see llvm::SequentialType::getElementType()
 *)
PROCEDURE LLVMGetElementType(Ty: TypeRef): TypeRef;

(**
 * Returns type's subtypes
 *
 * @see llvm::Type::subtypes()
 *)
PROCEDURE LLVMGetSubtypes(Tp : TypeRef; Arr : UNTRACED REF TypeRef);

(**
 *  Return the number of types in the derived type.
 *
 * @see llvm::Type::getNumContainedTypes()
 *)
PROCEDURE LLVMGetNumContainedTypes(Tp : TypeRef) : unsigned;

(**
 * Create a fixed size array type that refers to a specific type.
 *
 * The created type will exist in the context that its element type
 * exists in.
 *
 * @see llvm::ArrayType::get()
 *)
PROCEDURE LLVMArrayType(ElementType: TypeRef; ElementCount: unsigned): TypeRef;

(**
 * Obtain the length of an array type.
 *
 * This only works on types that represent arrays.
 *
 * @see llvm::ArrayType::getNumElements()
 *)
PROCEDURE LLVMGetArrayLength(ArrayTy: TypeRef): unsigned;

(**
 * Create a pointer type that points to a defined type.
 *
 * The created type will exist in the context that its pointee type
 * exists in.
 *
 * @see llvm::PointerType::get()
 * PRE: ElementType is not void, metadata, or label. 
 *)
PROCEDURE LLVMPointerType
  (ElementType: TypeRef; AddressSpace: unsigned := 0): TypeRef;

(**
 * Obtain the address space of a pointer type.
 *
 * This only works on types that represent pointers.
 *
 * @see llvm::PointerType::getAddressSpace()
 *)
PROCEDURE LLVMGetPointerAddressSpace(PointerTy: TypeRef): unsigned;

(**
 * Create a vector type that contains a defined type and has a specific
 * number of elements.
 *
 * The created type will exist in the context thats its element type
 * exists in.
 *
 * @see llvm::VectorType::get()
 *)
PROCEDURE LLVMVectorType(ElementType: TypeRef; ElementCount: unsigned): TypeRef;

(**
 * Obtain the number of elements in a vector type.
 *
 * This only works on types that represent vectors.
 *
 * @see llvm::VectorType::getNumElements()
 *)
PROCEDURE LLVMGetVectorSize(VectorTy: TypeRef): unsigned;

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreTypeOther Other Types
 *
 * @{
 *)

(**
 * Create a void type in a context.
 *)
PROCEDURE LLVMVoidTypeInContext(C: ContextRef): TypeRef;

(**
 * Create a label type in a context.
 *)
PROCEDURE LLVMLabelTypeInContext(C: ContextRef): TypeRef;

(**
 * Create a X86 MMX type in a context.
 *)
PROCEDURE LLVMX86MMXTypeInContext(C: ContextRef): TypeRef;

(**
 * Create a token type in a context.
 *)
PROCEDURE LLVMTokenTypeInContext(C : ContextRef) : TypeRef;

(**
 * Create a metadata type in a context.
 *)
PROCEDURE LLVMMetadataTypeInContext(C : ContextRef) : TypeRef;


(**
 * These are similar to the above functions except they operate on the
 * global context.
 *)
PROCEDURE LLVMVoidType(): TypeRef;
PROCEDURE LLVMLabelType(): TypeRef;
PROCEDURE LLVMX86MMXType(): TypeRef;

(**
 * @}
 *)

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreValueGeneral General APIs
 *
 * Functions in this section work on all LLVMValueRef instances,
 * regardless of their sub-type. They correspond to functions available
 * on llvm::Value.
 *
 * @{
 *)

(**
 * Obtain the type of a value.
 *
 * @see llvm::Value::getType()
 *)
PROCEDURE LLVMTypeOf(Val: ValueRef): TypeRef;

(**
 * Obtain the enumerated type of a Value instance.
 *
 * @see llvm::Value::getValueID()
 *)
PROCEDURE LLVMGetValueKind(Val : ValueRef) : ValueKind;

(**
 * Obtain the string name of a value.
 *
 * @see llvm::Value::getName()
 *)
PROCEDURE LLVMGetValueName2(Val: ValueRef; VAR Length : unsigned): const_char_star;

(**
 * Set the string name of a value.
 *
 * @see llvm::Value::setName()
 *)
PROCEDURE LLVMSetValueName2(Val: ValueRef; Name: const_char_star; NameLen : unsigned);

<*EXTERNAL LLVMSetValueName*>
PROCEDURE LLVMSetValueNameCHAR(Val: ValueRef; Name: UNTRACED REF CHAR);

(**
 * Dump a representation of a value to stderr.
 *
 * @see llvm::Value::dump()
 *)
PROCEDURE LLVMDumpValue(Val: ValueRef);

(**
 * Return a string representation of the value. Use
 * LLVMDisposeMessage to free the string.
 *
 * @see llvm::Value::print()
 *)
PROCEDURE LLVMPrintValueToString(Val : ValueRef) : char_star;

(**
 * Replace all uses of a value with another one.
 *
 * @see llvm::Value::replaceAllUsesWith()
 *)
PROCEDURE LLVMReplaceAllUsesWith(OldVal: ValueRef; NewVal: ValueRef);

(**
 * Determine whether the specified constant instance is constant.
 *)
PROCEDURE LLVMIsConstant(Val: ValueRef): Bool;

(**
 * Determine whether a value instance is undefined.
 *)
PROCEDURE LLVMIsUndef(Val: ValueRef): Bool;

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreValueUses Usage
 *
 * This module defines functions that allow you to inspect the uses of a
 * LLVMValueRef.
 *
 * It is possible to obtain a LLVMUseRef for any LLVMValueRef instance.
 * Each LLVMUseRef (which corresponds to a llvm::Use instance) holds a
 * llvm::User and llvm::Value.
 *
 * @{
 *)

(**
 * Obtain the first use of a value.
 *
 * Uses are obtained in an iterator fashion. First, call this function
 * to obtain a reference to the first use. Then, call LLVMGetNextUse()
 * on that instance and all subsequently obtained instances until
 * LLVMGetNextUse() returns NULL.
 *
 * @see llvm::Value::use_begin()
 *)
PROCEDURE LLVMGetFirstUse(Val: ValueRef): UseRef;

(**
 * Obtain the next use of a value.
 *
 * This effectively advances the iterator. It returns NULL if you are on
 * the final use and no more are available.
 *)
PROCEDURE LLVMGetNextUse(U: UseRef): UseRef;

(**
 * Obtain the user value for a use.
 *
 * The returned value corresponds to a llvm::User type.
 *
 * @see llvm::Use::getUser()
 *)
PROCEDURE LLVMGetUser(U: UseRef): ValueRef;

(**
 * Obtain the value this use corresponds to.
 *
 * @see llvm::Use::get().
 *)
PROCEDURE LLVMGetUsedValue(U: UseRef): ValueRef;

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreValueUser User value
 *
 * Function in this group pertain to LLVMValueRef instances that descent
 * from llvm::User. This includes constants, instructions, and
 * operators.
 *
 * @{
 *)

(**
 * Obtain an operand at a specific index in a llvm::User value.
 *
 * @see llvm::User::getOperand()
 *)
PROCEDURE LLVMGetOperand(Val: ValueRef; Index: unsigned): ValueRef;

(**
 * Obtain the use of an operand at a specific index in a llvm::User value.
 *
 * @see llvm::User::getOperandUse()
 *)
PROCEDURE LLVMGetOperandUse(Val : ValueRef; Index : unsigned) : UseRef;


(**
 * Set an operand at a specific index in a llvm::User value.
 *
 * @see llvm::User::setOperand()
 *)
PROCEDURE LLVMSetOperand(User: ValueRef; Index: unsigned; Val: ValueRef);

(**
 * Obtain the number of operands in a llvm::User value.
 *
 * @see llvm::User::getNumOperands()
 *)
PROCEDURE LLVMGetNumOperands(Val: ValueRef): int;

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreValueConstant Constants
 *
 * This section contains APIs for interacting with LLVMValueRef that
 * correspond to llvm::Constant instances.
 *
 * These functions will work for any LLVMValueRef in the llvm::Constant
 * class hierarchy.
 *
 * @{
 *)

(**
 * Obtain a constant value referring to the null instance of a type.
 *
 * @see llvm::Constant::getNullValue()
 *)
PROCEDURE LLVMConstNull(Ty: TypeRef): ValueRef; (* all zeroes *)

(**
 * Obtain a constant value referring to the instance of a type
 * consisting of all ones.
 *
 * This is only valid for integer types.
 *
 * @see llvm::Constant::getAllOnesValue()
 *)
PROCEDURE LLVMConstAllOnes(Ty: TypeRef): ValueRef;

(**
 * Obtain a constant value referring to an undefined value of a type.
 *
 * @see llvm::UndefValue::get()
 *)
PROCEDURE LLVMGetUndef(Ty: TypeRef): ValueRef;

(**
 * Determine whether a value instance is null.
 *
 * @see llvm::Constant::isNullValue()
 *)
PROCEDURE LLVMIsNull(Val: ValueRef): Bool;

(**
 * Obtain a constant that is a constant pointer pointing to NULL for a
 * specified type.
 *)
PROCEDURE LLVMConstPointerNull(Ty: TypeRef): ValueRef;

(**
 * @defgroup LLVMCCoreValueConstantScalar Scalar constants
 *
 * Functions in this group model LLVMValueRef instances that correspond
 * to constants referring to scalar types.
 *
 * For integer types, the LLVMTypeRef parameter should correspond to a
 * llvm::IntegerType instance and the returned LLVMValueRef will
 * correspond to a llvm::ConstantInt.
 *
 * For floating point types, the LLVMTypeRef returned corresponds to a
 * llvm::ConstantFP.
 *
 * @{
 *)

(**
 * Obtain a constant value for an integer type.
 *
 * The returned value corresponds to a llvm::ConstantInt.
 *
 * @see llvm::ConstantInt::get()
 *
 * @param IntTy Integer type to obtain value of.
 * @param N The value the returned instance should refer to.
 * @param SignExtend Whether to sign extend the produced value.
 *)
PROCEDURE LLVMConstInt(IntTy: TypeRef; N: unsigned_long_long;
                   SignExtend: Bool): ValueRef;

(**
 * Obtain a constant value for an integer of arbitrary precision.
 *
 * @see llvm::ConstantInt::get()
 *)
PROCEDURE LLVMConstIntOfArbitraryPrecision(IntTy: TypeRef;
                                       NumWords: unsigned;
                                       Words: UNTRACED REF uint64_t): ValueRef;

(**
 * Obtain a constant value for an integer parsed from a string.
 *
 * A similar API, LLVMConstIntOfStringAndSize is also available. If the
 * string's length is available, it is preferred to call that function
 * instead.
 *
 * @see llvm::ConstantInt::get()
 *)
PROCEDURE LLVMConstIntOfString(IntTy: TypeRef; Text: const_char_star;
                           Radix: uint8_t): ValueRef;

(**
 * Obtain a constant value for an integer parsed from a string with
 * specified length.
 *
 * @see llvm::ConstantInt::get()
 *)
PROCEDURE LLVMConstIntOfStringAndSize(IntTy: TypeRef; Text: const_char_star;
                                  SLen: unsigned; Radix: uint8_t): ValueRef;

(**
 * Obtain a constant value referring to a double floating point value.
 *)
PROCEDURE LLVMConstReal(RealTy: TypeRef; N: double): ValueRef;

(**
 * Obtain a constant for a floating point value parsed from a string.
 *
 * A similar API, LLVMConstRealOfStringAndSize is also available. It
 * should be used if the input string's length is known.
 *)
PROCEDURE LLVMConstRealOfString(RealTy: TypeRef; Text: const_char_star): ValueRef;

(**
 * Obtain a constant for a floating point value parsed from a string.
 *)
PROCEDURE LLVMConstRealOfStringAndSize(RealTy: TypeRef; Text: const_char_star;
                                   SLen: unsigned): ValueRef;

(**
 * Obtain the zero extended value for an integer constant value.
 *
 * @see llvm::ConstantInt::getZExtValue()
 *)
PROCEDURE LLVMConstIntGetZExtValue(ConstantVal: ValueRef): unsigned_long_long;

(**
 * Obtain the sign extended value for an integer constant value.
 *
 * @see llvm::ConstantInt::getSExtValue()
 *)
PROCEDURE LLVMConstIntGetSExtValue(ConstantVal: ValueRef): long_long;

(**
 * Obtain the double value for an floating point constant value.
 * losesInfo indicates if some precision was lost in the conversion.
 *
 * @see llvm::ConstantFP::getDoubleValue
 *)
PROCEDURE LLVMConstRealGetDouble(ConstantVal : ValueRef; VAR losesInfo : Bool) : double;

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreValueConstantComposite Composite Constants
 *
 * Functions in this group operate on composite constants.
 *
 * @{
 *)

(**
 * Create a ConstantDataSequential and initialize it with a string.
 *
 * @see llvm::ConstantDataArray::getString()
 *)
PROCEDURE LLVMConstStringInContext(C: ContextRef; Str: const_char_star;
                               Length: unsigned;
                               DontNullTerminate: Bool): ValueRef;

(**
 * Create a ConstantDataSequential with string content in the global context.
 *
 * This is the same as LLVMConstStringInContext except it operates on the
 * global context.
 *
 * @see LLVMConstStringInContext()
 * @see llvm::ConstantDataArray::getString()
 *)
PROCEDURE LLVMConstString(Str: const_char_star; Length: unsigned;
                      DontNullTerminate: Bool): ValueRef;

(**
 * Returns true if the specified constant is an array of i8.
 *
 * @see ConstantDataSequential::getAsString()
 *)
PROCEDURE LLVMIsConstantString(c: ValueRef) : Bool;

(**
 * Get the given constant data sequential as a string.
 *
 * @see ConstantDataSequential::getAsString()
 *)
PROCEDURE LLVMGetAsString(c : ValueRef; VAR Length : unsigned) : const_char_star;


(**
 * Create an anonymous ConstantStruct with the specified values.
 *
 * @see llvm::ConstantStruct::getAnon()
 *)
PROCEDURE LLVMConstStructInContext(C: ContextRef;
                               ConstantVals: UNTRACED REF ValueRef;
                               Count: unsigned; Packed: Bool): ValueRef;

(**
 * Create a ConstantStruct in the global Context.
 *
 * This is the same as LLVMConstStructInContext except it operates on the
 * global Context.
 *
 * @see LLVMConstStructInContext()
 *)
PROCEDURE LLVMConstStruct(ConstantVals: UNTRACED REF ValueRef;
                      Count: unsigned; Packed: Bool): ValueRef;

(**
 * Create a ConstantArray from values.
 *
 * @see llvm::ConstantArray::get()
 *)
PROCEDURE LLVMConstArray(ElementTy: TypeRef;
                     ConstantVals: UNTRACED REF ValueRef;
                     Length: unsigned): ValueRef;

(**
 * Create a non-anonymous ConstantStruct from values.
 *
 * @see llvm::ConstantStruct::get()
 *)
PROCEDURE LLVMConstNamedStruct(StructTy: TypeRef;
                           ConstantVals: UNTRACED REF ValueRef;
                           Count: unsigned): ValueRef;

(**
 * Get an element at specified index as a constant.
 *
 * @see ConstantDataSequential::getElementAsConstant()
 *)
PROCEDURE LLVMGetElementAsConstant(C : ValueRef; idx : unsigned) : ValueRef;

(**
 * Create a ConstantVector from values.
 *
 * @see llvm::ConstantVector::get()
 *)
PROCEDURE LLVMConstVector(ScalarConstantVals: UNTRACED REF ValueRef;
                      Size: unsigned): ValueRef;

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreValueConstantExpressions Constant Expressions
 *
 * Functions in this group correspond to APIs on llvm::ConstantExpr.
 *
 * @see llvm::ConstantExpr.
 *
 * @{
 *)
PROCEDURE LLVMGetConstOpcode(ConstantVal: ValueRef): Opcode;
PROCEDURE LLVMAlignOf(Ty: TypeRef): ValueRef;
PROCEDURE LLVMSizeOf(Ty: TypeRef): ValueRef;
PROCEDURE LLVMConstNeg(ConstantVal: ValueRef): ValueRef;
PROCEDURE LLVMConstNSWNeg(ConstantVal: ValueRef): ValueRef;
PROCEDURE LLVMConstNUWNeg(ConstantVal: ValueRef): ValueRef;
PROCEDURE LLVMConstFNeg(ConstantVal: ValueRef): ValueRef;
PROCEDURE LLVMConstNot(ConstantVal: ValueRef): ValueRef;
PROCEDURE LLVMConstAdd(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstNSWAdd(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstNUWAdd(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstFAdd(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstSub(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstNSWSub(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstNUWSub(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstFSub(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstMul(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstNSWMul(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstNUWMul(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstFMul(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstUDiv(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstExactUDiv(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstSDiv(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstExactSDiv(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstFDiv(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstURem(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstSRem(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstFRem(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstAnd(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstOr(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstXor(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstICmp(Predicate: IntPredicate;
                    LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstFCmp(Predicate: RealPredicate;
                    LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstShl(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstLShr(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstAShr(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstGEP(ConstantVal: ValueRef;
                   ConstantIndices: UNTRACED REF ValueRef;
                   NumIndices: unsigned): ValueRef;
PROCEDURE LLVMConstGEP2(Ty : TypeRef; ConstantVal: ValueRef;
                   ConstantIndices: UNTRACED REF ValueRef;
                   NumIndices: unsigned): ValueRef;                   
PROCEDURE LLVMConstInBoundsGEP(ConstantVal: ValueRef;
                           ConstantIndices: UNTRACED REF ValueRef;
                           NumIndices: unsigned): ValueRef;
PROCEDURE LLVMConstInBoundsGEP2(Ty : TypeRef; ConstantVal: ValueRef;
                           ConstantIndices: UNTRACED REF ValueRef;
                           NumIndices: unsigned): ValueRef;
PROCEDURE LLVMConstTrunc(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE LLVMConstSExt(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE LLVMConstZExt(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE LLVMConstFPTrunc(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE LLVMConstFPExt(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE LLVMConstUIToFP(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE LLVMConstSIToFP(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE LLVMConstFPToUI(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE LLVMConstFPToSI(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE LLVMConstPtrToInt(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE LLVMConstIntToPtr(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE LLVMConstBitCast(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE LLVMConstZExtOrBitCast(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE LLVMConstSExtOrBitCast(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE LLVMConstTruncOrBitCast(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE LLVMConstPointerCast(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE LLVMConstIntCast(ConstantVal: ValueRef; ToType: TypeRef;
                       isSigned: Bool): ValueRef;
PROCEDURE LLVMConstFPCast(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE LLVMConstSelect(ConstantCondition: ValueRef;
                      ConstantIfTrue: ValueRef;
                      ConstantIfFalse: ValueRef): ValueRef;
PROCEDURE LLVMConstExtractElement(VectorConstant: ValueRef;
                              IndexConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstInsertElement(VectorConstant: ValueRef;
                             ElementValueConstant: ValueRef;
                             IndexConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstShuffleVector(VectorAconstant, VectorBConstant: ValueRef;
                             MaskConstant: ValueRef): ValueRef;
PROCEDURE LLVMConstExtractValue(AggConstant: ValueRef;
                            IdxList: UNTRACED REF unsigned;
                            NumIdx: unsigned): ValueRef;
PROCEDURE LLVMConstInsertValue(AggConstant: ValueRef;
                           ElementValueConstant: ValueRef;
                           IdxList: UNTRACED REF unsigned;
                           NumIdx: unsigned): ValueRef;
PROCEDURE LLVMBlockAddress(F: ValueRef; BB: BasicBlockRef): ValueRef;
(* deprecated : use LLVMGetInlineAsm instead *)
PROCEDURE LLVMConstInlineAsm(Ty: TypeRef;
                         AsmString, Constraints: const_char_star;
                         HasSideEffects, IsAlignStack: Bool): ValueRef;

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreValueConstantGlobals Global Values
 *
 * This group contains functions that operate on global values. Functions in
 * this group relate to functions in the llvm::GlobalValue class tree.
 *
 * @see llvm::GlobalValue
 *
 * @{
 *)

PROCEDURE LLVMGetGlobalParent(Global: ValueRef): ModuleRef;
PROCEDURE LLVMIsDeclaration(Global: ValueRef): Bool;
PROCEDURE LLVMGetLinkage(Global: ValueRef): Linkage;
PROCEDURE LLVMSetLinkage(Global: ValueRef; Linkage: Linkage);
PROCEDURE LLVMGetSection(Global: ValueRef): const_char_star;
PROCEDURE LLVMSetSection(Global: ValueRef; Section: const_char_star);
PROCEDURE LLVMGetVisibility(Global: ValueRef): Visibility;
PROCEDURE LLVMSetVisibility(Global: ValueRef; Viz: Visibility);

PROCEDURE LLVMGetDLLStorageClass (Global: ValueRef;
): DLLStorageClass;

PROCEDURE LLVMSetDLLStorageClass (Global: ValueRef;
Class: DLLStorageClass;
);

PROCEDURE LLVMGetUnnamedAddress (Global: ValueRef;
): UnnamedAddr;

PROCEDURE LLVMSetUnnamedAddress (Global: ValueRef;
unNamedAddr: UnnamedAddr);

(**
 * Returns the "value type" of a global value.  This differs from the formal
 * type of a global value which is always a pointer type.
 *
 * @see llvm::GlobalValue::getValueType()
 *)
PROCEDURE LLVMGlobalGetValueType(Global : ValueRef) : TypeRef;


(**
 * @defgroup LLVMCCoreValueWithAlignment Values with alignment
 *
 * Functions in this group only apply to values with alignment, i.e.
 * global variables, load and store instructions.
 *)

(**
 * Obtain the preferred alignment of the value.
 * @see llvm::AllocaInst::getAlignment()
 * @see llvm::LoadInst::getAlignment()
 * @see llvm::StoreInst::getAlignment()
 * @see llvm::GlobalValue::getAlignment()
 *)

PROCEDURE LLVMGetAlignment(V : ValueRef) : unsigned;

(**
 * Set the preferred alignment of the value.
 * @see llvm::AllocaInst::setAlignment()
 * @see llvm::LoadInst::setAlignment()
 * @see llvm::StoreInst::setAlignment()
 * @see llvm::GlobalValue::setAlignment()
 *)
PROCEDURE LLVMSetAlignment(V: ValueRef; Bytes : unsigned);


(**
 * Sets a metadata attachment, erasing the existing metadata attachment if
 * it already exists for the given kind.
 *
 * @see llvm::GlobalObject::setMetadata()
 *)
PROCEDURE LLVMGlobalSetMetadata (Global: ValueRef; Kind: uint32_t;
                             MD: MetadataRef);

(**
 * Erases a metadata attachment of the given kind if it exists.
 *
 * @see llvm::GlobalObject::eraseMetadata()
 *)
PROCEDURE LLVMGlobalEraseMetadata (Global: ValueRef;
Kind: uint32_t);

(**
 * Removes all metadata attachments from this value.
 *
 * @see llvm::GlobalObject::clearMetadata()
 *)
PROCEDURE LLVMGlobalClearMetadata (Global: ValueRef);

(**
 * Retrieves an array of metadata entries representing the metadata attached to
 * this value. The caller is responsible for freeing this array by calling
 * \c LLVMDisposeValueMetadataEntries.
 *
 * @see llvm::GlobalObject::getAllMetadata()
 *)                                                  
PROCEDURE LLVMGlobalCopyAllMetadata (Value: ValueRef;
                                 VAR NumEntries: uint32_t) : ValueMetadataEntryRef;


(**
 * Destroys value metadata entries.
 *)
PROCEDURE LLVMDisposeValueMetadataEntries (Entries: ValueMetadataEntryRef);

(**
 * Returns the kind of a value metadata entry at a specific index.
 *)                                  
PROCEDURE LLVMValueMetadataEntriesGetKind(Entries: ValueMetadataEntryRef;
                                      Index: uint32_t) : uint32_t;
(**
 * Returns the underlying metadata node of a value metadata entry at a
 * specific index.
 *)
PROCEDURE LLVMValueMetadataEntriesGetMetadata(Entries: ValueMetadataEntryRef;
                                          Index: uint32_t): MetadataRef;
(**
  * @}
  *)
(**
 * @defgroup LLVMCoreValueConstantGlobalVariable Global Variables
 *
 * This group contains functions that operate on global variable values.
 *
 * @see llvm::GlobalVariable
 *
 * @{
 *)
PROCEDURE LLVMAddGlobal(M: ModuleRef; Ty: TypeRef; Name: const_char_star): ValueRef;
PROCEDURE LLVMAddGlobalInAddressSpace(M: ModuleRef; Ty: TypeRef;
                                  Name: const_char_star;
                                  AddressSpace: unsigned): ValueRef;
PROCEDURE LLVMGetNamedGlobal(M: ModuleRef; Name: const_char_star): ValueRef;
PROCEDURE LLVMGetFirstGlobal(M: ModuleRef): ValueRef;
PROCEDURE LLVMGetLastGlobal(M: ModuleRef): ValueRef;
PROCEDURE LLVMGetNextGlobal(GlobalVar: ValueRef): ValueRef;
PROCEDURE LLVMGetPreviousGlobal(GlobalVar: ValueRef): ValueRef;
PROCEDURE LLVMDeleteGlobal(GlobalVar: ValueRef);
PROCEDURE LLVMGetInitializer(GlobalVar: ValueRef): ValueRef;
PROCEDURE LLVMSetInitializer(GlobalVar: ValueRef; ConstantVal: ValueRef);
PROCEDURE LLVMIsThreadLocal(GlobalVar: ValueRef): Bool;
PROCEDURE LLVMSetThreadLocal(GlobalVar: ValueRef; IsThreadLocal: Bool);
PROCEDURE LLVMIsGlobalConstant(GlobalVar: ValueRef): Bool;
PROCEDURE LLVMSetGlobalConstant(GlobalVar: ValueRef; IsConstant: Bool);

PROCEDURE LLVMGetThreadLocalMode(GlobalVar: ValueRef;
): ThreadLocalMode;

PROCEDURE LLVMSetThreadLocalMode (GlobalVar: ValueRef;
Mode: ThreadLocalMode);


PROCEDURE LLVMIsExternallyInitialized (GlobalVar: ValueRef;
): Bool;

PROCEDURE LLVMSetExternallyInitialized (GlobalVar: ValueRef;
IsExtInit: Bool);

(**
 * @}
 *)

(**
 * @defgroup LLVMCoreValueConstantGlobalAlias Global Aliases
 *
 * This group contains function that operate on global alias values.
 *
 * @see llvm::GlobalAlias
 *
 * @{
 *)
PROCEDURE LLVMAddAlias(M: ModuleRef; Ty: TypeRef; Aliasee: ValueRef;
                   Name: const_char_star): ValueRef;

(**
 * Obtain a GlobalAlias value from a Module by its name.
 *
 * The returned value corresponds to a llvm::GlobalAlias value.
 *
 * @see llvm::Module::getNamedAlias()
 *)
PROCEDURE LLVMGetNamedGlobalAlias (M: ModuleRef;
 Name: char_star;
NameLen: uint32_t;
): ValueRef;

(**
 * Obtain an iterator to the first GlobalAlias in a Module.
 *
 * @see llvm::Module::alias_begin()
 *)
PROCEDURE LLVMGetFirstGlobalAlias (M: ModuleRef;
): ValueRef;

(**
 * Obtain an iterator to the last GlobalAlias in a Module.
 *
 * @see llvm::Module::alias_end()
 *)
PROCEDURE LLVMGetLastGlobalAlias (M: ModuleRef;
): ValueRef;

(**
 * Advance a GlobalAlias iterator to the next GlobalAlias.
 *
 * Returns NULL if the iterator was already at the end and there are no more
 * global aliases.
 *)
PROCEDURE LLVMGetNextGlobalAlias (GA: ValueRef;
): ValueRef;

(**
 * Decrement a GlobalAlias iterator to the previous GlobalAlias.
 *
 * Returns NULL if the iterator was already at the beginning and there are
 * no previous global aliases.
 *)
PROCEDURE LLVMGetPreviousGlobalAlias (GA: ValueRef;
): ValueRef;

(**
 * Retrieve the target value of an alias.
 *)
PROCEDURE LLVMAliasGetAliasee (Alias: ValueRef;
): ValueRef;

(**
 * Set the target value of an alias.
 *)
PROCEDURE LLVMAliasSetAliasee (Alias, Aliasee: ValueRef;
);

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreValueFunction Function values
 *
 * Functions in this group operate on LLVMValueRef instances that
 * correspond to llvm::Function instances.
 *
 * @see llvm::Function
 *
 * @{
 *)

(**
 * Remove a function from its containing module and delete it.
 *
 * @see llvm::Function::eraseFromParent()
 *)
PROCEDURE LLVMDeleteFunction(Fn: ValueRef);

(**
 * Check whether the given function has a personality function.
 *
 * @see llvm::Function::hasPersonalityFn()
 *)
PROCEDURE LLVMHasPersonalityFn(Fn : ValueRef) : Bool;

(**
 * Obtain the personality function attached to the function.
 *
 * @see llvm::Function::getPersonalityFn()
 *)
PROCEDURE LLVMGetPersonalityFn(Fn : ValueRef) : ValueRef;

(**
 * Set the personality function attached to the function.
 *
 * @see llvm::Function::setPersonalityFn()
 *)
PROCEDURE LLVMSetPersonalityFn(Fn, PersonalityFn : ValueRef);

(**
 * Obtain the ID number from a function instance.
 *
 * @see llvm::Function::getIntrinsicID()
 *)
PROCEDURE LLVMGetIntrinsicID(Fn: ValueRef): unsigned;

(**
 * Create or insert the declaration of an intrinsic.  For overloaded intrinsics,
 * parameter types must be provided to uniquely identify an overload.
 *
 * @see llvm::Intrinsic::getDeclaration()
 *)
 
(* this finally duplicates our function in m3extras so 
 can prob dlete that one *)
PROCEDURE LLVMGetIntrinsicDeclaration(Mod : ModuleRef;
                                      ID : unsigned;
                                      ParamTypes : UNTRACED REF TypeRef;
                                      ParamCount : unsigned) : ValueRef;

                             
(**
 * Retrieves the type of an intrinsic.  For overloaded intrinsics, parameter
 * types must be provided to uniquely identify an overload.
 *
 * @see llvm::Intrinsic::getType()
 *)
PROCEDURE LLVMIntrinsicGetType(C : ContextRef; 
                               ID : unsigned;
                               ParamTypes : UNTRACED REF TypeRef;
                               ParamCount : unsigned) : TypeRef;


(**
 * Retrieves the name of an intrinsic.
 *
 * @see llvm::Intrinsic::getName()
 *)
PROCEDURE LLVMIntrinsicGetName(ID : unsigned; VAR NameLength : unsigned) : char_star;


(**
 * Copies the name of an overloaded intrinsic identified by a given list of
 * parameter types.
 *
 * Unlike LLVMIntrinsicGetName, the caller is responsible for freeing the
 * returned string.
 *
 * @see llvm::Intrinsic::getName()
 *)
PROCEDURE LLVMIntrinsicCopyOverloadedName(ID : unsigned;
                                      ParamTypes : UNTRACED REF TypeRef;
                                      ParamCount : unsigned;
                                      VAR NameLength : unsigned) : char_star;
                                            
(**
 * Obtain if the intrinsic identified by the given ID is overloaded.
 *
 * @see llvm::Intrinsic::isOverloaded()
 *)
PROCEDURE LLVMIntrinsicIsOverloaded(ID : unsigned) : Bool;

(* Keep this in sync with enum M3Intrinsic in M3Extras.h: *) 
TYPE M3Intrinsic = {
  m3memset,
  m3memcpy,
  m3memmov,
  m3round,
  m3floor,
  m3trunc,
  m3ceil,
  m3fabs,
  m3minnum,
  m3maxnum,
  m3fshl,
  m3fshr
};

(* return the LLVm version *)
PROCEDURE GetLLVMVersion(VAR major,minor : int);
(* ^Binding found in M3Extras.h and M3Extras.cpp. *) 

(* return the intrinsic id from the enumeration. *) 
PROCEDURE GetM3IntrinsicId(Id: M3Intrinsic) : unsigned;
(* ^Binding found in M3Extras.h and M3Extras.cpp. *) 

(* Get the function decl for the intrinsic with id and the type sig Types *)
(*
deprecated can delete - now in Core as llvmgetinstrinsicdeclaration
PROCEDURE LLVMGetDeclaration(M: ModuleRef;
                             id : unsigned;
                             Types: UNTRACED REF TypeRef;
                             Count: unsigned): ValueRef;
*)
(* ^Binding found in M3Extras.h and M3Extras.cpp. *) 

(* Get the debug location info for this scope, line and col *)
PROCEDURE DIBGetDebugLoc(C : ContextRef;
                         line,col : unsigned;
                         scope : MetadataRef;
                         inlinedAt : MetadataRef): MetadataRef;                          
(* ^Binding found in M3Extras.h and M3Extras.cpp. *) 

(* Set the debug tag for this function *)
PROCEDURE DIBSetFunctionTag(func : ValueRef;
                           sp : MetadataRef);
(* ^Binding found in M3Extras.h and M3Extras.cpp. *) 

(* Set the debug tag for this global *)
PROCEDURE DIBSetGlobalTag(global : ValueRef;
                          gve : MetadataRef);
(* ^Binding found in M3Extras.h and M3Extras.cpp. *) 

TYPE
  Quad = RECORD a,b : uint64_t; END;
  QuadRef = UNTRACED REF Quad;
  
(* Get 128 bit float constant *)
PROCEDURE LLVMConstQuad(C : ContextRef; q : QuadRef) : ValueRef;
(* ^Binding found in M3Extras.h and M3Extras.cpp. *) 

(**
 * Obtain the calling convention of a function.
 *
 * The returned value corresponds to the LLVMCallConv enumeration.
 *
 * @see llvm::Function::getCallingConv()
 *)
PROCEDURE LLVMGetFunctionCallConv(Fn: ValueRef): unsigned;

(**
 * Set the calling convention of a function.
 *
 * @see llvm::Function::setCallingConv()
 *
 * @param Fn Function to operate on
 * @param CC LLVMCallConv to set calling convention to
 *)
PROCEDURE LLVMSetFunctionCallConv(Fn: ValueRef; CC: unsigned);

(**
 * Obtain the name of the garbage collector to use during code
 * generation.
 *
 * @see llvm::Function::getGC()
 *)
PROCEDURE LLVMGetGC(Fn: ValueRef): const_char_star;

(**
 * Define the garbage collector to use during code generation.
 *
 * @see llvm::Function::setGC()
 *)
PROCEDURE LLVMSetGC(Fn: ValueRef; Name: const_char_star);

 (**
 * Add an attribute to a function.
 *
 * @see llvm::Function::addAttribute()
 *)
PROCEDURE LLVMAddAttributeAtIndex(Fn: ValueRef; 
                                  Idx : AttributeIndex; 
                                  A: AttributeRef);
PROCEDURE LLVMGetAttributeCountAtIndex(Fn : ValueRef; Idx : AttributeIndex) : unsigned;
PROCEDURE LLVMGetAttributesAtIndex(Fn : ValueRef; 
                                   Idx : AttributeIndex;
                                   VAR Attrs : AttributeRef);
PROCEDURE LLVMGetEnumAttributeAtIndex(Fn : ValueRef; 
                                      Idx : AttributeIndex;
                                      kindID : unsigned) : AttributeRef; 
PROCEDURE LLVMSetStringAttributeAtIndex(Fn : ValueRef; 
                                        Idx : AttributeIndex;
                                        K : const_char_star;
                                        KLen : unsigned) : AttributeRef;
PROCEDURE LLVMRemoveEnumAttributeAtIndex(Fn : ValueRef; 
                                         Idx : AttributeIndex;
                                         kindID : unsigned);
PROCEDURE LLVMRemoveStringAttributeAtIndex(Fn : ValueRef; 
                                          Idx : AttributeIndex;
                                          K : const_char_star;
                                          KLen : unsigned);
(**
 * Add a target-dependent attribute to a function
 * @see llvm::AttrBuilder::addAttribute()
 *)
PROCEDURE LLVMAddTargetDependentFunctionAttr(Fn : ValueRef; 
                                             A : const_char_star;
                                             V : const_char_star);                                        

(**
 * @defgroup LLVMCCoreValueFunctionParameters Function Parameters
 *
 * Functions in this group relate to arguments/parameters on functions.
 *
 * Functions in this group expect LLVMValueRef instances that correspond
 * to llvm::Function instances.
 *
 * @{
 *)
 
(**
 * Obtain the number of parameters in a function.
 *
 * @see llvm::Function::arg_size()
 *)
PROCEDURE LLVMCountParams(Fn: ValueRef): unsigned;

(**
 * Obtain the parameters in a function.
 *
 * The takes a pointer to a pre-allocated array of LLVMValueRef that is
 * at least LLVMCountParams() long. This array will be filled with
 * LLVMValueRef instances which correspond to the parameters the
 * function receives. Each LLVMValueRef corresponds to a llvm::Argument
 * instance.
 *
 * @see llvm::Function::arg_begin()
 *)
PROCEDURE LLVMGetParams(Fn: ValueRef; Params: UNTRACED REF ValueRef);

(**
 * Obtain the parameter at the specified index.
 *
 * Parameters are indexed from 0.
 *
 * @see llvm::Function::arg_begin()
 *)
PROCEDURE LLVMGetParam(Fn: ValueRef; Index: unsigned): ValueRef;

(**
 * Obtain the function to which this argument belongs.
 *
 * Unlike other functions in this group, this one takes a LLVMValueRef
 * that corresponds to a llvm::Attribute.
 *
 * The returned LLVMValueRef is the llvm::Function to which this
 * argument belongs.
 *)
PROCEDURE LLVMGetParamParent(Inst: ValueRef): ValueRef;

(**
 * Obtain the first parameter to a function.
 *
 * @see llvm::Function::arg_begin()
 *)
PROCEDURE LLVMGetFirstParam(Fn: ValueRef): ValueRef;

(**
 * Obtain the last parameter to a function.
 *
 * @see llvm::Function::arg_end()
 *)
PROCEDURE LLVMGetLastParam(Fn: ValueRef): ValueRef;

(**
 * Obtain the next parameter to a function.
 *
 * This takes a LLVMValueRef obtained from LLVMGetFirstParam() (which is
 * actually a wrapped iterator) and obtains the next parameter from the
 * underlying iterator.
 *)
PROCEDURE LLVMGetNextParam(Arg: ValueRef): ValueRef;

(**
 * Obtain the previous parameter to a function.
 *
 * This is the opposite of LLVMGetNextParam().
 *)
PROCEDURE LLVMGetPreviousParam(Arg: ValueRef): ValueRef;

(**
 * Set the alignment for a function parameter.
 *
 * @see llvm::Argument::addAttr()
 * @see llvm::Attribute::constructAlignmentFromInt()
 *)
PROCEDURE LLVMSetParamAlignment(Arg: ValueRef; align: unsigned);

(**
 * @}
 *)

(**
 * @}
 *)

(**
 * @}
 *)

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreValueMetadata Metadata
 *
 * @{
 *)

(**
 * Obtain a MDString value from a context.
 *
 * The returned instance corresponds to the llvm::MDString class.
 *
 * The instance is specified by string data of a specified length. The
 * string content is copied, so the backing memory can be freed after
 * this function returns.
 *)
PROCEDURE LLVMMDStringInContext(C: ContextRef; 
                                Str: const_char_star;
                                SLen: unsigned): ValueRef;

(**
 * Obtain a MDString value from the global context.
 *)
PROCEDURE LLVMMDString(Str: const_char_star; SLen: unsigned): ValueRef;

(**
 * Obtain a MDNode value from a context.
 *
 * The returned value corresponds to the llvm::MDNode class.
 *)
PROCEDURE LLVMMDNodeInContext(C: ContextRef; Vals: UNTRACED REF ValueRef;
                          Count: unsigned): ValueRef;

(**
 * Obtain a MDNode value from the global context.
 *)
PROCEDURE LLVMMDNode(Vals: UNTRACED REF ValueRef; Count: unsigned): ValueRef;

(**
 * Obtain a Metadata as a Value.
 *)
PROCEDURE LLVMMetadataAsValue(C : ContextRef; MD : MetadataRef) : ValueRef;

(**
 * Obtain a Value as a Metadata.
 *)
PROCEDURE LLVMValueAsMetadata(Val : ValueRef) : MetadataRef;

(**
 * Obtain the underlying string from a MDString value.
 *
 * @param V Instance to obtain string from.
 * @param Len Memory address which will hold length of returned string.
 * @return String data in MDString.
 *)
PROCEDURE LLVMGetMDString(V: ValueRef; VAR Len: unsigned): const_char_star;

(**
 * Obtain the number of operands from an MDNode value.
 *
 * @param V MDNode to get number of operands from.
 * @return Number of operands of the MDNode.
 *)
PROCEDURE LLVMGetMDNodeNumOperands(V : ValueRef) : unsigned;

(**
 * Obtain the given MDNode's operands.
 *
 * The passed LLVMValueRef pointer should point to enough memory to hold all of
 * the operands of the given MDNode (see LLVMGetMDNodeNumOperands) as
 * LLVMValueRefs. This memory will be populated with the LLVMValueRefs of the
 * MDNode's operands.
 *
 * @param V MDNode to get the operands from.
 * @param Dest Destination array for operands.
 *)
PROCEDURE LLVMGetMDNodeOperands(V : ValueRef; VAR Dest : ValueRef);

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreValueBasicBlock Basic Block
 *
 * A basic block represents a single entry single exit section of code.
 * Basic blocks contain a list of instructions which form the body of
 * the block.
 *
 * Basic blocks belong to functions. They have the type of label.
 *
 * Basic blocks are themselves values. However, the C API models them as
 * LLVMBasicBlockRef.
 *
 * @see llvm::BasicBlock
 *
 * @{
 *)

(**
 * Convert a basic block instance to a value type.
 *)
PROCEDURE LLVMBasicBlockAsValue(BB: BasicBlockRef): ValueRef;

(**
 * Determine whether a LLVMValueRef is itself a basic block.
 *)
PROCEDURE LLVMValueIsBasicBlock(Val: ValueRef): Bool;

(**
 * Convert a LLVMValueRef to a LLVMBasicBlockRef instance.
 *)
PROCEDURE LLVMValueAsBasicBlock(Val: ValueRef): BasicBlockRef;

(**
 * Obtain the string name of a basic block.
 *)
PROCEDURE LLVMGetBasicBlockName(BB : BasicBlockRef) : const_char_star;

(**
 * Obtain the function to which a basic block belongs.
 *
 * @see llvm::BasicBlock::getParent()
 *)
PROCEDURE LLVMGetBasicBlockParent(BB: BasicBlockRef): ValueRef;

(**
 * Obtain the terminator instruction for a basic block.
 *
 * If the basic block does not have a terminator (it is not well-formed
 * if it doesn't), then NULL is returned.
 *
 * The returned LLVMValueRef corresponds to a llvm::Instruction.
 *
 * @see llvm::BasicBlock::getTerminator()
 *)
PROCEDURE LLVMGetBasicBlockTerminator(BB: BasicBlockRef): ValueRef;

(**
 * Obtain the number of basic blocks in a function.
 *
 * @param Fn Function value to operate on.
 *)
PROCEDURE LLVMCountBasicBlocks(Fn: ValueRef): unsigned;

(**
 * Obtain all of the basic blocks in a function.
 *
 * This operates on a function value. The BasicBlocks parameter is a
 * pointer to a pre-allocated array of LLVMBasicBlockRef of at least
 * LLVMCountBasicBlocks() in length. This array is populated with
 * LLVMBasicBlockRef instances.
 *)
PROCEDURE LLVMGetBasicBlocks(Fn: ValueRef; BasicBlocks: UNTRACED REF BasicBlockRef);

(**
 * Obtain the first basic block in a function.
 *
 * The returned basic block can be used as an iterator. You will likely
 * eventually call into LLVMGetNextBasicBlock() with it.
 *
 * @see llvm::Function::begin()
 *)
PROCEDURE LLVMGetFirstBasicBlock(Fn: ValueRef): BasicBlockRef;

(**
 * Obtain the last basic block in a function.
 *
 * @see llvm::Function::end()
 *)
PROCEDURE LLVMGetLastBasicBlock(Fn: ValueRef): BasicBlockRef;

(**
 * Advance a basic block iterator.
 *)
PROCEDURE LLVMGetNextBasicBlock(BB: BasicBlockRef): BasicBlockRef;

(**
 * Go backwards in a basic block iterator.
 *)
PROCEDURE LLVMGetPreviousBasicBlock(BB: BasicBlockRef): BasicBlockRef;

(**
 * Obtain the basic block that corresponds to the entry point of a
 * function.
 *
 * @see llvm::Function::getEntryBlock()
 *)
PROCEDURE LLVMGetEntryBasicBlock(Fn: ValueRef): BasicBlockRef;

(**
 * Create a new basic block without inserting it into a function.
 *
 * @see llvm::BasicBlock::Create()
 *)
PROCEDURE LLVMCreateBasicBlockInContext(C : ContextRef;
                                        Name : const_char_star) : BasicBlockRef;
                                                
(**
 * Append a basic block to the end of a function.
 *
 * @see llvm::BasicBlock::Create()
 *)
PROCEDURE LLVMAppendBasicBlockInContext(C: ContextRef;
                                    Fn: ValueRef;
                                    Name: const_char_star): BasicBlockRef;

(**
 * Append a basic block to the end of a function using the global
 * context.
 *
 * @see llvm::BasicBlock::Create()
 *)
PROCEDURE LLVMAppendBasicBlock(Fn: ValueRef; Name: const_char_star): BasicBlockRef;

(**
 * Insert a basic block in a function before another basic block.
 *
 * The function to add to is determined by the function of the
 * passed basic block.
 *
 * @see llvm::BasicBlock::Create()
 *)
PROCEDURE LLVMInsertBasicBlockInContext(C: ContextRef;
                                    BB: BasicBlockRef;
                                    Name: const_char_star): BasicBlockRef;

(**
 * Insert a basic block in a function using the global context.
 *
 * @see llvm::BasicBlock::Create()
 *)
PROCEDURE LLVMInsertBasicBlock(InsertBeforeBB: BasicBlockRef;
                           Name: const_char_star): BasicBlockRef;

(**
 * Remove a basic block from a function and delete it.
 *
 * This deletes the basic block from its containing function and deletes
 * the basic block itself.
 *
 * @see llvm::BasicBlock::eraseFromParent()
 *)
PROCEDURE LLVMDeleteBasicBlock(BB: BasicBlockRef);

(**
 * Remove a basic block from a function.
 *
 * This deletes the basic block from its containing function but keep
 * the basic block alive.
 *
 * @see llvm::BasicBlock::removeFromParent()
 *)
PROCEDURE LLVMRemoveBasicBlockFromParent(BB: BasicBlockRef);

(**
 * Move a basic block to before another one.
 *
 * @see llvm::BasicBlock::moveBefore()
 *)
PROCEDURE LLVMMoveBasicBlockBefore(BB, MovePos: BasicBlockRef);

(**
 * Move a basic block to after another one.
 *
 * @see llvm::BasicBlock::moveAfter()
 *)
PROCEDURE LLVMMoveBasicBlockAfter(BB, MovePos: BasicBlockRef);

(**
 * Obtain the first instruction in a basic block.
 *
 * The returned LLVMValueRef corresponds to a llvm::Instruction
 * instance.
 *)
PROCEDURE LLVMGetFirstInstruction(BB: BasicBlockRef): ValueRef;

(**
 * Obtain the last instruction in a basic block.
 *
 * The returned LLVMValueRef corresponds to a LLVM:Instruction.
 *)
PROCEDURE LLVMGetLastInstruction(BB: BasicBlockRef): ValueRef;

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreValueInstruction Instructions
 *
 * Functions in this group relate to the inspection and manipulation of
 * individual instructions.
 *
 * In the C++ API, an instruction is modeled by llvm::Instruction. This
 * class has a large number of descendents. llvm::Instruction is a
 * llvm::Value and in the C API, instructions are modeled by
 * LLVMValueRef.
 *
 * This group also contains sub-groups which operate on specific
 * llvm::Instruction types, e.g. llvm::CallInst.
 *
 * @{
 *)

(**
 * Determine whether an instruction has any metadata attached.
 *)
PROCEDURE LLVMHasMetadata(Val: ValueRef): int;

(**
 * Return metadata associated with an instruction value.
 *)
PROCEDURE LLVMGetMetadata(Val: ValueRef; KindID: unsigned): ValueRef;

(**
 * Set metadata associated with an instruction value.
 *)
PROCEDURE LLVMSetMetadata(Val: ValueRef; KindID: unsigned; Node: ValueRef);

(**
 * Returns the metadata associated with an instruction value, but filters out
 * all the debug locations.
 *
 * @see llvm::Instruction::getAllMetadataOtherThanDebugLoc()
 *)
PROCEDURE LLVMInstructionGetAllMetadataOtherThanDebugLoc (Instr: ValueRef;
VAR NumEntries: unsigned;
): UNTRACED REF ValueMetadataEntryRef;


(**
 * Obtain the basic block to which an instruction belongs.
 *
 * @see llvm::Instruction::getParent()
 *)
PROCEDURE LLVMGetInstructionParent(Inst: ValueRef): BasicBlockRef;

(**
 * Obtain the instruction that occurs after the one specified.
 *
 * The next instruction will be from the same basic block.
 *
 * If this is the last instruction in a basic block, NULL will be
 * returned.
 *)
PROCEDURE LLVMGetNextInstruction(Inst: ValueRef): ValueRef;

(**
 * Obtain the instruction that occurred before this one.
 *
 * If the instruction is the first instruction in a basic block, NULL
 * will be returned.
 *)
PROCEDURE LLVMGetPreviousInstruction(Inst: ValueRef): ValueRef;

(**
 * Remove and delete an instruction.
 *
 * The instruction specified is removed from its containing building
 * block but is kept alive.
 *
 * @see llvm::Instruction::removeFromParent()
 *)
PROCEDURE LLVMInstructionRemoveFromParent(Inst: ValueRef);

(**
 * Remove and delete an instruction.
 *
 * The instruction specified is removed from its containing building
 * block and then deleted.
 *
 * @see llvm::Instruction::eraseFromParent()
 *)
PROCEDURE LLVMInstructionEraseFromParent(Inst: ValueRef);

(**
 * Obtain the code opcode for an individual instruction.
 *
 * @see llvm::Instruction::getOpCode()
 *)
PROCEDURE LLVMGetInstructionOpcode(Inst: ValueRef): Opcode;

(**
 * Obtain the predicate of an instruction.
 *
 * This is only valid for instructions that correspond to llvm::ICmpInst
 * or llvm::ConstantExpr whose opcode is llvm::Instruction::ICmp.
 *
 * @see llvm::ICmpInst::getPredicate()
 *)
PROCEDURE LLVMGetICmpPredicate(Inst: ValueRef): IntPredicate;

(**
 * Obtain the float predicate of an instruction.
 *
 * This is only valid for instructions that correspond to llvm::FCmpInst
 * or llvm::ConstantExpr whose opcode is llvm::Instruction::FCmp.
 *
 * @see llvm::FCmpInst::getPredicate()
 *)
PROCEDURE LLVMGetFCmpPredicate(Inst : ValueRef) : RealPredicate;

(**
 * Create a copy of 'this' instruction that is identical in all ways
 * except the following:
 *   * The instruction has no parent
 *   * The instruction has no name
 *
 * @see llvm::Instruction::clone()
 *)
PROCEDURE LLVMInstructionClone(Inst : ValueRef) : ValueRef;

(**
 * Determine whether an instruction is a terminator. This routine is named to
 * be compatible with historical functions that did this by querying the
 * underlying C++ type.
 *
 * @see llvm::Instruction::isTerminator()
 *)
PROCEDURE LLVMIsATerminatorInst(Inst : ValueRef) : ValueRef;

(**
 * @defgroup LLVMCCoreValueInstructionCall Call Sites and Invocations
 *
 * Functions in this group apply to instructions that refer to call
 * sites and invocations. These correspond to C++ types in the
 * llvm::CallInst class tree.
 *
 * @{
 *)

(**
 * Obtain the argument count for a call instruction.
 *
 * This expects an LLVMValueRef that corresponds to a llvm::CallInst or
 * llvm::InvokeInst.
 *
 * @see llvm::CallInst::getNumArgOperands()
 * @see llvm::InvokeInst::getNumArgOperands()
 *)
PROCEDURE LLVMGetNumArgOperands(Instr : ValueRef) : unsigned;

(**
 * Set the calling convention for a call instruction.
 *
 * This expects an LLVMValueRef that corresponds to a llvm::CallInst or
 * llvm::InvokeInst.
 *
 * @see llvm::CallInst::setCallingConv()
 * @see llvm::InvokeInst::setCallingConv()
 *)
PROCEDURE LLVMSetInstructionCallConv(Instr: ValueRef; CC: unsigned);

(**
 * Obtain the calling convention for a call instruction.
 *
 * This is the opposite of LLVMSetInstructionCallConv(). Reads its
 * usage.
 *
 * @see LLVMSetInstructionCallConv()
 *)
PROCEDURE LLVMGetInstructionCallConv(Instr: ValueRef): unsigned;


PROCEDURE LLVMSetInstrParamAlignment(Instr: ValueRef; index, align: unsigned);

PROCEDURE LLVMAddCallSiteAttribute(C : ValueRef;
                                   Idx : AttributeIndex;
                                   A : AttributeRef);
PROCEDURE LLVMGetCallSiteAttributeCount(C : ValueRef;
                                        Idx : AttributeIndex) : unsigned;
PROCEDURE LLVMGetCallSiteAttributes(C : ValueRef; 
                                    Idx : AttributeIndex;
                                    VAR Attrs : AttributeRef);
PROCEDURE LLVMGetCallSiteEnumAttribute(C : ValueRef;
                                      Idx : AttributeIndex;
                                      KindID : unsigned) : AttributeRef;
PROCEDURE LLVMGetCallSiteStringAttribute(C : ValueRef;
                                         Idx : AttributeIndex;
                                         K : const_char_star; 
                                         KLen : unsigned) : AttributeRef;
PROCEDURE LLVMRemoveCallSiteEnumAttribute(C : ValueRef;
                                          Idx : AttributeIndex;
                                          KindID : unsigned);
PROCEDURE LLVMRemoveCallSiteStringAttribute(C : ValueRef;
                                            Idx : AttributeIndex;
                                            K : const_char_star;
                                            KLen : unsigned);

(**
 * Obtain the function type called by this instruction.
 *
 * @see llvm::CallBase::getFunctionType()
 *)
PROCEDURE LLVMGetCalledFunctionType(C : ValueRef) : TypeRef;
                                            
(**
 * Obtain the pointer to the function invoked by this instruction.
 *
 * This expects an LLVMValueRef that corresponds to a llvm::CallInst or
 * llvm::InvokeInst.
 *
 * @see llvm::CallInst::getCalledValue()
 * @see llvm::InvokeInst::getCalledValue()
 *)
PROCEDURE LLVMGetCalledValue(Instr : ValueRef) : ValueRef;

(**
 * Obtain whether a call instruction is a tail call.
 *
 * This only works on llvm::CallInst instructions.
 *
 * @see llvm::CallInst::isTailCall()
 *)
PROCEDURE LLVMIsTailCall(CallInst: ValueRef): Bool;

(**
 * Set whether a call instruction is a tail call.
 *
 * This only works on llvm::CallInst instructions.
 *
 * @see llvm::CallInst::setTailCall()
 *)
PROCEDURE LLVMSetTailCall(CallInst: ValueRef; IsTailCall: Bool);

(**
 * Return the normal destination basic block.
 *
 * This only works on llvm::InvokeInst instructions.
 *
 * @see llvm::InvokeInst::getNormalDest()
 *)
PROCEDURE LLVMGetNormalDest(InvokeInst : ValueRef) : BasicBlockRef;

(**
 * Return the unwind destination basic block.
 *
 * This only works on llvm::InvokeInst instructions.
 *
 * @see llvm::InvokeInst::getUnwindDest()
 *)
PROCEDURE LLVMGetUnwindDest(InvokeInst : ValueRef) : BasicBlockRef;

(**
 * Set the normal destination basic block.
 *
 * This only works on llvm::InvokeInst instructions.
 *
 * @see llvm::InvokeInst::setNormalDest()
 *)
PROCEDURE LLVMSetNormalDest(InvokeInst : ValueRef; B : BasicBlockRef);

(**
 * Set the unwind destination basic block.
 *
 * This only works on llvm::InvokeInst instructions.
 *
 * @see llvm::InvokeInst::setUnwindDest()
 *)
PROCEDURE LLVMSetUnwindDest(InvokeInst : ValueRef; B : BasicBlockRef);

(**
 * @}
 *)
 
(**
 * @defgroup LLVMCCoreValueInstructionTerminator Terminators
 *
 * Functions in this group only apply to instructions that map to
 * llvm::TerminatorInst instances.
 *
 * @{
 *)

(**
 * Return the number of successors that this terminator has.
 *
 * @see llvm::TerminatorInst::getNumSuccessors
 *)
PROCEDURE LLVMGetNumSuccessors(Term : ValueRef) : unsigned;

(**
 * Return the specified successor.
 *
 * @see llvm::TerminatorInst::getSuccessor
 *)
PROCEDURE LLVMGetSuccessor(Term : ValueRef; i : unsigned) : BasicBlockRef;

(**
 * Update the specified successor to point at the provided block.
 *
 * @see llvm::TerminatorInst::setSuccessor
 *)
PROCEDURE LLVMSetSuccessor(Term : ValueRef; i : unsigned; block : BasicBlockRef);

(**
 * Return if a branch is conditional.
 *
 * This only works on llvm::BranchInst instructions.
 *
 * @see llvm::BranchInst::isConditional
 *)
PROCEDURE LLVMIsConditional(Branch : ValueRef) : Bool;

(**
 * Return the condition of a branch instruction.
 *
 * This only works on llvm::BranchInst instructions.
 *
 * @see llvm::BranchInst::getCondition
 *)
PROCEDURE LLVMGetCondition(Branch : ValueRef) : ValueRef;

(**
 * Set the condition of a branch instruction.
 *
 * This only works on llvm::BranchInst instructions.
 *
 * @see llvm::BranchInst::setCondition
 *)
PROCEDURE LLVMSetCondition(Branch,Cond : ValueRef);


(**
 * Obtain the default destination basic block of a switch instruction.
 *
 * This only works on llvm::SwitchInst instructions.
 *
 * @see llvm::SwitchInst::getDefaultDest()
 *)
PROCEDURE LLVMGetSwitchDefaultDest(SwitchInstr: ValueRef): BasicBlockRef;


(**
 * @defgroup LLVMCCoreValueInstructionAlloca Allocas
 *
 * Functions in this group only apply to instructions that map to
 * llvm::AllocaInst instances.
 *
 * @{
 *)

(**
 * Obtain the type that is being allocated by the alloca instruction.
 *)
PROCEDURE LLVMGetAllocatedType(Alloca : ValueRef) : TypeRef;

(**
 * @}
 *)
 
 
(**
 * @defgroup LLVMCCoreValueInstructionGetElementPointer GEPs
 *
 * Functions in this group only apply to instructions that map to
 * llvm::GetElementPtrInst instances.
 *
 * @{
 *)

(**
 * Check whether the given GEP instruction is inbounds.
 *)
PROCEDURE LLVMIsInBounds(GEP : ValueRef) : Bool;

(**
 * Set the given GEP instruction to be inbounds or not.
 *)
PROCEDURE LLVMSetIsInBounds(GEP : ValueRef; InBounds : Bool);

(**
 * @}
 *)
 
(**
 * @defgroup LLVMCCoreValueInstructionPHINode PHI Nodes
 *
 * Functions in this group only apply to instructions that map to
 * llvm::PHINode instances.
 *
 * @{
 *)

(**
 * Add an incoming value to the end of a PHI list.
 *)
PROCEDURE LLVMAddIncoming(PhiNode: ValueRef;
                      IncomingValues: UNTRACED REF ValueRef;
                      IncomingBlocks: UNTRACED REF BasicBlockRef;
                      Count: unsigned);

(**
 * Obtain the number of incoming basic blocks to a PHI node.
 *)
PROCEDURE LLVMCountIncoming(PhiNode: ValueRef): unsigned;

(**
 * Obtain an incoming value to a PHI node as a LLVMValueRef.
 *)
PROCEDURE LLVMGetIncomingValue(PhiNode: ValueRef; Index: unsigned): ValueRef;

(**
 * Obtain an incoming value to a PHI node as a LLVMBasicBlockRef.
 *)
PROCEDURE LLVMGetIncomingBlock(PhiNode: ValueRef; Index: unsigned): BasicBlockRef;

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreValueInstructionExtractValue ExtractValue
 * @defgroup LLVMCCoreValueInstructionInsertValue InsertValue
 *
 * Functions in this group only apply to instructions that map to
 * llvm::ExtractValue and llvm::InsertValue instances.
 *
 * @{
 *)

(**
 * Obtain the number of indices.
 * NB: This also works on GEP.
 *)
PROCEDURE LLVMGetNumIndices(Inst : ValueRef) : unsigned;

(**
 * Obtain the indices as an array.
 *)
PROCEDURE LLVMGetIndices(Inst : ValueRef) : REF unsigned;

(**
 * @defgroup LLVMCCoreInstructionBuilder Instruction Builders
 *
 * An instruction builder represents a point within a basic block and is
 * the exclusive means of building instructions using the C interface.
 *
 * @{
 *)

PROCEDURE LLVMCreateBuilderInContext(C: ContextRef): BuilderRef;
PROCEDURE LLVMCreateBuilder(): BuilderRef;
PROCEDURE LLVMPositionBuilder(Builder: BuilderRef; Block: BasicBlockRef;
                         Instr: ValueRef);
PROCEDURE LLVMPositionBuilderBefore(Builder: BuilderRef; Instr: ValueRef);
PROCEDURE LLVMPositionBuilderAtEnd(Builder: BuilderRef; Block: BasicBlockRef);
PROCEDURE LLVMGetInsertBlock(Builder: BuilderRef): BasicBlockRef;
PROCEDURE LLVMClearInsertionPosition(Builder: BuilderRef);
PROCEDURE LLVMInsertIntoBuilder(Builder: BuilderRef; Instr: ValueRef);
PROCEDURE LLVMInsertIntoBuilderWithName(Builder: BuilderRef; Instr: ValueRef;
                                    Name: const_char_star);
PROCEDURE LLVMDisposeBuilder(Builder: BuilderRef);

(* Metadata *)
PROCEDURE LLVMSetCurrentDebugLocation(Builder: BuilderRef; L: ValueRef);
PROCEDURE LLVMGetCurrentDebugLocation(Builder: BuilderRef): ValueRef;
PROCEDURE LLVMSetInstDebugLocation(Builder: BuilderRef; Inst: ValueRef);


(* Terminators *)
PROCEDURE LLVMBuildRetVoid(B: BuilderRef): ValueRef;
PROCEDURE LLVMBuildRet(B: BuilderRef; V: ValueRef): ValueRef;
PROCEDURE LLVMBuildAggregateRet(B: BuilderRef; RetVals: UNTRACED REF ValueRef;
                            N: unsigned): ValueRef;
PROCEDURE LLVMBuildBr(B: BuilderRef; Dest: BasicBlockRef): ValueRef;
PROCEDURE LLVMBuildCondBr(B: BuilderRef; If: ValueRef;
                      Then, Else: BasicBlockRef): ValueRef;
PROCEDURE LLVMBuildSwitch(B: BuilderRef; V: ValueRef; Else: BasicBlockRef;
                      NumCases: unsigned): ValueRef;
PROCEDURE LLVMBuildIndirectBr(B: BuilderRef; Addr: ValueRef;
                          NumDests: unsigned): ValueRef;
(* LLVMBuildInvoke is deprecated in favor of LLVMBuildInvoke2, in preparation
 for opaque pointer types.*)                          
PROCEDURE LLVMBuildInvoke(B: BuilderRef; Fn: ValueRef;
                      Args: UNTRACED REF ValueRef; NumArgs: unsigned;
                      Then, Catch: BasicBlockRef;
                      Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildInvoke2(B: BuilderRef; Ty : TypeRef;Fn: ValueRef;
                      Args: UNTRACED REF ValueRef; NumArgs: unsigned;
                      Then, Catch: BasicBlockRef;
                      Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildUnreachable(B: BuilderRef): ValueRef;

(* Exception Handling *)

PROCEDURE LLVMBuildResume(B: BuilderRef; Exn: ValueRef): ValueRef;

PROCEDURE LLVMBuildLandingPad (B: BuilderRef;
Ty: TypeRef;
PersFn: ValueRef;
NumClauses: uint32_t;
 Name: char_star;
): ValueRef;

PROCEDURE LLVMBuildCleanupRet (B: BuilderRef;
CatchPad: ValueRef;
BB: BasicBlockRef;
): ValueRef;

PROCEDURE LLVMBuildCatchRet (B: BuilderRef;
CatchPad: ValueRef;
BB: BasicBlockRef;
): ValueRef;

PROCEDURE LLVMBuildCatchPad (B: BuilderRef;
ParentPad: ValueRef;
 Args: UNTRACED REF ValueRef;
NumArgs: uint32_t;
 Name: char_star;
): ValueRef;

PROCEDURE LLVMBuildCleanupPad (B: BuilderRef;
ParentPad: ValueRef;
 Args: UNTRACED REF ValueRef;
NumArgs: uint32_t;
 Name: char_star;
): ValueRef;

PROCEDURE LLVMBuildCatchSwitch (B: BuilderRef;
ParentPad: ValueRef;
UnwindBB: BasicBlockRef;
NumHandlers: uint32_t;
 Name: char_star;
): ValueRef;

(* Add a case to the switch instruction *)
PROCEDURE LLVMAddCase(Switch, OnVal: ValueRef; Dest: BasicBlockRef);

(* Add a destination to the indirectbr instruction *)
PROCEDURE LLVMAddDestination(IndirectBr: ValueRef; Dest: BasicBlockRef);

(* Get the number of clauses on the landingpad instruction *)
PROCEDURE LLVMGetNumClauses(LandingPad : ValueRef) : unsigned;

(* Get the value of the clause at idnex Idx on the landingpad instruction *)
PROCEDURE LLVMGetClause(LandingPad : ValueRef; Idx : unsigned) : ValueRef;

(* Add a catch or filter clause to the landingpad instruction *)
PROCEDURE LLVMAddClause(LandingPad, ClauseVal: ValueRef);

(* Get the 'cleanup' flag in the landingpad instruction *)
PROCEDURE LLVMIsCleanup(LandingPad : ValueRef) : Bool;

(* Set the 'cleanup' flag in the landingpad instruction *)
PROCEDURE LLVMSetCleanup(LandingPad: ValueRef; Val: Bool);

(* Funclets *)

(* Get the number of funcletpad arguments. *)

PROCEDURE GetArgOperand (Funclet: ValueRef;
i: uint32_t;
): ValueRef;

(* Set a funcletpad argument at the given index. *)
PROCEDURE SetArgOperand (Funclet: ValueRef;
i: uint32_t;
value: ValueRef;
);

(**
 * Get the parent catchswitch instruction of a catchpad instruction.
 *
 * This only works on llvm::CatchPadInst instructions.
 *
 * @see llvm::CatchPadInst::getCatchSwitch()
 *)
PROCEDURE GetParentCatchSwitch (CatchPad: ValueRef;
): ValueRef;

(**
 * Set the parent catchswitch instruction of a catchpad instruction.
 *
 * This only works on llvm::CatchPadInst instructions.
 *
 * @see llvm::CatchPadInst::setCatchSwitch()
 *)
PROCEDURE SetParentCatchSwitch (CatchPad, CatchSwitch: ValueRef;
);

(* Add a destination to the catchswitch instruction *)
PROCEDURE LLVMAddHandler (CatchSwitch: ValueRef;
Dest: BasicBlockRef;
);

(* Get the number of handlers on the catchswitch instruction *)
PROCEDURE LLVMGetNumHandlers (CatchSwitch: ValueRef;
): uint32_t;

(**
 * Obtain the basic blocks acting as handlers for a catchswitch instruction.
 *
 * The Handlers parameter should point to a pre-allocated array of
 * LLVMBasicBlockRefs at least LLVMGetNumHandlers() large. On return, the
 * first LLVMGetNumHandlers() entries in the array will be populated
 * with LLVMBasicBlockRef instances.
 *
 * @param CatchSwitch The catchswitch instruction to operate on.
 * @param Handlers Memory address of an array to be filled with basic blocks.
 *)
PROCEDURE LLVMGetHandlers (CatchSwitch: ValueRef;
 Handlers: UNTRACED REF BasicBlockRef;
);



(* Arithmetic *)
PROCEDURE LLVMBuildAdd(B: BuilderRef; LHS, RHS: ValueRef;
                   Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildNSWAdd(B: BuilderRef; LHS, RHS: ValueRef;
                      Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildNUWAdd(B: BuilderRef; LHS, RHS: ValueRef;
                      Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildFAdd(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildSub(B: BuilderRef; LHS, RHS: ValueRef;
                   Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildNSWSub(B: BuilderRef; LHS, RHS: ValueRef;
                      Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildNUWSub(B: BuilderRef; LHS, RHS: ValueRef;
                      Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildFSub(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildMul(B: BuilderRef; LHS, RHS: ValueRef;
                   Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildNSWMul(B: BuilderRef; LHS, RHS: ValueRef;
                      Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildNUWMul(B: BuilderRef; LHS, RHS: ValueRef;
                      Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildFMul(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildUDiv(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildExtractUDiv(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildSDiv(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildExactSDiv(B: BuilderRef; LHS, RHS: ValueRef;
                         Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildFDiv(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildURem(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildSRem(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildFRem(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildShl(B: BuilderRef; LHS, RHS: ValueRef;
                   Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildLShr(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildAShr(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildAnd(B: BuilderRef; LHS, RHS: ValueRef;
                   Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildOr(B: BuilderRef; LHS, RHS: ValueRef;
                  Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildXor(B: BuilderRef; LHS, RHS: ValueRef;
                   Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildBinop(B: BuilderRef; Op: Opcode; LHS, RHS: ValueRef;
                   Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildNeg(B: BuilderRef; V: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildNSWNeg(B: BuilderRef; V: ValueRef;
                      Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildNUWNeg(B: BuilderRef; V: ValueRef;
                      Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildFNeg(B: BuilderRef; V: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildNot(B: BuilderRef; V: ValueRef;
                   Name: const_char_star): ValueRef;

(* Memory *)
PROCEDURE LLVMBuildMalloc(
    B: BuilderRef; Ty: TypeRef; Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildArrayMalloc(
    B: BuilderRef; Ty: TypeRef; Val: ValueRef; Name: const_char_star): ValueRef;
    
(**
 * Creates and inserts a memset to the specified pointer and the 
 * specified value.
 *
 * @see llvm::IRRBuilder::CreateMemSet()
 *)
PROCEDURE LLVMBuildMemSet (B: BuilderRef;
Ptr, Val, Len: ValueRef;
Align: uint32_t;
): ValueRef;

(**
 * Creates and inserts a memcpy between the specified pointers.
 *
 * @see llvm::IRRBuilder::CreateMemCpy()
 *)
PROCEDURE LLVMBuildMemCpy (B: BuilderRef;
Dst: ValueRef;
DstAlign: uint32_t;
Src: ValueRef;
SrcAlign: uint32_t;
Size: ValueRef;
): ValueRef;

(**
 * Creates and inserts a memmove between the specified pointers.
 *
 * @see llvm::IRRBuilder::CreateMemMove()
 *)
PROCEDURE LLVMBuildMemMove (B: BuilderRef;
Dst: ValueRef;
DstAlign: uint32_t;
Src: ValueRef;
SrcAlign: uint32_t;
Size: ValueRef;
): ValueRef;

PROCEDURE LLVMBuildAlloca(
    B: BuilderRef; Ty: TypeRef; Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildArrayAlloca(
    B: BuilderRef; Ty: TypeRef; Val: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildFree(
    B: BuilderRef; PointerVal: ValueRef): ValueRef;
(* LLVMBuildLoad is deprecated in favor of LLVMBuildLoad2, in preparation for
 opaque pointer types.*)    
PROCEDURE LLVMBuildLoad(
    B: BuilderRef; PointerVal: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildLoad2(
    B: BuilderRef; Ty : TypeRef; PointerVal: ValueRef; Name: const_char_star): ValueRef;    
PROCEDURE LLVMBuildStore(B: BuilderRef; Val, Ptr: ValueRef): ValueRef;
(* LLVMBuildGEP, LLVMBuildInBoundsGEP, and LLVMBuildStructGEP are deprecated in
 favor of LLVMBuild*GEP2, in preparation for opaque pointer types.*)
PROCEDURE LLVMBuildGEP(
    B: BuilderRef; Pointer: ValueRef;
    Indices: UNTRACED REF ValueRef; NumIndices: unsigned;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildInBoundsGEP(
    B: BuilderRef; Pointer: ValueRef;
    Indices: UNTRACED REF ValueRef; NumIndices: unsigned;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildStructGEP(
    B: BuilderRef; Pointer: ValueRef; Idx: unsigned;
    Name: const_char_star := NIL): ValueRef;
PROCEDURE LLVMBuildGEP2(
    B: BuilderRef; Ty : TypeRef; Pointer: ValueRef;
    Indices: UNTRACED REF ValueRef; NumIndices: unsigned;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildInBoundsGEP2(
    B: BuilderRef; Ty : TypeRef; Pointer: ValueRef;
    Indices: UNTRACED REF ValueRef; NumIndices: unsigned;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildStructGEP2(
    B: BuilderRef; Ty : TypeRef; Pointer: ValueRef; Idx: unsigned;
    Name: const_char_star := NIL): ValueRef;    
PROCEDURE LLVMBuildGlobalString(
    B: BuilderRef; Str, Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildGlobalStringPtr(
    B: BuilderRef; Str, Name: const_char_star): ValueRef;
PROCEDURE LLVMGetVolatile(MemoryAccessInst: ValueRef): Bool;
PROCEDURE LLVMSetVolatile(MemoryAccessInst: ValueRef; IsVolatile: Bool);
PROCEDURE LLVMGetOrdering(MemoryAccessInst : ValueRef) : AtomicOrdering;
PROCEDURE LLVMSetOrdering(MemoryAccessInst : ValueRef; Ordering : AtomicOrdering);

(* Casts *)
PROCEDURE LLVMBuildTrunc(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildZExt(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildSExt(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildFPToUI(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildFPToSI(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildUIToFP(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildSIToFP(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildFPTrunc(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildFPExt(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildPtrToInt(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildIntToPtr(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildBitCast(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildZExtOrBitCast(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildSExtOrBitCast(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildTruncOrBitCast(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildCast(
    B: BuilderRef; Op: Opcode; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildPointerCast(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildIntCast2(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef; IsSigned : Bool;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildFPCast( (* Signed cast! *)
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
    
(** Deprecated: This cast is always signed. Use LLVMBuildIntCast2 instead. *)
PROCEDURE LLVMBuildIntCast( (* Signed cast! *)
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
    
(* Comparisons *)
PROCEDURE LLVMBuildICmp(
    B: BuilderRef; Op: IntPredicate; LHS, RHS: ValueRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildFCmp(
    B: BuilderRef; Op: RealPredicate; LHS, RHS: ValueRef;
    Name: const_char_star): ValueRef;

(* Miscellaneous instructions *)
PROCEDURE LLVMBuildPhi(
    B: BuilderRef; Ty: TypeRef; Name: const_char_star): ValueRef;
(* LLVMBuildCall is deprecated in favor of LLVMBuildCall2, in preparation for
 opaque pointer types.*)    
PROCEDURE LLVMBuildCall(
    B: BuilderRef; Fn: ValueRef;
    Args: UNTRACED REF ValueRef; NumArgs: unsigned;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildCall2(
    B: BuilderRef; Ty : TypeRef; Fn: ValueRef;
    Args: UNTRACED REF ValueRef; NumArgs: unsigned;
    Name: const_char_star): ValueRef;    
PROCEDURE LLVMBuildSelect(
    B: BuilderRef; If, Then, Else: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildVAArg(
    B: BuilderRef; List: ValueRef; Ty: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildExtractElement(
    B: BuilderRef; VecVal, Index: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildInsertElement(
    B: BuilderRef; VecVal, EltVal, Index: ValueRef;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildShuffleVector(
    B: BuilderRef; V1, V2, Mask: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildExtractValue(
    B: BuilderRef; AggVal: ValueRef; Index: unsigned;
    Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildInsertValue(
    B: BuilderRef; AggVal, EltVal: ValueRef; Index: unsigned;
    Name: const_char_star): ValueRef;

PROCEDURE LLVMBuildIsNull(
    B: BuilderRef; Val: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildIsNotNull(
    B: BuilderRef; Val: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildPtrDiff(
    B: BuilderRef; LHS, RGS: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE LLVMBuildFence(B: BuilderRef; ordering : AtomicOrdering; singleThread : Bool; Name : const_char_star): ValueRef;
PROCEDURE LLVMBuildAtomicRMW(B : BuilderRef; op : AtomicRMWBinOp;
                                PTR : ValueRef; Val : ValueRef;
                                ordering : AtomicOrdering;
                                singleThread : Bool) : ValueRef;
PROCEDURE LLVMBuildAtomicCmpXchg(B : BuilderRef;
                                PTR : ValueRef ; Cmp: ValueRef; New : ValueRef;
                                successOrdering : AtomicOrdering;
                                failureOrdering : AtomicOrdering;                                
                                singleThread : Bool) : ValueRef;
PROCEDURE LLVMIsAtomicSingleThread(AtomicInst : ValueRef) : Bool;
PROCEDURE LLVMSetAtomicSingleThread(AtomicInst : ValueRef; SingleThread : Bool);
PROCEDURE LLVMGetCmpXchgSuccessOrdering(CmpXchgInst : ValueRef) : AtomicOrdering;
PROCEDURE LLVMSetCmpXchgSuccessOrdering(CmpXchgInst : ValueRef; Ordering : AtomicOrdering);
PROCEDURE LLVMGetCmpXchgFailureOrdering(CmpXchgInst : ValueRef) : AtomicOrdering;
PROCEDURE LLVMSetCmpXchgFailureOrdering(CmpXchgInst : ValueRef; Ordering : AtomicOrdering);

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreModuleProvider Module Providers
 *
 * @{
 *)

(**
 * Changes the type of M so it can be passed to FunctionPassManagers and the
 * JIT.  They take ModuleProviders for historical reasons.
 *)
PROCEDURE LLVMCreateModuleProviderForExistingModule(
    M: ModuleRef): ModuleProviderRef;

(**
 * Destroys the module M.
 *)
PROCEDURE LLVMDisposeModuleProvider(M: ModuleProviderRef);

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreMemoryBuffers Memory Buffers
 *
 * @{
 *)

PROCEDURE LLVMCreateMemoryBufferWithContentsOfFile(
    Path: const_char_star;
    OutMemBuf: UNTRACED REF MemoryBufferRef;
    OutMessage: char_star_star): Bool;
PROCEDURE LLVMCreateMemoryBufferWithSTDIN(
    OutMemBuf: UNTRACED REF MemoryBufferRef;
    OutMessage: char_star_star): Bool;

PROCEDURE LLVMCreateMemoryBufferWithMemoryRange ( InputData: char_star;
InputDataLength: uint32_t;
 BufferName: char_star;
RequiresNullTerminator: Bool;
): MemoryBufferRef;

PROCEDURE LLVMCreateMemoryBufferWithMemoryRangeCopy ( InputData: char_star;
InputDataLength: uint32_t;
 BufferName: char_star;
): MemoryBufferRef;

PROCEDURE LLVMDisposeMemoryBuffer(MemBuf: MemoryBufferRef);

(**
 * @}
 *)

(**
 * @defgroup LLVMCCorePassRegistry Pass Registry
 *
 * @{
 *)

(** Return the global pass registry, for use with initialization functions.
    @see llvm::PassRegistry::getPassRegistry *)
PROCEDURE LLVMGetGlobalPassRegistry(): PassRegistryRef;

(**
 * @}
 *)

(**
 * @defgroup LLVMCCorePassManagers Pass Managers
 *
 * @{
 *)

(** Constructs a new whole-module pass pipeline. This type of pipeline is
    suitable for link-time optimization and whole-module transformations.
    @see llvm::PassManager::PassManager *)
PROCEDURE LLVMCreatePassManager(): PassManagerRef;

(** Constructs a new function-by-function pass pipeline over the module
    provider. It does not take ownership of the module provider. This type of
    pipeline is suitable for code generation and JIT compilation tasks.
    @see llvm::FunctionPassManager::FunctionPassManager *)
PROCEDURE LLVMCreateFunctionPassManagerForModule(M: ModuleRef): PassManagerRef;

(** Deprecated: Use LLVMCreateFunctionPassManagerForModule instead. *)
PROCEDURE LLVMCreateFunctionPassManager(MP: ModuleProviderRef): PassManagerRef;

(** Initializes, executes on the provided module, and finalizes all of the
    passes scheduled in the pass manager. Returns 1 if any of the passes
    modified the module, 0 otherwise.
    @see llvm::PassManager::run(Module&) *)
PROCEDURE LLVMRunPassManager(PM: PassManagerRef; M: ModuleRef): Bool;

(** Initializes all of the function passes scheduled in the function pass
    manager. Returns 1 if any of the passes modified the module, 0 otherwise.
    @see llvm::FunctionPassManager::doInitialization *)
PROCEDURE LLVMInitializeFunctionPassManager(FPM: PassManagerRef): Bool;

(** Executes all of the function passes scheduled in the function pass manager
    on the provided function. Returns 1 if any of the passes modified the
    function, false otherwise.
    @see llvm::FunctionPassManager::run(Function&) *)
PROCEDURE LLVMRunFunctionPassManager(FPM: PassManagerRef; F: ValueRef): Bool;

(** Finalizes all of the function passes scheduled in in the function pass
    manager. Returns 1 if any of the passes modified the module, 0 otherwise.
    @see llvm::FunctionPassManager::doFinalization *)
PROCEDURE LLVMFinalizeFunctionPassManager(FPM: PassManagerRef): Bool;

(** Frees the memory of a pass pipeline. For function pipelines, does not free
    the module provider.
    @see llvm::PassManagerBase::~PassManagerBase. *)
PROCEDURE LLVMDisposePassManager(PM: PassManagerRef);

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreThreading Threading
 *
 * Handle the structures needed to make LLVM safe for multithreading.
 *
 * @{
 *)

(** Check whether LLVM is executing in thread-safe mode or not.
    @see llvm::llvm_is_multithreaded *)
PROCEDURE LLVMIsMultithreaded() : Bool;


(*===-- llvm-c/BitWriter.h - BitWriter Library C Interface ------*- C++ -*-===*\
|*                                                                            *|
|*                     The LLVM Compiler Infrastructure                       *|
|*                                                                            *|
|* This file is distributed under the University of Illinois Open Source      *|
|* License. See LICENSE.TXT for details.                                      *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This header declares the C interface to libLLVMBitWriter.a, which          *|
|* implements output of the LLVM bitcode format.                              *|
|*                                                                            *|
|* Many exotic languages can interoperate with C code but have a harder time  *|
|* with C++ due to name mangling. So in addition to C, this interface enables *|
|* tools written in such languages.                                           *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*)

(**
 * @defgroup LLVMCBitWriter Bit Writer
 * @ingroup LLVMC
 *
 * @{
 *)

(*===-- Operations on modules ---------------------------------------------===*)

(** Writes a module to the specified path. Returns 0 on success. *)
PROCEDURE LLVMWriteBitcodeToFile(M: ModuleRef; Path: const_char_star): int;

(** Writes a module to an open file descriptor. Returns 0 on success. *)
PROCEDURE LLVMWriteBitcodeToFD(M: ModuleRef; FD, ShouldClose, Unbuffered: int): int;

(** Deprecated for LLVMWriteBitcodeToFD. Writes a module to an open file
    descriptor. Returns 0 on success. Closes the Handle. *)
PROCEDURE LLVMWriteBitcodeToFileHandle(M: ModuleRef; Handle: int): int;

(**
 * @}
 *)

(*===-- llvm-c/Target.h - Target Lib C Iface --------------------*- C++ -*-===*/
/*                                                                            */
/*                     The LLVM Compiler Infrastructure                       */
/*                                                                            */
/* This file is distributed under the University of Illinois Open Source      */
/* License. See LICENSE.TXT for details.                                      */
/*                                                                            */
/*===----------------------------------------------------------------------===*/
/*                                                                            */
/* This header declares the C interface to libLLVMTarget.a, which             */
/* implements target information.                                             */
/*                                                                            */
/* Many exotic languages can interoperate with C code but have a harder time  */
/* with C++ due to name mangling. So in addition to C, this interface enables */
/* tools written in such languages.                                           */
/*                                                                            */
/*===----------------------------------------------------------------------===*)

(**
 * @defgroup LLVMCTarget Target information
 * @ingroup LLVMC
 *
 * @{
 *)

TYPE ByteOrdering = { BigEndian, LittleEndian };

TYPE TargetDataRef = UNTRACED BRANDED "OpaqueTargetData" REF Opaque;
TYPE TargetLibraryInfoRef = UNTRACED BRANDED "OpaqueTargetLibraryInfo" REF Opaque;
TYPE StructLayoutRef = UNTRACED BRANDED "OpaqueStructLayout" REF Opaque;

(*===-- Target Data -------------------------------------------------------===*)

(** Creates target data from a target layout string.
    See the constructor llvm::TargetData::TargetData. *)
PROCEDURE LLVMCreateTargetData(StringRep: const_char_star): TargetDataRef;

(** Adds target data information to a pass manager. This does not take ownership
    of the target data.
    See the method llvm::PassManagerBase::add. *)
PROCEDURE LLVMAddTargetData(T: TargetDataRef; P: PassManagerRef);

(** Adds target library information to a pass manager. This does not take
    ownership of the target library info.
    See the method llvm::PassManagerBase::add. *)
PROCEDURE LLVMAddTargetLibraryInfo(T: TargetLibraryInfoRef; P: PassManagerRef);

(** Converts target data to a target layout string. The string must be disposed
    with LLVMDisposeMessage.
    See the constructor llvm::TargetData::TargetData. *)
PROCEDURE LLVMCopyStringRepOfTargetData(T: TargetDataRef): char_star;

(** Returns the byte order of a target, either LLVMBigEndian or
    LLVMLittleEndian.
    See the method llvm::TargetData::isLittleEndian. *)
PROCEDURE LLVMByteOrder(T: TargetDataRef): ByteOrdering;

(** Returns the pointer size in bytes for a target.
    See the method llvm::TargetData::getPointerSize. *)
PROCEDURE LLVMPointerSize(T: TargetDataRef): unsigned;

(** Returns the integer type that is the same size as a pointer on a target.
    See the method llvm::TargetData::getIntPtrType. *)
PROCEDURE LLVMIntPtrType(T: TargetDataRef): TypeRef;

(** Computes the size of a type in bits for a target.
    See the method llvm::TargetData::getTypeSizeInBits. *)
PROCEDURE LLVMSizeOfTypeInBits(T: TargetDataRef; Type: TypeRef): unsigned_long_long;

(** Computes the storage size of a type in bytes for a target.
    See the method llvm::TargetData::getTypeStoreSize. *)
PROCEDURE LLVMStoreSizeOfType(T: TargetDataRef; Type: TypeRef): unsigned_long_long;

(** Computes the ABI size of a type in bytes for a target.
    See the method llvm::TargetData::getTypeAllocSize. *)
PROCEDURE LLVMABISizeOfType(T: TargetDataRef; Type: TypeRef): unsigned_long_long;

(** Computes the ABI alignment of a type in bytes for a target.
    See the method llvm::TargetData::getTypeABISize. *)
PROCEDURE LLVMABIAlignmentOfType(T: TargetDataRef; Type: TypeRef): unsigned;

(** Computes the call frame alignment of a type in bytes for a target.
    See the method llvm::TargetData::getTypeABISize. *)
PROCEDURE LLVMCallFrameAlignmentOfType(T: TargetDataRef; Type: TypeRef): unsigned;

(** Computes the preferred alignment of a type in bytes for a target.
    See the method llvm::TargetData::getTypeABISize. *)
PROCEDURE LLVMPreferredAlignmentOfType(T: TargetDataRef; Type: TypeRef): unsigned;

(** Computes the preferred alignment of a global variable in bytes for a target.
    See the method llvm::TargetData::getPreferredAlignment. *)
PROCEDURE LLVMPreferredAlignmentOfGlobal(T: TargetDataRef;
                                     GlobalVar: ValueRef): unsigned;

(** Computes the structure element that contains the byte offset for a target.
    See the method llvm::StructLayout::getElementContainingOffset. *)
PROCEDURE LLVMElementAtOffset(T: TargetDataRef; StructTy: TypeRef;
                          Offset: unsigned_long_long): unsigned;

(** Computes the byte offset of the indexed struct element for a target.
    See the method llvm::StructLayout::getElementContainingOffset. *)
PROCEDURE LLVMOffsetOfElement(T: TargetDataRef; StructTy: TypeRef;
                          Element: unsigned): unsigned_long_long;

(** Deallocates a TargetData.
    See the destructor llvm::TargetData::~TargetData. *)
PROCEDURE LLVMDisposeTargetData(T: TargetDataRef);

(*===-- llvm-c/TargetMachine.h - Target Machine Library C Interface - C++ -*-=*\
|*                                                                            *|
|*                     The LLVM Compiler Infrastructure                       *|
|*                                                                            *|
|* This file is distributed under the University of Illinois Open Source      *|
|* License. See LICENSE.TXT for details.                                      *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This header declares the C interface to the Target and TargetMachine       *|
|* classes, which can be used to generate assembly or object files.           *|
|*                                                                            *|
|* Many exotic languages can interoperate with C code but have a harder time  *|
|* with C++ due to name mangling. So in addition to C, this interface enables *|
|* tools written in such languages.                                           *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*)

(** See include/llvm-c/TargetMachine.h **) 
TYPE TargetMachineRef = UNTRACED BRANDED "LLVMTargetMachineRef" REF Opaque;

(** See include/llvm-c/TargetMachine.h **) 
TYPE TargetRef = UNTRACED BRANDED "LLVMTarget" REF Opaque;

TYPE CodeGenOptLevel = {
  LevelNone,
  LevelLess,
  LevelDefault,
  LevelAggressive
};

TYPE RelocMode = {
  Default,
  Static,
  PIC,
  DynamicNoPic
};

TYPE CodeModel = {
  Default,
  JITDefault,
  Small,
  Kernel,
  Medium,
  Large
};

TYPE CodeGenFileType = {
  AssemblyFile,
  ObjectFile
};

(** Returns the first llvm::Target in the registered targets list. *)
PROCEDURE LLVMGetFirstTarget(): TargetRef;
(** Returns the next llvm::Target given a previous one (or null if there's none) *)
PROCEDURE LLVMGetNextTarget(T: TargetRef): TargetRef;

(*===-- Target ------------------------------------------------------------===*)
(** Returns the name of a target. See llvm::Target::getName *)
PROCEDURE LLVMGetTargetName(T: TargetRef): const_char_star;

(** Returns the description  of a target. See llvm::Target::getDescription *)
PROCEDURE LLVMGetTargetDescription(T: TargetRef): const_char_star;

(** Returns if the target has a JIT *)
PROCEDURE LLVMTargetHasJIT(T: TargetRef): Bool;

(** Returns if the target has a TargetMachine associated *)
PROCEDURE LLVMTargetHasTargetMachine(T: TargetRef): Bool;

(** Returns if the target has an ASM backend (required for emitting output) *)
PROCEDURE LLVMTargetHasAsmBackend(T: TargetRef): Bool;

(*===-- Target Machine ----------------------------------------------------===*)
(** Creates a new llvm::TargetMachine. See llvm::Target::createTargetMachine *)
PROCEDURE LLVMCreateTargetMachine(
    T: TargetRef; Triple: char_star; CPU: char_star; Features: char_star;
    Level: CodeGenOptLevel; Reloc: RelocMode;
    CodeModel: CodeModel): TargetMachineRef;

(** Dispose the LLVMTargetMachineRef instance generated by
  LLVMCreateTargetMachine. *)
PROCEDURE LLVMDisposeTargetMachine(T: TargetMachineRef);

(** Returns the Target used in a TargetMachine *)
PROCEDURE LLVMGetTargetMachineTarget(T: TargetMachineRef): TargetRef;

(** Returns the triple used creating this target machine. See
  llvm::TargetMachine::getTriple. The result needs to be disposed with
  LLVMDisposeMessage. *)
PROCEDURE LLVMGetTargetMachineTriple(T: TargetMachineRef): char_star;

(** Returns the cpu used creating this target machine. See
  llvm::TargetMachine::getCPU. The result needs to be disposed with
  LLVMDisposeMessage. *)
PROCEDURE LLVMGetTargetMachineCPU(T: TargetMachineRef): char_star;

(** Returns the feature string used creating this target machine. See
  llvm::TargetMachine::getFeatureString. The result needs to be disposed with
  LLVMDisposeMessage. *)
PROCEDURE LLVMGetTargetMachineFeatureString(T: TargetMachineRef): char_star;

(** Returns the llvm::TargetData used for this llvm:TargetMachine. *)
PROCEDURE LLVMGetTargetMachineData(T: TargetMachineRef): TargetDataRef;

(** Emits an asm or object file for the given module to the filename. This
  wraps several c++ only classes (among them a file stream). Returns any
  error in ErrorMessage. Use LLVMDisposeMessage to dispose the message. *)
PROCEDURE LLVMTargetMachineEmitToFile(
    T: TargetMachineRef; M: ModuleRef; Filename: char_star;
    codegen: CodeGenFileType; VAR ErrorMessage: char_star): Bool;

END LLVM.
