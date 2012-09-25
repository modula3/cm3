INTERFACE LLVM;

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

IMPORT Word;
FROM Ctypes
IMPORT int, unsigned, char_star, const_char_star, char_star_star,
       unsigned_long_long, double, long_long;
FROM Cstdint IMPORT uint8_t, uint64_t;

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

TYPE Bool = BOOLEAN;

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

TYPE Attribute = int;
CONST
  ZExtAttribute            = Word.Shift(1, 0);
  SExtAttribute            = Word.Shift(1, 1);
  NoReturnAttribute        = Word.Shift(1, 2);
  InRegAttribute           = Word.Shift(1, 3);
  StructRetAttribute       = Word.Shift(1, 4);
  NoUnwindAttribute        = Word.Shift(1, 5);
  NoAliasAttribute         = Word.Shift(1, 6);
  ByValAttribute           = Word.Shift(1, 7);
  NestAttribute            = Word.Shift(1, 8);
  ReadNoneAttribute        = Word.Shift(1, 9);
  ReadOnlyAttribute        = Word.Shift(1, 10);
  NoInlineAttribute        = Word.Shift(1, 11);
  AlwaysInlineAttribute    = Word.Shift(1, 12);
  OptimizeForSizeAttribute = Word.Shift(1, 13);
  StackProtectAttribute    = Word.Shift(1, 14);
  StackProtectReqAttribute = Word.Shift(1, 15);
  Alignment                = Word.Shift(31, 16);
  NoCaptureAttribute       = Word.Shift(1, 21);
  NoRedZoneAttribute       = Word.Shift(1, 22);
  NoImplicitFloatAttribute = Word.Shift(1, 23);
  NakedAttribute           = Word.Shift(1, 24);
  InlineHintAttribute      = Word.Shift(1, 25);
  StackAlignment           = Word.Shift(7, 26);
  ReturnsTwice             = Word.Shift(1, 29);
  UWTable                  = Word.Shift(1, 30);
  NonLazyBind              = Word.Shift(1, 31);

  (* FIXME: This attribute is currently not included in the C API as
     a temporary measure until the API/ABI impact to the C API is understood
     and the path forward agreed upon.
     LLVMAddressSafety = Word.Shift(1ULL , 32);
  *)

TYPE Opcode = {
  (* Terminator Instructions *)
  XXXXX0,  		(*  0 *)
  Ret,			(*  1 *)
  Br,			(*  2 *)
  Switch,		(*  3 *)
  IndirectBr,		(*  4 *)
  Invoke,		(*  5 *)
  XXXXX6,		(* removed 6 due to API changes *)
  Unreachable,		(*  7 *)

  (* Standard Binary Operators *)
  Add,			(*  8 *)
  FAdd,			(*  9 *)
  Sub,			(* 10 *)
  FSub,			(* 11 *)
  Mul,			(* 12 *)
  FMul,			(* 13 *)
  UDiv,			(* 14 *)
  SDiv,			(* 15 *)
  FDiv,			(* 16 *)
  URem,			(* 17 *)
  SRem,			(* 18 *)
  FRem,			(* 19 *)

  (* Logical Operators *)
  Shl,			(* 20 *)
  LShr,			(* 21 *)
  AShr,			(* 22 *)
  And,			(* 23 *)
  Or,			(* 24 *)
  Xor,			(* 25 *)

  (* Memory Operators *)
  Alloca,		(* 26 *)
  Load,			(* 27 *)
  Store,		(* 28 *)
  GetElementPtr,	(* 29 *)

  (* Cast Operators *)
  Trunc,		(* 30 *)
  ZExt,			(* 31 *)
  SExt,			(* 32 *)
  FPToUI,		(* 33 *)
  FPToSI,		(* 34 *)
  UIToFP,		(* 35 *)
  SIToFP,		(* 36 *)
  FPTrunc,		(* 37 *)
  FPExt,		(* 38 *)
  PtrToInt,		(* 39 *)
  IntToPtr,		(* 40 *)
  BitCast,		(* 41 *)

  (* Other Operators *)
  ICmp,			(* 42 *)
  FCmp,			(* 43 *)
  PHI,			(* 44 *)
  Call,			(* 45 *)
  Select,		(* 46 *)
  UserOp1,		(* 47 *)
  UserOp2,		(* 48 *)
  VAArg,		(* 49 *)
  ExtractElement,	(* 50 *)
  InsertElement,	(* 51 *)
  ShuffleVector,	(* 52 *)
  ExtractValue,		(* 53 *)
  InsertValue,		(* 54 *)

  (* Atomic operators *)
  Fence,		(* 55 *)
  AtomicCmpXchg,	(* 56 *)
  AtomicRMW,		(* 57 *)

  (* Exception Handling Operators *)
  Resume,		(* 58 *)
  LandingPad		(* 59 *)
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
  X86_MMX      (**< X86 MMX *)
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

CONST
  CCallConv           = 0;
  FastCallConv        = 8;
  ColdCallConv        = 9;
  X86StdcallCallConv  = 64;
  X86FastcallCallConv = 65;

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

(**
 * @}
 *)

PROCEDURE InitializeCore(R: PassRegistryRef);


(*===-- Error handling ----------------------------------------------------===*)

PROCEDURE DisposeMessage(Message: char_star);


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
PROCEDURE ContextCreate(): ContextRef;

(**
 * Obtain the global context instance.
 *)
PROCEDURE GetGlobalContext(): ContextRef;

(**
 * Destroy a context instance.
 *
 * This should be called for every call to LLVMContextCreate() or memory
 * will be leaked.
 *)
PROCEDURE ContextDispose(C: ContextRef);

PROCEDURE GetMDKindIDInContext(
    C: ContextRef; Name: const_char_star; SLen: unsigned): unsigned;
PROCEDURE GetMDKindID(Name: const_char_star; SLen: unsigned): unsigned;

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
PROCEDURE ModuleCreateWithName(ModuleID: const_char_star): ModuleRef;

(**
 * Create a new, empty module in a specific context.
 *
 * Every invocation should be paired with LLVMDisposeModule() or memory
 * will be leaked.
 *)
PROCEDURE ModuleCreateWithNameInContext(
    ModuleID: const_char_star; C: ContextRef): ModuleRef;

(**
 * Destroy a module instance.
 *
 * This must be called for every created module or memory will be
 * leaked.
 *)
PROCEDURE DisposeModule(M: ModuleRef);

(**
 * Obtain the data layout for a module.
 *
 * @see Module::getDataLayout()
 *)
PROCEDURE GetDataLayout(M: ModuleRef): const_char_star;

(**
 * Set the data layout for a module.
 *
 * @see Module::setDataLayout()
 *)
PROCEDURE SetDataLayout(M: ModuleRef; Triple: const_char_star);

(**
 * Obtain the target triple for a module.
 *
 * @see Module::getTargetTriple()
 *)
PROCEDURE GetTarget(M: ModuleRef): const_char_star;

(**
 * Set the target triple for a module.
 *
 * @see Module::setTargetTriple()
 *)
PROCEDURE SetTarget(M: ModuleRef; Triple: const_char_star);

(**
 * Dump a representation of a module to stderr.
 *
 * @see Module::dump()
 *)
PROCEDURE DumpModule(M: ModuleRef);

(**
 * Print a representation of a module to a file. The ErrorMessage needs to be
 * disposed with LLVMDisposeMessage. Returns 0 on success, 1 otherwise.
 *
 * @see Module::print()
 *)
PROCEDURE PrintModuleToFile(M: ModuleRef; Filename: const_char_star;
    ErrorMessage: char_star_star): Bool;

(**
 * Set inline assembly for a module.
 *
 * @see Module::setModuleInlineAsm()
 *)
PROCEDURE SetModuleInlineAsm(M: ModuleRef; Asm: const_char_star);

(**
 * Obtain the context to which this module is associated.
 *
 * @see Module::getContext()
 *)
PROCEDURE GetModuleContext(M: ModuleRef): ContextRef;

(**
 * Obtain a Type from a module by its registered name.
 *)
PROCEDURE GetTypeByName(M: ModuleRef; Name: const_char_star): TypeRef;

(**
 * Obtain the number of operands for named metadata in a module.
 *
 * @see llvm::Module::getNamedMetadata()
 *)
PROCEDURE GetNamedMetadataNumOperands(
    M: ModuleRef; name: const_char_star): unsigned;

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
PROCEDURE GetNamedMetadataOperands(
    M: ModuleRef; name: const_char_star; Dest: UNTRACED REF ValueRef);

(**
 * Add an operand to named metadata.
 *
 * @see llvm::Module::getNamedMetadata()
 * @see llvm::MDNode::addOperand()
 *)
PROCEDURE AddNamedMetadataOperand(M: ModuleRef; name: const_char_star;
                                  Val: ValueRef);

(**
 * Add a function to a module under a specified name.
 *
 * @see llvm::Function::Create()
 *)
PROCEDURE AddFunction(M: ModuleRef; Name: const_char_star;
                      FunctionTy: TypeRef): ValueRef;

(**
 * Obtain a Function value from a Module by its name.
 *
 * The returned value corresponds to a llvm::Function value.
 *
 * @see llvm::Module::getFunction()
 *)
PROCEDURE GetNamedFunction(M: ModuleRef; Name: const_char_star): ValueRef;

(**
 * Obtain an iterator to the first Function in a Module.
 *
 * @see llvm::Module::begin()
 *)
PROCEDURE GetFirstFunction(M: ModuleRef): ValueRef;

(**
 * Obtain an iterator to the last Function in a Module.
 *
 * @see llvm::Module::end()
 *)
PROCEDURE GetLastFunction(M: ModuleRef): ValueRef;

(**
 * Advance a Function iterator to the next Function.
 *
 * Returns NULL if the iterator was already at the end and there are no more
 * functions.
 *)
PROCEDURE GetNextFunction(Fn: ValueRef): ValueRef;

(**
 * Decrement a Function iterator to the previous Function.
 *
 * Returns NULL if the iterator was already at the beginning and there are
 * no previous functions.
 *)
PROCEDURE GetPreviousFunction(Fn: ValueRef): ValueRef;

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
PROCEDURE GetTypeKind(Ty: TypeRef): TypeKind;

(**
 * Whether the type has a known size.
 *
 * Things that don't have a size are abstract types, labels, and void.a
 *
 * @see llvm::Type::isSized()
 *)
PROCEDURE TypeIsSized(Ty: TypeRef): Bool;

(**
 * Obtain the context to which this type instance is associated.
 *
 * @see llvm::Type::getContext()
 *)
PROCEDURE GetTypeContext(Ty: TypeRef): ContextRef;

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
PROCEDURE Int1TypeInContext(C: ContextRef): TypeRef;
PROCEDURE Int8TypeInContext(C: ContextRef): TypeRef;
PROCEDURE Int16TypeInContext(C: ContextRef): TypeRef;
PROCEDURE Int32TypeInContext(C: ContextRef): TypeRef;
PROCEDURE Int64TypeInContext(C: ContextRef): TypeRef;
PROCEDURE IntTypeInContext(C: ContextRef; NumBits: unsigned): TypeRef;

(**
 * Obtain an integer type from the global context with a specified bit
 * width.
 *)
PROCEDURE Int1Type(): TypeRef;
PROCEDURE Int8Type(): TypeRef;
PROCEDURE Int16Type(): TypeRef;
PROCEDURE Int32Type(): TypeRef;
PROCEDURE Int64Type(): TypeRef;
PROCEDURE IntType(NumBits: unsigned): TypeRef;
PROCEDURE GetIntTypeWidth(IntegerTy: TypeRef): unsigned;

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
PROCEDURE HalfTypeInContext(C: ContextRef): TypeRef;

(**
 * Obtain a 32-bit floating point type from a context.
 *)
PROCEDURE FloatTypeInContext(C: ContextRef): TypeRef;

(**
 * Obtain a 64-bit floating point type from a context.
 *)
PROCEDURE DoubleTypeInContext(C: ContextRef): TypeRef;

(**
 * Obtain a 80-bit floating point type (X87) from a context.
 *)
PROCEDURE X86FP80TypeInContext(C: ContextRef): TypeRef;

(**
 * Obtain a 128-bit floating point type (112-bit mantissa) from a
 * context.
 *)
PROCEDURE FP128TypeInContext(C: ContextRef): TypeRef;

(**
 * Obtain a 128-bit floating point type (two 64-bits) from a context.
 *)
PROCEDURE PPCFP128TypeInContext(C: ContextRef): TypeRef;

(**
 * Obtain a floating point type from the global context.
 *
 * These map to the functions in this group of the same name.
 *)
PROCEDURE HalfType(): TypeRef;
PROCEDURE FloatType(): TypeRef;
PROCEDURE DoubleType(): TypeRef;
PROCEDURE X86FP80Type(): TypeRef;
PROCEDURE FP128Type(): TypeRef;
PROCEDURE PPCFP128Type(): TypeRef;

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
PROCEDURE FunctionType(ReturnType: TypeRef;
                       ParamTypes: UNTRACED REF TypeRef; ParamCount: unsigned;
                       IsVarArg: Bool): TypeRef;

(**
 * Returns whether a function type is variadic.
 *)
PROCEDURE IsFunctionVarArg(FunctionTy: TypeRef): Bool;

(**
 * Obtain the Type this function Type returns.
 *)
PROCEDURE GetReturnType(FunctionTy: TypeRef): TypeRef;

(**
 * Obtain the number of parameters this function accepts.
 *)
PROCEDURE CountParamTypes(FunctionTy: TypeRef): unsigned;

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
PROCEDURE GetParamTypes(FunctionTy: TypeRef; Dest: UNTRACED REF TypeRef);

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
PROCEDURE StructTypeInContext(C: ContextRef; ElementTypes: UNTRACED REF TypeRef;
                              ElementCount: unsigned; Packed: Bool): TypeRef;

(**
 * Create a new structure type in the global context.
 *
 * @see llvm::StructType::create()
 *)
PROCEDURE StructType(ElementTypes: UNTRACED REF TypeRef; ElementCount: unsigned;
                     Packed: Bool): TypeRef;

(**
 * Create an empty structure in a context having a specified name.
 *
 * @see llvm::StructType::create()
 *)
PROCEDURE StructCreateNamed(C: ContextRef; Name: const_char_star): TypeRef;

(**
 * Obtain the name of a structure.
 *
 * @see llvm::StructType::getName()
 *)
PROCEDURE GetStructName(Ty: TypeRef): const_char_star;

(**
 * Set the contents of a structure type.
 *
 * @see llvm::StructType::setBody()
 *)
PROCEDURE StructSetBody(StructTy: TypeRef; ElementTypes: UNTRACED REF TypeRef;
                        ElementCount: unsigned; Packed: Bool := FALSE);

(**
 * Get the number of elements defined inside the structure.
 *
 * @see llvm::StructType::getNumElements()
 *)
PROCEDURE CountStructElementTypes(StructTy: TypeRef): unsigned;

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
PROCEDURE GetStructElementTypes(StructTy: TypeRef; Dest: UNTRACED REF TypeRef);

(**
 * Determine whether a structure is packed.
 *
 * @see llvm::StructType::isPacked()
 *)
PROCEDURE IsPackedStruct(StructTy: TypeRef): Bool;

(**
 * Determine whether a structure is opaque.
 *
 * @see llvm::StructType::isOpaque()
 *)
PROCEDURE IsOpaqueStruct(StructTy: TypeRef): Bool;

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
PROCEDURE GetElementType(Ty: TypeRef): TypeRef;

(**
 * Create a fixed size array type that refers to a specific type.
 *
 * The created type will exist in the context that its element type
 * exists in.
 *
 * @see llvm::ArrayType::get()
 *)
PROCEDURE ArrayType(ElementType: TypeRef; ElementCount: unsigned): TypeRef;

(**
 * Obtain the length of an array type.
 *
 * This only works on types that represent arrays.
 *
 * @see llvm::ArrayType::getNumElements()
 *)
PROCEDURE GetArrayLength(ArrayTy: TypeRef): unsigned;

(**
 * Create a pointer type that points to a defined type.
 *
 * The created type will exist in the context that its pointee type
 * exists in.
 *
 * @see llvm::PointerType::get()
 *)
PROCEDURE PointerType
  (ElementType: TypeRef; AddressSpace: unsigned := 0): TypeRef;

(**
 * Obtain the address space of a pointer type.
 *
 * This only works on types that represent pointers.
 *
 * @see llvm::PointerType::getAddressSpace()
 *)
PROCEDURE GetPointerAddressSpace(PointerTy: TypeRef): unsigned;

(**
 * Create a vector type that contains a defined type and has a specific
 * number of elements.
 *
 * The created type will exist in the context thats its element type
 * exists in.
 *
 * @see llvm::VectorType::get()
 *)
PROCEDURE VectorType(ElementType: TypeRef; ElementCount: unsigned): TypeRef;

(**
 * Obtain the number of elements in a vector type.
 *
 * This only works on types that represent vectors.
 *
 * @see llvm::VectorType::getNumElements()
 *)
PROCEDURE GetVectorSize(VectorTy: TypeRef): unsigned;

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
PROCEDURE VoidTypeInContext(C: ContextRef): TypeRef;

(**
 * Create a label type in a context.
 *)
PROCEDURE LabelTypeInContext(C: ContextRef): TypeRef;

(**
 * Create a X86 MMX type in a context.
 *)
PROCEDURE X86MMXTypeInContext(C: ContextRef): TypeRef;

(**
 * These are similar to the above functions except they operate on the
 * global context.
 *)
PROCEDURE VoidType(): TypeRef;
PROCEDURE LabelType(): TypeRef;
PROCEDURE X86MMXType(): TypeRef;

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
PROCEDURE TypeOf(Val: ValueRef): TypeRef;

(**
 * Obtain the string name of a value.
 *
 * @see llvm::Value::getName()
 *)
PROCEDURE GetValueName(Val: ValueRef): const_char_star;

(**
 * Set the string name of a value.
 *
 * @see llvm::Value::setName()
 *)
PROCEDURE SetValueName(Val: ValueRef; Name: const_char_star);

(**
 * Dump a representation of a value to stderr.
 *
 * @see llvm::Value::dump()
 *)
PROCEDURE DumpValue(Val: ValueRef);

(**
 * Replace all uses of a value with another one.
 *
 * @see llvm::Value::replaceAllUsesWith()
 *)
PROCEDURE ReplaceAllUsesWith(OldVal: ValueRef; NewVal: ValueRef);

(**
 * Determine whether the specified constant instance is constant.
 *)
PROCEDURE IsConstant(Val: ValueRef): Bool;

(**
 * Determine whether a value instance is undefined.
 *)
PROCEDURE IsUndef(Val: ValueRef): Bool;

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
PROCEDURE GetFirstUse(Val: ValueRef): UseRef;

(**
 * Obtain the next use of a value.
 *
 * This effectively advances the iterator. It returns NULL if you are on
 * the final use and no more are available.
 *)
PROCEDURE GetNextUse(U: UseRef): UseRef;

(**
 * Obtain the user value for a use.
 *
 * The returned value corresponds to a llvm::User type.
 *
 * @see llvm::Use::getUser()
 *)
PROCEDURE GetUser(U: UseRef): ValueRef;

(**
 * Obtain the value this use corresponds to.
 *
 * @see llvm::Use::get().
 *)
PROCEDURE GetUsedValue(U: UseRef): ValueRef;

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
PROCEDURE GetOperand(Val: ValueRef; Index: unsigned): ValueRef;

(**
 * Set an operand at a specific index in a llvm::User value.
 *
 * @see llvm::User::setOperand()
 *)
PROCEDURE SetOperand(User: ValueRef; Index: unsigned; Val: ValueRef);

(**
 * Obtain the number of operands in a llvm::User value.
 *
 * @see llvm::User::getNumOperands()
 *)
PROCEDURE GetNumOperands(Val: ValueRef): int;

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
PROCEDURE ConstNull(Ty: TypeRef): ValueRef; (* all zeroes *)

(**
 * Obtain a constant value referring to the instance of a type
 * consisting of all ones.
 *
 * This is only valid for integer types.
 *
 * @see llvm::Constant::getAllOnesValue()
 *)
PROCEDURE ConstAllOnes(Ty: TypeRef): ValueRef;

(**
 * Obtain a constant value referring to an undefined value of a type.
 *
 * @see llvm::UndefValue::get()
 *)
PROCEDURE GetUndef(Ty: TypeRef): ValueRef;

(**
 * Determine whether a value instance is null.
 *
 * @see llvm::Constant::isNullValue()
 *)
PROCEDURE IsNull(Val: ValueRef): Bool;

(**
 * Obtain a constant that is a constant pointer pointing to NULL for a
 * specified type.
 *)
PROCEDURE ConstPointerNull(Ty: TypeRef): ValueRef;

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
PROCEDURE ConstInt(IntTy: TypeRef; N: unsigned_long_long;
                   SignExtend: Bool): ValueRef;

(**
 * Obtain a constant value for an integer of arbitrary precision.
 *
 * @see llvm::ConstantInt::get()
 *)
PROCEDURE ConstIntOfArbitraryPrecision(IntTy: TypeRef;
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
PROCEDURE ConstIntOfString(IntTy: TypeRef; Text: const_char_star;
                           Radix: uint8_t): ValueRef;

(**
 * Obtain a constant value for an integer parsed from a string with
 * specified length.
 *
 * @see llvm::ConstantInt::get()
 *)
PROCEDURE ConstIntOfStringAndSize(IntTy: TypeRef; Text: const_char_star;
                                  SLen: unsigned; Radix: uint8_t): ValueRef;

(**
 * Obtain a constant value referring to a double floating point value.
 *)
PROCEDURE ConstReal(RealTy: TypeRef; N: double): ValueRef;

(**
 * Obtain a constant for a floating point value parsed from a string.
 *
 * A similar API, LLVMConstRealOfStringAndSize is also available. It
 * should be used if the input string's length is known.
 *)
PROCEDURE ConstRealOfString(RealTy: TypeRef; Text: const_char_star): ValueRef;

(**
 * Obtain a constant for a floating point value parsed from a string.
 *)
PROCEDURE ConstRealOfStringAndSize(RealTy: TypeRef; Text: const_char_star;
                                   SLen: unsigned): ValueRef;

(**
 * Obtain the zero extended value for an integer constant value.
 *
 * @see llvm::ConstantInt::getZExtValue()
 *)
PROCEDURE ConstIntGetZExtValue(ConstantVal: ValueRef): unsigned_long_long;

(**
 * Obtain the sign extended value for an integer constant value.
 *
 * @see llvm::ConstantInt::getSExtValue()
 *)
PROCEDURE ConstIntGetSExtValue(ConstantVal: ValueRef): long_long;

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
PROCEDURE ConstStringInContext(C: ContextRef; Str: const_char_star;
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
PROCEDURE ConstString(Str: const_char_star; Length: unsigned;
                      DontNullTerminate: Bool): ValueRef;

(**
 * Create an anonymous ConstantStruct with the specified values.
 *
 * @see llvm::ConstantStruct::getAnon()
 *)
PROCEDURE ConstStructInContext(C: ContextRef;
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
PROCEDURE ConstStruct(ConstantVals: UNTRACED REF ValueRef;
                      Count: unsigned; Packed: Bool): ValueRef;

(**
 * Create a ConstantArray from values.
 *
 * @see llvm::ConstantArray::get()
 *)
PROCEDURE ConstArray(ElementTy: TypeRef;
                     ConstantVals: UNTRACED REF ValueRef;
                     Length: unsigned): ValueRef;

(**
 * Create a non-anonymous ConstantStruct from values.
 *
 * @see llvm::ConstantStruct::get()
 *)
PROCEDURE ConstNamedStruct(StructTy: TypeRef;
                           ConstantVals: UNTRACED REF ValueRef;
                           Count: unsigned): ValueRef;

(**
 * Create a ConstantVector from values.
 *
 * @see llvm::ConstantVector::get()
 *)
PROCEDURE ConstVector(ScalarConstantVals: UNTRACED REF ValueRef;
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
PROCEDURE GetConstOpcode(ConstantVal: ValueRef): Opcode;
PROCEDURE AlignOf(Ty: TypeRef): ValueRef;
PROCEDURE SizeOf(Ty: TypeRef): ValueRef;
PROCEDURE ConstNeg(ConstantVal: ValueRef): ValueRef;
PROCEDURE ConstNSWNeg(ConstantVal: ValueRef): ValueRef;
PROCEDURE ConstNUWNeg(ConstantVal: ValueRef): ValueRef;
PROCEDURE ConstFNeg(ConstantVal: ValueRef): ValueRef;
PROCEDURE ConstNot(ConstantVal: ValueRef): ValueRef;
PROCEDURE ConstAdd(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstNSWAdd(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstNUWAdd(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstFAdd(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstSub(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstNSWSub(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstNUWSub(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstFSub(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstMul(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstNSWMul(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstNUWMul(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstFMul(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstUDiv(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstSDiv(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstExactSDiv(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstFDiv(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstURem(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstSRem(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstFRem(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstAnd(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstOr(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstXor(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstICmp(Predicate: IntPredicate;
                    LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstFCmp(Predicate: RealPredicate;
                    LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstShl(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstLShr(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstAShr(LHSConstant, RHSConstant: ValueRef): ValueRef;
PROCEDURE ConstGEP(ConstantVal: ValueRef;
                   ConstantIndices: UNTRACED REF ValueRef;
                   NumIndices: unsigned): ValueRef;
PROCEDURE ConstInBoundsGEP(ConstantVal: ValueRef;
                           ConstantIndices: UNTRACED REF ValueRef;
                           NumIndices: unsigned): ValueRef;
PROCEDURE ConstTrunc(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE ConstSExt(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE ConstZExt(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE ConstFPTrunc(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE ConstFPExt(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE ConstUIToFP(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE ConstSIToFP(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE ConstFPToUI(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE ConstFPToSI(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE ConstPtrToInt(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE ConstIntToPtr(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE ConstBitCast(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE ConstZExtOrBitCast(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE ConstSExtOrBitCast(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE ConstTruncOrBitCast(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE ConstPointerCast(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE ConstIntCast(ConstantVal: ValueRef; ToType: TypeRef;
                       isSigned: Bool): ValueRef;
PROCEDURE ConstFPCast(ConstantVal: ValueRef; ToType: TypeRef): ValueRef;
PROCEDURE ConstSelect(ConstantCondition: ValueRef;
                      ConstantIfTrue: ValueRef;
                      ConstantIfFalse: ValueRef): ValueRef;
PROCEDURE ConstExtractElement(VectorConstant: ValueRef;
                              IndexConstant: ValueRef): ValueRef;
PROCEDURE ConstInsertElement(VectorConstant: ValueRef;
                             ElementValueConstant: ValueRef;
                             IndexConstant: ValueRef): ValueRef;
PROCEDURE ConstShuffleVector(VectorAconstant, VectorBConstant: ValueRef;
                             MaskConstant: ValueRef): ValueRef;
PROCEDURE ConstExtractValue(AggConstant: ValueRef;
                            IdxList: UNTRACED REF unsigned;
                            NumIdx: unsigned): ValueRef;
PROCEDURE ConstInsertValue(AggConstant: ValueRef;
                           ElementValueConstant: ValueRef;
                           IdxList: UNTRACED REF unsigned;
                           NumIdx: unsigned): ValueRef;
PROCEDURE ConstInlineAsm(Ty: TypeRef;
                         AsmString, Constraints: const_char_star;
                         HasSideEffects, IsAlignStack: Bool): ValueRef;
PROCEDURE BlockAddress(F: ValueRef; BB: BasicBlockRef): ValueRef;

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

PROCEDURE GetGlobalParent(Global: ValueRef): ModuleRef;
PROCEDURE IsDeclaration(Global: ValueRef): Bool;
PROCEDURE GetLinkage(Global: ValueRef): Linkage;
PROCEDURE SetLinkage(Global: ValueRef; Linkage: Linkage);
PROCEDURE GetSection(Global: ValueRef): const_char_star;
PROCEDURE SetSection(Global: ValueRef; Section: const_char_star);
PROCEDURE GetVisibility(Global: ValueRef): Visibility;
PROCEDURE SetVisibility(Global: ValueRef; Viz: Visibility);
PROCEDURE GetAlignment(Global: ValueRef): unsigned;
PROCEDURE SetAlignment(Global: ValueRef; Bytes: unsigned);

(**
 * @defgroup LLVMCoreValueConstantGlobalVariable Global Variables
 *
 * This group contains functions that operate on global variable values.
 *
 * @see llvm::GlobalVariable
 *
 * @{
 *)
PROCEDURE AddGlobal(M: ModuleRef; Ty: TypeRef; Name: const_char_star): ValueRef;
PROCEDURE AddGlobalInAddressSpace(M: ModuleRef; Ty: TypeRef;
                                  Name: const_char_star;
                                  AddressSpace: unsigned): ValueRef;
PROCEDURE GetNamedGlobal(M: ModuleRef; Name: const_char_star): ValueRef;
PROCEDURE GetFirstGlobal(M: ModuleRef): ValueRef;
PROCEDURE GetLastGlobal(M: ModuleRef): ValueRef;
PROCEDURE GetNextGlobal(GlobalVar: ValueRef): ValueRef;
PROCEDURE GetPreviousGlobal(GlobalVar: ValueRef): ValueRef;
PROCEDURE DeleteGlobal(GlobalVar: ValueRef);
PROCEDURE GetInitializer(GlobalVar: ValueRef): ValueRef;
PROCEDURE SetInitializer(GlobalVar: ValueRef; ConstantVal: ValueRef);
PROCEDURE IsThreadLocal(GlobalVar: ValueRef): Bool;
PROCEDURE SetThreadLocal(GlobalVar: ValueRef; IsThreadLocal: Bool);
PROCEDURE IsGlobalConstant(GlobalVar: ValueRef): Bool;
PROCEDURE SetGlobalConstant(GlobalVar: ValueRef; IsConstant: Bool);

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
PROCEDURE AddAlias(M: ModuleRef; Ty: TypeRef; Aliasee: ValueRef;
                   Name: const_char_star): ValueRef;

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
 * Remove a function from its containing module and deletes it.
 *
 * @see llvm::Function::eraseFromParent()
 *)
PROCEDURE DeleteFunction(Fn: ValueRef);

(**
 * Obtain the ID number from a function instance.
 *
 * @see llvm::Function::getIntrinsicID()
 *)
PROCEDURE GetIntrinsicID(Fn: ValueRef): unsigned;

(**
 * Obtain the calling function of a function.
 *
 * The returned value corresponds to the LLVMCallConv enumeration.
 *
 * @see llvm::Function::getCallingConv()
 *)
PROCEDURE GetFunctionCallConv(Fn: ValueRef): unsigned;

(**
 * Set the calling convention of a function.
 *
 * @see llvm::Function::setCallingConv()
 *
 * @param Fn Function to operate on
 * @param CC LLVMCallConv to set calling convention to
 *)
PROCEDURE SetFunctionCallConv(Fn: ValueRef; CC: unsigned);

(**
 * Obtain the name of the garbage collector to use during code
 * generation.
 *
 * @see llvm::Function::getGC()
 *)
PROCEDURE GetGC(Fn: ValueRef): const_char_star;

(**
 * Define the garbage collector to use during code generation.
 *
 * @see llvm::Function::setGC()
 *)
PROCEDURE SetGC(Fn: ValueRef; Name: const_char_star);

(**
 * Add an attribute to a function.
 *
 * @see llvm::Function::addAttribute()
 *)
PROCEDURE AddFunctionAttr(Fn: ValueRef; PA: Attribute);

(**
 * Obtain an attribute from a function.
 *
 * @see llvm::Function::getAttributes()
 *)
PROCEDURE GetFunctionAttr(Fn: ValueRef): Attribute;

(**
 * Remove an attribute from a function.
 *)
PROCEDURE RemoveFunctionAttr(Fn: ValueRef; PA: Attribute);

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
PROCEDURE CountParams(Fn: ValueRef): unsigned;

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
PROCEDURE GetParams(Fn: ValueRef; Params: UNTRACED REF ValueRef);

(**
 * Obtain the parameter at the specified index.
 *
 * Parameters are indexed from 0.
 *
 * @see llvm::Function::arg_begin()
 *)
PROCEDURE GetParam(Fn: ValueRef; Index: unsigned): ValueRef;

(**
 * Obtain the function to which this argument belongs.
 *
 * Unlike other functions in this group, this one takes a LLVMValueRef
 * that corresponds to a llvm::Attribute.
 *
 * The returned LLVMValueRef is the llvm::Function to which this
 * argument belongs.
 *)
PROCEDURE GetParamParent(Inst: ValueRef): ValueRef;

(**
 * Obtain the first parameter to a function.
 *
 * @see llvm::Function::arg_begin()
 *)
PROCEDURE GetFirstParam(Fn: ValueRef): ValueRef;

(**
 * Obtain the last parameter to a function.
 *
 * @see llvm::Function::arg_end()
 *)
PROCEDURE GetLastParam(Fn: ValueRef): ValueRef;

(**
 * Obtain the next parameter to a function.
 *
 * This takes a LLVMValueRef obtained from LLVMGetFirstParam() (which is
 * actually a wrapped iterator) and obtains the next parameter from the
 * underlying iterator.
 *)
PROCEDURE GetNextParam(Arg: ValueRef): ValueRef;

(**
 * Obtain the previous parameter to a function.
 *
 * This is the opposite of LLVMGetNextParam().
 *)
PROCEDURE GetPreviousParam(Arg: ValueRef): ValueRef;

(**
 * Add an attribute to a function argument.
 *
 * @see llvm::Argument::addAttr()
 *)
PROCEDURE AddAttribute(Arg: ValueRef; PA: Attribute);

(**
 * Remove an attribute from a function argument.
 *
 * @see llvm::Argument::removeAttr()
 *)
PROCEDURE RemoveAttribute(Arg: ValueRef; PA: Attribute);

(**
 * Get an attribute from a function argument.
 *)
PROCEDURE GetAttribute(Arg: ValueRef): Attribute;

(**
 * Set the alignment for a function parameter.
 *
 * @see llvm::Argument::addAttr()
 * @see llvm::Attribute::constructAlignmentFromInt()
 *)
PROCEDURE SetParamAlignment(Arg: ValueRef; align: unsigned);

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
PROCEDURE StringInContext(C: ContextRef; Str: const_char_star;
                          SLen: unsigned): ValueRef;

(**
 * Obtain a MDString value from the global context.
 *)
PROCEDURE MDString(Str: const_char_star; SLen: unsigned): ValueRef;

(**
 * Obtain a MDNode value from a context.
 *
 * The returned value corresponds to the llvm::MDNode class.
 *)
PROCEDURE MDNodeInContext(C: ContextRef; Vals: UNTRACED REF ValueRef;
                          Count: unsigned): ValueRef;

(**
 * Obtain a MDNode value from the global context.
 *)
PROCEDURE MDNode(Vals: UNTRACED REF ValueRef; Count: unsigned): ValueRef;

(**
 * Obtain the underlying string from a MDString value.
 *
 * @param V Instance to obtain string from.
 * @param Len Memory address which will hold length of returned string.
 * @return String data in MDString.
 *)
PROCEDURE GetMDString(V: ValueRef; VAR Len: unsigned): const_char_star;

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
PROCEDURE BasicBlockAsValue(BB: BasicBlockRef): ValueRef;

(**
 * Determine whether a LLVMValueRef is itself a basic block.
 *)
PROCEDURE IsBasicBlock(Val: ValueRef): Bool;

(**
 * Convert a LLVMValueRef to a LLVMBasicBlockRef instance.
 *)
PROCEDURE ValueAsBasicBlock(Val: ValueRef): BasicBlockRef;

(**
 * Obtain the function to which a basic block belongs.
 *
 * @see llvm::BasicBlock::getParent()
 *)
PROCEDURE GetBasicBlockParent(BB: BasicBlockRef): ValueRef;

(**
 * Obtain the terminator instruction for a basic block.
 *
 * If the basic block does not have a terminator (it is not well-formed
 * if it doesn't), then NULL is returned.
 *
 * The returned LLVMValueRef corresponds to a llvm::TerminatorInst.
 *
 * @see llvm::BasicBlock::getTerminator()
 *)
PROCEDURE GetBasicBlockTerminator(BB: BasicBlockRef): ValueRef;

(**
 * Obtain the number of basic blocks in a function.
 *
 * @param Fn Function value to operate on.
 *)
PROCEDURE CountBasicBlocks(Fn: ValueRef): unsigned;

(**
 * Obtain all of the basic blocks in a function.
 *
 * This operates on a function value. The BasicBlocks parameter is a
 * pointer to a pre-allocated array of LLVMBasicBlockRef of at least
 * LLVMCountBasicBlocks() in length. This array is populated with
 * LLVMBasicBlockRef instances.
 *)
PROCEDURE GetBasicBlocks(Fn: ValueRef; BasicBlocks: UNTRACED REF BasicBlockRef);

(**
 * Obtain the first basic block in a function.
 *
 * The returned basic block can be used as an iterator. You will likely
 * eventually call into LLVMGetNextBasicBlock() with it.
 *
 * @see llvm::Function::begin()
 *)
PROCEDURE GetFirstBasicBlock(Fn: ValueRef): BasicBlockRef;

(**
 * Obtain the last basic block in a function.
 *
 * @see llvm::Function::end()
 *)
PROCEDURE GetLastBasicBlock(Fn: ValueRef): BasicBlockRef;

(**
 * Advance a basic block iterator.
 *)
PROCEDURE GetNextBasicBlock(BB: BasicBlockRef): BasicBlockRef;

(**
 * Go backwards in a basic block iterator.
 *)
PROCEDURE GetPreviousBasicBlock(BB: BasicBlockRef): BasicBlockRef;

(**
 * Obtain the basic block that corresponds to the entry point of a
 * function.
 *
 * @see llvm::Function::getEntryBlock()
 *)
PROCEDURE GetEntryBasicBlock(Fn: ValueRef): BasicBlockRef;

(**
 * Append a basic block to the end of a function.
 *
 * @see llvm::BasicBlock::Create()
 *)
PROCEDURE AppendBasicBlockInContext(C: ContextRef;
                                    Fn: ValueRef;
                                    Name: const_char_star): BasicBlockRef;

(**
 * Append a basic block to the end of a function using the global
 * context.
 *
 * @see llvm::BasicBlock::Create()
 *)
PROCEDURE AppendBasicBlock(Fn: ValueRef; Name: const_char_star): BasicBlockRef;

(**
 * Insert a basic block in a function before another basic block.
 *
 * The function to add to is determined by the function of the
 * passed basic block.
 *
 * @see llvm::BasicBlock::Create()
 *)
PROCEDURE InsertBasicBlockInContext(C: ContextRef;
                                    BB: BasicBlockRef;
                                    Name: const_char_star): BasicBlockRef;

(**
 * Insert a basic block in a function using the global context.
 *
 * @see llvm::BasicBlock::Create()
 *)
PROCEDURE InsertBasicBlock(InsertBeforeBB: BasicBlockRef;
                           Name: const_char_star): BasicBlockRef;

(**
 * Remove a basic block from a function and delete it.
 *
 * This deletes the basic block from its containing function and deletes
 * the basic block itself.
 *
 * @see llvm::BasicBlock::eraseFromParent()
 *)
PROCEDURE DeleteBasicBlock(BB: BasicBlockRef);

(**
 * Remove a basic block from a function.
 *
 * This deletes the basic block from its containing function but keep
 * the basic block alive.
 *
 * @see llvm::BasicBlock::removeFromParent()
 *)
PROCEDURE RemoveBasicBlockFromParent(BB: BasicBlockRef);

(**
 * Move a basic block to before another one.
 *
 * @see llvm::BasicBlock::moveBefore()
 *)
PROCEDURE MoveBasicBlockBefore(BB, MovePos: BasicBlockRef);

(**
 * Move a basic block to after another one.
 *
 * @see llvm::BasicBlock::moveAfter()
 *)
PROCEDURE MoveBasicBlockAfter(BB, MovePos: BasicBlockRef);

(**
 * Obtain the first instruction in a basic block.
 *
 * The returned LLVMValueRef corresponds to a llvm::Instruction
 * instance.
 *)
PROCEDURE GetFirstInstruction(BB: BasicBlockRef): ValueRef;

(**
 * Obtain the last instruction in a basic block.
 *
 * The returned LLVMValueRef corresponds to a LLVM:Instruction.
 *)
PROCEDURE GetLastInstruction(BB: BasicBlockRef): ValueRef;

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
PROCEDURE HasMetadata(Val: ValueRef): int;

(**
 * Return metadata associated with an instruction value.
 *)
PROCEDURE GetMetadata(Val: ValueRef; KindID: unsigned): ValueRef;

(**
 * Set metadata associated with an instruction value.
 *)
PROCEDURE SetMetadata(Val: ValueRef; KindID: unsigned; Node: ValueRef);

(**
 * Obtain the basic block to which an instruction belongs.
 *
 * @see llvm::Instruction::getParent()
 *)
PROCEDURE GetInstructionParent(Inst: ValueRef): BasicBlockRef;

(**
 * Obtain the instruction that occurs after the one specified.
 *
 * The next instruction will be from the same basic block.
 *
 * If this is the last instruction in a basic block, NULL will be
 * returned.
 *)
PROCEDURE GetNextInstruction(Inst: ValueRef): ValueRef;

(**
 * Obtain the instruction that occurred before this one.
 *
 * If the instruction is the first instruction in a basic block, NULL
 * will be returned.
 *)
PROCEDURE GetPreviousInstruction(Inst: ValueRef): ValueRef;

(**
 * Remove and delete an instruction.
 *
 * The instruction specified is removed from its containing building
 * block and then deleted.
 *
 * @see llvm::Instruction::eraseFromParent()
 *)
PROCEDURE InstructionEraseFromParent(Inst: ValueRef);

(**
 * Obtain the code opcode for an individual instruction.
 *
 * @see llvm::Instruction::getOpCode()
 *)
PROCEDURE GetInstructionOpcode(Inst: ValueRef): Opcode;

(**
 * Obtain the predicate of an instruction.
 *
 * This is only valid for instructions that correspond to llvm::ICmpInst
 * or llvm::ConstantExpr whose opcode is llvm::Instruction::ICmp.
 *
 * @see llvm::ICmpInst::getPredicate()
 *)
PROCEDURE GetICmpPredicate(Inst: ValueRef): IntPredicate;

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
 * Set the calling convention for a call instruction.
 *
 * This expects an LLVMValueRef that corresponds to a llvm::CallInst or
 * llvm::InvokeInst.
 *
 * @see llvm::CallInst::setCallingConv()
 * @see llvm::InvokeInst::setCallingConv()
 *)
PROCEDURE SetInstructionCallConv(Instr: ValueRef; CC: unsigned);

(**
 * Obtain the calling convention for a call instruction.
 *
 * This is the opposite of LLVMSetInstructionCallConv(). Reads its
 * usage.
 *
 * @see LLVMSetInstructionCallConv()
 *)
PROCEDURE GetInstructionCallConv(Instr: ValueRef): unsigned;


PROCEDURE AddInstrAttribute(Instr: ValueRef; index: unsigned; PA: Attribute);
PROCEDURE RemoveInstrAttribute(Instr: ValueRef; index: unsigned; PA: Attribute);
PROCEDURE SetInstrParamAlignment(Instr: ValueRef; index, align: unsigned);

(**
 * Obtain whether a call instruction is a tail call.
 *
 * This only works on llvm::CallInst instructions.
 *
 * @see llvm::CallInst::isTailCall()
 *)
PROCEDURE IsTailCall(CallInst: ValueRef): Bool;

(**
 * Set whether a call instruction is a tail call.
 *
 * This only works on llvm::CallInst instructions.
 *
 * @see llvm::CallInst::setTailCall()
 *)
PROCEDURE SetTailCall(CallInst: ValueRef; IsTailCall: Bool);

(**
 * @}
 *)

(**
 * Obtain the default destination basic block of a switch instruction.
 *
 * This only works on llvm::SwitchInst instructions.
 *
 * @see llvm::SwitchInst::getDefaultDest()
 *)
PROCEDURE GetSwitchDefaultDest(SwitchInstr: ValueRef): BasicBlockRef;

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
PROCEDURE AddIncoming(PhiNode: ValueRef;
                      IncomingValues: UNTRACED REF ValueRef;
                      IncomingBlocks: UNTRACED REF BasicBlockRef;
                      Count: unsigned);

(**
 * Obtain the number of incoming basic blocks to a PHI node.
 *)
PROCEDURE CountIncoming(PhiNode: ValueRef): unsigned;

(**
 * Obtain an incoming value to a PHI node as a LLVMValueRef.
 *)
PROCEDURE GetIncomingValue(PhiNode: ValueRef; Index: unsigned): ValueRef;

(**
 * Obtain an incoming value to a PHI node as a LLVMBasicBlockRef.
 *)
PROCEDURE GetIncomingBlock(PhiNode: ValueRef; Index: unsigned): BasicBlockRef;

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
 * @defgroup LLVMCCoreInstructionBuilder Instruction Builders
 *
 * An instruction builder represents a point within a basic block and is
 * the exclusive means of building instructions using the C interface.
 *
 * @{
 *)

PROCEDURE CreateBuilderInContext(C: ContextRef): BuilderRef;
PROCEDURE CreateBuilder(): BuilderRef;
PROCEDURE PositionBuilder(Builder: BuilderRef; Block: BasicBlockRef;
                         Instr: ValueRef);
PROCEDURE PositionBuilderBefore(Builder: BuilderRef; Instr: ValueRef);
PROCEDURE PositionBuilderAtEnd(Builder: BuilderRef; Block: BasicBlockRef);
PROCEDURE GetInsertBlock(Builder: BuilderRef): BasicBlockRef;
PROCEDURE ClearInsertionPosition(Builder: BuilderRef);
PROCEDURE InsertIntoBuilder(Builder: BuilderRef; Instr: ValueRef);
PROCEDURE InsertIntoBuilderWithName(Builder: BuilderRef; Instr: ValueRef;
                                    Name: const_char_star);
PROCEDURE DisposeBuilder(Builder: BuilderRef);

(* Metadata *)
PROCEDURE SetCurrentDebugLocation(Builder: BuilderRef; L: ValueRef);
PROCEDURE GetCurrentDebugLocation(Builder: BuilderRef): ValueRef;
PROCEDURE SetInstDebugLocation(Builder: BuilderRef; Inst: ValueRef);


(* Terminators *)
PROCEDURE BuildRetVoid(B: BuilderRef): ValueRef;
PROCEDURE BuildRet(B: BuilderRef; V: ValueRef): ValueRef;
PROCEDURE BuildAggregateRet(B: BuilderRef; RetVals: UNTRACED REF ValueRef;
                            N: unsigned): ValueRef;
PROCEDURE BuildBr(B: BuilderRef; Dest: BasicBlockRef): ValueRef;
PROCEDURE BuildCondBr(B: BuilderRef; If: ValueRef;
                      Then, Else: BasicBlockRef): ValueRef;
PROCEDURE BuildSwitch(B: BuilderRef; V: ValueRef; Else: BasicBlockRef;
                      NumCases: unsigned): ValueRef;
PROCEDURE BuildIndirectBr(B: BuilderRef; Addr: ValueRef;
                          NumDests: unsigned): ValueRef;
PROCEDURE BuildInvoke(B: BuilderRef; Fn: ValueRef;
                      Args: UNTRACED REF ValueRef; NumArgs: unsigned;
                      Then, Catch: BasicBlockRef;
                      Name: const_char_star): ValueRef;
PROCEDURE BuildLandingPad(B: BuilderRef; Ty: TypeRef;
                          PersFn: ValueRef; NumClauses: unsigned;
                          Name: const_char_star): ValueRef;
PROCEDURE BuildResume(B: BuilderRef; Exn: ValueRef): ValueRef;
PROCEDURE BuildUnreachable(B: BuilderRef): ValueRef;

(* Add a case to the switch instruction *)
PROCEDURE AddCase(Switch, OnVal: ValueRef; Dest: BasicBlockRef);

(* Add a destination to the indirectbr instruction *)
PROCEDURE AddDestination(IndirectBr: ValueRef; Dest: BasicBlockRef);

(* Add a catch or filter clause to the landingpad instruction *)
PROCEDURE AddClause(LandingPad, ClauseVal: ValueRef);

(* Set the 'cleanup' flag in the landingpad instruction *)
PROCEDURE SetCleanup(LandingPad: ValueRef; Val: Bool);

(* Arithmetic *)
PROCEDURE BuildAdd(B: BuilderRef; LHS, RHS: ValueRef;
                   Name: const_char_star): ValueRef;
PROCEDURE BuildNSWAdd(B: BuilderRef; LHS, RHS: ValueRef;
                      Name: const_char_star): ValueRef;
PROCEDURE BuildNUWAdd(B: BuilderRef; LHS, RHS: ValueRef;
                      Name: const_char_star): ValueRef;
PROCEDURE BuildFAdd(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE BuildSub(B: BuilderRef; LHS, RHS: ValueRef;
                   Name: const_char_star): ValueRef;
PROCEDURE BuildNSWSub(B: BuilderRef; LHS, RHS: ValueRef;
                      Name: const_char_star): ValueRef;
PROCEDURE BuildNUWSub(B: BuilderRef; LHS, RHS: ValueRef;
                      Name: const_char_star): ValueRef;
PROCEDURE BuildFSub(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE BuildMul(B: BuilderRef; LHS, RHS: ValueRef;
                   Name: const_char_star): ValueRef;
PROCEDURE BuildNSWMul(B: BuilderRef; LHS, RHS: ValueRef;
                      Name: const_char_star): ValueRef;
PROCEDURE BuildNUWMul(B: BuilderRef; LHS, RHS: ValueRef;
                      Name: const_char_star): ValueRef;
PROCEDURE BuildFMul(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE BuildUDiv(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE BuildSDiv(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE BuildExactSDiv(B: BuilderRef; LHS, RHS: ValueRef;
                         Name: const_char_star): ValueRef;
PROCEDURE BuildFDiv(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE BuildURem(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE BuildSRem(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE BuildFRem(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE BuildShl(B: BuilderRef; LHS, RHS: ValueRef;
                   Name: const_char_star): ValueRef;
PROCEDURE BuildLShr(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE BuildAShr(B: BuilderRef; LHS, RHS: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE BuildAnd(B: BuilderRef; LHS, RHS: ValueRef;
                   Name: const_char_star): ValueRef;
PROCEDURE BuildOr(B: BuilderRef; LHS, RHS: ValueRef;
                  Name: const_char_star): ValueRef;
PROCEDURE BuildXor(B: BuilderRef; LHS, RHS: ValueRef;
                   Name: const_char_star): ValueRef;
PROCEDURE BuildBinop(B: BuilderRef; Op: Opcode; LHS, RHS: ValueRef;
                   Name: const_char_star): ValueRef;
PROCEDURE BuildNeg(B: BuilderRef; V: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE BuildNSWNeg(B: BuilderRef; V: ValueRef;
                      Name: const_char_star): ValueRef;
PROCEDURE BuildNUWNeg(B: BuilderRef; V: ValueRef;
                      Name: const_char_star): ValueRef;
PROCEDURE BuildFNeg(B: BuilderRef; V: ValueRef;
                    Name: const_char_star): ValueRef;
PROCEDURE BuildNot(B: BuilderRef; V: ValueRef;
                   Name: const_char_star): ValueRef;

(* Memory *)
PROCEDURE BuildMalloc(
    B: BuilderRef; Ty: TypeRef; Name: const_char_star): ValueRef;
PROCEDURE BuildArrayMalloc(
    B: BuilderRef; Ty: TypeRef; Val: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE BuildAlloca(
    B: BuilderRef; Ty: TypeRef; Name: const_char_star): ValueRef;
PROCEDURE BuildArrayAlloca(
    B: BuilderRef; Ty: TypeRef; Val: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE BuildFree(
    B: BuilderRef; PointerVal: ValueRef): ValueRef;
PROCEDURE BuildLoad(
    B: BuilderRef; PointerVal: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE BuildStore(B: BuilderRef; Val, Ptr: ValueRef): ValueRef;
PROCEDURE BuildGEP(
    B: BuilderRef; Pointer: ValueRef;
    Indices: UNTRACED REF ValueRef; NumIndices: unsigned;
    Name: const_char_star): ValueRef;
PROCEDURE BuildInBoundsGEP(
    B: BuilderRef; Pointer: ValueRef;
    Indices: UNTRACED REF ValueRef; NumIndices: unsigned;
    Name: const_char_star): ValueRef;
PROCEDURE BuildStructGEP(
    B: BuilderRef; Pointer: ValueRef; Idx: unsigned;
    Name: const_char_star): ValueRef;
PROCEDURE BuildGlobalString(
    B: BuilderRef; Str, Name: const_char_star): ValueRef;
PROCEDURE BuildGlobalStringPtr(
    B: BuilderRef; Str, Name: const_char_star): ValueRef;
PROCEDURE GetVolatile(MemoryAccessInst: ValueRef): Bool;
PROCEDURE SetVolatile(MemoryAccessInst: ValueRef; IsVolatile: Bool);

(* Casts *)
PROCEDURE BuildTrunc(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildZExt(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildSExt(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildFPToUI(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildFPToSI(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildUIToFP(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildSIToFP(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildFPTrunc(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildFPExt(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildPtrToInt(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildIntToPtr(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildBitCast(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildZExtOrBitCast(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildSExtOrBitCast(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildTruncOrBitCast(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildCast(
    B: BuilderRef; Op: Opcode; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildPointerCast(
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildIntCast( (* Signed cast! *)
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildFPCast( (* Signed cast! *)
    B: BuilderRef; Val: ValueRef; DestTy: TypeRef;
    Name: const_char_star): ValueRef;

(* Comparisons *)
PROCEDURE BuildICmp(
    B: BuilderRef; Op: IntPredicate; LHS, RHS: ValueRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildFCmp(
    B: BuilderRef; Op: RealPredicate; LHS, RHS: ValueRef;
    Name: const_char_star): ValueRef;

(* Miscellaneous instructions *)
PROCEDURE BuildPhi(
    B: BuilderRef; Ty: TypeRef; Name: const_char_star): ValueRef;
PROCEDURE BuildCall(
    B: BuilderRef; Fn: ValueRef;
    Args: UNTRACED REF ValueRef; NumArgs: unsigned;
    Name: const_char_star): ValueRef;
PROCEDURE BuildSelect(
    B: BuilderRef; If, Then, Else: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE BuildVAArg(
    B: BuilderRef; List: ValueRef; Ty: TypeRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildExtractElement(
    B: BuilderRef; VecVal, Index: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE BuildInsertElement(
    B: BuilderRef; VecVal, EltVal, Index: ValueRef;
    Name: const_char_star): ValueRef;
PROCEDURE BuildShuffleVector(
    B: BuilderRef; V1, V2, Mask: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE BuildExtractValue(
    B: BuilderRef; AggVal: ValueRef; Index: unsigned;
    Name: const_char_star): ValueRef;
PROCEDURE BuildInsertValue(
    B: BuilderRef; AggVal, EltVal: ValueRef; Index: unsigned;
    Name: const_char_star): ValueRef;

PROCEDURE BuildIsNull(
    B: BuilderRef; Val: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE BuildIsNotNull(
    B: BuilderRef; Val: ValueRef; Name: const_char_star): ValueRef;
PROCEDURE BuildPtrDiff(
    B: BuilderRef; LHS, RGS: ValueRef; Name: const_char_star): ValueRef;

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
PROCEDURE CreateModuleProviderForExistingModule(
    M: ModuleRef): ModuleProviderRef;

(**
 * Destroys the module M.
 *)
PROCEDURE DisposeModuleProvider(M: ModuleProviderRef);

(**
 * @}
 *)

(**
 * @defgroup LLVMCCoreMemoryBuffers Memory Buffers
 *
 * @{
 *)

PROCEDURE CreateMemoryBufferWithContentsOfFile(
    Path: const_char_star;
    OutMemBuf: UNTRACED REF MemoryBufferRef;
    OutMessage: char_star_star): Bool;
PROCEDURE CreateMemoryBufferWithSTDIN(
    OutMemBuf: UNTRACED REF MemoryBufferRef;
    OutMessage: char_star_star): Bool;
PROCEDURE DisposeMemoryBuffer(MemBuf: MemoryBufferRef);

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
PROCEDURE GetGlobalPassRegistry(): PassRegistryRef;

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
PROCEDURE CreatePassManager(): PassManagerRef;

(** Constructs a new function-by-function pass pipeline over the module
    provider. It does not take ownership of the module provider. This type of
    pipeline is suitable for code generation and JIT compilation tasks.
    @see llvm::FunctionPassManager::FunctionPassManager *)
PROCEDURE CreateFunctionPassManagerForModule(M: ModuleRef): PassManagerRef;

(** Deprecated: Use LLVMCreateFunctionPassManagerForModule instead. *)
PROCEDURE CreateFunctionPassManager(MP: ModuleProviderRef): PassManagerRef;

(** Initializes, executes on the provided module, and finalizes all of the
    passes scheduled in the pass manager. Returns 1 if any of the passes
    modified the module, 0 otherwise.
    @see llvm::PassManager::run(Module&) *)
PROCEDURE RunPassManager(PM: PassManagerRef; M: ModuleRef): Bool;

(** Initializes all of the function passes scheduled in the function pass
    manager. Returns 1 if any of the passes modified the module, 0 otherwise.
    @see llvm::FunctionPassManager::doInitialization *)
PROCEDURE InitializeFunctionPassManager(FPM: PassManagerRef): Bool;

(** Executes all of the function passes scheduled in the function pass manager
    on the provided function. Returns 1 if any of the passes modified the
    function, false otherwise.
    @see llvm::FunctionPassManager::run(Function&) *)
PROCEDURE RunFunctionPassManager(FPM: PassManagerRef; F: ValueRef): Bool;

(** Finalizes all of the function passes scheduled in in the function pass
    manager. Returns 1 if any of the passes modified the module, 0 otherwise.
    @see llvm::FunctionPassManager::doFinalization *)
PROCEDURE FinalizeFunctionPassManager(FPM: PassManagerRef): Bool;

(** Frees the memory of a pass pipeline. For function pipelines, does not free
    the module provider.
    @see llvm::PassManagerBase::~PassManagerBase. *)
PROCEDURE DisposePassManager(PM: PassManagerRef);

(**
 * @}
 *)

(**
 * @}
 *)

(**
 * @}
 *)

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
PROCEDURE WriteBitcodeToFile(M: ModuleRef; Path: const_char_star): int;

(** Writes a module to an open file descriptor. Returns 0 on success. *)
PROCEDURE WriteBitcodeToFD(M: ModuleRef; FD, ShouldClose, Unbuffered: int): int;

(** Deprecated for LLVMWriteBitcodeToFD. Writes a module to an open file
    descriptor. Returns 0 on success. Closes the Handle. *)
PROCEDURE WriteBitcodeToFileHandle(M: ModuleRef; Handle: int): int;

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
PROCEDURE CreateTargetData(StringRep: const_char_star): TargetDataRef;

(** Adds target data information to a pass manager. This does not take ownership
    of the target data.
    See the method llvm::PassManagerBase::add. *)
PROCEDURE AddTargetData(T: TargetDataRef; P: PassManagerRef);

(** Adds target library information to a pass manager. This does not take
    ownership of the target library info.
    See the method llvm::PassManagerBase::add. *)
PROCEDURE AddTargetLibraryInfo(T: TargetLibraryInfoRef; P: PassManagerRef);

(** Converts target data to a target layout string. The string must be disposed
    with LLVMDisposeMessage.
    See the constructor llvm::TargetData::TargetData. *)
PROCEDURE CopyStringRepOfTargetData(T: TargetDataRef): char_star;

(** Returns the byte order of a target, either LLVMBigEndian or
    LLVMLittleEndian.
    See the method llvm::TargetData::isLittleEndian. *)
PROCEDURE ByteOrder(T: TargetDataRef): ByteOrdering;

(** Returns the pointer size in bytes for a target.
    See the method llvm::TargetData::getPointerSize. *)
PROCEDURE PointerSize(T: TargetDataRef): unsigned;

(** Returns the integer type that is the same size as a pointer on a target.
    See the method llvm::TargetData::getIntPtrType. *)
PROCEDURE IntPtrType(T: TargetDataRef): TypeRef;

(** Computes the size of a type in bytes for a target.
    See the method llvm::TargetData::getTypeSizeInBits. *)
PROCEDURE SizeOfTypeInBits(T: TargetDataRef; Type: TypeRef): unsigned_long_long;

(** Computes the storage size of a type in bytes for a target.
    See the method llvm::TargetData::getTypeStoreSize. *)
PROCEDURE StoreSizeOfType(T: TargetDataRef; Type: TypeRef): unsigned_long_long;

(** Computes the ABI size of a type in bytes for a target.
    See the method llvm::TargetData::getTypeAllocSize. *)
PROCEDURE ABISizeOfType(T: TargetDataRef; Type: TypeRef): unsigned_long_long;

(** Computes the ABI alignment of a type in bytes for a target.
    See the method llvm::TargetData::getTypeABISize. *)
PROCEDURE ABIAlignmentOfType(T: TargetDataRef; Type: TypeRef): unsigned;

(** Computes the call frame alignment of a type in bytes for a target.
    See the method llvm::TargetData::getTypeABISize. *)
PROCEDURE CallFrameAlignmentOfType(T: TargetDataRef; Type: TypeRef): unsigned;

(** Computes the preferred alignment of a type in bytes for a target.
    See the method llvm::TargetData::getTypeABISize. *)
PROCEDURE PreferredAlignmentOfType(T: TargetDataRef; Type: TypeRef): unsigned;

(** Computes the preferred alignment of a global variable in bytes for a target.
    See the method llvm::TargetData::getPreferredAlignment. *)
PROCEDURE PreferredAlignmentOfGlobal(T: TargetDataRef;
                                     GlobalVar: ValueRef): unsigned;

(** Computes the structure element that contains the byte offset for a target.
    See the method llvm::StructLayout::getElementContainingOffset. *)
PROCEDURE ElementAtOffset(T: TargetDataRef; StructTy: TypeRef;
                          Offset: unsigned_long_long): unsigned;

(** Computes the byte offset of the indexed struct element for a target.
    See the method llvm::StructLayout::getElementContainingOffset. *)
PROCEDURE OffsetOfElement(T: TargetDataRef; StructTy: TypeRef;
                          Element: unsigned): unsigned_long_long;

(** Deallocates a TargetData.
    See the destructor llvm::TargetData::~TargetData. *)
PROCEDURE DisposeTargetData(T: TargetDataRef);

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

TYPE TargetMachineRef = UNTRACED BRANDED "LLVMTargetMachine" REF Opaque;
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
PROCEDURE GetFirstTarget(): TargetRef;
(** Returns the next llvm::Target given a previous one (or null if there's none) *)
PROCEDURE GetNextTarget(T: TargetRef): TargetRef;

(*===-- Target ------------------------------------------------------------===*)
(** Returns the name of a target. See llvm::Target::getName *)
PROCEDURE GetTargetName(T: TargetRef): const_char_star;

(** Returns the description  of a target. See llvm::Target::getDescription *)
PROCEDURE GetTargetDescription(T: TargetRef): const_char_star;

(** Returns if the target has a JIT *)
PROCEDURE TargetHasJIT(T: TargetRef): Bool;

(** Returns if the target has a TargetMachine associated *)
PROCEDURE TargetHasTargetMachine(T: TargetRef): Bool;

(** Returns if the target as an ASM backend (required for emitting output) *)
PROCEDURE TargetHasAsmBackend(T: TargetRef): Bool;

(*===-- Target Machine ----------------------------------------------------===*)
(** Creates a new llvm::TargetMachine. See llvm::Target::createTargetMachine *)
PROCEDURE CreateTargetMachine(
    T: TargetRef; Triple: char_star; CPU: char_star; Features: char_star;
    Level: CodeGenOptLevel; Reloc: RelocMode;
    CodeModel: CodeModel): TargetMachineRef;

(** Dispose the LLVMTargetMachineRef instance generated by
  LLVMCreateTargetMachine. *)
PROCEDURE DisposeTargetMachine(T: TargetMachineRef);

(** Returns the Target used in a TargetMachine *)
PROCEDURE GetTargetMachineTarget(T: TargetMachineRef): TargetRef;

(** Returns the triple used creating this target machine. See
  llvm::TargetMachine::getTriple. The result needs to be disposed with
  LLVMDisposeMessage. *)
PROCEDURE GetTargetMachineTriple(T: TargetMachineRef): char_star;

(** Returns the cpu used creating this target machine. See
  llvm::TargetMachine::getCPU. The result needs to be disposed with
  LLVMDisposeMessage. *)
PROCEDURE FetTargetMachineCPU(T: TargetMachineRef): char_star;

(** Returns the feature string used creating this target machine. See
  llvm::TargetMachine::getFeatureString. The result needs to be disposed with
  LLVMDisposeMessage. *)
PROCEDURE GetTargetMachineFeatureString(T: TargetMachineRef): char_star;

(** Returns the llvm::TargetData used for this llvm:TargetMachine. *)
PROCEDURE GetTargetMachineData(T: TargetMachineRef): TargetDataRef;

(** Emits an asm or object file for the given module to the filename. This
  wraps several c++ only classes (among them a file stream). Returns any
  error in ErrorMessage. Use LLVMDisposeMessage to dispose the message. *)
PROCEDURE TargetMachineEmitToFile(
    T: TargetMachineRef; M: ModuleRef; Filename: char_star;
    codegen: CodeGenFileType; VAR ErrorMessage: char_star): Bool;

END LLVM.
