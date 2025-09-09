%module LLVM

// Bindings for LLVM Core.h BitWriter.h and Target.h version 22.0
// Build bindings for all 3 h files into one LLVM.i3 module
// You will need to have swig installed plus the LLVM developer files 
// that is the headers, which means you have cloned llvm and built it.

%pragma(modula3) unsafe="true";

%{
#include "Core.h"
#include "BitWriter.h"
#include "Target.h"
%}

%insert(m3rawintf) %{
IMPORT Word;
%}

%insert(m3wrapimpl) %{
IMPORT Word;
%}

%insert(m3wrapintf) %{
IMPORT LLVMTypes, Word;

TYPE
  int64_t = LLVMTypes.int64_t;  
  uint64_t = LLVMTypes.uint64_t;  
  int32_t = LLVMTypes.int32_t;  
  uint32_t = LLVMTypes.uint32_t;
  uint8_t = LLVMTypes.uint8_t;

TYPE 
  Opaque = RECORD END;

MetadataRef = LLVMTypes.MetadataRef;
PassRegistryRef = UNTRACED BRANDED "LLVMOpaquePassRegistry" REF Opaque;
ContextRef = UNTRACED BRANDED "LLVMOpaqueContext" REF Opaque;
DiagnosticInfoRef = UNTRACED BRANDED "LLVMOpaqueDiagnosticInfo" REF Opaque;
AttributeRef = UNTRACED BRANDED "LLVMOpaqueAttribute" REF Opaque;
ModuleRef = UNTRACED BRANDED "LLVMOpaqueModule" REF Opaque;
TypeRef = UNTRACED BRANDED "LLVMOpaqueType" REF Opaque;
ValueRef = UNTRACED BRANDED "LLVMOpaqueValue" REF Opaque;
NamedMDNodeRef = UNTRACED BRANDED "LLVMOpaqueNamedMDNode" REF Opaque;
TypeKindRef = UNTRACED BRANDED "LLVMOpaqueTypeKind" REF Opaque;
UseRef = UNTRACED BRANDED "LLVMOpaqueUse" REF Opaque;
BasicBlockRef = UNTRACED BRANDED "LLVMOpaqueBasicBlock" REF Opaque;
ValueMetadataEntryRef = UNTRACED BRANDED "LLVMOpaqueValueMetadataEntry" REF Opaque;
BuilderRef = UNTRACED BRANDED "LLVMOpaqueBuilder" REF Opaque;
ModuleProviderRef = UNTRACED BRANDED "LLVMOpaqueModuleProvider" REF Opaque;
MemoryBufferRef = UNTRACED BRANDED "LLVMOpaqueMemoryBuffer" REF Opaque;
PassManagerRef = UNTRACED BRANDED "LLVMOpaquePassManager" REF Opaque;
ModuleFlagEntry = UNTRACED BRANDED "LLVMOpaqueModuleFlagEntry" REF Opaque;

DiagnosticHandler = PROCEDURE(info : DiagnosticInfoRef; parm : ADDRESS);
YieldCallback = PROCEDURE(info : ContextRef; parm : ADDRESS);

DbgRecordRef = UNTRACED BRANDED "LLVMDbgRecordRef" REF Opaque;
OperandBundleRef = UNTRACED BRANDED "LLVMOperandBundleRef" REF Opaque;
TargetDataRef = UNTRACED BRANDED "LLVMTargetDataRef" REF Opaque;
TargetLibraryInfoRef = UNTRACED BRANDED "LLVMTargetLibraryInfoRef" REF Opaque;

TYPE
  DiagnosticSeverity = LLVMDiagnosticSeverity;
  ModuleFlagBehaviour = LLVMModuleFlagBehavior;
  InLineAsmDialect = LLVMInlineAsmDialect;
  LLStorageClass = LLVMDLLStorageClass;
  Visibility = LLVMVisibility;
  Linkage = LLVMLinkage;
  Opcode = LLVMOpcode;
  ValueKind = LLVMValueKind;
  UnnameAddr = LLVMUnnamedAddr;
  ThreadLocalMode = LLVMThreadLocalMode;
  IntPredicate = LLVMIntPredicate;
  RealPredicate = LLVMRealPredicate;
  AtomicOrdering = LLVMAtomicOrdering;
  AtomicRMWBinOp = LLVMAtomicRMWBinOp;
  TailCallKind = LLVMTailCallKind;

  QuadArr = ARRAY[0..1] OF uint64_t;
  QuadRef = UNTRACED REF QuadArr;

  AttributeIndex = Word.T;
%}

#define LLVM_C_EXTERN_C_BEGIN
#define LLVM_C_EXTERN_C_END
#define LLVM_ATTRIBUTE_C_DEPRECATED(decl, msg)
#define LLVM_C_ABI

//define a java typemap to avoid crash in doxygen generation in tags like
//@see function(param) - not necessary if notranslate is invoked below 
%typemap(jstype) SWIGTYPE ""

//Use this feature to turn off javadoc parsing
%feature ("doxygen:notranslate");

//define a macro to handle enumeration parms and returns.

%define DoEnum(PARM1)

//need to convert the int to an enumeration
%typemap("m3wraprettype") PARM1 %{PARM1%}
%typemap("m3wrapretvar")  PARM1 %{ret : C.int;%}
%typemap("m3wrapretraw")  PARM1 %{ret%}
%typemap("m3wrapretconv") PARM1 %{VAL(ret,PARM1)%}
%typemap("m3wrapargvar")  PARM1 %{$1tmp : C.int;%}
%typemap("m3wrapinconv")  PARM1 %{$1tmp := ORD($1_name);%}
%typemap("m3wrapargraw")  PARM1 %{$1tmp%}

%enddef

//define a macro for most of our opaque conversions

%define DoRefs(PARM1,PARM2)

%typemap("m3rawintype")   PARM1 %{ADDRESS%}
%typemap("m3wrapintype")  PARM1 %{PARM2%}
%typemap("m3rawrettype")  PARM1 %{ADDRESS%}
%typemap("m3wraprettype") PARM1 %{PARM2%}

%enddef

//define a macro for conversions with pointers
%define DoPtrs(PARM1,PARM2)

%typemap("m3rawintype")   PARM1 * %{ADDRESS%}
%typemap("m3wrapintype")  PARM1 * %{UNTRACED REF PARM2%}
%typemap("m3rawrettype")  PARM1 * %{ADDRESS%}
%typemap("m3wraprettype") PARM1 * %{UNTRACED REF PARM2%}
%typemap("m3rawinmode")   PARM1 * %{%}
%typemap("m3wrapinmode")  PARM1 * %{%}

%enddef


// ***  basic typemaps ***
%apply const char * {char *};

//void *
%typemap("m3rawintype")     void *   %{ADDRESS%}
%typemap("m3rawrettype")    void *   %{ADDRESS%}
%typemap("m3wrapintype")    void *   %{ADDRESS%}
%typemap("m3wrapinmode")    void *   %{%}
%typemap("m3wraprettype")   void *   %{ADDRESS%}

//char** missing ret typemaps
//this case is a var of a char *
%typemap(m3rawintype)   char **        %{C.char_star%}
%typemap(m3wrapintype)  char **        %{TEXT%}
%typemap(m3wrapargvar)  char **        %{$1: C.char_star;%}
%typemap(m3wrapinconv)  char **        %{$1 := M3toC.SharedTtoS($1_name);%}
%typemap(m3wrapargraw)  char **        %{$1%}
%typemap(m3wrapfreearg) char **        %{M3toC.FreeSharedS($1_name,$1);%}
%typemap(m3rawinmode)   char **        %{VAR%}
%typemap(m3wrapinmode)  char **        %{VAR%}

// ***unsigned***
%typemap("m3rawintype")     unsigned   %{C.unsigned_int%}
%typemap("m3rawrettype")    unsigned   %{C.unsigned_int%}
%typemap("m3wrapintype")    unsigned   %{uint32_t%}
%typemap("m3wraprettype")   unsigned   %{uint32_t%}

// ***unsigned pointer***
%typemap("m3rawintype")     unsigned *  %{C.unsigned_int%}
%typemap("m3rawrettype")    unsigned *  %{UNTRACED REF C.unsigned_int%}
%typemap("m3wrapintype")    unsigned *  %{uint32_t%}
%typemap("m3wraprettype")   unsigned *  %{UNTRACED REF uint32_t%}
//pointers change mode for simple input types
%typemap("m3rawinmode")     unsigned *  %{VAR%}
%typemap("m3wrapinmode")    unsigned *  %{VAR%}

// ***size_t***
%typemap("m3rawintype")     size_t     %{Word.T%}
%typemap("m3wrapintype")    size_t     %{Word.T%}
%typemap("m3rawrettype")    size_t     %{Word.T%}
%typemap("m3wraprettype")   size_t     %{Word.T%}

// ***size_t pointer***
%typemap("m3rawintype")     size_t *    %{Word.T%}
%typemap("m3wrapintype")    size_t *    %{Word.T%}
//these not used
//%typemap("m3rawrettype")    size_t *    %{UNTRACED REF Word.T%}
//%typemap("m3wraprettype")   size_t *    %{UNTRACED REF Word.T%}

//pointers change mode for simple input types
%typemap("m3rawinmode")     size_t *    %{VAR%}
%typemap("m3wrapinmode")    size_t *    %{VAR%}

// ***uint32_t***
%typemap("m3rawintype")     uint32_t   %{C.unsigned_int%}
%typemap("m3rawrettype")    uint32_t   %{C.unsigned_int%}
%typemap("m3wrapintype")    uint32_t   %{uint32_t%}
%typemap("m3wraprettype")   uint32_t   %{uint32_t%}

// ***int32_t***
%typemap("m3rawintype")     int32_t    %{C.int%}
%typemap("m3rawrettype")    int32_t    %{C.int%}
%typemap("m3wrapintype")    int32_t    %{int32_t%}
%typemap("m3wraprettype")   int32_t    %{int32_t%}

%typemap(in) int32_t  %{ $1 = ($1_ltype)$input; %}
%typemap(in) uint32_t %{ $1 = ($1_ltype)$input; %}

// ***int8_t***
%typemap("m3rawintype")     int8_t    %{C.char%}
%typemap("m3rawrettype")    int8_t    %{C.char%}
%typemap("m3wrapintype")    int8_t    %{int8_t%}
%typemap("m3wraprettype")   int8_t    %{int8_t%}

// ***uint8_t***
%typemap("m3rawintype")     uint8_t   %{C.unsigned_char%}
%typemap("m3rawrettype")    uint8_t   %{C.unsigned_char%}
%typemap("m3wrapintype")    uint8_t   %{uint8_t%}
%typemap("m3wraprettype")   uint8_t   %{uint8_t%}


// ***int64_t***
%typemap("m3rawintype")     int64_t    %{C.long_long%}
%typemap("m3rawrettype")    int64_t    %{C.long_long%}
%typemap("m3wrapintype")    int64_t    %{int64_t%}
%typemap("m3wraprettype")   int64_t    %{int64_t%}

// ***uint64_t***
%typemap("m3rawintype")     uint64_t   %{C.unsigned_long_long%}
%typemap("m3rawrettype")    uint64_t   %{C.unsigned_long_long%}
%typemap("m3wrapintype")    uint64_t   %{uint64_t%}
%typemap("m3wraprettype")   uint64_t   %{uint64_t%}


%apply uint64_t {unsigned long long}
%apply int64_t {long long}

%typemap(in) int64_t  %{ $1 = ($1_ltype)$input; %}
%typemap(in) uint64_t %{ $1 = ($1_ltype)$input; %}

// ***const uint64_t[]*** missing ret typemaps
%typemap("m3rawintype")   const  uint64_t[]   %{REF ARRAY OF C.unsigned_long_long%}
%typemap("m3wrapintype")  const  uint64_t[]   %{REF ARRAY OF uint64_t%}
%typemap("m3rawinmode")   const  uint64_t[]   %{%}
%typemap("m3wrapinmode")  const  uint64_t[]   %{%}

//rename parm otherwise the C is treated as the import
%typemap("m3wrapinname") LLVMContextRef C %{context%}
%typemap("m3wrapinname") LLVMValueRef C   %{context%}

//change the type to Word.T to avoid range warning for uint32_t
%typemap("m3rawintype")  LLVMAttributeIndex Idx   %{Word.T%}
%typemap("m3wrapintype") LLVMAttributeIndex Idx   %{AttributeIndex%}

// ***LLVMBool***
%typemap("m3rawintype")   LLVMBool %{BOOLEAN%}
%typemap("m3rawrettype")  LLVMBool %{BOOLEAN%}
%typemap("m3wrapintype")  LLVMBool %{BOOLEAN%}
%typemap("m3wraprettype") LLVMBool %{BOOLEAN%}

//similar
%apply LLVMBool {LLVMBool *}
%typemap("m3rawinmode")   LLVMBool * %{VAR%}
%typemap("m3wrapinmode")  LLVMBool * %{VAR%}

// ***proc types***
%typemap("m3rawintype")   LLVMDiagnosticHandler %{ADDRESS%}
%typemap("m3rawrettype")  LLVMDiagnosticHandler %{ADDRESS%}
%typemap("m3wrapintype")  LLVMDiagnosticHandler %{DiagnosticHandler%}
%typemap("m3wrapinmode")  LLVMDiagnosticHandler %{%}
%typemap("m3wraprettype") LLVMDiagnosticHandler %{DiagnosticHandler%}
%typemap("m3wrapargvar")  LLVMDiagnosticHandler %{$1: ADDRESS;%}
%typemap("m3wrapinconv")  LLVMDiagnosticHandler %{$1 := LOOPHOLE($1_name,ADDRESS);%}
%typemap("m3wrapargraw")  LLVMDiagnosticHandler %{$1%}

%typemap("m3wrapretvar")  LLVMDiagnosticHandler  %{ret: ADDRESS;%}
%typemap("m3wrapretraw")  LLVMDiagnosticHandler  %{ret%}
%typemap("m3wrapretconv") LLVMDiagnosticHandler  %{LOOPHOLE(ret,DiagnosticHandler)%}

%typemap("m3wrapindefault")  unsigned AddressSpace %{0%}

%typemap("m3rawintype")   LLVMYieldCallback %{ADDRESS%}
%typemap("m3rawrettype")  LLVMYieldCallback %{ADDRESS%}
%typemap("m3wrapintype")  LLVMYieldCallback %{YieldCallback%}
%typemap("m3wrapinmode")  LLVMYieldCallback %{%}
%typemap("m3wrapintype")  LLVMYieldCallback %{YieldCallback%}
%typemap("m3wraprettype") LLVMYieldCallback %{YieldCallback%}
%typemap("m3wrapargvar")  LLVMYieldCallback %{$1: ADDRESS;%}
%typemap("m3wrapinconv")  LLVMYieldCallback %{$1 := LOOPHOLE($1_name,ADDRESS);%}
%typemap("m3wrapargraw")  LLVMYieldCallback %{$1%}

// ***quadrec*** - for the function which should be in Core.h 
%typemap("m3rawintype")   const uint64_t qi[2] %{ADDRESS%}
%typemap("m3wrapintype")  const uint64_t qi[2] %{QuadRef%}
%typemap("m3wrapargraw")  const uint64_t qi[2] %{$1%}
%typemap("m3wrapargvar")  const uint64_t qi[2] %{$1: ADDRESS;%}
%typemap("m3wrapinconv")  const uint64_t qi[2] %{$1 := LOOPHOLE($1_name,ADDRESS);%}
%typemap("m3rawinmode")   const uint64_t qi[2] %{%}
%typemap("m3wrapinmode")  const uint64_t qi[2] %{%}

// ***pointers***
DoPtrs(LLVMBasicBlockRef,BasicBlockRef);
DoPtrs(LLVMAttributeRef,AttributeRef);

//check this one is it really a pointer to a pointer
DoPtrs(LLVMValueMetadataEntry,ValueMetadataEntryRef);

DoPtrs(LLVMValueRef,ValueRef);
DoPtrs(LLVMTypeRef,TypeRef);
DoPtrs(LLVMMemoryBufferRef,MemoryBufferRef);
DoPtrs(LLVMModuleFlagEntry,ModuleFlagEntry);
DoPtrs(LLVMMetadataRef,MetadataRef);
DoPtrs(LLVMOperandBundleRef,OperandBundleRef);

// ***normal classes***
DoRefs(LLVMPassRegistryRef,PassRegistryRef);
DoRefs(LLVMContextRef,ContextRef);
DoRefs(LLVMDiagnosticInfoRef,DiagnosticInfoRef);
DoRefs(LLVMAttributeRef,AttributeRef);
DoRefs(LLVMModuleRef,ModuleRef);
DoRefs(LLVMMetadataRef,MetadataRef);
DoRefs(LLVMTypeRef,TypeRef);
DoRefs(LLVMValueRef,ValueRef);
DoRefs(LLVMNamedMDNodeRef,NamedMDNodeRef);
DoRefs(LLVMTypeKind,TypeKindRef);
DoRefs(LLVMUseRef,UseRef);
DoRefs(LLVMBasicBlockRef,BasicBlockRef);
DoRefs(LLVMBuilderRef,BuilderRef);
DoRefs(LLVMModuleProviderRef,ModuleProviderRef);
DoRefs(LLVMMemoryBufferRef,MemoryBufferRef);
DoRefs(LLVMPassManagerRef,PassManagerRef);
DoRefs(LLVMDbgRecordRef,DbgRecordRef);
DoRefs(LLVMOperandBundleRef,OperandBundleRef);

DoRefs(LLVMTargetDataRef,TargetDataRef);
DoRefs(LLVMTargetLibraryInfoRef,TargetLibraryInfoRef);
//these have a pointer attached and we have to remove the var
%typemap("m3rawinmode")   struct LLVMOpaqueTargetData* %{%}
%typemap("m3wrapinmode")  struct LLVMOpaqueTargetData* %{%}
%typemap("m3rawinmode")   struct LLVMOpaqueTargetLibraryInfotData* %{%}
%typemap("m3wrapinmode")  struct LLVMOpaqueTargetLibraryInfotData* %{%}

// ***enumerations***
DoEnum(LLVMDiagnosticSeverity);
DoEnum(LLVMModuleFlagBehavior);
DoEnum(LLVMInlineAsmDialect);
DoEnum(LLVMDLLStorageClass);
DoEnum(LLVMVisibility);
DoEnum(LLVMLinkage);
DoEnum(LLVMOpcode);
DoEnum(LLVMValueKind);
DoEnum(LLVMUnnamedAddr);
DoEnum(LLVMThreadLocalMode);
DoEnum(LLVMIntPredicate);
DoEnum(LLVMRealPredicate);
DoEnum(LLVMAtomicOrdering);
DoEnum(LLVMAtomicRMWBinOp);
DoEnum(LLVMTailCallKind);
DoEnum(LLVMCallConv);

DoEnum(LLVMByteOrdering);
%typemap("m3wraprettype")   enum LLVMByteOrdering   %{uint32_t%}

//rename to remove LLVM prefix from functions and camelcase the name
//this should be enabled
%rename("%(camelcase)s",sourcefmt="%(regex:/LLVM(.*)/\\1/)s",%$isfunction) ""; 

//renames the enum but not its uses
//%rename("%(camelcase)s",sourcefmt="%(regex:/LLVM(.*)/\\1/)s",%$isenum) "";

//renames the enum item
//this should be enabled but it causes some expressions in enums to be not
//calculated correctly
//%rename("%(camelcase)s",sourcefmt="%(regex:/LLVM(.*)/\\1/)s",%$isenumitem) "";


//test rename an enum item
//%rename(OGE) LLVMRealOGE;
//rename an enum item - this case of trying to specialise the
//rename first before the general rename below does not work
//%rename("%(camelcase)s",sourcefmt="%(regex:/LLVMInt(.*)/\\1/)s",%$isenumitem) "";


//removes the Ctypes import from the safe interface
%typemap("m3wrapretvar:import")  char * ""

//These have AS for a parm which conflicts with M3 keyword
//could rename the parm I guess
%ignore LLVMPointerSizeForAS;
%ignore LLVMIntPtrTypeForAS;
%ignore LLVMIntPtrTypeForASInContext;

//these are not in libLLVMTarget for some reason
%ignore LLVMInitializeAllTargetInfos;
%ignore LLVMInitializeAllTargets;
%ignore LLVMInitializeAllTargetMCs;
%ignore LLVMInitializeAllAsmPrinters;
%ignore LLVMInitializeAllAsmParsers;
%ignore LLVMInitializeAllDisassemblers;
%ignore LLVMInitializeNativeTarget;
%ignore LLVMInitializeNativeAsmParser;
%ignore LLVMInitializeNativeAsmPrinter;
%ignore LLVMInitializeNativeDisassembler;

%include <Core.h>
%include <BitWriter.h>
%include <Target.h>

/**
 * these implemented in extras.c need to go in Core.h
 * for the moment to avoid dependency on user needing llvm include files
 * installed they are commented out.
 *
 */
/**
 * Return a value for a 128 bit float represented as 2 64 bit quantities
 * this function is for future reference if we ever get 128 bit float support
 * in the front end and it should be in Core.cpp.
 *
 */
//LLVMValueRef LLVMConstQuad(LLVMContextRef C, const uint64_t qi[2]);
