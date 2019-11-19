%module M3DIBuilder

// Bindings for LLVM DIBuilder version 3.9, 4.0, 5.0, 7.0, 8.0, 9.0

%pragma(modula3) unsafe="true";

/* 5.0 introduced conversion macros in attempt at C binding
but swig was not impressed so redefine it to nothing */
#define DEFINE_ISA_CONVERSION_FUNCTIONS(a,b)

%{
#include "llvm/IR/DIBuilder.h"

#include "llvm/ADT/StringExtras.h"

using namespace llvm;
typedef struct StringRefStruct {char *Data; size_t Length; } 
  const * LLVMStringRef;
typedef struct ArrayInt64Struct {int64_t *Data; size_t Length; } 
  const * LLVMArrayInt64;
typedef struct ArrayUInt64Struct {uint64_t *Data; size_t Length; } 
  const * LLVMArrayUInt64;
/* dont need this
typedef struct ArrayMetadataStruct {Metadata *Data; size_t Length; } 
  const * LLVMArrayMD;
*/
%}

%insert(m3rawintf) %{
%}

%insert(m3wrapimpl) %{
IMPORT WeakRef,M3toC;
%}

%insert(m3wrapintf) %{
IMPORT LLVM,LLVMTypes;

CONST DITypeEmpty : DIType = NIL;

TYPE
  int64_t = LLVMTypes.int64_t;  
  uint64_t = LLVMTypes.uint64_t;  
  int32_t = LLVMTypes.int32_t;  
  uint32_t = LLVMTypes.uint32_t;  
  uint = LLVMTypes.unsigned;  
  
  ModuleRef = LLVM.ModuleRef;
  StringRef = LLVMTypes.StringRef;  
  ArrayInt64 = LLVMTypes.ArrayRefOfint64_t;
  ArrayUInt64 = LLVMTypes.ArrayRefOfuint64_t;
  ArrayMDPtr = LLVMTypes.ArrayRefOfMetadataRef;

  MetadataRef = LLVMTypes.MetadataRef;

  DICompileUnit = MetadataRef;
  DIFile = MetadataRef;
  DIEnumerator = MetadataRef;
  DIBasicType = MetadataRef;
  DIType = MetadataRef;
  DIDerivedType = MetadataRef;
  DIScope = MetadataRef;
  DIConstant = MetadataRef;
  DICompositeType = MetadataRef;
  DIObjCProperty = MetadataRef;
  DINodeArray = MetadataRef;
  MDNode = MetadataRef;
  DITemplateTypeParameter = MetadataRef;
  DITemplateValueParameter = MetadataRef;
  DITypeRefArray = MetadataRef;
  DISubroutineType = MetadataRef;
  DISubrange = MetadataRef;
  DIGlobalVariable = MetadataRef;
  DILocalVariable = MetadataRef;
  DIExpression = MetadataRef;
  DISubprogram = MetadataRef;
  DITemplateParameterArray = MetadataRef;
  DINamespace = MetadataRef;
  DIModule = MetadataRef;
  DILexicalBlockFile = MetadataRef;
  DILexicalBlock = MetadataRef;
  DIImportedEntity = MetadataRef;
  DINode = MetadataRef;
  
  DILocation = MDNode;
  ValueRef = LLVM.ValueRef;
  InstructionRef = LLVMTypes.InstructionRef;
  BasicBlockRef = LLVM.BasicBlockRef;

(*4.0.1*)
  DIMacro = MetadataRef;
  DIMacroFile = MetadataRef;
  DIMacroNodeArray = MetadataRef;
  DIGlobalVariableExpression = MetadataRef;

(*5.0.0*)
  DITypeArray = MetadataRef;

(*7.0.0*)
  DILabel = MetadataRef;
  
(*8.0.0*)
  MDTuple = MetadataRef;

(*9.0.0*)
  DICommonBlock = MetadataRef;
  
%}

//define a java typemap to avoid crash in doxygen generation in tags like
//@see function(param) - not necessary if notranslate is invoked below
%typemap(jstype) SWIGTYPE ""

//Use this feature to turn off javadoc parsing
%feature ("doxygen:notranslate");

//***  basic typemaps ***

//unsigned
%typemap("m3rawintype")     unsigned   %{C.unsigned_int%}
%typemap("m3wrapintype")    unsigned   %{uint%}

//int32_t and uint32_t

%typemap("m3rawintype")     uint32_t   %{C.unsigned_int%}
%typemap("m3rawintype")     int32_t    %{C.int%}
%typemap("m3wrapintype")    int32_t    %{int32_t%}
%typemap("m3wrapintype")    uint32_t   %{uint32_t%}

%typemap(in) int32_t  %{ $1 = ($1_ltype)$input; %}
%typemap(in) uint32_t %{ $1 = ($1_ltype)$input; %}


//int64_t and uint64_t

%typemap("m3rawintype")     uint64_t   %{C.unsigned_long_long%}
%typemap("m3rawintype")     int64_t    %{C.long_long%}
%typemap("m3wrapintype")    int64_t    %{int64_t%}
%typemap("m3wrapintype")    uint64_t   %{uint64_t%}

//these are a mistake and not needed for distinct signed and unsigned int 64 types
//%typemap("m3wrapargvar")   int64_t    %{$1tmp: C.long;%}
//%typemap("m3wrapinconv")   int64_t    %{$1tmp := VAL($1_name,C.long);%}
//%typemap("m3wrapargraw")   int64_t    %{$1tmp%}

%typemap(in) int64_t  %{ $1 = ($1_ltype)$input; %}
%typemap(in) uint64_t %{ $1 = ($1_ltype)$input; %}

//no need to apply int64 to uint64 all typemaps are supplied
//and they are different types
//%apply int64_t {uint64_t};


//array ref int64

//4.0.1
%typemap("m3rawintype")   ArrayRef<int64_t>  %{ADDRESS%}
%typemap("m3wrapintype")  ArrayRef<int64_t>  %{ArrayInt64%}

%typemap("m3wrapinmode")  ArrayRef<int64_t>  %{READONLY%}
%typemap("m3wrapargraw")  ArrayRef<int64_t>  %{ADR($1_name)%}

%typemap("ctype")   ArrayRef<int64_t>        %{LLVMArrayInt64%}
%typemap("in")      ArrayRef<int64_t>        %{$1 = $1_basetype($input->Data,$input->Length);%}

//3.9
%apply ArrayRef<int64_t> {llvm::ArrayRef<int64_t>};


//array ref uint64

//4.0.1
%typemap("m3rawintype")   ArrayRef<uint64_t>  %{ADDRESS%}
%typemap("m3wrapintype")  ArrayRef<uint64_t>  %{ArrayUInt64%}

%typemap("m3wrapinmode")  ArrayRef<uint64_t>  %{READONLY%}
%typemap("m3wrapargraw")  ArrayRef<uint64_t>  %{ADR($1_name)%}

%typemap("ctype")   ArrayRef<uint64_t>        %{LLVMArrayUInt64%}
%typemap("in")      ArrayRef<uint64_t>        %{$1 = $1_basetype($input->Data,$input->Length);%}

//3.9
%apply ArrayRef<uint64_t> {llvm::ArrayRef<uint64_t>};

//array ref metadata

//4.0.1
%typemap("m3rawintype")   ArrayRef<Metadata *> %{ADDRESS%}
%typemap("m3wrapintype")  ArrayRef<Metadata *> %{ArrayMDPtr%}

%typemap("m3wrapinmode")  ArrayRef<Metadata *> %{READONLY%}
%typemap("m3wrapargraw")  ArrayRef<Metadata *> %{ADR($1_name)%}

//Not needed
//%typemap("ctype")   ArrayRef<Metadata *>  %{LLVMArrayMD%}
//%typemap("in")      ArrayRef<Metadata *>  %{$1 = $1_basetype($input->Data,$input->Length);%}

//3.9
%apply ArrayRef<Metadata *> {llvm::ArrayRef<Metadata *>};


//DIBuilder - constructor

//changes the return type of the raw constructor
%typemap("m3rawrettype")   llvm::DIBuilder *llvm::DIBuilder::DIBuilder     %{ADDRESS%}

//changes the return type of the wrapped constructor plus the 
//type of the self parm in wrapped module
%typemap("m3wraprettype")   llvm::DIBuilder *llvm::DIBuilder::DIBuilder     %{DIBuilder%}

//assign the constructor return to our cxx and setup gc
%typemap("m3wrapretcheck")   llvm::DIBuilder *llvm::DIBuilder::DIBuilder     %{
  self.cxxObj := result;
  EVAL WeakRef.FromRef(self,Cleanup_DIBuilder);
%}

//typemap for constructors needing local return var from raw
%typemap("m3wrapretvar")   llvm::DIBuilder *llvm::DIBuilder::DIBuilder %{result : ADDRESS;%}

//typemap for naming the result var in call to raw
//check this since if remove it then default is set in the code somewhere
%typemap("m3wrapretraw") llvm::DIBuilder *llvm::DIBuilder::DIBuilder %{result%}
//is same as this only this is more general
//%typemap("m3wrapretraw")       SWIGTYPE * %{result%}

//return the self var
%typemap("m3wrapretconv") llvm::DIBuilder *llvm::DIBuilder::DIBuilder %{self%}
//is same as this except less general
//%typemap("m3wrapretconv")      SWIGTYPE *  %{self%}


//changes the raw self parm mode in methods not ctors
%typemap("m3rawinmode")   llvm::DIBuilder *self     %{%}
//changes the raw self type in methods not ctors
%typemap("m3rawintype")   llvm::DIBuilder *self     %{ADDRESS%}

//changes the wrapped self parm mode in methods
%typemap("m3wrapinmode")   llvm::DIBuilder *self     %{%}
//changes the wrapped self type in methods
%typemap("m3wrapintype")   llvm::DIBuilder *self     %{DIBuilder%}

//methods need to access the cxx object to call the raw proc
%typemap("m3wrapargvar")  llvm::DIBuilder *self  %{selfAdr: ADDRESS := LOOPHOLE($1_name.cxxObj,ADDRESS);%}
%typemap("m3wrapargraw")  llvm::DIBuilder *self  %{selfAdr%}


//Module

%typemap("m3rawintype")    llvm::Module &    %{ADDRESS%}
%typemap("m3wrapintype")   llvm::Module &    %{ModuleRef%}
//remove VAR parm mode
%typemap("m3wrapinmode")   llvm::Module &    %{%}

//convert Module to Address in call to raw
%typemap("m3wrapargvar")   llvm::Module &     %{$1tmp :=  LOOPHOLE($1_name,ADDRESS);%}
%typemap("m3wrapargraw")   llvm::Module &     %{$1tmp%}


//StringRef

//4.0.1
%typemap("m3rawintype")  StringRef %{ADDRESS%}
%typemap("m3wrapintype") StringRef %{StringRef%}

%typemap("m3wrapinmode") StringRef %{READONLY%}
%typemap("m3wrapargraw") StringRef %{ADR($1_name)%}

//Fix the c interface
%typemap("ctype")        StringRef %{LLVMStringRef%}
%typemap("in")           StringRef %{$1 = $1_basetype($input->Data,$input->Length);%}

//3.9 
%apply StringRef {llvm::StringRef};

//Assorted extras

%typemap("m3rawintype") DITemplateParameterArray %{ADDRESS%}
%typemap("in")  DITemplateParameterArray  %{$1 = $1_name;%}

%typemap("m3rawintype")    llvm::Constant  *   %{ADDRESS%}
%typemap("m3wrapintype")   llvm::Constant  *   %{DIConstant%}

%typemap("m3rawintype")    llvm::Value  *   %{ADDRESS%}
%typemap("m3wrapintype")   llvm::Value  *   %{ValueRef%}

%typemap("m3rawintype")    llvm::BasicBlock  *   %{ADDRESS%}
%typemap("m3wrapintype")   llvm::BasicBlock  *   %{BasicBlockRef%}

%typemap("m3rawintype")     DINodeArray    %{ADDRESS%}
%typemap("m3rawintype")     DITypeRefArray %{ADDRESS%}

%typemap("m3rawintype")     DICompileUnit::DebugEmissionKind   %{C.int%}
%typemap("m3wrapintype")    DICompileUnit::DebugEmissionKind   %{INTEGER%}

//8.0
%typemap("m3rawintype")     DICompileUnit::DebugNameTableKind   %{C.int%}
%typemap("m3wrapintype")    DICompileUnit::DebugNameTableKind   %{INTEGER%}


//4.0.1 - we should generate proper enum values
%typemap("m3rawintype")     DIFile::ChecksumKind   %{C.int%}
%typemap("m3wrapintype")    DIFile::ChecksumKind   %{INTEGER%}

%typemap("m3rawintype")     DINode::DIFlags   %{C.int%}
%typemap("m3wrapintype")    DINode::DIFlags   %{INTEGER%}

%typemap("m3rawintype")     DISubprogram::DISPFlags   %{C.int%}
%typemap("m3wrapintype")    DISubprogram::DISPFlags   %{INTEGER%}

//Fix the c parms creating unnecessary pointer casts
%typemap("in")   DICompileUnit::DebugEmissionKind  %{$1 = $1_name;%}
%typemap("in")   DINode::DIFlags        %{$1 = $1_name;%}
%typemap("in")   DIFile::ChecksumKind   %{$1 = $1_name;%}
%typemap("in")   DITypeArray            %{$1 = $1_name;%}

%typemap("in")   DICompileUnit::DebugNameTableKind  %{$1 = $1_name;%}

%typemap("in")   DISubprogram::DISPFlags %{$1 = $1_name;%}

//5.0.0
//Fix Optional used with DWARFAddressSpace
//Optional is class to typecheck parms
%typemap("m3rawintype")   Optional<unsigned>  %{C.unsigned_int%}
%typemap("m3wrapintype")  Optional<unsigned>  %{uint32_t%}
//whatever is passed in from M3, the value None will be used
//%typemap("in")       Optional<unsigned>        %{$1 = $1_name;%}
%typemap("in")       Optional<unsigned>        %{$1 = None;%}

//7.0.0 

%typemap("m3rawintype")   Optional<DIFile::ChecksumInfo<StringRef>>  %{C.char_star%}
%typemap("m3wrapintype")  Optional<DIFile::ChecksumInfo<StringRef>>  %{TEXT%}

%typemap("m3wrapinmode")  Optional<DIFile::ChecksumInfo<StringRef>>  %{READONLY%}
%typemap("m3wrapargraw")  Optional<DIFile::ChecksumInfo<StringRef>>  %{
M3toC.CopyTtoS($1_name)%}

//Checksum for createFile
%typemap("in")       Optional<DIFile::ChecksumInfo<StringRef>>        %{
  SmallString<32> cs(Checksum);
$1.emplace(llvm::DIFile::CSK_MD5, cs);%}

%typemap("ctype")       Optional<DIFile::ChecksumInfo<StringRef>>        %{
char *%}

//Optional<StringRef> for createFile

%typemap("m3rawintype")  Optional<StringRef> %{ADDRESS%}
%typemap("m3wrapintype") Optional<StringRef> %{StringRef%}

%typemap("m3wrapinmode") Optional<StringRef> %{READONLY%}
%typemap("m3wrapargraw") Optional<StringRef> %{ADR($1_name)%}

//Fix the c interface
%typemap("ctype")        Optional<StringRef> %{LLVMStringRef%}
%typemap("in")           Optional<StringRef> %{$1 = StringRef($input->Data,$input->Length);%}

//%ignore createFile;

//DIType is a set of typemaps for all methods with a class type parameter and is actioned with an apply

%typemap("m3rawintype")    DIType  *   %{ADDRESS%}
%typemap("m3rawinmode")    DIType  *   %{%}
%typemap("m3wrapinmode")   DIType  *   %{%}
%typemap("m3wrapargvar")   DIType  *   %{$1tmp: ADDRESS;%}
%typemap("m3wrapinconv")   DIType  *   %{$1tmp := LOOPHOLE($1_name,ADDRESS);%}
%typemap("m3wrapargraw")   DIType  *   %{$1tmp%}

%apply DIType * {DIFile *};
%apply DIType * {DIScope *};
%apply DIType * {llvm::Constant *};
%apply DIType * {MDNode *};
%apply DIType * {DISubroutineType *};
%apply DIType * {DISubprogram *};
%apply DIType * {DITemplateParameterArray };
%apply DIType * {DIImportedEntity *};
%apply DIType * {DINamespace *};
%apply DIType * {DIModule *};
%apply DIType * {DINode *};
%apply DIType * {DILocalVariable *};
%apply DIType * {DIExpression *};
%apply DIType * {DILocation *};
%apply DIType * {llvm::Value *};
%apply DIType * {llvm::BasicBlock *};
%apply DIType * {DICompositeType *};
%apply DIType * {DICompositeType *&};

//4.0,1
%apply DIType * {DIMacroFile *};

//5.0.0
%apply DIType * {DITypeArray };

//7.0.0
%apply DIType * {DICompileUnit *};
%apply DIType * {DIDerivedType *};
%apply DIType * {DILabel *};

//8.0.0
%apply DIType * {MDTuple *};

//9.0.0
%apply DIType * {DICommonBlock *};
%apply DIType * {DIGlobalVariable *};

%apply DIType * {Metadata *};
%typemap("m3wrapintype") Metadata  *  %{MetadataRef%}

//Cant use it on Instruction since the set is not complete enough see below
//%apply DIType * {llvm::Instruction *};


//define a macro for methods returning an object. This handles
//the type conversion and call to the raw method

%define LLVM(RETTYPE,PROC)

%typemap("m3rawrettype")  RETTYPE PROC %{ADDRESS%}
%typemap("m3wraprettype") RETTYPE PROC %{RETTYPE%}

%typemap("m3wrapretvar")  RETTYPE PROC %{ret: ADDRESS;%}
%typemap("m3wrapretraw")  RETTYPE PROC %{ret%}
%typemap("m3wrapretconv") RETTYPE PROC %{LOOPHOLE(ret,RETTYPE)%}

%enddef

//Call our defined macro on methods returning an object in DIBuilder.h

LLVM(DICompileUnit, *llvm::DIBuilder::createCompileUnit)
LLVM(DIFile, *llvm::DIBuilder::createFile)
LLVM(DIBasicType, *llvm::DIBuilder::createNullPtrType)
LLVM(DIEnumerator, *llvm::DIBuilder::createEnumerator)
LLVM(DIBasicType, *llvm::DIBuilder::createUnspecifiedType)
LLVM(DIBasicType, *llvm::DIBuilder::createBasicType)
LLVM(DIDerivedType, *llvm::DIBuilder::createQualifiedType)
LLVM(DIDerivedType, *llvm::DIBuilder::createPointerType)
LLVM(DIDerivedType, *llvm::DIBuilder::createMemberPointerType)
LLVM(DIDerivedType, *llvm::DIBuilder::createReferenceType)
LLVM(DIDerivedType, *llvm::DIBuilder::createTypedef)
LLVM(DIDerivedType, *llvm::DIBuilder::createFriend)
LLVM(DIDerivedType, *llvm::DIBuilder::createInheritance)
LLVM(DIDerivedType, *llvm::DIBuilder::createMemberType)
LLVM(DIDerivedType, *llvm::DIBuilder::createBitFieldMemberType)
LLVM(DIDerivedType, *llvm::DIBuilder::createStaticMemberType)
LLVM(DIDerivedType, *llvm::DIBuilder::createObjCIVar)
LLVM(DIObjCProperty, *llvm::DIBuilder::createObjCProperty)

LLVM(DICompositeType, *llvm::DIBuilder::createClassType)
LLVM(DICompositeType, *llvm::DIBuilder::createStructType)
LLVM(DICompositeType, *llvm::DIBuilder::createUnionType)
LLVM(DITemplateTypeParameter, *llvm::DIBuilder::createTemplateTypeParameter)
LLVM(DITemplateValueParameter, *llvm::DIBuilder::createTemplateValueParameter)
LLVM(DITemplateValueParameter, *llvm::DIBuilder::createTemplateTemplateParameter)
LLVM(DITemplateValueParameter, *llvm::DIBuilder::createTemplateParameterPack)
LLVM(DICompositeType, *llvm::DIBuilder::createArrayType)
LLVM(DICompositeType, *llvm::DIBuilder::createVectorType)
LLVM(DICompositeType, *llvm::DIBuilder::createEnumerationType)
LLVM(DISubroutineType, *llvm::DIBuilder::createSubroutineType)
LLVM(DICompositeType, *llvm::DIBuilder::createExternalTypeRef)
LLVM(DIType, *llvm::DIBuilder::createArtificialType)
LLVM(DIType, *llvm::DIBuilder::createObjectPointerType)
LLVM(DICompositeType, *llvm::DIBuilder::createForwardDecl)
LLVM(DICompositeType, *llvm::DIBuilder::createReplaceableCompositeType)
LLVM(DIBasicType, *llvm::DIBuilder::createUnspecifiedParameter)
LLVM(DINodeArray, llvm::DIBuilder::getOrCreateArray)
LLVM(DITypeRefArray, llvm::DIBuilder::getOrCreateTypeArray)
LLVM(DISubrange, *llvm::DIBuilder::getOrCreateSubrange)
LLVM(DIGlobalVariable, *llvm::DIBuilder::createGlobalVariable)
LLVM(DIGlobalVariable, *llvm::DIBuilder::createTempGlobalVariableFwdDecl)
LLVM(DILocalVariable, *llvm::DIBuilder::createAutoVariable)
LLVM(DILocalVariable, *llvm::DIBuilder::createParameterVariable)
LLVM(DIExpression, *llvm::DIBuilder::createExpression)
LLVM(DIExpression, *llvm::DIBuilder::createBitPieceExpression)
LLVM(DISubprogram, *llvm::DIBuilder::createFunction)
LLVM(DISubprogram, *llvm::DIBuilder::createTempFunctionFwdDecl)
LLVM(DISubprogram, *llvm::DIBuilder::createMethod)
LLVM(DINamespace, *llvm::DIBuilder::createNameSpace)
LLVM(DIModule, *llvm::DIBuilder::createModule)
LLVM(DILexicalBlockFile, *llvm::DIBuilder::createLexicalBlockFile)
LLVM(DILexicalBlock, *llvm::DIBuilder::createLexicalBlock)
LLVM(DIImportedEntity, *llvm::DIBuilder::createImportedModule)
LLVM(DIImportedEntity, *llvm::DIBuilder::createImportedDeclaration)

//4.0.1
LLVM(DIMacro, *llvm::DIBuilder::createMacro)
LLVM(DIMacroFile, *llvm::DIBuilder::createTempMacroFile)
LLVM(DIMacroNodeArray, llvm::DIBuilder::getOrCreateMacroArray)
LLVM(DIGlobalVariableExpression, *llvm::DIBuilder::createGlobalVariableExpression)
LLVM(DIExpression, *llvm::DIBuilder::createFragmentExpression)
LLVM(DIExpression, *llvm::DIBuilder::createConstantValueExpression)

//7.0.0
LLVM(DIDerivedType, *llvm::DIBuilder::createVariantMemberType)
LLVM(DICompositeType, *llvm::DIBuilder::createVariantPart)
LLVM(DISubprogram, *llvm::DIBuilder::createArtificialSubprogram)
LLVM(DILabel, *llvm::DIBuilder::createLabel)

//9.0.0
LLVM(DICommonBlock, *llvm::DIBuilder::createCommonBlock)

//test create set type
LLVM(DIDerivedType, *llvm::DIBuilder::createSetType)
//test for subrange
LLVM(DIDerivedType, *llvm::DIBuilder::getSubrange)


//not complete enough need full swag of tmaps
//LLVM(llvm::Instruction, *llvm::DIBuilder::insertDeclare)

//Instruction - This is a special case needing more typemaps

%typemap("m3rawintype")  llvm::Instruction  *  %{ADDRESS%}
%typemap("m3wrapintype") llvm::Instruction  *  %{InstructionRef%}
%typemap("m3rawinmode")  llvm::Instruction  *  %{%}
%typemap("m3wrapinmode") llvm::Instruction  *  %{%}
%typemap("m3wrapargvar") llvm::Instruction  *  %{$1tmp: ADDRESS;%}
%typemap("m3wrapinconv") llvm::Instruction  *  %{$1tmp := LOOPHOLE($1_name,ADDRESS);%}
%typemap("m3wrapargraw") llvm::Instruction  *  %{$1tmp%}
%typemap("m3wrapouttype")llvm::Instruction  *  %{InstructionRef%}
%typemap("m3rawrettype") llvm::Instruction  *  %{ADDRESS%}
%typemap("m3wraprettype")llvm::Instruction  *  %{InstructionRef%}

%typemap("m3wrapretvar") llvm::Instruction  *  %{ret: ADDRESS;%}
%typemap("m3wrapretraw") llvm::Instruction  *  %{ret%}
%typemap("m3wrapretconv")llvm::Instruction  *  %{LOOPHOLE(ret,InstructionRef)%}

//DICompositeType has funny *& passing method which we convert to
//pointers

%typemap("m3rawinmode")  DICompositeType *&T     %{VAR%}
%typemap("ctype")    DICompositeType *&T    %{DICompositeType **%}
%typemap("in")       DICompositeType *&T   %{$1 = $1_name;%}

//gcc is ok but clang and cl complain that 
//DINodeArray,DITypeRefArray and DIMacroNodeArray are
//user defined types incompatible with c linkage
%typemap("ctype")    DINodeArray getOrCreateArray   %{MDNode *%}
%typemap("ctype")    DITypeRefArray getOrCreateTypeArray   %{MDNode *%}
%typemap("ctype")    DIMacroNodeArray getOrCreateMacroArray   %{MDNode *%}


//ignore certain overloaded methods with default params
//so we get the signature we want and cut down on multiple methods
//with the same name but different signatures.
//M3 has default parms which could match the c++ method but
//the C interface we go through does not.

/*
Comment out for the moment since llvm version changes render
the signatures too volatile.

//%ignore llvm::DIBuilder::createCompileUnit(unsigned, StringRef, StringRef, StringRef, bool, StringRef, unsigned);

%ignore llvm::DIBuilder::createCompileUnit(unsigned, StringRef, StringRef, StringRef, bool, StringRef, unsigned, StringRef);
%ignore llvm::DIBuilder::createCompileUnit(unsigned, StringRef, StringRef, StringRef, bool, StringRef, unsigned, StringRef, DICompileUnit::DebugEmissionKind);
%ignore llvm::DIBuilder::createCompileUnit(unsigned, StringRef, StringRef, StringRef, bool, StringRef, unsigned, StringRef, DICompileUnit::DebugEmissionKind, uint64_t);


%ignore llvm::DIBuilder::createPointerType(DIType *, uint64_t);
%ignore llvm::DIBuilder::createPointerType(DIType *, uint64_t, uint64_t);
//%ignore llvm::DIBuilder::createPointerType(DIType *, uint64_t, uint64_t, StringRef);


//%ignore llvm::DIBuilder::createMemberPointerType(DIType *, DIType *, uint64_t);
%ignore llvm::DIBuilder::createMemberPointerType(DIType *, DIType *, uint64_t, uint64_t);
%ignore llvm::DIBuilder::createMemberPointerType(DIType *, DIType *, uint64_t, uint64_t, unsigned);


%ignore llvm::DIBuilder::createReferenceType(unsigned, DIType *);
%ignore llvm::DIBuilder::createReferenceType(unsigned, DIType *, uint64_t);

%ignore llvm::DIBuilder::createClassType(DIScope *, StringRef, DIFile *, unsigned, uint64_t, uint64_t, uint64_t, unsigned, DIType *, DINodeArray);
%ignore llvm::DIBuilder::createClassType(DIScope *, StringRef, DIFile *, unsigned, uint64_t, uint64_t, uint64_t, unsigned, DIType *, DINodeArray, DIType *);
%ignore llvm::DIBuilder::createClassType(DIScope *, StringRef, DIFile *, unsigned, uint64_t, uint64_t, uint64_t, unsigned, DIType *, DINodeArray, DIType *, MDNode *);
                                     
%ignore llvm::DIBuilder::createStructType(DIScope *, StringRef, DIFile *, unsigned, uint64_t, uint64_t, unsigned, DIType *, DINodeArray);
%ignore llvm::DIBuilder::createStructType(DIScope *, StringRef, DIFile *, unsigned, uint64_t, uint64_t, unsigned, DIType *, DINodeArray, unsigned);
%ignore llvm::DIBuilder::createStructType(DIScope *, StringRef, DIFile *, unsigned, uint64_t, uint64_t, unsigned, DIType *, DINodeArray, unsigned, DIType *);

%ignore llvm::DIBuilder::createUnionType(DIScope *, StringRef, DIFile *, unsigned, uint64_t, uint64_t, unsigned, DINodeArray);
%ignore llvm::DIBuilder::createUnionType(DIScope *, StringRef, DIFile *, unsigned, uint64_t, uint64_t, unsigned, DINodeArray, unsigned);
%ignore llvm::DIBuilder::createUnionType(DIScope *, StringRef, DIFile *, unsigned, uint64_t, uint64_t, unsigned, DINodeArray, unsigned, DIType *);


%ignore llvm::DIBuilder::createEnumerationType(DIScope *, StringRef, DIFile *, unsigned, uint64_t, uint64_t, DINodeArray, DIType *);
        
%ignore llvm::DIBuilder::createSubroutineType(DITypeRefArray);
%ignore llvm::DIBuilder::createSubroutineType(DITypeRefArray, unsigned);

%ignore llvm::DIBuilder::createForwardDecl(unsigned, StringRef, DIScope *, DIFile *, unsigned);
%ignore llvm::DIBuilder::createForwardDecl(unsigned, StringRef, DIScope *, DIFile *, unsigned, unsigned);
%ignore llvm::DIBuilder::createForwardDecl(unsigned, StringRef, DIScope *, DIFile *, unsigned, unsigned, uint64_t);
%ignore llvm::DIBuilder::createForwardDecl(unsigned, StringRef, DIScope *, DIFile *, unsigned, unsigned, uint64_t, uint64_t);

%ignore llvm::DIBuilder::createReplaceableCompositeType(unsigned Tag, StringRef, DIScope *, DIFile *, unsigned);
%ignore llvm::DIBuilder::createReplaceableCompositeType(unsigned Tag, StringRef, DIScope *, DIFile *, unsigned, unsigned);
%ignore llvm::DIBuilder::createReplaceableCompositeType(unsigned Tag, StringRef, DIScope *, DIFile *, unsigned, unsigned, uint64_t);
%ignore llvm::DIBuilder::createReplaceableCompositeType(unsigned Tag, StringRef, DIScope *, DIFile *, unsigned, unsigned, uint64_t, uint64_t);
%ignore llvm::DIBuilder::createReplaceableCompositeType(unsigned Tag, StringRef, DIScope *, DIFile *, unsigned, unsigned, uint64_t, uint64_t, unsigned);

%ignore llvm::DIBuilder::createGlobalVariable(DIScope *, StringRef, StringRef, DIFile *, unsigned, DIType *, bool, llvm::Constant *);

%ignore llvm::DIBuilder::createTempGlobalVariableFwdDecl(DIScope *, StringRef, StringRef, DIFile *, unsigned, DIType *, bool, llvm::Constant *);
        
%ignore llvm::DIBuilder::createAutoVariable(DIScope *, StringRef, DIFile *, unsigned, DIType *);
%ignore llvm::DIBuilder::createAutoVariable(DIScope *, StringRef, DIFile *, unsigned, DIType *, bool);

%ignore llvm::DIBuilder::createParameterVariable(DIScope *, StringRef, unsigned, DIFile *, unsigned, DIType *);
%ignore llvm::DIBuilder::createParameterVariable(DIScope *, StringRef, unsigned, DIFile *, unsigned, DIType *, bool);

%ignore llvm::DIBuilder::createExpression(ArrayRef<uint64_t> Addr = None);


%ignore llvm::DIBuilder::createFunction(DIScope *, StringRef, StringRef, DIFile *, unsigned, DISubroutineType *, bool, bool, unsigned);

%ignore llvm::DIBuilder::createFunction(DIScope *, StringRef, StringRef, DIFile *, unsigned, DISubroutineType *, bool, bool, unsigned, unsigned);
%ignore llvm::DIBuilder::createFunction(DIScope *, StringRef, StringRef, DIFile *, unsigned, DISubroutineType *, bool, bool, unsigned, unsigned, bool);
%ignore llvm::DIBuilder::createFunction(DIScope *, StringRef, StringRef, DIFile *, unsigned, DISubroutineType *, bool, bool, unsigned, unsigned, bool, DITemplateParameterArray);

//%ignore llvm::DIBuilder::createFunction(DIScope *, StringRef, StringRef, DIFile *, unsigned, DISubroutineType *, bool, bool, unsigned, unsigned, bool, DITemplateParameterArray, DISubprogram *);

%ignore llvm::DIBuilder::createTempFunctionFwdDecl(DIScope *, StringRef, StringRef, DIFile *, unsigned, DISubroutineType *, bool, bool, unsigned);
%ignore llvm::DIBuilder::createTempFunctionFwdDecl(DIScope *, StringRef, StringRef, DIFile *, unsigned, DISubroutineType *, bool, bool, unsigned, unsigned);
%ignore llvm::DIBuilder::createTempFunctionFwdDecl(DIScope *, StringRef, StringRef, DIFile *, unsigned, DISubroutineType *, bool, bool, unsigned, unsigned, bool);
%ignore llvm::DIBuilder::createTempFunctionFwdDecl(DIScope *, StringRef, StringRef, DIFile *, unsigned, DISubroutineType *, bool, bool, unsigned, unsigned, bool, DITemplateParameterArray);

%ignore llvm::DIBuilder::createMethod(DIScope *, StringRef, StringRef, DIFile *, unsigned, DISubroutineType *, bool, bool);
%ignore llvm::DIBuilder::createMethod(DIScope *, StringRef, StringRef, DIFile *, unsigned, DISubroutineType *, bool, bool, unsigned Virtuality);
%ignore llvm::DIBuilder::createMethod(DIScope *, StringRef, StringRef, DIFile *, unsigned, DISubroutineType *, bool, bool, unsigned, unsigned);
%ignore llvm::DIBuilder::createMethod(DIScope *, StringRef, StringRef, DIFile *, unsigned, DISubroutineType *, bool, bool, unsigned, unsigned, int ThisAdjustment);
%ignore llvm::DIBuilder::createMethod(DIScope *, StringRef, StringRef, DIFile *, unsigned, DISubroutineType *, bool, bool, unsigned, unsigned, int, DIType *);
%ignore llvm::DIBuilder::createMethod(DIScope *, StringRef, StringRef, DIFile *, unsigned, DISubroutineType *, bool, bool, unsigned, unsigned, int, DIType *, unsigned);
%ignore llvm::DIBuilder::createMethod(DIScope *, StringRef, StringRef, DIFile *, unsigned, DISubroutineType *, bool, bool, unsigned, unsigned, int, DIType *, unsigned, bool);

%ignore llvm::DIBuilder::createLexicalBlockFile(DIScope *, DIFile *);

%ignore llvm::DIBuilder::createImportedDeclaration(DIScope *Context, DINode *, unsigned);
                
//%ignore llvm::DIBuilder::replaceArrays(DICompositeType *&T, DINodeArray);
%ignore llvm::DIBuilder::replaceArrays(DICompositeType *&T, DINodeArray, DINodeArray);  
  
*/

//rename overloaded methods like InsertDeclare

%rename(insertDeclareBefore) llvm::DIBuilder::insertDeclare(llvm::Value *, DILocalVariable *, DIExpression *, const DILocation *, Instruction *);

%rename(insertDeclareAtEnd) llvm::DIBuilder::insertDeclare(llvm::Value *, DILocalVariable *, DIExpression *, const DILocation *,BasicBlock *);


%rename(insertLabelBefore) llvm::DIBuilder::insertLabel(DILabel *, const DILocation *,  Instruction *);

%rename(insertLabelAtEnd) llvm::DIBuilder::insertLabel(DILabel *, const DILocation *,  BasicBlock *);

%rename(insertDbgValueBefore) llvm::DIBuilder::insertDbgValueIntrinsic(llvm::Value *, DILocalVariable *, DIExpression *, const DILocation *, Instruction *);

%rename(insertDbgValueAtEnd) llvm::DIBuilder::insertDbgValueIntrinsic(llvm::Value *,  DILocalVariable *, DIExpression *, const DILocation *, BasicBlock *);



//Commented out list of ignores for all exported methods. 
//Generated via swig -modula3 -generateignores -c++ M3DIBuilder.i
//Useful in development where you ignore everything except 
//the method you are working on and want to cut the error list down.

//%include llvmignores

%include <IR/DIBuilder.h>
