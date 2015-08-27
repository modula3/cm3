//===- DIBuilderBindings.cpp - Bindings for DIBuilder ---------------------===//
//
//  The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines C bindings for the DIBuilder class.
//
//===----------------------------------------------------------------------===//
#include "llvm-c/Core.h"

#include "DIBuilderBindings.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/Support/Debug.h"

using namespace llvm;

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(DIBuilder, LLVMDIBuilderRef)
//DEFINE_SIMPLE_CONVERSION_FUNCTIONS(DIFile, LLVMValueRef)
// ^duplicates Value.h:559

namespace {
  template <typename T> T unwrapDI(LLVMValueRef v) {
    return v ? T(unwrap<MDNode>(v)) : T();
  }
}


LLVMDIBuilderRef LLVMNewDIBuilder(LLVMModuleRef M)
{
  return wrap(new DIBuilder(*unwrap(M)));
}

void LLVMDIBuilderDestroy(LLVMDIBuilderRef D)
{
  delete unwrap(D);
}

void LLVMDIBuilderFinalize(LLVMDIBuilderRef D)
{
  unwrap(D)->finalize();
}

LLVMValueRef LLVMDIBuilderCreateCompileUnit(
  LLVMDIBuilderRef D,
  unsigned Lang,
  const char *File,
  const char *Dir,
  const char *Producer,
  bool Optimized,
  const char *Flags,
  unsigned RuntimeVersion)
{
  DIBuilder *db = unwrap(D);
  DICompileUnit CU = db->createCompileUnit(
    Lang,
    File,
    Dir,
    Producer,
    Optimized,
    Flags,
    RuntimeVersion);
    return wrap(CU);

  //LLVMValueRef val;
  // return reinterpret_cast<LLVMValueRef>(&CU);
//    return reinterpret_cast<LLVMValueRef>(const_cast<DICompileUnit*>(CU));

}

//
LLVMValueRef LLVMDIBuilderCreateFile(
  LLVMDIBuilderRef D,
  const char *File,
  const char *Dir)
{
  DIBuilder *db = unwrap(D);
  DIFile F = db->createFile(File, Dir);
  return wrap(F);
}
//
//
LLVMValueRef LLVMDIBuilderCreateLexicalBlock(
  LLVMDIBuilderRef D,
  LLVMValueRef Scope,
  LLVMValueRef File,
  unsigned Line,
  unsigned Column)
{
  DIBuilder *db = unwrap(D);
  DILexicalBlock LB = db->createLexicalBlock(
    unwrapDI<DIDescriptor>(Scope),
    unwrapDI<DIFile>(File),
    Line,
    Column,
    0 /*DWARF Discriminator.  What is that? */ );
  return wrap(LB);
}

LLVMValueRef LLVMDIBuilderCreateFunction(
  LLVMDIBuilderRef D,
  LLVMValueRef Scope,
  const char *Name,
  const char *LinkageName,
  LLVMValueRef File,
  unsigned Line,
  LLVMValueRef CompositeType,
  bool IsLocalToUnit,
  bool IsDefinition,
  unsigned ScopeLine,
  unsigned Flags,
  bool IsOptimized,
  LLVMValueRef Func)
{
  DIBuilder *db = unwrap(D);
  DISubprogram SP = db->createFunction(
    unwrapDI<DIDescriptor>(Scope),
    Name,
    LinkageName,
    unwrapDI<DIFile>(File),
    Line,
    unwrapDI<DICompositeType>(CompositeType),
    IsLocalToUnit,
    IsDefinition,
    ScopeLine,
    Flags,
    IsOptimized,
    unwrap<Function>(Func));
  return wrap(SP);
}

LLVMValueRef LLVMDIBuilderCreateLocalVariable(
  LLVMDIBuilderRef D,
  unsigned Tag,
  LLVMValueRef Scope,
  const char *Name,
  LLVMValueRef File,
  unsigned Line,
  LLVMValueRef Ty,
  bool AlwaysPreserve,
  unsigned Flags,
  unsigned ArgNo)
{
  DIBuilder *db = unwrap(D);
  DIVariable V = db->createLocalVariable(
    Tag,
    unwrapDI<DIDescriptor>(Scope),
    Name,
    unwrapDI<DIFile>(File),
    Line,
    unwrapDI<DIType>(Ty),
    AlwaysPreserve,
    Flags,
    ArgNo);
  return wrap(V);
}

LLVMValueRef LLVMDIBuilderCreateGlobalVariable(
  LLVMDIBuilderRef D,
  const char *Name,
  LLVMValueRef File,
  unsigned Line,
  LLVMValueRef Ty,
  bool isLocalToUnit,
  LLVMValueRef Val)
{
  DIBuilder *db = unwrap(D);
  DIGlobalVariable V = db->createGlobalVariable(
    Name,
    unwrapDI<DIFile>(File),
    Line,
    unwrapDI<DIType>(Ty),
    isLocalToUnit,
    unwrap(Val));
  return wrap(V);
}

LLVMValueRef LLVMDIBuilderCreateReplaceableForwardDecl(
  LLVMDIBuilderRef D,
  unsigned Tag,
  const char *Name,
  LLVMValueRef Scope,
  LLVMValueRef File,
  unsigned Line,
  unsigned RuntimeLang,
  uint64_t SizeInBits,
  uint64_t AlignInBits,
  const char *UniqueIdentifier)
{
  DIBuilder *db = unwrap(D);
  DICompositeType CT = db->createReplaceableForwardDecl(
    Tag,
    Name,
    unwrapDI<DIScope>(Scope),
    unwrapDI<DIFile>(File),
    Line,
    RuntimeLang,
    SizeInBits,
    AlignInBits,
    UniqueIdentifier);
  return wrap(CT);
}

LLVMValueRef LLVMDIBuilderCreateEnumerator(
  LLVMDIBuilderRef D,
  const char *Name,
  int64_t Val)
{
  DIBuilder *db = unwrap(D);
  DIEnumerator de = db->createEnumerator(Name,Val);
  return wrap(de);
}

LLVMValueRef LLVMDIBuilderCreateBasicType(
  LLVMDIBuilderRef D,
  const char *Name,
  uint64_t SizeInBits,
  uint64_t AlignInBits,
  unsigned Encoding)
{
  DIBuilder *db = unwrap(D);
  DIBasicType T = db->createBasicType(
    Name,
    SizeInBits,
    AlignInBits,
    Encoding);
  return wrap(T);
}

LLVMValueRef LLVMDIBuilderCreatePointerType(
  LLVMDIBuilderRef D,
  LLVMValueRef PointeeType,
  uint64_t SizeInBits,
  uint64_t AlignInBits,
  const char *Name)
{
  DIBuilder *db = unwrap(D);
  DIDerivedType T = db->createPointerType(
    unwrapDI<DIType>(PointeeType),
    SizeInBits,
    AlignInBits,
    Name);
  return wrap(T);
}

LLVMValueRef LLVMDIBuilderCreateInheritance(
    LLVMDIBuilderRef D,
    LLVMValueRef Ty,
    LLVMValueRef BaseTy,
    uint64_t BaseOffset,
    unsigned Flags)
{
  DIBuilder *db = unwrap(D);
  DIDerivedType T = db->createInheritance(
    unwrapDI<DIType>(Ty),
    unwrapDI<DIType>(BaseTy),
    BaseOffset,
    Flags);
  return wrap(T);
}

LLVMValueRef LLVMDIBuilderCreateSetType(
  LLVMDIBuilderRef D,
  LLVMValueRef ElementType,
  uint64_t SizeInBits,
  uint64_t AlignInBits,
  const char *Name)
{
  DIBuilder *db = unwrap(D);
  DIDerivedType T; // Let's hope there's a default constructor.  
/* Not in 3.5.0 DIBuilder, not used in 3.5 version of M3CG_LLVM 
    DIDerivedType T = db->createSetType(
    unwrapDI<DIType>(ElementType),
    SizeInBits,
    AlignInBits,
    Name);
*/ 
  return wrap(T);
}

LLVMValueRef LLVMDIBuilderCreateSubroutineType(
  LLVMDIBuilderRef D,
  LLVMValueRef File,
  LLVMValueRef ParameterTypes)
{
  DIBuilder *db = unwrap(D);
  DICompositeType CT = db->createSubroutineType(
    unwrapDI<DIFile>(File),
    unwrapDI<DIArray>(ParameterTypes));
  return wrap(CT);
}

LLVMValueRef LLVMDIBuilderCreateStructType(
  LLVMDIBuilderRef D,
  LLVMValueRef Scope,
  const char *Name,
  LLVMValueRef File,
  unsigned Line,
  uint64_t SizeInBits,
  uint64_t AlignInBits,
  unsigned Flags,
  LLVMValueRef DerivedFrom,
  LLVMValueRef ElementTypes)
{
  DIBuilder *db = unwrap(D);
  DICompositeType CT = db->createStructType(
    unwrapDI<DIDescriptor>(Scope),
    Name,
    unwrapDI<DIFile>(File),
    Line,
    SizeInBits,
    AlignInBits,
    Flags,
    unwrapDI<DIType>(DerivedFrom),
    unwrapDI<DIArray>(ElementTypes));
  return wrap(CT);
}

LLVMValueRef LLVMDIBuilderCreateClassType(
  LLVMDIBuilderRef D,
  LLVMValueRef Scope,
  const char *Name,
  LLVMValueRef File,
  unsigned LineNumber,
  uint64_t SizeInBits,
  uint64_t AlignInBits,
  uint64_t OffsetInBits,
  unsigned Flags,
  LLVMValueRef DerivedFrom,
  LLVMValueRef Elements)
{
  DIBuilder *db = unwrap(D);
  DICompositeType CT = db->createClassType(
    unwrapDI<DIDescriptor>(Scope),
    Name,
    unwrapDI<DIFile>(File),
    LineNumber,
    SizeInBits,
    AlignInBits,
    OffsetInBits,
    Flags,
    unwrapDI<DIType>(DerivedFrom),
    unwrapDI<DIArray>(Elements));
  return wrap(CT);
}

LLVMValueRef LLVMDIBuilderCreateMemberType(
  LLVMDIBuilderRef D,
  LLVMValueRef Scope,
  const char *Name,
  LLVMValueRef File,
  unsigned Line,
  uint64_t SizeInBits,
  uint64_t AlignInBits,
  uint64_t OffsetInBits,
  unsigned Flags,
  LLVMValueRef Ty)
{
  DIBuilder *db = unwrap(D);
  DIDerivedType DT = db->createMemberType(
    unwrapDI<DIDescriptor>(Scope),
    Name,
    unwrapDI<DIFile>(File),
    Line,
    SizeInBits,
    AlignInBits,
    OffsetInBits,
    Flags,
    unwrapDI<DIType>(Ty));
  return wrap(DT);
}

LLVMValueRef LLVMDIBuilderCreateArrayType(
  LLVMDIBuilderRef D,
  uint64_t SizeInBits,
  uint64_t AlignInBits,
  LLVMValueRef ElementType,
  LLVMValueRef Subscripts)
{
  DIBuilder *db = unwrap(D);
  DICompositeType CT = db->createArrayType(
    SizeInBits,
    AlignInBits,
    unwrapDI<DIType>(ElementType),
    unwrapDI<DIArray>(Subscripts));
  return wrap(CT);
}

LLVMValueRef LLVMDIBuilderCreateTypedef(
  LLVMDIBuilderRef D,
  LLVMValueRef Ty,
  const char *Name,
  LLVMValueRef File,
  unsigned Line,
  LLVMValueRef Context)
{
  DIBuilder *db = unwrap(D);
  DIDerivedType DT = db->createTypedef(
    unwrapDI<DIType>(Ty),
    Name,
    unwrapDI<DIFile>(File),
    Line,
    unwrapDI<DIDescriptor>(Context));
  return wrap(DT);
}

LLVMValueRef LLVMDIBuilderGetOrCreateSubrange(
  LLVMDIBuilderRef D,
  int64_t Lo,
  int64_t Count)
{
  DIBuilder *db = unwrap(D);
  DISubrange S = db->getOrCreateSubrange(Lo, Count);
  return wrap(S);
}

LLVMValueRef LLVMDIBuilderCreateEnumerationType(
  LLVMDIBuilderRef D,
  LLVMValueRef Scope,
  const char *Name,
  LLVMValueRef File,
  unsigned LineNumber,
  uint64_t SizeInBits,
  uint64_t AlignInBits,
  LLVMValueRef Elements,
  LLVMValueRef UnderlyingType,
  const char *UniqueIdentifier)
{
  DIBuilder *db = unwrap(D);
  DICompositeType CT = db->createEnumerationType(
    unwrapDI<DIDescriptor>(Scope),
    Name,
    unwrapDI<DIFile>(File),
    LineNumber,
    SizeInBits,
    AlignInBits,
    unwrapDI<DIArray>(Elements),
    unwrapDI<DIType>(UnderlyingType),
    UniqueIdentifier);
  return wrap(CT);
}

LLVMValueRef LLVMDIBuilderGetOrCreateArray(
  LLVMDIBuilderRef D,
  LLVMValueRef *Data,
  size_t Length)
{
  DIBuilder *db = unwrap(D);
  ArrayRef<Value *> DataList(unwrap(Data), Length);
  DIArray A = db->getOrCreateArray(DataList);
  return wrap(A);
}
/* No such method in DIBuilder
LLVMValueRef LLVMDIBuilderGetOrCreateTypeArray(
  LLVMDIBuilderRef D,
  LLVMValueRef *Data,
  size_t Length)
{
  DIBuilder *db = unwrap(D);
  ArrayRef<Value *> DataList(unwrap(Data), Length);
  DITypeArray A = db->getOrCreateTypeArray(DataList);
  return wrap(A);
}
*/

//could do the before equivs of the next 2
LLVMValueRef LLVMDIBuilderInsertDeclareAtEnd(
  LLVMDIBuilderRef D,
  LLVMValueRef Storage,
  LLVMValueRef VarInfo,
  LLVMBasicBlockRef Block)
{
  DIBuilder *db = unwrap(D);
  Instruction *Instr = db->insertDeclare(
    unwrap(Storage),
    unwrapDI<DIVariable>(VarInfo),
    unwrap(Block));
  return wrap(Instr);
}

LLVMValueRef LLVMDIBuilderInsertValueAtEnd(
  LLVMDIBuilderRef D,
  LLVMValueRef Val,
  uint64_t Offset,
  LLVMValueRef VarInfo,
  LLVMBasicBlockRef Block)
{
  DIBuilder *db = unwrap(D);
  Instruction *Instr = db->insertDbgValueIntrinsic(
    unwrap(Val),
    Offset,
    unwrapDI<DIVariable>(VarInfo),
    unwrap(Block));
  return wrap(Instr);
}

//changed scope was *
LLVMValueRef LLVMGetDebugLoc(unsigned Line, unsigned Col, LLVMValueRef Scope)
{
//  MDNode *S = unwrapDI<DIDescriptor>(*Scope);
//  DebugLoc loc = DebugLoc::get(Line,Col,S);

  DIScope S = unwrapDI<DIScope>(Scope);
  DebugLoc loc = DebugLoc::get(Line,Col,S);
  LLVMContext &ctx = S->getContext();
  MDNode *L = loc.getAsMDNode(ctx);
  return wrap(L);
}

const char *LLVMGetFileName(LLVMValueRef Scope)
{
  DIScope S = unwrapDI<DIScope>(Scope);
  StringRef str = S.getFilename();
  return str.data();
}

const char *LLVMGetDirectory(LLVMValueRef Scope)
{
  DIScope S = unwrapDI<DIScope>(Scope);
  StringRef str = S.getDirectory();
  return str.data();
}
// Was commented out to here. 
/* test of returning the intrinsic id from the enum
enum M3Intrinsic {
  m3memset,
  m3memcpy,
  m3memmov,
  m3round,
  m3floor,
  m3ceil
};

unsigned GetId(M3Intrinsic id) {

  if (id == m3memset) return Intrinsic::memset;
  if (id == m3memcpy) return Intrinsic::memcpy;

  return 1;
}
*/
