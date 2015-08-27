//===- DIBuilderBindings.h - Bindings for DIBuilder -------------*- C++ -*-===//
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

#ifndef LLVM_BINDINGS_LLVM_DIBUILDERBINDINGS_H
#define LLVM_BINDINGS_LLVM_DIBUILDERBINDINGS_H

#include "llvm-c/Core.h"

#ifdef __cplusplus
#include "llvm/IR/Metadata.h"
#include "llvm/Support/CBindingWrapping.h"
#endif

#ifdef __cplusplus
extern "C" {
  #endif

  typedef struct LLVMOpaqueDIBuilder *LLVMDIBuilderRef;

  LLVMDIBuilderRef LLVMNewDIBuilder(LLVMModuleRef M);

  void LLVMDIBuilderDestroy(LLVMDIBuilderRef D);

  void LLVMDIBuilderFinalize(LLVMDIBuilderRef D);

  LLVMValueRef LLVMDIBuilderCreateCompileUnit(
    LLVMDIBuilderRef D,
    unsigned Lang,
    const char *File,
    const char *Dir,
    const char *Producer,
    bool Optimized,
    const char *Flags,
    unsigned RuntimeVersion);

  LLVMValueRef LLVMDIBuilderCreateFile(
    LLVMDIBuilderRef D,
    const char *File,
    const char *Dir);

  LLVMValueRef LLVMDIBuilderCreateLexicalBlock(
    LLVMDIBuilderRef D,
    LLVMValueRef Scope,
    LLVMValueRef File,
    unsigned Line,
    unsigned Column);

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
    LLVMValueRef Func);

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
    unsigned ArgNo);

  LLVMValueRef LLVMDIBuilderCreateGlobalVariable(
    LLVMDIBuilderRef D,
    const char *Name,
    LLVMValueRef File,
    unsigned Line,
    LLVMValueRef Ty,
    bool isLocalToUnit,
    LLVMValueRef Val);

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
    const char *UniqueIdentifier);

  LLVMValueRef LLVMDIBuilderCreateEnumerator(
    LLVMDIBuilderRef D,
    const char *Name,
    int64_t Val);

  LLVMValueRef LLVMDIBuilderCreateBasicType(
    LLVMDIBuilderRef D,
    const char *Name,
    uint64_t SizeInBits,
    uint64_t AlignInBits,
    unsigned Encoding);

  LLVMValueRef LLVMDIBuilderCreatePointerType(
    LLVMDIBuilderRef D,
    LLVMValueRef PointeeType,
    uint64_t SizeInBits,
    uint64_t AlignInBits,
    const char *Name);

  LLVMValueRef LLVMDIBuilderCreateInheritance(
    LLVMDIBuilderRef D,
    LLVMValueRef Ty,
    LLVMValueRef BaseTy,
    uint64_t BaseOffset,
    unsigned Flags);

  LLVMValueRef LLVMDIBuilderCreateSetType(
    LLVMDIBuilderRef D,
    LLVMValueRef ElementType,
    uint64_t SizeInBits,
    uint64_t AlignInBits,
    const char *Name);

  LLVMValueRef LLVMDIBuilderCreateSubroutineType(
    LLVMDIBuilderRef D,
    LLVMValueRef File,
    LLVMValueRef ParameterTypes);

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
    LLVMValueRef ElementTypes);

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
    LLVMValueRef Elements);

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
    LLVMValueRef Ty);

  LLVMValueRef LLVMDIBuilderCreateArrayType(
    LLVMDIBuilderRef D,
    uint64_t SizeInBits,
    uint64_t AlignInBits,
    LLVMValueRef ElementType,
    LLVMValueRef Subscripts);

  LLVMValueRef LLVMDIBuilderCreateTypedef(
    LLVMDIBuilderRef D,
    LLVMValueRef Ty,
    const char *Name,
    LLVMValueRef File,
    unsigned Line,
    LLVMValueRef Context);

  LLVMValueRef LLVMDIBuilderGetOrCreateSubrange(
    LLVMDIBuilderRef D,
    int64_t Lo,
    int64_t Count);

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
    const char *UniqueIdentifier);

  LLVMValueRef LLVMDIBuilderGetOrCreateArray(
    LLVMDIBuilderRef D,
    LLVMValueRef *Data,
    size_t Length);

/* No such method in DIBuilder
  LLVMValueRef LLVMDIBuilderGetOrCreateTypeArray(
    LLVMDIBuilderRef D,
    LLVMValueRef *Data,
    size_t Length);
*/

  //could do the before equivs of the next 2
  LLVMValueRef LLVMDIBuilderInsertDeclareAtEnd(
    LLVMDIBuilderRef D,
    LLVMValueRef Storage,
    LLVMValueRef VarInfo,
    LLVMBasicBlockRef Block);

  LLVMValueRef LLVMDIBuilderInsertValueAtEnd(
    LLVMDIBuilderRef D,
    LLVMValueRef Val,
    uint64_t Offset,
    LLVMValueRef VarInfo,
    LLVMBasicBlockRef Block);

//changed scope was *
  LLVMValueRef LLVMGetDebugLoc(unsigned Line, unsigned Col, LLVMValueRef Scope);

const char *LLVMGetFileName(LLVMValueRef Scope);
const char *LLVMGetDirectory(LLVMValueRef Scope);

  #ifdef __cplusplus
} // extern "C"
#endif
#endif
