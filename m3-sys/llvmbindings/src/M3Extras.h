//
// File M3Extras.h.
// C declarations for C binding to various things not provided elsewhere.
// 
// Derived from various files in the following: 

//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//

#ifndef M3Extras_H
#define M3Extras_H

//#include "llvm-c/Support.h"

#ifdef __cplusplus
extern "C" {
#endif

enum M3Intrinsic {
  m3memset,
  m3memcpy,
  m3memmov,
  m3round,
  m3floor,
  m3trunc,
  m3ceil
};

// return the intrinsic id from the enum
// As of 2015-05-25, unused in M3 code. 
unsigned GetM3IntrinsicId(M3Intrinsic id);

//peter added
LLVMValueRef LLVMGetDeclaration(LLVMModuleRef M, unsigned id, /*Intrinsic::ID id,*/ LLVMTypeRef *Types, unsigned Count);

//peter added
LLVMValueRef LLVMBuildAtomicCmpXchg(LLVMBuilderRef B,
                                    LLVMValueRef PTR,
                                    LLVMValueRef Cmp,
                                    LLVMValueRef New,
                                    LLVMAtomicOrdering SuccessOrdering,
                                    LLVMAtomicOrdering FailureOrdering,
                                    LLVMBool singleThread);

/**
 * Obtain the module identifier for a module.
 *
 * @see Module::getModuleIdentifier()
 */
const char *LLVMGetModuleIdentifier(LLVMModuleRef M);

/**
 * Set the module identifier for a module.
 *
 * @see Module::setModuleIdentifier()
 */
void LLVMSetModuleIdentifier(LLVMModuleRef M, const char *Id);

#ifdef __cplusplus
}
#endif /* defined(__cplusplus) */

#endif /* defined(M3Extras_H) */
