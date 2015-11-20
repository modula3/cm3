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

/*
 * Map of intrinsics that M3 uses to the C++ intrinsic id
 */
enum M3Intrinsic {
  m3memset,
  m3memcpy,
  m3memmov,
  m3round,
  m3floor,
  m3trunc,
  m3ceil,
  m3fabs,
  m3minnum,
  m3maxnum
};

/*
 * Get the C++ instrinsic id which we pass to LLVMGetDeclaration to get the
 * intrinsic signature.
 */
unsigned GetM3IntrinsicId(M3Intrinsic id);

/*
 * Return the function signature for the intrinsic id and the types parameters
 */
LLVMValueRef LLVMGetDeclaration(LLVMModuleRef M, unsigned id, LLVMTypeRef *Types, unsigned Count);

LLVMValueRef LLVMBuildAtomicCmpXchg(LLVMBuilderRef B,
                                    LLVMValueRef PTR,
                                    LLVMValueRef Cmp,
                                    LLVMValueRef New,
                                    LLVMAtomicOrdering SuccessOrdering,
                                    LLVMAtomicOrdering FailureOrdering,
                                    LLVMBool singleThread);

/*
 * Set the atomic ordering attribute on a load or store instruction
 */
void LLVMSetAtomic(LLVMValueRef MemAccessInst, LLVMAtomicOrdering Ordering);

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

// End M3Extras.h 
