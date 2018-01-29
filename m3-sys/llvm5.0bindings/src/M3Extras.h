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

#ifdef __cplusplus
extern "C" {
#endif
  
typedef struct DILocation * LLVMDILocation;
typedef struct DIScope * LLVMDIScope;
typedef struct DISubprogram * LLVMDISubprogram;
typedef struct DIGlobalVariableExpression * LLVMDIGlobalVariableExpression;

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

/*
 * Get LLVM version info
 */ 
void GetLLVMVersion(int *major,int *minor);


// The debugging code needs extra functions not provided by
/// DIBuilder.h

/*
 * Get a location for use with SetCurrentDebugLocation
 */
LLVMDILocation DIBGetDebugLoc(unsigned Line, 
                              unsigned Col, 
                              LLVMDIScope Scope);

/*
 *  Put the !dbg tag in the define func pointing to the DISubprogram definition
 */
void DIBSetFunctionTag(LLVMValueRef func, LLVMDISubprogram sp);

/*
 *  Put the !dbg tag in a global declaration
 */
void DIBSetGlobalTag(LLVMValueRef global, LLVMDIGlobalVariableExpression GVE);
  
#ifdef __cplusplus
}
#endif /* defined(__cplusplus) */

#endif /* defined(M3Extras_H) */

// End M3Extras.h 
