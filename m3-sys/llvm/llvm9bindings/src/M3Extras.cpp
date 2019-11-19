//
// File M3Extras.cpp.
// C++ wrappers for C binding to various things not provided elsewhere.
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

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "./M3Extras.h"

// Some of these could become necessary as this binding grows: 
//#include "llvm/IR/Metadata.h" 
//#include "llvm/IR/Value.h" 
//#include "llvm/IR/Function.h" 
//#include "llvm/IR/Instruction.h" 
//#include "llvm/IR/BasicBlock.h" 
//#include "llvm/ADT/ArrayRef.h"
//#include "llvm/ADT/StringRef.h"
//#include "llvm/DebugInfo.h"
//#include "llvm/IR/DIBuilder.h"
//#include "llvm/IR/DebugInfo.h"
//#include "llvm-c/Core.h"

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(llvm::DISubprogram, LLVMDISubprogram)
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(llvm::DIGlobalVariableExpression, LLVMDIGlobalVariableExpression)

using namespace llvm;

/* in llvm core
LLVMValueRef LLVMGetDeclaration(LLVMModuleRef M, unsigned id, LLVMTypeRef *Types, unsigned Count) {
  Intrinsic::ID intrinId = (Intrinsic::ID) id;
  ArrayRef<Type*> Tys(unwrap(Types), Count);
  Function *F = Intrinsic::getDeclaration(unwrap(M),intrinId,Tys);
  return wrap(F);
}
*/

void GetLLVMVersion(int *major,int *minor) {
  *major = LLVM_VERSION_MAJOR;
  *minor = LLVM_VERSION_MINOR;
}

unsigned GetM3IntrinsicId(M3Intrinsic id) {

  if (id == m3memset) return Intrinsic::memset;
  if (id == m3memcpy) return Intrinsic::memcpy;
  if (id == m3memmov) return Intrinsic::memmove;
  if (id == m3round) return Intrinsic::round;
  if (id == m3floor) return Intrinsic::floor;
  if (id == m3trunc) return Intrinsic::trunc;
  if (id == m3ceil) return Intrinsic::ceil;
  if (id == m3fabs) return Intrinsic::fabs;
  if (id == m3minnum) return Intrinsic::minnum;
  if (id == m3maxnum) return Intrinsic::maxnum;
  if (id == m3fshl) return Intrinsic::fshl;
  if (id == m3fshr) return Intrinsic::fshr;
  
  assert(false && " invalid m3 intrinsic id");
}

//from DebugInfo.c - LLVMDIBuilderCreateDebugLoc
LLVMMetadataRef
DIBGetDebugLoc(LLVMContextRef Ctx, unsigned Line,
                                 unsigned Column, LLVMMetadataRef Scope,
                                 LLVMMetadataRef InlinedAt) {
  return wrap(DILocation::get(*unwrap(Ctx), Line, Column, unwrap(Scope),
                              unwrap(InlinedAt)));
}

/* old
LLVMDILocation DIBGetDebugLoc(unsigned Line, 
                              unsigned Col, 
                              LLVMDIScope Scope) {

  llvm::DIScope *S = unwrap(Scope);
  llvm::DebugLoc loc = llvm::DebugLoc::get(Line,Col,reinterpret_cast<MDNode *>(S));
  return wrap(loc);
}
*/

void DIBSetFunctionTag(LLVMValueRef func, LLVMDISubprogram sp) {
  
  llvm::DISubprogram *sub;  
  llvm::Function *f = llvm::unwrap<llvm::Function>(func);
  sub = unwrap(sp);
  f->setSubprogram(sub);  
}

void DIBSetGlobalTag(LLVMValueRef global, LLVMDIGlobalVariableExpression GVE) {

  llvm::DIGlobalVariableExpression *gve;
  llvm::GlobalVariable *glob = llvm::unwrap<llvm::GlobalVariable>(global);
  gve = unwrap(GVE);  
  glob->addDebugInfo(gve);
}

LLVMValueRef LLVMConstQuad(LLVMContextRef C, QuadRec *q) {
  Type *ty = Type::getFP128Ty(*unwrap(C));
  uint64_t ar[2] = {q->a,q->b};
  APInt ai(128,ar);
  APFloat quad(APFloat::IEEEquad(), ai);
  return wrap(ConstantFP::get(ty,quad));  
}

// End M3Extras.cpp
