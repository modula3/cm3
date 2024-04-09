
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/DebugInfoMetadata.h"

using namespace llvm;

#ifdef __cplusplus
extern "C" {
#endif

//build a value ref of an 128 bit quad type from 2 64 bit values
//this function is for future reference if we ever get 128 bit float support
//in the front end and it should be in core.cpp.
LLVMValueRef LLVMConstQuad(LLVMContextRef C, const uint64_t qi[2]) {
  Type *ty = Type::getFP128Ty(*unwrap(C));
  //makearrayref deprecated
  //APInt ai(128,makeArrayRef(qi,2));
  APInt ai(128,ArrayRef<uint64_t>(qi,2));
  APFloat quad(APFloat::IEEEquad(), ai);
  return wrap(ConstantFP::get(ty,quad));  
}

//Get the function type for instrinsic functions. Should be in core.cpp
LLVMTypeRef LLVMGetFunctionType(LLVMValueRef Fn) {
  Function *Func = unwrap<Function>(Fn);
  FunctionType *FnT =
      cast<FunctionType>(Func->getFunctionType());
  return wrap(FnT);
}

#ifdef __cplusplus
}
#endif

