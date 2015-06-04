//
// File M3Extras.cpp.
// C++ wrappers for C binding to various things not provided elsewhere.
//

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
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
//#include "llvm-c/Core.h"

using namespace llvm;


/* peter added getdeclaration */
LLVMValueRef LLVMGetDeclaration(LLVMModuleRef M, unsigned id, LLVMTypeRef *Types, unsigned Count) {
  Intrinsic::ID intrinId = (Intrinsic::ID) id;
  ArrayRef<Type*> Tys(unwrap(Types), Count);
  Function *F = Intrinsic::getDeclaration(unwrap(M),intrinId,Tys);
  return wrap(F);
}

//copied from core.cpp
static AtomicOrdering mapFromLLVMOrdering(LLVMAtomicOrdering Ordering) {
  switch (Ordering) {
    case LLVMAtomicOrderingNotAtomic: return NotAtomic;
    case LLVMAtomicOrderingUnordered: return Unordered;
    case LLVMAtomicOrderingMonotonic: return Monotonic;
    case LLVMAtomicOrderingAcquire: return Acquire;
    case LLVMAtomicOrderingRelease: return Release;
    case LLVMAtomicOrderingAcquireRelease: return AcquireRelease;
    case LLVMAtomicOrderingSequentiallyConsistent:
      return SequentiallyConsistent;
  }

  llvm_unreachable("Invalid LLVMAtomicOrdering value!");
}

//peter added
LLVMValueRef LLVMBuildAtomicCmpXchg(LLVMBuilderRef B,
                                LLVMValueRef PTR,
                                LLVMValueRef Cmp,
                                LLVMValueRef New,
                                LLVMAtomicOrdering SuccessOrdering,
                                LLVMAtomicOrdering FailureOrdering,
                                LLVMBool singleThread) {
  return wrap(unwrap(B)->CreateAtomicCmpXchg(unwrap(PTR),
                                             unwrap(Cmp),
                                             unwrap(New),
                                      mapFromLLVMOrdering(SuccessOrdering),
                                      mapFromLLVMOrdering(FailureOrdering),
                                      singleThread ? SingleThread : CrossThread));
}

// return the intrinsic id from the enum
// As of 2015-05-25, unused in M3 code and has no M3 binding. 
unsigned GetM3IntrinsicId(M3Intrinsic id) {

  if (id == m3memset) return Intrinsic::memset;
  if (id == m3memcpy) return Intrinsic::memcpy;
  if (id == m3memmov) return Intrinsic::memmove;
  if (id == m3round) return Intrinsic::round;
  if (id == m3floor) return Intrinsic::floor;
  if (id == m3trunc) return Intrinsic::trunc;
  if (id == m3ceil) return Intrinsic::ceil;

  return 1;
}

/*--.. Module identifier ...................................................--*/
const char * LLVMGetModuleIdentifer(LLVMModuleRef M) {
  return unwrap(M)->getModuleIdentifier().c_str();
}

void LLVMSetModuleIdentifier(LLVMModuleRef M, const char *Id) {
  unwrap(M)->setModuleIdentifier(Id);
}


