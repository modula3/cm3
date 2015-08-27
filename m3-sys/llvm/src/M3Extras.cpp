#include "M3Extras.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"

//should include these in core.cpp

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
