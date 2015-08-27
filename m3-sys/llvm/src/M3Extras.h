#include "llvm-c/Core.h"

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

#ifdef __cplusplus
}
#endif /* !defined(__cplusplus) */
