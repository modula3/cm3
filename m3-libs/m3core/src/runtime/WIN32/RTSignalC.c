/* Copyright 1996-2000, Critical Mass, Inc.   All rights reserved. */
/* See file COPYRIGHT-CMASS for details. */
#include <windows.h>
#if __cplusplus
extern "C" {
#endif

size_t RTSignalC_i386() {return 1;}
size_t RTSignalC_Amd64() {return 2;}
size_t RTSignalC_Arm64() {return 3;}

#if defined(_M_IX86) || defined(i386) || defined(_X86_)

size_t RTSignalC_Arch() { return RTSignalC_i386(); }

#elif defined(_M_AMD64) || defined(AMD64) || defined(_AMD64_) || defined(__x86_64__)

size_t RTSignalC_Arch() { return RTSignalC_Amd64(); }

#elif defined(_M_ARM64) || defined(ARM4) || defined(_ARM4_)

size_t RTSignalC_Arch() { return RTSignalC_Arm64(); }

#else
#error unknown architecture
#endif

#if __cplusplus
}
#endif
