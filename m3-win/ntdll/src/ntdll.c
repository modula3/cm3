/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#if !defined(_MSC_VER) && !defined(__stdcall)
#define __stdcall /* nothing */
#endif

#ifdef __cplusplus
extern "C" {
#endif

long __stdcall NtDuplicateObject(int a, int b, int c, int d, int e, int f, int g) { return -1; }
long __stdcall NtSuspendThread(int thread, int suspendCount) { return -1; }
long __stdcall NtResumeThread(int thread, int suspendCount) { return -1; }
long __stdcall NtGetContextThread(int thread, int context) { return -1; }

#ifdef __cplusplus
}
#endif
