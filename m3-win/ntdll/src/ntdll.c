/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

typedef void* HANDLE;
typedef unsigned long ULONG;
typedef void* PCONTEXT;
typedef long NTSTATUS;

#if !defined(_MSC_VER) && !defined(__stdcall)
#define __stdcall /* nothing */
#endif

NTSTATUS __stdcall NtDuplicateObject(HANDLE a, HANDLE b, HANDLE c, HANDLE* d, ULONG e, ULONG f, ULONG g) { return -1; }
NTSTATUS __stdcall NtSuspendThread(HANDLE thread, void* suspendCount) { return -1; }
NTSTATUS __stdcall NtResumeThread(HANDLE thread, void* suspendCount) { return -1; }
NTSTATUS __stdcall NtGetContextThread(HANDLE thread, PCONTEXT context) { return -1; }
