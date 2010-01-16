/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#ifdef _MSC_VER
#pragma optimize("gty", on)
#undef _DLL
#ifndef _MT
#define _MT
#endif
#endif

#include "m3core.h"
#define M3MODULE Uexec

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32

void Uexec__RepackStatus(int* var_status)
{
    int status;

    assert(var_status != NULL);

    /* Posix says you must pass "the original" to the macros. */
    status = ((WTERMSIG(*var_status) << 8) | WEXITSTATUS(*var_status));
#ifdef WCOREDUMP
    status |= (WCOREDUMP(*var_status) ? 0x8000 : 0);
#endif
    *var_status = status;
}

/* If needed, define functions Uexec_WTERMSIG, Uexec_WEXITSTATUS, etc. */

#endif

#ifdef _WIN64
typedef intptr_t m3_exec_t; /* correct for Win32 but requires newer headers */
#else
typedef int m3_exec_t;
#endif

M3WRAP2_(m3_exec_t, execv, const char*, char**)
M3WRAP2_(m3_exec_t, execvp, const char*, char**)
M3WRAP3_(m3_exec_t, execve, const char*, char**, char**)
#ifndef _WIN32
M3WRAP3(m3_pid_t, waitpid, m3_pid_t, int*, int)
#endif

#ifdef __cplusplus
}
#endif
