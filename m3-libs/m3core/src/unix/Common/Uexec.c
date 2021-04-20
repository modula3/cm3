/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif
#undef M3MODULE /* Support concatenating multiple .c files. */
#define M3MODULE Uexec

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32

M3_DLL_EXPORT void __cdecl
Uexec__RepackStatus(int* var_status)
{
    int status = { 0 };

    assert(var_status != NULL);

    /* Posix says you must pass "the original" to the macros.
     * That is why we don't read *var_status just once.
     *
     * On OSF, using WEXITSTATUS if WIFEXITED is false or
     * WTERMSIG if WIFSIGNALED is false, return -1, which
     * later violates a subrange: ProcessPosixCommon.Wait:
     * MIN(LAST(Process.ExitCode), status) ends up -1
     * which doesn't fit in ExitCode.
     *
     * A close reading of POSIX says what WEXITSTATUS
     * does if WIFEXITED is true and what WTERMSIG does
     * if WIFSIGNALED is true, but not what they do
     * if the conditions are false.
     */

    assert(!(WIFEXITED(*var_status) && WIFSIGNALED(*var_status)));

    if (WIFEXITED(*var_status))
      status |= WEXITSTATUS(*var_status);

    if (WIFSIGNALED(*var_status))
      status |= (WTERMSIG(*var_status) << 8);

    /* What about WSTOPSIG, WIFSTOPPED, WIFCONTINUED? */

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

#ifdef _WIN32
M3WRAP2_(m3_exec_t, execv, const char*, char const*const*)
M3WRAP2_(m3_exec_t, execvp, const char*, char const*const*)
M3WRAP3_(m3_exec_t, execve, const char*, char const*const*, char const*const*)
#else
M3WRAP3(m3_pid_t, waitpid, m3_pid_t, int*, int)
M3WRAP2(m3_exec_t, execv, const char*, char**)
M3WRAP2(m3_exec_t, execvp, const char*, char**)
M3WRAP3(m3_exec_t, execve, const char*, char**, char**)
#endif

#ifdef __cplusplus
}
#endif
