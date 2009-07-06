/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#include "m3unix.h"

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

m3_exec_t Uexec__execv(const char* name, char** argv)
{
#ifdef _WIN32
    return _execv(name, argv);
#else
    return execv(name, argv);
#endif
}

m3_exec_t Uexec__execvp(const char* name, char** argv)
{
#ifdef _WIN32
    return _execvp(name, argv);
#else
    return execvp(name, argv);
#endif
}

m3_exec_t Uexec__execve(const char* name, char** argv, char** envp)
{
#ifdef _WIN32
    return _execve(name, argv, envp);
#else
    return execve(name, argv, envp);
#endif
}

#ifndef _WIN32

pid_t Uexec__waitpid(pid_t pid, int* status, int options)
{
    return waitpid(pid, status, options);
}

#endif

#ifdef __cplusplus
}
#endif
