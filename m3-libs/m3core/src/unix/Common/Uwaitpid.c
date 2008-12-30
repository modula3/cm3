/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#include "m3unix.h"
#include <unistd.h>
#include <sys/wait.h>
#include <assert.h>

typedef struct _m3_waitpid_status_t {
    UINT8 w_Coredump;
    UINT8 w_Termsig;
    UINT8 w_Retcode;
} m3_waitpid_status_t;

#define m3_WNOHANG 1

pid_t
m3_waitpid(
    pid_t pid,
    m3_waitpid_status_t* out_m3_status,
    int m3_options)
{
    pid_t pid_result = { 0 };
    unsigned status = { 0 };
    int options = { 0 };
    m3_waitpid_status_t m3_status = { 0 };

    *out_m3_status = m3_status;

    if (m3_options & m3_WNOHANG)
        m3_options |= WNOHANG;

    pid_result = waitpid(pid, &status, options);
    if ((pid_result == 0) || (pid_result == -1))
        goto Exit;

    if (WIFEXITED(status))
    {
        assert(WEXITSTATUS(status) <= 0xFF);
        m3_status.w_Retcode = WEXITSTATUS(status);
        m3_status.w_Exited = 1;
    }
    else if (WIFSIGNALED(status))
    {
        assert(WTERMSIG(status) <= 0x7F);
        m3_status.w_Termsig = WTERMSIG(status);
        m3_status.w_Signaled = 1;
    }
    else if (WIFSTOPPED(status))
    {
        m3_status.w_Stopsig = WSTOPSIG(status);
        m3_status.w_Stopped = 1;
    }
#ifdef WIFCONTINUED     /* Not all implementations support this */
    else if (WIFCONTINUED(status))
    {
        m3_status.w_Continued = 1;
    }
#ifdef WCOREDUMP
    m3_status.w_Coredump = WCOREDUMP(status);
#endif
    m3_status.w_A_Loophole = ((UINT32) m3_status.w_Coredump << 15) | ((UINT32) m3_status.w_Termsig << 8) | ((UINT32) m3_status.w_Retcode);

Exit:
    *out_m3_status = m3_status;
    return pid_result;
}
