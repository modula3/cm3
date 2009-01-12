
/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <assert.h>

typedef unsigned char UINT8;
typedef unsigned short UINT16;

typedef struct _m3_waitpid_status_t {
/* sort by size and then by name */
    UINT16 w_Loophole;
    UINT8  w_Continued;
    UINT8  w_Coredump;
    UINT8  w_Exited;
    UINT8  w_Retcode;
    UINT8  w_Signaled;
    UINT8  w_Stopped;
    UINT8  w_Stopsig;
    UINT8  w_Termsig;
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

    if (out_m3_status != NULL)
    {
       *out_m3_status = m3_status;
    }

    if ((m3_options & m3_WNOHANG) != 0)
    {
        m3_options |= WNOHANG;
    }

    pid_result = waitpid(pid, &status, options);
    if ((pid_result == 0) || (pid_result == -1) || (out_m3_status == NULL))
    {
        goto Exit;
    }

/* Again, see http://www.opengroup.org/onlinepubs/009695399/functions/waitpid.html */
/* Or search the web for 'opengroup waitpid'. */

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
        assert(WSTOPSIG(status) <= 0xFF);
        m3_status.w_Stopsig = WSTOPSIG(status);
        m3_status.w_Stopped = 1;
    }
#ifdef WIFCONTINUED /* Not all implementations support this */
    else if (WIFCONTINUED(status))
    {
        m3_status.w_Continued = 1;
    }
#endif

#ifdef WCOREDUMP
    m3_status.w_Coredump = WCOREDUMP(status);
#endif

    m3_status.w_Loophole = ((((UINT16) m3_status.w_Coredump) << 15) | (((UINT16) m3_status.w_Termsig) << 8) | ((UINT16) m3_status.w_Retcode));

Exit:
    if (out_m3_status != NULL)
    {
       *out_m3_status = m3_status;
    }
    return pid_result;
}
