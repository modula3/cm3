/* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. */
/* See file COPYRIGHT-CMASS for details. */

#include <sys/utsname.h>
typedef struct utsname utsname_t;

typedef void* TEXT;

TEXT M3toC__CopyStoT(const char*);

int
OSConfigPosixC__Init(
    TEXT* host_name,
    TEXT* host_arch,
    TEXT* os_name,
    TEXT* os_version)
{
    utsname_t uts;
    int result;

    result = uname(&uts);
    if (result < 0)
        goto Exit;

    *host_name  = M3toC__CopyStoT(uts.nodename);
    *host_arch  = M3toC__CopyStoT(uts.machine);
    *os_name    = M3toC__CopyStoT(uts.sysname);
    *os_version = M3toC__CopyStoT(uts.release);

Exit:
    return result;
}
