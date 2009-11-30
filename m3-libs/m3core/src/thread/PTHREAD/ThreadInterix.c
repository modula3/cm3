/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#ifndef __INTERIX

/* avoid empty file */

void ThreadInterix__Dummy(void)
{
}
 
#else

#include <windows.h>
#include <ntapi.h>
#include <stddef.h>
#include <assert.h>
#include <stdio.h>
#include <unistd.h>

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

static int CheckStatus(long status, int line)
{
    if (status == 0)
        return TRUE;
    fprintf(stderr, "status %lx at line %d\n", status, line);
    assert(status == 0);
    return FALSE;
}
#define CheckStatus(a) CheckStatus(a, __LINE__)

HANDLE __cdecl ThreadPThread__GetCurrentThreadHandleForSuspendResume(void)
{
    HANDLE self = { 0 };
    unsigned i = { 0 };
    long status = STATUS_ACCESS_DENIED;
    /* strange! */
    for (i = 0; (status == STATUS_ACCESS_DENIED) && i < 200; ++i)
    {
        if (i)
            usleep(1);
        status = NtDuplicateObject(NtCurrentProcess(), NtCurrentThread(), NtCurrentProcess(), &self, THREAD_ALL_ACCESS, FALSE, 0);
        if (status == 0)
            return self;
    }
    fprintf(stderr, "NtDuplicateObject(current thread) failed with %lx\n", status);
    CheckStatus(status);
    return self;
}

int __cdecl ThreadPThread__SuspendThread(HANDLE thread)
{
    long status = NtSuspendThread(thread, NULL);
    if (status == 0)
        return TRUE;
    fprintf(stderr, "NtSuspendThread(%p) failed with %lx\n", thread, status);
    return CheckStatus(status);
}

int __cdecl ThreadPThread__RestartThread(HANDLE thread)
{
    long status = NtResumeThread(thread, NULL);
    if (status == 0)
        return TRUE;
    fprintf(stderr, "NtResumeThread(%p) failed with %lx\n", thread, status);
    return CheckStatus(status);
}

void __cdecl ThreadPThread__ProcessStopped(HANDLE thread, void *bottom, void *signal_context,
                                           void (*p)(void *start, void *limit))
{
    CONTEXT context = { 0 };
    long status = { 0 };
    void *sp = { 0 };
    
    ZeroMemory(&context, sizeof(context));
    context.ContextFlags = (CONTEXT_CONTROL | CONTEXT_INTEGER);
    status = NtGetContextThread(thread, &context);
    if (status != 0)
        fprintf(stderr, "NtGetContextThread(%p) failed with %lx\n", thread, status);
    CheckStatus(status);

#if defined(_X86_)
    sp = (void*)context.Esp;
#elif defined(_AMD64_)
    sp = (void*)context.Rsp;
#else
#error unknown architecture
#endif
    /* process the stack */
    assert(signal_context == 0);
    p(sp, bottom);
    /* process the registers */
    p(&context, (char *)&context + sizeof(context));
}

#endif
