/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#ifndef __INTERIX
#error This is for Interix only.
#endif

#include <windows.h>
#include <ntapi.h>
#include <stddef.h>
#include <assert.h>

HANDLE ThreadPThread__pthread_self(void)
/* Cleverly define pthread_self as the NT handle, since
   we don't use pthread_self for other than suspend/resume.
*/
{
    /* convert pseudo handle to real handle
    This is exactly analogous to DuplicateHandle(GetCurrentProcess, GetCurrentThread(), ...) */
    HANDLE self = { 0 };
    long status = NtDuplicateObject(NtCurrentProcess(), NtCurrentThread(), NtCurrentProcess(), &self, 0, FALSE, DUPLICATE_SAME_ACCESS);
    int success = (status == 0);
    assert(success);
    return self;
}

int ThreadPThread__SuspendThread(HANDLE thread)
{
    long status = NtSuspendThread(thread, NULL);
    int success = (status == 0);
    assert(success);
    return success;
}

int ThreadPThread__RestartThread(HANDLE thread)
{
    long status = NtResumeThread(thread, NULL);
    int success = (status == 0);
    assert(success);
    return success;
}

void ThreadPThread__ProcessStopped(HANDLE thread, void *bottom, void *signal_context,
                                   void (*p)(void *start, void *limit))
{
    CONTEXT context = { 0 };
    int status = { 0 };
    void *sp = { 0 };
    long success = { 0 };
    
    ZeroMemory(&context, sizeof(context));
    context.ContextFlags = (CONTEXT_CONTROL | CONTEXT_INTEGER);
    status = NtGetContextThread(thread, &context);
    success = (status == 0);
    assert(success);

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
