/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#ifdef __INTERIX

/*#include "../../../../../m3-win/w32api/include/windows.h"*/
/*#include "../../../../../m3-win/w32api/include/ddk/ntapi.h"*/

/* NOTE: HANDLE thread not pthread thread, needs work */

__declspec(dllimport) long __stdcall NtSuspendThread(HANDLE thread, void* suspendCount);
__declspec(dllimport) long __stdcall NtResumeThread(HANDLE thread, void* suspendCount);
__declspec(dllimport) long __stdcall NtGetContextThread(HANDLE thread, CONTEXT* context);

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
    assert(stack_grows_down);
    assert(context == 0);
    p(sp, bottom);
    /* process the registers */
    p(&context, (char *)&context + sizeof(context));
}

#endif /* interix */
