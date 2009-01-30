/*
see http://www.opengroup.org/onlinepubs/009695399/functions/swapcontext.html
see http://www.crystalclearsoftware.com/soc/coroutine/coroutine/fibers.html
*/
#define _WIN32_WINNT 0x0400
#include "context.h"
#include <windows.h>
#include <stdarg.h>
#include <assert.h>

#ifndef __CYGWIN__
#define sigprocmask(a, b, c) /* nothing */
#define SIG_SETMASK 0
#else
#include <signal.h>
#endif

void __stdcall internal_context_fiberentry(void* a)
{
    ucontext_t* context = (ucontext_t*)a;
    context->function(context->argv[ 0], context->argv[ 1], context->argv[ 2], context->argv[ 3],
                      context->argv[ 4], context->argv[ 5], context->argv[ 6], context->argv[ 7],
                      context->argv[ 8], context->argv[ 9], context->argv[10], context->argv[11],
                      context->argv[12], context->argv[13], context->argv[14], context->argv[15],
                      context->argv[16], context->argv[17], context->argv[18], context->argv[19],
                      context->argv[20], context->argv[21], context->argv[22], context->argv[23],
                      context->argv[24], context->argv[25], context->argv[26], context->argv[27],
                      context->argv[28], context->argv[29], context->argv[30], context->argv[31]);
    setcontext(context->uc_link);
}

typedef BOOL (__stdcall* IsThreadAFiber_t)(void);
BOOL __stdcall downlevel_IsThreadAFiber(void);
BOOL __stdcall init_IsThreadAFiber(void);

IsThreadAFiber_t _IsThreadAFiber = init_IsThreadAFiber;

BOOL __stdcall downlevel_IsThreadAFiber(void)
{
    return !((GetCurrentFiber() == (void*)0x1E00) || (GetCurrentFiber() == NULL));
}

BOOL __stdcall init_IsThreadAFiber(void)
{
    IsThreadAFiber_t temp = NULL;
    HMODULE Kernel32;

    Kernel32 = LoadLibraryW(L"Kernel32.dll");
    if (Kernel32 != NULL)
        temp = (IsThreadAFiber_t) GetProcAddress(Kernel32, "IsThreadAFiber");
    if (temp == NULL)
        temp = downlevel_IsThreadAFiber;
    _IsThreadAFiber = temp;
    return (*temp)();
}

#define IsThreadAFiber _IsThreadAFiber

int getcontext(ucontext_t* context)
{
    if (context == NULL)
        return 0;

    ZeroMemory(context, sizeof(*context));

    if (IsThreadAFiber() == FALSE)
        ConvertThreadToFiber(0);

    context->fiber = GetCurrentFiber();

    return 0;
}

int setcontext(const ucontext_t* context)
{
    if (context == NULL)
        return 0;

    if (IsThreadAFiber() == FALSE)
        ConvertThreadToFiber(0);

    SwitchToFiber(context->fiber);
    return 0;
}

void makecontext(ucontext_t* context, void (*function)(), int argc, ...)
{
    va_list args;
    int i;

    if ((context == NULL) || (argc > 32))
        return;
    context->fiber = CreateFiber(0, internal_context_fiberentry, context);
#if 0
    memcpy(context->argv, &argc + 1, argc * sizeof(int));
#else
    va_start(args, argc);
    for (i = 0 ; i < argc ; ++i)
    {
        context->argv[i] = va_arg(args, size_t);
    }
    va_end(args);
#endif
    context->function = function;
}

int swapcontext(ucontext_t* old_context, const ucontext_t* new_context)
{
    setcontext(new_context);
    return 0;
}
