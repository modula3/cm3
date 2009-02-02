/*
see http://www.opengroup.org/onlinepubs/009695399/functions/swapcontext.html
*/
#include "context.h"
#include <stdarg.h>
#include <string.h>
#include <stdio.h>

#ifdef __CYGWIN__
#include <windows.h>
#endif

#ifdef __CYGWIN__
#define CONTEXT_PC 8
#define CONTEXT_STACK 7
#define CONTEXT_FRAME 6
#endif
#if defined(__OpenBSD__) && defined(__i386__)
#define CONTEXT_PC 0
#define CONTEXT_STACK 2
#define CONTEXT_FRAME 3
#endif

int getcontext(ucontext_t* context)
{
    sigprocmask(SIG_SETMASK, NULL, &context->uc_sigmask);
    setjmp(*(jmp_buf*)&context->uc_mcontext);
}

int setcontext(const ucontext_t* context)
{
    sigprocmask(SIG_SETMASK, &context->uc_sigmask, NULL);
    longjmp(*(jmp_buf*)&context->uc_mcontext, 1);
    return 0;
}

void internal_endcontext();

void makecontext(ucontext_t* context, void (*function)(), int argc, ...)
{
    va_list args;
    int i;
    size_t* stack;

    va_start(args, argc);
    stack = (size_t*)(((size_t)context->uc_stack.ss_sp) + context->uc_stack.ss_size);

    /* push a return to setcontext(context->uc_link) */

    *--stack = (size_t)context->uc_link;
    *--stack = context->uc_mcontext[CONTEXT_PC];
    *--stack = (size_t)setcontext;

    if (argc > 0)
    {
        /* endeavor to cleanup the parameters by returning
        to internal_endcontext with the frame setup with argc available;
        ebp is non-volatile and function must preserve it */

        context->uc_mcontext[CONTEXT_FRAME] = (size_t)stack;
        *--stack = (argc + 1) * sizeof(size_t);

        stack -= argc;
        for (i = 0 ; i < argc ; ++i)
        {
            stack[i] = va_arg(args, size_t);
        }
        *--stack = (size_t)internal_endcontext;
    }

#if defined(__OpenBSD__) && defined(__i386__)
    stack -= 1; /* why? */
#endif

    context->uc_mcontext[CONTEXT_PC] = (size_t)function;
    context->uc_mcontext[CONTEXT_STACK] = (size_t)stack;

    va_end(args);
}

int swapcontext(ucontext_t* old_context, const ucontext_t* new_context)
{
    getcontext(old_context);
    setcontext(new_context);
    return 0;
}
