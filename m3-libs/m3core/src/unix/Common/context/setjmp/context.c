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

#ifdef __CYGWIN__ /* This isn't going to work anyway. */
#define CONTEXT_PC 8 /* experimentally derived */
#define CONTEXT_STACK 7 /* experimentally derived */
#define CONTEXT_FRAME 6 /* experimentally derived */
#endif
#if defined(__OpenBSD__) && defined(__i386__)
#define CONTEXT_PC 0 /* experimentally derived */
#define CONTEXT_STACK 2 /* experimentally derived */
#define CONTEXT_FRAME 3 /* experimentally derived */
#endif
#if defined(__APPLE__) && defined(__ppc__)
#define CONTEXT_PC 21 /* experimentally derived */
#define CONTEXT_STACK 0 /* experimentally derived */
#endif

int getcontext(ucontext_t* context)
{
    sigprocmask(SIG_SETMASK, NULL, &context->uc_sigmask);
    setjmp(*(jmp_buf*)&context->uc_mcontext);
}

int setcontext(const ucontext_t* context)
{
    if (context)
    {
        sigprocmask(SIG_SETMASK, &context->uc_sigmask, NULL);
        longjmp(*(jmp_buf*)&context->uc_mcontext, 1);
    }
    return 0;
}

void internal_endcontext();

#if defined(__APPLE__) && defined(__ppc__)

/* see:
http://developer.apple.com/documentation/DeveloperTools/Conceptual/LowLevelABI/100-32-bit_PowerPC_Function_Calling_Conventions/32bitPowerPC.html#//apple_ref/doc/uid/TP40002438

From reading the APSL code, setjmp does not preserve the registers that are used for parameters; so burn them up and use
the later stack-based parameters for the actual parameters
*/
void internal_setcontext(size_t r3, size_t r4, size_t r5, size_t r6, size_t r7, size_t r8, size_t r9, size_t r10,
    ucontext_t* uc_link, void (*function)(), size_t argc, ...)
{
    va_list args;
    size_t r[8];
    size_t i;
    /*printf("1 argc is %d\n", argc);*/
    va_start(args, argc);
    for (i = 0 ; (i != 8) && (i != argc) ; ++i)
    {
        r[i] = va_arg(args, size_t);
    }
    va_end(args);
    function(r[0], r[1], r[2], r[3], r[4], r[5], r[6], r[7]);
    setcontext(uc_link);        
    exit(0); /* exit thread? */
}
#endif

void makecontext(ucontext_t* context, void (*function)(), int argc, ...)
{
    va_list args;
    int i;
    size_t j;
    size_t* stack;

    va_start(args, argc);
    stack = (size_t*)(((size_t)context->uc_stack.ss_sp) + context->uc_stack.ss_size);

#if defined(__APPLE__) && defined(__ppc__)
    /*stack -= 16; experimentally derived */
    /*printf("2 argc is %d\n", argc);*/
    stack -= argc;
    for (i = 0 ; i < argc ; ++i)
    {
        stack[i] = va_arg(args, size_t);
        /* printf("arg is %p\n", (void*)stack[i]); */
    }
    *--stack = (size_t)argc;
    *--stack = (size_t)function;
    *--stack = (size_t)context->uc_link;
    stack -= 14; /* experimentally derived */
    /* printf("1 stack is %p\n", stack); */
    context->uc_mcontext[CONTEXT_PC] = (size_t)internal_setcontext;
#endif

#if defined(__OpenBSD__) && defined(__i386__)

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

    stack -= 1; /* why? */

    context->uc_mcontext[CONTEXT_PC] = (size_t)function;
#endif /* __OpenBSD__ __i386__ */

    context->uc_mcontext[CONTEXT_STACK] = (size_t)stack;

    va_end(args);
}

int swapcontext(ucontext_t* old_context, const ucontext_t* new_context)
{
    getcontext(old_context);
    setcontext(new_context);
    return 0;
}
