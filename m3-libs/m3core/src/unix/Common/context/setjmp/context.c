/*
see http://www.opengroup.org/onlinepubs/009695399/functions/swapcontext.html
*/
#include "context.h"
#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef __CYGWIN__
#include <windows.h>
#endif

#ifdef __CYGWIN__ /* This isn't going to work anyway. */
#define CONTEXT_PC 8 /* experimentally derived */
#define CONTEXT_STACK 7 /* experimentally derived */
#error unsupported platform
#elif defined(__OpenBSD__) && defined(__i386__)
#define CONTEXT_PC 0 /* experimentally derived */
#define CONTEXT_STACK 2 /* experimentally derived */
#elif defined(__APPLE__) && defined(__ppc__)
#define CONTEXT_PC 21 /* experimentally derived */
#define CONTEXT_STACK 0 /* experimentally derived */
#else
#error unsupported platform
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

/* see:
http://developer.apple.com/documentation/DeveloperTools/Conceptual/LowLevelABI/100-32-bit_PowerPC_Function_Calling_Conventions/32bitPowerPC.html#//apple_ref/doc/uid/TP40002438

From reading the APSL code, setjmp does not preserve the registers that are used for parameters; so burn them up and use
the later stack-based parameters for the actual parameters
*/
void internal_setcontext(
    size_t r3, size_t r4, size_t r5, size_t r6, size_t r7, size_t r8, size_t r9, size_t r10,
    ucontext_t* uc_link, void (*function)(), size_t argc, ...)
{
#if defined(__APPLE__) && defined(__ppc__)
    va_list args;
    size_t i;
    va_start(args, argc);
    for (i = 0 ; (i != 8) && (i != argc) ; ++i)
        (&r3)[i] = va_arg(args, size_t);
    va_end(args);
#endif
    function(r3, r4, r5, r6, r7, r8, r9, r10);
    setcontext(uc_link);        
    exit(0); /* exit thread? */
}

void makecontext(ucontext_t* context, void (*function)(), int argc, ...)
{
    va_list args;
    int i;
    size_t* stack;

    va_start(args, argc);
    stack = (size_t*)(((size_t)context->uc_stack.ss_sp) + context->uc_stack.ss_size);
#if defined(__APPLE__) && defined(__ppc__)
    stack -= argc;
    for (i = 0 ; i < argc ; ++i)
        stack[i] = va_arg(args, size_t);
#endif
    *--stack = (size_t)argc;
    *--stack = (size_t)function;
    *--stack = (size_t)context->uc_link;
#if defined(__APPLE__) && defined(__ppc__)
    stack -= 14; /* experimentally derived */
#endif
#if defined(__OpenBSD__) && defined(__i386__)
    stack -= 8;
    for (i = 0 ; i < argc ; ++i)
        stack[i] = va_arg(args, size_t);
    stack -= 2; /* experimentally derived; 1 for
                   return address, 1 for ? */
#endif
    context->uc_mcontext[CONTEXT_PC] = (size_t)internal_setcontext;
    context->uc_mcontext[CONTEXT_STACK] = (size_t)stack;

    va_end(args);
}

int swapcontext(ucontext_t* old_context, const ucontext_t* new_context)
{
    getcontext(old_context);
    setcontext(new_context);
    return 0;
}
