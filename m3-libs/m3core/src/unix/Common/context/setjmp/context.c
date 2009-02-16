/*
see http://www.opengroup.org/onlinepubs/009695399/functions/swapcontext.html
*/
#include "context.h"
#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#ifdef __CYGWIN__
#include <windows.h>
#endif

#ifdef __OpenBSD__
#ifdef __i386__
#define OpenBSD_i386
#elif defined(__powerpc__)
#define OpenBSD_powerpc
#else
#error unsupported platform
#endif
#elif defined(__APPLE__)
#ifdef __ppc__
#define Apple_ppc
#else
#error unsupported platform
#endif
#else
#error unsupported platform
#endif

#ifdef __CYGWIN__ /* This isn't going to work anyway. */
#define CONTEXT_PC 8 /* experimentally derived */
#define CONTEXT_STACK 7 /* experimentally derived */
#elif defined(OpenBSD_i386)
#define CONTEXT_PC 0 /* experimentally derived */
#define CONTEXT_STACK 2 /* experimentally derived */
#elif defined(OpenBSD_powerpc)
#define CONTEXT_PC 20 /* experimentally derived */
#define CONTEXT_STACK 1 /* experimentall derived */
#elif defined(Apple_ppc)
#define CONTEXT_PC 21 /* experimentally derived */
#define CONTEXT_STACK 0 /* experimentally derived */
#endif

#if 0
static void print_context(const char* name, const ucontext_t* context)
{
    size_t i;

    printf("%s:%p: ", name, context);
    if (context)
    {
        printf(" st:%p pc:%pc ", context->uc_mcontext.a[CONTEXT_STACK], context->uc_mcontext.a[CONTEXT_PC]);
        for (i = 0 ; i != sizeof(jmp_buf) / sizeof(void*) ; ++i)
        {
            if (context->uc_mcontext[i])
                printf("%p ", context->uc_mcontext.a[i]);
        }
    }
    printf("\n");
    fflush(stdout);
}
#endif

int getcontext(ucontext_t* context)
{
    jmp_buf jb;
    sigprocmask(SIG_SETMASK, NULL, &context->uc_sigmask);
    setjmp(context->uc_mcontext.jb);
    return 0;
}

int setcontext(const ucontext_t* const_context)
{
    ucontext_t* context = (ucontext_t*)const_context;
    if (context)
    {
        sigprocmask(SIG_SETMASK, &context->uc_sigmask, NULL);
        longjmp(context->uc_mcontext.jb, 1);
    }
    return 0;
}

/* see:
http://developer.apple.com/documentation/DeveloperTools/Conceptual/LowLevelABI/100-32-bit_PowerPC_Function_Calling_Conventions/32bitPowerPC.html#//apple_ref/doc/uid/TP40002438

From reading the APSL code, setjmp does not preserve the registers that are used for parameters; so burn them up and use
the later stack-based parameters for the actual parameters

OpenBSD/powerpc also does not preserve a bunch of registers in the
  setjmp code, but the prolog/epilog appears to handle them.
*/
void internal_setcontext(
    size_t r3,
    size_t r4,
    size_t r5,
    size_t r6,
    size_t r7,
    size_t r8,
    size_t r9,
    size_t r10,
    size_t marker,
    ucontext_t* uc_link,
    void (*function)(),
    size_t argc,
    ...)
{
#if defined(Apple_ppc) || defined(OpenBSD_powerpc)
    va_list args;
    size_t i;
    va_start(args, argc);
    for (i = 0 ; (i != 8) && (i != argc) ; ++i)
        (&r3)[i] = va_arg(args, size_t);
    va_end(args);
#endif
#if 0 /* fishing mode */
    printf("marker is %lx\n", (unsigned long)marker);
    exit(1);
#endif
    assert(marker == (0x12345678 << 8));
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
#if defined(Apple_ppc) || defined(OpenBSD_powerpc)
    stack -= argc;
    for (i = 0 ; i < argc ; ++i)
        stack[i] = va_arg(args, size_t);
#endif
    *--stack = (size_t)argc;
    *--stack = (size_t)function;
    *--stack = (size_t)context->uc_link;
    *--stack = 0x12345678 << 8; /* marker */
#if 0 /* fish around for the stack layout */
    for (i = 0 ; i != 255 ; ++i)
    {
        *--stack = (i << 16) | ((i + 1) << 8) | (i + 2);
    }
#endif
#ifdef OpenBSD_powerpc
    stack -= 2; /* experimentally derived */
#endif
#ifdef Apple_ppc
    stack -= 14; /* experimentally derived */
#endif
#ifdef OpenBSD_i386
    stack -= 8;
    for (i = 0 ; i < argc ; ++i)
        stack[i] = va_arg(args, size_t);
    stack -= 2; /* experimentally derived; 1 for
                   return address, 1 for ? */
#endif
    context->uc_mcontext.a[CONTEXT_PC] = (size_t)internal_setcontext;
    context->uc_mcontext.a[CONTEXT_STACK] = (size_t)stack;

    va_end(args);
}

int swapcontext(ucontext_t* old_context, const ucontext_t* new_context)
{
    getcontext(old_context);
    setcontext(new_context);
    return 0;
}
