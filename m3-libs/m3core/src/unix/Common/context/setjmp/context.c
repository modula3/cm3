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

#ifdef __cplusplus
extern "C"
{
#endif

#define MARKER_R10      ((unsigned long)'R' | 0x12345600)
#define MARKER_UC_LINK  ((unsigned long)'U' | 0x81234500)
#define MARKER_FUNCTION ((unsigned long)'F' | 0x78123400)
#define MARKER_ARGC     ((unsigned long)'A' | 0x67812300)

#ifdef __OpenBSD__
#ifdef __i386__
#define OpenBSD_i386
#elif defined(__powerpc__)
#define OpenBSD_powerpc
#elif defined(__sparc64__)
#define OpenBSD_sparc64
#else
#error unsupported OpenBSD platform
#endif
#elif defined(__APPLE__)
#ifdef __ppc__
#define Apple_ppc
#else
#error unsupported Apple platform
#endif
#else
#error unsupported operating system
#endif

/* The various numbers are experimentally derived or based
 * on reading (assembly) source or stepping through with
 * the debugger, etc.
 */
#ifdef __CYGWIN__           /* Cygwin isn't going to work anyway. */
#define CONTEXT_PC      8
#define CONTEXT_STACK   7
#elif defined(OpenBSD_i386)
#define CONTEXT_PC      0
#define CONTEXT_STACK   2
#define STACK_ADJUST    40
#elif defined(OpenBSD_powerpc)
#define CONTEXT_PC      20
#define CONTEXT_STACK   1
#define STACK_ADJUST    4
#elif defined(Apple_ppc)
#define CONTEXT_PC      21
#define CONTEXT_STACK   0
#define STACK_ADJUST    (14 * 4)
#elif defined(OpenBSD_sparc64)
#define CONTEXT_STACK    1
#define CONTEXT_PC       2
/*define CONTEXT_FRAME   5*/
#define STACK_ADJUST     2239
#endif

/* This must be a macro and is even needed on x86, else -O3 break us.
 * We don't actually need the longjmp.
 */
#define FlushRegistersAndOrDeoptimize() do { sigjmp_buf jb; if (sigsetjmp(jb, 0) == 0) siglongjmp(jb, 1); } while(0)

#if 0
static void print_context(const char* name, const ucontext_t* context)
{
    size_t i;

    printf("%s:%p: ", name, context);
    if (context)
    {
        printf(" st:%p pc:%pc ", context->uc_mcontext.a[CONTEXT_STACK], context->uc_mcontext.a[CONTEXT_PC]);
        for (i = 0; i < (sizeof(sigjmp_buf) / sizeof(void*)); ++i)
        {
            if (context->uc_mcontext[i])
                printf("%p ", context->uc_mcontext.a[i]);
        }
    }
    printf("\n");
    fflush(stdout);
}
#endif

int getcontext(ucontext_t* xcontext)
{
    ucontext_t* volatile context = xcontext;
#ifdef OpenBSD_powerpc
    volatile char a[65]; /* This does make a positive difference, but breaks others. */
#endif
    FlushRegistersAndOrDeoptimize();
    if (sigsetjmp(context->uc_mcontext.jb, 1) == 0)
        context->uc_stack.ss_sp = (void*)context->uc_mcontext.a[CONTEXT_STACK];
    return 0;
}

int setcontext(const ucontext_t* const_context)
{
    ucontext_t* volatile context = (ucontext_t*)const_context;
    if (context)
    {
        FlushRegistersAndOrDeoptimize();
        siglongjmp(context->uc_mcontext.jb, 1);
    }
    return 0;
}

/* see:
http://developer.apple.com/documentation/DeveloperTools/Conceptual/LowLevelABI/100-32-bit_PowerPC_Function_Calling_Conventions/32bitPowerPC.html#//apple_ref/doc/uid/TP40002438

From reading the APSL code, sigsetjmp does not preserve the registers that are used for parameters; so burn them up and use
the later stack-based parameters for the actual parameters

OpenBSD/powerpc also does not preserve a bunch of registers in the
  setjmp code, but the prolog/epilog appears to handle them.
*/
static void internal_setcontext(
    /* burn up parameters that would go in registers */
    size_t r3,
    size_t r4,
    size_t r5,
    size_t r6,
    size_t r7,
    size_t r8,
    size_t r9,
    size_t r10,
    unsigned long markerR10,
    ucontext_t* uc_link,
    unsigned long markerUcLink,
    void (*function)(),
    unsigned long markerFunction,
    size_t argc,
    unsigned long markerArgc,
    ...)
{
#define CHECK_MARKER(a,A) do { if (a != A) fprintf(stderr, #a " is %lx instead of %lx\n", a, A); assert(a == A); } while(0)
    FlushRegistersAndOrDeoptimize();
    CHECK_MARKER(markerR10,      MARKER_R10);
    CHECK_MARKER(markerUcLink,   MARKER_UC_LINK);
    CHECK_MARKER(markerFunction, MARKER_FUNCTION);
    CHECK_MARKER(markerArgc,     MARKER_ARGC);
    {
        va_list args;
        size_t i;

        va_start(args, markerArgc);
        for (i = 0; i < 8 && i < argc; ++i)
            (&r3)[i] = va_arg(args, size_t);
        va_end(args);

#if 0 /* fishing mode */
        printf("markerR10 at %p\n", &markerR10);
        printf("markerUcLink at %p\n", &markerUcLink);
        printf("marker is %lx\n", (unsigned long)marker2);
        exit(1);
#endif
        function(r3, r4, r5, r6, r7, r8, r9, r10);
        setcontext(uc_link);        
        exit(0); /* exit thread? */
    }
}

void makecontext(ucontext_t* context, void (*function)(), int argc, ...)
{
    va_list args;
    int i;
    size_t* stack;

    FlushRegistersAndOrDeoptimize();
    va_start(args, argc);
    stack = (size_t*)(((size_t)context->uc_stack.ss_sp) + context->uc_stack.ss_size);
    stack -= argc;
    for (i = 0; i < argc; ++i)
        stack[i] = va_arg(args, size_t);
    *--stack = MARKER_ARGC;
    *--stack = (size_t)argc;
    *--stack = MARKER_FUNCTION;
    *--stack = (size_t)function;
    *--stack = MARKER_UC_LINK;
    *--stack = (size_t)context->uc_link;
    *--stack = MARKER_R10;
#if 0 /* fish around for the stack layout */
    printf("markers at %p\n", stack);
    for (i = 0; i < 255; ++i)
    {
        *--stack = (i << 16) | ((i + 1) << 8) | (i + 2);
    }
#endif
    context->uc_mcontext.a[CONTEXT_PC] = (size_t)&internal_setcontext;
#ifdef OpenBSD_sparc64
    context->uc_mcontext.a[CONTEXT_PC + 1] = 4 + (size_t)&internal_setcontext;
#endif
    context->uc_mcontext.a[CONTEXT_STACK] = ((size_t)stack) - STACK_ADJUST;
    va_end(args);
}

int swapcontext(ucontext_t* old_context, const ucontext_t* new_context)
{
    FlushRegistersAndOrDeoptimize();
    getcontext(old_context);
    setcontext(new_context);
    return 0;
}

#ifdef __cplusplus
} /* extern "C" */
#endif
