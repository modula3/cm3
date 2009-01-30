/*
see http://www.opengroup.org/onlinepubs/009695399/functions/swapcontext.html
*/
#include "context.h"
#include <stdarg.h>
#include <string.h>

#ifdef _MSC_VER
#pragma warning(disable:4100) /* unreferenced parameter */
#pragma warning(disable:4035) /* no return value */
#pragma warning(disable:4700) /* uninitialized local variable used */
#endif

#ifndef __CYGWIN__
#define sigprocmask(a, b, c) /* nothing */
#define SIG_SETMASK 0
#else
#include <signal.h>
#endif

void
internal_getcontext(
    size_t zero,
    size_t edi,
    size_t esi,
    size_t ebp,
    size_t esp,
    size_t ebx,
    size_t edx,
    size_t ecx,
    size_t eax,
    size_t eip,
    ucontext_t* context)
{
    if (zero)
        memset(context, 0, sizeof(*context));
    esp += 4;
    eax = (size_t)context;
    memcpy(&context->uc_mcontext, &edi, sizeof(context->uc_mcontext));
    sigprocmask(SIG_SETMASK, NULL, &context->uc_sigmask);
}

__declspec(naked)
int getcontext(ucontext_t* context)
{
    __asm
    {
        pushad
        push 1
        call internal_getcontext
        add esp, 36
        mov eax, 0
        ret
    }
}

void internal_setcontext(size_t ret, ucontext_t* context)
{
    sigprocmask(SIG_SETMASK, &context->uc_sigmask, NULL);
}

__declspec(naked)
int setcontext(const ucontext_t* context)
{
    __asm
    {
        call internal_setcontext
        mov eax, DWORD PTR[esp + 4]
        mov esp, eax
        popad
        mov esp, DWORD PTR[eax + 12]
        jmp DWORD PTR[eax + 32]
    }
}

__declspec(naked)
void internal_endcontext()
{
    /* ebp is setup by makecontext */

    int argc;

    argc = (argc + 1) * sizeof(size_t);
    __asm
    {
        add esp, argc
        ret
    }
}

void makecontext(ucontext_t* context, void (*function)(), int argc, ...)
{
    va_list args;
    int i;
    size_t* esp;

    va_start(args, argc);
    esp = (size_t*)(((size_t)context->uc_stack.ss_sp) + context->uc_stack.ss_size);

    /* push a return to setcontext(context->uc_link) */

    *--esp = (size_t)context->uc_link;
    *--esp = context->uc_mcontext.eip;
    *--esp = (size_t)setcontext;

    if (argc != 0)
    {
        /* endeavor to cleanup the parameters by returning
        to internal_endcontext with the frame setup with argc available;
        ebp is non-volatile and function must preserve it */

        context->uc_mcontext.ebp = (size_t)esp;
        *--esp = argc;

        esp -= argc;
        for (i = 0 ; i < argc ; ++i)
        {
            esp[i] = va_arg(args, size_t);
        }
        *--esp = (size_t)internal_endcontext;
    }
    context->uc_mcontext.eip = (size_t)function;
    context->uc_mcontext.esp = (size_t)esp;

    va_end(args);
}

__declspec(naked)
int swapcontext(ucontext_t* old_context, const ucontext_t* new_context)
{
    __asm
    {
        pushad
        push 0
        call internal_getcontext
        add esp, 40
        jmp setcontext
    }
}
