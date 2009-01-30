/*
see http://www.opengroup.org/onlinepubs/009695399/functions/swapcontext.html
*/
#include "context.h"
#include <stdarg.h>
#include <string.h>

#ifdef _MSC_VER
#pragma warning(disable:4100) /* unreferenced parameter */
#pragma warning(disable:4035) /* no return value */
#endif

#ifndef __CYGWIN__
#define sigprocmask(a, b, c) /* nothing */
#define SIG_SETMASK 0
#else
#include <signal.h>
#endif

void
internal_getcontext(
    size_t ebp,
    size_t ebx,
    size_t edi,
    size_t esi,
    size_t eip,
    ucontext_t* context)
{
    memset(context, 0, sizeof(*context));
    context->uc_mcontext.ebp = ebp;
    context->uc_mcontext.ebx = ebx;
    context->uc_mcontext.edi = edi;
    context->uc_mcontext.esi = esi;
    context->uc_mcontext.eip = eip;
    context->uc_mcontext.esp = (size_t*)&context;
    sigprocmask(SIG_SETMASK, NULL, &context->uc_sigmask);
}

__declspec(naked)
int getcontext(ucontext_t* context)
{
    __asm
    {
        push esi
        push edi
        push ebx
        push ebp
        call internal_getcontext
        add esp, 16
        mov eax, 0
        ret
    }
}

void* internal_setcontext(size_t ret, const ucontext_t* const_context)
{
    ucontext_t* context = (ucontext_t*)const_context;
    sigprocmask(SIG_SETMASK, &context->uc_sigmask, NULL);
    return context;
}

__declspec(naked)
int setcontext(const ucontext_t* const_context)
{
    __asm
    {
        call internal_setcontext
        mov esi, DWORD PTR[eax]
        mov edi, DWORD PTR[eax + 4]
        mov ebx, DWORD PTR[eax + 8]
        mov ebp, DWORD PTR[eax + 12]
        mov esp, DWORD PTR[eax + 16]
        jmp DWORD PTR[eax + 20]
    }
}

__declspec(naked)
void internal_endcontext()
{
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

    va_start(args, argc);
    context->uc_mcontext.esp = (size_t*)(((size_t)context->uc_stack.ss_sp) + context->uc_stack.ss_size);

    /* push a return or call to setcontext(context->uc_link) */

    *--context->uc_mcontext.esp = (size_t)context->uc_link;
    *--context->uc_mcontext.esp = (size_t)context->uc_mcontext.eip;
    *--context->uc_mcontext.esp = (size_t)setcontext;

    if (argc != 0)
    {
        /* endeavor to cleanup the parameters  */

        context->uc_mcontext.ebp = (size_t)context->uc_mcontext.esp;
        *--context->uc_mcontext.esp = argc;

        context->uc_mcontext.esp -= argc;
        for (i = 0 ; i < argc ; ++i)
        {
            context->uc_mcontext.esp[i] = va_arg(args, size_t);
        }
        *--context->uc_mcontext.esp = (size_t)internal_endcontext;
    }
    context->uc_mcontext.eip = (size_t)function;

    va_end(args);
}

__declspec(naked)
int swapcontext(ucontext_t* old_context, const ucontext_t* new_context)
{
    __asm
    {
        /* it would be nice if we could avoid this duplication */

        push esi
        push edi
        push ebx
        push ebp
        call internal_getcontext
        add esp, 20 /* pop internal_getcontext parameters and old_context */
        jmp setcontext
    }
}
