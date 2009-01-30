/*
see http://www.opengroup.org/onlinepubs/009695399/functions/swapcontext.html
*/
#include "context.h"
#include <stdarg.h>
#include <string.h>
#include <signal.h>

void internal_endcontext();

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

void _internal_getcontext(size_t zero, size_t edi, size_t esi, size_t ebp, size_t esp, size_t ebx, size_t edx, size_t ecx, size_t eax, size_t eip, ucontext_t* context)
{
    internal_getcontext(zero, edi, esi, ebp, esp, ebx, edx, ecx, eax, eip, context);
}

void internal_setcontext(size_t ret, ucontext_t* context)
{
    sigprocmask(SIG_SETMASK, &context->uc_sigmask, NULL);
}

void _internal_setcontext(size_t ret, ucontext_t* context)
{
    internal_setcontext(ret, context);
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
        *--esp = (argc + 1) * sizeof(size_t);

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
