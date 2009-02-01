#include <stdio.h>
#include <signal.h>
#include <stddef.h>

int main()
{
#if defined(__OpenBSD__) && defined(__i386__)
#define X(x) printf(".set %s, %u\n", #x, offsetof(struct sigcontext, x));
    X(sc_edi)
    X(sc_esi)
    X(sc_ebp)
    X(sc_esp)
    X(sc_ebx)
    X(sc_edx)
    X(sc_ecx)
    X(sc_eax)
    X(sc_eip)
#endif
    return 0;
}
