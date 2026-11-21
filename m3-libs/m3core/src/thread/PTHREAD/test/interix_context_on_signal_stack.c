/* let's fish around and see if context is on the stack */

/* Compile this with cc instead of gcc
 * Registers are referenced in the order
 * they occur in CONTEXT for easier
 * matching up the stack in the debugger
 * with the values in the code.
 *
 * open two windows, interix and cmd.
 * interix: cc -g interix_context_on_signal_stack.c
 * interix: ./a.out
 * cmd: cdb -pn a.out
 * cdb: g
 * interix: press enter
 * cdb will break in
 * cdb: dps @esp
 * cdb: dps a few times
 * You'll see the context on the stack,
 * including the debugging identifying EIP.
 * cdb: g
 * will break in another thread, repeat
 * until convinced
 */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

typedef unsigned long DWORD;
typedef unsigned char BYTE;

#define SIZE_OF_80387_REGISTERS      80

#define CONTEXT_i386    0x00010000    // this assumes that i386 and
#define CONTEXT_CONTROL         (CONTEXT_i386 | 0x00000001L) // SS:SP, CS:IP, FLAGS, BP
#define CONTEXT_INTEGER         (CONTEXT_i386 | 0x00000002L) // AX, BX, CX, DX, SI, DI
#define CONTEXT_SEGMENTS        (CONTEXT_i386 | 0x00000004L) // DS, ES, FS, GS
#define CONTEXT_FLOATING_POINT  (CONTEXT_i386 | 0x00000008L) // 387 state
#define CONTEXT_DEBUG_REGISTERS (CONTEXT_i386 | 0x00000010L) // DB 0-3,6,7

#define CONTEXT_FULL (CONTEXT_CONTROL | CONTEXT_INTEGER |\
                      CONTEXT_SEGMENTS)

typedef struct _FLOATING_SAVE_AREA {
    DWORD   ControlWord;
    DWORD   StatusWord;
    DWORD   TagWord;
    DWORD   ErrorOffset;
    DWORD   ErrorSelector;
    DWORD   DataOffset;
    DWORD   DataSelector;
    BYTE    RegisterArea[SIZE_OF_80387_REGISTERS];
    DWORD   Cr0NpxState;
} FLOATING_SAVE_AREA;

typedef FLOATING_SAVE_AREA *PFLOATING_SAVE_AREA;

typedef struct _CONTEXT {
    DWORD ContextFlags;
    DWORD   Dr0;
    DWORD   Dr1;
    DWORD   Dr2;
    DWORD   Dr3;
    DWORD   Dr6;
    DWORD   Dr7;
    FLOATING_SAVE_AREA FloatSave;
    DWORD   SegGs;
    DWORD   SegFs;
    DWORD   SegEs;
    DWORD   SegDs;
    DWORD   Edi;
    DWORD   Esi;
    DWORD   Ebx;
    DWORD   Edx;
    DWORD   Ecx;
    DWORD   Eax;
    DWORD   Ebp;
    DWORD   Eip;
    DWORD   SegCs;
    DWORD   EFlags;
    DWORD   Esp;
    DWORD   SegSs;
} CONTEXT;

void* thread(void* a)
{
    __asm
    {
        mov edi, a
        mov esi, edi
        inc esi
        mov ebx, esi
        inc ebx
        mov edx, ebx
        inc edx
        mov ecx, edx
        inc ecx
        mov eax, ecx
        inc eax
A:      jmp A
    }
}

void* thread1(void*a)
{
    __asm
    {
        mov edi, 0x1
        mov esi, 0x2
        mov ebx, 0x3
        mov edx, 0x4
        mov ecx, 0x5
        mov eax, 0x6
A:      jmp A
    }
    return 0;
}

void* thread2(void*a)
{
    __asm
    {
        mov edi, 0x10
        mov esi, 0x20
        mov ebx, 0x30
        mov edx, 0x40
        mov ecx, 0x50
        mov eax, 0x60
A:      jmp A
    }
    return 0;
}

void* thread3(void*a)
{
    __asm
    {
        mov edi, 0x100
        mov esi, 0x200
        mov ebx, 0x300
        mov edx, 0x400
        mov ecx, 0x500
        mov eax, 0x600
A:      jmp A
    }
    return 0;
}

void* thread4(void*a)
{
    __asm
    {
        mov edi, 0x1FE
        mov esi, 0x21F
        mov ebx, 0x321
        mov edx, 0x432
        mov ecx, 0x543
        mov eax, 0x654
A:      jmp A
    }
    return 0;
}

void* thread5(void*a)
{
    __asm
    {
        mov edi, 0xA1FE
        mov esi, 0xB21F
        mov ebx, 0xC321
        mov edx, 0xD432
        mov ecx, 0xE543
        mov eax, 0xF654
A:      jmp A
    }
    return 0;
}

void SignalHandler(int sig)
{
    CONTEXT context = { 0 };
    CONTEXT* pcontext = { 0 };

    memset(&context, 0, sizeof(context));
    context.ContextFlags = CONTEXT_i386 | CONTEXT_CONTROL | CONTEXT_INTEGER | CONTEXT_SEGMENTS;

    __asm
    {
        xor eax, eax
        mov ax, cs
        mov context.SegCs, eax
        mov ax, ds
        mov context.SegDs, eax
        mov ax, es
        mov context.SegEs, eax
        mov ax, fs
        mov context.SegFs, eax
        mov ax, gs
        mov context.SegGs, eax
        mov ax, ss
        mov context.SegSs, eax
    }

    pcontext = &context + 1;
    while (1)
    {
        if (   pcontext->SegCs == context.SegCs
            && pcontext->SegDs == context.SegDs
            && pcontext->SegEs == context.SegEs
            && pcontext->SegFs == context.SegFs
            && pcontext->SegGs == context.SegGs
            && pcontext->SegSs == context.SegSs
            && pcontext->ContextFlags == context.ContextFlags)
        {
            printf("thread %ld context search from @%p found at @%p %x %x %x %x %x %x %x %x %x\n",
                    pthread_self(),
                    &context,
                    pcontext,
                    pcontext->Edi, pcontext->Esi, pcontext->Ebx, pcontext->Edx,
                    pcontext->Ecx, pcontext->Eax, pcontext->Ebp, pcontext->Eip,
                    pcontext->Esp);
            break;
        }
        pcontext = (CONTEXT*)(1 + (DWORD*)pcontext);
    }
}

int main()
{
    pthread_t t[8] = { 0 };
    int i = { 0 };

    getchar();

    signal(SIGUSR1, SignalHandler);
    pthread_create(&t[0], NULL, thread1, 0);
    pthread_create(&t[1], NULL, thread2, 0);
    pthread_create(&t[2], NULL, thread3, 0);
    pthread_create(&t[3], NULL, thread4, 0);
    pthread_create(&t[4], NULL, thread, 0);
    pthread_create(&t[5], NULL, thread, (void*)(size_t)0x100);
    pthread_create(&t[6], NULL, thread, (void*)(size_t)0x12345678);
    pthread_create(&t[7], NULL, thread, (void*)(size_t)0xABCDEF);
    sleep(1);

    for (i = 0; i < 8; ++i)
        pthread_kill(t[i], SIGUSR1);

    getchar();

    return 0;
}
