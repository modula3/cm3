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

#include <stdio.h>
#include <signal.h>

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
        mov eax, 0xA1FE
        mov ebx, 0xB21F
        mov ecx, 0xC321
        mov edx, 0xD432
        mov edi, 0xE543
        mov esi, 0xF654
A:      jmp A
    }
    return 0;
}

void SignalHandler(int sig)
{
    printf("SignalHandler pthread_self=%ld sig=%d\n", (long)pthread_self(), sig);
    __debugbreak();
}

int main()
{
    pthread_t t[8];
    int i;
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
