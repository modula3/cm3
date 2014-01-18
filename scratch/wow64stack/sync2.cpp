#include <stdio.h>
#include <windows.h>

volatile long value;

unsigned long __stdcall Thread(PVOID parameter)
{
    while (1)
        InterlockedIncrement(&value);
    return 0;
}

int __cdecl main()
{
    CONTEXT context;
    ZeroMemory(&context, sizeof(context));
    context.ContextFlags = CONTEXT_INTEGER;
    HANDLE thread = CreateThread(0, 0, Thread, 0, 0, 0);
    UINT i = 0;
    while (1)
    {
        i += 1;
        if (SuspendThread(thread) == (DWORD)-1)
        {
            printf("suspend failed %X\n", GetLastError());
            Sleep(1);
            continue;
        }
        // Microsoft Rotor/sscli 2.0: Threads continue running
        // after SuspendThread, but not if you follow that with GetThreadContext.
        GetThreadContext(thread, &context);
        volatile long a = value; 
        volatile long b = value;
        if (a != b)
        {
            printf("%d %d %d %d\n", i, a, b, b - a);
        }
        ResumeThread(thread);
    }
}
