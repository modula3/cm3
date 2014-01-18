#undef NDEBUG
#include <assert.h>
#include <stddef.h>
#include <windows.h>
extern "C"
{
volatile int expected;

__declspec(noinline) unsigned long __stdcall Thread(PVOID parameter)
{
    while (1)
    {
        expected = 1;
        MemoryBarrier();
        Sleep(1);
        expected = 0;
        MemoryBarrier();
    }
    return 0;
}

int __cdecl main()
{
    HANDLE thread = CreateThread(0, 0, Thread, 0, 0, 0);
    CONTEXT context;
    ZeroMemory(&context, sizeof(context));
    context.ContextFlags = CONTEXT_CONTROL;
    while (1)
    {
        SuspendThread(thread);
        GetThreadContext(thread, &context); // GetThreadContext is necessary to ensure SuspendThread is done
        assert(expected == expected);
        ResumeThread(thread);
    }
}
}
