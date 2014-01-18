#undef NDEBUG
#include <set>
#include <assert.h>
#include <stddef.h>
#include <windows.h>
extern "C"
{
void* __cdecl _AddressOfReturnAddress(void);
#pragma optimize("", off)
#ifdef _M_IX86
#define Stack Esp
#else
#define Stack Rsp
#endif
volatile size_t expected;

__declspec(noinline) void X(void *)
{
    expected = (size_t)_AddressOfReturnAddress();
    MemoryBarrier();
    Sleep(1);
    expected = 0;
    MemoryBarrier();
}

__declspec(noinline) void F1(void) { X(_alloca(1000)); }
__declspec(noinline) void F2(void) { X(_alloca(100000)); }

__declspec(noinline) unsigned long __stdcall Thread(PVOID parameter)
{
    while (1)
    {
        F1();
        F2();
    }
    return 0;
}

int __cdecl main()
{
    HANDLE thread = CreateThread(0, 0, Thread, 0, 0, 0);
    CONTEXT context;
    ZeroMemory(&context, sizeof(context));
    context.ContextFlags = CONTEXT_CONTROL;
    std::set<size_t> stacks;
    UINT i = 0;
    while (1)
    {
        i += 1;
        SuspendThread(thread);
        GetThreadContext(thread, &context); // GetThreadContext is necessary to ensure SuspendThread is done
        if (expected)
        {
            bool print = false;
            if (stacks.find(context.Stack) == stacks.end())
            {
                stacks.insert(stacks.end(), context.Stack);
                print = true;
            }
            print |= !(context.Stack && context.Stack <= expected);
            if (print)
            {
                printf("expected:%p stack:%p i:%u\n", (void*)expected, (void*)context.Stack, i);
                fflush(stdout);
            }
            assert(expected && context.Stack && context.Stack <= expected);
        }
        ResumeThread(thread);
    }
}
}
