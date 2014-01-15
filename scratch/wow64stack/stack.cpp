#undef NDEBUG
#ifndef _M_IX86
#error Only valid for x86
#endif
#include <assert.h>
#include <stddef.h>
#include <windows.h>
void* _AddressOfReturnAddress(void);
volatile size_t expected;

#define PAGE 4096

__declspec(noinline) void X(void)
{
  expected = (size_t)_AddressOfReturnAddress();
  Sleep(1);
  expected = 0;
}

__declspec(noinline) void F1(void) { volatile char a[PAGE * 8]; X(); }
__declspec(noinline) void F2(void) { volatile char a[PAGE * 4]; X(); }

__declspec(noinline) unsigned long __stdcall Thread(PVOID parameter)
{
  while (1)
  {
    F1();
    F2();
  }
  return 0;
}

int main()
{
  UINT64 volatile count = 0;
  HANDLE thread = CreateThread(0, 0, Thread, 0, 0, 0);
  CONTEXT context;
  ZeroMemory(&context, sizeof(context));
  context.ContextFlags = CONTEXT_CONTROL;
  while (count++ < 100000)
  {
    SuspendThread(thread);
    GetThreadContext(thread, &context);
    assert(expected == 0 || (context.Esp && context.Esp < expected));
    //printf("%p ", (void*)context.Esp);
    ResumeThread(thread);
  }
}
