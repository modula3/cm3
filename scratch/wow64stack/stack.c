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
  unsigned __int64 count = 0;
  HANDLE thread = CreateThread(0, 0, Thread, 0, 0, 0);
  CONTEXT context;
  ZeroMemory(&context, sizeof(context));
  context.ContextFlags = CONTEXT_CONTROL;
  while (count++ < 1000)
  {
    SuspendThread(thread);
    GetThreadContext(thread, &context);
    assert(expected == 0 || context.Esp < expected);
    ResumeThread(thread);
  }
}
