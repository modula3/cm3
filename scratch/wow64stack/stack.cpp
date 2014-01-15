#undef NDEBUG
#ifndef _M_IX86
#error Only valid for x86
#endif
#include <set>
#include <assert.h>
#include <stddef.h>
#include <windows.h>
void* _AddressOfReturnAddress(void);
volatile size_t expected;
#pragma optimize("", off)
#define PAGE 4096

__declspec(noinline) void X(void *)
{
  expected = (size_t)_AddressOfReturnAddress();
  Sleep(1);
  expected = 0;
}

__declspec(noinline) void F1(void) { X(_alloca(PAGE * 4)); }
__declspec(noinline) void F2(void) { X(_alloca(PAGE * 8)); }

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
  std::set<size_t> stacks;
  //while (count++ < 100000)
  while (1)
  {
    if (SuspendThread(thread) == (DWORD)-1)
    {
      Sleep(1);
      continue;
    }
    GetThreadContext(thread, &context);
    assert(expected == 0 || (context.Esp && context.Esp < expected));
    if (stacks.find(context.Esp) == stacks.end())
    {
      stacks.insert(stacks.end(), context.Esp);
      printf("%p %p\n", (void*)expected, (void*)context.Esp);
    }
    ResumeThread(thread);
  }
}
