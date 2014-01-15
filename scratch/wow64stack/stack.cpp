#undef NDEBUG
#include <set>
#include <assert.h>
#include <stddef.h>
#include <windows.h>
extern "C" {
void* __cdecl _AddressOfReturnAddress(void);
}
#pragma optimize("", off)
#define PAGE 4096
#ifdef _M_IX86
#define Stack Esp
#else
#define Stack Rsp
#endif
size_t expected;

__declspec(noinline) void X(void *)
{
  MemoryBarrier(); // why?
  expected = (size_t)_AddressOfReturnAddress();
  MemoryBarrier();
  Sleep(1);
  MemoryBarrier(); // why?
  expected = 0;
  MemoryBarrier();
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

int __cdecl main()
{
  HANDLE thread = CreateThread(0, 0, Thread, 0, 0, 0);
  CONTEXT context;
  ZeroMemory(&context, sizeof(context));
  context.ContextFlags = CONTEXT_CONTROL;
  std::set<size_t> stacks;
  while (1)
  {
    if (SuspendThread(thread) == (DWORD)-1)
    {
      Sleep(1);
      continue;
    }
    MemoryBarrier();
    if (expected == 0)
    {
      ResumeThread(thread);
      continue;
    }
    assert(expected);
    GetThreadContext(thread, &context);
    //MemoryBarrier(); // why?
    assert(expected);
    bool print = false;
    if (stacks.find(context.Stack) == stacks.end())
    {
      stacks.insert(stacks.end(), context.Stack);
      print = true;
    }
    print |= !(context.Stack && context.Stack < expected);
    if (print)
    {
      printf("expected:%p stack:%p\n", (void*)expected, (void*)context.Stack);
      fflush(stdout);
    }
    assert(expected);
    assert(context.Stack && context.Stack < expected);
    ResumeThread(thread);
  }
}
