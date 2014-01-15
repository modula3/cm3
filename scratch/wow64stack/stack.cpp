#undef NDEBUG
#include <set>
#include <assert.h>
#include <stddef.h>
#include <windows.h>
extern "C"
{
void* __cdecl _AddressOfReturnAddress(void);
#pragma optimize("", off)
#define PAGE 4096
#ifdef _M_IX86
#define Stack Esp
#else
#define Stack Rsp
#endif
size_t expected;

__declspec(noinline) void write(size_t value)
{
  //MemoryBarrier();
  expected = value;
  MemoryBarrier();
}

__declspec(noinline) size_t read()
{
  MemoryBarrier();
  size_t value = expected;
  //MemoryBarrier();
  return value;
}

__declspec(noinline) void X(void *)
{
  write((size_t)_AddressOfReturnAddress());
  MemoryBarrier();
  Sleep(1);
  write(0);
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
    if (read() == 0)
    {
      ResumeThread(thread);
      continue;
    }
    assert(read());
    GetThreadContext(thread, &context);
    assert(read());
    bool print = false;
    if (stacks.find(context.Stack) == stacks.end())
    {
      stacks.insert(stacks.end(), context.Stack);
      print = true;
    }
    print |= !(context.Stack && context.Stack < read());
    if (print)
    {
      printf("expected:%p stack:%p\n", (void*)read(), (void*)context.Stack);
      fflush(stdout);
    }
    assert(read());
    assert(context.Stack && context.Stack < read());
    ResumeThread(thread);
  }
}
}
