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
volatile size_t expected;

#if 0

__declspec(noinline) void write(size_t value)
{
#ifdef _WIN64
  InterlockedExchange64((INT64*)&expected, (INT64)value);
#else
  InterlockedExchange((long*)&expected, (long)value);
#endif
}

__declspec(noinline) size_t read()
{
#ifdef _WIN64
  return (size_t)InterlockedExchange64((INT64*)&expected, (INT64)expected);
#else
  return (size_t)InterlockedExchange((long*)&expected, (long)expected);
#endif
}

#else

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
#endif

__declspec(noinline) void X(void *)
{
  write((size_t)_AddressOfReturnAddress());
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
    assert(read());
    if (stacks.find(context.Stack) == stacks.end())
    {
      stacks.insert(stacks.end(), context.Stack);
      print = true;
    }
    assert(read());
    print |= !(context.Stack && context.Stack < expected);
    assert(read());
    if (print)
    {
      printf("expected:%p stack:%p\n", (void*)expected, (void*)context.Stack);
      fflush(stdout);
    }
    assert(read());
    assert(context.Stack && context.Stack < expected);
    ResumeThread(thread);
  }
}
