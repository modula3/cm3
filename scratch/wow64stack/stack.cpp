#undef NDEBUG
#include <set>
#include <assert.h>
#include <stddef.h>
#include <windows.h>
void* _AddressOfReturnAddress(void);
volatile size_t expected;
#pragma optimize("", off)
#define PAGE 4096
#ifdef _M_IX86
#define Stack Esp
#else
#define Stack Rsp
#endif

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
    if (execpted == 0)
    {
      ResumeThread(thread);
      continue;
    }
    GetThreadContext(thread, &context);
    if (stacks.find(context.Stack) == stacks.end())
    {
      stacks.insert(stacks.end(), context.Stack);
      printf("%p %p\n", (void*)expected, (void*)context.Stack);
    }
    assert(context.Stack && context.Stack < expected);
    ResumeThread(thread);
  }
}
