#include <windows.h>

#ifdef __cplusplus
extern "C" {
#endif

LONG
__stdcall
WinBase__InterlockedIncrement(volatile LONG* a)
{
  return InterlockedIncrement(a);
}

LONG
__stdcall
WinBase__InterlockedDecrement(volatile LONG* a)
{
  return InterlockedDecrement(a);
}

LONG
__stdcall
WinBase__InterlockedCompareExchange(volatile LONG* a, LONG b, LONG c)
{
  return InterlockedCompareExchange(a, b, c);
}

PVOID
__stdcall
WinBase__InterlockedCompareExchangePointer(volatile PVOID* a, PVOID b, PVOID c)
{
  return InterlockedCompareExchangePointer(a, b, c);
}

PVOID
__stdcall
WinBase__InterlockedCompareExchange64(volatile INT64* a, INT64 b, INT64 c)
{
  return InterlockedCompareExchangePointer(a, b, c);
}

LONG
__stdcall
WinBase__InterlockedExchange(volatile LONG* a, LONG b)
{
  return InterlockedExchange(a, b);
}

PVOID
__stdcall
WinBase__InterlockedExchangePointer(volatile PVOID* a, PVOID b)
{
  return InterlockedExchangePointer(a, b);
}

LONG
__stdcall
WinBase__InterlockedExchangeAdd(volatile LONG* a, LONG b)
{
  return InterlockedExchangeAdd(a, b);
}

INT64
__stdcall
WinBase__InterlockedExchangeAdd64(volatile INT64* a, INT64 b)
{
  return InterlockedExchangeAdd64(a, b);
}

#ifdef __cplusplus
}
#endif
