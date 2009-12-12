/* Copyright (C) 1994, Digital Equipment Corporation         */
/* All rights reserved.                                      */
/* See the file COPYRIGHT for a full description.            */

/*
Even this approach has problems.
In particular:
 - The functions don't all work on all processors.
 - When they don't work, Win32 does provide at least Increment/Decrement
   that is portable, EXCEPT that their return values vary per OS versions.
*/

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

#ifdef _MSC_VER
#pragma warning(disable:4024) /* volatile mismatch on Interlocked */
#pragma warning(disable:4090) /* volatile mismatch on Interlocked */
#endif

#ifdef _MSC_VER
#pragma optimize("gty", on)
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if _MSC_VER < 1010
#define NAKED __declspec(naked)
#define ASM(x) __asm x
#define ASM2(x, y) __asm x, y
#define INTRINSIC(x)
#else
#define NAKED
#define ASM(x)
#define ASM2(x, y) __asm x, y
#define INTRINSIC(x) x
#endif

#if _MSC_VER >= 1010
long __cdecl _InterlockedIncrement(volatile long* a);
long __cdecl _InterlockedDecrement(volatile long* a);
long __cdecl _InterlockedExchange(volatile long* target, long value);
long __cdecl _InterlockedCompareExchange(volatile long* destination, long exchange, long comparand);
#pragma intrinsic(_InterlockedIncrement)
#pragma intrinsic(_InterlockedDecrement)
#pragma intrinsic(_InterlockedExchange)
#pragma intrinsic(_InterlockedCompareExchange)
#endif

NAKED extern __inline long __cdecl WinBase__InterlockedIncrement(volatile long* a)
{
    INTRINSIC(return _InterlockedIncrement(a);)
#if _MSC_VER < 1010
    __asm
    {
        mov         eax,dword ptr [esp+4]
        mov         ecx,1
        lock xadd   dword ptr [eax],ecx
        inc         ecx
        mov         eax,ecx
        ret
    }
#endif
}

NAKED extern __inline long __cdecl WinBase__InterlockedDecrement(volatile long* a)
{
    INTRINSIC(return _InterlockedDecrement(a);)
#if _MSC_VER < 1010
    __asm
    {
         mov         eax,dword ptr [esp+4]
         or          ecx,0FFFFFFFFh
         lock xadd   dword ptr [eax],ecx
         dec         ecx
         mov         eax,ecx
         ret
    }
#endif
}

NAKED extern __inline long __cdecl WinBase__InterlockedExchange(volatile long* target, long value)
{
    INTRINSIC(return _InterlockedExchange(target, value);)
#if _MSC_VER < 1010
    __asm
    {
        mov         eax,dword ptr [esp+8]
        mov         ecx,dword ptr [esp+4]
        xchg        eax,dword ptr [ecx]
        ret
    }
#endif
}

NAKED extern __inline long __cdecl WinBase__InterlockedCompareExchange(volatile long* destination, long exchange, long comparand)
{
    INTRINSIC(return _InterlockedCompareExchange(destination, exchange, comparand);)
#if _MSC_VER < 1010
    __asm
    {
        mov         ecx,dword ptr [esp+8]
        mov         edx,dword ptr [esp+4]
        mov         eax,dword ptr [esp+0Ch]
        lock cmpxchg dword ptr [edx],ecx
        ret
    }
#endif
}

#ifdef __cplusplus
} /* extern "C" */
#endif
