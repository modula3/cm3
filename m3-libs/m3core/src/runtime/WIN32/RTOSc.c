#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#if defined (_WIN32) || defined (__CYGWIN__)
#include <windows.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

// TODO: Consolidate RTOSs.c
BOOLEAN __cdecl RTOS__Cygwin (void)
{
#ifdef __CYGWIN__
    return TRUE;
#else
    return FALSE;
#endif
}

#if defined (_WIN32) || defined (__CYGWIN__)

#if 0

ADDRESS __cdecl RTOS__GetMemory(INTEGER isize)
{
    WORD_T const Size = (WORD_T)isize; // Modula-3 lacks unsigned types, pass as signed and cast.
    return (ADDRESS)calloc(Size, 1);
}

#elif 0

ADDRESS __cdecl RTOS__GetMemory(INTEGER isize)
{
    WORD_T const Size = (WORD_T)isize; // Modula-3 lacks unsigned types, pass as signed and cast.
    return (ADDRESS)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, Size);
}

#elif 1

ADDRESS __cdecl RTOS__GetMemory(INTEGER isize)
{
    WORD_T const Size = (WORD_T)isize; // Modula-3 lacks unsigned types, pass as signed and cast.
    return (ADDRESS)VirtualAlloc(NULL, Size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
}

#elif 0

/* Same as previous, but with some internal logging, which 
reveals the lossage that results from using VirtualAlloc with RTMachine.PageSize < GetSystemInfo.AllocationGranularity.
*/
typedef struct _RTOSMemoryLogEntry_t
{
    WORD_T Size;
    void* Result;
} RTOSMemoryLogEntry_t;

RTOSMemoryLogEntry_t RTOSMemoryLog[128];
WORD_T RTOSMemoryLogIndex;
#define NUMBER_OF(a) (sizeof(a)/sizeof((a)[0]))

ADDRESS __cdecl RTOS__GetMemory(INTEGER isize)
{
    WORD_T const Size = (WORD_T)isize; // Modula-3 lacks unsigned types, pass as signed and cast.
    RTOSMemoryLogEntry_t LogEntry;
    
    LogEntry.Size = Size;
    LogEntry.Result = VirtualAlloc(NULL, Size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);

    /* It does not matter if this is thread safe or not. */

    RTOSMemoryLog[(RTOSMemoryLogIndex++) % NUMBER_OF(RTOSMemoryLog)] = LogEntry;

    return (ADDRESS)LogEntry.Result;
}

#endif

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
