#ifdef _MSC_VER
#undef _DLL
#endif

#include <stddef.h>
#include <stdlib.h>
#include <windows.h>

/* WORD_T/INTEGER are always exactly the same size as a pointer.
 * VMS sometimes has 32bit size_t/ptrdiff_t but 64bit pointers.
 */
#if __INITIAL_POINTER_SIZE == 64
typedef unsigned __int64 WORD_T;
#else
typedef size_t WORD_T;
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if 0

void* __cdecl RTOS__GetMemory(WORD_T Size)
{
    return calloc(Size, 1);
}

#elif 0

void* __cdecl RTOS__GetMemory(WORD_T Size)
{
    return HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, Size);
}

#elif 1

void* __cdecl RTOS__GetMemory(WORD_T Size)
{
    return VirtualAlloc(NULL, Size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
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

void* __cdecl RTOS__GetMemory(WORD_T Size)
{
    RTOSMemoryLogEntry_t LogEntry;
    
    LogEntry.Size = Size;
    LogEntry.Result = VirtualAlloc(NULL, Size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);

    /* It does not matter if this is thread safe or not. */

    RTOSMemoryLog[(RTOSMemoryLogIndex++) % NUMBER_OF(RTOSMemoryLog)] = LogEntry;

    return LogEntry.Result;
}

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
