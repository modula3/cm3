#ifdef _MSC_VER
#pragma optimize("gt", on)
#pragma optimize("y", off)
#undef _DLL
#endif

#include <stdlib.h>
#include <windows.h>

#ifdef __cplusplus
extern "C" {
#endif

#if 0

void* __cdecl RTOS__GetMemory(size_t Size)
{
    return calloc(Size, 1);
}

#elif 0

void* __cdecl RTOS__GetMemory(size_t Size)
{
    return HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, Size);
}

#elif 1

void* __cdecl RTOS__GetMemory(size_t Size)
{
    return VirtualAlloc(NULL, Size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
}

#elif 0

/* Same as previous, but with some internal logging, which 
reveals the lossage that results from using VirtualAlloc with RTMachine.PageSize < GetSystemInfo.AllocationGranularity.
*/
typedef struct _RTOSMemoryLogEntry_t
{
    size_t Size;
    void* Result;
} RTOSMemoryLogEntry_t;

RTOSMemoryLogEntry_t RTOSMemoryLog[128];
size_t RTOSMemoryLogIndex;
#define NUMBER_OF(a) (sizeof(a)/sizeof((a)[0]))

void* __cdecl RTOS__GetMemory(size_t Size)
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
