#include <stddef.h>

// Put types inside extern C because some compilers (Sun) consider
// such linkage as part of the type and error upon conflicts,
// between m3core.h and _m3main.c when they are concatenated.
// At least for function pointer types.

#ifdef __cplusplus
extern "C" {
#endif

#if __INITIAL_POINTER_SIZE == 64
typedef __int64 INTEGER;
#else
typedef ptrdiff_t INTEGER;
#endif

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

typedef char* ADDRESS;
typedef void (__cdecl*M3PROC)(void);

#ifndef RT0__ModulePtr
#define RT0__ModulePtr RT0__ModulePtr
typedef ADDRESS RT0__ModulePtr;
#endif

//correct, but match preexisting m3core
//void __cdecl RTLinker__InitRuntime(INTEGER argc, char** argv, char** envp, void* hinstance);
void __cdecl RTLinker__InitRuntime(INTEGER argc, ADDRESS argv, ADDRESS envp, ADDRESS hinstance);
void __cdecl RTProcess__Exit(INTEGER);

//if not s.lazyInit
//correct, but workaround hash collisions in ProcType
//void __cdecl RTLinker__AddUnitImports(void* (__cdecl*)(void));
void __cdecl RTLinker__AddUnitImports(M3PROC);

//correct, but void __cdecl RTLinker__AddUnit(void* (__cdecl*)(void));
void __cdecl RTLinker__AddUnit(M3PROC);

RT0__ModulePtr __cdecl Main_M3(INTEGER mode);

#ifdef __cplusplus
} /* extern "C" */
#endif


int main (int argc, char** argv, char** envp)
{
  RTLinker__InitRuntime (argc, (ADDRESS)argv, (ADDRESS)envp, 0);
  RTLinker__AddUnit ((M3PROC)Main_M3);
  RTProcess__Exit (0);
  return 0;
}

