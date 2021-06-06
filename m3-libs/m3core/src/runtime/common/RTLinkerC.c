#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*
EnvFromMain is either char** from main, or char* GetEnvironmentStringsA from WinMain.
Rather than make a coordinated compiler/runtime change, we just ignore
the compiler-provided data and make the runtime always work.
One additional copy of the environment variables is leaked per .exe/.dll.

RTLinker__GetEnvironmentStrings here matches M3C output (subject to change).
That is why "ADDRESS" and not e.g. "void*".
*/
ADDRESS
__cdecl
RTLinker__GetEnvironmentStrings (ADDRESS EnvFromMain)
{
#ifdef _WIN32
    return (ADDRESS)GetEnvironmentStringsA ();
#else
    return EnvFromMain;
#endif
}

#ifndef _WIN32 /* Do not accidentally export printf. */

#if 1 /* debugging code */

/*
  see RT0.i3 for the types here
  It would be good for the Modula-3 compiler to generate C headers.
*/

#include <stdio.h>

struct ModuleInfo_t; typedef struct ModuleInfo_t ModuleInfo_t;
struct ImportInfo_t; typedef struct ImportInfo_t ImportInfo_t;
#if 0
struct Text_t; typedef struct Text_t Text_t;
#endif

struct ImportInfo_t
{
    ModuleInfo_t* Import;
    void* Binder; /* returns "import" pointer */
    ImportInfo_t* Next;
};

struct ModuleInfo_t
{
    const char* File;               /* 0 */
    void* TypeCells;                /* 4 8 */
    void* TypeCellPointers;         /* 8 16 */
    void* FullRevelation;           /* 12 24 */
    void* PartialRevelation;        /* 16 32 */
    void* ProcedureInformation;     /* 20 40 */
    void* TryScopes;                /* 24 48 */
    void* VariableMap;              /* 28 56 */
    void* GarbageCollectionMap;     /* 32 64 */
    ImportInfo_t* Imports;          /* 36 72 */
    size_t LinkState;               /* 40 80 */
    void* Binder;                   /* 44 88 */
    size_t GarbageCollectionFlags;  /* 48 96 */
};

#if 0
struct Text_t
{
    void* Functions;
    ptrdiff_t Length;
    char Chars[1];
};
#endif

#if 0
#define traceInit RTLinker__traceInit
enum Trace_t
{
    Trace_None,
    Trace_M3,
    Trace_C,
};
size_t traceInit /* = Trace_C */;

void
RTIO__PutString(const char* a);

void
RTIO__PutText(Text_t* a);

void
RTIO__PutInt(int a);

static
void
RTIO__Flush(void);

static
void
RTLinker__PrintFlush(void)
{
    if (traceInit == Trace_M3)
        RTIO__Flush();
}

static
void
RTLinker__PrintString(const char* a)
{
    if (a == NULL || a[0] == 0)
        return;
    switch (traceInit)
    {
    case Trace_None:
        break;
    case Trace_M3:
        RTIO__PutString(a);
        break;
    case Trace_C:
        printf("%s", a);
        break;
    }
}

static
void
RTLinker__PrintText(Text_t* a)
{
    if (a == NULL || a->Length < 1)
        return;
    switch (traceInit)
    {
    case Trace_None:
        break;
    case Trace_M3:
        RTIO__PutText(a);
        break;
    case Trace_C:
        printf("%.*s", ((int)a->Length), a->Chars);
        break;
    }
}

static
void
RTLinker__PrintInt(int a)
{
    switch (traceInit)
    {
    case Trace_None:
        break;
    case Trace_M3:
        RTIO__PutInt(a);
        break;
    case Trace_C:
        printf("%X", a);
        break;
    }
}

#endif

void
RTLinker__PrintModule(ModuleInfo_t* Module)
{
    ImportInfo_t* Imports = { 0 };

#if 0
    /* This is fine with Mac OSX 10.5.8 but gcc/clang
       in 10.10.4 Yosemite warns or errors. */
    if ((Module == NULL) /*|| (traceInit != Trace_C)*/)
        return;
#else
    if (Module == NULL /*|| (traceInit != Trace_C)*/)
        return;
#endif
    Imports = Module->Imports;
    while (Imports != NULL)
    {
        printf("Module %p %s Imports %p{Import %p, Binder %p, Next %p}",
               Module,
               Module->File,
               Imports,
               (Imports ? Imports->Import : NULL),
               (Imports ? Imports->Binder : NULL),
               (Imports ? Imports->Next : NULL));
        fflush(0);
        printf(" %p ", Imports && Imports->Import ? Imports->Import->File : "");
        fflush(0);
        printf(" %s\n", Imports && Imports->Import ? Imports->Import->File : "");
        Imports = Imports->Next;
    }
    printf("\n");
}

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
