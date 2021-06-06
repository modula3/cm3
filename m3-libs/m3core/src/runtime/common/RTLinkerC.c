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

STRUCT_TYPEDEF(RT0__Module)
STRUCT_TYPEDEF(RT0__Import)
STRUCT_TYPEDEF(RT0__Revelation)
STRUCT_TYPEDEF(RT0__Typecell)
STRUCT_TYPEDEF(RT0__Brand)
STRUCT_TYPEDEF(RT0__TypeLink)
STRUCT_TYPEDEF(RT0__Proc)

typedef unsigned char    RT0__Fingerprint[8]; // 64 bits, no alignment (but actually 2pointer aligned)
typedef const char*      RT0__String;

typedef RT0__Module* (__cdecl*RT0__Binder)(INTEGER mode);
typedef         void (__cdecl*RT0__TypeInitProc)(ADDRESS);

struct RT0__Proc // one of these is generated for each top-level procedure
{
    ADDRESS     proc;
    const char* name;
};

struct RT0__TypeLink
{
    RT0__Typecell* defn;     // initially a pointer to the next TypeLink
    INTEGER        typecode; // initially the compile-time UID of the type
};

struct RT0__Brand
{
    INTEGER length;
    char chars [1 /* length */];
};

struct RT0__Revelation
{
    INTEGER lhs_id;
    INTEGER rhs_id;
};

struct RT0__Import
// Keep this in sync with RT0.i3
// one of these is generated for each imported interface reference
{
    RT0__Module* import;
    RT0__Binder  binder; // returns "import" pointer
    RT0__Import* next;
};

struct RT0__Typecell
{
    INTEGER           typecode;       // Typecode
    INTEGER           selfID;
    RT0__Fingerprint  fp;
    BOOLEAN           traced;
    UINT8             kind;           // == ORD (TypeKind = { Unknown, Ref, Obj, Array })
    UINT8             link_state;
    UINT8             dataAlignment;
    INTEGER           dataSize;
    ADDRESS           type_map;       // RTTypeMap.T
    ADDRESS           gc_map;         // reduced RTTypeMap.T for collector
    ADDRESS           type_desc;      // enhanced RTTipe map for new pickles
    RT0__TypeInitProc initProc;       // called by NEW
    RT0__Brand*       brand_ptr;
    const char*       name;
    RT0__Typecell*    next;
};

struct RT0__Module
// Keep this in sync with RT0.i3
// allocated at offset 0 of each compilation unit's global data
{
    const char*      file;           //  0
    RT0__Typecell*   type_cells;     //  4  8
    RT0__TypeLink*   type_cell_ptrs; //  8 16
    RT0__Revelation* full_rev;       // 12 24
    RT0__Revelation* partial_rev;    // 16 32
    RT0__Proc*       proc_info;      // 20 40
    ADDRESS          try_scopes;     // 24 48
    ADDRESS          var_map;        // 28 56
    ADDRESS          gc_map;         // 32 64
    RT0__Import*     imports;        // 36 72
    INTEGER          link_state;     // 40 80
    RT0__Binder      binder;         // 44 88
    INTEGER          gc_flags;       // 48 96
};

#include <stdio.h>

#if 0
STRUCT_TYPEDEF(RT0__TypeLink)
struct Text_t
{
    void* Functions;
    ptrdiff_t Length;
    char Chars[1];
};

enum Trace_t
{
    Trace_None,
    Trace_M3,
    Trace_C,
};
size_t RTLinker__traceInit /* = Trace_C */;

void
__cdecl
RTIO__PutString(const char* a);

void
__cdecl
RTIO__PutText(Text_t* a);

void
__cdecl
RTIO__PutInt(int a);

void
__cdecl
RTIO__Flush(void);

static
void
RTLinker__PrintFlush(void)
{
    if (RTLinker__traceInit == Trace_M3)
        RTIO__Flush();
}

static
void
RTLinker__PrintString(const char* a)
{
    if (a == NULL || a[0] == 0)
        return;
    switch (RTLinker__traceInit)
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
    switch (RTLinker__traceInit)
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
    switch (RTLinker__traceInit)
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
__cdecl
RTLinker__PrintModule(RT0__Module* module)
{
    RT0__Import* imports = { 0 };

    if (module == NULL)
        return;

    imports = module->imports;
    while (imports)
    {
        printf("module %p %s imports %p{import %p, binder %p, next %p}",
               (void*)module,
               module->file,
               (void*)imports,
               (void*)(imports ? imports->import : NULL),
               (void*)(imports ? *(void**)&imports->binder : NULL),
               (void*)(imports ? imports->next : NULL));
        fflush(0);
        printf(" %p ", imports && imports->import ? imports->import->file : "");
        fflush(0);
        printf(" %s\n", imports && imports->import ? imports->import->file : "");
        imports = imports->next;
    }
    printf("\n");
}

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
