/* $Id$ */

/*
a replacement for RTIO that works when things are too
broken for even RTIO to work

  see RT0.i3 for the types here
  It would be good for the Modula-3 compiler to generate C headers.
*/

#include <stdio.h>

struct ModuleInfo_t;
typedef struct ModuleInfo_t ModuleInfo_t;

struct ImportInfo_t;
typedef struct ImportInfo_t ImportInfo_t;

struct Text_t;
typedef struct Text_t Text_t;

struct ImportInfo_t
{
    ModuleInfo_t* Import;
    void* Binder; /* returns "import" pointer */
    ImportInfo_t* Next;
};

struct ModuleInfo_t
{
    const char* File;
    void* TypeCells;
    void* TypeCellPointers;
    void* FullRevelation;
    void* PartialRevelation;
    void* ProcedureInformation;
    void* TryScopes;
    void* VariableMap;
    void* GarbageCollectionMap;
    ImportInfo_t* Imports;
    size_t LinkState;
    void* Binder;
    size_t GarbageCollectionFlags;
};

struct Text_t
{
    void* Functions;
    size_t Length;
    char Chars[1];
};

#define traceInit RTLinker__traceInit
enum Trace_t
{
	Trace_None,
	Trace_M3,
	Trace_C,
};
size_t traceInit /* = Trace_C */;

void
RTIO__PutString(
    const char* a
    );

void
RTIO__PutText(
    Text_t* a
    );

void
RTIO__PutInt(
    int a
    );

void
RTIO__Flush(
    void
    );

void
RTLinker__PrintFlush(
	void
	)
{
	if (traceInit == Trace_M3)
		RTIO__Flush();
}

void
RTLinker__PrintString(
    const char* a
    )
{
    if (a == NULL)
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

void
RTLinker__PrintText(
    Text_t* a
    )
{
    if (a == NULL)
        return;
	switch (traceInit)
	{
	case Trace_None:
		break;
	case Trace_M3:
		RTIO__PutText(a);
		break;
	case Trace_C:
		printf("%.*s", ((int) a->Length), a->Chars);
		break;
	}
}

void
RTLinker__PrintInt(
    int a
    )
{
	switch (traceInit)
	{
	case Trace_None:
		break;
	case Trace_M3:
		RTIO__PutInt(a);
		break;
	case Trace_C:
		printf("%x", a);
		break;
	}
}

void
RTLinker__PrintModule(
    ModuleInfo_t* Module
    )
{
    ImportInfo_t* Imports;

    if ((Module == NULL) || (traceInit != Trace_C))
        return;
    Imports = Module->Imports;
    while (Imports != NULL)
    {
        printf(
            "Module %p %s Imports %p{Import %p, Binder %p, Next %p} %s\n",
            Module,
            Module->File,
            Imports,
            (Imports ? Imports->Import : NULL),
            (Imports ? Imports->Binder : NULL),
            (Imports ? Imports->Next : NULL),
            (Imports && Imports->Import ? Imports->Import->File : "")
            );
        Imports = Imports->Next;
    }
    printf("\n");
}
