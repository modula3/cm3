/* $Id: RTLinkerC.c,v 1.1 2008-01-20 11:01:09 jkrell Exp $ */

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

struct String_t;
typedef struct String_t String_t;

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

struct String_t
{
    void* VTable;
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
    String_t* a
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
		fprintf(stdout, "%s", a);
		break;
	}
}

void
RTLinker__PrintText(
    String_t* a
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
		fprintf(stdout, "%.*s", ((int) a->Length), a->Chars);
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
		fprintf(stdout, "%x", a);
		break;
	}
}

void
RTLinker__PrintModule(
    ModuleInfo_t* Module
    )
{
    ImportInfo_t* Imports;
    FILE* Out;

    if ((Module == NULL) || (traceInit != Trace_C))
        return;
    Out = stdout;
    Imports = Module->Imports;
    while (Imports != NULL)
    {
        fprintf(
            Out,
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
    fprintf(Out, "\n");
}
