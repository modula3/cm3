/*
Jay Krell
jay.krell@cornell.edu
January 2009

A sort of autoconf replacement, though far less general.
And maybe faster and simpler.

Bundled cc on HP_UX is K&R, so this is K&R,
at least for function prototypes. And a doesn't like
"signed" and at least somewhat "const".

On some systems this file cannot run from a.out because
that conflicts with its own output, so, for example:
  cc config.c -o config
  ./config
*/

#define _INCLUDE_POSIX_SOURCE
#define _INCLUDE_HPUX_SOURCE
#define _FILE_OFFSET_BITS 64

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <stddef.h>
#include <pthread.h>
#ifndef _WIN32
#ifndef __STDC__
#define const /* nothing, for bundled HP-UX cc, esp. within the system's own headers */
#endif
#include <netdb.h>
#include <netinet/in.h>
#include <sys/stat.h>
#endif /* WIN32 */
#include <setjmp.h>
#include <time.h>
#include <signal.h>
typedef int BOOL;
#define TRUE 1
#define FALSE 0
typedef ptrdiff_t INTEGER;

typedef struct timeval timeval_t;

#define IS_TYPE_SIGNED(x)  (((x)-1) < (x)0)
#define IS_FIELD_SIGNED(x) (((x) = -1) < 0)

/* If (x)-1 generates a warning, try ~(x)0.
If (x) = -1 generates a warning, try (memset(&x, -1, sizeof(x)), x) */

#ifdef __STDC__
#define CHECK(x) ((x) || (CheckFailed(#x), 0))
#else
/* bundled HP-UX cc */
#define CHECK(x) ((x) || (CheckFailed("x"), 0))
#endif

/* Define a 64bit integer type, and verify that a exists. */
#ifndef _MSC_VER
#define __int64 long long
#endif

#define ALIGN_OF_TYPE(x) (sizeof(struct {char a; x b;}) - sizeof(x))
#define SIZEOF_FIELD(struc, field) (sizeof((struc*)0)->field)

typedef unsigned U;

FILE* LogFile;

/* Hypothetically would change these for a C generating mode.
But the right way to implement all this is probably to output XML and then apply an XSL style sheet,
but that would really bloat up the implementation. */
char* BeginComment = "(*";
char* EndComment = "*)";

BOOL TryCompile();

#if __STDC__
void Print(char* Format, ...)
#else
void Print(Format, va_alist)
    char* Format;
    va_dcl
#endif
{
    va_list Args;

#if __STDC__
    va_start(Args, Format);
#else
    va_start(Args);
#endif
    vprintf(Format, Args);
    vfprintf(LogFile, Format, Args);
    va_end(Args);
}

char* ConcatN(a, n)
    char** a;
    size_t n;
{
    size_t i = 0;
    size_t TotalLength = 1;
    size_t NextLength = 1;
    size_t Length;
    char* Result = 0;
    char* Cursor = 0;

    for (i = 0 ; i != n ; ++i)
    {
        Length = strlen(a[i]);
        NextLength += Length;
        if (NextLength < TotalLength)
        {
            Print("integer overflow\n");
            exit(1);
        }
        TotalLength = NextLength;
    }
    Result = (char*) malloc(TotalLength);
    if (Result == NULL)
    {
        Print("out of memory\n");
        exit(1);
    }
    Cursor = Result;
    for (i = 0 ; i != n ; ++i)
    {
        Length = strlen(a[i]);
        memcpy(Cursor, a[i], Length + 1);
        Cursor += Length;
    }
    return Result;
}

char* Concat3(a, b, c)
    char* a;
    char* b;
    char* c;
{
    char* d[3];
    d[0] = a;
    d[1] = b;
    d[2] = c;
    return ConcatN(d, 3);
}

char* Concat2(a, b)
    char* a;
    char* b;
{
    return Concat3(a, b, "");
}

void CheckFailed(x)
    char* x;
{
    Print("%s is not true; giving up\n", x);
    exit(1);
}

void SanityCheck()
{
    CHECK(CHAR_BIT == 8);

    /* Check that all the types exist and are reasonably sized.
    NOTE that these assumptions are beyond what the standard guarantees.
    */
    CHECK(sizeof(char) == 1);
    CHECK(sizeof(short) == 2);
    CHECK(sizeof(int) == 4);
    CHECK(sizeof(long) >= 4);
    CHECK((sizeof(long) == sizeof(int)) || (sizeof(long) == sizeof(int64_t)));
    CHECK(sizeof(int64_t) == 8);

    CHECK(sizeof(char) == sizeof(unsigned char));
#ifdef __STDC__
    CHECK(sizeof(char) == sizeof(signed char));
#endif
    CHECK(sizeof(short) == sizeof(unsigned short));
    CHECK(sizeof(int) == sizeof(unsigned int));
    CHECK(sizeof(long) == sizeof(unsigned long));
    CHECK(sizeof(__int64) == sizeof(unsigned __int64));

    CHECK(sizeof(float) == 4);
    CHECK(sizeof(double) == 8);

#ifndef _WIN64
    CHECK(sizeof(long) == sizeof(void*));
#endif
    CHECK(sizeof(char*) == sizeof(void*));
    CHECK(sizeof(void*) == sizeof(void (*)()));

    /* Verify 2's compliment. */

#define UMAX(x) ((x)~(x)0)
#define SMAX(x) (((x)~(x)0) ^ (x)((~(x)0) << (sizeof(x) * 8 - 1)))
#define SMIN(x) ((x)(-SMAX(x) - 1))

    CHECK(SCHAR_MIN == -128);
    CHECK(SCHAR_MAX == 127);
    CHECK(UCHAR_MAX == 255);
    CHECK((CHAR_MIN == 0) || (CHAR_MIN == -128));
    CHECK((CHAR_MAX == 127) || (CHAR_MAX == 255));

#ifdef __STDC__
    CHECK(SCHAR_MIN == SMIN(signed char));
    CHECK(SCHAR_MAX == SMAX(signed char));
#endif
    CHECK(UCHAR_MAX == UMAX(unsigned char));

    CHECK(SHRT_MIN == -32768);
    CHECK(SHRT_MAX == 32767);
    CHECK(USHRT_MAX == 65535);

    CHECK(SHRT_MIN == SMIN(short));
    CHECK(SHRT_MAX == SMAX(short));
    CHECK(USHRT_MAX == UMAX(unsigned short));

    CHECK(INT_MIN == (-2147483647 - 1));
    CHECK(INT_MAX == 2147483647);
#ifdef __STDC__
    /* bundled HP-UX cc */
    CHECK(UINT_MAX == 4294967295U);
#endif

    CHECK(INT_MIN == SMIN(int));
    CHECK(INT_MAX == SMAX(int));
    CHECK(UINT_MAX == UMAX(unsigned int));

    if (sizeof(long) == 4)
    {
        CHECK(LONG_MIN == INT_MIN);
        CHECK(LONG_MAX == INT_MAX);
    }
}

typedef struct _Field_t {
    char* Name; /* initialize this */
    size_t Size; /* filled in at runtime */
    size_t Offset; /* filled in at runtime */
    char* Type; /* filled in at runtime */
} Field_t;

typedef struct _Struct_t {
    Field_t* Fields;
    size_t NumberOfFields;
    /* CONSIDER: OptionalFields, and #define HAVE_NativeName_FieldName for what is present. */
    char* NativeName;
    char* MyName;
} Struct_t;

void ReconstituteStruct(Prefix, Struct, FilePath)
    char* Prefix;
    Struct_t* Struct;
    char* FilePath;
/*
Given a type declared in Prefix, write out FilePath that redeclares, containing
the fields identified by Struct, and any necessary padding for fields not
listed in Struct. Prefix should declare a as NativeName. FilePath
will declare a as MyName.

Methodology:
    Write out a program that writes out what a can discover about the type.
*/
{
    size_t i;
    size_t j;
    char* a[60];
    char* Source;
    char* NativeName = Struct->NativeName;
    char* MyName = Struct->MyName ? Struct->MyName : Struct->NativeName;

    unlink(FilePath);

    /* Make sure Prefix compiles. */

    if (TryCompile(Prefix) == FALSE)
    {
        printf("ReconstituteStruct unable to compile prefix\n");
        exit(1);
    }

    for (i = 0 ; FilePath[i] ; ++i)
    {
        if (FilePath[i] == '\\')
            FilePath[i] = '/';
    }

    for (i = 0 ; i != Struct->NumberOfFields ; ++i)
    {
        j = 0;
        a[j++] = Prefix;
        a[j++] = "\n";
        a[j++] = "#include <stdio.h>\n";
        a[j++] = "#include <stddef.h>\n"; /* offsetof */
        a[j++] = "#define SIZEOF_FIELD(struc, field) (sizeof((struc*)0)->field)\n";
        a[j++] = "#define ALIGN_OF_TYPE(x) (sizeof(struct {char a; x b;}) - sizeof(x))\n";
        a[j++] = "typedef unsigned U;\n";
        a[j++] = "int main()\n";
        a[j++] = "{\n";
        a[j++] = "  FILE* File;\n";
        a[j++] = "  File = fopen(\"";
        a[j++] = FilePath;
        a[j++] = "\", \"a\");\n";

        if (i == 0)
        {
            a[j++] = "fprintf(File, \"sizeof(%s):\", \"";
            a[j++] = NativeName;
            a[j++] = "\");\n";
            a[j++] = "  fprintf(File, \"%u\\n\", (U)sizeof(";
            a[j++] = NativeName;
            a[j++] = "));\n";

            a[j++] = "fprintf(File, \"ALIGN_OF_TYPE(%s):\", \"";
            a[j++] = NativeName;
            a[j++] = "\");\n";
            a[j++] = "  fprintf(File, \"%u\\n\", (U)ALIGN_OF_TYPE(";
            a[j++] = NativeName;
            a[j++] = "));\n";
        }

        a[j++] = "fprintf(File, \"offsetof(%s, %s):\", \"";
        a[j++] = NativeName;
        a[j++] = "\", \"";
        a[j++] = Struct->Fields[i].Name;
        a[j++] = "\");\n";
        a[j++] = "  fprintf(File, \"%u\\n\", (U)offsetof(";
        a[j++] = NativeName;
        a[j++] = ", ";
        a[j++] = Struct->Fields[i].Name;
        a[j++] = "));\n";

        a[j++] = "fprintf(File, \"SIZEOF_FIELD(%s, %s):\", \"";
        a[j++] = NativeName;
        a[j++] = "\", \"";
        a[j++] = Struct->Fields[i].Name;
        a[j++] = "\");\n";
        a[j++] = "  fprintf(File, \"%u\\n\", (U)SIZEOF_FIELD(";
        a[j++] = NativeName;
        a[j++] = ", ";
        a[j++] = Struct->Fields[i].Name;
        a[j++] = "));\n";

        a[j++] = "return 0;\n}\n";
        Source = ConcatN(a, j);
        TryCompileAndLinkAndRun(Source);
        free(Source);
    }
}

void DefineOpaqueType(Name, Size, Align)
    char* Name;
    size_t Size;
    size_t Align;
{
    /* TODO:
    pthread initializers -- should be doable by instantiating them here
    and examining their bits.
    */
    char* Element;

    if ((Size % Align) != 0)
    {
        Print("Size must be multiple of Align (%s, %u, %u)\n", Name, (U)Size, (U)Align);
        exit(1);
    }

    if (Size == sizeof(void*) && Align == ALIGN_OF_TYPE(void*))
    {
        Print("%s = INTEGER; %s opaque %s\n", Name, BeginComment, EndComment);
        return;
    }
    if (Size == sizeof(__int64) && Align == ALIGN_OF_TYPE(__int64))
    {
        Print("%s = LONGINT; %s opaque %s\n", Name, BeginComment, EndComment);
        return;
    }
    if (Size == sizeof(int) && Align == ALIGN_OF_TYPE(int))
    {
        Print("%s = int32_t; %s opaque %s\n", Name, BeginComment, EndComment);
        return;
    }
    if (Align == ALIGN_OF_TYPE(void*))
    {
        Element = "INTEGER";
    }
    else if (Align == ALIGN_OF_TYPE(__int64))
    {
        Element = "LONGINT";
    }
    else if (Align > ALIGN_OF_TYPE(__int64))
    {
        Print("WARNING: %s alignment lowered from %u to LONGINT\n", Name, (U)Align);
        Element = "LONGINT";
        Align = ALIGN_OF_TYPE(__int64);
    }
    else if (Align == ALIGN_OF_TYPE(int))
    {
        Element = "uint32_t";
    }
    else if (Align == ALIGN_OF_TYPE(short))
    {
        Element = "uint16_t";
    }
    else if (Align == ALIGN_OF_TYPE(char))
    {
        Element = "uint8_t";
    }
    else
    {
        Print("ERROR: unable to represent alignment %u for type %s\n", (U)Align, Name);
        exit(1);
    }
    /* need to check that a is an even multiple */
    if ((Size % Align) != 0)
    {
        printf("ERROR: size (%u) is not an even multiple of align (%u) for type %s\n", Size, Align, Name);
        exit(1);
    }
    Print("%s = RECORD opaque: ARRAY [1..%u] OF %s; END\n", Name, (U)(Size / Align), Element);
}

#ifdef __STDC__
#define DEFINE_OPAQUE_TYPE(name) DefineOpaqueType(#name, sizeof(name), ALIGN_OF_TYPE(name))
#else
#define DEFINE_OPAQUE_TYPE(name) DefineOpaqueType("name", sizeof(name), ALIGN_OF_TYPE(name))
#endif


char* GetIntegerType(Size, Signed)
    size_t Size;
    BOOL Signed;
{
    Size *= 8;
    switch (Size | Signed)
    {
    case 8: return "uint8_t";
    case 16: return "uint16_t";
    case 32: return "uint32_t";
    case 64: return "uint64_t";
    case 8|1: return "int8_t";
    case 16|1: return "int16_t";
    case 32|1: return "int32_t";
    case 64|1: return "int64_t";
    default:
        /* consider using array of smaller type */
        printf("ERROR: not able to represent size %u\n", (U)Size);
        exit(1);
    }
}

void DefineIntegerType(Name, Size, Signed, Align)
    char* Name;
    size_t Size;
    BOOL Signed;
    size_t Align;
{
    Align *= 8;
    Print("%s = %s; %s align = %u %s\n", Name, GetIntegerType(Size, Signed), BeginComment, (U)Align, EndComment);
}

#ifdef __STDC__
#define DEFINE_INTEGER_TYPE(x) DefineIntegerType(#x, sizeof(x), IS_TYPE_SIGNED(x), ALIGN_OF_TYPE(x))
#else
#define DEFINE_INTEGER_TYPE(x) DefineIntegerType("x", sizeof(x), IS_TYPE_SIGNED(x), ALIGN_OF_TYPE(x))
#endif

void DefineIntegerFieldType(struc, field, myname, Size, Signed)
    char* struc;
    char* field;
    char* myname;
    size_t Size;
    BOOL Signed;
{
    Print("%s = %s; %s %s.%s %s\n", myname, GetIntegerType(Size, Signed), BeginComment, struc, field, EndComment);
}

#ifdef __STDC__
#define DEFINE_INTEGER_FIELD_TYPE(struc, field, myinstance, myname) \
    DefineIntegerFieldType(#struc, #field, #myname, sizeof(myinstance), IS_FIELD_SIGNED(myinstance))
#else
#define DEFINE_INTEGER_FIELD_TYPE(struc, field, myinstance, myname) \
    DefineIntegerFieldType("struc", "field", "myname", sizeof(myinstance), IS_FIELD_SIGNED(myinstance))
#endif

char* Compiler;

char* PossibleCompilers[] = {
    /* favor compiler used to build this, if user defines a */
#ifdef CC
    CC
#ifdef CFLAGS
    CFLAGS
#endif
    ,
#endif
    /* My HP-UX requires -lpthread to link, seems wrong.
    -Werror is because certain code that really should error, only warns. */
    "gcc -lpthread -Werror",
    "gcc -Werror",
    "cc -lpthread -Werror",
    "gcc -lpthread",
    "gcc",
    "cc",

#if 0
    /* MPW compilers */
    "C", /* original 68K */
    "ppcc", /* original PowerPC */
    "MrC", /* later PowerPC?68K */
    "SC", /* later PowerPC?68K */

    /* Windows compilers */
    "cl", /* Microsoft Visual C++ */
    "wcl386", /* Watcom */
    "dmc", /* Digtal Mars */
    "mwcc", /* Metrowerks */
    "sc", /* Symentic, but also the name of another command, that pauses to prompt, beware */
    NULL,
#endif
};

char* PossibleLinkOutput[] = {
    "conf1.exe",
    "a.exe",
    "a.out",
    NULL
};

char* PossibleCompileAndLinkOutput[] = {
    "conf1.exe",
    "conf1.o",
    "conf1.obj",
    "a.exe",
    "a.out",
    /* Check which one of these MPW produces */
    "conf1.c.o",
    "conf1.c.obj",
    NULL
};

char* PossibleCompileOutput[] = {
    "conf1.exe",
    "conf1.o",
    "conf1.obj",
    /* Check which one of these MPW produces */
    "conf1.c.o",
    "conf1.c.obj",
    NULL
};

char* WhichOutputExists(s)
    char** s;
{
    FILE* File = 0;
    size_t i = 0;
    for (i = 0 ; s[i] ; ++i)
    {
        File = fopen(s[i], "rb");
        if (File != NULL)
        {
            fclose(File);
            break;
        }
    }
    return s[i];
}

BOOL DoesOutputExist(s)
    char** s;
{
    return (WhichOutputExists(s) != NULL);
}

void DeleteOutput(s)
    char** s;
{
    size_t i = 0;
    for (i = 0 ; s[i] ; ++i)
    {
        unlink(s[i]);
    }
}

void DeleteCompileOutput()
{
    DeleteOutput(PossibleCompileOutput);
}

void DeleteCompileAndLinkOutput()
{
    DeleteOutput(PossibleCompileAndLinkOutput);
}

BOOL DoesCompileOutputExist()
{
    return DoesOutputExist(PossibleCompileOutput);
}

BOOL DoesCompileAndLinkOutputExist()
{
    return DoesOutputExist(PossibleCompileAndLinkOutput);
}

void CreateSourceFile(Snippet)
    char* Snippet;
{
    size_t i = 0;
    FILE* FileHandle = 0;

    FileHandle = fopen("conf1.c", "w");
    if (FileHandle == NULL)
    {
        Print("fopen(conf1.c, w) failed\n");
        exit(1);
    }

    Snippet = Concat2("#define _INCLUDE_POSIX_SOURCE\n#define _INCLUDE_HPUX_SOURCE\n#define _FILE_OFFSET_BITS 64\n", Snippet);
    fprintf(LogFile, "compiling: %s", Snippet);
    fprintf(FileHandle, "%s", Snippet);
    free(Snippet);
    fclose(FileHandle);
}

/* gcc warns about the -lpthread when we are only compiling; annoying */

void RemoveLinkOptionFromCommandLine(CommandLine)
    char* CommandLine;
{
    char* pthread;

    pthread = strstr(CommandLine, " -lpthread ");
    if (pthread)
    {
        memmove(pthread, pthread + 10, 1 + strlen(pthread + 10));
    }
}

BOOL TryCompile(Snippet)
    char* Snippet;
{
    char* CommandLine = 0;
    int ExitCode = 0;
    BOOL Exists = 0;

    CommandLine = Concat2(Compiler, " -c conf1.c");
    RemoveLinkOptionFromCommandLine(CommandLine);
    CreateSourceFile(Snippet);
    fprintf(LogFile, "running: %s\n", CommandLine);
    ExitCode = system(CommandLine);
    fprintf(LogFile, "=> %d\n", ExitCode);
    free(CommandLine);
    Exists = DoesCompileOutputExist();
    DeleteCompileOutput();
    return ((ExitCode == 0) && Exists);
}

BOOL TryCompileAndLink(Snippet)
    char* Snippet;
{
    char* CommandLine = 0;
    int ExitCode = 0;
    BOOL Exists = 0;

    CommandLine = Concat2(Compiler, " conf1.c");
    CreateSourceFile(Snippet);
    fprintf(LogFile, "running: %s\n", CommandLine);
    ExitCode = system(CommandLine);
    fprintf(LogFile, "=> %d\n", ExitCode);
    free(CommandLine);
    Exists = DoesCompileAndLinkOutputExist();
    DeleteCompileAndLinkOutput();
    return ((ExitCode == 0) && Exists);
}

BOOL TryCompileAndLinkAndRun(Snippet)
    char* Snippet;
{
    char* CommandLine = 0;
    int ExitCode = 0;
    char* Exists = 0;

    CommandLine = Concat2(Compiler, " conf1.c");
    CreateSourceFile(Snippet);
    fprintf(LogFile, "running: %s\n", CommandLine);
    ExitCode = system(CommandLine);
    free(CommandLine);
    Exists = WhichOutputExists(PossibleLinkOutput);
    if (Exists == FALSE)
        return FALSE;
    CommandLine = Concat2("./", Exists);
    ExitCode = system(CommandLine);
    DeleteCompileAndLinkOutput();
    return (ExitCode == 0);
}

char* DevNull;
char* PossibleDevNull[] = { "nul:", "/dev/null", NULL };

void FindDevNull()
{
    size_t i = 0;
    FILE* File = 0;

    if (DevNull)
        return;

    Print("looking for /dev/null..");
    for (i = 0 ; DevNull = PossibleDevNull[i] ; ++i)
    {
        File = fopen(DevNull, "r");
        if (File != NULL)
        {
            fclose(File);
            Print("%s\n", DevNull);
            return;
        }
    }
    Print("no /dev/null found\n");
    exit(1);
}

void FindCompiler()
{
    size_t i = 0;
    char* CFlags;

    if (Compiler)
        return;

    Print("looking for C compiler..");

    if (CFlags = getenv("CFLAGS"))
    {
        Print("using environment variable CFLAGS: %s\n", CFlags);
    }

    if (Compiler = getenv("CC"))
    {
        Print("using CC environment variable: %s\n", Compiler);
        Compiler = CFlags ? Concat3(Compiler, " ", CFlags) : Compiler;
        return;
    }
    for (i = 0 ; Compiler = PossibleCompilers[i] ; ++i)
    {
        Compiler = CFlags ? Concat3(Compiler, " ", CFlags) : Compiler;
        if (TryCompile("int main() { return 0; }\n"))
        {
            Print("%s\n", Compiler);
            return;
        }
    }
    Print("no C compiler found\n");
    exit(1);
}

BOOL CheckHeader(Header)
    char* Header;
{
    char* Source;
    BOOL Result;

    Source = Concat3("#include ", Header, "\n");
    Result = TryCompile(Source);
    free(Source);
    return Result;
}

BOOL CheckField(Prefix, Struct, Type, Field)
    char* Prefix;
    char* Struct;
    char* Type;
    char* Field;
{
    char* Source = 0;
    char* a[30];
    BOOL Result = 0;
    size_t i = 0;

    if (Type[0])
    {
        if (CheckField(Prefix, Struct, "", Field) == FALSE)
        {
            return FALSE;
        }
    }

    Print("checking for %s%s%s.%s\n", Type, Type[0] ? " " : "", Struct, Field);

    a[i++] = Prefix;
    a[i++] = Struct;
    a[i++] = " a;\n";
    a[i++] = Type[0] ? Type : "void";
    a[i++] = "* b = ";
    a[i++] = "&a.";
    a[i++] = Field;
    a[i++] = ";\n";
    Source = ConcatN(a, i);
    Result = TryCompile(Source);
    free(Source);

    do
    {
        if (Type[0] && Result)
        {
            /* gcc is lenient here and only warns; try another test? */
            /* unable to come up with one, will try -Werror */

            /* HP-UX bundled cc also accepts such code without warning,
            and I'm not able to find a way to make a an error.
            We can do some other things though.
            We can assert that the size matches.
            We can make another variable of the type, assign a non-zero value,
            and memcmp the two. This at least discerns float from int.
            It does not discern various pointer types and integer types.
            */
            i = 0;
            a[i++] = Prefix;
            a[i++] = Struct;
            a[i++] = " a;\n";
            /* char a[(sizeof(b) == sizeof(c)) ? 1 : -1];
               is legal if the sizes are the same, else illegal.
            Remember to test this on SGI. History says a won't compile either way. */
            a[i++] = "char b[(sizeof(a.";
            a[i++] = Field;
            a[i++] = ") == sizeof(";
            a[i++] = Type;
            a[i++] = ")) ? 1 : -1];\n";
            Source = ConcatN(a, i);
            Result = TryCompile(Source);
            free(Source);
            if (Result == FALSE)
                break;

            i = 0;
            a[i++] = Prefix;
            a[i++] = Struct;
            a[i++] = " a;\n";
            a[i++] = "int main() {";
            a[i++] = Type;
            a[i++] = " b;\n";
            a[i++] = "b = (";
            a[i++] = Type;
            a[i++] = ")1;\n";

            a[i++] = "a.";
            a[i++] = Field;
            a[i++] = " = (";
            a[i++] = Type;
            a[i++] = ")1;\n";
            a[i++] = "return (memcmp(&a.";
            a[i++] = Field;
            a[i++] = ", &b, sizeof(b)) != 0);}\n";
            Source = ConcatN(a, i);
            Result = TryCompileAndLinkAndRun(Source);
            free(Source);
        }
    } while(0);

    if (Type[0])
    {
        if (Result)
            Print("%s.%s DOES exist, with type %s\n", Struct, Field, Type);
        else
            Print("%s.%s DOES exist, but is NOT of type %s\n", Struct, Field, Type);
    }
    else
    {
        if (Result)
            Print("%s.%s DOES exist, of unspecified type\n", Struct, Field);
        else
            Print("%s.%s does NOT exist\n", Struct, Field);
    }
    return Result;
}

BOOL CheckGlobalVariable(Prefix, Type, Name)
    char* Prefix;
    char* Type;
    char* Name;
{
    char* Source = 0;
    char* a[20];
    BOOL Result = 0;
    size_t i = 0;

    if (Type[0])
    {
        if (CheckGlobalVariable(Prefix, "", Name) == FALSE)
        {
            return FALSE;
        }
    }

    Print("checking for %s%s%s\n", Type, Type[0] ? " " : "", Name);

    a[i++] = Prefix;
    a[i++] = "int main() { ";
    a[i++] = Type[0] ? Type : "void";
    a[i++] = "* a = &";
    a[i++] = Name;
    a[i++] = "; return 0;}\n";
    Source = ConcatN(a, i);
    Result = TryCompileAndLink(Source);
    free(Source);

    /* Again, compilers are very lenient and allow mixing pointer types without error,
    so try to check a little better. Even these checks can fail. */

    do
    {
        if (Type[0] && Result)
        {
            i = 0;
            a[i++] = Prefix;
            /* char a[(sizeof(b) == sizeof(c)) ? 1 : -1];
               is legal if the sizes are the same, else illegal.
            Remember to test this on SGI. History says a won't compile either way. */
            a[i++] = "char b[(sizeof(";
            a[i++] = Name;
            a[i++] = ") == sizeof(";
            a[i++] = Type;
            a[i++] = ")) ? 1 : -1];\n";
            Source = ConcatN(a, i);
            Result = TryCompile(Source);
            free(Source);
            if (Result == FALSE)
                break;

            i = 0;
            a[i++] = Prefix;
            a[i++] = "int main() {";
            a[i++] = Type;
            a[i++] = " a;\n";

            a[i++] = "a = (";
            a[i++] = Type;
            a[i++] = ")1;\n";

            a[i++] = Name;
            a[i++] = " = (";
            a[i++] = Type;
            a[i++] = ")1;\n";
            a[i++] = "return (memcmp(&";
            a[i++] = Name;
            a[i++] = ", &a, sizeof(a) != 0));}\n";
            Source = ConcatN(a, i);
            Result = TryCompileAndLinkAndRun(Source);
            free(Source);

            /* loop through "all possible types", int, long, float, until assigning
            the address of a field to such a pointer successfuly compiles; user could
            feed us list of possible types, such as other structs. */
        }
    } while(0);

    if (Type[0])
    {
        if (Result)
            Print("%s DOES exist, of type %s\n", Name, Type);
        else
            Print("%s does NOT exist or is not of type %s\n", Name, Type);
    }
    else
    {
        if (Result)
            Print("%s DOES exist, of unspecified type\n", Name);
        else
            Print("%s does NOT exist, of any type\n", Name);
    }
    return Result;
}

void StackDirection(a)
    char* a;
{
    char b = 0;

    if (a > &b)
        Print("stack grows down\n");
    else
        Print("stack grows up\n");
}

void Config()
{
    union {
        char bytes[sizeof(int)];
        int value;
    } endian;

    memset(&endian, 0, sizeof(endian));
    endian.bytes[0] = 1;

    LogFile = fopen("config.log", "w");
    if (LogFile == NULL)
    {
        Print("unable to open config.log\n");
        exit(1);
    }

    endian.value = (endian.value == 1);
    Print(endian.value ? "little endian\n" : "big endian\n");

    StackDirection(&endian.bytes[0]);

    SanityCheck();

    /* get the alignments and check that infrastructure works */

    DEFINE_INTEGER_TYPE(short);
    DEFINE_INTEGER_TYPE(int);
    DEFINE_INTEGER_TYPE(long); /* WordSize, except on Win64 */
    DEFINE_INTEGER_TYPE(__int64);

    DEFINE_INTEGER_TYPE(unsigned short);
    DEFINE_INTEGER_TYPE(unsigned int);
    DEFINE_INTEGER_TYPE(unsigned long); /* WordSize, except on Win64 */
    DEFINE_INTEGER_TYPE(unsigned __int64);

    DEFINE_INTEGER_TYPE(size_t);
    DEFINE_INTEGER_TYPE(ptrdiff_t);

    DEFINE_INTEGER_TYPE(pid_t);
    DEFINE_INTEGER_TYPE(gid_t);
    DEFINE_INTEGER_TYPE(clock_t);
    DEFINE_INTEGER_TYPE(uid_t);
    DEFINE_INTEGER_TYPE(time_t);
    DEFINE_INTEGER_TYPE(off_t);
    DEFINE_INTEGER_TYPE(mode_t);
    DEFINE_INTEGER_TYPE(socklen_t);

    {
        timeval_t tv;

        DEFINE_INTEGER_FIELD_TYPE(timeval_t, tv_sec, tv.tv_sec, tv_sec_t);
        DEFINE_INTEGER_FIELD_TYPE(timeval_t, tv_usec, tv.tv_usec, tv_usec_t);

        /* Check that timeval_t has only these two fields, in this order. */
        CHECK(&tv == (void*)&tv.tv_sec);
        CHECK((&tv.tv_sec + 1) == &tv.tv_usec);
        CHECK((&tv.tv_usec + 1) == (void*)(&tv + 1));
    }

    {
        /* Check that timeval_t has only these two fields, in this order, this type. */
        typedef struct { int tz_minuteswest, tz_dsttime; } timezone2_t;
        typedef struct timezone timezone_t;

        timezone_t a;
        timezone2_t b;
        timezone_t* p = 0;
        timezone2_t* q = 0;

        CHECK(sizeof(a) == sizeof(b));
        CHECK(ALIGN_OF_TYPE(timezone_t) == ALIGN_OF_TYPE(timezone2_t));

        CHECK(GetIntegerType(sizeof(a.tz_minuteswest), IS_FIELD_SIGNED(a.tz_minuteswest))
            == GetIntegerType(sizeof(b.tz_minuteswest), IS_FIELD_SIGNED(b.tz_minuteswest)));

        CHECK(GetIntegerType(sizeof(a.tz_dsttime), IS_FIELD_SIGNED(a.tz_dsttime))
            == GetIntegerType(sizeof(b.tz_dsttime), IS_FIELD_SIGNED(b.tz_dsttime)));

        CHECK(&p->tz_minuteswest == &q->tz_minuteswest);
        CHECK(&p->tz_dsttime == &q->tz_dsttime);
    }

    {
        /* Check that timespec has only these two fields, in this order, this type. */
        typedef struct { time_t tv_sec; long tv_nsec; } timespec2_t;
        typedef struct timespec timespec_t;

        timespec_t a;
        timespec2_t b;
        timespec_t* p = 0;
        timespec2_t* q = 0;

        CHECK(sizeof(a) == sizeof(b));
        CHECK(ALIGN_OF_TYPE(timespec_t) == ALIGN_OF_TYPE(timespec2_t));

        CHECK(GetIntegerType(sizeof(a.tv_sec), IS_FIELD_SIGNED(a.tv_sec))
            == GetIntegerType(sizeof(b.tv_sec), IS_FIELD_SIGNED(b.tv_sec)));

        CHECK(GetIntegerType(sizeof(a.tv_nsec), IS_FIELD_SIGNED(a.tv_nsec))
            == GetIntegerType(sizeof(b.tv_nsec), IS_FIELD_SIGNED(b.tv_nsec)));

        CHECK(&p->tv_sec == &q->tv_sec);
        CHECK(&p->tv_nsec == &q->tv_nsec);
    }

    {
        /* Check that itimerval has only these two fields, in this order, this type. */
        typedef struct { timeval_t it_interval, it_value; } itimerval2_t;
        typedef struct itimerval itimerval_t;

        itimerval_t a;
        itimerval2_t b;
        itimerval_t* p = 0;
        itimerval2_t* q = 0;

        CHECK(sizeof(a) == sizeof(b));
        CHECK(ALIGN_OF_TYPE(itimerval_t) == ALIGN_OF_TYPE(itimerval2_t));

        CHECK(sizeof(a.it_interval) == sizeof(b.it_interval));
        CHECK(sizeof(a.it_value) == sizeof(b.it_value));

        CHECK(&p->it_interval == &q->it_interval);
        CHECK(&p->it_value == &q->it_value);
    }

    FindDevNull();
    FindCompiler();

#if 0
    CheckHeader("<time.h>");

    CheckField("#include <time.h>\n", "struct tm", "int", "tm_sec");
    CheckField("#include <time.h>\n", "struct tm", "int", "tm_min");
    CheckField("#include <time.h>\n", "struct tm", "int", "tm_hour");
    CheckField("#include <time.h>\n", "struct tm", "int", "tm_mday");
    CheckField("#include <time.h>\n", "struct tm", "int", "tm_mon");
    CheckField("#include <time.h>\n", "struct tm", "int", "tm_year");
    CheckField("#include <time.h>\n", "struct tm", "int", "tm_wday");
    CheckField("#include <time.h>\n", "struct tm", "int", "tm_yday");
#endif

    /* test code */
    CheckField("#include <time.h>\n", "struct tm", "int", "tm_isdst");
    CheckField("#include <time.h>\n", "struct tm", "long", "tm_isdst");
    CheckField("#include <time.h>\n", "struct tm", "float", "tm_isdst");

    CheckField("#include <time.h>\n", "struct tm", "long", "tm_gmtoff");
    CheckField("#include <time.h>\n", "struct tm", "char*", "tm_zone");
    {
        typedef struct tm tm_t;
        typedef struct { int tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year, tm_wday, tm_yday, tm_isdst; } tm2_t;
        tm_t a;
        tm2_t b;

        CHECK(sizeof(tm_t) >= sizeof(tm2_t));

#define X(x) \
do { \
    CHECK(offsetof(tm_t, x) == offsetof(tm2_t, x)); \
    CHECK(sizeof(a.x) == sizeof(b.x)); \
    CHECK(IS_FIELD_SIGNED(a.x) == IS_FIELD_SIGNED(b.x)); \
} while(0)
        X(tm_sec);
        X(tm_min);
        X(tm_hour);
        X(tm_mday);
        X(tm_mon);
        X(tm_year);
        X(tm_wday);
        X(tm_yday);
        X(tm_isdst);
#undef X
    }

    /* one we know which fields exist, the next thing is to construct a program
    that checks that we have all the fields, sort them by offset, and declare a;
    or write a little more Modula-3 code in C */

    /* test code */
    CheckField("#include <time.h>\n", "struct tm", "float", "tm_sec");
    CheckField("#include <time.h>\n", "struct tm", "float", "tm_zone");

    /* The underscore names are favored on Cygwin, since they are macros for functions. */

    if (CheckGlobalVariable("#include <time.h>\n", "int", "_daylight") == FALSE)
    {
        CheckGlobalVariable("#include <time.h>\n", "int", "daylight");
    }

    /* Is this always long or sometimes int? */
    if (CheckGlobalVariable("#include <time.h>\n", "long", "_timezone") == FALSE)
    {
        CheckGlobalVariable("#include <time.h>\n", "long", "timezone");
    }

    {
        /* test code */
        CheckGlobalVariable("int i;\n", "int", "i");
        CheckGlobalVariable("int i;\n", "int", "j");
        CheckGlobalVariable("int i;\n", "float", "i");
        CheckGlobalVariable("int i;\n", "float", "j");
        CheckGlobalVariable("int i;\n", "double", "i");

        CheckField("typedef struct { int i;} T;\n", "T", "int ", "j");
        CheckField("typedef struct { int i;} T;\n", "T", "int", "j");
        CheckField("typedef struct { int i;} T;\n", "T", "float", "j");
        CheckField("typedef struct { int i;} T;\n", "T", "float", "j");
    }

    {
        /*
        We should probably just write C code for this.

          hostent_addrtype_t = int16_t;
          hostent_length_t = int16_t;

          struct_hostent = RECORD
            h_name:       char_star;
            h_aliases:    char_star_star;
            h_addrtype:   hostent_addrtype_t;
            h_length:     hostent_length_t;
            h_addr_list:  char_star_star;
          END;
        */
        typedef struct hostent hostent_t;
        hostent_t hostent;
        char* t1;
        char* t2;

        DEFINE_INTEGER_FIELD_TYPE(hostent_t, h_addrtype, hostent.h_addrtype, hostent_addrtype_t);
        DEFINE_INTEGER_FIELD_TYPE(hostent_t, h_length, hostent.h_length, hostent_length_t);

        /* Check that all these fields are present, in this order, and no more. */

        CHECK(&hostent == (void*)&hostent.h_name);
        CHECK((&hostent.h_name + 1) == (void*)&hostent.h_aliases);
        CHECK((&hostent.h_aliases + 1) == (void*)&hostent.h_addrtype);
        CHECK((&hostent.h_addrtype + 1) == (void*)&hostent.h_length);
        CHECK((&hostent.h_length + 1) == (void*)&hostent.h_addr_list);
        CHECK((&hostent.h_addr_list + 1) == (void*)(&hostent + 1));

        t1 = GetIntegerType(sizeof(hostent.h_addrtype), IS_FIELD_SIGNED(hostent.h_addrtype));
        t2 = GetIntegerType(sizeof(hostent.h_length), IS_FIELD_SIGNED(hostent.h_length));

        Print("hostent_addrtype_t = %s;\nhostent_length_t = %s;\n", t1, t2);
    }

    {
        /*
        We should probably just write C code for this.

        struct linger {
          unsigned short	l_onoff;	Linger active
          unsigned short	l_linger;	How long to linger for
        };
        */
        typedef struct linger linger_t;
        linger_t linger;
        char* t1;
        char* t2;

        /* Assert these are the only two fields. We also don't allow padding, if we can get
        away with that. We can loosen these restrictions if needed. (see unfinished
        ReconstituteStruct; though really, just writing more C is a good solution).
        */
        CHECK(&linger == (void*)&linger.l_onoff);
        CHECK((&linger + 1) == (void*)(&linger.l_linger + 1));
        CHECK(&linger.l_linger == (&linger.l_onoff + 1));

        t1 = GetIntegerType(sizeof(linger.l_onoff), IS_FIELD_SIGNED(linger.l_onoff));
        t2 = GetIntegerType(sizeof(linger.l_linger), IS_FIELD_SIGNED(linger.l_linger));
        CHECK(t1 == t2);

        Print("struct_linger = RECORD\n  l_onoff: %s;\n  l_linger: %s;\nEND;\n", t1, t1);
    }

    {
        /* test code */
        typedef struct { unsigned char a;
#ifdef __STDC__
        signed
#endif
            char b; unsigned short c; short d; unsigned int e;
        int f; unsigned __int64 g; __int64 h; } T;
        T t;

        DEFINE_INTEGER_FIELD_TYPE(T, a, t.a, a_t);
        DEFINE_INTEGER_FIELD_TYPE(T, b, t.b, b_t);
        DEFINE_INTEGER_FIELD_TYPE(T, c, t.c, c_t);
        DEFINE_INTEGER_FIELD_TYPE(T, d, t.d, d_t);
        DEFINE_INTEGER_FIELD_TYPE(T, e, t.e, e_t);
        DEFINE_INTEGER_FIELD_TYPE(T, f, t.f, f_t);
        DEFINE_INTEGER_FIELD_TYPE(T, g, t.g, g_t);
        DEFINE_INTEGER_FIELD_TYPE(T, h, t.h, h_t);
    }

    DEFINE_OPAQUE_TYPE(pthread_t);
    DEFINE_OPAQUE_TYPE(pthread_attr_t);
    DEFINE_OPAQUE_TYPE(pthread_mutex_t);
    DEFINE_OPAQUE_TYPE(pthread_cond_t);
    DEFINE_OPAQUE_TYPE(pthread_key_t);

    DEFINE_OPAQUE_TYPE(jmp_buf);

#ifdef _SIGRTMAX
    printf("_SIGRTMAX = %u;\n", _SIGRTMAX);
#else
    printf("_SIGRTMAX not defined\n");
#endif
#ifdef SIGRTMAX
    printf("SIGRTMAX = %u;\n", SIGRTMAX);
#else
    printf("SIGRTMAX not defined\n");
#endif
#ifdef SIGUSR2
    printf("SIGUSR2 = %u\n", SIGUSR2);
#else
    printf("SIGUSR2 not defined\n");
#endif
#ifdef _SIGUSR2
    printf("_SIGUSR2 = %u\n", _SIGUSR2);
#else
    printf("_SIGUSR2 not defined\n");
#endif

#ifdef _NSIG
    printf("_NSIG = %u\n", _NSIG);
#else
    printf("_NSIG not defined\n");
#endif

#ifndef _WIN32
    /* test code */
    {
        typedef struct stat stat_t;
        DEFINE_OPAQUE_TYPE(stat_t);
    }
#endif

    /* There are two known definitions of uin.h. Check for each. */
/*
  struct_in_addr = RECORD
    s_addr: unsigned;
  END;

  struct_sockaddr_in = RECORD
    sin_len: unsigned_char; (* This is absent on most platforms. *)
    sin_family: unsigned_char; (* This is 16 bits on most platforms. *)
    sin_port: unsigned_short;
    sin_addr: struct_in_addr;
    sin_zero: ARRAY [0..7] OF char;
  END;

  struct_sockaddr_in = RECORD
    sin_family: unsigned_short; (* this is signed on some platforms; a does not matter *)
    sin_port: unsigned_short;
    sin_addr: struct_in_addr;
    sin_zero: ARRAY [0..7] OF char;
  END;
*/
    {
        typedef struct in_addr in_addr_t;
        typedef struct { unsigned addr; } in_addr2_t;
        in_addr2_t a;
        in_addr_t b;
        CHECK(sizeof(a) == sizeof(b));
        CHECK(ALIGN_OF_TYPE(in_addr2_t) == ALIGN_OF_TYPE(in_addr_t));
        a.addr = 1234;
        b.s_addr = 1234;
        CHECK(memcmp(&a, &b, sizeof(a)) == 0);
    }
    {
        typedef struct in_addr in_addr_t;
        typedef struct sockaddr_in sockaddr_in_t;
        typedef struct { unsigned short family, port; in_addr_t addr; char zero[8]; } sockaddr_in_nolen_t;
        typedef struct { unsigned char len, family; unsigned short port; in_addr_t addr; char zero[8]; } sockaddr_in_len_t;
        sockaddr_in_t a;
        sockaddr_in_nolen_t nolen;
        sockaddr_in_len_t len;
        const static char Prefix[] = "#include <netinet/in.h>\n";

        /* This could be relaxed but our memcmps below are sloppy. */

        CHECK(sizeof(len) == sizeof(nolen));
        CHECK(sizeof(len) == sizeof(a));
        CHECK(ALIGN_OF_TYPE(sockaddr_in_len_t) == ALIGN_OF_TYPE(sockaddr_in_nolen_t));
        CHECK(ALIGN_OF_TYPE(sockaddr_in_len_t) == ALIGN_OF_TYPE(sockaddr_in_t));

        /* A more correct check. */
        CHECK((sizeof(a) == sizeof(len)) || (sizeof(a) == sizeof(nolen)));
        CHECK((ALIGN_OF_TYPE(sockaddr_in_t) == ALIGN_OF_TYPE(sockaddr_in_len_t)) || (ALIGN_OF_TYPE(sockaddr_in_t) == ALIGN_OF_TYPE(sockaddr_in_nolen_t)));

        CHECK((sizeof(a.sin_family) == 2) || (sizeof(a.sin_family) == 1));
        CHECK(sizeof(a.sin_port) == 2);
        CHECK(sizeof(a.sin_addr) == 4);
        CHECK(sizeof(a.sin_zero) == 8);

        memset(&a, 0, sizeof(a));
        memset(&len, 0, sizeof(len));
        memset(&nolen, 0, sizeof(nolen));

        nolen.family = len.family = a.sin_family = 1;
        nolen.port = len.port = a.sin_port = 2;
        nolen.addr.s_addr = len.addr.s_addr = a.sin_addr.s_addr = 3;
        nolen.zero[0] = len.zero[0] = a.sin_zero[0] = 4;
        nolen.zero[1] = len.zero[1] = a.sin_zero[1] = 5;
        nolen.zero[2] = len.zero[2] = a.sin_zero[2] = 6;
        nolen.zero[3] = len.zero[3] = a.sin_zero[3] = 7;
        nolen.zero[4] = len.zero[4] = a.sin_zero[4] = 8;
        nolen.zero[5] = len.zero[5] = a.sin_zero[5] = 9;
        nolen.zero[6] = len.zero[6] = a.sin_zero[6] = 10;
        nolen.zero[7] = len.zero[7] = a.sin_zero[7] = 11;

        CHECK((memcmp(&a, &len, sizeof(len)) == 0) || (memcmp(&a, &nolen, sizeof(nolen)) == 0));

        /* This check is confused on a big endian system, so.. */
#if 0
        if (memcmp(&a, &len, sizeof(len)) == 0)
        {
            CheckField(Prefix, "struct sockaddr_in", "unsigned char", "len");
            CHECK(sizeof(a.sin_family) == 1);
            printf("sockaddr_in_t => has len field\n");
        }
        else
        {
            CHECK(memcmp(&a, &nolen, sizeof(nolen)) == 0);
            CHECK(sizeof(a.sin_family) == 2);
            printf("sockaddr_in_t => no len field\n");
        }
#else
        if (sizeof(a.sin_family) == 2)
        {
            CHECK(memcmp(&a, &nolen, sizeof(nolen)) == 0);
            if (CheckField(Prefix, "struct sockaddr_in", "unsigned char", "len") == TRUE)
            {
                printf("ERROR: confused about sockaddr_in_t.len\n");
                exit(1);
            }
            printf("sockaddr_in_t => no len field\n");
        }
        else if (sizeof(a.sin_family) == 1)
        {
            CHECK(memcmp(&a, &len, sizeof(len)) == 0);
            if (CheckField(Prefix, "struct sockaddr_in", "unsigned char", "len") == FALSE)
            {
                printf("ERROR: confused about sockaddr_in_t.len\n");
                exit(1);
            }
            printf("sockaddr_in_t => len field\n");
        }
        else
        {
            printf("ERROR: confused about sockaddr_in_t\n");
            exit(1);
        }
#endif
    }

    {
        Field_t Fields[30];
        size_t i;
        Struct_t Struct;
        char* Prefix;

        memset(&Struct, 0, sizeof(Struct));
        memset(&Fields, 0, sizeof(Fields));
        
        Struct.NativeName = "stat_t";
        Prefix = "#include <sys/stat.h>\ntypedef struct stat stat_t;\n";
        Struct.MyName = "mystat_t";
        i = 0;
        Fields[i++].Name = "st_size";
        Fields[i++].Name = "st_dev";
        Fields[i++].Name = "st_ino";
        Fields[i++].Name = "st_mode";
        Fields[i++].Name = "st_nlink";
        Fields[i++].Name = "st_nlink";
        Fields[i++].Name = "st_uid";
        Fields[i++].Name = "st_uid";
        Fields[i++].Name = "st_gid";
        Fields[i++].Name = "st_rdev";
        Fields[i++].Name = "st_size";
        Struct.Fields = Fields;
        Struct.NumberOfFields = i;
        ReconstituteStruct(Prefix, &Struct, "mystat.h");
    }

    Print("done\n");

    fclose(LogFile);
}

int main()
{
    Config();
    return 0;
}
