#define _INCLUDE_POSIX_SOURCE
#define _INCLUDE_HPUX_SOURCE
#define _FILE_OFFSET_BITS 64

#include <sys/types.h>
#include <stdio.h>
#include <stddef.h>

#define IS_TYPE_SIGNED(x)  (((x)-1) < (x)0)
#define ALIGN_OF_TYPE(x) (sizeof(struct {char a; x b;}) - sizeof(x))

typedef unsigned U;
typedef int BOOL;

char* BeginComment = "(*";
char* EndComment = "*)";

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
    printf("%s = %s; %s align = %u %s\n", Name, GetIntegerType(Size, Signed), BeginComment, (U)Align, EndComment);
}

#ifdef __STDC__
#define DEFINE_INTEGER_TYPE(x) DefineIntegerType(#x, sizeof(x), IS_TYPE_SIGNED(x), ALIGN_OF_TYPE(x))
#else
#define DEFINE_INTEGER_TYPE(x) DefineIntegerType("x", sizeof(x), IS_TYPE_SIGNED(x), ALIGN_OF_TYPE(x))
#endif

void Config()
{
    DEFINE_INTEGER_TYPE(mode_t);
}

int main()
{
    Config();
    return 0;
}
