#include <stdio.h>

typedef unsigned int UINT32;
#if defined(_MSC_VER) || defined(__DECC) || defined(__int64)
typedef unsigned __int64 UINT64;
#else
typedef unsigned long long UINT64;
#endif
#define m3_extract_T(T) static T m3_extract_##T(T value,unsigned offset,unsigned count){return((value>>offset)&~(((~(T)0))<<count));}
#define m3_insert_T(T) static T m3_insert_##T(T x,T y,unsigned offset,unsigned count) { T mask = (~((~(T)0)<<count)) << offset; return(((y << offset) & mask) | (x & ~mask)); }
m3_extract_T(UINT32)
m3_extract_T(UINT64)
m3_insert_T(UINT32)
m3_insert_T(UINT64)

int main()
{
#define X(x) printf("%s:%llX\n", #x, (unsigned long long)x)

    X(m3_extract_UINT64(0x12345678, 0, 4));
    X(m3_extract_UINT64(0x12345678, 4, 4));
    X(m3_extract_UINT64(0x12345678, 8, 4));
    X(m3_extract_UINT64(0x12345678, 12, 4));
    X(m3_extract_UINT64(0x12345678, 16, 4));
    X(m3_extract_UINT64(0x12345678, 20, 4));
    X(m3_extract_UINT64(0x12345678, 24, 4));
    X(m3_extract_UINT64(0x12345678, 28, 4));

    X(m3_extract_UINT64((((UINT64)0x90ABCDEF) << 32) | 0x12345678, 32, 4));
    X(m3_extract_UINT64((((UINT64)0x90ABCDEF) << 32) | 0x12345678, 36, 4));
    X(m3_extract_UINT64((((UINT64)0x90ABCDEF) << 32) | 0x12345678, 40, 4));
    X(m3_extract_UINT64((((UINT64)0x90ABCDEF) << 32) | 0x12345678, 44, 4));
    X(m3_extract_UINT64((((UINT64)0x90ABCDEF) << 32) | 0x12345678, 48, 4));
    X(m3_extract_UINT64((((UINT64)0x90ABCDEF) << 32) | 0x12345678, 52, 4));
    X(m3_extract_UINT64((((UINT64)0x90ABCDEF) << 32) | 0x12345678, 56, 4));
    X(m3_extract_UINT64((((UINT64)0x90ABCDEF) << 32) | 0x12345678, 60, 4));
    X(m3_extract_UINT64((((UINT64)0x90ABCDEF) << 32) | 0x12345678, 60, 1));
    X(m3_extract_UINT64((((UINT64)0x90ABCDEF) << 32) | 0x12345678, 61, 1));
    X(m3_extract_UINT64((((UINT64)0x90ABCDEF) << 32) | 0x12345678, 62, 1));
    X(m3_extract_UINT64((((UINT64)0x90ABCDEF) << 32) | 0x12345678, 63, 1));
    X(m3_extract_UINT64((((UINT64)0x90ABCDEF) << 32) | 0x12345678, 62, 2));
    X(m3_extract_UINT64((((UINT64)0x90ABCDEF) << 32) | 0x12345678, 61, 3));

    X(m3_extract_UINT32(0x12345678, 0, 4));
    X(m3_extract_UINT32(0x12345678, 4, 4));
    X(m3_extract_UINT32(0x12345678, 8, 4));
    X(m3_extract_UINT32(0x12345678, 12, 4));
    X(m3_extract_UINT32(0x12345678, 16, 4));
    X(m3_extract_UINT32(0x12345678, 20, 4));
    X(m3_extract_UINT32(0x12345678, 24, 4));
    X(m3_extract_UINT32(0x12345678, 28, 4));

    X(m3_extract_UINT32(0x12345678, 0, 5));
    X(m3_extract_UINT32(0x12345678, 4, 5));
    X(m3_extract_UINT32(0x12345678, 8, 5));
    X(m3_extract_UINT32(0x12345678, 12, 5));
    X(m3_extract_UINT32(0x12345678, 16, 5));
    X(m3_extract_UINT32(0x12345678, 20, 5));
    X(m3_extract_UINT32(0x12345678, 24, 5));
    X(m3_extract_UINT32(0x12345678, 28, 5));

    X(m3_extract_UINT32(0x12345678, 0, 8));
    X(m3_extract_UINT32(0x12345678, 0, 10));
    X(m3_extract_UINT32(0x12345678, 0, 12));
    X(m3_extract_UINT32(0x12345678, 0, 20));
    X(m3_extract_UINT32(0x12345678, 4, 1));
    X(m3_extract_UINT32(0x12345678, 8, 1));
    X(m3_extract_UINT32(0x12345678, 16, 8));
    X(m3_extract_UINT32(0x12345678, 16, 16));
    X(m3_extract_UINT32(0x12345678, 16, 20));
    X(m3_extract_UINT32(0x12345678, 24, 1));
    X(m3_extract_UINT32(0x12345678, 28, 1));

    return 0;
}
