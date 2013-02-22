#include <stdio.h>

typedef unsigned char UINT8;
typedef unsigned short UINT16;
typedef unsigned int UINT32;
typedef unsigned long long UINT64;

#define GCC_VERSION (__GNUC__ * 1000 + __GNUC_MINOR__)

#ifdef __cplusplus
#define ENUM_BITFIELD(TYPE, NAME, SIZE) enum TYPE NAME : SIZE
#elif (GCC_VERSION > 2000)
#define ENUM_BITFIELD(TYPE, NAME, SIZE) __extension__ enum TYPE NAME : SIZE
#else
#define ENUM_BITFIELD(TYPE, NAME, SIZE) unsigned int NAME : SIZE
#endif

/* gcc -fshort-enums makes enums shrink to fit their values.
   adding in a "large" value ensures the size grows to desired.
   This also combats this behavior, if present, in any other compiler. */
#define M3_ENUM_FORCE_UINT16(name) ,M3_ENUM_FORCE_INT_##name = 1 << 15
#define M3_ENUM_FORCE_UINT32(name) ,M3_ENUM_FORCE_INT_##name = ((UINT32)1) << 31
#define M3_ENUM_FORCE_UINT64(name) ,M3_ENUM_FORCE_INT_##name = ((UINT64)1) << 63

#if __GNUC__
#define M3_ENUM_MODE8 __attribute__((__mode__(__QI__)))
#define M3_ENUM_MODE16 __attribute__((__mode__(__HI__)))
#define M3_ENUM_MODE32 __attribute__((__mode__(__SI__)))
#define M3_ENUM_MODE64 __attribute__((__mode__(__DI__)))
#else
#define M3_ENUM_MODE8 /* nothing */
#define M3_ENUM_MODE16 /* nothing */
#define M3_ENUM_MODE32 /* nothing */
#define M3_ENUM_MODE64 /* nothing */
#endif
#if defined(_MSC_VER) && defined(__cplusplus)
#define M3_ENUM_BASE8  : UINT8
#define M3_ENUM_BASE16 : UINT16
#define M3_ENUM_BASE32 : UINT32
#define M3_ENUM_BASE64 : UINT64
#else
#define M3_ENUM_BASE8  /* nothing */
#define M3_ENUM_BASE16 /* nothing */
#define M3_ENUM_BASE32 /* nothing */
#define M3_ENUM_BASE64 /* nothing */
#endif

typedef M3_ENUM_MODE8  enum M3_ENUM_BASE8 { Number8_Zero } Number8;
typedef M3_ENUM_MODE16 enum M3_ENUM_BASE16 { Number16_Zero } Number16;
typedef M3_ENUM_MODE32 enum M3_ENUM_BASE32 { Number32_Zero } Number32;
typedef M3_ENUM_MODE64 enum M3_ENUM_BASE64 { Number64_Zero } Number64;

typedef enum { Number_Zero } Number;
typedef enum { NumberInt16_Zero M3_ENUM_FORCE_UINT16(NumberInt16) } NumberInt16;
typedef enum { NumberInt32_Zero M3_ENUM_FORCE_UINT32(NumberInt32) } NumberInt32;
typedef enum { NumberInt64_Zero M3_ENUM_FORCE_UINT64(NumberInt64) } NumberInt64;

int main()
{
    printf("%X %X %X %X\n%X %X %X %X\n",
        sizeof(Number),
        sizeof(NumberInt16),
        sizeof(NumberInt32),
        sizeof(NumberInt64),
        sizeof(Number8), sizeof(Number16), sizeof(Number32), sizeof(Number64)
        );
    int c9x = 0;
    return 0;
}
