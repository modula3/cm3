#include <stdio.h>
typedef unsigned UINT32;
typedef unsigned long long UINT64;

#ifdef __cplusplus
#define CAST_TO_BITFIELD(bitfield_type, int_type, value) (((bitfield_type(value)))
#else
#define CAST_TO_BITFIELD(bitfield_type, int_type, value) ((bitfield_type)(int_type)value)
#endif

/* type is UINT32 or UINT64 */
#define BITFIELD_TYPE_NAME(type, offset, count)    \
    BITFIELD_##type##_##offset##_##count##_t

#define DEFINE_BITFIELD_TYPE(type, offset, count)   \
typedef union {                                     \
    type value;                                     \
    struct {                                        \
        type xoffset : offset;                       \
        type xvalue : count;                         \
    } bitfield;                                     \
} BITFIELD_TYPE_NAME(type, offset, count);

DEFINE_BITFIELD_TYPE(UINT32, 1, 1)
DEFINE_BITFIELD_TYPE(UINT32, 1, 2)
DEFINE_BITFIELD_TYPE(UINT32, 1, 3)
DEFINE_BITFIELD_TYPE(UINT32, 1, 4)
DEFINE_BITFIELD_TYPE(UINT32, 2, 1)
DEFINE_BITFIELD_TYPE(UINT32, 2, 2)
DEFINE_BITFIELD_TYPE(UINT32, 2, 3)
DEFINE_BITFIELD_TYPE(UINT32, 2, 4)

#define EXTRACT(type, value, offset, count) \
    (CAST_TO_BITFIELD(BITFIELD_TYPE_NAME(type, offset, count), type, value).bitfield.xvalue)

int main()
{
    printf("%X\n", EXTRACT(UINT32, 0xFu, 1, 1));
    printf("%X\n", EXTRACT(UINT32, 0xFu, 1, 2));
    printf("%X\n", EXTRACT(UINT32, 0xFu, 1, 3));
    printf("%X\n", EXTRACT(UINT32, 0xFu, 1, 4));
    printf("%X\n", EXTRACT(UINT32, 0xFu, 2, 1));
    printf("%X\n", EXTRACT(UINT32, 0xFu, 2, 2));
    printf("%X\n", EXTRACT(UINT32, 0xFu, 2, 3));
    printf("%X\n", EXTRACT(UINT32, 0xFu, 2, 4));
    return 0;
}
