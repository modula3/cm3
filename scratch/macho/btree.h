typedef int BOOL;
typedef signed char int8, INT8;
typedef unsigned char BOOL8, UINT8, uint8_t, uchar, UCHAR;
typedef unsigned short UINT16, uint16_t, uint16;
typedef short int16, INT16;
typedef int int32, INT, INT32;
typedef unsigned UINT32, uint32_t, uint, UINT;
#ifdef _MSC_VER
typedef __int64 int64, INT64, int64_t, int64;
typedef unsigned __int64 UINT64, uint64_t, int64;
#else
typedef long long INT64, int64_t, int64;
typedef unsigned long long UINT64, uint64_t, uint64;
#endif
typedef unsigned long ulong, ULONG, ulong_t;

struct _btree_t;
typedef struct _btree_t btree_t;

struct _btree_traits_t;
typedef struct _btree_traits_t btree_traits_t;

struct _btree_traits_t
{
    uint32_t blocksize;
    int (*compare)(const void*, const void*);
}

void btree_add();
void btree_remove();
void btree_map();
void btree_new();
