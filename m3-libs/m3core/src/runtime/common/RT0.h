/* See RT0.3. */

#include <stddef.h>

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

typedef ptrdiff_t INTEGER;
typedef unsigned char UINT8;
typedef void* ADDRESS;

namespace RT0
{

typedef char* String;
typedef char Fingerprint[8];

typedef struct Brand
{
    INTEGER length;
    char    chars[1];
} *BrandPtr;

typedef void (__cdecl * TypeInitProc)(ADDRESS ref);

struct Typecode
{
    size_t value : 20;
};

struct TypeKind
{
    typedef UINT8 T;
    enum
    {
        Unknown,
        Ref,
        Obj,
        Array
    };
};

struct Typecell;
typedef struct Typecell *TypeDefn;

struct Typecell
{
    Typecode        typecode;
    INTEGER         selfID;
    Fingerprint     fp;
    bool            traced;
    TypeKind::T     kind;
    UINT8           link_state;
    UINT8           dataAlignment;
    INTEGER         dataSize;
    ADDRESS         type_map;       /* RTTypeMap.T */
    ADDRESS         gc_map;         /* reduced RTTypeMap.T for collector */
    ADDRESS         type_desc;      /* enhanced RTTipe map for new pickles */
    TypeInitProc    initProc;       /* called by NEW */
    BrandPtr        brand_ptr;
    String          name;
    TypeDefn        next;
};

}
