#ifndef hashtable_h
#define hashtable_h

#include <stddef.h>

typedef int BOOL;
#if defined(_MSC_VER) || defined(__DECC)
typedef unsigned __int64 UINT64, uint64_t, int64;
#else
typedef unsigned long long UINT64, uint64_t, uint64;
#endif

struct _hashtable_t;
struct _hashtable_entry_t;
struct _hashtable_traits_t;
struct _hashtable_lookup_t;

typedef struct _hashtable_t hashtable_t;
typedef struct _hashtable_entry_t hashtable_entry_t;
typedef struct _hashtable_traits_t hashtable_traits_t;
typedef struct _hashtable_lookup_t hashtable_lookup_t;

struct _hashtable_traits_t
{
    BOOL (*equal)(hashtable_traits_t*, void*, void*);
    uint64_t (*hash)(hashtable_traits_t*, void*);
    size_t key_size;
    size_t value_size;
};

void* hashtable_entry_getkey(hashtable_t* hashtable, hashtable_entry_t* entry);
void* hashtable_entry_getvalue(hashtable_t* hashtable, hashtable_entry_t* entry);
int hashtable_new(hashtable_t** hashtable, hashtable_traits_t* traits);
int hashtable_add(hashtable_t*, void* key, void* value);
void* hashtable_find(hashtable_t*, void* key);
void hashtable_remove(hashtable_t*, void* key);
void hashtable_clear(hashtable_t* hashtable);
void hashtable_first(hashtable_t* hashtable, hashtable_entry_t** entry);
void hashtable_next(hashtable_t* hashtable, hashtable_entry_t** entry);

#endif
