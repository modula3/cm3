#include "hash.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct _hashtable_t
{
    hashtable_entry_t** buckets;
    size_t bucket_count;
    size_t entry_count;
    hashtable_traits_t* traits;
    size_t key_size;
    size_t value_size;
    hashtable_entry_t* buckets0[97];
};

struct _hashtable_entry_t
{
    uint64_t hashcode;
    hashtable_entry_t* next;
    /* key and value follow */
};

struct _hashtable_lookup_t
{
    hashtable_t* table;
    void* key;
    hashtable_entry_t* entry;
    hashtable_entry_t* bucket;
    hashtable_entry_t* previous;
    size_t bucket_index;
    uint64_t hashcode;
};

void* hashtable_entry_getkey(hashtable_t* t, hashtable_entry_t* e)
{
    return (e && t->key_size) ? (1 + e) : 0;
}

void* hashtable_entry_getvalue(hashtable_t* t, hashtable_entry_t* e)
{
    return (e && t->value_size) ? (t->key_size + (char*)(1 + e)) : 0;
}

struct hashtable_t
{
    hashtable_traits_t* traits;
};

int hashtable_new(hashtable_t** out_table,
                  hashtable_traits_t* traits)
{
  hashtable_t* table = 0;
  
  *out_table = table;  
  if (!(table = (hashtable_t*)calloc(1, sizeof(*table))))
    return -1;
  *out_table = table;  
  table->bucket_count = sizeof(table->buckets0) / sizeof(table->buckets0[0]);
  table->buckets = table->buckets0;   
  table->traits = traits;
  table->key_size = traits->key_size;
  table->value_size = traits->value_size;
  return 0;
}

void hashtable_lookup(hashtable_lookup_t* lookup)
{
  void* key = lookup->key;
  hashtable_t* table = lookup->table;
  size_t bucket_count = table->bucket_count;
  hashtable_traits_t* traits = table->traits;
  uint64_t hashcode = lookup->hashcode ? lookup->hashcode : traits->hash(traits, lookup->key);
  size_t bucket_index = (hashcode % table->bucket_count);
  hashtable_entry_t* bucket = table->buckets[bucket_index];
  hashtable_entry_t* entry = bucket;
  hashtable_entry_t* previous = { 0 };
  lookup->bucket = bucket;
  lookup->entry = 0;
  lookup->previous = 0;
  lookup->bucket_index = bucket_index;
  lookup->hashcode = hashcode;
  while (entry && (entry->hashcode != hashcode
        || !traits->equal(traits, key, hashtable_entry_getkey(table, entry))))
  {
    previous = entry;
    entry = entry->next;
  }
  if (entry)
  {
    lookup->entry = entry;
    lookup->previous = previous;
  }
}

void hashtable_rebucket(hashtable_t* table)
{
}

void* hashtable_find(hashtable_t* table, void* key)
{
  hashtable_entry_t* entry = { 0 };
  hashtable_lookup_t lookup = { 0 };
  lookup.table = table;
  lookup.key = key;
  hashtable_lookup(&lookup);
  return lookup.entry ? hashtable_entry_getvalue(table, lookup.entry) : 0;
}

int hashtable_add(hashtable_t* table, void* key, void* value)
{
  hashtable_entry_t* entry = { 0 };
  hashtable_lookup_t lookup = { 0 };
  lookup.table = table;
  lookup.key = key;
  hashtable_lookup(&lookup);
  if (entry = lookup.entry)
  {
    memcpy(hashtable_entry_getvalue(table, entry), value, table->value_size);
    return 0;
  }
  entry = (hashtable_entry_t*)calloc(1, table->key_size + table->value_size + sizeof(hashtable_entry_t));
  if (!entry)
    return -1;
  memcpy(hashtable_entry_getkey(table, entry), key, table->key_size);
  memcpy(hashtable_entry_getvalue(table, entry), value, table->value_size);
  entry->next = lookup.bucket;
  entry->hashcode = lookup.hashcode;
  table->buckets[lookup.bucket_index] = entry;
  table->entry_count += 1;
  hashtable_rebucket(table);
  return 0;
}

void hashtable_remove(hashtable_t* table, void* key)
{
  hashtable_entry_t* entry = { 0 };
  hashtable_lookup_t lookup = { 0 };
  size_t bucket_index = { 0 };
  lookup.table = table;
  lookup.key = key;
  hashtable_lookup(&lookup);
  entry = lookup.entry;
  if (!entry)
    return;
  if (lookup.previous)
    lookup.previous->next = entry->next;
  else
    table->buckets[lookup.bucket_index] = 0;
  table->entry_count -= 1;
  free(entry);
}

void hashtable_first(hashtable_t* table, hashtable_entry_t** entry)
{
  size_t a;
  hashtable_entry_t* e = { 0 };
  hashtable_entry_t** buckets = table->buckets;
  size_t bucket_count = table->bucket_count;
  for (a = 0; a < bucket_count && !(e = buckets[a]); ++a)
  {
    /* nothing */
  }
  *entry = e;
}

void hashtable_next(hashtable_t* table, hashtable_entry_t** entry)
{
  hashtable_entry_t* e = *entry;
  hashtable_entry_t* next = e->next;
  size_t a = { 0 };
  hashtable_entry_t** buckets = { 0 };
  size_t bucket_count = { 0 };
  if (next)
  {
    *entry = next;
    return;
  }
  buckets = table->buckets;
  bucket_count = table->bucket_count;
  a = (e->hashcode % bucket_count);
  for (; a < bucket_count && !(e = buckets[a]); ++a)
  {
    /* nothing */
  }
  *entry = e;
}

void hashtable_clear(hashtable_t* table)
{
  /* leaky */
  table->entry_count = 0;
  memset(table->buckets, 0, table->bucket_count * sizeof(table->buckets[0]));
}

#if 1

uint64_t string_hash(hashtable_traits_t* c, void* x)
{
  const unsigned char* a = (const unsigned char*)x;
  uint64_t b = { 0 };
  while (*a)
    b = (b << 6) ^ *a++;
  return b;
}

BOOL string_equal(hashtable_traits_t* x, void* a, void* b)
{
  return !strcmp(a, b);
}

int main()
{
  int value = { 0 };
  hashtable_t* table = { 0 };
  hashtable_traits_t
  traits = { string_equal,
             string_hash,
             sizeof(char*),
             sizeof(int) };
             
  hashtable_new(&table, &traits);
  
  hashtable_add(table, "one", (value = 1, &value));
  hashtable_add(table, "two", (value = 2, &value));
  hashtable_add(table, "three", (value = 3, &value));
  
  printf("%d\n", *(int*)hashtable_find(table, "one"));
  printf("%d\n", *(int*)hashtable_find(table, "two"));
  printf("%d\n", *(int*)hashtable_find(table, "three"));
  
  return 0;
}

#endif
