
struct _btree_block
{
    uint64_t
};

struct _btree_t
{
    uint32_t block_size;
    uint32_t key_size;
    int (*compare)(const void*, const void*);
    unsigned char* root;
};

struct _btree_entry
{
    uint32_t key_size;
    uint32_t value_size;
    uint64_t key_offset;
    uint64_t value_offset;
};

btree_lookup(btree_t* t, void* key, uint32_t keysize)
{
}

btree_add(btree_t* t, void* key, uint32_t keysize, void* value, uint32_t valuesize)
{
}
