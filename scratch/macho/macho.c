#include <stdlib.h>
#include <string.h>
#include "macho.h"
#include <stdio.h>
#include <stddef.h>
#include <limits.h>
#ifdef _WIN32
#define PRINT64 "I64"
#else
#define PRINT64 "ll"
#endif
#define NUMBER_OF(a) (sizeof(a)/sizeof((a)[0]))
#define STRINGIZEx(a) #a
#define STRINGIZE(a) STRINGIZEx(a)
#define PASTEx(a, b) a##b
#define PASTE(a, b) PASTEx(a, b)

/* const means static const in C++, and extern const in C,
 * "extern const" portable means the same thing in either, but gcc warns.
 */
#if defined(__cplusplus) || !defined(__GNUC__)
#define extern_const extern const
#else
#define extern_const const
#endif

typedef struct _field_t {
    char name[14];
    uchar offset;
    uchar size;
    uchar element_size;
    uchar str;
} field_t;

#define FIELD(t, f) {STRINGIZE(f), offsetof(t, f), sizeof((((t*)0)->f))}
#define FIELD_ARRAY(t, f) {STRINGIZE(f), offsetof(t, f), sizeof((((t*)0)->f)), sizeof((((t*)0)->f)[0]) }
#define FIELD_STRING(t, f) {STRINGIZE(f), offsetof(t, f), sizeof((((t*)0)->f)), sizeof((((t*)0)->f)[0]), 1 }

typedef struct _struct_t {
    const char* name;
    uint    size;
    uint    nfields;
    const field_t* fields;
    int widest_field;
} struct_t;

#define STRUCT(a) {STRINGIZE(a), sizeof(a), NUMBER_OF(PASTE(a,_fields)), PASTE(a,_fields)}

extern_const field_t macho_header32_t_fields[] = {
    FIELD(macho_header32_t, magic),
    FIELD(macho_header32_t, cputype),
    FIELD(macho_header32_t, cpusubtype),
    FIELD(macho_header32_t, filetype),
    FIELD(macho_header32_t, ncmds),
    FIELD(macho_header32_t, sizeofcmds),
    FIELD(macho_header32_t, flags)
};

extern_const field_t macho_header64_t_fields[] = {
    FIELD(macho_header64_t, magic),
    FIELD(macho_header64_t, cputype),
    FIELD(macho_header64_t, cpusubtype),
    FIELD(macho_header64_t, filetype),
    FIELD(macho_header64_t, ncmds),
    FIELD(macho_header64_t, sizeofcmds),
    FIELD(macho_header64_t, flags),
    FIELD(macho_header64_t, reserved)
};

struct_t struct_macho_header32 = STRUCT(macho_header32_t);
struct_t struct_macho_header64 = STRUCT(macho_header64_t);

typedef union _macho_load_command_u {
    macho_loadcommand_t base;
    macho_segment32_t segment32;
    macho_segment64_t segment64;
} macho_load_command_u;

typedef struct _macho_file_t {
    const char* path;
    uchar* contents;
    size_t size;
    macho_header32_t* mh32;
    macho_header64_t* mh64;
    uchar m64;
    uchar swapped;
} macho_file_t;

uint32 swap32(uint32 a)
{
    return (a >> 24) | ((a >> 8) & 0xFF00) | ((a << 8) & 0xFF0000) | (a << 24);
}

uint64 swap64(uint64 a)
{
    const uint64 FF = 0xFF;
    return (a >> 56)
        | ((a >> 40) & (FF << 8))
        | ((a >> 24) & (FF << 16))
        | ((a >>  8) & (FF << 24))
        | ((a <<  8) & (FF << 32))
        | ((a << 24) & (FF << 40))
        | ((a << 40) & (FF << 48))
        | (a << 56);
}

uint32 macho_swap32(macho_file_t* m, uint32 a)
{
    return m->swapped ? swap32(a) : a;
}

void swap32p(uint32* a)
{
    *a = swap32(*a);
}

int char_is_printable(char ch)
{
    return (ch >= 0x20 && ch <= 0x7E);
}

char char_to_printable(char ch)
{
    return char_is_printable(ch) ? ch : '.';
}

void adjust_hex_case(char* a)
{
    a = strchr(a, 'X');
    if (a) *a = 'x';
    else return;
    a = strchr(a, 'X');
    if (a) *a = 'x';
}

void field_print(struct_t* s, const field_t* f, void* p)
{
    char buffer[256];
    char* cursor = buffer;
    uchar* q = (f->offset + (uchar*)p);
    uint32* q32 = (uint32*)q;
    uint64* q64 = (uint64*)q;
    uint i = {0};
    uchar size = f->size;
    uchar str = f->str;
    cursor += sprintf(cursor, "%s.%-*s ", s->name, s->widest_field, f->name);
    if (str)
    {
        uint length = 0;
        for (i = 0; i < size && q[i]; ++i)
            length += 1;
        while (length < 16)
        {
            length += 1;
            *cursor++ = ' ';
        }
        for (i = 0; i < size && q[i]; ++i)
            *cursor++ = q[i];
        *cursor++ = 0;
    }
    else
    {
        if (size == 4)
            i = sprintf(cursor, "%#16lX %#lX", (ulong)*q32, (ulong)swap32(*q32));
        else if (size == 8)
            i = sprintf(cursor, "%#16" PRINT64 "X %#" PRINT64 "X", *q64, swap64(*q64));
        adjust_hex_case(cursor);
        cursor += i;
    }
    printf("%s\n", buffer);
}

void struct_print(struct_t* t, void* p)
{
    uint i;
    int widest_field = t->widest_field;
    if (widest_field == 0)
    {
        for (i = 0; i < t->nfields; ++i)
        {
            size_t j = strlen(t->fields[i].name);
            if (j > widest_field && j <= INT_MAX)
                widest_field = (int)j;
        }
        t->widest_field = widest_field;
     }
    for (i = 0; i < t->nfields; ++i)
        field_print(t, &t->fields[i], p);
}

const char* macho_loadcommand_name(uint32_t cmd)
{
    static char a[99];
    switch (cmd & ~macho_loadcommand_require_dyld)
    {
#define X(a) case macho_loadcommand_##a & ~macho_loadcommand_require_dyld: return #a;
    X(segment32)
    X(symtab)
    X(symseg)
    X(thread)
    X(unixthread)
    X(fixed_vm_lib)
    X(id_fixed_vm_lib)
    X(ident)
    X(fixed_vm_file)
    X(prepage)
    X(dysymtab)
    X(load_dylib)
    X(id_dylib)
    X(load_dylinker)
    X(id_dylinker)
    X(prebound_dylib)
    X(routines32)
    X(sub_framework)
    X(sub_umbrella)
    X(sub_client)
    X(sub_library)
    X(twolevel_hints)
    X(prebind_checksum)
    X(load_weak_dylib)
    X(segment64)
    X(routines64)
    X(uuid)
    X(rpath)
    X(code_signature)
    X(segment_split_info)
    X(reexport_dylib)
    X(lazy_load_dylib)
    X(encryption_info)
#undef X
    }
    sprintf(a, "unknown (%u)", cmd);
    return a;
}

macho_loadcommand_t* macho_first_load_command(macho_file_t* m)
{
    return (m->m64 ? (macho_loadcommand_t*)(m->mh64 + 1) : (macho_loadcommand_t*)(m->mh32 + 1));
}

macho_loadcommand_t* macho_next_load_command(macho_file_t* m, macho_loadcommand_t* L)
{
    return (macho_loadcommand_t*)(macho_swap32(m, L->cmdsize) + (uchar*)L);
}

uint macho_ncmds(macho_file_t* m)
{
    return macho_swap32(m, m->m64 ? m->mh64->ncmds : m->mh32->ncmds);
}

extern_const field_t macho_segment32_t_fields[] = {
    FIELD(macho_segment32_t, cmd),
    FIELD(macho_segment32_t, cmdsize),
    FIELD_STRING(macho_segment32_t, segname),
    FIELD(macho_segment32_t, vmaddr),
    FIELD(macho_segment32_t, vmsize),
    FIELD(macho_segment32_t, fileoff),
    FIELD(macho_segment32_t, filesize),
    FIELD(macho_segment32_t, maxprot),
    FIELD(macho_segment32_t, initprot),
    FIELD(macho_segment32_t, nsects),
    FIELD(macho_segment32_t, flags)
};

struct_t struct_macho_segment32 = STRUCT(macho_segment32_t);

extern_const field_t macho_segment64_t_fields[] = {
    FIELD(macho_segment64_t, cmd),
    FIELD(macho_segment64_t, cmdsize),
    FIELD_STRING(macho_segment64_t, segname),
    FIELD(macho_segment64_t, vmaddr),
    FIELD(macho_segment64_t, vmsize),
    FIELD(macho_segment64_t, fileoff),
    FIELD(macho_segment64_t, filesize),
    FIELD(macho_segment64_t, maxprot),
    FIELD(macho_segment64_t, initprot),
    FIELD(macho_segment64_t, nsects),
    FIELD(macho_segment64_t, flags)
};

struct_t struct_macho_segment64 = STRUCT(macho_segment64_t);

extern_const field_t macho_section32_t_fields[] = {
    FIELD_STRING(macho_section32_t, sectname),
    FIELD_STRING(macho_section32_t, segname),
    FIELD(macho_section32_t, addr),
    FIELD(macho_section32_t, size),
    FIELD(macho_section32_t, offset),
    FIELD(macho_section32_t, align),
    FIELD(macho_section32_t, reloff),
    FIELD(macho_section32_t, nreloc),
    FIELD(macho_section32_t, flags),
    FIELD(macho_section32_t, reserved1),
    FIELD(macho_section32_t, reserved2)
};

struct_t struct_macho_section32 = STRUCT(macho_section32_t);

extern_const field_t macho_section64_t_fields[] = {
    FIELD_STRING(macho_section64_t, sectname),
    FIELD_STRING(macho_section64_t, segname),
    FIELD(macho_section64_t, addr),
    FIELD(macho_section64_t, size),
    FIELD(macho_section64_t, offset),
    FIELD(macho_section64_t, align),
    FIELD(macho_section64_t, reloff),
    FIELD(macho_section64_t, nreloc),
    FIELD(macho_section64_t, flags),
    FIELD(macho_section64_t, reserved1),
    FIELD(macho_section64_t, reserved2),
    FIELD(macho_section64_t, reserved3)
};

struct_t struct_macho_section64 = STRUCT(macho_section64_t);

void macho_dump_load_command_segment32(macho_file_t* m, macho_loadcommand_t* L)
{
    struct_print(&struct_macho_segment32, L);
}

void macho_dump_load_command_segment64(macho_file_t* m, macho_loadcommand_t* L)
{
    struct_print(&struct_macho_segment64, L);
}

void macho_dump_load_command_symtab(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_symseg(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_thread(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_unixthread(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_fixed_vm_lib(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_id_fixed_vm_lib(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_ident(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_fixed_vm_file(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_prepage(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_dysymtab(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_load_dylib(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_id_dylib(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_load_dylinker(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_id_dylinker(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_prebound_dylib(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_routines32(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_sub_framework(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_sub_umbrella(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_sub_client(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_sub_library(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_twolevel_hints(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_prebind_checksum(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_load_weak_dylib(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_routines64(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_uuid(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_rpath(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_code_signature(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_segment_split_info(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_reexport_dylib(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_lazy_load_dylib(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command_encryption_info(macho_file_t* m, macho_loadcommand_t* L)
{
}

void macho_dump_load_command(macho_file_t* m, macho_loadcommand_t* L, uint i)
{
    uint cmd = macho_swap32(m, L->cmd);
    printf("cmd %u %s\n", i, macho_loadcommand_name(cmd));
    switch (cmd)
    {
#define X(a) case macho_loadcommand_##a & ~macho_loadcommand_require_dyld: macho_dump_load_command_##a(m, L); break;
    X(segment32)
    X(symtab)
    X(symseg)
    X(thread)
    X(unixthread)
    X(fixed_vm_lib)
    X(id_fixed_vm_lib)
    X(ident)
    X(fixed_vm_file)
    X(prepage)
    X(dysymtab)
    X(load_dylib)
    X(id_dylib)
    X(load_dylinker)
    X(id_dylinker)
    X(prebound_dylib)
    X(routines32)
    X(sub_framework)
    X(sub_umbrella)
    X(sub_client)
    X(sub_library)
    X(twolevel_hints)
    X(prebind_checksum)
    X(load_weak_dylib)
    X(segment64)
    X(routines64)
    X(uuid)
    X(rpath)
    X(code_signature)
    X(segment_split_info)
    X(reexport_dylib)
    X(lazy_load_dylib)
    X(encryption_info)
#undef X
    }
}

void macho_dump_load_commands(macho_file_t* m)
{
    uint ncmds = macho_ncmds(m);
    uint i;
    macho_loadcommand_t* L = macho_first_load_command(m);
    for (i = 0; i < ncmds; ++i)
    {
        macho_dump_load_command(m, L, i);
        L = macho_next_load_command(m, L);
    }
}

void open_and_read_entire_file(const char* path, uchar** contents, size_t* size)
{
    uchar* buffer = { 0 };
    uchar* prev_buffer = { 0 };
    size_t buffer_size = { 0x10000 / 2 };
    size_t prev_buffer_size = { 0 };
    FILE* file = { 0 };
    size_t bytes_read = { 0 };
    
    *size = 0;
    *contents = 0;
    file = fopen(path, "rb");
    if (!file)
        goto Exit;
    while (1)
    {
        buffer_size *= 2;
        prev_buffer = buffer;
        buffer = malloc(buffer_size);
        if (!buffer)
            goto Exit;
        if (prev_buffer)
        {
            memcpy(buffer, prev_buffer, prev_buffer_size);
            free(prev_buffer);
            prev_buffer = 0;
        }
        bytes_read = fread(buffer + prev_buffer_size, 1, buffer_size - prev_buffer_size, file);
        if (bytes_read < (buffer_size - prev_buffer_size))
            break;
        prev_buffer_size = buffer_size;
    }
Exit:
    if (prev_buffer) free(prev_buffer);
    *size = prev_buffer_size + bytes_read;
    *contents = buffer;
    if (file) fclose(file);
}

int main(int argc, char** argv)
{
    macho_file_t m = { 0 };

    m.path = argv[1];
    open_and_read_entire_file(m.path, &m.contents, &m.size);
    /*printf("%p %lu\n", mh32, (ulong)size);*/
    
    m.mh32 = (macho_header32_t*)m.contents;
    
    m.m64 =     (m.mh32->magic == macho_magic64          || m.mh32->magic == macho_magic64_reversed);
    m.swapped = (m.mh32->magic == macho_magic32_reversed || m.mh32->magic == macho_magic64_reversed);
    if (m.m64)
        m.mh64 = (macho_header64_t*)m.contents;

    if (m.m64)
        struct_print(&struct_macho_header64, m.mh64);
    else
        struct_print(&struct_macho_header32, m.mh32);

    macho_dump_load_commands(&m);
    return 0;
}
