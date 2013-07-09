/* *INDENT-OFF* */

/* M3 language support routines for GDB, the GNU debugger.
   Copyright 1992, 1993 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#if !defined (M3_UTIL_H)
#define M3_UTIL_H 1

#include "m3-bool.h"

#include "defs.h"
#include "gdbtypes.h"
#include "symtab.h"
#include "value.h"

#define M3_MAX_SYMBOLLEN 350

extern int rt0_tc_selfID_size;
extern int rt0_tc_selfID_offset;
extern int rt0_tc_dataSize_size;
extern int rt0_tc_dataSize_offset;
extern int rt0_tc_kind_size;
extern int rt0_tc_kind_offset;

extern int rt0_dataOffset_size;
extern int rt0_dataOffset_offset;
extern int rt0_methodOffset_size;
extern int rt0_methodOffset_offset;
extern int rt0_parent_size;
extern int rt0_parent_offset;
extern int rt0_defaultMethods_size;
extern int rt0_defaultMethods_offset;

extern CORE_ADDR rt0u_types_value; /* used for SRC, PM3 and EZM3 only. */
extern CORE_ADDR rttype_types_addr; /* Used for CM3 only. */

extern int rttype_info_def_size;
extern int rttype_info_def_offset;
extern int rttype_infomap_map_size;
extern int rttype_infomap_map_offset;
extern int rttype_infomap_cnt_size;
extern int rttype_infomap_cnt_offset;

extern BOOL m3_constant_init_done;

/* We are allowing unsafe operations. */
extern BOOL
is_unsafe ( void );

/* Special value attached to "type" values
   FIXME: There has to be a cleaner way to do this. */
#define M3_TYPE_MAGIC 766579803L
extern const LONGEST m3_type_magic_value;

extern void
m3_emit_char (int, struct ui_file *, int);

extern void
m3_emit_widechar (int, struct ui_file *, int);

extern void
m3_print_char_lit (int, struct ui_file *);

extern void
m3_print_widechar_lit (int, struct ui_file *);

extern void
m3_print_string (
    struct ui_file * stream,
    const gdb_byte * string,
    unsigned int length,
    int width,
    int force_ellipsis
  );

/* Convert Modula-3 numbers into newly allocated values */
extern struct value *
m3_value_from_longest ( struct type *type, LONGEST num);

/* Modula-3 expressions can be either values or types.  m3gdb
   'struct value's represent either, despite the name.  This
   creates a 'struct value' that represents a type. */
extern struct value *
m3_allocate_value_that_is_type ( struct type * type_of_value );

/* Modula-3 expressions can be either values or types.  m3gdb
   'struct value's represent either, despite the name.  This
   distinguishes. */
extern BOOL
m3_value_is_type ( struct value * val );

/* Given a block, return the symtab it belongs to. */
extern struct symtab *
m3_symtab_of_block ( const struct block * blk );

extern
struct type *
find_m3_type_named
  ( char * name
  ,  BOOL must_find /* Emit an error message, if can't find it. */
  );

extern char * find_m3_type_name (struct type *);

/* Lookup a symbol in the one global block associated with block.  */
extern struct symbol *
m3_lookup_symbol_one_global (
    const char *name,
    const char *linkage_name,
    const struct block *block,
    const domain_enum domain,
    struct symtab * * symtab
  );

/* Lookup a symbol in the all static and global blocks.  */
extern struct symbol *
m3_lookup_symbol_all_static_and_global (
    const char *name,
    const char *linkage_name,
    const domain_enum domain,
    struct symtab * * symtab
  );

/* If blk contains a symbol with the right name spelling to be an inteface
   or module global record, return it.  kind == 'M' to find a module global
   symbol, or kind == 'I' for an interface. Set unit_name to the declared name
   of the interface or module. */
extern struct symbol *
m3_block_globals_symbol (
  const struct block * blk, const char kind, char * * unit_name );

/* Return the interface or module global variable record symbol for
   interface or module named 'unit_name'.  kind == 'M' to find a
   module global symbol, or kind == 'I' for an interface.
   For an interface, its demangled name is "I$<interfaceName>".
   For a module, its demangled name is "M$<moduleName>".
   kind should be either 'I' or 'M', for interface or module.
   It could be in any static or global block. name is a terminated
   string (see m3_term_strings_equal).
*/
extern struct symbol *
m3_unit_name_globals_symbol (
    int kind,
    const char *unit_name,
    struct symtab * * symtab
  );

/* Is sym the symbol of a Modula-3 globals record for either an interface
   of a module? */
extern BOOL
m3_is_globals_record_symbol ( const struct symbol * sym );

extern BOOL
m3_is_interface_global_record ( struct symbol * sym );

extern BOOL
m3_is_module_global_record ( struct symbol * sym );

/* Return the pseudo-record-type that has one field for each exported
   interface.  It's demangled name is "H$<moduleName>.
   If the result is NIL, this means that the module exports exactly one
   interface, with the same name as the module.
*/
extern struct type *
find_m3_exported_interfaces ( const char * module_name );

/* Is sym the symbol of a Modula-3 type name? */
extern BOOL
m3_is_type_name_symbol ( const struct symbol * sym );

/* Look in 'block' for a declaration of a type. */
extern struct symbol *
m3_lookup_type (
    const char * unit_name,
    const char * name,
    const struct block * blk,
    struct symtab * * symtab
  );

/* See if identifier 'ident' is declared in interface named 'interface_name'.
   Return its symbol if so, or NULL if not.  EXCEPT: global variables have no
   symbol.  If it's a global variable, return the symbol for the globals
   record for the interface.  Caller will have to detect this case and combine
   it with 'ident' in its own way.  If a symbol is found and symtab is non-NULL,
   set symtab to the containing symbol table.  If this is a procedure declared
   in interface 'interface_name', the symbol and symtab returned will belong
   to the exporting _module_ (this is the only symbol we have.)  This will not
   find a procedure that is not declared in an interface. */
extern struct symbol *
m3_lookup_interface_id (
    const char * interface_name,
    const char * ident,
    struct symtab * * symtab
  );

/* See if identifier 'ident' is declared in module named 'module_name'.
   Return its symbol if so, or NULL if not.  EXCEPT: global variables have no
   symbol.  If it's a global variable, return the globals record for the
   module.  Caller will have to detect this case and combine it with
   'ident' in its own way. */
extern struct symbol *
m3_lookup_module_id (
    const char * module_name,
    const char * ident,
    struct symtab * * symtab
  );

/* See if 'ident' is declared in an exported interface of module named
   'module', which we assume we are currently executing in some block of.
   An exported procedure whose body is actually provided in the module named
   'module_name' should have been previously found in the static or
   global block of the module itself.  */
extern struct symbol *
m3_lookup_exported (
  const char *module_name, const char * ident, struct symtab * * symtab );

/* Which Modula-3 compiler do we have, if any. */

enum m3_compiler_kind_typ
  { m3_ck_unknown,   /* We haven't tested it yet. */
    m3_ck_pm3,       /* It's SRC, PM3, oor EZM3. */
    m3_ck_cm3,       /* It's CM3. */
    m3_ck_not_m3     /* It's not a Modula-3 compiler. */
  };
extern enum m3_compiler_kind_typ m3_compiler_kind_value;

extern void
m3_ascertain_compiler_kind ( void );

extern enum m3_compiler_kind_typ
m3_compiler_kind (void);

/* Strip away any indirect types from a type. */
extern struct type *
m3_direct_type ( struct type * param_type );

/* Strip away any indirect and packed types from a type. */
extern struct type *
m3_unpacked_direct_type ( struct type * param_type );

/* Convert an opaque type to its revealed type, or identity. */
extern struct  type *
m3_revealed_type ( struct type * opaque_type );

/* Strip off any indirects, packeds, and opaques. */
extern struct  type *
m3_revealed_unpacked_direct_type ( struct type * param_type );

/* The base type of param_type, if, after stripping indirects and packeds,
   it is an ordinal type. Otherwise, NULL. */
extern struct type *
m3_ordinal_base_type ( struct type * param_type, BOOL * is_int_or_card );

/* Return the typecode of the object at inferior address addr.
   PRE: addr is the inferior address of a object with a typecode header,
        i.e., either it's a traced ref or an untraced object type.
*/
extern LONGEST
m3_typecode_from_inf_object_addr ( CORE_ADDR addr );

/* Return the inferior address of the typecell for the dyanamic (allocated)
   type of the object at inferior address addr.
*/
extern CORE_ADDR
m3_tc_addr_from_inf_object_addr ( CORE_ADDR );

/* Given a type from a Modula-3 program, return its numeric uid. */
extern LONGEST
int_uid_from_m3_type ( struct type * t );

/* given a gdb type, find the address of the corresponding typecell */
extern CORE_ADDR
m3_tc_addr_from_type ( struct type * t );

/* given the address of a typecell, find the M3 numeric uid for it. */
extern LONGEST
m3_int_uid_from_tc_addr ( CORE_ADDR addr );

/* given the address of a typecell, find the gdb type for it */
extern struct type *
m3_type_from_tc ( CORE_ADDR );

/* Given a heap reference, find it's actual type.
   PRE: addr is the inferior address of a object with a typecode header,
        i.e., either it's a traced ref or an untraced object type.
*/
extern struct type *
m3_allocated_type_from_object_addr ( CORE_ADDR );

extern int
m3_dataOffset_from_tc_addr (CORE_ADDR);

extern int
m3_methodOffset_from_tc_addr (CORE_ADDR);

extern int
m3_dataSize_from_tc_addr (CORE_ADDR);

extern CORE_ADDR
m3_super_tc_addr_from_tc_addr (CORE_ADDR);

extern CORE_ADDR
m3_defaultMethods_from_tc_addr (CORE_ADDR);

extern BOOL
m3_find_rec_field (struct type *, const char *, int *, int *,
                  struct type **);

extern BOOL
m3_find_obj_field (struct type *, char *, int *, int *,
                  struct type **);

extern BOOL
m3_find_obj_method (struct type *obj_type,
                               char *name,
                               int *size, int *offset,
                               struct type **type);

extern BOOL
m3_is_ordinal_type (struct type *);

extern BOOL
m3_type_is_signed ( struct type *type );

extern void
m3_ordinal_bounds (struct type *, LONGEST *, LONGEST *);

/* If range_type is an ordinal type, range-check value against it.
   If the check fails, emit an error (which implies noreturn.) */
extern void
m3_ordinal_range_check (
    LONGEST value, struct type * range_type, char * purpose );

extern gdb_byte *
m3_read_object_fields_bits (CORE_ADDR);

extern LONGEST
m3_extract_ord (const gdb_byte *, int, int, BOOL);

extern CORE_ADDR
m3_extract_address (const gdb_byte *, int);

extern LONGEST
m3_value_as_integer (struct value *);

extern double
m3_value_as_float (struct value *);

extern struct value *
m3_ensure_value_is_unpacked ( struct value * packed_val );

extern int
m3_open_array_dope_align (void);

/* Return the gdb-space byte offset of 'dimension'-th shape component, relative
   to the beginning of open arrray dope. */
extern int
m3_shape_component_offset ( int dimension );

extern CORE_ADDR
m3_open_array_elems_addr ( const gdb_byte * addr );

extern void
m3_set_open_array_elems_addr ( gdb_byte * addr, CORE_ADDR val );

/* Return the dimension-th shape component (i.e., the element count for the
   dimension-th dimension), of the Modula-3 open array whose dope is in
   gdb-space at address. */
extern ULONGEST
m3_open_array_shape_component ( const gdb_byte * addr , int dimension );

/* Store shape component val into the dimension-th slot of Modula-3
   open array dope located in gdb space at address addr. */
extern void
m3_set_open_array_shape_component (
    gdb_byte * addr,
    int dimension,
    ULONGEST val
  );

/* Return the dimension-th shape component (i.e., the element count for the
   dimension-th dimension), of the Modula-3 open array whose dope is in
   gdb-space value array_val. */
extern ULONGEST
m3_value_open_array_shape_component (
  struct value * array_value, int dimension  );

/* Store shape component val into the dimension-th slot of the Modula-3
   open array dope that is in gdb-space value array_val. */
extern void
m3_set_value_open_array_shape_component (
  struct value * array_value, int dimension, ULONGEST val );

/* Fetch the inferior address of the zero-th element of the Modula-3 open array
   whose dope begins at inferior address ref. */
extern CORE_ADDR
m3_inf_open_array_elems_addr ( CORE_ADDR ref );

/* Return the dimension-th shape component (i.e., the element count for the
   dimension-th dimension), of the Modula-3 open array whose dope begins at
   inferior address ref. */
extern ULONGEST
m3_inf_open_array_shape_component ( CORE_ADDR ref, int dimension );

/* Fetch the inferior address of the zero-th element of the Modula-3 open array
   whose dope is in gdb-space value array_val. */
extern CORE_ADDR
m3_value_open_array_elems_addr ( struct value * array_value );

/* Store inferior elements address val into the Modula-3 open array
   dope value located in gdb-space value array_val. */
extern void
m3_set_value_open_array_elems_addr (
    struct value * array_value,
    CORE_ADDR val
  );

/* For a Modula-3 type, we will construct only one indirect type to it.
   So reuse existing pointer-type mechanism from C/C++, but change
   its type code.
   This could duplicate a compiler-generated type, but that would be
   hard to find. */
extern struct type *
m3_indirect_type_from_type ( struct type *type );
#endif /* !defined (M3_UTIL_H) */

/* Where static link is stored, relative to frame locals. */
extern const int static_link_offset;

CORE_ADDR
m3_1st_inf_static_link (struct frame_info *start_frame);

/* From start_frame, traverse the static chain and return the ancestor
   frame whose corresponding procedure block is the same as the procedure
   block containing ancestor_block.  ancestor_block really should be an
   ancestor of the block corresponding to start_frame.
   Also set *ancestor_static_link to the inferior static link value
   corresponding to this frame.  ancestor_static_link can be NULL.
   Emit an error message if can't get a good ancestor frame and static
   link value.
*/
extern struct frame_info *
m3_static_ancestor_frame (
    struct frame_info * start_frame,
    struct block * ancestor_block,
    CORE_ADDR * ancestor_static_link
  );

extern int
m3_proc_closure_align (void);

/* FIXME:  This struct is the wrong way to do this, because it now needs
   to reflect target layout, in addition to values constructed entirely
   within gdb-space.  This declaration relys on the host C compiler's
   layout rules, and these don't necessarily reflect the target Modula-3
   compiler's rules.  Also, get references to it out of other modules. */
struct m3_proc_closure
  { CORE_ADDR closure_mark; /* Contents always == closure_mark, if this is
                               actually a closure. */
    CORE_ADDR code_addr;
    CORE_ADDR env_ptr;
  };

/* FIXME: Get this value out of a header file. */
/* CHECK: Is this value target-dependent? */
extern const CORE_ADDR closure_mark;

extern struct type *
m3_alloc_closure_type ( struct type * proc_type );

/* Build a procedure closure value, entirely in gdb's process space. */
extern struct value *
m3_build_gdb_proc_closure (
    struct type * proc_type,
    CORE_ADDR code_addr,
    CORE_ADDR env_ptr
  );

/* Is inf_addr the inferior address of a Modula-3 procedure closure? */
extern BOOL
m3_inf_address_is_proc_closure ( CORE_ADDR inf_addr );

/* Is closure_value a Modula-3 procedure closure value?  */
extern BOOL
m3_value_is_proc_closure ( struct value * closure_value );

/* valaddr/bitpos point to a gdb-space value of Modula-3 procedure type,
   which in turn could be a pointer to the procedures 's code or to a closure.
   Either way, return the code address. */
extern CORE_ADDR
m3_proc_code_addr ( const gdb_byte * valaddr, int bitpos );

/* valaddr/bitpos point to a gdb-space value of Modula-3 procedure type,
   which in turn could be a pointer to the procedures 's code or to a closure.
   Either way, return the enviroment pointer, which will, of course, be
   zero in the former case. */
extern CORE_ADDR
m3_proc_env_ptr ( const gdb_byte * valaddr, int bitpos );

/* Return the first superblock ancestor* of block that is a function block. */
extern struct block *
m3_block_proc_block ( struct block * blk );

extern BOOL
m3_address_lies_within_frame_locals (
    CORE_ADDR address,
    struct frame_info * frame
  );

/* Given a block for a procedure procblock, return the amount to add
   to the frame base address to get the place in the activation record
   where static links point. */
extern ULONGEST
m3_frame_base_to_sl_target_offset ( struct block * procblock );

/* If proc_block was discovered earlier to contain an artificial,
   compiler-generated block, return it, otherwise, identity. */
extern struct block *
m3_proc_body_block ( struct block * proc_block );

/* Return the symbol of procedure named 'name', that is nested inside
   the block parent_block, found in block_symtab.  Name is a "terminated
   string", see m3_term_strings_equal. NULL if anything goes wrong. */
extern struct symbol *
m3_lookup_nested_proc (
    struct block * parent_block,
    struct symtab * block_symtab,
    char * name,
    char * name_to
  );

/* Return the 'block_no'-th block, that is nested directly inside
   'parent_block', found in 'block_symtab'.  Here, blocks are numbered
   starting from one.  NULL if anything goes wrong. */
extern struct block *
m3_find_nested_block (
  struct block * parent_block, struct symtab * block_symtab, int block_no );

/* PRE: string,string_to are a terminated string that is all digits.
   Convert it to an integer.
*/
extern int
m3_int_value ( char * string, char * string_to );

/* Make a list of canonical linespecs for values.  This will always
   have one element, because there is no user-defined overloading in
   Modula-3, and however the procedure is identified, it will be unique.
*/
extern void
m3_make_canonical ( struct symtabs_and_lines * values, char * * * canonical );
/* Evaluate the string.  If any errors occur, ignore them and return NULL. */
extern struct value *
m3_evaluate_string ( char * string );

enum m3_target_typ
  { TARGET_UNKNOWN,
    TARGET_NT386, /* NT386, NT386GNU */
    TARGET_64,    /* Anything 64-bit. */
    TARGET_OTHER
  };

extern enum m3_target_typ m3_current_target;

extern int m3_target_integer_bit;
extern int m3_target_longint_bit;
extern int m3_widechar_bit; /* Depends on compiler version, not target. */
extern int m3_widechar_byte; 
extern LONGEST m3_widechar_LAST; 

extern void
m3_set_derived_target_info ( void );

extern enum m3_target_typ
m3_target_pure ( char * name );

/* End of file m3-util.h */
