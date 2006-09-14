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
 
#include <stdbool.h>

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

extern CORE_ADDR rt0u_types_value; /* SRC, PM3 and EZM3 only. */
extern CORE_ADDR rttype_types_addr; /* CM3 only. */

extern int rttype_info_def_size; 
extern int rttype_info_def_offset;
extern int rttype_infomap_map_size; 
extern int rttype_infomap_map_offset;
extern int rttype_infomap_cnt_size; 
extern int rttype_infomap_cnt_offset;

/* We are allowing unsafe operations. */ 
extern bool is_unsafe ( void );  

/* Special value attached to "type" values 
   FIXME: There has to be a cleaner way to do this. */
#define M3_TYPE_MAGIC 766579803L
extern const LONGEST m3_type_magic_value;

extern const LONGEST LONGEST_MAXxxx; 

extern void m3_emit_char (int, struct ui_file *, int);

extern void m3_emit_widechar (int, struct ui_file *, int);

extern void m3_print_char (int, struct ui_file *);

extern void m3_print_widechar (int, struct ui_file *);

extern void
m3_print_string (
    struct ui_file * stream,
    const gdb_byte * string,
    unsigned int length,
    int width,
    int force_ellipsis 
  );

/* Convert Modula-3 numbers into newly allocated values */
struct value * 
m3_value_from_longest ( struct type *type, LONGEST num);

/* Given a block, return the symtab it belongs to. */ 
extern struct symtab * 
m3_symtab_of_block ( const struct block * blk ); 

extern struct type * find_m3_type_named (char *, int);

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

/* Return the pseudo-record-type that has one field for each 
   globally-declared identifier in an interface or module.  
   For an interface, its demangled name is "I$<interfaceName>".
   For a module, its demangled name is "M$<moduleName>".
   kind should be either 'I' or 'M', for interface or module. 
   It could be in any static or global block.
*/ 
extern struct symbol *
m3_unit_name_globals_symbol ( 
  int kind, const char *unit_name, struct symtab * * symtab ); 

/* Is sym the symbol of a Modula-3 globals record for either an interface
   of a module? */ 
extern bool
m3_is_globals_record_symbol ( const struct symbol * sym ); 

extern bool 
m3_is_interface_global_record ( struct symbol * sym ); 

extern bool 
m3_is_module_global_record ( struct symbol * sym );  

/* Return the pseudo-record-type that has one field for each exported
   interface.  It's demangled name is "H$<moduleName>.  
   If the result is NIL, this means that the module exports exactly one
   interface, with the same name as the module. 
*/
extern struct type * 
find_m3_exported_interfaces ( const char * module_name );

/* Is sym the symbol of a Modula-3 type name? */
extern bool
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
   Return its symbol, if so, NULL if not.  EXCEPT: global variables have no
   symbol.  If it's a global variable, return the globals record for the
   interface.  Caller will have to detect this case and combine it with
   'ident' in its own way. */ 
extern struct symbol *
m3_lookup_interface_id ( 
    const char * interface_name, const char * ident, struct symtab * * symtab );

/* See if 'ident' is declared in an exported interface of module named 'module',
   which we assume we are currently executing in some block of. 
   An exported procedure whose body is actually provided in the module named 
   'module_name' should have been previously found in the static or 
   global block of the module itself.  */ 
extern struct symbol * 
m3_lookup_exported ( 
  const char *module_name, const char * ident, struct symtab * * symtab );

extern void note_is_pm3 ( void );

extern void note_is_cm3 ( void );

extern bool m3_is_cm3 ( void ); 

/* Return the typecode of the object at inferior address addr. */ 
LONGEST m3_typecode_from_inf_address ( CORE_ADDR addr ); 

/* Return the inferior address of the typecell for the dyanamic (allocated) type
   of the object at inferior address addr.  
*/
extern CORE_ADDR m3_tc_addr_from_object_addr (CORE_ADDR);

/* Given a type from a Modula-3 program, return its numeric uid. */ 
extern int int_uid_from_m3_type ( struct type * t );  

/* given a gdb type, find the address of the corresponding typecell */
extern CORE_ADDR m3_tc_addr_from_type (struct type *t);

/* given the address of a typecell, find the M3 numeric uid for it. */
extern int m3_int_uid_from_tc (CORE_ADDR);

/* given the address of a typecell, find the gdb type for it */
extern struct type *m3_type_from_tc (CORE_ADDR);

/* given a heap reference, find it's actual type */
extern struct type * m3_allocated_type_from_object_addr (CORE_ADDR);

extern int m3_dataOffset_from_tc_addr (CORE_ADDR);

extern int m3_methodOffset_from_tc_addr (CORE_ADDR);

extern int m3_dataSize_from_tc_addr (CORE_ADDR);

extern CORE_ADDR m3_super_tc_addr_from_tc_addr (CORE_ADDR);

extern CORE_ADDR m3_defaultMethods_from_tc_addr (CORE_ADDR);

extern int m3_find_rec_field (struct type *, const char *, int *, int *,
			      struct type **);

extern int m3_find_obj_field (struct type *, char *, int *, int *,
			      struct type **);

extern int m3_find_obj_method (struct type *obj_type,
                               char *name,
                               int *size, int *offset,
                               struct type **type);

extern bool m3_is_ordinal_type (struct type *);

extern bool m3_type_is_signed ( struct type *type );  

extern void m3_ordinal_bounds (struct type *, LONGEST *, LONGEST *);

extern gdb_byte *m3_read_object_fields_bits (CORE_ADDR);

extern LONGEST m3_extract_ord (const gdb_byte *, int, int, int);

extern CORE_ADDR m3_extract_address (const gdb_byte *, int);

extern LONGEST m3_value_as_integer (struct value *);

extern double m3_value_as_float (struct value *);

extern struct value * m3_ensure_value_is_unpacked ( struct value * packed_val ); 

extern CORE_ADDR m3_open_array_elems_addr ( const gdb_byte * addr ); 

extern void m3_set_open_array_elems_addr ( gdb_byte * addr, CORE_ADDR val );  

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

extern struct frame_info * 
m3_static_parent_frame ( struct frame_info *start_frame ); 

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
extern bool 
m3_inf_address_is_proc_closure ( CORE_ADDR inf_addr ); 

/* Is closure_value a Modula-3 procedure closure value?  */ 
extern bool 
m3_value_is_proc_closure ( struct value * closure_value ); 

/* valaddr points to a gdb-space value of Modula-3 procedure type, which
   in turn could be a pointer to the procedures 's code or to a closure.  
   Either way, return the code address. */ 
extern CORE_ADDR 
m3_proc_code_addr ( const gdb_byte * valaddr );  

/* valaddr points to a gdb-space value of Modula-3 procedure type, which
   in turn could be a pointer to the procedures 's code or to a closure.  
   Either way, return the enviroment pointer, which will, of course, be
   zero in the former case. */ 
extern CORE_ADDR 
m3_proc_env_ptr ( const gdb_byte * valaddr );  

/* Return the first superblock ancestor of block that is a function block. */
extern struct block * 
m3_proc_block ( struct block * blk );  

extern bool 
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

/* End of file m3-util.h */ 
