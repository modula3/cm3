/* Modula-3 language support definitions for GDB, the GNU debugger.
   Copyright 1992, 2001 Free Software Foundation, Inc.

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

#if !defined (M3_LANG_H)
#define M3_LANG_H 1
 
#include "value.h"

#include <stdbool.h> 

/* FIXME: This is a mess.  Prototypes are here for functions that are
     defined all over the place. 
*/

/* We are allowing unsafe operations. */ 
extern bool is_unsafe ( void );  

extern int
m3_parse PARAMS ((void));	/* Defined in c-exp.y */

extern char * m3_demangle (const char *mangled, int options /*UNUSED*/);
  
/* Defined in c-typeprint.c */
extern void m3_print_type (struct type *, char *, struct ui_file *, int, int);

extern void m3_emit_char (int, struct ui_file *, int);
extern void m3_emit_widechar (int, struct ui_file *, int);

/* defined in m3-valprint.c: */ 

/* Fetch the inferior address of the zero-th element of the Modula-3 open array 
   whose dope is in gdb-space value array_val. */ 
CORE_ADDR 
m3_value_open_array_elems_addr ( struct value * array_value ); 

/* Store inferior elements address val into the Modula-3 open array 
   dope value located in gdb-space value array_val. */ 
void 
m3_set_value_open_array_elems_addr ( 
    struct value * array_value, 
    CORE_ADDR val 
  );  

/* Return the dimension-th shape component (i.e., the element count for the
   dimension-th dimension), of the Modula-3 open array whose dope is in 
   gdb-space value array_val. */ 
ULONGEST 
m3_value_open_array_shape_component ( 
  struct value * array_value, int dimension  ); 

/* Store shape component val into the dimension-th slot of the Modula-3 
   open array dope that is in gdb-space value array_val. */ 
void
m3_set_value_open_array_shape_component ( 
  struct value * array_value, int dimension, ULONGEST val ); 

/* See whether field field_name of object at inferior address ref, with inferior
   typecell address tc_addr is the "buf" field of an object of type 
   TextLiteral.T.  If so, return a trumped-up type that is right for printing
   the buf field, having computed its element count and element type from the
   object.  This type is good until the next call on this function.  If not,
   return zero.  
*/ 
extern bool /* Yes, it's that field of that type. */ 
m3_check_TextLiteral_buf 
  ( CORE_ADDR ref, 
    CORE_ADDR tc_addr, 
    char * field_name, 
    int * bitsize,
    int * bitpos, /* of the field, relative to ref */  
    struct type ** field_type 
  );  

extern int m3_val_print (struct type *, const gdb_byte *, int, CORE_ADDR,
			 struct ui_file *, int, int, int,
			 enum val_prettyprint);
extern int m3_val_print2 (struct type *, const gdb_byte *, int, int, 
                          struct ui_file *, int, int, int);

/* True if debugging CM3-compiled Modula-3 code. */ 
extern int m3_is_cm3 ( void ) ; 

extern struct type *m3_find_export_type (struct type *);

extern struct type *builtin_type_m3_address;
extern struct type *builtin_type_m3_boolean;
extern struct type *builtin_type_m3_cardinal;
extern struct type *builtin_type_m3_char;
extern struct type *builtin_type_m3_extended;
extern struct type *builtin_type_m3_integer;
extern struct type *builtin_type_m3_longreal;
extern struct type *builtin_type_m3_mutex;
extern struct type *builtin_type_m3_null;
extern struct type *builtin_type_m3_real;
extern struct type *builtin_type_m3_refany;
extern struct type *builtin_type_m3_root;
extern struct type *builtin_type_m3_text;
extern struct type *builtin_type_m3_untraced_root;
extern struct type *builtin_type_m3_void;
extern struct type *builtin_type_m3_widechar;
extern struct type *builtin_type_m3_proc_closure;

extern LONGEST m3_extract_ord (const gdb_byte *, int, int, int);

extern CORE_ADDR m3_extract_address (const gdb_byte *, int);

extern LONGEST m3_value_as_integer (struct value *);

extern double m3_value_as_float (struct value *);

extern CORE_ADDR m3_value_as_address (struct value *);

extern struct value * m3_ensure_value_is_unpacked ( struct value * packed_val ); 

extern struct type *find_m3_type_with_uid (int);

extern struct type *find_m3_type_named (char *, int);

extern struct type *find_m3_exported_interfaces (char *);

extern struct symbol *find_m3_ir (int, char *);

extern char *find_m3_type_name (struct type *);

/* Return the typecode of the object at inferior address addr. */ 
LONGEST m3_typecode ( CORE_ADDR addr ); 

/* Return the inferior address of the typecell for the dyanamic (allocated) type
   of the object at inferior address addr.  
*/
extern CORE_ADDR find_m3_heap_tc_addr (CORE_ADDR);

/* given the address of a typecell, find the M3 numeric uid for it. */
extern int find_m3_uid_from_tc (CORE_ADDR);

/* given the address of a typecell, find the gdb type for it */
extern struct type *find_m3_type_from_tc (CORE_ADDR);

/* Given a type from a Modula-3 program, return its numeric uid. */ 
extern int int_uid_from_m3_type ( struct type * t );  

/* given a gdb type, find the address of the corresponding typecell */
extern CORE_ADDR find_tc_from_m3_type (struct type *t);

/* given a heap reference, find it's actual type */
extern struct type *find_m3_heap_type (CORE_ADDR);

extern int m3_tc_address_to_dataOffset (CORE_ADDR);

extern int m3_tc_address_to_methodOffset (CORE_ADDR);

extern int m3_tc_address_to_dataSize (CORE_ADDR);

extern CORE_ADDR m3_tc_addr_to_super_tc_addr (CORE_ADDR);

extern CORE_ADDR m3_tc_address_to_defaultMethods (CORE_ADDR);

extern bool m3_types_equal ( struct type * left, struct type * right ); 

extern int is_m3_type ( struct type * m3_type );  

extern int is_m3_ordinal_type (struct type *);

extern bool m3_type_is_signed ( struct type *type );  

extern void m3_ordinal_bounds (struct type *, LONGEST *, LONGEST *);

extern void m3_printchar (int, struct ui_file *);

extern void m3_printwidechar (int, struct ui_file *);

extern int m3_value_print (struct value *, struct ui_file *, int,
			   enum val_prettyprint);

extern void  m3_fix_param 
  ( struct type * func_type, int fieldno, struct symbol * param_sym ); 

extern void m3_decode_struct (struct type *);

extern void m3_fix_symtab (struct symtab *st);

extern int find_m3_rec_field (struct type *, char *, int *, int *,
			      struct type **);

extern int find_m3_obj_field (struct type *, char *, int *, int *,
			      struct type **);

extern int find_m3_obj_method (struct type *obj_type,
                               char *name,
                               int *size, int *offset,
                               struct type **type);

extern gdb_byte *m3_read_object_fields_bits (CORE_ADDR);

extern void m3_patch_nested_procs  ( struct blockvector *bv );  

/* If closure is a Modula-3 procedure closure value, return its result type. 
   Otherwise, return NULL. */ 
struct type *  
m3_proc_closure_result_type ( struct value * closure_value );  

/* If closure is a Modula-3 procedure closure value, return its code address.  
   Otherwise, return zero. */ 
CORE_ADDR 
m3_proc_closure_code_addr ( struct value * closure_value ); 
 
/* If closure is a Modula-3 procedure closure, return its environment pointer. 
   Otherwise return zero. */ 
extern CORE_ADDR 
m3_proc_closure_env_ptr ( struct value * closure ); 

/* Take care of pushing any required auxiliary data on the stack, prior to
   pushing the real parameters.  This is data that was constructed and
   exists only in gdb process space.  It includes closures for procedure
   values and dope for open arrays. */ 
void 
m3_push_aux_param_data ( int nargs, struct value **args, CORE_ADDR * sp ); 

/* Special value attached to "type" values 
   FIXME: There has to be a cleaner way to do this. */
extern LONGEST m3_type_magic_value;

#endif /* !defined (M3_LANG_H) */
