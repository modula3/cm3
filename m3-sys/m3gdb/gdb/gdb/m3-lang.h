/* Modula-3 language support definitions for GDB, the GNU debugger.
   Copyright 1992, 2001, 2006 Free Software Foundation, Inc.

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

#include "m3-bool.h"

#include "defs.h"
#include "gdbtypes.h"
#include "symtab.h"
#include "value.h"

/* Values of type m3_result_code are stored in the struct type node of a
   procedure constant (which has TYPE_CODE_FUNC) to show whether it has
   a function result of a "large" type that is, at machine code level,
   passed as a reference parameter, and, if so, which end of the machine-
   level parameter list it is at.  The latter varies for different targets. */
enum m3_result_code
  { m3_res_none,
    m3_res_leftmost,
    m3_res_rightmost
  };

/* Nonzero if a Modula-3 compiler that does not use a gcc code generator.
   NOTE: Some Modula-3 compilers use a gcc-derived code generator.  They
   do not cause this to be TRUE, but they do cause
   processing_gcc_compilation to be nonzero. */
extern BOOL processing_pm3_compilation;

/* TRUE indicates that the debug info will show an extra block
   surrounding the non-prologue-non-epilogue part of every procedure.
   The outer block of the pair has the procedure symbol in block_function
   and contains the formals.  The inner has NULL block_function and
   contains the locals.  This happens when code was produced by a code
   generator derived from later gcc versions (3.4.5, for example).
*/
extern BOOL procedures_have_extra_block;

extern const char * m3_static_link_var_name;
extern const char * m3_nonlocal_var_rec_name;
extern const char * m3_static_link_copy_field_name;

/* Use the directory name and object file name (taken from stabs SO entries,)
   to maybe detect the Modula-3 target name. */
extern void
m3_check_target ( char * dir_name, char * file_name );

/* Called from dbxread.c when starting a new object file. */
void
m3_start_stabs ( char * filename );  

/* Called from dbxread.c when done reading a new object file. */
void
m3_end_stabs ( void ); 

/* Use the string from the N_OPT stabs entry to maybe set
   processing_pm3_compilation, procedures_have_extra_block, 
   and widechar_occupies_32_bits. */
extern void
m3_check_compiler ( char * name );

extern struct type *
m3_resolve_type (char * uid);

extern char *
m3_demangle (const char *mangled, int options /*UNUSED*/);

extern struct type *builtin_type_m3_address;
extern struct type *builtin_type_m3_boolean;
extern struct type *builtin_type_m3_cardinal;
extern struct type *builtin_type_m3_char;
extern struct type *builtin_type_m3_extended;
extern struct type *builtin_type_m3_integer;
extern struct type *builtin_type_m3_cardinal;
extern struct type *builtin_type_m3_longint;
extern struct type *builtin_type_m3_longcard;
extern struct type *builtin_type_m3_longreal;
extern struct type *builtin_type_m3_mutex;
extern struct type *builtin_type_m3_null;
extern struct type *builtin_type_m3_real;
extern struct type *builtin_type_m3_refany;
extern struct type *builtin_type_m3_transient_refany;
extern struct type *builtin_type_m3_root;
extern struct type *builtin_type_m3_transient_root;
extern struct type *builtin_type_m3_text;
extern struct type *builtin_type_m3_untraced_root;
extern struct type *builtin_type_m3_void;
extern struct type *builtin_type_m3_widechar;
extern struct type *builtin_type_m3_proc_closure;
extern struct type *builtin_type_m3_array_of_char;
extern struct type *builtin_type_m3_array_of_widechar;

extern void
m3_patch_nested_procs ( struct blockvector *bv );

extern int
is_m3_type ( struct type * m3_type );

extern int
m3_value_print (struct value *, struct ui_file *, int,
               enum val_prettyprint);

extern void
m3_fix_param (
    struct type * func_type, int fieldno, struct symbol * param_sym );

extern void
m3_decode_struct (struct type *);

extern void
m3_fix_symtab (struct symtab *st);

/* A NOTE on global variables.  The debug information produced by the
   Modula-3 compilers describes the set of local variables of an interface
   or a module only as a record, with a field for each global variable in the
   unit.  Unfortunately, this means there is no gdb symbol built for a
   global variable.  The lookup_symbol and la_lookup_symbol_nonlocal
   callback mechanism have their interfaces set up on the reasonable assumption
   that whatever a lookup finds, it is representable by a symbol.

   We kludge our way around this as follows:  When m3_lookup_symbol_nonlocal
   finds a global variable, it returns the symbol for the record of
   globals the variable was found in.  This will, in turn, also be returned
   by lookup_symbol.  A caller of either function must, after it returns
   sym, check for this case, by calling m3_is_globals_record_symbol ( sym ).
   It then has to handle it as it likes, using the globals record and the name
   that it originally passed, as a field name in this record. */

/* A Modula-3-specific callback. */
extern struct symbol *
m3_lookup_symbol_nonlocal (
    const char *name,
    const char *linkage_name,
    const struct block *block,
    const domain_enum domain,
    struct symtab * * symtab
  );

/* Return the result type of the procedure value proc_value. */
extern struct type *
m3_proc_value_result_type ( struct value * proc_value );

/* If closure is a Modula-3 procedure closure value, return its code address.
   Otherwise, return zero. */
extern CORE_ADDR
m3_proc_value_code_addr ( struct value * closure_value );

/* If closure is a Modula-3 procedure closure, return its environment pointer.
   Otherwise return zero. */
extern CORE_ADDR
m3_proc_value_env_ptr ( struct value * closure );

/* Take care of pushing any required auxiliary data on the stack, prior to
   pushing the real parameters.  This is data that was constructed and
   exists only in gdb process space.  It includes closures for procedure
   values and dope for open arrays. */
extern void
m3_push_aux_param_data ( int nargs, struct value **args, CORE_ADDR * sp );

extern void
m3_typedef_print (
    struct type *type,
    struct symbol *new,
    struct ui_file *stream
  );

extern void
m3_print_type (
     struct type *type,
     char *varstring,
     struct ui_file *stream,
     int show,
     int indent
   );

extern CORE_ADDR
m3_value_as_address (struct value *);

/* See whether the string name is a PM3-style compilation unit body procedure
   name.  These have the form "_INITI_<interfaceName> or "_INITM_<moduleName>"...
   If so, return the pointer to the beginning of <interfaceName> or
   <moduleName>.  Otherwise, return NULL. "*/
extern const char *
pm3_comp_unit_body_name_start ( const char * name );

/* See whether the string name is a CM3-style compilation unit body procedure
   name.  These have the form "<interfaceName>_I3..." or "<moduleName>_M3...".
   If so, return the length of <interfaceName> or <moduleName>.
   Otherwise, return 0.
*/
extern int
cm3_comp_unit_body_name_len ( const char * name );

/* If m3_decode_linespec returns with result.nelts == 0, nothing was found
   that has a Modula-3 interpretation, but other interpretations (including
   file:line, etc.) should be tried.  If argptr has a Modula-3 interpretation,
   but it is somehow invalid, this will produce an error and/or throw an
   exception.
 */
extern struct symtabs_and_lines
m3_decode_linespec (
    char ** argptr,
    int funfirstline,
    char * * * canonical,
    int *not_found_ptr
  );

extern char *
m3_main_name (void);

#endif /* !defined (M3_LANG_H) */

/* End of file m3-lang.h */
