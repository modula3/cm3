/* Modula-3 language support definitions for GDB, the GNU debugger.
   Copyright 2006 Free Software Foundation, Inc.

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

  /* This file contains utility routines for Modula-3 debugging support. */ 

#include <stdbool.h>

#include "defs.h"
#include "gdb_assert.h"
#include "gdbcore.h"
#include "gdbtypes.h"
#include "gdb_string.h"
#include "expression.h"
#include "language.h" 
#include "symtab.h" 
#include "valprint.h"
#include "frame.h" 
#include "dictionary.h" 
#include "block.h" 
#include "parser-defs.h"
#include "objfiles.h"

#include "m3-util.h"
#include "m3-lang.h"
#include "m3-uid.h"

int rt0_tc_selfID_size = 0; 
int rt0_tc_selfID_offset = 0;
int rt0_tc_dataSize_size = 0; 
int rt0_tc_dataSize_offset = 0;
int rt0_tc_kind_size = 0; 
int rt0_tc_kind_offset = 0;

int rt0_dataOffset_size = 0; 
int rt0_dataOffset_offset = 0;
int rt0_methodOffset_size = 0; 
int rt0_methodOffset_offset = 0;
int rt0_parent_size = 0; 
int rt0_parent_offset = 0;
int rt0_defaultMethods_size = 0; 
int rt0_defaultMethods_offset = 0;

CORE_ADDR rt0u_types_value; /* SRC, PM3 and EZM3 only. */
CORE_ADDR rttype_types_addr; /* CM3 only. */

int rttype_info_def_size = 0; 
int rttype_info_def_offset = 0;
int rttype_infomap_map_size = 0; 
int rttype_infomap_map_offset = 0;
int rttype_infomap_cnt_size = 0; 
int rttype_infomap_cnt_offset = 0;

static bool constant_init_done = false;

void
init_m3_constants ( )
{ bool is_cm3; 

  if ( constant_init_done ) { return; }

  { struct type* rt0_tc;
    struct type* rt0_otc;
  
    rt0_tc = find_m3_type_named ("RT0.Typecell",0);
    if (!rt0_tc) 
      { 
        error ("Can't find RT0.Typecell. Maybe M3 libraries are compiled "
               "without debug info, or not stabs.\n"
              ); 
        return; /* without setting constant_init_done. */ 
      } 
    m3_find_rec_field (rt0_tc, "selfID",
		     &rt0_tc_selfID_size, &rt0_tc_selfID_offset, 0);
    m3_find_rec_field (rt0_tc, "dataSize",
		     &rt0_tc_dataSize_size, &rt0_tc_dataSize_offset, 0);

    rt0_otc = find_m3_type_named ("RT0.ObjectTypecell",0);
    is_cm3 = (rt0_otc != 0); 

    if (is_cm3)
      { struct type* t;
        struct symbol *rttype;
        int types_size, types_offset;

        note_is_cm3 ( ); 
        m3_find_rec_field 
          (rt0_tc, "kind", &rt0_tc_kind_size, &rt0_tc_kind_offset, 0);

        t = find_m3_type_named ("RTType.InfoMap",1);
        m3_find_rec_field (t, "map", 
          &rttype_infomap_map_size, &rttype_infomap_map_offset, 0);
        m3_find_rec_field (t, "cnt", 
          &rttype_infomap_cnt_size, &rttype_infomap_cnt_offset, 0);

        t = find_m3_type_named ("RTType.Info",1);

        m3_find_rec_field (t, "def", 
           &rttype_info_def_size, &rttype_info_def_offset, 0);

        rttype = m3_unit_name_globals_symbol ('M', "RTType", NULL );

        m3_find_rec_field (SYMBOL_TYPE (rttype), "types", 
                           &types_size, &types_offset, 0);

        rttype_types_addr = SYMBOL_VALUE_ADDRESS (rttype) + types_offset / 8;
      } 
    else /* SRC, PM3, EZM3. */ 
      { 
        struct symbol *rt0u;
        int rt0u_types_size, rt0u_types_offset;

        note_is_pm3 ( ); 
        rt0u = m3_unit_name_globals_symbol ('I', "RT0u", NULL );

        m3_find_rec_field (SYMBOL_TYPE (rt0u), "types", 
                           &rt0u_types_size, &rt0u_types_offset, 0);

        read_memory (SYMBOL_VALUE_ADDRESS (rt0u) 
                              + rt0u_types_offset / TARGET_CHAR_BIT
                           , (char *)&rt0u_types_value
                           , rt0u_types_size /TARGET_CHAR_BIT);
        rt0_otc = rt0_tc; 
      } 

    m3_find_rec_field (rt0_otc, "dataOffset", 
		     &rt0_dataOffset_size, &rt0_dataOffset_offset, 0);
    m3_find_rec_field (rt0_otc, "methodOffset", 
		     &rt0_methodOffset_size, &rt0_methodOffset_offset, 0);
    m3_find_rec_field (rt0_otc, "parent",
		     &rt0_parent_size, &rt0_parent_offset, 0);
    m3_find_rec_field (rt0_otc, "defaultMethods", 
		     &rt0_defaultMethods_size, 
		     &rt0_defaultMethods_offset, 0);
  }

  constant_init_done = true;
}

bool 
is_unsafe ( void ) 

  { /* FIXME: Implement this. */ 
    return true; 
  } /* is_unsafe */ 


const LONGEST m3_type_magic_value = ( LONGEST ) M3_TYPE_MAGIC;

const LONGEST LONGEST_MAXxx = ( ( ULONGEST ) ( 0 - 1 ) ) / 2;  

/* Print the character C on STREAM as part of the contents of a literal
   string whose delimiter is QUOTER.  Note that that format for printing
   characters and strings is language specific. */

void
m3_emit_char (c, stream, quoter)
     int c;
     struct ui_file *stream;
     int quoter;
{

  c &= 0xFF;			/* Avoid sign bit follies */

  if (PRINT_LITERAL_FORM (c))
    {
      if (c == '\\' || c == quoter)
	{
	  fputs_filtered ("\\", stream);
	}
      fprintf_filtered (stream, "%c", c);
    }
  else
    {
      switch (c)
	{
	case '\n':
	  fputs_filtered ("\\n", stream);
	  break;
	case '\b':
	  fputs_filtered ("\\b", stream);
	  break;
	case '\t':
	  fputs_filtered ("\\t", stream);
	  break;
	case '\f':
	  fputs_filtered ("\\f", stream);
	  break;
	case '\r':
	  fputs_filtered ("\\r", stream);
	  break;
	default:
	  fprintf_filtered (stream, "\\%.2X", (unsigned int) c);
	  break;
	}
    }
}

void
m3_emit_widechar (c, stream, quoter)
     int c;
     struct ui_file *stream;
     int quoter;
{

  c &= 0xFFFF;			/* Avoid sign bit follies */

  if ((c <= 0xFF) && PRINT_LITERAL_FORM (c))
    {
      if (c == '\\' || c == quoter)
	{
	  fputs_filtered ("\\", stream);
	}
      fprintf_filtered (stream, "%c", c);
    }
  else
    {
      switch (c)
	{
	case '\n':
	  fputs_filtered ("\\n", stream);
	  break;
	case '\b':
	  fputs_filtered ("\\b", stream);
	  break;
	case '\t':
	  fputs_filtered ("\\t", stream);
	  break;
	case '\f':
	  fputs_filtered ("\\f", stream);
	  break;
	case '\r':
	  fputs_filtered ("\\r", stream);
	  break;
	default:
	  fprintf_filtered (stream, "\\x%.4X", (unsigned int) c);
	  break;
	}
    }
}

void
m3_print_char (c, stream)
     int c;
     struct ui_file *stream;
{
  fputs_filtered ("'", stream);
  m3_emit_char (c, stream, '\'');
  fputs_filtered ("'", stream);
} /* m3_print_char */ 

void
m3_print_widechar (c, stream)
     int c;
     struct ui_file *stream;
{
  fputs_filtered ("W'", stream);
  m3_emit_widechar (c, stream, '\'');
  fputs_filtered ("'", stream);
} /* m3_print_widechar */ 

/* Print the character string STRING, printing at most LENGTH characters.
   LENGTH is -1 if the string is nul terminated.  Each character is WIDTH bytes
   long.  Printing stops early if the number hits print_max; repeat counts are
   printed as appropriate.  Print ellipsis at the end if we had to stop before
   printing LENGTH characters, or if FORCE_ELLIPSIS.  */
void
m3_print_string (
    struct ui_file * stream,
    const gdb_byte * string,
    unsigned int length,
    int width,
    int force_ellipsis
  )

{
  unsigned int i;
  unsigned int things_printed = 0;
  int in_quotes = 0;
  int need_comma = 0;
  extern int inspect_it;

  /* If the string was not truncated due to `set print elements', and
     the last byte of it is a null, we don't print that, in traditional C
     style.  */
  if ((!force_ellipsis) && length > 0 
      && extract_unsigned_integer (string + (length - 1) * width, width) == '\0'
     )
    length--;

  if (length == 0)
    {
      fputs_filtered ("\"\"", stream);
      return;
    }

  for (i = 0; i < length && things_printed < print_max; ++i)
    {
      /* Position of the character we are examining
	 to see whether it is repeated.  */
      unsigned int rep1;
      /* Number of repetitions we have detected so far.  */
      unsigned int reps;
      unsigned long current_char;

      QUIT;

      if (need_comma)
	{
	  fputs_filtered (", ", stream);
	  need_comma = 0;
	}

      current_char = extract_unsigned_integer (string + i * width, width);

      rep1 = i + 1;
      reps = 1;
      while (rep1 < length
	     && extract_unsigned_integer (string + rep1 * width, width)
	     == current_char)
	{
	  ++rep1;
	  ++reps;
	}

      if (reps > repeat_count_threshold)
	{
	  if (in_quotes)
	    {
	      if (inspect_it)
		fputs_filtered ("\\\", ", stream);
	      else
		fputs_filtered ("\", ", stream);
	      in_quotes = 0;
	    }
	  m3_print_char (string[i], stream);
	  fprintf_filtered (stream, " <repeats %u times>", reps);
	  i = rep1 - 1;
	  things_printed += repeat_count_threshold;
	  need_comma = 1;
	}
      else
	{
	  if (!in_quotes)
	    {
	      if (inspect_it)
		fputs_filtered ("\\\"", stream);
	      else
		fputs_filtered ("\"", stream);
	      in_quotes = 1;
	    }
	  m3_emit_char (string[i], stream, '"');
	  ++things_printed;
	}
    }

  /* Terminate the quotes if necessary.  */
  if (in_quotes)
    {
      if (inspect_it)
	fputs_filtered ("\\\"", stream);
      else
	fputs_filtered ("\"", stream);
    }

  if (force_ellipsis || i < length)
    fputs_filtered ("...", stream);
} /* m3_print_string */ 

/* Convert Modula-3 numbers into newly allocated values */
struct value * 
m3_value_from_longest (struct type *type, LONGEST num)

  { struct value * val = allocate_value ( type );
    enum type_code code = TYPE_CODE ( type );
    int len = TYPE_LENGTH ( type );

    switch ( code )
      { case TYPE_CODE_M3_INTEGER:
        case TYPE_CODE_M3_CARDINAL:
        case TYPE_CODE_M3_CHAR:
        case TYPE_CODE_M3_WIDECHAR:
        case TYPE_CODE_M3_ENUM:
        case TYPE_CODE_M3_SUBRANGE:
        case TYPE_CODE_M3_BOOLEAN:
        case TYPE_CODE_M3_PACKED:
          store_signed_integer ( value_contents_raw ( val ), len, num );
          break;

        case TYPE_CODE_M3_REFANY:
        case TYPE_CODE_M3_TRANSIENT_REFANY:
        case TYPE_CODE_M3_POINTER:
        case TYPE_CODE_M3_ADDRESS:
        case TYPE_CODE_M3_ROOT:
        case TYPE_CODE_M3_TRANSIENT_ROOT:
        case TYPE_CODE_M3_UN_ROOT:
        case TYPE_CODE_M3_NULL:
          ADDRESS_TO_POINTER 
            ( type, value_contents_raw ( val ), (CORE_ADDR) num );
          break;

        default:
          error ( "Unexpected type in m3_value_from_longest." );
      }
    return val;
  } /* m3_value_from_longest */ 

/* Given a block, return the symtab it belongs to.  This result is probably 
   never used, but we need it to satisfy the interface of the language-specific
   callback la_lookup_nonlocal. */ 
struct symtab * 
m3_symtab_of_block ( const struct block * blk ) 

  { struct objfile *objfile = NULL;
    struct blockvector *bv;
    struct block *b;
    struct symtab *s = NULL;

    /* Search the list of symtabs for one which contains the
       address of the start of this block.  */
    ALL_SYMTABS ( objfile, s )
      { bv = BLOCKVECTOR ( s );
        b = BLOCKVECTOR_BLOCK ( bv, GLOBAL_BLOCK );
        if ( BLOCK_START ( b ) <= BLOCK_START ( blk )
             && BLOCK_END ( b ) > BLOCK_START ( blk )
           )
          { goto found; }
      }
    found:
    return s; 

  } /* m3_symtab_of_block */ 

struct type *
find_m3_type_named (name, must_find)
     char *name;
     int must_find; /* Emit an error message, if can't find it. */ 
{
  char struct_name [ M3_MAX_SYMBOLLEN ]; 
  struct symbol *s;

  /* FIXME: Rework this.  Use a less drastic way than lookup_symbol, 
     take apart the qualified name and lookup the interface and type
     id separately, and change the demangler to unqualify the name
     of a type found in an Mn_zzzzzz_Interface.Type entry.  Also, 
     get rid of the B$ in the prefix, which will allow ordinary lookup
     to not need special cases for types. */ 
  snprintf (struct_name, M3_MAX_SYMBOLLEN, "B$%s", name);
  s = lookup_symbol (struct_name, 0, STRUCT_DOMAIN, 0, 0);
  if (s == NULL) {
    if (must_find) error ("unable to find type named \"%s\"\n", name);
    return NULL;
  };
  return TYPE_M3_TYPE_TYPE (SYMBOL_TYPE (s));
}

/* Lookup a symbol in the one global block associated with block.  */
struct symbol *
m3_lookup_symbol_one_global (
    const char *name,
    const char *linkage_name,
    const struct block *block,
    const domain_enum domain,
    struct symtab * * symtab
  )

  { const struct block *global_block = block_global_block ( block );
    struct symbol * sym; 

    if ( symtab != NULL ) { * symtab = NULL; } /* In case we find nothing. */ 
    sym = NULL; 
    if ( global_block != NULL )
      { sym = lookup_symbol_aux_block 
                ( name, linkage_name, global_block, domain, symtab );
      } 
    return sym;  
  } /* m3_lookup_symbol_one_global */ 

/* Lookup a symbol in the all static and global blocks.  */
struct symbol *
m3_lookup_symbol_all_static_and_global (
    const char *name,
    const char *linkage_name,
    const domain_enum domain,
    struct symtab * * symtab
  )

  { struct symbol * sym;

    if ( symtab != NULL ) { * symtab = NULL; } /* In case we find nothing. */ 
    sym = lookup_symbol_aux_symtabs 
            ( STATIC_BLOCK, name, linkage_name, domain, symtab );
    if ( sym != NULL ) { return sym; } 
    sym = lookup_symbol_aux_symtabs 
            ( GLOBAL_BLOCK, name, linkage_name, domain, symtab );
    if ( sym != NULL ) { return sym; } 
    sym = lookup_symbol_aux_psymtabs 
            ( STATIC_BLOCK, name, linkage_name, domain, symtab );
    if ( sym != NULL ) { return sym; } 
    sym = lookup_symbol_aux_psymtabs 
            ( GLOBAL_BLOCK, name, linkage_name, domain, symtab );
    return sym;  
  } /* m3_lookup_symbol_all_static_and_global */ 

/* If blk contains a symbol with the right name spelling to be an inteface 
   or module global record, return it.  kind == 'M' to find a module global
   symbol, or kind == 'I' for an interface. Set unit_name to the declared name
   of the interface or module. */ 
struct symbol * 
m3_block_globals_symbol ( 
    const struct block * blk, const char kind, char * * unit_name ) 

  { struct symbol * sym; 
    struct dict_iterator iter; 
    char * linkage_name; 
    size_t len; 

    sym = dict_iterator_first ( blk -> dict, & iter ); 
    while ( sym != NULL ) 
      { linkage_name = SYMBOL_LINKAGE_NAME ( sym );
        len = strlen ( linkage_name ); 
        if ( len > 3 
             && linkage_name [ 0 ] == 'M' 
             && linkage_name [ 1 ] == kind   
             && linkage_name [ 2 ] == '_' 
           ) 
          { /* Still need to avoid PM3 symbol of form MM_<module>_CRASH */ 
            if ( len > 9 
                 && linkage_name [ len - 6 ] == '_'
                 && linkage_name [ len - 5 ] == 'C'
                 && linkage_name [ len - 4 ] == 'R'
                 && linkage_name [ len - 3 ] == 'A'
                 && linkage_name [ len - 2 ] == 'S'
                 && linkage_name [ len - 1 ] == 'H'
               ) { /* Skip this one and loop. */ } 
            else /* This is it. */  
              { if ( unit_name != NULL ) { * unit_name = linkage_name + 3; } 
                return sym; 
              } 
          } 
        sym = dict_iterator_next ( & iter ); 
      } 
    if ( unit_name != NULL ) { * unit_name = NULL; } 
    return NULL; 
  } /* m3_block_globals_symbol */ 

/* Return the interface or module global variable record symbol for
   interface or module named 'unit_name'.  kind == 'M' to find a 
   module global symbol, or kind == 'I' for an interface. 
   For an interface, its demangled name is "I$<interfaceName>".
   For a module, its demangled name is "M$<moduleName>".
   kind should be either 'I' or 'M', for interface or module. 
   It could be in any static or global block.
*/ 
struct symbol *
m3_unit_name_globals_symbol ( 
    int kind, const char *unit_name, struct symtab * * symtab ) 

  { struct symbol * sym; 
    char struct_name [ M3_MAX_SYMBOLLEN ]; 

    snprintf (struct_name, M3_MAX_SYMBOLLEN, "%c$%s", kind, unit_name);
    sym = m3_lookup_symbol_all_static_and_global 
            ( struct_name, NULL, VAR_DOMAIN, symtab );
   return sym; 
  } /* m3_unit_name_globals_symbol */ 

/* Is sym the symbol of a Modula-3 globals record for either an interface
   of a module? */ 
bool
m3_is_globals_record_symbol ( const struct symbol * sym ) 

{ char * name; 
  size_t len; 

  name = SYMBOL_LINKAGE_NAME ( sym );
  if ( name == NULL ) { return false; }  
  len = strlen ( name ); 
  if ( len < 4 ) { return false; } 
  if ( name [ 0 ] == 'M' 
       && ( name [ 1 ] == 'I' || name [ 1 ] == 'M' ) 
       && name [ 2 ] == '_' ) 
    { return true; } 
  else { return false; } 
} /* m3_is_globals_record_symbol */ 

bool 
m3_is_interface_global_record ( struct symbol * sym ) 

{ char * name; 
  size_t len; 

  name = SYMBOL_LINKAGE_NAME ( sym );
  if ( name == NULL ) { return false; }  
  len = strlen ( name ); 
  if ( len < 4 ) { return false; } 
  if ( name [ 0 ] == 'M' && name [ 1 ] == 'I' && name [ 2 ] == '_' ) 
    { return true; } 
  else { return false; } 
} /* m3_is_interface_global_record */ 

bool 
m3_is_module_global_record ( struct symbol * sym ) 

{ char * name; 
  size_t len; 

  name = SYMBOL_LINKAGE_NAME ( sym );
  if ( name == NULL ) { return false; }  
  len = strlen ( name ); 
  if ( len < 4 ) { return false; } 
  if ( name [ 0 ] == 'M' && name [ 1 ] == 'M' && name [ 2 ] == '_' ) 
    { return true; } 
  else { return false; } 
} /* m3_is_module_global_record */ 

/* Return the pseudo-record-type that has one field for each exported
   interface.  It's demangled name is "H$<moduleName>.  
   If the result is NIL, this means that the module exports exactly one
   interface, with the same name as the module. 
*/
struct type *
find_m3_exported_interfaces ( const char * module_name )

  { char struct_name [ M3_MAX_SYMBOLLEN ]; 
    const struct block * static_block
      = block_static_block ( expression_context_block ); 
    struct symbol *sym;

    snprintf ( struct_name, M3_MAX_SYMBOLLEN, "H$%s", module_name );
    sym = lookup_symbol_static 
            ( struct_name, NULL, static_block, STRUCT_DOMAIN, NULL );
    if ( sym != NULL ) 
      { return SYMBOL_TYPE ( sym ); } 
    else { return NULL; }
  } /* find_m3_exported_interfaces */ 

char *
find_m3_type_name (t)
     struct type *t;
{
  char *uid = TYPE_TAG_NAME (t);
  char struct_name [ M3_MAX_SYMBOLLEN ]; 
  struct symbol *sym;

  if (TYPE_NAME (t) == 0) {
    if (uid == NULL) return NULL;
    snprintf (struct_name, M3_MAX_SYMBOLLEN, "G$%s", uid);
    if ((sym = lookup_symbol (struct_name, 0, STRUCT_DOMAIN, 0, 0))) {
      TYPE_NAME (t) = TYPE_FIELD_NAME (SYMBOL_TYPE (sym), 0);
    } else {
      char *n;
      if (uid == NULL) {
	  n = malloc (strlen("<typeid=(null)>")+1);
	  strcpy(n, "<typeid=(null)>");
      } else {
	  n = malloc (strlen(uid)+strlen("<typeid=>")+1);
	  snprintf (n, M3_MAX_SYMBOLLEN, "<typeid=%s>", uid);
      }
      TYPE_NAME (t) = n;
    }
  }
  return TYPE_NAME (t);
}

/* Is sym the symbol of a Modula-3 type name? */
bool
m3_is_type_name_symbol ( const struct symbol * sym ) 

{ char * name; 
  size_t len; 

  name = SYMBOL_SEARCH_NAME ( sym );
  if ( name == NULL ) { return false; }  
  len = strlen ( name ); 
  if ( len < 3 ) { return false; } 
  if ( name [ 0 ] == 'B' 
       && name [ 1 ] == '$' 
     ) 
    { return true; } 
  else { return false; } 
} /* m3_is_type_name_symbol */ 

/* Look in 'block' for a declaration of a type. */ 
struct symbol *
m3_lookup_type (
    const char * unit_name,
    const char * name,
    const struct block * blk,
    struct symtab * * symtab
  ) 

{ char type_name [ M3_MAX_SYMBOLLEN ];
  struct symbol * sym; 

  /* Just in case we ever demangle these into unqualified names. */ 
  snprintf ( type_name, M3_MAX_SYMBOLLEN, "B$%s", name );
  sym = lookup_symbol_static ( type_name, NULL, blk, STRUCT_DOMAIN, symtab );
  if ( sym != NULL ) { return sym; } 
  if ( unit_name != NULL ) 
    { snprintf ( type_name, M3_MAX_SYMBOLLEN, "B$%s.%s", unit_name, name );
      sym 
        = lookup_symbol_static ( type_name, NULL, blk, STRUCT_DOMAIN, symtab );
    } 
  return sym; 
} /* m3_lookup_type */ 

/* See if identifier 'ident' is declared in interface named 'interface_name'. 
   Return its symbol, if so, NULL if not.  EXCEPT: global variables have no
   symbol.  If it's a global variable, return the globals record for the
   interface.  Caller will have to detect this case and combine it with
   'ident' in its own way. */ 
struct symbol *
m3_lookup_interface_id ( 
  const char * interface_name, const char * ident, struct symtab * * symtab ) 

  { struct symbol * interface_rec_sym;
    struct symbol * sym;
    struct block * interface_static_block; 
    int found; 
    struct type * global_type; 
    char linkage_name [ M3_MAX_SYMBOLLEN ];
    struct blockvector *bv;
    struct symtab * l_symtab;
    
    /* Try looking in the interface global record for a variable. */
    interface_rec_sym 
      = m3_unit_name_globals_symbol ( 'I', interface_name, & l_symtab ); 
    if ( interface_rec_sym != NULL ) 
      { if ( symtab != NULL) { * symtab = l_symtab; } 
        /* Look in the interface global record of an exported interface, 
           where we will find a procedure or variable that was declared 
           in the interface. */ 
        found 
          = m3_find_rec_field 
              ( SYMBOL_TYPE ( interface_rec_sym ), 
                ident, 0, 0, & global_type 
              );
        if ( found 
             /* SRC, PM3, and EZM3 put a procedure declared in an
                interface into the interface global record.  We don't
                want to use it from there, because that gives no symbol 
                for it.  Ignore a procedure here, and find it differently, 
                below. CM3 does not put it in the interface record in this 
                case.  */ 
             && ( global_type == NULL 
                  || TYPE_CODE ( global_type ) != TYPE_CODE_FUNC 
                ) 
           ) 
          { return interface_rec_sym; }

        /* Look in the static block of the interface, where we will find 
           a type declared in the interface, with transformed name, the 
           same for all compilers. */ 
        if ( l_symtab != NULL ) 
          { bv = BLOCKVECTOR ( l_symtab );
            interface_static_block = BLOCKVECTOR_BLOCK ( bv, STATIC_BLOCK );
            sym = m3_lookup_type 
                    ( interface_name, ident, interface_static_block, symtab ); 
            if ( sym != NULL ) { return sym; } 
          }

      }

    /* The consistent way to find a procedure declared in an interface
       and get its symbol is to construct the linkage name
       "interfaceName__ProcName" and look in all symtabs for it. */
    strncpy ( linkage_name, interface_name, M3_MAX_SYMBOLLEN );
    strncat ( linkage_name, "__", M3_MAX_SYMBOLLEN );
    strncat ( linkage_name, ident, M3_MAX_SYMBOLLEN );
    sym = m3_lookup_symbol_all_static_and_global 
            ( ident, linkage_name, VAR_DOMAIN, symtab ); 
    if ( sym != NULL 
         && sym->aclass != LOC_STATIC /* Is this still necessary? */ 
       ) 
      { return sym; }
    return NULL; 
  } /* m3_lookup_interface_id */ 

/* See if 'ident' is declared in an exported interface of module named 'module',
   which we assume we are currently executing in some block of. 
   An exported procedure whose body is actually provided in the module named 
   'module_name' should have been previously found in the static or 
   global block of the module itself.  */ 
struct symbol * 
m3_lookup_exported ( 
  const char *module_name, const char * ident, struct symtab * * symtab )

  { struct type *exports;
    int i;
    struct symbol * sym; 
    char * interface_name; 

    /* Try all the exported interfaces. */ 
    exports = find_m3_exported_interfaces ( module_name ); 
    if ( exports == NULL ) 
      { /* Treat this as a module that exports exactly one interface, with the
           same name as itself. */ 
        sym = m3_lookup_interface_id ( module_name, ident, symtab );
        if ( sym != NULL ) { return sym; } 
      } 
    else 
      { for ( i = 0; i < TYPE_NFIELDS ( exports ); i++ ) 
          { interface_name = TYPE_FIELD_NAME ( exports, i ); 
            sym = m3_lookup_interface_id 
                    ( interface_name, ident, symtab ); 
            if ( sym != NULL ) { return sym; } 
	  }
      }

    /* Not found. */ 
    if ( symtab != NULL ) { * symtab = NULL; } 
    return NULL;
  } /* m3_lookup_exported */ 

static enum compiler_kind_typ { ck_unknown, ck_pm3, ck_cm3 } compiler_kind 
  = ck_unknown; 

void 
note_is_cm3 ( void )

  { gdb_assert ( compiler_kind == ck_unknown ) ; 
    compiler_kind = ck_cm3; 
  } /* note_is_cm3 */ 

void 
note_is_pm3 ( void )

  { gdb_assert ( compiler_kind == ck_unknown ) ; 
    compiler_kind = ck_pm3; 
  } /* note_is_pm3 */ 

bool 
m3_is_cm3 ( void ) 
  { init_m3_constants ( ); 
    if ( compiler_kind == ck_unknown ) 
      { warning ( "Can't tell what Modula-3 compiler was used, assuming CM3." ); 
        return true; 
      } 
    return compiler_kind == ck_cm3; 
  } 

/* Return the typecode of the object at inferior address addr. */ 
LONGEST 
m3_typecode_from_inf_address ( CORE_ADDR addr ) 

{
  LONGEST typecodeword, typecode;
  if (!addr) { return 0; }

  read_memory (addr - (TARGET_PTR_BIT / TARGET_CHAR_BIT), 
		      (char *)&typecodeword, 
		      TARGET_PTR_BIT / TARGET_CHAR_BIT);

  /* the typecode is in Modula-3 bits 1..21 */
  typecode = m3_extract_ord((char *)&typecodeword, 1, 20, 0);
  return typecode; 

} /* m3_typecode_from_inf_address */ 

/* Return the inferior address of the typecell for the dyanamic (allocated) type
   of the object at inferior address addr.  
*/
CORE_ADDR 
m3_tc_addr_from_object_addr (CORE_ADDR addr)

{
  LONGEST typecode, n_types;
  CORE_ADDR result, map_ptr;

  if (!addr) { return 0; }

  typecode = m3_typecode_from_inf_address ( addr ) ; 

  init_m3_constants ();

  if ( m3_is_cm3 ( ) ) 
    { 
      n_types = 0;
      read_memory (rttype_types_addr + rttype_infomap_cnt_offset / 8,
                          (char*)&n_types, rttype_infomap_cnt_size / 8);

      if (typecode >= n_types) {
        warning ("encountered out-of-range typecode: %d (ref: 16_%lx)"
                 "\n   good typecode values are: [0..%d]",
                 (int)typecode, addr, (int)(n_types-1)  );
        return 0;
      }

      map_ptr = 0;
      read_memory (rttype_types_addr + rttype_infomap_map_offset / 8,
                          (char*)&map_ptr, rttype_infomap_map_size / 8);
      if (map_ptr == 0) {
        warning ("no allocated typecell map (typecode: %d, ref: 16_%lx)",
                 (int)typecode, addr);
        return 0;
      }

      read_memory (map_ptr
                          + typecode * TARGET_PTR_BIT / TARGET_CHAR_BIT,
                          (char *)&result, TARGET_PTR_BIT / TARGET_CHAR_BIT);
      if (result == 0) {
        warning ("typecode %d (ref: 16_%lx) has NIL RTType.InfoPtr value",
                 (int)typecode, addr);
        return 0;
      }

      read_memory (result + rttype_info_def_offset / 8,
                          (char*)&result, TARGET_PTR_BIT / TARGET_CHAR_BIT);
      if (result == 0) {
        warning ("typecode %d (ref: 16_%lx) has no associated typecell",
                 (int)typecode, addr);
        return 0;
      }

      return result;
    } 
  else /* pm3, etc. */
    {
      read_memory (rt0u_types_value 
	  	          + typecode * TARGET_PTR_BIT / TARGET_CHAR_BIT,
		          (char *)&result, TARGET_PTR_BIT / TARGET_CHAR_BIT);
      return result; 

    } 
} /* m3_tc_addr_from_object_addr */

/* Given a type from a Modula-3 program, return its numeric uid. */ 
int /* Numeric uid. */ 
int_uid_from_m3_type ( struct type * t ) 

{ int uid; 
   if ( m3uid_to_int ( TYPE_TAG_NAME ( t ), &uid) ) { return uid; } 
   return 0;
} /* int_uid_from_m3_type */ 

/*
 *  Return the address of the runtime typecell that corresponds to type "t".
 */
CORE_ADDR
m3_tc_addr_from_type (t)
     struct type *t;
{
  LONGEST typecode, n_types;
  CORE_ADDR map_ptr, info_ptr, tc_addr;
  int selfID, uid;

  init_m3_constants ( );

  if (! m3_is_cm3 ( ) ) return 0; 
  /* The following is only used for cm3's Text* modules. */

  if ((TYPE_CODE(t) != TYPE_CODE_M3_OBJECT)
      && (TYPE_CODE(t) != TYPE_CODE_M3_POINTER)) {
    return 0;  /* not an OBJECT or REF type */
  }

  if (!m3uid_to_int (TYPE_TAG_NAME (t), &uid)) {
    return 0; /* no name or bad format */
  }

  n_types = 0;
  read_memory (rttype_types_addr + rttype_infomap_cnt_offset / 8,
		      (char*)&n_types, rttype_infomap_cnt_size / 8);

  map_ptr = 0;
  read_memory (rttype_types_addr + rttype_infomap_map_offset / 8,
		      (char*)&map_ptr, rttype_infomap_map_size / 8);
  if (map_ptr == 0) {
    return 0;  /* no mapped typecells yet */
  }

  for (typecode = 0;  typecode < n_types;  typecode++) {
    /* get the InfoPtr */
    read_memory (map_ptr
		      + typecode * TARGET_PTR_BIT / TARGET_CHAR_BIT,
		      (char*)&info_ptr, TARGET_PTR_BIT / TARGET_CHAR_BIT);
    if (!info_ptr) { continue; }

    /* get the typecell pointer */
    read_memory (info_ptr + rttype_info_def_offset / 8,
		      (char*)&tc_addr, TARGET_PTR_BIT / TARGET_CHAR_BIT);
    if (!tc_addr) { continue; }

    /* get the type's UID */
    read_memory (tc_addr + rt0_tc_selfID_offset / TARGET_CHAR_BIT,
		      (char*)&selfID, rt0_tc_selfID_size / HOST_CHAR_BIT);
    if (selfID == uid) { return tc_addr; }
  }

  return 0;
} /* m3_tc_addr_from_type */ 

int 
m3_int_uid_from_tc (tc_addr)
     CORE_ADDR tc_addr;
{
  int selfID;

  init_m3_constants ();

  read_memory (tc_addr + rt0_tc_selfID_offset / TARGET_CHAR_BIT,
		      (char *)&selfID, rt0_tc_selfID_size / HOST_CHAR_BIT);

  return selfID;
} /* m3_int_uid_from_tc */ 

struct type *
m3_type_from_tc (tc_addr)
     CORE_ADDR tc_addr;
{
  int selfID;

  init_m3_constants ();

  read_memory (tc_addr + rt0_tc_selfID_offset / TARGET_CHAR_BIT,
		      (char *)&selfID, rt0_tc_selfID_size / HOST_CHAR_BIT);

  return (m3_resolve_type (m3uid_from_int (selfID)));
} /* m3_type_from_tc */ 

struct type *
m3_allocated_type_from_object_addr (addr)
     CORE_ADDR addr;
{
  return m3_type_from_tc (m3_tc_addr_from_object_addr (addr));
} /* m3_allocated_type_from_object_addr */ 


/* return LOOPHOLE (tc_addr, RT0.ObjectTypeDefn).dataOffset */
int 
m3_dataOffset_from_tc_addr (tc_addr)
     CORE_ADDR tc_addr;
{
  int result;
  char kind;

  init_m3_constants ();

  if ( m3_is_cm3 ( ) )
    { 
      kind = 0;
      read_memory (tc_addr + rt0_tc_kind_offset / TARGET_CHAR_BIT,
                          (char *)&kind, sizeof(kind));

      if (kind != 2/*RT0.TypeKind.Obj*/) { return 0; }
    } 

  read_memory (tc_addr + rt0_dataOffset_offset / TARGET_CHAR_BIT,
		      (char *)&result, rt0_dataOffset_size / TARGET_CHAR_BIT);
  return result;
} /* m3_dataOffset_from_tc_addr */ 

int 
m3_methodOffset_from_tc_addr (tc_addr)
     CORE_ADDR tc_addr;
{
  int result;
  char kind;

  init_m3_constants ();

  if ( m3_is_cm3 ( ) )
    { 
      kind = 0;
      read_memory (tc_addr + rt0_tc_kind_offset / TARGET_CHAR_BIT,
                          (char *)&kind, sizeof(kind));

      if (kind != 2/*RT0.TypeKind.Obj*/) { return 0; }
    } 

  read_memory (tc_addr + rt0_methodOffset_offset / TARGET_CHAR_BIT,
		      (char *)&result, rt0_methodOffset_size / TARGET_CHAR_BIT);
  return result;
} /* m3_methodOffset_from_tc_addr */ 
		      
int 
m3_dataSize_from_tc_addr (tc_addr)
     CORE_ADDR tc_addr;
{
  int result;
  init_m3_constants ();
  read_memory (tc_addr + rt0_tc_dataSize_offset / TARGET_CHAR_BIT,
		      (char *)&result, rt0_tc_dataSize_size / TARGET_CHAR_BIT);
  return result;
} /* m3_dataSize_from_tc_addr */ 
		      
CORE_ADDR  
m3_super_tc_addr_from_tc_addr (tc_addr)
     CORE_ADDR tc_addr;
{
  char kind;
  CORE_ADDR  result;

  init_m3_constants ();

  if ( m3_is_cm3 ( ) )
    { 
      kind = 0;
      read_memory (tc_addr + rt0_tc_kind_offset / TARGET_CHAR_BIT,
                          (char *)&kind, sizeof(kind));

      if (kind != 2/*RT0.TypeKind.Obj*/) { return 0; }
    } 

  read_memory (tc_addr + rt0_parent_offset / TARGET_CHAR_BIT,
		      (char *)&result, rt0_parent_size / TARGET_CHAR_BIT);
  return result;
} /* m3_super_tc_addr_from_tc_addr */ 
		      
CORE_ADDR 
m3_defaultMethods_from_tc_addr ( CORE_ADDR tc_addr ) 
{
  char kind;
  CORE_ADDR result;

  init_m3_constants ();
  if ( m3_is_cm3 ( ) )
    { 
      kind = 0;
      read_memory (tc_addr + rt0_tc_kind_offset / TARGET_CHAR_BIT,
                          (char *)&kind, sizeof(kind));

      if (kind != 2/*RT0.TypeKind.Obj*/) { return 0; }
    } 

  read_memory ( tc_addr + rt0_defaultMethods_offset / TARGET_CHAR_BIT,
	        (char *)&result, rt0_defaultMethods_size / TARGET_CHAR_BIT);
  return result;
} /* m3_defaultMethods_from_tc_addr */ 

/*
 * m3_find_rec_field
 * takes: REC_TYPE - a m3 Record type
 *        NAME - the name of a field of such a record as a character pointer
 *        SIZE - a reference to an integer, which this routine will
 *               fill the size of that field in.
 *        OFFSET - a reference to an integer, which this routine will
 *                 fill the offset of that field in.
 *        TYPE - a reference to a struct type, which this routine will
 *                 fill the type of that field in.
 *
 * If SIZE, OFFSET or TYPE are NULL they won't be set.
 *
 * RETURNs 0 if NAME wasn't found in REC_TYPE
 *         1 otherwise.
 */
int 
m3_find_rec_field (
    struct type * rec_type,
    const char * name,
    int * bitsize, 
    int * bitpos,
    struct type * * type
  ) 

  {
    int i;
    if (rec_type == NULL) return 0;
    for (i = 0; i < TYPE_M3_REC_NFIELDS (rec_type); i++) {
      if (strcmp (TYPE_M3_REC_FIELD_NAME (rec_type, i), name) == 0) {
        if ( bitsize != NULL ) 
          { * bitsize = TYPE_M3_REC_FIELD_BITSIZE ( rec_type, i ); }
        if ( bitpos != NULL ) 
          { * bitpos = TYPE_M3_REC_FIELD_BITPOS ( rec_type, i ); }
        if ( type != NULL ) { * type = TYPE_M3_REC_FIELD_TYPE ( rec_type, i ); }
        return 1;
      }
    }
    if ( bitsize != NULL ) { * bitsize = 0; }
    if ( bitpos != NULL ) { * bitpos = 0; } 
    return 0; 
  } /* m3_find_rec_field */ 
		      
int
m3_find_obj_field (
    struct type *obj_type,
    char *name,
    int *bitsize, 
    int *bitpos,
    struct type **type
  ) 

  {
    int i;
    if (obj_type == NULL) return 0;
    for (i = 0; i < TYPE_M3_OBJ_NFIELDS (obj_type); i++) {
      if (strcmp (TYPE_M3_OBJ_FIELD_NAME (obj_type, i), name) == 0) {
        if ( bitsize != NULL )   
          { * bitsize = TYPE_M3_OBJ_FIELD_BITSIZE ( obj_type, i ); }
        if ( bitpos != NULL ) 
          { * bitpos = TYPE_M3_OBJ_FIELD_BITPOS ( obj_type, i ); }
        if ( type != NULL )   
          { * type = TYPE_M3_OBJ_FIELD_TYPE ( obj_type, i ); }
        return 1;
      }
    }
    return 0; 
  } /* m3_find_obj_field */ 

int
m3_find_obj_method (
    struct type *obj_type,
    char *name,
    int *bitsize, 
    int *bitpos,
    struct type **type
  ) 

  {
    int i;
    if (obj_type == NULL) return 0;
    for (i = 0; i < TYPE_M3_OBJ_NMETHODS (obj_type); i++) {
      if (strcmp (TYPE_M3_OBJ_METHOD_NAME (obj_type, i), name) == 0) {
        if ( bitsize != NULL )   
          { * bitsize   = TYPE_M3_OBJ_METHOD_BITSIZE ( obj_type, i ); }
        if ( bitpos != NULL ) 
          { * bitpos = TYPE_M3_OBJ_METHOD_BITPOS ( obj_type, i ); }
        if ( type != NULL ) { * type = TYPE_M3_OBJ_METHOD_TYPE ( obj_type, i ); }
        return 1;
      }
    }
    return 0; 
  } /* m3_find_obj_method */ 

bool
m3_is_ordinal_type (type)
     struct type *type;
{
  enum type_code tc;

  tc = TYPE_CODE (type);
  while (tc == TYPE_CODE_M3_PACKED) {
    type = TYPE_M3_PACKED_TARGET (type);
    tc = TYPE_CODE (type);
  }

  switch (tc) {
    case TYPE_CODE_M3_SUBRANGE:
    case TYPE_CODE_M3_ENUM:
    case TYPE_CODE_M3_BOOLEAN:
    case TYPE_CODE_M3_CHAR:
    case TYPE_CODE_M3_WIDECHAR:
    case TYPE_CODE_M3_CARDINAL:
    case TYPE_CODE_M3_INTEGER:
      return 1;
    default:
      return 0;
  }
} /* m3_is_ordinal_type */ 

bool 
m3_type_is_signed ( struct type *type ) 

  { enum type_code tc;
    LONGEST lower; 

    tc = TYPE_CODE (type);
    while ( true ) 
      { if ( tc == TYPE_CODE_M3_INDIRECT ) 
          { type = TYPE_M3_INDIRECT_TARGET ( type );
            tc = TYPE_CODE ( type );
          }
        else if ( tc == TYPE_CODE_M3_PACKED ) 
          { type = TYPE_M3_PACKED_TARGET ( type );
            tc = TYPE_CODE ( type );
          }
        else break; 
      } 

    switch ( tc ) 
      { case TYPE_CODE_M3_SUBRANGE:
          lower = TYPE_M3_SUBRANGE_MIN (type);
          return lower < 0; 
        case TYPE_CODE_M3_INTEGER:
          return true; 
        default:
          return false; 
      }
  } /* m3_type_is_signed */ 

void
m3_ordinal_bounds ( struct type *type, LONGEST *lower, LONGEST *upper ) 

  { enum type_code tc;

    tc = TYPE_CODE (type);
    while ( true ) 
      { if ( tc == TYPE_CODE_M3_INDIRECT ) 
          { type = TYPE_M3_INDIRECT_TARGET ( type );
            tc = TYPE_CODE ( type );
          }
        else if ( tc == TYPE_CODE_M3_PACKED ) 
          { type = TYPE_M3_PACKED_TARGET ( type );
            tc = TYPE_CODE ( type );
          }
        else break; 
      } 

    switch ( tc ) 
      { case TYPE_CODE_M3_SUBRANGE:
          *lower = TYPE_M3_SUBRANGE_MIN (type);
          *upper = TYPE_M3_SUBRANGE_MAX (type);
          break;
        case TYPE_CODE_M3_ENUM:
          *lower = 0;
          *upper = TYPE_M3_ENUM_NVALS (type) - 1;
          break;
        case TYPE_CODE_M3_BOOLEAN:
          *lower = 0;
          *upper = 1;
          break;
        case TYPE_CODE_M3_CHAR:
          *lower = 0;
          *upper = 255;
          break;
        case TYPE_CODE_M3_WIDECHAR:
          *lower = 0;
          *upper = 0xffff;
          break;
        case TYPE_CODE_M3_CARDINAL:
          /* assumes a 2's complement machine... */
          *lower = 0;
          *upper = ~ ((-1L) << (TARGET_LONG_BIT-1));
          break;
        case TYPE_CODE_M3_INTEGER:
          /* assumes a 2's complement machine... */
          *lower = (-1L) << (TARGET_LONG_BIT-1);
          *upper = ~ ((-1L) << (TARGET_LONG_BIT-1));
          break;
        default:
          error ("gdb internal error: bad Modula-3 ordinal type code %d", tc );
          *lower = 0;
          *upper = 0;
      }
  } /* m3_ordinal_bounds */ 

gdb_byte *
m3_read_object_fields_bits (CORE_ADDR ref)
{
  CORE_ADDR tc_addr;
  int dataSize;
  gdb_byte *bits;

  if (ref == 0) { return 0; }
  tc_addr = m3_tc_addr_from_object_addr (ref);
  dataSize = m3_dataSize_from_tc_addr (tc_addr);
  bits = malloc (dataSize);
  /* FIXME^ Surely this is not the right way to allocate space in gdb. 
     rodney.bates@wichita.edu */
  read_memory (ref, bits, dataSize);
  return bits;
}

LONGEST
m3_extract_ord (
  const gdb_byte* valaddr, int bitpos, int bitsize, int sign_extend )

  {
    ULONGEST val;
    ULONGEST valmask;
    int lsbcount;

    /* FIXME: Using extract_unsigned_integer to get the range of bytes that
       cover the packed field into val will fail for packed fields that 
       could (depending on their bit alignment) span one more byte than
       is in a ULONGEST.  We should be able to handle this case.  It would
       require combining the functions of this function and 
       extract_unsigned_integer. */ 
    val = extract_unsigned_integer 
            ( valaddr + bitpos / TARGET_CHAR_BIT, 
              ( bitsize + TARGET_CHAR_BIT - 2 ) / TARGET_CHAR_BIT + 1
            );
    if ( BITS_BIG_ENDIAN )
      lsbcount = sizeof val * HOST_CHAR_BIT - bitpos - bitsize ;
    else
      lsbcount = bitpos % HOST_CHAR_BIT;
    val >>= lsbcount;

    /* If the field does not entirely fill a LONGEST, either zero the sign bits.
       or sign extend, as requested. */
    if ( bitsize < HOST_CHAR_BIT * ( int ) sizeof ( val ) )
      {
        valmask = ( ( ( ULONGEST ) 1 ) << bitsize ) - 1;
        val &= valmask;
        if ( sign_extend )
          { if ( val & ( valmask ^ ( valmask >> 1 ) ) )
              { val |= ~valmask; }
          }
      }
    return val;
  } /* m3_extract_ord */ 

LONGEST
m3_value_as_integer ( struct value * val )

{ LONGEST lower; 
 LONGEST upper; 
  struct type * type = value_type ( val );
  int typebitsize;
  int valbitsize;
  int bitsize;

  m3_ordinal_bounds ( type, & lower, & upper );
  typebitsize = TYPE_M3_SIZE ( type );
  valbitsize = value_bitsize ( val ); 
  if ( valbitsize == 0 ) 
    { bitsize = typebitsize; } 
  else { bitsize = valbitsize; } 
  return m3_extract_ord 
           ( value_contents ( val ),  
             value_bitpos ( val ),
	     bitsize, 
             ( lower < 0 )
           );
} /* m3_value_as_integer */ 

CORE_ADDR
m3_extract_address (const gdb_byte* valaddr, int bitpos)
{
  return (CORE_ADDR) m3_extract_ord (valaddr, bitpos, TARGET_PTR_BIT, 0);
}

double 
m3_value_as_float (struct value * val)
{
  double res;
  int size = TYPE_LENGTH (value_type (val));

  if (size == 4) {
    res = *(float *) value_contents (val); }
  else {
    res = *(double*) value_contents (val); }
  return res;
} /* m3_value_as_float */ 

/* If packed_val is not byte-aligned, return an equivalent, non-lval value
   that is.  Otherwise, return argument unchanged.  Either way, the result 
   can be fetched into gdb space and this passed to various things that 
   don't take bitpos and bitsize values.  
*/ 
struct value * 
m3_ensure_value_is_unpacked ( struct value * packed_val ) 

  { int bitsize; 
    int bitpos; 
    struct type * val_type; 
    struct value * result_val; 
    unsigned byte_length; 
    LONGEST value; 
    void * source_addr; 

    if ( packed_val == NULL ) { return packed_val; } 
    bitsize = value_bitsize ( packed_val ); 
    if ( bitsize == 0 ) /* Size will come from the type. */
       { return packed_val; } 
    bitpos = value_bitpos ( packed_val ); 
    if ( bitsize % HOST_CHAR_BIT == 0 && bitpos % HOST_CHAR_BIT == 0 ) 
       { return packed_val; }  
    /* Here, we have a non-byte aligned value. */ 
    val_type = value_type ( packed_val ); 
    byte_length = TYPE_LENGTH ( val_type ); 
    /* FIXME: The following is one byte too restrictive.  See the FIXME at 
       m3_unpack_ord. */ 
    if ( byte_length > sizeof ( value ) ) 
      { error ( "Packed field has %d bytes, can only unpack %d.", 
                byte_length,
                sizeof ( value )
              ); 
      } 
    value 
      = m3_extract_ord 
          ( value_contents ( packed_val ), 
            bitpos,
            bitsize, 
            m3_type_is_signed ( val_type ) 
          ); 
    result_val 
      = value_from_longest ( TYPE_M3_PACKED_TARGET ( val_type ), value ); 
#if 0
    result_val = allocate_value ( val_type ); 
    if ( BITS_BIG_ENDIAN )
      { source_addr = & value + sizeof ( value ) - byte_length; }  
    else { source_addr = & value; } 
    /* Host-to-host, no byte order problem: */ 
    /* FIXME: Not true.  struct value contents are in target order. */ 
    memcpy ( value_contents_raw ( result_val ), source_addr, byte_length );
    set_value_lazy ( result_val, 0 ); 
#endif 
    deprecated_set_value_modifiable ( result_val, 0 ); 
    return result_val; 
  } /* m3_ensure_value_is_unpacked */ 

static int 
m3_shape_component_offset ( int dimension ) 

  { int result; 

    result 
      = TARGET_PTR_BIT / HOST_CHAR_BIT /* Skip the elements pointer. */
        + dimension * ( TARGET_LONG_BIT/TARGET_CHAR_BIT );
    return result; 
  } /* m3_shape_component_offset */ 

/* Return the address of the zero-th element of the Modula-3 open array 
   whose dope is in gdb-space at address addr. */ 
CORE_ADDR 
m3_open_array_elems_addr ( const gdb_byte * addr ) 

  { CORE_ADDR result; 

    result = m3_extract_address ( addr, 0 ); 
    return result; 
  } /* m3_open_array_elems_addr */ 

/* Store elements address val into the Modula-3 open array 
   dope value located in gdb-space at address addr. */ 
void
m3_set_open_array_elems_addr ( gdb_byte * addr, CORE_ADDR val ) 

  { store_unsigned_integer 
      ( addr, TARGET_PTR_BIT / HOST_CHAR_BIT, ( ULONGEST ) val ) ; 
  } /* m3_set_open_array_elems_addr */ 

/* Return the dimension-th shape component (i.e., the element count for the
   dimension-th dimension), of the Modula-3 open array whose dope is in 
   gdb-space at address. */ 
ULONGEST 
m3_open_array_shape_component ( const gdb_byte * addr , int dimension )

  { ULONGEST result; 
    int target_offset; 

    target_offset = m3_shape_component_offset ( dimension );
    result 
      = m3_extract_ord ( addr + target_offset, 0, TARGET_LONG_BIT, 0 );
    return result; 
  } /* m3_open_array_shape_component */ 

/* Store shape component val into the dimension-th slot of Modula-3 
   open array dope located in gdb space at address addr. */ 
void
m3_set_open_array_shape_component ( 
    gdb_byte * addr, 
    int dimension, 
    ULONGEST val
  ) 

  { int target_offset; 

    target_offset = m3_shape_component_offset ( dimension );
    store_unsigned_integer 
      ( addr + target_offset, TARGET_LONG_BIT / HOST_CHAR_BIT, val ); 
  } /* m3_set_open_array_shape_component */ 

/* Fetch the inferior address of the zero-th element of the Modula-3 open array 
   whose dope begins at inferior address ref. */ 
CORE_ADDR 
m3_inf_open_array_elems_addr ( CORE_ADDR ref ) 

{ CORE_ADDR result; 
  gdb_byte buf [16] ; /* liberal. */ 

  read_memory ( ref, buf, TARGET_PTR_BIT );  
  result = m3_open_array_elems_addr ( buf ); 
  return result; 
} /* m3_inf_open_array_elems_addr */ 

/* TODO: Rationalize the following to maybe use [set_]open_array_shape_component,
   deciding which is responsible for computing the dimension offset. */ 

/* Return the dimension-th shape component (i.e., the element count for the
   dimension-th dimension), of the Modula-3 open array whose dope begins at 
   inferior address ref. */ 
ULONGEST 
m3_inf_open_array_shape_component ( CORE_ADDR ref, int dimension ) 

{ ULONGEST result; 
  int target_offset; 
  gdb_byte buf [16]; /* liberal. */ 

  target_offset = m3_shape_component_offset ( dimension ); 
  read_memory ( ref + target_offset, buf, TARGET_LONG_BIT );  
  result = m3_extract_ord (buf, 0, TARGET_LONG_BIT, 0);
  return result; 
} /* m3_inf_open_array_shape_component */ 

/* Fetch the inferior address of the zero-th element of the Modula-3 open array 
   whose dope is in gdb-space value array_val. */ 
CORE_ADDR 
m3_value_open_array_elems_addr ( struct value * array_value ) 

{ CORE_ADDR result; 

  result 
    = m3_extract_address 
        ( ( gdb_byte * ) value_contents ( array_value ), 0 ); 
  return result; 
} /* m3_value_open_array_elems_addr */ 

/* Store inferior elements address val into the Modula-3 open array 
   dope value located in gdb-space value array_val. */ 
void
m3_set_value_open_array_elems_addr ( 
    struct value * array_value, 
    CORE_ADDR val 
  ) 

{ store_unsigned_integer 
    ( ( gdb_byte * ) value_contents_raw ( array_value ), 
      TARGET_PTR_BIT / HOST_CHAR_BIT, 
      ( ULONGEST ) val
    ) ; 
} /* m3_set_value_open_array_elems_addr */ 

/* Return the dimension-th shape component (i.e., the element count for the
   dimension-th dimension), of the Modula-3 open array whose dope is in 
   gdb-space value array_val. */ 
ULONGEST 
m3_value_open_array_shape_component ( struct value * array_value, int dimension )

{ ULONGEST result; 
  int target_offset; 

  target_offset = m3_shape_component_offset ( dimension );
  result 
    = m3_extract_ord 
        ( ( gdb_byte * ) value_contents ( array_value ) + target_offset, 
          0, TARGET_LONG_BIT, 0
        );
  return result; 
} /* m3_value_open_array_shape_component */ 

/* Store shape component val into the dimension-th slot of the Modula-3 
   open array dope that is in gdb-space value array_val. */ 
void
m3_set_value_open_array_shape_component ( 
    struct value * array_value,
    int dimension, 
    ULONGEST val
   ) 

{ int target_offset; 

  target_offset = m3_shape_component_offset ( dimension );
  store_unsigned_integer 
    ( ( gdb_byte * ) value_contents_raw ( array_value ) + target_offset, 
      TARGET_LONG_BIT / HOST_CHAR_BIT, 
      val
    ) ; 
} /* m3_set_value_open_array_shape_component */ 

/* For a Modula-3 type, we will construct only one indirect type to it.  
   So reuse existing pointer-type mechanism from C/C++, but change
   its type code.  
   This could duplicate a compiler-generated type, but that would be
   hard to find. */ 
struct type *
m3_indirect_type_from_type (struct type *type)
{ struct type * result; 

  result = make_pointer_type (type, (struct type **) 0 );
  TYPE_CODE ( result ) = TYPE_CODE_M3_INDIRECT; 
  return result; 
}

/* TODO:  Make this value more dependable: */ 
const int static_link_offset = 0; 

struct frame_info * 
m3_static_parent_frame ( struct frame_info *start_frame ) 

  { struct frame_info * frame ; 
    CORE_ADDR static_link; 

    if (start_frame == NULL) return NULL; 

    /* Maybe just call read_memory? */
    static_link 
       = read_memory_typed_address /* from gdbcore.h */ 
           ( get_frame_locals_address ( start_frame ) + static_link_offset, 
             builtin_type_void_data_ptr 
           ); 
    frame = start_frame; 
    do 
      { frame = get_prev_frame (frame); 
        if (frame == NULL )
          { error (_("Static link does not lead to a valid frame."));
            /* NORETURN */ 
          }  
      } 
    while ( ! m3_address_lies_within_frame_locals ( static_link, frame ) ); 
    return frame; 
  } /* m3_static_parent_frame */ 

/* CHECK: Is this value target-dependent? */ 
const CORE_ADDR closure_mark = - 1L;  

struct type *
m3_alloc_closure_type ( struct type * proc_type ) 

{ struct type * result; 

  /* FIXME: There is only one type object builtin_type_m3_proc_closure, 
     and we always return it.  We ought to allocate a new one here,
     because we patch its TYPE_TARGET_TYPE.  But it would need to be
     in the same space as values, and that would entail duplicating
     allocate_type.  Instead, we assume there will never be more than one 
     user command being evaluated at a time and only one call at a time
     within a command, even for nested calls.  So we should be able to
     get away with this.  But it's sleazy. */ 
  result = builtin_type_m3_proc_closure;
  TYPE_TARGET_TYPE ( result ) = proc_type; 
  return result; 
} /* m3_alloc_closure_type */ 

/* Build a procedure closure value, entirely in gdb's process space. */ 
struct value * 
m3_build_gdb_proc_closure (
    struct type * proc_type, 
    CORE_ADDR code_addr, 
    CORE_ADDR env_ptr 
  ) 

  { struct value * result; 
    struct type * closure_type; 
    struct m3_proc_closure * closure; 

    closure_type = m3_alloc_closure_type ( proc_type ); 
    result = allocate_value ( closure_type ); 
    closure = ( struct m3_proc_closure * ) value_contents_raw ( result ); 
    store_unsigned_integer 
      ( ( gdb_byte * ) & closure -> closure_mark, 
        TARGET_PTR_BIT / HOST_CHAR_BIT, 
        ( ULONGEST ) closure_mark 
      ) ; 
    store_unsigned_integer 
      ( ( gdb_byte * ) & closure -> code_addr, 
        TARGET_PTR_BIT / HOST_CHAR_BIT, 
        ( ULONGEST ) code_addr 
      ) ; 
    store_unsigned_integer 
      ( ( gdb_byte * ) & closure -> env_ptr, 
        TARGET_PTR_BIT / HOST_CHAR_BIT, 
        ( ULONGEST ) env_ptr 
      ) ; 
    return result;     
  } /* m3_build_gdb_proc_closure */

/* Is inf_addr the inferior address of a Modula-3 procedure closure? */
bool 
m3_inf_address_is_proc_closure ( CORE_ADDR inf_addr ) 

  { struct value * deref_value;
    CORE_ADDR l_closure_mark;  

    if ( inf_addr == 0 ) { return false; } 
    deref_value = value_at_lazy ( builtin_type_m3_integer, inf_addr );
    l_closure_mark = m3_value_as_integer ( deref_value );  
    return l_closure_mark == closure_mark;  
  } /* m3_inf_address_is_proc_closure */ 

/* Is closure_value a Modula-3 procedure closure value?  */ 
bool 
m3_value_is_proc_closure ( struct value * closure_value ) 

  { struct type * closure_type; 
    struct m3_proc_closure  * closure; 

    if ( closure_value == NULL ) { return false; } 
    closure_type = value_type ( closure_value ); 
    if ( closure_type == NULL ) { return false; } 
    if ( TYPE_CODE ( closure_type ) == TYPE_CODE_M3_PROC_CLOSURE ) 
      { closure = ( struct m3_proc_closure * ) value_contents ( closure_value ); 
        return closure -> closure_mark == closure_mark; 
      } 
    else { return false; } 
  } /* m3_value_is_proc_closure */ 

/* valaddr points to a gdb-space value of Modula-3 procedure type, which
   in turn could be a pointer to the procedures 's code or to a closure.  
   Either way, return the code address. */ 
CORE_ADDR 
m3_proc_code_addr ( const gdb_byte * valaddr ) 

  { CORE_ADDR inf_addr; 
    struct value * closure_value;

    inf_addr = m3_extract_address ( valaddr, 0 );  
    if ( m3_inf_address_is_proc_closure ( inf_addr ) ) 
      { closure_value = value_at_lazy ( builtin_type_m3_proc_closure, inf_addr );
      /* Kinda sleazy, reusing builtin_type_m3_proc_closure, without even 
         patching its target type, but I think it will work. */ 
        return 
          ( * ( struct m3_proc_closure * ) value_contents ( closure_value ) ) 
          . code_addr;
      } 
    else { return inf_addr; } 
  } /* m3_proc_code_addr */ 

/* valaddr points to a gdb-space value of Modula-3 procedure type, which
   in turn could be a pointer to the procedures 's code or to a closure.  
   Either way, return the enviroment pointer, which will, of course, be
   zero in the former case. */ 
CORE_ADDR 
m3_proc_env_ptr ( const gdb_byte * valaddr ) 

  { CORE_ADDR inf_addr; 
    struct value * closure_value;

    inf_addr = m3_extract_address ( valaddr, 0 );  
    if ( m3_inf_address_is_proc_closure ( inf_addr ) ) 
      { closure_value = value_at_lazy ( builtin_type_m3_proc_closure, inf_addr );
      /* Kinda sleazy, reusing builtin_type_m3_proc_closure, without even 
         patching its target type, but I think it will work. */ 
        return 
          ( * ( struct m3_proc_closure * ) value_contents ( closure_value ) ) 
          . env_ptr;
      } 
    else { return 0; } 
  } /* m3_proc_env_ptr */ 

/* mininum and maximum displacements of local variables in a block.  
   the range will be half-open, such that the variables occupy 
   displacements [min,max) */
static void  
m3_block_locals_range ( struct block * blk, LONGEST * min, LONGEST * max ) 

  { struct dict_iterator iter;
    struct symbol * sym; 
    LONGEST l_min;
    LONGEST l_max;
    LONGEST l_displ = 0; 
    struct symbol * l_max_sym = NULL; 
    struct type * l_max_type; 

    l_min = 0;
    l_max = 0;
    ALL_BLOCK_SYMBOLS ( blk, iter, sym )
      { l_displ = SYMBOL_VALUE ( sym ); 
        if ( l_displ < l_min ) { l_min = l_displ; } 
        if ( l_displ > l_max ) 
           { l_max = l_displ; 
             l_max_sym = sym; 
           } 
      }  
    if ( l_max_sym != NULL ) 
      { l_max_type = SYMBOL_TYPE ( l_max_sym ); 
        if ( l_max_type != NULL ) { l_max += TYPE_LENGTH ( l_max_type ); } 
      }
    if ( min != NULL ) { * min = l_min; }  
    if ( max != NULL ) { * max = l_max; }  
  } /* m3_block_locals_range */ 

bool 
m3_address_lies_within_frame_locals ( 
    CORE_ADDR address, 
    struct frame_info * frame 
  ) 

{ struct block * blk; 
  LONGEST min_displ; 
  LONGEST max_displ; 
  CORE_ADDR locals_address; 
  
  if ( frame == NULL ) { return false; } 
  blk = get_frame_block ( frame, NULL ); 
  if ( blk == NULL ) { return false; } 
  m3_block_locals_range ( blk, & min_displ, & max_displ ); 
  locals_address = get_frame_locals_address ( frame ); 
  if ( address < locals_address + min_displ ) { return false; } 
  if ( address >= locals_address + max_displ ) { return false; } 
  return true; 
} /* m3_address_lies_within_block_locals */ 

/* End of file m3-util.c */ 
