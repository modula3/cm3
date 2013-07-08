/* *INDENT-OFF* */

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

#include "m3-bool.h"

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
#include "exceptions.h"
#include "target.h"

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

BOOL m3_constant_init_done = FALSE;

/* Compare two terminated strings for equality.  A terminated string is
   described by a from pointer and a to pointer.  Its last character is
   the character before either a null byte or before the character pointed
   to by the to pointer. */
static BOOL
m3_term_strings_equal (
    const char * left,
    const char * left_to,
    const char * right,
    const char * right_to
  )
  { BOOL left_done, right_done;

    left_done = ( left == NULL || left == left_to || * left == '\0' );
    right_done = ( right == NULL || right == right_to || * right == '\0' );
    while ( TRUE )
      { if ( left_done ) { return right_done; }
        else /* ! left_done */
          { if ( right_done ) { return FALSE; }
            else if ( * left != * right ) { return FALSE; }
            else
              { left ++;
                left_done = ( left == left_to || * left == '\0' );
                right ++;
                right_done = ( right == right_to || * right == '\0' );
              }
          }
      }
  }

void
init_m3_constants (void)
{ BOOL is_cm3;

  if ( m3_constant_init_done ) { return; }

  { struct type* rt0_tc;
    struct type* rt0_otc;

    rt0_tc = find_m3_type_named ("RT0.Typecell", /*must_find =*/ FALSE);
    if (!rt0_tc)
      {
        error ("Can't find RT0.Typecell. Maybe M3 libraries are compiled "
               "without debug info, or not stabs.\n"
              );
        return; /* without setting m3_constant_init_done. */
      }
    m3_find_rec_field (rt0_tc, "selfID",
                     &rt0_tc_selfID_size, &rt0_tc_selfID_offset, 0);
    /* I don't know why, but the compiler seems to be sign-extending
       uid values into 64 bits on 64-bit targets.  Artificially change
       rt0_tc_selfID_size to compensate. */
    rt0_tc_selfID_size = 32;
    m3_find_rec_field (rt0_tc, "dataSize",
                     &rt0_tc_dataSize_size, &rt0_tc_dataSize_offset, 0);

    rt0_otc = find_m3_type_named ("RT0.ObjectTypecell", /*must_find =*/ FALSE);
    is_cm3 = (rt0_otc != 0);

    if (is_cm3)
      { struct type* t;
        struct symbol *rttype;
        int types_size, types_offset;

        gdb_assert ( m3_compiler_kind ( ) == m3_ck_cm3 );
        m3_find_rec_field
          (rt0_tc, "kind", &rt0_tc_kind_size, &rt0_tc_kind_offset, 0);

        t = find_m3_type_named ("RTType.InfoMap", /*must_find =*/ TRUE);
        m3_find_rec_field (t, "map",
          &rttype_infomap_map_size, &rttype_infomap_map_offset, 0);
        m3_find_rec_field (t, "cnt",
          &rttype_infomap_cnt_size, &rttype_infomap_cnt_offset, 0);

        t = find_m3_type_named ("RTType.Info", /*must_find =*/ TRUE);

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

        gdb_assert ( m3_compiler_kind ( ) == m3_ck_pm3 );
        rt0u = m3_unit_name_globals_symbol ('I', "RT0u", NULL );

        m3_find_rec_field (SYMBOL_TYPE (rt0u), "types",
                           &rt0u_types_size, &rt0u_types_offset, 0);

        rt0u_types_value = 0; /* So as to left-extend with zeros. */
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

  m3_constant_init_done = TRUE;
}

BOOL
is_unsafe ( void )

  { /* FIXME: Implement this. */
    return TRUE;
  } /* is_unsafe */


const LONGEST m3_type_magic_value = ( LONGEST ) M3_TYPE_MAGIC;

/* Print the character C on STREAM as part of the contents of a literal
   string whose delimiter is QUOTER.  Note that that format for printing
   characters and strings is language specific. */

void
m3_emit_char (
     int c,
     struct ui_file *stream,
     int quoter
   )
{ c &= 0xFF; /* Avoid sign bit follies */
  if ( PRINT_LITERAL_FORM ( c ) )
    { if (c == '\\' || c == quoter)
        { fputs_filtered ("\\", stream); }
      fprintf_filtered (stream, "%c", c);
    }
  else
    { switch (c)
        { case '\n':
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
            fprintf_filtered (stream, "\\%.3o", (unsigned int) c);
            break;
        }
    }
} /* m3_emit_char */

void
m3_emit_widechar (
    int c, struct ui_file *stream, int quoter )

  { c &= 0xFFFF; /* Avoid sign bit follies */
    if ((c <= 0xFF) && PRINT_LITERAL_FORM (c))
      { if (c == '\\' || c == quoter)
         { fputs_filtered ("\\", stream); }
        fprintf_filtered (stream, "%c", c);
      }
    else
      { switch (c)
          { case '\n':
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
  } /* m3_emit_widechar */

void
m3_print_char_lit (
    int c,
    struct ui_file *stream
  )
  { fputs_filtered ("'", stream);
    m3_emit_char (c, stream, '\'');
    fputs_filtered ("'", stream);
  } /* m3_print_char_lit */

void
m3_print_widechar_lit (
    int c,
    struct ui_file *stream
  )
  { fputs_filtered ("W'", stream);
    m3_emit_widechar (c, stream, '\'');
    fputs_filtered ("'", stream);
  } /* m3_print_widechar_lit */

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
  /* FIXME:  This looks like a mess.  The length passed seems to be in
             bytes regardless of width, but this looks mostly coded to
             assume in is in units of width bytes.  But not everywhere,
             see, eg. call on m3_print_char_lit(string[i]. */

  /* If the string was not truncated due to `set print elements', and
     the last byte of it is a null, we don't print that, in traditional C
     style.  */
  if ((!force_ellipsis) && length > 0
      && extract_unsigned_integer (string + (length - 1)*width, width) == '\0'
     )
    length -+ width;

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
          m3_print_char_lit (string[i], stream);
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
    struct type * base_type = m3_ordinal_base_type ( type, NULL );
    enum type_code code = TYPE_CODE ( base_type );
    int len = TYPE_LENGTH ( type );

    /* FIXME: Do bounds checks here? */
    switch ( code )
      { case TYPE_CODE_M3_INTEGER:
        case TYPE_CODE_M3_LONGINT:
        case TYPE_CODE_M3_CARDINAL:
        case TYPE_CODE_M3_LONGCARD:
          /* Although CARDINAL and LONGCARD are non-negative, they are
             effectively subranges of INTEGER and LONGINT. */
          store_signed_integer ( value_contents_raw ( val ), len, num );
          break;

        case TYPE_CODE_M3_CHAR:
        case TYPE_CODE_M3_WIDECHAR:
        case TYPE_CODE_M3_ENUM:
        case TYPE_CODE_M3_BOOLEAN:
          store_unsigned_integer ( value_contents_raw ( val ), len, num );
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

/* Modula-3 expressions can be either values or types.  m3gdb
   'struct value's represent either, despite the name.  This
   creates a 'struct value' that represents a type. */
struct value *
m3_allocate_value_that_is_type ( struct type * type_of_value )
  { struct value * result;

    result = allocate_value ( type_of_value );
    * ( LONGEST * ) value_contents_raw ( result ) = m3_type_magic_value;
    return result;
  } /* m3_allocate_value_that_is_type */

/* Modula-3 expressions can be either values or types.  m3gdb
   'struct value's represent either, despite the name.  This
   distinguishes. */
BOOL
m3_value_is_type ( struct value * val )
  { BOOL result;

    result = ( * ( LONGEST * ) value_contents ( val ) == m3_type_magic_value );
    return result;
  } /* m3_value_is_type */

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
find_m3_type_named (
    char * name,
    BOOL must_find /* Emit an error message, if can't find it. */
  )

  { char struct_name [ M3_MAX_SYMBOLLEN ];
    struct symbol *s;

    /* FIXME: Rework this.  Use a less drastic way than lookup_symbol,
       take apart the qualified name and look up the interface and type
       id separately, and change the demangler to unqualify the name
       of a type found in an Mn_zzzzzz_Interface.Type entry.  Also,
       get rid of the B$ in the prefix, which will allow ordinary lookup
       to not need special cases for types. */
    snprintf (struct_name, M3_MAX_SYMBOLLEN, "B$%s", name);
    s = lookup_symbol (struct_name, 0, STRUCT_DOMAIN, 0, 0);
    if (s == NULL) {
      if (must_find) error ("unable to find type named \"%s\"\n", name);
      return NULL;
    }
    return TYPE_M3_TYPE_TYPE (SYMBOL_TYPE (s));
  } /* find_m3_type_named */

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

/* Lookup a symbol in the all static blocks.  */
struct symbol *
m3_lookup_symbol_all_static (
    const char *name,
    const char *linkage_name,
    const domain_enum domain,
    struct symtab * * symtab
  )

  { struct symbol * sym;

    if ( symtab != NULL )
      { * symtab = NULL; } /* In case we find nothing. */
    sym = lookup_symbol_aux_symtabs
            ( STATIC_BLOCK, name, linkage_name, domain, symtab );
    if ( sym != NULL )
       { return sym; }
    sym = lookup_symbol_aux_psymtabs
            ( STATIC_BLOCK, name, linkage_name, domain, symtab );
    return sym;
  } /* m3_lookup_symbol_all_static */

/* Lookup a symbol in the all global blocks.  */
struct symbol *
m3_lookup_symbol_all_global (
    const char *name,
    const char *linkage_name,
    const domain_enum domain,
    struct symtab * * symtab
  )

  { struct symbol * sym;

    if ( symtab != NULL ) { * symtab = NULL; } /* In case we find nothing. */
    sym = lookup_symbol_aux_symtabs
            ( GLOBAL_BLOCK, name, linkage_name, domain, symtab );
    if ( sym != NULL ) { return sym; }
    sym = lookup_symbol_aux_psymtabs
            ( GLOBAL_BLOCK, name, linkage_name, domain, symtab );
    return sym;
  } /* m3_lookup_symbol_all_global */

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

/* If blk contains a symbol with the right name spelling to be an interface
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

/* Return the interface or module global variable record symbol,
   and the symtab it was found in, for interface or module named
   'unit_name'.  kind == 'M' to find a module global symbol, or
   kind == 'I' for an interface.  For an interface, its demangled
   name is "I$<interfaceName>".  For a module, its demangled name
   is "M$<moduleName>".  kind should be either 'I' or 'M', for
   interface or module.  It could be located in any static or global
   block in the mass of linked-together code.
*/
struct symbol *
m3_unit_name_globals_symbol (
    int kind,
    const char *unit_name,
    struct symtab * * symtab
  )

  { struct symbol * sym;
    char struct_name [ M3_MAX_SYMBOLLEN + 3 ];

    struct_name [ 0 ] = kind;
    struct_name [ 1 ] = '$';
    struct_name [ 2 ] = '\0';
    strncat ( struct_name, unit_name, M3_MAX_SYMBOLLEN );
    sym = m3_lookup_symbol_all_static_and_global
            ( struct_name, NULL, VAR_DOMAIN, symtab );
   return sym;
  } /* m3_unit_name_globals_symbol */

/* Is sym the symbol of a Modula-3 globals record for either an interface
   or a module? */
BOOL
m3_is_globals_record_symbol ( const struct symbol * sym )

{ char * name;
  size_t len;

  name = SYMBOL_LINKAGE_NAME ( sym );
  if ( name == NULL ) { return FALSE; }
  len = strlen ( name );
  if ( len < 4 ) { return FALSE; }
  if ( name [ 0 ] == 'M'
       && ( name [ 1 ] == 'I' || name [ 1 ] == 'M' )
       && name [ 2 ] == '_' )
    { return TRUE; }
  else { return FALSE; }
} /* m3_is_globals_record_symbol */

BOOL
m3_is_interface_global_record ( struct symbol * sym )

{ char * name;
  size_t len;

  name = SYMBOL_LINKAGE_NAME ( sym );
  if ( name == NULL ) { return FALSE; }
  len = strlen ( name );
  if ( len < 4 ) { return FALSE; }
  if ( name [ 0 ] == 'M' && name [ 1 ] == 'I' && name [ 2 ] == '_' )
    { return TRUE; }
  else { return FALSE; }
} /* m3_is_interface_global_record */

BOOL
m3_is_module_global_record ( struct symbol * sym )

{ char * name;
  size_t len;

  name = SYMBOL_LINKAGE_NAME ( sym );
  if ( name == NULL ) { return FALSE; }
  len = strlen ( name );
  if ( len < 4 ) { return FALSE; }
  if ( name [ 0 ] == 'M' && name [ 1 ] == 'M' && name [ 2 ] == '_' )
    { return TRUE; }
  else { return FALSE; }
} /* m3_is_module_global_record */

/* Return the pseudo-record-type that has one field for each exported
   interface.  It's demangled name is "H$<moduleName>.
   If the result is NIL, this means that the module exports exactly one
   interface, with the same name as the module.
*/
struct type *
find_m3_exported_interfaces ( const char * module_name )

  { char struct_name [ M3_MAX_SYMBOLLEN ];
    struct symbol *sym;

    snprintf ( struct_name, M3_MAX_SYMBOLLEN, "H$%s", module_name );
    sym = m3_lookup_symbol_all_static_and_global
            ( struct_name, NULL, STRUCT_DOMAIN, NULL );
    if ( sym != NULL )
      { return SYMBOL_TYPE ( sym ); }
    else { return NULL; }
  } /* find_m3_exported_interfaces */

char *
find_m3_type_name ( struct type * t )

  { char *uid = TYPE_TAG_NAME (t);
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
  } /* find_m3_type_name */

/* Is sym the symbol of a Modula-3 type name? */
BOOL
m3_is_type_name_symbol ( const struct symbol * sym )

  { char * name;
    size_t len;

    name = SYMBOL_SEARCH_NAME ( sym );
    if ( name == NULL ) { return FALSE; }
    len = strlen ( name );
    if ( len < 3 ) { return FALSE; }
    if ( name [ 0 ] == 'B'
         && name [ 1 ] == '$'
       )
      { return TRUE; }
    else { return FALSE; }
  } /* m3_is_type_name_symbol */

/* Look in 'block' for a declaration of a type. */
struct symbol *
m3_lookup_type (
    const char * unit_name,
    const char * name,
    const struct block * blk,
    struct symtab * * symtab
  )

  { char type_name [ M3_MAX_SYMBOLLEN * 2 + 4 ];
    struct symbol * sym;

    /* Just in case we ever demangle these into unqualified names. */
    snprintf ( type_name, M3_MAX_SYMBOLLEN, "B$%s", name );
    sym = lookup_symbol_static ( type_name, NULL, blk, STRUCT_DOMAIN, symtab );
    if ( sym != NULL ) { return sym; }
    if ( unit_name != NULL )
      { type_name [ 0 ] = 'B';
        type_name [ 1 ] = '$';
        type_name [ 2 ] = '\0';
        strncat ( type_name, unit_name, M3_MAX_SYMBOLLEN );
        strcat ( type_name, "." );
        strncat ( type_name, name, M3_MAX_SYMBOLLEN );
        /* ^I know this is absurdly inefficient, but it's the C way. */
        sym
          = lookup_symbol_static
              ( type_name, NULL, blk, STRUCT_DOMAIN, symtab );
      }
    return sym;
  } /* m3_lookup_type */

/* See if identifier 'ident' is declared in interface named 'interface_name'.
   Return its symbol if so, or NULL if not.  EXCEPT: global variables have no
   symbol.  If it's a global variable, return the symbol for the globals
   record for the interface.  Caller will have to detect this case and combine
   it with 'ident' in its own way. If a symbol is found and result_symtab is
   non-NULL, set result_symtab to the containing symbol table.  If this is a
   procedure declared in interface 'interface_name', the symbol and
   result_symtab returned will belong to the exporting _module_ (this is
   the only symbol we have.)  This will not find a procedure that is not
   declared in an interface. */
struct symbol *
m3_lookup_interface_id (
    const char * interface_name,
    const char * ident,
    struct symtab * * result_symtab
  )

  { struct symbol * interface_rec_sym;
    struct symbol * sym;
    struct block * interface_static_block;
    struct type * global_type;
    char linkage_name [ M3_MAX_SYMBOLLEN * 2 + 3 ];
    struct blockvector *bv;
    struct symtab * interface_symtab;
    struct symtab * module_symtab;
    BOOL found;

    /* Look for the interface global record. */
    interface_rec_sym
      = m3_unit_name_globals_symbol
          ( 'I', interface_name, & interface_symtab );
    if ( interface_rec_sym != NULL )
      { /* Look in the static block of the interface, where we will find
           a type declared in the interface, with transformed name, the
           same for all compilers. */
        if ( interface_symtab != NULL )
          { bv = BLOCKVECTOR ( interface_symtab );
            interface_static_block = BLOCKVECTOR_BLOCK ( bv, STATIC_BLOCK );
            sym = m3_lookup_type
                    ( interface_name,
                      ident,
                      interface_static_block,
                      result_symtab
                    );
            if ( sym != NULL ) { return sym; }
          }

        /* Look in the interface global record, where we will find a variable
           or, for PM3, a procedure, that was declared in the interface. */
        found
          = m3_find_rec_field
              ( SYMBOL_TYPE ( interface_rec_sym ),
                ident, 0, 0, & global_type
              );
        /* SRC, PM3, and EZM3 put a procedure declared in an
           interface into the interface global record.  But we don't
           want to use it from there, because this gives no symbol
           for it. Find it differently below.  However, for PM3 and friends,
           just its presence here will be important below, if it is also
           found as a procedure symbol. */

        /* A procedure declared in an interface has a symbol only in the
           exporting module.  Look for it by constructing the linkage name
           "interfaceName__ProcName" and looking in all global and static
           blocks. */
        linkage_name [ 0 ] = '\0';
        strncat ( linkage_name, interface_name, M3_MAX_SYMBOLLEN );
        strcat ( linkage_name, "__" );
        strncat ( linkage_name, ident, M3_MAX_SYMBOLLEN );
        sym = m3_lookup_symbol_all_global
                ( ident, linkage_name, VAR_DOMAIN, & module_symtab );
        if ( sym != NULL
             && sym->aclass != LOC_STATIC /* Is this still necessary? */
           )
          /* Finding this linkage name in a global block means this is CM3 and
             this is a procedure declared in interface 'interface_name'.  Even
             so, it is in the symbol table for the exporting _module_ where sym
             will have been found.  This could have a different module name.
             'result_symtab' will be set for this module. */
          { if ( result_symtab != NULL) { * result_symtab = module_symtab; }
            return sym;
          }

        /* Now try the static block of all symtabs. */
        sym = m3_lookup_symbol_all_static
                ( ident, linkage_name, VAR_DOMAIN, & module_symtab );
        if ( sym != NULL
             && sym->aclass != LOC_STATIC /* Is this still necessary? */
             && found
           )
          /* Finding 'linkage_name' in a static block after having previously
             found 'name' in the interface globals record means this is PM3
             etc. and this is a procedure declared in interface
             'interface_name'.  Even so, it is in the symbol table for the
             exporting _module_, where sym will have been found.  This could
             have a different module name.  'result_symtab' will be set for this
              module. */
          { if ( result_symtab != NULL) { * result_symtab = module_symtab; }
            return sym;
          }

        if ( found )
          /* We found ident in the interface global record earlier, but did not
             also find it as a procedure, so it must really be a variable. */
          { if ( result_symtab != NULL) { * result_symtab = interface_symtab; }
            return interface_rec_sym;
          }
      }
    /* 'name' is not declared in an interface named 'interface_name'. */
    return NULL;
  } /* m3_lookup_interface_id */

/* See if identifier 'ident' is declared in module named 'module_name'.
   Return its symbol if so, or NULL if not.  EXCEPT: global variables have no
   symbol.  If it's a global variable, return the globals record for the
   module.  Caller will have to detect this case and combine it with
   'ident' in its own way. */
struct symbol *
m3_lookup_module_id (
    const char * module_name,
    const char * ident,
    struct symtab * * result_symtab
  )

  { struct symbol * module_rec_sym;
    struct symbol * sym;
    struct block * module_static_block;
    struct block * module_global_block;
    struct type * global_type;
    char linkage_name [ M3_MAX_SYMBOLLEN * 2 + 3 ];
/* FIXME: ^Get the space allocation right for this. */
    struct blockvector *bv = NULL;
    struct symtab * module_symtab;
    struct symtab * l_symtab;
    BOOL found;

    /* Look for the module global record. */
    module_rec_sym
      = m3_unit_name_globals_symbol ( 'M', module_name, & module_symtab );
    if ( module_rec_sym != NULL && module_symtab != NULL )
      { bv = BLOCKVECTOR ( module_symtab );
        module_static_block = BLOCKVECTOR_BLOCK ( bv, STATIC_BLOCK );

        /* Look in the static block of the module, where we will find
           a type or procedure declared in the module.  This is the same
           for all compilers. */
        sym = m3_lookup_type
                ( module_name,
                  ident,
                  module_static_block,
                  result_symtab
                );
        if ( sym != NULL ) { return sym; }
       
        /* Construct the linkage name "moduleName__ProcName" and look for
           it in the static and global blocks of this module. */
        linkage_name [ 0 ] = '\0';
        strncat ( linkage_name, module_name , M3_MAX_SYMBOLLEN );
        strcat ( linkage_name, "__" );
        strncat ( linkage_name, ident, M3_MAX_SYMBOLLEN );
        sym = lookup_symbol_aux_block
                ( ident, linkage_name, module_static_block,
                  VAR_DOMAIN, & l_symtab
                );
        if ( sym != NULL
             && sym->aclass != LOC_STATIC /* Is this still necessary? */
           )
          /* We found it in the static block.  This is the procedure we want,
             regardless of whether it is exported. */
          { if ( result_symtab != NULL) { * result_symtab = l_symtab; }
            return sym;
          }

        /* For CM3, if the procedure is exported or it's a later compiler,
           it will be in the global block. */
        module_global_block = BLOCKVECTOR_BLOCK ( bv, GLOBAL_BLOCK );
        sym = lookup_symbol_aux_block
                ( ident, linkage_name, module_global_block,
                  VAR_DOMAIN, & l_symtab
                );
        if ( sym != NULL
             && sym->aclass != LOC_STATIC /* Is this still necessary? */
           )
          { if ( result_symtab != NULL) { * result_symtab = l_symtab; }
            return sym;
          }

        /* Look in the module global record, where we will find a variable.
           For PM3, a procedure that was declared  in the module will be
           here too, but we would have found it differently above and
           returned. */
        found
          = m3_find_rec_field
              ( SYMBOL_TYPE ( module_rec_sym ),
                ident, 0, 0, & global_type
              );
        if ( found )
          { if ( result_symtab != NULL) { * result_symtab = module_symtab; }
            return module_rec_sym;
          }
      }

    return NULL;
  } /* m3_lookup_module_id */

/* See if 'ident' is declared in an exported interface of module named
   'module', which we assume we are currently executing in some block of.
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
            sym = m3_lookup_interface_id ( interface_name, ident, symtab );
            if ( sym != NULL ) { return sym; }
          }
      }

    /* Not found. */
    if ( symtab != NULL ) { * symtab = NULL; }
    return NULL;
  } /* m3_lookup_exported */

enum m3_compiler_kind_typ m3_compiler_kind_value = m3_ck_unknown;
enum m3_compiler_kind_typ m3_old_compiler_kind = m3_ck_unknown;

/* This must be called after symbols for the executable itself have been
   loaded, but RTS symbols (which could be dynamically linked) are not
   required. */
void
m3_ascertain_compiler_kind ( void )
  { struct symbol * sym;
    struct minimal_symbol * minsym;

    if ( m3_compiler_kind_value == m3_ck_unknown )
      { if ( ! have_full_symbols ( ) && ! have_partial_symbols ( ) )
          { error
              (_("No symbol table is loaded.  Use the \"file\" command."));
            /* NORETURN */
          }

        sym = m3_lookup_symbol_all_static
                ( "m3_link_info", "m3_link_info", VAR_DOMAIN, NULL);
            /* ^This symbol appears in _m3main.o, of PM3-compiled code only, 
                either code generator. */
        if ( sym != NULL )
           { m3_compiler_kind_value = m3_ck_pm3;
             return;
           }

        sym = m3_lookup_symbol_all_static
                ( "m3main", "MM__m3main", VAR_DOMAIN, NULL);
        /* ^This symbol appears in _m3main.o, of earlier CM3-compiled 
            code only. */
        if ( sym != NULL )
           { m3_compiler_kind_value = m3_ck_cm3;
             return;
           }

        sym = m3_lookup_symbol_all_static
                ( "INTEGER", "INTEGER", VAR_DOMAIN, NULL);
            /* ^This symbol appears in _m3main.o, of later CM3-compiled
                code, gcc backend. */
        if ( sym != NULL )
           { m3_compiler_kind_value = m3_ck_cm3;
             return;
           }

        minsym = lookup_minimal_symbol ( "m3_link_info", NULL, NULL);
        if ( minsym != NULL )
           { m3_compiler_kind_value = m3_ck_pm3;
             return;
           }

        minsym = lookup_minimal_symbol ( "MM__m3main", NULL, NULL);
        if ( minsym != NULL )
           { m3_compiler_kind_value = m3_ck_cm3;
             return;
           }

        minsym = lookup_minimal_symbol ( "INTEGER", NULL, NULL);
        if ( minsym != NULL )
           { m3_compiler_kind_value = m3_ck_cm3;
             return;
           }

        m3_compiler_kind_value = m3_ck_not_m3;
      }
  } /* m3_ascertain_compiler_kind */

enum m3_compiler_kind_typ
m3_compiler_kind (void)
  { m3_ascertain_compiler_kind ( );
    return m3_compiler_kind_value;
  } /* m3_compiler_kind */

/* Strip away any indirect types from a type. */
struct type *
m3_direct_type ( struct type * param_type )

  { struct type * l_type;

    if ( param_type == NULL )
      { return NULL; }
    l_type = param_type;
    while ( TYPE_CODE ( l_type ) == TYPE_CODE_M3_INDIRECT )
      { l_type = TYPE_M3_INDIRECT_TARGET ( l_type ); }
    return l_type;
  } /* m3_direct_type */

/* Strip away any indirect and packed types from a type. */
struct type *
m3_unpacked_direct_type ( struct type * param_type )

  { struct type * l_type;

    if ( param_type == NULL )
      { return NULL; }
    l_type = param_type;
    while ( TRUE )
      { switch  ( TYPE_CODE ( l_type ) )
          { case TYPE_CODE_M3_INDIRECT:
              l_type = TYPE_M3_INDIRECT_TARGET ( l_type );
              break; /* And loop. */
            case TYPE_CODE_M3_PACKED:
              l_type = TYPE_M3_PACKED_TARGET ( l_type );
              break; /* And loop. */
            default:
              return l_type;
          }
      }
  } /* m3_unpacked_direct_type */

/* Convert an opaque type to its revealed type, or identity. */
struct  type *
m3_revealed_type ( struct type * opaque_type )

  { if ( opaque_type != NULL
         && TYPE_CODE ( opaque_type ) == TYPE_CODE_M3_OPAQUE
       )
      { return TYPE_M3_OPAQUE_REVEALED ( opaque_type ); }
    return opaque_type;
  } /* m3_revealed_type */

/* Strip off any indirects, packeds, and opaques. */
struct  type *
m3_revealed_unpacked_direct_type ( struct type * param_type )

  { struct type * l_type;

    if ( param_type == NULL ) { return NULL; }
    l_type = param_type;
    while ( TRUE )
      { switch  ( TYPE_CODE ( l_type ) )
          { case TYPE_CODE_M3_INDIRECT:
              l_type = TYPE_M3_INDIRECT_TARGET ( l_type );
              break; /* And loop. */
            case TYPE_CODE_M3_PACKED:
              l_type = TYPE_M3_PACKED_TARGET ( l_type );
              break; /* And loop. */
            case TYPE_CODE_M3_OPAQUE:
              l_type = TYPE_M3_OPAQUE_REVEALED ( l_type );
              break; /* And loop. */
            default:
              return l_type;
          }
      }
  } /* m3_revealed_unpacked_direct_type */

/* The base type of param_type, if, after stripping indirects and packeds,
   it is an ordinal type. Otherwise, NULL. */
struct type *
m3_ordinal_base_type ( struct type * param_type, BOOL * is_int_or_card )
  { struct type * result_type;

    if ( is_int_or_card != NULL ) { * is_int_or_card = FALSE; }
    result_type = param_type;
    while ( TRUE )
      { switch ( TYPE_CODE ( result_type ) )
          { case TYPE_CODE_M3_INDIRECT :
              result_type = TYPE_M3_INDIRECT_TARGET ( result_type );
              break; /* And loop. */
            case TYPE_CODE_M3_OPAQUE :
              result_type = TYPE_M3_OPAQUE_REVEALED ( result_type );
              break; /* And loop. */
            case TYPE_CODE_M3_PACKED :
              /* The value should still be in packed form, with bitpos and
                 bitsize fields set. */
              result_type = TYPE_M3_PACKED_TARGET ( result_type );
              break; /* And loop. */
            case TYPE_CODE_M3_SUBRANGE :
              result_type = TYPE_M3_SUBRANGE_TARGET ( result_type );
              break; /* And loop. */
            case TYPE_CODE_M3_INTEGER :
            case TYPE_CODE_M3_CARDINAL :
            case TYPE_CODE_M3_LONGINT :
            case TYPE_CODE_M3_LONGCARD :
              if ( is_int_or_card != NULL )
                { * is_int_or_card = TRUE; }
              return result_type;
            case TYPE_CODE_M3_BOOLEAN :
            case TYPE_CODE_M3_CHAR :
            case TYPE_CODE_M3_WIDECHAR :
            case TYPE_CODE_M3_ENUM :
              return result_type;
            default:
              return NULL;
          }
      }
  } /* m3_ordinal_base_type */

/* Return the typecode of the object at inferior address addr.
   PRE: addr is the inferior address of a object with a typecode header,
        i.e., either it's a traced ref or an untraced object type.
*/
LONGEST
m3_typecode_from_inf_object_addr ( CORE_ADDR addr )

{
  LONGEST typecodeword, typecode;
  if (!addr) { return 0; }

  read_memory (addr - (TARGET_PTR_BIT / TARGET_CHAR_BIT),
                      (char *)&typecodeword,
                      TARGET_PTR_BIT / TARGET_CHAR_BIT);

  /* the typecode is in Modula-3 bits 1..21 */
  typecode = m3_extract_ord((char *)&typecodeword, 1, 20, 0);
  /* FIXME: ^Get these bit nos from symbol for RT0.RefHeader.typecode.*/
  return typecode;

} /* m3_typecode_from_inf_object_addr */

/* Return the inferior address of the typecell for the dyanamic (allocated)
   type of the object at inferior address addr.
*/
CORE_ADDR
m3_tc_addr_from_inf_object_addr ( CORE_ADDR addr )

{ LONGEST typecode, n_types;
  CORE_ADDR result, map_ptr, debugee_addr, info_ptr;
  int result_code;

  if (!addr) { return 0; }

  typecode = m3_typecode_from_inf_object_addr ( addr );

  init_m3_constants ();
  if ( m3_compiler_kind ( ) == m3_ck_cm3 )
    { /* Read from RTType.types.cnt, of type CARDINAL. */
      n_types = 0; /* So as to left-extend with zeros. */
      debugee_addr = rttype_types_addr + rttype_infomap_cnt_offset / 8;
      result_code
        = target_read_memory
            ( debugee_addr,
              (char*) &n_types,
              rttype_infomap_cnt_size / 8
            );
      if (result_code != 0)
        { warning
            ( "Unable to read RT variable \"RTType.types.cnt\" from 16_%lx."
            , debugee_addr
            );
          return 0;
        }
      if (typecode >= n_types)
        { warning
            ( "Out-of-range typecode: %d (ref: 16_%lx)"
              "\n   good typecode values are: [0..%d]",
              (int)typecode, addr, (int)(n_types-1)
            );
          return 0;
        }

      /* Read from RTType.types.map, of type ADDRESS. */
      map_ptr = 0; /* So as to left-extend with zeros. */
      debugee_addr = rttype_types_addr + rttype_infomap_map_offset / 8,
      result_code
        = target_read_memory
            ( debugee_addr,
              (char*) &map_ptr, rttype_infomap_map_size / 8
            );
      if (result_code != 0)
        { warning
            ( "Unable to read RT variable \"RTType.types.map\" from 16_%lx."
            , debugee_addr
            );
          return 0;
        }
      if (map_ptr == 0)
        { warning
            ( "No allocated typecell map (typecode: %d, ref: 16_%lx)",
              (int)typecode,
              addr
             );
          return 0;
        }

      /* Read from RTType.types.map^[typecode], of type RTType.InfoPtr.*/
      info_ptr = 0; /* So as to left-extend with zeros. */
      debugee_addr = map_ptr + typecode * TARGET_PTR_BIT / TARGET_CHAR_BIT;
      result_code
        = target_read_memory
            ( debugee_addr,
              (char *) &info_ptr,
              TARGET_PTR_BIT / TARGET_CHAR_BIT
            );
      if (result_code != 0)
        { warning
            ( "Unable to read \"RTType.InfoPtr\" value for typecode %d, "
              "from 16_%lx.",
              (int) typecode,
              debugee_addr
            );
          return 0;
        }
      if (info_ptr == 0)
        { warning
            ( "Typecode %d (ref: 16_%lx) has NIL RTType.InfoPtr value",
              (int) typecode,
              addr
            );
          return 0;
        }

      /* Read from info_ptr^.def, of type RT0.TypeDefn. */
      result = 0; /* So as to left-extend with zeros. */
      debugee_addr = info_ptr + rttype_info_def_offset / 8;
      result_code
        = target_read_memory
            ( debugee_addr,
              (char*) &result,
              TARGET_PTR_BIT / TARGET_CHAR_BIT
            );
      if (result_code != 0)
        { warning
            ( "Unable to read \"RTType.InfoPtr.def\" field for typecode %d, "
              "from 16_%lx.",
              (int) typecode,
              debugee_addr
            );
          return 0;
        }
      if (result == 0)
        { warning
            ( "Typecode %d (ref: 16_%lx) has NIL associated typecell at 16_%lx",
              (int)typecode,
              addr,
              debugee_addr
            );
          return 0;
        }
      return result;
    }
  else if ( m3_compiler_kind ( ) == m3_ck_pm3 )
    { /* Read RT0u.types[typecode], of type RT0.TypeDefn. */
      result = 0; /* So as to left-extend with zeros. */
      debugee_addr
        = rt0u_types_value + typecode * TARGET_PTR_BIT / TARGET_CHAR_BIT;
      result_code
        = target_read_memory
            ( debugee_addr,
              (char *)&result,
              TARGET_PTR_BIT / TARGET_CHAR_BIT
            );
      if (result_code != 0)
        { warning
            ( "Unable to read \"RT0u.types\" element for typecode %d, "
              "from 16_%lx.",
              (int) typecode,
              debugee_addr
            );
          return 0;
        }
      if (result == 0)
        { warning
            ( "Typecode %d (ref: 16_%lx) has NIL associated typecell at 16_%lx",
              (int)typecode,
              addr,
              debugee_addr
            );
        }
      return result;

    }
  else { return 0; }
} /* m3_tc_addr_from_inf_object_addr */

/* Given a type from a Modula-3 program, return its numeric uid. */
LONGEST /* Numeric uid. */
int_uid_from_m3_type ( struct type * t )

{ LONGEST num_uid;
   if ( m3uid_to_num ( TYPE_TAG_NAME ( t ), &num_uid) )
     { return num_uid; }
   return 0;
} /* int_uid_from_m3_type */

/*
 *  Return the address of the runtime typecell that corresponds to type "t".
 */
CORE_ADDR
m3_tc_addr_from_type ( struct type * t )
{
  LONGEST typecode, n_types;
  CORE_ADDR map_ptr, info_ptr, tc_addr;
  LONGEST selfID, num_uid;

  init_m3_constants ( );

  if ( m3_compiler_kind ( ) != m3_ck_cm3 )
    { return 0; }
  /* The following is only used for CM3's Text* modules. */

  if ((TYPE_CODE(t) != TYPE_CODE_M3_OBJECT)
      && (TYPE_CODE(t) != TYPE_CODE_M3_POINTER)) {
    return 0;  /* not an OBJECT or REF type */
  }

  if (!m3uid_to_num (TYPE_TAG_NAME (t), &num_uid)) {
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
    info_ptr = 0;
    read_memory (map_ptr
                      + typecode * TARGET_PTR_BIT / TARGET_CHAR_BIT,
                      (char*)&info_ptr, TARGET_PTR_BIT / TARGET_CHAR_BIT);
    if (!info_ptr) { continue; }

    /* get the typecell pointer */
    tc_addr = 0;
    read_memory (info_ptr + rttype_info_def_offset / 8,
                      (char*)&tc_addr, TARGET_PTR_BIT / TARGET_CHAR_BIT);
    if (!tc_addr) { continue; }

    /* get the type's UID */
    selfID = 0;
    read_memory (tc_addr + rt0_tc_selfID_offset / TARGET_CHAR_BIT,
                      (char*)&selfID, rt0_tc_selfID_size / HOST_CHAR_BIT);
    if (selfID == num_uid) { return tc_addr; }
  }

  return 0;
} /* m3_tc_addr_from_type */

LONGEST
m3_int_uid_from_tc_addr ( CORE_ADDR tc_addr )
{
  LONGEST selfID = 0; /* So as to left-extend with zeros. */

  init_m3_constants ();

  selfID = 0;
  read_memory (tc_addr + rt0_tc_selfID_offset / TARGET_CHAR_BIT,
                      (char *)&selfID, rt0_tc_selfID_size / HOST_CHAR_BIT);

  return selfID;
} /* m3_int_uid_from_tc_addr */

struct type *
m3_type_from_tc ( CORE_ADDR tc_addr )
{
  LONGEST selfID;

  init_m3_constants ();

  selfID = 0;
  read_memory (tc_addr + rt0_tc_selfID_offset / TARGET_CHAR_BIT,
                      (char *)&selfID, rt0_tc_selfID_size / HOST_CHAR_BIT);

  return (m3_resolve_type (m3uid_from_num (selfID)));
} /* m3_type_from_tc */

/* Given a heap reference, find it's actual type.
   PRE: addr is the inferior address of an object with a typecode header,
        i.e., either it's a traced ref or an untraced object type.
*/
struct type *
m3_allocated_type_from_object_addr ( CORE_ADDR addr )
{
  return m3_type_from_tc (m3_tc_addr_from_inf_object_addr (addr));
} /* m3_allocated_type_from_object_addr */


/* return LOOPHOLE (tc_addr, RT0.ObjectTypeDefn).dataOffset */
int
/* TODO: make this return a LONGEST, and propagate this change all over
         m3gdb, wherever bit postions are used? */
m3_dataOffset_from_tc_addr ( CORE_ADDR tc_addr )
{ LONGEST result;
  char kind;

  init_m3_constants ();

  if ( m3_compiler_kind ( ) == m3_ck_cm3 )
    { kind = 0;
      read_memory (tc_addr + rt0_tc_kind_offset / TARGET_CHAR_BIT,
                          (char *)&kind, sizeof(kind));
      /* TODO: Don't assume RT0.Typecell.kind is one byte. */

      if (kind != 2/*RT0.TypeKind.Obj*/) { return 0; }
    }

  result = 0;
  read_memory (tc_addr + rt0_dataOffset_offset / TARGET_CHAR_BIT,
                      (char *)&result, rt0_dataOffset_size / TARGET_CHAR_BIT);
  return result;
} /* m3_dataOffset_from_tc_addr */

int
/* TODO: make this return a LONGEST, and propagate this change all over
         m3gdb, wherever bit postions are used? */
m3_methodOffset_from_tc_addr ( CORE_ADDR tc_addr )
{
  LONGEST result;
  char kind;

  init_m3_constants ();

  if ( m3_compiler_kind ( ) == m3_ck_cm3 )
    { kind = 0;
      read_memory (tc_addr + rt0_tc_kind_offset / TARGET_CHAR_BIT,
                          (char *)&kind, sizeof(kind));
      /* TODO: Don't assume RT0.Typecell.kind is one byte. */

      if (kind != 2 /*RT0.TypeKind.Obj*/)
        { return 0; }
    }

  result = 0;
  read_memory (tc_addr + rt0_methodOffset_offset / TARGET_CHAR_BIT,
                      (char *)&result, rt0_methodOffset_size / TARGET_CHAR_BIT);
  return result;
} /* m3_methodOffset_from_tc_addr */
                
int
m3_dataSize_from_tc_addr ( CORE_ADDR tc_addr )
{
  LONGEST result;
  init_m3_constants ();
  result = 0;
  read_memory (tc_addr + rt0_tc_dataSize_offset / TARGET_CHAR_BIT,
                      (char *)&result, rt0_tc_dataSize_size / TARGET_CHAR_BIT);
  return result;
} /* m3_dataSize_from_tc_addr */
                
CORE_ADDR
m3_super_tc_addr_from_tc_addr ( CORE_ADDR tc_addr )
{ char kind;
  CORE_ADDR  result;

  init_m3_constants ();

  if ( m3_compiler_kind ( ) == m3_ck_cm3 )
    { kind = 0;
      read_memory (tc_addr + rt0_tc_kind_offset / TARGET_CHAR_BIT,
                          (char *)&kind, sizeof(kind));
      /* TODO: Don't assume RT0.Typecell.kind is one byte. */

      if (kind != 2/*RT0.TypeKind.Obj*/) { return 0; }
    }

  result = 0;
  read_memory (tc_addr + rt0_parent_offset / TARGET_CHAR_BIT,
                      (char *)&result, rt0_parent_size / TARGET_CHAR_BIT);
  return result;
} /* m3_super_tc_addr_from_tc_addr */
                
CORE_ADDR
m3_defaultMethods_from_tc_addr ( CORE_ADDR tc_addr )
{ char kind;
  CORE_ADDR result;

  init_m3_constants ();
  if ( m3_compiler_kind ( ) == m3_ck_cm3 )
    { kind = 0;
      read_memory (tc_addr + rt0_tc_kind_offset / TARGET_CHAR_BIT,
                          (char *)&kind, sizeof(kind));
      /* TODO: Don't assume RT0.Typecell.kind is one byte. */

      if (kind != 2/*RT0.TypeKind.Obj*/) { return 0; }
    }

  result = 0;
  read_memory ( tc_addr + rt0_defaultMethods_offset / TARGET_CHAR_BIT,
                (char *)&result, rt0_defaultMethods_size / TARGET_CHAR_BIT);
  return result;
} /* m3_defaultMethods_from_tc_addr */

/*
 * m3_find_rec_field
 * takes: REC_TYPE - a m3 record type
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
 * RETURNs TRUE iff NAME was found in REC_TYPE.
 */
BOOL
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
        if ( type != NULL )
          { * type = TYPE_M3_REC_FIELD_TYPE ( rec_type, i ); }
        return 1;
      }
    }
    if ( bitsize != NULL ) { * bitsize = 0; }
    if ( bitpos != NULL ) { * bitpos = 0; }
    return 0;
  } /* m3_find_rec_field */
                
BOOL
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

BOOL
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
        if ( type != NULL )
          { * type = TYPE_M3_OBJ_METHOD_TYPE ( obj_type, i ); }
        return 1;
      }
    }
    return 0;
  } /* m3_find_obj_method */

BOOL
m3_is_ordinal_type ( struct type * param_type )
  { struct type *unpacked_type;
    enum type_code tc;

    unpacked_type = m3_unpacked_direct_type ( param_type );
    tc = TYPE_CODE ( unpacked_type );

    switch (tc)
      { case TYPE_CODE_M3_SUBRANGE :
        case TYPE_CODE_M3_ENUM :
        case TYPE_CODE_M3_BOOLEAN :
        case TYPE_CODE_M3_CHAR :
        case TYPE_CODE_M3_WIDECHAR :
        case TYPE_CODE_M3_INTEGER :
        case TYPE_CODE_M3_CARDINAL :
        case TYPE_CODE_M3_LONGINT :
        case TYPE_CODE_M3_LONGCARD :
          return TRUE;
        default:
          return FALSE;
      }
  } /* m3_is_ordinal_type */

BOOL
m3_type_is_signed ( struct type * param_type )

  { struct type *unpacked_type;
    enum type_code tc;
    LONGEST lower;

    unpacked_type = m3_unpacked_direct_type ( param_type );
    tc = TYPE_CODE ( unpacked_type );
    switch ( tc )
      { case TYPE_CODE_M3_SUBRANGE :
          lower = TYPE_M3_SUBRANGE_MIN ( unpacked_type );
          return lower < 0;
        case TYPE_CODE_M3_INTEGER :
        case TYPE_CODE_M3_LONGINT :
          return TRUE;
        default:
          return FALSE;
      }
  } /* m3_type_is_signed */

/* For unsigned param_type, f_lower and f_upper wiii actually be unsigned
   values, stored in a LONGEST.  Caller can cast it to a ULONGEST, and
   this is defined by C to do what you want. */
void
m3_ordinal_bounds (
    struct type * param_type, LONGEST * f_lower, LONGEST * f_upper )

  { struct type *unpacked_type;
    enum type_code tc;
    LONGEST lower;
    LONGEST upper;

    unpacked_type = m3_unpacked_direct_type ( param_type );
    tc = TYPE_CODE ( unpacked_type );
    switch ( tc )
      { case TYPE_CODE_M3_SUBRANGE:
          lower = TYPE_M3_SUBRANGE_MIN ( unpacked_type );
          upper = TYPE_M3_SUBRANGE_MAX ( unpacked_type );
          break;
        case TYPE_CODE_M3_ENUM:
          lower = 0;
          upper = TYPE_M3_ENUM_NVALS ( unpacked_type ) - 1;
          break;
        case TYPE_CODE_M3_BOOLEAN:
          lower = 0;
          upper = 1;
          break;
        case TYPE_CODE_M3_CHAR:
          lower = 0;
          upper = 255;
          break;
        case TYPE_CODE_M3_WIDECHAR:
          lower = 0;
          upper = 0xffff;
          break;
        case TYPE_CODE_M3_INTEGER:
          /* assumes a 2's complement machine... */
          lower = (-1L) << (m3_target_integer_bit-1);
          upper = ~ ((-1L) << (m3_target_integer_bit-1));
          break;
        case TYPE_CODE_M3_CARDINAL:
          /* assumes a 2's complement machine... */
          lower = 0;
          upper = ~ ((-1L) << (m3_target_integer_bit-1));
          break;
        case TYPE_CODE_M3_LONGINT:
          /* assumes a 2's complement machine... */
          lower = (-1L) << (m3_target_longint_bit-1);
          upper = ~ ((-1L) << (m3_target_longint_bit-1));
          break;
        case TYPE_CODE_M3_LONGCARD:
          /* assumes a 2's complement machine... */
          lower = 0;
          upper = ~ ((-1L) << (m3_target_longint_bit-1));
          break;
        default:
          error
            (_("m3gdb internal error: bad Modula-3 ordinal type code %d"), tc );
          lower = 0;
          upper = 0;
          break;
      }
    if ( f_lower != NULL )
      { * f_lower = lower; }
    if ( f_upper != NULL )
      { * f_upper = upper; }
  } /* m3_ordinal_bounds */

/* If range_type is an ordinal type, range-check value against it.
   If the check fails, emit an error (which implies noreturn.) */
void
m3_ordinal_range_check (
    LONGEST value, struct type * range_type, char * purpose )

  { ULONGEST uval, ulower, uupper;
    LONGEST slower, supper;

    if ( ! m3_is_ordinal_type ( range_type ) )
      { return; }
    m3_ordinal_bounds ( range_type, & slower, & supper );
    if ( m3_type_is_signed ( range_type ) )
      { if ( ( value < slower) || ( supper < value ) )
          { error (_( "Value is out of range for %s.\n" ), purpose );
            /* FIXME: Put value, lower, and Upper into this message: */
            /* NORETURN */
          }
      }
    else
      { uval = ( ULONGEST ) value;      /* C defines these conversions as */
        ulower = ( ULONGEST ) slower;  /* Modulo the variable size. */
        uupper = ( ULONGEST ) supper;
        if ( ( uval < ulower) || ( uupper < uval ) )
          { error (_( "Value is out of range for %s.\n" ), purpose );
            /* FIXME: Put value, lower, and upper into this message: */
            /* NORETURN */
          }
      }
  } /* m3_ordinal_range_check */

gdb_byte *
m3_read_object_fields_bits ( CORE_ADDR ref )
{
  CORE_ADDR tc_addr;
  int dataSize;
  gdb_byte *buf;

  if (ref == 0) { return 0; }
  tc_addr = m3_tc_addr_from_inf_object_addr (ref);
  dataSize = m3_dataSize_from_tc_addr (tc_addr);
  buf = malloc (dataSize);
  /* FIXME^ Surely this is not the right way to allocate space in gdb.
     rodney.bates@wichita.edu */
  read_memory (ref, buf, dataSize);
  return buf;
}

/* Extract an ordinal bitfield from a gdb-space buffer. */
LONGEST
m3_extract_ord (
  const gdb_byte* valaddr, int bitpos, int bitsize, BOOL sign_extend )

  {
    ULONGEST val;
    ULONGEST valmask;
    int bytesize;
    int lsbcount;

    if ( bitsize == sizeof ( ULONGEST ) * TARGET_CHAR_BIT )
      { bytesize = 8; } /* Special case to circumvent the problem below,
                           for 64-bit operands, until we get this rewritten
                           properly. */
    else
      { bytesize = ( bitsize + TARGET_CHAR_BIT - 2 ) / TARGET_CHAR_BIT + 1; }
    /* FIXME: ^Using extract_unsigned_integer to get the range of bytes that
       cover the packed field into val will fail for packed fields that
       could (depending on their bit alignment) span one more byte than
       is in a ULONGEST.  We should be able to handle this case.  It would
       require combining the functions of this function and
       extract_unsigned_integer. */
    val = extract_unsigned_integer
            ( valaddr + bitpos / TARGET_CHAR_BIT, bytesize );
    if ( BITS_BIG_ENDIAN )
      { lsbcount = sizeof val * HOST_CHAR_BIT - bitpos - bitsize; }
    else
      { lsbcount = bitpos % HOST_CHAR_BIT; }
    val >>= lsbcount;

    /* If the field does not entirely fill a LONGEST, either zero the sign
       bits or sign extend, as requested. */
    if ( bitsize < HOST_CHAR_BIT * ( int ) sizeof ( val ) )
      { valmask = ( ( ( ULONGEST ) 1 ) << bitsize ) - 1;
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
  struct type * val_type = value_type ( val );
  int typebitsize;
  int valbitsize;
  int bitsize;
  BOOL is_signed;

  if ( m3_type_is_signed ( val_type ) )
    { m3_ordinal_bounds ( val_type, & lower, & upper );
      /* ^Here, lower is guaranteed signed. */
      is_signed = ( lower < 0 );
      /* ^If a compiler represented a bit-packed, non-negative subrange of
         a signed type maximally compactly by omitting a sign bit, we must
         not sign-extend.  OTOH, if there is a sign bit, it won't hurt to not
         sign-extend. */
    }
  else { is_signed = FALSE; }
  typebitsize = TYPE_M3_SIZE ( val_type );
  valbitsize = value_bitsize ( val );
  if ( valbitsize == 0 )
    { bitsize = typebitsize; }
  else { bitsize = valbitsize; }
  return m3_extract_ord
           ( value_contents ( val ),
             value_bitpos ( val ),
             bitsize,
             is_signed
           );
} /* m3_value_as_integer */

CORE_ADDR
m3_extract_address (const gdb_byte* valaddr, int bitpos)
{
  return (CORE_ADDR) m3_extract_ord (valaddr, bitpos, TARGET_PTR_BIT, FALSE);
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

    if ( packed_val == NULL )
      { return packed_val; }
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
      { error ( "Packed field has %u bytes, can only unpack %lu.",
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
    /* FIXME: Not TRUE.  struct value contents are in target order. */
    memcpy ( value_contents_raw ( result_val ), source_addr, byte_length );
    set_value_lazy ( result_val, 0 );
#endif
    deprecated_set_value_modifiable ( result_val, 0 );
    return result_val;
  } /* m3_ensure_value_is_unpacked */

int
m3_open_array_dope_align (void)
  { return TARGET_PTR_BIT / TARGET_CHAR_BIT; }

/* Return the gdb-space offset of 'dimension'-th shape component, relative
   to the beginning of open arrray dope. */
int
m3_shape_component_offset ( int dimension )

  { int result;

    result
      = TARGET_PTR_BIT / HOST_CHAR_BIT /* Skip the elements pointer. */
        + dimension * ( m3_target_integer_bit/HOST_CHAR_BIT );
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

/* Store elements address 'val' into the Modula-3 open array
   dope value located in gdb-space at address addr. */
void
m3_set_open_array_elems_addr ( gdb_byte * addr, CORE_ADDR val )

  { store_unsigned_integer
      ( addr, TARGET_PTR_BIT / HOST_CHAR_BIT, ( ULONGEST ) val );
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
      = m3_extract_ord ( addr + target_offset, 0, m3_target_integer_bit, FALSE);
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
      ( addr + target_offset, m3_target_integer_bit / HOST_CHAR_BIT, val );
  } /* m3_set_open_array_shape_component */

/* Fetch the inferior address of the zero-th element of the Modula-3 open array
   whose dope begins at inferior address ref. */
CORE_ADDR
m3_inf_open_array_elems_addr ( CORE_ADDR ref )

{ CORE_ADDR result;
  gdb_byte buf [64]; /* liberal. */

  read_memory ( ref, buf, TARGET_PTR_BIT / TARGET_CHAR_BIT );
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
  gdb_byte buf [64]; /* liberal. */

  target_offset = m3_shape_component_offset ( dimension );
  /* FIXME: ^This would fail if TARGET_CHAR_BIT /= HOST_CHAR_BIT, because
     m3-shape_component_offset returns a gdb-space offset, while we need
     a target offset here. */
  read_memory
    ( ref + target_offset, buf, m3_target_integer_bit/TARGET_CHAR_BIT );
  result = m3_extract_ord (buf, 0, m3_target_integer_bit, FALSE);
  return result;
} /* m3_inf_open_array_shape_component */

/* Fetch the inferior address of the zero-th element of the Modula-3 open array
   whose dope is in gdb-space value array_val. */
CORE_ADDR
m3_value_open_array_elems_addr ( struct value * array_value )

{ CORE_ADDR result;

  result
    = m3_extract_address
        ( ( gdb_byte * ) value_contents ( array_value ),
          value_offset ( array_value )
        );
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
    ( ( gdb_byte * )
        value_contents_raw ( array_value ) + value_offset ( array_value ),
      TARGET_PTR_BIT / HOST_CHAR_BIT,
      ( ULONGEST ) val
    );
} /* m3_set_value_open_array_elems_addr */

/* Return the dimension-th shape component (i.e., the element count for the
   dimension-th dimension), of the Modula-3 open array whose dope is in
   gdb-space value array_val. */
ULONGEST
m3_value_open_array_shape_component (
    struct value * array_value, int dimension )

  { ULONGEST result;
    int target_offset;

    target_offset = m3_shape_component_offset ( dimension );
    result
      = m3_extract_ord
          ( ( gdb_byte * ) value_contents ( array_value ) + target_offset,
            0, m3_target_integer_bit, FALSE
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
      m3_target_integer_bit / HOST_CHAR_BIT,
      val
    );
} /* m3_set_value_open_array_shape_component */

/* For a Modula-3 type, we will construct only one indirect type to it.
   So reuse existing pointer-type mechanism from C/C++, but change
   its type code.
   This could duplicate a compiler-generated type, but that would be
   a lot of code and execution to find. */
struct type *
m3_indirect_type_from_type (struct type *type)
{ struct type * result;

  result = make_pointer_type (type, (struct type **) 0 );
  TYPE_CODE ( result ) = TYPE_CODE_M3_INDIRECT;
  return result;
}

/* If proc_block was discovered earlier to contain an artificial,
   compiler-generated block, return it, otherwise, identity. */
struct block *
m3_proc_body_block ( struct block * proc_block )

{ struct block * result;
  struct symbol * proc_sym;

  if ( proc_block == NULL ) { return NULL; }
  proc_sym = BLOCK_FUNCTION ( proc_block );
  if ( proc_sym == NULL || SYMBOL_LANGUAGE ( proc_sym ) != language_m3 )
    { return proc_block; }
  result = M3_BLOCK_BODY_BLOCK ( proc_block );
  if ( result == NULL ) { return proc_block; }
  return result;
} /* m3_proc_body_block */

/* The symbol table that contains param_block. */
struct symtab *
m3_symtab_containing_block ( struct block * param_block )

  { struct objfile *objfile = NULL;
    struct blockvector *bv;
    struct block *bl;
    struct symtab *st = NULL;

    /* Search the list of symtabs for one whose global block encloses the
       inferior address range of this block.  */
    ALL_SYMTABS (objfile, st)
      {
        bv = BLOCKVECTOR (st);
        bl = BLOCKVECTOR_BLOCK (bv, GLOBAL_BLOCK);
        if (BLOCK_START (bl) <= BLOCK_START (param_block)
            && BLOCK_END (bl) > BLOCK_START (param_block))
          { return st; }
      }
    return NULL;
  } /* m3_symtab_containing_block */

/* Return the symbol of procedure named 'name', that is nested inside
   the block parent_block, found in block_symtab.  name is a "terminated
   string", see m3_term_strings_equal.  parent_block can be a procedure
   block that contains an extra block for the procedure body.
   Return NULL if anything goes wrong. */
struct symbol *
m3_lookup_nested_proc (
    struct block * parent_block,
    struct symtab * block_symtab,
    char * name,
    char * name_to
  )

  { struct blockvector * bv;
    struct block * body_block;
    struct block * trial_block;
    int i;
    struct symbol * trial_sym;

    if ( parent_block == NULL ) { return NULL; }
    body_block = m3_proc_body_block ( parent_block );
    bv = BLOCKVECTOR ( block_symtab );
    for ( i = 0; i < BLOCKVECTOR_NBLOCKS ( bv ); i ++ )
      { trial_block = BLOCKVECTOR_BLOCK ( bv, i );
        trial_sym = BLOCK_FUNCTION ( trial_block );
        if ( trial_sym != NULL
             && TYPE_CODE ( SYMBOL_TYPE ( trial_sym ) ) == TYPE_CODE_FUNC
                /* ^Probably can't fail, but I'm paranoid. */
             && BLOCK_SUPERBLOCK ( trial_block ) == body_block
             && m3_term_strings_equal
                  ( SYMBOL_NATURAL_NAME ( trial_sym ), NULL, name, name_to )
           )
          { return trial_sym; }
      }
    return NULL;

  } /* m3_lookup_nested_proc */

/* Return the 'block_no'-th block that is nested directly inside
   'parent_block', found in 'block_symtab'.  Here, blocks are numbered
   starting from one.  parent_block can be a procedure block that
   contains an extra block for the procedure body. Return NULL if
   anything goes wrong. */
struct block *
m3_find_nested_block (
    struct block * parent_block,
    struct symtab * block_symtab,
    int block_no
  )

  { struct blockvector * bv;
    struct block * body_block;
    struct block * trial_block;
    int i;
    int trial_block_no;

    if ( parent_block == NULL ) { return NULL; }
    body_block = m3_proc_body_block ( parent_block );
    bv = BLOCKVECTOR ( block_symtab );
    trial_block_no = 1;
    for ( i = 0; i < BLOCKVECTOR_NBLOCKS ( bv ); i ++ )
      { trial_block = BLOCKVECTOR_BLOCK ( bv, i );
        if ( BLOCK_FUNCTION ( trial_block ) == NULL
             && BLOCK_SUPERBLOCK ( trial_block ) == body_block
           )
          { /* It's a directly nested block.  See if the right one. */
            if ( trial_block_no == block_no )
              { return trial_block; }
            else { trial_block_no ++; }
          }
      }
    return NULL;

  } /* m3_find_nested_block */

/* Given a block for a procedure, start_block, return the amount to add
   to the frame base address to get the place in the activation record
   where static links point. */
ULONGEST
m3_frame_base_to_sl_target_offset ( struct block * start_block )

  { struct symbol * offset_sym;
    struct block * proc_block;
    struct block * body_block;
    struct symtab * block_symtab;

    if (start_block == NULL) { return 0; }
    proc_block = m3_block_proc_block (start_block);
    body_block = m3_proc_body_block (proc_block);
    offset_sym
      = lookup_block_symbol
          ( body_block,
            m3_nonlocal_var_rec_name, NULL, VAR_DOMAIN
          );
    if (offset_sym == NULL) { return 0; }
    return SYMBOL_VALUE (offset_sym);
  } /* m3_frame_base_to_sl_target_offset */

/* Return the procedure block of the static parent procedure of
   the referring procedure, which child_block is inside* of.
   NULL if child_block is NULL or no such block exists.
*/
static struct block *
m3_static_parent_proc_block ( struct block * child_block )

{ struct block * child_proc_block;
  struct block * parent_block;
  struct block * parent_proc_block;

  child_proc_block = m3_block_proc_block (child_block);
  if (child_proc_block == NULL) { return NULL; } 
  parent_block = BLOCK_SUPERBLOCK (child_proc_block); 
  parent_proc_block = m3_block_proc_block (parent_block);
  return parent_proc_block;
} /* m3_static_parent_proc_block */

/* TODO:  Make this target-dependent value adapt.  It does seems to be very
          deeply built in to gcc (and the standalone backend too) that it is
          always the first thing in the local variables area, but it will
          still vary with stack growth direction. */
static const int static_link_location_offset = - 4;

/* This is the actual SL value that is used in the inferior.
   It may point to a different place in the activation record
   than where the base pointer points.
*/
CORE_ADDR
m3_1st_inf_static_link (struct frame_info *start_frame)

  { long displ;
    struct block * referring_block;
    struct block * referring_proc_block;
    struct block * target_proc_block;
    struct block * target_body_block;
    struct symbol * sl_sym;
    struct value * sl_value;
    CORE_ADDR inf_frame_address;
    CORE_ADDR static_link;
    CORE_ADDR offset;

    inf_frame_address = get_frame_locals_address (start_frame);
    referring_block = get_frame_block (start_frame, NULL);
    referring_proc_block = m3_block_proc_block (referring_block);
    target_proc_block = m3_static_parent_proc_block (referring_proc_block);
    target_body_block = m3_proc_body_block (target_proc_block);
    sl_sym
      = lookup_block_symbol
          (referring_proc_block, m3_static_link_var_name, NULL, VAR_DOMAIN);
    if (sl_sym != NULL)
      { sl_value = read_var_value (sl_sym, start_frame);
        static_link = m3_value_as_address (sl_value);
      }
    else /* An older compiler that always puts SL in a fixed place. */
      { displ = static_link_location_offset;
        static_link
          = read_memory_typed_address /* from gdbcore.h */
              ( inf_frame_address + displ, builtin_type_void_data_ptr );
      }
    offset = m3_frame_base_to_sl_target_offset ( target_body_block );
    return static_link /* - offset */;
  } /* m3_1st_inf_static_link */

/* starting_link is an inferior static link value that points into an
   activation record for referring_block.  Return the next inferior
   static link, for the next outer containing activation record, found
   in the nonlocally referenced record of the block.  Such a static
   link will exist only when compiled by later gcc backends.  Return
   zero if it can't be found.

 */
CORE_ADDR
m3_inf_subseq_static_link (
    struct block * referring_block, CORE_ADDR starting_link)

  { struct block * referring_proc_block;
    struct symbol * nonlocal_var_sym;
    struct type * nonlocal_var_type;
    struct type * field_type;
    CORE_ADDR static_link;
    int bitsize;
    int bitpos;


    referring_proc_block = m3_block_proc_block (referring_block);
    nonlocal_var_sym
      = lookup_block_symbol
          (referring_proc_block, m3_nonlocal_var_rec_name, NULL, VAR_DOMAIN);
    if (nonlocal_var_sym != NULL)
      { nonlocal_var_type = SYMBOL_TYPE (nonlocal_var_sym);
        if ( m3_find_rec_field
               ( nonlocal_var_type, m3_static_link_copy_field_name,
                 & bitsize, & bitpos, & field_type
               )
           )
          { static_link
              = read_memory_typed_address /* from gdbcore.h */
                  ( starting_link + bitpos / TARGET_CHAR_BIT,
                    builtin_type_void_data_ptr
                  );
            return static_link;
          }
      }
    return 0;
  } /* m3_inf_subseq_static_link */

/* Using the selected frame as a starting point, the frame whose corresponding
   inferior activation record contains inf_addr.
*/
static struct frame_info *
m3_frame_containing_inf_addr (
    CORE_ADDR inf_addr, struct frame_info * start_frame )

  { struct frame_info * frame;
    CORE_ADDR inf_static_link;

    frame = start_frame;
    do { frame = get_prev_frame (frame); }
    while
      ( frame != NULL
        && ! m3_address_lies_within_frame_locals ( inf_addr, frame )
        /* Using m3_address_lies_within_frame_locals instead of requiring
           an exact inferior activation record address match adds some
           immunity to gcc's debugger-hostile habit of making static links
           point elsewhere within the activation record than where the
           frame pointer does. */
      );
    return frame;
  } /* m3_frame_containing_inf_addr */

/* Compute and return the inferior static link value that points to the
   activation record of frame, as compiler-generated code would pass it.
   For SRC, PM3, EZM3, and CM3s that use a code generator based on gcc
   not later than 2.7.2, this is just the traditional pointer to the base
   of the activation record.  For code generators starting with 4.3.0, it
   will be the pointer to the record of nonlocally accessed variables and
   formals, located inside the activation record.
*/
CORE_ADDR
m3_static_link_pointing_to_frame (struct frame_info * start_frame)
  { CORE_ADDR inf_ar_base_addr;
    CORE_ADDR displ;
    CORE_ADDR result;
    struct block * proc_block;
    struct symbol * nonlocal_var_sym;
    struct type * nonlocal_var_type;
    struct type * field_type;
    int bitsize;
    int bitpos;

    inf_ar_base_addr = get_frame_locals_address (start_frame);
    proc_block = m3_block_proc_block (get_frame_block (start_frame, NULL));
    nonlocal_var_sym
      = lookup_block_symbol
          (proc_block, m3_nonlocal_var_rec_name, NULL, VAR_DOMAIN);
    if (nonlocal_var_sym != NULL) { displ = SYMBOL_VALUE(nonlocal_var_sym); }
    else { displ = 0; }
    result
      = read_memory_typed_address /* from gdbcore.h */
          ( inf_ar_base_addr + displ, builtin_type_void_data_ptr );
    return result;

  } /* m3_static_link_pointing_to_frame */

BOOL use_static_link = TRUE;

/* Like m3_static_ancestor_frame, except just return NULL
   if can't get a good ancestor frame and static link value.
*/
static struct frame_info *
m3_inner_static_ancestor_frame (
    struct frame_info * start_frame,
    struct block * ancestor_block,
    CORE_ADDR * ancestor_static_link
  )

  { struct block * ancestor_proc_block;
    struct block * l_proc_block;
    struct frame_info * l_frame;
    struct symbol * ancestor_proc_sym;
    char * ancestor_proc_name;
    CORE_ADDR l_link;
    CORE_ADDR start;
    CORE_ADDR end;
    CORE_ADDR calling_pc;

    if (ancestor_static_link != 0) { * ancestor_static_link = 0; }
    if (start_frame == NULL) { return NULL; }
    ancestor_proc_block = m3_block_proc_block (ancestor_block);
    if (ancestor_proc_block == NULL) { return NULL; }
    l_frame = start_frame;
    l_proc_block = m3_block_proc_block (get_frame_block (l_frame, NULL));
    if (l_proc_block == ancestor_proc_block)
      /* We followed zero static links. */
      { /* Get the right static link value for start_frame. */
        if (ancestor_static_link != 0)
          { * ancestor_static_link
              = m3_static_link_pointing_to_frame (start_frame);
          }
        return start_frame;
      }
    if (use_static_link)
      { /* Now follow the first static link. */
        l_link = m3_1st_inf_static_link (start_frame);
        l_frame = m3_frame_containing_inf_addr (l_link, l_frame);
        if (l_frame != NULL)
          { l_proc_block
              = m3_block_proc_block (get_frame_block (l_frame, NULL));
            while (TRUE)
              { if (l_proc_block == ancestor_proc_block)
                  /* Found the link and frame for the right block. */
                  { if (ancestor_static_link != 0)
                      { * ancestor_static_link = l_link; }
                    return l_frame;
                  }
                /* Advance another static link. */
                l_link = m3_inf_subseq_static_link ( l_proc_block, l_link);
                if (l_link != 0)
                  { l_frame = m3_frame_containing_inf_addr (l_link, l_frame);
                    if (l_frame == NULL) { break; }
                    l_proc_block
                      = m3_block_proc_block (get_frame_block (l_frame, NULL));
                  }
                else /* Try to find the static link the standard way. */
                  { l_link = m3_1st_inf_static_link (l_frame);
                    l_frame = m3_frame_containing_inf_addr (l_link, l_frame);
                    if (l_frame == NULL) { break; }
                    l_proc_block
                      = m3_block_proc_block (get_frame_block (l_frame, NULL));
                  }
              }
          }
      }

    /* If we fall through to here, following static links failed.
       Try a cruder method.

       Follow _dynamic_ ancestors, looking for the first frame corresponding
       to ancestor_block.  This is a crude way to locate non-local
       variables/parameters of statically-enclosing procedures of the selected
       frame's procedure.  If all procedures are called as procedure constants,
       this should find the right frame.  If something was called as the
       value of a procedure parameter, it may be wrong.
    */
    start = BLOCK_START ( ancestor_block );
    end = BLOCK_END ( ancestor_block );
    l_frame = start_frame;
    while ( TRUE )
      { if ( l_frame == NULL ) { return NULL; }
        calling_pc = get_frame_address_in_block ( l_frame );
        if ( start <= calling_pc && calling_pc < end )
          /* Found it the approximate way. */
          { ancestor_proc_sym = BLOCK_FUNCTION (ancestor_proc_block);
            if (ancestor_proc_sym != NULL)
              { ancestor_proc_name = SYMBOL_PRINT_NAME (ancestor_proc_sym); }
            else {ancestor_proc_name = ""; }
            warning
              (_("This could be the wrong instance of procedure %s, "
                  "if a proc param with a nested procedure value was called."
                ),
                ancestor_proc_name
              );
            if (ancestor_static_link != 0)
              { l_link = m3_static_link_pointing_to_frame (l_frame);
                * ancestor_static_link = l_link;
              }
            return l_frame;
          }
        l_frame = get_prev_frame ( l_frame );
      } /* while */
    /* That failed too. */
    return NULL;
  } /* m3_inner_static_ancestor_frame */

/* From start_frame, traverse the static chain and return the ancestor
   frame whose corresponding procedure block is the same as the procedure
   block containing ancestor_block.  ancestor_block really should be an
   ancestor of the block corresponding to start_frame.
   Also set *ancestor_static_link to the inferior static link value
   corresponding to this frame.  ancestor_static_link can be NULL.
   Emit an error message if can't get a good ancestor frame and static
   link value.
*/
struct frame_info *
m3_static_ancestor_frame (
    struct frame_info * start_frame,
    struct block * ancestor_block,
    CORE_ADDR * ancestor_static_link
  )

  { struct frame_info * result;
    struct symbol * ancestor_sym;
    struct symbol * ancestor_proc_sym;
    char * ancestor_proc_name;

    result
      = m3_inner_static_ancestor_frame
          (start_frame, ancestor_block, ancestor_static_link);
    if (result == NULL)
      { ancestor_proc_sym
          = BLOCK_FUNCTION (m3_block_proc_block (ancestor_block));
        if (ancestor_proc_sym != NULL)
          { ancestor_proc_name = SYMBOL_PRINT_NAME (ancestor_proc_sym); }
        else {ancestor_proc_name = ""; }
        error
          ( _("Static links do not lead to a valid frame for procedure \"%s\"."),
            ancestor_proc_name
          ); /* NORETURN */
      }
    return result;
  } /* m3_static_ancestor_frame */

/* Inferior alignment for procedure closures. */
int
m3_proc_closure_align (void)
  { return m3_target_integer_bit / TARGET_CHAR_BIT; }
 /* CHECK: ^Is this value always right? */

/* CHECK: Is this value target-dependent? */
const CORE_ADDR closure_mark = - 1L;

struct type *
m3_alloc_closure_type ( struct type * proc_type )

{ struct type * result;

  /* FIXME: There is only one type object builtin_type_m3_proc_closure,
     and we always return it.  We ought to allocate a new one here,
     because we patch its TYPE_TARGET_TYPE.  But then it would need to be
     in the same space as values, and that would further entail duplicating
     proc_type.  Instead, we assume there will never be more than one
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
      );
    store_unsigned_integer
      ( ( gdb_byte * ) & closure -> code_addr,
        TARGET_PTR_BIT / HOST_CHAR_BIT,
        ( ULONGEST ) code_addr
      );
    store_unsigned_integer
      ( ( gdb_byte * ) & closure -> env_ptr,
        TARGET_PTR_BIT / HOST_CHAR_BIT,
        ( ULONGEST ) env_ptr
      );
    return result;
  } /* m3_build_gdb_proc_closure */

/* Is inf_addr the inferior address of a Modula-3 procedure closure? */
BOOL
m3_inf_address_is_proc_closure ( CORE_ADDR inf_addr )

  { struct value * deref_value;
    CORE_ADDR l_closure_mark;

    if ( inf_addr == 0 ) { return FALSE; }
    deref_value = value_at_lazy ( builtin_type_m3_integer, inf_addr );
    l_closure_mark = m3_value_as_integer ( deref_value );
    return l_closure_mark == closure_mark;
  } /* m3_inf_address_is_proc_closure */

/* Is closure_value a Modula-3 procedure closure value?  */
BOOL
m3_value_is_proc_closure ( struct value * closure_value )

  { struct type * closure_type;
    struct m3_proc_closure  * closure;

    if ( closure_value == NULL ) { return FALSE; }
    closure_type = value_type ( closure_value );
    if ( closure_type == NULL ) { return FALSE; }
    if ( TYPE_CODE ( closure_type ) == TYPE_CODE_M3_PROC_CLOSURE )
      { closure = ( struct m3_proc_closure * ) value_contents ( closure_value );
        return closure -> closure_mark == closure_mark;
      }
    else { return FALSE; }
  } /* m3_value_is_proc_closure */

/* valaddr/bitpos point to a gdb-space value of Modula-3 procedure type,
   which in turn could be a pointer to the procedure's code or to a closure.
   Either way, return the code address. */
CORE_ADDR
m3_proc_code_addr ( const gdb_byte * valaddr, int bitpos )

  { CORE_ADDR inf_addr;
    struct value * closure_value;

    inf_addr = m3_extract_address ( valaddr, bitpos );
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

/* valaddr/bitpos point to a gdb-space value of Modula-3 procedure type,
   which in turn could be a pointer to the procedure's code or to a closure.
   Either way, return the enviroment pointer, which will, of course, be
   zero in the former case. */
CORE_ADDR
m3_proc_env_ptr ( const gdb_byte * valaddr, int bitpos )

  { CORE_ADDR inf_addr;
    struct value * closure_value;

    inf_addr = m3_extract_address ( valaddr, bitpos );
    if ( m3_inf_address_is_proc_closure ( inf_addr ) )
      { closure_value
          = value_at_lazy ( builtin_type_m3_proc_closure, inf_addr );
      /* Kinda sleazy, reusing builtin_type_m3_proc_closure, without even
         patching its target type, but I think it will work. */
        return
          ( * ( struct m3_proc_closure * ) value_contents ( closure_value ) )
          . env_ptr;
      }
    else { return 0; }
  } /* m3_proc_env_ptr */

/* Return the first superblock* ancestor of block that is a function block. */
struct block *
m3_block_proc_block ( struct block * blk )

  { struct block * l_block;

    l_block = blk;
    while ( l_block != NULL && BLOCK_FUNCTION ( l_block ) == NULL )
      { l_block = BLOCK_SUPERBLOCK ( l_block ); }
    return l_block;
  } /* m3_block_proc_block */

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

/* Does (inferior) address lie within the range of the parameters and local
   variables of frame?
*/
BOOL
m3_address_lies_within_frame_locals (
    CORE_ADDR address,
    struct frame_info * frame
  )

{ struct block * proc_block;
  struct block * body_block;
  LONGEST min_proc_displ = 0;
  LONGEST max_proc_displ = 0;
  LONGEST min_body_displ = 0;
  LONGEST max_body_displ = 0;
  LONGEST min_displ = 0;
  LONGEST max_displ = 0;
  CORE_ADDR locals_address;

  if ( frame == NULL ) { return FALSE; }
  proc_block = m3_block_proc_block ( get_frame_block ( frame, NULL ) );
  if ( proc_block == NULL ) { return FALSE; }
  body_block = m3_proc_body_block (proc_block);
  m3_block_locals_range (body_block, & min_body_displ, & max_body_displ);
  /* Accounting for displacements in the proc block could be a bit of
     overkill, but I'm paranoid.
  */
  if (proc_block != body_block)
    { m3_block_locals_range (proc_block, & min_proc_displ, & max_proc_displ);}
  if (min_proc_displ < min_body_displ) { min_displ = min_proc_displ; }
  else { min_displ = min_body_displ; }
  if (max_proc_displ > max_body_displ) { max_displ = max_proc_displ; }
  else { max_displ = max_body_displ; }
  locals_address = get_frame_locals_address ( frame );
  if ( address < locals_address + min_displ ) { return FALSE; }
  if ( address > locals_address + max_displ ) { return FALSE; }
  return TRUE;
} /* m3_address_lies_within_block_locals */

/* PRE: string,string_to are a terminated string that is all digits.
   Convert it to an integer.
*/
int
m3_int_value ( char * string, char * string_to )

  { int result;

    result = 0;
    while ( string != string_to && * string != '\0' )
      { result = result * 10 + ( * string - '0' );
        /* God help us if it has nondigits or a value too big. */
        string ++;
      }
    return result;
  } /* m3_int_value */

/* Make a list of canonical linespecs for values.  This will always
   have one element, because there is no user-defined overloading in
   Modula-3, and however the procedure is identified, it will be unique.
*/
void
m3_make_canonical ( struct symtabs_and_lines * values, char * * * canonical )

  { char * * canonical_arr;
    char * canonical_name;
    struct symtab * sym;

    if ( canonical != NULL && values != NULL && values -> nelts == 1 )
      { sym = values -> sals [ 0 ] . symtab;
        if ( sym != NULL )
          { canonical_arr = (char * *) xmalloc (sizeof (char *));
            *canonical = canonical_arr;
            /* Apparently, callers know how big this array is by making
               it parallel to the array values . sals, with values . nelts. */

            canonical_name = xmalloc (strlen (sym ->filename) + 30);
            sprintf
              ( canonical_name,
                "%s:%d",
                sym -> filename,
                values -> sals [ 0 ] . line
              );
            canonical_arr[0] = canonical_name;

          }
      }
  } /* m3_make_canonical */

/* Evaluate the string as an expression.  If any errors occur, ignore them
   and return NULL. */
struct value *
m3_evaluate_string ( char * string )

  { struct expression * expr = NULL;
    struct value * val = NULL;
    volatile struct gdb_exception exception1;
    volatile struct gdb_exception exception2;

    exception1.reason = 0;
    exception2.reason = 0;
    TRY_CATCH (exception1, RETURN_MASK_ALL)
      { expr = parse_expression ( string ); }
    if (exception1.reason != 0)
      { return NULL; }
    TRY_CATCH (exception2, RETURN_MASK_ALL)
      { val = evaluate_expression ( expr ); }
    if (exception2.reason != 0)
      { return NULL; }
    return val;
  } /* m3_evaluate_string */

enum m3_target_typ m3_current_target = TARGET_UNKNOWN;

int m3_target_integer_bit = 32;
/* DANGER, WILL ROBINSON!!!: You cannot use TARGET_INT_BIT (from existing
   gdb code) for the size of Modula-3 INTEGER.  TARGET_INT_BIT is the size
   of a C int, which can be 32, even on a 64-bit machine!!!
*/ 

int m3_target_longint_bit = 64;

/* After m3_current_target has been set or changed, this sets variables
   that can be derived from it. */
void
m3_set_derived_target_info ( void )
  { m3_target_integer_bit = 32;
    m3_target_longint_bit = 64;
    switch ( m3_current_target )
      { case TARGET_NT386 :
          m3_target_longint_bit = 32;
          break;
        case TARGET_64 :
          m3_target_integer_bit = 64;
          break;
        default:
          break;
      }
  TYPE_LENGTH ( builtin_type_m3_integer )
    = m3_target_integer_bit / HOST_CHAR_BIT;
  TYPE_M3_SIZE (builtin_type_m3_integer) = m3_target_integer_bit;

  TYPE_LENGTH ( builtin_type_m3_cardinal )
    = m3_target_integer_bit / HOST_CHAR_BIT;
  TYPE_M3_SIZE (builtin_type_m3_cardinal) = m3_target_integer_bit;

  TYPE_LENGTH ( builtin_type_m3_longint )
    = m3_target_longint_bit / HOST_CHAR_BIT;
  TYPE_M3_SIZE (builtin_type_m3_longint) = m3_target_longint_bit;

  TYPE_LENGTH ( builtin_type_m3_longcard )
    = m3_target_longint_bit / HOST_CHAR_BIT;
  TYPE_M3_SIZE (builtin_type_m3_longcard) = m3_target_longint_bit;

  } /* m3_set_derived_target_info */

enum m3_target_typ
m3_target_pure ( char * name )
  { if (strstr(name, "NT386")) { return TARGET_NT386; }
    /* This is crude but very likely to always be correct.
    There is a 32bit mode on HP-UX/IA64 and ALPHA_NT is 32bit,
    but these are hypothetical and unlikely to materialize
    and could have "32" put in their names, and ALPHA_NT
    would unlikely support m3gdb. */
    if (strstr(name, "32"))    { return TARGET_OTHER; }
    if (strstr(name, "ALPHA")) { return TARGET_64; }
    if (strstr(name, "64" ))   { return TARGET_64; }

    /* FIXME: Positively check for all M3 compiler target names, or at
              least all those in m3middle/src/Target.m3 that cause a call on
              Init64. */
    return TARGET_OTHER;
  } /* m3_target_pure */

/* End of file m3-util.c */
