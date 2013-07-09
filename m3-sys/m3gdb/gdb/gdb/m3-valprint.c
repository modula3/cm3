/* *INDENT-OFF* */

/* Support for printing Modula-3 values for GDB, the GNU debugger.
   Copyright 1994, Digital Equipement Corporation */

#include "m3-bool.h"

#include "defs.h"
#include "gdbtypes.h"
#include "value.h"
#include "gdbcore.h"
#include "c-lang.h"
#include "frame.h"
#include "gdb_string.h"
#include "symtab.h"
#include "valprint.h"

#include "m3-valprint.h"
#include "m3-lang.h"
#include "m3-util.h"

static ULONGEST
m3_trunc_to_bits ( ULONGEST val, int bitct )

  { ULONGEST mask;
    int trunc_bit;

    if ( bitct <= 0 )
      { return val; }
    if ( bitct < TARGET_CHAR_BIT * sizeof ( ULONGEST ) )
      { mask = ( ( ULONGEST ) - 1 ) << bitct;
        return val & ( ~ mask);
      }
    else { return val; }

  } /* m3_trunc_to_bits */

static void
m3_print_scalar (
     const gdb_byte *valaddr,
     int bitpos,
     int bitsize,
     struct ui_file *stream,
     int format,
     int trunc_bits /* If > 0, truncate from left to this many bits. */
  )

  { LONGEST rawval, val;

    rawval = m3_extract_ord ( valaddr, bitpos, bitsize, trunc_bits > FALSE );
    val = m3_trunc_to_bits ( rawval, trunc_bits );
    switch ( format ) {
      case '&':
        if ( val == 0 )
          { fputs_filtered ( "NIL", stream );
            break;
          }
        /* else fall through. */
      case 'x':
          { fputs_filtered ( "16_", stream );
            fputs_filtered
              ( int_string ( val, 16, 0, ( bitsize + 3 ) / 4, 0 ),
                stream
              );
            break;
          }
      case 'o':
        fputs_filtered ( "8_", stream );
        fputs_filtered
          ( int_string
              ( val, 8, 0, ( bitsize + 2 ) / 3, 0 ),
            stream
          );
        break;
      case 'b':
        fputs_filtered ( "16_", stream );
        fputs_filtered ( int_string ( val, 16, 0, 2, 0 ), stream );
        break;
      case 'h':
        fputs_filtered ( "16_", stream );
        fputs_filtered ( int_string ( val, 16, 0, 4, 0 ), stream );
        break;
      case 'w':
        fputs_filtered ( "16_", stream );
        fputs_filtered ( int_string ( val, 16, 0, 8, 0 ), stream );
        break;
      case 'g':
        fputs_filtered ( "16_", stream );
        fputs_filtered ( int_string ( val, 16, 0, 16, 0 ), stream );
        break;
      case 'u':
        fputs_filtered ( int_string ( val, 10, 0, 0, 0 ), stream );
        break;
      case 'd':
      case 0:
        fputs_filtered ( int_string ( val, 10, 1, 0, 0 ), stream );
        break;
      default:
        fputs_filtered ( int_string ( val, 10, 1, 0, 0 ), stream );
        fprintf_filtered
          ( stream,
            "<Format '%c' is invalid for Modula-3 scalar.>",
            (char) format
          );
    }
  } /* m3_print_scalar */

static int
compare (const gdb_byte *valaddr, int bitpos1, int bitpos2, int bitsize)

  { if ((bitpos1 % 8) != 0 || (bitpos2 % 8) != 0 || (bitsize % 8 != 0))
      { /* these comparisons are too hard for now */
        return 0;
      }
    return memcmp (valaddr + bitpos1/8, valaddr + bitpos2/8, bitsize/8) == 0;
  } /* compare */

struct type_info {
  char *        type_name;
  LONGEST       uid;
  struct type * type;
  CORE_ADDR     tc_addr;
  int           wide_mode; /* 0=CHAR,
                              1=WIDECHAR,
                              2=sign of len,
                              3=it's not simple. */
};

struct field_info {
  char *        field_name;
  int           bitpos;     /* Relative to supermost type. */
  int           bitsize;
  struct type * type;
};

static int text_debug_warning_ct = 0;

static BOOL /* success */
get_TEXT_subtype_info (
    char *nm,
    int wide_mode,
    struct type_info *ti,
    LONGEST default_uid
            /* ^If can't find type named nm in symbol tables, use this
               to match types.  This happens if the CM3 libraries are
               compiled without debug info. */
  )

  { struct type *t;
  /* REVIEWME: Failure cases. */
    if ( ti -> tc_addr != 0 || ti -> uid != 0 ) { return TRUE; }
    ti->type_name = nm;
    ti->wide_mode = wide_mode;

    t = find_m3_type_named (nm, /*must_find =*/ FALSE);
    /* If compiled by SRC, PM3, or EZM3, there will be no such type. */
    if ( t )
      { ti -> type = t;
        ti -> tc_addr   = m3_tc_addr_from_type (t);
        ti -> uid = 0; /* TODO: It would be nice to get the real uid from the
                                typecell. See m3_tc_addr_from_type for
                                inspiration.
                       */
        return TRUE;
      }
    else if ( default_uid != 0 )
      { ti -> type = 0;
        ti -> tc_addr = 0;
        ti -> uid = default_uid;
        warning ( "Using defaults for TEXT subtype %s." , nm );
        if ( text_debug_warning_ct ++ == 0)
          { warning
              ( "This probably means libm3core was compiled without debug info." );
          }
        return TRUE;
      }
    else
      { warning ( "No information on TEXT subtype %s." , nm );
        return FALSE;
      }
  } /* get_TEXT_subtype_info */

static BOOL /* success */
get_obj_field_info ( struct type_info * ti,
    char * field_name ,
    struct field_info * fi,
    int default_bitpos,
    int default_bitsize
   )

  { struct type * l_type = ti -> type;
    CORE_ADDR l_tc_addr = ti -> tc_addr;

    fi -> field_name = field_name;
    while ( l_type != 0 && TYPE_CODE ( l_type ) == TYPE_CODE_M3_OBJECT)
      { if ( m3_find_obj_field
               ( l_type, field_name,
                 & fi -> bitsize,
                 & fi -> bitpos,
                 & fi -> type
               )
           )
           { fi-> bitpos
               += TARGET_CHAR_BIT * m3_dataOffset_from_tc_addr ( l_tc_addr );
             return TRUE;
           }

         l_tc_addr = m3_super_tc_addr_from_tc_addr ( l_tc_addr );
         l_type = m3_type_from_tc ( l_tc_addr );
      }
    fi -> type = 0;
    fi -> bitpos = default_bitpos;
    fi -> bitsize = default_bitsize;
    warning
      ( "Using defaults for field %s of type %s" , field_name, ti->type_name );
    if ( text_debug_warning_ct ++ == 0)
      { warning
          ( "This probably means libm3core was compiled without debug info." );
      }
    return FALSE;
  } /* get_obj_field_info */

static BOOL text_info_is_initialized = FALSE;

static struct type_info  Text;
static struct type_info  TextLiteral;
static struct   field_info TextLiteral_cnt;
static struct   field_info TextLiteral_buf;
static struct type_info  Text8;
static struct   field_info Text8_contents;
static struct type_info  Text8Short;
static struct   field_info Text8Short_len;
static struct   field_info Text8Short_contents;
static struct type_info  Text16;
static struct   field_info Text16_contents;
static struct type_info  Text16Short;
static struct   field_info Text16Short_len;
static struct   field_info Text16Short_contents;
static struct type_info  Text8CString;
static struct   field_info Text8CString_str;
static struct type_info  TextSub;
static struct   field_info TextSub_base;
static struct   field_info TextSub_start;
static struct   field_info TextSub_len;
static struct type_info  TextCat;
static struct   field_info TextCat_a;
static struct   field_info TextCat_b;
static struct   field_info TextCat_a_len;
static struct   field_info TextCat_b_len;
static struct   field_info TextCat_a_or_b_wide;

static void
init_m3_text_info (void)

  { /* The hard-wired defaults here for uids of types and of (bit)
       offsets and sizes of fields will allow m3gdb to handle TEXT
       values even if the Modula-3 text library code was compiled
       without debug information.  However, they have to be kept in sync
       with the text libraries and are very fragile if those libraries
       change.  The values here were taken from the CM3 CVS distribution
       on 2006-5-20.

       If the libraries were compiled with debug information, that will
       be used in preference to these defaults, and that will be far
       more robust.
    */
/* TODO: Make the defaults 32/64-bit adapted from, e.g. m3_integer_bit. */ 
    if ( ! text_info_is_initialized )
      { if ( m3_compiler_kind ( ) == m3_ck_cm3 )
          { if ( ! Text . tc_addr && ! Text . uid )
               { get_TEXT_subtype_info ( "Text.T", 2, &Text, 0x7e2f4762 ); }
            if ( ! TextLiteral.tc_addr  && ! TextLiteral . uid )
               { get_TEXT_subtype_info
                   ( "TextLiteral.T", 2, &TextLiteral, 0xc69eecff );
                 get_obj_field_info
                   ( &TextLiteral, "cnt", &TextLiteral_cnt ,32, 32);
                 get_obj_field_info
                   ( &TextLiteral, "buf", &TextLiteral_buf, 64, 0x1ffffef8 );

               }
            if ( ! Text8 . tc_addr && ! Text8 . uid )
               { get_TEXT_subtype_info ( "Text8.T", 0, &Text8, 0x49d4e83f );
                 get_obj_field_info
                   ( &Text8, "contents", &Text8_contents, 32, 32 );
               }
            if ( ! Text8Short . tc_addr  & ! Text8Short . uid )
               { get_TEXT_subtype_info
                   ( "Text8Short.T", 0, &Text8Short, 0xe57bbfe0 );
                 get_obj_field_info
                   ( &Text8Short, "len", &Text8Short_len, 32, 32 );
                 get_obj_field_info
                   ( &Text8Short, "contents", &Text8Short_contents, 64, 128 );
               }
            if ( ! Text16 . tc_addr && ! Text16 . uid )
               { get_TEXT_subtype_info ( "Text16.T", 1, &Text16, 0x89ce6bf1 );
                 get_obj_field_info
                   ( &Text16, "contents", &Text16_contents, 32, 32 );
               }
            if ( ! Text16Short . tc_addr && ! Text16Short . uid )
               { get_TEXT_subtype_info
                   ( "Text16Short.T", 1, &Text16Short, 0xed75fe12 );
                 get_obj_field_info
                   ( &Text16Short, "len", &Text16Short_len, 32, 32 );
                 get_obj_field_info
                   ( &Text16Short, "contents", &Text16Short_contents, 64, 256 );
               }
            if ( ! Text8CString . tc_addr && ! Text8CString . uid )
               { get_TEXT_subtype_info
                   ( "Text8CString.T", 0, &Text8CString, 0xbd2b102e );
                 get_obj_field_info
                   ( &Text8CString, "str", &Text8CString_str, 32, 32 );
               }
            if ( ! TextSub . tc_addr && ! TextSub . uid )
               { get_TEXT_subtype_info
                   ( "TextSub.TT", 3, &TextSub, 0xe2215acf );
                 get_obj_field_info
                   ( &TextSub, "base", &TextSub_base, 32, 32 );
                 get_obj_field_info
                   ( &TextSub, "len", &TextSub_len, 96, 32 );
                 get_obj_field_info
                   ( &TextSub, "start", &TextSub_start, 64, 32 );
               }
            if ( ! TextCat.tc_addr && ! TextCat . uid )
               { get_TEXT_subtype_info ( "TextCat.T", 3, &TextCat, 0x493d82da );
                 get_obj_field_info
                   ( &TextCat, "a", &TextCat_a, 32, 32 );
                 get_obj_field_info
                   ( &TextCat, "a_len", &TextCat_a_len, 96, 32 );
                 get_obj_field_info
                   ( &TextCat, "b", &TextCat_b, 64, 32 );
                 get_obj_field_info
                   ( &TextCat, "b_len", &TextCat_b_len, 128, 32 );
                 get_obj_field_info
                   ( &TextCat, "a_or_b_wide", &TextCat_a_or_b_wide, 160, 8 );
               }
          }
        else if ( m3_compiler_kind ( ) == m3_ck_pm3 )
          { if ( ! Text . tc_addr & ! Text . uid )
              { get_TEXT_subtype_info ( "Text.T", 2, &Text, 0xcd7f2264 ); }
          }
        text_info_is_initialized = TRUE;
      }
  } /* init_m3_text_info */

static BOOL
type_info_matches ( CORE_ADDR tc_addr, struct type_info * info )

  { LONGEST uid;

    if ( info -> tc_addr != 0 && tc_addr == info -> tc_addr ) { return TRUE; }
    uid = m3_int_uid_from_tc_addr ( tc_addr );
    if ( info -> uid != 0 && uid == info -> uid ) { return TRUE; }
    return FALSE;
  } /* type_info_matches */

static struct type * library_type_m3_TextLiteral_buf_char;
static struct type * library_type_m3_TextLiteral_buf_char_index;
static struct type * library_type_m3_TextLiteral_buf_widechar;
static struct type * library_type_m3_TextLiteral_buf_widechar_index;
static BOOL TextLiteral_types_initialized = FALSE;

static void
init_m3_TextLiteral_library_types (void)

  { if ( ! TextLiteral_types_initialized )
      { library_type_m3_TextLiteral_buf_char_index
          = init_type
             ( TYPE_CODE_M3_SUBRANGE,
               m3_target_integer_bit/TARGET_CHAR_BIT /* size */,
               0 /*flags*/,  "<TextLiteral.T.buff.index_char>",
               (struct objfile *) NULL
             );
        TYPE_M3_SIZE ( library_type_m3_TextLiteral_buf_char_index )
          = m3_target_integer_bit;
        TYPE_M3_SUBRANGE_MIN ( library_type_m3_TextLiteral_buf_char_index )
          = 0;
        TYPE_M3_SUBRANGE_MAX ( library_type_m3_TextLiteral_buf_char_index )
          = - 1;
        /* TYPE_M3_SUBRANGE_MAX will be set each time this is used. */

        library_type_m3_TextLiteral_buf_char
          = init_type
             ( TYPE_CODE_M3_ARRAY, 0 /* size */,
               0 /* flags */,  "<TextLiteral.T.buff_char>",
               (struct objfile *) NULL
             );
        TYPE_FIELDS ( library_type_m3_TextLiteral_buf_char )
          = (struct field *) malloc ( 2 * sizeof ( struct field ) );
        TYPE_NFIELDS ( library_type_m3_TextLiteral_buf_char ) = 2;
        LHS_TYPE_M3_ARRAY_INDEX ( library_type_m3_TextLiteral_buf_char )
          = library_type_m3_TextLiteral_buf_char_index;
        LHS_TYPE_M3_ARRAY_ELEM ( library_type_m3_TextLiteral_buf_char )
          = builtin_type_m3_char;

        library_type_m3_TextLiteral_buf_widechar_index
          = init_type
             ( TYPE_CODE_M3_SUBRANGE, 
               m3_target_integer_bit/TARGET_CHAR_BIT /* size */,
               0 /*flags*/,
               "<TextLiteral.T.buff.index_widechar>",
               (struct objfile *) NULL
             );
        TYPE_M3_SIZE ( library_type_m3_TextLiteral_buf_widechar_index )
          = m3_target_integer_bit;
        TYPE_M3_SUBRANGE_MIN ( library_type_m3_TextLiteral_buf_widechar_index )
          = 0;
        TYPE_M3_SUBRANGE_MAX ( library_type_m3_TextLiteral_buf_widechar_index )
          = - 1;
        /* TYPE_M3_SUBRANGE_MAX will be set each time this is used. */

        library_type_m3_TextLiteral_buf_widechar
          = init_type
             ( TYPE_CODE_M3_ARRAY, 0 /* size */,
               0 /* flags */,  "<TextLiteral.T.buff_widechar>",
               (struct objfile *) NULL
             );
        TYPE_FIELDS ( library_type_m3_TextLiteral_buf_widechar )
          = (struct field *) malloc ( 2 * sizeof ( struct field ) );
        TYPE_NFIELDS ( library_type_m3_TextLiteral_buf_char ) = 2;
        LHS_TYPE_M3_ARRAY_INDEX ( library_type_m3_TextLiteral_buf_widechar )
          = library_type_m3_TextLiteral_buf_widechar_index;
        LHS_TYPE_M3_ARRAY_ELEM ( library_type_m3_TextLiteral_buf_widechar )
          = builtin_type_m3_widechar;

        TextLiteral_types_initialized = TRUE;
      }
  } /* init_m3_TextLiteral_library_types */

/* FIXME: The parameters to m3_extract_ord can't be right here. */
/* Fetch the contents of the boolean field described by fi, of the object at
   inferior address ref, of object type described by ti. */
static BOOL
m3_text_field_boolean (
    CORE_ADDR ref, struct type_info * ti, struct field_info * fi )

  { CORE_ADDR addr;
    LONGEST result;
    gdb_byte buf [16];

    addr = ref + fi -> bitpos / TARGET_CHAR_BIT;
    read_memory ( addr, buf, fi -> bitsize / TARGET_CHAR_BIT );
    result = m3_extract_ord ( buf, 0, fi -> bitsize, FALSE);
    return result != 0;
  }  /* m3_text_field_boolean */

/* Fetch the contents of the signed length field described by fi, of the object
   at inferior address ref, of object type described by ti. */
static LONGEST
m3_text_field_length (
    CORE_ADDR ref,
    struct type_info * ti,
    struct field_info * fi,
    BOOL sign_extend
  )

  { CORE_ADDR addr;
    LONGEST result;
    gdb_byte buf [16];

    addr = ref + fi -> bitpos / TARGET_CHAR_BIT;
    read_memory ( addr, buf, fi -> bitsize / TARGET_CHAR_BIT );
    result = m3_extract_ord ( buf, 0, fi -> bitsize, sign_extend);
    return result;
  }  /* m3_text_field_length */

/* Fetch the contents of the pointer field described by fi, of the object at
   inferior address ref, of object type described by ti. */
static CORE_ADDR
m3_text_field_pointer (
    CORE_ADDR ref, struct type_info * ti, struct field_info * fi )

  { CORE_ADDR addr;
    CORE_ADDR result;
    gdb_byte buf [16];

    addr = ref + fi -> bitpos / TARGET_CHAR_BIT;
    read_memory ( addr, buf, fi -> bitsize / TARGET_CHAR_BIT );
    result = m3_extract_address ( buf, 0 );
    return result;
  }  /* m3_text_field_pointer */

/* Return the inferior address of the field described by fi, of the object at
   inferior address ref, of object type described by ti. */
static CORE_ADDR
m3_text_field_addr (
    CORE_ADDR ref,
    struct type_info * ti,
    struct field_info * fi
  )

  { CORE_ADDR result;

    result = ref + fi -> bitpos / TARGET_CHAR_BIT;
    return result;
  }  /* m3_text_field_addr */

/* See whether field 'field_name' of object at inferior address 'ref',
   with inferior typecell address 'tc_addr' is the 'buf' field of an
   object of Modula-3 type TextLiteral.T.  This is a pseudo-object
   that is actually compiled into static data by the compiler.  The
   'buf' field's type shows it as a very long field, but the actual
   value length is encoded in the 'cnt' field.

   If not, return FALSE. If so, return TRUE and construct trumped-up
   type that is right for printing the buf field, having computed its
   element count and element type from the object.  This type is good
   until the next call on this function.  Also supply the 'bitsize',
   'bitpos', and 'field_name', for consistency with other field/method
   lookup functions.
*/
BOOL /* Yes, it's that field of that type. */
m3_check_TextLiteral_buf (
    CORE_ADDR ref,
    CORE_ADDR tc_addr,
    char * field_name,
    int * bitsize,
    int * bitpos, /* of the field, relative to ref */
    struct type ** field_type
  )

  { LONGEST string_length;
    LONGEST byte_length;

    init_m3_text_info ( );
    init_m3_TextLiteral_library_types ( );
    if ( type_info_matches ( tc_addr, &TextLiteral )
         && TextLiteral_buf . field_name != 0
         && strcmp ( field_name, TextLiteral_buf . field_name ) == 0
       ) /* It's the TextLiteral.T.buf field we are looking for. */
      { string_length
          = m3_text_field_length ( ref, &TextLiteral, &TextLiteral_cnt , TRUE);
        if ( string_length < 0 )
          { TYPE_M3_SUBRANGE_MAX
              ( library_type_m3_TextLiteral_buf_widechar_index )
              = - string_length;
            byte_length = ( - string_length + 1 ) * m3_widechar_byte;
            TYPE_LENGTH ( library_type_m3_TextLiteral_buf_widechar )
              = byte_length;
            TYPE_M3_SIZE ( library_type_m3_TextLiteral_buf_widechar )
              = ( - string_length + 1 ) * m3_widechar_bit;
            if ( bitsize != NULL )
              { * bitsize = byte_length * TARGET_CHAR_BIT; }
            if ( bitpos != NULL ) { * bitpos = TextLiteral_buf . bitpos; }
            if ( field_type != NULL )
              { * field_type = library_type_m3_TextLiteral_buf_widechar; }
            return TRUE;
          }
        else
          { TYPE_M3_SUBRANGE_MAX ( library_type_m3_TextLiteral_buf_char_index )
               = string_length;
            byte_length = string_length + 1;
            TYPE_LENGTH ( library_type_m3_TextLiteral_buf_char ) = byte_length;
            TYPE_M3_SIZE ( library_type_m3_TextLiteral_buf_char )
              = ( string_length + 1 ) * TARGET_CHAR_BIT;
            if ( bitsize != NULL )
              { * bitsize = byte_length * TARGET_CHAR_BIT; }
            if ( bitpos != NULL ) { * bitpos = TextLiteral_buf . bitpos; }
            if ( field_type != NULL )
              { * field_type = library_type_m3_TextLiteral_buf_char; }
            return TRUE;
          }
      }
    else { return FALSE; }
  } /* m3_check_TextLiteral_buf */

/* Print a C-style string, getting the data from the inferior, from inferior
   address 'ref'.  Normally stops on a null byte, but will not exceed 'len'
   chars. */
static ULONGEST /* Actual number of chars printed. */
m3_print_cstring (
    CORE_ADDR ref,
    LONGEST start,
    LONGEST len,
    struct ui_file *stream
  )

#define BUFFSIZE 256

  { CORE_ADDR lref;
    ULONGEST charsremaining;
    ULONGEST charsprinted;
    ULONGEST buffct;
    ULONGEST i;
    char c;
    char buff[BUFFSIZE];

    lref = ref + start;
    charsremaining = len - start;
    charsprinted = 0;
    while (charsremaining > 0)
      { /* copy one buffer load into gdb's process space and print it. */
        buffct = charsremaining < BUFFSIZE ? charsremaining : BUFFSIZE;
        read_memory (lref, buff, buffct);
        for (i = 0; i < buffct; i++)
          { c = buff[i];
            if ( c == '\0' )
              { return charsprinted; }
            m3_emit_char ( c, stream, '"');
            charsprinted ++;
          }
        lref += buffct;
        charsremaining -= buffct;
      }
    return charsprinted;
  } /* m3_print_cstring */

/* Does not stop on a null byte, which, in TEXT, can occur.  Inserts escapes,
   but does not add enclosing quotes. */
static void
m3_print_chars (
    CORE_ADDR ref,
    LONGEST start, /* In bytes */
    LONGEST len,   /* In bytes */
    struct ui_file *stream
  )

  { char *str;
    int i;

    /* Copy the string characters into gdb's process space. */
    str = (char *) alloca (len);
    read_memory (ref + start, str, len);
    for (i = 0; i < len; i++) { m3_emit_char ( str[i], stream, '"'); }
  } /* m3_print_chars */

/* Does not stop on a null byte, which, in TEXT, can occur.  Inserts escapes,
   but does not add enclosing quotes. */
static void
m3_print_widechars (
    CORE_ADDR ref,
    LONGEST start, /* In WIDECHARs. */
    LONGEST len,   /* In WIDECHARs. */
    struct ui_file *stream
  )

  { gdb_byte * wstr;
    LONGEST byte_length;
    int i;

    /* Copy the wide characters into gdb's process space. */
    wstr = ( gdb_byte * )  alloca ( len * m3_widechar_byte);
    read_memory
      ( ref + start * m3_widechar_byte,
        wstr, len * m3_widechar_byte
      );
    byte_length = m3_widechar_byte * len;
    for (i = 0; i < byte_length; i+=m3_widechar_byte)
      { m3_emit_widechar
         ( m3_extract_ord ( & wstr [ i ], 0, m3_widechar_bit, FALSE),
           stream,
           '"'
         );
      }
  } /* m3_print_widechars */

/* Special case to print CM3 TextLiteral.T value as an object.  It contains
   a field named "buf", whose type is a very long array, but actually, only
   contains (usually far) fewer CHARs or WIDECHARs, as indicated by the value
   of field "ABS(cnt)".  Also, it's WIDECHARs iff cnt < 0.
   PRE: m3_compiler_kind ( ) == m3_ck_cm3, and init_m3_text_info has been
   called.
*/
static void
m3_print_cm3_TextLiteral_object (
    CORE_ADDR ref,         /* Inferior address of the object. */
    struct ui_file *stream
  )

  { CORE_ADDR chars;
    LONGEST string_length;

    string_length
      = m3_text_field_length ( ref, &TextLiteral, &TextLiteral_cnt , TRUE);
    fputs_filtered ("(*", stream);
    fputs_filtered ( TextLiteral.type_name, stream);
    fputs_filtered ("*) ", stream);
    fputs_filtered ( TextLiteral_cnt.field_name, stream);
    fputs_filtered (" = ", stream);
    fputs_filtered ( int_string ( string_length, 10, 1, 0, 0 ), stream );
    fputs_filtered ("; ", stream);
    fputs_filtered ( TextLiteral_buf.field_name, stream);
    fputs_filtered (" = ", stream);

    chars = m3_text_field_addr ( ref, &TextLiteral, &TextLiteral_buf );
    if (string_length < 0) /* Wide chars. */
      { fputs_filtered ("W\"", stream);
        m3_print_widechars ( chars, 0, - string_length, stream);
      }
    else /* Narrow chars. */
      { fputs_filtered ("\"", stream);
        m3_print_chars ( chars, 0, string_length, stream);
      }
    fputs_filtered ("\" END", stream);
  } /* m3_print_cm3_TextLiteral_object */

/* See if an object is one of the CM3 TEXT subtypes and, if so, print it as a
   string or wide string.
   Call this only if target code is known to be CM3-compiled and
   init_m3_text_info is known to have been called.
   Try to print as a CM3 TEXT value.
*/
static ULONGEST /* - 3 if not a CM3 TEXT or subtype thereof.
                   - 2 if a CM3 TEXT subtype, but improperly formed.
                       (The reason has been printed)
                   - 1 if CM3 string, but longer than length, so was truncated.
                   Else, number of chars (of either size) actually printed. */
m3_print_cm3_text (
    CORE_ADDR ref,      /* Inferior address of the object. */
    CORE_ADDR tc_addr,  /* Inferior address of its Typecell. */
    ULONGEST start,     /* skip this many chars before starting printing. */
    ULONGEST length,    /* print at most this many, (beyond start). */
    BOOL add_quotes,
    struct ui_file *stream
  )

  { ULONGEST result;
    CORE_ADDR chars;
    LONGEST string_length;
    ULONGEST print_length;
    CORE_ADDR open_array;

    if ( type_info_matches ( tc_addr, &Text ) )
      { fputs_filtered ("\"<Not a proper subtype of CM3 TEXT>\"", stream );
        return - 2;
      }
    else if ( type_info_matches ( tc_addr, &TextLiteral ) )
      { string_length
          = m3_text_field_length ( ref, &TextLiteral, &TextLiteral_cnt , TRUE);
        chars = m3_text_field_addr ( ref, &TextLiteral, &TextLiteral_buf );
        if (string_length < 0) /* Wide chars. */
          { string_length = - string_length - start;
            if ( length < string_length)
              { print_length = length; result = - 1; }
            else { print_length = string_length; result = string_length; }
            if (add_quotes)
              { fputs_filtered ("W\"", stream); }
            m3_print_widechars ( chars, start, print_length, stream);
          }
        else /* Narrow chars. */
          { string_length -= start;
            if ( length < string_length)
              { print_length = length; result = - 1; }
            else { print_length = string_length; result = string_length; }
            if (add_quotes)
              { fputs_filtered ("\"", stream); }
            m3_print_chars ( chars, start, print_length, stream);
          }
      }

    else if ( type_info_matches ( tc_addr, &Text8 ) )
      { open_array = m3_text_field_pointer ( ref , &Text8, &Text8_contents );
        string_length
          = m3_inf_open_array_shape_component ( open_array, 0 ) - start - 1;
        chars = m3_inf_open_array_elems_addr ( open_array );
        if ( length < string_length )
          { print_length = length; result = - 1; }
        else { print_length = string_length; result = string_length; }
        if (add_quotes)
          { fputs_filtered ("\"", stream); }
        m3_print_chars ( chars, start, print_length, stream);
      }

    else if ( type_info_matches ( tc_addr, &Text16 ) )
      { open_array = m3_text_field_pointer ( ref , &Text16, &Text16_contents );
        string_length
          = m3_inf_open_array_shape_component ( open_array, 0 ) - start - 1;
        chars = m3_inf_open_array_elems_addr ( open_array );
        if ( length < string_length )
          { print_length = length; result = - 1; }
        else { print_length = string_length; result = string_length; }
        if (add_quotes)
          { fputs_filtered ("W\"", stream); }
        m3_print_widechars ( chars, start, print_length, stream);
      }

    else if ( type_info_matches ( tc_addr, &Text8Short ) )
      { string_length
          = m3_text_field_length ( ref, &Text8Short, &Text8Short_len, FALSE)
            - start;
        if ( string_length
             > Text8Short_contents . bitsize / TARGET_CHAR_BIT - start
           )
          { return - 2; }
        chars = m3_text_field_addr ( ref, &Text8Short, &Text8Short_contents );
        if ( length < string_length )
          { print_length = length; result = - 1; }
        else { print_length = string_length; result = string_length; }
        if (add_quotes)
          { fputs_filtered ("\"", stream); }
        m3_print_chars ( chars, start, print_length, stream);
      }

    else if ( type_info_matches ( tc_addr, &Text16Short ) )
      { string_length
          = m3_text_field_length
              ( ref, &Text16Short, &Text16Short_len, FALSE) - start;
        if ( string_length
             > Text16Short_contents . bitsize / m3_widechar_bit - start
           )
          { return - 2; }
        chars = m3_text_field_addr ( ref, &Text16Short, &Text16Short_contents );
        if ( length < string_length )
          { print_length = length; result = - 1; }
        else { print_length = string_length; result = string_length; }
        if (add_quotes)
          { fputs_filtered ("W\"", stream); }
        m3_print_widechars ( chars, start, print_length, stream);
      }

    else if ( type_info_matches ( tc_addr, &Text8CString ) )
      { chars
          = m3_text_field_pointer ( ref , &Text8CString, &Text8CString_str );
        if (add_quotes)
          { fputs_filtered ("\"", stream); }
        result = m3_print_cstring ( chars, start, length, stream);
      }

    else if ( type_info_matches ( tc_addr, &TextCat ) )
      { CORE_ADDR a_object, b_object;
        CORE_ADDR a_tc_addr, b_tc_addr;
        LONGEST a_printed = 0;
        LONGEST b_printed = 0;
        LONGEST a_len = 0;

        a_object = m3_text_field_pointer ( ref, &TextCat, &TextCat_a );
        b_object = m3_text_field_pointer ( ref, &TextCat, &TextCat_b );
        if (a_object == 0 || b_object == 0 )
           { fputs_filtered ("\"<Ill-formed CM3 TEXT value>\"", stream );
             return - 2;
           }
        if (add_quotes)
          { BOOL a_or_b_wide
              = m3_text_field_boolean ( ref, &TextCat, &TextCat_a_or_b_wide );
            if (a_or_b_wide)
              { fputs_filtered ("W\"", stream); }
            else { fputs_filtered ("\"", stream); }
          }
        if ( length > 0 )
          { a_tc_addr = m3_tc_addr_from_inf_object_addr (a_object);
            a_len = m3_text_field_length
                      ( ref , &TextCat, &TextCat_a_len, FALSE);
            if ( start >= a_len )
              { start -= a_len; }
            else
              { a_printed
                  = m3_print_cm3_text
                      ( a_object, a_tc_addr, start, length, FALSE, stream );
                switch ( a_printed )
                  { case - 2: /* Substring is not good CM3 TEXT subtype. */
                      { fputs_filtered ("<Ill-formed CM3 TEXT value>", stream );
                        return - 2;
                      }
                    case - 1 :  /* String was truncated to length. */
                      { result = - 1;
                        length = 0; /* Cause skip of b-side code. */
                        break;
                      }
                    default:
                      { start = 0;
                        length -= a_printed;
                      }
                  } /* switch */
              } /* else */
          } /* if */
        if ( length > 0 )
          { b_tc_addr = m3_tc_addr_from_inf_object_addr (b_object);
            b_printed
              = m3_print_cm3_text
                  ( b_object, b_tc_addr, start, length, FALSE, stream );
            switch (b_printed )
              { case - 2 : /* Substring is not good CM3 TEXT subtype. */
                  { fputs_filtered ("<Ill-formed CM3 TEXT value>", stream );
                    return - 2;
                  }
                case - 1 :  /* String was truncated. */
                  { result = - 1;
                    break;
                  }
                default : { result = a_printed + b_printed; }
              }
          }
      }

    else if ( type_info_matches ( tc_addr, &TextSub ) )
      { CORE_ADDR base;
        LONGEST substart;
        CORE_ADDR tc_addr;

        base = m3_text_field_pointer ( ref , &TextSub, &TextSub_base );
        if ( base == 0 )
          { fputs_filtered ("\"<Ill-formed CM3 TEXT value>\"", stream );
            return - 2;
          }
        substart
          = m3_text_field_length
              ( ref , &TextSub, &TextSub_start, FALSE) + start;
        string_length
          = m3_text_field_length ( ref , &TextSub, &TextSub_len, FALSE);
        if ( length < string_length)
          { print_length = length; result = - 1; }
        else { print_length = string_length; result = string_length; }
        tc_addr = m3_tc_addr_from_inf_object_addr ( base );
        result
          = m3_print_cm3_text
              ( base, tc_addr, start + substart, print_length, add_quotes,
                stream
              );
        add_quotes = FALSE;
        if ( result == - 1 ) /* We expect this. */
          { result = print_length; }
        /* result values - 2 and - 3 just get passed up. */
      }

    else /* It's not a CM3 TEXT subtype. */ { return - 3; }

    /* Fall through to here if we recognized and printed a CM3 TEXT subtype. */
    if (add_quotes)
      { fputs_filtered ("\"", stream); }
    return result;
  } /* m3_print_cm3_text */

static void
m3_print_plain_object (
     const gdb_byte *valaddr,
     CORE_ADDR tc_addr,
     struct ui_file *stream,
     int format
  )

  { char name [100];
    struct type *this_obj;
    struct symbol *this_obj_sym;
    int i, data_offset;

    if (tc_addr == 0) {
      return; }

    this_obj = m3_type_from_tc (tc_addr);
    if (TYPE_CODE (this_obj) == TYPE_CODE_M3_ROOT
        || TYPE_CODE (this_obj) == TYPE_CODE_M3_TRANSIENT_ROOT
        || TYPE_CODE (this_obj) == TYPE_CODE_M3_UN_ROOT) {
      return; }

    m3_print_plain_object (valaddr, m3_super_tc_addr_from_tc_addr (tc_addr),
                       stream, format);
    data_offset = m3_dataOffset_from_tc_addr (tc_addr);

    /* TODO: Make the printing of types selectable by a user option or format
       letter. */
    if (0 && (TYPE_M3_OBJ_NFIELDS (this_obj) > 0)) {
      fputs_filtered ("(*", stream);
      m3_print_type(this_obj, 0, stream, 0, 0);
      fputs_filtered ("*) ", stream);
    }

    for (i = 0; i < TYPE_M3_OBJ_NFIELDS (this_obj); i++) {
      if (i != 0) {
        fputs_filtered ("; ", stream);
        wrap_here ("    "); }
      fputs_filtered (TYPE_M3_OBJ_FIELD_NAME (this_obj, i), stream);
      fputs_filtered (" = ", stream);
      m3_val_print2 (TYPE_M3_OBJ_FIELD_TYPE (this_obj, i),
                     valaddr,
                     data_offset * TARGET_CHAR_BIT +
                        TYPE_M3_OBJ_FIELD_BITPOS (this_obj, i),
                     TYPE_M3_OBJ_FIELD_BITSIZE (this_obj, i),
                     stream, format, 0, 0); }
    if (i > 0) {
      fputs_filtered ("; ", stream);
      wrap_here ("    "); }
  } /* m3_print_plain_object */

/* TODO: make this queryable and alterable by user command: */
int max_string_length = 2048;

/* This format letter causes CM3 TEXT values to be displayed as objects,
   rather than strings. */
/* TODO: Figure out what letter this really should be. */
int m3_text_object_format = 'k';

static void
m3_print_object (
     const gdb_byte * valaddr,
     int bitpos,
     struct type* type,
     struct ui_file *stream,
     int format
  )

  {
    gdb_byte *bits;
    CORE_ADDR ref;
    CORE_ADDR tc_addr;

    /* Get the inferior pointer value in ref. */
    ref = m3_extract_address (valaddr, bitpos);
    if (ref == 0) {
      fputs_filtered ("NIL", stream);
      return;
    }

    /* Get the inferior runtime type cell address in tc_addr. */
    tc_addr = m3_tc_addr_from_inf_object_addr (ref);
    if (tc_addr == 0) {
      fprintf_filtered
        (stream, _("<Can't find Modula-3 TypeCell for object at "));
      m3_print_scalar (valaddr, bitpos, TARGET_PTR_BIT, stream, '&', 0);
      /* FIXME: valaddr does not point to a place where the 
         negative-displacement GC word is present.
      fprintf_filtered
        ( stream, ", 
          with typecode %d>", 
          m3_typecode_from_inf_object_addr ( valaddr ) 
        );
      */
      return;
    }

    init_m3_text_info ( );

    /* Print the address of the object. */
    fprintf_filtered (stream, "(*");
    m3_print_scalar (valaddr, bitpos, TARGET_PTR_BIT, stream, '&', 0);
    fprintf_filtered (stream, "*) ");

    /* Is it a CM3 TEXT? */
    if ( m3_compiler_kind ( ) == m3_ck_cm3 && format != m3_text_object_format )
      { int actual_printed;

        actual_printed
          = m3_print_cm3_text
              ( ref , tc_addr, 0, max_string_length, TRUE, stream );
        switch ( actual_printed )
          { case - 1: /* CM3 string, was truncated at requested max length. */
              { fputs_filtered ("...", stream);
                return;
              }
            case - 2: /* Ill-formed CM3 string.  Also print it as an object. */
            case - 3: /* Wasn't a CM3 string. */
              { break; }
            default: /* CM3 string, was printed at full length. */
              { return; }
          }
      }

    /* Is it CM3 TextLiteral.T, in object format? */
    if ( format == m3_text_object_format
         && type_info_matches ( tc_addr , &TextLiteral )
       )
      { m3_print_cm3_TextLiteral_object ( ref, stream );
        return;
      }

    /* Fall through to here for normal object. */
    bits = m3_read_object_fields_bits (ref);
    if (bits == 0) /* This shouldn't happen. */
      { fputs_filtered ("NIL", stream);
        return;
      }
    fprintf_filtered (stream, "OBJECT ");
    m3_print_plain_object
      ( bits, tc_addr, stream, format == m3_text_object_format ? 0 : format );
    fputs_filtered (" END", stream);
    free (bits);
  } /* m3_print_object */

extern unsigned int repeat_count_threshold;

/* Print a fully-qualified name of a procedure symbol to stream, with no
   terminating new line. */
static void
m3_print_proc_name ( struct symbol * sym, struct ui_file *stream )

  { const char * name;
    const char * next_comp;
    char *char_p;
    int len;

    name = SYMBOL_LINKAGE_NAME ( sym );
    next_comp = name;
    char_p = strchr ( next_comp, '_' );
    if ( char_p != NULL && char_p != next_comp && char_p[ 1 ] == '_' )
       /* Two underscores. */
      { while ( TRUE )
          { len = char_p - next_comp;
            if ( '0' <= * next_comp && * next_comp <= '9' )
              { fprintf_filtered ( stream, "<block-%.*s>.", len, next_comp ); }
            else
              { fprintf_filtered ( stream, "%.*s.", len, next_comp ); }
            next_comp = char_p + 2;
            char_p = strchr ( next_comp, '_' );
            if ( char_p != NULL && char_p != next_comp && char_p[1] == '_' )
              /* There is another component, terminated by double underscore.
                 Loop. */
              { }
            else
              { char_p = strchr ( next_comp, '.' );
                if ( ( char_p != NULL ) && ( char_p != next_comp ) )
                  /* The last component is terminated by a dot. */
                  { len = char_p - next_comp;
                    fprintf_filtered ( stream, "%.*s", len, next_comp );
                  }
                else { fputs_filtered ( next_comp, stream ); }
                break;
              }
          } /* while */
      }
    else
      { fputs_filtered ( next_comp, stream ); }
  } /* m3_print_proc_name */

/* Print the filename and line number of a procedure symbol, on stream, with
   no terminating new line. */
static void
m3_print_proc_file_and_line (
    struct symbol * proc_sym, struct ui_file *stream )

  { struct symtab_and_line sal;

    sal = find_function_start_sal ( proc_sym, 0 );
    fprintf_filtered
      ( stream, "%s:%d", sal . symtab -> filename, SYMBOL_LINE ( proc_sym ) );
  } /* m3_print_proc_file_and_line */

static void
m3_print_proc_value (
  CORE_ADDR inf_code_addr, CORE_ADDR inf_static_link, struct ui_file *stream )

  { struct symbol * proc_sym;
    struct block * proc_block;
    struct frame_info * proc_frame;

    if ( inf_code_addr == 0 )
      { fputs_filtered ( "NIL", stream ); }
    else
      { proc_sym = find_pc_function ( inf_code_addr );
        if (proc_sym != NULL )
          { fputs_filtered ( "{\"", stream );
            m3_print_proc_name ( proc_sym, stream );
            /* FIXME:  gdb is just full of calls on wrap_here, which takes no
               stream parameter, but instead is hard-coded to use gdb_stdout.
               Everything else takes a stream parameter.  Either it's
               inconsistent, or the stream parameter is unnecessary. */
            wrap_here ( "" );
            fputs_filtered ( "\", Declared at: ", stream );
            m3_print_proc_file_and_line ( proc_sym, stream );
            if ( inf_static_link != 0 )
              { proc_block = SYMBOL_BLOCK_VALUE (proc_sym);
                proc_frame
                  = m3_static_ancestor_frame
                     ( deprecated_safe_get_selected_frame ( ),
                       proc_block,
                       NULL
                     );
                fputs_filtered ( ", ", stream );
                wrap_here ( "" );
                fprintf_filtered
                  ( stream,
                    "Static ancestor=(16_%lx), frame #%d",
                    inf_static_link,
                    frame_relative_level (proc_frame)
                  );
              }
            fputs_filtered ( "}", stream );
          }
      }
  } /* m3_print_proc_value */

int
m3_val_print2 (
     struct type *type,
     const gdb_byte *valaddr,
     int bitpos,
     int bitsize,
     struct ui_file *stream,
     int format,
     int deref_ref,
     int toplevel)
{
  unsigned int i = 0;   /* Number of characters printed */
  unsigned len;
  struct type *elttype;
  unsigned eltlen;
  LONGEST val;
  unsigned char c;
  CORE_ADDR addr;
  int things_printed = 0;
  int reps, j;
  int tmpbitsize;

  switch (TYPE_CODE (type))
    {
    case TYPE_CODE_M3GDB_STRING :
      { LONGEST length = TYPE_LENGTH ( type );
        LONGEST i;

        fputs_filtered ("\"", stream );
        for ( i = 0; i < length; i ++ )
          { m3_emit_char ( valaddr [ i ], stream, '"' ); }
        fputs_filtered ("\"", stream );
        break;
      }

    case TYPE_CODE_M3GDB_WIDESTRING :
      { LONGEST length = TYPE_LENGTH ( type );
        LONGEST i;

        fputs_filtered ("W\"", stream );
        for ( i = 0; i < length; i += m3_widechar_byte )
          { m3_emit_widechar
              ( m3_extract_ord
                  ( & valaddr [ i ], 0, m3_widechar_bit, FALSE),
                stream,
                '"'
              );
          }
        fputs_filtered ("\"", stream );
        break;
      }

    case TYPE_CODE_M3_ARRAY: {
      struct type *index = TYPE_M3_ARRAY_INDEX (type);
      struct type *elt   = TYPE_M3_ARRAY_ELEM (type);
      LONGEST lower, upper, n;

      fputs_filtered ("{", stream);

      m3_ordinal_bounds (index, &lower, &upper);
      n = upper - lower + 1;

      for (i = things_printed = 0; i < n && things_printed < print_max; i++) {
        if (i != 0) {
          fputs_filtered (", ", stream);
          wrap_here ("    "); }

        tmpbitsize = TYPE_M3_SIZE (elt);
        m3_val_print2 (elt, valaddr,
                       bitpos + i * tmpbitsize, tmpbitsize,
                       stream, format, 0, 0);
        things_printed++;
        for (j = i + 1, reps = 1;
             j < n &&  compare (valaddr, bitpos + i * tmpbitsize,
                                bitpos + j * tmpbitsize,
                                tmpbitsize);
             j++, reps++);
        if (reps > repeat_count_threshold) {
          fprintf_filtered (stream, " <repeats %d times>", reps);
          i += reps - 1;
          things_printed += repeat_count_threshold; }}
        
      if (i < n) {
        fputs_filtered ("...", stream); }

      fputs_filtered ("}", stream);
      break; }

    case TYPE_CODE_M3_OPEN_ARRAY: {
      struct type *elt_type = TYPE_M3_OPEN_ARRAY_ELEM (type);
      CORE_ADDR    elems    = m3_extract_address (valaddr, bitpos);
      int          eltsize  = 1;
      int          nelems;
      int          open_dimension_ct;


      if (bitpos % HOST_CHAR_BIT != 0) {
        error (_("Improperly aligned open array dope.")); }

      valaddr += (bitpos / HOST_CHAR_BIT);
      bitpos = 0;

      nelems = m3_extract_ord (valaddr + TARGET_PTR_BIT/HOST_CHAR_BIT,
                                       bitpos, m3_target_integer_bit, FALSE);
      /*FIXME: Redundancy here and a few lines below, fetching the element count. */
      { struct type *e = elt_type;
        const gdb_byte *nelem_addr = valaddr
                            + (TARGET_PTR_BIT + m3_target_integer_bit)/HOST_CHAR_BIT;
        open_dimension_ct = 1;
        while (TYPE_CODE (e) == TYPE_CODE_M3_OPEN_ARRAY)
          { eltsize
              = eltsize * m3_extract_ord
                            (nelem_addr, 0, m3_target_integer_bit, FALSE);
            nelem_addr += m3_target_integer_bit / HOST_CHAR_BIT;
            e = TYPE_M3_OPEN_ARRAY_ELEM (e);
            open_dimension_ct ++;
          }
        eltsize = eltsize * TYPE_M3_SIZE (e); }
      if (eltsize % 8 != 0) {
        error (_("Improperly aligned open array elements")); }
      eltsize = eltsize / 8;

      fputs_filtered ("{", stream);
      if ( TYPE_CODE ( elt_type ) == TYPE_CODE_M3_OPEN_ARRAY )
        { gdb_byte * subval = alloca ( TYPE_LENGTH ( elt_type ) );
          int dimension;
          ULONGEST shape_component;

          for ( dimension = 1; dimension < open_dimension_ct; dimension ++ )
            { shape_component
                = m3_open_array_shape_component ( valaddr, dimension );
              m3_set_open_array_shape_component
                ( subval, dimension - 1, shape_component );
            }
          for ( i = things_printed = 0;
                i < nelems && things_printed < print_max;
                i++
              )
            { if ( i > 0 )
                { fputs_filtered ( ",", stream );
                  wrap_here ( "    " );
                }
              m3_set_open_array_elems_addr ( subval, elems + i * eltsize );
              tmpbitsize = TYPE_M3_SIZE ( elt_type );
              m3_val_print2
                ( elt_type, subval,
                  0, tmpbitsize,
                  stream, format, 0, 0
                );
              things_printed++;
            }
        }
      else {
        gdb_byte *a = alloca (eltsize);
        gdb_byte *previous = alloca (eltsize);
        reps = 0;
        for (i = things_printed = 0;
             i < nelems && things_printed < print_max; i++) {
          read_memory (elems, a, eltsize);
          if (reps > 0 && memcmp (a, previous, eltsize) == 0) {
            reps++; }
          else {
            if (reps > 1) {
              if (reps > repeat_count_threshold) {
                fprintf_filtered (stream, _(" <repeats %d times>"), reps); }
              else {
                for (j = 0; j < reps - 1 && things_printed < print_max; j++) {
                  if (things_printed) {
                    fputs_filtered (",", stream);
                    wrap_here ("    "); }
                  tmpbitsize = TYPE_M3_SIZE (elt_type);
                  m3_val_print2 (elt_type, previous,
                                 0, tmpbitsize,
                                 stream, format, 0, 0);
                  things_printed++; }}
              things_printed += reps; }
            if (things_printed < print_max) {
              if (things_printed) {
                fputs_filtered (",", stream);
                wrap_here ("    "); }
              tmpbitsize = TYPE_M3_SIZE (elt_type);
              m3_val_print2 (elt_type, a,
                             0, tmpbitsize,
                             stream, format, 0, 0);
              things_printed++; }
            reps = 1;
            memcpy (previous, a, eltsize); }
          elems += eltsize; }
        if (reps > 1) {
          if (reps > repeat_count_threshold) {
            fprintf_filtered (stream, _(" <repeats %d times>"), reps);
            things_printed += reps - 1; }
          else {
            for (j = 0; j < reps - 1 && things_printed < print_max; j++) {
              if (things_printed) {
                fputs_filtered (",", stream);
                wrap_here ("    ");  }
              tmpbitsize = TYPE_M3_SIZE (elt_type);
              m3_val_print2 (elt_type, previous,
                             0, tmpbitsize,
                             stream, format, 0, 0);
              things_printed++; }}}}
      if (things_printed < nelems) {
        fputs_filtered ("...", stream); }
      fputs_filtered ("}", stream);
      break; }

    case TYPE_CODE_M3_PACKED: {
      tmpbitsize = TYPE_M3_SIZE (type);
      m3_val_print2 (TYPE_M3_PACKED_TARGET (type), valaddr,
                     bitpos, tmpbitsize,
                     stream, format, 0, 0);
      break; }

    case TYPE_CODE_M3_ENUM: {
      LONGEST lower, upper, val;
      m3_ordinal_bounds (type, &lower, &upper);
      val = m3_extract_ord (valaddr, bitpos, bitsize, (lower < 0));
      if ((lower <= val) && (val <= upper)) {
        fputs_filtered (TYPE_M3_ENUM_VALNAME (type, val), stream);
      } else {
        fprintf_filtered (stream, "<enum value ");
        print_longest (stream, 'd', 1, val);
        fprintf_filtered (stream, " out of range [");
        print_longest (stream, 'd', 1, lower);
        fprintf_filtered (stream, "..");
        print_longest (stream, 'd', 1, upper);
        fprintf_filtered (stream, "]>");
      }
      break; }

    case TYPE_CODE_M3_INDIRECT: {
      CORE_ADDR target_addr = m3_extract_address (valaddr, bitpos);
      struct type *target = TYPE_M3_INDIRECT_TARGET (type);
      int target_size = TYPE_LENGTH (target);
      gdb_byte *target_val = alloca (target_size);

      read_memory (target_addr, target_val, target_size);
      tmpbitsize = TYPE_M3_SIZE (target);
      m3_val_print2 (target, target_val,
                     0, tmpbitsize,
                     stream, format, deref_ref, toplevel);
      break; }

    case TYPE_CODE_M3_METHOD:
    case TYPE_CODE_M3_PROC:
      m3_print_proc_value
        ( m3_proc_code_addr ( valaddr, bitpos ),
          m3_proc_env_ptr ( valaddr, bitpos ),
          stream
        );
      break;

    case TYPE_CODE_M3_RECORD: {
      fputs_filtered ("RECORD ", stream);
      for (i = 0; i < TYPE_M3_REC_NFIELDS (type); i++) {
        if (TYPE_M3_REC_FIELD_NAME (type, i)[0] != '_') {
          fputs_filtered (TYPE_M3_REC_FIELD_NAME (type, i), stream);
          fputs_filtered (" = ", stream);
          tmpbitsize = TYPE_M3_SIZE (TYPE_M3_REC_FIELD_TYPE (type, i));
          m3_val_print2 (TYPE_M3_REC_FIELD_TYPE (type, i), valaddr,
                         bitpos + TYPE_M3_REC_FIELD_BITPOS (type, i),
                         tmpbitsize,
                         stream, format, 0, 0);
          fputs_filtered ("; ", stream);
          wrap_here ("    ");  }}
      fputs_filtered (" END", stream);
      break; }

    case TYPE_CODE_M3_SET: {
      int n = 0;
      int j;
      LONGEST one_longest = 1; 
      LONGEST lower, upper;
      struct type *target = TYPE_M3_SET_TARGET (type);
      int nelems = TYPE_NFIELDS (target);
      int en = (TYPE_CODE (target) == TYPE_CODE_M3_ENUM);
      int ch = (TYPE_CODE (target) == TYPE_CODE_M3_CHAR);
      int chs = (TYPE_CODE (target) == TYPE_CODE_M3_SUBRANGE)
        && (TYPE_CODE (TYPE_M3_SET_TARGET (target)) == TYPE_CODE_M3_CHAR);

      m3_ordinal_bounds (target, &lower, &upper);
      fputs_filtered ("{", stream);

      for (i = 0; i < TYPE_LENGTH (type) / sizeof (long); i++) {
        val = m3_extract_ord (valaddr, bitpos, m3_target_integer_bit, FALSE);
        for (j = 0; j < m3_target_integer_bit; j++) {
          LONGEST ord = i * m3_target_integer_bit + j + lower;
          if ((val & one_longest << j) && (ord <= upper)) {
            if (n > 0) {
              fputs_filtered (", ", stream); }
            if (en) {
              fputs_filtered (TYPE_FIELD_NAME (target, ord), stream); }
            else if (ch) {
              fprintf_filtered (stream, "'%c'", (char)ord); }
            else if (chs) {
              fprintf_filtered (stream, "'%c'", (char)ord); }
            else {
              print_longest (stream, 'd', 1, ord); }
            n++; }}
        valaddr += sizeof (long); }

      fputs_filtered ("}", stream);

      break; }

    case TYPE_CODE_M3_SUBRANGE :
      { LONGEST lower, upper, val;
        struct type *target = TYPE_M3_SUBRANGE_TARGET (type);

        m3_ordinal_bounds ( type, &lower, &upper );
        val = m3_extract_ord ( valaddr, bitpos, bitsize, ( lower < 0 ) );
        if ( ( val < lower ) || ( upper < val ) )
          { fprintf_filtered(stream, "<subrange value ");
            print_longest (stream, 'd', 1, val);
            fprintf_filtered(stream, " out of range [");
            print_longest (stream, 'd', 1, lower);
            fprintf_filtered(stream, "..");
            print_longest (stream, 'd', 1, upper);
            fprintf_filtered(stream, "]>");
          }
        else if ( TYPE_CODE ( target ) == TYPE_CODE_M3_ENUM )
          { fputs_filtered ( TYPE_M3_ENUM_VALNAME ( target, val ), stream ); }
        else if ( 0 <= lower )
          { m3_print_scalar ( valaddr, bitpos, bitsize, stream, format, 0 ); }
        else if ( TYPE_CODE ( target ) == TYPE_CODE_M3_LONGINT )
          { m3_print_scalar
              ( valaddr, bitpos, bitsize, stream, format,
                m3_target_longint_bit
              );
          }
        else
          { m3_print_scalar
              ( valaddr, bitpos, bitsize, stream, format, 
                m3_target_integer_bit 
              );
          }
        break;
      }

    case TYPE_CODE_M3_ADDRESS :
      m3_print_scalar (valaddr, bitpos, bitsize, stream,
                       format ? format : '&', 0);
      break;

    case TYPE_CODE_M3_BOOLEAN :
      if (m3_extract_ord (valaddr, bitpos, bitsize, FALSE)) {
        fputs_filtered ("TRUE", stream); }
      else {
        fputs_filtered ("FALSE", stream); }
      break;

    case TYPE_CODE_M3_CHAR :
      m3_print_char_lit
       ( (int)m3_extract_ord (valaddr, bitpos, TARGET_CHAR_BIT, FALSE), stream);
      break;

    case TYPE_CODE_M3_WIDECHAR :
      m3_print_widechar_lit
       ( (int)m3_extract_ord (valaddr, bitpos, bitsize , FALSE),
         stream
       );
      break;

    case TYPE_CODE_M3_INTEGER :
      m3_print_scalar
        ( valaddr, bitpos, bitsize, stream, format, m3_target_integer_bit );
      break;

    case TYPE_CODE_M3_LONGINT :
      m3_print_scalar
        ( valaddr, bitpos, bitsize, stream, format, m3_target_longint_bit );
      break;

    case TYPE_CODE_M3_CARDINAL :
    case TYPE_CODE_M3_LONGCARD :
      m3_print_scalar (valaddr, bitpos, bitsize, stream, format, 0);
      break;

    case TYPE_CODE_M3_NULL :
      fputs_filtered ("NIL", stream);
      break;

    case TYPE_CODE_M3_VOID :
      fputs_filtered ("<void>", stream);
      break;

    case TYPE_CODE_M3_ROOT :
    case TYPE_CODE_M3_TRANSIENT_ROOT :
    case TYPE_CODE_M3_UN_ROOT :
    case TYPE_CODE_M3_OBJECT : {
      if ( deref_ref && ( format == 0 || format == m3_text_object_format ) ) {
        m3_print_object (valaddr, bitpos, type, stream, format); }
      else {
        m3_print_scalar (valaddr, bitpos, bitsize, stream,
                         format ? format : '&', 0); }
      break; }

    case TYPE_CODE_M3_REFANY :
    case TYPE_CODE_M3_TRANSIENT_REFANY : {
      m3_print_scalar (valaddr, bitpos, bitsize, stream,
                       format ? format : '&', 0);
      break; }

    case TYPE_CODE_M3_POINTER : {
      int down_format;
      CORE_ADDR text_value;
      CORE_ADDR chars_addr;
      int nelems;

      if ( format == 0 || format == m3_text_object_format )
        { down_format = '&'; }
      else { down_format = format; }
      if ( m3_compiler_kind ( ) == m3_ck_cm3
           || m3_extract_address ( valaddr, bitpos) == 0
/*            ^If the inferior process has not been created yet,
               init_m3_text_info, called below, won't be able to get the needed
               types and will ungracefully display warnings right in the middle
               of the value.  This condition will at least prevent this from
               happening unnecessarily when the text is NIL. */
         )
        { m3_print_scalar
            ( valaddr, bitpos, bitsize, stream, down_format , 0 );
        }
      else /* PM3, and non-NIL */
        { init_m3_text_info ( );
          if ( format != m3_text_object_format
               && ( ( Text . type != 0 && Text . type == type )
                    || ( Text . uid != 0
                         && Text . uid == int_uid_from_m3_type ( type )
                       )
                  )
             )
            /* PM3 TEXT value. */
            { text_value = m3_extract_address (valaddr, bitpos);
              /* text_value is the pointer to the open array. */
              if (text_value == 0)
                { fputs_filtered ("NIL", stream); }
              else
                { fputs_filtered ("(*", stream);
                  m3_print_scalar ( valaddr, bitpos, bitsize, stream, '&', 0 );
                  fputs_filtered ("*)\"", stream);
                  nelems
                    = m3_inf_open_array_shape_component ( text_value, 0 ) - 1;
                  chars_addr = m3_inf_open_array_elems_addr ( text_value );
                  /* To see the bytes as an array,
                     the user can dereference the TEXT value. */
                  m3_print_chars ( chars_addr, 0, nelems, stream);
                  fputs_filtered ("\"", stream);
                }
            }
          else /* Not TEXT. */
            { m3_print_scalar
                ( valaddr, bitpos, bitsize, stream, down_format, 0 );
            }
        }
      break; }

    case TYPE_CODE_FLT: {
      if (format) {
        m3_print_scalar (valaddr, bitpos, bitsize, stream, format, 0); }
      else {
        if (bitpos % 8 != 0) {
          error ("improperly aligned floating point value"); }
        print_floating (valaddr + bitpos / 8, type, stream); }
      break; }

    case TYPE_CODE_STRING:
      fputs_filtered ("\"", stream);
      /* FIXME:  If this can happen at all in language M3, then is it
         really a null-terminated string, as it is treated here?  And it
         should have the escapes printed properly.  Note that
         m3_print_cstring isn't right, because it takes an inferior
         address, whereas here, we have a gdb-space address in valaddr. */
      fputs_filtered ( valaddr, stream );
      fputs_filtered ("\"", stream);
      break;

    case TYPE_CODE_FUNC:
      fputs_filtered( "<procedure>", stream );
      break;
    default:
     return 1; }

  gdb_flush (stream);
  return (0);
} /* m3_val_print2 */

/* Print data of type TYPE located at VALADDR (within the GDB process space),
   which came from the inferior at address ADDRESS, onto stdio stream STREAM,
   according to FORMAT (a letter or 0 for natural format).  The data at VALADDR
   is in target byte order.

   If the data are a string pointer, returns the number of string characters
   printed.

   If DEREF_REF is nonzero, then dereference references, otherwise just print
   them like pointers.

   The PRETTY parameter controls prettyprinting.  */
int
m3_val_print (
     struct type *type,
     const gdb_byte *valaddr,
     int embedded_offset,
     CORE_ADDR address,
     struct ui_file *stream,
     int format,
     int deref_ref,
     int recurse,
     enum val_prettyprint pretty)

{ int tmpbitsize = TYPE_M3_SIZE(type);

  if ( TYPE_CODE ( type ) == TYPE_CODE_FUNC )
    /* We have to do this here, because we need address, which is not
       passed to m3_val_print_2. */
    { m3_print_proc_value ( address, 0, stream ); }
  else if (m3_val_print2 (type, valaddr, 0, tmpbitsize,
                     stream, format, deref_ref, 1)) {
    /* like the value of registers */
    return c_val_print (type, valaddr, embedded_offset, address, stream,
                        format, deref_ref, recurse, pretty);
  }
  return 0;
}

/* End of file m3-valprint.c */
