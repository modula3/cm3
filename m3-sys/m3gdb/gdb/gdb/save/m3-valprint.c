/* *INDENT-OFF* */ 

/* Support for printing Modula-3 values for GDB, the GNU debugger.
   Copyright 1994, Digitial Equipement Corporation */

#include "defs.h"
#include "symtab.h"
#include "gdbtypes.h"
#include "expression.h"
#include "value.h"
#include "demangle.h"
#include "valprint.h"
#include "language.h"
#include "target.h"
#include "gdbcore.h"
#include "c-lang.h"
#include "m3-lang.h"
#include "gdb_string.h"

#include <stdbool.h>

/* FIXME: define TARGET_WIDECHAR_BIT properly. */
#define TARGET_WIDECHAR_BIT ( ( TARGET_CHAR_BIT ) * 2 ) 

static void
m3_store_bit_field ( ULONGEST val, int bitpos, int bitsize ) 

  { gdb_byte buff [ 16 /* generous. */ ]; 

     
  } /* m3_store_bit_field */

LONGEST
m3_extract_ord (
  const gdb_byte* valaddr, int bitpos, int bitsize, int sign_extend )

  {
    ULONGEST val;
    ULONGEST valmask;
    int lsbcount;

    val = extract_unsigned_integer 
            ( valaddr + bitpos / HOST_CHAR_BIT, 
              ( bitsize + bitpos ) / HOST_CHAR_BIT + 1 
            );
    if ( BITS_BIG_ENDIAN )
      lsbcount = sizeof val * HOST_CHAR_BIT - bitpos % HOST_CHAR_BIT - bitsize ;
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

CORE_ADDR
m3_value_as_address (struct value * val)
{
  /* REVIEWME: Do we need the other cases found in value_as_address? */ 
  return m3_extract_address ( ( CORE_ADDR * ) value_contents ( val ), 0 );
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
    if ( byte_length > sizeof ( value ) ) 
      { error ( "Packed field has %d bytes, can only unpack %d.", 
                byte_length,
                sizeof ( value )
              ); 
      } 
    result_val = allocate_value ( val_type ); 
    value 
      = m3_extract_ord 
          ( value_contents ( packed_val ), 
            bitpos,
            bitsize, 
            m3_type_is_signed ( val_type ) 
          ); 
    if ( BITS_BIG_ENDIAN )
      { source_addr = & value + sizeof ( value ) - byte_length; }  
    else { source_addr = & value; } 
    /* Host-to-host, no byte order problem: */ 
    memcpy ( value_contents_raw ( result_val ), source_addr, byte_length );
    set_value_lazy ( result_val, 0 ); 
    deprecated_set_value_modifiable ( result_val, 0 ); 
    return result_val; 
  } /* m3_ensure_value_is_unpacked */ 

static int 
shape_component_offset ( int dimension ) 

  { int result; 

    result 
      = TARGET_PTR_BIT / HOST_CHAR_BIT /* Skip the elements pointer. */
        + dimension * ( TARGET_LONG_BIT/TARGET_CHAR_BIT );
    return result; 
  } /* shape_component_offset */ 

/* Return the address of the zero-th element of the Modula-3 open array 
   whose dope is in gdb-space at address addr. */ 
static CORE_ADDR 
open_array_elems_addr ( const gdb_byte * addr ) 

  { CORE_ADDR result; 

    result = m3_extract_address ( addr, 0 ); 
    return result; 
  } /*open_array_elems_addr */ 

/* Store elements address val into the Modula-3 open array 
   dope value located in gdb-space at address addr. */ 
static void
set_open_array_elems_addr ( gdb_byte * addr, CORE_ADDR val ) 

  { store_unsigned_integer 
      ( addr, TARGET_PTR_BIT / HOST_CHAR_BIT, ( ULONGEST ) val ) ; 
  } /* set_open_array_elems_addr */ 

/* Return the dimension-th shape component (i.e., the element count for the
   dimension-th dimension), of the Modula-3 open array whose dope is in 
   gdb-space at address. */ 
static ULONGEST 
open_array_shape_component ( const gdb_byte * addr , int dimension )

  { ULONGEST result; 
    int target_offset; 

    target_offset = shape_component_offset ( dimension );
    result 
      = m3_extract_ord ( addr + target_offset, 0, TARGET_LONG_BIT, 0 );
    return result; 
  } /* open_array_shape_component */ 

/* Store shape component val into the dimension-th slot of Modula-3 
   open array dope located in gdb space at address addr. */ 
static void
set_open_array_shape_component ( 
    gdb_byte * addr, 
    int dimension, 
    ULONGEST val
  ) 

  { int target_offset; 

    target_offset = shape_component_offset ( dimension );
    store_unsigned_integer 
      ( addr + target_offset, TARGET_LONG_BIT / HOST_CHAR_BIT, val ); 
  } /* set_open_array_shape_component */ 

/* Fetch the inferior address of the zero-th element of the Modula-3 open array 
   whose dope begins at inferior address ref. */ 
static CORE_ADDR 
m3_inf_open_array_elems_addr ( CORE_ADDR ref ) 

{ CORE_ADDR result; 
  gdb_byte buf [16] ; /* liberal. */ 

  read_memory ( ref, buf, TARGET_PTR_BIT );  
  result = open_array_elems_addr ( buf ); 
  return result; 
} /* m3_inf_open_array_elems_addr */ 

/* TODO: Rationalize the following to maybe use [set_]open_array_shape_component,
   deciding which is responsible for computing the dimension offset. */ 

/* Return the dimension-th shape component (i.e., the element count for the
   dimension-th dimension), of the Modula-3 open array whose dope begins at 
   inferior address ref. */ 
static ULONGEST 
m3_inf_open_array_shape_component ( CORE_ADDR ref, int dimension ) 

{ ULONGEST result; 
  int target_offset; 
  gdb_byte buf [16]; /* liberal. */ 

  target_offset = shape_component_offset ( dimension ); 
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

  target_offset = shape_component_offset ( dimension );
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

  target_offset = shape_component_offset ( dimension );
  store_unsigned_integer 
    ( ( gdb_byte * ) value_contents_raw ( array_value ) + target_offset, 
      TARGET_LONG_BIT / HOST_CHAR_BIT, 
      val
    ) ; 
} /* m3_set_value_open_array_shape_component */ 

static ULONGEST 
m3_truncate_to_target_longest ( ULONGEST val ) 

{ ULONGEST mask ; 

  if ( TARGET_INT_BIT / TARGET_CHAR_BIT < sizeof ( LONGEST ) ) 
    { mask = ( ( ULONGEST ) - 1 ) >> TARGET_INT_BIT; 
      return val & mask; 
    } 
  else { return val; }

} /* m3_truncate_to_target_longest */

static void 
m3_print_scalar (
     const gdb_byte *valaddr,
     int bitpos,
     int bitsize,
     struct ui_file *stream,
     int format,
     int sign_extend)
{
  LONGEST val = m3_extract_ord (valaddr, bitpos, bitsize, sign_extend);

  switch (format) {
    case 'x':
      if (val == 0)
	{ fputs_filtered ( "NIL", stream ); 
          break; 
        } 
      else
        { fputs_filtered ( "16_", stream ); 
          fputs_filtered 
            ( int_string 
                ( m3_truncate_to_target_longest ( val ), 
                  16, 0, bitsize/TARGET_CHAR_BIT*2, 0 ), stream 
                );
          break;
        } 
    case 'o': 
      fputs_filtered ( "8_", stream ); 
      fputs_filtered 
        ( int_string 
            ( m3_truncate_to_target_longest ( val ), 
              8, 0, bitsize/TARGET_CHAR_BIT*2, 0 ), stream 
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
      fprintf_filtered 
        ( stream, "Format '%c' is invalid for Modula-3 scalar.", (char) format ); 
  } 
}

gdb_byte *
m3_read_object_fields_bits (CORE_ADDR ref)
{
  CORE_ADDR tc_addr;
  int dataSize;
  gdb_byte *bits;

  if (ref == 0) { return 0; }
  tc_addr = find_m3_heap_tc_addr (ref);
  dataSize = m3_tc_address_to_dataSize (tc_addr);
  bits = malloc (dataSize);
  /* FIXME^ Surely this is not the right way to allocate space in gdb. 
     rodney.bates@wichita.edu */
  read_memory (ref, bits, dataSize);
  return bits;
}

static int 
compare (const gdb_byte *valaddr, int bitpos1, int bitpos2, int bitsize)
{
  if ((bitpos1 % 8) != 0 || (bitpos2 % 8) != 0 || (bitsize % 8 != 0)) {
    /* this comparaisons are too hard for now */
    return 0; }
  return memcmp (valaddr + bitpos1/8, valaddr + bitpos2/8, bitsize/8) == 0;
}

struct type_info {
  char *        type_name;
  int           uid; 
  struct type * type; 
  CORE_ADDR     tc_addr;
  int           wide_mode; /* 0=CHAR, 1=WIDECHAR, 2=sign of len, 3=it's not simple. */
};

struct field_info { 
  char *        field_name;
  int           bitpos;     /* Relative to supermost type. */ 
  int           bitsize;
  struct type * type; 
}; 

static int text_debug__warning_ct = 0; 

static bool /* success */ 
get_type_info 
  ( char *nm, 
    int wide_mode, 
    struct type_info *ti, 
    int default_uid /* If can't find type named nm in symbol tables, use this
                       to match types.  This happens if the CM3 libraries ard
                       compiled without debug info. */ 
  )

{ struct type *t;
/* REVIEWME: Failure cases. */
  if ( ti -> tc_addr != 0 || ti -> uid != 0 ) { return true; }
  ti->type_name = nm;
  ti->wide_mode = wide_mode;

  t = find_m3_type_named (nm, 0);
  /* If compiled by pm3 or earlier, there will be no such type. */ 
  if ( t ) 
    { ti -> type = t; 
      ti -> tc_addr   = find_tc_from_m3_type (t);
      ti -> uid = 0; /* TODO: It would be nice to get the real uid from the 
                              typecell. See find_tc_from_m3_type for inspiration. */ 
      return true; 
    } 
  else if ( default_uid != 0 ) 
    { ti -> type = 0; 
      ti -> tc_addr = 0; 
      ti -> uid = default_uid; 
      warning ( "Using defaults for TEXT subtype %s." , nm ); 
      if ( text_debug__warning_ct ++ == 0) 
        { warning 
            ( "This probably means libm3core was compiled without debug info." ); 
        } 
      return true; 
    } 
  else 
    { warning ( "No information on TEXT subtype %s." , nm );  
      return false; 
    } 
} /* get_type_info */

static bool /* success */ 
get_obj_field_info  ( struct type_info * ti, 
    char * field_name , 
    struct field_info * fi, 
    int default_bitpos,
    int default_bitsize
   ) 
 
{ struct type * l_type = ti -> type; 
  CORE_ADDR l_tc_addr = ti -> tc_addr; 

  fi -> field_name = field_name; 
  while ( l_type != 0 && TYPE_CODE ( l_type ) == TYPE_CODE_M3_OBJECT) 
    { if ( find_m3_obj_field 
             ( l_type, field_name, 
               & fi -> bitsize, 
               & fi -> bitpos, 
               & fi -> type 
             ) 
         )
         { fi-> bitpos
             += TARGET_CHAR_BIT * m3_tc_address_to_dataOffset ( l_tc_addr ); 
           return true; 
         } 

       l_tc_addr = m3_tc_addr_to_super_tc_addr ( l_tc_addr ); 
       l_type = find_m3_type_from_tc ( l_tc_addr ); 
    } 
  fi -> type = 0;
  fi -> bitpos = default_bitpos;
  fi -> bitsize = default_bitsize; 
  warning 
    ( "Using defaults for field %s of type %s" , field_name, ti->type_name );
  if ( text_debug__warning_ct ++ == 0) 
    { warning 
        ( "This probably means libm3core was compiled without debug info." ); 
    } 
  return false;
} /* get_obj_field_info */ 

static bool text_info_is_initialized = false; 

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
init_m3_text_info ( )
{ /* The hard-wired defaults here for uids of types and of (bit) offsets and sizes 
     of fields will allow m3gdb to handle TEXT values even if the Modula-3 text
     library code was compiled without debug information.  However, they have
     to be kept in sync with the text libraries and are very fragile if those 
     libraries change.  The values here were taken from the CM3 CVS distribution 
     on 2006-5-20. 

     If the libraries were compiled with debug information, that will be used
     in preference to these defaults, and that will be far more robust. 
  */ 
  if ( ! text_info_is_initialized ) 
    { if ( m3_is_cm3 ( ) ) 
        { if ( ! Text . tc_addr && ! Text . uid ) 
             { get_type_info ( "Text.T", 2, &Text, 0x7e2f4762 ); } 
          if ( ! TextLiteral.tc_addr  && ! TextLiteral . uid ) 
             { get_type_info ( "TextLiteral.T", 2, &TextLiteral, 0xc69eecff ); 
               get_obj_field_info 
                 ( &TextLiteral, "cnt", &TextLiteral_cnt ,32, 32);
               get_obj_field_info 
                 ( &TextLiteral, "buf", &TextLiteral_buf, 64, 0x1ffffef8 ); 

             } 
          if ( ! Text8 . tc_addr && ! Text8 . uid ) 
             { get_type_info ( "Text8.T", 0, &Text8, 0x49d4e83f );
               get_obj_field_info 
                 ( &Text8, "contents", &Text8_contents, 32, 32 );
             } 
          if ( ! Text8Short . tc_addr  & ! Text8Short . uid ) 
             { get_type_info ( "Text8Short.T", 0, &Text8Short, 0xe57bbfe0 ); 
               get_obj_field_info 
                 ( &Text8Short, "len", &Text8Short_len, 32, 32 ); 
               get_obj_field_info 
                 ( &Text8Short, "contents", &Text8Short_contents, 64, 128 ); 
             } 
          if ( ! Text16 . tc_addr && ! Text16 . uid ) 
             { get_type_info ( "Text16.T", 1, &Text16, 0x89ce6bf1 );
               get_obj_field_info 
                 ( &Text16, "contents", &Text16_contents, 32, 32 ); 
             } 
          if ( ! Text16Short . tc_addr && ! Text16Short . uid ) 
             { get_type_info ( "Text16Short.T", 1, &Text16Short, 0xed75fe12 );
               get_obj_field_info 
                 ( &Text16Short, "len", &Text16Short_len, 32, 32 ); 
               get_obj_field_info 
                 ( &Text16Short, "contents", &Text16Short_contents, 64, 256 ); 
             } 
          if ( ! Text8CString . tc_addr && ! Text8CString . uid ) 
             { get_type_info ( "Text8CString.T", 0, &Text8CString, 0xbd2b102e );
               get_obj_field_info 
                 ( &Text8CString, "str", &Text8CString_str, 32, 32 ); 
             } 
          if ( ! TextSub . tc_addr && ! TextSub . uid ) 
             { get_type_info ( "TextSub.TT", 3, &TextSub, 0xe2215acf );
               get_obj_field_info 
                 ( &TextSub, "base", &TextSub_base, 32, 32 ); 
               get_obj_field_info 
                 ( &TextSub, "len", &TextSub_len, 96, 32 ); 
               get_obj_field_info 
                 ( &TextSub, "start", &TextSub_start, 64, 32 ); 
             } 
          if ( ! TextCat.tc_addr && ! TextCat . uid ) 
             { get_type_info ( "TextCat.T", 3, &TextCat, 0x493d82da );
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
      else /* PM3 */ 
        { if ( ! Text . tc_addr & ! Text . uid ) 
            { get_type_info ( "Text.T", 2, &Text, 0xcd7f2264 ); } 
        } 
      text_info_is_initialized = true; 
    } 
} /* init_m3_text_info */ 

bool type_info_matches ( CORE_ADDR tc_addr, struct type_info * info ) 

{ int uid; 

  if ( info -> tc_addr != 0 && tc_addr == info -> tc_addr ) { return true; } 
  uid = find_m3_uid_from_tc ( tc_addr ); 
  if ( info -> uid != 0 && uid == info -> uid ) { return true; }
  return false; 
} /* type_info_matches */

static struct type * library_type_m3_TextLiteral_buf_char; 
static struct type * library_type_m3_TextLiteral_buf_char_index; 
static struct type * library_type_m3_TextLiteral_buf_widechar; 
static struct type * library_type_m3_TextLiteral_buf_widechar_index; 
static bool TextLiteral_types_initialized = false; 

static void init_m3_TextLiteral_library_types ( ) 

{ if ( ! TextLiteral_types_initialized ) 
    { library_type_m3_TextLiteral_buf_char_index  
        = init_type 
           ( TYPE_CODE_M3_SUBRANGE, TARGET_INT_BIT/TARGET_CHAR_BIT /* size */, 
             0 /*flags*/,  "<TextLiteral.T.buff.index_char>", 
             (struct objfile *) NULL 
           );
      TYPE_M3_SIZE ( library_type_m3_TextLiteral_buf_char_index ) 
        = TARGET_INT_BIT; 
      TYPE_M3_SUBRANGE_MIN ( library_type_m3_TextLiteral_buf_char_index ) = 0; 
      TYPE_M3_SUBRANGE_MAX ( library_type_m3_TextLiteral_buf_char_index ) = - 1; 
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
           ( TYPE_CODE_M3_SUBRANGE, TARGET_INT_BIT/TARGET_CHAR_BIT /* size */, 
             0 /*flags*/,  "<TextLiteral.T.buff.index_widechar>", 
             (struct objfile *) NULL 
           );
      TYPE_M3_SIZE ( library_type_m3_TextLiteral_buf_widechar_index ) 
        = TARGET_INT_BIT; 
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
   
      TextLiteral_types_initialized = true; 
    } 
} /* init_m3_TextLiteral_library_types */  

/* Fetch the contents of the boolean field described by fi, of the object at 
   inferior address ref, of object type described by ti. */ 
static bool 
m3_text_field_boolean 
  ( CORE_ADDR ref, struct type_info * ti, struct field_info * fi )  

{ CORE_ADDR addr; 
  LONGEST result; 
  gdb_byte buf [16]; 
  
  addr = ref + fi -> bitpos / TARGET_CHAR_BIT; 
  read_memory ( addr, buf, fi -> bitsize );  
  result = m3_extract_ord ( buf, 0, fi -> bitsize, 0); 
  return result != 0; 
}  /* m3_text_field_boolean */ 

/* Fetch the contents of the signed length field described by fi, of the object 
   at inferior address ref, of object type described by ti. */ 
static LONGEST 
m3_text_field_length 
  ( CORE_ADDR ref, 
    struct type_info * ti, 
    struct field_info * fi, 
    int sign_extend 
  )  

{ CORE_ADDR addr; 
  LONGEST result; 
  gdb_byte buf [16]; 
  
  addr = ref + fi -> bitpos / TARGET_CHAR_BIT; 
  read_memory ( addr, buf, fi -> bitsize );  
  result = m3_extract_ord ( buf, 0, fi -> bitsize, sign_extend); 
  return result; 
}  /* m3_text_field_length */ 

/* Fetch the contents of the pointer field described by fi, of the object at 
   inferior address ref, of object type described by ti. */ 
static CORE_ADDR
m3_text_field_pointer 
  ( CORE_ADDR ref, struct type_info * ti, struct field_info * fi )  

{ CORE_ADDR addr; 
  CORE_ADDR result; 
  gdb_byte buf [16]; 
  
  addr = ref + fi -> bitpos / TARGET_CHAR_BIT; 
  read_memory ( addr, buf, fi -> bitsize );  
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

   If not, return false. If so, return true and construct trumped-up
   type that is right for printing the buf field, having computed its
   element count and element type from the object.  This type is good
   until the next call on this function.  Also supply the 'bitsize',
   'bitpos', and 'field_name', for consistency with other field/method
   lookup functions.
*/ 
bool /* Yes, it's that field of that type. */ 
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
        = m3_text_field_length ( ref, &TextLiteral, &TextLiteral_cnt , 1 ) ;
      if ( string_length < 0 ) 
        { TYPE_M3_SUBRANGE_MAX ( library_type_m3_TextLiteral_buf_widechar_index )
            = - string_length; 
          byte_length 
            = ( - string_length + 1 ) * TARGET_WIDECHAR_BIT / TARGET_CHAR_BIT;
          TYPE_LENGTH ( library_type_m3_TextLiteral_buf_widechar ) = byte_length;
          TYPE_M3_SIZE ( library_type_m3_TextLiteral_buf_widechar )
            = ( - string_length + 1 ) * TARGET_WIDECHAR_BIT ;  
          if ( bitsize != NULL ) { * bitsize = byte_length * TARGET_CHAR_BIT; }  
          if ( bitpos != NULL ) { * bitpos = TextLiteral_buf . bitpos; } 
          if ( field_type != NULL ) 
            { * field_type = library_type_m3_TextLiteral_buf_widechar; } 
          return true; 
        }  
      else 
        { TYPE_M3_SUBRANGE_MAX ( library_type_m3_TextLiteral_buf_char_index ) 
             = string_length; 
          byte_length = string_length + 1; 
          TYPE_LENGTH ( library_type_m3_TextLiteral_buf_char ) = byte_length; 
          TYPE_M3_SIZE ( library_type_m3_TextLiteral_buf_char )
            = ( string_length + 1 ) * TARGET_CHAR_BIT;  
          if ( bitsize != NULL ) { * bitsize = byte_length * TARGET_CHAR_BIT; } 
          if ( bitpos != NULL ) { * bitpos = TextLiteral_buf . bitpos; } 
          if ( field_type != NULL ) 
            { * field_type = library_type_m3_TextLiteral_buf_char; } 
          return true; 
        }  
    }  
  else { return false; } 
} /* m3_check_TextLiteral_buf */

/* Normally stops on a null byte, but will not exceed len chars. */ 
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
          if ( c == '\0' ) { return charsprinted; } 
          m3_emit_char ( c, stream, '"');
          charsprinted ++; 
        }
      lref += buffct;
      charsremaining -= buffct; 
    } 
  return charsprinted; 
} /* m3_print_cstring */ 

/* Does not stop on a null byte, which, in TEXT subtypes, should not occur. */ 
static void
m3_print_chars ( 
    CORE_ADDR ref, 
    LONGEST start,
    LONGEST len, 
    struct ui_file *stream 
  )

{ char *str;
  int i;

  /* copy the string characters into gdb's process space. */
  str = (char *) malloc (len);
  read_memory (ref + start, str, len);
  for (i = 0; i < len; i++) { m3_emit_char ( str[i], stream, '"'); }
  free (str);
} /* m3_print_chars */ 

/* Does not stop on a null byte, which, in TEXT subtypes, should not occur. */ 
static void
m3_print_widechars ( 
    CORE_ADDR ref, 
    LONGEST start, /* In WIDECHARs. */ 
    LONGEST len,   /* In WIDECHARs. */ 
    struct ui_file *stream 
  )

{ unsigned short *wstr;
  int i;
  int widecharsperchar = TARGET_WIDECHAR_BIT / TARGET_CHAR_BIT; 

  /* copy the wide characters into gdb's process space. */
  wstr = (unsigned short*) malloc ( len * widecharsperchar);
  read_memory 
    (ref + start * widecharsperchar , (char *) wstr, len * widecharsperchar );

  for (i = 0; i < len; i++) { m3_emit_widechar ( wstr[i] , stream, '"' ); }
  free (wstr);
} /* m3_print_widechars */ 

/* Special case to print CM3 TextLiteral.T value as an object.  It contains
   a field named "buf", whose type is a very long array, but actually, only
   contains (usually far) fewer CHARs or WIDECHARs, as indicated by the value
   of field "ABS(cnt)".  Also, it's WIDECHARs iff cnt < 0. 
   PRE: m3_is_cm3 ( ), and init_m3_text_info has been called.  
*/ 
void 
m3_print_cm3_TextLiteral_object ( 
    CORE_ADDR ref,         /* Inferior address of the object. */ 
    struct ui_file *stream 
  ) 

{ CORE_ADDR chars;
  LONGEST string_length;

  string_length 
    = m3_text_field_length ( ref, &TextLiteral, &TextLiteral_cnt , 1 ) ;
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
   Call this only if code is known to be CM3-compiled and init_m3_text_info 
   has been called. 
   Try to print as a CM3 TEXT value.
*/ 
static ULONGEST /* - 3 if not a CM3 TEXT or subtype thereof. 
                   - 2 if a CM3 TEXT subtype, but improperly formed.   
                   - 1 if CM3 string, but longer than length, so was truncated.
                   Otherwise, number of chars (of either size) actually printed. */ 
m3_print_cm3_text ( 
    CORE_ADDR ref,      /* Inferior address of the object. */ 
    CORE_ADDR tc_addr,  /* Inferior address of its Typecell. */
    ULONGEST start,     /* skip this many chars before starting printing. */
    ULONGEST length,    /* print at most this many, (beyond start). */ 
    bool add_quotes,
    struct ui_file *stream 
  )

{ ULONGEST result;
  CORE_ADDR chars;
  ULONGEST string_length;
  ULONGEST print_length;
  CORE_ADDR open_array; 

  if ( type_info_matches ( tc_addr, &Text ) ) 
    { fputs_filtered ("\"<Not a proper subtype of CM3 TEXT>\"", stream ); 
      return - 2; 
    } 
  else if ( type_info_matches ( tc_addr, &TextLiteral ) ) 
    { string_length 
        = m3_text_field_length ( ref, &TextLiteral, &TextLiteral_cnt , 1 ) ;
      chars = m3_text_field_addr ( ref, &TextLiteral, &TextLiteral_buf );   
      if (string_length < 0) /* Wide chars. */ 
        { string_length = - string_length - start; 
          if ( length < string_length) { print_length = length; result = - 1; }
          else { print_length = string_length; result = string_length; } 
          if (add_quotes) { fputs_filtered ("W\"", stream); } 
          m3_print_widechars ( chars, start, print_length, stream);
        } 
      else /* Narrow chars. */ 
        { string_length -= start; 
          if ( length < string_length) { print_length = length; result = - 1; }
          else { print_length = string_length; result = string_length; } 
          if (add_quotes) { fputs_filtered ("\"", stream); } 
          m3_print_chars ( chars, start, print_length, stream);
        } 
    } 
    
  else if ( type_info_matches ( tc_addr, &Text8 ) ) 
    { open_array = m3_text_field_pointer ( ref , &Text8, &Text8_contents ); 
      string_length 
        = m3_inf_open_array_shape_component ( open_array, 0 ) - start - 1; 
      chars = m3_inf_open_array_elems_addr ( open_array );
      if ( length < string_length ) { print_length = length; result = - 1; }
      else { print_length = string_length; result = string_length; } 
      if (add_quotes) { fputs_filtered ("\"", stream); } 
      m3_print_chars ( chars, start, print_length, stream);
    } 

  else if ( type_info_matches ( tc_addr, &Text16 ) ) 
    { open_array = m3_text_field_pointer ( ref , &Text16, &Text16_contents ); 
      string_length 
        = m3_inf_open_array_shape_component ( open_array, 0 ) - start - 1;
      chars = m3_inf_open_array_elems_addr ( open_array );
      if ( length < string_length ) { print_length = length; result = - 1; }
      else { print_length = string_length; result = string_length; } 
      if (add_quotes) { fputs_filtered ("W\"", stream); }
      m3_print_widechars ( chars, start, print_length, stream);
    } 

  else if ( type_info_matches ( tc_addr, &Text8Short ) ) 
    { string_length 
        = m3_text_field_length ( ref, &Text8Short, &Text8Short_len, 0 ) - start;
      if ( string_length 
           > Text8Short_contents . bitsize / TARGET_CHAR_BIT - start 
         )
        { return - 2; } 
      chars = m3_text_field_addr ( ref, &Text8Short, &Text8Short_contents );
      if ( length < string_length ) { print_length = length; result = - 1; }
      else { print_length = string_length ; result = string_length; } 
      if (add_quotes) { fputs_filtered ("\"", stream); } 
      m3_print_chars ( chars, start, print_length, stream);
    } 

  else if ( type_info_matches ( tc_addr, &Text16Short ) ) 
    { string_length 
        = m3_text_field_length 
            ( ref, &Text16Short, &Text16Short_len, 0 ) - start;
      if ( string_length 
           > Text16Short_contents . bitsize / TARGET_WIDECHAR_BIT - start 
         ) 
        { return - 2; } 
      chars = m3_text_field_addr ( ref, &Text16Short, &Text16Short_contents );
      if ( length < string_length ) { print_length = length; result = - 1; }
      else { print_length = string_length ; result = string_length; } 
      if (add_quotes) { fputs_filtered ("W\"", stream); } 
      m3_print_widechars ( chars, start, print_length, stream);
    } 

  else if ( type_info_matches ( tc_addr, &Text8CString ) ) 
    { chars = m3_text_field_pointer ( ref , &Text8CString, &Text8CString_str ); 
      if (add_quotes) { fputs_filtered ("\"", stream); } 
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
        { bool a_or_b_wide 
            = m3_text_field_boolean ( ref, &TextCat, &TextCat_a_or_b_wide ); 
          if (a_or_b_wide) { fputs_filtered ("W\"", stream); } 
          else { fputs_filtered ("\"", stream); } 
        } 
      if ( length > 0 ) 
        { a_tc_addr = find_m3_heap_tc_addr (a_object);
          a_len = m3_text_field_length ( ref , &TextCat, &TextCat_a_len, 0 ); 
          if ( start >= a_len ) { start -= a_len; } 
          else 
            { a_printed 
                = m3_print_cm3_text 
                    ( a_object, a_tc_addr, start, length, false, stream ); 
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
        { b_tc_addr = find_m3_heap_tc_addr (b_object);
          b_printed 
            = m3_print_cm3_text 
                ( b_object, b_tc_addr, start, length, false, stream ); 
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
        = m3_text_field_length ( ref , &TextSub, &TextSub_start, 0 ) + start;
      string_length = m3_text_field_length ( ref , &TextSub, &TextSub_len, 0 ); 
      if ( length < string_length) { print_length = length; result = - 1; }
      else { print_length = string_length; result = string_length; } 
      tc_addr = find_m3_heap_tc_addr ( base );
      result 
        = m3_print_cm3_text 
            ( base, tc_addr, start + substart, print_length, add_quotes, stream);
      add_quotes = false; 
      if ( result == - 1 ) /* We expect this. */ { result = print_length; } 
      /* result values - 2 and - 3 just get passed up. */ 
    } 

  else /* It's not a CM3 TEXT subtype. */ { return - 3; }

  /* Fall through to here if we recognized and printed a CM3 TEXT subtype. */ 
  if (add_quotes) { fputs_filtered ("\"", stream); } 
  return result;  
} /* m3_print_cm3_text */ 

static void
m3_print_object_1 (
     const gdb_byte *valaddr,
     CORE_ADDR tc_addr,
     struct ui_file *stream,
     int format)

{
  char name [100];
  struct type *this_obj;
  struct symbol *this_obj_sym;
  int i, data_offset; 

  if (tc_addr == 0) {
    return; }

  this_obj = find_m3_type_from_tc (tc_addr);
  if (TYPE_CODE (this_obj) == TYPE_CODE_M3_ROOT
      || TYPE_CODE (this_obj) == TYPE_CODE_M3_TRANSIENT_ROOT
      || TYPE_CODE (this_obj) == TYPE_CODE_M3_UN_ROOT) {
    return; }

  m3_print_object_1 (valaddr, m3_tc_addr_to_super_tc_addr (tc_addr),
		     stream, format);
  data_offset = m3_tc_address_to_dataOffset (tc_addr);

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
}

/* TODO: make this queryable and alterable by user command: */ 
int max_string_length = 2048; 

/* This format letter causes CM3 TEXT values to be displayed as objects,
   rather than strings. */ 
/* TODO: Figure out what letter this really should be. */ 
int m3_text_object_format = 'k'; 

static void
m3_print_object(
     const gdb_byte * valaddr,
     int bitpos,
     struct type* type,
     struct ui_file *stream,
     int format)

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
  tc_addr = find_m3_heap_tc_addr (ref);
  if (tc_addr == 0) {
    fprintf_filtered 
      (stream, "<Can't find Modula-3 TypeCell for object at ");
    m3_print_scalar (valaddr, bitpos, TARGET_PTR_BIT, stream, 'x', 0); 
    /* FIXME: valaddr does not point to a place where the negative-displacement
       GC word is present. 
    fprintf_filtered (stream, ", with typecode %d>", m3_typecode ( valaddr ) );
    */ 
    return;
  }

  init_m3_text_info ( );  

  /* Print the address of the object. */ 
  fprintf_filtered (stream, "(*");
  m3_print_scalar (valaddr, bitpos, TARGET_PTR_BIT, stream, 'x', 0); 
  fprintf_filtered (stream, "*) ");

  /* Is it a CM3 TEXT? */
  if ( m3_is_cm3 ( ) && format != m3_text_object_format ) 
    { int actual_printed; 

      actual_printed 
        = m3_print_cm3_text 
            ( ref , tc_addr, 0, max_string_length, true, stream );  
      switch ( actual_printed ) 
        { case - 1: /* CM3 string, was truncated at requested max length. */ 
            { fputs_filtered ("...", stream);
              return ; 
            }  
          case - 2: /* Ill-formed CM3 string.  Also print it as an object. */  
          case - 3: /* Wasn't a CM3 string. */ { break; }   
          default: /* CM3 string, was printed at full length. */ { return; } 
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
  m3_print_object_1 
    ( bits, tc_addr, stream, format == m3_text_object_format ? 0 : format );
  fputs_filtered (" END", stream);
  free (bits);
}

/* Print data of type TYPE located at VALADDR (within the GDB process space), 
   which came from the inferior at address ADDRESS, onto stdio stream STREAM,
   according to FORMAT (a letter or 0 for natural format).  The data at VALADDR 
   is in target byte order.
   
   If the data are a string pointer, returns the number of string characters
   printed.
   
   If DEREF_REF is nonzero, then dereference references, otherwise just print
   them like pointers.
   
   The PRETTY parameter controls prettyprinting.  */

extern unsigned int repeat_count_threshold;

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
  register unsigned int i = 0;		/* Number of characters printed */
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
    case TYPE_CODE_M3_ARRAY: {
      struct type *index = TYPE_M3_ARRAY_INDEX (type);
      struct type *elt   = TYPE_M3_ARRAY_ELEM (type);
      LONGEST lower, upper, n;
      
      fputs_filtered ("[", stream);

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

      fputs_filtered ("]", stream);
      break; }
      
    case TYPE_CODE_M3_OPEN_ARRAY: {
      struct type *elt_type = TYPE_M3_OPEN_ARRAY_ELEM (type);
      CORE_ADDR    elems    = m3_extract_address (valaddr, bitpos);
      int          eltsize  = 1;
      int          nelems;
      int          open_dimension_ct; 

      nelems = m3_extract_ord (valaddr + TARGET_PTR_BIT/HOST_CHAR_BIT,
				       bitpos, TARGET_LONG_BIT, 0);
      /*FIXME: Redundancy here and a few lines below, fetching the element count. */ 

      if (bitpos % HOST_CHAR_BIT != 0) {
	error ("improperly aligned open array"); }

      valaddr += (bitpos / HOST_CHAR_BIT);
      bitpos = 0;

      { struct type *e = elt_type;
	const gdb_byte *nelem_addr = valaddr
	                    + (TARGET_PTR_BIT + TARGET_LONG_BIT)/HOST_CHAR_BIT;
        open_dimension_ct = 1; 
	while (TYPE_CODE (e) == TYPE_CODE_M3_OPEN_ARRAY) 
          { eltsize = eltsize * m3_extract_ord (nelem_addr, 0, TARGET_LONG_BIT,0);
	    nelem_addr += TARGET_LONG_BIT / HOST_CHAR_BIT;
	    e = TYPE_M3_OPEN_ARRAY_ELEM (e); 
            open_dimension_ct ++; 
          }  
	eltsize = eltsize * TYPE_M3_SIZE (e); }
      if (eltsize % 8 != 0) {
	error ("another improper alignment"); }
      eltsize = eltsize / 8;

      fputs_filtered ("[", stream);
      if ( TYPE_CODE ( elt_type ) == TYPE_CODE_M3_OPEN_ARRAY ) 
        { gdb_byte * subval = alloca ( TYPE_LENGTH ( elt_type ) ); 
          int dimension;
          ULONGEST shape_component;  

          for ( dimension = 1; dimension < open_dimension_ct; dimension ++ ) 
            { shape_component 
                = open_array_shape_component ( valaddr, dimension );
              set_open_array_shape_component
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
              set_open_array_elems_addr ( subval, elems + i * eltsize ); 
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
		fprintf_filtered (stream, " <repeats %d times>", reps); }
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
	    fprintf_filtered (stream, " <repeats %d times>", reps); 
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
      fputs_filtered ("]", stream);
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
      };
      break; }
      
    case TYPE_CODE_M3_INDIRECT: {
      CORE_ADDR target_addr = m3_extract_address (valaddr, 0);
      struct type *target = TYPE_M3_INDIRECT_TARGET (type);
      int target_size = TYPE_LENGTH (target);
      gdb_byte *target_val = alloca (target_size); 
      
      read_memory (target_addr, target_val, target_size);
      tmpbitsize = TYPE_M3_SIZE (target);
      m3_val_print2 (target, target_val, 
		     0, tmpbitsize, 
		     stream, format, deref_ref, toplevel);
      break; }
      
    case TYPE_CODE_M3_PROC: 
    case TYPE_CODE_M3_METHOD:  /* REVIEWME: Is this right?  */ 
      {
      m3_print_scalar (valaddr, bitpos, bitsize, stream,
		       format ? format : 'x', 0);
      break; }
      
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
      LONGEST lower, upper;
      struct type *target = TYPE_M3_SET_TARGET (type);
      int nelems = TYPE_NFIELDS (target);
      int en = (TYPE_CODE (target) == TYPE_CODE_M3_ENUM);
      int ch = (TYPE_CODE (target) == TYPE_CODE_M3_CHAR);
      int chs = (TYPE_CODE (target) == TYPE_CODE_M3_SUBRANGE)
	&& (TYPE_CODE (TYPE_M3_SUBRANGE_TARGET (target)) == TYPE_CODE_M3_CHAR);

      m3_ordinal_bounds (target, &lower, &upper);
      fputs_filtered ("{", stream);
      
      for (i = 0; i < TYPE_LENGTH (type) / sizeof (long); i++) {
	val = m3_extract_ord (valaddr, bitpos, TARGET_LONG_BIT, 0);
	for (j = 0; j < TARGET_LONG_BIT; j++) {
	  LONGEST ord = i * TARGET_LONG_BIT + j + lower;
	  if ((val & 1 << j) && (ord <= upper)) {
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
      
    case TYPE_CODE_M3_SUBRANGE: {
      LONGEST lower, upper, val;
      struct type *target = TYPE_M3_SUBRANGE_TARGET (type);
      m3_ordinal_bounds (type, &lower, &upper);
      val = m3_extract_ord (valaddr, bitpos, bitsize, (lower < 0));
      if ((val < lower) || (upper < val)) {
        fprintf_filtered(stream, "<subrange value ");
	print_longest (stream, 'd', 1, val);
        fprintf_filtered(stream, " out of range [");
	print_longest (stream, 'd', 1, lower);
        fprintf_filtered(stream, "..");
	print_longest (stream, 'd', 1, upper);
        fprintf_filtered(stream, "]>");
      } else if (TYPE_CODE (target) == TYPE_CODE_M3_ENUM) {
        fputs_filtered (TYPE_M3_ENUM_VALNAME (target, val), stream);
      } else {
	m3_print_scalar (valaddr, bitpos, bitsize, stream, format, (lower <0));
      }
      break; }

    case TYPE_CODE_M3_ADDRESS:
      m3_print_scalar (valaddr, bitpos, bitsize, stream, 
		       format ? format : 'x', 0);
      break;

    case TYPE_CODE_M3_BOOLEAN:
      if (m3_extract_ord (valaddr, bitpos, bitsize, 0)) {
	fputs_filtered ("TRUE", stream); }
      else {
	fputs_filtered ("FALSE", stream); }
      break;

    case TYPE_CODE_M3_CHAR:
      m3_printchar 
       ( (int) m3_extract_ord (valaddr, bitpos, TARGET_CHAR_BIT, 0), stream);
      break;

    case TYPE_CODE_M3_WIDECHAR:
      m3_printwidechar 
        ( (int) m3_extract_ord (valaddr, bitpos, TARGET_WIDECHAR_BIT , 0), 
          stream 
        );
      break;

    case TYPE_CODE_M3_INTEGER:
      m3_print_scalar (valaddr, bitpos, bitsize, stream, format, 1);
      break;

    case TYPE_CODE_M3_CARDINAL:
      m3_print_scalar (valaddr, bitpos, bitsize, stream, format, 0);
      break;

    case TYPE_CODE_M3_NULL:
      fputs_filtered ("NIL", stream); 
      break;

    case TYPE_CODE_M3_VOID:
      fputs_filtered ("<void>", stream); 
      break;

    case TYPE_CODE_M3_ROOT:
    case TYPE_CODE_M3_TRANSIENT_ROOT:
    case TYPE_CODE_M3_UN_ROOT:
    case TYPE_CODE_M3_OBJECT: {
      if ( deref_ref && ( format == 0 || format == m3_text_object_format ) ) {
	m3_print_object (valaddr, bitpos, type, stream, format); }
      else {
	m3_print_scalar (valaddr, bitpos, bitsize, stream, 
			 format ? format : 'x', 0); }
      break; }

    case TYPE_CODE_M3_REFANY:
    case TYPE_CODE_M3_TRANSIENT_REFANY: {
      m3_print_scalar (valaddr, bitpos, bitsize, stream, 
		       format ? format : 'x', 0);
      break; }

    case TYPE_CODE_M3_POINTER: {
      int down_format; 

      if ( format == 0 || format == m3_text_object_format ) 
        { down_format = 'x'; } 
      else { down_format = format; } 
      if ( m3_is_cm3 ( ) )
        { m3_print_scalar ( valaddr, bitpos, bitsize, stream, down_format , 0 );
        } 
      else /* PM3 */   
        { init_m3_text_info ( ); 
          if ( format != m3_text_object_format 
               && ( ( Text . type != 0 && Text . type == type ) 
                    || ( Text . uid != 0 
                         && Text . uid == int_uid_from_m3_type ( type ) 
                       ) 
                  ) 
             )  
            /* PM3 TEXT value. */ 
            { CORE_ADDR text_value;
              CORE_ADDR chars_addr;
              int nelems;

              text_value = m3_extract_address (valaddr, bitpos);
              /* text_value is the pointer to the open array. */ 
              if (text_value == 0) 
                { fputs_filtered ("NIL", stream); }
              else 
                { fputs_filtered ("(*", stream); 
                  m3_print_scalar ( valaddr, bitpos, bitsize, stream, 'x', 0 );
                  fputs_filtered ("*)", stream); 
                  nelems = m3_inf_open_array_shape_component ( text_value, 0 );
                  chars_addr = m3_inf_open_array_elems_addr ( text_value ); 
                  /* val_print_string will stop on a null byte, but not exceed
                     nelems.  For a properly-formed PM3 text value, that will
                     work fine, as it has both a null byte and a count (that
                     includes the null byte.  To see the bytes as an array, the
                     user can dereference the TEXT value. */ 
                  val_print_string (chars_addr, nelems, 1, stream); 
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
      fputs_filtered (valaddr, stream);
      fputs_filtered ("\"", stream);
      break;

    default: 
     return 1; }

  gdb_flush (stream); 
  return (0);
} /* m3_val_print2 */ 

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

  if (m3_val_print2 (type, valaddr, 0, tmpbitsize,
		     stream, format, deref_ref, 1)) {
    /* like the value of registers */
    return c_val_print (type, valaddr, embedded_offset, address, stream,
			format, deref_ref, recurse, pretty);
  }
  return 0;
}

int
m3_value_print (
     struct value * val,
     struct ui_file *stream,
     int format,
     enum val_prettyprint pretty)

{ struct value * unpacked_val;
  struct type * val_type;

  val_type = value_type (val);
  if (*(LONGEST *) value_contents (val) == m3_type_magic_value) 
    { m3_print_type (val_type, 0, stream, 0, 0); 
      return 0; 
    } 
  else 
    { /* On top-level, prefix pointer value with (type).  RMB. For PM3, this used
         to not write the prefix if it's a text, but I don't think it will
         be bad, even in that case.  */
      if ( 0 && TYPE_CODE (val_type) == TYPE_CODE_M3_POINTER) 
        { fprintf_filtered (stream, "(*");
          m3_print_type (val_type, 0, stream, 0, 0);
          fprintf_filtered (stream, "*)");
        }
      return 
        ( val_print 
            ( val_type, value_contents (val),
	      value_embedded_offset (val), 
	      VALUE_ADDRESS (val) + value_offset ( val ), 
              stream, format, 1, 0, pretty
            )
        );
    } 
} /* m3_value_print */ 
