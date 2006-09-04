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

#define MAX_SYMBOLLEN 150

#include "defs.h"
#include "gdb_assert.h"
#include "symtab.h"
#include "gdbtypes.h"
#include "expression.h"
#include "parser-defs.h"
#include "language.h"
  /* #include "value.h" */
#include "m3-lang.h"
#include "m3-uid.h"
#include "frame.h"
#include "target.h"
#include "gdbcore.h" 
#include "gdb_string.h"
#include "command.h" 
#include "valprint.h"
#include "regcache.h"
#include "block.h"
#include "dictionary.h" 
#include "infcall.h" 
#include "gdbarch.h" 
#include "gdb_string.h"

#include <stdbool.h>

bool 
is_unsafe ( void ) 

  { /* FIXME: Implement this. */ 
    return true; 
  } /* is_unsafe */ 

/* Special value attached to "type" values 
   FIXME: There has to be a cleaner way to do this. */
#define M3_TYPE_MAGIC 766579803L
LONGEST m3_type_magic_value = ( LONGEST ) M3_TYPE_MAGIC;

static void
m3_print_subexp (struct expression *exp, int *pos,
                 struct ui_file *stream, enum precedence prec);
static void
m3_operator_length (struct expression *expr, int endpos,
                    int *oplenp, int *argsp);
static char *m3_op_name (enum exp_opcode);
static int
m3_dump_subexp_body (struct expression *exp, struct ui_file *stream, int elt);

static struct value *
m3_evaluate_subexp(struct type *expect_type,
		   struct expression *exp, int *pos,
                   enum noside noside);

struct symbol * find_m3_ir ( int, char * );

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
	  fprintf_filtered (stream, "\\%.3o", (unsigned int) c);
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
	  fprintf_filtered (stream, "\\x%.4x", (unsigned int) c);
	  break;
	}
    }
}

void
m3_printchar (c, stream)
     int c;
     struct ui_file *stream;
{
  fputs_filtered ("'", stream);
  m3_emit_char (c, stream, '\'');
  fputs_filtered ("'", stream);
}

void
m3_printwidechar (c, stream)
     int c;
     struct ui_file *stream;
{
  fputs_filtered ("W'", stream);
  m3_emit_widechar (c, stream, '\'');
  fputs_filtered ("'", stream);
}

/* Print the character string STRING, printing at most LENGTH characters.
   LENGTH is -1 if the string is nul terminated.  Each character is WIDTH bytes
   long.  Printing stops early if the number hits print_max; repeat counts are
   printed as appropriate.  Print ellipsis at the end if we had to stop before
   printing LENGTH characters, or if FORCE_ELLIPSIS.  */

static void
m3_printstr (stream, string, length, width, force_ellipsis)
     struct ui_file *stream;
     char *string;
     unsigned int length;
     int width;
     int force_ellipsis;
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
	  m3_printchar (string[i], stream);
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
}

/* Create a fundamental C type using default reasonable for the current
   target machine.

   Some object/debugging file formats (DWARF version 1, COFF, etc) do not
   define fundamental types such as "int" or "double".  Others (stabs or
   DWARF version 2, etc) do define fundamental types.  For the formats which
   don't provide fundamental types, gdb can create such types using this
   function.

   FIXME:  Some compilers distinguish explicitly signed integral types
   (signed short, signed int, signed long) from "regular" integral types
   (short, int, long) in the debugging information.  There is some dis-
   agreement as to how useful this feature is.  In particular, gcc does
   not support this.  Also, only some debugging formats allow the
   distinction to be passed on to a debugger.  For now, we always just
   use "short", "int", or "long" as the type name, for both the implicit
   and explicitly signed types.  This also makes life easier for the
   gdb test suite since we don't have to account for the differences
   in output depending upon what the compiler and debugging format
   support.  We will probably have to re-examine the issue when gdb
   starts taking it's fundamental type information directly from the
   debugging information supplied by the compiler.  fnf@cygnus.com */

static struct type *
m3_create_fundamental_type (objfile, typeid)
     struct objfile *objfile;
     int typeid;
{
  struct type *type = NULL;

  switch (typeid)
    {
      default:
	/* FIXME:  For now, if we are asked to produce a type not in this
	   language, create the equivalent of a C integer type with the
	   name "<?type?>".  When all the dust settles from the type
	   reconstruction work, this should probably become an error. */
	type = init_type (TYPE_CODE_INT,
			  TARGET_INT_BIT / TARGET_CHAR_BIT,
			  0, "<?type?>", objfile);
        warning ("internal error: no C/C++ fundamental type %d", typeid);
	break;
      case FT_VOID:
	type = init_type (TYPE_CODE_VOID,
			  TARGET_CHAR_BIT / TARGET_CHAR_BIT,
			  0, "void", objfile);
	break;
      case FT_CHAR:
	type = init_type (TYPE_CODE_INT,
			  TARGET_CHAR_BIT / TARGET_CHAR_BIT,
			  0, "char", objfile);
	break;
      case FT_SIGNED_CHAR:
	type = init_type (TYPE_CODE_INT,
			  TARGET_CHAR_BIT / TARGET_CHAR_BIT,
			  0, "signed char", objfile);
	break;
      case FT_UNSIGNED_CHAR:
	type = init_type (TYPE_CODE_INT,
			  TARGET_CHAR_BIT / TARGET_CHAR_BIT,
			  TYPE_FLAG_UNSIGNED, "unsigned char", objfile);
	break;
      case FT_SHORT:
	type = init_type (TYPE_CODE_INT,
			  TARGET_SHORT_BIT / TARGET_CHAR_BIT,
			  0, "short", objfile);
	break;
      case FT_SIGNED_SHORT:
	type = init_type (TYPE_CODE_INT,
			  TARGET_SHORT_BIT / TARGET_CHAR_BIT,
			  0, "short", objfile);	/* FIXME-fnf */
	break;
      case FT_UNSIGNED_SHORT:
	type = init_type (TYPE_CODE_INT,
			  TARGET_SHORT_BIT / TARGET_CHAR_BIT,
			  TYPE_FLAG_UNSIGNED, "unsigned short", objfile);
	break;
      case FT_INTEGER:
	type = init_type (TYPE_CODE_INT,
			  TARGET_INT_BIT / TARGET_CHAR_BIT,
			  0, "int", objfile);
	break;
      case FT_SIGNED_INTEGER:
	type = init_type (TYPE_CODE_INT,
			  TARGET_INT_BIT / TARGET_CHAR_BIT,
			  0, "int", objfile); /* FIXME -fnf */
	break;
      case FT_UNSIGNED_INTEGER:
	type = init_type (TYPE_CODE_INT,
			  TARGET_INT_BIT / TARGET_CHAR_BIT,
			  TYPE_FLAG_UNSIGNED, "unsigned int", objfile);
	break;
      case FT_LONG:
	type = init_type (TYPE_CODE_INT,
			  TARGET_LONG_BIT / TARGET_CHAR_BIT,
			  0, "long", objfile);
	break;
      case FT_SIGNED_LONG:
	type = init_type (TYPE_CODE_INT,
			  TARGET_LONG_BIT / TARGET_CHAR_BIT,
			  0, "long", objfile); /* FIXME -fnf */
	break;
      case FT_UNSIGNED_LONG:
	type = init_type (TYPE_CODE_INT,
			  TARGET_LONG_BIT / TARGET_CHAR_BIT,
			  TYPE_FLAG_UNSIGNED, "unsigned long", objfile);
	break;
      case FT_LONG_LONG:
	type = init_type (TYPE_CODE_INT,
			  TARGET_LONG_LONG_BIT / TARGET_CHAR_BIT,
			  0, "long long", objfile);
	break;
      case FT_SIGNED_LONG_LONG:
	type = init_type (TYPE_CODE_INT,
			  TARGET_LONG_LONG_BIT / TARGET_CHAR_BIT,
			  0, "signed long long", objfile);
	break;
      case FT_UNSIGNED_LONG_LONG:
	type = init_type (TYPE_CODE_INT,
			  TARGET_LONG_LONG_BIT / TARGET_CHAR_BIT,
			  TYPE_FLAG_UNSIGNED, "unsigned long long", objfile);
	break;
      case FT_FLOAT:
	type = init_type (TYPE_CODE_FLT,
			  TARGET_FLOAT_BIT / TARGET_CHAR_BIT,
			  0, "float", objfile);
	break;
      case FT_DBL_PREC_FLOAT:
	type = init_type (TYPE_CODE_FLT,
			  TARGET_DOUBLE_BIT / TARGET_CHAR_BIT,
			  0, "double", objfile);
	break;
      case FT_EXT_PREC_FLOAT:
	type = init_type (TYPE_CODE_FLT,
			  TARGET_LONG_DOUBLE_BIT / TARGET_CHAR_BIT,
			  0, "long double", objfile);
	break;
      }
  return (type);
}


/* Table mapping opcodes into strings for printing operators,
   also precedences, associativities, pre/postfix properties. */


static const struct op_print m3_op_print_tab[] =
{ {":=", BINOP_ASSIGN, PREC_ASSIGN, 0},
  {"ABS", UNOP_M3_ABS, PREC_BUILTIN_FUNCTION, 0},
  {"ADR", UNOP_M3_ADR, PREC_BUILTIN_FUNCTION, 0},
  {"ADRSIZE", UNOP_M3_ADRSIZE, PREC_BUILTIN_FUNCTION, 0},
  {"BITSIZE", UNOP_M3_BITSIZE, PREC_BUILTIN_FUNCTION, 0},
  {"BYTESIZE", UNOP_M3_BYTESIZE, PREC_BUILTIN_FUNCTION, 0},
  {"CEILING", UNOP_M3_CEILING, PREC_BUILTIN_FUNCTION, 0},
  {"FIRST", UNOP_M3_FIRST, PREC_BUILTIN_FUNCTION, 0},
  {"FLOOK", UNOP_M3_FLOOR, PREC_BUILTIN_FUNCTION, 0},
  {"LAST", UNOP_M3_LAST, PREC_BUILTIN_FUNCTION, 0},
  {"NUMBER", UNOP_M3_NUMBER, PREC_BUILTIN_FUNCTION, 0},
  {"ORD", UNOP_M3_ORD, PREC_BUILTIN_FUNCTION, 0},
  {"ROUND", UNOP_M3_ROUND, PREC_BUILTIN_FUNCTION, 0},
  {"TRUNC", UNOP_M3_TRUNC, PREC_BUILTIN_FUNCTION, 0},

  {"^", UNOP_M3_DEREF, PREC_SUFFIX, 1},

  {"-", UNOP_M3_NEG, PREC_PREFIX, 0},

  {"*", BINOP_M3_MULT, PREC_MUL, 0},
  {"/", BINOP_M3_DIVIDE, PREC_MUL, 0},
  {"DIV", BINOP_M3_DIV, PREC_MUL, 0},
  {"MOD", BINOP_M3_MOD, PREC_MUL, 0},

  {"+", BINOP_M3_ADD, PREC_ADD, 0},
  {"-", BINOP_M3_MINUS, PREC_ADD, 0},
  {"&", BINOP_M3_CAT, PREC_ADD, 0},

  {"=", BINOP_M3_EQUAL, PREC_ORDER, 0},
  {"<>", BINOP_M3_NE, PREC_ORDER, 0},
  {"<=", BINOP_M3_LE, PREC_ORDER, 0},
  {">=", BINOP_M3_GE, PREC_ORDER, 0},
  {">", BINOP_M3_GT, PREC_ORDER, 0},
  {"<", BINOP_M3_LT, PREC_ORDER, 0},
  {"IN", BINOP_M3_IN, PREC_ORDER, 0},

  {"NOT", UNOP_M3_NOT, PREC_EQUAL, 0},

  {"AND", BINOP_M3_AND, PREC_LOGICAL_AND, 0},

  {"OR", BINOP_M3_OR, PREC_LOGICAL_OR, 0},

  {NULL, 0, 0, 0}
};

struct type ** const (m3_builtin_types[]) = 
{
  &builtin_type_m3_integer,
  &builtin_type_long,
  &builtin_type_short,
  &builtin_type_char,
  &builtin_type_m3_widechar,
  &builtin_type_float,
  &builtin_type_double,
  &builtin_type_void,
  &builtin_type_long_long,
  &builtin_type_signed_char,
  &builtin_type_unsigned_char,
  &builtin_type_unsigned_short,
  &builtin_type_unsigned_int,
  &builtin_type_unsigned_long,
  &builtin_type_unsigned_long_long,
  &builtin_type_long_double,
  &builtin_type_complex,
  &builtin_type_double_complex,
  0
};

static void
m3_error (char *msg)
{ if (msg) error (msg); 
  else error ( "Invalid syntax in expression." );
}


static const struct exp_descriptor m3_exp_descriptor = {
  m3_print_subexp,
  m3_operator_length, 
  m3_op_name,
  m3_dump_subexp_body,
  m3_evaluate_subexp   
};

const struct language_defn m3_language_defn = {
  "m3",				 /* la_name: Language name */
  language_m3,                   /* la_language: */
  m3_builtin_types,              /* la_builtin_type_vector: */ 
  range_check_on,                /* la_range_check: */
  type_check_off,                /* la_type_check: */
  case_sensitive_on,             /* la_case_sensitivity: */ 
  array_row_major,               /* la_array_ordering: */ 
  &m3_exp_descriptor,            /* la_exp_desc: */ 
  m3_parse,                      /* la_parser: */ 
  m3_error,                      /* la_error: */ 
  null_post_parser,              /* la_post_parser: */ 
  m3_printchar,			 /* la_printchar: Print a character constant */
  m3_printstr,			 /* la_printstr: Print a string constant */
  m3_emit_char,			 /* la_emitchar: Print a character constant */
  m3_create_fundamental_type,	 /* la_fund_type: Create fundamental type */
  m3_print_type,		 /* la_print_type: */
  m3_val_print,			 /* la_val_print */
  m3_value_print,		 /* la_value_print: a top-level value */
  NULL,                          /* la_skip_trampoline: */ 
  NULL,                          /* la_value_of_this: */ 
  basic_lookup_symbol_nonlocal,  /* la_lookup_symbol_nonlocal: */ 
  basic_lookup_transparent_type, /* la_lookup_transparent_type: */
  m3_demangle,                   /* la_demangle: symbol demangler */
  NULL,                          /* la_class_name_from_physname: */
  m3_op_print_tab,               /* la_op_print_tab: */ 
  0,				 /* c_style_arrays: arrays are first-class */
  0,				 /* string_lower_bound: */
  &builtin_type_m3_char,	 /* string_char_type: Type of string elements */
  default_word_break_characters, /* la_word_break_characters: */
  NULL,                          /* FIXME: la_language_arch_info: */
  default_print_array_index,     /* la_print_array_index: */ 

  LANG_MAGIC
};

#define eval(x) evaluate_expression (parse_expression (x))
#define print(x) value_print (x, stdout, 0, Val_pretty_default)
#define printx(y) value_print (y, stdout, 'x', Val_pretty_default)


static struct type *thread__t = 0;
static int thread__t__id_size, thread__t__id_offset;
static int thread__t__state_size, thread__t__state_offset;
static int thread__t__next_size, thread__t__next_offset;
static int thread__t__cond_size, thread__t__cond_offset;
static struct type * thread__t__cond_type;
static int thread__t__mutex_size, thread__t__mutex_offset;
static struct type * thread__t__mutex_type;
static int thread__t__time_size, thread__t__time_offset;
static struct type * thread__t__time_type;
static int thread__t__context_size, thread__t__context_offset;
static struct type * thread__t__context_type;
static int thread__t__buf_size, thread__t__buf_offset;
static struct type * thread__t__buf_type;

/* Convert Modula-3 numbers into newly allocated values */
static struct value * 
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

static void
m3_print_subexp (struct expression *exp, int *pos,
		       struct ui_file *stream, enum precedence prec)
{
  unsigned length;
  int pc;
  enum exp_opcode opcode;
  char * sym_name; 

  pc = (*pos)++;
  opcode = exp->elts[pc].opcode;
  switch (opcode)
    {
      /* Common ops, specialized Modula-3 versions: */

    case OP_VAR_VALUE:
      { struct block *b;
	(*pos) += 3;
#if 0
        /* This code was present when the OP_VAR_VALUE case in
           print_subexp_standard was specialized for Modula-3.
           It is unneeded for now, but I wonder if it might be
           wanted someday to print qualified names? */  
        b = exp->elts[pc + 1].block;
	if (b != NULL
	    && BLOCK_FUNCTION (b) != NULL
	    && SYMBOL_PRINT_NAME (BLOCK_FUNCTION (b)) != NULL) {

            if (exp->language_defn->la_language != language_m3) {
	      fputs_filtered (SYMBOL_PRINT_NAME (BLOCK_FUNCTION (b)), stream);
	      fputs_filtered ("::", stream);
            }
	}
#endif
        sym_name = SYMBOL_PRINT_NAME (exp->elts[pc + 2].symbol);
        if ((sym_name[0] == 'I' || sym_name[0] == 'M') &&
             sym_name[1] == '$')
	  fputs_filtered (sym_name+2, stream);
        else
	  fputs_filtered (sym_name, stream);
	}
      return;

    /* Modula-3-specific ops: */

    case OP_M3_LONG:
    case OP_M3_CHAR:
    case OP_M3_WIDECHAR:
      (*pos) += 3;
      value_print ( m3_value_from_longest ( exp->elts[pc + 1].type,
			      	            exp->elts[pc + 2].longconst),
		   stream, 0, Val_no_prettyprint);
      return;

    case OP_M3_REEL:
    case OP_M3_LREEL:
    case OP_M3_XREEL:
      (*pos) += 3;
      value_print (value_from_double (exp->elts[pc + 1].type,
				      exp->elts[pc + 2].doubleconst),
		   stream, 0, Val_no_prettyprint);
      return;

    case OP_M3_WIDETEXT: 
      /* like OP_STRING */
      length = longest_to_int (exp -> elts[pc + 1].longconst);
      (*pos) += 3 + 2 * BYTES_TO_EXP_ELEM (length + 1);
      LA_PRINT_STRING (stream, &exp->elts[pc + 2].string, length, 2, 0);
      return;

    case OP_M3_TEXT: 
      /* like OP_STRING */
      length = longest_to_int (exp -> elts[pc + 1].longconst);
      (*pos) += 3 + BYTES_TO_EXP_ELEM (length + 1);
      LA_PRINT_STRING (stream, &exp->elts[pc + 2].string, length, 1, 0);
      return;

    case M3_FINAL_TYPE:
      print_subexp (exp, pos, stream, PREC_PREFIX);
      return;

    case UNOP_M3_DEREF:
      print_subexp (exp, pos, stream, PREC_SUFFIX);
      fprintf_unfiltered(stream,"^");
      return;
      
    case STRUCTOP_M3_MODULE:
    case STRUCTOP_M3_INTERFACE:
    case STRUCTOP_M3_STRUCT: {
      char *field_name;

      field_name = &exp->elts[pc + 2].string;
      length = longest_to_int (exp->elts[pc + 1].longconst);
      (*pos) += 3 + BYTES_TO_EXP_ELEM (length + 1);
      print_subexp (exp, pos, stream, PREC_PREFIX);
      fprintf_unfiltered(stream,".");
      fputs_filtered (field_name, stream);
      return;
    }

    case BINOP_M3_SUBSCRIPT:
      /* like BINOP_SUBSCRIPT */
      print_subexp (exp, pos, stream, PREC_SUFFIX);
      fprintf_unfiltered(stream,"[");
      print_subexp (exp, pos, stream, PREC_ABOVE_COMMA);
      fprintf_unfiltered(stream,"]");
      return;

    /* All of the UNOP_M3_*s, BINOP_M3_*s, and BINOP_ASSIGN are passed on to
       print_subexp_standard, which can handle them, because it uses the 
       Modula-3 version of la_op_print_tab. */
    default:
      (*pos)--; /* I hate this kludge, but it sure saves code. */
      print_subexp_standard (exp, pos, stream, prec ) ; 
  };
} /* m3_print_subexp */

void
m3_operator_length (struct expression *expr, int endpos,
			  int *oplenp, int *argsp)
{ enum exp_opcode opcode;
  int strlen; 

  if (endpos < 1)
    error ("?bad endpos parameter given to m3_operator_length");
  *oplenp = 1;
  *argsp = 0;
  opcode = expr->elts[endpos - 1].opcode;
  switch (opcode)
  { case OP_M3_LONG:
    case OP_M3_REEL:
    case OP_M3_LREEL:
    case OP_M3_XREEL:
    case OP_M3_CHAR:
    case OP_M3_WIDECHAR:
      *oplenp = 4;
      break;

    case OP_M3_TEXT:
    case OP_M3_WIDETEXT:
      strlen = longest_to_int (expr->elts[endpos - 2].longconst);
      *oplenp = 4 + BYTES_TO_EXP_ELEM (strlen + 1);
      break;

    case OP_M3_TYPE:
      *oplenp = 3;
      break;

    case BINOP_M3_SUBSCRIPT:
    case BINOP_M3_MULT:
    case BINOP_M3_DIVIDE:
    case BINOP_M3_DIV:
    case BINOP_M3_MOD:
    case BINOP_M3_ADD:
    case BINOP_M3_MINUS:
    case BINOP_M3_CAT:
    case BINOP_M3_EQUAL:
    case BINOP_M3_NE:
    case BINOP_M3_LT:
    case BINOP_M3_LE:
    case BINOP_M3_GT:
    case BINOP_M3_GE:
    case BINOP_M3_IN:
    case BINOP_M3_AND:
    case BINOP_M3_OR:
    case BINOP_M3_MAX:
    case BINOP_M3_MIN:
    case BINOP_M3_VAL:
    case BINOP_M3_FLOAT:
    case BINOP_M3_LOOPHOLE:
    case BINOP_ASSIGN: /* I have no idea why this one is not handled in
                          operator_length_standard.  But Modula-3 can 
                          produce it, so Modula-3 will handle it. 
                          rodney.bates@wichita.edu 2005-1-16 */ 
      *argsp = 2;
      break;

    case UNOP_M3_ABS:
    case UNOP_M3_ADR:
    case UNOP_M3_ADRSIZE:
    case UNOP_M3_BITSIZE:
    case UNOP_M3_BYTESIZE:
    case UNOP_M3_CEILING:
    case UNOP_M3_DEREF:
    case UNOP_M3_FIRST:
    case UNOP_M3_FLOOR:
    case UNOP_M3_LAST: 
    case UNOP_M3_NEG:
    case UNOP_M3_NOT:
    case UNOP_M3_NUMBER:
    case UNOP_M3_ORD:
    case UNOP_M3_ROUND:
    case UNOP_M3_TRUNC:
    case M3_FINAL_TYPE:
      *argsp = 1;
      break;

    case STRUCTOP_M3_INTERFACE:
    case STRUCTOP_M3_MODULE:
    case STRUCTOP_M3_STRUCT:
      strlen = longest_to_int (expr->elts[endpos - 2].longconst);
      *oplenp = 4 + BYTES_TO_EXP_ELEM (strlen + 1);
      *argsp = 1;
      break;

    default:
      operator_length_standard (expr, endpos, oplenp, argsp); 
      return; 
    }
} /* m3_operator_length */ 

static char *
m3_op_name (enum exp_opcode opcode)
{ switch (opcode)
    { case OP_M3_LONG: return "OP_M3_LONG";
      case OP_M3_REEL: return "OP_M3_REEL";
      case OP_M3_LREEL: return "OP_M3_LREEL";
      case OP_M3_XREEL: return "OP_M3_XREEL";
      case OP_M3_CHAR: return "OP_M3_CHAR";
      case OP_M3_WIDECHAR: return "OP_M3_WIDECHAR";
      case OP_M3_TEXT: return "OP_M3_TEXT";
      case OP_M3_WIDETEXT: return "OP_M3_WIDETEXT";
      case OP_M3_TYPE: return "OP_M3_TYPE";
      case STRUCTOP_M3_STRUCT: return "STRUCTOP_M3_STRUCT";
      case STRUCTOP_M3_INTERFACE: return "STRUCTOP_M3_INTERFACE";
      case STRUCTOP_M3_MODULE: return "STRUCTOP_M3_MODULE";
      case M3_FINAL_TYPE: return "M3_FINAL_TYPE";
      case UNOP_M3_ABS: return "UNOP_M3_ABS";
      case UNOP_M3_ADR: return "UNOP_M3_ADR";
      case UNOP_M3_ADRSIZE: return "UNOP_M3_ADRSIZE";
      case UNOP_M3_BITSIZE: return "UNOP_M3_BITSIZE";
      case UNOP_M3_BYTESIZE: return "UNOP_M3_BYTESIZE";
      case UNOP_M3_CEILING: return "UNOP_M3_CEILING";
      case UNOP_M3_DEREF: return "UNOP_M3_DEREF";
      case UNOP_M3_FIRST: return "UNOP_M3_FIRST";
      case UNOP_M3_FLOOR: return "UNOP_M3_FLOOR";
      case UNOP_M3_LAST: return "UNOP_M3_LAST";
      case UNOP_M3_NEG: return "UNOP_M3_NEG";
      case UNOP_M3_NOT: return "UNOP_M3_NOT";
      case UNOP_M3_NUMBER: return "UNOP_M3_NUMBER";
      case UNOP_M3_ORD: return "UNOP_M3_ORD";
      case UNOP_M3_ROUND: return "UNOP_M3_ROUND";
      case UNOP_M3_TRUNC: return "UNOP_M3_TRUNC";
      case BINOP_M3_SUBSCRIPT: return "BINOP_M3_SUBSCRIPT";
      case BINOP_M3_MULT: return "BINOP_M3_MULT";
      case BINOP_M3_DIVIDE: return "BINOP_M3_DIVIDE";
      case BINOP_M3_DIV: return "BINOP_M3_DIV";
      case BINOP_M3_MOD: return "BINOP_M3_MOD";
      case BINOP_M3_ADD: return "BINOP_M3_ADD";
      case BINOP_M3_MINUS: return "BINOP_M3_MINUS";
      case BINOP_M3_CAT: return "BINOP_M3_CAT";
      case BINOP_M3_EQUAL: return "BINOP_M3_EQUAL";
      case BINOP_M3_NE: return "BINOP_M3_NE";
      case BINOP_M3_LT: return "BINOP_M3_LT";
      case BINOP_M3_LE: return "BINOP_M3_LE";
      case BINOP_M3_GT: return "BINOP_M3_GT";
      case BINOP_M3_GE: return "BINOP_M3_GE";
      case BINOP_M3_IN: return "BINOP_M3_IN";
      case BINOP_M3_AND: return "BINOP_M3_AND";
      case BINOP_M3_OR: return "BINOP_M3_OR";
      case BINOP_M3_MAX: return "BINOP_M3_MAX";
      case BINOP_M3_MIN: return "BINOP_M3_MIN";
      case BINOP_M3_VAL: return "BINOP_M3_VAL";
      case BINOP_M3_FLOAT: return "BINOP_M3_FLOAT";
      case BINOP_M3_LOOPHOLE: return "BINOP_M3_LOOPHOLE";
      default: return op_name_standard (opcode);
    }
} /* m3_op_name */ 

static int
m3_dump_subexp_body (struct expression *exp, struct ui_file *stream, int elt)
{ int pc = elt;
  enum exp_opcode opcode = exp->elts[elt++].opcode;
  int length;
  char * field_name;
  long longval;   
  
  switch ( opcode ) 
  { case OP_M3_LONG:
    case OP_M3_CHAR:
    case OP_M3_WIDECHAR: 
      fprintf_filtered (stream, "Type @");
      gdb_print_host_address (exp->elts[elt].type, stream);
      fprintf_filtered (stream, " (");
      type_print (exp->elts[elt].type, NULL, stream, 0);
      longval = (long) exp->elts[elt + 1].longconst; 
      fprintf_filtered (stream, "), value %ld (0x%lx)",
			longval, longval);
      elt += 3;
      break;

    case OP_M3_REEL:
    case OP_M3_LREEL:
    case OP_M3_XREEL:
      fprintf_filtered (stream, "Type @");
      gdb_print_host_address (exp->elts[elt].type, stream);
      fprintf_filtered (stream, " (");
      type_print (exp->elts[elt].type, NULL, stream, 0);
      fprintf_filtered (stream, "), value %g",
			(double) exp->elts[elt + 1].doubleconst);
      elt += 3;
      break;

    case OP_M3_TEXT: 
      /* like OP_STRING */
      length = longest_to_int (exp -> elts[pc + 1].longconst);
      elt += 3 + BYTES_TO_EXP_ELEM (length + 1);
      LA_PRINT_STRING (stream, &(exp->elts[pc + 2].string), length, 1, 0);
      break;

    case OP_M3_WIDETEXT: 
      length = longest_to_int (exp -> elts[pc + 1].longconst);
      elt += 3 + 2 * BYTES_TO_EXP_ELEM (length + 1);
      LA_PRINT_STRING (stream, &(exp->elts[pc + 2].string), length, 2, 0);
      break;

    case OP_M3_TYPE:
      fprintf_filtered (stream, "Type @");
      gdb_print_host_address (exp->elts[elt].type, stream);
      fprintf_filtered (stream, " (");
      type_print (exp->elts[elt].type, NULL, stream, 0);
      fprintf_filtered (stream, ") ");
      elt += 2;
      break; 

    case STRUCTOP_M3_MODULE:
    case STRUCTOP_M3_INTERFACE:
    case STRUCTOP_M3_STRUCT: 
      length = longest_to_int (exp->elts[pc + 1].longconst);
      elt += 3 + BYTES_TO_EXP_ELEM (length + 1);
      field_name = &exp->elts[pc + 2].string;
      fprintf_filtered (stream, "Element name: `%.*s'", length, field_name);
      elt = dump_subexp (exp, stream, elt);
      break;

    case M3_FINAL_TYPE:
    case UNOP_M3_ABS:
    case UNOP_M3_ADR:
    case UNOP_M3_ADRSIZE:
    case UNOP_M3_BITSIZE:
    case UNOP_M3_BYTESIZE:
    case UNOP_M3_CEILING:
    case UNOP_M3_DEREF:
    case UNOP_M3_FIRST:
    case UNOP_M3_FLOOR:
    case UNOP_M3_LAST:
    case UNOP_M3_NEG:
    case UNOP_M3_NOT:
    case UNOP_M3_NUMBER:
    case UNOP_M3_ORD:
    case UNOP_M3_ROUND:
    case UNOP_M3_TRUNC:
      elt = dump_subexp ( exp, stream, elt); 
      break; 

    case BINOP_M3_SUBSCRIPT:
    case BINOP_M3_MULT:
    case BINOP_M3_DIVIDE:
    case BINOP_M3_DIV:
    case BINOP_M3_MOD:
    case BINOP_M3_ADD:
    case BINOP_M3_MINUS:
    case BINOP_M3_CAT:
    case BINOP_M3_EQUAL:
    case BINOP_M3_NE:
    case BINOP_M3_LT:
    case BINOP_M3_LE:
    case BINOP_M3_GT:
    case BINOP_M3_GE:
    case BINOP_M3_IN:
    case BINOP_M3_AND:
    case BINOP_M3_OR:
    case BINOP_M3_MAX:
    case BINOP_M3_MIN:
    case BINOP_M3_VAL:
    case BINOP_M3_FLOAT:
    case BINOP_M3_LOOPHOLE:
      elt = dump_subexp ( exp, stream, elt); 
      elt = dump_subexp ( exp, stream, elt); 
      break; 

    default:
      elt--; /* I hate this kludge, but it sure saves code. */
      return dump_subexp_body_standard (exp, stream, elt);
  } 
  return elt; 
} /* m3_dump_subexp_body */ 

static LONGEST
m3_div (a, b)
  LONGEST a, b;
{
  if (a == 0) { return 0; }
  if (a < 0) {
    return (b < 0)
             ? ((-a) / (-b))
	     : (- ((-a-1) / b) - 1);
  } else {
    return (b < 0)
             ? (- ((a - 1) / (-b)) - 1)
             : (a / b);
  }
} /* m3_div */

static LONGEST
m3_modi (a, b)
  LONGEST a, b;
{
  if (a == 0) { return 0; }
  if (a < 0) {
    return (b < 0)
             ? (- ((-a) % (-b)))
             : (b - 1 - ((-a-1) % b));
  } else {
    return (b < 0)
             ? (b + 1 + ((a - 1) % (-b)))
             : (a % b);
  }
} /* m3_modi */

static double
m3_modf (a, b)
  double a, b;
{
  double  z = a / b;
  LONGEST zi = (LONGEST) z;
  if ((z < 0.0) && ((double)zi != z)) { zi--; }
  return a - b * (double)zi;
} /* m3_modf */

/* Simulate the Modula-3  operator = by returning a 1
   iff ARG1 and ARG2 have equal contents.  */

static int
m3_value_equal (struct value *arg1, struct value *arg2) 
{
  int len;
  const char * p1, * p2;
  enum type_code code1;
  enum type_code code2;

  coerce_array (arg1);
  coerce_array (arg2);

  code1 = TYPE_CODE (value_type (arg1));
  code2 = TYPE_CODE (value_type (arg2));

  if (code1 == TYPE_CODE_M3_INTEGER) { code1 = TYPE_CODE_INT; }
  if (code2 == TYPE_CODE_M3_INTEGER) { code2 = TYPE_CODE_INT; }
  if (code1 == TYPE_CODE_M3_POINTER) { code1 = TYPE_CODE_PTR; }
  if (code2 == TYPE_CODE_M3_POINTER) { code2 = TYPE_CODE_PTR; }

  if (code1 == TYPE_CODE_M3_INTEGER && code2 == TYPE_CODE_M3_INTEGER)
    return m3_value_as_integer (arg1) == m3_value_as_integer (arg2);
  else if (code1 == TYPE_CODE_FLT && code2 == TYPE_CODE_M3_INTEGER)
    return m3_value_as_float (arg1) == (double) m3_value_as_integer (arg2);
  else if (code2 == TYPE_CODE_FLT && code1 == TYPE_CODE_M3_INTEGER)
    return m3_value_as_float (arg2) == (double) m3_value_as_integer (arg1);
  else if (code1 == TYPE_CODE_FLT && code2 == TYPE_CODE_FLT)
    return m3_value_as_float (arg1) == m3_value_as_float (arg2);

  /* FIXME: Need to promote to either CORE_ADDR or LONGEST, whichever
     is bigger.  */
  else if (code1 == TYPE_CODE_M3_POINTER && code2 == TYPE_CODE_M3_INTEGER)
    return m3_value_as_address (arg1) == (CORE_ADDR) m3_value_as_integer (arg2);
  else if (code2 == TYPE_CODE_M3_POINTER && code1 == TYPE_CODE_M3_INTEGER)
    return (CORE_ADDR) m3_value_as_integer (arg1) == m3_value_as_address (arg2);

  else if (code1 == code2
           && ((len = TYPE_LENGTH (value_type (arg1)))
               == TYPE_LENGTH (value_type (arg2))))
    {
      p1 = value_contents (arg1);
      p2 = value_contents (arg2);
      while (--len >= 0)
        {
          if (*p1++ != *p2++)
            break;
        }
      return len < 0;
    }
  else
    {
      error ("Invalid type combination in equality test.");
      return 0;  /* For lint -- never reached */
    }
}

bool
m3_types_equal ( struct type * left, struct type * right ); 

bool m3_type_fields_equal ( struct type * left, struct type * right ) 

{ int i;

  if ( TYPE_NFIELDS ( left ) != TYPE_NFIELDS ( right ) )
    { return false; } 
  for ( i = 0; i < TYPE_NFIELDS ( left ); i ++ ) 
    { if ( ! m3_types_equal 
               ( TYPE_M3_FIELD_TYPE ( left, i ), 
                 TYPE_M3_FIELD_TYPE ( right, i ) 
               ) 
         ) { return false; } 
    } 
  return true; 
} 

int is_m3_type ( struct type * m3_type ) 

  { 
    switch (TYPE_CODE ( m3_type ) )
      { case TYPE_CODE_M3_ARRAY:
        case TYPE_CODE_M3_OPEN_ARRAY:
        case TYPE_CODE_M3_PACKED:
        case TYPE_CODE_M3_ENUM:
        case TYPE_CODE_M3_INDIRECT:
        case TYPE_CODE_M3_OBJECT: 
        case TYPE_CODE_M3_PROC:
        case TYPE_CODE_M3_METHOD: 
        case TYPE_CODE_M3_RECORD:
        case TYPE_CODE_M3_SET:
        case TYPE_CODE_M3_POINTER: 
        case TYPE_CODE_M3_SUBRANGE: 
        case TYPE_CODE_M3_ADDRESS:
        case TYPE_CODE_M3_BOOLEAN:
        case TYPE_CODE_M3_CHAR:
        case TYPE_CODE_M3_INTEGER:
        case TYPE_CODE_M3_CARDINAL:
        case TYPE_CODE_M3_REFANY:
        case TYPE_CODE_M3_TRANSIENT_REFANY:
        case TYPE_CODE_M3_MUTEX:
        case TYPE_CODE_M3_NULL:
        case TYPE_CODE_M3_ROOT:
        case TYPE_CODE_M3_TRANSIENT_ROOT:
        case TYPE_CODE_M3_TEXT:
        case TYPE_CODE_M3_UN_ROOT:
        case TYPE_CODE_M3_VOID:
          return 1; 
        default:
          return 0; 
      }
  } /* is_m3_type */ 

/* Strip away any indirect types from a type. */ 
static struct type * 
m3_direct_type ( struct type * param_type ) 

  { struct type * l_type; 

    if ( param_type == NULL ) { return NULL; } 
    l_type = param_type; 
    while ( TYPE_CODE ( l_type ) == TYPE_CODE_M3_INDIRECT ) 
      { l_type = TYPE_TARGET_TYPE ( l_type ); } 
    return l_type; 
  } /* m3_direct_type */ 

static struct  type *  
m3_revealed_type ( struct type * opaque_type ) 

  { if ( opaque_type != NULL 
         && TYPE_CODE ( opaque_type ) == TYPE_CODE_M3_OPAQUE 
       ) 
      { return TYPE_M3_OPAQUE_REVEALED ( opaque_type ); } 
    return opaque_type; 
  } /* m3_revealed_type */ 

bool
m3_types_equal ( struct type * left, struct type * right ) 

{ struct type * left_direct; 
  struct type * right_direct; 
  int i; 

  if ( left == NULL || right == NULL ) { return false; } 
  left_direct = m3_direct_type ( left ); 
  right_direct = m3_direct_type ( right ); 
  left_direct = m3_revealed_type ( left ); 
  right_direct = m3_revealed_type ( right ); 
  if ( TYPE_CODE ( left_direct ) != TYPE_CODE ( right_direct ) ) 
    { return false; } 
  switch ( TYPE_CODE ( left_direct ) ) 
    { case TYPE_CODE_M3_ARRAY : 
        return m3_type_fields_equal ( left_direct, right_direct ); 
      case TYPE_CODE_M3_OPEN_ARRAY : 
        return m3_type_fields_equal ( left_direct, right_direct ); 
      case TYPE_CODE_M3_ENUM : 
        return m3_type_fields_equal ( left_direct, right_direct ); 
      case TYPE_CODE_M3_PACKED :
        return m3_type_fields_equal ( left_direct, right_direct ); 
      case TYPE_CODE_M3_SET : 
        return m3_type_fields_equal ( left_direct, right_direct ); 
      case TYPE_CODE_M3_SUBRANGE : 
        if ( ! m3_type_fields_equal ( left_direct, right_direct ) ) 
          { return false; } 
        return 
          TYPE_M3_SUBRANGE_MIN ( left_direct ) 
          == TYPE_M3_SUBRANGE_MIN ( right_direct )  
          && TYPE_M3_SUBRANGE_MAX ( left_direct ) 
             == TYPE_M3_SUBRANGE_MAX ( right_direct );
      case TYPE_CODE_M3_POINTER : 
        if ( ! m3_type_fields_equal ( left_direct, right_direct ) ) 
          { return false; } 
        if ( TYPE_M3_POINTER_TRACED ( left_direct ) 
             != TYPE_M3_POINTER_TRACED ( right_direct ) 
           )
          { return false; } 
        if ( TYPE_M3_POINTER_BRANDED ( left_direct ) 
             != TYPE_M3_POINTER_BRANDED ( right_direct ) 
           )
          { return false; } 
        if ( TYPE_M3_POINTER_BRANDED ( left_direct ) ) 
          { return TYPE_M3_POINTER_BRAND ( left_direct ) 
                   == TYPE_M3_POINTER_BRAND ( right_direct );
          } 
        return true;  
      case TYPE_CODE_M3_OPAQUE : 
        return false; /* Shouldn't happen. */ 

      case TYPE_CODE_M3_RECORD : 
        if ( ! m3_type_fields_equal ( left_direct, right_direct ) ) 
          { return false; } 
        for ( i = 0; i < TYPE_NFIELDS ( left_direct ); i ++ ) 
          { if ( TYPE_FIELD_NAME ( left_direct, i ) 
                 != TYPE_FIELD_NAME ( right_direct, i ) 
               ) 
              { return false; }
            if ( TYPE_M3_REC_FIELD_BITPOS ( left_direct, i ) 
                 != TYPE_M3_REC_FIELD_BITPOS ( right_direct, i ) 
               ) 
              { return false; }
            if ( TYPE_M3_REC_FIELD_BITSIZE ( left_direct, i ) 
                 != TYPE_M3_REC_FIELD_BITSIZE ( right_direct, i ) 
               ) 
              { return false; }
          } 
        return true; 
      case TYPE_CODE_M3_OBJECT : 
        if ( TYPE_M3_OBJ_NMETHODS ( left_direct ) 
             != TYPE_M3_OBJ_NMETHODS ( right_direct) 
           )
          { return false; } 
        if ( ! m3_type_fields_equal ( left_direct, right_direct ) ) 
          { return false; } 
        for ( i = 0; i < TYPE_NFIELDS ( left_direct ); i ++ ) 
          { if ( TYPE_FIELD_NAME ( left_direct, i ) 
                 != TYPE_FIELD_NAME ( right_direct, i ) 
               ) 
              { return false; }
            if ( TYPE_M3_OBJ_FIELD_BITPOS ( left_direct, i ) 
                 != TYPE_M3_OBJ_FIELD_BITPOS ( right_direct, i ) 
               ) 
              { return false; }
            if ( TYPE_M3_OBJ_FIELD_BITSIZE ( left_direct, i ) 
                 != TYPE_M3_OBJ_FIELD_BITSIZE ( right_direct, i ) 
               ) 
              { return false; }
          } 
        if ( TYPE_M3_OBJ_TRACED ( left_direct ) 
             != TYPE_M3_OBJ_TRACED ( right_direct ) 
           )
          { return false; } 
        if ( TYPE_M3_OBJ_BRANDED ( left_direct ) 
             != TYPE_M3_OBJ_BRANDED ( right_direct ) 
           )
          { return false; } 
        if ( TYPE_M3_OBJ_BRANDED ( left_direct ) ) 
          { return TYPE_M3_OBJ_BRAND ( left_direct ) 
                   == TYPE_M3_OBJ_BRAND ( right_direct );
          } 
        return true;  

      case TYPE_CODE_M3_PROC : 
      case TYPE_CODE_M3_METHOD : 
        if ( TYPE_M3_PROC_NRAISES ( left_direct ) 
             != TYPE_M3_PROC_NRAISES ( right_direct ) 
           )
          { return false; } 
        if ( ! m3_type_fields_equal ( left_direct, right_direct ) ) 
          { return false; } 
        for ( i = 0; i < TYPE_NFIELDS ( left_direct ); i ++ ) 
          { if ( TYPE_FIELD_NAME ( left_direct, i ) 
                 != TYPE_FIELD_NAME ( right_direct, i ) 
               ) 
              { return false; }
          } 
        return true; 

      case TYPE_CODE_M3_ADDRESS : 
      case TYPE_CODE_M3_BOOLEAN : 
      case TYPE_CODE_M3_CHAR : 
      case TYPE_CODE_M3_WIDECHAR : 
      case TYPE_CODE_M3_INTEGER : 
      case TYPE_CODE_M3_CARDINAL : 
      case TYPE_CODE_M3_REFANY : 
      case TYPE_CODE_M3_TRANSIENT_REFANY : 
      case TYPE_CODE_M3_ROOT : 
      case TYPE_CODE_M3_TRANSIENT_ROOT : 
      case TYPE_CODE_M3_UN_ROOT : 
      case TYPE_CODE_M3_MUTEX : 
      case TYPE_CODE_M3_TEXT : 
      case TYPE_CODE_M3_NULL : 
      case TYPE_CODE_M3_VOID : 
        /* There is only one type with each of these code. */ 
        return TYPE_CODE ( left_direct ) == TYPE_CODE ( right_direct ); 
      default : { return false; } 
    } /* switch */ 
} /* m3_types_equal */

/* This system is crafted so that (hopefully), 
   if m3_type_code_tier ( tc1 ) < m3_type_code_tier ( tc2 ), 
   then, loosly, m3_subtype_relation ( tc1, tc2 ) != subtype_super.
   Also, the reference types are all at the high end and ordered 
   such that if tc1 is reference type code and 
   m3_type_code_tier ( tc1 ) < m3_type_code_tier ( tc2 )
   then loosly, m3_subtype_relation ( tc1, tc2 ) == subtype_sub,
   except for the different object types. 

   It allows ordering of types by their code for subtype checking, 
   so as to cut down on the number of elements of the cartesion square 
   of type codes that have to be explicitly coded.  Don't try this on 
   TYPE_CODE_M3_INDIRECT, which can't be made to fit such a system. */ 

static int 
m3_type_code_tier ( enum type_code code ) 

  { switch ( code ) 
      { case TYPE_CODE_M3_PACKED :
          return 0; 
        case TYPE_CODE_M3_SET : 
        case TYPE_CODE_M3_OPAQUE : /* Probably shouldn't happen. */  
        case TYPE_CODE_M3_RECORD : 
        case TYPE_CODE_M3_METHOD : 
        case TYPE_CODE_M3_VOID : 
          return 1; 
        case TYPE_CODE_M3_OPEN_ARRAY : 
          return 2; 
        case TYPE_CODE_M3_ARRAY : 
          return 3; 
        case TYPE_CODE_M3_SUBRANGE : 
          return 4; 
        case TYPE_CODE_M3_BOOLEAN : 
        case TYPE_CODE_M3_CHAR : 
        case TYPE_CODE_M3_WIDECHAR : 
        case TYPE_CODE_M3_INTEGER : 
        case TYPE_CODE_M3_CARDINAL : 
        case TYPE_CODE_M3_ENUM : 
          return 5; 
        case TYPE_CODE_M3_PROC : 
        case TYPE_CODE_M3_PROC_CLOSURE :
          return 6; 
        case TYPE_CODE_M3_REFANY : 
        case TYPE_CODE_M3_TRANSIENT_REFANY : 
        case TYPE_CODE_M3_ADDRESS : 
          return 7;  
        case TYPE_CODE_M3_TEXT : 
          return 8; 
        case TYPE_CODE_M3_ROOT : 
        case TYPE_CODE_M3_TRANSIENT_ROOT : 
        case TYPE_CODE_M3_UN_ROOT : 
        case TYPE_CODE_M3_POINTER : 
          return 9; 
        case TYPE_CODE_M3_MUTEX : 
          return 10; 
        case TYPE_CODE_M3_OBJECT : 
          return 11; 
        case TYPE_CODE_M3_NULL : 
          return 12; 
        default : 
          return 1;  
      } /* switch */ 
  } /* m3_type_code_tier */ 

enum subtype_rel 
  { subtype_sub, subtype_equal, subtype_super, subtype_both, subtype_norel };

static enum subtype_rel 
reverse_subtype_relation ( enum subtype_rel param_rel ) 

  { switch ( param_rel ) 
      { case subtype_sub : return subtype_super; 
        case subtype_super : return subtype_sub; 
        default: return param_rel; 
      } 
  } /* reverse_subtype_relation */ 

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

/* Does not strip off indirects, unless it's a real value of reference type. */
static struct type * 
m3_allocated_type ( struct value * val ) 

  { struct type * val_type; 
    struct type * direct_type; 
    struct type * result_type; 
    CORE_ADDR val_contents; 

    val_type = value_type ( val ) ; 
    if ( * ( LONGEST * ) value_contents ( val ) == m3_type_magic_value ) 
      { return val_type; } 
    else 
      { direct_type = m3_direct_type ( val_type ); 
        direct_type = m3_revealed_type ( val_type ); 
        switch ( TYPE_CODE ( direct_type ) ) 
          { case TYPE_CODE_M3_REFANY:
            case TYPE_CODE_M3_ROOT:
            case TYPE_CODE_M3_OBJECT:
            case TYPE_CODE_M3_TRANSIENT_ROOT:
            case TYPE_CODE_M3_TRANSIENT_REFANY:
              val_contents = value_as_address ( val ); 
              if ( val_contents == 0 ) 
                { return builtin_type_m3_null; } 
              result_type = find_m3_heap_type ( val_contents ); 
              return result_type; 
            default: 
              return val_type; 
          } 
      } 
  } /* m3_allocated_type*/ 

/* Call this only if left_type is known to be an ordinal type. */ 
static enum subtype_rel
m3_ordinal_subtype_relation ( 
    struct type * left_type, 
    struct type * left_base_type, 
    struct type * right_type
  ) 

  { enum type_code left_base_code; 
    enum type_code right_base_code; 
    struct type * right_base_type;
    LONGEST left_lower; 
    LONGEST left_upper; 
    LONGEST right_lower; 
    LONGEST right_upper; 

    switch ( TYPE_CODE ( right_type ) ) 
      { case TYPE_CODE_M3_SUBRANGE : 
          right_base_type = TYPE_M3_SUBRANGE_TARGET ( right_type ); 
        case TYPE_CODE_M3_BOOLEAN : 
        case TYPE_CODE_M3_CHAR : 
        case TYPE_CODE_M3_WIDECHAR : 
        case TYPE_CODE_M3_INTEGER : 
        case TYPE_CODE_M3_CARDINAL : 
        case TYPE_CODE_M3_ENUM : 
          right_base_type = right_type;
        default : 
          return subtype_norel; 
      } 
    left_base_code = TYPE_CODE ( left_base_type ); 
    right_base_code = TYPE_CODE ( right_base_type ); 
    if ( left_base_code != right_base_code ) 
      { return subtype_norel; } 
    if ( left_base_code == TYPE_CODE_M3_ENUM ) 
      { if ( ! m3_type_fields_equal ( left_base_type, right_base_type ) ) 
          { return subtype_norel; } 
      } 
    /* Here, we know both base types are equal. */   
    if ( left_type == left_base_type && right_type == right_base_type ) 
      { return subtype_equal; } 
    /* Here, at least one is a subrange. */ 
    m3_ordinal_bounds ( left_type , & left_lower, & left_upper );  
    m3_ordinal_bounds ( right_type , & right_lower, & right_upper );  
    if ( left_lower == right_lower && left_upper == right_upper ) 
      /* Bounds are equal. */ 
      { if ( left_type != left_base_type && right_type != right_base_type ) 
          /* Both are subranges. */ 
          { return subtype_equal; } 
        else 
          { return subtype_both; }
      }  
    if ( left_lower <= right_lower && left_upper >= right_upper ) 
      { return subtype_super; } /* Left is supertype. */ 
    if ( left_lower >= right_lower && left_upper <= right_upper ) 
      { return subtype_sub; } /* Left is subtype. */ 
    return subtype_norel; 
  } /* m3_ordinal_subtype_relation */ 

static bool
m3_equal_object_types ( struct type * left, struct type * right ) 

  { enum type_code left_code;  
  int i; 

    left_code = TYPE_CODE ( left );  
    switch ( left_code ) 
      { case TYPE_CODE_M3_REFANY : 
        case TYPE_CODE_M3_TRANSIENT_REFANY : 
        case TYPE_CODE_M3_ADDRESS : 
        case TYPE_CODE_M3_ROOT : 
        case TYPE_CODE_M3_TRANSIENT_ROOT : 
        case TYPE_CODE_M3_UN_ROOT : 
        case TYPE_CODE_M3_NULL : 
        case TYPE_CODE_M3_MUTEX : 
        case TYPE_CODE_M3_TEXT : 
          return left_code == TYPE_CODE ( right );  

        case TYPE_CODE_M3_OBJECT : 
          if ( left_code != TYPE_CODE ( right ) ) 
            { return false; } 
          if ( TYPE_M3_OBJ_TRACED ( left ) != TYPE_M3_OBJ_TRACED ( right ) )
            { return false; } 
          if ( TYPE_M3_OBJ_BRANDED ( left ) != TYPE_M3_OBJ_BRANDED ( right ) )
            { return false; } 
          if ( TYPE_M3_OBJ_BRANDED ( left ) 
               && TYPE_M3_OBJ_BRAND ( left ) != TYPE_M3_OBJ_BRAND ( right )
             ) 
            { return false; } 
          if ( TYPE_M3_OBJ_NFIELDS ( left ) != TYPE_M3_OBJ_NFIELDS ( right ) )
            { return false; } 
          if ( TYPE_M3_OBJ_NMETHODS ( left ) != TYPE_M3_OBJ_NMETHODS ( right ) )
            { return false; } 
          for ( i = 0; i < TYPE_M3_OBJ_NMETHODS ( left ); i ++ )
            { if ( strcmp ( TYPE_M3_OBJ_METHOD_NAME ( left, i ), 
                            TYPE_M3_OBJ_METHOD_NAME ( right, i )
                          ) 
                   != 0  
                 ) 
                { return false; }
              if ( ! m3_types_equal 
                       ( TYPE_M3_OBJ_METHOD_TYPE ( left, i ), 
                         TYPE_M3_OBJ_METHOD_TYPE ( right, i ) 
                       ) 
                 ) 
                { return false; }
            } 
          for ( i = 0; i < TYPE_M3_OBJ_NFIELDS ( left ); i ++ ) 
            { if ( strcmp ( TYPE_M3_OBJ_FIELD_NAME ( left, i ), 
                            TYPE_M3_OBJ_FIELD_NAME ( right, i ) 
                          )
                   != 0 
                 ) 
                { return false; }
              if ( ! m3_types_equal 
                       ( TYPE_M3_OBJ_FIELD_TYPE ( left, i ), 
                         TYPE_M3_OBJ_FIELD_TYPE ( right, i ) 
                       ) 
                 ) 
                { return false; }
            } 
          return m3_equal_object_types  
                   ( TYPE_M3_OBJ_SUPER ( left ), TYPE_M3_OBJ_SUPER ( right ) ); 
        default:
          return false; 
      } 

  }  /* m3_equal_object_types */ 

/* Return the number of reference type ancestors start_type has. */ 
static int 
reference_type_depth ( struct type * start_type ) 

{ struct type * l_type; 
  int result; 

  result = 0; 
  l_type = start_type; 
  while ( true ) 
    { if ( l_type == NULL ) 
        { return result; } 
      switch ( TYPE_CODE ( l_type ) )  
        { case TYPE_CODE_M3_REFANY : 
          case TYPE_CODE_M3_TRANSIENT_REFANY : 
          case TYPE_CODE_M3_ADDRESS : 
            return result;
          case TYPE_CODE_M3_ROOT : 
          case TYPE_CODE_M3_TRANSIENT_ROOT : 
          case TYPE_CODE_M3_UN_ROOT : 
            return result + 1;
          case TYPE_CODE_M3_TEXT : 
            return result + m3_is_cm3 ( ); 

          case TYPE_CODE_M3_MUTEX : 
            return result + 2; 

          case TYPE_CODE_M3_OBJECT :
            l_type = TYPE_M3_OBJ_SUPER ( l_type ); 
            result ++; 
            break; 
            /* and loop */  

          default: /* Shouldn't happen. */ 
            return result; 
        }
    } /* while */ 
} /* reference_type_depth */ 

/* Is this type TEXT or the PM3 revelation of TEXT, namely 
   BRANDED "Text-1.0" REF ARRAY OF CHAR? 
*/ 
static bool
is_pm3_text_revelation ( struct type * text_type ) 

{ struct type * array_type; 
  struct type * elem_type; 

  if ( text_type == NULL ) { return false; } 
  switch ( TYPE_CODE ( text_type ) ) 
    { case TYPE_CODE_M3_TEXT: 
        return true; 
      case TYPE_CODE_M3_POINTER:
        if ( ! TYPE_M3_POINTER_BRANDED ( text_type ) )  
          { return false; } 
        if ( strcmp ( TYPE_M3_POINTER_BRAND ( text_type ), "Text-1.0" ) != 0 )
          { return false; } 
        array_type = TYPE_M3_POINTER_TARGET ( text_type ); 
        if ( array_type == NULL 
             || TYPE_CODE ( array_type ) != TYPE_CODE_M3_OPEN_ARRAY 
           ) 
          { return false; } 
        elem_type = TYPE_M3_OPEN_ARRAY_ELEM ( array_type ); 
        if ( elem_type == NULL 
             || TYPE_CODE ( elem_type ) != TYPE_CODE_M3_CHAR  
           ) 
          { return false; } 
        return true; 
      default: 
        return false; 
    } 
} /* is_pm3_text_revelation */ 

static enum subtype_rel 
m3_subtype_relation ( struct type * left, struct type * right ) 

  { struct type * left_direct; 
    struct type * right_direct; 
    enum type_code left_code; 
    enum type_code right_code; 
    int left_tier; 
    int right_tier; 
    enum subtype_rel child_rel; 
    LONGEST lower; 
    LONGEST upper; 

    if ( left == NULL || right == NULL ) { return subtype_norel; } 
    left_direct = m3_direct_type ( left ); 
    right_direct = m3_direct_type ( right ); 
    left_direct = m3_revealed_type ( left ); 
    right_direct = m3_revealed_type ( right ); 

    if ( left_direct == right_direct ) { return subtype_equal; } 
    if ( m3_types_equal ( left_direct, right_direct ) ) { return subtype_equal; }
    left_code = TYPE_CODE ( left_direct ); 
    right_code = TYPE_CODE ( right_direct ); 
    /* Swap operands so that if the type codes differ, left will not be the 
       supertype only. */ 
    left_tier = m3_type_code_tier ( left_code );
    right_tier = m3_type_code_tier ( right_code ); 
    if ( left_tier > right_tier ) 
      { return reverse_subtype_relation 
                 ( m3_subtype_relation ( right_direct, left_direct ) ); 
      } 

    switch ( left_code ) 
      { case TYPE_CODE_M3_PACKED :
          child_rel 
            = m3_subtype_relation 
                ( TYPE_M3_PACKED_TARGET ( left_direct ) , right_direct ); 
          if ( child_rel == subtype_equal 
               && right_code != TYPE_CODE_M3_PACKED 
             ) 
            { return subtype_both; } 
          else { return child_rel; } 

        case TYPE_CODE_M3_OPAQUE : 
          /* Shouldn't get here. */
          return subtype_norel;  

        case TYPE_CODE_M3_SUBRANGE : 
          return m3_ordinal_subtype_relation 
                   ( left_direct, 
                     TYPE_M3_SUBRANGE_TARGET ( left_direct ), 
                     right_direct 
                   ); 

        case TYPE_CODE_M3_BOOLEAN : 
        case TYPE_CODE_M3_CHAR : 
        case TYPE_CODE_M3_WIDECHAR : 
        case TYPE_CODE_M3_INTEGER : 
        case TYPE_CODE_M3_CARDINAL : 
        case TYPE_CODE_M3_ENUM : 
          return m3_ordinal_subtype_relation 
                   ( left_direct, left_direct, right_direct ); 

        case TYPE_CODE_M3_REFANY : 
        case TYPE_CODE_M3_TRANSIENT_REFANY : 
        case TYPE_CODE_M3_ADDRESS : 
        case TYPE_CODE_M3_ROOT : 
        case TYPE_CODE_M3_TRANSIENT_ROOT : 
        case TYPE_CODE_M3_UN_ROOT : 
        case TYPE_CODE_M3_NULL : 
          if ( left_code == right_code )
            { return subtype_equal; } 
          else if ( left_tier < right_tier ) 
            { return subtype_sub; } 
          else { return subtype_norel; }  

        case TYPE_CODE_M3_POINTER : 
          if ( right_code != left_code ) 
            { return subtype_norel; } 
          else if ( ! m3_type_fields_equal ( left_direct, right_direct ) ) 
            { return subtype_norel; } 
          else if ( TYPE_M3_POINTER_TRACED ( left_direct ) 
                    != TYPE_M3_POINTER_TRACED ( right_direct ) 
                  )
            { return subtype_norel; } 
          else if ( TYPE_M3_POINTER_BRANDED ( left_direct ) 
                    != TYPE_M3_POINTER_BRANDED ( right_direct ) 
                  )
            { return subtype_norel; } 
          else if ( TYPE_M3_POINTER_BRANDED ( left_direct ) 
                    && strcmp ( TYPE_M3_POINTER_BRAND ( left_direct ), 
                                TYPE_M3_POINTER_BRAND ( right_direct )
                              ) 
                       != 0 
                  ) 
            { return subtype_norel; } 
          else { return subtype_norel; } 

        case TYPE_CODE_M3_TEXT : 
          if ( ! m3_is_cm3 ( ) )  
            { if ( left_code == right_code )
                { return subtype_equal; } 
            if ( is_pm3_text_revelation ( left_direct ) ) 
              { return subtype_sub; } 
              else { return subtype_norel; }  
            } 
          /* else for CM3, fall through to object case. */ 

        case TYPE_CODE_M3_MUTEX : 
        case TYPE_CODE_M3_OBJECT : 
          switch ( right_code ) 
            { case TYPE_CODE_M3_NULL: 
                return subtype_sub; 
              case TYPE_CODE_M3_MUTEX : 
              case TYPE_CODE_M3_OBJECT: 
                { int left_depth; 
                  int right_depth; 
                  struct type * left_super; 
                  struct type * right_super; 

                  left_depth = reference_type_depth ( left_direct ); 
                  right_depth = reference_type_depth ( right_direct ); 
                  left_super = left_direct; 
                  right_super = right_direct; 
                  if ( left_depth < right_depth ) 
                    { while ( left_depth < right_depth ) 
                        { right_super = TYPE_M3_OBJ_SUPER ( right_super ); 
                          right_depth --; 
                        } 

                      if ( m3_equal_object_types ( left_super, right_super ) ) 
                        { return subtype_super; }
                    } 
                  else if ( left_depth > right_depth ) 
                    { while ( left_depth > right_depth ) 
                        { left_super = TYPE_M3_OBJ_SUPER ( left_super ); 
                          left_depth --; 
                        } 
                      if ( m3_equal_object_types ( left_super, right_super ) ) 
                        { return subtype_sub; }
                    } 
                  else if ( m3_equal_object_types ( left_super, right_super ) ) 
                    { return subtype_equal; }
                  return subtype_norel;  
                } 
              default: /* Shouldn't happen because of tiers. */ 
                return subtype_norel; 
            } 

        case TYPE_CODE_M3_ARRAY : 
        case TYPE_CODE_M3_OPEN_ARRAY : 
        case TYPE_CODE_M3_PROC : 
        case TYPE_CODE_M3_PROC_CLOSURE :
          error ( "Subtype relation not implemented for array or procedure, "
                  "types." 
                ); /* NORETURN */  

        case TYPE_CODE_M3_SET : 
        case TYPE_CODE_M3_RECORD : 
        case TYPE_CODE_M3_METHOD :
          if ( m3_types_equal ( left_direct, right_direct ) )  
            { return subtype_equal; } 
          else { return subtype_norel; } 

        case TYPE_CODE_M3_VOID : 
          return right_code == left_code; 

        default : { return subtype_norel; } 
      } /* switch ( left_code ) */ 
  } /* m3_subtype_relation */ 

/* For a Modula-3 type, we will construct only one indirect type to it.  
   So reuse existing pointer-type mechanism from C/C++, but change
   its type code.  
   This could duplicate a compiler-generated type, but that would be
   hard to find. */ 
struct type *
lookup_m3_indirect_type (struct type *type)
{ struct type * result; 

  result = make_pointer_type (type, (struct type **) 0 );
  TYPE_CODE ( result ) = TYPE_CODE_M3_INDIRECT; 
  return result; 
}

/* Where static link is stored, relative to frame locals. */ 
/* TODO:  Make this value more dependable: */ 
const int static_link_offset = 0 ; 

static struct frame_info* 
m3_static_parent_frame ( struct frame_info *start_frame) 

{ struct frame_info * frame ; 
  CORE_ADDR static_link; 
  CORE_ADDR frame_base_addr; 

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

        return NULL;
      }  
      frame_base_addr = get_frame_base_address ( frame ); 
    } 
  while ( static_link != frame_base_addr) ;  
  return frame; 
} /* m3_static_parent_frame */ 

/* Given a block, find the first superblock ancestor that is a function block. */
static struct block * 
m3_proc_block ( struct block * blk ) 

  { struct block * l_block; 

    l_block = blk ; 
    while ( l_block != NULL && BLOCK_FUNCTION ( l_block ) == NULL ) 
      { l_block = BLOCK_SUPERBLOCK ( l_block ) ; } 
    return l_block; 
  } /* m3_proc_block */ 

bool use_static_link = true; 

/* Follow static links, as needed, to get the right frame for target_block. */ 
static struct frame_info *
m3_frame_for_block (struct block *target_block )
{
  struct frame_info *l_frame; 
  struct block * l_frame_proc_block; 
  struct block * l_target_proc_block ; 

  CORE_ADDR start;
  CORE_ADDR end;
  CORE_ADDR calling_pc;

  if ( target_block == NULL) { return NULL; } 
  l_frame = deprecated_safe_get_selected_frame ( ); 
  if ( use_static_link ) 
    { /* Find the first frame in the static chain that has the same 
         procedure-level block as target_block.  In a static chain, 
         there can be only one such frame. 
         It might be nice to count the number of static links followed,
         but the number would have to be computed during expression 
         parsing and stored in the OP_VAR_VALUE node, which would have 
         to be changed to M3_OP_VAR_VALUE. */ 
      l_target_proc_block = m3_proc_block ( target_block ); 
      while ( true ) 
        { l_frame_proc_block 
            = m3_proc_block 
                ( get_frame_block ( l_frame , NULL ) 
                  /* ^ Which may not be the right static ancestor block at all,
                       but it doesn't matter, because we skip to the enclosing 
                       procedure block for this block and target_block. */ 
                ); 
          if ( l_frame_proc_block == NULL ) { return NULL; } 
          if ( l_frame_proc_block == l_target_proc_block ) { return l_frame; } 
          l_frame = m3_static_parent_frame ( l_frame );
        } /* while */
    } /* if */ 
  else /* Do it the old way. */  
    { /* Starting with the selected frame and working outward, find the first
         frame that belongs to block.  This is a crude way to locate non-local
         variables/parameters of statically-enclosing procedures of the selected
         frame's procedure.  If procedures are called as procedure constants,
         this should find the right frame.  If something was called as the
         value of a procedure parameter, it may be wrong.  
      */
      start = BLOCK_START ( target_block );
      end = BLOCK_END ( target_block );
      while ( true )
        { if ( l_frame == NULL ) { return NULL; } 
          calling_pc = get_frame_address_in_block ( l_frame );
          if ( start <= calling_pc && calling_pc < end ) { return l_frame; }
          l_frame = get_prev_frame ( l_frame );
        } /* while */ 
    } /* else */ 
} /* m3_frame_for_block */ 

/* Handle value conversion of an ordinal value for either assignment
   or parameter passing.  Returns an appropriate struct value * if 
   the types are ordinal and everything is OK.  Displays an error (and
   thus doesn't return) if ordinal types are involved but something is
   wrong.  Retuns NULL if ordinal types are irrelevant. */  
static struct value * 
m3_check_and_coerce_ordinal ( 
    struct type * lhs_type,
    struct value * rhs_value,
    struct type * rhs_type,
    char * proc_name,
    char * formal_name 
  ) 

  { struct type * lhs_base_type; 
    struct type * rhs_base_type; 
    struct value * result_value; 
    LONGEST lhs_lower; 
    LONGEST lhs_upper; 
    LONGEST rhs_lower; 
    LONGEST rhs_upper; 
    LONGEST contents; 
    bool lhs_is_int_or_card = false; 
    bool rhs_is_int_or_card = false; 

    lhs_base_type = lhs_type; 
    while ( true ) 
      { switch ( TYPE_CODE ( lhs_base_type ) ) 
          { case TYPE_CODE_M3_PACKED : 
              /* The value should still be in packed form, with bitpos and 
                 bitsize fields set. */ 
              lhs_base_type = TYPE_M3_PACKED_TARGET ( lhs_base_type );
              break; /* And loop. */  
            case TYPE_CODE_M3_SUBRANGE :
              lhs_base_type = TYPE_M3_SUBRANGE_TARGET ( lhs_base_type ); 
              lhs_is_int_or_card 
                = TYPE_CODE ( lhs_base_type ) == TYPE_CODE_M3_INTEGER 
                || TYPE_CODE ( lhs_base_type ) == TYPE_CODE_M3_INTEGER; 
              break; /* And loop. */  
            case TYPE_CODE_M3_INTEGER : 
            case TYPE_CODE_M3_CARDINAL : 
              lhs_is_int_or_card = true;
              goto lhs_exit;  
            case TYPE_CODE_M3_BOOLEAN : 
            case TYPE_CODE_M3_CHAR : 
            case TYPE_CODE_M3_WIDECHAR : 
            case TYPE_CODE_M3_ENUM : 
              lhs_is_int_or_card = false;
              goto lhs_exit;  
            default: 
              return NULL; 
          } 
      } 
  lhs_exit: 
    m3_ordinal_bounds ( lhs_type, & lhs_lower, & lhs_upper ); 
    rhs_base_type = rhs_type; 
    while ( true ) 
      { switch ( TYPE_CODE ( rhs_base_type ) ) 
          { case TYPE_CODE_M3_PACKED : 
              /* Unpacking the value will already been taken care of when 
                 evaluating the RHS. */ 
              rhs_base_type = TYPE_M3_PACKED_TARGET ( rhs_base_type );
              break; /* And loop. */  
            case TYPE_CODE_M3_SUBRANGE : 
              rhs_base_type = TYPE_M3_SUBRANGE_TARGET ( rhs_base_type ); 
              break; /* And loop. */ 
            case TYPE_CODE_M3_INTEGER : 
            case TYPE_CODE_M3_CARDINAL : 
              rhs_is_int_or_card = true;
              goto rhs_exit; 
            case TYPE_CODE_M3_BOOLEAN : 
            case TYPE_CODE_M3_CHAR : 
            case TYPE_CODE_M3_WIDECHAR : 
            case TYPE_CODE_M3_ENUM : 
              rhs_is_int_or_card = false;
              goto rhs_exit; 
            default: 
              if ( proc_name == NULL ) 
                { error 
                    ( "Type not assignable to ordinal type." ); /* NORETURN */ 
                } 
              else 
                { error 
                    ( "Type not assignable to ordinal type, " 
                      " formal \"%s\" of procedure \"%s\", ",
                      formal_name, proc_name 
                    ); /* NORETURN */ 
                } 
          } /* switch */  
      } /* while */  
  rhs_exit: 
    m3_ordinal_bounds ( rhs_type, & rhs_lower, & rhs_upper ); 
    if ( lhs_is_int_or_card && rhs_is_int_or_card ) 
      { /* Base types OK */ } 
    else if ( m3_types_equal ( lhs_base_type, rhs_base_type ) )
      { /* Base types OK */ } 
    else 
      { if ( proc_name == NULL ) 
          { error 
              ( "Ordinal types with different base types." ); /* NORETURN */ 
          } 
        else 
          { error 
              ( "Ordinal types with different base types, " 
                " formal \"%s\" of procedure \"%s\", ",
                formal_name, proc_name 
              ); /* NORETURN */ 
          } 
      } 
    contents = m3_value_as_integer ( rhs_value );  
    if ( lhs_lower <= rhs_lower && rhs_upper <= lhs_upper )
      { /* RHS <: LHS. All is well. */ }
    else if ( rhs_upper < lhs_lower || rhs_lower > lhs_upper )  
      { /* Disjoint value sets.  A static error. */ 
        if ( proc_name == NULL ) 
          { error 
              ( "Ordinal types with disjoint ranges." ); /* NORETURN */ 
          } 
        else 
          { error 
              ( "Ordinal types with disjoint ranges, " 
                " formal \"%s\" of procedure \"%s\", ",
                formal_name, proc_name 
              ); /* NORETURN */ 
          } 
      }
    else if ( contents < lhs_lower || lhs_upper < contents ) 
      { /* value not in LHS range. */ 
        if ( proc_name == NULL ) 
          { error 
              ( "Value out of range." ); /* NORETURN */ 
          } 
        else 
          { error 
              ( "Value out of range, " 
                " formal \"%s\" of procedure \"%s\", ",
                formal_name, proc_name 
              ); /* NORETURN */ 
          } 
      }  
    result_value = m3_value_from_longest ( lhs_base_type, contents ); 
    return result_value; 

  } /* m3_check_and_coerce_ordinal */ 

/* Handle value conversion of a reference value for either assignment
   or parameter passing.  Returns an appropriate struct value * if the
   types are reference types and everything is OK.  Displays an error
   (and thus doesn't return) if reference types are involved but
   something is wrong.  Retuns NULL if reference types are
   irrelevant. */  
static struct value * 
m3_check_and_coerce_reference ( 
    struct type * lhs_type,
    struct value * rhs_value,
    struct type * rhs_type,
    char * proc_name,
    char * formal_name 
 ) 

  { enum subtype_rel static_rel; 
    enum subtype_rel allocated_rel; 
    struct type * allocated_type; 
    struct type * lhs_revealed_type; 
    struct type * rhs_revealed_type; 

    switch ( TYPE_CODE ( lhs_revealed_type ) ) 
      { case TYPE_CODE_M3_OBJECT:
        case TYPE_CODE_M3_POINTER:
        case TYPE_CODE_M3_ADDRESS:
        case TYPE_CODE_M3_REFANY:
        case TYPE_CODE_M3_TRANSIENT_REFANY:
        case TYPE_CODE_M3_ROOT:
        case TYPE_CODE_M3_TRANSIENT_ROOT:
        case TYPE_CODE_M3_UN_ROOT:
        case TYPE_CODE_M3_MUTEX:
        case TYPE_CODE_M3_TEXT:
        case TYPE_CODE_M3_NULL:
          /* LHS is a reference type */ 
          switch ( TYPE_CODE ( rhs_revealed_type ) ) 
            { case TYPE_CODE_M3_OBJECT:
              case TYPE_CODE_M3_POINTER:
              case TYPE_CODE_M3_ADDRESS:
              case TYPE_CODE_M3_REFANY:
              case TYPE_CODE_M3_TRANSIENT_REFANY:
              case TYPE_CODE_M3_ROOT:
              case TYPE_CODE_M3_TRANSIENT_ROOT:
              case TYPE_CODE_M3_UN_ROOT:
              case TYPE_CODE_M3_MUTEX:
              case TYPE_CODE_M3_TEXT:
              case TYPE_CODE_M3_NULL:
                /* RHS is a reference type too. */ 
                static_rel 
                   = m3_subtype_relation 
                       ( lhs_revealed_type, rhs_revealed_type );
                switch ( static_rel ) 
                  { case subtype_norel: 
                      break; 
                    case subtype_equal: 
                    case subtype_super: 
                    case subtype_both: /* Can this happen? */
                      /* Statically legal, no runtime check needed. */ 
                      return rhs_value; 
                    case subtype_sub: 
                      allocated_type = m3_allocated_type ( rhs_value ); 
                      allocated_rel 
                        = m3_subtype_relation 
                            ( lhs_revealed_type, allocated_type );
                      switch ( allocated_rel ) 
                        { case subtype_norel: 
                            break; 
                          case subtype_equal: 
                          case subtype_super: 
                          case subtype_both: /* Can this happen? */
                            /* Passes dynamic check, no coercion needed. */ 
                            return rhs_value; 
                          case subtype_sub: 
                            if ( proc_name == NULL ) 
                              { error ( "NARROW failure." ); /* NORETURN */ }  
                            else 
                              { error 
                                 ( "NARROW failure, " 
                                   "formal \"%s\" of procedure \"%s\"",
                                   formal_name, proc_name 
                                 ); /* NORETURN */
                              }  
                        }  
                  }
              default: 
                break; 
            }  
        default:
          return NULL; 
      } 
    if ( proc_name == NULL ) 
      { error ( "Reference type not assignable." ); /* NORETURN */ }  
    else 
      { error 
         ( "Reference type not assignable, " 
           "formal \"%s\" of procedure \"%s\"",
           formal_name, proc_name 
         ); /* NORETURN */
      }  
  } /* m3_check_and_coerce_reference */ 

/* For a fixed or open array type, traverse all the way to the final 
   (non-array) element type and return the stated info. */
static void              
get_array_info ( struct type * array_type, 
                 int * open_dims, 
                 int * fixed_dims, 
                 struct type * * elem_type
               )

{ struct type * l_type = array_type; 
  int l_open_dims = 0; 
  int l_fixed_dims = 0; 

  if ( l_type != NULL ) 
    { while ( true ) 
        { switch ( TYPE_CODE ( l_type ) ) 
            { case TYPE_CODE_M3_ARRAY: 
                l_fixed_dims ++; 
                l_type = TYPE_M3_ARRAY_ELEM ( l_type );
                break;  
              case TYPE_CODE_M3_OPEN_ARRAY: 
                l_open_dims ++; 
                l_type = TYPE_M3_OPEN_ARRAY_ELEM ( l_type ); 
                break; 
              default: 
                if ( open_dims != NULL ) { * open_dims = l_open_dims; } 
                if ( fixed_dims != NULL ) { * fixed_dims = l_fixed_dims; } 
                if ( elem_type != NULL ) { * elem_type = l_type; } 
                return; 
            } 
        }
    } 
} /* get_array_info */ 

/* Handle value conversion of an array value for either assignment or parameter 
   passing.  If lhs_value is non NULL, its element counts for open dimensions
   are shape-checked against the rhs shape.  Otherwise, open dimensions have
   their element counts set from the rhs shape. Returns NULL if either lhs
   or rhs is not an array type. */ 
static struct value * 
m3_check_and_coerce_array ( 
    struct value * lhs_value,
    struct type * lhs_type,
    struct value * rhs_value,
    struct type * rhs_type,
    char * proc_name,
    char * formal_name 
  ) 

  { enum type_code lhs_type_code; 
    enum type_code rhs_type_code; 
    int lhs_open_dims;
    int lhs_fixed_dims;
    int rhs_open_dims;
    int rhs_fixed_dims;
    struct type * lhs_elem_type; 
    struct type * rhs_elem_type; 
    struct value * result;
    CORE_ADDR array_addr; 
    struct type * l_lhs_type = lhs_type; 
    struct type * l_rhs_type = rhs_type; 
    int dimension = 0;
    int lhs_shape_comp;
    int rhs_shape_comp;
    LONGEST lower; 
    LONGEST upper; 

    lhs_type_code = TYPE_CODE ( lhs_type );  
    rhs_type_code = TYPE_CODE ( rhs_type );  
    get_array_info 
      ( rhs_type, 
        & rhs_open_dims, 
        & rhs_fixed_dims, 
        & rhs_elem_type
      );
    get_array_info 
      ( lhs_type, 
        & lhs_open_dims, 
        & lhs_fixed_dims, 
        & lhs_elem_type
      );
    switch ( lhs_type_code ) 
      { case TYPE_CODE_M3_OPEN_ARRAY: 
          switch ( rhs_type_code ) 
            { case TYPE_CODE_M3_OPEN_ARRAY: 
                if ( lhs_value == NULL 
                     && lhs_fixed_dims == 0 
                     && rhs_fixed_dims == 0 
                     && lhs_open_dims == rhs_open_dims 
                   ) 
                  { result = rhs_value; } /* Nothing in rhs_value will change. */
                else 
                  { result = allocate_value ( lhs_type ); 
                    array_addr = m3_value_open_array_elems_addr ( rhs_value );
                    m3_set_value_open_array_elems_addr ( result, array_addr );
                  } 
                break; 
              case TYPE_CODE_M3_ARRAY: 
                if ( VALUE_LVAL ( rhs_value ) != lval_memory ) 
                  error 
                    ( "Passing an array constructor to an open array formal "
                      "is not supported, " 
                      " formal \"%s\" of procedure \"%s\"",
                      formal_name, proc_name 
                    ); /* NORETURN */ 
                result = allocate_value ( lhs_type ); 
                array_addr 
                  = VALUE_ADDRESS ( rhs_value ) + value_offset ( rhs_value ); 
                m3_set_value_open_array_elems_addr ( result, array_addr );
                break; 
              default: 
                return NULL; 
            } 
          break; 
        case TYPE_CODE_M3_ARRAY: 
          switch ( rhs_type_code ) 
            { case TYPE_CODE_M3_OPEN_ARRAY: 
                result = allocate_value ( lhs_type ); 
                array_addr = m3_value_open_array_elems_addr ( rhs_value );
                VALUE_ADDRESS ( result ) = array_addr; 
                set_value_lazy ( result, 1 );
                break; 
              case TYPE_CODE_M3_ARRAY: 
                if ( lhs_value == NULL ) 
                  { result = rhs_value; } /* Nothing in rhs_value will change. */
                else 
                  { result = allocate_value ( lhs_type ); 
                    array_addr = VALUE_ADDRESS ( rhs_value ); 
                    VALUE_ADDRESS ( result ) = array_addr; 
                    set_value_offset ( result, value_offset ( rhs_value ) ); 
                    set_value_lazy ( result, 1 );
                  } 
                break; 
              default: 
                return NULL; 
            } 
          break; 
        default: 
          return NULL; 
      } 
    if ( lhs_open_dims + lhs_fixed_dims != rhs_open_dims + rhs_fixed_dims ) 
      { error 
          ( "Unequal array dimensions, %d, %d",
            lhs_open_dims + lhs_fixed_dims,
            rhs_open_dims + rhs_fixed_dims
            ); /* NORETURN */ 
      } 
    if ( ! m3_types_equal ( lhs_elem_type, rhs_elem_type ) ) 
      { if ( proc_name == NULL ) 
          { error ( "Unequal array element types." ); /* NORETURN */ }  
        else 
          { error 
              ( "Unequal array element types,"
                " formal \"%s\" of procedure \"%s\"",
                formal_name, proc_name 
              ); /* NORETURN */
          }  
      } 
    if ( lhs_type_code == TYPE_CODE_M3_OPEN_ARRAY 
         && rhs_type_code == TYPE_CODE_M3_OPEN_ARRAY 
         && result == rhs_value
       ) 
      /* Reusing dope. No check or change to shape required. */ 
      { return result; } 
    while ( true ) 
      { switch ( TYPE_CODE ( l_lhs_type ) ) 
          { case TYPE_CODE_M3_OPEN_ARRAY: 
              switch ( TYPE_CODE ( l_rhs_type ) ) 
                { case TYPE_CODE_M3_OPEN_ARRAY: 
                    rhs_shape_comp 
                      = m3_value_open_array_shape_component 
                          ( rhs_value, dimension );
                    l_rhs_type = TYPE_M3_OPEN_ARRAY_ELEM ( l_rhs_type ); 
                    break; 
                  case TYPE_CODE_M3_ARRAY: 
                    m3_ordinal_bounds 
                      ( TYPE_M3_ARRAY_INDEX ( l_rhs_type ), 
                        & lower, & upper 
                      ); 
                    rhs_shape_comp = upper - lower + 1; 
                    l_rhs_type = TYPE_M3_ARRAY_ELEM ( l_rhs_type ); 
                    break; 
                } 
              if ( lhs_value != NULL ) 
                { lhs_shape_comp 
                    = m3_value_open_array_shape_component 
                        ( lhs_value, dimension );
                  if ( lhs_shape_comp != rhs_shape_comp ) 
                    { if ( proc_name == NULL ) 
                        { error 
                            ( "Shape check failure." 
                              "dimension %d, lhs %d, rhs %d.",
                               dimension, lhs_shape_comp, rhs_shape_comp
                            ); /* NORETURN */ 
                        } 
                      else 
                        { error 
                            ( "Shape check failure." 
                              " formal \"%s\" of procedure \"%s\", "
                              "dimension %d, lhs %d, rhs %d.",
                              formal_name, proc_name, 
                              dimension, lhs_shape_comp, rhs_shape_comp
                            ); /* NORETURN */ 
                        } 
                    } 
                } 
              m3_set_value_open_array_shape_component 
                ( result, dimension, rhs_shape_comp );  
              l_lhs_type = TYPE_M3_OPEN_ARRAY_ELEM ( l_lhs_type ); 
              break; 
            case TYPE_CODE_M3_ARRAY: 
              m3_ordinal_bounds 
                ( TYPE_M3_ARRAY_INDEX ( l_lhs_type ), 
                  & lower, & upper 
                ); 
              lhs_shape_comp = upper - lower + 1; 
              switch ( TYPE_CODE ( l_rhs_type ) ) 
                { case TYPE_CODE_M3_OPEN_ARRAY: 
                    rhs_shape_comp 
                      = m3_value_open_array_shape_component 
                          ( rhs_value, dimension );
                    l_rhs_type = TYPE_M3_OPEN_ARRAY_ELEM ( l_rhs_type ); 
                    break; 
                  case TYPE_CODE_M3_ARRAY: 
                    m3_ordinal_bounds 
                      ( TYPE_M3_ARRAY_INDEX ( l_rhs_type ), 
                        & lower, & upper 
                      ); 
                    rhs_shape_comp = upper - lower + 1; 
                    l_rhs_type = TYPE_M3_ARRAY_ELEM ( l_rhs_type ); 
                    break; 
                } 
              if ( lhs_shape_comp != rhs_shape_comp ) 
                { if ( proc_name == NULL ) 
                    { error 
                        ( "Shape check failure." 
                          "dimension %d, lhs %d, rhs %d.",
                           dimension, lhs_shape_comp, rhs_shape_comp
                        ); /* NORETURN */ 
                    } 
                  else 
                    { error 
                        ( "Shape check failure." 
                          " formal \"%s\" of procedure \"%s\", "
                          "dimension %d, lhs %d, rhs %d.",
                          formal_name, proc_name, 
                          dimension, lhs_shape_comp, rhs_shape_comp
                        ); /* NORETURN */ 
                    } 
                } 
              l_lhs_type = TYPE_M3_ARRAY_ELEM ( l_lhs_type ); 
              break; 
            default: /* A non-array type, this is the ultimate element type. */  
             /* We just built the dope for this open array parameter
                and it is located only in gdb process space.  A copy
                of the dope has to be pushed on the inferior stack,
                then the copy's address passed as the parameter, all
                of which has to be done inside call_function_by_hand,
                after it has done various stack-aligning, etc.
                call_function_by_hand can distinguish this case by
                TYPE_CODE_M3_OPEN_ARRAY, which will not occur
                otherwise.  (If the dope is already in inferior space,
                the parameter value will be for a pointer to the dope
                and have TYPE_CODE_M3_INDIRECT.) */ 
              return result; 
          } 
        dimension ++;
      } 
  } /* m3_check_and_coerce_array */ 

/* The Modula-3 compilers do a poor job of giving enough info to figure out if 
   we have a function procedure and if so, what its result type is.  Here is a 
   hare-brained scheme, worked out experimentally, for inferring it.  
   If it's a function and the result type is "small", there will be a local
   variable named "_result" whose type is what we want.  If the result type is
   "big" (i.e., compiled as if it were a VAR parameter), there will be a formal
   parameter, of the same name, whose type is that of a VAR parameter of the
   result type.  
   In order to do this, We have to have the symbol for the actual procedure 
   constant.  This will have a type whose code is TYPE_CODE_FUNC, and whose 
   "fields" have been built from the separate N_PSYM stab entries that follow
   the N_FUN stab entry for the procedure.  It will have a constructed result
   type that is almost no help, except when it has a builtin type.  However,
   its block will have entries for the formals, also constructed from N_PSYM
   entries, and locals, constructed from N_LSYM entries.  
   type is what we will replace here, with the type of "_result".    
   Procedure type stabs entries for procedure variables and methods are produced
   a little differently by the compilers, and don't contain enough info at all.  
   Fortunately, for evaluating a user-typed call, we can get the runtime address 
   of the procedure constant first, then get the symbol from that. 
   It is not clear that the type of "_result" will be available when the 
   procedure constant symbol is constructed, so we delay this lookup until 
   a user-typed call is being evaluated.  
   Note that Modula-3 identifiers must begin with a letter, so the compiler-
   generated name "_result" can't be spoofed by the Modula-3 programmer. 
   This function patches the result type of the procedure constant symbol
   and also returns its result type.  is_formal (which may be NULL) is set to
   true iff it is a result that is, at machine-code level, implemented as a 
   VAR parameter. 
*/
static struct type * 
m3_patch_proc_result_type ( 
  CORE_ADDR code_addr, char * name, bool * result_is_ref_param )

{ struct symbol * proc_sym; 
  struct block * proc_block;
  struct symbol * result_sym; 
  struct type * result_type;  
  bool l_result_is_ref_param; 

  if ( result_is_ref_param != NULL ) { * result_is_ref_param = false; } 
  proc_sym = find_pc_function ( code_addr ); 
  if ( proc_sym == NULL ) 
    { error ( "Can't get symbol for procedure \"%s\".", name ); /* NORETURN */ }
  proc_block = SYMBOL_BLOCK_VALUE ( proc_sym );
  result_sym = lookup_block_symbol ( proc_block, "_result", NULL, VAR_DOMAIN ); 
  if ( result_sym == NULL ) /* No result type => proper procedure. */ 
    { return NULL; } 
  l_result_is_ref_param = ( SYMBOL_CLASS ( result_sym ) == LOC_ARG ); 
  result_type = SYMBOL_TYPE ( result_sym ); 
  gdb_assert  
    ( l_result_is_ref_param 
      == ( TYPE_CODE ( result_type ) == TYPE_CODE_M3_INDIRECT )
      /* We will need this inside call_fuction_by_hand to tell if result
                is passed by reference. */ 
    ); 
  if ( result_is_ref_param != NULL ) 
    { * result_is_ref_param = l_result_is_ref_param; } 
  /* Patch the procedure type's result type, which will be used elsewhere to
     detect that this is a function whose result is actually a ref parameter. */ 
  TYPE_TARGET_TYPE ( SYMBOL_TYPE ( proc_sym ) ) = result_type; 
  return result_type; 
} /* m3_patch_proc_result_type */

/* NOTE: on Modula-3 dynamic values of procedure types. 

   All the compilers represent dynamic procedure values as a pointer.
   This points to the beginning of the procedure code, iff it is a top-level
   procedure.  If it is a local (i.e. nested) procedure, it points to a
   "closure".  A closure is a record with three words.  The first is a "mark",
   a word that has all bits set.  You can tell whether you have a pointer to
   the code of a top-level procedure or a closure by checking for this value.
   The obvious assumption is that this bit pattern will never occur as machine
   code in any of the targets.  The remaining two words are the address of the
   procedure's target machine code and its environment pointer. 

   In gdb, we handle closures in two ways.  Procedure values that are stored
   in the inferior program are represented in gdb space as struct value 
   objects  whose value is the pointer (to either kind), and whose type has 
   TYPE_CODE_M3_PROC.  In evaluating user-typed expressions and statements,
   gdb checks which kind it is when:
     1) Trying to assign it to a variable.  Only top-level is legal.
     2) Calling the denoted procedure, which requires different call
        mechanisms for the two cases.  
     3) Printing its value, which also has to be done differently.
   When passing it as a parameter, which kind it is does not matter.  The
   pointer itself is just passed. 

   gdb also constructs closures in gdb space that don't exist in inferior
   space.  These are represented by a struct value node whose value is the
   entire 3-word closure, and whose type has TYPE_CODE_M3_PROC_CLOSURE and
   whose TYPE_TARGET_TYPE is the type of the procedure constant (which has
   TYPE_CODE_FUNC).  These have a relatively short lifetime.  This is just 
   a trick to get extra information into call_function_by_hand and several 
   other functions it can call, including many target-dependent functions,
   without changing their parameter lists.  These are built: 
     1) When a user-typed call on a nested procedure constant is evaluated.
        The closure is built and passed in to call_function_by_hand as the
        function to be called.  It is then recognized and used by m3-dependent  
        code to pass the static link.  
     2) When a user_typed call that passes a nested procedure constant as
        an actual parameter is evaluated.  It is later used by m3-dependent 
        code to push a copy of the closure onto the inferior stack and then 
        pass a pointer to this closure.
*/ 

/* CHECK: Is this value target-dependent? */ 
static const CORE_ADDR closure_mark = - 1L;  

struct m3_proc_closure 
  { CORE_ADDR closure_mark; /* Contents always == closure_mark, if this is  
                               actually a closure. */ 
    CORE_ADDR code_addr;
    CORE_ADDR env_ptr; 
  };   

struct type *
m3_alloc_closure_type ( struct type * proc_type ) 

{ struct type * result; 

  /* FIXME: There is only one type object builtin_type_m3_proc_closure, 
     and we always return it.  We ought to allocate a new one here,
     because we patch its TYPE_TARGET_TYPE.  But it would need to be
     in the same space as values, and that would entail duplicating
     allocate_type.  We assume there will never be more than one user
     command being evaluated at a time and only one call at a time
     within a command, even for nested calls.  So we should be able to
     get away with this.  But it's sleazy. */ 
  result = builtin_type_m3_proc_closure;
  TYPE_TARGET_TYPE ( result ) = proc_type; 
  return result; 
} /* m3_alloc_closure_type */ 

/* Build a procedure closure entirely in gdb's process space. */ 
struct value * 
m3_build_gdb_proc_closure 
  ( struct type * proc_type, 
    CORE_ADDR code_addr, 
    CORE_ADDR env_ptr 
  ) 

  { struct value * result; 
    struct type * closure_type; 
    struct m3_proc_closure * closure; 

    closure_type = m3_alloc_closure_type ( proc_type ); 
    result = allocate_value ( closure_type ); 
    /* REVIEWME: Is there any endian-dependent problem here? */ 
    closure = ( struct m3_proc_closure * ) value_contents_raw ( result ); 
    closure -> closure_mark = closure_mark; 
    closure -> code_addr = code_addr; 
    closure -> env_ptr = env_ptr;
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
    struct m3_proc_closure * closure; 

    if ( closure_value == NULL ) { return false; }  
    closure_type = value_type ( closure_value ); 
    if ( closure_type == NULL ) { return false; }  
    if ( TYPE_CODE ( closure_type ) == TYPE_CODE_M3_PROC_CLOSURE ) 
      { closure = ( struct m3_proc_closure * ) value_contents ( closure_value );
        return closure -> closure_mark == closure_mark; 
      } 
    else { return false; } 
  } /* m3_value_is_proc_closure */  

/* If closure is a Modula-3 procedure closure value, return its result type. 
   Otherwise, return NULL. */ 
struct type *  
m3_proc_closure_result_type ( struct value * closure_value ) 

  { struct type * closure_type; 
    struct type * proc_const_type; 

    if ( m3_value_is_proc_closure ( closure_value ) ) 
      { closure_type = value_type ( closure_value ); 
        proc_const_type = TYPE_TARGET_TYPE ( closure_type ); 
        return TYPE_TARGET_TYPE ( proc_const_type ); 
      } 
    else { return NULL; } 
  } /* m3_proc_closure_result_type */ 

/* If closure is a Modula-3 procedure closure value, return its code address.  
   Otherwise, return zero. */ 
CORE_ADDR 
m3_proc_closure_code_addr ( struct value * closure_value ) 

  { if ( m3_value_is_proc_closure ( closure_value ) ) 
      { return 
          ( * ( struct m3_proc_closure * ) value_contents ( closure_value ) ) 
          . code_addr; 
      } 
    else { return 0; } 
  } /* m3_proc_closure_code_addr */ 

/* If closure is a Modula-3 procedure closure, return its environment pointer. 
   Otherwise return zero. */ 
CORE_ADDR 
m3_proc_closure_env_ptr ( struct value * closure_value ) 

  { if ( m3_value_is_proc_closure ( closure_value ) ) 
      { return 
          ( * ( struct m3_proc_closure * ) value_contents ( closure_value ) ) 
          . env_ptr; 
      } 
    else { return 0; } 
  } /* m3_proc_closure_env_ptr */ 

/* If proc_const_value is a value for a procedure constant that is nested,
   return a value for a closure for that procedure, using the current, 
   user-selected frame to construct its environment pointer. 
   Nested or not, if inf_code_addr_result is non-NULL, set it to the 
   inferior code address of the procedure. */
struct value *
m3_close_nested_proc_const ( 
    struct value * proc_const_value, 
    CORE_ADDR * inf_code_addr_result
  ) 

{ struct type * proc_type; 
  struct block * callee_block; 
  struct block * callee_pred_block; 
  struct frame_info * static_link_frame; 
  struct value * result; 
  CORE_ADDR inf_code_addr; 
  CORE_ADDR static_link; 
  
  if ( proc_const_value == NULL ) { return NULL; } 
  proc_type = value_type ( proc_const_value ); 
  if ( proc_type == NULL || TYPE_CODE ( proc_type ) != TYPE_CODE_FUNC ) 
    { return proc_const_value; }
  /* Types with TYPE_CODE_FUNC are constructed with length of 1 byte, but
     Modula-3 calls will  use these as the type of a (procedure) parameter to be
     passed, whose size must be the size of a pointer. */ 
  TYPE_LENGTH ( proc_type ) = TARGET_PTR_BIT / TARGET_CHAR_BIT; 
  inf_code_addr 
    = VALUE_ADDRESS ( proc_const_value ) + value_offset ( proc_const_value ); 
  if ( inf_code_addr == 0 ) { return proc_const_value; } 
  callee_block = block_for_pc ( inf_code_addr );
  callee_pred_block = m3_proc_block ( BLOCK_SUPERBLOCK ( callee_block ) );
  if ( callee_pred_block == NULL ) /* Not nested. */
    { result = proc_const_value; } 
  else /* Nested procedure. */  
    { static_link_frame = m3_frame_for_block ( callee_pred_block );  
      static_link = get_frame_base_address ( static_link_frame );  
      result  
        = m3_build_gdb_proc_closure 
            ( proc_type, inf_code_addr, static_link ); 
    } 
  if ( inf_code_addr_result != NULL ) * inf_code_addr_result = inf_code_addr;  
  return result; 
} /* m3_close_nested_proc_const */  

/* This handles both assignment and parameter passing of procedure types. 
   Returns NULL if the types are not relevant.  To do something, lhs_type
   needs to have TYPE_CODE_M3_PROC and rhs_type needs to have TYPE_CODE_FUNC. */ 
static struct value * 
m3_check_and_coerce_proc (
    struct type * lhs_type,
    struct value * rhs_value,
    struct type * rhs_type,
    char * proc_name,
    char * formal_name 
  ) 

  { struct value * result_val; 
    struct value * result_val_2;  
    CORE_ADDR inf_code_addr; 

    if ( TYPE_CODE ( lhs_type ) != TYPE_CODE_M3_PROC ) { return NULL; } 
    if ( TYPE_CODE ( rhs_type ) != TYPE_CODE_FUNC ) { return NULL; }  
    /* FIXME: Do type check of two similar procedure types. */ 
    if ( false )
      { if ( proc_name == NULL )  
          { error ( "Procedure type not assignable." ); /* NORETURN */ } 
        else 
          { error 
              ( "Actual parameter type not assignable to procedure "
                " formal \"%s\" of procedure \"%s\"" 
                , formal_name, proc_name
              ); /* NORETURN */ 
          } 
      } 
    result_val 
      = m3_close_nested_proc_const 
          ( rhs_value, & inf_code_addr /* unused. */ ); 
    if ( TYPE_CODE ( value_type ( result_val ) ) == TYPE_CODE_FUNC ) 
      /* Convert (top-level) procedure constant to a variable. */
      { result_val_2 = allocate_value ( lhs_type ); 
        store_unsigned_integer 
          ( value_contents_raw ( result_val_2 ), 
            TYPE_LENGTH ( lhs_type ), 
            VALUE_ADDRESS ( result_val ) + value_offset ( result_val )
          );
        return result_val_2;  
      } 
    else { return result_val; } 
  } /* m3_check_and_coerce_proc */ 

static struct value * 
m3_check_and_coerce_actual ( 
    struct type * formal_type, 
    struct value * actual_value,   
    char * proc_name,
    char * formal_name 
  ) 

{ struct type * actual_type; 
  struct type * actual_direct_type;
  struct type * formal_direct_type;
  struct value * actual_direct_value;
  struct value * result_value;
  struct value * result_direct_value;

  if ( actual_value == NULL ) { return NULL; } 
  actual_type = value_type ( actual_value );  
  if ( actual_type == NULL || formal_type == NULL ) { return actual_value; } 
  
  if ( TYPE_CODE ( actual_type ) == TYPE_CODE_M3_INDIRECT )
    { actual_direct_type = TYPE_TARGET_TYPE ( actual_type ); 
      actual_direct_value 
         = value_at_lazy 
             ( actual_direct_type , value_as_address ( actual_value ) );
    } 
  else 
    { actual_direct_type = actual_type; 
      actual_direct_value = actual_value; 
    } 
  if ( TYPE_CODE ( formal_type ) == TYPE_CODE_M3_INDIRECT ) 
    /* Passed by reference.  This includes all VAR and READONLY parameters and
       open array formals of any mode.  

       For READONLY of an ordinal type where the actual has a
       non-equal but assignable type, compiler-generated code makes a
       copy at the call site.  We can't distinguish READONLY from VAR
       in the stabs info, so we just assume it's VAR and refuse
       non-equal type actuals.

       For a VALUE mode open array parameter, a copy is made inside the called
       procedure, so we don't have to worry about doing this or even about
       what mode the parameter has. 

       For parameters of procedure type, VAR and READONLY are the same, since
       values of these types are effectively pointers to immutable ojbects
       and no copies ever have to be made. 

       For other type categories, there are no assignable but non-identical
       types, and the semantics of READONLY and VAR are always the same.

    */ 
    { formal_direct_type = TYPE_TARGET_TYPE ( formal_type ); 
      result_direct_value = m3_check_and_coerce_array 
        ( /* formal_value */ NULL, formal_direct_type, actual_direct_value, 
          actual_direct_type, proc_name, formal_name );  
      if ( result_direct_value == NULL ) 
        { if ( ! m3_types_equal ( formal_direct_type, actual_direct_type ) ) 
            { error 
                ( "Actual parameter type not equal for VAR formal \"%s\" "
                  "of procedure \"%s\"", 
                  formal_name, proc_name
                ); /* NORETURN */ 
            } 
          if ( ! VALUE_LVAL ( actual_direct_value ) ) 
            { error 
                ( "Actual parameter is not a designator for VAR formal \"%s\" "
                  "of procedure \"%s\".", 
                  formal_name, proc_name
                ); /* NORETURN */ 
            } 
          result_direct_value = actual_direct_value; 
        } 
      if ( actual_direct_type == actual_type 
           && TYPE_CODE ( formal_direct_type ) != TYPE_CODE_M3_OPEN_ARRAY 
         )  
        /* Must take address of actual. */ 
        { result_value = value_from_pointer 
            ( lookup_m3_indirect_type ( actual_direct_type ),
	      ( VALUE_ADDRESS ( result_direct_value )
		+ value_offset ( result_direct_value )
		+ value_embedded_offset ( result_direct_value )
              ) 
            );
          return result_value; 
        } 
      else /* Actual is also indirect, so actual_value is correct. */
        { return result_direct_value; } 
    }
  else /* VALUE mode (passed by value.) */ 
    { formal_direct_type = formal_type; 
      if ( m3_types_equal ( formal_type, actual_direct_type ) ) 
        { return actual_direct_value; } 

      result_direct_value = m3_check_and_coerce_array 
        ( /* formal_value */ NULL, formal_direct_type, actual_direct_value, 
          actual_direct_type, proc_name, formal_name 
        );  
      if ( result_direct_value != NULL ) { return result_direct_value; }  

      result_direct_value = m3_check_and_coerce_proc 
        ( formal_direct_type, actual_direct_value, actual_direct_type, 
          proc_name, formal_name 
        );  
      if ( result_direct_value != NULL ) { return result_direct_value; }  

      result_direct_value = m3_check_and_coerce_ordinal 
        ( formal_direct_type, actual_direct_value, 
          actual_direct_type, proc_name, formal_name 
        );  
      if ( result_direct_value != NULL ) { return result_direct_value; }  

      result_direct_value = m3_check_and_coerce_reference
        ( formal_direct_type, actual_direct_value, 
          actual_direct_type, proc_name, formal_name 
        );  
      if ( result_direct_value != NULL ) { return result_direct_value; }  

      error 
        ( "Actual parameter type not assignable to formal \"%s\" "
          "of procedure \"%s\"" 
          , formal_name, proc_name
        ); /* NORETURN */
    } 
  return NULL; /* Suppress warnings.  Shouldn't get here. */ 
} /* m3_check_and_coerce_actual */ 

/* Take care of pushing any required auxiliary data on the stack,
   prior to pushing the real parameters.  This is data that was
   constructed and exists only in gdb process space.  It includes
   closures for procedure values and dope for open arrays.  If array
   constructors are ever implemented and allowed to be passed to open
   array formals, it will also have to include the gdb-space_only
   contents of the array, in addition to the dope.  This gets called
   from inside call_function_by_hand, after it has set things up for
   gdb to push stuff on the stack. */ 
void 
m3_push_aux_param_data ( int nargs, struct value **args, CORE_ADDR * sp )

{ struct  value * actual_value; 
  struct type * actual_type; 
  struct type * pointer_type; 
  int i; 
  int aux_len; 
  CORE_ADDR aux_addr;

  for ( i = nargs - 1; i >= 0; i -- ) 
    { actual_value = args [ i ];
      actual_type = value_type ( actual_value ); 
      switch ( TYPE_CODE ( actual_type ) ) 
        { default: 
            continue; 
          case TYPE_CODE_M3_PROC_CLOSURE : 
            aux_len = TYPE_LENGTH ( actual_type ); 
            pointer_type = TYPE_TARGET_TYPE ( actual_type );  
            break; 
          case TYPE_CODE_M3_OPEN_ARRAY: 
            aux_len = TYPE_LENGTH ( actual_type );  
            pointer_type = lookup_m3_indirect_type ( actual_type ); 
            break;
        }  
      if (INNER_THAN (1, 2))
        { /* stack grows downward and the address of what 
             we push is the stack pointer after we push it.*/
          * sp -= aux_len;
          aux_addr = * sp;
        }
      else
        { /* The stack grows up, so the address of what 
             we push is the stack pointer before we push it.  */
          aux_addr = * sp;
          * sp += aux_len;
        }
      /* Store the auxiliary data. */
      write_memory 
        ( aux_addr, 
          value_contents_all ( actual_value ), 
          aux_len  
        );
      args [ i ] = value_from_pointer ( pointer_type , aux_addr );
    }  
} /* m3_push_aux_param_data */  

static struct value *
m3_evaluate_call
  ( struct value * proc_const_value,  /* Procedure constant to be called.  Should
                                         have a type with TYPE_CODE_FUNC. */
    struct type * check_actuals_type, /* Use for checking/coercing actuals. Can
                                         be either TYPE_CODE_M3_PROC or 
                                         TYPE_CODE_FUNC. */ 
    CORE_ADDR code_addr,              /* Inferior address of procedure code. */ 
    struct value * self,          /* If a method call, the self expression. */
    int nargs,                    /* Number actually typed by user. */ 
    char * name, 
    struct expression *exp, 
    int *pos,
    enum noside noside
  )

  { struct type * result_type; 
    bool result_is_ref_param; 
    struct value * * argvec; 
    int downward_nargs; /* number in argvec, passed at machine level. */ 
    int expected_args; 
    int next_argvec; 
    int next_formal; 
    int next_actual; /* */  

    next_formal = 0; 
    expected_args = TYPE_NFIELDS ( check_actuals_type ); 
    if ( TYPE_CODE ( check_actuals_type ) == TYPE_CODE_M3_PROC ) 
      /* First "formal" in fields is actually result type, possibly void. */ 
      { next_formal ++; 
        expected_args --;
      } 
    result_type 
      = m3_patch_proc_result_type ( code_addr, name, & result_is_ref_param ); 
    if ( result_type != NULL 
         && TYPE_CODE ( result_type ) == TYPE_CODE_M3_INDIRECT 
         && TYPE_CODE ( check_actuals_type ) == TYPE_CODE_FUNC 
       ) /* Last "formal" of check_actuals_type is really a large function 
            result, passed by reference. */ 
      { expected_args --; } 
    if ( nargs != expected_args ) 
      { error 
         ( "Procedure %s requires %d parameters, but %d were supplied.", 
           name,
           expected_args, 
           nargs  
         ); /* NORETURN */ 
      }
    downward_nargs = nargs + ( self != NULL ) + ( result_is_ref_param );
    argvec 
       = ( struct value * * ) 
             alloca ( sizeof ( struct value * ) * ( downward_nargs + 1 ) );
    next_argvec = 0; 
    if ( self != NULL ) /* This is a method call. */ 
      { argvec [ 0 ] = self; 
        next_argvec ++; 
      } 
    for ( next_actual = 0; next_actual < nargs; next_actual ++ )  
      { argvec [ next_argvec ] = m3_evaluate_subexp ( NULL, exp, pos, noside );
        argvec [ next_argvec ] 
          = m3_check_and_coerce_actual 
              ( TYPE_M3_FIELD_TYPE ( check_actuals_type , next_formal ),
                argvec [ next_argvec ], 
                name,
                TYPE_FIELD_NAME ( check_actuals_type , next_formal )
              ); 
        next_formal ++; 
        next_argvec ++; 
      } 
    if ( result_is_ref_param ) 
      { argvec [ next_argvec ] 
          = allocate_value ( result_type /* has TYPE_CODE_M3_INDIRECT */ ); 
      /* Setting value_contents_raw ( argvec [ i ] will have to wait until the
         space for the result is pushed on the inferior stack, which happens 
         inside call_function_by_hand. */ 
        set_value_lazy ( argvec [ next_argvec ], 0 ); 
        next_argvec ++;
      } 
    argvec [ next_argvec ] = NULL; 
    return call_function_by_hand ( proc_const_value, downward_nargs, argvec ); 
  } /* m3_evaluate_call */ 

/* Construct and return a new a struct value node for a field (or method),
   with parent record/object value parent_val, located at bitsize and bit
   pos, and having type field_type. */ 
struct value * 
m3_field_value ( 
    struct value * parent_val,
    int bitsize,
    int bitpos,
    struct type * field_type
  ) 

  { struct value * result_val; 

    result_val = allocate_value ( field_type );
    VALUE_LVAL ( result_val ) = 1;
    VALUE_ADDRESS ( result_val ) = VALUE_ADDRESS ( parent_val );
    set_value_offset 
      ( result_val, 
        value_offset ( parent_val ) + bitpos / TARGET_CHAR_BIT 
      ); 
    set_value_bitpos ( result_val, bitpos % TARGET_CHAR_BIT ) ; 
#if 0 /* But we never let these get anywhere but to m3_specific code. */ 
    if ( bitpos % TARGET_CHAR_BIT == 0 
         && TYPE_LENGTH ( field_type ) == bitpos / TARGET_CHAR_BIT ) 
      /* Code elsewhere in gdb takes nonzero value_bitsize to mean 
         non-byte-aligned.  When value_bitsize == 0, it uses the size
         (in bytes) from the type. */ 
      { set_value_bitsize ( result_val, 0 ); } 
      else
#endif     
      { set_value_bitsize ( result_val, bitsize ); } 
    set_value_lazy ( result_val, 1 );
    return result_val; 
  } /* m3_field_value */ 

/* Evaluate a dot construct.  If the caller is evaluating a call construct
   (and uses this for its left operand), and the dot construct turns out
   to denote a method of an object value, the caller will need values for
   both the dot construct itself (what to call) and the left operand of the
   dot (to pass as the first, ('self') parameter.  The former value is returned
   as the function result.  If this is a method, 'self' will be set to the latter
   value.  Otherwise, 'self' will be set to null. */ 
/* NOTE: As of 2006-6-5, m3_parse_e8, in m3-exp.c, during expression parsing,
         handles dot-constructs that are merely qualified references to 
         declared entities.  So This only needs to handle access to fields
         and methods of record and object values. 
*/ 
static struct value *
m3_evaluate_dot
  ( struct expression *exp, 
    int *pos,
    enum noside noside,  
    struct value ** self, 
    char * * out_field_name
  )

{ int pc; 
  int field_name_len; 
  char * field_name; 
  struct value * lhs_value; 
  struct value * dot_value; 
  struct type * lhs_type; 
  struct type * dot_type; 
  struct type * supertype; 
  struct type * allocated_type; 
  CORE_ADDR allocated_tc_addr; 
  CORE_ADDR supertype_tc_addr; 
  CORE_ADDR lhs_inf_addr; 
  int bitsize;  
  int bitpos; 

  if ( self != NULL ) { *self = NULL; } 
  if ( out_field_name != NULL ) { *out_field_name = NULL; } 
  pc = * pos; 
  field_name_len = longest_to_int ( exp -> elts [ pc + 1 ] . longconst );  
  field_name = &exp -> elts [ pc + 2 ] . string;  
  *pos += 4 + BYTES_TO_EXP_ELEM ( field_name_len + 1 ); 
  lhs_value = m3_evaluate_subexp ( NULL_TYPE, exp, pos, noside ); 
  lhs_type = value_type ( lhs_value ); 
  while ( TYPE_CODE ( lhs_type ) == TYPE_CODE_M3_INDIRECT  
          || ( TYPE_CODE ( lhs_type ) == TYPE_CODE_M3_POINTER 
               && TYPE_CODE ( TYPE_M3_TARGET ( lhs_type ) ) 
                  == TYPE_CODE_M3_RECORD  
             ) 
        ) 
    { lhs_inf_addr = value_as_address ( lhs_value ); 
      if ( lhs_inf_addr == 0 ) 
        { error 
            ( "Attempt to implicitly dereference NIL, for record field \"%s\".",
              field_name
            ); /* NORETURN */ 
        } 
      lhs_value = value_at_lazy ( TYPE_M3_TARGET ( lhs_type ), lhs_inf_addr ); 
      VALUE_LVAL ( lhs_value ) = 1;
      lhs_type = value_type ( lhs_value ); 
    } /* while */
  switch ( TYPE_CODE ( lhs_type ) )  
    { case TYPE_CODE_M3_RECORD: 
        if ( find_m3_rec_field 
               ( lhs_type, field_name, & bitsize, & bitpos, & dot_type ) 
           ) 
          { dot_value = m3_field_value ( lhs_value, bitsize, bitpos, dot_type ); 
            if ( out_field_name != NULL ) 
              { *out_field_name = field_name; } 
             return dot_value; 
          } 
        else 
          { error 
              ( "Record has no field named \"%s\".", field_name ); /* NORETURN */
          } 
        break; 

      case TYPE_CODE_M3_REFANY:
      case TYPE_CODE_M3_ROOT:
      case TYPE_CODE_M3_TRANSIENT_REFANY:
      case TYPE_CODE_M3_TRANSIENT_ROOT: 
      case TYPE_CODE_M3_OBJECT:
        lhs_inf_addr = value_as_address ( lhs_value ); 
        if ( lhs_inf_addr == 0 ) 
          { error 
              ( "NIL object cannot have a selection (\".%s\") applied.",
                field_name
              ); /* NORETURN */ 
          } 
	allocated_tc_addr = find_m3_heap_tc_addr ( lhs_inf_addr );
        if ( m3_check_TextLiteral_buf 
              ( lhs_inf_addr, allocated_tc_addr, field_name, & bitsize, & bitpos,
                & dot_type 
              ) 
           ) 
          { dot_value = value_at_lazy ( dot_type, lhs_inf_addr ); 
            set_value_offset 
              ( dot_value, value_offset ( dot_value ) + bitpos / TARGET_CHAR_BIT );
            if ( out_field_name != NULL ) 
              { *out_field_name = field_name; } 
            return dot_value; 
          } 
        else 
          { /* This is a faster way to find the typecell corresponding to 
               lhs_type than find_tc_from_m3_type, which must search all 
               typecodes: */ 
            supertype_tc_addr = allocated_tc_addr; 
            while ( find_m3_type_from_tc ( supertype_tc_addr ) != lhs_type ) 
              { supertype_tc_addr 
                  = m3_tc_addr_to_super_tc_addr ( supertype_tc_addr ); 
              } 
            supertype = lhs_type; 
            /* Here, supertype_tc_addr and supertype are for the static type. */
            while ( true ) /* Search supertypes for field/method. */ 
              { if ( TYPE_CODE ( supertype ) != TYPE_CODE_M3_OBJECT ) 
                  { error 
                      ( "Object has no field or method named \"%s\"."
                      , field_name 
                      ); /* NORETURN */ 
                    break; 
                  }
                else if ( find_m3_obj_field 
                            ( supertype, field_name, & bitsize, & bitpos, 
                              & dot_type 
                            ) 
                        ) 
                  { dot_value = value_at_lazy ( dot_type, lhs_inf_addr );
                    bitpos 
                      += TARGET_CHAR_BIT 
                         * m3_tc_address_to_dataOffset ( supertype_tc_addr ); 
                    dot_value 
                      = m3_field_value ( dot_value, bitsize, bitpos, dot_type ); 
                    if ( out_field_name != NULL ) 
                      { *out_field_name = field_name; } 
                    return dot_value; 
                    break; 
                  } /* if */ 
                else if ( find_m3_obj_method 
                            ( supertype, field_name, & bitsize, & bitpos, 
                              & dot_type 
                            ) 
                        ) 
                  { dot_value 
                      = value_at_lazy 
                          ( dot_type, 
                            m3_tc_address_to_defaultMethods ( allocated_tc_addr )
                          );
                    set_value_offset 
                      ( dot_value, 
                        m3_tc_address_to_methodOffset ( supertype_tc_addr )
                        + bitpos / TARGET_CHAR_BIT 
                      ); 
                    if ( self != NULL ) { *self = lhs_value; } 
                    if ( out_field_name != NULL ) 
                      { *out_field_name = field_name; } 
                    return dot_value; 
                  } /* else if */ 
                else /* Go up a level in supertype hierarchy. */ 
                  { supertype_tc_addr 
                      = m3_tc_addr_to_super_tc_addr ( supertype_tc_addr ); 
                    supertype = find_m3_type_from_tc ( supertype_tc_addr ); 
                  } /* else */ 
              } /* while */ 
          } /* else */ 
        break; 

      default:  
        error ( "A selection (\".%s\") can apply only to a RECORD, "
                "REF RECORD, or OBJECT.",
                field_name
              ); /* NORETURN */ 
    } /* switch */ 

  /* We get here for a field of a record or object. */ 
  if ( bitpos % TARGET_CHAR_BIT != 0 ) 
    { /* We have a non-byte aligned value, we need to fetch it
         now, so that we can shift the bits.  Fortunately, we
         know that the type is a scalar.  We need to fetch the
         word that contains this field, because the bitpos is 
         expressed from the lsb of this word */
      LONGEST inf_word_contents;


      /* REVIEW: Is TYPE_LENGTH ( dot_value ) going to be enough?  
                 Is this the right representation of the unpacked value? 
                 Do we need to sign-extend for signed values? */       
      read_memory 
        ( VALUE_ADDRESS ( dot_value ) 
            + value_offset ( dot_value ) 
            + ( bitpos / TARGET_LONG_BIT ) * ( TARGET_LONG_BIT / TARGET_CHAR_BIT ), 
          ( char* ) &inf_word_contents, 
          TARGET_LONG_BIT / TARGET_CHAR_BIT
        );
      *(LONGEST *) value_contents_raw ( dot_value ) 
        = m3_extract_ord 
            ( (char *) &inf_word_contents, 
              bitpos % TARGET_LONG_BIT, 
              TYPE_M3_SIZE ( dot_type ), 
              0 
            ); 
      set_value_lazy ( dot_value, 0 ); 
    }
  else 
    { set_value_lazy ( dot_value, 1 );
      set_value_offset 
        ( dot_value, value_offset ( dot_value ) + bitpos / TARGET_CHAR_BIT );
    } 
  return dot_value; 
} /* m3_evaluate_dot */ 

/* This evaluates a call that is known not a dispatching method call, but could
   be a call on a procedure constant, a procedure variable, or something not 
   callable.
*/ 
static struct value *
m3_call_proc_const_or_var 
  ( struct value * orig_proc_value, 
    int nargs, 
    char * name, 
    struct expression *exp, 
    int *pos,
    enum noside noside
) 

{ struct value * proc_value; 
  struct type * closure_type; 
  struct type * proc_type; 
  struct symbol * proc_const_sym; 
  struct type * proc_const_type;
  struct value * deref_value; 
  CORE_ADDR inf_addr; 
  CORE_ADDR inf_code_addr; 

  proc_value = orig_proc_value; 
  proc_type = value_type ( proc_value ); 
  while ( TYPE_CODE ( proc_type ) == TYPE_CODE_M3_INDIRECT ) 
    { inf_addr = value_as_address ( proc_value ); 
      if ( inf_addr == 0 ) 
        { error 
            ( "NIL indirect.  Modula-3 compiler shouldn't allow this to happen." ); 
          /* NORETURN */ 
        } 
      proc_value = value_at_lazy ( TYPE_M3_TARGET ( proc_type ), inf_addr ); 
      proc_type = value_type ( proc_value ); 
    } /* while */
  switch ( TYPE_CODE ( proc_type ) )  
    { case TYPE_CODE_FUNC: /* A procedure constant. */
        proc_value = m3_close_nested_proc_const ( proc_value, & inf_code_addr ); 
        break; 
      case TYPE_CODE_M3_PROC: /* A procedure variable. */  
        inf_addr = value_as_address ( proc_value ); 
        if ( inf_addr == 0 ) 
          { error ( "Attempt to call NIL procedure variable \"%s\".", name ); 
            /* NORETURN */ 
          } 
        if ( m3_inf_address_is_proc_closure ( inf_addr ) ) 
          /* inf_addr points to a closure in inferior. 
             Build a gdb closure value. */ 
          { closure_type = m3_alloc_closure_type ( NULL );
            deref_value = value_at_lazy ( closure_type, inf_addr );
            inf_code_addr = m3_proc_closure_code_addr ( deref_value );
            proc_const_sym = find_pc_function ( inf_code_addr ); 
            proc_const_type = SYMBOL_TYPE ( proc_const_sym ); 
            TYPE_TARGET_TYPE ( closure_type ) = proc_const_type; 
            proc_value = deref_value; 
          } 
        else /* It's not a closure. */ 
          /* inf_addr is the direct code address.  
             Give proc_value the procedure constant value and type. */ 
          { inf_code_addr = inf_addr; 
            proc_const_sym = find_pc_function ( inf_code_addr ); 
            proc_const_type = SYMBOL_TYPE ( proc_const_sym ); 
            proc_value = value_at_lazy ( proc_const_type, inf_code_addr ); 
            /* proc_type = value_type ( proc_value ); */ 
          }
        break; 
      default: 
        error ( "Attempt call non-procedure \"%s\".", name ); /* NORETURN */  
    } 
  
  return 
    m3_evaluate_call 
      ( proc_value, /* For the procedure constant. */ 
        proc_type,  /* Original proc type, for checking actuals. */ 
        inf_code_addr,
        /* self */ NULL,
        nargs, name, exp, pos, noside 
      );  
} /* m3_call_proc_const_or_var */ 

/* Never returns a value whose type has TYPE_CODE M3_INDIRECT. */ 
static struct value * 
m3_check_and_coerce_assignment ( 
    struct value * lhs_value,   
    struct value * rhs_value   
  ) 

{ struct type * lhs_type; 
  struct type * rhs_type; 
  struct type * rhs_direct_type;
  struct type * lhs_direct_type;
  struct value * lhs_direct_value;
  struct value * rhs_direct_value;
  struct value * result_direct_value;

  if ( lhs_value == NULL  || rhs_value == NULL ) { return NULL; } 
  lhs_type = value_type ( lhs_value );  
  rhs_type = value_type ( rhs_value );  
  
  if ( TYPE_CODE ( rhs_type ) == TYPE_CODE_M3_INDIRECT )
    { rhs_direct_type = TYPE_TARGET_TYPE ( rhs_type ); 
      rhs_direct_value 
         = value_at_lazy 
             ( rhs_direct_type , value_as_address ( rhs_value ) );
    } 
  else 
    { rhs_direct_type = rhs_type; 
      rhs_direct_value = rhs_value; 
    } 

  if ( TYPE_CODE ( lhs_type ) == TYPE_CODE_M3_INDIRECT ) 
    { lhs_direct_type = TYPE_TARGET_TYPE ( lhs_type ); 
      lhs_direct_value 
         = value_at_lazy 
             ( lhs_direct_type , value_as_address ( lhs_value ) );
    }
  else /* VALUE mode (passed by value.) */ 
    { lhs_direct_type = lhs_type; 
      lhs_direct_value = lhs_value; 
      if ( ! VALUE_LVAL ( lhs_direct_value ) ) 
        { error ( "LHS of assignment is not a designator." ); /* NORETURN */ } 
    } 
  if ( m3_types_equal ( lhs_direct_type, rhs_direct_type ) ) 
    { return rhs_direct_value; } 

  result_direct_value = m3_check_and_coerce_array 
    ( lhs_direct_value, lhs_direct_type, rhs_direct_value, 
      rhs_direct_type, NULL, NULL 
    );  
  if ( result_direct_value != NULL ) { return result_direct_value; }  

  result_direct_value = m3_check_and_coerce_proc 
    ( lhs_direct_type, rhs_direct_value, rhs_direct_type, 
      NULL, NULL 
    );  
  if ( result_direct_value != NULL ) { return result_direct_value; }  

  result_direct_value = m3_check_and_coerce_ordinal 
    ( lhs_direct_type, rhs_direct_value, 
      rhs_direct_type, NULL, NULL 
    );  
  if ( result_direct_value != NULL ) { return result_direct_value; }  

  result_direct_value = m3_check_and_coerce_reference
    ( lhs_direct_type, rhs_direct_value, 
      rhs_direct_type, NULL, NULL 
    );  
  if ( result_direct_value != NULL ) { return result_direct_value; }  

  error ( "RHS expression type not assignable to LHS type in "
          "assignment statement/" 
        ); /* NORETURN */

  return NULL; /* Suppress warnings.  Shouldn't get here. */ 
} /* m3_check_and_coerce_assignment */ 

static struct value *
m3_evaluate_subexp_maybe_packed ( 
    struct type *expect_type,
    struct expression *exp, 
    int *pos,
    enum noside noside
  )
{
  enum exp_opcode op;
  int length;
  int tempval;
  int pc;
  struct value *arg1 = NULL;
  struct value *arg2 = NULL;
  /*struct type *type;*/
  /*int upper, lower;*/
  int float_ok, int_ok;

  pc = *pos;
  op = exp->elts[pc].opcode;

  switch (op)
    {
    case OP_VAR_VALUE: 
      { struct symbol * sym;
        struct block *b;
        struct value *val;
        struct frame_info *frame;

        (*pos) += 4; 
        if (noside == EVAL_SKIP) 
           { return m3_value_from_longest (builtin_type_long, (LONGEST) 1);} 
        b = exp->elts[pc+1].block; 
        sym = exp->elts[pc+2].symbol; 
        if (symbol_read_needs_frame (sym))
          {
            frame = m3_frame_for_block (b);
            if (!frame)
              {
                if (BLOCK_FUNCTION (b)
                    && SYMBOL_PRINT_NAME (BLOCK_FUNCTION (b)))
                  error (_("No frame is currently executing in block %s."),
                         SYMBOL_PRINT_NAME (BLOCK_FUNCTION (b)));
                else
                  error (_("No frame is currently executing in specified block"));
              }
          }
        else { frame = NULL; } 

        val = read_var_value (sym, frame);
        if (!val)
          error (_("Address of symbol \"%s\" is unknown."), 
                 SYMBOL_PRINT_NAME (sym));

        return val;
      } 

    case OP_REGISTER:
      {
	int regno = longest_to_int (exp->elts[pc + 1].longconst);
	struct value *val 
           = value_of_register (regno, get_selected_frame ("No frame"));
	(*pos) += 3;
	if (val == NULL)
	  error ("Value of register %s not available.",
		 frame_map_regnum_to_name 
                   (get_selected_frame ("No frame"), regno));

 	else {
 	  arg1 
            = value_of_register 
                (longest_to_int (exp->elts[pc +1].longconst)
                , get_selected_frame("No frame")
                );
 	  /* hack to convert registers to Modula-3 types... */
 	  if (value_type(arg1) == builtin_type_long) {
 	    deprecated_set_value_type(arg1, builtin_type_m3_integer);
   	    return arg1;
  	  } 
 	  else if (value_type(arg1) == builtin_type_double) {
 	    deprecated_set_value_type(arg1, builtin_type_m3_longreal);
   	    return arg1;
 	  }
 	  else return val;
        }
      }

    case OP_M3_WIDETEXT:
    case OP_M3_TEXT:
      length = longest_to_int (exp->elts[pc + 1].longconst);
      (*pos) += 4 + BYTES_TO_EXP_ELEM (length + 1);
      if (noside == EVAL_SKIP)
        return m3_value_from_longest (builtin_type_long, (LONGEST) 1);
      else 
      return value_string (&exp->elts[pc + 2].string, length);

    case OP_M3_LONG:
    case OP_M3_CHAR:
    case OP_M3_WIDECHAR:
      (*pos) += 4;
      return m3_value_from_longest (exp->elts[pc+1].type,
                                 exp->elts[pc + 2].longconst);

    case OP_M3_REEL:
    case OP_M3_LREEL:
    case OP_M3_XREEL: {
      (*pos) += 4;
      return value_from_double (exp->elts[pc + 1].type,
				exp->elts[pc + 2].doubleconst); }

    case STRUCTOP_M3_INTERFACE:
    case STRUCTOP_M3_MODULE:
    case STRUCTOP_M3_STRUCT: 
      { arg1 = m3_evaluate_dot ( exp, pos, noside, NULL, NULL );  
        return arg1; 
      } 

    case OP_FUNCALL: 
      { enum exp_opcode subop;
        struct value * proc_value; 
        struct value * proc_const_value; 
        struct symbol * proc_const_sym; 
        struct type * proc_type; 
        struct type * proc_const_type; 
        int nargs; 
        struct value * self;
        char * field_name; 
        CORE_ADDR inf_proc_addr; 

        (*pos) += 3; 
        subop = exp -> elts [ *pos ] . opcode; 
        nargs = longest_to_int ( exp -> elts [ pc + 1 ] . longconst ); 
        switch ( subop ) 
          { case STRUCTOP_M3_INTERFACE:
            case STRUCTOP_M3_MODULE:
            case STRUCTOP_M3_STRUCT:  
              proc_value 
                = m3_evaluate_dot ( exp, pos, noside, &self, &field_name );  
              if ( self == NULL ) 
                { return 
                    m3_call_proc_const_or_var 
                      ( proc_value, nargs, field_name, exp, pos, noside );  
                } 
              else /* Method call. proc_value denotes the self object. */
                { proc_type = value_type ( proc_value ); 
                  inf_proc_addr = value_as_address ( proc_value ); 
                  proc_const_sym = find_pc_function ( inf_proc_addr ); 
                  proc_const_type = SYMBOL_TYPE ( proc_const_sym ); 
                  proc_const_value = value_at_lazy 
                    ( proc_const_type, inf_proc_addr ); 
                  return 
                    m3_evaluate_call 
                      ( proc_const_value, 
                        proc_type, 
                        inf_proc_addr, 
                        self, 
                        nargs, field_name, exp, pos, noside 
                      );
                } 
            default:
              proc_value = m3_evaluate_subexp (0, exp, pos, noside);
              proc_type = value_type ( proc_value ); 
              inf_proc_addr = value_as_address ( proc_value ); 
              proc_const_sym = find_pc_function ( inf_proc_addr ); 
              return 
                m3_call_proc_const_or_var 
                  ( proc_value, nargs, SYMBOL_PRINT_NAME ( proc_const_sym ), 
                    exp, pos, noside 
                  );
          } /* switch */ 
      } 

    /* M3_FINAL_TYPE converts an expression of reference type to its allocated 
       type. */
    case M3_FINAL_TYPE: 
      { struct type * arg1_type;
        struct type * allocated_type;

        ( * pos)  += 1; 
        arg1 = m3_evaluate_subexp ( 0, exp, pos, noside );
        arg1_type = value_type ( arg1 );

        allocated_type = m3_allocated_type ( arg1 ); 
        if ( allocated_type != arg1_type ) 
          { deprecated_set_value_type ( arg1, arg1_type ); } 
        return ( arg1 ); 
      }

    case OP_M3_TYPE:
      (*pos) += 3;
      arg1 = allocate_value (exp->elts[pc+1].type);
      *(LONGEST *) value_contents_raw (arg1) = m3_type_magic_value;
      return arg1;

    case UNOP_M3_DEREF: {
      struct type *res_type, *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (0, exp, pos, noside);
      arg1_type = value_type (arg1);

      while (TYPE_CODE (arg1_type) == TYPE_CODE_M3_INDIRECT) 
        { arg1_type = TYPE_M3_TARGET (arg1_type);
	  arg1 = value_at_lazy (arg1_type, m3_value_as_address (arg1)); 
        }

      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_REFANY) 
        { if (value_as_address (arg1) == 0) 
            { error ("^ applied to NIL"); 
              return arg1; 
            }
        /* REVIEWME:  Do we really want to allow dereferencing of REFANY to
           take the allocated type?  If so, don't we want to do the same
           for a traced pointer type as well? */ 
	  res_type = find_m3_heap_type (value_as_address (arg1)); 
          return value_at_lazy (res_type, m3_value_as_address (arg1)); 
        }

      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_POINTER) 
        { if (value_as_address (arg1) == 0) 
            { error ("^ applied to NIL"); 
              return arg1; 
            }
          res_type = TYPE_M3_TARGET (arg1_type); 
          return value_at_lazy (res_type, m3_value_as_address (arg1)); 
        }

      else if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_OBJECT ) 
        /* FIXME:  Do this for builtin object types as well, e.g. ROOT. */ 
        { warning ("Ignoring redundant ^ applied to object"); 
          if (value_as_address (arg1) == 0) 
            /* REVIEWME: Is this check really necessary here? */ 
            { error ("^ applied to NIL object"); }
          return arg1; 
        }

      else 
        { error ("^ applied to a non-REF"); 
          return arg1; 
        }
      } 

    case UNOP_M3_NEG: {
      struct type *neg_type; 

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (0, exp, pos, noside);
      neg_type = value_type (arg1);
      if (TYPE_CODE (neg_type) == TYPE_CODE_FLT)
	return value_from_double (neg_type, - m3_value_as_float (arg1));
      else if (TYPE_CODE (neg_type) == TYPE_CODE_M3_INTEGER)
	return m3_value_from_longest (neg_type, - m3_value_as_integer (arg1));
      else {
	error ("'-' must be applied to an integer or floating-point value");
	return arg1;
      }}
      
    case UNOP_M3_NOT: {
      LONGEST val;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      val  = ! m3_value_as_integer (arg1);
      return m3_value_from_longest (builtin_type_m3_boolean, val);  }
	
    case UNOP_M3_FIRST:
    case UNOP_M3_LAST: 
    case UNOP_M3_NUMBER: {
      struct value * res, * array;
      struct type * array_type, * index_type = NULL;
      LONGEST lowerbound, upperbound, val = 0;

      (*pos) += 1; 
      array = m3_evaluate_subexp (0, exp, pos, noside);
      array_type = value_type (array);

      while (TYPE_CODE (array_type) == TYPE_CODE_M3_POINTER
	     || TYPE_CODE (array_type) == TYPE_CODE_M3_INDIRECT) {
	array_type = TYPE_M3_TARGET (array_type);
	array = value_at_lazy (array_type, m3_value_as_address (array));
        if (array == 0) {
          error ("FIRST, LAST or NUMBER applied to NIL");  }}

      if (TYPE_CODE (array_type) == TYPE_CODE_M3_ARRAY) {
	index_type = TYPE_M3_ARRAY_INDEX (array_type);
	m3_ordinal_bounds (index_type, &lowerbound, &upperbound);
      } else if (TYPE_CODE (array_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	lowerbound = 0;
	upperbound = *(long*) (value_contents (array) + sizeof(long)) - 1;
        index_type = builtin_type_m3_integer; 
      } else if (is_m3_ordinal_type (array_type)) {
	index_type = array_type;
	m3_ordinal_bounds (index_type, &lowerbound, &upperbound);
      } else {
	error ("FIRST, LAST, NUMBER can only be applied to arrays.");
      }

      res = allocate_value (builtin_type_m3_integer);
      set_value_lazy (res, 0);
      switch (op) {
	case UNOP_M3_FIRST:   val = lowerbound;  break;
	case UNOP_M3_LAST:    val = upperbound;  break;
	case UNOP_M3_NUMBER:  val = upperbound - lowerbound + 1; 
                              index_type = builtin_type_m3_cardinal;  break;
      }
      res = allocate_value (index_type);
      *(LONGEST *)value_contents_raw (res) = val;
      set_value_lazy (res, 0);
      return res;
    }

    case UNOP_M3_ABS: {
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);
      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_INTEGER) {
	LONGEST val = m3_value_as_integer (arg1);
	if (val < 0) { val = -val; };
	return m3_value_from_longest (arg1_type, val);
      } else if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
	double val = m3_value_as_float (arg1);
	if (val < 0.0) { val = -val; };
	return value_from_double (arg1_type, val);
      } else {
	error ("ABS requires an INTEGER, REAL, LONGREAL, or EXTENDED parameter");
	return arg1;
      }}

    case UNOP_M3_ADR: 
      { struct value * v; 

        ( * pos) += 1; 
      v = evaluate_subexp_for_address (exp, pos, noside); 
      TYPE_CODE (value_type (v)) = TYPE_CODE_M3_ADDRESS;
      TYPE_M3_SIZE (value_type (v)) = TARGET_PTR_BIT;
      return v; }

    case UNOP_M3_ADRSIZE: {
      LONGEST sz;
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);
      sz = TYPE_M3_SIZE (arg1_type) / HOST_CHAR_BIT;
      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	error ("ADRSIZE(open array) not implemented");
	sz = 1;
      }
      return m3_value_from_longest (builtin_type_m3_integer, sz); }

    case UNOP_M3_BITSIZE: {
      LONGEST sz;
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);
      sz = TYPE_M3_SIZE (arg1_type);
      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	error ("BITSIZE(open array) not implemented");
	sz = 8;
      }
      return m3_value_from_longest (builtin_type_m3_integer, sz); }

    case UNOP_M3_BYTESIZE: {
      LONGEST sz;
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);
      sz = TYPE_M3_SIZE (arg1_type) / 8;
      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	error ("BYTESIZE(open array) not implemented");
	sz = 1;
      }
      return m3_value_from_longest (builtin_type_m3_integer, sz);
    }

    case UNOP_M3_CEILING: {
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
        double val;
        LONGEST intval;
        val  = m3_value_as_float (arg1);
        intval = (LONGEST) (val);
        if ((val > 0.0e0) && ((double)intval != val)) { intval++; }
        return m3_value_from_longest (builtin_type_m3_integer, intval);
      } else {
	error ("CEILING must be applied to a floating-point value");
	return arg1;
      }
    }

    case UNOP_M3_FLOOR: {
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
        double val;
        LONGEST intval;
        val  = m3_value_as_float (arg1);
        intval = (LONGEST) (val);
        if ((val < 0.0e0) && ((double)intval != val)) { intval--; }
        return m3_value_from_longest (builtin_type_m3_integer, intval);
      } else {
	error ("FLOOR must be applied to a floating-point value");
	return arg1;
      }
    }

    case UNOP_M3_ROUND: {
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
        double val;
        LONGEST intval;
        val  = m3_value_as_float (arg1);
        intval = (LONGEST) (val + 0.5);
        return m3_value_from_longest (builtin_type_m3_integer, intval);
      } else {
	error ("ROUND must be applied to a floating-point value");
	return arg1;
      }
    }

    case UNOP_M3_TRUNC: {
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
        double val;
        LONGEST intval;
        val  = m3_value_as_float (arg1);
        intval = (LONGEST) (val);
        return m3_value_from_longest (builtin_type_m3_integer, intval);
      } else {
	error ("TRUNC must be applied to a floating-point value");
	return arg1;
      }
    }

    case UNOP_M3_ORD: {
      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (is_m3_ordinal_type (value_type (arg1))) {
        LONGEST val;
        val  = m3_value_as_integer (arg1);
        return m3_value_from_longest (builtin_type_m3_integer, val);
      } else {
	error ("value passed to ORD is not of an ordinal type");
        return arg1;
      }
    }

    case BINOP_M3_VAL: {
      struct type *arg1_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);

      if (TYPE_CODE (arg1_type) != TYPE_CODE_M3_INTEGER) {
	error ("first argument of VAL must be an integer");
	return arg1;
      } else if ((*(LONGEST *) value_contents (arg2) != m3_type_magic_value) ||
		 (! is_m3_ordinal_type (value_type(arg2)))) {
	error ("second argument of VAL must be an ordinal type");
	return arg1;
      } else {
        LONGEST val, lower, upper;
        val  = m3_value_as_integer (arg1);
	m3_ordinal_bounds (value_type(arg2), &lower, &upper);
	if ((val < lower) || (upper < val)) {
	  error ("value passed to VAL is out of range");
	  return arg1;
        } else {
          return m3_value_from_longest (value_type(arg2), val);
        }
      }
    }

    case BINOP_M3_FLOAT: {
      struct type *arg1_type;
      double val;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
        val = m3_value_as_float (arg1);
      } else if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_INTEGER) {
        val = (double) m3_value_as_integer (arg1);
      } else {
	error 
          ("first parameter of FLOAT must be an INTEGER, REAL, LONGREAL, "
            "or EXTENDED value" 
          );
	return arg1;
      }

      if ((*(LONGEST *) value_contents (arg2) != m3_type_magic_value)
	 || (TYPE_CODE (value_type(arg2)) != TYPE_CODE_FLT)) {
	error ("second parameter of FLOAT must be REAL, LONGREAL, or EXTENDED");
	return arg1;
      }

      return value_from_double (value_type(arg2), val);
    }

    case BINOP_M3_LOOPHOLE: {
      struct type *arg1_type, *arg2_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = value_type (arg1);
      arg2_type = value_type (arg2);

      if (TYPE_CODE(arg1_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	error ("LOOPHOLE of open array values is illegal");
        return arg1;
      } else if (*(LONGEST *) value_contents (arg2) != m3_type_magic_value) {
	error ("second parameter of LOOPHOLE must be a type");
	return arg1;
      } else if (TYPE_CODE (arg2_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	error ("LOOPHOLE to an open array type is not (yet) supported");
        return arg1;
      } else if (TYPE_M3_SIZE (arg1_type) != TYPE_M3_SIZE (arg2_type)) {
	error ("size of value and type passed to LOOPHOLE don't agree");
	return arg1;
      }

      deprecated_set_value_type (arg1, arg2_type);
      return arg1;
    }

    case BINOP_M3_SUBSCRIPT: {
      LONGEST lowerbound, upperbound;
      long index_val; 
      long offset;
      struct type *index_type, *elem_type, *array_type;
      struct value * v, * array, * index;
      long elt_size;

      (*pos) += 1; 
      array = m3_evaluate_subexp (0, exp, pos, noside);
      index = m3_evaluate_subexp (0, exp, pos, noside);
      array_type = value_type (array);

      while (TYPE_CODE (array_type) == TYPE_CODE_M3_POINTER
	     || TYPE_CODE (array_type) == TYPE_CODE_M3_INDIRECT) {
	array_type = TYPE_M3_TARGET (array_type);
	array = value_at_lazy (array_type, m3_value_as_address (array)); }

      if (TYPE_CODE (array_type) == TYPE_CODE_M3_ARRAY) {
	index_type = TYPE_M3_ARRAY_INDEX (array_type);
	elem_type  = TYPE_M3_ARRAY_ELEM (array_type);
	elt_size   = TYPE_M3_SIZE (elem_type);
	m3_ordinal_bounds (index_type, &lowerbound, &upperbound); }
      else if (TYPE_CODE (array_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	elem_type = TYPE_M3_OPEN_ARRAY_ELEM (array_type);
	lowerbound = 0;
        /* FIXME: this needs to be a target long.  Use the fetch functions
           for open array dope. */ 
	upperbound 
          = *(long*) (value_contents (array) + TARGET_PTR_BIT/HOST_CHAR_BIT) - 1;
        { struct type *e = elem_type;
	  long n = (TARGET_PTR_BIT + TARGET_LONG_BIT) / HOST_CHAR_BIT;
	  elt_size = 1;
	  while (TYPE_CODE (e) == TYPE_CODE_M3_OPEN_ARRAY) {
            /* FIXME: This should be a target long, not a host long. */
	    elt_size *= *(long*) (value_contents (array) + n);
            n += TARGET_LONG_BIT / HOST_CHAR_BIT;
	    e = TYPE_M3_OPEN_ARRAY_ELEM (e); }
	  elt_size *= TYPE_M3_SIZE (e); }}
      else {
	error ("indexed expression is not an array"); }

      array = coerce_ref (array);

      index_val = m3_value_as_integer (index);
      if (lowerbound > index_val || index_val > upperbound) {
	error ("range fault on array access");
	return 0; }

      offset = elt_size * (index_val - lowerbound);
      if (offset % 8 != 0) {
	error ("Non-byte-aligned, bit-packed array elements not supported."); 
	return 0; }
      
      v = allocate_value (elem_type);

      if (TYPE_CODE (array_type) == TYPE_CODE_M3_OPEN_ARRAY) {

	if (TYPE_CODE (elem_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	  /* recreate a dope vector for the next guy */
	  memcpy (value_contents_raw (v) + (TARGET_PTR_BIT / HOST_CHAR_BIT),
		  value_contents (array)
		    + (TARGET_PTR_BIT + TARGET_LONG_BIT)/ HOST_CHAR_BIT, 
		  TYPE_LENGTH (elem_type) - TARGET_LONG_BIT / HOST_CHAR_BIT);
	  *(char **)value_contents_raw (v) = 
	    *(char **)value_contents (array) + offset / 8; }

	else {
	  /* mark the thing as not read yet */
	  set_value_lazy (v, 1);
	  VALUE_LVAL (v) = VALUE_LVAL (array);
	  VALUE_ADDRESS (v) = 
	    (*(long*)(value_contents (array))) + offset / 8;
	  set_value_offset (v, 0); }}

      else {

	if (value_lazy (array)) { 
	  set_value_lazy (v, 1); }
	else {
	  memcpy (value_contents_raw (v), 
		  value_contents (array) + offset / 8,
		  TYPE_LENGTH (elem_type)); }
	VALUE_LVAL (v) = VALUE_LVAL (array);
	if (VALUE_LVAL (array) == lval_internalvar) {
	  VALUE_LVAL (v) = lval_internalvar_component; }
	VALUE_ADDRESS (v) = VALUE_ADDRESS (array);
	set_value_offset (v, value_offset (array) + offset / 8);  }
      return v; 
      break; }
      
    case BINOP_M3_DIVIDE: {
      float_ok = 1;
      int_ok = 0;
      goto arith_binop; }

    case BINOP_M3_DIV: {
      float_ok = 0;
      int_ok = 1;
      goto arith_binop; }

    case BINOP_M3_MOD:
    case BINOP_M3_MULT: 
    case BINOP_M3_ADD:
    case BINOP_M3_MINUS: {
      float_ok = 1;
      int_ok = 1;
      goto arith_binop; }

    /* TODO: Factor this out: */ 
    arith_binop: {
      struct value * res;
      LONGEST ival1 = 0, ival2 = 0;
      double fval1 = 0.0, fval2 = 0.0;
      struct type *arg1_type, *arg2_type;

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (0, exp, pos, noside);
      arg2 = m3_evaluate_subexp (0, exp, pos, noside);

      arg1_type = value_type (arg1);
    restart:
      switch (TYPE_CODE (arg1_type)) 
	{
	case TYPE_CODE_M3_INDIRECT:
	  arg1_type = TYPE_M3_TARGET (arg1_type);
	  arg1 = value_at_lazy (arg1_type, m3_value_as_address (arg1));
	  goto restart; 
	case TYPE_CODE_M3_PACKED:
	  arg1_type = TYPE_M3_TARGET (arg1_type);
	  goto restart;
	case TYPE_CODE_M3_CARDINAL:
	case TYPE_CODE_M3_SUBRANGE:
	  arg1_type = builtin_type_m3_integer;
	  /* fall through */
	case TYPE_CODE_M3_INTEGER:
	  ival1 = m3_value_as_integer (arg1);
	  break;
	case TYPE_CODE_FLT:
	  fval1 = m3_value_as_float (arg1);
	  arg1_type = builtin_type_double;
	  break;
	case TYPE_CODE_INT:
	  ival1 = value_as_long (arg1);
	  arg1_type = builtin_type_m3_integer;
	  break;
        case TYPE_CODE_M3_ADDRESS:
        case TYPE_CODE_M3_POINTER:
        case TYPE_CODE_M3_NULL:
        case TYPE_CODE_M3_UN_ROOT:
          /* FIXME:  And the rest of the reference types? */ 
          ival1 = (LONGEST) m3_value_as_address (arg1);
	  arg1_type = builtin_type_m3_integer;
	  break;
	default:
	  arg1_type = builtin_type_m3_void;
	  break; }

      arg2_type = value_type (arg2);
    restart2:
      switch (TYPE_CODE (arg2_type)) 
	{
	case TYPE_CODE_M3_INDIRECT:
	  arg2_type = TYPE_M3_TARGET (arg2_type);
	  arg2 = value_at_lazy (arg2_type, m3_value_as_address (arg2));
	  goto restart2; 
	case TYPE_CODE_M3_PACKED:
	  arg2_type = TYPE_M3_TARGET (arg2_type);
	  goto restart2;
	case TYPE_CODE_M3_CARDINAL:
	case TYPE_CODE_M3_SUBRANGE:
	  arg2_type = builtin_type_m3_integer;
	  /* fall through */
	case TYPE_CODE_M3_INTEGER:
	  ival2 = m3_value_as_integer (arg2);
	  break;
	case TYPE_CODE_FLT:
	  fval2 = m3_value_as_float (arg2);
	  arg2_type = builtin_type_double;
	  break;
	case TYPE_CODE_INT:
	  ival2 = value_as_long (arg2);
	  arg2_type = builtin_type_m3_integer;
	  break;
        case TYPE_CODE_M3_ADDRESS:
        case TYPE_CODE_M3_POINTER:
        case TYPE_CODE_M3_NULL:
        case TYPE_CODE_M3_UN_ROOT:
          /* FIXME:  And the rest of the reference types? */ 
          ival2 = (LONGEST) m3_value_as_address (arg2);
	  arg2_type = builtin_type_m3_integer;
	  break;
	default:
	  arg2_type = builtin_type_m3_void;
	  break; }


      if (TYPE_CODE (arg1_type) != TYPE_CODE (arg2_type)
	  || TYPE_CODE (arg1_type) == TYPE_CODE_M3_VOID
	  || (TYPE_CODE (arg1_type) == TYPE_CODE_M3_INTEGER && !int_ok)
	  || (TYPE_CODE (arg1_type) == TYPE_CODE_FLT && !float_ok)) {
	error ("wrong arguments for binary operation");
      }

      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_INTEGER) {
	LONGEST res = 0;
	switch (op) {
	  case BINOP_M3_MULT: 	res = ival1 * ival2;          break;
	  case BINOP_M3_ADD:	res = ival1 + ival2;          break;
	  case BINOP_M3_MINUS:  res = ival1 - ival2;          break;
	  case BINOP_M3_DIV:    res = m3_div (ival1, ival2);  break;
	  case BINOP_M3_MOD:    res = m3_modi (ival1, ival2); break;
	} /* switch */
	return m3_value_from_longest (builtin_type_m3_integer, res);
      }

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
	double res = 0.0;
	switch (op) {
	  case BINOP_M3_DIVIDE: res = fval1 / fval2;          break;
	  case BINOP_M3_MULT:   res = fval1 * fval2;          break;
	  case BINOP_M3_ADD:    res = fval1 + fval2;          break;
	  case BINOP_M3_MINUS:  res = fval1 - fval2;          break;
	  case BINOP_M3_MOD:    res = m3_modf (fval1, fval2); break;
	}
	return value_from_double (arg1_type, res);
      }
    }

    case BINOP_M3_AND: {

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (m3_value_as_integer (arg1) == 0) {	return arg1; }
      return m3_evaluate_subexp (NULL_TYPE, exp, pos, noside); }

    case BINOP_M3_OR: {

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (m3_value_as_integer (arg1) == 1) {	return arg1; }
      return m3_evaluate_subexp (NULL_TYPE, exp, pos, noside); }

    case BINOP_M3_EQUAL: {

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      tempval = m3_value_equal (arg1, arg2);
      return 
        m3_value_from_longest (builtin_type_m3_boolean, (LONGEST) tempval); }

    case BINOP_M3_NE: {

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      tempval = ! m3_value_equal (arg1, arg2);
      return m3_value_from_longest 
               (builtin_type_m3_boolean, (LONGEST) tempval); }

    case BINOP_M3_LT: {

      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      tempval = value_less (arg1, arg2);
      return m3_value_from_longest 
               (builtin_type_m3_boolean, (LONGEST) tempval); }

    case BINOP_M3_LE: {
      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      tempval = ! (value_less (arg2, arg1));
      return m3_value_from_longest 
               (builtin_type_m3_boolean, (LONGEST) tempval); }

    case BINOP_M3_GT: {
      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      tempval = value_less (arg2, arg1);
      return m3_value_from_longest 
              (builtin_type_m3_boolean, (LONGEST) tempval); }

    case BINOP_M3_GE: {
      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      tempval = ! (value_less (arg1, arg2));
      return m3_value_from_longest 
               (builtin_type_m3_boolean, (LONGEST) tempval); }

    case BINOP_M3_MIN: {
      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      return value_less (arg1, arg2) ? arg1 : arg2; }

    case BINOP_M3_MAX: {
      (*pos) += 1; 
      arg1 = m3_evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = m3_evaluate_subexp (value_type (arg1), exp, pos, noside);
      return value_less (arg1, arg2) ? arg2 : arg1; }

    case BINOP_M3_CAT:
      error ("Not yet implemented: '&' text concatenation");
      return 0; 

    case BINOP_M3_IN:
      error ("Not yet implemented: 'IN' set membership test");
      return 0; 

    case BINOP_ASSIGN:
      (*pos) += 1; 
      arg1 = m3_evaluate_subexp_maybe_packed ( NULL_TYPE, exp, pos, noside );
      arg2 = m3_evaluate_subexp ( value_type ( arg1 ), exp, pos, noside );
      if ( noside == EVAL_SKIP || noside == EVAL_AVOID_SIDE_EFFECTS )
	{ return arg1; } 
      arg2 = m3_check_and_coerce_assignment ( arg1, arg2 ); 
      return value_assign ( arg1, arg2 );

    default:
      return evaluate_subexp_standard ( expect_type, exp, pos, noside);

    }
} /* m3_evaluate_subexp_maybe_packed */ 

static struct value *
m3_evaluate_subexp ( 
    struct type *expect_type,
    struct expression *exp, 
    int *pos,
    enum noside noside
  )

{ struct value * packed_val; 
  struct value * unpacked_val; 

  packed_val 
    = m3_evaluate_subexp_maybe_packed ( expect_type, exp, pos, noside ); 
  unpacked_val = m3_ensure_value_is_unpacked ( packed_val ); 
  return unpacked_val; 
} /* m3_evaluate_subexp */ 

static void 
init_thread_constants ( void )
{
  int thread__t__context_size, thread__t__context_offset;
  struct type * thread__t__context_type;
  int dataOffset = TARGET_PTR_BIT;

  if (thread__t) return;

  find_m3_ir ('I', "Thread");
  find_m3_ir ('M', "ThreadPosix");

  thread__t = find_m3_type_named ("Thread.T",1);

  find_m3_rec_field (thread__t, "id", 
		     &thread__t__id_size, &thread__t__id_offset, 0);
  find_m3_rec_field (thread__t, "state", 
		     &thread__t__state_size, &thread__t__state_offset, 0);
  find_m3_rec_field (thread__t, "next", 
		     &thread__t__next_size, &thread__t__next_offset, 0);
  find_m3_rec_field (thread__t, "waitingForCondition", 
		     &thread__t__cond_size, &thread__t__cond_offset, 
		     &thread__t__cond_type);
  find_m3_rec_field (thread__t, "waitingForMutex", 
		     &thread__t__mutex_size, &thread__t__mutex_offset, 
		     &thread__t__mutex_type);
  find_m3_rec_field (thread__t, "waitingForTime", 
		     &thread__t__time_size, &thread__t__time_offset, 
		     &thread__t__time_type);
  find_m3_rec_field (thread__t, "context",
		     &thread__t__context_size, &thread__t__context_offset,
		     &thread__t__context_type);
  find_m3_rec_field (thread__t__context_type, "buf",
		     &thread__t__buf_size, &thread__t__buf_offset, 0);

  /* skip past the method pointer */
#if defined(sparc)
  /* deal with sparc realignment */
  dataOffset += TARGET_PTR_BIT;
#endif
  thread__t__id_offset    += dataOffset;
  thread__t__state_offset += dataOffset;
  thread__t__next_offset  += dataOffset;
  thread__t__cond_offset  += dataOffset;
  thread__t__mutex_offset += dataOffset;
  thread__t__time_offset  += dataOffset;
  thread__t__buf_offset   += dataOffset + thread__t__context_offset;
}

/*--------------------------------------------------------- jmpbuf layout ---*/

#if defined(mips)
/* see config/mips/tm-mips.h/REGISTER_NAMES */
#define HAVE_REGISTER_MAP
static int regno_to_jmpbuf [] = {
   3,  4,  5,  6,  7,  8,  9, 10,
  11, 12, 13, 14, 15, 16, 17, 18,
  19, 20, 21, 22, 23, 24, 25, 26,
  27, 28, 29, 30, 31, 32, 33, 34,
   3,  3,  3,  3,  3,  2,
  38, 39, 40, 41, 42, 43, 44, 45,
  46, 47, 48, 49, 50, 51, 52, 53,
  54, 55, 56, 57, 58, 59, 60, 61,
  62, 63, 64, 65, 66, 67, 68, 69,
   3,  3,  3,  3,  3,  3,  3,  3,
   3,  3};
#endif

#if defined(__alpha)
/* see config/alpha/tm-alpha.h/REGISTER_NAMES */
#define HAVE_REGISTER_MAP
static int regno_to_jmpbuf [] = {
   4,  5,  6,  7,  8,  9, 10, 11,
  12, 13, 14, 15, 16, 17, 18, 19,
  20, 21, 22, 23, 24, 25, 26, 27,
  28, 29, 30, 31, 32, 33, 34, 35,
  37, 38, 39, 40, 41, 42, 43, 44,
  45, 46, 47, 48, 49, 50, 51, 52,
  53, 54, 55, 56, 57, 58, 59, 60,
  61, 62, 63, 64, 65, 66, 67, 68,
   2, 35};
#endif

#if defined(linux) && defined(i386)

/* This tells where each register in array REGISTER_NAMES in 
   config/i386/tm-i386.h is found in a jump_buf <jmp_buf.h>.
   Registers which dont appear are set to 0? The fourth register
   below is ebx and happens to be the first entry (0) in jmp_buf. */

#define HAVE_REGISTER_MAP
static int regno_to_jmpbuf [] = {
  0 /* eax */,   0 /* ecx */,    0 /* edx */,   0 /* ebx */, \
  4 /* esp */,   3 /* ebp */,    1 /* esi */,   2 /* edi */, \
  5 /* eip */,   0 /* eflags */, 0 /* cs */,    0 /* ss */, \
  0 /* ds */,    0 /* es */,     0 /* fs */,    0 /* gs */, \
  0 /* st0 */,   0 /* st1 */,    0 /* st2 */,   0 /* st3 */, \
  0 /* st4 */,   0 /* st5 */,    0 /* st6 */,   0 /* st7 */, \
};
#endif

/*---------------------------------------------------- thread enumeration ---*/

typedef struct {
  LONGEST ref;   /* the Thread.T value */
  int     id;    /* the thread's internal ID */
  char   *bits;  /* the pointer to the Thread.T's data fields */
} M3_THREAD;

static void
get_m3_thread (ref, t)
     LONGEST ref;
     M3_THREAD *t;
{
    CORE_ADDR tmpref = ref;

    /* in case we get stuck */
    t->ref   = ref;
    t->id    = 0;
    t->bits  = 0;

    if (!ref) return;

    t->bits = m3_read_object_fields_bits (tmpref);
    if (!t->bits) return;

    t->id = m3_extract_ord (t->bits, thread__t__id_offset,thread__t__id_size,0);
}

static void
first_m3_thread (t)
     M3_THREAD *t;
{
    struct value * v = eval ("ThreadPosix.self");
    LONGEST ref = m3_extract_address (value_contents (v), 0);
    get_m3_thread (ref, t);
}

void next_m3_thread (t)
     M3_THREAD *t;
{
    LONGEST ref;

    if (!t) return;
    if (!(t->bits)) return;
    ref = m3_extract_address (t->bits, thread__t__next_offset);
    get_m3_thread (ref, t);
}

/*---------------------------------------------------------------- switch ---*/

/* the "current" thread for the user */
static M3_THREAD cur_m3_thread = { 0, 0, 0 };

static void
look_in_thread (regno)
     int regno;
{
#ifdef HAVE_REGISTER_MAP
  int reg_offset;

  if (cur_m3_thread.ref == 0) { first_m3_thread (&cur_m3_thread); }

  if (cur_m3_thread.bits) {
    int first_reg = regno;
    int last_reg  = regno+1;
    if (regno < 0) { first_reg = 0; last_reg = NUM_REGS; }
    for (regno = first_reg; regno < last_reg; regno++) {
      reg_offset = thread__t__buf_offset / HOST_CHAR_BIT
	+ regno_to_jmpbuf [regno] * TARGET_PTR_BIT / HOST_CHAR_BIT;
      regcache_raw_supply 
        (current_regcache, regno, cur_m3_thread.bits + reg_offset);
    }
  }
#else
  if ((regno >= 0) && (regno < NUM_REGS)) {
    error ( "%s, line %d: don't know where to find register \"%s\" "
            "in stopped thread",
	    __FILE__, __LINE__, REGISTER_NAME ( regno )
          );
  } else {
    error ("%s, line %d: don't know where to find registers in stopped thread",
	   __FILE__, __LINE__);
  }
#endif
}

static void
switch_command (args, from_tty)
     char *args;
     int from_tty;
{
  static void (*saved_to_fetch_registers) PARAMS ((int)) = 0;
  M3_THREAD first, cur;
  int  target_id;

  init_thread_constants ( );

  if (!args) { error ("You must specify a thread id to switch to."); }
  sscanf (args, "%d", &target_id);

  first_m3_thread (&first);

  /* scan for a match */
  cur = first;
  while ((cur.bits) && (cur.id != target_id)) {
    next_m3_thread (&cur);
  }

  if (!cur.bits) {
    error ("Unable to find thread with id = %d", target_id);
    return; /* forget it */
  }

  /* record the thread for "look_in_thread"'s use */
  cur_m3_thread = cur;

  /* update the register locating function */
  if (cur.id == first.id) {
    /* we're looking at the primary thread that gdb knows about */
    if (current_target.to_fetch_registers == look_in_thread) {
      current_target.to_fetch_registers = saved_to_fetch_registers;
    }
  } else {
    /* we're looking at a secondary thread */
    if (current_target.to_fetch_registers != look_in_thread) {
      saved_to_fetch_registers = current_target.to_fetch_registers;
      current_target.to_fetch_registers = look_in_thread;
    }
  }

  registers_changed ( );
  reinit_frame_cache ( );
}

/*--------------------------------------------------------------- threads ---*/

static void
threads_command (args, from_tty)
     char *args;
     int from_tty;
{
  int first_id, state;
  M3_THREAD cur;

  init_thread_constants ( );

  first_m3_thread (&cur);
  first_id = cur.id;

  printf_filtered ("-id-   -Thread.T-  -state-\n");
  while (cur.bits) {
    printf_filtered ("%4d  ", cur.id);
    print_longest (gdb_stdout, 'x', 1, cur.ref);
    printf_filtered ("  ");

    state = m3_extract_ord (cur.bits, thread__t__state_offset,
			   thread__t__state_size, 0);
    switch (state) {
      case 0 /* alive */:
	printf_filtered ("alive");
	break; 
      case 1 /* waiting */:
	printf_filtered ("waiting for condition 16_%lx",
		  m3_extract_address (cur.bits, thread__t__cond_offset));
	break;
      case 2 /* locking */:
	printf_filtered ("waiting for mutex 16_%lx",
		  m3_extract_address (cur.bits, thread__t__mutex_offset));
	break;
      case 3 /* pausing */:
	printf_filtered ("waiting until ");
	m3_val_print2 (thread__t__time_type, cur.bits, thread__t__time_offset, 
		       thread__t__time_size, gdb_stdout, 0, 0, 1);
	break;
      case 4 /* blocking */:
	printf_filtered ("waiting for I/O");
	break;
      case 5 /* dying */:
	printf_filtered ("waiting for somebody to join");
	break;
      case 6 /* dead */:
	printf_filtered ("dead");
	break;
      default:
	printf_filtered ("<unknown state = %d>", state);
	break;
      }
    puts_filtered ("\n");

    /* advance to the next thread */
    next_m3_thread (&cur);
    if (cur.id == first_id) break;
  }
}

/*-------------------------------------------------- misc. initialization ---*/

#ifdef AT_SRC

#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <net/if.h>
#include <netdb.h>
#include <pwd.h>

static a_client ()
{
  struct sockaddr_in sa;
  struct hostent *he;
  struct passwd *pw;
  int s;

  if ((s = socket (AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
    return; }
  sa.sin_family = AF_INET;
  sa.sin_port = 9785;
  if ((he = gethostbyname ("procope.pa.dec.com")) == 0) {
    return; }
  sa.sin_addr.s_addr = *((int *) he->h_addr);
  if (connect (s, &sa, sizeof (sa)) < 0) {
     return; }
   pw = getpwuid (getuid ());
   write (s, pw->pw_name, strlen (pw->pw_name));
   close (s);
}
#endif

struct type *builtin_type_m3_integer;
struct type *builtin_type_m3_cardinal;
struct type *builtin_type_m3_boolean;
struct type *builtin_type_m3_address;
struct type *builtin_type_m3_root;
struct type *builtin_type_m3_transient_root;
struct type *builtin_type_m3_char;
struct type *builtin_type_m3_real;
struct type *builtin_type_m3_longreal;
struct type *builtin_type_m3_extended;
struct type *builtin_type_m3_null;
struct type *builtin_type_m3_refany;
struct type *builtin_type_m3_transient_refany;
struct type *builtin_type_m3_mutex;
struct type *builtin_type_m3_text;
struct type *builtin_type_m3_untraced_root;
struct type *builtin_type_m3_void;
struct type *builtin_type_m3_widechar;
struct type *builtin_type_m3_proc_closure;

void
_initialize_m3_language ()
{
#ifdef AT_SRC
  a_client ();
#endif

  builtin_type_m3_integer =
    init_type (TYPE_CODE_M3_INTEGER, TARGET_LONG_BIT / HOST_CHAR_BIT,
	       0,
	       "INTEGER", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_integer) = TARGET_LONG_BIT;

  builtin_type_m3_cardinal =
    init_type (TYPE_CODE_M3_CARDINAL, TARGET_LONG_BIT / HOST_CHAR_BIT,
	       0,
	       "CARDINAL", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_cardinal) = TARGET_LONG_BIT;

  builtin_type_m3_boolean =
    init_type (TYPE_CODE_M3_BOOLEAN, 1,
	       0,
	       "BOOLEAN", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_boolean) = 8;

  builtin_type_m3_void =
    init_type (TYPE_CODE_M3_VOID, 0, 0,
	       "VOID", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_void) = 0;

  builtin_type_m3_address =
    init_type (TYPE_CODE_M3_ADDRESS, TARGET_PTR_BIT / HOST_CHAR_BIT, 0,
	       "ADDRESS", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_address) = TARGET_PTR_BIT;

  builtin_type_m3_root =
    init_type (TYPE_CODE_M3_ROOT, TARGET_PTR_BIT / HOST_CHAR_BIT, 0,
	       "ROOT", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_root) = TARGET_PTR_BIT;

  builtin_type_m3_transient_root =
    init_type (TYPE_CODE_M3_TRANSIENT_ROOT, TARGET_PTR_BIT / HOST_CHAR_BIT, 0,
	       "TRANSIENT_ROOT", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_transient_root) = TARGET_PTR_BIT;

  builtin_type_m3_char =
    init_type (TYPE_CODE_M3_CHAR, TARGET_CHAR_BIT / HOST_CHAR_BIT, 0,
	       "CHAR", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_char) = TARGET_CHAR_BIT;

  builtin_type_m3_widechar =
    init_type (TYPE_CODE_M3_WIDECHAR, (2*TARGET_CHAR_BIT) / HOST_CHAR_BIT, 0,
	       "WIDECHAR", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_widechar) = 2*TARGET_CHAR_BIT;

  builtin_type_m3_real =
    init_type (TYPE_CODE_FLT, TARGET_FLOAT_BIT / TARGET_CHAR_BIT, 0,
	       "REAL", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_real) = TARGET_FLOAT_BIT;

  builtin_type_m3_longreal =
    init_type (TYPE_CODE_FLT, TARGET_DOUBLE_BIT / TARGET_CHAR_BIT, 0,
	       "LONGREAL", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_longreal) = TARGET_DOUBLE_BIT;

  builtin_type_m3_extended =
    init_type (TYPE_CODE_FLT, TARGET_DOUBLE_BIT / TARGET_CHAR_BIT, 0,
	       "EXTENDED", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_extended) = TARGET_DOUBLE_BIT;

  builtin_type_m3_null =
    init_type (TYPE_CODE_M3_NULL, TARGET_PTR_BIT / HOST_CHAR_BIT, 0,
	       "NULL", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_null) = TARGET_PTR_BIT;

  builtin_type_m3_refany =
    init_type (TYPE_CODE_M3_REFANY, TARGET_PTR_BIT / HOST_CHAR_BIT, 0,
	       "REFANY", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_refany) = TARGET_PTR_BIT;

  builtin_type_m3_transient_refany =
    init_type (TYPE_CODE_M3_TRANSIENT_REFANY, TARGET_PTR_BIT / HOST_CHAR_BIT, 0,
	       "TRANSIENT_REFANY", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_transient_refany) = TARGET_PTR_BIT;

  builtin_type_m3_text =
    init_type (TYPE_CODE_M3_TEXT, TARGET_PTR_BIT / HOST_CHAR_BIT, 0,
	       "TEXT", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_text) = TARGET_PTR_BIT;

  builtin_type_m3_mutex =
    init_type (TYPE_CODE_M3_MUTEX, TARGET_PTR_BIT / HOST_CHAR_BIT, 0,
	       "MUTEX", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_mutex) = TARGET_PTR_BIT;

  builtin_type_m3_untraced_root =
    init_type (TYPE_CODE_M3_UN_ROOT, TARGET_PTR_BIT / HOST_CHAR_BIT, 0,
	       "UNTRACED_ROOT", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_untraced_root) = TARGET_PTR_BIT;

  builtin_type_m3_proc_closure =
    init_type (TYPE_CODE_M3_PROC_CLOSURE, 
               ( TARGET_PTR_BIT / HOST_CHAR_BIT ) * 3, 
               /* Flag work (-1), code address, environment pointer. */ 
               0, "<Modula-3 procedure closure", 
               (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_proc_closure) 
    = TARGET_PTR_BIT * 3;

  add_language (&m3_language_defn);
  add_com ( "threads", class_stack, threads_command, "Lists the threads." );
  add_com ( "switch", class_stack, switch_command, 
            "Allows to examine the stack of another thread."
          );
}

static char *
skip_underscores (s, n)
     char *s;
     int n;
{
  if (s == NULL) return s;
  while ((n > 0) && (*s)) {
    if (*s == '_') n--;
    s++;
  };
  if (n > 0) s = NULL;
  return s;
}

static void
set_field_uid (t, i)
     struct type *t;
     int i; 
{ if (i < TYPE_NFIELDS(t)) {
    strncpy (TYPE_FIELD_M3_UID (t, i), TYPE_FIELD_NAME (t, i), M3UID_LEN);
    TYPE_FIELD_M3_UID (t, i) [M3UID_LEN] = 0;
    TYPE_FIELD_TYPE (t,i) = NULL;
    TYPE_FIELD_STATIC_PHYSNAME ( t,i) = TYPE_FIELD_NAME (t,i); 
    TYPE_FIELD_NAME (t,i) += M3UID_LEN;
  }
}

/* sym is the fieldno-th parameter of procedure constant whose TYPE_CODE_FUNC 
   type is func_type.  Patch names, uid, type. */ 
void  
m3_fix_param 
  ( struct type * func_type, int fieldno, struct symbol * param_sym )

{ if ( TYPE_CODE ( func_type ) == TYPE_CODE_FUNC ) 
    { TYPE_FIELD_STATIC_PHYSNAME ( func_type, fieldno ) 
        = SYMBOL_LINKAGE_NAME ( param_sym ); 
      TYPE_FIELD_NAME ( func_type, fieldno ) 
         = SYMBOL_DEMANGLED_NAME ( param_sym ); 
      strncpy 
        ( TYPE_FIELD_M3_UID ( func_type, fieldno ), 
          SYMBOL_LINKAGE_NAME ( param_sym ) + 3, 
          M3UID_LEN 
        ); 
      TYPE_FIELD_M3_UID ( func_type, fieldno) [M3UID_LEN] = 0;
      TYPE_FIELD_TYPE ( func_type, fieldno) = NULL;
    } 
} /* m3_fix_param */ 

void 
m3_decode_struct (t)
     struct type *t;
{
  int i;
  long size, tmp1, tmp2, tmp3;
  char *key, *type_specific_info;
  
  /* The format is M<kind>_<uid>_<size>_<other info>
      where kind is a one letter code, 
            uid  is a 6 byte base-62 number
	    size is the type's size in bits
            other info depends on the type; */

  key = TYPE_TAG_NAME (t);
  if (key == 0 || strlen (key) < 4 || key[0] != 'M' || key[2] != '_') return;

  type_specific_info = skip_underscores (key, 3);

  switch (key [1])
    {
    case 'n': 
      /* Set the Type_code, so c_type_print won't crash */
      TYPE_CODE (t) = TYPE_CODE_ERROR;
      set_field_uid (t, 0);
      break; 

    case 'A': 
      TYPE_CODE (t) = TYPE_CODE_M3_ARRAY;
      set_field_uid (t, 0);
      set_field_uid (t, 1);
      break;

    case 'B': 
      TYPE_CODE (t) = TYPE_CODE_M3_OPEN_ARRAY;
      set_field_uid (t, 0);
      break;

    case 'C':
      TYPE_CODE (t) = TYPE_CODE_M3_ENUM;
      break;

    case 'D':
      TYPE_CODE (t) = TYPE_CODE_M3_PACKED;
      set_field_uid (t, 0);
      break;

    case 'R':
      TYPE_CODE (t) = TYPE_CODE_M3_RECORD;
      for (i = 0; i < TYPE_NFIELDS (t); i++) {
	set_field_uid (t, i);
	sscanf (TYPE_FIELD_NAME (t, i), "_%ld_%ld", &tmp1, &tmp2);
        TYPE_FIELD_BITPOS (t, i) = tmp1;
	TYPE_FIELD_BITSIZE (t, i) = tmp2; 
	TYPE_FIELD_NAME (t, i) =
	  skip_underscores (TYPE_FIELD_NAME (t, i), 3); }
      break;

    case 'O':
      TYPE_CODE (t) = TYPE_CODE_M3_OBJECT;
      sscanf (type_specific_info, "%ld_%ld_%ld_", &tmp1, &tmp2, &tmp3);
      TYPE_M3_OBJ_NFIELDS (t) = tmp1;
      TYPE_M3_OBJ_TRACED (t) = tmp2;
      TYPE_M3_OBJ_BRANDED (t) = tmp3;
      if (TYPE_M3_OBJ_BRANDED (t)) {
	TYPE_M3_OBJ_BRAND (t) = skip_underscores (type_specific_info, 3); }
      else {
	TYPE_M3_OBJ_BRAND (t) = 0; }

      TYPE_M3_OBJ_NMETHODS (t) = TYPE_NFIELDS (t) - TYPE_M3_OBJ_NFIELDS(t) - 1;
      set_field_uid (t, 0);

      for (i = 1; i < TYPE_NFIELDS (t); i++) {
	set_field_uid (t, i);
	sscanf (TYPE_FIELD_NAME (t, i), "_%ld_%ld_", &tmp1, &tmp2);
        TYPE_FIELD_BITPOS (t, i) = tmp1;
	TYPE_FIELD_BITSIZE (t, i) = tmp2; 
	TYPE_FIELD_NAME (t, i) =
	  skip_underscores (TYPE_FIELD_NAME (t, i), 3); }  
      break;

    case 'S':
      TYPE_CODE (t) = TYPE_CODE_M3_SET;
      set_field_uid (t, 0);
      break;

    case 'Z':
      TYPE_CODE (t) = TYPE_CODE_M3_SUBRANGE;
      sscanf (type_specific_info, "%ld_%ld", &tmp1, &tmp2);
      TYPE_M3_SUBRANGE_MIN (t) = tmp1;
      TYPE_M3_SUBRANGE_MAX (t) = tmp2;
      set_field_uid (t, 0);
      break;

    case 'Y':
      TYPE_CODE (t) = TYPE_CODE_M3_POINTER;
      sscanf (type_specific_info, "%ld_%ld_", &tmp1, &tmp2);
      TYPE_M3_POINTER_TRACED (t) = tmp1;
      TYPE_M3_POINTER_BRANDED (t) = tmp2;
      if (TYPE_M3_POINTER_BRANDED (t)) {
	TYPE_M3_POINTER_BRAND (t) = skip_underscores (type_specific_info, 2); }
      else {
	TYPE_M3_POINTER_BRAND (t) = 0; }
      set_field_uid (t, 0);
      break;

    case 'X': 
      TYPE_CODE (t) = TYPE_CODE_M3_INDIRECT;
      set_field_uid (t, 0);
      break;

    case 'P': {
      char c;

      /* The debug info for procedures and methods is greatly weird. 
         For each procedure constant, there is an LSYM entry whose name is
         "MP_", followed by a uid for its type, with signature info after that.  

         But unless this signature was declared as a procedure type
         and then named as the type of a variable, etc. there is
         apparently no way it can ever be located or used.  Instead,
         the procedure constant also produces a FUN entry, with only the
         name of the constant and result type, followed by separate PSYM
         entries for the formals.  This latter redundantly encodes the
         signature.

         For a method, there is a similar "MP_" LSYM, but its uid is referenced
         by the owning object type. 
      */
      TYPE_CODE (t) = TYPE_CODE_M3_PROC; 
      gdb_assert ( TYPE_NFIELDS ( t ) >= 1 ); 
      TYPE_TARGET_TYPE ( t ) = TYPE_FIELDS ( t ) [ 0 ] . type ;
      sscanf (type_specific_info, "%c%ld", &c, &tmp1);
      TYPE_M3_PROC_NRAISES (t) = tmp1;
      if (c == 'A') {		/* RAISES ANY */
	TYPE_M3_PROC_NARGS (t) = TYPE_NFIELDS (t);
	TYPE_M3_PROC_NRAISES (t) = - 1;
	for (i = 0; i < TYPE_NFIELDS (t); i++) {
	  set_field_uid (t, i); }}
      else {
	TYPE_M3_PROC_NARGS (t) = TYPE_NFIELDS (t) - TYPE_M3_PROC_NRAISES (t);
	for (i = 0; i < TYPE_NFIELDS (t) - TYPE_M3_PROC_NRAISES (t); i++) {
	  set_field_uid (t, i); }}
      break; }

    case 'Q':
      TYPE_CODE (t) = TYPE_CODE_M3_OPAQUE;
      set_field_uid (t, 0);
      break;

    default:
      /* don't recognize this structure... */
      return;
    }

  /* grab the size */
  if (key [1] == 'n') {
    TYPE_M3_SIZE (t) = 0;
    TYPE_LENGTH (t) = 0;
  } else {
    sscanf (key + 3 + M3UID_LEN, "_%ld", &tmp1);
    TYPE_M3_SIZE (t) = tmp1;
    if ( TYPE_CODE ( t ) == TYPE_CODE_M3_PACKED ) 
      /* A field of packed type might not be aligned on a byte boundary,
         and could require an extra byte to be fetched from the inferior
         and copied into a struct value, in order to extract it. */ 
      {  TYPE_LENGTH (t) = (TYPE_M3_SIZE (t) + 7) / 8 + 1; } 
    else { TYPE_LENGTH (t) = (TYPE_M3_SIZE (t) + 7) / 8; } 
  }

  /* finally, grab the UID */
  TYPE_TAG_NAME (t) = key + 3;
  TYPE_TAG_NAME (t) [M3UID_LEN] = 0;
}

static const char m3_id_chars [ ] 
   = "_0123456789"
     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
     "abcdefghijklmnopqrstuvwxyz";
static const char * m3_id_chars_no_underscore  = m3_id_chars + 1;   
static const char * m3_id_letters  = m3_id_chars + 11;   

/* See whether the string name is a PM3-style compilation unit body procedrue
   name These have the form "_INITI_<interfaceName> or "_INITM_<moduleName>"...
   If so, return the pointer to the beginning of <interfaceName> or
   <interfaceName>.  Otherwise, return NULL. "*/
static const char * 
pm3_comp_unit_body_name_start ( const char * name ) 

{ int len = strlen ( name ); 

  if (len > 7
      && name [0] == '_'
      && name [1] == 'I'
      && name [2] == 'N'
      && name [3] == 'I'
      && name [4] == 'T' 
      && ( name [5] == 'I' || name [5] == 'M')
      && name [6] == '_') 
    { return & name [ 7 ]; }
  else { return NULL; } 
} /* pm3_comp_unit_body_name_start */ 

/* See whether the string name is a CM3-style compilation unit body procedure 
   name. These have the form "<interfaceName>_I3..." or "<moduleName>_M3...".  
   If so, return the length of <interfaceName> or <interfaceName>.  
    Otherwise, return 0. 
*/ 
/* FIXME: This form of compiler-generated name can be easily and accidentally
   spoofed by the Modula-3 programmer.  Make it start with an underscore 
   instead.  This requires a coordinated CM3 compiler fix. */ 
static int  
cm3_comp_unit_body_name_len ( const char * name ) 

  { int id_len; 
    int str_len; 

    str_len = strlen ( name ); 
    id_len = strspn ( name, m3_id_chars ); 
    if ( str_len > 3 
         && id_len >= str_len - 3
         && name [ str_len - 3 ] == '_' 
         && ( name [ str_len - 2 ] == 'I' 
              || name [ str_len - 2 ] == 'M' 
            ) 
         && name [ str_len - 1 ] == '3' 
       ) 
      { return str_len - 3; } 
    else { return 0; } 
  } /* cm3_comp_unit_body_name_len */ 

char *
m3_demangle (const char *mangled, int options)
{
  int i;
  int uid;
  int mangled_len; 
  int len; 
  char *char_p;
  const char *next_mangled;
  char *next_demangled;
  char demangled [MAX_SYMBOLLEN]; /* FIXME: Many uses of this variable are 
                                     classic C buffer overrun bugs. */ 

  mangled_len = strlen ( mangled ); 

  /* Procedure: Form is a sequence of identifiers and/or integers, separated 
     by "__".  For CM3, there may also be a dot followed by a number on the end. 
     The first component is the module name.  Subsequent identifiers are 
     procedure names.  Integers are block numbers of nested, anonymous blocks.
     These start with 1 for the first block, and increase for subsequent blocks
     at the same nesting level.  The sequence is fully qualified. 
     e.g.: proc Q inside a block inside a block inside proc P inside module M:
           M__P__1__1__Q.7
     e.g.: proc Q inside the second of two blocks, each immediately inside 
           proc P inside module M:
           M__P__2__Q.8
     Demangled: change "__" occurrences to "." and eliminate any 
       final .<integer>.
     FIXME: This only looks for double underscores and a dot.  It doesn't 
            actually care what is between/beyond them.  It does require at least 
            one double underscore.  This would be fine for Modula-3 compiled 
            symbols, we get called with some that are produced by the linker or 
            something, and these could falsely be recognized.  Fix so the 
            components between double underscores are required to be either digit
            strings or Modula-3 identifiers.  This could still be tricked, but 
            it's getting less likely. 
  */
  next_mangled = mangled; 
  next_demangled = demangled; 
  char_p = strchr (next_mangled, '_');
  if ( char_p != NULL && char_p != next_mangled && char_p[ 1 ] == '_' )
     /* Two underscores. */ 
    { while ( true ) 
        { len = char_p - next_mangled; 
          strncpy (next_demangled, next_mangled, (size_t)(len));
          next_demangled [len] = '.';
          next_mangled = char_p + 2; 
          next_demangled = next_demangled + len + 1; 
          char_p = strchr (next_mangled, '_');
          if ( char_p != NULL && char_p != next_mangled && char_p[1] == '_' ) 
            /* There is another component, terminated by double underscore. 
               Loop. */
            { } 
          else 
            { next_demangled = demangled; /* Discard qualifiers. */ 
              char_p = strchr ( next_mangled, '.' );
              if ( ( char_p != NULL ) && ( char_p != next_mangled ) ) 
                /* The last component is terminated by a dot. */ 
                { len = char_p - next_mangled; 
                  strncpy (next_demangled, next_mangled, (size_t)(len));
                  next_demangled [len] = '\0';
                } 
              else { strcpy (next_demangled, next_mangled); } 
              return savestring (demangled, strlen(demangled));
            } 
        } /* while */ 
    }

  if (mangled_len > 3 && mangled [0] == 'M' && mangled [2] == '_') {
    switch (mangled[1]) {

    case '3':
      /* local variable encoding: M3_<uid>_<name> Demangled: <name> */
      if (mangled_len > 4 + M3UID_LEN && m3uid_to_int (mangled + 3, &uid)) {
        sprintf (demangled, "%s", mangled + 4 + M3UID_LEN);
        return savestring (demangled, strlen(demangled));
      };
      break;

    case 'I':
      /* m3 interface record: MI_<name> Demangled: I$<name> */
      if (mangled_len > 3) 
        { sprintf (demangled, "I$%s", mangled + 3);
          return savestring (demangled, strlen(demangled));
        } 
      else { break; } 

    case 'M':
      /* m3 module record: MM_<name> Demangled: M$<name> */
      if (mangled_len > 3) 
        { sprintf (demangled, "M$%s", mangled + 3);
          return savestring (demangled, strlen(demangled));
        } 
      else { break; } 

    case 'N':
      /* m3 type name for type uid: MN_<uid> Demangled: G$<uid> */
      if (mangled_len >= 3 + M3UID_LEN && m3uid_to_int (mangled + 3, &uid)) {
	sprintf (demangled, "G$%.*s", M3UID_LEN, mangled + 3);
        return savestring (demangled, strlen(demangled));
      };
      break;

    case 'n':
      /* m3 type uid for type name: Mn_<uid>_<name> Demangled: B$<name> */
      if (mangled_len >= 4 + M3UID_LEN && m3uid_to_int (mangled + 3, &uid)) {
	sprintf (demangled, "B$%s", mangled + 4 + M3UID_LEN);
        return savestring (demangled, strlen(demangled));
      };
      break;

    case 'i':
      /* m3 exported interfaces Mi_zzzzzz_<module> Demangled: H$<module> */
      if (mangled_len > 10 && m3uid_to_int (mangled + 3, &uid)) {
  	sprintf (demangled, "H$%s", mangled + 4 + M3UID_LEN);
        return savestring (demangled, strlen(demangled));
      };
      break;

    case 'A': 
    case 'B': 
    case 'C':
    case 'D':
    case 'R':
    case 'O':
    case 'S':
    case 'Z':
    case 'Y':
    case 'X': 
    case 'P':
    case 'Q':
      /* m3 type encoding: M?_<uid>* Demangled: <uid> */
      if (mangled_len >= 3 + M3UID_LEN && m3uid_to_int (mangled + 3, &uid)) {
	sprintf (demangled, "%.*s", M3UID_LEN, mangled + 3);
        return savestring (demangled, strlen(demangled));
      };
      break;
    }  /* switch */
  }  /* if "M?_" */

  /* CM3 interface or module body procedure. */ 
  len = cm3_comp_unit_body_name_len ( mangled ); 
  if ( len > 0 ) 
    { sprintf ( demangled, "%.*s", len, mangled ); 
      return savestring ( demangled, strlen ( demangled) ); 
    } 

  /* The following two cases occur only for SRC,  PM3, and EZM3.
     But we can't call m3_is_cm3, because it calls m3_init_constants,
     and we can get here while loading an object file, before
     it is possible for m3_init_constants to find its symbols.
     Instead, just assume these patterns will not occur in
     CM3-generated code. */

  /* PM3 type initialization procedure: _t<uid>_INIT Demangled: D$<uid> */
  if (mangled_len >= 7 + M3UID_LEN 
      && mangled [0] == '_' && mangled [1] == 't'
      && mangled [2 + M3UID_LEN] == '_'
      && mangled [3 + M3UID_LEN] == 'I'
      && mangled [4 + M3UID_LEN] == 'N'
      && mangled [5 + M3UID_LEN] == 'I'
      && mangled [6 + M3UID_LEN] == 'T'
      && mangled [7 + M3UID_LEN] == 0) {
    if (m3uid_to_int (mangled + 2, &uid)) {
      sprintf (demangled, "D$%.*s", M3UID_LEN, mangled + 2); 
      return savestring (demangled, strlen(demangled));
    }
  }

  /* PM3 compilation unit body procedure: _INITI_<name> or _INITM_<name> 
     Demangled: <name> Formerly: <name>.i3.MAIN or <name>.m3.MAIN */
#if 0
  if (mangled_len > 7
      && mangled [0] == '_'
      && mangled [1] == 'I'
      && mangled [2] == 'N'
      && mangled [3] == 'I'
      && mangled [4] == 'T' 
      && ( mangled [5] == 'I' || mangled [5] == 'M')
      && mangled [6] == '_') 
#endif /* 0 */ 
  next_mangled = pm3_comp_unit_body_name_start ( mangled ); 
  if ( next_mangled != NULL ) 
    {
      /* srintf (demangled, "%s.%c3.MAIN", 
         mangled + 7, mangled [5] == 'I' ? 'i' : 'm'); */ 
      next_mangled = mangled + 7;
      len = strspn ( next_mangled, m3_id_chars ); 
      sprintf ( demangled, "%.*s", len, next_mangled );
      return savestring (demangled, strlen(demangled));
    }

  return NULL ;
} /* m3_demangle */ 

/* print, one per line, the search names of at most max symbols of blk. */ 
static void
list_block_symbols ( struct block * blockp, int max ) 

{ int id_ct = 0; 
  struct symbol * sym; 
  struct dict_iterator iter; 

  sym = dict_iterator_first ( BLOCK_DICT ( blockp ), &iter ); 
  if ( sym == NULL ) { printf_filtered ("    <empty>\n" ); } 
  else 
    { while ( sym != NULL && id_ct < max ) 
        { printf_filtered ( "    \"%s\"\n", SYMBOL_SEARCH_NAME ( sym ) ); 
          id_ct ++;
          sym = dict_iterator_next ( &iter ); 
        } 
    if ( sym != NULL ) { printf_filtered ( "    ...\n" ); } 
    } 
} /* list_block_symbols */ 

static void 
dump_blockvector ( struct blockvector * block_vec, int max_syms_per_block )

{ int block_ss; 
  struct block * blk; 
  struct symbol * sym; 

  printf_filtered ( "Dump of blockvector 0x%08x:\n", (int)block_vec );
  for ( block_ss = 0; block_ss < BLOCKVECTOR_NBLOCKS ( block_vec ); block_ss ++) 
    { blk = BLOCKVECTOR_BLOCK ( block_vec, block_ss ); 
      sym = BLOCK_FUNCTION ( blk ); 
      printf_filtered 
         ( "%3d 0x%08x superblock=0x%08x", 
            block_ss, (int) blk, (int) BLOCK_SUPERBLOCK( blk) 
         );
      if ( sym == NULL ) 
        { printf_filtered ( ", anonymous, contains:\n" );
          list_block_symbols ( blk, max_syms_per_block ); 
        } 
      else
        { printf_filtered 
            ( ", procedure \"%s\", contains:\n", 
              SYMBOL_SEARCH_NAME ( sym ) 
            ); 
          list_block_symbols ( blk, max_syms_per_block ); 
        }
    } 
  printf_filtered ( "End of blockvector dump\n" );
} /* dump_blockvector */   

/* Parse a demangled procedure name, to see if it is for a nested
   procedure and return some places needed to find its parent block. 
   If the string at start has the form 
   <id>__<int>__<int>__ ... __<int>__<id> ... , for zero or more <int>s,
   return true, set *first_end to point to the first "__", and 
   set *next_start to point to the first char of the second <id>. 
   Otherwise, leave first_end and next_start alone and return false. */

/* FIXME:  Names of this form can easily be spoofed by the Modula-3 programmer
   by declaring things with "__" in their names.  Avoiding this without
   another character that is acceptable in linker names would be hard.  Any
   fix would require coordinated fixes in the compilers. */ 
static bool 
find_m3_nested_component 
  ( char * start, char ** first_end, char ** next_start ) 

  { char * l_first_end;
    char * l_next_start; 
    char ch; 
#if 0
    if ( next_start != NULL ) { * next_start = NULL; } 
    if ( first_end != NULL ) { * first_end = NULL; } 
#endif 
    ch = * start; 
    if ( ( 'A' <= ch && ch <= 'Z' ) || ( 'a' <= ch && ch <= 'z' ) ) 
      { start ++; 
        while ( true ) 
          { l_first_end  = start + strspn ( start, m3_id_chars_no_underscore ); 
            if ( * l_first_end != '_' ) { return false; } 
            else if ( * ( l_first_end + 1 ) == '_' ) 
              /* We have "__", beginning at l_first_end. */ 
              { l_next_start = l_first_end + 2; 
                /* FIXME: Assuming less about what strings the compilers 
                   produce would reduce the likelyhood of spoofing. */ 
                l_next_start 
                  = l_next_start + strcspn ( l_next_start , m3_id_letters );
                if ( * l_next_start == '\0' ) { return false; } 
                else 
                  { if ( next_start != NULL ) { * next_start = l_next_start; } 
                    if ( first_end != NULL ) { * first_end = l_first_end; } 
                    return true; 
                  } 
              } 
            else 
              { start = l_first_end + 1; /* And loop. */ } 
          } 
      } 
    else { return false; } 
  } /* find_m3_nested_component */ 

/* Used to keep track of a heap-allocated object. */ 
struct space_info 
  { char * space_ptr;
    int space_len; 
  };  

/* Ensure a string space is long enough. */ 
static void 
ensure_space 
  ( struct space_info * space_ref, int min_size ) 

  { if ( space_ref -> space_ptr == NULL ) 
      { space_ref -> space_len = min_size * 2;
        space_ref -> space_ptr = malloc ( space_ref -> space_len ); 
      } 
    else if ( space_ref -> space_len < min_size ) 
      { free ( space_ref -> space_ptr ); 
        space_ref -> space_len = min_size * 2;
        space_ref -> space_ptr = malloc ( space_ref -> space_len ); 
      } 
  } /* ensure_space */ 

/* Search block_vec for a procedure with mangled name proc_name. Return 
   its blockvector subscript, or < 0 if not found. 
*/ 
static int 
find_m3_proc_in_blockvector ( 
  struct blockvector * block_vec, char * proc_name ) 

{ int block_ss; 
  struct block * blk; 
  struct symbol * sym; 
  char * linkage_name; 

  /* Do this by brute-force search through the blocks.  Note that
     there is no ordering constraint on the blocks for separate
     procedures, even when nested.  So the block for the procedure we
     want could be anywhere within the blockvector.  In fact, PM3 and
     CM3 order them oppositely in the debug info.  Faster techniques
     and using dictionaries proved troublesome and not worth it. You
     can't put a symbol in >1 hashed dictionary, because the symbols
     are linked using a field located right in the symbol node. */ 
  block_ss = 0; 
  while ( true ) 
    { if ( block_ss >= BLOCKVECTOR_NBLOCKS ( block_vec ) ) 
        { return - 1; } 
      else 
        { blk = BLOCKVECTOR_BLOCK ( block_vec, block_ss );
          sym = BLOCK_FUNCTION ( blk ); 
          if ( sym != NULL ) 
            { linkage_name = SYMBOL_LINKAGE_NAME ( sym ); 
              if ( strcmp ( linkage_name, proc_name ) == 0 )   
                { return block_ss; } 
              if ( cm3_comp_unit_body_name_len ( linkage_name ) > 0 
                   && strcmp ( SYMBOL_DEMANGLED_NAME ( sym ) , proc_name ) == 0 
                 ) 
                { return block_ss; } 
              if ( pm3_comp_unit_body_name_start ( linkage_name ) != NULL   
                   && strcmp ( SYMBOL_DEMANGLED_NAME ( sym ) , proc_name ) == 0 
                 ) 
                { return block_ss; } 
            } 
          block_ss ++;  
        } /* else */ 
    } /* while */ 
} /* find_m3_proc_in_blockvector */ 

/* We have just read a symtab; fix it for Modula-3 purposes.
   We want to clean variables: Null out the type indicated in the symbol table,
      and remember the uid in the place where the type resolver will find it.
   We also want to find the connection between an interface record
      and its type description (the uid of interface records is -1; 
      this is about the only place where we have the scope information
      that is necessary to make the connection. 
   We also need to set the BLOCK_SUPERBLOCK pointers of the blocks of
      nested procedures to the rght block. 
*/
void
m3_fix_symtab ( struct symtab *st )
{ struct blockvector * block_vec; 
  int block_ss;
  struct block *bl;
  struct symbol *sym;
  struct symbol *ir = 0;
  struct type *ir_type = 0;
  char *ir_name = NULL;
  char *ir_kind = NULL;
  struct space_info prefix_space; 
  struct dict_iterator iter;

  prefix_space . space_len = 0; 
  prefix_space . space_ptr = NULL; 

  block_vec = BLOCKVECTOR ( st ); 

  if ( st -> language != language_m3 ) { return; } 
 
  if (info_verbose)
    { printf_filtered ("Fixing M3 symtab for file \"%s\"\n", st->filename);
      dump_blockvector ( block_vec, 50 );
      gdb_flush (gdb_stdout);
    }

  for (block_ss = 0; block_ss < BLOCKVECTOR_NBLOCKS (block_vec); block_ss ++) 
    { bl = BLOCKVECTOR_BLOCK (block_vec, block_ss);
      ALL_BLOCK_SYMBOLS (bl, iter, sym) 
        { char *name = SYMBOL_LINKAGE_NAME (sym);

          if (false && info_verbose)
            { printf_filtered 
                ("Fixing block %d M3 symbol \"%sym\"\n", block_ss , name );
              gdb_flush (gdb_stdout);
            }

          if (name [0] == 'M' && name [2] == '_') /* An interesting symbol. */
            { if (name [1] == 'I' ) 
                /* The one interface record, for an interface. */ 
                { ir = sym;
                  ir_name = name + 3;
                  ir_kind = "interface";
                } 
              else if (name [1] == 'M' ) 
                { int len = strlen ( name ); 

                if ( len >= 10 
                     && name [ len - 6 ] == '_' 
                     && name [ len - 5 ] == 'C' 
                     && name [ len - 4 ] == 'R' 
                     && name [ len - 3 ] == 'A' 
                     && name [ len - 2 ] == 'S' 
                     && name [ len - 1 ] == 'H'
                   ) /* This is not the module interface record. */ 
                  { }
                else  /* The one "interface" record, for a module. */
                  { ir = sym;
                    ir_name = name + 3;
                    ir_kind = "module";
                  } 
                } 
              else if (name [1] == '3' && SYMBOL_DOMAIN (sym) == VAR_DOMAIN) 
                /* A variable. */ 
                { LHS_SYMBOL_TYPE (sym) = 0; 
                  strncpy (sym->m3_uid, name + 3, M3UID_LEN);
                  sym->m3_uid [M3UID_LEN] = 0;
                } 
              else if (strncmp (name + 1, "R_zzzzzz", 8) == 0) 
                /* A pseudo-record with the globals of the module/interface. */ 
                { ir_type = SYMBOL_TYPE (sym); }
            } /* if */ 
        } /* ALL_BLOCK_SYMBOLS */ 

      /* Patch BLOCK_SUPERBLOCK if it's a nested procedure. */ 
      sym = BLOCK_FUNCTION ( bl ); 
      if ( sym && SYMBOL_LANGUAGE ( sym ) == language_m3 ) 
           /* It's a Modula-3 procedure block. */  
        { char * full_name; 
          char * prev_start; 
          char * prev_end; 
          char * next_start; 
          int ident_ct; 
          int prefix_len; 
          int parent_block_ss;
          int child_ss; 
          struct block *parent_block = NULL;
          struct symbol *parent_sym;
          int block_num;
          int block_num_2; 
          int block_level = 0; 
          struct block * child_block; 
          char ch; 

          full_name = SYMBOL_LINKAGE_NAME ( sym ); 
          next_start = full_name; 
          ident_ct = 0; 

          do { prev_start = next_start; 
               ident_ct ++; 
             } 
          while 
            ( find_m3_nested_component ( prev_start , &prev_end, &next_start ) );
          /* Here, next_start is the start of the last identifier in the 
             qualified name.  */
          if ( ident_ct < 2 /* Not a procedure name */
               || ( ident_ct == 2 && prev_end + 2 == next_start ) 
                  /* Two idents with no block in between.  This is a top-level
                     procedure.  Its superblock will already be set to the static
                     block. */  
             ) 
            { /* This block does not need its superblock field patched. */ }
          else  /* This procedure is inside another programmer-declared 
                   procedure. [full_name, prev_end) is the qualified name of the 
                   containing procedure.  There could be anonymous blocks in 
                   between. */ 
            { prefix_len = prev_end - full_name + 1; 
              ensure_space ( &prefix_space, prefix_len );
              memcpy ( prefix_space . space_ptr, full_name, prefix_len - 1 ); 
              prefix_space . space_ptr [ prefix_len - 1 ] = '\0'; 
              parent_block_ss 
                = find_m3_proc_in_blockvector 
                    ( block_vec, prefix_space . space_ptr ); 
              if ( parent_block_ss < 0 ) /* Not found. */ 
                { printf_filtered  
                    ( "Can't find parent procedure \"%s\" "
                      "of nested procedure \"%s\"\n",
                      prefix_space . space_ptr, full_name 
                    ); 
                  goto done_with_nested; 
                } 

              prev_end += 2; /* Skip "__" before the next name component. */ 
              parent_block = BLOCKVECTOR_BLOCK (block_vec, parent_block_ss);
              parent_sym = BLOCK_FUNCTION ( parent_block ); 
              if ( ident_ct == 2 
                   && cm3_comp_unit_body_name_len 
                        ( SYMBOL_LINKAGE_NAME ( parent_sym ) )
                      > 0  
                 ) 
                /* The parent procedure is a CM3-compiled compilation
                   unit body procedure, possibly with some anonymous
                   blocks between.  This will only happen if the comp
                   unit is a module.  Peculiarly and inconsistently,
                   CM3 flattens a block located inside the executable
                   body of a module into its <moduleName>_M3 procedure
                   in the actual stabs block construction, yet still
                   adds a block number 1 to the name of procedures
                   located inside it.  We must consume this "1__" to
                   get the code below to find the right block. */ 
                { while ( '0' <= *prev_end && *prev_end <= '9') { prev_end ++; } 
                  prev_end += 2; /* Skip "__" before the next name component. */ 
                }  
        
              /* Handle any anonymous blocks between parent and nested. */ 
              while ( prev_end < next_start ) /* There is an anonymous  block. */
                { block_num = 0; 
                  while ( true )  
                    { ch = * prev_end; 
                      if ( '0' <= ch && ch <= '9' ) 
                        { block_num = block_num * 10 + ch - '0';
                          prev_end ++; /* and loop */ 
                        } 
                      else { break ; } 
                    } /* while */
                  /* Now block_num is a block sequence number of an M3 anonymous
                     block that is immediately inside the one identified by
                     blockvector index parent_block_ss.  The gdb block we want is
                     the block_num-th such block.  For anonymous blocks, 
                     the children always come after the parent in the 
                     blockvector, so search forwards from parent_block_ss.
                   */   
                  child_ss = parent_block_ss + 1; 
                  block_num_2 = block_num; 
                  while ( true ) 
                    { if ( child_ss >= BLOCKVECTOR_NBLOCKS (block_vec) )
                        { printf_filtered  
                            ( "Can't find child block %d, at level %d, "
                              "of parent procedure \"%s\", "
                              "for nested procedure \"%s\"\n",
                              block_num, block_level, prefix_space . space_ptr, 
                              full_name  
                              ); 
                          goto done_with_nested; 
                        }   
                      child_block = BLOCKVECTOR_BLOCK (block_vec, child_ss ); 
                      if ( BLOCK_FUNCTION ( child_block ) == NULL 
                            && BLOCK_SUPERBLOCK ( child_block )
                               == BLOCKVECTOR_BLOCK (block_vec, parent_block_ss )
                          ) 
                        /* child_block is for an anonymous, immediate child of 
                           block at parent_block_ss. */  
                        { block_num_2 --; 
                          if ( block_num_2 == 0 ) 
                            { parent_block_ss = child_ss; 
                              break; 
                            } 
                        } 
                      child_ss ++; 
                    } /* while */ 
                  prev_end += 2; /* Skip "__" after the block number. */ 
                  block_level ++; 
                } /* while */ 

              /* Here, parent_block_ss is the immediate parent block of nested 
                 procedure block block_ss. */ 

              parent_block = BLOCKVECTOR_BLOCK (block_vec, parent_block_ss ); 
              if (info_verbose)
                { parent_sym = BLOCK_FUNCTION ( parent_block ); 
                if ( parent_sym != NULL ) 
                  { printf_filtered 
                      ( "Setting superblock of nested procedure \"%s\", bv %d, "
                        " to \"%s\", bv %d.\n",
                        full_name, block_ss, SYMBOL_SEARCH_NAME ( parent_sym ),
                        parent_block_ss 
                      );
                    list_block_symbols ( parent_block, 1); 
                  } 
                else 
                  { printf_filtered 
                      ( "Setting superblock of nested procedure \"%s\", bv %d,"
                        "to anonymous block, bv %d\n",
                        full_name, block_ss, parent_block_ss
                      );
                    list_block_symbols ( parent_block, 1 ); 
                  } 
                  gdb_flush (gdb_stdout);
                }
              BLOCK_SUPERBLOCK ( BLOCKVECTOR_BLOCK (block_vec, block_ss ) )
                = parent_block; 
              done_with_nested: { }  
            } /* else nested procedure. */ 
        } /* if this is a procedure block. */ 
    } /* for blocks in blockvector. */ 

  if ( prefix_space . space_ptr != NULL ) { free ( prefix_space . space_ptr ); } 

  if (ir) {
    if (ir_type == 0) { 
      /* This is happening when stabs info was not produced at all, and 
         objdump -G gives almost nothing, yet we get here with a lot of
         symbols from dwarf2read.  They do not look like the right complement of
         symbols.  There are MI_ symbols for imported interfaces as well as the
         one MM_ or MI_ for the unit itself.  But there is no MR_zzzzzz symbol.
      */ 
      error ("Debug info for file \"%s\" not in stabs format", st->filename);
      LHS_SYMBOL_TYPE (ir) = 0;
    } else {
      LHS_SYMBOL_TYPE (ir) = ir_type;
    }
  }

  if (info_verbose)
    { printf_filtered ("Done with M3 symtab for file \"%s\"\n", st->filename);
      gdb_flush (gdb_stdout);
    }

} /* m3_fix_symtab */ 

void
m3_fix_target_type (t)
     struct type *t;
{
  if (TYPE_TARGET_TYPE (t)) return;

  switch (TYPE_CODE (t))
    {
    case TYPE_CODE_M3_ARRAY:
      TYPE_TARGET_TYPE (t) = TYPE_M3_ARRAY_ELEM (t);
      break;
    case TYPE_CODE_M3_OPEN_ARRAY:
      TYPE_TARGET_TYPE (t) = TYPE_M3_OPEN_ARRAY_ELEM (t);
      break;
    case TYPE_CODE_M3_SUBRANGE:
      TYPE_TARGET_TYPE (t) = TYPE_M3_SUBRANGE_TARGET (t);
      break;
    case TYPE_CODE_M3_POINTER:
      TYPE_TARGET_TYPE (t) = TYPE_M3_POINTER_TARGET (t);
      break;
    case TYPE_CODE_M3_INDIRECT:
      TYPE_TARGET_TYPE (t) = TYPE_M3_INDIRECT_TARGET (t);
      break;
    case TYPE_CODE_M3_PROC:
    case TYPE_CODE_M3_METHOD:
      TYPE_TARGET_TYPE (t) = TYPE_M3_PROC_RESTYPE (t);
      break;
    default:
      break;
    }
}

struct type *
m3_resolve_type (uid)
     char *uid;
{
  int uid_val;
  struct symbol *sym = lookup_symbol (uid, 0, STRUCT_DOMAIN, 0, 0);

  if (sym) {
    struct type *t = SYMBOL_TYPE (sym);
    if (TYPE_CODE (t) == TYPE_CODE_M3_OPAQUE) {
      t = m3_resolve_type (TYPE_FIELD_M3_UID (t, 0)); };
    m3_fix_target_type (t);
    return t;
  };

  if (m3uid_to_int (uid, &uid_val)) {
    if      (uid_val == 0x195c2a74) { return builtin_type_m3_integer; }
    else if (uid_val == 0x50f86574) { return builtin_type_m3_text; }
    else if (uid_val == 0x97e237e2) { return builtin_type_m3_cardinal; }
    else if (uid_val == 0x1e59237d) { return builtin_type_m3_boolean; }
    else if (uid_val == 0x08402063) { return builtin_type_m3_address; }
    else if (uid_val == 0x9d8fb489) { return builtin_type_m3_root; }
    else if (uid_val == 0xa973d3a6) { return builtin_type_m3_transient_root; }
    else if (uid_val == 0x56e16863) { return builtin_type_m3_char; }
    /* For widechar, the uid_val was once 0xb0830411.  Presumably, this is an
       outdated leftover from a transitional implementation of widechar, at
       Critical Mass, before the final one came from them. */
    else if (uid_val == 0x88f439fc) { return builtin_type_m3_widechar; }
    else if (uid_val == 0x48e16572) { return builtin_type_m3_real; }
    else if (uid_val == 0x94fe32f6) { return builtin_type_m3_longreal; }
    else if (uid_val == 0x9ee024e3) { return builtin_type_m3_extended; }
    else if (uid_val == 0x48ec756e) { return builtin_type_m3_null; }
    else if (uid_val == 0x1c1c45e6) { return builtin_type_m3_refany; }
    else if (uid_val == 0x51e4b739) { return builtin_type_m3_transient_refany; }
    else if (uid_val == 0x898ea789) { return builtin_type_m3_untraced_root; }
    else if (uid_val == 0x00000000) { return builtin_type_m3_void; }
  }

  error ("Cannot resolve type with uid %s", uid);
}

struct type *
find_m3_type_named (name, must_find)
     char *name;
     int must_find; /* Emit an error message, if can't find it. */ 
{
  char struct_name [MAX_SYMBOLLEN];
  struct symbol *s;

  sprintf (struct_name, "B$%s", name);
  s = lookup_symbol (struct_name, 0, STRUCT_DOMAIN, 0, 0);
  if (s == NULL) {
    if (must_find) error ("unable to find type named \"%s\"\n", name);
    return NULL;
  };
  return TYPE_M3_NAME_TYPE (SYMBOL_TYPE (s));
}

/* return the pseudo-record-type that has one field for each exported
   interface.  It's demangled name is "H$<moduleName>.  
   If the result is NIL, this means that the module exports itself only. 
*/
struct type *
find_m3_exported_interfaces (name)
     char *name;
{
  char struct_name [MAX_SYMBOLLEN];
  struct symbol *s;

  sprintf (struct_name, "H$%s", name);
  if ((s = lookup_symbol (struct_name, 0, STRUCT_DOMAIN, 0, 0))) {
    return (SYMBOL_TYPE (s));
  } else {
    return 0;
  }
}

/* Return the pseudo-record-type that has one field for each 
   globally-declared identifier in an interface or module.  
   For an interface, its demangled name is "I$<interfaceName>".
   For a module, its demangled name is "M$<moduleName>".
   kind should be either 'I' or 'M', for interface or module. 
*/ 
struct symbol *
find_m3_ir (kind, name)
     int kind; 
     char *name;
{
  char struct_name [MAX_SYMBOLLEN];
  sprintf (struct_name, "%c$%s", kind, name);
  return lookup_symbol (struct_name, 0, VAR_DOMAIN, 0, 0);
}

char *
find_m3_type_name (t)
     struct type *t;
{
  char *uid = TYPE_TAG_NAME (t);
  char struct_name [MAX_SYMBOLLEN];
  struct symbol *sym;

  if (TYPE_NAME (t) == 0) {
    if (uid == NULL) return NULL;
    sprintf (struct_name, "G$%s", uid);
    if ((sym = lookup_symbol (struct_name, 0, STRUCT_DOMAIN, 0, 0))) {
      TYPE_NAME (t) = TYPE_FIELD_NAME (SYMBOL_TYPE (sym), 0);
    } else {
      char *n;
      if (uid == NULL) {
	  n = malloc (strlen("<typeid=(null)>")+1);
	  strcpy(n, "<typeid=(null)>");
      } else {
	  n = malloc (strlen(uid)+strlen("<typeid=>")+1);
	  sprintf (n, "<typeid=%s>", uid);
      }
      TYPE_NAME (t) = n;
    }
  }
  return TYPE_NAME (t);
}

static int is_cm3; /* The cm3 distribution of Modula-3 has some differences
                      in its RTS. */ 
static int rt0_tc_selfID_size = 0,        rt0_tc_selfID_offset = 0;
static int rt0_tc_dataSize_size = 0,      rt0_tc_dataSize_offset = 0;
static int rt0_tc_kind_size = 0,          rt0_tc_kind_offset = 0;

static int rt0_dataOffset_size = 0,       rt0_dataOffset_offset = 0;
static int rt0_methodOffset_size = 0,     rt0_methodOffset_offset = 0;
static int rt0_parent_size = 0,           rt0_parent_offset = 0;
static int rt0_defaultMethods_size = 0,   rt0_defaultMethods_offset = 0;

static CORE_ADDR rt0u_types_value; /* Pm3 and other older Modula-3's only. */
static CORE_ADDR rttype_types_addr; /* Cm3 only. */

static int rttype_info_def_size = 0,      rttype_info_def_offset = 0;
static int rttype_infomap_map_size = 0,   rttype_infomap_map_offset = 0;
static int rttype_infomap_cnt_size = 0,   rttype_infomap_cnt_offset = 0;

static int constant_init_done;

void
init_m3_constants ()
{
  if (constant_init_done) { return; }

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
    find_m3_rec_field (rt0_tc, "selfID",
		     &rt0_tc_selfID_size, &rt0_tc_selfID_offset, 0);
    find_m3_rec_field (rt0_tc, "dataSize",
		     &rt0_tc_dataSize_size, &rt0_tc_dataSize_offset, 0);

    rt0_otc = find_m3_type_named ("RT0.ObjectTypecell",0);
    is_cm3 = (rt0_otc != 0); 

    if (is_cm3)
      { struct type* t;
        struct symbol *rttype;
        int types_size, types_offset;

        find_m3_rec_field 
          (rt0_tc, "kind", &rt0_tc_kind_size, &rt0_tc_kind_offset, 0);

        t = find_m3_type_named ("RTType.InfoMap",1);
        find_m3_rec_field (t, "map", 
          &rttype_infomap_map_size, &rttype_infomap_map_offset, 0);
        find_m3_rec_field (t, "cnt", 
          &rttype_infomap_cnt_size, &rttype_infomap_cnt_offset, 0);

        t = find_m3_type_named ("RTType.Info",1);

        find_m3_rec_field (t, "def", 
           &rttype_info_def_size, &rttype_info_def_offset, 0);

        rttype = find_m3_ir ('M', "RTType");

        find_m3_rec_field (SYMBOL_TYPE (rttype), "types", 
                           &types_size, &types_offset, 0);

        rttype_types_addr = SYMBOL_VALUE_ADDRESS (rttype) + types_offset / 8;
      } 
    else /* SRC, PM3, EZM3. */ 
      { 
        struct symbol *rt0u;
        int rt0u_types_size, rt0u_types_offset;

        rt0u = find_m3_ir ('I', "RT0u");

        find_m3_rec_field (SYMBOL_TYPE (rt0u), "types", 
                           &rt0u_types_size, &rt0u_types_offset, 0);

        read_memory (SYMBOL_VALUE_ADDRESS (rt0u) 
                              + rt0u_types_offset / TARGET_CHAR_BIT
                           , (char *)&rt0u_types_value
                           , rt0u_types_size /TARGET_CHAR_BIT);
        rt0_otc = rt0_tc; 
      } 

    find_m3_rec_field (rt0_otc, "dataOffset", 
		     &rt0_dataOffset_size, &rt0_dataOffset_offset, 0);
    find_m3_rec_field (rt0_otc, "methodOffset", 
		     &rt0_methodOffset_size, &rt0_methodOffset_offset, 0);
    find_m3_rec_field (rt0_otc, "parent",
		     &rt0_parent_size, &rt0_parent_offset, 0);
    find_m3_rec_field (rt0_otc, "defaultMethods", 
		     &rt0_defaultMethods_size, 
		     &rt0_defaultMethods_offset, 0);
  }

  constant_init_done = 1;
}

/* True if debugging CM3-compiled Modula-3 code. */ 
int
m3_is_cm3 ( void ) 
  { init_m3_constants (); 
    return is_cm3; 
  } 

/* Return the typecode of the object at inferior address addr. */ 
LONGEST 
m3_typecode ( CORE_ADDR addr ) 

{
  LONGEST typecodeword, typecode;
  if (!addr) { return 0; }

  read_memory (addr - (TARGET_PTR_BIT / TARGET_CHAR_BIT), 
		      (char *)&typecodeword, 
		      TARGET_PTR_BIT / TARGET_CHAR_BIT);

  /* the typecode is in Modula-3 bits 1..21 */
  typecode = m3_extract_ord((char *)&typecodeword, 1, 20, 0);
  return typecode; 

} /* m3_typecode */ 

/* Return the inferior address of the typecell for the dyanamic (allocated) type
   of the object at inferior address addr.  
*/
CORE_ADDR 
find_m3_heap_tc_addr (CORE_ADDR addr)

{
  LONGEST typecode, n_types;
  CORE_ADDR result, map_ptr;

  if (!addr) { return 0; }

  typecode = m3_typecode ( addr ) ; 

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
}

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
find_tc_from_m3_type (t)
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
}

int 
find_m3_uid_from_tc (tc_addr)
     CORE_ADDR tc_addr;
{
  int selfID;

  init_m3_constants ();

  read_memory (tc_addr + rt0_tc_selfID_offset / TARGET_CHAR_BIT,
		      (char *)&selfID, rt0_tc_selfID_size / HOST_CHAR_BIT);

  return selfID;
} /* find_m3_uid_from_tc */ 

struct type *
find_m3_type_from_tc (tc_addr)
     CORE_ADDR tc_addr;
{
  int selfID;

  init_m3_constants ();

  read_memory (tc_addr + rt0_tc_selfID_offset / TARGET_CHAR_BIT,
		      (char *)&selfID, rt0_tc_selfID_size / HOST_CHAR_BIT);

  return (m3_resolve_type (m3uid_from_int (selfID)));
}

struct type *
find_m3_heap_type (addr)
     CORE_ADDR addr;
{
  return find_m3_type_from_tc (find_m3_heap_tc_addr (addr));
}


/* return LOOPHOLE (tc_addr, RT0.ObjectTypeDefn).dataOffset */
int 
m3_tc_address_to_dataOffset (tc_addr)
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
}

int 
m3_tc_address_to_methodOffset (tc_addr)
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
}
		      
int 
m3_tc_address_to_dataSize (tc_addr)
     CORE_ADDR tc_addr;
{
  int result;
  init_m3_constants ();
  read_memory (tc_addr + rt0_tc_dataSize_offset / TARGET_CHAR_BIT,
		      (char *)&result, rt0_tc_dataSize_size / TARGET_CHAR_BIT);
  return result;
}
		      
CORE_ADDR  
m3_tc_addr_to_super_tc_addr (tc_addr)
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
}
		      
CORE_ADDR 
m3_tc_address_to_defaultMethods ( CORE_ADDR tc_addr ) 
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
}

/*
 * find_m3_rec_field
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
find_m3_rec_field (
    struct type *rec_type,
    char *name,
    int *bitsize, 
    int *bitpos,
    struct type **type
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
  } /* find_m3_rec_field */ 
		      
int
find_m3_obj_field (
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
  } /* find_m3_obj_field */ 

int
find_m3_obj_method (
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
  } /* find_m3_obj_method */ 

int
is_m3_ordinal_type (type)
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
}

/* Name is a string that might be the demangled name of a nested procedure.
   If so, it will be of the form: 
     "<Module>.<Proc>.<InnerProc>.<DeeperProc>. ... .<DeepestProc>",
   where "..." is a meta-ellipsis. 
   Skip "<Module>.<Proc>" and as many occurences of ".<DeeperProc>" as
   will still leave a ".<DeepestProc>", returning the length of everything 
   skipped.  The prefix of name, with this length, will be the demangled 
   name of the containing procedure. 
   Return 0 if name is not of this form, with at least one inner procedure. 
*/
int nested_prefix_len ( char * name) 
  { char * current; 
    char * i; 
  
    current = strchr (name, '.'); 
    if (current == 0) return 0;
    current++; 
    current = strchr (current, '.'); 
    if (current == 0) return 0;
    while ( (i = strchr (current + 1 , '.') ) > current) { current = i; } 
    return current - name;
  } 

/* For each block in bv that is for a nested procedure, patch the 
   BLOCK_SUPERBLOCK link to point to the block for the containing
   procedure.  Probably, there is not enough stabs info to do this
   correctly when the nested proc is inside a non-proc block.  
 */ 
void
m3_patch_nested_procs 
  ( struct blockvector *bv ) 

  { struct dictionary *dict; 
    struct dict_iterator iter; 
    struct block *block_ptr;
    int block_no;
    struct symbol *sym; 
    struct symbol *parent_sym; 
    char * name; 
    int prefix_len; 
    char * prefix_copy; 

    return; 

#if 1
/*  -------------------DEBUG -----------------------*/
  if (false && info_verbose)
    { int i; 
      struct block * b;
      struct symbol * s; 
      struct dict_iterator iter;
      char *name; 

      printf_filtered ("\nPatching M3 blockvector\n");
      gdb_flush (gdb_stdout);

      for (i = 0; i < BLOCKVECTOR_NBLOCKS (bv); i ++) 
        { b = BLOCKVECTOR_BLOCK (bv, i);
          ALL_BLOCK_SYMBOLS (b, iter, s) 
            { name = SYMBOL_LINKAGE_NAME (s);
              printf_filtered ("block %d has M3 symbol \"%s\"\n", i , name);
              gdb_flush (gdb_stdout);
            } 
        } 

    }
/*  -------------------END DEBUG -----------------------*/
#endif 

    /* Fill a dictionary with symbols of procedures in the blockvector,
       because they might turn out later to contain a nested procedure. */ 
    /* WATCH OUT! Hashed dictionaries use a link pointer (hash_next) in
       the symbol struct itself, so a symbol can not be in two dictionaries
       at once.  Some are already in hashed dictionaries of their symbol
       table.  It would be nice to be able to hash the symbols here, but
       it would require a different hash table implementation.
    */
    dict = dict_create_linear_expandable ( ); 
    for (block_no = FIRST_LOCAL_BLOCK; block_no < BLOCKVECTOR_NBLOCKS (bv); 
         block_no++
        )  
      { block_ptr = BLOCKVECTOR_BLOCK (bv, block_no); 
        if (block_ptr) 
          { sym = BLOCK_FUNCTION (block_ptr); 
            if (sym && SYMBOL_LANGUAGE (sym) == language_m3) 
              { dict_add_symbol (dict, sym); } 
          } 
      } 
    parent_sym = 0; 
    /* Now go through the blockvector again, looking for nested functions. */
    for (block_no = FIRST_LOCAL_BLOCK; block_no < BLOCKVECTOR_NBLOCKS (bv); 
         block_no++
        )  
      { block_ptr = BLOCKVECTOR_BLOCK (bv, block_no); 
        if (block_ptr) 
        { sym = BLOCK_FUNCTION (block_ptr);
          if (sym && SYMBOL_LANGUAGE (sym) == language_m3)  
            { name = SYMBOL_SEARCH_NAME (sym); 
              prefix_len = nested_prefix_len (name);  
              if (prefix_len > 0) 
                { prefix_copy = alloca (prefix_len + 1); 
                  memcpy (prefix_copy, name, prefix_len);
                  prefix_copy[prefix_len] = '\0';
                  parent_sym 
                    = dict_iter_name_first (dict, prefix_copy, &iter);  
                  if (parent_sym) 
                    { if (info_verbose) 
                        { printf_filtered 
                            ( "Patching parent \"%s\" of \"%s\"\n", 
                              prefix_copy, name 
                            ); 
                        } 
                      BLOCK_SUPERBLOCK (block_ptr) 
                        = SYMBOL_BLOCK_VALUE (parent_sym);  
                    } 
                  else if (info_verbose) 
                    { printf_filtered 
                        ( "Parent \"%s\" of \"%s\" not found\n", 
                          prefix_copy, name 
                        ); 
                    } 
                } 
            } 
        } 
      } 
    dict_free (dict); 
  } 

/* End of m3-lang.c */ 

