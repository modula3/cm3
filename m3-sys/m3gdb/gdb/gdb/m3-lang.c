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

/* This file contains globals and initialization for Modula-3 support. */ 

#include "defs.h"
#include "gdb_assert.h"
#include "symtab.h"
#include "expression.h"
#include "language.h"
  /* #include "value.h" */
#include "gdbcore.h" 
#include "gdb_string.h"
#include "command.h" 
#include "block.h"
#include "dictionary.h" 

#include "m3-lang.h"
#include "m3-util.h"
#include "m3-eval.h"
#include "m3-exp.h"
#include "m3-uid.h"
#include "m3-threads.h"
#include "m3-typeprint.h"
#include "m3-valprint.h"

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

extern void _initialize_m3_language ( );

/* Nonzero if a Modula-3 compiler that does not use a gcc code generator.  
   NOTE: Some Modula-3 compilers use a gcc-derived code generator.  They 
   do not cause this to be nonzero, but they do cause 
   processing_gcc_compilation to be nonzero. */ 
int processing_pm3_compilation = 0; 

/* Nonzero indicates that the debug info will show an extra block 
   surrounding the non-prologue-non-epilogue part of every procedure. 
   This happens when code was produced by a code generator derived from
   later gcc versions (3.4.5, for example).  
*/   
int procedures_have_extra_block = 0; 

static const char * SRC_compiler_string = "SRC-Modula3_compiled."; 

/* Keep this string consistent with that of the same name in gcc, 
   */ 
static const char * procedures_have_extra_block_string 
  = "procedures_have_extra_block."; 

/* Use the string from the N_OPT stabs entry to maybe set 
   processing_pm3_compilation and procedures_have_extra_block. */
void
m3_check_compiler ( char * name ) 

  { if ( strcmp ( name, SRC_compiler_string ) == 0 ) 
      { processing_pm3_compilation = 1; } 
    if ( strcmp ( name, procedures_have_extra_block_string ) == 0 ) 
      { procedures_have_extra_block = 1; } 
  } /* m3_check_compiler */ 
 
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

    case STRUCTOP_M3_ENUM:
      strlen = longest_to_int (expr->elts[endpos - 2].longconst);
      *oplenp = 6 + BYTES_TO_EXP_ELEM (strlen + 1);
      *argsp = 0;
      break;

    default:
      operator_length_standard (expr, endpos, oplenp, argsp); 
      return; 
    }
} /* m3_operator_length */ 

static char *
m3_operator_name (enum exp_opcode opcode)
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
      case STRUCTOP_M3_ENUM: return "STRUCTOP_M3_ENUM";
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
} /* m3_operator_name */ 

static void
m3_error (char *msg)
{ if (msg) error (msg); 
  else error ( "Invalid syntax in expression." );
} /* m3_error */ 

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

static const struct exp_descriptor m3_exp_descriptor = {
  m3_print_subexp,
  m3_operator_length, 
  m3_operator_name,
  m3_dump_subexp,
  m3_evaluate_subexp   
};

const struct language_defn m3_language_defn = {
  "Modula-3",			 /* la_name: Language name */
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
  m3_print_char,       		 /* la_printchar: Print a character constant */
  m3_print_string,		 /* la_printstr: Print a string constant */
  m3_emit_char,			 /* la_emitchar: Print a character constant */
  m3_create_fundamental_type,	 /* la_fund_type: Create fundamental type */
  m3_print_type,		 /* la_print_type: */
  m3_val_print,			 /* la_val_print */
  m3_value_print,		 /* la_value_print: a top-level value */
  NULL,                          /* la_skip_trampoline: */ 
  NULL,                          /* la_value_of_this: */ 
  m3_lookup_symbol_nonlocal,     /* la_lookup_symbol_nonlocal: */ 
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
m3_fix_param (
    struct type * func_type, int fieldno, struct symbol * param_sym )

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

static const char * dot_only = "."; 

#if 0 /* Currently not called. Suppress warning. */ 
/* If name has dot-separated components, return the pointer to the
   last such component.  Otherwise, identity. */ 
static const char * 
simple_name ( const char * name ) 

{ const char * start; 
  size_t remaining_len; 
  size_t prefix_len; 

  start = name; 
  remaining_len = strlen ( start ); 
  while ( true ) 
    { prefix_len = strcspn ( start, dot_only ); 
      if ( prefix_len >= remaining_len ) { return start; }   
      start += prefix_len + 1; 
      remaining_len -= prefix_len + 1; 
    } 
} /* simple_name */ 
#endif 

void 
m3_decode_struct ( struct type *t ) 
{
  int i;
  long size, tmp1, tmp2, tmp3;
  char *key, *type_specific_info;
  
  /* The format is M<kind>_<uid>_<bitsize>_<other info>
      where kind is a one letter code, 
            uid  is a 6 byte base-62 number
	    bitsize is the type's size in bits
            other info depends on the type; */

  key = TYPE_TAG_NAME (t);
  if (key == 0 || strlen (key) < 4 || key[0] != 'M' || key[2] != '_') return;

  type_specific_info = skip_underscores (key, 3);

  switch (key [1])
    {
    case 'N': 
      TYPE_CODE (t) = TYPE_CODE_M3_TYPE_NAME;
      strncpy ( TYPE_FIELD_M3_UID ( t, 0 ), key + 3, M3UID_LEN );
      break; 

    case 'n': 
      TYPE_CODE (t) = TYPE_CODE_M3_TYPE;
      set_field_uid (t, 0);
      TYPE_FIELD_NAME ( t, 0 ) = key + 4 + M3UID_LEN;
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
      for ( i = 0; i < TYPE_NFIELDS ( t ); i ++ ) 
        { TYPE_FIELD_TYPE ( t, i ) = t; } 
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
      {  TYPE_LENGTH ( t ) 
           = ( TYPE_M3_SIZE ( t ) + TARGET_CHAR_BIT - 2 ) / TARGET_CHAR_BIT + 1; 
      } 
    else 
      { TYPE_LENGTH ( t ) 
          = ( TYPE_M3_SIZE ( t ) + TARGET_CHAR_BIT - 1 ) / TARGET_CHAR_BIT; 
      } 
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

/* See whether the string name is a PM3-style compilation unit body procedure
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

/* Between the various compilers and different kinds of things to look up,
   there is a real hodge-podge of places to find them in the debug info
   we get.  Here is a table, laboriously constructed from many experiments:

   Type T:           Always in the static block of the unit declared in.  
                     One entry has linkage name Mn_zzzzzz_U.T and maps to
                     the uid.  Another has linkage name MN_<uid> and maps 
                     to U.T.         

   Variable V:       A field of the globals record of the unit declared in.
                     Has no gdb symbol.  

   Globals record:   Has a variable-like entry in the global block for
                     PM3 and friends, or the static block for CM3, of
                     the unit declared in.  It has linkage name 
                     MI_<interfaceName> or MM_<moduleName>.  Its type
                     is patched by m3_fix_symtab to point to the globals
                     record type.  

   Globals record    Has a type entry with linkage name MR_zzzzzz_<bitSize>, 
   type:             always in the static block.  This defines the type of the 
                     globals record, with a field for each global variable.
                     For PM3 only, it also has a field for each procedure 
                     declared in the unit.  CM3 has two of these.  The first
                     has no fields in the cases examined so far.  The second
                     is the one we want.  Probably should merge the fields of
                     all such record types. 

   Procedure in      PM3 only has a field in the globals record of the 
   an interface:     interface. CM3 has nothing in the interface symtab. 

   Procedure in a    In the static block of the module, for all compilers.  
   module, not in    Linkage name is <moduleName>__<procName>.  PM3 also 
   any exported      has a field in the globals record of the module. 
   interface:

   Procedure in a    For CM3, only in the global block of the module. 
   module, also in   For PM3, in the local block of the module.  
   an exported       Linkage name is <InterfaceName>__<procName>.  
   interface:        PM3 also has a field in the globals record of the module. 

*/ 

/* A Modula-3-specific callback. */ 
struct symbol *
m3_lookup_symbol_nonlocal (
    const char *name,
    const char *linkage_name,
    const struct block *block,
    const domain_enum domain,
    struct symtab * * symtab
  )

{ const struct block * static_block = block_static_block ( block ); 
  const struct block * global_block = block_global_block ( block ); 
  struct symbol * sym = NULL;
  struct symbol * unit_sym = NULL;
  int i; 
  char type_name [ M3_MAX_SYMBOLLEN ]; 
  char * unit_name = NULL;
  int found; 

  /* Look in the static block surrounding the execution context, where we 
     will find exported and nonexported  global procedures in PM3 etc. and 
     nonexported global procedures in CM3.  "exported" here means the 
     procedure body is actually provided in this module. */ 
  if ( static_block != NULL ) 
    { sym = lookup_symbol_static 
              ( name, linkage_name, static_block, domain, symtab );
      if ( sym != NULL ) { return sym; } 
    } 

  /* Look in the one global block surrounding the execution context, where 
     we will find exported global procedures in CM3. */ 
  if ( global_block != NULL ) 
    { sym = m3_lookup_symbol_one_global 
              ( name, linkage_name, global_block, domain, symtab );
      if ( sym != NULL ) { return sym; }  
    } 

  /* Look in the module's globals record, were we will find global variables 
     declared in the current module.  The globals record is found in the
     global block of the execution context for PM3 etc. and in the static 
     block for CM3. */ 
  if ( static_block != NULL ) 
    { unit_sym = m3_block_globals_symbol ( static_block, 'M', & unit_name ); 
      if ( unit_sym == NULL ) 
        { if ( global_block != NULL ) 
          { unit_sym 
              = m3_block_globals_symbol ( global_block, 'M' , & unit_name ); 
          } 
        } 
    }
  if ( unit_sym != NULL ) 
    { found = m3_find_rec_field 
                ( SYMBOL_TYPE ( unit_sym ), name, NULL, NULL, NULL );
      if ( found ) 
        { if ( symtab != NULL ) 
            { /* This result is probably never used, but we need it to 
                 satisfy the interface of the la_lookup_symbol_nonlocal 
                 callback.  */
              * symtab = m3_symtab_of_block ( global_block ); 
            }
          /* Here is the global variable case, where we just return the 
             symbol for the globals record and let the caller sort it out. */
          return unit_sym; 
        } 
    } 

  /* Look in the static block of the execution context, where we will find 
     a type declared in the module, with transformed name, the same for all 
     compilers. */ 
  if ( static_block != NULL && unit_name != NULL ) 
    { sym = m3_lookup_type ( unit_name, name, static_block, symtab ); 
      if ( sym != NULL ) { return sym; } 
    } 

  /* Look in all interfaces exported by the current module. */ 
  if ( unit_name != NULL ) 
    { sym = m3_lookup_exported ( unit_name, name, symtab ); } 

  return sym; 

} /* m3_lookup_symbol_nonlocal */ 

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
  char demangled [ M3_MAX_SYMBOLLEN ]; /* FIXME: Many uses of this variable are 
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
      /* m3 type uid for type name: Mn_zzzzzz_<moduleName.typeName>
         Name is not fully qualified.  
         Demangled: B$<name> */
      if (mangled_len >= 4 + M3UID_LEN && m3uid_to_int (mangled + 3, &uid)) {
        /* TODO: For purposes of looking up user-typed type names, it would be 
           more straightforward to strip off the interface qualifier from
           the type name, as by this statement: 
	sprintf (demangled, "B$%s", simple_name ( mangled + 4 + M3UID_LEN ) );
           And also eliminate the B$ from the demangled name, so a plain
           lookup could find either a type or a procedure.  But there are
           lots of places that use the qualified name to lookup  a hard-coded
           reference to a runtime type, that need the qualified name.  So
           leave this, for now. 
        */ 
	sprintf (demangled, "B$%s", mangled + 4 + M3UID_LEN );
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
  /* FIXME: Try to use pm3_comp_unit_body_name_start here. */ 
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
      /* sprintf (demangled, "%s.%c3.MAIN", 
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
        { printf_filtered 
            ( "    \"%s\",\"%s\"\n", 
              SYMBOL_SEARCH_NAME ( sym ), 
              SYMBOL_LINKAGE_NAME ( sym ) 
            ); 
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
            block_ss, ( int ) blk, ( int ) BLOCK_SUPERBLOCK( blk) 
         );
      printf_filtered 
         ( ", bodyblock=0x%08x", 
            ( int ) M3_BLOCK_BODY_BLOCK( blk) 
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

/* Parse a mangled procedure name, to see if it is for a nested
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
find_m3_nested_component (
    char * start, char ** first_end, char ** next_start ) 

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
                   produce would reduce the likelihood of spoofing. */ 
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
ensure_space ( struct space_info * space_ref, int min_size ) 

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
      This is about the only place where we have the scope information
      that is necessary to make the connection. 
   We also need to set the BLOCK_SUPERBLOCK pointers of the blocks of
      nested procedures to the right block. 
*/
void
m3_fix_symtab ( struct symtab *st )
{ struct blockvector * block_vec; 
  int block_ss;
  int body_block_ss;
  struct block *bl = NULL;
  struct block *superblock = NULL;
  struct symbol *sym = NULL;
  struct symbol *ir = NULL;
  struct type *ir_type = NULL;
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
      gdb_flush (gdb_stdout);
    }

  /* Make a pass over the blockvector, setting the M3_BLOCK_BODY_BLOCK
     pointers. */ 
  for ( block_ss = 0; block_ss < BLOCKVECTOR_NBLOCKS ( block_vec ); block_ss ++)
    { bl = BLOCKVECTOR_BLOCK ( block_vec, block_ss );
      M3_BLOCK_BODY_BLOCK ( bl ) = bl; 
      if ( block_ss != GLOBAL_BLOCK 
           && block_ss != STATIC_BLOCK 
           && procedures_have_extra_block 
         )
        { superblock = BLOCK_SUPERBLOCK ( bl ); 
          if ( superblock != NULL && BLOCK_FUNCTION ( superblock ) != NULL ) 
            { M3_BLOCK_BODY_BLOCK ( superblock ) = bl; } 
        }  
    } 

  /* Make another pass, looking for the interface/module record, its name,
     and kind, and patching up nested procedure superblocks. */ 
  for ( block_ss = 0; block_ss < BLOCKVECTOR_NBLOCKS ( block_vec ); block_ss ++)
    { bl = BLOCKVECTOR_BLOCK ( block_vec, block_ss );
      ALL_BLOCK_SYMBOLS ( bl, iter, sym ) 
        { char *name = SYMBOL_LINKAGE_NAME ( sym );

          if ( info_verbose )
            { printf_filtered 
                ("Fixing block %d, linker symbol \"%s\"\n", block_ss , name );
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
                  else if ( strcmp ( st -> filename, "_m3main.mc" ) == 0 ) 
                    { } /* Neither is this. */   
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
      /* TODO: Factor out the nested procedure and block stuff. */ 
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
          struct block *body_block = NULL;
          struct symbol *parent_sym = NULL;
          struct symbol *proc_sym = NULL;
          int block_num = - 1;
          int block_num_2 = - 1; 
          int block_level = 0; 
          struct block * child_block = NULL; 
          char ch; 

          full_name = SYMBOL_LINKAGE_NAME ( sym ); 
          next_start = full_name; 
          ident_ct = 0; 

          do { prev_start = next_start; 
               ident_ct ++; 
             } 
          while 
            ( find_m3_nested_component 
                ( prev_start , &prev_end, &next_start ) 
            );
          /* Here, next_start is the start of the last identifier in the 
             qualified linker name.  */
          if ( ident_ct < 2 /* Not a procedure name */
               || ( ident_ct == 2 && prev_end + 2 == next_start ) 
                  /* Two idents with no block in between.  This is a top-level
                     procedure.  Its superblock will already be set to the 
                     static block. */  
             ) 
            { /* This block is for a topmost procedure, thus does not need 
                 its superblock field patched. */ 
            }
          else  /* This procedure is inside another programmer-declared 
                   procedure. [full_name, prev_end) is the qualified name of 
                   the containing procedure.  There could be anonymous blocks 
                   in between. */ 
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
              parent_block = BLOCKVECTOR_BLOCK ( block_vec, parent_block_ss );
              parent_sym = BLOCK_FUNCTION ( parent_block ); 
              proc_sym = parent_sym; 
              body_block = M3_BLOCK_BODY_BLOCK ( parent_block ); 
              if ( body_block != NULL && body_block != parent_block ) 
                { parent_block = body_block;  
                  while ( BLOCKVECTOR_BLOCK ( block_vec, parent_block_ss ) 
                          != parent_block 
                        ) 
                    { parent_block_ss ++; 
                      gdb_assert 
                        ( parent_block_ss < BLOCKVECTOR_NBLOCKS ( block_vec ) ); 
                    } 
                  parent_sym = BLOCK_FUNCTION ( parent_block ); 
                } 

              prev_end += 2; /* Skip "__" before the next name component. */ 
              if ( ident_ct == 2 
                   && cm3_comp_unit_body_name_len 
                        ( SYMBOL_LINKAGE_NAME ( proc_sym ) )
                      > 0  
                 ) 
                /* The parent procedure is a CM3-compiled compilation
                   unit body procedure, possibly with some anonymous
                   blocks between the nested procedure and it.  This 
                   will only happen if the comp unit is a module.  
                   CM3 flattens a block located inside the executable 
                   body of a module into its <moduleName>_M3 procedure 
                   in the actual stabs block construction, (by omitting 
                   the usual N_LBRAC, N_RBRAC pair), yet, peculiarly 
                   and inconsistently,  still adds a block number 1 
                   to the name of procedures located inside it.  We must 
                   consume this "1__" to get the code below to find the 
                   right block. */ 
                { while ( '0' <= *prev_end && *prev_end <= '9') 
                    { prev_end ++; } 
                  prev_end += 2; /* Skip "__" before the next name component. */
                }  
        
              /* Handle any anonymous blocks between parent and nested. */ 
              while ( prev_end < next_start ) /* There is an anonymous block. */
                { block_num = 0; 
                  while ( true )  
                    { ch = * prev_end; 
                      if ( '0' <= ch && ch <= '9' ) 
                        { block_num = block_num * 10 + ch - '0';
                          prev_end ++; /* and loop */ 
                        } 
                      else { break ; } 
                    } /* while */

                  /* Now block_num is a block sequence number of one
                     of possibly many M3 anonymous blocks that are
                     immediately inside the one identified by
                     parent_block_ss/parent_block/parent_sym.  The gdb
                     block we want is the block_num-th such block.
                     For anonymous blocks, the children always come
                     after the parent in the blockvector, so search
                     forwards from parent_block_ss.
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
                      child_block = BLOCKVECTOR_BLOCK ( block_vec, child_ss ); 
                      if ( BLOCK_FUNCTION ( child_block ) == NULL 
                            && BLOCK_SUPERBLOCK ( child_block ) == parent_block
                          ) 
                        /* child_block is for an anonymous, immediate child of 
                           block at parent_block_ss. */  
                        { block_num_2 --; 
                          if ( block_num_2 == 0 ) 
                            { parent_block_ss = child_ss; 
                              parent_block 
                                = BLOCKVECTOR_BLOCK 
                                    ( block_vec, parent_block_ss ); 
                              parent_sym = BLOCK_FUNCTION ( parent_block ); 
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

              if ( info_verbose )
                { if ( parent_sym != NULL ) 
                    { printf_filtered 
                        ( "Setting superblock of nested procedure \"%s\", "
                          " bv %d, to \"%s\", bv %d.\n",
                          full_name, 
                          block_ss, 
                          SYMBOL_SEARCH_NAME ( parent_sym ),
                          parent_block_ss 
                        );
                      list_block_symbols ( parent_block, 1); 
                    } 
                  else 
                    { printf_filtered 
                        ( "Setting superblock of nested procedure \"%s\","
                          " bv %d, to anonymous block, bv %d\n",
                          full_name, block_ss, parent_block_ss
                        );
                      list_block_symbols ( parent_block, 1 ); 
                    } 
                    gdb_flush (gdb_stdout);
                }
              BLOCK_SUPERBLOCK ( BLOCKVECTOR_BLOCK ( block_vec, block_ss ) )
                = parent_block; 
              done_with_nested: { }  
            } /* else nested procedure. */ 
        } /* if this is a procedure block. */ 
    } /* for blocks in blockvector. */ 

  if ( prefix_space . space_ptr != NULL ) 
    { free ( prefix_space . space_ptr ); } 

  if ( ir != 0 ) 
    { if ( ir_type == 0 ) 
        { /* This is happening when stabs info was not produced at all, and 
             objdump -G gives almost nothing, yet we get here with a lot of
             symbols from dwarf2read.  They do not look like the right 
             complement of symbols.  There are MI_ symbols for imported 
             interfaces as well as the one MM_ or MI_ for the unit itself.  
             But there is no MR_zzzzzz symbol.
          */ 
          error 
            ( "Debug info for file \"%s\" not in stabs format", st->filename );
          LHS_SYMBOL_TYPE (ir) = 0;
        } 
      else { LHS_SYMBOL_TYPE (ir) = ir_type; }
    }

  if ( info_verbose ) 
    { dump_blockvector ( block_vec, 50 );
      printf_filtered ( "Done with M3 symtab for file \"%s\"\n", st->filename );
      gdb_flush ( gdb_stdout );
    }

} /* m3_fix_symtab */ 

static void
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
m3_resolve_type ( char *uid ) 

{ int uid_val;
  struct symbol *sym = lookup_symbol ( uid, 0, STRUCT_DOMAIN, 0, 0);

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
} /* m3_resolve_type */ 

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
static int 
nested_prefix_len ( char * name) 

  { char * current; 
    char * i; 
  
    current = strchr (name, '.'); 
    if (current == 0) return 0;
    current++; 
    current = strchr (current, '.'); 
    if (current == 0) return 0;
    while ( (i = strchr (current + 1 , '.') ) > current) { current = i; } 
    return current - name;
  } /* nested_prefix_len */ 

/* For each block in bv that is for a nested procedure, patch the 
   BLOCK_SUPERBLOCK link to point to the block for the containing
   procedure.  Probably, there is not enough stabs info to do this
   correctly when the nested proc is inside a non-proc block.  
 */
void
m3_patch_nested_procs ( struct blockvector *bv ) 

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
  } /* m3_patch_nested_procs */ 

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

/* Return the result type of the procedure value proc_value. */ 
struct type *  
m3_proc_value_result_type ( struct value * proc_value ) 

  { struct type * closure_type; 
    struct type * proc_const_type; 

    if ( m3_value_is_proc_closure ( proc_value ) ) 
      { closure_type = value_type ( proc_value ); 
        proc_const_type = TYPE_TARGET_TYPE ( closure_type ); 
        return TYPE_TARGET_TYPE ( proc_const_type ); 
      } 
    else { return TYPE_TARGET_TYPE ( value_type ( proc_value ) ); } 
  } /* m3_proc_value_result_type */ 

/* If closure is a Modula-3 procedure closure value, return its code address.  
   Otherwise, return zero. */ 
CORE_ADDR 
m3_proc_value_code_addr ( struct value * closure_value ) 

  { if ( m3_value_is_proc_closure ( closure_value ) ) 
      { return 
          ( * ( struct m3_proc_closure * ) value_contents ( closure_value ) ) 
          . code_addr; 
      } 
    else { return value_as_address ( closure_value ); } 
  } /* m3_proc_valeu_code_addr */ 

/* If closure is a Modula-3 procedure closure, return its environment pointer. 
   Otherwise return zero. */ 
CORE_ADDR 
m3_proc_value_env_ptr ( struct value * closure_value ) 

  { if ( m3_value_is_proc_closure ( closure_value ) ) 
      { return 
          ( * ( struct m3_proc_closure * ) value_contents ( closure_value ) ) 
          . env_ptr; 
      } 
    else { return 0; } 
  } /* m3_proc_value_env_ptr */ 

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
            pointer_type = m3_indirect_type_from_type ( actual_type ); 
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

/* Print a description of a type in the format of a 
   typedef for the current language.
   NEW is the new name for a type TYPE. */
/* FIXME: This is now uncalled.  typedef_print in typeprint.c contains
   essentially a copy, in a switch on langauge.  Remove the copy and
   make it call this. rodney.bates@wichita.edu 2005-01-06 */

void  
m3_typedef_print ( 
    struct type *type,
    struct symbol *new,
    struct ui_file *stream
  ) 
{ fprintf_filtered(stream, "TYPE %s = ", SYMBOL_NATURAL_NAME(new));
  type_print(type,"",stream,0);
} /* m3_typedef_print */ 

/* indent is the depth to indent lines by.  */
void
m3_print_type (
     struct type *type,
     char *varstring,
     struct ui_file *stream,
     int show,
     int indent
   ) 
{
  m3_type_print_base (type, stream, show, indent);
  if (varstring != NULL && *varstring != '\0') {
    fputs_filtered (" ", stream);
    fputs_filtered (varstring, stream);
  }
} /* m3_print_type */

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
    { /* On top-level, prefix pointer value with (type).  RMB. For PM3, this 
         used to not write the prefix if it's a text, but I don't think it will
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

CORE_ADDR
m3_value_as_address (struct value * val)
{
  /* REVIEWME: Do we need the other cases found in value_as_address? */ 
  return m3_extract_address ( ( gdb_byte * ) value_contents ( val ), 0 );
}

/* End of file m3-lang.c */ 

