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

#include <string.h>
#include "defs.h"
#include "symtab.h"
#include "gdbtypes.h"
#include "expression.h"
#include "parser-defs.h"
#include "language.h"
#include "value.h"
#include "m3-lang.h"
#include "m3-uid.h"
#include "frame.h"
#include "target.h"

struct symbol * find_m3_ir ();

/* Print the character C on STREAM as part of the contents of a literal
   string whose delimiter is QUOTER.  Note that that format for printing
   characters and strings is language specific. */

static void
emit_char (c, stream, quoter)
     register int c;
     FILE *stream;
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
	case '\033':
	  fputs_filtered ("\\e", stream);
	  break;
	case '\007':
	  fputs_filtered ("\\a", stream);
	  break;
	default:
	  fprintf_filtered (stream, "\\%.3o", (unsigned int) c);
	  break;
	}
    }
}

void
m3_printchar (c, stream)
     int c;
     FILE *stream;
{
  fputs_filtered ("'", stream);
  emit_char (c, stream, '\'');
  fputs_filtered ("'", stream);
}

/* Print the character string STRING, printing at most LENGTH characters.
   Printing stops early if the number hits print_max; repeat counts
   are printed as appropriate.  Print ellipses at the end if we
   had to stop before printing LENGTH characters, or if FORCE_ELLIPSES.  */

static void
m3_printstr (stream, string, length, force_ellipses)
     FILE *stream;
     char *string;
     unsigned int length;
     int force_ellipses;
{
  register unsigned int i;
  unsigned int things_printed = 0;
  int in_quotes = 0;
  int need_comma = 0;
  extern int inspect_it;
  extern int repeat_count_threshold;
  extern int print_max;

  /* If the string was not truncated due to `set print elements', and
     the last byte of it is a null, we don't print that, in traditional C
     style.  */
  if ((!force_ellipses) && length > 0 && string[length-1] == '\0')
    length--;

  if (length == 0)
    {
      fputs_filtered ("\"\"", stdout);
      return;
    }

  for (i = 0; i < length && things_printed < print_max; ++i)
    {
      /* Position of the character we are examining
	 to see whether it is repeated.  */
      unsigned int rep1;
      /* Number of repetitions we have detected so far.  */
      unsigned int reps;

      QUIT;

      if (need_comma)
	{
	  fputs_filtered (", ", stream);
	  need_comma = 0;
	}

      rep1 = i + 1;
      reps = 1;
      while (rep1 < length && string[rep1] == string[i])
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
	  emit_char (string[i], stream, '"');
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

  if (force_ellipses || i < length)
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
  register struct type *type = NULL;

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


/* Table mapping opcodes into strings for printing operators
   and precedences of the operators.  */

static const struct op_print m3_op_print_tab[] =
  {
    {",",  BINOP_COMMA, PREC_COMMA, 0},
    {"=",  BINOP_ASSIGN, PREC_ASSIGN, 1},
    {"||", BINOP_LOGICAL_OR, PREC_LOGICAL_OR, 0},
    {"&&", BINOP_LOGICAL_AND, PREC_LOGICAL_AND, 0},
    {"|",  BINOP_BITWISE_IOR, PREC_BITWISE_IOR, 0},
    {"^",  BINOP_BITWISE_XOR, PREC_BITWISE_XOR, 0},
    {"&",  BINOP_BITWISE_AND, PREC_BITWISE_AND, 0},
    {"==", BINOP_EQUAL, PREC_EQUAL, 0},
    {"!=", BINOP_NOTEQUAL, PREC_EQUAL, 0},
    {"<=", BINOP_LEQ, PREC_ORDER, 0},
    {">=", BINOP_GEQ, PREC_ORDER, 0},
    {">",  BINOP_GTR, PREC_ORDER, 0},
    {"<",  BINOP_LESS, PREC_ORDER, 0},
    {">>", BINOP_RSH, PREC_SHIFT, 0},
    {"<<", BINOP_LSH, PREC_SHIFT, 0},
    {"+",  BINOP_ADD, PREC_ADD, 0},
    {"-",  BINOP_SUB, PREC_ADD, 0},
    {"*",  BINOP_MUL, PREC_MUL, 0},
    {"/",  BINOP_DIV, PREC_MUL, 0},
    {"%",  BINOP_REM, PREC_MUL, 0},
    {"@",  BINOP_REPEAT, PREC_REPEAT, 0},
    {"-",  UNOP_NEG, PREC_PREFIX, 0},
    {"!",  UNOP_LOGICAL_NOT, PREC_PREFIX, 0},
    {"~",  UNOP_COMPLEMENT, PREC_PREFIX, 0},
    {"*",  UNOP_IND, PREC_PREFIX, 0},
    {"&",  UNOP_ADDR, PREC_PREFIX, 0},
    {"sizeof ", UNOP_SIZEOF, PREC_PREFIX, 0},
    {"++", UNOP_PREINCREMENT, PREC_PREFIX, 0},
    {"--", UNOP_PREDECREMENT, PREC_PREFIX, 0},
    /* C++  */
    {"::", BINOP_SCOPE, PREC_PREFIX, 0},
    {NULL, 0, 0, 0}
};


struct type ** const (m3_builtin_types[]) = 
{
  &builtin_type_m3_integer,
  &builtin_type_long,
  &builtin_type_short,
  &builtin_type_char,
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
m3_error (msg)
     char *msg;
{
  error (msg ? msg : "Invalid syntax in expression.");
}


const struct language_defn m3_language_defn = {
  "m3",				/* Language name */
  language_m3,
  m3_builtin_types,
  range_check_on,
  type_check_off,
  m3_parse,
  m3_error,
  evaluate_subexp_standard,
  m3_printchar,			/* Print a character constant */
  m3_printstr,			/* Function to print string constant */
  m3_create_fundamental_type,	/* Create fundamental type in this language */
  m3_print_type,		/* Print a type using appropriate syntax */
  m3_val_print,			/* Print a value using appropriate syntax */
  m3_value_print,		/* Print a top-level value */
  {"",       "",     "",  ""},	/* Binary format info */
  {"8_%lo",  "8_",   "o", ""},	/* Octal format info */
  {"%ld",    "",     "d", ""},	/* Decimal format info */
  {"16_%lx", "16_",  "x", ""},	/* Hex format info */
  m3_op_print_tab,		/* expression operators for printing */
  0,				/* arrays are first-class (not c-style) */
  0,				/* String lower bound */
  &builtin_type_char,		/* Type of string elements */ 
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

static void 
init_thread_constants ()
{
  int thread__t__context_size, thread__t__context_offset;
  struct type * thread__t__context_type;

  if (thread__t) return;

  find_m3_ir ('I', "Thread");
  find_m3_ir ('M', "ThreadPosix");

  thread__t = find_m3_type_named ("Thread.T");

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
  thread__t__id_offset    += TARGET_PTR_BIT;
  thread__t__state_offset += TARGET_PTR_BIT;
  thread__t__next_offset  += TARGET_PTR_BIT;
  thread__t__cond_offset  += TARGET_PTR_BIT;
  thread__t__mutex_offset += TARGET_PTR_BIT;
  thread__t__time_offset  += TARGET_PTR_BIT;
  thread__t__buf_offset   += TARGET_PTR_BIT + thread__t__context_offset;
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

#if defined(linux)
/* see config/i386/tm-i386.h/REGISTER_NAMES */
#define HAVE_REGISTER_MAP
static int regno_to_jmpbuf [] = {
 -1 /* eax */,  -1 /* ecx */,   -1 /* edx */,   0 /* ebx */,
  4 /* esp */,   3 /* ebp */,    1 /* esi */,   2 /* edi */,
  5 /* eip */,  -1 /* eflags */,-1 /* cs */,   -1 /* ss */,
 -1 /* ds */,   -1 /* es */,    -1 /* fs */,   -1 /* gs */,
 -1 /* st0 */,  -1 /* st1 */,   -1 /* st2 */,  -1 /* st3 */,
 -1 /* st4 */,  -1 /* st5 */,   -1 /* st6 */,  -1 /* st7 */,
};
#endif

#if defined(sparc)
/* see config/sparc/tm-sparc.h/REGISTER_NAMES */
#define HAVE_REGISTER_MAP
static int regno_to_jmpbuf [] = {
  -1 /* g0 */,   6 /* g1 */,  -1 /* g2 */,  -1 /* g3 */,
  -1 /* g4 */,  -1 /* g5 */,  -1 /* g6 */,  -1 /* g7 */,
   7 /* o0 */,  -1 /* o1 */,  -1 /* o2 */,  -1 /* o3 */,
  -1 /* o4 */,  -1 /* o5 */,   2 /* sp */,  -1 /* o7 */,
  -1 /* l0 */,  -1 /* l1 */,  -1 /* l2 */,  -1 /* l3 */,
  -1 /* l4 */,  -1 /* l5 */,  -1 /* l6 */,  -1 /* l7 */,
  -1 /* i0 */,  -1 /* i1 */,  -1 /* i2 */,  -1 /* i3 */,
  -1 /* i4 */,  -1 /* i5 */,  -1 /* fp */,  -1 /* i7 */,

  -1 /* f0 */,  -1 /* f1 */,  -1 /* f2 */,  -1 /* f3 */,
  -1 /* f4 */,  -1 /* f5 */,  -1 /* f6 */,  -1 /* f7 */,
  -1 /* f8 */,  -1 /* f9 */,  -1 /* f10 */, -1 /* f11 */,
  -1 /* f12 */, -1 /* f13 */, -1 /* f14 */, -1 /* f15 */,
  -1 /* f16 */, -1 /* f17 */, -1 /* f18 */, -1 /* f19 */,
  -1 /* f20 */, -1 /* f21 */, -1 /* f22 */, -1 /* f23 */,
  -1 /* f24 */, -1 /* f25 */, -1 /* f26 */, -1 /* f27 */,
  -1 /* f28 */, -1 /* f29 */, -1 /* f30 */, -1 /* f31 */,

  -1 /* y */,    5 /* psr */, -1 /* wim */, -1 /* tbr */,
   3 /* pc */,   4 /* npc */, -1 /* fpsr */,-1 /* cpsr */
};
#endif

#if defined(mips) && defined(sgi)
/* see config/mips/tm-irix3.h/REGISTER_NAMES */
#define HAVE_REGISTER_MAP
static int regno_to_jmpbuf [] = {
   0 /* zero */,  0 /* at */,    0 /* v0 */,    0 /* v1 */,
   0 /* a0 */,    0 /* a1 */,    0 /* a2 */,    0 /* a3 */,
   0 /* t0 */,    0 /* t1 */,    0 /* t2 */,    0 /* t3 */,
   0 /* t4 */,    0 /* t5 */,    0 /* t6 */,    0 /* t7 */,
   0 /* s0 */,    0 /* s1 */,    0 /* s2 */,    0 /* s3 */,
   0 /* s4 */,    0 /* s5 */,    0 /* s6 */,    0 /* s7 */,
   0 /* t8 */,    0 /* t9 */,    0 /* k0 */,    0 /* k1 */,
   0 /* gp */,    0 /* sp */,    0 /* fp */,    0 /* ra */,
   0 /* f0 */,    0 /* f1 */,    0 /* f2 */,    0 /* f3 */,
   0 /* f4 */,    0 /* f5 */,    0 /* f6 */,    0 /* f7 */,
   0 /* f8 */,    0 /* f9 */,    0 /* f10 */,   0 /* f11 */,
   0 /* f12 */,   0 /* f13 */,   0 /* f14 */,   0 /* f15 */,
   0 /* f16 */,   0 /* f17 */,   0 /* f18 */,   0 /* f19 */,
   0 /* f20 */,   0 /* f21 */,   0 /* f22 */,   0 /* f23 */,
   0 /* f24 */,   0 /* f25 */,   0 /* f26 */,   0 /* f27 */,
   0 /* f28 */,   0 /* f29 */,   0 /* f30 */,   0 /* f31 */,
   0 /* pc */,    0 /* cause */, 0 /* bad */,   0 /* hi */,
   0 /* lo */,    0 /* fsr */,   0 /* fir */
};
#endif

#if defined(hppa)
/* see config/mips/tm-hppa.h/REGISTER_NAMES */
#define HAVE_REGISTER_MAP
static int regno_to_jmpbuf [] = {
  0 /* flags */,   0 /* r1 */,      0 /* rp */,      0 /* r3 */, 
  0 /* r4 */,      0 /* r5 */,      0 /* r6 */,      0 /* r7 */, 
  0 /* r8 */,      0 /* r9 */,      0 /* r10 */,     0 /* r11 */, 
  0 /* r12 */,     0 /* r13 */,     0 /* r14 */,     0 /* r15 */, 
  0 /* r16 */,     0 /* r17 */,     0 /* r18 */,     0 /* r19 */, 
  0 /* r20 */,     0 /* r21 */,     0 /* r22 */,     0 /* r23 */, 
  0 /* r24 */,     0 /* r25 */,     0 /* r26 */,     0 /* dp */, 
  0 /* ret0 */,    0 /* ret1 */,    0 /* sp */,      0 /* r31 */, 
  0 /* sar */,     0 /* pcoqh */,   0 /* pcsqh */,   0 /* pcoqt */, 
  0 /* pcsqt */,   0 /* eiem */,    0 /* iir */,     0 /* isr */, 
  0 /* ior */,     0 /* ipsw */,    0 /* goto */,    0 /* sr4 */, 
  0 /* sr0 */,     0 /* sr1 */,     0 /* sr2 */,     0 /* sr3 */, 
  0 /* sr5 */,     0 /* sr6 */,     0 /* sr7 */,     0 /* cr0 */, 
  0 /* cr8 */,     0 /* cr9 */,     0 /* ccr */,     0 /* cr12 */, 
  0 /* cr13 */,    0 /* cr24 */,    0 /* cr25 */,    0 /* cr26 */, 
  0 /* mpsfu_high */, 0 /* mpsfu_low */, 0 /* mpsfu_ovflo */,   0 /* pad */, 
  0 /* fpsr */,    0 /* fpe1 */,    0 /* fpe2 */,    0 /* fpe3 */, 
  0 /* fpe4 */,    0 /* fpe5 */,    0 /* fpe6 */,    0 /* fpe7 */, 
  0 /* fr4 */,     0 /* fr4R */,    0 /* fr5 */,     0 /* fr5R */, 
  0 /* fr6 */,     0 /* fr6R */,    0 /* fr7 */,     0 /* fr7R */, 
  0 /* fr8 */,     0 /* fr8R */,    0 /* fr9 */,     0 /* fr9R */, 
  0 /* fr10 */,    0 /* fr10R */,   0 /* fr11 */,    0 /* fr11R */, 
  0 /* fr12 */,    0 /* fr12R */,   0 /* fr13 */,    0 /* fr13R */, 
  0 /* fr14 */,    0 /* fr14R */,   0 /* fr15 */,    0 /* fr15R */, 
  0 /* fr16 */,    0 /* fr16R */,   0 /* fr17 */,    0 /* fr17R */, 
  0 /* fr18 */,    0 /* fr18R */,   0 /* fr19 */,    0 /* fr19R */, 
  0 /* fr20 */,    0 /* fr20R */,   0 /* fr21 */,    0 /* fr21R */, 
  0 /* fr22 */,    0 /* fr22R */,   0 /* fr23 */,    0 /* fr23R */, 
  0 /* fr24 */,    0 /* fr24R */,   0 /* fr25 */,    0 /* fr25R */, 
  0 /* fr26 */,    0 /* fr26R */,   0 /* fr27 */,    0 /* fr27R */, 
  0 /* fr28 */,    0 /* fr28R */,   0 /* fr29 */,    0 /* fr29R */, 
  0 /* fr30 */,    0 /* fr30R */,   0 /* fr31 */,    0 /* fr31R */
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
     CORE_ADDR ref;
     M3_THREAD *t;
{
    /* in case we get stuck */
    t->ref   = ref;
    t->id    = 0;
    t->bits  = 0;

    if (!ref) return;

    m3_read_object_fields_bits (ref, thread__t, 0, &(t->bits));
    if (!t->bits) return;

    t->id = m3_unpack_ord (t->bits, thread__t__id_offset,thread__t__id_size,0);
}

static void
first_m3_thread (t)
     M3_THREAD *t;
{
    value_ptr v = eval ("ThreadPosix.self");
    LONGEST ref = m3_unpack_pointer (VALUE_CONTENTS (v), 0);
    get_m3_thread (ref, t);
}

next_m3_thread (t)
     M3_THREAD *t;
{
    LONGEST ref;

    if (!t) return;
    if (!(t->bits)) return;
    ref = m3_unpack_pointer (t->bits, thread__t__next_offset);
    get_m3_thread (ref, t);
}

/*---------------------------------------------------------------- switch ---*/

/* the "current" thread for the user */
static M3_THREAD cur_m3_thread = { 0, 0, 0 };

static void
look_in_thread (regno)
     int regno;
{
  static char* NO_REG_MSG =
    "%s, line %d: don't know where to find register \"%s\" in stopped thread";
  static char* NO_REGS_MSG =
    "%s, line %d: don't know where to find registers in stopped thread";
#ifdef HAVE_REGISTER_MAP
  int reg_offset, reg_index;

  if (cur_m3_thread.ref == 0) { first_m3_thread (&cur_m3_thread); }

  if (cur_m3_thread.bits) {
    if (regno < 0) {
      for (regno = 0; regno < NUM_REGS; regno++) {
	reg_index = regno_to_jmpbuf [regno];
	if (reg_index >= 0) {
          reg_offset = thread__t__buf_offset / HOST_CHAR_BIT
	             + reg_index * TARGET_PTR_BIT / HOST_CHAR_BIT;
          supply_register (regno, cur_m3_thread.bits + reg_offset);
        }
      }
    } else if (regno < NUM_REGS) { /* we only need one register */
      reg_index = regno_to_jmpbuf [regno];
      if (reg_index >= 0) {
	reg_offset = thread__t__buf_offset / HOST_CHAR_BIT
	           + reg_index * TARGET_PTR_BIT / HOST_CHAR_BIT;
	supply_register (regno, cur_m3_thread.bits + reg_offset);
      } else {
        error (NO_REG_MSG, __FILE__, __LINE__, reg_names[regno]);
      }
    } else { /* bogus register number? */
      error (NO_REGS_MSG, __FILE__, __LINE__);
    }
  } else {
    error ("%s, line %d: no Modula-3 thread information is available.",
	   __FILE__, __LINE__);
  }
#else
  if ((regno >= 0) && (regno < NUM_REGS)) {
    error (NO_REG_MSG, __FILE__, __LINE__, reg_names[regno]);
  } else {
    error (NO_REGS_MSG, __FILE__, __LINE__);
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

  init_thread_constants ();

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

  registers_changed ();
  reinit_frame_cache ();
}

/*--------------------------------------------------------------- threads ---*/

static void
threads_command (args, from_tty)
     char *args;
     int from_tty;
{
  int first_id, state;
  M3_THREAD cur;

  init_thread_constants ();

  first_m3_thread (&cur);
  first_id = cur.id;

  fprintf_filtered (stdout, "-id-   -Thread.T-  -state-\n");
  while (cur.bits) {
    fprintf_filtered (stdout, "%4d  16_%lx  ", cur.id, cur.ref);

    state = m3_unpack_ord (cur.bits, thread__t__state_offset,
			   thread__t__state_size, 0);
    switch (state) {
      case 0 /* alive */:
	fprintf_filtered (stdout, "alive");
	break; 
      case 1 /* waiting */:
	fprintf_filtered (stdout, "waiting for condition 16_%x",
		  m3_unpack_pointer (cur.bits, thread__t__cond_offset));
	break;
      case 2 /* locking */:
	fprintf_filtered (stdout, "waiting for mutex 16_%x",
		  m3_unpack_pointer (cur.bits, thread__t__mutex_offset));
	break;
      case 3 /* pausing */:
	fprintf_filtered (stdout, "waiting until ");
	m3_val_print2 (thread__t__time_type, cur.bits, thread__t__time_offset, 
		       thread__t__time_size, stdout, 0, 0);
	break;
      case 4 /* blocking */:
	fprintf_filtered (stdout, "waiting for I/O");
	break;
      case 5 /* dying */:
	fprintf_filtered (stdout, "waiting for somebody to join");
	break;
      case 6 /* dead */:
	fprintf_filtered (stdout, "dead");
	break;
      default:
	fprintf_filtered (stdout, "<unknown state = %d>", state);
	break;
      }
    fputs_filtered ("\n", stdout);

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
struct type *builtin_type_m3_char;
struct type *builtin_type_m3_real;
struct type *builtin_type_m3_longreal;
struct type *builtin_type_m3_extended;
struct type *builtin_type_m3_null;
struct type *builtin_type_m3_refany;
struct type *builtin_type_m3_mutex;
struct type *builtin_type_m3_text;
struct type *builtin_type_m3_untraced_root;
struct type *builtin_type_m3_void;

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

  builtin_type_m3_char =
    init_type (TYPE_CODE_M3_CHAR, TARGET_CHAR_BIT / HOST_CHAR_BIT, 0,
	       "CHAR", (struct objfile *) NULL);
  TYPE_M3_SIZE (builtin_type_m3_char) = TARGET_CHAR_BIT;

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

  add_language (&m3_language_defn);
  add_com ("threads", class_stack, threads_command, "Lists the threads.");
  add_com ("switch", class_stack, switch_command, "Allows to examine the stack of another thread.");
}

char *
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
{
  if (i < TYPE_NFIELDS(t)) {
    strncpy (TYPE_FIELD_M3_UID (t, i), TYPE_FIELD_NAME (t, i), M3UID_LEN);
    TYPE_FIELD_M3_UID (t, i) [M3UID_LEN] = 0;
    TYPE_FIELD_TYPE (t,i) = 0;
    TYPE_FIELD_NAME (t,i) += M3UID_LEN;
  }
}

void 
m3_decode_struct (t)
     struct type *t;
{
  int i;
  long size;
  char *key, *type_specific_info;
  
  /* the format is M<kind>_<uid>_<size>_<other info>
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
	sscanf (TYPE_FIELD_NAME (t, i), "_%d_%d", 
		&TYPE_FIELD_BITPOS (t, i), &TYPE_FIELD_BITSIZE (t, i)); 
	TYPE_FIELD_NAME (t, i) =
	  skip_underscores (TYPE_FIELD_NAME (t, i), 3); }
      break;

    case 'O':
      TYPE_CODE (t) = TYPE_CODE_M3_OBJECT;
      sscanf (type_specific_info, "%ld_%ld_%ld_",
	      &TYPE_M3_OBJ_NFIELDS (t), &TYPE_M3_OBJ_TRACED (t),
	      &TYPE_M3_OBJ_BRANDED (t));
      if (TYPE_M3_OBJ_BRANDED (t)) {
	TYPE_M3_OBJ_BRAND (t) = skip_underscores (type_specific_info, 3); }
      else {
	TYPE_M3_OBJ_BRAND (t) = 0; }

      TYPE_M3_OBJ_NMETHODS (t) = TYPE_NFIELDS (t) - TYPE_M3_OBJ_NFIELDS(t) - 1;
      set_field_uid (t, 0);

      for (i = 1; i < TYPE_NFIELDS (t); i++) {
	set_field_uid (t, i);
	sscanf (TYPE_FIELD_NAME (t, i), "_%d_%d_",
		&TYPE_FIELD_BITPOS (t, i), &TYPE_FIELD_BITSIZE (t, i));
	TYPE_FIELD_NAME (t, i) =
	  skip_underscores (TYPE_FIELD_NAME (t, i), 3); }
      break;

    case 'S':
      TYPE_CODE (t) = TYPE_CODE_M3_SET;
      set_field_uid (t, 0);
      break;

    case 'Z':
      TYPE_CODE (t) = TYPE_CODE_M3_SUBRANGE;
      sscanf (type_specific_info, "%ld_%ld", 
	      &TYPE_M3_SUBRANGE_MIN (t), &TYPE_M3_SUBRANGE_MAX (t));
      set_field_uid (t, 0);
      break;

    case 'Y':
      TYPE_CODE (t) = TYPE_CODE_M3_POINTER;
      sscanf (type_specific_info, "%ld_%ld_", 
	      &TYPE_M3_POINTER_TRACED (t),
	      &TYPE_M3_POINTER_BRANDED (t));
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
      TYPE_CODE (t) = TYPE_CODE_M3_PROC;
      sscanf (type_specific_info, "%c%ld", &c, &TYPE_M3_PROC_NRAISES (t));
      if (c == 'A') {		/* RAISES ANY */
	TYPE_M3_PROC_NARGS (t) = TYPE_NFIELDS (t) - 1;
	TYPE_M3_PROC_NRAISES (t) = -1;
	for (i = 0; i < TYPE_NFIELDS (t); i++) {
	  set_field_uid (t, i); }}
      else {
	TYPE_M3_PROC_NARGS (t) = TYPE_NFIELDS (t) - TYPE_M3_PROC_NRAISES (t) - 1;
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
    sscanf (key + 3 + M3UID_LEN, "_%ld", &TYPE_M3_SIZE (t));
    TYPE_LENGTH (t) = (TYPE_M3_SIZE (t) + 7) / 8;
  }

  /* finally, grab the UID */
  TYPE_TAG_NAME (t) = key + 3;
  TYPE_TAG_NAME (t) [M3UID_LEN] = 0;
}

char *
m3_demangle (mangled)
     char *mangled;
{
  int i, uid;
  char demangled [100];

  char * u;

  if (mangled [0] == 'M' && mangled [2] == '_') {
    switch (mangled[1]) {

    case '3':
      /* local variable encoding: M3_<uid>_<name> */
      if (m3uid_to_int (mangled + 3, &uid)) {
        sprintf (demangled, "%s", mangled + 4 + M3UID_LEN);
        return strsave (demangled);
      };
      break;

    case 'I':
      /* m3 interface record: MI_<name> */
      sprintf (demangled, "I$%s", mangled + 3);
      return strsave (demangled);

    case 'M':
      /* m3 module record: MM_<name> */
      sprintf (demangled, "M$%s", mangled + 3);
      return strsave (demangled);

    case 'N':
      /* m3 type name for type uid: MN_<uid> */
      if (m3uid_to_int (mangled + 3, &uid)) {
	sprintf (demangled, "G$%.*s", M3UID_LEN, mangled + 3);
        return strsave (demangled);
      };
      break;

    case 'n':
      /* m3 type uid for type name: Mn_<uid>_<name> */
      if (m3uid_to_int (mangled + 3, &uid)) {
	sprintf (demangled, "B$%s", mangled + 4 + M3UID_LEN);
        return strsave (demangled);
      };
      break;

    case 'i':
      /* m3 exported interfaces Mi_zzzzzz_<module> */
      if (m3uid_to_int (mangled + 3, &uid)) {
  	sprintf (demangled, "H$%s", mangled + 4 + M3UID_LEN);
        return strsave (demangled);
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
      /* m3 type encoding: M?_<uid>* */
      if (m3uid_to_int (mangled + 3, &uid)) {
	sprintf (demangled, "%.*s", M3UID_LEN, mangled + 3);
        return strsave (demangled);
      };
      break;
    }  /* switch */
  }  /* if "M?_" */

  /* type init proc: _t<uid>_INIT */
  if (mangled [0] == '_' && mangled [1] == 't'
      && mangled [2 + M3UID_LEN] == '_'
      && mangled [3 + M3UID_LEN] == 'I'
      && mangled [4 + M3UID_LEN] == 'N'
      && mangled [5 + M3UID_LEN] == 'I'
      && mangled [6 + M3UID_LEN] == 'T'
      && mangled [7 + M3UID_LEN] == 0) {
    if (m3uid_to_int (mangled + 2, &uid)) {
      sprintf (demangled, "D$%.*s", M3UID_LEN, mangled + 2); 
      return strsave (demangled);
    }
  }

  /* compilation unit body: _INIT[IM]_* */
  if (mangled [0] == '_'
      && mangled [1] == 'I'
      && mangled [2] == 'N'
      && mangled [3] == 'I'
      && mangled [4] == 'T' 
      && (mangled [5] == 'I' || mangled [5] == 'M')
      && mangled [6] == '_') {
    sprintf (demangled, "%s.%c3.MAIN", 
	     mangled + 7, mangled [5] == 'I' ? 'i' : 'm');
    return strsave (demangled);
  }

  /* procedure: *__* */
  if ((u = strchr (mangled, '_')) && u != mangled && u[1] == '_') {
    strncpy (demangled, mangled, u - mangled);
    demangled [u - mangled] = '.';
    strcpy (demangled + (u - mangled) + 1, u + 2);
    return strsave (demangled);
  }

  return 0;
}

/* we have just read a symtab; fix it for Modula-3 purposes.
   We want to clean variables: we should forget the type
      indicated in the symbol table,
      remember the uid in the place where the type resolver will find it.
   We also want to find the connection between an interface record
      and its type description (the uid of interface records is -1; 
      this is about the only place where we have the scope information
      that is necessary to make the connection. */

void
m3_fix_symtab (st)
     struct symtab *st;
{
  int i, j;
  struct block *b;
  struct symbol *ir = 0;
  struct type *ir_type = 0;
  char *ir_name;
  char *ir_kind;

  for (i = 0; i < BLOCKVECTOR_NBLOCKS (BLOCKVECTOR (st)); i ++) {
    b = BLOCKVECTOR_BLOCK (BLOCKVECTOR (st), i);
    for (j = 0; j < BLOCK_NSYMS (b); j++) {
      struct symbol *s = BLOCK_SYM (b, j);
      char *name = SYMBOL_NAME (s);

      if (name [0] == 'M' && name [2] == '_') {
	if (name [1] == 'I') {
	  ir = s;
	  ir_name = name + 3;
	  ir_kind = "interface";
	} else if (name [1] == 'M') {
	  ir = s;
	  ir_name = name + 3;
	  ir_kind = "module";
	} else if (name [1] == '3' && SYMBOL_NAMESPACE (s) == VAR_NAMESPACE) {
	  SET_SYMBOL_TYPE (s) = 0; 
	  strncpy (s->m3_uid, name + 3, M3UID_LEN);
	  s->m3_uid [M3UID_LEN] = 0;
	} else if (strncmp (name + 1, "R_zzzzzz", 8) == 0) {
	  ir_type = SYMBOL_TYPE (s);
	}
      }

    }
  }

  if (ir) {
    if (ir_type == 0) {
      error ("%s \"%s\": missing debug info for global data", ir_kind,ir_name);
    } else {
      SET_SYMBOL_TYPE (ir) = ir_type;
    }
  }
}

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
  struct symbol *sym = lookup_symbol (uid, 0, STRUCT_NAMESPACE, 0, 0);

  if (sym) {
    struct type *t = SYMBOL_TYPE (sym);
    if (TYPE_CODE (t) == TYPE_CODE_M3_OPAQUE) {
      t = m3_resolve_type (TYPE_FIELD_M3_UID (t, 0)); };
    m3_fix_target_type (t);
    return t;
  };

  if (m3uid_to_int (uid, &uid_val)) {
    if      (uid_val == 0x195c2a74) { return builtin_type_m3_integer; }
    else if (uid_val == 0x97e237e2) { return builtin_type_m3_cardinal; }
    else if (uid_val == 0x1e59237d) { return builtin_type_m3_boolean; }
    else if (uid_val == 0x08402063) { return builtin_type_m3_address; }
    else if (uid_val == 0x9d8fb489) { return builtin_type_m3_root; }
    else if (uid_val == 0x56e16863) { return builtin_type_m3_char; }
    else if (uid_val == 0x48e16572) { return builtin_type_m3_real; }
    else if (uid_val == 0x94fe32f6) { return builtin_type_m3_longreal; }
    else if (uid_val == 0x9ee024e3) { return builtin_type_m3_extended; }
    else if (uid_val == 0x48ec756e) { return builtin_type_m3_null; }
    else if (uid_val == 0x1c1c45e6) { return builtin_type_m3_refany; }
    else if (uid_val == 0x898ea789) { return builtin_type_m3_untraced_root; }
    else if (uid_val == 0x00000000) { return builtin_type_m3_void; }
  }

  error ("Cannot resolve type with uid %s", uid);
}

struct type *
find_m3_type_named (name)
     char *name;
{
  char struct_name [100];
  struct symbol *s;

  sprintf (struct_name, "B$%s", name);
  s = lookup_symbol (struct_name, 0, STRUCT_NAMESPACE, 0, 0);
  if (s == NULL) {
    error ("unable to find type named \"%s\"\n", name);
    return NULL;
  };
  return TYPE_M3_NAME_TYPE (SYMBOL_TYPE (s));
}

struct type *
find_m3_exported_interfaces (name)
     char *name;
     /* return the record type that has one field for each exported
	interface; note that if the result is NIL, this means
        that the module exports itself only. */
{
  char struct_name [100];
  struct symbol *s;

  sprintf (struct_name, "H$%s", name);
  if (s = lookup_symbol (struct_name, 0, STRUCT_NAMESPACE, 0, 0)) {
    return (SYMBOL_TYPE (s));
  } else {
    return 0;
  }
}

struct symbol *
find_m3_ir (kind, name)
     int kind; 
     char *name;
{
  char struct_name [100];
  sprintf (struct_name, "%c$%s", kind, name);
  return lookup_symbol (struct_name, 0, VAR_NAMESPACE, 0, 0);
}

char *
find_m3_type_name (t)
     struct type *t;
{
  char *uid = TYPE_TAG_NAME (t);
  char struct_name [100];
  struct symbol *sym;

  if (TYPE_NAME (t) == 0) {
    if (uid == NULL) return 0;
    sprintf (struct_name, "G$%s", uid);
    if (sym = lookup_symbol (struct_name, 0, STRUCT_NAMESPACE, 0, 0)) {
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

static int rt0_tc_selfID_size,          rt0_tc_selfID_offset;
static int rt0_tc_dataOffset_size,      rt0_tc_dataOffset_offset;
static int rt0_tc_methodOffset_size,    rt0_tc_methodOffset_offset;
static int rt0_tc_dataSize_size,        rt0_tc_dataSize_offset;
static int rt0_tc_parent_size,          rt0_tc_parent_offset;
static int rt0_tc_defaultMethods_size,  rt0_tc_defaultMethods_offset;
static CORE_ADDR rt0u_types_value;

void
init_m3_constants ()
{
  struct type* rt0_tc;
  struct symbol *rt0u;
  int rt0u_types_size, rt0u_types_offset;

  if (rt0u_types_value) { return; }

  rt0_tc = find_m3_type_named ("RT0.Typecell");

  find_m3_rec_field (rt0_tc, "selfID",
		     &rt0_tc_selfID_size, &rt0_tc_selfID_offset, 0);
  find_m3_rec_field (rt0_tc, "dataOffset", 
		     &rt0_tc_dataOffset_size, &rt0_tc_dataOffset_offset, 0);
  find_m3_rec_field (rt0_tc, "methodOffset", 
		     &rt0_tc_methodOffset_size, &rt0_tc_methodOffset_offset, 0);
  find_m3_rec_field (rt0_tc, "dataSize",
		     &rt0_tc_dataSize_size, &rt0_tc_dataSize_offset, 0);
  find_m3_rec_field (rt0_tc, "parent",
		     &rt0_tc_parent_size, &rt0_tc_parent_offset, 0);
  find_m3_rec_field (rt0_tc, "defaultMethods", 
		     &rt0_tc_defaultMethods_size, 
		     &rt0_tc_defaultMethods_offset, 0);

  rt0u = find_m3_ir ('I', "RT0u");

  find_m3_rec_field (SYMBOL_TYPE (rt0u), "types", 
		     &rt0u_types_size, &rt0u_types_offset, 0);
  
  target_read_memory (SYMBOL_VALUE_ADDRESS (rt0u) + rt0u_types_offset / 8,
		      (char *)&rt0u_types_value, rt0u_types_size / 8);
}

/*
 * should return a ref to the typecell for the var at addr
 */
CORE_ADDR 
find_m3_heap_tc_addr (addr)
     CORE_ADDR addr;
{
  LONGEST typecode;
  CORE_ADDR result;

  init_m3_constants ();

  target_read_memory (addr - (TARGET_PTR_BIT / TARGET_CHAR_BIT), 
		      (char *)&typecode, 
		      TARGET_PTR_BIT / TARGET_CHAR_BIT);

  /* the typecode is in Modula-3 bits 1..21 */
#if TARGET_BYTE_ORDER == BIG_ENDIAN
  typecode = (typecode >> 11) & 0xfffff;
#else
  typecode = (typecode >> 1) & 0xfffff;
#endif

  target_read_memory (rt0u_types_value 
		      + typecode * TARGET_PTR_BIT / TARGET_CHAR_BIT,
		      (char *)&result, TARGET_PTR_BIT / TARGET_CHAR_BIT);
  return result;
}

struct type *
find_m3_type_from_tc (tc_addr)
     CORE_ADDR tc_addr;
{
  int selfID;

  init_m3_constants ();

  target_read_memory (tc_addr + rt0_tc_selfID_offset / TARGET_CHAR_BIT,
		      (char *)&selfID, rt0_tc_selfID_size / HOST_CHAR_BIT);

  return (m3_resolve_type (m3uid_from_int (selfID)));
}

struct type *
find_m3_heap_type (addr)
     CORE_ADDR addr;
{
  return find_m3_type_from_tc (find_m3_heap_tc_addr (addr));
}


/* return LOOPHOLE (tc_addr, RT0.TypeDefn).dataOffset */
int 
tc_address_to_dataOffset (tc_addr)
     CORE_ADDR tc_addr;
{
  int result;
  init_m3_constants ();

  target_read_memory (tc_addr + rt0_tc_dataOffset_offset / 8,
		      (char *)&result, rt0_tc_dataOffset_size / 8);
  return result;
}

int 
tc_address_to_methodOffset (tc_addr)
     CORE_ADDR tc_addr;
{
  int result;
  init_m3_constants ();
  target_read_memory (tc_addr + rt0_tc_methodOffset_offset / TARGET_CHAR_BIT,
		      (char *)&result, rt0_tc_methodOffset_size / TARGET_CHAR_BIT);
  return result;
}
		      
int 
tc_address_to_dataSize (tc_addr)
     CORE_ADDR tc_addr;
{
  int result;
  init_m3_constants ();
  target_read_memory (tc_addr + rt0_tc_dataSize_offset / TARGET_CHAR_BIT,
		      (char *)&result, rt0_tc_dataSize_size / TARGET_CHAR_BIT);
  return result;
}
		      
CORE_ADDR  
tc_address_to_parent_tc_address (tc_addr)
     CORE_ADDR tc_addr;
{
  CORE_ADDR  result;
  init_m3_constants ();
  target_read_memory (tc_addr + rt0_tc_parent_offset / TARGET_CHAR_BIT,
		      (char *)&result, rt0_tc_parent_size / TARGET_CHAR_BIT);
  return result;
}
		      
CORE_ADDR 
tc_address_to_defaultMethods (tc_addr)
     CORE_ADDR tc_addr;
{
  CORE_ADDR result;
  init_m3_constants ();
  target_read_memory (tc_addr + rt0_tc_defaultMethods_offset / TARGET_CHAR_BIT,
		      (char *)&result, rt0_tc_defaultMethods_size / TARGET_CHAR_BIT);
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
 *        TYPE - a reference to a strcut type, which this routine will
 *                 fill the type of that field in.
 *
 * If SIZE, OFFSET or TYPE are NULL they won't be set.
 *
 * RETURNs 0 if NAME wasn't found in REC_TYPE
 *         1 otherwise.
 */
int 
find_m3_rec_field (rec_type, name, size, offset, type)
     struct type *rec_type;
     char *name;
     int *size, *offset;
     struct type **type;
{
  int i;
  if (rec_type == NULL) return 0;
  for (i = 0; i < TYPE_M3_REC_NFIELDS (rec_type); i++) {
    if (STREQ (TYPE_M3_REC_FIELD_NAME (rec_type, i), name)) {
      if (size)   { *size   = TYPE_M3_REC_FIELD_BITSIZE (rec_type, i); }
      if (offset) { *offset = TYPE_M3_REC_FIELD_BITPOS (rec_type, i); }
      if (type)   { *type   = TYPE_M3_REC_FIELD_TYPE (rec_type, i); }
      return 1;
    }
  }
  return 0; 
}
		      
int
find_m3_obj_field (obj_type, name, size, offset, type)
     struct type *obj_type;
     char *name;
     int *size, *offset;
     struct type **type;
{
  int i;
  if (obj_type == NULL) return 0;
  for (i = 0; i < TYPE_M3_OBJ_NFIELDS (obj_type); i++) {
    if (STREQ (TYPE_M3_OBJ_FIELD_NAME (obj_type, i), name)) {
      if (size)   { *size   = TYPE_M3_OBJ_FIELD_BITSIZE (obj_type, i); }
      if (offset) { *offset = TYPE_M3_OBJ_FIELD_BITPOS (obj_type, i); }
      if (type)   { *type   = TYPE_M3_OBJ_FIELD_TYPE (obj_type, i); }
      return 1;
    }
  }
  return 0; 
}

int
find_m3_obj_method (obj_type, name, size, offset, type)
     struct type *obj_type;
     char *name;
     int *size, *offset;
     struct type **type;
{
  int i;
  if (obj_type == NULL) return 0;
  for (i = 0; i < TYPE_M3_OBJ_NMETHODS (obj_type); i++) {
    if (STREQ (TYPE_M3_OBJ_METHOD_NAME (obj_type, i), name)) {
      if (size)   { *size   = TYPE_M3_OBJ_METHOD_BITSIZE (obj_type, i); }
      if (offset) { *offset = TYPE_M3_OBJ_METHOD_BITPOS (obj_type, i); }
      if (type)   { *type   = TYPE_M3_OBJ_METHOD_TYPE (obj_type, i); }
      return 1;
    }
  }
  return 0; 
}

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
    case TYPE_CODE_M3_CARDINAL:
    case TYPE_CODE_M3_INTEGER:
      return 1;
    default:
      return 0;
  }
}

void
m3_ordinal_bounds (type, lower, upper)
     struct type *type;
     register LONGEST *lower;
     register LONGEST *upper;
{
  enum type_code tc;

  tc = TYPE_CODE (type);
  while (tc == TYPE_CODE_M3_PACKED) {
    type = TYPE_M3_PACKED_TARGET (type);
    tc = TYPE_CODE (type);
  }

  switch (tc) {
    case TYPE_CODE_M3_SUBRANGE:
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
      error ("gdb internal error: bad Modula-3 ordinal type");
      *lower = 0;
      *upper = 0;
  }
}
