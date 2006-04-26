/* Copyright (C) 1993, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

#include <stdio.h>
#include <errno.h>
#include <setjmp.h>
#include "config.h"
#include "system.h"
#include "flags.h"
#include "tree.h"
#include "rtl.h"
#include "input.h"
#include "function.h"
#include "expr.h"
#include "m3-parse.h"
#include "m3-tree.h"
#include "toplev.h"
#include "output.h"
#include "varray.h"
#include "ggc.h"

#include "m3cg.h"

#ifdef MULTIBYTE_CHARS
#include <stdlib.h>
#include <locale.h>
#endif

#ifndef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE 32
#endif
#ifndef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE 64
#endif

#if modula3_was_fully_implemented
#ifndef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64
#endif
#else
#ifdef LONG_DOUBLE_TYPE_SIZE
#undef LONG_DOUBLE_TYPE_SIZE
#endif
#define LONG_DOUBLE_TYPE_SIZE 64
#endif

#ifndef errno
extern int errno;
#endif

#ifndef MAX
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#endif

#define STREQ(a,b) (a[0] == b[0] ? strcmp (a, b) == 0 : 0)

/* Variable arrays of trees. */
enum
{
  M3VA_VARS,
  M3VA_PROCS,
  M3VA_LABELS,
  M3VA_EXPR_STACK,
  M3VA_CALL_STACK,
  M3VA_MAX
};

static varray_type m3_global_varrays[M3VA_MAX];

#define all_vars	m3_global_varrays[M3VA_VARS]
#define all_procs	m3_global_varrays[M3VA_PROCS]
#define all_labels	m3_global_varrays[M3VA_LABELS]
#define expr_stack	m3_global_varrays[M3VA_EXPR_STACK]
#define call_stack	m3_global_varrays[M3VA_CALL_STACK]

#define STACK_PUSH(stk, x)	VARRAY_PUSH_TREE (stk, x)
#define STACK_POP(stk)		((void)VARRAY_POP (stk))
#define STACK_REF(stk, n)	((&VARRAY_TOP_TREE (stk) + 1)[n])

#define EXPR_PUSH(x)	STACK_PUSH (expr_stack, x)
#define EXPR_POP()	STACK_POP (expr_stack)
#define EXPR_REF(n)	STACK_REF (expr_stack, n)

/* The call stack has triples on it: first the argument chain, then
   the type chain, then the static chain expression. */
#define CALL_PUSH(a, t, s)		\
    do					\
      {					\
	STACK_PUSH (call_stack, a);	\
	STACK_PUSH (call_stack, t);	\
	STACK_PUSH (call_stack, s);	\
      }					\
    while (0)

#define CALL_POP()			\
    do					\
      {					\
	STACK_POP (call_stack);		\
	STACK_POP (call_stack);		\
	STACK_POP (call_stack);		\
      }					\
    while (0)

#define CALL_TOP_ARG()		STACK_REF (call_stack, -3)
#define CALL_TOP_TYPE()		STACK_REF (call_stack, -2)
#define CALL_TOP_STATIC_CHAIN()	STACK_REF (call_stack, -1)


/* Function declarations. */

static void condop (enum tree_code, tree, tree);
static void debug_field (const char *);
static void debug_field_id (long);
static void debug_field_fmt (long, ...);
static tree debug_struct (void);
static void debug_tag (char, long, ...);
static void declare_fault_proc (void);
static tree declare_temp (tree);
static void emit_fault_proc (void);
static tree fix_name (const char *, long);
static void fmt_uid (long, char*);
static void generate_fault (int);
static void init_lex (void);
static void m3_call_direct (tree, tree);
static void m3_call_indirect (tree);
static void m3_load (tree, int, tree, m3_type, tree, m3_type);
static void m3_pop_param (tree);
static void m3_start_call (void);
static void m3_store (tree, int, tree, tree);
static void m3_swap (void);
static void one_field (int, tree, tree *, tree *);
static tree proc_addr (tree, int);
static void reload_buffer (void);
static long get_byte (void);
static long get_int (void);
static int scan_boolean (void);
static tree scan_float (int*);
static tree scan_label (void);
static tree scan_mtype (m3_type *);
static tree scan_proc (void);
static char *scan_quoted_string (long *);
static char scan_sign (void);
static char *scan_string (void);
static tree scan_target_int (void);
static m3_type scan_type (void);
static tree scan_var (int);
static void setop (tree, int, int);
static void setop2 (tree, int);
static varray_type varray_extend (varray_type, size_t);

/*======================================================= OPTION HANDLING ===*/

static int option_opcodes_trace     = 0;
static int option_source_line_trace = 0;
static int option_vars_trace        = 0;
static int option_procs_trace       = 0;
static int option_exprs_trace       = 0;
static int option_misc_trace        = 0;
static int option_types_trace       = 0;

/* Decode the string P as a language-specific option.
   Return the number of strings consumed for a valid option.
   Otherwise return 0.  Should not complain if it does not
   recognise the option.  */

int
m3_decode_option (argc, argv)
     int argc;
     char **argv;
{
  const char *p = argv[0];
  int recognized = 0;

  if (argc < 1) return 0;
  if (strcmp(p, "--m3cg-trace-source-line") == 0) {
    option_source_line_trace = 1; recognized++;
  } else if (strcmp(p, "--m3cg-trace-opcodes") == 0) {
    option_opcodes_trace = 1; recognized++;
  } else if (strcmp(p, "--m3cg-trace-vars") == 0) {
    option_vars_trace = 1; recognized++;
  } else if (strcmp(p, "--m3cg-trace-procs") == 0) {
    option_procs_trace = 1; recognized++;
  } else if (strcmp(p, "--m3cg-trace-exprs") == 0) {
    option_exprs_trace = 1; recognized++;
  } else if (strcmp(p, "--m3cg-trace-misc") == 0) {
    option_misc_trace = 1; recognized++;
  } else if (strcmp(p, "--m3cg-trace-types") == 0) {
    option_types_trace = 1; recognized++;
  }
  return recognized;
}

/*=============================================================== PARSING ===*/

#define BUFFER_SIZE 0x10000

static char input_buffer[BUFFER_SIZE];
static int  input_len    = 0;
static int  input_cursor = 0;
static int  input_eof    = 0;
int  m3cg_lineno  = 0;

/* Stream for reading from the input file.  */
FILE *finput;

/*-------------------------------------------------------- buffer loading ---*/

void
m3_init_parse (const char *filename)
{
  /* Open input file.  */
  if (filename == 0 || !strcmp (filename, "-"))
    {
      finput = stdin;
      filename = "stdin";
    }
  else
    finput = fopen (filename, "r");
  if (finput == 0)
    fatal_io_error ("can't open %s", filename);
  init_lex ();

  VARRAY_TREE_INIT (all_vars, 100, "all_vars");
  VARRAY_TREE_INIT (all_procs, 100, "all_procs");
  VARRAY_TREE_INIT (all_labels, 100, "all_labels");
  VARRAY_TREE_INIT (expr_stack, 100, "expr_stack");
  VARRAY_TREE_INIT (call_stack, 100 * 2, "call_stack");
  ggc_add_tree_varray_root (m3_global_varrays, M3VA_MAX);
}

void m3_finish_parse ()
{
  if (finput != NULL)
    {
      fclose (finput);
      finput = NULL;
    }
}

static void
reload_buffer (void)
{
  input_len = fread (input_buffer, 1, BUFFER_SIZE, finput);
  input_cursor = 0;
  input_eof = (input_len <= 0);
}

void
init_lex (void)
{
  reload_buffer ();
  m3cg_lineno = 1;
}

static long
get_byte (void)
{
  if (input_cursor >= input_len) {
    reload_buffer ();
    if (input_eof) return 0;
  };
  return (long)(input_buffer[input_cursor++] & 0xff);
}


#define INTEGER(x) long x = get_int()
#define UNUSED_INTEGER(x) int x ATTRIBUTE_UNUSED = get_int()
static long
get_int (void)
{
  long i, n_bytes, sign, val, shift;

  i = (long) get_byte ();
  switch (i) {
  case M3CG_Int1:   return (long) get_byte ();
  case M3CG_NInt1:  return - (long) get_byte ();
  case M3CG_Int2:   n_bytes = 2;  sign =  1;  break;
  case M3CG_NInt2:  n_bytes = 2;  sign = -1;  break;
  case M3CG_Int4:   n_bytes = 4;  sign =  1;  break;
  case M3CG_NInt4:  n_bytes = 4;  sign = -1;  break;
  case M3CG_Int8:   n_bytes = 8;  sign =  1;  break;
  case M3CG_NInt8:  n_bytes = 8;  sign = -1;  break;
  default:          return i;
  }

  for (val = 0, shift = 0; n_bytes > 0;  n_bytes--, shift += 8) {
    val = val | (((long) get_byte ()) << shift);
  }
  return sign * val;
}

/*-------------------------------------------------------- quoted strings ---*/

#define QUOTED_STRING(x,l) long l; char *x = scan_quoted_string (&l)
#define UNUSED_QUOTED_STRING(x,l) long l; char *x ATTRIBUTE_UNUSED = scan_quoted_string (&l)
static char *
scan_quoted_string (long *length)
{
  long x, len;
  char *result;

  len = get_int ();
  if (length) *length = len;
  if (len <= 0) return 0;

  result = (char*) xmalloc (len + 1);
  for (x = 0; x < len; x++) {
    result[x] = (char) get_byte ();
  }
  result[len] = 0;
  return result;  
}

/*----------------------------------------------------------------- names ---*/

#define NAME(x) char *x = scan_string ()
#define UNUSED_NAME(x) char *x ATTRIBUTE_UNUSED = scan_string ()
static char *
scan_string (void)
{
  long len;
  return scan_quoted_string (&len);
}

/*----------------------------------------------------------------- types ---*/

#define IS_WORD_TYPE(t) (t == T_word_32 || t == T_word_8 || t == T_word_16 || \
			 t == T_word_64 || t == T_word)

#define IS_INTEGER_TYPE(t) (t == T_int_32 || t == T_int_8 || t == T_int_16 || \
			    t == T_int_64 || t == T_int)

#define IS_REAL_TYPE(t) (t == T_reel || t == T_lreel || t == T_xreel)

#define TYPE(x) m3_type x = scan_type ()
#define UNUSED_TYPE(x) m3_type x ATTRIBUTE_UNUSED = scan_type ()
static m3_type
scan_type (void)
{
  long i = get_int ();
  if ((i < 0) || (T_LAST <= i))
    fatal_error (" *** illegal type: %ld, at m3cg_lineno %d", i, m3cg_lineno);
  return (m3_type) i;
}

#define MTYPE(x) tree x = scan_mtype (0)
#define UNUSED_MTYPE(x) tree x ATTRIBUTE_UNUSED = scan_mtype (0)
#define MTYPE2(x, y) m3_type y; tree x = scan_mtype (&y)
#define UNUSED_MTYPE2(x, y) m3_type y; tree x ATTRIBUTE_UNUSED = scan_mtype (&y)

static tree
scan_mtype (m3_type *T)
{
  m3_type TT = scan_type ();
  if (T) { *T = TT; }
  return m3_build_type (TT, 0, 0);
}

/*----------------------------------------------------------------- signs ---*/

#define SIGN(x) char x = scan_sign ()
static char
scan_sign (void)
{
  long x = get_int ();
  switch (x) {
  case 0:  return 'P';  /* positive */
  case 1:  return 'N';  /* negative */
  case 2:  return 'U';  /* unknown */
  default: 
    fatal_error(" *** bad sign: %ld, at m3cg_lineno %d", x, m3cg_lineno);
  };
  return '0';
}

/*-------------------------------------------------------------- integers ---*/

#define TARGET_INTEGER(x) tree x = scan_target_int ()

static tree
scan_target_int (void)
{
  HOST_WIDE_INT low, hi;
  long i, n_bytes, sign, shift;
  tree res;

  i = (long) get_byte ();
  switch (i) {
  case M3CG_Int1:   return m3_build_int (get_byte ());
  case M3CG_NInt1:  return m3_build_int (-get_byte ());
  case M3CG_Int2:   n_bytes = 2;  sign =  1;  break;
  case M3CG_NInt2:  n_bytes = 2;  sign = -1;  break;
  case M3CG_Int4:   n_bytes = 4;  sign =  1;  break;
  case M3CG_NInt4:  n_bytes = 4;  sign = -1;  break;
  case M3CG_Int8:   n_bytes = 8;  sign =  1;  break;
  case M3CG_NInt8:  n_bytes = 8;  sign = -1;  break;
  default:          return m3_build_int (i);
  }

  hi = low = 0;
  for (shift = 0; n_bytes > 0;  n_bytes--, shift += 8) {
    if (shift < HOST_BITS_PER_WIDE_INT) {
      low = low | (((long) get_byte ()) << shift);
    } else {
      hi = hi | (((long) get_byte ()) << shift);
    }
  };

  res = build_int_2 (low, hi);
  if (sign < 0) { res = m3_build1 (NEGATE_EXPR, t_int, res); }
  return res;
}


#define LEVEL(x)     INTEGER(x)
#define UNUSED_LEVEL(x)     UNUSED_INTEGER(x)
#define BITSIZE(x)   INTEGER(x)
#define UNUSED_BITSIZE(x)   UNUSED_INTEGER(x)
#define BYTESIZE(x)  long x = BITS_PER_UNIT * get_int()
#define UNUSED_BYTESIZE(x)  long x ATTRIBUTE_UNUSED = 8 * get_int()
#define ALIGNMENT(x) long x = BITS_PER_UNIT * get_int()
#define FREQUENCY(x) INTEGER(x)
#define UNUSED_FREQUENCY(x) UNUSED_INTEGER(x)
#define BIAS(x)      INTEGER(x)
#define BITOFFSET(x) INTEGER(x)
#define BYTEOFFSET(x) long x = BITS_PER_UNIT * get_int()

/*------------------------------------------------------------- type uids ---*/
/* Modula-3 type uids are unsiged 32-bit values.  They are passed as signed
   decimal integers in the intermediate code, but converted to 6-byte, base 62
   strings of characters from here to the debugger.  To avoid surprises downstream,
   these generated strings are legal C identifiers.  */

#define UID_SIZE 6

#define NO_UID -1

#define TYPEID(x)    long x = get_int ()
#define UNUSED_TYPEID(x)    long x ATTRIBUTE_UNUSED = get_int ()

static void
fmt_uid (long x, char *buf)
{
  unsigned digit;
  int i;

  buf[UID_SIZE] = 0;
  if (x == NO_UID) { strcpy (buf, "zzzzzz");  return; }

  for (i = UID_SIZE-1; i >= 0; i--) {
    digit = ((unsigned)x) % 62;
    x = ((unsigned)x) / 62;
    if      (digit < 26) { buf[i] = 'A' + digit; }
    else if (digit < 52) { buf[i] = 'a' + (digit - 26); }
    else                 { buf[i] = '0' + (digit - 52); }
  }

  if ((x != 0) || (buf[0] < 'A') || ('Z' < buf[0])) {
    fatal_error (" *** bad uid -> identifier conversion!!"); }
}

/*----------------------------------------------------------------- float ---*/

#define FLOAT(x,fkind)  int fkind;  tree x = scan_float(&fkind)

#define REEL_BYTES  (FLOAT_TYPE_SIZE / 8)
#define LREEL_BYTES (DOUBLE_TYPE_SIZE / 8)
#define XREEL_BYTES (LONG_DOUBLE_TYPE_SIZE / 8)

static tree
scan_float (int *fkind)
{
  long i = get_int ();
  long n_bytes;
  struct { double xx_align;  char z[XREEL_BYTES]; } data;
  tree tipe;
  REAL_VALUE_TYPE val;

  *fkind = i;
  switch (i) {
  case 0:  tipe = t_reel;  n_bytes = REEL_BYTES;  break;
  case 1:  tipe = t_lreel; n_bytes = LREEL_BYTES; break;
  case 2:  tipe = t_xreel; n_bytes = XREEL_BYTES; break;
  default:
    fatal_error(" *** invalid floating point value, precision = %ld, at m3cg_lineno %d",
                i, m3cg_lineno);
  }

  /* read the value's bytes */
  for (i = 0;  i < n_bytes;  i++)  { data.z[i] = get_int (); }

  /* finally, assemble a floating point value */
  if (tipe == t_reel) {
    val = REAL_VALUE_FROM_TARGET_SINGLE (*(int*)(&data.z[0]));
  } else {
    val = REAL_VALUE_FROM_TARGET_DOUBLE ((HOST_WIDE_INT*)&data.z[0]);
  }
  return build_real (tipe, val);
}

/*-------------------------------------------------------------- booleans ---*/

#define BOOLEAN(x) int x = scan_boolean()
#define UNUSED_BOOLEAN(x) int x ATTRIBUTE_UNUSED = scan_boolean()

static int
scan_boolean (void)
{
  return (get_int () != 0);
}

/*------------------------------------------------------------- variables ---*/

#define VAR(x) tree x = scan_var (ERROR_MARK)
#define UNUSED_VAR(x) tree x ATTRIBUTE_UNUSED = scan_var (ERROR_MARK)
#define RETURN_VAR(x, code) tree x = scan_var (code)

#define VARRAY_EXTEND(va, n) ((va) = varray_extend (va, n))
static varray_type
varray_extend (varray_type va, size_t n)
{
  size_t num_elements;

  if (n <= VARRAY_ACTIVE_SIZE(va))
    return va;
  num_elements = VARRAY_SIZE (va);
  if (n > num_elements)
    {
      do
	num_elements *= 2;
      while (n > num_elements);
      VARRAY_GROW (va, num_elements);
    }
  VARRAY_ACTIVE_SIZE(va) = n;
  return va;
}

static tree
scan_var (int code)
{
  int i = get_int();

  VARRAY_EXTEND (all_vars, i + 1);
  if (code == ERROR_MARK)
    {
      if (VARRAY_TREE (all_vars, i) == NULL)
	{
	  fatal_error ("*** variable should already exist, v.%d, line %d",
		       i, m3cg_lineno);
	}
    }
  else
    {
      if (VARRAY_TREE (all_vars, i) != NULL)
	{
	  fatal_error ("*** variable should not already exist, v.%d, line %d",
		       i, m3cg_lineno);
	}
      VARRAY_TREE (all_vars, i) = make_node (code);
      DECL_NAME (VARRAY_TREE (all_vars, i)) = NULL_TREE;
    }

  return VARRAY_TREE (all_vars, i);
}

/*------------------------------------------------------------ procedures ---*/

#define PROC(x) tree x = scan_proc ()
#define UNUSED_PROC(x) tree x ATTRIBUTE_UNUSED = scan_proc ()

static tree
scan_proc (void)
{
  int i = get_int ();

  if (i <= 0) { return 0; }
  VARRAY_EXTEND (all_procs, i + 1);
  if (VARRAY_TREE (all_procs, i) == NULL)
    VARRAY_TREE (all_procs, i) = make_node (FUNCTION_DECL);
  return VARRAY_TREE (all_procs, i);
}


/*---------------------------------------------------------------- labels ---*/

#define LABEL(l) tree  l = scan_label()

static tree
scan_label (void)
{
  int i = get_int ();

  if (i < 0) { return 0; }
  VARRAY_EXTEND (all_labels, i + 1);
  if (VARRAY_TREE (all_labels, i) == NULL)
    VARRAY_TREE (all_labels, i) = build_decl (LABEL_DECL, NULL_TREE, t_addr);
  return VARRAY_TREE (all_labels, i);
}


/*================================================= debugging information ===*/

static char current_dbg_type_tag [100];
static int current_dbg_type_count1;
static int current_dbg_type_count2;
static int current_dbg_type_count3;

static void
debug_tag (char kind, long id, ...)
{
  va_list args;
  char *fmt;

  va_start (args, id);

  current_dbg_type_tag [0] = 'M';
  current_dbg_type_tag [1] = kind;
  current_dbg_type_tag [2] = '_';
  fmt_uid (id, current_dbg_type_tag + 3);

  fmt = va_arg (args, char *);
  vsnprintf (current_dbg_type_tag + UID_SIZE + 3,
	     sizeof(current_dbg_type_tag) - (UID_SIZE + 3), fmt, args);
  va_end (args);
}

static void
debug_field (const char *name)
{
  tree f = build_decl (FIELD_DECL, get_identifier (name), t_int);

  DECL_FIELD_OFFSET (f) = size_zero_node;
  DECL_FIELD_BIT_OFFSET (f) = bitsize_zero_node;
  /* XXX DECL_BIT_FIELD_TYPE ? */

  layout_decl (f, 1);

  TREE_CHAIN (f) = debug_fields;
  debug_fields = f;
}

static void
debug_field_id (long id)
{
  char buf [UID_SIZE+1];
  fmt_uid (id, buf);
  debug_field (buf);
}

static void
debug_field_fmt (long id, ...)
{
  va_list args;
  char name [100];
  char *fmt;

  va_start (args, id);

  fmt_uid (id, name);
  fmt = va_arg (args, char *);
  vsnprintf (name + UID_SIZE, sizeof(name) - UID_SIZE, fmt, args);
  va_end (args);

  debug_field (name);
}

static tree
debug_struct (void)
{
  tree t = make_node (RECORD_TYPE);
  TYPE_NAME (t) =
    build_decl (TYPE_DECL, get_identifier (current_dbg_type_tag), t);
  TYPE_FIELDS (t) = nreverse (debug_fields);
  debug_fields = 0;
  TYPE_SIZE (t) = bitsize_one_node;
  TYPE_SIZE_UNIT (t) = convert (sizetype,
                                size_binop (FLOOR_DIV_EXPR,
				            TYPE_SIZE (t),
				            bitsize_int (BITS_PER_UNIT)));
  TYPE_ALIGN (t) = BITS_PER_UNIT;
  TYPE_MODE (t) = QImode;

  rest_of_decl_compilation (build_decl (TYPE_DECL, NULL_TREE, t), 0, 1, 0);
  return t;
}

/*========================================== GLOBALS FOR THE M3CG MACHINE ===*/

static const char *current_unit_name;

/* the exported interfaces */
static int exported_interfaces;
static char *exported_interfaces_names [100];

/*================================= SUPPORT FOR INITIALIZED DATA CREATION ===*/

static int current_record_offset;

static void one_gap (int offset);

static void
one_field (int offset, tree tipe, tree *f, tree *v)
{
  if (option_vars_trace)
    {
      fprintf (stderr, "  one_field: offset %d\n", offset);
    }
  if (offset > current_record_offset)
    {
      one_gap (offset);
    }

  *f = build_decl (FIELD_DECL, 0, tipe);
  layout_decl (*f, 1);
  DECL_FIELD_OFFSET (*f) = size_int (offset / BITS_PER_UNIT);
  DECL_FIELD_BIT_OFFSET (*f) = bitsize_int (offset % BITS_PER_UNIT);
  /* XXX DECL_BIT_FIELD_TYPE ? */
  DECL_CONTEXT (*f) = current_record_type;
  TREE_CHAIN (*f) = TYPE_FIELDS (current_record_type);
  TYPE_FIELDS (current_record_type) = *f;

  *v = current_record_vals = tree_cons (*f, NULL_TREE, current_record_vals);
  current_record_offset = offset + TREE_INT_CST_LOW (TYPE_SIZE (tipe));
}

static void
one_gap (int offset)
{
  tree f, v, tipe;
  int gap;

  gap = offset - current_record_offset;
  if (option_vars_trace)
    {
      fprintf (stderr, "  one_gap: offset %d, gap %d\n", offset, gap);
    }
  tipe = make_node (LANG_TYPE);
  TYPE_SIZE (tipe) = bitsize_int (gap);
  TYPE_SIZE_UNIT (tipe) = size_int (gap / BITS_PER_UNIT);
  TYPE_ALIGN (tipe) = BITS_PER_UNIT;
  one_field (current_record_offset, tipe, &f, &v);
  TREE_VALUE (v) = make_node (CONSTRUCTOR);
  CONSTRUCTOR_ELTS (TREE_VALUE (v)) = 0;
  TREE_TYPE (TREE_VALUE (v)) = TREE_TYPE (f);
}

/*========================================= SUPPORT FUNCTIONS FOR YYPARSE ===*/

static tree
fix_name (const char *name, long id) /* GCC32OK */
{
  char buf[100];

  if (name == 0 || name[0] == '*') {
    static int anonymous_counter = 1;
    snprintf (buf, sizeof(buf), "L_%d", anonymous_counter++);
  } else if (id == 0) {
    return get_identifier (name);
  } else if (id == NO_UID) {
    snprintf (buf, sizeof(buf), "M%s", name);
  } else {
    buf[0] = 'M';  buf[1] = '3';  buf[2] = '_';
    fmt_uid (id, buf + 3);
    buf[3 + UID_SIZE] = '_';
    strcpy (buf + 4 + UID_SIZE, name);
  }
  return get_identifier (buf);
}

static tree
declare_temp (tree t) /* GCC32OK */
{      
  tree v = build_decl (VAR_DECL, 0, t);

  TREE_UNSIGNED (v) = TREE_UNSIGNED (t);
  DECL_CONTEXT (v) = current_function_decl;

  TREE_CHAIN (v) = BLOCK_VARS (BLOCK_SUBBLOCKS
                               (DECL_INITIAL (current_function_decl)));
  BLOCK_VARS (BLOCK_SUBBLOCKS (DECL_INITIAL (current_function_decl))) = v;

  expand_decl (v);
  rest_of_decl_compilation (v, 0, 0, 1);
  return v;
}


 /* Return a tree representing the address of the given procedure.  If
    NO_TRAMP is true, the static address is used rather than the trampoline
    address for a nested procedure.  */
 
static tree
proc_addr (tree proc, int no_tramp)
{
  tree expr = m3_build1 (ADDR_EXPR,
                         build_pointer_type (TREE_TYPE (proc)),
                         proc);
  if (no_tramp)
    TREE_STATIC (expr) = 1;
  return expr;
}

static void
m3_start_call (void) /* GCC32OK */
{
  CALL_PUSH (NULL_TREE, NULL_TREE, NULL_TREE);
}

static void
m3_pop_param (tree t) /* GCC32OK */
{
  CALL_TOP_ARG ()
    = chainon (CALL_TOP_ARG (),
	       build_tree_list (NULL_TREE, EXPR_REF (-1)));
  CALL_TOP_TYPE ()
    = chainon (CALL_TOP_TYPE (),
	       build_tree_list (NULL_TREE, t));
  EXPR_POP ();
}

static void
m3_call_direct (tree p, tree return_type) /* GCC32OK */
{
  tree call;

  if (return_type == NULL_TREE) {
    return_type = TREE_TYPE (TREE_TYPE (p));
  }

  call = build (CALL_EXPR, return_type,
		proc_addr (p, 1),
		CALL_TOP_ARG (),
		CALL_TOP_STATIC_CHAIN ());
  TREE_SIDE_EFFECTS (call) = 1;
  if (return_type == t_void) {
    expand_expr_stmt (call);
  } else {
    EXPR_PUSH (call);
  }
  CALL_POP ();
}

static void
m3_call_indirect (tree t) /* GCC32OK */
{
  tree argtypes = chainon (CALL_TOP_TYPE (),
			   tree_cons (NULL_TREE, t_void, NULL_TREE));
  tree fntype = build_pointer_type (build_function_type (t, argtypes));
  tree call;
  tree fnaddr = EXPR_REF (-1);
  EXPR_POP ();

  call = build (CALL_EXPR, t,
                m3_cast (fntype, fnaddr),
		CALL_TOP_ARG (),
		CALL_TOP_STATIC_CHAIN ());
  if (t == t_void) {
    TREE_SIDE_EFFECTS (call) = 1;
    expand_expr_stmt (call);
  } else {
    EXPR_PUSH (call);
  }
  CALL_POP ();
}

static void
m3_swap (void) /* GCC32OK */
{
  tree tmp = EXPR_REF (-1);
  EXPR_REF (-1) = EXPR_REF (-2);
  EXPR_REF (-2) = tmp;
}

static void
m3_load (tree v, int o,
	 tree src_t, m3_type src_T,
	 tree dst_t, m3_type dst_T) /* GCC32OK */
{
  if (o == 0 && TREE_TYPE (v) == src_t) {
    EXPR_PUSH (v);
  } else {
    EXPR_PUSH (m3_build3 (BIT_FIELD_REF, src_t, v, TYPE_SIZE (src_t),
			  bitsize_int (o)));
  }
  if (src_T != dst_T) {
    EXPR_REF (-1) = m3_build1 (CONVERT_EXPR, dst_t, EXPR_REF (-1));
  }
  if (option_vars_trace) {
    const char *name = "noname";
    if (v != 0 && DECL_NAME(v) != 0) {
      name = IDENTIFIER_POINTER(DECL_NAME(v));
    }
    fprintf(stderr, "  m3_load (%s): offset %d, convert %d -> %d\n", name,
	    o, src_T, dst_T);
  }
}

static void
m3_store (tree v, int o, tree src_t, tree dst_t)  /* GCC32OK */
{
  tree lhs, rhs;

  if (TREE_TYPE (EXPR_REF (-1)) == src_t) {
    rhs = EXPR_REF (-1);
  } else {
    rhs = m3_cast (src_t, EXPR_REF (-1));
  }
  if (o == 0 && TREE_TYPE (v) == dst_t) {
    lhs = v;
  } else {
    lhs = m3_build3 (BIT_FIELD_REF, dst_t, v, TYPE_SIZE (dst_t),
		     bitsize_int (o));
  }
  if (src_t != dst_t) {
    rhs = m3_build1 (CONVERT_EXPR, dst_t, rhs);
  }
  expand_assignment (lhs, rhs, 0, 0);
  EXPR_POP ();
}

#define binaryop(o,t) \
  do { \
    EXPR_REF (-2) = m3_build2 (o, t, EXPR_REF (-2), EXPR_REF (-1));  EXPR_POP (); \
    } while (0)

#define unaryop(o,t) \
  do { \
    EXPR_REF (-1) = m3_build1 (o, t, EXPR_REF (-1)); \
    } while (0)

static void
condop (enum tree_code o, tree l, tree t) /* GCC32OK */
{
  tree t1 = m3_cast (t, EXPR_REF (-1));
  tree t2 = m3_cast (t, EXPR_REF (-2));
  TREE_UNSIGNED (t1) = TREE_UNSIGNED (t);
  TREE_UNSIGNED (t2) = TREE_UNSIGNED (t);

  do_jump (m3_build2 (o, t_int, t2, t1),
	   NULL_RTX, label_rtx (l));
  EXPR_POP ();
  EXPR_POP ();
}

static void
setop (tree p, int n, int q) /* GCC32OK */
{
  m3_start_call ();
  EXPR_PUSH (size_int (n));
  m3_pop_param (t_int);
  while (q--) {
    m3_pop_param (t_addr);
  }
  m3_call_direct (p, NULL_TREE);
}

static void
setop2 (tree p, int q) /* GCC32OK */
{
  m3_start_call ();
  while (q--) {
    m3_pop_param (t_addr);
  }
  m3_call_direct (p, NULL_TREE);
}

/*---------------------------------------------------------------- faults ---*/

static int  fault_offs;                /*   + offset                */

static void
declare_fault_proc (void)
{
  tree proc, parm;
  tree parm_block = make_node (BLOCK);
  tree top_block  = make_node (BLOCK);

  proc = build_decl (FUNCTION_DECL, get_identifier ("_m3_fault"),
		     build_function_type (t_void,
					  tree_cons (0, t_word,
						     tree_cons (0, t_void, 0))));
  DECL_RESULT (proc) = build_decl (RESULT_DECL, NULL_TREE, t_void);
  DECL_CONTEXT (DECL_RESULT (proc)) = proc;
  TREE_STATIC (proc) = 1;
  TREE_PUBLIC (proc) = 0;
  DECL_CONTEXT (proc) = 0;

  parm = build_decl (PARM_DECL, fix_name ("arg", 0x195c2a74), t_word);
  if (DECL_MODE (parm) == VOIDmode) DECL_MODE (parm) = Pmode;
  DECL_ARG_TYPE (parm) = t_word;
  DECL_ARGUMENTS (proc) = parm;
  DECL_CONTEXT (parm) = proc;

  BLOCK_SUPERCONTEXT (parm_block) = proc;
  DECL_INITIAL (proc) = parm_block;

  BLOCK_SUPERCONTEXT (top_block) = parm_block;
  BLOCK_SUBBLOCKS (parm_block) = top_block;

  /*  make_decl_rtl (proc, NULL); */

  rest_of_decl_compilation (parm, 0, 0, 1);

  fault_proc = proc;
}

static void
emit_fault_proc (void)
{
  DECL_SOURCE_FILE (fault_proc) = "<internal>";
  DECL_SOURCE_LINE (fault_proc) = 0;

  if (current_block) push_function_context ();

  current_function_decl = fault_proc;

  make_decl_rtl (fault_proc, NULL);

  init_function_start (fault_proc, "<internal>", 0);
  expand_function_start (fault_proc, 0);

  clear_last_expr ();
  pending_blocks = tree_cons (NULL_TREE, current_block, pending_blocks);
  current_block = DECL_INITIAL (fault_proc); /* parm_block */
  TREE_USED (current_block) = 1;
  current_block = BLOCK_SUBBLOCKS (current_block); /* top_block */
  TREE_USED (current_block) = 1;
  expand_start_bindings_and_block (0, current_block);

  /* compile the locals we have already seen */
  { tree local;
    for (local = BLOCK_VARS (current_block);
	 local; local = TREE_CHAIN (local)) {
      expand_decl (local);
      rest_of_decl_compilation (local, 0, 0, 1);
    }
  }

  m3_start_call ();
  EXPR_PUSH (m3_build1 (ADDR_EXPR, t_addr, current_segment));
  m3_pop_param (t_addr);
  EXPR_PUSH (DECL_ARGUMENTS (fault_proc));
  m3_pop_param (t_word);
  if (fault_handler != NULL_TREE) {
    m3_call_direct (fault_handler, t_void);
  } else {
    m3_load (fault_intf, fault_offs, t_addr, T_addr, t_addr, T_addr);
    m3_call_indirect (t_void);
  }
  emit_barrier ();
  expand_null_return ();

  if (current_block != BLOCK_SUBBLOCKS (DECL_INITIAL (fault_proc))) abort ();
  expand_end_bindings (BLOCK_VARS (current_block), 1, 0);
  expand_function_end ("<internal>", 0, 0);
  rest_of_compilation (fault_proc);

  current_block = TREE_VALUE (pending_blocks);
  pending_blocks = TREE_CHAIN (pending_blocks);

  if (current_block) pop_function_context ();
}

// FIXME: jdp says 0x0f and 4; cm3 may need more
#define FAULT_MASK 0x1f
#define LINE_SHIFT 5

static void
generate_fault (int code)
{
  if (fault_proc == 0) declare_fault_proc ();
  m3_start_call ();
  EXPR_PUSH (m3_build_int ((lineno << LINE_SHIFT) + (code & FAULT_MASK)));
  m3_pop_param (t_word);
  m3_call_direct (fault_proc, t_void);
  emit_barrier ();
}

/*================================================== ENTRY POINTS FOR GCC ===*/

/* One would expect that flag_traditional is only for the benefit of
   the C front-end, but dwarfout.c references it. */

int flag_traditional;

/* If DECL has a cleanup, build and return that cleanup here.
   This is a callback called by expand_expr.  */

/*ARGSUSED*/
tree
maybe_build_cleanup (decl)
     tree decl ATTRIBUTE_UNUSED;
{
  /* There are no cleanups in Modula-3.  */
  return NULL_TREE;
}

/*ARGSUSED*/
void incomplete_type_error (value, typ)
     tree value;
     tree typ;
{
  if (TREE_CODE (typ) == ERROR_MARK)
    return;
  fputs("type:\n", stderr);
  debug_tree(typ);
  fputs("value:\n", stderr);
  debug_tree(value);
  fatal_error 
    (" *** language-dependent function called: incomplete_type_error");
}

/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   Value is 1 if successful.  */

int mark_addressable (exp)
     tree exp;
{
  tree x = exp;
  while (1) {
    switch (TREE_CODE (x))
      {
      case ADDR_EXPR:
      case COMPONENT_REF:
      case ARRAY_REF:
	x = TREE_OPERAND (x, 0);
	break;

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (x) = 1;
	return 1;

      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	put_var_into_stack (x);

      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;

      default:
	return 1; }}
}

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside teh BIND_EXPR.  */
/*ARGSUSED*/
void
insert_block (block tree block ATTRIBUTE_UNUSED);
{
  TREE_USED (block) = 1;
  BLOCK_SUBBLOCKS (current_block)
    = chainon (BLOCK_SUBBLOCKS (current_block), block);
}


/* Nonzero if we are currently in the global binding level.  */
int global_bindings_p ()
{
  return current_block == 0;
}

/*ARGSUSED*/
void copy_lang_decl (x)
     tree x ATTRIBUTE_UNUSED;
{
}

/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */
tree type_for_size (bits, unsignedp)
     unsigned bits;
     int unsignedp;
{
  if (unsignedp) {
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_word_8))) {
      return t_word_8; }
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_word_16))) {
      return t_word_16; }
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_word_32))) {
      return t_word_32; }
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_word_64))) {
      return t_word_64; }
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_word))) {
      return t_word; }
  } else {				      
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_int_8))) {
      return t_int_8; }
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_int_16))) {
      return t_int_16; }
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_int_32))) {
      return t_int_32; }
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_int_64))) {
      return t_int_64; }
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_int))) {
      return t_int; }
  }

  fatal_error (" *** type_for_size, called for %d bits, unsignedp = %d", 
	 bits, unsignedp);
  return NULL_TREE;
}

/* Return a type the same as TYPE except unsigned or
   signed according to UNSIGNEDP.  */

tree signed_or_unsigned_type (unsignedp, typ)
     int unsignedp;
     tree typ;
{
  if (! INTEGRAL_TYPE_P (typ))
    return typ;
  if (TYPE_PRECISION (typ) == TYPE_PRECISION (t_int_8))
    return unsignedp ? t_word_8 : t_int_8;
  if (TYPE_PRECISION (typ) == TYPE_PRECISION (t_int_16))
    return unsignedp ? t_word_16 : t_int_16;
  if (TYPE_PRECISION (typ) == TYPE_PRECISION (t_int_32))
    return unsignedp ? t_word_32 : t_int_32;
  if (TYPE_PRECISION (typ) == TYPE_PRECISION (t_int_64))
    return unsignedp ? t_word_64 : t_int_64;
  if (TYPE_PRECISION (typ) == TYPE_PRECISION (t_int))
    return unsignedp ? t_word : t_int;
  return typ;
}

/* Return a data type that has machine mode MODE.
   If the mode is an integer,
   then UNSIGNEDP selects between signed and unsigned types.  */
tree type_for_mode (mode, unsignedp)
     enum machine_mode mode;
     int unsignedp;
{
  if (mode == TYPE_MODE (t_int_64)) return unsignedp ? t_word_64 : t_int_64;
  if (mode == TYPE_MODE (t_int_8))  return unsignedp ? t_word_8  : t_int_8;
  if (mode == TYPE_MODE (t_int_16)) return unsignedp ? t_word_16 : t_int_16;
  if (mode == TYPE_MODE (t_int_32)) return unsignedp ? t_word_32 : t_int_32;
  if (mode == TYPE_MODE (t_int))    return unsignedp ? t_word    : t_int;
  if (mode == TYPE_MODE (t_reel))   return t_reel;
  if (mode == TYPE_MODE (t_lreel))  return t_lreel;
  if (mode == TYPE_MODE (t_xreel))  return t_xreel;
  return 0;
}

/* Return an unsigned type the same as TYPE in other respects.  */
tree unsigned_type (typ)
     tree typ;
{
  if (TREE_UNSIGNED (typ)) { return typ; }
  if (typ == t_int)    return t_word;
  if (typ == t_int_64) return t_word_64;
  if (typ == t_int_32) return t_word_32;
  if (typ == t_int_16) return t_word_16;
  if (typ == t_int_8)  return t_word_8;
  fatal_error (" *** language-dependent function called: unsigned_type");
  /*NOTREACHED*/
}

/* Return a signed type the same as TYPE in other respects.  */
tree signed_type (typ)
     tree typ;
{
  if (!TREE_UNSIGNED (typ)) { return typ; }
  if (typ == t_word)    return t_int;
  if (typ == t_word_64) return t_int_64;
  if (typ == t_word_32) return t_int_32;
  if (typ == t_word_16) return t_int_16;
  if (typ == t_word_8)  return t_int_8;
  fatal_error (" *** language-dependent function called: signed_type");
  /*NOTREACHED*/
}

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */
/*ARGSUSED*/
void set_block (block)
     tree block ATTRIBUTE_UNUSED;
{
  fatal_error (" *** language-dependent function called: set_block");
}

/* Enter a new binding level.
   If TAG_TRANSPARENT is nonzero, do so only for the name space of variables,
   not for that of tags.  */
/*ARGSUSED*/
void pushlevel (tag_transparent)
     int tag_transparent ATTRIBUTE_UNUSED;
{
  fatal_error (" *** language-dependent function called: pushlevel");
}

/* Exit a binding level.
   Pop the level off, and restore the state of the identifier-decl mappings
   that were in effect when this level was entered.

   If KEEP is nonzero, this level had explicit declarations, so
   and create a "block" (a BLOCK node) for the level
   to record its declarations and subblocks for symbol table output.

   If FUNCTIONBODY is nonzero, this level is the body of a function,
   so create a block as if KEEP were set and also clear out all
   label names.

   If REVERSE is nonzero, reverse the order of decls before putting
   them into the BLOCK.  */

/*ARGSUSED*/
tree poplevel (keep, reverse, functionbody)
     int keep ATTRIBUTE_UNUSED;
     int reverse ATTRIBUTE_UNUSED;
     int functionbody ATTRIBUTE_UNUSED;
{
  fatal_error (" *** language-dependent function called: poplevel");
  /*NOTREACHED*/
}

/* Record a decl-node X as belonging to the current lexical scope.
   Check for errors (such as an incompatible declaration for the same
   name already seen in the same scope).

   Returns either X or an old decl for the same name.
   If an old decl is returned, it may have been smashed
   to agree with what X says.  */
/*ARGSUSED*/
tree pushdecl (x)
     tree x ATTRIBUTE_UNUSED;
{
  fatal_error (" *** language-dependent function called: pushdecl");
  /*NOTREACHED*/
}

tree getdecls ()
{
  return current_block ? BLOCK_VARS (current_block) : global_vars;
}

/*------------------------------------- stolen and hacked from c-common.c ---*/

/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or validate its data type for an `if' or `while' statement or ?..: exp.

   This preparation consists of taking the ordinary
   representation of an expression expr and producing a valid tree
   boolean expression describing whether expr is nonzero.  We could
   simply always do build_binary_op (NE_EXPR, expr, integer_zero_node, 1),
   but we optimize comparisons, &&, ||, and !.

   The resulting type should always be `integer_type_node'.  */

tree
truthvalue_conversion (tree expr)
{
  return (TREE_CODE (expr) == ERROR_MARK) ? expr
    : m3_build2 (NE_EXPR, t_int, expr, integer_zero_node);
}

/*-------------------------------------------------- M3CG opcode handlers ---*/

static void m3cg_abort (void);
static void m3cg_abs (void);
static void m3cg_add (void);
static void m3cg_add_offset (void);
static void m3cg_and (void);
static void m3cg_begin_block (void);
static void m3cg_begin_init (void);
static void m3cg_begin_procedure (void);
static void m3cg_begin_unit (void);
static void m3cg_bind_segment (void);
static void m3cg_call_direct (void);
static void m3cg_call_indirect (void);
static void m3cg_case_jump (void);
static void m3cg_ceiling (void);
static void m3cg_check_eq (void);
static void m3cg_check_hi (void);
static void m3cg_check_index (void);
static void m3cg_check_lo (void);
static void m3cg_check_nil (void);
static void m3cg_check_range (void);
static void m3cg_chop (void);
static void m3cg_comment (void);
static void m3cg_compare (enum tree_code);
static void m3cg_copy (void);
static void m3cg_copy_n (void);
static void m3cg_cvt_float (void);
static void m3cg_declare_array (void);
static void m3cg_declare_constant (void);
static void m3cg_declare_enum (void);
static void m3cg_declare_enum_elt (void);
static void m3cg_declare_exception (void);
static void m3cg_declare_field (void);
static void m3cg_declare_formal (void);
static void m3cg_declare_global (void);
static void m3cg_declare_indirect (void);
static void m3cg_declare_local (void);
static void m3cg_declare_method (void);
static void m3cg_declare_object (void);
static void m3cg_declare_opaque (void);
static void m3cg_declare_open_array (void);
static void m3cg_declare_packed (void);
static void m3cg_declare_param (void);
static void m3cg_declare_pointer (void);
static void m3cg_declare_procedure (void);
static void m3cg_declare_proctype (void);
static void m3cg_declare_raises (void);
static void m3cg_declare_record (void);
static void m3cg_declare_segment (void);
static void m3cg_declare_set (void);
static void m3cg_declare_subrange (void);
static void m3cg_declare_temp (void);
static void m3cg_declare_typename (void);
static void m3cg_div (void);
static void m3cg_divide (void);
static void m3cg_end_block (void);
static void m3cg_end_init (void);
static void m3cg_end_procedure (void);
static void m3cg_end_unit (void);
static void m3cg_eq (void);
static void m3cg_exit_proc (void);
static void m3cg_export_unit (void);
static void m3cg_extract_mn (void);
static void m3cg_extract_n (void);
static void m3cg_floor (void);
static void m3cg_free_temp (void);
static void m3cg_ge (void);
static void m3cg_gt (void);
static void m3cg_if_compare (enum tree_code);
static void m3cg_if_eq (void);
static void m3cg_if_false (void);
static void m3cg_if_ge (void);
static void m3cg_if_gt (void);
static void m3cg_if_le (void);
static void m3cg_if_lt (void);
static void m3cg_if_ne (void);
static void m3cg_if_true (void);
static void m3cg_import_global (void);
static void m3cg_import_procedure (void);
static void m3cg_import_unit (void);
static void m3cg_index_address (void);
static void m3cg_init_chars (void);
static void m3cg_init_float (void);
static void m3cg_init_int (void);
static void m3cg_init_label (void);
static void m3cg_init_offset (void);
static void m3cg_init_proc (void);
static void m3cg_init_var (void);
static void m3cg_insert_mn (void);
static void m3cg_insert_n (void);
static void m3cg_le (void);
static void m3cg_load (void);
static void m3cg_load_address (void);
static void m3cg_load_float (void);
static void m3cg_load_indirect (void);
static void m3cg_load_integer (void);
static void m3cg_load_nil (void);
static void m3cg_load_procedure (void);
static void m3cg_load_static_link (void);
static void m3cg_loophole (void);
static void m3cg_lt (void);
static void m3cg_m3_extract (void);
static void m3cg_m3_insert (void);
static void m3cg_m3_jump (void);
static void m3cg_m3_rotate (void);
static void m3cg_m3_shift (void);
static void m3cg_max (void);
static void m3cg_min (void);
static void m3cg_mod (void);
static void m3cg_multiply (void);
static void m3cg_ne (void);
static void m3cg_negate (void);
static void m3cg_not (void);
static void m3cg_note_procedure_origin (void);
static void m3cg_or (void);
static void m3cg_pop (void);
static void m3cg_pop_param (void);
static void m3cg_pop_static_link (void);
static void m3cg_pop_struct (void);
static void m3cg_reveal_opaque (void);
static void m3cg_rotate_left (void);
static void m3cg_rotate_right (void);
static void m3cg_round (void);
static void m3cg_set_compare (tree);
static void m3cg_set_difference (void);
static void m3cg_set_eq (void);
static void m3cg_set_ge (void);
static void m3cg_set_gt (void);
static void m3cg_set_intersection (void);
static void m3cg_set_label (void);
static void m3cg_set_le (void);
static void m3cg_set_lt (void);
static void m3cg_set_member (void);
static void m3cg_set_ne (void);
static void m3cg_set_range (void);
static void m3cg_set_runtime_hook (void);
static void m3cg_set_runtime_proc (void);
static void m3cg_set_singleton (void);
static void m3cg_set_source_file (void);
static void m3cg_set_source_line (void);
static void m3cg_set_sym_difference (void);
static void m3cg_set_union (void);
static void m3cg_shift_left (void);
static void m3cg_shift_right (void);
static void m3cg_start_call_direct (void);
static void m3cg_start_call_indirect (void);
static void m3cg_store (void);
static void m3cg_store_indirect (void);
static void m3cg_subtract (void);
static void m3cg_swap (void);
static void m3cg_trunc (void);
static void m3cg_widen (void);
static void m3cg_xor (void);
static void m3cg_zero (void);
static void m3cg_zero_n (void);

static void
m3cg_begin_unit (void) /* GCC32OK */
{
  UNUSED_INTEGER (n);
  exported_interfaces = 0;
  m3_declare_runtime_functions ();
}

static void
m3cg_end_unit (void) /* GCC32OK */
{
  int j;
  tree decl;

  if (current_block) abort ();
  for (decl = global_vars; decl; decl = TREE_CHAIN (decl))
    if (TREE_CODE (decl) == FUNCTION_DECL
	&& ! TREE_ASM_WRITTEN (decl)
	&& TREE_ADDRESSABLE (decl)
	&& DECL_INITIAL (decl))
      {
	output_inline_function (decl);
      }

  debug_tag ('i', NO_UID, "_%s", current_unit_name);
  for (j = 0; j < exported_interfaces; j++)
    debug_field (exported_interfaces_names [j]);
  debug_struct ();
  if (fault_proc != NULL_TREE) emit_fault_proc ();
}

static void
m3cg_import_unit (void) /* GCC32OK */
{
  UNUSED_NAME (n);
  /* ignore */
}

static void
m3cg_export_unit (void) /* GCC32OK */
{
  NAME (n);
  /* remember the set of exported interfaces */
  exported_interfaces_names [exported_interfaces++] = n;
}

static void
m3cg_set_source_file (void) /* GCC32OK */
{
  NAME (s);

  input_filename = s;
  if (cfun != NULL) emit_line_note (input_filename, lineno);
}

static void
m3cg_set_source_line (void) /* GCC32OK */
{
  INTEGER (i);

  if (option_source_line_trace) fprintf(stderr, "  source line %4ld\n", i);
  lineno = i;
  if (cfun != NULL) emit_line_note (input_filename, lineno);
}

static void
m3cg_declare_typename (void) /* GCC32OK */
{
  TYPEID (my_id);
  NAME   (name);

  char fullname [100];

  if (option_types_trace)
    fprintf(stderr, "  typename %s, id %ld\n", name, my_id);

  snprintf (fullname, sizeof(fullname), "%s.%s", current_unit_name, name);
  debug_tag ('N', my_id, "");
  debug_field (fullname);
  debug_struct ();

  debug_tag ('n', NO_UID, "_%s", fullname);
  debug_field_id (my_id);
  debug_struct ();
}

static void
m3cg_declare_array (void) /* GCC32OK */
{
  TYPEID  (my_id);
  TYPEID  (index_id);
  TYPEID  (elts_id);
  BITSIZE (size);

  if (option_types_trace)
    fprintf(stderr, 
            "  array id %ld, index id %ld, elements id %ld, size %ld\n",
            my_id, index_id, elts_id, size);

  debug_tag ('A', my_id, "_%d", size);
  debug_field_id (index_id);
  debug_field_id (elts_id);
  debug_struct ();
}

static void
m3cg_declare_open_array (void) /* GCC32OK */
{
  TYPEID  (my_id);
  TYPEID  (elts_id);
  BITSIZE (size);

  if (option_types_trace)
    fprintf(stderr,
            "  open array id %ld, elements id %ld, size %ld\n",
            my_id, elts_id, size);

  debug_tag ('B', my_id, "_%d", size);
  debug_field_id (elts_id);
  debug_struct ();
}

static void
m3cg_declare_enum (void) /* GCC32OK */
{
  TYPEID  (my_id);
  INTEGER (n_elts);
  BITSIZE (size);

  if (option_types_trace)
    fprintf(stderr,
            "  enum id %ld, elements %ld, size %ld\n",
            my_id, n_elts, size);

  debug_tag ('C', my_id, "_%d", size);
  current_dbg_type_count1 = n_elts;
}

static void
m3cg_declare_enum_elt (void) /* GCC32OK */
{
  NAME (n);

  if (option_types_trace)
    fprintf(stderr,
            "  enum elem %s\n", n);
  debug_field (n);
  if (--current_dbg_type_count1 == 0) { debug_struct (); }
}

static void
m3cg_declare_packed (void) /* GCC32OK */
{
  TYPEID  (my_id);
  BITSIZE (size);
  TYPEID  (target_id);

  if (option_types_trace)
    fprintf(stderr,
            "  packed id %ld, target id %ld, size %ld\n",
            my_id, target_id, size);
  debug_field_id (target_id);
  debug_tag ('D', my_id, "_%d", size);
  debug_struct ();
}

static void
m3cg_declare_record (void) /* GCC32OK */
{
  TYPEID  (my_id);
  BITSIZE (size);
  INTEGER (n_fields);

  if (option_types_trace)
    fprintf(stderr,
            "  record id %ld, fields %ld, size %ld\n",
            my_id, n_fields, size);
  debug_tag ('R', my_id, "_%d", size);
  current_dbg_type_count1 = n_fields;
  current_dbg_type_count2 = 0;
  if (current_dbg_type_count1 == 0) { debug_struct (); }
}

static void
m3cg_declare_field (void) /* GCC32OK */
{
  NAME      (name);
  BITOFFSET (offset);
  BITSIZE   (size);
  TYPEID    (my_id);

  if (option_types_trace)
    fprintf(stderr, "  field %s, id %ld, size %ld, offset %ld\n",
            name, my_id, size, offset);

  debug_field_fmt (my_id, "_%d_%d_%s", offset, size, name);
  current_dbg_type_count1--;
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
    debug_struct ();
  }
}

static void
m3cg_declare_set (void) /* GCC32OK */
{
  TYPEID  (my_id);
  TYPEID  (domain_id);
  BITSIZE (size);

  debug_tag ('S', my_id, "_%d", size);
  debug_field_id (domain_id);
  debug_struct ();
}

static void
m3cg_declare_subrange (void) /* GCC32OK */
{

  TYPEID         (my_id);
  TYPEID         (domain_id);
  TARGET_INTEGER (min);
  TARGET_INTEGER (max);
  BITSIZE        (size);

  HOST_WIDE_INT  a0, a1, b0, b1;
  a1 = TREE_INT_CST_HIGH(min);  a0 = TREE_INT_CST_LOW(min);
  b1 = TREE_INT_CST_HIGH(max);  b0 = TREE_INT_CST_LOW(max);

  if (option_types_trace)
    fprintf(stderr, 
            "  subrange id %ld, a0 %d, a1 %d, b0 %d, b1 %d, size %ld\n",
            my_id, a0, a1, b0, b1, size);

  if ((a1 != 0) && (a1 != -1 || a0 >= 0)) {
    fatal_error ("cannot print minimum subrange value");
  }
  if ((b1 != 0) && (b1 != -1 || b0 >= 0)) {
    fatal_error ("cannot print maximum subrange value");
  }

  debug_tag ('Z', my_id, "_%d_%ld_%ld", size, a0, b0);
  debug_field_id (domain_id);
  debug_struct ();
}

static void
m3cg_declare_pointer (void) /* GCC32OK */
{
  TYPEID        (my_id);
  TYPEID        (target_id);
  QUOTED_STRING (brand, brand_len);
  BOOLEAN       (traced);

  if (option_types_trace) {
    const char * sbrand = "null";
    if (brand) sbrand = brand;
    fprintf(stderr, 
            "  pointer id %ld, target id %ld, brand %s, traced %d\n",
            my_id, target_id, sbrand, traced);
  }

  debug_tag ('Y', my_id, "_%d_%d_%d_%s", GET_MODE_BITSIZE (Pmode),
	     traced, (brand ? 1 : 0), (brand ? brand : "" ));
  debug_field_id (target_id);
  debug_struct ();
}

static void
m3cg_declare_indirect (void) /* GCC32OK */
{
  TYPEID (my_id);
  TYPEID (target_id);

  if (option_types_trace)
    fprintf(stderr, "  indirect id %ld, target_id %ld\n", my_id, target_id);
  debug_tag ('X', my_id, "_%d", GET_MODE_BITSIZE (Pmode));
  debug_field_id (target_id);
  debug_struct ();
}

static void
m3cg_declare_proctype (void) /* GCC32OK */
{
  TYPEID  (my_id);
  INTEGER (n_formals);
  TYPEID  (result_id); 
  INTEGER (n_raises);
  UNUSED_INTEGER (call_conv);

  if (option_types_trace)
    fprintf(stderr, 
            "  proctype id %ld, result id %ld, formals %ld, raises %ld\n",
            my_id, result_id, n_formals, n_raises);
  debug_tag ('P', my_id, "_%d_%c%d", GET_MODE_BITSIZE (Pmode),
	     n_raises < 0 ? 'A' : 'L', MAX (n_raises, 0));
  current_dbg_type_count1 = n_formals;
  current_dbg_type_count2 = MAX (0, n_raises);
  debug_field_id (result_id);
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
    debug_struct ();
  }
}

static void
m3cg_declare_formal (void) /* GCC32OK */
{
  NAME   (n);
  TYPEID (my_id);

  if (option_types_trace)
    fprintf(stderr, "  formal %s id %ld\n", n, my_id);
  debug_field_fmt (my_id, "_%s", n);
  current_dbg_type_count1--;
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
    debug_struct ();
  }
}

static void
m3cg_declare_raises (void) /* GCC32OK */
{
  NAME (n);

  if (option_types_trace)
    fprintf(stderr, "  exception %s\n", n);
  debug_field (n);
  current_dbg_type_count2--;
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
    debug_struct ();
  }
}

static void
m3cg_declare_object (void) /* GCC32OK */
{
  TYPEID        (my_id);
  TYPEID        (super_id);
  QUOTED_STRING (brand, brand_length);
  BOOLEAN       (traced);
  INTEGER       (n_fields);
  INTEGER       (n_methods);
  UNUSED_BITSIZE       (field_size);

  if (option_types_trace) {
    const char * sbrand = "null";
    if (brand) sbrand = brand;
    fprintf(stderr, 
            "  object id %ld, super id %ld, brand %s, traced %d, fields %ld, methods %ld\n",
            my_id, super_id, sbrand, traced, n_fields, n_methods);
  }

  debug_tag ('O', my_id, "_%d_%d_%d_%d_%s", POINTER_SIZE, n_fields, traced,
	     (brand ? 1:0), (brand ? brand : ""));
  debug_field_id (super_id);
  current_dbg_type_count1 = n_fields;
  current_dbg_type_count2 = n_methods;
  current_dbg_type_count3 = 0;
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
    debug_struct ();
  }
}

static void
m3cg_declare_method (void) /* GCC32OK */
{
  NAME   (name);
  TYPEID (my_id);

  if (option_procs_trace)
    fprintf(stderr, "  method %s typeid %ld\n", name, my_id);

  debug_field_fmt (my_id, "_%d_%d_%s", 
		   current_dbg_type_count3++  * GET_MODE_BITSIZE (Pmode),
		   GET_MODE_BITSIZE (Pmode), name);
  current_dbg_type_count2--;
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
    debug_struct ();
  }
}

static void
m3cg_declare_opaque (void) /* GCC32OK */
{
  UNUSED_TYPEID (my_id);
  UNUSED_TYPEID (super_id);
  /* we don't pass this info to the debugger, only the revelation is interesting */
}

static void
m3cg_reveal_opaque (void) /* GCC32OK */
{
  TYPEID (lhs);
  TYPEID (rhs);

  if (option_procs_trace)
    fprintf(stderr, "  typeid %ld = typeid %ld\n", lhs, rhs);

  debug_tag ('Q', lhs, "_%d", GET_MODE_BITSIZE (Pmode));
  debug_field_id (rhs);
  debug_struct ();
}

static void
m3cg_declare_exception (void) /* GCC32OK */
{
  UNUSED_NAME    (n);
  UNUSED_TYPEID  (t);
  UNUSED_BOOLEAN (raise_proc); 
  UNUSED_VAR     (base);
  UNUSED_INTEGER (offset);

  /* nothing yet */
}

static void
m3cg_set_runtime_proc (void) /* GCC32OK */
{
  NAME (s);
  PROC (p);

  if (STREQ (s, "ReportFault")) { fault_handler = p; }
}

static void
m3cg_set_runtime_hook (void) /* GCC32OK */
{
  NAME       (s);
  VAR        (v);
  BYTEOFFSET (o);

  if (STREQ (s, "ReportFault")) { fault_intf = v; fault_offs = o; }
}

static void
m3cg_import_global (void) /* GCC32OK */
{
  NAME       (n);
  BYTESIZE   (s);
  ALIGNMENT  (a);
  TYPE       (t);
  TYPEID     (id);
  RETURN_VAR (v, VAR_DECL);

  DECL_NAME(v) = fix_name (n, id);
  if (option_vars_trace)
    fprintf(stderr, "  import var %s type %d size %ld alignment %ld\n",
	    IDENTIFIER_POINTER(DECL_NAME(v)), t, s, a);
  DECL_EXTERNAL (v) = 1;
  TREE_PUBLIC   (v) = 1;

  TREE_TYPE (v) = m3_build_type (t, s, a);
  layout_decl (v, a);

  rest_of_decl_compilation (v, 0, 1, 0);

  /* assemble_external (v); */
  /* TREE_USED  (v) = 1; */
  TREE_CHAIN (v) = global_vars;
  global_vars = v;
}

static void
m3cg_declare_segment (void) /* GCC32OK */
{
  NAME       (n);
  TYPEID     (id);
  BOOLEAN    (is_const);
  RETURN_VAR (v, VAR_DECL);
      
  DECL_NAME (v) = fix_name (n, id);
  if (option_vars_trace)
    fprintf(stderr, "  segment %s typeid %ld\n",
	    IDENTIFIER_POINTER(DECL_NAME(v)), id);
  /* we really don't have an idea of what the type of this var is; 
     let's try to put something that will be good enough for all
     the uses of this var we are going to see before we have a bind_segment */
  TREE_TYPE (v)
    = m3_build_type (T_struct, BIGGEST_ALIGNMENT, BIGGEST_ALIGNMENT);
  layout_decl (v, BIGGEST_ALIGNMENT);
  TREE_UNSIGNED (TREE_TYPE (v)) = 1;
  TREE_STATIC (v) = 1;
  TREE_PUBLIC (v) = 1;
  TREE_READONLY (v) = is_const;
  DECL_DEFER_OUTPUT (v) = 1;
  rest_of_decl_compilation (v, 0, 1, 0); 
  TREE_CHAIN (v) = global_vars;
  global_vars = v;
  current_segment = v;

  /* do not use "n", it is going to go away at the next instruction;
	 skip the 'MI_' or 'MM_' prefix. */
  current_unit_name = IDENTIFIER_POINTER (DECL_NAME (v)) + 3;
}

static void
m3cg_bind_segment (void) /* GCC32OK */
{
  VAR       (v);
  BYTESIZE  (s);
  ALIGNMENT (a);
  TYPE      (t);
  BOOLEAN   (exported);
  UNUSED_BOOLEAN   (initialized);

  if (option_vars_trace)
    fprintf(stderr, "  bind segment %s type %d size %ld alignment %ld\n",
            IDENTIFIER_POINTER(DECL_NAME(v)), t, s, a);
  current_segment = v;
  /* Clear the size, alignment, and mode of the variable so that layout_decl
     will set them properly using our updated information. */
  DECL_SIZE (v) = 0;
  DECL_SIZE_UNIT (v) = 0;
  DECL_ALIGN (v) = 0;
  DECL_MODE (v) = VOIDmode;
  TREE_TYPE (v) = m3_build_type (t, s, a);
  layout_decl (v, a);
  TREE_UNSIGNED (v) = TREE_UNSIGNED (TREE_TYPE (v));
  TREE_PUBLIC (v) = exported;
  TREE_STATIC (v) = 1;
}

static void
m3cg_declare_global (void) /* GCC32OK */
{
  NAME       (n);
  BYTESIZE   (s);
  ALIGNMENT  (a);
  TYPE       (t);
  TYPEID     (id);
  BOOLEAN    (exported);
  BOOLEAN    (initialized);
  RETURN_VAR (v, VAR_DECL);

  DECL_NAME (v) = fix_name (n, id);
  if (option_vars_trace)
    fprintf(stderr, "  global var %s type %d size %ld alignment %ld\n",
            IDENTIFIER_POINTER(DECL_NAME(v)), t, s, a);
  TREE_TYPE (v) = m3_build_type (t, s, a);
  DECL_COMMON (v) = (initialized == 0);
  TREE_PUBLIC (v) = exported;
  TREE_STATIC (v) = 1;
  layout_decl (v, a);

  rest_of_decl_compilation (v, 0, 1, 0);
  TREE_CHAIN (v) = global_vars;
  global_vars = v;
}

static void
m3cg_declare_constant (void) /* GCC32OK */
{
  NAME       (n);
  BYTESIZE   (s);
  ALIGNMENT  (a);
  TYPE       (t);
  TYPEID     (id);
  BOOLEAN    (exported);
  BOOLEAN    (initialized);
  RETURN_VAR (v, VAR_DECL);

  DECL_NAME (v) = fix_name (n, id);
  TREE_TYPE (v) = m3_build_type (t, s, a);
  DECL_COMMON (v) = (initialized == 0);
  TREE_PUBLIC (v) = exported;
  TREE_STATIC (v) = 1;
  TREE_READONLY (v) = 1;
  layout_decl (v, a);
  
  rest_of_decl_compilation (v, 0, 1, 0);
  TREE_CHAIN (v) = global_vars;
  global_vars = v;
}

static void
m3cg_declare_local (void) /* GCC32OK */
{
  NAME       (n);
  BYTESIZE   (s);
  ALIGNMENT  (a);
  TYPE       (t);
  TYPEID     (id);
  BOOLEAN    (in_memory);
  BOOLEAN    (up_level);
  UNUSED_FREQUENCY  (f);
  RETURN_VAR (v, VAR_DECL);

  DECL_NAME (v) = fix_name (n, id);
  if (option_vars_trace)
    fprintf(stderr, "  local var %s type %d size %ld alignment %ld\n",
            IDENTIFIER_POINTER(DECL_NAME(v)), t, s, a);
  TREE_TYPE (v) = m3_build_type (t, s, a);
  DECL_NONLOCAL (v) = up_level || in_memory;
  TREE_ADDRESSABLE (v) = in_memory;
  DECL_CONTEXT (v) = current_function_decl; 
  layout_decl (v, a);

  if (current_block)
    {
      expand_decl (v);
      rest_of_decl_compilation (v, 0, 0, 1);
      TREE_CHAIN (v) = BLOCK_VARS (current_block);
      BLOCK_VARS (current_block) = v;
    }
  else
    {
      tree subblocks = BLOCK_SUBBLOCKS (DECL_INITIAL (current_function_decl));
      TREE_CHAIN (v) = BLOCK_VARS (subblocks);
      BLOCK_VARS (subblocks) = v;
    }
}

static int current_param_count;

static void
m3cg_declare_param (void) /* GCC32OK */
{
  NAME       (n);
  BYTESIZE   (s);
  ALIGNMENT  (a);
  TYPE       (t);
  TYPEID     (id);
  BOOLEAN    (in_memory);
  BOOLEAN    (up_level);
  UNUSED_FREQUENCY  (f);
  RETURN_VAR (v, PARM_DECL);

  if (current_param_count == 0) return;	/* ignore */

  DECL_NAME (v) = fix_name (n, id);
  if (option_procs_trace)
    fprintf(stderr, "  param %s type %d typeid %ld\n",
	    IDENTIFIER_POINTER(DECL_NAME(v)), t, id);
  TREE_TYPE (v) = m3_build_type (t, s, a);
  DECL_NONLOCAL (v) = up_level || in_memory;
  TREE_ADDRESSABLE (v) = in_memory;
  DECL_ARG_TYPE (v) = TREE_TYPE (v);
  DECL_CONTEXT (v) = current_function_decl;
  layout_decl (v, a);
  if (DECL_MODE (v) == VOIDmode) DECL_MODE (v) = Pmode;

  TREE_CHAIN (v) = DECL_ARGUMENTS (current_function_decl);
  DECL_ARGUMENTS (current_function_decl) = v;

  rest_of_decl_compilation (v, 0, 0, 1);

  --current_param_count;
}

static void
m3cg_declare_temp (void) /* GCC32OK */
{
  BYTESIZE   (s);
  ALIGNMENT  (a);
  TYPE       (t);
  BOOLEAN    (in_memory);
  RETURN_VAR (v, VAR_DECL);

  if (option_vars_trace)
    fprintf(stderr, "  temp var type %d size %ld alignment %ld\n", t, s, a);
  if (t == T_void) t = T_struct;
  TREE_TYPE (v) = m3_build_type (t, s, a);
  layout_decl (v, 0);
  TREE_UNSIGNED (v) = TREE_UNSIGNED (TREE_TYPE (v));
  TREE_ADDRESSABLE (v) = in_memory;
  DECL_CONTEXT (v) = current_function_decl;

  TREE_CHAIN (v) = BLOCK_VARS (BLOCK_SUBBLOCKS
                               (DECL_INITIAL (current_function_decl)));
  BLOCK_VARS (BLOCK_SUBBLOCKS (DECL_INITIAL (current_function_decl))) = v;

  expand_decl (v);
  rest_of_decl_compilation (v, 0, 0, 1);
}

static void
m3cg_free_temp (void) /* GCC32OK */
{
  UNUSED_VAR (v);
  /* nothing to do */
}

static void
m3cg_begin_init (void) /* GCC32OK */
{
  UNUSED_VAR (v);

  current_record_offset = 0;
  current_record_vals = NULL_TREE;
  current_record_type = make_node (RECORD_TYPE);
  TREE_ASM_WRITTEN (current_record_type) = 1;
}

static void
m3cg_end_init (void) /* GCC32OK */
{
  VAR (v);

  int v_size = TREE_INT_CST_LOW (DECL_SIZE (v));
  if (current_record_offset < v_size) { one_gap (v_size); }
	  
  TYPE_FIELDS (current_record_type) = 
    nreverse (TYPE_FIELDS (current_record_type));
  layout_type (current_record_type);
  
  DECL_INITIAL (v) = make_node (CONSTRUCTOR);
  TREE_CONSTANT (DECL_INITIAL (v)) = 1;
  TREE_TYPE (DECL_INITIAL (v)) = current_record_type;
  CONSTRUCTOR_ELTS (DECL_INITIAL (v)) = nreverse (current_record_vals);
}

static void
m3cg_init_int (void) /* GCC32OK */
{
  BYTEOFFSET     (o);
  TARGET_INTEGER (v);
  MTYPE          (t);

  tree f, vv;

  one_field (o, t, &f, &vv);
#if 1
  TREE_VALUE (vv) = convert (TREE_TYPE (f), v);
#else
  TREE_TYPE (f) = t;
  TREE_VALUE (vv) = v;
  TREE_TYPE (TREE_VALUE (vv)) = TREE_TYPE (f);
#endif
}

static void
m3cg_init_proc (void) /* GCC32OK */
{
  BYTEOFFSET (o);
  PROC       (p);

  tree f, v;
  tree expr = proc_addr (p, 1);
  one_field (o, TREE_TYPE (expr), &f, &v);
  TREE_VALUE (v) = expr;
}

static void
m3cg_init_label (void) /* GCC32OK */
{
  BYTEOFFSET (o);
  LABEL      (l);

  tree f, v;

  one_field (o, t_addr, &f, &v);
  TREE_VALUE (v) = build (RTL_EXPR, t_addr, NULL_TREE, label_rtx (l));
}

static void
m3cg_init_var (void) /* GCC32OK */
{
  BYTEOFFSET (o);
  VAR        (v);
  BYTEOFFSET (b);

  tree F, V;

  one_field (o, t_addr, &F, &V);
  TREE_VALUE (V) = m3_build2 (PLUS_EXPR, t_addr,
                              m3_build1 (ADDR_EXPR, t_addr, v),
                              size_int (b / BITS_PER_UNIT));
}

static void
m3cg_init_offset (void) /* GCC32OK */
{
  BYTEOFFSET (o);
  VAR        (v);

  tree F, V;
  int j;

  one_field (o, t_int, &F, &V);

  /* take apart the rtx, which is of the form
       (insn n m p (use (mem: (plus: (reg: r $fp) 
       (const_int offset))) ...)
     or 
       (insn n m p (use (mem: (reg: r $fp))) ...)
       for offset 0. */
  {
    rtx r = DECL_RTL (v);	/* (mem ...) */
    r = XEXP (r, 0);	/* (plus ...) or (reg ...) */
    if (REG_P (r)) {
      j = 0;
    } else {
      r = XEXP (r, 1);	/* (const_int ...) */
      j = XINT (r, 0);  /* offset */
    }
  }

  /* TREE_VALUE (V) = m3_build_int (j); ? */
  TREE_VALUE (V) = size_int (j);
}

static void
m3cg_init_chars (void) /* GCC32OK */
{
  BYTEOFFSET    (o);
  QUOTED_STRING (s, l);

  tree f, v, tipe;
  
  tipe = build_array_type (char_type_node,
                           build_index_type (size_int (l - 1)));
  one_field (o, tipe, &f, &v);
  TREE_VALUE (v) = build_string (l, s);
  TREE_TYPE (TREE_VALUE (v)) = TREE_TYPE (f);
}

static void
m3cg_init_float (void) /* GCC32OK */
{
  BYTEOFFSET (o);
  FLOAT      (f, fkind);

  tree F, V, t;

  switch (fkind) {
  case 0: t = t_reel;   break; 
  case 1: t = t_lreel;  break; 
  case 2: t = t_xreel;  break;
  default: t = t_lreel; break; /* make the compiler happy */
  }

  one_field (o, t, &F, &V);
  TREE_TYPE (F) = t;
  TREE_VALUE (V) = f;

#if 0 /* FIXME? used to be */
  TREE_VALUE (V) = build_real (TREE_TYPE (F),
                               REAL_VALUE_ATOF (f, TYPE_MODE (TREE_TYPE (F))));
#endif
}

#define M3CG_ADAPT_RETURN_TYPE  1
#define M3CG_ADAPT_RETURN_TYPE2 0

static void
m3cg_import_procedure (void) /* GCC32OK */
{
  NAME    (n);
  INTEGER (n_params);
  MTYPE2  (return_type, ret_type);
  INTEGER (call_conv);
  PROC    (p);

  if (option_procs_trace)
    fprintf(stderr, "  procedure %s nparams %ld rettype %d\n", n, n_params,
            ret_type);

#if M3CG_ADAPT_RETURN_TYPE
  /** 4/30/96 -- WKK -- It seems gcc can't hack small return values... */
  if (IS_INTEGER_TYPE(ret_type) && 
      TYPE_SIZE(return_type) <= TYPE_SIZE(t_int)) {
    return_type = t_int;
  }
  if (IS_WORD_TYPE(ret_type) && TYPE_SIZE(return_type) <= TYPE_SIZE(t_word)) {
    return_type = t_word;
  }
#endif

  if (call_conv < 0 || call_conv > 1) {
    fatal_error (" *** m3cg_import_procedure: invalid call convention id"); 
  }
  if (p == 0) {
    p = build_decl (FUNCTION_DECL, get_identifier (n), NULL_TREE);
  } else {
    DECL_NAME (p) = get_identifier (n);
  }

  TREE_TYPE (p) = build_function_type (return_type, NULL_TREE);
  TREE_PUBLIC (p) = 1;
  TREE_THIS_VOLATILE (p) = 0;
  TREE_SIDE_EFFECTS (p) = 1;
  DECL_EXTERNAL (p) = 1;
  DECL_CONTEXT (p) = NULL_TREE;
  DECL_MODE (p) = FUNCTION_MODE;

  /* make_decl_rtl (p, 0); */
  /* assemble_external (p); */
  /* TREE_USED (p) = 1; */

  current_param_count = 0;	/* ignore them */
}

static void
m3cg_declare_procedure (void) /* GCC32OK */
{
  NAME    (n);
  INTEGER (n_params);
  MTYPE2  (return_type, ret_type);
  UNUSED_LEVEL (lev);
  INTEGER (call_conv);
  BOOLEAN (exported);
  PROC    (parent);
  PROC    (p);

  tree parm_block = make_node (BLOCK);
  tree top_block  = make_node (BLOCK);

  if (option_procs_trace)
    fprintf(stderr, "  procedure %s nparams %ld rettype %d\n", n, n_params,
            ret_type);

  if (call_conv < 0 || call_conv > 1) {
    fatal_error(" *** m3cg_declare_procedure: invalid call convention id"); 
  }
#if M3CG_ADAPT_RETURN_TYPE
  /** 4/30/96 -- WKK -- It seems gcc can't hack small return values... */
  if (IS_INTEGER_TYPE(ret_type) && 
      TYPE_SIZE(return_type) <= TYPE_SIZE(t_int)) {
    return_type = t_int;
  }
  if (IS_WORD_TYPE(ret_type) && TYPE_SIZE(return_type) <= TYPE_SIZE(t_word)) {
    return_type = t_word;
  }
#endif
  DECL_NAME (p) = get_identifier (n);
  TREE_STATIC (p) = 1;
  TREE_PUBLIC (p) = exported;
  DECL_CONTEXT (p) = parent;
  TREE_TYPE (p) = build_function_type (return_type, NULL_TREE);
  DECL_MODE (p) = FUNCTION_MODE;
  DECL_RESULT (p) = build_decl (RESULT_DECL, NULL_TREE, return_type);
  DECL_CONTEXT (DECL_RESULT (p)) = p;

  BLOCK_SUPERCONTEXT (parm_block) = p;
  DECL_INITIAL (p) = parm_block;

  BLOCK_SUPERCONTEXT (top_block) = parm_block;
  BLOCK_SUBBLOCKS (parm_block) = top_block;

  if (current_block) abort ();
  TREE_CHAIN (p) = global_vars;
  global_vars = p;

  /* make_decl_rtl (p, 0); */
  current_function_decl = p;
  current_param_count = n_params;
}

static void
m3cg_begin_procedure (void) /* GCC32OK */
{
  PROC (p);
  tree parm, local, args_types;

  if (option_procs_trace)
    fprintf(stderr, "  procedure %s\n", IDENTIFIER_POINTER(DECL_NAME(p)));

  DECL_SOURCE_LINE (p) = lineno;

  args_types = tree_cons (NULL_TREE, t_void, NULL_TREE);
  for (parm = DECL_ARGUMENTS (p); parm; parm = TREE_CHAIN (parm)) {
    args_types = tree_cons (NULL_TREE, TREE_TYPE (parm), args_types);
  }
  TREE_TYPE (p) = build_function_type (TREE_TYPE (DECL_RESULT (p)), args_types);
  DECL_ARGUMENTS (p) = nreverse (DECL_ARGUMENTS (p));
  announce_function (p);
  make_decl_rtl (p, NULL);

  if (current_block) push_function_context ();

  current_function_decl = p;

  init_function_start (p, input_filename, lineno);
  expand_function_start (p, 0);

  clear_last_expr ();
  pending_blocks = tree_cons (NULL_TREE, current_block, pending_blocks);
  current_block = DECL_INITIAL (p); /* parm_block */
  TREE_USED (current_block) = 1;
  current_block = BLOCK_SUBBLOCKS (current_block); /* top_block */
  TREE_USED (current_block) = 1;
  expand_start_bindings_and_block (0, current_block);

  /* compile the locals we have already seen */
  for (local = BLOCK_VARS (current_block); local; local = TREE_CHAIN (local))
    {
      expand_decl (local);
      rest_of_decl_compilation (local, 0, 0, 1);
    }
}

static void
m3cg_end_procedure (void) /* GCC32OK */
{
  PROC (p);

  if (option_procs_trace)
    fprintf(stderr, "  procedure %s\n", IDENTIFIER_POINTER(DECL_NAME(p)));

  if (current_block != BLOCK_SUBBLOCKS (DECL_INITIAL (p))) abort ();
  expand_end_bindings (BLOCK_VARS (current_block), 1, 0);
  expand_function_end (input_filename, lineno, 0);
  rest_of_compilation (current_function_decl);

  current_block = TREE_VALUE (pending_blocks);
  pending_blocks = TREE_CHAIN (pending_blocks);

  if (current_block) pop_function_context ();
}

static void
m3cg_begin_block (void) /* GCC32OK */
{
  tree b = build_block (NULL_TREE, 0, NULL_TREE, current_block, NULL_TREE);
  BLOCK_SUBBLOCKS (current_block)
    = chainon (BLOCK_SUBBLOCKS (current_block), b);
  TREE_USED (b) = 1;
  clear_last_expr ();
  expand_start_bindings_and_block (0, b);
  pending_blocks = tree_cons (NULL_TREE, current_block, pending_blocks);
  current_block = b;
}

static void
m3cg_end_block (void) /* GCC32OK */
{
  expand_end_bindings (BLOCK_VARS (current_block), 1, 0);
  current_block = TREE_VALUE (pending_blocks);
  pending_blocks = TREE_CHAIN (pending_blocks);
}

static void
m3cg_note_procedure_origin (void) /* GCC32OK */
{
  UNUSED_PROC (p);

  fatal_error("note_procedure_origin psuedo-op encountered.");
}

static void
m3cg_set_label (void) /* GCC32OK */
{
  LABEL   (l);
  BOOLEAN (barrier);

  DECL_CONTEXT (l) = current_function_decl;
  expand_label (l);
  if (barrier) {
    rtx r = label_rtx(l);
    LABEL_PRESERVE_P (r) = 1;
    /* Tell flow about the strange goings on.  Putting the label on
       `nonlocal_goto_handler_labels' indicates that function calls may
       traverse the arc back to this label.  */
    current_function_has_nonlocal_label = 1;
    nonlocal_goto_handler_labels
      = gen_rtx_EXPR_LIST (VOIDmode, r, nonlocal_goto_handler_labels);
    forced_labels = gen_rtx_EXPR_LIST (VOIDmode, r, forced_labels);

    TREE_USED (l) = 1;
  }
}

static void
m3cg_m3_jump (void) /* GCC32OK */
{
  LABEL (l);

  expand_goto (l);
}

static void
m3cg_if_true (void) /* GCC32OK */
{
  UNUSED_TYPE      (t);
  LABEL     (l);
  UNUSED_FREQUENCY (f);

  tree cond = EXPR_REF (-1);
  EXPR_POP ();
  do_jump (cond, NULL_RTX, label_rtx (l));
}

static void
m3cg_if_false (void) /* GCC32OK */
{
  UNUSED_TYPE      (t);
  LABEL     (l);
  UNUSED_FREQUENCY (f);

  tree cond = EXPR_REF (-1);
  EXPR_POP ();
  do_jump (cond, label_rtx (l), NULL_RTX);
}

static void
m3cg_if_compare (enum tree_code condition)  /* GCC32OK */
{
  MTYPE     (t);
  LABEL     (l);
  UNUSED_FREQUENCY (f);

  condop (condition, l, t);
}

static void m3cg_if_eq (void) { m3cg_if_compare (EQ_EXPR); } /*GCC32OK*/
static void m3cg_if_ne (void) { m3cg_if_compare (NE_EXPR); } /*GCC32OK*/
static void m3cg_if_gt (void) { m3cg_if_compare (GT_EXPR); } /*GCC32OK*/
static void m3cg_if_ge (void) { m3cg_if_compare (GE_EXPR); } /*GCC32OK*/
static void m3cg_if_lt (void) { m3cg_if_compare (LT_EXPR); } /*GCC32OK*/
static void m3cg_if_le (void) { m3cg_if_compare (LE_EXPR); } /*GCC32OK*/

static void
m3cg_case_jump (void) /* GCC32OK */
{
  MTYPE   (t);
  INTEGER (n);

  tree index_expr = EXPR_REF (-1);
  int i;

  expand_start_case (1, index_expr, t, "case_jump");
  for (i = 0; i < n; i++) {
    LABEL (target_label);
    tree case_label;
    tree duplicate;

    case_label = build_decl (LABEL_DECL, NULL_TREE, t_addr);
    DECL_CONTEXT (case_label) = current_function_decl;
    add_case_node (m3_build_int (i), NULL, case_label, &duplicate);
    expand_goto (target_label);
  }
  expand_end_case_type (index_expr, t);
  EXPR_POP();
}

static void
m3cg_exit_proc (void) /* GCC32OK */
{
  MTYPE2 (t, m3t);

#if M3CG_ADAPT_RETURN_TYPE2
  /** 4/30/96 -- WKK -- It seems gcc can't hack small return values... */
  if (IS_INTEGER_TYPE(m3t) && 
      TYPE_SIZE(t) <= TYPE_SIZE(t_int)) {
    t = t_int;
  }
  if (IS_WORD_TYPE(m3t) && TYPE_SIZE(t) <= TYPE_SIZE(t_word)) {
    t = t_word;
  }
#endif
  if (t == t_void) {
    expand_null_return ();
  } else {
    tree res = m3_build2 (MODIFY_EXPR, t, DECL_RESULT (current_function_decl),
			  build1 (CONVERT_EXPR, t, EXPR_REF (-1)));
    TREE_SIDE_EFFECTS (res) = 1;
    expand_return (res);
    EXPR_POP ();
  }
}

static void
m3cg_load (void) /* GCC32OK */
{
  VAR        (v);
  BYTEOFFSET (o);
  MTYPE2     (src_t, src_T);
  MTYPE2     (dst_t, dst_T);

  m3_load (v, o, src_t, src_T, dst_t, dst_T);
}

static void
m3cg_load_address (void) /* GCC32OK */
{
  VAR        (v);
  BYTEOFFSET (o);

  tree expr = m3_build1 (ADDR_EXPR, t_addr, v);
  if (option_vars_trace) {
    const char *name = "noname";
    if (v != 0 && DECL_NAME(v) != 0) {
      name = IDENTIFIER_POINTER(DECL_NAME(v));
    }
    fprintf(stderr, "  load address (%s) offset %ld\n", name, o);
  }
  if (o != 0) {
    expr = m3_build2 (PLUS_EXPR, t_addr, expr,
                      size_int (o / BITS_PER_UNIT));
  }
  EXPR_PUSH (expr);
}

static void
m3cg_load_indirect (void) /* GCC32OK */
{
  BYTEOFFSET (o);
  MTYPE2     (src_t, src_T);
  MTYPE2     (dst_t, dst_T);

  tree val = EXPR_REF (-1);

  if (option_vars_trace) {
    fprintf(stderr, "  load address offset %ld src_t %d dst_t %d\n",
            o, src_T, dst_T);
  }

  if (o != 0) { 
    val = m3_build2 (PLUS_EXPR, t_addr, val, size_int (o / BITS_PER_UNIT));
  }
  val = m3_cast (build_pointer_type (src_t), val);
  val = m3_build1 (INDIRECT_REF, src_t, val);
  if (src_T != dst_T) {
    val = m3_build1 (CONVERT_EXPR, dst_t, val);
  }

  EXPR_REF (-1) = val;
}

static void
m3cg_store (void) /* GCC32OK */
{
  VAR        (v);
  BYTEOFFSET (o);
  MTYPE2     (src_t, src_T);
  MTYPE2     (dst_t, dst_T);

  if (option_vars_trace) {
    const char *name = "noname";
    if (v != 0 && DECL_NAME(v) != 0) {
      name = IDENTIFIER_POINTER(DECL_NAME(v));
    }
    fprintf(stderr, "  store (%s) offset %ld src_t %d dst_t %d\n", 
            name, o, src_T, dst_T);
  }
  m3_store (v, o, src_t, dst_t);
}

static void
m3cg_store_indirect (void) /* GCC32OK */
{
  BYTEOFFSET (o);
  UNUSED_MTYPE2 (src_t, src_T);
  MTYPE2 (dst_t, dst_T);

  tree ptr = EXPR_REF (-2);
  tree val = EXPR_REF (-1);

  if (option_vars_trace) {
    fprintf(stderr, "  store indirect offset %ld src_t %d dst_t %d\n",
            o, src_T, dst_T);
  }
  if (o != 0) { 
    ptr = m3_build2 (PLUS_EXPR, t_addr, ptr, size_int (o / BITS_PER_UNIT));
  }
  ptr = m3_cast (build_pointer_type (dst_t), ptr);
  ptr = m3_build1 (INDIRECT_REF, dst_t, ptr);
  expand_assignment (ptr, m3_build1 (CONVERT_EXPR, dst_t, val), 0, 0);
  EXPR_POP ();
  EXPR_POP ();
}

static void
m3cg_load_nil (void) /* GCC32OK */
{
  EXPR_PUSH (v_null);
}

static void
m3cg_load_integer (void) /* GCC32OK */
{
  MTYPE          (t);
  TARGET_INTEGER (n);

  if (TREE_TYPE (n) != t) { n = m3_build1 (CONVERT_EXPR, t, n); }
  EXPR_PUSH(n);
}

static void
m3cg_load_float (void) /* GCC32OK */
{
  UNUSED_MTYPE (t);
  FLOAT (f, fkind);

  EXPR_PUSH (f);
}

static void
m3cg_compare (enum tree_code op) /* GCC32OK */
{
  MTYPE (src_t);
  MTYPE (dst_t);

  tree t1 = m3_cast (src_t, EXPR_REF(-1));
  tree t2 = m3_cast (src_t, EXPR_REF(-2));

  TREE_UNSIGNED (t1) = TREE_UNSIGNED (src_t);
  TREE_UNSIGNED (t2) = TREE_UNSIGNED (src_t);
  EXPR_REF(-2) = m3_build2 (op, dst_t, t2, t1);
  EXPR_POP ();
}

static void m3cg_eq (void) { m3cg_compare (EQ_EXPR); } /* GCC32OK */
static void m3cg_ne (void) { m3cg_compare (NE_EXPR); } /* GCC32OK */
static void m3cg_gt (void) { m3cg_compare (GT_EXPR); } /* GCC32OK */
static void m3cg_ge (void) { m3cg_compare (GE_EXPR); } /* GCC32OK */
static void m3cg_lt (void) { m3cg_compare (LT_EXPR); } /* GCC32OK */
static void m3cg_le (void) { m3cg_compare (LE_EXPR); } /* GCC32OK */

static void
m3cg_add (void) /* GCC32OK */
{
  MTYPE (t);

  binaryop (PLUS_EXPR, t);
}

static void
m3cg_subtract (void) /* GCC32OK */
{
  MTYPE (t);

  binaryop (MINUS_EXPR, t);
}

static void
m3cg_multiply (void) /* GCC32OK */
{
  MTYPE (t);

  binaryop (MULT_EXPR, t);
}

static void
m3cg_divide (void) /* GCC32OK */
{
  MTYPE (t);

  binaryop (RDIV_EXPR, t);
}

static void
m3cg_negate (void) /* GCC32OK */
{
  MTYPE (t);

  unaryop (NEGATE_EXPR, t);
}

static void
m3cg_abs (void) /* GCC32OK */
{
  MTYPE (t);

  unaryop (ABS_EXPR, t);
}

static void
m3cg_max (void) /* GCC32OK */
{
  MTYPE (t);

  tree temp1 = declare_temp (t);
  tree temp2 = declare_temp (t);
  tree t1 = m3_build2 (MODIFY_EXPR, t, temp1, EXPR_REF (-1));
  tree t2 = m3_build2 (MODIFY_EXPR, t, temp2, EXPR_REF (-2));
  tree res;
  TREE_SIDE_EFFECTS (t1) = 1;
  TREE_SIDE_EFFECTS (t2) = 1;
  res = m3_build3 (COND_EXPR, t,
                   m3_build2 (LE_EXPR, t_int, temp2, temp1), temp1, temp2);
  EXPR_REF (-2) = m3_build2 (COMPOUND_EXPR, t,
                             m3_build2 (COMPOUND_EXPR, t, t1, t2), res);
  EXPR_POP ();
}

static void
m3cg_min (void) /* GCC32OK */
{
  MTYPE (t);

  tree temp1 = declare_temp (t);
  tree temp2 = declare_temp (t);
  
  tree t1 = m3_build2 (MODIFY_EXPR, t, temp1, EXPR_REF (-1));
  tree t2 = m3_build2 (MODIFY_EXPR, t, temp2, EXPR_REF (-2));
  tree res;
  TREE_SIDE_EFFECTS (t1) = 1;
  TREE_SIDE_EFFECTS (t2) = 1;
  res = m3_build3 (COND_EXPR, t,
                   m3_build2 (LE_EXPR, t_int, temp1, temp2), temp1, temp2);
  EXPR_REF (-2) = m3_build2 (COMPOUND_EXPR, t,
                             m3_build2 (COMPOUND_EXPR, t, t1, t2), res);
  EXPR_POP ();
}

static void
m3cg_round (void) /* GCC32OK */
{
  MTYPE (src_t);
  MTYPE (dst_t);

  tree temp1 = declare_temp (t_lreel);
  tree t1, zero, half, res;

  if (src_t == t_reel) { src_t = t_lreel; unaryop (CONVERT_EXPR, src_t); }

  t1 = m3_build2 (MODIFY_EXPR, src_t, temp1, EXPR_REF(-1));
  zero = m3_build_real ("0.0", src_t);
  half = m3_build_real ("0.5", src_t);

  TREE_SIDE_EFFECTS (t1) = 1;
  res = m3_build1 (FIX_TRUNC_EXPR, dst_t,
		   m3_build3 (COND_EXPR, src_t,
			      m3_build2 (GE_EXPR, src_t, temp1, zero),
			      m3_build2 (PLUS_EXPR, src_t, temp1, half),
			      m3_build2 (MINUS_EXPR, src_t, temp1, half)));
  EXPR_REF(-1) = m3_build2 (COMPOUND_EXPR, dst_t, t1, res);
}

static void
m3cg_trunc (void) /* GCC32OK */
{
  UNUSED_MTYPE (src_t);
  MTYPE (dst_t);

  unaryop (FIX_TRUNC_EXPR, dst_t);
}

static void
m3cg_floor (void) /* GCC32OK */
{
  MTYPE (src_t);
  MTYPE (dst_t);

  tree temp1 = declare_temp (src_t);
  tree temp2 = declare_temp (dst_t);
  tree t1    = m3_build2 (MODIFY_EXPR, src_t, temp1, EXPR_REF(-1));
  tree t2    = m3_build2 (MODIFY_EXPR, dst_t, 
			  temp2, m3_build1 (FIX_TRUNC_EXPR, dst_t, temp1));
  tree zero = m3_build_real ("0.0", src_t);
  tree res;

  TREE_SIDE_EFFECTS (t1) = 1;
  TREE_SIDE_EFFECTS (t2) = 1;
  res = m3_build3 (COND_EXPR, dst_t,
		   m3_build2 (GE_EXPR, src_t, temp1, zero),
		   temp2,
		   m3_build3 (COND_EXPR, dst_t,
			      m3_build2 (EQ_EXPR, src_t, 
					 temp1, build1 (FLOAT_EXPR, 
                                                        src_t, temp2)),
			      temp2,
			      m3_build2 (MINUS_EXPR, dst_t,
					 temp2, v_one)));
  EXPR_REF(-1) = m3_build2 (COMPOUND_EXPR, dst_t,
                            m3_build2 (COMPOUND_EXPR, dst_t, t1, t2), res);
}

static void
m3cg_ceiling (void) /* GCC32OK */
{
  MTYPE (src_t);
  MTYPE (dst_t);

  tree temp1 = declare_temp (src_t);
  tree temp2 = declare_temp (dst_t);
  tree t1    = m3_build2 (MODIFY_EXPR, src_t, temp1, EXPR_REF(-1));
  tree t2    = m3_build2 (MODIFY_EXPR, dst_t,
			  temp2, m3_build1 (FIX_TRUNC_EXPR, dst_t, temp1));
  tree zero = m3_build_real ("0.0", src_t);
  tree res;

  TREE_SIDE_EFFECTS (t1) = 1;
  TREE_SIDE_EFFECTS (t2) = 1;
  res = m3_build3 (COND_EXPR, dst_t,
		   m3_build2 (LE_EXPR, src_t, temp1, zero),
		   temp2,
		   m3_build3 (COND_EXPR, dst_t,
			      m3_build2 (EQ_EXPR, src_t, temp1,
					 m3_build1 (FLOAT_EXPR, src_t, temp2)),
			      temp2,
			      m3_build2 (PLUS_EXPR, dst_t, temp2, v_one)));
  EXPR_REF(-1) = m3_build2 (COMPOUND_EXPR, dst_t,
                            m3_build2 (COMPOUND_EXPR, dst_t, t1, t2), res);
}

static void
m3cg_cvt_float (void) /* GCC32OK */
{
  UNUSED_MTYPE2 (src_t, src_T);
  MTYPE  (dst_t);

  if (IS_REAL_TYPE(src_T)) {
    unaryop (CONVERT_EXPR, dst_t);
  } else {
    unaryop (FLOAT_EXPR, dst_t);
  }
}

static void
m3cg_div (void) /* GCC32OK */
{
  MTYPE2 (t, T);
  SIGN   (a);
  SIGN   (b);

  if ((b == 'P' && a == 'P') || IS_WORD_TYPE(T)) {
    EXPR_REF (-2) = m3_cast (t_word, EXPR_REF (-2));
    EXPR_REF (-1) = m3_cast (t_word, EXPR_REF (-1));
    binaryop (FLOOR_DIV_EXPR, t);
  } else {
    m3_start_call ();
    m3_pop_param (t_int);
    m3_pop_param (t_int);
    m3_call_direct (div_proc, NULL_TREE);
  }
}

static void
m3cg_mod (void) /* GCC32OK */
{
  MTYPE2 (t, T);
  SIGN   (a);
  SIGN   (b);

  if ((b == 'P' && a == 'P') || IS_WORD_TYPE(T)) {
    EXPR_REF (-2) = m3_cast (t_word, EXPR_REF (-2));
    EXPR_REF (-1) = m3_cast (t_word, EXPR_REF (-1));
    binaryop (FLOOR_MOD_EXPR, t);
  } else {
    m3_start_call ();
    m3_pop_param (t_int);
    m3_pop_param (t_int);
    m3_call_direct (mod_proc, NULL_TREE);
  }
}

static void
m3cg_set_union (void) /* GCC32OK */
{
  BYTESIZE (n);

  setop (set_union_proc, n, 3);
}

static void
m3cg_set_difference (void) /* GCC32OK */
{
  BYTESIZE (n);

  setop (set_diff_proc, n, 3);
}

static void
m3cg_set_intersection (void) /* GCC32OK */
{
  BYTESIZE (n);

  setop (set_inter_proc, n, 3);
}

static void
m3cg_set_sym_difference (void) /* GCC32OK */
{
  BYTESIZE (n);

  setop (set_sdiff_proc, n, 3);
}

static void
m3cg_set_member (void) /* GCC32OK */
{
  UNUSED_BYTESIZE (n);
  MTYPE    (t);

  if (t != t_int) { fatal_error ("set_member on non-native integer size"); }
  setop2 (set_member_proc, 2);
}

static void
m3cg_set_compare (tree proc) /* GCC32OK */
{
  BYTESIZE (n);
  MTYPE    (t);

  if (t != t_int) { fatal_error ("set_compare on non-native integer size"); }
  setop (proc, n, 2);
}

static void m3cg_set_eq (void) { m3cg_set_compare (set_eq_proc); } /* GCC32OK */
static void m3cg_set_ne (void) { m3cg_set_compare (set_ne_proc); } /* GCC32OK */
static void m3cg_set_gt (void) { m3cg_set_compare (set_gt_proc); } /* GCC32OK */
static void m3cg_set_ge (void) { m3cg_set_compare (set_ge_proc); } /* GCC32OK */
static void m3cg_set_lt (void) { m3cg_set_compare (set_lt_proc); } /* GCC32OK */
static void m3cg_set_le (void) { m3cg_set_compare (set_le_proc); } /* GCC32OK */

static void
m3cg_set_range (void) /* GCC32OK */
{
  UNUSED_BYTESIZE (n);
  MTYPE    (t);

  if (t != t_int) { fatal_error ("set_range on non-native integer size"); }
  setop2 (set_range_proc, 3);
}

static void
m3cg_set_singleton (void) /* GCC32OK */
{
  UNUSED_BYTESIZE (n);
  MTYPE    (t);

  if (t != t_int) { fatal_error ("set_singleton on non-native integer size"); }
  setop2 (set_sing_proc, 2);
}

static void
m3cg_not (void) /* GCC32OK */
{
  MTYPE (t);

  unaryop (BIT_NOT_EXPR, unsigned_type (t));
}

static void
m3cg_and (void) /* GCC32OK */
{
  MTYPE (t);

  binaryop (BIT_AND_EXPR, unsigned_type(t));
}

static void
m3cg_or (void) /* GCC32OK */
{
  MTYPE (t);

  binaryop (BIT_IOR_EXPR, unsigned_type(t));
}

static void
m3cg_xor (void) /* GCC32OK */
{
  MTYPE (t);

  binaryop (BIT_XOR_EXPR, unsigned_type(t));
}

static void
m3cg_m3_shift (void) /* GCC32OK */
{
  MTYPE (t);

  tree temp1 = declare_temp (t);
  tree temp2 = declare_temp (t);
  tree t1    = m3_build2 (MODIFY_EXPR, t, temp1, EXPR_REF(-1));
  tree t2    = m3_build2 (MODIFY_EXPR, t, temp2, EXPR_REF(-2));
  tree res;


  TREE_SIDE_EFFECTS (t1) = 1;
  TREE_SIDE_EFFECTS (t2) = 1;
  res  = m3_build3 (COND_EXPR, unsigned_type(t),
		    m3_build2 (GE_EXPR, t, temp1, v_zero),
		    m3_do_shift (temp2, temp1, 0, t),
		    m3_do_shift (temp2, m3_build1 (NEGATE_EXPR, t, temp1),
                                 1, t));
  EXPR_REF(-2) = m3_build2 (COMPOUND_EXPR, unsigned_type(t),
			     m3_build2 (COMPOUND_EXPR, t, t1, t2), res);
  EXPR_POP();
}

static void
m3cg_shift_left (void) /* GCC32OK */
{
  MTYPE (t);

  EXPR_REF (-2) = m3_do_shift (EXPR_REF (-2), EXPR_REF (-1), 0, t);
  EXPR_POP ();
}

static void
m3cg_shift_right (void) /* GCC32OK */
{
  MTYPE (t);

  EXPR_REF (-2) = m3_do_shift (EXPR_REF (-2), EXPR_REF (-1), 1, t);
  EXPR_POP ();
}

static void
m3cg_m3_rotate (void) /* GCC32OK */
{
  MTYPE (t);

  tree temp1 = declare_temp (t);
  tree temp2 = declare_temp (t);
  tree t1    = m3_build2 (MODIFY_EXPR, t, temp1, EXPR_REF(-1));
  tree t2    = m3_build2 (MODIFY_EXPR, t, temp2, EXPR_REF(-2));
  tree res;

  TREE_SIDE_EFFECTS (t1) = 1;
  TREE_SIDE_EFFECTS (t2) = 1;
  res = m3_build3 (COND_EXPR, t,
		   m3_build2 (GE_EXPR, t, temp1, v_zero),
		   m3_do_rotate (temp2, temp1, 0, t),
		   m3_do_rotate (temp2, m3_build1 (NEGATE_EXPR, t_int, temp1),
                                 1, t));
  EXPR_REF(-2) = m3_build2 (COMPOUND_EXPR, t,
                            m3_build2 (COMPOUND_EXPR, t, t1, t2), res);
  EXPR_POP();
}

static void
m3cg_rotate_left (void) /* GCC32OK */
{
  MTYPE (t);

  EXPR_REF (-2) = m3_do_rotate (EXPR_REF (-2), EXPR_REF (-1), 0, t);
  EXPR_POP ();
}

static void
m3cg_rotate_right (void) /* GCC32OK */
{
  MTYPE (t);

  EXPR_REF (-2) = m3_do_rotate (EXPR_REF (-2), EXPR_REF (-1), 1, t);
  EXPR_POP ();
}

static void
m3cg_widen (void) /* GCC32OK */
{
  BOOLEAN (sign);

  tree dst_t = (sign ? t_int_64 : t_word_64);
  tree src_t  = (sign ? t_int_32 : t_word_32);

  EXPR_REF(-1) = m3_build1 (CONVERT_EXPR, dst_t, 
                            m3_cast (src_t, EXPR_REF(-1)));
}

static void
m3cg_chop (void) /* GCC32OK */
{
  EXPR_REF(-1) = m3_build1 (CONVERT_EXPR, t_int_32,
			    m3_build2 (BIT_AND_EXPR, t_int_64, EXPR_REF(-1),
				       m3_build_int (0xffffffff)));
}

static void
m3cg_m3_extract (void) /* GCC32OK */
{
  MTYPE   (t);
  BOOLEAN (sign_extend);

  EXPR_REF (-3) = m3_do_extract (EXPR_REF (-3), EXPR_REF (-2), EXPR_REF (-1),
                                 t, sign_extend);
  EXPR_POP ();
  EXPR_POP ();
}

static void
m3cg_extract_n (void) /* GCC32OK */
{
  MTYPE   (t);
  BOOLEAN (sign_extend);
  INTEGER (n);

  EXPR_REF (-2) = m3_do_extract (EXPR_REF (-2), EXPR_REF (-1),
                                 m3_build_int (n), t, sign_extend);
  EXPR_POP ();
}

static void
m3cg_extract_mn (void) /* GCC32OK */
{
  MTYPE   (t);
  BOOLEAN (sign_extend);
  INTEGER (m);
  INTEGER (n);

  EXPR_REF (-1) = m3_do_fixed_extract (EXPR_REF (-1), m, n, t, sign_extend);
}

static void
m3cg_m3_insert (void) /* GCC32OK */
{
  MTYPE (t);

  EXPR_REF (-4) = m3_do_insert (EXPR_REF (-4), EXPR_REF (-3),
                                EXPR_REF (-2), EXPR_REF (-1), t);
  EXPR_POP ();
  EXPR_POP ();
  EXPR_POP ();
}

static void
m3cg_insert_n (void) /* GCC32OK */
{
  MTYPE   (t);
  INTEGER (n);

  EXPR_REF (-3) = m3_do_insert (EXPR_REF (-3), EXPR_REF (-2),
                                EXPR_REF (-1), m3_build_int (n), t);
  EXPR_POP ();
  EXPR_POP ();
}

static void
m3cg_insert_mn (void) /* GCC32OK */
{
  MTYPE   (t);
  INTEGER (m);
  INTEGER (n);

  EXPR_REF (-2) = m3_do_fixed_insert (EXPR_REF (-2), EXPR_REF (-1), m, n, t);
  EXPR_POP ();
}

static void
m3cg_swap (void) /* GCC32OK */
{
  UNUSED_MTYPE (t);
  UNUSED_MTYPE (u);

  m3_swap ();
}

static void
m3cg_pop (void) /* GCC32OK */
{
  UNUSED_MTYPE (t);

  tree expr = EXPR_REF (-1);
  EXPR_POP ();
  TREE_SIDE_EFFECTS (expr) = 1;
  expand_expr_stmt (expr);
}

static void
m3cg_copy_n (void) /* GCC32OK */
{
  MTYPE (cnt_t);
  MTYPE (mem_t);
  BOOLEAN (overlap);

  if (cnt_t != t_int) { 
    fatal_error ("copy_n called with non-native integer count");
  }

  m3_start_call ();

  /* rearrange the parameters */
  {
    tree tmp = EXPR_REF (-3);
    EXPR_REF (-3) = EXPR_REF (-2);
    EXPR_REF (-2) = EXPR_REF (-1);
    EXPR_REF (-1) = tmp;
  }

  m3_pop_param (t_addr);
  m3_swap ();
  m3_pop_param (t_addr);

  EXPR_REF (-1) = 
    m3_build2 (MULT_EXPR, t_int,
               EXPR_REF (-1),
               size_int (TREE_INT_CST_LOW (TYPE_SIZE (mem_t)) / 
                         BITS_PER_UNIT));
  m3_pop_param (cnt_t);
  m3_call_direct (overlap ? memmove_proc : memcpy_proc, t_void);
}

static void
m3cg_copy (void) /* GCC32OK */
{
  INTEGER (n);
  MTYPE2  (t, T);
  UNUSED_BOOLEAN (overlap);

  tree pts;
  tree ts = make_node (LANG_TYPE);
  int s = n * TREE_INT_CST_LOW (TYPE_SIZE (t));

  TYPE_SIZE (ts) = size_int (s);
  TYPE_SIZE_UNIT (ts) = size_binop (FLOOR_DIV_EXPR, TYPE_SIZE(ts),
                                    size_int(BITS_PER_UNIT));
  TYPE_ALIGN (ts) = TYPE_ALIGN (t);

  if (IS_REAL_TYPE(T)) {
    TYPE_MODE (ts) = mode_for_size (s, MODE_FLOAT, 0);
  } else {
    TYPE_MODE (ts) = BLKmode;
  }

  pts = build_pointer_type (ts);

  expand_assignment (m3_build1 (INDIRECT_REF, ts,
                                m3_cast (pts, EXPR_REF (-2))),
                     m3_build1 (INDIRECT_REF, ts,
                                m3_cast (pts, EXPR_REF (-1))),
                     0, 0);
  EXPR_POP ();
  EXPR_POP ();
}

static void
m3cg_zero_n (void) /* GCC32OK */
{
  MTYPE (cnt_t);
  MTYPE (mem_t);

  int chunk_size = TREE_INT_CST_LOW (TYPE_SIZE (mem_t)) / BITS_PER_UNIT; 

  if (cnt_t != t_int) { fatal_error ("zero_n called with non-native count"); }

  if (chunk_size > 1) {
    EXPR_REF(-1) = m3_build2(MULT_EXPR, cnt_t, EXPR_REF(-1),
			     m3_build_int (chunk_size));
  }

  m3_start_call ();
  m3_swap ();
  m3_pop_param (t_addr);
  m3_pop_param (cnt_t);
  EXPR_PUSH (v_zero);
  m3_pop_param (t_int);
  m3_call_direct (memset_proc, t_void);
}

static void
m3cg_zero (void) /* GCC32OK */
{
  INTEGER (n);
  MTYPE   (mem_t);

  int chunk_size = TREE_INT_CST_LOW (TYPE_SIZE (mem_t)) / BITS_PER_UNIT; 

  m3_start_call ();
  m3_pop_param (t_addr);
  EXPR_PUSH (v_zero);
  m3_pop_param (t_int);
  EXPR_PUSH (size_int (n * chunk_size));
  m3_pop_param (t_int);
  m3_call_direct (memset_proc, t_void);
}

static void
m3cg_loophole (void) /* GCC32OK */
{
  MTYPE2 (t, T);
  MTYPE2 (u, U);

  if (IS_REAL_TYPE(T) != IS_REAL_TYPE(U)) {
    tree v = declare_temp (t);
    m3_store (v, 0, t, t);
    m3_load (v, 0, u, U, u, U);
  } else {
    EXPR_REF (-1) = m3_cast (u, EXPR_REF (-1));
  }
}

static void
m3cg_abort (void) /* GCC32OK */
{
  INTEGER (code);

  generate_fault (code);
}

static void
m3cg_check_nil (void) /* GCC32OK */
{
  INTEGER (code);

  tree temp1 = declare_temp (t_addr);

  m3_store (temp1, 0, t_addr, t_addr);
  EXPR_PUSH (temp1);
  expand_start_cond (m3_build2 (EQ_EXPR, t_addr, temp1, v_null), 0);
  generate_fault (code);
  expand_end_cond ();
}

static void
m3cg_check_lo (void) /* GCC32OK */
{
  MTYPE2         (t, m3t);
  TARGET_INTEGER (a);
  INTEGER        (code);

  tree temp1 = declare_temp (t);

  if (option_exprs_trace) {
    fprintf (stderr, "  check low type %d code %ld\n", m3t, code);
  }
  if (TREE_TYPE (EXPR_REF (-1)) != t) {
    EXPR_REF (-1) = m3_build1 (CONVERT_EXPR, t, EXPR_REF (-1));
  }
  m3_store (temp1, 0, t, t);
  EXPR_PUSH (temp1);
  expand_start_cond (m3_build2 (LT_EXPR, t_int, temp1, a), 0);
  generate_fault (code);
  expand_end_cond ();
}

static void
m3cg_check_hi (void) /* GCC32OK */
{
  MTYPE2         (t, m3t);
  TARGET_INTEGER (a);
  INTEGER        (code);

  tree temp1 = declare_temp (t);

  if (option_exprs_trace) {
    fprintf (stderr, "  check high type %d code %ld\n", m3t, code);
  }
  if (TREE_TYPE (EXPR_REF (-1)) != t) {
    EXPR_REF (-1) = m3_build1 (CONVERT_EXPR, t, EXPR_REF (-1));
  }
  m3_store (temp1, 0, t, t);
  EXPR_PUSH (temp1);
  expand_start_cond (m3_build2 (GT_EXPR, t_int, temp1, a), 0);
  generate_fault (code);
  expand_end_cond ();
}

static void
m3cg_check_range (void) /* GCC32OK */
{
  MTYPE2         (t, m3t);
  TARGET_INTEGER (a);
  TARGET_INTEGER (b);
  INTEGER        (code);

  tree temp1 = declare_temp (t);

  if (option_exprs_trace) {
    fprintf (stderr, "  check range type %d code %ld\n", m3t, code);
  }
  if (TREE_TYPE (EXPR_REF (-1)) != t) {
    EXPR_REF (-1) = m3_build1 (CONVERT_EXPR, t, EXPR_REF (-1));
  }
  m3_store (temp1, 0, t, t);
  EXPR_PUSH (temp1);
  expand_start_cond (m3_build2 (TRUTH_ORIF_EXPR, t_int,
				m3_build2 (LT_EXPR, t_int, temp1, a),
				m3_build2 (GT_EXPR, t_int, temp1, b)), 
		     0);
  generate_fault (code);
  expand_end_cond ();
}

static void
m3cg_check_index (void) /* GCC32OK */
{
  MTYPE   (t);
  INTEGER (code);

  t = unsigned_type (t);
  expand_start_cond (m3_build2 (GE_EXPR, t,
				m3_build1 (CONVERT_EXPR, t, EXPR_REF(-2)),
				m3_build1 (CONVERT_EXPR, t, EXPR_REF(-1))),
		     0);
  generate_fault (code);
  expand_end_cond ();
  EXPR_POP();
}

static void
m3cg_check_eq (void) /* GCC32OK */
{
  MTYPE   (t);
  INTEGER (code);

  tree temp1 = declare_temp (t);
  tree temp2 = declare_temp (t);

  m3_store (temp1, 0, t, t);
  m3_store (temp2, 0, t, t);
  EXPR_PUSH (temp2);
  EXPR_PUSH (temp1);
  expand_start_cond (m3_build2 (NE_EXPR, t_int, temp1, temp2), 0);
  generate_fault (code);
  expand_end_cond ();
  EXPR_POP ();
  EXPR_POP ();
}

static void
m3cg_add_offset (void) /* GCC32OK */
{
  BYTESIZE (n);

  if (option_vars_trace) {
    fprintf(stderr, "  add offset %ld\n", n);
  }
  EXPR_REF (-1) = m3_build2 (PLUS_EXPR, t_addr,
                             EXPR_REF (-1), size_int (n / BITS_PER_UNIT));
}

static void
m3cg_index_address (void) /* GCC32OK */
{
  MTYPE2   (t, m3t);
  BYTESIZE (n);

  HOST_WIDE_INT incr_val;
  int n_bytes = n / BITS_PER_UNIT;
  tree incr = EXPR_REF (-1);

  if (option_vars_trace) {
    fprintf(stderr, "  index address n %ld n_bytes %d type %d\n",
            n, n_bytes, m3t);
  }
  if (n_bytes != 1) {
    if (m3_is_small_cardinal (incr, &incr_val)
	&& (0 <= incr_val) && (incr_val < 1024)
	&& (0 <= n_bytes) && (n_bytes < 1024)) {
      incr = size_int (incr_val * n_bytes);
    } else {
      incr = m3_build2 (MULT_EXPR, t, incr, size_int (n_bytes));
    }
  };

  EXPR_REF (-2) = m3_build2 (PLUS_EXPR, t_addr,
                             m3_cast (t_addr, EXPR_REF (-2)),
                             incr);
  EXPR_POP ();
}

static void
m3cg_start_call_direct (void) /* GCC32OK */
{
  UNUSED_PROC    (p);
  INTEGER (level);
  UNUSED_MTYPE2  (t, m3t);

  if (option_procs_trace)
    fprintf(stderr, "  start call procedure %s, level %ld, type %d\n",
            IDENTIFIER_POINTER(DECL_NAME(p)), level, m3t);
#if 0
  /** 4/30/96 -- WKK -- It seems gcc can't hack small return values... */
  if (IS_INTEGER_TYPE(m3t) && 
      TYPE_SIZE(t) <= TYPE_SIZE(t_int)) {
    t = t_int;
  }
  if (IS_WORD_TYPE(m3t) && TYPE_SIZE(t) <= TYPE_SIZE(t_word)) {
    t = t_word;
  }
#endif
  m3_start_call ();
}

static void
m3cg_call_direct (void) /* GCC32OK */
{
  PROC  (p);
  MTYPE2  (t, m3t);

  if (option_procs_trace)
    fprintf(stderr, "  call procedure %s, type %d\n",
            IDENTIFIER_POINTER(DECL_NAME(p)), m3t);
#if M3CG_ADAPT_RETURN_TYPE2
  /** 4/30/96 -- WKK -- It seems gcc can't hack small return values... */
  if (IS_INTEGER_TYPE(m3t) && 
      TYPE_SIZE(t) <= TYPE_SIZE(t_int)) {
    t = t_int;
  }
  if (IS_WORD_TYPE(m3t) && TYPE_SIZE(t) <= TYPE_SIZE(t_word)) {
    t = t_word;
  }
#endif
  m3_call_direct (p, t);
}

static void
m3cg_start_call_indirect (void) /* GCC32OK */
{
  UNUSED_MTYPE2 (t, m3t);
  UNUSED_INTEGER (call_conv);

  if (option_procs_trace)
    fprintf(stderr, "  start call procedure indirect, type %d\n", m3t);
  m3_start_call ();
}

static void
m3cg_call_indirect (void) /* GCC32OK */
{
  MTYPE2 (t, m3t);
  UNUSED_INTEGER (call_conv);

  if (option_procs_trace)
    fprintf(stderr, "  call procedure indirect, type %d\n", m3t);
#if M3CG_ADAPT_RETURN_TYPE2
  /** 4/30/96 -- WKK -- It seems gcc can't hack small return values... */
  if (IS_INTEGER_TYPE(m3t) && 
      TYPE_SIZE(t) <= TYPE_SIZE(t_int)) {
    t = t_int;
  }
  if (IS_WORD_TYPE(m3t) && TYPE_SIZE(t) <= TYPE_SIZE(t_word)) {
    t = t_word;
  }
#endif
  m3_call_indirect (t);
}

static void
m3cg_pop_param (void) /* GCC32OK */
{
  MTYPE2 (t, m3t);

  if (option_vars_trace)
    fprintf(stderr, "  pop param type %d\n", m3t);
  m3_pop_param (t);
}

static void
m3cg_pop_struct (void) /* GCC32OK */
{
  BYTESIZE  (s);
  ALIGNMENT (a);

  tree t = m3_build_type (T_struct, s, a);
  if (option_vars_trace)
    fprintf(stderr, "  pop struct size %ld alignment %ld\n", s, a);
  EXPR_REF (-1) = m3_build1 (INDIRECT_REF, t,
                             m3_cast (build_pointer_type (t), EXPR_REF (-1)));
  m3_pop_param (t);
}

static void
m3cg_pop_static_link (void) /* GCC32OK */
{
  tree v = declare_temp (t_addr);
  m3_store (v, 0, TREE_TYPE (v), t_addr);
  CALL_TOP_STATIC_CHAIN () = v;
}

static void
m3cg_load_procedure (void) /* GCC32OK */
{
  PROC (p);

  if (option_procs_trace)
    fprintf(stderr, "  load procedure %s\n", IDENTIFIER_POINTER(DECL_NAME(p)));
  EXPR_PUSH (proc_addr (p, 1));
}

static void
m3cg_load_static_link (void) /* GCC32OK */
{
  /* This is ignored now that we use gcc's built-in nested function
     support. */
  PROC (p);
  if (option_procs_trace)
    fprintf(stderr, "  load link %s\n",
            IDENTIFIER_POINTER(DECL_NAME(p)));
  EXPR_PUSH (build (RTL_EXPR, t_addr, NULL_TREE, lookup_static_chain (p)));;
}

static void
m3cg_comment (void) /* GCC32OK */
{
  UNUSED_QUOTED_STRING (comment, len);
  if (option_misc_trace)
    fprintf(stderr, "  comment: `%s'\n", comment);
}

/*----------------------------------------------------------- M3CG parser ---*/

typedef void (*OP_HANDLER) (void);
typedef struct { M3CG_opcode op;  OP_HANDLER proc; } OpProc;

OpProc ops[] = {
  { M3CG_BEGIN_UNIT,             m3cg_begin_unit             },
  { M3CG_END_UNIT,               m3cg_end_unit               },
  { M3CG_IMPORT_UNIT,            m3cg_import_unit            },
  { M3CG_EXPORT_UNIT,            m3cg_export_unit            },
  { M3CG_SET_SOURCE_FILE,        m3cg_set_source_file        },
  { M3CG_SET_SOURCE_LINE,        m3cg_set_source_line        },
  { M3CG_DECLARE_TYPENAME,       m3cg_declare_typename       },
  { M3CG_DECLARE_ARRAY,          m3cg_declare_array          },
  { M3CG_DECLARE_OPEN_ARRAY,     m3cg_declare_open_array     },
  { M3CG_DECLARE_ENUM,           m3cg_declare_enum           },
  { M3CG_DECLARE_ENUM_ELT,       m3cg_declare_enum_elt       },
  { M3CG_DECLARE_PACKED,         m3cg_declare_packed         },
  { M3CG_DECLARE_RECORD,         m3cg_declare_record         },
  { M3CG_DECLARE_FIELD,          m3cg_declare_field          },
  { M3CG_DECLARE_SET,            m3cg_declare_set            },
  { M3CG_DECLARE_SUBRANGE,       m3cg_declare_subrange       },
  { M3CG_DECLARE_POINTER,        m3cg_declare_pointer        },
  { M3CG_DECLARE_INDIRECT,       m3cg_declare_indirect       },
  { M3CG_DECLARE_PROCTYPE,       m3cg_declare_proctype       },
  { M3CG_DECLARE_FORMAL,         m3cg_declare_formal         },
  { M3CG_DECLARE_RAISES,         m3cg_declare_raises         },
  { M3CG_DECLARE_OBJECT,         m3cg_declare_object         },
  { M3CG_DECLARE_METHOD,         m3cg_declare_method         },
  { M3CG_DECLARE_OPAQUE,         m3cg_declare_opaque         },
  { M3CG_REVEAL_OPAQUE,          m3cg_reveal_opaque          },
  { M3CG_DECLARE_EXCEPTION,      m3cg_declare_exception      },
  { M3CG_SET_RUNTIME_PROC,       m3cg_set_runtime_proc       },
  { M3CG_SET_RUNTIME_HOOK,       m3cg_set_runtime_hook       },
  { M3CG_IMPORT_GLOBAL,          m3cg_import_global          },
  { M3CG_DECLARE_SEGMENT,        m3cg_declare_segment        },
  { M3CG_BIND_SEGMENT,           m3cg_bind_segment           },
  { M3CG_DECLARE_GLOBAL,         m3cg_declare_global         },
  { M3CG_DECLARE_CONSTANT,       m3cg_declare_constant       },
  { M3CG_DECLARE_LOCAL,          m3cg_declare_local          },
  { M3CG_DECLARE_PARAM,          m3cg_declare_param          },
  { M3CG_DECLARE_TEMP,           m3cg_declare_temp           },
  { M3CG_FREE_TEMP,              m3cg_free_temp              },
  { M3CG_BEGIN_INIT,             m3cg_begin_init             },
  { M3CG_END_INIT,               m3cg_end_init               },
  { M3CG_INIT_INT,               m3cg_init_int               },
  { M3CG_INIT_PROC,              m3cg_init_proc              },
  { M3CG_INIT_LABEL,             m3cg_init_label             },
  { M3CG_INIT_VAR,               m3cg_init_var               },
  { M3CG_INIT_OFFSET,            m3cg_init_offset            },
  { M3CG_INIT_CHARS,             m3cg_init_chars             },
  { M3CG_INIT_FLOAT,             m3cg_init_float             },
  { M3CG_IMPORT_PROCEDURE,       m3cg_import_procedure       },
  { M3CG_DECLARE_PROCEDURE,      m3cg_declare_procedure      },
  { M3CG_BEGIN_PROCEDURE,        m3cg_begin_procedure        },
  { M3CG_END_PROCEDURE,          m3cg_end_procedure          },
  { M3CG_BEGIN_BLOCK,            m3cg_begin_block            },
  { M3CG_END_BLOCK,              m3cg_end_block              },
  { M3CG_NOTE_PROCEDURE_ORIGIN,  m3cg_note_procedure_origin  },
  { M3CG_SET_LABEL,              m3cg_set_label              },
  { M3CG_JUMP,                   m3cg_m3_jump                },
  { M3CG_IF_TRUE,                m3cg_if_true                },
  { M3CG_IF_FALSE,               m3cg_if_false               },
  { M3CG_IF_EQ,                  m3cg_if_eq                  },
  { M3CG_IF_NE,                  m3cg_if_ne                  },
  { M3CG_IF_GT,                  m3cg_if_gt                  },
  { M3CG_IF_GE,                  m3cg_if_ge                  },
  { M3CG_IF_LT,                  m3cg_if_lt                  },
  { M3CG_IF_LE,                  m3cg_if_le                  },
  { M3CG_CASE_JUMP,              m3cg_case_jump              },
  { M3CG_EXIT_PROC,              m3cg_exit_proc              },
  { M3CG_LOAD,                   m3cg_load                   },
  { M3CG_LOAD_ADDRESS,           m3cg_load_address           },
  { M3CG_LOAD_INDIRECT,          m3cg_load_indirect          },
  { M3CG_STORE,                  m3cg_store                  },
  { M3CG_STORE_INDIRECT,         m3cg_store_indirect         },
  { M3CG_LOAD_NIL,               m3cg_load_nil               },
  { M3CG_LOAD_INTEGER,           m3cg_load_integer           },
  { M3CG_LOAD_FLOAT,             m3cg_load_float             },
  { M3CG_EQ,                     m3cg_eq                     },
  { M3CG_NE,                     m3cg_ne                     },
  { M3CG_GT,                     m3cg_gt                     },
  { M3CG_GE,                     m3cg_ge                     },
  { M3CG_LT,                     m3cg_lt                     },
  { M3CG_LE,                     m3cg_le                     },
  { M3CG_ADD,                    m3cg_add                    },
  { M3CG_SUBTRACT,               m3cg_subtract               },
  { M3CG_MULTIPLY,               m3cg_multiply               },
  { M3CG_DIVIDE,                 m3cg_divide                 },
  { M3CG_NEGATE,                 m3cg_negate                 },
  { M3CG_ABS,                    m3cg_abs                    },
  { M3CG_MAX,                    m3cg_max                    },
  { M3CG_MIN,                    m3cg_min                    },
  { M3CG_ROUND,                  m3cg_round                  },
  { M3CG_TRUNC,                  m3cg_trunc                  },
  { M3CG_FLOOR,                  m3cg_floor                  },
  { M3CG_CEILING,                m3cg_ceiling                },
  { M3CG_CVT_FLOAT,              m3cg_cvt_float              },
  { M3CG_DIV,                    m3cg_div                    },
  { M3CG_MOD,                    m3cg_mod                    },
  { M3CG_SET_UNION,              m3cg_set_union              },
  { M3CG_SET_DIFFERENCE,         m3cg_set_difference         },
  { M3CG_SET_INTERSECTION,       m3cg_set_intersection       },
  { M3CG_SET_SYM_DIFFERENCE,     m3cg_set_sym_difference     },
  { M3CG_SET_MEMBER,             m3cg_set_member             },
  { M3CG_SET_EQ,                 m3cg_set_eq                 },
  { M3CG_SET_NE,                 m3cg_set_ne                 },
  { M3CG_SET_LT,                 m3cg_set_lt                 },
  { M3CG_SET_LE,                 m3cg_set_le                 },
  { M3CG_SET_GT,                 m3cg_set_gt                 },
  { M3CG_SET_GE,                 m3cg_set_ge                 },
  { M3CG_SET_RANGE,              m3cg_set_range              },
  { M3CG_SET_SINGLETON,          m3cg_set_singleton          },
  { M3CG_NOT,                    m3cg_not                    },
  { M3CG_AND,                    m3cg_and                    },
  { M3CG_OR,                     m3cg_or                     },
  { M3CG_XOR,                    m3cg_xor                    },
  { M3CG_SHIFT,                  m3cg_m3_shift               },
  { M3CG_SHIFT_LEFT,             m3cg_shift_left             },
  { M3CG_SHIFT_RIGHT,            m3cg_shift_right            },
  { M3CG_ROTATE,                 m3cg_m3_rotate              },
  { M3CG_ROTATE_LEFT,            m3cg_rotate_left            },
  { M3CG_ROTATE_RIGHT,           m3cg_rotate_right           },
  { M3CG_WIDEN,                  m3cg_widen                  },
  { M3CG_CHOP,                   m3cg_chop                   },
  { M3CG_EXTRACT,                m3cg_m3_extract             },
  { M3CG_EXTRACT_N,              m3cg_extract_n              },
  { M3CG_EXTRACT_MN,             m3cg_extract_mn             },
  { M3CG_INSERT,                 m3cg_m3_insert              },
  { M3CG_INSERT_N,               m3cg_insert_n               },
  { M3CG_INSERT_MN,              m3cg_insert_mn              },
  { M3CG_SWAP,                   m3cg_swap                   },
  { M3CG_POP,                    m3cg_pop                    },
  { M3CG_COPY_N,                 m3cg_copy_n                 },
  { M3CG_COPY,                   m3cg_copy                   },
  { M3CG_ZERO_N,                 m3cg_zero_n                 },
  { M3CG_ZERO,                   m3cg_zero                   },
  { M3CG_LOOPHOLE,               m3cg_loophole               },
  { M3CG_ABORT,                  m3cg_abort                  },
  { M3CG_CHECK_NIL,              m3cg_check_nil              },
  { M3CG_CHECK_LO,               m3cg_check_lo               },
  { M3CG_CHECK_HI,               m3cg_check_hi               },
  { M3CG_CHECK_RANGE,            m3cg_check_range            },
  { M3CG_CHECK_INDEX,            m3cg_check_index            },
  { M3CG_CHECK_EQ,               m3cg_check_eq               },
  { M3CG_ADD_OFFSET,             m3cg_add_offset             },
  { M3CG_INDEX_ADDRESS,          m3cg_index_address          },
  { M3CG_START_CALL_DIRECT,      m3cg_start_call_direct      },
  { M3CG_CALL_DIRECT,            m3cg_call_direct            },
  { M3CG_START_CALL_INDIRECT,    m3cg_start_call_indirect    },
  { M3CG_CALL_INDIRECT,          m3cg_call_indirect          },
  { M3CG_POP_PARAM,              m3cg_pop_param              },
  { M3CG_POP_STRUCT,             m3cg_pop_struct             },
  { M3CG_POP_STATIC_LINK,        m3cg_pop_static_link        },
  { M3CG_LOAD_PROCEDURE,         m3cg_load_procedure         },
  { M3CG_LOAD_STATIC_LINK,       m3cg_load_static_link       },
  { M3CG_COMMENT,                m3cg_comment                },
  { LAST_OPCODE,                 0                              }
  };

int yyparse ()
{
  int op, i;

  /* first, verify the handler table is complete and consistent. */
  for (i = 0;  ops[i].proc != 0;  i++ ) {
    if (i != (int)ops[i].op) { fatal_error (" *** bad opcode table"); };
  }
  if (i != (int)LAST_OPCODE) { fatal_error (" *** bad opcode table"); };


  /* check the version stamp */
  i = get_int ();
  if (i != M3CG_Version) {
    fatal_error (" *** bad M3CG version stamp (%lx), expected %lx",
                 i, M3CG_Version);
  }

  op = (int)LAST_OPCODE;
  while (op != (int)M3CG_END_UNIT) {
    op = get_int ();
    if (op < 0 || (int)LAST_OPCODE <= op) {
      fatal_error (" *** bad opcode: %ld, at m3cg_lineno %d", op, m3cg_lineno);
    }
    if (option_opcodes_trace) { 
      fprintf (stderr, "(%d) %s\n", m3cg_lineno, M3CG_opnames[op]);
    }
    m3cg_lineno ++;
    ops[op].proc ();
  }

  return 0;
}
