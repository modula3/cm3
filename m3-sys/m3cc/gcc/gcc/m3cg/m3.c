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
#include "obstack.h"
#include "rtl.h"
#include "input.h"
#include "expr.h"

#include "m3cg.h"

extern rtx label_rtx PROTO((tree label));

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

static tree m3_cast          PROTO((tree, tree));
static tree m3_rtl           PROTO((rtx));
static tree m3_build1        PROTO((enum tree_code, tree, tree));
static tree m3_build2        PROTO((enum tree_code, tree, tree, tree));
static tree m3_build3        PROTO((enum tree_code, tree, tree, tree, tree));
static tree m3_build_int     PROTO (());
static int is_small_cardinal PROTO(());
extern char* current_function_name;

#define STREQ(a,b) (a[0] == b[0] ? strcmp (a, b) == 0 : 0)

/*=========================================================== IDENTIFIERS ===*/

struct lang_identifier {
  struct tree_identifier ignore; };

/*=============================================================== PARSING ===*/

#define BUFFER_SIZE 0x10000

static char input_buffer[BUFFER_SIZE];
static int  input_len    = 0;
static int  input_cursor = 0;
static int  input_eof    = 0;
static int  m3cg_lineno  = 0;

/* Stream for reading from the input file.  */
FILE *finput;

/*-------------------------------------------------------- buffer loading ---*/

char *
init_parse (filename)
     char *filename;
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
    pfatal_with_name (filename);
#if 0
#ifdef IO_BUFFER_SIZE
  setvbuf (finput, (char *) xmalloc (IO_BUFFER_SIZE), _IOFBF, IO_BUFFER_SIZE);
#endif
#endif

  init_lex ();

  return filename;
}

void finish_parse ()
{
  fclose (finput);
}

static void reload_buffer ()
{
  input_len = fread (input_buffer, 1, BUFFER_SIZE, finput);
  input_cursor = 0;
  input_eof = (input_len <= 0);
}

void init_lex ()
{
  reload_buffer ();
  set_identifier_size (sizeof (struct lang_identifier));
}

static long get_byte ()
{
  if (input_cursor >= input_len) {
    reload_buffer ();
    if (input_eof) return 0;
  };
  return (long)(input_buffer[input_cursor++] & 0xff);
};


#define INTEGER(x) long x = get_int()
static long get_int ()
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
static char *scan_quoted_string (length)
     long *length;
{
  long x, len;
  char *result;

  len = get_int ();
  if (length) *length = len;
  if (len <= 0) return 0;

  result = (char*) malloc (len + 1);
  for (x = 0; x < len; x++) {
    result[x] = (char) get_byte ();
  }
  result[len] = 0;
  return result;  
}

/*----------------------------------------------------------------- names ---*/

#define NAME(x) char *x = scan_string ()
static char *scan_string ()
{
  long len;
  return scan_quoted_string (&len);
}

/*----------------------------------------------------------------- types ---*/

typedef enum {
  T_word8,  T_int8,
  T_word16, T_int16,
  T_word32, T_int32,
  T_word64, T_int64,
  T_reel, T_lreel, T_xreel,
  T_addr, T_struct, T_void, T_LAST} type;

static tree
  t_word8,  t_int8,
  t_word16, t_int16,
  t_word32, t_int32,
  t_word64, t_int64, 
  t_reel, t_lreel, t_xreel,
  t_addr, t_void,
  t_int, t_word;

#define IS_WORD_TYPE(t) (t == T_word32 || t == T_word8 || t == T_word16 || t == T_word64)
#define IS_INTEGER_TYPE(t) (t == T_int32 || t == T_int8 || t == T_int16 || t == T_int64)
#define IS_REAL_TYPE(t) (t == T_reel || t == T_lreel || t == T_xreel)

#define TYPE(x) type x = scan_type ()
static type scan_type ()
{
  long i = get_int ();
  if ((i < 0) || (T_LAST <= i))
    fatal ("**** illegal type: %ld, at m3cg_lineno %d", i, m3cg_lineno);
  return (type) i;
}

static tree build_type (t, s, a)
     type t;
     int s;
     int a;
{
  switch (t) {
  case T_word8:    return t_word8;
  case T_int8:     return t_int8;
  case T_word16:   return t_word16;
  case T_int16:    return t_int16;
  case T_word32:   return t_word32;
  case T_int32:    return t_int32;
  case T_word64:   return t_word64;
  case T_int64:    return t_int64;
  case T_reel:     return t_reel;
  case T_lreel:    return t_lreel;
  case T_xreel:    return t_xreel;
  case T_addr:     return t_addr;
  case T_void:     return t_void;

  case T_struct: {
    tree ts = make_node (RECORD_TYPE);
    TYPE_NAME (ts) = NULL_TREE;
    TYPE_FIELDS (ts) = NULL_TREE;
    TYPE_SIZE (ts) = m3_build_int (s);
    TYPE_SIZE_UNIT (ts) = size_binop (FLOOR_DIV_EXPR, TYPE_SIZE(ts),
                                      size_int(BITS_PER_UNIT));
    TYPE_ALIGN (ts) = a;
    TYPE_MODE (ts) = mode_for_size (s, MODE_INT, 1);
    /* If structure's known alignment is less than what the scalar mode
       would need, and it matters, then stick with BLKmode.  */
    if (STRICT_ALIGNMENT && ! (a >= BIGGEST_ALIGNMENT || (a >= s))) {
      if (TYPE_MODE (ts) != BLKmode)
        /* If this is the only reason this type is BLKmode,
           then don't force containing types to be BLKmode.  */
        TYPE_NO_FORCE_BLK (ts) = 1;
      TYPE_MODE (ts) = BLKmode;
    }
    return ts;
  }

  default:  fatal ("**** cannot build type, m3cg_lineno %d", m3cg_lineno);
  } /*switch*/
}

#define MTYPE(x) tree x = scan_mtype (0)
#define MTYPE2(x,y) type y; tree x = scan_mtype (&y)

static tree scan_mtype (T)
     type *T;
{
  type TT = scan_type ();
  if (T) { *T = TT; }
  return build_type (TT, 0, 0);
}

/*----------------------------------------------------------------- signs ---*/

#define SIGN(x) char x = scan_sign ()
static char scan_sign ()
{
  long x = get_int ();
  switch (x) {
  case 0:  return 'P';  /* positive */
  case 1:  return 'N';  /* negative */
  case 2:  return 'U';  /* unknown */
  default: fatal ("bad sign: %ld, at m3cg_lineno %d", x, m3cg_lineno);
  };
}

/*-------------------------------------------------------------- integers ---*/

#define MAX_V_CACHE 257
static tree v_zero, v_one, v_null;
static tree v_cache[MAX_V_CACHE];

#define TARGET_INTEGER(x) tree x = scan_target_int ()

static tree scan_target_int ()
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
#define BITSIZE(x)   INTEGER(x)
#define BYTESIZE(x)  long x = 8 * get_int()
#define ALIGNMENT(x) long x = 8 * get_int()
#define FREQUENCY(x) INTEGER(x)
#define BIAS(x)      INTEGER(x)
#define BITOFFSET(x) INTEGER(x)
#define BYTEOFFSET(x) long x= 8 * get_int()

/*------------------------------------------------------------- type uids ---*/
/* Modula-3 type uids are unsiged 32-bit values.  They are passed as signed
   decimal integers in the intermediate code, but converted to 6-byte, base 62
   strings of characters from here to the debugger.  To avoid surprises downstream,
   these generated strings are legal C identifiers.  */

#define UID_SIZE 6

#define NO_UID -1

#define TYPEID(x)    long x = get_int ()

static void fmt_uid (x, buf)
  long x;
  char *buf;
{
  char *res;
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
    fatal ("bad uid -> identifier conversion!!"); }
}

/*----------------------------------------------------------------- float ---*/

#define FLOAT(x,fkind)  int fkind;  tree x = scan_float(&fkind)

#define REEL_BYTES  (FLOAT_TYPE_SIZE / 8)
#define LREEL_BYTES (DOUBLE_TYPE_SIZE / 8)
#define XREEL_BYTES (LONG_DOUBLE_TYPE_SIZE / 8)

static tree scan_float (fkind)
  int *fkind;
{
  long i = get_int ();
  long n_bytes;
  struct { double xx_align;  char z[XREEL_BYTES]; } data;
  tree tipe, result;
  REAL_VALUE_TYPE val;

  *fkind = i;
  switch (i) {
  case 0:  tipe = t_reel;  n_bytes = REEL_BYTES;  break;
  case 1:  tipe = t_lreel; n_bytes = LREEL_BYTES; break;
  case 2:  tipe = t_xreel; n_bytes = XREEL_BYTES; break;
  default:
    fatal ("*** invalid floating point value, precision = %ld, at m3cg_lineno %d",
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

#define BOOLEAN(x) int x=scan_boolean()

static int scan_boolean ()
{
  return (get_int () != 0);
}

/*------------------------------------------------------------- variables ---*/

#define VAR(x) tree x = scan_var (ERROR_MARK)
#define RETURN_VAR(x,code) tree x = scan_var (code)

static tree *all_vars = 0;
static long  max_vars = 0;

static expand_vars (goal)
     long goal;
{
  long n_new;
  tree *new_vars;

  n_new = (max_vars == 0 ? 512 : max_vars);
  while (n_new <= goal) { n_new += n_new; }
  new_vars = (tree *) malloc (n_new * sizeof (tree));
  memset (new_vars, 0, n_new * sizeof (tree));
  if (all_vars) {
    memcpy (new_vars, all_vars, max_vars * sizeof (tree));
    free (all_vars);
  }
  all_vars = new_vars;
  max_vars = n_new;
}

static tree scan_var (code)
     int code;
{
  long i = get_int ();
  tree v = 0;

  if (i <= 0) { return 0; }
  if (i >= max_vars) { expand_vars (i); }
  v = all_vars [i];

  if (code == ERROR_MARK) {
    if (v == 0) {
      fatal ("********** variable should already exist, v.%d, line %d", 
	       i, m3cg_lineno); 
      all_vars [i] = v = make_node (VAR_DECL);
    }
  } else {
    if (v != 0) {
      fatal ("********** variable should not already exist, v.%d, line %d",
	       i, m3cg_lineno); }
    all_vars [i] = v = make_node (code);
    DECL_NAME (all_vars [i]) = NULL_TREE;
  }
  
  return v;
}

/*------------------------------------------------------------ procedures ---*/

static tree *all_procs = 0;
static long  max_procs = 0;

static expand_procs (goal)
     long goal;
{
  long n_new;
  tree *new_procs;

  n_new = (max_procs == 0 ? 512 : max_procs);
  while (n_new <= goal) { n_new += n_new; }
  new_procs = (tree *) malloc (n_new * sizeof (tree));
  memset (new_procs, 0, n_new * sizeof (tree));
  if (all_procs) {
    memcpy (new_procs, all_procs, max_procs * sizeof (tree));
    free (all_procs);
  }
  all_procs = new_procs;
  max_procs = n_new;
}

#define PROC(x) tree x = scan_proc ()
static tree scan_proc ()
{
  long i = get_int ();

  if (i <= 0) { return 0; }
  if (i >= max_procs) { expand_procs (i); }
  if (all_procs[i] == 0) { all_procs[i] = make_node (FUNCTION_DECL); }
  return all_procs[i];
}


/*---------------------------------------------------------------- labels ---*/

static tree *all_labels;
static long  max_labels = 0;

static expand_labels (goal)
     long goal;
{
  long n_new;
  tree *new_labels;

  n_new = (max_labels == 0 ? 512 : max_labels);
  while (n_new <= goal) { n_new += n_new; }
  new_labels = (tree *) malloc (n_new * sizeof (tree));
  memset (new_labels, 0, n_new * sizeof (tree));
  if (all_labels) {
    memcpy (new_labels, all_labels, max_labels * sizeof (tree));
    free (all_labels);
  }
  all_labels = new_labels;
  max_labels = n_new;
}

#define LABEL(l) tree  l = scan_label()
static tree scan_label ()
{
  long i = get_int ();

  if (i < 0) { return 0; }
  if (i >= max_labels) { expand_labels (i); }
  if (all_labels[i] == 0) {
    all_labels[i] = build_decl (LABEL_DECL, NULL_TREE, t_addr);
  }
  return all_labels[i];
}  


/*========================================== insert, shift, rotate and co ===*/

static tree do_insert (x, y, i, n, t)
     tree x, y, i, n, t;
{
  tree a, b, c, d, e, f, g, h, j, k, l;

  t = unsigned_type (t);
  a = m3_build1 (BIT_NOT_EXPR, t, v_zero);
  b = m3_build2 (LSHIFT_EXPR, t, a, n);
  c = m3_build1 (BIT_NOT_EXPR, t, b);
  d = m3_build2 (BIT_AND_EXPR, t, y, c);
  e = m3_build2 (LSHIFT_EXPR, t, d, i);
  f = m3_build2 (LSHIFT_EXPR, t, c, i);
  g = m3_build1 (BIT_NOT_EXPR, t, f);
  h = m3_build2 (BIT_AND_EXPR, t, x, g);
  j = m3_build2 (BIT_IOR_EXPR, t, h, e);
  k = m3_build3 (COND_EXPR, t,
	           m3_build2 (EQ_EXPR, t_int, n, m3_build_int (BITS_PER_WORD)),
	           y, j);
  l = m3_build3 (COND_EXPR, t,
	           m3_build2 (EQ_EXPR, t_int, n, v_zero),
    	           x, k);
  return l;
}

static tree left_shift (t, i)
     tree t;
     int i;
{
  if (i) t = m3_build2 (LSHIFT_EXPR, t_word, t, m3_build_int (i));
  return t;
}

static tree do_fixed_insert (x, y, i, n, t)
     tree x, y, t;
     int i, n;
{
  HOST_WIDE_INT y_val;

  if ((i < 0) || (BITS_PER_WORD <= i) || (n < 0) || (BITS_PER_WORD <= n)) {
    return do_insert (x, y, m3_build_int (i), m3_build_int (n), t); }

  if (n == 0) return x;

  t = unsigned_type (t);

  if ((n == 1) && (i < sizeof(int) * 8)) {
    if (is_small_cardinal (y, &y_val)) {
      if (y_val & 1) {
	return m3_build2 (BIT_IOR_EXPR, t, x, m3_build_int(1 << i));
      } else {
	return m3_build2 (BIT_AND_EXPR, t, x, m3_build_int(~(1 << i)));
      }
    } else { /* non-constant, 1-bit value */
      tree a, b;
      a = m3_build2 (BIT_AND_EXPR, t, y, v_one);
      b = m3_build2 (BIT_AND_EXPR, t, x, m3_build_int (~(1 << i)));
      return m3_build2 (BIT_IOR_EXPR, t, b, left_shift (a, i)); }}
  else { /* multi-bit value */
    tree saved_bits, new_bits;
    if (i + n < sizeof(int) * 8) {
      int mask = (1 << n) - 1;
      saved_bits = m3_build_int (~(mask << i));
      if (is_small_cardinal (y, &y_val)) {
	new_bits = m3_build_int ((y_val & mask) << i); }
      else {
        new_bits = m3_build2 (BIT_AND_EXPR, t, y, m3_build_int (mask));
	new_bits = left_shift (new_bits, i); };
      }
    else if (n < sizeof(int) * 8) {
      int mask = (1 << n) - 1;
      tree a = m3_build_int (mask);
      if (is_small_cardinal (y, &y_val)) {
	new_bits = m3_build_int (y_val & mask); }
      else {
        new_bits = m3_build2 (BIT_AND_EXPR, t, y, m3_build_int (mask)); };
      new_bits = left_shift (new_bits, i);
      saved_bits = m3_build1 (BIT_NOT_EXPR, t, left_shift (a, i));
      }
    else { /* n >= sizeof(int)*8 */
      tree mask;
      mask = m3_build2 (LSHIFT_EXPR, t, m3_build_int(~0L), m3_build_int (n));
      mask = m3_build1 (BIT_NOT_EXPR, t, mask);
      new_bits = left_shift (m3_build2 (BIT_AND_EXPR, t, y, mask), i);
      saved_bits = m3_build1 (BIT_NOT_EXPR, t, left_shift (mask, i));
      };
    x = m3_build2 (BIT_AND_EXPR, t, x, saved_bits);
    return m3_build2 (BIT_IOR_EXPR, t, x, new_bits); }
}

static tree do_extract (x, i, n, t, sign_extend)
     tree x, i, n, t;
     int sign_extend;
{
  tree a, b, c, d, e, f;

  a = m3_build2 (MINUS_EXPR, t, m3_build_int (BITS_PER_WORD), n);
  b = m3_build2 (MINUS_EXPR, t, a, i);
  c = m3_build1 (CONVERT_EXPR, unsigned_type(t), x);
  d = m3_build2 (LSHIFT_EXPR, unsigned_type(t), c, b);
  e = m3_build2 (RSHIFT_EXPR, (sign_extend ? signed_type(t) : unsigned_type(t)),
		 d, a);
  f = m3_build3 (COND_EXPR, t,
	          m3_build2 (EQ_EXPR, t, n, v_zero),
	          v_zero, e);
  return f;
}

static tree do_fixed_extract (x, i, n, t, sign_extend)
     tree x, t;
     int i, n, sign_extend;
{
  int a = BITS_PER_WORD - n;
  int b = BITS_PER_WORD - n - i;
  tree c, d, e;

  if ((a < 0) || (BITS_PER_WORD <= a) || (b < 0) || (BITS_PER_WORD <= b)) {
    return do_extract (x, m3_build_int (i),
		       m3_build_int (n), t, sign_extend); }

  c = m3_build1 (CONVERT_EXPR, unsigned_type(t), x);
  d = (b == 0) ? c : m3_build2 (LSHIFT_EXPR, unsigned_type(t), c, m3_build_int (b));
  e = (a == 0) ? d : m3_build2 (RSHIFT_EXPR,
				(sign_extend ? signed_type(t) : unsigned_type(t)),
				     d, m3_build_int (a));
  return e;
}

static tree do_rotate (val, cnt, right, t)
     tree val, cnt, t;
     int right;
{
  tree a, b, c, d, e, f, g;

  t = unsigned_type(t);
  a = m3_build_int (BITS_PER_WORD - 1);
  b = m3_build2 (BIT_AND_EXPR, t, cnt, a);
  c = m3_build2 (MINUS_EXPR, t, m3_build_int (BITS_PER_WORD), b);
  d = m3_build1 (CONVERT_EXPR, t, val);
  e = m3_build2 (LSHIFT_EXPR, t, d, (right) ? c : b);
  f = m3_build2 (RSHIFT_EXPR, t, d, (right) ? b : c);
  g = m3_build2 (BIT_IOR_EXPR, t, e, f);
  return g;
}

static tree do_shift (val, cnt, right, t)
     tree val, cnt, t;
     int right;
{
  tree a, b, c, d;
  HOST_WIDE_INT cnt_val;

  t = unsigned_type (t);
  a = m3_build1 (CONVERT_EXPR, t, val);
  b = m3_build2 ((right) ? RSHIFT_EXPR : LSHIFT_EXPR, t, a, cnt);
  if (is_small_cardinal (cnt, &cnt_val)
      && (0 <= cnt_val) && (cnt_val < BITS_PER_WORD)) {
    return b; };
  c = m3_build2 (GE_EXPR, t, cnt, m3_build_int (BITS_PER_WORD));
  d = m3_build3 (COND_EXPR, t, c, v_zero, b);
  return d;
}

/*================================================= debugging information ===*/

static tree debug_fields = 0;
static char current_dbg_type_tag [100];
static int current_dbg_type_count1;
static int current_dbg_type_count2;
static int current_dbg_type_count3;

static void debug_tag VPROTO((char kind, long id, ...))
{
#ifndef ANSI_PROTOTYPES
  char kind;
  long id;
#endif
  va_list args;
  char *fmt;

  VA_START (args, id);

#ifndef ANSI_PROTOTYPES
  kind = va_arg (args, char);
  id = va_arg (args, long);
#endif

  current_dbg_type_tag [0] = 'M';
  current_dbg_type_tag [1] = kind;
  current_dbg_type_tag [2] = '_';
  fmt_uid (id, current_dbg_type_tag + 3);

  fmt = va_arg (args, char *);
  vsnprintf (current_dbg_type_tag + UID_SIZE + 3,
	     sizeof(current_dbg_type_tag) - (UID_SIZE + 3), fmt, args);
  va_end (args);
}

static void debug_field (name)
     char *name;
{
  tree f;

  f = make_node (FIELD_DECL);

  TREE_CHAIN (f) = debug_fields;
  debug_fields = f;

  DECL_ASSEMBLER_NAME (f) = DECL_NAME (f) = get_identifier (name);
  DECL_FIELD_BITPOS (f) = v_zero;

  TREE_TYPE (f) = t_int;
  DECL_SIZE (f) = v_one;
  DECL_ALIGN (f) = 1;
  DECL_MODE (f) = QImode;
}

static void debug_field_id (id)
     long id;
{
  char buf [UID_SIZE+1];
  fmt_uid (id, buf);
  debug_field (buf);
}

static void debug_field_fmt VPROTO((long id, ...))
{
#ifndef ANSI_PROTOTYPES
  long id;
#endif
  va_list args;
  char name [100];
  char *fmt;

  VA_START (args, id);

#ifndef ANSI_PROTOTYPES
  id = va_arg (args, long);
#endif

  fmt_uid (id, name);
  fmt = va_arg (args, char *);
  vsnprintf (name + UID_SIZE, sizeof(name) - UID_SIZE, fmt, args);
  va_end (args);

  debug_field (name);
}

static tree debug_struct ()
{
  tree t = make_node (RECORD_TYPE);
  TYPE_NAME (t) =
    build_decl (TYPE_DECL, get_identifier (current_dbg_type_tag), t);
  TYPE_FIELDS (t) = nreverse (debug_fields);
  debug_fields = 0;
  TYPE_SIZE (t) = v_one;
  TYPE_SIZE_UNIT (t) = size_binop (FLOOR_DIV_EXPR, TYPE_SIZE(t),
                                   size_int(BITS_PER_UNIT));
  TYPE_ALIGN (t) = 1;
  TYPE_MODE (t) = QImode;

  rest_of_decl_compilation (build_decl (TYPE_DECL, NULL_TREE, t), 0, 1, 0);
  return t;
}

/*===================================================== RUNTIME FUNCTIONS ===*/

static tree memcpy_proc;
static tree memmove_proc;
static tree memset_proc;

static tree div_proc;
static tree mod_proc;

static tree set_union_proc;
static tree set_diff_proc;
static tree set_inter_proc;
static tree set_sdiff_proc;
static tree set_eq_proc;
static tree set_ne_proc;
static tree set_gt_proc;
static tree set_ge_proc;
static tree set_lt_proc;
static tree set_le_proc;
static tree set_member_proc;
static tree set_range_proc;
static tree set_sing_proc;

static tree declare_external_proc (p, name, typ)
     tree p;
     char *name;
     tree typ;
{
  if (p == 0) {
    p = build_decl (FUNCTION_DECL, get_identifier (name), NULL_TREE);
    DECL_BUILT_IN (p) = 0; }
  else {
    DECL_ASSEMBLER_NAME (p) = DECL_NAME (p) = get_identifier (name); }

  TREE_TYPE (p) = typ;
  TREE_PUBLIC (p) = 1;
  TREE_THIS_VOLATILE (p) = 0;
  TREE_SIDE_EFFECTS (p) = 1;
  DECL_EXTERNAL (p) = 1;
  DECL_CONTEXT (p) = NULL_TREE;
  DECL_MODE (p) = FUNCTION_MODE;
  make_function_rtl (p);

  assemble_external (p);
  TREE_USED (p) = 1;

  return p;
}

void declare_runtime_functions ()
{
  tree t;

  t = build_function_type (t_addr,
	    tree_cons (NULL_TREE, t_addr,
	       tree_cons (NULL_TREE, t_addr,
		  tree_cons (NULL_TREE, t_int,
		     tree_cons (NULL_TREE, t_void, NULL_TREE)))));

  memcpy_proc = declare_external_proc (NULL_TREE, "memcpy", t);
  DECL_BUILT_IN (memcpy_proc) = 1;
  DECL_SET_FUNCTION_CODE (memcpy_proc, BUILT_IN_MEMCPY);

  memmove_proc = declare_external_proc (NULL_TREE, "memmove", t);

  t = build_function_type (t_addr,
	   tree_cons (NULL_TREE, t_addr,
	      tree_cons (NULL_TREE, t_int,
		 tree_cons (NULL_TREE, t_int,
		    tree_cons (NULL_TREE, t_void, NULL_TREE)))));
  memset_proc = declare_external_proc (NULL_TREE, "memset", t);

  t = build_function_type (t_int,
	   tree_cons (NULL_TREE, t_int,
	      tree_cons (NULL_TREE, t_int,
		 tree_cons (NULL_TREE, t_void, NULL_TREE))));
  div_proc = declare_external_proc (0, "m3_div", t);
  mod_proc = declare_external_proc (0, "m3_mod", t);

  t = build_function_type (t_void, NULL_TREE);
  set_union_proc  = declare_external_proc (0, "set_union", t);
  set_diff_proc   = declare_external_proc (0, "set_difference", t);
  set_inter_proc  = declare_external_proc (0, "set_intersection", t);
  set_sdiff_proc  = declare_external_proc (0, "set_sym_difference", t);
  set_sing_proc   = declare_external_proc (0, "set_singleton", t);
  set_range_proc  = declare_external_proc (0, "set_range", t);
  
  t = build_function_type (t_int, NULL_TREE);
  set_member_proc = declare_external_proc (0, "set_member", t);
  set_eq_proc = declare_external_proc (0, "set_eq", t);
  set_ne_proc = declare_external_proc (0, "set_ne", t);
  set_gt_proc = declare_external_proc (0, "set_gt", t);
  set_ge_proc = declare_external_proc (0, "set_ge", t);
  set_lt_proc = declare_external_proc (0, "set_lt", t);
  set_le_proc = declare_external_proc (0, "set_le", t);

}

/*================================================================ BLOCKS ===*/

static tree current_block = NULL_TREE;

typedef struct blocklist {
  tree head;
  struct blocklist *tail; } *blocklist;

static blocklist pending_blocks = 0;

static void m3_push_block (b)
     tree b;
{
  if (b == 0) {
    b = make_node (BLOCK);
    BLOCK_SUPERCONTEXT (b) = current_block;
    if (current_block) {
      BLOCK_SUBBLOCKS (current_block) 
	= chainon (BLOCK_SUBBLOCKS (current_block), b); }}
  else {
    struct blocklist *new = 
      (struct blocklist *) malloc (sizeof (struct blocklist));
    new->head = current_block;
    new->tail = pending_blocks;
    pending_blocks = new; }
  TREE_USED (b) = 1;
  current_block = b;
}

static void m3_pop_block (b)
     tree b;
{
  if (b == 0) {
    current_block = BLOCK_SUPERCONTEXT (current_block); }
  else {
    if (current_block != b) {
      abort (); }
    current_block = pending_blocks->head;
    pending_blocks = pending_blocks->tail; }
}

/*========================================== GLOBALS FOR THE M3CG MACHINE ===*/

static tree global_vars = NULL_TREE;
static int compiling_body;
static char *current_unit_name;
static tree current_segment;
static int max_lineno;

/* the stack */
static tree stack[100];
static int tos = 0;

/* the call stack */
static tree call_stack_args[1000];
static tree call_stack_link[1000];
static tree call_stack_argtypes[1000];
static int call_stack_top = 0;

/* the exported interfaces */
static int exported_interfaces;
static char *exported_interfaces_names [100];

static int ignore_params = 0;
/* are the following DECLARE_PARAMs for an IMPORT_PROCEDURE or 
   a DECLARE_PROCEDURE ? */

/*================================= SUPPORT FOR INITIALIZED DATA CREATION ===*/

static int current_record_offset;
static tree current_record_type;
static tree current_record_vals;

static void one_gap PROTO((int offset));

static void one_field (offset, size, f, v)
     int offset;
     int size;
     tree *f;
     tree *v;
{
  if (offset > current_record_offset) {
    one_gap (offset); }

  *f = make_node (FIELD_DECL);
  DECL_NAME (*f) = NULL_TREE;
  DECL_ASSEMBLER_NAME (*f) = NULL_TREE;
  DECL_ALIGN (*f) = 1;
  DECL_SIZE (*f) = m3_build_int (size);
  DECL_FIELD_BITPOS (*f) = m3_build_int (offset);
  DECL_CONTEXT (*f) = current_record_type;
  TREE_CHAIN (*f) = TYPE_FIELDS (current_record_type);
  TYPE_FIELDS (current_record_type) =  *f;

  *v = make_node (TREE_LIST);
  TREE_PURPOSE (*v) = *f;
  TREE_CHAIN (*v) = current_record_vals;
  current_record_vals = *v;

  current_record_offset = offset + size;
}

static void one_gap (offset)
     int offset;
{
  tree f, v;

  one_field (current_record_offset, offset - current_record_offset, &f, &v);

  TREE_TYPE (f) = make_node (LANG_TYPE);
  TYPE_SIZE (TREE_TYPE (f)) = DECL_SIZE (f);
  TYPE_ALIGN (TREE_TYPE (f)) = DECL_ALIGN (f); 
	    
  TREE_VALUE (v) = make_node (CONSTRUCTOR);
  CONSTRUCTOR_ELTS (TREE_VALUE (v)) = 0;
  TREE_TYPE (TREE_VALUE (v)) = TREE_TYPE (f);
}

/*========================================= SUPPORT FUNCTIONS FOR YYPARSE ===*/

static tree fix_type (v, t, s, a)
     tree v; 
     type t;
     int s;
     int a;
{
  TREE_TYPE (v) = build_type (t, s, a);
  DECL_SIZE (v) = m3_build_int (s);
  DECL_ALIGN (v) = a;
  DECL_MODE (v) = TYPE_MODE (TREE_TYPE (v));
}

static tree fix_name (name, id)
     char *name;
     long id;
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

static tree m3_cast (tipe, op0)
     tree tipe, op0;
{
  return fold (build1 (NOP_EXPR, tipe, op0));
}

static tree m3_rtl (x)
     rtx x;
{
  return build (RTL_EXPR, t_addr, NULL_TREE, x);
}

static tree m3_build1 (code, tipe, op0)
     enum tree_code code;
     tree tipe, op0;
{
  return fold (build1 (code, tipe, op0));
}

static tree m3_build2 (code, tipe, op0, op1)
     enum tree_code code;
     tree tipe, op0, op1;
{
  return fold (build (code, tipe, op0, op1));
}

static tree m3_build3 (code, tipe, op0, op1, op2)
     enum tree_code code;
     tree tipe, op0, op1, op2;
{
  return fold (build (code, tipe, op0, op1, op2));
}

static tree m3_build_real (value, tipe)
     char *value;
     tree tipe;
{
  tree x = make_node (REAL_CST);
  TREE_TYPE (x) = tipe;
  if (tipe == t_reel) {
    TREE_REAL_CST (x) = REAL_VALUE_ATOF (value, SFmode); }
  else if (tipe == t_lreel) {
    TREE_REAL_CST (x) = REAL_VALUE_ATOF (value, DFmode); }
  else if (tipe == t_xreel) {
    TREE_REAL_CST (x) = REAL_VALUE_ATOF (value, XFmode); }
  else {
    abort (); }
  return x;
}

static tree m3_build_int (n)
     int n;
{
  if ((0 <= n) && (n < MAX_V_CACHE)) {
    if (v_cache[n] == NULL_TREE) {
      v_cache[n] = build_int_2 (n, 0);
      if (n == 0) v_zero = v_cache[0];
      if (n == 1) v_one  = v_cache[1];
    }
    return v_cache[n];
  }

  if ((BITS_PER_WORD > sizeof (int) * 8) && (n < 0)) {
    return build_int_2 (n, ~0); }
  else {
    return build_int_2 (n, 0); }
}

static int is_small_cardinal (t, n)
     tree t;
     HOST_WIDE_INT *n;
{
  if (TREE_CODE (t) != INTEGER_CST || TREE_INT_CST_HIGH (t) != 0) return 0;
  *n = TREE_INT_CST_LOW (t);
  return 1;
}

static void compile_local (v)
     tree v;
{
  expand_decl (v); 
  rest_of_decl_compilation (v, 0, 0, 1);
}

static tree declare_temp (t, in_mem, v)
     tree t;
     int in_mem;
     tree v;
{      
  if (v == 0) {
    v = make_node (VAR_DECL);
    DECL_NAME (v) = NULL_TREE; }

  DECL_ASSEMBLER_NAME (v) = DECL_NAME (v);
  TREE_TYPE (v) = t;
  DECL_SIZE (v) = TYPE_SIZE (t);
  DECL_ALIGN (v) = TYPE_ALIGN (t);
  DECL_MODE (v) = TYPE_MODE (t);
  TREE_UNSIGNED (v) = TREE_UNSIGNED (t);
  TREE_ADDRESSABLE (v) = in_mem;
  DECL_REGISTER (v) = ! in_mem;
  
  DECL_CONTEXT (v) = current_function_decl;
  
  TREE_CHAIN (v) = BLOCK_VARS (BLOCK_SUBBLOCKS (DECL_INITIAL (current_function_decl)));
  BLOCK_VARS (BLOCK_SUBBLOCKS (DECL_INITIAL (current_function_decl))) = v;

  compile_local (v);
  return v;
}


static void m3_start_call ()
{
  call_stack_top++;
  call_stack_link     [call_stack_top] = NULL_TREE;
  call_stack_args     [call_stack_top] = NULL_TREE; 
  call_stack_argtypes [call_stack_top] = NULL_TREE;
}

static void m3_pop_param (t)
     tree t;
{
  call_stack_args [call_stack_top]
    = chainon (call_stack_args [call_stack_top], 
	       build_tree_list (NULL_TREE, stack [tos-1]));
  call_stack_argtypes [call_stack_top]
    = chainon (call_stack_argtypes [call_stack_top],
	       build_tree_list (NULL_TREE, t));
  tos--;
}

static void m3_call_direct (p, return_type)
     tree p;
     tree return_type;
{
  tree call;

  if (return_type == NULL_TREE) {
    return_type = TREE_TYPE (TREE_TYPE (p)); }

  call = build (CALL_EXPR, return_type,
                m3_build1 (ADDR_EXPR, build_pointer_type(TREE_TYPE(p)), p),
		call_stack_args [call_stack_top], NULL_TREE,
		call_stack_link [call_stack_top]);
  TREE_SIDE_EFFECTS (call) = 1;
  if (return_type == t_void) {
    expand_expr_stmt (call); }
  else {
    stack[tos++] = call; }
  call_stack_top--;
}

static void m3_call_indirect (t)
     tree t;
{
  tree argtypes = chainon (call_stack_argtypes [call_stack_top],
	                   tree_cons (NULL_TREE, t_void, NULL_TREE));
  tree fntype = build_pointer_type (build_function_type (t, argtypes));
  tree call = build (CALL_EXPR, t, 
		     m3_cast (fntype, stack [--tos]),
		     call_stack_args [call_stack_top], NULL_TREE,
		     call_stack_link [call_stack_top]);
  if (t == t_void) {
    TREE_SIDE_EFFECTS (call) = 1;
    expand_expr_stmt (call); }
  else {
    stack[tos++] = call; }
  call_stack_top--;
}

static void m3_swap ()
{
  tree tmp = stack [tos-1];
  stack [tos-1] = stack [tos-2];
  stack [tos-2] = tmp;
}

static void m3_load (v, o, src_t, src_T, dest_t, dest_T)
     tree v;
     int o;
     tree src_t, dest_t;
     type src_T, dest_T;
{
  if (o == 0 && TREE_TYPE (v) == src_t) {
    stack [tos++] = v;
  } else {
    tree adr = m3_build1 (ADDR_EXPR, t_addr, v);
    if (o != 0) {
      adr = m3_build2 (PLUS_EXPR, t_addr, adr, m3_build_int (o/8));
    }
    stack [tos++] = m3_build1 (INDIRECT_REF, src_t,
			         m3_cast (build_pointer_type (src_t), adr));
  }

  if (src_T != dest_T) {
    stack [tos-1] = m3_build1 (CONVERT_EXPR, dest_t, stack [tos-1]);
  }
}

static void m3_store (v, o, src_t, dest_t)
     tree v;
     int o;
     tree src_t, dest_t;
{
  tree lhs, rhs;

  if (TREE_TYPE (stack [tos-1]) == src_t) {
    rhs = stack [tos-1];
  } else {
    rhs = m3_cast (src_t, stack [tos-1]);
  }

  if (o == 0 && TREE_TYPE (v) == dest_t) {
    lhs = v;
  } else {
    tree f = make_node (FIELD_DECL);
    lhs = m3_build2 (COMPONENT_REF, dest_t, v, f);
    TREE_TYPE (f) = dest_t;
    DECL_ALIGN (f) = TYPE_ALIGN (dest_t);
    DECL_SIZE (f) = TYPE_SIZE (dest_t);
    DECL_MODE (f) = TYPE_MODE (dest_t);
    DECL_FIELD_BITPOS (f) = m3_build_int (o);
  }

  expand_assignment (lhs, m3_build1 (CONVERT_EXPR, dest_t, rhs), 0, 0);
  tos--;
}

#define binaryop(o,t) \
  do { \
    stack [tos-2] = m3_build2 (o, t, stack [tos-2], stack [tos-1]);  tos--; \
    } while (0)

#define unaryop(o,t) \
  do { \
    stack [tos-1] = m3_build1 (o, t, stack [tos-1]); \
    } while (0)

static void condop (o, l, t)
     enum tree_code o;
     tree l, t;
{
  tree t1 = m3_cast (t, stack [tos-1]);
  tree t2 = m3_cast (t, stack [tos-2]);
  TREE_UNSIGNED (t1) = TREE_UNSIGNED (t);
  TREE_UNSIGNED (t2) = TREE_UNSIGNED (t);

  do_jump (m3_build2 (o, t_int /* was t */, t2, t1),
	   NULL_RTX, label_rtx (l));
  tos -= 2;
}

static void setop (p, n, q)
     tree p;
     int n, q; 
{
  m3_start_call ();
  stack [tos++] = m3_build_int (n);
  m3_pop_param (t_int);
  while (q--) {
    m3_pop_param (t_addr);
  }
  m3_call_direct (p, NULL_TREE);
}

static void setop2 (p, q)
     tree p;
     int q; 
{
  m3_start_call ();
  while (q--) {
    m3_pop_param (t_addr); }
  m3_call_direct (p, NULL_TREE);
}

/*---------------------------------------------------------------- faults ---*/

static tree fault_proc = NULL_TREE;    /* internal fault handler    */
static tree fault_arg;                 /* (line# + code) parameter  */
static tree fault_handler = NULL_TREE; /* external fault handler */
static tree fault_intf;                /* global fault handler base */
static int  fault_offs;                /*   + offset                */

static declare_fault_proc ()
{
  tree proc       = make_node (FUNCTION_DECL);
  tree parm_block = make_node (BLOCK);
  tree top_block  = make_node (BLOCK);
  tree parm       = make_node (PARM_DECL);
  tree parm_types;

  DECL_NAME (proc) = DECL_ASSEMBLER_NAME (proc) = get_identifier ("_m3_fault");
  DECL_RESULT (proc) = build_decl (RESULT_DECL, NULL_TREE, t_void);
  DECL_CONTEXT (DECL_RESULT (proc)) = proc;
  TREE_STATIC (proc) = 1;
  TREE_PUBLIC (proc) = 0;
  DECL_CONTEXT (proc) = 0;

  DECL_NAME (parm) = DECL_ASSEMBLER_NAME (parm) = fix_name ("arg", 0x195c2a74);
  DECL_NONLOCAL (parm) = 0;
  TREE_ADDRESSABLE (parm) = 0;
  TREE_TYPE (parm) = t_word;
  DECL_SIZE (parm) = TYPE_SIZE (t_word);
  DECL_ALIGN (parm) = TYPE_ALIGN (t_word);
  DECL_MODE (parm) = TYPE_MODE (t_word);
  DECL_ARG_TYPE (parm) = t_word;

  parm_types = tree_cons (NULL_TREE, t_void, NULL_TREE);
  parm_types = tree_cons (NULL_TREE, TREE_TYPE (parm), parm_types);
  TREE_TYPE (proc) = build_function_type (t_void, parm_types);
  DECL_ARGUMENTS (proc) = nreverse (DECL_ARGUMENTS (proc));

  BLOCK_SUPERCONTEXT (parm_block) = proc;
  DECL_INITIAL (proc) = parm_block;
  TREE_USED (parm_block) = 1;

  BLOCK_SUPERCONTEXT (top_block) = parm_block;
  BLOCK_SUBBLOCKS (parm_block) = top_block;
  TREE_USED (top_block) = 1;

  make_function_rtl (proc);

  DECL_CONTEXT (parm) = proc;

  TREE_CHAIN (parm) = DECL_ARGUMENTS (proc);
  DECL_ARGUMENTS (proc) = parm;

  if (DECL_MODE (parm) == VOIDmode) {
	DECL_MODE (parm) = Pmode; }

  rest_of_decl_compilation (parm, 0, 0, 1);

  fault_proc = proc;
  fault_arg  = parm;
}

static emit_fault_proc ()
{
  lineno = max_lineno + 1;
  DECL_SOURCE_LINE (fault_proc) = lineno;
      
  current_function_decl = fault_proc;
  current_function_name = IDENTIFIER_POINTER (DECL_NAME (fault_proc));

  init_function_start (fault_proc, input_filename, lineno);
  expand_function_start (fault_proc, 0);
      
  m3_push_block (BLOCK_SUBBLOCKS (DECL_INITIAL (fault_proc))); 

  /* compile the locals we have already seen */
  { tree local;
    for (local = BLOCK_VARS (current_block);
	 local; local = TREE_CHAIN (local)) {
      compile_local (local); }}
      
  clear_last_expr ();
  expand_start_bindings (0);

  m3_start_call ();
  stack[tos++] = m3_build1 (ADDR_EXPR, t_addr, current_segment);
  m3_pop_param (t_addr);
  stack[tos++] = fault_arg;
  m3_pop_param (t_word);
  if (fault_handler != NULL_TREE) {
    m3_call_direct (fault_handler, t_void);
  } else {
    m3_load (fault_intf, fault_offs, t_addr, T_addr, t_addr, T_addr);
    m3_call_indirect (t_void);
  }
  emit_barrier ();
  expand_null_return ();

  expand_end_bindings (BLOCK_VARS (current_block), 1, 0);

  expand_function_end (input_filename, lineno, 0);
  rest_of_compilation (current_function_decl);

  m3_pop_block (BLOCK_SUBBLOCKS (DECL_INITIAL (fault_proc))); 
}

#define FAULT_MASK 0x1f
#define LINE_SHIFT 5

static generate_fault (code)
     int code;
{
  if (fault_proc == 0) declare_fault_proc ();
  m3_start_call ();
  stack[tos++] = m3_build_int ((lineno << LINE_SHIFT) + (code & FAULT_MASK));
  m3_pop_param (t_word);
  m3_call_direct (fault_proc, t_void);
  emit_barrier ();
}

/*================================================== ENTRY POINTS FOR GCC ===*/

/* One would expect that flag_traditional is only for the benefit of
   the C front-end, but dwarfout.c references it. */

int flag_traditional;

tree integer_type_node, void_type_node, char_type_node, null_pointer_node;
tree error_mark_node, integer_zero_node, integer_one_node;
tree current_function_decl;

char *language_string = "Critical Mass Modula-3";

/* If DECL has a cleanup, build and return that cleanup here.
   This is a callback called by expand_expr.  */

/*ARGSUSED*/
tree
maybe_build_cleanup (decl)
     tree decl;
{
  /* There are no cleanups in Modula-3.  */
  return NULL_TREE;
}

/*ARGSUSED*/
void print_lang_identifier (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
}

void print_lang_statistics () 
{
}

/*ARGSUSED*/
void print_lang_decl (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
  return;
}

void
lang_init_options ()
{
}

void
lang_print_xnode (file, node, indent)
     FILE *file ATTRIBUTE_UNUSED;
     tree node ATTRIBUTE_UNUSED;
     int indent ATTRIBUTE_UNUSED;
{
}

void print_lang_type (file, node, indent)
     FILE *file;
     tree node;
     int indent;
/*void print_lang_type ()*/
{
}

/*ARGSUSED*/
void incomplete_type_error (value, typ)
     tree value;
     tree typ;
{
  if (TREE_CODE (typ) == ERROR_MARK)
    return;
  fputs (" *** language-dependent function called: incomplete_type_error\n",
         stderr);
  fputs("type:\n", stderr);
  debug_tree(typ);
  fputs("value:\n", stderr);
  debug_tree(value);
  fatal (" *** language-dependent function called: incomplete_type_error");
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
void insert_block (block)
     tree block;
{
  fatal ("********* language-dependent function called: insert_block");
}


/* Nonzero if we are currently in the global binding level.  */
int global_bindings_p ()
{
  fatal ("********* language-dependent function called: global_bindings_p");
  /*NOTREACHED*/
}

/*ARGSUSED*/
void set_yydebug (value)
     int value;
{
  fatal ("********** language-dependent function called: set_yydebug");
}

char * lang_identify ()
{
  return "m3";
}

void lang_init () 
{
}

/*ARGSUSED*/
void copy_lang_decl (x)
     tree x;
{
}

void lang_finish () 
{
}

int lang_decode_option (n, p)
     int n;
     char **p;
/*int lang_decode_option (p)
  char *p; */
{
  char buf[1024];
  int i;
  buf[0] = '\000';
  for (i = 0; i < n; i++) {
    if (i > 0) {
      strcpy(buf, ", ");
    }
    strcpy(buf, p[i]);
  }
  fatal (" *** language-dependent function called: lang_decode_option: %s", buf);
  /*NOTREACHED*/
}

/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */
tree type_for_size (bits, unsignedp)
     unsigned bits;
     int unsignedp;
{
  if (unsignedp) {
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_word8))) {
      return t_word8; }	      
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_word16))) {
      return t_word16; }	      
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_word32))) {
      return t_word32; }	      
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_word64))) {
      return t_word64; }}      
  else {				      
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_int8))) {
      return t_int8; }	      
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_int16))) {
      return t_int16; }	      
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_int32))) {
      return t_int32; }	      
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_int64))) {
      return t_int64; }}

  fatal ("********* type_for_size, called for %d bits, unsignedp = %d", 
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
  if (TYPE_PRECISION (typ) == TYPE_PRECISION (t_int8))
    return unsignedp ? t_word8 : t_int8;
  if (TYPE_PRECISION (typ) == TYPE_PRECISION (t_int16))
    return unsignedp ? t_word16 : t_int16;
  if (TYPE_PRECISION (typ) == TYPE_PRECISION (t_int32))
    return unsignedp ? t_word32 : t_int32;
  if (TYPE_PRECISION (typ) == TYPE_PRECISION (t_int64))
    return unsignedp ? t_word64 : t_int64;
  return typ;
}

/* Return a data type that has machine mode MODE.
   If the mode is an integer,
   then UNSIGNEDP selects between signed and unsigned types.  */
tree type_for_mode (mode, unsignedp)
     enum machine_mode mode;
     int unsignedp;
{
  if (mode == TYPE_MODE (t_int64))  return unsignedp ? t_word64 : t_int64;
  if (mode == TYPE_MODE (t_int8))   return unsignedp ? t_word8 : t_int8;
  if (mode == TYPE_MODE (t_int16))  return unsignedp ? t_word16 : t_int16;
  if (mode == TYPE_MODE (t_int32))  return unsignedp ? t_word32 : t_int32;
  if (mode == TYPE_MODE (t_reel))   return t_reel;
  if (mode == TYPE_MODE (t_lreel))  return t_lreel;
  if (mode == TYPE_MODE (t_xreel))  return t_xreel;
  return 0;
}

/* convert a case label value to the proper type */
static tree convert_case (target_type, value)
     tree target_type, value;
{
  return value;
}

/* Return an unsigned type the same as TYPE in other respects.  */
tree unsigned_type (typ)
     tree typ;
{
  if (TREE_UNSIGNED (typ)) { return typ; }
  if (typ == t_int64) return t_word64;
  if (typ == t_int32) return t_word32;
  if (typ == t_int16) return t_word16;
  if (typ == t_int8)  return t_word8;
  fatal ("********* language-dependent function called: unsigned_type");
  /*NOTREACHED*/
}

/* Return a signed type the same as TYPE in other respects.  */
tree signed_type (typ)
     tree typ;
{
  if (!TREE_UNSIGNED (typ)) { return typ; }
  if (typ == t_word64) return t_int64;
  if (typ == t_word32) return t_int32;
  if (typ == t_word16) return t_int16;
  if (typ == t_word8)  return t_int8;
  fatal ("********* language-dependent function called: signed_type");
  /*NOTREACHED*/
}

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */
/*ARGSUSED*/
void set_block (block)
     tree block;
{
  fatal ("********* language-dependent function called: set_block");
}

/* Enter a new binding level.
   If TAG_TRANSPARENT is nonzero, do so only for the name space of variables,
   not for that of tags.  */
/*ARGSUSED*/
void pushlevel (tag_transparent)
     int tag_transparent;
{
  fatal ("********* language-dependent function called: pushlevel");
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
     int keep;
     int reverse;
     int functionbody;
{
  fatal ("********* language-dependent function called: poplevel");
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
     tree x;
{
  fatal ("********* language-dependent function called: pushdecl");
  /*NOTREACHED*/
}

void init_decl_processing ()
{
  /* int has to be first to properly initialize everything */
  t_int64  = make_signed_type (64);
  TYPE_NAME (t_int64)
    = build_decl (TYPE_DECL, get_identifier ("int_64"), t_int64);
  t_int32  = make_signed_type (32);
  TYPE_NAME (t_int32)
    = build_decl (TYPE_DECL, get_identifier ("int_32"), t_int32);
  t_int16  = make_signed_type (16);
  TYPE_NAME (t_int16)
    = build_decl (TYPE_DECL, get_identifier ("int_16"), t_int16);
  t_int8   = make_signed_type (8);
  TYPE_NAME (t_int8)
    = build_decl (TYPE_DECL, get_identifier ("int_8"), t_int8);
  t_int    = (BITS_PER_WORD == 32) ? t_int32 : t_int64;

  integer_type_node = t_int;
  sizetype = t_int;
  set_sizetype(t_int);
  char_type_node = t_int8;

  t_word64  = make_unsigned_type (64);
  TYPE_NAME (t_word64)
    = build_decl (TYPE_DECL, get_identifier ("word_64"), t_word64);
  t_word32  = make_unsigned_type (32);
  TYPE_NAME (t_word32)
    = build_decl (TYPE_DECL, get_identifier ("word_32"), t_word32);
  t_word16  = make_unsigned_type (16);
  TYPE_NAME (t_word16)
    = build_decl (TYPE_DECL, get_identifier ("word_16"), t_word16);
  t_word8   = make_unsigned_type (8);
  TYPE_NAME (t_word8)
    = build_decl (TYPE_DECL, get_identifier ("word_8"), t_word8);
  t_word    = (BITS_PER_WORD == 32) ? t_word32 : t_word64;

  t_reel = make_node (REAL_TYPE);  
  TYPE_PRECISION (t_reel) = FLOAT_TYPE_SIZE;
  TYPE_NAME (t_reel) 
    = build_decl (TYPE_DECL, get_identifier ("reel"), t_reel);
  layout_type (t_reel);
  t_lreel = make_node (REAL_TYPE); 
  TYPE_PRECISION (t_lreel) = DOUBLE_TYPE_SIZE;
  TYPE_NAME (t_lreel)
    = build_decl (TYPE_DECL, get_identifier ("lreel"), t_lreel);
  layout_type (t_lreel);
  t_xreel = make_node (REAL_TYPE); 
  TYPE_PRECISION (t_xreel) = LONG_DOUBLE_TYPE_SIZE;
  TYPE_NAME (t_xreel)
    = build_decl (TYPE_DECL, get_identifier ("xreel"), t_xreel);
  layout_type (t_xreel);

  v_zero = m3_build_int (0);  TREE_TYPE (v_zero) = t_int;
  v_one  = m3_build_int (1);  TREE_TYPE (v_zero) = t_int;
  integer_zero_node = v_zero;
  integer_one_node = v_one;
  size_zero_node = v_zero;
  size_one_node = v_one;

  t_void = make_node (VOID_TYPE);
  TYPE_NAME (t_void) 
    = build_decl (TYPE_DECL, get_identifier ("void"), t_void);
  layout_type (t_void);
  TYPE_ALIGN (t_void) = BITS_PER_UNIT;
  void_type_node = t_void;

  t_addr = build_pointer_type (t_void);
  TYPE_NAME (t_addr) 
    = build_decl (TYPE_DECL, get_identifier ("addr"), t_addr);
  layout_type (t_addr);

  v_null = build_int_2 (0, 0);  
  TREE_TYPE (v_null) = t_addr;
  null_pointer_node = v_null;
}

tree getdecls ()
{
  if (current_block) {
    return BLOCK_VARS (current_block); }
  else {
    return global_vars; }
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
truthvalue_conversion (expr)
     tree expr;
{
  if (TREE_CODE (expr) == ERROR_MARK)
    return expr;

  return m3_build2 (NE_EXPR, t_int, expr, integer_zero_node);
}
      
/*----------------------------------------------------- M3CG opcode handlers ---*/

static void
do_begin_unit ()
{
  INTEGER (n);

  exported_interfaces = 0;
  declare_runtime_functions ();
}

static void
do_end_unit ()
{
  int j;

  debug_tag ('i', NO_UID, "_%s", current_unit_name);
  for (j = 0; j < exported_interfaces; j++) {
    debug_field (exported_interfaces_names [j]); }
  debug_struct ();
  if (fault_proc != NULL_TREE) emit_fault_proc ();
}

static void
do_import_unit ()
{
  NAME (n);

  /* ignore */
}

static void
do_export_unit ()
{
  NAME (n);

  /* remember the set of exported interfaces */
  exported_interfaces_names [exported_interfaces++] = n;
}

static void
do_set_source_file ()
{
  NAME (s);

  input_filename = s;
  main_input_filename = s;
  emit_line_note (input_filename, lineno);
}

static void
do_set_source_line ()
{
  INTEGER (i);

  lineno = i;
  if (i > max_lineno) max_lineno = i;
  emit_line_note (input_filename, lineno);
}

static void
do_declare_typename ()
{
  TYPEID (my_id);
  NAME   (name);

  char fullname [100];

  snprintf (fullname, sizeof(fullname), "%s.%s", current_unit_name, name);
  debug_tag ('N', my_id, "");
  debug_field (fullname);
  debug_struct ();

  debug_tag ('n', NO_UID, "_%s", fullname);
  debug_field_id (my_id);
  debug_struct ();
}

static void
do_declare_array ()
{
  TYPEID  (my_id);
  TYPEID  (index_id);
  TYPEID  (elts_id);
  BITSIZE (size);

  debug_tag ('A', my_id, "_%d", size);
  debug_field_id (index_id);
  debug_field_id (elts_id);
  debug_struct ();
}

static void
do_declare_open_array ()
{
  TYPEID  (my_id);
  TYPEID  (elts_id);
  BITSIZE (size);

  debug_tag ('B', my_id, "_%d", size);
  debug_field_id (elts_id);
  debug_struct ();
}

static void
do_declare_enum ()
{
  TYPEID  (my_id);
  INTEGER (n_elts);
  BITSIZE (size);

  debug_tag ('C', my_id, "_%d", size);
  current_dbg_type_count1 = n_elts;
}

static void
do_declare_enum_elt ()
{
  NAME (n);

  debug_field (n);
  if (--current_dbg_type_count1 == 0) { debug_struct (); }
}

static void
do_declare_packed ()
{
  TYPEID  (my_id);
  BITSIZE (size);
  TYPEID  (target_id);

  debug_field_id (target_id);
  debug_tag ('D', my_id, "_%d", size);
  debug_struct ();
}

static void
do_declare_record ()
{
  TYPEID  (my_id);
  BITSIZE (size);
  INTEGER (n_fields);

  debug_tag ('R', my_id, "_%d", size);
  current_dbg_type_count1 = n_fields;
  current_dbg_type_count2 = 0;
  if (current_dbg_type_count1 == 0) { debug_struct (); }
}

static void
do_declare_field ()
{
  NAME      (name);
  BITOFFSET (offset);
  BITSIZE   (size);
  TYPEID    (my_id);

  debug_field_fmt (my_id, "_%d_%d_%s", offset, size, name);
  current_dbg_type_count1--;
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
    debug_struct (); }
}

static void
do_declare_set ()
{
  TYPEID  (my_id);
  TYPEID  (domain_id);
  BITSIZE (size);

  debug_tag ('S', my_id, "_%d", size);
  debug_field_id (domain_id);
  debug_struct ();
}

static void
do_declare_subrange ()
{

  TYPEID         (my_id);
  TYPEID         (domain_id);
  TARGET_INTEGER (min);
  TARGET_INTEGER (max);
  BITSIZE        (size);

  HOST_WIDE_INT  a0, a1, b0, b1;
  a1 = TREE_INT_CST_HIGH(min);  a0 = TREE_INT_CST_LOW(min);
  b1 = TREE_INT_CST_HIGH(max);  b0 = TREE_INT_CST_LOW(max);

  if ((a1 != 0) && (a1 != -1 || a0 >= 0)) {
    fatal ("cannot print minimum subrange value");
  }
  if ((b1 != 0) && (b1 != -1 || b0 >= 0)) {
    fatal ("cannot print maximum subrange value");
  }

  debug_tag ('Z', my_id, "_%d_%ld_%ld", size, a0, b0);
  debug_field_id (domain_id);
  debug_struct ();
}

static void
do_declare_pointer ()
{
  TYPEID        (my_id);
  TYPEID        (target_id);
  QUOTED_STRING (brand, brand_len);
  BOOLEAN       (traced);

  debug_tag ('Y', my_id, "_%d_%d_%d_%s", GET_MODE_BITSIZE (Pmode),
	     traced, (brand ? 1 : 0), (brand ? brand : "" ));
  debug_field_id (target_id);
  debug_struct ();
}

static void
do_declare_indirect ()
{
  TYPEID (my_id);
  TYPEID (target_id);

  debug_tag ('X', my_id, "_%d", GET_MODE_BITSIZE (Pmode));
  debug_field_id (target_id);
  debug_struct ();
}

static void
do_declare_proctype ()
{
  TYPEID  (my_id);
  INTEGER (n_formals);
  TYPEID  (result_id); 
  INTEGER (n_raises);
  INTEGER (call_conv);

  debug_tag ('P', my_id, "_%d_%c%d", GET_MODE_BITSIZE (Pmode),
	     n_raises < 0 ? 'A' : 'L', MAX (n_raises, 0));
  current_dbg_type_count1 = n_formals;
  current_dbg_type_count2 = MAX (0, n_raises);
  debug_field_id (result_id);
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
    debug_struct (); }
}

static void
do_declare_formal ()
{
  NAME   (n);
  TYPEID (my_id);

  debug_field_fmt (my_id, "_%s", n);
  current_dbg_type_count1--;
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
    debug_struct (); }
}

static void
do_declare_raises ()
{
  NAME (n);

  debug_field (n);
  current_dbg_type_count2--;
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
    debug_struct (); }
}

static void
do_declare_object ()
{
  TYPEID        (my_id);
  TYPEID        (super_id);
  QUOTED_STRING (brand, brand_length);
  BOOLEAN       (traced);
  INTEGER       (n_fields);
  INTEGER       (n_methods);
  BITSIZE       (field_size);

  debug_tag ('O', my_id, "_%d_%d_%d_%d_%s", POINTER_SIZE, n_fields, traced,
	     (brand ? 1:0), (brand ? brand : ""));
  debug_field_id (super_id);
  current_dbg_type_count1 = n_fields;
  current_dbg_type_count2 = n_methods;
  current_dbg_type_count3 = 0;
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
    debug_struct (); }
}

static void
do_declare_method ()
{
  NAME   (name);
  TYPEID (my_id);

  debug_field_fmt (my_id, "_%d_%d_%s", 
		   current_dbg_type_count3++  * GET_MODE_BITSIZE (Pmode),
		   GET_MODE_BITSIZE (Pmode), name);
  current_dbg_type_count2--;
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
    debug_struct (); }
}

static void
do_declare_opaque ()
{
  TYPEID (my_id);
  TYPEID (super_id);
  /* we don't pass this info to the debugger, only the revelation is interesting */
}

static void
do_reveal_opaque ()
{
  TYPEID (lhs);
  TYPEID (rhs);

  debug_tag ('Q', lhs, "_%d", GET_MODE_BITSIZE (Pmode));
  debug_field_id (rhs);
  debug_struct ();
}

static void
do_declare_exception ()
{
  NAME    (n);
  TYPEID  (t);
  BOOLEAN (raise_proc); 
  VAR     (base);
  INTEGER (offset);

  /* nothing yet */
}

static void
do_set_runtime_proc ()
{
  NAME (s);
  PROC (p);

  if (STREQ (s, "ReportFault")) { fault_handler = p; }
}

static void
do_set_runtime_hook ()
{
  NAME       (s);
  VAR        (v);
  BYTEOFFSET (o);

  if (STREQ (s, "ReportFault")) { fault_intf = v; fault_offs = o; }
}

static void
do_import_global ()
{
  NAME       (n);
  BYTESIZE   (s);
  ALIGNMENT  (a);
  TYPE       (t);
  TYPEID     (id);
  RETURN_VAR (v, VAR_DECL);


  DECL_NAME     (v) = DECL_ASSEMBLER_NAME (v) = fix_name (n, id);
  DECL_EXTERNAL (v) = 1;
  TREE_PUBLIC   (v) = 1;

  fix_type (v, t, s, a);

  rest_of_decl_compilation (v, 0, 1, 0);

  assemble_external (v);
  TREE_USED  (v) = 1;
  TREE_CHAIN (v) = global_vars;
  global_vars = v;
}

static void
do_declare_segment ()
{
  NAME       (n);
  TYPEID     (id);
  BOOLEAN    (is_const);
  RETURN_VAR (v, VAR_DECL);
      
  DECL_NAME (v) = DECL_ASSEMBLER_NAME (v) = fix_name (n, id);
  DECL_EXTERNAL (v) = 0;
  TREE_PUBLIC (v) = 1;
  /* we really don't have an idea of what the type of this var is; 
     let's try to put something that will be good enough for all
     the uses of this var we are going to see before  we have a bind_segment */
  fix_type (v, T_struct, BIGGEST_ALIGNMENT, BIGGEST_ALIGNMENT);
  TREE_UNSIGNED (TREE_TYPE (v)) = 1;
  TREE_STATIC (v) = 1;
  TREE_READONLY (v) = is_const;
  TREE_CONSTANT (v) = is_const;

  rest_of_decl_compilation (v, 0, 1, 0); 
  TREE_CHAIN (v) = global_vars;
  global_vars = v;
  current_segment = v;

  /* do not use "n", it is going to go away at the next instruction;
	 skip the 'MI_' or 'MM_' prefix. */
  current_unit_name = IDENTIFIER_POINTER (DECL_NAME (v)) + 3;
}

static void
do_bind_segment ()
{
  VAR       (v);
  BYTESIZE  (s);
  ALIGNMENT (a);
  TYPE      (t);
  BOOLEAN   (exported);
  BOOLEAN   (initialized);

  current_segment = v;
  fix_type (v, t, s, a);
  TREE_UNSIGNED (v) = TREE_UNSIGNED (TREE_TYPE (v));
  TREE_PUBLIC (v) = exported;
  TREE_STATIC (v) = 1;
}

static void
do_declare_global ()
{
  NAME       (n);
  BYTESIZE   (s);
  ALIGNMENT  (a);
  TYPE       (t);
  TYPEID     (id);
  BOOLEAN    (exported);
  BOOLEAN    (initialized);
  RETURN_VAR (v, VAR_DECL);

  DECL_NAME (v) = DECL_ASSEMBLER_NAME (v) = fix_name (n, id);
  DECL_EXTERNAL (v) = 0;
  DECL_COMMON (v) = (initialized == 0);  /*** -- in gcc 2.6.0 ***/
  TREE_PUBLIC (v) = exported;
  TREE_STATIC (v) = 1;
  fix_type (v, t, s, a);

  rest_of_decl_compilation (v, 0, 1, 0);
  TREE_CHAIN (v) = global_vars;
  global_vars = v;
}

static void
do_declare_constant ()
{
  NAME       (n);
  BYTESIZE   (s);
  ALIGNMENT  (a);
  TYPE       (t);
  TYPEID     (id);
  BOOLEAN    (exported);
  BOOLEAN    (initialized);
  RETURN_VAR (v, VAR_DECL);

  DECL_NAME (v) = DECL_ASSEMBLER_NAME (v) = fix_name (n, id);
  DECL_EXTERNAL (v) = 0;
  DECL_COMMON (v) = (initialized == 0);  /*** -- in gcc 2.6.0 ***/
  TREE_PUBLIC (v) = exported;
  TREE_STATIC (v) = 1;
  TREE_READONLY (v) = 1;
  TREE_CONSTANT (v) = 1;
  fix_type (v, t, s, a);
  
  rest_of_decl_compilation (v, 0, 1, 0);
  TREE_CHAIN (v) = global_vars;
  global_vars = v;
}

static void
do_declare_local ()
{
  NAME       (n);
  BYTESIZE   (s);
  ALIGNMENT  (a);
  TYPE       (t);
  TYPEID     (id);
  BOOLEAN    (in_memory);
  BOOLEAN    (up_level);
  FREQUENCY  (f);
  RETURN_VAR (v, VAR_DECL);

  DECL_NAME (v) = DECL_ASSEMBLER_NAME (v) = fix_name (n, id);
  DECL_NONLOCAL (v) = up_level;
  TREE_ADDRESSABLE (v) = in_memory;
  DECL_CONTEXT (v) = current_function_decl; 
  fix_type (v, t, s, a);

  if (compiling_body) {
    TREE_CHAIN (v) = BLOCK_VARS (current_block);
    BLOCK_VARS (current_block) = v;
  } else {
    tree subblocks = BLOCK_SUBBLOCKS (DECL_INITIAL (current_function_decl));
    TREE_CHAIN (v) = BLOCK_VARS (subblocks);
    BLOCK_VARS (subblocks) = v;
  }

  if (compiling_body) { compile_local (v); }
}

static void
do_declare_param ()
{
  NAME       (n);
  BYTESIZE   (s);
  ALIGNMENT  (a);
  TYPE       (t);
  TYPEID     (id);
  BOOLEAN    (in_memory);
  BOOLEAN    (up_level);
  FREQUENCY  (f);
  RETURN_VAR (v, PARM_DECL);

  if (ignore_params) { return; }

  DECL_NAME (v) = DECL_ASSEMBLER_NAME (v) = fix_name (n, id);
  DECL_NONLOCAL (v) = up_level;
  TREE_ADDRESSABLE (v) = in_memory;
  fix_type (v, t, s, a);
  DECL_ARG_TYPE (v) = TREE_TYPE (v);
  DECL_CONTEXT (v) = current_function_decl;

  TREE_CHAIN (v) = DECL_ARGUMENTS (current_function_decl);
  DECL_ARGUMENTS (current_function_decl) = v;
  if (DECL_MODE (v) == VOIDmode) { DECL_MODE (v) = Pmode; }

  rest_of_decl_compilation (v, 0, 0, 1);
}

static void
do_declare_temp ()
{
  BYTESIZE   (s);
  ALIGNMENT  (a);
  TYPE       (t);
  BOOLEAN    (in_memory);
  RETURN_VAR (v, VAR_DECL);

  if (t == T_void) { t = T_struct; }
  declare_temp (build_type (t, s, a), in_memory, v);
}

static void
do_free_temp ()
{
  VAR (v);
  /* nothing to do */
}

static void
do_begin_init ()
{
  VAR (v);

  current_record_offset = 0;
  current_record_vals = NULL_TREE;
  current_record_type = make_node (RECORD_TYPE);
  TREE_ASM_WRITTEN (current_record_type) = 1;
}

static void
do_end_init ()
{
  VAR (v);

  int v_size = TREE_INT_CST_LOW (DECL_SIZE (v));
  if (current_record_offset < v_size) { one_gap (v_size); }
	  
  TYPE_FIELDS (current_record_type) = nreverse (TYPE_FIELDS (current_record_type));
  layout_type (current_record_type);
  
  DECL_INITIAL (v) = make_node (CONSTRUCTOR);
  TREE_CONSTANT (DECL_INITIAL (v)) = 1;
  TREE_TYPE (DECL_INITIAL (v)) = current_record_type;
  CONSTRUCTOR_ELTS (DECL_INITIAL (v)) = nreverse (current_record_vals);
}

static void
do_init_int ()
{
  BYTEOFFSET     (o);
  TARGET_INTEGER (v);
  MTYPE          (t);

  tree f, vv;
  int bits = TREE_INT_CST_LOW (TYPE_SIZE (t));

  one_field (o, bits, &f, &vv);
  TREE_TYPE (f) = t;
  TREE_VALUE (vv) = v;
  TREE_TYPE (TREE_VALUE (vv)) = TREE_TYPE (f);
}

static void
do_init_proc ()
{
  BYTEOFFSET (o);
  PROC       (p);

  tree f, v;

  one_field (o, POINTER_SIZE, &f, &v);
  TREE_TYPE (f) = t_addr;
  TREE_VALUE (v) = m3_rtl (XEXP (DECL_RTL (p), 0));
}

static void
do_init_label ()
{
  BYTEOFFSET (o);
  LABEL      (l);

  tree f, v;

  one_field (o, POINTER_SIZE, &f, &v);
  TREE_TYPE (f) = t_addr;
  TREE_VALUE (v) = m3_rtl (label_rtx (l));
}

static void
do_init_var ()
{
  BYTEOFFSET (o);
  VAR        (v);
  BYTEOFFSET (b);

  tree F, V;

  one_field (o, POINTER_SIZE, &F, &V);
  TREE_TYPE (F) = t_addr;
  TREE_VALUE (V) = m3_build2 (PLUS_EXPR, t_addr,
			      m3_build1 (ADDR_EXPR, t_addr, v),
			      m3_build_int (b / 8));
}

static void
do_init_offset ()
{
  BYTEOFFSET (o);
  VAR        (v);

  tree F, V;
  int j;

  one_field (o, POINTER_SIZE, &F, &V);
  TREE_TYPE (F) = t_int;

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

  TREE_VALUE (V) = m3_build_int (j);
}

static void
do_init_chars ()
{
  BYTEOFFSET    (o);
  QUOTED_STRING (s, l);

  tree f, v;

  one_field (o, l * 8, &f, &v);
  TREE_TYPE (f) = make_node (ARRAY_TYPE);
  TYPE_SIZE (TREE_TYPE (f)) = DECL_SIZE (f);
  TYPE_ALIGN (TREE_TYPE (f)) = DECL_ALIGN (f); 
  TREE_VALUE (v) =  build_string (l, s);
  TREE_TYPE (TREE_VALUE (v)) = TREE_TYPE (f);
}

static void
do_init_float ()
{
  BYTEOFFSET (o);
  FLOAT      (f, fkind);

  tree F, V, t;
  int s;


  switch (fkind) {
  case 0: s = FLOAT_TYPE_SIZE;        t = t_reel;   break; 
  case 1: s = DOUBLE_TYPE_SIZE;       t = t_lreel;  break; 
  case 2: s = LONG_DOUBLE_TYPE_SIZE;  t = t_xreel;  break;
  }

  one_field (o, s, &F, &V);
  TREE_TYPE (F) = t;
  TREE_VALUE (V) = f;

#if 0 /* FIXME? used to be */
  TREE_VALUE (V)
    = build_real (TREE_TYPE (F),
                REAL_VALUE_ATOF (f, TYPE_MODE (TREE_TYPE (F))));
#endif
}

static void
do_import_procedure ()
{
  NAME    (n);
  INTEGER (n_params);
  MTYPE2  (return_type, ret_type);
  INTEGER (call_conv);
  PROC    (p);

  /** 4/30/96 -- WKK -- It seems gcc can't hack small return values... */
  if (IS_INTEGER_TYPE(ret_type)) { return_type = t_int; }
  if (IS_WORD_TYPE(ret_type))    { return_type = t_word; }

  ignore_params = 1;  /* we don't declare the formals for imported procs */
  declare_external_proc (p, n, 
			 build_function_type (return_type, NULL_TREE));
}

static void
do_declare_procedure ()
{
  NAME    (n);
  INTEGER (n_params);
  MTYPE2  (return_type, ret_type);
  LEVEL   (lev);
  INTEGER (call_conv);
  BOOLEAN (exported);
  PROC    (parent);
  PROC    (p);

  tree parm_block = make_node (BLOCK);
  tree top_block  = make_node (BLOCK);

  /** 4/30/96 -- WKK -- It seems gcc can't hack small return values... */
  if (IS_INTEGER_TYPE(ret_type)) { return_type = t_int; }
  if (IS_WORD_TYPE(ret_type))    { return_type = t_word; }

  ignore_params = 0;

  DECL_NAME (p) = DECL_ASSEMBLER_NAME (p) = get_identifier (n);
  DECL_RESULT (p) = build_decl (RESULT_DECL, NULL_TREE, return_type);
  DECL_CONTEXT (DECL_RESULT (p)) = p;
  TREE_STATIC (p) = 1;
  TREE_PUBLIC (p) = exported;
  TREE_TYPE (p) = build_function_type (return_type, NULL_TREE);
  
  DECL_CONTEXT (p) = parent;

  BLOCK_SUPERCONTEXT (parm_block) = p;
  DECL_INITIAL (p) = parm_block;
  TREE_USED (parm_block) = 1;

  BLOCK_SUPERCONTEXT (top_block) = parm_block;
  BLOCK_SUBBLOCKS (parm_block) = top_block;
  TREE_USED (top_block) = 1;

  make_function_rtl (p);
  current_function_decl = p;
}

static void
do_begin_procedure ()
{
  PROC (p);

  tree parm, local, args_types;

  DECL_SOURCE_LINE (p) = lineno;

  args_types = tree_cons (NULL_TREE, t_void, NULL_TREE);
  for (parm = DECL_ARGUMENTS (p); parm; parm = TREE_CHAIN (parm)) {
    args_types = tree_cons (NULL_TREE, TREE_TYPE (parm), args_types);
  }
  TREE_TYPE (p) = build_function_type (TREE_TYPE (DECL_RESULT (p)), args_types);
  DECL_ARGUMENTS (p) = nreverse (DECL_ARGUMENTS (p));
  announce_function (p);
  make_function_rtl (p);

  if (DECL_CONTEXT (p)) {
    push_function_context ();
  } else {
    compiling_body = 1;
    /* make sure there is a difference between saveable_obstack and
       current_obstack: the back-end optimizers rely on this (e.g.,
       varasm) */
    push_obstacks_nochange();
    temporary_allocation();
  }

  current_function_decl = p;
  current_function_name = IDENTIFIER_POINTER (DECL_NAME (p));

  init_function_start (p, input_filename, lineno);
  expand_function_start (p, 0);

  m3_push_block (BLOCK_SUBBLOCKS (DECL_INITIAL (p))); 

  /* compile the locals we have already seen */
  for (local = BLOCK_VARS (current_block); local; local = TREE_CHAIN (local)) {
    compile_local (local);
  }

  clear_last_expr ();
  expand_start_bindings (0);
}

static void
do_end_procedure ()
{
  PROC (p);

  expand_end_bindings (BLOCK_VARS (current_block), 1, 0);
  expand_function_end (input_filename, lineno, 0);
  rest_of_compilation (current_function_decl);

  m3_pop_block (BLOCK_SUBBLOCKS (DECL_INITIAL (p))); 

  if (DECL_CONTEXT (p)) {
    pop_function_context ();
  } else {
    compiling_body = 0;
    /* Go back to permanent allocation, but without freeing objects
       created in the interim, since M3 holds on to some nested
       allocations until the end of the module */
    pop_obstacks ();
  }
}

static void
do_begin_block ()
{
  m3_push_block (NULL_TREE);
  clear_last_expr ();
  expand_start_bindings (0);
}

static void
do_end_block ()
{
  expand_end_bindings (BLOCK_VARS (current_block), 1, 0);
  m3_pop_block (NULL_TREE);
}

static void
do_note_procedure_origin ()
{
  PROC (p);

  fatal("note_procedure_origin psuedo-op encountered.");
}

static void
do_set_label ()
{
  LABEL   (l);
  BOOLEAN (barrier);

  DECL_CONTEXT (l) = current_function_decl;
  expand_label (l);
  if (barrier) { LABEL_PRESERVE_P (label_rtx (l)) = 1; }
}

static void
do_m3_jump ()
{
  LABEL (l);

  expand_goto (l);
}

static void
do_if_true ()
{
  TYPE      (t);
  LABEL     (l);
  FREQUENCY (f);

  do_jump (stack [--tos], NULL_RTX, label_rtx (l));
}

static void
do_if_false ()
{
  TYPE      (t);
  LABEL     (l);
  FREQUENCY (f);

  do_jump (stack [--tos], label_rtx (l), NULL_RTX);
}

static void
do_if_compare (condition)
     enum tree_code condition;
{
  MTYPE     (t);
  LABEL     (l);
  FREQUENCY (f);

  condop (condition, l, t);
}

static void do_if_eq ()  { do_if_compare (EQ_EXPR); }
static void do_if_ne ()  { do_if_compare (NE_EXPR); }
static void do_if_gt ()  { do_if_compare (GT_EXPR); }
static void do_if_ge ()  { do_if_compare (GE_EXPR); }
static void do_if_lt ()  { do_if_compare (LT_EXPR); }
static void do_if_le ()  { do_if_compare (LE_EXPR); }

static void
do_case_jump ()
{
  MTYPE   (t);
  INTEGER (n);

#ifdef pa_cpu_attr
  /* generating a table of PIC labels on HPUX is too hard.  So we'll
     generate a C switch statement where the case bodies are just "goto"s. */
  if (flag_pic)
  {
    tree duplicate;
    int i, err;

    expand_start_case (0, stack[tos-1], t_int, "CASE statement");
    for (i = 0; i < n; i++) {
      LABEL (ll);
      tree here = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
      if (pushcase (m3_build_int (i), convert_case, here, &duplicate)) {
    abort (); }
      expand_goto (ll);
    }
    expand_end_case (stack[tos-1]);
    tos--;
  }
  else
#endif
  {
    tree index_type = build_index_type (m3_build_int (n-1));
    tree table_type = build_array_type (t_addr, index_type);
    tree table = make_node (VAR_DECL);
    tree labels = NULL_TREE;
    tree dest_label;
    int i;

    for (i = 0; i < n; i++) {
      LABEL (ll);
      tree l = m3_build1 (ADDR_EXPR, t_addr, ll);

      TREE_CONSTANT (l) = 1;
      if (labels == NULL_TREE) {
        labels = build_tree_list (NULL_TREE, l);
      } else {
        labels = tree_cons (NULL_TREE, l, labels);
      }
    }

    DECL_NAME (table) = fix_name (0, 0);
    TREE_READONLY (table) = 1;
    TREE_STATIC (table) = 1;
    DECL_INITIAL (table) = make_node (CONSTRUCTOR);
    TREE_CONSTANT (DECL_INITIAL (table)) = 1;
    TREE_TYPE (DECL_INITIAL (table)) = table_type;
    CONSTRUCTOR_ELTS (DECL_INITIAL (table)) = nreverse (labels);

    declare_temp (table_type, 1, table);
    dest_label = m3_build2 (ARRAY_REF, t_addr, table, stack [tos-1]);
    expand_computed_goto (dest_label);
    tos--;
  }
}

static void
do_exit_proc ()
{
  MTYPE (t);

  if (t == t_void) {
    expand_null_return ();
  } else {
    tree res = m3_build2 (MODIFY_EXPR, t, DECL_RESULT (current_function_decl),
			  stack [tos-1]);
    TREE_SIDE_EFFECTS (res) = 1;
    expand_return (res);
    tos--;
  }
}

static void
do_load ()
{
  VAR        (v);
  BYTEOFFSET (o);
  MTYPE2     (src_t, src_T);
  MTYPE2     (dest_t, dest_T);

  m3_load (v, o, src_t, src_T, dest_t, dest_T);
}

static void
do_load_address ()
{
  VAR        (v);
  BYTEOFFSET (o);

  stack [tos] = m3_build1 (ADDR_EXPR, t_addr, v);
  if (o != 0) {
    stack [tos] = m3_build2 (PLUS_EXPR, t_addr, 
			     stack [tos], m3_build_int (o / 8));
  }
  tos++;
}

static void
do_load_indirect ()
{
  BYTEOFFSET (o);
  MTYPE2     (src_t, src_T);
  MTYPE2     (dest_t, dest_T);

  tree val = stack [tos-1];

  if (o != 0) { val = m3_build2 (PLUS_EXPR, t_addr, val, m3_build_int (o/8)); }
  val = m3_cast (build_pointer_type (src_t), val);
  val = m3_build1 (INDIRECT_REF, src_t, val);
  if (src_T != dest_T) { val = m3_build1 (CONVERT_EXPR, dest_t, val); }

  stack[tos-1] = val;
}

static void
do_store ()
{
  VAR        (v);
  BYTEOFFSET (o);
  MTYPE      (src_t);
  MTYPE      (dest_t);

  m3_store (v, o, src_t, dest_t);
}

static void
do_store_indirect ()
{
  BYTEOFFSET (o);
  MTYPE      (src_t);
  MTYPE      (dest_t);

  tree ptr = stack[tos-2];
  tree val = stack[tos-1];

  if (o != 0) { ptr = m3_build2 (PLUS_EXPR, t_addr, ptr, m3_build_int (o/8)); }
  ptr = m3_cast (build_pointer_type (dest_t), ptr);
  ptr = m3_build1 (INDIRECT_REF, dest_t, ptr);
  expand_assignment (ptr, m3_build1 (CONVERT_EXPR, dest_t, val), 0,0);
  tos -= 2;
}

static void
do_load_nil ()
{
  stack [tos++] = v_null;
}

static void
do_load_integer ()
{
  MTYPE          (t);
  TARGET_INTEGER (n);

  if (TREE_TYPE (n) != t) { n = m3_build1 (CONVERT_EXPR, t, n); }
  stack [tos++] = n;
}

static void
do_load_float ()
{
  MTYPE (t);
  FLOAT (f, fkind);

  stack [tos++] = f;
}

static void
do_compare (op)
     enum tree_code op;
{
  MTYPE (src_t);
  MTYPE (dest_t);

  tree t1 = m3_cast (src_t, stack [tos-1]);
  tree t2 = m3_cast (src_t, stack [tos-2]);

  TREE_UNSIGNED (t1) = TREE_UNSIGNED (src_t);
  TREE_UNSIGNED (t2) = TREE_UNSIGNED (src_t);
  stack [tos-2] = m3_build2 (op, dest_t, t2, t1);
  tos --;
}

static void do_eq () { do_compare (EQ_EXPR); }
static void do_ne () { do_compare (NE_EXPR); }
static void do_gt () { do_compare (GT_EXPR); }
static void do_ge () { do_compare (GE_EXPR); }
static void do_lt () { do_compare (LT_EXPR); }
static void do_le () { do_compare (LE_EXPR); }

static void
do_add ()
{
  MTYPE (t);

  binaryop (PLUS_EXPR, t);
}

static void
do_subtract ()
{
  MTYPE (t);

  binaryop (MINUS_EXPR, t);
}

static void
do_multiply ()
{
  MTYPE (t);

  binaryop (MULT_EXPR, t);
}

static void
do_divide ()
{
  MTYPE (t);

  binaryop (RDIV_EXPR, t);
}

static void
do_negate ()
{
  MTYPE (t);

  unaryop (NEGATE_EXPR, t);
}

static void
do_abs ()
{
  MTYPE (t);

  unaryop (ABS_EXPR, t);
}

static void
do_max ()
{
  MTYPE (t);

  /*** ?? binaryop (MAX_EXPR, t);  ?? ****/

  tree temp1 = declare_temp (t, 0, 0);
  tree temp2 = declare_temp (t, 0, 0);
  tree t1    = m3_build2 (MODIFY_EXPR, t, temp1, stack [tos-1]);
  tree t2    = m3_build2 (MODIFY_EXPR, t, temp2, stack [tos-2]);
  tree res; 

  TREE_SIDE_EFFECTS (t1) = 1;
  TREE_SIDE_EFFECTS (t2) = 1;
  res = m3_build3 (COND_EXPR, t, 
		   m3_build2 (LE_EXPR, t_int, temp2, temp1), temp1, temp2);
  stack [tos-2] = m3_build2 (COMPOUND_EXPR, t,
			     m3_build2 (COMPOUND_EXPR, t, t1, t2), res);
  tos--;
}

static void
do_min ()
{
  MTYPE (t);

  /*** ?? binaryop (MIN_EXPR, t);  ?? ****/

  tree temp1 = declare_temp (t, 0, 0);
  tree temp2 = declare_temp (t, 0, 0);
  tree t1    = m3_build2 (MODIFY_EXPR, t, temp1, stack [tos-1]);
  tree t2    = m3_build2 (MODIFY_EXPR, t, temp2, stack [tos-2]);
  tree res; 

  TREE_SIDE_EFFECTS (t1) = 1;
  TREE_SIDE_EFFECTS (t2) = 1;
  res = m3_build3 (COND_EXPR, t, 
		   m3_build2 (LE_EXPR, t_int, temp1, temp2), temp1, temp2);
  stack [tos-2] = m3_build2 (COMPOUND_EXPR, t,
			     m3_build2 (COMPOUND_EXPR, t, t1, t2), res);
  tos--;
}

static void
do_round ()
{
  MTYPE (src_t);
  MTYPE (dest_t);

  tree temp1 = declare_temp (t_lreel, 0, 0);
  tree t1, zero, half, res;

  if (src_t == t_reel) { src_t = t_lreel; unaryop (CONVERT_EXPR, src_t); }

  t1 = m3_build2 (MODIFY_EXPR, src_t, temp1, stack [tos-1]);
  zero = m3_build_real ("0.0", src_t);
  half = m3_build_real ("0.5", src_t);

  TREE_SIDE_EFFECTS (t1) = 1;
  res = m3_build1 (FIX_TRUNC_EXPR, dest_t,
		   m3_build3 (COND_EXPR, src_t,
			      m3_build2 (GE_EXPR, src_t, temp1, zero),
			      m3_build2 (PLUS_EXPR, src_t, temp1, half),
			      m3_build2 (MINUS_EXPR, src_t, temp1, half)));
  stack [tos-1] = m3_build2 (COMPOUND_EXPR, dest_t, t1, res);
}

static void
do_trunc ()
{
  MTYPE (src_t);
  MTYPE (dest_t);

  unaryop (FIX_TRUNC_EXPR, dest_t);
}

static void
do_floor ()
{
  MTYPE (src_t);
  MTYPE (dest_t);

  tree temp1 = declare_temp (src_t, 0, 0);
  tree temp2 = declare_temp (dest_t, 0, 0);
  tree t1    = m3_build2 (MODIFY_EXPR, src_t, temp1, stack[tos-1]);
  tree t2    = m3_build2 (MODIFY_EXPR, dest_t, 
			  temp2, m3_build1 (FIX_TRUNC_EXPR, dest_t, temp1));
  tree zero = m3_build_real ("0.0", src_t);
  tree res;

  TREE_SIDE_EFFECTS (t1) = 1;
  TREE_SIDE_EFFECTS (t2) = 1;
  res = m3_build3 (COND_EXPR, dest_t,
		   m3_build2 (GE_EXPR, src_t, temp1, zero),
		   temp2,
		   m3_build3 (COND_EXPR, dest_t,
			      m3_build2 (EQ_EXPR, src_t, 
					 temp1, build1 (FLOAT_EXPR, src_t, temp2)),
			      temp2,
			      m3_build2 (MINUS_EXPR, dest_t,
					 temp2, v_one)));
  stack [tos-1] = m3_build2 (COMPOUND_EXPR, dest_t,
			     m3_build2 (COMPOUND_EXPR, dest_t, t1, t2), res);
}

static void
do_ceiling ()
{
  MTYPE (src_t);
  MTYPE (dest_t);

  tree temp1 = declare_temp (src_t, 0, 0);
  tree temp2 = declare_temp (dest_t, 0, 0);
  tree t1    = m3_build2 (MODIFY_EXPR, src_t, temp1, stack [tos-1]);
  tree t2    = m3_build2 (MODIFY_EXPR, dest_t,
			  temp2, m3_build1 (FIX_TRUNC_EXPR, dest_t, temp1));
  tree zero = m3_build_real ("0.0", src_t);
  tree res;

  TREE_SIDE_EFFECTS (t1) = 1;
  TREE_SIDE_EFFECTS (t2) = 1;
  res = m3_build3 (COND_EXPR, dest_t,
		   m3_build2 (LE_EXPR, src_t, temp1, zero),
		   temp2,
		   m3_build3 (COND_EXPR, dest_t,
			      m3_build2 (EQ_EXPR, src_t, temp1,
					 m3_build1 (FLOAT_EXPR, src_t, temp2)),
			      temp2,
			      m3_build2 (PLUS_EXPR, dest_t, temp2, v_one)));
  stack [tos-1] = m3_build2 (COMPOUND_EXPR, dest_t,
			     m3_build2 (COMPOUND_EXPR, dest_t, t1, t2), res);
}

static void
do_cvt_float ()
{
  MTYPE2 (src_t, src_T);
  MTYPE  (dest_t);

  if (IS_REAL_TYPE(src_T)) {
    unaryop (CONVERT_EXPR, dest_t);
  } else {
    unaryop (FLOAT_EXPR, dest_t);
  }
}

static void
do_div ()
{
  MTYPE2 (t, T);
  SIGN   (a);
  SIGN   (b);

  if ((b == 'P' && a == 'P') || IS_WORD_TYPE(T)) {
    stack [tos-2] = m3_cast (t_word, stack [tos-2]);
    stack [tos-1] = m3_cast (t_word, stack [tos-1]);
    binaryop (FLOOR_DIV_EXPR, t);
  } else {
    m3_start_call ();
    m3_pop_param (t_int);
    m3_pop_param (t_int);
    m3_call_direct (div_proc, NULL_TREE);
  }
}

static void
do_mod ()
{
  MTYPE2 (t, T);
  SIGN   (a);
  SIGN   (b);

  if ((b == 'P' && a == 'P') || IS_WORD_TYPE(T)) {
    stack [tos-2] = m3_cast (t_word, stack [tos-2]);
    stack [tos-1] = m3_cast (t_word, stack [tos-1]);
    binaryop (FLOOR_MOD_EXPR, t);
  } else {
    m3_start_call ();
    m3_pop_param (t_int);
    m3_pop_param (t_int);
    m3_call_direct (mod_proc, NULL_TREE);
  }
}

static void
do_set_union ()
{
  BYTESIZE (n);

  setop (set_union_proc, n, 3);
}

static void
do_set_difference ()
{
  BYTESIZE (n);

  setop (set_diff_proc, n, 3);
}

static void
do_set_intersection ()
{
  BYTESIZE (n);

  setop (set_inter_proc, n, 3);
}

static void
do_set_sym_difference ()
{
  BYTESIZE (n);

  setop (set_sdiff_proc, n, 3);
}

static void
do_set_member ()
{
  BYTESIZE (n);
  MTYPE    (t);

  if (t != t_int) { fatal ("set_member on non-native integer size"); }
  setop2 (set_member_proc, 2);
}

static void
do_set_compare (proc)
     tree proc;
{
  BYTESIZE (n);
  MTYPE    (t);

  if (t != t_int) { fatal ("set_compare on non-native integer size"); }
  setop (proc, n, 2);
}

static void do_set_eq () { do_set_compare (set_eq_proc); }
static void do_set_ne () { do_set_compare (set_ne_proc); }
static void do_set_gt () { do_set_compare (set_gt_proc); }
static void do_set_ge () { do_set_compare (set_ge_proc); }
static void do_set_lt () { do_set_compare (set_lt_proc); }
static void do_set_le () { do_set_compare (set_le_proc); }

static void
do_set_range ()
{
  BYTESIZE (n);
  MTYPE    (t);

  if (t != t_int) { fatal ("set_range on non-native integer size"); }
  setop2 (set_range_proc, 3);
}

static void
do_set_singleton ()
{
  BYTESIZE (n);
  MTYPE    (t);

  if (t != t_int) { fatal ("set_singleton on non-native integer size"); }
  setop2 (set_sing_proc, 2);
}

static void
do_not ()
{
  MTYPE (t);

  unaryop (BIT_NOT_EXPR, unsigned_type (t));
}

static void
do_and ()
{
  MTYPE (t);

  binaryop (BIT_AND_EXPR, unsigned_type(t));
}

static void
do_or ()
{
  MTYPE (t);

  binaryop (BIT_IOR_EXPR, unsigned_type(t));
}

static void
do_xor ()
{
  MTYPE (t);

  binaryop (BIT_XOR_EXPR, unsigned_type(t));
}

static void
do_m3_shift ()
{
  MTYPE (t);

  tree temp1 = declare_temp (t, 0, 0);
  tree temp2 = declare_temp (t, 0, 0);
  tree t1    = m3_build2 (MODIFY_EXPR, t, temp1, stack [tos-1]);
  tree t2    = m3_build2 (MODIFY_EXPR, t, temp2, stack [tos-2]);
  tree res;


  TREE_SIDE_EFFECTS (t1) = 1;
  TREE_SIDE_EFFECTS (t2) = 1;
  res  = m3_build3 (COND_EXPR, unsigned_type(t),
		    m3_build2 (GE_EXPR, t, temp1, v_zero),
		    do_shift (temp2, temp1, 0, t),
		    do_shift (temp2, m3_build1 (NEGATE_EXPR, t, temp1), 1, t));
  stack [tos-2] = m3_build2 (COMPOUND_EXPR, unsigned_type(t),
			     m3_build2 (COMPOUND_EXPR, t, t1, t2), res);
  tos--; 
}

static void
do_shift_left ()
{
  MTYPE (t);

  stack [tos-2] = do_shift (stack[tos-2], stack[tos-1], 0, t);
  tos--;
}

static void
do_shift_right ()
{
  MTYPE (t);

  stack [tos-2] = do_shift (stack[tos-2], stack[tos-1], 1, t);
  tos--;
}

static void
do_m3_rotate ()
{
  MTYPE (t);

  tree temp1 = declare_temp (t, 0, 0);
  tree temp2 = declare_temp (t, 0, 0);
  tree t1    = m3_build2 (MODIFY_EXPR, t, temp1, stack [tos-1]);
  tree t2    = m3_build2 (MODIFY_EXPR, t, temp2, stack [tos-2]);
  tree res;

  TREE_SIDE_EFFECTS (t1) = 1;
  TREE_SIDE_EFFECTS (t2) = 1;
  res = m3_build3 (COND_EXPR, t,
		   m3_build2 (GE_EXPR, t, temp1, v_zero),
		   do_rotate (temp2, temp1, 0, t),
		   do_rotate (temp2, m3_build1 (NEGATE_EXPR, t_int, temp1), 1, t));
  stack [tos-2] = m3_build2 (COMPOUND_EXPR, t,
			     m3_build2 (COMPOUND_EXPR, t, t1, t2), res);
  tos--;
}

static void
do_rotate_left ()
{
  MTYPE (t);

  stack [tos-2] = do_rotate (stack[tos-2], stack[tos-1], 0, t);
  tos--;
}

static void
do_rotate_right ()
{
  MTYPE (t);

  stack [tos-2] = do_rotate (stack[tos-2], stack[tos-1], 1, t);
  tos--;
}

static void
do_widen ()
{
  BOOLEAN (sign);

  tree dest_t = (sign ? t_int64 : t_word64);
  tree src_t  = (sign ? t_int32 : t_word32);

  stack[tos-1] = m3_build1 (CONVERT_EXPR, dest_t, m3_cast (src_t, stack[tos-1]));
}

static void
do_chop ()
{
  stack[tos-1] = m3_build1 (CONVERT_EXPR, t_int32,
			    m3_build2 (BIT_AND_EXPR, t_int64, stack[tos-1],
				       m3_build_int (0xffffffff)));
}

static void
do_m3_extract ()
{
  MTYPE   (t);
  BOOLEAN (sign_extend);

  stack [tos-3] = do_extract (stack[tos-3], stack[tos-2], stack[tos-1],
			      t, sign_extend);
  tos -= 2;
}

static void
do_extract_n ()
{
  MTYPE   (t);
  BOOLEAN (sign_extend);
  INTEGER (n);

  stack [tos-2] = do_extract (stack[tos-2], stack[tos-1], m3_build_int (n),
			      t, sign_extend);
  tos -= 1;
}

static void
do_extract_mn ()
{
  MTYPE   (t);
  BOOLEAN (sign_extend);
  INTEGER (m);
  INTEGER (n);

  stack [tos-1] = do_fixed_extract (stack[tos-1], m, n,
				    t, sign_extend);
}

static void
do_m3_insert ()
{
  MTYPE (t);

  stack [tos-4] = do_insert (stack[tos-4], stack[tos-3], 
			     stack[tos-2], stack[tos-1], t);
  tos -= 3;
}

static void
do_insert_n ()
{
  MTYPE   (t);
  INTEGER (n);

  stack [tos-3] = do_insert (stack[tos-3], stack[tos-2], stack[tos-1],
			     m3_build_int (n), t);
  tos -= 2;
}

static void
do_insert_mn ()
{
  MTYPE   (t);
  INTEGER (m);
  INTEGER (n);

  stack [tos-2] = do_fixed_insert (stack[tos-2], stack[tos-1], m, n, t);
  tos--;
}

static void
do_swap ()
{
  MTYPE (t);
  MTYPE (u);

  m3_swap ();
}

static void
do_pop ()
{
  MTYPE (t);

  TREE_SIDE_EFFECTS (stack [tos-1]) = 1;
  expand_expr_stmt (stack [--tos]);
}

static void
do_copy_n ()
{
  MTYPE   (cnt_t);
  MTYPE   (mem_t);
  BOOLEAN (overlap);

  tree tmp;

  if (cnt_t != t_int) { fatal ("copy_n called with non-native integer count"); }

  m3_start_call ();

  /* rearrange the parameters */
  tmp = stack [tos-3];
  stack [tos-3] = stack [tos-2];
  stack [tos-2] = stack [tos-1];
  stack [tos-1] = tmp;

  m3_pop_param (t_addr);
  m3_swap ();
  m3_pop_param (t_addr);

  stack [tos-1] = m3_build2 (MULT_EXPR, cnt_t, 
			     stack [tos-1],
			     m3_build_int(TREE_INT_CST_LOW (TYPE_SIZE(mem_t))/8));
  m3_pop_param (cnt_t);

  m3_call_direct (overlap ? memmove_proc : memcpy_proc, t_void);
}

static void
do_copy ()
{
  INTEGER (n);
  MTYPE2  (t, T);
  BOOLEAN (overlap);

  tree pts;
  tree ts = make_node (LANG_TYPE);
  int s = n * TREE_INT_CST_LOW (TYPE_SIZE (t));

  TYPE_SIZE (ts) = m3_build_int (s);
  TYPE_SIZE_UNIT (ts) = size_binop (FLOOR_DIV_EXPR, TYPE_SIZE(ts),
                                    size_int(BITS_PER_UNIT));
  TYPE_ALIGN (ts) = TYPE_ALIGN (t);

  if (IS_REAL_TYPE(T)) {
    TYPE_MODE (ts) = mode_for_size (s, MODE_FLOAT, 0);
  } else {
    TYPE_MODE (ts) = BLKmode;
  }

  pts = build_pointer_type (ts);

  expand_assignment (m3_build1 (INDIRECT_REF, ts, m3_cast (pts, stack [tos-2])),
		     m3_build1 (INDIRECT_REF, ts, m3_cast (pts, stack [tos-1])),
		     0, 0);
  tos -= 2; 
}

static void
do_zero_n ()
{
  MTYPE (cnt_t);
  MTYPE (mem_t);

  int chunk_size = TREE_INT_CST_LOW (TYPE_SIZE (mem_t)) / 8; 

  if (cnt_t != t_int) { fatal ("zero_n called with non-native count"); }

  if (chunk_size > 1) {
    stack[tos-1] = m3_build2(MULT_EXPR, cnt_t, stack[tos-1],
			     m3_build_int (chunk_size));
  }

  m3_start_call ();
  m3_swap ();
  m3_pop_param (t_addr);
  m3_pop_param (cnt_t);
  stack [tos++] = v_zero;
  m3_pop_param (t_int);
  m3_call_direct (memset_proc, t_void);
}

static void
do_zero ()
{
  INTEGER (n);
  MTYPE   (mem_t);

  int chunk_size = TREE_INT_CST_LOW (TYPE_SIZE (mem_t)) / 8; 

  m3_start_call ();
  m3_pop_param (t_addr);
  stack [tos++] = v_zero;
  m3_pop_param (t_int);
  stack [tos++] = m3_build_int (n * chunk_size); 
  m3_pop_param (t_int);
  m3_call_direct (memset_proc, t_void);
}

static void
do_loophole ()
{
  MTYPE2 (t, T);
  MTYPE2 (u, U);

  if (IS_REAL_TYPE(T) != IS_REAL_TYPE(U)) {
    tree v = declare_temp (t, 0, 0);
    m3_store (v, 0, t, t);
    m3_load (v, 0, u, U, u, U);
  } else {
    stack [tos-1] = m3_cast (u, stack [tos-1]);
  }
}

static void
do_abort ()
{
  INTEGER (code);

  generate_fault (code);
}

static void
do_check_nil ()
{
  INTEGER (code);

  tree temp1 = declare_temp (t_addr, 0, 0);

  m3_store (temp1, 0, t_addr, t_addr);
  stack [tos++] = temp1;
  expand_start_cond (m3_build2 (EQ_EXPR, t_addr, temp1, v_null), 0);
  generate_fault (code);
  expand_end_cond ();
}

static void
do_check_lo ()
{
  MTYPE          (t);
  TARGET_INTEGER (a);
  INTEGER        (code);

  tree temp1 = declare_temp (t, 0, 0);

  if (TREE_TYPE (stack [tos-1]) != t) {
    stack [tos-1] = m3_build1 (CONVERT_EXPR, t, stack [tos-1]);
  }
  m3_store (temp1, 0, t, t);
  stack [tos++] = temp1;
  expand_start_cond (m3_build2 (LT_EXPR, t_int, temp1, a), 0);
  generate_fault (code);
  expand_end_cond ();
}

static void
do_check_hi ()
{
  MTYPE          (t);
  TARGET_INTEGER (a);
  INTEGER        (code);

  tree temp1 = declare_temp (t, 0, 0);

  if (TREE_TYPE (stack [tos-1]) != t) {
    stack [tos-1] = m3_build1 (CONVERT_EXPR, t, stack [tos-1]);
  }
  m3_store (temp1, 0, t, t);
  stack [tos++] = temp1;
  expand_start_cond (m3_build2 (GT_EXPR, t_int, temp1, a), 0);
  generate_fault (code);
  expand_end_cond ();
}

static void
do_check_range ()
{
  MTYPE          (t);
  TARGET_INTEGER (a);
  TARGET_INTEGER (b);
  INTEGER        (code);

  tree temp1 = declare_temp (t, 0, 0);

  if (TREE_TYPE (stack [tos-1]) != t) {
    stack [tos-1] = m3_build1 (CONVERT_EXPR, t, stack [tos-1]);
  }
  m3_store (temp1, 0, t, t);
  stack [tos++] = temp1;
  expand_start_cond (m3_build2 (TRUTH_ORIF_EXPR, t_int,
				m3_build2 (LT_EXPR, t_int, temp1, a),
				m3_build2 (GT_EXPR, t_int, temp1, b)), 
		     0);
  generate_fault (code);
  expand_end_cond ();
}

static void
do_check_index ()
{
  MTYPE   (t);
  INTEGER (code);

  t = unsigned_type (t);
  expand_start_cond (m3_build2 (GE_EXPR, t,
				m3_build1 (CONVERT_EXPR, t, stack [tos-2]),
				m3_build1 (CONVERT_EXPR, t, stack [tos-1])),
		     0);
  generate_fault (code);
  expand_end_cond ();
  tos -= 1;
}

static void
do_check_eq ()
{
  MTYPE   (t);
  INTEGER (code);

  tree temp1 = declare_temp (t, 0, 0);
  tree temp2 = declare_temp (t, 0, 0);

  m3_store (temp1, 0, t, t);
  m3_store (temp2, 0, t, t);
  stack [tos++] = temp2;
  stack [tos++] = temp1;
  expand_start_cond (m3_build2 (NE_EXPR, t_int, temp1, temp2), 0);
  generate_fault (code);
  expand_end_cond ();
  tos -= 2;
}

static void
do_add_offset ()
{
  BYTESIZE (n);

  stack [tos-1] = m3_build2 (PLUS_EXPR, t_addr,
			     stack [tos-1], m3_build_int (n/8));
}

static void
do_index_address ()
{
  MTYPE    (t);
  BYTESIZE (n);

  HOST_WIDE_INT incr_val;
  int n_bytes = n/8;
  tree incr = stack [tos-1];

  if (n_bytes != 1) {
    if (is_small_cardinal (incr, &incr_val)
	&& (0 <= incr_val) && (incr_val < 1024)
	&& (0 <= n_bytes) && (n_bytes < 1024)) {
      incr = m3_build_int (incr_val * n_bytes);
    } else {
      incr = m3_build2 (MULT_EXPR, t, incr, m3_build_int (n_bytes));
    }
  };

  stack [tos-2] = m3_build2 (PLUS_EXPR, t_addr,
			     m3_cast (t_addr, stack [tos-2]),
			     incr);
  tos--;
}

static void
do_start_call_direct ()
{
  PROC    (p);
  INTEGER (level);
  MTYPE   (t);

  m3_start_call ();
}

static void
do_call_direct ()
{
  PROC  (p);
  MTYPE (t);

  m3_call_direct (p, t);
}

static void
do_start_call_indirect ()
{
  MTYPE   (t);
  INTEGER (call_conv);

  m3_start_call ();
}

static void
do_call_indirect ()
{
  MTYPE   (t);
  INTEGER (call_conv);

  m3_call_indirect (t);
}

static void
do_pop_param ()
{
  MTYPE (t);

  m3_pop_param (t);
}

static void
do_pop_struct ()
{
  BYTESIZE  (s);
  ALIGNMENT (a);

  tree t = build_type (T_struct, s, a);

  stack [tos-1] = m3_build1 (INDIRECT_REF, t,
			     m3_cast (build_pointer_type (t), stack [tos-1]));
  m3_pop_param (t);
}

static void
do_pop_static_link ()
{
  tree v = declare_temp (t_addr, 0, 0);

  m3_store (v, 0, TREE_TYPE (v), TREE_TYPE (v));
  call_stack_link [call_stack_top] = v;
}

static void
do_load_procedure ()
{
  PROC (p);

  /* we do this extra dance assigning to a temporary procedure
     pointer variable so that any platform-dependent thunk code
     that's needed gets emitted. */

  /**********************
  tree proc_type = TREE_TYPE (p);
  tree ptr_type  = build_pointer_type (proc_type);
  tree raw_proc  = m3_rtl (XEXP (DECL_RTL (p), 0));
  tree proc_ptr  = declare_temp (ptr_type, 0, 0);
  tree proc_val  = m3_build2 (MODIFY_EXPR, ptr_type, proc_ptr, raw_proc);
  TREE_SIDE_EFFECTS (proc_val) = 1;
  stack [tos++] = proc_val;
  ************************/

#ifdef RS6000_ARG_SIZE
  /* On rs6000/AIX we need to be careful about procedure pointers... */
  tree proc_type = TREE_TYPE (p);
  tree ptr_type  = build_pointer_type (proc_type);
  stack [tos++]  = m3_build1 (ADDR_EXPR, ptr_type, p);
#else
  /* On most platforms the Modula-3 assumption (proc = code ptr + env ptr)
     and handling are sufficient */
  stack [tos++]  = m3_rtl (XEXP (DECL_RTL (p), 0));
#endif
}

static void
do_load_static_link ()
{
  PROC (p);

  stack [tos++] = m3_rtl (lookup_static_chain (p));
}

static void
do_comment ()
{
  QUOTED_STRING (comment, len);
}

/*-------------------------------------------------------------- M3CG parser ---*/

typedef void (*OP_HANDLER)();
typedef struct { M3CG_opcode op;  OP_HANDLER proc; } OpProc;

OpProc ops[] = {
  { M3CG_BEGIN_UNIT,             do_begin_unit             },
  { M3CG_END_UNIT,               do_end_unit               },
  { M3CG_IMPORT_UNIT,            do_import_unit            },
  { M3CG_EXPORT_UNIT,            do_export_unit            },
  { M3CG_SET_SOURCE_FILE,        do_set_source_file        },
  { M3CG_SET_SOURCE_LINE,        do_set_source_line        },
  { M3CG_DECLARE_TYPENAME,       do_declare_typename       },
  { M3CG_DECLARE_ARRAY,          do_declare_array          },
  { M3CG_DECLARE_OPEN_ARRAY,     do_declare_open_array     },
  { M3CG_DECLARE_ENUM,           do_declare_enum           },
  { M3CG_DECLARE_ENUM_ELT,       do_declare_enum_elt       },
  { M3CG_DECLARE_PACKED,         do_declare_packed         },
  { M3CG_DECLARE_RECORD,         do_declare_record         },
  { M3CG_DECLARE_FIELD,          do_declare_field          },
  { M3CG_DECLARE_SET,            do_declare_set            },
  { M3CG_DECLARE_SUBRANGE,       do_declare_subrange       },
  { M3CG_DECLARE_POINTER,        do_declare_pointer        },
  { M3CG_DECLARE_INDIRECT,       do_declare_indirect       },
  { M3CG_DECLARE_PROCTYPE,       do_declare_proctype       },
  { M3CG_DECLARE_FORMAL,         do_declare_formal         },
  { M3CG_DECLARE_RAISES,         do_declare_raises         },
  { M3CG_DECLARE_OBJECT,         do_declare_object         },
  { M3CG_DECLARE_METHOD,         do_declare_method         },
  { M3CG_DECLARE_OPAQUE,         do_declare_opaque         },
  { M3CG_REVEAL_OPAQUE,          do_reveal_opaque          },
  { M3CG_DECLARE_EXCEPTION,      do_declare_exception      },
  { M3CG_SET_RUNTIME_PROC,       do_set_runtime_proc       },
  { M3CG_SET_RUNTIME_HOOK,       do_set_runtime_hook       },
  { M3CG_IMPORT_GLOBAL,          do_import_global          },
  { M3CG_DECLARE_SEGMENT,        do_declare_segment        },
  { M3CG_BIND_SEGMENT,           do_bind_segment           },
  { M3CG_DECLARE_GLOBAL,         do_declare_global         },
  { M3CG_DECLARE_CONSTANT,       do_declare_constant       },
  { M3CG_DECLARE_LOCAL,          do_declare_local          },
  { M3CG_DECLARE_PARAM,          do_declare_param          },
  { M3CG_DECLARE_TEMP,           do_declare_temp           },
  { M3CG_FREE_TEMP,              do_free_temp              },
  { M3CG_BEGIN_INIT,             do_begin_init             },
  { M3CG_END_INIT,               do_end_init               },
  { M3CG_INIT_INT,               do_init_int               },
  { M3CG_INIT_PROC,              do_init_proc              },
  { M3CG_INIT_LABEL,             do_init_label             },
  { M3CG_INIT_VAR,               do_init_var               },
  { M3CG_INIT_OFFSET,            do_init_offset            },
  { M3CG_INIT_CHARS,             do_init_chars             },
  { M3CG_INIT_FLOAT,             do_init_float             },
  { M3CG_IMPORT_PROCEDURE,       do_import_procedure       },
  { M3CG_DECLARE_PROCEDURE,      do_declare_procedure      },
  { M3CG_BEGIN_PROCEDURE,        do_begin_procedure        },
  { M3CG_END_PROCEDURE,          do_end_procedure          },
  { M3CG_BEGIN_BLOCK,            do_begin_block            },
  { M3CG_END_BLOCK,              do_end_block              },
  { M3CG_NOTE_PROCEDURE_ORIGIN,  do_note_procedure_origin  },
  { M3CG_SET_LABEL,              do_set_label              },
  { M3CG_JUMP,                   do_m3_jump                },
  { M3CG_IF_TRUE,                do_if_true                },
  { M3CG_IF_FALSE,               do_if_false               },
  { M3CG_IF_EQ,                  do_if_eq                  },
  { M3CG_IF_NE,                  do_if_ne                  },
  { M3CG_IF_GT,                  do_if_gt                  },
  { M3CG_IF_GE,                  do_if_ge                  },
  { M3CG_IF_LT,                  do_if_lt                  },
  { M3CG_IF_LE,                  do_if_le                  },
  { M3CG_CASE_JUMP,              do_case_jump              },
  { M3CG_EXIT_PROC,              do_exit_proc              },
  { M3CG_LOAD,                   do_load                   },
  { M3CG_LOAD_ADDRESS,           do_load_address           },
  { M3CG_LOAD_INDIRECT,          do_load_indirect          },
  { M3CG_STORE,                  do_store                  },
  { M3CG_STORE_INDIRECT,         do_store_indirect         },
  { M3CG_LOAD_NIL,               do_load_nil               },
  { M3CG_LOAD_INTEGER,           do_load_integer           },
  { M3CG_LOAD_FLOAT,             do_load_float             },
  { M3CG_EQ,                     do_eq                     },
  { M3CG_NE,                     do_ne                     },
  { M3CG_GT,                     do_gt                     },
  { M3CG_GE,                     do_ge                     },
  { M3CG_LT,                     do_lt                     },
  { M3CG_LE,                     do_le                     },
  { M3CG_ADD,                    do_add                    },
  { M3CG_SUBTRACT,               do_subtract               },
  { M3CG_MULTIPLY,               do_multiply               },
  { M3CG_DIVIDE,                 do_divide                 },
  { M3CG_NEGATE,                 do_negate                 },
  { M3CG_ABS,                    do_abs                    },
  { M3CG_MAX,                    do_max                    },
  { M3CG_MIN,                    do_min                    },
  { M3CG_ROUND,                  do_round                  },
  { M3CG_TRUNC,                  do_trunc                  },
  { M3CG_FLOOR,                  do_floor                  },
  { M3CG_CEILING,                do_ceiling                },
  { M3CG_CVT_FLOAT,              do_cvt_float              },
  { M3CG_DIV,                    do_div                    },
  { M3CG_MOD,                    do_mod                    },
  { M3CG_SET_UNION,              do_set_union              },
  { M3CG_SET_DIFFERENCE,         do_set_difference         },
  { M3CG_SET_INTERSECTION,       do_set_intersection       },
  { M3CG_SET_SYM_DIFFERENCE,     do_set_sym_difference     },
  { M3CG_SET_MEMBER,             do_set_member             },
  { M3CG_SET_EQ,                 do_set_eq                 },
  { M3CG_SET_NE,                 do_set_ne                 },
  { M3CG_SET_LT,                 do_set_lt                 },
  { M3CG_SET_LE,                 do_set_le                 },
  { M3CG_SET_GT,                 do_set_gt                 },
  { M3CG_SET_GE,                 do_set_ge                 },
  { M3CG_SET_RANGE,              do_set_range              },
  { M3CG_SET_SINGLETON,          do_set_singleton          },
  { M3CG_NOT,                    do_not                    },
  { M3CG_AND,                    do_and                    },
  { M3CG_OR,                     do_or                     },
  { M3CG_XOR,                    do_xor                    },
  { M3CG_SHIFT,                  do_m3_shift               },
  { M3CG_SHIFT_LEFT,             do_shift_left             },
  { M3CG_SHIFT_RIGHT,            do_shift_right            },
  { M3CG_ROTATE,                 do_m3_rotate              },
  { M3CG_ROTATE_LEFT,            do_rotate_left            },
  { M3CG_ROTATE_RIGHT,           do_rotate_right           },
  { M3CG_WIDEN,                  do_widen                  },
  { M3CG_CHOP,                   do_chop                   },
  { M3CG_EXTRACT,                do_m3_extract             },
  { M3CG_EXTRACT_N,              do_extract_n              },
  { M3CG_EXTRACT_MN,             do_extract_mn             },
  { M3CG_INSERT,                 do_m3_insert              },
  { M3CG_INSERT_N,               do_insert_n               },
  { M3CG_INSERT_MN,              do_insert_mn              },
  { M3CG_SWAP,                   do_swap                   },
  { M3CG_POP,                    do_pop                    },
  { M3CG_COPY_N,                 do_copy_n                 },
  { M3CG_COPY,                   do_copy                   },
  { M3CG_ZERO_N,                 do_zero_n                 },
  { M3CG_ZERO,                   do_zero                   },
  { M3CG_LOOPHOLE,               do_loophole               },
  { M3CG_ABORT,                  do_abort                  },
  { M3CG_CHECK_NIL,              do_check_nil              },
  { M3CG_CHECK_LO,               do_check_lo               },
  { M3CG_CHECK_HI,               do_check_hi               },
  { M3CG_CHECK_RANGE,            do_check_range            },
  { M3CG_CHECK_INDEX,            do_check_index            },
  { M3CG_CHECK_EQ,               do_check_eq               },
  { M3CG_ADD_OFFSET,             do_add_offset             },
  { M3CG_INDEX_ADDRESS,          do_index_address          },
  { M3CG_START_CALL_DIRECT,      do_start_call_direct      },
  { M3CG_CALL_DIRECT,            do_call_direct            },
  { M3CG_START_CALL_INDIRECT,    do_start_call_indirect    },
  { M3CG_CALL_INDIRECT,          do_call_indirect          },
  { M3CG_POP_PARAM,              do_pop_param              },
  { M3CG_POP_STRUCT,             do_pop_struct             },
  { M3CG_POP_STATIC_LINK,        do_pop_static_link        },
  { M3CG_LOAD_PROCEDURE,         do_load_procedure         },
  { M3CG_LOAD_STATIC_LINK,       do_load_static_link       },
  { M3CG_COMMENT,                do_comment                },
  { LAST_OPCODE,                 0                         }
  };

yyparse ()
{
  int op, i;
  int show_opcodes = 0;

  /* first, verify the handler table is complete and consistent. */
  for (i = 0;  ops[i].proc != 0;  i++ ) {
    if (i != (int)ops[i].op) { fatal ("bad opcode table"); };
  }
  if (i != (int)LAST_OPCODE) { fatal ("bad opcode table"); };


  /* check the version stamp */
  i = get_int ();
  if (i != M3CG_Version) {
    fatal ("bad M3CG version stamp (%lx), expected %lx", i, M3CG_Version);
  }

  op = (int)LAST_OPCODE;
  while (op != (int)M3CG_END_UNIT) {
    op = get_int ();   m3cg_lineno ++;
    if (op < 0 || (int)LAST_OPCODE <= op) {
      fatal ("**** bad opcode: %ld, at m3cg_lineno %d", op, m3cg_lineno);
    }
    if (show_opcodes) { warning ("(%d) %s", m3cg_lineno, M3CG_opnames[op]); }
    ops[op].proc ();
  }

  return 0;
}
