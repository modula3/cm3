/* Copyright (C) 1993, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

/* Last modified on Tue Jun 20 16:52:55 PDT 1995 by kalsow                   */
/*      modified on Wed Jun 14 08:17:29 PDT 1995 by ericv                    */
/*      modified on Thu Dec  9 14:07:21 PST 1993 by muller                   */


#include <varargs.h>
#include <stdio.h>
#include <errno.h>
#include <setjmp.h>

#include "config.h"
#include "rtl.h"
#include "tree.h"
#include "input.h"
#include "flags.h"
#include "expr.h"

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

/*  #define NEST_NESTED 1  */
/* comment out NEST_NESTED if the front-end delivers nested procedure
   bodies where they appear in the source */

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


/*-------------------------------------------------- globals and typedefs ---*/

static char *cur_char, *last_nl;
static int m3cg_lineno;

#define BUFFER_SIZE 0x10000

typedef struct input_buffer {
  struct input_buffer *next; 
  char chars [BUFFER_SIZE];
  char *cur_char, *last_nl, *last_char; } *INPUT_BUFFER;
  
static INPUT_BUFFER input_buffer;

/*-------------------------------------------------------- buffer loading ---*/

static void reload_buffer ()
{
  int n;
  char *start;

  if (input_buffer->next == 0) {
    /* we must be reading from the file */
    if (input_buffer->last_nl < input_buffer->last_char) {
      n = input_buffer->last_char - input_buffer->last_nl;
      memcpy (input_buffer->chars, input_buffer->last_nl + 1, n);
      start = input_buffer->chars + n; }
    else {
      n = 0;
      start = input_buffer->chars; }
    
    input_buffer->last_char 
      = start + fread (start, 1, BUFFER_SIZE - n, finput) - 1;
    input_buffer->last_nl = input_buffer->last_char;
    while (*input_buffer->last_nl != '\n') {
      input_buffer->last_nl--; }

    input_buffer->cur_char = input_buffer->chars; }

  else {
    input_buffer = input_buffer->next; }

  cur_char = input_buffer->cur_char;
  last_nl = input_buffer->last_nl;
}


void init_lex ()
{
  input_buffer = (INPUT_BUFFER) malloc (sizeof (struct input_buffer));
  input_buffer->next = 0;
  input_buffer->cur_char = input_buffer->last_nl = input_buffer->last_char = 0;
  reload_buffer ();
  m3cg_lineno = 1;

  set_identifier_size (sizeof (struct lang_identifier));
}

/*--------------------------------------------------------- word scanning ---*/

/* fetch the next word;
   set start to the first character of the word, 
   set stop to the last character  of the word,

   return the number of characters in the word

   the validity of the chars pointed by start and stop is guaranteed
   only on the current line. */

static char *word_start, *word_stop;
static int at_eol;

static int scan_word ()
{
  char c;

  c = *cur_char;
  while (c == ' ' || c == '\t' || c == 0 || c == '\n' || c == '\r') {
    c = *(++cur_char);
  }
  word_start = cur_char;

  while (c != ' ' && c != '\t' && c != 0 && c != '\n' && c != '\r') {
    c = *(++cur_char);
  }
  word_stop = cur_char - 1;

  at_eol = (c == '\n') || (c == '\r');
  *cur_char = 0;
  return (cur_char - word_start);
}

static void skip_to_end_of_line ()
{
  char c;

  c = *cur_char;
  while (c != '\n' && c != '\r' && !at_eol) { c = *(++cur_char); }
  m3cg_lineno++;
  if (cur_char == last_nl) {
    reload_buffer ();
  } else {
    cur_char++;
  }
}


/*------------------------------------------------------- opcode scanning ---*/

#define M3CG_OP(a,b) b,
typedef enum {
#include "m3.def"
  LAST_OPCODE} m3cg_opcode;
#undef M3CG_OP

#define M3CG_OP(a,b) a,
static char *m3cg_opcodes[] = {
#include "m3.def"
  0};
#undef M3CG_OP


static m3cg_opcode scan_opcode ()
{
  /* C tables produced by gperf version 2.5 (GNU C++ version) */
  /* Command-line: gperf -o -j1 -i 1 -C -k 1-7,10,12,13 xx.xx  */
  static m3cg_opcode wordlist[] =
    {
      0, 0, M3_SET_LABEL, 0, M3_NE, M3_EQ, 0, 0, 0, M3_GE, 0,
      0, 0, 0, 0, 0, M3_GT, M3_SET_SOURCE_FILE, M3_SET_SOURCE_LINE, 0, 0, 0,
      0, M3_FREE_TEMP, M3_NEGATE, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      M3_OR, 0, M3_POP, 0, M3_ZERO, 0, 0, 0, M3_ZERO_N, M3_NOT, M3_LE, 0,
      M3_POP_PARAM, 0, M3_IF_NE, M3_IF_EQ, 0, M3_LT, 0, M3_IF_GE, 0, 0,
      M3_ROTATE, 0, 0, 0, M3_IF_GT, 0, 0, M3_ROTATE_LEFT, 0, M3_COPY, 0,
      M3_CHECK_EQ, M3_CHECK_NIL, M3_COPY_N, 0, 0, 0, 0, 0, 0,
      M3_CHECK_RANGE, M3_ROTATE_RIGHT, 0, M3_EXTRACT, 0, M3_EXTRACT_N,
      M3_CHECK_HI, M3_EXTRACT_MN, 0, M3_XOR, 0, 0, 0, 0, 0, 0, 0, M3_IF_LE,
      0, 0, 0, 0, 0, 0, M3_IF_LT, 0, 0, 0, M3_INIT_PROC, 0, 0,
      M3_BEGIN_BLOCK, 0, M3_INIT_VAR, M3_EXIT_PROC, M3_CVT_FLOAT,
      M3_CHECK_LO, 0, M3_CHECK_INDEX, 0, M3_SWAP, 0, 0, 0, M3_FLOOR, 0, 0,
      M3_SET_NE, M3_SET_EQ, 0, 0, M3_SET_RANGE, M3_SET_GE, M3_ABS,
      M3_BEGIN_INIT, 0, 0, 0, 0, M3_SET_GT, M3_CASE_FAULT, 0, 0, 0, 0, 0,
      M3_INIT_OFFSET, 0, 0, 0, M3_INIT_INT, 0, M3_EXPORT_UNIT, M3_AND, 0, 0,
      M3_CEILING, 0, 0, 0, M3_STORE, 0, M3_INIT_FLOAT, 0, 0, 0,
      M3_STORE_REF, 0, 0, 0, M3_INSERT, 0, M3_SET_LE, M3_INSERT_N,
      M3_INSERT_MN, M3_POP_STRUCT, M3_CALL_INDIRECT, 0, 0, M3_SET_LT, 0, 0,
      0, M3_SHIFT, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, M3_MIN, 0, 0, 0, 0,
      M3_INIT_LABEL, M3_MAX, M3_DIV, 0, 0, 0, 0, M3_SHIFT_RIGHT, 0, 0, 0,
      M3_IF_FALSE, 0, 0, 0, 0, 0, 0, M3_DECLARE_SET, 0, M3_DECLARE_TEMP, 0,
      0, 0, M3_LOOPHOLE, 0, 0, M3_DECLARE_OPEN_ARRAY, 0, 0, 0, M3_TRUNC, 0,
      M3_DECLARE_PROCEDURE, 0, M3_LOAD, 0, 0, 0, M3_DECLARE_PROCTYPE, 0,
      M3_INIT_CHARS, M3_DECLARE_ARRAY, M3_SHIFT_LEFT, 0,
      M3_DECLARE_TYPENAME, M3_START_CALL_INDIRECT, 0, M3_END_INIT,
      M3_DECLARE_OBJECT, 0, M3_DECLARE_PACKED, 0, 0, 0, 0, 0, 0,
      M3_DECLARE_RECORD, 0, 0, 0, M3_END_BLOCK, 0, 0, 0, M3_DECLARE_POINTER,
      M3_STORE_INDIRECT, M3_DECLARE_INDIRECT, 0, M3_POP_STATIC_LINK,
      M3_IF_TRUE, 0, M3_DECLARE_METHOD, M3_NARROW_FAULT,
      M3_DECLARE_EXCEPTION, 0, 0, 0, 0, M3_NOTE_PROCEDURE_ORIGIN,
      M3_REVEAL_OPAQUE, 0, 0, 0, M3_LOAD_NIL, 0, M3_DECLARE_GLOBAL, 0, 0, 0,
      0, 0, M3_IMPORT_UNIT, 0, 0, M3_LOAD_INTEGER, M3_LOAD_FLOAT, 0,
      M3_BEGIN_UNIT, 0, M3_ADD, M3_DECLARE_LOCAL, 0, 0, 0, 0,
      M3_LOAD_INDIRECT, 0, 0, 0, 0, 0, M3_CALL_DIRECT, M3_SET_INTER,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, M3_SET_SING, M3_STORE_REF_INDIRECT,
      0, 0, 0, 0, 0, M3_DECLARE_RAISES, 0, M3_IMPORT_GLOBAL, M3_MOD, 0,
      M3_SET_DIFF, 0, M3_CASE_JUMP, 0, 0, 0, 0, 0, 0, M3_BIND_SEGMENT,
      0, 0, 0, 0, 0, 0, M3_END_PROCEDURE, 0, M3_ADD_OFFSET, 0, 0, 0, 0, 0,
      0, M3_JUMP, 0, M3_SUBTRACT, 0, 0, M3_DECLARE_ENUM, 0,
      M3_DECLARE_PARAM, 0, M3_DECLARE_SEGMENT, M3_DECLARE_ENUM_ELT, 0, 0, 0,
      M3_DECLARE_CONSTANT, 0, 0, 0, 0, 0, M3_SET_UNION, 0, 0, 0, 0, 0, 0, 0,
      0, 0, M3_START_CALL_DIRECT, 0, 0, 0, 0, 0, 0, M3_DIVIDE, M3_ROUND, 0,
      0, 0, 0, M3_DECLARE_FORMAL, 0, M3_GET_RUNTIME_HOOK, 0, 0, 0, 0, 0,
      M3_END_UNIT, 0, 0, 0, 0, 0, M3_TYPECASE_FAULT, 0, 0, 0, 0, 0, 0, 0,
      M3_SET_MEMBER, 0, 0, 0, 0, M3_DECLARE_OPAQUE, M3_DECLARE_SUBRANGE, 0,
      0, 0, 0, 0, 0, M3_IMPORT_PROCEDURE, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      M3_RETURN_FAULT, 0, 0, 0, M3_BEGIN_PROCEDURE, 0, 0,
      M3_LOAD_STATIC_LINK, M3_DECLARE_FIELD, M3_LOAD_PROCEDURE,
      M3_ASSERT_FAULT, 0, M3_SET_SDIFF, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, M3_INDEX_ADDRESS, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, M3_SET_RUNTIME_PROC, 0, 0, 0, 0, 0, 0, 0, 0,
      M3_LOAD_ADDRESS, 0, 0, 0, M3_MULTIPLY, 0, M3_SET_RUNTIME_HOOK, 
    };
  static const unsigned short asso_values[] =
    {
     513, 513, 513, 513, 513, 513, 513, 513, 513, 513,
     513, 513, 513, 513, 513, 513, 513, 513, 513, 513,
     513, 513, 513, 513, 513, 513, 513, 513, 513, 513,
     513, 513, 513, 513, 513, 513, 513, 513, 513, 513,
     513, 513, 513, 513, 513,   1,   1, 513, 513, 513,
     513, 513, 513, 513, 513, 513, 513, 513, 513, 513,
     513, 513, 513, 513, 513, 513, 513, 513, 513, 513,
       2, 513, 513,   1, 513, 513,   3, 513, 513, 513,
     513, 513, 513, 513, 513, 513, 513, 513, 513, 513,
     513, 513, 513, 513, 513,   1, 513,   1,  20,   8,
     149,   1,   1,   6,  16,  44,   1,  29,  46, 148,
       1,  36,   1,   2,   1, 110,   8, 209,   7,   5,
      50,  21,   1, 513, 513, 513, 513, 513,
    };
  register int hval;

  restart: /* for comments */

  switch (hval = scan_word())
    {
      default:
      case 13:  hval += asso_values[word_start[12]];
      case 12:  hval += asso_values[word_start[11]];
      case 11:
      case 10:  hval += asso_values[word_start[9]];
      case 9:
      case 8:
      case 7:   hval += asso_values[word_start[6]];
      case 6:   hval += asso_values[word_start[5]];
      case 5:   hval += asso_values[word_start[4]];
      case 4:   hval += asso_values[word_start[3]];
      case 3:   hval += asso_values[word_start[2]];
      case 2:   hval += asso_values[word_start[1]]
	                + asso_values[word_start[0]];;
                break;

      case 1:  if (word_start [0] == '.') return M3_SET_LABEL;
	       /* otherwise, it's a "#" comment */
               skip_to_end_of_line ();
               goto restart;
    }

  return wordlist[hval];
}

/*----------------------------------------------------- nested procedures ---*/

#if defined (NEST_NESTED)
static void grab_chars (from, res, last)
     char *from;
     INPUT_BUFFER *res;
     INPUT_BUFFER *last;
{
  int nb_chars = cur_char - from + 1;
  INPUT_BUFFER n = (INPUT_BUFFER) malloc (sizeof (struct input_buffer));

  memcpy (n->chars, from, nb_chars); 
  n->last_char = n->last_nl = n->chars + nb_chars - 1;
  n->cur_char = n->chars;
  n->next = 0;

  if (*last) {
    (*last)->next = n; }
  else {
    *res = n; }
  *last = n; 
}
#endif

#if defined (NEST_NESTED)
static INPUT_BUFFER copy_current_proc (from)
     char *from;
{
  INPUT_BUFFER res = 0, last = 0;
  m3cg_opcode o = M3_BEGIN_PROCEDURE;

  while (o != M3_END_PROCEDURE) {
    while (*cur_char != '\n') {
    cur_char++; }

    if (cur_char == last_nl) {
      grab_chars (from, &res, &last);
      reload_buffer ();
      from = cur_char; }

    o = scan_opcode (); }

  while (*cur_char != '\n') {
    cur_char++; }
  grab_chars (from, &res, &last);

  return res;
}
#endif


static void push_proc (p)
     INPUT_BUFFER p;
{
  INPUT_BUFFER q;

  if (p == 0) {
    fatal ("nested procedure referenced but not yet seen."); } 
  input_buffer->cur_char = cur_char;
  
  q = p;
  while (q->next) {
    q = q->next; }
  q->next = input_buffer;

  input_buffer = p;
  cur_char = input_buffer->cur_char;
  last_nl = input_buffer->last_nl;
}

/*-------------------------------------------------------- quoted strings ---*/

#define QUOTED_STRING(x,l) int l; char *x = scan_quoted_string (&l)
static char *scan_quoted_string (len)
     int *len;
{
  char c;

  c = *cur_char;
  while (c == ' ' || c == '\n' || c == '\r' || c == '\t' || c == 0) {
    c = *(++cur_char);
  }
  
  if (c == '*') {
    word_start = word_stop = cur_char;
    cur_char++;
    *len = 0;
    return 0; }
  else {
    cur_char++;			/* skip the " */
    word_start = cur_char;
    while (*cur_char != '"') { cur_char++; }
    word_stop = cur_char - 1;
    *cur_char = '\0';
    *len = cur_char - word_start;

    /* restore the quoted characters */
    { char *cp, *dp;
      int shift_by;
      for (cp = word_start; cp < word_stop; cp++) {
	if (cp[0] == '\\') {
	  if (cp[1] == '\\') {
	    shift_by = 1; }
	  else {
	    cp[0] = (cp[1] - '0') * 64 + (cp[2] - '0') * 8 + (cp[3] - '0');
	    shift_by = 3; }
	  word_stop -= shift_by;
	  *len -= shift_by;
	  for (dp = cp + 1; dp <= word_stop; dp++) {
	    dp[0] = dp [shift_by]; }
	  word_stop [1] = 0; }}}

    return word_start; }
}  

/*--------------------------------------------------------------- strings ---*/

#define STRING(x) char *x = scan_string ()
static char *scan_string ()
{
  char *res;
  res = (char *) malloc (scan_word () + 1);
  strcpy (res, word_start);
  return (res);
}

/*----------------------------------------------------------------- signs ---*/

#define SIGN(x) char x = scan_sign ()
static char scan_sign ()
{
  scan_word ();
  return *word_start;
}

/*-------------------------------------------------------------- integers ---*/

static tree
  v_zero, v_one, v_null;


#define TARGET_INTEGER(x) tree x = scan_target_int ()

#define TOTAL_PARTS ((HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR) * 2 + 2)

static tree scan_target_int ()
{
  int i = scan_word ();
  char *w = word_start;
  HOST_WIDE_INT low, hi;
  int j, k, minus;
  int parts [TOTAL_PARTS];

  if (word_start == word_stop) {
    if (word_start [0] == '0') {
      return v_zero; }
    else if (word_start [1] == '1') {
      return v_one; }}

  for (k = 0; k < TOTAL_PARTS; k++) {
    parts [k] = 0; }

  if (i < 1) {
    fatal ("******** scan_target_int: invalid int"); }
  if (*w == '-') {
    minus = 1;
    w++; }
  else {
    minus = 0; }

  for (j = minus; j < i; j++) {
    for (k = 0; k < TOTAL_PARTS; k++) {
      parts [k] *= 10;
      if (k == 0) {
	parts [k] += *w++ - '0'; }
      else {
	parts [k] += (parts [k-1] >> HOST_BITS_PER_CHAR);
	parts [k-1] &= (1 << HOST_BITS_PER_CHAR) - 1; }}}

  if (minus) {
    for (k = 0; k < TOTAL_PARTS; k++) {
      parts [k] = (~ parts [k]) & ((1 << HOST_BITS_PER_CHAR) - 1);
      if (k == 0) {
	parts [k] += 1; }
      else {
	parts [k] += (parts [k-1] >> HOST_BITS_PER_CHAR);
	parts [k-1] &= (1 << HOST_BITS_PER_CHAR) - 1; }}}

  hi = low = 0;
  for (k = 0; k < HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR; k++) {
    hi |= ((HOST_WIDE_INT) parts[k + (HOST_BITS_PER_WIDE_INT
				      / HOST_BITS_PER_CHAR)]
	   << (k * HOST_BITS_PER_CHAR));
    low |= (HOST_WIDE_INT) parts[k] << (k * HOST_BITS_PER_CHAR); }

  return build_int_2 (low, hi);
}

#define INTEGER(x) int x = scan_int()
static int scan_int ()
{
  int i;
  scan_word ();
  if (sscanf (word_start, "%d", &i) != 1) {
    fatal ("********* scan_int: invalid int"); }
  return i;
}

#define LEVEL(x)     INTEGER(x)
#define BITSIZE(x)   INTEGER(x)
#define BYTESIZE(x)  int x = 8 * scan_int()
#define ALIGNMENT(x) int x = 8 * scan_int()
#define FREQUENCY(x) INTEGER(x)
#define BIAS(x)      INTEGER(x)
#define BITOFFSET(x) INTEGER(x)
#define BYTEOFFSET(x) int x= 8 * scan_int()

/*------------------------------------------------------------- type uids ---*/
/* Modula-3 type uids are unsiged 32-bit values.  They are passed as signed
   decimal integers in the intermediate code, but converted to 6-byte, base 62
   strings of characters from here to the debugger.  To avoid surprises, these
   strings are legal C identifiers.  */

#define UID_SIZE 6

static char *fmt_uid (x)
  int x;
{
  char *res;
  unsigned digit;
  int i;

  if (x == -1) return "zzzzzz";

  res = (char *) malloc (UID_SIZE + 1);
  res[UID_SIZE] = 0;
  for (i = UID_SIZE-1; i >= 0; i--) {
    digit = ((unsigned)x) % 62;
    x = ((unsigned)x) / 62;
    if      (digit < 26) { res[i] = 'A' + digit; }
    else if (digit < 52) { res[i] = 'a' + (digit - 26); }
    else                 { res[i] = '0' + (digit - 52); }
  }

  if ((x != 0) || (res[0] < 'A') || ('Z' < res[0])) {
    fatal ("bad uid -> identifier conversion!!"); }

  return res;
}

static char *scan_uid ()
{
  INTEGER (x);
  return fmt_uid (x);
}

#define TYPEID(x)    char *x = scan_uid ()

/*----------------------------------------------------------------- float ---*/

#define FLOAT(x) char *x = scan_float()

static char *scan_float ()
{
  STRING (res);
  char *r;
  for (r = res; *r != '\0'; r++) {
    if (*r == 'X') *r = 'e'; }
  return res;
}

/*---------------------------------------------------------- fingerprints ---*/

#define FINGERPRINT(x,y) INTEGER(x); INTEGER (y)

/*-------------------------------------------------------------- booleans ---*/

#define BOOLEAN(x) int x=scan_boolean()

static int scan_boolean ()
{
  scan_word ();
  return (*word_start == 'T');
}

/*------------------------------------------------------------- variables ---*/

#define VAR(x) tree x = scan_var (ERROR_MARK)
#define RETURN_VAR(x,code) tree x = scan_var (code)

static tree all_vars [10000];

static tree scan_var (code)
     int code;
{
  int i;
  scan_word ();

  if (sscanf (word_start,"v.%d", &i) != 1) {
    fatal ("************* scan_var: invalid variable"); }

  if (i >= sizeof (all_vars) / sizeof (tree)) {
    fatal ("****** all_vars overflow"); }

  if (code == ERROR_MARK) {
    if (all_vars [i] == 0) {
      fatal ("********** variable should already exist, v.%d, line %d", 
	       i, m3cg_lineno); 
      all_vars [i] = make_node (VAR_DECL); }}
  else {
    if (all_vars [i] != 0) {
      fatal ("********** variable should not already exist, v.%d, line %d",
	       i, m3cg_lineno); }
    all_vars [i] = make_node (code);
    DECL_NAME (all_vars [i]) = NULL_TREE; }
  
  return all_vars [i];
}

/*------------------------------------------------------------ procedures ---*/

#define PROC(x) tree x = scan_proc ()

static tree all_procs [10000];

static tree scan_proc ()
{
  int i;
  scan_word ();

  if (*word_start == '*') {
    return (0); }

  else {
    if (sscanf (word_start,"p.%d", &i) != 1) {
      warning ("********* scan_proc: invalid procedure"); }

    if (i >= sizeof (all_procs) / sizeof (tree)) {
      error ("****** all_procs overflow"); }

    if (all_procs[i] == 0) {
      all_procs[i] = make_node (FUNCTION_DECL); }

    return all_procs[i]; }
}

/*----------------------------------------------------------------- types ---*/

typedef enum {
  T_int_8, T_int_16, T_int_32, T_int_32d, T_int, 
  T_word_8, T_word_16, T_word_32, T_word_32d, T_word,
  T_reel, T_lreel, T_xreel,
  T_addr, T_void, T_struct, T_LAST} type;

static tree
  t_addr, t_word, t_int, t_reel, t_lreel, t_xreel,
  t_int_8, t_int_16, t_int_32, t_int_32d, 
  t_word_8, t_word_16, t_word_32, t_word_32d, t_void;

#define TYPE(x) type x = scan_type ()
static type scan_type ()
{
  scan_word ();
  switch (*word_start) 
    {
    case 'I': switch (word_stop - word_start) {
		case 2: return T_int;
		case 4: return T_int_8;
		case 5: return (word_start [4] == '1' ? T_int_16 : T_int_32);
		case 6: return T_int_32d;
		default: break; }
              break;
    case 'W': switch (word_stop - word_start) {
		case 3: return T_word;
		case 5: return T_word_8;
		case 6: return (word_start [5] == '1' ? T_word_16 : T_word_32);
		case 7: return T_word_32d;
		default: break; }
              break;
    case 'R': return T_reel;
    case 'L': return T_lreel;
    case 'X': return T_xreel;
    case 'A': return T_addr;
    case 'V': return T_void;
    case 'S': return T_struct;
    default: break; }
    fatal ("******* invalid type, at m3cg_lineno %d", m3cg_lineno);
    /*NOTREACHED*/
}

static tree build_type (t, s, a)
     type t;
     int s;
     int a;
{
  switch (t) {

    case T_word: {
      switch (s) {
      case 0: return t_word_32d;
      case 8: return t_word_8;
      case 16: return t_word_16;
      case 32: return t_word_32;
      default:  if (s == BITS_PER_WORD) return t_word_32d;
      }
      break;
    }

    case T_int: {
      switch (s) {
      case 0: return t_int_32d;
      case 8: return t_int_8;
      case 16: return t_int_16;
      case 32: return t_int_32;
      default: if (s == BITS_PER_WORD) return t_int_32d;
      }
      break;
    }

    case T_addr:      return t_addr;
    case T_reel:      return t_reel;
    case T_lreel:     return t_lreel;
    case T_xreel:     return t_xreel;
    case T_int_8:     return t_int_8;
    case T_int_16:    return t_int_16;
    case T_int_32:    return t_int_32;
    case T_int_32d:   return t_int_32d;
    case T_word_8:    return t_word_8;
    case T_word_16:   return t_word_16;
    case T_word_32:   return t_word_32;
    case T_word_32d:  return t_word_32d;
    case T_void:      return t_void;

    case T_struct: {
      tree ts = make_node (RECORD_TYPE);
      TYPE_NAME (ts) = NULL_TREE;
      TYPE_FIELDS (ts) = NULL_TREE;
      TYPE_SIZE (ts) = m3_build_int (s);
      TYPE_ALIGN (ts) = a;
      TYPE_MODE (ts) = mode_for_size (s, MODE_INT, 1);
      /* If structure's known alignment is less than
	 what the scalar mode would need, and it matters,
	 then stick with BLKmode.  */
      if (STRICT_ALIGNMENT && ! (a >= BIGGEST_ALIGNMENT || (a >= s))) {
	if (TYPE_MODE (ts) != BLKmode)
	  /* If this is the only reason this type is BLKmode,
	     then don't force containing types to be BLKmode.  */
	  TYPE_NO_FORCE_BLK (ts) = 1;
	TYPE_MODE (ts) = BLKmode;
      }
      return ts;
    }

    default:
      break;
  } /*switch*/

  fatal ("Cannot build this type");
  /*NOTREACHED*/
}

#define MTYPE(x) tree x = scan_mtype (0)
#define MTYPE2(x,y) type y; tree x = scan_mtype (&y)

static tree scan_mtype (T)
     type *T;
{
  type TT = scan_type ();
  if (T) {
    *T = TT; }
  return build_type (TT, 0, 0);
}


/*---------------------------------------------------------------- labels ---*/

#define LABEL(l) tree  l = scan_label()

static tree all_labels[10000];

static tree scan_label ()
{
  int i;
  scan_word ();

  if (sscanf (word_start, "L.%d", &i) != 1) {
    fatal ("********* scan_label: invalid label"); }

  if (i >= sizeof (all_labels) / sizeof (tree)) {
    fatal ("***** all_labels overflow"); }

  if (all_labels[i] == 0) {
    all_labels[i] = build_decl (LABEL_DECL, NULL_TREE, t_addr); }

  return all_labels[i];
}  


/*========================================== insert, shift, rotate and co ===*/

static tree do_insert (x, y, i, n)
     tree x, y, i, n;
{
  tree a, b, c, d, e, f, g, h, j, k, l;

  a = m3_build1 (BIT_NOT_EXPR, t_word, v_zero);
  b = m3_build2 (LSHIFT_EXPR, t_word, a, n);
  c = m3_build1 (BIT_NOT_EXPR, t_word, b);
  d = m3_build2 (BIT_AND_EXPR, t_word, y, c);
  e = m3_build2 (LSHIFT_EXPR, t_word, d, i);
  f = m3_build2 (LSHIFT_EXPR, t_word, c, i);
  g = m3_build1 (BIT_NOT_EXPR, t_word, f);
  h = m3_build2 (BIT_AND_EXPR, t_word, x, g);
  j = m3_build2 (BIT_IOR_EXPR, t_word, h, e);
  k = m3_build3 (COND_EXPR, t_int,
	           m3_build2 (EQ_EXPR, t_int, n, m3_build_int (BITS_PER_WORD)),
	           y, j);
  l = m3_build3 (COND_EXPR, t_int,
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

static tree do_fixed_insert (x, y, i, n)
     tree x, y;
     int i, n;
{
  HOST_WIDE_INT y_val;

  if ((i < 0) || (BITS_PER_WORD <= i) || (n < 0) || (BITS_PER_WORD <= n)) {
    return do_insert (x, y, m3_build_int (i), m3_build_int (n)); }

  if (n == 0) return x;

  if ((n == 1) && (i < sizeof(int) * 8)) {
    if (is_small_cardinal (y, &y_val)) {
      if (y_val & 1) {
	return m3_build2 (BIT_IOR_EXPR, t_word, x, m3_build_int(1 << i));
      } else {
	return m3_build2 (BIT_AND_EXPR, t_word, x, m3_build_int(~(1 << i)));
      }
    } else { /* non-constant, 1-bit value */
      tree a, b;
      a = m3_build2 (BIT_AND_EXPR, t_word, y, v_one);
      b = m3_build2 (BIT_AND_EXPR, t_word, x, m3_build_int (~(1 << i)));
      return m3_build2 (BIT_IOR_EXPR, t_word, b, left_shift (a, i)); }}
  else { /* multi-bit value */
    tree saved_bits, new_bits;
    if (i + n < sizeof(int) * 8) {
      int mask = (1 << n) - 1;
      saved_bits = m3_build_int (~(mask << i));
      if (is_small_cardinal (y, &y_val)) {
	new_bits = m3_build_int ((y_val & mask) << i); }
      else {
        new_bits = m3_build2 (BIT_AND_EXPR, t_word, y, m3_build_int (mask));
	new_bits = left_shift (new_bits, i); };
      }
    else if (n < sizeof(int) * 8) {
      int mask = (1 << n) - 1;
      tree a = m3_build_int (mask);
      if (is_small_cardinal (y, &y_val)) {
	new_bits = m3_build_int (y_val & mask); }
      else {
        new_bits = m3_build2 (BIT_AND_EXPR, t_word, y, m3_build_int (mask)); };
      new_bits = left_shift (new_bits, i);
      saved_bits = m3_build1 (BIT_NOT_EXPR, t_word, left_shift (a, i));
      }
    else { /* n >= sizeof(int)*8 */
      tree mask;
      mask = m3_build2 (LSHIFT_EXPR, t_word, m3_build_int(~0L), m3_build_int (n));
      mask = m3_build1 (BIT_NOT_EXPR, t_word, mask);
      new_bits = left_shift (m3_build2 (BIT_AND_EXPR, t_word, y, mask), i);
      saved_bits = m3_build1 (BIT_NOT_EXPR, t_word, left_shift (mask, i));
      };
    x = m3_build2 (BIT_AND_EXPR, t_word, x, saved_bits);
    return m3_build2 (BIT_IOR_EXPR, t_word, x, new_bits); }
}

static tree do_extract (x, i, n, sign_extend)
     tree x, i, n;
     int sign_extend;
{
  tree a, b, c, d, e, f;

  a = m3_build2 (MINUS_EXPR, t_int, m3_build_int (BITS_PER_WORD), n);
  b = m3_build2 (MINUS_EXPR, t_int, a, i);
  c = m3_build1 (CONVERT_EXPR, t_word, x);
  d = m3_build2 (LSHIFT_EXPR, t_word, c, b);
  e = m3_build2 (RSHIFT_EXPR, (sign_extend ? t_int : t_word), d, a);
  f = m3_build3 (COND_EXPR, t_int,
	          m3_build2 (EQ_EXPR, t_int, n, v_zero),
	          v_zero, e);
  return f;
}

static tree do_fixed_extract (x, i, n, sign_extend)
     tree x;
     int i, n, sign_extend;
{
  int a = BITS_PER_WORD - n;
  int b = BITS_PER_WORD - n - i;
  tree c, d, e;

  if ((a < 0) || (BITS_PER_WORD <= a) || (b < 0) || (BITS_PER_WORD <= b)) {
    return do_extract (x, m3_build_int (i),
		       m3_build_int (n), sign_extend); }

  c = m3_build1 (CONVERT_EXPR, t_word, x);
  d = (b == 0) ? c : m3_build2 (LSHIFT_EXPR, t_word, c, m3_build_int (b));
  e = (a == 0) ? d : m3_build2 (RSHIFT_EXPR, (sign_extend ? t_int : t_word),
				     d, m3_build_int (a));
  return e;
}

static tree do_rotate (val, cnt, right)
     tree val, cnt;
     int right;
{
  tree a, b, c, d, e, f, g;

  a = m3_build_int (BITS_PER_WORD - 1);
  b = m3_build2 (BIT_AND_EXPR, t_int, cnt, a);
  c = m3_build2 (MINUS_EXPR, t_int, m3_build_int (BITS_PER_WORD), b);
  d = m3_build1 (CONVERT_EXPR, t_word, val);
  e = m3_build2 (LSHIFT_EXPR, t_word, d, (right) ? c : b);
  f = m3_build2 (RSHIFT_EXPR, t_word, d, (right) ? b : c);
  g = m3_build2 (BIT_IOR_EXPR, t_word, e, f);
  return g;
}

static tree do_shift (val, cnt, right)
     tree val, cnt;
     int right;
{
  tree a, b, c, d;
  HOST_WIDE_INT cnt_val;

  a = m3_build1 (CONVERT_EXPR, t_word, val);
  b = m3_build2 ((right) ? RSHIFT_EXPR : LSHIFT_EXPR, t_word, a, cnt);
  if (is_small_cardinal (cnt, &cnt_val)
      && (0 <= cnt_val) && (cnt_val < BITS_PER_WORD)) {
    return b; };
  c = m3_build2 (GE_EXPR, t_int, cnt, m3_build_int (BITS_PER_WORD));
  d = m3_build3 (COND_EXPR, t_word, c, v_zero, b);
  return d;
}

/*================================================= debugging information ===*/

static tree debug_fields = 0;
static char current_dbg_type_tag [100];
static int current_dbg_type_count1;
static int current_dbg_type_count2;
static int current_dbg_type_count3;

static void debug_tag (kind, id, va_alist)
     char kind;
     char *id;
     va_dcl
{
  va_list args;
  char *fmt;

  va_start (args);
  current_dbg_type_tag [0] = 'M';
  current_dbg_type_tag [1] = kind;
  current_dbg_type_tag [2] = '_';
  memcpy (current_dbg_type_tag + 3, id, UID_SIZE);

  fmt = va_arg (args, char *);
  vsprintf (current_dbg_type_tag + UID_SIZE + 3, fmt, args);
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

#define debug_field_id debug_field

static void debug_field_fmt (id, va_alist)
     char *id;
     va_dcl
{
  va_list args;
  char name [100];
  char *fmt;

  memcpy (name, id, UID_SIZE);
  va_start (args);
  fmt = va_arg (args, char *);
  vsprintf (name + UID_SIZE, fmt, args);
  va_end (args);

  debug_field (name);
}

static tree debug_struct ()
{
  tree t = make_node (RECORD_TYPE);
  TYPE_NAME (t) = get_identifier (current_dbg_type_tag);
  TYPE_FIELDS (t) = nreverse (debug_fields);
  debug_fields = 0;
  TYPE_SIZE (t) = v_one;
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

/* are the following DECLARE_PARAM for an IMPORT_PROCEDURE or 
   a DECLARE_PROCEDURE ? */

static int ignore_params = 0;

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
     char *id;
{
  if (name == 0 || name[0] == '*') {
    char new_name [100];
    static int anonymous_counter = 1;
    sprintf (new_name, "L_%d", anonymous_counter++);
    return get_identifier (new_name); }
  else if (strcmp (id, "AAAAAA") == 0) {
    return get_identifier (name); }
  else {
    char mangled_name [100];
    if (strcmp (id, "zzzzzz") == 0) {
      sprintf (mangled_name, "M%s", name); }
    else {
      sprintf (mangled_name, "M3_%s_%s", id, name); }
    return get_identifier (mangled_name); }
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
  if (n == 0) {
    if (v_zero == NULL_TREE) {
      v_zero = build_int_2 (0, 0); }
    return v_zero; }

  if (n == 1) {
    if (v_one == NULL_TREE) {
      v_one = build_int_2 (1, 0); }
    return v_one; }
  
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


static void m3_start_call_direct ()
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

static void m3_load (v, o, t, T)
     tree v;
     int o;
     tree t;
     type T;
{
  if (o == 0 && TREE_TYPE (v) == t) {
    stack [tos++] = v; }

  else {
    tree adr = m3_build1 (ADDR_EXPR, t_addr, v);
    if (o != 0) {
      adr = m3_build2 (PLUS_EXPR, t_addr, adr, m3_build_int (o/8)); }
    stack [tos++] = m3_build1 (INDIRECT_REF, t, 
			         m3_cast (build_pointer_type (t), adr)); }

  if (T_int_8 <= T && T < T_int && t != t_int) { 
    stack [tos-1] = m3_build1 (CONVERT_EXPR, t_int, stack [tos-1]); }
  else if (T_word_8 <= T && T < T_word && t != t_word) {
    stack [tos-1] = m3_build1 (CONVERT_EXPR, t_word, stack [tos-1]); }
}

static void m3_store (v, o, t)
     tree v;
     int o;
     tree t;
{
  tree lhs, rhs;

  if (TREE_TYPE (stack [tos-1]) == t) {
    rhs = stack [tos-1]; }
  else {
    rhs = m3_cast (t, stack [tos-1]); }
  if (o == 0 && TREE_TYPE (v) == t) {
    lhs = v; }
  else {
    tree f = make_node (FIELD_DECL);
    lhs = m3_build2 (COMPONENT_REF, t, v, f);
    TREE_TYPE (f) = t;
    DECL_ALIGN (f) = TYPE_ALIGN (t);
    DECL_SIZE (f) = TYPE_SIZE (t);
    DECL_MODE (f) = TYPE_MODE (t);
    DECL_FIELD_BITPOS (f) = m3_build_int (o);  }

  expand_assignment (lhs, rhs, 0, 0);
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

static void compareop (o, t)
     enum tree_code o;
     tree t;
{
  tree t1 = m3_cast (t, stack [tos-1]);
  tree t2 = m3_cast (t, stack [tos-2]);
  TREE_UNSIGNED (t1) = TREE_UNSIGNED (t);
  TREE_UNSIGNED (t2) = TREE_UNSIGNED (t);
  stack [tos-2] = m3_build2 (o, t_int, t2, t1);
  tos --;
}

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
  m3_start_call_direct ();
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
  m3_start_call_direct ();
  while (q--) {
    m3_pop_param (t_addr); }
  m3_call_direct (p, NULL_TREE);
}

/*---------------------------------------------------------------- faults ---*/

#define ASSERT_FAULT    0
#define RANGE_FAULT     1
#define SUBSCRIPT_FAULT 2
#define SHAPE_FAULT	3
#define NIL_FAULT	4
#define NARROW_FAULT	5
#define RETURN_FAULT	6
#define CASE_FAULT	7
#define TYPECASE_FAULT	8
#define STACK_OVERFLOW	9
#define LAST_FAULT     10

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
  char *int_uid   = fmt_uid (0x195c2a74);
  tree parm_types;

  DECL_NAME (proc) = DECL_ASSEMBLER_NAME (proc) = get_identifier ("_m3_fault");
  DECL_RESULT (proc) = build_decl (RESULT_DECL, NULL_TREE, t_void);
  DECL_CONTEXT (DECL_RESULT (proc)) = proc;
  TREE_STATIC (proc) = 1;
  TREE_PUBLIC (proc) = 0;
  DECL_CONTEXT (proc) = 0;

  DECL_NAME (parm) = DECL_ASSEMBLER_NAME (parm) = fix_name ("arg", int_uid);
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

  m3_start_call_direct ();
  stack[tos++] = m3_build1 (ADDR_EXPR, t_addr, current_segment);
  m3_pop_param (t_addr);
  stack[tos++] = fault_arg;
  m3_pop_param (t_word);
  if (fault_handler != NULL_TREE) {
    m3_call_direct (fault_handler, t_void);
  } else {
    m3_load (fault_intf, fault_offs, t_addr, T_addr);
    m3_call_indirect (t_void);
  }
  emit_barrier ();
  expand_null_return ();

  expand_end_bindings (BLOCK_VARS (current_block), 1, 0);

  expand_function_end (input_filename, lineno, 0);
  rest_of_compilation (current_function_decl);

  m3_pop_block (BLOCK_SUBBLOCKS (DECL_INITIAL (fault_proc))); 
}

static generate_fault (code)
     int code;
{
  if (fault_proc == 0) declare_fault_proc ();
  m3_start_call_direct ();
  stack[tos++] = m3_build_int ((lineno << 4) + (code & 0xf));
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


void print_lang_type ()
{
}

/*ARGSUSED*/
void incomplete_type_error (value, typ)
     tree value;
     tree typ;
{
  fatal ("******** language-dependent function called: incomplete_type_error");
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

int lang_decode_option (p) 
     char *p;
{
  fatal ("*********language-dependent function called: lang_decode_option: %s", p);
  /*NOTREACHED*/
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
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_word_32d))) {
      return t_word_32d; }}      
  else {				      
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_int_8))) {
      return t_int_8; }	      
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_int_16))) {
      return t_int_16; }	      
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_int_32))) {
      return t_int_32; }	      
    if (bits <= TREE_INT_CST_LOW (TYPE_SIZE (t_int_32d))) {
      return t_int_32d; }}

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
  if (TYPE_PRECISION (typ) == TYPE_PRECISION (t_int_8))
    return unsignedp ? t_word_8 : t_int_8;
  if (TYPE_PRECISION (typ) == TYPE_PRECISION (t_int_16))
    return unsignedp ? t_word_16 : t_int_16;
  if (TYPE_PRECISION (typ) == TYPE_PRECISION (t_int_32))
    return unsignedp ? t_word_32 : t_int_32;
  if (TYPE_PRECISION (typ) == TYPE_PRECISION (t_int_32d))
    return unsignedp ? t_word_32d : t_int_32d;
  return typ;
}

/* Return a data type that has machine mode MODE.
   If the mode is an integer,
   then UNSIGNEDP selects between signed and unsigned types.  */
tree type_for_mode (mode, unsignedp)
     enum machine_mode mode;
     int unsignedp;
{
  if (mode == TYPE_MODE (t_int_32d)) return unsignedp ? t_word_32d : t_int_32d;
  if (mode == TYPE_MODE (t_int_8))   return unsignedp ? t_word_8 : t_int_8;
  if (mode == TYPE_MODE (t_int_16))  return unsignedp ? t_word_16 : t_int_16;
  if (mode == TYPE_MODE (t_int_32))  return unsignedp ? t_word_32 : t_int_32;
  if (mode == TYPE_MODE (t_reel))    return t_reel;
  if (mode == TYPE_MODE (t_lreel))   return t_lreel;
  if (mode == TYPE_MODE (t_xreel))   return t_xreel;
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
  if (typ == t_int_32d) return t_word_32d;
  if (typ == t_int_32)  return t_word_32;
  if (typ == t_int_16)  return t_word_16;
  if (typ == t_int_8)   return t_word_8;
  fatal ("********* language-dependent function called: unsigned_type");
  /*NOTREACHED*/
}

/* Return a signed type the same as TYPE in other respects.  */
tree signed_type (typ)
     tree typ;
{
  if (!TREE_UNSIGNED (typ)) { return typ; }
  if (typ == t_word_32d) return t_int_32d;
  if (typ == t_word_32)  return t_int_32;
  if (typ == t_word_16)  return t_int_16;
  if (typ == t_word_8)   return t_int_8;
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
  t_int_32  = make_signed_type (32);
  t_int_16  = make_signed_type (16);
  t_int_8   = make_signed_type (8);
  if (BITS_PER_WORD == 32) {
    t_int_32d = t_int_32; }
  else {
    t_int_32d = make_signed_type (BITS_PER_WORD); }
  t_int     = t_int_32d;

  layout_type (t_int_32d);
  layout_type (t_int_8);
  layout_type (t_int_16);
  layout_type (t_int_32);

  integer_type_node = t_int;
  sizetype = t_int;
  char_type_node = t_int_8;

  t_word_32  = make_unsigned_type (32);
  t_word_16  = make_unsigned_type (16);
  t_word_8   = make_unsigned_type (8);
  if (BITS_PER_WORD == 32) {
    t_word_32d = t_word_32; }
  else {
    t_word_32d = make_unsigned_type (BITS_PER_WORD); }
  t_word     = t_word_32d;

  layout_type (t_word_32d);
  layout_type (t_word_8);
  layout_type (t_word_16);
  layout_type (t_word_32);

  t_reel = make_node (REAL_TYPE);  
  TYPE_PRECISION (t_reel) = FLOAT_TYPE_SIZE;
  t_lreel = make_node (REAL_TYPE); 
  TYPE_PRECISION (t_lreel) = DOUBLE_TYPE_SIZE;
  t_xreel = make_node (REAL_TYPE); 
  TYPE_PRECISION (t_xreel) = LONG_DOUBLE_TYPE_SIZE;

  layout_type (t_reel);
  layout_type (t_lreel);
  layout_type (t_xreel);

  v_zero = m3_build_int (0);    TREE_TYPE (v_zero) = t_int;
  v_one= m3_build_int (1);      TREE_TYPE (v_zero) = t_int;
  integer_zero_node = v_zero;
  integer_one_node = v_one;
  size_zero_node = v_zero;
  size_one_node = v_one;

  t_void =     make_node (VOID_TYPE);
  layout_type (t_void);
  TYPE_ALIGN (t_void) = BITS_PER_UNIT;
  void_type_node = t_void;

  t_addr = build_pointer_type (t_void);
  layout_type (t_addr);

  v_null = build_int_2 (0, 0);  TREE_TYPE (v_null) = t_addr;
  null_pointer_node = v_null;


  TYPE_NAME (t_addr) =  get_identifier ("addr");
  TYPE_NAME (t_word) =   get_identifier ("word");
  TYPE_NAME (t_int) =  get_identifier ("int");
  TYPE_NAME (t_reel) =  get_identifier ("reel");
  TYPE_NAME (t_lreel) =  get_identifier ("lreel");
  TYPE_NAME (t_xreel) =  get_identifier ("xreel");
  TYPE_NAME (t_int_8) =  get_identifier ("int_8");
  TYPE_NAME (t_int_16) =  get_identifier ("int_16");
  TYPE_NAME (t_int_32) =  get_identifier ("int_32");
  TYPE_NAME (t_int_32d) =  get_identifier ("int_32d");
  TYPE_NAME (t_word_8) =  get_identifier ("word_8");
  TYPE_NAME (t_word_16) =  get_identifier ("word_16");
  TYPE_NAME (t_word_32) =  get_identifier ("word_32");
  TYPE_NAME (t_word_32d) =  get_identifier ("word_32d");
  TYPE_NAME (t_void) =  get_identifier ("void");
}

tree getdecls ()
{
  if (current_block) {
    return BLOCK_VARS (current_block); }
  else {
    return global_vars; }
}

yyparse ()
{
  m3cg_opcode current_opcode;
  int show_opcodes = 0;

  while (1) {
    current_opcode = scan_opcode ();
    if (show_opcodes) {
      warning ("1: %s", m3cg_opcodes[current_opcode]); }

    switch (current_opcode) {
      
    case M3_SET_RUNTIME_PROC: {
      STRING (s);
      PROC (p);
      if (STREQ (s, "ReportFault")) {
	fault_handler = p; }
      break; }
      
    case M3_SET_RUNTIME_HOOK: {
      STRING (s);
      VAR (v);
      BYTEOFFSET (o);
      if (STREQ (s, "ReportFault")) {
	fault_intf = v;
	fault_offs = o; }
      break; }

    case M3_BEGIN_UNIT: {
      INTEGER (n);
      exported_interfaces = 0;
      declare_runtime_functions ();
      break; }

    case M3_END_UNIT: {
      int j;
      debug_tag ('i', "zzzzzz", "_%s", current_unit_name);
      for (j = 0; j < exported_interfaces; j++) {
	debug_field (exported_interfaces_names [j]); }
      debug_struct ();
      if (fault_proc != NULL_TREE) emit_fault_proc ();
      return 0; }

    case M3_IMPORT_UNIT: {
      STRING (n);
      break; }

    case M3_EXPORT_UNIT: {
      STRING (n);
      /* remember the set of exported interfaces */
      exported_interfaces_names [exported_interfaces++] = n;
      break; }

    case M3_SET_SOURCE_FILE: {
      STRING (s);
      input_filename = s;
      main_input_filename = s;
      emit_line_note (input_filename, lineno);
      break; }

    case M3_SET_SOURCE_LINE: {
      INTEGER (i);
      lineno = i;
      if (i > max_lineno) max_lineno = i;
      emit_line_note (input_filename, lineno);
      break; }

    case M3_DECLARE_TYPENAME: {
      TYPEID (my_id); STRING (name);
      char fullname [100];
      sprintf (fullname, "%s.%s", current_unit_name, name);

      debug_tag ('N', my_id, "");
      debug_field (fullname);
      debug_struct ();

      debug_tag ('n', "zzzzzz", "_%s", fullname);
      debug_field_id (my_id);
      debug_struct ();
      break; }

    case M3_DECLARE_ARRAY: {
      TYPEID (my_id); TYPEID (index_id); TYPEID (elts_id); BITSIZE (size);
      debug_tag ('A', my_id, "_%d", size);
      debug_field_id (index_id);
      debug_field_id (elts_id);
      debug_struct ();
      break; }

    case M3_DECLARE_OPEN_ARRAY: {
      TYPEID (my_id); TYPEID (elts_id); BITSIZE (size);
      debug_tag ('B', my_id, "_%d", size);
      debug_field_id (elts_id);
      debug_struct ();
      break; }

    case M3_DECLARE_ENUM: {
      TYPEID (my_id); INTEGER (n); BITSIZE (size);
      debug_tag ('C', my_id, "_%d", size);
      current_dbg_type_count1 = n;
      break; }

    case M3_DECLARE_ENUM_ELT: {
      STRING (n);
      debug_field (n);
      if (--current_dbg_type_count1 == 0) {
	debug_struct (); }
      break; }

    case M3_DECLARE_PACKED: {
      TYPEID (my_id); BITSIZE (size); TYPEID (target_id);
      debug_field_id (target_id);
      debug_tag ('D', my_id, "_%d", size);
      debug_struct ();
      break; }

    case M3_DECLARE_RECORD: {
      TYPEID (my_id); BITSIZE (size); INTEGER (nfields);
      debug_tag ('R', my_id, "_%d", size);
      current_dbg_type_count1 = nfields;
      current_dbg_type_count2 = 0;
      if (current_dbg_type_count1 == 0) {
	debug_struct (); }
      break; }

    case M3_DECLARE_OBJECT: {
      TYPEID (my_id); TYPEID (super_id); QUOTED_STRING (brand, brand_length);
      BOOLEAN (traced); INTEGER (nfields); INTEGER (nmethods); BITSIZE (size);
      debug_tag ('O', my_id, "_%d_%d_%d_%d_%s", 
		 POINTER_SIZE, nfields, traced, (brand ? 1:0),
		 (brand ? brand : ""));
      debug_field_id (super_id);
      current_dbg_type_count1 = nfields;
      current_dbg_type_count2 = nmethods;
      current_dbg_type_count3 = 0;
      if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
	debug_struct (); }
      break; }

    case M3_DECLARE_FIELD: {
      STRING (name); BITOFFSET (offset); BITSIZE (size); TYPEID (my_id);
      debug_field_fmt (my_id, "_%d_%d_%s", offset, size, name);
      current_dbg_type_count1--;
      if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
	debug_struct (); }
      break; }

    case M3_DECLARE_METHOD: {
      STRING (name); TYPEID (my_id);
      debug_field_fmt (my_id, "_%d_%d_%s", 
		       current_dbg_type_count3++  * GET_MODE_BITSIZE (Pmode),
		       GET_MODE_BITSIZE (Pmode), name);
      current_dbg_type_count2--;
      if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
	debug_struct (); }
      break; }

    case M3_DECLARE_SET: {
      TYPEID (my_id); TYPEID (domain_id); BITSIZE (size);
      debug_tag ('S', my_id, "_%d", size);
      debug_field_id (domain_id);
      debug_struct ();
      break; }

    case M3_DECLARE_SUBRANGE: {
      TYPEID (my_id); TYPEID (domain_id);
      STRING (min);		/* we don't really care about the value */
      STRING (max);		/* and we need to print it */
      BITSIZE (size);
      debug_tag ('Z', my_id, "_%d_%s_%s", size, min, max);
      debug_field_id (domain_id);
      debug_struct ();
      break; }

    case M3_DECLARE_POINTER: {
      TYPEID (my_id); TYPEID (target_id); QUOTED_STRING (brand, l);
      BOOLEAN (traced);
      debug_tag ('Y', my_id, "_%d_%d_%d_%s",
		 GET_MODE_BITSIZE (Pmode),
		 traced, (brand ? 1 : 0),
		 (brand ? brand : "" ));
      debug_field_id (target_id);
      debug_struct ();
      break; }

    case M3_DECLARE_INDIRECT: {
      TYPEID (my_id); TYPEID (target_id);
      debug_tag ('X', my_id, "_%d", GET_MODE_BITSIZE (Pmode));
      debug_field_id (target_id);
      debug_struct ();
      break; }

    case M3_DECLARE_PROCTYPE: {
      TYPEID (my_id);
      INTEGER (nformals);
      TYPEID (result_id); 
      INTEGER (nraises);
      INTEGER (call_conv);
      debug_tag ('P', my_id, "_%d_%c%d", GET_MODE_BITSIZE (Pmode),
		 nraises < 0 ? 'A' : 'L', MAX (nraises, 0));
      current_dbg_type_count1 = nformals;
      current_dbg_type_count2 = MAX (0, nraises);
      debug_field_id (result_id);
      if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
	debug_struct (); }
      break;  }

    case M3_DECLARE_FORMAL: {
      STRING (n); TYPEID (my_id);
      debug_field_fmt (my_id, "_%s", n);
      current_dbg_type_count1--;
      if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
	debug_struct (); }
      break; }
      
    case M3_DECLARE_RAISES: {
      STRING (n);
      debug_field (n);
      current_dbg_type_count2--;
      if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
	debug_struct (); }
      break; }

    case M3_DECLARE_OPAQUE: {
      TYPEID (my_id); TYPEID (super_id);
      /* we do not care for that, only the revelation is interesting */
      break; }

    case M3_REVEAL_OPAQUE: {
      TYPEID (my_id);
      TYPEID (v);
      debug_tag ('Q', my_id, "_%d", GET_MODE_BITSIZE (Pmode));
      debug_field_id (v);
      debug_struct ();
      break; }

    case M3_DECLARE_EXCEPTION : {
      STRING (n);
      TYPEID (t);
      BOOLEAN (raise_proc); 
      VAR (base);
      INTEGER (offset);
      /* nothing yet */
      break; }
      
    case M3_IMPORT_GLOBAL: {
      STRING (n);
      BYTESIZE (s); ALIGNMENT (a); TYPE (t);
      TYPEID (id);
      RETURN_VAR (v, VAR_DECL);

      DECL_NAME  (v) = DECL_ASSEMBLER_NAME (v) = fix_name (n, id);
      DECL_EXTERNAL (v) = 1;
      TREE_PUBLIC (v) = 1;
      fix_type (v, t, s, a);

      rest_of_decl_compilation (v, 0, 1, 0);
      assemble_external (v);
      TREE_USED (v) = 1;

      TREE_CHAIN (v) = global_vars;
      global_vars = v;
      break; }

    case M3_DECLARE_SEGMENT: {
      STRING (n);
      TYPEID (id);
      RETURN_VAR (v, VAR_DECL);
      
      DECL_NAME (v) = DECL_ASSEMBLER_NAME (v) = fix_name (n, id);
      DECL_EXTERNAL (v) = 0;
      TREE_PUBLIC (v) = 1;
      /* we really don't have an idea of what the type of this var is; 
         let's try to put something that will be good enough for all
         the uses of this var we are going to see before  
         we have a bind_segment */
      fix_type (v, T_struct, BIGGEST_ALIGNMENT, BIGGEST_ALIGNMENT);
      TREE_UNSIGNED (TREE_TYPE (v)) = 1;
      TREE_STATIC (v) = 1;

      rest_of_decl_compilation (v, 0, 1, 0); 
      TREE_CHAIN (v) = global_vars;
      global_vars = v;
      current_segment = v;

      /* do not use n, it is going to go away at the next instruction;
	 skip the 'MI_' or 'MM_' prefix. */
      current_unit_name = IDENTIFIER_POINTER (DECL_NAME (v)) + 3;

      break; }
      
    case M3_BIND_SEGMENT: {
      VAR (v);
      BYTESIZE (s); ALIGNMENT (a); TYPE (t);
      BOOLEAN (exported); BOOLEAN (initialized);

      current_segment = v;
      fix_type (v, t, s, a);
      TREE_UNSIGNED (v) = TREE_UNSIGNED (TREE_TYPE (v));
      TREE_PUBLIC (v) = exported;
      TREE_STATIC (v) = 1;
      break; }

    case M3_DECLARE_GLOBAL: {
      STRING (n);
      BYTESIZE (s); ALIGNMENT (a); TYPE (t);
      TYPEID (id);
      BOOLEAN (exported); BOOLEAN (initialized);
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
      break; }
      
    case M3_DECLARE_CONSTANT: {
      STRING (n);
      BYTESIZE (s); ALIGNMENT (a); TYPE (t);
      TYPEID (id);
      BOOLEAN (exported); BOOLEAN (initialized);
      RETURN_VAR (v, VAR_DECL);

      DECL_NAME (v) = DECL_ASSEMBLER_NAME (v) = fix_name (n, id);
      DECL_EXTERNAL (v) = 0;
      DECL_COMMON (v) = (initialized == 0);  /*** -- in gcc 2.6.0 ***/
      TREE_PUBLIC (v) = exported;
      TREE_STATIC (v) = 1;
      TREE_CONSTANT (v) = 1;
      fix_type (v, t, s, a);

      rest_of_decl_compilation (v, 0, 1, 0);
      TREE_CHAIN (v) = global_vars;
      global_vars = v;
      break; }
      
    case M3_DECLARE_LOCAL: {
      STRING (n);
      BYTESIZE (s); ALIGNMENT (a); TYPE (t);
      TYPEID (id);
      BOOLEAN (in_mem); BOOLEAN (up_level); FREQUENCY (f);
      RETURN_VAR (v, VAR_DECL);

      DECL_NAME (v) = DECL_ASSEMBLER_NAME (v) = fix_name (n, id);
      DECL_NONLOCAL (v) = up_level;
      TREE_ADDRESSABLE (v) = in_mem;
      DECL_CONTEXT (v) = current_function_decl; 
      fix_type (v, t, s, a);

      if (compiling_body) {
	TREE_CHAIN (v) = BLOCK_VARS (current_block);
	BLOCK_VARS (current_block) = v; }
      else {
	TREE_CHAIN (v) = BLOCK_VARS (BLOCK_SUBBLOCKS (DECL_INITIAL (current_function_decl)));
	BLOCK_VARS (BLOCK_SUBBLOCKS (DECL_INITIAL (current_function_decl))) = v; }

      if (compiling_body) {
	compile_local (v); }
      break; }
      
    case M3_DECLARE_PARAM: {
      STRING (n);
      BYTESIZE (s); ALIGNMENT (a); TYPE (t);
      TYPEID (id);
      BOOLEAN (in_mem); BOOLEAN (up_level); FREQUENCY (f);
      RETURN_VAR (v, PARM_DECL);

      if (ignore_params) {
	break; }

      DECL_NAME (v) = DECL_ASSEMBLER_NAME (v) = fix_name (n, id);
      DECL_NONLOCAL (v) = up_level;
      TREE_ADDRESSABLE (v) = in_mem;
      fix_type (v, t, s, a);
      DECL_ARG_TYPE (v) = TREE_TYPE (v);
      DECL_CONTEXT (v) = current_function_decl;

      TREE_CHAIN (v) = DECL_ARGUMENTS (current_function_decl);
      DECL_ARGUMENTS (current_function_decl) = v;

      if (DECL_MODE (v) == VOIDmode) {
	DECL_MODE (v) = Pmode; }

      rest_of_decl_compilation (v, 0, 0, 1);
      break; }
      
    case M3_DECLARE_TEMP: {
      BYTESIZE (s); ALIGNMENT (a); TYPE (t);
      BOOLEAN (in_mem);
      RETURN_VAR (v, VAR_DECL);

      if (t == T_void) {
	t = T_struct; }
      declare_temp (build_type (t, s, a), in_mem, v);
      break; }

    case M3_FREE_TEMP: {
      VAR (v);
      /* nothing to do */
      break; }
      
    case M3_BEGIN_INIT: {
      VAR (v);
      current_record_offset = 0;
      current_record_vals = NULL_TREE;
      current_record_type = make_node (RECORD_TYPE);
      TREE_ASM_WRITTEN (current_record_type) = 1;
      break; }

    case M3_END_INIT: {
      VAR (v);
      if (current_record_offset < TREE_INT_CST_LOW (DECL_SIZE (v))) {
	one_gap (TREE_INT_CST_LOW (DECL_SIZE (v))); }
	  
      TYPE_FIELDS (current_record_type) =
	nreverse (TYPE_FIELDS (current_record_type));
      layout_type (current_record_type);
	  
      DECL_INITIAL (v) = make_node (CONSTRUCTOR);
      TREE_CONSTANT (DECL_INITIAL (v)) = 1;
      TREE_TYPE (DECL_INITIAL (v)) = current_record_type;
      CONSTRUCTOR_ELTS (DECL_INITIAL (v)) = nreverse (current_record_vals);
      break; }

    case M3_INIT_INT: {
      BYTEOFFSET (o);
      TARGET_INTEGER (v);
      MTYPE (t);
      tree f, vv;
      int bits = TREE_INT_CST_LOW (TYPE_SIZE (t));
      one_field (o, bits, &f, &vv);
      TREE_TYPE (f) = t;
/*?????????
      if (bits <= sizeof (int) * 8) {
	TREE_INT_CST_HIGH (v) = 0;
	TREE_INT_CST_LOW (v) = 
	  TREE_INT_CST_LOW (v) & ((~0) >> (sizeof (int) * 8 - bits)); }
      else {
	TREE_INT_CST_HIGH (v) = 
	  TREE_INT_CST_HIGH (v) & ((~0) >> (2 * sizeof (int) * 8 - bits)); }
*/
      TREE_VALUE (vv) = v;
      TREE_TYPE (TREE_VALUE (vv)) = TREE_TYPE (f);
      break; }
      
    case M3_INIT_PROC: {
      BYTEOFFSET (o);
      PROC (p);

      tree f, v;
      one_field (o, POINTER_SIZE, &f, &v);
      TREE_TYPE (f) = t_addr;
      TREE_VALUE (v) = m3_rtl (XEXP (DECL_RTL (p), 0));
      break; }

    case M3_INIT_LABEL: {
      BYTEOFFSET (o);
      LABEL (l);
      tree f, v;
      one_field (o, POINTER_SIZE, &f, &v);
      TREE_TYPE (f) = t_addr;
      TREE_VALUE (v) = m3_rtl (label_rtx (l));
      break; }

    case M3_INIT_VAR: {
      BYTEOFFSET (o);
      VAR (v);
      BYTEOFFSET (b);
      tree F, V;
      one_field (o, POINTER_SIZE, &F, &V);
      TREE_TYPE (F) = t_addr;
      TREE_VALUE (V) = m3_build2 (PLUS_EXPR, t_addr,
			           m3_build1 (ADDR_EXPR, t_addr, v),
			           m3_build_int (b / 8));
      break; }

    case M3_INIT_OFFSET: {
      BYTEOFFSET (o);
      VAR (v);
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
      { rtx r = DECL_RTL (v);	/* (mem ...) */
	r = XEXP (r, 0);	/* (plus ...) or (reg ...) */
	if (REG_P (r)) {
	  j = 0; }
	else {
	  r = XEXP (r, 1);	/* (const_int ...) */
	  j = XINT (r, 0);  /* offset */ }}
      TREE_VALUE (V) = m3_build_int (j);
      break; }

    case M3_INIT_CHARS: {
      BYTEOFFSET (o);
      QUOTED_STRING (s, l);
      tree f, v;
      one_field (o, l * 8, &f, &v);
      TREE_TYPE (f) = make_node (ARRAY_TYPE);
      TYPE_SIZE (TREE_TYPE (f)) = DECL_SIZE (f);
      TYPE_ALIGN (TREE_TYPE (f)) = DECL_ALIGN (f); 
	  
      TREE_VALUE (v) =  build_string (l, s);
      TREE_TYPE (TREE_VALUE (v)) = TREE_TYPE (f);
      break; }
      
    case M3_INIT_FLOAT: {
      BYTEOFFSET (o);
      STRING (xx);
      FLOAT (f);

      tree F, V;
      int s;
      switch (xx[0])
	{
	case 'R': s = FLOAT_TYPE_SIZE; break; 
	case 'L': s = DOUBLE_TYPE_SIZE; break; 
	case 'X': s = LONG_DOUBLE_TYPE_SIZE; break; }
      one_field (o, s, &F, &V);
      switch (xx[0])
	{
	case 'R': TREE_TYPE (F) = t_reel; break;
	case 'L': TREE_TYPE (F) = t_lreel; break;
	case 'X': TREE_TYPE (F) = t_xreel; break; }
      TREE_VALUE (V) = 
	build_real (TREE_TYPE (F), 
		    REAL_VALUE_ATOF (f, TYPE_MODE (TREE_TYPE (F))));
      break; }


    case M3_IMPORT_PROCEDURE: {
      STRING (n);
      INTEGER (n_params);
      MTYPE2 (return_type, ret_type);
      INTEGER (call_conv);
      PROC (p);

      /** 4/30/96 -- WKK -- It seems gcc can't hack small return values... */
      if (T_int_8 <= ret_type && ret_type <= T_int_32d) {
	return_type = t_int; }
      if (T_word_8 <= ret_type && ret_type <= T_word_32d) {
	return_type = t_word; }

      ignore_params = 1;

      declare_external_proc (p, n, 
			     build_function_type (return_type, NULL_TREE));
      break; }
      
    case M3_DECLARE_PROCEDURE: {
      STRING (n);
      INTEGER (n_params);
      MTYPE2 (return_type, ret_type);
      LEVEL (lev);
      INTEGER (call_conv);
      BOOLEAN (exported);
      PROC (parent);
      PROC (p);

      tree parm_block = make_node (BLOCK);
      tree top_block  = make_node (BLOCK);

      /** 4/30/96 -- WKK -- It seems gcc can't hack small return values... */
      if (T_int_8 <= ret_type && ret_type <= T_int_32d) {
	return_type = t_int; }
      if (T_word_8 <= ret_type && ret_type <= T_word_32d) {
	return_type = t_word; }

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
      break; }
      
    case M3_NOTE_PROCEDURE_ORIGIN: {
      PROC (p);
      skip_to_end_of_line ();
      push_proc ((INPUT_BUFFER) DECL_LANG_SPECIFIC (p));
      break; }

    case M3_BEGIN_PROCEDURE: {
      PROC (p);
#if defined (NEST_NESTED)
      if (DECL_CONTEXT (p)) {
	DECL_LANG_SPECIFIC (p) = 
	  (struct lang_decl *) copy_current_proc (word_start); }
      else {
#endif

      tree parm, local, args_types;

      DECL_SOURCE_LINE (p) = lineno;
      args_types = tree_cons (NULL_TREE, t_void, NULL_TREE);
      for (parm = DECL_ARGUMENTS (p); parm; parm = TREE_CHAIN (parm)) {
	args_types = tree_cons (NULL_TREE, TREE_TYPE (parm), args_types); }
      TREE_TYPE (p) =
	build_function_type (TREE_TYPE (DECL_RESULT (p)), args_types);
      DECL_ARGUMENTS (p) = nreverse (DECL_ARGUMENTS (p));
      make_function_rtl (p);
      
      if (DECL_CONTEXT (p)) {
	push_function_context (); } 
      else {
	compiling_body = 1; }
      
      current_function_decl = p;
      current_function_name = IDENTIFIER_POINTER (DECL_NAME (p));

      init_function_start (p, input_filename, lineno);
      expand_function_start (p, 0);
      
      m3_push_block (BLOCK_SUBBLOCKS (DECL_INITIAL (p))); 

      /* compile the locals we have already seen */
      for (local = BLOCK_VARS (current_block);
	   local; local = TREE_CHAIN (local)) {
	compile_local (local); }
      
      clear_last_expr ();
      expand_start_bindings (0);
#if defined(NEST_NESTED)
 }
#endif
      break; }

    case M3_END_PROCEDURE: {
      PROC (p);

      expand_end_bindings (BLOCK_VARS (current_block), 1, 0);

      expand_function_end (input_filename, lineno, 0);
      rest_of_compilation (current_function_decl);

      m3_pop_block (BLOCK_SUBBLOCKS (DECL_INITIAL (p))); 

      if (DECL_CONTEXT (p)) {
	pop_function_context (); } 
      else {
	compiling_body = 0; }
      break; }

    case M3_BEGIN_BLOCK: {
      m3_push_block (NULL_TREE);
      clear_last_expr ();
      expand_start_bindings (0);
      break; }
      
    case M3_END_BLOCK: {
      expand_end_bindings (BLOCK_VARS (current_block), 1, 0);
      m3_pop_block (NULL_TREE);
      break; }
      
    case M3_SET_LABEL: {
      LABEL (l);
      BOOLEAN (barrier);

      DECL_CONTEXT (l) = current_function_decl;
      expand_label (l);
      if (barrier) {
	LABEL_PRESERVE_P (label_rtx (l)) = 1; }
      break; }
 
     case M3_JUMP: { LABEL (l);
      expand_goto (l);
      break; }

    case M3_IF_TRUE: { LABEL (l); FREQUENCY (f);
      do_jump (stack [--tos], NULL_RTX, label_rtx (l));
      break; }

    case M3_IF_FALSE: { LABEL (l); FREQUENCY (f);
      do_jump (stack [--tos], label_rtx (l), NULL_RTX);
      break; }
		    
    case M3_IF_EQ: {LABEL (l); MTYPE (t);  condop (EQ_EXPR, l, t); break;}
    case M3_IF_NE: {LABEL (l); MTYPE (t);  condop (NE_EXPR, l, t); break;}
    case M3_IF_GT: {LABEL (l); MTYPE (t);  condop (GT_EXPR, l, t); break;}
    case M3_IF_GE: {LABEL (l); MTYPE (t);  condop (GE_EXPR, l, t); break;}
    case M3_IF_LT: {LABEL (l); MTYPE (t);  condop (LT_EXPR, l, t); break;}
    case M3_IF_LE: {LABEL (l); MTYPE (t);  condop (LE_EXPR, l, t); break;}
      
    case M3_CASE_JUMP: { 
#ifdef pa_cpu_attr
      /* generating a table of PIC labels on HPUX is too hard.
         so we'll generate a C switch statement where the case
	 bodies are just "goto"s. */
      if (flag_pic)
      {
        INTEGER (n);
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
        INTEGER (n);

        tree table_type = build_array_type (t_addr,
		            build_index_type (m3_build_int (n-1)));
        tree table = make_node (VAR_DECL);
        tree labels = NULL_TREE;
        tree dest_label;
        int i;

        for (i = 0; i < n; i++) {
 	  LABEL (ll);
 	  tree l = m3_build1 (ADDR_EXPR, t_addr, ll);
          TREE_CONSTANT (l) = 1;
	  if (labels == NULL_TREE) {
	    labels = build_tree_list (NULL_TREE, l); }
	  else {
	     labels = tree_cons (NULL_TREE, l, labels); }}
      
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
      break; }
      
    case M3_EXIT_PROC: {
      MTYPE (t);
      if (t == t_void) {
	expand_null_return (); }
      else {
	tree res = m3_build2 (MODIFY_EXPR, t,
			        DECL_RESULT (current_function_decl),
			        stack [tos-1]);
	TREE_SIDE_EFFECTS (res) = 1;
	expand_return (res);
	tos--; }
      break;}

    case M3_LOAD: {
      VAR (v);
      BYTEOFFSET (o);
      MTYPE2 (t, T);

      m3_load (v, o, t, T);


      break; }

    case M3_LOAD_ADDRESS: {
      VAR (v);
      BYTEOFFSET (o);
      stack [tos] = m3_build1 (ADDR_EXPR, t_addr, v);
      if (o != 0) {
	stack [tos] = m3_build2 (PLUS_EXPR, t_addr, 
			           stack [tos], m3_build_int (o / 8));
      }
      tos++;
      break; }

    case M3_LOAD_INDIRECT: {
      BYTEOFFSET (o);
      MTYPE2 (t, T);

      stack [tos-1] = m3_build1 (INDIRECT_REF, t, 
			         m3_cast (build_pointer_type (t),
				      m3_build2 (PLUS_EXPR, t_addr,
					         stack [tos-1],
					         m3_build_int (o/8))));
      if (T_int_8 <= T && T <= T_int_32d) {
	stack [tos-1] = m3_build1 (CONVERT_EXPR, t_int, 
				   stack [tos-1]); }
      if (T_word_8 <= T && T <= T_word_32d) {
	stack [tos-1] = m3_build1 (CONVERT_EXPR, t_word,
				   stack [tos-1]); }
      break; }

    case M3_STORE: {
      VAR (v);
      BYTEOFFSET (o);
      MTYPE (t);
      m3_store (v, o, t);
      break; }

    case M3_STORE_INDIRECT: {
      BYTEOFFSET (o);
      MTYPE (t);

      tree lhs = m3_build1 (INDIRECT_REF, t, 
			    m3_cast (build_pointer_type (t),
				 m3_build2 (PLUS_EXPR, t_addr,
				 	    stack[tos-2],
					    m3_build_int (o/8))));
      expand_assignment (lhs, m3_build1 (CONVERT_EXPR, t, stack [tos-1]), 0,0);
      tos -= 2;
      break; }
      
    case M3_STORE_REF:
    case M3_STORE_REF_INDIRECT: {
      fatal ("********* not handled: %s", m3cg_opcodes[current_opcode]);
      break; }

    case M3_LOAD_NIL: {
      stack [tos++] = m3_build1 (CONVERT_EXPR, t_addr, v_zero);
      break; }
      
    case M3_LOAD_INTEGER: {
      TARGET_INTEGER (n);
      stack [tos++] = n;
      break; }
	
    case M3_LOAD_FLOAT: {
      STRING (xx);
      FLOAT (f);
      tree t;
      switch (xx[0])
	{
	case 'R': t = t_reel; break; 
	case 'L': t = t_lreel; break;
	case 'X': t = t_xreel; break; };
      stack [tos++] = m3_build_real (f, t);
      break; }

    case M3_EQ:       { MTYPE (t); compareop (EQ_EXPR,       t);       break; }
    case M3_NE:       { MTYPE (t); compareop (NE_EXPR,       t);       break; }
    case M3_GT:       { MTYPE (t); compareop (GT_EXPR,       t);       break; }
    case M3_GE:       { MTYPE (t); compareop (GE_EXPR,       t);       break; }
    case M3_LT:       { MTYPE (t); compareop (LT_EXPR,       t);       break; }
    case M3_LE:       { MTYPE (t); compareop (LE_EXPR,       t);       break; }
    case M3_ADD:      { MTYPE (t); binaryop  (PLUS_EXPR,     t);       break; }
    case M3_SUBTRACT: { MTYPE (t); binaryop  (MINUS_EXPR,    t);       break; }
    case M3_MULTIPLY: { MTYPE (t); binaryop  (MULT_EXPR,     t);       break; }
    case M3_DIVIDE:   { MTYPE (t); binaryop  (RDIV_EXPR,     t);       break; }
    case M3_NEGATE:   { MTYPE (t); unaryop   (NEGATE_EXPR,   t);       break; }
    case M3_ABS:      { MTYPE (t); unaryop   (ABS_EXPR,      t);       break; }

#if 0
    case M3_MIN:      { MTYPE (t); binaryop (MIN_EXPR,       t);       break; }
    case M3_MAX:      { MTYPE (t); binaryop (MAX_EXPR,       t);       break; }
#endif

    case M3_MAX: {
      MTYPE (t);
      tree temp1 = declare_temp (t, 0, 0);
      tree temp2 = declare_temp (t, 0, 0);
      tree t1 = m3_build2 (MODIFY_EXPR, t, temp1, stack [tos-1]);
      tree t2 = m3_build2 (MODIFY_EXPR, t, temp2, stack [tos-2]);
      tree res; 
      TREE_SIDE_EFFECTS (t1) = 1;
      TREE_SIDE_EFFECTS (t2) = 1;
      res = m3_build3 (COND_EXPR, t, 
		       m3_build2 (LE_EXPR, t_int, temp2, temp1), temp1, temp2);
      stack [tos-2] = m3_build2 (COMPOUND_EXPR, t,
			         m3_build2 (COMPOUND_EXPR, t, t1, t2), res);
      tos--;
      break; }

    case M3_MIN: {
      MTYPE (t);
      tree temp1 = declare_temp (t, 0, 0);
      tree temp2 = declare_temp (t, 0, 0);

      tree t1 = m3_build2 (MODIFY_EXPR, t, temp1, stack [tos-1]);
      tree t2 = m3_build2 (MODIFY_EXPR, t, temp2, stack [tos-2]);
      tree res; 
      TREE_SIDE_EFFECTS (t1) = 1;
      TREE_SIDE_EFFECTS (t2) = 1;
      res = m3_build3 (COND_EXPR, t, 
		   m3_build2 (LE_EXPR, t_int, temp1, temp2), temp1, temp2);
      stack [tos-2] = m3_build2 (COMPOUND_EXPR, t,
			     m3_build2 (COMPOUND_EXPR, t, t1, t2), res);
      tos--;
      break; }

    case M3_TRUNC:    { MTYPE (t); unaryop (FIX_TRUNC_EXPR,  t_int);   break; }

    case M3_ROUND: {
      MTYPE (t); 
      tree temp1 = declare_temp (t_lreel, 0, 0);

      if (t == t_reel) {
	t = t_lreel;
	unaryop (CONVERT_EXPR, t); }

      { tree t1 = m3_build2 (MODIFY_EXPR, t, temp1, stack [tos-1]);
	tree zero = m3_build_real ("0.0", t);
	tree half = m3_build_real ("0.5", t);
	tree res;
	TREE_SIDE_EFFECTS (t1) = 1;

	res = m3_build1 (FIX_TRUNC_EXPR, t_int,
		      m3_build3 (COND_EXPR, t,
			     m3_build2 (GE_EXPR, t, temp1, zero),
			     m3_build2 (PLUS_EXPR, t, temp1, half),
			     m3_build2 (MINUS_EXPR, t, temp1, half)));
	stack [tos-1] = m3_build2 (COMPOUND_EXPR, t_int, t1, res); }
      break; }

    case M3_FLOOR: {
      MTYPE (t);
      tree temp1 = declare_temp (t, 0, 0);
      tree temp2 = declare_temp (t_int, 0, 0);
      tree t1 = m3_build2 (MODIFY_EXPR, t, temp1, stack[tos-1]);
      tree t2 = m3_build2 (MODIFY_EXPR, t_int, 
			   temp2, m3_build1 (FIX_TRUNC_EXPR, t_int, temp1));
      tree zero = m3_build_real ("0.0", t);
      tree res;
      TREE_SIDE_EFFECTS (t1) = 1;
      TREE_SIDE_EFFECTS (t2) = 1;
      res = m3_build3 (COND_EXPR, t_int,
		   m3_build2 (GE_EXPR, t, temp1, zero),
		   temp2,
		   m3_build3 (COND_EXPR, t_int,
			  m3_build2 (EQ_EXPR, t, 
				     temp1, build1 (FLOAT_EXPR, t, temp2)),
			  temp2,
			  m3_build2 (MINUS_EXPR, t_int,
				     temp2, v_one)));
      stack [tos-1] = m3_build2 (COMPOUND_EXPR, t_int,
			     m3_build2 (COMPOUND_EXPR, t_int, t1, t2), res);
      break; }

    case M3_CEILING: {
      MTYPE (t);
      tree temp1 = declare_temp (t, 0, 0);
      tree temp2 = declare_temp (t_int, 0, 0);
      tree t1 = m3_build2 (MODIFY_EXPR, t, temp1, stack [tos-1]);
      tree t2 = m3_build2 (MODIFY_EXPR, t_int,
		            temp2, m3_build1 (FIX_TRUNC_EXPR, t_int, temp1));
      tree zero = m3_build_real ("0.0", t);
      tree res;
      TREE_SIDE_EFFECTS (t1) = 1;
      TREE_SIDE_EFFECTS (t2) = 1;

      res = m3_build3 (COND_EXPR, t_int,
		   m3_build2 (LE_EXPR, t, temp1, zero),
		   temp2,
		   m3_build3 (COND_EXPR, t_int,
			  m3_build2 (EQ_EXPR, t, temp1,
				 m3_build1 (FLOAT_EXPR, t, temp2)),
			  temp2,
			  m3_build2 (PLUS_EXPR, t_int, temp2, v_one)));
      stack [tos-1] = m3_build2 (COMPOUND_EXPR, t_int,
			     m3_build2 (COMPOUND_EXPR, t_int, t1, t2), res);
      break; }

    case M3_CVT_FLOAT: {
      MTYPE (t);
      MTYPE (u);
      if (t == t_word || t == t_int) {
	unaryop (FLOAT_EXPR, u); }
      else {
	unaryop (CONVERT_EXPR, u); }
      break; }

    case M3_DIV:
    case M3_MOD: {
      MTYPE2 (t, T);
      SIGN (a);
      SIGN (b);
      if ((b == 'P' && a == 'P') || (T_word_8 <= T && T <= T_word)) {
	stack [tos-2] = m3_cast (t_word, stack [tos-2]);
	stack [tos-1] = m3_cast (t_word, stack [tos-1]);
	binaryop (current_opcode == M3_DIV ? FLOOR_DIV_EXPR : FLOOR_MOD_EXPR,
		  t); }
      else {
	m3_start_call_direct ();
	m3_pop_param (t_int);
	m3_pop_param (t_int);
	m3_call_direct (current_opcode == M3_DIV ? div_proc : mod_proc, 
			NULL_TREE); }
      break; }

    case M3_SET_UNION:  { BYTESIZE (n); setop (set_union_proc,  n, 3); break; }
    case M3_SET_DIFF:   { BYTESIZE (n); setop (set_diff_proc,   n, 3); break; }
    case M3_SET_INTER:  { BYTESIZE (n); setop (set_inter_proc,  n, 3); break; }
    case M3_SET_SDIFF:  { BYTESIZE (n); setop (set_sdiff_proc,  n, 3); break; }
    case M3_SET_MEMBER: { BYTESIZE (n); setop2 (set_member_proc, 2); break; }
    case M3_SET_EQ:     { BYTESIZE (n); setop (set_eq_proc,     n, 2); break; }
    case M3_SET_NE:     { BYTESIZE (n); setop (set_ne_proc,     n, 2); break; }
    case M3_SET_LT:     { BYTESIZE (n); setop (set_lt_proc,     n, 2); break; }
    case M3_SET_LE:     { BYTESIZE (n); setop (set_le_proc,     n, 2); break; }
    case M3_SET_GT:     { BYTESIZE (n); setop (set_gt_proc,     n, 2); break; }
    case M3_SET_GE:     { BYTESIZE (n); setop (set_ge_proc,     n, 2); break; }
    case M3_SET_RANGE:  { BYTESIZE (n); setop2 (set_range_proc,  3); break; }
    case M3_SET_SING:   { BYTESIZE (n); setop2 (set_sing_proc,   2); break; }


    case M3_NOT: { unaryop  (BIT_NOT_EXPR, t_word); break; }
    case M3_AND: { binaryop (BIT_AND_EXPR, t_word); break; }
    case M3_OR:  { binaryop (BIT_IOR_EXPR, t_word); break; }
    case M3_XOR: { binaryop (BIT_XOR_EXPR, t_word); break; }


    case M3_SHIFT:  {
      tree temp1 = declare_temp (t_int, 0, 0);
      tree temp2 = declare_temp (t_int, 0, 0);
      tree t1 = m3_build2 (MODIFY_EXPR, t_int, temp1, stack [tos-1]);
      tree t2 = m3_build2 (MODIFY_EXPR, t_int, temp2, stack [tos-2]);
      tree res;

      TREE_SIDE_EFFECTS (t1) = 1;
      TREE_SIDE_EFFECTS (t2) = 1;
      res  = m3_build3 (COND_EXPR, t_word,
		    m3_build2 (GE_EXPR, t_int, temp1, v_zero),
		    do_shift (temp2, temp1, 0),
		    do_shift (temp2, m3_build1 (NEGATE_EXPR, t_int, temp1),1));
      stack [tos-2] = m3_build2 (COMPOUND_EXPR, t_int,
			     m3_build2 (COMPOUND_EXPR, t_int, t1, t2), res);
      tos--; 
      break; }


    case M3_SHIFT_LEFT: {
      stack [tos-2] = do_shift (stack[tos-2], stack[tos-1], 0);
      tos--;
      break; }

    case M3_SHIFT_RIGHT: {
      stack [tos-2] = do_shift (stack[tos-2], stack[tos-1], 1);
      tos--;
      break; }

    case M3_ROTATE: {
      tree temp1 = declare_temp (t_int, 0, 0);
      tree temp2 = declare_temp (t_int, 0, 0);
      tree t1 = m3_build2 (MODIFY_EXPR, t_int, temp1, stack [tos-1]);
      tree t2 = m3_build2 (MODIFY_EXPR, t_int, temp2, stack [tos-2]);
      tree res;

      TREE_SIDE_EFFECTS (t1) = 1;
      TREE_SIDE_EFFECTS (t2) = 1;

      res = m3_build3 (COND_EXPR, t_word,
		   m3_build2 (GE_EXPR, t_int, temp1, v_zero),
		   do_rotate (temp2, temp1, 0),
		   do_rotate (temp2, m3_build1 (NEGATE_EXPR, t_int, temp1),1));
      stack [tos-2] = m3_build2 (COMPOUND_EXPR, t_int,
			     m3_build2 (COMPOUND_EXPR, t_int, t1, t2), res);
      tos--;
      break; }

    case M3_ROTATE_LEFT: {
      stack [tos-2] = do_rotate (stack[tos-2], stack[tos-1], 0);
      tos--;
      break; }

    case M3_ROTATE_RIGHT: {
      stack [tos-2] = do_rotate (stack[tos-2], stack[tos-1], 1);
      tos--;
      break; }
      
    case M3_EXTRACT: {
      BOOLEAN (b);
      stack [tos-3] = do_extract (stack[tos-3], stack[tos-2], stack[tos-1], b);
      tos -= 2;
      break; }

    case M3_EXTRACT_N: {
      BOOLEAN (b);
      INTEGER (n);
      stack [tos-2] = do_extract (stack[tos-2], stack[tos-1], 
				  m3_build_int (n), b);
      tos -= 1;
      break; }

    case M3_EXTRACT_MN: {
      BOOLEAN (b);
      INTEGER (m);
      INTEGER (n);
      stack [tos-1] = do_fixed_extract (stack[tos-1], m, n, b);
      break; }

    case M3_INSERT: {
      stack [tos-4] = do_insert (stack[tos-4], stack[tos-3], 
				 stack[tos-2], stack[tos-1]);
      tos -= 3;
      break; }

    case M3_INSERT_N: {
      INTEGER (n);
      stack [tos-3] = do_insert (stack[tos-3], stack[tos-2], stack[tos-1],
				 m3_build_int (n));
      tos -= 2;
      break; }
    
    case M3_INSERT_MN: {
      INTEGER (m);
      INTEGER (n);
      stack [tos-2] = do_fixed_insert (stack[tos-2], stack[tos-1], m, n);
      tos--;
      break; }

    case M3_SWAP: {
      MTYPE (t);
      MTYPE (u);
      m3_swap ();
      break; }
      
    case M3_POP: {
      MTYPE (t);
      TREE_SIDE_EFFECTS (stack [tos-1]) = 1;
      expand_expr_stmt (stack [--tos]);
      break; }

    case M3_COPY: {
      INTEGER (n);
      MTYPE2 (t, T);
      BOOLEAN (overlap);

      tree pts;
      tree ts = make_node (LANG_TYPE);
      int s = n * TREE_INT_CST_LOW (TYPE_SIZE (t));

      TYPE_SIZE (ts) = m3_build_int (s);
      TYPE_ALIGN (ts) = TYPE_ALIGN (t);
      if (T_reel <= T && T <= T_xreel) {
	TYPE_MODE (ts) = mode_for_size (s, MODE_FLOAT, 0); }
      else {
	TYPE_MODE (ts) = BLKmode; }
      
      pts = build_pointer_type (ts);
      expand_assignment (m3_build1 (INDIRECT_REF, ts,
				 m3_cast (pts, stack [tos-2])),
			 m3_build1 (INDIRECT_REF, ts,
				 m3_cast (pts, stack [tos-1])),
			 0, 0);
      tos -= 2; 
      break; }

    case M3_COPY_N: {
      MTYPE (t);
      BOOLEAN (overlap);
      m3_start_call_direct ();
      { tree tmp = stack [tos-3];
	stack [tos-3] = stack [tos-2];
	stack [tos-2] = stack [tos-1];
	stack [tos-1] = tmp; }
      m3_pop_param (t_addr);
      m3_swap ();
      m3_pop_param (t_addr);
      stack [tos-1] = m3_build2 (MULT_EXPR, t_int, 
			     stack [tos-1],
			     m3_build_int(TREE_INT_CST_LOW (TYPE_SIZE(t))/8));
      m3_pop_param (t_int);
      m3_call_direct (overlap ? memmove_proc : memcpy_proc, t_void);
      break; }

    case M3_ZERO: {
      INTEGER (n);
      MTYPE (t);

      m3_start_call_direct ();
      m3_pop_param (t_addr);
      stack [tos++] = v_zero;
      m3_pop_param (t_int);
      stack [tos++] = m3_build_int ((n*TREE_INT_CST_LOW (TYPE_SIZE (t)))/ 8); 
      m3_pop_param (t_int);
      m3_call_direct (memset_proc, t_void);
      break; }

    case M3_ZERO_N: {
      MTYPE (t);
      m3_start_call_direct ();
      m3_swap ();
      m3_pop_param (t_addr);
      m3_pop_param (t_int);
      stack [tos++] = v_zero;
      m3_pop_param (t_int);
      m3_call_direct (memset_proc, t_void);
      break; }

    case M3_LOOPHOLE: {
      MTYPE2 (t, T);
      MTYPE2 (u, U);

      if ((T_reel <= T && T <= T_xreel) != (T_reel <= U &&  U <= T_xreel)) {
	tree v = declare_temp (t, 0, 0);
	m3_store (v, 0, t);
	m3_load (v, 0, u, U); }
      else {
	stack [tos-1] = m3_cast (u, stack [tos-1]); }
      break; }

    case M3_ASSERT_FAULT:   { generate_fault (ASSERT_FAULT); break; }
    case M3_NARROW_FAULT:   { generate_fault (NARROW_FAULT); break; }
    case M3_RETURN_FAULT:   { generate_fault (RETURN_FAULT); break; }
    case M3_CASE_FAULT:     { generate_fault (CASE_FAULT); break; }
    case M3_TYPECASE_FAULT: { generate_fault (TYPECASE_FAULT); break; }

    case M3_CHECK_NIL: {
      tree temp1 = declare_temp (t_addr, 0, 0);
      m3_store (temp1, 0, t_addr);
      stack [tos++] = temp1;

      expand_start_cond (m3_build2 (EQ_EXPR, t_addr, 
				temp1,
				m3_build1 (CONVERT_EXPR, t_addr,
					v_zero)), 0);
      generate_fault (NIL_FAULT);
      expand_end_cond ();
      break; }

    case M3_CHECK_LO: {
      TARGET_INTEGER (a);
      tree temp1 = declare_temp (t_int, 0, 0);

      if (TREE_TYPE (stack [tos-1]) != t_int) {
	stack [tos-1] = m3_build1 (CONVERT_EXPR, t_int, stack [tos-1]); }
      m3_store (temp1, 0, t_int);
      stack [tos++] = temp1;

      expand_start_cond (m3_build2 (LT_EXPR, t_int, temp1, a), 0);
      generate_fault (RANGE_FAULT);
      expand_end_cond ();
      break; }

    case M3_CHECK_HI: {
      TARGET_INTEGER (a);
      tree temp1 = declare_temp (t_int, 0, 0);

      if (TREE_TYPE (stack [tos-1]) != t_int) {
	stack [tos-1] = m3_build1 (CONVERT_EXPR, t_int, stack [tos-1]); }
      m3_store (temp1, 0, t_int);
      stack [tos++] = temp1;
      
      expand_start_cond (m3_build2 (GT_EXPR, t_int, temp1, a), 0);
      generate_fault (RANGE_FAULT);
      expand_end_cond ();
      break; }

    case M3_CHECK_RANGE: {
      TARGET_INTEGER (a);
      TARGET_INTEGER (b);
      tree temp1 = declare_temp (t_int, 0, 0);

      if (TREE_TYPE (stack [tos-1]) != t_int) {
	stack [tos-1] = m3_build1 (CONVERT_EXPR, t_int, stack [tos-1]); }
      m3_store (temp1, 0, t_int);
      stack [tos++] = temp1;

      expand_start_cond (m3_build2 (TRUTH_ORIF_EXPR, t_int,
				m3_build2 (LT_EXPR, t_int, temp1, a),
				m3_build2 (GT_EXPR, t_int, temp1, b)), 
			 0);
      generate_fault (RANGE_FAULT);
      expand_end_cond ();
      break; }

    case M3_CHECK_INDEX: {
      expand_start_cond (m3_build2 (GE_EXPR, t_word,
			   m3_build1 (CONVERT_EXPR, t_word, stack [tos-2]),
			   m3_build1 (CONVERT_EXPR, t_word, stack [tos-1])),
			 0);
      generate_fault (SUBSCRIPT_FAULT);
      expand_end_cond ();
      tos -= 1;
      break; }

    case M3_CHECK_EQ: {
      tree temp1 = declare_temp (t_int, 0, 0);
      tree temp2 = declare_temp (t_int, 0, 0);
      m3_store (temp1, 0, t_int);
      m3_store (temp2, 0, t_int);
      stack [tos++] = temp2;
      stack [tos++] = temp1;

      expand_start_cond (m3_build2 (NE_EXPR, t_int, temp1, temp2), 0);
      generate_fault (SHAPE_FAULT);
      expand_end_cond ();
      tos -= 2;
      break; }

    case M3_ADD_OFFSET: {
      BYTESIZE (n);
      stack [tos-1] = m3_build2 (PLUS_EXPR, t_addr,
			     stack [tos-1], m3_build_int (n/8));
      break; }

    case M3_INDEX_ADDRESS: {
      HOST_WIDE_INT incr_val;
      BYTESIZE (n);
      int n_bytes = n/8;
      tree incr = stack [tos-1];
      if (n_bytes != 1) {
	if (is_small_cardinal (incr, &incr_val)
	    && (0 <= incr_val) && (incr_val < 1024)
	    && (0 <= n_bytes) && (n_bytes < 1024)) {
	  incr = m3_build_int (incr_val * n_bytes); }
	else {
	  incr = m3_build2 (MULT_EXPR, t_int, incr, m3_build_int (n_bytes)); }};
      stack [tos-2] = m3_build2 (PLUS_EXPR, t_addr,
			     m3_cast (t_addr, stack [tos-2]),
			     incr);
      tos--;
      break; }

    case M3_START_CALL_DIRECT: {
      PROC (p);
      INTEGER (n);
      MTYPE (t);
      m3_start_call_direct ();
      break; }
      
    case M3_CALL_DIRECT: {
      PROC (p);
      MTYPE (t);
      m3_call_direct (p, t);
      break; }
      
    case M3_START_CALL_INDIRECT: {
      MTYPE (t);
      INTEGER (call_conv);
      m3_start_call_direct ();
      break; }

    case M3_CALL_INDIRECT: {
      MTYPE (t);
      INTEGER (call_conv);
      m3_call_indirect (t);
      break; }

    case M3_POP_PARAM: {
      MTYPE (t);
      m3_pop_param (t);
      break; }

    case M3_POP_STRUCT: {
      BYTESIZE (s);
      ALIGNMENT (a);
      tree t = build_type (T_struct, s, a);
      stack [tos-1] = m3_build1 (INDIRECT_REF, t,
			      m3_cast (build_pointer_type (t), stack [tos-1]));
      m3_pop_param (t);
      break; }

    case M3_POP_STATIC_LINK: {
      tree v = declare_temp (t_addr, 0, 0);
      m3_store (v, 0, TREE_TYPE (v));
      call_stack_link [call_stack_top] = v;
      break; }

    case M3_LOAD_PROCEDURE: {
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
      break; }

    case M3_LOAD_STATIC_LINK: {
      PROC (p);
      stack [tos++] = m3_rtl (lookup_static_chain (p));
      break; }

    default: {
      static int seen [(int)LAST_OPCODE];
      if (seen [current_opcode] == 0) {
	seen [current_opcode] = 1;
	fatal ("********* not handled: %s", m3cg_opcodes[current_opcode]);
      }
      break; }}

    skip_to_end_of_line (); }
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
