/* Modula-3 Compiler back end parser.

   Copyright (C) 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.

   In other words, you are welcome to use, share and improve this program.
   You are forbidden to forbid anyone else to use, share and improve
   what you give them.   Help stamp out software-hoarding! */

#include <vector>
#include <map>
using namespace std;
#define typedef_vector typedef vector /* hack for gengtype */
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <setjmp.h>
#include <limits.h>
#if !GCC42 && !GCC45 /* i.e. GCC43 */
#include "gmp.h"
#endif
extern "C" {
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "flags.h"
#include "tree.h"
#include "real.h"
#include "tm_p.h"
#include "tree-dump.h"
#include "tree-iterator.h"
#ifndef MTAG_P
#define GCC45 1
#include "gimple.h"
#else
#define GCC45 0
#include "tree-gimple.h"
#endif
#include "function.h"
#include "output.h"
#include "ggc.h"
#include "hashtab.h"
#include "toplev.h"
#include "langhooks-def.h"
#include "langhooks.h"
#include "input.h"
#include "target.h"
#include "version.h"
#include "cgraph.h"
#include "expr.h"
#include "diagnostic.h"
#include "m3cg.h"
#include "opts.h"
#include "options.h"
#include "debug.h"
#include "convert.h"
#include "dbxout.h"
} /* extern "C" */
#include "m3gty43.h"
#include "m3-parse.h"

#if !defined(GCC47) || (defined(GCC47) && !GCC47)
#define GCC47 0
#define BUILT_IN_SYNC_SYNCHRONIZE               BUILT_IN_SYNCHRONIZE
#define BUILT_IN_SYNC_LOCK_TEST_AND_SET_N       BUILT_IN_LOCK_TEST_AND_SET_N
#define BUILT_IN_SYNC_FETCH_AND_ADD_N           BUILT_IN_FETCH_AND_ADD_N
#define BUILT_IN_SYNC_FETCH_AND_SUB_N           BUILT_IN_FETCH_AND_SUB_N
#define BUILT_IN_SYNC_FETCH_AND_OR_N            BUILT_IN_FETCH_AND_OR_N
#define BUILT_IN_SYNC_FETCH_AND_AND_N           BUILT_IN_FETCH_AND_AND_N
#define BUILT_IN_SYNC_FETCH_AND_XOR_N           BUILT_IN_FETCH_AND_XOR_N
#define BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_N   BUILT_IN_BOOL_COMPARE_AND_SWAP_N
#define cgraph_get_create_node                  cgraph_node
#endif

typedef char* PSTR;
typedef const char* PCSTR;
typedef signed char SCHAR;
typedef unsigned char UCHAR;
typedef unsigned int UINT;
typedef unsigned HOST_WIDE_INT UWIDE;
typedef HOST_WIDE_INT WIDE;

typedef enum
{
  /* 00 */ T_word_8,
  /* 01 */ T_int_8,
  /* 02 */ T_word_16,
  /* 03 */ T_int_16,
  /* 04 */ T_word_32,
  /* 05 */ T_int_32,
  /* 06 */ T_word_64,
  /* 07 */ T_int_64,
  /* 08 */ T_reel,
  /* 09 */ T_lreel,
  /* 0A */ T_xreel,
  /* 0B */ T_addr,
  /* 0C */ T_struct,
  /* 0D */ T_void,
  /* 0E */ T_word,
  /* 0F */ T_int,
  /* 10 */ T_longword,
  /* 11 */ T_longint,
  /* 12 */ T_LAST
} m3_type;

typedef struct _m3buf_t {
  char buf[256];
} m3buf_t;

#if !GCC45
static bool m3_mark_addressable (tree exp);
#endif
static tree m3_type_for_size (UINT precision, int unsignedp);
static tree m3_type_for_mode (enum machine_mode, int unsignedp);
static tree m3_unsigned_type (tree type_node);
static tree m3_signed_type (tree type_node);
static tree m3_signed_or_unsigned_type (int unsignedp, tree type);

#if GCC42
extern "C" {
#endif
/* Functions to keep track of the current scope */
static tree pushdecl (tree decl);
#if GCC42
} /* extern "C" */
#endif

/* Langhooks.  */
static tree
builtin_function (PCSTR name, tree type,
#if GCC42
                  int function_code,
#else
                  built_in_function function_code,
#endif
		  enum built_in_class clas, const char *library_name,
		  tree attrs);

static tree getdecls (void);
static bool global_bindings_p (void);
#if !GCC45
static void insert_block (tree block);
#endif

static tree m3_push_type_decl (tree type, tree name);
static void m3_write_globals (void);

static PCSTR trace_name (PCSTR* inout_name);
static PSTR trace_upper_hex (PSTR format);
static void trace_int (PCSTR name, INT64 val);
static void trace_typeid (PCSTR name, UINT32 val);
static void trace_string (PCSTR name, PCSTR result, long length);
static void trace_type (PCSTR name, m3_type type);
static void trace_type_tree (PCSTR name, tree type);
/*static void trace_float (PCSTR name, UINT kind, long Longs[2]);*/
static void trace_boolean (PCSTR name, bool val);
static void trace_var (PCSTR name, tree var, size_t a);
static void trace_proc (PCSTR, tree p, size_t a);
static void trace_label (PCSTR name, size_t a);

static INT64 get_int (void);
static UINT64 get_uint (void);
static UINT32 get_typeid (void);
static PCSTR scan_string (long length);
static tree scan_calling_convention (void);
static m3_type scan_type (void);
static tree scan_mtype (m3_type* T);
static UINT scan_sign (void);
static tree scan_float (UINT *out_Kind);
static bool scan_boolean (void);
static tree scan_var (enum tree_code code, size_t* a);
static tree scan_proc (size_t* a);
static tree scan_label (size_t* a);

static struct language_function* m3_language_function (void);
static void m3_volatilize_decl (tree decl);
#if !GCC42
static void m3_volatilize_current_function (void);
#endif

static void m3_breakpoint(void);
#if GCC46
static void m3_parse_file (void);
#else
static void m3_parse_file (int);
#endif

#if !GCC46
static UINT m3_init_options (UINT argc, PCSTR* argv);
#endif
static
#if GCC46
bool
m3_handle_option (size_t code, PCSTR arg, int value, int /* kind */,
                  location_t /* loc */,
                  const struct cl_option_handlers * /* handlers */);
#else
int m3_handle_option (size_t code, PCSTR arg, int value);
#endif
static bool m3_post_options (PCSTR* pfilename);
static bool m3_init (void);

tree convert (tree type, tree expr);

#if !GCC47 /* copied from gcc 4.7 */
#ifdef __cplusplus
extern "C" {
#endif
tree
build_case_label (tree low_value, tree high_value, tree label_decl)
/* Create a CASE_LABEL_EXPR tree node and return it.  */
{
  tree t = make_node (CASE_LABEL_EXPR);

  TREE_TYPE (t) = void_type_node;
  SET_EXPR_LOCATION (t, DECL_SOURCE_LOCATION (label_decl));

  CASE_LOW (t) = low_value;
  CASE_HIGH (t) = high_value;
  CASE_LABEL (t) = label_decl;
  /*CASE_CHAIN (t) = NULL_TREE;*/

  return t;
}
#ifdef __cplusplus
}
#endif
#endif

/*======================================================= OPTION HANDLING ===*/

static int option_trace_all;

/*===========================================================================*/

#ifndef TARGET_MACHO
#define TARGET_MACHO 0
#endif

#define WIDE_PRINT_HEX HOST_WIDE_INT_PRINT_HEX
#define WIDE_PRINT_DEC HOST_WIDE_INT_PRINT_DEC

/* m3gdb is true if we should generate the debug information
   that m3gdb specifically uses. That is, if we should embed
   typeids in various identifiers. This is only useful
   if the command line includes -gstabs or -gstabs+, and
   if the target platform supports stabs and m3gdb.
   m3gdb is not currently supported on MacOSX (aka Mach-O).
   stabs is not currently supported on PA64_HPUX? That isn't checked here. */
static bool m3gdb;

static UINT8 pointer_size; /* for debugging */
static GTY (()) tree stdcall_list;

static bool M3_TYPES = false;
static bool M3_TYPES_SUBRANGE_NEW = false;
/*static bool M3_TYPES_OPEN_ARRAY_NEW = true;*/
static bool M3_TYPES_INT = false;
static bool M3_TYPES_ENUM = false;
static bool M3_TYPES_SEGMENT = false;
static bool M3_TYPES_CHECK_RECORD_SIZE = false;
static bool M3_TYPES_REQUIRE_ALL_FIELD_TYPES = false;
static bool M3_LOOPHOLE_VIEW_CONVERT = false;

#if GCC45

/* error: attempt to use poisoned "USE_MAPPED_LOCATION" */
#define M3_USE_MAPPED_LOCATION

/* error: macro "build_decl" requires 4 arguments, but only 3 given */
#undef build_decl
#define build_decl(code, tree, memstat) \
    build_decl_stat (input_location, code, tree, memstat MEM_STAT_INFO)

#else
#if defined USE_MAPPED_LOCATION
#define M3_USE_MAPPED_LOCATION
#endif
#endif

#ifndef SET_TYPE_MODE
#define SET_TYPE_MODE(node, mode) (TYPE_MODE (node) = (mode))
#endif

#ifndef GCC42
#define GCC42 0
#endif
#ifndef GCC46
#define GCC46 0
#endif

#ifndef GCC_APPLE
#define GCC_APPLE 0
#endif

#if GCC42
#define POINTER_PLUS_EXPR PLUS_EXPR
/*typedef const union tree_node *const_tree;*/
#define const_tree tree
#define allocate_struct_function(a, b) allocate_struct_function (a)
/* This is not merely copied from 4.3's tree.h, though it is.
It also matches what convert_call_expr does. */
/*#define CALL_EXPR_STATIC_CHAIN(NODE) TREE_OPERAND (CALL_EXPR_CHECK (NODE), 2) */

/* are we missing an #include? */
int arm_float_words_big_endian (void);
#endif

#define COUNT_OF(a) (sizeof(a)/sizeof((a)[0]))

/*---------------------------------------------------------------------------*/

#define LEVEL(x) UNSIGNED_INTEGER (x)
#define BITSIZE(x) UNSIGNED_INTEGER (x)
#define FREQUENCY(x) UNSIGNED_INTEGER (x)
#define BITOFFSET(x) UNSIGNED_INTEGER (x)
#define ALIGNMENT(x) BYTESIZE(x)
#define BYTEOFFSET(x) BYTESIZE(x)
#define NOTHING /* nothing */

#define M3CG_FIELD(type, name) type name;

#define INTEGER(x) M3CG_FIELD (INT64, x)
#define UNSIGNED_INTEGER(x) M3CG_FIELD (UINT64, x)
#define STRING(x, length) M3CG_FIELD (PCSTR, x) M3CG_FIELD (long, length)
#define CALLING_CONVENTION(x) M3CG_FIELD (tree, x)
#define TYPE(x) M3CG_FIELD (m3_type, x)
#define MTYPE(x) M3CG_FIELD (tree, x)
#define MTYPE2(x, y) M3CG_FIELD (m3_type, y) M3CG_FIELD (tree, x)
#define SIGN(x) M3CG_FIELD (UINT, x)
#define BYTESIZE(x) UNSIGNED_INTEGER (x)
#define FLOAT(x, fkind) M3CG_FIELD (UINT, fkind) M3CG_FIELD (tree, x)
#define BOOLEAN(x) M3CG_FIELD (bool, x)
#define VAR(x) M3CG_FIELD (tree, x) M3CG_FIELD (size_t, x##_integer)
#define RETURN_VAR(x, code) M3CG_FIELD (tree, x) M3CG_FIELD (size_t, x##_integer)
#define PROC(x) M3CG_FIELD (tree, x) M3CG_FIELD (size_t, x##_integer)
#define LABEL(x) M3CG_FIELD (tree, x) M3CG_FIELD (size_t, x##_integer)
#define TYPEID(x) M3CG_FIELD (UINT32, x)
#define M3CG_EXTRA_FIELDS(x) x

#define DESTRUCTOR ~ /* hack for gengtype */

struct m3cg_op_t
{
  UCHAR op;
  m3cg_op_t(UCHAR a) : op(a) { }
  virtual DESTRUCTOR m3cg_op_t() { }
  virtual void handler() = 0;
  virtual void trace() = 0;
  virtual void read() = 0;
  virtual void read_extended() { }
  static m3cg_op_t* create(UCHAR op);
};

#define M3CG(sym, fields) struct m3cg_##sym##_t : m3cg_op_t { \
        m3cg_##sym##_t() : m3cg_op_t(M3CG_##sym) { } \
        virtual void handler(); \
        virtual void trace(); \
        virtual void read(); \
        fields };
#include "m3-def.h"
#undef M3CG

#undef M3CG_EXTRA_FIELDS
#define M3CG_EXTRA_FIELDS(x) /* nothing */

#undef INTEGER
#undef UNSIGNED_INTEGER
#undef STRING
#undef CALLING_CONVENTION
#undef TYPE
#undef MTYPE
#undef MTYPE2
#undef SIGN
#undef BYTESIZE
#undef FLOAT
#undef BOOLEAN
#undef VAR
#undef RETURN_VAR
#undef PROC
#undef LABEL
#undef TYPEID

#define INTEGER(x) x = get_int ();
#define UNSIGNED_INTEGER(x) x = get_uint ();
#define STRING(x, length) length = get_int (); x = scan_string (length);
#define CALLING_CONVENTION(x) x = scan_calling_convention ();
#define TYPE(x) x = scan_type ();
#define MTYPE(x) x = scan_mtype (0);
#define MTYPE2(x, y) x = scan_mtype (&y);
#define SIGN(x) x = scan_sign ();
#define BYTESIZE(x)  x = BITS_PER_UNIT * get_uint ();
#define FLOAT(x, fkind) x = scan_float (&fkind);
#define BOOLEAN(x) x = scan_boolean ();
#define VAR(x) x = scan_var (ERROR_MARK, &x##_integer);
#define RETURN_VAR(x, code) x = scan_var (code, &x##_integer);
#define PROC(x) x = scan_proc (&x##_integer);
#define LABEL(x) x = scan_label (&x##_integer);
#define TYPEID(x) x = get_typeid ();

#define M3CG(sym, fields) void m3cg_##sym##_t::read() { fields; read_extended(); if (option_trace_all) trace(); }
#include "m3-def.h"
#undef M3CG

#undef INTEGER
#undef UNSIGNED_INTEGER
#undef STRING
#undef CALLING_CONVENTION
#undef TYPE
#undef MTYPE
#undef MTYPE2
#undef SIGN
#undef BYTESIZE
#undef FLOAT
#undef BOOLEAN
#undef VAR
#undef RETURN_VAR
#undef PROC
#undef LABEL
#undef TYPEID

#define INTEGER(x) trace_int (#x, x);
#define UNSIGNED_INTEGER(x) trace_int (#x, x);
#define STRING(x, length) trace_string (#x, x, length);
#define CALLING_CONVENTION(x) /* nothing */
#define TYPE(x) trace_type (#x, x);
#define MTYPE(x) trace_type_tree (#x, x);
#define MTYPE2(x, y) trace_type (#x, y);
#define SIGN(x) /* nothing */
#define BYTESIZE(x) trace_int (#x, x);
#define FLOAT(x, fkind) /* nothing */
#define BOOLEAN(x) trace_boolean (#x, x);
#define VAR(x) trace_var (#x, x, x##_integer);
#define RETURN_VAR(x, code) trace_var (#x, x, x##_integer);
#define PROC(x) trace_proc (#x, x, x##_integer);
#define LABEL(x) trace_label (#x, x##_integer);
#define TYPEID(x) trace_typeid (#x, x);

#define M3CG(sym, fields) void m3cg_##sym##_t::trace() { fields }
#include "m3-def.h"
#undef M3CG

m3cg_op_t* m3cg_op_t::create(UCHAR op) { switch (op) {
#define M3CG(sym, fields) case M3CG_##sym: return new m3cg_##sym##_t();
#include "m3-def.h"
#undef M3CG
 } return 0; }
#undef M3CG

/*------------------------------------------------------------- utils -------*/

/*   m3_append_char
     m3_revstr
     m3_int64_to_hex_shortest
     m3_fill_hex_value help
   m3cg_declare_subrange to write hex values of lower and upper bounds.
   They omit some redundant high bits, either positive or negative.
   However, the leftmost bit explicitly specified by the output is
   always a sign bit and can be sign-extended.  For example,
   0x7F and 0x0FF are positive, while 0xF7F and 0xFF are negative.
   (0xFF would never be written, but truncated to 0xF for -1.)
*/

static void
m3_append_char (char c, PSTR* p, PSTR limit)
{
  PSTR q = *p;
  if (q >= limit)
    fatal_error ("buffer overflow\n");
  *q = c;
  *p = (q + 1);
}

static void m3_revstr (PSTR a, size_t len)
{
  if (len < 2)
    return;
  len -= 1;
  size_t i = { 0 };
  while (i < len)
  {
    char temp = a[i];
    a[i] = a[len];
    a[len] = temp;
    i += 1;
    len -= 1;
  }
}

#if 0 /* for illustrative purposes */
static void m3_uint64_to_hex_full (UINT64 a, PSTR buf)
{
   UINT i = { 0 };
   for (i = 0; i < sizeof(a) * CHAR_BIT / 4; a >>= 4)
      buf[i++] = "0123456789ABCDEF"[a & 0xF];
   m3_revstr (buf, i);
   buf[i] = 0;
}
#endif

#if 0 /* for illustrative purposes */
static void m3_uint64_to_hex_shortest (UINT64 a, PSTR buf)
{
   UINT i = { 0 };
   do /* do/while necessary to handle 0 */
      buf[i++] = "0123456789ABCDEF"[a & 0xF];
   while (a >>= 4);
   m3_revstr (buf, i);
   buf[i] = 0;
}
#endif

static void m3_uint64_to_dec_shortest (UINT64 a, PSTR buf)
{
   UINT i = { 0 };
   do /* do/while necessary to handle 0 */
      buf[i++] = "0123456789"[a % 10];
   while (a /= 10);
   m3_revstr (buf, i);
   buf[i] = 0;
}

static void m3_int64_to_hex_shortest (INT64 a, PSTR buf)
/* if negative, first character must be >=8
 * if positive, first character must < 8;
 * skip leading characters otherwise
 * insert leading 0 or F if necessary
 * e.g. 127 => 7F
 *      255 => 0FF
 *     -255 => F01
 *        7 => 7
 *        8 => 08
 *       15 => 0F
 *     -128 => 80
 *
 * algorithm: do "full" and then trim digits
 * Positive numbers can have 0 trimmed as long as next is <= 7.
 * Negative numbers can have F trimmed as long as next is > 7.
 * Result must be at least one character.
 */
{
   UINT i = { 0 };
   UINT neg = (a < 0);
   char trim = (neg ? 'F' : '0');
   for (i = 0; i < sizeof(a) * CHAR_BIT / 4; a >>= 4)
      buf[i++] = "0123456789ABCDEF"[a & 0xF];
   while (i >= 2 && (buf[i - 1] == trim) && (neg == (buf[i - 2] > '7')))
      i -= 1;
   m3_revstr (buf, i);
   buf[i] = 0;
}

static void
m3_fill_hex_value (INT64 value, PSTR* p, PSTR limit)
{
  m3_append_char('0', p, limit);
  m3_append_char('x', p, limit);
  m3_int64_to_hex_shortest (value, *p);
  *p += strlen (*p);
  gcc_assert (*p < limit);
}

/*------------------------------------------------------------- type uids ---*/
/* Modula-3 type uids are unsigned 32-bit values.  They are passed as signed
   decimal integers in the intermediate code, but converted to 6-byte, base 62
   strings of characters from here to the debugger.  To avoid surprises downstream,
   these generated strings are legal C identifiers.  */

#define UID_SIZE 6
#define NO_UID (0xFFFFFFFFUL)

/* see RTBuiltin.mx
   see RT0.i3 */
#define UID_INTEGER 0x195C2A74 /* INTEGER */
#define UID_LONGINT 0x05562176 /* LONGINT */
#define UID_WORD 0x97E237E2 /* CARDINAL */
#define UID_LONGWORD 0x9CED36E7 /* LONGCARD */
#define UID_REEL 0x48E16572 /* REAL */
#define UID_LREEL 0x94FE32F6 /* LONGREAL */
#define UID_XREEL 0x9EE024E3 /* EXTENDED */
#define UID_BOOLEAN 0x1E59237D /* BOOLEAN [0..1] */
#define UID_CHAR 0x56E16863 /* CHAR [0..255] */
#define UID_WIDECHAR 0x88F439FC
#define UID_MUTEX 0x1541F475 /* MUTEX */
#define UID_TEXT 0x50F86574 /* TEXT */
#define UID_UNTRACED_ROOT 0x898EA789 /* UNTRACED ROOT */
#define UID_ROOT 0x9D8FB489 /* ROOT */
#define UID_REFANY 0x1C1C45E6 /* REFANY */
#define UID_ADDR 0x08402063 /* ADDRESS */
#define UID_RANGE_0_31 0x2DA6581D /* [0..31] */
#define UID_RANGE_0_63 0x2FA3581D /* [0..63] */
#define UID_PROC1 0x9C9DE465 /* PROCEDURE (x, y: INTEGER): INTEGER */
#define UID_PROC2 0x20AD399F /* PROCEDURE (x, y: INTEGER): BOOLEAN */
#define UID_PROC3 0x3CE4D13B /* PROCEDURE (x: INTEGER): INTEGER */
#define UID_PROC4 0xFA03E372 /* PROCEDURE (x, n: INTEGER): INTEGER */
#define UID_PROC5 0x509E4C68 /* PROCEDURE (x: INTEGER;  n: [0..31]): INTEGER */
#define UID_PROC6 0xDC1B3625 /* PROCEDURE (x: INTEGER;  n: [0..63]): INTEGER */
#define UID_PROC7 0xEE17DF2C /* PROCEDURE (x: INTEGER;  i, n: CARDINAL): INTEGER */
#define UID_PROC8 0xB740EFD0 /* PROCEDURE (x, y: INTEGER;  i, n: CARDINAL): INTEGER */
#define UID_NULL 0x48EC756E /* NULL */

/* type trees */
#define t_addr ptr_type_node
static GTY (()) tree t_word;
static GTY (()) tree t_int;
#define t_longword t_word_64
#define t_longint t_int_64
#define t_reel float_type_node
#define t_lreel double_type_node
#if 0
  /* XXX The M3 front end (m3middle/src/Target.m3) seems to treat extended
     reals the same as LONGREAL.  That may be due to limitations in other
     parts of the front end.  I don't know yet.  For now we likewise treat
     the xreel type as if it were lreel. */
#define t_xreel long_double_type_node
#else
#define t_xreel double_type_node
#endif
#define t_int_8 intQI_type_node
#define t_int_16 intHI_type_node
#define t_int_32 intSI_type_node
#define t_int_64 intDI_type_node
#define t_word_8 unsigned_intQI_type_node
#define t_word_16 unsigned_intHI_type_node
#define t_word_32 unsigned_intSI_type_node
#define t_word_64 unsigned_intDI_type_node
#define t_void void_type_node
static GTY (()) tree t_set;

static tree m3_alloca;

static const struct { UINT32 type_id; tree* t; } builtin_uids[] = {
  { UID_INTEGER, &t_int },
  { UID_LONGINT, &t_longint },
  { UID_WORD, &t_word },
  { UID_LONGWORD, &t_longword },
  { UID_REEL, &t_reel },
  { UID_LREEL, &t_lreel },
  { UID_XREEL, &t_xreel },
  { UID_BOOLEAN, &t_word_8 },
  { UID_CHAR, &t_word_8 },
  { UID_WIDECHAR, &t_word_16 }, 
      /* ^Default to 16, in case we compile output of an older cm3 front end, 
         which produces no widechar_size operator. */
  { UID_MUTEX, &t_addr },
  { UID_TEXT, &t_addr },
  { UID_UNTRACED_ROOT, &t_addr },
  { UID_ROOT, &t_addr },
  { UID_REFANY, &t_addr },
  { UID_ADDR, &t_addr },
  { UID_RANGE_0_31, &t_word_8 },
  { UID_RANGE_0_63, &t_word_8 },
  { UID_PROC1, &t_addr },
  { UID_PROC2, &t_addr },
  { UID_PROC3, &t_addr },
  { UID_PROC4, &t_addr },
  { UID_PROC5, &t_addr },
  { UID_PROC6, &t_addr },
  { UID_PROC7, &t_addr },
  { UID_PROC8, &t_addr },
  { UID_NULL, &t_void }
};

#define CONSTANT_STRING_AND_LENGTH(a) a, sizeof(a) - 1

static const struct { tree* t; char name[9]; char length; }
builtin_types[T_LAST] = {
/* This is ordered per m3_type. It is also used by typestr. */
  { &t_word_8, CONSTANT_STRING_AND_LENGTH ("word_8") },
  { &t_int_8, CONSTANT_STRING_AND_LENGTH ("int_8") },
  { &t_word_16, CONSTANT_STRING_AND_LENGTH ("word_16") },
  { &t_int_16, CONSTANT_STRING_AND_LENGTH ("int_16") },
  { &t_word_32, CONSTANT_STRING_AND_LENGTH ("word_32") },
  { &t_int_32, CONSTANT_STRING_AND_LENGTH ("int_32") },
  { &t_word_64, CONSTANT_STRING_AND_LENGTH ("word_64") },
  { &t_int_64, CONSTANT_STRING_AND_LENGTH ("int_64") },
  { &t_reel, CONSTANT_STRING_AND_LENGTH ("reel") },
  { &t_lreel, CONSTANT_STRING_AND_LENGTH ("lreel") },
  { &t_xreel, CONSTANT_STRING_AND_LENGTH ("xreel") },
  { &t_addr, CONSTANT_STRING_AND_LENGTH ("addr") },
  { 0, CONSTANT_STRING_AND_LENGTH ("struct") },
  { 0, CONSTANT_STRING_AND_LENGTH ("void") },
  { 0, CONSTANT_STRING_AND_LENGTH ("word") },
  { 0, CONSTANT_STRING_AND_LENGTH ("int") },
  { 0, CONSTANT_STRING_AND_LENGTH ("longword") },
  { 0, CONSTANT_STRING_AND_LENGTH ("longint") },
};

/* store all trees here for garbage collector */
static GTY(()) VEC(tree, gc) *m3trees; /* see alias.c for a GTY+VEC example */

static tree m3_gc_tree (tree a)
{
  if (!m3trees)
    m3trees = VEC_alloc (tree, gc, 100);
  VEC_safe_push (tree, gc, m3trees, a);
  return a;
}

/* Maintain a qsorted/bsearchable array of type_id/tree pairs to map type_id to tree. */

static bool m3type_table_dirty;

DEF_VEC_O (m3type_t);
DEF_VEC_ALLOC_O (m3type_t, gc);
static GTY(()) VEC(m3type_t, gc) *m3type_table; /* see alias.c for a GTY+VEC example */
#define m3type_table_address VEC_address (m3type_t, m3type_table)
#define m3type_table_size_used VEC_length (m3type_t, m3type_table)

static int
m3type_compare (const void* a, const void *b)
{
  UINT32 x = ((const m3type_t*)a)->type_id;
  UINT32 y = ((const m3type_t*)b)->type_id;
  /* Do not use subtraction here. It does not work. Not just
   * because sizeof(int) < sizeof(long) but also because
   * these are unsigned numbers.
   */
  return ((x < y) ? -1 : ((x > y) ? 1 : 0));
}

static m3type_t*
m3type_get (UINT32 type_id)
{
  m3type_t* found = { 0 };
  if (M3_TYPES)
  {
    m3type_t to_find;

    if (option_trace_all >= 2)
      fprintf (stderr, "\n  m3type_get(0x%X) ", (UINT)type_id);

    if (!m3type_table_size_used || type_id == NO_UID)
      return NULL;

    if (m3type_table_dirty)
    {
       qsort (m3type_table_address,
              m3type_table_size_used,
              sizeof(m3type_t),
              m3type_compare);
      m3type_table_dirty = false;
    }
    to_find.type_id = type_id;
    found = (m3type_t*)bsearch (&to_find,
                                m3type_table_address,
                                m3type_table_size_used,
                                sizeof(m3type_t),
                                m3type_compare);
  }
  return found;
}

static tree
get_typeid_to_tree (UINT32 type_id)
{
/* Additional type information can give optimizer liberty to
   further transform, and break, the code. Beware.
*/
  if (M3_TYPES)
  {
    /* optimize some common ones (even skip tracing) */
    switch (type_id)
    {
    case UID_INTEGER: return t_int;
    case UID_ADDR: return t_addr;
    }
    {
      m3type_t* found = m3type_get (type_id);
      tree t = found ? found->t : 0;
      if (type_id != NO_UID && option_trace_all >= 2)
        fprintf (stderr, "\n  get_typeid_to_tree(0x%X):%p  ", (UINT)type_id, t);
      return t;
    }
  }
  return NULL;
}

static int m3_indent;

static PCSTR m3_indentstr(void)
/* This function returns a string of spaces for the current tracing indent
   level. */
{
  static char str[100];
  m3_indent = ((m3_indent < 0) ? 0 : m3_indent);
  if (m3_indent >= (int)COUNT_OF(str) || m3_indent < 0)
    return "";
  if (!str[0])
    memset(str, ' ', sizeof(str) - 1);
  return str + sizeof(str) - 1 - m3_indent;
}

static void m3_outdent(void)
/* This function reduces the tracing indent level. */
{
  m3_indent -= (m3_indent > 0 ? 2 : 0);
}

static size_t sizet_add (size_t a, size_t b)
{
  size_t c = a + b;
  if (c < a)
    fatal_error ("sizet_add: integer overflow");
  return c;
}

static int sizet_to_int (size_t a)
{
  if (a > INT_MAX)
    fatal_error ("sizet_to_int: integer overflow");
  return (int)a;
}

static int long_to_printf_length (long a)
{
  if (a > INT_MAX)
    fatal_error ("long_to_printf_length: integer overflow");
  return ((a < 0) ? 0 : (int)a);
}

static ptrdiff_t int64_to_ptrdifft (INT64 a)
{
#if defined(__LP64__) || defined(_WIN64) || __INITIAL_POINTER_SIZE == 64
  return a;
#else
  if (a > INT_MAX || a < INT_MIN)
    fatal_error ("int64_to_ptrdifft: integer overflow");
  return (ptrdiff_t)a;
#endif
}

static size_t uint64_to_sizet (UINT64 a)
{
#if defined(__LP64__) || defined(_WIN64) || __INITIAL_POINTER_SIZE == 64
  return a;
#else
  if (a > UINT_MAX)
    fatal_error ("uint64_to_sizet: integer overflow");
  return (size_t)a;
#endif
}

static size_t long_to_sizet (long a)
{
  if (a < 0)
    fatal_error ("long_to_sizet: integer overflow");
  return (size_t)a;
}

static void
set_typeid_to_tree_replace (UINT32 type_id, tree t, bool replace)
/* This function establishes a global mapping of type_id to tree.
   If a mapping already exists, the 'replace' parameter determines
   if it is left alone or replaced. */
{
/* Additional type information can give optimizer liberty to
   further transform, and break, the code. Beware.
*/
  if (M3_TYPES)
  {
    m3type_t* found = { 0 };
    m3type_t to_add = { 0, 0 };

    if (option_trace_all >= 2)
      fprintf (stderr, "\n  set_typeid_to_tree(0x%X, %p)  ", (UINT)type_id, t);

    if (type_id == NO_UID || !t)
      return;

    found = m3type_get (type_id);
    if (found)
    {
      if (replace)
        found->t = t;
      return;
    }
    to_add.type_id = type_id;
    to_add.t = t;
    if (!m3type_table)
      m3type_table = VEC_alloc (m3type_t, gc, 100);
    VEC_safe_push (m3type_t, gc, m3type_table, &to_add);
    m3type_table_dirty = true;
  }
}

static void
set_typeid_to_tree (UINT32 type_id, tree t)
/* This function establishes a global mapping of type_id to tree.
   If a mapping from this type_id already exists, it is left unchanged. */
{
  set_typeid_to_tree_replace (type_id, t, false);
}

static void
fmt_uid (UINT32 x, PSTR buf)
{
  UINT i = UID_SIZE;
  static const char alphabet[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

  gcc_assert (sizeof(alphabet) == 63);

  if (x == NO_UID)
  {
    static const char zzzzzz[] = "zzzzzz";
    memcpy (buf, zzzzzz, sizeof(zzzzzz) - 1);
    return;
  }

  while (i)
  {
    buf[--i] = alphabet[x % 62];
    x /= 62;
  }

  if (x || buf[0] < 'A' || buf[0] > 'Z')
    fatal_error (" *** bad uid -> identifier conversion!!");
}

/*================================================================= TREES ===*/

#define IS_UNSIGNED_INTEGER_TYPE(t) ((t) == T_word_32 || (t) == T_word_8 \
                                     || (t) == T_word_16 \
                                     || (t) == T_word_64 || (t) == T_word)

#define IS_SIGNED_INTEGER_TYPE(t) ((t) == T_int_32 || (t) == T_int_8 \
                                   || (t) == T_int_16 || (t) == T_int_64 \
                                   || (t) == T_int)

#define IS_INTEGER_TYPE(t) (IS_SIGNED_INTEGER_TYPE(t) || IS_UNSIGNED_INTEGER_TYPE(t))

#define IS_SIGNED_INTEGER_TYPE_TREE(t) ((t) == t_int_32 || (t) == t_int_8 \
                                        || (t) == t_int_16 || (t) == t_int_64 \
                                        || (t) == t_int)

#define IS_UNSIGNED_INTEGER_TYPE_TREE(t) ((t) == t_word_32 || (t) == t_word_8 \
                                          || (t) == t_word_16 || (t) == t_word_64 \
                                          || (t) == t_word)

#define IS_REAL_TYPE(t) ((t) == T_reel || (t) == T_lreel || (t) == T_xreel)
#define unused_IS_REAL_TYPE_TREE(t) ((t) == t_reel || (t) == t_lreel || (t) == t_xreel)

#define boolstr(a) ((a) ? "true" : "false")

static PCSTR typestr (UINT a)
{
    return (a < COUNT_OF (builtin_types) ? builtin_types[a].name : "invalid");
}

/* In gcc, "word" means roughly "register".
 * To us, "word" means unsigned "INTEGER",
 * and "INTEGER" is the same size as a pointer.
 * Generally these are the same, except on 32bit Alpha
 * targets, where word is 64bits and INTEGER is 32.
 * So in place of BITS_PER_WORD, we use POINTER_SIZE
 * and call it BITS_PER_INTEGER.
 */
#define BITS_PER_INTEGER POINTER_SIZE
static GTY (()) tree bits_per_integer_tree;
static GTY (()) tree bytes_per_integer_tree;

/* Values. */
#define v_zero integer_zero_node
#define v_one integer_one_node
#define v_null null_pointer_node

#if !GCC47
#define builtin_decl_explicit(a) (built_in_decls[a])
#endif

/* Procedures. */
#define memcpy_proc builtin_decl_explicit (BUILT_IN_MEMCPY)
#define memmove_proc builtin_decl_explicit (BUILT_IN_MEMMOVE)
#define memset_proc builtin_decl_explicit (BUILT_IN_MEMSET)
#define memcmp_proc builtin_decl_explicit (BUILT_IN_MEMCMP)
static GTY (()) tree set_union_proc;
static GTY (()) tree set_diff_proc;
static GTY (()) tree set_inter_proc;
static GTY (()) tree set_sdiff_proc;
static GTY (()) tree set_gt_proc;
static GTY (()) tree set_ge_proc;
static GTY (()) tree set_lt_proc;
static GTY (()) tree set_le_proc;
static GTY (()) tree set_range_proc;
static GTY (()) tree fault_proc;
static GTY (()) tree fault_handler;

/* Miscellaneous. */
static GTY (()) tree global_decls;
static GTY (()) tree debug_fields;
static GTY (()) tree current_block;
static GTY (()) tree current_record_type;
static GTY (()) tree current_record_vals;
static GTY (()) tree enumtype;
static GTY (()) tree enumtype_elementtype;
static GTY (()) tree current_segment;
static GTY (()) tree pending_blocks;
static GTY (()) tree current_stmts;
static GTY (()) tree pending_stmts;
static GTY (()) tree pending_inits;

static tree m3_current_scope (void)
{
  return current_function_decl ? current_function_decl : global_decls;
}

static bool
get_volatize (void)
{
  /* gcc 4.2 volatize nothing
     gcc 4.6 volatize everything */
  if (GCC42)
    return false;
  else
    return m3_language_function () && m3_language_function ()->volatil;
}

#if !GCC42

static void
set_volatize (bool a ATTRIBUTE_UNUSED)
{
  /* gcc 4.2 volatize nothing
     gcc 4.6 volatize everything */
  if (GCC42)
    return;
  m3_language_function ()->volatil = a;
}

#endif

/* The front end language hooks (addresses of code for this front
   end).  These are not really very language-dependent, i.e.
   treelang, C, Mercury, etc. can all use almost the same definitions.  */

#if !GCC45
#undef LANG_HOOKS_MARK_ADDRESSABLE
#define LANG_HOOKS_MARK_ADDRESSABLE m3_mark_addressable
#endif
#if GCC42
#undef LANG_HOOKS_SIGNED_TYPE
#define LANG_HOOKS_SIGNED_TYPE m3_signed_type
#undef LANG_HOOKS_UNSIGNED_TYPE
#define LANG_HOOKS_UNSIGNED_TYPE m3_unsigned_type
#undef LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE
#define LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE m3_signed_or_unsigned_type
#endif
#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE m3_type_for_mode
#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE m3_type_for_size
#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE m3_parse_file

#undef LANG_HOOKS_WRITE_GLOBALS
#define LANG_HOOKS_WRITE_GLOBALS m3_write_globals

#if GCC42
  /* We have nothing special to do while expanding functions for Modula-3.  */
#undef LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION
#define LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION tree_rest_of_compilation
#endif

/* Hook routines and data unique to Modula-3 back-end.  */

#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT m3_init
#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "Modula-3 backend"
#if !GCC46
#undef LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS  m3_init_options
#endif
#undef LANG_HOOKS_HANDLE_OPTION
#define LANG_HOOKS_HANDLE_OPTION m3_handle_option
#undef LANG_HOOKS_POST_OPTIONS
#define LANG_HOOKS_POST_OPTIONS m3_post_options
#if GCC46
/* Return language mask for option parsing. (gcc 4.6) */
#undef LANG_HOOKS_OPTION_LANG_MASK
static unsigned int m3_lang_mask (void) { return CL_m3cg | CL_M3CG; }
#define LANG_HOOKS_OPTION_LANG_MASK m3_lang_mask
#endif
#if !GCC45
const
#endif
struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#if !GCC45

/* Tree code type/name/code tables.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) (tree_code_class)TYPE,

extern const enum tree_code_class tree_code_type[] = {
#include "tree.def"
  tcc_exceptional
};
#undef DEFTREECODE

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

extern const UCHAR tree_code_length[] = {
#include "tree.def"
  0
};
#undef DEFTREECODE

#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

extern const PCSTR tree_code_name[] = {
#include "tree.def"
  "@@dummy"
};
#undef DEFTREECODE

#endif

static tree
m3_build1 (enum tree_code code, tree tipe, tree op0)
{
  return /*stabilize_reference*/ (fold_build1 (code, tipe, op0));
}

static tree
m3_build2 (enum tree_code code, tree tipe, tree op0, tree op1)
{
  return /*stabilize_reference*/ (fold_build2 (code, tipe, op0, op1));
}

static tree
m3_build3 (enum tree_code code, tree tipe, tree op0, tree op1, tree op2)
{
  return /*stabilize_reference*/ (fold_build3 (code, tipe, op0, op1, op2));
}

static tree
m3_cast (tree type, tree op0)
{
  return m3_build1 (NOP_EXPR, type, op0);
}

static tree
m3_convert (tree type, tree op0)
{
  return /*stabilize_reference*/ (convert (type, op0));
}

static tree
m3_build_pointer_type (tree a)
{
  a = build_pointer_type_for_mode (a, ptr_mode, true);
  /*DECL_NO_TBAA_P (a) = true;*/
  /* m3_gc_tree (a); */
  return a;
}

static tree
m3_build_type_id (m3_type type,
                  UINT64 size,
                  UINT64 align,
                  UINT32 type_id)
{
  tree ts = { 0 };

  /* TODO? build_qualified_type (x, TYPE_QUAL_VOLATILE); */

  if (!M3_TYPES)
    type_id = NO_UID; /* disable */

  /* Convert integer to enum, or possibly typename/subrange.
     Be careful, if type_id is a packed type e.g. BITS 20 for typecode,
     but frontend asked for something bigger, e.g. word32, then give
     it word32. */

  if (M3_TYPES_INT
      && (type_id != NO_UID)
      && IS_INTEGER_TYPE(type)
      && ((ts = get_typeid_to_tree (type_id))))
 {
    if (TYPE_SIZE (ts))
    {
      if (TREE_INT_CST_LOW (TYPE_SIZE (ts)) == size
          && TYPE_ALIGN (ts) == align
          && size == align)
      {
        return ts;
      } 
    }
    /*fprintf (stderr, "type missing size 0x%X\n", (UINT)type_id);*/
  }

  switch (type)
    {
    case T_word:
    case T_longword:
      switch (size)
        {
        case 0:  return ((type == T_word) ? t_word : t_longword);
        case 8:  return t_word_8;
        case 16: return t_word_16;
        case 32: return t_word_32;
        case 64: return t_word_64;
        default: if (size == BITS_PER_INTEGER) return t_word;
        }
      break;

    case T_int:
    case T_longint:
      switch (size)
        {
        case 0:  return ((type == T_int) ? t_int : t_longint);
        case 8:  return t_int_8;
        case 16: return t_int_16;
        case 32: return t_int_32;
        case 64: return t_int_64;
        default: if (size == BITS_PER_INTEGER) return t_int;
        }
      break;

    case T_reel:    return t_reel;
    case T_lreel:   return t_lreel;
    case T_xreel:   return t_xreel;
    case T_int_8:   return t_int_8;
    case T_int_16:  return t_int_16;
    case T_int_32:  return t_int_32;
    case T_int_64:  return t_int_64;
    case T_word_8:  return t_word_8;
    case T_word_16: return t_word_16;
    case T_word_32: return t_word_32;
    case T_word_64: return t_word_64;
    case T_void:    return t_void;

    case T_addr:
      if (type_id != NO_UID)
        ts = get_typeid_to_tree (type_id);
#if 0
      return ts ? m3_build_pointer_type (ts) : t_addr;
#else
      return ts ? ts : t_addr;
#endif

    case T_struct:
      if (type_id != NO_UID)
        ts = get_typeid_to_tree (type_id);
      if (!ts
          || TYPE_SIZE (ts) != bitsize_int (size)
          || TYPE_SIZE_UNIT (ts) != size_int (size / BITS_PER_UNIT)
          || TYPE_ALIGN (ts) != align)
      {
        ts = m3_gc_tree (make_node (RECORD_TYPE));
        TYPE_NAME (ts) = NULL_TREE;
        TYPE_FIELDS (ts) = NULL_TREE;
        TYPE_SIZE (ts) = bitsize_int (size);
        TYPE_SIZE_UNIT (ts) = size_int (size / BITS_PER_UNIT);
        TYPE_ALIGN (ts) = align;
        compute_record_mode (ts);
      }
      return ts;

    default:
      break;
    } /*switch*/

  gcc_unreachable ();
}

static tree
m3_build_type (m3_type type, UINT64 size, UINT64 align)
{
  return m3_build_type_id (type, size, align, NO_UID);
}

/*========================================== insert, shift, rotate and co ===*/

static tree
m3_do_insert (tree x, tree y, tree offset, tree count, tree orig_type)
{
  tree type = m3_unsigned_type (orig_type);
  x = m3_cast (type, m3_cast (orig_type, x));
  y = m3_cast (type, m3_cast (orig_type, y));
  tree a = m3_build1 (BIT_NOT_EXPR, type, m3_cast (type, v_zero));
  tree b = m3_build2 (LSHIFT_EXPR, type, a, count);
  tree c = m3_build1 (BIT_NOT_EXPR, type, b);
  tree d = m3_build2 (BIT_AND_EXPR, type, y, c);
  tree e = m3_build2 (LSHIFT_EXPR, type, d, offset);
  tree f = m3_build2 (LSHIFT_EXPR, type, c, offset);
  tree g = m3_build1 (BIT_NOT_EXPR, type, f);
  tree h = m3_build2 (BIT_AND_EXPR, type, x, g);
  tree j = m3_build2 (BIT_IOR_EXPR, type, h, e);
  tree k = m3_build3 (COND_EXPR, type,
                      m3_build2 (EQ_EXPR, boolean_type_node, count, m3_cast (t_int, TYPE_SIZE (type))),
                      y, j);
  tree m = m3_build3 (COND_EXPR, type,
                      m3_build2 (EQ_EXPR, boolean_type_node, count, m3_cast (t_int, v_zero)),
                      x, k);
  return m3_cast (orig_type, m);
}

static tree
left_shift (tree t, int i)
{
  if (i)
    t = m3_build2 (LSHIFT_EXPR, TREE_TYPE (t), t, build_int_cst (t_int, i));
  return t;
}

static tree
m3_do_fixed_insert (tree x, tree y, UINT64 offset, UINT64 count, tree type)
{
  /* ??? Use BIT_FIELD_REF ??? */

  /*gcc_assert (offset >= 0);*/
  /*gcc_assert (count >= 0);*/
  gcc_assert (offset <= 64);
  gcc_assert (count <= 64);
  gcc_assert ((offset + count) <= 64);
  gcc_assert (offset <= TYPE_PRECISION (type));
  gcc_assert (count <= TYPE_PRECISION (type));
  gcc_assert ((offset + count) <= TYPE_PRECISION (type));

  if (!   ((offset <= 64)
        && (count <= 64)
        && ((offset + count) <= 64)
        && (64 <= HOST_BITS_PER_WIDE_INT)
        && (HOST_BITS_PER_WIDE_INT >= TYPE_PRECISION (type))))
  {
    fprintf (stderr, "m3_do_fixed_insert: offset:0x%X count:0x%X wide:0x%X type:0x%X\n",
             (UINT)offset, (UINT)count, (UINT)HOST_BITS_PER_WIDE_INT, (UINT)TYPE_PRECISION (type));
    printf  (        "m3_do_fixed_insert: offset:0x%X count:0x%X wide:0x%X type:0x%X\n",
             (UINT)offset, (UINT)count, (UINT)HOST_BITS_PER_WIDE_INT, (UINT)TYPE_PRECISION (type));
  }

  /*
    y_mask = (~(~0 << count)) << offset
    x_mask = ~y_mask
    x = (x & x_mask) | ((y << offset) & y_mask)
  or equivalent
    y_mask = ~(~0 << count)
    x_mask = ~(y_mask << offset)
    x = (x & x_mask) | ((y & y_mask) << offset)
    
  There are then some optimizations.
  Such as, if count is constant 1 and y is constant then
    if y & 1:
      x |= (1 << offset)
    else
      x &= ~(1 << offset)
  if count is constant 1 and y is not constant:
    x = (x & ~(1 << offset)) | ((y & 1) << offset)
  */

  x = m3_cast (type, x);

  if (count == 0)
    return x;

  y = m3_cast (type, y);
    
  if (offset == 0 && count == TYPE_PRECISION (type))
    return y;

  if (offset >= TYPE_PRECISION (type) || count >= TYPE_PRECISION (type))
    {
      fprintf (stderr, "m3_do_fixed_insert => m3_do_insert %u %u\n", (UINT)offset, (UINT)count);
      printf  (        "m3_do_fixed_insert => m3_do_insert %u %u\n", (UINT)offset, (UINT)count);
      return m3_do_insert (x, y,
                           build_int_cst (t_int, offset),
                           build_int_cst (t_int, count), type);
    }

  if ((count == 1) && (offset < HOST_BITS_PER_WIDE_INT))
    {
      tree bit = build_int_cstu (type, (((INT64)1) << offset));
      tree nbit = m3_build1 (BIT_NOT_EXPR, type, bit);
      if (host_integerp (y, 0))
        {
          if (TREE_INT_CST_LOW (y) & 1)
            {
              return m3_build2 (BIT_IOR_EXPR, type, x, bit); /* set the bit */
            }
          else
            {
              return m3_build2 (BIT_AND_EXPR, type, x, nbit); /* clear the bit */
            }
        }
      else
        {                       /* non-constant, 1-bit value */
          tree a = m3_build2 (BIT_AND_EXPR, type, y, m3_cast (type, v_one)); /* extract bit */
          tree b = m3_build2 (BIT_AND_EXPR, type, x, nbit); /* clear bit */
          return m3_build2 (BIT_IOR_EXPR, type, b, left_shift (a, offset)); /* combine */
        }
    }
  else
    {                           /* multi-bit value */
      tree saved_bits = { 0 };
      tree new_bits = { 0 };
      if ((offset + count) < HOST_BITS_PER_WIDE_INT)
        {
          INT64 mask = ((INT64)1 << count) - 1;
          saved_bits = m3_build1 (BIT_NOT_EXPR, type,
                                  build_int_cstu (type, mask << offset));
          if (host_integerp (y, 0))
            {
              new_bits = build_int_cstu (type, (TREE_INT_CST_LOW (y) & mask) << offset);
            }
          else
            {
              new_bits = m3_build2 (BIT_AND_EXPR, type, y,
                                    build_int_cstu (type, mask));
              new_bits = left_shift (new_bits, offset);
            }
        }
      else if (count < HOST_BITS_PER_WIDE_INT)
        {
          INT64 mask = ((INT64)1 << count) - 1;
          tree a = build_int_cstu (type, mask);
          if (host_integerp (y, 0))
            {
              new_bits = build_int_cstu (type, TREE_INT_CST_LOW (y) & mask);
            }
          else
            {
              new_bits = m3_build2 (BIT_AND_EXPR, type, y, a);
            }
          new_bits = left_shift (new_bits, offset);
          saved_bits = m3_build1 (BIT_NOT_EXPR, type, left_shift (a, offset));
        }
      else
        {                       /* count >= sizeof(int)*8 */
          tree mask;

          gcc_unreachable ();

          mask = m3_build2 (LSHIFT_EXPR, type, build_int_cst (type, ~0),
                            build_int_cst (t_int, count));
          mask = m3_build1 (BIT_NOT_EXPR, type, mask);
          new_bits = left_shift (m3_build2 (BIT_AND_EXPR, type, y, mask), offset);
          saved_bits = m3_build1 (BIT_NOT_EXPR, type, left_shift (mask, offset));
        }
      x = m3_build2 (BIT_AND_EXPR, type, x, saved_bits);
      return m3_build2 (BIT_IOR_EXPR, type, x, new_bits);
    }
}

static tree
m3_do_extract (tree x, tree offset, tree count, tree orig_type)
{
  tree type = m3_unsigned_type (orig_type);
  x = m3_cast (type, m3_cast (orig_type, x));

  /* i.e. x = ((x << (32 - count - offset)) >> (32 - count));
     shifting left will throw out the bits we don't want,
     leaving the bits we do want left justified.
     Shift those back to the right, right justifying them,
     and throwing out bits below them.

     If count is zero, special case that and return the original value.
     (Shifting to the right by 32 as the general case would do might
     produce that, but it is probably undefined.) */

  tree right = m3_build2 (MINUS_EXPR, t_int, m3_cast (t_int, TYPE_SIZE (type)), count);
  tree left = m3_build2 (MINUS_EXPR, t_int, right, offset);
  x = m3_build2 (LSHIFT_EXPR, type, x, left);
  x = m3_build2 (RSHIFT_EXPR, type, x, right);
  x = m3_build3 (COND_EXPR, type,
                 m3_build2 (EQ_EXPR,
                            boolean_type_node,
                            count,
                            m3_cast (TREE_TYPE (count), v_zero)),
                 m3_cast (type, v_zero), x);
  return m3_cast (orig_type, x);
}

static tree
m3_do_rotate (enum tree_code code, tree orig_type, tree val, tree cnt)
{
  /* ??? Use LROTATE_EXPR/RROTATE_EXPR.  */

  tree type = m3_unsigned_type (orig_type);
  tree a = build_int_cst (t_int, TYPE_PRECISION (type) - 1);
  tree b = m3_build2 (BIT_AND_EXPR, t_int, cnt, a);
  tree c = m3_build2 (MINUS_EXPR, t_int, m3_cast (t_int, TYPE_SIZE (type)), b);
  tree d = m3_convert (type, val);
  tree e = m3_build2 (LSHIFT_EXPR, type, d, (code == LROTATE_EXPR) ? b : c);
  tree f = m3_build2 (RSHIFT_EXPR, type, d, (code == RROTATE_EXPR) ? b : c);
  tree g = m3_build2 (BIT_IOR_EXPR, type, e, f);
  return g;
}

#if 0

static tree
m3_do_shift (enum tree_code code, tree orig_type, tree val, tree count)
{
  tree type = m3_unsigned_type (orig_type);
  tree a = m3_convert (type, val);
  tree b = m3_build2 (code, type, a, count);
  if (host_integerp (count, 1) && (TREE_INT_CST_LOW (count) < TYPE_PRECISION (type)))
    return b;
  tree c = m3_build2 (GE_EXPR, boolean_type_node, count, TYPE_SIZE (type));
  tree d = m3_build3 (COND_EXPR, type, c, v_zero, b);
  return d;
}

#else

static tree
m3_do_shift (enum tree_code code, tree type, tree val, tree count)
{
  tree unsigned_type = m3_unsigned_type (type);
  tree a = m3_convert (unsigned_type, val); /* cast? */
  tree b = m3_cast (type, m3_build2 (code, unsigned_type, a, count));
  if (host_integerp (count, 1) && (TREE_INT_CST_LOW (count) < TYPE_PRECISION (type)))
    return b;
  tree c = m3_build2 (GE_EXPR, boolean_type_node, count, m3_cast (TREE_TYPE (count), TYPE_SIZE (type)));
  tree d = m3_build3 (COND_EXPR, type, c, m3_cast (type, v_zero), b);
  return d;
}

#endif

#if !GCC45
static bool
m3_mark_addressable (tree x)
/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   Value is 1 if successful.

   This implementation was copied from c-decl.c. */
{
  while (1)
    switch (TREE_CODE (x))
      {
      case COMPONENT_REF:
      case ADDR_EXPR:
      case ARRAY_REF:
        x = TREE_OPERAND (x, 0);
        break;

      case CONSTRUCTOR:
      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
      case FUNCTION_DECL:
        TREE_ADDRESSABLE (x) = true;
        return true;

      default:
        return true;
      }
}
#endif

static tree
m3_type_for_size (UINT bits, int unsignedp)
/* Return an integer type with the number of bits of precision given by
   PRECISION.  UNSIGNEDP is nonzero if the type is unsigned; otherwise
   it is a signed type.  */
{
  if (bits <= TYPE_PRECISION (t_int_8))
    return unsignedp ? t_word_8  : t_int_8;

  if (bits <= TYPE_PRECISION (t_int_16))
    return unsignedp ? t_word_16 : t_int_16;

  if (bits <= TYPE_PRECISION (t_int_32))
    return unsignedp ? t_word_32  : t_int_32;

  if (bits <= TYPE_PRECISION (t_int_64))
    return unsignedp ? t_word_64  : t_int_64;

  if (bits <= TYPE_PRECISION (t_int))
    return unsignedp ? t_word : t_int;

  return NULL;
}

static tree
m3_type_for_mode (enum machine_mode mode, int unsignedp)
/* Return a data type that has machine mode MODE.  UNSIGNEDP selects
   an unsigned type; otherwise a signed type is returned.  */
{
  if (mode == TYPE_MODE (t_int_8))   return unsignedp ? t_word_8   : t_int_8;
  if (mode == TYPE_MODE (t_int_16))  return unsignedp ? t_word_16  : t_int_16;
  if (mode == TYPE_MODE (t_int_32))  return unsignedp ? t_word_32  : t_int_32;
  if (mode == TYPE_MODE (t_int_64))  return unsignedp ? t_word_64  : t_int_64;
  if (mode == TYPE_MODE (t_int))     return unsignedp ? t_word     : t_int;
  if (mode == TYPE_MODE (t_reel))    return t_reel;
  if (mode == TYPE_MODE (t_lreel))   return t_lreel;
  if (mode == TYPE_MODE (t_xreel))   return t_xreel;
  if (mode == TYPE_MODE (t_int))     return unsignedp ? t_word     : t_int;

  /* TImode is needed to do 64bit mod for ARM. */
  if (mode ==  TImode) return unsignedp ? unsigned_intTI_type_node : intTI_type_node;

  return NULL;
}

static tree
m3_unsigned_type (tree type_node)
/* Return the unsigned version of a TYPE_NODE, a scalar type.  */
{
  return m3_signed_or_unsigned_type (true, type_node);
}

static tree
m3_signed_type (tree type_node)
/* Return the signed version of a TYPE_NODE, a scalar type.  */
{
  return m3_signed_or_unsigned_type (false, type_node);
}

static tree
m3_signed_or_unsigned_type (int unsignedp, tree type)
/* Return a type the same as TYPE except unsigned or signed according to
   UNSIGNEDP.  */
{
  if (! INTEGRAL_TYPE_P (type) || TYPE_UNSIGNED (type) == !!unsignedp)
    return type;
  else
    return m3_type_for_size (TYPE_PRECISION (type), unsignedp);
}

static bool
global_bindings_p (void)
/* Return non-zero if we are currently in the global binding level.  */
{
  return current_block == NULL;
}

/* Return the list of declarations in the current level. Note that this list
   is in reverse order (it has to be so for back-end compatibility).  */

static tree
getdecls (void)
{
  return current_block ? BLOCK_VARS (current_block) : global_decls;
}

#if !GCC45

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside the BIND_EXPR.  */

static void
insert_block (tree block)
{
  TREE_USED (block) = true;
  BLOCK_SUBBLOCKS (current_block)
    = chainon (BLOCK_SUBBLOCKS (current_block), block);
  BLOCK_SUPERCONTEXT (block) = current_block;
}

#endif

/* Records a ..._DECL node DECL as belonging to the current lexical scope.
   Returns the ..._DECL node. */

static tree
pushdecl (tree decl)
{
  gcc_assert (current_block == NULL_TREE || M3_TYPES_ENUM);
  gcc_assert (current_function_decl == NULL_TREE || M3_TYPES_ENUM);
  DECL_CONTEXT (decl) = M3_TYPES_ENUM ? m3_current_scope () : 0;
  TREE_CHAIN (decl) = global_decls;
  global_decls = decl;
  return decl;
}

static void
m3_set_decl_source_location_avoid_unknown (tree decl)
{
#if !GCC42
  if (input_location == UNKNOWN_LOCATION)
    DECL_SOURCE_LOCATION (decl) = BUILTINS_LOCATION;
  else
#endif
    DECL_SOURCE_LOCATION (decl) = input_location;
}

static tree
m3_push_type_decl (tree type, tree name)
{
  gcc_assert (name || M3_TYPES_ENUM);
  gcc_assert (type || M3_TYPES_ENUM);
  if (!type)
    return NULL;
  tree decl = build_decl (TYPE_DECL, name, type);
  if (name)
    TYPE_NAME (type) = name;
  if (M3_TYPES_ENUM)
  {
    DECL_CONTEXT (decl) = m3_current_scope ();
  }
  m3_set_decl_source_location_avoid_unknown (decl);
  TREE_CHAIN (decl) = global_decls;
  global_decls = decl;
  return decl;
}

/* Return a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.

   If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.  If
   ATTRS is nonzero, use that for the function's attribute list.

   copied from gcc/c-decl.c
*/

static tree
builtin_function (PCSTR name, tree type,
#if GCC42
                  int function_code,
#else
                  built_in_function function_code,
#endif
		  enum built_in_class clas,
		  const char *library_name,
		  tree attrs ATTRIBUTE_UNUSED)
{
  tree identifier = get_identifier (name);
  tree decl = build_decl (FUNCTION_DECL, identifier, type);

  TREE_PUBLIC (decl) = true;
  DECL_EXTERNAL (decl) = true;
  DECL_BUILT_IN_CLASS (decl) = clas;
  DECL_FUNCTION_CODE (decl) = (built_in_function)function_code;

  if (GCC42)
  {
    if (library_name)
      SET_DECL_ASSEMBLER_NAME (decl, get_identifier (library_name));
  }
  else
  {
    gcc_assert (!library_name);
  }

  TREE_CHAIN (decl) = global_decls;
  global_decls = decl;

  return decl;
}

static void
m3_write_globals (void)
{
  /* Fix init_offset fields in constructors: VAR_DECL -> offset */
  for (tree ctors = pending_inits; ctors; ctors = TREE_CHAIN (ctors))
  {
    VEC (constructor_elt, gc) *elts = CONSTRUCTOR_ELTS (TREE_VALUE (ctors));
    UINT64 idx;
    tree index, value;

    FOR_EACH_CONSTRUCTOR_ELT (elts, idx, index, value)
    {
      tree var = (tree)DECL_LANG_SPECIFIC (index);
      if (var)
      {
        gcc_assert (TREE_CODE (var) == VAR_DECL);
        if (TREE_ADDRESSABLE (var))
        /* take apart the rtx, which is of the form
           (insn n m p (use (mem: (plus: (reg: r $fp)
           (const_int offset))) ...)
           or
           (insn n m p (use (mem: (reg: r $fp))) ...)
           for offset 0. */
        {
          int j;
          rtx r = DECL_RTL (var);       /* (mem ...) */
          r = XEXP (r, 0);              /* (plus ...) or (reg ...) */
          if (REG_P (r))
          {
            j = 0;
          }
          else
          {
            r = XEXP (r, 1);    /* (const_int ...) */
            j = XWINT (r, 0);  /* offset */
          }
          VEC_index (constructor_elt, elts, idx)->value = size_int (j);
        }
        else
        {
          /*fprintf (stderr, "%s %s not addressable, should be?\n",
                     input_filename, IDENTIFIER_POINTER (DECL_NAME (var)));*/
        }
      }
    }
  }

  if (!GCC45)
    write_global_declarations ();
}

#if !GCC47
static void
sync_builtin (enum built_in_function fncode, tree type, PCSTR name)
{
  tree decl = builtin_function (name, type, fncode, BUILT_IN_NORMAL, 0, 0);
  TREE_NOTHROW (decl) = true;
  built_in_decls[fncode] = implicit_built_in_decls[fncode] = decl;
}
#endif

#if 0 /* future? */

static tree
m3_make_integer_type (UINT size, UINT align, UINT signd)
{
  gcc_assert (size >= 1 && size <= 64);
  gcc_assert (align <= 64);
  gcc_assert (signd <= 1);
  if (size == align)
  {
    if (size == BITS_PER_INTEGER)
      return signd ? t_int : t_word;
    switch (size)
    {
     case  8: return signd ? t_int_8 : t_word_8;
     case 16: return signd ? t_int_16 : t_word_16;
     case 32: return signd ? t_int_32 : t_word_32;
     case 64: return signd ? t_int_64 : t_word_64;
    }
  }
  gcc_assert (align < 2);
  tree type = make_node (INTEGER_TYPE);

  if (size <= 8)
    SET_TYPE_MODE (type, QImode);
  else if (size <= 16)
    SET_TYPE_MODE (type, HImode);
  else if (size <= 32)
    SET_TYPE_MODE (type, SImode);
  else if (size <= 64)
    SET_TYPE_MODE (type, DImode);
  else
    gcc_unreachable ();

  TYPE_ALIGN (type) = align;
  TYPE_USER_ALIGN (type) = true;
  TYPE_UNSIGNED (type) = !signd;
  TYPE_SIZE (type) = build_int_cst (type, i);
  TYPE_SIZE_UNIT (type) = build_int_cst (type, (i + BITS_PER_UNIT - 1) / BITS_PER_UNIT);
  TYPE_PRECISION (type) = i;
  TYPE_PACKED (type) = true;
  return type;
}

#endif

/* Create the predefined scalar types of M3CG,
   and some nodes representing standard constants (0, 1, (void *) 0).
   Initialize the global binding level.
   Make definitions for built-in primitive functions.  */

static void
m3_init_decl_processing (void)
{
  tree t = { 0 };
  enum built_in_function ezero = (enum built_in_function)0;
  UINT i = { 0 };

  current_function_decl = NULL;

#if GCC46 && !GCC47
  build_common_tree_nodes (false /* unsigned char */);
#else
  build_common_tree_nodes (false /* unsigned char */,
                           false /* unsigned size_type or short_double */);
#endif

  if (BITS_PER_INTEGER == 32)
    {
      t_int = t_int_32;
      t_word = t_word_32;
    }
  else if (BITS_PER_INTEGER == 64)
    {
      t_int = t_int_64;
      t_word = t_word_64;
    }
  else
    {
      t_int = make_signed_type (BITS_PER_INTEGER);
      m3_push_type_decl (t_int, get_identifier_with_length (CONSTANT_STRING_AND_LENGTH ("int")));
      t_word = make_unsigned_type (BITS_PER_INTEGER);
      m3_push_type_decl (t_word, get_identifier_with_length (CONSTANT_STRING_AND_LENGTH ("word")));
    }

  pointer_size = POINTER_SIZE; /* for debugging */
  bits_per_integer_tree = build_int_cst (t_word, BITS_PER_INTEGER);
  bytes_per_integer_tree = build_int_cst (t_word, BITS_PER_INTEGER / BITS_PER_UNIT);
  tree stdcall = get_identifier_with_length (CONSTANT_STRING_AND_LENGTH ("stdcall"));
  m3_alloca = get_identifier_with_length (CONSTANT_STRING_AND_LENGTH ("m3_alloca"));
  stdcall_list = build_tree_list (stdcall, NULL);
  t_set = m3_build_pointer_type (t_word);

  /* Set the type used for sizes and build the remaining common nodes. */
  size_type_node = t_word;
#if !GCC47
  set_sizetype (size_type_node);
  build_common_tree_nodes_2 (0);
#endif
  void_list_node = build_tree_list (NULL_TREE, void_type_node);

  /* add builtin uids */

  for (i = 0; i < COUNT_OF (builtin_uids); ++i)
    set_typeid_to_tree (builtin_uids[i].type_id, *builtin_uids[i].t);

  /* declare/name builtin types */

  for (i = 0; i < COUNT_OF (builtin_types); ++i)
  {
    if (builtin_types[i].t)
    {
      m3_push_type_decl (*builtin_types[i].t,
                         get_identifier_with_length (builtin_types[i].name,
                                                     builtin_types[i].length));
    }
  }

  build_common_builtin_nodes ();

  targetm.init_builtins ();

#if !GCC47
  t = t_int_8;
  t = build_function_type_list (t, t_addr, t, NULL_TREE);
  sync_builtin (BUILT_IN_FETCH_AND_ADD_1,  t, "__sync_fetch_and_add_1");
  sync_builtin (BUILT_IN_FETCH_AND_SUB_1,  t, "__sync_fetch_and_sub_1");
  sync_builtin (BUILT_IN_FETCH_AND_OR_1,   t, "__sync_fetch_and_or_1");
  sync_builtin (BUILT_IN_FETCH_AND_AND_1,  t, "__sync_fetch_and_and_1");
  sync_builtin (BUILT_IN_FETCH_AND_XOR_1,  t, "__sync_fetch_and_xor_1");
#if 0 /* warning -- nand changed semantics in gcc 4.4 */
  sync_builtin (BUILT_IN_FETCH_AND_NAND_1, t, "__sync_fetch_and_nand_1");
#endif
  sync_builtin (BUILT_IN_ADD_AND_FETCH_1,  t, "__sync_add_and_fetch_1");
  sync_builtin (BUILT_IN_SUB_AND_FETCH_1,  t, "__sync_sub_and_fetch_1");
  sync_builtin (BUILT_IN_OR_AND_FETCH_1,   t, "__sync_or_and_fetch_1");
  sync_builtin (BUILT_IN_AND_AND_FETCH_1,  t, "__sync_and_and_fetch_1");
  sync_builtin (BUILT_IN_XOR_AND_FETCH_1,  t, "__sync_xor_and_fetch_1");
#if 0 /* warning -- nand changed semantics in gcc 4.4 */
  sync_builtin (BUILT_IN_NAND_AND_FETCH_1, t, "__sync_nand_and_fetch_1");
#endif

  t = t_int_16;
  t = build_function_type_list (t, t_addr, t, NULL_TREE);
  sync_builtin (BUILT_IN_FETCH_AND_ADD_2,  t, "__sync_fetch_and_add_2");
  sync_builtin (BUILT_IN_FETCH_AND_SUB_2,  t, "__sync_fetch_and_sub_2");
  sync_builtin (BUILT_IN_FETCH_AND_OR_2,   t, "__sync_fetch_and_or_2");
  sync_builtin (BUILT_IN_FETCH_AND_AND_2,  t, "__sync_fetch_and_and_2");
  sync_builtin (BUILT_IN_FETCH_AND_XOR_2,  t, "__sync_fetch_and_xor_2");
#if 0 /* warning -- nand changed semantics in gcc 4.4 */
  sync_builtin (BUILT_IN_FETCH_AND_NAND_2, t, "__sync_fetch_and_nand_2");
#endif
  sync_builtin (BUILT_IN_ADD_AND_FETCH_2,  t, "__sync_add_and_fetch_2");
  sync_builtin (BUILT_IN_SUB_AND_FETCH_2,  t, "__sync_sub_and_fetch_2");
  sync_builtin (BUILT_IN_OR_AND_FETCH_2,   t, "__sync_or_and_fetch_2");
  sync_builtin (BUILT_IN_AND_AND_FETCH_2,  t, "__sync_and_and_fetch_2");
  sync_builtin (BUILT_IN_XOR_AND_FETCH_2,  t, "__sync_xor_and_fetch_2");
#if 0 /* warning -- nand changed semantics in gcc 4.4 */
  sync_builtin (BUILT_IN_NAND_AND_FETCH_2, t, "__sync_nand_and_fetch_2");
#endif

  t = t_int_32;
  t = build_function_type_list (t, t_addr, t, NULL_TREE);
  sync_builtin (BUILT_IN_FETCH_AND_ADD_4,  t, "__sync_fetch_and_add_4");
  sync_builtin (BUILT_IN_FETCH_AND_SUB_4,  t, "__sync_fetch_and_sub_4");
  sync_builtin (BUILT_IN_FETCH_AND_OR_4,   t, "__sync_fetch_and_or_4");
  sync_builtin (BUILT_IN_FETCH_AND_AND_4,  t, "__sync_fetch_and_and_4");
  sync_builtin (BUILT_IN_FETCH_AND_XOR_4,  t, "__sync_fetch_and_xor_4");
#if 0 /* warning -- nand changed semantics in gcc 4.4 */
  sync_builtin (BUILT_IN_FETCH_AND_NAND_4, t, "__sync_fetch_and_nand_4");
#endif
  sync_builtin (BUILT_IN_ADD_AND_FETCH_4,  t, "__sync_add_and_fetch_4");
  sync_builtin (BUILT_IN_SUB_AND_FETCH_4,  t, "__sync_sub_and_fetch_4");
  sync_builtin (BUILT_IN_OR_AND_FETCH_4,   t, "__sync_or_and_fetch_4");
  sync_builtin (BUILT_IN_AND_AND_FETCH_4,  t, "__sync_and_and_fetch_4");
  sync_builtin (BUILT_IN_XOR_AND_FETCH_4,  t, "__sync_xor_and_fetch_4");
#if 0 /* warning -- nand changed semantics in gcc 4.4 */
  sync_builtin (BUILT_IN_NAND_AND_FETCH_4, t, "__sync_nand_and_fetch_4");
#endif

  t = t_int_64;
  t = build_function_type_list (t, t_addr, t, NULL_TREE);
  sync_builtin (BUILT_IN_FETCH_AND_ADD_8,  t, "__sync_fetch_and_add_8");
  sync_builtin (BUILT_IN_FETCH_AND_SUB_8,  t, "__sync_fetch_and_sub_8");
  sync_builtin (BUILT_IN_FETCH_AND_OR_8,   t, "__sync_fetch_and_or_8");
  sync_builtin (BUILT_IN_FETCH_AND_AND_8,  t, "__sync_fetch_and_and_8");
  sync_builtin (BUILT_IN_FETCH_AND_XOR_8,  t, "__sync_fetch_and_xor_8");
#if 0 /* warning -- nand changed semantics in gcc 4.4 */
  sync_builtin (BUILT_IN_FETCH_AND_NAND_8, t, "__sync_fetch_and_nand_8");
#endif
  sync_builtin (BUILT_IN_ADD_AND_FETCH_8,  t, "__sync_add_and_fetch_8");
  sync_builtin (BUILT_IN_SUB_AND_FETCH_8,  t, "__sync_sub_and_fetch_8");
  sync_builtin (BUILT_IN_OR_AND_FETCH_8,   t, "__sync_or_and_fetch_8");
  sync_builtin (BUILT_IN_AND_AND_FETCH_8,  t, "__sync_and_and_fetch_8");
  sync_builtin (BUILT_IN_XOR_AND_FETCH_8,  t, "__sync_xor_and_fetch_8");
#if 0 /* warning -- nand changed semantics in gcc 4.4 */
  sync_builtin (BUILT_IN_NAND_AND_FETCH_8, t, "__sync_nand_and_fetch_8");
#endif

  t = t_int_8;
  sync_builtin (BUILT_IN_BOOL_COMPARE_AND_SWAP_1,
                build_function_type_list (boolean_type_node, t_addr, t, t,
                                          NULL_TREE),
                "__sync_bool_compare_and_swap_1");
  sync_builtin (BUILT_IN_VAL_COMPARE_AND_SWAP_1,
                build_function_type_list (t, t_addr, t, t, NULL_TREE),
                "__sync_val_compare_and_swap_1");

  t = t_int_16;
  sync_builtin (BUILT_IN_BOOL_COMPARE_AND_SWAP_2,
                build_function_type_list (boolean_type_node, t_addr, t, t,
                                          NULL_TREE),
                "__sync_bool_compare_and_swap_2");
  sync_builtin (BUILT_IN_VAL_COMPARE_AND_SWAP_2,
                build_function_type_list (t, t_addr, t, t, NULL_TREE),
                "__sync_val_compare_and_swap_2");

  t = t_int_32;
  sync_builtin (BUILT_IN_BOOL_COMPARE_AND_SWAP_4,
                build_function_type_list (boolean_type_node, t_addr, t, t,
                                          NULL_TREE),
                "__sync_bool_compare_and_swap_4");
  sync_builtin (BUILT_IN_VAL_COMPARE_AND_SWAP_4,
                build_function_type_list (t, t_addr, t, t, NULL_TREE),
                "__sync_val_compare_and_swap_4");

  t = t_int_64;
  sync_builtin (BUILT_IN_BOOL_COMPARE_AND_SWAP_8,
                build_function_type_list (boolean_type_node, t_addr, t, t,
                                          NULL_TREE),
                "__sync_bool_compare_and_swap_8");
  sync_builtin (BUILT_IN_VAL_COMPARE_AND_SWAP_8,
                build_function_type_list (t, t_addr, t, t, NULL_TREE),
                "__sync_val_compare_and_swap_8");

  sync_builtin (BUILT_IN_SYNCHRONIZE,
                build_function_type_list (t_void, NULL_TREE),
                "__sync_synchronize");

  t = t_int_8;
  sync_builtin (BUILT_IN_LOCK_TEST_AND_SET_1,
                build_function_type_list (t, t_addr, t, NULL_TREE),
                "__sync_lock_test_and_set_1");
  sync_builtin (BUILT_IN_LOCK_RELEASE_1,
                build_function_type_list (t, t_addr, NULL_TREE),
                "__sync_lock_release_1");

  t = t_int_16;
  sync_builtin (BUILT_IN_LOCK_TEST_AND_SET_2,
                build_function_type_list (t, t_addr, t, NULL_TREE),
                "__sync_lock_test_and_set_2");
  sync_builtin (BUILT_IN_LOCK_RELEASE_2,
                build_function_type_list (t, t_addr, NULL_TREE),
                "__sync_lock_release_2");

  t = t_int_32;
  sync_builtin (BUILT_IN_LOCK_TEST_AND_SET_4,
                build_function_type_list (t, t_addr, t, NULL_TREE),
                "__sync_lock_test_and_set_4");
  sync_builtin (BUILT_IN_LOCK_RELEASE_4,
                 build_function_type_list (t, t_addr, NULL_TREE),
                 "__sync_lock_release_4");

  t = t_int_64;
  sync_builtin (BUILT_IN_LOCK_TEST_AND_SET_8,
                build_function_type_list (t, t_addr, t, NULL_TREE),
                "__sync_lock_test_and_set_8");
  sync_builtin (BUILT_IN_LOCK_RELEASE_8,
                build_function_type_list (t, t_addr, NULL_TREE),
                "__sync_lock_release_8");
#endif

  t = build_function_type_list (t_void, NULL_TREE);
  set_union_proc  = builtin_function ("set_union", t, ezero, NOT_BUILT_IN, 0, 0);
  set_diff_proc   = builtin_function ("set_difference", t, ezero, NOT_BUILT_IN, 0, 0);
  set_inter_proc  = builtin_function ("set_intersection", t, ezero, NOT_BUILT_IN, 0, 0);
  set_sdiff_proc  = builtin_function ("set_sym_difference", t, ezero, NOT_BUILT_IN, 0, 0);
  set_range_proc  = builtin_function ("set_range", t, ezero, NOT_BUILT_IN, 0, 0);

  t = build_function_type_list (t_int, NULL_TREE);
  set_gt_proc = builtin_function ("set_gt", t, ezero, NOT_BUILT_IN, 0, 0);
  set_ge_proc = builtin_function ("set_ge", t, ezero, NOT_BUILT_IN, 0, 0);
  set_lt_proc = builtin_function ("set_lt", t, ezero, NOT_BUILT_IN, 0, 0);
  set_le_proc = builtin_function ("set_le", t, ezero, NOT_BUILT_IN, 0, 0);
}

/*========================================================== DECLARATIONS ===*/

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
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE DOUBLE_TYPE_SIZE
#endif

#ifndef MAX
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))
#endif

/* Variable arrays of trees. */

static map<size_t, m3cg_BIND_SEGMENT_t*> bind_segments; /* wasteful but ok -- there's only ever about two */
static std::vector<tree> all_vars;
static std::vector<tree> all_procs;
static std::vector<tree> all_labels;
static std::vector<tree> expr_stack;
static std::vector<tree> call_stack;

static void STACK_PUSH(std::vector<tree>& stack, tree x) { stack.push_back(x); }
static void STACK_POP(std::vector<tree>& stack) { stack.pop_back(); }
static tree& STACK_REF(std::vector<tree>& stack, ptrdiff_t n)
{
  size_t const size = stack.size ();
  gcc_assert (n < 0);
  gcc_assert (size >= (size_t)-n);
  return stack[size + n];
}

static void EXPR_PUSH (tree x) { STACK_PUSH (expr_stack, x); }
static void EXPR_POP (void) { STACK_POP (expr_stack); }
static tree& EXPR_REF (int n) { return STACK_REF (expr_stack, n); }

/* The call stack has triples on it: first the argument chain, then
   the type chain, then the static chain expression. */
static void CALL_PUSH (tree args, tree types, tree static_chain)
{
  STACK_PUSH (call_stack, args);
  STACK_PUSH (call_stack, types);
  STACK_PUSH (call_stack, static_chain);
}

static void CALL_POP (void)
{
  call_stack.resize(call_stack.size() - 3);
}

static tree& CALL_TOP_ARG (void) { return STACK_REF (call_stack, -3); }
static tree& CALL_TOP_TYPE (void) { return STACK_REF (call_stack, -2); }
static tree& CALL_TOP_STATIC_CHAIN (void) { return STACK_REF (call_stack, -1); }

/*=============================================================== PARSING ===*/

static UCHAR* input_buffer;
static size_t input_len;
static size_t input_cursor;
static bool input_eof;
static UINT m3cg_lineno = 1;

/*-------------------------------------------------------- buffer loading ---*/

static void
m3_init_parse (void)
{
  all_vars.reserve(100);
  all_procs.reserve(100);
  all_labels.reserve(100);
  expr_stack.reserve(100);
  call_stack.reserve(200 * 2);
}

static void
m3_read_entire_file (FILE* file, UCHAR** out_buffer, size_t* out_size)
{
  size_t buffer_size = { 0 };
  UCHAR* buffer = { 0 };
  size_t bytes_read = { 0 };
  size_t total_bytes_read = { 0 };
  size_t bytes_to_read = { 0 };

  while (bytes_read == bytes_to_read)
  {
    buffer_size += buffer_size ? buffer_size : 0x10000;
    buffer = (UCHAR*)xrealloc (buffer, buffer_size);
    bytes_to_read = (buffer_size - total_bytes_read);
    bytes_read = fread (buffer + total_bytes_read,
                        1,
                        buffer_size - total_bytes_read,
                        file);
    total_bytes_read += bytes_read;
  }
  /* shrink it down (or up by 1) and add terminal nul */
  buffer = (UCHAR*)xrealloc (buffer, total_bytes_read + 1);
  buffer[total_bytes_read] = 0;
  *out_buffer = buffer;
  *out_size = total_bytes_read;
}

static void
m3_init_lex (void)
{
  input_cursor = 0;
  input_eof = (input_len <= 0);
}

static UCHAR
get_byte (void)
{
  if (input_cursor >= input_len)
  {
    input_eof = true;
    return 0;
  }
  return input_buffer[input_cursor++];
}

static UCHAR*
get_bytes_direct (size_t count)
{
  if ((input_cursor + count) > input_len)
  {
    fatal_error ("read past buffer input_cursor:%u count:%u input_len:%u\n",
                 (UINT)input_cursor,
                 (UINT)count,
                 (UINT)input_len);
  }
  input_cursor += count;
  return &input_buffer[input_cursor - count];
}

static PCSTR
m3_get_var_trace_name (tree var)
{
  if (option_trace_all && var && DECL_NAME (var) && IDENTIFIER_POINTER (DECL_NAME (var)))
    return IDENTIFIER_POINTER (DECL_NAME (var));
  return "noname";
}

static PCSTR
trace_name (PCSTR* inout_name)
/* if name is single character, change to empty and return
   empty string delineate it; else return colon to delineate */
{
  PCSTR name = *inout_name;
  if (!name[0] || !name[1]) /* don't print single character names */
  {
    *inout_name = "";
    return "";
  }
  return ":";
}

static PSTR
trace_upper_hex (PSTR format)
/* This function adjusts a format string, changing lowercase 'x' to
   uppercase 'X'. The first character in the string serves as an indicator
   as to if the conversion has been done. It is 'x' for not yet done and
   ' ' for done. */
{
  PSTR a = format;
  if (a[0] == 'x')
  {
    a[0] = ' ';
    while ((a = strchr(a, 'x')))
    {
      if (a[-1] != '0')
        *a = 'X';
      a += 1;
    }
  }
  return format;
}

static void
trace_int (PCSTR name, INT64 val)
/* This function prints an integer, taking a little pain for readability.
   Single digit integers are printed only in decimal.
   Larger integers are printed in hex and decimal, like:
     0x40(64) */
{
  if (!name || !option_trace_all)
    return;
  PCSTR colon = trace_name (&name);
  static char hex[] = "x%s%s"WIDE_PRINT_HEX"("WIDE_PRINT_DEC")";
  if (val >= -9 && val <= 9)
    fprintf (stderr, " %s%s"WIDE_PRINT_DEC, name, colon, (WIDE)val);
  else
    fprintf (stderr, trace_upper_hex (hex), name, colon, (WIDE)val, (WIDE)val);
}

static INT64
get_int (void)
/* This function reads an integer from our specially encoded format. */
{
  UINT n_bytes = { 0 };
  int sign = { 0 };
  UINT shift = { 0 };
  INT64 val = { 0 };
  UINT i = get_byte ();

  gcc_assert (sizeof(INT64) >= 8);

  switch (i)
  {
  case M3CG_Int1:   return (INT64) get_byte ();
  case M3CG_NInt1:  return - (INT64) get_byte ();
  case M3CG_Int2:   n_bytes = 2;  sign =  1;  break;
  case M3CG_NInt2:  n_bytes = 2;  sign = -1;  break;
  case M3CG_Int4:   n_bytes = 4;  sign =  1;  break;
  case M3CG_NInt4:  n_bytes = 4;  sign = -1;  break;
  case M3CG_Int8:   n_bytes = 8;  sign =  1;  break;
  case M3CG_NInt8:  n_bytes = 8;  sign = -1;  break;
  default:          return i;
  }

  for (i = 0; i < n_bytes;  (++i), (shift += 8))
    val |= (((INT64)get_byte ()) << shift);
  return sign * val;
}

static UINT64
get_uint (void)
{
  INT64 i = get_int ();
  gcc_assert (i >= 0);
  return (UINT64)i;
}

static UINT32
get_typeid ()
/* This function reads and traces a type_id in specially encoded format.
   Typeids simply 32bit unsigned integers. */
{
  return (UINT32)(0xFFFFFFFFUL & (UINT32)get_int ());
}

static void
trace_typeid (PCSTR name, UINT32 val)
{
  if (!name || !option_trace_all)
    return;
  PCSTR colon = trace_name (&name);
  fprintf (stderr, " %s%s0x%X", name, colon, (UINT)val);
}

/*--------------------------------------------------------------- strings ---*/

static PCSTR
scan_string (long length)
/* NOTE: these are not null terminated */
{
  PCSTR result = { 0 };
  if (length > 0)
    result = (PCSTR)get_bytes_direct (long_to_sizet (length));
  return result;
}

static void
trace_string (PCSTR name, PCSTR result, long length)
{
  if (!name || !option_trace_all)
    return;
  PCSTR colon = trace_name (&name);
  fprintf (stderr, " %s%s%.*s", name, colon,
           result ? long_to_printf_length (length) : 4,
           result ? result : "null");
}

/*---------------------------------------------------- calling convention ---*/

static tree
scan_calling_convention (void)
{
  UINT8 id = get_byte ();
  switch (id)
  {
  case 0: return NULL_TREE;
  case 1: return stdcall_list;
  default:
    fatal_error (" *** invalid calling convention: 0x%x, at m3cg_lineno %u",
                 (int)id, m3cg_lineno);
  }
}

/*----------------------------------------------------------------- types ---*/

static m3_type
scan_type (void)
{
  UINT8 i = get_byte ();
  if (i >= T_LAST)
    fatal_error (" *** illegal type: 0x%x, at m3cg_lineno %u", (UINT)i, m3cg_lineno);
  return (m3_type)i;
}

static void
trace_type (PCSTR name, m3_type i)
{
  if (!option_trace_all)
    return;
  PCSTR colon = trace_name (&name);
  fprintf (stderr, " %s%s%s", name, colon, typestr (i));
}

static tree
scan_mtype (m3_type* T)
{
  m3_type TT = scan_type ();
  if (T)
    *T = TT;
  tree t = m3_build_type (TT, 0, 0);
  /* m3_gc_tree (t); */
  return t;
}

static void
trace_type_tree (PCSTR name, tree t)
{
  if (!option_trace_all)
    return;
  for (size_t i = 0; i < COUNT_OF (builtin_types); ++i)
  {
    if (builtin_types [i].t && *builtin_types [i].t == t)
    {
      trace_type (name, (m3_type)i);
      return;
    }
  }
}

/*----------------------------------------------------------------- signs ---*/

static UINT
scan_sign (void)
{
  UINT8 x = get_byte ();
  switch (x)
  {
  case 0: break; /* positive */
  case 1: break; /* negative */
  case 2: break; /* unknown */
  default:
    fatal_error (" *** bad sign: 0x%x, at m3cg_lineno %u", (UINT)x, m3cg_lineno);
  }
  return x;
}

/*----------------------------------------------------------------- float ---*/

static bool
IsHostBigEndian (void)
{
    union
    {
        int i;
        char c[sizeof(int)];
    } u;
    u.i = 1;
    return (u.c[0] == 0);
}

static tree
scan_float (UINT *out_Kind)
{
  /* real_from_target_fmt wants floats stored in an array of longs, 32 bits
     per long, even if long can hold more.  So for example a 64 bit double on
     a system with 64 bit long will have 32 bits of zeros in the middle. */
  long Longs[2] = { 0, 0 };
  UCHAR * const Bytes = (UCHAR*)Longs;
  static const struct {
    tree* Tree;
    UINT Size;
    const struct real_format* format;
  } Map[] = { { &t_reel ,  (FLOAT_TYPE_SIZE / 8), &ieee_single_format },
              { &t_lreel, (DOUBLE_TYPE_SIZE / 8), &ieee_double_format },
              { &t_xreel, (LONG_DOUBLE_TYPE_SIZE / 8), &ieee_double_format }};
  REAL_VALUE_TYPE val;

  memset (&val, 0, sizeof(val));
  gcc_assert (sizeof(float) == 4);
  gcc_assert (sizeof(double) == 8);
  gcc_assert (FLOAT_TYPE_SIZE == 32);
  gcc_assert (DOUBLE_TYPE_SIZE == 64);
  gcc_assert (LONG_DOUBLE_TYPE_SIZE == 64);
  gcc_assert (sizeof(long) == 4 || sizeof(long) == 8);

  UINT Kind = (UINT)get_byte ();
  if (Kind >= (sizeof(Map) / sizeof(Map[0])))
    {
      fatal_error (" *** invalid floating point value, precision = 0x%x, at m3cg_lineno %u",
                   Kind, m3cg_lineno);
    }
  *out_Kind = Kind;
  UINT Size = Map[Kind].Size;

  gcc_assert (Size == 4 || Size == 8);

  /* read the value's bytes; each long holds 32 bits, even if long is larger
     than 32 bits always read the bytes in increasing address, independent of
     endianness */
  for (UINT i = 0; i < Size; ++i)
    Bytes[i / 4 * sizeof(long) + i % 4] = get_byte ();

  /* When crossing and host/target different endian, swap the longs. */

  if ((Size == 8) && (IsHostBigEndian () != FLOAT_WORDS_BIG_ENDIAN))
  {
      long t = Longs[0];
      Longs[0] = Longs[1];
      Longs[1] = t;
  }

  /* finally, assemble a floating point value */
  real_from_target_fmt (&val, Longs, Map[Kind].format);
  tree t = build_real (*Map[Kind].Tree, val);
  m3_gc_tree (t);
  return t;
}

#if 0 /* future? */

static void
trace_float (PCSTR /*name*/, UINT kind, long Longs[2])
{
  if (!option_trace_all)
    return;

  UCHAR * const Bytes = (UCHAR*)Longs;
  static const UINT Sizes[] = { FLOAT_TYPE_SIZE / 8,
                                DOUBLE_TYPE_SIZE / 8,
                                LONG_DOUBLE_TYPE_SIZE / 8 };
  UINT Size = Sizes[kind];
  union
  {
    UCHAR Bytes[sizeof(long double)]; /* currently double suffices */
    float Float;
    double Double;
    long double LongDouble; /* not currently used */
  } u = { { 0 } };

  /* repack the bytes adjacent to each other */

  for (UINT i = 0; i < Size; ++i)
    u.Bytes[i] = Bytes[i / 4 * sizeof(long) + i % 4];
  if (Size == 4)
  {
    fprintf (stderr, " float:%f bytes:0x%02x%02x%02x%02x",
             u.Float, u.Bytes[0], u.Bytes[1], u.Bytes[2], u.Bytes[3]);
  }
  else
  {
    fprintf (stderr, " double:%f bytes:0x%02x%02x%02x%02x%02x%02x%02x%02x",
             u.Double,
             u.Bytes[0], u.Bytes[1], u.Bytes[2], u.Bytes[3],
             u.Bytes[4], u.Bytes[5], u.Bytes[6], u.Bytes[7]);
  }
}

#endif

/*-------------------------------------------------------------- booleans ---*/

static bool
scan_boolean ()
{
  return (get_byte () != 0);
}

static void
trace_boolean (PCSTR name, bool val)
{
  if (name && option_trace_all)
    fprintf (stderr, " %s:%s", name, boolstr (val));
}

/*------------------------------------------------------------- variables ---*/

static void
VARRAY_EXTEND (std::vector<tree>& va, size_t n)
{
  va.reserve(n);
  while (n > va.size())
    va.push_back(NULL);
}

static tree
scan_var (enum tree_code code, size_t* p)
{
  size_t i = uint64_to_sizet (get_uint ());
  if (p)
    *p = i;

  VARRAY_EXTEND (all_vars, i + 1);
  tree var = all_vars[i];
  if (code == ERROR_MARK)
  {
    if (var == NULL)
      fatal_error ("*** variable should already exist, v.0x%x, line %u",
                   (int)i, m3cg_lineno);
  }
  else
  {
    if (var != NULL)
      fatal_error ("*** variable should not already exist, v.0x%x, line %u",
                   (int)i, m3cg_lineno);
    var = m3_gc_tree (make_node (code));
    m3_set_decl_source_location_avoid_unknown (var);
    all_vars[i] = var;
    DECL_NAME (var) = NULL_TREE;
  }
  return var;
}

static void
trace_var (PCSTR name, tree var, size_t a)
{
  if (!option_trace_all)
    return;
  const char* var_string = "var:";
  if (strcmp (name, "var") == 0)
    var_string = "";
  const char* var_name = m3_get_var_trace_name (var);
  const char* colon = ":";
  if (strcmp (var_name, "noname") == 0)
  {
    var_name = "";
    colon = "";
  }
  fprintf (stderr, " %s%s:0x%X%s%s", var_string, name, (UINT)a, colon, var_name);
}

/*------------------------------------------------------------ procedures ---*/

static tree
scan_proc (size_t* pi)
{
  ptrdiff_t i = int64_to_ptrdifft (get_int ());
  if (pi)
    *pi = i;
  tree p = { 0 };

  if (i <= 0)
    return NULL;
  VARRAY_EXTEND (all_procs, i + 1);
  if (all_procs[i] == NULL)
  {
    p = m3_gc_tree (make_node (FUNCTION_DECL));
    m3_set_decl_source_location_avoid_unknown (p);
    DECL_PRESERVE_P (p) = true;
    all_procs[i] = p;
  }
  else
  {
    p = all_procs[i];
  }
  return p;
}

static void
trace_proc (PCSTR, tree p, size_t a)
{
  if (!option_trace_all)
    return;
  fprintf (stderr, " procedure:0x%X", (UINT)a);
  if (p && DECL_NAME (p) && IDENTIFIER_POINTER (DECL_NAME (p)))
    fprintf (stderr, ":%s", IDENTIFIER_POINTER (DECL_NAME (p)));
}

/*---------------------------------------------------------------- labels ---*/

static tree
scan_label (size_t* p)
{
  ptrdiff_t i = int64_to_ptrdifft (get_int ());
  if (p)
    *p = i;

  if (i < 0)
    return NULL;
  VARRAY_EXTEND (all_labels, i + 1);
  if (all_labels[i] == NULL)
    all_labels[i] = m3_gc_tree (build_decl (LABEL_DECL, NULL_TREE, t_void));
  return all_labels[i];
}

static void
trace_label (PCSTR name, size_t a)
{
  trace_int (name, a);
}

/*======================================== debugging and type information ===*/

static m3buf_t current_dbg_type_tag_buf;
static PSTR current_dbg_type_tag = current_dbg_type_tag_buf.buf;
static UINT32 current_record_type_id = NO_UID;
static UINT32 current_object_type_id = NO_UID;
static UINT32 current_proc_type_id = NO_UID; /* not right yet */
static UINT64 current_record_size;
static int current_dbg_type_count1;
static int current_dbg_type_count2;
static int current_dbg_type_count3;

static void
format_tag_v (m3buf_t* buf, char kind, UINT32 type_id, PCSTR fmt, va_list args)
{
  if (!m3gdb)
    return;
  buf->buf [0] = 'M';
  buf->buf [1] = kind;
  buf->buf [2] = '_';
  fmt_uid (type_id, &buf->buf[3]);

  buf->buf [sizeof(buf->buf) - 2] = 0;
  vsnprintf (&buf->buf[UID_SIZE + 3], sizeof(buf->buf) - UID_SIZE - 3, fmt, args);
  if (buf->buf[sizeof(buf->buf) - 2])
  {
    buf->buf[sizeof(buf->buf) - 1] = 0;
    fatal_error ("identifier too long (in debug_tag, %s)", buf->buf);
  }
}

static void
debug_tag (char kind, UINT32 type_id, PCSTR fmt, ...)
{
  va_list args;
  if (!m3gdb)
    return;
  va_start (args, fmt);
  format_tag_v (&current_dbg_type_tag_buf, kind, type_id, fmt, args);
  va_end (args);
}

#if 0

static PCSTR
safe_identifier_pointer_decl_name (tree t)
/* safe form of IDENTIFIER_POINTER (DECL_NAME (t)) */
{
  if (t && DECL_NAME (t) && IDENTIFIER_POINTER (DECL_NAME (t)))
    return IDENTIFIER_POINTER (DECL_NAME (t));
  return "(null)";
}

static long
safe_decl_field_offset (tree t)
/* safe form of TREE_INT_CST_LOW (DECL_FIELD_OFFSET (t)) */
{
  if (t && DECL_FIELD_OFFSET (t))
    return TREE_INT_CST_LOW (DECL_FIELD_OFFSET (t));
  return -1;
}

#endif

static void
dump_record_type (tree record_type)
{
  tree field = { 0 };
  UINT32 type_id = { 0 };
  
  if (!option_trace_all)
    return;

  if (current_record_type_id != NO_UID)
    type_id = current_record_type_id;
  else if (current_object_type_id != NO_UID)
    type_id = current_object_type_id;
  fprintf (stderr, "\ndump_record_type type_id=0x%X, size=0x%X:\n",
           (UINT)type_id, (UINT)current_record_size);
  for (field = TYPE_FIELDS (record_type); field; field = TREE_CHAIN (field))
  {
    fprintf (stderr, "  %s offset=0x%X\n",
             IDENTIFIER_POINTER (DECL_NAME (field)),
             (UINT)(TREE_INT_CST_LOW (DECL_FIELD_OFFSET (field))
             + TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (field))));
  }
  fprintf (stderr, "\n");
}

static void
debug_field_name_length (PCSTR name, size_t length)
{
  tree f = { 0 };
  if (!m3gdb)
    return;
  f = build_decl (FIELD_DECL, get_identifier_with_length (name, length), t_int);
  DECL_FIELD_OFFSET (f) = size_zero_node;
  DECL_FIELD_BIT_OFFSET (f) = bitsize_zero_node;
  layout_decl (f, 1);
  TREE_CHAIN (f) = debug_fields;
  debug_fields = f;
}

static void
debug_field_name (PCSTR name)
{
  debug_field_name_length (name, strlen (name));
}

static void
debug_field_id (UINT32 type_id)
{
  char buf [UID_SIZE];
  if (!m3gdb)
    return;
  fmt_uid (type_id, buf);
  debug_field_name_length (buf, sizeof(buf));
}

static void
debug_field_fmt_v (UINT32 type_id, PCSTR fmt, va_list args)
{
  char name [256];

  if (!m3gdb)
    return;
  fmt_uid (type_id, name);
  name[sizeof(name) - 2] = 0;
  vsnprintf (name + UID_SIZE, sizeof(name) - UID_SIZE, fmt, args);
  if (name[sizeof(name) - 2])
  {
    name[sizeof(name) - 1] = 0;
    fatal_error ("identifier too long (in debug_field_fmt, %s)", name);
  }
  debug_field_name (name);
}

static void
debug_field_fmt (UINT32 type_id, PCSTR fmt, ...)
{
  if (!m3gdb)
    return;
  va_list args;
  va_start (args, fmt);
  debug_field_fmt_v (type_id, fmt, args);
  va_end (args);
}

static void
debug_struct (void)
{
  m3_outdent ();
  if (!m3gdb)
    return;

  tree t = make_node (RECORD_TYPE);
  TYPE_FIELDS (t) = nreverse (debug_fields);
  debug_fields = NULL;
  TYPE_NAME (t) = build_decl (TYPE_DECL, get_identifier (current_dbg_type_tag), t);
  /* TYPE_MAIN_VARIANT (t) = t; */
  TYPE_SIZE (t) = bitsize_one_node;
  TYPE_SIZE_UNIT (t) = m3_convert (sizetype,
                                   size_binop (FLOOR_DIV_EXPR,
                                               TYPE_SIZE (t),
                                               bitsize_int (BITS_PER_UNIT)));
  TYPE_ALIGN (t) = BITS_PER_UNIT;
  SET_TYPE_MODE (t, QImode);

  tree d = build_decl (TYPE_DECL, NULL_TREE, t);
  /* TYPE_MAIN_VARIANT (d) = d; */
  TREE_CHAIN (d) = global_decls;
  global_decls = d;
  debug_hooks -> type_decl
    ( d, false /* This argument means "IsLocal", but it's unused by dbx. */ );
}

/*========================================== GLOBALS FOR THE M3CG MACHINE ===*/

static PCSTR current_unit_name; /* not nul terminated */
static size_t current_unit_name_length;

/* the exported interfaces */
static long exported_interfaces;
static PCSTR exported_interfaces_names [100];

/*================================= SUPPORT FOR INITIALIZED DATA CREATION ===*/

static UINT64 current_record_offset;

static void one_gap (UINT64 offset);

static void
one_field (UINT64 offset,
           UINT64 size,
           tree type,
           tree *out_f,
           tree *out_v)
{
  if (option_trace_all)
      fprintf (stderr, " one_field: offset:0x%X size:0x%X", (UINT)offset,
               (UINT)size);

  one_gap (offset);
  tree f = build_decl (FIELD_DECL, 0, type);
  *out_f = f;
  if ((offset % BITS_PER_UNIT) == 0 && (size % BITS_PER_UNIT) == 0)
  {
    TREE_ADDRESSABLE (f) = true;
    DECL_ALIGN (f) = BITS_PER_UNIT;
  }
  else
  {
    DECL_BIT_FIELD (f) = true;
    DECL_ALIGN (f) = 1;
  }
  DECL_FIELD_OFFSET (f) = size_int (offset / BITS_PER_UNIT);
  DECL_FIELD_BIT_OFFSET (f) = bitsize_int (offset % BITS_PER_UNIT);
  DECL_CONTEXT (f) = current_record_type;
  TREE_CHAIN (f) = TYPE_FIELDS (current_record_type);
  TYPE_FIELDS (current_record_type) = f;
  layout_decl (f, DECL_ALIGN (f));

  *out_v = current_record_vals = tree_cons (f, NULL_TREE, current_record_vals);
#if 1
  current_record_offset = offset + TREE_INT_CST_LOW (TYPE_SIZE (type));
#else
  current_record_offset = offset + size;
#endif
}

static void
one_gap (UINT64 next_offset)
{
  tree f = { 0 };
  tree v = { 0 };
  UINT64 size = next_offset - current_record_offset;

  if (next_offset <= current_record_offset)
    return;

  if (option_trace_all)
      fprintf (stderr, "\n one_gap: offset:0x%X size:0x%X\n",
               (UINT)current_record_offset, (UINT)size);

  tree type = make_node (LANG_TYPE);
  TYPE_SIZE (type) = bitsize_int (size);
  TYPE_SIZE_UNIT (type) = size_int (size / BITS_PER_UNIT);
  TYPE_ALIGN (type) = BITS_PER_UNIT;
  one_field (current_record_offset, size, type, &f, &v);
  TREE_VALUE (v) = build_constructor (TREE_TYPE (f), 0);
}

static void
m3_field (PCSTR name, size_t name_length, tree type, UINT64 offset,
          UINT64 size, tree*, tree*);

static void
m3_gap (UINT64 next_offset)
{
  tree f = { 0 };
  tree v = { 0 };
  UINT64 size = next_offset - current_record_offset;
  char name[256];

  if (next_offset <= current_record_offset || !M3_TYPES)
    return;

  if (option_trace_all)
    fprintf (stderr, "\n m3_gap: offset:0x%X size:0x%X\n",
             (UINT)current_record_offset, (UINT)size);

  sprintf(name, "_m3gap_"WIDE_PRINT_DEC"_"WIDE_PRINT_DEC,
          (WIDE)current_record_offset, (WIDE)size);

  tree type = make_node (RECORD_TYPE);
  TYPE_SIZE (type) = bitsize_int (size);
  TYPE_SIZE_UNIT (type) = size_int (size / BITS_PER_UNIT);
  TYPE_ALIGN (type) = 1;
  m3_field (name, strlen (name), type, current_record_offset, size, &f, &v);
  DECL_PACKED (f) = true;
  DECL_BIT_FIELD (f) = true;
  TREE_VALUE (v) = build_constructor (TREE_TYPE (f), 0);
}

static void
m3_field (PCSTR name, size_t name_length, tree type, UINT64 offset,
          UINT64 size, tree* out_f, tree* out_v)
{
  tree f = { 0 };
  tree v = { 0 };

  if (!M3_TYPES)
    return;

  gcc_assert (offset >= current_record_offset);
  m3_gap (offset);

  f = build_decl (FIELD_DECL, 0, type);
  *out_f = f;
  if ((offset % BITS_PER_UNIT) == 0 && (size % BITS_PER_UNIT) == 0)
  {
    TREE_ADDRESSABLE (f) = true;
    DECL_ALIGN (f) = BITS_PER_UNIT;
  }
  else
  {
    DECL_BIT_FIELD (f) = true;
    DECL_ALIGN (f) = 1;
  }
  DECL_FIELD_OFFSET (f) = size_int (offset / BITS_PER_UNIT);
  DECL_FIELD_BIT_OFFSET (f) = bitsize_int (offset % BITS_PER_UNIT);
  DECL_CONTEXT (f) = current_record_type;
  TREE_CHAIN (f) = TYPE_FIELDS (current_record_type);
  TYPE_FIELDS (current_record_type) = f;

  v = current_record_vals = tree_cons (f, NULL_TREE, current_record_vals);
  *out_v = v;
  current_record_offset += size;
  DECL_NAME (f) = get_identifier_with_length (name, name_length);
  DECL_SIZE_UNIT (f) = size_int (size / BITS_PER_UNIT);
  DECL_SIZE (f) = bitsize_int (size);
  layout_decl (f, DECL_ALIGN (f));
}

/*========================================= SUPPORT FUNCTIONS FOR YYPARSE ===*/

static void add_stmt (tree t)
{
  enum tree_code code = TREE_CODE (t);

#if GCC42
  if (EXPR_P (t) && code != LABEL_EXPR)
#else /* GCC43, GCC45 */
  if (CAN_HAVE_LOCATION_P (t) && code != LABEL_EXPR)
#endif
    {
      if (!EXPR_HAS_LOCATION (t))
        SET_EXPR_LOCATION (t, input_location);
    }

  TREE_USED (t) = true; /* why? */
  TREE_SIDE_EFFECTS (t) = true; /* why? */
  append_to_statement_list (t, &current_stmts);
}

static tree
fix_name (PCSTR name, size_t length, UINT32 type_id)
{
  PSTR buf = { 0 };

  if (name == 0 || name[0] == '*')
  {
    static UINT64 anonymous_counter;
    buf = (PSTR)alloca (23); /* "L_-9223372036854775808" */
    buf[0] = 'L';
    buf[1] = '_';
    m3_uint64_to_dec_shortest (++anonymous_counter, &buf[2]);
    length = strlen (buf);
    gcc_assert (length < 23);
  }
  else if (type_id == NO_UID)
  {
    buf = (PSTR)alloca (sizet_add(length, 1));
    buf[0] = 'M';
    memcpy (&buf[1], name, length);
    length += 1;
  }
  else if (type_id == 0 || !m3gdb)
  {
    buf = (PSTR)name;
  }
  else
  {
    buf = (PSTR)alloca (sizet_add (length, UID_SIZE + 4));
    buf[0] = 'M';  buf[1] = '3';  buf[2] = '_';
    fmt_uid (type_id, buf + 3);
    buf[3 + UID_SIZE] = '_';
    memcpy (&buf[4 + UID_SIZE], name, length);
    length += UID_SIZE + 4;
  }
  return get_identifier_with_length (buf, length);
}

static tree
declare_temp (tree type)
{
  tree v = build_decl (VAR_DECL, 0, type);

  m3_set_decl_source_location_avoid_unknown (v);
  DECL_UNSIGNED (v) = TYPE_UNSIGNED (type);
  DECL_CONTEXT (v) = current_function_decl;

  if (get_volatize ())
    m3_volatilize_decl (v);

  add_stmt (build1 (DECL_EXPR, t_void, v));
  TREE_CHAIN (v) = BLOCK_VARS (current_block);
  BLOCK_VARS (current_block) = v;

  return v;
}

/* Return a tree representing the address of the given procedure.  The static
   address is used rather than the trampoline address for a nested
   procedure.  */
static tree
proc_addr (tree p)
{
  tree expr = build1 (ADDR_EXPR, m3_build_pointer_type (TREE_TYPE (p)), p);
#if GCC45
  TREE_NO_TRAMPOLINE (expr) = true;
#else
  TREE_STATIC (expr) = true; /* see check for TREE_STATIC on ADDR_EXPR
                                in tree-nested.c */
#endif
  return expr;
}

static void
m3_start_call (void)
{
  CALL_PUSH (NULL_TREE, NULL_TREE, NULL_TREE);
}

static void
m3_pop_param (tree type)
{
  CALL_TOP_ARG ()
    = chainon (CALL_TOP_ARG (),
               build_tree_list (NULL_TREE, EXPR_REF (-1)));
  CALL_TOP_TYPE ()
    = chainon (CALL_TOP_TYPE (),
               build_tree_list (NULL_TREE, type));
  EXPR_POP ();
}

static tree
m3_convert_function_to_builtin (tree p)
{
  if (DECL_NAME (p) == m3_alloca)
    p = builtin_decl_explicit (BUILT_IN_ALLOCA);
  return p;
}

#if GCC42

static void
m3_call_direct (tree p, tree return_type)
{
  p = m3_convert_function_to_builtin (p);
  tree call = m3_build3 (CALL_EXPR, return_type, proc_addr (p), CALL_TOP_ARG (),
                         CALL_TOP_STATIC_CHAIN ());
  if (return_type == t_void)
  {
    add_stmt (call);
  }
  else
  {
    TREE_SIDE_EFFECTS (call) = true; /* needed? */
    EXPR_PUSH (call);
  }
  CALL_POP ();
}

static void
m3_call_indirect (tree return_type, tree /*calling_convention*/)
{
  tree argtypes = chainon (CALL_TOP_TYPE (),
                           tree_cons (NULL_TREE, t_void, NULL_TREE));
  tree fntype = m3_build_pointer_type (build_function_type (return_type, argtypes));
  tree fnaddr = EXPR_REF (-1);

  EXPR_POP ();

  tree call = build3 (CALL_EXPR, return_type, m3_cast (fntype, fnaddr), CALL_TOP_ARG (),
                      CALL_TOP_STATIC_CHAIN ());
  if (VOID_TYPE_P (return_type))
  {
    add_stmt (call);
  }
  else
  {
    TREE_SIDE_EFFECTS (call) = true; /* needed? */
    EXPR_PUSH (call);
  }
  CALL_POP ();
}

#else

#if GCC46

static
tree
build_call_list (tree return_type, tree fn, tree arglist)
{
#if 1
  int n = list_length (arglist);
  tree *argarray = (tree *) alloca (n * sizeof (tree));
  for (int i = 0; i < n; i++, arglist = TREE_CHAIN (arglist))
    argarray[i] = TREE_VALUE (arglist);
  tree t = build_call_array_loc (UNKNOWN_LOCATION, return_type, fn, n, argarray);
#else
/* copied from/based on gcc 4.{5,6}/tree.c */
  tree t = build_vl_exp (CALL_EXPR, list_length (arglist) + 3);
  TREE_TYPE (t) = return_type;
  CALL_EXPR_FN (t) = fn;
  CALL_EXPR_STATIC_CHAIN (t) = NULL_TREE;
  for (int i = 0; arglist; arglist = TREE_CHAIN (arglist), i++)
    CALL_EXPR_ARG (t, i) = TREE_VALUE (arglist);
#endif
  TREE_SIDE_EFFECTS (t) = true; /* pessimistic process_call_operands */
  TREE_READONLY (t) = true;     /* pessimistic process_call_operands */
  return t;
}

static
tree
build_function_call_expr (location_t loc, tree fndecl, tree arglist)
/* copied from gcc 4.5/builtins.c */
{
  tree fntype = TREE_TYPE (fndecl);
  tree fn = build1 (ADDR_EXPR, build_pointer_type (fntype), fndecl);
  int n = list_length (arglist);
  tree *argarray = (tree *) alloca (n * sizeof (tree));
  int i;

  for (i = 0; i < n; i++, arglist = TREE_CHAIN (arglist))
    argarray[i] = TREE_VALUE (arglist);
  return fold_builtin_call_array (loc, TREE_TYPE (fntype), fn, n, argarray);
}

#endif

static void
m3_call_direct (tree p, tree return_type)
{
  p = m3_convert_function_to_builtin (p);
  if (TREE_USED (p) == false)
  {
      TREE_USED (p) = true;
      assemble_external (p);
  }
  tree call = build_call_list (return_type, proc_addr (p), CALL_TOP_ARG ());
  CALL_EXPR_STATIC_CHAIN (call) = CALL_TOP_STATIC_CHAIN ();
  if (VOID_TYPE_P (return_type))
  {
    add_stmt (call);
  }
  else
  {
    TREE_SIDE_EFFECTS (call) = true; /* needed? */
    EXPR_PUSH (call);
  }
  CALL_POP ();
  if (call_expr_flags (call) & ECF_RETURNS_TWICE)
  {
    /* call to setjmp: make locals, etc. volatile */
    m3_volatilize_current_function ();
  }
}

static void
m3_call_indirect (tree return_type, tree calling_convention)
{
  tree argtypes = chainon (CALL_TOP_TYPE (), void_list_node);
  tree fntype = m3_build_pointer_type (build_function_type (return_type, argtypes));
  tree fnaddr = EXPR_REF (-1);
  EXPR_POP ();

  decl_attributes (&fntype, calling_convention, 0);

  tree call = build_call_list (return_type, m3_cast (fntype, fnaddr), CALL_TOP_ARG ());
  CALL_EXPR_STATIC_CHAIN (call) = CALL_TOP_STATIC_CHAIN ();
  if (VOID_TYPE_P (return_type))
  {
    add_stmt (call);
  }
  else
  {
    TREE_SIDE_EFFECTS (call) = true; /* needed? */
    EXPR_PUSH (call);
  }
  CALL_POP ();
}

#endif

static struct language_function*
m3_language_function (void)
{
  /* gcc 4.2 volatize nothing
     gcc 4.6 volatize everything */
    if (GCC42)
    {
        /* gcc_unreachable (); almost */
        return 0;
    }
    if (!current_function_decl || !DECL_STRUCT_FUNCTION (current_function_decl))
      return 0;
    struct language_function* f
      = DECL_STRUCT_FUNCTION (current_function_decl)->language;
    if (!f)
    {
#if defined(GGC_CNEW)
        f = GGC_CNEW (struct language_function);
#elif defined(ggc_alloc_cleared_language_function)
        f = ggc_alloc_cleared_language_function ();
#else
#error neither GGC_CNEW nor ggc_alloc_cleared_language_function defined
#endif
        DECL_STRUCT_FUNCTION (current_function_decl)->language = f;
    }
    return f;
}

static void
m3_volatilize_decl (tree decl)
{
  if (GCC42)
    return;

  enum tree_code code = TREE_CODE (decl);
  if ((code == VAR_DECL || code == PARM_DECL)
      && !TYPE_VOLATILE (TREE_TYPE (decl))
      && !TREE_STATIC (decl))
  {
    if (option_trace_all && DECL_NAME (decl) && IDENTIFIER_POINTER (DECL_NAME (decl)))
        fprintf(stderr, "volatile:%s\n", IDENTIFIER_POINTER (DECL_NAME (decl)));
    TREE_TYPE (decl) = build_qualified_type (TREE_TYPE (decl), TYPE_QUAL_VOLATILE);
    TREE_THIS_VOLATILE (decl) = true;
    TREE_SIDE_EFFECTS (decl) = true;
    DECL_REGISTER (decl) = false;
  }
}

#if !GCC42

static void
m3_volatilize_current_function (void)
{
  /* note it for later so that later temporaries and locals ("WITH")
   * are also made volatile */
  set_volatize (true);

  /* make locals volatile */

  for (tree block = current_block;
       block != current_function_decl;
       block = BLOCK_SUPERCONTEXT (block))
  {
    for (tree decl = BLOCK_VARS (block); decl; decl = TREE_CHAIN (decl))
      m3_volatilize_decl (decl);
  }

  /* make arguments volatile  */

  for (tree decl = DECL_ARGUMENTS (current_function_decl);
       decl; decl = TREE_CHAIN (decl))
    m3_volatilize_decl (decl);
}

#endif

static void
m3_swap (void)
{
  tree tmp = EXPR_REF (-1);
  EXPR_REF (-1) = EXPR_REF (-2);
  EXPR_REF (-2) = tmp;
}

static void
mark_address_taken (tree ref)
/* from tree-ssa-operands.c */
{
  /* Note that it is *NOT OKAY* to use the target of a COMPONENT_REF
     as the only thing we take the address of.  If VAR is a structure,
     taking the address of a field means that the whole structure may
     be referenced using pointer arithmetic.  See PR 21407 and the
     ensuing mailing list discussion.  */
  tree var = get_base_address (ref);
  if (var)
    {
      if (DECL_P (var))
	TREE_ADDRESSABLE (var) = 1;
#if GCC46
      else if (TREE_CODE (var) == MEM_REF
	       && TREE_CODE (TREE_OPERAND (var, 0)) == ADDR_EXPR
	       && DECL_P (TREE_OPERAND (TREE_OPERAND (var, 0), 0)))
	TREE_ADDRESSABLE (TREE_OPERAND (TREE_OPERAND (var, 0), 0)) = 1;
#endif
    }
}

static tree
m3_deduce_field_reference (PCSTR caller, tree value, UINT64 offset,
                           tree /*field_treetype*/, m3_type /*field_m3type*/)
{
  tree record_type = { 0 };
  tree tree_type = TREE_TYPE (value);
  enum tree_code code = TREE_CODE (value);
  tree field = { 0 };

  return 0;

  if ((code == VAR_DECL || code == CONST_DECL || code == RESULT_DECL || code == PARM_DECL)
    && tree_type
    && TREE_CODE (tree_type) == POINTER_TYPE
    && ((record_type = TREE_TYPE (tree_type)))
    && TREE_CODE (record_type) == RECORD_TYPE)
  {
     /* linear search! */
     for (field = TYPE_FIELDS (record_type); field; field = TREE_CHAIN (field))
     {
       if (offset == (TREE_INT_CST_LOW (DECL_FIELD_OFFSET (field))
                      + TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (field))))
         break;
     }
     if (option_trace_all)
     {
       fprintf (stderr, "\ndeduce_field_reference %s offset:0x%X => %s\n",
                caller, (UINT)offset,
                field ? IDENTIFIER_POINTER (DECL_NAME (field)) : "?unknown");
     }
   }
   return field;
}

static bool
m3_type_match (tree t1, tree t2)
{
  gcc_assert (t1 && t2);
  return (t1 == t2 || (POINTER_TYPE_P (t1) && POINTER_TYPE_P (t2)));
}

static bool
m3_type_mismatch (tree t1, tree t2)
{
  return !m3_type_match (t1, t2);
}

static void
m3_load (tree v, UINT64 offset, tree src_t, m3_type src_T, tree dst_t, m3_type dst_T)
{
  gcc_assert ((offset % BITS_PER_UNIT) == 0);
  m3_deduce_field_reference ("m3_load", v, offset, src_t, src_T);
  if (offset || m3_type_mismatch (TREE_TYPE (v),  src_t))
  {
    if (GCC42 || IS_REAL_TYPE (src_T) || IS_REAL_TYPE (dst_T))
    {
      /* failsafe, but inefficient */
      v = m3_build1 (ADDR_EXPR, t_addr, v);
      if (offset)
        v = m3_build2 (POINTER_PLUS_EXPR, t_addr, v, size_int (offset / BITS_PER_UNIT));
      v = m3_build1 (INDIRECT_REF, src_t,
                     m3_cast (m3_build_pointer_type (src_t), v));
    }
    else
    {
      mark_address_taken (v);
      v = m3_build3 (BIT_FIELD_REF, src_t, v, TYPE_SIZE (src_t),
                     bitsize_int (offset));
    }
  }
  if (src_T != dst_T)
    v = m3_convert (dst_t, v);
  EXPR_PUSH (v);
}

static void
m3_store (tree v, UINT64 offset, tree src_t, m3_type src_T, tree dst_t, m3_type dst_T)
{
  gcc_assert ((offset % BITS_PER_UNIT) == 0);
  m3_deduce_field_reference ("m3_store", v, offset, dst_t, dst_T);
  if (offset || m3_type_mismatch (TREE_TYPE (v),  dst_t))
  {
    if (GCC42 || IS_REAL_TYPE (src_T) || IS_REAL_TYPE (dst_T))
    {
      /* failsafe, but inefficient */
      v = m3_build1 (ADDR_EXPR, t_addr, v);
      if (offset)
        v = m3_build2 (POINTER_PLUS_EXPR, t_addr, v, size_int (offset / BITS_PER_UNIT));
      v = m3_build1 (INDIRECT_REF, dst_t,
                     m3_cast (m3_build_pointer_type (dst_t), v));
    }
    else
    {
      mark_address_taken (v);
      v = m3_build3 (BIT_FIELD_REF, dst_t, v, TYPE_SIZE (dst_t),
                     bitsize_int (offset));
    }
  }
  tree val = m3_cast (src_t, EXPR_REF (-1));
  if (src_T != dst_T)
    val = m3_convert (dst_t, val);
  add_stmt (build2 (MODIFY_EXPR, dst_t, v, val));
  EXPR_POP ();
}

static void
setop (tree p, INT64 n, int q)
{
  m3_start_call ();
  EXPR_PUSH (size_int (n));
  m3_pop_param (t_word);
  while (q--)
    m3_pop_param (t_addr);
  m3_call_direct (p, TREE_TYPE (TREE_TYPE (p)));
}

static void
setop2 (tree p, int q)
{
  m3_start_call ();
  while (q--)
    m3_pop_param (t_addr);
  tree type = TREE_TYPE (TREE_TYPE (p));
  m3_call_direct (p, type);
}

/*----------------------------------------------------------------------------*/

static PCSTR mode_to_string (enum machine_mode mode)
{
  switch (mode)
  {
    default:        return "";
    case VOIDmode:  return "VOIDmode";
    case DImode:    return "DImode";
    case BLKmode:   return "BLKmode";
  }
}

/*---------------------------------------------------------------- faults ---*/

static void
declare_fault_proc (void)
{
  tree parm_block = make_node (BLOCK);
  tree top_block  = make_node (BLOCK);

  tree proc = build_decl (FUNCTION_DECL,
                          get_identifier_with_length (CONSTANT_STRING_AND_LENGTH ("_m3_fault")),
                          build_function_type_list (t_void, t_word, NULL_TREE));

  tree resultdecl = build_decl (RESULT_DECL, NULL_TREE, t_void);
  DECL_CONTEXT (resultdecl) = proc;
  DECL_ARTIFICIAL (resultdecl) = true;
  DECL_IGNORED_P (resultdecl) = true;
  DECL_RESULT (proc) = resultdecl;
#if !GCC42
  DECL_SOURCE_LOCATION (proc) = BUILTINS_LOCATION;
#endif

  /* TREE_THIS_VOLATILE (proc) = true; / * noreturn */
  TREE_STATIC (proc) = true;
  TREE_PUBLIC (proc) = false;
  DECL_CONTEXT (proc) = NULL;

  tree parm = build_decl (PARM_DECL, fix_name ("arg", 3, UID_INTEGER), t_word);

  if (DECL_MODE (parm) == VOIDmode)
  {
      if (option_trace_all)
        fprintf (stderr, " declare_fault_proc: converting parameter from VOIDmode to Pmode\n");
      DECL_MODE (parm) = Pmode;
  }
  DECL_ARG_TYPE (parm) = t_word;
  DECL_ARGUMENTS (proc) = parm;
  DECL_CONTEXT (parm) = proc;

  BLOCK_SUPERCONTEXT (parm_block) = proc;
  DECL_INITIAL (proc) = parm_block;

  BLOCK_SUPERCONTEXT (top_block) = parm_block;
  BLOCK_SUBBLOCKS (parm_block) = top_block;

  if (option_trace_all)
  {
    enum machine_mode mode = TYPE_MODE (TREE_TYPE (parm));
    fprintf (stderr, " declare_fault_proc: type is 0x%X (%s)", (UINT)mode, mode_to_string (mode));
  }

  fault_proc = proc;
}

static void
m3_gimplify_function (tree fndecl)
{
  dump_function (TDI_original, fndecl);
  gimplify_function_tree (fndecl);
  dump_function (TDI_generic, fndecl);

  /* Convert all nested functions to GIMPLE now.  We do things in this order
     so that items like VLA sizes are expanded properly in the context of the
     correct function.  */
  struct cgraph_node* node = cgraph_get_create_node (fndecl);
  cgraph_mark_needed_node (node);    /* keep all functions */
  for (node = node->nested; node; node = node->next_nested)
  {
    /* don't inline nested functions of unlinable parents; bug?
       e.g. if parent has a barrier label (exception handling) */
    DECL_UNINLINABLE (node->decl) |= DECL_UNINLINABLE (fndecl);
    m3_gimplify_function (node->decl);
  }
}

static void
emit_fault_proc (void)
{
  location_t save_loc = input_location;
  tree p = fault_proc;

#ifdef M3_USE_MAPPED_LOCATION
  input_location = BUILTINS_LOCATION;
#else
  input_line = 0;
#endif

  gcc_assert (current_function_decl == NULL_TREE);
  gcc_assert (current_block == NULL_TREE);
  current_function_decl = p;
  allocate_struct_function (p, false);

  pending_blocks = tree_cons (NULL_TREE, current_block, pending_blocks);
  current_block = DECL_INITIAL (p); /* parm_block */
  TREE_USED (current_block) = true;
  current_block = BLOCK_SUBBLOCKS (current_block); /* top_block */
  TREE_USED (current_block) = true;

  pending_stmts = tree_cons (NULL_TREE, current_stmts, pending_stmts);
  current_stmts = alloc_stmt_list ();

  m3_start_call ();
  EXPR_PUSH (m3_build1 (ADDR_EXPR, t_addr, current_segment));
  m3_pop_param (t_addr);
  EXPR_PUSH (DECL_ARGUMENTS (p));
  m3_pop_param (t_word);
  gcc_assert (fault_handler != NULL_TREE);
  m3_call_direct (fault_handler, t_void);
  add_stmt (build1 (RETURN_EXPR, t_void, NULL_TREE));

  /* Attach block to the function */
  gcc_assert (current_block == BLOCK_SUBBLOCKS (DECL_INITIAL (p)));
  DECL_SAVED_TREE (p) = build3 (BIND_EXPR, t_void,
                                BLOCK_VARS (current_block),
                                current_stmts, current_block);
  TREE_SIDE_EFFECTS (DECL_SAVED_TREE (p)) = true; /* needed? */
  current_block = TREE_VALUE (pending_blocks);
  pending_blocks = TREE_CHAIN (pending_blocks);
  current_stmts = TREE_VALUE (pending_stmts);
  pending_stmts = TREE_CHAIN (pending_stmts);

  /* good line numbers for epilog */
  DECL_STRUCT_FUNCTION (p)->function_end_locus = input_location;

  input_location = save_loc;

  m3_gimplify_function (p);
  cgraph_finalize_function (p, false);
  current_function_decl = NULL_TREE;
}

/* see M3CG.RuntimeError, RuntimeError.T */
#define FAULT_MASK 0x1f
#define LINE_SHIFT 5

static tree
generate_fault (int code ATTRIBUTE_UNUSED)
{
  tree t = { 0 };
  /* Losing bits of the code seems bad: wrong error reported.
   * Losing bits of the line number is "ok".
   * Line numbers up to around 100 million are preserved.
   */
  gcc_assert (code <= FAULT_MASK);
  /* gcc_assert (LOCATION_LINE (input_location) <= (long)((~0UL) >> LINE_SHIFT)); */
  if (fault_proc == NULL)
    declare_fault_proc ();
  tree arg = build_int_cst (t_int, (LOCATION_LINE (input_location) << LINE_SHIFT) + (code & FAULT_MASK));
#if GCC45
  t = build_function_call_expr (input_location, fault_proc, build_tree_list (NULL_TREE, arg));
#else
  t = build_function_call_expr (fault_proc, build_tree_list (NULL_TREE, arg));
#endif
  TREE_SIDE_EFFECTS (t) = true; /* needed? */
  return t;
}

/*-------------------------------------------------- M3CG opcode handlers ---*/

#define M3CG_HANDLER(code) void m3cg_##code##_t::handler ()

M3CG_HANDLER (BEGIN_UNIT)
{
  exported_interfaces = 0;
}

M3CG_HANDLER (END_UNIT)
{
  gcc_assert (current_block == NULL_TREE);
  debug_tag ('i', NO_UID, "_%.*s", sizet_to_int (current_unit_name_length), current_unit_name);
  for (long j = 0; j < exported_interfaces; ++j)
    debug_field_name (exported_interfaces_names [j]);
  debug_struct ();
  if (fault_proc != NULL_TREE)
    emit_fault_proc ();
}

M3CG_HANDLER (IMPORT_UNIT)
{
  /* ignore */
}

M3CG_HANDLER (EXPORT_UNIT)
{
  name = IDENTIFIER_POINTER (get_identifier_with_length (name, name_length));
  if (exported_interfaces == COUNT_OF (exported_interfaces_names))
    fatal_error ("internal limit exporting more than 100 interfaces");
  /* remember the set of exported interfaces */
  exported_interfaces_names [exported_interfaces++] = name;
}

M3CG_HANDLER (SET_SOURCE_FILE)
{
  const char* xname = IDENTIFIER_POINTER (get_identifier_with_length (name, name_length));

#ifdef M3_USE_MAPPED_LOCATION
  linemap_add (line_table, LC_RENAME, false, xname, 1);
  input_location = linemap_line_start (line_table, 1, 80);
#else
  input_filename = xname;
#endif
}

M3CG_HANDLER (SET_SOURCE_LINE)
{
#ifdef M3_USE_MAPPED_LOCATION
  input_location = linemap_line_start (line_table, i, 80);
#else
  input_line = i;
#endif
}

M3CG_HANDLER (DECLARE_TYPENAME)
{
  size_t fullname_length =
    sizet_add (current_unit_name_length, sizet_add (long_to_sizet (name_length), 1));
  PSTR fullname = (PSTR)alloca (fullname_length);
  gcc_assert (name_length > 0);
  memcpy(fullname, current_unit_name, current_unit_name_length);
  fullname[current_unit_name_length] = '.';
  memcpy(&fullname[current_unit_name_length + 1], name, name_length);

  debug_tag ('N', my_id, "");
  debug_field_name_length (fullname, fullname_length);
  debug_struct ();

  debug_tag ('n', NO_UID, "_%.*s", sizet_to_int (fullname_length), fullname);
  debug_field_id (my_id);
  debug_struct ();
}

M3CG_HANDLER (DECLARE_ARRAY)
{
  debug_tag ('A', my_id, "_"WIDE_PRINT_DEC, (WIDE)size);
  debug_field_id (index_id);
  debug_field_id (elts_id);
  debug_struct ();

  if (M3_TYPES)
  {
    /* These will fail if we turn off some of the type code, e.g. M3_TYPES_ENUM. */
    /* gcc_assert (get_typeid_to_tree (index_id)); */
    /* gcc_assert (get_typeid_to_tree (elts_id)); */
    /* This is wrong and could be much better. */
    set_typeid_to_tree (my_id, m3_build_type_id (T_struct, size, 1, NO_UID));
  }
}

M3CG_HANDLER (DECLARE_OPEN_ARRAY)
/*
  An open array is a record consisting of a pointer
  an an array of integers; the array's size is equal
  to the depth of the array; we can deduce the depth
  from the overall size given here.
  
  Typically, for a one dimensional array, size === 2 * BITSIZE(INTEGER).
  Two dimentions: size == 3 * BITSIZE(INTEGER).
  and so on.
*/
{
  if (M3_TYPES)
  {
    /*tree record = make_node (RECORD_TYPE);*/
    /*tree array = make_node (ARRAY_TYPE);*/
    if (false/*M3_TYPES_REPLAY*/ && get_typeid_to_tree (elts_id) == NULL)
    {
      if (option_trace_all)
        fprintf (stderr, "\n declare_open_array: missing type 0x%X\n",
                 (UINT)elts_id);
      /*m3_replay = M3_TYPES_REPLAY;*/
      /* This is wrong and could be much better.
       * TODO: use a useful stub here, i.e. one that
       * will recieve further fixup
       */
      set_typeid_to_tree (my_id, m3_build_type_id (T_struct, size, 1, NO_UID));
      /*if (m3_replay)
        return;*/
    }
    else
    {
      /*gcc_assert (get_typeid_to_tree (elts_id));*/
      set_typeid_to_tree (my_id, m3_build_type_id (T_struct, size, 1, NO_UID));
    }
  }

  debug_tag ('B', my_id, "_"WIDE_PRINT_DEC, (WIDE)size);
  debug_field_id (elts_id);
  debug_struct ();
}

M3CG_HANDLER (DECLARE_ENUM)
/* see start_enum, build_enumerator, finish_enum */
{
  gcc_assert (size == 8 || size == 16 || size == 32 || size == 64);
  gcc_assert (size <= BITS_PER_INTEGER || n_elts == 0);

  debug_tag ('C', my_id, "_"WIDE_PRINT_DEC, (WIDE)size);
  current_dbg_type_count1 = n_elts;
  current_dbg_type_count2 = n_elts;

  if (M3_TYPES_ENUM)
  {
    UINT bits = (n_elts == 0) ? 64
              : (n_elts <= (((UINT64)1) << 8)) ? 8
              : (n_elts <= (((UINT64)1) << 16)) ? 16
              : (n_elts <= (((UINT64)1) << 32)) ? 32
              : 64;
    if (size != bits)
    {
      fprintf(stderr, "BITS_PER_INTEGER: 0x%X\n", (UINT)BITS_PER_INTEGER);
      fprintf(stderr, "m3cg_declare_enum: size 0x%X vs. bits 0x%X vs. elts 0x%X\n",
              (UINT)size, bits, (UINT)n_elts);
    }
    gcc_assert (size == bits);
    enumtype = make_node (ENUMERAL_TYPE);
    TYPE_USER_ALIGN (enumtype) = true;
    TYPE_UNSIGNED (enumtype) = true;
    TYPE_MIN_VALUE (enumtype) = integer_zero_node;
    enumtype_elementtype = m3_build_type_id (T_word, bits, bits, my_id);
    TYPE_MAX_VALUE (enumtype) = build_int_cstu (enumtype_elementtype, n_elts - 1);
    SET_TYPE_MODE (enumtype, TYPE_MODE (enumtype_elementtype));
    TYPE_SIZE (enumtype) = bitsize_int (bits);
    TYPE_SIZE_UNIT (enumtype) = size_int (bits / BITS_PER_UNIT);
    TYPE_PRECISION (enumtype) = bits;
    TYPE_ALIGN (enumtype) = bits;
    TYPE_PACKED (enumtype) = true;
    TYPE_STUB_DECL (enumtype) = m3_push_type_decl (enumtype, 0);
    TYPE_MAIN_VARIANT (enumtype) = enumtype;
    set_typeid_to_tree (my_id, enumtype);
  }
}

M3CG_HANDLER (DECLARE_ENUM_ELT)
/* see build_enumerator, finish_enum */
{
  gcc_assert (current_dbg_type_count1 > 0);

  if (M3_TYPES_ENUM)
  {
    tree decl = build_decl (CONST_DECL, get_identifier_with_length (name, name_length), enumtype_elementtype);
    tree value = build_int_cstu (t_word, current_dbg_type_count2 - current_dbg_type_count1);
    DECL_SOURCE_LOCATION (decl) = input_location;
    DECL_CONTEXT (decl) = m3_current_scope ();
    gcc_assert (current_dbg_type_count2 > 0);
    gcc_assert (current_dbg_type_count2 >= current_dbg_type_count1);
    DECL_INITIAL (decl) = m3_convert (enumtype_elementtype, value);
    pushdecl (decl);
    decl = tree_cons (decl, value, NULL_TREE);
    TREE_CHAIN (decl) = TYPE_VALUES (enumtype);
    TYPE_VALUES (enumtype) = decl;
  }

  debug_field_name_length (name, name_length);

  if (--current_dbg_type_count1 == 0)
  {
    debug_struct (); /* m3gdb */

    if (M3_TYPES_ENUM)
    {
      for (tree pair = TYPE_VALUES (enumtype); pair; pair = TREE_CHAIN (pair))
      {
        tree enu = TREE_PURPOSE (pair);
        tree ini = DECL_INITIAL (enu);

        TREE_TYPE (enu) = enumtype;
        DECL_INITIAL (enu) = ini;
        TREE_PURPOSE (pair) = DECL_NAME (enu);
        TREE_VALUE (pair) = ini;
      }
      TYPE_VALUES (enumtype) = nreverse (TYPE_VALUES (enumtype));
      layout_type (enumtype);
      /* rest_of_type_compilation (enumtype, true); */
      current_dbg_type_count2 = 0; /* done */
      enumtype = 0; /* done */
      enumtype_elementtype = 0; /* done */
    }
  }
}

M3CG_HANDLER (DECLARE_PACKED)
{
  debug_field_id (target_id);
  debug_tag ('D', my_id, "_"WIDE_PRINT_DEC, (WIDE)size);
  debug_struct ();

#if 1
  /* Could be better. */
  set_typeid_to_tree (my_id, m3_build_type_id (T_struct, size, 1, NO_UID));
#else
  set_typeid_to_tree (my_id, get_typeid_to_tree (target_id));
#endif
}

static void
m3_declare_record_common (void)
{
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0)
  {
    tree type = current_record_type;
    debug_struct ();

    m3_gap (current_record_size);
    if (TYPE_FIELDS (type))
      TYPE_FIELDS (type) = nreverse (TYPE_FIELDS (type));
    layout_type (type);
    /* rest_of_type_compilation (type, true); */
    dump_record_type (type);
    if (current_record_type_id != NO_UID)
    {
      UINT64 a = TREE_INT_CST_LOW (TYPE_SIZE (type));
      UINT64 b = current_record_size;
      set_typeid_to_tree (current_record_type_id, type);
      if (M3_TYPES_CHECK_RECORD_SIZE && a != b)
      {
        fprintf (stderr, "m3_declare_record_common backend:0x%X vs. frontend:0x%X\n",
                 (UINT)a, (UINT)b);
        gcc_assert (a == b);
      }
    }
    else if (current_object_type_id != NO_UID)
    {
      type = m3_build_pointer_type (type);
      set_typeid_to_tree (current_object_type_id, type);
    }
    current_record_type_id = NO_UID;
    current_object_type_id = NO_UID;
    current_record_size = 0;
  }
}

M3CG_HANDLER (DECLARE_RECORD)
{
  debug_tag ('R', my_id, "_"WIDE_PRINT_DEC, (WIDE)size);
  current_dbg_type_count1 = n_fields;
  current_dbg_type_count2 = 0;

  gcc_assert (current_record_type_id == NO_UID);
  gcc_assert (current_object_type_id == NO_UID);
  current_record_size = size;
  current_record_type_id = my_id;
  current_record_offset = 0;
  current_record_vals = NULL_TREE;
  current_record_type = make_node (RECORD_TYPE);

  m3_declare_record_common ();
}

M3CG_HANDLER (DECLARE_FIELD)
{
  tree f = { 0 };
  tree v = { 0 };

  tree t = get_typeid_to_tree (my_id);
  if (M3_TYPES_REQUIRE_ALL_FIELD_TYPES && t == NULL) /* This is frequently NULL. Why? */
  {
    fprintf (stderr,
             "\ndeclare_field: type_id 0x%X to type is null for field %.*s\n",
             (UINT)my_id, long_to_printf_length (name_length), name);
    if (M3_TYPES_REQUIRE_ALL_FIELD_TYPES == 1)
      t = t_addr;
    gcc_assert (t);
  }
  else
  {
    t = t ? t : m3_build_type_id (T_struct, size, size, NO_UID);
  }
  debug_field_fmt (my_id,
                   "_"WIDE_PRINT_DEC"_"WIDE_PRINT_DEC"_%.*s",
                   (WIDE)offset,
                   (WIDE)size,
                   long_to_printf_length (name_length),
                   name);
  current_dbg_type_count1--;

  m3_field (name, name_length, t, offset, size, &f, &v);

  m3_declare_record_common ();
}

M3CG_HANDLER (DECLARE_SET)
{
  if (option_trace_all)
    fprintf (stderr, " declare_set my_id:0x%X domain_id:0x%X size:0x%X",
             (UINT)my_id, (UINT)domain_id, (UINT)size);

  debug_tag ('S', my_id, "_"WIDE_PRINT_DEC, (WIDE)size);
  debug_field_id (domain_id);
  debug_struct ();

  /* Could be better. */
  set_typeid_to_tree (my_id, m3_build_type_id (T_struct, size, 1, NO_UID));
}

M3CG_HANDLER (DECLARE_SUBRANGE)
{
  char buff [256]; /* plenty */
  PSTR p = buff;
  PSTR p_limit = p + sizeof(buff);

  /* You might think so, but no.
  see cm3/m3-ui/X11R4/src/Common/X.i3
  gcc_assert (min <= max);
  */

  m3_append_char ('_', &p, p_limit);
  m3_append_char ('%', &p, p_limit);
  m3_append_char ('d', &p, p_limit);
  m3_append_char ('_', &p, p_limit);
  m3_fill_hex_value (min, &p, p_limit);
  m3_append_char ('_', &p, p_limit);
  m3_fill_hex_value (max, &p, p_limit);
  m3_append_char ('\0', &p, p_limit);
  debug_tag ('Z', my_id, buff, size);

  debug_field_id (domain_id);
  debug_struct ();

  if (M3_TYPES)
  {
    tree super_type = m3_type_for_size (size, min < 0);
    if (!super_type)
    {
      fprintf (stderr, "no type for size:0x%X min:0x%X max:0x%X\n",
               (UINT)size, (UINT)min, (UINT)max);
      gcc_assert (super_type);
    }
    if (M3_TYPES_SUBRANGE_NEW)
    {
      tree type = make_node (INTEGER_TYPE);
      TYPE_MIN_VALUE (type) = build_int_cst (super_type, min);
      TYPE_MAX_VALUE (type) = build_int_cst (super_type, max);
      TREE_TYPE (type) = super_type;
      set_typeid_to_tree (my_id, type);
    }
    else
    {
      set_typeid_to_tree (my_id, super_type);
    }
  }
}

static void
m3_declare_pointer_common (PCSTR caller, UINT32 my_id, UINT32 target_id)
{
  if (M3_TYPES)
  {
    tree t = get_typeid_to_tree (target_id);
    if (!t)
    {
      if (option_trace_all)
        fprintf (stderr, "\n %s: missing type 0x%X\n", caller, (UINT)target_id);
      t = t_addr; /* fallback for now */
      /*m3_replay = M3_TYPES_REPLAY;*/
    }
    else
    {
      t = m3_build_pointer_type (t);
    }
    set_typeid_to_tree (my_id, t);
  }
}

M3CG_HANDLER (DECLARE_POINTER)
{
  m3_declare_pointer_common ("declare_pointer", my_id, target_id);
  /*if (m3_replay)
    return;*/

  debug_tag ('Y', my_id, "_%d_%d_%d_%.*s", GET_MODE_BITSIZE (Pmode),
             traced, (brand ? 1 : 0), (brand ? brand_length : 0),
             (brand ? brand : ""));
  debug_field_id (target_id);
  debug_struct ();
}

M3CG_HANDLER (DECLARE_INDIRECT)
{
  m3_declare_pointer_common ("declare_indirect", my_id, target_id);
  /*if (m3_replay)
    return;*/

  debug_tag ('X', my_id, "_%d", GET_MODE_BITSIZE (Pmode));
  debug_field_id (target_id);
  debug_struct ();
}

M3CG_HANDLER (DECLARE_PROCTYPE)
{
  set_typeid_to_tree (my_id, t_addr);
  debug_tag ('P', my_id, "_%d_%c"WIDE_PRINT_DEC, GET_MODE_BITSIZE (Pmode),
             n_raises < 0 ? 'A' : 'L', (WIDE)MAX (n_raises, 0));
  current_dbg_type_count1 = n_formals;
  current_dbg_type_count2 = MAX (0, n_raises);
  debug_field_id (result_id);
  gcc_assert (current_proc_type_id == NO_UID);
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0)
    debug_struct ();
  else
    current_proc_type_id = my_id;
}

M3CG_HANDLER (DECLARE_FORMAL)
{
  debug_field_fmt (my_id, "_%.*s", long_to_printf_length (name_length), name);
  current_dbg_type_count1--;
  gcc_assert (current_proc_type_id != NO_UID);
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0)
  {
    debug_struct ();
    current_proc_type_id = NO_UID;
  }
}

M3CG_HANDLER (DECLARE_RAISES)
{
  debug_field_name_length (name, name_length);
  current_dbg_type_count2--;
  gcc_assert (current_proc_type_id != NO_UID);
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0)
  {
    debug_struct ();
    current_proc_type_id = NO_UID;
  }
}

M3CG_HANDLER (DECLARE_OBJECT)
{
  gcc_assert (brand_length >= -1);

  debug_tag ('O', my_id, "_%d_"WIDE_PRINT_DEC"_%d_%d_%.*s",
             POINTER_SIZE, n_fields, traced, (brand ? 1:0),
             (brand ? brand_length : 0), (brand ? brand : ""));
  debug_field_id (super_id);
  current_dbg_type_count1 = n_fields;
  current_dbg_type_count2 = n_methods;
  current_dbg_type_count3 = 0;

  gcc_assert (current_record_type_id == NO_UID);
  gcc_assert (current_object_type_id == NO_UID);
  current_record_size = field_size;
  current_object_type_id = my_id;
  current_record_offset = 0;
  current_record_vals = NULL_TREE;
  current_record_type = make_node (RECORD_TYPE);

  m3_declare_record_common ();
}

M3CG_HANDLER (DECLARE_METHOD)
{
  debug_field_fmt (my_id, "_%d_%d_%.*s",
                   current_dbg_type_count3++ * GET_MODE_BITSIZE (Pmode),
                   GET_MODE_BITSIZE (Pmode),
                   long_to_printf_length (name_length),
                   name);
  current_dbg_type_count2--;

  gcc_assert (current_record_type_id == NO_UID);
  gcc_assert (current_object_type_id != NO_UID);

  m3_declare_record_common ();
}

M3CG_HANDLER (DECLARE_OPAQUE)
{
  /* Opaque types are always pointers.
     It would be great if we could provide more type information here. */

#if 0
  {
    tree t = get_typeid_to_tree (my_id);
    tree tsuper = get_typeid_to_tree (super_id);
    if (tsuper)
      set_typeid_to_tree_replace (my_id, tsuper, true);
    else if (!t)
      set_typeid_to_tree (my_id, t_addr);
  }
#else
  set_typeid_to_tree(my_id, t_addr);
#endif

  /* we don't pass this info to the debugger, only the revelation is interesting */
}

M3CG_HANDLER (REVEAL_OPAQUE)
{
  tree tl = get_typeid_to_tree (lhs);
  tree tr = get_typeid_to_tree (rhs);

  debug_tag ('Q', lhs, "_%d", (int)GET_MODE_BITSIZE (Pmode));
  debug_field_id (rhs);
  debug_struct ();

  if (tr)
    set_typeid_to_tree_replace (lhs, tr, true);
  else if (!tl)
    set_typeid_to_tree (lhs, t_addr);
}

M3CG_HANDLER (DECLARE_EXCEPTION)
{
  /* nothing yet */
}

static const char ReportFault[] = "ReportFault";

M3CG_HANDLER (SET_RUNTIME_PROC)
{
  if (name_length == (sizeof(ReportFault) - 1) && memcmp (name, ReportFault, sizeof(ReportFault) - 1) == 0)
    fault_handler = p;
}

M3CG_HANDLER (IMPORT_GLOBAL)
{
  DECL_NAME (var) = fix_name (name, name_length, type_id);

  if (option_trace_all)
    fprintf (stderr, " m3name:%s ", m3_get_var_trace_name (var));

  gcc_assert (align >= !!size);

  DECL_EXTERNAL (var) = true;
  TREE_PUBLIC   (var) = true;
  TREE_TYPE (var) = m3_build_type_id (type, size, align, type_id);
  layout_decl (var, align);

  TREE_CHAIN (var) = global_decls;
  global_decls = var;
}

M3CG_HANDLER (DECLARE_SEGMENT)
{
  DECL_NAME (var) = fix_name (name, name_length, type_id);

  if (option_trace_all)
    fprintf (stderr, " m3name:%s ", m3_get_var_trace_name (var));

   m3cg_BIND_SEGMENT_t* bind_segment = bind_segments[var_integer];

  TREE_TYPE (var) = m3_build_type_id (T_struct,
                                      bind_segment->size,
                                      bind_segment->align,
                                      type_id);
  layout_decl (var, BIGGEST_ALIGNMENT);
  DECL_COMMON (var) = (bind_segment->initialized == false);
  TREE_PUBLIC (var) = bind_segment->exported;
  TREE_STATIC (var) = true;
  TREE_READONLY (var) = is_const;
  TREE_ADDRESSABLE (var) = true;
  /*DECL_DEFER_OUTPUT (var) = true; ? todo/revisit? */
  current_segment = var;

  TREE_CHAIN (var) = global_decls;
  global_decls = var;

  if (name_length > 2)
  {
    gcc_assert (name && name[0] != '_' && (name[1] == '_' || name[2] == '_'));
    current_unit_name_length = name_length - 2;
    current_unit_name = name + 2;
    while (current_unit_name[0] == '_')
    {
      current_unit_name_length -= 1;
      current_unit_name += 1;
    }
  }
}

M3CG_HANDLER (BIND_SEGMENT)
{
  gcc_assert (align >= !!size);
  current_segment = var;
}

M3CG_HANDLER (DECLARE_GLOBAL)
{
  DECL_NAME (var) = fix_name (name, name_length, type_id);

  if (option_trace_all)
    fprintf (stderr, " m3name:%s ", m3_get_var_trace_name (var));

  gcc_assert (align >= !!size);

  TREE_TYPE (var) = m3_build_type_id (type, size, align, type_id);
  DECL_COMMON (var) = (initialized == false);
  TREE_PUBLIC (var) = exported;
  TREE_STATIC (var) = true;
  layout_decl (var, align);

  TREE_CHAIN (var) = global_decls;
  global_decls = var;
}

M3CG_HANDLER (DECLARE_CONSTANT)
{
  DECL_NAME (var) = fix_name (name, name_length, type_id);

  if (option_trace_all)
    fprintf (stderr, " m3name:%s ", m3_get_var_trace_name (var));

  gcc_assert (align >= !!size);

  TREE_TYPE (var) = m3_build_type_id (type, size, align, type_id);
  DECL_COMMON (var) = (initialized == false);
  TREE_PUBLIC (var) = exported;
  TREE_STATIC (var) = true;
  TREE_READONLY (var) = true;
  layout_decl (var, align);

  TREE_CHAIN (var) = global_decls;
  global_decls = var;
}

M3CG_HANDLER (DECLARE_LOCAL)
{
  DECL_NAME (var) = fix_name (name, name_length, type_id);

  if (option_trace_all)
    fprintf (stderr, " m3name:%s ", m3_get_var_trace_name (var));

  gcc_assert (align >= !!size);

  TREE_TYPE (var) = m3_build_type_id (type, size, align, type_id);
  DECL_NONLOCAL (var) = up_level || in_memory;
  TREE_ADDRESSABLE (var) = in_memory;
  DECL_CONTEXT (var) = current_function_decl;
  layout_decl (var, align);

  if (current_block)
    {
      add_stmt (build1 (DECL_EXPR, t_void, var));
      TREE_CHAIN (var) = BLOCK_VARS (current_block);
      BLOCK_VARS (current_block) = var;
    }
  else
    {
      tree subblocks = BLOCK_SUBBLOCKS (DECL_INITIAL (current_function_decl));
      TREE_CHAIN (var) = BLOCK_VARS (subblocks);
      BLOCK_VARS (subblocks) = var;
    }
}

static int current_param_count; /* <0 => import_procedure, >0 => declare_procedure */

M3CG_HANDLER (DECLARE_PARAM)
{
  tree p = current_function_decl;

  DECL_NAME (var) = fix_name (name, name_length, type_id);

  if (option_trace_all)
    fprintf (stderr, " m3name:%s ", m3_get_var_trace_name (var));

  if (current_param_count > 0)
  {
    /* declare_procedure... */
    current_param_count -= 1;
  }
  else if (current_param_count < 0)
  {
    /* import_procedure... */
    current_param_count += 1;
    if (current_param_count == 0)
    {
      /* pop current_function_decl and reset DECL_CONTEXT=0 */
      current_function_decl = DECL_CONTEXT (p);
      DECL_CONTEXT (p) = NULL_TREE; /* imports are in global scope */
    }
  }
  else
  {
    gcc_unreachable ();
  }

  gcc_assert (align >= !!size);

  TREE_TYPE (var) = m3_build_type_id (type, size, align, type_id);
  DECL_NONLOCAL (var) = up_level || in_memory;
  TREE_ADDRESSABLE (var) = in_memory;
  DECL_ARG_TYPE (var) = TREE_TYPE (var);
  DECL_CONTEXT (var) = p;
  layout_decl (var, align);

  if (option_trace_all)
  {
    enum machine_mode mode = TYPE_MODE (TREE_TYPE (var));
    fprintf (stderr, " mode 0x%X (%s)", (UINT)mode, mode_to_string (mode));
  }

  if (DECL_MODE (var) == VOIDmode)
  {
      if (option_trace_all)
        fprintf (stderr, "\n  converting from VOIDmode to Pmode\n  ");
      DECL_MODE (var) = Pmode;
  }

  TREE_CHAIN (var) = DECL_ARGUMENTS (p);
  DECL_ARGUMENTS (p) = var;

  if (current_param_count == 0)
  {
    /* arguments were accumulated in reverse, build type, then unreverse */
    tree args = void_list_node;
    for (tree parm = DECL_ARGUMENTS (p); parm; parm = TREE_CHAIN (parm))
      args = tree_cons (NULL_TREE, TREE_TYPE (parm), args);
    args = build_function_type (TREE_TYPE (TREE_TYPE (p)), args);
    decl_attributes (&args, TYPE_ATTRIBUTES (TREE_TYPE (p)), 0);
    TREE_TYPE (p) = args;
    DECL_ARGUMENTS (p) = nreverse (DECL_ARGUMENTS (p));
    m3_outdent ();
  }
}

M3CG_HANDLER (DECLARE_TEMP)
{
  gcc_assert (align >= !!size);

  if (type == T_void)
    type = T_struct;

  TREE_TYPE (var) = m3_build_type (type, size, align);
  layout_decl (var, 1);
  DECL_UNSIGNED (var) = TYPE_UNSIGNED (TREE_TYPE (var));
  TREE_ADDRESSABLE (var) = in_memory;
  DECL_CONTEXT (var) = current_function_decl;
  if (get_volatize ())
    m3_volatilize_decl (var);

  TREE_CHAIN (var) = BLOCK_VARS (BLOCK_SUBBLOCKS
                                 (DECL_INITIAL (current_function_decl)));
  BLOCK_VARS (BLOCK_SUBBLOCKS (DECL_INITIAL (current_function_decl))) = var;

  add_stmt (build1 (DECL_EXPR, t_void, var));
}

M3CG_HANDLER (FREE_TEMP)
{
  /* nothing to do */
}

M3CG_HANDLER (BEGIN_INIT)
{
  current_record_offset = 0;
  current_record_vals = NULL_TREE;
  current_record_type = make_node (RECORD_TYPE);
  TREE_ASM_WRITTEN (current_record_type) = true;
}

M3CG_HANDLER (END_INIT)
{
  if (DECL_SIZE (var))
    one_gap (TREE_INT_CST_LOW (DECL_SIZE (var)));

  TYPE_FIELDS (current_record_type) =
    nreverse (TYPE_FIELDS (current_record_type));
  layout_type (current_record_type);

  /* remember this init so we can fix any init_offset later */
  pending_inits
    = tree_cons (NULL_TREE,
                 build_constructor_from_list (current_record_type,
                                              nreverse (current_record_vals)),
                 pending_inits);
  DECL_INITIAL (var) = TREE_VALUE (pending_inits);

  if (M3_TYPES_SEGMENT)
  {
    TREE_TYPE (var) = current_record_type;
    relayout_decl (var);
  }
}

M3CG_HANDLER (INIT_INT)
{
  tree f = { 0 };
  tree v = { 0 };

  one_field (offset, BITS_PER_INTEGER, type, &f, &v);
  TREE_VALUE (v) = build_int_cst (type, value);
}

M3CG_HANDLER (INIT_PROC)
{
  tree f = { 0 };
  tree v = { 0 };
  tree expr = proc_addr (proc);

  one_field (offset, POINTER_SIZE, TREE_TYPE (expr), &f, &v);
  TREE_VALUE (v) = expr;
}

M3CG_HANDLER (INIT_LABEL)
{
  tree f = { 0 };
  tree v = { 0 };

  one_field (offset, POINTER_SIZE, t_addr, &f, &v);
  TREE_USED (label) = true;
  TREE_VALUE (v) = build1 (ADDR_EXPR, t_addr, label);
}

M3CG_HANDLER (INIT_VAR)
{
  tree f = { 0 };
  tree v = { 0 };
  
  /* hack? */
  if (GCC46 && DECL_COMMON (var))
  {
    if (option_trace_all)
      fprintf (stderr, " clearing DECL_COMMON on %s", m3_get_var_trace_name (var));
    DECL_COMMON (var) = false;
  }

  TREE_USED (var) = true;
  one_field (offset, POINTER_SIZE, t_addr, &f, &v);
  TREE_VALUE (v) = m3_build2 (POINTER_PLUS_EXPR, t_addr,
                              m3_build1 (ADDR_EXPR, t_addr, var),
                              size_int (b));
}

M3CG_HANDLER (INIT_OFFSET)
{
  tree f = { 0 };
  tree v = { 0 };

  TREE_USED (var) = true;
  /* M3 hack to preserve TREE_ADDRESSABLE: see tree-ssa.c, tree-ssa-alias.c */
  TREE_THIS_VOLATILE (var) = true;
  one_field (offset, POINTER_SIZE, t_int, &f, &v);
  DECL_LANG_SPECIFIC (f) = (lang_decl_t*)var; /* we will fix the offset later once we have rtl */
  TREE_VALUE (v) = v_zero;
}

M3CG_HANDLER (INIT_CHARS)
{
  tree f = { 0 };
  tree v = { 0 };

  tree type = build_array_type (char_type_node,
                                build_index_type (size_int (length - 1)));
  one_field (offset, length * 8, type, &f, &v);
  TREE_VALUE (v) = build_string (length, s);
  TREE_TYPE (TREE_VALUE (v)) = TREE_TYPE (f);
}

M3CG_HANDLER (INIT_FLOAT)
{
  tree f = { 0 };
  tree v = { 0 };
  tree type = { 0 };

  switch (fkind)
  {
  case 0: type = t_reel;   break;
  case 1: type = t_lreel;  break;
  case 2: type = t_xreel;  break;
  default: fatal_error ("unknown float kind");
  }

  one_field (offset, 32 * (fkind ? 2 : 1), type, &f, &v);
  TREE_TYPE (f) = type;
  TREE_VALUE (v) = val;
}

M3CG_HANDLER (IMPORT_PROCEDURE)
{
  DECL_NAME (p) = get_identifier_with_length (name, name_length);
  TREE_TYPE (p) = build_function_type (return_type, NULL_TREE);
  TREE_PUBLIC (p) = true;
  DECL_EXTERNAL (p) = true;
  DECL_CONTEXT (p) = NULL_TREE;
  DECL_MODE (p) = FUNCTION_MODE;

  decl_attributes (&TREE_TYPE (p), calling_convention, 0);

  TREE_CHAIN (p) = global_decls;
  global_decls = p;

  current_param_count = -n_params;
  if (current_param_count)
  {
    /* stack current_function_decl while we get the params */
    DECL_CONTEXT (p) = current_function_decl;
    current_function_decl = p;
  }
  else
  {
    m3_outdent ();
  }
}

M3CG_HANDLER (DECLARE_PROCEDURE)
{
  tree parm_block = make_node (BLOCK);
  tree top_block  = make_node (BLOCK);

  /* Nested functions are never exported. */
  gcc_assert (lev == 0 || !exported);

  DECL_NAME (p) = get_identifier_with_length (name, name_length);
  TREE_STATIC (p) = true;

  TREE_PUBLIC (p) = exported;
  if (exported)
  {
   /* We really want to use VISIBILITY_PROTECTED here but we can't for
    * multiple reasons. It doesn't work on Darwin.
    * Even on Linux, we still reference symbols indirectly via GOT and get
    * an error. I don't know why.
    * see: http://gcc.gnu.org/bugzilla/show_bug.cgi?id=44166
    *
    * More general problem is we don't know in import_procedure
    * what we are importing from other modules within the same shared
    * object, vs. from other shared objects.
    */
    DECL_VISIBILITY (p) = VISIBILITY_DEFAULT;
  }
  else
  {
#ifdef HAVE_GAS_HIDDEN
    /* otherwise varasm.c warns:
        "visibility attribute not supported in this configuration; ignored"
    */
    DECL_VISIBILITY (p) = VISIBILITY_HIDDEN;
#endif
  }
  gcc_assert ((parent == NULL) == (lev == 0));
#if GCC45
  if (parent)
    DECL_STATIC_CHAIN (parent) = true;
#endif
  DECL_CONTEXT (p) = parent;
  TREE_TYPE (p) = build_function_type (return_type, NULL_TREE);
  DECL_MODE (p) = FUNCTION_MODE;
  tree resultdecl = build_decl (RESULT_DECL, NULL_TREE, return_type);
  DECL_CONTEXT (resultdecl) = p;
  DECL_ARTIFICIAL (resultdecl) = true;
  DECL_IGNORED_P (resultdecl) = true;
  DECL_RESULT (p) = resultdecl;

  decl_attributes (&TREE_TYPE (p), calling_convention, 0);

  BLOCK_SUPERCONTEXT (parm_block) = p;
  DECL_INITIAL (p) = parm_block;

  BLOCK_SUPERCONTEXT (top_block) = parm_block;
  BLOCK_SUBBLOCKS (parm_block) = top_block;

  gcc_assert (current_block == NULL_TREE);
  TREE_CHAIN (p) = global_decls;
  global_decls = p;

  current_function_decl = p;
  current_param_count = n_params;

  if (n_params < 1)
    m3_outdent ();
}

M3CG_HANDLER (BEGIN_PROCEDURE)
{
  DECL_SOURCE_LOCATION (p) = input_location;

  announce_function (p);
  current_function_decl = p;
  allocate_struct_function (p, false);
  m3_language_function (); /* force allocation is this needed? todo? revisit */

  pending_blocks = tree_cons (NULL_TREE, current_block, pending_blocks);
  current_block = DECL_INITIAL (p); /* parm_block */
  TREE_USED (current_block) = true;
  current_block = BLOCK_SUBBLOCKS (current_block); /* top_block */
  TREE_USED (current_block) = true;

  pending_stmts = tree_cons (NULL_TREE, current_stmts, pending_stmts);
  current_stmts = alloc_stmt_list ();

  /* compile the locals we have already seen */
  for (tree local = BLOCK_VARS (current_block); local; local = TREE_CHAIN (local))
    add_stmt (build1 (DECL_EXPR, t_void, local));
}

M3CG_HANDLER (END_PROCEDURE)
{
  /* Attach block to the function */
  gcc_assert (current_block == BLOCK_SUBBLOCKS (DECL_INITIAL (p)));
  DECL_SAVED_TREE (p) = build3 (BIND_EXPR, t_void,
                                BLOCK_VARS (current_block),
                                current_stmts, current_block);
  TREE_SIDE_EFFECTS (DECL_SAVED_TREE (p)) = true; /* needed? */
  current_block = TREE_VALUE (pending_blocks);
  pending_blocks = TREE_CHAIN (pending_blocks);
  current_stmts = TREE_VALUE (pending_stmts);
  pending_stmts = TREE_CHAIN (pending_stmts);

  /* good line numbers for epilog */
  DECL_STRUCT_FUNCTION (p)->function_end_locus = input_location;

  current_function_decl = DECL_CONTEXT (p);

  gcc_assert (!current_block == !current_function_decl);
  
  if (current_block)
  { /* Register this function with cgraph just far enough to get it
       added to our parent's nested function list.  */
    struct cgraph_node* node = cgraph_get_create_node (p);
    cgraph_mark_needed_node (node);    /* keep all functions */
  }
  else
  { /* We are not inside of any scope now. */
    m3_gimplify_function (p);
    cgraph_finalize_function (p, false);
  }
}

M3CG_HANDLER (BEGIN_BLOCK)
{
  tree b = build_block (NULL_TREE, NULL_TREE, current_block, NULL_TREE);
  BLOCK_SUBBLOCKS (current_block)
    = chainon (BLOCK_SUBBLOCKS (current_block), b);
  BLOCK_SUPERCONTEXT (b) = current_block;
  TREE_USED (b) = true;
  pending_blocks = tree_cons (NULL_TREE, current_block, pending_blocks);
  current_block = b;
  pending_stmts = tree_cons (NULL_TREE, current_stmts, pending_stmts);
  current_stmts = alloc_stmt_list ();
}

M3CG_HANDLER (END_BLOCK)
{
  tree bind = build3 (BIND_EXPR, t_void,
                      BLOCK_VARS (current_block),
                      current_stmts, current_block);
  TREE_SIDE_EFFECTS (bind) = true; /* needed? */
  current_block = TREE_VALUE (pending_blocks);
  pending_blocks = TREE_CHAIN (pending_blocks);
  current_stmts = TREE_VALUE (pending_stmts);
  pending_stmts = TREE_CHAIN (pending_stmts);
  add_stmt (bind);
}

M3CG_HANDLER (NOTE_PROCEDURE_ORIGIN)
{
  fatal_error ("note_procedure_origin psuedo-op encountered.");
}

M3CG_HANDLER (SET_LABEL)
{
  DECL_CONTEXT (label) = current_function_decl;
  DECL_MODE (label) = VOIDmode;
  DECL_SOURCE_LOCATION (label) = input_location;

  if (barrier)
    {
      rtx r = label_rtx (label);
      LABEL_PRESERVE_P (r) = true;
      FORCED_LABEL (label) = true;
      DECL_UNINLINABLE (current_function_decl) = true;
      DECL_STRUCT_FUNCTION (current_function_decl)->has_nonlocal_label = true;
#if !GCC45
      /* ?
      DECL_NONLOCAL seems very similar, but causes bad codegen:

          DECL_NONLOCAL on label before PushEFrame
          causes $rbp to be altered incorrectly.
          e.g. in RTAllocator__AllocTraced,
          such that PushEFrame overwrites the local "thread"
          in its caller and the assert thread.something access violates.

          We need to find another way to achieve this?
          "this": getting the label onto nonlocal_goto_handler_labels list.

          cm3 built with gcc-4.5 gets further now.
          My mistake here, trying to replace a 4.3 construct
          with a 4.5 construct. Either I haven't found the correct 4.5
          construct or there is something wrong with 4.5.
          The instruction that altered rbp was, uh, surprising.
      */
      {
        rtx list = DECL_STRUCT_FUNCTION (current_function_decl)->x_nonlocal_goto_handler_labels;
        DECL_STRUCT_FUNCTION (current_function_decl)->x_nonlocal_goto_handler_labels
          = gen_rtx_EXPR_LIST (VOIDmode, r, list);
      }
#endif
      /* put asm("") before and after the label */
      for (UINT i = 0; i < 2; ++i)
      {
        tree bar = make_node (ASM_EXPR);
        TREE_TYPE (bar) = t_void;
        ASM_STRING (bar) = build_string (0, "");
        ASM_VOLATILE_P (bar) = true;
        add_stmt (bar);

        if (i == 0)
          add_stmt (build1 (LABEL_EXPR, t_void, label));
      }
    }
  else
    add_stmt (build1 (LABEL_EXPR, t_void, label));
}

M3CG_HANDLER (JUMP)
{
  add_stmt (build1 (GOTO_EXPR, t_void, label));
}

M3CG_HANDLER (IF_TRUE)
{
  tree cond = m3_cast (boolean_type_node, EXPR_REF (-1));
  EXPR_POP ();

  add_stmt (build3 (COND_EXPR, t_void, cond,
                    build1 (GOTO_EXPR, t_void, label),
                    NULL_TREE));
}

M3CG_HANDLER (IF_FALSE)
{
  tree cond = m3_cast (boolean_type_node, EXPR_REF (-1));
  EXPR_POP ();
  add_stmt (build3 (COND_EXPR, t_void, cond,
                    NULL_TREE,
                    build1 (GOTO_EXPR, t_void, label)));
}

static void
m3cg_if_compare (tree type, tree label, enum tree_code code)
{
  tree t1 = m3_cast (type, EXPR_REF (-1));
  tree t2 = m3_cast (type, EXPR_REF (-2));
  add_stmt (build3 (COND_EXPR, t_void, build2 (code, boolean_type_node, t2, t1),
                    build1 (GOTO_EXPR, t_void, label),
                    NULL_TREE));
  EXPR_POP ();
  EXPR_POP ();
}

M3CG_HANDLER (IF_EQ) { m3cg_if_compare (type, label, EQ_EXPR); }
M3CG_HANDLER (IF_NE) { m3cg_if_compare (type, label, NE_EXPR); }
M3CG_HANDLER (IF_GT) { m3cg_if_compare (type, label, GT_EXPR); }
M3CG_HANDLER (IF_GE) { m3cg_if_compare (type, label, GE_EXPR); }
M3CG_HANDLER (IF_LT) { m3cg_if_compare (type, label, LT_EXPR); }
M3CG_HANDLER (IF_LE) { m3cg_if_compare (type, label, LE_EXPR); }

M3CG_HANDLER (CASE_JUMP)
{
  tree index_expr = EXPR_REF (-1);

  pending_stmts = tree_cons (NULL_TREE, current_stmts, pending_stmts);
  current_stmts = alloc_stmt_list ();
  for (UINT64 i = 0; i < n; ++i)
  {
    tree target_label = labels[i];

#if GCC45
    tree case_label = create_artificial_label (input_location);
#else
    tree case_label = create_artificial_label ();
#endif
    add_stmt (build_case_label(build_int_cst (t_int, i), NULL_TREE, case_label));
    add_stmt (build1 (GOTO_EXPR, t_void, target_label));
  }
  tree body = current_stmts;

  current_stmts = TREE_VALUE (pending_stmts);
  pending_stmts = TREE_CHAIN (pending_stmts);
  add_stmt (build3 (SWITCH_EXPR, type, index_expr, body, NULL_TREE));
  EXPR_POP ();
}

void m3cg_CASE_JUMP_t::read_extended ()
{
  /* case_jump is a special case */
  size_t j = n;
  labels.resize(j);
  for (size_t i = 0; i < j; ++i)
    labels[i] = scan_label (0);
}

M3CG_HANDLER (EXIT_PROC)
{
  tree res = NULL_TREE;

  if (type != t_void)
  {
    res = DECL_RESULT (current_function_decl);
#if 0
    m3_store (res, 0, type, T, type, T);
#else
    res = build2 (MODIFY_EXPR, TREE_TYPE (res), res, m3_cast (TREE_TYPE (res), EXPR_REF (-1)));
    TREE_SIDE_EFFECTS (res); /* needed? */
    EXPR_POP ();
#endif
  }
  add_stmt (build1 (RETURN_EXPR, t_void, res));
}

M3CG_HANDLER (LOAD)
{
  if (option_trace_all)
    fprintf (stderr, " m3name:%s ", m3_get_var_trace_name (var));
  m3_load (var, offset, src_t, src_T, dst_t, dst_T);
}

M3CG_HANDLER (LOAD_ADDRESS)
{
  if (option_trace_all)
    fprintf (stderr, " m3name:%s ", m3_get_var_trace_name (var));
  TREE_USED (var) = true;
  tree xvar = m3_build1 (ADDR_EXPR, t_addr, var);
  if (offset)
    xvar = m3_build2 (POINTER_PLUS_EXPR, t_addr, xvar, size_int (offset));
  EXPR_PUSH (xvar);
}

M3CG_HANDLER (LOAD_INDIRECT)
{
  tree v = EXPR_REF (-1);

  /* gcc_assert (offset >= 0); */
  if (offset >= 0)
    m3_deduce_field_reference ("m3cg_load_indirect", v, (UINT64)offset * BITS_PER_UNIT, src_t, src_T);
  if (offset)
    v = m3_build2 (POINTER_PLUS_EXPR, t_addr, v, size_int (offset));
  v = m3_cast (m3_build_pointer_type (src_t), v);
  v = m3_build1 (INDIRECT_REF, src_t, v);
  if (src_T != dst_T)
    v = m3_convert (dst_t, v);
  EXPR_REF (-1) = v;
}

M3CG_HANDLER (STORE)
{
  if (option_trace_all)
    fprintf (stderr, " m3name:%s ", m3_get_var_trace_name (var));
  m3_store (var, offset, src_t, src_T, dst_t, dst_T);
}

M3CG_HANDLER (STORE_INDIRECT)
{
  tree v = EXPR_REF (-2);

  m3_deduce_field_reference ("m3cg_store_indirect", v, offset * BITS_PER_UNIT, src_t, src_T);
  if (offset)
    v = m3_build2 (POINTER_PLUS_EXPR, t_addr, v, size_int (offset));
  v = m3_cast (m3_build_pointer_type (dst_t), v);
  v = m3_build1 (INDIRECT_REF, dst_t, v);
  add_stmt (build2 (MODIFY_EXPR, dst_t, v,
                    m3_convert (dst_t,
                                m3_cast (src_t, EXPR_REF (-1)))));
  EXPR_POP ();
  EXPR_POP ();
}

M3CG_HANDLER (LOAD_NIL)
{
  EXPR_PUSH (v_null);
}

M3CG_HANDLER (LOAD_INTEGER)
{
  EXPR_PUSH (build_int_cst (type, n));
}

M3CG_HANDLER (LOAD_FLOAT)
{
  tree a = f;
  if (TREE_TYPE (a) != type)
    a = m3_convert (type, a);
  EXPR_PUSH (a);
}

static void
m3cg_compare (tree src_t, tree dst_t, enum tree_code code)
{
  tree t1 = m3_cast (src_t, EXPR_REF (-1));
  tree t2 = m3_cast (src_t, EXPR_REF (-2));

  EXPR_REF (-2) = m3_build2 (code, dst_t, t2, t1);
  EXPR_POP ();
}

M3CG_HANDLER (EQ) { m3cg_compare (src_t, dst_t, EQ_EXPR); }
M3CG_HANDLER (NE) { m3cg_compare (src_t, dst_t, NE_EXPR); }
M3CG_HANDLER (GT) { m3cg_compare (src_t, dst_t, GT_EXPR); }
M3CG_HANDLER (GE) { m3cg_compare (src_t, dst_t, GE_EXPR); }
M3CG_HANDLER (LT) { m3cg_compare (src_t, dst_t, LT_EXPR); }
M3CG_HANDLER (LE) { m3cg_compare (src_t, dst_t, LE_EXPR); }

static void
binop (tree type, enum tree_code code)
/* binary operation */
{
  EXPR_REF (-2) = m3_build2 (code, type,
                             m3_cast (type, EXPR_REF (-2)),
                             m3_cast (type, EXPR_REF (-1)));
  EXPR_POP ();
}

static void
unop (tree type, enum tree_code code)
/* unary operation */
{
  EXPR_REF (-1) = m3_build1 (code, type, m3_cast (type, EXPR_REF (-1)));
}

M3CG_HANDLER (ADD)
{
  binop (type, PLUS_EXPR);
}

M3CG_HANDLER (SUBTRACT)
{
  binop (type, MINUS_EXPR);
}

M3CG_HANDLER (MULTIPLY)
{
  binop (type, MULT_EXPR);
}

M3CG_HANDLER (DIVIDE)
{
  binop (type, RDIV_EXPR);
}

M3CG_HANDLER (NEGATE)
{
  unop (type, NEGATE_EXPR);
}

M3CG_HANDLER (ABS)
{
  unop (type, ABS_EXPR);
}

static void
m3_minmax (tree type, int min)
{
  tree x[2] = { m3_cast (type, EXPR_REF (-1)),
                m3_cast (type, EXPR_REF (-2)) };
  EXPR_REF (-2) = m3_build3 (COND_EXPR, type,
                             m3_build2 (LE_EXPR, boolean_type_node, x[!min],
                             x[min]), x[0], x[1]);
  EXPR_POP ();
}

M3CG_HANDLER (MIN) { m3_minmax (type, 1); }
M3CG_HANDLER (MAX) { m3_minmax (type, 0); }

M3CG_HANDLER (ROUND)
{
  REAL_VALUE_TYPE r;

  memset (&r, 0, sizeof(r));
  tree arg = declare_temp (src_t);
  add_stmt (m3_build2 (MODIFY_EXPR, src_t, arg,
                       m3_cast (src_t, EXPR_REF (-1))));

  real_from_string (&r, "0.5");
  tree pos = build_real (src_t, r);

  real_from_string (&r, "-0.5");
  tree neg = build_real (src_t, r);

  tree cond = m3_build2 (GT_EXPR, boolean_type_node, arg,
                         build_real_from_int_cst (src_t, v_zero));

  EXPR_REF (-1) = m3_build1 (FIX_TRUNC_EXPR, dst_t,
                             m3_build2 (PLUS_EXPR, src_t, arg,
                                        m3_build3 (COND_EXPR, src_t,
                                                   cond, pos, neg)));
}

M3CG_HANDLER (TRUNC)
{
  EXPR_REF (-1) =
    m3_build1 (FIX_TRUNC_EXPR, dst_t, m3_cast (src_t, EXPR_REF (-1)));
}

M3CG_HANDLER (FLOOR)
{
  tree arg = declare_temp (src_t);
  add_stmt (m3_build2 (MODIFY_EXPR, src_t, arg,
                       m3_cast (src_t, EXPR_REF (-1))));

  tree intval = declare_temp (dst_t);
  add_stmt (m3_build2 (MODIFY_EXPR, dst_t, intval,
                       m3_build1 (FIX_TRUNC_EXPR, dst_t, arg)));

  tree cond = m3_build2 (LE_EXPR, boolean_type_node,
                         m3_build1 (FLOAT_EXPR, src_t, intval), arg);

  EXPR_REF (-1) = m3_build3 (COND_EXPR, dst_t, cond, intval,
                             m3_build2 (MINUS_EXPR, dst_t, intval,
                                        build_int_cst (dst_t, 1)));
}

M3CG_HANDLER (CEILING)
{
  tree arg = declare_temp (src_t);
  add_stmt (m3_build2 (MODIFY_EXPR, src_t, arg,
                       m3_cast (src_t, EXPR_REF (-1))));

  tree intval = declare_temp (dst_t);
  add_stmt (m3_build2 (MODIFY_EXPR, dst_t, intval,
                       m3_build1 (FIX_TRUNC_EXPR, dst_t, arg)));

  tree cond = m3_build2 (GE_EXPR, boolean_type_node,
                         m3_build1 (FLOAT_EXPR, src_t, intval), arg);

  EXPR_REF (-1) = m3_build3 (COND_EXPR, dst_t, cond, intval,
                             m3_build2 (PLUS_EXPR, dst_t, intval,
                                        build_int_cst (dst_t, 1)));
}

M3CG_HANDLER (CVT_FLOAT)
{
  if (FLOAT_TYPE_P (src_t))
    EXPR_REF (-1) = m3_convert (dst_t, EXPR_REF (-1));
  else
    EXPR_REF (-1) = m3_build1 (FLOAT_EXPR, dst_t, EXPR_REF (-1));
}

M3CG_HANDLER (DIV)
{
  binop (type, FLOOR_DIV_EXPR);
}

M3CG_HANDLER (MOD)
{
  binop (type, FLOOR_MOD_EXPR);
}

M3CG_HANDLER (SET_UNION)
{
  setop (set_union_proc, n, 3);
}

M3CG_HANDLER (SET_DIFFERENCE)
{
  setop (set_diff_proc, n, 3);
}

M3CG_HANDLER (SET_INTERSECTION)
{
  setop (set_inter_proc, n, 3);
}

M3CG_HANDLER (SET_SYM_DIFFERENCE)
{
  setop (set_sdiff_proc, n, 3);
}

static void
m3cg_assert_int(tree t)
{
  if (t != t_int && t != t_word)
  {
    fprintf (stderr, "\nword size/target/configure confusion? forgot -m32 or -m64?\n");
    gcc_assert (t == t_int || t == t_word);
  }
}

static tree
m3cg_set_member_ref (tree type, tree* out_bit_in_word)
{
  /* Common code for set_member and set_singleton, something like:
    set_member_ref (const size_t* set, size_t bit_index, tree* bit_in_word)
    {
      grain = (sizeof(size_t) * 8)
      *bit_in_word = (((size_t)1) << (bit_index % grain));
      return set[bit_index / grain];
    }
  */

  tree bit         = m3_cast (t_word, EXPR_REF (-1));
  tree set         = m3_cast (t_set, EXPR_REF (-2));

  /* div and mod work as well as shifting, even when not optimizing. */

  tree word        = m3_build2 (TRUNC_DIV_EXPR, t_word, bit, bits_per_integer_tree);
  tree bit_in_word = m3_build2 (TRUNC_MOD_EXPR, t_word, bit, bits_per_integer_tree);
  tree byte        = m3_build2 (MULT_EXPR, t_word, word, bytes_per_integer_tree);
  tree word_ref    = m3_build2 (POINTER_PLUS_EXPR, t_set, set, byte);
  tree one         = m3_cast (t_word, v_one);

  word_ref         = m3_build1 (INDIRECT_REF, t_word, word_ref);
  *out_bit_in_word = m3_build2 (LSHIFT_EXPR, t_word, one, bit_in_word);
  m3cg_assert_int (type);
  EXPR_POP ();
  EXPR_POP ();
  return word_ref;
}

M3CG_HANDLER (SET_MEMBER)
{
  /* Equivalent to
    int set_member (const size_t* set, size_t bit_index)
    {
      grain = (sizeof(size_t) * 8);
      return ((set[bit_index  / grain] & (((size_t)1) << (bit_index % grain))) != 0);
    }
  */
  tree bit_in_word;
  tree word_ref = m3cg_set_member_ref (type, &bit_in_word);
  tree t = m3_build2 (BIT_AND_EXPR, t_word, word_ref, bit_in_word);
  t = m3_build2 (NE_EXPR, boolean_type_node, t, m3_cast (t_word, v_zero));
  EXPR_PUSH (t);
}

static void
m3cg_set_compare (UINT64 n, tree type, tree proc)
{
  m3cg_assert_int (type);
  setop (proc, n, 2);
}

M3CG_HANDLER (SET_GT) { m3cg_set_compare (n, type, set_gt_proc); }
M3CG_HANDLER (SET_GE) { m3cg_set_compare (n, type, set_ge_proc); }
M3CG_HANDLER (SET_LT) { m3cg_set_compare (n, type, set_lt_proc); }
M3CG_HANDLER (SET_LE) { m3cg_set_compare (n, type, set_le_proc); }

static void
m3cg_set_compare_eq_or_ne (UINT64 n, tree type, enum tree_code code)
{
  m3cg_assert_int (type);
  m3_start_call ();
  m3_pop_param (t_addr);
  m3_pop_param (t_addr);
  EXPR_PUSH (size_int (n));
  m3_pop_param (t_word);
  m3_call_direct (memcmp_proc, TREE_TYPE (TREE_TYPE (memcmp_proc)));
  EXPR_REF (-1) = m3_build2 (code, boolean_type_node, EXPR_REF (-1), m3_cast (t_int_32, v_zero));
}

M3CG_HANDLER (SET_EQ)
{
  m3cg_set_compare_eq_or_ne (n, type, EQ_EXPR);
}

M3CG_HANDLER (SET_NE)
{
  m3cg_set_compare_eq_or_ne (n, type, NE_EXPR);
}

M3CG_HANDLER (SET_RANGE)
{
  m3cg_assert_int (type);
  setop2 (set_range_proc, 3);
}

M3CG_HANDLER (SET_SINGLETON)
{
  /* Equivalent to
    int set_singleton (size_t* set, size_t bit_index)
    {
      grain = (sizeof(size_t) * 8);
      set[bit_index / grain] |= (((size_t)1) << (bit_index % grain));
    }
  */
  tree bit_in_word;
  tree word_ref = m3cg_set_member_ref (type, &bit_in_word);
  tree t = m3_build2 (BIT_IOR_EXPR, t_word, word_ref, bit_in_word);
  add_stmt (m3_build2 (MODIFY_EXPR, t_word, word_ref, t));
}

M3CG_HANDLER (NOT)
{
  unop (type, BIT_NOT_EXPR);
}

M3CG_HANDLER (AND)
{
  binop (type, BIT_AND_EXPR);
}

M3CG_HANDLER (OR)
{
  binop (type, BIT_IOR_EXPR);
}

M3CG_HANDLER (XOR)
{
  binop (type, BIT_XOR_EXPR);
}

#if 0

M3CG_HANDLER (SHIFT)
{
  tree n = m3_convert (t_int, EXPR_REF (-1));
  tree x = m3_convert (type, EXPR_REF (-2));

  gcc_assert (INTEGRAL_TYPE_P (type));

  EXPR_REF (-2) = m3_build3 (COND_EXPR, m3_unsigned_type (type),
                             m3_build2 (GE_EXPR, boolean_type_node, n, v_zero),
                             m3_do_shift (LSHIFT_EXPR, type, x, n),
                             m3_do_shift (RSHIFT_EXPR, type, x,
                                          m3_build1 (NEGATE_EXPR, t_int, n)));
  EXPR_POP();
}

#else

M3CG_HANDLER (SHIFT)
{
  tree n = m3_convert (t_int, EXPR_REF (-1));
  tree x = EXPR_REF (-2);
  /* x = m3_convert (type, x);    redundant with do_shift */

  gcc_assert (INTEGRAL_TYPE_P (type));

  EXPR_REF (-2) = m3_build3 (COND_EXPR, type,
                             m3_build2 (GE_EXPR, boolean_type_node, n, m3_cast (t_int, v_zero)),
                             m3_do_shift (LSHIFT_EXPR, type, x, n),
                             m3_do_shift (RSHIFT_EXPR, type, x,
                                          m3_build1 (NEGATE_EXPR, t_int, n)));
  EXPR_POP();
}

#endif

M3CG_HANDLER (SHIFT_LEFT)
{
  gcc_assert (INTEGRAL_TYPE_P (type));

  EXPR_REF (-2) = m3_do_shift (LSHIFT_EXPR, type, EXPR_REF (-2), EXPR_REF (-1));
  EXPR_POP ();
}

M3CG_HANDLER (SHIFT_RIGHT)
{
  gcc_assert (INTEGRAL_TYPE_P (type));

  EXPR_REF (-2) = m3_do_shift (RSHIFT_EXPR, type, EXPR_REF (-2), EXPR_REF (-1));
  EXPR_POP ();
}

M3CG_HANDLER (ROTATE)
{
  tree n = m3_convert (t_int, EXPR_REF (-1));
  tree x = m3_convert (type, EXPR_REF (-2)); /* cast? */

  gcc_assert (INTEGRAL_TYPE_P (type));

  EXPR_REF (-2) = m3_build3 (COND_EXPR, type,
                             m3_build2 (GE_EXPR, boolean_type_node, n, m3_cast (t_int, v_zero)),
                             m3_do_rotate (LROTATE_EXPR, type, x, n),
                             m3_do_rotate (RROTATE_EXPR, type, x,
                                           m3_build1 (NEGATE_EXPR, t_int, n)));
  EXPR_POP();
}

M3CG_HANDLER (ROTATE_LEFT)
{
  gcc_assert (INTEGRAL_TYPE_P (type));

  EXPR_REF (-2) = m3_do_rotate (LROTATE_EXPR, type, EXPR_REF (-2), EXPR_REF (-1));
  EXPR_POP ();
}

M3CG_HANDLER (ROTATE_RIGHT)
{
  gcc_assert (INTEGRAL_TYPE_P (type));

  EXPR_REF (-2) = m3_do_rotate (RROTATE_EXPR, type, EXPR_REF (-2), EXPR_REF (-1));
  EXPR_POP ();
}

M3CG_HANDLER (WIDEN)
{
  tree dst_t = (sign ? t_int_64 : t_word_64);
  tree src_t  = (sign ? t_int_32 : t_word_32);

  EXPR_REF (-1) = m3_convert (dst_t,
                              m3_cast (src_t, EXPR_REF (-1)));
}

M3CG_HANDLER (CHOP)
{
  EXPR_REF (-1) = m3_convert (t_int_32,
                              m3_build2 (BIT_AND_EXPR, t_int_64, EXPR_REF (-1),
                                         build_int_cst (t_int, 0xffffffff)));
}

M3CG_HANDLER (EXTRACT)
{
  gcc_assert (INTEGRAL_TYPE_P (type));

  type = sign_extend ? m3_signed_type (type) : m3_unsigned_type (type);
  EXPR_REF (-3) = m3_do_extract (EXPR_REF (-3), EXPR_REF (-2), EXPR_REF (-1), type);
  EXPR_POP ();
  EXPR_POP ();
}

M3CG_HANDLER (EXTRACT_N)
{
  gcc_assert (INTEGRAL_TYPE_P (type));
  gcc_assert (count <= 64);
  gcc_assert (count <= TYPE_PRECISION (type));

  if (count == 0)
    EXPR_REF (-2) = m3_cast (type, v_zero);
  else
  {
    type = sign_extend ? m3_signed_type (type) : m3_unsigned_type (type);
    EXPR_REF (-2) = m3_do_extract (EXPR_REF (-2), EXPR_REF (-1),
                                   build_int_cst (t_int, count), type);
  }
  EXPR_POP ();
}

static tree
m3_do_fixed_extract (tree x, INT64 m, INT64 n, tree type)
{
  /* ??? Use BIT_FIELD_REF ???  */
  INT64 a = TYPE_PRECISION (type) - n;
  INT64 b = TYPE_PRECISION (type) - n - m;

  gcc_assert (m >= 0);
  gcc_assert (n > 0);
  gcc_assert (m <= 64);
  gcc_assert (n <= 64);
  gcc_assert ((m + n) <= 64);
  gcc_assert (m <= TYPE_PRECISION (type));
  gcc_assert (n <= TYPE_PRECISION (type));
  gcc_assert ((m + n) <= TYPE_PRECISION (type));

  if ((a < 0) || (a >= TYPE_PRECISION (type)) ||
      (b < 0) || (b >= TYPE_PRECISION (type)))
    {
      return m3_do_extract (x,
                            build_int_cst (t_int, m),
                            build_int_cst (t_int, n),
                            type);
    }

  x = m3_convert (type, x);
  x = (b ? m3_build2 (LSHIFT_EXPR, type, x, build_int_cst (t_int, b)) : x);
  x = (a ? m3_build2 (RSHIFT_EXPR, type, x, build_int_cst (t_int, a)) : x);
  return x;
}

#if 1

M3CG_HANDLER (EXTRACT_MN)
{
  gcc_assert (INTEGRAL_TYPE_P (type));
  gcc_assert (offset <= 64);
  gcc_assert (count <= 64);
  gcc_assert ((offset + count) <= 64);
  gcc_assert (offset <= TYPE_PRECISION (type));
  gcc_assert (count <= TYPE_PRECISION (type));
  gcc_assert ((offset + count) <= TYPE_PRECISION (type));

  if (count == 0)
    EXPR_REF (-1) = m3_cast (type, v_zero);
  else
  {
    type = sign_extend ? m3_signed_type (type) : m3_unsigned_type (type);
    EXPR_REF (-1) = m3_do_fixed_extract (EXPR_REF (-1), offset, count, type);
  }
}

#else

M3CG_HANDLER (EXTRACT_MN)
{
  gcc_assert (INTEGRAL_TYPE_P (type));
  gcc_assert (offset <= 64);
  gcc_assert (count <= 64);
  gcc_assert ((offset + count) <= 64);
  gcc_assert (offset <= TYPE_PRECISION (type));
  gcc_assert (count <= TYPE_PRECISION (type));
  gcc_assert ((offset + count) <= TYPE_PRECISION (type));

  tree v = m3_cast (type, EXPR_REF (-1));

  if (count == 0)
  {
    v = m3_cast (type, v_zero);
  }
  else if (offset == 0 && count == TYPE_PRECISION (type))
  {
    /* nothing further */
  }
  else
  {
    v = m3_build3 (BIT_FIELD_REF, type, v, bitsize_int (count), bitsize_int (offset));
    BIT_FIELD_REF_UNSIGNED(v) = !sign_extend;
  }
  EXPR_REF (-1) = v;
}

#endif

M3CG_HANDLER (INSERT)
{
  gcc_assert (INTEGRAL_TYPE_P (type));

  EXPR_REF (-4) = m3_do_insert (EXPR_REF (-4), EXPR_REF (-3),
                                EXPR_REF (-2), EXPR_REF (-1), type);
  EXPR_POP ();
  EXPR_POP ();
  EXPR_POP ();
}

M3CG_HANDLER (INSERT_N)
{
  gcc_assert (INTEGRAL_TYPE_P (type));

  EXPR_REF (-3) = m3_do_insert (EXPR_REF (-3), EXPR_REF (-2),
                                EXPR_REF (-1), build_int_cst (t_int, count), type);
  EXPR_POP ();
  EXPR_POP ();
}

#if 1

M3CG_HANDLER (INSERT_MN)
{
  gcc_assert (INTEGRAL_TYPE_P (type));

  EXPR_REF (-2) = m3_do_fixed_insert (EXPR_REF (-2), EXPR_REF (-1), offset, count, type);
  EXPR_POP ();
}

#else

M3CG_HANDLER (INSERT_MN)
{
  gcc_assert (INTEGRAL_TYPE_P (type));
  gcc_assert (offset <= 64);
  gcc_assert (count <= 64);
  gcc_assert ((offset + count) <= 64);
  gcc_assert (offset <= TYPE_PRECISION (type));
  gcc_assert (count <= TYPE_PRECISION (type));
  gcc_assert ((offset + count) <= TYPE_PRECISION (type));

  tree x = m3_cast (type, EXPR_REF (-2));
  tree y = m3_cast (type, EXPR_REF (-1));
  tree v = { 0 };

  if (count == 0)
  {
    v = x;
  }
  else if (offset == 0 && count == TYPE_PRECISION (type))
  {
    v = y;
  }
  else
  {
    EXPR_PUSH (x);
    x = declare_temp (type);
    m3_store (temp, 0, type, T, type, T);
    tree y = m3_cast (type, EXPR_REF (-1));
    v = m3_build3 (BIT_FIELD_REF, type, x, bitsize_int (count), bitsize_int (offset));
    BIT_FIELD_REF_UNSIGNED(v) = !sign_extend;
    v = m3_build2 (MODIFY_EXPR, type, y);
  }
  EXPR_REF (-2) = v;
  EXPR_POP ();
}

#endif

M3CG_HANDLER (SWAP)
{
  m3_swap ();
}

M3CG_HANDLER (POP)
{
  EXPR_POP ();
}

#if 0

M3CG_HANDLER (COPY_N)
{
  m3cg_assert_int (count_type);
  m3_start_call ();

  /* rearrange the parameters */
  tree tmp = EXPR_REF (-3);
  EXPR_REF (-3) = EXPR_REF (-2);
  EXPR_REF (-2) = EXPR_REF (-1);
  EXPR_REF (-1) = tmp;

  m3_pop_param (t_addr);
  m3_swap ();
  m3_pop_param (t_addr);

  EXPR_REF (-1) =
    m3_build2 (MULT_EXPR, t_int,
               EXPR_REF (-1),
               TYPE_SIZE_UNIT (mem_type));
  m3_pop_param (count_type);
  m3_call_direct (overlap ? memmove_proc : memcpy_proc, t_void);
}

#else

M3CG_HANDLER (COPY_N)
{
  m3cg_assert_int (count_type);
  gcc_assert (count_type == t_int || count_type == t_word);
  m3_start_call ();

  /* rearrange the parameters */
  tree count = EXPR_REF (-1);
  tree source = EXPR_REF (-2);
  tree dest = EXPR_REF (-3);
  EXPR_REF (-3) = count;
  EXPR_REF (-2) = source;
  EXPR_REF (-1) = dest;

  m3_pop_param (t_addr); /* dest */
  m3_pop_param (t_addr); /* source */
  
  EXPR_REF (-1) = m3_cast (t_word, m3_cast (count_type, EXPR_REF (-1)));
  EXPR_REF (-1) = m3_build2 (MULT_EXPR, t_word, EXPR_REF (-1),
                             TYPE_SIZE_UNIT (mem_type));
  m3_pop_param (t_word);
  m3_call_direct (overlap ? memmove_proc : memcpy_proc, t_void);
}

#endif

#if 0

M3CG_HANDLER (COPY)
{
  tree ts = make_node (LANG_TYPE);
  UINT64 s = n * TREE_INT_CST_LOW (TYPE_SIZE (type));

  TYPE_SIZE (ts) = size_int (s);
  TYPE_SIZE_UNIT (ts) = size_binop (FLOOR_DIV_EXPR, TYPE_SIZE (ts),
                                    size_int (BITS_PER_UNIT));
  TYPE_ALIGN (ts) = TYPE_ALIGN (type);

  if (FLOAT_TYPE_P (type))
    SET_TYPE_MODE (ts, mode_for_size (s, MODE_FLOAT, 0));
  else
    SET_TYPE_MODE (ts, BLKmode);

  tree pts = m3_build_pointer_type (ts);

  add_stmt (build2 (MODIFY_EXPR, type,
                    m3_build1 (INDIRECT_REF, ts,
                               m3_cast (pts, EXPR_REF (-2))),
                    m3_build1 (INDIRECT_REF, ts,
                               m3_cast (pts, EXPR_REF (-1)))));
  EXPR_POP ();
  EXPR_POP ();
}

#else

M3CG_HANDLER (COPY)
{
  m3_start_call ();
  m3_swap ();
  m3_pop_param (t_addr);
  m3_pop_param (t_addr);
  EXPR_PUSH (m3_build2 (MULT_EXPR, t_word, size_int (n), TYPE_SIZE_UNIT (type)));
  m3_pop_param (t_word);
  m3_call_direct (overlap ? memmove_proc : memcpy_proc, t_void);
}

#endif

#if 0

M3CG_HANDLER (ZERO_N)
{
  m3cg_assert_int (count_type);
  EXPR_REF (-1) = m3_build2 (MULT_EXPR, t_int, EXPR_REF (-1),
                             TYPE_SIZE_UNIT (mem_type));

  m3_start_call ();
  m3_swap ();
  m3_pop_param (t_addr);
  EXPR_PUSH (v_zero);
  m3_pop_param (t_int);
  m3_pop_param (t_int);
  m3_call_direct (memset_proc, t_void);
}

#else

M3CG_HANDLER (ZERO_N)
{
  m3cg_assert_int (count_type);
  gcc_assert (count_type == t_int || count_type == t_word);
  EXPR_REF (-1) = m3_cast (t_word, m3_cast (count_type, EXPR_REF (-1)));
  EXPR_REF (-1) = m3_build2 (MULT_EXPR, t_word, EXPR_REF (-1),
                             TYPE_SIZE_UNIT (mem_type));
  m3_start_call ();
  m3_swap ();
  m3_pop_param (t_addr);
  EXPR_PUSH (m3_cast (t_int_32, v_zero));
  m3_pop_param (t_int_32);
  m3_pop_param (t_word);
  m3_call_direct (memset_proc, t_void);
}

#endif

#if 0

M3CG_HANDLER (ZERO)
{
  m3_start_call ();
  m3_pop_param (t_addr);
  EXPR_PUSH (v_zero);
  m3_pop_param (t_int);
  EXPR_PUSH (m3_build2 (MULT_EXPR, t_int, size_int (n), TYPE_SIZE_UNIT (mem_type)));
  m3_pop_param (t_int);
  m3_call_direct (memset_proc, t_void);
}

#else

M3CG_HANDLER (ZERO)
{
  m3_start_call ();
  m3_pop_param (t_addr);
  EXPR_PUSH (m3_cast (t_int_32, v_zero));
  m3_pop_param (t_int_32);
  EXPR_PUSH (m3_build2 (MULT_EXPR, t_word, size_int (n), TYPE_SIZE_UNIT (mem_type)));
  m3_pop_param (t_word);
  m3_call_direct (memset_proc, t_void);
}

#endif

M3CG_HANDLER (LOOPHOLE)
{
  if (IS_INTEGER_TYPE (Type1) && IS_INTEGER_TYPE (Type2))
  {
    EXPR_REF (-1) = m3_cast (type2, EXPR_REF (-1));
    return;
  }
  if (M3_LOOPHOLE_VIEW_CONVERT
        && TYPE_SIZE (type1)
        && TYPE_SIZE (type2)
        && TREE_INT_CST_LOW (TYPE_SIZE (type1)) == TREE_INT_CST_LOW (TYPE_SIZE (type2)))
   {
     EXPR_REF (-1) = m3_build1 (VIEW_CONVERT_EXPR, type2, EXPR_REF (-1));
     return;
  }
  if (M3_LOOPHOLE_VIEW_CONVERT && option_trace_all)
  {
    fprintf (stderr, "loophole falling back to old path\n");
  }
  if (FLOAT_TYPE_P (type1) != FLOAT_TYPE_P (type2))
  {
    tree v = declare_temp (type1);
    m3_store (v, 0, type1, Type1, type1, Type1);
    m3_load (v, 0, type2, Type2, type2, Type2);
    return;
  }
  EXPR_REF (-1) = m3_cast (type2, EXPR_REF (-1));
}

M3CG_HANDLER (ABORT)
{
  add_stmt (generate_fault (code));
}

M3CG_HANDLER (CHECK_NIL)
{
  EXPR_REF (-1) = m3_convert (t_addr, EXPR_REF (-1));
  add_stmt (build3 (COND_EXPR, t_void,
		    m3_build2 (EQ_EXPR, boolean_type_node, EXPR_REF (-1), v_null),
		    generate_fault (code),
		    NULL_TREE));
}

M3CG_HANDLER (CHECK_LO)
{
  EXPR_REF (-1) = m3_convert (type, EXPR_REF (-1));
  add_stmt (build3 (COND_EXPR, t_void,
                    m3_build2 (LT_EXPR,
                               boolean_type_node,
                               EXPR_REF (-1),
                               build_int_cst (type, a)),
                    generate_fault (code),
                    NULL_TREE));
}

M3CG_HANDLER (CHECK_HI)
{
  EXPR_REF (-1) = m3_convert (type, EXPR_REF (-1));
  add_stmt (build3 (COND_EXPR, t_void,
                    m3_build2 (GT_EXPR,
                               boolean_type_node,
                               EXPR_REF (-1),
                               build_int_cst (type, a)),
                    generate_fault (code),
                    NULL_TREE));
}

M3CG_HANDLER (CHECK_RANGE)
{
  EXPR_REF (-1) = m3_convert (type, EXPR_REF (-1));
  add_stmt (build3 (COND_EXPR, t_void,
                    m3_build2 (TRUTH_ORIF_EXPR, boolean_type_node,
                               m3_build2 (LT_EXPR,
                                          boolean_type_node,
                                          EXPR_REF (-1),
                                          build_int_cst (type, a)),
                               m3_build2 (GT_EXPR,
                                          boolean_type_node,
                                          EXPR_REF (-1),
                                          build_int_cst (type, b))),
                    generate_fault (code),
                    NULL_TREE));
}

M3CG_HANDLER (CHECK_INDEX)
{
  type = m3_unsigned_type (type);
  add_stmt (build3 (COND_EXPR, t_void,
                    m3_build2 (GE_EXPR, boolean_type_node,
                               m3_convert (type, EXPR_REF (-2)),
                               m3_convert (type, EXPR_REF (-1))),
                    generate_fault (code),
                    NULL_TREE));
  EXPR_POP ();
}

M3CG_HANDLER (CHECK_EQ)
{
  tree temp1 = m3_convert (type, EXPR_REF (-1));
  tree temp2 = m3_convert (type, EXPR_REF (-2));
  add_stmt (build3 (COND_EXPR, t_void,
		    m3_build2 (NE_EXPR, boolean_type_node, temp1, temp2),
                    generate_fault (code),
                    NULL_TREE));
  EXPR_POP ();
  EXPR_POP ();
}

M3CG_HANDLER (ADD_OFFSET)
{
  EXPR_REF (-1) = m3_build2 (POINTER_PLUS_EXPR, t_addr,
                             EXPR_REF (-1), size_int (n));
}

M3CG_HANDLER (INDEX_ADDRESS)
{
  bool signd = IS_SIGNED_INTEGER_TYPE_TREE (type);
  gcc_assert (signd || IS_UNSIGNED_INTEGER_TYPE_TREE (type));
  tree a = (signd ? ssize_int (bytes) : size_int (bytes));
  a = m3_build2 (MULT_EXPR, type, m3_cast (type, EXPR_REF (-1)), a);
  if (signd)
    a = m3_cast (ssizetype, a);
  a = m3_cast (sizetype, a);
  EXPR_REF (-2) = m3_build2 (POINTER_PLUS_EXPR, t_addr, m3_cast (t_addr, EXPR_REF (-2)), a);
  EXPR_POP ();
}

M3CG_HANDLER (START_CALL_DIRECT)
{
  m3_start_call ();
}

M3CG_HANDLER (CALL_DIRECT)
{
  m3_call_direct (p, type);
}

M3CG_HANDLER (START_CALL_INDIRECT)
{
  m3_start_call ();
}

M3CG_HANDLER (CALL_INDIRECT)
{
  m3_call_indirect (type, calling_convention);
}

M3CG_HANDLER (POP_PARAM)
{
  m3_pop_param (type);
}

M3CG_HANDLER (POP_STRUCT)
{
  tree type = m3_build_type_id (T_struct, size, align, my_id);

  if (option_trace_all)
    fprintf (stderr, " type:%p", type);

  gcc_assert (align >= !!size);

  EXPR_REF (-1) = m3_build1 (INDIRECT_REF, type,
                             m3_cast (m3_build_pointer_type (type), EXPR_REF (-1)));
  m3_pop_param (type);
}

M3CG_HANDLER (POP_STATIC_LINK)
{
  DECL_UNINLINABLE (current_function_decl) = true; /* bug */
  tree v = declare_temp (t_addr);
  m3_store (v, 0, t_addr, T_addr, t_addr, T_addr);
  CALL_TOP_STATIC_CHAIN () = v;
}

M3CG_HANDLER (LOAD_PROCEDURE)
{
  EXPR_PUSH (proc_addr (p));
}

M3CG_HANDLER (LOAD_STATIC_LINK)
{
  DECL_UNINLINABLE (current_function_decl) = true; /* bug */
  EXPR_PUSH (build1 (STATIC_CHAIN_EXPR, t_addr, p));
}

M3CG_HANDLER (COMMENT)
{
}

typedef enum { Relaxed, Release, Acquire, AcquireRelease, Sequential } Order;

M3CG_HANDLER (STORE_ORDERED)
{
  tree v = EXPR_REF (-2);

  if (order != Relaxed)
    warning (0, "only Relaxed memory order for stores is supported");

  v = m3_cast (m3_build_pointer_type (dst_t), v);
  v = m3_build1 (INDIRECT_REF, dst_t, v);
  add_stmt (build2 (MODIFY_EXPR, dst_t, v,
                    m3_convert (dst_t,
                                m3_cast (src_t, EXPR_REF (-1)))));
  EXPR_POP ();
  EXPR_POP ();
}

M3CG_HANDLER (LOAD_ORDERED)
{
  tree v = EXPR_REF (-1);

  if (order != Relaxed)
    warning (0, "only Relaxed memory order for loads is supported");

  v = m3_cast (m3_build_pointer_type (src_t), v);
  v = m3_build1 (INDIRECT_REF, src_t, v);
  if (src_T != dst_T)
    v = m3_convert (dst_t, v);

  EXPR_REF (-1) = v;
}

M3CG_HANDLER (EXCHANGE)
{
  INT64 size = { 0 };
  enum built_in_function fncode = BUILT_IN_SYNC_LOCK_TEST_AND_SET_N;
  /* SYNC_LOCK_TEST_AND_SET is an acquire barrier */

  if (!INTEGRAL_TYPE_P (type1) && !POINTER_TYPE_P (type1))
    goto incompatible;
  size = tree_low_cst (TYPE_SIZE_UNIT (type1), 1);
  if (size != 1 && size != 2 && size != 4 && size != 8)
    goto incompatible;

  if (order == Sequential || order == AcquireRelease)
  {
    /* is this enough to make it Sequential or just AcquireRelease */
    m3_start_call ();
    m3_call_direct (builtin_decl_explicit (BUILT_IN_SYNC_SYNCHRONIZE), t_void);
  }
  m3_start_call ();
  m3_pop_param (type2);
  m3_pop_param (t_addr);
  CALL_TOP_ARG () = nreverse (CALL_TOP_ARG ());
  CALL_TOP_TYPE () = nreverse (CALL_TOP_TYPE ());
  m3_call_direct (builtin_decl_explicit ((enum built_in_function)(fncode + exact_log2 (size) + 1)), type2);
  return;

incompatible:
  fatal_error ("incompatible type for argument to atomic op");
}

M3CG_HANDLER (COMPARE_EXCHANGE)
{
  tree v = declare_temp (type2);
  INT64 size = { 0 };
  enum built_in_function fncode = BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_N;

  if (!INTEGRAL_TYPE_P (type1) && !POINTER_TYPE_P (type1))
    goto incompatible;
  size = tree_low_cst (TYPE_SIZE_UNIT (type1), 1);
  if (size != 1 && size != 2 && size != 4 && size != 8)
    goto incompatible;

  /* capture "expected" value */
  /* we will return it unchanged, even if success (i.e., we can fail spuriously) */
  /* we should really return the actual value seen by the exchange comparison */
  /* unfortunately, the gcc intrinsic doesn't give both a boolean and the actual value */
  add_stmt (m3_build2 (MODIFY_EXPR, type2, v, EXPR_REF (-2)));
  EXPR_REF (-2) = v;

  m3_start_call ();
  m3_pop_param (type2);
  m3_pop_param (type2);
  m3_pop_param (t_addr);
  CALL_TOP_ARG () = nreverse (CALL_TOP_ARG ());
  CALL_TOP_TYPE () = nreverse (CALL_TOP_TYPE ());
  m3_call_direct (builtin_decl_explicit ((enum built_in_function)(fncode + exact_log2 (size) + 1)), return_type);

  EXPR_PUSH (v);

  return;

incompatible:
  fatal_error ("incompatible type for argument to atomic op");
}

M3CG_HANDLER (FENCE)
{
  m3_start_call ();
  m3_call_direct (builtin_decl_explicit (BUILT_IN_SYNC_SYNCHRONIZE), t_void);
}

static void
m3cg_fetch_and_op (tree type1, tree type2, enum built_in_function fncode)
{
  INT64 size = { 0 };

  if (!INTEGRAL_TYPE_P (type1) && !POINTER_TYPE_P (type1))
    goto incompatible;
  size = tree_low_cst (TYPE_SIZE_UNIT (type1), 1);
  if (size != 1 && size != 2 && size != 4 && size != 8)
    goto incompatible;

  m3_start_call ();
  m3_pop_param (type1);
  m3_pop_param (t_addr);
  CALL_TOP_ARG () = nreverse (CALL_TOP_ARG ());
  CALL_TOP_TYPE () = nreverse (CALL_TOP_TYPE ());
  m3_call_direct (builtin_decl_explicit ((enum built_in_function)(fncode + exact_log2 (size) + 1)), type2);
  return;

incompatible:
  fatal_error ("incompatible type for argument to atomic op");
}

M3CG_HANDLER (FETCH_AND_ADD) { m3cg_fetch_and_op (type1, type2, BUILT_IN_SYNC_FETCH_AND_ADD_N); }
M3CG_HANDLER (FETCH_AND_SUB) { m3cg_fetch_and_op (type1, type2, BUILT_IN_SYNC_FETCH_AND_SUB_N); }
M3CG_HANDLER (FETCH_AND_OR) { m3cg_fetch_and_op (type1, type2, BUILT_IN_SYNC_FETCH_AND_OR_N); }
M3CG_HANDLER (FETCH_AND_AND) { m3cg_fetch_and_op (type1, type2, BUILT_IN_SYNC_FETCH_AND_AND_N); }
M3CG_HANDLER (FETCH_AND_XOR) { m3cg_fetch_and_op (type1, type2, BUILT_IN_SYNC_FETCH_AND_XOR_N); }

#if 0
M3CG_HANDLER (LOCK_TEST_AND_SET)
{
  INT64 size = { 0 };
  enum built_in_function fncode = BUILT_IN_LOCK_TEST_AND_SET_N;

  if (!INTEGRAL_TYPE_P (type) && !POINTER_TYPE_P (type))
    goto incompatible;
  size = tree_low_cst (TYPE_SIZE_UNIT (type), 1);
  if (size != 1 && size != 2 && size != 4 && size != 8)
    goto incompatible;

  m3_start_call ();
  m3_pop_param (type);
  m3_pop_param (t_addr);
  CALL_TOP_ARG () = nreverse (CALL_TOP_ARG ());
  CALL_TOP_TYPE () = nreverse (CALL_TOP_TYPE ());
  m3_call_direct (builtin_decl_explicit ((enum built_in_function)(fncode + exact_log2 (size) + 1)), type);
  return;

incompatible:
  fatal_error ("incompatible type for argument to atomic op");
}

M3CG_HANDLER (LOCK_RELEASE)
{
  INT64 size = { 0 };
  enum built_in_function fncode = BUILT_IN_LOCK_RELEASE_N;

  if (!INTEGRAL_TYPE_P (type) && !POINTER_TYPE_P (type))
    goto incompatible;
  size = tree_low_cst (TYPE_SIZE_UNIT (type), 1);
  if (size != 1 && size != 2 && size != 4 && size != 8)
    goto incompatible;

  m3_start_call ();
  m3_pop_param (t_addr);
  m3_call_direct (builtin_decl_explicit ((enum built_in_function)(fncode + exact_log2 (size) + 1)), t_void);
  return;

incompatible:
  fatal_error ("incompatible type for argument to atomic op");
}
#endif

M3CG_HANDLER (WIDECHAR_SIZE)
{ if (bitsize == 16)
    { set_typeid_to_tree (UID_WIDECHAR, t_word_16);
      dbxout_emit_widechar_N_OPT (16); 
    } 
  else if (bitsize == 32)
    { set_typeid_to_tree (UID_WIDECHAR, t_word_32);
      dbxout_emit_widechar_N_OPT (32); 
    } 
  else
    fatal_error ("WIDECHAR bitsize (%u) must be 16 or 32");
}

/*----------------------------------------------------------- M3CG parser ---*/

static SCHAR m3_indent_op[LAST_OPCODE];

static volatile UINT m3_break_lineno; /* set in debugger */

static
void
m3_breakpoint(void) /* set breakpoint in debugger */
{
#ifdef __GNUC__
  asm(""); /* do not inline */
#endif
}

static
void
trace_op (UCHAR op)
{
  if (option_trace_all)
  {
    int indent = m3_indent_op[op];
    m3_indent += (indent < 0 ? indent : 0);
    fprintf (stderr, "%s%s(%u)%s %s",
             (indent > 0 ? "\n" : ""),
             (m3cg_lineno >= 2) ? "\n" : "",
             m3cg_lineno,
             m3_indentstr (),
             M3CG_opnames[op]);
    m3_indent += (indent > 0 ? indent : 0);
  }
}

static void
#if GCC46
m3_parse_file (void)
#else
m3_parse_file (int)
#endif
{
  typedef_vector<m3cg_op_t*> ops_t;
  ops_t ops;
  ops.reserve (0x10000);

  /* This has to be done here, because gcc does not pass -gstabs or -gstabs+ to
     LANG_HOOK_HANDLE_OPTION.  In any case, gstabs and gstabs+ are the only
     debug formats parse.c can produce anyway. */ 
  if (!TARGET_MACHO) m3gdb = true;

  /* Setup indentation. */

  m3_indent_op[M3CG_IMPORT_PROCEDURE] = 2;
  m3_indent_op[M3CG_DECLARE_PROCTYPE] = 2;
  m3_indent_op[M3CG_DECLARE_PROCEDURE] = 2;
  m3_indent_op[M3CG_BEGIN_PROCEDURE] = 2;
  m3_indent_op[M3CG_END_PROCEDURE] = -2;
  m3_indent_op[M3CG_START_CALL_INDIRECT] = 2;
  m3_indent_op[M3CG_START_CALL_DIRECT] = 2;
  m3_indent_op[M3CG_CALL_INDIRECT] = -2;
  m3_indent_op[M3CG_CALL_DIRECT] = -2;
  m3_indent_op[M3CG_DECLARE_RECORD] = 2;
  m3_indent_op[M3CG_DECLARE_ENUM] = 2;

  /* check the version stamp */
  INT64 i = get_int ();
  if (i != M3CG_Version)
  {
    fatal_error (" *** bad M3CG version stamp (0x%x), expected 0x%x",
                 (UINT)i, M3CG_Version);
  }

  M3CG_opcode op = LAST_OPCODE;  
  while (op != M3CG_END_UNIT)
  {
    gcc_assert (input_cursor <= input_len);

    if (m3cg_lineno == m3_break_lineno)
      m3_breakpoint ();
    op = (M3CG_opcode)get_byte ();
    if (op >= LAST_OPCODE)
      fatal_error (" *** bad opcode: 0x%x, at m3cg_lineno %u", op, m3cg_lineno);
    
    if (option_trace_all)
      trace_op (op);
    ops.push_back(m3cg_op_t::create(op));
    ops.back()->read ();
    gcc_assert (input_cursor <= input_len);
    m3cg_lineno += 1;
  }

  ops_t::iterator e = ops.end();
  ops_t::iterator it;
  
  /* First go through and set segment sizes,
     so that declare_segment gets it right upfront.
     In future we will do more here -- get the fields right early.
  */
  for (it = ops.begin(); it != e; ++it)
  {
    switch ((*it)->op)
    {
      case M3CG_BIND_SEGMENT:
        m3cg_BIND_SEGMENT_t* bind_segment = (m3cg_BIND_SEGMENT_t*)*it;
        bind_segments[bind_segment->var_integer] = bind_segment;
        break;
    }
  }

  m3cg_lineno = 1;
  for (it = ops.begin(); it != e; ++it)
  {
    if (m3cg_lineno == m3_break_lineno)
      m3_breakpoint ();
    if (option_trace_all)
    {
      trace_op ((*it)->op);
      (*it)->trace ();
    }
    (*it)->handler();
    m3cg_lineno += 1;
  }

  if (GCC45)
  {
    write_global_declarations ();
  }
  else
  {
    cgraph_finalize_compilation_unit ();
    cgraph_optimize ();
  }
}

/*===================================================== RUNTIME FUNCTIONS ===*/

#if !GCC46

/* Prepare to handle switches.  */
static UINT
m3_init_options (UINT /*argc*/, PCSTR* /*argv*/)
{
  return CL_m3cg;
}

#endif

static bool version_done;

/* Process a switch - called by opts.c.  */
static
#if GCC46
bool
m3_handle_option (size_t code, PCSTR /*arg*/, int /*value*/, int /*kind*/,
                  location_t /*loc*/,
                  const struct cl_option_handlers * /*handlers*/)
#else
int
m3_handle_option (size_t code, PCSTR /*arg*/, int /*value*/)
#endif
{
  switch ((enum opt_code)code)
    {
    default:
      return 1;

    case OPT_gstabs:
    case OPT_gstabs_:
      /* This never happens.  gcc doesn't pass these options to 
         m3_handle_options.  It is done instead near the top of 
         m3_parse_file. */  
      if (!TARGET_MACHO)
        m3gdb = true;
      break;

    case OPT_v:
      if (!version_done)
        {
          PCSTR const ver = version_string; /* type check */
          printf ("M3CG - Modula-3 Compiler back end %s\n", ver);
          version_done = true;
        }
      break;

    case OPT_y:
    case OPT_fopcodes_trace:
    case OPT_fsource_line_trace:
    case OPT_fvars_trace:
    case OPT_fprocs_trace:
    case OPT_fexprs_trace:
    case OPT_fmisc_trace:
    case OPT_ftypes_trace:
      option_trace_all += 1;
      break;
    }

  return 1;
}

/* Post-switch processing. */
bool
m3_post_options (PCSTR* /*pfilename*/)
{
  flag_unit_at_a_time = 1; /* to handle multiple .mc files on command line */
  flag_tree_sra = false;                        /* Scalar Replacement of Aggregates -- we abuse bit_field_ref */
  flag_reorder_blocks = false;                  /* breaks our exception handling? */
  flag_reorder_blocks_and_partition = false;    /* breaks our exception handling? */
  flag_strict_aliasing = false;                 /* ? */
  flag_strict_overflow = false;                 /* ? */
  flag_exceptions = true;                       /* ? */
#if !GCC45
  flag_tree_store_ccp = false;                  /* needs retesting */
  flag_tree_salias = false;                     /* needs retesting */
#endif
#if GCC45
  /* Excess precision other than "fast" requires front-end support.  */
  flag_excess_precision_cmdline = EXCESS_PRECISION_FAST;
#if !GCC47
  flag_tree_forwprop = false;                   /* bug */
#endif
#endif

  return false;
}

/* Language dependent parser setup.  */

static bool
m3_init (void)
{
  FILE *finput = { 0 }; /* Stream for reading from the input file.  */
#ifdef M3_USE_MAPPED_LOCATION
  #undef input_filename /* global input_filename is not an lvalue. */
  PCSTR input_filename = { 0 };
  linemap_add (line_table, LC_ENTER, false, main_input_filename, 1);
#endif
  input_filename = main_input_filename;

  /* Open input file.  */
  if (input_filename == NULL || !strcmp (input_filename, "-"))
    {
      finput = stdin;
#ifdef M3_USE_MAPPED_LOCATION
      linemap_add (line_table, LC_RENAME, false, "<stdin>", 1);
#endif
      input_filename = "<stdin>";
    }
  else
    finput = fopen (input_filename, "rb");

  if (finput == NULL)
    {
      fprintf (stderr, "Unable to open input file %s\n", input_filename);
      exit (1);
    }

  /* Read the entire file */
  m3_read_entire_file (finput, &input_buffer, &input_len);
  fclose (finput);

  m3_init_lex ();
  m3_init_parse ();
  m3_init_decl_processing ();
  return true;
}

tree
convert (tree type, tree expr)
/* from c-convert.c */
/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */
{
  enum tree_code code = TREE_CODE (type);

  if (type == TREE_TYPE (expr)
      || TREE_CODE (expr) == ERROR_MARK
      || code == ERROR_MARK || TREE_CODE (TREE_TYPE (expr)) == ERROR_MARK)
    return expr;

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (expr)))
    return fold_build1 (NOP_EXPR, type, expr);
  if (TREE_CODE (TREE_TYPE (expr)) == ERROR_MARK)
    return error_mark_node;
  if (TREE_CODE (TREE_TYPE (expr)) == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  switch (code)
  {
  case VOID_TYPE:
    return build1 (CONVERT_EXPR, type, expr);
    
  case INTEGER_TYPE:
  case ENUMERAL_TYPE:
    return fold (convert_to_integer (type, expr));
      
  case BOOLEAN_TYPE:
    /* If it returns a NOP_EXPR, we must fold it here to avoid
       infinite recursion between fold () and convert ().  */
    if (TREE_CODE (expr) == NOP_EXPR)
      return fold_build1 (NOP_EXPR, type, TREE_OPERAND (expr, 0));
    else
      return fold_build1 (NOP_EXPR, type, expr);
	
  case POINTER_TYPE:
  case REFERENCE_TYPE:
    return fold (convert_to_pointer (type, expr));
    
  case REAL_TYPE:
    return fold (convert_to_real (type, expr));

  default:
    break;
  }

  error ("conversion to non-scalar type requested");
  return error_mark_node;
}

#if GCC_APPLE
extern "C" {
int flag_iasm_blocks;
bool iasm_in_operands;
struct cpp_reader* parse_in;
#define add_stmt c_add_stmt
#include "../stub-objc.c"
c_language_kind c_language;
iasm_states iasm_state;

/* This is used by cfstring code; providing it here makes us not have to #if 0 out a few bits. */
tree pushdecl_top_level (tree)
{
  gcc_unreachable ();
  return NULL;
}

} /* extern "C" */
#endif

/* garbage collection support, see gty.texi */
#include "debug.h"
#include "gtype-m3cg.h"
#include "gt-m3cg-parse.h"
