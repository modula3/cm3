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

enum machine_mode;
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <setjmp.h>

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tm_p.h"
#include "tree.h"
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
#include "flags.h"
#include "output.h"
#include "ggc.h"
#include "hashtab.h"
#include "toplev.h"
#include "varray.h"
#include "langhooks-def.h"
#include "langhooks.h"
#include "input.h"
#include "target.h"
#include "version.h"

#include "cgraph.h"

#include "expr.h"
#include "real.h"
#include "diagnostic.h"

#include "m3cg.h"
#include "opts.h"
#include "options.h"

#include "debug.h"

#ifndef TARGET_SOLARIS
#define TARGET_SOLARIS 0
#endif
#ifndef TARGET_SPARC
#define TARGET_SPARC 0
#endif
#ifndef TARGET_ARCH32
#define TARGET_ARCH32 0
#endif

/* In particular, Solaris/sparc32 has a stack walker.
   Why does the stack walker matter?
 */
#define M3_ALL_VOLATILE (TARGET_SPARC && TARGET_SOLARIS && TARGET_ARCH32)

#if GCC45

/* error: attempt to use poisoned "USE_MAPPED_LOCATION" */
#define M3_USE_MAPPED_LOCATION

/* error: macro "build_decl" requires 4 arguments, but only 3 given */
#undef build_decl
#define build_decl(code, tree, memstat) \
    build_decl_stat(UNKNOWN_LOCATION, code, tree, memstat MEM_STAT_INFO)

#elif defined USE_MAPPED_LOCATION
#define M3_USE_MAPPED_LOCATION
#define SET_TYPE_MODE(node, mode) (TYPE_MODE(node) = (mode))
#endif

#ifndef GCC42
#define GCC42 0
#endif

#ifndef GCC_APPLE
#define GCC_APPLE 0
#endif

#if GCC42
/*typedef const union tree_node *const_tree;*/
#define const_tree tree
#define allocate_struct_function(a, b) allocate_struct_function(a)
/* This is not merely copied from 4.3's tree.h, though it is.
It also matches what convert_call_expr does. */
/*#define CALL_EXPR_STATIC_CHAIN(NODE) TREE_OPERAND (CALL_EXPR_CHECK (NODE), 2) */

/* are we missing an #include? */
int arm_float_words_big_endian (void);
#endif

#define COUNT_OF(a) (sizeof(a)/sizeof((a)[0]))

/*================================================================= TREES ===*/

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
}
m3_type;

static const char* m3cg_typename(unsigned a)
{
    static const char m3cg_typenames[][9] =
    { "word8",      "int8",     "word16",   "int16",
      "word32",     "int32",    "word64",   "int64",
      "real",       "lreal",    "xreal",    "addr",
      "struct",     "void",     "word",     "int",
      "longword",   "longint"
    };
    return (a < COUNT_OF(m3cg_typenames) ? m3cg_typenames[a] : "invalid");
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

/* Types. */
static GTY (()) tree t_addr;
static GTY (()) tree t_word;
static GTY (()) tree t_int;
static GTY (()) tree t_longword;
static GTY (()) tree t_longint;
static GTY (()) tree t_reel;
static GTY (()) tree t_lreel;
static GTY (()) tree t_xreel;
static GTY (()) tree t_int_8;
static GTY (()) tree t_int_16;
static GTY (()) tree t_int_32;
static GTY (()) tree t_int_64;
static GTY (()) tree t_word_8;
static GTY (()) tree t_word_16;
static GTY (()) tree t_word_32;
static GTY (()) tree t_word_64;
static GTY (()) tree t_void;

/* Values. */
static GTY (()) tree v_zero;
static GTY (()) tree v_one;
static GTY (()) tree v_null;

/* Procedures. */
static GTY (()) tree memcpy_proc;
static GTY (()) tree memmove_proc;
static GTY (()) tree memset_proc;
static GTY (()) tree memcmp_proc;
#if 0
/* commented out due to garbage collector
static GTY (()) tree div_proc;
static GTY (()) tree mod_proc;
static GTY (()) tree divL_proc;
static GTY (()) tree modL_proc;
*/
#endif
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
static GTY (()) tree current_segment;
static GTY (()) tree fault_intf;
static GTY (()) tree pending_blocks;
static GTY (()) tree current_stmts;
static GTY (()) tree pending_stmts;
static GTY (()) tree pending_inits;

#include "m3gty43.h"

#if !GCC45
static bool m3_mark_addressable (tree exp);
#endif
static tree m3_type_for_size (unsigned precision, int unsignedp);
static tree m3_type_for_mode (enum machine_mode, int unsignedp);
static tree m3_unsigned_type (tree type_node);
static tree m3_signed_type (tree type_node);
static tree m3_signed_or_unsigned_type (int unsignedp, tree type);
static unsigned int m3_init_options (unsigned int argc, const char **argv);
static int m3_handle_option (size_t scode, const char *arg, int value);
static bool m3_post_options (const char **);
static bool m3_init (void);
static void m3_parse_file (int);
static alias_set_type m3_get_alias_set (tree);
static void m3_finish (void);
static bool m3_volatize_p (void);
static void m3_volatilize_decl (tree decl);

static bool m3_next_store_volatile;

/* Functions to keep track of the current scope */
static tree pushdecl (tree decl);

/* Langhooks.  */
static tree builtin_function (const char *name, tree type, int function_code,
                              enum built_in_class class,
                              const char *library_name,
                              tree attrs);
static tree getdecls (void);
static int global_bindings_p (void);
#if !GCC45
static void insert_block (tree block);
#endif

static void m3_push_type_decl (tree, tree);
static void m3_write_globals (void);

/* The front end language hooks (addresses of code for this front
   end).  These are not really very language-dependent, i.e.
   treelang, C, Mercury, etc. can all use almost the same definitions.  */

#if !GCC45
#undef LANG_HOOKS_MARK_ADDRESSABLE
#define LANG_HOOKS_MARK_ADDRESSABLE m3_mark_addressable
#endif
#undef LANG_HOOKS_SIGNED_TYPE
#define LANG_HOOKS_SIGNED_TYPE m3_signed_type
#undef LANG_HOOKS_UNSIGNED_TYPE
#define LANG_HOOKS_UNSIGNED_TYPE m3_unsigned_type
#undef LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE
#define LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE m3_signed_or_unsigned_type
#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE m3_type_for_mode
#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE m3_type_for_size
#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE m3_parse_file
#undef LANG_HOOKS_GET_ALIAS_SET
#define LANG_HOOKS_GET_ALIAS_SET m3_get_alias_set

#undef LANG_HOOKS_WRITE_GLOBALS
#define LANG_HOOKS_WRITE_GLOBALS m3_write_globals

#if GCC42
static void
m3_expand_function (tree fndecl)
{
  /* We have nothing special to do while expanding functions for Modula-3.  */
  tree_rest_of_compilation (fndecl);
}
#undef LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION
#define LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION m3_expand_function
#endif

/* Hook routines and data unique to Modula-3 back-end.  */

#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT m3_init
#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "Modula-3 backend"
#undef LANG_HOOKS_FINISH
#define LANG_HOOKS_FINISH m3_finish
#undef LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS  m3_init_options
#undef LANG_HOOKS_HANDLE_OPTION
#define LANG_HOOKS_HANDLE_OPTION m3_handle_option
#undef LANG_HOOKS_POST_OPTIONS
#define LANG_HOOKS_POST_OPTIONS m3_post_options
#if !GCC45
const
#endif
struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#if !GCC45

/* Tree code type/name/code tables.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

const enum tree_code_class tree_code_type[] = {
#include "tree.def"
  tcc_exceptional
};
#undef DEFTREECODE

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

const unsigned char tree_code_length[] = {
#include "tree.def"
  0
};
#undef DEFTREECODE

#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

const char *const tree_code_name[] = {
#include "tree.def"
  "@@dummy"
};
#undef DEFTREECODE

#endif

static tree
m3_convert (tree tipe, tree op0)
{
  return fold_build1 (CONVERT_EXPR, tipe, op0);
}

static tree
m3_cast (tree tipe, tree op0)
{
  return fold_build1 (NOP_EXPR, tipe, op0);
}

static tree
m3_build1 (enum tree_code code, tree tipe, tree op0)
{
  return fold_build1 (code, tipe, op0);
}

static tree
m3_build2 (enum tree_code code, tree tipe, tree op0, tree op1)
{
  return fold_build2 (code, tipe, op0, op1);
}

static tree
m3_build3 (enum tree_code code, tree tipe, tree op0, tree op1, tree op2)
{
  return fold_build3 (code, tipe, op0, op1, op2);
}

static tree
m3_build_pointer_type (tree a)
{
  a = build_pointer_type(a);
  /*DECL_NO_TBAA_P (a) = true;*/
  return a;
}

static tree
m3_build_type (m3_type t, int s, int a)
{
  switch (t)
    {
    case T_word:
    case T_longword:
      switch (s)
        {
        case 0:  return ((t == T_word) ? t_word : t_longword);
        case 8:  return t_word_8;
        case 16: return t_word_16;
        case 32: return t_word_32;
        case 64: return t_word_64;
        default: if (s == BITS_PER_INTEGER) return t_word;
        }
      break;

    case T_int:
    case T_longint:
      switch (s)
        {
        case 0:  return ((t == T_int) ? t_int : t_longint);
        case 8:  return t_int_8;
        case 16: return t_int_16;
        case 32: return t_int_32;
        case 64: return t_int_64;
        default: if (s == BITS_PER_INTEGER) return t_int;
        }
      break;

    case T_addr:    return t_addr;
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

    case T_struct:
      {
        tree ts = make_node (RECORD_TYPE);
        TYPE_NAME (ts) = NULL_TREE;
        TYPE_FIELDS (ts) = NULL_TREE;
        TYPE_SIZE (ts) = bitsize_int (s);
        TYPE_SIZE_UNIT (ts) = size_int (s / BITS_PER_UNIT);
        TYPE_ALIGN (ts) = a;

        compute_record_mode (ts);
        return ts;
      }
    default:
      break;
    } /*switch*/

  gcc_unreachable ();
}

/*========================================== insert, shift, rotate and co ===*/

static tree
m3_do_insert (tree x, tree y, tree i, tree n, tree t)
{
  tree a, b, c, d, e, f, g, h, j, k, l;

  t = m3_unsigned_type (t);
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
                 m3_build2 (EQ_EXPR, boolean_type_node, n, TYPE_SIZE (t)),
                 y, j);
  l = m3_build3 (COND_EXPR, t,
                 m3_build2 (EQ_EXPR, boolean_type_node, n, v_zero),
                 x, k);
  return l;
}

static tree
left_shift (tree t, int i)
{
  if (i)
    t = m3_build2 (LSHIFT_EXPR,
                   m3_unsigned_type (TREE_TYPE (t)), t,
                   build_int_cst (t_int, i));
  return t;
}

static tree
m3_do_fixed_insert (tree x, tree y, int i, int n, tree t)
{
  /* ??? Use BIT_FIELD_REF ??? */

  gcc_assert (i >= 0);
  gcc_assert (n >= 0);
  gcc_assert (i <= 64);
  gcc_assert (n <= 64);
  gcc_assert ((i + n) <= 64);
  gcc_assert (i <= TYPE_PRECISION (t));
  gcc_assert (n <= TYPE_PRECISION (t));
  gcc_assert ((i + n) <= TYPE_PRECISION (t));

  if ((i < 0) || (i >= TYPE_PRECISION (t)) ||
      (n < 0) || (n >= TYPE_PRECISION (t)))
    {
      return m3_do_insert (x, y,
                           build_int_cst (t_int, i),
                           build_int_cst (t_int, n), t);
    }

  if (n == 0)
    return x;

  if ((n == 1) && (i < HOST_BITS_PER_WIDE_INT))
    {
      if (host_integerp (y, 0))
        {
          if (TREE_INT_CST_LOW (y) & 1)
            {
              return m3_build2 (BIT_IOR_EXPR, t, x,
                                build_int_cstu (t, (HOST_WIDE_INT)1 << i));
            }
          else
            {
              return m3_build2 (BIT_AND_EXPR, t, x,
                                m3_build1 (BIT_NOT_EXPR, t,
                                           build_int_cstu (t, (HOST_WIDE_INT)1 << i)));
            }
        }
      else
        {                       /* non-constant, 1-bit value */
          tree a, b;
          a = m3_build2 (BIT_AND_EXPR, t, y, v_one);
          b = m3_build2 (BIT_AND_EXPR, t, x,
                         m3_build1 (BIT_NOT_EXPR, t,
                                    build_int_cstu (t, (HOST_WIDE_INT)1 << i)));
          return m3_build2 (BIT_IOR_EXPR, t, b, left_shift (a, i));
        }
    }
  else
    {                           /* multi-bit value */
      tree saved_bits, new_bits;
      if (i + n < HOST_BITS_PER_WIDE_INT)
        {
          HOST_WIDE_INT mask = ((HOST_WIDE_INT)1 << n) - 1;
          saved_bits = m3_build1 (BIT_NOT_EXPR, t,
                                  build_int_cstu (t, mask << i));
          if (host_integerp (y, 0))
            {
              new_bits = build_int_cstu (t, (TREE_INT_CST_LOW (y) & mask) << i);
            }
          else
            {
              new_bits = m3_build2 (BIT_AND_EXPR, t, y,
                                    build_int_cstu (t, mask));
              new_bits = left_shift (new_bits, i);
            }
        }
      else if (n < HOST_BITS_PER_WIDE_INT)
        {
          HOST_WIDE_INT mask = ((HOST_WIDE_INT)1 << n) - 1;
          tree a = build_int_cstu (t, mask);
          if (host_integerp (y, 0))
            {
              new_bits = build_int_cstu (t, TREE_INT_CST_LOW (y) & mask);
            }
          else
            {
              new_bits = m3_build2 (BIT_AND_EXPR, t, y, a);
            }
          new_bits = left_shift (new_bits, i);
          saved_bits = m3_build1 (BIT_NOT_EXPR, t, left_shift (a, i));
        }
      else
        {                       /* n >= sizeof(int)*8 */
          tree mask;
          
          gcc_assert (false);

          mask = m3_build2 (LSHIFT_EXPR, t, build_int_cst (t, ~0),
                            build_int_cst (t_int, n));
          mask = m3_build1 (BIT_NOT_EXPR, t, mask);
          new_bits = left_shift (m3_build2 (BIT_AND_EXPR, t, y, mask), i);
          saved_bits = m3_build1 (BIT_NOT_EXPR, t, left_shift (mask, i));
        }
      x = m3_build2 (BIT_AND_EXPR, t, x, saved_bits);
      return m3_build2 (BIT_IOR_EXPR, t, x, new_bits);
    }
}

static tree
m3_do_extract (tree x, tree i, tree n, tree t)
{
  tree a, b, c, d, e, f;

  a = m3_build2 (MINUS_EXPR, t_int, TYPE_SIZE (t), n);
  b = m3_build2 (MINUS_EXPR, t_int, a, i);
  c = m3_convert (m3_unsigned_type (t), x);
  d = m3_build2 (LSHIFT_EXPR, m3_unsigned_type (t), c, b);
  e = m3_build2 (RSHIFT_EXPR, t, d, a);
  f = m3_build3 (COND_EXPR, t,
                 m3_build2 (EQ_EXPR, boolean_type_node, n, v_zero),
                 v_zero, e);
  return f;
}

static tree
m3_do_rotate (enum tree_code code, tree t, tree val, tree cnt)
{
  /* ??? Use LROTATE_EXPR/RROTATE_EXPR.  */
  tree a, b, c, d, e, f, g;

  t = m3_unsigned_type (t);
  a = build_int_cst (t_int, TYPE_PRECISION (t) - 1);
  b = m3_build2 (BIT_AND_EXPR, t_int, cnt, a);
  c = m3_build2 (MINUS_EXPR, t_int, TYPE_SIZE(t), b);
  d = m3_convert (t, val);
  e = m3_build2 (LSHIFT_EXPR, t, d, (code == LROTATE_EXPR) ? b : c);
  f = m3_build2 (RSHIFT_EXPR, t, d, (code == RROTATE_EXPR) ? b : c);
  g = m3_build2 (BIT_IOR_EXPR, t, e, f);
  return g;
}

static tree
m3_do_shift (enum tree_code code, tree t, tree val, tree cnt)
{
  tree a, b, c, d;

  t = m3_unsigned_type (t);
  a = m3_convert (t, val);
  b = m3_build2 (code, t, a, cnt);
  if (host_integerp (cnt, 1)
      && (TREE_INT_CST_LOW (cnt) < TYPE_PRECISION (t)))
    {
      return b;
    }
  c = m3_build2 (GE_EXPR, boolean_type_node, cnt, TYPE_SIZE(t));
  d = m3_build3 (COND_EXPR, t, c, v_zero, b);
  return d;
}

alias_set_type
m3_get_alias_set (tree ARG_UNUSED (t))
{
  return 0;
}

#if !GCC45

/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   Value is 1 if successful.

   This implementation was copied from c-decl.c. */

static bool
m3_mark_addressable (tree exp)
{
  register tree x = exp;
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
        TREE_ADDRESSABLE (x) = 1;
        return true;

      default:
        return true;
      }
}

#endif

/* Return an integer type with the number of bits of precision given by
   PRECISION.  UNSIGNEDP is nonzero if the type is unsigned; otherwise
   it is a signed type.  */

static tree
m3_type_for_size (unsigned bits, int unsignedp)
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

  return 0;
}

/* Return a data type that has machine mode MODE.  UNSIGNEDP selects
   an unsigned type; otherwise a signed type is returned.  */

static tree
m3_type_for_mode (enum machine_mode mode, int unsignedp)
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
  if (mode ==  TImode) return unsignedp ? unsigned_intTI_type_node : intDI_type_node;

  return 0;
}

/* Return the unsigned version of a TYPE_NODE, a scalar type.  */

static tree
m3_unsigned_type (tree type_node)
{
  return m3_signed_or_unsigned_type (1, type_node);
}

/* Return the signed version of a TYPE_NODE, a scalar type.  */

static tree
m3_signed_type (tree type_node)
{
  return m3_signed_or_unsigned_type (0, type_node);
}

/* Return a type the same as TYPE except unsigned or signed according to
   UNSIGNEDP.  */

static tree
m3_signed_or_unsigned_type (int unsignedp, tree type)
{
  if (! INTEGRAL_TYPE_P (type) || TYPE_UNSIGNED (type) == unsignedp)
    return type;
  else
    return m3_type_for_size (TYPE_PRECISION (type), unsignedp);
}

/* Return non-zero if we are currently in the global binding level.  */

static int
global_bindings_p (void)
{
  return current_block == 0;
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
  TREE_USED (block) = 1;
  BLOCK_SUBBLOCKS (current_block)
    = chainon (BLOCK_SUBBLOCKS (current_block), block);
  BLOCK_SUPERCONTEXT(block) = current_block;
}

#endif

/* Records a ..._DECL node DECL as belonging to the current lexical scope.
   Returns the ..._DECL node. */

static tree
pushdecl (tree decl)
{
  gcc_assert (current_block == NULL_TREE);
  gcc_assert (current_function_decl == NULL_TREE);
  DECL_CONTEXT (decl) = 0;
  TREE_CHAIN (decl) = global_decls;
  global_decls = decl;
  return decl;
}


static void
m3_push_type_decl(tree id, tree type_node)
{
  tree decl = build_decl (TYPE_DECL, id, type_node);
  TYPE_NAME (type_node) = id;
  TREE_CHAIN (decl) = global_decls;
  global_decls = decl;
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

static GTY((param_is (union tree_node))) htab_t builtins;

static hashval_t
htab_hash_builtin (const PTR p)
{
  const_tree t = (const_tree)p;

  return htab_hash_pointer(DECL_NAME(t));
}

static int
htab_eq_builtin (const PTR p1, const PTR p2)
{
  const_tree t1 = (const_tree)p1;
  const_tree t2 = (const_tree)p2;

  return DECL_NAME(t1) == DECL_NAME(t2);
}

static tree
builtin_function (const char *name, tree type, int function_code,
                  enum built_in_class class, const char *library_name,
                  tree attrs ATTRIBUTE_UNUSED)
{
  tree id = get_identifier (name);
  tree decl = build_decl (FUNCTION_DECL, id, type);
  TREE_PUBLIC (decl) = 1;
  DECL_EXTERNAL (decl) = 1;
  DECL_BUILT_IN_CLASS (decl) = class;
  DECL_FUNCTION_CODE (decl) = function_code;
  if (library_name)
    SET_DECL_ASSEMBLER_NAME (decl, get_identifier (library_name));

  {
    tree *slot;
    if (!builtins)
      builtins = htab_create_ggc (1021, htab_hash_builtin,
                                  htab_eq_builtin, NULL);
    slot = (tree *)htab_find_slot (builtins, decl, INSERT);
    gcc_assert (*slot == NULL);
    *slot = decl;
  }

  TREE_CHAIN (decl) = global_decls;
  global_decls = decl;

  return decl;
}

static void
m3_write_globals (void)
{
  tree ctors;

  /* Fix init_offset fields in constructors: VAR_DECL -> offset */
  for (ctors = pending_inits; ctors; ctors = TREE_CHAIN(ctors)) {
    VEC(constructor_elt,gc) *elts = CONSTRUCTOR_ELTS (TREE_VALUE (ctors));
    unsigned HOST_WIDE_INT idx;
    tree index, value;

    FOR_EACH_CONSTRUCTOR_ELT (elts, idx, index, value) {
      tree var = (tree)DECL_LANG_SPECIFIC(index);
      if (var) {
        gcc_assert(TREE_CODE(var) == VAR_DECL);
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
          if (REG_P (r)) {
            j = 0;
          } else {
            r = XEXP (r, 1);    /* (const_int ...) */
            j = XWINT (r, 0);  /* offset */
          }
          VEC_index (constructor_elt, elts, idx)->value = size_int (j);
        }
        else
        {
          /*fprintf(stderr, "%s %s not addressable, should be?\n",
                  input_filename, IDENTIFIER_POINTER(DECL_NAME(var)));*/
        }
      }
    }
  }

  if (!GCC45)
    write_global_declarations ();
}

static void
sync_builtin (enum built_in_function fncode, tree type, const char *name)
{
  tree decl = builtin_function (name, type, fncode, BUILT_IN_NORMAL, NULL,
                                NULL_TREE);
  TREE_NOTHROW (decl) = 1;
  built_in_decls[fncode] = implicit_built_in_decls[fncode] = decl;
}

/* Create the predefined scalar types of M3CG,
   and some nodes representing standard constants (0, 1, (void *) 0).
   Initialize the global binding level.
   Make definitions for built-in primitive functions.  */

static void
m3_init_decl_processing (void)
{
  tree t;

  current_function_decl = NULL;

  build_common_tree_nodes (0, false);

  t_int_8 = intQI_type_node;
  m3_push_type_decl (get_identifier ("int_8"), t_int_8);
  t_word_8 = unsigned_intQI_type_node;
  m3_push_type_decl (get_identifier ("word_8"), t_word_8);
  t_int_16 = intHI_type_node;
  m3_push_type_decl (get_identifier ("int_16"), t_int_16);
  t_word_16 = unsigned_intHI_type_node;
  m3_push_type_decl (get_identifier ("word_16"), t_word_16);
  t_int_32 = intSI_type_node;
  m3_push_type_decl (get_identifier ("int_32"), t_int_32);
  t_word_32 = unsigned_intSI_type_node;
  m3_push_type_decl (get_identifier ("word_32"), t_word_32);
  t_int_64 = intDI_type_node;
  m3_push_type_decl (get_identifier ("int_64"), t_int_64);
  t_word_64 = unsigned_intDI_type_node;
  m3_push_type_decl (get_identifier ("word_64"), t_word_64);

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
      m3_push_type_decl (get_identifier ("int"), t_int);
      t_word = make_unsigned_type (BITS_PER_INTEGER);
      m3_push_type_decl (get_identifier ("word"), t_word);
    }

  t_longint = t_int_64;
  t_longword = t_word_64;

  /* Set the type used for sizes and build the remaining common nodes. */
  size_type_node = t_word;
  set_sizetype (size_type_node);
  build_common_tree_nodes_2 (0);
  void_list_node = build_tree_list (NULL_TREE, void_type_node);

  /* Build the remaining M3-specific type and value nodes. */
  t_addr = ptr_type_node;
  m3_push_type_decl (get_identifier ("addr"), t_addr);
  t_reel = float_type_node;
  m3_push_type_decl (get_identifier ("reel"), t_reel);
  t_lreel = double_type_node;
  m3_push_type_decl (get_identifier ("lreel"), t_lreel);
#if 0
  /* XXX The M3 front end (m3middle/src/Target.m3) seems to treat extended
     reals the same as LONGREAL.  That may be due to limitations in other
     parts of the front end.  I don't know yet.  For now we likewise treat
     the xreel type as if it were lreel. */
  t_xreel = long_double_type_node;
#else
  t_xreel = double_type_node;
#endif
  m3_push_type_decl (get_identifier ("xreel"), t_xreel);

  t_void = void_type_node;
  v_zero = build_int_cst (t_int, 0);
  v_one = build_int_cst (t_int, 1);
  v_null = null_pointer_node;

  build_common_builtin_nodes ();

  targetm.init_builtins ();

  t = t_int_8;
  t = build_function_type_list (t, t_addr, t, NULL_TREE);
  sync_builtin (BUILT_IN_FETCH_AND_ADD_1,  t, "__sync_fetch_and_add_1");
  sync_builtin (BUILT_IN_FETCH_AND_SUB_1,  t, "__sync_fetch_and_sub_1");
  sync_builtin (BUILT_IN_FETCH_AND_OR_1,   t, "__sync_fetch_and_or_1");
  sync_builtin (BUILT_IN_FETCH_AND_AND_1,  t, "__sync_fetch_and_and_1");
  sync_builtin (BUILT_IN_FETCH_AND_XOR_1,  t, "__sync_fetch_and_xor_1");
  sync_builtin (BUILT_IN_FETCH_AND_NAND_1, t, "__sync_fetch_and_nand_1");
  sync_builtin (BUILT_IN_ADD_AND_FETCH_1,  t, "__sync_add_and_fetch_1");
  sync_builtin (BUILT_IN_SUB_AND_FETCH_1,  t, "__sync_sub_and_fetch_1");
  sync_builtin (BUILT_IN_OR_AND_FETCH_1,   t, "__sync_or_and_fetch_1");
  sync_builtin (BUILT_IN_AND_AND_FETCH_1,  t, "__sync_and_and_fetch_1");
  sync_builtin (BUILT_IN_XOR_AND_FETCH_1,  t, "__sync_xor_and_fetch_1");
  sync_builtin (BUILT_IN_NAND_AND_FETCH_1, t, "__sync_nand_and_fetch_1");

  t = t_int_16;
  t = build_function_type_list (t, t_addr, t, NULL_TREE);
  sync_builtin (BUILT_IN_FETCH_AND_ADD_2,  t, "__sync_fetch_and_add_2");
  sync_builtin (BUILT_IN_FETCH_AND_SUB_2,  t, "__sync_fetch_and_sub_2");
  sync_builtin (BUILT_IN_FETCH_AND_OR_2,   t, "__sync_fetch_and_or_2");
  sync_builtin (BUILT_IN_FETCH_AND_AND_2,  t, "__sync_fetch_and_and_2");
  sync_builtin (BUILT_IN_FETCH_AND_XOR_2,  t, "__sync_fetch_and_xor_2");
  sync_builtin (BUILT_IN_FETCH_AND_NAND_2, t, "__sync_fetch_and_nand_2");
  sync_builtin (BUILT_IN_ADD_AND_FETCH_2,  t, "__sync_add_and_fetch_2");
  sync_builtin (BUILT_IN_SUB_AND_FETCH_2,  t, "__sync_sub_and_fetch_2");
  sync_builtin (BUILT_IN_OR_AND_FETCH_2,   t, "__sync_or_and_fetch_2");
  sync_builtin (BUILT_IN_AND_AND_FETCH_2,  t, "__sync_and_and_fetch_2");
  sync_builtin (BUILT_IN_XOR_AND_FETCH_2,  t, "__sync_xor_and_fetch_2");
  sync_builtin (BUILT_IN_NAND_AND_FETCH_2, t, "__sync_nand_and_fetch_2");

  t = t_int_32;
  t = build_function_type_list (t, t_addr, t, NULL_TREE);
  sync_builtin (BUILT_IN_FETCH_AND_ADD_4,  t, "__sync_fetch_and_add_4");
  sync_builtin (BUILT_IN_FETCH_AND_SUB_4,  t, "__sync_fetch_and_sub_4");
  sync_builtin (BUILT_IN_FETCH_AND_OR_4,   t, "__sync_fetch_and_or_4");
  sync_builtin (BUILT_IN_FETCH_AND_AND_4,  t, "__sync_fetch_and_and_4");
  sync_builtin (BUILT_IN_FETCH_AND_XOR_4,  t, "__sync_fetch_and_xor_4");
  sync_builtin (BUILT_IN_FETCH_AND_NAND_4, t, "__sync_fetch_and_nand_4");
  sync_builtin (BUILT_IN_ADD_AND_FETCH_4,  t, "__sync_add_and_fetch_4");
  sync_builtin (BUILT_IN_SUB_AND_FETCH_4,  t, "__sync_sub_and_fetch_4");
  sync_builtin (BUILT_IN_OR_AND_FETCH_4,   t, "__sync_or_and_fetch_4");
  sync_builtin (BUILT_IN_AND_AND_FETCH_4,  t, "__sync_and_and_fetch_4");
  sync_builtin (BUILT_IN_XOR_AND_FETCH_4,  t, "__sync_xor_and_fetch_4");
  sync_builtin (BUILT_IN_NAND_AND_FETCH_4, t, "__sync_nand_and_fetch_4");

  t = t_int_64;
  t = build_function_type_list (t, t_addr, t, NULL_TREE);
  sync_builtin (BUILT_IN_FETCH_AND_ADD_8,  t, "__sync_fetch_and_add_8");
  sync_builtin (BUILT_IN_FETCH_AND_SUB_8,  t, "__sync_fetch_and_sub_8");
  sync_builtin (BUILT_IN_FETCH_AND_OR_8,   t, "__sync_fetch_and_or_8");
  sync_builtin (BUILT_IN_FETCH_AND_AND_8,  t, "__sync_fetch_and_and_8");
  sync_builtin (BUILT_IN_FETCH_AND_XOR_8,  t, "__sync_fetch_and_xor_8");
  sync_builtin (BUILT_IN_FETCH_AND_NAND_8, t, "__sync_fetch_and_nand_8");
  sync_builtin (BUILT_IN_ADD_AND_FETCH_8,  t, "__sync_add_and_fetch_8");
  sync_builtin (BUILT_IN_SUB_AND_FETCH_8,  t, "__sync_sub_and_fetch_8");
  sync_builtin (BUILT_IN_OR_AND_FETCH_8,   t, "__sync_or_and_fetch_8");
  sync_builtin (BUILT_IN_AND_AND_FETCH_8,  t, "__sync_and_and_fetch_8");
  sync_builtin (BUILT_IN_XOR_AND_FETCH_8,  t, "__sync_xor_and_fetch_8");
  sync_builtin (BUILT_IN_NAND_AND_FETCH_8, t, "__sync_nand_and_fetch_8");

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

#if 0
  t = build_function_type_list (t_addr, t_addr, t_addr, t_int, NULL_TREE);
  memcpy_proc = builtin_function ("memcpy", t, BUILT_IN_MEMCPY,
                                  BUILT_IN_NORMAL, NULL, NULL_TREE);
  memmove_proc = builtin_function ("memmove", t, BUILT_IN_MEMMOVE,
                                   BUILT_IN_NORMAL, NULL, NULL_TREE);

  t = build_function_type_list (t_addr, t_addr, t_int, t_int, NULL_TREE);
  memset_proc = builtin_function ("memset", t, BUILT_IN_MEMSET,
                                  BUILT_IN_NORMAL, NULL, NULL_TREE);
#else
  memcpy_proc = built_in_decls[BUILT_IN_MEMCPY];
  memmove_proc = built_in_decls[BUILT_IN_MEMMOVE];
  memset_proc = built_in_decls[BUILT_IN_MEMSET];
  memcmp_proc = built_in_decls[BUILT_IN_MEMCMP];
#endif

#if 0
  t = build_function_type_list (t_int, t_int, t_int, NULL_TREE);
  div_proc = builtin_function ("m3_div", t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  mod_proc = builtin_function ("m3_mod", t, 0, NOT_BUILT_IN, NULL, NULL_TREE);

  t = build_function_type_list (t_int_64, t_int_64, t_int_64, NULL_TREE);
  divL_proc = builtin_function ("m3_divL", t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  modL_proc = builtin_function ("m3_modL", t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
#endif

  t = build_function_type_list (t_void, NULL_TREE);
  set_union_proc  = builtin_function ("set_union",
                                      t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_diff_proc   = builtin_function ("set_difference",
                                      t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_inter_proc  = builtin_function ("set_intersection",
                                      t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_sdiff_proc  = builtin_function ("set_sym_difference",
                                      t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_range_proc  = builtin_function ("set_range",
                                      t, 0, NOT_BUILT_IN, NULL, NULL_TREE);

  t = build_function_type_list (t_int, NULL_TREE);
  set_gt_proc
    = builtin_function ("set_gt", t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_ge_proc
    = builtin_function ("set_ge", t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_lt_proc
    = builtin_function ("set_lt", t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_le_proc
    = builtin_function ("set_le", t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
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

#define STREQ(a, b) (((a)[0] == (b)[0]) ? (strcmp ((a), (b)) == 0) : 0)

/* Variable arrays of trees. */

static GTY (()) varray_type all_vars;
static GTY (()) varray_type all_procs;
static GTY (()) varray_type all_labels;
static GTY (()) varray_type expr_stack;
static GTY (()) varray_type call_stack;

#define STACK_PUSH(stk, x)      VARRAY_PUSH_TREE (stk, (x))
#define STACK_POP(stk)          VARRAY_POP (stk)
#define STACK_REF(stk, n)       ((&VARRAY_TOP_TREE (stk) + 1)[(n)])

#define EXPR_PUSH(x)    STACK_PUSH (expr_stack, (x))
#define EXPR_POP()      STACK_POP (expr_stack)
#define EXPR_REF(n)     STACK_REF (expr_stack, (n))

/* The call stack has triples on it: first the argument chain, then
   the type chain, then the static chain expression. */
#define CALL_PUSH(a, t, s)              \
    do                                  \
      {                                 \
        STACK_PUSH (call_stack, (a));   \
        STACK_PUSH (call_stack, (t));   \
        STACK_PUSH (call_stack, (s));   \
      }                                 \
    while (0)

#define CALL_POP()                      \
    do                                  \
      {                                 \
        STACK_POP (call_stack);         \
        STACK_POP (call_stack);         \
        STACK_POP (call_stack);         \
      }                                 \
    while (0)

#define CALL_TOP_ARG()          STACK_REF (call_stack, -3)
#define CALL_TOP_TYPE()         STACK_REF (call_stack, -2)
#define CALL_TOP_STATIC_CHAIN() STACK_REF (call_stack, -1)

/*======================================================= OPTION HANDLING ===*/

static int option_trace_all;
static int option_opcodes_trace;
static int option_source_line_trace;
static int option_vars_trace;
static int option_procs_trace;
static int option_exprs_trace;
static int option_misc_trace;
static int option_types_trace;

/*=============================================================== PARSING ===*/

#define BUFFER_SIZE 0x10000

static unsigned char input_buffer[BUFFER_SIZE];
static int  input_len;
static int  input_cursor;
static int  input_eof;
static int  m3cg_lineno;

/* Stream for reading from the input file.  */
FILE *finput;

/*-------------------------------------------------------- buffer loading ---*/

static void
m3_init_parse (void)
{
  VARRAY_TREE_INIT (all_vars, 100, "all_vars");
  VARRAY_TREE_INIT (all_procs, 100, "all_procs");
  VARRAY_TREE_INIT (all_labels, 100, "all_labels");
  VARRAY_TREE_INIT (expr_stack, 100, "expr_stack");
  VARRAY_TREE_INIT (call_stack, 100 * 2, "call_stack");
}

static void
reload_buffer (void)
{
  gcc_assert(finput);
  input_len = fread (input_buffer, 1, BUFFER_SIZE, finput);
  input_cursor = 0;
  input_eof = (input_len <= 0);
}

static void
m3_init_lex (void)
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
  }
  return (long)(input_buffer[input_cursor++] & 0xff);
}


#define INTEGER(x) long x = get_int()
#define UNUSED_INTEGER(x) long x ATTRIBUTE_UNUSED = get_int()
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

/*---------------------------------------------------- calling convention ---*/

#define CC(x) tree x = scan_cc ()
#define UNUSED_CC(x) tree x ATTRIBUTE_UNUSED = scan_cc ()
static tree
scan_cc (void)
{
  int id = get_int ();

  switch (id) {
  case 0: return NULL_TREE;
  case 1: return build_tree_list (get_identifier ("stdcall"), NULL);
  default:
    fatal_error(" *** invalid calling convention: 0x%x, at m3cg_lineno %d",
                id, m3cg_lineno);
  }
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

#define IS_INTEGER_TYPE_TREE(t) (t == t_int_32 || t == t_int_8 \
                || t == t_int_16 || t == t_int_64 || t == t_int)

#define IS_WORD_TYPE_TREE(t) (t == t_word_32 || t == t_word_8 \
                || t == t_word_16 || t == t_word_64 || t == t_word)

#define IS_REAL_TYPE(t) (t == T_reel || t == T_lreel || t == T_xreel)

#define TYPE(x) m3_type x = scan_type ()
#define UNUSED_TYPE(x) m3_type x ATTRIBUTE_UNUSED = scan_type ()
static m3_type
scan_type (void)
{
  long i = get_int ();

  if ((i < 0) || (T_LAST <= i))
    fatal_error (" *** illegal type: 0x%lx, at m3cg_lineno %d", i, m3cg_lineno);

  if (option_trace_all)
    fprintf(stderr, "  type:%s\n", m3cg_typename(i));

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
#define UNUSED_SIGN(x) char x ATTRIBUTE_UNUSED = scan_sign ()
 
static char
scan_sign (void)
{
  long x = get_int ();
  switch (x) {
  case 0:  return 'P';  /* positive */
  case 1:  return 'N';  /* negative */
  case 2:  return 'U';  /* unknown */
  default:
    fatal_error(" *** bad sign: 0x%lx, at m3cg_lineno %d", x, m3cg_lineno);
  }
  return '0';
}

/*-------------------------------------------------------------- integers ---*/

#define TARGET_INTEGER(x) tree x = scan_target_int ()

static tree
scan_target_int (void)
{
  unsigned HOST_WIDE_INT low;
  HOST_WIDE_INT hi;
  long i, n_bytes, original_n_bytes = { 0 }, sign, shift;
  tree res, t = long_long_integer_type_node;

  i = (long) get_byte ();
  switch (i) {
  case M3CG_Int1:   n_bytes = 1;  sign =  1;  break;
  case M3CG_NInt1:  n_bytes = 1;  sign = -1;  break;
  case M3CG_Int2:   n_bytes = 2;  sign =  1;  break;
  case M3CG_NInt2:  n_bytes = 2;  sign = -1;  break;
  case M3CG_Int4:   n_bytes = 4;  sign =  1;  break;
  case M3CG_NInt4:  n_bytes = 4;  sign = -1;  break;
  case M3CG_Int8:   n_bytes = 8;  sign =  1;  break;
  case M3CG_NInt8:  n_bytes = 8;  sign = -1;  break;
  default:

    if (option_trace_all)
      fprintf(stderr,"  integer:0x%lx\n", (unsigned long)i);

    return build_int_cst (t, i);
  }

  if (option_trace_all)
    original_n_bytes = n_bytes;

  hi = low = 0;
  for (shift = 0; n_bytes > 0;  n_bytes--, shift += 8) {
    if (shift < HOST_BITS_PER_WIDE_INT) {
      low = low | (((unsigned HOST_WIDE_INT) get_byte ()) << shift);
    } else {
      hi = hi | (((HOST_WIDE_INT) get_byte ()) << (shift - HOST_BITS_PER_WIDE_INT));
    }
  }

  res = build_int_cst_wide (t, low, hi);
  if (sign < 0) { res = m3_build1 (NEGATE_EXPR, t, res); }

  if (option_trace_all)
    fprintf(stderr,"  integer i:0x%lx n_bytes:0x%lx " HOST_WIDE_INT_PRINT_DOUBLE_HEX " sign %ld\n",
           (unsigned long)i, (unsigned long)original_n_bytes, hi, low, sign);

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

#define NO_UID (0xFFFFFFFFUL)

#define TYPEID(x)    unsigned long x = (0xFFFFFFFFUL & (unsigned long) get_int ())
#define UNUSED_TYPEID(x)    long x ATTRIBUTE_UNUSED = get_int ()

static void
fmt_uid (unsigned long x, char *buf)
{
  unsigned digit;
  int i;

  buf[UID_SIZE] = 0;
  if (x == NO_UID) { strcpy (buf, "zzzzzz");  return; }

  for (i = UID_SIZE-1; i >= 0; i--) {
    digit = (x % 62);
    x = (x / 62);
    if      (digit < 26) { buf[i] = 'A' + digit; }
    else if (digit < 52) { buf[i] = 'a' + (digit - 26); }
    else                 { buf[i] = '0' + (digit - 52); }
  }

  if ((x != 0) || (buf[0] < 'A') || ('Z' < buf[0])) {
    fatal_error (" *** bad uid -> identifier conversion!!"); }
}

/*----------------------------------------------------------------- float ---*/

static int IsHostBigEndian(void)
{
    union
    {
        int i;
        char c[sizeof(int)];
    } u;
    u.i = 1;
    return (u.c[0] == 0);
}

#define FLOAT(x, fkind)  unsigned fkind;  tree x = scan_float(&fkind)

static tree
scan_float (unsigned *out_Kind)
{
  /* real_from_target_fmt wants floats stored in an array of longs, 32 bits
     per long, even if long can hold more.  So for example a 64 bit double on
     a system with 64 bit long will have 32 bits of zeros in the middle. */
  long Longs[2] = { 0, 0 };
  unsigned char * const Bytes = (unsigned char*) &Longs;
  unsigned i = { 0 };
  unsigned Kind = { 0 };
  static const struct {
    tree* Tree;
    unsigned Size;
    const struct real_format* format;
  } Map[] = { { &t_reel ,  (FLOAT_TYPE_SIZE / 8), &ieee_single_format },
              { &t_lreel, (DOUBLE_TYPE_SIZE / 8), &ieee_double_format },
              { &t_xreel, (LONG_DOUBLE_TYPE_SIZE / 8), &ieee_double_format }};
  unsigned Size = { 0 };
  REAL_VALUE_TYPE val;

  gcc_assert (sizeof(float) == 4);
  gcc_assert (sizeof(double) == 8);
  gcc_assert (FLOAT_TYPE_SIZE == 32);
  gcc_assert (DOUBLE_TYPE_SIZE == 64);
  gcc_assert (LONG_DOUBLE_TYPE_SIZE == 64);
  gcc_assert ((sizeof(long) == 4) || (sizeof(long) == 8));

  Kind = (unsigned) get_int();
  if (Kind >= (sizeof(Map) / sizeof(Map[0])))
    {
      fatal_error(" *** invalid floating point value, precision = 0x%x, at m3cg_lineno %d",
                  Kind, m3cg_lineno);
    }
  *out_Kind = Kind;
  Size = Map[Kind].Size;
  
  gcc_assert ((Size == 4) || (Size == 8));

  /* read the value's bytes; each long holds 32 bits, even if long is larger
     than 32 bits always read the bytes in increasing address, independent of
     endianness */
  for (i = 0;  i < Size;  i++)
    {
      Bytes[i / 4 * sizeof(long) + i % 4] = (unsigned char) (0xFF & get_int ());
    }

  /* When crossing and host/target different endian, swap the longs. */

  if ((Size == 8) && (IsHostBigEndian() != FLOAT_WORDS_BIG_ENDIAN))
  {
      long t = Longs[0];
      Longs[0] = Longs[1];
      Longs[1] = t;
  }

  if (option_trace_all)
    {
      union
      {
        unsigned char Bytes[sizeof(long double)]; /* currently double suffices */
        float Float;
        double Double;
        long double LongDouble; /* not currently used */
      } u = { { 0 } };

      /* repack the bytes adjacent to each other */

      for (i = 0;  i < Size;  i++)
        {
          u.Bytes[i] = Bytes[i / 4 * sizeof(long) + i % 4];
        }
      if (Size == 4)
        {
          fprintf(stderr, " float:%f bytes:0x%02x%02x%02x%02x\n",
                  u.Float, u.Bytes[0], u.Bytes[1], u.Bytes[2], u.Bytes[3]);
        }
      else
        {
          fprintf(stderr, " double:%f bytes:0x%02x%02x%02x%02x%02x%02x%02x%02x\n",
                  u.Double,
                  u.Bytes[0], u.Bytes[1], u.Bytes[2], u.Bytes[3],
                  u.Bytes[4], u.Bytes[5], u.Bytes[6], u.Bytes[7]);
        }
    }

  /* finally, assemble a floating point value */
  real_from_target_fmt (&val, Longs, Map[Kind].format);
  return build_real (*Map[Kind].Tree, val);
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
          fatal_error ("*** variable should already exist, v.0x%x, line %d",
                       i, m3cg_lineno);
        }
    }
  else
    {
      if (VARRAY_TREE (all_vars, i) != NULL)
        {
          fatal_error ("*** variable should not already exist, v.0x%x, line %d",
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

  if (option_trace_all)
    fprintf(stderr, "  label %d\n", i);

  if (i < 0) { return 0; }
  VARRAY_EXTEND (all_labels, i + 1);
  if (VARRAY_TREE (all_labels, i) == NULL)
    VARRAY_TREE (all_labels, i) = build_decl (LABEL_DECL, NULL_TREE, t_void);
  return VARRAY_TREE (all_labels, i);
}


/*================================================= debugging information ===*/

static char current_dbg_type_tag [100];
static int current_dbg_type_count1;
static int current_dbg_type_count2;
static int current_dbg_type_count3;

static void
debug_tag (char kind, unsigned long id, ...)
{
  va_list args;
  char *fmt;

  va_start (args, id);

  current_dbg_type_tag [0] = 'M';
  current_dbg_type_tag [1] = kind;
  current_dbg_type_tag [2] = '_';
  fmt_uid (id, current_dbg_type_tag + 3);

  fmt = va_arg (args, char *);
  current_dbg_type_tag[sizeof(current_dbg_type_tag) - 2] = 0;
  vsnprintf (current_dbg_type_tag + UID_SIZE + 3,
             sizeof(current_dbg_type_tag) - (UID_SIZE + 3), fmt, args);
  if (current_dbg_type_tag[sizeof(current_dbg_type_tag) - 2])
  {
    current_dbg_type_tag[sizeof(current_dbg_type_tag) - 1] = 0;
    fatal_error("identifier too long (in debug_tag, %s)", current_dbg_type_tag);
  }
  va_end (args);
}

static void
debug_field (const char *name)
{
  tree f = build_decl (FIELD_DECL, get_identifier (name), t_int);

  DECL_FIELD_OFFSET (f) = size_zero_node;
  DECL_FIELD_BIT_OFFSET (f) = bitsize_zero_node;

  layout_decl (f, 1);

  TREE_CHAIN (f) = debug_fields;
  debug_fields = f;
}

static void
debug_field_id (unsigned long id)
{
  char buf [UID_SIZE+1];
  fmt_uid (id, buf);
  debug_field (buf);
}

static void
debug_field_fmt (unsigned long id, ...)
{
  va_list args;
  char name [100];
  char *fmt;

  va_start (args, id);

  fmt_uid (id, name);
  fmt = va_arg (args, char *);
  name[sizeof(name) - 2] = 0;
  vsnprintf (name + UID_SIZE, sizeof(name) - UID_SIZE, fmt, args);
  va_end (args);
  if (name[sizeof(name) - 2])
  {
    name[sizeof(name) - 1] = 0;
    fatal_error("identifier too long (in debug_field_fmt, %s)", name);
  }
  debug_field (name);
}

static void
debug_struct (void)
{
  tree d;
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
  SET_TYPE_MODE (t, QImode);

  d = build_decl (TYPE_DECL, NULL_TREE, t);
  TREE_CHAIN (d) = global_decls;
  global_decls = d;
  debug_hooks -> type_decl
    ( d, false /* This argument means "IsLocal", but it's unused by dbx. */ );
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
      fprintf (stderr, "  one_field: offset:0x%x\n", offset);

  if (offset > current_record_offset)
      one_gap (offset);

  *f = build_decl (FIELD_DECL, 0, tipe);
  layout_decl (*f, 1);
  DECL_FIELD_OFFSET (*f) = size_int (offset / BITS_PER_UNIT);
  DECL_FIELD_BIT_OFFSET (*f) = bitsize_int (offset % BITS_PER_UNIT);
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
  int gap = offset - current_record_offset;

  if (option_vars_trace)
      fprintf (stderr, "  one_gap: offset:0x%x, gap:0x%x\n", offset, gap);

  tipe = make_node (LANG_TYPE);
  TYPE_SIZE (tipe) = bitsize_int (gap);
  TYPE_SIZE_UNIT (tipe) = size_int (gap / BITS_PER_UNIT);
  TYPE_ALIGN (tipe) = BITS_PER_UNIT;
  one_field (current_record_offset, tipe, &f, &v);
  TREE_VALUE (v) = build_constructor (TREE_TYPE (f), 0);
}

/*========================================= SUPPORT FUNCTIONS FOR YYPARSE ===*/

static void add_stmt (tree t)
{
  enum tree_code code = TREE_CODE (t);

  if (EXPR_P (t) && code != LABEL_EXPR)
    {
      if (!EXPR_HAS_LOCATION (t))
        SET_EXPR_LOCATION (t, input_location);
    }

  TREE_USED (t) = 1;
  TREE_SIDE_EFFECTS (t) = 1;
  append_to_statement_list (t, &current_stmts);
}

static tree
fix_name (const char *name, unsigned long id)
{
  char buf[100];
  size_t len;

  if (name == 0 || name[0] == '*') {
    static int anonymous_counter = 1;
    snprintf (buf, sizeof(buf), "L_%d", anonymous_counter++);
  } else if (id == 0) {
    return get_identifier (name);
  } else if (id == NO_UID) {
    len = strlen(name);
    if (len >= (sizeof(buf) - 1))
      fatal_error("identifier too long (in fix_name, 1st case, %s)", name);
    buf[0] = 'M';
    memcpy(&buf[1], name, len + 1);
  } else {
    buf[0] = 'M';  buf[1] = '3';  buf[2] = '_';
    fmt_uid (id, buf + 3);
    buf[3 + UID_SIZE] = '_';
    len = strlen(name);
    if (len >= (sizeof(buf) - 4 - UID_SIZE))
      fatal_error("identifier too long (in fix_name, 2nd case, %s)", name);
    memcpy(&buf[4 + UID_SIZE], name, len + 1);
  }
  return get_identifier (buf);
}

static tree
declare_temp (tree t)
{
  tree v = build_decl (VAR_DECL, 0, t);

  DECL_UNSIGNED (v) = TYPE_UNSIGNED (t);
  DECL_CONTEXT (v) = current_function_decl;

  if (m3_volatize_p ())
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
  TREE_NO_TRAMPOLINE (expr) = 1;
#else
  TREE_STATIC (expr) = 1; /* see check for TREE_STATIC on ADDR_EXPR
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
m3_pop_param (tree t)
{
  CALL_TOP_ARG ()
    = chainon (CALL_TOP_ARG (),
               build_tree_list (NULL_TREE, EXPR_REF (-1)));
  CALL_TOP_TYPE ()
    = chainon (CALL_TOP_TYPE (),
               build_tree_list (NULL_TREE, t));
  EXPR_POP ();
}

#if GCC42

static void
m3_call_direct (tree p, tree t)
{
  tree call;

  call = fold_build3 (CALL_EXPR, t, proc_addr (p), CALL_TOP_ARG (),
                      CALL_TOP_STATIC_CHAIN ());
  if (t == t_void) {
    add_stmt (call);
  } else {
    TREE_SIDE_EFFECTS (call) = 1;
    EXPR_PUSH (call);
  }
  CALL_POP ();
}

static void
m3_call_indirect (tree t, tree cc ATTRIBUTE_UNUSED)
{
  tree argtypes = chainon (CALL_TOP_TYPE (),
                           tree_cons (NULL_TREE, t_void, NULL_TREE));
  tree fntype = m3_build_pointer_type (build_function_type (t, argtypes));
  tree call;
  tree fnaddr = EXPR_REF (-1);
  EXPR_POP ();

  call = build3 (CALL_EXPR, t, m3_cast (fntype, fnaddr), CALL_TOP_ARG (),
                 CALL_TOP_STATIC_CHAIN ());
  if (t == t_void) {
    add_stmt (call);
  } else {
    TREE_SIDE_EFFECTS (call) = 1;
    EXPR_PUSH (call);
  }
  CALL_POP ();
}

#else

static struct language_function*
m3_get_language_function (void)
{
    if (M3_ALL_VOLATILE)
        return 0;
    return DECL_STRUCT_FUNCTION(current_function_decl)->language;
}

static struct language_function*
m3_set_language_function (void)
{
    struct language_function* f;
    if (M3_ALL_VOLATILE)
        return 0;
    f = DECL_STRUCT_FUNCTION(current_function_decl)->language;
    if (!f)
    {
        f = GGC_NEW (struct language_function);
        DECL_STRUCT_FUNCTION(current_function_decl)->language = f;
    }
    return f;
}

static bool
m3_volatize_p (void)
{
    struct language_function* f;
    if (M3_ALL_VOLATILE)
        return true;
    f = m3_get_language_function();
    return (f && f->volatil);
}

static void
m3_set_volatize (void)
{
    if (!M3_ALL_VOLATILE)
      m3_set_language_function()->volatil = true;
}

static void
m3_volatilize_decl (tree decl)
{
  if (!TYPE_VOLATILE(TREE_TYPE(decl)) && !TREE_STATIC(decl)
      && (TREE_CODE(decl) == VAR_DECL
          || TREE_CODE(decl) == PARM_DECL)) {
    TREE_TYPE(decl) = build_qualified_type(TREE_TYPE(decl), TYPE_QUAL_VOLATILE);
    TREE_THIS_VOLATILE(decl) = 1;
    TREE_SIDE_EFFECTS(decl) = 1;
    DECL_REGISTER(decl) = 0;
  }
}

static void
m3_call_direct (tree p, tree t)
{
  tree call;
  tree *slot = (tree *)htab_find_slot (builtins, p, NO_INSERT);
  int flags;
  if (slot) p = *slot;
  if (TREE_USED (p) == 0)
  {
      TREE_USED (p) = 1;
      assemble_external (p);
  }
  call = build_call_list (t, proc_addr (p), CALL_TOP_ARG ());
  CALL_EXPR_STATIC_CHAIN (call) = CALL_TOP_STATIC_CHAIN ();
  if (VOID_TYPE_P(t)) {
    add_stmt (call);
  } else {
    TREE_SIDE_EFFECTS (call) = 1;
    EXPR_PUSH (call);
  }
  CALL_POP ();
  flags = call_expr_flags (call);
  if (flags & ECF_RETURNS_TWICE) {
    tree block, decl;
    /* call to setjmp: make locals volatile */
    m3_set_volatize (); /* for later temporaries, locals */
    for (block = current_block;
         block != current_function_decl;
         block = BLOCK_SUPERCONTEXT(block)) {
      for (decl = BLOCK_VARS(block); decl; decl = TREE_CHAIN(decl)) {
        m3_volatilize_decl(decl);
      }
    }
    /* and arguments */
    for (decl = DECL_ARGUMENTS(current_function_decl);
         decl; decl = TREE_CHAIN(decl)) {
      m3_volatilize_decl(decl);
    }
  }
}

static void
m3_call_indirect (tree t, tree cc)
{
  tree argtypes = chainon (CALL_TOP_TYPE (), void_list_node);
  tree fntype = m3_build_pointer_type (build_function_type (t, argtypes));
  tree call;
  tree fnaddr = EXPR_REF (-1);
  EXPR_POP ();

  decl_attributes (&fntype, cc, 0);

  call = build_call_list (t, m3_cast (fntype, fnaddr), CALL_TOP_ARG ());
  CALL_EXPR_STATIC_CHAIN (call) = CALL_TOP_STATIC_CHAIN ();
  if (VOID_TYPE_P(t)) {
    add_stmt (call);
  } else {
    TREE_SIDE_EFFECTS (call) = 1;
    EXPR_PUSH (call);
  }
  CALL_POP ();
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
m3_load_1 (tree v, int o, tree src_t, m3_type src_T, tree dst_t, m3_type dst_T,
           bool volatil)
{
  if (o != 0 || TREE_TYPE (v) != src_t)
  {
    if (GCC42)
    {
      /* failsafe, but inefficient */
      v = m3_build1 (ADDR_EXPR, t_addr, v);
      v = m3_build2 (PLUS_EXPR, t_addr, v, size_int (o / BITS_PER_UNIT));
      v = m3_build1 (INDIRECT_REF, src_t,
                     m3_cast (m3_build_pointer_type (src_t), v));
    }
    else
    {
      v = m3_build3 (BIT_FIELD_REF, src_t, v, TYPE_SIZE (src_t),
                     bitsize_int (o));
    }
  }
  if (volatil)
    TREE_THIS_VOLATILE(v) = 1; /* force this to avoid aliasing problems */
  if (M3_ALL_VOLATILE)
    TREE_THIS_VOLATILE(v) = TREE_SIDE_EFFECTS(v) = 1; /* force this to avoid aliasing problems */
  if (src_T != dst_T) {
    v = m3_convert (dst_t, v);
  }
  EXPR_PUSH (v);
}

static void
m3_load (tree v, int o, tree src_t, m3_type src_T, tree dst_t, m3_type dst_T)
{
  bool volatil = false;
  m3_load_1 (v, o, src_t, src_T, dst_t, dst_T, volatil);
}

static void ATTRIBUTE_UNUSED
m3_load_volatile (tree v, int o, tree src_t, m3_type src_T,
                  tree dst_t, m3_type dst_T)
{
  bool volatil = true;
  m3_load_1 (v, o, src_t, src_T, dst_t, dst_T, volatil);
}

static void
m3_store_1 (tree v, int o, tree src_t, m3_type src_T, tree dst_t, m3_type dst_T,
            bool volatil)
{
  tree val;
  if (o != 0 || TREE_TYPE (v) != dst_t)
  {
    if (GCC42)
    {
      /* failsafe, but inefficient */
      v = m3_build1 (ADDR_EXPR, t_addr, v);
      v = m3_build2 (PLUS_EXPR, t_addr, v, size_int (o / BITS_PER_UNIT));
      v = m3_build1 (INDIRECT_REF, dst_t,
                     m3_cast (m3_build_pointer_type (dst_t), v));
    }
    else
    {
      v = m3_build3 (BIT_FIELD_REF, dst_t, v, TYPE_SIZE (dst_t),
                     bitsize_int (o));
    }
  }
  if (volatil || m3_next_store_volatile)
    TREE_THIS_VOLATILE(v) = 1; /* force this to avoid aliasing problems */
  if (M3_ALL_VOLATILE)
    TREE_THIS_VOLATILE(v) = TREE_SIDE_EFFECTS(v) = 1; /* force this to avoid aliasing problems */
  m3_next_store_volatile = false;
  val = m3_cast (src_t, EXPR_REF (-1));
  if (src_T != dst_T) {
    val = m3_convert (dst_t, val);
  }
  add_stmt (build2 (MODIFY_EXPR, dst_t, v, val));
  EXPR_POP ();
}

static void
m3_store (tree v, int o, tree src_t, m3_type src_T, tree dst_t, m3_type dst_T)
{
  bool volatil = false;
  m3_store_1(v, o, src_t, src_T, dst_t, dst_T, volatil);
}

static void
m3_store_volatile (tree v, int o, tree src_t, m3_type src_T, tree dst_t,
                   m3_type dst_T)
{
  bool volatil = true;
  m3_store_1(v, o, src_t, src_T, dst_t, dst_T, volatil);
}

static void
setop (tree p, int n, int q)
{
  m3_start_call ();
  EXPR_PUSH (size_int (n));
  m3_pop_param (t_int);
  while (q--) {
    m3_pop_param (t_addr);
  }
  m3_call_direct (p, TREE_TYPE (TREE_TYPE (p)));
}

static void
setop2 (tree p, int q)
{
  tree t;

  m3_start_call ();
  while (q--) {
    m3_pop_param (t_addr);
  }
  t = TREE_TYPE (TREE_TYPE (p));
  m3_call_direct (p, t);
}

/*----------------------------------------------------------------------------*/

static const char* mode_to_string(enum machine_mode mode)
{
  const char* modestring = "";

  switch (mode)
  {
    default:
      break;
    case VOIDmode:
      modestring = "VOIDmode";
      break;
    case DImode:
      modestring = "DImode";
      break;
    case BLKmode:
      modestring = "BLKmode";
      break;
  }
  return modestring;
}

/*---------------------------------------------------------------- faults ---*/

static int  fault_offs;                /*   + offset                */

static void
declare_fault_proc (void)
{
  tree proc, parm, resultdecl;
  tree parm_block = make_node (BLOCK);
  tree top_block  = make_node (BLOCK);

  proc = build_decl (FUNCTION_DECL, get_identifier ("_m3_fault"),
                     build_function_type_list (t_void, t_word, NULL_TREE));

  resultdecl = build_decl (RESULT_DECL, NULL_TREE, t_void);
  DECL_CONTEXT (resultdecl) = proc;
  DECL_ARTIFICIAL (resultdecl) = 1;
  DECL_IGNORED_P (resultdecl) = 1;
  DECL_RESULT (proc) = resultdecl;

  /* TREE_THIS_VOLATILE (proc) = 1; / * noreturn */
  TREE_STATIC (proc) = 1;
  TREE_PUBLIC (proc) = 0;
  DECL_CONTEXT (proc) = 0;

  parm = build_decl (PARM_DECL, fix_name ("arg", 0x195c2a74), t_word);
  if (DECL_MODE (parm) == VOIDmode)
  {
      if (option_trace_all)
        fprintf(stderr, "  declare_fault_proc: converting parameter from VOIDmode to Pmode\n");
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

    fprintf(stderr, "  declare_fault_proc: type is 0x%x (%s)\n", (unsigned) mode, mode_to_string(mode));
  }

  fault_proc = proc;
}

static void
m3_gimplify_function (tree fndecl)
{
  struct cgraph_node *cgn;

  dump_function (TDI_original, fndecl);
  gimplify_function_tree (fndecl);
  dump_function (TDI_generic, fndecl);

  /* Convert all nested functions to GIMPLE now.  We do things in this order
     so that items like VLA sizes are expanded properly in the context of the
     correct function.  */
  cgn = cgraph_node (fndecl);
  for (cgn = cgn->nested; cgn; cgn = cgn->next_nested)
    m3_gimplify_function (cgn->decl);
}

static void
emit_fault_proc (void)
{
  location_t save_loc = input_location;
  tree p = fault_proc;

#ifdef M3_USE_MAPPED_LOCATION
  input_location = UNKNOWN_LOCATION;
#else
  input_line = 0;
#endif
  DECL_SOURCE_LOCATION (p) = input_location;

  gcc_assert (current_function_decl == NULL_TREE);
  gcc_assert (current_block == NULL_TREE);
  current_function_decl = p;
  allocate_struct_function (p, false);

  pending_blocks = tree_cons (NULL_TREE, current_block, pending_blocks);
  current_block = DECL_INITIAL (p); /* parm_block */
  TREE_USED (current_block) = 1;
  current_block = BLOCK_SUBBLOCKS (current_block); /* top_block */
  TREE_USED (current_block) = 1;

  pending_stmts = tree_cons (NULL_TREE, current_stmts, pending_stmts);
  current_stmts = alloc_stmt_list ();

  m3_start_call ();
  EXPR_PUSH (m3_build1 (ADDR_EXPR, t_addr, current_segment));
  m3_pop_param (t_addr);
  EXPR_PUSH (DECL_ARGUMENTS (p));
  m3_pop_param (t_word);
  if (fault_handler != NULL_TREE) {
    m3_call_direct (fault_handler, t_void);
  } else {
    m3_load (fault_intf, fault_offs, t_addr, T_addr, t_addr, T_addr);
    m3_call_indirect (t_void, NULL_TREE);
  }
  add_stmt (build1 (RETURN_EXPR, t_void, NULL_TREE));

  /* Attach block to the function */
  gcc_assert (current_block == BLOCK_SUBBLOCKS (DECL_INITIAL (p)));
  DECL_SAVED_TREE (p) = build3 (BIND_EXPR, t_void,
                                BLOCK_VARS (current_block),
                                current_stmts, current_block);
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

/* FIXME: jdp says 0x0f and 4; cm3 may need more */
#define FAULT_MASK 0x1f
#define LINE_SHIFT 5

static tree
generate_fault (int code)
{
  tree arg;

  if (fault_proc == 0) declare_fault_proc ();
  arg = build_int_cst (t_int, (LOCATION_LINE(input_location) << LINE_SHIFT) + (code & FAULT_MASK));
#if GCC45
  return build_function_call_expr (UNKNOWN_LOCATION, fault_proc, build_tree_list (NULL_TREE, arg));
#else
  return build_function_call_expr (fault_proc, build_tree_list (NULL_TREE, arg));
#endif
}

/*-------------------------------------------------- M3CG opcode handlers ---*/

static void
m3cg_begin_unit (void)
{
  UNUSED_INTEGER (n);
  exported_interfaces = 0;
}

static void
m3cg_end_unit (void)
{
  int j;

  gcc_assert (current_block == NULL_TREE);
  debug_tag ('i', NO_UID, "_%s", current_unit_name);
  for (j = 0; j < exported_interfaces; j++)
    debug_field (exported_interfaces_names [j]);
  debug_struct ();
  if (fault_proc != NULL_TREE) emit_fault_proc ();
}

static void
m3cg_import_unit (void)
{
  UNUSED_NAME (n);
  /* ignore */
}

static void
m3cg_export_unit (void)
{
  NAME (n);
  if (exported_interfaces == COUNT_OF(exported_interfaces_names))
    fatal_error("internal limit exporting more than 100 interfaces");
  /* remember the set of exported interfaces */
  exported_interfaces_names [exported_interfaces++] = n;
}

static void
m3cg_set_source_file (void)
{
  NAME (s);

#ifdef M3_USE_MAPPED_LOCATION
  linemap_add (line_table, LC_RENAME, false, s, 1);
  input_location = linemap_line_start (line_table, 1, 80);
#else
  input_filename = s;
#endif
}

static void
m3cg_set_source_line (void)
{
  INTEGER (i);

  if (option_source_line_trace)
    fprintf(stderr, "  source line %4ld\n", i);

#ifdef M3_USE_MAPPED_LOCATION
  input_location = linemap_line_start (line_table, i, 80);
#else
  input_line = i;
#endif
}

static size_t m3_add(size_t a, size_t b)
{
  size_t c = a + b;
  if (c < a)
    fatal_error("integer overflow");
  return c;
}

static void
m3cg_declare_typename (void)
{
  TYPEID (my_id);
  NAME   (name);

  char fullname [100];
  size_t unit_len = strlen(current_unit_name);
  size_t name_len = strlen(name);
  size_t full_len;

  if (option_types_trace)
    fprintf(stderr, "  typename:%s id:0x%lx\n", name, my_id);

  full_len = m3_add(unit_len, m3_add(name_len, 1));
    
  if (full_len >= sizeof(fullname))
    fatal_error("identifier too long (in m3cg_declare_typename, %s.%s)", current_unit_name, name);
    
  memcpy(&fullname[0], current_unit_name, unit_len);
  fullname[unit_len] = '.';
  memcpy(&fullname[unit_len + 1], name, name_len + 1);

  debug_tag ('N', my_id, "");
  debug_field (fullname);
  debug_struct ();

  debug_tag ('n', NO_UID, "_%s", fullname);
  debug_field_id (my_id);
  debug_struct ();
}

static void
m3cg_declare_array (void)
{
  TYPEID  (my_id);
  TYPEID  (index_id);
  TYPEID  (elts_id);
  BITSIZE (size);

  if (option_types_trace)
    fprintf(stderr,
            "  array id:0x%lx index_id:0x%lx elements_id:0x%lx size:0x%lx\n",
            my_id, index_id, elts_id, size);

  debug_tag ('A', my_id, "_%ld", size);
  debug_field_id (index_id);
  debug_field_id (elts_id);
  debug_struct ();
}

static void
m3cg_declare_open_array (void)
{
  TYPEID  (my_id);
  TYPEID  (elts_id);
  BITSIZE (size);

  if (option_types_trace)
    fprintf(stderr,
            "  open array id:0x%lx elements_id:0x%lx,size:0x%lx\n",
            my_id, elts_id, size);

  debug_tag ('B', my_id, "_%ld", size);
  debug_field_id (elts_id);
  debug_struct ();
}

static void
m3cg_declare_enum (void)
{
  TYPEID  (my_id);
  INTEGER (n_elts);
  BITSIZE (size);

  if (option_types_trace)
    fprintf(stderr,
            "  enum id:0x%lx elements:0x%lx size:0x%lx\n",
            my_id, n_elts, size);

  debug_tag ('C', my_id, "_%ld", size);
  current_dbg_type_count1 = n_elts;
}

static void
m3cg_declare_enum_elt (void)
{
  NAME (n);

  if (option_types_trace)
    fprintf(stderr, "  enum elem %s\n", n);

  debug_field (n);
  if (--current_dbg_type_count1 == 0) { debug_struct (); }
}

static void
m3cg_declare_packed (void)
{
  TYPEID  (my_id);
  BITSIZE (size);
  TYPEID  (target_id);

  if (option_types_trace)
    fprintf(stderr,
            "  packed id:0x%lx target_id:0x%lx size:0x%lx\n",
            my_id, target_id, size);

  debug_field_id (target_id);
  debug_tag ('D', my_id, "_%ld", size);
  debug_struct ();
}

static void
m3cg_declare_record (void)
{
  TYPEID  (my_id);
  BITSIZE (size);
  INTEGER (n_fields);

  if (option_types_trace)
    fprintf(stderr,
            "  record id:0x%lx fields:0x%lx size:0x%lx\n",
            my_id, n_fields, size);

  debug_tag ('R', my_id, "_%ld", size);
  current_dbg_type_count1 = n_fields;
  current_dbg_type_count2 = 0;
  if (current_dbg_type_count1 == 0) { debug_struct (); }
}

static void
m3cg_declare_field (void)
{
  NAME      (name);
  BITOFFSET (offset);
  BITSIZE   (size);
  TYPEID    (my_id);

  if (option_types_trace)
    fprintf(stderr, "  field %s, id 0x%lx, size 0x%lx, offset 0x%lx\n",
            name, my_id, size, offset);

  debug_field_fmt (my_id, "_%ld_%ld_%s", offset, size, name);
  current_dbg_type_count1--;
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
    debug_struct ();
  }
}

static void
m3cg_declare_set (void)
{
  TYPEID  (my_id);
  TYPEID  (domain_id);
  BITSIZE (size);

  debug_tag ('S', my_id, "_%ld", size);
  debug_field_id (domain_id);
  debug_struct ();
}

/* m3cg_append_char, m3cg_fill_word_in_hex, and m3cg_fill_hex_value help
   m3cg_declare_subrange to write hex values of lower and upper bounds.
   They omit some redundant high bits, either positive or negative.  
   However, the leftmost bit explicitly specified by the output is
   always a sign bit and can be sign-extended.  For example, 
   0x7F and 0x0FF are positive, while 0xF7F and 0xFF are negative. */
 
static void
m3cg_append_char (char c, char ** var_p, char * p_limit) 
{
  if (*var_p < p_limit) { **var_p = c; (*var_p)++; } 
} 

static void
m3cg_fill_word_in_hex 
( HOST_WIDE_INT i, char **var_p, char *p_limit, int *var_leading)
{ 
  int digit; 
  int shift_ct = HOST_BITS_PER_WIDE_INT - 4; 

  while (shift_ct >= 0) 
    { digit = (i >> shift_ct) & 0xF; 
      if (digit != *var_leading) 
        { if (*var_leading == 0 && digit > 7) 
            { /* A leading zero sign-bit is needed. */  
              m3cg_append_char ('0', var_p, p_limit); 
            }
          else if (*var_leading == 0xF && digit <= 7) 
            { /* A leading one sign-bit is needed. */  
              m3cg_append_char ('F', var_p, p_limit); 
            }
          if (digit < 10) { m3cg_append_char (digit + '0', var_p , p_limit); }  
          else { m3cg_append_char (digit - 10 + 'A', var_p , p_limit); }  
          *var_leading = 0x10; 
        } 
      shift_ct -= 4; 
    } 
} 

static void 
m3cg_fill_hex_value 
  (HOST_WIDE_INT a1, HOST_WIDE_INT a0, char **var_p, char *p_limit) 
{ int leading; /*     0: skipping leading zero hex digits. 
                    0xF: skipping leading 'F' hex digits.
                  > 0xF: write all digits. */ 

  if (a1 >= 0) {leading = 0; }
  else {leading = 0XF; }   
  m3cg_append_char('0', var_p, p_limit); 
  m3cg_append_char('x', var_p, p_limit);
  m3cg_fill_word_in_hex (a1, var_p , p_limit, &leading); 
  m3cg_fill_word_in_hex (a0, var_p , p_limit, &leading); 
  if (leading == 0) { m3cg_append_char('0', var_p, p_limit); } 
  else if (leading == 0xF) { m3cg_append_char('F', var_p, p_limit); } 
} 

static void
m3cg_declare_subrange (void)
{

  TYPEID         (my_id);
  TYPEID         (domain_id);
  TARGET_INTEGER (min);
  TARGET_INTEGER (max);
  BITSIZE        (size);

  HOST_WIDE_INT  a0, a1, b0, b1;
  char *p; 
  char *p_limit; 
  char buff [80]; /* Liberal. */ 

  a1 = TREE_INT_CST_HIGH(min);  a0 = TREE_INT_CST_LOW(min);
  b1 = TREE_INT_CST_HIGH(max);  b0 = TREE_INT_CST_LOW(max);

  if (option_types_trace)
    fprintf(stderr,
            "  subrange id 0x%lx"
            ", a0 " HOST_WIDE_INT_PRINT_DEC
            ", a1 " HOST_WIDE_INT_PRINT_DEC
            ", b0 " HOST_WIDE_INT_PRINT_DEC
            ", b1 " HOST_WIDE_INT_PRINT_DEC
            ", size 0x%lx\n",
            my_id, a0, a1, b0, b1, size);

  p = buff; 
  p_limit = p + sizeof(buff); 
  m3cg_append_char('_', &p, p_limit); 
  m3cg_append_char('%', &p, p_limit); 
  m3cg_append_char('d', &p, p_limit); 
  m3cg_append_char('_', &p, p_limit); 
  m3cg_fill_hex_value (a1, a0, &p, p_limit); 
  m3cg_append_char('_', &p, p_limit); 
  m3cg_fill_hex_value (b1, b0, &p, p_limit); 
  m3cg_append_char('\0', &p, p_limit); 
  debug_tag('Z', my_id, buff, size); 

  debug_field_id (domain_id);
  debug_struct ();
}

static void
m3cg_declare_pointer (void)
{
  TYPEID        (my_id);
  TYPEID        (target_id);
  QUOTED_STRING (brand, brand_len);
  BOOLEAN       (traced);

  if (option_types_trace) {
    const char * sbrand = "null";
    if (brand) sbrand = brand;
    fprintf(stderr,
            "  pointer id 0x%lx, target id 0x%lx, brand %s, traced 0x%x\n",
            my_id, target_id, sbrand, traced);
  }

  debug_tag ('Y', my_id, "_%d_%d_%d_%s", GET_MODE_BITSIZE (Pmode),
             traced, (brand ? 1 : 0), (brand ? brand : "" ));
  debug_field_id (target_id);
  debug_struct ();
}

static void
m3cg_declare_indirect (void)
{
  TYPEID (my_id);
  TYPEID (target_id);

  if (option_types_trace)
    fprintf(stderr, "  indirect id 0x%lx, target_id 0x%lx\n", my_id, target_id);

  debug_tag ('X', my_id, "_%d", GET_MODE_BITSIZE (Pmode));
  debug_field_id (target_id);
  debug_struct ();
}

static void
m3cg_declare_proctype (void)
{
  TYPEID  (my_id);
  INTEGER (n_formals);
  TYPEID  (result_id);
  INTEGER (n_raises);
  UNUSED_CC (cc);

  if (option_types_trace)
    fprintf(stderr,
            "  proctype id 0x%lx, result id 0x%lx, formals 0x%lx, raises 0x%lx\n",
            my_id, result_id, n_formals, n_raises);

  debug_tag ('P', my_id, "_%d_%c%ld", GET_MODE_BITSIZE (Pmode),
             n_raises < 0 ? 'A' : 'L', MAX (n_raises, 0));
  current_dbg_type_count1 = n_formals;
  current_dbg_type_count2 = MAX (0, n_raises);
  debug_field_id (result_id);
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
    debug_struct ();
  }
}

static void
m3cg_declare_formal (void)
{
  NAME   (n);
  TYPEID (my_id);

  if (option_types_trace)
    fprintf(stderr, "  formal %s id 0x%lx\n", n, my_id);

  debug_field_fmt (my_id, "_%s", n);
  current_dbg_type_count1--;
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
    debug_struct ();
  }
}

static void
m3cg_declare_raises (void)
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
m3cg_declare_object (void)
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
            "  object id 0x%lx, super id 0x%lx, brand %s, traced 0x%x, fields 0x%lx, methods 0x%lx\n",
            my_id, super_id, sbrand, traced, n_fields, n_methods);
  }

  debug_tag ('O', my_id, "_%d_%ld_%d_%d_%s", POINTER_SIZE, n_fields, traced,
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
m3cg_declare_method (void)
{
  NAME   (name);
  TYPEID (my_id);

  if (option_procs_trace)
    fprintf(stderr, "  method %s typeid 0x%lx\n", name, my_id);

  debug_field_fmt (my_id, "_%d_%d_%s",
                   current_dbg_type_count3++  * GET_MODE_BITSIZE (Pmode),
                   GET_MODE_BITSIZE (Pmode), name);
  current_dbg_type_count2--;
  if (current_dbg_type_count1 == 0 && current_dbg_type_count2 == 0) {
    debug_struct ();
  }
}

static void
m3cg_declare_opaque (void)
{
  UNUSED_TYPEID (my_id);
  UNUSED_TYPEID (super_id);
  /* we don't pass this info to the debugger, only the revelation is interesting */
}

static void
m3cg_reveal_opaque (void)
{
  TYPEID (lhs);
  TYPEID (rhs);

  if (option_procs_trace)
    fprintf(stderr, "  typeid 0x%lx = typeid 0x%lx\n", lhs, rhs);

  debug_tag ('Q', lhs, "_%d", GET_MODE_BITSIZE (Pmode));
  debug_field_id (rhs);
  debug_struct ();
}

static void
m3cg_declare_exception (void)
{
  UNUSED_NAME    (n);
  UNUSED_TYPEID  (t);
  UNUSED_BOOLEAN (raise_proc);
  UNUSED_VAR     (base);
  UNUSED_INTEGER (offset);

  /* nothing yet */
}

static void
m3cg_set_runtime_proc (void)
{
  NAME (s);
  PROC (p);

  if (STREQ (s, "ReportFault")) {
    fault_handler = p;
  }
}

static void
m3cg_set_runtime_hook (void)
{
  NAME       (s);
  VAR        (v);
  BYTEOFFSET (o);

  TREE_USED (v) = 1;
  if (STREQ (s, "ReportFault")) {
    fault_intf = v;
    fault_offs = o;
  }
}

static void
m3cg_import_global (void)
{
  NAME       (n);
  BYTESIZE   (s);
  ALIGNMENT  (a);
  TYPE       (t);
  TYPEID     (id);
  RETURN_VAR (v, VAR_DECL);

  DECL_NAME(v) = fix_name (n, id);

  if (option_vars_trace)
    fprintf(stderr, "  import var %s type:%s size:0x%lx alignment:0x%lx\n",
            IDENTIFIER_POINTER(DECL_NAME(v)), m3cg_typename(t), s, a);

  DECL_EXTERNAL (v) = 1;
  TREE_PUBLIC   (v) = 1;
  TREE_TYPE (v) = m3_build_type (t, s, a);
  layout_decl (v, a);

  TREE_CHAIN (v) = global_decls;
  global_decls = v;
}

static void
m3cg_declare_segment (void)
{
  NAME       (n);
  TYPEID     (id);
  BOOLEAN    (is_const);
  RETURN_VAR (v, VAR_DECL);

  DECL_NAME (v) = fix_name (n, id);

  if (option_vars_trace)
    fprintf(stderr, "  segment %s typeid 0x%lx\n",
            IDENTIFIER_POINTER(DECL_NAME(v)), id);

  /* we really don't have an idea of what the type of this var is; let's try
     to put something that will be good enough for all the uses of this var we
     are going to see before we have a bind_segment. Use a large size so that
     gcc doesn't think it fits in a register, so that loads out of it do get
     their offsets applied. */
  TREE_TYPE (v)
    = m3_build_type (T_struct, BIGGEST_ALIGNMENT * 2, BIGGEST_ALIGNMENT);
  layout_decl (v, BIGGEST_ALIGNMENT);
  TYPE_UNSIGNED (TREE_TYPE (v)) = 1;
  TREE_STATIC (v) = 1;
  TREE_PUBLIC (v) = 1;
  TREE_READONLY (v) = is_const;
  TREE_ADDRESSABLE (v) = 1;
  DECL_DEFER_OUTPUT (v) = 1;
  current_segment = v;

  TREE_CHAIN (v) = global_decls;
  global_decls = v;

  /* do not use "n", it is going to go away at the next instruction;
     skip the 'MI_' or 'MM_' prefix. */
  current_unit_name = IDENTIFIER_POINTER (DECL_NAME (v)) + 3;
}

static void
m3cg_bind_segment (void)
{
  VAR       (v);
  BYTESIZE  (s);
  ALIGNMENT (a);
  TYPE      (t);
  BOOLEAN   (exported);
  BOOLEAN   (initialized);

  if (option_vars_trace)
    fprintf(stderr, "  bind segment %s type:%s size:0x%lx alignment:0x%lx\n",
            IDENTIFIER_POINTER(DECL_NAME(v)), m3cg_typename(t), s, a);

  current_segment = v;
  TREE_TYPE (v) = m3_build_type (t, s, a);
  relayout_decl (v);

  DECL_UNSIGNED (v) = TYPE_UNSIGNED (TREE_TYPE (v));
  DECL_COMMON (v) = (initialized == 0);
  TREE_PUBLIC (v) = exported;
  TREE_STATIC (v) = 1;
}

static void
m3cg_declare_global (void)
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
    fprintf(stderr, "  global var %s type:%s size:0x%lx alignment:0x%lx\n",
            IDENTIFIER_POINTER(DECL_NAME(v)), m3cg_typename(t), s, a);

  TREE_TYPE (v) = m3_build_type (t, s, a);
  DECL_COMMON (v) = (initialized == 0);
  TREE_PUBLIC (v) = exported;
  TREE_STATIC (v) = 1;
  layout_decl (v, a);

  TREE_CHAIN (v) = global_decls;
  global_decls = v;
}

static void
m3cg_declare_constant (void)
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

  TREE_CHAIN (v) = global_decls;
  global_decls = v;
}

static void
m3cg_declare_local (void)
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
    fprintf(stderr, "  local var %s type:%s size:0x%lx alignment:0x%lx\n",
            IDENTIFIER_POINTER(DECL_NAME(v)), m3cg_typename(t), s, a);

  TREE_TYPE (v) = m3_build_type (t, s, a);
  DECL_NONLOCAL (v) = up_level || in_memory;
  TREE_ADDRESSABLE (v) = in_memory;
  DECL_CONTEXT (v) = current_function_decl;
  layout_decl (v, a);

  if (current_block)
    {
      if (m3_volatize_p ())
        m3_volatilize_decl(v);
      add_stmt (build1 (DECL_EXPR, t_void, v));
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

static int current_param_count; /* <0 => import_procedure, >0 => declare_procedure */

static void
m3cg_declare_param (void)
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

  tree p;

  p = current_function_decl;
  if (current_param_count > 0) {
    /* declare_procedure... */
    current_param_count -= 1;
  } else if (current_param_count < 0) {
    /* import_procedure... */
    current_param_count += 1;
    if (current_param_count == 0) {
      /* pop current_function_decl and reset DECL_CONTEXT=0 */
      current_function_decl = DECL_CONTEXT(p);
      DECL_CONTEXT(p) = NULL_TREE; /* imports are in global scope */
    }
  } else {
    gcc_unreachable();
  }

  DECL_NAME (v) = fix_name (n, id);

  if (option_procs_trace)
    fprintf(stderr, "  param %s type:%s typeid:0x%lx bytesize:0x%lx alignment:0x%lx in_memory:0x%x up_level:0x%x\n",
            IDENTIFIER_POINTER(DECL_NAME(v)), m3cg_typename(t), id, s, a, in_memory, up_level);

  TREE_TYPE (v) = m3_build_type (t, s, a);
  DECL_NONLOCAL (v) = up_level || in_memory;
  TREE_ADDRESSABLE (v) = in_memory;
  DECL_ARG_TYPE (v) = TREE_TYPE (v);
  DECL_CONTEXT (v) = p;
  layout_decl (v, a);

  if (option_trace_all)
  {
    enum machine_mode mode = TYPE_MODE (TREE_TYPE (v));
    fprintf(stderr, "  mode 0x%x (%s)\n", (unsigned) mode, mode_to_string(mode));
  }

  if (DECL_MODE (v) == VOIDmode)
  {
      if (option_trace_all)
        fprintf(stderr, "  converting from VOIDmode to Pmode\n");
      DECL_MODE (v) = Pmode;
  }

  TREE_CHAIN (v) = DECL_ARGUMENTS (p);
  DECL_ARGUMENTS (p) = v;

  if (current_param_count == 0) {
    /* arguments were accumulated in reverse, build type, then unreverse */
    tree parm, args = void_list_node;
    for (parm = DECL_ARGUMENTS (p); parm; parm = TREE_CHAIN(parm))
      args = tree_cons (NULL_TREE, TREE_TYPE (parm), args);
    args = build_function_type (TREE_TYPE (TREE_TYPE (p)), args);
    decl_attributes (&args, TYPE_ATTRIBUTES (TREE_TYPE (p)), 0);
    TREE_TYPE (p) = args;
    DECL_ARGUMENTS(p) = nreverse (DECL_ARGUMENTS (p));
  }
}

static void
m3cg_declare_temp (void)
{
  BYTESIZE   (s);
  ALIGNMENT  (a);
  TYPE       (t);
  BOOLEAN    (in_memory);
  RETURN_VAR (v, VAR_DECL);

  if (option_vars_trace)
    fprintf(stderr, "  temp var type:%s size:0x%lx alignment:0x%lx\n", m3cg_typename(t), s, a);
 
  if (t == T_void)
    t = T_struct;

  TREE_TYPE (v) = m3_build_type (t, s, a);
  layout_decl (v, 0);
  DECL_UNSIGNED (v) = TYPE_UNSIGNED (TREE_TYPE (v));
  TREE_ADDRESSABLE (v) = in_memory;
  DECL_CONTEXT (v) = current_function_decl;
  if (m3_volatize_p ())
    m3_volatilize_decl(v);

  TREE_CHAIN (v) = BLOCK_VARS (BLOCK_SUBBLOCKS
                               (DECL_INITIAL (current_function_decl)));
  BLOCK_VARS (BLOCK_SUBBLOCKS (DECL_INITIAL (current_function_decl))) = v;

  add_stmt (build1 (DECL_EXPR, t_void, v));
}

static void
m3cg_free_temp (void)
{
  UNUSED_VAR (v);
  /* nothing to do */
}

static void
m3cg_begin_init (void)
{
  UNUSED_VAR (v);

  current_record_offset = 0;
  current_record_vals = NULL_TREE;
  current_record_type = make_node (RECORD_TYPE);
  TREE_ASM_WRITTEN (current_record_type) = 1;
}

static void
m3cg_end_init (void)
{
  VAR (v);

  if (DECL_SIZE (v)) {
    int v_size = TREE_INT_CST_LOW (DECL_SIZE (v));
    if (current_record_offset < v_size) { one_gap (v_size); }
  }

  TYPE_FIELDS (current_record_type) =
    nreverse (TYPE_FIELDS (current_record_type));
  layout_type (current_record_type);

  /* remember this init so we can fix any init_offset later */
  pending_inits
    = tree_cons (NULL_TREE,
                 build_constructor_from_list (current_record_type,
                                              nreverse (current_record_vals)),
                 pending_inits);
  DECL_INITIAL (v) = TREE_VALUE (pending_inits);
}

static void
m3cg_init_int (void)
{
  BYTEOFFSET     (o);
  TARGET_INTEGER (val);
  MTYPE          (t);

  tree f, v;
  one_field (o, t, &f, &v);
  val = convert (t, val);
  TREE_VALUE (v) = val;
}

static void
m3cg_init_proc (void)
{
  BYTEOFFSET (o);
  PROC       (p);

  tree f, v;
  tree expr = proc_addr (p);
  one_field (o, TREE_TYPE (expr), &f, &v);
  TREE_VALUE (v) = expr;
}

static void
m3cg_init_label (void)
{
  BYTEOFFSET (o);
  LABEL      (l);

  tree f, v;

  one_field (o, t_addr, &f, &v);
  TREE_USED (l) = 1;
  TREE_VALUE (v) = build1 (ADDR_EXPR, t_addr, l);
}

static void
m3cg_init_var (void)
{
  BYTEOFFSET (o);
  VAR        (var);
  BYTEOFFSET (b);

  tree f, v;
  enum tree_code plus = (GCC45 ? POINTER_PLUS_EXPR : PLUS_EXPR);

  TREE_USED (var) = 1;

  one_field (o, t_addr, &f, &v);
  TREE_VALUE (v) = m3_build2 (plus, t_addr,
                              m3_build1 (ADDR_EXPR, t_addr, var),
                              size_int (b / BITS_PER_UNIT));
}

static void
m3cg_init_offset (void)
{
  BYTEOFFSET (o);
  VAR        (var);

  tree f, v;
  TREE_USED (var) = 1;
  /* M3 hack to preserve TREE_ADDRESSABLE: see tree-ssa.c, tree-ssa-alias.c */
  TREE_THIS_VOLATILE (var) = 1;
  one_field (o, t_int, &f, &v);
  DECL_LANG_SPECIFIC(f) = (lang_decl_t*)var; /* we will fix the offset later once we have rtl */
  TREE_VALUE (v) = v_zero;
}

static void
m3cg_init_chars (void)
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
m3cg_init_float (void)
{
  BYTEOFFSET (o);
  FLOAT      (val, fkind);

  tree f, v, t = { 0 };

  switch (fkind) {
  case 0: t = t_reel;   break;
  case 1: t = t_lreel;  break;
  case 2: t = t_xreel;  break;
  }

  one_field (o, t, &f, &v);
  TREE_TYPE (f) = t;
  TREE_VALUE (v) = val;
}

#define M3CG_ADAPT_RETURN_TYPE 1

static void
m3cg_import_procedure (void)
{
  NAME    (n);
  INTEGER (n_params);
  MTYPE2  (return_type, ret_type);
  CC      (cc);
  PROC    (p);

  if (option_procs_trace)
    fprintf(stderr, "  procedure:%s nparams:0x%lx rettype:%s\n", n, n_params,
            m3cg_typename(ret_type));

#if M3CG_ADAPT_RETURN_TYPE
  /** 4/30/96 -- WKK -- It seems gcc can't hack small return values... */
  if (INTEGRAL_TYPE_P (return_type)) {
    if (TYPE_UNSIGNED (return_type)) {
      if (TYPE_SIZE (return_type) <= TYPE_SIZE (t_word))
        return_type = t_word;
    } else {
      if (TYPE_SIZE (return_type) <= TYPE_SIZE (t_int))
        return_type = t_int;
    }
  }
#endif

  DECL_NAME (p) = get_identifier (n);
  TREE_TYPE (p) = build_function_type (return_type, NULL_TREE);
  TREE_PUBLIC (p) = 1;
  DECL_EXTERNAL (p) = 1;
  DECL_CONTEXT (p) = NULL_TREE;
  DECL_MODE (p) = FUNCTION_MODE;

  decl_attributes (&TREE_TYPE (p), cc, 0);

  TREE_CHAIN (p) = global_decls;
  global_decls = p;

  current_param_count = -n_params;
  if (current_param_count) {
    /* stack current_function_decl while we get the params */
    DECL_CONTEXT(p) = current_function_decl;
    current_function_decl = p;
  }
}

static void
m3cg_declare_procedure (void)
{
  NAME    (n);
  INTEGER (n_params);
  MTYPE2  (return_type, ret_type);
  LEVEL   (lev);
  CC      (cc);
  BOOLEAN (exported);
  PROC    (parent);
  PROC    (p);

  tree resultdecl;
  tree parm_block = make_node (BLOCK);
  tree top_block  = make_node (BLOCK);
  
  /* Level must be positive and nested functions are never exported. */
  gcc_assert(lev >= 0);
  gcc_assert(lev == 0 || !exported);

  if (option_procs_trace)
    fprintf(stderr,
            "  procedure:%s nparams:0x%lx rettype:%s lev:%u exported:%u\n",
            n, n_params, m3cg_typename(ret_type), (unsigned)lev, (unsigned)exported);

#if M3CG_ADAPT_RETURN_TYPE
  /** 4/30/96 -- WKK -- It seems gcc can't hack small return values... */
  if (INTEGRAL_TYPE_P (return_type)) {
    if (TYPE_UNSIGNED (return_type)) {
      if (TYPE_SIZE (return_type) <= TYPE_SIZE (t_word))
        return_type = t_word;
    } else {
      if (TYPE_SIZE (return_type) <= TYPE_SIZE (t_int))
        return_type = t_int;
    }
  }
#endif

  DECL_NAME (p) = get_identifier (n);
  TREE_STATIC (p) = 1;

  /* TREE_PUBLIC (p) should be 'exported', but that fails to keep any
   * implementation of nonexported functions, even with TREE_ADDRESSABLE(p) = 1
   */
  TREE_PUBLIC (p) = (lev == 0);
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
  gcc_assert((parent == 0) == (lev == 0));
#if GCC45
  if (parent)
    DECL_STATIC_CHAIN (parent) = 1;
#endif
  DECL_CONTEXT (p) = parent;
  TREE_TYPE (p) = build_function_type (return_type, NULL_TREE);
  DECL_MODE (p) = FUNCTION_MODE;
  resultdecl = build_decl (RESULT_DECL, NULL_TREE, return_type);
  DECL_CONTEXT (resultdecl) = p;
  DECL_ARTIFICIAL (resultdecl) = 1;
  DECL_IGNORED_P (resultdecl) = 1;
  DECL_RESULT (p) = resultdecl;

  decl_attributes (&TREE_TYPE (p), cc, 0);

  BLOCK_SUPERCONTEXT (parm_block) = p;
  DECL_INITIAL (p) = parm_block;

  BLOCK_SUPERCONTEXT (top_block) = parm_block;
  BLOCK_SUBBLOCKS (parm_block) = top_block;

  gcc_assert (current_block == NULL_TREE);
  TREE_CHAIN (p) = global_decls;
  global_decls = p;

  current_function_decl = p;
  current_param_count = n_params;
}

static void
m3cg_begin_procedure (void)
{
  PROC (p);
  tree local;

  if (option_procs_trace)
    fprintf(stderr, "  procedure %s\n", IDENTIFIER_POINTER(DECL_NAME(p)));

  DECL_SOURCE_LOCATION (p) = input_location;

  announce_function (p);

  current_function_decl = p;
  allocate_struct_function (p, false);

  pending_blocks = tree_cons (NULL_TREE, current_block, pending_blocks);
  current_block = DECL_INITIAL (p); /* parm_block */
  TREE_USED (current_block) = 1;
  current_block = BLOCK_SUBBLOCKS (current_block); /* top_block */
  TREE_USED (current_block) = 1;

  pending_stmts = tree_cons (NULL_TREE, current_stmts, pending_stmts);
  current_stmts = alloc_stmt_list ();

  /* compile the locals we have already seen */
  for (local = BLOCK_VARS (current_block); local; local = TREE_CHAIN (local))
    {
      add_stmt (build1 (DECL_EXPR, t_void, local));
    }
}

static void
m3cg_end_procedure (void)
{
  PROC (p);

  gcc_assert (current_function_decl == p);

  if (option_procs_trace)
    fprintf(stderr, "  procedure %s\n", IDENTIFIER_POINTER(DECL_NAME(p)));

  /* Attach block to the function */
  gcc_assert (current_block == BLOCK_SUBBLOCKS (DECL_INITIAL (p)));
  DECL_SAVED_TREE (p) = build3 (BIND_EXPR, t_void,
                                BLOCK_VARS (current_block),
                                current_stmts, current_block);
  current_block = TREE_VALUE (pending_blocks);
  pending_blocks = TREE_CHAIN (pending_blocks);
  current_stmts = TREE_VALUE (pending_stmts);
  pending_stmts = TREE_CHAIN (pending_stmts);

  /* good line numbers for epilog */
  DECL_STRUCT_FUNCTION (p)->function_end_locus = input_location;

  current_function_decl = DECL_CONTEXT (p);

  if (current_block)
    {
      /* Register this function with cgraph just far enough to get it
         added to our parent's nested function list.  */
      (void) cgraph_node (p);
    }
  else
    /* We are not inside of any scope now. */
    {
      m3_gimplify_function (p);
      cgraph_finalize_function (p, false);
    }
}

static void
m3cg_begin_block (void)
{
  tree b = build_block (NULL_TREE, NULL_TREE, current_block, NULL_TREE);
  BLOCK_SUBBLOCKS (current_block)
    = chainon (BLOCK_SUBBLOCKS (current_block), b);
  BLOCK_SUPERCONTEXT(b) = current_block;
  TREE_USED (b) = 1;
  pending_blocks = tree_cons (NULL_TREE, current_block, pending_blocks);
  current_block = b;
  pending_stmts = tree_cons (NULL_TREE, current_stmts, pending_stmts);
  current_stmts = alloc_stmt_list ();
}

static void
m3cg_end_block (void)
{
  tree bind = build3 (BIND_EXPR, t_void,
                      BLOCK_VARS (current_block),
                      current_stmts, current_block);
  current_block = TREE_VALUE (pending_blocks);
  pending_blocks = TREE_CHAIN (pending_blocks);
  current_stmts = TREE_VALUE (pending_stmts);
  pending_stmts = TREE_CHAIN (pending_stmts);
  add_stmt (bind);
}

static void
m3cg_note_procedure_origin (void)
{
  UNUSED_PROC (p);

  fatal_error("note_procedure_origin psuedo-op encountered.");
}

static void
m3cg_set_label (void)
{
  LABEL   (l);
  BOOLEAN (barrier);

  DECL_CONTEXT (l) = current_function_decl;
  DECL_MODE (l) = VOIDmode;
  DECL_SOURCE_LOCATION (l) = input_location;

  if (barrier)
    {
      unsigned i = { 0 };
      rtx r = label_rtx (l);
      LABEL_PRESERVE_P (r) = 1;
      FORCED_LABEL (l) = 1;
      DECL_UNINLINABLE (current_function_decl) = 1;
      DECL_STRUCT_FUNCTION (current_function_decl)->has_nonlocal_label = 1;
#if GCC45
      /* ? */
      DECL_NONLOCAL(l) = true;
#else
      {
        rtx list = DECL_STRUCT_FUNCTION (current_function_decl)->x_nonlocal_goto_handler_labels;
        DECL_STRUCT_FUNCTION (current_function_decl)->x_nonlocal_goto_handler_labels
          = gen_rtx_EXPR_LIST (VOIDmode, r, list);
      }
#endif
      /* put asm("") before and after the label */
      for (i = 0; i < 2; ++i)
      {
        tree bar = make_node(ASM_EXPR);
        TREE_TYPE(bar) = t_void;
        ASM_STRING(bar) = build_string (0, "");
        ASM_VOLATILE_P (bar) = 1;
        add_stmt (bar);

        if (i == 0)
          add_stmt (build1 (LABEL_EXPR, t_void, l));
      }
    }
  else
    add_stmt (build1 (LABEL_EXPR, t_void, l));
}

static void
m3cg_jump (void)
{
  LABEL (l);

  add_stmt (build1 (GOTO_EXPR, t_void, l));
}

static void
m3cg_if_true (void)
{
  UNUSED_TYPE      (t);
  LABEL     (l);
  UNUSED_FREQUENCY (f);

  tree cond = m3_cast (boolean_type_node, EXPR_REF (-1));
  EXPR_POP ();

  add_stmt (build3 (COND_EXPR, t_void, cond,
                    build1 (GOTO_EXPR, t_void, l),
                    NULL_TREE));
}

static void
m3cg_if_false (void)
{
  UNUSED_TYPE      (t);
  LABEL     (l);
  UNUSED_FREQUENCY (f);

  tree cond = m3_cast (boolean_type_node, EXPR_REF (-1));
  EXPR_POP ();
  add_stmt (build3 (COND_EXPR, t_void, cond,
                    NULL_TREE,
                    build1 (GOTO_EXPR, t_void, l)));
}

static void
m3cg_if_compare (enum tree_code o)
{
  MTYPE     (t);
  LABEL     (l);
  UNUSED_FREQUENCY (f);

  tree t1 = m3_cast (t, EXPR_REF (-1));
  tree t2 = m3_cast (t, EXPR_REF (-2));
  add_stmt (build3 (COND_EXPR, t_void, build2 (o, boolean_type_node, t2, t1),
                    build1 (GOTO_EXPR, t_void, l),
                    NULL_TREE));
  EXPR_POP ();
  EXPR_POP ();
}

static void m3cg_if_eq (void) { m3cg_if_compare (EQ_EXPR); }
static void m3cg_if_ne (void) { m3cg_if_compare (NE_EXPR); }
static void m3cg_if_gt (void) { m3cg_if_compare (GT_EXPR); }
static void m3cg_if_ge (void) { m3cg_if_compare (GE_EXPR); }
static void m3cg_if_lt (void) { m3cg_if_compare (LT_EXPR); }
static void m3cg_if_le (void) { m3cg_if_compare (LE_EXPR); }

static void
m3cg_case_jump (void)
{
  MTYPE   (t);
  INTEGER (n);

  tree index_expr = EXPR_REF (-1);
  int i;
  tree body;

  pending_stmts = tree_cons (NULL_TREE, current_stmts, pending_stmts);
  current_stmts = alloc_stmt_list ();
  for (i = 0; i < n; i++) {
    LABEL (target_label);

#if GCC45
    tree case_label = create_artificial_label (UNKNOWN_LOCATION);
#else
    tree case_label = create_artificial_label ();
#endif
    add_stmt (build3 (CASE_LABEL_EXPR, t_void, build_int_cst (t_int, i),
                      NULL_TREE, case_label));
    add_stmt (build1 (GOTO_EXPR, t_void, target_label));
  }
  body = current_stmts;

  current_stmts = TREE_VALUE (pending_stmts);
  pending_stmts = TREE_CHAIN (pending_stmts);
  add_stmt (build3 (SWITCH_EXPR, t, index_expr, body, NULL_TREE));
  EXPR_POP();
}

static void
m3cg_exit_proc (void)
{
  MTYPE2 (t, T);

  tree res = NULL_TREE;
  if (t != t_void) {
    res = DECL_RESULT (current_function_decl);
    m3_store (res, 0, t, T, t, T);
  }
  add_stmt (build1 (RETURN_EXPR, t_void, res));
}

static void
m3cg_load (void)
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
    fprintf(stderr, "  m3cg_load (%s): offset 0x%lx, convert %s -> %s\n", name,
            o, m3cg_typename(src_T), m3cg_typename(dst_T));
  }
  m3_load (v, o, src_t, src_T, dst_t, dst_T);
}

static void
m3cg_load_address (void)
{
  enum tree_code plus = (GCC45 ? POINTER_PLUS_EXPR : PLUS_EXPR);
  VAR        (v);
  BYTEOFFSET (o);

  if (option_vars_trace) {
    const char *name = "noname";
    if (v != 0 && DECL_NAME(v) != 0) {
      name = IDENTIFIER_POINTER(DECL_NAME(v));
    }
    fprintf(stderr, "  load address (%s) offset 0x%lx\n", name, o);
  }
  TREE_USED (v) = 1;
  v = m3_build1 (ADDR_EXPR, t_addr, v);
  if (o != 0) {
    v = m3_build2 (plus, t_addr, v, size_int (o / BITS_PER_UNIT));
  }
  EXPR_PUSH (v);
}

static void
m3cg_load_indirect (void)
{
  BYTEOFFSET (o);
  MTYPE2     (src_t, src_T);
  MTYPE2     (dst_t, dst_T);

  tree v;
  enum tree_code plus = (GCC45 ? POINTER_PLUS_EXPR : PLUS_EXPR);

  if (option_vars_trace)
    fprintf(stderr, "  load address offset:0x%lx src_t:%s dst_t:%s\n",
            o, m3cg_typename(src_T), m3cg_typename(dst_T));

  v = EXPR_REF (-1);
  if (o != 0) {
    v = m3_build2 (plus, t_addr, v, size_int (o / BITS_PER_UNIT));
  }
  v = m3_cast (m3_build_pointer_type (src_t), v);
  v = m3_build1 (INDIRECT_REF, src_t, v);
  if (src_T != dst_T) {
    v = m3_convert (dst_t, v);
  }
  EXPR_REF (-1) = v;
}

static void
m3cg_store (void)
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
    fprintf(stderr, "  store (%s) offset:0x%lx src_t:%s dst_t:%s\n",
            name, o, m3cg_typename(src_T), m3cg_typename(dst_T));
  }
  m3_store (v, o, src_t, src_T, dst_t, dst_T);
}

static void
m3cg_store_indirect (void)
{
  BYTEOFFSET (o);
  MTYPE2 (src_t, src_T);
  MTYPE2 (dst_t, dst_T);

  tree v;
  enum tree_code plus = (GCC45 ? POINTER_PLUS_EXPR : PLUS_EXPR);

  if (option_vars_trace)
    fprintf(stderr, "  store indirect offset:0x%lx src_t:%s dst_t:%s\n",
            o, m3cg_typename(src_T), m3cg_typename(dst_T));

  v = EXPR_REF (-2);
  if (o != 0) {
    v = m3_build2 (plus, t_addr, v, size_int (o / BITS_PER_UNIT));
  }
  v = m3_cast (m3_build_pointer_type (dst_t), v);
  v = m3_build1 (INDIRECT_REF, dst_t, v);
  add_stmt (build2 (MODIFY_EXPR, dst_t, v,
                    m3_convert (dst_t,
                                m3_cast (src_t, EXPR_REF (-1)))));
  EXPR_POP ();
  EXPR_POP ();
}

static void
m3cg_load_nil (void)
{
  EXPR_PUSH (v_null);
}

static void
m3cg_load_integer (void)
{
  MTYPE          (t);
  TARGET_INTEGER (n);

  n = convert (t, n);
  EXPR_PUSH(n);
}

static void
m3cg_load_float (void)
{
  UNUSED_MTYPE (t);
  FLOAT (f, fkind);

  if (TREE_TYPE (f) != t) { f = m3_convert (t, f); }
  EXPR_PUSH (f);
}

static void
m3cg_compare (enum tree_code op)
{
  MTYPE (src_t);
  MTYPE (dst_t);

  tree t1 = m3_cast (src_t, EXPR_REF(-1));
  tree t2 = m3_cast (src_t, EXPR_REF(-2));

  EXPR_REF(-2) = m3_build2 (op, dst_t, t2, t1);
  EXPR_POP ();
}

static void m3cg_eq (void) { m3cg_compare (EQ_EXPR); }
static void m3cg_ne (void) { m3cg_compare (NE_EXPR); }
static void m3cg_gt (void) { m3cg_compare (GT_EXPR); }
static void m3cg_ge (void) { m3cg_compare (GE_EXPR); }
static void m3cg_lt (void) { m3cg_compare (LT_EXPR); }
static void m3cg_le (void) { m3cg_compare (LE_EXPR); }

static void
m3cg_add (void)
{
  MTYPE (t);

  EXPR_REF (-2) = m3_build2 (PLUS_EXPR, t,
                             m3_cast (t, EXPR_REF (-2)),
                             m3_cast (t, EXPR_REF (-1)));
  EXPR_POP ();
}

static void
m3cg_subtract (void)
{
  MTYPE (t);

  EXPR_REF (-2) = m3_build2 (MINUS_EXPR, t,
                             m3_cast (t, EXPR_REF (-2)),
                             m3_cast (t, EXPR_REF (-1)));
  EXPR_POP ();
}

static void
m3cg_multiply (void)
{
  MTYPE (t);

  EXPR_REF (-2) = m3_build2 (MULT_EXPR, t,
                             m3_cast (t, EXPR_REF (-2)),
                             m3_cast (t, EXPR_REF (-1)));
  EXPR_POP ();
}

static void
m3cg_divide (void)
{
  MTYPE (t);

  EXPR_REF (-2) = m3_build2 (RDIV_EXPR, t,
                             m3_cast (t, EXPR_REF (-2)),
                             m3_cast (t, EXPR_REF (-1)));
  EXPR_POP ();
}

static void
m3cg_negate (void)
{
  MTYPE (t);

  EXPR_REF (-1) = m3_build1 (NEGATE_EXPR, t, m3_cast (t, EXPR_REF (-1)));
}

static void
m3cg_abs (void)
{
  MTYPE (t);

  EXPR_REF (-1) = m3_build1 (ABS_EXPR, t, m3_cast (t, EXPR_REF (-1)));
}

static void
m3cg_max (void)
{
  MTYPE (t);

  tree t1 = declare_temp (t);
  tree t2 = declare_temp (t);

  add_stmt (m3_build2 (MODIFY_EXPR, t, t1, EXPR_REF (-1)));
  add_stmt (m3_build2 (MODIFY_EXPR, t, t2, EXPR_REF (-2)));

  EXPR_REF (-2) = m3_build3 (COND_EXPR, t,
                             m3_build2 (LE_EXPR, boolean_type_node, t2, t1),
                             t1, t2);
  EXPR_POP ();
}

static void
m3cg_min (void)
{
  MTYPE (t);

  tree t1 = declare_temp (t);
  tree t2 = declare_temp (t);

  add_stmt (m3_build2 (MODIFY_EXPR, t, t1, EXPR_REF (-1)));
  add_stmt (m3_build2 (MODIFY_EXPR, t, t2, EXPR_REF (-2)));

  EXPR_REF (-2) = m3_build3 (COND_EXPR, t,
                             m3_build2 (LE_EXPR, boolean_type_node, t1, t2),
                             t1, t2);
  EXPR_POP ();
}

static void
m3cg_round (void)
{
  MTYPE (src_t);
  MTYPE (dst_t);

  tree arg;
  tree cond;
  tree neg;
  tree pos;
  REAL_VALUE_TYPE r;

  arg = declare_temp (src_t);
  add_stmt (m3_build2 (MODIFY_EXPR, src_t, arg,
                       m3_cast (src_t, EXPR_REF(-1))));

  real_from_string (&r, "0.5");
  pos = build_real (src_t, r);

  real_from_string (&r, "-0.5");
  neg = build_real (src_t, r);

  cond = fold_build2 (GT_EXPR, boolean_type_node, arg,
                      build_real_from_int_cst (src_t, v_zero));

  EXPR_REF(-1) = m3_build1 (FIX_TRUNC_EXPR, dst_t,
                            m3_build2 (PLUS_EXPR, src_t, arg,
                                       m3_build3 (COND_EXPR, src_t,
                                                  cond, pos, neg)));
}

static void
m3cg_trunc (void)
{
  MTYPE (src_t);
  MTYPE (dst_t);

  EXPR_REF (-1) =
    m3_build1 (FIX_TRUNC_EXPR, dst_t, m3_cast (src_t, EXPR_REF (-1)));
}

static void
m3cg_floor (void)
{
  MTYPE (src_t);
  MTYPE (dst_t);

  tree arg;
  tree cond;
  tree intval;

  arg = declare_temp (src_t);
  add_stmt (m3_build2 (MODIFY_EXPR, src_t, arg,
                       m3_cast (src_t, EXPR_REF(-1))));

  intval = declare_temp (dst_t);
  add_stmt (m3_build2 (MODIFY_EXPR, dst_t, intval,
                       m3_build1 (FIX_TRUNC_EXPR, dst_t, arg)));

  cond = m3_build2 (LE_EXPR, boolean_type_node,
                    m3_build1 (FLOAT_EXPR, src_t, intval), arg);

  EXPR_REF(-1) = m3_build3 (COND_EXPR, dst_t, cond, intval,
                            m3_build2 (MINUS_EXPR, dst_t, intval,
                                       build_int_cst (dst_t, 1)));
}

static void
m3cg_ceiling (void)
{
  MTYPE (src_t);
  MTYPE (dst_t);

  tree arg;
  tree cond;
  tree intval;

  arg = declare_temp (src_t);
  add_stmt (m3_build2 (MODIFY_EXPR, src_t, arg,
                       m3_cast (src_t, EXPR_REF(-1))));

  intval = declare_temp (dst_t);
  add_stmt (m3_build2 (MODIFY_EXPR, dst_t, intval,
                       m3_build1 (FIX_TRUNC_EXPR, dst_t, arg)));

  cond = m3_build2 (GE_EXPR, boolean_type_node,
                    m3_build1 (FLOAT_EXPR, src_t, intval), arg);

  EXPR_REF(-1) = m3_build3 (COND_EXPR, dst_t, cond, intval,
                            m3_build2 (PLUS_EXPR, dst_t, intval,
                                       build_int_cst (dst_t, 1)));
}

static void
m3cg_cvt_float (void)
{
  MTYPE (src_t);
  MTYPE (dst_t);

  if (FLOAT_TYPE_P (src_t)) {
    EXPR_REF (-1) = m3_convert (dst_t, EXPR_REF (-1));
  } else {
    EXPR_REF (-1) = m3_build1 (FLOAT_EXPR, dst_t, EXPR_REF (-1));
  }
}

static void
m3cg_div (void)
{
  MTYPE2 (t, T);
#if 0
  SIGN   (a);
  SIGN   (b);
  if ((b == 'P' && a == 'P') || IS_WORD_TYPE(T)) {
    EXPR_REF (-2) = m3_build2 (FLOOR_DIV_EXPR, t,
                               m3_cast (t, EXPR_REF (-2)),
                               m3_cast (t, EXPR_REF (-1)));
    EXPR_POP ();
  } else {
    m3_start_call ();
    m3_pop_param (t);
    m3_pop_param (t);
    if (TYPE_SIZE (t) == TYPE_SIZE (long_long_integer_type_node))
      m3_call_direct (divL_proc, TREE_TYPE (TREE_TYPE (divL_proc)));
    else
      m3_call_direct (div_proc, TREE_TYPE (TREE_TYPE (div_proc)));
  }
#else
  UNUSED_SIGN (a);
  UNUSED_SIGN (b);
  EXPR_REF (-2) = m3_build2 (FLOOR_DIV_EXPR, t,
                             m3_cast (t, EXPR_REF (-2)),
                             m3_cast (t, EXPR_REF (-1)));
  EXPR_POP ();
#endif
}

static void
m3cg_mod (void)
{
  MTYPE2 (t, T);
#if 0
  SIGN   (a);
  SIGN   (b);
  if ((b == 'P' && a == 'P') || IS_WORD_TYPE(T)) {
    EXPR_REF (-2) = m3_build2 (FLOOR_MOD_EXPR, t,
                               m3_cast (t, EXPR_REF (-2)),
                               m3_cast (t, EXPR_REF (-1)));
    EXPR_POP ();
  } else {
    m3_start_call ();
    m3_pop_param (t);
    m3_pop_param (t);
    if (TYPE_SIZE (t) == TYPE_SIZE (long_long_integer_type_node))
      m3_call_direct (modL_proc, TREE_TYPE (TREE_TYPE (modL_proc)));
    else
      m3_call_direct (mod_proc, TREE_TYPE (TREE_TYPE (mod_proc)));
  }
#else
  UNUSED_SIGN (a);
  UNUSED_SIGN (b);
  EXPR_REF (-2) = m3_build2 (FLOOR_MOD_EXPR, t,
                             m3_cast (t, EXPR_REF (-2)),
                             m3_cast (t, EXPR_REF (-1)));
  EXPR_POP ();
#endif
}

static void
m3cg_set_union (void)
{
  BYTESIZE (n);

  setop (set_union_proc, n, 3);
}

static void
m3cg_set_difference (void)
{
  BYTESIZE (n);

  setop (set_diff_proc, n, 3);
}

static void
m3cg_set_intersection (void)
{
  BYTESIZE (n);

  setop (set_inter_proc, n, 3);
}

static void
m3cg_set_sym_difference (void)
{
  BYTESIZE (n);

  setop (set_sdiff_proc, n, 3);
}

static tree
m3cg_bits_per_integer(void)
{
    static tree t;
    return (t = (t ? t : build_int_cst(t_word, BITS_PER_INTEGER)));
}

static tree
m3cg_bytes_per_integer(void)
{
    static tree t;
    return (t = (t ? t : build_int_cst(t_word, BITS_PER_INTEGER / BITS_PER_UNIT)));
}

static tree
m3cg_set_member_ref(tree* out_bit_in_word)
{
  /* Common code for set_member and set_singleton, something like:
    set_member_ref(const size_t* set, size_t bit_index, tree* bit_in_word)
    {
      grain = (sizeof(size_t) * 8)
      *bit_in_word = (((size_t)1) << (bit_index % grain));
      return set[bit_index / grain];
    }
  */

  UNUSED_BYTESIZE (n);
  MTYPE    (type);
  tree bit         = m3_cast(t_word, EXPR_REF (-1));
  tree set         = m3_cast(t_addr, EXPR_REF (-2));

  /* div and mod work as well as shifting, even when not optimizing. */

  tree bits_per_integer = m3cg_bits_per_integer();
  tree bytes_per_integer = m3cg_bytes_per_integer();
  tree word        = m3_build2(TRUNC_DIV_EXPR, t_word, bit, bits_per_integer);
  tree bit_in_word = m3_build2(TRUNC_MOD_EXPR, t_word, bit, bits_per_integer);
  tree byte        = m3_build2(MULT_EXPR, t_word, word, bytes_per_integer);
  tree word_ref    = m3_build2(POINTER_PLUS_EXPR, t_addr, set, byte);
  tree one         = m3_cast(t_word, v_one);

  word_ref         = m3_build1(INDIRECT_REF, t_word, word_ref);
  *out_bit_in_word = m3_build2(LSHIFT_EXPR, t_word, one, bit_in_word);
  gcc_assert (type == t_int);
  EXPR_POP ();
  EXPR_POP ();
  return stabilize_reference(word_ref); /* stabilize: avoid recomputing */
}

static void
m3cg_set_member (void)
{
  /* Equivalent to
    int set_member(const size_t* set, size_t bit_index)
    {
      grain = (sizeof(size_t) * 8);
      return ((set[bit_index  / grain] & (((size_t)1) << (bit_index % grain))) != 0);
    }
  */
  tree bit_in_word;
  tree word_ref = m3cg_set_member_ref(&bit_in_word);
  tree t = m3_build2(BIT_AND_EXPR, t_word, word_ref, bit_in_word);
  t = m3_build2(NE_EXPR, boolean_type_node, t, m3_cast(t_word, v_zero));
  EXPR_PUSH(t);
}

static void
m3cg_set_compare (tree proc)
{
  BYTESIZE (n);
  MTYPE    (t);

  gcc_assert (t == t_int);
  setop (proc, n, 2);
}
static void m3cg_set_gt (void) { m3cg_set_compare (set_gt_proc); }
static void m3cg_set_ge (void) { m3cg_set_compare (set_ge_proc); }
static void m3cg_set_lt (void) { m3cg_set_compare (set_lt_proc); }
static void m3cg_set_le (void) { m3cg_set_compare (set_le_proc); }

static void m3cg_set_eq (void)
{
  BYTESIZE (n);
  MTYPE    (t);

  gcc_assert (t == t_int);
  m3_start_call ();
  m3_pop_param (t_addr);
  m3_pop_param (t_addr);
  EXPR_PUSH (size_int (n / BITS_PER_UNIT));
  m3_pop_param (t_int);
  m3_call_direct (memcmp_proc, TREE_TYPE (TREE_TYPE (memcmp_proc)));
  EXPR_REF(-1) = m3_build2 (EQ_EXPR, t_int, EXPR_REF(-1), v_zero);
}

static void m3cg_set_ne (void)
{
  BYTESIZE (n);
  MTYPE    (t);

  gcc_assert (t == t_int);
  m3_start_call ();
  m3_pop_param (t_addr);
  m3_pop_param (t_addr);
  EXPR_PUSH (size_int (n / BITS_PER_UNIT));
  m3_pop_param (t_int);
  m3_call_direct (memcmp_proc, TREE_TYPE (TREE_TYPE (memcmp_proc)));
  EXPR_REF(-1) = m3_build2 (NE_EXPR, t_int, EXPR_REF(-1), v_zero);
}

static void
m3cg_set_range (void)
{
  UNUSED_BYTESIZE (n);
  MTYPE    (t);

  gcc_assert (t == t_int);
  setop2 (set_range_proc, 3);
}

static void
m3cg_set_singleton (void)
{
  /* Equivalent to
    int set_singleton(size_t* set, size_t bit_index)
    {
      grain = (sizeof(size_t) * 8);
      set[bit_index / grain] |= (((size_t)1) << (bit_index % grain));
    }
  */
  tree bit_in_word;
  tree word_ref = m3cg_set_member_ref(&bit_in_word);
  tree t = m3_build2(BIT_IOR_EXPR, t_word, word_ref, bit_in_word);
  t = m3_build2(MODIFY_EXPR, t_word, word_ref, t);
  add_stmt(t);
}

static void
m3cg_not (void)
{
  MTYPE (t);

  EXPR_REF (-1) = m3_build1 (BIT_NOT_EXPR, t, m3_cast (t, EXPR_REF (-1)));
}

static void
m3cg_and (void)
{
  MTYPE (t);

  EXPR_REF (-2) = m3_build2 (BIT_AND_EXPR, t,
                             m3_cast (t, EXPR_REF (-2)),
                             m3_cast (t, EXPR_REF (-1)));
  EXPR_POP ();
}

static void
m3cg_or (void)
{
  MTYPE (t);

  EXPR_REF (-2) = m3_build2 (BIT_IOR_EXPR, t,
                             m3_cast (t, EXPR_REF (-2)),
                             m3_cast (t, EXPR_REF (-1)));
  EXPR_POP ();
}

static void
m3cg_xor (void)
{
  MTYPE (t);

  EXPR_REF (-2) = m3_build2 (BIT_XOR_EXPR, t,
                             m3_cast (t, EXPR_REF (-2)),
                             m3_cast (t, EXPR_REF (-1)));
  EXPR_POP ();
}

static void
m3cg_shift (void)
{
  MTYPE (t);

  tree n = declare_temp (t_int);
  tree x = declare_temp (t);

  gcc_assert (INTEGRAL_TYPE_P (t));

  add_stmt (m3_build2 (MODIFY_EXPR, t_int, n, EXPR_REF(-1)));
  add_stmt (m3_build2 (MODIFY_EXPR, t, x, EXPR_REF(-2)));

  EXPR_REF(-2) = m3_build3 (COND_EXPR, m3_unsigned_type (t),
                            m3_build2 (GE_EXPR, boolean_type_node, n, v_zero),
                            m3_do_shift (LSHIFT_EXPR, t, x, n),
                            m3_do_shift (RSHIFT_EXPR, t, x,
                                         m3_build1 (NEGATE_EXPR, t_int, n)));
  EXPR_POP();
}

static void
m3cg_shift_left (void)
{
  MTYPE (t);

  gcc_assert (INTEGRAL_TYPE_P (t));

  EXPR_REF (-2) = m3_do_shift (LSHIFT_EXPR, t, EXPR_REF (-2), EXPR_REF (-1));
  EXPR_POP ();
}

static void
m3cg_shift_right (void)
{
  MTYPE (t);

  gcc_assert (INTEGRAL_TYPE_P (t));

  EXPR_REF (-2) = m3_do_shift (RSHIFT_EXPR, t, EXPR_REF (-2), EXPR_REF (-1));
  EXPR_POP ();
}

static void
m3cg_rotate (void)
{
  MTYPE (t);

  tree n = declare_temp (t_int);
  tree x = declare_temp (t);

  gcc_assert (INTEGRAL_TYPE_P (t));

  add_stmt (m3_build2 (MODIFY_EXPR, t_int, n, EXPR_REF(-1)));
  add_stmt (m3_build2 (MODIFY_EXPR, t, x, EXPR_REF(-2)));

  EXPR_REF(-2) = m3_build3 (COND_EXPR, t,
                            m3_build2 (GE_EXPR, boolean_type_node, n, v_zero),
                            m3_do_rotate (LROTATE_EXPR, t, x, n),
                            m3_do_rotate (RROTATE_EXPR, t, x,
                                          m3_build1 (NEGATE_EXPR, t_int, n)));
  EXPR_POP();
}

static void
m3cg_rotate_left (void)
{
  MTYPE (t);

  gcc_assert (INTEGRAL_TYPE_P (t));

  EXPR_REF (-2) = m3_do_rotate (LROTATE_EXPR, t, EXPR_REF (-2), EXPR_REF (-1));
  EXPR_POP ();
}

static void
m3cg_rotate_right (void)
{
  MTYPE (t);

  gcc_assert (INTEGRAL_TYPE_P (t));

  EXPR_REF (-2) = m3_do_rotate (RROTATE_EXPR, t, EXPR_REF (-2), EXPR_REF (-1));
  EXPR_POP ();
}

static void
m3cg_widen (void)
{
  BOOLEAN (sign);

  tree dst_t = (sign ? t_int_64 : t_word_64);
  tree src_t  = (sign ? t_int_32 : t_word_32);

  EXPR_REF(-1) = m3_convert (dst_t,
                             m3_cast (src_t, EXPR_REF(-1)));
}

static void
m3cg_chop (void)
{
  EXPR_REF(-1) = m3_convert (t_int_32,
                             m3_build2 (BIT_AND_EXPR, t_int_64, EXPR_REF(-1),
                                        build_int_cst (t_int, 0xffffffff)));
}

static void
m3cg_extract (void)
{
  MTYPE   (t);
  BOOLEAN (sign_extend);

  if (option_trace_all)
    fprintf(stderr, " extract sign_extend:%u\n", (unsigned)sign_extend);

  gcc_assert (INTEGRAL_TYPE_P (t));

  t = sign_extend ? m3_signed_type (t) : m3_unsigned_type (t);
  EXPR_REF (-3) = m3_do_extract (EXPR_REF (-3), EXPR_REF (-2), EXPR_REF (-1), t);
  EXPR_POP ();
  EXPR_POP ();
}

static void
m3cg_extract_n (void)
{
  MTYPE   (t);
  BOOLEAN (sign_extend);
  INTEGER (n);

  gcc_assert (INTEGRAL_TYPE_P (t));
  gcc_assert (n >= 0);
  gcc_assert (n <= 64);
  gcc_assert (n <= TYPE_PRECISION (t));

  if (option_trace_all)
    fprintf(stderr, " extract_n count:%u sign_extend:%u\n",
            (unsigned)n, (unsigned)sign_extend);

  if (n == 0)
    EXPR_REF (-2) = m3_cast(t, v_zero);
  else
  {
    t = sign_extend ? m3_signed_type (t) : m3_unsigned_type (t);
    EXPR_REF (-2) = m3_do_extract (EXPR_REF (-2), EXPR_REF (-1),
                                   build_int_cst (t_int, n), t);
  }
  EXPR_POP ();
}

static tree
m3_do_fixed_extract (tree x, int m, int n, tree t)
{
  /* ??? Use BIT_FIELD_REF ???  */
  int a = TYPE_PRECISION (t) - n;
  int b = TYPE_PRECISION (t) - n - m;

  gcc_assert (m >= 0);
  gcc_assert (n > 0);
  gcc_assert (m <= 64);
  gcc_assert (n <= 64);
  gcc_assert ((m + n) <= 64);
  gcc_assert (m <= TYPE_PRECISION (t));
  gcc_assert (n <= TYPE_PRECISION (t));
  gcc_assert ((m + n) <= TYPE_PRECISION (t));

  if ((a < 0) || (a >= TYPE_PRECISION (t)) ||
      (b < 0) || (b >= TYPE_PRECISION (t)))
    {
      return m3_do_extract (x,
                            build_int_cst (t_int, m),
                            build_int_cst (t_int, n),
                            t);
    }

  x = m3_convert (t, x);
  x = (b ? m3_build2 (LSHIFT_EXPR, t, x, build_int_cst (t_int, b)) : x);
  x = (a ? m3_build2 (RSHIFT_EXPR, t, x, build_int_cst (t_int, a)) : x);
  return x;
}

static void
m3cg_extract_mn (void)
{
  MTYPE   (t);
  BOOLEAN (sign_extend);
  INTEGER (m);
  INTEGER (n);

  gcc_assert (INTEGRAL_TYPE_P (t));
  gcc_assert (m >= 0);
  gcc_assert (n >= 0);
  gcc_assert (m <= 64);
  gcc_assert (n <= 64);
  gcc_assert ((m + n) <= 64);
  gcc_assert (m <= TYPE_PRECISION (t));
  gcc_assert (n <= TYPE_PRECISION (t));
  gcc_assert ((m + n) <= TYPE_PRECISION (t));

  if (option_trace_all)
    fprintf(stderr, " extract_mn offset:%u count:%u sign_extend:%u\n",
            (unsigned)m, (unsigned)n, (unsigned)sign_extend);

  if (n == 0)
    EXPR_REF (-1) = m3_cast(t, v_zero);
  else
  {
    t = sign_extend ? m3_signed_type (t) : m3_unsigned_type (t);
    EXPR_REF (-1) = m3_do_fixed_extract (EXPR_REF (-1), m, n, t);
  }
}

static void
m3cg_insert (void)
{
  MTYPE (t);

  gcc_assert (INTEGRAL_TYPE_P (t));

  EXPR_REF (-4) = m3_do_insert (EXPR_REF (-4), EXPR_REF (-3),
                                EXPR_REF (-2), EXPR_REF (-1), t);
  EXPR_POP ();
  EXPR_POP ();
  EXPR_POP ();
}

static void
m3cg_insert_n (void)
{
  MTYPE   (t);
  INTEGER (n);

  gcc_assert (INTEGRAL_TYPE_P (t));
  gcc_assert (n >= 0);

  if (option_trace_all)
    fprintf(stderr, " insert_n count:%u\n", (unsigned)n);

  EXPR_REF (-3) = m3_do_insert (EXPR_REF (-3), EXPR_REF (-2),
                                EXPR_REF (-1), build_int_cst (t_int, n), t);
  EXPR_POP ();
  EXPR_POP ();
}

static void
m3cg_insert_mn (void)
{
  MTYPE   (t);
  INTEGER (m);
  INTEGER (n);

  gcc_assert (INTEGRAL_TYPE_P (t));
  gcc_assert (m >= 0);
  gcc_assert (n >= 0);
  
  /* workaround the fact that we use
   * insert_mn on uninitialized variables.
   */
  m3_next_store_volatile = true;

  if (option_trace_all)
    fprintf(stderr, " insert_mn offset:%u count:%u\n", (unsigned)m,
           (unsigned)n);

  EXPR_REF (-2) = m3_do_fixed_insert (EXPR_REF (-2), EXPR_REF (-1), m, n, t);
  EXPR_POP ();
}

static void
m3cg_swap (void)
{
  UNUSED_MTYPE (t);
  UNUSED_MTYPE (u);

  m3_swap ();
}

static void
m3cg_pop (void)
{
  UNUSED_MTYPE (t);

  EXPR_POP ();
}

static void
m3cg_copy_n (void)
{
  MTYPE (cnt_t);
  MTYPE (mem_t);
  BOOLEAN (overlap);

  gcc_assert (cnt_t == t_int);
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
               TYPE_SIZE_UNIT (mem_t));
  m3_pop_param (cnt_t);
  m3_call_direct (overlap ? memmove_proc : memcpy_proc, t_void);
}

static void
m3cg_copy (void)
{
  INTEGER (n);
  MTYPE (t);
  UNUSED_BOOLEAN (overlap);

  tree pts;
  tree ts = make_node (LANG_TYPE);
  int s = n * TREE_INT_CST_LOW (TYPE_SIZE (t));

  TYPE_SIZE (ts) = size_int (s);
  TYPE_SIZE_UNIT (ts) = size_binop (FLOOR_DIV_EXPR, TYPE_SIZE(ts),
                                    size_int(BITS_PER_UNIT));
  TYPE_ALIGN (ts) = TYPE_ALIGN (t);

  if (FLOAT_TYPE_P (t)) {
    SET_TYPE_MODE (ts, mode_for_size (s, MODE_FLOAT, 0));
  } else {
    SET_TYPE_MODE (ts, BLKmode);
  }

  pts = m3_build_pointer_type (ts);

  add_stmt (build2 (MODIFY_EXPR, t,
                    m3_build1 (INDIRECT_REF, ts,
                               m3_cast (pts, EXPR_REF (-2))),
                    m3_build1 (INDIRECT_REF, ts,
                               m3_cast (pts, EXPR_REF (-1)))));
  EXPR_POP ();
  EXPR_POP ();
}

static void
m3cg_zero_n (void)
{
  MTYPE (cnt_t);
  MTYPE (mem_t);

  gcc_assert (cnt_t == t_int);
  EXPR_REF(-1) = m3_build2(MULT_EXPR, t_int, EXPR_REF(-1),
                           TYPE_SIZE_UNIT(mem_t));

  m3_start_call ();
  m3_swap ();
  m3_pop_param (t_addr);
  EXPR_PUSH (v_zero);
  m3_pop_param (t_int);
  m3_pop_param (t_int);
  m3_call_direct (memset_proc, t_void);
}

static void
m3cg_zero (void)
{
  INTEGER (n);
  MTYPE   (mem_t);

  m3_start_call ();
  m3_pop_param (t_addr);
  EXPR_PUSH (v_zero);
  m3_pop_param (t_int);
  EXPR_PUSH (m3_build2 (MULT_EXPR, t_int, size_int(n), TYPE_SIZE_UNIT (mem_t)));
  m3_pop_param (t_int);
  m3_call_direct (memset_proc, t_void);
}

static void
m3cg_loophole (void)
{
  MTYPE2 (t, T);
  MTYPE2 (u, U);

  if (FLOAT_TYPE_P (t) != FLOAT_TYPE_P (u)) {
    tree v = declare_temp (t);
    m3_store (v, 0, t, T, t, T);
    m3_load (v, 0, u, U, u, U);
  } else {
    EXPR_REF (-1) = m3_cast (u, EXPR_REF (-1));
  }
}

static void
m3cg_abort (void)
{
  INTEGER (code);

  add_stmt (generate_fault (code));
}

static void
m3cg_check_nil (void)
{
  INTEGER (code);

  tree temp1 = declare_temp (t_addr);

  m3_store (temp1, 0, t_addr, T_addr, t_addr, T_addr);
  EXPR_PUSH (temp1);
  add_stmt (build3 (COND_EXPR, t_void,
                    m3_build2 (EQ_EXPR, boolean_type_node, temp1, v_null),
                    generate_fault (code),
                    NULL_TREE));
}

static void
m3cg_check_lo (void)
{
  MTYPE2         (t, T);
  TARGET_INTEGER (a);
  INTEGER        (code);

  tree temp1 = declare_temp (t);

  a = convert (t, a);

  if (option_exprs_trace)
    fprintf (stderr, "  check low type:%s code:0x%lx\n", m3cg_typename(T), code);

  if (TREE_TYPE (EXPR_REF (-1)) != t)
    EXPR_REF (-1) = m3_convert (t, EXPR_REF (-1));

  m3_store (temp1, 0, t, T, t, T);
  EXPR_PUSH (temp1);
  add_stmt (build3 (COND_EXPR, t_void,
                    m3_build2 (LT_EXPR, boolean_type_node, temp1, a),
                    generate_fault (code),
                    NULL_TREE));
}

static void
m3cg_check_hi (void)
{
  MTYPE2         (t, T);
  TARGET_INTEGER (a);
  INTEGER        (code);

  tree temp1 = declare_temp (t);

  a = convert (t, a);

  if (option_exprs_trace)
    fprintf (stderr, "  check high type:%s code:0x%lx\n", m3cg_typename(T), code);

  if (TREE_TYPE (EXPR_REF (-1)) != t)
    EXPR_REF (-1) = m3_convert (t, EXPR_REF (-1));

  m3_store (temp1, 0, t, T, t, T);
  EXPR_PUSH (temp1);
  add_stmt (build3 (COND_EXPR, t_void,
                    m3_build2 (GT_EXPR, boolean_type_node, temp1, a),
                    generate_fault (code),
                    NULL_TREE));
}

static void
m3cg_check_range (void)
{
  MTYPE2         (t, T);
  TARGET_INTEGER (a);
  TARGET_INTEGER (b);
  INTEGER        (code);

  tree temp1 = declare_temp (t);

  a = convert (t, a);
  b = convert (t, b);

  if (option_exprs_trace)
    fprintf (stderr, "  check range type:%s code:0x%lx\n", m3cg_typename(T), code);

  if (TREE_TYPE (EXPR_REF (-1)) != t)
    EXPR_REF (-1) = m3_convert (t, EXPR_REF (-1));

  m3_store (temp1, 0, t, T, t, T);
  EXPR_PUSH (temp1);
  add_stmt (build3 (COND_EXPR, t_void,
                    m3_build2 (TRUTH_ORIF_EXPR, boolean_type_node,
                               m3_build2 (LT_EXPR,
                                          boolean_type_node, temp1, a),
                               m3_build2 (GT_EXPR,
                                          boolean_type_node, temp1, b)),
                    generate_fault (code),
                    NULL_TREE));
}

static void
m3cg_check_index (void)
{
  MTYPE   (t);
  INTEGER (code);

  t = m3_unsigned_type (t);
  add_stmt (build3 (COND_EXPR, t_void,
                    m3_build2 (GE_EXPR, boolean_type_node,
                               convert (t, EXPR_REF (-2)),
                               convert (t, EXPR_REF (-1))),
                    generate_fault (code),
                    NULL_TREE));
  EXPR_POP();
}

static void
m3cg_check_eq (void)
{
  MTYPE2  (t, T);
  INTEGER (code);

  tree temp1 = declare_temp (t);
  tree temp2 = declare_temp (t);

  m3_store (temp1, 0, t, T, t, T);
  m3_store (temp2, 0, t, T, t, T);
  add_stmt (build3 (COND_EXPR, t_void,
                    m3_build2 (NE_EXPR, boolean_type_node, temp1, temp2),
                    generate_fault (code),
                    NULL_TREE));
}

static void
m3cg_add_offset (void)
{
  enum tree_code plus = (GCC45 ? POINTER_PLUS_EXPR : PLUS_EXPR);
  BYTESIZE (n);

  if (option_vars_trace)
    fprintf(stderr, "  add offset 0x%lx\n", n);

  EXPR_REF (-1) = m3_build2 (plus, t_addr,
                             EXPR_REF (-1), size_int (n / BITS_PER_UNIT));
}

static void
m3cg_index_address (void)
{
  tree a = { 0 };
  bool signd = { 0 };
  long bytes = { 0 };

  MTYPE2   (t, T);
  BYTESIZE (bits);
  
  bytes = (bits / BITS_PER_UNIT);

  if (option_vars_trace)
    fprintf(stderr, "  index address n_bytes:0x%lx type:%s\n",
            bytes, m3cg_typename(T));

  signd = IS_INTEGER_TYPE_TREE(t);
  gcc_assert(signd || IS_WORD_TYPE_TREE(t));
  a = (signd ? ssize_int (bytes) : size_int (bytes));
  a = m3_build2 (MULT_EXPR, t, EXPR_REF (-1), a);
  if (IS_INTEGER_TYPE_TREE(t))
    a = m3_cast (ssizetype, a);
  a = m3_cast (sizetype, a);
  EXPR_REF (-2) = m3_build2 (POINTER_PLUS_EXPR, t_addr, m3_cast (t_addr, EXPR_REF (-2)), a);
  EXPR_POP ();
}

static void
m3cg_start_call_direct (void)
{
  UNUSED_PROC    (p);
  INTEGER (level);
  UNUSED_MTYPE2  (t, m3t);

  if (option_procs_trace)
    fprintf(stderr, "  start call procedure:%s, level:0x%lx, type:%s\n",
            IDENTIFIER_POINTER(DECL_NAME(p)), level, m3cg_typename(m3t));

  m3_start_call ();
}

static void
m3cg_call_direct (void)
{
  PROC  (p);
  MTYPE2  (t, m3t);

  if (option_procs_trace)
    fprintf(stderr, "  call procedure:%s type:%s\n",
            IDENTIFIER_POINTER(DECL_NAME(p)), m3cg_typename(m3t));

  m3_call_direct (p, t);
}

static void
m3cg_start_call_indirect (void)
{
  UNUSED_MTYPE2 (t, m3t);
  UNUSED_CC (cc);

  if (option_procs_trace)
    fprintf(stderr, "  start call procedure indirect type:%s\n", m3cg_typename(m3t));

  m3_start_call ();
}

static void
m3cg_call_indirect (void)
{
  MTYPE2 (t, m3t);
  CC (cc);

  if (option_procs_trace)
    fprintf(stderr, "  call procedure indirect type:%s\n", m3cg_typename(m3t));

  m3_call_indirect (t, cc);
}

static void
m3cg_pop_param (void)
{
  MTYPE2 (t, m3t);

  if (option_vars_trace)
    fprintf(stderr, "  pop param type:%s\n", m3cg_typename(m3t));

  m3_pop_param (t);
}

static void
m3cg_pop_struct (void)
{
  BYTESIZE  (s);
  ALIGNMENT (a);

  tree t = m3_build_type (T_struct, s, a);

  if (option_vars_trace)
    fprintf(stderr, "  pop struct size:0x%lx alignment:0x%lx\n", s, a);

  EXPR_REF (-1) = m3_build1 (INDIRECT_REF, t,
                             m3_cast (m3_build_pointer_type (t), EXPR_REF (-1)));
  m3_pop_param (t);
}

static void
m3cg_pop_static_link (void)
{
  tree v = declare_temp (t_addr);
  /* volatile otherwise gcc complains it is not intialized */
  m3_store_volatile (v, 0, t_addr, T_addr, t_addr, T_addr);
  CALL_TOP_STATIC_CHAIN () = v;
}

static void
m3cg_load_procedure (void)
{
  PROC (p);

  if (option_procs_trace)
    fprintf(stderr, "  load procedure %s\n", IDENTIFIER_POINTER(DECL_NAME(p)));

  EXPR_PUSH (proc_addr (p));
}

static void
m3cg_load_static_link (void)
{
  PROC (p);

  if (option_procs_trace)
    fprintf(stderr, "  load link %s\n", IDENTIFIER_POINTER(DECL_NAME(p)));

  EXPR_PUSH (build1 (STATIC_CHAIN_EXPR, t_addr, p));
}

static void
m3cg_comment (void)
{
  UNUSED_QUOTED_STRING (comment, len);

  if (option_misc_trace)
    fprintf(stderr, "  comment: `%s'\n", comment);
}

typedef enum { Relaxed, Release, Acquire, AcquireRelease, Sequential } Order;

static void
m3cg_store_ordered (void)
{
  MTYPE2 (src_t, src_T);
  MTYPE2 (dst_t, dst_T);
  INTEGER (order);

  tree v;

  if (order != Relaxed)
    warning (0, "only Relaxed memory order for stores is supported");

  v = EXPR_REF (-2);
  v = m3_cast (m3_build_pointer_type (dst_t), v);
  v = m3_build1 (INDIRECT_REF, dst_t, v);
  add_stmt (build2 (MODIFY_EXPR, dst_t, v,
                    m3_convert (dst_t,
                                m3_cast (src_t, EXPR_REF (-1)))));
  EXPR_POP ();
  EXPR_POP ();
}

static void
m3cg_load_ordered (void)
{
  MTYPE2     (src_t, src_T);
  MTYPE2     (dst_t, dst_T);
  INTEGER    (order);

  tree v;

  if (order != Relaxed)
    warning (0, "only Relaxed memory order for loads is supported");

  v = EXPR_REF (-1);
  v = m3_cast (m3_build_pointer_type (src_t), v);
  v = m3_build1 (INDIRECT_REF, src_t, v);
  if (src_T != dst_T) {
    v = m3_convert (dst_t, v);
  }
  EXPR_REF (-1) = v;
}

static void
m3cg_exchange (void)
{
  MTYPE2     (t, T);
  MTYPE2     (u, U);
  INTEGER    (order);

  int size;
  enum built_in_function fncode = BUILT_IN_LOCK_TEST_AND_SET_N;
  /* SYNCH_LOCK_TEST_AND_SET is an acquire barrier */

  if (!INTEGRAL_TYPE_P (t) && !POINTER_TYPE_P (t))
    goto incompatible;
  size = tree_low_cst (TYPE_SIZE_UNIT (t), 1);
  if (size != 1 && size != 2 && size != 4 && size != 8)
    goto incompatible;

  if (order == Sequential || order == AcquireRelease) {
    /* is this enough to make it Sequential or just AcquireRelease */
    m3_start_call ();
    m3_call_direct (built_in_decls[BUILT_IN_SYNCHRONIZE], t_void);
  }
  m3_start_call ();
  m3_pop_param (u);
  m3_pop_param (t_addr);
  CALL_TOP_ARG() = nreverse(CALL_TOP_ARG());
  CALL_TOP_TYPE() = nreverse(CALL_TOP_TYPE());
  m3_call_direct (built_in_decls[fncode + exact_log2 (size) + 1], u);
  return;

 incompatible:
  fatal_error ("incompatible type for argument to atomic op");
}

static void
m3cg_compare_exchange (void)
{
  MTYPE2         (t, T);
  MTYPE2         (u, U);
  MTYPE2         (r, R);
  UNUSED_INTEGER (success);
  UNUSED_INTEGER (failure);

  tree v;
  int size;
  enum built_in_function fncode = BUILT_IN_BOOL_COMPARE_AND_SWAP_N;

  if (!INTEGRAL_TYPE_P (t) && !POINTER_TYPE_P (t))
    goto incompatible;
  size = tree_low_cst (TYPE_SIZE_UNIT (t), 1);
  if (size != 1 && size != 2 && size != 4 && size != 8)
    goto incompatible;

  v = EXPR_REF (-2);
  v = m3_cast (m3_build_pointer_type (t), v);
  v = m3_build1 (INDIRECT_REF, t, v);
  if (t != u) {
    v = m3_convert (u, v);
  }
  EXPR_REF (-2) = v;

  m3_start_call ();
  m3_pop_param (u);
  m3_pop_param (u);
  m3_pop_param (t_addr);
  CALL_TOP_ARG() = nreverse(CALL_TOP_ARG());
  CALL_TOP_TYPE() = nreverse(CALL_TOP_TYPE());
  m3_call_direct (built_in_decls[fncode + exact_log2 (size) + 1], r);
  return;

 incompatible:
  fatal_error ("incompatible type for argument to atomic op");
}

static void
m3cg_fence (void)
{
  UNUSED_INTEGER (order);

  m3_start_call ();
  m3_call_direct (built_in_decls[BUILT_IN_SYNCHRONIZE], t_void);
}

static void
m3cg_fetch_and_op (enum built_in_function fncode)
{
  MTYPE2         (t, T);
  MTYPE2         (u, U);
  UNUSED_INTEGER (order);

  int size;

  if (!INTEGRAL_TYPE_P (t) && !POINTER_TYPE_P (t))
    goto incompatible;
  size = tree_low_cst (TYPE_SIZE_UNIT (t), 1);
  if (size != 1 && size != 2 && size != 4 && size != 8)
    goto incompatible;

  m3_start_call ();
  m3_pop_param (t);
  m3_pop_param (t_addr);
  CALL_TOP_ARG() = nreverse(CALL_TOP_ARG());
  CALL_TOP_TYPE() = nreverse(CALL_TOP_TYPE());
  m3_call_direct (built_in_decls[fncode + exact_log2 (size) + 1], u);
  return;

 incompatible:
  fatal_error ("incompatible type for argument to atomic op");
}

static void
m3cg_fetch_and_add (void) { m3cg_fetch_and_op (BUILT_IN_FETCH_AND_ADD_N); }
static void
m3cg_fetch_and_sub (void) { m3cg_fetch_and_op (BUILT_IN_FETCH_AND_SUB_N); }
static void
m3cg_fetch_and_or  (void) { m3cg_fetch_and_op (BUILT_IN_FETCH_AND_OR_N); }
static void
m3cg_fetch_and_and (void) { m3cg_fetch_and_op (BUILT_IN_FETCH_AND_AND_N); }
static void
m3cg_fetch_and_xor (void) { m3cg_fetch_and_op (BUILT_IN_FETCH_AND_XOR_N); }

#if 0
static void
m3cg_lock_test_and_set (void)
{
  MTYPE2     (t, T);

  int size;
  enum built_in_function fncode = BUILT_IN_LOCK_TEST_AND_SET_N;

  if (!INTEGRAL_TYPE_P (t) && !POINTER_TYPE_P (t))
    goto incompatible;
  size = tree_low_cst (TYPE_SIZE_UNIT (t), 1);
  if (size != 1 && size != 2 && size != 4 && size != 8)
    goto incompatible;

  m3_start_call ();
  m3_pop_param (t);
  m3_pop_param (t_addr);
  CALL_TOP_ARG() = nreverse(CALL_TOP_ARG());
  CALL_TOP_TYPE() = nreverse(CALL_TOP_TYPE());
  m3_call_direct (built_in_decls[fncode + exact_log2 (size) + 1], t);
  return;

 incompatible:
  fatal_error ("incompatible type for argument to atomic op");
}

static void
m3cg_lock_release (void)
{
  MTYPE2     (t, T);

  int size;
  enum built_in_function fncode = BUILT_IN_LOCK_RELEASE_N;

  if (!INTEGRAL_TYPE_P (t) && !POINTER_TYPE_P (t))
    goto incompatible;
  size = tree_low_cst (TYPE_SIZE_UNIT (t), 1);
  if (size != 1 && size != 2 && size != 4 && size != 8)
    goto incompatible;

  m3_start_call ();
  m3_pop_param (t_addr);
  m3_call_direct (built_in_decls[fncode + exact_log2 (size) + 1], t_void);
  return;

 incompatible:
  fatal_error ("incompatible type for argument to atomic op");
}
#endif

/*----------------------------------------------------------- M3CG parser ---*/

typedef void (*OP_HANDLER) (void);
typedef struct { M3CG_opcode op;  OP_HANDLER proc; } OpProc;

static const OpProc ops[] = {
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
  { M3CG_JUMP,                   m3cg_jump                   },
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
  { M3CG_SHIFT,                  m3cg_shift                  },
  { M3CG_SHIFT_LEFT,             m3cg_shift_left             },
  { M3CG_SHIFT_RIGHT,            m3cg_shift_right            },
  { M3CG_ROTATE,                 m3cg_rotate                 },
  { M3CG_ROTATE_LEFT,            m3cg_rotate_left            },
  { M3CG_ROTATE_RIGHT,           m3cg_rotate_right           },
  { M3CG_WIDEN,                  m3cg_widen                  },
  { M3CG_CHOP,                   m3cg_chop                   },
  { M3CG_EXTRACT,                m3cg_extract                },
  { M3CG_EXTRACT_N,              m3cg_extract_n              },
  { M3CG_EXTRACT_MN,             m3cg_extract_mn             },
  { M3CG_INSERT,                 m3cg_insert                 },
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
  { M3CG_STORE_ORDERED,          m3cg_store_ordered          },
  { M3CG_LOAD_ORDERED,           m3cg_load_ordered           },
  { M3CG_EXCHANGE,               m3cg_exchange               },
  { M3CG_COMPARE_EXCHANGE,       m3cg_compare_exchange       },
  { M3CG_FENCE,                  m3cg_fence                  },
  { M3CG_FETCH_AND_ADD,          m3cg_fetch_and_add          },
  { M3CG_FETCH_AND_SUB,          m3cg_fetch_and_sub          },
  { M3CG_FETCH_AND_OR,           m3cg_fetch_and_or           },
  { M3CG_FETCH_AND_AND,          m3cg_fetch_and_and          },
  { M3CG_FETCH_AND_XOR,          m3cg_fetch_and_xor          },
  { LAST_OPCODE,                 0                           }
  };

static void
m3_parse_file (int xx ATTRIBUTE_UNUSED)
{
  int op, i;

  /* first, verify the handler table is complete and consistent. */
  for (i = 0;  ops[i].proc != 0;  i++ ) {
    gcc_assert (i == (int)ops[i].op);
  }
  gcc_assert (i == (int)LAST_OPCODE);


  /* check the version stamp */
  i = get_int ();
  if (i != M3CG_Version) {
    fatal_error (" *** bad M3CG version stamp (0x%x), expected 0x%x",
                 i, M3CG_Version);
  }

  op = (int)LAST_OPCODE;
  while (op != (int)M3CG_END_UNIT) {
    op = get_int ();
    if (op < 0 || (int)LAST_OPCODE <= op)
      fatal_error (" *** bad opcode: 0x%x, at m3cg_lineno %d", op, m3cg_lineno);

    if (option_opcodes_trace)
      fprintf (stderr, "(%d) %s\n", m3cg_lineno, M3CG_opnames[op]);

    ops[op].proc ();
    m3cg_lineno ++;
  }

  if (GCC45)
  {
    write_global_declarations();
  }
  else
  {
    cgraph_finalize_compilation_unit ();
    cgraph_optimize ();
  }
}

/*===================================================== RUNTIME FUNCTIONS ===*/

/* Prepare to handle switches.  */
static unsigned int
m3_init_options (unsigned int argc ATTRIBUTE_UNUSED,
                 const char **argv ATTRIBUTE_UNUSED)
{
  return CL_m3cg;
}

static int version_done;
static const char const language_string[] = "M3CG - Modula-3 Compiler back end";

/* Process a switch - called by opts.c.  */
static int
m3_handle_option (size_t scode, const char *arg ATTRIBUTE_UNUSED, int value)
{
  enum opt_code code = (enum opt_code) scode;

  switch (code)
    {
    default:
      return 1;
    case OPT_v:
      if (!version_done)
        {
          char const * const lang = language_string; /* type check */
          char const * const ver = version_string; /* type check */
          printf ("%s %s\n", lang, ver);
          version_done = 1;
        }
      break;

    case OPT_y:
      option_trace_all         = 1;
      option_opcodes_trace     = 1;
      option_source_line_trace = 1;
      option_vars_trace        = 1;
      option_procs_trace       = 1;
      option_exprs_trace       = 1;
      option_misc_trace        = 1;
      option_types_trace       = 1;
      break;

    case OPT_fopcodes_trace:
      option_opcodes_trace = value;
      break;

    case OPT_fsource_line_trace:
      option_source_line_trace = value;
      break;

    case OPT_fvars_trace:
      option_vars_trace = value;
      break;

    case OPT_fprocs_trace:
      option_procs_trace = value;
      break;

    case OPT_fexprs_trace:
      option_exprs_trace = value;
      break;

    case OPT_fmisc_trace:
      option_misc_trace = value;
      break;

    case OPT_ftypes_trace:
      option_types_trace = value;
      break;
    }

  return 1;
}

/* Post-switch processing. */
bool
m3_post_options (const char **pfilename ATTRIBUTE_UNUSED)
{
  /* These optimizations break our exception handling? */
  flag_reorder_blocks = 0;
  flag_reorder_blocks_and_partition = 0;
  
  if (M3_ALL_VOLATILE)
  {
    /* Make all loads/stores volatile like we used to,
     * which greatly hampers optimization, but
     * we don't then have to disable any particular
     * optimizations below.
     */
  }
  else
  {
    /* Turn off some optimizations that cause problems,
     * unless we make all loads/stores volatile.
     */
    flag_tree_vrp = 0; /* value range propagation, causes problems? */
    flag_tree_fre = 0; /* full redundancy elimination, causes problems? */

    flag_tree_loop_im = 0; /* loop Invariant Motion, breaks compiling Trestle */
#ifdef INSN_SCHEDULING
    flag_schedule_insns = 0; /* fails to compile, surprising */
#endif
    /*flag_strict_aliasing = 0;*/ /* consider? */
    /*flag_strict_overflow = 0;*/ /* consider? */
    flag_tree_ter = 0; /* fails to compile m3core CopySign */
    /*flag_delete_null_pointer_checks = 0;*/ /* consider? */
    /*flag_reorder_functions = 0;*/ /* consider? */
  }

  /* otherwise:
MODULE Main;

PROCEDURE F1() =

    PROCEDURE NestedUnused1() =
    BEGIN
    END NestedUnused1;

BEGIN
  IF FALSE THEN
    NestedUnused1();
  END;
END F1;

BEGIN
  F1();
END Main.
Undefined symbols:
  "_Main__F1__NestedUnused1.496", referenced from:
      _L_1 in Main.mo
  */
  flag_unit_at_a_time = 0;

  /* partial redundancy elmination;
     crashes compiling m3-tools/cvsup/server/FSServer.m3 */
  flag_tree_pre = 0;

#if GCC45
  /* Excess precision other than "fast" requires front-end support.  */
  flag_excess_precision_cmdline = EXCESS_PRECISION_FAST;
#endif

  return false;
}

/* Language dependent parser setup.  */

static bool
m3_init (void)
{
#ifdef M3_USE_MAPPED_LOCATION
  #undef input_filename /* global input_filename is not an lvalue. */
  const char* input_filename = { 0 };
  linemap_add (line_table, LC_ENTER, false, main_input_filename, 1);
#endif
  input_filename = main_input_filename;

  /* Open input file.  */
  if (input_filename == 0 || !strcmp (input_filename, "-"))
    {
      finput = stdin;
#ifdef M3_USE_MAPPED_LOCATION
      linemap_add (line_table, LC_RENAME, false, "<stdin>", 1);
#endif
      input_filename = "<stdin>";
    }
  else
    finput = fopen (input_filename, "rb");
  if (finput == 0)
    {
      fprintf (stderr, "Unable to open input file %s\n", input_filename);
      exit(1);
    }
  m3_init_lex ();
  m3_init_parse ();
  m3_init_decl_processing ();
  return true;
}

/* Language dependent wrapup.  */

static void
m3_finish (void)
{
  if (finput != NULL)
    {
      fclose (finput);
      finput = NULL;
    }
}

#if GCC_APPLE
int flag_iasm_blocks;
enum iasm_states iasm_state;
bool iasm_in_operands;
struct cpp_reader* parse_in;
#define add_stmt c_add_stmt
#include "../stub-objc.c"
c_language_kind c_language;

/* This is used by cfstring code; providing it here makes us not have to #if 0 out a few bits. */
tree pushdecl_top_level (tree x ATTRIBUTE_UNUSED) { gcc_unreachable(); return NULL_TREE; }

#endif

/* New garbage collection regime see gty.texi.  */
#include "debug.h"
#include "gtype-m3cg.h"
#include "gt-m3cg-parse.h"
