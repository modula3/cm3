/*

    Modula-3 Compiler back end parser.  

    ---------------------------------------------------------------------

    Copyright (C) 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

    This program is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the Free
    Software Foundation; either version 2, or (at your option) any later
    version.

    This program is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
    or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
    for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, 59
    Temple Place - Suite 330, Boston, MA 02111-1307, USA.

    In other words, you are welcome to use, share and improve this program.
    You are forbidden to forbid anyone else to use, share and improve what you
    give them.  Help stamp out software-hoarding!

*/

#include <stdio.h>
#include <errno.h>
#include <setjmp.h>

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "output.h"
#include "rtl.h"
#include "ggc.h"
#include "toplev.h"
#include "varray.h"
#include "function.h"
#include "expr.h"
#include "input.h"
#include "real.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "target.h"
#include "opts.h"
#include "options.h"

#include "m3cg.h"

/*================================================================= TREES ===*/

typedef enum
{
  T_word_8,  T_int_8,
  T_word_16, T_int_16,
  T_word_32, T_int_32,
  T_word_64, T_int_64,
  T_reel, T_lreel, T_xreel,
  T_addr, T_struct, T_void,
  T_word, T_int,
  T_LAST
}
m3_type;

enum m3_tree_index
{
  /* Types. */
  M3TI_ADDR,
  M3TI_WORD,
  M3TI_INT,
  M3TI_REEL,
  M3TI_LREEL,
  M3TI_XREEL,
  M3TI_INT_8,
  M3TI_INT_16,
  M3TI_INT_32,
  M3TI_INT_64,
  M3TI_WORD_8,
  M3TI_WORD_16,
  M3TI_WORD_32,
  M3TI_WORD_64,
  M3TI_VOID,

  /* Values. */
  M3TI_ZERO,
  M3TI_ONE,
  M3TI_NULL,

  /* Procedures. */
  M3TI_MEMCPY,
  M3TI_MEMMOVE,
  M3TI_MEMSET,
  M3TI_DIV,
  M3TI_MOD,
  M3TI_SET_UNION,
  M3TI_SET_DIFF,
  M3TI_SET_INTER,
  M3TI_SET_SDIFF,
  M3TI_SET_EQ,
  M3TI_SET_NE,
  M3TI_SET_GT,
  M3TI_SET_GE,
  M3TI_SET_LT,
  M3TI_SET_LE,
  M3TI_SET_MEMBER,
  M3TI_SET_RANGE,
  M3TI_SET_SING,
  M3TI_FAULT_PROC,
  M3TI_FAULT_HANDLER,

  /* Miscellaneous. */
  M3TI_GLOBAL_VARS,
  M3TI_DEBUG_FIELDS,
  M3TI_CURRENT_BLOCK,
  M3TI_CURRENT_RECORD_TYPE,
  M3TI_CURRENT_RECORD_VALS,
  M3TI_CURRENT_SEGMENT,
  M3TI_FAULT_INTF,
  M3TI_PENDING_BLOCKS,

  M3TI_MAX
};

static GTY (()) tree m3_global_trees[M3TI_MAX];

#define t_addr		m3_global_trees[M3TI_ADDR]
#define t_word		m3_global_trees[M3TI_WORD]
#define t_int		m3_global_trees[M3TI_INT]
#define t_reel		m3_global_trees[M3TI_REEL]
#define t_lreel		m3_global_trees[M3TI_LREEL]
#define t_xreel		m3_global_trees[M3TI_XREEL]
#define t_int_8		m3_global_trees[M3TI_INT_8]
#define t_int_16	m3_global_trees[M3TI_INT_16]
#define t_int_32	m3_global_trees[M3TI_INT_32]
#define t_int_64	m3_global_trees[M3TI_INT_64]
#define t_word_8	m3_global_trees[M3TI_WORD_8]
#define t_word_16	m3_global_trees[M3TI_WORD_16]
#define t_word_32	m3_global_trees[M3TI_WORD_32]
#define t_word_64	m3_global_trees[M3TI_WORD_64]
#define t_void		m3_global_trees[M3TI_VOID]

#define v_zero		m3_global_trees[M3TI_ZERO]
#define v_one		m3_global_trees[M3TI_ONE]
#define v_null		m3_global_trees[M3TI_NULL]

#define memcpy_proc	m3_global_trees[M3TI_MEMCPY]
#define memmove_proc	m3_global_trees[M3TI_MEMMOVE]
#define memset_proc	m3_global_trees[M3TI_MEMSET]
#define div_proc	m3_global_trees[M3TI_DIV]
#define mod_proc	m3_global_trees[M3TI_MOD]
#define set_union_proc	m3_global_trees[M3TI_SET_UNION]
#define set_diff_proc	m3_global_trees[M3TI_SET_DIFF]
#define set_inter_proc	m3_global_trees[M3TI_SET_INTER]
#define set_sdiff_proc	m3_global_trees[M3TI_SET_SDIFF]
#define set_eq_proc	m3_global_trees[M3TI_SET_EQ]
#define set_ne_proc	m3_global_trees[M3TI_SET_NE]
#define set_gt_proc	m3_global_trees[M3TI_SET_GT]
#define set_ge_proc	m3_global_trees[M3TI_SET_GE]
#define set_lt_proc	m3_global_trees[M3TI_SET_LT]
#define set_le_proc	m3_global_trees[M3TI_SET_LE]
#define set_member_proc	m3_global_trees[M3TI_SET_MEMBER]
#define set_range_proc	m3_global_trees[M3TI_SET_RANGE]
#define set_sing_proc	m3_global_trees[M3TI_SET_SING]
#define fault_proc	m3_global_trees[M3TI_FAULT_PROC]
#define fault_handler	m3_global_trees[M3TI_FAULT_HANDLER]

#define global_vars	m3_global_trees[M3TI_GLOBAL_VARS]
#define debug_fields	m3_global_trees[M3TI_DEBUG_FIELDS]
#define current_block	m3_global_trees[M3TI_CURRENT_BLOCK]
#define current_record_type	m3_global_trees[M3TI_CURRENT_RECORD_TYPE]
#define current_record_vals	m3_global_trees[M3TI_CURRENT_RECORD_VALS]
#define current_segment	m3_global_trees[M3TI_CURRENT_SEGMENT]
#define fault_intf	m3_global_trees[M3TI_FAULT_INTF]
#define pending_blocks	m3_global_trees[M3TI_PENDING_BLOCKS]

/* Types expected by gcc's garbage collector.
   These types exist to allow language front-ends to
   add extra information in gcc's parse tree data structure.
   But the treelang front end doesn't use them -- it has
   its own parse tree data structure.
   We define them here only to satisfy gcc's garbage collector.  */

/* Language-specific identifier information.  */

struct lang_identifier GTY(())
{
  struct tree_identifier common;
};

/* Language-specific tree node information.  */

union lang_tree_node 
  GTY((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE")))
{
  union tree_node GTY ((tag ("0"), 
			desc ("tree_node_structure (&%h)"))) 
    generic;
  struct lang_identifier GTY ((tag ("1"))) identifier;
};

/* Language-specific type information.  */

struct lang_type GTY(())
{
  char junk; /* dummy field to ensure struct is not empty */
};

/* Language-specific declaration information.  */

struct lang_decl GTY(())
{
  char junk; /* dummy field to ensure struct is not empty */
};

struct language_function GTY(())
{
  char junk; /* dummy field to ensure struct is not empty */
};

static tree m3_truthvalue_conversion (tree expr);
static bool m3_mark_addressable (tree exp);
static tree m3_type_for_size (unsigned precision, int unsignedp);
static tree m3_type_for_mode (enum machine_mode, int unsignedp);
static tree m3_unsigned_type (tree type_node);
static tree m3_signed_type (tree type_node);
static tree m3_signed_or_unsigned_type (int unsignedp, tree type);
static HOST_WIDE_INT m3_get_alias_set (tree);
static unsigned int m3_init_options (unsigned int argc, const char **argv);
static int m3_handle_option (size_t scode, const char *arg, int value);
static bool m3_init (void);
static void m3_parse_file (int);
static void m3_finish (void);

/* XXX these should be static */
void pushlevel (int ignore);
tree poplevel (int keep, int reverse, int functionbody);
int global_bindings_p (void);
void insert_block (tree block);
void set_block (tree block);
tree pushdecl (tree decl);
tree getdecls (void);

/* The front end language hooks (addresses of code for this front
   end).  These are not really very language-dependent, i.e.
   treelang, C, Mercury, etc. can all use almost the same definitions.  */

#undef LANG_HOOKS_TRUTHVALUE_CONVERSION
#define LANG_HOOKS_TRUTHVALUE_CONVERSION m3_truthvalue_conversion
#undef LANG_HOOKS_MARK_ADDRESSABLE
#define LANG_HOOKS_MARK_ADDRESSABLE m3_mark_addressable
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
const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

/* Tree code type/name/code tables.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

const char tree_code_type[] = {
#include "tree.def"
  'x'
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

static HOST_WIDE_INT
m3_get_alias_set (tree t ATTRIBUTE_UNUSED)
{
  return 0;
}

static tree
m3_cast (tree tipe, tree op0)
{
  return fold (build1 (NOP_EXPR, tipe, op0));
}

static tree
m3_build1 (enum tree_code code, tree tipe, tree op0)
{
  return fold (build1 (code, tipe, op0));
}

static tree
m3_build2 (enum tree_code code, tree tipe, tree op0, tree op1)
{
  return fold (build (code, tipe, op0, op1));
}

static tree
m3_build3 (enum tree_code code, tree tipe, tree op0, tree op1, tree op2)
{
  return fold (build (code, tipe, op0, op1, op2));
}

static tree
m3_build_int (int n)
{
  if (n == 0)
    return v_zero;

  if (n == 1)
    return v_one;

  if ((BITS_PER_WORD > sizeof (int) * 8) && (n < 0))
    {
      return build_int_2 (n, ~0);
    }
  else
    {
      return build_int_2 (n, 0);
    }
}

static bool
m3_is_small_cardinal (tree t, HOST_WIDE_INT *n)
{
  if (TREE_CODE (t) != INTEGER_CST || TREE_INT_CST_HIGH (t) != 0)
    return false;
  *n = TREE_INT_CST_LOW (t);
  return true;
}

static tree
m3_build_type (m3_type t, int s, int a)
{
  switch (t)
    {
    case T_word:
      switch (s)
	{
	case 0:
	  return t_word;
	case 8:
	  return t_word_8;
	case 16:
	  return t_word_16;
	case 32:
	  return t_word_32;
	case 64:
	  return t_word_64;
	default:
	  if (s == BITS_PER_WORD) return t_word;
	}
      break;

    case T_int:
      switch (s)
	{
	case 0:
	  return t_int;
	case 8:
	  return t_int_8;
	case 16:
	  return t_int_16;
	case 32:
	  return t_int_32;
	case 64:
	  return t_int_64;
	default:
	  if (s == BITS_PER_WORD) return t_int;
	}
      break;

    case T_addr:
      return t_addr;
    case T_reel:
      return t_reel;
    case T_lreel:
      return t_lreel;
    case T_xreel:
      return t_xreel;
    case T_int_8:
      return t_int_8;
    case T_int_16:
      return t_int_16;
    case T_int_32:
      return t_int_32;
    case T_int_64:
      return t_int_64;
    case T_word_8:
      return t_word_8;
    case T_word_16:
      return t_word_16;
    case T_word_32:
      return t_word_32;
    case T_word_64:
      return t_word_64;
    case T_void:
      return t_void;

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

  abort ();
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
                 m3_build2 (EQ_EXPR, t_int, n, m3_build_int (BITS_PER_WORD)),
                 y, j);
  l = m3_build3 (COND_EXPR, t,
                 m3_build2 (EQ_EXPR, t_int, n, v_zero),
                 x, k);
  return l;
}

static tree
left_shift (tree t, int i)
{
  if (i)
    t = m3_build2 (LSHIFT_EXPR, t_word, t, m3_build_int (i));
  return t;
}

static tree
m3_do_fixed_insert (tree x, tree y, int i, int n, tree t)
{
  /* ??? Use BIT_FIELD_REF ??? */
  HOST_WIDE_INT y_val;

  if ((i < 0) || (BITS_PER_WORD <= i) || (n < 0) || (BITS_PER_WORD <= n))
    {
      return m3_do_insert (x, y, m3_build_int (i), m3_build_int (n), t);
    }

  if (n == 0)
    return x;

  if ((n == 1) && (i < (int)(sizeof (int) * 8)))
    {
      if (m3_is_small_cardinal (y, &y_val))
	{
	  if (y_val & 1)
	    {
	      return m3_build2 (BIT_IOR_EXPR, t, x,
				m3_build_int (1 << i));
	    }
	  else
	    {
	      return m3_build2 (BIT_AND_EXPR, t, x,
				m3_build_int (~(1 << i)));
	    }
	}
      else
	{			/* non-constant, 1-bit value */
	  tree a, b;
	  a = m3_build2 (BIT_AND_EXPR, t, y, v_one);
	  b = m3_build2 (BIT_AND_EXPR, t, x, m3_build_int (~(1 << i)));
	  return m3_build2 (BIT_IOR_EXPR, t, b, left_shift (a, i));
	}
    }
  else
    {				/* multi-bit value */
      tree saved_bits, new_bits;
      if (i + n < (int)(sizeof (int) * 8))
	{
	  int mask = (1 << n) - 1;
	  saved_bits = m3_build_int (~(mask << i));
	  if (m3_is_small_cardinal (y, &y_val))
	    {
	      new_bits = m3_build_int ((y_val & mask) << i);
	    }
	  else
	    {
	      new_bits = m3_build2 (BIT_AND_EXPR, t, y,
				    m3_build_int (mask));
	      new_bits = left_shift (new_bits, i);
	    };
	}
      else if (n < (int)(sizeof (int) * 8))
	{
	  int mask = (1 << n) - 1;
	  tree a = m3_build_int (mask);
	  if (m3_is_small_cardinal (y, &y_val))
	    {
	      new_bits = m3_build_int (y_val & mask);
	    }
	  else
	    {
	      new_bits = m3_build2 (BIT_AND_EXPR, t, y,
				    m3_build_int (mask));
	    };
	  new_bits = left_shift (new_bits, i);
	  saved_bits = m3_build1 (BIT_NOT_EXPR, t, left_shift (a, i));
	}
      else
	{			/* n >= sizeof(int)*8 */
	  tree mask;
	  mask = m3_build2 (LSHIFT_EXPR, t, m3_build_int (~0L),
			    m3_build_int (n));
	  mask = m3_build1 (BIT_NOT_EXPR, t, mask);
	  new_bits = left_shift (m3_build2 (BIT_AND_EXPR, t, y, mask), i);
	  saved_bits = m3_build1 (BIT_NOT_EXPR, t, left_shift (mask, i));
	};
      x = m3_build2 (BIT_AND_EXPR, t, x, saved_bits);
      return m3_build2 (BIT_IOR_EXPR, t, x, new_bits);
    }
}

static tree
m3_do_extract (tree x, tree i, tree n, tree t, int sign_extend)
{
  tree a, b, c, d, e, f;

  a = m3_build2 (MINUS_EXPR, t, m3_build_int (BITS_PER_WORD), n);
  b = m3_build2 (MINUS_EXPR, t, a, i);
  c = m3_build1 (CONVERT_EXPR, m3_unsigned_type (t), x);
  d = m3_build2 (LSHIFT_EXPR, m3_unsigned_type (t), c, b);
  e = m3_build2 (RSHIFT_EXPR, 
                 sign_extend ? m3_signed_type (t) : m3_unsigned_type (t),
		 d, a);
  f = m3_build3 (COND_EXPR, t,
		 m3_build2 (EQ_EXPR, t, n, v_zero),
		 v_zero, e);
  return f;
}

static tree
m3_do_fixed_extract (tree x, int i, int n, tree t, int sign_extend)
{
  /* ??? Use BIT_FIELD_REF ???  */
  int a = BITS_PER_WORD - n;
  int b = BITS_PER_WORD - n - i;
  tree c, d, e;

  if ((a < 0) || (BITS_PER_WORD <= a) || (b < 0) || (BITS_PER_WORD <= b))
    {
      return m3_do_extract (x, m3_build_int (i),
			    m3_build_int (n), t, sign_extend);
    }

  c = m3_build1 (CONVERT_EXPR, m3_unsigned_type (t), x);
  d = (b == 0) ? c : m3_build2 (LSHIFT_EXPR, m3_unsigned_type (t), c, 
                                m3_build_int (b));
  e = (a == 0) ? d : 
    m3_build2 (RSHIFT_EXPR,
	       sign_extend ? m3_signed_type (t) : m3_unsigned_type (t),
               d, m3_build_int (a));
  return e;
}

static tree
m3_do_rotate (tree val, tree cnt, int right, tree t)
{
  /* ??? Use LROTATE_EXPR/RROTATE_EXPR.  */
  tree a, b, c, d, e, f, g;

  t = m3_unsigned_type (t);
  a = m3_build_int (BITS_PER_WORD - 1);
  b = m3_build2 (BIT_AND_EXPR, t, cnt, a);
  c = m3_build2 (MINUS_EXPR, t, m3_build_int (BITS_PER_WORD), b);
  d = m3_build1 (CONVERT_EXPR, t, val);
  e = m3_build2 (LSHIFT_EXPR, t, d, (right) ? c : b);
  f = m3_build2 (RSHIFT_EXPR, t, d, (right) ? b : c);
  g = m3_build2 (BIT_IOR_EXPR, t, e, f);
  return g;
}

static tree
m3_do_shift (tree val, tree cnt, int right, tree t)
{
  tree a, b, c, d;
  HOST_WIDE_INT cnt_val;

  t = m3_unsigned_type (t);
  a = m3_build1 (CONVERT_EXPR, t, val);
  b = m3_build2 ((right) ? RSHIFT_EXPR : LSHIFT_EXPR, t, a, cnt);
  if (m3_is_small_cardinal (cnt, &cnt_val)
      && (0 <= cnt_val) && (cnt_val < BITS_PER_WORD))
    {
      return b;
    };
  c = m3_build2 (GE_EXPR, t, cnt, m3_build_int (BITS_PER_WORD));
  d = m3_build3 (COND_EXPR, t, c, v_zero, b);
  return d;
}

static tree
m3_build_real (const char *value, tree tipe)
{
  tree x;

  if (tipe == t_reel)
    x = build_real (tipe, REAL_VALUE_ATOF (value, SFmode));
  else if (tipe == t_lreel)
    x = build_real (tipe, REAL_VALUE_ATOF (value, DFmode));
# if 0
  else if (tipe == t_xreel)
    x = build_real (tipe, REAL_VALUE_ATOF (value, XFmode));
# endif
  else
    abort ();
  return x;
}

/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or validate its data type for an `if' or `while' statement or ?..: exp.

   This preparation consists of taking the ordinary
   representation of an expression expr and producing a valid tree
   boolean expression describing whether expr is nonzero.  We could
   simply always do build_binary_op (NE_EXPR, expr, truthvalue_false_node, 1),
   but we optimize comparisons, &&, ||, and !.

   The resulting type should always be `truthvalue_type_node'.  */

tree
m3_truthvalue_conversion (tree expr)
{
  if (TREE_CODE (expr) == ERROR_MARK)
    return expr;

  return m3_build2 (NE_EXPR, t_int, expr, integer_zero_node);
}

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
	TREE_ADDRESSABLE (x) = 1;
	return true;

      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	put_var_into_stack (x, /*rescan=*/true);

	/* drops in */
      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;
	/* drops out */
      default:
	return true;
      }
}

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
  if (! INTEGRAL_TYPE_P (type) || TREE_UNSIGNED (type) == unsignedp)
    return type;
  else
    return m3_type_for_size (TYPE_PRECISION (type), unsignedp);
}

/* Return non-zero if we are currently in the global binding level.  */

int
global_bindings_p (void)
{
  return current_block == 0;
}

/* Return the list of declarations in the current level. Note that this list
   is in reverse order (it has to be so for back-end compatibility).  */

tree
getdecls (void)
{
  return current_block ? BLOCK_VARS (current_block) : global_vars;
}

/* Enter a new binding level. The input parameter is ignored, but has to be
   specified for back-end compatibility.  */

void
pushlevel (int ignore ATTRIBUTE_UNUSED)
{
  abort ();
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

tree
poplevel (int keep ATTRIBUTE_UNUSED,
	  int reverse ATTRIBUTE_UNUSED,
	  int functionbody ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside the BIND_EXPR.  */

void
insert_block (tree block)
{
  TREE_USED (block) = 1;
  BLOCK_SUBBLOCKS (current_block)
    = chainon (BLOCK_SUBBLOCKS (current_block), block);
}

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */

void
set_block (tree block ATTRIBUTE_UNUSED)
{
  abort ();
}

/* Records a ..._DECL node DECL as belonging to the current lexical scope.
   Returns the ..._DECL node. */

tree
pushdecl (tree decl ATTRIBUTE_UNUSED)
{
  abort ();
}


static void
m3_push_type_decl(tree id, tree type_node)
{
  tree decl = build_decl (TYPE_DECL, id, type_node);
  TYPE_NAME (type_node) = decl;
  TYPE_STUB_DECL (type_node) = decl;
}

/* push_atomic_type_decl() ensures that the type's type is itself. 
   Needed for DBX.  Must only be used for atomic types,
   not for e.g. pointer or array types.  */

static void
m3_push_atomic_type_decl(tree id, tree type_node)
{
  TREE_TYPE (type_node) = type_node;
  m3_push_type_decl (id, type_node);
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

tree
builtin_function (const char *name, tree type, int function_code,
		  enum built_in_class class, const char *library_name,
		  tree attrs ATTRIBUTE_UNUSED)
{
  tree decl = build_decl (FUNCTION_DECL, get_identifier (name), type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  if (library_name)
    SET_DECL_ASSEMBLER_NAME (decl, get_identifier (library_name));
  DECL_BUILT_IN_CLASS (decl) = class;
  DECL_FUNCTION_CODE (decl) = function_code;
  return decl;
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

  build_common_tree_nodes (0);

  t_int_8 = make_signed_type (8);
  m3_push_atomic_type_decl (get_identifier ("int_8"), t_int_8);
  t_word_8 = make_unsigned_type (8);
  m3_push_atomic_type_decl (get_identifier ("word_8"), t_word_8);
  t_int_16 = make_signed_type (16);
  m3_push_atomic_type_decl (get_identifier ("int_16"), t_int_16);
  t_word_16 = make_unsigned_type (16);
  m3_push_atomic_type_decl (get_identifier ("word_16"), t_word_16);
  t_int_32 = make_signed_type (32);
  m3_push_atomic_type_decl (get_identifier ("int_32"), t_int_32);
  t_word_32 = make_unsigned_type (32);
  m3_push_atomic_type_decl (get_identifier ("word_32"), t_word_32);
  t_int_64 = make_signed_type (64);
  m3_push_atomic_type_decl (get_identifier ("int_64"), t_int_64);
  t_word_64 = make_unsigned_type (64);
  m3_push_atomic_type_decl (get_identifier ("word_64"), t_word_64);

  if (BITS_PER_WORD == 32)
    {
      t_int = t_int_32;
      t_word = t_word_32;
    }
  else if (BITS_PER_WORD == 64)
    {
      t_int = t_int_64;
      t_word = t_word_64;
    }
  else
    {
      t_int = make_signed_type (BITS_PER_WORD);
      m3_push_atomic_type_decl (get_identifier ("int"), t_int);
      t_word = make_unsigned_type (BITS_PER_WORD);
      m3_push_atomic_type_decl (get_identifier ("word"), t_word);
    }

  /* Set the type used for sizes and build the remaining common nodes. */
  set_sizetype (t_int);
  build_common_tree_nodes_2 (0);

  /* Build the remaining M3-specific type and value nodes. */
  t_addr = ptr_type_node;
  m3_push_atomic_type_decl (get_identifier ("addr"), t_addr);
  t_reel = float_type_node;
  m3_push_atomic_type_decl (get_identifier ("reel"), t_reel);
  t_lreel = double_type_node;
  m3_push_atomic_type_decl (get_identifier ("lreel"), t_lreel);
#if 0
  /* XXX The M3 front end (m3middle/src/Target.m3) seems to treat extended
     reals the same as LONGREAL.  That may be due to limitations in other
     parts of the front end.  I don't know yet.  For now we likewise treat
     the xreel type as if it were lreel. */
  t_xreel = long_double_type_node;
#else
  t_xreel = double_type_node;
#endif
  m3_push_atomic_type_decl (get_identifier ("xreel"), t_xreel);

  t_void = void_type_node;
  v_zero = integer_zero_node;
  v_one = integer_one_node;
  v_null = null_pointer_node;

  t = build_function_type_list (t_addr, t_addr, t_addr, t_int, NULL_TREE);
  memcpy_proc = builtin_function ("memcpy", t, BUILT_IN_MEMCPY,
				  BUILT_IN_NORMAL, NULL, NULL_TREE);
  memmove_proc = builtin_function ("memmove", t, BUILT_IN_MEMMOVE,
				   BUILT_IN_NORMAL, NULL, NULL_TREE);

  t = build_function_type_list (t_addr, t_addr, t_int, t_int, NULL_TREE);
  memset_proc = builtin_function ("memset", t, BUILT_IN_MEMSET,
				  BUILT_IN_NORMAL, NULL, NULL_TREE);

  t = build_function_type_list (t_int, t_int, t_int, NULL_TREE);
  div_proc = builtin_function ("m3_div", t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  mod_proc = builtin_function ("m3_mod", t, 0, NOT_BUILT_IN, NULL, NULL_TREE);

  t = build_function_type (t_void, NULL_TREE);
  set_union_proc  = builtin_function ("set_union",
				      t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_diff_proc   = builtin_function ("set_difference",
				      t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_inter_proc  = builtin_function ("set_intersection",
				      t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_sdiff_proc  = builtin_function ("set_sym_difference",
				      t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_sing_proc   = builtin_function ("set_singleton",
				      t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_range_proc  = builtin_function ("set_range",
				      t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  
  t = build_function_type (t_int, NULL_TREE);
  set_member_proc = builtin_function
    ("set_member", t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_eq_proc = builtin_function ("set_eq",
				  t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_ne_proc = builtin_function ("set_ne",
				  t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_gt_proc = builtin_function ("set_gt",
				  t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_ge_proc = builtin_function ("set_ge",
				  t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_lt_proc = builtin_function ("set_lt",
				  t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
  set_le_proc = builtin_function ("set_le",
				  t, 0, NOT_BUILT_IN, NULL, NULL_TREE);
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
#ifdef LONG_DOUBLE_TYPE_SIZE
#undef LONG_DOUBLE_TYPE_SIZE
#endif
#define LONG_DOUBLE_TYPE_SIZE 64
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

static GTY (()) varray_type m3_global_varrays[M3VA_MAX];

#define all_vars	m3_global_varrays[M3VA_VARS]
#define all_procs	m3_global_varrays[M3VA_PROCS]
#define all_labels	m3_global_varrays[M3VA_LABELS]
#define expr_stack	m3_global_varrays[M3VA_EXPR_STACK]
#define call_stack	m3_global_varrays[M3VA_CALL_STACK]

#define STACK_PUSH(stk, x)	VARRAY_PUSH_TREE (stk, x)
#define STACK_POP(stk)		VARRAY_POP (stk)
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

/*======================================================= OPTION HANDLING ===*/

static int option_opcodes_trace     = 0;
static int option_source_line_trace = 0;
static int option_vars_trace        = 0;
static int option_procs_trace       = 0;
static int option_exprs_trace       = 0;
static int option_misc_trace        = 0;
static int option_types_trace       = 0;

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
    real_from_target_fmt (&val, (long *)(&data.z[0]), &ieee_single_format);
  } else {
    real_from_target_fmt (&val, (long *)(&data.z[0]), &ieee_double_format);
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
fix_name (const char *name, long id)
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
declare_temp (tree t)
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


/* Return a tree representing the address of the given procedure.  The static
   address is used rather than the trampoline address for a nested
   procedure.  */
 
static tree
proc_addr (tree proc)
{
  tree expr = m3_build1 (ADDR_EXPR,
                         build_pointer_type (TREE_TYPE (proc)),
                         proc);
  TREE_STATIC (expr) = 1;
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

static void
m3_call_direct (tree p, tree t)
{
  tree formal, actual;

  /* need to force type of actual struct to type of formal to preserve types
     when inlining */
  for (formal = DECL_ARGUMENTS (p), actual = CALL_TOP_ARG ();
       formal && actual;	/* why actual? because fall through finally
				   handlers can be invoked without an
				   actual -- this seems broken in
				   TryFinStmt.m3  FIXME */
       formal = TREE_CHAIN (formal), actual = TREE_CHAIN (actual))
    {
      tree ft = TREE_TYPE (formal);
      tree at = TREE_TYPE (TREE_VALUE (actual));

      if (TREE_CODE (ft) == RECORD_TYPE) {
	if (TYPE_SIZE (ft) != TYPE_SIZE (at)) abort ();
	if (TYPE_SIZE_UNIT (ft) != TYPE_SIZE_UNIT (at)) abort ();
	if (TYPE_ALIGN (ft) != TYPE_ALIGN (at)) abort ();
	TREE_TYPE (TREE_VALUE (actual)) = TREE_TYPE (formal);
      }
    }

  tree fnaddr = proc_addr (p);
  tree call = build (CALL_EXPR, t,
		     fnaddr, CALL_TOP_ARG (), CALL_TOP_STATIC_CHAIN ());
  if (t == t_void) {
    TREE_SIDE_EFFECTS (call) = 1;
    expand_expr_stmt (call);
  } else {
    EXPR_PUSH (call);
  }
  CALL_POP ();
}

static void
m3_call_indirect (tree t)
{
  tree argtypes = chainon (CALL_TOP_TYPE (),
			   tree_cons (NULL_TREE, t_void, NULL_TREE));
  tree fntype = build_pointer_type (build_function_type (t, argtypes));
  tree fnaddr = m3_cast (fntype, EXPR_REF (-1));
  EXPR_POP ();

  tree call = build (CALL_EXPR, t,
		     fnaddr, CALL_TOP_ARG (), CALL_TOP_STATIC_CHAIN ());
  if (t == t_void) {
    TREE_SIDE_EFFECTS (call) = 1;
    expand_expr_stmt (call);
  } else {
    EXPR_PUSH (call);
  }
  CALL_POP ();
}

static void
m3_swap (void)
{
  tree tmp = EXPR_REF (-1);
  EXPR_REF (-1) = EXPR_REF (-2);
  EXPR_REF (-2) = tmp;
}

static void
m3_load (tree v, int o,
	 tree src_t, m3_type src_T,
	 tree dst_t, m3_type dst_T)
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
m3_store (tree v, int o, tree src_t, tree dst_t) 
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
  expand_assignment (lhs, rhs, 0);
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
condop (enum tree_code o, tree l, tree t)
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
  m3_start_call ();
  while (q--) {
    m3_pop_param (t_addr);
  }
  m3_call_direct (p, TREE_TYPE (TREE_TYPE (p)));
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
		     build_function_type_list (t_void, t_word, NULL_TREE));
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
  location_t save_loc = input_location;

  input_location.file = "<internal>";
  input_location.line = 0;
  DECL_SOURCE_LOCATION (fault_proc) = input_location;

  if (current_block) push_function_context ();

  current_function_decl = fault_proc;

  make_decl_rtl (fault_proc, NULL);

  init_function_start (fault_proc);
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
  expand_function_end ();
  rest_of_compilation (fault_proc);

  current_block = TREE_VALUE (pending_blocks);
  pending_blocks = TREE_CHAIN (pending_blocks);

  if (current_block) pop_function_context ();

  input_location = save_loc;
}

// FIXME: jdp says 0x0f and 4; cm3 may need more
#define FAULT_MASK 0x1f
#define LINE_SHIFT 5

static void
generate_fault (int code)
{
  if (fault_proc == 0) declare_fault_proc ();
  m3_start_call ();
  EXPR_PUSH (m3_build_int ((input_location.line << LINE_SHIFT) + (code & FAULT_MASK)));
  m3_pop_param (t_word);
  m3_call_direct (fault_proc, t_void);
  emit_barrier ();
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
m3cg_import_unit (void)
{
  UNUSED_NAME (n);
  /* ignore */
}

static void
m3cg_export_unit (void)
{
  NAME (n);
  /* remember the set of exported interfaces */
  exported_interfaces_names [exported_interfaces++] = n;
}

static void
m3cg_set_source_file (void)
{
  NAME (s);

  input_location.file = s;
  if (cfun != NULL) emit_line_note (input_location);
}

static void
m3cg_set_source_line (void)
{
  INTEGER (i);

  if (option_source_line_trace) fprintf(stderr, "  source line %4ld\n", i);
  input_location.line = i;
  if (cfun != NULL) emit_line_note (input_location);
}

static void
m3cg_declare_typename (void)
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
m3cg_declare_array (void)
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
m3cg_declare_open_array (void)
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
m3cg_declare_enum (void)
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
m3cg_declare_enum_elt (void)
{
  NAME (n);

  if (option_types_trace)
    fprintf(stderr,
            "  enum elem %s\n", n);
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
            "  packed id %ld, target id %ld, size %ld\n",
            my_id, target_id, size);
  debug_field_id (target_id);
  debug_tag ('D', my_id, "_%d", size);
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
            "  record id %ld, fields %ld, size %ld\n",
            my_id, n_fields, size);
  debug_tag ('R', my_id, "_%d", size);
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
    fprintf(stderr, "  field %s, id %ld, size %ld, offset %ld\n",
            name, my_id, size, offset);

  debug_field_fmt (my_id, "_%d_%d_%s", offset, size, name);
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

  debug_tag ('S', my_id, "_%d", size);
  debug_field_id (domain_id);
  debug_struct ();
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
  a1 = TREE_INT_CST_HIGH(min);  a0 = TREE_INT_CST_LOW(min);
  b1 = TREE_INT_CST_HIGH(max);  b0 = TREE_INT_CST_LOW(max);

  if (option_types_trace)
    fprintf(stderr, 
	    "  subrange id %ld, a0 %lld, a1 %lld, b0 %lld, b1 %lld, size %ld\n",
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
            "  pointer id %ld, target id %ld, brand %s, traced %d\n",
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
    fprintf(stderr, "  indirect id %ld, target_id %ld\n", my_id, target_id);
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
m3cg_declare_formal (void)
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
m3cg_declare_method (void)
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
    fprintf(stderr, "  typeid %ld = typeid %ld\n", lhs, rhs);

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

  if (STREQ (s, "ReportFault")) { fault_handler = p; }
}

static void
m3cg_set_runtime_hook (void)
{
  NAME       (s);
  VAR        (v);
  BYTEOFFSET (o);

  if (STREQ (s, "ReportFault")) { fault_intf = v; fault_offs = o; }
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
m3cg_declare_segment (void)
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
m3cg_bind_segment (void)
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
  
  rest_of_decl_compilation (v, 0, 1, 0);
  TREE_CHAIN (v) = global_vars;
  global_vars = v;
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

  if (current_param_count == 0) return;	/* ignore */

  tree p = current_function_decl;

  DECL_NAME (v) = fix_name (n, id);
  if (option_procs_trace)
    fprintf(stderr, "  param %s type %d typeid %ld\n",
	    IDENTIFIER_POINTER(DECL_NAME(v)), t, id);
  TREE_TYPE (v) = m3_build_type (t, s, a);
  DECL_NONLOCAL (v) = up_level || in_memory;
  TREE_ADDRESSABLE (v) = in_memory;
  DECL_ARG_TYPE (v) = TREE_TYPE (v);
  DECL_CONTEXT (v) = p;
  layout_decl (v, a);
  if (DECL_MODE (v) == VOIDmode) DECL_MODE (v) = Pmode;

  TREE_CHAIN (v) = DECL_ARGUMENTS (p);
  DECL_ARGUMENTS (p) = v;

  rest_of_decl_compilation (v, 0, 0, 1);

  if (--current_param_count == 0) {
    /* arguments were accumulated in reverse, build type, then unreverse */
    tree parm;
    tree atypes = tree_cons (NULL_TREE, t_void, NULL_TREE);
    for (parm = DECL_ARGUMENTS (p); parm; parm = TREE_CHAIN (parm)) {
      atypes = tree_cons (NULL_TREE, TREE_TYPE (parm), atypes);
    }
    TREE_TYPE (p) = build_function_type (TREE_TYPE (DECL_RESULT (p)), atypes);
    DECL_ARGUMENTS (p) = nreverse (DECL_ARGUMENTS (p));
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
m3cg_init_int (void)
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
  TREE_VALUE (v) = build (RTL_EXPR, t_addr, NULL_TREE, label_rtx (l));
}

static void
m3cg_init_var (void)
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
m3cg_init_offset (void)
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
      j = XWINT (r, 0);  /* offset */
    }
  }

  /* TREE_VALUE (V) = m3_build_int (j); ? */
  TREE_VALUE (V) = size_int (j);
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
m3cg_import_procedure (void)
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
m3cg_declare_procedure (void)
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
m3cg_begin_procedure (void)
{
  PROC (p);
  tree local;

  if (option_procs_trace)
    fprintf(stderr, "  procedure %s\n", IDENTIFIER_POINTER(DECL_NAME(p)));

  DECL_SOURCE_LOCATION (p) = input_location;

  announce_function (p);
  make_decl_rtl (p, NULL);

  if (current_block) push_function_context ();

  current_function_decl = p;

  init_function_start (p);
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
m3cg_end_procedure (void)
{
  PROC (p);

  if (option_procs_trace)
    fprintf(stderr, "  procedure %s\n", IDENTIFIER_POINTER(DECL_NAME(p)));

  if (current_block != BLOCK_SUBBLOCKS (DECL_INITIAL (p))) abort ();
  expand_end_bindings (BLOCK_VARS (current_block), 1, 0);
  expand_function_end ();
  rest_of_compilation (current_function_decl);

  current_block = TREE_VALUE (pending_blocks);
  pending_blocks = TREE_CHAIN (pending_blocks);

  if (current_block) pop_function_context ();
}

static void
m3cg_begin_block (void)
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
m3cg_end_block (void)
{
  expand_end_bindings (BLOCK_VARS (current_block), 1, 0);
  current_block = TREE_VALUE (pending_blocks);
  pending_blocks = TREE_CHAIN (pending_blocks);
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
m3cg_m3_jump (void)
{
  LABEL (l);

  expand_goto (l);
}

static void
m3cg_if_true (void)
{
  UNUSED_TYPE      (t);
  LABEL     (l);
  UNUSED_FREQUENCY (f);

  tree cond = EXPR_REF (-1);
  EXPR_POP ();
  do_jump (cond, NULL_RTX, label_rtx (l));
}

static void
m3cg_if_false (void)
{
  UNUSED_TYPE      (t);
  LABEL     (l);
  UNUSED_FREQUENCY (f);

  tree cond = EXPR_REF (-1);
  EXPR_POP ();
  do_jump (cond, label_rtx (l), NULL_RTX);
}

static void
m3cg_if_compare (enum tree_code condition) 
{
  MTYPE     (t);
  LABEL     (l);
  UNUSED_FREQUENCY (f);

  condop (condition, l, t);
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
m3cg_exit_proc (void)
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
m3cg_load (void)
{
  VAR        (v);
  BYTEOFFSET (o);
  MTYPE2     (src_t, src_T);
  MTYPE2     (dst_t, dst_T);

  m3_load (v, o, src_t, src_T, dst_t, dst_T);
}

static void
m3cg_load_address (void)
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
m3cg_load_indirect (void)
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
    fprintf(stderr, "  store (%s) offset %ld src_t %d dst_t %d\n", 
            name, o, src_T, dst_T);
  }
  m3_store (v, o, src_t, dst_t);
}

static void
m3cg_store_indirect (void)
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
  expand_assignment (ptr, m3_build1 (CONVERT_EXPR, dst_t, val), 0);
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

  if (TREE_TYPE (n) != t) { n = m3_build1 (CONVERT_EXPR, t, n); }
  EXPR_PUSH(n);
}

static void
m3cg_load_float (void)
{
  UNUSED_MTYPE (t);
  FLOAT (f, fkind);

  EXPR_PUSH (f);
}

static void
m3cg_compare (enum tree_code op)
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

  binaryop (PLUS_EXPR, t);
}

static void
m3cg_subtract (void)
{
  MTYPE (t);

  binaryop (MINUS_EXPR, t);
}

static void
m3cg_multiply (void)
{
  MTYPE (t);

  binaryop (MULT_EXPR, t);
}

static void
m3cg_divide (void)
{
  MTYPE (t);

  binaryop (RDIV_EXPR, t);
}

static void
m3cg_negate (void)
{
  MTYPE (t);

  unaryop (NEGATE_EXPR, t);
}

static void
m3cg_abs (void)
{
  MTYPE (t);

  unaryop (ABS_EXPR, t);
}

static void
m3cg_max (void)
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
m3cg_min (void)
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
m3cg_round (void)
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
m3cg_trunc (void)
{
  UNUSED_MTYPE (src_t);
  MTYPE (dst_t);

  unaryop (FIX_TRUNC_EXPR, dst_t);
}

static void
m3cg_floor (void)
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
m3cg_ceiling (void)
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
m3cg_cvt_float (void)
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
m3cg_div (void)
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
    m3_call_direct (div_proc, TREE_TYPE (TREE_TYPE (div_proc)));
  }
}

static void
m3cg_mod (void)
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
    m3_call_direct (mod_proc, TREE_TYPE (TREE_TYPE (mod_proc)));
  }
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

static void
m3cg_set_member (void)
{
  UNUSED_BYTESIZE (n);
  MTYPE    (t);

  if (t != t_int) { fatal_error ("set_member on non-native integer size"); }
  setop2 (set_member_proc, 2);
}

static void
m3cg_set_compare (tree proc)
{
  BYTESIZE (n);
  MTYPE    (t);

  if (t != t_int) { fatal_error ("set_compare on non-native integer size"); }
  setop (proc, n, 2);
}

static void m3cg_set_eq (void) { m3cg_set_compare (set_eq_proc); }
static void m3cg_set_ne (void) { m3cg_set_compare (set_ne_proc); }
static void m3cg_set_gt (void) { m3cg_set_compare (set_gt_proc); }
static void m3cg_set_ge (void) { m3cg_set_compare (set_ge_proc); }
static void m3cg_set_lt (void) { m3cg_set_compare (set_lt_proc); }
static void m3cg_set_le (void) { m3cg_set_compare (set_le_proc); }

static void
m3cg_set_range (void)
{
  UNUSED_BYTESIZE (n);
  MTYPE    (t);

  if (t != t_int) { fatal_error ("set_range on non-native integer size"); }
  setop2 (set_range_proc, 3);
}

static void
m3cg_set_singleton (void)
{
  UNUSED_BYTESIZE (n);
  MTYPE    (t);

  if (t != t_int) { fatal_error ("set_singleton on non-native integer size"); }
  setop2 (set_sing_proc, 2);
}

static void
m3cg_not (void)
{
  MTYPE (t);

  unaryop (BIT_NOT_EXPR, m3_unsigned_type (t));
}

static void
m3cg_and (void)
{
  MTYPE (t);

  binaryop (BIT_AND_EXPR, m3_unsigned_type (t));
}

static void
m3cg_or (void)
{
  MTYPE (t);

  binaryop (BIT_IOR_EXPR, m3_unsigned_type (t));
}

static void
m3cg_xor (void)
{
  MTYPE (t);

  binaryop (BIT_XOR_EXPR, m3_unsigned_type (t));
}

static void
m3cg_m3_shift (void)
{
  MTYPE (t);

  tree temp1 = declare_temp (t);
  tree temp2 = declare_temp (t);
  tree t1    = m3_build2 (MODIFY_EXPR, t, temp1, EXPR_REF(-1));
  tree t2    = m3_build2 (MODIFY_EXPR, t, temp2, EXPR_REF(-2));
  tree res;


  TREE_SIDE_EFFECTS (t1) = 1;
  TREE_SIDE_EFFECTS (t2) = 1;
  res  = m3_build3 (COND_EXPR, m3_unsigned_type (t),
		    m3_build2 (GE_EXPR, t, temp1, v_zero),
		    m3_do_shift (temp2, temp1, 0, t),
		    m3_do_shift (temp2, m3_build1 (NEGATE_EXPR, t, temp1),
                                 1, t));
  EXPR_REF(-2) = m3_build2 (COMPOUND_EXPR,
			    m3_unsigned_type (t),
			    m3_build2 (COMPOUND_EXPR, t, t1, t2), res);
  EXPR_POP();
}

static void
m3cg_shift_left (void)
{
  MTYPE (t);

  EXPR_REF (-2) = m3_do_shift (EXPR_REF (-2), EXPR_REF (-1), 0, t);
  EXPR_POP ();
}

static void
m3cg_shift_right (void)
{
  MTYPE (t);

  EXPR_REF (-2) = m3_do_shift (EXPR_REF (-2), EXPR_REF (-1), 1, t);
  EXPR_POP ();
}

static void
m3cg_m3_rotate (void)
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
m3cg_rotate_left (void)
{
  MTYPE (t);

  EXPR_REF (-2) = m3_do_rotate (EXPR_REF (-2), EXPR_REF (-1), 0, t);
  EXPR_POP ();
}

static void
m3cg_rotate_right (void)
{
  MTYPE (t);

  EXPR_REF (-2) = m3_do_rotate (EXPR_REF (-2), EXPR_REF (-1), 1, t);
  EXPR_POP ();
}

static void
m3cg_widen (void)
{
  BOOLEAN (sign);

  tree dst_t = (sign ? t_int_64 : t_word_64);
  tree src_t  = (sign ? t_int_32 : t_word_32);

  EXPR_REF(-1) = m3_build1 (CONVERT_EXPR, dst_t, 
                            m3_cast (src_t, EXPR_REF(-1)));
}

static void
m3cg_chop (void)
{
  EXPR_REF(-1) = m3_build1 (CONVERT_EXPR, t_int_32,
			    m3_build2 (BIT_AND_EXPR, t_int_64, EXPR_REF(-1),
				       m3_build_int (0xffffffff)));
}

static void
m3cg_m3_extract (void)
{
  MTYPE   (t);
  BOOLEAN (sign_extend);

  EXPR_REF (-3) = m3_do_extract (EXPR_REF (-3), EXPR_REF (-2), EXPR_REF (-1),
                                 t, sign_extend);
  EXPR_POP ();
  EXPR_POP ();
}

static void
m3cg_extract_n (void)
{
  MTYPE   (t);
  BOOLEAN (sign_extend);
  INTEGER (n);

  EXPR_REF (-2) = m3_do_extract (EXPR_REF (-2), EXPR_REF (-1),
                                 m3_build_int (n), t, sign_extend);
  EXPR_POP ();
}

static void
m3cg_extract_mn (void)
{
  MTYPE   (t);
  BOOLEAN (sign_extend);
  INTEGER (m);
  INTEGER (n);

  EXPR_REF (-1) = m3_do_fixed_extract (EXPR_REF (-1), m, n, t, sign_extend);
}

static void
m3cg_m3_insert (void)
{
  MTYPE (t);

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

  EXPR_REF (-3) = m3_do_insert (EXPR_REF (-3), EXPR_REF (-2),
                                EXPR_REF (-1), m3_build_int (n), t);
  EXPR_POP ();
  EXPR_POP ();
}

static void
m3cg_insert_mn (void)
{
  MTYPE   (t);
  INTEGER (m);
  INTEGER (n);

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

  tree expr = EXPR_REF (-1);
  EXPR_POP ();
  TREE_SIDE_EFFECTS (expr) = 1;
  expand_expr_stmt (expr);
}

static void
m3cg_copy_n (void)
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
m3cg_copy (void)
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
                     0);
  EXPR_POP ();
  EXPR_POP ();
}

static void
m3cg_zero_n (void)
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
m3cg_zero (void)
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
m3cg_loophole (void)
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
m3cg_abort (void)
{
  INTEGER (code);

  generate_fault (code);
}

static void
m3cg_check_nil (void)
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
m3cg_check_lo (void)
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
m3cg_check_hi (void)
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
m3cg_check_range (void)
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
m3cg_check_index (void)
{
  MTYPE   (t);
  INTEGER (code);

  t = m3_unsigned_type (t);
  expand_start_cond (m3_build2 (GE_EXPR, t,
				m3_build1 (CONVERT_EXPR, t, EXPR_REF(-2)),
				m3_build1 (CONVERT_EXPR, t, EXPR_REF(-1))),
		     0);
  generate_fault (code);
  expand_end_cond ();
  EXPR_POP();
}

static void
m3cg_check_eq (void)
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
m3cg_add_offset (void)
{
  BYTESIZE (n);

  if (option_vars_trace) {
    fprintf(stderr, "  add offset %ld\n", n);
  }
  EXPR_REF (-1) = m3_build2 (PLUS_EXPR, t_addr,
                             EXPR_REF (-1), size_int (n / BITS_PER_UNIT));
}

static void
m3cg_index_address (void)
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
m3cg_start_call_direct (void)
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
m3cg_call_direct (void)
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
m3cg_start_call_indirect (void)
{
  UNUSED_MTYPE2 (t, m3t);
  UNUSED_INTEGER (call_conv);

  if (option_procs_trace)
    fprintf(stderr, "  start call procedure indirect, type %d\n", m3t);
  m3_start_call ();
}

static void
m3cg_call_indirect (void)
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
m3cg_pop_param (void)
{
  MTYPE2 (t, m3t);

  if (option_vars_trace)
    fprintf(stderr, "  pop param type %d\n", m3t);
  m3_pop_param (t);
}

static void
m3cg_pop_struct (void)
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
m3cg_pop_static_link (void)
{
  tree v = declare_temp (t_addr);
  m3_store (v, 0, TREE_TYPE (v), t_addr);
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
  /* This is ignored now that we use gcc's built-in nested function
     support. */
  PROC (p);
  if (option_procs_trace)
    fprintf(stderr, "  load link %s\n",
            IDENTIFIER_POINTER(DECL_NAME(p)));
  EXPR_PUSH (build (RTL_EXPR, t_addr, NULL_TREE, lookup_static_chain (p)));
}

static void
m3cg_comment (void)
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

static void
m3_parse_file (int xx ATTRIBUTE_UNUSED)
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
    fatal_error (" *** bad M3CG version stamp (%d), expected %x",
                 i, M3CG_Version);
  }

  op = (int)LAST_OPCODE;
  while (op != (int)M3CG_END_UNIT) {
    op = get_int ();
    if (op < 0 || (int)LAST_OPCODE <= op) {
      fatal_error (" *** bad opcode: %d, at m3cg_lineno %d", op, m3cg_lineno);
    }
    if (option_opcodes_trace) { 
      fprintf (stderr, "(%d) %s\n", m3cg_lineno, M3CG_opnames[op]);
    }
    m3cg_lineno ++;
    ops[op].proc ();
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

static int version_done = 0;
const char *const language_string = "M3CG - Modula-3 Compiler back end";

/* Process a switch - called by opts.c.  */
static int
m3_handle_option (size_t scode, const char *arg ATTRIBUTE_UNUSED, int value)
{
  enum opt_code code = (enum opt_code) scode;

  switch (code)
    {
    default:
      abort();

    case OPT_v:
      if (!version_done)
	{
	  fputs (language_string, stdout);
	  fputs (version_string, stdout);
	  fputs ("\n", stdout);
	  version_done = 1;
	}
      break;

    case OPT_y:
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

/* Language dependent parser setup.  */

static bool
m3_init (void)
{
  const char *filename = main_input_filename;

  /* Open input file.  */
  if (filename == 0 || !strcmp (filename, "-"))
    {
      finput = stdin;
      filename = "stdin";
    }
  else
    finput = fopen (filename, "r");
  if (finput == 0)
    {
      fprintf (stderr, "Unable to open input file %s\n", filename);
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

/* New garbage collection regime see gty.texi.  */
#include "debug.h"
#include "gtype-m3cg.h"
#include "gt-m3cg-parse.h"
