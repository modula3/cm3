#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "toplev.h"
#include "flags.h"
#include "m3-tree.h"
#include "function.h"
#include "expr.h"
#include "libfuncs.h"
#include "except.h"
#include "convert.h"
#include "ggc.h"
#include "c-common.h"	/* XXX for build_stmt declaration */

tree m3_global_trees[M3TI_MAX];

static tree left_shift PARAMS ((tree, int));

/*========================================== insert, shift, rotate and co ===*/

tree
m3_do_insert (x, y, i, n, t)
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

static tree
left_shift (t, i)
     tree t;
     int i;
{
  if (i)
    t = m3_build2 (LSHIFT_EXPR, t_word, t, m3_build_int (i));
  return t;
}

tree
m3_do_fixed_insert (x, y, i, n, t)
     tree x, y, t;
     int i, n;
{
  /* ??? Use store_bit_field.  */
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

tree
m3_do_extract (x, i, n, t, sign_extend)
     tree x, i, n, t;
     int sign_extend;
{
  tree a, b, c, d, e, f;

  a = m3_build2 (MINUS_EXPR, t, m3_build_int (BITS_PER_WORD), n);
  b = m3_build2 (MINUS_EXPR, t, a, i);
  c = m3_build1 (CONVERT_EXPR, unsigned_type(t), x);
  d = m3_build2 (LSHIFT_EXPR, unsigned_type(t), c, b);
  e = m3_build2 (RSHIFT_EXPR, 
                 (sign_extend ? signed_type(t) : unsigned_type(t)), d, a);
  f = m3_build3 (COND_EXPR, t,
		 m3_build2 (EQ_EXPR, t, n, v_zero),
		 v_zero, e);
  return f;
}

tree
m3_do_fixed_extract (x, i, n, t, sign_extend)
     tree x, t;
     int i, n, sign_extend;
{
  /* ??? Use extract_bit_field.  */
  int a = BITS_PER_WORD - n;
  int b = BITS_PER_WORD - n - i;
  tree c, d, e;

  if ((a < 0) || (BITS_PER_WORD <= a) || (b < 0) || (BITS_PER_WORD <= b))
    {
      return m3_do_extract (x, m3_build_int (i),
			    m3_build_int (n), t, sign_extend);
    }

  c = m3_build1 (CONVERT_EXPR, unsigned_type(t), x);
  d = (b == 0) ? c : m3_build2 (LSHIFT_EXPR, unsigned_type(t), c, 
                                m3_build_int (b));
  e = (a == 0) ? d : 
    m3_build2 (RSHIFT_EXPR, (sign_extend ? signed_type(t) : unsigned_type(t)),
               d, m3_build_int (a));
  return e;
}

tree
m3_do_rotate (val, cnt, right, t)
     tree val, cnt, t;
     int right;
{
  /* ??? Use LROTATE_EXPR/RROTATE_EXPR.  */
  tree a, b, c, d, e, f, g;

  t = unsigned_type (t);
  a = m3_build_int (BITS_PER_WORD - 1);
  b = m3_build2 (BIT_AND_EXPR, t, cnt, a);
  c = m3_build2 (MINUS_EXPR, t, m3_build_int (BITS_PER_WORD), b);
  d = m3_build1 (CONVERT_EXPR, t, val);
  e = m3_build2 (LSHIFT_EXPR, t, d, (right) ? c : b);
  f = m3_build2 (RSHIFT_EXPR, t, d, (right) ? b : c);
  g = m3_build2 (BIT_IOR_EXPR, t, e, f);
  return g;
}

tree
m3_do_shift (val, cnt, right, t)
     tree val, cnt, t;
     int right;
{
  tree a, b, c, d;
  HOST_WIDE_INT cnt_val;

  t = unsigned_type (t);
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

tree
m3_cast (tipe, op0)
     tree tipe, op0;
{
  return fold (build1 (NOP_EXPR, tipe, op0));
}

tree
m3_build1 (code, tipe, op0)
     enum tree_code code;
     tree tipe, op0;
{
  return fold (build1 (code, tipe, op0));
}

tree
m3_build2 (code, tipe, op0, op1)
     enum tree_code code;
     tree tipe, op0, op1;
{
  return fold (build (code, tipe, op0, op1));
}

tree
m3_build3 (code, tipe, op0, op1, op2)
     enum tree_code code;
     tree tipe, op0, op1, op2;
{
  return fold (build (code, tipe, op0, op1, op2));
}

tree
m3_build_real (value, tipe)
     const char *value;
     tree tipe;
{
  tree x = make_node (REAL_CST);
  TREE_TYPE (x) = tipe;
  /* TREE_REAL_CST (x) = REAL_VALUE_ATOF (value, TYPE_MODE (tipe)); */
  if (tipe == t_reel) {
    TREE_REAL_CST (x) = REAL_VALUE_ATOF (value, SFmode); }
  else if (tipe == t_lreel) {
    TREE_REAL_CST (x) = REAL_VALUE_ATOF (value, DFmode); }
  else if (tipe == t_xreel) {
    TREE_REAL_CST (x) = REAL_VALUE_ATOF (value, XFmode); }
  else {
    abort (); 
  }
  return x;
}

tree
m3_build_int (n)
     int n;
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


int
m3_is_small_cardinal (t, n)
     tree t;
     HOST_WIDE_INT *n;
{
  if (TREE_CODE (t) != INTEGER_CST || TREE_INT_CST_HIGH (t) != 0)
    return 0;
  *n = TREE_INT_CST_LOW (t);
  return 1;
}

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */

tree
convert (type, expr)
     tree type, expr;
{
  tree e = expr;
  enum tree_code code = TREE_CODE (type);

  if (type == TREE_TYPE (expr)
      || TREE_CODE (expr) == ERROR_MARK
      || code == ERROR_MARK || TREE_CODE (TREE_TYPE (expr)) == ERROR_MARK)
    return expr;

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (expr)))
    return fold (build1 (NOP_EXPR, type, expr));
  if (TREE_CODE (TREE_TYPE (expr)) == ERROR_MARK)
    return error_mark_node;
  if (TREE_CODE (TREE_TYPE (expr)) == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  if (code == VOID_TYPE)
    return build1 (CONVERT_EXPR, type, e);
  if (code == INTEGER_TYPE || code == ENUMERAL_TYPE)
    return fold (convert_to_integer (type, e));
  if (code == BOOLEAN_TYPE)
    {
      tree t = truthvalue_conversion (expr);
      /* If truthvalue_conversion returns a NOP_EXPR, we must fold it here
	 to avoid infinite recursion between fold () and convert ().  */
      if (TREE_CODE (t) == NOP_EXPR)
	return fold (build1 (NOP_EXPR, type, TREE_OPERAND (t, 0)));
      else
	return fold (build1 (NOP_EXPR, type, t));
    }
  if (code == POINTER_TYPE || code == REFERENCE_TYPE)
    return fold (convert_to_pointer (type, e));
  if (code == REAL_TYPE)
    return fold (convert_to_real (type, e));
  if (code == COMPLEX_TYPE)
    return fold (convert_to_complex (type, e));
  if (code == VECTOR_TYPE)
    return fold (convert_to_vector (type, e));

  error ("conversion to non-scalar type requested");
  return error_mark_node;
}

/* Since we have no language-specific tree nodes, our lang_mark_tree function
   doesn't need to do anything. */
void
lang_mark_tree (t)
     tree t ATTRIBUTE_UNUSED;
{
}

tree
build_stmt VPARAMS ((enum tree_code code ATTRIBUTE_UNUSED, ...))
{
  fatal_error ("C-specific function build_stmt called");
}

tree
builtin_function (name, type, function_code, class, library_name)
     const char *name ATTRIBUTE_UNUSED;
     tree type ATTRIBUTE_UNUSED;
     int function_code ATTRIBUTE_UNUSED;
     enum built_in_class class ATTRIBUTE_UNUSED;
     const char *library_name ATTRIBUTE_UNUSED;
{
  fatal_error ("C-specific function builtin_function called");
}

tree
decl_constant_value (decl)
     tree decl ATTRIBUTE_UNUSED;
{
  fatal_error ("C-specific function decl_constant_value called");
}

int
statement_code_p (code)
     enum tree_code code ATTRIBUTE_UNUSED;
{
  fatal_error ("C-specific function statement_code_p called");
}
