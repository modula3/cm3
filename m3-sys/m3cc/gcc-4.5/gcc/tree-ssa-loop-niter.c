/* Modula-3: modified */

/* Functions to determine/estimate number of iterations of a loop.
   Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation,
   Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "diagnostic.h"
#include "intl.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "cfgloop.h"
#include "tree-pass.h"
#include "ggc.h"
#include "tree-chrec.h"
#include "tree-scalar-evolution.h"
#include "tree-data-ref.h"
#include "params.h"
#include "flags.h"
#include "toplev.h"
#include "tree-inline.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Substitute NEW for OLD in EXPR and fold the result.  */

static tree
simplify_replace_tree (tree expr, tree old, tree new_tree)
{
  unsigned i, n;
  tree ret = NULL_TREE, e, se;

  if (!expr)
    return NULL_TREE;

  if (expr == old
      || operand_equal_p (expr, old, 0))
    return unshare_expr (new_tree);

  if (!EXPR_P (expr))
    return expr;

  n = TREE_OPERAND_LENGTH (expr);
  for (i = 0; i < n; i++)
    {
      e = TREE_OPERAND (expr, i);
      se = simplify_replace_tree (e, old, new_tree);
      if (e == se)
	continue;

      if (!ret)
	ret = copy_node (expr);

      TREE_OPERAND (ret, i) = se;
    }

  return (ret ? fold (ret) : expr);
}

/* Expand definitions of ssa names in EXPR as long as they are simple
   enough, and return the new expression.  */

tree
expand_simple_operations (tree expr)
{
  unsigned i, n;
  tree ret = NULL_TREE, e, ee, e1;
  enum tree_code code;
  gimple stmt;

  if (expr == NULL_TREE)
    return expr;

  if (is_gimple_min_invariant (expr))
    return expr;

  code = TREE_CODE (expr);
  if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code)))
    {
      n = TREE_OPERAND_LENGTH (expr);
      for (i = 0; i < n; i++)
	{
	  e = TREE_OPERAND (expr, i);
	  ee = expand_simple_operations (e);
	  if (e == ee)
	    continue;

	  if (!ret)
	    ret = copy_node (expr);

	  TREE_OPERAND (ret, i) = ee;
	}

      if (!ret)
	return expr;

      fold_defer_overflow_warnings ();
      ret = fold (ret);
      fold_undefer_and_ignore_overflow_warnings ();
      return ret;
    }

  if (TREE_CODE (expr) != SSA_NAME)
    return expr;

  stmt = SSA_NAME_DEF_STMT (expr);
  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      basic_block src, dest;

      if (gimple_phi_num_args (stmt) != 1)
	return expr;
      e = PHI_ARG_DEF (stmt, 0);

      /* Avoid propagating through loop exit phi nodes, which
	 could break loop-closed SSA form restrictions.  */
      dest = gimple_bb (stmt);
      src = single_pred (dest);
      if (TREE_CODE (e) == SSA_NAME
	  && src->loop_father != dest->loop_father)
	return expr;

      return expand_simple_operations (e);
    }
  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return expr;

  e = gimple_assign_rhs1 (stmt);
  code = gimple_assign_rhs_code (stmt);
  if (get_gimple_rhs_class (code) == GIMPLE_SINGLE_RHS)
    {
      if (is_gimple_min_invariant (e))
	return e;

      if (code == SSA_NAME)
	return expand_simple_operations (e);

      return expr;
    }

  switch (code)
    {
    CASE_CONVERT:
      /* Casts are simple.  */
      ee = expand_simple_operations (e);
      return fold_build1 (code, TREE_TYPE (expr), ee);

    case PLUS_EXPR:
    case MINUS_EXPR:
    case POINTER_PLUS_EXPR:
      /* And increments and decrements by a constant are simple.  */
      e1 = gimple_assign_rhs2 (stmt);
      if (!is_gimple_min_invariant (e1))
	return expr;

      ee = expand_simple_operations (e);
      return fold_build2 (code, TREE_TYPE (expr), ee, e1);

    default:
      return expr;
    }
}

/* Returns true if EXIT is the only possible exit from LOOP.  */

bool
loop_only_exit_p (const struct loop *loop, const_edge exit)
{
  basic_block *body;
  gimple_stmt_iterator bsi;
  unsigned i;
  gimple call;

  if (exit != single_exit (loop))
    return false;

  body = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
    {
      for (bsi = gsi_start_bb (body[i]); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  call = gsi_stmt (bsi);
	  if (gimple_code (call) != GIMPLE_CALL)
	    continue;

	  if (gimple_has_side_effects (call))
	    {
	      free (body);
	      return false;
	    }
	}
    }

  free (body);
  return true;
}

/* Stores description of number of iterations of LOOP derived from
   EXIT (an exit edge of the LOOP) in NITER.  Returns true if some
   useful information could be derived (and fields of NITER has
   meaning described in comments at struct tree_niter_desc
   declaration), false otherwise.  If WARN is true and
   -Wunsafe-loop-optimizations was given, warn if the optimizer is going to use
   potentially unsafe assumptions.  */

bool
number_of_iterations_exit (struct loop *loop, edge exit,
			   struct tree_niter_desc *niter,
			   bool warn)
{
  return false;
}

/* Tries to count the number of iterations of LOOP till it exits by EXIT
   by brute force -- i.e. by determining the value of the operands of the
   condition at EXIT in first few iterations of the loop (assuming that
   these values are constant) and determining the first one in that the
   condition is not satisfied.  Returns the constant giving the number
   of the iterations of LOOP if successful, chrec_dont_know otherwise.  */

tree
loop_niter_by_eval (struct loop *loop, edge exit)
{
  return chrec_dont_know;
}

/* Returns true if statement S1 dominates statement S2.  */

bool
stmt_dominates_stmt_p (gimple s1, gimple s2)
{
  basic_block bb1 = gimple_bb (s1), bb2 = gimple_bb (s2);

  if (!bb1
      || s1 == s2)
    return true;

  if (bb1 == bb2)
    {
      gimple_stmt_iterator bsi;

      if (gimple_code (s2) == GIMPLE_PHI)
	return false;

      if (gimple_code (s1) == GIMPLE_PHI)
	return true;

      for (bsi = gsi_start_bb (bb1); gsi_stmt (bsi) != s2; gsi_next (&bsi))
	if (gsi_stmt (bsi) == s1)
	  return true;

      return false;
    }

  return dominated_by_p (CDI_DOMINATORS, bb2, bb1);
}

/* Returns true if the arithmetics in TYPE can be assumed not to wrap.  */

bool
nowrap_type_p (tree type)
{
  if (INTEGRAL_TYPE_P (type)
      && TYPE_OVERFLOW_UNDEFINED (type))
    return true;

  if (POINTER_TYPE_P (type))
    return true;

  return false;
}

/* Return false only when the induction variable BASE + STEP * I is
   known to not overflow: i.e. when the number of iterations is small
   enough with respect to the step and initial condition in order to
   keep the evolution confined in TYPEs bounds.  Return true when the
   iv is known to overflow or when the property is not computable.

   USE_OVERFLOW_SEMANTICS is true if this function should assume that
   the rules for overflow of the given language apply (e.g., that signed
   arithmetics in C does not overflow).  */

bool
scev_probably_wraps_p (tree base, tree step,
		       gimple at_stmt, struct loop *loop,
		       bool use_overflow_semantics)
{
  return true;
}

/* Frees the information on upper bounds on numbers of iterations of LOOP.  */

void
free_numbers_of_iterations_estimates_loop (struct loop *loop)
{
  struct nb_iter_bound *bound, *next;

  loop->nb_iterations = NULL;
  loop->estimate_state = EST_NOT_COMPUTED;
  for (bound = loop->bounds; bound; bound = next)
    {
      next = bound->next;
      ggc_free (bound);
    }

  loop->bounds = NULL;
}

/* Frees the information on upper bounds on numbers of iterations of loops.  */

void
free_numbers_of_iterations_estimates (void)
{
  loop_iterator li;
  struct loop *loop;

  FOR_EACH_LOOP (li, loop, 0)
    {
      free_numbers_of_iterations_estimates_loop (loop);
    }
}

/* Substitute value VAL for ssa name NAME inside expressions held
   at LOOP.  */

void
substitute_in_loop_info (struct loop *loop, tree name, tree val)
{
  loop->nb_iterations = simplify_replace_tree (loop->nb_iterations, name, val);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
