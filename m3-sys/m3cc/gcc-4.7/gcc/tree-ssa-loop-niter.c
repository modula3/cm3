/* Modula-3: modified */

/* Functions to determine/estimate number of iterations of a loop.
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012
   Free Software Foundation, Inc.

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
#include "tm_p.h"
#include "basic-block.h"
#include "output.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
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
#include "diagnostic-core.h"
#include "tree-inline.h"

#define SWAP(X, Y) do { affine_iv *tmp = (X); (X) = (Y); (Y) = tmp; } while (0)

/* The maximum number of dominator BBs we search for conditions
   of loop header copies we use for simplifying a conditional
   expression.  */
#define MAX_DOMINATORS_TO_WALK 8

/*

   Analysis of number of iterations of an affine exit test.

*/

/* Substitute NEW for OLD in EXPR and fold the result.  */

static tree
simplify_replace_tree (tree expr, tree old, tree new_tree)
{
  unsigned i, n;
  tree ret = NULL_TREE, e, se;

  if (!expr)
    return NULL_TREE;

  /* Do not bother to replace constants.  */
  if (CONSTANT_CLASS_P (old))
    return expr;

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


/* Try to determine the number of iterations of LOOP.  If we succeed,
   expression giving number of iterations is returned and *EXIT is
   set to the edge from that the information is obtained.  Otherwise
   chrec_dont_know is returned.  */

tree
find_loop_niter (struct loop *loop, edge *exit)
{
  return chrec_dont_know;
}

/* Return true if loop is known to have bounded number of iterations.  */

bool
finite_loop_p (struct loop *loop)
{
  return false;
}

/*

   Analysis of a number of iterations of a loop by a brute-force evaluation.

*/

/* Bound on the number of iterations we try to evaluate.  */

#define MAX_ITERATIONS_TO_TRACK \
  ((unsigned) PARAM_VALUE (PARAM_MAX_ITERATIONS_TO_TRACK))

/* Returns the loop phi node of LOOP such that ssa name X is derived from its
   result by a chain of operations such that all but exactly one of their
   operands are constants.  */

static gimple
chain_of_csts_start (struct loop *loop, tree x)
{
  gimple stmt = SSA_NAME_DEF_STMT (x);
  tree use;
  basic_block bb = gimple_bb (stmt);
  enum tree_code code;

  if (!bb
      || !flow_bb_inside_loop_p (loop, bb))
    return NULL;

  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      if (bb == loop->header)
	return stmt;

      return NULL;
    }

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return NULL;

  code = gimple_assign_rhs_code (stmt);
  if (gimple_references_memory_p (stmt)
      || TREE_CODE_CLASS (code) == tcc_reference
      || (code == ADDR_EXPR
	  && !is_gimple_min_invariant (gimple_assign_rhs1 (stmt))))
    return NULL;

  use = SINGLE_SSA_TREE_OPERAND (stmt, SSA_OP_USE);
  if (use == NULL_TREE)
    return NULL;

  return chain_of_csts_start (loop, use);
}

/* Determines whether the expression X is derived from a result of a phi node
   in header of LOOP such that

   * the derivation of X consists only from operations with constants
   * the initial value of the phi node is constant
   * the value of the phi node in the next iteration can be derived from the
     value in the current iteration by a chain of operations with constants.

   If such phi node exists, it is returned, otherwise NULL is returned.  */

static gimple
get_base_for (struct loop *loop, tree x)
{
  gimple phi;
  tree init, next;

  if (is_gimple_min_invariant (x))
    return NULL;

  phi = chain_of_csts_start (loop, x);
  if (!phi)
    return NULL;

  init = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (loop));
  next = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (loop));

  if (TREE_CODE (next) != SSA_NAME)
    return NULL;

  if (!is_gimple_min_invariant (init))
    return NULL;

  if (chain_of_csts_start (loop, next) != phi)
    return NULL;

  return phi;
}

/* Given an expression X, then

   * if X is NULL_TREE, we return the constant BASE.
   * otherwise X is a SSA name, whose value in the considered loop is derived
     by a chain of operations with constant from a result of a phi node in
     the header of the loop.  Then we return value of X when the value of the
     result of this phi node is given by the constant BASE.  */

static tree
get_val_for (tree x, tree base)
{
  gimple stmt;

  gcc_assert (is_gimple_min_invariant (base));

  if (!x)
    return base;

  stmt = SSA_NAME_DEF_STMT (x);
  if (gimple_code (stmt) == GIMPLE_PHI)
    return base;

  gcc_assert (is_gimple_assign (stmt));

  /* STMT must be either an assignment of a single SSA name or an
     expression involving an SSA name and a constant.  Try to fold that
     expression using the value for the SSA name.  */
  if (gimple_assign_ssa_name_copy_p (stmt))
    return get_val_for (gimple_assign_rhs1 (stmt), base);
  else if (gimple_assign_rhs_class (stmt) == GIMPLE_UNARY_RHS
	   && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME)
    {
      return fold_build1 (gimple_assign_rhs_code (stmt),
			  gimple_expr_type (stmt),
			  get_val_for (gimple_assign_rhs1 (stmt), base));
    }
  else if (gimple_assign_rhs_class (stmt) == GIMPLE_BINARY_RHS)
    {
      tree rhs1 = gimple_assign_rhs1 (stmt);
      tree rhs2 = gimple_assign_rhs2 (stmt);
      if (TREE_CODE (rhs1) == SSA_NAME)
	rhs1 = get_val_for (rhs1, base);
      else if (TREE_CODE (rhs2) == SSA_NAME)
	rhs2 = get_val_for (rhs2, base);
      else
	gcc_unreachable ();
      return fold_build2 (gimple_assign_rhs_code (stmt),
			  gimple_expr_type (stmt), rhs1, rhs2);
    }
  else
    gcc_unreachable ();
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
  tree acnd;
  tree op[2], val[2], next[2], aval[2];
  gimple phi, cond;
  unsigned i, j;
  enum tree_code cmp;

  cond = last_stmt (exit->src);
  if (!cond || gimple_code (cond) != GIMPLE_COND)
    return chrec_dont_know;

  cmp = gimple_cond_code (cond);
  if (exit->flags & EDGE_TRUE_VALUE)
    cmp = invert_tree_comparison (cmp, false);

  switch (cmp)
    {
    case EQ_EXPR:
    case NE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case LE_EXPR:
      op[0] = gimple_cond_lhs (cond);
      op[1] = gimple_cond_rhs (cond);
      break;

    default:
      return chrec_dont_know;
    }

  for (j = 0; j < 2; j++)
    {
      if (is_gimple_min_invariant (op[j]))
	{
	  val[j] = op[j];
	  next[j] = NULL_TREE;
	  op[j] = NULL_TREE;
	}
      else
	{
	  phi = get_base_for (loop, op[j]);
	  if (!phi)
	    return chrec_dont_know;
	  val[j] = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (loop));
	  next[j] = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (loop));
	}
    }

  /* Don't issue signed overflow warnings.  */
  fold_defer_overflow_warnings ();

  for (i = 0; i < MAX_ITERATIONS_TO_TRACK; i++)
    {
      for (j = 0; j < 2; j++)
	aval[j] = get_val_for (op[j], val[j]);

      acnd = fold_binary (cmp, boolean_type_node, aval[0], aval[1]);
      if (acnd && integer_zerop (acnd))
	{
	  fold_undefer_and_ignore_overflow_warnings ();
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "Proved that loop %d iterates %d times using brute force.\n",
		     loop->num, i);
	  return build_int_cst (unsigned_type_node, i);
	}

      for (j = 0; j < 2; j++)
	{
	  val[j] = get_val_for (next[j], val[j]);
	  if (!is_gimple_min_invariant (val[j]))
	    {
	      fold_undefer_and_ignore_overflow_warnings ();
	      return chrec_dont_know;
	    }
	}
    }

  fold_undefer_and_ignore_overflow_warnings ();

  return chrec_dont_know;
}

/* Finds the exit of the LOOP by that the loop exits after a constant
   number of iterations and stores the exit edge to *EXIT.  The constant
   giving the number of iterations of LOOP is returned.  The number of
   iterations is determined using loop_niter_by_eval (i.e. by brute force
   evaluation).  If we are unable to find the exit for that loop_niter_by_eval
   determines the number of iterations, chrec_dont_know is returned.  */

tree
find_loop_niter_by_eval (struct loop *loop, edge *exit)
{
  unsigned i;
  VEC (edge, heap) *exits = get_loop_exit_edges (loop);
  edge ex;
  tree niter = NULL_TREE, aniter;

  *exit = NULL;

  /* Loops with multiple exits are expensive to handle and less important.  */
  if (!flag_expensive_optimizations
      && VEC_length (edge, exits) > 1)
    return chrec_dont_know;

  FOR_EACH_VEC_ELT (edge, exits, i, ex)
    {
      if (!just_once_each_iteration_p (loop, ex->src))
	continue;

      aniter = loop_niter_by_eval (loop, ex);
      if (chrec_contains_undetermined (aniter))
	continue;

      if (niter
	  && !tree_int_cst_lt (aniter, niter))
	continue;

      niter = aniter;
      *exit = ex;
    }
  VEC_free (edge, heap, exits);

  return niter ? niter : chrec_dont_know;
}

/*

   Analysis of upper bounds on number of iterations of a loop.

*/

static double_int derive_constant_upper_bound_ops (tree, tree,
						   enum tree_code, tree);

/* Returns a constant upper bound on the value of the right-hand side of
   an assignment statement STMT.  */

static double_int
derive_constant_upper_bound_assign (gimple stmt)
{
  enum tree_code code = gimple_assign_rhs_code (stmt);
  tree op0 = gimple_assign_rhs1 (stmt);
  tree op1 = gimple_assign_rhs2 (stmt);

  return derive_constant_upper_bound_ops (TREE_TYPE (gimple_assign_lhs (stmt)),
					  op0, code, op1);
}

/* Returns a constant upper bound on the value of expression VAL.  VAL
   is considered to be unsigned.  If its type is signed, its value must
   be nonnegative.  */

static double_int
derive_constant_upper_bound (tree val)
{
  enum tree_code code;
  tree op0, op1;

  extract_ops_from_tree (val, &code, &op0, &op1);
  return derive_constant_upper_bound_ops (TREE_TYPE (val), op0, code, op1);
}

/* Returns a constant upper bound on the value of expression OP0 CODE OP1,
   whose type is TYPE.  The expression is considered to be unsigned.  If
   its type is signed, its value must be nonnegative.  */

static double_int
derive_constant_upper_bound_ops (tree type, tree op0,
				 enum tree_code code, tree op1)
{
  tree subtype, maxt;
  double_int bnd, max, mmax, cst;
  gimple stmt;

  if (INTEGRAL_TYPE_P (type))
    maxt = TYPE_MAX_VALUE (type);
  else
    maxt = upper_bound_in_type (type, type);

  max = tree_to_double_int (maxt);

  switch (code)
    {
    case INTEGER_CST:
      return tree_to_double_int (op0);

    CASE_CONVERT:
      subtype = TREE_TYPE (op0);
      if (!TYPE_UNSIGNED (subtype)
	  /* If TYPE is also signed, the fact that VAL is nonnegative implies
	     that OP0 is nonnegative.  */
	  && TYPE_UNSIGNED (type)
	  && !tree_expr_nonnegative_p (op0))
	{
	  /* If we cannot prove that the casted expression is nonnegative,
	     we cannot establish more useful upper bound than the precision
	     of the type gives us.  */
	  return max;
	}

      /* We now know that op0 is an nonnegative value.  Try deriving an upper
	 bound for it.  */
      bnd = derive_constant_upper_bound (op0);

      /* If the bound does not fit in TYPE, max. value of TYPE could be
	 attained.  */
      if (double_int_ucmp (max, bnd) < 0)
	return max;

      return bnd;

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
      if (TREE_CODE (op1) != INTEGER_CST
	  || !tree_expr_nonnegative_p (op0))
	return max;

      /* Canonicalize to OP0 - CST.  Consider CST to be signed, in order to
	 choose the most logical way how to treat this constant regardless
	 of the signedness of the type.  */
      cst = tree_to_double_int (op1);
      cst = double_int_sext (cst, TYPE_PRECISION (type));
      if (code != MINUS_EXPR)
	cst = double_int_neg (cst);

      bnd = derive_constant_upper_bound (op0);

      if (double_int_negative_p (cst))
	{
	  cst = double_int_neg (cst);
	  /* Avoid CST == 0x80000...  */
	  if (double_int_negative_p (cst))
	    return max;;

	  /* OP0 + CST.  We need to check that
	     BND <= MAX (type) - CST.  */

	  mmax = double_int_sub (max, cst);
	  if (double_int_ucmp (bnd, mmax) > 0)
	    return max;

	  return double_int_add (bnd, cst);
	}
      else
	{
	  /* OP0 - CST, where CST >= 0.

	     If TYPE is signed, we have already verified that OP0 >= 0, and we
	     know that the result is nonnegative.  This implies that
	     VAL <= BND - CST.

	     If TYPE is unsigned, we must additionally know that OP0 >= CST,
	     otherwise the operation underflows.
	   */

	  /* This should only happen if the type is unsigned; however, for
	     buggy programs that use overflowing signed arithmetics even with
	     -fno-wrapv, this condition may also be true for signed values.  */
	  if (double_int_ucmp (bnd, cst) < 0)
	    return max;

	  if (TYPE_UNSIGNED (type))
	    {
	      tree tem = fold_binary (GE_EXPR, boolean_type_node, op0,
				      double_int_to_tree (type, cst));
	      if (!tem || integer_nonzerop (tem))
		return max;
	    }

	  bnd = double_int_sub (bnd, cst);
	}

      return bnd;

    case FLOOR_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if (TREE_CODE (op1) != INTEGER_CST
	  || tree_int_cst_sign_bit (op1))
	return max;

      bnd = derive_constant_upper_bound (op0);
      return double_int_udiv (bnd, tree_to_double_int (op1), FLOOR_DIV_EXPR);

    case BIT_AND_EXPR:
      if (TREE_CODE (op1) != INTEGER_CST
	  || tree_int_cst_sign_bit (op1))
	return max;
      return tree_to_double_int (op1);

    case SSA_NAME:
      stmt = SSA_NAME_DEF_STMT (op0);
      if (gimple_code (stmt) != GIMPLE_ASSIGN
	  || gimple_assign_lhs (stmt) != op0)
	return max;
      return derive_constant_upper_bound_assign (stmt);

    default:
      return max;
    }
}

/* Records that every statement in LOOP is executed I_BOUND times.
   REALISTIC is true if I_BOUND is expected to be close to the real number
   of iterations.  UPPER is true if we are sure the loop iterates at most
   I_BOUND times.  */

static void
record_niter_bound (struct loop *loop, double_int i_bound, bool realistic,
		    bool upper)
{
  /* Update the bounds only when there is no previous estimation, or when the current
     estimation is smaller.  */
  if (upper
      && (!loop->any_upper_bound
	  || double_int_ucmp (i_bound, loop->nb_iterations_upper_bound) < 0))
    {
      loop->any_upper_bound = true;
      loop->nb_iterations_upper_bound = i_bound;
    }
  if (realistic
      && (!loop->any_estimate
	  || double_int_ucmp (i_bound, loop->nb_iterations_estimate) < 0))
    {
      loop->any_estimate = true;
      loop->nb_iterations_estimate = i_bound;
    }
}

/* Records that AT_STMT is executed at most BOUND + 1 times in LOOP.  IS_EXIT
   is true if the loop is exited immediately after STMT, and this exit
   is taken at last when the STMT is executed BOUND + 1 times.
   REALISTIC is true if BOUND is expected to be close to the real number
   of iterations.  UPPER is true if we are sure the loop iterates at most
   BOUND times.  I_BOUND is an unsigned double_int upper estimate on BOUND.  */

static void
record_estimate (struct loop *loop, tree bound, double_int i_bound,
		 gimple at_stmt, bool is_exit, bool realistic, bool upper)
{
  double_int delta;
  edge exit;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Statement %s", is_exit ? "(exit)" : "");
      print_gimple_stmt (dump_file, at_stmt, 0, TDF_SLIM);
      fprintf (dump_file, " is %sexecuted at most ",
	       upper ? "" : "probably ");
      print_generic_expr (dump_file, bound, TDF_SLIM);
      fprintf (dump_file, " (bounded by ");
      dump_double_int (dump_file, i_bound, true);
      fprintf (dump_file, ") + 1 times in loop %d.\n", loop->num);
    }

  /* If the I_BOUND is just an estimate of BOUND, it rarely is close to the
     real number of iterations.  */
  if (TREE_CODE (bound) != INTEGER_CST)
    realistic = false;
  if (!upper && !realistic)
    return;

  /* If we have a guaranteed upper bound, record it in the appropriate
     list.  */
  if (upper)
    {
      struct nb_iter_bound *elt = ggc_alloc_nb_iter_bound ();

      elt->bound = i_bound;
      elt->stmt = at_stmt;
      elt->is_exit = is_exit;
      elt->next = loop->bounds;
      loop->bounds = elt;
    }

  /* Update the number of iteration estimates according to the bound.
     If at_stmt is an exit or dominates the single exit from the loop,
     then the loop latch is executed at most BOUND times, otherwise
     it can be executed BOUND + 1 times.  */
  exit = single_exit (loop);
  if (is_exit
      || (exit != NULL
	  && dominated_by_p (CDI_DOMINATORS,
			     exit->src, gimple_bb (at_stmt))))
    delta = double_int_zero;
  else
    delta = double_int_one;
  i_bound = double_int_add (i_bound, delta);

  /* If an overflow occurred, ignore the result.  */
  if (double_int_ucmp (i_bound, delta) < 0)
    return;

  record_niter_bound (loop, i_bound, realistic, upper);
}

/* Record the estimate on number of iterations of LOOP based on the fact that
   the induction variable BASE + STEP * i evaluated in STMT does not wrap and
   its values belong to the range <LOW, HIGH>.  REALISTIC is true if the
   estimated number of iterations is expected to be close to the real one.
   UPPER is true if we are sure the induction variable does not wrap.  */

static void
record_nonwrapping_iv (struct loop *loop, tree base, tree step, gimple stmt,
		       tree low, tree high, bool realistic, bool upper)
{
  tree niter_bound, extreme, delta;
  tree type = TREE_TYPE (base), unsigned_type;
  double_int max;

  if (TREE_CODE (step) != INTEGER_CST || integer_zerop (step))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Induction variable (");
      print_generic_expr (dump_file, TREE_TYPE (base), TDF_SLIM);
      fprintf (dump_file, ") ");
      print_generic_expr (dump_file, base, TDF_SLIM);
      fprintf (dump_file, " + ");
      print_generic_expr (dump_file, step, TDF_SLIM);
      fprintf (dump_file, " * iteration does not wrap in statement ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
      fprintf (dump_file, " in loop %d.\n", loop->num);
    }

  unsigned_type = unsigned_type_for (type);
  base = fold_convert (unsigned_type, base);
  step = fold_convert (unsigned_type, step);

  if (tree_int_cst_sign_bit (step))
    {
      extreme = fold_convert (unsigned_type, low);
      if (TREE_CODE (base) != INTEGER_CST)
	base = fold_convert (unsigned_type, high);
      delta = fold_build2 (MINUS_EXPR, unsigned_type, base, extreme);
      step = fold_build1 (NEGATE_EXPR, unsigned_type, step);
    }
  else
    {
      extreme = fold_convert (unsigned_type, high);
      if (TREE_CODE (base) != INTEGER_CST)
	base = fold_convert (unsigned_type, low);
      delta = fold_build2 (MINUS_EXPR, unsigned_type, extreme, base);
    }

  /* STMT is executed at most NITER_BOUND + 1 times, since otherwise the value
     would get out of the range.  */
  niter_bound = fold_build2 (FLOOR_DIV_EXPR, unsigned_type, delta, step);
  max = derive_constant_upper_bound (niter_bound);
  record_estimate (loop, niter_bound, max, stmt, false, realistic, upper);
}

/* Returns true if REF is a reference to an array at the end of a dynamically
   allocated structure.  If this is the case, the array may be allocated larger
   than its upper bound implies.  */

bool
array_at_struct_end_p (tree ref)
{
  tree base = get_base_address (ref);
  tree parent, field;

  /* Unless the reference is through a pointer, the size of the array matches
     its declaration.  */
  if (!base || (!INDIRECT_REF_P (base) && TREE_CODE (base) != MEM_REF))
    return false;

  for (;handled_component_p (ref); ref = parent)
    {
      parent = TREE_OPERAND (ref, 0);

      if (TREE_CODE (ref) == COMPONENT_REF)
	{
	  /* All fields of a union are at its end.  */
	  if (TREE_CODE (TREE_TYPE (parent)) == UNION_TYPE)
	    continue;

	  /* Unless the field is at the end of the struct, we are done.  */
	  field = TREE_OPERAND (ref, 1);
	  if (DECL_CHAIN (field))
	    return false;
	}

      /* The other options are ARRAY_REF, ARRAY_RANGE_REF, VIEW_CONVERT_EXPR.
	 In all these cases, we might be accessing the last element, and
	 although in practice this will probably never happen, it is legal for
	 the indices of this last element to exceed the bounds of the array.
	 Therefore, continue checking.  */
    }

  return true;
}

/* Determine information about number of iterations a LOOP from the index
   IDX of a data reference accessed in STMT.  RELIABLE is true if STMT is
   guaranteed to be executed in every iteration of LOOP.  Callback for
   for_each_index.  */

struct ilb_data
{
  struct loop *loop;
  gimple stmt;
  bool reliable;
};

static bool
idx_infer_loop_bounds (tree base, tree *idx, void *dta)
{
  struct ilb_data *data = (struct ilb_data *) dta;
  tree ev, init, step;
  tree low, high, type, next;
  bool sign, upper = data->reliable, at_end = false;
  struct loop *loop = data->loop;

  if (TREE_CODE (base) != ARRAY_REF)
    return true;

  /* For arrays at the end of the structure, we are not guaranteed that they
     do not really extend over their declared size.  However, for arrays of
     size greater than one, this is unlikely to be intended.  */
  if (array_at_struct_end_p (base))
    {
      at_end = true;
      upper = false;
    }

  ev = instantiate_parameters (loop, analyze_scalar_evolution (loop, *idx));
  init = initial_condition (ev);
  step = evolution_part_in_loop_num (ev, loop->num);

  if (!init
      || !step
      || TREE_CODE (step) != INTEGER_CST
      || integer_zerop (step)
      || tree_contains_chrecs (init, NULL)
      || chrec_contains_symbols_defined_in_loop (init, loop->num))
    return true;

  low = array_ref_low_bound (base);
  high = array_ref_up_bound (base);

  /* The case of nonconstant bounds could be handled, but it would be
     complicated.  */
  if (TREE_CODE (low) != INTEGER_CST
      || !high
      || TREE_CODE (high) != INTEGER_CST)
    return true;
  sign = tree_int_cst_sign_bit (step);
  type = TREE_TYPE (step);

  /* The array of length 1 at the end of a structure most likely extends
     beyond its bounds.  */
  if (at_end
      && operand_equal_p (low, high, 0))
    return true;

  /* In case the relevant bound of the array does not fit in type, or
     it does, but bound + step (in type) still belongs into the range of the
     array, the index may wrap and still stay within the range of the array
     (consider e.g. if the array is indexed by the full range of
     unsigned char).

     To make things simpler, we require both bounds to fit into type, although
     there are cases where this would not be strictly necessary.  */
  if (!int_fits_type_p (high, type)
      || !int_fits_type_p (low, type))
    return true;
  low = fold_convert (type, low);
  high = fold_convert (type, high);

  if (sign)
    next = fold_binary (PLUS_EXPR, type, low, step);
  else
    next = fold_binary (PLUS_EXPR, type, high, step);

  if (tree_int_cst_compare (low, next) <= 0
      && tree_int_cst_compare (next, high) <= 0)
    return true;

  record_nonwrapping_iv (loop, init, step, data->stmt, low, high, true, upper);
  return true;
}

/* Determine information about number of iterations a LOOP from the bounds
   of arrays in the data reference REF accessed in STMT.  RELIABLE is true if
   STMT is guaranteed to be executed in every iteration of LOOP.*/

static void
infer_loop_bounds_from_ref (struct loop *loop, gimple stmt, tree ref,
			    bool reliable)
{
  struct ilb_data data;

  data.loop = loop;
  data.stmt = stmt;
  data.reliable = reliable;
  for_each_index (&ref, idx_infer_loop_bounds, &data);
}

/* Determine information about number of iterations of a LOOP from the way
   arrays are used in STMT.  RELIABLE is true if STMT is guaranteed to be
   executed in every iteration of LOOP.  */

static void
infer_loop_bounds_from_array (struct loop *loop, gimple stmt, bool reliable)
{
  if (is_gimple_assign (stmt))
    {
      tree op0 = gimple_assign_lhs (stmt);
      tree op1 = gimple_assign_rhs1 (stmt);

      /* For each memory access, analyze its access function
	 and record a bound on the loop iteration domain.  */
      if (REFERENCE_CLASS_P (op0))
	infer_loop_bounds_from_ref (loop, stmt, op0, reliable);

      if (REFERENCE_CLASS_P (op1))
	infer_loop_bounds_from_ref (loop, stmt, op1, reliable);
    }
  else if (is_gimple_call (stmt))
    {
      tree arg, lhs;
      unsigned i, n = gimple_call_num_args (stmt);

      lhs = gimple_call_lhs (stmt);
      if (lhs && REFERENCE_CLASS_P (lhs))
	infer_loop_bounds_from_ref (loop, stmt, lhs, reliable);

      for (i = 0; i < n; i++)
	{
	  arg = gimple_call_arg (stmt, i);
	  if (REFERENCE_CLASS_P (arg))
	    infer_loop_bounds_from_ref (loop, stmt, arg, reliable);
	}
    }
}

/* Determine information about number of iterations of a LOOP from the fact
   that pointer arithmetics in STMT does not overflow.  */

static void
infer_loop_bounds_from_pointer_arith (struct loop *loop, gimple stmt)
{
  tree def, base, step, scev, type, low, high;
  tree var, ptr;

  if (!is_gimple_assign (stmt)
      || gimple_assign_rhs_code (stmt) != POINTER_PLUS_EXPR)
    return;

  def = gimple_assign_lhs (stmt);
  if (TREE_CODE (def) != SSA_NAME)
    return;

  type = TREE_TYPE (def);
  if (!nowrap_type_p (type))
    return;

  ptr = gimple_assign_rhs1 (stmt);
  if (!expr_invariant_in_loop_p (loop, ptr))
    return;

  var = gimple_assign_rhs2 (stmt);
  if (TYPE_PRECISION (type) != TYPE_PRECISION (TREE_TYPE (var)))
    return;

  scev = instantiate_parameters (loop, analyze_scalar_evolution (loop, def));
  if (chrec_contains_undetermined (scev))
    return;

  base = initial_condition_in_loop_num (scev, loop->num);
  step = evolution_part_in_loop_num (scev, loop->num);

  if (!base || !step
      || TREE_CODE (step) != INTEGER_CST
      || tree_contains_chrecs (base, NULL)
      || chrec_contains_symbols_defined_in_loop (base, loop->num))
    return;

  low = lower_bound_in_type (type, type);
  high = upper_bound_in_type (type, type);

  /* In C, pointer arithmetic p + 1 cannot use a NULL pointer, and p - 1 cannot
     produce a NULL pointer.  The contrary would mean NULL points to an object,
     while NULL is supposed to compare unequal with the address of all objects.
     Furthermore, p + 1 cannot produce a NULL pointer and p - 1 cannot use a
     NULL pointer since that would mean wrapping, which we assume here not to
     happen.  So, we can exclude NULL from the valid range of pointer
     arithmetic.  */
  if (flag_delete_null_pointer_checks && int_cst_value (low) == 0)
    low = build_int_cstu (TREE_TYPE (low), TYPE_ALIGN_UNIT (TREE_TYPE (type)));

  record_nonwrapping_iv (loop, base, step, stmt, low, high, false, true);
}

/* Determine information about number of iterations of a LOOP from the fact
   that signed arithmetics in STMT does not overflow.  */

static void
infer_loop_bounds_from_signedness (struct loop *loop, gimple stmt)
{
  tree def, base, step, scev, type, low, high;

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return;

  def = gimple_assign_lhs (stmt);

  if (TREE_CODE (def) != SSA_NAME)
    return;

  type = TREE_TYPE (def);
  if (!INTEGRAL_TYPE_P (type)
      || !TYPE_OVERFLOW_UNDEFINED (type))
    return;

  scev = instantiate_parameters (loop, analyze_scalar_evolution (loop, def));
  if (chrec_contains_undetermined (scev))
    return;

  base = initial_condition_in_loop_num (scev, loop->num);
  step = evolution_part_in_loop_num (scev, loop->num);

  if (!base || !step
      || TREE_CODE (step) != INTEGER_CST
      || tree_contains_chrecs (base, NULL)
      || chrec_contains_symbols_defined_in_loop (base, loop->num))
    return;

  low = lower_bound_in_type (type, type);
  high = upper_bound_in_type (type, type);

  record_nonwrapping_iv (loop, base, step, stmt, low, high, false, true);
}

/* The following analyzers are extracting informations on the bounds
   of LOOP from the following undefined behaviors:

   - data references should not access elements over the statically
     allocated size,

   - signed variables should not overflow when flag_wrapv is not set.
*/

static void
infer_loop_bounds_from_undefined (struct loop *loop)
{
  unsigned i;
  basic_block *bbs;
  gimple_stmt_iterator bsi;
  basic_block bb;
  bool reliable;

  bbs = get_loop_body (loop);

  for (i = 0; i < loop->num_nodes; i++)
    {
      bb = bbs[i];

      /* If BB is not executed in each iteration of the loop, we cannot
	 use the operations in it to infer reliable upper bound on the
	 # of iterations of the loop.  However, we can use it as a guess.  */
      reliable = dominated_by_p (CDI_DOMINATORS, loop->latch, bb);

      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  gimple stmt = gsi_stmt (bsi);

	  infer_loop_bounds_from_array (loop, stmt, reliable);

	  if (reliable)
            {
              infer_loop_bounds_from_signedness (loop, stmt);
              infer_loop_bounds_from_pointer_arith (loop, stmt);
            }
  	}

    }

  free (bbs);
}

/* Converts VAL to double_int.  */

static double_int
gcov_type_to_double_int (gcov_type val)
{
  double_int ret;

  ret.low = (unsigned HOST_WIDE_INT) val;
  /* If HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_WIDEST_INT, avoid shifting by
     the size of type.  */
  val >>= HOST_BITS_PER_WIDE_INT - 1;
  val >>= 1;
  ret.high = (unsigned HOST_WIDE_INT) val;

  return ret;
}

/* Records estimates on numbers of iterations of LOOP.  If USE_UNDEFINED_P
   is true also use estimates derived from undefined behavior.  */

void
estimate_numbers_of_iterations_loop (struct loop *loop, bool use_undefined_p)
{
  VEC (edge, heap) *exits;
  tree niter, type;
  unsigned i;
  struct tree_niter_desc niter_desc;
  edge ex;
  double_int bound;

  /* Give up if we already have tried to compute an estimation.  */
  if (loop->estimate_state != EST_NOT_COMPUTED)
    return;
  loop->estimate_state = EST_AVAILABLE;
  loop->any_upper_bound = false;
  loop->any_estimate = false;

  exits = get_loop_exit_edges (loop);
  FOR_EACH_VEC_ELT (edge, exits, i, ex)
    {
      if (!number_of_iterations_exit (loop, ex, &niter_desc, false))
	continue;

      niter = niter_desc.niter;
      type = TREE_TYPE (niter);
      if (TREE_CODE (niter_desc.may_be_zero) != INTEGER_CST)
	niter = build3 (COND_EXPR, type, niter_desc.may_be_zero,
			build_int_cst (type, 0),
			niter);
      record_estimate (loop, niter, niter_desc.max,
		       last_stmt (ex->src),
		       true, true, true);
    }
  VEC_free (edge, heap, exits);

  if (use_undefined_p)
    infer_loop_bounds_from_undefined (loop);

  /* If we have a measured profile, use it to estimate the number of
     iterations.  */
  if (loop->header->count != 0)
    {
      gcov_type nit = expected_loop_iterations_unbounded (loop) + 1;
      bound = gcov_type_to_double_int (nit);
      record_niter_bound (loop, bound, true, false);
    }

  /* If an upper bound is smaller than the realistic estimate of the
     number of iterations, use the upper bound instead.  */
  if (loop->any_upper_bound
      && loop->any_estimate
      && double_int_ucmp (loop->nb_iterations_upper_bound,
			  loop->nb_iterations_estimate) < 0)
    loop->nb_iterations_estimate = loop->nb_iterations_upper_bound;
}

/* Sets NIT to the estimated number of executions of the latch of the
   LOOP.  If CONSERVATIVE is true, we must be sure that NIT is at least as
   large as the number of iterations.  If we have no reliable estimate,
   the function returns false, otherwise returns true.  */

bool
estimated_loop_iterations (struct loop *loop, bool conservative,
			   double_int *nit)
{
  estimate_numbers_of_iterations_loop (loop, true);
  if (conservative)
    {
      if (!loop->any_upper_bound)
	return false;

      *nit = loop->nb_iterations_upper_bound;
    }
  else
    {
      if (!loop->any_estimate)
	return false;

      *nit = loop->nb_iterations_estimate;
    }

  return true;
}

/* Similar to estimated_loop_iterations, but returns the estimate only
   if it fits to HOST_WIDE_INT.  If this is not the case, or the estimate
   on the number of iterations of LOOP could not be derived, returns -1.  */

HOST_WIDE_INT
estimated_loop_iterations_int (struct loop *loop, bool conservative)
{
  double_int nit;
  HOST_WIDE_INT hwi_nit;

  if (!estimated_loop_iterations (loop, conservative, &nit))
    return -1;

  if (!double_int_fits_in_shwi_p (nit))
    return -1;
  hwi_nit = double_int_to_shwi (nit);

  return hwi_nit < 0 ? -1 : hwi_nit;
}

/* Returns an upper bound on the number of executions of statements
   in the LOOP.  For statements before the loop exit, this exceeds
   the number of execution of the latch by one.  */

HOST_WIDE_INT
max_stmt_executions_int (struct loop *loop, bool conservative)
{
  HOST_WIDE_INT nit = estimated_loop_iterations_int (loop, conservative);
  HOST_WIDE_INT snit;

  if (nit == -1)
    return -1;

  snit = (HOST_WIDE_INT) ((unsigned HOST_WIDE_INT) nit + 1);

  /* If the computation overflows, return -1.  */
  return snit < 0 ? -1 : snit;
}

/* Sets NIT to the estimated number of executions of the latch of the
   LOOP, plus one.  If CONSERVATIVE is true, we must be sure that NIT is at
   least as large as the number of iterations.  If we have no reliable
   estimate, the function returns false, otherwise returns true.  */

bool
max_stmt_executions (struct loop *loop, bool conservative, double_int *nit)
{
  double_int nit_minus_one;

  if (!estimated_loop_iterations (loop, conservative, nit))
    return false;

  nit_minus_one = *nit;

  *nit = double_int_add (*nit, double_int_one);

  return double_int_ucmp (*nit, nit_minus_one) > 0;
}

/* Records estimates on numbers of iterations of loops.  */

void
estimate_numbers_of_iterations (bool use_undefined_p)
{
  loop_iterator li;
  struct loop *loop;

  /* We don't want to issue signed overflow warnings while getting
     loop iteration estimates.  */
  fold_defer_overflow_warnings ();

  FOR_EACH_LOOP (li, loop, 0)
    {
      estimate_numbers_of_iterations_loop (loop, use_undefined_p);
    }

  fold_undefer_and_ignore_overflow_warnings ();
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

/* Returns true when we can prove that the number of executions of
   STMT in the loop is at most NITER, according to the bound on
   the number of executions of the statement NITER_BOUND->stmt recorded in
   NITER_BOUND.  If STMT is NULL, we must prove this bound for all
   statements in the loop.  */

static bool
n_of_executions_at_most (gimple stmt,
			 struct nb_iter_bound *niter_bound,
			 tree niter)
{
  double_int bound = niter_bound->bound;
  tree nit_type = TREE_TYPE (niter), e;
  enum tree_code cmp;

  gcc_assert (TYPE_UNSIGNED (nit_type));

  /* If the bound does not even fit into NIT_TYPE, it cannot tell us that
     the number of iterations is small.  */
  if (!double_int_fits_to_tree_p (nit_type, bound))
    return false;

  /* We know that NITER_BOUND->stmt is executed at most NITER_BOUND->bound + 1
     times.  This means that:

     -- if NITER_BOUND->is_exit is true, then everything before
        NITER_BOUND->stmt is executed at most NITER_BOUND->bound + 1
	times, and everything after it at most NITER_BOUND->bound times.

     -- If NITER_BOUND->is_exit is false, then if we can prove that when STMT
	is executed, then NITER_BOUND->stmt is executed as well in the same
	iteration (we conclude that if both statements belong to the same
	basic block, or if STMT is after NITER_BOUND->stmt), then STMT
	is executed at most NITER_BOUND->bound + 1 times.  Otherwise STMT is
	executed at most NITER_BOUND->bound + 2 times.  */

  if (niter_bound->is_exit)
    {
      if (stmt
	  && stmt != niter_bound->stmt
	  && stmt_dominates_stmt_p (niter_bound->stmt, stmt))
	cmp = GE_EXPR;
      else
	cmp = GT_EXPR;
    }
  else
    {
      if (!stmt
	  || (gimple_bb (stmt) != gimple_bb (niter_bound->stmt)
	      && !stmt_dominates_stmt_p (niter_bound->stmt, stmt)))
	{
	  bound = double_int_add (bound, double_int_one);
	  if (double_int_zero_p (bound)
	      || !double_int_fits_to_tree_p (nit_type, bound))
	    return false;
	}
      cmp = GT_EXPR;
    }

  e = fold_binary (cmp, boolean_type_node,
		   niter, double_int_to_tree (nit_type, bound));
  return e && integer_nonzerop (e);
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
  struct nb_iter_bound *bound;
  tree delta, step_abs;
  tree unsigned_type, valid_niter;
  tree type = TREE_TYPE (step);

  /* FIXME: We really need something like
     http://gcc.gnu.org/ml/gcc-patches/2005-06/msg02025.html.

     We used to test for the following situation that frequently appears
     during address arithmetics:

       D.1621_13 = (long unsigned intD.4) D.1620_12;
       D.1622_14 = D.1621_13 * 8;
       D.1623_15 = (doubleD.29 *) D.1622_14;

     And derived that the sequence corresponding to D_14
     can be proved to not wrap because it is used for computing a
     memory access; however, this is not really the case -- for example,
     if D_12 = (unsigned char) [254,+,1], then D_14 has values
     2032, 2040, 0, 8, ..., but the code is still legal.  */

  if (chrec_contains_undetermined (base)
      || chrec_contains_undetermined (step))
    return true;

  if (integer_zerop (step))
    return false;

  /* If we can use the fact that signed and pointer arithmetics does not
     wrap, we are done.  */
  if (use_overflow_semantics && nowrap_type_p (TREE_TYPE (base)))
    return false;

  /* To be able to use estimates on number of iterations of the loop,
     we must have an upper bound on the absolute value of the step.  */
  if (TREE_CODE (step) != INTEGER_CST)
    return true;

  /* Don't issue signed overflow warnings.  */
  fold_defer_overflow_warnings ();

  /* Otherwise, compute the number of iterations before we reach the
     bound of the type, and verify that the loop is exited before this
     occurs.  */
  unsigned_type = unsigned_type_for (type);
  base = fold_convert (unsigned_type, base);

  if (tree_int_cst_sign_bit (step))
    {
      tree extreme = fold_convert (unsigned_type,
				   lower_bound_in_type (type, type));
      delta = fold_build2 (MINUS_EXPR, unsigned_type, base, extreme);
      step_abs = fold_build1 (NEGATE_EXPR, unsigned_type,
			      fold_convert (unsigned_type, step));
    }
  else
    {
      tree extreme = fold_convert (unsigned_type,
				   upper_bound_in_type (type, type));
      delta = fold_build2 (MINUS_EXPR, unsigned_type, extreme, base);
      step_abs = fold_convert (unsigned_type, step);
    }

  valid_niter = fold_build2 (FLOOR_DIV_EXPR, unsigned_type, delta, step_abs);

  estimate_numbers_of_iterations_loop (loop, true);
  for (bound = loop->bounds; bound; bound = bound->next)
    {
      if (n_of_executions_at_most (at_stmt, bound, valid_niter))
	{
	  fold_undefer_and_ignore_overflow_warnings ();
	  return false;
	}
    }

  fold_undefer_and_ignore_overflow_warnings ();

  /* At this point we still don't have a proof that the iv does not
     overflow: give up.  */
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
