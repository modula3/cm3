/* Modula-3: modified */

/* Copy propagation and SSA_NAME replacement support routines.
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2010
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "tm_p.h"
#include "basic-block.h"
#include "output.h"
#include "function.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "timevar.h"
#include "tree-dump.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"
#include "langhooks.h"
#include "cfgloop.h"

EXTERN_C_START

/* This file implements the copy propagation pass and provides a
   handful of interfaces for performing const/copy propagation and
   simple expression replacement which keep variable annotations
   up-to-date.

   We require that for any copy operation where the RHS and LHS have
   a non-null memory tag the memory tag be the same.   It is OK
   for one or both of the memory tags to be NULL.

   We also require tracking if a variable is dereferenced in a load or
   store operation.

   We enforce these requirements by having all copy propagation and
   replacements of one SSA_NAME with a different SSA_NAME to use the
   APIs defined in this file.  */

/* Return true if we may propagate ORIG into DEST, false otherwise.  */

bool
may_propagate_copy (tree dest, tree orig)
{
  tree type_d = TREE_TYPE (dest);
  tree type_o = TREE_TYPE (orig);

  gcc_unreachable ();

  /* If ORIG flows in from an abnormal edge, it cannot be propagated.  */
  if (TREE_CODE (orig) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (orig))
    return false;

  /* If DEST is an SSA_NAME that flows from an abnormal edge, then it
     cannot be replaced.  */
  if (TREE_CODE (dest) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (dest))
    return false;

  /* Do not copy between types for which we *do* need a conversion.  */
  if (!useless_type_conversion_p (type_d, type_o))
    return false;

  /* Propagating virtual operands is always ok.  */
  if (TREE_CODE (dest) == SSA_NAME && !is_gimple_reg (dest))
    {
      /* But only between virtual operands.  */
      gcc_assert (TREE_CODE (orig) == SSA_NAME && !is_gimple_reg (orig));

      return true;
    }

  /* Anything else is OK.  */
  return true;
}

/* Like may_propagate_copy, but use as the destination expression
   the principal expression (typically, the RHS) contained in
   statement DEST.  This is more efficient when working with the
   gimple tuples representation.  */

bool
may_propagate_copy_into_stmt (gimple dest, tree orig)
{
  tree type_d;
  tree type_o;

  gcc_unreachable ();

  /* If the statement is a switch or a single-rhs assignment,
     then the expression to be replaced by the propagation may
     be an SSA_NAME.  Fortunately, there is an explicit tree
     for the expression, so we delegate to may_propagate_copy.  */

  if (gimple_assign_single_p (dest))
    return may_propagate_copy (gimple_assign_rhs1 (dest), orig);
  else if (gimple_code (dest) == GIMPLE_SWITCH)
    return may_propagate_copy (gimple_switch_index (dest), orig);

  /* In other cases, the expression is not materialized, so there
     is no destination to pass to may_propagate_copy.  On the other
     hand, the expression cannot be an SSA_NAME, so the analysis
     is much simpler.  */

  if (TREE_CODE (orig) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (orig))
    return false;

  if (is_gimple_assign (dest))
    type_d = TREE_TYPE (gimple_assign_lhs (dest));
  else if (gimple_code (dest) == GIMPLE_COND)
    type_d = boolean_type_node;
  else if (is_gimple_call (dest)
           && gimple_call_lhs (dest) != NULL_TREE)
    type_d = TREE_TYPE (gimple_call_lhs (dest));
  else
    gcc_unreachable ();

  type_o = TREE_TYPE (orig);

  if (!useless_type_conversion_p (type_d, type_o))
    return false;

  return true;
}

/* Similarly, but we know that we're propagating into an ASM_EXPR.  */

bool
may_propagate_copy_into_asm (tree dest)
{
  gcc_unreachable ();
  /* Hard register operands of asms are special.  Do not bypass.  */
  return !(TREE_CODE (dest) == SSA_NAME
	   && TREE_CODE (SSA_NAME_VAR (dest)) == VAR_DECL
	   && DECL_HARD_REGISTER (SSA_NAME_VAR (dest)));
}


/* Common code for propagate_value and replace_exp.

   Replace use operand OP_P with VAL.  FOR_PROPAGATION indicates if the
   replacement is done to propagate a value or not.  */

static void
replace_exp_1 (use_operand_p op_p, tree val,
    	       bool for_propagation ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}


/* Propagate the value VAL (assumed to be a constant or another SSA_NAME)
   into the operand pointed to by OP_P.

   Use this version for const/copy propagation as it will perform additional
   checks to ensure validity of the const/copy propagation.  */

void
propagate_value (use_operand_p op_p, tree val)
{
  gcc_unreachable ();
}

/* Replace *OP_P with value VAL (assumed to be a constant or another SSA_NAME).

   Use this version when not const/copy propagating values.  For example,
   PRE uses this version when building expressions as they would appear
   in specific blocks taking into account actions of PHI nodes.

   The statement in which an expression has been replaced should be
   folded using fold_stmt_inplace.  */

void
replace_exp (use_operand_p op_p, tree val)
{
  gcc_unreachable ();
  replace_exp_1 (op_p, val, false);
}

EXTERN_C_END
