/* Modula-3: modified */

/* Induction variable optimizations.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
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

/* This pass tries to find the optimal set of induction variables for the loop.
   It optimizes just the basic linear induction variables (although adding
   support for other types should not be too hard).  It includes the
   optimizations commonly known as strength reduction, induction variable
   coalescing and induction variable elimination.  It does it in the
   following steps:

   1) The interesting uses of induction variables are found.  This includes

      -- uses of induction variables in non-linear expressions
      -- addresses of arrays
      -- comparisons of induction variables

   2) Candidates for the induction variables are found.  This includes

      -- old induction variables
      -- the variables defined by expressions derived from the "interesting
	 uses" above

   3) The optimal (w.r. to a cost function) set of variables is chosen.  The
      cost function assigns a cost to sets of induction variables and consists
      of three parts:

      -- The use costs.  Each of the interesting uses chooses the best induction
	 variable in the set and adds its cost to the sum.  The cost reflects
	 the time spent on modifying the induction variables value to be usable
	 for the given purpose (adding base and offset for arrays, etc.).
      -- The variable costs.  Each of the variables has a cost assigned that
	 reflects the costs associated with incrementing the value of the
	 variable.  The original variables are somewhat preferred.
      -- The set cost.  Depending on the size of the set, extra cost may be
	 added to reflect register pressure.

      All the costs are defined in a machine-specific way, using the target
      hooks and machine descriptions to determine them.

   4) The trees are transformed to use the new variables, the dead code is
      removed.

   All of this is done loop by loop.  Doing it globally is theoretically
   possible, it might give a better performance and it might enable us
   to decide costs more precisely, but getting all the interactions right
   would be complicated.  */

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
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "tree-pass.h"
#include "ggc.h"
#include "insn-config.h"
#include "recog.h"
#include "pointer-set.h"
#include "hashtab.h"
#include "tree-chrec.h"
#include "tree-scalar-evolution.h"
#include "cfgloop.h"
#include "params.h"
#include "langhooks.h"
#include "tree-affine.h"
#include "target.h"
#include "tree-inline.h"
#include "tree-ssa-propagate.h"

/* FIXME: Expressions are expanded to RTL in this pass to determine the
   cost of different addressing modes.  This should be moved to a TBD
   interface between the GIMPLE and RTL worlds.  */
#include "expr.h"

EXTERN_C_START

/* The infinite cost.  */
#define INFTY 10000000

#define AVG_LOOP_NITER(LOOP) 5

/* Returns the expected number of loop iterations for LOOP.
   The average trip count is computed from profile data if it
   exists. */

static inline HOST_WIDE_INT
avg_loop_niter (struct loop *loop)
{
  gcc_unreachable ();
  return 0;
}

/* Representation of the induction variable.  */
struct iv
{
  tree base;		/* Initial value of the iv.  */
  tree base_object;	/* A memory object to that the induction variable points.  */
  tree step;		/* Step of the iv (constant only).  */
  tree ssa_name;	/* The ssa name with the value.  */
  bool biv_p;		/* Is it a biv?  */
  bool have_use_for;	/* Do we already have a use for it?  */
  unsigned use_id;	/* The identifier in the use if it is the case.  */
};

/* Per-ssa version information (induction variable descriptions, etc.).  */
struct version_info
{
  tree name;		/* The ssa name.  */
  struct iv *iv;	/* Induction variable description.  */
  bool has_nonlin_use;	/* For a loop-level invariant, whether it is used in
			   an expression that is not an induction variable.  */
  bool preserve_biv;	/* For the original biv, whether to preserve it.  */
  unsigned inv_id;	/* Id of an invariant.  */
};

/* Types of uses.  */
enum use_type
{
  USE_NONLINEAR_EXPR,	/* Use in a nonlinear expression.  */
  USE_ADDRESS,		/* Use in an address.  */
  USE_COMPARE		/* Use is a compare.  */
};

/* Cost of a computation.  */
typedef struct
{
  int cost;		/* The runtime cost.  */
  unsigned complexity;	/* The estimate of the complexity of the code for
			   the computation (in no concrete units --
			   complexity field should be larger for more
			   complex expressions and addressing modes).  */
} comp_cost;

static const comp_cost zero_cost = {0, 0};
static const comp_cost infinite_cost = {INFTY, INFTY};

/* The candidate - cost pair.  */
struct cost_pair
{
  struct iv_cand *cand;	/* The candidate.  */
  comp_cost cost;	/* The cost.  */
  bitmap depends_on;	/* The list of invariants that have to be
			   preserved.  */
  tree value;		/* For final value elimination, the expression for
			   the final value of the iv.  For iv elimination,
			   the new bound to compare with.  */
  int inv_expr_id;      /* Loop invariant expression id.  */
};

/* Use.  */
struct iv_use
{
  unsigned id;		/* The id of the use.  */
  enum use_type type;	/* Type of the use.  */
  struct iv *iv;	/* The induction variable it is based on.  */
  gimple stmt;		/* Statement in that it occurs.  */
  tree *op_p;		/* The place where it occurs.  */
  bitmap related_cands;	/* The set of "related" iv candidates, plus the common
			   important ones.  */

  unsigned n_map_members; /* Number of candidates in the cost_map list.  */
  struct cost_pair *cost_map;
			/* The costs wrto the iv candidates.  */

  struct iv_cand *selected;
			/* The selected candidate.  */
};

/* The position where the iv is computed.  */
enum iv_position
{
  IP_NORMAL,		/* At the end, just before the exit condition.  */
  IP_END,		/* At the end of the latch block.  */
  IP_BEFORE_USE,	/* Immediately before a specific use.  */
  IP_AFTER_USE,		/* Immediately after a specific use.  */
  IP_ORIGINAL		/* The original biv.  */
};

/* The induction variable candidate.  */
struct iv_cand
{
  unsigned id;		/* The number of the candidate.  */
  bool important;	/* Whether this is an "important" candidate, i.e. such
			   that it should be considered by all uses.  */
  ENUM_BITFIELD(iv_position) pos : 8;	/* Where it is computed.  */
  gimple incremented_at;/* For original biv, the statement where it is
			   incremented.  */
  tree var_before;	/* The variable used for it before increment.  */
  tree var_after;	/* The variable used for it after increment.  */
  struct iv *iv;	/* The value of the candidate.  NULL for
			   "pseudocandidate" used to indicate the possibility
			   to replace the final value of an iv by direct
			   computation of the value.  */
  unsigned cost;	/* Cost of the candidate.  */
  unsigned cost_step;	/* Cost of the candidate's increment operation.  */
  struct iv_use *ainc_use; /* For IP_{BEFORE,AFTER}_USE candidates, the place
			      where it is incremented.  */
  bitmap depends_on;	/* The list of invariants that are used in step of the
			   biv.  */
};

/* Loop invariant expression hashtable entry.  */
struct iv_inv_expr_ent
{
  tree expr;
  int id;
  hashval_t hash;
};

/* The data used by the induction variable optimizations.  */

typedef struct iv_use *iv_use_p;
DEF_VEC_P(iv_use_p);
DEF_VEC_ALLOC_P(iv_use_p,heap);

typedef struct iv_cand *iv_cand_p;
DEF_VEC_P(iv_cand_p);
DEF_VEC_ALLOC_P(iv_cand_p,heap);

struct ivopts_data
{
  /* The currently optimized loop.  */
  struct loop *current_loop;

  /* Numbers of iterations for all exits of the current loop.  */
  struct pointer_map_t *niters;

  /* Number of registers used in it.  */
  unsigned regs_used;

  /* The size of version_info array allocated.  */
  unsigned version_info_size;

  /* The array of information for the ssa names.  */
  struct version_info *version_info;

  /* The hashtable of loop invariant expressions created
     by ivopt.  */
  htab_t inv_expr_tab;

  /* Loop invariant expression id.  */
  int inv_expr_id;

  /* The bitmap of indices in version_info whose value was changed.  */
  bitmap relevant;

  /* The uses of induction variables.  */
  VEC(iv_use_p,heap) *iv_uses;

  /* The candidates.  */
  VEC(iv_cand_p,heap) *iv_candidates;

  /* A bitmap of important candidates.  */
  bitmap important_candidates;

  /* The maximum invariant id.  */
  unsigned max_inv_id;

  /* Whether to consider just related and important candidates when replacing a
     use.  */
  bool consider_all_candidates;

  /* Are we optimizing for speed?  */
  bool speed;

  /* Whether the loop body includes any function calls.  */
  bool body_includes_call;
};

/* An assignment of iv candidates to uses.  */

struct iv_ca
{
  /* The number of uses covered by the assignment.  */
  unsigned upto;

  /* Number of uses that cannot be expressed by the candidates in the set.  */
  unsigned bad_uses;

  /* Candidate assigned to a use, together with the related costs.  */
  struct cost_pair **cand_for_use;

  /* Number of times each candidate is used.  */
  unsigned *n_cand_uses;

  /* The candidates used.  */
  bitmap cands;

  /* The number of candidates in the set.  */
  unsigned n_cands;

  /* Total number of registers needed.  */
  unsigned n_regs;

  /* Total cost of expressing uses.  */
  comp_cost cand_use_cost;

  /* Total cost of candidates.  */
  unsigned cand_cost;

  /* Number of times each invariant is used.  */
  unsigned *n_invariant_uses;

  /* The array holding the number of uses of each loop
     invariant expressions created by ivopt.  */
  unsigned *used_inv_expr;

  /* The number of created loop invariants.  */
  unsigned num_used_inv_expr;

  /* Total cost of the assignment.  */
  comp_cost cost;
};

/* Difference of two iv candidate assignments.  */

struct iv_ca_delta
{
  /* Changed use.  */
  struct iv_use *use;

  /* An old assignment (for rollback purposes).  */
  struct cost_pair *old_cp;

  /* A new assignment.  */
  struct cost_pair *new_cp;

  /* Next change in the list.  */
  struct iv_ca_delta *next_change;
};

/* Bound on number of candidates below that all candidates are considered.  */

#define CONSIDER_ALL_CANDIDATES_BOUND \
  ((unsigned) PARAM_VALUE (PARAM_IV_CONSIDER_ALL_CANDIDATES_BOUND))

/* If there are more iv occurrences, we just give up (it is quite unlikely that
   optimizing such a loop would help, and it would take ages).  */

#define MAX_CONSIDERED_USES \
  ((unsigned) PARAM_VALUE (PARAM_IV_MAX_CONSIDERED_USES))

/* If there are at most this number of ivs in the set, try removing unnecessary
   ivs from the set always.  */

#define ALWAYS_PRUNE_CAND_SET_BOUND \
  ((unsigned) PARAM_VALUE (PARAM_IV_ALWAYS_PRUNE_CAND_SET_BOUND))

/* The list of trees for that the decl_rtl field must be reset is stored
   here.  */

static VEC(tree,heap) *decl_rtl_to_reset;

/* Number of uses recorded in DATA.  */

static inline unsigned
n_iv_uses (struct ivopts_data *data)
{
  return VEC_length (iv_use_p, data->iv_uses);
}

/* Ith use recorded in DATA.  */

static inline struct iv_use *
iv_use (struct ivopts_data *data, unsigned i)
{
  return VEC_index (iv_use_p, data->iv_uses, i);
}

/* Number of candidates recorded in DATA.  */

static inline unsigned
n_iv_cands (struct ivopts_data *data)
{
  return VEC_length (iv_cand_p, data->iv_candidates);
}

/* Ith candidate recorded in DATA.  */

static inline struct iv_cand *
iv_cand (struct ivopts_data *data, unsigned i)
{
  return VEC_index (iv_cand_p, data->iv_candidates, i);
}

/* The single loop exit if it dominates the latch, NULL otherwise.  */

edge
single_dom_exit (struct loop *loop)
{
  edge exit = single_exit (loop);

  if (!exit)
    return NULL;

  if (!just_once_each_iteration_p (loop, exit->src))
    return NULL;

  return exit;
}

/* Dumps information about the induction variable IV to FILE.  */

extern void dump_iv (FILE *, struct iv *);
void
dump_iv (FILE *file, struct iv *iv)
{
  if (iv->ssa_name)
    {
      fprintf (file, "ssa name ");
      print_generic_expr (file, iv->ssa_name, TDF_SLIM);
      fprintf (file, "\n");
    }

  fprintf (file, "  type ");
  print_generic_expr (file, TREE_TYPE (iv->base), TDF_SLIM);
  fprintf (file, "\n");

  if (iv->step)
    {
      fprintf (file, "  base ");
      print_generic_expr (file, iv->base, TDF_SLIM);
      fprintf (file, "\n");

      fprintf (file, "  step ");
      print_generic_expr (file, iv->step, TDF_SLIM);
      fprintf (file, "\n");
    }
  else
    {
      fprintf (file, "  invariant ");
      print_generic_expr (file, iv->base, TDF_SLIM);
      fprintf (file, "\n");
    }

  if (iv->base_object)
    {
      fprintf (file, "  base object ");
      print_generic_expr (file, iv->base_object, TDF_SLIM);
      fprintf (file, "\n");
    }

  if (iv->biv_p)
    fprintf (file, "  is a biv\n");
}

/* Dumps information about the USE to FILE.  */

extern void dump_use (FILE *, struct iv_use *);
void
dump_use (FILE *file, struct iv_use *use)
{
  fprintf (file, "use %d\n", use->id);

  switch (use->type)
    {
    case USE_NONLINEAR_EXPR:
      fprintf (file, "  generic\n");
      break;

    case USE_ADDRESS:
      fprintf (file, "  address\n");
      break;

    case USE_COMPARE:
      fprintf (file, "  compare\n");
      break;

    default:
      gcc_unreachable ();
    }

  fprintf (file, "  in statement ");
  print_gimple_stmt (file, use->stmt, 0, 0);
  fprintf (file, "\n");

  fprintf (file, "  at position ");
  if (use->op_p)
    print_generic_expr (file, *use->op_p, TDF_SLIM);
  fprintf (file, "\n");

  dump_iv (file, use->iv);

  if (use->related_cands)
    {
      fprintf (file, "  related candidates ");
      dump_bitmap (file, use->related_cands);
    }
}

/* Dumps information about the uses to FILE.  */

extern void dump_uses (FILE *, struct ivopts_data *);
void
dump_uses (FILE *file, struct ivopts_data *data)
{
  unsigned i;
  struct iv_use *use;

  for (i = 0; i < n_iv_uses (data); i++)
    {
      use = iv_use (data, i);

      dump_use (file, use);
      fprintf (file, "\n");
    }
}

/* Dumps information about induction variable candidate CAND to FILE.  */

extern void dump_cand (FILE *, struct iv_cand *);
void
dump_cand (FILE *file, struct iv_cand *cand)
{
  struct iv *iv = cand->iv;

  fprintf (file, "candidate %d%s\n",
	   cand->id, cand->important ? " (important)" : "");

  if (cand->depends_on)
    {
      fprintf (file, "  depends on ");
      dump_bitmap (file, cand->depends_on);
    }

  if (!iv)
    {
      fprintf (file, "  final value replacement\n");
      return;
    }

  if (cand->var_before)
    {
      fprintf (file, "  var_before ");
      print_generic_expr (file, cand->var_before, TDF_SLIM);
      fprintf (file, "\n");
    }
  if (cand->var_after)
    {
      fprintf (file, "  var_after ");
      print_generic_expr (file, cand->var_after, TDF_SLIM);
      fprintf (file, "\n");
    }

  switch (cand->pos)
    {
    case IP_NORMAL:
      fprintf (file, "  incremented before exit test\n");
      break;

    case IP_BEFORE_USE:
      fprintf (file, "  incremented before use %d\n", cand->ainc_use->id);
      break;

    case IP_AFTER_USE:
      fprintf (file, "  incremented after use %d\n", cand->ainc_use->id);
      break;

    case IP_END:
      fprintf (file, "  incremented at end\n");
      break;

    case IP_ORIGINAL:
      fprintf (file, "  original biv\n");
      break;
    }

  dump_iv (file, iv);
}

/* Returns the info for ssa version VER.  */

static inline struct version_info *
ver_info (struct ivopts_data *data, unsigned ver)
{
  return data->version_info + ver;
}

/* Returns the info for ssa name NAME.  */

static inline struct version_info *
name_info (struct ivopts_data *data, tree name)
{
  return ver_info (data, SSA_NAME_VERSION (name));
}

/* Returns true if STMT is after the place where the IP_NORMAL ivs will be
   emitted in LOOP.  */

static bool
stmt_after_ip_normal_pos (struct loop *loop, gimple stmt)
{
  basic_block bb = ip_normal_pos (loop), sbb = gimple_bb (stmt);

  gcc_assert (bb);

  if (sbb == loop->latch)
    return true;

  if (sbb != bb)
    return false;

  return stmt == last_stmt (bb);
}

/* Returns true if STMT if after the place where the original induction
   variable CAND is incremented.  If TRUE_IF_EQUAL is set, we return true
   if the positions are identical.  */

static bool
stmt_after_inc_pos (struct iv_cand *cand, gimple stmt, bool true_if_equal)
{
  basic_block cand_bb = gimple_bb (cand->incremented_at);
  basic_block stmt_bb = gimple_bb (stmt);

  if (!dominated_by_p (CDI_DOMINATORS, stmt_bb, cand_bb))
    return false;

  if (stmt_bb != cand_bb)
    return true;

  if (true_if_equal
      && gimple_uid (stmt) == gimple_uid (cand->incremented_at))
    return true;
  return gimple_uid (stmt) > gimple_uid (cand->incremented_at);
}

/* Returns true if STMT if after the place where the induction variable
   CAND is incremented in LOOP.  */

static bool
stmt_after_increment (struct loop *loop, struct iv_cand *cand, gimple stmt)
{
  switch (cand->pos)
    {
    case IP_END:
      return false;

    case IP_NORMAL:
      return stmt_after_ip_normal_pos (loop, stmt);

    case IP_ORIGINAL:
    case IP_AFTER_USE:
      return stmt_after_inc_pos (cand, stmt, false);

    case IP_BEFORE_USE:
      return stmt_after_inc_pos (cand, stmt, true);

    default:
      gcc_unreachable ();
    }
}

/* Returns true if EXP is a ssa name that occurs in an abnormal phi node.  */

static bool
abnormal_ssa_name_p (tree exp)
{
  if (!exp)
    return false;

  if (TREE_CODE (exp) != SSA_NAME)
    return false;

  return SSA_NAME_OCCURS_IN_ABNORMAL_PHI (exp) != 0;
}

/* Returns true if EXPR contains a ssa name that occurs in an
   abnormal phi node.  */

bool
contains_abnormal_ssa_name_p (tree expr)
{
  gcc_unreachable ();  
  return true;
}

/* Hash table equality function for expressions.  */

static int
htab_inv_expr_eq (const void *ent1, const void *ent2)
{
  const struct iv_inv_expr_ent *expr1 =
      (const struct iv_inv_expr_ent *)ent1;
  const struct iv_inv_expr_ent *expr2 =
      (const struct iv_inv_expr_ent *)ent2;

  return expr1->hash == expr2->hash
	 && operand_equal_p (expr1->expr, expr2->expr, 0);
}

/* Hash function for loop invariant expressions.  */

static hashval_t
htab_inv_expr_hash (const void *ent)
{
  const struct iv_inv_expr_ent *expr =
      (const struct iv_inv_expr_ent *)ent;
  return expr->hash;
}


/* Returns a memory object to that EXPR points.  In case we are able to
   determine that it does not point to any such object, NULL is returned.  */

static tree
determine_base_object (tree expr)
{
  enum tree_code code = TREE_CODE (expr);
  tree base, obj;

  /* If this is a pointer casted to any type, we need to determine
     the base object for the pointer; so handle conversions before
     throwing away non-pointer expressions.  */
  if (CONVERT_EXPR_P (expr))
    return determine_base_object (TREE_OPERAND (expr, 0));

  if (!POINTER_TYPE_P (TREE_TYPE (expr)))
    return NULL_TREE;

  switch (code)
    {
    case INTEGER_CST:
      return NULL_TREE;

    case ADDR_EXPR:
      obj = TREE_OPERAND (expr, 0);
      base = get_base_address (obj);

      if (!base)
	return expr;

      if (TREE_CODE (base) == MEM_REF)
	return determine_base_object (TREE_OPERAND (base, 0));

      return fold_convert (ptr_type_node,
		           build_fold_addr_expr (base));

    case POINTER_PLUS_EXPR:
      return determine_base_object (TREE_OPERAND (expr, 0));

    case PLUS_EXPR:
    case MINUS_EXPR:
      /* Pointer addition is done solely using POINTER_PLUS_EXPR.  */
      gcc_unreachable ();

    default:
      return fold_convert (ptr_type_node, expr);
    }
}

/* Allocates an induction variable with given initial value BASE and step STEP
   for loop LOOP.  */

static struct iv *
alloc_iv (tree base, tree step)
{
  struct iv *iv = XCNEW (struct iv);
  gcc_assert (step != NULL_TREE);

  iv->base = base;
  iv->base_object = determine_base_object (base);
  iv->step = step;
  iv->biv_p = false;
  iv->have_use_for = false;
  iv->use_id = 0;
  iv->ssa_name = NULL_TREE;

  return iv;
}

/* Sets STEP and BASE for induction variable IV.  */

static void
set_iv (struct ivopts_data *data, tree iv, tree base, tree step)
{
  struct version_info *info = name_info (data, iv);

  gcc_assert (!info->iv);

  bitmap_set_bit (data->relevant, SSA_NAME_VERSION (iv));
  info->iv = alloc_iv (base, step);
  info->iv->ssa_name = iv;
}

/* Finds induction variable declaration for VAR.  */

static struct iv *
get_iv (struct ivopts_data *data, tree var)
{
  basic_block bb;
  tree type = TREE_TYPE (var);

  if (!POINTER_TYPE_P (type)
      && !INTEGRAL_TYPE_P (type))
    return NULL;

  if (!name_info (data, var)->iv)
    {
      bb = gimple_bb (SSA_NAME_DEF_STMT (var));

      if (!bb
	  || !flow_bb_inside_loop_p (data->current_loop, bb))
	set_iv (data, var, var, build_int_cst (type, 0));
    }

  return name_info (data, var)->iv;
}

/* Determines the step of a biv defined in PHI.  Returns NULL if PHI does
   not define a simple affine biv with nonzero step.  */

static tree
determine_biv_step (gimple phi)
{
  struct loop *loop = gimple_bb (phi)->loop_father;
  tree name = PHI_RESULT (phi);
  affine_iv iv;

  if (!is_gimple_reg (name))
    return NULL_TREE;

  if (!simple_iv (loop, loop, name, &iv, true))
    return NULL_TREE;

  return integer_zerop (iv.step) ? NULL_TREE : iv.step;
}

/* Finds basic ivs.  */

static bool
find_bivs (struct ivopts_data *data)
{
  gimple phi;
  tree step, type, base;
  bool found = false;
  struct loop *loop = data->current_loop;
  gimple_stmt_iterator psi;

  for (psi = gsi_start_phis (loop->header); !gsi_end_p (psi); gsi_next (&psi))
    {
      phi = gsi_stmt (psi);

      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (PHI_RESULT (phi)))
	continue;

      step = determine_biv_step (phi);
      if (!step)
	continue;

      base = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (loop));
      base = expand_simple_operations (base);
      if (contains_abnormal_ssa_name_p (base)
	  || contains_abnormal_ssa_name_p (step))
	continue;

      type = TREE_TYPE (PHI_RESULT (phi));
      base = fold_convert (type, base);
      if (step)
	{
	  if (POINTER_TYPE_P (type))
	    step = fold_convert (sizetype, step);
	  else
	    step = fold_convert (type, step);
	}

      set_iv (data, PHI_RESULT (phi), base, step);
      found = true;
    }

  return found;
}

/* Marks basic ivs.  */

static void
mark_bivs (struct ivopts_data *data)
{
  gimple phi;
  tree var;
  struct iv *iv, *incr_iv;
  struct loop *loop = data->current_loop;
  basic_block incr_bb;
  gimple_stmt_iterator psi;

  for (psi = gsi_start_phis (loop->header); !gsi_end_p (psi); gsi_next (&psi))
    {
      phi = gsi_stmt (psi);

      iv = get_iv (data, PHI_RESULT (phi));
      if (!iv)
	continue;

      var = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (loop));
      incr_iv = get_iv (data, var);
      if (!incr_iv)
	continue;

      /* If the increment is in the subloop, ignore it.  */
      incr_bb = gimple_bb (SSA_NAME_DEF_STMT (var));
      if (incr_bb->loop_father != data->current_loop
	  || (incr_bb->flags & BB_IRREDUCIBLE_LOOP))
	continue;

      iv->biv_p = true;
      incr_iv->biv_p = true;
    }
}

/* Checks whether STMT defines a linear induction variable and stores its
   parameters to IV.  */

static bool
find_givs_in_stmt_scev (struct ivopts_data *data, gimple stmt, affine_iv *iv)
{
  tree lhs;
  struct loop *loop = data->current_loop;

  iv->base = NULL_TREE;
  iv->step = NULL_TREE;

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return false;

  lhs = gimple_assign_lhs (stmt);
  if (TREE_CODE (lhs) != SSA_NAME)
    return false;

  if (!simple_iv (loop, loop_containing_stmt (stmt), lhs, iv, true))
    return false;
  iv->base = expand_simple_operations (iv->base);

  if (contains_abnormal_ssa_name_p (iv->base)
      || contains_abnormal_ssa_name_p (iv->step))
    return false;

  /* If STMT could throw, then do not consider STMT as defining a GIV.  
     While this will suppress optimizations, we can not safely delete this
     GIV and associated statements, even if it appears it is not used.  */
  if (stmt_could_throw_p (stmt))
    return false;

  return true;
}

/* Finds general ivs in statement STMT.  */

static void
find_givs_in_stmt (struct ivopts_data *data, gimple stmt)
{
  affine_iv iv;

  if (!find_givs_in_stmt_scev (data, stmt, &iv))
    return;

  set_iv (data, gimple_assign_lhs (stmt), iv.base, iv.step);
}

/* Finds general ivs in basic block BB.  */

static void
find_givs_in_bb (struct ivopts_data *data, basic_block bb)
{
  gimple_stmt_iterator bsi;

  for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
    find_givs_in_stmt (data, gsi_stmt (bsi));
}

/* Finds general ivs.  */

static void
find_givs (struct ivopts_data *data)
{
  struct loop *loop = data->current_loop;
  basic_block *body = get_loop_body_in_dom_order (loop);
  unsigned i;

  for (i = 0; i < loop->num_nodes; i++)
    find_givs_in_bb (data, body[i]);
  free (body);
}

/* Records a use of type USE_TYPE at *USE_P in STMT whose value is IV.  */

static struct iv_use *
record_use (struct ivopts_data *data, tree *use_p, struct iv *iv,
	    gimple stmt, enum use_type use_type)
{
  struct iv_use *use = XCNEW (struct iv_use);

  use->id = n_iv_uses (data);
  use->type = use_type;
  use->iv = iv;
  use->stmt = stmt;
  use->op_p = use_p;
  use->related_cands = BITMAP_ALLOC (NULL);

  /* To avoid showing ssa name in the dumps, if it was not reset by the
     caller.  */
  iv->ssa_name = NULL_TREE;

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_use (dump_file, use);

  VEC_safe_push (iv_use_p, heap, data->iv_uses, use);

  return use;
}

/* Checks whether OP is a loop-level invariant and if so, records it.
   NONLINEAR_USE is true if the invariant is used in a way we do not
   handle specially.  */

static void
record_invariant (struct ivopts_data *data, tree op, bool nonlinear_use)
{
  basic_block bb;
  struct version_info *info;

  if (TREE_CODE (op) != SSA_NAME
      || !is_gimple_reg (op))
    return;

  bb = gimple_bb (SSA_NAME_DEF_STMT (op));
  if (bb
      && flow_bb_inside_loop_p (data->current_loop, bb))
    return;

  info = name_info (data, op);
  info->name = op;
  info->has_nonlin_use |= nonlinear_use;
  if (!info->inv_id)
    info->inv_id = ++data->max_inv_id;
  bitmap_set_bit (data->relevant, SSA_NAME_VERSION (op));
}

/* Checks whether the use OP is interesting and if so, records it.  */

static struct iv_use *
find_interesting_uses_op (struct ivopts_data *data, tree op)
{
  struct iv *iv;
  struct iv *civ;
  gimple stmt;
  struct iv_use *use;

  if (TREE_CODE (op) != SSA_NAME)
    return NULL;

  iv = get_iv (data, op);
  if (!iv)
    return NULL;

  if (iv->have_use_for)
    {
      use = iv_use (data, iv->use_id);

      gcc_assert (use->type == USE_NONLINEAR_EXPR);
      return use;
    }

  if (integer_zerop (iv->step))
    {
      record_invariant (data, op, true);
      return NULL;
    }
  iv->have_use_for = true;

  civ = XNEW (struct iv);
  *civ = *iv;

  stmt = SSA_NAME_DEF_STMT (op);
  gcc_assert (gimple_code (stmt) == GIMPLE_PHI
	      || is_gimple_assign (stmt));

  use = record_use (data, NULL, civ, stmt, USE_NONLINEAR_EXPR);
  iv->use_id = use->id;

  return use;
}

/* Given a condition in statement STMT, checks whether it is a compare
   of an induction variable and an invariant.  If this is the case,
   CONTROL_VAR is set to location of the iv, BOUND to the location of
   the invariant, IV_VAR and IV_BOUND are set to the corresponding
   induction variable descriptions, and true is returned.  If this is not
   the case, CONTROL_VAR and BOUND are set to the arguments of the
   condition and false is returned.  */

static bool
extract_cond_operands (struct ivopts_data *data, gimple stmt,
		       tree **control_var, tree **bound,
		       struct iv **iv_var, struct iv **iv_bound)
{
  /* The objects returned when COND has constant operands.  */
  static struct iv const_iv;
  static tree zero;
  tree *op0 = &zero, *op1 = &zero, *tmp_op;
  struct iv *iv0 = &const_iv, *iv1 = &const_iv, *tmp_iv;
  bool ret = false;

  if (gimple_code (stmt) == GIMPLE_COND)
    {
      op0 = gimple_cond_lhs_ptr (stmt);
      op1 = gimple_cond_rhs_ptr (stmt);
    }
  else
    {
      op0 = gimple_assign_rhs1_ptr (stmt);
      op1 = gimple_assign_rhs2_ptr (stmt);
    }

  zero = integer_zero_node;
  const_iv.step = integer_zero_node;

  if (TREE_CODE (*op0) == SSA_NAME)
    iv0 = get_iv (data, *op0);
  if (TREE_CODE (*op1) == SSA_NAME)
    iv1 = get_iv (data, *op1);

  /* Exactly one of the compared values must be an iv, and the other one must
     be an invariant.  */
  if (!iv0 || !iv1)
    goto end;

  if (integer_zerop (iv0->step))
    {
      /* Control variable may be on the other side.  */
      tmp_op = op0; op0 = op1; op1 = tmp_op;
      tmp_iv = iv0; iv0 = iv1; iv1 = tmp_iv;
    }
  ret = !integer_zerop (iv0->step) && integer_zerop (iv1->step);

end:
  if (control_var)
    *control_var = op0;;
  if (iv_var)
    *iv_var = iv0;;
  if (bound)
    *bound = op1;
  if (iv_bound)
    *iv_bound = iv1;

  return ret;
}

/* Checks whether the condition in STMT is interesting and if so,
   records it.  */

static void
find_interesting_uses_cond (struct ivopts_data *data, gimple stmt)
{
  tree *var_p, *bound_p;
  struct iv *var_iv, *civ;

  if (!extract_cond_operands (data, stmt, &var_p, &bound_p, &var_iv, NULL))
    {
      find_interesting_uses_op (data, *var_p);
      find_interesting_uses_op (data, *bound_p);
      return;
    }

  civ = XNEW (struct iv);
  *civ = *var_iv;
  record_use (data, NULL, civ, stmt, USE_COMPARE);
}

/* Returns true if expression EXPR is obviously invariant in LOOP,
   i.e. if all its operands are defined outside of the LOOP.  LOOP
   should not be the function body.  */

bool
expr_invariant_in_loop_p (struct loop *loop, tree expr)
{
  basic_block def_bb;
  unsigned i, len;

  gcc_assert (loop_depth (loop) > 0);

  if (is_gimple_min_invariant (expr))
    return true;

  if (TREE_CODE (expr) == SSA_NAME)
    {
      def_bb = gimple_bb (SSA_NAME_DEF_STMT (expr));
      if (def_bb
	  && flow_bb_inside_loop_p (loop, def_bb))
	return false;

      return true;
    }

  if (!EXPR_P (expr))
    return false;

  len = TREE_OPERAND_LENGTH (expr);
  for (i = 0; i < len; i++)
    if (!expr_invariant_in_loop_p (loop, TREE_OPERAND (expr, i)))
      return false;

  return true;
}

/* Returns true if statement STMT is obviously invariant in LOOP,
   i.e. if all its operands on the RHS are defined outside of the LOOP.
   LOOP should not be the function body.  */

bool
stmt_invariant_in_loop_p (struct loop *loop, gimple stmt)
{
  unsigned i;
  tree lhs;

  gcc_assert (loop_depth (loop) > 0);

  lhs = gimple_get_lhs (stmt);
  for (i = 0; i < gimple_num_ops (stmt); i++)
    {
      tree op = gimple_op (stmt, i);
      if (op != lhs && !expr_invariant_in_loop_p (loop, op))
	return false;
    }

  return true;
}

/* Cumulates the steps of indices into DATA and replaces their values with the
   initial ones.  Returns false when the value of the index cannot be determined.
   Callback for for_each_index.  */

struct ifs_ivopts_data
{
  struct ivopts_data *ivopts_data;
  gimple stmt;
  tree step;
};

static bool
idx_find_step (tree base, tree *idx, void *data)
{
  struct ifs_ivopts_data *dta = (struct ifs_ivopts_data *) data;
  struct iv *iv;
  tree step, iv_base, iv_step, lbound, off;
  struct loop *loop = dta->ivopts_data->current_loop;

  /* If base is a component ref, require that the offset of the reference
     be invariant.  */
  if (TREE_CODE (base) == COMPONENT_REF)
    {
      off = component_ref_field_offset (base);
      return expr_invariant_in_loop_p (loop, off);
    }

  /* If base is array, first check whether we will be able to move the
     reference out of the loop (in order to take its address in strength
     reduction).  In order for this to work we need both lower bound
     and step to be loop invariants.  */
  if (TREE_CODE (base) == ARRAY_REF || TREE_CODE (base) == ARRAY_RANGE_REF)
    {
      /* Moreover, for a range, the size needs to be invariant as well.  */
      if (TREE_CODE (base) == ARRAY_RANGE_REF
	  && !expr_invariant_in_loop_p (loop, TYPE_SIZE (TREE_TYPE (base))))
	return false;

      step = array_ref_element_size (base);
      lbound = array_ref_low_bound (base);

      if (!expr_invariant_in_loop_p (loop, step)
	  || !expr_invariant_in_loop_p (loop, lbound))
	return false;
    }

  if (TREE_CODE (*idx) != SSA_NAME)
    return true;

  iv = get_iv (dta->ivopts_data, *idx);
  if (!iv)
    return false;

  /* XXX  We produce for a base of *D42 with iv->base being &x[0]
	  *&x[0], which is not folded and does not trigger the
	  ARRAY_REF path below.  */
  *idx = iv->base;

  if (integer_zerop (iv->step))
    return true;

  if (TREE_CODE (base) == ARRAY_REF || TREE_CODE (base) == ARRAY_RANGE_REF)
    {
      step = array_ref_element_size (base);

      /* We only handle addresses whose step is an integer constant.  */
      if (TREE_CODE (step) != INTEGER_CST)
	return false;
    }
  else
    /* The step for pointer arithmetics already is 1 byte.  */
    step = size_one_node;

  iv_base = iv->base;
  iv_step = iv->step;
  if (!convert_affine_scev (dta->ivopts_data->current_loop,
			    sizetype, &iv_base, &iv_step, dta->stmt,
			    false))
    {
      /* The index might wrap.  */
      return false;
    }

  step = fold_build2 (MULT_EXPR, sizetype, step, iv_step);
  dta->step = fold_build2 (PLUS_EXPR, sizetype, dta->step, step);

  return true;
}

/* Records use in index IDX.  Callback for for_each_index.  Ivopts data
   object is passed to it in DATA.  */

static bool
idx_record_use (tree base, tree *idx,
		void *vdata)
{
  struct ivopts_data *data = (struct ivopts_data *) vdata;
  find_interesting_uses_op (data, *idx);
  if (TREE_CODE (base) == ARRAY_REF || TREE_CODE (base) == ARRAY_RANGE_REF)
    {
      find_interesting_uses_op (data, array_ref_element_size (base));
      find_interesting_uses_op (data, array_ref_low_bound (base));
    }
  return true;
}

/* If we can prove that TOP = cst * BOT for some constant cst,
   store cst to MUL and return true.  Otherwise return false.
   The returned value is always sign-extended, regardless of the
   signedness of TOP and BOT.  */

static bool
constant_multiple_of (tree top, tree bot, double_int *mul)
{
  tree mby;
  enum tree_code code;
  double_int res, p0, p1;
  unsigned precision = TYPE_PRECISION (TREE_TYPE (top));

  STRIP_NOPS (top);
  STRIP_NOPS (bot);

  if (operand_equal_p (top, bot, 0))
    {
      *mul = double_int_one;
      return true;
    }

  code = TREE_CODE (top);
  switch (code)
    {
    case MULT_EXPR:
      mby = TREE_OPERAND (top, 1);
      if (TREE_CODE (mby) != INTEGER_CST)
	return false;

      if (!constant_multiple_of (TREE_OPERAND (top, 0), bot, &res))
	return false;

      *mul = double_int_sext (double_int_mul (res, tree_to_double_int (mby)),
			      precision);
      return true;

    case PLUS_EXPR:
    case MINUS_EXPR:
      if (!constant_multiple_of (TREE_OPERAND (top, 0), bot, &p0)
	  || !constant_multiple_of (TREE_OPERAND (top, 1), bot, &p1))
	return false;

      if (code == MINUS_EXPR)
	p1 = double_int_neg (p1);
      *mul = double_int_sext (double_int_add (p0, p1), precision);
      return true;

    case INTEGER_CST:
      if (TREE_CODE (bot) != INTEGER_CST)
	return false;

      p0 = double_int_sext (tree_to_double_int (top), precision);
      p1 = double_int_sext (tree_to_double_int (bot), precision);
      if (double_int_zero_p (p1))
	return false;
      *mul = double_int_sext (double_int_sdivmod (p0, p1, FLOOR_DIV_EXPR, &res),
			      precision);
      return double_int_zero_p (res);

    default:
      return false;
    }
}

/* Returns true if memory reference REF with step STEP may be unaligned.  */

static bool
may_be_unaligned_p (tree ref, tree step)
{
  tree base;
  tree base_type;
  HOST_WIDE_INT bitsize;
  HOST_WIDE_INT bitpos;
  tree toffset;
  enum machine_mode mode;
  int unsignedp, volatilep;
  unsigned base_align;

  /* TARGET_MEM_REFs are translated directly to valid MEMs on the target,
     thus they are not misaligned.  */
  if (TREE_CODE (ref) == TARGET_MEM_REF)
    return false;

  /* The test below is basically copy of what expr.c:normal_inner_ref
     does to check whether the object must be loaded by parts when
     STRICT_ALIGNMENT is true.  */
  base = get_inner_reference (ref, &bitsize, &bitpos, &toffset, &mode,
			      &unsignedp, &volatilep, true);
  base_type = TREE_TYPE (base);
  base_align = TYPE_ALIGN (base_type);

  if (mode != BLKmode)
    {
      unsigned mode_align = GET_MODE_ALIGNMENT (mode);

      if (base_align < mode_align
	  || (bitpos % mode_align) != 0
	  || (bitpos % BITS_PER_UNIT) != 0)
	return true;

      if (toffset
	  && (highest_pow2_factor (toffset) * BITS_PER_UNIT) < mode_align)
	return true;

      if ((highest_pow2_factor (step) * BITS_PER_UNIT) < mode_align)
	return true;
    }

  return false;
}

/* Return true if EXPR may be non-addressable.   */

bool
may_be_nonaddressable_p (tree expr)
{
  switch (TREE_CODE (expr))
    {
    case TARGET_MEM_REF:
      /* TARGET_MEM_REFs are translated directly to valid MEMs on the
	 target, thus they are always addressable.  */
      return false;

    case COMPONENT_REF:
      return DECL_NONADDRESSABLE_P (TREE_OPERAND (expr, 1))
	     || may_be_nonaddressable_p (TREE_OPERAND (expr, 0));

    case VIEW_CONVERT_EXPR:
      /* This kind of view-conversions may wrap non-addressable objects
	 and make them look addressable.  After some processing the
	 non-addressability may be uncovered again, causing ADDR_EXPRs
	 of inappropriate objects to be built.  */
      if (is_gimple_reg (TREE_OPERAND (expr, 0))
	  || !is_gimple_addressable (TREE_OPERAND (expr, 0)))
	return true;

      /* ... fall through ... */

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      return may_be_nonaddressable_p (TREE_OPERAND (expr, 0));

    CASE_CONVERT:
      return true;

    default:
      break;
    }

  return false;
}

/* Finds addresses in *OP_P inside STMT.  */

static void
find_interesting_uses_address (struct ivopts_data *data, gimple stmt, tree *op_p)
{
  tree base = *op_p, step = size_zero_node;
  struct iv *civ;
  struct ifs_ivopts_data ifs_ivopts_data;

  /* Do not play with volatile memory references.  A bit too conservative,
     perhaps, but safe.  */
  if (gimple_has_volatile_ops (stmt))
    goto fail;

  /* Ignore bitfields for now.  Not really something terribly complicated
     to handle.  TODO.  */
  if (TREE_CODE (base) == BIT_FIELD_REF)
    goto fail;

  base = unshare_expr (base);

  if (TREE_CODE (base) == TARGET_MEM_REF)
    {
      gcc_unreachable ();
    }
  else
    {
      ifs_ivopts_data.ivopts_data = data;
      ifs_ivopts_data.stmt = stmt;
      ifs_ivopts_data.step = size_zero_node;
      if (!for_each_index (&base, idx_find_step, &ifs_ivopts_data)
	  || integer_zerop (ifs_ivopts_data.step))
	goto fail;
      step = ifs_ivopts_data.step;

      /* Check that the base expression is addressable.  This needs
	 to be done after substituting bases of IVs into it.  */
      if (may_be_nonaddressable_p (base))
	goto fail;

      /* Moreover, on strict alignment platforms, check that it is
	 sufficiently aligned.  */
      if (STRICT_ALIGNMENT && may_be_unaligned_p (base, step))
	goto fail;

      base = build_fold_addr_expr (base);

      /* Substituting bases of IVs into the base expression might
	 have caused folding opportunities.  */
      if (TREE_CODE (base) == ADDR_EXPR)
	{
	  tree *ref = &TREE_OPERAND (base, 0);
	  while (handled_component_p (*ref))
	    ref = &TREE_OPERAND (*ref, 0);
	  if (TREE_CODE (*ref) == MEM_REF)
	    {
	      tree tem = fold_binary (MEM_REF, TREE_TYPE (*ref),
				      TREE_OPERAND (*ref, 0),
				      TREE_OPERAND (*ref, 1));
	      if (tem)
		*ref = tem;
	    }
	}
    }

  civ = alloc_iv (base, step);
  record_use (data, op_p, civ, stmt, USE_ADDRESS);
  return;

fail:
  for_each_index (op_p, idx_record_use, data);
}

/* Finds and records invariants used in STMT.  */

static void
find_invariants_stmt (struct ivopts_data *data, gimple stmt)
{
  ssa_op_iter iter;
  use_operand_p use_p;
  tree op;

  FOR_EACH_PHI_OR_STMT_USE (use_p, stmt, iter, SSA_OP_USE)
    {
      op = USE_FROM_PTR (use_p);
      record_invariant (data, op, false);
    }
}

/* Finds interesting uses of induction variables in the statement STMT.  */

static void
find_interesting_uses_stmt (struct ivopts_data *data, gimple stmt)
{
  struct iv *iv;
  tree op, *lhs, *rhs;
  ssa_op_iter iter;
  use_operand_p use_p;
  enum tree_code code;

  find_invariants_stmt (data, stmt);

  if (gimple_code (stmt) == GIMPLE_COND)
    {
      find_interesting_uses_cond (data, stmt);
      return;
    }

  if (is_gimple_assign (stmt))
    {
      lhs = gimple_assign_lhs_ptr (stmt);
      rhs = gimple_assign_rhs1_ptr (stmt);

      if (TREE_CODE (*lhs) == SSA_NAME)
	{
	  /* If the statement defines an induction variable, the uses are not
	     interesting by themselves.  */

	  iv = get_iv (data, *lhs);

	  if (iv && !integer_zerop (iv->step))
	    return;
	}

      code = gimple_assign_rhs_code (stmt);
      if (get_gimple_rhs_class (code) == GIMPLE_SINGLE_RHS
	  && (REFERENCE_CLASS_P (*rhs)
	      || is_gimple_val (*rhs)))
	{
	  if (REFERENCE_CLASS_P (*rhs))
	    find_interesting_uses_address (data, stmt, rhs);
	  else
	    find_interesting_uses_op (data, *rhs);

	  if (REFERENCE_CLASS_P (*lhs))
	    find_interesting_uses_address (data, stmt, lhs);
	  return;
	}
      else if (TREE_CODE_CLASS (code) == tcc_comparison)
	{
	  find_interesting_uses_cond (data, stmt);
	  return;
	}

      /* TODO -- we should also handle address uses of type

	 memory = call (whatever);

	 and

	 call (memory).  */
    }

  if (gimple_code (stmt) == GIMPLE_PHI
      && gimple_bb (stmt) == data->current_loop->header)
    {
      iv = get_iv (data, PHI_RESULT (stmt));

      if (iv && !integer_zerop (iv->step))
	return;
    }

  FOR_EACH_PHI_OR_STMT_USE (use_p, stmt, iter, SSA_OP_USE)
    {
      op = USE_FROM_PTR (use_p);

      if (TREE_CODE (op) != SSA_NAME)
	continue;

      iv = get_iv (data, op);
      if (!iv)
	continue;

      find_interesting_uses_op (data, op);
    }
}

/* Finds interesting uses of induction variables outside of loops
   on loop exit edge EXIT.  */

static void
find_interesting_uses_outside (struct ivopts_data *data, edge exit)
{
  gimple phi;
  gimple_stmt_iterator psi;
  tree def;

  for (psi = gsi_start_phis (exit->dest); !gsi_end_p (psi); gsi_next (&psi))
    {
      phi = gsi_stmt (psi);
      def = PHI_ARG_DEF_FROM_EDGE (phi, exit);
      if (is_gimple_reg (def))
        find_interesting_uses_op (data, def);
    }
}

/* Finds uses of the induction variables that are interesting.  */

static void
find_interesting_uses (struct ivopts_data *data)
{
  basic_block bb;
  gimple_stmt_iterator bsi;
  basic_block *body = get_loop_body (data->current_loop);
  unsigned i;
  struct version_info *info;
  edge e;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Uses:\n\n");

  for (i = 0; i < data->current_loop->num_nodes; i++)
    {
      edge_iterator ei;
      bb = body[i];

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->dest != EXIT_BLOCK_PTR
	    && !flow_bb_inside_loop_p (data->current_loop, e->dest))
	  find_interesting_uses_outside (data, e);

      for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	find_interesting_uses_stmt (data, gsi_stmt (bsi));
      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	if (!is_gimple_debug (gsi_stmt (bsi)))
	  find_interesting_uses_stmt (data, gsi_stmt (bsi));
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      bitmap_iterator bi;

      fprintf (dump_file, "\n");

      EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, i, bi)
	{
	  info = ver_info (data, i);
	  if (info->inv_id)
	    {
	      fprintf (dump_file, "  ");
	      print_generic_expr (dump_file, info->name, TDF_SLIM);
	      fprintf (dump_file, " is invariant (%d)%s\n",
		       info->inv_id, info->has_nonlin_use ? "" : ", eliminable");
	    }
	}

      fprintf (dump_file, "\n");
    }

  free (body);
}

/* Strips constant offsets from EXPR and stores them to OFFSET.  If INSIDE_ADDR
   is true, assume we are inside an address.  If TOP_COMPREF is true, assume
   we are at the top-level of the processed address.  */

static tree
strip_offset_1 (tree expr, bool inside_addr, bool top_compref,
		unsigned HOST_WIDE_INT *offset)
{
  tree op0 = NULL_TREE, op1 = NULL_TREE, tmp, step;
  enum tree_code code;
  tree type, orig_type = TREE_TYPE (expr);
  unsigned HOST_WIDE_INT off0, off1, st;
  tree orig_expr = expr;

  STRIP_NOPS (expr);

  type = TREE_TYPE (expr);
  code = TREE_CODE (expr);
  *offset = 0;

  switch (code)
    {
    case INTEGER_CST:
      if (!cst_and_fits_in_hwi (expr)
	  || integer_zerop (expr))
	return orig_expr;

      *offset = int_cst_value (expr);
      return build_int_cst (orig_type, 0);

    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
      op0 = TREE_OPERAND (expr, 0);
      op1 = TREE_OPERAND (expr, 1);

      op0 = strip_offset_1 (op0, false, false, &off0);
      op1 = strip_offset_1 (op1, false, false, &off1);

      *offset = (code == MINUS_EXPR ? off0 - off1 : off0 + off1);
      if (op0 == TREE_OPERAND (expr, 0)
	  && op1 == TREE_OPERAND (expr, 1))
	return orig_expr;

      if (integer_zerop (op1))
	expr = op0;
      else if (integer_zerop (op0))
	{
	  if (code == MINUS_EXPR)
	    expr = fold_build1 (NEGATE_EXPR, type, op1);
	  else
	    expr = op1;
	}
      else
	expr = fold_build2 (code, type, op0, op1);

      return fold_convert (orig_type, expr);

    case MULT_EXPR:
      op1 = TREE_OPERAND (expr, 1);
      if (!cst_and_fits_in_hwi (op1))
	return orig_expr;

      op0 = TREE_OPERAND (expr, 0);
      op0 = strip_offset_1 (op0, false, false, &off0);
      if (op0 == TREE_OPERAND (expr, 0))
	return orig_expr;

      *offset = off0 * int_cst_value (op1);
      if (integer_zerop (op0))
	expr = op0;
      else
	expr = fold_build2 (MULT_EXPR, type, op0, op1);

      return fold_convert (orig_type, expr);

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      if (!inside_addr)
	return orig_expr;

      step = array_ref_element_size (expr);
      if (!cst_and_fits_in_hwi (step))
	break;

      st = int_cst_value (step);
      op1 = TREE_OPERAND (expr, 1);
      op1 = strip_offset_1 (op1, false, false, &off1);
      *offset = off1 * st;

      if (top_compref
	  && integer_zerop (op1))
	{
	  /* Strip the component reference completely.  */
	  op0 = TREE_OPERAND (expr, 0);
	  op0 = strip_offset_1 (op0, inside_addr, top_compref, &off0);
	  *offset += off0;
	  return op0;
	}
      break;

    case COMPONENT_REF:
      if (!inside_addr)
	return orig_expr;

      tmp = component_ref_field_offset (expr);
      if (top_compref
	  && cst_and_fits_in_hwi (tmp))
	{
	  /* Strip the component reference completely.  */
	  op0 = TREE_OPERAND (expr, 0);
	  op0 = strip_offset_1 (op0, inside_addr, top_compref, &off0);
	  *offset = off0 + int_cst_value (tmp);
	  return op0;
	}
      break;

    case ADDR_EXPR:
      op0 = TREE_OPERAND (expr, 0);
      op0 = strip_offset_1 (op0, true, true, &off0);
      *offset += off0;

      if (op0 == TREE_OPERAND (expr, 0))
	return orig_expr;

      expr = build_fold_addr_expr (op0);
      return fold_convert (orig_type, expr);

    case MEM_REF:
      /* ???  Offset operand?  */
      inside_addr = false;
      break;

    default:
      return orig_expr;
    }

  /* Default handling of expressions for that we want to recurse into
     the first operand.  */
  op0 = TREE_OPERAND (expr, 0);
  op0 = strip_offset_1 (op0, inside_addr, false, &off0);
  *offset += off0;

  if (op0 == TREE_OPERAND (expr, 0)
      && (!op1 || op1 == TREE_OPERAND (expr, 1)))
    return orig_expr;

  expr = copy_node (expr);
  TREE_OPERAND (expr, 0) = op0;
  if (op1)
    TREE_OPERAND (expr, 1) = op1;

  /* Inside address, we might strip the top level component references,
     thus changing type of the expression.  Handling of ADDR_EXPR
     will fix that.  */
  expr = fold_convert (orig_type, expr);

  return expr;
}

/* Strips constant offsets from EXPR and stores them to OFFSET.  */

static tree
strip_offset (tree expr, unsigned HOST_WIDE_INT *offset)
{
  return strip_offset_1 (expr, false, false, offset);
}

/* Returns variant of TYPE that can be used as base for different uses.
   We return unsigned type with the same precision, which avoids problems
   with overflows.  */

static tree
generic_type_for (tree type)
{
  if (POINTER_TYPE_P (type))
    return unsigned_type_for (type);

  if (TYPE_UNSIGNED (type))
    return type;

  return unsigned_type_for (type);
}

/* Records invariants in *EXPR_P.  Callback for walk_tree.  DATA contains
   the bitmap to that we should store it.  */

static struct ivopts_data *fd_ivopts_data;
static tree
find_depends (tree *expr_p, int *ws ATTRIBUTE_UNUSED, void *data)
{
  bitmap *depends_on = (bitmap *) data;
  struct version_info *info;

  if (TREE_CODE (*expr_p) != SSA_NAME)
    return NULL_TREE;
  info = name_info (fd_ivopts_data, *expr_p);

  if (!info->inv_id || info->has_nonlin_use)
    return NULL_TREE;

  if (!*depends_on)
    *depends_on = BITMAP_ALLOC (NULL);
  bitmap_set_bit (*depends_on, info->inv_id);

  return NULL_TREE;
}

/* Adds a candidate BASE + STEP * i.  Important field is set to IMPORTANT and
   position to POS.  If USE is not NULL, the candidate is set as related to
   it.  If both BASE and STEP are NULL, we add a pseudocandidate for the
   replacement of the final value of the iv by a direct computation.  */

static struct iv_cand *
add_candidate_1 (struct ivopts_data *data,
		 tree base, tree step, bool important, enum iv_position pos,
		 struct iv_use *use, gimple incremented_at)
{
  unsigned i;
  struct iv_cand *cand = NULL;
  tree type, orig_type;

  if (base)
    {
      orig_type = TREE_TYPE (base);
      type = generic_type_for (orig_type);
      if (type != orig_type)
	{
	  base = fold_convert (type, base);
	  step = fold_convert (type, step);
	}
    }

  for (i = 0; i < n_iv_cands (data); i++)
    {
      cand = iv_cand (data, i);

      if (cand->pos != pos)
	continue;

      if (cand->incremented_at != incremented_at
	  || ((pos == IP_AFTER_USE || pos == IP_BEFORE_USE)
	      && cand->ainc_use != use))
	continue;

      if (!cand->iv)
	{
	  if (!base && !step)
	    break;

	  continue;
	}

      if (!base && !step)
	continue;

      if (operand_equal_p (base, cand->iv->base, 0)
	  && operand_equal_p (step, cand->iv->step, 0)
          && (TYPE_PRECISION (TREE_TYPE (base))
              == TYPE_PRECISION (TREE_TYPE (cand->iv->base))))
	break;
    }

  if (i == n_iv_cands (data))
    {
      cand = XCNEW (struct iv_cand);
      cand->id = i;

      if (!base && !step)
	cand->iv = NULL;
      else
	cand->iv = alloc_iv (base, step);

      cand->pos = pos;
      if (pos != IP_ORIGINAL && cand->iv)
	{
	  cand->var_before = create_tmp_var_raw (TREE_TYPE (base), "ivtmp");
	  cand->var_after = cand->var_before;
	}
      cand->important = important;
      cand->incremented_at = incremented_at;
      VEC_safe_push (iv_cand_p, heap, data->iv_candidates, cand);

      if (step
	  && TREE_CODE (step) != INTEGER_CST)
	{
	  fd_ivopts_data = data;
	  walk_tree (&step, find_depends, &cand->depends_on, NULL);
	}

      if (pos == IP_AFTER_USE || pos == IP_BEFORE_USE)
	cand->ainc_use = use;
      else
	cand->ainc_use = NULL;

      if (dump_file && (dump_flags & TDF_DETAILS))
	dump_cand (dump_file, cand);
    }

  if (important && !cand->important)
    {
      cand->important = true;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Candidate %d is important\n", cand->id);
    }

  if (use)
    {
      bitmap_set_bit (use->related_cands, i);
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Candidate %d is related to use %d\n",
		 cand->id, use->id);
    }

  return cand;
}

/* Returns true if incrementing the induction variable at the end of the LOOP
   is allowed.

   The purpose is to avoid splitting latch edge with a biv increment, thus
   creating a jump, possibly confusing other optimization passes and leaving
   less freedom to scheduler.  So we allow IP_END_POS only if IP_NORMAL_POS
   is not available (so we do not have a better alternative), or if the latch
   edge is already nonempty.  */

static bool
allow_ip_end_pos_p (struct loop *loop)
{
  if (!ip_normal_pos (loop))
    return true;

  if (!empty_block_p (ip_end_pos (loop)))
    return true;

  return false;
}

/* If possible, adds autoincrement candidates BASE + STEP * i based on use USE.
   Important field is set to IMPORTANT.  */

static void
add_autoinc_candidates (struct ivopts_data *data, tree base, tree step,
			bool important, struct iv_use *use)
{
  basic_block use_bb = gimple_bb (use->stmt);
  enum machine_mode mem_mode;
  unsigned HOST_WIDE_INT cstepi;

  /* If we insert the increment in any position other than the standard
     ones, we must ensure that it is incremented once per iteration.
     It must not be in an inner nested loop, or one side of an if
     statement.  */
  if (use_bb->loop_father != data->current_loop
      || !dominated_by_p (CDI_DOMINATORS, data->current_loop->latch, use_bb)
      || stmt_could_throw_p (use->stmt)
      || !cst_and_fits_in_hwi (step))
    return;

  cstepi = int_cst_value (step);

  mem_mode = TYPE_MODE (TREE_TYPE (*use->op_p));
  if ((HAVE_PRE_INCREMENT && GET_MODE_SIZE (mem_mode) == cstepi)
      || (HAVE_PRE_DECREMENT && GET_MODE_SIZE (mem_mode) == -cstepi))
    {
      enum tree_code code = MINUS_EXPR;
      tree new_base;
      tree new_step = step;

      if (POINTER_TYPE_P (TREE_TYPE (base)))
	{
	  new_step = fold_build1 (NEGATE_EXPR, TREE_TYPE (step), step);
	  code = POINTER_PLUS_EXPR;
	}
      else
	new_step = fold_convert (TREE_TYPE (base), new_step);
      new_base = fold_build2 (code, TREE_TYPE (base), base, new_step);
      add_candidate_1 (data, new_base, step, important, IP_BEFORE_USE, use,
		       use->stmt);
    }
  if ((HAVE_POST_INCREMENT && GET_MODE_SIZE (mem_mode) == cstepi)
      || (HAVE_POST_DECREMENT && GET_MODE_SIZE (mem_mode) == -cstepi))
    {
      add_candidate_1 (data, base, step, important, IP_AFTER_USE, use,
		       use->stmt);
    }
}

/* Adds a candidate BASE + STEP * i.  Important field is set to IMPORTANT and
   position to POS.  If USE is not NULL, the candidate is set as related to
   it.  The candidate computation is scheduled on all available positions.  */

static void
add_candidate (struct ivopts_data *data,
	       tree base, tree step, bool important, struct iv_use *use)
{
  if (ip_normal_pos (data->current_loop))
    add_candidate_1 (data, base, step, important, IP_NORMAL, use, NULL);
  if (ip_end_pos (data->current_loop)
      && allow_ip_end_pos_p (data->current_loop))
    add_candidate_1 (data, base, step, important, IP_END, use, NULL);

  if (use != NULL && use->type == USE_ADDRESS)
    add_autoinc_candidates (data, base, step, important, use);
}

/* Add a standard "0 + 1 * iteration" iv candidate for a
   type with SIZE bits.  */

static void
add_standard_iv_candidates_for_size (struct ivopts_data *data,
				     unsigned int size)
{
  tree type = lang_hooks.types.type_for_size (size, true);
  add_candidate (data, build_int_cst (type, 0), build_int_cst (type, 1),
		 true, NULL);
}

/* Adds standard iv candidates.  */

static void
add_standard_iv_candidates (struct ivopts_data *data)
{
  add_standard_iv_candidates_for_size (data, INT_TYPE_SIZE);

  /* The same for a double-integer type if it is still fast enough.  */
  if (BITS_PER_WORD >= INT_TYPE_SIZE * 2)
    add_standard_iv_candidates_for_size (data, INT_TYPE_SIZE * 2);
}


/* Adds candidates bases on the old induction variable IV.  */

static void
add_old_iv_candidates (struct ivopts_data *data, struct iv *iv)
{
  gimple phi;
  tree def;
  struct iv_cand *cand;

  add_candidate (data, iv->base, iv->step, true, NULL);

  /* The same, but with initial value zero.  */
  if (POINTER_TYPE_P (TREE_TYPE (iv->base)))
    add_candidate (data, size_int (0), iv->step, true, NULL);
  else
    add_candidate (data, build_int_cst (TREE_TYPE (iv->base), 0),
		   iv->step, true, NULL);

  phi = SSA_NAME_DEF_STMT (iv->ssa_name);
  if (gimple_code (phi) == GIMPLE_PHI)
    {
      /* Additionally record the possibility of leaving the original iv
	 untouched.  */
      def = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (data->current_loop));
      cand = add_candidate_1 (data,
			      iv->base, iv->step, true, IP_ORIGINAL, NULL,
			      SSA_NAME_DEF_STMT (def));
      cand->var_before = iv->ssa_name;
      cand->var_after = def;
    }
}

/* Adds candidates based on the old induction variables.  */

static void
add_old_ivs_candidates (struct ivopts_data *data)
{
  unsigned i;
  struct iv *iv;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, i, bi)
    {
      iv = ver_info (data, i)->iv;
      if (iv && iv->biv_p && !integer_zerop (iv->step))
	add_old_iv_candidates (data, iv);
    }
}

/* Adds candidates based on the value of the induction variable IV and USE.  */

static void
add_iv_value_candidates (struct ivopts_data *data,
			 struct iv *iv, struct iv_use *use)
{
  unsigned HOST_WIDE_INT offset;
  tree base;
  tree basetype;

  add_candidate (data, iv->base, iv->step, false, use);

  /* The same, but with initial value zero.  Make such variable important,
     since it is generic enough so that possibly many uses may be based
     on it.  */
  basetype = TREE_TYPE (iv->base);
  if (POINTER_TYPE_P (basetype))
    basetype = sizetype;
  add_candidate (data, build_int_cst (basetype, 0),
		 iv->step, true, use);

  /* Third, try removing the constant offset.  Make sure to even
     add a candidate for &a[0] vs. (T *)&a.  */
  base = strip_offset (iv->base, &offset);
  if (offset
      || base != iv->base)
    add_candidate (data, base, iv->step, false, use);
}

/* Adds candidates based on the uses.  */

static void
add_derived_ivs_candidates (struct ivopts_data *data)
{
  unsigned i;

  for (i = 0; i < n_iv_uses (data); i++)
    {
      struct iv_use *use = iv_use (data, i);

      if (!use)
	continue;

      switch (use->type)
	{
	case USE_NONLINEAR_EXPR:
	case USE_COMPARE:
	case USE_ADDRESS:
	  /* Just add the ivs based on the value of the iv used here.  */
	  add_iv_value_candidates (data, use->iv, use);
	  break;

	default:
	  gcc_unreachable ();
	}
    }
}

/* Record important candidates and add them to related_cands bitmaps
   if needed.  */

static void
record_important_candidates (struct ivopts_data *data)
{
  unsigned i;
  struct iv_use *use;

  for (i = 0; i < n_iv_cands (data); i++)
    {
      struct iv_cand *cand = iv_cand (data, i);

      if (cand->important)
	bitmap_set_bit (data->important_candidates, i);
    }

  data->consider_all_candidates = (n_iv_cands (data)
				   <= CONSIDER_ALL_CANDIDATES_BOUND);

  if (data->consider_all_candidates)
    {
      /* We will not need "related_cands" bitmaps in this case,
	 so release them to decrease peak memory consumption.  */
      for (i = 0; i < n_iv_uses (data); i++)
	{
	  use = iv_use (data, i);
	  BITMAP_FREE (use->related_cands);
	}
    }
  else
    {
      /* Add important candidates to the related_cands bitmaps.  */
      for (i = 0; i < n_iv_uses (data); i++)
	bitmap_ior_into (iv_use (data, i)->related_cands,
			 data->important_candidates);
    }
}

/* Allocates the data structure mapping the (use, candidate) pairs to costs.
   If consider_all_candidates is true, we use a two-dimensional array, otherwise
   we allocate a simple list to every use.  */

static void
alloc_use_cost_map (struct ivopts_data *data)
{
  unsigned i, size, s, j;

  for (i = 0; i < n_iv_uses (data); i++)
    {
      struct iv_use *use = iv_use (data, i);
      bitmap_iterator bi;

      if (data->consider_all_candidates)
	size = n_iv_cands (data);
      else
	{
	  s = 0;
	  EXECUTE_IF_SET_IN_BITMAP (use->related_cands, 0, j, bi)
	    {
	      s++;
	    }

	  /* Round up to the power of two, so that moduling by it is fast.  */
	  for (size = 1; size < s; size <<= 1)
	    continue;
	}

      use->n_map_members = size;
      use->cost_map = XCNEWVEC (struct cost_pair, size);
    }
}

/* Returns description of computation cost of expression whose runtime
   cost is RUNTIME and complexity corresponds to COMPLEXITY.  */

static comp_cost
new_cost (unsigned runtime, unsigned complexity)
{
  comp_cost cost;

  cost.cost = runtime;
  cost.complexity = complexity;

  return cost;
}

/* Adds costs COST1 and COST2.  */

static comp_cost
add_costs (comp_cost cost1, comp_cost cost2)
{
  cost1.cost += cost2.cost;
  cost1.complexity += cost2.complexity;

  return cost1;
}
/* Subtracts costs COST1 and COST2.  */

static comp_cost
sub_costs (comp_cost cost1, comp_cost cost2)
{
  cost1.cost -= cost2.cost;
  cost1.complexity -= cost2.complexity;

  return cost1;
}

/* Returns a negative number if COST1 < COST2, a positive number if
   COST1 > COST2, and 0 if COST1 = COST2.  */

static int
compare_costs (comp_cost cost1, comp_cost cost2)
{
  if (cost1.cost == cost2.cost)
    return cost1.complexity - cost2.complexity;

  return cost1.cost - cost2.cost;
}

/* Returns true if COST is infinite.  */

static bool
infinite_cost_p (comp_cost cost)
{
  return cost.cost == INFTY;
}

/* Sets cost of (USE, CANDIDATE) pair to COST and record that it depends
   on invariants DEPENDS_ON and that the value used in expressing it
   is VALUE.  */

static void
set_use_iv_cost (struct ivopts_data *data,
		 struct iv_use *use, struct iv_cand *cand,
		 comp_cost cost, bitmap depends_on, tree value,
                 int inv_expr_id)
{
  unsigned i, s;

  if (infinite_cost_p (cost))
    {
      BITMAP_FREE (depends_on);
      return;
    }

  if (data->consider_all_candidates)
    {
      use->cost_map[cand->id].cand = cand;
      use->cost_map[cand->id].cost = cost;
      use->cost_map[cand->id].depends_on = depends_on;
      use->cost_map[cand->id].value = value;
      use->cost_map[cand->id].inv_expr_id = inv_expr_id;
      return;
    }

  /* n_map_members is a power of two, so this computes modulo.  */
  s = cand->id & (use->n_map_members - 1);
  for (i = s; i < use->n_map_members; i++)
    if (!use->cost_map[i].cand)
      goto found;
  for (i = 0; i < s; i++)
    if (!use->cost_map[i].cand)
      goto found;

  gcc_unreachable ();

found:
  use->cost_map[i].cand = cand;
  use->cost_map[i].cost = cost;
  use->cost_map[i].depends_on = depends_on;
  use->cost_map[i].value = value;
  use->cost_map[i].inv_expr_id = inv_expr_id;
}

/* Gets cost of (USE, CANDIDATE) pair.  */

static struct cost_pair *
get_use_iv_cost (struct ivopts_data *data, struct iv_use *use,
		 struct iv_cand *cand)
{
  unsigned i, s;
  struct cost_pair *ret;

  if (!cand)
    return NULL;

  if (data->consider_all_candidates)
    {
      ret = use->cost_map + cand->id;
      if (!ret->cand)
	return NULL;

      return ret;
    }

  /* n_map_members is a power of two, so this computes modulo.  */
  s = cand->id & (use->n_map_members - 1);
  for (i = s; i < use->n_map_members; i++)
    if (use->cost_map[i].cand == cand)
      return use->cost_map + i;

  for (i = 0; i < s; i++)
    if (use->cost_map[i].cand == cand)
      return use->cost_map + i;

  return NULL;
}

/* Returns estimate on cost of computing SEQ.  */

static unsigned
seq_cost (rtx seq, bool speed)
{
  unsigned cost = 0;
  rtx set;

  for (; seq; seq = NEXT_INSN (seq))
    {
      set = single_set (seq);
      if (set)
	cost += rtx_cost (set, SET,speed);
      else
	cost++;
    }

  return cost;
}

/* Produce DECL_RTL for object obj so it looks like it is stored in memory.  */
static rtx
produce_memory_decl_rtl (tree obj, int *regno)
{
  addr_space_t as = TYPE_ADDR_SPACE (TREE_TYPE (obj));
  enum machine_mode address_mode = targetm.addr_space.address_mode (as);
  rtx x;

  gcc_assert (obj);
  if (TREE_STATIC (obj) || DECL_EXTERNAL (obj))
    {
      const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (obj));
      x = gen_rtx_SYMBOL_REF (address_mode, name);
      SET_SYMBOL_REF_DECL (x, obj);
      x = gen_rtx_MEM (DECL_MODE (obj), x);
      set_mem_addr_space (x, as);
      targetm.encode_section_info (obj, x, true);
    }
  else
    {
      x = gen_raw_REG (address_mode, (*regno)++);
      x = gen_rtx_MEM (DECL_MODE (obj), x);
      set_mem_addr_space (x, as);
    }

  return x;
}

/* Prepares decl_rtl for variables referred in *EXPR_P.  Callback for
   walk_tree.  DATA contains the actual fake register number.  */

static tree
prepare_decl_rtl (tree *expr_p, int *ws, void *data)
{
  tree obj = NULL_TREE;
  rtx x = NULL_RTX;
  int *regno = (int *) data;

  switch (TREE_CODE (*expr_p))
    {
    case ADDR_EXPR:
      for (expr_p = &TREE_OPERAND (*expr_p, 0);
	   handled_component_p (*expr_p);
	   expr_p = &TREE_OPERAND (*expr_p, 0))
	continue;
      obj = *expr_p;
      if (DECL_P (obj) && !DECL_RTL_SET_P (obj))
        x = produce_memory_decl_rtl (obj, regno);
      break;

    case SSA_NAME:
      *ws = 0;
      obj = SSA_NAME_VAR (*expr_p);
      if (!DECL_RTL_SET_P (obj))
	x = gen_raw_REG (DECL_MODE (obj), (*regno)++);
      break;

    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      *ws = 0;
      obj = *expr_p;

      if (DECL_RTL_SET_P (obj))
	break;

      if (DECL_MODE (obj) == BLKmode)
	x = produce_memory_decl_rtl (obj, regno);
      else
	x = gen_raw_REG (DECL_MODE (obj), (*regno)++);

      break;

    default:
      break;
    }

  if (x)
    {
      VEC_safe_push (tree, heap, decl_rtl_to_reset, obj);
      SET_DECL_RTL (obj, x);
    }

  return NULL_TREE;
}

/* Determines cost of the computation of EXPR.  */

static unsigned
computation_cost (tree expr, bool speed)
{
  rtx seq, rslt;
  tree type = TREE_TYPE (expr);
  unsigned cost;
  /* Avoid using hard regs in ways which may be unsupported.  */
  int regno = LAST_VIRTUAL_REGISTER + 1;
  struct cgraph_node *node = cgraph_node (current_function_decl);
  enum node_frequency real_frequency = node->frequency;

  node->frequency = NODE_FREQUENCY_NORMAL;
  crtl->maybe_hot_insn_p = speed;
  walk_tree (&expr, prepare_decl_rtl, &regno, NULL);
  start_sequence ();
  rslt = expand_expr (expr, NULL_RTX, TYPE_MODE (type), EXPAND_NORMAL);
  seq = get_insns ();
  end_sequence ();
  default_rtl_profile ();
  node->frequency = real_frequency;

  cost = seq_cost (seq, speed);
  if (MEM_P (rslt))
    cost += address_cost (XEXP (rslt, 0), TYPE_MODE (type),
			  TYPE_ADDR_SPACE (type), speed);

  return cost;
}

/* Returns variable containing the value of candidate CAND at statement AT.  */

static tree
var_at_stmt (struct loop *loop, struct iv_cand *cand, gimple stmt)
{
  if (stmt_after_increment (loop, cand, stmt))
    return cand->var_after;
  else
    return cand->var_before;
}

/* Return the most significant (sign) bit of T.  Similar to tree_int_cst_msb,
   but the bit is determined from TYPE_PRECISION, not MODE_BITSIZE.  */

int
tree_int_cst_sign_bit (const_tree t)
{
  unsigned bitno = TYPE_PRECISION (TREE_TYPE (t)) - 1;
  unsigned HOST_WIDE_INT w;

  if (bitno < HOST_BITS_PER_WIDE_INT)
    w = TREE_INT_CST_LOW (t);
  else
    {
      w = TREE_INT_CST_HIGH (t);
      bitno -= HOST_BITS_PER_WIDE_INT;
    }

  return (w >> bitno) & 1;
}

/* If A is (TYPE) BA and B is (TYPE) BB, and the types of BA and BB have the
   same precision that is at least as wide as the precision of TYPE, stores
   BA to A and BB to B, and returns the type of BA.  Otherwise, returns the
   type of A and B.  */

static tree
determine_common_wider_type (tree *a, tree *b)
{
  tree wider_type = NULL;
  tree suba, subb;
  tree atype = TREE_TYPE (*a);

  if (CONVERT_EXPR_P (*a))
    {
      suba = TREE_OPERAND (*a, 0);
      wider_type = TREE_TYPE (suba);
      if (TYPE_PRECISION (wider_type) < TYPE_PRECISION (atype))
	return atype;
    }
  else
    return atype;

  if (CONVERT_EXPR_P (*b))
    {
      subb = TREE_OPERAND (*b, 0);
      if (TYPE_PRECISION (wider_type) != TYPE_PRECISION (TREE_TYPE (subb)))
	return atype;
    }
  else
    return atype;

  *a = suba;
  *b = subb;
  return wider_type;
}

/* Determines the expression by that USE is expressed from induction variable
   CAND at statement AT in LOOP.  The expression is stored in a decomposed
   form into AFF.  Returns false if USE cannot be expressed using CAND.  */

static bool
get_computation_aff (struct loop *loop,
		     struct iv_use *use, struct iv_cand *cand, gimple at,
		     struct affine_tree_combination *aff)
{
  tree ubase = use->iv->base;
  tree ustep = use->iv->step;
  tree cbase = cand->iv->base;
  tree cstep = cand->iv->step, cstep_common;
  tree utype = TREE_TYPE (ubase), ctype = TREE_TYPE (cbase);
  tree common_type, var;
  tree uutype;
  aff_tree cbase_aff, var_aff;
  double_int rat;

  if (TYPE_PRECISION (utype) > TYPE_PRECISION (ctype))
    {
      /* We do not have a precision to express the values of use.  */
      return false;
    }

  var = var_at_stmt (loop, cand, at);
  uutype = unsigned_type_for (utype);

  /* If the conversion is not noop, perform it.  */
  if (TYPE_PRECISION (utype) < TYPE_PRECISION (ctype))
    {
      cstep = fold_convert (uutype, cstep);
      cbase = fold_convert (uutype, cbase);
      var = fold_convert (uutype, var);
    }

  if (!constant_multiple_of (ustep, cstep, &rat))
    return false;

  /* In case both UBASE and CBASE are shortened to UUTYPE from some common
     type, we achieve better folding by computing their difference in this
     wider type, and cast the result to UUTYPE.  We do not need to worry about
     overflows, as all the arithmetics will in the end be performed in UUTYPE
     anyway.  */
  common_type = determine_common_wider_type (&ubase, &cbase);

  /* use = ubase - ratio * cbase + ratio * var.  */
  tree_to_aff_combination (ubase, common_type, aff);
  tree_to_aff_combination (cbase, common_type, &cbase_aff);
  tree_to_aff_combination (var, uutype, &var_aff);

  /* We need to shift the value if we are after the increment.  */
  if (stmt_after_increment (loop, cand, at))
    {
      aff_tree cstep_aff;

      if (common_type != uutype)
	cstep_common = fold_convert (common_type, cstep);
      else
	cstep_common = cstep;

      tree_to_aff_combination (cstep_common, common_type, &cstep_aff);
      aff_combination_add (&cbase_aff, &cstep_aff);
    }

  aff_combination_scale (&cbase_aff, double_int_neg (rat));
  aff_combination_add (aff, &cbase_aff);
  if (common_type != uutype)
    aff_combination_convert (aff, uutype);

  aff_combination_scale (&var_aff, rat);
  aff_combination_add (aff, &var_aff);

  return true;
}

/* Determines the expression by that USE is expressed from induction variable
   CAND at statement AT in LOOP.  The computation is unshared.  */

static tree
get_computation_at (struct loop *loop,
		    struct iv_use *use, struct iv_cand *cand, gimple at)
{
  aff_tree aff;
  tree type = TREE_TYPE (use->iv->base);

  if (!get_computation_aff (loop, use, cand, at, &aff))
    return NULL_TREE;
  unshare_aff_combination (&aff);
  return fold_convert (type, aff_combination_to_tree (&aff));
}

/* Determines the expression by that USE is expressed from induction variable
   CAND in LOOP.  The computation is unshared.  */

static tree
get_computation (struct loop *loop, struct iv_use *use, struct iv_cand *cand)
{
  return get_computation_at (loop, use, cand, use->stmt);
}

/* Adjust the cost COST for being in loop setup rather than loop body.
   If we're optimizing for space, the loop setup overhead is constant;
   if we're optimizing for speed, amortize it over the per-iteration cost.  */
static unsigned
adjust_setup_cost (struct ivopts_data *data, unsigned cost)
{
  if (cost == INFTY)
    return cost;
  else if (optimize_loop_for_speed_p (data->current_loop))
    return cost / avg_loop_niter (data->current_loop);
  else
    return cost;
}

/* Returns cost of addition in MODE.  */

static unsigned
add_cost (enum machine_mode mode, bool speed)
{
  static unsigned costs[NUM_MACHINE_MODES];
  rtx seq;
  unsigned cost;

  if (costs[mode])
    return costs[mode];

  start_sequence ();
  force_operand (gen_rtx_fmt_ee (PLUS, mode,
				 gen_raw_REG (mode, LAST_VIRTUAL_REGISTER + 1),
				 gen_raw_REG (mode, LAST_VIRTUAL_REGISTER + 2)),
		 NULL_RTX);
  seq = get_insns ();
  end_sequence ();

  cost = seq_cost (seq, speed);
  if (!cost)
    cost = 1;

  costs[mode] = cost;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Addition in %s costs %d\n",
	     GET_MODE_NAME (mode), cost);
  return cost;
}

/* Entry in a hashtable of already known costs for multiplication.  */
struct mbc_entry
{
  HOST_WIDE_INT cst;		/* The constant to multiply by.  */
  enum machine_mode mode;	/* In mode.  */
  unsigned cost;		/* The cost.  */
};

/* Counts hash value for the ENTRY.  */

static hashval_t
mbc_entry_hash (const void *entry)
{
  const struct mbc_entry *e = (const struct mbc_entry *) entry;

  return 57 * (hashval_t) e->mode + (hashval_t) (e->cst % 877);
}

/* Compares the hash table entries ENTRY1 and ENTRY2.  */

static int
mbc_entry_eq (const void *entry1, const void *entry2)
{
  const struct mbc_entry *e1 = (const struct mbc_entry *) entry1;
  const struct mbc_entry *e2 = (const struct mbc_entry *) entry2;

  return (e1->mode == e2->mode
	  && e1->cst == e2->cst);
}

/* Returns cost of multiplication by constant CST in MODE.  */

unsigned
multiply_by_cost (HOST_WIDE_INT cst, enum machine_mode mode, bool speed)
{
  static htab_t costs;
  struct mbc_entry **cached, act;
  rtx seq;
  unsigned cost;

  if (!costs)
    costs = htab_create (100, mbc_entry_hash, mbc_entry_eq, free);

  act.mode = mode;
  act.cst = cst;
  cached = (struct mbc_entry **) htab_find_slot (costs, &act, INSERT);
  if (*cached)
    return (*cached)->cost;

  *cached = XNEW (struct mbc_entry);
  (*cached)->mode = mode;
  (*cached)->cst = cst;

  start_sequence ();
  expand_mult (mode, gen_raw_REG (mode, LAST_VIRTUAL_REGISTER + 1),
	       gen_int_mode (cst, mode), NULL_RTX, 0);
  seq = get_insns ();
  end_sequence ();

  cost = seq_cost (seq, speed);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Multiplication by %d in %s costs %d\n",
	     (int) cst, GET_MODE_NAME (mode), cost);

  (*cached)->cost = cost;

  return cost;
}

/* Returns true if multiplying by RATIO is allowed in an address.  Test the
   validity for a memory reference accessing memory of mode MODE in
   address space AS.  */

DEF_VEC_P (sbitmap);
DEF_VEC_ALLOC_P (sbitmap, heap);

bool
xxmultiplier_allowed_in_address_p (HOST_WIDE_INT ratio, enum machine_mode mode,
				 addr_space_t as)
{
#define MAX_RATIO 128
  unsigned int data_index = (int) as * MAX_MACHINE_MODE + (int) mode;
  static VEC (sbitmap, heap) *valid_mult_list;
  sbitmap valid_mult;

  if (data_index >= VEC_length (sbitmap, valid_mult_list))
    VEC_safe_grow_cleared (sbitmap, heap, valid_mult_list, data_index + 1);

  valid_mult = VEC_index (sbitmap, valid_mult_list, data_index);
  if (!valid_mult)
    {
      enum machine_mode address_mode = targetm.addr_space.address_mode (as);
      rtx reg1 = gen_raw_REG (address_mode, LAST_VIRTUAL_REGISTER + 1);
      rtx addr;
      HOST_WIDE_INT i;

      valid_mult = sbitmap_alloc (2 * MAX_RATIO + 1);
      sbitmap_zero (valid_mult);
      addr = gen_rtx_fmt_ee (MULT, address_mode, reg1, NULL_RTX);
      for (i = -MAX_RATIO; i <= MAX_RATIO; i++)
	{
	  XEXP (addr, 1) = gen_int_mode (i, address_mode);
	  if (memory_address_addr_space_p (mode, addr, as))
	    SET_BIT (valid_mult, i + MAX_RATIO);
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  allowed multipliers:");
	  for (i = -MAX_RATIO; i <= MAX_RATIO; i++)
	    if (TEST_BIT (valid_mult, i + MAX_RATIO))
	      fprintf (dump_file, " %d", (int) i);
	  fprintf (dump_file, "\n");
	  fprintf (dump_file, "\n");
	}

      VEC_replace (sbitmap, valid_mult_list, data_index, valid_mult);
    }

  if (ratio > MAX_RATIO || ratio < -MAX_RATIO)
    return false;

  return TEST_BIT (valid_mult, ratio + MAX_RATIO);
}

EXTERN_C_END
