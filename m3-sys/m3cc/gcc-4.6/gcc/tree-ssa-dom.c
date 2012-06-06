/* Modula-3: modified */

/* SSA Dominator optimizations for trees
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

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
#include "cfgloop.h"
#include "output.h"
#include "function.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "timevar.h"
#include "tree-dump.h"
#include "tree-flow.h"
#include "domwalk.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"
#include "langhooks.h"
#include "params.h"

EXTERN_C_START

/* This file implements optimizations on the dominator tree.  */

/* Representation of a "naked" right-hand-side expression, to be used
   in recording available expressions in the expression hash table.  */

enum expr_kind
{
  EXPR_SINGLE,
  EXPR_UNARY,
  EXPR_BINARY,
  EXPR_TERNARY,
  EXPR_CALL
};

struct hashable_expr
{
  tree type;
  enum expr_kind kind;
  union {
    struct { tree rhs; } single;
    struct { enum tree_code op;  tree opnd; } unary;
    struct { enum tree_code op;  tree opnd0, opnd1; } binary;
    struct { enum tree_code op;  tree opnd0, opnd1, opnd2; } ternary;
    struct { tree fn; bool pure; size_t nargs; tree *args; } call;
  } ops;
};

/* Structure for recording known values of a conditional expression
   at the exits from its block.  */

typedef struct cond_equivalence_s
{
  struct hashable_expr cond;
  tree value;
} cond_equivalence;

DEF_VEC_O(cond_equivalence);
DEF_VEC_ALLOC_O(cond_equivalence,heap);

/* Structure for recording edge equivalences as well as any pending
   edge redirections during the dominator optimizer.

   Computing and storing the edge equivalences instead of creating
   them on-demand can save significant amounts of time, particularly
   for pathological cases involving switch statements.

   These structures live for a single iteration of the dominator
   optimizer in the edge's AUX field.  At the end of an iteration we
   free each of these structures and update the AUX field to point
   to any requested redirection target (the code for updating the
   CFG and SSA graph for edge redirection expects redirection edge
   targets to be in the AUX field for each edge.  */

struct edge_info
{
  /* If this edge creates a simple equivalence, the LHS and RHS of
     the equivalence will be stored here.  */
  tree lhs;
  tree rhs;

  /* Traversing an edge may also indicate one or more particular conditions
     are true or false.  */
  VEC(cond_equivalence, heap) *cond_equivalences;
};

/* Hash table with expressions made available during the renaming process.
   When an assignment of the form X_i = EXPR is found, the statement is
   stored in this table.  If the same expression EXPR is later found on the
   RHS of another statement, it is replaced with X_i (thus performing
   global redundancy elimination).  Similarly as we pass through conditionals
   we record the conditional itself as having either a true or false value
   in this table.  */
static htab_t avail_exprs;

/* Stack of available expressions in AVAIL_EXPRs.  Each block pushes any
   expressions it enters into the hash table along with a marker entry
   (null).  When we finish processing the block, we pop off entries and
   remove the expressions from the global hash table until we hit the
   marker.  */
typedef struct expr_hash_elt * expr_hash_elt_t;
DEF_VEC_P(expr_hash_elt_t);
DEF_VEC_ALLOC_P(expr_hash_elt_t,heap);

static VEC(expr_hash_elt_t,heap) *avail_exprs_stack;

/* Structure for entries in the expression hash table.  */

struct expr_hash_elt
{
  /* The value (lhs) of this expression.  */
  tree lhs;

  /* The expression (rhs) we want to record.  */
  struct hashable_expr expr;

  /* The stmt pointer if this element corresponds to a statement.  */
  gimple stmt;

  /* The hash value for RHS.  */
  hashval_t hash;

  /* A unique stamp, typically the address of the hash
     element itself, used in removing entries from the table.  */
  struct expr_hash_elt *stamp;
};

/* Stack of dest,src pairs that need to be restored during finalization.

   A NULL entry is used to mark the end of pairs which need to be
   restored during finalization of this block.  */
static VEC(tree,heap) *const_and_copies_stack;

/* Track whether or not we have changed the control flow graph.  */
static bool cfg_altered;

/* Bitmap of blocks that have had EH statements cleaned.  We should
   remove their dead edges eventually.  */
static bitmap need_eh_cleanup;

/* Statistics for dominator optimizations.  */
struct opt_stats_d
{
  long num_stmts;
  long num_exprs_considered;
  long num_re;
  long num_const_prop;
  long num_copy_prop;
};

static struct opt_stats_d opt_stats;

/* Local functions.  */
static tree lookup_avail_expr (gimple, bool);
static hashval_t avail_expr_hash (const void *);
static void htab_statistics (FILE *, htab_t);
static void record_cond (cond_equivalence *);
static void record_const_or_copy (tree, tree);
static void record_equality (tree, tree);
static void record_equivalences_from_stmt (gimple, int);
static void remove_local_expressions_from_table (void);
static void restore_vars_to_original_value (void);
static edge single_incoming_edge_ignoring_loop_edges (basic_block);


/* Given a statement STMT, initialize the hash table element pointed to
   by ELEMENT.  */

static void
initialize_hash_element (gimple stmt, tree lhs,
                         struct expr_hash_elt *element)
{
  enum gimple_code code = gimple_code (stmt);
  struct hashable_expr *expr = &element->expr;

  if (code == GIMPLE_ASSIGN)
    {
      enum tree_code subcode = gimple_assign_rhs_code (stmt);

      expr->type = NULL_TREE;

      switch (get_gimple_rhs_class (subcode))
        {
        case GIMPLE_SINGLE_RHS:
	  expr->kind = EXPR_SINGLE;
	  expr->ops.single.rhs = gimple_assign_rhs1 (stmt);
	  break;
        case GIMPLE_UNARY_RHS:
	  expr->kind = EXPR_UNARY;
	  expr->type = TREE_TYPE (gimple_assign_lhs (stmt));
	  expr->ops.unary.op = subcode;
	  expr->ops.unary.opnd = gimple_assign_rhs1 (stmt);
	  break;
        case GIMPLE_BINARY_RHS:
	  expr->kind = EXPR_BINARY;
	  expr->type = TREE_TYPE (gimple_assign_lhs (stmt));
	  expr->ops.binary.op = subcode;
	  expr->ops.binary.opnd0 = gimple_assign_rhs1 (stmt);
	  expr->ops.binary.opnd1 = gimple_assign_rhs2 (stmt);
	  break;
        case GIMPLE_TERNARY_RHS:
	  expr->kind = EXPR_TERNARY;
	  expr->type = TREE_TYPE (gimple_assign_lhs (stmt));
	  expr->ops.ternary.op = subcode;
	  expr->ops.ternary.opnd0 = gimple_assign_rhs1 (stmt);
	  expr->ops.ternary.opnd1 = gimple_assign_rhs2 (stmt);
	  expr->ops.ternary.opnd2 = gimple_assign_rhs3 (stmt);
	  break;
        default:
          gcc_unreachable ();
        }
    }
  else if (code == GIMPLE_COND)
    {
      expr->type = boolean_type_node;
      expr->kind = EXPR_BINARY;
      expr->ops.binary.op = gimple_cond_code (stmt);
      expr->ops.binary.opnd0 = gimple_cond_lhs (stmt);
      expr->ops.binary.opnd1 = gimple_cond_rhs (stmt);
    }
  else if (code == GIMPLE_CALL)
    {
      size_t nargs = gimple_call_num_args (stmt);
      size_t i;

      gcc_assert (gimple_call_lhs (stmt));

      expr->type = TREE_TYPE (gimple_call_lhs (stmt));
      expr->kind = EXPR_CALL;
      expr->ops.call.fn = gimple_call_fn (stmt);

      if (gimple_call_flags (stmt) & (ECF_CONST | ECF_PURE))
        expr->ops.call.pure = true;
      else
        expr->ops.call.pure = false;

      expr->ops.call.nargs = nargs;
      expr->ops.call.args = (tree *) xcalloc (nargs, sizeof (tree));
      for (i = 0; i < nargs; i++)
        expr->ops.call.args[i] = gimple_call_arg (stmt, i);
    }
  else if (code == GIMPLE_SWITCH)
    {
      expr->type = TREE_TYPE (gimple_switch_index (stmt));
      expr->kind = EXPR_SINGLE;
      expr->ops.single.rhs = gimple_switch_index (stmt);
    }
  else if (code == GIMPLE_GOTO)
    {
      expr->type = TREE_TYPE (gimple_goto_dest (stmt));
      expr->kind = EXPR_SINGLE;
      expr->ops.single.rhs = gimple_goto_dest (stmt);
    }
  else
    gcc_unreachable ();

  element->lhs = lhs;
  element->stmt = stmt;
  element->hash = avail_expr_hash (element);
  element->stamp = element;
}

/* Given a conditional expression COND as a tree, initialize
   a hashable_expr expression EXPR.  The conditional must be a
   comparison or logical negation.  A constant or a variable is
   not permitted.  */

static void
initialize_expr_from_cond (tree cond, struct hashable_expr *expr)
{
  expr->type = boolean_type_node;

  if (COMPARISON_CLASS_P (cond))
    {
      expr->kind = EXPR_BINARY;
      expr->ops.binary.op = TREE_CODE (cond);
      expr->ops.binary.opnd0 = TREE_OPERAND (cond, 0);
      expr->ops.binary.opnd1 = TREE_OPERAND (cond, 1);
    }
  else if (TREE_CODE (cond) == TRUTH_NOT_EXPR)
    {
      expr->kind = EXPR_UNARY;
      expr->ops.unary.op = TRUTH_NOT_EXPR;
      expr->ops.unary.opnd = TREE_OPERAND (cond, 0);
    }
  else
    gcc_unreachable ();
}

/* Given a hashable_expr expression EXPR and an LHS,
   initialize the hash table element pointed to by ELEMENT.  */

static void
initialize_hash_element_from_expr (struct hashable_expr *expr,
                                   tree lhs,
                                   struct expr_hash_elt *element)
{
  element->expr = *expr;
  element->lhs = lhs;
  element->stmt = NULL;
  element->hash = avail_expr_hash (element);
  element->stamp = element;
}

/* Compute a hash value for a hashable_expr value EXPR and a
   previously accumulated hash value VAL.  If two hashable_expr
   values compare equal with hashable_expr_equal_p, they must
   hash to the same value, given an identical value of VAL.
   The logic is intended to follow iterative_hash_expr in tree.c.  */

static hashval_t
iterative_hash_hashable_expr (const struct hashable_expr *expr, hashval_t val)
{
  switch (expr->kind)
    {
    case EXPR_SINGLE:
      val = iterative_hash_expr (expr->ops.single.rhs, val);
      break;

    case EXPR_UNARY:
      val = iterative_hash_object (expr->ops.unary.op, val);

      /* Make sure to include signedness in the hash computation.
         Don't hash the type, that can lead to having nodes which
         compare equal according to operand_equal_p, but which
         have different hash codes.  */
      if (CONVERT_EXPR_CODE_P (expr->ops.unary.op)
          || expr->ops.unary.op == NON_LVALUE_EXPR)
        val += TYPE_UNSIGNED (expr->type);

      val = iterative_hash_expr (expr->ops.unary.opnd, val);
      break;

    case EXPR_BINARY:
      val = iterative_hash_object (expr->ops.binary.op, val);
      if (commutative_tree_code (expr->ops.binary.op))
	val = iterative_hash_exprs_commutative (expr->ops.binary.opnd0,
						expr->ops.binary.opnd1, val);
      else
        {
          val = iterative_hash_expr (expr->ops.binary.opnd0, val);
          val = iterative_hash_expr (expr->ops.binary.opnd1, val);
        }
      break;

    case EXPR_TERNARY:
      val = iterative_hash_object (expr->ops.ternary.op, val);
      if (commutative_ternary_tree_code (expr->ops.ternary.op))
	val = iterative_hash_exprs_commutative (expr->ops.ternary.opnd0,
						expr->ops.ternary.opnd1, val);
      else
        {
          val = iterative_hash_expr (expr->ops.ternary.opnd0, val);
          val = iterative_hash_expr (expr->ops.ternary.opnd1, val);
        }
      val = iterative_hash_expr (expr->ops.ternary.opnd2, val);
      break;

    case EXPR_CALL:
      {
        size_t i;
        enum tree_code code = CALL_EXPR;

        val = iterative_hash_object (code, val);
        val = iterative_hash_expr (expr->ops.call.fn, val);
        for (i = 0; i < expr->ops.call.nargs; i++)
          val = iterative_hash_expr (expr->ops.call.args[i], val);
      }
      break;

    default:
      gcc_unreachable ();
    }

  return val;
}

/* Print a diagnostic dump of an expression hash table entry.  */

static void
print_expr_hash_elt (FILE * stream, const struct expr_hash_elt *element)
{
  if (element->stmt)
    fprintf (stream, "STMT ");
  else
    fprintf (stream, "COND ");

  if (element->lhs)
    {
      print_generic_expr (stream, element->lhs, 0);
      fprintf (stream, " = ");
    }

  switch (element->expr.kind)
    {
      case EXPR_SINGLE:
        print_generic_expr (stream, element->expr.ops.single.rhs, 0);
        break;

      case EXPR_UNARY:
        fprintf (stream, "%s ", tree_code_name[element->expr.ops.unary.op]);
        print_generic_expr (stream, element->expr.ops.unary.opnd, 0);
        break;

      case EXPR_BINARY:
        print_generic_expr (stream, element->expr.ops.binary.opnd0, 0);
        fprintf (stream, " %s ", tree_code_name[element->expr.ops.binary.op]);
        print_generic_expr (stream, element->expr.ops.binary.opnd1, 0);
        break;

      case EXPR_TERNARY:
        fprintf (stream, " %s <", tree_code_name[element->expr.ops.ternary.op]);
        print_generic_expr (stream, element->expr.ops.ternary.opnd0, 0);
	fputs (", ", stream);
        print_generic_expr (stream, element->expr.ops.ternary.opnd1, 0);
	fputs (", ", stream);
        print_generic_expr (stream, element->expr.ops.ternary.opnd2, 0);
	fputs (">", stream);
        break;

      case EXPR_CALL:
        {
          size_t i;
          size_t nargs = element->expr.ops.call.nargs;

          print_generic_expr (stream, element->expr.ops.call.fn, 0);
          fprintf (stream, " (");
          for (i = 0; i < nargs; i++)
            {
              print_generic_expr (stream, element->expr.ops.call.args[i], 0);
              if (i + 1 < nargs)
                fprintf (stream, ", ");
            }
          fprintf (stream, ")");
        }
        break;
    }
  fprintf (stream, "\n");

  if (element->stmt)
    {
      fprintf (stream, "          ");
      print_gimple_stmt (stream, element->stmt, 0, 0);
    }
}

/* Delete an expr_hash_elt and reclaim its storage.  */

static void
free_expr_hash_elt (void *elt)
{
  struct expr_hash_elt *element = ((struct expr_hash_elt *)elt);

  if (element->expr.kind == EXPR_CALL)
    free (element->expr.ops.call.args);

  free (element);
}

/* Allocate an EDGE_INFO for edge E and attach it to E.
   Return the new EDGE_INFO structure.  */

static struct edge_info *
allocate_edge_info (edge e)
{
  struct edge_info *edge_info;

  edge_info = XCNEW (struct edge_info);

  e->aux = edge_info;
  return edge_info;
}

/* Free all EDGE_INFO structures associated with edges in the CFG.
   If a particular edge can be threaded, copy the redirection
   target from the EDGE_INFO structure into the edge's AUX field
   as required by code to update the CFG and SSA graph for
   jump threading.  */

static void
free_all_edge_infos (void)
{
  basic_block bb;
  edge_iterator ei;
  edge e;

  FOR_EACH_BB (bb)
    {
      FOR_EACH_EDGE (e, ei, bb->preds)
        {
	 struct edge_info *edge_info = (struct edge_info *) e->aux;

	  if (edge_info)
	    {
	      if (edge_info->cond_equivalences)
		VEC_free (cond_equivalence, heap, edge_info->cond_equivalences);
	      free (edge_info);
	      e->aux = NULL;
	    }
	}
    }
}

/* Given a conditional statement CONDSTMT, convert the
   condition to a canonical form.  */

static void
canonicalize_comparison (gimple condstmt)
{
  tree op0;
  tree op1;
  enum tree_code code;

  gcc_assert (gimple_code (condstmt) == GIMPLE_COND);

  op0 = gimple_cond_lhs (condstmt);
  op1 = gimple_cond_rhs (condstmt);

  code = gimple_cond_code (condstmt);

  /* If it would be profitable to swap the operands, then do so to
     canonicalize the statement, enabling better optimization.

     By placing canonicalization of such expressions here we
     transparently keep statements in canonical form, even
     when the statement is modified.  */
  if (tree_swap_operands_p (op0, op1, false))
    {
      /* For relationals we need to swap the operands
	 and change the code.  */
      if (code == LT_EXPR
	  || code == GT_EXPR
	  || code == LE_EXPR
	  || code == GE_EXPR)
	{
          code = swap_tree_comparison (code);

          gimple_cond_set_code (condstmt, code);
          gimple_cond_set_lhs (condstmt, op1);
          gimple_cond_set_rhs (condstmt, op0);

          update_stmt (condstmt);
	}
    }
}

/* Use the source/dest pairs in CONST_AND_COPIES_STACK to restore
   CONST_AND_COPIES to its original state, stopping when we hit a
   NULL marker.  */

static void
restore_vars_to_original_value (void)
{
  while (VEC_length (tree, const_and_copies_stack) > 0)
    {
      tree prev_value, dest;

      dest = VEC_pop (tree, const_and_copies_stack);

      if (dest == NULL)
	break;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "<<<< COPY ");
	  print_generic_expr (dump_file, dest, 0);
	  fprintf (dump_file, " = ");
	  print_generic_expr (dump_file, SSA_NAME_VALUE (dest), 0);
	  fprintf (dump_file, "\n");
	}

      prev_value = VEC_pop (tree, const_and_copies_stack);
      set_ssa_name_value (dest, prev_value);
    }
}

/* A trivial wrapper so that we can present the generic jump
   threading code with a simple API for simplifying statements.  */
static tree
simplify_stmt_for_jump_threading (gimple stmt,
				  gimple within_stmt ATTRIBUTE_UNUSED)
{
  return lookup_avail_expr (stmt, false);
}

/* Ignoring loop backedges, if BB has precisely one incoming edge then
   return that edge.  Otherwise return NULL.  */
static edge
single_incoming_edge_ignoring_loop_edges (basic_block bb)
{
  edge retval = NULL;
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      /* A loop back edge can be identified by the destination of
	 the edge dominating the source of the edge.  */
      if (dominated_by_p (CDI_DOMINATORS, e->src, e->dest))
	continue;

      /* If we have already seen a non-loop edge, then we must have
	 multiple incoming non-loop edges and thus we return NULL.  */
      if (retval)
	return NULL;

      /* This is the first non-loop incoming edge we have found.  Record
	 it.  */
      retval = e;
    }

  return retval;
}

/* Dump SSA statistics on FILE.  */

void
dump_dominator_optimization_stats (FILE *file)
{
  fprintf (file, "Total number of statements:                   %6ld\n\n",
	   opt_stats.num_stmts);
  fprintf (file, "Exprs considered for dominator optimizations: %6ld\n",
           opt_stats.num_exprs_considered);

  fprintf (file, "\nHash table statistics:\n");

  fprintf (file, "    avail_exprs: ");
  htab_statistics (file, avail_exprs);
}


/* Dump SSA statistics on stderr.  */

DEBUG_FUNCTION void
debug_dominator_optimization_stats (void)
{
  dump_dominator_optimization_stats (stderr);
}


/* Dump statistics for the hash table HTAB.  */

static void
htab_statistics (FILE *file, htab_t htab)
{
  fprintf (file, "size %ld, %ld elements, %f collision/search ratio\n",
	   (long) htab_size (htab),
	   (long) htab_elements (htab),
	   htab_collisions (htab));
}


/* Enter condition equivalence into the expression hash table.
   This indicates that a conditional expression has a known
   boolean value.  */

static void
record_cond (cond_equivalence *p)
{
  struct expr_hash_elt *element = XCNEW (struct expr_hash_elt);
  void **slot;

  initialize_hash_element_from_expr (&p->cond, p->value, element);

  slot = htab_find_slot_with_hash (avail_exprs, (void *)element,
				   element->hash, INSERT);
  if (*slot == NULL)
    {
      *slot = (void *) element;

      if (dump_file && (dump_flags & TDF_DETAILS))
        {
          fprintf (dump_file, "1>>> ");
          print_expr_hash_elt (dump_file, element);
        }

      VEC_safe_push (expr_hash_elt_t, heap, avail_exprs_stack, element);
    }
  else
    free (element);
}

/* Build a cond_equivalence record indicating that the comparison
   CODE holds between operands OP0 and OP1 and push it to **P.  */

static void
build_and_record_new_cond (enum tree_code code,
                           tree op0, tree op1,
                           VEC(cond_equivalence, heap) **p)
{
  cond_equivalence c;
  struct hashable_expr *cond = &c.cond;

  gcc_assert (TREE_CODE_CLASS (code) == tcc_comparison);

  cond->type = boolean_type_node;
  cond->kind = EXPR_BINARY;
  cond->ops.binary.op = code;
  cond->ops.binary.opnd0 = op0;
  cond->ops.binary.opnd1 = op1;

  c.value = boolean_true_node;
  VEC_safe_push (cond_equivalence, heap, *p, &c);
}

/* Record that COND is true and INVERTED is false into the edge information
   structure.  Also record that any conditions dominated by COND are true
   as well.

   For example, if a < b is true, then a <= b must also be true.  */

static void
record_conditions (struct edge_info *edge_info, tree cond, tree inverted)
{
  tree op0, op1;
  cond_equivalence c;

  if (!COMPARISON_CLASS_P (cond))
    return;

  op0 = TREE_OPERAND (cond, 0);
  op1 = TREE_OPERAND (cond, 1);

  switch (TREE_CODE (cond))
    {
    case LT_EXPR:
    case GT_EXPR:
      if (FLOAT_TYPE_P (TREE_TYPE (op0)))
	{
	  build_and_record_new_cond (ORDERED_EXPR, op0, op1,
				     &edge_info->cond_equivalences);
	  build_and_record_new_cond (LTGT_EXPR, op0, op1,
				     &edge_info->cond_equivalences);
	}

      build_and_record_new_cond ((TREE_CODE (cond) == LT_EXPR
				  ? LE_EXPR : GE_EXPR),
				 op0, op1, &edge_info->cond_equivalences);
      build_and_record_new_cond (NE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      break;

    case GE_EXPR:
    case LE_EXPR:
      if (FLOAT_TYPE_P (TREE_TYPE (op0)))
	{
	  build_and_record_new_cond (ORDERED_EXPR, op0, op1,
				     &edge_info->cond_equivalences);
	}
      break;

    case EQ_EXPR:
      if (FLOAT_TYPE_P (TREE_TYPE (op0)))
	{
	  build_and_record_new_cond (ORDERED_EXPR, op0, op1,
				     &edge_info->cond_equivalences);
	}
      build_and_record_new_cond (LE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      build_and_record_new_cond (GE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      break;

    case UNORDERED_EXPR:
      build_and_record_new_cond (NE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      build_and_record_new_cond (UNLE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      build_and_record_new_cond (UNGE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      build_and_record_new_cond (UNEQ_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      build_and_record_new_cond (UNLT_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      build_and_record_new_cond (UNGT_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      break;

    case UNLT_EXPR:
    case UNGT_EXPR:
      build_and_record_new_cond ((TREE_CODE (cond) == UNLT_EXPR
				  ? UNLE_EXPR : UNGE_EXPR),
				 op0, op1, &edge_info->cond_equivalences);
      build_and_record_new_cond (NE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      break;

    case UNEQ_EXPR:
      build_and_record_new_cond (UNLE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      build_and_record_new_cond (UNGE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      break;

    case LTGT_EXPR:
      build_and_record_new_cond (NE_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      build_and_record_new_cond (ORDERED_EXPR, op0, op1,
				 &edge_info->cond_equivalences);
      break;

    default:
      break;
    }

  /* Now store the original true and false conditions into the first
     two slots.  */
  initialize_expr_from_cond (cond, &c.cond);
  c.value = boolean_true_node;
  VEC_safe_push (cond_equivalence, heap, edge_info->cond_equivalences, &c);

  /* It is possible for INVERTED to be the negation of a comparison,
     and not a valid RHS or GIMPLE_COND condition.  This happens because
     invert_truthvalue may return such an expression when asked to invert
     a floating-point comparison.  These comparisons are not assumed to
     obey the trichotomy law.  */
  initialize_expr_from_cond (inverted, &c.cond);
  c.value = boolean_false_node;
  VEC_safe_push (cond_equivalence, heap, edge_info->cond_equivalences, &c);
}

/* A helper function for record_const_or_copy and record_equality.
   Do the work of recording the value and undo info.  */

static void
record_const_or_copy_1 (tree x, tree y, tree prev_x)
{
  set_ssa_name_value (x, y);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "0>>> COPY ");
      print_generic_expr (dump_file, x, 0);
      fprintf (dump_file, " = ");
      print_generic_expr (dump_file, y, 0);
      fprintf (dump_file, "\n");
    }

  VEC_reserve (tree, heap, const_and_copies_stack, 2);
  VEC_quick_push (tree, const_and_copies_stack, prev_x);
  VEC_quick_push (tree, const_and_copies_stack, x);
}

/* Return the loop depth of the basic block of the defining statement of X.
   This number should not be treated as absolutely correct because the loop
   information may not be completely up-to-date when dom runs.  However, it
   will be relatively correct, and as more passes are taught to keep loop info
   up to date, the result will become more and more accurate.  */

int
loop_depth_of_name (tree x)
{
  gimple defstmt;
  basic_block defbb;

  /* If it's not an SSA_NAME, we have no clue where the definition is.  */
  if (TREE_CODE (x) != SSA_NAME)
    return 0;

  /* Otherwise return the loop depth of the defining statement's bb.
     Note that there may not actually be a bb for this statement, if the
     ssa_name is live on entry.  */
  defstmt = SSA_NAME_DEF_STMT (x);
  defbb = gimple_bb (defstmt);
  if (!defbb)
    return 0;

  return defbb->loop_depth;
}

/* Record that X is equal to Y in const_and_copies.  Record undo
   information in the block-local vector.  */

static void
record_const_or_copy (tree x, tree y)
{
  tree prev_x = SSA_NAME_VALUE (x);

  gcc_assert (TREE_CODE (x) == SSA_NAME);

  if (TREE_CODE (y) == SSA_NAME)
    {
      tree tmp = SSA_NAME_VALUE (y);
      if (tmp)
	y = tmp;
    }

  record_const_or_copy_1 (x, y, prev_x);
}

/* Similarly, but assume that X and Y are the two operands of an EQ_EXPR.
   This constrains the cases in which we may treat this as assignment.  */

static void
record_equality (tree x, tree y)
{
  tree prev_x = NULL, prev_y = NULL;

  if (TREE_CODE (x) == SSA_NAME)
    prev_x = SSA_NAME_VALUE (x);
  if (TREE_CODE (y) == SSA_NAME)
    prev_y = SSA_NAME_VALUE (y);

  /* If one of the previous values is invariant, or invariant in more loops
     (by depth), then use that.
     Otherwise it doesn't matter which value we choose, just so
     long as we canonicalize on one value.  */
  if (is_gimple_min_invariant (y))
    ;
  else if (is_gimple_min_invariant (x)
	   || (loop_depth_of_name (x) <= loop_depth_of_name (y)))
    prev_x = x, x = y, y = prev_x, prev_x = prev_y;
  else if (prev_x && is_gimple_min_invariant (prev_x))
    x = y, y = prev_x, prev_x = prev_y;
  else if (prev_y)
    y = prev_y;

  /* After the swapping, we must have one SSA_NAME.  */
  if (TREE_CODE (x) != SSA_NAME)
    return;

  /* For IEEE, -0.0 == 0.0, so we don't necessarily know the sign of a
     variable compared against zero.  If we're honoring signed zeros,
     then we cannot record this value unless we know that the value is
     nonzero.  */
  if (HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (x)))
      && (TREE_CODE (y) != REAL_CST
	  || REAL_VALUES_EQUAL (dconst0, TREE_REAL_CST (y))))
    return;

  record_const_or_copy_1 (x, y, prev_x);
}

/* Returns true when STMT is a simple iv increment.  It detects the
   following situation:

   i_1 = phi (..., i_2)
   i_2 = i_1 +/- ...  */

static bool
simple_iv_increment_p (gimple stmt)
{
  tree lhs, preinc;
  gimple phi;
  size_t i;

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return false;

  lhs = gimple_assign_lhs (stmt);
  if (TREE_CODE (lhs) != SSA_NAME)
    return false;

  if (gimple_assign_rhs_code (stmt) != PLUS_EXPR
      && gimple_assign_rhs_code (stmt) != MINUS_EXPR)
    return false;

  preinc = gimple_assign_rhs1 (stmt);

  if (TREE_CODE (preinc) != SSA_NAME)
    return false;

  phi = SSA_NAME_DEF_STMT (preinc);
  if (gimple_code (phi) != GIMPLE_PHI)
    return false;

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    if (gimple_phi_arg_def (phi, i) == lhs)
      return true;

  return false;
}

/* We have finished optimizing BB, record any information implied by
   taking a specific outgoing edge from BB.  */

static void
record_edge_info (basic_block bb)
{
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  struct edge_info *edge_info;

  if (! gsi_end_p (gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      location_t loc = gimple_location (stmt);

      if (gimple_code (stmt) == GIMPLE_SWITCH)
	{
	  tree index = gimple_switch_index (stmt);

	  if (TREE_CODE (index) == SSA_NAME)
	    {
	      int i;
              int n_labels = gimple_switch_num_labels (stmt);
	      tree *info = XCNEWVEC (tree, last_basic_block);
	      edge e;
	      edge_iterator ei;

	      for (i = 0; i < n_labels; i++)
		{
		  tree label = gimple_switch_label (stmt, i);
		  basic_block target_bb = label_to_block (CASE_LABEL (label));
		  if (CASE_HIGH (label)
		      || !CASE_LOW (label)
		      || info[target_bb->index])
		    info[target_bb->index] = error_mark_node;
		  else
		    info[target_bb->index] = label;
		}

	      FOR_EACH_EDGE (e, ei, bb->succs)
		{
		  basic_block target_bb = e->dest;
		  tree label = info[target_bb->index];

		  if (label != NULL && label != error_mark_node)
		    {
		      tree x = fold_convert_loc (loc, TREE_TYPE (index),
						 CASE_LOW (label));
		      edge_info = allocate_edge_info (e);
		      edge_info->lhs = index;
		      edge_info->rhs = x;
		    }
		}
	      free (info);
	    }
	}

      /* A COND_EXPR may create equivalences too.  */
      if (gimple_code (stmt) == GIMPLE_COND)
	{
	  edge true_edge;
	  edge false_edge;

          tree op0 = gimple_cond_lhs (stmt);
          tree op1 = gimple_cond_rhs (stmt);
          enum tree_code code = gimple_cond_code (stmt);

	  extract_true_false_edges_from_block (bb, &true_edge, &false_edge);

          /* Special case comparing booleans against a constant as we
             know the value of OP0 on both arms of the branch.  i.e., we
             can record an equivalence for OP0 rather than COND.  */
          if ((code == EQ_EXPR || code == NE_EXPR)
              && TREE_CODE (op0) == SSA_NAME
              && TREE_CODE (TREE_TYPE (op0)) == BOOLEAN_TYPE
              && is_gimple_min_invariant (op1))
            {
              if (code == EQ_EXPR)
                {
                  edge_info = allocate_edge_info (true_edge);
                  edge_info->lhs = op0;
                  edge_info->rhs = (integer_zerop (op1)
                                    ? boolean_false_node
                                    : boolean_true_node);

                  edge_info = allocate_edge_info (false_edge);
                  edge_info->lhs = op0;
                  edge_info->rhs = (integer_zerop (op1)
                                    ? boolean_true_node
                                    : boolean_false_node);
                }
              else
                {
                  edge_info = allocate_edge_info (true_edge);
                  edge_info->lhs = op0;
                  edge_info->rhs = (integer_zerop (op1)
                                    ? boolean_true_node
                                    : boolean_false_node);

                  edge_info = allocate_edge_info (false_edge);
                  edge_info->lhs = op0;
                  edge_info->rhs = (integer_zerop (op1)
                                    ? boolean_false_node
                                    : boolean_true_node);
                }
            }
          else if (is_gimple_min_invariant (op0)
                   && (TREE_CODE (op1) == SSA_NAME
                       || is_gimple_min_invariant (op1)))
            {
              tree cond = build2 (code, boolean_type_node, op0, op1);
              tree inverted = invert_truthvalue_loc (loc, cond);
              struct edge_info *edge_info;

              edge_info = allocate_edge_info (true_edge);
              record_conditions (edge_info, cond, inverted);

              if (code == EQ_EXPR)
                {
                  edge_info->lhs = op1;
                  edge_info->rhs = op0;
                }

              edge_info = allocate_edge_info (false_edge);
              record_conditions (edge_info, inverted, cond);

              if (TREE_CODE (inverted) == EQ_EXPR)
                {
                  edge_info->lhs = op1;
                  edge_info->rhs = op0;
                }
            }

          else if (TREE_CODE (op0) == SSA_NAME
                   && (is_gimple_min_invariant (op1)
                       || TREE_CODE (op1) == SSA_NAME))
            {
              tree cond = build2 (code, boolean_type_node, op0, op1);
              tree inverted = invert_truthvalue_loc (loc, cond);
              struct edge_info *edge_info;

              edge_info = allocate_edge_info (true_edge);
              record_conditions (edge_info, cond, inverted);

              if (code == EQ_EXPR)
                {
                  edge_info->lhs = op0;
                  edge_info->rhs = op1;
                }

              edge_info = allocate_edge_info (false_edge);
              record_conditions (edge_info, inverted, cond);

              if (TREE_CODE (inverted) == EQ_EXPR)
                {
                  edge_info->lhs = op0;
                  edge_info->rhs = op1;
                }
            }
        }

      /* ??? TRUTH_NOT_EXPR can create an equivalence too.  */
    }
}

/* STMT, a GIMPLE_ASSIGN, may create certain equivalences, in either
   the available expressions table or the const_and_copies table.
   Detect and record those equivalences.  */
/* We handle only very simple copy equivalences here.  The heavy
   lifing is done by eliminate_redundant_computations.  */

static void
record_equivalences_from_stmt (gimple stmt, int may_optimize_p)
{
  tree lhs;
  enum tree_code lhs_code;

  gcc_assert (is_gimple_assign (stmt));

  lhs = gimple_assign_lhs (stmt);
  lhs_code = TREE_CODE (lhs);

  if (lhs_code == SSA_NAME
      && gimple_assign_single_p (stmt))
    {
      tree rhs = gimple_assign_rhs1 (stmt);

      /* If the RHS of the assignment is a constant or another variable that
	 may be propagated, register it in the CONST_AND_COPIES table.  We
	 do not need to record unwind data for this, since this is a true
	 assignment and not an equivalence inferred from a comparison.  All
	 uses of this ssa name are dominated by this assignment, so unwinding
	 just costs time and space.  */
      if (may_optimize_p
	  && (TREE_CODE (rhs) == SSA_NAME
	      || is_gimple_min_invariant (rhs)))
      {
	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    fprintf (dump_file, "==== ASGN ");
	    print_generic_expr (dump_file, lhs, 0);
	    fprintf (dump_file, " = ");
	    print_generic_expr (dump_file, rhs, 0);
	    fprintf (dump_file, "\n");
	  }

	set_ssa_name_value (lhs, rhs);
      }
    }

  /* A memory store, even an aliased store, creates a useful
     equivalence.  By exchanging the LHS and RHS, creating suitable
     vops and recording the result in the available expression table,
     we may be able to expose more redundant loads.  */
  if (!gimple_has_volatile_ops (stmt)
      && gimple_references_memory_p (stmt)
      && gimple_assign_single_p (stmt)
      && (TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
	  || is_gimple_min_invariant (gimple_assign_rhs1 (stmt)))
      && !is_gimple_reg (lhs))
    {
      tree rhs = gimple_assign_rhs1 (stmt);
      gimple new_stmt;

      /* Build a new statement with the RHS and LHS exchanged.  */
      if (TREE_CODE (rhs) == SSA_NAME)
        {
          /* NOTE tuples.  The call to gimple_build_assign below replaced
             a call to build_gimple_modify_stmt, which did not set the
             SSA_NAME_DEF_STMT on the LHS of the assignment.  Doing so
             may cause an SSA validation failure, as the LHS may be a
             default-initialized name and should have no definition.  I'm
             a bit dubious of this, as the artificial statement that we
             generate here may in fact be ill-formed, but it is simply
             used as an internal device in this pass, and never becomes
             part of the CFG.  */
          gimple defstmt = SSA_NAME_DEF_STMT (rhs);
          new_stmt = gimple_build_assign (rhs, lhs);
          SSA_NAME_DEF_STMT (rhs) = defstmt;
        }
      else
        new_stmt = gimple_build_assign (rhs, lhs);

      gimple_set_vuse (new_stmt, gimple_vdef (stmt));

      /* Finally enter the statement into the available expression
	 table.  */
      lookup_avail_expr (new_stmt, true);
    }
}

/* Search for an existing instance of STMT in the AVAIL_EXPRS table.
   If found, return its LHS. Otherwise insert STMT in the table and
   return NULL_TREE.

   Also, when an expression is first inserted in the  table, it is also
   is also added to AVAIL_EXPRS_STACK, so that it can be removed when
   we finish processing this block and its children.  */

static tree
lookup_avail_expr (gimple stmt, bool insert)
{
  void **slot;
  tree lhs;
  tree temp;
  struct expr_hash_elt element;

  /* Get LHS of assignment or call, else NULL_TREE.  */
  lhs = gimple_get_lhs (stmt);

  initialize_hash_element (stmt, lhs, &element);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "LKUP ");
      print_expr_hash_elt (dump_file, &element);
    }

  /* Don't bother remembering constant assignments and copy operations.
     Constants and copy operations are handled by the constant/copy propagator
     in optimize_stmt.  */
  if (element.expr.kind == EXPR_SINGLE
      && (TREE_CODE (element.expr.ops.single.rhs) == SSA_NAME
          || is_gimple_min_invariant (element.expr.ops.single.rhs)))
    return NULL_TREE;

  /* Finally try to find the expression in the main expression hash table.  */
  slot = htab_find_slot_with_hash (avail_exprs, &element, element.hash,
				   (insert ? INSERT : NO_INSERT));
  if (slot == NULL)
    return NULL_TREE;

  if (*slot == NULL)
    {
      struct expr_hash_elt *element2 = XNEW (struct expr_hash_elt);
      *element2 = element;
      element2->stamp = element2;
      *slot = (void *) element2;

      if (dump_file && (dump_flags & TDF_DETAILS))
        {
          fprintf (dump_file, "2>>> ");
          print_expr_hash_elt (dump_file, element2);
        }

      VEC_safe_push (expr_hash_elt_t, heap, avail_exprs_stack, element2);
      return NULL_TREE;
    }

  /* Extract the LHS of the assignment so that it can be used as the current
     definition of another variable.  */
  lhs = ((struct expr_hash_elt *)*slot)->lhs;

  /* See if the LHS appears in the CONST_AND_COPIES table.  If it does, then
     use the value from the const_and_copies table.  */
  if (TREE_CODE (lhs) == SSA_NAME)
    {
      temp = SSA_NAME_VALUE (lhs);
      if (temp)
	lhs = temp;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "FIND: ");
      print_generic_expr (dump_file, lhs, 0);
      fprintf (dump_file, "\n");
    }

  return lhs;
}

/* Hashing and equality functions for AVAIL_EXPRS.  We compute a value number
   for expressions using the code of the expression and the SSA numbers of
   its operands.  */

static hashval_t
avail_expr_hash (const void *p)
{
  gimple stmt = ((const struct expr_hash_elt *)p)->stmt;
  const struct hashable_expr *expr = &((const struct expr_hash_elt *)p)->expr;
  tree vuse;
  hashval_t val = 0;

  val = iterative_hash_hashable_expr (expr, val);

  /* If the hash table entry is not associated with a statement, then we
     can just hash the expression and not worry about virtual operands
     and such.  */
  if (!stmt)
    return val;

  /* Add the SSA version numbers of the vuse operand.  This is important
     because compound variables like arrays are not renamed in the
     operands.  Rather, the rename is done on the virtual variable
     representing all the elements of the array.  */
  if ((vuse = gimple_vuse (stmt)))
    val = iterative_hash_expr (vuse, val);

  return val;
}

/* PHI-ONLY copy and constant propagation.  This pass is meant to clean
   up degenerate PHIs created by or exposed by jump threading.  */

/* Given PHI, return its RHS if the PHI is a degenerate, otherwise return
   NULL.  */

tree
degenerate_phi_result (gimple phi)
{
  tree lhs = gimple_phi_result (phi);
  tree val = NULL;
  size_t i;

  /* Ignoring arguments which are the same as LHS, if all the remaining
     arguments are the same, then the PHI is a degenerate and has the
     value of that common argument.  */
  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree arg = gimple_phi_arg_def (phi, i);

      if (arg == lhs)
	continue;
      else if (!arg)
	break;
      else if (!val)
	val = arg;
      else if (arg == val)
	continue;
      /* We bring in some of operand_equal_p not only to speed things
	 up, but also to avoid crashing when dereferencing the type of
	 a released SSA name.  */
      else if (TREE_CODE (val) != TREE_CODE (arg)
	       || TREE_CODE (val) == SSA_NAME
	       || !operand_equal_p (arg, val, 0))
	break;
    }
  return (i == gimple_phi_num_args (phi) ? val : NULL);
}

EXTERN_C_END
