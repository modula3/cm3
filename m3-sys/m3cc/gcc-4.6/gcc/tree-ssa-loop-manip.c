/* Modula-3: modified */

/* High-level loop manipulation functions.
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2010
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
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "tree-pass.h"
#include "cfglayout.h"
#include "tree-scalar-evolution.h"
#include "params.h"
#include "tree-inline.h"
#include "langhooks.h"

EXTERN_C_START

/* Creates an induction variable with value BASE + STEP * iteration in LOOP.
   It is expected that neither BASE nor STEP are shared with other expressions
   (unless the sharing rules allow this).  Use VAR as a base var_decl for it
   (if NULL, a new temporary will be created).  The increment will occur at
   INCR_POS (after it if AFTER is true, before it otherwise).  INCR_POS and
   AFTER can be computed using standard_iv_increment_position.  The ssa versions
   of the variable before and after increment will be stored in VAR_BEFORE and
   VAR_AFTER (unless they are NULL).  */

void
create_iv (tree base, tree step, tree var, struct loop *loop,
	   gimple_stmt_iterator *incr_pos, bool after,
	   tree *var_before, tree *var_after)
{
  gimple stmt;
  tree initial, step1;
  gimple_seq stmts;
  tree vb, va;
  enum tree_code incr_op = PLUS_EXPR;
  edge pe = loop_preheader_edge (loop);

  if (!var)
    {
      var = create_tmp_var (TREE_TYPE (base), "ivtmp");
      add_referenced_var (var);
    }

  vb = make_ssa_name (var, NULL);
  if (var_before)
    *var_before = vb;
  va = make_ssa_name (var, NULL);
  if (var_after)
    *var_after = va;

  /* For easier readability of the created code, produce MINUS_EXPRs
     when suitable.  */
  if (TREE_CODE (step) == INTEGER_CST)
    {
      if (TYPE_UNSIGNED (TREE_TYPE (step)))
	{
	  step1 = fold_build1 (NEGATE_EXPR, TREE_TYPE (step), step);
	  if (tree_int_cst_lt (step1, step))
	    {
	      incr_op = MINUS_EXPR;
	      step = step1;
	    }
	}
      else
	{
	  bool ovf;

	  if (!tree_expr_nonnegative_warnv_p (step, &ovf)
	      && may_negate_without_overflow_p (step))
	    {
	      incr_op = MINUS_EXPR;
	      step = fold_build1 (NEGATE_EXPR, TREE_TYPE (step), step);
	    }
	}
    }
  if (POINTER_TYPE_P (TREE_TYPE (base)))
    {
      if (TREE_CODE (base) == ADDR_EXPR)
	mark_addressable (TREE_OPERAND (base, 0));
      step = fold_convert (sizetype, step);
      if (incr_op == MINUS_EXPR)
	step = fold_build1 (NEGATE_EXPR, sizetype, step);
      incr_op = POINTER_PLUS_EXPR;
    }
  /* Gimplify the step if necessary.  We put the computations in front of the
     loop (i.e. the step should be loop invariant).  */
  step = force_gimple_operand (step, &stmts, true, NULL_TREE);
  if (stmts)
    gsi_insert_seq_on_edge_immediate (pe, stmts);

  stmt = gimple_build_assign_with_ops (incr_op, va, vb, step);
  if (after)
    gsi_insert_after (incr_pos, stmt, GSI_NEW_STMT);
  else
    gsi_insert_before (incr_pos, stmt, GSI_NEW_STMT);

  initial = force_gimple_operand (base, &stmts, true, var);
  if (stmts)
    gsi_insert_seq_on_edge_immediate (pe, stmts);

  stmt = create_phi_node (vb, loop->header);
  SSA_NAME_DEF_STMT (vb) = stmt;
  add_phi_arg (stmt, initial, loop_preheader_edge (loop), UNKNOWN_LOCATION);
  add_phi_arg (stmt, va, loop_latch_edge (loop), UNKNOWN_LOCATION);
}

/* Add exit phis for the USE on EXIT.  */

static void
add_exit_phis_edge (basic_block exit, tree use)
{
  gimple phi, def_stmt = SSA_NAME_DEF_STMT (use);
  basic_block def_bb = gimple_bb (def_stmt);
  struct loop *def_loop;
  edge e;
  edge_iterator ei;

  /* Check that some of the edges entering the EXIT block exits a loop in
     that USE is defined.  */
  FOR_EACH_EDGE (e, ei, exit->preds)
    {
      def_loop = find_common_loop (def_bb->loop_father, e->src->loop_father);
      if (!flow_bb_inside_loop_p (def_loop, e->dest))
	break;
    }

  if (!e)
    return;

  phi = create_phi_node (use, exit);
  create_new_def_for (gimple_phi_result (phi), phi,
		      gimple_phi_result_ptr (phi));
  FOR_EACH_EDGE (e, ei, exit->preds)
    add_phi_arg (phi, use, e, UNKNOWN_LOCATION);
}

/* Add exit phis for VAR that is used in LIVEIN.
   Exits of the loops are stored in EXITS.  */

static void
add_exit_phis_var (tree var, bitmap livein, bitmap exits)
{
  bitmap def;
  unsigned index;
  basic_block def_bb = gimple_bb (SSA_NAME_DEF_STMT (var));
  bitmap_iterator bi;

  if (is_gimple_reg (var))
    bitmap_clear_bit (livein, def_bb->index);
  else
    bitmap_set_bit (livein, def_bb->index);

  def = BITMAP_ALLOC (NULL);
  bitmap_set_bit (def, def_bb->index);
  compute_global_livein (livein, def);
  BITMAP_FREE (def);

  EXECUTE_IF_AND_IN_BITMAP (exits, livein, 0, index, bi)
    {
      add_exit_phis_edge (BASIC_BLOCK (index), var);
    }
}

/* Add exit phis for the names marked in NAMES_TO_RENAME.
   Exits of the loops are stored in EXITS.  Sets of blocks where the ssa
   names are used are stored in USE_BLOCKS.  */

static void
add_exit_phis (bitmap names_to_rename, bitmap *use_blocks, bitmap loop_exits)
{
  unsigned i;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (names_to_rename, 0, i, bi)
    {
      add_exit_phis_var (ssa_name (i), use_blocks[i], loop_exits);
    }
}

/* Returns a bitmap of all loop exit edge targets.  */

static bitmap
get_loops_exits (void)
{
  bitmap exits = BITMAP_ALLOC (NULL);
  basic_block bb;
  edge e;
  edge_iterator ei;

  FOR_EACH_BB (bb)
    {
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (e->src != ENTRY_BLOCK_PTR
	    && !flow_bb_inside_loop_p (e->src->loop_father, bb))
	  {
	    bitmap_set_bit (exits, bb->index);
	    break;
	  }
    }

  return exits;
}

/* For USE in BB, if it is used outside of the loop it is defined in,
   mark it for rewrite.  Record basic block BB where it is used
   to USE_BLOCKS.  Record the ssa name index to NEED_PHIS bitmap.  */

static void
find_uses_to_rename_use (basic_block bb, tree use, bitmap *use_blocks,
			 bitmap need_phis)
{
  unsigned ver;
  basic_block def_bb;
  struct loop *def_loop;

  if (TREE_CODE (use) != SSA_NAME)
    return;

  /* We don't need to keep virtual operands in loop-closed form.  */
  if (!is_gimple_reg (use))
    return;

  ver = SSA_NAME_VERSION (use);
  def_bb = gimple_bb (SSA_NAME_DEF_STMT (use));
  if (!def_bb)
    return;
  def_loop = def_bb->loop_father;

  /* If the definition is not inside a loop, it is not interesting.  */
  if (!loop_outer (def_loop))
    return;

  /* If the use is not outside of the loop it is defined in, it is not
     interesting.  */
  if (flow_bb_inside_loop_p (def_loop, bb))
    return;

  if (!use_blocks[ver])
    use_blocks[ver] = BITMAP_ALLOC (NULL);
  bitmap_set_bit (use_blocks[ver], bb->index);

  bitmap_set_bit (need_phis, ver);
}

/* For uses in STMT, mark names that are used outside of the loop they are
   defined to rewrite.  Record the set of blocks in that the ssa
   names are defined to USE_BLOCKS and the ssa names themselves to
   NEED_PHIS.  */

static void
find_uses_to_rename_stmt (gimple stmt, bitmap *use_blocks, bitmap need_phis)
{
  ssa_op_iter iter;
  tree var;
  basic_block bb = gimple_bb (stmt);

  if (is_gimple_debug (stmt))
    return;

  FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_ALL_USES)
    find_uses_to_rename_use (bb, var, use_blocks, need_phis);
}

/* Marks names that are used in BB and outside of the loop they are
   defined in for rewrite.  Records the set of blocks in that the ssa
   names are defined to USE_BLOCKS.  Record the SSA names that will
   need exit PHIs in NEED_PHIS.  */

static void
find_uses_to_rename_bb (basic_block bb, bitmap *use_blocks, bitmap need_phis)
{
  gimple_stmt_iterator bsi;
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    for (bsi = gsi_start_phis (e->dest); !gsi_end_p (bsi); gsi_next (&bsi))
      find_uses_to_rename_use (bb, PHI_ARG_DEF_FROM_EDGE (gsi_stmt (bsi), e),
			       use_blocks, need_phis);

  for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
    find_uses_to_rename_stmt (gsi_stmt (bsi), use_blocks, need_phis);
}

/* Marks names that are used outside of the loop they are defined in
   for rewrite.  Records the set of blocks in that the ssa
   names are defined to USE_BLOCKS.  If CHANGED_BBS is not NULL,
   scan only blocks in this set.  */

static void
find_uses_to_rename (bitmap changed_bbs, bitmap *use_blocks, bitmap need_phis)
{
  basic_block bb;
  unsigned index;
  bitmap_iterator bi;

  if (changed_bbs && !bitmap_empty_p (changed_bbs))
    {
      EXECUTE_IF_SET_IN_BITMAP (changed_bbs, 0, index, bi)
	{
	  find_uses_to_rename_bb (BASIC_BLOCK (index), use_blocks, need_phis);
	}
    }
  else
    {
      FOR_EACH_BB (bb)
	{
	  find_uses_to_rename_bb (bb, use_blocks, need_phis);
	}
    }
}

/* Rewrites the program into a loop closed ssa form -- i.e. inserts extra
   phi nodes to ensure that no variable is used outside the loop it is
   defined in.

   This strengthening of the basic ssa form has several advantages:

   1) Updating it during unrolling/peeling/versioning is trivial, since
      we do not need to care about the uses outside of the loop.
   2) The behavior of all uses of an induction variable is the same.
      Without this, you need to distinguish the case when the variable
      is used outside of the loop it is defined in, for example

      for (i = 0; i < 100; i++)
	{
	  for (j = 0; j < 100; j++)
	    {
	      k = i + j;
	      use1 (k);
	    }
	  use2 (k);
	}

      Looking from the outer loop with the normal SSA form, the first use of k
      is not well-behaved, while the second one is an induction variable with
      base 99 and step 1.

      If CHANGED_BBS is not NULL, we look for uses outside loops only in
      the basic blocks in this set.

      UPDATE_FLAG is used in the call to update_ssa.  See
      TODO_update_ssa* for documentation.  */

void
rewrite_into_loop_closed_ssa (bitmap changed_bbs, unsigned update_flag)
{
  bitmap loop_exits;
  bitmap *use_blocks;
  unsigned i, old_num_ssa_names;
  bitmap names_to_rename;

  loops_state_set (LOOP_CLOSED_SSA);
  if (number_of_loops () <= 1)
    return;

  loop_exits = get_loops_exits ();
  names_to_rename = BITMAP_ALLOC (NULL);

  /* If the pass has caused the SSA form to be out-of-date, update it
     now.  */
  update_ssa (update_flag);

  old_num_ssa_names = num_ssa_names;
  use_blocks = XCNEWVEC (bitmap, old_num_ssa_names);

  /* Find the uses outside loops.  */
  find_uses_to_rename (changed_bbs, use_blocks, names_to_rename);

  /* Add the PHI nodes on exits of the loops for the names we need to
     rewrite.  */
  add_exit_phis (names_to_rename, use_blocks, loop_exits);

  for (i = 0; i < old_num_ssa_names; i++)
    BITMAP_FREE (use_blocks[i]);
  free (use_blocks);
  BITMAP_FREE (loop_exits);
  BITMAP_FREE (names_to_rename);

  /* Fix up all the names found to be used outside their original
     loops.  */
  update_ssa (TODO_update_ssa);
}

/* Check invariants of the loop closed ssa form for the USE in BB.  */

static void
check_loop_closed_ssa_use (basic_block bb, tree use)
{
  gimple def;
  basic_block def_bb;

  if (TREE_CODE (use) != SSA_NAME || !is_gimple_reg (use))
    return;

  def = SSA_NAME_DEF_STMT (use);
  def_bb = gimple_bb (def);
  gcc_assert (!def_bb
	      || flow_bb_inside_loop_p (def_bb->loop_father, bb));
}

/* Checks invariants of loop closed ssa form in statement STMT in BB.  */

static void
check_loop_closed_ssa_stmt (basic_block bb, gimple stmt)
{
  ssa_op_iter iter;
  tree var;

  if (is_gimple_debug (stmt))
    return;

  FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_ALL_USES)
    check_loop_closed_ssa_use (bb, var);
}

/* Checks that invariants of the loop closed ssa form are preserved.
   Call verify_ssa when VERIFY_SSA_P is true.  */

DEBUG_FUNCTION void
verify_loop_closed_ssa (bool verify_ssa_p)
{
  basic_block bb;
  gimple_stmt_iterator bsi;
  gimple phi;
  edge e;
  edge_iterator ei;

  if (number_of_loops () <= 1)
    return;

  if (verify_ssa_p)
    verify_ssa (false);

  timevar_push (TV_VERIFY_LOOP_CLOSED);

  FOR_EACH_BB (bb)
    {
      for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  phi = gsi_stmt (bsi);
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    check_loop_closed_ssa_use (e->src,
				       PHI_ARG_DEF_FROM_EDGE (phi, e));
	}

      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	check_loop_closed_ssa_stmt (bb, gsi_stmt (bsi));
    }

  timevar_pop (TV_VERIFY_LOOP_CLOSED);
}

/* Split loop exit edge EXIT.  The things are a bit complicated by a need to
   preserve the loop closed ssa form.  The newly created block is returned.  */

basic_block
split_loop_exit_edge (edge exit)
{
  basic_block dest = exit->dest;
  basic_block bb = split_edge (exit);
  gimple phi, new_phi;
  tree new_name, name;
  use_operand_p op_p;
  gimple_stmt_iterator psi;
  source_location locus;

  for (psi = gsi_start_phis (dest); !gsi_end_p (psi); gsi_next (&psi))
    {
      phi = gsi_stmt (psi);
      op_p = PHI_ARG_DEF_PTR_FROM_EDGE (phi, single_succ_edge (bb));
      locus = gimple_phi_arg_location_from_edge (phi, single_succ_edge (bb));

      name = USE_FROM_PTR (op_p);

      /* If the argument of the PHI node is a constant, we do not need
	 to keep it inside loop.  */
      if (TREE_CODE (name) != SSA_NAME)
	continue;

      /* Otherwise create an auxiliary phi node that will copy the value
	 of the SSA name out of the loop.  */
      new_name = duplicate_ssa_name (name, NULL);
      new_phi = create_phi_node (new_name, bb);
      SSA_NAME_DEF_STMT (new_name) = new_phi;
      add_phi_arg (new_phi, name, exit, locus);
      SET_USE (op_p, new_name);
    }

  return bb;
}

/* Returns the basic block in that statements should be emitted for induction
   variables incremented at the end of the LOOP.  */

basic_block
ip_end_pos (struct loop *loop)
{
  return loop->latch;
}

/* Copies phi node arguments for duplicated blocks.  The index of the first
   duplicated block is FIRST_NEW_BLOCK.  */

static void
copy_phi_node_args (unsigned first_new_block)
{
  unsigned i;

  for (i = first_new_block; i < (unsigned) last_basic_block; i++)
    BASIC_BLOCK (i)->flags |= BB_DUPLICATED;

  for (i = first_new_block; i < (unsigned) last_basic_block; i++)
    add_phi_args_after_copy_bb (BASIC_BLOCK (i));

  for (i = first_new_block; i < (unsigned) last_basic_block; i++)
    BASIC_BLOCK (i)->flags &= ~BB_DUPLICATED;
}


/* The same as cfgloopmanip.c:duplicate_loop_to_header_edge, but also
   updates the PHI nodes at start of the copied region.  In order to
   achieve this, only loops whose exits all lead to the same location
   are handled.

   Notice that we do not completely update the SSA web after
   duplication.  The caller is responsible for calling update_ssa
   after the loop has been duplicated.  */

bool
gimple_duplicate_loop_to_header_edge (struct loop *loop, edge e,
				    unsigned int ndupl, sbitmap wont_exit,
				    edge orig, VEC (edge, heap) **to_remove,
				    int flags)
{
  unsigned first_new_block;

  if (!loops_state_satisfies_p (LOOPS_HAVE_SIMPLE_LATCHES))
    return false;
  if (!loops_state_satisfies_p (LOOPS_HAVE_PREHEADERS))
    return false;

#ifdef ENABLE_CHECKING
  if (loops_state_satisfies_p (LOOP_CLOSED_SSA))
    verify_loop_closed_ssa (true);
#endif

  first_new_block = last_basic_block;
  if (!duplicate_loop_to_header_edge (loop, e, ndupl, wont_exit,
				      orig, to_remove, flags))
    return false;

  /* Readd the removed phi args for e.  */
  flush_pending_stmts (e);

  /* Copy the phi node arguments.  */
  copy_phi_node_args (first_new_block);

  scev_reset ();

  return true;
}

EXTERN_C_END
