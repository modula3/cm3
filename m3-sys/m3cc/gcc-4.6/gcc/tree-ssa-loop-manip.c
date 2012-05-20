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
#include "params.h"
#include "tree-inline.h"
#include "langhooks.h"

EXTERN_C_START

void
create_iv (tree base, tree step, tree var, struct loop *loop,
	   gimple_stmt_iterator *incr_pos, bool after,
	   tree *var_before, tree *var_after)
{
  gcc_unreachable ();
}

void
rewrite_into_loop_closed_ssa (bitmap changed_bbs, unsigned update_flag)
{
  gcc_unreachable ();
}

DEBUG_FUNCTION void
verify_loop_closed_ssa (bool verify_ssa_p)
{
  gcc_unreachable ();
}

basic_block
split_loop_exit_edge (edge exit)
{
  gcc_unreachable ();
  return 0;
}

basic_block
ip_end_pos (struct loop *loop)
{
  gcc_unreachable ();
  return loop->latch;
}

/* Returns the basic block in that statements should be emitted for induction
   variables incremented just before exit condition of a LOOP.  */

basic_block
ip_normal_pos (struct loop *loop)
{
  gimple last;
  basic_block bb;
  edge exit;

  gcc_unreachable ();
  if (!single_pred_p (loop->latch))
    return NULL;

  bb = single_pred (loop->latch);
  last = last_stmt (bb);
  if (!last
      || gimple_code (last) != GIMPLE_COND)
    return NULL;

  exit = EDGE_SUCC (bb, 0);
  if (exit->dest == loop->latch)
    exit = EDGE_SUCC (bb, 1);

  if (flow_bb_inside_loop_p (loop, exit->dest))
    return NULL;

  return bb;
}

void
standard_iv_increment_position (struct loop *loop, gimple_stmt_iterator *bsi,
				bool *insert_after)
{
  gcc_unreachable ();
}

bool
gimple_duplicate_loop_to_header_edge (struct loop *loop, edge e,
				    unsigned int ndupl, sbitmap wont_exit,
				    edge orig, VEC (edge, heap) **to_remove,
				    int flags)
{
  gcc_unreachable ();
  return false;
}

bool
can_unroll_loop_p (struct loop *loop, unsigned factor,
		   struct tree_niter_desc *niter)
{
  gcc_unreachable ();
  return false;
}

void
tree_transform_and_unroll_loop (struct loop *loop, unsigned factor,
				edge exit, struct tree_niter_desc *desc,
				transform_callback transform,
				void *data)
{
  gcc_unreachable ();
}

void
tree_unroll_loop (struct loop *loop, unsigned factor,
		  edge exit, struct tree_niter_desc *desc)
{
  gcc_unreachable ();
}

tree
canonicalize_loop_ivs (struct loop *loop, tree *nit, bool bump_in_latch)
{
  gcc_unreachable ();
  return 0;
}

EXTERN_C_END
