/* Modula-3: modified */

/* Control flow optimization code for GNU compiler.
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This file contains optimizer of the control flow.  The main entry point is
   cleanup_cfg.  Following optimizations are performed:

   - Unreachable blocks removal
   - Edge forwarding (edge to the forwarder block is forwarded to its
     successor.  Simplification of the branch instruction is performed by
     underlying infrastructure so branch can be converted to simplejump or
     eliminated).
   - Cross jumping (tail merging)
   - Conditional jump-around-simplejump simplification
   - Basic block merging.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "timevar.h"
#include "output.h"
#include "insn-config.h"
#include "flags.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "cselib.h"
#include "params.h"
#include "tm_p.h"
#include "target.h"
#include "cfglayout.h"
#include "emit-rtl.h"
#include "tree-pass.h"
#include "cfgloop.h"
#include "expr.h"
#include "df.h"
#include "dce.h"
#include "dbgcnt.h"

EXTERN_C_START

#define FORWARDER_BLOCK_P(BB) ((BB)->flags & BB_FORWARDER_BLOCK)

/* Set to true when we are running first pass of try_optimize_cfg loop.  */
static bool first_pass;

/* Set to true if crossjumps occured in the latest run of try_optimize_cfg.  */
static bool crossjumps_occured;

/* Set to true if we couldn't run an optimization due to stale liveness
   information; we should run df_analyze to enable more opportunities.  */
static bool block_was_dirty;

static bool try_crossjump_to_edge (int, edge, edge);
static bool try_crossjump_bb (int, basic_block);
static bool outgoing_edges_match (int, basic_block, basic_block);
static bool old_insns_match_p (int, rtx, rtx);

static void merge_blocks_move_predecessor_nojumps (basic_block, basic_block);
static void merge_blocks_move_successor_nojumps (basic_block, basic_block);
static bool try_optimize_cfg (int);
static bool try_simplify_condjump (basic_block);
static bool try_forward_edges (int, basic_block);
static edge thread_jump (edge, basic_block);
static bool mark_effect (rtx, bitmap);
static void notice_new_block (basic_block);
static void update_forwarder_flag (basic_block);
static int mentions_nonequal_regs (rtx *, void *);
static void merge_memattrs (rtx, rtx);

/* Set flags for newly created block.  */

static void
notice_new_block (basic_block bb)
{
  if (!bb)
    return;

  if (forwarder_block_p (bb))
    bb->flags |= BB_FORWARDER_BLOCK;
}

/* Recompute forwarder flag after block has been modified.  */

static void
update_forwarder_flag (basic_block bb)
{
  if (forwarder_block_p (bb))
    bb->flags |= BB_FORWARDER_BLOCK;
  else
    bb->flags &= ~BB_FORWARDER_BLOCK;
}

/* Simplify a conditional jump around an unconditional jump.
   Return true if something changed.  */

static bool
try_simplify_condjump (basic_block cbranch_block)
{
  basic_block jump_block, jump_dest_block, cbranch_dest_block;
  edge cbranch_jump_edge, cbranch_fallthru_edge;
  rtx cbranch_insn;

  /* Verify that there are exactly two successors.  */
  if (EDGE_COUNT (cbranch_block->succs) != 2)
    return false;

  /* Verify that we've got a normal conditional branch at the end
     of the block.  */
  cbranch_insn = BB_END (cbranch_block);
  if (!any_condjump_p (cbranch_insn))
    return false;

  cbranch_fallthru_edge = FALLTHRU_EDGE (cbranch_block);
  cbranch_jump_edge = BRANCH_EDGE (cbranch_block);

  /* The next block must not have multiple predecessors, must not
     be the last block in the function, and must contain just the
     unconditional jump.  */
  jump_block = cbranch_fallthru_edge->dest;
  if (!single_pred_p (jump_block)
      || jump_block->next_bb == EXIT_BLOCK_PTR
      || !FORWARDER_BLOCK_P (jump_block))
    return false;
  jump_dest_block = single_succ (jump_block);

  /* If we are partitioning hot/cold basic blocks, we don't want to
     mess up unconditional or indirect jumps that cross between hot
     and cold sections.

     Basic block partitioning may result in some jumps that appear to
     be optimizable (or blocks that appear to be mergeable), but which really
     must be left untouched (they are required to make it safely across
     partition boundaries).  See the comments at the top of
     bb-reorder.c:partition_hot_cold_basic_blocks for complete details.  */

  if (BB_PARTITION (jump_block) != BB_PARTITION (jump_dest_block)
      || (cbranch_jump_edge->flags & EDGE_CROSSING))
    return false;

  /* The conditional branch must target the block after the
     unconditional branch.  */
  cbranch_dest_block = cbranch_jump_edge->dest;

  if (cbranch_dest_block == EXIT_BLOCK_PTR
      || !can_fallthru (jump_block, cbranch_dest_block))
    return false;

  /* Invert the conditional branch.  */
  if (!invert_jump (cbranch_insn, block_label (jump_dest_block), 0))
    return false;

  if (dump_file)
    fprintf (dump_file, "Simplifying condjump %i around jump %i\n",
	     INSN_UID (cbranch_insn), INSN_UID (BB_END (jump_block)));

  /* Success.  Update the CFG to match.  Note that after this point
     the edge variable names appear backwards; the redirection is done
     this way to preserve edge profile data.  */
  cbranch_jump_edge = redirect_edge_succ_nodup (cbranch_jump_edge,
						cbranch_dest_block);
  cbranch_fallthru_edge = redirect_edge_succ_nodup (cbranch_fallthru_edge,
						    jump_dest_block);
  cbranch_jump_edge->flags |= EDGE_FALLTHRU;
  cbranch_fallthru_edge->flags &= ~EDGE_FALLTHRU;
  update_br_prob_note (cbranch_block);

  /* Delete the block with the unconditional jump, and clean up the mess.  */
  delete_basic_block (jump_block);
  tidy_fallthru_edge (cbranch_jump_edge);
  update_forwarder_flag (cbranch_block);

  return true;
}

/* Attempt to prove that operation is NOOP using CSElib or mark the effect
   on register.  Used by jump threading.  */

static bool
mark_effect (rtx exp, regset nonequal)
{
  int regno;
  rtx dest;
  switch (GET_CODE (exp))
    {
      /* In case we do clobber the register, mark it as equal, as we know the
	 value is dead so it don't have to match.  */
    case CLOBBER:
      if (REG_P (XEXP (exp, 0)))
	{
	  dest = XEXP (exp, 0);
	  regno = REGNO (dest);
	  CLEAR_REGNO_REG_SET (nonequal, regno);
	  if (regno < FIRST_PSEUDO_REGISTER)
	    {
	      int n = hard_regno_nregs[regno][GET_MODE (dest)];
	      while (--n > 0)
		CLEAR_REGNO_REG_SET (nonequal, regno + n);
	    }
	}
      return false;

    case SET:
      if (rtx_equal_for_cselib_p (SET_DEST (exp), SET_SRC (exp)))
	return false;
      dest = SET_DEST (exp);
      if (dest == pc_rtx)
	return false;
      if (!REG_P (dest))
	return true;
      regno = REGNO (dest);
      SET_REGNO_REG_SET (nonequal, regno);
      if (regno < FIRST_PSEUDO_REGISTER)
	{
	  int n = hard_regno_nregs[regno][GET_MODE (dest)];
	  while (--n > 0)
	    SET_REGNO_REG_SET (nonequal, regno + n);
	}
      return false;

    default:
      return false;
    }
}

/* Return nonzero if X is a register set in regset DATA.
   Called via for_each_rtx.  */
static int
mentions_nonequal_regs (rtx *x, void *data)
{
  regset nonequal = (regset) data;
  if (REG_P (*x))
    {
      int regno;

      regno = REGNO (*x);
      if (REGNO_REG_SET_P (nonequal, regno))
	return 1;
      if (regno < FIRST_PSEUDO_REGISTER)
	{
	  int n = hard_regno_nregs[regno][GET_MODE (*x)];
	  while (--n > 0)
	    if (REGNO_REG_SET_P (nonequal, regno + n))
	      return 1;
	}
    }
  return 0;
}
/* Attempt to prove that the basic block B will have no side effects and
   always continues in the same edge if reached via E.  Return the edge
   if exist, NULL otherwise.  */

static edge
thread_jump (edge e, basic_block b)
{
  gcc_unreachable ();
  return 0;
}

/* Attempt to forward edges leaving basic block B.
   Return true if successful.  */

static bool
try_forward_edges (int mode, basic_block b)
{
  gcc_unreachable ();
  return 0;
}

/* Blocks A and B are to be merged into a single block.  A has no incoming
   fallthru edge, so it can be moved before B without adding or modifying
   any jumps (aside from the jump from A to B).  */

static void
merge_blocks_move_predecessor_nojumps (basic_block a, basic_block b)
{
  gcc_unreachable ();
}

/* Blocks A and B are to be merged into a single block.  B has no outgoing
   fallthru edge, so it can be moved after A without adding or modifying
   any jumps (aside from the jump from A to B).  */

static void
merge_blocks_move_successor_nojumps (basic_block a, basic_block b)
{
  gcc_unreachable ();
}

/* Attempt to merge basic blocks that are potentially non-adjacent.
   Return NULL iff the attempt failed, otherwise return basic block
   where cleanup_cfg should continue.  Because the merging commonly
   moves basic block away or introduces another optimization
   possibility, return basic block just before B so cleanup_cfg don't
   need to iterate.

   It may be good idea to return basic block before C in the case
   C has been moved after B and originally appeared earlier in the
   insn sequence, but we have no information available about the
   relative ordering of these two.  Hopefully it is not too common.  */

static basic_block
merge_blocks_move (edge e, basic_block b, basic_block c, int mode)
{
  gcc_unreachable ();
  return 0;
}

/* Removes the memory attributes of MEM expression
   if they are not equal.  */

void
merge_memattrs (rtx x, rtx y)
{
  gcc_unreachable ();
}

/* Return true if I1 and I2 are equivalent and thus can be crossjumped.  */

static bool
old_insns_match_p (int mode ATTRIBUTE_UNUSED, rtx i1, rtx i2)
{
  gcc_unreachable ();
  return false;
}

/* When comparing insns I1 and I2 in flow_find_cross_jump or
   flow_find_head_matching_sequence, ensure the notes match.  */

static void
merge_notes (rtx i1, rtx i2)
{
  gcc_unreachable ();
}

/* Look through the insns at the end of BB1 and BB2 and find the longest
   sequence that are equivalent.  Store the first insns for that sequence
   in *F1 and *F2 and return the sequence length.

   To simplify callers of this function, if the blocks match exactly,
   store the head of the blocks in *F1 and *F2.  */

int
flow_find_cross_jump (basic_block bb1, basic_block bb2, rtx *f1, rtx *f2)
{
  rtx i1, i2, last1, last2, afterlast1, afterlast2;
  int ninsns = 0;

  /* Skip simple jumps at the end of the blocks.  Complex jumps still
     need to be compared for equivalence, which we'll do below.  */

  i1 = BB_END (bb1);
  last1 = afterlast1 = last2 = afterlast2 = NULL_RTX;
  if (onlyjump_p (i1)
      || (returnjump_p (i1) && !side_effects_p (PATTERN (i1))))
    {
      last1 = i1;
      i1 = PREV_INSN (i1);
    }

  i2 = BB_END (bb2);
  if (onlyjump_p (i2)
      || (returnjump_p (i2) && !side_effects_p (PATTERN (i2))))
    {
      last2 = i2;
      /* Count everything except for unconditional jump as insn.  */
      if (!simplejump_p (i2) && !returnjump_p (i2) && last1)
	ninsns++;
      i2 = PREV_INSN (i2);
    }

  while (true)
    {
      /* Ignore notes.  */
      while (!NONDEBUG_INSN_P (i1) && i1 != BB_HEAD (bb1))
	i1 = PREV_INSN (i1);

      while (!NONDEBUG_INSN_P (i2) && i2 != BB_HEAD (bb2))
	i2 = PREV_INSN (i2);

      if (i1 == BB_HEAD (bb1) || i2 == BB_HEAD (bb2))
	break;

      if (!old_insns_match_p (0, i1, i2))
	break;

      merge_memattrs (i1, i2);

      /* Don't begin a cross-jump with a NOTE insn.  */
      if (INSN_P (i1))
	{
	  merge_notes (i1, i2);

	  afterlast1 = last1, afterlast2 = last2;
	  last1 = i1, last2 = i2;
	  ninsns++;
	}

      i1 = PREV_INSN (i1);
      i2 = PREV_INSN (i2);
    }

#ifdef HAVE_cc0
  /* Don't allow the insn after a compare to be shared by
     cross-jumping unless the compare is also shared.  */
  if (ninsns && reg_mentioned_p (cc0_rtx, last1) && ! sets_cc0_p (last1))
    last1 = afterlast1, last2 = afterlast2, ninsns--;
#endif

  /* Include preceding notes and labels in the cross-jump.  One,
     this may bring us to the head of the blocks as requested above.
     Two, it keeps line number notes as matched as may be.  */
  if (ninsns)
    {
      while (last1 != BB_HEAD (bb1) && !NONDEBUG_INSN_P (PREV_INSN (last1)))
	last1 = PREV_INSN (last1);

      if (last1 != BB_HEAD (bb1) && LABEL_P (PREV_INSN (last1)))
	last1 = PREV_INSN (last1);

      while (last2 != BB_HEAD (bb2) && !NONDEBUG_INSN_P (PREV_INSN (last2)))
	last2 = PREV_INSN (last2);

      if (last2 != BB_HEAD (bb2) && LABEL_P (PREV_INSN (last2)))
	last2 = PREV_INSN (last2);

      *f1 = last1;
      *f2 = last2;
    }

  return ninsns;
}

/* Like flow_find_cross_jump, except start looking for a matching sequence from
   the head of the two blocks.  Do not include jumps at the end.
   If STOP_AFTER is nonzero, stop after finding that many matching
   instructions.  */

int
flow_find_head_matching_sequence (basic_block bb1, basic_block bb2, rtx *f1,
				  rtx *f2, int stop_after)
{
  rtx i1, i2, last1, last2, beforelast1, beforelast2;
  int ninsns = 0;
  edge e;
  edge_iterator ei;
  int nehedges1 = 0, nehedges2 = 0;

  FOR_EACH_EDGE (e, ei, bb1->succs)
    if (e->flags & EDGE_EH)
      nehedges1++;
  FOR_EACH_EDGE (e, ei, bb2->succs)
    if (e->flags & EDGE_EH)
      nehedges2++;

  i1 = BB_HEAD (bb1);
  i2 = BB_HEAD (bb2);
  last1 = beforelast1 = last2 = beforelast2 = NULL_RTX;

  while (true)
    {
      /* Ignore notes, except NOTE_INSN_EPILOGUE_BEG.  */
      while (!NONDEBUG_INSN_P (i1) && i1 != BB_END (bb1))
	{
	  if (NOTE_P (i1) && NOTE_KIND (i1) == NOTE_INSN_EPILOGUE_BEG)
	    break;
	  i1 = NEXT_INSN (i1);
	}

      while (!NONDEBUG_INSN_P (i2) && i2 != BB_END (bb2))
	{
	  if (NOTE_P (i2) && NOTE_KIND (i2) == NOTE_INSN_EPILOGUE_BEG)
	    break;
	  i2 = NEXT_INSN (i2);
	}

      if ((i1 == BB_END (bb1) && !NONDEBUG_INSN_P (i1))
	  || (i2 == BB_END (bb2) && !NONDEBUG_INSN_P (i2)))
	break;

      if (NOTE_P (i1) || NOTE_P (i2)
	  || JUMP_P (i1) || JUMP_P (i2))
	break;

      /* A sanity check to make sure we're not merging insns with different
	 effects on EH.  If only one of them ends a basic block, it shouldn't
	 have an EH edge; if both end a basic block, there should be the same
	 number of EH edges.  */
      if ((i1 == BB_END (bb1) && i2 != BB_END (bb2)
	   && nehedges1 > 0)
	  || (i2 == BB_END (bb2) && i1 != BB_END (bb1)
	      && nehedges2 > 0)
	  || (i1 == BB_END (bb1) && i2 == BB_END (bb2)
	      && nehedges1 != nehedges2))
	break;

      if (!old_insns_match_p (0, i1, i2))
	break;

      merge_memattrs (i1, i2);

      /* Don't begin a cross-jump with a NOTE insn.  */
      if (INSN_P (i1))
	{
	  merge_notes (i1, i2);

	  beforelast1 = last1, beforelast2 = last2;
	  last1 = i1, last2 = i2;
	  ninsns++;
	}

      if (i1 == BB_END (bb1) || i2 == BB_END (bb2)
	  || (stop_after > 0 && ninsns == stop_after))
	break;

      i1 = NEXT_INSN (i1);
      i2 = NEXT_INSN (i2);
    }

#ifdef HAVE_cc0
  /* Don't allow a compare to be shared by cross-jumping unless the insn
     after the compare is also shared.  */
  if (ninsns && reg_mentioned_p (cc0_rtx, last1) && sets_cc0_p (last1))
    last1 = beforelast1, last2 = beforelast2, ninsns--;
#endif

  if (ninsns)
    {
      *f1 = last1;
      *f2 = last2;
    }

  return ninsns;
}

/* Return true iff outgoing edges of BB1 and BB2 match, together with
   the branch instruction.  This means that if we commonize the control
   flow before end of the basic block, the semantic remains unchanged.

   We may assume that there exists one edge with a common destination.  */

static bool
outgoing_edges_match (int mode, basic_block bb1, basic_block bb2)
{
  int nehedges1 = 0, nehedges2 = 0;
  edge fallthru1 = 0, fallthru2 = 0;
  edge e1, e2;
  edge_iterator ei;

  /* If BB1 has only one successor, we may be looking at either an
     unconditional jump, or a fake edge to exit.  */
  if (single_succ_p (bb1)
      && (single_succ_edge (bb1)->flags & (EDGE_COMPLEX | EDGE_FAKE)) == 0
      && (!JUMP_P (BB_END (bb1)) || simplejump_p (BB_END (bb1))))
    return (single_succ_p (bb2)
	    && (single_succ_edge (bb2)->flags
		& (EDGE_COMPLEX | EDGE_FAKE)) == 0
	    && (!JUMP_P (BB_END (bb2)) || simplejump_p (BB_END (bb2))));

  /* Match conditional jumps - this may get tricky when fallthru and branch
     edges are crossed.  */
  if (EDGE_COUNT (bb1->succs) == 2
      && any_condjump_p (BB_END (bb1))
      && onlyjump_p (BB_END (bb1)))
    {
      edge b1, f1, b2, f2;
      bool reverse, match;
      rtx set1, set2, cond1, cond2;
      enum rtx_code code1, code2;

      if (EDGE_COUNT (bb2->succs) != 2
	  || !any_condjump_p (BB_END (bb2))
	  || !onlyjump_p (BB_END (bb2)))
	return false;

      b1 = BRANCH_EDGE (bb1);
      b2 = BRANCH_EDGE (bb2);
      f1 = FALLTHRU_EDGE (bb1);
      f2 = FALLTHRU_EDGE (bb2);

      /* Get around possible forwarders on fallthru edges.  Other cases
	 should be optimized out already.  */
      if (FORWARDER_BLOCK_P (f1->dest))
	f1 = single_succ_edge (f1->dest);

      if (FORWARDER_BLOCK_P (f2->dest))
	f2 = single_succ_edge (f2->dest);

      /* To simplify use of this function, return false if there are
	 unneeded forwarder blocks.  These will get eliminated later
	 during cleanup_cfg.  */
      if (FORWARDER_BLOCK_P (f1->dest)
	  || FORWARDER_BLOCK_P (f2->dest)
	  || FORWARDER_BLOCK_P (b1->dest)
	  || FORWARDER_BLOCK_P (b2->dest))
	return false;

      if (f1->dest == f2->dest && b1->dest == b2->dest)
	reverse = false;
      else if (f1->dest == b2->dest && b1->dest == f2->dest)
	reverse = true;
      else
	return false;

      set1 = pc_set (BB_END (bb1));
      set2 = pc_set (BB_END (bb2));
      if ((XEXP (SET_SRC (set1), 1) == pc_rtx)
	  != (XEXP (SET_SRC (set2), 1) == pc_rtx))
	reverse = !reverse;

      cond1 = XEXP (SET_SRC (set1), 0);
      cond2 = XEXP (SET_SRC (set2), 0);
      code1 = GET_CODE (cond1);
      if (reverse)
	code2 = reversed_comparison_code (cond2, BB_END (bb2));
      else
	code2 = GET_CODE (cond2);

      if (code2 == UNKNOWN)
	return false;

      /* Verify codes and operands match.  */
      match = ((code1 == code2
		&& rtx_renumbered_equal_p (XEXP (cond1, 0), XEXP (cond2, 0))
		&& rtx_renumbered_equal_p (XEXP (cond1, 1), XEXP (cond2, 1)))
	       || (code1 == swap_condition (code2)
		   && rtx_renumbered_equal_p (XEXP (cond1, 1),
					      XEXP (cond2, 0))
		   && rtx_renumbered_equal_p (XEXP (cond1, 0),
					      XEXP (cond2, 1))));

      /* If we return true, we will join the blocks.  Which means that
	 we will only have one branch prediction bit to work with.  Thus
	 we require the existing branches to have probabilities that are
	 roughly similar.  */
      if (match
	  && optimize_bb_for_speed_p (bb1)
	  && optimize_bb_for_speed_p (bb2))
	{
	  int prob2;

	  if (b1->dest == b2->dest)
	    prob2 = b2->probability;
	  else
	    /* Do not use f2 probability as f2 may be forwarded.  */
	    prob2 = REG_BR_PROB_BASE - b2->probability;

	  /* Fail if the difference in probabilities is greater than 50%.
	     This rules out two well-predicted branches with opposite
	     outcomes.  */
	  if (abs (b1->probability - prob2) > REG_BR_PROB_BASE / 2)
	    {
	      if (dump_file)
		fprintf (dump_file,
			 "Outcomes of branch in bb %i and %i differ too much (%i %i)\n",
			 bb1->index, bb2->index, b1->probability, prob2);

	      return false;
	    }
	}

      if (dump_file && match)
	fprintf (dump_file, "Conditionals in bb %i and %i match.\n",
		 bb1->index, bb2->index);

      return match;
    }

  /* Generic case - we are seeing a computed jump, table jump or trapping
     instruction.  */

  /* Check whether there are tablejumps in the end of BB1 and BB2.
     Return true if they are identical.  */
    {
      rtx label1, label2;
      rtx table1, table2;

      if (tablejump_p (BB_END (bb1), &label1, &table1)
	  && tablejump_p (BB_END (bb2), &label2, &table2)
	  && GET_CODE (PATTERN (table1)) == GET_CODE (PATTERN (table2)))
	{
	  /* The labels should never be the same rtx.  If they really are same
	     the jump tables are same too. So disable crossjumping of blocks BB1
	     and BB2 because when deleting the common insns in the end of BB1
	     by delete_basic_block () the jump table would be deleted too.  */
	  /* If LABEL2 is referenced in BB1->END do not do anything
	     because we would loose information when replacing
	     LABEL1 by LABEL2 and then LABEL2 by LABEL1 in BB1->END.  */
	  if (label1 != label2 && !rtx_referenced_p (label2, BB_END (bb1)))
	    {
	      /* Set IDENTICAL to true when the tables are identical.  */
	      bool identical = false;
	      rtx p1, p2;

	      p1 = PATTERN (table1);
	      p2 = PATTERN (table2);
	      if (GET_CODE (p1) == ADDR_VEC && rtx_equal_p (p1, p2))
		{
		  identical = true;
		}
	      else if (GET_CODE (p1) == ADDR_DIFF_VEC
		       && (XVECLEN (p1, 1) == XVECLEN (p2, 1))
		       && rtx_equal_p (XEXP (p1, 2), XEXP (p2, 2))
		       && rtx_equal_p (XEXP (p1, 3), XEXP (p2, 3)))
		{
		  int i;

		  identical = true;
		  for (i = XVECLEN (p1, 1) - 1; i >= 0 && identical; i--)
		    if (!rtx_equal_p (XVECEXP (p1, 1, i), XVECEXP (p2, 1, i)))
		      identical = false;
		}

	      if (identical)
		{
		  replace_label_data rr;
		  bool match;

		  /* Temporarily replace references to LABEL1 with LABEL2
		     in BB1->END so that we could compare the instructions.  */
		  rr.r1 = label1;
		  rr.r2 = label2;
		  rr.update_label_nuses = false;
		  for_each_rtx (&BB_END (bb1), replace_label, &rr);

		  match = old_insns_match_p (mode, BB_END (bb1), BB_END (bb2));
		  if (dump_file && match)
		    fprintf (dump_file,
			     "Tablejumps in bb %i and %i match.\n",
			     bb1->index, bb2->index);

		  /* Set the original label in BB1->END because when deleting
		     a block whose end is a tablejump, the tablejump referenced
		     from the instruction is deleted too.  */
		  rr.r1 = label2;
		  rr.r2 = label1;
		  for_each_rtx (&BB_END (bb1), replace_label, &rr);

		  return match;
		}
	    }
	  return false;
	}
    }

  /* First ensure that the instructions match.  There may be many outgoing
     edges so this test is generally cheaper.  */
  if (!old_insns_match_p (mode, BB_END (bb1), BB_END (bb2)))
    return false;

  /* Search the outgoing edges, ensure that the counts do match, find possible
     fallthru and exception handling edges since these needs more
     validation.  */
  if (EDGE_COUNT (bb1->succs) != EDGE_COUNT (bb2->succs))
    return false;

  FOR_EACH_EDGE (e1, ei, bb1->succs)
    {
      e2 = EDGE_SUCC (bb2, ei.index);

      if (e1->flags & EDGE_EH)
	nehedges1++;

      if (e2->flags & EDGE_EH)
	nehedges2++;

      if (e1->flags & EDGE_FALLTHRU)
	fallthru1 = e1;
      if (e2->flags & EDGE_FALLTHRU)
	fallthru2 = e2;
    }

  /* If number of edges of various types does not match, fail.  */
  if (nehedges1 != nehedges2
      || (fallthru1 != 0) != (fallthru2 != 0))
    return false;

  /* fallthru edges must be forwarded to the same destination.  */
  if (fallthru1)
    {
      basic_block d1 = (forwarder_block_p (fallthru1->dest)
			? single_succ (fallthru1->dest): fallthru1->dest);
      basic_block d2 = (forwarder_block_p (fallthru2->dest)
			? single_succ (fallthru2->dest): fallthru2->dest);

      if (d1 != d2)
	return false;
    }

  /* Ensure the same EH region.  */
  {
    rtx n1 = find_reg_note (BB_END (bb1), REG_EH_REGION, 0);
    rtx n2 = find_reg_note (BB_END (bb2), REG_EH_REGION, 0);

    if (!n1 && n2)
      return false;

    if (n1 && (!n2 || XEXP (n1, 0) != XEXP (n2, 0)))
      return false;
  }

  /* The same checks as in try_crossjump_to_edge. It is required for RTL
     version of sequence abstraction.  */
  FOR_EACH_EDGE (e1, ei, bb2->succs)
    {
      edge e2;
      edge_iterator ei;
      basic_block d1 = e1->dest;

      if (FORWARDER_BLOCK_P (d1))
        d1 = EDGE_SUCC (d1, 0)->dest;

      FOR_EACH_EDGE (e2, ei, bb1->succs)
        {
          basic_block d2 = e2->dest;
          if (FORWARDER_BLOCK_P (d2))
            d2 = EDGE_SUCC (d2, 0)->dest;
          if (d1 == d2)
            break;
        }

      if (!e2)
        return false;
    }

  return true;
}

/* Returns true if BB basic block has a preserve label.  */

static bool
block_has_preserve_label (basic_block bb)
{
  return (bb
          && block_label (bb)
          && LABEL_PRESERVE_P (block_label (bb)));
}

/* E1 and E2 are edges with the same destination block.  Search their
   predecessors for common code.  If found, redirect control flow from
   (maybe the middle of) E1->SRC to (maybe the middle of) E2->SRC.  */

static bool
try_crossjump_to_edge (int mode, edge e1, edge e2)
{
  int nmatch;
  basic_block src1 = e1->src, src2 = e2->src;
  basic_block redirect_to, redirect_from, to_remove;
  rtx newpos1, newpos2;
  edge s;
  edge_iterator ei;

  newpos1 = newpos2 = NULL_RTX;

  /* If we have partitioned hot/cold basic blocks, it is a bad idea
     to try this optimization.

     Basic block partitioning may result in some jumps that appear to
     be optimizable (or blocks that appear to be mergeable), but which really
     must be left untouched (they are required to make it safely across
     partition boundaries).  See the comments at the top of
     bb-reorder.c:partition_hot_cold_basic_blocks for complete details.  */

  if (flag_reorder_blocks_and_partition && reload_completed)
    return false;

  /* Search backward through forwarder blocks.  We don't need to worry
     about multiple entry or chained forwarders, as they will be optimized
     away.  We do this to look past the unconditional jump following a
     conditional jump that is required due to the current CFG shape.  */
  if (single_pred_p (src1)
      && FORWARDER_BLOCK_P (src1))
    e1 = single_pred_edge (src1), src1 = e1->src;

  if (single_pred_p (src2)
      && FORWARDER_BLOCK_P (src2))
    e2 = single_pred_edge (src2), src2 = e2->src;

  /* Nothing to do if we reach ENTRY, or a common source block.  */
  if (src1 == ENTRY_BLOCK_PTR || src2 == ENTRY_BLOCK_PTR)
    return false;
  if (src1 == src2)
    return false;

  /* Seeing more than 1 forwarder blocks would confuse us later...  */
  if (FORWARDER_BLOCK_P (e1->dest)
      && FORWARDER_BLOCK_P (single_succ (e1->dest)))
    return false;

  if (FORWARDER_BLOCK_P (e2->dest)
      && FORWARDER_BLOCK_P (single_succ (e2->dest)))
    return false;

  /* Likewise with dead code (possibly newly created by the other optimizations
     of cfg_cleanup).  */
  if (EDGE_COUNT (src1->preds) == 0 || EDGE_COUNT (src2->preds) == 0)
    return false;

  /* Look for the common insn sequence, part the first ...  */
  if (!outgoing_edges_match (mode, src1, src2))
    return false;

  /* ... and part the second.  */
  nmatch = flow_find_cross_jump (src1, src2, &newpos1, &newpos2);

  /* Don't proceed with the crossjump unless we found a sufficient number
     of matching instructions or the 'from' block was totally matched
     (such that its predecessors will hopefully be redirected and the
     block removed).  */
  if ((nmatch < PARAM_VALUE (PARAM_MIN_CROSSJUMP_INSNS))
      && (newpos1 != BB_HEAD (src1)))
    return false;

  /* Avoid deleting preserve label when redirecting ABNORMAL edges.  */
  if (block_has_preserve_label (e1->dest)
      && (e1->flags & EDGE_ABNORMAL))
    return false;

  /* Here we know that the insns in the end of SRC1 which are common with SRC2
     will be deleted.
     If we have tablejumps in the end of SRC1 and SRC2
     they have been already compared for equivalence in outgoing_edges_match ()
     so replace the references to TABLE1 by references to TABLE2.  */
    {
      rtx label1, label2;
      rtx table1, table2;

      if (tablejump_p (BB_END (src1), &label1, &table1)
	  && tablejump_p (BB_END (src2), &label2, &table2)
	  && label1 != label2)
	{
	  replace_label_data rr;
	  rtx insn;

	  /* Replace references to LABEL1 with LABEL2.  */
	  rr.r1 = label1;
	  rr.r2 = label2;
	  rr.update_label_nuses = true;
	  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	    {
	      /* Do not replace the label in SRC1->END because when deleting
		 a block whose end is a tablejump, the tablejump referenced
		 from the instruction is deleted too.  */
	      if (insn != BB_END (src1))
		for_each_rtx (&insn, replace_label, &rr);
	    }
	}
    }

  /* Avoid splitting if possible.  We must always split when SRC2 has
     EH predecessor edges, or we may end up with basic blocks with both
     normal and EH predecessor edges.  */
  if (newpos2 == BB_HEAD (src2)
      && !(EDGE_PRED (src2, 0)->flags & EDGE_EH))
    redirect_to = src2;
  else
    {
      if (newpos2 == BB_HEAD (src2))
	{
	  /* Skip possible basic block header.  */
	  if (LABEL_P (newpos2))
	    newpos2 = NEXT_INSN (newpos2);
	  while (DEBUG_INSN_P (newpos2))
	    newpos2 = NEXT_INSN (newpos2);
	  if (NOTE_P (newpos2))
	    newpos2 = NEXT_INSN (newpos2);
	  while (DEBUG_INSN_P (newpos2))
	    newpos2 = NEXT_INSN (newpos2);
	}

      if (dump_file)
	fprintf (dump_file, "Splitting bb %i before %i insns\n",
		 src2->index, nmatch);
      redirect_to = split_block (src2, PREV_INSN (newpos2))->dest;
    }

  if (dump_file)
    fprintf (dump_file,
	     "Cross jumping from bb %i to bb %i; %i common insns\n",
	     src1->index, src2->index, nmatch);

  /* We may have some registers visible through the block.  */
  df_set_bb_dirty (redirect_to);

  /* Recompute the frequencies and counts of outgoing edges.  */
  FOR_EACH_EDGE (s, ei, redirect_to->succs)
    {
      edge s2;
      edge_iterator ei;
      basic_block d = s->dest;

      if (FORWARDER_BLOCK_P (d))
	d = single_succ (d);

      FOR_EACH_EDGE (s2, ei, src1->succs)
	{
	  basic_block d2 = s2->dest;
	  if (FORWARDER_BLOCK_P (d2))
	    d2 = single_succ (d2);
	  if (d == d2)
	    break;
	}

      s->count += s2->count;

      /* Take care to update possible forwarder blocks.  We verified
	 that there is no more than one in the chain, so we can't run
	 into infinite loop.  */
      if (FORWARDER_BLOCK_P (s->dest))
	{
	  single_succ_edge (s->dest)->count += s2->count;
	  s->dest->count += s2->count;
	  s->dest->frequency += EDGE_FREQUENCY (s);
	}

      if (FORWARDER_BLOCK_P (s2->dest))
	{
	  single_succ_edge (s2->dest)->count -= s2->count;
	  if (single_succ_edge (s2->dest)->count < 0)
	    single_succ_edge (s2->dest)->count = 0;
	  s2->dest->count -= s2->count;
	  s2->dest->frequency -= EDGE_FREQUENCY (s);
	  if (s2->dest->frequency < 0)
	    s2->dest->frequency = 0;
	  if (s2->dest->count < 0)
	    s2->dest->count = 0;
	}

      if (!redirect_to->frequency && !src1->frequency)
	s->probability = (s->probability + s2->probability) / 2;
      else
	s->probability
	  = ((s->probability * redirect_to->frequency +
	      s2->probability * src1->frequency)
	     / (redirect_to->frequency + src1->frequency));
    }

  /* Adjust count and frequency for the block.  An earlier jump
     threading pass may have left the profile in an inconsistent
     state (see update_bb_profile_for_threading) so we must be
     prepared for overflows.  */
  redirect_to->count += src1->count;
  redirect_to->frequency += src1->frequency;
  if (redirect_to->frequency > BB_FREQ_MAX)
    redirect_to->frequency = BB_FREQ_MAX;
  update_br_prob_note (redirect_to);

  /* Edit SRC1 to go to REDIRECT_TO at NEWPOS1.  */

  /* Skip possible basic block header.  */
  if (LABEL_P (newpos1))
    newpos1 = NEXT_INSN (newpos1);

  while (DEBUG_INSN_P (newpos1))
    newpos1 = NEXT_INSN (newpos1);

  if (NOTE_INSN_BASIC_BLOCK_P (newpos1))
    newpos1 = NEXT_INSN (newpos1);

  while (DEBUG_INSN_P (newpos1))
    newpos1 = NEXT_INSN (newpos1);

  redirect_from = split_block (src1, PREV_INSN (newpos1))->src;
  to_remove = single_succ (redirect_from);

  redirect_edge_and_branch_force (single_succ_edge (redirect_from), redirect_to);
  delete_basic_block (to_remove);

  update_forwarder_flag (redirect_from);
  if (redirect_to != src2)
    update_forwarder_flag (src2);

  return true;
}

/* Search the predecessors of BB for common insn sequences.  When found,
   share code between them by redirecting control flow.  Return true if
   any changes made.  */

static bool
try_crossjump_bb (int mode, basic_block bb)
{
  edge e, e2, fallthru;
  bool changed;
  unsigned max, ix, ix2;
  basic_block ev, ev2;

  /* Nothing to do if there is not at least two incoming edges.  */
  if (EDGE_COUNT (bb->preds) < 2)
    return false;

  /* Don't crossjump if this block ends in a computed jump,
     unless we are optimizing for size.  */
  if (optimize_bb_for_size_p (bb)
      && bb != EXIT_BLOCK_PTR
      && computed_jump_p (BB_END (bb)))
    return false;

  /* If we are partitioning hot/cold basic blocks, we don't want to
     mess up unconditional or indirect jumps that cross between hot
     and cold sections.

     Basic block partitioning may result in some jumps that appear to
     be optimizable (or blocks that appear to be mergeable), but which really
     must be left untouched (they are required to make it safely across
     partition boundaries).  See the comments at the top of
     bb-reorder.c:partition_hot_cold_basic_blocks for complete details.  */

  if (BB_PARTITION (EDGE_PRED (bb, 0)->src) !=
					BB_PARTITION (EDGE_PRED (bb, 1)->src)
      || (EDGE_PRED (bb, 0)->flags & EDGE_CROSSING))
    return false;

  /* It is always cheapest to redirect a block that ends in a branch to
     a block that falls through into BB, as that adds no branches to the
     program.  We'll try that combination first.  */
  fallthru = NULL;
  max = PARAM_VALUE (PARAM_MAX_CROSSJUMP_EDGES);

  if (EDGE_COUNT (bb->preds) > max)
    return false;

  fallthru = find_fallthru_edge (bb->preds);

  changed = false;
  for (ix = 0, ev = bb; ix < EDGE_COUNT (ev->preds); )
    {
      e = EDGE_PRED (ev, ix);
      ix++;

      /* As noted above, first try with the fallthru predecessor (or, a
	 fallthru predecessor if we are in cfglayout mode).  */
      if (fallthru)
	{
	  /* Don't combine the fallthru edge into anything else.
	     If there is a match, we'll do it the other way around.  */
	  if (e == fallthru)
	    continue;
	  /* If nothing changed since the last attempt, there is nothing
	     we can do.  */
	  if (!first_pass
	      && !((e->src->flags & BB_MODIFIED)
		   || (fallthru->src->flags & BB_MODIFIED)))
	    continue;

	  if (try_crossjump_to_edge (mode, e, fallthru))
	    {
	      changed = true;
	      ix = 0;
	      ev = bb;
	      continue;
	    }
	}

      /* Non-obvious work limiting check: Recognize that we're going
	 to call try_crossjump_bb on every basic block.  So if we have
	 two blocks with lots of outgoing edges (a switch) and they
	 share lots of common destinations, then we would do the
	 cross-jump check once for each common destination.

	 Now, if the blocks actually are cross-jump candidates, then
	 all of their destinations will be shared.  Which means that
	 we only need check them for cross-jump candidacy once.  We
	 can eliminate redundant checks of crossjump(A,B) by arbitrarily
	 choosing to do the check from the block for which the edge
	 in question is the first successor of A.  */
      if (EDGE_SUCC (e->src, 0) != e)
	continue;

      for (ix2 = 0, ev2 = bb; ix2 < EDGE_COUNT (ev2->preds); )
	{
	  e2 = EDGE_PRED (ev2, ix2);
	  ix2++;

	  if (e2 == e)
	    continue;

	  /* We've already checked the fallthru edge above.  */
	  if (e2 == fallthru)
	    continue;

	  /* The "first successor" check above only prevents multiple
	     checks of crossjump(A,B).  In order to prevent redundant
	     checks of crossjump(B,A), require that A be the block
	     with the lowest index.  */
	  if (e->src->index > e2->src->index)
	    continue;

	  /* If nothing changed since the last attempt, there is nothing
	     we can do.  */
	  if (!first_pass
	      && !((e->src->flags & BB_MODIFIED)
		   || (e2->src->flags & BB_MODIFIED)))
	    continue;

	  if (try_crossjump_to_edge (mode, e, e2))
	    {
	      changed = true;
	      ev2 = bb;
	      ix = 0;
	      break;
	    }
	}
    }

  if (changed)
    crossjumps_occured = true;

  return changed;
}

/* Search the successors of BB for common insn sequences.  When found,
   share code between them by moving it across the basic block
   boundary.  Return true if any changes made.  */

static bool
try_head_merge_bb (basic_block bb)
{
  basic_block final_dest_bb = NULL;
  int max_match = INT_MAX;
  edge e0;
  rtx *headptr, *currptr, *nextptr;
  bool changed, moveall;
  unsigned ix;
  rtx e0_last_head, cond, move_before;
  unsigned nedges = EDGE_COUNT (bb->succs);
  rtx jump = BB_END (bb);
  regset live, live_union;

  /* Nothing to do if there is not at least two outgoing edges.  */
  if (nedges < 2)
    return false;

  /* Don't crossjump if this block ends in a computed jump,
     unless we are optimizing for size.  */
  if (optimize_bb_for_size_p (bb)
      && bb != EXIT_BLOCK_PTR
      && computed_jump_p (BB_END (bb)))
    return false;

  cond = get_condition (jump, &move_before, true, false);
  if (cond == NULL_RTX)
    move_before = jump;

  for (ix = 0; ix < nedges; ix++)
    if (EDGE_SUCC (bb, ix)->dest == EXIT_BLOCK_PTR)
      return false;

  for (ix = 0; ix < nedges; ix++)
    {
      edge e = EDGE_SUCC (bb, ix);
      basic_block other_bb = e->dest;

      if (df_get_bb_dirty (other_bb))
	{
	  block_was_dirty = true;
	  return false;
	}

      if (e->flags & EDGE_ABNORMAL)
	return false;

      /* Normally, all destination blocks must only be reachable from this
	 block, i.e. they must have one incoming edge.

	 There is one special case we can handle, that of multiple consecutive
	 jumps where the first jumps to one of the targets of the second jump.
	 This happens frequently in switch statements for default labels.
	 The structure is as follows:
	 FINAL_DEST_BB
	 ....
	 if (cond) jump A;
	 fall through
	 BB
	 jump with targets A, B, C, D...
	 A
	 has two incoming edges, from FINAL_DEST_BB and BB

	 In this case, we can try to move the insns through BB and into
	 FINAL_DEST_BB.  */
      if (EDGE_COUNT (other_bb->preds) != 1)
	{
	  edge incoming_edge, incoming_bb_other_edge;
	  edge_iterator ei;

	  if (final_dest_bb != NULL
	      || EDGE_COUNT (other_bb->preds) != 2)
	    return false;

	  /* We must be able to move the insns across the whole block.  */
	  move_before = BB_HEAD (bb);
	  while (!NONDEBUG_INSN_P (move_before))
	    move_before = NEXT_INSN (move_before);

	  if (EDGE_COUNT (bb->preds) != 1)
	    return false;
	  incoming_edge = EDGE_PRED (bb, 0);
	  final_dest_bb = incoming_edge->src;
	  if (EDGE_COUNT (final_dest_bb->succs) != 2)
	    return false;
	  FOR_EACH_EDGE (incoming_bb_other_edge, ei, final_dest_bb->succs)
	    if (incoming_bb_other_edge != incoming_edge)
	      break;
	  if (incoming_bb_other_edge->dest != other_bb)
	    return false;
	}
    }

  e0 = EDGE_SUCC (bb, 0);
  e0_last_head = NULL_RTX;
  changed = false;

  for (ix = 1; ix < nedges; ix++)
    {
      edge e = EDGE_SUCC (bb, ix);
      rtx e0_last, e_last;
      int nmatch;

      nmatch = flow_find_head_matching_sequence (e0->dest, e->dest,
						 &e0_last, &e_last, 0);
      if (nmatch == 0)
	return false;

      if (nmatch < max_match)
	{
	  max_match = nmatch;
	  e0_last_head = e0_last;
	}
    }

  /* If we matched an entire block, we probably have to avoid moving the
     last insn.  */
  if (max_match > 0
      && e0_last_head == BB_END (e0->dest)
      && (find_reg_note (e0_last_head, REG_EH_REGION, 0)
	  || control_flow_insn_p (e0_last_head)))
    {
      max_match--;
      if (max_match == 0)
	return false;
      do
	e0_last_head = prev_real_insn (e0_last_head);
      while (DEBUG_INSN_P (e0_last_head));
    }

  if (max_match == 0)
    return false;

  /* We must find a union of the live registers at each of the end points.  */
  live = BITMAP_ALLOC (NULL);
  live_union = BITMAP_ALLOC (NULL);

  currptr = XNEWVEC (rtx, nedges);
  headptr = XNEWVEC (rtx, nedges);
  nextptr = XNEWVEC (rtx, nedges);

  for (ix = 0; ix < nedges; ix++)
    {
      int j;
      basic_block merge_bb = EDGE_SUCC (bb, ix)->dest;
      rtx head = BB_HEAD (merge_bb);

      while (!NONDEBUG_INSN_P (head))
	head = NEXT_INSN (head);
      headptr[ix] = head;
      currptr[ix] = head;

      /* Compute the end point and live information  */
      for (j = 1; j < max_match; j++)
	do
	  head = NEXT_INSN (head);
	while (!NONDEBUG_INSN_P (head));
      simulate_backwards_to_point (merge_bb, live, head);
      IOR_REG_SET (live_union, live);
    }

  /* If we're moving across two blocks, verify the validity of the
     first move, then adjust the target and let the loop below deal
     with the final move.  */
  if (final_dest_bb != NULL)
    {
      rtx move_upto;

      moveall = can_move_insns_across (currptr[0], e0_last_head, move_before,
				       jump, e0->dest, live_union,
				       NULL, &move_upto);
      if (!moveall)
	{
	  if (move_upto == NULL_RTX)
	    goto out;

	  while (e0_last_head != move_upto)
	    {
	      df_simulate_one_insn_backwards (e0->dest, e0_last_head,
					      live_union);
	      e0_last_head = PREV_INSN (e0_last_head);
	    }
	}
      if (e0_last_head == NULL_RTX)
	goto out;

      jump = BB_END (final_dest_bb);
      cond = get_condition (jump, &move_before, true, false);
      if (cond == NULL_RTX)
	move_before = jump;
    }

  do
    {
      rtx move_upto;
      moveall = can_move_insns_across (currptr[0], e0_last_head,
				       move_before, jump, e0->dest, live_union,
				       NULL, &move_upto);
      if (!moveall && move_upto == NULL_RTX)
	{
	  if (jump == move_before)
	    break;

	  /* Try again, using a different insertion point.  */
	  move_before = jump;

#ifdef HAVE_cc0
	  /* Don't try moving before a cc0 user, as that may invalidate
	     the cc0.  */
	  if (reg_mentioned_p (cc0_rtx, jump))
	    break;
#endif

	  continue;
	}

      if (final_dest_bb && !moveall)
	/* We haven't checked whether a partial move would be OK for the first
	   move, so we have to fail this case.  */
	break;

      changed = true;
      for (;;)
	{
	  if (currptr[0] == move_upto)
	    break;
	  for (ix = 0; ix < nedges; ix++)
	    {
	      rtx curr = currptr[ix];
	      do
		curr = NEXT_INSN (curr);
	      while (!NONDEBUG_INSN_P (curr));
	      currptr[ix] = curr;
	    }
	}

      /* If we can't currently move all of the identical insns, remember
	 each insn after the range that we'll merge.  */
      if (!moveall)
	for (ix = 0; ix < nedges; ix++)
	  {
	    rtx curr = currptr[ix];
	    do
	      curr = NEXT_INSN (curr);
	    while (!NONDEBUG_INSN_P (curr));
	    nextptr[ix] = curr;
	  }

      reorder_insns (headptr[0], currptr[0], PREV_INSN (move_before));
      df_set_bb_dirty (EDGE_SUCC (bb, 0)->dest);
      if (final_dest_bb != NULL)
	df_set_bb_dirty (final_dest_bb);
      df_set_bb_dirty (bb);
      for (ix = 1; ix < nedges; ix++)
	{
	  df_set_bb_dirty (EDGE_SUCC (bb, ix)->dest);
	  delete_insn_chain (headptr[ix], currptr[ix], false);
	}
      if (!moveall)
	{
	  if (jump == move_before)
	    break;

	  /* For the unmerged insns, try a different insertion point.  */
	  move_before = jump;

#ifdef HAVE_cc0
	  /* Don't try moving before a cc0 user, as that may invalidate
	     the cc0.  */
	  if (reg_mentioned_p (cc0_rtx, jump))
	    break;
#endif

	  for (ix = 0; ix < nedges; ix++)
	    currptr[ix] = headptr[ix] = nextptr[ix];
	}
    }
  while (!moveall);

 out:
  free (currptr);
  free (headptr);
  free (nextptr);

  crossjumps_occured |= changed;

  return changed;
}

/* Return true if BB contains just bb note, or bb note followed
   by only DEBUG_INSNs.  */

static bool
trivially_empty_bb_p (basic_block bb)
{
  rtx insn = BB_END (bb);

  while (1)
    {
      if (insn == BB_HEAD (bb))
	return true;
      if (!DEBUG_INSN_P (insn))
	return false;
      insn = PREV_INSN (insn);
    }
}

/* Do simple CFG optimizations - basic block merging, simplifying of jump
   instructions etc.  Return nonzero if changes were made.  */

static bool
try_optimize_cfg (int mode)
{
  bool changed_overall = false;
  bool changed;
  int iterations = 0;
  basic_block bb, b, next;

  if (mode & (CLEANUP_CROSSJUMP | CLEANUP_THREADING))
    clear_bb_flags ();

  crossjumps_occured = false;

  FOR_EACH_BB (bb)
    update_forwarder_flag (bb);

  if (! targetm.cannot_modify_jumps_p ())
    {
      first_pass = true;
      /* Attempt to merge blocks as made possible by edge removal.  If
	 a block has only one successor, and the successor has only
	 one predecessor, they may be combined.  */
      do
	{
	  block_was_dirty = false;
	  changed = false;
	  iterations++;

	  if (dump_file)
	    fprintf (dump_file,
		     "\n\ntry_optimize_cfg iteration %i\n\n",
		     iterations);

	  for (b = ENTRY_BLOCK_PTR->next_bb; b != EXIT_BLOCK_PTR;)
	    {
	      basic_block c;
	      edge s;
	      bool changed_here = false;

	      /* Delete trivially dead basic blocks.  This is either
		 blocks with no predecessors, or empty blocks with no
		 successors.  However if the empty block with no
		 successors is the successor of the ENTRY_BLOCK, it is
		 kept.  This ensures that the ENTRY_BLOCK will have a
		 successor which is a precondition for many RTL
		 passes.  Empty blocks may result from expanding
		 __builtin_unreachable ().  */
	      if (EDGE_COUNT (b->preds) == 0
		  || (EDGE_COUNT (b->succs) == 0
		      && trivially_empty_bb_p (b)
		      && single_succ_edge (ENTRY_BLOCK_PTR)->dest != b))
		{
		  c = b->prev_bb;
		  if (EDGE_COUNT (b->preds) > 0)
		    {
		      edge e;
		      edge_iterator ei;

		      if (current_ir_type () == IR_RTL_CFGLAYOUT)
			{
			  if (b->il.rtl->footer
			      && BARRIER_P (b->il.rtl->footer))
			    FOR_EACH_EDGE (e, ei, b->preds)
			      if ((e->flags & EDGE_FALLTHRU)
				  && e->src->il.rtl->footer == NULL)
				{
				  if (b->il.rtl->footer)
				    {
				      e->src->il.rtl->footer = b->il.rtl->footer;
				      b->il.rtl->footer = NULL;
				    }
				  else
				    {
				      start_sequence ();
				      e->src->il.rtl->footer = emit_barrier ();
				      end_sequence ();
				    }
				}
			}
		      else
			{
			  rtx last = get_last_bb_insn (b);
			  if (last && BARRIER_P (last))
			    FOR_EACH_EDGE (e, ei, b->preds)
			      if ((e->flags & EDGE_FALLTHRU))
				emit_barrier_after (BB_END (e->src));
			}
		    }
		  delete_basic_block (b);
		  changed = true;
		  /* Avoid trying to remove ENTRY_BLOCK_PTR.  */
		  b = (c == ENTRY_BLOCK_PTR ? c->next_bb : c);
		  continue;
		}

	      /* Remove code labels no longer used.  */
	      if (single_pred_p (b)
		  && (single_pred_edge (b)->flags & EDGE_FALLTHRU)
		  && !(single_pred_edge (b)->flags & EDGE_COMPLEX)
		  && LABEL_P (BB_HEAD (b))
		  /* If the previous block ends with a branch to this
		     block, we can't delete the label.  Normally this
		     is a condjump that is yet to be simplified, but
		     if CASE_DROPS_THRU, this can be a tablejump with
		     some element going to the same place as the
		     default (fallthru).  */
		  && (single_pred (b) == ENTRY_BLOCK_PTR
		      || !JUMP_P (BB_END (single_pred (b)))
		      || ! label_is_jump_target_p (BB_HEAD (b),
						   BB_END (single_pred (b)))))
		{
		  rtx label = BB_HEAD (b);

		  delete_insn_chain (label, label, false);
		  /* If the case label is undeletable, move it after the
		     BASIC_BLOCK note.  */
		  if (NOTE_KIND (BB_HEAD (b)) == NOTE_INSN_DELETED_LABEL)
		    {
		      rtx bb_note = NEXT_INSN (BB_HEAD (b));

		      reorder_insns_nobb (label, label, bb_note);
		      BB_HEAD (b) = bb_note;
		      if (BB_END (b) == bb_note)
			BB_END (b) = label;
		    }
		  if (dump_file)
		    fprintf (dump_file, "Deleted label in block %i.\n",
			     b->index);
		}

	      /* If we fall through an empty block, we can remove it.  */
	      if (!(mode & CLEANUP_CFGLAYOUT)
		  && single_pred_p (b)
		  && (single_pred_edge (b)->flags & EDGE_FALLTHRU)
		  && !LABEL_P (BB_HEAD (b))
		  && FORWARDER_BLOCK_P (b)
		  /* Note that forwarder_block_p true ensures that
		     there is a successor for this block.  */
		  && (single_succ_edge (b)->flags & EDGE_FALLTHRU)
		  && n_basic_blocks > NUM_FIXED_BLOCKS + 1)
		{
		  if (dump_file)
		    fprintf (dump_file,
			     "Deleting fallthru block %i.\n",
			     b->index);

		  c = b->prev_bb == ENTRY_BLOCK_PTR ? b->next_bb : b->prev_bb;
		  redirect_edge_succ_nodup (single_pred_edge (b),
					    single_succ (b));
		  delete_basic_block (b);
		  changed = true;
		  b = c;
		  continue;
		}

	      /* Merge B with its single successor, if any.  */
	      if (single_succ_p (b)
		  && (s = single_succ_edge (b))
		  && !(s->flags & EDGE_COMPLEX)
		  && (c = s->dest) != EXIT_BLOCK_PTR
		  && single_pred_p (c)
		  && b != c)
		{
		  /* When not in cfg_layout mode use code aware of reordering
		     INSN.  This code possibly creates new basic blocks so it
		     does not fit merge_blocks interface and is kept here in
		     hope that it will become useless once more of compiler
		     is transformed to use cfg_layout mode.  */

		  if ((mode & CLEANUP_CFGLAYOUT)
		      && can_merge_blocks_p (b, c))
		    {
		      merge_blocks (b, c);
		      update_forwarder_flag (b);
		      changed_here = true;
		    }
		  else if (!(mode & CLEANUP_CFGLAYOUT)
			   /* If the jump insn has side effects,
			      we can't kill the edge.  */
			   && (!JUMP_P (BB_END (b))
			       || (reload_completed
				   ? simplejump_p (BB_END (b))
				   : (onlyjump_p (BB_END (b))
				      && !tablejump_p (BB_END (b),
						       NULL, NULL))))
			   && (next = merge_blocks_move (s, b, c, mode)))
		      {
			b = next;
			changed_here = true;
		      }
		}

	      /* Simplify branch over branch.  */
	      if ((mode & CLEANUP_EXPENSIVE)
		   && !(mode & CLEANUP_CFGLAYOUT)
		   && try_simplify_condjump (b))
		changed_here = true;

	      /* If B has a single outgoing edge, but uses a
		 non-trivial jump instruction without side-effects, we
		 can either delete the jump entirely, or replace it
		 with a simple unconditional jump.  */
	      if (single_succ_p (b)
		  && single_succ (b) != EXIT_BLOCK_PTR
		  && onlyjump_p (BB_END (b))
		  && !find_reg_note (BB_END (b), REG_CROSSING_JUMP, NULL_RTX)
		  && try_redirect_by_replacing_jump (single_succ_edge (b),
						     single_succ (b),
						     (mode & CLEANUP_CFGLAYOUT) != 0))
		{
		  update_forwarder_flag (b);
		  changed_here = true;
		}

	      /* Simplify branch to branch.  */
	      if (try_forward_edges (mode, b))
		changed_here = true;

	      /* Look for shared code between blocks.  */
	      if ((mode & CLEANUP_CROSSJUMP)
		  && try_crossjump_bb (mode, b))
		changed_here = true;

	      if ((mode & CLEANUP_CROSSJUMP)
		  /* This can lengthen register lifetimes.  Do it only after
		     reload.  */
		  && reload_completed
		  && try_head_merge_bb (b))
		changed_here = true;

	      /* Don't get confused by the index shift caused by
		 deleting blocks.  */
	      if (!changed_here)
		b = b->next_bb;
	      else
		changed = true;
	    }

	  if ((mode & CLEANUP_CROSSJUMP)
	      && try_crossjump_bb (mode, EXIT_BLOCK_PTR))
	    changed = true;

	  if (block_was_dirty)
	    {
	      /* This should only be set by head-merging.  */
	      gcc_assert (mode & CLEANUP_CROSSJUMP);
	      df_analyze ();
	    }

#ifdef ENABLE_CHECKING
	  if (changed)
	    verify_flow_info ();
#endif

	  changed_overall |= changed;
	  first_pass = false;
	}
      while (changed);
    }

  FOR_ALL_BB (b)
    b->flags &= ~(BB_FORWARDER_BLOCK | BB_NONTHREADABLE_BLOCK);

  return changed_overall;
}

/* Delete all unreachable basic blocks.  */

bool
delete_unreachable_blocks (void)
{
  bool changed = false;
  basic_block b, prev_bb;

  find_unreachable_blocks ();

  /* When we're in GIMPLE mode and there may be debug insns, we should
     delete blocks in reverse dominator order, so as to get a chance
     to substitute all released DEFs into debug stmts.  If we don't
     have dominators information, walking blocks backward gets us a
     better chance of retaining most debug information than
     otherwise.  */
  if (MAY_HAVE_DEBUG_STMTS && current_ir_type () == IR_GIMPLE
      && dom_info_available_p (CDI_DOMINATORS))
    {
      for (b = EXIT_BLOCK_PTR->prev_bb; b != ENTRY_BLOCK_PTR; b = prev_bb)
	{
	  prev_bb = b->prev_bb;

	  if (!(b->flags & BB_REACHABLE))
	    {
	      /* Speed up the removal of blocks that don't dominate
		 others.  Walking backwards, this should be the common
		 case.  */
	      if (!first_dom_son (CDI_DOMINATORS, b))
		delete_basic_block (b);
	      else
		{
		  VEC (basic_block, heap) *h
		    = get_all_dominated_blocks (CDI_DOMINATORS, b);

		  while (VEC_length (basic_block, h))
		    {
		      b = VEC_pop (basic_block, h);

		      prev_bb = b->prev_bb;

		      gcc_assert (!(b->flags & BB_REACHABLE));

		      delete_basic_block (b);
		    }

		  VEC_free (basic_block, heap, h);
		}

	      changed = true;
	    }
	}
    }
  else
    {
      for (b = EXIT_BLOCK_PTR->prev_bb; b != ENTRY_BLOCK_PTR; b = prev_bb)
	{
	  prev_bb = b->prev_bb;

	  if (!(b->flags & BB_REACHABLE))
	    {
	      delete_basic_block (b);
	      changed = true;
	    }
	}
    }

  if (changed)
    tidy_fallthru_edges ();
  return changed;
}

/* Delete any jump tables never referenced.  We can't delete them at the
   time of removing tablejump insn as they are referenced by the preceding
   insns computing the destination, so we delay deleting and garbagecollect
   them once life information is computed.  */
void
delete_dead_jumptables (void)
{
  gcc_unreachable ();
}

/* Tidy the CFG by deleting unreachable code and whatnot.  */

bool
cleanup_cfg (int mode)
{
  gcc_unreachable ();
  return false;
}

EXTERN_C_END
