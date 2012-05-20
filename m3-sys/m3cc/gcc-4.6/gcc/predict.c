/* Modula-3: modified */

/* Branch prediction routines for the GNU compiler.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2009, 2010
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

/* References:

   [1] "Branch Prediction for Free"
       Ball and Larus; PLDI '93.
   [2] "Static Branch Frequency and Program Profile Analysis"
       Wu and Larus; MICRO-27.
   [3] "Corpus-based Static Branch Prediction"
       Calder, Grunwald, Lindsay, Martin, Mozer, and Zorn; PLDI '95.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "insn-config.h"
#include "regs.h"
#include "flags.h"
#include "output.h"
#include "function.h"
#include "except.h"
#include "diagnostic-core.h"
#include "recog.h"
#include "expr.h"
#include "predict.h"
#include "sreal.h"
#include "params.h"
#include "target.h"
#include "cfgloop.h"
#include "tree-flow.h"
#include "ggc.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "timevar.h"
#include "cfgloop.h"
#include "pointer-set.h"

EXTERN_C_START

/* Random guesstimation given names.
   PROV_VERY_UNLIKELY should be small enough so basic block predicted
   by it gets bellow HOT_BB_FREQUENCY_FRANCTION.  */
#define PROB_VERY_UNLIKELY	(REG_BR_PROB_BASE / 2000 - 1)
#define PROB_EVEN		(REG_BR_PROB_BASE / 2)
#define PROB_VERY_LIKELY	(REG_BR_PROB_BASE - PROB_VERY_UNLIKELY)
#define PROB_ALWAYS		(REG_BR_PROB_BASE)

static void combine_predictions_for_insn (rtx, basic_block);
static void dump_prediction (FILE *, enum br_predictor, int, basic_block, int);
static void predict_paths_leading_to (basic_block, enum br_predictor, enum prediction);
static void predict_paths_leading_to_edge (edge, enum br_predictor, enum prediction);
static bool can_predict_insn_p (const_rtx);

/* Information we hold about each branch predictor.
   Filled using information from predict.def.  */

struct predictor_info
{
  const char *const name;	/* Name used in the debugging dumps.  */
  const int hitrate;		/* Expected hitrate used by
				   predict_insn_def call.  */
  const int flags;
};

/* Use given predictor without Dempster-Shaffer theory if it matches
   using first_match heuristics.  */
#define PRED_FLAG_FIRST_MATCH 1

/* Recompute hitrate in percent to our representation.  */

#define HITRATE(VAL) ((int) ((VAL) * REG_BR_PROB_BASE + 50) / 100)

#define DEF_PREDICTOR(ENUM, NAME, HITRATE, FLAGS) {NAME, HITRATE, FLAGS},
static const struct predictor_info predictor_info[]= {
#include "predict.def"

  /* Upper bound on predictors.  */
  {NULL, 0, 0}
};
#undef DEF_PREDICTOR

/* Return TRUE if frequency FREQ is considered to be hot.  */

static inline bool
maybe_hot_frequency_p (int freq)
{
  return false;
}

/* Return TRUE if frequency FREQ is considered to be hot.  */

static inline bool
maybe_hot_count_p (gcov_type count)
{
  return false;
}

/* Return true in case BB can be CPU intensive and should be optimized
   for maximal performance.  */

bool
maybe_hot_bb_p (const_basic_block bb)
{
  return false;
}

/* Return true if the call can be hot.  */

bool
cgraph_maybe_hot_edge_p (struct cgraph_edge *edge)
{
  return false;
}

/* Return true in case BB can be CPU intensive and should be optimized
   for maximal performance.  */

bool
maybe_hot_edge_p (edge e)
{
  if (profile_status == PROFILE_READ)
    return maybe_hot_count_p (e->count);
  return maybe_hot_frequency_p (EDGE_FREQUENCY (e));
}

/* Return true in case BB is probably never executed.  */
bool
probably_never_executed_bb_p (const_basic_block bb)
{
  return false;
}

/* Return true when current function should always be optimized for size.  */

bool
optimize_function_for_size_p (struct function *fun)
{
  return (optimize_size
	  || (fun && fun->decl
	      && (cgraph_node (fun->decl)->frequency
		  == NODE_FREQUENCY_UNLIKELY_EXECUTED)));
}

/* Return true when current function should always be optimized for speed.  */

bool
optimize_function_for_speed_p (struct function *fun)
{
  return !optimize_function_for_size_p (fun);
}

/* Return TRUE when BB should be optimized for size.  */

bool
optimize_bb_for_size_p (const_basic_block bb)
{
  return optimize_function_for_size_p (cfun) || !maybe_hot_bb_p (bb);
}

/* Return TRUE when BB should be optimized for speed.  */

bool
optimize_bb_for_speed_p (const_basic_block bb)
{
  return !optimize_bb_for_size_p (bb);
}

/* Return TRUE when BB should be optimized for size.  */

bool
optimize_edge_for_size_p (edge e)
{
  return optimize_function_for_size_p (cfun) || !maybe_hot_edge_p (e);
}

/* Return TRUE when BB should be optimized for speed.  */

bool
optimize_edge_for_speed_p (edge e)
{
  return !optimize_edge_for_size_p (e);
}

/* Return TRUE when BB should be optimized for size.  */

bool
optimize_insn_for_size_p (void)
{
  return optimize_function_for_size_p (cfun) || !crtl->maybe_hot_insn_p;
}

/* Return TRUE when BB should be optimized for speed.  */

bool
optimize_insn_for_speed_p (void)
{
  return !optimize_insn_for_size_p ();
}

/* Return TRUE when LOOP should be optimized for size.  */

bool
optimize_loop_for_size_p (struct loop *loop)
{
  return optimize_bb_for_size_p (loop->header);
}

/* Return TRUE when LOOP should be optimized for speed.  */

bool
optimize_loop_for_speed_p (struct loop *loop)
{
  return optimize_bb_for_speed_p (loop->header);
}

/* Return TRUE when LOOP nest should be optimized for speed.  */

bool
optimize_loop_nest_for_speed_p (struct loop *loop)
{
  struct loop *l = loop;

  gcc_unreachable ();

  if (optimize_loop_for_speed_p (loop))
    return true;
  l = loop->inner;
  while (l && l != loop)
    {
      if (optimize_loop_for_speed_p (l))
        return true;
      if (l->inner)
        l = l->inner;
      else if (l->next)
        l = l->next;
      else
        {
	  while (l != loop && !l->next)
	    l = loop_outer (l);
	  if (l != loop)
	    l = l->next;
	}
    }
  return false;
}

/* Return TRUE when LOOP nest should be optimized for size.  */

bool
optimize_loop_nest_for_size_p (struct loop *loop)
{
  gcc_unreachable ();
  return !optimize_loop_nest_for_speed_p (loop);
}

/* Return true when edge E is likely to be well predictable by branch
   predictor.  */

bool
predictable_edge_p (edge e)
{
  gcc_unreachable ();
  if (profile_status == PROFILE_ABSENT)
    return false;
  if ((e->probability
       <= PARAM_VALUE (PARAM_PREDICTABLE_BRANCH_OUTCOME) * REG_BR_PROB_BASE / 100)
      || (REG_BR_PROB_BASE - e->probability
          <= PARAM_VALUE (PARAM_PREDICTABLE_BRANCH_OUTCOME) * REG_BR_PROB_BASE / 100))
    return true;
  return false;
}


/* Set RTL expansion for BB profile.  */

void
rtl_profile_for_bb (basic_block bb)
{
  gcc_unreachable ();
  crtl->maybe_hot_insn_p = maybe_hot_bb_p (bb);
}

/* Set RTL expansion for edge profile.  */

void
rtl_profile_for_edge (edge e)
{
  gcc_unreachable ();
  crtl->maybe_hot_insn_p = maybe_hot_edge_p (e);
}

/* Set RTL expansion to default mode (i.e. when profile info is not known).  */
void
default_rtl_profile (void)
{
  crtl->maybe_hot_insn_p = true;
}

/* Return true if the one of outgoing edges is already predicted by
   PREDICTOR.  */

bool
rtl_predicted_by_p (const_basic_block bb, enum br_predictor predictor)
{
  rtx note;
  gcc_unreachable ();
  if (!INSN_P (BB_END (bb)))
    return false;
  for (note = REG_NOTES (BB_END (bb)); note; note = XEXP (note, 1))
    if (REG_NOTE_KIND (note) == REG_BR_PRED
	&& INTVAL (XEXP (XEXP (note, 0), 0)) == (int)predictor)
      return true;
  return false;
}

/* This map contains for a basic block the list of predictions for the
   outgoing edges.  */

static struct pointer_map_t *bb_predictions;

/*  Structure representing predictions in tree level. */

struct edge_prediction {
    struct edge_prediction *ep_next;
    edge ep_edge;
    enum br_predictor ep_predictor;
    int ep_probability;
};

/* Return true if the one of outgoing edges is already predicted by
   PREDICTOR.  */

bool
gimple_predicted_by_p (const_basic_block bb, enum br_predictor predictor)
{
  struct edge_prediction *i;
  void **preds = pointer_map_contains (bb_predictions, bb);

  gcc_unreachable ();
  if (!preds)
    return false;

  for (i = (struct edge_prediction *) *preds; i; i = i->ep_next)
    if (i->ep_predictor == predictor)
      return true;
  return false;
}
static bool
probability_reliable_p (int prob)
{
  gcc_unreachable ();
  return (profile_status == PROFILE_READ
	  || (profile_status == PROFILE_GUESSED
	      && (prob <= HITRATE (1) || prob >= HITRATE (99))));
}

/* Same predicate as above, working on edges.  */
bool
edge_probability_reliable_p (const_edge e)
{
  gcc_unreachable ();
  return probability_reliable_p (e->probability);
}

/* Same predicate as edge_probability_reliable_p, working on notes.  */
bool
br_prob_note_reliable_p (const_rtx note)
{
  gcc_assert (REG_NOTE_KIND (note) == REG_BR_PROB);
  return probability_reliable_p (INTVAL (XEXP (note, 0)));
}

/* Predict insn by given predictor.  */

void
predict_insn_def (rtx insn, enum br_predictor predictor,
		  enum prediction taken)
{
  gcc_unreachable ();
}

void
rtl_predict_edge (edge e, enum br_predictor predictor, int probability)
{
  gcc_unreachable ();
}

/* Predict edge E with the given PROBABILITY.  */
void
gimple_predict_edge (edge e, enum br_predictor predictor, int probability)
{
  gcc_unreachable ();
}

void
remove_predictions_associated_with_edge (edge e)
{
  gcc_unreachable ();
}

/* Predict edge E by given predictor if possible.  */

void
predict_edge_def (edge e, enum br_predictor predictor,
		  enum prediction taken)
{
  gcc_unreachable ();
}

void
invert_br_probabilities (rtx insn)
{
  gcc_unreachable ();
}

/* Set edge->probability for each successor edge of BB.  */
void
guess_outgoing_edge_probabilities (basic_block bb)
{
  gcc_unreachable ();
}

void
tree_estimate_probability (void)
{
  gcc_unreachable ();
}

/* Decide whether function is hot, cold or unlikely executed.  */
void
compute_function_frequency (void)
{
  gcc_unreachable ();
}

/* Build PREDICT_EXPR.  */
tree
build_predict_expr (enum br_predictor predictor, enum prediction taken)
{
  tree t = build1 (PREDICT_EXPR, void_type_node,
		   build_int_cst (NULL, predictor));
  gcc_unreachable ();
  SET_PREDICT_EXPR_OUTCOME (t, taken);
  return t;
}

const char *
predictor_name (enum br_predictor predictor)
{
  gcc_unreachable ();
  return predictor_info[predictor].name;
}

void
rebuild_frequencies (void)
{
  gcc_unreachable ();
}

EXTERN_C_END
