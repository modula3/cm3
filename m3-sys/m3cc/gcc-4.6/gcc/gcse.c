/* Modula-3: modified */

/* Global common subexpression elimination/Partial redundancy elimination
   and global constant/copy propagation for GNU compiler.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
   2006, 2007, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.

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

/* TODO
   - reordering of memory allocation and freeing to be more space efficient
   - do rough calc of how many regs are needed in each block, and a rough
     calc of how many regs are available in each class and use that to
     throttle back the code in cases where RTX_COST is minimal.
   - a store to the same address as a load does not kill the load if the
     source of the store is also the destination of the load.  Handling this
     allows more load motion, particularly out of loops.

*/

/* References searched while implementing this.

   Compilers Principles, Techniques and Tools
   Aho, Sethi, Ullman
   Addison-Wesley, 1988

   Global Optimization by Suppression of Partial Redundancies
   E. Morel, C. Renvoise
   communications of the acm, Vol. 22, Num. 2, Feb. 1979

   A Portable Machine-Independent Global Optimizer - Design and Measurements
   Frederick Chow
   Stanford Ph.D. thesis, Dec. 1983

   A Fast Algorithm for Code Movement Optimization
   D.M. Dhamdhere
   SIGPLAN Notices, Vol. 23, Num. 10, Oct. 1988

   A Solution to a Problem with Morel and Renvoise's
   Global Optimization by Suppression of Partial Redundancies
   K-H Drechsler, M.P. Stadel
   ACM TOPLAS, Vol. 10, Num. 4, Oct. 1988

   Practical Adaptation of the Global Optimization
   Algorithm of Morel and Renvoise
   D.M. Dhamdhere
   ACM TOPLAS, Vol. 13, Num. 2. Apr. 1991

   Efficiently Computing Static Single Assignment Form and the Control
   Dependence Graph
   R. Cytron, J. Ferrante, B.K. Rosen, M.N. Wegman, and F.K. Zadeck
   ACM TOPLAS, Vol. 13, Num. 4, Oct. 1991

   Lazy Code Motion
   J. Knoop, O. Ruthing, B. Steffen
   ACM SIGPLAN Notices Vol. 27, Num. 7, Jul. 1992, '92 Conference on PLDI

   What's In a Region?  Or Computing Control Dependence Regions in Near-Linear
   Time for Reducible Flow Control
   Thomas Ball
   ACM Letters on Programming Languages and Systems,
   Vol. 2, Num. 1-4, Mar-Dec 1993

   An Efficient Representation for Sparse Sets
   Preston Briggs, Linda Torczon
   ACM Letters on Programming Languages and Systems,
   Vol. 2, Num. 1-4, Mar-Dec 1993

   A Variation of Knoop, Ruthing, and Steffen's Lazy Code Motion
   K-H Drechsler, M.P. Stadel
   ACM SIGPLAN Notices, Vol. 28, Num. 5, May 1993

   Partial Dead Code Elimination
   J. Knoop, O. Ruthing, B. Steffen
   ACM SIGPLAN Notices, Vol. 29, Num. 6, Jun. 1994

   Effective Partial Redundancy Elimination
   P. Briggs, K.D. Cooper
   ACM SIGPLAN Notices, Vol. 29, Num. 6, Jun. 1994

   The Program Structure Tree: Computing Control Regions in Linear Time
   R. Johnson, D. Pearson, K. Pingali
   ACM SIGPLAN Notices, Vol. 29, Num. 6, Jun. 1994

   Optimal Code Motion: Theory and Practice
   J. Knoop, O. Ruthing, B. Steffen
   ACM TOPLAS, Vol. 16, Num. 4, Jul. 1994

   The power of assignment motion
   J. Knoop, O. Ruthing, B. Steffen
   ACM SIGPLAN Notices Vol. 30, Num. 6, Jun. 1995, '95 Conference on PLDI

   Global code motion / global value numbering
   C. Click
   ACM SIGPLAN Notices Vol. 30, Num. 6, Jun. 1995, '95 Conference on PLDI

   Value Driven Redundancy Elimination
   L.T. Simpson
   Rice University Ph.D. thesis, Apr. 1996

   Value Numbering
   L.T. Simpson
   Massively Scalar Compiler Project, Rice University, Sep. 1996

   High Performance Compilers for Parallel Computing
   Michael Wolfe
   Addison-Wesley, 1996

   Advanced Compiler Design and Implementation
   Steven Muchnick
   Morgan Kaufmann, 1997

   Building an Optimizing Compiler
   Robert Morgan
   Digital Press, 1998

   People wishing to speed up the code here should read:
     Elimination Algorithms for Data Flow Analysis
     B.G. Ryder, M.C. Paull
     ACM Computing Surveys, Vol. 18, Num. 3, Sep. 1986

     How to Analyze Large Programs Efficiently and Informatively
     D.M. Dhamdhere, B.K. Rosen, F.K. Zadeck
     ACM SIGPLAN Notices Vol. 27, Num. 7, Jul. 1992, '92 Conference on PLDI

   People wishing to do something different can find various possibilities
   in the above papers and elsewhere.
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "diagnostic-core.h"
#include "toplev.h"

#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "insn-config.h"
#include "recog.h"
#include "basic-block.h"
#include "output.h"
#include "function.h"
#include "expr.h"
#include "except.h"
#include "ggc.h"
#include "params.h"
#include "cselib.h"
#include "intl.h"
#include "obstack.h"
#include "timevar.h"
#include "tree-pass.h"
#include "hashtab.h"
#include "df.h"
#include "dbgcnt.h"
#include "target.h"
#include "gcse.h"

EXTERN_C_START

/* We support GCSE via Partial Redundancy Elimination.  PRE optimizations
   are a superset of those done by classic GCSE.

   We perform the following steps:

   1) Compute table of places where registers are set.

   2) Perform copy/constant propagation.

   3) Perform global cse using lazy code motion if not optimizing
      for size, or code hoisting if we are.

   4) Perform another pass of copy/constant propagation.  Try to bypass
      conditional jumps if the condition can be computed from a value of
      an incoming edge.

   Two passes of copy/constant propagation are done because the first one
   enables more GCSE and the second one helps to clean up the copies that
   GCSE creates.  This is needed more for PRE than for Classic because Classic
   GCSE will try to use an existing register containing the common
   subexpression rather than create a new one.  This is harder to do for PRE
   because of the code motion (which Classic GCSE doesn't do).

   Expressions we are interested in GCSE-ing are of the form
   (set (pseudo-reg) (expression)).
   Function want_to_gcse_p says what these are.

   In addition, expressions in REG_EQUAL notes are candidates for GCSE-ing.
   This allows PRE to hoist expressions that are expressed in multiple insns,
   such as complex address calculations (e.g. for PIC code, or loads with a
   high part and a low part).

   PRE handles moving invariant expressions out of loops (by treating them as
   partially redundant).

   **********************

   We used to support multiple passes but there are diminishing returns in
   doing so.  The first pass usually makes 90% of the changes that are doable.
   A second pass can make a few more changes made possible by the first pass.
   Experiments show any further passes don't make enough changes to justify
   the expense.

   A study of spec92 using an unlimited number of passes:
   [1 pass] = 1208 substitutions, [2] = 577, [3] = 202, [4] = 192, [5] = 83,
   [6] = 34, [7] = 17, [8] = 9, [9] = 4, [10] = 4, [11] = 2,
   [12] = 2, [13] = 1, [15] = 1, [16] = 2, [41] = 1

   It was found doing copy propagation between each pass enables further
   substitutions.

   This study was done before expressions in REG_EQUAL notes were added as
   candidate expressions for optimization, and before the GIMPLE optimizers
   were added.  Probably, multiple passes is even less efficient now than
   at the time when the study was conducted.

   PRE is quite expensive in complicated functions because the DFA can take
   a while to converge.  Hence we only perform one pass.

   **********************

   The steps for PRE are:

   1) Build the hash table of expressions we wish to GCSE (expr_hash_table).

   2) Perform the data flow analysis for PRE.

   3) Delete the redundant instructions

   4) Insert the required copies [if any] that make the partially
      redundant instructions fully redundant.

   5) For other reaching expressions, insert an instruction to copy the value
      to a newly created pseudo that will reach the redundant instruction.

   The deletion is done first so that when we do insertions we
   know which pseudo reg to use.

   Various papers have argued that PRE DFA is expensive (O(n^2)) and others
   argue it is not.  The number of iterations for the algorithm to converge
   is typically 2-4 so I don't view it as that expensive (relatively speaking).

   PRE GCSE depends heavily on the second CPROP pass to clean up the copies
   we create.  To make an expression reach the place where it's redundant,
   the result of the expression is copied to a new register, and the redundant
   expression is deleted by replacing it with this new register.  Classic GCSE
   doesn't have this problem as much as it computes the reaching defs of
   each register in each block and thus can try to use an existing
   register.  */

/* GCSE global vars.  */

struct target_gcse default_target_gcse;
#if SWITCHABLE_TARGET
struct target_gcse *this_target_gcse = &default_target_gcse;
#endif

/* Set to non-zero if CSE should run after all GCSE optimizations are done.  */
int flag_rerun_cse_after_global_opts;

/* An obstack for our working variables.  */
static struct obstack gcse_obstack;

struct reg_use {rtx reg_rtx; };

/* Hash table of expressions.  */

struct expr
{
  /* The expression (SET_SRC for expressions, PATTERN for assignments).  */
  rtx expr;
  /* Index in the available expression bitmaps.  */
  int bitmap_index;
  /* Next entry with the same hash.  */
  struct expr *next_same_hash;
  /* List of anticipatable occurrences in basic blocks in the function.
     An "anticipatable occurrence" is one that is the first occurrence in the
     basic block, the operands are not modified in the basic block prior
     to the occurrence and the output is not used between the start of
     the block and the occurrence.  */
  struct occr *antic_occr;
  /* List of available occurrence in basic blocks in the function.
     An "available occurrence" is one that is the last occurrence in the
     basic block and the operands are not modified by following statements in
     the basic block [including this insn].  */
  struct occr *avail_occr;
  /* Non-null if the computation is PRE redundant.
     The value is the newly created pseudo-reg to record a copy of the
     expression in all the places that reach the redundant copy.  */
  rtx reaching_reg;
  /* Maximum distance in instructions this expression can travel.
     We avoid moving simple expressions for more than a few instructions
     to keep register pressure under control.
     A value of "0" removes restrictions on how far the expression can
     travel.  */
  int max_distance;
};

/* Occurrence of an expression.
   There is one per basic block.  If a pattern appears more than once the
   last appearance is used [or first for anticipatable expressions].  */

struct occr
{
  /* Next occurrence of this expression.  */
  struct occr *next;
  /* The insn that computes the expression.  */
  rtx insn;
  /* Nonzero if this [anticipatable] occurrence has been deleted.  */
  char deleted_p;
  /* Nonzero if this [available] occurrence has been copied to
     reaching_reg.  */
  /* ??? This is mutually exclusive with deleted_p, so they could share
     the same byte.  */
  char copied_p;
};

typedef struct occr *occr_t;
DEF_VEC_P (occr_t);
DEF_VEC_ALLOC_P (occr_t, heap);

/* Expression and copy propagation hash tables.
   Each hash table is an array of buckets.
   ??? It is known that if it were an array of entries, structure elements
   `next_same_hash' and `bitmap_index' wouldn't be necessary.  However, it is
   not clear whether in the final analysis a sufficient amount of memory would
   be saved as the size of the available expression bitmaps would be larger
   [one could build a mapping table without holes afterwards though].
   Someday I'll perform the computation and figure it out.  */

struct hash_table_d
{
  /* The table itself.
     This is an array of `expr_hash_table_size' elements.  */
  struct expr **table;

  /* Size of the hash table, in elements.  */
  unsigned int size;

  /* Number of hash table elements.  */
  unsigned int n_elems;

  /* Whether the table is expression of copy propagation one.  */
  int set_p;
};

/* Expression hash table.  */
static struct hash_table_d expr_hash_table;

/* Copy propagation hash table.  */
static struct hash_table_d set_hash_table;

/* This is a list of expressions which are MEMs and will be used by load
   or store motion.
   Load motion tracks MEMs which aren't killed by
   anything except itself. (i.e., loads and stores to a single location).
   We can then allow movement of these MEM refs with a little special
   allowance. (all stores copy the same value to the reaching reg used
   for the loads).  This means all values used to store into memory must have
   no side effects so we can re-issue the setter value.
   Store Motion uses this structure as an expression table to track stores
   which look interesting, and might be moveable towards the exit block.  */

struct ls_expr
{
  struct expr * expr;		/* Gcse expression reference for LM.  */
  rtx pattern;			/* Pattern of this mem.  */
  rtx pattern_regs;		/* List of registers mentioned by the mem.  */
  rtx loads;			/* INSN list of loads seen.  */
  rtx stores;			/* INSN list of stores seen.  */
  struct ls_expr * next;	/* Next in the list.  */
  int invalid;			/* Invalid for some reason.  */
  int index;			/* If it maps to a bitmap index.  */
  unsigned int hash_index;	/* Index when in a hash table.  */
  rtx reaching_reg;		/* Register to use when re-writing.  */
};

/* Array of implicit set patterns indexed by basic block index.  */
static rtx *implicit_sets;

/* Head of the list of load/store memory refs.  */
static struct ls_expr * pre_ldst_mems = NULL;

/* Hashtable for the load/store memory refs.  */
static htab_t pre_ldst_table = NULL;

/* Bitmap containing one bit for each register in the program.
   Used when performing GCSE to track which registers have been set since
   the start of the basic block.  */
static regset reg_set_bitmap;

/* Array, indexed by basic block number for a list of insns which modify
   memory within that block.  */
static rtx * modify_mem_list;
static bitmap modify_mem_list_set;

/* This array parallels modify_mem_list, but is kept canonicalized.  */
static rtx * canon_modify_mem_list;

/* Bitmap indexed by block numbers to record which blocks contain
   function calls.  */
static bitmap blocks_with_calls;

/* Various variables for statistics gathering.  */

/* Memory used in a pass.
   This isn't intended to be absolutely precise.  Its intent is only
   to keep an eye on memory usage.  */
static int bytes_used;

/* GCSE substitutions made.  */
static int gcse_subst_count;
/* Number of copy instructions created.  */
static int gcse_create_count;
/* Number of local constants propagated.  */
static int local_const_prop_count;
/* Number of local copies propagated.  */
static int local_copy_prop_count;
/* Number of global constants propagated.  */
static int global_const_prop_count;
/* Number of global copies propagated.  */
static int global_copy_prop_count;

/* Doing code hoisting.  */
static bool doing_code_hoisting_p = false;

/* For available exprs */
static sbitmap *ae_kill;

static void compute_can_copy (void);
static void *gmalloc (size_t) ATTRIBUTE_MALLOC;
static void *gcalloc (size_t, size_t) ATTRIBUTE_MALLOC;
static void *gcse_alloc (unsigned long);
static void alloc_gcse_mem (void);
static void free_gcse_mem (void);
static void hash_scan_insn (rtx, struct hash_table_d *);
static void hash_scan_set (rtx, rtx, struct hash_table_d *);
static void hash_scan_clobber (rtx, rtx, struct hash_table_d *);
static void hash_scan_call (rtx, rtx, struct hash_table_d *);
static int want_to_gcse_p (rtx, int *);
static bool gcse_constant_p (const_rtx);
static int oprs_unchanged_p (const_rtx, const_rtx, int);
static int oprs_anticipatable_p (const_rtx, const_rtx);
static int oprs_available_p (const_rtx, const_rtx);
static void insert_expr_in_table (rtx, enum machine_mode, rtx, int, int, int,
				  struct hash_table_d *);
static void insert_set_in_table (rtx, rtx, struct hash_table_d *);
static unsigned int hash_expr (const_rtx, enum machine_mode, int *, int);
static unsigned int hash_set (int, int);
static int expr_equiv_p (const_rtx, const_rtx);
static void record_last_reg_set_info (rtx, int);
static void record_last_mem_set_info (rtx);
static void record_last_set_info (rtx, const_rtx, void *);
static void compute_hash_table (struct hash_table_d *);
static void alloc_hash_table (struct hash_table_d *, int);
static void free_hash_table (struct hash_table_d *);
static void compute_hash_table_work (struct hash_table_d *);
static void dump_hash_table (FILE *, const char *, struct hash_table_d *);
static struct expr *lookup_set (unsigned int, struct hash_table_d *);
static struct expr *next_set (unsigned int, struct expr *);
static void reset_opr_set_tables (void);
static int oprs_not_set_p (const_rtx, const_rtx);
static void mark_call (rtx);
static void mark_set (rtx, rtx);
static void mark_clobber (rtx, rtx);
static void mark_oprs_set (rtx);
static void alloc_cprop_mem (int, int);
static void free_cprop_mem (void);
static void compute_transp (const_rtx, int, sbitmap *, int);
static void compute_local_properties (sbitmap *, sbitmap *, sbitmap *,
				      struct hash_table_d *);
static void compute_cprop_data (void);
static void find_used_regs (rtx *, void *);
static int try_replace_reg (rtx, rtx, rtx);
static struct expr *find_avail_set (int, rtx);
static int cprop_jump (basic_block, rtx, rtx, rtx, rtx);
static void mems_conflict_for_gcse_p (rtx, const_rtx, void *);
static int load_killed_in_block_p (const_basic_block, int, const_rtx, int);
static void canon_list_insert (rtx, const_rtx, void *);
static int cprop_insn (rtx);
static void find_implicit_sets (void);
static int one_cprop_pass (void);
static bool constprop_register (rtx, rtx, rtx);
static struct expr *find_bypass_set (int, int);
static bool reg_killed_on_edge (const_rtx, const_edge);
static int bypass_block (basic_block, rtx, rtx);
static int bypass_conditional_jumps (void);
static void alloc_pre_mem (int, int);
static void free_pre_mem (void);
static void compute_pre_data (void);
static int pre_expr_reaches_here_p (basic_block, struct expr *,
				    basic_block);
static void insert_insn_end_basic_block (struct expr *, basic_block);
static void pre_insert_copy_insn (struct expr *, rtx);
static void pre_insert_copies (void);
static int pre_delete (void);
static int pre_gcse (void);
static int one_pre_gcse_pass (void);
static void add_label_notes (rtx, rtx);
static void alloc_code_hoist_mem (int, int);
static void free_code_hoist_mem (void);
static void compute_code_hoist_vbeinout (void);
static void compute_code_hoist_data (void);
static int hoist_expr_reaches_here_p (basic_block, int, basic_block, char *,
				      int, int *);
static int hoist_code (void);
static int one_code_hoisting_pass (void);
static rtx process_insert_insn (struct expr *);
static int pre_edge_insert (struct edge_list *, struct expr **);
static int pre_expr_reaches_here_p_work (basic_block, struct expr *,
					 basic_block, char *);
static struct ls_expr * ldst_entry (rtx);
static void free_ldst_entry (struct ls_expr *);
static void free_ldst_mems (void);
static void print_ldst_list (FILE *);
static struct ls_expr * find_rtx_in_ldst (rtx);
static inline struct ls_expr * first_ls_expr (void);
static inline struct ls_expr * next_ls_expr (struct ls_expr *);
static int simple_mem (const_rtx);
static void invalidate_any_buried_refs (rtx);
static void compute_ld_motion_mems (void);
static void trim_ld_motion_mems (void);
static void update_ld_motion_stores (struct expr *);
static void free_insn_expr_list_list (rtx *);
static void clear_modify_mem_tables (void);
static void free_modify_mem_tables (void);
static rtx gcse_emit_move_after (rtx, rtx, rtx);
static void local_cprop_find_used_regs (rtx *, void *);
static bool do_local_cprop (rtx, rtx);
static int local_cprop_pass (void);
static bool is_too_expensive (const char *);

#define GNEW(T)			((T *) gmalloc (sizeof (T)))
#define GCNEW(T)		((T *) gcalloc (1, sizeof (T)))

#define GNEWVEC(T, N)		((T *) gmalloc (sizeof (T) * (N)))
#define GCNEWVEC(T, N)		((T *) gcalloc ((N), sizeof (T)))

#define GNEWVAR(T, S)		((T *) gmalloc ((S)))
#define GCNEWVAR(T, S)		((T *) gcalloc (1, (S)))

#define GOBNEW(T)		((T *) gcse_alloc (sizeof (T)))
#define GOBNEWVAR(T, S)		((T *) gcse_alloc ((S)))

/* Misc. utilities.  */

#define can_copy \
  (this_target_gcse->x_can_copy)
#define can_copy_init_p \
  (this_target_gcse->x_can_copy_init_p)

/* Compute which modes support reg/reg copy operations.  */

static void
compute_can_copy (void)
{
  int i;
#ifndef AVOID_CCMODE_COPIES
  rtx reg, insn;
#endif
  memset (can_copy, 0, NUM_MACHINE_MODES);

  start_sequence ();
  for (i = 0; i < NUM_MACHINE_MODES; i++)
    if (GET_MODE_CLASS (i) == MODE_CC)
      {
#ifdef AVOID_CCMODE_COPIES
	can_copy[i] = 0;
#else
	reg = gen_rtx_REG ((enum machine_mode) i, LAST_VIRTUAL_REGISTER + 1);
	insn = emit_insn (gen_rtx_SET (VOIDmode, reg, reg));
	if (recog (PATTERN (insn), insn, NULL) >= 0)
	  can_copy[i] = 1;
#endif
      }
    else
      can_copy[i] = 1;

  end_sequence ();
}

/* Returns whether the mode supports reg/reg copy operations.  */

bool
can_copy_p (enum machine_mode mode)
{
  if (! can_copy_init_p)
    {
      compute_can_copy ();
      can_copy_init_p = true;
    }

  return can_copy[mode] != 0;
}

/* Used internally by can_assign_to_reg_without_clobbers_p.  */

static GTY(()) rtx test_insn;

/* Return true if we can assign X to a pseudo register such that the
   resulting insn does not result in clobbering a hard register as a
   side-effect.

   Additionally, if the target requires it, check that the resulting insn
   can be copied.  If it cannot, this means that X is special and probably
   has hidden side-effects we don't want to mess with.

   This function is typically used by code motion passes, to verify
   that it is safe to insert an insn without worrying about clobbering
   maybe live hard regs.  */

bool
can_assign_to_reg_without_clobbers_p (rtx x)
{
  int num_clobbers = 0;
  int icode;

  /* If this is a valid operand, we are OK.  If it's VOIDmode, we aren't.  */
  if (general_operand (x, GET_MODE (x)))
    return 1;
  else if (GET_MODE (x) == VOIDmode)
    return 0;

  /* Otherwise, check if we can make a valid insn from it.  First initialize
     our test insn if we haven't already.  */
  if (test_insn == 0)
    {
      test_insn
	= make_insn_raw (gen_rtx_SET (VOIDmode,
				      gen_rtx_REG (word_mode,
						   FIRST_PSEUDO_REGISTER * 2),
				      const0_rtx));
      NEXT_INSN (test_insn) = PREV_INSN (test_insn) = 0;
    }

  /* Now make an insn like the one we would make when GCSE'ing and see if
     valid.  */
  PUT_MODE (SET_DEST (PATTERN (test_insn)), GET_MODE (x));
  SET_SRC (PATTERN (test_insn)) = x;

  icode = recog (PATTERN (test_insn), test_insn, &num_clobbers);
  if (icode < 0)
    return false;

  if (num_clobbers > 0 && added_clobbers_hard_reg_p (icode))
    return false;

  if (targetm.cannot_copy_insn_p && targetm.cannot_copy_insn_p (test_insn))
    return false;

  return true;
}

/* Similar to get_condition, only the resulting condition must be
   valid at JUMP, instead of at EARLIEST.

   This differs from noce_get_condition in ifcvt.c in that we prefer not to
   settle for the condition variable in the jump instruction being integral.
   We prefer to be able to record the value of a user variable, rather than
   the value of a temporary used in a condition.  This could be solved by
   recording the value of *every* register scanned by canonicalize_condition,
   but this would require some code reorganization.  */

rtx
fis_get_condition (rtx jump)
{
  return get_condition (jump, NULL, false, true);
}

EXTERN_C_END

#include "gt-gcse.h"
