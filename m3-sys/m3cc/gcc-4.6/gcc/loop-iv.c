/* Modula-3: modified */

/* Rtl-level induction variable analysis.
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
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

/* This is a simple analysis of induction variables of the loop.  The major use
   is for determining the number of iterations of a loop for loop unrolling,
   doloop optimization and branch prediction.  The iv information is computed
   on demand.

   Induction variables are analyzed by walking the use-def chains.  When
   a basic induction variable (biv) is found, it is cached in the bivs
   hash table.  When register is proved to be a biv, its description
   is stored to DF_REF_DATA of the def reference.

   The analysis works always with one loop -- you must call
   iv_analysis_loop_init (loop) for it.  All the other functions then work with
   this loop.   When you need to work with another loop, just call
   iv_analysis_loop_init for it.  When you no longer need iv analysis, call
   iv_analysis_done () to clean up the memory.

   The available functions are:

   iv_analyze (insn, reg, iv): Stores the description of the induction variable
     corresponding to the use of register REG in INSN to IV.  Returns true if
     REG is an induction variable in INSN. false otherwise.
     If use of REG is not found in INSN, following insns are scanned (so that
     we may call this function on insn returned by get_condition).
   iv_analyze_result (insn, def, iv):  Stores to IV the description of the iv
     corresponding to DEF, which is a register defined in INSN.
   iv_analyze_expr (insn, rhs, mode, iv):  Stores to IV the description of iv
     corresponding to expression EXPR evaluated at INSN.  All registers used bu
     EXPR must also be used in INSN.
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "obstack.h"
#include "basic-block.h"
#include "cfgloop.h"
#include "expr.h"
#include "intl.h"
#include "output.h"
#include "diagnostic-core.h"
#include "df.h"
#include "hashtab.h"

/* Possible return values of iv_get_reaching_def.  */

enum iv_grd_result
{
  /* More than one reaching def, or reaching def that does not
     dominate the use.  */
  GRD_INVALID,

  /* The use is trivial invariant of the loop, i.e. is not changed
     inside the loop.  */
  GRD_INVARIANT,

  /* The use is reached by initial value and a value from the
     previous iteration.  */
  GRD_MAYBE_BIV,

  /* The use has single dominating def.  */
  GRD_SINGLE_DOM
};

/* Information about a biv.  */

struct biv_entry
{
  unsigned regno;	/* The register of the biv.  */
  struct rtx_iv iv;	/* Value of the biv.  */
};

static bool clean_slate = true;

static unsigned int iv_ref_table_size = 0;

/* Table of rtx_ivs indexed by the df_ref uid field.  */
static struct rtx_iv ** iv_ref_table;

/* Induction variable stored at the reference.  */
#define DF_REF_IV(REF) iv_ref_table[DF_REF_ID(REF)]
#define DF_REF_IV_SET(REF, IV) iv_ref_table[DF_REF_ID(REF)] = (IV)

/* The current loop.  */

static struct loop *current_loop;

/* Bivs of the current loop.  */

static htab_t bivs;

static bool iv_analyze_op (rtx, rtx, struct rtx_iv *);

/* Dumps information about IV to FILE.  */

extern void dump_iv_info (FILE *, struct rtx_iv *);
void
dump_iv_info (FILE *file, struct rtx_iv *iv)
{
  if (!iv->base)
    {
      fprintf (file, "not simple");
      return;
    }

  if (iv->step == const0_rtx
      && !iv->first_special)
    fprintf (file, "invariant ");

  print_rtl (file, iv->base);
  if (iv->step != const0_rtx)
    {
      fprintf (file, " + ");
      print_rtl (file, iv->step);
      fprintf (file, " * iteration");
    }
  fprintf (file, " (in %s)", GET_MODE_NAME (iv->mode));

  if (iv->mode != iv->extend_mode)
    fprintf (file, " %s to %s",
	     rtx_name[iv->extend],
	     GET_MODE_NAME (iv->extend_mode));

  if (iv->mult != const1_rtx)
    {
      fprintf (file, " * ");
      print_rtl (file, iv->mult);
    }
  if (iv->delta != const0_rtx)
    {
      fprintf (file, " + ");
      print_rtl (file, iv->delta);
    }
  if (iv->first_special)
    fprintf (file, " (first special)");
}

/* Generates a subreg to get the least significant part of EXPR (in mode
   INNER_MODE) to OUTER_MODE.  */

rtx
lowpart_subreg (enum machine_mode outer_mode, rtx expr,
		enum machine_mode inner_mode)
{
  return simplify_gen_subreg (outer_mode, expr, inner_mode,
			      subreg_lowpart_offset (outer_mode, inner_mode));
}

static void
check_iv_ref_table_size (void)
{
  if (iv_ref_table_size < DF_DEFS_TABLE_SIZE())
    {
      unsigned int new_size = DF_DEFS_TABLE_SIZE () + (DF_DEFS_TABLE_SIZE () / 4);
      iv_ref_table = XRESIZEVEC (struct rtx_iv *, iv_ref_table, new_size);
      memset (&iv_ref_table[iv_ref_table_size], 0,
	      (new_size - iv_ref_table_size) * sizeof (struct rtx_iv *));
      iv_ref_table_size = new_size;
    }
}


/* Checks whether REG is a well-behaved register.  */

static bool
simple_reg_p (rtx reg)
{
  unsigned r;

  if (GET_CODE (reg) == SUBREG)
    {
      if (!subreg_lowpart_p (reg))
	return false;
      reg = SUBREG_REG (reg);
    }

  if (!REG_P (reg))
    return false;

  r = REGNO (reg);
  if (HARD_REGISTER_NUM_P (r))
    return false;

  if (GET_MODE_CLASS (GET_MODE (reg)) != MODE_INT)
    return false;

  return true;
}

/* Clears the information about ivs stored in df.  */

static void
clear_iv_info (void)
{
  unsigned i, n_defs = DF_DEFS_TABLE_SIZE ();
  struct rtx_iv *iv;

  check_iv_ref_table_size ();
  for (i = 0; i < n_defs; i++)
    {
      iv = iv_ref_table[i];
      if (iv)
	{
	  free (iv);
	  iv_ref_table[i] = NULL;
	}
    }

  htab_empty (bivs);
}

/* Returns hash value for biv B.  */

static hashval_t
biv_hash (const void *b)
{
  return ((const struct biv_entry *) b)->regno;
}

/* Compares biv B and register R.  */

static int
biv_eq (const void *b, const void *r)
{
  return ((const struct biv_entry *) b)->regno == REGNO ((const_rtx) r);
}

/* Prepare the data for an induction variable analysis of a LOOP.  */

void
iv_analysis_loop_init (struct loop *loop)
{
  basic_block *body = get_loop_body_in_dom_order (loop), bb;
  bitmap blocks = BITMAP_ALLOC (NULL);
  unsigned i;

  current_loop = loop;

  /* Clear the information from the analysis of the previous loop.  */
  if (clean_slate)
    {
      df_set_flags (DF_EQ_NOTES + DF_DEFER_INSN_RESCAN);
      bivs = htab_create (10, biv_hash, biv_eq, free);
      clean_slate = false;
    }
  else
    clear_iv_info ();

  for (i = 0; i < loop->num_nodes; i++)
    {
      bb = body[i];
      bitmap_set_bit (blocks, bb->index);
    }
  /* Get rid of the ud chains before processing the rescans.  Then add
     the problem back.  */
  df_remove_problem (df_chain);
  df_process_deferred_rescans ();
  df_chain_add_problem (DF_UD_CHAIN);
  df_note_add_problem ();
  df_set_blocks (blocks);
  df_analyze ();
  if (dump_file)
    df_dump_region (dump_file);

  check_iv_ref_table_size ();
  BITMAP_FREE (blocks);
  free (body);
}

/* Finds the definition of REG that dominates loop latch and stores
   it to DEF.  Returns false if there is not a single definition
   dominating the latch.  If REG has no definition in loop, DEF
   is set to NULL and true is returned.  */

static bool
latch_dominating_def (rtx reg, df_ref *def)
{
  df_ref single_rd = NULL, adef;
  unsigned regno = REGNO (reg);
  struct df_rd_bb_info *bb_info = DF_RD_BB_INFO (current_loop->latch);

  for (adef = DF_REG_DEF_CHAIN (regno); adef; adef = DF_REF_NEXT_REG (adef))
    {
      if (!bitmap_bit_p (df->blocks_to_analyze, DF_REF_BBNO (adef))
	  || !bitmap_bit_p (&bb_info->out, DF_REF_ID (adef)))
	continue;

      /* More than one reaching definition.  */
      if (single_rd)
	return false;

      if (!just_once_each_iteration_p (current_loop, DF_REF_BB (adef)))
	return false;

      single_rd = adef;
    }

  *def = single_rd;
  return true;
}

/* Gets definition of REG reaching its use in INSN and stores it to DEF.  */

static enum iv_grd_result
iv_get_reaching_def (rtx insn, rtx reg, df_ref *def)
{
  df_ref use, adef;
  basic_block def_bb, use_bb;
  rtx def_insn;
  bool dom_p;

  *def = NULL;
  if (!simple_reg_p (reg))
    return GRD_INVALID;
  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);
  gcc_assert (REG_P (reg));

  use = df_find_use (insn, reg);
  gcc_assert (use != NULL);

  if (!DF_REF_CHAIN (use))
    return GRD_INVARIANT;

  /* More than one reaching def.  */
  if (DF_REF_CHAIN (use)->next)
    return GRD_INVALID;

  adef = DF_REF_CHAIN (use)->ref;

  /* We do not handle setting only part of the register.  */
  if (DF_REF_FLAGS (adef) & DF_REF_READ_WRITE)
    return GRD_INVALID;

  def_insn = DF_REF_INSN (adef);
  def_bb = DF_REF_BB (adef);
  use_bb = BLOCK_FOR_INSN (insn);

  if (use_bb == def_bb)
    dom_p = (DF_INSN_LUID (def_insn) < DF_INSN_LUID (insn));
  else
    dom_p = dominated_by_p (CDI_DOMINATORS, use_bb, def_bb);

  if (dom_p)
    {
      *def = adef;
      return GRD_SINGLE_DOM;
    }

  /* The definition does not dominate the use.  This is still OK if
     this may be a use of a biv, i.e. if the def_bb dominates loop
     latch.  */
  if (just_once_each_iteration_p (current_loop, def_bb))
    return GRD_MAYBE_BIV;

  return GRD_INVALID;
}

/* Sets IV to invariant CST in MODE.  Always returns true (just for
   consistency with other iv manipulation functions that may fail).  */

static bool
iv_constant (struct rtx_iv *iv, rtx cst, enum machine_mode mode)
{
  if (mode == VOIDmode)
    mode = GET_MODE (cst);

  iv->mode = mode;
  iv->base = cst;
  iv->step = const0_rtx;
  iv->first_special = false;
  iv->extend = UNKNOWN;
  iv->extend_mode = iv->mode;
  iv->delta = const0_rtx;
  iv->mult = const1_rtx;

  return true;
}

/* Evaluates application of subreg to MODE on IV.  */

static bool
iv_subreg (struct rtx_iv *iv, enum machine_mode mode)
{
  /* If iv is invariant, just calculate the new value.  */
  if (iv->step == const0_rtx
      && !iv->first_special)
    {
      rtx val = get_iv_value (iv, const0_rtx);
      val = lowpart_subreg (mode, val, iv->extend_mode);

      iv->base = val;
      iv->extend = UNKNOWN;
      iv->mode = iv->extend_mode = mode;
      iv->delta = const0_rtx;
      iv->mult = const1_rtx;
      return true;
    }

  if (iv->extend_mode == mode)
    return true;

  if (GET_MODE_BITSIZE (mode) > GET_MODE_BITSIZE (iv->mode))
    return false;

  iv->extend = UNKNOWN;
  iv->mode = mode;

  iv->base = simplify_gen_binary (PLUS, iv->extend_mode, iv->delta,
				  simplify_gen_binary (MULT, iv->extend_mode,
						       iv->base, iv->mult));
  iv->step = simplify_gen_binary (MULT, iv->extend_mode, iv->step, iv->mult);
  iv->mult = const1_rtx;
  iv->delta = const0_rtx;
  iv->first_special = false;

  return true;
}

/* Evaluates application of EXTEND to MODE on IV.  */

static bool
iv_extend (struct rtx_iv *iv, enum rtx_code extend, enum machine_mode mode)
{
  /* If iv is invariant, just calculate the new value.  */
  if (iv->step == const0_rtx
      && !iv->first_special)
    {
      rtx val = get_iv_value (iv, const0_rtx);
      val = simplify_gen_unary (extend, mode, val, iv->extend_mode);

      iv->base = val;
      iv->extend = UNKNOWN;
      iv->mode = iv->extend_mode = mode;
      iv->delta = const0_rtx;
      iv->mult = const1_rtx;
      return true;
    }

  if (mode != iv->extend_mode)
    return false;

  if (iv->extend != UNKNOWN
      && iv->extend != extend)
    return false;

  iv->extend = extend;

  return true;
}

/* Evaluates negation of IV.  */

static bool
iv_neg (struct rtx_iv *iv)
{
  if (iv->extend == UNKNOWN)
    {
      iv->base = simplify_gen_unary (NEG, iv->extend_mode,
				     iv->base, iv->extend_mode);
      iv->step = simplify_gen_unary (NEG, iv->extend_mode,
				     iv->step, iv->extend_mode);
    }
  else
    {
      iv->delta = simplify_gen_unary (NEG, iv->extend_mode,
				      iv->delta, iv->extend_mode);
      iv->mult = simplify_gen_unary (NEG, iv->extend_mode,
				     iv->mult, iv->extend_mode);
    }

  return true;
}

/* Evaluates addition or subtraction (according to OP) of IV1 to IV0.  */

static bool
iv_add (struct rtx_iv *iv0, struct rtx_iv *iv1, enum rtx_code op)
{
  enum machine_mode mode;
  rtx arg;

  /* Extend the constant to extend_mode of the other operand if necessary.  */
  if (iv0->extend == UNKNOWN
      && iv0->mode == iv0->extend_mode
      && iv0->step == const0_rtx
      && GET_MODE_SIZE (iv0->extend_mode) < GET_MODE_SIZE (iv1->extend_mode))
    {
      iv0->extend_mode = iv1->extend_mode;
      iv0->base = simplify_gen_unary (ZERO_EXTEND, iv0->extend_mode,
				      iv0->base, iv0->mode);
    }
  if (iv1->extend == UNKNOWN
      && iv1->mode == iv1->extend_mode
      && iv1->step == const0_rtx
      && GET_MODE_SIZE (iv1->extend_mode) < GET_MODE_SIZE (iv0->extend_mode))
    {
      iv1->extend_mode = iv0->extend_mode;
      iv1->base = simplify_gen_unary (ZERO_EXTEND, iv1->extend_mode,
				      iv1->base, iv1->mode);
    }

  mode = iv0->extend_mode;
  if (mode != iv1->extend_mode)
    return false;

  if (iv0->extend == UNKNOWN && iv1->extend == UNKNOWN)
    {
      if (iv0->mode != iv1->mode)
	return false;

      iv0->base = simplify_gen_binary (op, mode, iv0->base, iv1->base);
      iv0->step = simplify_gen_binary (op, mode, iv0->step, iv1->step);

      return true;
    }

  /* Handle addition of constant.  */
  if (iv1->extend == UNKNOWN
      && iv1->mode == mode
      && iv1->step == const0_rtx)
    {
      iv0->delta = simplify_gen_binary (op, mode, iv0->delta, iv1->base);
      return true;
    }

  if (iv0->extend == UNKNOWN
      && iv0->mode == mode
      && iv0->step == const0_rtx)
    {
      arg = iv0->base;
      *iv0 = *iv1;
      if (op == MINUS
	  && !iv_neg (iv0))
	return false;

      iv0->delta = simplify_gen_binary (PLUS, mode, iv0->delta, arg);
      return true;
    }

  return false;
}

/* Evaluates multiplication of IV by constant CST.  */

static bool
iv_mult (struct rtx_iv *iv, rtx mby)
{
  enum machine_mode mode = iv->extend_mode;

  if (GET_MODE (mby) != VOIDmode
      && GET_MODE (mby) != mode)
    return false;

  if (iv->extend == UNKNOWN)
    {
      iv->base = simplify_gen_binary (MULT, mode, iv->base, mby);
      iv->step = simplify_gen_binary (MULT, mode, iv->step, mby);
    }
  else
    {
      iv->delta = simplify_gen_binary (MULT, mode, iv->delta, mby);
      iv->mult = simplify_gen_binary (MULT, mode, iv->mult, mby);
    }

  return true;
}

/* Evaluates shift of IV by constant CST.  */

static bool
iv_shift (struct rtx_iv *iv, rtx mby)
{
  enum machine_mode mode = iv->extend_mode;

  if (GET_MODE (mby) != VOIDmode
      && GET_MODE (mby) != mode)
    return false;

  if (iv->extend == UNKNOWN)
    {
      iv->base = simplify_gen_binary (ASHIFT, mode, iv->base, mby);
      iv->step = simplify_gen_binary (ASHIFT, mode, iv->step, mby);
    }
  else
    {
      iv->delta = simplify_gen_binary (ASHIFT, mode, iv->delta, mby);
      iv->mult = simplify_gen_binary (ASHIFT, mode, iv->mult, mby);
    }

  return true;
}

/* The recursive part of get_biv_step.  Gets the value of the single value
   defined by DEF wrto initial value of REG inside loop, in shape described
   at get_biv_step.  */

static bool
get_biv_step_1 (df_ref def, rtx reg,
		rtx *inner_step, enum machine_mode *inner_mode,
		enum rtx_code *extend, enum machine_mode outer_mode,
		rtx *outer_step)
{
  rtx set, rhs, op0 = NULL_RTX, op1 = NULL_RTX;
  rtx next, nextr, tmp;
  enum rtx_code code;
  rtx insn = DF_REF_INSN (def);
  df_ref next_def;
  enum iv_grd_result res;

  set = single_set (insn);
  if (!set)
    return false;

  rhs = find_reg_equal_equiv_note (insn);
  if (rhs)
    rhs = XEXP (rhs, 0);
  else
    rhs = SET_SRC (set);

  code = GET_CODE (rhs);
  switch (code)
    {
    case SUBREG:
    case REG:
      next = rhs;
      break;

    case PLUS:
    case MINUS:
      op0 = XEXP (rhs, 0);
      op1 = XEXP (rhs, 1);

      if (code == PLUS && CONSTANT_P (op0))
	{
	  tmp = op0; op0 = op1; op1 = tmp;
	}

      if (!simple_reg_p (op0)
	  || !CONSTANT_P (op1))
	return false;

      if (GET_MODE (rhs) != outer_mode)
	{
	  /* ppc64 uses expressions like

	     (set x:SI (plus:SI (subreg:SI y:DI) 1)).

	     this is equivalent to

	     (set x':DI (plus:DI y:DI 1))
	     (set x:SI (subreg:SI (x':DI)).  */
	  if (GET_CODE (op0) != SUBREG)
	    return false;
	  if (GET_MODE (SUBREG_REG (op0)) != outer_mode)
	    return false;
	}

      next = op0;
      break;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      if (GET_MODE (rhs) != outer_mode)
	return false;

      op0 = XEXP (rhs, 0);
      if (!simple_reg_p (op0))
	return false;

      next = op0;
      break;

    default:
      return false;
    }

  if (GET_CODE (next) == SUBREG)
    {
      if (!subreg_lowpart_p (next))
	return false;

      nextr = SUBREG_REG (next);
      if (GET_MODE (nextr) != outer_mode)
	return false;
    }
  else
    nextr = next;

  res = iv_get_reaching_def (insn, nextr, &next_def);

  if (res == GRD_INVALID || res == GRD_INVARIANT)
    return false;

  if (res == GRD_MAYBE_BIV)
    {
      if (!rtx_equal_p (nextr, reg))
	return false;

      *inner_step = const0_rtx;
      *extend = UNKNOWN;
      *inner_mode = outer_mode;
      *outer_step = const0_rtx;
    }
  else if (!get_biv_step_1 (next_def, reg,
			    inner_step, inner_mode, extend, outer_mode,
			    outer_step))
    return false;

  if (GET_CODE (next) == SUBREG)
    {
      enum machine_mode amode = GET_MODE (next);

      if (GET_MODE_SIZE (amode) > GET_MODE_SIZE (*inner_mode))
	return false;

      *inner_mode = amode;
      *inner_step = simplify_gen_binary (PLUS, outer_mode,
					 *inner_step, *outer_step);
      *outer_step = const0_rtx;
      *extend = UNKNOWN;
    }

  switch (code)
    {
    case REG:
    case SUBREG:
      break;

    case PLUS:
    case MINUS:
      if (*inner_mode == outer_mode
	  /* See comment in previous switch.  */
	  || GET_MODE (rhs) != outer_mode)
	*inner_step = simplify_gen_binary (code, outer_mode,
					   *inner_step, op1);
      else
	*outer_step = simplify_gen_binary (code, outer_mode,
					   *outer_step, op1);
      break;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      gcc_assert (GET_MODE (op0) == *inner_mode
		  && *extend == UNKNOWN
		  && *outer_step == const0_rtx);

      *extend = code;
      break;

    default:
      return false;
    }

  return true;
}

/* Gets the operation on register REG inside loop, in shape

   OUTER_STEP + EXTEND_{OUTER_MODE} (SUBREG_{INNER_MODE} (REG + INNER_STEP))

   If the operation cannot be described in this shape, return false.
   LAST_DEF is the definition of REG that dominates loop latch.  */

static bool
get_biv_step (df_ref last_def, rtx reg, rtx *inner_step,
	      enum machine_mode *inner_mode, enum rtx_code *extend,
	      enum machine_mode *outer_mode, rtx *outer_step)
{
  *outer_mode = GET_MODE (reg);

  if (!get_biv_step_1 (last_def, reg,
		       inner_step, inner_mode, extend, *outer_mode,
		       outer_step))
    return false;

  gcc_assert ((*inner_mode == *outer_mode) != (*extend != UNKNOWN));
  gcc_assert (*inner_mode != *outer_mode || *outer_step == const0_rtx);

  return true;
}

/* Records information that DEF is induction variable IV.  */

static void
record_iv (df_ref def, struct rtx_iv *iv)
{
  struct rtx_iv *recorded_iv = XNEW (struct rtx_iv);

  *recorded_iv = *iv;
  check_iv_ref_table_size ();
  DF_REF_IV_SET (def, recorded_iv);
}

/* If DEF was already analyzed for bivness, store the description of the biv to
   IV and return true.  Otherwise return false.  */

static bool
analyzed_for_bivness_p (rtx def, struct rtx_iv *iv)
{
  struct biv_entry *biv =
    (struct biv_entry *) htab_find_with_hash (bivs, def, REGNO (def));

  if (!biv)
    return false;

  *iv = biv->iv;
  return true;
}

static void
record_biv (rtx def, struct rtx_iv *iv)
{
  struct biv_entry *biv = XNEW (struct biv_entry);
  void **slot = htab_find_slot_with_hash (bivs, def, REGNO (def), INSERT);

  biv->regno = REGNO (def);
  biv->iv = *iv;
  gcc_assert (!*slot);
  *slot = biv;
}

/* Determines whether DEF is a biv and if so, stores its description
   to *IV.  */

static bool
iv_analyze_biv (rtx def, struct rtx_iv *iv)
{
  rtx inner_step, outer_step;
  enum machine_mode inner_mode, outer_mode;
  enum rtx_code extend;
  df_ref last_def;

  if (dump_file)
    {
      fprintf (dump_file, "Analyzing ");
      print_rtl (dump_file, def);
      fprintf (dump_file, " for bivness.\n");
    }

  if (!REG_P (def))
    {
      if (!CONSTANT_P (def))
	return false;

      return iv_constant (iv, def, VOIDmode);
    }

  if (!latch_dominating_def (def, &last_def))
    {
      if (dump_file)
	fprintf (dump_file, "  not simple.\n");
      return false;
    }

  if (!last_def)
    return iv_constant (iv, def, VOIDmode);

  if (analyzed_for_bivness_p (def, iv))
    {
      if (dump_file)
	fprintf (dump_file, "  already analysed.\n");
      return iv->base != NULL_RTX;
    }

  if (!get_biv_step (last_def, def, &inner_step, &inner_mode, &extend,
		     &outer_mode, &outer_step))
    {
      iv->base = NULL_RTX;
      goto end;
    }

  /* Loop transforms base to es (base + inner_step) + outer_step,
     where es means extend of subreg between inner_mode and outer_mode.
     The corresponding induction variable is

     es ((base - outer_step) + i * (inner_step + outer_step)) + outer_step  */

  iv->base = simplify_gen_binary (MINUS, outer_mode, def, outer_step);
  iv->step = simplify_gen_binary (PLUS, outer_mode, inner_step, outer_step);
  iv->mode = inner_mode;
  iv->extend_mode = outer_mode;
  iv->extend = extend;
  iv->mult = const1_rtx;
  iv->delta = outer_step;
  iv->first_special = inner_mode != outer_mode;

 end:
  if (dump_file)
    {
      fprintf (dump_file, "  ");
      dump_iv_info (dump_file, iv);
      fprintf (dump_file, "\n");
    }

  record_biv (def, iv);
  return iv->base != NULL_RTX;
}

/* Analyzes expression RHS used at INSN and stores the result to *IV.
   The mode of the induction variable is MODE.  */

bool
iv_analyze_expr (rtx insn, rtx rhs, enum machine_mode mode, struct rtx_iv *iv)
{
  rtx mby = NULL_RTX, tmp;
  rtx op0 = NULL_RTX, op1 = NULL_RTX;
  struct rtx_iv iv0, iv1;
  enum rtx_code code = GET_CODE (rhs);
  enum machine_mode omode = mode;

  iv->mode = VOIDmode;
  iv->base = NULL_RTX;
  iv->step = NULL_RTX;

  gcc_assert (GET_MODE (rhs) == mode || GET_MODE (rhs) == VOIDmode);

  if (CONSTANT_P (rhs)
      || REG_P (rhs)
      || code == SUBREG)
    {
      if (!iv_analyze_op (insn, rhs, iv))
	return false;

      if (iv->mode == VOIDmode)
	{
	  iv->mode = mode;
	  iv->extend_mode = mode;
	}

      return true;
    }

  switch (code)
    {
    case REG:
      op0 = rhs;
      break;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
    case NEG:
      op0 = XEXP (rhs, 0);
      omode = GET_MODE (op0);
      break;

    case PLUS:
    case MINUS:
      op0 = XEXP (rhs, 0);
      op1 = XEXP (rhs, 1);
      break;

    case MULT:
      op0 = XEXP (rhs, 0);
      mby = XEXP (rhs, 1);
      if (!CONSTANT_P (mby))
	{
	  tmp = op0;
	  op0 = mby;
	  mby = tmp;
	}
      if (!CONSTANT_P (mby))
	return false;
      break;

    case ASHIFT:
      op0 = XEXP (rhs, 0);
      mby = XEXP (rhs, 1);
      if (!CONSTANT_P (mby))
	return false;
      break;

    default:
      return false;
    }

  if (op0
      && !iv_analyze_expr (insn, op0, omode, &iv0))
    return false;

  if (op1
      && !iv_analyze_expr (insn, op1, omode, &iv1))
    return false;

  switch (code)
    {
    case SIGN_EXTEND:
    case ZERO_EXTEND:
      if (!iv_extend (&iv0, code, mode))
	return false;
      break;

    case NEG:
      if (!iv_neg (&iv0))
	return false;
      break;

    case PLUS:
    case MINUS:
      if (!iv_add (&iv0, &iv1, code))
	return false;
      break;

    case MULT:
      if (!iv_mult (&iv0, mby))
	return false;
      break;

    case ASHIFT:
      if (!iv_shift (&iv0, mby))
	return false;
      break;

    default:
      break;
    }

  *iv = iv0;
  return iv->base != NULL_RTX;
}

/* Analyzes iv DEF and stores the result to *IV.  */

static bool
iv_analyze_def (df_ref def, struct rtx_iv *iv)
{
  rtx insn = DF_REF_INSN (def);
  rtx reg = DF_REF_REG (def);
  rtx set, rhs;

  if (dump_file)
    {
      fprintf (dump_file, "Analyzing def of ");
      print_rtl (dump_file, reg);
      fprintf (dump_file, " in insn ");
      print_rtl_single (dump_file, insn);
    }

  check_iv_ref_table_size ();
  if (DF_REF_IV (def))
    {
      if (dump_file)
	fprintf (dump_file, "  already analysed.\n");
      *iv = *DF_REF_IV (def);
      return iv->base != NULL_RTX;
    }

  iv->mode = VOIDmode;
  iv->base = NULL_RTX;
  iv->step = NULL_RTX;

  if (!REG_P (reg))
    return false;

  set = single_set (insn);
  if (!set)
    return false;

  if (!REG_P (SET_DEST (set)))
    return false;

  gcc_assert (SET_DEST (set) == reg);
  rhs = find_reg_equal_equiv_note (insn);
  if (rhs)
    rhs = XEXP (rhs, 0);
  else
    rhs = SET_SRC (set);

  iv_analyze_expr (insn, rhs, GET_MODE (reg), iv);
  record_iv (def, iv);

  if (dump_file)
    {
      print_rtl (dump_file, reg);
      fprintf (dump_file, " in insn ");
      print_rtl_single (dump_file, insn);
      fprintf (dump_file, "  is ");
      dump_iv_info (dump_file, iv);
      fprintf (dump_file, "\n");
    }

  return iv->base != NULL_RTX;
}

/* Analyzes operand OP of INSN and stores the result to *IV.  */

static bool
iv_analyze_op (rtx insn, rtx op, struct rtx_iv *iv)
{
  df_ref def = NULL;
  enum iv_grd_result res;

  if (dump_file)
    {
      fprintf (dump_file, "Analyzing operand ");
      print_rtl (dump_file, op);
      fprintf (dump_file, " of insn ");
      print_rtl_single (dump_file, insn);
    }

  if (function_invariant_p (op))
    res = GRD_INVARIANT;
  else if (GET_CODE (op) == SUBREG)
    {
      if (!subreg_lowpart_p (op))
	return false;

      if (!iv_analyze_op (insn, SUBREG_REG (op), iv))
	return false;

      return iv_subreg (iv, GET_MODE (op));
    }
  else
    {
      res = iv_get_reaching_def (insn, op, &def);
      if (res == GRD_INVALID)
	{
	  if (dump_file)
	    fprintf (dump_file, "  not simple.\n");
	  return false;
	}
    }

  if (res == GRD_INVARIANT)
    {
      iv_constant (iv, op, VOIDmode);

      if (dump_file)
	{
	  fprintf (dump_file, "  ");
	  dump_iv_info (dump_file, iv);
	  fprintf (dump_file, "\n");
	}
      return true;
    }

  if (res == GRD_MAYBE_BIV)
    return iv_analyze_biv (op, iv);

  return iv_analyze_def (def, iv);
}

/* Analyzes value VAL at INSN and stores the result to *IV.  */

bool
iv_analyze (rtx insn, rtx val, struct rtx_iv *iv)
{
  rtx reg;

  /* We must find the insn in that val is used, so that we get to UD chains.
     Since the function is sometimes called on result of get_condition,
     this does not necessarily have to be directly INSN; scan also the
     following insns.  */
  if (simple_reg_p (val))
    {
      if (GET_CODE (val) == SUBREG)
	reg = SUBREG_REG (val);
      else
	reg = val;

      while (!df_find_use (insn, reg))
	insn = NEXT_INSN (insn);
    }

  return iv_analyze_op (insn, val, iv);
}

/* Analyzes definition of DEF in INSN and stores the result to IV.  */

bool
iv_analyze_result (rtx insn, rtx def, struct rtx_iv *iv)
{
  df_ref adef;

  adef = df_find_def (insn, def);
  if (!adef)
    return false;

  return iv_analyze_def (adef, iv);
}

/* Checks whether definition of register REG in INSN is a basic induction
   variable.  IV analysis must have been initialized (via a call to
   iv_analysis_loop_init) for this function to produce a result.  */

bool
biv_p (rtx insn, rtx reg)
{
  struct rtx_iv iv;
  df_ref def, last_def;

  if (!simple_reg_p (reg))
    return false;

  def = df_find_def (insn, reg);
  gcc_assert (def != NULL);
  if (!latch_dominating_def (reg, &last_def))
    return false;
  if (last_def != def)
    return false;

  if (!iv_analyze_biv (reg, &iv))
    return false;

  return iv.step != const0_rtx;
}

/* Calculates value of IV at ITERATION-th iteration.  */

rtx
get_iv_value (struct rtx_iv *iv, rtx iteration)
{
  rtx val;

  /* We would need to generate some if_then_else patterns, and so far
     it is not needed anywhere.  */
  gcc_assert (!iv->first_special);

  if (iv->step != const0_rtx && iteration != const0_rtx)
    val = simplify_gen_binary (PLUS, iv->extend_mode, iv->base,
			       simplify_gen_binary (MULT, iv->extend_mode,
						    iv->step, iteration));
  else
    val = iv->base;

  if (iv->extend_mode == iv->mode)
    return val;

  val = lowpart_subreg (iv->mode, val, iv->extend_mode);

  if (iv->extend == UNKNOWN)
    return val;

  val = simplify_gen_unary (iv->extend, iv->extend_mode, val, iv->mode);
  val = simplify_gen_binary (PLUS, iv->extend_mode, iv->delta,
			     simplify_gen_binary (MULT, iv->extend_mode,
						  iv->mult, val));

  return val;
}

/* Free the data for an induction variable analysis.  */

void
iv_analysis_done (void)
{
  if (!clean_slate)
    {
      clear_iv_info ();
      clean_slate = true;
      df_finish_pass (true);
      htab_delete (bivs);
      free (iv_ref_table);
      iv_ref_table = NULL;
      iv_ref_table_size = 0;
      bivs = NULL;
    }
}

/* Computes inverse to X modulo (1 << MOD).  */

static unsigned HOST_WIDEST_INT
inverse (unsigned HOST_WIDEST_INT x, int mod)
{
  unsigned HOST_WIDEST_INT mask =
	  ((unsigned HOST_WIDEST_INT) 1 << (mod - 1) << 1) - 1;
  unsigned HOST_WIDEST_INT rslt = 1;
  int i;

  for (i = 0; i < mod - 1; i++)
    {
      rslt = (rslt * x) & mask;
      x = (x * x) & mask;
    }

  return rslt;
}

/* Checks whether register *REG is in set ALT.  Callback for for_each_rtx.  */

static int
altered_reg_used (rtx *reg, void *alt)
{
  if (!REG_P (*reg))
    return 0;

  return REGNO_REG_SET_P ((bitmap) alt, REGNO (*reg));
}

/* Marks registers altered by EXPR in set ALT.  */

static void
mark_altered (rtx expr, const_rtx by ATTRIBUTE_UNUSED, void *alt)
{
  if (GET_CODE (expr) == SUBREG)
    expr = SUBREG_REG (expr);
  if (!REG_P (expr))
    return;

  SET_REGNO_REG_SET ((bitmap) alt, REGNO (expr));
}

/* Checks whether RHS is simple enough to process.  */

static bool
simple_rhs_p (rtx rhs)
{
  rtx op0, op1;

  if (function_invariant_p (rhs)
      || (REG_P (rhs) && !HARD_REGISTER_P (rhs)))
    return true;

  switch (GET_CODE (rhs))
    {
    case PLUS:
    case MINUS:
    case AND:
      op0 = XEXP (rhs, 0);
      op1 = XEXP (rhs, 1);
      /* Allow reg OP const and reg OP reg.  */
      if (!(REG_P (op0) && !HARD_REGISTER_P (op0))
	  && !function_invariant_p (op0))
	return false;
      if (!(REG_P (op1) && !HARD_REGISTER_P (op1))
	  && !function_invariant_p (op1))
	return false;

      return true;

    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
    case MULT:
      op0 = XEXP (rhs, 0);
      op1 = XEXP (rhs, 1);
      /* Allow reg OP const.  */
      if (!(REG_P (op0) && !HARD_REGISTER_P (op0)))
	return false;
      if (!function_invariant_p (op1))
	return false;

      return true;

    default:
      return false;
    }
}

/* If REG has a single definition, replace it with its known value in EXPR.
   Callback for for_each_rtx.  */

static int
replace_single_def_regs (rtx *reg, void *expr1)
{
  unsigned regno;
  df_ref adef;
  rtx set, src;
  rtx *expr = (rtx *)expr1;

  if (!REG_P (*reg))
    return 0;

  regno = REGNO (*reg);
  for (;;)
    {
      rtx note;
      adef = DF_REG_DEF_CHAIN (regno);
      if (adef == NULL || DF_REF_NEXT_REG (adef) != NULL
	    || DF_REF_IS_ARTIFICIAL (adef))
	return -1;

      set = single_set (DF_REF_INSN (adef));
      if (set == NULL || !REG_P (SET_DEST (set))
	  || REGNO (SET_DEST (set)) != regno)
	return -1;

      note = find_reg_equal_equiv_note (DF_REF_INSN (adef));

      if (note && function_invariant_p (XEXP (note, 0)))
	{
	  src = XEXP (note, 0);
	  break;
	}
      src = SET_SRC (set);

      if (REG_P (src))
	{
	  regno = REGNO (src);
	  continue;
	}
      break;
    }
  if (!function_invariant_p (src))
    return -1;

  *expr = simplify_replace_rtx (*expr, *reg, src);
  return 1;
}

/* A subroutine of simplify_using_initial_values, this function examines INSN
   to see if it contains a suitable set that we can use to make a replacement.
   If it is suitable, return true and set DEST and SRC to the lhs and rhs of
   the set; return false otherwise.  */

static bool
suitable_set_for_replacement (rtx insn, rtx *dest, rtx *src)
{
  rtx set = single_set (insn);
  rtx lhs = NULL_RTX, rhs;

  if (!set)
    return false;

  lhs = SET_DEST (set);
  if (!REG_P (lhs))
    return false;

  rhs = find_reg_equal_equiv_note (insn);
  if (rhs)
    rhs = XEXP (rhs, 0);
  else
    rhs = SET_SRC (set);

  if (!simple_rhs_p (rhs))
    return false;

  *dest = lhs;
  *src = rhs;
  return true;
}

/* Using the data returned by suitable_set_for_replacement, replace DEST
   with SRC in *EXPR and return the new expression.  Also call
   replace_single_def_regs if the replacement changed something.  */
static void
replace_in_expr (rtx *expr, rtx dest, rtx src)
{
  rtx old = *expr;
  *expr = simplify_replace_rtx (*expr, dest, src);
  if (old == *expr)
    return;
  while (for_each_rtx (expr, replace_single_def_regs, expr) != 0)
    continue;
}

/* Checks whether A implies B.  */

static bool
implies_p (rtx a, rtx b)
{
  rtx op0, op1, opb0, opb1, r;
  enum machine_mode mode;

  if (GET_CODE (a) == EQ)
    {
      op0 = XEXP (a, 0);
      op1 = XEXP (a, 1);

      if (REG_P (op0))
	{
	  r = simplify_replace_rtx (b, op0, op1);
	  if (r == const_true_rtx)
	    return true;
	}

      if (REG_P (op1))
	{
	  r = simplify_replace_rtx (b, op1, op0);
	  if (r == const_true_rtx)
	    return true;
	}
    }

  if (b == const_true_rtx)
    return true;

  if ((GET_RTX_CLASS (GET_CODE (a)) != RTX_COMM_COMPARE
       && GET_RTX_CLASS (GET_CODE (a)) != RTX_COMPARE)
      || (GET_RTX_CLASS (GET_CODE (b)) != RTX_COMM_COMPARE
	  && GET_RTX_CLASS (GET_CODE (b)) != RTX_COMPARE))
    return false;

  op0 = XEXP (a, 0);
  op1 = XEXP (a, 1);
  opb0 = XEXP (b, 0);
  opb1 = XEXP (b, 1);

  mode = GET_MODE (op0);
  if (mode != GET_MODE (opb0))
    mode = VOIDmode;
  else if (mode == VOIDmode)
    {
      mode = GET_MODE (op1);
      if (mode != GET_MODE (opb1))
	mode = VOIDmode;
    }

  /* A < B implies A + 1 <= B.  */
  if ((GET_CODE (a) == GT || GET_CODE (a) == LT)
      && (GET_CODE (b) == GE || GET_CODE (b) == LE))
    {

      if (GET_CODE (a) == GT)
	{
	  r = op0;
	  op0 = op1;
	  op1 = r;
	}

      if (GET_CODE (b) == GE)
	{
	  r = opb0;
	  opb0 = opb1;
	  opb1 = r;
	}

      if (SCALAR_INT_MODE_P (mode)
	  && rtx_equal_p (op1, opb1)
	  && simplify_gen_binary (MINUS, mode, opb0, op0) == const1_rtx)
	return true;
      return false;
    }

  /* A < B or A > B imply A != B.  TODO: Likewise
     A + n < B implies A != B + n if neither wraps.  */
  if (GET_CODE (b) == NE
      && (GET_CODE (a) == GT || GET_CODE (a) == GTU
	  || GET_CODE (a) == LT || GET_CODE (a) == LTU))
    {
      if (rtx_equal_p (op0, opb0)
	  && rtx_equal_p (op1, opb1))
	return true;
    }

  /* For unsigned comparisons, A != 0 implies A > 0 and A >= 1.  */
  if (GET_CODE (a) == NE
      && op1 == const0_rtx)
    {
      if ((GET_CODE (b) == GTU
	   && opb1 == const0_rtx)
	  || (GET_CODE (b) == GEU
	      && opb1 == const1_rtx))
	return rtx_equal_p (op0, opb0);
    }

  /* A != N is equivalent to A - (N + 1) <u -1.  */
  if (GET_CODE (a) == NE
      && CONST_INT_P (op1)
      && GET_CODE (b) == LTU
      && opb1 == constm1_rtx
      && GET_CODE (opb0) == PLUS
      && CONST_INT_P (XEXP (opb0, 1))
      /* Avoid overflows.  */
      && ((unsigned HOST_WIDE_INT) INTVAL (XEXP (opb0, 1))
	  != ((unsigned HOST_WIDE_INT)1
	      << (HOST_BITS_PER_WIDE_INT - 1)) - 1)
      && INTVAL (XEXP (opb0, 1)) + 1 == -INTVAL (op1))
    return rtx_equal_p (op0, XEXP (opb0, 0));

  /* Likewise, A != N implies A - N > 0.  */
  if (GET_CODE (a) == NE
      && CONST_INT_P (op1))
    {
      if (GET_CODE (b) == GTU
	  && GET_CODE (opb0) == PLUS
	  && opb1 == const0_rtx
	  && CONST_INT_P (XEXP (opb0, 1))
	  /* Avoid overflows.  */
	  && ((unsigned HOST_WIDE_INT) INTVAL (XEXP (opb0, 1))
	      != ((unsigned HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT - 1)))
	  && rtx_equal_p (XEXP (opb0, 0), op0))
	return INTVAL (op1) == -INTVAL (XEXP (opb0, 1));
      if (GET_CODE (b) == GEU
	  && GET_CODE (opb0) == PLUS
	  && opb1 == const1_rtx
	  && CONST_INT_P (XEXP (opb0, 1))
	  /* Avoid overflows.  */
	  && ((unsigned HOST_WIDE_INT) INTVAL (XEXP (opb0, 1))
	      != ((unsigned HOST_WIDE_INT) 1 << (HOST_BITS_PER_WIDE_INT - 1)))
	  && rtx_equal_p (XEXP (opb0, 0), op0))
	return INTVAL (op1) == -INTVAL (XEXP (opb0, 1));
    }

  /* A >s X, where X is positive, implies A <u Y, if Y is negative.  */
  if ((GET_CODE (a) == GT || GET_CODE (a) == GE)
      && CONST_INT_P (op1)
      && ((GET_CODE (a) == GT && op1 == constm1_rtx)
	  || INTVAL (op1) >= 0)
      && GET_CODE (b) == LTU
      && CONST_INT_P (opb1)
      && rtx_equal_p (op0, opb0))
    return INTVAL (opb1) < 0;

  return false;
}

/* Canonicalizes COND so that

   (1) Ensure that operands are ordered according to
       swap_commutative_operands_p.
   (2) (LE x const) will be replaced with (LT x <const+1>) and similarly
       for GE, GEU, and LEU.  */

rtx
canon_condition (rtx cond)
{
  rtx tem;
  rtx op0, op1;
  enum rtx_code code;
  enum machine_mode mode;

  code = GET_CODE (cond);
  op0 = XEXP (cond, 0);
  op1 = XEXP (cond, 1);

  if (swap_commutative_operands_p (op0, op1))
    {
      code = swap_condition (code);
      tem = op0;
      op0 = op1;
      op1 = tem;
    }

  mode = GET_MODE (op0);
  if (mode == VOIDmode)
    mode = GET_MODE (op1);
  gcc_assert (mode != VOIDmode);

  if (CONST_INT_P (op1)
      && GET_MODE_CLASS (mode) != MODE_CC
      && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
    {
      HOST_WIDE_INT const_val = INTVAL (op1);
      unsigned HOST_WIDE_INT uconst_val = const_val;
      unsigned HOST_WIDE_INT max_val
	= (unsigned HOST_WIDE_INT) GET_MODE_MASK (mode);

      switch (code)
	{
	case LE:
	  if ((unsigned HOST_WIDE_INT) const_val != max_val >> 1)
	    code = LT, op1 = gen_int_mode (const_val + 1, GET_MODE (op0));
	  break;

	/* When cross-compiling, const_val might be sign-extended from
	   BITS_PER_WORD to HOST_BITS_PER_WIDE_INT */
	case GE:
	  if ((HOST_WIDE_INT) (const_val & max_val)
	      != (((HOST_WIDE_INT) 1
		   << (GET_MODE_BITSIZE (GET_MODE (op0)) - 1))))
	    code = GT, op1 = gen_int_mode (const_val - 1, mode);
	  break;

	case LEU:
	  if (uconst_val < max_val)
	    code = LTU, op1 = gen_int_mode (uconst_val + 1, mode);
	  break;

	case GEU:
	  if (uconst_val != 0)
	    code = GTU, op1 = gen_int_mode (uconst_val - 1, mode);
	  break;

	default:
	  break;
	}
    }

  if (op0 != XEXP (cond, 0)
      || op1 != XEXP (cond, 1)
      || code != GET_CODE (cond)
      || GET_MODE (cond) != SImode)
    cond = gen_rtx_fmt_ee (code, SImode, op0, op1);

  return cond;
}

/* Tries to use the fact that COND holds to simplify EXPR.  ALTERED is the
   set of altered regs.  */

void
simplify_using_condition (rtx cond, rtx *expr, regset altered)
{
}

/* Use relationship between A and *B to eventually eliminate *B.
   OP is the operation we consider.  */

static void
eliminate_implied_condition (enum rtx_code op, rtx a, rtx *b)
{
  switch (op)
    {
    case AND:
      /* If A implies *B, we may replace *B by true.  */
      if (implies_p (a, *b))
	*b = const_true_rtx;
      break;

    case IOR:
      /* If *B implies A, we may replace *B by false.  */
      if (implies_p (*b, a))
	*b = const0_rtx;
      break;

    default:
      gcc_unreachable ();
    }
}

/* Eliminates the conditions in TAIL that are implied by HEAD.  OP is the
   operation we consider.  */

static void
eliminate_implied_conditions (enum rtx_code op, rtx *head, rtx tail)
{
  rtx elt;

  for (elt = tail; elt; elt = XEXP (elt, 1))
    eliminate_implied_condition (op, *head, &XEXP (elt, 0));
  for (elt = tail; elt; elt = XEXP (elt, 1))
    eliminate_implied_condition (op, XEXP (elt, 0), head);
}

/* Simplifies *EXPR using initial values at the start of the LOOP.  If *EXPR
   is a list, its elements are assumed to be combined using OP.  */

static void
simplify_using_initial_values (struct loop *loop, enum rtx_code op, rtx *expr)
{
  gcc_unreachable ();
}

/* Transforms invariant IV into MODE.  Adds assumptions based on the fact
   that IV occurs as left operands of comparison COND and its signedness
   is SIGNED_P to DESC.  */

static void
shorten_into_mode (struct rtx_iv *iv, enum machine_mode mode,
		   enum rtx_code cond, bool signed_p, struct niter_desc *desc)
{
  rtx mmin, mmax, cond_over, cond_under;

  get_mode_bounds (mode, signed_p, iv->extend_mode, &mmin, &mmax);
  cond_under = simplify_gen_relational (LT, SImode, iv->extend_mode,
					iv->base, mmin);
  cond_over = simplify_gen_relational (GT, SImode, iv->extend_mode,
				       iv->base, mmax);

  switch (cond)
    {
      case LE:
      case LT:
      case LEU:
      case LTU:
	if (cond_under != const0_rtx)
	  desc->infinite =
		  alloc_EXPR_LIST (0, cond_under, desc->infinite);
	if (cond_over != const0_rtx)
	  desc->noloop_assumptions =
		  alloc_EXPR_LIST (0, cond_over, desc->noloop_assumptions);
	break;

      case GE:
      case GT:
      case GEU:
      case GTU:
	if (cond_over != const0_rtx)
	  desc->infinite =
		  alloc_EXPR_LIST (0, cond_over, desc->infinite);
	if (cond_under != const0_rtx)
	  desc->noloop_assumptions =
		  alloc_EXPR_LIST (0, cond_under, desc->noloop_assumptions);
	break;

      case NE:
	if (cond_over != const0_rtx)
	  desc->infinite =
		  alloc_EXPR_LIST (0, cond_over, desc->infinite);
	if (cond_under != const0_rtx)
	  desc->infinite =
		  alloc_EXPR_LIST (0, cond_under, desc->infinite);
	break;

      default:
	gcc_unreachable ();
    }

  iv->mode = mode;
  iv->extend = signed_p ? SIGN_EXTEND : ZERO_EXTEND;
}

/* Transforms IV0 and IV1 compared by COND so that they are both compared as
   subregs of the same mode if possible (sometimes it is necessary to add
   some assumptions to DESC).  */

static bool
canonicalize_iv_subregs (struct rtx_iv *iv0, struct rtx_iv *iv1,
			 enum rtx_code cond, struct niter_desc *desc)
{
  enum machine_mode comp_mode;
  bool signed_p;

  /* If the ivs behave specially in the first iteration, or are
     added/multiplied after extending, we ignore them.  */
  if (iv0->first_special || iv0->mult != const1_rtx || iv0->delta != const0_rtx)
    return false;
  if (iv1->first_special || iv1->mult != const1_rtx || iv1->delta != const0_rtx)
    return false;

  /* If there is some extend, it must match signedness of the comparison.  */
  switch (cond)
    {
      case LE:
      case LT:
	if (iv0->extend == ZERO_EXTEND
	    || iv1->extend == ZERO_EXTEND)
	  return false;
	signed_p = true;
	break;

      case LEU:
      case LTU:
	if (iv0->extend == SIGN_EXTEND
	    || iv1->extend == SIGN_EXTEND)
	  return false;
	signed_p = false;
	break;

      case NE:
	if (iv0->extend != UNKNOWN
	    && iv1->extend != UNKNOWN
	    && iv0->extend != iv1->extend)
	  return false;

	signed_p = false;
	if (iv0->extend != UNKNOWN)
	  signed_p = iv0->extend == SIGN_EXTEND;
	if (iv1->extend != UNKNOWN)
	  signed_p = iv1->extend == SIGN_EXTEND;
	break;

      default:
	gcc_unreachable ();
    }

  /* Values of both variables should be computed in the same mode.  These
     might indeed be different, if we have comparison like

     (compare (subreg:SI (iv0)) (subreg:SI (iv1)))

     and iv0 and iv1 are both ivs iterating in SI mode, but calculated
     in different modes.  This does not seem impossible to handle, but
     it hardly ever occurs in practice.

     The only exception is the case when one of operands is invariant.
     For example pentium 3 generates comparisons like
     (lt (subreg:HI (reg:SI)) 100).  Here we assign HImode to 100, but we
     definitely do not want this prevent the optimization.  */
  comp_mode = iv0->extend_mode;
  if (GET_MODE_BITSIZE (comp_mode) < GET_MODE_BITSIZE (iv1->extend_mode))
    comp_mode = iv1->extend_mode;

  if (iv0->extend_mode != comp_mode)
    {
      if (iv0->mode != iv0->extend_mode
	  || iv0->step != const0_rtx)
	return false;

      iv0->base = simplify_gen_unary (signed_p ? SIGN_EXTEND : ZERO_EXTEND,
				      comp_mode, iv0->base, iv0->mode);
      iv0->extend_mode = comp_mode;
    }

  if (iv1->extend_mode != comp_mode)
    {
      if (iv1->mode != iv1->extend_mode
	  || iv1->step != const0_rtx)
	return false;

      iv1->base = simplify_gen_unary (signed_p ? SIGN_EXTEND : ZERO_EXTEND,
				      comp_mode, iv1->base, iv1->mode);
      iv1->extend_mode = comp_mode;
    }

  /* Check that both ivs belong to a range of a single mode.  If one of the
     operands is an invariant, we may need to shorten it into the common
     mode.  */
  if (iv0->mode == iv0->extend_mode
      && iv0->step == const0_rtx
      && iv0->mode != iv1->mode)
    shorten_into_mode (iv0, iv1->mode, cond, signed_p, desc);

  if (iv1->mode == iv1->extend_mode
      && iv1->step == const0_rtx
      && iv0->mode != iv1->mode)
    shorten_into_mode (iv1, iv0->mode, swap_condition (cond), signed_p, desc);

  if (iv0->mode != iv1->mode)
    return false;

  desc->mode = iv0->mode;
  desc->signed_p = signed_p;

  return true;
}

/* Tries to estimate the maximum number of iterations in LOOP, and store the
   result in DESC.  This function is called from iv_number_of_iterations with
   a number of fields in DESC already filled in.  OLD_NITER is the original
   expression for the number of iterations, before we tried to simplify it.  */

static unsigned HOST_WIDEST_INT
determine_max_iter (struct loop *loop, struct niter_desc *desc, rtx old_niter)
{
  rtx niter = desc->niter_expr;
  rtx mmin, mmax, cmp;
  unsigned HOST_WIDEST_INT nmax, inc;

  if (GET_CODE (niter) == AND
      && CONST_INT_P (XEXP (niter, 0)))
    {
      nmax = INTVAL (XEXP (niter, 0));
      if (!(nmax & (nmax + 1)))
	{
	  desc->niter_max = nmax;
	  return nmax;
	}
    }

  get_mode_bounds (desc->mode, desc->signed_p, desc->mode, &mmin, &mmax);
  nmax = INTVAL (mmax) - INTVAL (mmin);

  if (GET_CODE (niter) == UDIV)
    {
      if (!CONST_INT_P (XEXP (niter, 1)))
	{
	  desc->niter_max = nmax;
	  return nmax;
	}
      inc = INTVAL (XEXP (niter, 1));
      niter = XEXP (niter, 0);
    }
  else
    inc = 1;

  /* We could use a binary search here, but for now improving the upper
     bound by just one eliminates one important corner case.  */
  cmp = simplify_gen_relational (desc->signed_p ? LT : LTU, VOIDmode,
				 desc->mode, old_niter, mmax);
  simplify_using_initial_values (loop, UNKNOWN, &cmp);
  if (cmp == const_true_rtx)
    {
      nmax--;

      if (dump_file)
	fprintf (dump_file, ";; improved upper bound by one.\n");
    }
  desc->niter_max = nmax / inc;
  return nmax / inc;
}

/* Creates a simple loop description of LOOP if it was not computed
   already.  */

struct niter_desc *
get_simple_loop_desc (struct loop *loop)
{
  gcc_unreachable ();
  return 0;
}

/* Releases simple loop description for LOOP.  */

void
free_simple_loop_desc (struct loop *loop)
{
  struct niter_desc *desc = simple_loop_desc (loop);

  if (!desc)
    return;

  free (desc);
  loop->aux = NULL;
}
