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

/* Returns estimate on cost of computing SEQ.  */

static unsigned
seq_cost (rtx seq, bool speed)
{
  unsigned cost = 0;
  rtx set;

  gcc_unreachable ();
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

  gcc_unreachable ();
  return 57 * (hashval_t) e->mode + (hashval_t) (e->cst % 877);
}

/* Compares the hash table entries ENTRY1 and ENTRY2.  */

static int
mbc_entry_eq (const void *entry1, const void *entry2)
{
  const struct mbc_entry *e1 = (const struct mbc_entry *) entry1;
  const struct mbc_entry *e2 = (const struct mbc_entry *) entry2;

  gcc_unreachable ();
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

  gcc_unreachable ();

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
multiplier_allowed_in_address_p (HOST_WIDE_INT ratio, enum machine_mode mode,
				 addr_space_t as)
{
#define MAX_RATIO 128
  unsigned int data_index = (int) as * MAX_MACHINE_MODE + (int) mode;
  static VEC (sbitmap, heap) *valid_mult_list;
  sbitmap valid_mult;
  
  gcc_unreachable ();

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
