/* Move registers around to reduce number of move instructions needed.
   Copyright (C) 1987, 88, 89, 92, 93, 94, 1995 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* This module looks for cases where matching constraints would force
   an instruction to need a reload, and this reload would be a register
   to register move.  It then attempts to change the registers used by the
   instruction to avoid the move instruction.  */

#include "config.h"
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

/* Must precede rtl.h for FFS.  */
#include <stdio.h>

#include "rtl.h"
#include "insn-config.h"
#include "recog.h"
#include "output.h"
#include "reload.h"
#include "regs.h"

void
regmove_optimize (f, nregs, regmove_dump_file)
     rtx f;
     int nregs;
     FILE *regmove_dump_file;
{
#ifdef REGISTER_CONSTRAINTS
  rtx insn;
  int matches[MAX_RECOG_OPERANDS][MAX_RECOG_OPERANDS];
  int modified[MAX_RECOG_OPERANDS];
  int early_clobber[MAX_RECOG_OPERANDS];
  int commutative;

  /* A forward pass.  Replace output operands with input operands.  */

  if (regmove_dump_file)
    fprintf (regmove_dump_file, "Starting forward pass...\n");

  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	{
	  int insn_code_number = recog_memoized (insn);
	  int operand_number, match_number;
	  
	  if (insn_code_number < 0)
	    continue;

	  insn_extract (insn);
	  if (! constrain_operands (insn_code_number, 0))
	    continue;
	  
	  commutative = -1;

	  /* Must initialize this before the loop, because the code for
	     the commutative case may set matches for operands other than
	     the current one.  */
	  bzero (matches, sizeof (matches));

	  for (operand_number = 0;
	       operand_number < insn_n_operands[insn_code_number];
	       operand_number++)
	    {
	      int output_operand = 0;
	      int matching_operand = operand_number;
	      char *p, c;
	      int i = 0;

	      modified[operand_number] = 0;
	      early_clobber[operand_number] = 0;

	      for (p = insn_operand_constraint[insn_code_number][operand_number];
		   *p && i < which_alternative; p++)
		if (*p == ',')
		  i++;

	      while ((c = *p++) != '\0' && c != ',')
		switch (c)
		  {
		  case '=':
		    modified[operand_number] = 2;
		    break;
		  case '+':
		    modified[operand_number] = 1;
		    break;
		  case '&':
		    early_clobber[operand_number] = 1;
		    break;
		  case '%':
		    commutative = operand_number;
		    break;
		  case '0': case '1': case '2': case '3': case '4':
		  case '5': case '6': case '7': case '8': case '9':
		    c -= '0';
		    matches[operand_number][c] = 1;
		    if (commutative >= 0)
		      {
			if (c == commutative || c == commutative + 1)
			  {
			    int other = c + (c == commutative ? 1 : -1);
			    matches[operand_number][other] = 1;
			  }
			if (operand_number == commutative
			    || operand_number == commutative + 1)
			  {
			    int other = (operand_number
					 + (operand_number == commutative
					    ? 1 : -1));
			    matches[other][c] = 1;
			  }
		      }
		    break;
		  }
	    }

	  /* Now scan through the operands looking for a source operand
	     which is supposed to match the destination operand.
	     Then scan forward for an instruction which uses the dest operand.
	     If it dies there, then replace the dest in both operands with
	     the source operand.  */

	  for (operand_number = 0;
	       operand_number < insn_n_operands[insn_code_number];
	       operand_number++)
	    {
	      for (match_number = 0;
		   match_number < insn_n_operands[insn_code_number];
		   match_number++)
		{
		  rtx set, p, src, dst;
		  rtx src_note, dst_note;
		  int success = 0;
		  int num_calls = 0;

		  /* Nothing to do if the two operands aren't supposed to
		     match.  */
		  if (matches[operand_number][match_number] == 0)
		    continue;

		  src = recog_operand[operand_number];
		  dst = recog_operand[match_number];

		  if (GET_CODE (src) != REG
		      || REGNO (src) < FIRST_PSEUDO_REGISTER)
		    continue;

		  if (GET_CODE (dst) != REG
		      || REGNO (dst) < FIRST_PSEUDO_REGISTER)
		    continue;

		  /* If the operands already match, then there is nothing
		     to do.  */
		  if (operands_match_p (src, dst))
		    continue;

		  set = single_set (insn);
		  if (! set)
		    continue;

		  /* operand_number/src must be a read-only operand, and
		     match_operand/dst must be a write-only operand.  */
		  if (modified[match_number] != 2)
		    continue;

		  if (early_clobber[match_number] == 1)
		    continue;

		  if (modified[operand_number] != 0)
		    continue;

		  /* Make sure match_operand is the destination.  */
		  if (recog_operand[match_number] != SET_DEST (set))
		    continue;
	      
		  if (! (src_note = find_reg_note (insn, REG_DEAD, src)))
		    continue;

		  if (regmove_dump_file)
		    fprintf (regmove_dump_file,
			     "Could fix operand %d of insn %d matching operand %d.\n",
			     operand_number, INSN_UID (insn), match_number);

		  /* ??? If src is set once, and is set equal to a constant,
		     then do not use it for this optimization, as this would
		     make it no longer equivalent to a constant?  */

		  /* Scan forward to find the next instruction that uses
		     the output operand.  If the operand dies here, then
		     replace it in both instructions with operand_number.  */

		  for (p = NEXT_INSN (insn); p; p = NEXT_INSN (p))
		    {
		      if (GET_CODE (p) == CODE_LABEL
			  || GET_CODE (p) == JUMP_INSN
			  || (GET_CODE (p) == NOTE
			      && (NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_BEG
				  || NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_END)))
			break;

		      if (GET_RTX_CLASS (GET_CODE (p)) != 'i')
			continue;

		      if (reg_set_p (src, p) || reg_set_p (dst, p)
			  || (GET_CODE (PATTERN (p)) == USE
			      && reg_overlap_mentioned_p (src,
							  XEXP (PATTERN (p), 0))))
			break;

		      /* See if all of DST dies in P.  This test is slightly
			 more conservative than it needs to be.  */
		      if ((dst_note = find_regno_note (p, REG_DEAD, REGNO (dst)))
			  && GET_MODE (XEXP (dst_note, 0)) == GET_MODE (dst))
			{
			  if (validate_replace_rtx (dst, src, p))
			    {
			      if (validate_change (insn,
						   recog_operand_loc[match_number],
						   src, 0))
				success = 1;
			      else
				validate_replace_rtx (src, dst, p);
			    }
			  break;
			}

		      if (reg_overlap_mentioned_p (dst, PATTERN (p)))
			break;

		      /* If we have passed a call instruction, and the
			 pseudo-reg SRC is not already live across a call,
			 then don't perform the optimization.  */
		      if (GET_CODE (p) == CALL_INSN)
			{
			  num_calls++;

			  if (reg_n_calls_crossed[REGNO (src)] == 0)
			    break;
			}
		    }

		  if (success)
		    {
		      /* Remove the death note for DST from P.  */
		      remove_note (p, dst_note);
		      /* Move the death note for DST to INSN if it is used
			 there.  */
		      if (reg_overlap_mentioned_p (dst, PATTERN (insn)))
			{
			  XEXP (dst_note, 1) = REG_NOTES (insn);
			  REG_NOTES (insn) = dst_note;
			}

		      /* Move the death note for SRC from INSN to P.  */
		      remove_note (insn, src_note);
		      XEXP (src_note, 1) = REG_NOTES (p);
		      REG_NOTES (p) = src_note;

		      reg_n_sets[REGNO (src)]++;
		      reg_n_sets[REGNO (dst)]--;

		      reg_n_calls_crossed[REGNO (src)] += num_calls;
		      reg_n_calls_crossed[REGNO (dst)] -= num_calls;

		      /* ??? Must adjust reg_live_length, and reg_n_refs for
			 both registers.  Must keep track of loop_depth in
			 order to get reg_n_refs adjustment correct.  */

		      if (regmove_dump_file)
			fprintf (regmove_dump_file,
				 "Fixed operand %d of insn %d matching operand %d.\n",
				 operand_number, INSN_UID (insn), match_number);

		      goto done_forwards;
		    }
		}
	    }
	done_forwards:
	  ;
	}
    }

  /* A backward pass.  Replace input operands with output operands.  */

  if (regmove_dump_file)
    fprintf (regmove_dump_file, "Starting backward pass...\n");

  for (insn = get_last_insn (); insn; insn = PREV_INSN (insn))
    {
      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	{
	  int insn_code_number = recog_memoized (insn);
	  int operand_number, match_number;
	  
	  if (insn_code_number < 0)
	    continue;

	  insn_extract (insn);
	  if (! constrain_operands (insn_code_number, 0))
	    continue;
	  
	  commutative = -1;

	  /* Must initialize this before the loop, because the code for
	     the commutative case may set matches for operands other than
	     the current one.  */
	  bzero (matches, sizeof (matches));

	  for (operand_number = 0;
	       operand_number < insn_n_operands[insn_code_number];
	       operand_number++)
	    {
	      int output_operand = 0;
	      int matching_operand = operand_number;
	      char *p, c;
	      int i = 0;

	      modified[operand_number] = 0;
	      early_clobber[operand_number] = 0;

	      for (p = insn_operand_constraint[insn_code_number][operand_number];
		   *p && i < which_alternative; p++)
		if (*p == ',')
		  i++;

	      while ((c = *p++) != '\0' && c != ',')
		switch (c)
		  {
		  case '=':
		    modified[operand_number] = 2;
		    break;
		  case '+':
		    modified[operand_number] = 1;
		    break;
		  case '&':
		    early_clobber[operand_number] = 1;
		    break;
		  case '%':
		    commutative = operand_number;
		    break;
		  case '0': case '1': case '2': case '3': case '4':
		  case '5': case '6': case '7': case '8': case '9':
		    c -= '0';
		    matches[c][operand_number] = 1;
		    if (commutative >= 0)
		      {
			if (c == commutative || c == commutative + 1)
			  {
			    int other = c + (c == commutative ? 1 : -1);
			    matches[other][operand_number] = 1;
			  }
			if (operand_number == commutative
			    || operand_number == commutative + 1)
			  {
			    int other = (operand_number
					 + (operand_number == commutative
					    ? 1 : -1));
			    matches[c][other] = 1;
			  }
		      }
		    break;
		  }
	    }

	  /* Now scan through the operands looking for a destination operand
	     which is supposed to match a source operand.
	     Then scan backward for an instruction which sets the source
	     operand.  If safe, then replace the source operand with the
	     dest operand in both instructions.  */

	  for (operand_number = 0;
	       operand_number < insn_n_operands[insn_code_number];
	       operand_number++)
	    {
	      for (match_number = 0;
		   match_number < insn_n_operands[insn_code_number];
		   match_number++)
		{
		  rtx set, p, src, dst;
		  rtx src_note, dst_note;
		  int success = 0;
		  int num_calls = 0;

		  /* Nothing to do if the two operands aren't supposed to
		     match.  */
		  if (matches[operand_number][match_number] == 0)
		    continue;

		  dst = recog_operand[operand_number];
		  src = recog_operand[match_number];

		  if (GET_CODE (src) != REG
		      || REGNO (src) < FIRST_PSEUDO_REGISTER)
		    continue;

		  if (GET_CODE (dst) != REG
		      || REGNO (dst) < FIRST_PSEUDO_REGISTER)
		    continue;

		  /* If the operands already match, then there is nothing
		     to do.  */
		  if (operands_match_p (src, dst))
		    continue;

		  set = single_set (insn);
		  if (! set)
		    continue;

		  /* operand_number/dst must be a write-only operand, and
		     match_operand/src must be a read-only operand.  */
		  if (modified[match_number] != 0)
		    continue;

		  if (early_clobber[operand_number] == 1)
		    continue;

		  if (modified[operand_number] != 2)
		    continue;

		  /* Make sure operand_number is the destination.  */
		  if (recog_operand[operand_number] != SET_DEST (set))
		    continue;
	      
		  if (! (src_note = find_reg_note (insn, REG_DEAD, src)))
		    continue;

		  /* Can not modify an earlier insn to set dst if this insn
		     uses an old value in the source.  */
		  if (reg_overlap_mentioned_p (dst, SET_SRC (set)))
		    continue;

		  if (regmove_dump_file)
		    fprintf (regmove_dump_file,
			     "Could fix operand %d of insn %d matching operand %d.\n",
			     operand_number, INSN_UID (insn), match_number);

		  /* ??? If src is set once, and is set equal to a constant,
		     then do not use it for this optimization, as this would
		     make it no longer equivalent to a constant?  */

		  /* Scan backward to find the first instruction that uses
		     the input operand.  If the operand is set here, then
		     replace it in both instructions with operand_number.  */

		  for (p = PREV_INSN (insn); p; p = PREV_INSN (p))
		    {
		      rtx pset;

		      if (GET_CODE (p) == CODE_LABEL
			  || GET_CODE (p) == JUMP_INSN
			  || (GET_CODE (p) == NOTE
			      && (NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_BEG
				  || NOTE_LINE_NUMBER (p) == NOTE_INSN_LOOP_END)))
			break;

		      if (GET_RTX_CLASS (GET_CODE (p)) != 'i')
			continue;

		      /* ??? See if all of SRC is set in P.  This test is much
			 more conservative than it needs to be.  */
		      pset = single_set (p);
		      if (pset && SET_DEST (pset) == src)
			{
			  /* We use validate_replace_rtx, in case there
			     are multiple identical source operands.  All of
			     them have to be changed at the same time.  */
			  if (validate_replace_rtx (src, dst, insn))
			    {
			      if (validate_change (p, &SET_DEST (pset),
						   dst, 0))
				success = 1;
			      else
				{
				  /* Change all source operands back.
				     This modifies the dst as a side-effect.  */
				  validate_replace_rtx (dst, src, insn);
				  /* Now make sure the dst is right.  */
				  validate_change (insn,
						   recog_operand_loc[operand_number],
						   dst, 0);
				}
			    }
			  break;
			}

		      if (reg_overlap_mentioned_p (src, PATTERN (p))
			  || reg_overlap_mentioned_p (dst, PATTERN (p)))
			break;

		      /* If we have passed a call instruction, and the
			 pseudo-reg DST is not already live across a call,
			 then don't perform the optimization.  */
		      if (GET_CODE (p) == CALL_INSN)
			{
			  num_calls++;

			  if (reg_n_calls_crossed[REGNO (dst)] == 0)
			    break;
			}
		    }

		  if (success)
		    {
		      /* Remove the death note for SRC from INSN.  */
		      remove_note (insn, src_note);
		      /* Move the death note for SRC to P if it is used
			 there.  */
		      if (reg_overlap_mentioned_p (src, PATTERN (p)))
			{
			  XEXP (src_note, 1) = REG_NOTES (p);
			  REG_NOTES (p) = src_note;
			}
		      /* If there is a REG_DEAD note for DST on P, then remove
			 it, because DST is now set there.  */
		      if (dst_note = find_reg_note (p, REG_DEAD, dst))
			remove_note (p, dst_note);

		      reg_n_sets[REGNO (dst)]++;
		      reg_n_sets[REGNO (src)]--;

		      reg_n_calls_crossed[REGNO (dst)] += num_calls;
		      reg_n_calls_crossed[REGNO (src)] -= num_calls;

		      /* ??? Must adjust reg_live_length, and reg_n_refs for
			 both registers.  Must keep track of loop_depth in
			 order to get reg_n_refs adjustment correct.  */

		      if (regmove_dump_file)
			fprintf (regmove_dump_file,
				 "Fixed operand %d of insn %d matching operand %d.\n",
				 operand_number, INSN_UID (insn), match_number);

		      goto done_backwards;
		    }
		}
	    }
	done_backwards:
	  ;
	}
    }
#endif /* REGISTER_CONSTRAINTS */
}
