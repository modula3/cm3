/* CYGNUS LOCAL mentoropt/law (entire file ) */
/* Reduce lifetimes of certain pseudos to avoid spills in reload.
   Copyright (C) 1995 Free Software Foundation, Inc.

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

/* This module minimizes lifetimes of pseudo registers which must be
   allocated to a register class which has only one entry by inserting
   copies to/from new pseudo registers.

   By minimizing such lifetimes it becomes much less likely that reload
   will need a spill register from that register class which leads to
   better overall code.  */

#include "config.h"
#include "rtl.h"
#include "insn-config.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "basic-block.h"
#include "obstack.h"

extern char *insn_operand_constraint[][MAX_RECOG_OPERANDS];

/* This obstack keeps track of the block number and loop depth
   for insns we create.  We use this information when updating
   dataflow information.  */

static struct obstack shorten_lifetimes_obstack;
static int *firstobj;
#define shorten_base \
  ((int *)obstack_base (&shorten_lifetimes_obstack))
#define shorten_next \
  ((int *)obstack_next_free (&shorten_lifetimes_obstack))

static void update_dataflow_information PROTO ((void));

void
shorten_lifetimes (f)
     rtx f;
{
  rtx insn, pattern, last;
  int found, i, depth, b, update_dataflow = 0;

  /* First see if there are any register classes with only one
     member.  If not, then quit now.  */
  found = 0;
  for (i = 0; i < N_REG_CLASSES; i++)
    if (reg_class_size[i] == 1)
      {
	found = 1;
	break;
      }

  if (!found)
    return;

  gcc_obstack_init (&shorten_lifetimes_obstack);
  firstobj = (int *) obstack_alloc (&shorten_lifetimes_obstack, 0);

  /* We need to record both the loop depth and the basic block any
     copy is made in.  */
  depth = 1;
  last = NULL;
  for (b = 0; b < n_basic_blocks; b++)
    {
      /* Handle notes where are not contained within any basic block.  */
      insn = basic_block_head[b];
      while (PREV_INSN (insn) != last)
	insn = PREV_INSN (insn);

      /* Now go forward to the end of this block.  */
      for (; insn != NEXT_INSN (basic_block_end[b]); insn = NEXT_INSN (insn))
	{
	  /* Keep track of the current loop depth.  */
	  if (GET_CODE (insn) == NOTE)
	    {
	      if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
		depth++;
	      else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END)
		depth--;
	      continue;
	    }

	  /* Only INSNs are of interest.  */
	  if (GET_CODE (insn) != INSN)
	    continue;

	  /* It's an INSN of some kind.  See if it is a simple set of
	     a register.  */
	  pattern = PATTERN (insn);
	  if (!single_set (insn)
	      || GET_CODE (pattern) != SET
	      || GET_CODE (SET_DEST (pattern)) != REG)
	    continue;

	  /* We know it's an INSN that sets a single register.  Now see
	     if that register must be allocated to a register class that
	     has only one member.  */
	  {
	    int insn_code_number;
	    char *p, c;
	    enum reg_class class;
	    rtx dest;

	    insn_code_number = recog_memoized (insn);

	    if (insn_code_number < 0)
	      continue;

	    dest = SET_DEST (pattern);
	    found = 1;
	    for (p = insn_operand_constraint[insn_code_number][0];
		 p && *p && found; )
	      {

		while ((c = *p++) != '\0' && c != ',')
		  switch (c)
		    {
		    case '=':  case '+':  case '?':
		    case '#':  case '&':  case '!':
		    case '*':  case '%':
		      continue;

		    case '0':  case '1':  case '2':  case '3':  case '4':
		    case '5':  case '6':  case '7':  case '8':  case '9':
		    case 'm':  case '<':  case '>':  case 'V':  case 'o':
		    case 'E':  case 'F':  case 'G':  case 'H':
		    case 's':  case 'i':  case 'n':
		    case 'I':  case 'J':  case 'K':  case 'L':
		    case 'M':  case 'N':  case 'O':  case 'P':
#ifdef EXTRA_CONSTRAINT
		    case 'Q':  case 'R':  case 'S':  case 'T':  case 'U':
#endif
		    case 'p':
		    case 'X':
		    case 'g': case 'r':
		      /* I don't think any of these restrict the register
			 class of our output register.  */
		      found = 0;
		      break;

		    default:
		      class = REG_CLASS_FROM_LETTER (c);
		      if (reg_class_size[(int) class] != 1)
			found = 0;
		      break;
		    }

		if (!*p)
		  break;
	      }

	    /* Did the output require a register class with only
	       one member?  */
	    if (found)
		{
		  rtx newreg;
		  rtx newinsn;
		  int regno;

		  /* At this time INSN and PATTERN refer to an instruction
		     which requires its output pseudo to be allocated to
		     a register class with a single register.

		     We will create a new pseudo and change this instruction
		     to use it as its output register.  A copy from the new
		     pseudo to the original output pseudo will be emitted
		     immediately after this instruction.

		     This creates nonconflicting lifetimes for all pseudos
		     which must be allocated into this register class making
		     it much less likely we will need a spill register from
		     this class.  If possible, global register allocation
		     will try to allocate both pseudos to the same register,
		     when that happens the copy is eliminated (but we still
		     have nonconflicting lifetimes and don't create spills). 

		     We might want to consider register scavenging to avoid
		     having to update the basic_block_live_at_start arrays
		     if we don't create too many new registers.  */
		  newreg = gen_reg_rtx (Pmode);
		  update_dataflow = 1;

		  /* Emit a copy from our new pseudo to the destination.  */
		  newinsn = emit_insn_after (gen_rtx (SET, Pmode,
						      SET_DEST (pattern),
						      newreg),
					     insn);
		  /* Our new pseudo dies in the copy insn, make note of
		     that for later passes.  */
		  REG_NOTES (newinsn)
		    = gen_rtx (EXPR_LIST, REG_DEAD, newreg, NULL);
		  
		  /* Make our new pseudo the output of this insn.  */
		  SET_DEST (pattern) = newreg;

		  /* Record our block number so that we can use it while
		     updating lifetime information later.  */
		  obstack_int_grow (&shorten_lifetimes_obstack, depth);
		  obstack_int_grow (&shorten_lifetimes_obstack, b);
		}
	  }
	}
      last = basic_block_end[b];
    }

  /* This pass runs after flow and is therefore responsible for
     updating dataflow information if it creates any new pseudos.  */
  if (update_dataflow)
    update_dataflow_information ();

  obstack_free (&shorten_lifetimes_obstack, firstobj);
}

/* This pass creates new pseudos after flow has run; therefore it must
   take care to insure dataflow information for the new pseudos is
   provided for later passes.

   This routine creates new arrays for the dataflow information, copies
   the old data into the new arrays, then initializes entries in the
   new arrays for the new pseudo registers.  */
   
static void
update_dataflow_information ()
{
  int i;
  regset tem;
  int nregset_bytes, nregset_size;
  int *nreg_n_refs, *nreg_live_length, *nreg_n_calls_crossed;
  int *nreg_basic_block;
  short *nreg_n_sets, *nreg_n_deaths;
  char *nreg_changes_size;

  /* First allocate new arrays big enough to hold our new registers
     and copy data from the old arrays into the new arrays.  */
  nregset_size = ((max_reg_num () + REGSET_ELT_BITS - 1) / REGSET_ELT_BITS);
  nregset_bytes = nregset_size * sizeof (*(regset) 0);

  nreg_n_refs = (int *) oballoc (max_reg_num () * sizeof (int));
  bzero ((char *) nreg_n_refs, max_reg_num () * sizeof (int));
  bcopy ((char *) reg_n_refs, (char *) nreg_n_refs, max_regno * sizeof (int));
  reg_n_refs = nreg_n_refs;

  nreg_n_sets = (short *) oballoc (max_reg_num () * sizeof (short));
  bzero ((char *) nreg_n_sets, max_reg_num () * sizeof (short));
  bcopy ((char *) reg_n_sets, (char *) nreg_n_sets, max_regno * sizeof (short));
  reg_n_sets = nreg_n_sets;

  nreg_n_deaths = (short *) oballoc (max_reg_num () * sizeof (short));
  bzero ((char *) nreg_n_deaths, max_reg_num () * sizeof (short));
  bcopy ((char *) reg_n_deaths,
	 (char *) nreg_n_deaths,
	 max_regno * sizeof (short));
  reg_n_deaths = nreg_n_deaths;

  nreg_changes_size = (char *) oballoc (max_reg_num () * sizeof (char));
  bzero ((char *) nreg_changes_size, max_reg_num () * sizeof (char));
  bcopy (reg_changes_size, nreg_changes_size, max_regno * sizeof (char));;
  reg_changes_size = nreg_changes_size;

  nreg_live_length = (int *) oballoc (max_reg_num () * sizeof (int));
  bzero ((char *) nreg_live_length, max_reg_num () * sizeof (int));
  bcopy ((char *) reg_live_length,
	 (char *) nreg_live_length,
	 max_regno * sizeof (int));
  reg_live_length = nreg_live_length;

  nreg_n_calls_crossed = (int *) oballoc (max_reg_num () * sizeof (int));
  bzero ((char *) nreg_n_calls_crossed, max_reg_num () * sizeof (int));
  bcopy ((char *) reg_n_calls_crossed,
	 (char *) nreg_n_calls_crossed,
	 max_regno * sizeof (int));
  reg_n_calls_crossed = nreg_n_calls_crossed;

  nreg_basic_block = (int *) oballoc (max_reg_num () * sizeof (int));
  for (i = 0; i < max_reg_num (); i++)
    nreg_basic_block[i] = REG_BLOCK_UNKNOWN;
  bcopy ((char *) reg_basic_block,
	 (char *) nreg_basic_block,
	 max_regno * sizeof (int));
  reg_basic_block = nreg_basic_block;


  /* The size of a regset has probably changed.  We need to copy
     information from the old memory region pointed to by 
     basic_block_live_at_start[] into the new region, then point
     basic_block_live_at_start[] to the new region.  */
  tem = (regset) oballoc (n_basic_blocks * nregset_bytes);
  bzero ((char *) tem, n_basic_blocks * nregset_bytes);
  for (i = 0; i < n_basic_blocks; i++)
    {
      /* Copy old info into the new regset vectors.  */
      bcopy ((char *) basic_block_live_at_start[i],
	     (char *) tem,
	     regset_bytes);

      /* Point basic_block_live_at_start to the new regset vectors.  */
      basic_block_live_at_start[i] = tem;

      /* Update our temporary pointer.  */
      tem += nregset_bytes / sizeof (*tem);
    }
  /* Update regset_size and regset_bytes for the following passes.  */
  regset_size = nregset_size;
  regset_bytes = nregset_bytes;

  /* Setjmp stuff is never used after we run, so don't bother
     updating it.  */

  /* Now fill in information for our new registers; this is inexact
     information, but is accurate enough for our needs.  */
  for (i = max_regno; i < max_reg_num (); i++)
    {
      /* We know there's two references for each new pseudo and
	 both references happen at the same loop depth.  So
	 ref_n_refs is depth * 2.  */
      reg_n_refs[i] = shorten_base[2 * (i - max_regno)] * 50;

      /* We know the new pseudos are set once, die once, never change
	 size, live for only 2 insns (or is it 1 insn?), and are
	 never live across calls.  */
      reg_n_sets[i] = 1;
      reg_n_deaths[i] = 1;
      reg_changes_size[i] = 0;
      reg_live_length[i] = 2;
      reg_n_calls_crossed[i] = 0;

      /* Record the basic block in which this register's entire lifetime
	 is contained.  */
      reg_basic_block[i] = shorten_base[2 * (i - max_regno) + 1];
    }

  /* And let the world know that the information on the new registers
     is valid.  */
  max_regno = max_reg_num ();
}
/* END CYGNUS LOCAL */
