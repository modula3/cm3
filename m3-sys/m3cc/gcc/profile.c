/* CYGNUS LOCAL: gcov */
/* Calculate branch probabilities, and basic block execution counts. 
   Copyright (C) 1990-1994 Free Software Foundation, Inc.
   Contributed by James E. Wilson, UC Berkeley/Cygnus Support;
   based on some ideas from Dain Samples of UC Berkeley.

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

/* ??? Should use constructor/destructor mechanism to call __bb_init_func
   and __bb_exit_func.  */

/* ??? Really should not put insns inside of LIBCALL sequences, when putting
   insns after a call, should look for the insn setting the retval, and
   insert the insns after that one.  */

/* ??? Register allocation should use basic block execution counts to
   give preference to the most commonly executed blocks.  */

/* ??? The .da files are not safe.  Changing the program after creating .da
   files or using different options when compiling with -fbranch-probabilities
   can result the arc data not matching the program.  Maybe add instrumented
   arc count to .bbg file?  Maybe check whether PFG matches the .bbg file?  */

/* ??? Should calculate branch probabilities before instrumenting code, since
   then we can use arc counts to help decide which arcs to instrument.  */

/* ??? Rearrange code so that the most frequently executed arcs become from
   one block to the next block (i.e. a fall through), move seldom executed
   code outside of loops even at the expense of adding a few branches to
   achieve this, see Dain Sample's UC Berkeley thesis.  */

#include "config.h"
#include "rtl.h"
#include "flags.h"
#include "insn-flags.h"
#include "insn-config.h"
#include "output.h"
#include <stdio.h>

extern char * xmalloc ();
extern void free ();

/* One of these is dynamically created whenever we identify an arc in the
   function.  */

struct adj_list
{
  int source;
  int target;
  int arc_count;
  unsigned int count_valid : 1;
  unsigned int on_tree : 1;
  unsigned int fake : 1;
  unsigned int fall_through : 1;
  rtx branch_insn;
  struct adj_list *pred_next;
  struct adj_list *succ_next;
};

#define ARC_TARGET(ARCPTR) (ARCPTR->target)
#define ARC_SOURCE(ARCPTR) (ARCPTR->source)
#define ARC_COUNT(ARCPTR)  (ARCPTR->arc_count)

/* Count the number of basic blocks, and create an array of these structures,
   one for each bb in the function.  */

struct bb_info
{
  struct adj_list *succ;
  struct adj_list *pred;
  int succ_count;
  int pred_count;
  int exec_count;
  unsigned int count_valid : 1;
  unsigned int on_tree : 1;
  rtx first_insn;
};

/* Indexed by label number, gives the basic block number containing that
   label.  */

static int *label_to_bb;

/* Number of valid entries in the label_to_bb array.  */

static int label_to_bb_size;

/* Indexed by block index, holds the basic block graph.  */

static struct bb_info *bb_graph;

/* Name and file pointer of the output file for the basic block graph.  */

static char *bbg_file_name;
static FILE *bbg_file;

/* Name and file pointer of the input file for the arc count data.  */

static char *d_file_name;
static FILE *d_file;

/* Output file where the basic block/line number map is stored.  */

static FILE *test_coverage_file;

/* Last source file name written to the test_coverage_file.  */

static char *test_coverage_filename;

/* Indicates whether the next line number note should be output to the
   test_coverage_file or not.  Used to eliminate a redundant note after
   an expanded inline function call.  */

static int ignore_next_note;

/* Used by final, for allocating the proper amount of storage for the
   instrumented arc execution counts.  */

int count_instrumented_arcs;

/* Number of executions for the return label.  */

int return_label_execution_count;

/* Collect statistics on the performance of this pass for the entire source
   file.  */

static int total_num_blocks;
static int total_num_arcs;
static int total_num_arcs_instrumented;
static int total_num_blocks_created;
static int total_num_passes;
static int total_num_times_called;
static int total_hist_br_prob[20];
static int total_num_never_executed;
static int total_num_branches;

/* Forward declarations.  */
static void init_arc ();
static void find_spanning_tree ();
static void expand_spanning_tree ();
static void fill_spanning_tree ();
static void init_arc_profiler ();
static void output_arc_profiler ();
static void output_func_start_profiler ();

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

/* Add arc instrumentation code to the entire insn chain.

   F is the first insn of the chain.
   NUM_BLOCKS is the number of basic blocks found in F.
   DUMP_FILE, if nonzero, is an rtl dump file we can write to.  */

static void
instrument_arcs (f, num_blocks, dump_file)
     rtx f;
     int num_blocks;
     FILE *dump_file;
{
  register int i;
  register struct adj_list *arcptr, *backptr;
  int num_arcs = 0;
  int num_instr_arcs = 0;
  rtx insn;

  int neg_one = -1;
  int zero = 0;
  int inverted;
  rtx note;

  /* Instrument the program start.  */
  /* Handle block 0 specially, since it will always be instrumented,
     but it doesn't have a valid first_insn or branch_insn.  Also, we
     must emit a call to __bb_init_func here.  We must put the instructions
     before the NOTE_INSN_FUNCTION_BEG note, so that they don't clobber
     any of the parameters of the current function.  */
  for (insn = f; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == NOTE
	&& NOTE_LINE_NUMBER (insn) == NOTE_INSN_FUNCTION_BEG)
      break;
  insn = PREV_INSN (insn);
  output_func_start_profiler (insn);
  output_arc_profiler (total_num_arcs_instrumented + num_instr_arcs++, insn);

  for (i = 1; i < num_blocks; i++)
    for (arcptr = bb_graph[i].succ; arcptr; arcptr = arcptr->succ_next)
      if (! arcptr->on_tree)
	{
	  if (dump_file)
	    fprintf (dump_file, "Arc %d to %d instrumented\n", i,
		     ARC_TARGET (arcptr));

	  /* Check to see if this arc is the only exit from its source block,
	     or the only entrance to its target block.  In either case,
	     we don't need to create a new block to instrument the arc.  */
	  
	  if (bb_graph[i].succ == arcptr && arcptr->succ_next == 0)
	    {
	      /* Instrument the source block.  */
	      output_arc_profiler (total_num_arcs_instrumented
				   + num_instr_arcs++,
				   PREV_INSN (bb_graph[i].first_insn));
	    }
	  else if (arcptr == bb_graph[ARC_TARGET (arcptr)].pred
		   && arcptr->pred_next == 0)
	    {
	      /* Instrument the target block.  */
	      output_arc_profiler (total_num_arcs_instrumented
				   + num_instr_arcs++, 
				   PREV_INSN (bb_graph[ARC_TARGET (arcptr)].first_insn));
	    }
	  else if (arcptr->fall_through)
	    {
	      /* This is a fall-through; put the instrumentation code after
		 the branch that ends this block.  */
	      
	      for (backptr = bb_graph[i].succ; backptr;
		   backptr = backptr->succ_next)
		if (backptr != arcptr)
		  break;
	      
	      output_arc_profiler (total_num_arcs_instrumented
				   + num_instr_arcs++,
				   backptr->branch_insn);
	    }
	  else
	    {
	      /* Must emit a new basic block to hold the arc counting code.  */
	      enum rtx_code code = GET_CODE (PATTERN (arcptr->branch_insn));

	      if (code == SET)
		{
		  /* Create the new basic block right after the branch.
		     Invert the branch so that it jumps past the end of the new
		     block.  The new block will consist of the instrumentation
		     code, and a jump to the target of this arc.  */
		  int this_is_simplejump = simplejump_p (arcptr->branch_insn);
		  rtx new_label = gen_label_rtx ();
		  rtx old_label, set_src;
		  rtx after = arcptr->branch_insn;
		  
		  /* Simplejumps can't reach here.  */
		  if (this_is_simplejump)
		    abort ();

		  /* We can't use JUMP_LABEL, because it won't be set if we
		     are compiling without optimization.  */

		  set_src = SET_SRC (single_set (arcptr->branch_insn));
		  if (GET_CODE (set_src) == LABEL_REF)
		    old_label = set_src;
		  else if (GET_CODE (set_src) != IF_THEN_ELSE)
		    abort ();
		  else if (XEXP (set_src, 1) == pc_rtx)
		    old_label = XEXP (XEXP (set_src, 2), 0);
		  else
		    old_label = XEXP (XEXP (set_src, 1), 0);

		  /* Set the JUMP_LABEL so that redirect_jump will work.  */
		  JUMP_LABEL (arcptr->branch_insn) = old_label;

		  /* Add a use for OLD_LABEL that will be needed when we emit
		     the JUMP_INSN below.  If we don't do this here,
		     `invert_jump' might delete it for us.  We must add two
		     when not optimizing, because the NUSES is zero now,
		     but must be at least two to prevent the label from being
		     deleted.  */
		  LABEL_NUSES (old_label) += 2;
		  
		  /* Emit the insns for the new block in reverse order,
		     since that is most convenient.  */

		  if (this_is_simplejump)
		    {
		      after = NEXT_INSN (arcptr->branch_insn);
		      if (! redirect_jump (arcptr->branch_insn, new_label))
			/* Don't know what to do if this branch won't
			   redirect.  */
			abort ();
		    }
		  else
		    {
		      if (! invert_jump (arcptr->branch_insn, new_label))
			/* Don't know what to do if this branch won't invert.  */
			abort ();

		      emit_label_after (new_label, after);
		      LABEL_NUSES (new_label)++;
		    }
		  emit_barrier_after (after);
		  emit_jump_insn_after (gen_jump (old_label), after);
		  JUMP_LABEL (NEXT_INSN (after)) = old_label;
		  
		  /* Instrument the source arc.  */
		  output_arc_profiler (total_num_arcs_instrumented
				       + num_instr_arcs++,
				       after);
		  if (this_is_simplejump)
		    {
		      emit_label_after (new_label, after);
		      LABEL_NUSES (new_label)++;
		    }
		}
	      else if (code == ADDR_VEC || code == ADDR_DIFF_VEC)
		{
		  /* A table jump.  Create a new basic block immediately
		     after the table, by emitting a barrier, a label, a
		     counting note, and a jump to the old label.  Put the
		     new label in the table.  */
		  
		  rtx new_label = gen_label_rtx ();
		  rtx old_lref, new_lref;
		  int index;
		  
		  /* Must determine the old_label reference, do this
		     by counting the arcs after this one, which will
		     give the index of our label in the table.  */
		  
		  index = 0;
		  for (backptr = arcptr->succ_next; backptr;
		       backptr = backptr->succ_next)
		    index++;
		  
		  old_lref = XVECEXP (PATTERN (arcptr->branch_insn),
				      (code == ADDR_DIFF_VEC), index);
		  
		  /* Emit the insns for the new block in reverse order,
		     since that is most convenient.  */
		  emit_jump_insn_after (gen_jump (XEXP (old_lref, 0)),
					arcptr->branch_insn);
		  JUMP_LABEL (NEXT_INSN (arcptr->branch_insn))
		    = XEXP (old_lref, 0);

		  /* Instrument the source arc.  */
		  output_arc_profiler (total_num_arcs_instrumented
				       + num_instr_arcs++,
				       arcptr->branch_insn);

		  emit_label_after (new_label, arcptr->branch_insn);
		  LABEL_NUSES (NEXT_INSN (arcptr->branch_insn))++;
		  emit_barrier_after (arcptr->branch_insn);
		  
		  /* Fix up the table jump.  */
		  new_lref = gen_rtx (LABEL_REF, Pmode, new_label);
		  XVECEXP (PATTERN (arcptr->branch_insn),
			   (code == ADDR_DIFF_VEC), index) = new_lref;
		}
	      else
		abort ();

	      num_arcs += 1;
	      if (dump_file)
		fprintf (dump_file,
			 "Arc %d to %d needed new basic block\n", i,
			 ARC_TARGET (arcptr));
	    }
	}
  
  total_num_arcs_instrumented += num_instr_arcs;
  count_instrumented_arcs = total_num_arcs_instrumented;

  total_num_blocks_created += num_arcs;
  if (dump_file)
    {
      fprintf (dump_file, "%d arcs instrumented\n", num_instr_arcs);
      fprintf (dump_file, "%d extra basic blocks created\n", num_arcs);
    }
}

/* Output STRING to the test_coverage_file, surrounded by DELIMITER.  */

static void
output_gcov_string (string, delimiter)
     char *string;
     long delimiter;
{
  long temp, temp2;
			
  /* Write a delimiter to indicate that a file name follows.  */
  fwrite (&delimiter, sizeof (delimiter), 1, test_coverage_file);

  /* Write the string.  */
  temp = strlen (string) + 1;
  fwrite (string, temp, 1, test_coverage_file);

  /* Append a few zeros, to align the output to a 4 byte boundary.  */
  temp = temp & 0x3;
  if (temp)
    {
      temp2 = 0;
      fwrite (&temp2, sizeof (char), 4 - temp, test_coverage_file);
    }

  /* Store another delimiter in the .bb file, just to make it easy to find the
     end of the file name.  */
  fwrite (&delimiter, sizeof (delimiter), 1, test_coverage_file);
}

/* Instrument and/or analyze program behavior based on program flow graph.
   In either case, this function builds a flow graph for the function being
   compiled.  The flow graph is stored in BB_GRAPH.

   When FLAG_PROFILE_ARCS is nonzero, this function instruments the arcs in
   the flow graph that are needed to reconstruct the dynamic behavior of the
   flow graph.

   When FLAG_BRANCH_PROBABILITIES is nonzero, this function reads auxilliary
   information from a data file containing arc count information from previous
   executions of the function being compiled.  In this case, the flow graph is
   annotated with actual execution counts, which are later propagated into the
   rtl for optimization purposes.

   Main entry point of this file.  */

void
branch_prob (f, dump_file)
     rtx f;
     FILE *dump_file;
{
  int i, num_blocks;
  int dest;
  rtx insn;
  struct adj_list *arcptr;
  int num_arcs, changes, passes;
  int total, prob;
  int hist_br_prob[20], num_never_executed, num_branches;
  long temp;
  /* Set to non-zero if we got bad count information.  */
  int bad_counts = 0;

  /* start of a function.  */
  if (flag_test_coverage)
    output_gcov_string (current_function_name, (long) -2);

  /* Execute this only if doing arc profiling or branch probabilities.  */
  if (! profile_arc_flag && ! flag_branch_probabilities
      && ! flag_test_coverage)
    abort ();

  total_num_times_called++;

  /* Create an array label_to_bb of ints of size max_label_num.  */
  label_to_bb_size = max_label_num ();
  label_to_bb = (int *) oballoc (label_to_bb_size * sizeof (int));
  bzero ((char *) label_to_bb, label_to_bb_size * sizeof (int));

  /* Scan the insns in the function, count the number of basic blocks
     present.  When a code label is passed, set label_to_bb[label] = bb
     number.  */

  /* The first block found will be block 1, so that function entry can be
     block 0.  */

  {
    register RTX_CODE prev_code = JUMP_INSN;
    register RTX_CODE code;
    register rtx insn;
    register int i;
    int block_separator_emitted = 0;

    ignore_next_note = 0;

    for (insn = NEXT_INSN (f), i = 0; insn; insn = NEXT_INSN (insn))
      {
	code = GET_CODE (insn);

	if (code == BARRIER)
	  ;
	else if (code == CODE_LABEL)
	  /* This label is part of the next block, but we can't increment
	     block number yet since there might be multiple labels.  */
	  label_to_bb[CODE_LABEL_NUMBER (insn)] = i + 1;
	/* We make NOTE_INSN_SETJMP notes into a block of their own, so that
	   they can be the target of the fake arc for the setjmp call.
	   This avoids creating cycles of fake arcs, which would happen if
	   the block after the setjmp call contained a call insn.  */
	else if ((prev_code == JUMP_INSN || prev_code == CALL_INSN
		  || prev_code == CODE_LABEL || prev_code == BARRIER)
		 && (GET_RTX_CLASS (code) == 'i'
		     || (code == NOTE &&
			 NOTE_LINE_NUMBER (insn) == NOTE_INSN_SETJMP)))
	  {
	    i += 1;

	    /* Emit the block separator if it hasn't already been emitted.  */
	    if (flag_test_coverage && ! block_separator_emitted)
	      {
		/* Output a zero to the .bb file to indicate that a new
		   block list is starting.  */
		temp = 0;
		fwrite (&temp, sizeof (temp), 1, test_coverage_file);
	      }
	    block_separator_emitted = 0;
	  }
	/* If flag_test_coverage is true, then we must add an entry to the
	   .bb file for every note.  */
	else if (code == NOTE && flag_test_coverage)
	  {
	    /* Must ignore the line number notes that immediately follow the
	       end of an inline function to avoid counting it twice.  There
	       is a note before the call, and one after the call.  */
	    if (NOTE_LINE_NUMBER (insn) == NOTE_REPEATED_LINE_NUMBER)
	      ignore_next_note = 1;
	    else if (NOTE_LINE_NUMBER (insn) > 0)
	      {
		if (ignore_next_note)
		  ignore_next_note = 0;
		else
		  {
		    /* Emit a block separator here to ensure that a NOTE
		       immediately following a JUMP_INSN or CALL_INSN will end
		       up in the right basic block list.  */
		    if ((prev_code == JUMP_INSN || prev_code == CALL_INSN
			 || prev_code == CODE_LABEL || prev_code == BARRIER)
			&& ! block_separator_emitted)
		      {
			/* Output a zero to the .bb file to indicate that
			   a new block list is starting.  */
			temp = 0;
			fwrite (&temp, sizeof (temp), 1, test_coverage_file);

			block_separator_emitted = 1;
		      }
		    
		    /* If this is a new source file, then output the file's
		       name to the .bb file.  */
		    if (! test_coverage_filename
			|| strcmp (NOTE_SOURCE_FILE (insn),
				   test_coverage_filename))
		      output_gcov_string (NOTE_SOURCE_FILE (insn), (long)-1);

		    /* Output the line number to the .bb file.  Must be done
		       after the output_bb_profile_data() call, and after the
		       file name is written, to ensure that it is correctly
		       handled by gcov.  */
		    temp = NOTE_LINE_NUMBER (insn);
		    fwrite (&temp, sizeof (temp), 1, test_coverage_file);
		  }
	      }
	  }

	if (code != NOTE)
	  prev_code = code;
	else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_SETJMP)
	  prev_code = CALL_INSN;
      }

    /* Allocate last `normal' entry for bb_graph.  */

    /* The last insn was a jump, call, or label.  In that case we have
       a block at the end of the function with no insns.  */
    if (prev_code == JUMP_INSN || prev_code == CALL_INSN
	|| prev_code == CODE_LABEL || prev_code == BARRIER)
      {
	i++;

	/* Emit the block separator if it hasn't already been emitted.  */
	if (flag_test_coverage && ! block_separator_emitted)
	  {
	    /* Output a zero to the .bb file to indicate that a new
	       block list is starting.  */
	    temp = 0;
	    fwrite (&temp, sizeof (temp), 1, test_coverage_file);
	  }
      }

    /* Create another block to stand for EXIT, and make all return insns, and
       the last basic block point here.  Add one more to account for block
       zero.  */
    num_blocks = i + 2;
  }

  total_num_blocks += num_blocks;
  if (dump_file)
    fprintf (dump_file, "%d basic blocks\n", num_blocks);

  /* If we are only doing test coverage here, then return now.  */
  if (! profile_arc_flag && ! flag_branch_probabilities)
    return;

  /* Create and initialize the arrays that will hold bb_graph
     and execution count info.  */

  bb_graph = (struct bb_info *) alloca (num_blocks * sizeof (struct bb_info));
  bzero ((char *) bb_graph, (sizeof (struct bb_info) * num_blocks));

  {
    /* Scan the insns again:
       - at the entry to each basic block, increment the predecessor count
       (and successor of previous block) if it is a fall through entry,
       create adj_list entries for this and the previous block
       - at each jump insn, increment predecessor/successor counts for
       target/source basic blocks, add this insn to pred/succ lists.

       This also cannot be broken out as a separate subroutine
       because it uses `alloca'.  */

    register RTX_CODE prev_code = JUMP_INSN;
    register RTX_CODE code;
    register rtx insn;
    register int i;
    int fall_through = 0;
    struct adj_list *arcptr;
    int dest;

    /* Block 0 always falls through to block 1.  */
    num_arcs = 0;
    arcptr = (struct adj_list *) alloca (sizeof (struct adj_list));
    init_arc (arcptr, 0, 1, 0);
    arcptr->fall_through = 1;
    num_arcs++;

    /* Add a fake fall through arc from the last block to block 0, to make the
       graph complete.  */
    arcptr = (struct adj_list *) alloca (sizeof (struct adj_list));
    init_arc (arcptr, num_blocks - 1, 0, 0);
    arcptr->fake = 1;
    num_arcs++;

    /* Exit must be one node of the graph, and all exits from the function
       must point there.  When see a return branch, must point the arc to the
       exit node.  */

    /* Must start scan with second insn in function as above.  */
    for (insn = NEXT_INSN (f), i = 0; insn; insn = NEXT_INSN (insn))
      {
	code = GET_CODE (insn);

	if (code == BARRIER)
	  fall_through = 0;
	else if (code == CODE_LABEL)
	  ;
	/* We make NOTE_INSN_SETJMP notes into a block of their own, so that
	   they can be the target of the fake arc for the setjmp call.
	   This avoids creating cycles of fake arcs, which would happen if
	   the block after the setjmp call ended with a call.  */
	else if ((prev_code == JUMP_INSN || prev_code == CALL_INSN
		  || prev_code == CODE_LABEL || prev_code == BARRIER)
		 && (GET_RTX_CLASS (code) == 'i'
		     || (code == NOTE &&
			 NOTE_LINE_NUMBER (insn) == NOTE_INSN_SETJMP)))
	  {
	    /* This is the first insn of the block.  */
	    i += 1;
	    if (fall_through)
	      {
		arcptr = (struct adj_list *) alloca (sizeof (struct adj_list));
		init_arc (arcptr, i - 1, i, 0);
		arcptr->fall_through = 1;

		num_arcs++;
	      }
	    fall_through = 1;
	    bb_graph[i].first_insn = insn;
	  }
	else if (code == NOTE)
	  ;

	if (code == CALL_INSN)
	  {
	    /* In the normal case, the call returns, and this is just like
	       a branch fall through.  */
	    fall_through = 1;

	    /* Setjmp may return more times than called, so to make the graph
	       solvable, add a fake arc from the function entrance to the
	       next block.

	       All other functions may return fewer times than called (if
	       a descendent call longjmp or exit), so to make the graph
	       solvable, add a fake arc to the function exit from the
	       current block.

	       Distinguish the cases by checking for a SETJUMP note.
	       A call_insn can be the last ins of a function, so must check
	       to see if next insn actually exists.  */
	    arcptr = (struct adj_list *) alloca (sizeof (struct adj_list));
	    if (NEXT_INSN (insn)
		&& GET_CODE (NEXT_INSN (insn)) == NOTE
		&& NOTE_LINE_NUMBER (NEXT_INSN (insn)) == NOTE_INSN_SETJMP)
	      init_arc (arcptr, 0, i+1, insn);
	    else
	      init_arc (arcptr, i, num_blocks-1, insn);
	    arcptr->fake = 1;
	    num_arcs++;
	  }
	else if (code == JUMP_INSN)
	  {
	    rtx tem, pattern = PATTERN (insn);
	    rtx tablejump = 0;

	    /* If running without optimization, then jump label won't be valid,
	       so we must search for the destination label in that case.
	       We have to handle tablejumps and returns specially anyways, so
	       we don't check the JUMP_LABEL at all here.  */

	    if (GET_CODE (pattern) == PARALLEL)
	      {
		/* This assumes that PARALLEL jumps are tablejump entry
		   jumps.  */
		/* Make an arc from this jump to the label of the
		   jump table.  This will instrument the number of
		   times the switch statement is executed.  */
		if (GET_CODE (XVECEXP (pattern, 0, 1)) != USE)
		  abort ();
		tem = XEXP (XVECEXP (pattern, 0, 1), 0);
		if (GET_CODE (tem) != LABEL_REF)
		  abort ();
		dest = label_to_bb[CODE_LABEL_NUMBER (XEXP (tem, 0))];
	      }
	    else if (GET_CODE (pattern) == ADDR_VEC
		     || GET_CODE (pattern) == ADDR_DIFF_VEC)
	      tablejump = pattern;
	    else if (GET_CODE (pattern) == RETURN)
	      dest = num_blocks - 1;
	    else if ((tem = SET_SRC (pattern))
		     && GET_CODE (tem) == LABEL_REF)
	      dest = label_to_bb[CODE_LABEL_NUMBER (XEXP (tem, 0))];
	    else
	      {
		rtx label_ref;

		/* Must be an IF_THEN_ELSE branch.  */
		if (GET_CODE (tem) != IF_THEN_ELSE)
		  abort ();
		if (XEXP (tem, 1) != pc_rtx)
		  label_ref = XEXP (tem, 1);
		else
		  label_ref = XEXP (tem, 2);
		dest = label_to_bb[CODE_LABEL_NUMBER (XEXP (label_ref, 0))];
	      }

	    if (tablejump)
	      {
		int diff_vec_p = GET_CODE (tablejump) == ADDR_DIFF_VEC;
		int len = XVECLEN (tablejump, diff_vec_p);
		int k;

		for (k = 0; k < len; k++)
		  {
		    rtx tem = XEXP (XVECEXP (tablejump, diff_vec_p, k), 0);
		    dest = label_to_bb[CODE_LABEL_NUMBER (tem)];

		    arcptr = (struct adj_list *) alloca (sizeof(struct adj_list));
		    init_arc (arcptr, i, dest, insn);

		    num_arcs++;
		  }
	      }
	    else
	      {
		arcptr = (struct adj_list *) alloca (sizeof (struct adj_list));
		init_arc (arcptr, i, dest, insn);

		num_arcs++;
	      }

	    /* Determine whether or not this jump will fall through.
	       Unconditional jumps and returns are not always followed by
	       barriers.  */
	    pattern = PATTERN (insn);
	    if (GET_CODE (pattern) == PARALLEL
		|| GET_CODE (pattern) == RETURN)
	      fall_through = 0;
	    else if (GET_CODE (pattern) == ADDR_VEC
		     || GET_CODE (pattern) == ADDR_DIFF_VEC)
	      /* These aren't actually jump insns, but they never fall
		 through, so...  */
	      fall_through = 0;
	    else
	      {
		if (GET_CODE (pattern) != SET || SET_DEST (pattern) != pc_rtx)
		  abort ();
		if (GET_CODE (SET_SRC (pattern)) != IF_THEN_ELSE)
		  fall_through = 0;
	      }
	  }

	if (code != NOTE)
	  prev_code = code;
	else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_SETJMP)
	  prev_code = CALL_INSN;
      }

    /* If the code at the end of the function would give a new block, then
       do the following.  */

    if (prev_code == JUMP_INSN || prev_code == CALL_INSN
	|| prev_code == CODE_LABEL || prev_code == BARRIER)
      {
	if (fall_through)
	  {
	    arcptr = (struct adj_list *) alloca (sizeof (struct adj_list));
	    init_arc (arcptr, i, i + 1, 0);
	    arcptr->fall_through = 1;

	    num_arcs++;
	  }
	  
	/* This may not be a real insn, but that should not cause a problem.  */
	bb_graph[i+1].first_insn = get_last_insn ();
      }

    /* There is always a fake arc from the last block of the function
       to the function exit block.  */
    arcptr = (struct adj_list *) alloca (sizeof (struct adj_list));
    init_arc (arcptr, num_blocks-2, num_blocks-1, 0);
    arcptr->fake = 1;
    num_arcs++;
  }

  total_num_arcs += num_arcs;
  if (dump_file)
    fprintf (dump_file, "%d arcs\n", num_arcs);

  /* Create spanning tree from basic block graph, mark each arc that is
     on the spanning tree.  */

  /* To reduce the instrumentation cost, make two passes over the tree.
     First, put as many must-split (crowded and fake) arcs on the tree as
     possible, then on the second pass fill in the rest of the tree.
     Note that the spanning tree is considered undirected, so that as many
     must-split arcs as possible can be put on it.

     Fallthough arcs which are crowded should not be chosen on the first
     pass, since they do not require creating a new basic block.  These
     arcs will have fall_through set.  */

  find_spanning_tree (num_blocks);

  /* Create a .bbg file from which gcov can reconstruct the basic block
     graph.  First output the number of basic blocks, and then for every
     arc output the source and target basic block numbers.
     NOTE: The format of this file must be compatible with gcov.  */

  if (flag_test_coverage)
    {
      int flag_bits;

      fwrite (&num_blocks, sizeof (num_blocks), 1, bbg_file);
      fwrite (&num_arcs, sizeof (num_arcs), 1, bbg_file);

      for (i = 0; i < num_blocks; i++)
	{
	  for (arcptr = bb_graph[i].succ; arcptr; arcptr = arcptr->succ_next)
	    {
	      int tem = i;

	      fwrite (&tem, sizeof (tem), 1, bbg_file);
	      fwrite (&ARC_TARGET (arcptr), sizeof (ARC_TARGET (arcptr)), 1,
		      bbg_file);
	      flag_bits = 0;
	      if (arcptr->on_tree)
		flag_bits |= 0x1;
	      if (arcptr->fake)
		flag_bits |= 0x2;
	      if (arcptr->fall_through)
		flag_bits |= 0x4;
	      fwrite (&flag_bits, sizeof (flag_bits), 1, bbg_file);
	    }
	}

      /* Emit a -1 to separate the list of all arcs from the list of
	 loop back edges that follows.  */
      i = -1;
      fwrite (&i, sizeof (i), 1, bbg_file);
    }

  /* For each arc not on the spanning tree, add counting code as rtl.  */

  if (profile_arc_flag)
    instrument_arcs (f, num_blocks, dump_file);

  /* Execute the rest only if doing branch probabilities.  */
  if (! flag_branch_probabilities)
    return;

  /* For each arc not on the spanning tree, set its execution count from
     the .d file.  */

  /* The first count in the .d file is the number of times that the function
     was entered.  This is the exec_count for block zero.  */

  num_arcs = 0;
  for (i = 0; i < num_blocks; i++)
    for (arcptr = bb_graph[i].succ; arcptr; arcptr = arcptr->succ_next)
      if (! arcptr->on_tree)
	{
	  num_arcs++;
	  if (d_file)
	    {
	      int i;
	      unsigned char c;
	      /* Since we can't be sure of the byte endianness of the host,
		 we always read a byte at a time.  */
	      for (i = sizeof (int) - 1; i >= 0; i--)
		{
		  fread (&c, sizeof (c), 1, d_file);
		  ARC_COUNT (arcptr) |= (c << (8 * i));
		}
	    }
	  else
	    ARC_COUNT (arcptr) = 0;
	  arcptr->count_valid = 1;
	  bb_graph[i].succ_count--;
	  bb_graph[ARC_TARGET (arcptr)].pred_count--;
	}

  if (dump_file)
    fprintf (dump_file, "%d arc counts read\n", num_arcs);

  /* For every block in the file,
     - if every exit/entrance arc has a known count, then set the block count
     - if the block count is known, and every exit/entrance arc but one has
       a known execution count, then set the count of the remaining arc

     As arc counts are set, decrement the succ/pred count, but don't delete
     the arc, that way we can easily tell when all arcs are known, or only
     one arc is unknown.  */

  /* The order that the basic blocks are iterated through is important.
     Since the code that finds spanning trees starts with block 0, low numbered
     arcs are put on the spanning tree in preference to high numbered arcs.
     Hence, most instrumented arcs are at the end.  Graph solving works much
     faster if we propagate numbers from the end to the start.
     
     This takes an average of slightly more than 3 passes.  */

  changes = 1;
  passes = 0;
  while (changes)
    {
      passes++;
      changes = 0;

      for (i = num_blocks - 1; i >= 0; i--)
	{
	  struct bb_info *binfo = &bb_graph[i];
	  if (! binfo->count_valid)
	    {
	      if (binfo->succ_count == 0)
		{
		  total = 0;
		  for (arcptr = binfo->succ; arcptr;
		       arcptr = arcptr->succ_next)
		    total += ARC_COUNT (arcptr);
		  binfo->exec_count = total;
		  binfo->count_valid = 1;
		  changes = 1;
		}
	      else if (binfo->pred_count == 0)
		{
		  total = 0;
		  for (arcptr = binfo->pred; arcptr;
		       arcptr = arcptr->pred_next)
		    total += ARC_COUNT (arcptr);
		  binfo->exec_count = total;
		  binfo->count_valid = 1;
		  changes = 1;
		}
	    }
	  if (binfo->count_valid)
	    {
	      if (binfo->succ_count == 1)
		{
		  total = 0;
		  /* One of the counts will be invalid, but it is zero,
		     so adding it in also doesn't hurt.  */
		  for (arcptr = binfo->succ; arcptr;
		       arcptr = arcptr->succ_next)
		    total += ARC_COUNT (arcptr);
		  /* Calculate count for remaining arc by conservation.  */
		  total = binfo->exec_count - total;
		  /* Search for the invalid arc, and set its count.  */
		  for (arcptr = binfo->succ; arcptr;
		       arcptr = arcptr->succ_next)
		    if (! arcptr->count_valid)
		      break;
		  if (! arcptr)
		    abort ();
		  arcptr->count_valid = 1;
		  ARC_COUNT (arcptr) = total;
		  binfo->succ_count--;
		  
		  bb_graph[ARC_TARGET (arcptr)].pred_count--;
		  changes = 1;
		}
	      if (binfo->pred_count == 1)
		{
		  total = 0;
		  /* One of the counts will be invalid, but it is zero,
		     so adding it in also doesn't hurt.  */
		  for (arcptr = binfo->pred; arcptr;
		       arcptr = arcptr->pred_next)
		    total += ARC_COUNT (arcptr);
		  /* Calculate count for remaining arc by conservation.  */
		  total = binfo->exec_count - total;
		  /* Search for the invalid arc, and set its count.  */
		  for (arcptr = binfo->pred; arcptr;
		       arcptr = arcptr->pred_next)
		    if (! arcptr->count_valid)
		      break;
		  if (! arcptr)
		    abort ();
		  arcptr->count_valid = 1;
		  ARC_COUNT (arcptr) = total;
		  binfo->pred_count--;
		  
		  bb_graph[ARC_SOURCE (arcptr)].succ_count--;
		  changes = 1;
		}
	    }
	}
    }

  total_num_passes += passes;
  if (dump_file)
    fprintf (dump_file, "Graph solving took %d passes.\n\n", passes);

  /* If the graph has been correctly solved, every block will have a
     succ and pred count of zero.  */
  for (i = 0; i < num_blocks; i++)
    {
      struct bb_info *binfo = &bb_graph[i];
      if (binfo->succ_count || binfo->pred_count)
	abort ();
    }

  /* For every arc, calculate its branch probability and add a reg_note
     to the branch insn to indicate this.  */

  for (i = 0; i < 20; i++)
    hist_br_prob[i] = 0;
  num_never_executed = 0;
  num_branches = 0;

  for (i = 0; i < num_blocks; i++)
    {
      struct bb_info *binfo = &bb_graph[i];

      total = binfo->exec_count;
      for (arcptr = binfo->succ; arcptr; arcptr = arcptr->succ_next)
	{
	  if (arcptr->branch_insn)
	    {
	      /* This calculates the branch probability as an integer between
		 0 and REG_BR_PROB_BASE, properly rounded to the nearest
		 integer.  Perform the arithmetic in double to avoid
		 overflowing the range of ints.  */

	      if (total == 0)
		prob = -1;
	      else
		{
		  rtx pat = PATTERN (arcptr->branch_insn);
		  
		  prob = (((double)ARC_COUNT (arcptr) * REG_BR_PROB_BASE)
			  + (total >> 1)) / total;
		  if (prob < 0 || prob > REG_BR_PROB_BASE)
		    {
		      if (dump_file)
			fprintf (dump_file, "bad count: prob for %d-%d thought to be %d (forcibly normalized)\n",
				 ARC_SOURCE (arcptr), ARC_TARGET (arcptr),
				 prob);

		      bad_counts = 1;
		      prob = REG_BR_PROB_BASE / 2;
		    }
		  
		  /* Match up probability with JUMP pattern.  */

		  if (GET_CODE (pat) == SET
		      && GET_CODE (SET_SRC (pat)) == IF_THEN_ELSE)
		    {
		      if (ARC_TARGET (arcptr) == ARC_SOURCE (arcptr) + 1)
			{
			  /* A fall through arc should never have a
			     branch insn.  */
			  abort ();
			}
		      else
			{
			  /* This is the arc for the taken branch.  */
			  if (GET_CODE (XEXP (SET_SRC (pat), 2)) != PC)
			    prob = REG_BR_PROB_BASE - prob;
			}
		    }
		}
	      
	      if (prob == -1)
		num_never_executed++;
	      else
		{
		  int index = prob * 20 / REG_BR_PROB_BASE;
		  if (index == 20)
		    index = 19;
		  hist_br_prob[index]++;
		}
	      num_branches++;
	      
	      REG_NOTES (arcptr->branch_insn)
		= gen_rtx (INT_LIST, REG_BR_PROB, prob,
			   REG_NOTES (arcptr->branch_insn));
	    }
	}

      /* Add a REG_EXEC_COUNT note to the first instruction of this block.  */
      if (! binfo->first_insn 
	  || GET_RTX_CLASS (GET_CODE (binfo->first_insn)) != 'i')
	{
	  /* Block 0 is a fake block representing function entry, and does
	     not have a real first insn.  The second last block might not
	     begin with a real insn.  */
	  if (i == num_blocks - 1)
	    return_label_execution_count = total;
	  else if (i != 0 && i != num_blocks - 2)
	    abort ();
	}
      else
	{
	  REG_NOTES (binfo->first_insn)
	    = gen_rtx (INT_LIST, REG_EXEC_COUNT, total,
		       REG_NOTES (binfo->first_insn));
	  if (i == num_blocks - 1)
	    return_label_execution_count = total;
	}
    }
  
  /* This should never happen.  */
  if (bad_counts)
    warning ("Arc profiling: some arc counts were bad.");

  if (dump_file)
    {
      fprintf (dump_file, "%d branches\n", num_branches);
      fprintf (dump_file, "%d branches never executed\n",
	       num_never_executed);
      if (num_branches)
	for (i = 0; i < 10; i++)
	  fprintf (dump_file, "%d%% branches in range %d-%d%%\n",
		   (hist_br_prob[i]+hist_br_prob[19-i])*100/num_branches,
		   5*i, 5*i+5);

      total_num_branches += num_branches;
      total_num_never_executed += num_never_executed;
      for (i = 0; i < 20; i++)
	total_hist_br_prob[i] += hist_br_prob[i];
    }

}

/* Initialize a new arc.
   ARCPTR is the empty adj_list this function fills in.
   SOURCE is the block number of the source block.
   TARGET is the block number of the target block.
   INSN is the insn which transfers control from SOURCE to TARGET,
   or zero if the transfer is implicit.  */

static void
init_arc (arcptr, source, target, insn)
     struct adj_list *arcptr;
     int source, target;
     rtx insn;
{
  ARC_TARGET (arcptr) = target;
  ARC_SOURCE (arcptr) = source;

  ARC_COUNT (arcptr) = 0;
  arcptr->count_valid = 0;
  arcptr->on_tree = 0;
  arcptr->fake = 0;
  arcptr->fall_through = 0;
  arcptr->branch_insn = insn;

  arcptr->succ_next = bb_graph[source].succ;
  bb_graph[source].succ = arcptr;
  bb_graph[source].succ_count++;

  arcptr->pred_next = bb_graph[target].pred;
  bb_graph[target].pred = arcptr;
  bb_graph[target].pred_count++;
}

/* This function searches all of the arcs in the program flow graph, and puts
   as many bad arcs as possible onto the spanning tree.  Bad arcs include
   fake arcs (needed for setjmp(), longjmp(), exit()) which MUST be on the
   spanning tree as they can't be instrumented.  Also, arcs which must be
   split when instrumented should be part of the spanning tree if possible.  */

static void
find_spanning_tree (num_blocks)
     int num_blocks;
{
  int i;
  struct adj_list *arcptr;
  struct bb_info *binfo = &bb_graph[0];

  /* Fake arcs must be part of the spanning tree, and are always safe to put
     on the spanning tree.  Fake arcs will either be a successor of node 0,
     a predecessor of the last node, or from the last node to node 0.  */

  for (arcptr = bb_graph[0].succ; arcptr; arcptr = arcptr->succ_next)
    if (arcptr->fake)
      {
	/* Adding this arc should never cause a cycle.  This is a fatal 
	   error if it would.  */
	if (bb_graph[ARC_TARGET (arcptr)].on_tree && binfo->on_tree)
	  abort();
	else
	  {
	    arcptr->on_tree = 1;
	    bb_graph[ARC_TARGET (arcptr)].on_tree = 1;
	    binfo->on_tree = 1;
	  }
      }

  binfo = &bb_graph[num_blocks-1];
  for (arcptr = binfo->pred; arcptr; arcptr = arcptr->pred_next)
    if (arcptr->fake)
      {
	/* Adding this arc should never cause a cycle.  This is a fatal 
	   error if it would.  */
	if (bb_graph[ARC_SOURCE (arcptr)].on_tree && binfo->on_tree)
	  abort();
	else
	  {
	    arcptr->on_tree = 1;
	    bb_graph[ARC_SOURCE (arcptr)].on_tree = 1;
	    binfo->on_tree = 1;
	  }
      }
  /* The only entrace to node zero is a fake arc.  */
  bb_graph[0].pred->on_tree = 1;
  
  /* Arcs which are crowded at both the source and target should be put on
     the spanning tree if possible, except for fall_throuch arcs which never
     require adding a new block even if crowded, add arcs with the same source
     and dest which must always be instrumented.  */
  for (i = 0; i < num_blocks; i++)
    {
      binfo = &bb_graph[i];

      for (arcptr = binfo->succ; arcptr; arcptr = arcptr->succ_next)
	if (! ((binfo->succ == arcptr && arcptr->succ_next == 0)
	       || (bb_graph[ARC_TARGET (arcptr)].pred
		   && arcptr->pred_next == 0))
	    && ! arcptr->fall_through
	    && ARC_TARGET (arcptr) != i)
	  {
	    /* This is a crowded arc at both source and target.  Try to put
	       in on the spanning tree.  Can do this if either the source or
	       target block is not yet on the tree.  */
	    if (! bb_graph[ARC_TARGET (arcptr)].on_tree	|| ! binfo->on_tree)
	      {
		arcptr->on_tree = 1;
		bb_graph[ARC_TARGET (arcptr)].on_tree = 1;
		binfo->on_tree = 1;
	      }
	  }
    }

  /* Clear all of the basic block on_tree bits, so that we can use them to
     create the spanning tree.  */
  for (i = 0; i < num_blocks; i++)
    bb_graph[i].on_tree = 0;

  /* Now fill in the spanning tree until every basic block is on it.
     Don't put the 0 to 1 fall through arc on the tree, since it is 
     always cheap to instrument, so start filling the tree from node 1.  */

  for (i = 1; i < num_blocks; i++)
    for (arcptr = bb_graph[i].succ; arcptr; arcptr = arcptr->succ_next)
      if (! arcptr->on_tree
	  && ! bb_graph[ARC_TARGET (arcptr)].on_tree)
	{
	  fill_spanning_tree (i);
	  break;
	}
}

/* Add arcs reached from BLOCK to the spanning tree if they are needed and
   not already there.  */

static void
fill_spanning_tree (block)
     int block;
{
  struct adj_list *arcptr;
  
  expand_spanning_tree (block);

  for (arcptr = bb_graph[block].succ; arcptr; arcptr = arcptr->succ_next)
    if (! arcptr->on_tree
	&& ! bb_graph[ARC_TARGET (arcptr)].on_tree)
      {
	arcptr->on_tree = 1;
	fill_spanning_tree (ARC_TARGET (arcptr));
      }
}

/* When first visit a block, must add all blocks that are already connected
   to this block via tree arcs to the spanning tree.  */

static void
expand_spanning_tree (block)
     int block;
{
  struct adj_list *arcptr;

  bb_graph[block].on_tree = 1;

  for (arcptr = bb_graph[block].succ; arcptr; arcptr = arcptr->succ_next)
    if (arcptr->on_tree && ! bb_graph[ARC_TARGET (arcptr)].on_tree)
      expand_spanning_tree (ARC_TARGET (arcptr));
    
  for (arcptr = bb_graph[block].pred;
       arcptr; arcptr = arcptr->pred_next)
    if (arcptr->on_tree && ! bb_graph[ARC_SOURCE (arcptr)].on_tree)
      expand_spanning_tree (ARC_SOURCE (arcptr));
}

/* Perform file-level initialization for branch-prob processing.  */

void
init_branch_prob (filename)
     char *filename;
{
  int len, i;

  if (flag_test_coverage)
    {
      /* Open an output file for the basic block/line number map.  */
      int len = strlen (filename);
      char *data_file = (char *) alloca (len + 4);
      strcpy (data_file, filename);
      strip_off_ending (data_file, len);
      strcat (data_file, ".bb");
      if ((test_coverage_file = fopen (data_file, "w")) == 0)
	pfatal_with_name (data_file);

      /* Open an output file for the program flow graph.  */
      len = strlen (filename);
      bbg_file_name = (char *) alloca (len + 5);
      strcpy (bbg_file_name, filename);
      strip_off_ending (bbg_file_name, len);
      strcat (bbg_file_name, ".bbg");
      if ((bbg_file = fopen (bbg_file_name, "w")) == 0)
	pfatal_with_name (bbg_file_name);

      /* Initialize to zero, to ensure that the first file name will be
	 written to the .bb file.  */
      test_coverage_filename = 0;
    }

  if (flag_branch_probabilities)
    {
      len = strlen (filename);
      d_file_name = (char *) alloca (len + 3);
      strcpy (d_file_name, filename);
      strip_off_ending (d_file_name, len);
      strcat (d_file_name, ".da");
      if ((d_file = fopen (d_file_name, "r")) == 0)
	warning ("file %s not found, execution counts assumed to be zero.",
		 d_file_name);

      /* The first word in the .d file gives the number of instrumented arcs,
	 which is not needed for our purposes.  */

      if (d_file)
	fread (&len, sizeof (len), 1, d_file);
    }

  if (profile_arc_flag)
    init_arc_profiler ();

  total_num_blocks = 0;
  total_num_arcs = 0;
  total_num_arcs_instrumented = 0;
  total_num_blocks_created = 0;
  total_num_passes = 0;
  total_num_times_called = 0;
  total_num_branches = 0;
  total_num_never_executed = 0;
  for (i = 0; i < 20; i++)
    total_hist_br_prob[i] = 0;
}

/* Performs file-level cleanup after branch-prob processing
   is completed.  */

void
end_branch_prob (dump_file)
     FILE *dump_file;
{
  int temp, i;

  if (flag_test_coverage)
    {
      fclose (test_coverage_file);
      fclose (bbg_file);
    }

  if (flag_branch_probabilities)
    {
      if (d_file)
	{
	  if (feof (d_file))
	    warning (".da file contents exhausted too early\n");
	  /* Should be at end of file now.  */
	  if (fread (&temp, sizeof (temp), 1, d_file) != 0)
	    warning (".da file contents not exhausted\n");
	  fclose (d_file);
	}
    }

  if (dump_file)
    {
      fprintf (dump_file, "\n");
      fprintf (dump_file, "Total number of blocks: %d\n", total_num_blocks);
      fprintf (dump_file, "Total number of arcs: %d\n", total_num_arcs);
      fprintf (dump_file, "Total number of instrumented arcs: %d\n",
	       total_num_arcs_instrumented);
      fprintf (dump_file, "Total number of blocks created: %d\n",
	       total_num_blocks_created);
      fprintf (dump_file, "Total number of graph solution passes: %d\n",
	       total_num_passes);
      if (total_num_times_called != 0)
	fprintf (dump_file, "Average number of graph solution passes: %d\n",
		 (total_num_passes + (total_num_times_called  >> 1))
		 / total_num_times_called);
      fprintf (dump_file, "Total number of branches: %d\n", total_num_branches);
      fprintf (dump_file, "Total number of branches never executed: %d\n",
	       total_num_never_executed);
      if (total_num_branches)
	for (i = 0; i < 10; i++)
	  fprintf (dump_file, "%d%% branches in range %d-%d%%\n",
		   (total_hist_br_prob[i] + total_hist_br_prob[19-i]) * 100
		   / total_num_branches, 5*i, 5*i+5);
    }
}

/* The label used by the arc profiling code.  */

static rtx profiler_label;

/* Initialize the profiler_label.  */

static void
init_arc_profiler ()
{
  /* Generate and save a copy of this so it can be shared.  */
  char *name = xmalloc (20);
  ASM_GENERATE_INTERNAL_LABEL (name, "LPBX", 2);
  profiler_label = gen_rtx (SYMBOL_REF, Pmode, name);
}

/* Output instructions as RTL to increment the arc execution count.  */

static void
output_arc_profiler (arcno, insert_after)
     int arcno;
     rtx insert_after;
{
  rtx profiler_target_addr
    = (arcno
       ? gen_rtx (CONST, Pmode,
		  gen_rtx (PLUS, Pmode, profiler_label,
			   gen_rtx (CONST_INT, VOIDmode, 4 * arcno)))
       : profiler_label);
  enum machine_mode mode = mode_for_size (LONG_TYPE_SIZE, MODE_INT, 0);
  rtx profiler_reg = gen_reg_rtx (mode);
  rtx address_reg = gen_reg_rtx (Pmode);
  rtx mem_ref, add_ref;
  rtx sequence;

#ifdef SMALL_REGISTER_CLASSES
  /* In this case, reload can use explicitly mentioned hard registers for
     reloads.  It is not safe to output profiling code between a call
     and the instruction that copies the result to a pseudo-reg.  This
     is because reload may allocate one of the profiling code pseudo-regs
     to the return value reg, thus clobbering the return value.  So we
     must check for calls here, and emit the profiling code after the
     instruction that uses the return value, if any.

     ??? The code here performs the same tests that reload does so hopefully
     all the bases are covered.  */

  if (GET_CODE (insert_after) == CALL_INSN
      && (GET_CODE (PATTERN (insert_after)) == SET
	  || (GET_CODE (PATTERN (insert_after)) == PARALLEL
	      && GET_CODE (XVECEXP (PATTERN (insert_after), 0, 0)) == SET)))
    {
      rtx return_reg;
      rtx next_insert_after = next_nonnote_insn (insert_after);

      if (GET_CODE (next_insert_after) == INSN)
	{
	  /* The first insn after the call may be a stack pop, skip it.  */
	  if (GET_CODE (PATTERN (next_insert_after)) == SET
	      && SET_DEST (PATTERN (next_insert_after)) == stack_pointer_rtx)
	    next_insert_after = next_nonnote_insn (next_insert_after);

	  if (GET_CODE (PATTERN (insert_after)) == SET)
	    return_reg = SET_DEST (PATTERN (insert_after));
	  else
	    return_reg = SET_DEST (XVECEXP (PATTERN (insert_after), 0, 0));

	  if (reg_referenced_p (return_reg, PATTERN (next_insert_after)))
	    insert_after = next_insert_after;
	}
    }
#endif

  start_sequence ();

  emit_move_insn (address_reg, profiler_target_addr);
  mem_ref = gen_rtx (MEM, mode, address_reg);
  emit_move_insn (profiler_reg, mem_ref);

  add_ref = gen_rtx (PLUS, mode, profiler_reg, GEN_INT (1));
  emit_move_insn (profiler_reg, add_ref);

  /* This is the same rtx as above, but it is not legal to share this rtx.  */
  mem_ref = gen_rtx (MEM, mode, address_reg);
  emit_move_insn (mem_ref, profiler_reg);

  sequence = gen_sequence ();
  end_sequence ();
  emit_insn_after (sequence, insert_after);
}

/* Output code to initialize this source file's arc profiling info, if that
   has not already been done.  */

static void
output_func_start_profiler (insert_after)
     rtx insert_after;
{
  char *name;
  rtx table_address;
  rtx label = gen_label_rtx ();
  rtx sequence;
  enum machine_mode mode = mode_for_size (LONG_TYPE_SIZE, MODE_INT, 0);

  start_sequence ();

  name = xmalloc (20);
  ASM_GENERATE_INTERNAL_LABEL (name, "LPBX", 0);
  table_address = force_reg (Pmode, gen_rtx (SYMBOL_REF, Pmode, name));

  emit_cmp_insn (gen_rtx (MEM, mode, table_address),
		 const0_rtx, NE, 0, mode, 0, 0);
  emit_jump_insn (gen_bne (label));
  
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "__bb_init_func"), 0,
		     mode, 1, table_address, Pmode);
  
  emit_label (label);
  sequence = gen_sequence ();
  end_sequence ();
  emit_insn_after (sequence, insert_after);
}
