/* Modula-3: modified */

/* Transformations based on profile information for values.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "expr.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "value-prof.h"
#include "output.h"
#include "flags.h"
#include "insn-config.h"
#include "recog.h"
#include "optabs.h"
#include "regs.h"
#include "ggc.h"
#include "tree-flow.h"
#include "tree-flow-inline.h"
#include "diagnostic.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "coverage.h"
#include "tree.h"
#include "gcov-io.h"
#include "cgraph.h"
#include "timevar.h"
#include "tree-pass.h"
#include "pointer-set.h"

EXTERN_C_START

/* In this file value profile based optimizations are placed.  Currently the
   following optimizations are implemented (for more detailed descriptions
   see comments at value_profile_transformations):

   1) Division/modulo specialization.  Provided that we can determine that the
      operands of the division have some special properties, we may use it to
      produce more effective code.
   2) Speculative prefetching.  If we are able to determine that the difference
      between addresses accessed by a memory reference is usually constant, we
      may add the prefetch instructions.
      FIXME: This transformation was removed together with RTL based value
      profiling.

   3) Indirect/virtual call specialization. If we can determine most
      common function callee in indirect/virtual call. We can use this
      information to improve code effectiveness (especially info for
      inliner).

   Every such optimization should add its requirements for profiled values to
   insn_values_to_profile function.  This function is called from branch_prob
   in profile.c and the requested values are instrumented by it in the first
   compilation with -fprofile-arcs.  The optimization may then read the
   gathered data in the second compilation with -fbranch-probabilities.

   The measured data is pointed to from the histograms
   field of the statement annotation of the instrumented insns.  It is
   kept as a linked list of struct histogram_value_t's, which contain the
   same information as above.  */


static tree gimple_divmod_fixed_value (gimple, tree, int, gcov_type, gcov_type);
static tree gimple_mod_pow2 (gimple, int, gcov_type, gcov_type);
static tree gimple_mod_subtract (gimple, int, int, int, gcov_type, gcov_type,
				 gcov_type);
static bool gimple_divmod_fixed_value_transform (gimple_stmt_iterator *);
static bool gimple_mod_pow2_value_transform (gimple_stmt_iterator *);

/* Allocate histogram value.  */

static histogram_value
gimple_alloc_histogram_value (struct function *fun ATTRIBUTE_UNUSED,
			      enum hist_type type, gimple stmt, tree value)
{
   histogram_value hist = (histogram_value) xcalloc (1, sizeof (*hist));
   hist->hvalue.value = value;
   hist->hvalue.stmt = stmt;
   hist->type = type;
   return hist;
}

/* Hash value for histogram.  */

static hashval_t
histogram_hash (const void *x)
{
  return htab_hash_pointer (((const_histogram_value)x)->hvalue.stmt);
}

/* Return nonzero if decl_id of die_struct X is the same as UID of decl *Y.  */

static int
histogram_eq (const void *x, const void *y)
{
  return ((const_histogram_value) x)->hvalue.stmt == (const_gimple) y;
}

/* Set histogram for STMT.  */

static void
set_histogram_value (struct function *fun, gimple stmt, histogram_value hist)
{
  void **loc;
  if (!hist && !VALUE_HISTOGRAMS (fun))
    return;
  if (!VALUE_HISTOGRAMS (fun))
    VALUE_HISTOGRAMS (fun) = htab_create (1, histogram_hash,
				           histogram_eq, NULL);
  loc = htab_find_slot_with_hash (VALUE_HISTOGRAMS (fun), stmt,
                                  htab_hash_pointer (stmt),
				  hist ? INSERT : NO_INSERT);
  if (!hist)
    {
      if (loc)
	htab_clear_slot (VALUE_HISTOGRAMS (fun), loc);
      return;
    }
  *loc = hist;
}

/* Get histogram list for STMT.  */

histogram_value
gimple_histogram_value (struct function *fun, gimple stmt)
{
  if (!VALUE_HISTOGRAMS (fun))
    return NULL;
  return (histogram_value) htab_find_with_hash (VALUE_HISTOGRAMS (fun), stmt,
						htab_hash_pointer (stmt));
}

/* Add histogram for STMT.  */

void
gimple_add_histogram_value (struct function *fun, gimple stmt,
			    histogram_value hist)
{
  hist->hvalue.next = gimple_histogram_value (fun, stmt);
  set_histogram_value (fun, stmt, hist);
}


/* Remove histogram HIST from STMT's histogram list.  */

void
gimple_remove_histogram_value (struct function *fun, gimple stmt,
			       histogram_value hist)
{
  histogram_value hist2 = gimple_histogram_value (fun, stmt);
  if (hist == hist2)
    {
      set_histogram_value (fun, stmt, hist->hvalue.next);
    }
  else
    {
      while (hist2->hvalue.next != hist)
	hist2 = hist2->hvalue.next;
      hist2->hvalue.next = hist->hvalue.next;
    }
  free (hist->hvalue.counters);
#ifdef ENABLE_CHECKING
  memset (hist, 0xab, sizeof (*hist));
#endif
  free (hist);
}


/* Lookup histogram of type TYPE in the STMT.  */

histogram_value
gimple_histogram_value_of_type (struct function *fun, gimple stmt,
				enum hist_type type)
{
  histogram_value hist;
  for (hist = gimple_histogram_value (fun, stmt); hist;
       hist = hist->hvalue.next)
    if (hist->type == type)
      return hist;
  return NULL;
}

/* Dump information about HIST to DUMP_FILE.  */

static void
dump_histogram_value (FILE *dump_file, histogram_value hist)
{
  switch (hist->type)
    {
    case HIST_TYPE_INTERVAL:
      fprintf (dump_file, "Interval counter range %d -- %d",
	       hist->hdata.intvl.int_start,
	       (hist->hdata.intvl.int_start
	        + hist->hdata.intvl.steps - 1));
      if (hist->hvalue.counters)
	{
	   unsigned int i;
	   fprintf(dump_file, " [");
           for (i = 0; i < hist->hdata.intvl.steps; i++)
	     fprintf (dump_file, " %d:"HOST_WIDEST_INT_PRINT_DEC,
		      hist->hdata.intvl.int_start + i,
		      (HOST_WIDEST_INT) hist->hvalue.counters[i]);
	   fprintf (dump_file, " ] outside range:"HOST_WIDEST_INT_PRINT_DEC,
		    (HOST_WIDEST_INT) hist->hvalue.counters[i]);
	}
      fprintf (dump_file, ".\n");
      break;

    case HIST_TYPE_POW2:
      fprintf (dump_file, "Pow2 counter ");
      if (hist->hvalue.counters)
	{
	   fprintf (dump_file, "pow2:"HOST_WIDEST_INT_PRINT_DEC
		    " nonpow2:"HOST_WIDEST_INT_PRINT_DEC,
		    (HOST_WIDEST_INT) hist->hvalue.counters[0],
		    (HOST_WIDEST_INT) hist->hvalue.counters[1]);
	}
      fprintf (dump_file, ".\n");
      break;

    case HIST_TYPE_SINGLE_VALUE:
      fprintf (dump_file, "Single value ");
      if (hist->hvalue.counters)
	{
	   fprintf (dump_file, "value:"HOST_WIDEST_INT_PRINT_DEC
		    " match:"HOST_WIDEST_INT_PRINT_DEC
		    " wrong:"HOST_WIDEST_INT_PRINT_DEC,
		    (HOST_WIDEST_INT) hist->hvalue.counters[0],
		    (HOST_WIDEST_INT) hist->hvalue.counters[1],
		    (HOST_WIDEST_INT) hist->hvalue.counters[2]);
	}
      fprintf (dump_file, ".\n");
      break;

    case HIST_TYPE_AVERAGE:
      fprintf (dump_file, "Average value ");
      if (hist->hvalue.counters)
	{
	   fprintf (dump_file, "sum:"HOST_WIDEST_INT_PRINT_DEC
		    " times:"HOST_WIDEST_INT_PRINT_DEC,
		    (HOST_WIDEST_INT) hist->hvalue.counters[0],
		    (HOST_WIDEST_INT) hist->hvalue.counters[1]);
	}
      fprintf (dump_file, ".\n");
      break;

    case HIST_TYPE_IOR:
      fprintf (dump_file, "IOR value ");
      if (hist->hvalue.counters)
	{
	   fprintf (dump_file, "ior:"HOST_WIDEST_INT_PRINT_DEC,
		    (HOST_WIDEST_INT) hist->hvalue.counters[0]);
	}
      fprintf (dump_file, ".\n");
      break;

    case HIST_TYPE_CONST_DELTA:
      fprintf (dump_file, "Constant delta ");
      if (hist->hvalue.counters)
	{
	   fprintf (dump_file, "value:"HOST_WIDEST_INT_PRINT_DEC
		    " match:"HOST_WIDEST_INT_PRINT_DEC
		    " wrong:"HOST_WIDEST_INT_PRINT_DEC,
		    (HOST_WIDEST_INT) hist->hvalue.counters[0],
		    (HOST_WIDEST_INT) hist->hvalue.counters[1],
		    (HOST_WIDEST_INT) hist->hvalue.counters[2]);
	}
      fprintf (dump_file, ".\n");
      break;
    case HIST_TYPE_INDIR_CALL:
      fprintf (dump_file, "Indirect call ");
      if (hist->hvalue.counters)
	{
	   fprintf (dump_file, "value:"HOST_WIDEST_INT_PRINT_DEC
		    " match:"HOST_WIDEST_INT_PRINT_DEC
		    " all:"HOST_WIDEST_INT_PRINT_DEC,
		    (HOST_WIDEST_INT) hist->hvalue.counters[0],
		    (HOST_WIDEST_INT) hist->hvalue.counters[1],
		    (HOST_WIDEST_INT) hist->hvalue.counters[2]);
	}
      fprintf (dump_file, ".\n");
      break;
   }
}

/* Dump all histograms attached to STMT to DUMP_FILE.  */

void
dump_histograms_for_stmt (struct function *fun, FILE *dump_file, gimple stmt)
{
  histogram_value hist;
  for (hist = gimple_histogram_value (fun, stmt); hist; hist = hist->hvalue.next)
   dump_histogram_value (dump_file, hist);
}

/* Remove all histograms associated with STMT.  */

void
gimple_remove_stmt_histograms (struct function *fun, gimple stmt)
{
  histogram_value val;
  while ((val = gimple_histogram_value (fun, stmt)) != NULL)
    gimple_remove_histogram_value (fun, stmt, val);
}

/* Duplicate all histograms associates with OSTMT to STMT.  */

void
gimple_duplicate_stmt_histograms (struct function *fun, gimple stmt,
				  struct function *ofun, gimple ostmt)
{
  histogram_value val;
  for (val = gimple_histogram_value (ofun, ostmt); val != NULL; val = val->hvalue.next)
    {
      histogram_value new_val = gimple_alloc_histogram_value (fun, val->type, NULL, NULL);
      memcpy (new_val, val, sizeof (*val));
      new_val->hvalue.stmt = stmt;
      new_val->hvalue.counters = XNEWVAR (gcov_type, sizeof (*new_val->hvalue.counters) * new_val->n_counters);
      memcpy (new_val->hvalue.counters, val->hvalue.counters, sizeof (*new_val->hvalue.counters) * new_val->n_counters);
      gimple_add_histogram_value (fun, stmt, new_val);
    }
}


/* Move all histograms associated with OSTMT to STMT.  */

void
gimple_move_stmt_histograms (struct function *fun, gimple stmt, gimple ostmt)
{
  histogram_value val = gimple_histogram_value (fun, ostmt);
  if (val)
    {
      /* The following three statements can't be reordered,
         because histogram hashtab relies on stmt field value
	 for finding the exact slot. */
      set_histogram_value (fun, ostmt, NULL);
      for (; val != NULL; val = val->hvalue.next)
	val->hvalue.stmt = stmt;
      set_histogram_value (fun, stmt, val);
    }
}

static bool error_found = false;

/* Helper function for verify_histograms.  For each histogram reachable via htab
   walk verify that it was reached via statement walk.  */

static int
visit_hist (void **slot, void *data)
{
  struct pointer_set_t *visited = (struct pointer_set_t *) data;
  histogram_value hist = *(histogram_value *) slot;
  if (!pointer_set_contains (visited, hist))
    {
      error ("dead histogram");
      dump_histogram_value (stderr, hist);
      debug_gimple_stmt (hist->hvalue.stmt);
      error_found = true;
    }
  return 1;
}


/* Verify sanity of the histograms.  */

DEBUG_FUNCTION void
verify_histograms (void)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  histogram_value hist;
  struct pointer_set_t *visited_hists;

  error_found = false;
  visited_hists = pointer_set_create ();
  FOR_EACH_BB (bb)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple stmt = gsi_stmt (gsi);

	for (hist = gimple_histogram_value (cfun, stmt); hist;
	     hist = hist->hvalue.next)
	  {
	    if (hist->hvalue.stmt != stmt)
	      {
		error ("Histogram value statement does not correspond to "
		       "the statement it is associated with");
		debug_gimple_stmt (stmt);
		dump_histogram_value (stderr, hist);
		error_found = true;
	      }
            pointer_set_insert (visited_hists, hist);
	  }
      }
  if (VALUE_HISTOGRAMS (cfun))
    htab_traverse (VALUE_HISTOGRAMS (cfun), visit_hist, visited_hists);
  pointer_set_destroy (visited_hists);
  if (error_found)
    internal_error ("verify_histograms failed");
}

/* Helper function for verify_histograms.  For each histogram reachable via htab
   walk verify that it was reached via statement walk.  */

static int
free_hist (void **slot, void *data ATTRIBUTE_UNUSED)
{
  histogram_value hist = *(histogram_value *) slot;
  free (hist->hvalue.counters);
#ifdef ENABLE_CHECKING
  memset (hist, 0xab, sizeof (*hist));
#endif
  free (hist);
  return 1;
}

void
free_histograms (void)
{
  if (VALUE_HISTOGRAMS (cfun))
    {
      htab_traverse (VALUE_HISTOGRAMS (cfun), free_hist, NULL);
      htab_delete (VALUE_HISTOGRAMS (cfun));
      VALUE_HISTOGRAMS (cfun) = NULL;
    }
}

static struct cgraph_node** pid_map = NULL;

void
stringop_block_profile (gimple stmt, unsigned int *expected_align,
			HOST_WIDE_INT *expected_size)
{
  histogram_value histogram;
  histogram = gimple_histogram_value_of_type (cfun, stmt, HIST_TYPE_AVERAGE);
  if (!histogram)
    *expected_size = -1;
  else if (!histogram->hvalue.counters[1])
    {
      *expected_size = -1;
      gimple_remove_histogram_value (cfun, stmt, histogram);
    }
  else
    {
      gcov_type size;
      size = ((histogram->hvalue.counters[0]
	      + histogram->hvalue.counters[1] / 2)
	       / histogram->hvalue.counters[1]);
      /* Even if we can hold bigger value in SIZE, INT_MAX
	 is safe "infinity" for code generation strategies.  */
      if (size > INT_MAX)
	size = INT_MAX;
      *expected_size = size;
      gimple_remove_histogram_value (cfun, stmt, histogram);
    }
  histogram = gimple_histogram_value_of_type (cfun, stmt, HIST_TYPE_IOR);
  if (!histogram)
    *expected_align = 0;
  else if (!histogram->hvalue.counters[0])
    {
      gimple_remove_histogram_value (cfun, stmt, histogram);
      *expected_align = 0;
    }
  else
    {
      gcov_type count;
      int alignment;

      count = histogram->hvalue.counters[0];
      alignment = 1;
      while (!(count & alignment)
	     && (alignment * 2 * BITS_PER_UNIT))
	alignment <<= 1;
      *expected_align = alignment * BITS_PER_UNIT;
      gimple_remove_histogram_value (cfun, stmt, histogram);
    }
}

EXTERN_C_END
