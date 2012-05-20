/* Modula-3: modified */

/* Callgraph based interprocedural optimizations.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
   2011 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

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

/* This module implements main driver of compilation process as well as
   few basic interprocedural optimizers.

   The main scope of this file is to act as an interface in between
   tree based frontends and the backend (and middle end)

   The front-end is supposed to use following functionality:

    - cgraph_finalize_function

      This function is called once front-end has parsed whole body of function
      and it is certain that the function body nor the declaration will change.

      (There is one exception needed for implementing GCC extern inline
	function.)

    - varpool_finalize_variable

      This function has same behavior as the above but is used for static
      variables.

    - cgraph_finalize_compilation_unit

      This function is called once (source level) compilation unit is finalized
      and it will no longer change.

      In the the call-graph construction and local function
      analysis takes place here.  Bodies of unreachable functions are released
      to conserve memory usage.

      The function can be called multiple times when multiple source level
      compilation units are combined (such as in C frontend)

    - cgraph_optimize

      In this unit-at-a-time compilation the intra procedural analysis takes
      place here.  In particular the static functions whose address is never
      taken are marked as local.  Backend can then use this information to
      modify calling conventions, do better inlining or similar optimizations.

    - cgraph_mark_needed_node
    - varpool_mark_needed_node

      When function or variable is referenced by some hidden way the call-graph
      data structure must be updated accordingly by this function.
      There should be little need to call this function and all the references
      should be made explicit to cgraph code.  At present these functions are
      used by C++ frontend to explicitly mark the keyed methods.

    - analyze_expr callback

      This function is responsible for lowering tree nodes not understood by
      generic code into understandable ones or alternatively marking
      callgraph and varpool nodes referenced by the as needed.

      ??? On the tree-ssa genericizing should take place here and we will avoid
      need for these hooks (replacing them by genericizing hook)

        Analyzing of all functions is deferred
	to cgraph_finalize_compilation_unit and expansion into cgraph_optimize.

	In cgraph_finalize_compilation_unit the reachable functions are
	analyzed.  During analysis the call-graph edges from reachable
	functions are constructed and their destinations are marked as
	reachable.  References to functions and variables are discovered too
	and variables found to be needed output to the assembly file.  Via
	mark_referenced call in assemble_variable functions referenced by
	static variables are noticed too.

	The intra-procedural information is produced and its existence
	indicated by global_info_ready.  Once this flag is set it is impossible
	to change function from !reachable to reachable and thus
	assemble_variable no longer call mark_referenced.

	Finally the call-graph is topologically sorted and all reachable functions
	that has not been completely inlined or are not external are output.

	??? It is possible that reference to function or variable is optimized
	out.  We can not deal with this nicely because topological order is not
	suitable for it.  For tree-ssa we may consider another pass doing
	optimization and re-discovering reachable functions.

	??? Reorganize code so variables are output very last and only if they
	really has been referenced by produced code, so we catch more cases
	where reference has been optimized out.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tree-flow.h"
#include "tree-inline.h"
#include "langhooks.h"
#include "pointer-set.h"
#include "toplev.h"
#include "flags.h"
#include "ggc.h"
#include "debug.h"
#include "target.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "timevar.h"
#include "params.h"
#include "fibheap.h"
#include "intl.h"
#include "function.h"
#include "gimple.h"
#include "tree-iterator.h"
#include "tree-pass.h"
#include "tree-dump.h"
#include "output.h"
#include "plugin.h"

EXTERN_C_START

static void cgraph_expand_all_functions (void);
static void cgraph_mark_functions_to_output (void);
static void cgraph_expand_function (struct cgraph_node *);
static void cgraph_output_pending_asms (void);
static void cgraph_analyze_function (struct cgraph_node *);

FILE *cgraph_dump_file;

/* Used for vtable lookup in thunk adjusting.  */
static GTY (()) tree vtable_entry_type;

/* Determine if function DECL is needed.  That is, visible to something
   either outside this translation unit, something magic in the system
   configury.  */

bool
cgraph_decide_is_function_needed (struct cgraph_node *node, tree decl)
{ return true; }

/* Process CGRAPH_NEW_FUNCTIONS and perform actions necessary to add these
   functions into callgraph in a way so they look like ordinary reachable
   functions inserted into callgraph already at construction time.  */

bool
cgraph_process_new_functions (void)
{
  bool output = false;
  tree fndecl;
  struct cgraph_node *node;

  varpool_analyze_pending_decls ();
  /*  Note that this queue may grow as its being processed, as the new
      functions may generate new ones.  */
  while (cgraph_new_nodes)
    {
      node = cgraph_new_nodes;
      fndecl = node->decl;
      cgraph_new_nodes = cgraph_new_nodes->next_needed;
      switch (cgraph_state)
	{
	case CGRAPH_STATE_CONSTRUCTION:
	  /* At construction time we just need to finalize function and move
	     it into reachable functions list.  */

	  node->next_needed = NULL;
	  cgraph_finalize_function (fndecl, false);
	  cgraph_mark_reachable_node (node);
	  output = true;
	  break;

	case CGRAPH_STATE_IPA:
	case CGRAPH_STATE_IPA_SSA:
	  gcc_unreachable ();
	  break;

	case CGRAPH_STATE_EXPANSION:
	  /* Functions created during expansion shall be compiled
	     directly.  */
	  node->process = 0;
	  cgraph_expand_function (node);
	  break;

	default:
	  gcc_unreachable ();
	  break;
	}
      cgraph_call_function_insertion_hooks (node);
      varpool_analyze_pending_decls ();
    }
  return output;
}

/* As an GCC extension we allow redefinition of the function.  The
   semantics when both copies of bodies differ is not well defined.
   We replace the old body with new body so in unit at a time mode
   we always use new body, while in normal mode we may end up with
   old body inlined into some functions and new body expanded and
   inlined in others.

   ??? It may make more sense to use one body for inlining and other
   body for expanding the function but this is difficult to do.  */

static void
cgraph_reset_node (struct cgraph_node *node)
{
  /* If node->process is set, then we have already begun whole-unit analysis.
     This is *not* testing for whether we've already emitted the function.
     That case can be sort-of legitimately seen with real function redefinition
     errors.  I would argue that the front end should never present us with
     such a case, but don't enforce that for now.  */
  gcc_assert (!node->process);

  /* Reset our data structures so we can analyze the function again.  */
  memset (&node->local, 0, sizeof (node->local));
  memset (&node->global, 0, sizeof (node->global));
  memset (&node->rtl, 0, sizeof (node->rtl));
  node->analyzed = false;
  node->local.redefined_extern_inline = true;
  node->local.finalized = false;

  gcc_unreachable ();

  cgraph_node_remove_callees (node);

#if 1 /* remove for Modula-3? */
  /* We may need to re-queue the node for assembling in case
     we already proceeded it and ignored as not needed or got
     a re-declaration in IMA mode.  */
  if (node->reachable)
    {
      struct cgraph_node *n;

      for (n = cgraph_nodes_queue; n; n = n->next_needed)
	if (n == node)
	  break;
      if (!n)
	node->reachable = 0;
    }
#endif
}

static void
cgraph_lower_function (struct cgraph_node *node)
{
  if (node->lowered)
    return;

  if (node->nested)
    lower_nested_functions (node->decl);
  gcc_assert (!node->nested);

  tree_lowering_passes (node->decl);
  node->lowered = true;
}

/* DECL has been parsed.  Take it, queue it, compile it at the whim of the
   logic in effect.  If NESTED is true, then our caller cannot stand to have
   the garbage collector run at the moment.  We would need to either create
   a new GC context, or just not compile right now.  */

void
cgraph_finalize_function (tree decl, bool nested)
{
  struct cgraph_node *node = cgraph_node (decl);

  if (node->local.finalized)
    cgraph_reset_node (node);

  node->pid = cgraph_max_pid ++;
  notice_global_symbol (decl);
  node->local.finalized = true;
  node->lowered = DECL_STRUCT_FUNCTION (decl)->cfg != NULL;
  node->finalized_by_frontend = true;

  if (cgraph_decide_is_function_needed (node, decl))
    cgraph_mark_needed_node (node);

  /* Since we reclaim unreachable nodes at the end of every language
     level unit, we need to be conservative about possible entry points
     there.  */
  if ((TREE_PUBLIC (decl) && !DECL_COMDAT (decl) && !DECL_EXTERNAL (decl))
      || DECL_STATIC_CONSTRUCTOR (decl)
      || DECL_STATIC_DESTRUCTOR (decl)
      /* COMDAT virtual functions may be referenced by vtable from
	 other compilation unit.  Still we want to devirtualize calls
	 to those so we need to analyze them.
	 FIXME: We should introduce may edges for this purpose and update
	 their handling in unreachable function removal and inliner too.  */
      || (DECL_VIRTUAL_P (decl) && (DECL_COMDAT (decl) || DECL_EXTERNAL (decl))))
    cgraph_mark_reachable_node (node);

  /* If we've not yet emitted decl, tell the debug info about it.  */
  if (!TREE_ASM_WRITTEN (decl))
    (*debug_hooks->deferred_inline_function) (decl);

  /* Possibly warn about unused parameters.  */
  if (warn_unused_parameter)
    do_warn_unused_parameter (decl);

  if (!nested)
    ggc_collect ();
}

/* C99 extern inline keywords allow changing of declaration after function
   has been finalized.  We need to re-decide if we want to mark the function as
   needed then.   */

void
cgraph_mark_if_needed (tree decl)
{
  struct cgraph_node *node = cgraph_node (decl);
  if (node->local.finalized && cgraph_decide_is_function_needed (node, decl))
    cgraph_mark_needed_node (node);
}

/* Return TRUE if NODE2 is equivalent to NODE or its clone.  */
static bool
clone_of_p (struct cgraph_node *node, struct cgraph_node *node2)
{
  while (node != node2 && node2)
    node2 = node2->clone_of;
  return node2 != NULL;
}

/* Verify edge E count and frequency.  */

static bool
verify_edge_count_and_frequency (struct cgraph_edge *e)
{
  bool error_found = false;
  if (e->count < 0)
    {
      error ("caller edge count is negative");
      error_found = true;
    }
  if (e->frequency < 0)
    {
      error ("caller edge frequency is negative");
      error_found = true;
    }
  if (e->frequency > CGRAPH_FREQ_MAX)
    {
      error ("caller edge frequency is too large");
      error_found = true;
    }
  if (gimple_has_body_p (e->caller->decl)
      && !e->caller->global.inlined_to
      && (e->frequency
	  != compute_call_stmt_bb_frequency (e->caller->decl,
					     gimple_bb (e->call_stmt))))
    {
      error ("caller edge frequency %i does not match BB frequency %i",
	     e->frequency,
	     compute_call_stmt_bb_frequency (e->caller->decl,
					     gimple_bb (e->call_stmt)));
      error_found = true;
    }
  return error_found;
}

/* Switch to THIS_CFUN if needed and print STMT to stderr.  */
static void
cgraph_debug_gimple_stmt (struct function *this_cfun, gimple stmt)
{
  /* debug_gimple_stmt needs correct cfun */
  if (cfun != this_cfun)
    set_cfun (this_cfun);
  debug_gimple_stmt (stmt);
}

/* Verify cgraph nodes of given cgraph node.  */
DEBUG_FUNCTION void
verify_cgraph_node (struct cgraph_node *node)
{
  struct cgraph_edge *e;
  struct function *this_cfun = DECL_STRUCT_FUNCTION (node->decl);
  basic_block this_block;
  gimple_stmt_iterator gsi;
  bool error_found = false;

  if (seen_error ())
    return;

  timevar_push (TV_CGRAPH_VERIFY);
  for (e = node->callees; e; e = e->next_callee)
    if (e->aux)
      {
	error ("aux field set for edge %s->%s",
	       identifier_to_locale (cgraph_node_name (e->caller)),
	       identifier_to_locale (cgraph_node_name (e->callee)));
	error_found = true;
      }
  if (node->count < 0)
    {
      error ("execution count is negative");
      error_found = true;
    }
  if (node->global.inlined_to && node->local.externally_visible)
    {
      error ("externally visible inline clone");
      error_found = true;
    }
  if (node->global.inlined_to && node->address_taken)
    {
      error ("inline clone with address taken");
      error_found = true;
    }
  if (node->global.inlined_to && node->needed)
    {
      error ("inline clone is needed");
      error_found = true;
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    {
      if (e->aux)
	{
	  error ("aux field set for indirect edge from %s",
		 identifier_to_locale (cgraph_node_name (e->caller)));
	  error_found = true;
	}
      if (!e->indirect_unknown_callee
	  || !e->indirect_info)
	{
	  error ("An indirect edge from %s is not marked as indirect or has "
		 "associated indirect_info, the corresponding statement is: ",
		 identifier_to_locale (cgraph_node_name (e->caller)));
	  cgraph_debug_gimple_stmt (this_cfun, e->call_stmt);
	  error_found = true;
	}
    }
  for (e = node->callers; e; e = e->next_caller)
    {
      if (verify_edge_count_and_frequency (e))
	error_found = true;
      if (!e->inline_failed)
	{
	  if (node->global.inlined_to
	      != (e->caller->global.inlined_to
		  ? e->caller->global.inlined_to : e->caller))
	    {
	      error ("inlined_to pointer is wrong");
	      error_found = true;
	    }
	  if (node->callers->next_caller)
	    {
	      error ("multiple inline callers");
	      error_found = true;
	    }
	}
      else
	if (node->global.inlined_to)
	  {
	    error ("inlined_to pointer set for noninline callers");
	    error_found = true;
	  }
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    if (verify_edge_count_and_frequency (e))
      error_found = true;
  if (!node->callers && node->global.inlined_to)
    {
      error ("inlined_to pointer is set but no predecessors found");
      error_found = true;
    }
  if (node->global.inlined_to == node)
    {
      error ("inlined_to pointer refers to itself");
      error_found = true;
    }

  if (!cgraph_get_node (node->decl))
    {
      error ("node not found in cgraph_hash");
      error_found = true;
    }

  if (node->clone_of)
    {
      struct cgraph_node *n;
      for (n = node->clone_of->clones; n; n = n->next_sibling_clone)
        if (n == node)
	  break;
      if (!n)
	{
	  error ("node has wrong clone_of");
	  error_found = true;
	}
    }
  if (node->clones)
    {
      struct cgraph_node *n;
      for (n = node->clones; n; n = n->next_sibling_clone)
        if (n->clone_of != node)
	  break;
      if (n)
	{
	  error ("node has wrong clone list");
	  error_found = true;
	}
    }
  if ((node->prev_sibling_clone || node->next_sibling_clone) && !node->clone_of)
    {
       error ("node is in clone list but it is not clone");
       error_found = true;
    }
  if (!node->prev_sibling_clone && node->clone_of && node->clone_of->clones != node)
    {
      error ("node has wrong prev_clone pointer");
      error_found = true;
    }
  if (node->prev_sibling_clone && node->prev_sibling_clone->next_sibling_clone != node)
    {
      error ("double linked list of clones corrupted");
      error_found = true;
    }
  if (node->same_comdat_group)
    {
      struct cgraph_node *n = node->same_comdat_group;

      if (!DECL_ONE_ONLY (node->decl))
	{
	  error ("non-DECL_ONE_ONLY node in a same_comdat_group list");
	  error_found = true;
	}
      if (n == node)
	{
	  error ("node is alone in a comdat group");
	  error_found = true;
	}
      do
	{
	  if (!n->same_comdat_group)
	    {
	      error ("same_comdat_group is not a circular list");
	      error_found = true;
	      break;
	    }
	  n = n->same_comdat_group;
	}
      while (n != node);
    }

  if (node->analyzed && gimple_has_body_p (node->decl)
      && !TREE_ASM_WRITTEN (node->decl)
      && (!DECL_EXTERNAL (node->decl) || node->global.inlined_to)
      && !flag_wpa)
    {
      if (this_cfun->cfg)
	{
	  /* The nodes we're interested in are never shared, so walk
	     the tree ignoring duplicates.  */
	  struct pointer_set_t *visited_nodes = pointer_set_create ();
	  /* Reach the trees by walking over the CFG, and note the
	     enclosing basic-blocks in the call edges.  */
	  FOR_EACH_BB_FN (this_block, this_cfun)
	    for (gsi = gsi_start_bb (this_block);
                 !gsi_end_p (gsi);
                 gsi_next (&gsi))
	      {
		gimple stmt = gsi_stmt (gsi);
		if (is_gimple_call (stmt))
		  {
		    struct cgraph_edge *e = cgraph_edge (node, stmt);
		    tree decl = gimple_call_fndecl (stmt);
		    if (e)
		      {
			if (e->aux)
			  {
			    error ("shared call_stmt:");
			    cgraph_debug_gimple_stmt (this_cfun, stmt);
			    error_found = true;
			  }
			if (!e->indirect_unknown_callee)
			  {
			    struct cgraph_node *n;

			    if (e->callee->same_body_alias)
			      {
				error ("edge points to same body alias:");
				debug_tree (e->callee->decl);
				error_found = true;
			      }
			    else if (!e->callee->global.inlined_to
				     && decl
				     && cgraph_get_node (decl)
				     && (e->callee->former_clone_of
					 != cgraph_get_node (decl)->decl)
				     && !clone_of_p (cgraph_node (decl),
						     e->callee))
			      {
				error ("edge points to wrong declaration:");
				debug_tree (e->callee->decl);
				fprintf (stderr," Instead of:");
				debug_tree (decl);
				error_found = true;
			      }
			    else if (decl
				     && (n = cgraph_get_node_or_alias (decl))
				     && (n->same_body_alias
					 && n->thunk.thunk_p))
			      {
				error ("a call to thunk improperly represented "
				       "in the call graph:");
				cgraph_debug_gimple_stmt (this_cfun, stmt);
				error_found = true;
			      }
			  }
			else if (decl)
			  {
			    error ("an indirect edge with unknown callee "
				   "corresponding to a call_stmt with "
				   "a known declaration:");
			    error_found = true;
			    cgraph_debug_gimple_stmt (this_cfun, e->call_stmt);
			  }
			e->aux = (void *)1;
		      }
		    else if (decl)
		      {
			error ("missing callgraph edge for call stmt:");
			cgraph_debug_gimple_stmt (this_cfun, stmt);
			error_found = true;
		      }
		  }
	      }
	  pointer_set_destroy (visited_nodes);
	}
      else
	/* No CFG available?!  */
	gcc_unreachable ();

      for (e = node->callees; e; e = e->next_callee)
	{
	  if (!e->aux)
	    {
	      error ("edge %s->%s has no corresponding call_stmt",
		     identifier_to_locale (cgraph_node_name (e->caller)),
		     identifier_to_locale (cgraph_node_name (e->callee)));
	      cgraph_debug_gimple_stmt (this_cfun, e->call_stmt);
	      error_found = true;
	    }
	  e->aux = 0;
	}
      for (e = node->indirect_calls; e; e = e->next_callee)
	{
	  if (!e->aux)
	    {
	      error ("an indirect edge from %s has no corresponding call_stmt",
		     identifier_to_locale (cgraph_node_name (e->caller)));
	      cgraph_debug_gimple_stmt (this_cfun, e->call_stmt);
	      error_found = true;
	    }
	  e->aux = 0;
	}
    }
  if (error_found)
    {
      dump_cgraph_node (stderr, node);
      internal_error ("verify_cgraph_node failed");
    }
  timevar_pop (TV_CGRAPH_VERIFY);
}

/* Verify whole cgraph structure.  */
DEBUG_FUNCTION void
verify_cgraph (void)
{
  struct cgraph_node *node;

  if (seen_error ())
    return;

  for (node = cgraph_nodes; node; node = node->next)
    verify_cgraph_node (node);
}

/* Output all asm statements we have stored up to be output.  */

static void
cgraph_output_pending_asms (void)
{
  struct cgraph_asm_node *can;

  if (seen_error ())
    return;

  for (can = cgraph_asm_nodes; can; can = can->next)
    assemble_asm (can->asm_str);
  cgraph_asm_nodes = NULL;
}

/* Analyze the function scheduled to be output.  */
static void
cgraph_analyze_function (struct cgraph_node *node)
{
  tree save = current_function_decl;
  tree decl = node->decl;

  current_function_decl = decl;
  push_cfun (DECL_STRUCT_FUNCTION (decl));

  assign_assembler_name_if_neeeded (node->decl);

  /* disregard_inline_limits affects topological order of the early optimization,
     so we need to compute it ahead of rest of inline parameters.  */
  node->local.disregard_inline_limits
    = DECL_DISREGARD_INLINE_LIMITS (node->decl);

  /* Make sure to gimplify bodies only once.  During analyzing a
     function we lower it, which will require gimplified nested
     functions, so we can end up here with an already gimplified
     body.  */
  if (!gimple_body (decl))
    gimplify_function_tree (decl);
  dump_function (TDI_generic, decl);

  cgraph_lower_function (node);
  node->analyzed = true;

  pop_cfun ();
  current_function_decl = save;
}

/* Process attributes common for vars and functions.  */

static void
process_common_attributes (tree decl)
{
  tree weakref = lookup_attribute ("weakref", DECL_ATTRIBUTES (decl));

  if (weakref && !lookup_attribute ("alias", DECL_ATTRIBUTES (decl)))
    {
      warning_at (DECL_SOURCE_LOCATION (decl), OPT_Wattributes,
		  "%<weakref%> attribute should be accompanied with"
		  " an %<alias%> attribute");
      DECL_WEAK (decl) = 0;
      DECL_ATTRIBUTES (decl) = remove_attribute ("weakref",
						 DECL_ATTRIBUTES (decl));
    }
}

/* Look for externally_visible and used attributes and mark cgraph nodes
   accordingly.

   We cannot mark the nodes at the point the attributes are processed (in
   handle_*_attribute) because the copy of the declarations available at that
   point may not be canonical.  For example, in:

    void f();
    void f() __attribute__((used));

   the declaration we see in handle_used_attribute will be the second
   declaration -- but the front end will subsequently merge that declaration
   with the original declaration and discard the second declaration.

   Furthermore, we can't mark these nodes in cgraph_finalize_function because:

    void f() {}
    void f() __attribute__((externally_visible));

   is valid.

   So, we walk the nodes at the end of the translation unit, applying the
   attributes at that point.  */

static void
process_function_and_variable_attributes (struct cgraph_node *first,
                                          struct varpool_node *first_var)
{
  struct cgraph_node *node;
  struct varpool_node *vnode;

  for (node = cgraph_nodes; node != first; node = node->next)
    {
      tree decl = node->decl;
      if (DECL_PRESERVE_P (decl))
	cgraph_mark_needed_node (node);
      if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
	  && lookup_attribute ("dllexport", DECL_ATTRIBUTES (decl))
	  && TREE_PUBLIC (node->decl))
	{
	  if (node->local.finalized)
	    cgraph_mark_needed_node (node);
	}
      else if (lookup_attribute ("externally_visible", DECL_ATTRIBUTES (decl)))
	{
	  if (! TREE_PUBLIC (node->decl))
	    warning_at (DECL_SOURCE_LOCATION (node->decl), OPT_Wattributes,
			"%<externally_visible%>"
			" attribute have effect only on public objects");
	  else if (node->local.finalized)
	     cgraph_mark_needed_node (node);
	}
      if (lookup_attribute ("weakref", DECL_ATTRIBUTES (decl))
	  && node->local.finalized)
	{
	  warning_at (DECL_SOURCE_LOCATION (node->decl), OPT_Wattributes,
		      "%<weakref%> attribute ignored"
		      " because function is defined");
	  DECL_WEAK (decl) = 0;
	  DECL_ATTRIBUTES (decl) = remove_attribute ("weakref",
						     DECL_ATTRIBUTES (decl));
	}
      process_common_attributes (decl);
    }
  for (vnode = varpool_nodes; vnode != first_var; vnode = vnode->next)
    {
      tree decl = vnode->decl;
      if (DECL_PRESERVE_P (decl))
	{
	  vnode->force_output = true;
	  if (vnode->finalized)
	    varpool_mark_needed_node (vnode);
	}
      if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
	  && lookup_attribute ("dllexport", DECL_ATTRIBUTES (decl))
	  && TREE_PUBLIC (vnode->decl))
	{
	  if (vnode->finalized)
	    varpool_mark_needed_node (vnode);
	}
      else if (lookup_attribute ("externally_visible", DECL_ATTRIBUTES (decl)))
	{
	  if (! TREE_PUBLIC (vnode->decl))
	    warning_at (DECL_SOURCE_LOCATION (vnode->decl), OPT_Wattributes,
			"%<externally_visible%>"
			" attribute have effect only on public objects");
	  else if (vnode->finalized)
	    varpool_mark_needed_node (vnode);
	}
      if (lookup_attribute ("weakref", DECL_ATTRIBUTES (decl))
	  && vnode->finalized
	  && DECL_INITIAL (decl))
	{
	  warning_at (DECL_SOURCE_LOCATION (vnode->decl), OPT_Wattributes,
		      "%<weakref%> attribute ignored"
		      " because variable is initialized");
	  DECL_WEAK (decl) = 0;
	  DECL_ATTRIBUTES (decl) = remove_attribute ("weakref",
						      DECL_ATTRIBUTES (decl));
	}
      process_common_attributes (decl);
    }
}

/* Process CGRAPH_NODES_NEEDED queue, analyze each function (and transitively
   each reachable functions) and build cgraph.
   The function can be called multiple times after inserting new nodes
   into beginning of queue.  Just the new part of queue is re-scanned then.  */

static void
cgraph_analyze_functions (void)
{
  /* Keep track of already processed nodes when called multiple times for
     intermodule optimization.  */
  static struct cgraph_node *first_analyzed;
  struct cgraph_node *first_processed = first_analyzed;
  static struct varpool_node *first_analyzed_var;
  struct cgraph_node *node, *next;

  bitmap_obstack_initialize (NULL);
  process_function_and_variable_attributes (first_processed,
					    first_analyzed_var);
  first_processed = cgraph_nodes;
  first_analyzed_var = varpool_nodes;
  varpool_analyze_pending_decls ();
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Initial entry points:");
      for (node = cgraph_nodes; node != first_analyzed; node = node->next)
	if (node->needed)
	  fprintf (cgraph_dump_file, " %s", cgraph_node_name (node));
      fprintf (cgraph_dump_file, "\n");
    }
  cgraph_process_new_functions ();

  /* Propagate reachability flag and lower representation of all reachable
     functions.  In the future, lowering will introduce new functions and
     new entry points on the way (by template instantiation and virtual
     method table generation for instance).  */
  while (cgraph_nodes_queue)
    {
      struct cgraph_edge *edge;
      tree decl = cgraph_nodes_queue->decl;

      node = cgraph_nodes_queue;
      cgraph_nodes_queue = cgraph_nodes_queue->next_needed;
      node->next_needed = NULL;

      /* ??? It is possible to create extern inline function and later using
	 weak alias attribute to kill its body. See
	 gcc.c-torture/compile/20011119-1.c  */
      if (!DECL_STRUCT_FUNCTION (decl))
	{
	  cgraph_reset_node (node);
	  continue;
	}

      if (!node->analyzed)
	cgraph_analyze_function (node);

      for (edge = node->callees; edge; edge = edge->next_callee)
	if (!edge->callee->reachable)
	  cgraph_mark_reachable_node (edge->callee);

      if (node->same_comdat_group)
	{
	  for (next = node->same_comdat_group;
	       next != node;
	       next = next->same_comdat_group)
	    cgraph_mark_reachable_node (next);
	}

      /* If decl is a clone of an abstract function, mark that abstract
	 function so that we don't release its body. The DECL_INITIAL() of that
         abstract function declaration will be later needed to output debug info.  */
      if (DECL_ABSTRACT_ORIGIN (decl))
	{
	  struct cgraph_node *origin_node = cgraph_node (DECL_ABSTRACT_ORIGIN (decl));
	  origin_node->abstract_and_needed = true;
	}

      /* We finalize local static variables during constructing callgraph
         edges.  Process their attributes too.  */
      process_function_and_variable_attributes (first_processed,
						first_analyzed_var);
      first_processed = cgraph_nodes;
      first_analyzed_var = varpool_nodes;
      varpool_analyze_pending_decls ();
      cgraph_process_new_functions ();
    }

  /* Collect entry points to the unit.  */
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Unit entry points:");
      for (node = cgraph_nodes; node != first_analyzed; node = node->next)
	if (node->needed)
	  fprintf (cgraph_dump_file, " %s", cgraph_node_name (node));
      fprintf (cgraph_dump_file, "\n\nInitial ");
      dump_cgraph (cgraph_dump_file);
      dump_varpool (cgraph_dump_file);
    }

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\nReclaiming functions:");

  for (node = cgraph_nodes; node != first_analyzed; node = next)
    {
      tree decl = node->decl;
      next = node->next;

      if (node->local.finalized && !gimple_has_body_p (decl))
	cgraph_reset_node (node);

      if (!node->reachable && gimple_has_body_p (decl))
	{
	  if (cgraph_dump_file)
	    fprintf (cgraph_dump_file, " %s", cgraph_node_name (node));
	  cgraph_remove_node (node);
	  continue;
	}
      else
	node->next_needed = NULL;
      gcc_assert (!node->local.finalized || gimple_has_body_p (decl));
      gcc_assert (node->analyzed == node->local.finalized);
    }
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "\n\nReclaimed ");
      dump_cgraph (cgraph_dump_file);
      dump_varpool (cgraph_dump_file);
    }
  bitmap_obstack_release (NULL);
  first_analyzed = cgraph_nodes;
  ggc_collect ();
}


/* Analyze the whole compilation unit once it is parsed completely.  */

void
cgraph_finalize_compilation_unit (void)
{
  timevar_push (TV_CGRAPH);

  /* Do not skip analyzing the functions if there were errors, we
     miss diagnostics for following functions otherwise.  */

  /* Emit size functions we didn't inline.  */
  finalize_size_functions ();

  /* Mark alias targets necessary and emit diagnostics.  */
  finish_aliases_1 ();

  if (!quiet_flag)
    {
      fprintf (stderr, "\nAnalyzing compilation unit\n");
      fflush (stderr);
    }

  /* Gimplify and lower all functions, compute reachability and
     remove unreachable nodes.  */
  cgraph_analyze_functions ();

  /* Mark alias targets necessary and emit diagnostics.  */
  finish_aliases_1 ();

  /* Gimplify and lower thunks.  */
  cgraph_analyze_functions ();

  timevar_pop (TV_CGRAPH);
}


/* Figure out what functions we want to assemble.  */

static void
cgraph_mark_functions_to_output (void)
{
  struct cgraph_node *node;
#ifdef ENABLE_CHECKING
  bool check_same_comdat_groups = false;

  for (node = cgraph_nodes; node; node = node->next)
    gcc_assert (!node->process);
#endif

  for (node = cgraph_nodes; node; node = node->next)
    {
      tree decl = node->decl;
      struct cgraph_edge *e;

      gcc_assert (!node->process || node->same_comdat_group);
      if (node->process)
	continue;

      for (e = node->callers; e; e = e->next_caller)
	if (e->inline_failed)
	  break;

      /* We need to output all local functions that are used and not
	 always inlined, as well as those that are reachable from
	 outside the current compilation unit.  */
      if (node->analyzed
	  && !node->global.inlined_to
	  && (!cgraph_only_called_directly_p (node)
	      || (e && node->reachable))
	  && !TREE_ASM_WRITTEN (decl)
	  && !DECL_EXTERNAL (decl))
	{
	  node->process = 1;
	  if (node->same_comdat_group)
	    {
	      struct cgraph_node *next;
	      for (next = node->same_comdat_group;
		   next != node;
		   next = next->same_comdat_group)
		next->process = 1;
	    }
	}
      else if (node->same_comdat_group)
	{
#ifdef ENABLE_CHECKING
	  check_same_comdat_groups = true;
#endif
	}
      else
	{
	  /* We should've reclaimed all functions that are not needed.  */
#ifdef ENABLE_CHECKING
	  if (!node->global.inlined_to
	      && gimple_has_body_p (decl)
	      /* FIXME: in ltrans unit when offline copy is outside partition but inline copies
		 are inside partition, we can end up not removing the body since we no longer
		 have analyzed node pointing to it.  */
	      && !node->in_other_partition
	      && !DECL_EXTERNAL (decl))
	    {
	      dump_cgraph_node (stderr, node);
	      internal_error ("failed to reclaim unneeded function");
	    }
#endif
	  gcc_assert (node->global.inlined_to
		      || !gimple_has_body_p (decl)
		      || node->in_other_partition
		      || DECL_EXTERNAL (decl));

	}

    }
#ifdef ENABLE_CHECKING
  if (check_same_comdat_groups)
    for (node = cgraph_nodes; node; node = node->next)
      if (node->same_comdat_group && !node->process)
	{
	  tree decl = node->decl;
	  if (!node->global.inlined_to
	      && gimple_has_body_p (decl)
	      /* FIXME: in ltrans unit when offline copy is outside partition but inline copies
		 are inside partition, we can end up not removing the body since we no longer
		 have analyzed node pointing to it.  */
	      && !node->in_other_partition
	      && !DECL_EXTERNAL (decl))
	    {
	      dump_cgraph_node (stderr, node);
	      internal_error ("failed to reclaim unneeded function");
	    }
	}
#endif
}

/* DECL is FUNCTION_DECL.  Initialize datastructures so DECL is a function
   in lowered gimple form.
   
   Set current_function_decl and cfun to newly constructed empty function body.
   return basic block in the function body.  */

static basic_block
init_lowered_empty_function (tree decl)
{
  basic_block bb;

  current_function_decl = decl;
  allocate_struct_function (decl, false);
  gimple_register_cfg_hooks ();
  init_empty_tree_cfg ();
  init_tree_ssa (cfun);
  init_ssa_operands ();
  cfun->gimple_df->in_ssa_p = true;
  DECL_INITIAL (decl) = make_node (BLOCK);

  DECL_SAVED_TREE (decl) = error_mark_node;
  cfun->curr_properties |=
    (PROP_gimple_lcf | PROP_gimple_leh | PROP_cfg | PROP_referenced_vars |
     PROP_ssa);

  /* Create BB for body of the function and connect it properly.  */
  bb = create_basic_block (NULL, (void *) 0, ENTRY_BLOCK_PTR);
  make_edge (ENTRY_BLOCK_PTR, bb, 0);
  make_edge (bb, EXIT_BLOCK_PTR, 0);

  return bb;
}

/* Adjust PTR by the constant FIXED_OFFSET, and by the vtable
   offset indicated by VIRTUAL_OFFSET, if that is
   non-null. THIS_ADJUSTING is nonzero for a this adjusting thunk and
   zero for a result adjusting thunk.  */

static tree
thunk_adjust (gimple_stmt_iterator * bsi,
	      tree ptr, bool this_adjusting,
	      HOST_WIDE_INT fixed_offset, tree virtual_offset)
{
  gimple stmt;
  tree ret;
  
  gcc_unreachable ();

  if (this_adjusting
      && fixed_offset != 0)
    {
      stmt = gimple_build_assign (ptr,
				  fold_build2_loc (input_location,
						   POINTER_PLUS_EXPR,
						   TREE_TYPE (ptr), ptr,
						   size_int (fixed_offset)));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);
    }

  /* If there's a virtual offset, look up that value in the vtable and
     adjust the pointer again.  */
  if (virtual_offset)
    {
      tree vtabletmp;
      tree vtabletmp2;
      tree vtabletmp3;
      tree offsettmp;

      if (!vtable_entry_type)
	{
	  tree vfunc_type = make_node (FUNCTION_TYPE);
	  TREE_TYPE (vfunc_type) = integer_type_node;
	  TYPE_ARG_TYPES (vfunc_type) = NULL_TREE;
	  layout_type (vfunc_type);

	  vtable_entry_type = build_pointer_type (vfunc_type);
	}

      vtabletmp =
	create_tmp_var (build_pointer_type
			(build_pointer_type (vtable_entry_type)), "vptr");

      /* The vptr is always at offset zero in the object.  */
      stmt = gimple_build_assign (vtabletmp,
				  build1 (NOP_EXPR, TREE_TYPE (vtabletmp),
					  ptr));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);
      mark_symbols_for_renaming (stmt);
      gcc_unreachable ();
      //find_referenced_vars_in (stmt);

      /* Form the vtable address.  */
      vtabletmp2 = create_tmp_var (TREE_TYPE (TREE_TYPE (vtabletmp)),
				   "vtableaddr");
      stmt = gimple_build_assign (vtabletmp2,
				  build_simple_mem_ref (vtabletmp));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);
      mark_symbols_for_renaming (stmt);
      gcc_unreachable ();
      //find_referenced_vars_in (stmt);

      /* Find the entry with the vcall offset.  */
      stmt = gimple_build_assign (vtabletmp2,
				  fold_build2_loc (input_location,
						   POINTER_PLUS_EXPR,
						   TREE_TYPE (vtabletmp2),
						   vtabletmp2,
						   fold_convert (sizetype,
								 virtual_offset)));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);

      /* Get the offset itself.  */
      vtabletmp3 = create_tmp_var (TREE_TYPE (TREE_TYPE (vtabletmp2)),
				   "vcalloffset");
      stmt = gimple_build_assign (vtabletmp3,
				  build_simple_mem_ref (vtabletmp2));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);
      mark_symbols_for_renaming (stmt);
      gcc_unreachable ();
      //find_referenced_vars_in (stmt);

      /* Cast to sizetype.  */
      offsettmp = create_tmp_var (sizetype, "offset");
      stmt = gimple_build_assign (offsettmp, fold_convert (sizetype, vtabletmp3));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);
      mark_symbols_for_renaming (stmt);
      gcc_unreachable ();
      //find_referenced_vars_in (stmt);

      /* Adjust the `this' pointer.  */
      ptr = fold_build2_loc (input_location,
			     POINTER_PLUS_EXPR, TREE_TYPE (ptr), ptr,
			     offsettmp);
    }

  if (!this_adjusting
      && fixed_offset != 0)
    /* Adjust the pointer by the constant.  */
    {
      tree ptrtmp;

      if (TREE_CODE (ptr) == VAR_DECL)
        ptrtmp = ptr;
      else
        {
          ptrtmp = create_tmp_var (TREE_TYPE (ptr), "ptr");
          stmt = gimple_build_assign (ptrtmp, ptr);
	  gsi_insert_after (bsi, stmt, GSI_NEW_STMT);
	  mark_symbols_for_renaming (stmt);
          gcc_unreachable ();
	  //find_referenced_vars_in (stmt);
	}
      ptr = fold_build2_loc (input_location,
			     POINTER_PLUS_EXPR, TREE_TYPE (ptrtmp), ptrtmp,
			     size_int (fixed_offset));
    }

  /* Emit the statement and gimplify the adjustment expression.  */
  ret = create_tmp_var (TREE_TYPE (ptr), "adjusted_this");
  stmt = gimple_build_assign (ret, ptr);
  mark_symbols_for_renaming (stmt);
  gcc_unreachable ();
  //find_referenced_vars_in (stmt);
  gsi_insert_after (bsi, stmt, GSI_NEW_STMT);

  return ret;
}

/* Produce assembler for thunk NODE.  */

static void
assemble_thunk (struct cgraph_node *node)
{
  bool this_adjusting = node->thunk.this_adjusting;
  HOST_WIDE_INT fixed_offset = node->thunk.fixed_offset;
  HOST_WIDE_INT virtual_value = node->thunk.virtual_value;
  tree virtual_offset = NULL;
  tree alias = node->thunk.alias;
  tree thunk_fndecl = node->decl;
  tree a = DECL_ARGUMENTS (thunk_fndecl);

  current_function_decl = thunk_fndecl;

  /* Ensure thunks are emitted in their correct sections.  */
  resolve_unique_section (thunk_fndecl, 0, flag_function_sections);

  if (this_adjusting
      && targetm.asm_out.can_output_mi_thunk (thunk_fndecl, fixed_offset,
					      virtual_value, alias))
    {
      const char *fnname;
      tree fn_block;
      
      DECL_RESULT (thunk_fndecl)
	= build_decl (DECL_SOURCE_LOCATION (thunk_fndecl),
		      RESULT_DECL, 0, integer_type_node);
      fnname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (thunk_fndecl));

      /* The back end expects DECL_INITIAL to contain a BLOCK, so we
	 create one.  */
      fn_block = make_node (BLOCK);
      BLOCK_VARS (fn_block) = a;
      DECL_INITIAL (thunk_fndecl) = fn_block;
      init_function_start (thunk_fndecl);
      cfun->is_thunk = 1;
      assemble_start_function (thunk_fndecl, fnname);

      targetm.asm_out.output_mi_thunk (asm_out_file, thunk_fndecl,
				       fixed_offset, virtual_value, alias);

      assemble_end_function (thunk_fndecl, fnname);
      init_insn_lengths ();
      free_after_compilation (cfun);
      set_cfun (NULL);
      TREE_ASM_WRITTEN (thunk_fndecl) = 1;
    }
  else
    {
      tree restype;
      basic_block bb, then_bb, else_bb, return_bb;
      gimple_stmt_iterator bsi;
      int nargs = 0;
      tree arg;
      int i;
      tree resdecl;
      tree restmp = NULL;
      VEC(tree, heap) *vargs;

      gimple call;
      gimple ret;

      DECL_IGNORED_P (thunk_fndecl) = 1;
      bitmap_obstack_initialize (NULL);

      if (node->thunk.virtual_offset_p)
        virtual_offset = size_int (virtual_value);

      /* Build the return declaration for the function.  */
      restype = TREE_TYPE (TREE_TYPE (thunk_fndecl));
      if (DECL_RESULT (thunk_fndecl) == NULL_TREE)
	{
	  resdecl = build_decl (input_location, RESULT_DECL, 0, restype);
	  DECL_ARTIFICIAL (resdecl) = 1;
	  DECL_IGNORED_P (resdecl) = 1;
	  DECL_RESULT (thunk_fndecl) = resdecl;
	}
      else
	resdecl = DECL_RESULT (thunk_fndecl);

      bb = then_bb = else_bb = return_bb = init_lowered_empty_function (thunk_fndecl);

      bsi = gsi_start_bb (bb);

      /* Build call to the function being thunked.  */
      if (!VOID_TYPE_P (restype))
	{
	  if (!is_gimple_reg_type (restype))
	    {
	      restmp = resdecl;
	      add_local_decl (cfun, restmp);
	      BLOCK_VARS (DECL_INITIAL (current_function_decl)) = restmp;
	    }
	  else
            restmp = create_tmp_var_raw (restype, "retval");
	}

      for (arg = a; arg; arg = DECL_CHAIN (arg))
        nargs++;
      vargs = VEC_alloc (tree, heap, nargs);
      if (this_adjusting)
        VEC_quick_push (tree, vargs,
			thunk_adjust (&bsi,
				      a, 1, fixed_offset,
				      virtual_offset));
      else
        VEC_quick_push (tree, vargs, a);
      for (i = 1, arg = DECL_CHAIN (a); i < nargs; i++, arg = DECL_CHAIN (arg))
        VEC_quick_push (tree, vargs, arg);
      call = gimple_build_call_vec (build_fold_addr_expr_loc (0, alias), vargs);
      VEC_free (tree, heap, vargs);
      gimple_call_set_cannot_inline (call, true);
      gimple_call_set_from_thunk (call, true);
      if (restmp)
        gimple_call_set_lhs (call, restmp);
      gsi_insert_after (&bsi, call, GSI_NEW_STMT);
      mark_symbols_for_renaming (call);
      gcc_unreachable ();
      //find_referenced_vars_in (call);
      update_stmt (call);

      if (restmp && !this_adjusting)
        {
	  tree true_label = NULL_TREE;

	  if (TREE_CODE (TREE_TYPE (restmp)) == POINTER_TYPE)
	    {
	      gimple stmt;
	      /* If the return type is a pointer, we need to
		 protect against NULL.  We know there will be an
		 adjustment, because that's why we're emitting a
		 thunk.  */
	      then_bb = create_basic_block (NULL, (void *) 0, bb);
	      return_bb = create_basic_block (NULL, (void *) 0, then_bb);
	      else_bb = create_basic_block (NULL, (void *) 0, else_bb);
	      remove_edge (single_succ_edge (bb));
	      true_label = gimple_block_label (then_bb);
	      stmt = gimple_build_cond (NE_EXPR, restmp,
	      				build_zero_cst (TREE_TYPE (restmp)),
	      			        NULL_TREE, NULL_TREE);
	      gsi_insert_after (&bsi, stmt, GSI_NEW_STMT);
	      make_edge (bb, then_bb, EDGE_TRUE_VALUE);
	      make_edge (bb, else_bb, EDGE_FALSE_VALUE);
	      make_edge (return_bb, EXIT_BLOCK_PTR, 0);
	      make_edge (then_bb, return_bb, EDGE_FALLTHRU);
	      make_edge (else_bb, return_bb, EDGE_FALLTHRU);
	      bsi = gsi_last_bb (then_bb);
	    }

	  restmp = thunk_adjust (&bsi, restmp, /*this_adjusting=*/0,
			         fixed_offset, virtual_offset);
	  if (true_label)
	    {
	      gimple stmt;
	      bsi = gsi_last_bb (else_bb);
	      stmt = gimple_build_assign (restmp,
					  build_zero_cst (TREE_TYPE (restmp)));
	      gsi_insert_after (&bsi, stmt, GSI_NEW_STMT);
	      bsi = gsi_last_bb (return_bb);
	    }
	}
      else
        gimple_call_set_tail (call, true);

      /* Build return value.  */
      ret = gimple_build_return (restmp);
      gsi_insert_after (&bsi, ret, GSI_NEW_STMT);

      delete_unreachable_blocks ();
      update_ssa (TODO_update_ssa);

      cgraph_remove_same_body_alias (node);
      /* Since we want to emit the thunk, we explicitly mark its name as
	 referenced.  */
      cgraph_add_new_function (thunk_fndecl, true);
      bitmap_obstack_release (NULL);
    }
  current_function_decl = NULL;
}

/* Expand function specified by NODE.  */

static void
cgraph_expand_function (struct cgraph_node *node)
{
  tree decl = node->decl;

  /* We ought to not compile any inline clones.  */
  gcc_assert (!node->global.inlined_to);

  announce_function (decl);
  node->process = 0;
  if (node->same_body)
    {
      struct cgraph_node *alias, *next;
      bool saved_alias = node->alias;
      for (alias = node->same_body;
      	   alias && alias->next; alias = alias->next)
        ;
      /* Walk aliases in the order they were created; it is possible that
         thunks refers to the aliases made earlier.  */
      for (; alias; alias = next)
        {
	  next = alias->previous;
	  if (!alias->thunk.thunk_p)
	    assemble_alias (alias->decl,
			    DECL_ASSEMBLER_NAME (alias->thunk.alias));
	  else
	    assemble_thunk (alias);
	}
      node->alias = saved_alias;
      cgraph_process_new_functions ();
    }

  gcc_assert (node->lowered);

  /* Generate RTL for the body of DECL.  */
  tree_rest_of_compilation (decl);

  /* Make sure that BE didn't give up on compiling.  */
  gcc_assert (TREE_ASM_WRITTEN (decl));
  current_function_decl = NULL;
  cgraph_release_function_body (node);
  /* Eliminate all call edges.  This is important so the GIMPLE_CALL no longer
     points to the dead function body.  */
  cgraph_node_remove_callees (node);

  cgraph_function_flags_ready = true;
}

/* Return true when CALLER_DECL should be inlined into CALLEE_DECL.  */

bool
cgraph_inline_p (struct cgraph_edge *e, cgraph_inline_failed_t *reason)
{
  *reason = e->inline_failed;
  return !e->inline_failed;
}

/* This is used to sort the node types by the cgraph order number.  */

enum cgraph_order_sort_kind
{
  ORDER_UNDEFINED = 0,
  ORDER_FUNCTION,
  ORDER_VAR,
  ORDER_ASM
};

struct cgraph_order_sort
{
  enum cgraph_order_sort_kind kind;
  union
  {
    struct cgraph_node *f;
    struct varpool_node *v;
    struct cgraph_asm_node *a;
  } u;
};


/* Perform simple optimizations based on callgraph.  */

void
cgraph_optimize (void)
{ }

void
init_cgraph (void)
{
  if (!cgraph_dump_file)
    cgraph_dump_file = dump_begin (TDI_cgraph, NULL);
}

/* Once all functions from compilation unit are in memory, produce all clones
   and update all calls.  We might also do this on demand if we don't want to
   bring all functions to memory prior compilation, but current WHOPR
   implementation does that and it is is bit easier to keep everything right in
   this order.  */
void
cgraph_materialize_all_clones (void)
{ }

EXTERN_C_END

#include "gt-cgraphunit.h"
