/* Modula-3: modified */

/* Top-level control of tree optimizations.
   Copyright 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tm_p.h"
#include "basic-block.h"
#include "output.h"
#include "flags.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "function.h"
#include "langhooks.h"
#include "diagnostic-core.h"
#include "toplev.h"
#include "flags.h"
#include "cgraph.h"
#include "tree-inline.h"
#include "tree-mudflap.h"
#include "tree-pass.h"
#include "ggc.h"
#include "cgraph.h"
#include "graph.h"
#include "cfgloop.h"
#include "except.h"
#include "plugin.h"
#include "regset.h"	/* FIXME: For reg_obstack.  */

EXTERN_C_START

struct gimple_opt_pass pass_all_optimizations =
{
 {
  GIMPLE_PASS,
  "*all_optimizations",			/* name */
  NULL,		                        /* gate */
  NULL,					/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_OPTIMIZE,				/* tv_id */
  0,					/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
 }
};

/* Pass: do the actions required to finish with tree-ssa optimization
   passes.  */

unsigned int
execute_free_datastructures (void)
{
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);

  /* And get rid of annotations we no longer need.  */
  delete_tree_cfg_annotations ();

  return 0;
}

void
tree_lowering_passes (tree fn)
{
  tree saved_current_function_decl = current_function_decl;

  current_function_decl = fn;
  push_cfun (DECL_STRUCT_FUNCTION (fn));
  gimple_register_cfg_hooks ();
  bitmap_obstack_initialize (NULL);
  execute_pass_list (all_lowering_passes);
#if 0 /* Modula-3 */
  if (optimize && cgraph_global_info_ready)
    execute_pass_list (pass_early_local_passes.pass.sub);
#endif
  free_dominance_info (CDI_POST_DOMINATORS);
  free_dominance_info (CDI_DOMINATORS);
  compact_blocks ();
  current_function_decl = saved_current_function_decl;
  bitmap_obstack_release (NULL);
  pop_cfun ();
}

/* For functions-as-trees languages, this performs all optimization and
   compilation for FNDECL.  */

void
tree_rest_of_compilation (tree fndecl)
{
  location_t saved_loc;

  timevar_push (TV_REST_OF_COMPILATION);

  gcc_assert (cgraph_global_info_ready);

  /* Initialize the default bitmap obstack.  */
  bitmap_obstack_initialize (NULL);

  /* Initialize the RTL code for the function.  */
  current_function_decl = fndecl;
  saved_loc = input_location;
  input_location = DECL_SOURCE_LOCATION (fndecl);
  init_function_start (fndecl);

  /* Even though we're inside a function body, we still don't want to
     call expand_expr to calculate the size of a variable-sized array.
     We haven't necessarily assigned RTL to all variables yet, so it's
     not safe to try to expand expressions involving them.  */
  cfun->dont_save_pending_sizes_p = 1;

  gimple_register_cfg_hooks ();

  bitmap_obstack_initialize (&reg_obstack); /* FIXME, only at RTL generation*/

  execute_all_ipa_transforms ();

  /* Perform all tree transforms and optimizations.  */

  /* Signal the start of passes.  */
  invoke_plugin_callbacks (PLUGIN_ALL_PASSES_START, NULL);

  execute_pass_list (all_passes);

  /* Signal the end of passes.  */
  invoke_plugin_callbacks (PLUGIN_ALL_PASSES_END, NULL);

  bitmap_obstack_release (&reg_obstack);

  /* Release the default bitmap obstack.  */
  bitmap_obstack_release (NULL);

  set_cfun (NULL);

  /* If requested, warn about function definitions where the function will
     return a value (usually of some struct or union type) which itself will
     take up a lot of stack space.  */
  if (warn_larger_than && !DECL_EXTERNAL (fndecl) && TREE_TYPE (fndecl))
    {
      tree ret_type = TREE_TYPE (TREE_TYPE (fndecl));

      if (ret_type && TYPE_SIZE_UNIT (ret_type)
	  && TREE_CODE (TYPE_SIZE_UNIT (ret_type)) == INTEGER_CST
	  && 0 < compare_tree_int (TYPE_SIZE_UNIT (ret_type),
				   larger_than_size))
	{
	  unsigned int size_as_int
	    = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (ret_type));

	  if (compare_tree_int (TYPE_SIZE_UNIT (ret_type), size_as_int) == 0)
	    warning (OPT_Wlarger_than_, "size of return value of %q+D is %u bytes",
                     fndecl, size_as_int);
	  else
	    warning (OPT_Wlarger_than_, "size of return value of %q+D is larger than %wd bytes",
                     fndecl, larger_than_size);
	}
    }

  gimple_set_body (fndecl, NULL);
  if (DECL_STRUCT_FUNCTION (fndecl) == 0
      && !cgraph_node (fndecl)->origin)
    {
      /* Stop pointing to the local nodes about to be freed.
	 But DECL_INITIAL must remain nonzero so we know this
	 was an actual function definition.
	 For a nested function, this is done in c_pop_function_context.
	 If rest_of_compilation set this to 0, leave it 0.  */
      if (DECL_INITIAL (fndecl) != 0)
	DECL_INITIAL (fndecl) = error_mark_node;
    }

  input_location = saved_loc;

  ggc_collect ();
  timevar_pop (TV_REST_OF_COMPILATION);
}

EXTERN_C_END
