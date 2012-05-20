/* Modula-3: modified */

/* Top level of GCC compilers (cc1, cc1plus, etc.)
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
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

/* This is the top level of cc1/c++.
   It parses command args, opens files, invokes the various passes
   in the proper order, and counts the time used by each.
   Error messages and low-level interface to malloc also handled here.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "line-map.h"
#include "input.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "flags.h"
#include "insn-attr.h"
#include "insn-config.h"
#include "insn-flags.h"
#include "hard-reg-set.h"
#include "recog.h"
#include "output.h"
#include "except.h"
#include "function.h"
#include "toplev.h"
#include "expr.h"
#include "basic-block.h"
#include "intl.h"
#include "ggc.h"
#include "graph.h"
#include "regs.h"
#include "timevar.h"
#include "diagnostic-core.h"
#include "params.h"
#include "reload.h"
#include "dwarf2asm.h"
#include "integrate.h"
#include "debug.h"
#include "target.h"
#include "langhooks.h"
#include "cfglayout.h"
#include "cfgloop.h"
#include "hosthooks.h"
#include "cgraph.h"
#include "opts.h"
#include "tree-inline.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-dump.h"
#include "df.h"
#include "predict.h"
#include "plugin.h"

#if defined (DWARF2_UNWIND_INFO) || defined (DWARF2_DEBUGGING_INFO)
#include "dwarf2out.h"
#endif

#if defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)
#include "dbxout.h"
#endif

#ifdef SDB_DEBUGGING_INFO
#include "sdbout.h"
#endif

#ifdef XCOFF_DEBUGGING_INFO
#include "xcoffout.h"		/* Needed for external data
				   declarations for e.g. AIX 4.x.  */
#endif

EXTERN_C_START

/* This is used for debugging.  It allows the current pass to printed
   from anywhere in compilation.
   The variable current_pass is also used for statistics and plugins.  */
struct opt_pass *current_pass;

/* Call from anywhere to find out what pass this is.  Useful for
   printing out debugging information deep inside an service
   routine.  */
void
print_current_pass (FILE *file)
{
  if (current_pass)
    fprintf (file, "current pass = %s (%d)\n",
	     current_pass->name, current_pass->static_pass_number);
  else
    fprintf (file, "no current pass.\n");
}


/* Call from the debugger to get the current pass name.  */
DEBUG_FUNCTION void
debug_pass (void)
{
  print_current_pass (stderr);
}



/* Global variables used to communicate with passes.  */
int dump_flags;
bool in_gimple_form;
bool first_pass_instance;


/* This is called from various places for FUNCTION_DECL, VAR_DECL,
   and TYPE_DECL nodes.

   This does nothing for local (non-static) variables, unless the
   variable is a register variable with DECL_ASSEMBLER_NAME set.  In
   that case, or if the variable is not an automatic, it sets up the
   RTL and outputs any assembler code (label definition, storage
   allocation and initialization).

   DECL is the declaration.  TOP_LEVEL is nonzero
   if this declaration is not within a function.  */

void
rest_of_decl_compilation (tree decl,
			  int top_level,
			  int at_end)
{
  /* We deferred calling assemble_alias so that we could collect
     other attributes such as visibility.  Emit the alias now.  */
  if (!in_lto_p)
  {
    tree alias;
    alias = lookup_attribute ("alias", DECL_ATTRIBUTES (decl));
    if (alias)
      {
	alias = TREE_VALUE (TREE_VALUE (alias));
	alias = get_identifier (TREE_STRING_POINTER (alias));
	assemble_alias (decl, alias);
      }
  }

  /* Can't defer this, because it needs to happen before any
     later function definitions are processed.  */
  if (DECL_ASSEMBLER_NAME_SET_P (decl) && DECL_REGISTER (decl))
    make_decl_rtl (decl);

  /* Forward declarations for nested functions are not "external",
     but we need to treat them as if they were.  */
  if (TREE_STATIC (decl) || DECL_EXTERNAL (decl)
      || TREE_CODE (decl) == FUNCTION_DECL)
    {
      timevar_push (TV_VARCONST);

      /* Don't output anything when a tentative file-scope definition
	 is seen.  But at end of compilation, do output code for them.

	 We do output all variables and rely on
	 callgraph code to defer them except for forward declarations
	 (see gcc.c-torture/compile/920624-1.c) */
      if ((at_end
	   || !DECL_DEFER_OUTPUT (decl)
	   || DECL_INITIAL (decl))
	  && !DECL_EXTERNAL (decl))
	{
	  /* When reading LTO unit, we also read varpool, so do not
	     rebuild it.  */
	  if (in_lto_p && !at_end)
	    ;
	  else if (TREE_CODE (decl) != FUNCTION_DECL)
	    varpool_finalize_decl (decl);
	}

#ifdef ASM_FINISH_DECLARE_OBJECT
      if (decl == last_assemble_variable_decl)
	{
	  ASM_FINISH_DECLARE_OBJECT (asm_out_file, decl,
				     top_level, at_end);
	}
#endif

      timevar_pop (TV_VARCONST);
    }
  else if (TREE_CODE (decl) == TYPE_DECL
	   /* Like in rest_of_type_compilation, avoid confusing the debug
	      information machinery when there are errors.  */
	   && !seen_error ())
    {
      timevar_push (TV_SYMOUT);
      debug_hooks->type_decl (decl, !top_level);
      timevar_pop (TV_SYMOUT);
    }

  /* Let cgraph know about the existence of variables.  */
  if (in_lto_p && !at_end)
    ;
  else if (TREE_CODE (decl) == VAR_DECL && !DECL_EXTERNAL (decl)
	   && TREE_STATIC (decl))
    varpool_node (decl);
}

/* Called after finishing a record, union or enumeral type.  */

void
rest_of_type_compilation (tree type, int toplev)
{
  /* Avoid confusing the debug information machinery when there are
     errors.  */
  if (seen_error ())
    return;

  timevar_push (TV_SYMOUT);
  debug_hooks->type_decl (TYPE_STUB_DECL (type), !toplev);
  timevar_pop (TV_SYMOUT);
}



void
finish_optimization_passes (void)
{
  int i;
  struct dump_file_info *dfi;
  char *name;

  timevar_push (TV_DUMP);    

  /* Do whatever is necessary to finish printing the graphs.  */
  if (graph_dump_format != no_graph)
    for (i = TDI_end; (dfi = get_dump_file_info (i)) != NULL; ++i)
      if (dump_initialized_p (i)
	  && (dfi->flags & TDF_GRAPH) != 0
	  && (name = get_dump_file_name (i)) != NULL)
	{
	  finish_graph_dump_file (name);
	  free (name);
	}

  timevar_pop (TV_DUMP);
}

static bool
gate_rest_of_compilation (void)
{
  /* Early return if there were errors.  We can run afoul of our
     consistency checks, and there's not really much point in fixing them.  */
  return !(rtl_dump_and_exit || flag_syntax_only || seen_error ());
}

struct gimple_opt_pass pass_rest_of_compilation =
{
 {
  GIMPLE_PASS,
  "*rest_of_compilation",               /* name */
  gate_rest_of_compilation,             /* gate */
  NULL,                                 /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_REST_OF_COMPILATION,               /* tv_id */
  PROP_rtl,                             /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_ggc_collect                      /* todo_flags_finish */
 }
};

static bool
gate_postreload (void)
{
  return reload_completed;
}

struct rtl_opt_pass pass_postreload =
{
 {
  RTL_PASS,
  "*all-postreload",                        /* name */
  gate_postreload,                      /* gate */
  NULL,                                 /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_POSTRELOAD,                        /* tv_id */
  PROP_rtl,                             /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_ggc_collect | TODO_verify_rtl_sharing /* todo_flags_finish */
 }
};



/* The root of the compilation pass tree, once constructed.  */
struct opt_pass *all_passes, *all_small_ipa_passes, *all_lowering_passes,
  *all_regular_ipa_passes;

/* This is used by plugins, and should also be used in register_pass.  */
#define DEF_PASS_LIST(LIST) &LIST,
struct opt_pass **gcc_pass_lists[] = { GCC_PASS_LISTS NULL };
#undef DEF_PASS_LIST

/* A map from static pass id to optimization pass.  */
struct opt_pass **passes_by_id;
int passes_by_id_size;

/* Set the static pass number of pass PASS to ID and record that
   in the mapping from static pass number to pass.  */

static void
set_pass_for_id (int id, struct opt_pass *pass)
{
  pass->static_pass_number = id;
  if (passes_by_id_size <= id)
    {
      passes_by_id = XRESIZEVEC (struct opt_pass *, passes_by_id, id + 1);
      memset (passes_by_id + passes_by_id_size, 0,
	      (id + 1 - passes_by_id_size) * sizeof (void *));
      passes_by_id_size = id + 1;
    }
  passes_by_id[id] = pass;
}

/* Return the pass with the static pass number ID.  */

struct opt_pass *
get_pass_for_id (int id)
{
  if (id >= passes_by_id_size)
    return NULL;
  return passes_by_id[id];
}

/* Iterate over the pass tree allocating dump file numbers.  We want
   to do this depth first, and independent of whether the pass is
   enabled or not.  */

void
register_one_dump_file (struct opt_pass *pass)
{
  char *dot_name, *flag_name, *glob_name;
  const char *name, *prefix;
  char num[10];
  int flags, id;

  /* See below in next_pass_1.  */
  num[0] = '\0';
  if (pass->static_pass_number != -1)
    sprintf (num, "%d", ((int) pass->static_pass_number < 0
			 ? 1 : pass->static_pass_number));

  /* The name is both used to identify the pass for the purposes of plugins,
     and to specify dump file name and option.
     The latter two might want something short which is not quite unique; for
     that reason, we may have a disambiguating prefix, followed by a space
     to mark the start of the following dump file name / option string.  */
  name = strchr (pass->name, ' ');
  name = name ? name + 1 : pass->name;
  dot_name = concat (".", name, num, NULL);
  if (pass->type == SIMPLE_IPA_PASS || pass->type == IPA_PASS)
  {
    gcc_unreachable ();
    prefix = "ipa-", flags = TDF_IPA;
  }
  else if (pass->type == GIMPLE_PASS)
    prefix = "tree-", flags = TDF_TREE;
  else
    prefix = "rtl-", flags = TDF_RTL;

  flag_name = concat (prefix, name, num, NULL);
  glob_name = concat (prefix, name, NULL);
  id = dump_register (dot_name, flag_name, glob_name, flags);
  set_pass_for_id (id, pass);
}

/* Recursive worker function for register_dump_files.  */

static int
register_dump_files_1 (struct opt_pass *pass, int properties)
{
  do
    {
      int new_properties = (properties | pass->properties_provided)
			   & ~pass->properties_destroyed;

      if (pass->name && pass->name[0] != '*')
        register_one_dump_file (pass);

      if (pass->sub)
        new_properties = register_dump_files_1 (pass->sub, new_properties);

      /* If we have a gate, combine the properties that we could have with
         and without the pass being examined.  */
      if (pass->gate)
        properties &= new_properties;
      else
        properties = new_properties;

      pass = pass->next;
    }
  while (pass);

  return properties;
}

/* Register the dump files for the pipeline starting at PASS.
   PROPERTIES reflects the properties that are guaranteed to be available at
   the beginning of the pipeline.  */

static void
register_dump_files (struct opt_pass *pass,int properties)
{
  pass->properties_required |= properties;
  register_dump_files_1 (pass, properties);
}

/* Look at the static_pass_number and duplicate the pass
   if it is already added to a list. */

static struct opt_pass *
make_pass_instance (struct opt_pass *pass, bool track_duplicates)
{
  /* A nonzero static_pass_number indicates that the
     pass is already in the list.  */
  if (pass->static_pass_number)
    {
      struct opt_pass *new_pass;

      if (pass->type == GIMPLE_PASS
          || pass->type == RTL_PASS
          || pass->type == SIMPLE_IPA_PASS)
        {
          new_pass = XNEW (struct opt_pass);
          memcpy (new_pass, pass, sizeof (struct opt_pass));
        }
      else if (pass->type == IPA_PASS)
        {
          new_pass = (struct opt_pass *)XNEW (struct ipa_opt_pass_d);
          memcpy (new_pass, pass, sizeof (struct ipa_opt_pass_d));
        }
      else
        gcc_unreachable ();

      new_pass->next = NULL;

      new_pass->todo_flags_start &= ~TODO_mark_first_instance;

      /* Indicate to register_dump_files that this pass has duplicates,
         and so it should rename the dump file.  The first instance will
         be -1, and be number of duplicates = -static_pass_number - 1.
         Subsequent instances will be > 0 and just the duplicate number.  */
      if ((pass->name && pass->name[0] != '*') || track_duplicates)
        {
          pass->static_pass_number -= 1;
          new_pass->static_pass_number = -pass->static_pass_number;
	}
      return new_pass;
    }
  else
    {
      pass->todo_flags_start |= TODO_mark_first_instance;
      pass->static_pass_number = -1;

      invoke_plugin_callbacks (PLUGIN_NEW_PASS, pass);
    }
  return pass;
}

/* Add a pass to the pass list. Duplicate the pass if it's already
   in the list.  */

static struct opt_pass **
next_pass_1 (struct opt_pass **list, struct opt_pass *pass)
{
  /* Every pass should have a name so that plugins can refer to them.  */
  gcc_assert (pass->name != NULL);

  *list = make_pass_instance (pass, false);

  return &(*list)->next;
}

/* List node for an inserted pass instance. We need to keep track of all
   the newly-added pass instances (with 'added_pass_nodes' defined below)
   so that we can register their dump files after pass-positioning is finished.
   Registering dumping files needs to be post-processed or the
   static_pass_number of the opt_pass object would be modified and mess up
   the dump file names of future pass instances to be added.  */

struct pass_list_node
{
  struct opt_pass *pass;
  struct pass_list_node *next;
};

static struct pass_list_node *added_pass_nodes = NULL;
static struct pass_list_node *prev_added_pass_node;

/* Insert the pass at the proper position. Return true if the pass
   is successfully added.

   NEW_PASS_INFO - new pass to be inserted
   PASS_LIST - root of the pass list to insert the new pass to  */

static bool
position_pass (struct register_pass_info *new_pass_info,
               struct opt_pass **pass_list)
{
  struct opt_pass *pass = *pass_list, *prev_pass = NULL;
  bool success = false;

  for ( ; pass; prev_pass = pass, pass = pass->next)
    {
      /* Check if the current pass is of the same type as the new pass and
         matches the name and the instance number of the reference pass.  */
      if (pass->type == new_pass_info->pass->type
          && pass->name
          && !strcmp (pass->name, new_pass_info->reference_pass_name)
          && ((new_pass_info->ref_pass_instance_number == 0)
              || (new_pass_info->ref_pass_instance_number ==
                  pass->static_pass_number)
              || (new_pass_info->ref_pass_instance_number == 1
                  && pass->todo_flags_start & TODO_mark_first_instance)))
        {
          struct opt_pass *new_pass;
          struct pass_list_node *new_pass_node;

	  new_pass = make_pass_instance (new_pass_info->pass, true);

          /* Insert the new pass instance based on the positioning op.  */
          switch (new_pass_info->pos_op)
            {
              case PASS_POS_INSERT_AFTER:
                new_pass->next = pass->next;
                pass->next = new_pass;

		/* Skip newly inserted pass to avoid repeated
		   insertions in the case where the new pass and the
		   existing one have the same name.  */
                pass = new_pass;
                break;
              case PASS_POS_INSERT_BEFORE:
                new_pass->next = pass;
                if (prev_pass)
                  prev_pass->next = new_pass;
                else
                  *pass_list = new_pass;
                break;
              case PASS_POS_REPLACE:
                new_pass->next = pass->next;
                if (prev_pass)
                  prev_pass->next = new_pass;
                else
                  *pass_list = new_pass;
                new_pass->sub = pass->sub;
                new_pass->tv_id = pass->tv_id;
                pass = new_pass;
                break;
              default:
                error ("invalid pass positioning operation");
                return false;
            }

          /* Save the newly added pass (instance) in the added_pass_nodes
             list so that we can register its dump file later. Note that
             we cannot register the dump file now because doing so will modify
             the static_pass_number of the opt_pass object and therefore
             mess up the dump file name of future instances.  */
          new_pass_node = XCNEW (struct pass_list_node);
          new_pass_node->pass = new_pass;
          if (!added_pass_nodes)
            added_pass_nodes = new_pass_node;
          else
            prev_added_pass_node->next = new_pass_node;
          prev_added_pass_node = new_pass_node;

          success = true;
        }

      if (pass->sub && position_pass (new_pass_info, &pass->sub))
        success = true;
    }

  return success;
}

/* Hooks a new pass into the pass lists.

   PASS_INFO   - pass information that specifies the opt_pass object,
                 reference pass, instance number, and how to position
                 the pass  */

void
register_pass (struct register_pass_info *pass_info)
{
  bool all_instances, success;

  /* The checks below could fail in buggy plugins.  Existing GCC
     passes should never fail these checks, so we mention plugin in
     the messages.  */
  if (!pass_info->pass)
      fatal_error ("plugin cannot register a missing pass");

  if (!pass_info->pass->name)
      fatal_error ("plugin cannot register an unnamed pass");

  if (!pass_info->reference_pass_name)
      fatal_error
	("plugin cannot register pass %qs without reference pass name",
	 pass_info->pass->name);

  /* Try to insert the new pass to the pass lists.  We need to check
     all five lists as the reference pass could be in one (or all) of
     them.  */
  all_instances = pass_info->ref_pass_instance_number == 0;
  success = position_pass (pass_info, &all_lowering_passes);
  if (!success || all_instances)
    success |= position_pass (pass_info, &all_passes);
  if (!success)
    fatal_error
      ("pass %qs not found but is referenced by new pass %qs",
       pass_info->reference_pass_name, pass_info->pass->name);

  /* OK, we have successfully inserted the new pass. We need to register
     the dump files for the newly added pass and its duplicates (if any).
     Because the registration of plugin/backend passes happens after the
     command-line options are parsed, the options that specify single
     pass dumping (e.g. -fdump-tree-PASSNAME) cannot be used for new
     passes. Therefore we currently can only enable dumping of
     new passes when the 'dump-all' flags (e.g. -fdump-tree-all)
     are specified. While doing so, we also delete the pass_list_node
     objects created during pass positioning.  */
  while (added_pass_nodes)
    {
      struct pass_list_node *next_node = added_pass_nodes->next;
      enum tree_dump_index tdi;
      register_one_dump_file (added_pass_nodes->pass);
      if (added_pass_nodes->pass->type == SIMPLE_IPA_PASS
          || added_pass_nodes->pass->type == IPA_PASS)
      {
        gcc_unreachable ();
        tdi = TDI_ipa_all;
      }
      else if (added_pass_nodes->pass->type == GIMPLE_PASS)
        tdi = TDI_tree_all;
      else
        tdi = TDI_rtl_all;
      /* Check if dump-all flag is specified.  */
      if (get_dump_file_info (tdi)->state)
        get_dump_file_info (added_pass_nodes->pass->static_pass_number)
            ->state = get_dump_file_info (tdi)->state;
      XDELETE (added_pass_nodes);
      added_pass_nodes = next_node;
    }
}

/* Construct the pass tree.  The sequencing of passes is driven by
   the cgraph routines:

   cgraph_finalize_compilation_unit ()
       for each node N in the cgraph
	   cgraph_analyze_function (N)
	       cgraph_lower_function (N) -> all_lowering_passes

   If we are optimizing, cgraph_optimize is then invoked:

   cgraph_optimize ()
       ipa_passes () 			-> all_small_ipa_passes
       cgraph_expand_all_functions ()
           for each node N in the cgraph
	       cgraph_expand_function (N)
		  tree_rest_of_compilation (DECL (N))  -> all_passes
*/

void
init_optimization_passes (void)
{
  struct opt_pass **p;

#define NEXT_PASS(PASS)  (p = next_pass_1 (p, &((PASS).pass)))

 /* All passes needed to lower the function into shape optimizers can
    operate on.  These passes are always run first on the function, but
    backend might produce already lowered functions that are not processed
    by these passes.  */
  p = &all_lowering_passes;
  NEXT_PASS (pass_lower_cf);
  NEXT_PASS (pass_build_cfg);
  NEXT_PASS (pass_build_cgraph_edges);
  *p = NULL;

  NEXT_PASS (pass_rebuild_cgraph_edges);
  NEXT_PASS (pass_rebuild_cgraph_edges);

  /* These passes are run after IPA passes on every function that is being
     output to the assembler file.  */
  p = &all_passes;

  NEXT_PASS (pass_expand);
  
  NEXT_PASS (pass_rest_of_compilation);
    {
      struct opt_pass **p = &pass_rest_of_compilation.pass.sub;
      NEXT_PASS (pass_init_function);
      NEXT_PASS (pass_rtl_eh);
      NEXT_PASS (pass_initial_value_sets);
      NEXT_PASS (pass_unshare_all_rtl);
      NEXT_PASS (pass_instantiate_virtual_regs);
      NEXT_PASS (pass_into_cfg_layout_mode);
      NEXT_PASS (pass_lower_subreg);
      NEXT_PASS (pass_df_initialize_opt);
      NEXT_PASS (pass_reginfo_init);
      //NEXT_PASS (pass_web);
      //NEXT_PASS (pass_inc_dec);
      NEXT_PASS (pass_initialize_regs);
      NEXT_PASS (pass_outof_cfg_layout_mode);
      NEXT_PASS (pass_split_all_insns);
      NEXT_PASS (pass_lower_subreg2);
      NEXT_PASS (pass_df_initialize_no_opt);
      NEXT_PASS (pass_stack_ptr_mod);
      NEXT_PASS (pass_mode_switching);
      NEXT_PASS (pass_match_asm_constraints);
      NEXT_PASS (pass_ira);
      NEXT_PASS (pass_postreload);
	{
	  struct opt_pass **p = &pass_postreload.pass.sub;
	  NEXT_PASS (pass_split_after_reload);
	  NEXT_PASS (pass_implicit_zee);
	  //NEXT_PASS (pass_branch_target_load_optimize1);
	  //NEXT_PASS (pass_thread_prologue_and_epilogue);
	  NEXT_PASS (pass_regrename);
	  //NEXT_PASS (pass_cprop_hardreg);
	  NEXT_PASS (pass_branch_target_load_optimize2);
	  NEXT_PASS (pass_leaf_regs);
	  NEXT_PASS (pass_stack_regs);
	    {
	      struct opt_pass **p = &pass_stack_regs.pass.sub;
	      NEXT_PASS (pass_stack_regs_run);
	    }
	  NEXT_PASS (pass_compute_alignments);
	  NEXT_PASS (pass_free_cfg);
	  NEXT_PASS (pass_machine_reorg);
	  NEXT_PASS (pass_cleanup_barriers);
	  //NEXT_PASS (pass_delay_slots);
	  NEXT_PASS (pass_split_for_shorten_branches);
	  //NEXT_PASS (pass_convert_to_eh_region_ranges);
	  NEXT_PASS (pass_shorten_branches);
	  NEXT_PASS (pass_set_nothrow_function_flags);
	  NEXT_PASS (pass_final);
	}
      NEXT_PASS (pass_df_finish);
    }
  NEXT_PASS (pass_clean_state);
  *p = NULL;

#undef NEXT_PASS

  /* Register the passes with the tree dump code.  */
  register_dump_files (all_lowering_passes, PROP_gimple_any);
  register_dump_files (all_passes,
		       PROP_gimple_any | PROP_gimple_lcf | PROP_gimple_leh
		       | PROP_cfg);
}

/* If we are in IPA mode (i.e., current_function_decl is NULL), call
   function CALLBACK for every function in the call graph.  Otherwise,
   call CALLBACK on the current function.  */

static void
do_per_function (void (*callback) (void *data), void *data)
{
  if (current_function_decl)
    callback (data);
  else
    {
      struct cgraph_node *node;
      for (node = cgraph_nodes; node; node = node->next)
	if (node->analyzed && gimple_has_body_p (node->decl)
	    && (!node->clone_of || node->decl != node->clone_of->decl))
	  {
	    push_cfun (DECL_STRUCT_FUNCTION (node->decl));
	    current_function_decl = node->decl;
	    callback (data);
	    if (!flag_wpa)
	      {
	        free_dominance_info (CDI_DOMINATORS);
	        free_dominance_info (CDI_POST_DOMINATORS);
	      }
	    current_function_decl = NULL;
	    pop_cfun ();
	    ggc_collect ();
	  }
    }
}

/* Because inlining might remove no-longer reachable nodes, we need to
   keep the array visible to garbage collector to avoid reading collected
   out nodes.  */
static int nnodes;
static GTY ((length ("nnodes"))) cgraph_node_ptr *order;

/* If we are in IPA mode (i.e., current_function_decl is NULL), call
   function CALLBACK for every function in the call graph.  Otherwise,
   call CALLBACK on the current function.
   This function is global so that plugins can use it.  */
void
do_per_function_toporder (void (*callback) (void *data), void *data)
{
  int i;

  if (current_function_decl)
    callback (data);
  else
    {
      gcc_unreachable ();
      gcc_assert (!order);
      order = ggc_alloc_vec_cgraph_node_ptr (cgraph_n_nodes);
      nnodes = cgraph_postorder (order);
      for (i = nnodes - 1; i >= 0; i--)
        order[i]->process = 1;
      for (i = nnodes - 1; i >= 0; i--)
	{
	  struct cgraph_node *node = order[i];

	  /* Allow possibly removed nodes to be garbage collected.  */
	  order[i] = NULL;
	  node->process = 0;
	  if (node->analyzed)
	    {
	      push_cfun (DECL_STRUCT_FUNCTION (node->decl));
	      current_function_decl = node->decl;
	      callback (data);
	      free_dominance_info (CDI_DOMINATORS);
	      free_dominance_info (CDI_POST_DOMINATORS);
	      current_function_decl = NULL;
	      pop_cfun ();
	      ggc_collect ();
	    }
	}
    }
  ggc_free (order);
  order = NULL;
  nnodes = 0;
}

/* Perform all TODO actions that ought to be done on each function.  */

static void
execute_function_todo (void *data)
{
  unsigned int flags = (size_t)data;
  flags &= ~cfun->last_verified;
  if (!flags)
    return;

  cfun->last_verified = flags & TODO_verify_all;
}

/* Perform all TODO actions.  */
static void
execute_todo (unsigned int flags)
{
#if defined ENABLE_CHECKING
  if (cfun
      && need_ssa_update_p (cfun))
    gcc_assert (flags & TODO_update_ssa_any);
#endif

  timevar_push (TV_TODO);

  /* Inform the pass whether it is the first time it is run.  */
  first_pass_instance = (flags & TODO_mark_first_instance) != 0;

  statistics_fini_pass ();

  do_per_function (execute_function_todo, (void *)(size_t) flags);

  if (flags & TODO_ggc_collect)
    ggc_collect ();

  /* Now that the dumping has been done, we can get rid of the optional
     df problems.  */
  if (flags & TODO_df_finish)
    df_finish_pass ((flags & TODO_df_verify) != 0);

  timevar_pop (TV_TODO);
}

/* Verify invariants that should hold between passes.  This is a place
   to put simple sanity checks.  */

static void
verify_interpass_invariants (void)
{
  gcc_checking_assert (!fold_deferring_overflow_warnings_p ());
}

/* Clear the last verified flag.  */

static void
clear_last_verified (void *data ATTRIBUTE_UNUSED)
{
  cfun->last_verified = 0;
}

/* Helper function. Verify that the properties has been turn into the
   properties expected by the pass.  */

#ifdef ENABLE_CHECKING
static void
verify_curr_properties (void *data)
{
  unsigned int required = (size_t)data;
  unsigned int actual = cfun->curr_properties;
  required &= ~PROP_gimple_leh;
  actual &= ~PROP_gimple_leh;
  gcc_assert ((actual & required) == required);
}
#endif

/* Initialize pass dump file.  */
/* This is non-static so that the plugins can use it.  */

bool
pass_init_dump_file (struct opt_pass *pass)
{
  /* If a dump file name is present, open it if enabled.  */
  if (pass->static_pass_number != -1)
    {
      bool initializing_dump = !dump_initialized_p (pass->static_pass_number);
      dump_file_name = get_dump_file_name (pass->static_pass_number);
      dump_file = dump_begin (pass->static_pass_number, &dump_flags);
      if (dump_file && current_function_decl)
	{
	  const char *dname, *aname;
	  struct cgraph_node *node = cgraph_node (current_function_decl);
	  dname = lang_hooks.decl_printable_name (current_function_decl, 2);
	  aname = (IDENTIFIER_POINTER
		   (DECL_ASSEMBLER_NAME (current_function_decl)));
	  fprintf (dump_file, "\n;; Function %s (%s)%s\n\n", dname, aname,
	     node->frequency == NODE_FREQUENCY_HOT
	     ? " (hot)"
	     : node->frequency == NODE_FREQUENCY_UNLIKELY_EXECUTED
	     ? " (unlikely executed)"
	     : node->frequency == NODE_FREQUENCY_EXECUTED_ONCE
	     ? " (executed once)"
	     : "");
	}
      return initializing_dump;
    }
  else
    return false;
}

/* Flush PASS dump file.  */
/* This is non-static so that plugins can use it.  */

void
pass_fini_dump_file (struct opt_pass *pass)
{
  /* Flush and close dump file.  */
  if (dump_file_name)
    {
      free (CONST_CAST (char *, dump_file_name));
      dump_file_name = NULL;
    }

  if (dump_file)
    {
      dump_end (pass->static_pass_number, dump_file);
      dump_file = NULL;
    }
}

/* After executing the pass, apply expected changes to the function
   properties. */

static void
update_properties_after_pass (void *data)
{
  struct opt_pass *pass = (struct opt_pass *) data;
  cfun->curr_properties = (cfun->curr_properties | pass->properties_provided)
		           & ~pass->properties_destroyed;
}

/* Execute summary generation for all of the passes in IPA_PASS.  */

void
execute_ipa_summary_passes (struct ipa_opt_pass_d *ipa_pass)
{
  while (ipa_pass)
    {
      struct opt_pass *pass = &ipa_pass->pass;

      gcc_unreachable ();

      /* Execute all of the IPA_PASSes in the list.  */
      if (ipa_pass->pass.type == IPA_PASS
	  && (!pass->gate || pass->gate ())
	  && ipa_pass->generate_summary)
	{
	  pass_init_dump_file (pass);

	  /* If a timevar is present, start it.  */
	  if (pass->tv_id)
	    timevar_push (pass->tv_id);

	  ipa_pass->generate_summary ();

	  /* Stop timevar.  */
	  if (pass->tv_id)
	    timevar_pop (pass->tv_id);

	  pass_fini_dump_file (pass);
	}
      ipa_pass = (struct ipa_opt_pass_d *)ipa_pass->pass.next;
    }
}

/* Execute IPA_PASS function transform on NODE.  */

static void
execute_one_ipa_transform_pass (struct cgraph_node *node,
				struct ipa_opt_pass_d *ipa_pass)
{
  struct opt_pass *pass = &ipa_pass->pass;
  unsigned int todo_after = 0;

  gcc_unreachable ();

  current_pass = pass;
  if (!ipa_pass->function_transform)
    return;

  /* Note that the folders should only create gimple expressions.
     This is a hack until the new folder is ready.  */
  in_gimple_form = (cfun && (cfun->curr_properties & PROP_trees)) != 0;

  pass_init_dump_file (pass);

  /* Run pre-pass verification.  */
  execute_todo (ipa_pass->function_transform_todo_flags_start);

  /* If a timevar is present, start it.  */
  if (pass->tv_id != TV_NONE)
    timevar_push (pass->tv_id);

  /* Do it!  */
  todo_after = ipa_pass->function_transform (node);

  /* Stop timevar.  */
  if (pass->tv_id != TV_NONE)
    timevar_pop (pass->tv_id);

  /* Run post-pass cleanup and verification.  */
  execute_todo (todo_after);
  verify_interpass_invariants ();

  pass_fini_dump_file (pass);

  current_pass = NULL;
}

/* For the current function, execute all ipa transforms. */

void
execute_all_ipa_transforms (void)
{
  struct cgraph_node *node;
  gcc_unreachable ();
  if (!cfun)
    return;
  node = cgraph_node (current_function_decl);

  if (node->ipa_transforms_to_apply)
    {
      unsigned int i;

      for (i = 0; i < VEC_length (ipa_opt_pass, node->ipa_transforms_to_apply);
	   i++)
	execute_one_ipa_transform_pass (node,
					VEC_index (ipa_opt_pass,
						   node->ipa_transforms_to_apply,
						   i));
      VEC_free (ipa_opt_pass, heap, node->ipa_transforms_to_apply);
      node->ipa_transforms_to_apply = NULL;
    }
}

/* Execute PASS. */

bool
execute_one_pass (struct opt_pass *pass)
{
  bool initializing_dump;
  unsigned int todo_after = 0;

  bool gate_status;
  
  /* printf("%s\n", pass->name); */

  /* IPA passes are executed on whole program, so cfun should be NULL.
     Other passes need function context set.  */
  if (pass->type == SIMPLE_IPA_PASS || pass->type == IPA_PASS)
  {
    gcc_assert (!cfun && !current_function_decl);
    gcc_unreachable (); /* IPA passes removed */
  }
  else
    gcc_assert (cfun && current_function_decl);

  current_pass = pass;

  /* Check whether gate check should be avoided.
     User controls the value of the gate through the parameter "gate_status". */
  gate_status = (pass->gate == NULL) ? true : pass->gate();

  /* Override gate with plugin.  */
  invoke_plugin_callbacks (PLUGIN_OVERRIDE_GATE, &gate_status);

  if (!gate_status)
    {
      current_pass = NULL;
      return false;
    }

  /* Pass execution event trigger: useful to identify passes being
     executed.  */
  invoke_plugin_callbacks (PLUGIN_PASS_EXECUTION, pass);

  if (!quiet_flag && !cfun)
    fprintf (stderr, " <%s>", pass->name ? pass->name : "");

  /* Note that the folders should only create gimple expressions.
     This is a hack until the new folder is ready.  */
  in_gimple_form = (cfun && (cfun->curr_properties & PROP_trees)) != 0;

  initializing_dump = pass_init_dump_file (pass);

  /* Run pre-pass verification.  */
  execute_todo (pass->todo_flags_start);

#ifdef ENABLE_CHECKING
  do_per_function (verify_curr_properties,
		   (void *)(size_t)pass->properties_required);
#endif

  /* If a timevar is present, start it.  */
  if (pass->tv_id != TV_NONE)
    timevar_push (pass->tv_id);

  /* Do it!  */
  if (pass->execute)
    {
      todo_after = pass->execute ();
      do_per_function (clear_last_verified, NULL);
    }

  /* Stop timevar.  */
  if (pass->tv_id != TV_NONE)
    timevar_pop (pass->tv_id);

  do_per_function (update_properties_after_pass, pass);

  if (initializing_dump
      && dump_file
      && graph_dump_format != no_graph
      && cfun
      && (cfun->curr_properties & (PROP_cfg | PROP_rtl))
	  == (PROP_cfg | PROP_rtl))
    {
      get_dump_file_info (pass->static_pass_number)->flags |= TDF_GRAPH;
      dump_flags |= TDF_GRAPH;
      clean_graph_dump_file (dump_file_name);
    }

  /* Run post-pass cleanup and verification.  */
  execute_todo (todo_after | pass->todo_flags_finish);
  verify_interpass_invariants ();
  if (pass->type == IPA_PASS)
    {
      struct cgraph_node *node;
      gcc_unreachable ();
      for (node = cgraph_nodes; node; node = node->next)
        if (node->analyzed)
          VEC_safe_push (ipa_opt_pass, heap, node->ipa_transforms_to_apply,
			 (struct ipa_opt_pass_d *)pass);
    }

  if (!current_function_decl)
    cgraph_process_new_functions ();

  pass_fini_dump_file (pass);

  if (pass->type != SIMPLE_IPA_PASS && pass->type != IPA_PASS)
    gcc_assert (!(cfun->curr_properties & PROP_trees)
		|| pass->type != RTL_PASS);

  current_pass = NULL;

  return true;
}

void
execute_pass_list (struct opt_pass *pass)
{
  do
    {
      gcc_assert (pass->type == GIMPLE_PASS
		  || pass->type == RTL_PASS);
      if (execute_one_pass (pass) && pass->sub)
      {
        execute_pass_list (pass->sub);
      }  
      pass = pass->next;
    }
  while (pass);
}

/* Called by local passes to see if function is called by already processed nodes.
   Because we process nodes in topological order, this means that function is
   in recursive cycle or we introduced new direct calls.  */
bool
function_called_by_processed_nodes_p (void)
{
  struct cgraph_edge *e;
  for (e = cgraph_node (current_function_decl)->callers; e; e = e->next_caller)
    {
      if (e->caller->decl == current_function_decl)
        continue;
      if (!e->caller->analyzed)
        continue;
      if (TREE_ASM_WRITTEN (e->caller->decl))
        continue;
      if (!e->caller->process && !e->caller->global.inlined_to)
      	break;
    }
  if (dump_file && e)
    {
      fprintf (dump_file, "Already processed call to:\n");
      dump_cgraph_node (dump_file, e->caller);
    }
  return e != NULL;
}

EXTERN_C_END

#include "gt-passes.h"
