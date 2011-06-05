/* Modula-3: modified */

/* Callgraph based analysis of static variables.
   Copyright (C) 2004, 2005, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>

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

/* This file gathers information about how variables whose scope is
   confined to the compilation unit are used.

   The transitive call site specific clobber effects are computed
   for the variables whose scope is contained within this compilation
   unit.

   First each function and static variable initialization is analyzed
   to determine which local static variables are either read, written,
   or have their address taken.  Any local static that has its address
   taken is removed from consideration.  Once the local read and
   writes are determined, a transitive closure of this information is
   performed over the call graph to determine the worst case set of
   side effects of each call.  In later parts of the compiler, these
   local and global sets are examined to make the call clobbering less
   traumatic, promote some statics to registers, and improve aliasing
   information.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tree-flow.h"
#include "tree-inline.h"
#include "tree-pass.h"
#include "langhooks.h"
#include "pointer-set.h"
#include "splay-tree.h"
#include "ggc.h"
#include "ipa-utils.h"
#include "ipa-reference.h"
#include "gimple.h"
#include "cgraph.h"
#include "output.h"
#include "flags.h"
#include "timevar.h"
#include "diagnostic.h"
#include "langhooks.h"

static void remove_node_data (struct cgraph_node *node,
			      void *data ATTRIBUTE_UNUSED);
static void duplicate_node_data (struct cgraph_node *src,
				 struct cgraph_node *dst,
				 void *data ATTRIBUTE_UNUSED);

/* The static variables defined within the compilation unit that are
   loaded or stored directly by function that owns this structure.  */

struct ipa_reference_local_vars_info_d
{
  bitmap statics_read;
  bitmap statics_written;
};

/* Statics that are read and written by some set of functions. The
   local ones are based on the loads and stores local to the function.
   The global ones are based on the local info as well as the
   transitive closure of the functions that are called. */

struct ipa_reference_global_vars_info_d
{
  bitmap statics_read;
  bitmap statics_written;
};

/* Information we save about every function after ipa-reference is completed.  */

struct ipa_reference_optimization_summary_d
{
  bitmap statics_not_read;
  bitmap statics_not_written;
};

typedef struct ipa_reference_local_vars_info_d *ipa_reference_local_vars_info_t;
typedef struct ipa_reference_global_vars_info_d *ipa_reference_global_vars_info_t;
typedef struct ipa_reference_optimization_summary_d *ipa_reference_optimization_summary_t;

struct ipa_reference_vars_info_d
{
  struct ipa_reference_local_vars_info_d local;
  struct ipa_reference_global_vars_info_d global;
};

typedef struct ipa_reference_vars_info_d *ipa_reference_vars_info_t;

/* This splay tree contains all of the static variables that are
   being considered by the compilation level alias analysis.  */
static splay_tree reference_vars_to_consider;

/* A bit is set for every module static we are considering.  This is
   ored into the local info when asm code is found that clobbers all
   memory. */
static bitmap all_module_statics;

/* Obstack holding bitmaps of local analysis (live from analysis to
   propagation)  */
static bitmap_obstack local_info_obstack;
/* Obstack holding global analysis live forever.  */
static bitmap_obstack optimization_summary_obstack;

/* Holders of ipa cgraph hooks: */
static struct cgraph_2node_hook_list *node_duplication_hook_holder;
static struct cgraph_node_hook_list *node_removal_hook_holder;

/* Vector where the reference var infos are actually stored. */
DEF_VEC_P (ipa_reference_vars_info_t);
DEF_VEC_ALLOC_P (ipa_reference_vars_info_t, heap);
static VEC (ipa_reference_vars_info_t, heap) *ipa_reference_vars_vector;
DEF_VEC_P (ipa_reference_optimization_summary_t);
DEF_VEC_ALLOC_P (ipa_reference_optimization_summary_t, heap);
static VEC (ipa_reference_optimization_summary_t, heap) *ipa_reference_opt_sum_vector;

/* Return the ipa_reference_vars structure starting from the cgraph NODE.  */
static inline ipa_reference_vars_info_t
get_reference_vars_info (struct cgraph_node *node)
{
  if (!ipa_reference_vars_vector
      || VEC_length (ipa_reference_vars_info_t,
		     ipa_reference_vars_vector) <= (unsigned int) node->uid)
    return NULL;
  return VEC_index (ipa_reference_vars_info_t, ipa_reference_vars_vector,
		    node->uid);
}

/* Return the ipa_reference_vars structure starting from the cgraph NODE.  */
static inline ipa_reference_optimization_summary_t
get_reference_optimization_summary (struct cgraph_node *node)
{
  if (!ipa_reference_opt_sum_vector
      || (VEC_length (ipa_reference_optimization_summary_t,
		      ipa_reference_opt_sum_vector)
	  <= (unsigned int) node->uid))
    return NULL;
  return VEC_index (ipa_reference_optimization_summary_t, ipa_reference_opt_sum_vector,
		    node->uid);
}

/* Return the ipa_reference_vars structure starting from the cgraph NODE.  */
static inline void
set_reference_vars_info (struct cgraph_node *node,
			 ipa_reference_vars_info_t info)
{
  if (!ipa_reference_vars_vector
      || VEC_length (ipa_reference_vars_info_t,
		     ipa_reference_vars_vector) <= (unsigned int) node->uid)
    VEC_safe_grow_cleared (ipa_reference_vars_info_t, heap,
			   ipa_reference_vars_vector, node->uid + 1);
  VEC_replace (ipa_reference_vars_info_t, ipa_reference_vars_vector,
	       node->uid, info);
}

/* Return the ipa_reference_vars structure starting from the cgraph NODE.  */
static inline void
set_reference_optimization_summary (struct cgraph_node *node,
				    ipa_reference_optimization_summary_t info)
{
  if (!ipa_reference_opt_sum_vector
      || (VEC_length (ipa_reference_optimization_summary_t,
		      ipa_reference_opt_sum_vector)
	  <= (unsigned int) node->uid))
    VEC_safe_grow_cleared (ipa_reference_optimization_summary_t,
			   heap, ipa_reference_opt_sum_vector, node->uid + 1);
  VEC_replace (ipa_reference_optimization_summary_t,
	       ipa_reference_opt_sum_vector, node->uid, info);
}

/* Return a bitmap indexed by_DECL_UID uid for the static variables
   that are not read during the execution of the function FN.  Returns
   NULL if no data is available.  */

bitmap
ipa_reference_get_not_read_global (struct cgraph_node *fn)
{
  ipa_reference_optimization_summary_t info;

  info = get_reference_optimization_summary (fn);
  if (info)
    return info->statics_not_read;
  else if (flags_from_decl_or_type (fn->decl) & ECF_LEAF)
    return all_module_statics;
  else
    return NULL;
}

/* Return a bitmap indexed by DECL_UID uid for the static variables
   that are not written during the execution of the function FN.  Note
   that variables written may or may not be read during the function
   call.  Returns NULL if no data is available.  */

bitmap
ipa_reference_get_not_written_global (struct cgraph_node *fn)
{
  ipa_reference_optimization_summary_t info;

  info = get_reference_optimization_summary (fn);
  if (info)
    return info->statics_not_written;
  else if (flags_from_decl_or_type (fn->decl) & ECF_LEAF)
    return all_module_statics;
  else
    return NULL;
}

/* Serialize the ipa info for lto.  */

static void
ipa_reference_write_optimization_summary (cgraph_node_set set,
					  varpool_node_set vset)
{
}

/* Deserialize the ipa info for lto.  */

static void
ipa_reference_read_optimization_summary (void)
{
}

static bool
gate_reference (void)
{
  return false;
}

struct ipa_opt_pass_d pass_ipa_reference =
{
 {
  IPA_PASS,
  "static-var",				/* name */
  gate_reference,			/* gate */
  propagate,			        /* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_IPA_REFERENCE,		        /* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0                                     /* todo_flags_finish */
 },
 NULL,				        /* generate_summary */
 NULL,					/* write_summary */
 NULL,				 	/* read_summary */
 ipa_reference_write_optimization_summary,/* write_optimization_summary */
 ipa_reference_read_optimization_summary,/* read_optimization_summary */
 NULL,					/* stmt_fixup */
 0,					/* TODOs */
 NULL,			                /* function_transform */
 NULL					/* variable_transform */
};
