/* Modula-3: modified */

/* Functions for writing LTO sections.

   Copyright (C) 2009, 2010 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "expr.h"
#include "params.h"
#include "input.h"
#include "hashtab.h"
#include "basic-block.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "function.h"
#include "ggc.h"
#include "except.h"
#include "vec.h"
#include "pointer-set.h"
#include "bitmap.h"
#include "langhooks.h"
#include "data-streamer.h"
#include "lto-streamer.h"
#include "lto-compress.h"

VEC(lto_out_decl_state_ptr, heap) *lto_function_decl_states;

hashval_t
lto_hash_decl_slot_node (const void *p)
{
  return 0;
}

int
lto_eq_decl_slot_node (const void *p1, const void *p2)
{
  return 0;
}

hashval_t
lto_hash_type_slot_node (const void *p)
{
  return 0;
}

int
lto_eq_type_slot_node (const void *p1, const void *p2)
{
  return 0;
}

void
lto_begin_section (const char *name, bool compress)
{
  gcc_unreachable ();
}

void
lto_end_section (void)
{ }

void
lto_write_stream (struct lto_output_stream *obs)
{ }

void
lto_append_block (struct lto_output_stream *obs)
{ }

void
lto_output_data_stream (struct lto_output_stream *obs, const void *data,
			size_t len)
{ }

bool
lto_output_decl_index (struct lto_output_stream *obs,
		       struct lto_tree_ref_encoder *encoder,
		       tree name, unsigned int *this_index)
{
  return 0;
}

void
lto_output_field_decl_index (struct lto_out_decl_state *decl_state,
			     struct lto_output_stream * obs, tree decl)
{
}

void
lto_output_fn_decl_index (struct lto_out_decl_state *decl_state,
			  struct lto_output_stream * obs, tree decl)
{
}

void
lto_output_namespace_decl_index (struct lto_out_decl_state *decl_state,
				 struct lto_output_stream * obs, tree decl)
{
}

void
lto_output_var_decl_index (struct lto_out_decl_state *decl_state,
			   struct lto_output_stream * obs, tree decl)
{
}

void
lto_output_type_decl_index (struct lto_out_decl_state *decl_state,
			    struct lto_output_stream * obs, tree decl)
{
}

void
lto_output_type_ref_index (struct lto_out_decl_state *decl_state,
			   struct lto_output_stream *obs, tree ref)
{
}

struct lto_simple_output_block *
lto_create_simple_output_block (enum lto_section_type section_type)
{
  return 0;
}


/* Produce a simple section for one of the ipa passes.  */

void
lto_destroy_simple_output_block (struct lto_simple_output_block *ob)
{ }


/* Return a new lto_out_decl_state. */

struct lto_out_decl_state *
lto_new_out_decl_state (void)
{
  return 0;
}


/* Delete STATE and components.  */

void
lto_delete_out_decl_state (struct lto_out_decl_state *state)
{
}


/* Get the currently used lto_out_decl_state structure. */

struct lto_out_decl_state *
lto_get_out_decl_state (void)
{
  return 0;
}

/* Push STATE to top of out decl stack. */

void
lto_push_out_decl_state (struct lto_out_decl_state *state)
{
}

/* Pop the currently used out-decl state from top of stack. */

struct lto_out_decl_state *
lto_pop_out_decl_state (void)
{
  return 0;
}

void
lto_record_function_out_decl_state (tree fn_decl,
				    struct lto_out_decl_state *state)
{ }
