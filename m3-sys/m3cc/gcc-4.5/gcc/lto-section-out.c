/* Modula-3: modified */

/* Functions for writing LTO sections.

   Copyright (C) 2009 Free Software Foundation, Inc.
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
#include "toplev.h"
#include "tree.h"
#include "expr.h"
#include "params.h"
#include "input.h"
#include "varray.h"
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
#include "lto-streamer.h"
#include "lto-compress.h"

#ifdef __cplusplus
extern "C" {
#endif

void
lto_new_extern_inline_states (void)
{
  gcc_unreachable ();
}

void
lto_delete_extern_inline_states (void)
{
  gcc_unreachable ();
}

void
lto_force_functions_extern_inline (bitmap )
{
  gcc_unreachable ();
}

bool
lto_forced_extern_inline_p (tree )
{
  gcc_unreachable ();
  return 0;
}

hashval_t
lto_hash_decl_slot_node (const void *)
{
  gcc_unreachable ();
  return 0;
}

int
lto_eq_decl_slot_node (const void *, const void *)
{
  gcc_unreachable ();
  return 0;
}

hashval_t
lto_hash_type_slot_node (const void *)
{
  gcc_unreachable ();
  return 0;
}

int
lto_eq_type_slot_node (const void *, const void *)
{
  gcc_unreachable ();
  return 0;
}

void
lto_begin_section (const char *, bool)
{
  gcc_unreachable ();
}

void
lto_end_section (void)
{
  gcc_unreachable ();
}

void
lto_write_stream (struct lto_output_stream *)
{
  gcc_unreachable ();
}

void
lto_output_1_stream (struct lto_output_stream *, char)
{
}

void
lto_output_data_stream (struct lto_output_stream *, const void *,
			size_t )
{
}

void
lto_output_uleb128_stream (struct lto_output_stream *,
			   unsigned HOST_WIDE_INT )
{
}

void
lto_output_widest_uint_uleb128_stream (struct lto_output_stream *,
				       unsigned HOST_WIDEST_INT )
{
  gcc_unreachable ();
}

void
lto_output_sleb128_stream (struct lto_output_stream *, HOST_WIDE_INT )
{
  gcc_unreachable ();
}

bool
lto_output_decl_index (struct lto_output_stream *,
		       struct lto_tree_ref_encoder *,
		       tree , unsigned int *)
{
  gcc_unreachable ();
  return 0;
}

void
lto_output_field_decl_index (struct lto_out_decl_state *,
			     struct lto_output_stream * , tree )
{
  gcc_unreachable ();
}

void
lto_output_fn_decl_index (struct lto_out_decl_state *,
			  struct lto_output_stream * , tree )
{
  gcc_unreachable ();
}

void
lto_output_namespace_decl_index (struct lto_out_decl_state *,
				 struct lto_output_stream *, tree)
{
  gcc_unreachable ();
}

void
lto_output_var_decl_index (struct lto_out_decl_state *,
			   struct lto_output_stream * , tree )
{
  gcc_unreachable ();
}

void
lto_output_type_decl_index (struct lto_out_decl_state *,
			    struct lto_output_stream *, tree)
{
  gcc_unreachable ();
}

void
lto_output_type_ref_index (struct lto_out_decl_state *,
			   struct lto_output_stream *, tree)
{
  gcc_unreachable ();
}

struct lto_simple_output_block *
lto_create_simple_output_block (enum lto_section_type)
{
  gcc_unreachable ();
  return 0;
}

void
lto_destroy_simple_output_block (struct lto_simple_output_block *)
{
  gcc_unreachable ();
}

struct lto_out_decl_state *
lto_new_out_decl_state (void)
{
  gcc_unreachable ();
  return 0;
}

void
lto_delete_out_decl_state (struct lto_out_decl_state *)
{
  gcc_unreachable ();
}

struct lto_out_decl_state *
lto_get_out_decl_state (void)
{
  gcc_unreachable ();
  return 0;
}

void
lto_push_out_decl_state (struct lto_out_decl_state *)
{
  gcc_unreachable ();
}

struct lto_out_decl_state *
lto_pop_out_decl_state (void)
{
  gcc_unreachable ();
  return 0;
}

void
lto_record_function_out_decl_state (tree,
				    struct lto_out_decl_state *)
{
  gcc_unreachable ();
}

#ifdef __cplusplus
} /* extern "C" */
#endif
