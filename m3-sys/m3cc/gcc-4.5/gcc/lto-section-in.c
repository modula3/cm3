/* Modula-3: modified */

/* Input functions for reading LTO sections.

   Copyright 2009 Free Software Foundation, Inc.
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
#include "flags.h"
#include "params.h"
#include "input.h"
#include "varray.h"
#include "hashtab.h"
#include "basic-block.h"
#include "tree-flow.h"
#include "cgraph.h"
#include "function.h"
#include "ggc.h"
#include "diagnostic.h"
#include "except.h"
#include "vec.h"
#include "timevar.h"
#include "output.h"
#include "lto-streamer.h"
#include "lto-compress.h"

#ifdef __cplusplus
extern "C" {
#endif

unsigned char
lto_input_1_unsigned (struct lto_input_block *ib)
{
  gcc_unreachable ();
  return 0;
}

unsigned HOST_WIDE_INT
lto_input_uleb128 (struct lto_input_block *ib)
{
  gcc_unreachable ();
  return 0;
}

unsigned HOST_WIDEST_INT
lto_input_widest_uint_uleb128 (struct lto_input_block *ib)
{
  gcc_unreachable ();
  return 0;
}

HOST_WIDE_INT
lto_input_sleb128 (struct lto_input_block *ib)
{
  gcc_unreachable ();
  return 0;
}

void
lto_set_in_hooks (struct lto_file_decl_data ** data,
		  lto_get_section_data_f* get_f,
		  lto_free_section_data_f* free_f)
{
  gcc_unreachable ();
}

struct lto_file_decl_data **
lto_get_file_decl_data (void)
{
  gcc_unreachable ();
  return 0;
}

const char *
lto_get_section_data (struct lto_file_decl_data *file_data,
		      enum lto_section_type section_type,
		      const char *name,
		      size_t *len)
{
  gcc_unreachable ();
  return 0;
}

void
lto_free_section_data (struct lto_file_decl_data *file_data,
		       enum lto_section_type section_type,
		       const char *name,
		       const char *data,
		       size_t len)
{
  gcc_unreachable ();
}

struct lto_input_block *
lto_create_simple_input_block (struct lto_file_decl_data *file_data,
			       enum lto_section_type section_type,
			       const char **datar, size_t *len)
{
  gcc_unreachable ();
  return 0;
}

void
lto_destroy_simple_input_block (struct lto_file_decl_data *file_data,
				enum lto_section_type section_type,
				struct lto_input_block *ib,
				const char *data, size_t len)
{
  gcc_unreachable ();
}

htab_t
lto_create_renaming_table (void)
{
  gcc_unreachable ();
  return 0;
}

void
lto_record_renamed_decl (struct lto_file_decl_data *decl_data,
			 const char *old_name, const char *new_name)
{
  gcc_unreachable ();
}

const char *
lto_get_decl_name_mapping (struct lto_file_decl_data *decl_data,
			   const char *name)
{
  gcc_unreachable ();
  return 0;
}

struct lto_in_decl_state *
lto_new_in_decl_state (void)
{
  gcc_unreachable ();
  return 0;
}

void
lto_delete_in_decl_state (struct lto_in_decl_state *state)
{
  gcc_unreachable ();
}

hashval_t
lto_hash_in_decl_state (const void *p)
{
  gcc_unreachable ();
  return 0;
}

int
lto_eq_in_decl_state (const void *p1, const void *p2)
{
  gcc_unreachable ();
  return 0;
}

struct lto_in_decl_state*
lto_get_function_in_decl_state (struct lto_file_decl_data *file_data,
				tree func)
{
  gcc_unreachable ();
  return 0;
}

#ifdef __cplusplus
} /* extern "C" */
#endif
