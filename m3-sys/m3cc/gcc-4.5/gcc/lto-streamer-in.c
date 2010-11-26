/* Modula-3: modified */

/* Read the GIMPLE representation from a file stream.

   Copyright 2009, 2010 Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>
   Re-implemented by Diego Novillo <dnovillo@google.com>

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
#include "tree-pass.h"
#include "cgraph.h"
#include "function.h"
#include "ggc.h"
#include "diagnostic.h"
#include "libfuncs.h"
#include "except.h"
#include "debug.h"
#include "vec.h"
#include "timevar.h"
#include "output.h"
#include "ipa-utils.h"
#include "lto-streamer.h"
#include "tree-pass.h"

#ifdef __cplusplus
extern "C" {
#endif

void
lto_input_function_body (struct lto_file_decl_data *,
			 tree, const char *)
{
  gcc_unreachable ();
}

void
lto_input_constructors_and_inits (struct lto_file_decl_data *,
				  const char *)
{
  gcc_unreachable ();
}

struct bitpack_d *
lto_input_bitpack (struct lto_input_block *)
{
  gcc_unreachable ();
  return 0;
}

tree
lto_input_tree (struct lto_input_block *, struct data_in *)
{
  gcc_unreachable ();
  return 0;
}

void
lto_init_reader (void)
{
  gcc_unreachable ();
}

struct data_in *
lto_data_in_create (struct lto_file_decl_data *, const char *,
		    unsigned,
		    VEC(ld_plugin_symbol_resolution_t,heap) *)
{
  gcc_unreachable ();
  return 0;
}

void
lto_data_in_delete (struct data_in *)
{
  gcc_unreachable ();
}

#ifdef __cplusplus
} /* extern "C" */
#endif
