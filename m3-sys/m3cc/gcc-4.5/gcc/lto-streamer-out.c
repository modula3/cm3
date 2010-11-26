/* Modula-3: modified */

/* Write the GIMPLE representation to a file stream.

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
#include "except.h"
#include "vec.h"
#include "lto-symtab.h"
#include "lto-streamer.h"

#ifdef __cplusplus
extern "C" {
#endif

struct output_block *
create_output_block (enum lto_section_type)
{
  gcc_unreachable ();
  return 0;
}

void
destroy_output_block (struct output_block *)
{
  gcc_unreachable ();
}

void
lto_output_bitpack (struct lto_output_stream *, struct bitpack_d *)
{
  gcc_unreachable ();
}


void
lto_output_tree (struct output_block *, tree, bool)
{
  gcc_unreachable ();
}

void
produce_asm (struct output_block *, tree)
{
  gcc_unreachable ();
}

static void
lto_output (cgraph_node_set)
{
  gcc_unreachable ();
}

struct ipa_opt_pass_d pass_ipa_lto_gimple_out =
{
 {
  IPA_PASS,
  "lto_gimple_out",	                /* name */
  gate_lto_out,			        /* gate */
  NULL,		                	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_IPA_LTO_GIMPLE_IO,		        /* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,            			/* todo_flags_start */
  TODO_dump_func                        /* todo_flags_finish */
 },
 NULL,		                        /* generate_summary */
 lto_output,           			/* write_summary */
 NULL,		         		/* read_summary */
 NULL,					/* function_read_summary */
 NULL,					/* stmt_fixup */
 0,					/* TODOs */
 NULL,			                /* function_transform */
 NULL					/* variable_transform */
};

static void
produce_asm_for_decls (cgraph_node_set)
{
  gcc_unreachable ();
}

struct ipa_opt_pass_d pass_ipa_lto_finish_out =
{
 {
  IPA_PASS,
  "lto_decls_out",	                /* name */
  gate_lto_out,			        /* gate */
  NULL,        	                        /* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_IPA_LTO_DECL_IO,		        /* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,            			/* todo_flags_start */
  0                                     /* todo_flags_finish */
 },
 NULL,		                        /* generate_summary */
 produce_asm_for_decls,			/* write_summary */
 NULL,		         		/* read_summary */
 NULL,					/* function_read_summary */
 NULL,					/* stmt_fixup */
 0,					/* TODOs */
 NULL,			                /* function_transform */
 NULL					/* variable_transform */
};

#ifdef __cplusplus
} /* extern "C" */
#endif
