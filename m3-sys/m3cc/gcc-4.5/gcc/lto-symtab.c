/* Modula-3: modified */

/* LTO symbol table.
   Copyright 2009 Free Software Foundation, Inc.
   Contributed by CodeSourcery, Inc.

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
#include "toplev.h"
#include "tree.h"
#include "gimple.h"
#include "ggc.h"	/* lambda.h needs this */
#include "lambda.h"	/* gcd */
#include "hashtab.h"
#include "plugin-api.h"
#include "lto-streamer.h"

#ifdef __cplusplus
extern "C" {
#endif

void
lto_symtab_register_decl (tree,
			  ld_plugin_symbol_resolution_t,
			  struct lto_file_decl_data *)
{
  gcc_unreachable ();
}

enum ld_plugin_symbol_resolution
lto_symtab_get_resolution (tree)
{
  gcc_unreachable ();
  return (enum ld_plugin_symbol_resolution)0;
}

void
lto_symtab_merge_decls (void)
{
  gcc_unreachable ();
}

void
lto_symtab_merge_cgraph_nodes (void)
{
  gcc_unreachable ();
}

tree
lto_symtab_prevailing_decl (tree)
{
  gcc_unreachable ();
  return 0;
}

#ifdef __cplusplus
} /* extern "C" */
#endif
