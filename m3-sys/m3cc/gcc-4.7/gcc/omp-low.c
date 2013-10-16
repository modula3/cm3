/* Modula-3: modified */

/* Lowering pass for OpenMP directives.  Converts OpenMP directives
   into explicit calls to the runtime library (libgomp) and data
   marshalling to implement data sharing and copying clauses.
   Contributed by Diego Novillo <dnovillo@redhat.com>

   Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "gimple.h"
#include "tree-iterator.h"
#include "tree-inline.h"
#include "langhooks.h"
#include "diagnostic-core.h"
#include "tree-flow.h"
#include "timevar.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "tree-pass.h"
#include "ggc.h"
#include "except.h"
#include "splay-tree.h"
#include "optabs.h"
#include "cfgloop.h"

/* Create a new VAR_DECL and copy information from VAR to it.  */

tree
copy_var_decl (tree var, tree name, tree type)
{
  tree copy = build_decl (DECL_SOURCE_LOCATION (var), VAR_DECL, name, type);

  TREE_ADDRESSABLE (copy) = TREE_ADDRESSABLE (var);
  TREE_THIS_VOLATILE (copy) = TREE_THIS_VOLATILE (var);
  DECL_GIMPLE_REG_P (copy) = DECL_GIMPLE_REG_P (var);
  DECL_ARTIFICIAL (copy) = DECL_ARTIFICIAL (var);
  DECL_IGNORED_P (copy) = DECL_IGNORED_P (var);
  DECL_CONTEXT (copy) = DECL_CONTEXT (var);
  TREE_USED (copy) = 1;
  DECL_SEEN_IN_BIND_EXPR_P (copy) = 1;

  return copy;
}
