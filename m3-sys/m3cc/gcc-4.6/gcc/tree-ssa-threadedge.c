/* Modula-3: modified */

/* SSA Jump Threading
   Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Jeff Law  <law@redhat.com>

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
#include "flags.h"
#include "tm_p.h"
#include "basic-block.h"
#include "cfgloop.h"
#include "output.h"
#include "function.h"
#include "timevar.h"
#include "tree-dump.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"
#include "langhooks.h"
#include "params.h"

EXTERN_C_START

/* To avoid code explosion due to jump threading, we limit the
   number of statements we are going to copy.  This variable
   holds the number of statements currently seen that we'll have
   to copy as part of the jump threading process.  */
static int stmt_count;

/* Array to record value-handles per SSA_NAME.  */
VEC(tree,heap) *ssa_name_values;

/* Set the value for the SSA name NAME to VALUE.  */

void
set_ssa_name_value (tree name, tree value)
{
  if (SSA_NAME_VERSION (name) >= VEC_length (tree, ssa_name_values))
    VEC_safe_grow_cleared (tree, heap, ssa_name_values,
			   SSA_NAME_VERSION (name) + 1);
  VEC_replace (tree, ssa_name_values, SSA_NAME_VERSION (name), value);
}

EXTERN_C_END
