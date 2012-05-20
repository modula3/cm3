/* Modula-3: modified */

/* SSA Dominator optimizations for trees
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

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
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "timevar.h"
#include "tree-dump.h"
#include "tree-flow.h"
#include "domwalk.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"
#include "langhooks.h"
#include "params.h"

EXTERN_C_START

/* PHI-ONLY copy and constant propagation.  This pass is meant to clean
   up degenerate PHIs created by or exposed by jump threading.  */

/* Given PHI, return its RHS if the PHI is a degenerate, otherwise return
   NULL.  */

tree
degenerate_phi_result (gimple phi)
{
  tree lhs = gimple_phi_result (phi);
  tree val = NULL;
  size_t i;

  /* Ignoring arguments which are the same as LHS, if all the remaining
     arguments are the same, then the PHI is a degenerate and has the
     value of that common argument.  */
  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree arg = gimple_phi_arg_def (phi, i);

      if (arg == lhs)
	continue;
      else if (!arg)
	break;
      else if (!val)
	val = arg;
      else if (arg == val)
	continue;
      /* We bring in some of operand_equal_p not only to speed things
	 up, but also to avoid crashing when dereferencing the type of
	 a released SSA name.  */
      else if (TREE_CODE (val) != TREE_CODE (arg)
	       || TREE_CODE (val) == SSA_NAME
	       || !operand_equal_p (arg, val, 0))
	break;
    }
  return (i == gimple_phi_num_args (phi) ? val : NULL);
}

EXTERN_C_END
