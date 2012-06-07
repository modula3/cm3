/* Modula-3: modified */

/* Copy propagation and SSA_NAME replacement support routines.
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2010
   Free Software Foundation, Inc.

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
#include "output.h"
#include "function.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "timevar.h"
#include "tree-dump.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"
#include "langhooks.h"
#include "cfgloop.h"

EXTERN_C_START

/* This file implements the copy propagation pass and provides a
   handful of interfaces for performing const/copy propagation and
   simple expression replacement which keep variable annotations
   up-to-date.

   We require that for any copy operation where the RHS and LHS have
   a non-null memory tag the memory tag be the same.   It is OK
   for one or both of the memory tags to be NULL.

   We also require tracking if a variable is dereferenced in a load or
   store operation.

   We enforce these requirements by having all copy propagation and
   replacements of one SSA_NAME with a different SSA_NAME to use the
   APIs defined in this file.  */

/* Return true if we may propagate ORIG into DEST, false otherwise.  */

bool
may_propagate_copy (tree dest, tree orig)
{
  tree type_d = TREE_TYPE (dest);
  tree type_o = TREE_TYPE (orig);

  /* If ORIG flows in from an abnormal edge, it cannot be propagated.  */
  if (TREE_CODE (orig) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (orig))
    return false;

  /* If DEST is an SSA_NAME that flows from an abnormal edge, then it
     cannot be replaced.  */
  if (TREE_CODE (dest) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (dest))
    return false;

  /* Do not copy between types for which we *do* need a conversion.  */
  if (!useless_type_conversion_p (type_d, type_o))
    return false;

  /* Propagating virtual operands is always ok.  */
  if (TREE_CODE (dest) == SSA_NAME && !is_gimple_reg (dest))
    {
      /* But only between virtual operands.  */
      gcc_assert (TREE_CODE (orig) == SSA_NAME && !is_gimple_reg (orig));

      return true;
    }

  /* Anything else is OK.  */
  return true;
}

EXTERN_C_END
