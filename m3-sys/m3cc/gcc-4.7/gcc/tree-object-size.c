/* Modula-3: modified */

/* __builtin_object_size (ptr, object_size_type) computation
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>

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
#include "diagnostic-core.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"

/* Simple pass to optimize all __builtin_object_size () builtins.  */

static unsigned int
compute_object_sizes (void)
{
  basic_block bb;
  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator i;
      for (i = gsi_start_bb (bb); !gsi_end_p (i); gsi_next (&i))
	{
	  tree callee, result;
	  gimple call = gsi_stmt (i);

          if (gimple_code (call) != GIMPLE_CALL)
	    continue;

	  callee = gimple_call_fndecl (call);
	  if (!callee
	      || DECL_BUILT_IN_CLASS (callee) != BUILT_IN_NORMAL
	      || DECL_FUNCTION_CODE (callee) != BUILT_IN_OBJECT_SIZE)
	    continue;
	    
	  gcc_unreachable ();	    
	}
    }

  fini_object_sizes ();
  return 0;
}

struct gimple_opt_pass pass_object_sizes =
{
 {
  GIMPLE_PASS,
  "objsz",				/* name */
  NULL,					/* gate */
  compute_object_sizes,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_verify_ssa	                /* todo_flags_finish */
 }
};
