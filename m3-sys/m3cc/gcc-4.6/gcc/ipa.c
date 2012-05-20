/* Modula-3: modified */

/* Basic IPA optimizations and utilities.
   Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009, 2010, 2011
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
#include "cgraph.h"
#include "tree-pass.h"
#include "timevar.h"
#include "gimple.h"
#include "ggc.h"
#include "flags.h"
#include "pointer-set.h"
#include "target.h"
#include "tree-iterator.h"

EXTERN_C_START

/* Fill array order with all nodes with output flag set in the reverse
   topological order.  */

int
cgraph_postorder (struct cgraph_node **order)
{
  struct cgraph_node *node, *node2;
  int stack_size = 0;
  int order_pos = 0;
  struct cgraph_edge *edge, last;
  int pass;

  struct cgraph_node **stack =
    XCNEWVEC (struct cgraph_node *, cgraph_n_nodes);

  /* We have to deal with cycles nicely, so use a depth first traversal
     output algorithm.  Ignore the fact that some functions won't need
     to be output and put them into order as well, so we get dependencies
     right through inline functions.  */
  for (node = cgraph_nodes; node; node = node->next)
    node->aux = NULL;
  for (pass = 0; pass < 2; pass++)
    for (node = cgraph_nodes; node; node = node->next)
      if (!node->aux
	  && (pass
	      || (!node->address_taken
		  && !node->global.inlined_to
		  && !cgraph_only_called_directly_p (node))))
	{
	  node2 = node;
	  if (!node->callers)
	    node->aux = &last;
	  else
	    node->aux = node->callers;
	  while (node2)
	    {
	      while (node2->aux != &last)
		{
		  edge = (struct cgraph_edge *) node2->aux;
		  if (edge->next_caller)
		    node2->aux = edge->next_caller;
		  else
		    node2->aux = &last;
		  /* Break possible cycles involving always-inline
		     functions by ignoring edges from always-inline
		     functions to non-always-inline functions.  */
		  if (edge->caller->local.disregard_inline_limits
		      && !edge->callee->local.disregard_inline_limits)
		    continue;
		  if (!edge->caller->aux)
		    {
		      if (!edge->caller->callers)
			edge->caller->aux = &last;
		      else
			edge->caller->aux = edge->caller->callers;
		      stack[stack_size++] = node2;
		      node2 = edge->caller;
		      break;
		    }
		}
	      if (node2->aux == &last)
		{
		  order[order_pos++] = node2;
		  if (stack_size)
		    node2 = stack[--stack_size];
		  else
		    node2 = NULL;
		}
	    }
	}
  free (stack);
  for (node = cgraph_nodes; node; node = node->next)
    node->aux = NULL;
  return order_pos;
}

EXTERN_C_END
