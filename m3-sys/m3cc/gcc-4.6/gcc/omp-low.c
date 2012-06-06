/* Modula-3: modified */

/* Lowering pass for OpenMP directives.  Converts OpenMP directives
   into explicit calls to the runtime library (libgomp) and data
   marshalling to implement data sharing and copying clauses.
   Contributed by Diego Novillo <dnovillo@redhat.com>

   Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010
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

EXTERN_C_START

/* Deliberately kept for the garbage collection generated code,
   for target=alpha (config/alpha/alpha.c)
*/

static GTY(()) unsigned int tmp_ompfn_id_num;

static GTY((param1_is (tree), param2_is (tree)))
  splay_tree critical_name_mutexes;


EXTERN_C_END

#include "gt-omp-low.h"
