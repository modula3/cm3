/* Modula-3: modified */

/* Pretty formatting of GIMPLE statements and expressions.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com> and
   Diego Novillo <dnovillo@google.com>

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
#include "diagnostic.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "hashtab.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "gimple.h"

EXTERN_C_START

DEBUG_FUNCTION void
debug_gimple_stmt (gimple gs)
{
}

void
print_gimple_stmt (FILE *file, gimple g, int spc, int flags)
{
}

void
print_gimple_expr (FILE *file, gimple g, int spc, int flags)
{
}

void
print_gimple_seq (FILE *file, gimple_seq seq, int spc, int flags)
{
}

DEBUG_FUNCTION void
debug_gimple_seq (gimple_seq seq)
{
}

void
dump_gimple_stmt (pretty_printer *buffer, gimple gs, int spc, int flags)
{
}

void
gimple_dump_bb (basic_block bb, FILE *file, int indent, int flags)
{
}

EXTERN_C_END
