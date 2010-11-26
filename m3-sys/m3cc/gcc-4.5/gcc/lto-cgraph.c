/* Modula-3: modified */

/* Write and read the cgraph to the memory mapped representation of a
   .o file.

   Copyright 2009 Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>

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
#include "langhooks.h"
#include "basic-block.h"
#include "tree-flow.h"
#include "cgraph.h"
#include "function.h"
#include "ggc.h"
#include "diagnostic.h"
#include "except.h"
#include "vec.h"
#include "timevar.h"
#include "output.h"
#include "pointer-set.h"
#include "lto-streamer.h"
#include "gcov-io.h"

#ifdef __cplusplus
extern "C" {
#endif

lto_cgraph_encoder_t
lto_cgraph_encoder_new (void)
{
  gcc_unreachable ();
  return 0;
}

void
lto_cgraph_encoder_delete (lto_cgraph_encoder_t )
{
}

int
lto_cgraph_encoder_encode (lto_cgraph_encoder_t ,
			   struct cgraph_node *)
{
  gcc_unreachable ();
  return 0;
}

int
lto_cgraph_encoder_lookup (lto_cgraph_encoder_t ,
			   struct cgraph_node *)
{
  gcc_unreachable ();
  return 0;
}

struct cgraph_node *
lto_cgraph_encoder_deref (lto_cgraph_encoder_t , int )
{
  gcc_unreachable ();
  return 0;
}

void
output_cgraph (cgraph_node_set)
{
  gcc_unreachable ();
}

void
input_cgraph (void)
{
  gcc_unreachable ();
}

#ifdef __cplusplus
} /* extern "C" */
#endif
