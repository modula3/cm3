/* Modula-3: modified */

/* Top-level LTO routines.
   Copyright 2009, 2010 Free Software Foundation, Inc.
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
#include "opts.h"
#include "toplev.h"
#include "tree.h"
#include "diagnostic-core.h"
#include "tm.h"
#include "cgraph.h"
#include "ggc.h"
#include "tree-ssa-operands.h"
#include "tree-pass.h"
#include "langhooks.h"
#include "vec.h"
#include "bitmap.h"
#include "pointer-set.h"
#include "ipa-prop.h"
#include "common.h"
#include "debug.h"
#include "timevar.h"
#include "gimple.h"
#include "lto.h"
#include "lto-tree.h"
#include "lto-streamer.h"
#include "splay-tree.h"
#include "params.h"

struct GTY (()) ltrans_partition_def
{
  cgraph_node_set cgraph_set;
  varpool_node_set varpool_set;
  const char * GTY ((skip)) name;
  int insns;
};

typedef struct ltrans_partition_def *ltrans_partition;
DEF_VEC_P(ltrans_partition);
DEF_VEC_ALLOC_P(ltrans_partition,gc);
static GTY (()) VEC(ltrans_partition, gc) *ltrans_partitions;

#include "gt-lto-lto.h"
