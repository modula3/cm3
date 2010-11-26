/* Modula-3: modified */

/* Miscellaneous utilities for GIMPLE streaming.  Things that are used
   in both input and output are here.

   Copyright 2009, 2010 Free Software Foundation, Inc.
   Contributed by Doug Kwan <dougkwan@google.com>

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
#include "flags.h"
#include "tree.h"
#include "gimple.h"
#include "tree-flow.h"
#include "diagnostic.h"
#include "bitmap.h"
#include "vec.h"
#include "lto-streamer.h"

#ifdef __cplusplus
extern "C" {
#endif

const char *
lto_tag_name (enum LTO_tags)
{
  gcc_unreachable ();
  return 0;
}

bitmap
lto_bitmap_alloc (void)
{
  gcc_unreachable ();
  return 0;
}

void
lto_bitmap_free (bitmap)
{
  gcc_unreachable ();
}

char *
lto_get_section_name (int, const char *)
{
  gcc_unreachable ();
  return 0;
}

void
print_lto_report (void)
{
  gcc_unreachable ();
}

struct bitpack_d *
bitpack_create (void)
{
  return 0;
}

void
bitpack_delete (struct bitpack_d *)
{
  gcc_unreachable ();
}

void
bp_pack_value (struct bitpack_d *, bitpack_word_t, unsigned)
{
  gcc_unreachable ();
}

bitpack_word_t
bp_unpack_value (struct bitpack_d *, unsigned)
{
  gcc_unreachable ();
  return 0;
}

bool
lto_streamer_cache_insert (struct lto_streamer_cache_d *, tree,
			   int *, unsigned *)
{
  gcc_unreachable ();
  return 0;
}

bool
lto_streamer_cache_insert_at (struct lto_streamer_cache_d *,
			      tree, int)
{
  gcc_unreachable ();
  return 0;
}

bool
lto_streamer_cache_lookup (struct lto_streamer_cache_d *, tree,
			   int *)
{
  gcc_unreachable ();
  return 0;
}

tree
lto_streamer_cache_get (struct lto_streamer_cache_d *, int)
{
  gcc_unreachable ();
  return 0;
}

struct lto_streamer_cache_d *
lto_streamer_cache_create (void)
{
  gcc_unreachable ();
  return 0;
}

void
lto_streamer_cache_delete (struct lto_streamer_cache_d *)
{
  gcc_unreachable ();
}

void
lto_streamer_init (void)
{
  gcc_unreachable ();
}

bool
gate_lto_out (void)
{
  return false;
}

void
lto_check_version (int, int)
{
  gcc_unreachable ();
}

#ifdef __cplusplus
} /* extern "C" */
#endif
