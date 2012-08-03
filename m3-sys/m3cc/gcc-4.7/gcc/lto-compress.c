/* Modula-3: modified */

/* LTO IL compression streams.

   Copyright 2009, 2010 Free Software Foundation, Inc.
   Contributed by Simon Baldwin <simonb@google.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
/* zlib.h includes other system headers.  Those headers may test feature
   test macros.  config.h may define feature test macros.  For this reason,
   zlib.h needs to be included after, rather than before, config.h and
   system.h.  */
#include <zlib.h>
#include "coretypes.h"
#include "tree.h"
#include "diagnostic-core.h"
#include "langhooks.h"
#include "lto-streamer.h"
#include "lto-compress.h"

/* Compression stream structure, holds the flush callback and opaque token,
   the buffered data, and a note of whether compressing or uncompressing.  */

struct lto_compression_stream
{
  void (*callback) (const char *, unsigned, void *);
  void *opaque;
  char *buffer;
  size_t bytes;
  size_t allocation;
  bool is_compression;
};

struct lto_compression_stream *
lto_start_compression (void (*callback) (const char *, unsigned, void *),
		       void *opaque)
{
  gcc_unreachable ();
  return 0;
}

void
lto_compress_block (struct lto_compression_stream *stream,
		    const char *base, size_t num_chars)
{
  gcc_unreachable ();
}

void
lto_end_compression (struct lto_compression_stream *stream)
{
  gcc_unreachable ();
}

struct lto_compression_stream *
lto_start_uncompression (void (*callback) (const char *, unsigned, void *),
			 void *opaque)
{
  gcc_unreachable ();
  return 0;
}

void
lto_uncompress_block (struct lto_compression_stream *stream,
		      const char *base, size_t num_chars)
{
  gcc_unreachable ();
}

void
lto_end_uncompression (struct lto_compression_stream *stream)
{
  gcc_unreachable ();
}
