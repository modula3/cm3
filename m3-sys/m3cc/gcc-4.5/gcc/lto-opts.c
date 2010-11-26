/* Modula-3: modified */

/* LTO IL options.

   Copyright 2009 Free Software Foundation, Inc.
   Contributed by Simon Baldwin <simonb@google.com>

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
#include "tree.h"
#include "hashtab.h"
#include "ggc.h"
#include "vec.h"
#include "bitmap.h"
#include "flags.h"
#include "opts.h"
#include "options.h"
#include "target.h"
#include "toplev.h"
#include "lto-streamer.h"

#ifdef __cplusplus
extern "C" {
#endif

void
lto_register_user_option (size_t , const char *, int, int)
{
  gcc_unreachable ();
}

void
lto_clear_user_options (void)
{
  // nothing
}

void
lto_clear_file_options (void)
{
  gcc_unreachable ();
}

void
lto_write_options (void)
{
  gcc_unreachable ();
}

void
lto_read_file_options (struct lto_file_decl_data *)
{
  gcc_unreachable ();
}

void
lto_reissue_options (void)
{
  gcc_unreachable ();
}

#ifdef __cplusplus
} /* extern "C" */
#endif
