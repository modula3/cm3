/* Modula-3: modified */

/* Language-dependent hooks for LTO.
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
#include "flags.h"
#include "tm.h"
#include "tree.h"
#include "target.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "debug.h"
#include "lto-tree.h"
#include "lto.h"
#include "tree-inline.h"
#include "gimple.h"
#include "diagnostic-core.h"
#include "toplev.h"
#include "lto-streamer.h"


#include "ggc.h"
#include "gtype-lto.h"
#include "gt-lto-lto-lang.h"
