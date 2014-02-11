/* Modula-3: modified */

/* Default macros to initialize the lang_hooks data structure.
   Copyright 2003, 2004, 2007 Free Software Foundation, Inc.

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

#ifndef GCC_HOST_HOOKS_DEF_H
#define GCC_HOST_HOOKS_DEF_H

#include "hooks.h"

#define HOST_HOOKS_EXTRA_SIGNALS hook_void_void

/* The structure is defined in hosthooks.h.  */
#define HOST_HOOKS_INITIALIZER {		\
  HOST_HOOKS_EXTRA_SIGNALS,			\
}

#endif /* GCC_HOST_HOOKS_DEF_H */
