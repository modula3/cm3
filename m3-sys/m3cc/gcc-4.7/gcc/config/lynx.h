/* Modula-3: modified */

/* Target independent definitions for LynxOS.
   Copyright (C) 1993, 1994, 1995, 1996, 1999, 2000, 2002, 2003, 2004,
   2007 Free Software Foundation, Inc.

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

/* In this file we set up defaults that can be chosen by
   <target>/lynx.h files.  A target-specific lynx.h file can decide
   either to define and override these definitions or to use them by
   ensuring they are undefined at this point.  If we were to #undef
   them here we might accidentally disable some target-specific
   defines.  */

/* Define ASM_OUTPUT_ALIGN to use the .balign directive rather that
   the .align directive with GAS.  */

#ifndef ASM_OUTPUT_ALIGN
# define ASM_OUTPUT_ALIGN(FILE, LOG) 			\
  do							\
    {							\
      if ((LOG) != 0)					\
	fprintf ((FILE), "\t.balign %d\n", 1 << (LOG));	\
    }							\
  while (0)
#endif

/* Keep the *_DEBUGGING_INFO defines from elfos.h except that stabs is
   the default on LynxOS.  */

#ifndef PREFERRED_DEBUGGING_TYPE
# define PREFERRED_DEBUGGING_TYPE DBX_DEBUG
#endif

#ifndef TARGET_POSIX_IO
# define TARGET_POSIX_IO
#endif
