/* Modula-3: modified */

/* Base configuration file for all FreeBSD targets.
   Copyright (C) 1999, 2000, 2001, 2004, 2005, 2007, 2009, 2010, 2011,
   2012 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* Common FreeBSD configuration. 
   All FreeBSD architectures should include this file, which will specify
   their commonalities.
   Adapted from gcc/config/freebsd.h by 
   David O'Brien <obrien@FreeBSD.org>
   Loren J. Rittle <ljrittle@acm.org>.  */


/* In case we need to know.  */
#define USING_CONFIG_FREEBSD_SPEC 1

#define FBSD_TARGET_OS_CPP_BUILTINS()					\
  do									\
    {									\
	builtin_define_with_int_value ("__FreeBSD__", FBSD_MAJOR);	\
	builtin_define_std ("unix");					\
	builtin_define ("__KPRINTF_ATTRIBUTE__");		       	\
	builtin_assert ("system=unix");					\
	builtin_assert ("system=bsd");					\
	builtin_assert ("system=FreeBSD");				\
	FBSD_TARGET_CPU_CPP_BUILTINS();					\
    }									\
  while (0)

/* Define the default FreeBSD-specific per-CPU hook code.  */
#define FBSD_TARGET_CPU_CPP_BUILTINS() do {} while (0)

/* NOTE: The freebsd-spec.h header is included also for various
   non-FreeBSD powerpc targets, thus it should never define macros
   other than FBSD_* prefixed ones, or USING_CONFIG_FREEBSD_SPEC.  */
