/* Modula-3: modified */

/* Target-independent configuration for VxWorks and VxWorks AE.   
   Copyright (C) 2005, 2007, 2008 Free Software Foundation, Inc.
   Contributed by CodeSourcery, LLC.

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

/* VxWorks cannot have dots in constructor labels, because it uses a
   mutant variation of collect2 that generates C code instead of
   assembly.  Thus each constructor label must be a legitimate C
   symbol.  FIXME: Have VxWorks use real collect2 instead.  */
#undef NO_DOLLAR_IN_LABEL
#define NO_DOT_IN_LABEL

/* VxWorks uses wchar_t == unsigned short (UCS2) on all architectures.  */

/* Dwarf2 unwind info is not supported.  */
#undef DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO 0

/* VxWorks uses DWARF2.  */
#define DWARF2_DEBUGGING_INFO 1
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* None of these other formats is supported.  */
#undef DWARF_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO
#undef SDB_DEBUGGING_INFO
#undef XCOFF_DEBUGGING_INFO
#undef VMS_DEBUGGING_INFO

/* Kernel mode doesn't have ctors/dtors, but RTP mode does.  */
#define TARGET_HAVE_CTORS_DTORS false
#define VXWORKS_OVERRIDE_OPTIONS /* empty */

/* No math library needed.  */
#define MATH_LIBRARY ""

/* No profiling.  */
#define VXWORKS_FUNCTION_PROFILER(FILE, LABELNO) do	\
{							\
  sorry ("profiler support for VxWorks");		\
} while (0)

/* We occasionally need to distinguish between the VxWorks variants.  */
#define VXWORKS_KIND_NORMAL  1
#define VXWORKS_KIND_AE      2
