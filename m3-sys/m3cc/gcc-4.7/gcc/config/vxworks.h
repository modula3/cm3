/* Modula-3: modified */

/* Common VxWorks target definitions for GNU compiler.
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2007, 2010
   Free Software Foundation, Inc.
   Contributed by Wind River Systems.
   Rewritten by CodeSourcery, LLC.

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

/* Assert that we are targetting VxWorks.  */
#undef TARGET_VXWORKS
#define TARGET_VXWORKS 1

/* Do VxWorks-specific parts of TARGET_OPTION_OVERRIDE.  */
#undef VXWORKS_OVERRIDE_OPTIONS
#define VXWORKS_OVERRIDE_OPTIONS vxworks_override_options ()
extern void vxworks_override_options (void);

/* Only RTPs support prioritized constructors and destructors:
   the implementation relies on numbered .ctors* sections.  */
#define SUPPORTS_INIT_PRIORITY TARGET_VXWORKS_RTP

/* VxWorks requires special handling of constructors and destructors.
   All VxWorks configurations must use these functions.  */
#undef TARGET_ASM_CONSTRUCTOR
#define TARGET_ASM_CONSTRUCTOR vxworks_asm_out_constructor
#undef TARGET_ASM_DESTRUCTOR
#define TARGET_ASM_DESTRUCTOR vxworks_asm_out_destructor
extern void vxworks_asm_out_constructor (rtx symbol, int priority);
extern void vxworks_asm_out_destructor (rtx symbol, int priority);

/* Override the vxworks-dummy.h definitions.  TARGET_VXWORKS_RTP
   is defined by vxworks.opt.  */
#undef VXWORKS_GOTT_BASE
#define VXWORKS_GOTT_BASE "__GOTT_BASE__"
#undef VXWORKS_GOTT_INDEX
#define VXWORKS_GOTT_INDEX "__GOTT_INDEX__"

/* Both kernels and RTPs have the facilities required by this macro.  */
#define TARGET_POSIX_IO

/* A VxWorks implementation of TARGET_OS_CPP_BUILTINS.  */
#define VXWORKS_OS_CPP_BUILTINS()					\
  do									\
    {									\
      builtin_define ("__vxworks");					\
      builtin_define ("__VXWORKS__");					\
      builtin_assert ("system=unix");					\
      if (TARGET_VXWORKS_RTP)						\
	builtin_define ("__RTP__");					\
      else								\
	builtin_define ("_WRS_KERNEL");					\
    }									\
  while (0)

#define VXWORKS_KIND VXWORKS_KIND_NORMAL

/* The diab linker does not handle .gnu_attribute sections.  */
#undef HAVE_AS_GNU_ATTRIBUTE
