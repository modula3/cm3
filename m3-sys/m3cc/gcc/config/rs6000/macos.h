/* CYGNUS LOCAL mpw (entire file) */
/* Definitions of target machine for GNU compiler, for PowerPC
   Macintosh running MacOS.
   Copyright (C) 1995, 1996 Free Software Foundation, Inc.
   Contributed by Stan Shebs (shebs@cygnus.com).

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Say that we are MacOS, not AIX. */
#define TARGET_MACOS 1

/* This would be nice to have, but cpp needs to know the setting
   of this flag, and currently does not see target-specific flags. */
#if 0
#define MASK_MAP_CR		0x10000000
#define	TARGET_MAP_CR		(target_flags & MASK_MAP_CR)
#undef  SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES		\
  {"mapcr",		MASK_MAP_CR},				\
  {"no-mapcr",		- MASK_MAP_CR},
#endif

#include "rs6000/rs6000.h"

#define SWITCH_TAKES_ARG(CHAR)      \
  ((CHAR) == 'D' || (CHAR) == 'U' || (CHAR) == 'o' \
   || (CHAR) == 'e' || (CHAR) == 'T' || (CHAR) == 'u' \
   || (CHAR) == 'I' || (CHAR) == 'm' || (CHAR) == 'x' \
   || (CHAR) == 'L' || (CHAR) == 'A' \
   || (CHAR) == 'd' || (CHAR) == 'i' )

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_POWERPC | MASK_NEW_MNEMONICS /* | MASK_MAP_CR */)

#undef PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT PROCESSOR_PPC601

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmacintosh -D__POWERC -D__powerc -Dpowerc -DPPC -Asystem(macos) -Acpu(powerpc) -Amachine(powerpc)"

#undef CPP_SPEC
#define CPP_SPEC "\
%{.c: -lang-c-c++-comments} \
%{posix: -D_POSIX_SOURCE} \
%(cpp_cpu)"

#undef LINK_SPEC
#define LINK_SPEC \
 "%{!nostdlib:%{!r*:%{!e*:-e __start}}}"

#define DEFAULT_PCC_STRUCT_RETURN 0

/* MacOS flips the default assignment of newline and return. */

#undef TARGET_NEWLINE
/* #define TARGET_NEWLINE (TARGET_MAP_CR ? 015 : 012) */
#define TARGET_NEWLINE 015

#undef TARGET_CR
/* #define TARGET_CR (TARGET_MAP_CR ? 012 : 015) */
#define TARGET_CR 012

/* Support for m68k alignment pragma. */

#define HANDLE_PRAGMA(FILE, NODE) (handle_mac_pragma (FILE, NODE))

extern int mac68k_aligned;

#undef EMPTY_FIELD_BOUNDARY
#define EMPTY_FIELD_BOUNDARY (mac68k_aligned ? 16 : 32)

#undef BIGGEST_FIELD_ALIGNMENT
#define BIGGEST_FIELD_ALIGNMENT (mac68k_aligned ? 16 : 32)
