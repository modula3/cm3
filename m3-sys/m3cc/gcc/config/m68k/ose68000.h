/* CYGNUS LOCAL ericsson */
/* Definitions of target machine for GNU compiler.  OSE 68000 version.
   Copyright 1987, 1988, 1992, 1996 Free Software Foundation, Inc.

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

#include "m68k/m68k-none.h"
#include "aoutos.h"

#define DBX_DEBUGGING_INFO
#undef SDB_DEBUGGING_INFO

#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

/* Provide required defaults for linker -e. */
 
#define LINK_SPEC "%{!nostdlib:%{!r*:%{!e*:-e start}}}"

/* Alignment of field after `int : 0' in a structure.  */

#undef EMPTY_FIELD_BOUNDARY
#define EMPTY_FIELD_BOUNDARY 16

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16

/* User must provide startfile if desired.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""
