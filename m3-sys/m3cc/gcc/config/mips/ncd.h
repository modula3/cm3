/* CYGNUS LOCAL: entire file */

/* Definitions of target machine for GNU compiler.
   NCD R3000 or R4000 based X terminal.
   Copyright (C) 1994 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define CC1_SPEC "\
%{gline:%{!g:%{!g0:%{!g1:%{!g2: -g1}}}}} \
%{g: -ggdb} \
%{mips1:-mfp32 -mgp32} %{mips2:-mfp32 -mgp32} %{mips3:-mfp64 -mgp64} \
%{G*} \
%{pic-none:   -mno-half-pic} \
%{pic-lib:    -mhalf-pic} \
%{pic-extern: -mhalf-pic} \
%{pic-calls:  -mhalf-pic} \
%{save-temps: }"

#define BLOCK_PROFILER_CODE
#define DEFAULT_GDB_EXTENSIONS 1
#define TARGET_DEFAULT MASK_GAS
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG
#define MACHINE_TYPE "NCD R3000- or R4000-based X terminal"

#include "mips/mips.h"

#undef SDB_DEBUGGING_INFO
