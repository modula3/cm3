/* Modula-3: modified */

/* Rtl-level induction variable analysis.
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This is a simple analysis of induction variables of the loop.  The major use
   is for determining the number of iterations of a loop for loop unrolling,
   doloop optimization and branch prediction.  The iv information is computed
   on demand.

   Induction variables are analyzed by walking the use-def chains.  When
   a basic induction variable (biv) is found, it is cached in the bivs
   hash table.  When register is proved to be a biv, its description
   is stored to DF_REF_DATA of the def reference.

   The analysis works always with one loop -- you must call
   iv_analysis_loop_init (loop) for it.  All the other functions then work with
   this loop.   When you need to work with another loop, just call
   iv_analysis_loop_init for it.  When you no longer need iv analysis, call
   iv_analysis_done () to clean up the memory.

   The available functions are:

   iv_analyze (insn, reg, iv): Stores the description of the induction variable
     corresponding to the use of register REG in INSN to IV.  Returns true if
     REG is an induction variable in INSN. false otherwise.
     If use of REG is not found in INSN, following insns are scanned (so that
     we may call this function on insn returned by get_condition).
   iv_analyze_result (insn, def, iv):  Stores to IV the description of the iv
     corresponding to DEF, which is a register defined in INSN.
   iv_analyze_expr (insn, rhs, mode, iv):  Stores to IV the description of iv
     corresponding to expression EXPR evaluated at INSN.  All registers used bu
     EXPR must also be used in INSN.
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"

EXTERN_C_START

rtx
lowpart_subreg (enum machine_mode outer_mode, rtx expr,
		enum machine_mode inner_mode)
{
  return simplify_gen_subreg (outer_mode, expr, inner_mode,
			      subreg_lowpart_offset (outer_mode, inner_mode));
}

EXTERN_C_END
