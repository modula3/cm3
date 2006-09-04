/* M3 language support routines for GDB, the GNU debugger.
   Copyright 1992, 1993 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* This file contains globals and initialization for Modula-3 support. */ 

#if !defined (M3_EVAL_H)
#define M3_EVAL_H 1
 
#include <stdbool.h> 

#include "defs.h"
#include "expression.h"
#include "gdbtypes.h"
#include "value.h"

extern struct value *
m3_evaluate_subexp(struct type *expect_type,
		   struct expression *exp, int *pos,
                   enum noside noside);

#endif /* !defined (M3_EVAL_H) */

/* End of file m3-eval.h */ 
