/* Modula-3: mostly doesn't need this, gutted */

/* Fixed-point arithmetic support.
   Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "toplev.h"
#include "fixed-value.h"

/* Compare two fixed objects for bitwise identity.  */

bool
fixed_identical (const FIXED_VALUE_TYPE *a, const FIXED_VALUE_TYPE *b)
{
  return (a->mode == b->mode
	  && a->data.high == b->data.high
	  && a->data.low == b->data.low);
}
 
/* Calculate a hash value.  */

unsigned int
fixed_hash (const FIXED_VALUE_TYPE *f)
{
  return (unsigned int) (f->data.low ^ f->data.high);
}

void
fixed_from_string (FIXED_VALUE_TYPE *f ATTRIBUTE_UNUSED, const char *str ATTRIBUTE_UNUSED, enum machine_mode mode ATTRIBUTE_UNUSED)
  { gcc_unreachable (); }

void
fixed_to_decimal (char *str ATTRIBUTE_UNUSED, const FIXED_VALUE_TYPE *f_orig ATTRIBUTE_UNUSED,
		  size_t buf_size ATTRIBUTE_UNUSED)
  { gcc_unreachable (); }

bool
fixed_arithmetic (FIXED_VALUE_TYPE *f ATTRIBUTE_UNUSED, int icode ATTRIBUTE_UNUSED, const FIXED_VALUE_TYPE *op0 ATTRIBUTE_UNUSED,
		  const FIXED_VALUE_TYPE *op1 ATTRIBUTE_UNUSED, bool sat_p ATTRIBUTE_UNUSED)
  { gcc_unreachable ();
    return 0; }

bool
fixed_compare (int icode ATTRIBUTE_UNUSED, const FIXED_VALUE_TYPE *op0 ATTRIBUTE_UNUSED,
	       const FIXED_VALUE_TYPE *op1 ATTRIBUTE_UNUSED)
  { gcc_unreachable ();
    return 0; }

bool
fixed_convert (FIXED_VALUE_TYPE *f ATTRIBUTE_UNUSED, enum machine_mode mode ATTRIBUTE_UNUSED,
               const FIXED_VALUE_TYPE *a ATTRIBUTE_UNUSED, bool sat_p ATTRIBUTE_UNUSED)
  { gcc_unreachable ();
    return 0; }

bool
fixed_convert_from_int (FIXED_VALUE_TYPE *f ATTRIBUTE_UNUSED, enum machine_mode mode ATTRIBUTE_UNUSED,
			double_int a ATTRIBUTE_UNUSED, bool unsigned_p ATTRIBUTE_UNUSED, bool sat_p ATTRIBUTE_UNUSED)
  { gcc_unreachable ();
    return 0; }

bool
fixed_convert_from_real (FIXED_VALUE_TYPE *f ATTRIBUTE_UNUSED, enum machine_mode mode ATTRIBUTE_UNUSED,
			 const REAL_VALUE_TYPE *a ATTRIBUTE_UNUSED, bool sat_p ATTRIBUTE_UNUSED)
  { gcc_unreachable ();
    return 0; }

void
real_convert_from_fixed (REAL_VALUE_TYPE *r ATTRIBUTE_UNUSED, enum machine_mode mode ATTRIBUTE_UNUSED,
			 const FIXED_VALUE_TYPE *f ATTRIBUTE_UNUSED)
  { gcc_unreachable (); }

bool
fixed_isneg (const FIXED_VALUE_TYPE *f ATTRIBUTE_UNUSED)
  { gcc_unreachable ();
    return 0; }
