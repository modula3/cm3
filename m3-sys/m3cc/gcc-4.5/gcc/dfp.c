/* Modula-3: doesn't need this, gutted */

/* Decimal floating point support.
   Copyright (C) 2005, 2006, 2007, 2008, 2009 Free Software
   Foundation, Inc.

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
#include "real.h"
#include "tm_p.h"
#include "dfp.h"

#ifdef __cplusplus
extern "C" {
#endif

void
 decimal_real_from_string (REAL_VALUE_TYPE *r ATTRIBUTE_UNUSED, const char *s ATTRIBUTE_UNUSED)
{ gcc_unreachable (); }

void
encode_decimal32 (const struct real_format *fmt ATTRIBUTE_UNUSED,
		  long *buf ATTRIBUTE_UNUSED, const REAL_VALUE_TYPE *r ATTRIBUTE_UNUSED)
{ gcc_unreachable (); }

void
decode_decimal32 (const struct real_format *fmt ATTRIBUTE_UNUSED,
		  REAL_VALUE_TYPE *r ATTRIBUTE_UNUSED, const long *buf ATTRIBUTE_UNUSED)
{ gcc_unreachable (); }

void
encode_decimal64 (const struct real_format *fmt ATTRIBUTE_UNUSED,
		  long *buf ATTRIBUTE_UNUSED, const REAL_VALUE_TYPE *r ATTRIBUTE_UNUSED)
{ gcc_unreachable (); }

void
decode_decimal64 (const struct real_format *fmt ATTRIBUTE_UNUSED,
		  REAL_VALUE_TYPE *r ATTRIBUTE_UNUSED, const long *buf ATTRIBUTE_UNUSED)
{ gcc_unreachable (); }

void
encode_decimal128 (const struct real_format *fmt ATTRIBUTE_UNUSED,
		   long *buf ATTRIBUTE_UNUSED, const REAL_VALUE_TYPE *r ATTRIBUTE_UNUSED)
{ gcc_unreachable (); }

void
decode_decimal128 (const struct real_format *fmt ATTRIBUTE_UNUSED,
 		   REAL_VALUE_TYPE *r ATTRIBUTE_UNUSED, const long *buf ATTRIBUTE_UNUSED)
{ gcc_unreachable (); }

int
decimal_do_compare (const REAL_VALUE_TYPE *a ATTRIBUTE_UNUSED, const REAL_VALUE_TYPE *b ATTRIBUTE_UNUSED,
		    int nan_result ATTRIBUTE_UNUSED)
{ gcc_unreachable ();
  return 0; }

void
decimal_round_for_format (const struct real_format *fmt ATTRIBUTE_UNUSED, REAL_VALUE_TYPE *r ATTRIBUTE_UNUSED)
{ gcc_unreachable (); }

void
decimal_real_convert (REAL_VALUE_TYPE *r ATTRIBUTE_UNUSED, enum machine_mode mode ATTRIBUTE_UNUSED,
		      const REAL_VALUE_TYPE *a ATTRIBUTE_UNUSED)
{ gcc_unreachable (); }

void
decimal_real_to_decimal (char *str ATTRIBUTE_UNUSED, const REAL_VALUE_TYPE *r_orig ATTRIBUTE_UNUSED,
			 size_t buf_size ATTRIBUTE_UNUSED,
			 size_t digits ATTRIBUTE_UNUSED,
			 int crop_trailing_zeros ATTRIBUTE_UNUSED)
{ gcc_unreachable (); }

void
decimal_do_fix_trunc (REAL_VALUE_TYPE *r ATTRIBUTE_UNUSED, const REAL_VALUE_TYPE *a ATTRIBUTE_UNUSED)
{ gcc_unreachable (); }

HOST_WIDE_INT
decimal_real_to_integer (const REAL_VALUE_TYPE *r ATTRIBUTE_UNUSED)
{ gcc_unreachable ();
  return 0; }

void
decimal_real_to_integer2 (HOST_WIDE_INT *plow ATTRIBUTE_UNUSED, HOST_WIDE_INT *phigh ATTRIBUTE_UNUSED,
			  const REAL_VALUE_TYPE *r ATTRIBUTE_UNUSED)
{ gcc_unreachable (); }

bool
decimal_real_arithmetic (REAL_VALUE_TYPE *r ATTRIBUTE_UNUSED, enum tree_code code ATTRIBUTE_UNUSED,
			 const REAL_VALUE_TYPE *op0 ATTRIBUTE_UNUSED,
			 const REAL_VALUE_TYPE *op1 ATTRIBUTE_UNUSED)
{ gcc_unreachable ();
  return 0; }

void
decimal_real_maxval (REAL_VALUE_TYPE *r ATTRIBUTE_UNUSED, int sign ATTRIBUTE_UNUSED, enum machine_mode mode ATTRIBUTE_UNUSED)
{ gcc_unreachable (); }

#ifdef __cplusplus
} /* extern "C" */
#endif
