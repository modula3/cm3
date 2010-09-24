/* Modula-3: doesn't need this, gutted */

/* Decimal floating point support.
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "toplev.h"
#include "real.h"
#include "tm_p.h"
#include "dfp.h"

#include "decimal128.h"
#include "decimal64.h"
#include "decimal32.h"
#include "decNumber.h"

void
decimal_real_from_string (REAL_VALUE_TYPE *r, const char *s)
{ gcc_unreachable (); }

void
encode_decimal32 (const struct real_format *fmt ATTRIBUTE_UNUSED,
		  long *buf, const REAL_VALUE_TYPE *r)
{ gcc_unreachable (); }

void
decode_decimal32 (const struct real_format *fmt ATTRIBUTE_UNUSED,
		  REAL_VALUE_TYPE *r, const long *buf)
{ gcc_unreachable (); }

void
encode_decimal64 (const struct real_format *fmt ATTRIBUTE_UNUSED,
		  long *buf, const REAL_VALUE_TYPE *r)
{ gcc_unreachable (); }

void
decode_decimal64 (const struct real_format *fmt ATTRIBUTE_UNUSED,
		  REAL_VALUE_TYPE *r, const long *buf)
{ gcc_unreachable (); }

void
encode_decimal128 (const struct real_format *fmt ATTRIBUTE_UNUSED,
		   long *buf, const REAL_VALUE_TYPE *r)
{ gcc_unreachable (); }

void
decode_decimal128 (const struct real_format *fmt ATTRIBUTE_UNUSED,
		   REAL_VALUE_TYPE *r, const long *buf)
{ gcc_unreachable (); }

int
decimal_do_compare (const REAL_VALUE_TYPE *a, const REAL_VALUE_TYPE *b,
		    int nan_result)
{ gcc_unreachable (); }

void
decimal_round_for_format (const struct real_format *fmt, REAL_VALUE_TYPE *r)
{ gcc_unreachable (); }

void
decimal_real_convert (REAL_VALUE_TYPE *r, enum machine_mode mode, 
		      const REAL_VALUE_TYPE *a)
{ gcc_unreachable (); }

void
decimal_real_to_decimal (char *str, const REAL_VALUE_TYPE *r_orig,
			 size_t buf_size,
			 size_t digits ATTRIBUTE_UNUSED,
			 int crop_trailing_zeros ATTRIBUTE_UNUSED)
{ gcc_unreachable (); }

void
decimal_do_fix_trunc (REAL_VALUE_TYPE *r, const REAL_VALUE_TYPE *a)
{ gcc_unreachable (); }

HOST_WIDE_INT
decimal_real_to_integer (const REAL_VALUE_TYPE *r)
{ gcc_unreachable (); }

void
decimal_real_to_integer2 (HOST_WIDE_INT *plow, HOST_WIDE_INT *phigh,
			  const REAL_VALUE_TYPE *r)
{ gcc_unreachable (); }

bool
decimal_real_arithmetic (REAL_VALUE_TYPE *r, enum tree_code code,
			 const REAL_VALUE_TYPE *op0,
			 const REAL_VALUE_TYPE *op1)
{ gcc_unreachable (); }

void
decimal_real_maxval (REAL_VALUE_TYPE *r, int sign, enum machine_mode mode)
{ gcc_unreachable (); }
