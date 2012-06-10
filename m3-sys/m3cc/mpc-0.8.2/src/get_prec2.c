/* mpc_get_prec2 -- returns the precisions of the real and of the imaginary
part through the first two arguments

Copyright (C) 2007, 2009 Andreas Enge, Philippe Th\'eveny

This file is part of the MPC Library.

The MPC Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The MPC Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the MPC Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA. */

#include "mpc-impl.h"

void
mpc_get_prec2 (mp_prec_t *pr, mp_prec_t *pi, mpc_srcptr x)
{
   *pr = MPFR_PREC (MPC_RE (x));
   *pi = MPFR_PREC (MPC_IM (x));
}
