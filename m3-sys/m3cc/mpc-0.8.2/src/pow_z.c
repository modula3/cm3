/* mpc_pow_z -- Raise a complex number to an integer power.

Copyright (C) 2009 Paul Zimmermann

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

int
mpc_pow_z (mpc_ptr z, mpc_srcptr x, mpz_srcptr y, mpc_rnd_t rnd)
{
  mpc_t yy;
  int inex;
  size_t n = mpz_sizeinbase (y, 2);

  mpc_init3 (yy, (n < MPFR_PREC_MIN) ? MPFR_PREC_MIN : n, MPFR_PREC_MIN);
  mpc_set_z (yy, y, MPC_RNDNN);   /* exact */
  inex = mpc_pow (z, x, yy, rnd);
  mpc_clear (yy);
  return inex;
}

