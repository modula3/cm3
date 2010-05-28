/* test file for mpc_pow_ui.

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

#include "mpc-tests.h"

int
main (void)
{
  mpc_t z;

  test_start ();

  mpc_init2 (z, 5);
  mpc_set_ui_ui (z, 3, 2, MPC_RNDNN);
  mpc_pow_ui (z, z, 3, MPC_RNDNN);
  if (mpc_cmp_si_si (z, -9, 46) != 0)
    {
      printf ("Error for mpc_pow_ui (1)\n");
      exit (1);
    }
  mpc_clear (z);

  test_end ();

  return 0;
}
