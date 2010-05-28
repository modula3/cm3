/* test file for mpc_add_ui.

Copyright (C) 2008 Philippe Th\'eveny, Andreas Enge

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

#include <stdlib.h>
#include "mpc-tests.h"

static void
check_ternary_value (void)
{
  unsigned int i;
  mpc_t z;

  mpc_init2 (z, 2);

  for (i=2; i <= 1024; i++)
    {
      mpc_set_prec (z, i);

      mpc_set_ui (z, 1, MPC_RNDNN);
      if (mpc_add_ui (z, z, 1, MPC_RNDNZ))
        {
          printf ("Error in mpc_add_ui: 1+1 should be exact\n");
          exit (1);
        }

      mpc_set_ui (z, 1, MPC_RNDNN);
      mpc_mul_2exp (z, z, i, MPC_RNDNN);
      if (mpc_add_ui (z, z, 1, MPC_RNDNN) == 0)
        {
          printf ("Error in mpc_add_ui: 2^prec+1 cannot be exact\n");
          exit (1);
        }
    }

  mpc_clear (z);
}

int
main (void)
{
  DECL_FUNC (CCU, f, mpc_add_ui);

  test_start ();

  check_ternary_value ();
  tgeneric (f, 2, 1024, 7, -1);

  test_end ();

  return 0;
}
