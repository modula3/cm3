/* Test file for mpfr_out_str.

Copyright 1999, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
Contributed by the Arenaire and Cacao projects, INRIA.

This file is part of the GNU MPFR Library.

The GNU MPFR Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MPFR Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MPFR Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
MA 02110-1301, USA. */

#include <float.h>
#include <stdlib.h>

#include "mpfr-test.h"

FILE *fout;

#define check(d,r,b) check4(d,r,b,53)

static void
check4 (double d, mp_rnd_t rnd, int base, int prec)
{
  mpfr_t x;

  mpfr_init2 (x, prec);
  mpfr_set_d (x, d, rnd);
  fprintf (fout, "%1.19e base %d rnd %d:\n ", d, base, rnd);
  mpfr_out_str (fout, base, (base == 2) ? prec : 0, x, rnd);
  fputc ('\n', fout);
  mpfr_clear (x);
}

static void
special (void)
{
  mpfr_t x;

  mpfr_init (x);

  mpfr_set_nan (x);
  mpfr_out_str (fout, 10, 0, x, GMP_RNDN);

  mpfr_set_inf (x, 1);
  mpfr_out_str (fout, 10, 0, x, GMP_RNDN);

  mpfr_set_inf (x, -1);
  mpfr_out_str (fout, 10, 0, x, GMP_RNDN);

  mpfr_set_ui (x, 0, GMP_RNDN);
  mpfr_out_str (fout, 10, 0, x, GMP_RNDN);
  mpfr_neg (x, x, GMP_RNDN);
  mpfr_out_str (fout, 10, 0, x, GMP_RNDN);

  mpfr_clear (x);
}

int
main (int argc, char *argv[])
{
  int i, N=10000, p;
  mp_rnd_t rnd;
  double d;

  tests_start_mpfr ();

  /* with no argument: prints to /dev/null,
     tout_str N: prints N tests to stdout */
  if (argc == 1)
    {
      fout = fopen ("/dev/null", "w");
      /* If we failed to open this device, try with a dummy file */
      if (fout == NULL)
        fout = fopen ("mpfrtest.txt", "w");
    }
  else
    {
      fout = stdout;
      N = atoi (argv[1]);
    }

  if (fout == NULL)
    {
      printf ("Can't open /dev/null or stdout\n");
      exit (1);
    }

  special ();

  check (-1.37247529013405550000e+15, GMP_RNDN, 7);
  check (-1.5674376729569697500e+15, GMP_RNDN, 19);
  check (-5.71262771772792640000e-79, GMP_RNDU, 16);
  check (DBL_NEG_ZERO, GMP_RNDU, 7);
  check (-4.5306392613572974756e-308, GMP_RNDN, 8);
  check (-6.7265890111403371523e-165, GMP_RNDN, 4);
  check (-1.3242553591261807653e+156, GMP_RNDN, 16);
  check (-6.606499965302424244461355e233, GMP_RNDN, 10);
  check4 (1.0, GMP_RNDN, 10, 120);
  check (1.0, GMP_RNDU, 10);
  check (4.059650008e-83, GMP_RNDN, 10);
  check (-7.4, GMP_RNDN, 10);
  check (0.997, GMP_RNDN, 10);
  check (-4.53063926135729747564e-308, GMP_RNDN, 10);
  check (2.14478198760196000000e+16, GMP_RNDN, 10);
  check (7.02293374921793516813e-84, GMP_RNDN, 10);

  /* random tests */
  for (i=0;i<N;i++)
    {
      do
        {
          d = DBL_RAND ();
        }
#ifdef HAVE_DENORMS
      while (0);
#else
      while (ABS(d) < DBL_MIN);
#endif
      rnd = RND_RAND ();
      p = 2 + randlimb () % 35;
      check (d, rnd, p);
    }

  fclose (fout);

  tests_end_mpfr ();
  return 0;
}
