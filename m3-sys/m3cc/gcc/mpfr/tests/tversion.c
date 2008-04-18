/* Test file for mpfr_version.

Copyright 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
Contributed by the Arenaire and Cacao projects, INRIA.

This file is part of the MPFR Library.

The MPFR Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The MPFR Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the MPFR Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
MA 02110-1301, USA. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mpfr-test.h"

static void
err (int d, const char *shdr, const char *slib)
{
  printf ("Incorrect MPFR version [%d] (%s header vs %s library).\n"
          "This error should have never occurred and may be due to a\n"
          "corrupted mpfr.h, an incomplete build (try to rebuild MPFR\n"
          "from scratch and/or use 'make clean'), or something wrong\n"
          "in the system.\n", d, shdr, slib);
  exit (1);
}

int
main (void)
{
  char buffer[256];
  const char *version;

  version = mpfr_get_version ();

  /* This test is disabled when a suffix (e.g. -dev) has been defined. */
#if 1
  sprintf (buffer, "%d.%d.%d", MPFR_VERSION_MAJOR, MPFR_VERSION_MINOR,
           MPFR_VERSION_PATCHLEVEL);
  if (strcmp (buffer, version) != 0)
    err (1, buffer, version);
#endif

  if (strcmp (MPFR_VERSION_STRING, version) != 0)
    err (2, MPFR_VERSION_STRING, version);

  if (__GNU_MP_VERSION_PATCHLEVEL != 0)
    sprintf (buffer, "%d.%d.%d", __GNU_MP_VERSION, __GNU_MP_VERSION_MINOR,
             __GNU_MP_VERSION_PATCHLEVEL);
  else
    sprintf (buffer, "%d.%d", __GNU_MP_VERSION, __GNU_MP_VERSION_MINOR);
  /* In some cases, it may be acceptable to have different versions for
     the header and the library, in particular when shared libraries are
     used (e.g., after a bug-fix upgrade of the library). Versioning takes
     care of that, as long as the user has a correct configuration (which
     is not always the case, hence the following warning). Moreover MPFR
     uses GMP internals, which may lead to incompatibilities even though
     GMP's public interface has not changed (the following warning is
     useful in that case too). */
  if (strcmp (buffer, gmp_version) != 0)
    printf ("The versions of gmp.h (%s) and libgmp (%s) do not seem to "
            "match.\nThis may lead to errors, in particular with MPFR. "
            "If some tests fail,\nplease check that first. As we are not "
            "sure, we do not regard this as\nan error.\n",
            buffer, gmp_version);

  return 0;
}
