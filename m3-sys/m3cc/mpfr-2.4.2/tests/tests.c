/* Miscellaneous support for test programs.

Copyright 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
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

#ifdef HAVE_CONFIG_H
# if HAVE_CONFIG_H
#  include "config.h"     /* for a build within gmp */
# endif
#endif

#include <stdlib.h>
#include <float.h>
#include <errno.h>

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>  /* for struct timeval */
# include <time.h>
#elif defined HAVE_SYS_TIME_H
#  include <sys/time.h>
#else
#  include <time.h>
#endif

/* <sys/fpu.h> is needed to have union fpc_csr defined under IRIX64
   (see below). Let's include it only if need be. */
#if defined HAVE_SYS_FPU_H && defined HAVE_FPC_CSR
# include <sys/fpu.h>
#endif

#ifdef MPFR_TESTS_TIMEOUT
#include <sys/resource.h>
#endif

#include "mpfr-test.h"

#ifdef MPFR_FPU_PREC
/* This option allows to test MPFR on x86 processors when the FPU
 * rounding precision has been changed. As MPFR is a library, this can
 * occur in practice, either by the calling software or by some other
 * library or plug-in used by the calling software. This option is
 * mainly for developers. If it is used, then the <fpu_control.h>
 * header is assumed to exist and work like under Linux/x86. MPFR does
 * not need to be recompiled. So, a possible usage is the following:
 *
 *   cd tests
 *   make clean
 *   make check CFLAGS="-g -O2 -ffloat-store -DMPFR_FPU_PREC=_FPU_SINGLE"
 *
 * i.e. just add -DMPFR_FPU_PREC=... to the CFLAGS found in Makefile.
 *
 * Notes:
 *   + SSE2 (used to implement double's on x86_64, and possibly on x86
 *     too, depending on the compiler configuration and flags) is not
 *     affected by the dynamic precision.
 *   + When the FPU is set to single precision, the behavior of MPFR
 *     functions that have a native floating-point type (float, double,
 *     long double) as argument or return value is not guaranteed.
 */

#include <fpu_control.h>

static void
set_fpu_prec (void)
{
  fpu_control_t cw;

  _FPU_GETCW(cw);
  cw &= ~(_FPU_EXTENDED|_FPU_DOUBLE|_FPU_SINGLE);
  cw |= (MPFR_FPU_PREC);
  _FPU_SETCW(cw);
}

#endif

static mp_exp_t default_emin, default_emax;

static void tests_rand_start (void);
static void tests_rand_end   (void);
static void tests_limit_start (void);

/* We want to always import the function mpfr_dump inside the test
   suite, so that we can use it in GDB. But it doesn't work if we build
   a Windows DLL (initializer element is not a constant) */
#if !__GMP_LIBGMP_DLL
extern void (*dummy_func) (mpfr_srcptr);
void (*dummy_func)(mpfr_srcptr) = mpfr_dump;
#endif

void
test_version (void)
{
  const char *version;

  /* VL: I get the following error on an OpenSUSE machine, and changing
     the value of shlibpath_overrides_runpath in the libtool file from
     'no' to 'yes' fixes the problem. */

  version = mpfr_get_version ();
  if (strcmp (MPFR_VERSION_STRING, version) == 0)
    {
      char buffer[16];
      int i;

      sprintf (buffer, "%d.%d.%d", MPFR_VERSION_MAJOR, MPFR_VERSION_MINOR,
               MPFR_VERSION_PATCHLEVEL);
      for (i = 0; buffer[i] == version[i]; i++)
        if (buffer[i] == '\0')
          return;
      if (buffer[i] == '\0' && version[i] == '-')
        return;
      printf ("MPFR_VERSION_MAJOR.MPFR_VERSION_MINOR.MPFR_VERSION_PATCHLEVEL"
              " (%s)\nand MPFR_VERSION_STRING (%s) do not match!\nIt seems "
              "that the mpfr.h file has been corrupted.\n", buffer, version);
      exit (1);
    }

  printf ("Incorrect MPFR version! (%s header vs %s library)\n"
          "Nothing else has been tested since for this reason,\n"
          "any other test may fail. Please fix this one first.\n\n"
          "You can try to avoid this problem by changing the value of\n"
          "shlibpath_overrides_runpath in the libtool file and rebuild\n"
          "MPFR (make clean && make && make check).\n"
          "Otherwise this error may be due to a corrupted mpfr.h, an\n"
          "incomplete build (try to rebuild MPFR from scratch and/or\n"
          "use 'make clean'), or something wrong in the system.\n",
          MPFR_VERSION_STRING, version);
  exit (1);
}

void
tests_start_mpfr (void)
{
  test_version ();

  /* don't buffer, so output is not lost if a test causes a segv etc */
  setbuf (stdout, NULL);

#if defined HAVE_LOCALE_H && defined HAVE_SETLOCALE
  /* Added on 2005-07-09. This allows to test MPFR under various
     locales. New bugs will probably be found, in particular with
     LC_ALL="tr_TR.ISO8859-9" because of the i/I character... */
  setlocale (LC_ALL, "");
#endif

#ifdef MPFR_FPU_PREC
  set_fpu_prec ();
#endif

  tests_memory_start ();
  tests_rand_start ();
  tests_limit_start ();

  default_emin = mpfr_get_emin ();
  default_emax = mpfr_get_emax ();
}

void
tests_end_mpfr (void)
{
  int err = 0;

  if (mpfr_get_emin () != default_emin)
    {
      printf ("Default emin value has not been restored!\n");
      err = 1;
    }

  if (mpfr_get_emax () != default_emax)
    {
      printf ("Default emax value has not been restored!\n");
      err = 1;
    }

  mpfr_free_cache ();
  tests_rand_end ();
  tests_memory_end ();

  if (err)
    exit (err);
}

static void
tests_limit_start (void)
{
#ifdef MPFR_TESTS_TIMEOUT
  struct rlimit rlim[1];
  char *timeoutp;
  int timeout;

  timeoutp = getenv ("MPFR_TESTS_TIMEOUT");
  timeout = timeoutp != NULL ? atoi (timeoutp) : MPFR_TESTS_TIMEOUT;
  if (timeout > 0)
    {
      /* We need to call getrlimit first to initialize rlim_max to
         an acceptable value for setrlimit. When enabled, timeouts
         are regarded as important: we don't want to take too much
         CPU time on machines shared with other users. So, if we
         can't set the timeout, we exit immediately. */
      if (getrlimit (RLIMIT_CPU, rlim))
        {
          printf ("Error: getrlimit failed\n");
          exit (1);
        }
      rlim->rlim_cur = timeout;
      if (setrlimit (RLIMIT_CPU, rlim))
        {
          printf ("Error: setrlimit failed\n");
          exit (1);
        }
    }
#endif
}

static void
tests_rand_start (void)
{
  gmp_randstate_ptr  rands;
  char           *perform_seed;
  unsigned long  seed;

  if (__gmp_rands_initialized)
    {
      printf (
        "Please let tests_start() initialize the global __gmp_rands, i.e.\n"
        "ensure that function is called before the first use of RANDS.\n");
      exit (1);
    }

  gmp_randinit_default (__gmp_rands);
  __gmp_rands_initialized = 1;
  rands = __gmp_rands;

  perform_seed = getenv ("GMP_CHECK_RANDOMIZE");
  if (perform_seed != NULL)
    {
      seed = atoi (perform_seed);
      if (! (seed == 0 || seed == 1))
        {
          printf ("Re-seeding with GMP_CHECK_RANDOMIZE=%lu\n", seed);
          gmp_randseed_ui (rands, seed);
        }
      else
        {
#ifdef HAVE_GETTIMEOFDAY
          struct timeval  tv;
          gettimeofday (&tv, NULL);
          seed = tv.tv_sec + tv.tv_usec;
#else
          time_t  tv;
          time (&tv);
          seed = tv;
#endif
          gmp_randseed_ui (rands, seed);
          printf ("Seed GMP_CHECK_RANDOMIZE=%lu "
                  "(include this in bug reports)\n", seed);
        }
    }
  else
      gmp_randseed_ui (rands, 0x2143FEDC);
}

static void
tests_rand_end (void)
{
  RANDS_CLEAR ();
}

/* initialization function for tests using the hardware floats
   Not very useful now. */
void
mpfr_test_init (void)
{
  double d;
#ifdef HAVE_FPC_CSR
  /* to get denormalized numbers on IRIX64 */
  union fpc_csr exp;

  exp.fc_word = get_fpc_csr();
  exp.fc_struct.flush = 0;
  set_fpc_csr(exp.fc_word);
#endif
#ifdef HAVE_DENORMS
  d = DBL_MIN;
  if (2.0 * (d / 2.0) != d)
    {
      printf ("Error: HAVE_DENORMS defined, but no subnormals.\n");
      exit (1);
    }
#endif

  /* generate DBL_EPSILON with a loop to avoid that the compiler
     optimizes the code below in non-IEEE 754 mode, deciding that
     c = d is always false. */
#if 0
  for (eps = 1.0; eps != DBL_EPSILON; eps /= 2.0);
  c = 1.0 + eps;
  d = eps * (1.0 - eps) / 2.0;
  d += c;
  if (c != d)
    {
      printf ("Warning: IEEE 754 standard not fully supported\n"
              "         (maybe extended precision not disabled)\n"
              "         Some tests may fail\n");
    }
#endif
}


/* generate a random limb */
mp_limb_t
randlimb (void)
{
  mp_limb_t limb;

  _gmp_rand (&limb, RANDS, BITS_PER_MP_LIMB);
  return limb;
}

/* returns ulp(x) for x a 'normal' double-precision number */
double
Ulp (double x)
{
   double y, eps;

   if (x < 0) x = -x;

   y = x * 2.220446049250313080847263336181640625e-16 ; /* x / 2^52 */

   /* as ulp(x) <= y = x/2^52 < 2*ulp(x),
   we have x + ulp(x) <= x + y <= x + 2*ulp(x),
   therefore o(x + y) = x + ulp(x) or x + 2*ulp(x) */

   eps =  x + y;
   eps = eps - x; /* ulp(x) or 2*ulp(x) */

   return (eps > y) ? 0.5 * eps : eps;
}

/* returns the number of ulp's between a and b,
   where a and b can be any floating-point number, except NaN
 */
int
ulp (double a, double b)
{
  double twoa;

  if (a == b) return 0; /* also deals with a=b=inf or -inf */

  twoa = a + a;
  if (twoa == a) /* a is +/-0.0 or +/-Inf */
    return ((b < a) ? INT_MAX : -INT_MAX);

  return (int) ((a - b) / Ulp (a));
}

/* return double m*2^e */
double
dbl (double m, int e)
{
  if (e >=0 )
    while (e-- > 0)
      m *= 2.0;
  else
    while (e++ < 0)
      m /= 2.0;
  return m;
}

/* Warning: NaN values cannot be distinguished if MPFR_NANISNAN is defined. */
int
Isnan (double d)
{
  return (d) != (d);
}

void
d_trace (const char *name, double d)
{
  union {
    double         d;
    unsigned char  b[sizeof(double)];
  } u;
  int  i;

  if (name != NULL && name[0] != '\0')
    printf ("%s=", name);

  u.d = d;
  printf ("[");
  for (i = 0; i < (int) sizeof (u.b); i++)
    {
      if (i != 0)
        printf (" ");
      printf ("%02X", (int) u.b[i]);
    }
  printf ("] %.20g\n", d);
}

void
ld_trace (const char *name, long double ld)
{
  union {
    long double    ld;
    unsigned char  b[sizeof(long double)];
  } u;
  int  i;

  if (name != NULL && name[0] != '\0')
    printf ("%s=", name);

  u.ld = ld;
  printf ("[");
  for (i = 0; i < (int) sizeof (u.b); i++)
    {
      if (i != 0)
        printf (" ");
      printf ("%02X", (int) u.b[i]);
    }
  printf ("] %.20Lg\n", ld);
}

/* Open a file in the src directory - can't use fopen directly */
FILE *
src_fopen (const char *filename, const char *mode)
{
  const char *srcdir = getenv ("srcdir");
  char *buffer;
  FILE *f;

  if (srcdir == NULL)
    return fopen (filename, mode);
  buffer =
    (char*) (*__gmp_allocate_func) (strlen (filename) + strlen (srcdir) + 2);
  if (buffer == NULL)
    {
      printf ("src_fopen: failed to alloc memory)\n");
      exit (1);
    }
  sprintf (buffer, "%s/%s", srcdir, filename);
  f = fopen (buffer, mode);
  (*__gmp_free_func) (buffer, strlen (filename) + strlen (srcdir) + 2);
  return f;
}

void
set_emin (mp_exp_t exponent)
{
  if (mpfr_set_emin (exponent))
    {
      printf ("set_emin: setting emin to %ld failed\n", (long int) exponent);
      exit (1);
    }
}

void
set_emax (mp_exp_t exponent)
{
  if (mpfr_set_emax (exponent))
    {
      printf ("set_emax: setting emax to %ld failed\n", (long int) exponent);
      exit (1);
    }
}

/* pos is 512 times the proportion of negative numbers.
   If pos=256, half of the numbers are negative.
   If pos=0, all generated numbers are positive.
*/
void
tests_default_random (mpfr_ptr x, int pos, mp_exp_t emin, mp_exp_t emax)
{
  MPFR_ASSERTN (emin <= emax);
  MPFR_ASSERTN (emin >= MPFR_EMIN_MIN);
  MPFR_ASSERTN (emax <= MPFR_EMAX_MAX);
  /* but it isn't required that emin and emax are in the current
     exponent range (see below), so that underflow/overflow checks
     can be done on 64-bit machines. */

  mpfr_urandomb (x, RANDS);
  if (MPFR_IS_PURE_FP (x) && (emin >= 1 || (randlimb () & 1)))
    {
      mp_exp_t e;
      e = MPFR_GET_EXP (x) +
        (emin + (long) (randlimb () % (emax - emin + 1)));
      /* Note: There should be no overflow here because both terms are
         between MPFR_EMIN_MIN and MPFR_EMAX_MAX, but the sum e isn't
         necessarily between MPFR_EMIN_MIN and MPFR_EMAX_MAX. */
      if (mpfr_set_exp (x, e))
        {
          /* The random number doesn't fit in the current exponent range.
             In this case, test the function in the extended exponent range,
             which should be restored by the caller. */
          mpfr_set_emin (MPFR_EMIN_MIN);
          mpfr_set_emax (MPFR_EMAX_MAX);
          mpfr_set_exp (x, e);
        }
    }
  if (randlimb () % 512 < pos)
    mpfr_neg (x, x, GMP_RNDN);
}

/* The test_one argument is a boolean. If it is true, then the function
   is tested in only one rounding mode (the one provided in rnd) and the
   variable rndnext is not used (due to the break). If it is false, then
   the function is tested in the 4 rounding modes, and rnd must initially
   be GMP_RNDZ; thus rndnext will be initialized in the first iteration.
   gcc may give a warning about rndnext, but this is an easy and correct
   way to implement a simple queue for the rounding modes.
   As examples of use, see the calls to test4rm from the data_check and
   bad_cases functions. */
static void
test4rm (int (*fct) (FLIST), mpfr_srcptr x, mpfr_ptr y, mpfr_ptr z,
         mp_rnd_t rnd, int test_one, char *name)
{
  mp_prec_t yprec = MPFR_PREC (y);
  mp_rnd_t rndnext = GMP_RND_MAX;  /* means uninitialized */

  MPFR_ASSERTN (test_one || rnd == GMP_RNDZ);
  mpfr_set_prec (z, yprec);
  while (1)
    {
      MPFR_ASSERTN (rnd != GMP_RND_MAX);
      fct (z, x, rnd);
      if (! mpfr_equal_p (y, z))
        {
          printf ("Error for %s with xprec=%lu, yprec=%lu, rnd=%s\nx = ",
                  name, (unsigned long) MPFR_PREC (x), (unsigned long) yprec,
                  mpfr_print_rnd_mode (rnd));
          mpfr_out_str (stdout, 16, 0, x, GMP_RNDN);
          printf ("\nexpected ");
          mpfr_out_str (stdout, 16, 0, y, GMP_RNDN);
          printf ("\ngot      ");
          mpfr_out_str (stdout, 16, 0, z, GMP_RNDN);
          printf ("\n");
          exit (1);
        }
      if (test_one || rnd == GMP_RNDN)
        break;
      if (rnd == GMP_RNDZ)
        {
          if (MPFR_IS_NEG (y))
            {
              rnd = GMP_RNDU;
              rndnext = GMP_RNDD;
            }
          else
            {
              rnd = GMP_RNDD;
              rndnext = GMP_RNDU;
            }
        }
      else
        {
          rnd = rndnext;
          if (rndnext != GMP_RNDN)
            {
              rndnext = GMP_RNDN;
              mpfr_nexttoinf (y);
            }
          else
            {
              if (yprec == MPFR_PREC_MIN)
                break;
              mpfr_prec_round (y, --yprec, GMP_RNDZ);
              mpfr_set_prec (z, yprec);
            }
        }
    }
}

/* Check data in file f for function foo, with name 'name'.
   Each line consists of the file f one:

   xprec yprec rnd x y

   where:

   xprec is the input precision
   yprec is the output precision
   rnd is the rounding mode (n, z, u, d, Z)
   x is the input (hexadecimal format)
   y is the expected output (hexadecimal format) for foo(x) with rounding rnd

   If rnd is Z, y is the expected output in round-toward-zero, and the
   three directed rounding modes are tested, then the round-to-nearest
   mode is tested in precision yprec-1. This is useful for worst cases,
   where yprec is the minimum value such that one has a worst case in a
   directed rounding mode.
 */
void
data_check (char *f, int (*foo) (FLIST), char *name)
{
  FILE *fp;
  mp_prec_t xprec, yprec;
  mpfr_t x, y, z;
  mp_rnd_t rnd;
  char r;
  int c;

  fp = fopen (f, "r");
  if (fp == NULL)
    fp = src_fopen (f, "r");
  if (fp == NULL)
    {
      char *v = (char *) MPFR_VERSION_STRING;

      /* In the '-dev' versions, assume that the data file exists and
         return an error if the file cannot be opened to make sure
         that such failures are detected. */
      while (*v != '\0')
        v++;
      if (v[-4] == '-' && v[-3] == 'd' && v[-2] == 'e' && v[-1] == 'v')
        {
          printf ("Error: unable to open file '%s'\n", f);
          exit (1);
        }
      else
        return;
    }

  mpfr_init (x);
  mpfr_init (y);
  mpfr_init (z);

  while (!feof (fp))
    {
      /* skip whitespace, for consistency */
      if (fscanf (fp, " ") == EOF)
        {
          if (ferror (fp))
            {
              perror ("data_check");
              exit (1);
            }
          else
            break;  /* end of file */
        }

      if ((c = getc (fp)) == EOF)
        {
          if (ferror (fp))
            {
              perror ("data_check");
              exit (1);
            }
          else
            break;  /* end of file */
        }

      if (c == '#') /* comment: read entire line */
        {
          do
            {
              c = getc (fp);
            }
          while (!feof (fp) && c != '\n');
        }
      else
        {
          ungetc (c, fp);

          c = fscanf (fp, "%lu %lu %c", &xprec, &yprec, &r);
          if (c == EOF)
            {
              perror ("data_check");
              exit (1);
            }
          else if (c != 3)
            {
              printf ("Error: corrupted line in file '%s'\n", f);
              exit (1);
            }

          switch (r)
            {
            case 'n':
              rnd = GMP_RNDN;
              break;
            case 'z': case 'Z':
              rnd = GMP_RNDZ;
              break;
            case 'u':
              rnd = GMP_RNDU;
              break;
            case 'd':
              rnd = GMP_RNDD;
              break;
            default:
              printf ("Error: unexpected rounding mode"
                      " in file '%s': %c\n", f, (int) r);
              exit (1);
            }
          mpfr_set_prec (x, xprec);
          mpfr_set_prec (y, yprec);
          if (mpfr_inp_str (x, fp, 0, GMP_RNDN) == 0)
            {
              printf ("Error: corrupted argument in file '%s'\n", f);
              exit (1);
            }
          if (mpfr_inp_str (y, fp, 0, GMP_RNDN) == 0)
            {
              printf ("Error: corrupted result in file '%s'\n", f);
              exit (1);
            }
          if (getc (fp) != '\n')
            {
              printf ("Error: result not followed by \\n in file '%s'\n", f);
              exit (1);
            }
          /* Skip whitespace, in particular at the end of the file. */
          if (fscanf (fp, " ") == EOF && ferror (fp))
            {
              perror ("data_check");
              exit (1);
            }
          test4rm (foo, x, y, z, rnd, r != 'Z', name);
        }
    }

  mpfr_clear (x);
  mpfr_clear (y);
  mpfr_clear (z);

  fclose (fp);
}

/* Test n random bad cases. A precision py in [pymin,pymax] and
 * a number y of precision py are chosen randomly. One computes
 * x = inv(y) in precision px = py + psup (rounded to nearest).
 * Then (in general), y is a bad case for fct in precision py (in
 * the directed rounding modes, but also in the rounding-to-nearest
 * mode for some lower precision: see data_check).
 * fct, inv, name: data related to the function.
 * pos, emin, emax: arguments for tests_default_random.
 */
void
bad_cases (int (*fct)(FLIST), int (*inv)(FLIST), char *name,
           int pos, mp_exp_t emin, mp_exp_t emax,
           mp_prec_t pymin, mp_prec_t pymax, mp_prec_t psup,
           int n)
{
  mpfr_t x, y, z;
  char *dbgenv;
  int i, dbg;
  mp_exp_t old_emin, old_emax;

  old_emin = mpfr_get_emin ();
  old_emax = mpfr_get_emax ();

  dbgenv = getenv ("MPFR_DEBUG_BADCASES");
  dbg = dbgenv != 0 ? atoi (dbgenv) : 0;  /* debug level */
  mpfr_inits (x, y, z, (mpfr_ptr) 0);
  for (i = 0; i < n; i++)
    {
      mp_prec_t px, py, pz;
      int inex;

      if (dbg)
        printf ("bad_cases: i = %d\n", i);
      py = pymin + (randlimb () % (pymax - pymin + 1));
      mpfr_set_prec (y, py);
      tests_default_random (y, pos, emin, emax);
      if (dbg)
        {
          printf ("bad_cases: yprec =%4ld, y = ", (long) py);
          mpfr_out_str (stdout, 16, 0, y, GMP_RNDN);
          printf ("\n");
        }
      px = py + psup;
      mpfr_set_prec (x, px);
      mpfr_clear_flags ();
      inv (x, y, GMP_RNDN);
      if (mpfr_nanflag_p () || mpfr_overflow_p () || mpfr_underflow_p ())
        {
          if (dbg)
            printf ("bad_cases: no normal inverse\n");
          goto next_i;
        }
      if (dbg > 1)
        {
          printf ("bad_cases: x = ");
          mpfr_out_str (stdout, 16, 0, x, GMP_RNDN);
          printf ("\n");
        }
      pz = px;
      do
        {
          pz += 32;
          mpfr_set_prec (z, pz);
          if (fct (z, x, GMP_RNDN) == 0)
            {
              if (dbg)
                printf ("bad_cases: exact case\n");
              goto next_i;
            }
          if (dbg)
            {
              if (dbg > 1)
                {
                  printf ("bad_cases: %s(x) ~= ", name);
                  mpfr_out_str (stdout, 16, 0, z, GMP_RNDN);
                }
              else
                {
                  printf ("bad_cases:   [GMP_RNDZ]  ~= ");
                  mpfr_out_str (stdout, 16, 40, z, GMP_RNDZ);
                }
              printf ("\n");
            }
          inex = mpfr_prec_round (z, py, GMP_RNDN);
          if (mpfr_nanflag_p () || mpfr_overflow_p () || mpfr_underflow_p ()
              || ! mpfr_equal_p (z, y))
            {
              if (dbg)
                printf ("bad_cases: inverse doesn't match\n");
              goto next_i;
            }
        }
      while (inex == 0);
      /* We really have a bad case. */
      do
        py--;
      while (py >= MPFR_PREC_MIN && mpfr_prec_round (z, py, GMP_RNDZ) == 0);
      py++;
      /* py is now the smallest output precision such that we have
         a bad case in the directed rounding modes. */
      if (mpfr_prec_round (y, py, GMP_RNDZ) != 0)
        {
          printf ("Internal error for i = %d\n", i);
          exit (1);
        }
      if ((inex > 0 && MPFR_IS_POS (z)) ||
          (inex < 0 && MPFR_IS_NEG (z)))
        {
          mpfr_nexttozero (y);
          if (mpfr_zero_p (y))
            goto next_i;
        }
      if (dbg)
        {
          printf ("bad_cases: yprec =%4ld, y = ", (long) py);
          mpfr_out_str (stdout, 16, 0, y, GMP_RNDN);
          printf ("\n");
        }
      /* Note: y is now the expected result rounded toward zero. */
      test4rm (fct, x, y, z, GMP_RNDZ, 0, name);
    next_i:
      /* In case the exponent range has been changed by
         tests_default_random()... */
      mpfr_set_emin (old_emin);
      mpfr_set_emax (old_emax);
    }
  mpfr_clears (x, y, z, (mpfr_ptr) 0);
}
