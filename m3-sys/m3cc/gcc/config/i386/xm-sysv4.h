/* Configuration for GCC for Intel i386 running System V Release 4.  */

#include "i386/xm-i386.h"
#include "xm-svr4.h"

/* If not compiled with GNU C, use the portable alloca.  */
#ifndef __GNUC__
#define USE_C_ALLOCA
#endif
#ifdef __HIGHC__
#include <alloca.h>		/* for MetaWare High-C on NCR System 3000 */
#endif

/* Univel, at least, has a small ARG_MAX.  Defining this is harmless
   except for causing extra stat calls in the driver program.  */
#define SMALL_ARG_MAX

/* CYGNUS LOCAL fnf */
/* There is a declaration in /usr/include/signal.h that conflicts with
   the declaration in collect2.c, so disable gcc's declarations.  This
   is true for Unixware 2.0, but not Unixware 1.X.  However it appears
   to be OK to use this define for both versions. -fnf */
#define DONT_DECLARE_SYS_SIGLIST
/* END CYGNUS LOCAL */
