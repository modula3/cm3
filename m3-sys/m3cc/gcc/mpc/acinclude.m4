##### http://autoconf-archive.cryp.to/ax_c_check_flag.html
#
# SYNOPSIS
#
#
AX_C_CHECK_FLAG(FLAG-TO-CHECK,[PROLOGUE],[BODY],[ACTION-IF-SUCCESS],[ACTION-IF-FAILURE])
#
# DESCRIPTION
#
#   This macro tests if the C compiler supports the flag FLAG-TO-CHECK.
#   If successfull execute ACTION-IF-SUCCESS otherwise
#   ACTION-IF-FAILURE. PROLOGUE and BODY are optional and should be
#   used as in AC_LANG_PROGRAM macro.
#
#   This code is inspired from KDE_CHECK_COMPILER_FLAG macro. Thanks to
#   Bogdan Drozdowski <bogdandr@op.pl> for testing and bug fixes.
#
# LAST MODIFICATION
#
#   2007-11-26
#
# COPYLEFT
#
#   Copyright (c) 2007 Francesco Salvestrini <salvestrini@users.sourceforge.net>
#
#   This program is free software; you can redistribute it and/or
#   modify it under the terms of the GNU General Public License as
#   published by the Free Software Foundation; either version 2 of the
#   License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#   General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
#   02111-1307, USA.
#
#   As a special exception, the respective Autoconf Macro's copyright
#   owner gives unlimited permission to copy, distribute and modify the
#   configure scripts that are the output of Autoconf when processing
#   the Macro. You need not follow the terms of the GNU General Public
#   License when using or distributing such scripts, even though
#   portions of the text of the Macro appear in them. The GNU General
#   Public License (GPL) does govern all other use of the material that
#   constitutes the Autoconf Macro.
#
#   This special exception to the GPL applies to versions of the
#   Autoconf Macro released by the Autoconf Macro Archive. When you
#   make and distribute a modified version of the Autoconf Macro, you
#   may extend this special exception to the GPL to apply to your
#   modified version as well.

AC_DEFUN([AX_C_CHECK_FLAG],[
  AC_PREREQ([2.61])
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([AC_PROG_SED])

  flag=`echo "$1" | $SED 'y% .=/+-(){}<>:*,%_______________%'`

  AC_CACHE_CHECK([whether the C compiler accepts the $1 flag],
    [ax_cv_c_check_flag_$flag],[

    AC_LANG_PUSH([C])

    save_CFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS $1"

    AC_LINK_IFELSE([
      AC_LANG_PROGRAM([$2],[$3])
    ],[
      eval "ax_cv_c_check_flag_$flag=yes"
    ],[
      eval "ax_cv_c_check_flag_$flag=no"
    ])

    CFLAGS="$save_CFLAGS"

    AC_LANG_POP

  ])

  AS_IF([eval "test \"`echo '$ax_cv_c_check_flag_'$flag`\" = yes"],[
   :
    $4
  ],[
   :
    $5
  ])
])


#
# SYNOPSIS
#
#
MPC_PROG_CC_WARNINGFLAG([CFLAG-VAR])
#
# DESCRIPTION
#
# For development version only: Checks if gcc accepts warning flags.
# Put accepted ones into CFLAG-VAR.
#
AC_DEFUN([MPC_PROG_CC_WARNINGCFLAGS], [
  AC_REQUIRE([AC_PROG_GREP])
  if echo $VERSION | grep -c dev >/dev/null 2>&1 ; then
    if test x$GCC = xyes ; then
      case $host in
         *darwin*) ;;
         *) AX_C_CHECK_FLAG(-D_FORTIFY_SOURCE=2,,,$1="$$1 -D_FORTIFY_SOURCE=2",) ;;
      esac
      AX_C_CHECK_FLAG(-pedantic,,,$1="$$1 -pedantic",)
      AX_C_CHECK_FLAG(-Wno-long-long,,,$1="$$1 -Wno-long-long",)
      AX_C_CHECK_FLAG(-Wall,,,$1="$$1 -Wall",)
      AX_C_CHECK_FLAG(-Wextra,,,$1="$$1 -Wextra",)
      AX_C_CHECK_FLAG(-Werror,,,$1="$$1 -Werror",)
      AC_SUBST($1)
    fi
  fi
])


#
# SYNOPSIS
#
#
MPC_GMP_CC_CFLAGS
#
# DESCRIPTION
#
# Checks if CC and CFLAGS can be extracted from gmp.h
#
AC_DEFUN([MPC_GMP_CC_CFLAGS], [
   AC_MSG_CHECKING(for CC and CFLAGS in gmp.h)
   # Get CC
   echo "#include \"gmp.h\"" >  conftest.c
   echo "MPC_OPTION __GMP_CC"           >> conftest.c
   GMP_CC=`$CPP $CPPFLAGS conftest.c 2> /dev/null | $EGREP MPC_OPTION | $SED -e 's/MPC_OPTION //g' | $SED -e 's/"//g'`
   #Get CFLAGS
   echo "#include \"gmp.h\"" >  conftest.c
   echo "MPC_OPTION __GMP_CFLAGS"           >> conftest.c
   GMP_CFLAGS=`$CPP $CPPFLAGS conftest.c 2> /dev/null | $EGREP MPC_OPTION | $SED -e 's/MPC_OPTION //g'| $SED -e 's/"//g'`
   rm -f conftest.c
   if test "x$GMP_CFLAGS" = "x__GMP_CFLAGS" -o "x$GMP_CC" = "x__GMP_CC" ; then
      AC_MSG_RESULT(no)
      GMP_CFLAGS=
      GMP_CC=
   else
      AC_MSG_RESULT(yes [CC=$GMP_CC CFLAGS=$GMP_CFLAGS])
   fi

   dnl Check for validity of CC and CFLAGS obtained from gmp.h
   if test -n "$GMP_CFLAGS" ; then
      old_cflags=$CFLAGS
      old_cc=$CC
      CFLAGS=$GMP_CFLAGS
      CC=$GMP_CC
      AC_MSG_CHECKING(for CC=$GMP_CC and CFLAGS=$GMP_CFLAGS)
      AC_COMPILE_IFELSE(AC_LANG_PROGRAM([[/*hello*/]],[[/*world*/]]), [
         AC_MSG_RESULT(yes)
         ], [
         AC_MSG_RESULT(no)
         CFLAGS=$old_cflags
         CC=$old_cc
      ])
      dnl CC may have changed. Recheck for GCC.
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
   #ifndef __GNUC__
   #error "GCC Not Found"
   error
   #endif
      ]])], [
      GCC=yes
      ], [
      GCC=no
      ])
   fi

])


#
# SYNOPSIS
#
#
MPC_WINDOWS
#
# DESCRIPTION
#
# Additional checks on windows
# libtool requires "-no-undefined" for win32 dll
# It also disables the tests involving the linking with LIBGMP if DLL
#
AC_DEFUN([MPC_WINDOWS], [
   AC_MSG_CHECKING(for DLL/static GMP)
   if test "$enable_shared" = yes; then
     LDFLAGS="$LDFLAGS -no-undefined"
     AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include "gmp.h"
#if !__GMP_LIBGMP_DLL
#error
error
#endif
     ]], [[]])],[AC_MSG_RESULT(DLL)],[
  AC_MSG_RESULT(static)
  AC_MSG_ERROR([gmp.h isn't a DLL: use --enable-static --disable-shared]) ])
     AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include "mpfr.h"
#if !__GMP_LIBGMP_DLL
#error
error
#endif
     ]], [[]])],[AC_MSG_RESULT(DLL)],[
  AC_MSG_RESULT(static)
  AC_MSG_ERROR([gmp.h isn't a DLL: use --enable-static --disable-shared]) ])
   else
     AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include "gmp.h"
#if __GMP_LIBGMP_DLL
#error
error
#endif
     ]], [[]])],[AC_MSG_RESULT(static)],[
  AC_MSG_RESULT(DLL)
  AC_MSG_ERROR([gmp.h is a DLL: use --disable-static --enable-shared]) ])
  fi
  ;;
])
