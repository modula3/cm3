## Process this file with automake to generate Makefile.in

# reduced for Modula-3/gcc

# Copyright 1996, 1998, 1999, 2000, 2001, 2002, 2003 Free Software
# Foundation, Inc.
#
# This file is part of the GNU MP Library.
#
# The GNU MP Library is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 2.1 of the License, or (at your
# option) any later version.
#
# The GNU MP Library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
# the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA.


INCLUDES = -D__GMP_WITHIN_GMP -I$(top_srcdir)

noinst_LTLIBRARIES = libmpz.la
libmpz_la_SOURCES = \
  aors.h        \
  aors_ui.h     \
  add.c         \
  add_ui.c      \
  clear.c       \
  cmp.c         \
  export.c      \
  fdiv_q.c      \
  import.c      \
  init.c        \
  iset.c        \
  neg.c         \
  out_str.c     \
  realloc.c     \
  set.c         \
  set_ui.c      \
  sizeinbase.c  \
  sub.c         \
  sub_ui.c      \
  swap.c        \
  tdiv_qr.c

# These are BUILT_SOURCES at the top-level, so normally they're built before
# recursing into this directory.
#
fac_ui.h:
	cd ..; $(MAKE) $(AM_MAKEFLAGS) mpz/fac_ui.h
