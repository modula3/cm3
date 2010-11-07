# reduced for Modula-3/gcc

## Process this file with automake to generate Makefile.in

# Copyright 1996, 1998, 1999, 2000, 2001, 2002, 2005 Free Software Foundation,
# Inc.
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


INCLUDES = -D__GMP_WITHIN_GMP -I$(top_srcdir) \
  -DOPERATION_`echo $* | sed 's/_$$//'`

OFILES = @mpn_objects@


# All possible mpn normal and optional function files are listed here, to
# get automake to generate ansi2knr rules for each.  Such rules will be
# ignored for any that are instead implemented with a .asm (or whatever) for
# a particular target.
#
nodist_EXTRA_libmpn_la_SOURCES =	    \
  add.c                 \
  add_1.c               \
  add_n.c               \
  addmul_1.c            \
  cmp.c                 \
  dc_divrem_n.c         \
  diveby3.c             \
  divrem_1.c            \
  divrem_2.c            \
  get_str.c             \
  lshift.c              \
  mul.c                 \
  mul_1.c               \
  mul_fft.c             \
  mul_n.c               \
  mul_basecase.c        \
  pre_divrem_1.c        \
  rshift.c              \
  sb_divrem_mn.c        \
  sqr_basecase.c        \
  sub.c                 \
  sub_1.c               \
  sub_n.c               \
  submul_1.c            \
  tdiv_qr.c

noinst_LTLIBRARIES = libmpn.la
nodist_libmpn_la_SOURCES = mp_bases.c
libmpn_la_LIBADD = $(OFILES)
libmpn_la_DEPENDENCIES = $(OFILES)

TARG_DIST = generic

EXTRA_DIST = $(TARG_DIST)

mp_bases.c:
	cd ..; $(MAKE) $(AM_MAKEFLAGS) mpn/mp_bases.c
