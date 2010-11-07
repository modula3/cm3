## Process this file with automake to generate Makefile.in

# reduced for Modula-3/gcc

# Copyright 1991, 1993, 1994, 1996, 1997, 1999, 2000, 2001, 2002, 2003, 2004,
# 2006 Free Software Foundation, Inc.
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


## The following options are the same as AM_INIT_AUTOMAKE in configure.in,
## except no $(top_builddir) on ansi2knr.  That directory is wanted for the
## Makefiles in subdirectories, but here we must omit it so automake gives
## the actual ansi2knr build rule, not "cd $(top_builddir) && make ansi2knr".
##
AUTOMAKE_OPTIONS = 1.8 gnu no-dependencies


# Libtool -version-info for libgmp.la and libmp.la.  See "Versioning" in the
# libtool manual.
#
# 1. No interfaces changed, only implementations (good): Increment REVISION.
#
# 2. Interfaces added, none removed (good): Increment CURRENT, increment
#    AGE, set REVISION to 0.
#
# 3. Interfaces removed (BAD, breaks upward compatibility): Increment
#    CURRENT, set AGE and REVISION to 0.
#
# Do this separately for libgmp, libgmpxx and libmp, and only for releases.
#
#	  GMP	   -version-info
#       release   libgmp  libgmpxx libmp
#        2.0.x      -        -       -
#        3.0      3:0:0      -     3:0:0
#        3.0.1    3:1:0      -     3:0:0
#        3.1      4:0:1      -     4:0:1
#        3.1.1    4:1:1      -     4:1:1
#        4.0      5:0:2    3:0:0   4:2:1
#        4.0.1    5:1:2    3:1:0   4:3:1
#        4.1      6:0:3    3:2:0   4:4:1
#        4.1.1    6:1:3    3:3:0   4:5:1
#        4.1.2    6:2:3    3:4:0   4:6:1
#        4.1.3    6:3:3    3:5:0   4:7:1
#        4.1.4    6:3:3    3:5:0   4:7:1	WRONG, same as 4.1.3!
#        4.2      6:0:3    3:2:0   4:4:1	REALLY WRONG, same as 4.1!
#        4.2.1    7:1:4    4:1:1   4:10:1
#
# Starting at 3:0:0 is a slight abuse of the versioning system, but it
# ensures we're past soname libgmp.so.2, which was used on Debian GNU/Linux
# packages of gmp 2.  Pretend gmp 2 was 2:0:0, so the interface changes for
# gmp 3 mean 3:0:0 is right.
#
# We interpret "implementation changed" in item "1." above as meaning any
# release, ie. the REVISION is incremented every time (if nothing else).
# Even if we thought the code generated will be identical on all systems,
# it's still good to get the shared library filename (like
# libgmpxx.so.3.0.4) incrementing, to make it clear which GMP it's from.

LIBGMP_LT_CURRENT =  7
LIBGMP_LT_REVISION = 1
LIBGMP_LT_AGE =      4

SUBDIRS = mpn mpz

EXTRA_DIST = configfsf.guess configfsf.sub .gdbinit INSTALL.autoconf

# gmp.h and mp.h are architecture dependent, mainly since they encode the
# limb size used in libgmp.  For that reason they belong under $exec_prefix
# not $prefix, strictly speaking.
#
# $exec_prefix/include is not in the default include path for gcc built to
# the same $prefix and $exec_prefix, which might mean gmp.h is not found,
# but anyone knowledgable enough to be playing with exec_prefix will be able
# to address that.
#
includeexecdir = $(exec_prefix)/include
include_HEADERS = 
nodist_includeexec_HEADERS = gmp.h 
lib_LTLIBRARIES = libgmp.la 

BUILT_SOURCES = gmp.h

DISTCLEANFILES = $(BUILT_SOURCES) config.m4 @gmp_srclinks@

# Tell gmp.h it's building gmp, not an application, used by windows DLL stuff.
INCLUDES=-D__GMP_WITHIN_GMP

MPZ_OBJECTS =           \
  mpz/add.lo          \
  mpz/add_ui.lo       \
  mpz/clear.lo        \
  mpz/cmp.lo          \
  mpz/export.lo       \
  mpz/fdiv_q.lo       \
  mpz/import.lo       \
  mpz/init.lo         \
  mpz/iset.lo         \
  mpz/neg.lo          \
  mpz/out_str.lo      \
  mpz/realloc.lo      \
  mpz/set.lo          \
  mpz/set_ui.lo       \
  mpz/sizeinbase.lo   \
  mpz/sub.lo          \
  mpz/sub_ui.lo       \
  mpz/swap.lo         \
  mpz/tdiv_qr.lo

MPN_OBJECTS = mpn/mp_bases.lo

# In libtool 1.5 it doesn't work to build libgmp.la from the convenience
# libraries like mpz/libmpz.la.  Or rather it works, but it ends up putting
# PIC objects into libgmp.a if shared and static are both built.  (The PIC
# objects go into mpz/.libs/libmpz.a, and thence into .libs/libgmp.a.)
#
# For now the big lists of objects above are used.  Something like mpz/*.lo
# would probably work, but might risk missing something out or getting
# something extra.  The source files for each .lo are listed in the
# Makefile.am's in the subdirectories.
#
# Currently, for libgmp, unlike libmp below, we're not using
# -export-symbols, since the tune and speed programs, and perhaps some of
# the test programs, want to access undocumented symbols.

libgmp_la_SOURCES = \
  gmp-impl.h    \
  longlong.h    \
  assert.c      \
  errno.c       \
  memory.c      \
  mp_bpl.c      \
  mp_clz_tab.c  \
  version.c
EXTRA_libgmp_la_SOURCES = \
  tal-reent.c
libgmp_la_DEPENDENCIES = \
   @TAL_OBJECT@         \
   $(MPZ_OBJECTS)       \
   $(MPN_OBJECTS)       \
   @mpn_objs_in_libgmp@	
libgmp_la_LIBADD = $(libgmp_la_DEPENDENCIES)
libgmp_la_LDFLAGS = $(GMP_LDFLAGS) $(LIBGMP_LDFLAGS) \
  -version-info $(LIBGMP_LT_CURRENT):$(LIBGMP_LT_REVISION):$(LIBGMP_LT_AGE)

install-data-hook:


## The ansi2knr setups for the build programs are the same as the normal
## automake ansi2knr rules, but using $(CC_FOR_BUILD) instead of $(CC).
#
# The "test -f" support for srcdir!=builddir is similar to the automake .c.o
# etc rules, but with each foo.c explicitly, since $< is not portable
# outside an inference rule.
#
# A quoted 'foo.c' is used with the "test -f"'s to avoid Sun make rewriting
# it as part of its VPATH support.  See the autoconf manual "Limitations of
# Make".
#
# Generated .h files which are used by gmp-impl.h are BUILT_SOURCES since
# they must exist before anything can be compiled.
#
# Other generated .h files are also BUILT_SOURCES so as to get all the
# build-system stuff over and done with at the start.  Also, dependencies on
# the .h files are not properly expressed for the various objects that use
# them.

EXTRA_DIST += dumbmp.c

mp_bases.h: gen-bases$(EXEEXT_FOR_BUILD)
	./gen-bases header $(BITS_PER_MP_LIMB) $(GMP_NAIL_BITS) >mp_bases.h || (rm -f mp_bases.h; exit 1)
BUILT_SOURCES += mp_bases.h

mpn/mp_bases.c: gen-bases$(EXEEXT_FOR_BUILD)
	./gen-bases table $(BITS_PER_MP_LIMB) $(GMP_NAIL_BITS) >mpn/mp_bases.c || (rm -f mpn/mp_bases.c; exit 1)
BUILT_SOURCES += mpn/mp_bases.c

gen-bases$(EXEEXT_FOR_BUILD): gen-bases.c dumbmp.c
	$(CC_FOR_BUILD) $(srcdir)/gen-bases.c -o gen-bases$(EXEEXT_FOR_BUILD) $(LIBM_FOR_BUILD)
DISTCLEANFILES += gen-bases$(EXEEXT_FOR_BUILD)
EXTRA_DIST += gen-bases.c

# Avoid: CVS - cvs directories
#        *~  - emacs backups
#        .#* - cvs merge originals
#
# *~ and .#* only occur when a whole directory without it's own Makefile.am
# is distributed, like "doc" or the mpn cpu subdirectories.
#
dist-hook:
	-find $(distdir) \( -name CVS -type d \) -o -name "*~" -o -name ".#*" \
		| xargs rm -rf
