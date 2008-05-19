#! /usr/bin/env python

#
# This is reasonable to build a cm3cg on a new
# target that doesn't yet have a cm3.
# It is like building gcc, but has some tweaks
# to build less, equivalent to what the m3makefile does.
# It also removes dependencies on tools that aren't
# always installed (just like m3makefile).
#

import os

def SearchPath(name, paths = os.getenv("PATH")):
  #Given a search path, find file
  if (name.find("/") != -1) or (name.find("\\") != -1):
    if os.path.isfile(name):
      return name
  if paths == "":
    return None
  (base, exts) = os.path.splitext(name)
  if not exts:
    exts = (os.getenv("PATHEXT") or "").lower()
  for ext in exts.split(";"):
    if ext == ".":
      ext = ""
    name = (base + ext)
    for path in paths.split(os.path.pathsep):
      candidate = os.path.join(path, name)
      if os.path.isfile(candidate):
        return os.path.abspath(candidate)
  return None

os.mkdir("obj.1")
os.chdir("obj.1")

#
# These tools are often not present and for
# some reason make is deciding the outputs are out of date.
# So just stub them out.
#
env = " AUTOCONF=echo \
  AUTOMAKE=echo \
  LEX='touch lex.yy.c' \
  MAKEINFO=echo "

gmake = "make"
if SearchPath("gmake"):
  gmake = "gmake"
gmake = gmake + env

def Run(a):
  print(a + "\n")
  os.system(a)

if SearchPath("gcc"):
#
# If there is already a working gcc, we can build a lot less.
#
  env = env + " CC='gcc -g' CFLAGS="

  Run("../gcc/configure \
    --disable-bootstrap \
    --disable-doc \
    --disable-fixincludes \
    --disable-intl \
    --disable-libgcc \
    --disable-libgomp \
    --disable-libmudflag \
    --disable-libssp \
    --disable-multilib \
    --disable-nls \
    --disable-shared \
    --enable-languges=m3cg \
    --enable-targets=all \
    " + env)

  Run(gmake + "all-gmp all-mpfr all-libcpp all-libdecnumber \
    all-build-libiberty all-libiberty configure-gcc")

  os.chdir("gcc")

  Run(gmake + "s-modes insn-config.h m3cg")

else:

  Run("../gcc/configure \
    --disable-doc \
    --disable-multilib \
    --disable-nls \
    --disable-shared \
    --enable-languages=m3cg \
    --enable-targets=all \
    " + env)

  Run(gmake)
