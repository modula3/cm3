## Makefile.am for texinfo/util.
## $Id$
## Run automake in .. to produce Makefile.in from this.

noinst_PROGRAMS = install-info texindex
noinst_SCRIPTS = texi2dvi

localedir = $(datadir)/locale
INCLUDES = -I$(top_srcdir)/lib -I../intl -DLOCALEDIR=\"$(localedir)\"
LDADD = ../lib/libtxi.a @INTLLIBS@

EXTRA_DIST = README deref.c fixfonts gen-dir-node tex3patch texi2dvi \
             update-info 
