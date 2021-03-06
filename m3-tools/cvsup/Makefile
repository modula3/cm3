# Copyright 1996-2003 John D. Polstra.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. All advertising materials mentioning features or use of this software
#    must display the following acknowledgement:
#      This product includes software developed by John D. Polstra.
# 4. The name of the author may not be used to endorse or promote products
#    derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
# IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
# NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

###############################################################################
# Override the following defaults on the command line if necessary.
###############################################################################

# Base directory for installed files.
PREFIX=		/usr/local

# Modula-3 compiler.  For the SRC and PM3 compilers it is "m3build".  For
# the Critical Mass compiler use "cm3".
M3=		m3build

# Extra flags to pass to the Modula-3 compiler.
M3FLAGS=

###############################################################################
# End of configuration section.  You shouldn't need to change the rest
# of this file.
###############################################################################

SUBDIR=		suptcp suplib server client cvpasswd
SUBMAKEARGS=	PREFIX="${PREFIX}" M3="${M3}" M3FLAGS="${M3FLAGS}"

default:	all

all depend install:
	@${MAKE} subdirs ${SUBMAKEARGS} TARGET=$@

clean:
	@${MAKE} subdirs ${SUBMAKEARGS} TARGET=$@
	rm -rf config/`cat M3TARGET`
	rm -f M3TARGET

subdirs: M3TARGET
	@for i in ${SUBDIR}; do \
	  echo "===> $$i"; \
	  (cd $$i && ${MAKE} ${TARGET} ${SUBMAKEARGS}); \
	done

M3TARGET:
	@(cd config && ${M3})
