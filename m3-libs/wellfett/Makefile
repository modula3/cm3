
.PHONY: all build install testbuild

all:    build

build:
	cm3 -O

install:
	cm3 -ship

testbuild:
	mkdir wellfett
	ln -s $(PWD)/src wellfett/src
	(cd wellfett && cm3)
	(cd example && cm3 -DTESTLIBDIR=$(PWD) -prepend $(PWD)/example/src/m3testbuild)
	(cd test    && cm3 -DTESTLIBDIR=$(PWD) -prepend $(PWD)/test/src/m3testbuild && $(M3TARGET)/test)
# this way we receive a
# *** runtime error:
# ***    Exception "PathnamePosix.CheckedRuntimeError" not in RAISES list
# ***    file "../src/os/POSIX/PathnamePosix.m3", line 98
# probably because of the failed path expansion.
#	(cd test && ln -s $(PWD) wellfett && cm3 -DTESTLIBDIR=$(PWD)/test -prepend $(PWD)/test/src/m3testbuild ; rm wellfett)
