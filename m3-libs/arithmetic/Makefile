
TARGET = LINUXLIBC6

.PHONY:	all clean install lib test doc runtest

all:	lib install test doc

install:
	cm3 -ship

lib:
	cm3

doc:
	cd doc; make

test:	lib
	cd test; cm3

clean:
	cm3 -clean
#	cd test; cm3 -clean
	cd doc; make clean

runtest:	test
	cd test; $(TARGET)/Test
