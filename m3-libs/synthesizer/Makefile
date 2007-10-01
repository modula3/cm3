
PROJECT = synthesizer


.PHONY: all build install testbuild

all:    build

build:
	cm3 -O

install:
	cm3 -ship

testbuild:
	mkdir $(PROJECT)
	ln -s $(PWD)/src $(PROJECT)/src
	(cd $(PROJECT) && cm3)
	(for dir in example/* test; do \
	   (echo $$dir && \
            cd $$dir && \
	    cm3 -DTESTLIBDIR=$(PWD) -prepend $(PWD)/$$dir/src/m3testbuild); \
	 done)
	(cd test && $(M3TARGET)/test)
	rm $(PROJECT)/src
	rm -r $(PROJECT)
