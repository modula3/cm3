# -------------------------------------------------
# m3lua/Makefile
# -------------------------------------------------


SHELL = /bin/sh

# export CM3 		:=	/usr/bin/cm3
export CM3 		:=	$(HOME)/opt-m3/bin/cm3
export PROJECT_ROOT 	:= 	$(shell pwd)


# -----------------------

all: all-lua all-check

all-lua:
	@echo "CM3 = $(CM3)"
	@echo "PROJECT_ROOT = ${PROJECT_ROOT}"
	cd src ; ${CM3} -override

all-check:
	$(MAKE) -C check all 


clean:
	@echo "CM3 = $(CM3)"
	$(CM3) -realclean
	cd check ; $(MAKE) $@
	

