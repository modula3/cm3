# -------------------------------------------------
# m3lua/check/Makefile
# -------------------------------------------------


SHELL = /bin/sh
SUBDIRS = check1

# export CM3 		=	/usr/bin/cm3
export CHECK_ROOT 	:= 	$(shell pwd)
export PROJECT_ROOT 	:= 	$(shell dirname ${CHECK_ROOT})


# -----------------------

all:
	@echo "CM3 = ${CM3}"
	@echo "CHECK_ROOT = ${CHECK_ROOT}"
	@echo "PROJECT_ROOT = ${PROJECT_ROOT}"
	@for dir in $(SUBDIRS) ; do \
		echo "====> $${dir}" ; \
		cd $${dir} ; $(CM3) -override ; cd .. ; \
	done


clean:
	@for dir in $(SUBDIRS) ; do \
		cd $${dir} ; \
		$(CM3) -realclean ; \
	done
