  http://www.openbsd.org/cgi-bin/cvsweb/ports/lang/gcc/4.3/patches/  

  http://www.openbsd.org/anoncvs.html  

  Windows:
    set CVS_RSH=ssh
    mkdir temp
    cd temp
    cvs -z 3 -qd anoncvs@anoncvs.ca.openbsd.org:/cvs get -P ports/lang/gcc/4.2
    cvs -z 3 -qd anoncvs@anoncvs.ca.openbsd.org:/cvs get -P ports/lang/gcc/4.3

  Unix:
    export CVS_RSH=ssh
    mkdir temp
    cd temp
    cvs -z 3 -qd anoncvs@anoncvs.ca.openbsd.org:/cvs get -P ports/lang/gcc/4.2
    cvs -z 3 -qd anoncvs@anoncvs.ca.openbsd.org:/cvs get -P ports/lang/gcc/4.3


The patches seem messed up in places and have been edited.
  specifically patch-gcc_config_gcc
  What to do with rs6000/openbsd1.h is unclear.
