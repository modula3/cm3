P=""
# base libraries
[ ${TARGET} != NT386 ] && P="${P} m3gc-simple"
if syscall_wrappers_exist && [ -z "$M3GC_SIMPLE" ] ; then
  [ ${TARGET} != NT386 ] && P="${P} m3gc-enhanced"
fi
P="${P} m3core"
P="${P} libm3"
P="${P} patternmatching"

# system / compiler libraries and tools
P="${P} m3middle"
P="${P} m3quake"
P="${P} m3scanner"
P="${P} m3tools"
P="${P} m3cgcat"
P="${P} m3cggen"
#[ "${M3GDB}" = yes ] && P="${P} m3gdb" # not yet tested anywhere
P="${P} m3bundle"
[ "${M3OSTYPE}" = "WIN32" -o -n "${CM3_ALL}" ] && P="${P} mklib"
[ "${M3OSTYPE}" = "WIN32" -o -n "${CM3_ALL}" ] && P="${P} dll2lib"
[ "${M3OSTYPE}" = "WIN32" -o -n "${CM3_ALL}" ] && P="${P} fix_nl"
[ "${M3OSTYPE}" = "WIN32" -o -n "${CM3_ALL}" ] && P="${P} libdump"

# more useful quasi-standard libraries
P="${P} arithmetic"
P="${P} bitvector"
P="${P} digraph"
P="${P} parseparams"
P="${P} realgeometry"
P="${P} set"
P="${P} plplot"
P="${P} slisp"
P="${P} sortedtableextras"
P="${P} table-list"
P="${P} tempfiles"
[ "${HAVE_TCL}" = "yes" -o -n "${CM3_ALL}" ] && P="${P} tcl"
P="${P} tcp"
[ "${M3OSTYPE}" = "POSIX" -o -n "${CM3_ALL}" ] && P="${P} udp"
P="${P} libsio"
P="${P} libbuf"
P="${P} debug"
P="${P} listfuncs"
P="${P} embutils"
P="${P} m3tk-misc"
P="${P} http"
P="${P} binIO"
#P="${P} deepcopy"
#P="${P} sgml"
P="${P} commandrw"

# some CM3 communication extensions
[ "${M3OSTYPE}" = "WIN32" -o -n "${CM3_ALL}" ] && P="${P} tapi"
[ "${HAVE_SERIAL}" = "yes" -o -n "${CM3_ALL}" ] && P="${P} serial"

# tools
P="${P} m3tk"
P="${P} mtex"
P="${P} m3totex"
P="${P} m3tohtml"
P="${P} m3scan"
P="${P} m3markup"
P="${P} m3browser"
P="${P} cmpdir"
P="${P} cmpfp"
P="${P} dirfp"
P="${P} uniq"
#P="${P} pp" # needs lex and yacc or flex and bison
#P="${P} kate"   # can be shipped only on systems with KDE
#P="${P} nedit"

# network objects -- distributed programming
P="${P} netobj"
P="${P} netobjd"
P="${P} stubgen"
P="${P} events"
P="${P} rdwr"
P="${P} sharedobj"
P="${P} sharedobjgen"

# database packages
P="${P} odbc"
P="${P} postgres95"
P="${P} db"
P="${P} smalldb"
P="${P} stable"
P="${P} stablegen"

# the standard graphical user interface: trestle and formsvbt
[ "${M3OSTYPE}" != "WIN32" -o -n "${CM3_ALL}" ] && P="${P} X11R4"
P="${P} ui"
P="${P} PEX"
P="${P} vbtkit"
P="${P} cmvbt"
P="${P} jvideo"
P="${P} videovbt"
P="${P} web"
P="${P} formsvbtpixmaps"
P="${P} formsvbt"
P="${P} formsview"
P="${P} formsedit"
P="${P} codeview"
P="${P} mg"
P="${P} mgkit"
P="${P} opengl"
P="${P} anim3D"
P="${P} zeus"
P="${P} m3zume"

# obliq
P="${P} synloc"
P="${P} synex"
P="${P} metasyn"
P="${P} obliqrt"
P="${P} obliqparse"
P="${P} obliqprint"
P="${P} obliq"
P="${P} obliqlibemb"
P="${P} obliqlibm3"
P="${P} obliqlibui"
P="${P} obliqlibanim"
#P="${P} obliqlib3D" # does not compile
P="${P} obliqsrvstd"
P="${P} obliqsrvui"
P="${P} obliqbinmin"
P="${P} obliqbinstd"
P="${P} obliqbinui"
P="${P} obliqbinanim"
P="${P} visualobliq"
P="${P} vocgi"
P="${P} voquery"
P="${P} vorun"

# more graphics depending on obliq
P="${P} webvbt"

# more tools
P="${P} recordheap"
P="${P} rehearsecode"
P="${P} replayheap"
P="${P} showheap"
P="${P} shownew"
[ "${M3OSTYPE}" != "WIN32" -o -n "${CM3_ALL}" ] && P="${P} showthread"
# showthread needs ThreadEvent, which does not exist on win32

# The Juno-2 graphical constraint based editor
[ "${M3OSTYPE}" != "WIN32" -o -n "${CM3_ALL}" ] && P="${P} pkl-fonts"
[ "${M3OSTYPE}" != "WIN32" -o -n "${CM3_ALL}" ] && P="${P} juno-machine"
[ "${M3OSTYPE}" != "WIN32" -o -n "${CM3_ALL}" ] && P="${P} juno-compiler"
[ "${M3OSTYPE}" != "WIN32" -o -n "${CM3_ALL}" ] && P="${P} juno-app"

# demo programs
P="${P} cube"
P="${P} calculator"
P="${P} fisheye"
P="${P} mentor"

export P # or NetBSD's /bin/sh will fail
