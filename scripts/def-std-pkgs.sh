P=""
# base libraries
P="${P} m3core"
P="${P} libm3"

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
P="${P} bitvector"
P="${P} digraph"
P="${P} parseparams"
P="${P} realgeometry"
P="${P} set"
P="${P} slisp"
P="${P} sortedtableextras"
P="${P} table-list"
P="${P} tempfiles"
[ "${HAVE_TCL}" = "yes" -o -n "${CM3_ALL}" ] && P="${P} tcl"
P="${P} tcp"
P="${P} libsio"

# some CM3 communication extensions
[ "${M3OSTYPE}" = "WIN32" -o -n "${CM3_ALL}" ] && P="${P} tapi"
[ "${HAVE_SERIAL}" = "yes" -o -n "${CM3_ALL}" ] && P="${P} serial"

# tools
P="${P} m3tk"
P="${P} mtex"
P="${P} m3totex"
P="${P} m3tohtml"
P="${P} cmpdir"
P="${P} cmpfp"
P="${P} dirfp"
P="${P} uniq"
#P="${P} pp" # needs lex and yacc or flex and bison

# network objects -- distributed programming
P="${P} netobj"
P="${P} netobjd"
P="${P} stubgen"

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
