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

# some CM3 communication extensions
[ "${M3OSTYPE}" = "WIN32" -o -n "${CM3_ALL}" ] && P="${P} tapi"
[ "${HAVE_SERIAL}" = "yes" -o -n "${CM3_ALL}" ] && P="${P} serial"

# network objects -- distributed programming
P="${P} netobj"
P="${P} netobjd"
P="${P} m3tk"
P="${P} stubgen"

# the standard graphical user interface: trestle and formsvbt
[ "${M3OSTYPE}" != "WIN32" -o -n "${CM3_ALL}" ] && P="${P} X11R4"
P="${P} ui"
P="${P} vbtkit"
P="${P} tcp"
P="${P} cmvbt"
P="${P} jvideo"
P="${P} videovbt"
P="${P} web"
P="${P} formsvbtpixmaps"
P="${P} formsvbt"
P="${P} formsview"
P="${P} formsedit"

