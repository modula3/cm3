# CYGNUS LOCAL mpw (entire file)
# Sed commands to finish translating the GCC Unix makefile into MPW syntax.

# Remove control-Ls, they upset MPW make.
s///g

# Remove references to always-empty variables used to mark things.
/CYGNUS-LOCAL-/s/{CYGNUS-LOCAL-[a-z0-9]*}//g

# Add a bunch of definitions, mostly empty.
/^# Variables that exist for you to override.$/a\
\
xmake_file = \
tmake_file = \
build_xm_file = \
MALLOC = \
MD_DEPS = \
REAL_H = \
HOST_CC_LD = {CC_LD}\
ALL_CCLDFLAGS = \
HOST_CCLDFLAGS = \
INCLUDES_FOR_TARGET = -I: -I{srcdir} -I{srcdir}config: -I{topsrcdir}include:mpw: -I::extra-include:\

# The "target" variable is special to MPW make, avoid it.
/{target}/s/{target}/{target_canonical}/g

# Suppress the suppression of smart makes.
/^\.y\.c/d

/^libsubdir =/s/{libdir}:gcc-lib/{libdir}gcc-lib/
/^libsubdir =/s/$/:/

/^OLDAR = /s/^/#/
/^OLDAR_FLAGS = /s/^/#/
/^RANLIB_TEST = \[ -f/s/^/#/

/^GCC_FOR_TARGET/s/:xgcc -B:/xgC -B:/

/^GCC_CFLAGS/s/ -i / -I/

# Comment out the setting of version, has to be done from configure.
/version/s/^version=/# version=/

# Whack out "..." assignments.
/\.\.\./s/^\([a-zA-Z_]*= \.\.\.\)/#\1/

# Previous edits go a little overboard, undo.
/^objext = /s/"{o}"//

/^LIBGCC1 =/s/^/#/
/^EXTRA_HEADERS =/s/^/#/
/^LIB2FUNCS_EXTRA =/s/^/#/

# "MPW" is not built into GCC's flags, so we need to add it here.
/^TARGET_LIBGCC2_CFLAGS =/s/$/-DMPW -Dpascal=/

/^CROSS_FLOAT_H=/s/^/#/
/^FLOAT_H=/s/^/#/

/^MD_FILE =/s/^/#/

# Avoid collect2 for now.
/^USE_COLLECT2 = /s/ld//

# Avoid fixing includes.
/^FIXINCLUDES=/s/fixincludes/Makefile.in/
/^stmp-fixinc/s/ {FIXINCLUDES}//

/^# CYGNUS LOCAL.*SYSTEM_HEADER_DIR/,/^$/c\
	mpw-touch stmp-fixinc\

# Get rid of a weirdness.
/(CYGNUS LOCAL)/d

/case/s/` case "{HOST_\([A-Z]*\)}" in ?\\Option-x) echo {HOST_PREFIX}{HOST_\([A-Z]*\)} ;; esac `/{HOST_PREFIX}{HOST_\1}/

# The list of "precious" files is multi-line, so whack it all out.
/^#\.PRECIOUS/,/^$/d

# CHECK_TARGETS may be doubly defined, so we have to do something, but it's
# not useful to us anyway, so just get rid of all that.
/^CHECK_TARGETS =/s/^/#/
/^check \\Option-f/,/^$/d

/^ALLOCA =/s/$/ "{o}"alloca.c.o/
/^ALLOCA_FINISH =/s/true/null-command/

# Always link in low-level MPW functions.
/^LIBDEPS=/s/$/ "{o}"strerror.c.o "{o}"mpwlib.c.o/
# (should be host_prefix)
/^HOST_LIBDEPS=/s/$/ "{o}"strerror.c.o "{o}"mpwlib.c.o/
/{CLIB}/s/{CLIB}/ "{o}"strerror.c.o "{o}"mpwlib.c.o {CLIB}/

/^INCLUDES = .*$/s/$/ -i "{topsrcdir}"include:mpw: -i ::extra-include:/

/^STMP_FIXPROTO =/s/^STMP_FIXPROTO =.*$/STMP_FIXPROTO =/

# A nasty hack to reduce confusion.
/true/s/ ; @true$//

# Point to multilib.h in the objdir.
/multilib/s/"{s}"multilib\.h/"{o}"multilib.h/g

# Simplify pathname editing crud.
/echo/s/`echo "{srcdir}"\([-a-z]*\)\.c | sed '.*'`/"{s}"\1.c/

# Hacks to gcc driver building.

# Change the syntax and values of the arguments to gcc.c compilation.
/STANDARD_STARTFILE_PREFIX/s/STANDARD_STARTFILE_PREFIX=\\"{unlibsubdir}:\\"/STANDARD_STARTFILE_PREFIX={dq}{startfiledir}{dq}/
/STANDARD_EXEC_PREFIX/s/\\"/{dq}/g
/STANDARD_EXEC_PREFIX/s/{libdir}:/{libdir}/g
/DEFAULT_TARGET_VERSION/s/\\"/{dq}/g
/DEFAULT_TARGET_MACHINE/s/\\"/{dq}/g
/TOOLDIR_BASE_PREFIX/s/TOOLDIR_BASE_PREFIX=\\"{unlibsubdir}:::\\"/TOOLDIR_BASE_PREFIX={dq}{tooldirbaseprefix}{dq}/

/{MAYBE_TARGET_DEFAULT}/s/{MAYBE_TARGET_DEFAULT} \\Option-d/{MAYBE_TARGET_DEFAULT} @SEGMENT_FLAG(gcc)@ \\Option-d/

# Don't use the xgC script when dumping specs.
/-dumpspecs/s/{GCC_FOR_TARGET}/:xgcc/

# Hacks to cpp and cpplib building.

# Change the syntax of the arguments to cccp.c and cpplib.c compilation.
/GCC_INCLUDE_DIR/s/\\"/{dq}/g
/GPLUSPLUS_INCLUDE_DIR/s/\\".*\\"/{dq}{dq}/g
/LOCAL_INCLUDE_DIR/s/\\".*\\"/{dq}{includedir}{dq}/g
/CROSS_INCLUDE_DIR/s/\\"/{dq}/g
/CROSS_INCLUDE_DIR/s/{libsubdir}:/{libsubdir}/g
/TOOL_INCLUDE_DIR/s/\\".*\\"/{dq}{dq}/g

# Point to cexp.c generated in the objdir.
/cexp/s/"{s}"cexp\.c/"{o}"cexp.c/g
/cexp/s/^cexp\.c/"{o}"cexp.c/g

/ln {CCCP}/,/cp {CCCP/c\
	Duplicate -y {CCCP} cpp

# Hacks to cc1 building.

# Fix pathnames to all the generated files.
/c-parse/s/"{s}"c-parse\.\([chy]\)/"{o}"c-parse.\1/g
/c-parse/s/^c-parse\.\([chy]\)/"{o}"c-parse.\1/
/insn-/s/"{s}"insn-\([a-z]*\)\.h/"{o}"insn-\1.h/g
/insn-/s/^insn-\([a-z]*\)\.h/"{o}"insn-\1.h/
/insn-/s/"{s}"insn-\([a-z]*\)\.c/"{o}"insn-\1.c/g
/insn-/s/^insn-\([a-z]*\)\.c/"{o}"insn-\1.c/
/tmp-/s/"{s}"tmp-\([-a-z]*\)\.\([hcy]\)/"{o}"tmp-\1.\2/g
/tmp-/s/^tmp-\([-a-z]*\)\.\([hcy]\)/"{o}"tmp-\1.\2/
/bc-arity.h/s/"{s}"bc-arity\.h/"{o}"bc-arity.h/g
/bc-arity.h/s/^bc-arity\.h/"{o}"bc-arity.h/
/bc-opcode/s/"{s}"bc-opcode\.h/"{o}"bc-opcode.h/g
/bc-opcode/s/^bc-opcode\.h/"{o}"bc-opcode.h/
/bc-opname.h/s/"{s}"bc-opname\.h/"{o}"bc-opname.h/g
/bc-opname.h/s/^bc-opname\.h/"{o}"bc-opname.h/
/bi-parser.c/s/"{s}"bi-parser\.c/"{o}"bi-parser.c/g
/bi-parser.c/s/^bi-parser\.c/"{o}"bi-parser.c/
/bi-parser.h/s/"{s}"bi-parser\.h/"{o}"bi-parser.h/g
/bi-parser.h/s/^bi-parser\.h/"{o}"bi-parser.h/

/y.tab.c/s/"{s}"y\.tab\.c/"{o}"y.tab.c/g
/y.tab.c/s/^y\.tab\.c/"{o}"y.tab.c/
/y.tab.h/s/"{s}"y\.tab\.h/"{o}"y.tab.h/g
/y.tab.h/s/^y\.tab\.h/"{o}"y.tab.h/

# We could re-add the dependency, but dates might still go wrong.
#/c-gperf.h \\Option-f/s/\\Option-f .*$/\\Option-f "{s}"c-parse.gperf/
# so whack it instead.
/^"{srcdir}"c-gperf.h/,/MoveIfChange .*c-gperf.h/d

# Tweak the premade genattrtab stuff.
/cmp/s/cmp -s {PREMADE_ATTRTAB_MD} {md_file}/"{PREMADE_ATTRTAB}" != ""/
/;[ 	]/s/;[ 	]*\\Option-d$//
/^[ 	]*then[ 	]*\\Option-d$/d

# Paste in a segment spec for m68k compilers.
/{MAYBE_USE_COLLECT2}/s/{MAYBE_USE_COLLECT2} \\Option-d/{MAYBE_USE_COLLECT2} @SEGMENT_FLAG(gcc)@ \\Option-d/

/TARGET_NAME=/s/\\"/{dq}/g

# Hacks to cc1obj building.

/objc-parse/s/"{s}"objc-parse\.\([chy]\)/"{o}"objc-parse.\1/g
/objc-parse/s/^objc-parse\.\([chy]\)/"{o}"objc-parse.\1/

# Hacks to random thing building.
/SYSCALLS.c.X/s/"{s}"SYSCALLS.c.X/"{o}"SYSCALLS.c.X/g
/SYSCALLS.c.X/s/^SYSCALLS.c.X/"{o}"SYSCALLS.c.X/

/underscore/s/"{s}"underscore\.c/"{o}"underscore.c/g
/underscore/s/^underscore\.c/"{o}"underscore\.c/

/xlimits/s/"{s}"xlimits\.h/"{o}"xlimits.h/g
/xlimits/s/^xlimits\.h/"{o}"xlimits.h/

/gfloat/s/"{s}"gfloat\.h/"{o}"gfloat.h/g
/gfloat/s/^gfloat\.h/"{o}"gfloat.h/

# Just replace the entire limits.h test line.
/if {LIMITS_H_TEST}/s/^.*$/	If "`Exists "{SYSTEM_HEADER_DIR}"limits.h`" != ""/

# (should be in common translation?)
/{HOST_CC_LD} /s/$/ {EXTRALIBS}/
/{CC_LD} /s/$/ {EXTRALIBS}/

# Don't use general compiler flags (which may include definitions
# and other compiler-only bits) with linking commands.
/{CC_LD} /s/ALL_CFLAGS/ALL_CCLDFLAGS/
/{HOST_CC_LD} /s/HOST_CFLAGS/HOST_CCLDFLAGS/

# Whack out build rules that are not useful.
/^Makefile \\Option-f /,/^$/d
/^config.status \\Option-f /,/^$/d
/^fixhdr.ready \\Option-f /,/^$/d
/^multilib.h \\Option-f /,/^$/d
# (Note that MPW make is not case sensitive, and so this name
# is considered the same as "md_file".)
/^{MD_FILE} \\Option-f/,/^$/d
/^fixhdr.ready \\Options-f/,/^$/d
/^stmp-fixproto \\Options-f/,/^$/d

# Depending on config.status is not useful for us.
/config.status/s/ config.status//

# This is a stupid dependency, remove it.
/MoveIfChange/s/^\(.*\)\\Option-f\(.*\)MoveIfChange/\1\\Option-f\2/

# Repeat of stuff from generic edit.
/{s}/s/"{s}""{s}"/"{s}"/g
/{s}/s/"{s}""{srcdir}"/"{s}"/g
/{s}/s/"{srcdir}""{s}"/"{s}"/g

# MPWify some cc -c options that are on lines by themselves.
/-c/s/^\([ 	]*\)-c "\([^"]*\)"\([^ ]*\)\.c/\1 @DASH_C_FLAG@ "\2"\3.c -o "{o}"\3.c.o/
/-c/s/{CC} -c \(.*\) {out_file}$/{CC} @DASH_C_FLAG@ \1 {out_file} -o {out_object_file}/

# Change all Rez commands to use mac-gcc.r.
/{REZ}/s/"{s}"[-a-zA-Z{}]*\.r/"{s}"mac-gcc.r/

# Don't want GCC's attempt to define getpwd.
/getpwd/s/ "{o}"getpwd.c.o//

# Comment out the insn-....h etc build rules, they don't work.
/^"{o}"insn-.*\.h \\Option-f/s/^/#/
/^"{o}"insn-.*\.c \\Option-f/s/^/#/
/^"{o}"bc-.*\.h \\Option-f/s/^/#/

# Don't bother with protoize stuff.
/LANGUAGES/s/proto//
# Nor gcov, for now anyway.
/LANGUAGES/s/gcov//

# The double-$ stuff is unnecessary.
/\$/s/${\([_a-zA-Z][_a-zA-Z0-9]*}\)}/{\1}/g
/\$/s/${\([_a-zA-Z][_a-zA-Z0-9]*}\)}":"\.c\.o/{\1}.c.o/g
/\$/s/$\([_a-zA-Z]*\)/{\1}/g

# No chmod'ing ever necessary or useful.
/^\([ 	]*\)chmod/d

# Mentions of "include" dir are really to the newly created include
# in the objdir.
/include:/s/"{s}"include:/"{o}"include:/g

# Blow off the Objective-C header stuff.
/stmp-int-hdrs/s/ objc-headers//

/Set objdir `pwd`/d

# cd commands in MPW don't have quite the right semantics.
/cd {srcdir}$/d

# The trickiness to generate the include files' names is unnecessary.
/Set realfile/,/Duplicate -d -y ginclude/c\
	  Duplicate -d -y {file} :include:

# Lose the fixincludes README.
/^# Install the README/,/README-fixinc/d

# Don't bother with the lines that try to error out dynamically.
/if \[ {}? -eq 0 ]/d

# Undo MPWizations for when GCC is used.
/{GCC_FOR_TARGET}/s/ -d / -D/g
/{GCC_FOR_TARGET}/s/ -i / -I/g
/{GCC_FOR_TARGET}/s/ {INCLUDES}/ {INCLUDES_FOR_TARGET}/g

# A sleazy temporary hack to work around PowerMac GCC bug.
/-g1/s/ -g1//

# The first-stage or host AR may have to be different from the target AR.
/libgcc2\.a/s/{AR}/{AR_FOR_TARGET}/
/libgcc2\.a/s/{AR_FLAGS}/{AR_FOR_TARGET_FLAGS}/

# Call it .a instead of .o, like MPW C host libraries.
/libgcc.o/s/libgcc.o/libgcc.a/g

# Just replace the libgcc.a gluing action entire.
/^libgcc.a \\Option-f/,/^$/c\
\
libgcc.a \\Option-f {LIBGCC1} {LIBGCC2}\
	Delete -i -y tmplibgcc.o tmplibgcc.a libgcc.o libgcc.a tmpcopy\
	NewFolder tmpcopy\
	If "{LIBGCC1}" != ""\
                Set savedir "`Directory`"\
                SetDirectory "{o}"tmpcopy\
                :::binutils:ar x "{savedir}"{LIBGCC1}\
                SetDirectory "{savedir}"\
        End If\
        Set savedir "`Directory`"\
        SetDirectory "{o}"tmpcopy\
        :::binutils:ar x "{savedir}"{LIBGCC2}\
        For name In \\Option-x.o\
                :::binutils:ar qc tmplibgcc.a {name}\
        End For\
        :::binutils:ranlib tmplibgcc.a\
        SetDirectory "{savedir}"\
        Move -y :tmpcopy:tmplibgcc.a libgcc.a

/LIB1ASMSRC/s/"{srcdir}"config:{LIB1ASMSRC}/{LIB1ASMSRC}/

# Extra runs of "else true [fi]" translation, the generic sed file misses
# some of these.

/^[ 	]*else[ 	]*true[; 	]*fi$/c\
	Else\
		mpw-true\
	End If

/^[ 	]*else[ 	]*true[ 	]*$/c\
	Else\
		mpw-true

# Add the option for a segment flag to the cpp compile.
/cccp/s/cccp\.c -o /cccp.c @SEGMENT_FLAG(cccp)@ -o /

/^install-dir \\Option-f /,/^$/d
/^install-prog \\Option-f /,/^$/d
/^install-headers \\Option-f /,/^$/d

/^install \\Option-f /,/^$/c\
install \\Option-f all install-only\
\
install-only \\Option-f install-dir install-prog install-headers\
\
install-dir \\Option-f\
	If "`Exists "{prefix}"`" == ""\
		Echo "{prefix}" does not exist, cannot install anything\
		Exit 1\
	End If\
	If "`Exists "{bindir}"`" == ""\
		NewFolder "{bindir}"\
	End If\
	If "`Exists "{libdir}"`" == ""\
		NewFolder "{libdir}"\
	End If\
	If "`Exists "{libdir}gcc-lib:"`" == ""\
		NewFolder "{libdir}gcc-lib:"\
	End If\
	If "`Exists "{libdir}gcc-lib:{target_canonical}"`" == ""\
		NewFolder "{libdir}gcc-lib:{target_canonical}"\
	End If\
	If "`Exists "{libsubdir}"`" == ""\
		NewFolder "{libsubdir}"\
	End If\
	If "`Exists "{libsubdir}include:"`" == ""\
		NewFolder "{libsubdir}include:"\
	End If\
\
install-prog \\Option-f\
	sed -e '/xgcc/s/xgcc/gcc/' :xgC > "{bindir}"gC\
	# Suppress any cfrg resource, since this is just a script.\
	{REZ} -u WANT_CFRG "{s}"mac-gcc.r -o "{bindir}"gC -append -d VERSION_STRING='"'{version}'"'\
	Duplicate -y :xgcc "{bindir}"gcc\
	# (should) Install elements of the gcc library.\
	Duplicate -y :cpp "{libsubdir}"cpp\
	Duplicate -y :cc1 "{libsubdir}"cc1\
	If "`Exists ":cc1plus"`" != ""\
		Duplicate -y :cc1plus "{libsubdir}"cc1plus\
	End If\
	:xgcc -dumpspecs > "{libsubdir}"specs\
	Duplicate -y :libgcc.a "{libsubdir}"libgcc.a\
\
install-headers \\Option-f\
	Duplicate -y :include:\\Option-x.h "{libsubdir}"include:\



# Add compile rule for needed files from libiberty, and
# dependency for underscore.c.o, and
# artificial targets to rebuild stamped things.
$a\
\
"{o}"strerror.c.o \\Option-f "{srcroot}"libiberty:strerror.c \
   {CC} @DASH_C_FLAG@ "{srcroot}"libiberty:strerror.c {ALL_CFLAGS} -d IN_GCC {INCLUDES} -i "{srcroot}"include @SEGMENT_FLAG(strerror)@ -o "{o}"strerror.c.o\
\
"{o}"mpwlib.c.o \\Option-f "{srcroot}"libiberty:mpw.c \
   {CC} @DASH_C_FLAG@ "{srcroot}"libiberty:mpw.c {ALL_CFLAGS} -d IN_GCC {INCLUDES} -i "{srcroot}"include @SEGMENT_FLAG(mpwlib)@ -o "{o}"mpwlib.c.o\
\
"{o}"underscore.c.o \\Option-f "{o}"underscore.c\
\
lang.start.encap \\Option-f _oldest\
lang.rest.encap \\Option-f _oldest\
\
stamps-h \\Option-f \\Option-d\
	stamp-config \\Option-d\
	stamp-flags \\Option-d\
	stamp-codes \\Option-d\
	stamp-attr \\Option-d\
	stamp-bcarity \\Option-d\
	stamp-bcopcode \\Option-d\
	stamp-bcopname\
\
stamps-c \\Option-f \\Option-d\
	stamp-emit \\Option-d\
	stamp-recog \\Option-d\
	stamp-opinit \\Option-d\
	stamp-extract \\Option-d\
	stamp-peep \\Option-d\
	stamp-attrtab \\Option-d\
	stamp-output\

