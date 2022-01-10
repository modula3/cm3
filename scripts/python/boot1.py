#! /usr/bin/env python
#
# usage: ./boot1.py target
#
# This produces an archive that can be copied to the target,
# extract, run make, producing a cm3.
#
# Copy that into place and proceed with boot2.py or boot2min.
#
# The source trees on the two systems must be "compatible",
# essentially cm3/m3core/m3middle/etc. have to match.
#
# TODO: This should really produce the entire system
# in either assembly or C.
#
# See also:
#  boot1autotools.py           - uses autoconf and automake
#  concierge.py make-bootstrap - uses cmake
#
# Boot1.py:
#   output is somewhat target-specific, just a few lines in the Makefile and Unix vs. Windows
#   output uses "just" Makefile, no autoconf/automake
#   includes Windows support
#
import sys
import os.path
import pylib
from pylib import *

def Boot():

# TODO build a directory tree, one directory per package
# TODO as a result, support building separate libraries and possibly shared libraries
# TODO and out-of-tree builds
# Maybe something autotools or cmake-laden

    global BuildLocal
    BuildLocal += " -boot -no-m3ship-resolution -group-writable -keep -DM3CC_TARGET=" + Config

    Version = CM3VERSION + "-" + time.strftime("%Y%m%d")
    BootDir = "./cm3-boot-" + BuildDir + "-" + Version

    RemoveDirectoryRecursive(BootDir)
    CreateDirectory(BootDir)

    # This information is duplicated from the config files.
    # TBD: put it only in one place.
    # The older bootstraping method does get that right.

    vms = StringContainsI(Target, "VMS")
    nt = Config.startswith("NT") or Config.endswith("NT") or Tagged(Config, "NT")
    darwin = StringContainsI(Target, "DARWIN")
    mingw = StringContainsI(Target, "MINGW")
    solaris = StringContainsI(Target, "SOLARIS")
    sol = Target.startswith("SOL")
    hpux = StringContainsI(Target, "HPUX")
    osf = StringContainsI(Target, "OSF")
    interix = StringContainsI(Target, "INTERIX")
    alpha32 = StringContainsI(Target, "ALPHA32")
    alpha64 = StringContainsI(Target, "ALPHA64")
    alpha32vms = vms and alpha32
    alpha64vms = vms and alpha64
    solsun = Config == "SOLsun"
    cygwin = StringContainsI(Target, "Cygwin")
    linux = StringContainsI(Target, "Linux")
    bsd = StringContainsI(Target, "BSD")
    haiku = StringContainsI(Target, "haiku")

    CBackend = True

    # pick the compiler

    CCompilerFlags = " "

    # TODO: This is all misplaced. It belongs in autotools, or cmake, or
    # worst case, our own code at bootstrap runtime, not in bootstrap formation.

    if alpha32vms:
        CCompiler = "c++"
        CCompilerFlags = " "
    elif alpha64vms:
        CCompiler = "c++"
        CCompilerFlags = "/pointer_size=64 "
    elif hpux:
        CCompiler = "/opt/aCC/bin/aCC"
    elif solaris or solsun:
        #CCompiler = "/usr/bin/c++"
        #CCompilerFlags = " -g -mt -xldscope=symbolic " # TODO: consider -xldscope=hidden
        CCompiler = "./c_compiler"
        CopyFile("./c_compiler", BootDir)
    elif osf:
        # There is a problem on some installs such that linking with cxx fails, unless oldcxx is used.
        # This really should be fixed otherwise. Compiling with oldcxx fails due to
        # integer constants from INT64_ with trailing i64 or ui64.
        # Not-oldcxx defines _LONGLONG to skip that.
        CCompiler = "/usr/bin/cxx" # g++ should also work all work, but change -ieee to -mieee
        CCompilerFlags = " -g -pthread -x cxx -c99 -fprm d "
        #CCompiler = "g++"
        #CCompilerFlags = " -g -pthread -mfp-rounding-mode=d "
    elif nt:
        CCompiler = "cl"
    elif haiku:
        CCompiler = "g++"
        CCompilerFlags = "-g -m64 -fPIC" # -pthread not allowed
    else: # gcc and other platforms
        CCompiler = {
            "SOLgnu" : "/usr/sfw/bin/g++",
            "AMD64_MINGW"   : "x86_64-w64-mingw32-g++",
            }.get(Config) or "g++"

        # For now, bootstrap does not build any shared libraries and -fPIC is not needed.
        # -fPIC breaks Interix and is not needed on Cygwin/Mingw.
        if not nt:
            CCompilerFlags = {
                "I386_INTERIX"  : " -g ", # gcc -fPIC generates incorrect code on Interix
                "AMD64_MINGW"   : " -g ", # No need for -pthread
                "IA64_HPUX32"   : " -g ", # -pthread is not allowed
                "IA64_HPUX64"   : " -g ", # -pthread is not allowed
                }.get(Config) or " -pthread -g "

    CCompilerFlags = CCompilerFlags + ({
        "AMD64_LINUX"     : " -m64 ",

        # 10.5.8 gcc defaults to x86.
        # 10.10.4 Yosemite defaults to amd64, so be explicit.
        "I386_DARWIN"     : " -arch i386 ",

        "AMD64_DARWIN"    : " -arch x86_64 ",
        "PPC64_DARWIN"    : " -arch ppc64 ",
        "ARM_DARWIN"      : " -march=armv6 -mcpu=arm1176jzf-s ",
        "LINUXLIBC6"      : " -m32 ",
        "I386_LINUX"      : " -m32 ",
        "IA64_HPUX32"     : " +DD32 -mt -z ",
        "IA64_HPUX64"     : " +DD64 -mt -z ",
        "MIPS64_OPENBSD"  : " -mabi=64 ",
        "I386_SOLARIS"    : " -xarch=pentium_pro -Kpic ",
        "AMD64_SOLARIS"   : " -xarch=amd64       -Kpic ",
        "SOLsun"          : " -xarch=v8plus -xcode=pic32 -xregs=no%appl ",
        "SPARC32_SOLARIS" : " -xarch=v8plus -xcode=pic32 -xregs=no%appl ",
        "SPARC64_SOLARIS" : " -xarch=v9     -xcode=pic32 -xregs=no%appl ",
        "SOLgnu"          : " -m32 -mcpu=v9 -mno-app-regs ",
        "SPARC32_LINUX"   : " -m32 -mcpu=v9 -mno-app-regs ",
        "SPARC64_LINUX"   : " -m64 -mno-app-regs ",
        }.get(Config) or " ")

    obj = ["o", "obj"][nt]
    Link = "$(CC) $(CFLAGS) *." + obj + " "
    #Link = "$(CC) $(CFLAGS)"

    # link flags TODO merge compile and link sections

    # TBD: add more and retest, e.g. Irix, AIX, HPUX, Android
    # http://www.openldap.org/lists/openldap-bugs/200006/msg00070.html
    # http://www.gnu.org/software/autoconf-archive/ax_pthread.html#ax_pthread

    # TODO: All this logic should be in the Makefile so we can make one distribution.
    # See BootAutoTools.

    if darwin:
        pass
    elif haiku:
        Link = Link + " -lnetwork "
    elif mingw:
        Link = Link  +  " -liphlpapi -lrpcrt4 -lcomctl32 -lws2_32 -lgdi32 -luser32 -ladvapi32 "
    elif solaris or sol:
        Link = Link  +  " -lpthread -lrt -lm -lnsl -lsocket -lc -pthread "
    elif hpux:
        # -ldcekt is for uuid_create in MachineIDPosixC.c
        # -luca is for getting IP from context in RTSignalC.c
        # -pthread is not allowed
        Link = Link + " -lrt -lm -lpthread -ldcekt -luca "
    elif osf:
        # There is a problem on some installs such that linking with cxx fails, unless oldcxx is used.
        # This really should be fixed otherwise.
        # Compiling with oldcxx fails, because it does not
        # define _LONGLONG and then INT64_ appends i64 or ui64 and that fails.
        Link = Link + " -lrt -lm -pthread -oldcxx "
    elif interix:
        Link = Link + " -lm -pthread "
    elif nt:
        CCompilerFlags = " -Z7 -MD -Gy -O2 -GL "
        Link = "link /incremental:no /debug /pdb:$(@R).pdb "
        Link = Link + " user32.lib kernel32.lib ws2_32.lib comctl32.lib gdi32.lib advapi32.lib netapi32.lib iphlpapi.lib "
        Link = Link + " -delayload:gdi32.dll -delayload:user32.dll -delayload:iphlpapi.dll -delayload:advapi32.dll -delayload:ws2_32.dll "
        Link = Link + " delayimp.lib -opt:ref,icf "
    elif bsd or cygwin or linux:
        Link = Link  +  " -lm -pthread " # TODO: combine with next?
    else:
        Link = Link + " -lm -lpthread -pthread "

    # add -c to compiler but not link (i.e. not CCompilerFlags)

    Compile = "$(CC) $(CFLAGS) " + [" -c ", ""][vms]

    DeleteRecursiveCommand = ["rm -rf", "rmdir /q/s"][nt]
    DeleteCommand = ["rm -f", "del /f"][nt]

    P = FilterPackages([ "m3core", "libm3", "sysutils", "set", "m3middle",
          "m3quake", "m3objfile", "m3linker", "m3back", "m3front" ])
    main_packages = ["cm3"]

    # TODO: mklib = TRUE, something is wrong with the Makefile and it is not really needed,
    # unless we want the minimal bootstrap system to be capable of crossing to NT
    #if True:
    if nt:
        main_packages += ["mklib"]
    P += main_packages

    #DoPackage(["", "realclean"] + P) or sys.exit(1)
    DoPackage(["", "buildlocal"] + P) or sys.exit(1)

    # squeeze runs of spaces and spaces at ends
    Compile = pylib._SqueezeSpaces(Compile)
    CCompilerFlags = pylib._SqueezeSpaces(CCompilerFlags)
    Link = pylib._SqueezeSpaces(Link)

    NL = ["\n", "\r\n"][nt]
    NL2 = NL + NL
    EXE = ["", ".exe"][nt]
    VmsMake  = open(os.path.join(BootDir, "vmsmake.com"), "wb")
    VmsLink  = open(os.path.join(BootDir, "vmslink.opt"), "wb")
    Makefile = open(os.path.join(BootDir, "Makefile"), "wb")
    Objects = { }
    ObjectsExceptMain = { }

    for pkg in main_packages:
        CreateDirectory(os.path.join(BootDir, pkg + ".d"))

    Makefile.write("# edit up here" + NL2
            + "CC=" + CCompiler + NL
            + "CFLAGS=" + CCompilerFlags + NL
            + "Compile=" + Compile + NL
            + "Link=" + Link + NL2
            + "# no more editing should be needed" + NL2
            + ".SUFFIXES:" + NL + ".SUFFIXES: .cpp .c .o .obj" + NL2
            + "all: ")

    for pkg in main_packages:
        Makefile.write(pkg + EXE + " ")

    Makefile.write(NL2)

    obj_suffixes = ["o", "obj"]

    Makefile.write("clean:" + NL)

    for pkg in main_packages:
        Makefile.write("\t-" + DeleteCommand + " " + pkg + " " + pkg + ".exe" + NL)

    Makefile.write(NL)

    for q in P:
        dir = GetPackagePath(q)
        for a in os.listdir(os.path.join(Root, dir, BuildDir)):
            main_leaf = a
            ext = GetPathExtension(a)
            ext_c = (ext == "c")
            ext_cpp = (ext == "cpp")
            ext_h = (ext == "h")

            if not (ext_c or ext_cpp or ext_h):
                continue
            leaf = GetLastPathElement(a)
            is_main = (not vms) and (leaf.startswith("Main.m") or leaf.startswith("Main_m.")) # TODO vms cleanup
            fullpath = os.path.join(Root, dir, BuildDir, a)
            CopyFile(fullpath, os.path.join(BootDir, [".", q + ".d"][is_main], main_leaf))
            if ext_h:
                continue
            Object = pylib._GetObjectName(a, obj)
            if Objects.get(Object):
                continue
            Objects[Object] = 1
            if not is_main:
                ObjectsExceptMain[Object] = 1
            if ext_c:
                VmsMake.write("$ " + Compile + " " + a + "\n")
            VmsLink.write(Object + "/SELECTIVE_SEARCH\n")

    # write inference rules: {c,cpp} => {o,obj}
    for c in ["c", "cpp"]:
        if nt:
            # double colon batches and is much faster, requires circa Visual C++ 5.0 or newer
            Makefile.write("." + c + ".obj::\n\t$(Compile) @<<\n$<\n<<\n\n")
        else:
            Makefile.write("." + c + ".o:\n\t$(Compile) -o $@ $<\n\n")

    Makefile.write("OBJECTS=")
    Objects = ObjectsExceptMain.keys()
    Objects.sort()
    k = 8 # worst case tab render
    for a in Objects:
        k = k + 1 + len(a)
        if k > 76: # line wrap
            Makefile.write(" \\" + NL)
            k = 1 + len(a)
        Makefile.write(" " + a)

    Makefile.write(NL2)

    LinkOut = [" -o ", " -out:"][nt]

    # To make it look better, replace double space with single space.
    if nt:
        Makefile.write("OBJECTS=$(OBJECTS:  = )" + NL2)

    for pkg in main_packages:
        # NOTE: We use *.o/*.obj to avoid command line length limits.
        # TODO: Response files? gcc 4.2 supports them. Visual C++ all
        # versions support them. TODO: Research xlc, Sun CC, etc.
        # Or, use libraries (building them from small command lines).
        #
        # Use response files at least for Visual C++ linking as we
        # are over the limits.
        if nt:
            Makefile.write(pkg + """.exe: $(OBJECTS) $(@R).d/Main.m3.cpp
	$(Compile) $(@R).d/Main.m3.cpp /Fo$(@R).d/Main.m3.obj
    $(Link) -out:$@ @<<$(@).responseFile
$(@R).d/Main.m3.obj
$(OBJECTS: =
)
<<keep

""");
        else:
            Makefile.write(pkg + EXE + ":")
            Makefile.write(" " + "$(OBJECTS) ")
            Makefile.write(pkg + ".d/Main.m3.o")
            Makefile.write(NL)
            Makefile.write("\t$(Link) " + pkg + ".d/Main.m3.o" + LinkOut + "$@" + NL2)

    for o in obj_suffixes:
        VmsMake.write("$ set file/attr=(rfm=var,rat=none) *." + o + "\n")
    VmsMake.write("$ link /executable=cm3.exe vmslink/options\n")

    for a in [Makefile, VmsMake, VmsLink]:
        a.close()

    if vms or nt:
        pylib._MakeZip(BootDir[2:])
    else:
        pylib._MakeTGZ(BootDir[2:])

CopyConfigForDevelopment() or sys.exit(1)

Boot()
