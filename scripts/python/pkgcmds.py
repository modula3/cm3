import copy
from sysinfo import *
import sysinfo
import sys
import os

# define build and ship programs for Critical Mass Modula-3

CM3_BUILDLOCAL = BUILDLOCAL or "%(CM3)s -build -override -DROOT='%(CM3ROOT)s' %(BUILDARGS)s" % vars()
CM3_CLEANLOCAL = CLEANLOCAL or "%(CM3)s -clean -build -override -DROOT='%(CM3ROOT)s' %(CLEANARGS)s" % vars()
CM3_BUILDGLOBAL = BUILDGLOBAL or "%(CM3)s -build -DROOT='%(CM3ROOT)s' %(BUILDARGS)s" % vars()
CM3_CLEANGLOBAL = CLEANGLOBAL or "%(CM3)s -clean -DROOT='%(CM3ROOT)s' %(CLEANARGS)s" % vars()
CM3_SHIP = SHIP or "%(CM3)s -ship -DROOT='%(CM3ROOT)s' %(CLEANARGS)s" % vars()

# define build and ship programs for Poly. Modula-3 from Montreal

PM3_BUILDLOCAL = BUILDLOCAL or "%(M3BUILD)s -O -DROOT='%(CM3ROOT)s' %(BUILDARGS)s" % vars()
PM3_CLEANLOCAL = CLEANLOCAL or "%(M3BUILD)s clean -O -DROOT='%(CM3ROOT)s' %(CLEANARGS)s" % vars()
PM3_BUILDGLOBAL = BUILDGLOBAL or "%(M3BUILD)s -DROOT='%(CM3ROOT)s' %(BUILDARGS)s)s" % vars()
PM3_CLEANGLOBAL = CLEANGLOBAL or "%(M3BUILD)s clean -DROOT='%(CM3ROOT)s' %(CLEANARGS)s" % vars()
PM3_SHIP = SHIP or "%(M3SHIP)s -DROOT='%(CM3ROOT)s' %(SHIPARGS)s" % vars()

# define build and ship programs for DEC SRC Modula-3

SRC_BUILDLOCAL = BUILDLOCAL or "%(M3BUILD)s -O -DROOT='%(CM3ROOT)s' %(BUILDARGS)s" % vars()
SRC_CLEANLOCAL = CLEANLOCAL or "%(M3BUILD)s clean -O -DROOT='%(CM3ROOT)s' %(CLEANARGS)s" % vars()
SRC_BUILDGLOBAL = BUILDGLOBAL or "%(M3BUILD)s -DROOT='%(CM3ROOT)s' %(BUILDARGS)s" % vars()
SRC_CLEANGLOBAL = CLEANGLOBAL or "%(M3BUILD)s clean -DROOT='%(CM3ROOT)s' %(CLEANARGS)s" % vars()
SRC_SHIP = SHIP or "%(M3SHIP)s -DROOT='%(CM3ROOT)s' %(SHIPARGS)s" % vars()

# other commands

if (os.name == "nt"):
    REALCLEAN = REALCLEAN or "if exist %(TARGET)s rmdir /q/s %(TARGET)s" % vars()
else:
    REALCLEAN = REALCLEAN or "rm -rf %(TARGET)s" % vars()

# choose the compiler to use

if (SearchPath(CM3)):
    BUILDLOCAL = CM3_BUILDLOCAL
    CLEANLOCAL = CM3_CLEANLOCAL
    BUILDGLOBAL = CM3_BUILDGLOBAL
    CLEANGLOBAL = CM3_CLEANGLOBAL
    SHIP = CM3_SHIP
elif (SearchPath(M3BUILD)):
    BUILDLOCAL = PM3_BUILDLOCAL
    CLEANLOCAL = PM3_CLEANLOCAL
    BUILDGLOBAL = PM3_BUILDGLOBAL
    CLEANGLOBAL = PM3_CLEANGLOBAL
    SHIP = CM3_SHIP
else:
    if (not BUILDLOCAL or not BUILDGLOBAL or not SHIP):
        sys.stderr.write("%(CM3)s or %(M3BUILD)s not found in your path, don't know how to compile\n" % vars())
        sys.exit(1)

BUILDLOCAL = BUILDLOCAL.strip()
CLEANLOCAL = CLEANLOCAL.strip()
BUILDGLOBAL = BUILDGLOBAL.strip()
CLEANGLOBAL = CLEANGLOBAL.strip()
SHIP = SHIP.strip()

#
# propagate any module locals back to sysinfo
#
for a in sysinfo.Globals:
    exec("sysinfo.%s = %s" % (a, a))

def map_action(args):
    arg = "build"
    for a in args:
        if (not a.startswith("-")):
            arg = a
            break
    ACTION = {
        "build": BUILDLOCAL,
        "buildlocal": BUILDLOCAL,
        "buildglobal": BUILDGLOBAL + " && " + SHIP,
        "buildship": BUILDGLOBAL + " && " + SHIP,
        "ship": SHIP,
        "clean": CLEANLOCAL,
        "cleanlocal": CLEANLOCAL,
        "cleanglobal": CLEANGLOBAL,
        "realclean": REALCLEAN,
    }.get(arg)
    if (not ACTION):
        if (IGNORE_MISS):
            ACTION = BUILDLOCAL
        else:
            sys.stderr.write("unknown action %s\n" % arg)
            sys.exit(1)
    return ACTION

def add_action_opts(args):
    arg = "build"
    for a in args:
        if (not a.startswith("-")):
            arg = a
            break
    ARGS = {
        "clean": "-k",
        "cleanlocal": "-k",
        "cleanglobal": "-k",
        "realclean": "-k",
    }.get(arg, "")
    return ARGS

def extract_options(args):
    RES = ""
    for a in args:
        if (a.startswith("-")):
            if (RES):
                RES += " "
            RES += a
    return RES

def get_args(args):
    ARGS = "" # This should be changed to a list.
    i = 0
    j = len(args)
    while (i != j):
        if (not args[i].startswith("-")):
            break;
        i += 1        
    if ((i != j)
            and (args[i] in {
                "build": None,
                "buildlocal": None,
                "buildglobal": None,
                "buildship": None,
                "ship": None,
                "clean": None,
                "cleanlocal": None,
                "cleanglobal": None,
                "realclean": None,
            })):
        i += 1
    while (i != j):
        arg = args[i]
        if (arg.startswith("-")):
            sys.stderr.write("encountered option after command: %s (%s)\n" % (arg, i))
        else:
            if (ARGS):
                ARGS += " "
            ARGS += arg
        i += 1;
    return ARGS

GEN_CMDS = """
  build | buildlocal          build a package with local overrides (default)
  buildglobal | buildship     build a package without overrides and ship it
  ship                        ship a package
  clean | cleanlocal          clean a package with local overrides
  cleanglobal                 clean a package without overrides
  realclean                   remove the TARGET directory of a package
"""

GEN_OPTS = """
  -n                          no action (do not execute anything)
  -k                          keep going (ignore errors if possible)
"""

def format_one(width, string):
    return ("%-*s" % (width, string))

def print_list(strings):
    for string in strings:
        print("  " + string)

def print_list2(strings):
    if (len(strings) % 2 != 0):
        strings = copy.copy(strings) # unfortunate expense
        strings.append("")       
    width = 36
    for i in range(0, len(strings) / 2):
        print(
            "  "
            + format_one(width, strings[i * 2])
            + format_one(width, strings[i * 2 + 1])
            )

def print_list4(strings):
    width = 18
    if (len(strings) % 4 != 0):
        strings = copy.copy(strings) # unfortunate expense
        while (len(strings) % 4 != 0):
            strings.append("")       
    for i in range(0, len(strings) / 4):
        print(
            "  "
            + format_one(width, strings[i * 4])
            + format_one(width, strings[i * 4 + 1])
            + format_one(width, strings[i * 4 + 2])
            + format_one(width, strings[i * 4 + 3])
            )

if __name__ == "__main__":
    #
    # run test code if module run directly
    #
    print_list2(["a"])
    print("print_list2------------------------------")
    print_list2(["a", "b"])
    print("print_list2------------------------------")
    print_list2(["a", "b", "c"])
    print("print_list2------------------------------")
    print_list2(["a", "b", "c", "d"])
    print("print_list2------------------------------")
    print_list2(["a", "b", "c", "d", "e"])
    print("print_list2------------------------------")
    
    print_list4(["a"])
    print("print_list4------------------------------")
    print_list4(["a", "b"])
    print("print_list4------------------------------")
    print_list4(["a", "b", "c"])
    print("print_list4------------------------------")
    print_list4(["a", "b", "c", "d"])
    print("print_list4------------------------------")
    print_list4(["a", "b", "c", "d", "e"])
    print("print_list4------------------------------")
    
    CommandLines = [
        [],
        ["build"],
        ["buildlocal"],
        ["buildglobal"],
        ["buildship"],
        ["ship"],
        ["clean"],
        ["cleanlocal"],
        ["cleanglobal"],
        ["realclean"],
        ["-foo", "build"],
        #["unknown"],
        ["clean", "-bar"],
        ["-a", "-b", "-c"],
    
        #["m3core", "libm3"],
        ["build", "m3core", "libm3"],
        ["buildlocal", "m3core", "libm3"],
        ["buildglobal", "m3core", "libm3"],
        ["buildship", "m3core", "libm3"],
        ["ship", "m3core", "libm3"],
        ["clean", "m3core", "libm3"],
        ["cleanlocal", "m3core", "libm3"],
        ["cleanglobal", "m3core", "libm3"],
        ["realclean", "m3core", "libm3"],
        ["-foo", "build", "m3core", "libm3"],
        #["unknown", "m3core", "libm3"],
        ["clean", "-bar", "m3core", "libm3"],
        #["-a", "-b", "-c", "m3core", "libm3"],
        ]
    
    Functions = [
        map_action,
        add_action_opts,
        extract_options,
        get_args,
        ]
    
    Width = 0
    for CommandLine in CommandLines:
        Length = 0
        for Arg in CommandLine:
            Length += 4
            Length += len(Arg)
        if (Length > Width):
            Width = Length
    
    for Function in Functions:
        for CommandLine in CommandLines:
            print("%s(%-*s): %s" % (Function.__name__, Width, CommandLine, Function(CommandLine)))
    
def show_usage(args):
    for arg in args:
        if (arg in ["-h", "-help", "--help", "-?"]):
            print("")
            print("usage " + os.path.split(args[0])[1] + ":")
            if (sysinfo.USAGE):
                print(sysinfo.USAGE)
            else:
                print("")
                print("No specific usage notes available.")
                print("")
                print("Generic commands:")
                print(GEN_CMDS)
                print("Generic options:")
                print(GEN_OPTS)
            sys.exit(0)
