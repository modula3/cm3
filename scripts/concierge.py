#!/usr/bin/env python3

# The concierge utility is a basic package manager for cm3.
#
# The concierge has partial knowledge of package dependencies, as
# encoded in the file `scripts/pkginfo.txt` which topologically sorts
# all known packages in dependency order.  Given a request to operate
# on (build, clean, or install) a collection of packages, the
# concierge searches the source tree to find the requested packages
# and uses the available information to ensure the operations are
# carried-out in a consistent order.
#
# Unfortunately the concierge does not have full knowledge of package
# dependencies, so it is not able to ensure that all required
# dependencies for a package are either up-to-date or installed.  For
# some operations, such as upgrades, those requirements are hard-coded
# in this script.
#
# The concierge is also unable to invalidate packages whose
# dependencies have changed incompatibly.  Such functionality properly
# belongs to cm3 itself but is not implemented.
#
# The ability of the concierge to locate packages and--at least
# partially--manage their dependencies is the primary reason to
# contribute new libraries to the cm3 repo, instead of distributing
# them separately.


# Concierge features:
#
# * Package management
#   The primary purpose of the concierge is to build and install
#   packages, respecting their dependency order.  Specify packages on
#   either by individual name (i.e., `m3tohtml`) or set name (i.e.,
#   `m3devtool`).  Construct more complex requests using an add (`+`)
#   and remove (`-`) syntax, i.e., `+m3devtool -m3tohtml`.
#
# * Compiler upgrades
#   The concierge additionally knows what packages to rebuild and in
#   what order to upgrade the compiler from source.  This is primarily
#   useful to developers working from git.
#
# * System upgrades
#   After upgrading the compiler, the concierge can optionally rebuild
#   all libraries and applications to upgrade the entire CM3 system.
#
# * New system installs
#   On a new system without a pre-existing CM3 compiler, the concierge
#   can build and install a bootstrap compiler (from C) then build and
#   install a new compiler.  Optionally, it can build and install all
#   libraries and applications to construct a full system.
#
# * Boostrap creation
#   For developers the concierge can produce sources for a bootstrap
#   compiler by compiling the required sources to C.
#
# * Distribution creation
#   Finally, the concierge can produce a distribution tarball
#   including a bootstrap compiler that can be installed on a fresh
#   system.


# A note on CM3 backends:
#
# CM3 properly considers the integrated backend (Win32) and GCC
# backend (some systems) to be the default, most mature code
# generators where they are available.
#
# The concierge considers these defaults to be obsolete (integrated)
# and unmaintainable (GCC), and instead prefers the C backend.  Unless
# otherwise instructed the concierge will build only the C and
# integrated backends (only on I386_NT) and will not build GCC.
#
# Developers can specify `-gcc` or `--backend gcc` the build the GCC
# backend.
#
# GCC is not included in the distribution tarball and is not available
# on new system installs, it must be built from source using
#
#   scripts/concierge.py upgrade -gcc


import os
import platform
import re
import shlex
import shutil
import subprocess
import sys
import tempfile

from pathlib import Path


# The canonical package set defined in pkginfo.txt.
ALL = "all"

# Operating system classifications.
POSIX = "POSIX"
WIN32 = "WIN32"


# Setup logging to `concierge.log`

class Tee:
    "Utility for capturing all output to logfile"

    def __init__(self, left, right):
        self._left = left
        self._right = right

    def write(self, data):
        self._left.write(data)
        self._right.write(data)

    def flush(self):
        self._left.flush()
        self._right.flush()

# Capture all output to logfile in the current directory.
logfile = Path(sys.argv[0]).with_suffix(".log").name

# Replace any existing logfile.
sys.stdout = Tee(sys.stdout, open(logfile, "w"))
sys.stderr = sys.stdout

# Start by logging the command-line.
print(*sys.argv)


def show_usage():
    print(f"""usage:
{sys.argv[0]} (COMMAND | PACKAGE_ACTIONS) {{GENERAL_OPTION}} [COMPILER_OPTIONS] [PACKAGE_SELECTION]

Specify a command or a sequence of one-or-more package actions.  All
commands and package actions accept both general options and compiler
options, and most commands accept a package selection.

The available commands are

COMMAND = install [INSTALL_OPTIONS] | upgrade | full-upgrade | make-bootstrap | make-dist

  * install :: install a new system from a distribution
  * upgrade :: upgrade the compiler
  * full-upgrade :: upgrade the compiler and all libraries
  * make-bootstrap :: prepare a bootsrap compiler
  * make-dist :: prepare a distribution

"install" can take additional arguments

INSTALL_OPTIONS = [INSTALL_PREFIX] [CMAKE_OPTIONS]

The prefix specifies where to install the new system.  By default,
install will overwrite any existing cm3 installation.

INSTALL_PREFIX = --prefix <path>

Other options are passed directly to cmake when building the bootstrap
compiler.

CMAKE_OPTIONS = [GENERATOR_OPTION] {{CMAKE_DEFINE}}

GENERATOR_OPTION = -G <cmake_generator>

CMAKE_DEFINE = -DCMAKE_<name> | -DCMAKE_<name>=<value>

After building the bootstrap compiler, "install" finishes by
performing a "full-upgrade".  Both "install" and "full-upgrade" accept
a PACKAGE_SELECTION which defaults to "all".


Lacking a command, the default behavior is to execute one-or-more
package actions.  Any number of package actions can be requested, and
they are applied in sequence (i.e., "realclean buildship").

PACKAGE_ACTIONS = PACKAGE_ACTION {{PACKAGE_ACTION}}

PACKAGE_ACTION = build | buildglobal | buildlocal | buildship | clean | cleanglobal | cleanlocal | realclean | ship

  * build :: alias for buildlocal
  * buildglobal :: build without local overrides
  * buildlocal :: build with local overrides (i.e, "-override")
  * buildship :: build without local overrides then install
  * clean :: alias for cleanlocal
  * cleanglobal :: clean without local overrides
  * cleanlocal :: clean with local override
  * realclean :: "rm -R" all build directories
  * ship :: install packages


Options can be applied to all commands and package actions.

GENERAL_OPTION = -k | --keep-going | -l | --list-only | -n | --no-action

  * -k | --keep-going :: ignore errors and continue with commands
  * -l | --list-only :: list packages that would be affected
  * -n | --no-action :: print commands that would be executed, but make no changes


Compiler options are passed through to cm3.

COMPILER_OPTIONS = {{COMPILER_FLAG}} {{COMPILER_DEFINE}} [BACKEND_OPTION] [TARGET_OPTION]

The various flags are exactly those accepted by cm3.

COMPILER_FLAG = -boot | -commands | -debug | -keep | -override | -silent | -times | -trace | -verbose | -why

  * -commands :: list system commands as they are performed
  * -debug :: dump internal debugging information
  * -keep :: preserve intermediate and temporary files
  * -override :: include the "m3overrides" file
  * -silent :: produce no diagnostic output
  * -times :: produce a dump of elapsed times
  * -trace :: trace quake code execution
  * -verbose :: list internal steps as they are performed
  * -why :: explain why code is being recompiled

Defines are passed verbatim to cm3.

COMPILER_DEFINE = -D<name> | -D<name>=<value>

Select the backend to use for compliation.  Breaking from tradition,
the "c" backend is used as the default if nothing is specified, and
the gcc backend is only built when explicitly requested.

BACKEND_OPTION = --backend (c|gcc|integrated) | -c | -gcc | -integrated

Select the compile target.  The command-line "--target" takes
precedence over the "CM3_TARGET" environment variable, which takes
precedence over the default (usually matching the host).

TARGET_OPTION = --target <name>


Packages are specified using either package sets or individual
packages names (scripts/pkginfo.txt lists known packages and their
containing sets), and can optionally be specified using a subtractive
notation.

PACKAGE_SELECTION = {{<name> | +<name> | -<name>}}

Examples
  * "database -mysql" selects "odbc postgres95 db smalldb"
  * "-mysql database" selects "odbc mysql postgres95 db smalldb"

Package selections are processed in-order.  An empty package selection
is always interpreted to mean "all", and any selection that begins
with a subtraction is subtracted from an implicit "all".

Examples
  * "buildship" builds and installs everything
  * "buildship -gui" builds and installs everything except the gui libraries

ENVIRONMENT VARIABLES
  * CM3 :: provide the name of the cm3 executable, if it is other than "cm3"
  * CM3_ALL :: define to build all packages without considering the target environment
  * CM3_INSTALL :: provide the root of an existing cm3 installation, if not in path
  * CM3_TARGET :: another way to specify "--target"
  * HAVE_SERIAL :: define to build the serial libraries on Posix systems
  * HAVE_TCL :: define to build the tcl integration
""")


class Error(Exception):
    pass


class FatalError(Error):
    def __init__(self, message):
        self.message = message


class UsageError(Error):
    def __init__(self, message):
        self.message = message


class Platform:
    """Describes compilation host or target

    Given a CM3 platform name, this class provides knowledge of what
    backends and features CM3 supports on that platform.
    """

    @staticmethod
    def normalize_platform(name):
        "Error if name does not match a known platform"
        for target in Platform._all_platforms():
            if name.upper() == target.upper():
                return target
        raise UsageError(f"{name} is not a recognized target")

    @staticmethod
    def _all_platforms():
        "Not all named platforms are actually supported"
        machines = [
            "ALPHA", "ALPHA32", "ALPHA64", "AMD64", "ARM", "ARMEL", "ARM64",
            "IA64", "I386", "PPC", "PPC32", "PPC64", "SPARC", "SPARC32",
            "SPARC64", "MIPS32", "MIPS64EL", "MIPS64", "PA32", "PA64",
            "RISCV64", "SH"
        ]
        systems = [
            "AIX",  "CE", "CYGWIN", "DARWIN", "FREEBSD", "HPUX", "INTERIX",
            "IRIX", "LINUX", "MINGW", "NETBSD", "NT", "OPENBSD", "OSF",
            "SOLARIS", "VMS"
        ]
        legacy_platforms = ["NT386", "LINUXLIBC6", "SOLsun", "SOLgnu", "FreeBSD4"]
        return [f"{arch}_{os}" for arch in machines for os in systems] + legacy_platforms

    def __init__(self, name = None):
        if not name:
            arch = self._map_arch(platform.machine())
            os = platform.system()
            name = f"{arch}_{os}".upper()
        self._name = Platform.normalize_platform(name)

    def has_gcc_backend(self):
        "Supported by GCC backend"
        name = self.name()

        # These backends require Visual Studio.
        if name == "NT386" or name.endswith("_NT"):
            return False

        # Our GCC is too old for ARM support.
        if re.match(r"ARM|SOL", name):
            return False

        # Many platforms only work on the C backend.
        if re.search(r"ALPHA|CYGWIN|MINGW|OSF|RISCV|SOLARIS", name):
            return False

        return True

    def has_gdb(self):
        # So, no...
        return self.name() in \
            ["FreeBSD4", "I386_CYGWIN", "I386_FREEBSD", "I386_LINUX", "I386_NETBSD", "LINUXLIBC6", "SOLgnu"]

    def has_integrated_backend(self):
        "The integrated backend supports only 32-bit Windows"
        return self.name() in ["NT386", "I386_NT"]

    def has_serial(self):
        return self.is_win32()

    def is_mingw(self):
        return self.name().endswith("_MINGW")

    def is_nt(self):
        return self.name() == "NT386" or self.name().endswith("_NT")

    def is_posix(self):
        return not self.is_win32()

    def is_win32(self):
        return self.is_mingw() or self.is_nt()

    def name(self):
        "As recognized by cm3"
        return self._name

    def os(self):
        "As recognized by cm3"
        return WIN32 if self.is_win32() else POSIX

    def _map_arch(self, arch):
        "Map Python's architecture name to CM3's architecture name"
        return "amd64" if arch == "x86_64" else arch


class Cm3:
    """CM3 build environment

    This class is primarily responsible for running the CM3 compiler.
    It locates the compiler and the CM3 source and install
    directories.  It tracks requested compiler options (flags and
    defines) and the current compilation target.
    """

    def __init__(self, script, backend="c", defines=None, flags=None, target=None):
        # The script is used to locate the source directory.
        self._script  = script

        # Defines the backend to use when compiling packages.
        self._backend = backend

        # Various CM3 compiler options requested by the user.
        self._defines = defines or []
        self._flags   = flags or []

        # Compilation host and target platforms.
        self._host    = None
        self._target  = None
        if target:
            self._target = target if isinstance(target, Platform) else Platform(target)

        # Misc. options to direct the overall behavior of the
        # concierge script.
        self._keep_going = False
        self._list_only  = False
        self._no_action  = False

    def backend(self):
        "The compiler backend to use when building packages"

        # Don't try to use GCC when not available.
        if self._backend == "gcc" and not self.target().has_gcc_backend():
            self._backend = "c"

        # Don't try to use the integrated backend when not available.
        if self._backend == "integrated" and not self.target().has_integrated_backend():
            self._backend = "c"

        assert self._backend in ["c", "gcc", "integrated"]
        return self._backend

    def build(self, *paths):
        "Relative to root of current build directory"
        return self.source(*paths) / self.build_dir()

    def build_dir(self):
        "Basename of build directory"
        return self.config()

    def config(self):
        "Used as an alias of target name"
        return self.target().name()

    def defines(self):
        "Any '-D' command-line arguments intended for cm3"
        return self._defines

    def env(self):
        "Execution environment for cm3 child processes"

        # TODO it is not clear if some or all of these are redundant,
        # given the defines passed to cm3 on the command-line (in
        # `PackageAction.run`).  These may simply have been an
        # out-of-band communication mechanism for the legacy scripts.
        return dict(
            os.environ,
            CM3_INSTALL=str(self.install()),
            CM3_ROOT=str(self.source()),
            CM3_TARGET=self.config()
        )

    def exe(self):
        "Full path to cm3 executable"

        def fail():
            raise FatalError("environment variable CM3_INSTALL not set AND cm3 not found in PATH")

        # Environment may override name of executable.
        basename = os.environ.get("CM3", "cm3")

        # If the environment specified a path, it should work as-is.
        candidate = Path(basename)
        if len(candidate.parents) > 1:
            if candidate.is_file():
                return candidate.resolve()
            else:
                fail()

        # Environment may also override search path.
        if os.environ.get("CM3_INSTALL"):
            # Posix
            candidate = Path(os.environ["CM3_INSTALL"], "bin", basename)
            if candidate.is_file() and os.access(candidate, os.X_OK):
                return candidate
            # Windows
            candidate = candidate.with_suffix(".exe")
            if candidate.is_file():
                return candidate
            fail()

        # With no overrides, we search PATH.
        candidate = self._find_exe(basename)
        if candidate is None:
            fail()

        return candidate

    def _find_exe(self, basename):
        "Look for an executable in PATH"

        # Search PATH.
        for dir in os.get_exec_path():
            # Posix
            candidate = Path(dir, basename)
            if candidate.is_file() and os.access(candidate, os.X_OK):
                return candidate
            # Windows
            candidate = candidate.with_suffix(".exe")
            if candidate.is_file():
                return candidate

        # Not found.
        return None

    def flags(self):
        "Command-line arguments intended for cm3"
        return self._flags

    def host(self):
        "Compilation host, only used as default for target"
        if not self._host:
            self._host = self._sniff_host()
        return self._host

    def _sniff_host(self):
        "Guess the host platform"
        try:
            # Ask cm3.
            output = subprocess.check_output([str(self.exe()), "-version"], errors="ignore")
            for line in output.splitlines():
                host = line.find("host: ")
                if host >= 0:
                    return Platform(line[host + 6:].rstrip())
        except:
            pass

        # If there's a problem, we'll make our best guess.
        return Platform()

    def install(self, *paths):
        "Relative to root of current installation directory"
        exe_path    = self.exe();
        bin_dir     = exe_path.parent
        install_dir = bin_dir.parent
        return install_dir.joinpath(*paths)

    def keep_going(self):
        "Continue running the concierge script in event of errors"
        return self._keep_going

    def list_only(self):
        "List packages selected by current command-line"
        return self._list_only

    def no_action(self):
        "Perform a dry-run, do not make any changes to the system"
        return self._no_action

    def script(self):
        "The script is used to locate the source directory"
        return Path(self._script).resolve()

    def set_options(self, namespace):
        "Inform CM3 of options detected in argument parsing"
        for attr in ["_keep_going", "_list_only", "_no_action"]:
            if hasattr(namespace, attr):
                setattr(self, attr, getattr(namespace, attr))

    def source(self, *paths):
        "Relative to root of current source directory"
        script_path = self.script()
        script_dir  = script_path.parent
        source_dir  = script_dir.parent
        return source_dir.joinpath(*paths)

    def target(self):
        "Compilation target, passed to CM3"
        if not self._target:
            self._target = self._sniff_target()
        return self._target

    def _sniff_target(self):
        "Guess the target platform"
        try:
            # Ask cm3.
            output = subprocess.check_output([str(self.exe()), "-version"], errors="ignore")
            for line in output.splitlines():
                target = line.find("target: ")
                if target >= 0:
                    return Platform(line[target + 8:].rstrip())
        except:
            pass

        # If there's a problem, assume we're compiling for the host machine.
        return self.host()

    def use_c_backend(self):
        return self.backend() == "c"

    def use_gcc_backend(self):
        return self.backend() == "gcc"


class WithCm3:
    "Provides access to cm3 build context"

    def __init__(self, cm3):
        self._cm3 = cm3

    def __getattr__(self, method_name):
        "Delegate some requests to CM3"
        forwards = [
            "build",
            "build_dir",
            "config",
            "defines",
            "env",
            "exe",
            "flags",
            "install",
            "keep_going",
            "list_only",
            "no_action",
            "source",
            "target",
            "use_c_backend",
            "use_gcc_backend"
        ]
        if method_name not in forwards:
            raise AttributeError
        return getattr(self.cm3(), method_name)

    def cm3(self):
        return self._cm3

    def rmdir(self, dir):
        "Recursively remove a directory"
        if dir.is_dir():
            print("rm", "-Rf", dir)
            if not self.no_action():
                shutil.rmtree(dir)


class PackageDatabase(WithCm3):
    """Knows what packages are available and their dependency order

    Whereas `Cm3` knows *how* to run the compiler, the package
    database knows *where* and *when* (in what order) to run the
    compiler to build a set of requested packages.
    """

    def __init__(self, cm3):
        super().__init__(cm3)

        # There is an order dependency here, sets must be loaded
        # before the index.
        self._load_package_sets()
        self._load_package_index()

    def all_packages(self):
        "Canonical list of packages, in dependency order, as defined in pkginfo.txt"

        # This is a superset of the packages available on the system.
        return self._package_sets[ALL]

    def get_package_paths(self, names):
        """Locations of all requested packages, where `names` is a mixed list
        of individual packages and package sets"""

        # These packages will be present on the system.
        return [self.get_package_path(pkg) for pkg in self.get_packages(names)]

    def get_package_path(self, name):
        "Location of package relative to root of source tree"
        try:
            return self._package_index[name]
        except:
            raise FatalError(f"package {name} requested but not found")

    def get_packages(self, names):
        """List of requested packages, in dependency order

        Here `names` is a mixed list of packages and package sets, and
        may use the add/remove syntax (`+` or `-`) to make specific
        requests.

        The returned packages may be a subset of those requested,
        limited by what is available on the system.  For example, a
        request to build the GCC backend (`m3cc`) may come up empty
        when GCC is not included in the distribution.
        """

        # First determine what packages are requested, then later we
        # will work-out the bulid order.
        requested = set()

        # Incorporate each listed package or set into requested.
        for name in names:
            remove = name.startswith("-")
            if name.startswith("+") or name.startswith("-"):
                name = name[1:]

            if name in self._package_sets:
                # name identifies a package set
                if remove:
                    for package in self._package_sets[name]:
                        if package in requested:
                            requested.remove(package)
                else:
                    for package in self._package_sets[name]:
                        requested.add(package)
            elif name in self._package_index:
                # name identifies an individual package
                package = name
                if remove:
                    if package in requested:
                        requested.remove(package)
                else:
                    requested.add(package)

        # Determine the canonical order for all requested packages.
        packages = []
        for package in self.all_packages():
            if package in requested and package in self._package_index:
                packages.append(package)

        # Finally, omit anything not available to the current target.
        return self._filter_packages(packages)

    def is_package(self, name):
        "Name identifies a package or package set"
        if name.startswith("+") or name.startswith("-"):
            name = name[1:]
        return self._package_sets.get(name) or self._package_index.get(name)

    def _filter_packages(self, packages):
        "Exclude from packages anything that can't work in the target environment"
        return [pkg for pkg in packages if self._include_package(pkg)]

    def _include_package(self, name):
        "Do we try to build this package?"
        if os.environ.get("CM3_ALL"):
            return True

        if name == "X11R4":  return self.target().is_posix()
        if name == "m3cc":   return self.use_gcc_backend()
        if name == "m3gdb":  return self.use_gcc_backend() and self.target().has_gdb()
        if name == "serial": return self.target().has_serial() or  os.environ.get("HAVE_SERIAL")
        if name == "tapi":   return self.target().is_win32()
        if name == "tcl":    return os.environ.get("HAVE_TCL")
        return True

    def _load_package_index(self):
        """Scan the source directory for available packages

        Importantly, the package index reflects what packages actually
        exist and can be installed.  The packages listed in
        pkginfo.txt are a superset of what is actually available.
        """

        # Find all the fully-qualified package paths.
        package_paths = []

        root = self.source()
        for dir, children, files in os.walk(root):
            dir = Path(dir)

            # src may be a package directory
            if dir.name == "src" and "m3makefile" in files:
                package_dir  = dir.parent
                package_path = package_dir.relative_to(root).as_posix()
                package_paths.append(package_path)

                # We can prune the search here.
                children.clear()

            # We can also prune specific, named directories.
            if dir.name.startswith(".") or dir.name.startswith("_"):
                children.clear()

            if str(dir.as_posix()).endswith("examples/web"):
                children.clear()

        # Look for package names in the canonical list.
        self._package_index = dict()

        package_list = self.all_packages()
        for package_path in package_paths:
            # Find the canonical name of the package.  The canonical
            # name is some sub-path of the relative directory that
            # uniquely identifies the package in `pkginfo.txt`.
            package_name = str(package_path)
            while package_name not in package_list and package_name.find("/") >= 0:
                # Keep stripping off leading directories until we find
                # a match.
                package_name = package_name[package_name.find("/")+1:]

            # Index the package by its canonical name, if found.
            if package_name in package_list:
                self._package_index[package_name] = package_path

    def _load_package_sets(self):
        """Read package definitions from pkginfo.txt

        pkginfo.txt defines the canonical names of all known packages
        and their relative dependency order.  This is separate from
        the information about what packages are actually available.
        """

        self._package_sets = dict()
        with open(self.source("scripts/pkginfo.txt"), "r") as pkginfo:
            for line in pkginfo:
                line = line.rstrip()
                if not line:
                    continue
                package, *sets = line.split()
                sets.insert(0, ALL)
                for set in sets:
                    self._package_sets.setdefault(set, []).append(package)


class PackageAction(WithCm3):
    "Runs cm3 on a list of packages identified by relative paths"

    def __init__(self, cm3):
        super().__init__(cm3)
        self._success = False

    def execute(self, package_paths):
        "Runs cm3 for each listed package"
        self._success = True
        for package_path in package_paths:
            try:
                self.execute_path(package_path)
            except:
                self._success = False
                if not self.keep_going():
                    raise

    def run(self, package_path, args):
        "Execute a cm3 child process"
        cwd  = self.source(package_path)
        args = [str(self.exe())] + args + self.defines() + self.flags()
        print("cd", cwd)
        print(*args)

        if self.no_action():
            return

        proc = subprocess.run(
            args,
            cwd=cwd,
            env=self.env(),
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            errors="ignore"
        )
        sys.stdout.write(proc.stdout)
        proc.check_returncode()

    def success(self):
        "The last action was fully successful, with no failures"
        return self._success

    def defines(self):
        "List of '-D' arguments to pass to cm3"

        # TODO see comment in `Cm3.env`
        defines = [
            f"-DBUILD_DIR={self.build_dir()}",
            f"-DROOT={self.source()}",
            f"-DTARGET={self.config()}"
        ]

        # Where the gcc or integrated backends are available, they are
        # the default for their respective platforms and do not need
        # to be specified.
        if self.use_c_backend():
            defines.append(f"-DM3_BACKEND_MODE=C")

        # Include any defines given on the command-line.
        return defines + self.cm3().defines()


class CleanAction(PackageAction):
    "Clean actions need to reverse the package order, and can safely ignore errors"

    def execute(self, package_paths):
        # Clean in reverse dependency order to avoid warnings.
        self._success = True
        for package_path in reversed(package_paths):
            try:
                self.execute_path(package_path)
            except:
                self._success = False
                if not self.keep_going():
                    raise

    def keep_going(self):
        "Ignore errors when cleaning"
        return True


class BuildGlobal(PackageAction):
    "Build package without overrides"

    def execute_path(self, package_path):
        self.run(package_path, ["-build"])


class BuildLocal(PackageAction):
    "Build package with local overrides"

    def execute_path(self, package_path):
        self.run(package_path, ["-build", "-override"])


class CleanGlobal(CleanAction):
    "Clean package without overrides"

    def execute_path(self, package_path):
        self.run(package_path, ["-clean"])


class CleanLocal(CleanAction):
    "Clean package with local overrides"

    def execute_path(self, package_path):
        self.run(package_path, ["-clean", "-override"])


class RealClean(CleanAction):
    "Remove target directory of package"

    def execute_path(self, package_path):
        self.rmdir(self.build(package_path))


class Ship(PackageAction):
    "Install package"

    def execute_path(self, package_path):
        self.run(package_path, ["-ship"])


class BuildShip(PackageAction):
    "Build package *without* overrides and install it"

    def __init__(self, cm3):
        super(BuildShip, self).__init__(cm3)
        self._buildglobal = BuildGlobal(cm3)
        self._ship = Ship(cm3)

    def execute_path(self, package_path):
        # These have to be done in lockstep.  Because of various
        # unclear dependencies, building everything before shipping
        # anything yields a broken system.
        self._buildglobal.execute_path(package_path)
        self._ship.execute_path(package_path)

    def keep_going(self):
        "If the build fails, we don't ship"
        return False


class CompositeAction(PackageAction):
    "Executes sequential package actions"

    def __init__(self, cm3, actions):
        super().__init__(cm3)
        self._actions = actions

    def execute(self, package_paths):
        self._success = True
        for action in self._actions:
            try:
                action.execute(package_paths)
                self._success = self._success and action.success()
            except:
                self._success = False
                if not self.keep_going():
                    raise


class WithPackageDb(WithCm3):
    "Provides access to package database"

    def __init__(self, cm3, package_db):
        super().__init__(cm3)
        self._package_db = package_db

    def get_package_paths(self, names):
        "Locations of all requested packages"
        return self.package_db().get_package_paths(names)

    def is_package(self, name):
        "Name identifies a package or package set"
        return self.package_db().is_package(name)

    def package_db(self):
        return self._package_db


# Maps action names to command objects, but also provides a canonical
# list of available package actions.
PACKAGE_ACTIONS = dict(
    # By default, "build" means "build with local overrides".
    build=BuildLocal,

    # Build without local overrides.  This is necessary for anything
    # we want to install.
    buildglobal=BuildGlobal,

    # Build, using m3overrides if available.
    buildlocal=BuildLocal,

    # Build without overrides, then install.
    buildship=BuildShip,

    # By default, "clean" means "clean with local overrides".
    clean=CleanLocal,

    # Clean without local overrides.
    cleanglobal=CleanGlobal,

    # Clean, respecting m3overrides if available.
    cleanlocal=CleanLocal,

    # Instead of running clean, just nuke all the build directories.
    realclean=RealClean,

    # Install.
    ship=Ship
)


class WithPackageActions(WithPackageDb):
    "Conveniences for executing package actions"

    def __init__(self, cm3, package_db):
        super().__init__(cm3, package_db)

    # Note that this won't generate a "build" method, because that is
    # already defined in WithCm3 with an entirely different meanning.
    # It is better to be specific with "buildglobal" or "buildlocal"
    # if that is what is needed.
    def __getattr__(self, method_name):
        # Defer first to the methods defined in `WithCm3`.
        try:
            return super().__getattr__(method_name)
        except AttributeError:
            pass

        # Only if that fails, look for a named package action.
        try:
            constructor = PACKAGE_ACTIONS[method_name]
            action = constructor(self.cm3())
            def executor(packages):
                paths = self.get_package_paths(packages)
                print(method_name, *paths)
                action.execute(paths)
            return executor
        except KeyError:
            raise AttributeError


class ConciergeCommand(WithPackageActions):
    "A top-level command made to the concierge"

    @classmethod
    def parse_args(cls, args, namespace):
        "Interpret arguments common to all commands"
        cls._parse_compiler_options(args, namespace)
        cls._parse_options(args, namespace)

    @classmethod
    def parse_packages(cls, args, namespace):
        "After parsing all arguments, assume anything left is a package specification"
        packages = args[:]
        args.clear()
        setattr(namespace, "_packages", packages)

    @classmethod
    def _parse_options(cls, args, namespace):
        "Global options that direct concierge behavior"

        keep_going = False
        list_only  = False
        no_action  = False

        for option in ["-k", "--keep-going"]:
            while option in args:
                args.remove(option)
                keep_going = True

        for option in ["-l", "--list-only"]:
            while option in args:
                args.remove(option)
                list_only = True

        for option in ["-n", "--no-action"]:
            while option in args:
                args.remove(option)
                no_action = True

        setattr(namespace, "_keep_going", keep_going)
        setattr(namespace, "_list_only",  list_only)
        setattr(namespace, "_no_action",  no_action)

    @classmethod
    def _parse_compiler_options(cls, args, namespace):
        "Any arguments that define how we call-out to the compiler"
        cls._parse_backend(args, namespace)
        cls._parse_defines(args, namespace)
        cls._parse_flags(args, namespace)
        cls._parse_target(args, namespace)

    @classmethod
    def _parse_backend(cls, args, namespace):
        "Look for any command-line argument specifying a backend"

        # Default.
        backend = "c"

        tail = args[:]
        args.clear()
        while tail:
            head = tail.pop(0)
            if head == "--backend":
                if not tail:
                    raise UsageError("missing backend selection")
                backend = tail.pop(0)
            elif head.startswith("--backend="):
                backend = head[10:]
            elif head in ["-c", "-gcc", "-integrated"]:
                backend = head[1:]
            else:
                args.append(head)

        if backend not in ["c", "gcc", "integrated"]:
            raise UsageError(f"{backend} is not a recognized backend")

        setattr(namespace, "_backend", backend)

    @classmethod
    def _parse_defines(cls, args, namespace):
        "Look for defines that need to be passed-through to cm3"
        defines = [arg for arg in args if arg.startswith("-D") and not arg.startswith("-DCMAKE_")]
        args[:] = [arg for arg in args if arg not in defines]
        setattr(namespace, "_defines", defines)

    @classmethod
    def _parse_flags(cls, args, namespace):
        "Look for flags that need to be passed-through to cm3"
        cm3flags = [
            "-boot",
            "-commands",   # list system commands as they are performed
            "-debug",      # dump internal debugging information
            "-keep",       # preserve intermediate and temporary files
            "-override",   # include the "m3overrides" file
            "-silent",     # produce no diagnostic output
            "-times",      # produce a dump of elapsed times
            "-trace",      # trace quake code execution
            "-verbose",    # list internal steps as they are performed
            "-why"         # explain why code is being recompiled
        ]
        flags   = [arg for arg in args if arg in cm3flags]
        args[:] = [arg for arg in args if arg not in flags]
        setattr(namespace, "_flags", flags)

    @classmethod
    def _parse_target(cls, args, namespace):
        target = os.environ.get("CM3_TARGET")

        tail = args[:]
        args.clear()
        while tail:
            head = tail.pop(0)
            if head == "--target":
                if not tail:
                    raise UsageError("missing target selection")
                target = tail.pop(0)
            elif head.startswith("--target="):
                target = head[9:]
            else:
                args.append(head)

        if target:
            target = Platform.normalize_platform(target)

        setattr(namespace, "_target", target)

    def set_options(self, namespace):
        "Inform command of options detected in argument parsing"
        for attr in ["_actions", "_cmake_args", "_packages", "_prefix"]:
            if hasattr(namespace, attr):
                setattr(self, attr, getattr(namespace, attr))

    def actions(self):
        "List of package actions given on command-line"
        return self._actions

    def packages(self):
        "List of packages requested on the command-line"
        return [pkg for pkg in self._packages if self.is_package(pkg)]

    def cp(self, src, dst):
        "Copy a file"
        if src.is_file():
            print("cp", "-P", src, dst)
            if not self.no_action():
                shutil.copy(src, dst)

    def mkdir(self, dir):
        "Create a directory"
        print("mkdir", "-p", dir)
        if not self.no_action():
            dir.mkdir(parents=True, exist_ok=True)

    def rm(self, file):
        "Remove a file"
        if file.is_file():
            print("rm", "-f", file)
            if not self.no_action():
                file.unlink()


class PackageCommand(ConciergeCommand):
    "Clean, build, and/or ship packages"

    @classmethod
    def parse_args(cls, args, namespace):
        super().parse_args(args, namespace)
        cls._parse_actions(args, namespace)
        super().parse_packages(args, namespace)

    @classmethod
    def _parse_actions(cls, args, namespace):
        "Read list of package action from command-line"
        actions = [arg for arg in args if arg in PACKAGE_ACTIONS]
        args[:] = [arg for arg in args if arg not in actions]

        if len(actions) == 0:
            raise UsageError("no package actions specified")

        setattr(namespace, "_actions", actions)

    def execute(self):
        "Apply requested actions to requested packages"
        packages = self.get_package_paths(self.packages())
        if self.list_only():
            self.list_packages(packages)
            return
        self.action().execute(packages)

    def list_packages(self, packages):
        for package in packages:
            print(package)

    def action(self):
        "Return the action requested on the command-line"

        # This will be a composite action, executing all requested
        # actions in sequence.
        actions = []
        for arg in self.actions():
            actions.append(PACKAGE_ACTIONS[arg](self.cm3()))
        return CompositeAction(self.cm3(), actions)

    def packages(self):
        "Return the packages requested on the command-line"
        pkgs = super().packages()
        if not pkgs or pkgs[0].startswith("-"):
            pkgs.insert(0, ALL)
        return pkgs


class UpgradeCommand(ConciergeCommand):
    "Upgrade compiler and core system"

    def execute(self):
        base_packages = ["+front", "+m3bundle", "-m3cc"]

        # Guarantee a basic config if we're on a clean system.
        if not self.install("bin/cm3.cfg").is_file():
            self._install_config()

        # Build GCC first if we will need it.
        if self.use_gcc_backend() and not self.install("bin/cm3cg").is_file():
            self.buildship(["m3cc"])
            self._ship_back()

        # Then ensure we don't use an outdated GCC.
        self._clean()

        # Build the compiler using the installed version of the system.
        self._run_pass(base_packages)

        # Use the new compiler to build the core system.  There is no
        # need to rebuild GCC in the second pass.
        self._install_config()
        self._run_pass(base_packages, build_gcc = False)

    def _clean(self):
        "Delete lingering cm3cg so we can't accidentally use an old version"
        for exe in ["cm3cg", "gcc/m3cgc1"]:
            file = self.build("m3-sys/m3cc") / exe
            self.rm(file)
            self.rm(file.with_suffix(".exe"))

    def _run_pass(self, packages, build_gcc = True):
        "Perform one build/iteration within the system upgrade"

        # Build the compiler.
        self.realclean(packages)
        self.buildship(packages)

        # Continue with GCC.
        build_gcc = build_gcc and self.use_gcc_backend()
        if build_gcc:
            self.realclean(["m3cc"])
            self.buildship(["m3cc"])

        # Install the binaries.
        if build_gcc:
            self._ship_back()
        self._ship_front()

    def _ship_back(self):
        "Ship the compiler 'backend', i.e., GCC"
        self._copy_compiler(self.build("m3-sys/m3cc"), self.install("bin"))

    def _ship_front(self):
        "Ship the comiler 'frontent', i.e., cm3"
        self._copy_compiler(self.build("m3-sys/cm3"), self.install("bin"))

    def _copy_compiler(self, src, dst):
        "Copy compiler executables to their installed locations"

        executables = ["cm3", "cm3cg", "mips-tfile", "mklib"]

        # Ensure destination directory exists.
        self.mkdir(dst)

        # Copy executables.
        for exe in executables:
            item = src / exe
            if item.is_file():
                # Posix
                self.cp(item, dst)
            else:
                # Windows
                item = item.with_suffix(".exe")
                self.cp(item, dst)
                # Copy debug info.
                item = item.with_suffix(".pdb")
                self.cp(item, dst)

    def _install_config(self):
        "Copy config for distribution"

        src = self.source("m3-sys/cminstall/src/config-no-install")
        dst = self.install("bin/config")

        # Delete the old config files.
        self.rmdir(dst)

        # Ensure destination directory exists.
        self.mkdir(dst)

        # Copy all files from src to dst.
        for config in src.iterdir():
            self.cp(config, dst)

        # Write new cm3.cfg
        if self.no_action():
            return

        backend = ""
        if self.use_c_backend():
            backend = 'readonly M3_BACKEND_MODE = "C"\n'

        self.install("bin/cm3.cfg").write_text(
            f"""{backend}if not defined("SL") SL = "/" end
if not defined("HOST") HOST = "{self.config()}" end
if not defined("TARGET") TARGET = HOST end
INSTALL_ROOT = (path() & SL & "..")
include(path() & SL & "config" & SL & TARGET)
"""
        )


class FullUpgradeCommand(UpgradeCommand):
    "Upgrade system and reinstall packages"

    @classmethod
    def parse_args(cls, args, namespace):
        super().parse_args(args, namespace)
        super().parse_packages(args, namespace)

    def execute(self):
        # Upgrade the compiler.
        super().execute()

        # Clean, but again there is no point in rebuilding GCC.
        self.realclean([ALL, "-m3cc"])

        # Reinstall all packages.
        self.buildship(self.packages())

    def packages(self):
        "Return the packages requested on the command-line"
        pkgs = super().packages()
        if not pkgs or pkgs[0].startswith("-"):
            pkgs.insert(0, ALL)
        return pkgs


class InstallCommand(ConciergeCommand):
    "Bootstrap followed by full upgrade"

    @classmethod
    def parse_args(cls, args, namespace):
        super().parse_args(args, namespace)
        cls._parse_cmake_args(args, namespace)
        cls._parse_prefix(args, namespace)
        super().parse_packages(args, namespace)

    @classmethod
    def _parse_cmake_args(cls, args, namespace):
        "Look for arguments that should be passed directly to cmake"

        cmake_args = [arg for arg in args if arg.startswith("-DCMAKE_")]
        args[:] = [arg for arg in args if arg not in cmake_args]

        tail = [arg for arg in args if arg not in cmake_args]
        args.clear()
        while tail:
            head = tail.pop(0)
            if head == "-G":
                cmake_args.append(head)
                cmake_args.append(tail.pop(0))
            else:
                args.append(head)

        setattr(namespace, "_cmake_args", cmake_args)

    @classmethod
    def _parse_prefix(cls, args, namespace):
        "Specify the desired install location with --prefix"
        prefix = None

        tail = args[:]
        args.clear()
        while tail:
            head = tail.pop(0)
            if head == "--prefix":
                if not tail:
                    raise UsageError("missing install prefix")
                prefix = tail.pop(0)
            elif head.startswith("--prefix="):
                prefix = head[9:]
            else:
                args.append(head)

        setattr(namespace, "_prefix", prefix)

    def __init__(self, cm3, package_db):
        super().__init__(cm3, package_db)

        # We'll hand-off to `full-upgrade` after performing the
        # bootstrap.
        self._upgrade = FullUpgradeCommand(cm3, package_db)

    def set_options(self, namespace):
        super().set_options(namespace)
        self._upgrade.set_options(namespace)

    def execute(self):
        "Build the bootstrap compiler, then perform a system upgrade"
        self.bootstrap()
        self.full_upgrade()

    def bootstrap(self):
        "Build the bootstrap compiler"

        # Check that bootstrap sources are available.
        bootstrap_dir = self.source("bootstrap")
        if not (bootstrap_dir / "CMakeLists.txt").is_file():
            raise FatalError("missing bootstrap directory")

        # Run an out-of-tree build with cmake.
        with tempfile.TemporaryDirectory() as build_dir:
            setup = ["cmake", "-S", str(bootstrap_dir), "-B", build_dir] + self._cmake_args
            setup.append(f"-DCMAKE_INSTALL_PREFIX={self.prefix()}")
            if self.target().is_mingw():
                # At least for now, mingw/cmake can't seem to find its own libraries.
                setup.append("-DCMAKE_LIBRARY_PATH=/mingw64/x86_64-w64-mingw32/lib")
            self.rmdir(Path(self.prefix()))

            build = ["cmake", "--build", build_dir]
            install = ["cmake", "--install", build_dir]

            for command in [setup, build, install]:
                print(*command)
                if not self.no_action():
                    proc = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, errors="ignore")
                    sys.stdout.write(proc.stdout)
                    proc.check_returncode()

    def full_upgrade(self):
        "Perform a system upgrade"
        self._upgrade.execute()

    def prefix(self):
        "If not otherwise specified, replace the existing compiler"
        if not self._prefix:
            self._prefix = self.install()
        return self._prefix


class MakeBootstrapCommand(ConciergeCommand):
    "Generate sources for a bootstrap compiler"

    @classmethod
    def parse_args(cls, args, namespace):
        super().parse_args(args, namespace)

        # Override compiler configuration.
        setattr(namespace, "_backend", "c")
        setattr(namespace, "_flags", ["-boot", "-keep", "-no-m3ship-resolution"])

    def execute(self):
        # Compile cm3 and its dependencies to C.
        packages = ["+front", "-m3cc", "-m3cgcat", "-m3cggen"]
        self.realclean(packages)
        self.buildlocal(packages)

        # Create bootstrap directory.
        bootstrap_dir = self.source("bootstrap")
        self.rmdir(bootstrap_dir)
        self.mkdir(bootstrap_dir)

        # Copy generated C files to bootstrap.
        package_dirs = []
        for package_path in self.get_package_paths(packages):
            cmakelists = Path(package_path) / "CMakeLists.txt"
            if not cmakelists.is_file():
                continue
            package_dir = bootstrap_dir / Path(package_path).name
            self.mkdir(package_dir)
            self.cp(cmakelists, package_dir)
            package_dirs.append(package_dir.name)

            package_sources = []
            if not self.no_action():
                for file in self.build(package_path).iterdir():
                    if file.suffix in [".c", ".cpp", ".h"]:
                        self.cp(file, package_dir)
                        package_sources.append(file.name)

                with open(package_dir / "sources.lst", "w") as sources:
                    sources.write("set(cm3_SOURCES\n")
                    for filename in sorted(package_sources):
                        sources.write(f"{filename}\n")
                    sources.write(")\n")

        # Generate CMakeLists.txt
        cmake_install_prefix = f"/opt/cm3-{self.version()}" if self.version() else "/opt/cm3"
        cmake_header = f"""cmake_minimum_required(VERSION 3.10)
project(cm3 LANGUAGES C CXX)
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
   set(CMAKE_INSTALL_PREFIX "{cmake_install_prefix}" CACHE PATH "..." FORCE)
endif()
include(GNUInstallDirs)
include(bootstrap.cmake)
"""
        if not self.no_action():
            with open(bootstrap_dir / "CMakeLists.txt", "w") as cmake:
                cmake.write(cmake_header)
                for dir in sorted(package_dirs):
                    cmake.write(f"add_subdirectory({dir})\n")
        self.cp(self.source("scripts/bootstrap.cmake"), bootstrap_dir)

        # Generate tarballs.
        distname = f"cm3-boot-{self.config()}-{self.version()}"
        if self.target().is_win32():
            self.zip(f"{distname}.7z", ["bootstrap"])
        else:
            self.tar(f"{distname}.tar.xz", ["-C", str(self.source()), "bootstrap"])

    def zip(self, zipfile, args, noclean = False):
        if not noclean:
            self.rm(Path(zipfile))
        command = ["7z", "a", zipfile] + args

        print(*command)
        if not self.no_action():
            proc = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, errors="ignore")
            proc.check_returncode()
            sys.stdout.write(proc.stdout)

    def tar(self, tarfile, args):
        self.rm(Path(tarfile))
        command = ["tar", "acf", tarfile, "--warning=no-file-changed"] + args

        print(*command)
        if not self.no_action():
            proc = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, errors="ignore")
            sys.stdout.write(proc.stdout)

    def version(self):
        return self.tag()

    def tag(self):
        "Get the version from the current tag"
        try:
            return subprocess.check_output(["git", "describe", "--abbrev=0"], errors="ignore").rstrip()
        except:
            return None


class MakeDistributionCommand(MakeBootstrapCommand):

    def execute(self):
        # Prepare bootstrap.
        super().execute()

        # Clean.
        self.realclean([ALL])

        # Generate tarball.
        distname = f"cm3-dist-{self.config()}-{self.version()}"
        source   = self.source().name

        if self.target().is_win32():
            self.zip(f"{distname}.7z", ["-xr@.gitignore", "-xr@scripts/7z.exclude", f"../{source}"])
            self.zip(f"{distname}.7z", [f"../{source}/bootstrap"], noclean=True)
        else:
            parent  = str(self.source().parent)
            command = [
                "--directory", parent,
                "--exclude=*.7z",
                "--exclude=*.tar.xz",
                f"--transform=s!^{source}!{distname}!",
                f"{source}/bootstrap",
                f"{source}/m3-sys/cminstall/src/config-no-install",
                "--exclude-vcs",          # Don't include .git
                "--exclude-vcs-ignores",  # Don't include things ignored by git
                "--exclude=m3-sys/m3cc",  # Don't include GCC
                "--exclude=m3-sys/m3gdb",
                source]
            self.tar(f"{distname}.tar.xz", command)


class Concierge:
    "aka, main"

    def __init__(self, args = None):
        # Context defaults.
        self._cm3        = None
        self._command    = None
        self._package_db = None
        self._script     = None

        # Bootstrap defaults.
        self._cmake_args = []
        self._prefix     = None

        # Compiler defaults.
        self._backend = "c"
        self._defines = []
        self._flags   = []

        # Option defaults.
        self._keep_going = False
        self._list_only  = False
        self._no_action  = False

        # Package defaults.
        self._actions  = []
        self._packages = []

        # Target defaults.
        self._target = os.environ.get("CM3_TARGET")

        # Capture command-line arguments.
        args = (args or sys.argv)[:]
        args = [arg for arg in args if arg]
        self._parse_args(args)

    def main(self):
        "Carry-out the requested command"
        command = self._command(self.cm3(), self.package_db())
        command.set_options(self)
        command.execute()

    def cm3(self):
        if not self._cm3:
            self._cm3 = Cm3(
                script=self._script,
                backend=self._backend,
                defines=self._defines,
                flags=self._flags,
                target=self._target
            )
            self._cm3.set_options(self)
        return self._cm3

    def package_db(self):
        if not self._package_db:
            self._package_db = PackageDatabase(self.cm3())
        return self._package_db

    def _parse_args(self, args):
        "Try to make sense of command-line arguments"

        help = ["-?", "-h", "--help"]
        for item in help:
            if item in args:
                show_usage()
                sys.exit(0)

        self._script = args.pop(0)
        self._parse_command(args)

    def _parse_command(self, args):
        "Identify requested command and parse arguments"

        commands = {
            "full-upgrade":   FullUpgradeCommand,
            "install":        InstallCommand,
            "make-bootstrap": MakeBootstrapCommand,
            "make-dist":      MakeDistributionCommand,
            "upgrade":        UpgradeCommand
        }

        constructor = None
        for arg in args:
            if arg in commands:
                constructor = commands[arg]
                args.remove(arg)
                break

        if not constructor:
            # Default to package operations of nothing specified.
            for arg in args:
                if arg in PACKAGE_ACTIONS:
                    constructor = PackageCommand
                    break

        if not constructor:
            raise UsageError("no command specified")

        self._command = constructor
        self._command.parse_args(args, self)


# Start here.
if __name__ == "__main__":
    try:
        Concierge().main()
    except FatalError as err:
        print(f"{err.message}")
    except UsageError as err:
        print(f"{err.message}\n")
        show_usage()
