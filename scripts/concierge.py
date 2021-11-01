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

import os
import platform
import re
import shlex
import shutil
import subprocess
import sys

from pathlib import Path


# The canonical package set defined in pkginfo.txt.
ALL = "all"

# Operating system classifications.
POSIX = "POSIX"
WIN32 = "WIN32"


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


class Platform:
    "Describes compilation host or target"

    def __init__(self, name = None):
        self._name = name
        if not self._name:
            arch = self._map_arch(platform.machine())
            os = platform.system()
            self._name = f"{arch}_{os}".upper()

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
        return self.name() in ["I386_NT", "NT386"]

    def has_only_c_backend(self):
        "Not supported by either GCC or the integrated backend"
        return not self.has_gcc_backend() and not self.has_integrated_backend()

    def has_serial(self):
        return self.is_win32()

    def is_posix(self):
        return self.os() == POSIX

    def is_win32(self):
        return self.os() == WIN32

    def name(self):
        "As recognized by cm3"
        return self._name

    def os(self):
        "As recognized by cm3"
        name = self.name()

        if (name == "NT386"      or
            name.endswith("_NT") or
            name.endswith("_MINGW")):
            return WIN32
        else:
            return POSIX

    def _map_arch(self, arch):
        return "amd64" if arch == "x86_64" else arch


class Cm3:
    "Describes cm3 build environment"

    def __init__(self, script = None, target = None):
        # Used to find location of source directory.
        self._script = script or sys.argv[0]

        # Compiling *on* host
        self._host = self._sniff_host()

        # Compiling *for* target
        if target:
            self._target = target if isinstance(target, Platform) else Platform(target)
        else:
            self._target = self._sniff_target()

        # True to use the C backend.
        self._cbackend = False

        # Captures "-D" command-line defines intended for cm3.
        self._defines = []

        # Captures any other command-line arguments intended for cm3.
        self._flags = []

    def _sniff_host(self):
        "Guess the host platform.  We always guess, it cannot be overriden."

        try:
            # Ask cm3.
            proc = subprocess.run([str(self.exe()), "-version"], stdout=subprocess.PIPE, errors="ignore")
            proc.check_returncode()
            for line in proc.stdout.splitlines():
                # Check for legacy platforms.
                if platform.system() == "Windows":
                    if (line.startswith("Critical Mass Modula-3 version 5.1.") or
                        line.startswith("Critical Mass Modula-3 version 5.2.") or
                        line.startswith("Critical Mass Modula-3 version d5.5.")):
                        return Platform("NT386")

                # Otherwise let cm3 say what the host is.
                host = line.find("host: ")
                if host >= 0:
                    return Platform(line[host + 6:].rstrip())
        except:
            pass

        # If there's a problem, we'll make our best guess.
        return Platform()

    def _sniff_target(self):
        "Guess the target platform"

        try:
            # Ask cm3.
            proc = subprocess.run([str(self.exe()), "-version"], stdout=subprocess.PIPE, errors="ignore")
            proc.check_returncode()
            for line in proc.stdout.splitlines():
                target = line.find("target: ")
                if target >= 0:
                    return Platform(line[target + 8:].rstrip())
        except:
            pass

        # If there's a problem, assume we're compiling for the host machine.
        return self.host()

    def build(self, *paths):
        "Relative to root of current build directory"
        return self.source(*paths) / self.build_dir()

    def build_dir(self):
        "Basename of build directory"
        return self.config()

    def config(self):
        return self.target().name()

    def defines(self):
        "Any '-D' command-line arguments intended for cm3"
        return self._defines

    def env(self):
        "Execution environment for cm3 child processes"
        return dict(
            os.environ,
            CM3_INSTALL=str(self.install()),
            CM3_ROOT=str(self.source()),
            CM3_TARGET=self.target().name()
        )

    def exe(self):
        "Full path to cm3 executable"

        def fail():
            raise Exception("environment variable CM3_INSTALL not set AND cm3 not found in PATH; please fix")

        # Environment may override name of executable.
        basename = os.environ.get("CM3", "cm3")

        # If the environment specified a path, it should work as-is.
        candidate = Path(basename)
        if len(candidate.parents) > 1:
            if candidate.is_file() and os.access(candidate, os.X_OK):
                return candidate.resolve()
            else:
                fail()

        # Environment may also override search path.
        if os.environ.get("CM3_INSTALL"):
            candidate = Path(os.environ["CM3_INSTALL"], basename)
            if candidate.is_file() and os.access(candidate, os.X_OK):
                return candidate
            else:
                fail()

        # With no overrides, we search PATH.
        candidate = self.find_exe(basename)
        if candidate is None:
            fail()

        return candidate

    def find_exe(self, basename):
        "Look for an executable in PATH"

        if os.environ.get("PATHEXT"):
            # Search PATH on Windows.  There is no separate check for
            # executable access, that is determined by the extension.
            extensions = os.environ["PATHEXT"].split(";")
            for dir in os.get_exec_path():
                for ext in extensions:
                    candidate = Path(dir, basename).with_suffix(ext)
                    if candidate.is_file():
                        return candidate
        else:
            # Search PATH on Posix.
            for dir in os.get_exec_path():
                candidate = Path(dir, basename)
                if candidate.is_file() and os.access(candidate, os.X_OK):
                    return candidate

        # Not found.
        return None

    def flags(self):
        "Command-line arguments intended for cm3"
        return self._flags

    def host(self):
        return self._host

    def install(self, *paths):
        "Relative to root of current installation directory"
        exe_path = self.exe();
        bin_dir = exe_path.parent
        install_dir = bin_dir.parent
        return install_dir.joinpath(*paths)

    def source(self, *paths):
        "Relative to root of current source directory"
        script_path = Path(self._script).resolve()
        script_dir = script_path.parent
        source_dir = script_dir.parent
        return source_dir.joinpath(*paths)

    def set_cbackend(self, cbackend):
        "True to use the C backend"
        self._cbackend = cbackend

    def set_defines(self, defines):
        "List of defines for cm3"
        self._defines = defines

    def set_flags(self, flags):
        "List of compiler switches for cm3"
        self._flags = flags

    def target(self):
        "Current platform target"
        return self._target

    def use_c_backend(self):
        return self._cbackend or self.target().has_only_c_backend()

    def use_gcc_backend(self):
        return not self.use_c_backend() and self.target().has_gcc_backend()


class WithCm3:
    "Provides access to cm3 build context"

    def __init__(self, cm3):
        self._cm3 = cm3

    def build(self, *paths):
        "Relative to root of current build directory"
        return self.cm3().build(*paths)

    def build_dir(self):
        "Basename of build directory"
        return self.cm3().build_dir()

    def cm3(self):
        return self._cm3

    def config(self):
        return self.cm3().config()

    def env(self):
        "Execution environment for cm3 child processes"
        return self.cm3().env()

    def exe(self):
        "Full path to cm3 executable"
        return self.cm3().exe()

    def install(self, *paths):
        "Relative to root of current installation directory"
        return self.cm3().install(*paths)

    def source(self, *paths):
        "Relative to root of current source directory"
        return self.cm3().source(*paths)

    def target(self):
        "Current platform target"
        return self.cm3().target()


class PackageDatabase(WithCm3):

    def __init__(self, cm3):
        super(PackageDatabase, self).__init__(cm3)

        # There is an order dependency here, sets must be loaded
        # before the index.
        self._load_package_sets()
        self._load_package_index()

    def all_packages(self):
        "Canonical list of packages, in dependency order, as defined in pkginfo.txt"
        return self._package_sets[ALL]

    def get_package_paths(self, names):
        "Locations of all requested packages"
        return [self.get_package_path(pkg) for pkg in self.get_packages(names)]

    def get_package_path(self, name):
        "Location of package relative to root of source tree"
        try:
            return self._package_index[name]
        except:
            raise Exception(f"package {name} not found")

    def get_packages(self, names):
        "List of requested packages, in dependency order"

        # Incorporate each listed package or set into requested.
        requested = set()
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

        # Return the requested packages in canonical order.
        packages = []
        for package in self.all_packages():
            if package in requested:
                packages.append(package)

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
        if name == "m3cc":   return self.target().has_gcc_backend()
        if name == "m3gdb":  return os.environ.get("M3GDB") and self.target().has_gdb()
        if name == "serial": return os.environ.get("HAVE_SERIAL") or self.target().has_serial()
        if name == "tapi":   return self.target().is_win32()
        if name == "tcl":    return os.environ.get("HAVE_TCL")

        return True

    def _load_package_index(self):
        "Scan the source directory for available packages"

        # Find all the fully-qualified package paths.
        package_paths = []

        root = self.source()
        for dir, children, files in os.walk(root):
            dir = Path(dir)

            # src may be a package directory
            if dir.name == "src" and "m3makefile" in files:
                package_dir = dir.parent
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
            # Find the canonical name of the package.
            package_name = str(package_path)
            while package_name not in package_list and package_name.find("/") >= 0:
                package_name = package_name[package_name.find("/")+1:]

            # Index the package by its canonical name.
            if package_name in package_list:
                self._package_index[package_name] = package_path

    def _load_package_sets(self):
        "Read package definitions from pkginfo.txt"

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

    def __init__(self, cm3, options = None):
        super(PackageAction, self).__init__(cm3)
        self._options = options or dict()
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

    def keep_going(self):
        "Ignore errors and forge ahead, i.e., '-k'"
        return self.option("keep_going")

    def option(self, name):
        return self.options().get(name, False)

    def options(self):
        return self._options

    def no_action(self):
        return self.option("no_action")

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

    def run_env(self, package_path, vars):
        "Execute a command defined in environment"
        for var in vars:
            cmd = os.environ.get(var)
            if cmd:
                cwd = self.source(package_path)
                print("cd", cwd)
                print(cmd)
                if not self.no_action():
                    proc = subprocess.run(
                        cmd,
                        cwd=cwd,
                        env=self.env(),
                        stdout=subprocess.PIPE,
                        stderr=subprocess.STDOUT,
                        errors="ignore"
                    )
                    sys.stdout.write(proc.stdout)
                    proc.check_returncode()
                return True
        return False

    def success(self):
        return self._success

    def defines(self):
        "List of '-D' arguments to pass to cm3"

        defines = [
            f"-DBUILD_DIR={self.build_dir()}",
            f"-DROOT={self.source()}",
            f"-DTARGET={self.target().name()}"
        ]

        if self.use_c_backend():
            defines.append(f"-DM3_BACKEND_MODE=C")

        return defines + self.cm3().defines()

    def flags(self):
        "Pass-through compilation flags for cm3"
        return self.cm3().flags()

    def use_c_backend(self):
        "Tell cm3 to use the C backend?"
        return self.cm3().use_c_backend()

    def build_args(self):
        "Any arguments for bulid commands passed in the environment"
        return self.split_env("BUILDARGS")

    def clean_args(self):
        "Any arguments for clean commands passed in the environment"
        return self.split_env("CLEANARGS")

    def ship_args(self):
        "Any arguments for ship commands passed in the environment"
        return self.split_env("SHIPARGS")

    def split_env(self, var_name):
        "Split environment arguments on whitespace allowing for quoting as in the shell"
        return shlex.split(os.environ.get(var_name, ""))


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
        if not self.run_env(package_path, ["CM3_BUILDGLOBAL", "BUILDGLOBAL"]):
            self.run(package_path, ["-build"] + self.build_args())


class BuildLocal(PackageAction):
    "Build package with local overrides"

    def execute_path(self, package_path):
        if not self.run_env(package_path, ["CM3_BUILDLOCAL", "BUILDLOCAL"]):
            self.run(package_path, ["-build", "-override"] + self.build_args())


class CleanGlobal(CleanAction):
    "Clean package without overrides"

    def execute_path(self, package_path):
        if not self.run_env(package_path, ["CM3_CLEANGLOBAL", "CLEANGLOBAL"]):
            self.run(package_path, ["-clean"] + self.clean_args())


class CleanLocal(CleanAction):
    "Clean package with local overrides"

    def execute_path(self, package_path):
        if not self.run_env(package_path, ["CM3_CLEANLOCAL", "CLEANLOCAL"]):
            self.run(package_path, ["-clean", "-override"] + self.clean_args())


class RealClean(CleanAction):
    "Remove target directory of package"

    def execute_path(self, package_path):
        if self.run_env(package_path, ["CM3_REALCLEAN", "REALCLEAN"]):
            return

        build_dir = self.build(package_path)
        print("rm", "-Rf", build_dir)
        if self.no_action():
            return

        if build_dir.is_dir():
            shutil.rmtree(build_dir)


class Ship(PackageAction):
    "Install package"

    def execute_path(self, package_path):
        if not self.run_env(package_path, ["CM3_SHIP", "SHIP"]):
            self.run(package_path, ["-ship"] + self.ship_args())


class BuildShip(PackageAction):
    "Build package *without* overrides and install it"

    def __init__(self, cm3, options):
        super(BuildShip, self).__init__(cm3, options)
        self._buildglobal = BuildGlobal(cm3, options)
        self._ship = Ship(cm3, options)

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
        super(CompositeAction, self).__init__(cm3)
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
        super(WithPackageDb, self).__init__(cm3)
        self._package_db = package_db

    def get_package_paths(self, names):
        "Locations of all requested packages"
        return self.package_db().get_package_paths(names)

    def is_package(self, name):
        "Name identifies a package or package set"
        return self.package_db().is_package(name)

    def package_db(self):
        return self._package_db


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

    def __init__(self, cm3, package_db, options = None):
        super(WithPackageActions, self).__init__(cm3, package_db)
        self._options = options or dict()

    # Note that this won't generate a "build" method, because that is
    # already defined in WithCm3 with an entirely different meanning.
    # It is better to be specific with "buildglobal" or "buildlocal"
    # if that is what is needed.
    def __getattr__(self, method_name):
        try:
            constructor = PACKAGE_ACTIONS[method_name]
            action = constructor(self.cm3(), self.options())

            def executor(packages):
                paths = self.get_package_paths(packages)
                print(method_name, *paths)
                action.execute(paths)

            return executor
        except KeyError:
            raise AttributeError

    def option(self, name):
        return self.options().get(name, False)

    def options(self):
        return self._options


class ConciergeCommand(WithPackageActions):
    "A top-level command made to the concierge"

    def parse_args(self, args):
        "Parse arguments common to all concierge commands"
        self._parse_cbackend(args)
        self._parse_cm3defines(args)
        self._parse_cm3flags(args)
        self._parse_options(args)
        self._parse_packages(args)

    def _parse_cbackend(self, args):
        "Look for argument to request use of C backend"
        cbackend = False
        for flag in ["C", "c"]:
            if flag in args:
                cbackend = True
                args.remove(flag)
        self.cm3().set_cbackend(cbackend)

    def _parse_cm3defines(self, args):
        "Look for defines that need to be passed-through to cm3"
        cm3defines = [arg for arg in args if arg.startswith("-D")]
        args[:] = [arg for arg in args if arg not in cm3defines]
        self.cm3().set_defines(cm3defines)

    def _parse_cm3flags(self, args):
        "Look for flags that need to be passed-through to cm3"
        flags = [
            "boot",
            "commands",   # list system commands as they are performed
            "debug",      # dump internal debugging information
            "keep",       # preserve intermediate and temporary files
            "override",   # include the "m3overrides" file
            "silent",     # produce no diagnostic output
            "times",      # produce a dump of elapsed times
            "trace",      # trace quake code execution
            "verbose",    # list internal steps as they are performed
            "why"         # explain why code is being recompiled
        ]

        cm3flags = [arg for arg in args if arg[0] == "-" and arg[1:] in flags]
        args[:] = [arg for arg in args if arg not in cm3flags]
        self.cm3().set_flags(cm3flags)

    def _parse_options(self, args):
        if "-k" in args:
            self._options["keep_going"] = True
            args.remove("-k")

        if "-l" in args:
            self._options["list_only"] = True
            args.remove("-l")

        if "-n" in args:
            self._options["no_action"] = True
            args.remove("-n")

    def _parse_packages(self, args):
        "Look for package arguments in the command-line"
        self._packages = [arg for arg in args if self.is_package(arg)]
        args[:] = [arg for arg in args if arg not in self._packages]


class PackageCommand(ConciergeCommand):
    "Clean, build, and/or ship packages"

    def execute(self):
        packages = self.get_package_paths(self.packages())

        if self.option("list_only"):
            self.list_packages(packages)
            return

        self.action().execute(packages)

    def action(self):
        "Return the action requested on the command-line"
        actions = []
        for arg in self._actions:
            actions.append(PACKAGE_ACTIONS[arg](self.cm3(), self.options()))

        if len(actions) == 0:
            raise Exception("no actions specified")

        return CompositeAction(self.cm3(), actions)

    def list_packages(self, packages):
        for package in packages:
            print(package)

    def packages(self):
        "Return the packages requested on the command-line"
        if not self._packages:
            raise Exception("no packages specified")
        return self._packages

    def parse_args(self, args):
        "Parse arguments to the package command"
        super(PackageCommand, self).parse_args(args)
        self._parse_actions(args)

    def _parse_actions(self, args):
        "Look for action argumens in the command-line"
        self._actions = []
        while args and args[0] in PACKAGE_ACTIONS:
            self._actions.append(args.pop(0))


class UpgradeCommand(ConciergeCommand):
    "Upgrade the cm3 compiler and core system"

    def execute(self):
        base_packages = ["+front", "+m3bundle", "-m3cc"]

        # Some things must be cleaned up before an upgrade can begin.
        self._clean()

        # Build the compiler using the installed version of the system.
        self._run_pass(base_packages)

        # Use the new compiler to build the core system.  There is no
        # need to rebuild GCC in the second pass.
        self._install_config()
        self._run_pass(base_packages, build_gcc = False)

    def _clean(self):
        "Delete lingering cm3cg so old compiler can't use it"
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
        build_gcc = not self._skip_gcc and build_gcc and self.target().has_gcc_backend()
        if build_gcc:
            self.realclean(["m3cc"])
            self.buildship(["m3cc"])

        # Install the binaries.
        if build_gcc:
            self._ship_back()
        self._ship_front()

    def parse_args(self, args):
        "Parse arguments to the upgrade command"
        super(UpgradeCommand, self).parse_args(args)

        # Look for a flag to ignore gcc backend.
        self._skip_gcc = os.environ.get("OMIT_GCC")
        for flag in ["nogcc", "omitgcc", "skipgcc"]:
            if flag in args:
                self._skip_gcc = True
                args.remove(flag)

    def _ship_back(self):
        self._copy_compiler(self.build("m3-sys/m3cc"), self.install("bin"))

    def _ship_front(self):
        self._copy_compiler(self.build("m3-sys/cm3"), self.install("bin"))

    def _copy_compiler(self, src, dst):
        "Copy compiler executables to their installed locations"

        executables = ["cm3", "cm3cg", "mips-tfile", "mklib"]

        # Ensure destination directory exists.
        self.mkdir(dst)

        # Copy executables on Posix.
        for exe in executables:
            item = src / exe
            self.cp(item, dst)

        # Copy executables on Win32.
        if os.environ.get("PATHEXT"):
            extensions = os.environ["PATHEXT"].split(";")
            for exe in executables:
                for ext in extensions:
                    item = (src / exe).with_suffix(ext)
                    self.cp(item, dst)
                # Copy debug info.
                item = (src / exe).with_suffix(".pdb")
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

        self.install("bin/cm3.cfg").write_text(
            f"""if not defined("SL") SL = "/" end
if not defined("HOST") HOST = "{self.config()}" end
if not defined("TARGET") TARGET = HOST end
INSTALL_ROOT = (path() & SL & "..")
include(path() & SL & "config" & SL & TARGET)
"""
        )

    def cp(self, src, dst):
        if src.is_file():
            print("cp", "-P", src, dst)
            if not self.no_action():
                shutil.copy(src, dst)

    def mkdir(self, dir):
        print("mkdir", "-p", dir)
        if not self.no_action():
            dir.mkdir(parents=True, exist_ok=True)

    def no_action(self):
        return self.option("no_action")

    def rm(self, file):
        if file.is_file():
            print("rm", "-f", file)
            if not self.no_action():
                file.unlink()

    def rmdir(self, dir):
        if dir.is_dir():
            print("rm", "-Rf", dir)
            if not self.no_action():
                shutil.rmtree(dir)


class FullUpgradeCommand(UpgradeCommand):
    "Upgrade the system and reinstall all packages"

    def execute(self):
        # Upgrade the compiler.
        super(FullUpgradeCommand, self).execute()

        # Clean, but there is no point in rebuilding GCC.
        self.realclean([ALL, "-m3cc"])

        # Reinstall all packages.
        self.buildship(self.packages())

    def packages(self):
        "Return the packages requested on the command-line"
        if not self._packages or self._packages[0].startswith("-"):
            self._packages.insert(0, ALL)

        return self._packages


class ConciergeParser(WithPackageDb):
    "Parse command-line arguments for the concierge"

    def parse_command(self, args):
        "Identify the requested command and parse its arguments"

        commands = {
            "full-upgrade": FullUpgradeCommand,
            "upgrade":      UpgradeCommand
        }

        constructor = None
        for arg in args:
            if arg in commands:
                constructor = commands[arg]
                args.remove(arg)
                break

        if not constructor:
            # Default to package operations if nothing specified.
            for arg in args:
                if arg in PACKAGE_ACTIONS:
                    constructor = PackageCommand
                    break

        if not constructor:
            raise Exception("no command given")

        command = constructor(self.cm3(), self.package_db())
        command.parse_args(args)

        return command


class Concierge:
    "aka, main"

    def __init__(self, args = None):
        # Capture command-line arguments.
        args = (args or sys.argv)[:]
        self._args = [arg for arg in args if arg]
        self._parse_args()

        # Setup cm3 build environment.
        self._cm3 = Cm3(self._script, self._target)

        # Load package database.
        self._package_db = PackageDatabase(self._cm3)

    def main(self):
        "Carry-out the requested command"
        command = self._parse_command()
        command.execute()

    def _parse_args(self):
        # Script location is used to find cm3 source directory.
        self._script = self._args.pop(0)

        # Scan for any command-line parameter that names a platform
        self._target = os.environ.get("CM3_TARGET")
        all_targets = self._all_platforms()
        for arg in self._args:
            for case_arg in [arg, arg.upper()]:
                if case_arg in all_targets:
                    self._target = case_arg
                    self._args.remove(arg)
                    return

    def _parse_command(self):
        "Try to make sense of the command-line arguments"
        parser = ConciergeParser(self._cm3, self._package_db)
        return parser.parse_command(self._args)

    def _all_platforms(self):
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


if __name__ == "__main__":
    Concierge().main()
