= The language

Modula-3 is an optionally safe systems programming language.
 It has garbage collection (for the optionally safe part).
 Extent implementations are compiled statically to native code.

= The implementation

Critical Mass is a complete mature implementation of Modula-3.

Being an optionally safe "systems" programming language, the
 vast bulk of the system is implemented in Modula-3,
 including the garbage collector, and parts of the compiler.

 C is used mainly as a thin portable wrapper to interoperate with C libraries
 without encoding underlying ABI details, like the precise layout
 of struct stat, or if some functions are really macros, etc.

= The backends

The system contains three backends presently.

 - A gcc-based backend for many platforms.
   This is a gcc "frontend" that reads Modula-3 IR (intermediate representation)
   and then runs the gcc backend.
   This produces textual assembly source that is then assembled.

   This works for example on Macos/amd64 ("Darwin"), Linux/amd64, Linux/x86,
   as well other architectures and other Unix systems: SPARC, Alpha, FreeBSD, NetBSD, OpenBSD, Solaris, Cygwin etc.

    m3-sys/m3cc

 - An "integrated" backend, written in Modula-3.
   It is "integrated" in that one executable cm3 contains the frontend and complete backend.
   This presently targets only the Windows/x86 system.
    Also known as NT386 or I386_NT.
    It writes object files directly.
    m3-sys/m3back/src/*x86*.m3

 - A C backend that writes out textual C that can then be
   compiled with a variety of C compilers including gcc, clang,
   Visual C++, Sun CC, and more. This has been tested
   on many systems, including the ones targetted by the other backends.
   The code that writes the C is itself written in Modula-3.
   The C is not meant to be read or maintained by humans, as it is a representation
   of a low level IR (intermediate representation).
    m3-sys/m3back/src/M3C.m3

 -  A working LLVM backend?

With some exception, the backends should generate code compatible with each other.
But there is at least one exception so this is best avoided.
Specifically related to nested functions or at least pointers to them.

= The libraries

The system includes a variety of libraries including but not limited to:
  - threading and locking
  - formated textual io (similar to printf)
  - networking (sockets)
  - custom GUI library ("Trestle")

The system also includes "bindings" so you can directly call Win32, OpenGL, X Windows, ODBC, etc.
 That is, C headers rewritten in Modula-3. Very much like P/Invoke in C#.

As well some sample applications such as a web browser, forms editor, and some games.

= Build system

The compiler is also integrated into its own build system.

The build system is mostly declarative, embedded in a somewhat general "scripting" language,
 known as "quake" (quick make?)

Architecturally it resembles scons, though the again the build system and compiler are linked.

The build system contains per-platform "configuration" files, written in the underlying quake
scripting language, providing functions like "compile a .c file", "link an executable"
and "link a shared library". In this respect the build system largely subsumes libtool.
 See m3-sys/cminstall/src/config-no-install

= Building

Because the system is written in Modula-3 building it from source can be challenging.

Several distribution formats are provided.

 - "boot" is generated C that produces the compiler, integrated and C backends, and build sytem.
   These live in one executable -- "cm3".
   "boot" also produces mklib which is used on Windows platforms only.
   The C code is almost portable to all systems, however for now it is platform-specific.
   Specifically it encodes sizeof(void*), endian, and Windows vs. Unix.
   In time this matrix should be reduced.

 - "min" is a minimal binary distribution containing little more than "boot" -- cm3 and mklib --
   but also two libraries, m3core and libm3.
     See m3-libs/m3core and m3-libs/libm3.
   m3core is the lowest level library containing threading, garbage collector, unistd/win32 bindings.
   The compiler and m3core are somewhat tightly coupled. The compiler outputs calls into m3core,
   such as for garbage collection barriers.
   libm3 is a higher level utility library containing things like growable vectors, linked lists, etc.

 - "all" is everything. Sometimes some things are omitted, such as if a system lacked X Windows installed.

 The use-cases are fairly obvious.
   "min" is small and binary. You can write small Modula-3 programs right away.
   "all" is larger and provides a larger assortment of libraries.

 To use "min" or "all", extract them such as with tar xf and add <root>/bin to the PATH environment variable.

 <insert link to HelloWorld and Larger tutorials>

 "boot" is if you "like" or "need" to build from source as much as possible.
 Note again that this is not human readable/writable source, but it is at least C.

 The usage is roughly:
  tar xf cm3-boot.tgz
  cd cm3-boot
  make
  # If there any errors here, start by looking at the top of Makefile.
  mkdir -p /cm3/bin
  mv {cm3,mklib} /cm3/bin
  export PATH=/cm3/bin:$PATH
  cd ...
  git clone ... cm3
  cd cm3/scripts/python
  sudo apt-get install python2
  ./boot2.py

At which point you have the equivalent of "all", but have built mostly from source.

= Packages and scripts

The Critical Mass system is split up informally into groups of packages.
Such as core, games, demo.
These are declared in scripts/pkginfo.txt.
  Package names are on the left. The rest of each line is the groups the packages are in.
  Packages are discovered throughout the source tree, identified roughly as directories
  containing "m3makefile".
  I believe the order in pkginfo.txt is the flattened dependency graph?

You can build packages like:
  cd m3-libs/m3core
  cm3
  cm3 -ship

"ship" is analogous to "make install".

todo: Building from not-near-leaf directories is not useful:
  cd m3-libs
  cm3
  # Ideally would build m3core, libm3, etc. in dependency order, perhaps even
  # concurrently, but presently fails.

Or you can do:
 cd scripts/python
 ./do-pkg.py m3core buildship

The advantage of the scripts is that you can specify multiple packages, and/or groups, and
the scripts loop over them, in dependency order.

So you can say:
  ./do-pkg.py m3core libm3 sysutils m3middle m3back cm3 buildship 

You can intermix groups with packages:
  ./do-pkg.py front tetris buildship 

Note that the dependency graph is not walked "down".
Only the packages requested are built.
 If their dependencies have never been build, you will get an error about a missing .m3exports file.
 If their dependencies are out of data, errors can occur.
But they are built in dependency order.

Something needs to be said about "overrides", and this part of the system revamped.
cm3 -x builds against in-tree package store, w/o -x builds against
shipped packages.

= Super scripts

There are also scripts like:

 ./upgrade.py
Rebuilds the compiler, given an already working compiler and already
working m3core and libm3 and updates it in place -- be careful.
Sometimes this must be preceded by ./do-pkg.py m3core libm3 buildship

./make-dist.py

Produces the various binary .tgz/.msi files.
In particular though, it builds a new compiler from a present working system,
then builds the compiler with itself, and then rebuilds the entire system
with that compiler. This should be the basis of future automation.

./boot1.py c <target>

Produces the boot C archives.
todo: Ability to provide multiple targets.

= Windows

On Windows users should run the "shortcut" on the start menu that produces
a working C++ comman line build environment.
That is, cl.exe should be in path, and %INCLUD% and %LIB% should be reasonably populated.

= Caveats

This documentation needs work.
One thing it glosses over is chosing a backend and specifying it to the scripts.
In particular, it is often a good idea to say "skipgcc" and/or "c" to use the C backend.
