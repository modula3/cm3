"all" is everything and what you should use if not sure.
 Extract it somewhere.
 add somewhere/bin to $PATH
   sh/bash> export PATH=somewhere/bin:$PATH
   csh> setenv PATH somewhere/bin:$PATH
 On some systems, e.g. FreeBSD prior to 8.0, OSF/1, you
   need to set LD_LIBRARY_PATH or similar to contain somewhere/lib.

 Try building some Modula-3 code with "cm3".


"min" is compiler, m3core, libm3
  Use it for "small" programs or if you want to build
  the system yourself, with some early steps skipped.
  It can be used similarly to "all".


"boot" is "advanced".
"boot" is the result of a "cross build"
This text should probably be a link to save user's confusion.
  Not what people usually think of as a cross build:
    The Modula-3 contributing to the compiler has been
      compiled to assembly source.
    The C code contributing to the compiler has been left
      uncompiled.
    Final assembly, C compilation, linking, is deferred
      to the target machine or at least to a second
      machine with cross assembler, C compiler, linker.
    Some cases might be different:
      e.g. I386_NT is has .obj files instead of assembly source

  Use "boot" if you just enjoy building more stuff or
  the all/min archives don't work for you, esp.
  if they are for a different OS version and compatibility
  has become broken. "boot" abstracts most of the ABI
  into C source and is therefore more portable.
  "boot" requires more expertise. You might have to edit
  the Makefile!
  "boot" gives you cm3.
    If not, back and investigate.
  Run it.
  It should say "unable to find cm3.cfg".
    If not, back and investigate.
  mkdir -p somewhere/bin
  cp cm3 somewhere/bin
  add somewhere/bin to $PATH
  Continue, e.g. with scripts/python/boot2.sh.
