(* $Id$ *)

(*
msvcr*.dll exposes a fair amount of "unix i/o".
For some sort of portability, it is probably reasonable to expose
most of it, somewhere.
*)

INTERFACE Unix;

FROM Ctypes IMPORT int, long, const_char_star;
FROM Utypes IMPORT off_t, size_t;

TYPE
  ptrdiff_t = INTEGER;
  size_t = CARDINAL; (* not correct *)

CONST

<*EXTERNAL*> PROCEDURE access (path: const_char_star; mod: int): int;
<*EXTERNAL*> PROCEDURE chdir (path: const_char_star): int;
<*EXTERNAL*> PROCEDURE close (d: int): int;
<*EXTERNAL*> PROCEDURE exit (i: int);
<*EXTERNAL*> PROCEDURE isatty (filedes: int): int;
<*EXTERNAL*> PROCEDURE lseek (d: int; offset: off_t; whence: int): off_t;
<*EXTERNAL*> PROCEDURE mkdir (path: const_char_star; mode: int): int;
<*EXTERNAL*> PROCEDURE open (name: const_char_star; flags, mode: int): int;
<*EXTERNAL*> PROCEDURE rename (from, to: const_char_star): int;
<*EXTERNAL*> PROCEDURE rmdir (path: const_char_star): int;
<*EXTERNAL*> PROCEDURE system (string: const_char_star): int;
<*EXTERNAL*> PROCEDURE tell (d: int): long;
<*EXTERNAL*> PROCEDURE unlink (path: const_char_star): int;

END Unix.
