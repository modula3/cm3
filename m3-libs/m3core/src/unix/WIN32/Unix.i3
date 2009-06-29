(* $Id: Unix.i3,v 1.3 2009-06-29 19:20:42 jkrell Exp $ *)

(*
msvcr*.dll exposes a fair amount of "unix i/o".
For some sort of portability, it is probably reasonable to expose
most of it, somewhere.
*)

INTERFACE Unix;

FROM Ctypes IMPORT int, long, const_char_star;

<*EXTERNAL "_access"*> PROCEDURE access (path: const_char_star; mod: int): int;
<*EXTERNAL "_chdir"*> PROCEDURE chdir (path: const_char_star): int;
<*EXTERNAL "_close"*> PROCEDURE close (d: int): int;
<*EXTERNAL*> PROCEDURE exit (i: int);
<*EXTERNAL "_isatty"*> PROCEDURE isatty (filedes: int): int;
<*EXTERNAL "_lseek"*> PROCEDURE lseek (d: int; offset: long; whence: int): long;
<*EXTERNAL "_mkdir"*> PROCEDURE mkdir (path: const_char_star; mode: int): int;
<*EXTERNAL "_open"*> PROCEDURE open (name: const_char_star; flags, mode: int): int;
<*EXTERNAL*> PROCEDURE rename (from, to: const_char_star): int;
<*EXTERNAL "_rmdir"*> PROCEDURE rmdir (path: const_char_star): int;
<*EXTERNAL*> PROCEDURE system (string: const_char_star): int;
<*EXTERNAL "_tell"*> PROCEDURE tell (d: int): long;
<*EXTERNAL "_unlink"*> PROCEDURE unlink (path: const_char_star): int;

(* _lseeki64, telli64, etc. *)

END Unix.
