(* $Id: Uuio.i3,v 1.3 2009-06-29 19:20:41 jkrell Exp $ *)

(*
msvcr*.dll exposes a fair amount of "unix i/o".
For some sort of portability, it is probably reasonable to expose
most of it, somewhere.
*)

INTERFACE Uuio;

FROM Ctypes IMPORT int, void_star, const_void_star, unsigned_int;

TYPE

<*EXTERNAL "_read"*> PROCEDURE read (d: int; buf: void_star; nbytes: unsigned_int): int;
<*EXTERNAL "_write"*> PROCEDURE write (d: int; buf: const_void_star; nbytes: unsigned_int): int;

END Uuio.
