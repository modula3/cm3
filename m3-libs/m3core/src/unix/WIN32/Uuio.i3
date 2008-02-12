(* $Id$ *)

(*
msvcr*.dll exposes a fair amount of "unix i/o".
For some sort of portability, it is probably reasonable to expose
most of it, somewhere.
*)

INTERFACE Uuio;

FROM Ctypes IMPORT int, void_star, const_void_star, unsigned_int;

TYPE

<*EXTERNAL*> PROCEDURE read (d: int; buf: void_star; nbytes: unsigned_int): int;
<*EXTERNAL*> PROCEDURE write (d: int; buf: const_void_star; nbytes: unsigned_int): int;

END Uuio.
