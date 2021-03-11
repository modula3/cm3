(* $Id$ *)

INTERFACE SchemeProfilerSysDep;
FROM SchemeUnixDeps IMPORT struct_rusage;
FROM Ctypes IMPORT int;

PROCEDURE getrusage(who: int; VAR r: struct_rusage): int;

CONST Brand = "SchemeProfilerSysDep";

END SchemeProfilerSysDep.
