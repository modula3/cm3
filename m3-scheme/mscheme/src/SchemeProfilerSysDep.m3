(* $Id$ *)
MODULE SchemeProfilerSysDep;
IMPORT SchemeUnixDeps;
FROM Ctypes IMPORT int;

PROCEDURE getrusage(who: int; VAR r: SchemeUnixDeps.struct_rusage): int = 
  BEGIN RETURN SchemeUnixDeps.getrusage(who,r) END getrusage;

BEGIN END SchemeProfilerSysDep.

