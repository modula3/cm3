(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* FileSysExtra.m3 *)
(* Last modified on Fri Mar  4 17:10:11 PST 1994 by wobber *)
(*      modified on Wed Feb 23 09:12:57 PST 1994 by kalsow *)

UNSAFE MODULE FileSysExtra EXPORTS FileSys;

IMPORT M3toC, OSError, OSErrorPosix, Umount;

FROM Ctypes IMPORT int;

PROCEDURE FreeDiskSpace(fn: FN) : CARDINAL RAISES {OSError.E} =
  VAR
    status: int;
    p := M3toC.TtoS(fn);
    fs: Umount.struct_statfs;
  BEGIN
    status := Umount.statfs (
      p, LOOPHOLE(ADR(fs), Umount.struct_statfs_star), BYTESIZE(fs));
    IF status = -1 THEN OSErrorPosix.Raise(); END;
    RETURN MAX(fs.f_bfree, 0);
  END FreeDiskSpace;

BEGIN
END FileSysExtra.
