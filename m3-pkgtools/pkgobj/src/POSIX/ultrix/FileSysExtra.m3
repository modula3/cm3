(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* FileSysExtra.m3 *)
(* Last modified on Fri Mar  4 16:26:33 PST 1994 by wobber *)
(*      modified on Wed Feb 23 09:12:57 PST 1994 by kalsow *)

UNSAFE MODULE FileSysExtra EXPORTS FileSys;

IMPORT M3toC, OSError, OSErrorPosix, Umnt;

FROM Ctypes IMPORT char_star, int, unsigned_int, unsigned_int_star;

PROCEDURE FreeDiskSpace(fn: FN) : CARDINAL RAISES {OSError.E} =
  VAR
    status: int;
    start: unsigned_int;
    p: char_star;
    fs: Umnt.fs_data;
  BEGIN
    p := M3toC.TtoS(fn);
    status := Umnt.getmnt (
      LOOPHOLE(ADR(start), unsigned_int_star),
      LOOPHOLE(ADR(fs), Umnt.fs_data_star),
      BYTESIZE(fs), Umnt.STAT_ONE, p);
    IF status = -1 THEN OSErrorPosix.Raise(); END;
    RETURN MAX(fs.fd_req.bfreen, 0);
  END FreeDiskSpace;

BEGIN
END FileSysExtra.
