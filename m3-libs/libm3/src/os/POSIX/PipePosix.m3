(* Copyright (C) 1992, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Thu May 27 14:39:25 PDT 1993 by mcjones *)
(*      modified on Tue Feb  2 18:39:44 PST 1993 by mjordan *)

MODULE PipePosix EXPORTS Pipe;

IMPORT Ctypes, FilePosix, OSError, OSErrorPosix, Unix;

PROCEDURE Open(VAR hr, hw: T) RAISES {OSError.E} =
  VAR fd: ARRAY [0..1] OF Ctypes.int;
  BEGIN
    IF Unix.pipe(fd) < 0 THEN OSErrorPosix.Raise() END;
    hr := FilePosix.NewPipe(fd := fd[Unix.readEnd], ds := FilePosix.Read);
    hw := FilePosix.NewPipe(fd := fd[Unix.writeEnd], ds := FilePosix.Write)
  END Open;

BEGIN
END PipePosix.
