(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last modified on Thu Apr 22 15:48:59 PDT 1993 by wobber *)

UNSAFE MODULE OSSupport;

IMPORT OSError, File, FileWr, WrClass;
IMPORT Unix, OSErrorPosix, FilePosix;

REVEAL
  T = FileWr.T BRANDED OBJECT
    fd: INTEGER;
  OVERRIDES
    init := MyInit;
  END;

PROCEDURE MyInit(t: T; f: File.T; buffered: BOOLEAN := TRUE) : FileWr.T
    RAISES {OSError.E} =
  BEGIN
    t.fd := f.fd;
    RETURN FileWr.T.init(t, f, buffered);
  END MyInit;

PROCEDURE Sync(wr: T) RAISES {OSError.E} =
  BEGIN
    IF Unix.fsync (wr.fd) < 0 THEN OSErrorPosix.Raise(); END;
  END Sync;

PROCEDURE Truncate(wr: T) RAISES {OSError.E} =
  BEGIN
    WrClass.Lock(wr);
    TRY
      IF Unix.ftruncate(wr.fd, wr.cur) < 0 THEN
        OSErrorPosix.Raise();
      END;
    FINALLY
      WrClass.Unlock(wr);
    END;
  END Truncate;
  
BEGIN
END OSSupport.


