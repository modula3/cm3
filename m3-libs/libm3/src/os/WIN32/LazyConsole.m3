(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE LazyConsole;

IMPORT File, FileWin32, OSError, Terminal, WinBase, WinCon, WinDef;

TYPE
  T = Terminal.T OBJECT
    hd    : WinDef.INT32;
    setup : BOOLEAN := FALSE;
  OVERRIDES
    read   := Read;
    write  := Write;
    close  := Close;
    status := Status;
  END;

PROCEDURE New (hd: WinDef.INT32; ds: FileWin32.DirectionSet): File.T =
  BEGIN
    RETURN NEW (T, ds := ds, hd := hd);
  END New;

PROCEDURE Read (t: T;  VAR(*OUT*) b: ARRAY OF File.Byte;
                mayBlock: BOOLEAN): INTEGER
  RAISES {OSError.E} =
  BEGIN
    Init (t);
    RETURN Terminal.T.read (t, b, mayBlock);
  END Read;

PROCEDURE Write (t: T;  READONLY b: ARRAY OF File.Byte)
  RAISES {OSError.E} =
  BEGIN
    IF (NUMBER (b) <= 0) THEN RETURN; END;
    Init (t);
    Terminal.T.write (t, b);
  END Write;

PROCEDURE Close (t: T)
  RAISES {OSError.E} =
  BEGIN
    IF (t.setup) THEN
      Terminal.T.close (t);
    END;
  END Close;

PROCEDURE Status (<*UNUSED*> t: T): File.Status =
  VAR status: File.Status;
  BEGIN
    status.modificationTime := 0.0d0;
    status.type := Terminal.FileType;
    status.size := 0L;
    RETURN status;
  END Status;

PROCEDURE Init (t: T) =
  BEGIN
    IF (NOT t.setup) THEN
      t.setup := TRUE;
      EVAL WinCon.AllocConsole ();  (* make sure we've got one! *)
      t.handle := WinBase.GetStdHandle (t.hd);
    END;
  END Init;

BEGIN
END LazyConsole.
