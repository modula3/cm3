(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE OSWIN32 EXPORTS OS;

IMPORT FS, M3toC, Msg, OSError, OSErrorWin32, Pathname, WinBase, WinDef;

PROCEDURE GetDiskSpace (dir: TEXT): INTEGER =
  VAR
    path, parent: TEXT;
    sectorsPerCluster, bytesPerSector, freeClusters, totalClusters: WinDef.DWORD;
  BEGIN
    TRY
      path := FS.GetAbsolutePathname (dir);
      IF (path = NIL) THEN RETURN LAST (INTEGER); (* optimist! *) END;
      parent := Pathname.Prefix (path);
      WHILE (parent # NIL) AND NOT FileNameEq (parent, path) DO
        path := parent;
        parent := Pathname.Prefix (path);
      END;
      IF WinBase.GetDiskFreeSpace (M3toC.CopyTtoS (path), ADR (sectorsPerCluster),
                                   ADR (bytesPerSector), ADR (freeClusters),
                                   ADR (totalClusters)) = 0 THEN
        OSErrorWin32.Raise ();
      END;
      RETURN bytesPerSector * sectorsPerCluster DIV 1024 * freeClusters DIV 1024;
    EXCEPT OSError.E (ec) =>
      Msg.Warn ("Unable to determine free space in: ", dir, Err (ec));
      RETURN LAST (INTEGER); (* optimist! *)
    END;
  END GetDiskSpace;

BEGIN
END OSWIN32.
