(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE OSWIN32 EXPORTS OS;

IMPORT FS, M3toC, Msg, OSError, OSErrorWin32, Pathname, WinBase, WinDef;
IMPORT Fmt, Text2, WinError, WinNT;
FROM Wr IMPORT EOL;

PROCEDURE GetDiskSpace (dir: TEXT): INTEGER =
  VAR
    path, parent: TEXT;
    sectorsPerCluster, bytesPerSector, freeClusters, 
    totalClusters: WinDef.DWORD;
  BEGIN
    TRY
      path := FS.GetAbsolutePathname (dir);
      IF (path = NIL) THEN RETURN LAST (INTEGER); (* optimist! *) END;
      parent := Pathname.Prefix (path);
      WHILE (parent # NIL) AND NOT FileNameEq (parent, path) DO
        path := parent;
        parent := Pathname.Prefix (path);
      END;
      IF WinBase.GetDiskFreeSpace (M3toC.CopyTtoS (path), 
                                   ADR (sectorsPerCluster),
                                   ADR (bytesPerSector), ADR (freeClusters),
                                   ADR (totalClusters)) = 0 THEN
        OSErrorWin32.Raise ();
      END;
      RETURN bytesPerSector * sectorsPerCluster DIV 1024 * freeClusters 
      DIV 1024;
    EXCEPT OSError.E (ec) =>
      Msg.Warn ("Unable to determine free space in: ", dir, Err (ec));
      RETURN LAST (INTEGER); (* optimist! *)
    END;
  END GetDiskSpace;

CONST
  WarnWS1 =
    "CM3 currently cannot handle filenames with white space very well." & EOL &
    "The name `";

  WarnWS2 =
    "' has been shortened to `";

  WarnWS3 = "'." & EOL &
    "Please try to avoid using spaces in CM3 project filenames.";

PROCEDURE FilenameWithoutSpaces (fn: TEXT): TEXT =
  VAR res := fn;
  BEGIN
    Msg.Debug("FilenameWithoutSpaces `", fn, "'");
    IF Text2.FindChars(fn) THEN
      TRY
        res := GetShortFilename (fn);
        Msg.Warn(WarnWS1 & fn, WarnWS2 & res, WarnWS3);
      EXCEPT
        Error(e) =>
        Msg.Warn("Cannot compute short filename for `" & fn & "'." & EOL,
                 e & EOL,
                 "The installation will probably not work without " &
                 "manual fixes.");
      END;
    END;
    Msg.Debug(" => ", res);
    RETURN res;
  END FilenameWithoutSpaces;

PROCEDURE GetShortFilename (longFilename: TEXT): TEXT
   RAISES {Error} =
  (* Return the MS-DOS 8.3 mangled short filename that represents 
     longFilename on this host. *)
  CONST
    MaxPathLength = 1024; (* not counting null terminator *)
  TYPE
    Buffer = ARRAY [0..MaxPathLength] OF CHAR;
  VAR
    buffer: Buffer := Buffer{'\000', ..};
    lpszLongPath: WinNT.LPCSTR;
    lpszShortPath: WinNT.LPSTR;
    cchBuffer: WinDef.DWORD := MaxPathLength;
    retVal: WinDef.DWORD;
  BEGIN (* GetShortFilename *)
    lpszShortPath := ADR(buffer);
    lpszLongPath := M3toC.SharedTtoS(longFilename);
    retVal := WinBase.GetShortPathNameA(lpszLongPath,
                                        lpszShortPath, cchBuffer);
    M3toC.FreeSharedS(longFilename, lpszLongPath);
    IF (retVal = 0) OR (retVal = WinError.ERROR_INVALID_PARAMETER) OR
      (retVal > MaxPathLength) THEN
      RAISE Error("WinBase.GetShortPathNameA returned " &
            Fmt.Int(retVal));
    ELSE
      RETURN M3toC.CopyStoT(lpszShortPath);
    END; (* if *)
  END GetShortFilename;

BEGIN
END OSWIN32.
