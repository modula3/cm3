(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

UNSAFE MODULE RegistryWIN32 EXPORTS Registry;

IMPORT M3toC, OS, Text, WinDef, WinReg, WinNT;

PROCEDURE Lookup (key: TEXT): TEXT =
  VAR
    HKEY_CLASSES_ROOT := LOOPHOLE(16_80000000, WinReg.HKEY);
    hKey: WinReg.HKEY;
    length: WinDef.DWORD := BYTESIZE (buf);
    buf : ARRAY [0..255] OF CHAR;
    str := M3toC.SharedTtoS (key);
    err := WinReg.RegOpenKeyEx (HKEY_CLASSES_ROOT, str, 0,
                                WinNT.KEY_READ, ADR (hKey));
  BEGIN 
    M3toC.FreeSharedS (key, str);
    IF err # 0 THEN RETURN NIL; END;
    EVAL WinReg.RegQueryValue (hKey, NIL, ADR (buf[0]), ADR (length));
    EVAL WinReg.RegCloseKey (hKey);
    RETURN Text.FromChars (SUBARRAY (buf, 0, length-1));
  END Lookup;

PROCEDURE LookupByExtension (ext: TEXT): TEXT = 
  VAR app, pgm: TEXT;  blank: INTEGER;
  BEGIN
    app := Lookup (ext);
    IF app = NIL THEN RETURN NIL; END;

    pgm := Lookup (app & "\\shell\\open\\command");
    IF pgm = NIL THEN RETURN NIL; END;

    blank := Text.FindCharR (pgm, ' ');
    IF (blank >= 0) THEN pgm := Text.Sub (pgm, 0, blank); END;

    IF OS.IsExecutable (pgm) THEN
      RETURN OS.GetAbsolutePath (pgm);
    END;

    RETURN NIL;
  END LookupByExtension;

BEGIN
END RegistryWIN32.

