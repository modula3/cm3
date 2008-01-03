(* Copyright 1992 Digital Equipment Corporation.            *)
(* Distributed only by permission.                          *)
(*                                                          *)
(* Last modified on Tue Dec 20 08:39:03 PST 1994 by kalsow  *)
(*      modified on Fri May  7 10:40:38 PDT 1993 by mcjones *)
(*      modified on Thu May  6 13:34:41 PDT 1993 by mjordan *)

UNSAFE MODULE OSErrorWin32 EXPORTS OSError, OSErrorWin32;

IMPORT OSError, Text;
IMPORT Atom, AtomList, Fmt;
IMPORT WinBase;

VAR cache := ARRAY [0..2000] OF Atom.T { NIL, .. };
(* The table is initialized lazily. *)

PROCEDURE NewAtom (n: CARDINAL): Atom.T =
  BEGIN
    RETURN Atom.FromText("ErrorCode=" & Fmt.Int(n) & ErrorMsg(n));
  END NewAtom;

PROCEDURE ErrorMsg (err: INTEGER): TEXT =
  VAR len : INTEGER;  buf: ARRAY [0..255] OF CHAR;
  BEGIN
    len := WinBase.FormatMessage (WinBase.FORMAT_MESSAGE_FROM_SYSTEM
                 + WinBase.FORMAT_MESSAGE_IGNORE_INSERTS + 254, NIL,
                 err, 16_400, ADR (buf), BYTESIZE (buf), NIL);
    len := MAX (0, MIN (len, NUMBER (buf)));
    WHILE (len > 0) AND (buf[len-1] = ' ') DO DEC (len); END;
    IF (len <= 0) THEN RETURN ""; END;
    RETURN ": " & Text.FromChars (SUBARRAY (buf, 0, len));
  END ErrorMsg;

PROCEDURE ErrnoAtom(n: CARDINAL): Atom.T =
  BEGIN
    IF (n < NUMBER (cache)) THEN
      IF cache[n] = NIL THEN cache[n] := NewAtom(n) END;
      RETURN cache[n];
    ELSE
      RETURN NewAtom (n);
    END;
  END ErrnoAtom;

PROCEDURE Raise0(errno: INTEGER) RAISES {OSError.E} =
  BEGIN
    RAISE OSError.E(
      NEW(AtomList.T, head := ErrnoAtom(errno), tail := NIL))
  END Raise0;

PROCEDURE Raise() RAISES {OSError.E} =
  BEGIN
    Raise0(WinBase.GetLastError());
  END Raise;

BEGIN
END OSErrorWin32.
