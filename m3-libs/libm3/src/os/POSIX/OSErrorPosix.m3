(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Fri Dec  2 08:27:28 PST 1994 by kalsow  *)
(*      modified on Fri Jul 15 15:13:09 PDT 1994 by mcjones *)
(*      modified on Thu May  6 13:34:39 PDT 1993 by mjordan *)

MODULE OSErrorPosix EXPORTS OSError, OSErrorPosix;

IMPORT Atom, AtomList, Fmt, OSError, Text, Uerror;

VAR cache := ARRAY [0..Uerror.Max] OF Atom.T {NIL, ..};
(* The table is initialized lazily. *)

PROCEDURE NewAtom (n: CARDINAL): Atom.T =
  BEGIN
    RETURN Atom.FromText("errno=" & Fmt.Int(n));
  END NewAtom;

PROCEDURE ErrnoAtom(n: CARDINAL): Atom.T =
  BEGIN
    IF (n < NUMBER (cache)) THEN
      IF cache[n] = NIL THEN cache[n] := NewAtom(n) END;
      RETURN cache[n]
    ELSE
      RETURN NewAtom (n);
    END;
  END ErrnoAtom;

EXCEPTION CheckedRuntimeError; <*FATAL CheckedRuntimeError*>

PROCEDURE AtomToErrno(a: Atom.T): CARDINAL =
  VAR t := Atom.ToText(a); n := 0; c: CHAR;
  BEGIN
    IF NOT Text.Equal(Text.Sub(t, 0, 6), "errno=") THEN
      RAISE CheckedRuntimeError
    END;
    FOR i := 6 TO Text.Length(t)-1 DO
      c := Text.GetChar(t, i);
      IF '0' <= c AND c <= '9' THEN
        n := n * 10 + (ORD(c) - ORD('0'))
      ELSE
        RAISE CheckedRuntimeError
      END
    END;
    RETURN n
  END AtomToErrno;

PROCEDURE Raise0(errno: INTEGER) RAISES {OSError.E} =
  BEGIN
    RAISE OSError.E(
      NEW(AtomList.T, head := ErrnoAtom(errno), tail := NIL))
  END Raise0;

PROCEDURE Raise() RAISES {OSError.E} =
  BEGIN
    Raise0(Uerror.errno)
  END Raise;

BEGIN
END OSErrorPosix.
