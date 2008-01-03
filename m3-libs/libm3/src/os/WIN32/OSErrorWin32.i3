(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Tue Dec 20 08:37:54 PST 1994 by kalsow  *)
(*      modified on Fri Feb  5 14:17:09 PST 1993 by mjordan *)

INTERFACE OSErrorWin32;

(* Win32 *)

IMPORT Atom, OSError;

PROCEDURE ErrnoAtom(n: CARDINAL): Atom.T;
(* Return an atom for error value "n". *)

PROCEDURE Raise() RAISES {OSError.E};
(* == RAISE OSError.E(ErrnoAtom(WinBase.GetLastError())) *)

PROCEDURE Raise0(errno: INTEGER) RAISES {OSError.E};
(* == RAISE OSError.E(AtomList.List1(ErrnoAtom(errno))) *)

END OSErrorWin32.
