(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Fri Jul 15 15:43:03 PDT 1994 by mcjones *)
(*      modified on Mon Jan 25 15:47:43 PST 1993 by mjordan *)

INTERFACE OSErrorPosix;

(* Posix *)

IMPORT Atom, OSError;

PROCEDURE ErrnoAtom(n: CARDINAL): Atom.T;
(* Return an atom for "errno" value "n". *)

PROCEDURE Raise() RAISES {OSError.E};
(* == Raise0(Uerror.errno)) *)

PROCEDURE Raise0(errno: INTEGER) RAISES {OSError.E};
(* == RAISE OSError.E(AtomList.List1(ErrnoAtom(errno))) *)

PROCEDURE AtomToErrno(a: Atom.T): CARDINAL;
(* Return "n" such that "ErrnoAtom(n) = a", or cause a checked runtime
   error. *)

END OSErrorPosix.
