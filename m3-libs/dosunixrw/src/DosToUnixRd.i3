(*
A "DosToUnixRd.T" is a filter of Reader streams
which converts all DOS line ends (CR/LF) to UNIX ones (LF).

not well tested (only seek, but not getSub)
*)

INTERFACE DosToUnixRd;

IMPORT Rd;

TYPE
  T <: Public;
  Public = Rd.T OBJECT METHODS init (rd: Rd.T): T END;

PROCEDURE New (source: Rd.T): T;
(* Return a line end filter whose source is the reader "source". *)

(*
The call "New(rd)" is equivalent to

RETURN NEW(T).init(rd)
*)

END DosToUnixRd.
