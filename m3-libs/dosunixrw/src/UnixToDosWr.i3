(*
A "UnixToDosWr.T" is a filter of Writer streams
which converts all UNIX line ends (LF) to DOS ones (CR/LF).


untested and certainly non-working
*)

INTERFACE UnixToDosWr;

IMPORT Wr;

TYPE
  T <: Public;
  Public = Wr.T OBJECT METHODS init (wr: Wr.T; ): T END;

PROCEDURE New (target: Wr.T): T;
(* Return a line end filter whose destination is the writer "target". *)

(*
The call "New(p)" is equivalent to the following:

RETURN NEW(T).init(wr)
*)

END UnixToDosWr.
