(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Dec  2 16:37:57 PST 1992 by wobber *)

INTERFACE ConnRW;

IMPORT ConnFD, Rd, Wr;

PROCEDURE NewRd(fd: ConnFD.T) : Rd.T;
   (* produces a reader from a generic connection handle *)

PROCEDURE NewWr(fd: ConnFD.T) : Wr.T;
   (* produces a writer from a generic connection handle *)

END ConnRW.

