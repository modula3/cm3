(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Dec  2 16:37:57 PST 1992 by wobber *)

INTERFACE ConnRW;

IMPORT StreamRd, StreamWr;
IMPORT ConnFD;

PROCEDURE NewRd(fd: ConnFD.T) : StreamRd.T;
   (* produces a stream reader from a generic connection handle *)

PROCEDURE NewWr(fd: ConnFD.T) : StreamWr.T;
   (* produces a stream writer from a generic connection handle *)

END ConnRW.
