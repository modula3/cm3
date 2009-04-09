(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Dec  2 16:37:57 PST 1992 by wobber *)

INTERFACE SupConnRW;

IMPORT StreamRd, StreamWr;
IMPORT SupConnFD AS ConnFD;

PROCEDURE NewRd(fd: ConnFD.T) : StreamRd.T;
   (* produces a stream reader from a generic connection handle *)

PROCEDURE NewWr(fd: ConnFD.T) : StreamWr.T;
   (* produces a stream writer from a generic connection handle *)

END SupConnRW.
