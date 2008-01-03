(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Dec  2 16:40:57 PST 1992 by wobber *)

INTERFACE ConnMsgRW;

IMPORT ConnFD, MsgRd, MsgWr;

PROCEDURE NewRd(fd: ConnFD.T) : MsgRd.T;
   (* produces a message reader from a generic connection handle *)

PROCEDURE NewWr(fd: ConnFD.T) : MsgWr.T;
   (* produces a message writer from a generic connection handle *)

END ConnMsgRW.

