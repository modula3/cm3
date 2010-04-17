(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Wed Sep 16 16:03:33 PDT 1992 by sclafani   *)

INTERFACE MidiLineServer;

IMPORT Midi;
FROM Ctypes IMPORT int;

TYPE
    T <: Midi.T;

<*EXTERNAL MidiLineServer__Poll*>
PROCEDURE Poll(socket: int): int;
(* Wrapper around Posix poll(). *)

END MidiLineServer.
