(* Copyright (C) 1989, 1990, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Thu Oct 21 14:29:00 PDT 1993 by sfreeman *)

(* this is the "nice" interface to Jva.i3 which provides connections
   to the local audio server. These Ts are shared if possible and the
   connection only closed if the T has no more clients. *)

INTERFACE JVAudio;

IMPORT Jva, OSError, Thread;

TYPE T <: Jva.T;

PROCEDURE New(hostname: TEXT): T RAISES {OSError.E, Thread.Alerted};
(* return a T which is connected to "hostname," create a new
   one if necessary. This T will be disconnected if all its clients
   close it, or while it is being garbage collected *)

END JVAudio.
