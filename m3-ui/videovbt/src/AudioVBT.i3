(* Copyright (C) 1989, 1990, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Sun Oct 24 16:46:50 PDT 1993 by sfreeman *)

INTERFACE AudioVBT;

IMPORT Filter, Jva, OSError, Thread, VBT;
<* PRAGMA LL *>

(* An "AudioVBT.T" is a filter which is associated with the audio stream
   from a JVideo server.

   All audio streams for a given host in an application share the same
   connection so the most recent setting is used if there are several
   AudioVBT.Ts operating on the same source.  The meaning of the "source"
   string is determined by the underlying audio software.

   If "mute" is true then the audio connection is silent but remains
   connected.

   Unless "ignoreMapping" is true, the audio connection is muted whenever
   the "T" is unmapped, and unmuted when the "T" is mapped again.

   At present, "volume" is a value in the range [-30..30].  Its default
   value is 0.

   A mute push button can be implemented by wrapping a AudioVBT.T around a
   child of a TSplit with 2 children.  By switching between the children
   the TSplit will map and unmap the AudioVBT.T, so turning muting on and
   off.

   The T releases its connection to the source when it is deleted.  It does
   not reset the state of the connection before releasing. *)

TYPE
  T <: Public;
  Public = Filter.T OBJECT
           METHODS
             <* LL < self *>
             init (ch           : VBT.T;
                   source       : TEXT;
                   mute                      := FALSE;
                   ignoreMapping             := FALSE;
                   volume       : Jva.Volume := 0      ): T
                   RAISES {OSError.E, Thread.Alerted};

           END;

<* LL < self *>
PROCEDURE SetMute (t: T; mute: BOOLEAN) RAISES {Thread.Alerted};
PROCEDURE SetIgnoreMapping (t: T; ignore: BOOLEAN) RAISES {Thread.Alerted};
PROCEDURE SetVolume (t: T; volume: Jva.Volume) RAISES {Thread.Alerted};

END AudioVBT.
