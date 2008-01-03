(* Copyright (C) 1989, 1990, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Fri Jan  7 14:56:08 PST 1994 by msm      *)
(*      modified on Thu Oct 21 17:06:36 PDT 1993 by sfreeman *)

(* audio client interface to the local J-Video server.  All the methods
   lock the object and block the thread until the server replies *)

INTERFACE Jva;

IMPORT Atom, Jv, OSError, Thread;
FROM Ctypes IMPORT int;

TYPE
  Volume = [-30 .. 30];
  Statistics = RECORD
                 reads  : int;   (* # of calls to read() from input() *)
                 bytes  : int;   (* total bytes read from source *)
                 aFrames: int;
                 aBytes : int;
                 aFramesWithDrops: int;
                 aLatencyTotal   : int;
                 aLatePlays      : int;
                 aReorders       : int;  (* Out of order packets *)
                 aLateBytes: int;  (* How many bytes have been missed *)
               END;

TYPE
  T <: Public;
  Public =
    Jv.T OBJECT
    METHODS
      (* all methods LL < self *)
      init (): T RAISES {OSError.E, Thread.Alerted};
      (* initialise local state and establish a connection with the local
         server.  May raise OSError.E(invalidHostname) *)

      connect (hostname: TEXT) RAISES {OSError.E, Thread.Alerted};
      (* establish an audio connection with the source on "hostname" *)

      setMute (on: BOOLEAN) RAISES {OSError.E, Thread.Alerted};
      (* turn muting on or off *)

      setVolume (volume: Volume) RAISES {OSError.E, Thread.Alerted};
      (* set the volume level for the connection *)

      getStatistics (): Statistics RAISES {OSError.E, Thread.Alerted};
      (* get statistics about the performance of the audio stream *)

      (* "close()" is now LL < self *)
    END;

VAR invalidHostname: Atom.T;     (* may be raised in OSError.E *)

END Jva.
