INTERFACE ProcessLoad;
(* author: thielema *)

FROM ProcessInOut IMPORT WordSize;
IMPORT Signal, ProcessPipeIn;
IMPORT Rd, Thread;

PROCEDURE Do
  (name: TEXT; format: TEXT := NIL; wordSize := WordSize.Bits16; ):
  Signal.RefArray RAISES {Rd.Failure, Thread.Alerted};

TYPE
  T <: Public;
  Public = ProcessPipeIn.T OBJECT
           METHODS
             init (name    : TEXT;
                   format  : TEXT   := NIL;
                   wordSize         := WordSize.Bits16; ): T;
           END;

(* Load a sound with multiple channels, such as a stereo sound.  This
   routine invokes 'sox' and I don't know how to find out the number of
   channels of a sound file using this program.  Thus you must tell the
   Load object the number of channels which must match the number of
   channels stored in the file.  By the 'channels' array you can select the
   channels that shall be used.  Cf.  ProcessMultiOutput. *)
TYPE
  Multi <: MultiPublic;
  MultiPublic = ProcessPipeIn.Multi OBJECT
                METHODS
                  init (         numChannels: CARDINAL;
                        READONLY channels   : ARRAY OF CARDINAL;
                                 name       : TEXT;
                                 format     : TEXT                := NIL;
                        wordSize := WordSize.Bits16; ): Multi;
                END;

END ProcessLoad.
