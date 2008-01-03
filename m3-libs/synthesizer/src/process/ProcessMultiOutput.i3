INTERFACE ProcessMultiOutput;
(* author: thielema *)

(* Superclass of all processes which have multiple outputs.  This class
   provides buffering of the streams.  One output channel can be requested
   multiple times.  This is quite similar to Split. *)

IMPORT Signal;
IMPORT Thread;

TYPE
  T <: Public;
  Public =
    OBJECT
    METHODS
      (*----- abstract methods -----*)
      get (): REF ARRAY OF LONGREAL
           RAISES {Signal.End, Signal.Error, Thread.Alerted};
      (* Get the next sample or raises End if no further sample can be
         fetched. *)
      exit ();
      (* Finish reading of samples: Free resources. *)

      (*----- real methods -----*)
      channel (num: CARDINAL; ): Signal.T;
      (* get the channel (according to the list passed to createChannel) *)

      createChannels (         numOutputs: CARDINAL;
                      READONLY channels  : ARRAY OF CARDINAL; );
      (* given a list of channel numbers, setup buffers for the respecting
         channels *)
    END;


END ProcessMultiOutput.
