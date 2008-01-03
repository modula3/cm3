INTERFACE ProcessClip;
(* author: thielema *)

(* This process keep 'duration' samples of the input and throws Signal.End
   then. *)

IMPORT Signal;

TYPE
  T <: Public;
  Public = Signal.T OBJECT
           METHODS
             init (x: Signal.T; duration: INTEGER; ): T;
           END;

END ProcessClip.
