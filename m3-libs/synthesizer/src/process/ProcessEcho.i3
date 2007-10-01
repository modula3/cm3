INTERFACE ProcessEcho;
(* author: thielema *)

IMPORT Signal;

TYPE
  T <: Public;
  Public =
    Signal.T OBJECT
    METHODS
      init (x: Signal.T; delay: [1 .. LAST(CARDINAL)]; gain: LONGREAL; ):
            T;
    END;

END ProcessEcho.
