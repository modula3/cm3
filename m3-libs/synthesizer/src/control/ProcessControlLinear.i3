INTERFACE ProcessControlLinear;
(* author: thielema *)

IMPORT Signal, SignalControl;

(* Convert a signal to a control signal by a linear function. *)
PROCEDURE Do (READONLY x: Signal.Array;  (* The control curve *)
              center: LONGREAL;  (* A signal value of zero is mapped to
                                    'center'. *)
              depth: LONGREAL;  (* A signal value of one is mapped to
                                   'center+depth'. *)):
  SignalControl.RefArray;


TYPE
  T <: Public;
  Public = SignalControl.T OBJECT
           METHODS
             init (x: Signal.T; center, depth: LONGREAL; ): T;
           END;

END ProcessControlLinear.
