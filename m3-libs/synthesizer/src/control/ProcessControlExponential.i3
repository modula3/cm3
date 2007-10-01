INTERFACE ProcessControlExponential;
(* author: thielema *)

IMPORT Signal, SignalControl;

(* Convert a signal to a control signal by an exponential function. *)
PROCEDURE Do
  (READONLY x: Signal.Array;     (* The control curve *)
   center: LONGREAL;             (* A signal value of zero is mapped to
                                    'center'. *)
   depth: LONGREAL;  (* A signal value of one is mapped to 'center*depth',
                        minus one is mapped to 'center/depth'. *)):
  SignalControl.RefArray;

(* for speed comparison *)
PROCEDURE DoPow (READONLY x: Signal.Array; center, depth: LONGREAL; ):
  SignalControl.RefArray;


TYPE
  T <: Public;
  Public = SignalControl.T OBJECT
           METHODS
             init (x: Signal.T; center, depth: LONGREAL; ): T;
           END;

END ProcessControlExponential.
