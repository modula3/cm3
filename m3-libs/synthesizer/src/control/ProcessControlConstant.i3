INTERFACE ProcessControlConstant;
(* author: thielema *)

IMPORT SignalControl;

PROCEDURE Do (length: CARDINAL;  (* number of samples of the generated
                                    sound *)
              value: LONGREAL;  (* the value the signal is filled with *)):
  SignalControl.RefArray;


TYPE
  T <: Public;
  Public = SignalControl.T OBJECT METHODS init (value: LONGREAL; ): T; END;

END ProcessControlConstant.
