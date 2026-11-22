(* $Id: StackMachine.i3,v 1.1 2009/06/28 11:38:26 mika Exp $ *)

INTERFACE StackMachine;

(* stack frame layout

   <args in reverse order> <# args> <pointer to parent frame> <f> <locals>
                                                               ^
                                                               |
                                                               |
                                                             frame
                                                            pointer
*)

TYPE T <: REFANY;

PROCEDURE New() : T;

PROCEDURE Push(stack : T; item : Object);
  
PROCEDURE Pop(stack : T) : Object;

PROCEDURE Top(stack : T) : Object;

PROCEDURE ReplaceFrame(stack : T) : Object;
  (* shift top frame to be in place of top frame less one. 
     this routine needs to know frame layout *)

PROCEDURE FrameP(stack : T) : CARDINAL; (* maybe not *)
  
END StackMachine.
