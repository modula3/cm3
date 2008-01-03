(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Jul 29 10:20:50 PDT 1994 by najork                   *)
(*       Created on Fri Feb 18 10:53:01 PST 1994 by najork                   *)


GENERIC INTERFACE CBStack (AnyCB);

IMPORT CB, GO;

TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init () : T;
    invokeTop (rec : AnyCB.Rec) RAISES {CB.BadMethod};
    push (cb : AnyCB.T);
    pop () RAISES {GO.StackError};
    remove (cb : AnyCB.T) RAISES {GO.StackError};
  END;

(* Callback Stacks are monitored. "push", "pop", and "remove" happen inside 
   the monitor. "invokeTop" has two phases: obtaining the top element of the 
   stack (which happens inside the moditor), and invoking it (which is not 
   protected by the monitor). *)


END CBStack.
