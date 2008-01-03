(* Copyright (C) 1995, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Feb  9 08:51:16 PST 1995 by kalsow  *)
(* Last modified on Wed Feb 17 16:46:35 PST 1993 by johnh   *)

(*********************************************************************
|*  NOTE: This file is generated automatically from the event 
|*        definition file #(_ALGNAME_).evt.
|*********************************************************************)

<* PRAGMA LL *>

INTERFACE #(_ALGNAME_)IE;

$Algorithm
$Thread
$View
#(_IMPORTS_)

(* call the following to generate an event: *)

#{_OUTPUT
PROCEDURE #(_EVENT_) (
      initiator: Algorithm.T; 
      #(_ARGSTR_) 
    ) RAISES {Thread.Alerted};
    <* LL = {} *>

#}
#{_FEEDBACK
PROCEDURE #(_EVENT_) (
      initiator: View.T; 
      #(_ARGSTR_) 
    ) RAISES {Thread.Alerted};
    <* LL = VBT.mu *>

#}
#{_UPDATE
PROCEDURE #(_EVENT_) (
      initiator: Algorithm.T; 
      #(_ARGSTR_) 
    ) RAISES {Thread.Alerted};
    <* LL = VBT.mu *>

#}

END #(_ALGNAME_)IE.
