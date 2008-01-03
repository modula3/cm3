(* Copyright (C) 1995, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Feb  9 08:47:37 PST 1995 by kalsow  *)
(* Last modified on Wed Feb 17 16:44:58 PST 1993 by johnh   *)
(*      modified on Tue Jun  9 00:35:07 1992 by mhb         *)

(*********************************************************************
|*  NOTE: This file is generated automatically from the event 
|*        definition file #(_ALGNAME_).evt.
|*********************************************************************)

INTERFACE #(_ALGNAME_)AlgClass;

$Algorithm
$AlgorithmClass
#(_IMPORTS_)

<* PRAGMA LL *>

TYPE
  EventDataRec = RECORD
#{_OUTPUT
    stopAt#(_EVENT_): BOOLEAN := TRUE;
    waitAt#(_EVENT_): CARDINAL := 1;
    ctOf#(_EVENT_): CARDINAL := 0;
#}
  END;

  T <: Public;
  Public = Algorithm.T OBJECT
      eventDataRec: EventDataRec;
#(_ALGDATA_)
      METHODS
#{_FEEDBACK
        fe#(_EVENT_) (#(_ARGSTR_));    <* LL = VBT.mu *>
#}
      END;
      
REVEAL
  Algorithm.T <: AlgorithmClass.T;
  
END #(_ALGNAME_)AlgClass.
