(* Copyright (C) 1995, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Feb  9 08:56:28 PST 1995 by kalsow  *)
(*      modified on Wed Feb 17 17:04:21 PST 1993 by johnh   *)
(*      modified on Tue Jun  9 00:35:21 1992 by mhb         *)

(*********************************************************************
|*  NOTE: This file is generated automatically from the event 
|*        definition file #(_ALGNAME_).evt.
|*********************************************************************)

<* PRAGMA LL *>

INTERFACE #(_ALGNAME_)ViewClass;

$Thread
$View
#(_IMPORTS_)

TYPE
  T <: Public;
  Public = View.T OBJECT
      METHODS
      <*LL = {} *>
#{_OUTPUT
        oe#(_EVENT_) (#(_ARGSTR_)) RAISES {Thread.Alerted};
#}
      <*LL = VBT.mu *>
#{_UPDATE
        ue#(_EVENT_) (#(_ARGSTR_)) RAISES {Thread.Alerted};
#}
      END;
      
END #(_ALGNAME_)ViewClass.
