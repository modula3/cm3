(* Copyright (C) 1995, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Feb  9 08:57:10 PST 1995 by kalsow  *)
(*      modified on Sun Nov 28 23:09:13 PST 1993 by najork  *)

(*********************************************************************
|*  NOTE: This file is generated automatically from the event 
|*        definition file #(_ALGNAME_).evt.
|*********************************************************************)

<* PRAGMA LL *>

INTERFACE #(_ALGNAME_)3DViewClass;

$Thread
$View3D
#(_IMPORTS_)

TYPE
  T <: Public;
  Public = View3D.T OBJECT
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
      
END #(_ALGNAME_)3DViewClass.
