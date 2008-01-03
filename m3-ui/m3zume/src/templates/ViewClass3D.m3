(* Copyright (C) 1995, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Feb  9 08:57:32 PST 1995 by kalsow  *)
(*      modified on Sun Nov 28 23:08:58 PST 1993 by najork  *)

(*********************************************************************
|*  NOTE: This file is generated automatically from the event 
|*        definition file #(_ALGNAME_).evt.
|*********************************************************************)

<* PRAGMA LL *>

MODULE #(_ALGNAME_)3DViewClass;

$View3D
$ViewClass
$ZeusClass
$#(_ALGNAME_)AlgClass
#(_IMPORTS_)

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
       isCompat := IsCompat;
      <*LL = {} *>
#{_OUTPUT
        oe#(_EVENT_) := #(_EVENT_);
#}
      <*LL = VBT.mu *>
#{_UPDATE
        ue#(_EVENT_) := #(_EVENT_);
#}
      END;
      
PROCEDURE IsCompat (v: T; alg: ZeusClass.T): BOOLEAN =
  BEGIN
    RETURN ISTYPE(alg, #(_ALGNAME_)AlgClass.T) AND View3D.T.isCompat (v, alg);
  END IsCompat;

#{_OUTPUT
PROCEDURE #(_EVENT_) (view: T 
#{
    ; <*UNUSED*> #(_ARGMODE_)#(_ARGNAME_): #(_ARGTYPE_)
#}
) =
  <*LL = {} *>
  BEGIN 
    view.evtHandled := FALSE;
  END #(_EVENT_);

#}

#{_UPDATE
PROCEDURE #(_EVENT_) (view: T 
#{
    ; <*UNUSED*> #(_ARGMODE_)#(_ARGNAME_): #(_ARGTYPE_)
#}
) =
  <*LL = VBT.mu *>
  BEGIN 
    view.evtHandled := FALSE;
  END #(_EVENT_);

#}

BEGIN
END #(_ALGNAME_)3DViewClass.
