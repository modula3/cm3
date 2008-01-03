(* Copyright (C) 1995, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Feb  9 08:56:54 PST 1995 by kalsow  *)
(*      modified on Sat Jun  4 16:23:08 1994 by heydon      *)
(*      modified on Wed Feb 17 17:04:32 PST 1993 by johnh   *)
(*      modified on Tue Jun  9 00:35:35 1992 by mhb         *)

(*********************************************************************
|*  NOTE: This file is generated automatically from the event 
|*        definition file #(_ALGNAME_).evt.
|*********************************************************************)

<* PRAGMA LL *>

MODULE #(_ALGNAME_)ViewClass;

$ViewClass
$ZeusClass
$#(_ALGNAME_)AlgClass
#(_IMPORTS_)

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        <* LL.sup = VBT.mu *>
        isCompat := IsCompat;
        <* LL.sup < VBT.mu *>
#{_OUTPUT
        oe#(_EVENT_) := #(_EVENT_);
#}
        <* LL.sup = VBT.mu *>
#{_UPDATE
        ue#(_EVENT_) := #(_EVENT_);
#}
      END;
      
PROCEDURE IsCompat(<*UNUSED*> v: T; alg: ZeusClass.T): BOOLEAN =
  <* LL.sup = VBT.mu *>
  BEGIN
    RETURN ISTYPE(alg, #(_ALGNAME_)AlgClass.T)
  END IsCompat;

#{_OUTPUT
PROCEDURE #(_EVENT_) (view: T 
#{
    ; <*UNUSED*> #(_ARGMODE_)#(_ARGNAME_): #(_ARGTYPE_)
#}
) =
  <* LL.sup < VBT.mu *>
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
  <* LL.sup = VBT.mu *>
  BEGIN 
    view.evtHandled := FALSE;
  END #(_EVENT_);
#}

BEGIN
END #(_ALGNAME_)ViewClass.
