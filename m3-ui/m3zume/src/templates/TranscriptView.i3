(* Copyright (C) 1995, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Wed Feb  8 15:42:11 PST 1995 by kalsow  *)
(*      modified on Wed Feb 17 16:47:18 PST 1993 by johnh   *)
(*      modified on Tue Jun  9 00:43:17 1992 by mhb         *)

(*********************************************************************
|*  NOTE: This file is generated automatically from the event 
|*        definition file #(_ALGNAME_).evt.
|*********************************************************************)

<* PRAGMA LL *>

INTERFACE #(_ALGNAME_)TranscriptView;

IMPORT #(_ALGNAME_)ViewClass;

TYPE
  T <: Public;
  Public = #(_ALGNAME_)ViewClass.T OBJECT
      METHODS
        <* LL = VBT.mu *>
        init(): T;
      END;

END #(_ALGNAME_)TranscriptView.
