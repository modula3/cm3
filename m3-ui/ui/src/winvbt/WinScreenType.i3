(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Jan 20 21:50:03 PST 1995 by najork                   *)
(*       Created on Tue Jan 17 11:20:23 PST 1995 by najork                   *)


INTERFACE WinScreenType;

IMPORT Rect, VBT, WinTrestle;

TYPE 
  T <: Public;
  Public = VBT.ScreenType OBJECT
    trsl: WinTrestle.T;
    (* Remaining fields protected by the .trsl field. *)
    rootDom: Rect.T;
  END;

PROCEDURE New(trsl: WinTrestle.T): T;
(* Create a screentype for trsl.  LL <= VBT.mu. *)

END WinScreenType.
