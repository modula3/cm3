(* Copyright (C) 1992, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(* Last modified on Thu Jun 20 14:28:05 PDT 1996 by heydon   *)
(*      modified on Tue Jan 02 10:50:31 EST 1995 by mard     *)
(*      modified on Mon Apr 24 16:50:31 PDT 1995 by msm      *)
(*      modified on Tue Nov 23 14:21:24 PST 1993 by steveg   *)
(*      modified on Fri Oct 22 14:58:51 PDT 1993 by sfreeman *)

UNSAFE MODULE XNoSharedMem EXPORTS XSharedMem;

IMPORT Completion, Picture, TrestleComm, VBT, X, XClient, XClientExt,
       XClientF, XPicture, XScreenType;

(* New() exported by XSharedFree *)

(* -- XClient and XScreenType stuff -- *)

REVEAL
  XClient_T = XClientF.T_Rel BRANDED OBJECT
    shmEventBase := -1;
    (* GetEventBase returns -1 on error,
       so use it to signify no extension *)
  END;

PROCEDURE InitXClient (<*UNUSED*>v: XClient.T) RAISES {TrestleComm.Failure} =
  BEGIN
    (* SKIP *)
  END InitXClient;

PROCEDURE InitXScreenType (<* UNUSED *> st: XScreenType.T) =
  BEGIN
    (* SKIP *)
  END InitXScreenType;

PROCEDURE UsesExtension (<* UNUSED *> st: VBT.ScreenType): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END UsesExtension;

PROCEDURE EventBase (v: XClient.T): X.Int =
  BEGIN
    RETURN v.shmEventBase;
  END EventBase;

PROCEDURE PictureUsesExt (
  <* UNUSED *> st: VBT.ScreenType;
  <* UNUSED *> picture: Picture.T)
  : BOOLEAN =
  BEGIN
    RETURN FALSE;
  END PictureUsesExt;

PROCEDURE MakeCompletion (<*UNUSED*> im: T): Completion.T =
  BEGIN
    RETURN NIL;
  END MakeCompletion;

REVEAL
  T = XPicture.T BRANDED "XSharedMem.Picture" OBJECT
  END;

BEGIN
END XNoSharedMem.
