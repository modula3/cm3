(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Tue Nov 23 14:20:07 PST 1993 by steveg   *)
(*      modified on Wed Oct  6 09:25:04 PDT 1993 by sfreeman *)

UNSAFE INTERFACE XSharedMem;

IMPORT Completion, Picture, TrestleComm, VBT, X, XClient, XClientF, XPicture,
       XScreenType;

<* PRAGMA LL *>

TYPE XClient_T <: XClientF.T_Rel;

PROCEDURE InitXClient (v: XClient.T) RAISES {TrestleComm.Failure};
<* LL = trsl *>
(* initialise /v/ for using the shared memory eesExtension (st: to the ns
   -1 if none available *)

PROCEDURE InitXScreenType (st: XScreenType.T);
(* initialise /st/ for use with the extension *)

PROCEDURE UsesExtension (st: VBT.ScreenType): BOOLEAN;
(* returns true if the /st/ uses the shared memory extension *)

PROCEDURE EventBase (v: XClient.T): X.Int;
(* returns the offset for shared memory event types *)

PROCEDURE PictureUsesExt (st: VBT.ScreenType; picture: Picture.T): BOOLEAN;
(* return TRUE if /picture/ on /st/ will use the extension *)


TYPE T <: XPicture.T;

PROCEDURE New (): T;
(* Pictures acquired from this procedure will be returned to the free list
   when done with *)

PROCEDURE MakeCompletion (im: T): Completion.T;
(* used for PictureRep.MakeCompletion *)

END XSharedMem.
