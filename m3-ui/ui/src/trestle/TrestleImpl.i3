(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Mar  6 17:44:16 PST 1995 by msm     *)
<*PRAGMA LL*>

INTERFACE TrestleImpl;

IMPORT Trestle, VBT, TrestleClass, TrestleComm, TrestleConf;

PROCEDURE SetDefault(t: Trestle.T);
(* Set the default trestle to t *)

PROCEDURE RootChild(v: VBT.T; VAR trsl: Trestle.T; VAR ch: VBT.T): BOOLEAN;
(* If v is installed in a Trestle, return that Trestle, and the
   VBT child of that Trestle corresponding to v, and return TRUE.
   Otherwise, return FALSE *)

(* In the following procedures, ch must be a root child. *)

PROCEDURE GetDecor(ch: VBT.T): TrestleClass.Decoration;

PROCEDURE InnerDecorate(trsl: Trestle.T; ch: VBT.T;
                        new: TrestleClass.Decoration) 
  RAISES {TrestleComm.Failure};

PROCEDURE UpdateChalk(ch: VBT.T);
(* For all the buddies of ch, call updateChalk on their Trestle *)

PROCEDURE UpdateBuddies(ch: VBT.T);
(* For all the buddies of ch, call updateBuddies on their Trestle *)

PROCEDURE SetConfCtl(t: Trestle.T; on: BOOLEAN := FALSE);
(* By default, a Trestle.T automatically installs its children on all
   of the workstations in the same conference as the user attached to
   this Trestle, in the sense of the TrestleConf interface.  By setting
   on to FALSE, new applications will connect only to the Trestle t *)

TYPE
  App <: AppPublic;
  AppPublic = TrestleConf.App OBJECT primary: User := NIL;  END;
  User <: TrestleConf.User;
  TPublic = TrestleClass.Public OBJECT
              conf       : User := NIL;
              confEnabled       := TRUE
            END;

REVEAL Trestle.T <: TPublic;

PROCEDURE ChildApp(ch: VBT.T): App;
(* Each root child, even if conferencing is disabled, has an associated
   App.  This returns it. *)

END TrestleImpl.



