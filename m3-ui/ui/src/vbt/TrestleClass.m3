(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Thu Mar 26 02:54:30 PST 1992 by msm      *)
(*      modified on Tue Mar 10 19:03:35 1992 by steveg   *)
(*      modified on Mon Feb 24 13:58:27 PST 1992 by muller   *)
(*      modified on Wed Aug 28 23:29:01 PDT 1991 by gnelson  *)

<*PRAGMA LL*>

MODULE TrestleClass;

IMPORT Trestle, TrestleComm;

TYPE CCList = REF RECORD cc: ConnectClosure; link: CCList END;

VAR cclist: CCList := NIL; mu := NEW(MUTEX);

PROCEDURE Connect(
    inst: TEXT := NIL;
    localOnly: BOOLEAN := FALSE)
    : Trestle.T
  RAISES {TrestleComm.Failure} =
    VAR ccl: CCList; res: Trestle.T;
  BEGIN
    LOCK mu DO ccl := cclist END;
    WHILE ccl # NIL DO
      IF ccl.cc.apply(inst, localOnly, res) THEN RETURN res END;
      ccl := ccl.link
    END;
    RAISE TrestleComm.Failure
  END Connect;

PROCEDURE RegisterConnectClosure(cc: ConnectClosure) =
  BEGIN
    LOCK mu DO 
      cclist := NEW(CCList, cc := cc, link := cclist)
    END
  END RegisterConnectClosure;

BEGIN
  connectMu := NEW(MUTEX);
  closeMu := NEW(MUTEX)
END TrestleClass.
