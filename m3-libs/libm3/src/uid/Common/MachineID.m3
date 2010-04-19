(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Fri Feb 10 11:43:49 PST 1995 by kalsow     *)
(*      modified on Thu Jul 15 16:41:23 PDT 1993 by swart      *)
(*      modified on Thu May  6 13:27:58 PDT 1993 by mjordan    *)

UNSAFE MODULE MachineID;
IMPORT MachineIDC, Scheduler;

EXCEPTION Failure;

PROCEDURE Get (): T =
  <*FATAL Failure*>
  VAR id: T;
  BEGIN
    IF CanGet (id)
      THEN RETURN id;
      ELSE RAISE Failure;
    END;
  END Get;

PROCEDURE CanGet (VAR(*OUT*) id: T): BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    Scheduler.DisableSwitching();
    result := (MachineIDC.CanGet(ADR(id.r[0])) # 0);
    Scheduler.EnableSwitching();
    RETURN result;
  END CanGet;

BEGIN
END MachineID.
