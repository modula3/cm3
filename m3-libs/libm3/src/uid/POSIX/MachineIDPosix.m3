(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Fri Feb 10 11:56:08 PST 1995 by kalsow     *)
(*      modified on Thu Jul 15 16:23:08 PDT 1993 by swart      *)

UNSAFE MODULE MachineIDPosix EXPORTS MachineID;
IMPORT MachineIDPosixC;

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
  BEGIN
    RETURN MachineIDPosixC.CanGet(ADR(id.r[0]));
  END CanGet;

BEGIN
END MachineIDPosix.
