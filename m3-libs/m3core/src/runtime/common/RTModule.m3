(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Aug  6 08:19:50 PDT 1993 by kalsow     *)
(*      modified on Thu Mar  7 03:13:05 1991 by muller         *)

UNSAFE MODULE RTModule;

IMPORT RT0, RT0u, RTMisc;

PROCEDURE Count (): CARDINAL =
  BEGIN
    RETURN RT0u.nModules;
  END Count;

PROCEDURE Get (m: CARDINAL): RT0.ModulePtr =
  VAR p := RT0u.modules;
  BEGIN
    IF (m >= RT0u.nModules) THEN
      RTMisc.FatalErrorI ("improper module index: ", m);
    END;
    p := p + m * ADRSIZE (RT0.ModulePtr);
    RETURN p^;
  END Get;

BEGIN
END RTModule.

