(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue Jan 31 09:05:15 PST 1995 by kalsow   *)
(*      modified on Tue Nov 23 14:59:28 PST 1993 by steveg   *)

UNSAFE MODULE XExtensions;

IMPORT TrestleComm, XClient, XScreenType, XSharedMem;

PROCEDURE InitXClient (xclient: XClient.T) RAISES {TrestleComm.Failure} =
  BEGIN
    XSharedMem.InitXClient(xclient);
  END InitXClient;


PROCEDURE InitXScreenType (st: XScreenType.T) =
  BEGIN
    IF XSharedMem.UsesExtension(st) THEN
      XSharedMem.InitXScreenType(st);
    END;
  END InitXScreenType;

BEGIN
END XExtensions.
