(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PackageObj.m3 *)
(* Last modified on Tue Jun  8 12:33:44 PDT 1993 by wobber *)
(*      modified on Fri Dec  7 11:28:16 GMT+1:00 1990 by prusker *)

MODULE PackageObj;

IMPORT Site, Text, Thread;
IMPORT IP, NetObj, TCPNetObj, PkgProt, PkgErr;
FROM PkgErr IMPORT ImportError;

VAR
  default: T := NIL;
  defaultReplica: TEXT := NIL;
  
PROCEDURE New(replica: TEXT): T
    RAISES {PkgErr.E, NetObj.Error, Thread.Alerted} =
  VAR ipPort: IP.Port;
  VAR obj: NetObj.T;
      addr: IP.Address;
  BEGIN
    IF (replica = NIL) OR Text.Empty(replica) THEN
      IF default # NIL THEN RETURN default; END;
      PkgErr.Raise(ImportError);
    ELSE
      IF (default # NIL) AND Text.Equal(replica, defaultReplica) THEN
        RETURN default;
      END;
    END;
    TRY
      <* ASSERT replica # NIL *>
      ipPort := Site.Get().ipPort;
      IF ipPort = IP.NullPort THEN
        obj := NetObj.Import(
          PkgProt.PkgExportName, NetObj.Locate(replica));
      ELSE
        IF NOT IP.GetHostByName(replica, addr) THEN
          PkgErr.Raise(ImportError);
        END;
        obj := NetObj.Import(
          PkgProt.PkgExportName,
          TCPNetObj.Locate(IP.Endpoint{addr, ipPort}));
      END;
    EXCEPT
    | IP.Error(ipErr) => PkgErr.Raise(ImportError, ipErr);
    | NetObj.Invalid => PkgErr.Raise(ImportError, NIL);
    END;
    IF (obj = NIL) OR NOT ISTYPE(obj, T) THEN
      PkgErr.Raise(ImportError, NIL);
    END;
    RETURN NARROW(obj, T);
  END New;

PROCEDURE SetServerT(t: T; replica: TEXT) =
  BEGIN
    default := t;
    defaultReplica := replica;
  END SetServerT;
  
BEGIN
END PackageObj.
