(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Siphon.m3 *)
(* Last modified on Fri Jun  4 13:32:35 PDT 1993 by wobber  *)
(*      modified on Fri Jul  3  9:12:58 GMT+2:00 1992 by prusker *)

MODULE Siphon;

IMPORT Site, PkgErr, Text, Thread;
IMPORT IP, NetObj, TCPNetObj;

FROM LockOps IMPORT SiteName;
FROM PkgErr IMPORT ImportError;

VAR
  default: T := NIL;
  defaultSite: SiteName := NIL;

PROCEDURE New (site: SiteName): T
    RAISES {PkgErr.E, NetObj.Error, Thread.Alerted} =
  VAR rem: Site.Remote;
  VAR obj: NetObj.T;
      siteT: Site.T;
      loc: NetObj.Address;
      addr: IP.Address;
  BEGIN
    IF (site = NIL) OR Text.Empty(site) THEN
      IF default # NIL THEN RETURN default; END;
    ELSE
      IF (default # NIL) AND Text.Equal(site, defaultSite) THEN
        RETURN default;
      END;
    END;
    TRY
      siteT := Site.Get();
      IF (site = NIL) OR Text.Empty(site) OR Text.Equal(site, siteT.name) THEN
        IF siteT.ipPort = IP.NullPort THEN
          loc := NetObj.Locate(siteT.siphonserver);
        ELSE
          IF NOT IP.GetHostByName(siteT.siphonserver, addr) THEN
            PkgErr.Raise(ImportError);
          END;
          loc := TCPNetObj.Locate(IP.Endpoint{addr, siteT.ipPort});
        END;
      ELSE
        IF NOT Site.FindRemote(site, rem) THEN
          PkgErr.Raise(PkgErr.NoSuchSite);
        END;
        IF rem.ipPort = IP.NullPort THEN
          loc := NetObj.Locate(rem.siphonserver);
        ELSE
          IF NOT IP.GetHostByName(rem.siphonserver, addr) THEN
            PkgErr.Raise(ImportError);
          END;
          loc := TCPNetObj.Locate(IP.Endpoint{addr, rem.ipPort});
        END;
      END;
      obj := NetObj.Import(SiphonExportName, loc);
    EXCEPT
    | IP.Error(ipErr) => PkgErr.Raise(ImportError, ipErr);
    | NetObj.Invalid => PkgErr.Raise(ImportError);
    END;
    IF (obj = NIL) OR NOT ISTYPE(obj, T) THEN
      PkgErr.Raise(ImportError);
    END;
    RETURN NARROW(obj, T);
  END New;

PROCEDURE SetServerT(t: T; site: SiteName) =
  BEGIN
    default := t;
    defaultSite := site;
  END SetServerT;
  
BEGIN
END Siphon.


