(* Copyright 1990 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Site.m3 *)
(* Last modified on Wed Feb  1 09:33:46 PST 1995 by kalsow *)
(*      modified on Fri Jun  4 13:36:45 PDT 1993 by wobber *)
(*      modified on Wed Nov 27 17:58:48 GMT+1:00 1991 by prusker *)

MODULE Site EXPORTS Site, SiteObj;

IMPORT Env, Convert, IP, Text, Thread, Time;
IMPORT NetObj, TCPNetObj;

CONST
  CacheTimeout = 9.0D2; (* 15 minutes *)

VAR
  cacheMu: MUTEX := NEW(MUTEX);
  cachedT: T := NIL;
  cacheTimeout: Time.T;
  localST: ST := NIL;

PROCEDURE Init() : T RAISES {Error, Thread.Alerted} =
  BEGIN
    RETURN GetInner(FALSE);
  END Init;

PROCEDURE ErrMsg(ec: EC): TEXT =
  BEGIN
    CASE ec OF
    | EC.MissingEnvVariable =>
       RETURN "Site error: missing SIPHON_SITE environment variable";
    | EC.InfoServerUnavailable =>
       RETURN "Site error: info unavailable (packageserver down?)";
    | EC.BadInfoServerSpec, EC.BadIPPortSpec =>
       RETURN "Site error: bad address (in SIPHON_SITE environment variable?)";
    END;
  END ErrMsg;

PROCEDURE Get(cacheOK: BOOLEAN := TRUE): T =
    <* FATAL Error, Thread.Alerted *>
  BEGIN
    RETURN GetInner(cacheOK);
  END Get;

PROCEDURE GetInner(cacheOK: BOOLEAN := TRUE): T
   RAISES {Error, Thread.Alerted} =
  VAR loc: TEXT;
      st: ST;
      t: T;
  BEGIN
    LOCK cacheMu DO
      IF cachedT # NIL AND cacheOK AND Time.Now() < cacheTimeout THEN
        RETURN cachedT;
      ELSE
        t := cachedT;
      END;
    END;
    IF localST # NIL THEN
      st := localST;
    ELSE
      loc := Env.Get(EnvVarName);
      IF loc = NIL THEN
        RAISE Error(EC.MissingEnvVariable);
      END;
      st := Import(loc);
    END;
    TRY
      t := st.get();
      LOCK cacheMu DO
        cachedT := t;
        cacheTimeout := Time.Now() + CacheTimeout;
      END;
    EXCEPT
    | Thread.Alerted, NetObj.Error =>
        IF t # NIL THEN RETURN t; END;
        RAISE Error(EC.InfoServerUnavailable);
    END;
    RETURN t;
  END GetInner;

PROCEDURE FindRemote(
      site: TEXT;
      VAR (*OUT*) remote: Remote;
      cacheOK: BOOLEAN := FALSE) : BOOLEAN =
  VAR t := Get(cacheOK);
  BEGIN
    IF t.foreignSites # NIL THEN
      FOR i := 0 TO LAST(t.foreignSites^) DO
        IF Text.Equal(site, t.foreignSites[i].name) THEN
          remote := t.foreignSites[i];
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END FindRemote;

PROCEDURE SetServerST(st: ST) =
  BEGIN
    localST := st;
  END SetServerST;

PROCEDURE Import(loc: TEXT) : ST RAISES {Error, Thread.Alerted} =
  VAR o: NetObj.T;
      a: NetObj.Address;
      ipEP: IP.Endpoint;
  BEGIN
    IF loc = NIL THEN loc := ""; END;
    TRY
      IF ParseIPHostname(loc, ipEP) THEN
        a := TCPNetObj.Locate(ipEP);
      ELSE
        a := NetObj.Locate(loc);
      END;
      o := NetObj.Import(SiteObjName, a);
      IF o # NIL AND NOT ISTYPE(o, ST) THEN
        RAISE Error(EC.InfoServerUnavailable);
      END;
    EXCEPT
    | NetObj.Error =>
       RAISE Error(EC.InfoServerUnavailable);
    | NetObj.Invalid =>
       RAISE Error(EC.BadInfoServerSpec);
    END;
    RETURN o;
  END Import;

PROCEDURE ParseIPHostname(loc: TEXT; VAR ipEP: IP.Endpoint) : BOOLEAN
    RAISES {Error} =
  VAR i, j, n, res: INTEGER;
      buffer: ARRAY [0..9] OF CHAR;
  BEGIN
    i := Text.FindChar(loc, ':', 0);
    IF i >= 0 THEN
      TRY
        n := Text.Length(loc) - (i+1);
        Text.SetChars(buffer, Text.Sub(loc, i+1, n));
        res := Convert.ToInt(SUBARRAY(buffer, 0, MIN(NUMBER(buffer), n)), j);
        IF res >= 0 AND res <= LAST(IP.Port) THEN
          ipEP.port := res;
          IF IP.GetHostByName(Text.Sub(loc, 0, i), ipEP.addr) THEN
            RETURN TRUE;
          END;
        END;
      EXCEPT
      | IP.Error => RAISE Error(EC.BadInfoServerSpec);
      END;
      RAISE Error(EC.BadIPPortSpec);
    END;
    RETURN FALSE;
  END ParseIPHostname;

BEGIN
END Site.




