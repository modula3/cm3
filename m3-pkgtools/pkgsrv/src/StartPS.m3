(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* File: StartPS.mod *)
(* Last modified on Thu Feb  2 09:09:50 PST 1995 by kalsow *)
(*      modified on Fri Apr 29 12:59:34 PDT 1994 by wobber *)

MODULE StartPS EXPORTS Main;
(* starts package server process *)
(* unsafe because of RTHeapRep *)

IMPORT OpSys, Site, SiteObj, SiteServer, PackageDB, PackageServer,
       PackageObj, PathMap, PkgProt, PSLib, PSRestart, Params,
       LockOps, LockMethods, LockServer, LockRestart, ServerLog,
       Siphon, SiphonServer, Fmt, Text, TextList, Thread, NetObj,
       IP, TCPNetObj, NetPath;

IMPORT RTCollectorSRC;

FROM Stdio IMPORT stderr, stdout;
FROM PSLib IMPORT log, localSite;

CONST
  StatInterval = 1.8D2; (* three minutes *)

EXCEPTION InitFailed;

VAR thisIsSiphon: BOOLEAN := FALSE;

PROCEDURE RunForever () =
  BEGIN
    RTCollectorSRC.StartBackgroundCollection();
    LOOP (* forever *)
      FOR i := 1 TO 20 DO
        RTCollectorSRC.StartCollection();
        Thread.Pause (StatInterval);
        RTCollectorSRC.FinishCollection();
      END;
      ServerLog.WriteText (log, "Periodic statistics:\n" &
                                 ServerLog.DumpStats (log));
    END;
  END RunForever;

PROCEDURE Init () =
  VAR
    name, arg: Text.T;
    argNum: CARDINAL;
    configFile: Text.T := NIL;
    initialize: BOOLEAN;
  BEGIN
    stderr := stdout;
    name := NIL;
    argNum := 1;
    initialize := FALSE;
    WHILE argNum < Params.Count DO
      arg := Params.Get (argNum);
      INC (argNum);
      IF (arg = NIL) OR Text.Empty (arg) THEN EXIT;  END;
      IF Text.Equal (arg, "-c") THEN
        configFile := Params.Get(argNum);
        INC (argNum);
      ELSIF Text.Equal (arg, "-name") THEN
        name := Params.Get(argNum);
        INC (argNum);
      ELSIF Text.Equal (arg, "-init") THEN
        initialize := TRUE;
      END;
    END;
    log := ServerLog.Init (stdout);
    ServerLog.WriteText(log, "Initializing packageserver\n");
    TRY
      Start (configFile, name, initialize);
      RunForever ();
    EXCEPT
      | InitFailed =>
                  (* be sure that all messages to serverlog will be printed *)
          ServerLog.WriteText (log, "Initialization failed\n");
          (* Time.LongPause (30); *)
    END;
  END Init;

PROCEDURE Start (configFile: TEXT; nameArg: TEXT; initialize: BOOLEAN)
   RAISES {InitFailed} =
  VAR
    initSite: Text.T;
    lockServerIsMe: BOOLEAN;
    thisReplName: TEXT := NIL;
    packageT: PackageObj.T := NIL;
    ipPort: IP.Port;
    lockT: LockOps.T;
    agentLoc: NetObj.Address;
    st: SiteServer.T;
    siphonT: Siphon.T;
    site: Site.T;
    owner: Text.T;
    <* FATAL OpSys.Error *>
    <* FATAL NetObj.Error *>
    <* FATAL TCPNetObj.Failed *>
    <* FATAL Site.Error *>
    <* FATAL Thread.Alerted *>
  PROCEDURE Basename(t: TEXT) : TEXT =
    VAR pos := Text.FindChar(t, '.');
    BEGIN
      IF pos < 0 THEN RETURN t; END;
      RETURN Text.Sub(t, 0, pos);
    END Basename;
  PROCEDURE CompareToMe(addr: TEXT) : BOOLEAN =
    VAR bn1, bn2: TEXT;
    BEGIN
      IF Text.Equal(addr, nameArg) THEN RETURN TRUE; END;
      bn1 := Basename(addr);
      bn2 := Basename(nameArg);
      IF Text.Equal(bn1, bn2) AND ((bn1 = addr) OR (bn2 = nameArg)) THEN
        RETURN TRUE;
      END;
      RETURN FALSE;
    END CompareToMe;
  BEGIN
    OpSys.Init ();

    TRY
      st := SiteServer.Init(configFile);
    EXCEPT
    | SiteServer.BadDB(e) =>
        PSLib.LogIt("Site.Error: " & e);
        RAISE InitFailed;
    END;

    SiteObj.SetServerST(st);
    site := Site.Init();
    PathMap.Init(st);

    owner := site.owner;
    localSite := site.name;
    ipPort := site.ipPort;
    IF (site.replicas = NIL) OR (NUMBER (site.replicas^) = 0) THEN
      PSLib.LogIt("No replicas for site");
      RAISE InitFailed;
    END;
    IF nameArg = NIL THEN
      nameArg := OpSys.GetHostName ();
    END;
    FOR i := 0 TO LAST (site.replicas^) DO
      IF CompareToMe(site.replicas[i]) THEN
        thisReplName := site.replicas[i];
      END;
    END;
    IF thisReplName = NIL THEN
      PSLib.LogIt("Local instance not in replica group");
      RAISE InitFailed;
    END;
    lockServerIsMe := CompareToMe(site.lockserver);
    thisIsSiphon := CompareToMe(site.siphonserver);

    ServerLog.WriteText(
       log, "Local configuration (" & thisReplName & "):\n" &
               PrintSiteT(site, st));
    IF owner # NIL THEN
      TRY
        OpSys.SetUser (owner);
      EXCEPT
        | OpSys.Error =>
            PSLib.LogIt("Trouble setting server uid");
            RAISE InitFailed;
      END;
    END;

    IF ipPort # IP.NullPort THEN
      agentLoc := TCPNetObj.Listen(ipPort);
    ELSE
      agentLoc := NIL;
    END;
    
    PSLib.DefineStats("packageserver(" & thisReplName & ")");
    NetObj.Export(SiteObj.SiteObjName, st, agentLoc);

    packageT := PackageServer.New();
    PackageObj.SetServerT(packageT, thisReplName);
    IF lockServerIsMe THEN
            (* this is the lock server *)
      initSite := localSite;
      PSLib.LogIt("Opening package lock database.");
      IF NOT PackageDB.Init (initSite, FALSE) THEN
        IF NOT (initialize AND PackageDB.Init (initSite, TRUE)) THEN
          RAISE InitFailed;
        END;
      END;
      IF NOT Text.Equal (localSite, initSite) THEN
        PSLib.LogIt("Database doesn\'t agree about local site");
        RAISE InitFailed;
      END;
      lockT := LockServer.New();
      LockOps.SetServerT(lockT);
      IF initialize THEN LockRestart.Recover(); END;
      PSLib.LogIt("Starting lock server");
      IF NOT LockRestart.Restart() THEN RAISE InitFailed; END;
      NetObj.Export(PkgProt.LockExportName, lockT, agentLoc);
    END;
    IF packageT # NIL THEN
      PSLib.LogIt("Starting package server");
      PSRestart.Recover (initialize);
      NetObj.Export(PkgProt.PkgExportName, packageT, agentLoc);
    END;
    IF thisIsSiphon THEN
      PSLib.LogIt("Starting siphon server");
      siphonT := SiphonServer.New(localSite);
      Siphon.SetServerT(siphonT, localSite);
      NetObj.Export(Siphon.SiphonExportName, siphonT, agentLoc);
    END;

    (* ServerLog.Register (log); *)
    PSLib.LogIt("Initialization complete");
  END Start;

PROCEDURE PrintSiteT(t: Site.T; st: SiteServer.T) : TEXT =
  VAR out: TEXT;
  BEGIN
    out := "       site=" & t.name & "\n" &
           "       locksrv=" & t.lockserver & "\n" &
           "       owner=" & t.owner & "\n" &
           "       defRep=" & PSLib.PathText(t.defaultRepository) & "\n" &
           PrintTA("       replicas=", t.replicas) &
           PrintTA("       backups=", t.backupHosts);
    IF t.ipPort # IP.NullPort THEN
      out := out & "       ipPort=" & Fmt.Int(t.ipPort) & "\n";
    END;
    IF t.foreignSites # NIL THEN
      out := out & "       siphon=" & t.siphonserver & "\n" &
                                    "       remote sites:\n";
      FOR i := 0 TO LAST(t.foreignSites^) DO
        out := out & PrintTA(
            "         " & t.foreignSites[i].name & " -- siphon=" &
            t.foreignSites[i].siphonserver & " route=",
            t.foreignSites[i].route);
      END;
    END;
    out := out & PrintRepInfo(st);
    RETURN out;
  END PrintSiteT;

PROCEDURE PrintRepInfo(st: SiteServer.T) : TEXT =
  VAR r := st.repInfo();
      out := "";
  BEGIN
    LOCK r.mu DO
      VAR it := r.reps.iterate();
          p: NetPath.T;
          v: REFANY;
          rp: SiteServer.RepData;
          l: TextList.T;
      BEGIN
        out := out & "       Repository map:\n";
        WHILE it.next(p, v) DO
          rp := v;
          out := out & "         " & PSLib.DirText(p) & " -> " & rp.fn & "\n";
          l := rp.vols;
          WHILE l # NIL DO
            out := out & "             vol: " & l.head & "\n";
            l := l.tail;
          END;
        END;
      END;
      VAR it := r.exports.iterate();
          p: NetPath.T;
          v: REFANY;
      BEGIN
        out := out & "       Export map:\n";
        WHILE it.next(p, v) DO
          out := out & "         " & PSLib.DirText(p) & " -> " & v & "\n";
        END;
      END;
    END;
    RETURN out;
  END PrintRepInfo;

PROCEDURE PrintTA(pfx: TEXT; ta: REF ARRAY OF TEXT) : TEXT =
  VAR out := pfx & "(";
  BEGIN
    IF ta # NIL THEN
      FOR i := 0 TO LAST (ta^) DO
        IF i # 0 THEN out := out & " ";  END;
        out := out & ta[i];
      END;
    END;
    RETURN out & ")\n";
  END PrintTA;

BEGIN
  Init ();
END StartPS.
