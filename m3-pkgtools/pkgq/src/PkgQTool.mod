(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

(* Last modified on Mon Dec  2 16:35:46 GMT+1:00 1991 by prusker *)
(*      modified on Fri Apr  5 11:31:22 PST 1991 by wobber *)

SAFE MODULE PkgQTool;

IMPORT Text, Params, PackageLock, PackageLockClient, SiphonControlClient,
  PackageMgr, PackageMgrClient, RPC, FileStream, OS, Ref, Rd, Wr, WrV, Stdio,
  SiphonControl, Time, TimeConv, TextExtra, PackageConfig, Thread;

FROM Stdio IMPORT stdout, stderr;

TYPE
  Action = (CheckConfig, CheckLocal, CheckAll,
              (* action needing nothing *)
            ListDirs, FindPackage, Rename, CreateDir, RemoveDir, Convert,
            Corrupt, Change, Show, Locks, LocalLocks, CheckReplicas, ShowDirs,
              (* actions needing the siphon *)
            Skulk, Enqueue, ShowQueue, Dequeue);
  Field = (Shared, ManagedBy, OwnerKey, OwnerSite, CV, LV, PV, Instance);

TYPE
  DirEnumProc = PROCEDURE(REFANY, (*dirPath*) Text.T, (*child*) Text.T);

TYPE
  PBinding         = PackageLockClient.Binding;
  PMBinding        = PackageMgrClient.Binding;
  SBinding         = SiphonControlClient.Binding;
  ReplicaBindArray = REF ARRAY OF PMBinding;

CONST
  ConvertLockDir = "/proj/checkout";

TYPE
  Elem = REF RECORD
    name:   Text.T;
    ok:     BOOLEAN;
    source: Text.T;
    target: Text.T;
    vers:   PackageLock.Version;
    thread: Thread.T;
  END;

TYPE
  LockOpKind = (Lock, Unlock, CreateInitialized, Remove, RemoveForeign);
  Parameters = REF RECORD
    lockserver:  Text.T;
    package:     Text.T;
    key:         Text.T;
    keySite:     Text.T;
    shared:      BOOLEAN;
    version:     PackageLock.Version;
    remoteCheck: BOOLEAN;
  END;

VAR
  username, password, myInstance: Text.T;
  myWorkingDirName:               Text.T;
  verbose:                        BOOLEAN;
  packagePrefix: Text.T;

PROCEDURE Usage();
  BEGIN
    PRINTF(
      stderr,
"Usage: pkgq [-v] [-g packageGroup] [-s site] [-c config] [-q queue]\n");
    PRINTF(stderr, "       | showqueue <the default action>\n");
    PRINTF(stderr, "       | show [packages ...]\n");
    PRINTF(stderr, "       | showsubreps\n");
    PRINTF(stderr, "       | skulk [packages ...]\n");
    PRINTF(stderr, "       | showdirs [replicas=a+b+c] [subrepnames ...]\n");
    PRINTF(stderr, "       | check [config | local]\n");
    PRINTF(stderr, "       | checkreplicas [replicas=a+b+c] [packages ...]\n");
    PRINTF(stderr, "       | find filter\n");
    PRINTF(stderr, "       | locks packages ...\n");
    PRINTF(stderr, "       | locallocks [packages ...]\n");

    PRINTF(stderr, "       | synch [packages ...]\n");
    PRINTF(stderr, "       | createsubrep subrepname\n");
    PRINTF(stderr, "       | removesubrep subrepname\n");
    PRINTF(stderr, "       | rename [force] sourcePkg targetPkg\n");
    PRINTF(stderr, "       | enqueue [priority] packages ...\n");
    PRINTF(stderr, "       | dequeue [evenIfSent] packages ...\n");
    PRINTF(stderr, "       | specialsynch num [packages ...]\n");
    PRINTF(stderr, "       | change field=value packages ...\n");
    PRINTF(stderr, "       | corrupt [replicas=a+b+c] packages ...\n");
    HALT(1);
  END Usage;

PROCEDURE Run(): BOOLEAN;
  VAR
    xpac, xrep, narg, i, j: CARDINAL;
    enumDirs:               PackageLock.DirList;
    localSite, siteQueue, siteArg, m, arg, errMsg: Text.T;
    group, eText, filter, prefix:  Text.T;
    sourcePkg, targetPkg, dirName: Text.T;
    p:          Parameters;
    try, nGood: CARDINAL;
    lockB:      PBinding;
    siphonB:    SBinding;
    mgrB:       PackageMgrClient.Binding;
    pmB:        PMBinding;
    foreign, packages, replicas: Text.RefArray;
    urgent, evenIfSent, waste, corruptLock: BOOLEAN;
    first, ok, ok1, force: BOOLEAN;
    table:                REF ARRAY OF Elem;
    host:                 Elem;
    action:               Action;
    entry:                PackageLock.Entry;
    el:                   PackageLock.EnumList;
    lockEC:               PackageLock.EC;
    importEC:             RPC.ImportErrors;
    rpcEC:                RPC.CallErrors;
    pmEC:                 PackageMgr.EC;
    version, prevVersion: PackageLock.Version;
    oldLockServer:        Text.T;
    whichField:           Field;
    whichValue, refany:   REFANY;
    bindings:             ReplicaBindArray;
    synchKind:            SiphonControl.SynchKind;
    int:      INTEGER;
    siteInfo: PackageConfig.Site;

  EXCEPTION
    BadConfiguration;

  PROCEDURE GetParam(): Text.T;
    VAR
      t: Text.T;
    BEGIN
      t := Params.GetParameter(narg);
      IF Text.IsEmpty(t) THEN Usage(); END;
      INC(narg);
      RETURN t;
    END GetParam;

  PROCEDURE GetPackageList(must: BOOLEAN);
    VAR
      x, nb: CARDINAL;
    BEGIN
      nb := Params.NumParameters - narg;
      IF nb = 0 THEN
        IF must THEN Usage(); END;
        packages := NIL;
        RETURN;
      END;
      x := narg;
      NEW(packages, nb);
      WHILE narg < Params.NumParameters DO
        packages^[narg - x] := GetPackage(Params.GetParameter(narg));
        INC(narg);
      END;
    END GetPackageList;

  BEGIN
    narg      := 1;
    siteQueue := NIL;
    siteArg  := NIL;
    verbose   := FALSE;

    LOOP
      arg := Params.GetParameter(narg);
      IF Text.IsEmpty(arg) THEN EXIT; END;
      INC(narg);
      IF Text.GetChar(arg, 0) # '-' THEN EXIT; END;
      IF Text.Equal("-g", arg) THEN
        group := GetParam();
      ELSIF Text.Equal("-q", arg) THEN
        siteQueue := GetParam();
      ELSIF Text.Equal("-s", arg) THEN
        siteArg := GetParam();
      ELSIF Text.Equal("-v", arg) THEN
        verbose := TRUE;
      ELSIF Text.Equal("-c", arg) THEN
        PackageConfig.SetConfigFile(GetParam());
      ELSE
        Usage();
      END;
    END;

    TRY
      myInstance := PackageConfig.MyHost(FALSE);
      IF myInstance = NIL THEN
        PRINTF(stderr, "cannot find my host name\n");
        RETURN FALSE;
      END;
      IF siteArg = NIL THEN
        siteInfo := PackageConfig.DefaultSite();
      ELSE
        siteInfo := PackageConfig.GetSite(siteArg);
      END;
      IF siteInfo = NIL THEN 
        PRINTF(stderr, "No such site: %t\n", siteArg);
        RETURN FALSE;
      END;
    EXCEPT
    | PackageConfig.Error(eText):
      PRINTF(stderr, "Bad configuration for local site\n");
      PRINTF(stderr, "    %t\n", eText);
      RETURN FALSE;
    END;
    packagePrefix := siteInfo^.prefix;
    localSite := siteInfo^.name;

    IF Text.IsEmpty(arg) THEN
      action := ShowQueue;
    ELSIF Text.Equal("showqueue", arg) THEN
      action := ShowQueue;
    ELSIF Text.Equal("skulk", arg) THEN
      action    := Skulk;
      synchKind := SiphonControl.CheckOnly;
      GetPackageList(FALSE);
    ELSIF Text.Equal("synch", arg) THEN
      action    := Skulk;
      synchKind := SiphonControl.UpdateAll;
      GetPackageList(FALSE);
    ELSIF Text.Equal("specialsynch", arg) THEN
      action := Skulk;
      TRY
        int := TextExtra.ToInt(GetParam());
        IF (int < 0) OR (int > 7) THEN RAISE(Text.ScanFailed); END;
      EXCEPT Text.ScanFailed:
        PRINTF(stderr, "bad synch kind number %t\n",
               Params.GetParameter(narg - 1));
        RETURN FALSE;
      END;
      synchKind := VAL(SiphonControl.SynchKind, int);
      GetPackageList(FALSE);
    ELSIF Text.Equal("enqueue", arg) THEN
      action := Enqueue;
      IF Text.Equal("priority", Params.GetParameter(narg)) THEN
        INC(narg);
        urgent := TRUE;
      ELSE
        urgent := FALSE;
      END;
      GetPackageList(TRUE);
    ELSIF Text.Equal("dequeue", arg) THEN
      action := Dequeue;
      IF Text.Equal("evenIfSent", Params.GetParameter(narg)) THEN
        INC(narg);
        evenIfSent := TRUE;
      ELSE
        evenIfSent := FALSE;
      END;
      GetPackageList(TRUE);
    ELSIF Text.Equal(arg, "showsubreps") THEN
      action := ListDirs;
    ELSIF Text.Equal(arg, "showdirs") THEN
      action   := ShowDirs;
      replicas := GetReplicasName(Params.GetParameter(narg));
      IF replicas # NIL THEN INC(narg); END;
      GetPackageList(FALSE);
    ELSIF Text.Equal(arg, "find") THEN
      action := FindPackage;
      filter := GetParam();
    ELSIF Text.Equal(arg, "checkreplicas") THEN
      action   := CheckReplicas;
      replicas := GetReplicasName(Params.GetParameter(narg));
      IF replicas # NIL THEN INC(narg); END;
      GetPackageList(FALSE);
    ELSIF Text.Equal(arg, "check") THEN
      arg := Params.GetParameter(narg);
      IF arg # NIL THEN INC(narg); END;
      IF arg = NIL THEN
        action := CheckAll;
      ELSIF Text.Equal(arg, "config") THEN
        action := CheckConfig;
      ELSIF Text.Equal(arg, "local") THEN
        action := CheckLocal;
      ELSE
        Usage();
      END;

    ELSIF Text.Equal(arg, "rename") THEN
      IF Text.Equal("force", Params.GetParameter(narg)) THEN
        INC(narg);
        force := TRUE;
      ELSE
        force := FALSE;
      END;
      force     := FALSE;
      action    := Rename;
      sourcePkg := GetPackage(GetParam());
      targetPkg := GetPackage(GetParam());
    ELSIF Text.Equal(arg, "createsubrep") THEN
      action  := CreateDir;
      dirName := GetPackage(GetParam());
    ELSIF Text.Equal(arg, "removesubrep") THEN
      action  := RemoveDir;
      dirName := GetPackage(GetParam());

        (*****)
    ELSIF Text.Equal(arg, "show") THEN
      action := Show;
      GetPackageList(FALSE);
    ELSIF Text.Equal(arg, "locks") THEN
      action := Locks;
      GetPackageList(TRUE);
    ELSIF Text.Equal(arg, "locallocks") THEN
      action := LocalLocks;
      GetPackageList(FALSE);
    ELSIF Text.Equal(arg, "change") THEN
      action     := Change;
      whichField := GetFieldValueParam(GetParam(), whichValue);
      GetPackageList(TRUE);
    ELSIF Text.Equal(arg, "corrupt") THEN
      action   := Corrupt;
      replicas := GetReplicasName(Params.GetParameter(narg));
      IF replicas # NIL THEN INC(narg); END;
      GetPackageList(TRUE);
    ELSIF Text.Equal(arg, "convertlocks") THEN
      action        := Convert;
      oldLockServer := GetParam();
        (*****)

    ELSE
      Usage();
    END;
    IF narg # Params.NumParameters THEN Usage(); END;

    IF action <= CheckAll THEN RETURN SpecialChecks(action, siteInfo); END;
    TRY
      lockB    := NIL;
      siphonB  := NIL;

      lockB       := PackageLockClient.Import(siteInfo^.lockserver);
      foreign := siteInfo^.foreignSites;
      IF (action >= Skulk) OR ((action = Corrupt) AND (replicas = NIL)) THEN
        IF foreign # NIL THEN
          IF Text.IsEmpty(siteInfo^.siphonserver) THEN
            RAISE(BadConfiguration);
          END;
          siphonB := SiphonControlClient.Import(siteInfo^.siphonserver);
        ELSE
          PRINTF(stderr, "Siphon not enabled\n");
          RETURN FALSE;
        END;
      END;
      NEW(p);
      p^.lockserver := siteInfo^.lockserver;


      CASE action OF
      | Skulk:
        PRINTF(stdout, "Synchronizing remote sites (%t)- wait ...\n",
               SynchKindMsg(synchKind));
        IF packages = NIL THEN
          Wr.PutText(
            stdout, SiphonControlClient.Synch(siphonB, synchKind, NIL));
        ELSE
          FOR xpac := 0 TO HIGH(packages^) DO
            TRY
              Wr.PutText(stdout, SiphonControlClient.Synch(
                                   siphonB, synchKind, packages^[xpac]));
            EXCEPT
            | PackageLock.Error(lockEC):
              PRINTF(stderr, "%t %t\n", PrintPackage(packages^[xpac]),
                        GetLockErrMsg(lockEC));
            END;
          END;
        END;
      | ShowQueue:
        Wr.PutText(
          stdout, SiphonControlClient.ShowQueue(siphonB, siteQueue, verbose));
      | Enqueue:
        FOR xpac := 0 TO HIGH(packages^) DO
          TRY
            PackageLockClient.GetEntry(lockB, packages^[xpac], entry);
            version.vn := entry.curVN;
            version.t  := entry.instance;
            Wr.PutText(stdout, SiphonControlClient.EnqueueForSend(
                                 siphonB, entry.package, version,
                                 entry.managedBy, siteQueue, urgent, TRUE));
          EXCEPT
          | PackageLock.Error(lockEC):
            PRINTF(stderr, "Error on package %t: %t\n",
                      PrintPackage(packages^[xpac]), GetLockErrMsg(lockEC));
          END;
        END;
      | Dequeue:
        FOR xpac := 0 TO HIGH(packages^) DO
          TRY
            Wr.PutText(
              stdout, SiphonControlClient.RemoveFromQueue(
                        siphonB, packages^[xpac], siteQueue, evenIfSent));
          EXCEPT
          | PackageLock.Error(lockEC):
            PRINTF(stderr, "Error on package %t: %t\n",
                      PrintPackage(packages^[xpac]), GetLockErrMsg(lockEC));
          END;
        END;
      | ListDirs:
        enumDirs := PackageLockClient.EnumerateDirs(lockB);
        IF (enumDirs = NIL) OR (NUMBER(enumDirs^) = 0) THEN
          PRINTF(stderr, "No sub-repositories registered at lock server!!!\n");
          RETURN FALSE;
        END;
        FOR i := 0 TO HIGH(enumDirs^) DO
          PRINTF(stdout, "%t\n", enumDirs^[i]);
        END;
      | FindPackage:
        el := PackageLockClient.Enumerate(
                lockB, NIL, FALSE, FALSE, FALSE, FALSE, filter);
        FOR i := 0 TO HIGH(el^) DO PrintEntry(el^[i], localSite); END;

      | ShowDirs:
        IF replicas = NIL THEN
          replicas := siteInfo^.replicas;
          IF (replicas = NIL) OR (NUMBER(replicas^) = 0) THEN
            RAISE(BadConfiguration);
          END;
        END;
        IF NOT GetReplicas(replicas, bindings, errMsg) THEN
          PRINTF(stderr, "%t\n", errMsg);
          RETURN FALSE;
        END;
        PRINTF(stdout, "Checking sub-repositories at replicas %t\n",
                  TextFromList(replicas));
        enumDirs := packages;
        IF enumDirs = NIL THEN
          enumDirs := PackageLockClient.EnumerateDirs(lockB);
          IF (enumDirs = NIL) OR (NUMBER(enumDirs^) = 0) THEN
            PRINTF(stderr, "No sub-repositories at lock server!!!\n");
            RETURN FALSE;
          END;
        ELSE
          FOR i := 0 TO HIGH(enumDirs^) DO
            TRY PackageLockClient.CheckDir(lockB, enumDirs^[i]); EXCEPT
            | PackageLock.Error(lockEC):
              IF lockEC = PackageLock.NoSuchDir THEN
                PRINTF(stdout, "sub-repository %t unknown at lock server\n",
                       enumDirs^[i]);
              ELSE
                RAISE(PackageLock.Error, lockEC);
              END;
            END;
          END;
        END;
        FOR i := 0 TO HIGH(enumDirs^) DO
          CheckReplicaDirs(stdout, enumDirs^[i], replicas, bindings);
        END;
        FOR i := 0 TO HIGH(replicas^) DO
          IF bindings^[i] = NIL THEN
            PRINTF(
              stderr, "WARNING: replica  %t not reachable\n", replicas^[i]);
          END;
        END;

      | CheckReplicas:
        IF replicas = NIL THEN
          replicas := siteInfo^.replicas;
          IF (replicas = NIL) OR (NUMBER(replicas^) = 0) THEN
            RAISE(BadConfiguration);
          END;
        END;
        IF NOT GetReplicas(replicas, bindings, errMsg) THEN
          PRINTF(stderr, "%t\n", errMsg);
        ELSE
          PRINTF(stderr, "Checking current version at replicas %t",
                    TextFromList(replicas));
          IF packages = NIL THEN
            PRINTF(stderr, " - for all packages. Wait ...\n");
            el := PackageLockClient.Enumerate(
                    lockB, NIL, FALSE, FALSE, FALSE);
            FOR i := 0 TO HIGH(el^) DO
              CheckServers(stdout, FALSE, FALSE, el^[i], replicas, bindings);
            END;
          ELSE
            PRINTF(stderr, "\n");
            FOR xpac := 0 TO HIGH(packages^) DO
              TRY
                PackageLockClient.GetEntry(lockB, packages^[xpac], entry);
                CheckServers(stdout, TRUE, FALSE, entry, replicas, bindings);
              EXCEPT
              | PackageLock.Error:
                entry.package := packages^[xpac];
                CheckServers(stdout, TRUE, TRUE, entry, replicas, bindings);
              END;
            END;
          END;
          FOR i := 0 TO HIGH(replicas^) DO
            IF bindings^[i] = NIL THEN
              PRINTF(
                stderr, "WARNING: replica  %t not reachable\n", replicas^[i]);
            END;
          END;
        END;

      | CreateDir:
        replicas := siteInfo^.replicas;
        IF (replicas = NIL) OR (NUMBER(replicas^) = 0) THEN
          RAISE(BadConfiguration);
        END;
          (* checking for replicas directories *)
        m := NIL;
        FOR i := 0 TO HIGH(replicas^) DO
          TRY
            mgrB := PackageMgrClient.Import(replicas^[i]);
            PackageMgrClient.CheckDir(mgrB, dirName);
            IF m = NIL THEN
              m := replicas^[i];
            ELSE
              m := Text.Cat(m, ", ", replicas^[i]);
            END;
          EXCEPT
          | RPC.CallFailed, PackageMgr.Error, RPC.ImportFailed:
          END;
        END;
        IF m = NIL THEN
          PRINTF(stdout, "WARNING: %t unknown at all replicas\n", dirName);
        ELSE
            (* PRINTF(stdout, "%t exists at replicas: %t\n", dirName, m); *)
        END;
        PRINTF(
          stdout, "Creating sub-repository %t\n", dirName);
        PackageLockClient.CreateDir(lockB, username, password, dirName);
        PRINTF(stdout, "Sub-repository created\n");

      | RemoveDir:
        prefix := Text.Cat(dirName, "/");
        FOR try := 1 TO 2 DO
            (* first, enumerate packages in this directory *)
          el := PackageLockClient.Enumerate(
                  lockB, NIL, FALSE, FALSE, FALSE, FALSE, prefix);

          IF (el # NIL) AND (NUMBER(el^) # 0) THEN
              (* check for local entries *)
            first := TRUE;
            FOR i := 0 TO HIGH(el^) DO
              WITH el^[i] DO
                IF Text.Equal(managedBy, localSite) THEN
                  IF first THEN
                    first := FALSE;
                    PRINTF(
                      stderr,
                      "Sub-repository %t contains locally managed packages:\n",
                      dirName);
                  END;
                  PRINTF(stderr, "   %t\n", package);
                END;
              END;
            END;
            IF NOT first THEN RETURN FALSE; END;

              (* delete all remote entries *)
            PRINTF(stdout, "Deleting all foreign entries in %t\n", dirName);
            FOR i := 0 TO HIGH(el^) DO
              p^.package := el^[i].package;
              IF NOT DoLockOp(RemoveForeign, p) THEN RETURN FALSE; END;
            END;
          END;
          PRINTF(
            stdout, "Removing sub-repository %t\n", dirName);
          TRY
            PackageLockClient.RemoveDir(lockB, username, password, dirName);
            PRINTF(stdout, "Sub-repository removed\n");
            RETURN TRUE;
          EXCEPT
          | PackageLock.Error(lockEC):
            IF (try = 1) AND (lockEC = PackageLock.DirNotEmpty) THEN
              PRINTF(stdout, "Sub-repository not empty - retrying\n");
            ELSE
              RAISE(PackageLock.Error, lockEC);
            END;
          END;
        END;
        RETURN FALSE;

      | Rename:
        replicas := siteInfo^.replicas;
        IF (replicas = NIL) OR (NUMBER(replicas^) = 0) THEN
          RAISE(BadConfiguration);
        END;

          (* target package must be creatable (it can exist) *)
        PackageLockClient.CheckDir(lockB, Parent(targetPkg));
        TRY
          PackageLockClient.GetEntry(lockB, targetPkg, entry);
          PRINTF(stderr, "%t already exists\n", targetPkg);
          RETURN FALSE;
        EXCEPT
        | PackageLock.Error(lockEC):
          IF lockEC # PackageLock.NoSuchPackage THEN
            RAISE(PackageLock.Error, lockEC);
          END;
        END;

          (* source package must be local *)
        PackageLockClient.GetEntry(lockB, sourcePkg, entry);
        IF NOT Text.Equal(entry.managedBy, localSite) THEN
          PRINTF(stderr, "Package not managed locally\n");
          RETURN FALSE;
        END;
        p^.shared := entry.shared;

          (* lock source package *)
        p^.package := sourcePkg;
        p^.key     := BuildKey();
        p^.keySite := localSite;
        IF NOT DoLockOp(Lock, p) THEN RETURN FALSE; END;


          (* moving package at package servers *)
        PRINTF(stdout, "Moving %t to %t on %t\n", sourcePkg, targetPkg,
                  TextFromList(replicas));
        Wr.Flush(stdout);
        ok := FALSE;
        TRY
          NEW(table, NUMBER(replicas^));
          FOR i := 0 TO HIGH(table^) DO
            NEW(host);
            table^[i] := host;
            WITH host^ DO
              ok     := FALSE;
              name   := replicas^[i];
              source := sourcePkg;
              target := targetPkg;
              vers   := p^.version;
              thread := Thread.Fork(CopyPackageFork, host);
            END;
          END;
          nGood := 0;
          FOR i := 0 TO HIGH(table^) DO
            host   := table^[i];
            refany := Thread.Join(host^.thread);
            IF host^.ok THEN INC(nGood); END;
          END;
          IF nGood = 0 THEN
            PRINTF(stderr, "Copy failed on all replicas\n");
            RETURN FALSE;
          END;
          PRINTF(stdout, "Copy succeeded at replicas");
          first := TRUE;
          FOR i := 0 TO HIGH(table^) DO
            host := table^[i];
            IF host^.ok THEN
              IF first THEN first := FALSE; ELSE PRINTF(stdout, ","); END;
              PRINTF(stdout, " %t", host^.name);
            END;
          END;
          PRINTF(stdout, "\n");

            (* create and unlock target package *)
          p^.remoteCheck := NOT force;
          p^.package     := targetPkg;
          ok             := DoLockOp(CreateInitialized, p);
          ok1            := ok AND DoLockOp(Unlock, p);
          p^.package     := sourcePkg;

        FINALLY
            (* delete or unlock source package *)
          IF ok THEN
            ok := DoLockOp(Remove, p) AND ok1;
          ELSE
            waste := DoLockOp(Unlock, p);
          END;
        END;
        RETURN ok;



          (*****)
      | Convert:
        ConvertLocks(lockB, oldLockServer);
      | Corrupt:
        prevVersion.t  := 0;
        prevVersion.vn := PackageLock.NullVN;
        IF replicas = NIL THEN
          corruptLock := TRUE;
          replicas    := siteInfo^.replicas;
          IF (replicas = NIL) OR (NUMBER(replicas^) = 0) THEN
            RAISE(BadConfiguration);
          END;
        ELSE
          corruptLock := FALSE;
        END;
        FOR xpac := 0 TO HIGH(packages^) DO
          FOR xrep := 0 TO HIGH(replicas^) DO
            PRINTF(stderr, "Marking %t corrupt at %t ... ",
                      PrintPackage(packages^[xpac]), replicas^[xrep]);
            Wr.Flush(stderr);
            TRY
              pmB     := PackageMgrClient.Import(replicas^[xrep]);
              version := PackageMgrClient.GetVersion(pmB, packages^[xpac]);
              version.vn := PackageLock.NullVN;
              waste := PackageMgrClient.CommitVersion(
                         pmB, packages^[xpac], version, prevVersion);
              ASSERT(waste);
              PRINTF(stderr, "ok\n");
            EXCEPT
            | PackageMgr.Error(pmEC):
              PRINTF(stderr, "%t\n", GetMgrErrMsg(pmEC));
            | RPC.ImportFailed, RPC.CallFailed:
              PRINTF(stderr, "RPC error\n");
            END;
          END;
          IF corruptLock THEN
            TRY
              PRINTF(stderr, "Resetting %t curVersion ... ",
                        PrintPackage(packages^[xpac]));
              Wr.Flush(stderr);
              SiphonControlClient.InvalidateCache(siphonB, packages^[xpac]);
              ChangeEntry(lockB, packages^[xpac], CV,
                          Ref.NewInteger(PackageLock.InitialVN));
              PRINTF(stderr, "ok\n");
            EXCEPT
            | PackageLock.Error(lockEC):
              PRINTF(stderr, "%t\n", GetLockErrMsg(lockEC));
            END;
          END;
        END;
      | Change:
        FOR xpac := 0 TO HIGH(packages^) DO
          TRY
            ChangeEntry(lockB, packages^[xpac], whichField, whichValue);
          EXCEPT
          | PackageLock.Error(lockEC):
            PRINTF(stderr, "Error on package %t: %t\n",
                      PrintPackage(packages^[xpac]), GetLockErrMsg(lockEC));
          END;
        END;
      | Show:
        IF packages = NIL THEN
          el :=
            PackageLockClient.Enumerate(lockB, NIL, FALSE, FALSE, FALSE);
          FOR i := 0 TO HIGH(el^) DO PrintEntry(el^[i], localSite); END;
        ELSE
          FOR xpac := 0 TO HIGH(packages^) DO
            TRY
              PackageLockClient.GetEntry(lockB, packages^[xpac], entry);
              PrintEntry(entry, localSite);
            EXCEPT
            | PackageLock.Error(lockEC):
              PRINTF(stderr, "Error on package %t: %t\n",
                        PrintPackage(packages^[xpac]), GetLockErrMsg(lockEC));
            END;
          END;
        END;
      | LocalLocks:
        IF packages = NIL THEN
          el :=
            PackageLockClient.Enumerate(lockB, NIL, TRUE, FALSE, FALSE);
          PRINTF(stdout, "Packages managed by local site and locked\n");
          FOR i := 0 TO HIGH(el^) DO PrintLock(el^[i], FALSE); END;
        ELSE
          FOR xpac := 0 TO HIGH(packages^) DO
            TRY
              PackageLockClient.GetEntry(lockB, packages^[xpac], entry);
              IF entry.owner.key = NIL THEN
                IF Text.Equal(entry.managedBy, localSite) THEN
                  PRINTF(stdout, 
                      "%t is not locked\n", PrintPackage(entry.package));
                ELSE
                  PRINTF(stdout, 
    "%t: cannot tell lock status: managing site is remote (%t)\n",
                      PrintPackage(entry.package), entry.managedBy);
                END;
              ELSE
                PrintLock(entry, TRUE);
              END;
            EXCEPT
            | PackageLock.Error(lockEC):
              PRINTF(stderr, "Error on package %t: %t\n",
                        PrintPackage(packages^[xpac]), GetLockErrMsg(lockEC));
            END;
          END;
        END;
      | Locks:
        FOR xpac := 0 TO HIGH(packages^) DO
          TRY
            PackageLockClient.GetEntry(lockB, packages^[xpac], entry, TRUE);
            IF entry.owner.key = NIL THEN
              PRINTF(stdout, "%t is not locked\n", PrintPackage(entry.package));
            ELSE
              PrintLock(entry, TRUE);
            END;
          EXCEPT
          | PackageLock.Error(lockEC):
            PRINTF(stderr, "Error on package %t: %t\n",
                      PrintPackage(packages^[xpac]), GetLockErrMsg(lockEC));
          END;
        END;
          (*****)
      END;
      RETURN TRUE;
    EXCEPT
    | RPC.ImportFailed(importEC):
      IF lockB = NIL THEN m := "lock"; ELSE m := "siphon"; END;
      PRINTF(stderr, "Cannot contact %t server: %t\n", m,
                GetImportErrMsg(importEC));
    | RPC.CallFailed(rpcEC):
      IF (lockB # NIL) AND RPC.GetLastCallFailed(lockB) THEN
        m := "lock";
      ELSE
        m := "siphon";
      END;
      PRINTF(
        stderr, "RPC error with %t server: %t\n", m, GetRPCErrMsg(rpcEC));
    | PackageLock.Error(lockEC):
      PRINTF(stderr, "Lock server error: %t\n", GetLockErrMsg(lockEC));
    | BadConfiguration:
      IF siteArg = NIL THEN m := "local site"; ELSE m := siteArg; END;
      PRINTF(stderr, "Site %t: bad configuration\n", m);
    END;
    RETURN FALSE;
  END Run;

PROCEDURE GetFieldValueParam(arg: Text.T; VAR refVal: REFANY): Field;
  VAR
    f: Field;
    fieldText, valText: Text.T;
    i: INTEGER;
  BEGIN
    i := Text.FindChar(arg, 0, '=');
    IF i < 0 THEN Usage(); END;
    fieldText := Text.SubText(arg, 0, i);
    valText   := Text.SubText(arg, i + 1);
    IF Text.Equal(fieldText, "shared") THEN
      f      := Shared;
      refVal := GetBoolParam(valText);
    ELSIF Text.Equal(fieldText, "managedBy") THEN
      f      := ManagedBy;
      refVal := valText;
    ELSIF Text.Equal(fieldText, "ownerKey") THEN
      f      := OwnerKey;
      refVal := valText;
    ELSIF Text.Equal(fieldText, "ownerSite") THEN
      f      := OwnerSite;
      refVal := valText;
    ELSIF Text.Equal(fieldText, "instance") THEN
      f      := Instance;
      refVal := GetIntParam(valText);
    ELSIF Text.Equal(fieldText, "CV") THEN
      f      := CV;
      refVal := GetIntParam(valText);
    ELSIF Text.Equal(fieldText, "LV") THEN
      f      := LV;
      refVal := GetIntParam(valText);
    ELSIF Text.Equal(fieldText, "PV") THEN
      f      := PV;
      refVal := GetIntParam(valText);
    ELSE
      PRINTF(stderr, "Bad field name, possible field names are:\n");
      PRINTF(
        stderr,
        "   shared, managedBy, ownerKey, ownerSite, inst, CV, LV, PV\n");
      HALT(1);
    END;
    RETURN f;
  END GetFieldValueParam;

PROCEDURE GetIntParam(t: Text.T): REFANY;
  VAR
    i:  INTEGER;
    rd: Rd.T;
  BEGIN
    IF Text.Equal(t, "DeletedVN") THEN
      i := PackageLock.DeletedVN;
    ELSE
      TRY
        rd := Rd.FromText(t);
        SCANF(rd, "%d", i);
      EXCEPT
      | Rd.ScanFailed, Rd.EndOfFile:
        PRINTF(stderr, "Bad integer field value\n");
        HALT(1);
      END;
    END;
    RETURN Ref.NewInteger(i);
  END GetIntParam;

PROCEDURE GetBoolParam(t: Text.T): REFANY;
  VAR
    bool: BOOLEAN;
  BEGIN
    IF Text.Equal(t, "TRUE") THEN
      bool := TRUE;
    ELSIF Text.Equal(t, "FALSE") THEN
      bool := FALSE;
    ELSE
      PRINTF(stderr, "Bad boolean field value\n");
      HALT(1);
    END;
    RETURN Ref.NewBoolean(bool);
  END GetBoolParam;

PROCEDURE GetReplicasName(t: Text.T): Text.RefArray;
  VAR
    ra:       Text.RefArray;
    end, pos: INTEGER;
    arr:      ARRAY [0..9] OF Text.T;
    i:        CARDINAL;
  CONST
    SearchPattern = "replicas=";
  BEGIN
    i   := 0;
    pos := Text.Find(t, 0, SearchPattern);
    IF pos # 0 THEN RETURN NIL; END;
    pos := pos + Text.Length(SearchPattern);
    LOOP
      ASSERT(i < NUMBER(arr));
      end := Text.FindChar(t, pos, '+');
      IF end < 0 THEN
        arr[i] := Text.SubText(t, pos);
        INC(i);
        EXIT;
      ELSE
        IF (end # pos) THEN
          arr[i] := Text.SubText(t, pos, (end - pos));
          INC(i);
        END;
        pos := end + 1;
      END;
    END;
    NEW(ra, i);
    REPEAT
      DEC(i);
      ra^[i] := arr[i];
    UNTIL (i = 0);
    RETURN ra;
  END GetReplicasName;

PROCEDURE Date(time: Time.Seconds): Text.T;
  VAR
    t: Time.T;
  BEGIN
    t.seconds      := time;
    t.microseconds := 0;
    RETURN TimeConv.TimeLocalToText(t, TRUE);
  END Date;

PROCEDURE ConvertLocks(b: PBinding; oldLockServer: Text.T);
  VAR
    fullPath: Text.T;
  BEGIN
    fullPath := Text.Cat("#", oldLockServer, ConvertLockDir);
    PRINTF(stdout, "\nAcquiring locks according to %t\n", fullPath);
    EnumerateDir(fullPath, b, AcquireLock);
  END ConvertLocks;

PROCEDURE AcquireLock(r: REFANY; dirPath: Text.T; fn: Text.T);
  VAR
    ec:       PackageLock.EC;
    rd:       Rd.T;
    pkg, key: Text.T;
    lc:       PackageLock.Owner;
    v:        PackageLock.Version;
    b:        PBinding;
  BEGIN
    b   := NARROW(r, PBinding);
    pkg := TranslateLockName(fn, '+', '/');
    PRINTF(stdout, "Acquiring lock for %t\n", pkg);
    rd := FileStream.OpenRead(NIL, Text.Cat(dirPath, "/", fn));
    TRY SCANF(rd, "%t", key); EXCEPT
    | Rd.EndOfFile, Rd.ScanFailed, Rd.Error:
      PRINTF(stderr, "   ???? bad lock file format\n");
      Rd.Close(rd);
      RETURN;
    END;
    Rd.Close(rd);
    PRINTF(stdout, "   key is %t ... ", key);
    Wr.Flush(stdout);
    TRY
      v.t  := 0;
      v.vn := PackageLock.NullVN;
      v    := PackageLockClient.Lock(b, "pkgsrvr", NIL, pkg, v, key);
      PRINTF(stdout, "ok\n");
    EXCEPT
    | PackageLock.Error(ec):
      CASE ec OF
      | PackageLock.NoSuchPackage:
        PRINTF(stdout, "creating .. \n");
        Wr.Flush(stdout);
        PackageLockClient.Create(b, "pkgsrvr", NIL, pkg, key, TRUE);
        PRINTF(stdout, "ok\n");
      | ELSE
        RAISE(PackageLock.Error, ec);
      END;
    | PackageLock.LockConflict(lc):
      PRINTF(stderr, "PackageLock.LockConflict(%t)\n", lc.key);
    END;
  END AcquireLock;

PROCEDURE EnumerateDir(path: Text.T; ref: REFANY; proc: DirEnumProc);
  VAR
    rd:  Rd.T;
    t:   Text.T;
    dir: OS.Dir;
  PROCEDURE SpecialFile(t: Text.T): BOOLEAN;
    BEGIN
      RETURN (Text.GetChar(t, 0) = '.') OR Text.Equal(t, "backups") OR
             Text.Equal(t, "tmp");
    END SpecialFile;
  BEGIN
    dir := OS.OpenDir(NIL, path);
    rd  := NIL;
    OS.ListDir(dir, rd);
    REPEAT
      t := Rd.GetText(rd, LAST(Rd.Index));
      IF NOT SpecialFile(t) THEN proc(ref, path, t); END;
    UNTIL NOT OS.NextEntry(rd);
  END EnumerateDir;

PROCEDURE TranslateLockName(t: Text.T; from, to: CHAR): Text.T;
  VAR
    c:      CHAR;
    i, len: CARDINAL;
    wr:     WrV.T;
  BEGIN
    len := Text.Length(t);
    WrV.New(wr, len);
    FOR i := 0 TO len - 1 DO
      c := Text.GetChar(t, i);
      IF c = from THEN WrV.PutChar(wr, to); ELSE WrV.PutChar(wr, c); END;
    END;
    RETURN WrV.ToText(wr);
  END TranslateLockName;

PROCEDURE PrintEntry(VAR IN entry: PackageLock.Entry; localSite: Text.T);
  VAR
    shared:         Text.T;
  BEGIN
    IF verbose THEN
      PrintEntryVerbose(entry);
      RETURN;
    END;
    IF entry.shared THEN shared := ""; ELSE shared := "(not shared)"; END;
    IF entry.owner.key # NIL THEN
      shared := Text.Cat(shared, "(locked)");
    END;
    PRINTF(stdout, "%t: %t%t  cv=%d", PrintPackage(entry.package),
              entry.managedBy, shared, entry.curVN);
    IF (entry.curVN # entry.lastVN) 
       AND (Text.Equal(localSite, entry.managedBy)) THEN
      PRINTF(stdout, " lv=%d", entry.lastVN);
    END;
    IF entry.pendVN # PackageLock.NullVN THEN
      PRINTF(stdout, " pv=%d", entry.pendVN);
    END;
    PRINTF(stdout, "  %t\n", Date(entry.lastModified));
  END PrintEntry;

PROCEDURE PrintEntryVerbose(VAR IN e: PackageLock.Entry);
  VAR
    localStr: Text.T;
  BEGIN
    PRINTF(stdout, "%t\n", PrintPackage(e.package));
    IF e.owner.key # NIL THEN
      PRINTF(stdout, "  Locked: %t (%t)\n", e.owner.key, e.owner.site);
    END;
    IF e.shared THEN
      localStr := "shared";
    ELSE
      localStr := "not shared";
    END;
    PRINTF(stdout, "  Managed by: %t  (%t)\n", e.managedBy, localStr);
    PRINTF(
      stdout, "  Instance=%d; lv=%d; cv=%d; pv=%d\n",
      e.instance, e.lastVN, e.curVN, e.pendVN);
    PRINTF(stdout, "  Last modified: %t\n", Date(e.lastModified));
  END PrintEntryVerbose;

PROCEDURE PrintLock(VAR IN entry: PackageLock.Entry; one: BOOLEAN);
  VAR m: Text.T;
  BEGIN
    IF verbose THEN
      PrintEntryVerbose(entry);
    ELSE
      IF one THEN m := "checked out to" ELSE m := "->"; END;
      PRINTF(stdout, "%t %t %t (%t)\n", PrintPackage(entry.package), m,
        entry.owner.key, entry.owner.site);
    END;
  END PrintLock;
  
PROCEDURE ChangeEntry(b: PBinding; which: Text.T; field: Field; val: REFANY);
  VAR
    e:     PackageLock.Entry;
    rBool: Ref.Boolean;
    rInt:  Ref.Integer;
  BEGIN
    PackageLockClient.GetEntry(b, which, e);
    CASE field OF
    | Shared:
      rBool    := NARROW(val, Ref.Boolean);
      e.shared := rBool^;
    | ManagedBy:
      e.managedBy := NARROW(val, Text.T);
    | OwnerKey:
      e.owner.key := NARROW(val, Text.T);
    | OwnerSite:
      e.owner.site := NARROW(val, Text.T);
    | CV:
      rInt    := NARROW(val, Ref.Integer);
      e.curVN := rInt^;
    | LV:
      rInt     := NARROW(val, Ref.Integer);
      e.lastVN := rInt^;
    | PV:
      rInt     := NARROW(val, Ref.Integer);
      e.pendVN := rInt^;
    | Instance:
      rInt       := NARROW(val, Ref.Integer);
      e.instance := rInt^;
    END;
    PackageLockClient.SetEntry(b, "pkgsrvr", NIL, which, e);
  END ChangeEntry;

PROCEDURE GetPackage(package: Text.T): Text.T;
  BEGIN
    IF Text.GetChar(package, 0) # '/' THEN
      package := Text.Cat(packagePrefix, package);
    END;
    IF NOT CheckName(package) THEN
      PRINTF(stderr, "Bad package name: %t\n", package);
      HALT(1);
    END;
    RETURN package;
  END GetPackage;

PROCEDURE CheckName(name: Text.T): BOOLEAN;
  (* check if name is well formed *)
  (* raises BadParameter if empty, no beginning /, double /, end / *)
  VAR
    lg, x: CARDINAL;
    slash: BOOLEAN;
  BEGIN
    lg := Text.Length(name);
    IF lg = 0 THEN RETURN FALSE; END;
    IF Text.GetChar(name, 0) # '/' THEN RETURN FALSE; END;
    slash := TRUE;
    FOR x := 1 TO lg - 1 DO
      IF Text.GetChar(name, x) = '/' THEN
        IF slash THEN RETURN FALSE; END;
        slash := TRUE;
      ELSE
        slash := FALSE;
      END;
    END;
    IF slash THEN RETURN FALSE; END;
    RETURN TRUE;
  END CheckName;

PROCEDURE PrintPackage(package: Text.T): Text.T;
  BEGIN
    IF Text.Equal(
         packagePrefix, Text.SubText(package, 0, Text.Length(packagePrefix)))
    THEN
      RETURN Text.SubText(package, Text.Length(packagePrefix));
    ELSE
      RETURN package;
    END;
  END PrintPackage;

PROCEDURE GetImportErrMsg(ec: RPC.ImportErrors): Text.T RAISES {};
  (* printing on stdout or log file *)
  VAR
    m:  Text.T;
    wr: Wr.T;
  BEGIN
    wr := Wr.New();
    CASE ec OF
    | RPC.localAgentFailed:
      m := "local RPCAgent";
    | RPC.nameServiceFailed:
      m := "no NameServer";
    | RPC.nameServiceRejected:
      m := "can't resolve name";
    | RPC.exportersAgentFailed:
      m := "remote RPCAgent";
    | RPC.notExported:
      m := "no server";
    | RPC.exporterFailed:
      m := "exporter failed";
    | RPC.protocolVersion:
      m := "RPC mismatch";
    | RPC.interfaceVersion:
      m := "interface mismatch";
    ELSE
      m := NIL;
    END;
    PRINTF(wr, "import error(%d): %t", ORD(ec), m);
    RETURN Wr.ToText(wr);
  END GetImportErrMsg;

PROCEDURE GetRPCErrMsg(ec: RPC.CallErrors): Text.T RAISES {};
  (* printing on stdout or log file *)
  VAR
    m:  Text.T;
    wr: Wr.T;
  BEGIN
    wr := Wr.New();
    CASE ec OF
    | RPC.transportFailure:
      m := "transport failure";
    | RPC.exportersHostFailed:
      m := "timeout";
    | RPC.exporterBusy:
      m := "exporter busy";
    | RPC.exporterUnbound:
      m := "exporter unbound";
    | RPC.callProtocolError:
      m := "call protocol error";
    | RPC.returnProtocolError:
      m := "return protocol error";
    ELSE
      m := NIL;
    END;
    PRINTF(wr, "rpc error(%d): %t", ORD(ec), m);
    RETURN Wr.ToText(wr);
  END GetRPCErrMsg;

PROCEDURE GetLockErrMsg(ec: PackageLock.EC): Text.T RAISES {};
  (* printing on stdout or log file *)
  VAR
    m:  Text.T;
    wr: Wr.T;
  BEGIN
    wr := Wr.New();
    CASE ec OF
    | PackageLock.InvalidCredentials:
      m := "invalid credentials";
    | PackageLock.ServerProblem:
      m := "server problem";
    | PackageLock.BadParameter:
      m := "bad paramater";
    | PackageLock.NoSuchPackage:
      m := "no such package";
    | PackageLock.PackageNameInUse:
      m := "package name in use";
    | PackageLock.PackageIsBusy:
      m := "package is busy";
    | PackageLock.BadVersionStamp:
      m := "version stamp differs";
    | PackageLock.StaleVersion:
      m := "remote site has same or newer version";
    | PackageLock.OldLocalVersion:
      m := "old local version";
    | PackageLock.OutstandingVersion:
      m := "outstanding version";
    | PackageLock.PackageManagedRemotely:
      m := "package managed remotely";
    | PackageLock.PackageNotManaged:
      m := "package not managed";
    | PackageLock.PackageMultiplyManaged:
      m := "multiple managers";
    | PackageLock.LocalLockServerDown:
      m := "lockserver unavailable";
    | PackageLock.RemoteLockServerDown:
      m := "lockserver at remote site is unavailable";
    | PackageLock.RemoteSiphonDown:
      m := "siphon at remote site is unavailable";
    | PackageLock.LocalSiphonDown:
      m := "can't contact remote site - local siphon down";
    | PackageLock.NoSuchSite:
      m := "bad remote site name";
    | PackageLock.NoSuchDir:
      m := "no such sub-repository";
    | PackageLock.DirNameInUse:
      m := "sub-repository name in use";
    | PackageLock.ParentDirExists:
      m := "parent sub-repository exists";
    | PackageLock.DirNotEmpty:
      m := "sub-repository not empty";
    ELSE
      m := NIL;
    END;
    PRINTF(wr, "lock error(%d): %t", ORD(ec), m);
    RETURN Wr.ToText(wr);
  END GetLockErrMsg;

PROCEDURE GetMgrErrMsg(ec: PackageMgr.EC): Text.T RAISES {};
  (* printing on stdout or log file *)
  VAR
    m:  Text.T;
    wr: Wr.T;
  BEGIN
    wr := Wr.New();
    CASE ec OF
    | PackageMgr.InvalidCredentials:
      m := "invalid credentials";
    | PackageMgr.AccessViolation:
      m := "access violation";
    | PackageMgr.CantCreateAtPath:
      m := "can't create package";
    | PackageMgr.NoSuchFile:
      m := "BUG! no such file";
    | PackageMgr.NoSuchPackage:
      m := "no such package";
    | PackageMgr.InvalidHandle:
      m := "BUG! invalid handle";
    | PackageMgr.InvalidOperation:
      m := "BUG! invalid operation";
    | PackageMgr.StateError:
      m := "BUG! state error";
    | PackageMgr.NoRoomInFS:
      m := "no room in file system";
    | PackageMgr.IOError:
      m := "server i/o error";
    | PackageMgr.ServerProblem:
      m := "unexpected server problem";
    | PackageMgr.CommitNotPrepared:
      m := "BUG! commit not prepared";
    ELSE
      m := NIL;
    END;
    PRINTF(wr, "mgr error(%d): %t", ORD(ec), m);
    RETURN Wr.ToText(wr);
  END GetMgrErrMsg;

PROCEDURE CheckServers(
         wrArg:      Wr.T;
         verbose:    BOOLEAN;
         anyversion: BOOLEAN;           (* entry.version not meaningful *)
  VAR IN entry:      PackageLock.Entry;
         replicas:   Text.RefArray;
         bindings:   ReplicaBindArray
  );
  VAR
    finalTxt:  Text.T;
    i:       CARDINAL;
    msg:     Text.T;
    version: PackageLock.Version;
    mgrEC:   PackageMgr.EC;
    wr:      Wr.T;
  BEGIN
    wr := Wr.New();
    FOR i := 0 TO HIGH(replicas^) DO
      msg := NIL;
      IF bindings^[i] # NIL THEN
        TRY
          version :=
            PackageMgrClient.GetVersion(bindings^[i], entry.package);
          IF anyversion OR (version.t # entry.instance) OR
             (version.vn # entry.curVN)
          THEN
            PRINTF(
              wr, " %t:  %d.%d;", replicas^[i], version.t, version.vn);
          ELSE
            IF verbose THEN msg := "ok"; END;
          END;
        EXCEPT
        | RPC.CallFailed:
          bindings^[i] := NIL;
        | PackageMgr.Error(mgrEC):
          IF mgrEC = PackageMgr.NoSuchPackage THEN
            msg := "no such package";
            TRY
                (* Do a TestCreate instead of CheckDir, in case the package
                   is individually disabled (3maxserver kack) *)
              PackageMgrClient.TestCreate(bindings^[i], entry.package);
            EXCEPT
            | PackageMgr.Error(mgrEC):
              IF mgrEC = PackageMgr.CantCreateAtPath THEN
                IF verbose THEN
                  msg := "no sub-repository";
                ELSE
                  msg := NIL;
                END;
              END;
            | RPC.CallFailed:
              bindings^[i] := NIL;
            END;
          ELSE
            msg := Text.Cat("package server error: ", GetMgrErrMsg(mgrEC));
          END;
        END;
      END;
      IF bindings^[i] = NIL THEN
        IF verbose THEN msg := "not reachable"; ELSE msg := NIL; END;
      END;
      IF msg # NIL THEN PRINTF(wr, " %t: %t;", replicas^[i], msg); END;
    END;

    finalTxt := Wr.ToText(wr);
    IF anyversion OR NOT Text.IsEmpty(finalTxt) THEN
      PRINTF(wrArg, "%t", PrintPackage(entry.package));
      IF anyversion THEN
        PRINTF(wrArg, " (unknown at lock server)");
      ELSE
        PRINTF(wrArg, " (%d.%d)", entry.instance, entry.curVN);
      END;
      PRINTF(wrArg, "  %t\n", finalTxt);
    END;
  END CheckServers;

PROCEDURE GetReplicas(
  VAR (* in *)  replicas: Text.RefArray;
  VAR (* out *) bindings: ReplicaBindArray;
  VAR (* out *) errMsg:   Text.T
  ): BOOLEAN; (* TRUE if at least one replica *)
  VAR
    i: CARDINAL;
  BEGIN
    errMsg := "all replicas down";
    NEW(bindings, NUMBER(replicas^));
    FOR i := 0 TO HIGH(replicas^) DO
      TRY
        bindings^[i] := PackageMgrClient.Import(replicas^[i]);
        errMsg       := NIL;
      EXCEPT
      | RPC.ImportFailed:
        bindings^[i] := NIL;
      END;
    END;
    RETURN errMsg = NIL;
  END GetReplicas;

PROCEDURE SynchKindMsg(synchKind: SiphonControl.SynchKind): Text.T;
  VAR
    m: Text.T;
  BEGIN
    CASE synchKind OF
    | SiphonControl.CheckOnly:
      m := "check only";
    | SiphonControl.UpdateLocalFromConnected:
      m := "update local site from connected sites";
    | SiphonControl.UpdateLocalFromAny:
      m := "update local site";
    | SiphonControl.UpdateLocalDirect:
      m := "update local site directly";
    | SiphonControl.UpdateLocallyConnected:
      m := "update locally connected sites";
    | SiphonControl.UpdateFromLocal:
      m := "update all sites from local site";
    | SiphonControl.UpdateAllConnected:
      m := "update all sites using connected paths";
    | SiphonControl.UpdateAll:
      m := "update all sites";
    END;
    RETURN m;
  END SynchKindMsg;

PROCEDURE CopyPackageFork(a: REFANY): REFANY;
  VAR
    host: Elem;
    b:    PackageMgrClient.Binding;
  BEGIN
    host := NARROW(a, Elem);
    WITH host^ DO
      TRY
        b  := PackageMgrClient.Import(name);
        ok := PackageMgrClient.Copy(b, source, target, vers, TRUE);
      EXCEPT
      | RPC.CallFailed, PackageMgr.Error, RPC.ImportFailed:
      END;
    END;
    RETURN NIL;
  END CopyPackageFork;


PROCEDURE DoLockOp(op: LockOpKind; p: Parameters): BOOLEAN;
  VAR
    b:     PackageLockClient.Binding;
    ic:    RPC.ImportErrors;
    rc:    RPC.CallErrors;
    ec:    PackageLock.EC;
    cf:    PackageLock.CommitFailures;
    owner: PackageLock.Owner;
  BEGIN
    WITH p^ DO
      TRY
        b := PackageLockClient.Import(lockserver);
        CASE op OF
        | Remove:
          PRINTF(stdout, "Removing lock entry for %t\n", package);
          PackageLockClient.Remove(
            b, username, password, package, key, FALSE);
          PRINTF(stdout, "Removed\n");
        | Lock:
          version.t  := 0;
          version.vn := PackageLock.NullVN;
          PRINTF(stdout, "Locking package %t\n", package);
          version :=
            PackageLockClient.Lock(
              b, username, password, package, version, key, keySite);
            (* PRINTF(stdout, "Locked: %t\n", key); *)
        | Unlock:
          version.t  := 0;
          version.vn := PackageLock.NullVN;
          PRINTF(stdout, "Unlocking %t\n", package);
          PackageLockClient.Unlock(
            b, username, password, package, version, key);
          PRINTF(stdout, "Unlocked\n");
        | CreateInitialized:
          PRINTF(stdout, "Creating lock entry for %t\n", package);
          PackageLockClient.CreateInitialized(
            b, username, password, package, key, shared, version,
            remoteCheck);
            (* PRINTF(stdout, "Created and locked: %t\n", key); *)
        | RemoveForeign:
          PRINTF(stdout, "Removing foreign lock entry for %t\n", package);
          PackageLockClient.RemoveForeign(b, username, password, package);
          PRINTF(stdout, "Removed\n");
        END;
      EXCEPT
      | RPC.ImportFailed(ic):
        PRINTF(
          stderr, "Cannot contact lock server: %t\n", GetImportErrMsg(ic));
        RETURN FALSE;
      | PackageLock.CommitFailed(cf):
        PrintCommitFailures(cf);
        Wr.PutText(stderr, "Commit failed at all replicas\n");
        RETURN FALSE;
      | PackageLock.Error(ec):
        PRINTF(stderr, "Lock server error: %t\n", GetLockErrMsg(ec));
        RETURN FALSE;
      | RPC.CallFailed(rc):
        PRINTF(
          stderr, "RPC error with lock server: %t\n", GetRPCErrMsg(rc));
        RETURN FALSE;
      | PackageLock.LockConflict(owner):
        IF owner.key # NIL THEN
          PRINTF(stderr, "Error: package checked out to %t (%t)\n",
                    owner.key, owner.site);
        ELSE
          PRINTF(stderr, "Error: package not checked out\n");
        END;
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END DoLockOp;

PROCEDURE PrintCommitFailures(cf: PackageLock.CommitFailures);
  VAR
    i: CARDINAL;
  VAR
    str: Text.T;
  BEGIN
    IF (cf = NIL) OR (NUMBER(cf^) = 0) THEN RETURN; END;
    FOR i := 0 TO HIGH(cf^) DO
      CASE cf^[i].what OF
      | PackageLock.OK:
        str := NIL;
      | PackageLock.RPCImportFailure:
        str := "import failure";
      | PackageLock.RPCCallFailure:
        str := "rpc call failure";
      | PackageLock.NoPendingCommit:
        str := "no pending commit";
      | PackageLock.AccessNotAllowed:
        str := "access violation";
      | PackageLock.NotHoldingOldVersion:
        str := "not up-to-date";
      | PackageLock.Other:
        str := "other";
      | PackageLock.CantCreatePath:
        str := "no parent sub-repository";
      END;
      IF str # NIL THEN
        PRINTF(stderr, "Commit failure at %t; %t\n", cf^[i].which, str);
      END;
    END;
  END PrintCommitFailures;

PROCEDURE Parent(name: Text.T): Text.T;
  (* well formed name *)
  VAR
    i: CARDINAL;
  BEGIN
    i := Text.Length(name);
    REPEAT DEC(i) UNTIL Text.GetChar(name, i) = '/';
    RETURN Text.SubText(name, 0, i)
  END Parent;

PROCEDURE BuildKey(): Text.T;
  VAR
    myHostName: Text.T;
  VAR
    eText: Text.T;
  BEGIN
    IF (Text.GetChar(myWorkingDirName, 0) = '#') OR
       Text.Equal("/-", myWorkingDirName) OR
       Text.Equal("/-/", Text.SubText(myWorkingDirName, 0, 3))
    THEN
      RETURN myWorkingDirName;
    END;
      (* the following code only executes under Ultrix *)
    RETURN Text.Cat("#", myInstance, myWorkingDirName);
  END BuildKey;

PROCEDURE TextFromList(l: Text.RefArray): Text.T;
  VAR
    wr: Wr.T;
  VAR
    i: CARDINAL;
  BEGIN
    IF (l = NIL) OR (NUMBER(l^) = 0) THEN RETURN NIL; END;
    wr := Wr.New();
    FOR i := 0 TO HIGH(l^) DO
      IF i # 0 THEN Wr.PutChar(wr, '+'); END;
      Wr.PutText(wr, l^[i]);
    END;
    RETURN Wr.ToText(wr);
  END TextFromList;

PROCEDURE CheckReplicaDirs(
  wr:       Wr.T;
  dir:      PackageLock.Dir;
  replicas: Text.RefArray;
  bindings: ReplicaBindArray
  );
  VAR
    i, nok: CARDINAL;
    array:  REF ARRAY OF BOOLEAN;
    first:  BOOLEAN;
  BEGIN
    NEW(array, NUMBER(replicas^));
    nok := 0;
    FOR i := 0 TO HIGH(replicas^) DO
      array^[i] := FALSE;
      IF bindings^[i] # NIL THEN
        TRY
          PackageMgrClient.CheckDir(bindings^[i], dir);
          array^[i] := TRUE;
          INC(nok);
        EXCEPT
        | PackageMgr.Error:
        | RPC.CallFailed:
          bindings^[i] := NIL;
        END;
      END;
    END;
    PRINTF(wr, "%t ", dir);
    IF nok = 0 THEN
      PRINTF(wr, "unknown at all replicas");
    ELSIF nok = NUMBER(replicas^) THEN
      PRINTF(wr, "exists at all replicas");
    ELSE
      first := TRUE;
      FOR i := 0 TO HIGH(replicas^) DO
        IF array^[i] THEN
          IF first THEN
            first := FALSE;
            PRINTF(wr, "at replicas ");
          ELSE
            PRINTF(wr, "+");
          END;
          PRINTF(wr, "%t", replicas^[i]);
        END;
      END;
    END;
    PRINTF(wr, "\n");
  END CheckReplicaDirs;

PROCEDURE SpecialChecks(
  action:    Action;
  localSite: PackageConfig.Site
  ): BOOLEAN;
  VAR
    foreign:      Text.RefArray;
    i:            CARDINAL;
    error, ok:    BOOLEAN;
    foreignTable: REF ARRAY OF PackageConfig.Site;
    name, err:    Text.T;
    site:         PackageConfig.Site;
  BEGIN
    error   := NOT CheckSite(TRUE, localSite);
    foreign := localSite^.foreignSites;
    IF foreign = NIL THEN
      error := TRUE;
    ELSE
      NEW(foreignTable, NUMBER(foreign^));
      FOR i := 0 TO HIGH(foreign^) DO
        name             := foreign^[i];
        ok               := FALSE;
        TRY
          site := PackageConfig.GetSite(name);
          IF site = NIL THEN
            PRINTF(
              stdout, "Error: no configuration for remote site %t\n", name);
          ELSE
            ok := CheckSite(FALSE, site);
          END;
        EXCEPT
        | PackageConfig.Error(err):
          site := NIL;
          PRINTF(stdout, "Bad configuration for remote site %t\n", name);
          PRINTF(stdout, "    %t\n", err);
        END;
        foreignTable^[i] := site;
        error := error OR NOT ok;
      END;
    END;
    IF NOT error THEN PRINTF(stdout, "Configuration file ok\n"); END;
    IF action = CheckConfig THEN RETURN NOT error; END;

      (* calling local servers *)
    CallSite(TRUE, localSite);
    IF action = CheckLocal THEN RETURN NOT error; END;
    IF foreign # NIL THEN
      FOR i := 0 TO HIGH(foreign^) DO
        IF foreignTable^[i] # NIL THEN CallSite(FALSE, foreignTable^[i]) END;
      END;
    END;
    RETURN NOT error;
  END SpecialChecks;

PROCEDURE CallSite(local: BOOLEAN; site: PackageConfig.Site);
  VAR
    importEC: RPC.ImportErrors;
    i:        CARDINAL;
    lockb:    PackageLockClient.Binding;
    mgrb:     PackageMgrClient.Binding;
    siphonb:  SiphonControlClient.Binding;
    sitetxt:  Text.T;
  BEGIN
    IF site = NIL THEN RETURN; END;
    WITH site^ DO
      IF local THEN sitetxt := "local"; ELSE sitetxt := "remote"; END;
      sitetxt := Text.Cat(sitetxt, " site ", name);
      IF NOT Text.IsEmpty(lockserver) THEN
        PRINTF(
          stdout, "Calling lock server %t at %t... ", lockserver, sitetxt);
        Wr.Flush(stdout);
        TRY
          lockb := PackageLockClient.Import(lockserver);
          PRINTF(stdout, "ok\n");
        EXCEPT
        | RPC.ImportFailed(importEC):
          PRINTF(stdout, "failed\n    %t\n", GetImportErrMsg(importEC));
          RETURN;
        END;
      END;
      IF local AND (replicas # NIL) THEN
        FOR i := 0 TO HIGH(replicas^) DO
          PRINTF(stdout, "Calling package server %t at %t... ", replicas^[i],
                 sitetxt);
          Wr.Flush(stdout);
          TRY
            mgrb := PackageMgrClient.Import(replicas^[i]);
            PRINTF(stdout, "ok\n");
          EXCEPT
          | RPC.ImportFailed(importEC):
            PRINTF(stdout, "failed\n    %t\n", GetImportErrMsg(importEC));
          END;
        END;
      END;
      IF NOT Text.IsEmpty(siphonserver) THEN
        PRINTF(stdout, "Calling siphon server %t at %t... ", siphonserver,
               sitetxt);
        Wr.Flush(stdout);
        TRY
          siphonb := SiphonControlClient.Import(siphonserver);
          PRINTF(stdout, "ok\n");
        EXCEPT
        | RPC.ImportFailed(importEC):
          PRINTF(stdout, "failed\n    %t\n", GetImportErrMsg(importEC));
        END;
      END;
    END;
  END CallSite;
  
PROCEDURE SpecialGet(name, value: Text.T): Text.T;
  VAR
    adr: Text.T;
  BEGIN
    TRY
      adr := PackageConfig.Get(name, value);
      RETURN adr;
    EXCEPT
    | PackageConfig.Error:
      RETURN NIL;
    END;
  END SpecialGet;
  
PROCEDURE CheckSite(local: BOOLEAN; site: PackageConfig.Site): BOOLEAN;
  VAR
    errored: BOOLEAN;
    i:       CARDINAL;
  PROCEDURE Err(msg: Text.T);
    BEGIN
      IF NOT errored THEN
        errored := TRUE;
        PRINTF(stdout, "Bad configuration for site %t\n", site^.name);
      END;
      PRINTF(stdout, "    %t\n", msg);
    END Err;
  PROCEDURE CheckMachine(machine: Text.T);
    VAR
      adr: Text.T;
    BEGIN
      IF NOT PackageConfig.export THEN RETURN; END;
      adr := SpecialGet(machine, "address");
      IF Text.IsEmpty(adr) THEN
        adr := SpecialGet(machine, "decnetAddress");
        IF Text.IsEmpty(adr) THEN
          Err(Text.Cat(machine, " has not a network address"));
        END;
      END;
    END CheckMachine;
  BEGIN
    WITH site^ DO
      errored := FALSE;
      IF Text.IsEmpty(lockserver) THEN
        Err("no lock server");
      ELSE
        CheckMachine(lockserver);
      END;
      IF Text.IsEmpty(siphonserver) THEN
        Err("no siphon server");
      ELSE
        CheckMachine(siphonserver);
      END;
      IF (replicas = NIL) OR (NUMBER(replicas^) = 0) THEN
        IF local THEN Err("no package server"); END;
      ELSE
        FOR i := 0 TO HIGH(replicas^) DO CheckMachine(replicas^[i]); END;
      END;
      IF (foreignSites = NIL) OR (NUMBER(foreignSites^) = 0) THEN
        IF local THEN Err("no foreign sites"); END;
      END;
      IF (foreignSiteWeights = NIL) OR (NUMBER(foreignSiteWeights^) = 0) THEN
        Err("no weights");
      END;
    END;
    RETURN NOT errored;
  END CheckSite;

  
VAR
  pInfo: OS.ProcessInfo;

BEGIN
  TRY
    myWorkingDirName := OS.GetPath(OS.OpenDir(NIL, NIL));
    OS.GetProcessInfo(OS.NullPID, pInfo);
    username := pInfo.realUser;
    password := NIL;
  EXCEPT
  | OS.Error:
    PRINTF(stderr, "Can't find working directory\n");
    HALT(1);
  END;
(*
  TRY
    PackageConfig.SetConfigFile("/perle/usr/adm/siphon/config");
  EXCEPT
  | PackageConfig.Error:
      PRINTF( stderr, "PackageConfig Error\n");
      HALT(1);
  END;
*)
  IF NOT Run() THEN HALT(1); END;
END PkgQTool.
