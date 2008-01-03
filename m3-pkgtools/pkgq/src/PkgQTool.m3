(* Copyright 1902 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

(* Last modified on Thu Feb  2 09:43:30 PST 1995 by kalsow  *)
(*      modified on Thu Oct 13 13:30:32 PDT 1994 by wobber  *)
(*      modified on Mon Dec  2 16:35:46 GMT+1:00 1991 by prusker *)

MODULE PkgQTool EXPORTS Main;

IMPORT Text, Params, PackageObj, LockOps, Siphon, Site,
        NetPath, NetObj, Wr, Fmt, Stdio, Time, Thread,
        PkgErr, FloatMode, TextWr, SiteObj, IP, FmtTime, Process,
        Rd, TextRd, Lex;

FROM Stdio IMPORT stdout, stderr;

TYPE
  Dir = PackageObj.Dir;
  PN = PackageObj.PN;
  Version = LockOps.Version;

TYPE
  Action = {
              (* action needing nothing *)
            ListDirs, FindPackage, CreateDir, RemoveDir, Corrupt, Change,
            ChangeOwner, Show, Locks, LocalLocks, CheckReplicas, ShowDirs,
              (* actions needing the siphon *)
            Skulk, Enqueue, Status, Dequeue};
  Field = {ManagedBy, OwnerKey, OwnerSite, CV, LV, PV, Instance};
  ReplicaArray = REF ARRAY OF PackageObj.T;

VAR
  verbose: BOOLEAN;

VAR myAuth: PackageObj.Auth := "pkgq";
    defaultDir: NetPath.T;

<* FATAL Thread.Alerted *>

PROCEDURE Usage() =
  BEGIN
    PutText(stderr,
"Usage: pkgq [-v] [-f] [-s site] [-q queue]\n");
    PutText(stderr, "   | status <the default action>\n");
    PutText(stderr, "   | show [packages ...]\n");
    PutText(stderr, "   ! showreps\n");
    PutText(stderr, "   | skulk [packages ...]\n");
    PutText(stderr, "   | showdirs [replicas=a+b+c] [subrepnames ...]\n");
    PutText(stderr, "   | check [replicas=a+b+c] [packages ...]\n");
    PutText(stderr, "   | find packageNameArc\n");
    PutText(stderr, "   | locks packages ...\n");
    PutText(stderr, "   | locallocks [packages ...]\n");
    PutText(stderr, "   | synch [packages ...]\n");
    PutText(stderr, "   | createrep repositoryName\n");
    PutText(stderr, "   | removerep repositoryName\n");
    PutText(stderr, "   | enqueue packages ...\n");
    PutText(stderr, "   | dequeue packages ...\n");
    PutText(stderr, "   | change field=value packages ...\n");
    PutText(stderr, "   | corrupt [replicas=a+b+c] packages ...\n");
    PutText(stderr, "   | changeowner site packages ...\n");
    Process.Exit(1);
  END Usage;

PROCEDURE Run(): BOOLEAN =
  VAR
    narg: CARDINAL;
    el: LockOps.EnumList;
    siteArg, packageNameArc: TEXT;
    localSite, siteQueue, m, arg: TEXT;
    dirName: Dir;
    replicas: REF ARRAY OF TEXT;
    forceFlag, waste, corruptLock: BOOLEAN;
    first: BOOLEAN;
    action:               Action;
    entry:                LockOps.RefEntry;
    version, prevVersion: Version;
    whichField:           Field;
    whichValue:   REFANY;
    synchKind:            Siphon.SynchKind;
    site: Site.T;
    packages:   REF ARRAY OF PN;
    pkgT: PackageObj.T;
    siphonT: Siphon.T;
    bindings: ReplicaArray;
    dirs: LockOps.DirList;
    siteParam: TEXT;

  PROCEDURE GetParam(): Text.T =
    VAR
      t: Text.T;
    BEGIN
      IF narg >= Params.Count THEN Usage(); END;
      t := Params.Get(narg);
      IF Text.Empty(t) THEN Usage(); END;
      INC(narg);
      RETURN t;
    END GetParam;

  PROCEDURE GetPackageList(must: BOOLEAN) =
    VAR
      x, nb: CARDINAL;
    BEGIN
      nb := Params.Count - narg;
      IF nb = 0 THEN
        IF must THEN Usage(); END;
        packages := NIL;
        RETURN;
      END;
      x := narg;
      packages := NEW(REF ARRAY OF PN, nb);
      WHILE narg < Params.Count DO
        packages^[narg - x] := GetPackage(Params.Get(narg));
        INC(narg);
      END;
    END GetPackageList;

  PROCEDURE GetDirList(must: BOOLEAN) =
    VAR
      x, nb: CARDINAL;
    BEGIN
      nb := Params.Count - narg;
      IF nb = 0 THEN
        IF must THEN Usage(); END;
        dirs := NIL;
        RETURN;
      END;
      x := narg;
      dirs := NEW(REF ARRAY OF NetPath.T, nb);
      WHILE narg < Params.Count DO
        dirs^[narg - x] := GetDir(Params.Get(narg));
        INC(narg);
      END;
    END GetDirList;

  PROCEDURE CheckReplicaList() =
    BEGIN
      IF narg < Params.Count THEN
        replicas := GetReplicaList(Params.Get(narg));
        IF replicas # NIL THEN INC(narg); END;
      END;
    END CheckReplicaList;

  BEGIN
    arg       := NIL;
    narg      := 1;
    siteQueue := NIL;
    siteArg  := NIL;
    verbose   := FALSE;

    WHILE narg < Params.Count DO
      arg := GetParam();
      IF Text.GetChar(arg, 0) # '-' THEN EXIT; END;
      IF Text.Equal("-q", arg) THEN
        siteQueue := GetParam();
      ELSIF Text.Equal("-s", arg) THEN
        siteArg := GetParam();
      ELSIF Text.Equal("-f", arg) THEN
        forceFlag := TRUE;
      ELSIF Text.Equal("-v", arg) THEN
        verbose := TRUE;
      ELSE
        Usage();
      END;
      arg := NIL;
    END;

    IF arg = NIL OR Text.Empty(arg) OR Text.Equal("status", arg) THEN
      TRY
        site := Site.Init();
        IF siteArg # NIL AND NOT Text.Equal(siteArg, site.name) THEN
          siphonT := Siphon.New(siteArg);
        ELSE
          siphonT := Siphon.New(NIL);
        END;
        PutText(stdout, siphonT.status());
      EXCEPT
      | Site.Error(ec) =>
          PutText(stderr, Site.ErrMsg(ec) & "\n");
          RETURN FALSE;
      | PkgErr.E(ec) =>
          PutText(stderr, PkgErr.Msg(ec) & "\n");
          RETURN FALSE;
      | NetObj.Error(ec) =>
          PutText(stderr, PkgErr.Msg(ec) & "\n");
          RETURN FALSE;
      END;
      RETURN TRUE;
    END;

    TRY
      site := Site.Init();
      IF siteArg # NIL AND NOT Text.Equal(siteArg, site.name) THEN
        VAR importS: TEXT; rem: Site.Remote; BEGIN
          IF NOT Site.FindRemote(siteArg, rem) THEN
            PutText(stderr, "No such site:"  & siteArg & "\n");
            RETURN FALSE;
          END;
          importS := rem.siphonserver;
          IF rem.ipPort # IP.NullPort THEN
            importS := importS & ":" & Fmt.Int(rem.ipPort);
          END;
          SiteObj.SetServerST(SiteObj.Import(importS));
          site := Site.Init();
        END;
      END;
    EXCEPT
    | Site.Error(ec) =>
        PutText(stderr, Site.ErrMsg(ec) & "\n");
        RETURN FALSE;
    END;
    localSite := site.name;
    defaultDir := site.defaultRepository;

    IF Text.Equal("skulk", arg) THEN
      action    := Action.Skulk;
      synchKind := Siphon.SynchKind.CheckOnly;
      GetPackageList(FALSE);
    ELSIF Text.Equal("synch", arg) THEN
      action    := Action.Skulk;
      synchKind := Siphon.SynchKind.UpdateAll;
      GetPackageList(FALSE);
    ELSIF Text.Equal("enqueue", arg) THEN
      action := Action.Enqueue;
      GetPackageList(TRUE);
    ELSIF Text.Equal("dequeue", arg) THEN
      action := Action.Dequeue;
      GetPackageList(TRUE);
    ELSIF Text.Equal(arg, "showreps") THEN
      action := Action.ListDirs;
    ELSIF Text.Equal(arg, "showdirs") THEN
      action   := Action.ShowDirs;
      CheckReplicaList();
      GetDirList(FALSE);
    ELSIF Text.Equal(arg, "find") THEN
      action := Action.FindPackage;
      packageNameArc := GetParam();
    ELSIF Text.Equal(arg, "check") THEN
      action   := Action.CheckReplicas;
      CheckReplicaList();
      GetPackageList(FALSE);

    ELSIF Text.Equal(arg, "createrep") THEN
      action  := Action.CreateDir;
      dirName := GetDir(GetParam());
    ELSIF Text.Equal(arg, "removerep") THEN
      action  := Action.RemoveDir;
      dirName := GetDir(GetParam());

        (*****)
    ELSIF Text.Equal(arg, "show") THEN
      action := Action.Show;
      GetPackageList(FALSE);
    ELSIF Text.Equal(arg, "locks") THEN
      action := Action.Locks;
      GetPackageList(TRUE);
    ELSIF Text.Equal(arg, "locallocks") THEN
      action := Action.LocalLocks;
      GetPackageList(FALSE);
    ELSIF Text.Equal(arg, "change") THEN
      action     := Action.Change;
      whichField := GetFieldValueParam(GetParam(), whichValue);
      GetPackageList(TRUE);
    ELSIF Text.Equal(arg, "corrupt") THEN
      action   := Action.Corrupt;
      CheckReplicaList();
      GetPackageList(TRUE);
    ELSIF Text.Equal(arg, "changeowner") THEN
      action   := Action.ChangeOwner;
      siteParam := GetParam();
      IF NOT Text.Equal(siteParam, site.name) THEN
        VAR rem: Site.Remote; BEGIN
          IF NOT Site.FindRemote(siteParam, rem) THEN
            PutText(stderr, "No such site:"  & siteParam & "\n");
            RETURN FALSE;
          END;
        END;
      END;
      GetPackageList(TRUE);

    ELSE
      Usage();
    END;
    IF narg # Params.Count THEN Usage(); END;

    TRY
      IF (action >= Action.Skulk) OR
            ((action = Action.Corrupt) AND (replicas = NIL)) THEN
        IF site.foreignSites = NIL THEN
          PutText(stderr, "Siphon not enabled\n");
          RETURN FALSE;
        END;
        siphonT := Siphon.New(NIL);
      END;
      CASE action OF
      | Action.Status =>
      | Action.Skulk =>
        PutText(stdout, Fmt.F("Synchronizing remote sites (%s)- wait ...\n",
               SynchKindMsg(synchKind)));
        IF packages = NIL THEN
          PutText(stdout, siphonT.synch(synchKind, PN{NIL, NIL}));
        ELSE
          FOR xpac := 0 TO LAST(packages^) DO
            PutText(stdout, siphonT.synch(synchKind, packages^[xpac]));
          END;
        END;
      | Action.Enqueue =>
        FOR xpac := 0 TO LAST(packages^) DO
          TRY
            entry := LockOps.GetEntry(packages^[xpac]);
            version.vn := entry.curVN;
            version.t  := entry.instance;
            siphonT.enqueue(packages^[xpac], version,
                                 entry.managedBy, siteQueue, forceFlag);
          EXCEPT
          | PkgErr.E(ec) =>
              PutText(stderr, Fmt.F("Error on package %s: %s\n",
                      PrintPackage(packages^[xpac]), PkgErr.Msg(ec)));
          END;
        END;
      | Action.Dequeue =>
        FOR xpac := 0 TO LAST(packages^) DO
          TRY
            EVAL  siphonT.dequeue(packages^[xpac], siteQueue, forceFlag);
          EXCEPT
          | PkgErr.E(ec) =>
              PutText(stderr, Fmt.F("Error on package %s: %s\n",
                      PrintPackage(packages^[xpac]), PkgErr.Msg(ec)));
          END;
        END;
      | Action.ListDirs =>
        dirs := LockOps.EnumerateDirs();
        IF (dirs = NIL) OR (NUMBER(dirs^) = 0) THEN
          PutText(stderr,
                "No repositories registered at lock server!!!\n");
          RETURN FALSE;
        END;
        FOR i := 0 TO LAST(dirs^) DO
          PutText(stdout, Fmt.F("%s\n", PathToText(dirs^[i])));
        END;
      | Action.FindPackage =>
        dirs := LockOps.EnumerateDirs();
        IF dirs # NIL THEN
          FOR i := 0 TO LAST(dirs^) DO
            TRY
              VAR pkg := PN{dirs[i], packageNameArc}; BEGIN
                EVAL LockOps.GetEntry(pkg);
                PutText(stdout, PrintPackage(pkg) & "\n");
              END;
            EXCEPT
            | PkgErr.E(ec) =>
                IF ec.head # PkgErr.NoSuchPackage THEN RAISE PkgErr.E(ec); END;
            END;
          END;
        END;
      | Action.ShowDirs =>
        IF replicas = NIL THEN
          replicas := site.replicas;
        END;
        bindings := GetReplicas(replicas);
        IF bindings = NIL THEN
          PutText(stderr, "all replicas down\n");
          RETURN FALSE;
        END;
        PutText(stdout, Fmt.F("Checking repositories at replicas %s\n",
                  TextFromList(replicas)));
        IF dirs = NIL THEN
          dirs := LockOps.EnumerateDirs();
          IF (dirs = NIL) OR (NUMBER(dirs^) = 0) THEN
            PutText(stderr, "No repositories at lock server!!!\n");
            RETURN FALSE;
          END;
        ELSE
          FOR i := 0 TO LAST(dirs^) DO
            TRY LockOps.CheckDir(dirs^[i]); EXCEPT
            | PkgErr.E(lockEC) =>
              IF lockEC.head = PkgErr.NoSuchDir THEN
                PutText(stdout,
                   Fmt.F( "repository %s unknown at lock server\n",
                       PathToText(dirs^[i])));
              ELSE
                RAISE PkgErr.E(lockEC);
              END;
            END;
          END;
        END;
        FOR i := 0 TO LAST(dirs^) DO
          CheckReplicaDirs(stdout, dirs^[i], replicas, bindings);
        END;
        FOR i := 0 TO LAST(replicas^) DO
          IF bindings^[i] = NIL THEN
            PutText(
              stderr, Fmt.F(
                "WARNING: replica  %s not reachable\n", replicas^[i]));
          END;
        END;

      | Action.CheckReplicas =>
        IF replicas = NIL THEN
          replicas := site.replicas;
        END;
        bindings := GetReplicas(replicas);
        IF bindings = NIL THEN
          PutText(stderr, "all replicas down\n");
        ELSE
          PutText(stderr, "Checking current version at replicas " &
                    TextFromList(replicas));
          IF packages = NIL THEN
            PutText(stderr, " - for all packages. Wait ...\n");
            dirs := LockOps.EnumerateDirs();
            IF dirs # NIL THEN
              FOR i := 0 TO LAST(dirs^) DO
                el := LockOps.Enumerate(dirs[i], NIL, FALSE, FALSE, FALSE);
                IF el # NIL THEN
                  FOR j := 0 TO LAST(el^) DO
                    CheckServers(stdout, FALSE,
                        PN{dirs[i], el[j].arc}, el[j].e,
                        replicas, bindings);
                  END;
                END;
              END;
            END;
          ELSE
            PutText(stderr, "\n");
            FOR xpac := 0 TO LAST(packages^) DO
              entry := LockOps.GetEntry(packages^[xpac]);
              CheckServers(stdout, TRUE, packages^[xpac],
                            entry^, replicas, bindings);
            END;
          END;
          FOR i := 0 TO LAST(replicas^) DO
            IF bindings[i] = NIL THEN
              PutText(
                stderr, Fmt.F("WARNING: replica  %s not reachable\n",
                            replicas^[i]));
            END;
          END;
        END;

      | Action.CreateDir =>
        replicas := site.replicas;
          (* checking for replicas directories *)
        m := NIL;
        FOR i := 0 TO LAST(replicas^) DO
          TRY
            pkgT := PackageObj.New(replicas^[i]);
            pkgT.checkDir(dirName, NIL);
            IF m = NIL THEN
              m := replicas^[i];
            ELSE
              m := m & ", " & replicas^[i];
            END;
          EXCEPT
          | NetObj.Error, PkgErr.E =>
          END;
        END;
        IF m = NIL THEN
          PutText(stdout,
            Fmt.F("WARNING: %s unknown at all replicas\n",
                  PathToText(dirName)));
        ELSE
            (* PRINTF(stdout, "%s exists at replicas: %s\n", dirName, m); *)
        END;
        PutText(stdout,
           Fmt.F("Creating repository %s\n", PathToText(dirName)));
        LockOps.CreateDir(myAuth, dirName);
        PutText(stdout, "Repository created\n");

      | Action.RemoveDir =>
            (* first, enumerate packages in this directory *)
          el := LockOps.Enumerate(dirName, NIL, FALSE, FALSE, FALSE);

          IF (el # NIL) AND (NUMBER(el^) # 0) THEN
              (* check for local entries *)
            first := TRUE;
            FOR i := 0 TO LAST(el^) DO
              WITH e = el^[i].e DO
                IF Text.Equal(e.managedBy, localSite) THEN
                  IF first THEN
                    first := FALSE;
                    PutText(
                      stderr, Fmt.F(
                      "Repository %s contains locally managed packages:\n",
                      PathToText(dirName)));
                  END;
                  PutText(stderr, Fmt.F("   %s\n", el[i].arc));
                END;
              END;
            END;
            IF NOT first THEN RETURN FALSE; END;

              (* delete all remote entries *)
            PutText(stdout,
                 Fmt.F( "Deleting all foreign entries in %s\n",
                        PathToText(dirName)));
            FOR i := 0 TO LAST(el^) DO
              LockOps.RemoveForeign(myAuth, PN{dirName, el[i].arc});
            END;
          END;
          PutText(stdout, Fmt.F("Removing repository %s\n",
                      PathToText(dirName)));
          LockOps.RemoveDir(myAuth, dirName);
          PutText(stdout, "repository removed\n");
          RETURN TRUE;

          (*****)

      | Action.Corrupt =>
        prevVersion.t  := 0;
        prevVersion.vn := LockOps.NullVN;
        IF replicas = NIL THEN
          corruptLock := TRUE;
          replicas    := site.replicas;
        ELSE
          corruptLock := FALSE;
        END;
        FOR xpac := 0 TO LAST(packages^) DO
          FOR xrep := 0 TO LAST(replicas^) DO
            PutText(stderr, Fmt.F("Marking %s corrupt at %s ... ",
                      PrintPackage(packages^[xpac]), replicas^[xrep]), TRUE);
            TRY
              pkgT     := PackageObj.New(replicas^[xrep]);
              version := pkgT.version(packages^[xpac]);
              version.vn := LockOps.NullVN;
              waste := pkgT.vcommit(myAuth,
                         packages^[xpac], version, prevVersion);
              <* ASSERT(waste) *>
              PutText(stderr, "ok\n");
            EXCEPT
            | PkgErr.E(pmEC) =>
                PutText(stderr, PkgErr.Msg(pmEC) & "\n");
            | NetObj.Error(ec) =>
                PutText(stderr, PkgErr.Msg(ec) & "\n");
            END;
          END;
          IF corruptLock THEN
            TRY
              VAR ri: REF INTEGER := NEW(REF INTEGER); BEGIN
                PutText(stderr, Fmt.F("Resetting %s curVersion ... ",
                        PrintPackage(packages^[xpac])), TRUE);
                ri^ := LockOps.InitialVN;
                ChangeEntry(packages^[xpac], Field.CV, ri);
                PutText(stderr, "ok\n");
              END;
            EXCEPT
            | PkgErr.E(lockEC) =>
              PutText(stderr, PkgErr.Msg(lockEC) & "\n");
            END;
          END;
        END;
      | Action.Change =>
        FOR xpac := 0 TO LAST(packages^) DO
          TRY
            ChangeEntry(packages^[xpac], whichField, whichValue);
          EXCEPT
          | PkgErr.E(lockEC) =>
              PutText(stderr, Fmt.F("Error on package %s: %s\n",
                      PrintPackage(packages^[xpac]), PkgErr.Msg(lockEC)));
          END;
        END;
      | Action.ChangeOwner =>
        FOR xpac := 0 TO LAST(packages^) DO
          TRY
            ChangeOwner(packages^[xpac], siteParam);
          EXCEPT
          | PkgErr.E(lockEC) =>
              PutText(stderr, Fmt.F("Error on package %s: %s\n",
                      PrintPackage(packages^[xpac]), PkgErr.Msg(lockEC)));
          END;
        END;
      | Action.Show =>
        IF packages = NIL THEN
          dirs := LockOps.EnumerateDirs();
          IF dirs # NIL THEN
            FOR i := 0 TO LAST(dirs^) DO
              el := LockOps.Enumerate(dirs[i], NIL, FALSE, FALSE, FALSE);
              IF el # NIL THEN
                FOR j := 0 TO LAST(el^) DO
                  PrintEntry(PN{dirs[i], el[j].arc}, el[j].e, localSite);
                END;
              END;
            END;
          END;
        ELSE
          FOR xpac := 0 TO LAST(packages^) DO
            TRY
              entry := LockOps.GetEntry(packages^[xpac]);
              PrintEntry(packages^[xpac], entry^, localSite);
            EXCEPT
            | PkgErr.E(lockEC) =>
                PutText(stderr, Fmt.F("Error on package %s: %s\n",
                        PrintPackage(packages^[xpac]), PkgErr.Msg(lockEC)));
            END;
          END;
        END;
      | Action.LocalLocks =>
        IF packages = NIL THEN
          dirs := LockOps.EnumerateDirs();
          IF dirs # NIL THEN
            PutText(stdout, "Packages managed by local site and locked\n");
            FOR i := 0 TO LAST(dirs^) DO
              el := LockOps.Enumerate(dirs[i], NIL, TRUE, FALSE, FALSE);
              IF el # NIL THEN
                FOR j := 0 TO LAST(el^) DO
                  PrintLock(PN{dirs[i], el[j].arc}, el[j].e, FALSE);
                END;
              END;
            END;
          END;
        ELSE
          FOR xpac := 0 TO LAST(packages^) DO
            TRY
              entry := LockOps.GetEntry(packages^[xpac]);
              IF entry.owner.key = NIL THEN
                IF Text.Equal(entry.managedBy, localSite) THEN
                  PutText(stdout, Fmt.F(
                      "%s is not locked\n", PrintPackage(packages^[xpac])));
                ELSE
                  PutText(stdout, Fmt.F(
    "%s: cannot tell lock status: managing site is remote (%s)\n",
                      PrintPackage(packages^[xpac]), entry.managedBy));
                END;
              ELSE
                PrintLock(packages^[xpac], entry^, TRUE);
              END;
            EXCEPT
            | PkgErr.E(lockEC) =>
                PutText(stderr, Fmt.F("Error on package %s: %s\n",
                        PrintPackage(packages^[xpac]), PkgErr.Msg(lockEC)));
            END;
          END;
        END;
      | Action.Locks =>
        FOR xpac := 0 TO LAST(packages^) DO
          TRY
            entry := LockOps.GetEntry(packages^[xpac], TRUE);
            IF entry.owner.key = NIL THEN
              PutText(stdout,
                 Fmt.F("%s is not locked\n", PrintPackage(packages^[xpac])));
            ELSE
              PrintLock(packages^[xpac], entry^, TRUE);
            END;
          EXCEPT
          | PkgErr.E(lockEC) =>
              PutText(stderr, Fmt.F("Error on package %s: %s\n",
                      PrintPackage(packages^[xpac]), PkgErr.Msg(lockEC)));
          END;
        END;
          (*****)
      END;
      RETURN TRUE;
    EXCEPT
    | LockOps.CommitFailed(cf) =>
        FOR i := 0 TO LAST(cf^) DO
          IF cf[i] # NIL THEN
            PutText(stderr,
               Fmt.F("Commit failure at %s: %s\n",
                    Fmt.Int(i), PkgErr.Msg(cf[i])));
          END;
        END;
    | NetObj.Error(ec) =>
        PutText(stderr, PkgErr.Msg(ec) & "\n");
    | PkgErr.E(ec) =>
        PutText(stderr, PkgErr.Msg(ec) & "\n");
    END;
    RETURN FALSE;
  END Run;

PROCEDURE GetFieldValueParam(arg: Text.T; VAR refVal: REFANY): Field =
  VAR
    f: Field;
    fieldText, valText: Text.T;
    i: INTEGER;
  BEGIN
    i := Text.FindChar(arg, '=');
    IF i < 0 THEN Usage(); END;
    fieldText := Text.Sub(arg, 0, i);
    valText   := Text.Sub(arg, i+1, LAST(CARDINAL));
    IF Text.Equal(fieldText, "managedBy") THEN
      f      := Field.ManagedBy;
      refVal := valText;
    ELSIF Text.Equal(fieldText, "ownerKey") THEN
      f      := Field.OwnerKey;
      refVal := valText;
    ELSIF Text.Equal(fieldText, "ownerSite") THEN
      f      := Field.OwnerSite;
      refVal := valText;
    ELSIF Text.Equal(fieldText, "instance") THEN
      f      := Field.Instance;
      refVal := GetIntParam(valText);
    ELSIF Text.Equal(fieldText, "CV") THEN
      f      := Field.CV;
      refVal := GetIntParam(valText);
    ELSIF Text.Equal(fieldText, "LV") THEN
      f      := Field.LV;
      refVal := GetIntParam(valText);
    ELSIF Text.Equal(fieldText, "PV") THEN
      f      := Field.PV;
      refVal := GetIntParam(valText);
    ELSE
      PutText(stderr, "Bad field name, possible field names are:\n");
      PutText(
        stderr,
        "   managedBy, ownerKey, ownerSite, inst, CV, LV, PV\n");
      Process.Exit(1);
    END;
    RETURN f;
  END GetFieldValueParam;

PROCEDURE GetIntParam(t: Text.T): REF INTEGER =
  VAR
    rd: Rd.T;
    i: INTEGER;
    ri: REF INTEGER;
    <* FATAL Rd.Failure *>
  BEGIN
    IF t # NIL AND Text.Equal(t, "DeletedVN") THEN
      i := LockOps.DeletedVN;
    ELSE
      TRY
        rd := TextRd.New(t);
        i := Lex.Int(rd);
      EXCEPT
      | Lex.Error, FloatMode.Trap =>
          PutText(stderr, "Bad integer field value\n");
          Process.Exit(1);
      END;
    END;
    ri := NEW(REF INTEGER);
    ri^ := i;
    RETURN ri;
  END GetIntParam;

(*
PROCEDURE GetBoolParam(t: Text.T): REFANY =
  VAR
    b: BOOLEAN;
    bool: REF BOOLEAN;
  BEGIN
    IF t # NIL AND Text.Equal(t, "TRUE") THEN
      b := TRUE;
    ELSIF t # NIL AND Text.Equal(t, "FALSE") THEN
      b := FALSE;
    ELSE
      PutText(stderr, "Bad boolean field value\n");
      Process.Exit(1);
    END;
    bool := NEW(REF BOOLEAN);
    bool^ := b;
    RETURN bool;
  END GetBoolParam;
*)

PROCEDURE GetReplicaList(t: Text.T): REF ARRAY OF TEXT =
  VAR
    ra:       REF ARRAY OF TEXT;
    end, pos: INTEGER;
    arr:      ARRAY [0..9] OF Text.T;
    i:        CARDINAL;
  CONST
    SearchPattern = "replicas";
  BEGIN
    i  := 0;
    IF t = NIL THEN RETURN NIL; END;
    pos := Text.FindChar(t, '=');
    IF pos <= 0 OR NOT Text.Equal(SearchPattern, Text.Sub(t, 0, pos)) THEN
      RETURN NIL;
    END;
    INC(pos);
    LOOP
      end := Text.FindChar(t, '+', pos);
      IF end < 0 THEN
        arr[i] := Text.Sub(t, pos, LAST(CARDINAL));
        INC(i);
        EXIT;
      ELSE
        IF (end # pos) THEN
          arr[i] := Text.Sub(t, pos, (end - pos));
          INC(i);
        END;
        pos := end + 1;
      END;
    END;
    ra := NEW(REF ARRAY OF TEXT, i);
    REPEAT
      DEC(i);
      ra^[i] := arr[i];
    UNTIL (i = 0);
    RETURN ra;
  END GetReplicaList;

PROCEDURE Date(time: Time.T): Text.T =
  BEGIN
    RETURN FmtTime.Long(time);
  END Date;

PROCEDURE PrintEntry(pkg: PN; READONLY entry: LockOps.Entry;
                     localSite: Text.T) =
  VAR locked := "";
  BEGIN
    IF verbose THEN
      PrintEntryVerbose(pkg, entry);
      RETURN;
    END;
    IF entry.owner.key # NIL THEN locked := "(locked)"; END;
    PutText(stdout, Fmt.F("%s: %s%s  cv=%s", PrintPackage(pkg),
              entry.managedBy, locked, Fmt.Int(entry.curVN)));
    IF (entry.curVN # entry.lastVN) 
       AND (Text.Equal(localSite, entry.managedBy)) THEN
      PutText(stdout, Fmt.F(" lv=%s", Fmt.Int(entry.lastVN)));
    END;
    IF entry.pendVN # LockOps.NullVN THEN
      PutText(stdout, Fmt.F(" pv=%s", Fmt.Int(entry.pendVN)));
    END;
    PutText(stdout, Fmt.F("  %s\n", Date(entry.lastModified)));
  END PrintEntry;

PROCEDURE PrintEntryVerbose(pkg: PN; READONLY e: LockOps.Entry) =
  BEGIN
    PutText(stdout, PrintPackage(pkg) & "\n");
    IF e.owner.key # NIL THEN
      PutText(stdout,
          Fmt.F("  Locked: %s (%s)\n", e.owner.key, e.owner.site));
    END;
    PutText(stdout,
         Fmt.F("  Managed by: %s\n", e.managedBy));
    PutText(stdout,
         Fmt.F("  Instance=%s; lv=%s; cv=%s; pv=%s\n",
                Fmt.Int(e.instance), Fmt.Int(e.lastVN),
                Fmt.Int(e.curVN), Fmt.Int(e.pendVN)));
    PutText(stdout, "  Last modified: " & Date(e.lastModified) & "\n");
  END PrintEntryVerbose;

PROCEDURE PrintLock(pkg: PN; READONLY entry: LockOps.Entry; one: BOOLEAN) =
  VAR m: Text.T;
  BEGIN
    IF verbose THEN
      PrintEntryVerbose(pkg, entry);
    ELSE
      IF one THEN m := "checked out to" ELSE m := "->"; END;
      PutText(stdout,
           Fmt.F("%s %s %s (%s)\n", PrintPackage(pkg), m,
                entry.owner.key, entry.owner.site));
    END;
  END PrintLock;
  
PROCEDURE ChangeEntry(which: PN; field: Field; val: REFANY)
    RAISES {PkgErr.E} =
  VAR
    e:     LockOps.RefEntry;
  BEGIN
    e := LockOps.GetEntry(which);
    CASE field OF
    | Field.ManagedBy =>
      e.managedBy := NARROW(val, Text.T);
    | Field.OwnerKey =>
      e.owner.key := NARROW(val, Text.T);
    | Field.OwnerSite =>
      e.owner.site := NARROW(val, Text.T);
    | Field.CV =>
      e.curVN    := NARROW(val, REF INTEGER)^;
    | Field.LV =>
      e.lastVN     := NARROW(val, REF INTEGER)^;
    | Field.PV =>
      e.pendVN     := NARROW(val, REF INTEGER)^;
    | Field.Instance =>
      e.instance       := NARROW(val, REF INTEGER)^;
    END;
    LockOps.SetEntry(myAuth, which, e);
  END ChangeEntry;

PROCEDURE ChangeOwner(which: PN; site: TEXT)
    RAISES {PkgErr.E} =
  VAR
    e:     LockOps.RefEntry;
  BEGIN
    e := LockOps.GetEntry(which);
    e.managedBy := site;
    e.lastVN := e.curVN;
    LockOps.SetEntry(myAuth, which, e);
  END ChangeOwner;

PROCEDURE GetPackage(t: Text.T): PN =
  VAR pn: PN;
  BEGIN
    TRY
      pn := NetPath.PNFromText(t);
    EXCEPT
    | NetPath.Invalid =>
        PutText(stderr, "Invalid package name\n");
        Process.Exit(1);
    END;
    IF pn.dir = NIL THEN pn.dir := defaultDir; END;
    RETURN pn;
  END GetPackage;

PROCEDURE GetDir(t: Text.T): NetPath.T =
  VAR dir: NetPath.T;
  BEGIN
    TRY
      dir := NetPath.FromText(t);
    EXCEPT
    | NetPath.Invalid =>
        PutText(stderr, "Invalid directory name\n");
        Process.Exit(1);
    END;
    RETURN dir;
  END GetDir;

PROCEDURE PrintPackage(pn: PN): Text.T =
  BEGIN
    RETURN NetPath.PNToText(pn);
  END PrintPackage;

PROCEDURE PathToText(path: NetPath.T) : TEXT =
  BEGIN
    RETURN NetPath.ToText(path);
  END PathToText;

PROCEDURE CheckServers(
         wrArg:      Wr.T;
         verbose:    BOOLEAN;
         pkg: PN;
READONLY entry:      LockOps.Entry;
         replicas:   REF ARRAY OF TEXT;
         bindings:   ReplicaArray) =
  VAR
    finalTxt:  Text.T;
    msg:     Text.T;
    version: Version;
    wr:      Wr.T;
  BEGIN
    wr := TextWr.New();
    FOR i := 0 TO LAST(replicas^) DO
      msg := NIL;
      IF bindings^[i] # NIL THEN
        TRY
          version := bindings[i].version(pkg);
          IF (version.t # entry.instance) OR
             (version.vn # entry.curVN)
          THEN
            PutText(
              wr, Fmt.F(" %s:  %s.%s;", replicas^[i],
                 Fmt.Int(version.t), Fmt.Int(version.vn)));
          ELSE
            IF verbose THEN msg := "ok"; END;
          END;
        EXCEPT
        | NetObj.Error =>
            bindings^[i] := NIL;
        | PkgErr.E(mgrEC) =>
            IF mgrEC.head = PkgErr.NoSuchPackage THEN
              msg := "no such package";
            ELSIF mgrEC.head = PkgErr.NoSuchDir THEN
              IF verbose THEN
                msg := "no repository";
              ELSE
                msg := NIL;
              END;
            ELSE
              msg := Text.Cat("package server error: ", PkgErr.Msg(mgrEC));
            END;
        END;
      END;
      IF bindings^[i] = NIL THEN
        IF verbose THEN msg := "not reachable"; ELSE msg := NIL; END;
      END;
      IF msg # NIL THEN
        PutText(wr, Fmt.F(" %s: %s;", replicas^[i], msg));
      END;
    END;

    finalTxt := TextWr.ToText(wr);
    IF NOT Text.Empty(finalTxt) THEN
      PutText(wrArg, PrintPackage(pkg));
      PutText(wrArg, Fmt.F(" (%s.%s)",
                Fmt.Int(entry.instance), Fmt.Int(entry.curVN)));
      PutText(wrArg, Fmt.F("  %s\n", finalTxt));
    END;
  END CheckServers;

PROCEDURE GetReplicas(replicas: REF ARRAY OF TEXT
  ): ReplicaArray (* non-NIL if at least one replica *) =
  VAR
    bindings: ReplicaArray;
    ok: BOOLEAN := FALSE;
  BEGIN
    bindings := NEW(ReplicaArray, NUMBER(replicas^));
    FOR i := 0 TO LAST(replicas^) DO
      TRY
        bindings^[i] := PackageObj.New(replicas^[i]);
        ok := TRUE;
      EXCEPT
      | NetObj.Error, PkgErr.E =>
          bindings[i] := NIL;
      END;
    END;
    IF NOT ok THEN RETURN NIL; END;
    RETURN bindings;
  END GetReplicas;

PROCEDURE SynchKindMsg(synchKind: Siphon.SynchKind): Text.T =
  VAR
    m: Text.T;
  BEGIN
    CASE synchKind OF
    | Siphon.SynchKind.CheckOnly =>
      m := "check only";
    | Siphon.SynchKind.UpdateSelf =>
      m := "update self";
    | Siphon.SynchKind.UpdateAll =>
      m := "update all";
    END;
    RETURN m;
  END SynchKindMsg;

PROCEDURE TextFromList(l: REF ARRAY OF TEXT): Text.T =
  VAR
    wr: Wr.T;
    <* FATAL Wr.Failure *>
  BEGIN
    IF (l = NIL) OR (NUMBER(l^) = 0) THEN RETURN NIL; END;
    wr := TextWr.New();
    FOR i := 0 TO LAST(l^) DO
      IF i # 0 THEN Wr.PutChar(wr, '+'); END;
      Wr.PutText(wr, l^[i]);
    END;
    RETURN TextWr.ToText(wr);
  END TextFromList;

PROCEDURE CheckReplicaDirs(
  wr:       Wr.T;
  dir:      Dir;
  replicas: REF ARRAY OF TEXT;
  bindings: ReplicaArray) =
  VAR
    nok: CARDINAL;
    array:  REF ARRAY OF BOOLEAN;
    first:  BOOLEAN;
  BEGIN
    array := NEW(REF ARRAY OF BOOLEAN, NUMBER(replicas^));
    nok := 0;
    FOR i := 0 TO LAST(replicas^) DO
      array^[i] := FALSE;
      IF bindings[i] # NIL THEN
        TRY
          bindings[i].checkDir(dir, NIL);
          array^[i] := TRUE;
          INC(nok);
        EXCEPT
        | PkgErr.E =>
        | NetObj.Error =>
            bindings[i] := NIL;
        END;
      END;
    END;
    PutText(wr, PathToText(dir) & " ");
    IF nok = 0 THEN
      PutText(wr, "unknown at all replicas");
    ELSIF nok = NUMBER(replicas^) THEN
      PutText(wr, "exists at all replicas");
    ELSE
      first := TRUE;
      FOR i := 0 TO LAST(replicas^) DO
        IF array^[i] THEN
          IF first THEN
            first := FALSE;
            PutText(wr, "at replicas ");
          ELSE
            PutText(wr, "+");
          END;
          PutText(wr, replicas^[i]);
        END;
      END;
    END;
    PutText(wr, "\n");
  END CheckReplicaDirs;

PROCEDURE PutText(wr: Wr.T; t: TEXT; flush: BOOLEAN := FALSE) =
  BEGIN
    TRY
      Wr.PutText(wr, t);
      IF flush THEN Wr.Flush(wr); END;
    EXCEPT
    | Wr.Failure => Process.Exit(1);
    END;
  END PutText;

BEGIN
  IF NOT Run() THEN Process.Exit(1); END;
END PkgQTool.
