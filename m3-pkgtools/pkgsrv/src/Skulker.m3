(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Skulker.m3 *)
(* Last modified on Thu Feb  2 09:01:25 PST 1995 by kalsow  *)
(*      modified on Thu Apr 28 18:17:21 PDT 1994 by wobber  *)
(*      modified on Wed Jul  1 20:13:13 GMT+2:00 1992 by prusker *)

MODULE Skulker;

IMPORT Text, Util, LockOps, Thread, Time, Wr,
  Site, Send, Subrepo, TextWr, Fmt, FmtTime, Param,  Siphon, NetObj,
  PkgErr, PackageLib, PSLib, Fingerprint;

FROM LockOps IMPORT SiteName, Version, InitialVN, DeletedVN;
FROM PackageObj IMPORT PN, Dir;
FROM Siphon IMPORT SynchKind;

TYPE
  Kind = {Ignored, NotReachable, Deleted, NoDir, Shared};
                (* order important *)
  Typ = {Ignored, NotReachable, Absent, Present};
  State = {Ignored, OutOfDate, UpToDate};
  Host = REF RECORD
    sitename:   SiteName;
    typ: Typ := Typ.Present;
    entry:  LockOps.Entry; (* meaningful iff Typ.Present *)
    kind: Kind := Kind.Ignored; (* used in SynchOnePackage *)
    state: State := State.Ignored; (* used in SynchOnePackage *)
    siphonT: Siphon.T := NIL;
    enum: LockOps.EnumList := NIL;
    pos: CARDINAL := 0;
  END;
  HostArray = REF ARRAY OF Host;

  Closure = REF RECORD
        (* fixed *)
      hostArray: HostArray;
      synchKind: SynchKind;
      localHost: Host; (* hostArray[0] *)
  END;

VAR
  synchMutex: Thread.Mutex;
  synching:   BOOLEAN;
  synchKindMsg := ARRAY SynchKind OF TEXT {
    "check only", "update local site", "update all"};

PROCEDURE Synch(
  synchKind: SynchKind;
  package:  PN; (* all packages if nil*)): Text.T
  RAISES {Thread.Alerted}  =
  (* writes actions to logfile (except if check only), prefixed by Skulk *)
  (* text returned includes everything, actions and checks *)
  VAR wr: Wr.T;
  BEGIN
    wr := TextWr.New();
    IF package.dir = NIL THEN
      SynchAllPackages(synchKind, wr);
    ELSE
      SynchPackage(synchKind, wr, package);
    END;
    RETURN TextWr.ToText(wr);
  END Synch;

PROCEDURE InitClosure(synchKind: SynchKind) : Closure =
  VAR i: CARDINAL;
      c := NEW(Closure,
            synchKind := synchKind,
            hostArray := NIL,
            localHost := NEW(Host, sitename := Param.localSite));
  VAR remotes: REF ARRAY OF Site.Remote;
  BEGIN
    remotes := Site.Get().foreignSites;
    IF remotes = NIL THEN RETURN NIL; END;
    c.hostArray := NEW(HostArray, NUMBER(remotes^)+1);
    c.hostArray[0] := c.localHost;
    i := 1;
    FOR j := 0 TO LAST(remotes^) DO
      c.hostArray[i] := NEW(Host, sitename := remotes^[j].name);
      INC(i);
    END;
    RETURN c;
  END InitClosure;
  
PROCEDURE FindSite(c: Closure; host: TEXT): Host =
  BEGIN
    FOR i := 0 TO LAST(c.hostArray^) DO
      IF Text.Equal(host, c.hostArray[i].sitename) THEN
        RETURN c.hostArray[i];
      END;
    END;
    RETURN NIL;
  END FindSite;

PROCEDURE SynchAllPackages(synchKind: SynchKind; wr: Wr.T)
              RAISES {Thread.Alerted} =
  (* writes actions to logfile, prefixed by Skulk *)
  (* if wr not NIL, wr includes everything, actions and checks *)
  VAR
    nbNotReachable, nbPresent: CARDINAL := 0;
    dirs: LockOps.DirList;
    c: Closure := InitClosure(synchKind);
    h: Host;
  PROCEDURE OutPut(str: Text.T) RAISES {Thread.Alerted} =
      <* FATAL Wr.Failure *>
    BEGIN
      IF synchKind # SynchKind.CheckOnly THEN
        Util.LogText("SK: " & str);
      END;
      IF wr # NIL THEN Wr.PutText(wr, str); END;
    END OutPut;

  BEGIN
    IF c = NIL THEN RETURN; END;
    OutPut("skulk (" & synchKindMsg[synchKind] & ") of all packages\n");
    IF synchKind # SynchKind.CheckOnly THEN
      LOCK synchMutex DO
        IF synching THEN
          OutPut("synch sites already going on\n");
          RETURN;
        END;
        synching := TRUE;
      END;
    END;

    TRY
      TRY
        dirs := LockOps.EnumerateDirs();
        IF dirs = NIL THEN RETURN; END;
      EXCEPT
      | PkgErr.E(ec) =>
           OutPut("problem with lock server: " & PkgErr.Msg(ec) & "\n");
           RETURN;
      END;

      OutPut("calling remote sites\n");
      FOR i := 0 TO LAST(c.hostArray^) DO
        Subrepo.Query(c.hostArray[i].sitename);
      END;
      FOR idir := 0 TO LAST(dirs^) DO
        nbPresent := 0;
        nbNotReachable := 0;
        FOR s := 0 TO LAST(c.hostArray^) DO
          h := c.hostArray[s];
          IF h.typ = Typ.NotReachable THEN
            INC(nbNotReachable);
          ELSE
            TRY
              CASE Subrepo.HasDir(h.sitename, dirs[idir]) OF
              | Subrepo.R.No => h.typ := Typ.Absent;
              ELSE
                  h.pos := 0;
                  h.enum := LockOps.Enumerate(
                    dirs[idir], h.sitename, FALSE, FALSE, FALSE);
                  h.typ := Typ.Present;
                  INC(nbPresent);
              END;
            EXCEPT
            | PkgErr.E(ec) =>
                IF ec.head # PkgErr.NoSuchDir THEN
                  OutPut(h.sitename & ": " & PkgErr.Msg(ec) & "\n");
                  INC(nbNotReachable);
                  h.typ := Typ.NotReachable;
                END;
            END;
          END;
        END;
        IF nbPresent >= 2 THEN
          IF nbNotReachable = 0 THEN
            OutPut("starting synch: " &
                                      PSLib.PathText(dirs[idir]) & "\n");
          ELSE
            OutPut("starting (partial) synch: " &
                                      PSLib.PathText(dirs[idir]) & "\n");
          END;
          SynchOneDir(c, dirs[idir], OutPut);
        ELSE
          IF nbNotReachable # 0 THEN
            OutPut("can't synch: " & PSLib.PathText(dirs[idir]) & "\n");
          END;
        END;
      END;
      OutPut("end\n");
    FINALLY
      IF c.synchKind # SynchKind.CheckOnly THEN
        LOCK synchMutex DO synching := FALSE; END;
      END;
    END;
  END SynchAllPackages;

PROCEDURE SynchOneDir(c: Closure; dir: Dir;
                 print: PROCEDURE(t: TEXT) RAISES {Thread.Alerted})
       RAISES {Thread.Alerted} =
  VAR
    pn, txt: TEXT;
    h: Host;
  BEGIN
    LOOP
      pn := NIL;
      FOR i := 0 TO LAST(c.hostArray^) DO
        h := c.hostArray[i];
        IF h.typ > Typ.NotReachable THEN
          h.typ := Typ.Absent;
          IF h.enum # NIL AND h.pos <= LAST(h.enum^) THEN
            IF pn = NIL OR Text.Compare(pn, h.enum[h.pos].arc) # -1
                               THEN
              pn := h.enum[h.pos].arc;
              h.typ := Typ.Present;
            END;
          END;
        END;
      END;
      IF pn = NIL THEN EXIT; END;
      FOR i := 0 TO LAST(c.hostArray^) DO
        h := c.hostArray[i];
        IF h.typ = Typ.Present THEN
          IF Text.Equal(pn, h.enum[h.pos].arc) THEN
            h.entry := h.enum[h.pos].e;
            INC(h.pos);
          ELSE
            h.typ := Typ.Absent;
          END;
        END;
      END;
      txt := SynchOnePackage(c, PN{dir, pn});
      IF Text.Length(txt) # 0 THEN print(txt); END;
    END;
  END SynchOneDir;

PROCEDURE SynchPackage(
              synchKind: SynchKind;
              wr: Wr.T;
              package: PN) RAISES {Thread.Alerted} =
  VAR
    h: Host;
    nbPresent: CARDINAL := 0;
    nbNotReachable: CARDINAL := 0;
    c: Closure := InitClosure(synchKind);
    lockT: LockOps.T;
    err: PkgErr.TL;
    out: TEXT;

  PROCEDURE OutPut(str: Text.T) RAISES {Thread.Alerted} =
      <* FATAL Wr.Failure *>
    BEGIN
      IF synchKind # SynchKind.CheckOnly THEN
        Util.LogText("SK: " & str);
      END;
      IF wr # NIL THEN Wr.PutText(wr, str); END;
    END OutPut;

  BEGIN
    IF c = NIL THEN RETURN; END;
    OutPut("skulk (" & synchKindMsg[synchKind] & ") of " &
                      PSLib.PkgText(package) & "\n");
    FOR xsite := 0 TO LAST(c.hostArray^) DO
      h := c.hostArray[xsite];
      IF h.typ = Typ.Present THEN
        TRY
          err := NIL;
          TRY
            IF xsite = 0 THEN
              lockT := LockOps.New();
            ELSE
              h.siphonT := Siphon.New(h.sitename);
              lockT := h.siphonT.lockserver();
            END;
            h.entry := LockOps.GetEntry(package, FALSE, lockT)^;
            h.typ := Typ.Present;
            INC(nbPresent);
          EXCEPT
          | PkgErr.E(lockEC) =>
            IF lockEC.head = PkgErr.NoSuchDir THEN
              h.typ := Typ.NotReachable;
              Subrepo.Query(h.sitename);
            ELSIF lockEC.head = PkgErr.NoSuchPackage THEN
              h.typ := Typ.Absent;
              INC(nbPresent);
            ELSE
              RAISE PkgErr.E(lockEC);
            END;
          END;
        EXCEPT
        | PkgErr.E(ec) => err := ec;
        | NetObj.Error(ec) => err := ec;
        END;
        IF err # NIL THEN
          OutPut(h.sitename & ": " & PkgErr.Msg(err) & "\n");
          h.typ := Typ.NotReachable;
          INC(nbNotReachable);
        END;
      END;
    END;

    IF nbPresent >= 2 THEN
      out := SynchOnePackage(c, package);
      IF out # NIL THEN OutPut(out); END;
    ELSE
      IF nbNotReachable # 0 THEN
        OutPut("can't synch: " & PSLib.PkgText(package) & "\n");
      END;
    END;
    OutPut("end\n");
  END SynchPackage;

EXCEPTION
  Error( Text.T );
  Finished( Text.T );

PROCEDURE SynchOnePackage(c: Closure; package: PN): Text.T
   RAISES {Thread.Alerted} =
  VAR
        (* state vars *)
    nbDeleted, nbShared, nbNoDir, nbNotReachable: CARDINAL;
    maxPendVN: LockOps.VN;
    managerHost: Host;
    printAll, fprinterror: BOOLEAN;
    bestVers: Version;
    timeBestVersion: Time.T;
    managedBy: Text.T;
    deleted: BOOLEAN;
    toSynch, localOutOfDate: BOOLEAN;
    wr: Wr.T := NIL;
        (* working vars *)
    curFpValid: BOOLEAN;
    curFpVN: LockOps.VN;
    curFp:   Fingerprint.T;
    host: Host;
    m: Text.T;
    elapsed: Time.T;
    first: BOOLEAN;
        <* FATAL Wr.Failure *>
    
  PROCEDURE NewWriter(print: BOOLEAN := TRUE) RAISES {Thread.Alerted} =
    BEGIN
      IF wr = NIL THEN
        wr := TextWr.New();
        Wr.PutText(wr, PSLib.PkgText(package) & ": ");
      ELSE
        Wr.PutText(wr, "   ");
      END;
      printAll := printAll OR print;
    END NewWriter;

  PROCEDURE PrintEntries(nbKind: CARDINAL; kind: Kind; txt: Text.T)
      RAISES {Thread.Alerted}  =
    VAR
      nb: CARDINAL;
    BEGIN
      IF nbKind # 0 THEN
        nb := nbKind;
        Wr.PutText(wr, Fmt.F( " %s %s (", Fmt.Int(nbKind), txt));
        FOR xsite := 0 TO LAST(c.hostArray^) DO
          WITH h = c.hostArray[xsite] DO
            IF h.kind = kind THEN
              Wr.PutText(wr, h.sitename);
              DEC(nb);
              IF nb # 0 THEN Wr.PutText(wr, ", "); END;
            END;
          END;
        END;
        Wr.PutText(wr, "),");
      END;
    END PrintEntries;

  BEGIN
      (* do some work on local host, even if it is not shared *)
    printAll := FALSE;
    IF c.localHost.typ = Typ.Present THEN
      WITH e = c.localHost.entry DO
            (* check if not fprinted for a long time *)
          IF e.fp = Fingerprint.Zero THEN
            elapsed := Time.Now() - e.lastModified;
            IF ROUND(elapsed) > 200000 THEN
              NewWriter(FALSE);
              Wr.PutText(wr, "no local fprint for ");
              Wr.PutText(wr, Util.Interval(elapsed));
              Wr.PutChar(wr, '\n');
            END;
          END;
(*
        TRY
            (* check local package if not shared *)
          IF NOT e.shared THEN
              (* do some checking on a local package *)
            IF e.curVN < InitialVN OR e.curVN >= DeletedVN THEN
              RAISE Error("very bad version numbers");
            END;
            IF NOT Text.Equal(e.managedBy, Param.localSite) THEN
              RAISE Error("manager is remote");
            END;
              (* check versions numbers *)
            IF e.curVN > e.lastVN OR e.pendVN > e.lastVN THEN
              RAISE Error("bad version numbers");
            END;
            IF e.owner.key = NIL THEN
              IF e.lastVN # e.curVN THEN
                RAISE Error("not locked but lastVN # curVN");
              END;
            ELSE
                (* package locked *)
              IF NOT Text.Equal(e.owner.site, Param.localSite) THEN
                RAISE Error("locking site is remote??");
              END;
              IF e.lastVN > e.curVN THEN
                elapsed := Time.Now() - e.lastModified;
                IF ROUND(elapsed) > 600 THEN
                  NewWriter();
                  Wr.PutText(wr,
                    "pending update to not shared package for ");
                  Wr.PutText(wr, Util.Interval(elapsed));
                  Wr.PutChar(wr, '\n');
                END;
              END;
            END;
          END;
        EXCEPT
        | Error(msg) =>
          NewWriter();
          Wr.PutText(wr, "error on not shared package\n");
          Wr.PutText(wr, Fmt.F("   %s\n", msg));
        END;
*)
        IF printAll THEN
          Wr.PutText(wr, Fmt.F("   mgr=%s, vn=%s.%s", 
              e.managedBy, Fmt.Int(e.instance), Fmt.Int(e.curVN)));
          IF e.lastVN # e.curVN THEN 
            Wr.PutText(wr, Fmt.F(" (LV=%s)", Fmt.Int(e.lastVN))); 
          END;
          IF e.owner.key # NIL THEN 
            Wr.PutText(wr, Fmt.F(" [%s]", e.owner.site)); 
          END;
          Wr.PutText(wr, Fmt.F(", %s\n", FmtTime.Short(e.lastModified)));
        END;
      END;
    END;

      (* synchronize *)
    TRY
      printAll      := FALSE;
      fprinterror   := FALSE;
      nbDeleted     := 0;
      nbShared      := 0;
      nbNotReachable  := 0;
      nbNoDir       := 0;

        (* sorts and counts the different kind of entries *)

        (* subrep can be out of date, i.e. subrep[xsite] = FALSE for a
           package present; but it is not too bad since we use subrep
           mainly for Typ.Absent and Typ.NotReachable entries *)
      FOR xsite := 0 TO LAST(c.hostArray^) DO
        WITH h = c.hostArray[xsite] DO
          CASE h.typ OF
          | Typ.Present =>
              h.kind := Kind.Shared;
              INC(nbShared);
          | Typ.Absent =>
              CASE Subrepo.Has(h.sitename, package) OF
              | Subrepo.R.Yes =>
                  h.kind := Kind.Deleted;
                  INC(nbDeleted);
              | Subrepo.R.No =>
                  h.kind := Kind.NoDir;
                  INC(nbNoDir);
              ELSE
                <* ASSERT(FALSE) *>
              END;
          | Typ.NotReachable =>
              h.kind := Kind.NotReachable;
              INC(nbNotReachable);
          | Typ.Ignored =>
              h.kind := Kind.Ignored;
          END;
        END;
      END;

        (* no shared package at all *)
      IF nbShared = 0 THEN
        IF nbNotReachable # 0 THEN m := "unreachable"; ELSE m := "no"; END;
        RAISE Finished(m & " shared package in skulked sites");
      END;

        (* prints missing entries *)
      IF nbDeleted # 0 THEN
        NewWriter(FALSE);
        Wr.PutText(wr, "\n  ");
        PrintEntries(nbShared, Kind.Shared, "shared");
        PrintEntries(nbDeleted, Kind.Deleted, "deleted");
        PrintEntries(nbNoDir, Kind.NoDir, "no sub-repository");
        PrintEntries(nbNotReachable, Kind.NotReachable, "not reachable");
        Wr.PutText(wr, "\n");
      END;

        (* checks for synchronisation *)
      curFpValid := FALSE;
      first      := TRUE;
      FOR xsite := 0 TO LAST(c.hostArray^) DO
        host := c.hostArray[xsite];
        IF host.kind = Kind.Shared THEN
          WITH e = host.entry DO
            IF e.curVN < InitialVN OR e.curVN >= DeletedVN THEN
              RAISE Error("very bad version numbers");
            END;
            IF first THEN
              first           := FALSE;
              bestVers.t      := e.instance;
              managedBy       := e.managedBy;
              bestVers.vn     := e.curVN;
              maxPendVN       := e.pendVN;
              timeBestVersion := e.lastModified;
              managerHost := FindSite(c, managedBy);
            ELSE
              IF e.instance # bestVers.t THEN
                RAISE Error("instance mismatch");
              END;
              IF NOT Text.Equal(managedBy, e.managedBy) THEN 
                RAISE Error("manager mismatch"); 
              END;
              maxPendVN := MAX(maxPendVN, e.pendVN);
              IF e.curVN > bestVers.vn THEN
                bestVers.vn     := e.curVN;
                timeBestVersion := MIN(timeBestVersion, e.lastModified);
              END;
            END;
            IF e.fp # Fingerprint.Zero THEN
              IF curFpValid AND e.curVN = curFpVN AND (e.fp # curFp)
              THEN
                NewWriter(FALSE);
                fprinterror := TRUE;
                Wr.PutText(wr, "fingerprint mismatch\n");
              ELSIF NOT curFpValid OR e.curVN > curFpVN THEN
                curFpValid := TRUE;
                curFp      := e.fp;
                curFpVN    := e.curVN;
              END;
            END;
          END;
        END;
      END; (* for *)

        (* check manager *)
      IF managerHost = NIL OR managerHost.kind = Kind.Ignored THEN
        NewWriter();
        Wr.PutText(wr, Fmt.F("manager host %s unknown\n", managedBy));
      ELSE
        CASE managerHost.kind OF
        | Kind.Ignored =>
        | Kind.NotReachable =>
            (* manager host not reachable: do nothing *)
            RAISE Finished("managing site not reachable");
        | Kind.Deleted, Kind.NoDir =>
            (* manager deleted *)
            bestVers.vn := DeletedVN;
        | Kind.Shared =>
          WITH e = managerHost.entry DO
              (* check versions numbers *)
            IF (bestVers.vn > e.lastVN) OR (maxPendVN > e.lastVN) THEN
              RAISE Error("bad version numbers at manager");
            END;
            IF e.lastVN > bestVers.vn THEN
              elapsed := Time.Now() - e.lastModified;
              IF ROUND(elapsed) > 600 THEN
                NewWriter();
                Wr.PutText(wr, 
                    Fmt.F( "pending update at managing site %s for ", 
                        e.owner.site));
                Wr.PutText(wr, Util.Interval(elapsed));
                Wr.PutChar(wr, '\n');
              END;
            END;
            IF e.owner.key # NIL THEN
                (* package locked *)
              WITH host = FindSite(c, e.owner.site) DO
                IF host # NIL THEN
                  CASE host.kind OF
                  | Kind.NotReachable, Kind.Ignored =>
                  | Kind.Deleted, Kind.NoDir =>
                      NewWriter();
                      Wr.PutText(wr, "package deleted at locking site?\n");
                  | Kind.Shared =>
                      IF host.entry.curVN # bestVers.vn THEN
                        NewWriter();
                        Wr.PutText(wr,
                           "locking site doesn't have best version\n");
                      END;
                  END;
                END;
              END;
            END;
          END;
        END;
      END;
      deleted := bestVers.vn = DeletedVN;

        (* init h.update *)
      toSynch := FALSE;
      FOR xsite := 0 TO LAST(c.hostArray^) DO
        WITH h = c.hostArray[xsite] DO
          CASE h.kind OF
          | Kind.NotReachable, Kind.NoDir, Kind.Ignored =>
              h.state := State.Ignored;
          | Kind.Shared =>
              IF deleted OR bestVers.vn > h.entry.curVN THEN
                h.state := State.OutOfDate;
                toSynch := TRUE;
              ELSE
                h.state := State.UpToDate;
              END;
          | Kind.Deleted =>
              IF deleted THEN
                h.state := State.Ignored; 
              ELSE 
                h.state := State.OutOfDate;
                toSynch := TRUE;
              END;
          END;
        END;
      END;
      localOutOfDate  := (c.localHost.state = State.OutOfDate);

        (* synchronize versions *)

      IF toSynch THEN

          (* print what should be done *)
          
        NewWriter(FALSE);
        IF deleted THEN
          Wr.PutText(wr, "should delete package at: ");
        ELSE
          Wr.PutText(wr,
            Fmt.F("best=%s:", Fmt.Int(bestVers.vn)));
        END;
        first := TRUE;
        FOR xsite := 0 TO LAST(c.hostArray^) DO
          WITH h = c.hostArray[xsite] DO
            IF h.state = State.OutOfDate THEN
              IF first THEN first := FALSE; ELSE Wr.PutText(wr, ","); END;
              Wr.PutText(wr, " " & h.sitename);
              IF h.kind = Kind.Shared THEN
                Wr.PutText(wr, Fmt.F("(%s)", Fmt.Int(h.entry.curVN)));
              ELSE (* Kind.Deleted *)
                Wr.PutText(wr, "(missing)");
              END;
            END;
          END;
        END;
        Wr.PutText(wr, "\n");

          (* do it *)
            
        IF c.synchKind = SynchKind.CheckOnly THEN
          (* do nothing *)
      
        ELSIF deleted THEN
            (* update only local host *)
            (* if local entry need to be deleted, we do it only if there is
               no package to be created at local site, in case where the
               local package to be created is a rename of a package to be
               deleted ????? [EPW] *)
          IF localOutOfDate THEN
            DeleteLocalPackage(package, bestVers, managedBy, wr);
          END;

        ELSE
          IF localOutOfDate THEN
            RequestUpdate(c, package, wr);
          END;
          CASE c.synchKind OF
          | SynchKind.CheckOnly => <* ASSERT FALSE *>
          | SynchKind.UpdateSelf =>
          | SynchKind.UpdateAll =>
              IF NOT localOutOfDate THEN
                SendUpdate(c, package, wr);
              END;
          END;
        END;
      END;

    EXCEPT
    | Error(msg) =>
        NewWriter();
        Wr.PutText(wr, msg & "\n");
    | Finished(msg) =>
        NewWriter(FALSE);
        Wr.PutText(wr, msg & "\n");
    END;

    IF printAll OR fprinterror THEN
      FOR xsite := 0 TO LAST(c.hostArray^) DO
        WITH h = c.hostArray[xsite], e = h.entry DO
          Wr.PutText(wr, Fmt.F( "   %s: ", h.sitename));
          CASE h.kind OF
          | Kind.Ignored =>
            Wr.PutText(wr, "site ignored\n");
          | Kind.NoDir =>
            Wr.PutText(wr, "no sub-repository\n");
          | Kind.Deleted =>
            Wr.PutText(wr, "deleted entry\n");
          | Kind.NotReachable =>
            CASE Subrepo.Has(h.sitename, package) OF
            | Subrepo.R.Yes =>
              Wr.PutText(wr, "not reachable (sub-repository exists)\n");
            | Subrepo.R.No =>
              Wr.PutText(wr, "not reachable (no sub-repository)\n");
            | Subrepo.R.Dontknow =>
              Wr.PutText(wr, "not reachable\n");
            END;
          | Kind.Shared =>
            Wr.PutText(wr, Fmt.F("mgr=%s, vn=%s.%s", 
              e.managedBy, Fmt.Int(e.instance), Fmt.Int(e.curVN)));
            IF printAll AND Text.Equal(e.managedBy, h.sitename) THEN
              IF e.lastVN # e.curVN THEN 
                Wr.PutText(wr, Fmt.F(" (LV=%s)", Fmt.Int(e.lastVN))); 
              END;
              IF e.owner.key # NIL THEN 
                Wr.PutText(wr, Fmt.F(" [%s]", e.owner.site)); 
              END;
            END;
            Wr.PutText(wr, Fmt.F(", %s\n", FmtTime.Short(e.lastModified)));
            IF fprinterror THEN
              IF e.fp # Fingerprint.Zero THEN
                Wr.PutText(wr, Fmt.F("      fp="));
                FOR x := 0 TO LAST(e.fp.byte) DO
                  Wr.PutText(wr, Fmt.Pad(
                      Fmt.Unsigned(e.fp.byte[x], 16), 2, '0'));
                END;
                Wr.PutChar(wr, '\n');
              ELSE
                Wr.PutText(wr, "      no fingerprint\n");
              END;
            END;
          END;
        END;
      END;
    END;
    IF wr # NIL THEN RETURN TextWr.ToText(wr); ELSE RETURN ""; END;
  END SynchOnePackage;

PROCEDURE RequestUpdate(c: Closure; pn: PN; log: Wr.T)
    RAISES {Thread.Alerted} = 
  <* FATAL Wr.Failure *>
  BEGIN
    FOR i := 1 TO LAST(c.hostArray^) DO
      WITH h = c.hostArray[i] DO
        IF h.state = State.UpToDate THEN
          Wr.PutText(log, Fmt.F("   asking %s to send version %s\n",
                h.sitename, Fmt.Int(h.entry.curVN)));
          TRY
            IF h.siphonT = NIL THEN
              h.siphonT := Siphon.New(h.sitename);
            END;
            h.siphonT.enqueue(
                pn, Version {h.entry.instance, h.entry.curVN},
                h.entry.managedBy, Param.localSite);
          EXCEPT
          | PkgErr.E(ec) =>
              Wr.PutText(log, Fmt.F("   enqueue failed at %s: %s\n",
                                    h.sitename, PkgErr.Msg(ec)));
          | NetObj.Error(ec) =>
              Wr.PutText(log, Fmt.F("   enqueue failed at %s: %s\n",
                                    h.sitename, PkgErr.Msg(ec)));
          END;
          RETURN;
        END;
      END;
    END;
    <* ASSERT FALSE *>
  END RequestUpdate;

PROCEDURE SendUpdate(c: Closure; package: PN; log: Wr.T)
    RAISES {Thread.Alerted} = 
  <* FATAL Wr.Failure *>
  VAR h := c.localHost;
      site: SiteName;
  BEGIN
    FOR i := 1 TO LAST(c.hostArray^) DO
      IF c.hostArray[i].state = State.OutOfDate THEN
        site := c.hostArray[i].sitename;
        Wr.PutText(log, Fmt.F("sending version %s to %s\n", 
              Fmt.Int(h.entry.curVN), site));
        TRY
          Send.Enqueue(
            package, Version {h.entry.instance, h.entry.curVN},
            h.entry.managedBy, site);
        EXCEPT
        | PkgErr.E(ec) =>
            Wr.PutText(log, Fmt.F("    enqueue failed to %s: %s\n",
                                  site, PkgErr.Msg(ec)));

        END;
      END;
    END;
  END SendUpdate;

PROCEDURE DeleteLocalPackage(
      pkg: PN; v: Version; managedBy: SiteName; log: Wr.T)
      RAISES {Thread.Alerted} =
  <* FATAL NetObj.Error *>
  VAR siphonT: Siphon.T;
    <* FATAL Wr.Failure *>
  BEGIN
    Wr.PutText(log, Fmt.F("   deleting package at local site\n"));
    TRY
      siphonT := Siphon.New(NIL);
      siphonT.ship(
        pkg, PackageLib.EmptySource(), v, Param.localSite, managedBy, NIL);
    EXCEPT
    | PkgErr.E(ec) =>
         Wr.PutText(log, Fmt.F("    local deletion failed: %s\n",
                                 PkgErr.Msg(ec)));
    END;
  END DeleteLocalPackage;

PROCEDURE SkulkerDaemon(<* UNUSED *> a: Thread.Closure): REFANY =
  CONST
    PauseTime = (6 * 60 * 60);
    <* FATAL Thread.Alerted *>
  BEGIN
    LOOP
      SynchAllPackages(SynchKind.UpdateSelf, NIL);
      Thread.Pause(FLOAT(PauseTime, LONGREAL));
    END;
  END SkulkerDaemon;

PROCEDURE Init() RAISES {} =
  BEGIN
    synchMutex := NEW(MUTEX);
    synching := FALSE;
    EVAL Thread.Fork(NEW(Thread.Closure, apply := SkulkerDaemon));
  END Init;

BEGIN
END Skulker.
