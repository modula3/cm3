(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* SiphonServer.m3 *)
(* Last modified on Thu Feb  2 09:02:26 PST 1995 by kalsow  *)
(*      modified on Tue Jul 26 13:02:42 PDT 1994 by wobber  *)
(*      modified on Fri Jul  3  9:16:42 GMT+2:00 1992 by prusker *)

MODULE SiphonServer EXPORTS Param, SiphonServer;

IMPORT RefList, Send, Skulker, Text, Util,
    Siphon, Site, LockOps, NetObj, Fmt, ServerLog, Param,
    Thread, PackageObj, PkgErr, PSLib, PackageEvents, FmtTime, Time;
    
FROM Siphon IMPORT SiteList;
FROM LockOps IMPORT SiteName, Version;

TYPE
  PN = PackageObj.PN;

TYPE
  T = Siphon.T BRANDED OBJECT
  OVERRIDES
    ship := Ship;
    enqueue := Enqueue;
    dequeue := Dequeue;
    synch := Synch;
    lockserver := LockServer;
    status := Status;
  END;

  Rcv = PackageObj.Monitor OBJECT
    next: Rcv := NIL;
    who: SiteName;
    pkg: PN;
    ver: Version;
    r: RcvRec;
    duplicate: BOOLEAN := FALSE;
  OVERRIDES
    report := ShipReport;
  END;

  State = {Active, Finished, Abandoned, Duplicate};
  RcvRec = RECORD
    state: State := State.Active;
    startTime: Time.T;
    finishTime: Time.T;
    filesFetched := 0;
    bytesFetched: CARDINAL := 0;
    elapsedMSec: CARDINAL := 0;
    worstPulled: CARDINAL := 0;
    worstCached: CARDINAL := LAST(CARDINAL);
    worstUnchanged: CARDINAL := LAST(CARDINAL);
  END;

VAR
   rcvList: Rcv := NIL;
   rcvMutex := NEW(MUTEX);
   stats: ServerLog.Stats := NIL;

PROCEDURE New(siteName: TEXT): Siphon.T =
  VAR statNames := ARRAY [0..4] OF TEXT {
                         "sent", "rcvd", "sending", "recving", "enqueued"};
  BEGIN
    log := PSLib.log;
    localSite := siteName;

        (* call the various init *)
    stats := ServerLog.NewStats(log, "siphon(" & siteName & ")", statNames);
    Skulker.Init();
    RETURN NEW(T);
  END New;
  
PROCEDURE StatIncr(which: CARDINAL) =
  BEGIN
    stats.incr(which);
  END StatIncr;

PROCEDURE StatDecr(which: CARDINAL) =
  BEGIN
    stats.decr(which);
  END StatDecr;


EXCEPTION
  SameVersion;

PROCEDURE Ship(
  <*UNUSED*> t: T; 
    package: PackageObj.PN; 
    source: PackageObj.Source;
    version: Version;
    caller: SiteName;
    manager: SiteName;
    fwd: SiteList
    ) RAISES {PkgErr.E, Thread.Alerted} =
  (* start shipping a package across the siphon *)
  VAR
    rcv: Rcv;
    receive: BOOLEAN := FALSE;
    id: TEXT;
    finished := FALSE;
  BEGIN
    id := Fmt.F("S.Ship [%s]", Fmt.Int(Util.Unique()));

    Util.LogText(Fmt.F("%s %s(%s.%s) from %s\n", 
           id, PSLib.PkgText(package),
           Fmt.Int(version.t), Fmt.Int(version.vn), caller));

    (* check for lock database entry *)
    TRY
      IF AssertNewVersion(package, version, manager) THEN
        LockOps.CheckDir(package.dir);
        Util.LogText(Fmt.F("%s: creating foreign entry\n", id));
        LockOps.CreateForeign(SystemAuth, package, manager, version.t);
      END;
      receive := TRUE;
    EXCEPT
    | SameVersion =>
        Util.LogText(Fmt.F("%s: version present\n", id));
    | PkgErr.E(ec) =>
        Util.LogText(Fmt.F("%s: failed -- %s\n", id, PkgErr.Msg(ec)));
        RAISE PkgErr.E(ec);
    END;

    IF receive THEN
      LOCK rcvMutex DO
        rcv := rcvList;
        WHILE rcv # NIL DO
          IF Text.Equal(caller, rcv.who) THEN
            IF rcv.r.state # State.Active THEN EXIT; END;
            rcv.duplicate := TRUE;
          END;
          rcv := rcv.next;
        END;
        IF rcv = NIL THEN
          rcv := NEW(Rcv, next := rcvList, who := caller);
          rcvList := rcv;
        END;
        rcv.pkg := package;
        rcv.ver := version;
        rcv.r := RcvRec{startTime := Time.Now(), finishTime := 0.0D0};
      END;
      TRY
        StatIncr(StatCurrRecv);
        DoReceive(rcv, source, id);
        finished := TRUE;
        Util.LogText(
          Fmt.F(
            "%s: receive complete\n    pulled=%s unchanged=%s cached=%s\n",
             id, Fmt.Int(rcv.r.worstPulled),
             Fmt.Int(rcv.r.worstUnchanged), Fmt.Int(rcv.r.worstCached))
          & BytesFetched(rcv));
        StatIncr(StatRecv);
      FINALLY
        LOCK rcvMutex DO
          IF rcv.duplicate THEN
            rcv.r.state := State.Duplicate;
          ELSIF finished THEN
            rcv.r.state := State.Finished;
          ELSE
            rcv.r.state := State.Abandoned;
          END;
          rcv.r.finishTime := Time.Now();
        END;
        StatDecr(StatCurrRecv);
      END;
    END;
    WHILE fwd # NIL DO
      TRY
        Send.Enqueue(package, version, manager, fwd.head);
      EXCEPT
      | PkgErr.E =>
      END;
      fwd := fwd.tail;
    END;
  END Ship;
  
TYPE
  ShipFork = Thread.SizedClosure OBJECT
    name: TEXT;
    siblings: PackageObj.Siblings;
    index: CARDINAL;
    ok: BOOLEAN := TRUE;
    ec: PkgErr.TL := NIL;
    source: PackageObj.Source;
    id: TEXT;
    rcv: Rcv;
    thread: Thread.T := NIL;
  OVERRIDES
    apply := DoShipFork;
  END;

PROCEDURE DoReceive(rcv: Rcv; source: PackageObj.Source; id: TEXT)
    RAISES {PkgErr.E, Thread.Alerted} =
  VAR reps: REF ARRAY OF TEXT;
      options: PackageObj.ShipOptions;
      sa, l: RefList.T;
      repT: PackageObj.T;
      siblings: PackageObj.Siblings;
      sf: ShipFork;
      ok := FALSE;
      cf: LockOps.CommitFailures;
      failure: PkgErr.TL := NIL;
  PROCEDURE PrintCF(cf: LockOps.CommitFailures) : TEXT =
    VAR out := "";
    BEGIN
      FOR i := 0 TO LAST(cf^) DO
        IF cf[i] # NIL THEN
          out := out &
              Fmt.F("           %s: %s\n", reps[i], PkgErr.Msg(cf[i]));
        END;
      END;
      RETURN out;
    END PrintCF;
  BEGIN
    options.keepBackup := FALSE;
    options.purgeLinks := TRUE;
    options.forceDateMatch := TRUE;
    reps := Site.Get().replicas;
    siblings := NEW(PackageObj.Siblings, NUMBER(reps^));
    FOR i := 0 TO LAST(reps^) DO
      TRY
        repT := PackageObj.New(reps[i]);
        siblings[i] := repT.newShip(Param.SystemAuth, rcv.pkg, options);
        sa := RefList.Cons(NEW(ShipFork,
                    stackSize := 2 * Thread.GetDefaultStackSize(),
                    source := source, siblings := siblings,
                    index := i, id := id, rcv := rcv, name := reps[i]), sa);
      EXCEPT
      | NetObj.Error, PkgErr.E  =>
      END;
    END;
    IF sa = NIL THEN
      Util.LogText(Fmt.F("%s: no replicas available\n", id)); 
      PkgErr.Raise(PkgErr.NoReplicas);
    END;
    l := sa;
    WHILE l # NIL DO
      sf := l.head; l := l.tail;
      sf.thread := Thread.Fork(sf);
    END;
    l := sa;
    WHILE l # NIL DO
      sf := l.head; l := l.tail;
      EVAL Thread.Join(sf.thread);
      IF sf.ok THEN ok := TRUE; ELSE
        sf.siblings[sf.index] := NIL;
        failure := sf.ec;
      END;
    END;
    IF ok THEN
      TRY
        cf := LockOps.Commit(
            Param.SystemAuth, rcv.pkg, rcv.ver, siblings^, FALSE);
        IF cf # NIL THEN
          Util.LogText(Fmt.F("%s: replicas failed --\n%s",
                              id, PrintCF(cf))); 
        END;
      EXCEPT
      | PkgErr.E(ec) =>
          Util.LogText(Fmt.F("%s: commit error -- %s\n", id, PkgErr.Msg(ec))); 
          RAISE PkgErr.E(ec);
      | LockOps.CommitFailed(cf) =>
          Util.LogText(Fmt.F("%s: commit failed all replicas --\n%s",
                              id, PrintCF(cf))); 
          RaiseCF(cf);
      END;
    ELSE
      Util.LogText(Fmt.F("%s: all replicas failed\n", id)); 
      IF failure # NIL THEN
        RAISE PkgErr.E(failure);
      ELSE
        PkgErr.Raise(PkgErr.NoReplicas);
      END;
    END;
  END DoReceive; 

PROCEDURE RaiseCF(cf: LockOps.CommitFailures) RAISES {PkgErr.E} =
  VAR res: PkgErr.TL := NIL;
      this: PkgErr.TL;
      useThis: BOOLEAN := FALSE;
  BEGIN
    FOR i := 0 TO LAST(cf^) DO
      this := cf[i];
      IF res = NIL THEN
        res := this;
      ELSIF NOT useThis THEN
        IF this.head = PkgErr.LockServerDown OR
              this.head = PkgErr.SourceFailed OR
              this.head = PkgErr.NoReplicas THEN
          res := this;
          useThis := TRUE;
        ELSIF this.head = PkgErr.NoRoomInFS OR
              this.head = PkgErr.IOError OR
              this.head = PkgErr.AccessViolation THEN
          res := this;
        END;
      END;
    END;
    IF res = NIL THEN
      PkgErr.Raise(PkgErr.NoReplicas);
    ELSE
      RAISE PkgErr.E(res);
    END;
  END RaiseCF;

PROCEDURE ShipReport(rcv: Rcv; this: REFANY) =
  BEGIN
    LOCK rcvMutex DO
      TYPECASE this OF
      | PackageEvents.FileReport(fr) =>
          IF NOT fr.fromSibling AND
                  (fr.type = PackageEvents.FileET.New OR
                  fr.type = PackageEvents.FileET.Updated) THEN
            INC(rcv.r.filesFetched);
            INC(rcv.r.bytesFetched, fr.bytesTransferred);
            INC(rcv.r.elapsedMSec, fr.elapsedMSec);
          END;
      | PackageEvents.PrepareReport(pr) =>
          rcv.r.worstPulled := MAX(rcv.r.worstPulled, pr.filesPulled);
          rcv.r.worstCached := MIN(rcv.r.worstCached, pr.filesFoundInCache);
          rcv.r.worstUnchanged := MIN(rcv.r.worstUnchanged, pr.filesUnchanged);
      ELSE
      END;
    END;
  END ShipReport;

PROCEDURE DoShipFork(sf: ShipFork) : REFANY =
  VAR err: PkgErr.TL := NIL;
  <* FATAL Thread.Alerted *>
  <* FATAL PackageObj.SourceOutOfDate *>
  BEGIN
    TRY
      (* add monitor here !!! *)
      sf.siblings[sf.index].prepare(sf.source, sf.siblings, sf.rcv);
    EXCEPT
    | PkgErr.E(ec) =>
        sf.ec := ec;
        sf.ok := FALSE;
        err := ec;
    | NetObj.Error(ec) =>
        sf.ec := ec;
        sf.ok := FALSE;
        err := ec;
    END;
    IF err # NIL THEN
      Util.LogText(Fmt.F("%s: prepare failed at %s -- %s\n",
                   sf.id, sf.name, PkgErr.Msg(err))); 
    END;
    RETURN NIL;
  END DoShipFork;

PROCEDURE BytesFetched(rcv: Rcv) : TEXT =
  VAR rate, timeval: TEXT;
  BEGIN
    (* called with rcvMutex held *)
    IF rcv.r.elapsedMSec = 0 THEN
      rate := "0";
    ELSE
      rate := Fmt.Int((rcv.r.bytesFetched DIV rcv.r.elapsedMSec) * 8);
    END;
    IF rcv.r.state = State.Active THEN
      timeval := Util.IntervalSince(rcv.r.startTime);
    ELSE
      timeval := Util.Interval(rcv.r.finishTime - rcv.r.startTime);
    END;
    RETURN Fmt.F("    fetched %s bytes, %s files in %s, avgRate=%s kbit/s\n",
              Fmt.Int(rcv.r.bytesFetched), Fmt.Int(rcv.r.filesFetched),
              timeval, rate); 
  END BytesFetched;

PROCEDURE ReceiveStatus() : TEXT =
  VAR out := "Receive status ... ";
      rcv: Rcv;
  BEGIN
    LOCK rcvMutex DO
      rcv := rcvList;
      IF rcv = NIL THEN out := out & "none\n"; ELSE out := out & "\n"; END;
      WHILE rcv # NIL DO
        CASE rcv.r.state OF
        | State.Finished =>
            out := out &
              Fmt.F("  Received %s(%s) from %s, finished %s\n", 
                      PSLib.PkgText(rcv.pkg),
                      Fmt.Int(rcv.ver.vn), rcv.who,
                      FmtTime.Short(rcv.r.finishTime)) &
              BytesFetched(rcv) &
              Fmt.F("     pulled=%s unchanged=%s cached=%s\n",
                 Fmt.Int(rcv.r.worstPulled),
                 Fmt.Int(rcv.r.worstUnchanged), Fmt.Int(rcv.r.worstCached));
        | State.Active =>
            out := out &
              Fmt.F("  Receiving %s(%s) from %s, started %s\n", 
                      PSLib.PkgText(rcv.pkg),
                      Fmt.Int(rcv.ver.vn), rcv.who,
                      FmtTime.Short(rcv.r.startTime)) &
              BytesFetched(rcv);
        | State.Abandoned =>
            out := out &
              Fmt.F("  Abandoned  %s(%s) from %s, started %s\n", 
                      PSLib.PkgText(rcv.pkg),
                      Fmt.Int(rcv.ver.vn), rcv.who,
                      FmtTime.Short(rcv.r.startTime)) &
              BytesFetched(rcv);
        | State.Duplicate =>
        END;
        rcv := rcv.next;
      END;
    END;
    RETURN out;
  END ReceiveStatus;

PROCEDURE AssertNewVersion(pkg: PN; ver: Version; mgr: SiteName): BOOLEAN
    RAISES {PkgErr.E, SameVersion} =
  VAR
    entry:      LockOps.RefEntry;
  BEGIN
    TRY
      entry := LockOps.GetEntry(pkg);
    EXCEPT
    | PkgErr.E(ec) =>
        IF ec.head = PkgErr.NoSuchPackage AND
                  NOT Text.Equal(mgr, localSite) THEN
          RETURN TRUE;
        END;
        RAISE PkgErr.E(ec);
    END;
    IF (ver.t # entry.instance) THEN
      PkgErr.Raise(PkgErr.BadVersionStamp);
    END;
    IF NOT Text.Equal(mgr, entry.managedBy) THEN
      PkgErr.Raise(PkgErr.PackageMultiplyManaged);
    END;
    IF ver.vn <= entry.curVN THEN
      IF (ver.vn # LockOps.InitialVN) OR
         (entry.curVN # LockOps.InitialVN)
      THEN
        IF ver.vn = entry.curVN THEN
          RAISE SameVersion;
        ELSE
          PkgErr.Raise(PkgErr.StaleVersion);
        END;
      END;
    END;
    RETURN FALSE;
  END AssertNewVersion;

(* old SiphonControl *)

PROCEDURE Enqueue(
  <*UNUSED*> t: T; 
  package:   PN;
  version:   Version;
  manager: SiteName;
  forSite: SiteName := NIL;
  urgent:    BOOLEAN := FALSE
  ) RAISES {PkgErr.E} =
  BEGIN
    Send.Enqueue(package, version, manager, forSite, urgent);
  END Enqueue;

PROCEDURE Dequeue(
  <*UNUSED*> t: T; 
  package:       PN;
  forSite:       SiteName := NIL;
  evenIfSent: BOOLEAN := FALSE
  ): BOOLEAN RAISES {PkgErr.E} =
  BEGIN
    RETURN Send.Dequeue(package, forSite, evenIfSent);
  END Dequeue;

PROCEDURE Synch(<*UNUSED*> t: T; kind: Siphon.SynchKind; package: PN): TEXT
    RAISES {Thread.Alerted} =
  BEGIN
    RETURN Skulker.Synch(kind, package);
  END Synch;

PROCEDURE LockServer(<*UNUSED*> t: T): LockOps.T RAISES {PkgErr.E} =
  BEGIN
    RETURN LockOps.New();
  END LockServer;

PROCEDURE Status(<*UNUSED*> t: T): Text.T RAISES {Thread.Alerted} =
  BEGIN
    RETURN Send.Showqueues() & ReceiveStatus() & ReplicaStatus();
  END Status;

PROCEDURE ReplicaStatus() : TEXT RAISES {Thread.Alerted} =
  VAR reps := Site.Get().replicas;
      out := "Replica statistics ...\n";
  BEGIN
    FOR i := 0 TO LAST(reps^) DO
      TRY
        out := out & PackageObj.New(reps[i]).status();
      EXCEPT
      | PkgErr.E, NetObj.Error =>
          out := out & "Replica " & reps[i] & " is unreachable\n";
      END;
    END;
    RETURN out;
  END ReplicaStatus;

BEGIN
  log := NIL;
END SiphonServer.

(*
TYPE
  Elem = Thread.Closure OBJECT
    name:   Text.T;
    ok:     BOOLEAN;
    source: Text.T;
    target: Text.T;
    vers:   Version;
    thread: Thread.T;
  OVERRIDES
    apply := CopyPackageFork;
  END;
  ElemArray = REF ARRAY OF Elem;

PROCEDURE CopyPackage(sourcePkg, targetPkg: Text.T; version: Version): Text.T =
  VAR
    replicas: LockOps.ReplicaSet;
    nGood: CARDINAL;
    table:    ElemArray;
    host:     Elem;
    refany:   REFANY;
    msg:      Text.T;
    first:    BOOLEAN;
  BEGIN
    replicas := Info.GetReplicas();
    table := NEW(ElemArray, NUMBER(replicas^));
    FOR i := 0 TO LAST(table^) DO
      host := NEW(Elem);
      table[i] := host;
      host.ok     := FALSE;
      host.name   := replicas^[i];
      host.source := sourcePkg;
      host.target := targetPkg;
      host.vers   := version;
      host.thread := Thread.Fork(host);
    END;
    nGood := 0;
    FOR i := 0 TO LAST(table^) DO
      host   := table^[i];
      refany := Thread.Join(host.thread);
      IF host.ok THEN INC(nGood); END;
    END;
    IF nGood = 0 THEN RETURN "failed on all replicas"; END;
    msg   := "succeeded at replicas";
    first := TRUE;
    FOR i := 0 TO LAST(table^) DO
      host := table^[i];
      IF host.ok THEN
        IF first THEN first := FALSE; ELSE msg := Text.Cat(msg, ","); END;
        msg := msg & " " & host.name;
      END;
    END;
    RETURN msg;
  END CopyPackage;

PROCEDURE CopyPackageFork(a: REFANY): REFANY =
  VAR
    host: Elem;
  BEGIN
    host := a;
    TRY
      pkgT := PackageObj.New(NIL); (*????*)
      host.ok := t.copy(host.source, host.target, host.vers);
    EXCEPT
    | NetObj.Error, PkgErr.Err =>
    END;
    RETURN NIL;
  END CopyPackageFork;
*)
