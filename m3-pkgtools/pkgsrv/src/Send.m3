(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Send.m3 *)
(* Last modified on Thu Feb  2 09:01:48 PST 1995 by kalsow  *)
(*      modified on Tue Jul 26 12:44:58 PDT 1994 by wobber  *)
(*      modified on Fri Jul  3  7:40:52 GMT+2:00 1992 by prusker *)

MODULE Send;

IMPORT Text, TextList, Wr, NetPath, LockOps, Thread, Time, PackageObj,
    TextWr, Site, Siphon, Fmt, Subrepo, NetObj, Param,
    PackageLib, PSLib, PkgErr, Util, FmtTime;

FROM LockOps IMPORT Instance, SiteName, Version;
FROM PackageObj IMPORT PN;
FROM Siphon IMPORT SiteList;

TYPE
  Route = RECORD first, final: SiteName; END;

  Queue = Thread.Closure OBJECT
    site: SiteName;
    thread: Thread.T := NIL;
    head: QueueElem := NIL;
    deferrals: QueueElem := NIL;
    sending: QueueElem := NIL;
    startTime: Time.T;
    linkStatus: PkgErr.TL := NIL;
    linkStatusChange: Time.T;
    nextQ: Queue := NIL;
  OVERRIDES
    apply := Sender;
  END;

  QueueElem = REF RECORD
    next: QueueElem := NIL;
    package: PN;
    version: Version;
    managedBy: SiteName;
    fwd: SiteList;
    enqueueTime: Time.T;
    deferralTime: Time.T;
    try: CARDINAL := 0;
  END;

VAR
  mutex: MUTEX := NEW(MUTEX);
  queueList: Queue := NIL;

(* exports to Send.i3 *)

PROCEDURE Enqueue(
    package: PN;
    version: Version;
    manager: SiteName;
    forSite: SiteName;
    urgent := FALSE) RAISES {PkgErr.E} =
  VAR
    remotes: REF ARRAY OF Site.Remote;
    route: Route;
  BEGIN
    IF forSite # NIL THEN
      Util.LogText(Fmt.F("Send.EnQ of %s(%s) to %s\n",
            PSLib.PkgText(package), Fmt.Int(version.vn), forSite));
      EVAL ComputeRoute(
         package, CheckSite("Send.EnQ", forSite), FALSE, route);
      EnqueueIt(route, package, version, manager, urgent);
    ELSE
      remotes := Site.Get().foreignSites;
      IF remotes # NIL THEN
        Util.LogText(Fmt.F("Send.EnQ of %s(%s)\n",
                PSLib.PkgText(package), Fmt.Int(version.vn)));
        FOR i := 0 TO LAST(remotes^) DO
          IF ComputeRoute(package, remotes[i], TRUE, route) THEN
            EnqueueIt(route, package, version, manager, urgent);
          END;
        END;
      END;
    END;
  END Enqueue;

PROCEDURE Dequeue(
    package: PN;
    forSite: SiteName;
    interruptSend: BOOLEAN) : BOOLEAN RAISES {PkgErr.E} =
  VAR
    q: Queue;
    res: BOOLEAN := FALSE;
  BEGIN
    IF forSite = NIL THEN
      Util.LogText(Fmt.F("Send.DeQ of %s\n", PSLib.PkgText(package)));
    ELSE
      Util.LogText(Fmt.F("Send.DeQ of %s for %s\n",
            PSLib.PkgText(package), forSite));
      EVAL CheckSite("Send.DeQQ", forSite);
    END;
    LOCK mutex DO
      q := queueList;
      WHILE q # NIL DO
        IF forSite = NIL OR Text.Equal(forSite, q.site) THEN
          IF interruptSend AND q.sending # NIL AND
               NetPath.EqualPN(package, q.sending.package) THEN
            Thread.Alert(q.thread);
            res := TRUE;
          END;
          VAR waste: BOOLEAN;
              qe := FindQueueElem(q, package, LAST(Instance), waste);
          BEGIN
            IF qe # NIL THEN
              DequeueElem(q, qe);
              res := TRUE;
            END;
          END;
        END; (* IF*)
        q := q.nextQ;
      END; (* WHILE *)
    END; (* LOCK *)
    RETURN res;
  END Dequeue;
  
PROCEDURE Showqueues(): TEXT  =
  VAR
    q: Queue;
    wr: Wr.T;
    <* FATAL Wr.Failure, Thread.Alerted *>
  PROCEDURE PrintElem(x: QueueElem; deferred: BOOLEAN) = 
    VAR str: TEXT;
    BEGIN
      IF x = q.sending THEN
        str := "try [" & Fmt.Int(x.try) & "] since " &
                     FmtTime.Short(q.startTime) &
                     " (" & Util.IntervalSince(q.startTime) & ")";
      ELSE
        str := "enqueued at " & FmtTime.Short(x.enqueueTime);
        IF deferred THEN
          str := str & ", deferred until " & FmtTime.Short(x.deferralTime);
        END;
      END;
      Wr.PutText(wr, Fmt.F("    %s(%s) %s\n",
        PSLib.PkgText(x.package), Fmt.Int(x.version.vn), str));
    END PrintElem;
  PROCEDURE PrintConnStatus() =
    BEGIN
      IF q.linkStatus = NIL THEN
        Wr.PutText(wr,
         Fmt.F("ok for %s\n",
             Util.IntervalSince(q.linkStatusChange)));
      ELSE
        Wr.PutText(wr,
         Fmt.F("down for %s\n",
             Util.IntervalSince(q.linkStatusChange)));
        Wr.PutText(wr,
           Fmt.F("     Reason: %s\n", PkgErr.Msg(q.linkStatus)));
      END;
    END PrintConnStatus;
  BEGIN
    wr := TextWr.New();
    Wr.PutText(wr, "Send queue status ... ");
    LOCK mutex DO
      q := queueList;
      IF q = NIL THEN Wr.PutText(wr, "none\n"); ELSE Wr.PutText(wr, "\n"); END;
      WHILE q # NIL DO
        Wr.PutText(wr, "  " & q.site & ": ");
        PrintConnStatus();
        IF q.sending # NIL THEN PrintElem(q.sending, FALSE); END;
        VAR qe := q.head; BEGIN
          WHILE qe # NIL DO PrintElem(qe, FALSE); qe := qe.next; END;
        END;
        VAR qe := q.deferrals; BEGIN
          WHILE qe # NIL DO PrintElem(qe, TRUE); qe := qe.next; END;
          q := q.nextQ;
        END;
      END;
    END;
    RETURN TextWr.ToText(wr);
  END Showqueues;

PROCEDURE EnqueueIt(
    route: Route; package: PN; v: Version;
    mgr: SiteName; urgent: BOOLEAN) =
  VAR
    q: Queue;
    qe: QueueElem;
    deferred: BOOLEAN;
  BEGIN
    LOCK mutex DO
      q := FindQueue(route.first);
      IF q.sending # NIL AND
            NetPath.EqualPN(package, q.sending.package) AND
            (q.sending.version.t = v.t) AND
            (v.vn <= q.sending.version.vn) THEN
        (* if greater than sending version -- re-enqueue *)
        IF v.vn <= q.sending.version.vn THEN RETURN; END;
      END;
      qe := FindQueueElem(q, package, v.t, deferred);
      IF qe # NIL THEN
        IF (deferred OR urgent) AND v.vn >= qe.version.vn THEN
          (* greater or equal to deferred version -- re-enqueue *)
          DequeueElem(q, qe);
        ELSE
          (* update version number if greater *)
          IF v.vn > qe.version.vn THEN qe.version := v; END;
          MaybeAddRoute(qe, route);
          qe.enqueueTime := Time.Now();
          RETURN;
        END;
      END;
      qe := NEW(QueueElem, package := package,
                  version := v, managedBy := mgr, enqueueTime := Time.Now());
      MaybeAddRoute(qe, route);
      EnqueueElem(q, qe, FALSE, urgent);
    END;
  END EnqueueIt;

PROCEDURE FindQueue(site: SiteName): Queue =
    (* called with mutex held *)
  VAR q := queueList;
  BEGIN
    WHILE q # NIL AND NOT Text.Equal(site, q.site) DO q := q.nextQ; END;
    IF q = NIL THEN
      q := NEW(Queue, site := site, linkStatusChange := Time.Now());
      q.thread := Thread.Fork(q);
      q.nextQ := queueList;
      queueList := q;
    END;
    RETURN q;
  END FindQueue;

PROCEDURE FindQueueElem(q: Queue; package: PN;
              inst: Instance; VAR (*OUT*) deferred: BOOLEAN): QueueElem =
  PROCEDURE TestIt(qe: QueueElem): BOOLEAN =
    BEGIN
      RETURN NetPath.EqualPN(package, qe.package) AND
                (inst = LAST(Instance)) OR (inst = qe.version.t);
    END TestIt;
  BEGIN
    VAR qe := q.head; BEGIN
      WHILE qe # NIL DO
        IF TestIt(qe) THEN
          deferred := FALSE;
          RETURN qe;
        ELSE
          qe := qe.next;
        END;
      END;
    END;
    VAR qe := q.deferrals; BEGIN
      WHILE qe # NIL DO
        IF TestIt(qe) THEN
          deferred := TRUE;
          RETURN qe;
        ELSE
          qe := qe.next;
        END;
      END;
    END;
    RETURN NIL;
  END FindQueueElem;

PROCEDURE EnqueueElem(q: Queue; qe: QueueElem; defer, urgent: BOOLEAN) =
    (* called with mutex held *)
  BEGIN
    Param.StatIncr(Param.StatQueued);
    IF urgent THEN
      qe.next := q.head;
      q.head := qe;
    ELSIF defer THEN
      VAR tail := q.deferrals; BEGIN
        IF tail = NIL THEN
          q.deferrals := qe;
        ELSE
          WHILE tail.next # NIL DO tail := tail.next; END;
          tail.next := qe;
        END;
        qe.next := NIL;
      END;
    ELSE
      VAR tail := q.head; BEGIN
        IF tail = NIL THEN
          q.head := qe;
        ELSE
          WHILE tail.next # NIL DO tail := tail.next; END;
          tail.next := qe;
        END;
        qe.next := NIL;
      END;
    END;
  END EnqueueElem;

PROCEDURE DequeueElem(q: Queue; qe: QueueElem) =
    (* called with mutex held *)
  BEGIN
    VAR try := q.head; prev: QueueElem := NIL; BEGIN
      WHILE try # NIL DO
        IF try = qe THEN
          IF prev = NIL THEN
            q.head := try.next;
          ELSE
            prev.next := try.next;
          END;
          Param.StatDecr(Param.StatQueued);
          RETURN;
        END;
        prev := try;
        try := try.next;
      END;
    END;
    VAR try := q.deferrals; prev: QueueElem := NIL; BEGIN
      WHILE try # NIL DO
        IF try = qe THEN
          IF prev = NIL THEN
            q.deferrals := try.next;
          ELSE
            prev.next := try.next;
          END;
          Param.StatDecr(Param.StatQueued);
          RETURN;
        END;
        prev := try;
        try := try.next;
      END;
    END;
  END DequeueElem;

PROCEDURE DequeueFirstElem(q: Queue): QueueElem =
    (* called with mutex held *)
  VAR qe := q.head;
  BEGIN
    IF qe # NIL THEN
      q.head := qe.next;
      Param.StatDecr(Param.StatQueued);
      RETURN qe;
    END;
    qe := q.deferrals;
    IF qe # NIL AND qe.deferralTime <= Time.Now() THEN
      q.deferrals := qe.next;
      Param.StatDecr(Param.StatQueued);
      RETURN qe;
    END;
    RETURN NIL;
  END DequeueFirstElem;

PROCEDURE MaybeAddRoute(qe: QueueElem; route: Route) =
  VAR l := qe.fwd;
  BEGIN
    IF route.first # route.final THEN
      WHILE l # NIL DO
        IF Text.Equal(route.final, l.head) THEN RETURN; END;
        l := l.tail;
      END;
      qe.fwd := TextList.Cons(route.final, qe.fwd);
    END;
  END MaybeAddRoute;

PROCEDURE ComputeRoute(
    package: PN; READONLY rem: Site.Remote;
    checkSubrep: BOOLEAN; VAR (*out*) route: Route) : BOOLEAN =
  BEGIN
    route.first := rem.name;
    route.final := rem.name;
    IF rem.route # NIL THEN
      FOR j := 0 TO LAST(rem.route^) DO
        IF NOT checkSubrep OR
               Subrepo.Has(rem.route[j], package) # Subrepo.R.No THEN
          route.first := rem.route[j];
          EXIT;
        END;
      END;
    END;
    IF checkSubrep AND Subrepo.Has(rem.name, package) = Subrepo.R.No THEN
      RETURN FALSE;
    END;
    RETURN TRUE;
  END ComputeRoute;

PROCEDURE CheckSite(op: TEXT; dest: SiteName) : Site.Remote
    RAISES {PkgErr.E} =
  VAR rem: Site.Remote;
  BEGIN
    IF NOT Site.FindRemote(dest, rem) THEN
      Util.LogText(Fmt.F("%s failed for %s: no such site\n", op, dest));
      PkgErr.Raise(PkgErr.NoSuchSite);
    END;
    RETURN rem;
  END CheckSite;

CONST
    SoonPause = 30;  (* seconds *)
    DeferralPause = 60 * 60;   (* one hour *)
    PollPause = 30;  (* seconds *)
    PollPauseMax = 20;  (* 20 * PollPause = 10 minutes *)

TYPE SendResult = {DequeueIt, DeferIt, RetryIt};

PROCEDURE Sender(q: Queue) : REFANY =
  VAR
    siphonT: Siphon.T := NIL;
    res: SendResult;
    pollPauses: CARDINAL := 0;
    qe: QueueElem;
  BEGIN
    LOOP
      TRY
        LOOP
          IF siphonT = NIL THEN siphonT := GetSiphonT(q); END;
          LOCK mutex DO
            qe := DequeueFirstElem(q);
            IF qe = NIL THEN EXIT; END;
            q.sending := qe;
            q.startTime := Time.Now();
            INC(qe.try);
          END;
          Param.StatIncr(Param.StatCurrSend);
          res := SendOne(siphonT, q, qe);
          Param.StatDecr(Param.StatCurrSend);
          LOCK mutex DO
            CASE res OF
            | SendResult.DeferIt =>
                (* add to deferral queue *)
                qe.deferralTime := Time.Now() + FLOAT(DeferralPause, LONGREAL);
                EnqueueElem(q, qe, TRUE, FALSE);
                pollPauses := 0;
            | SendResult.RetryIt =>
                (* add back to head of main queue *)
                EnqueueElem(q, qe, FALSE, TRUE);
                siphonT := NIL;
            ELSE
                pollPauses := 0;
            END;
            q.sending := NIL;
            EVAL Thread.TestAlert();  (* in case an abort came late *)
          END;
          IF siphonT = NIL THEN
            (* failure case, wait for a while before retrying *)
            Thread.Pause(FLOAT(SoonPause, LONGREAL));
          END;
        END;
        (* empty queue, or no deferrals to act on now *)
        Thread.Pause(FLOAT(PollPause, LONGREAL));
        INC(pollPauses);
        IF pollPauses = PollPauseMax THEN
          pollPauses := 0;
          siphonT := NIL;
        END;
      EXCEPT
      | Thread.Alerted =>
      END;
    END;
    <*NOWARN*> RETURN NIL;
  END Sender;

PROCEDURE SendOne(sT: Siphon.T; q: Queue; qe: QueueElem) : SendResult =
  VAR
    replica: TEXT;
    source: PackageObj.Source;
    id := Util.Unique();
    res := SendResult.DequeueIt;
  BEGIN
    TRY
      Util.LogText(
        Fmt.FN("NewSend %s(%s.%s) [%s] to %s (try %s)\n",
          ARRAY OF TEXT {
            PSLib.PkgText(qe.package),
            Fmt.Int(qe.version.t), Fmt.Int(qe.version.vn), 
            Fmt.Int(id), q.site, Fmt.Int(qe.try)} ));
      CASE qe.version.vn OF
      | LockOps.InitialVN, LockOps.DeletedVN =>
          source := PackageLib.EmptySource();
      ELSE
          source := PickAServer(qe.package, qe.version, replica);
      END;
      sT.ship(
          qe.package, source, qe.version,
          Param.localSite, qe.managedBy, qe.fwd);
      Param.StatIncr(Param.StatSend);
      Util.LogText(Fmt.F("Send [%s] complete\n", Fmt.Int(id)));
    EXCEPT
    | Thread.Alerted =>
        Util.LogText(Fmt.F("Send [%s] aborted\n", Fmt.Int(id)));
    | NetObj.Error(ec) =>
        IF ec.head = NetObj.Alerted THEN
          Util.LogText(Fmt.F("Send [%s] aborted\n", Fmt.Int(id)));
        ELSE
          Util.LogText(
            Fmt.F("Send [%s] failed: %s\n", Fmt.Int(id), PkgErr.Msg(ec)));
          res := SendResult.RetryIt;
        END;
    | NoSource(none) =>
        IF none THEN
          Util.LogText(
            Fmt.F("Send [%s] failed: package version does not exist\n",
                   Fmt.Int(id)));
        ELSE
          Util.LogText(
            Fmt.F("Send [%s] failed: package version may not exist\n",
                     Fmt.Int(id)));
          res := SendResult.DeferIt;
        END;
    | PkgErr.E(ec) =>
        Util.LogText(
            Fmt.F("Send [%s] failed: %s\n", Fmt.Int(id), PkgErr.Msg(ec)));
        IF ec.head = PkgErr.NoSuchDir THEN
          WHILE qe.fwd # NIL DO
            TRY Enqueue(qe.package, qe.version,  qe.managedBy, qe.fwd.head);
            EXCEPT | PkgErr.E =>
            END;
            qe.fwd := qe.fwd.tail;
          END;
        ELSIF ec.head = PkgErr.NoRoomInFS OR
              ec.head = PkgErr.IOError OR
              ec.head = PkgErr.AccessViolation THEN
          res := SendResult.DeferIt;
        ELSIF ec.head = PkgErr.LockServerDown OR
              ec.head = PkgErr.SourceFailed OR
              ec.head = PkgErr.NoReplicas THEN
          res := SendResult.RetryIt;
        ELSE
          res := SendResult.DequeueIt;
        END;
    END;
    RETURN res;
  END SendOne;

PROCEDURE GetSiphonT(q: Queue) : Siphon.T RAISES {Thread.Alerted} =
  VAR  t: Siphon.T := NIL;
       e: PkgErr.TL;
  BEGIN
    LOOP
      e := NIL;
      TRY
        t := Siphon.New(q.site);
      EXCEPT
      | NetObj.Error(ec) => e := ec;
      | PkgErr.E(ec) => e := ec;
      END;
      LOCK mutex DO
        IF NOT PkgErrEq(e, q.linkStatus) THEN
          IF e = NIL THEN
            Util.LogText(Fmt.F("Site %s is now reachable\n", q.site));
          ELSE
            Util.LogText(Fmt.F("Site %s is unreachable: %s\n",
                                         q.site, PkgErr.Msg(e)));
          END;
          q.linkStatus := e;
          q.linkStatusChange := Time.Now();
        END;
      END;
      IF t # NIL THEN EXIT; END;
      Thread.Pause(FLOAT(SoonPause, LONGREAL));
    END;
    RETURN t;
  END GetSiphonT;

PROCEDURE PkgErrEq(a,b: PkgErr.TL) : BOOLEAN =
  BEGIN
    WHILE a # NIL DO
      IF b = NIL OR a.head # b.head THEN RETURN FALSE; END;
      a := a.tail; b := b.tail;
    END;
    RETURN (b=NIL);
  END PkgErrEq;

EXCEPTION NoSource((*conclusive*) BOOLEAN);

PROCEDURE PickAServer
    (pn: PN; v: Version; VAR (*OUT*) replica: TEXT): PackageObj.Source
    RAISES {NoSource} =
    (* returns always the correct instance *)
  VAR
    start, i: CARDINAL;
    testV: LockOps.Version;
    source: PackageObj.Source;
    reps: REF ARRAY OF TEXT;
    pkgT: PackageObj.T;
    conclusive: BOOLEAN := TRUE;
  BEGIN
    reps := Site.Get().replicas;
    start := ROUND(Time.Now()) MOD NUMBER (reps^);
    i := start;
    REPEAT
      INC (i);
      IF i = NUMBER (reps^) THEN i := 0;  END;
      TRY
        pkgT := PackageObj.New (reps[i]);
        source := pkgT.newSource (Param.SystemAuth, pn, testV);
        IF (testV.t = v.t) AND (testV.vn = v.vn) THEN
          replica := reps[i];
          RETURN source;
        END;
      EXCEPT
      | Thread.Alerted, NetObj.Error =>
          conclusive := FALSE;
      | PkgErr.E(ec) =>
         IF ec.head # PkgErr.NoSuchPackage AND ec.head # PkgErr.NoSuchDir THEN
           conclusive := FALSE;
         END;
      END;
    UNTIL (i = start);
    RAISE NoSource(conclusive);
  END PickAServer;

BEGIN
END Send.

