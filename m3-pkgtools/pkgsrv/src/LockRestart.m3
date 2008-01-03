(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* LockRestart.m3 *)
(* Last modified on Thu Feb  2 08:53:40 PST 1995 by kalsow *)
(*      modified on Tue May 25 11:09:26 PDT 1993 by wobber *)
(*      modified on Mon Nov 18 14:53:36 GMT+1:00 1991 by prusker *)

(* this module builds a lock database from scratch *)

MODULE LockRestart;

IMPORT Fmt, NetObj;
IMPORT PackageDB, PathMap, PSLib, PkgErr;
IMPORT LockOps, PackageObj;
IMPORT Time, Thread;

TYPE PN = LockOps.PN;

EXCEPTION AllDown;
<* FATAL Thread.Alerted *>

PROCEDURE Restart (): BOOLEAN =
  VAR
    v: LockOps.Version;
    en: LockOps.EnumList;
    e: PackageDB.Entry;
    ok: BOOLEAN;
    pn: PN;
    dirs: LockOps.DirList;
    <* FATAL AllDown *>
    <* FATAL PkgErr.E *>
  BEGIN
    IF repStates = NIL THEN InitReplicaStates ();  END;
    PSLib.LogIt ("Scanning lock database for dead transactions");
    dirs := PackageDB.EnumerateDirs();
    IF dirs = NIL THEN RETURN TRUE; END;
    FOR i := 0 TO LAST(dirs^) DO
      en := PackageDB.Enumerate (dirs[i], FALSE, FALSE, TRUE);
      IF en # NIL THEN
        FOR j := 0 TO LAST (en^) DO
          e := en[j].e;
          pn := PN{dirs[i], en[j].arc};
          v := GetMaxVersion(pn);
          ok := FALSE;
          IF v.t = e.instance THEN
            IF v.vn = e.pendVN THEN
                        (* new version actually got installed *)
              e.pendVN := LockOps.NullVN;
              e.curVN := v.vn;
              SetLockEntry (pn, e);
              ok := TRUE;
            ELSIF v.vn = e.curVN THEN
                        (* new version wasn't installed *)
              e.pendVN := LockOps.NullVN;
              SetLockEntry (pn, e);
              ok := TRUE;
            END;
          END;
          IF NOT ok THEN
            PSLib.LogIt ("LR: Can\'t reconcile " & PSLib.PkgText(pn));
          END;
        END;
      END;
    END;
    RETURN TRUE;
  END Restart;

PROCEDURE Recover() =
  VAR
    en: LockOps.EnumList;
    initReps, dirs: LockOps.DirList;
    <* FATAL PkgErr.E *>
  BEGIN
    initReps := PathMap.GetReps();
    PSLib.LogIt("LR: Rebuilding lock database");
    IF initReps # NIL THEN
      FOR i := 0 TO LAST (initReps^) DO
        TRY
          PackageDB.CheckDir (initReps[i]);
        EXCEPT
        | PkgErr.E =>
            TRY
              PackageDB.CreateDir (initReps[i]);
            EXCEPT
              | PkgErr.E =>
                  PSLib.LogIt("LR: initReps error with " &
                                    PSLib.PathText(initReps[i]));
            END;
        END;
      END;
    END;
    (* now we'll do a query all replicas on the entries we have *)
    dirs := PackageDB.EnumerateDirs();
    IF dirs = NIL THEN RETURN; END;
    FOR i := 0 TO LAST(dirs^) DO
      en := PackageDB.Enumerate (dirs[i], FALSE, FALSE, FALSE);
      TRY
        IF repStates = NIL THEN InitReplicaStates ();  END;
        IF en # NIL THEN ScanLocalEntries (dirs[i], en);  END;
      EXCEPT
        | AllDown =>
            PSLib.LogIt("LR: Failed - all replicas down");
      END;
    END;
  END Recover;

PROCEDURE ScanLocalEntries (dir: LockOps.Dir; en: LockOps.EnumList)
  RAISES {AllDown, Thread.Alerted} =
  VAR e: PackageDB.Entry; v: LockOps.Version;
      pn: PackageObj.PN;
  BEGIN
    FOR i := 0 TO LAST (en^) DO
      e := en[i].e;
      pn := PN{dir, en[i].arc};
      v := GetMaxVersion (pn);
      IF v.vn # LockOps.NullVN THEN
        IF v.t = e.instance THEN
          IF e.curVN # v.vn THEN
            e.lastVN := MAX (e.curVN, v.vn);
            e.curVN := v.vn;
            SetLockEntry (pn, e);
          END;
        ELSE
                    (* our local replicas have a different instance ! *)
                    (* protect ourself by installing OUR version *)
                    (* let the siphon admin work out the conflict *)
          e.instance := v.t;
          e.lastVN := v.vn;
          e.curVN := v.vn;
          SetLockEntry (pn, e);
        END;
      ELSE
        PSLib.LogIt("LR: No version of " & PSLib.PkgText(pn) & " was found");
      END;
    END;
  END ScanLocalEntries;

PROCEDURE SetLockEntry (pn: PN; VAR e: PackageDB.Entry) =
    <*FATAL PkgErr.E*>
  BEGIN
    PSLib.LogIt ("LR: SetLockEntry " & PSLib.PkgText(pn) & " ("
                   & Fmt.Int (e.instance) & "." & Fmt.Int (e.curVN)
                   & "), mgr=" & e.managedBy);
    e.lastModified := Time.Now();
    PackageDB.SetEntry (pn, e);
  END SetLockEntry;


(* restart isn't concurrent, don't need mutexes around these *)
VAR
  upReplicas: CARDINAL;
  repNames: REF ARRAY OF TEXT;
  repStates: REF ARRAY OF PackageObj.T;

PROCEDURE GetMaxVersion (pn: PN): LockOps.Version
    RAISES {AllDown, Thread.Alerted} =
  VAR v, nv: LockOps.Version;
  BEGIN
    IF repStates = NIL THEN InitReplicaStates ();  END;
    v.t := 0;
    v.vn := LockOps.NullVN;
    FOR i := 0 TO LAST (repStates^) DO
      TRY
        IF repStates[i] # NIL THEN
          nv := repStates[i].version(pn);
          IF (nv.t > v.t) OR (nv.vn > v.vn) THEN v := nv;  END;
        END;
      EXCEPT
        | NetObj.Error =>
            PSLib.LogIt ("LR: Call failed at replica " & repNames[i]);
            repStates[i] := NIL;
            IF upReplicas = 0 THEN RAISE AllDown;  END;
        | PkgErr.E =>
      END;
    END;
    RETURN v;
  END GetMaxVersion;

PROCEDURE InitReplicaStates () RAISES {AllDown, Thread.Alerted} =
  BEGIN
    upReplicas := 0;
    repNames := PSLib.GetReplicas ();
    repStates :=  NEW (REF ARRAY  OF PackageObj.T, NUMBER (repNames^));
    FOR i := 0 TO LAST (repNames^) DO
      TRY
        repStates[i] := PackageObj.New (repNames[i]);
        INC (upReplicas);
      EXCEPT
        | NetObj.Error, PkgErr.E =>
            PSLib.LogIt ("LR: Replica " & repNames[i] & " is not available");
            repStates[i] := NIL;
      END;
    END;
    IF upReplicas = 0 THEN RAISE AllDown;  END;
  END InitReplicaStates;

BEGIN
END LockRestart.
