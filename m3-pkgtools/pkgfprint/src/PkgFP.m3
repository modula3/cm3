(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

(* Last modified on Thu Mar 10 13:56:32 PST 1994 by wobber  *)
(*      modified on Wed Feb 10 04:52:42 PST 1993 by prusker *)
(*      modified on Thu Mar  9 12:07:1x1 PST 1989 by glassman *)

MODULE PkgFP EXPORTS Main;


IMPORT Fingerprint, Text, Params, PackageObj, LockOps, Site,
        Wr, Fmt, Stdio, Time, Thread, NetObj, Pathname,
        PkgErr, Rd, AtomList, Process, NetPath, Export, ExportSort;

IMPORT TextF;

FROM Stdio IMPORT stdout;

TYPE
  PN = NetPath.PN;
  Version = LockOps.Version;

TYPE
  Kind = {Normal, Force, Compare, Print, Raz};

<* FATAL Thread.Alerted *>

CONST myAuth = "pkgfp";

VAR site: Site.T;
    replica: TEXT := NIL;
    verbose := FALSE;

PROCEDURE Run() =
  VAR
    kind: Kind;
    pns := ParseCommandLine(kind);
    reps: LockOps.DirList;
    enum: LockOps.EnumList;
  BEGIN
    TRY
      IF pns # NIL THEN
        FOR i := 0 TO LAST(pns^) DO
          TRY
            VAR entry := LockOps.GetEntry(pns[i]); BEGIN
              ActOnOnePkg(pns[i], entry^, kind);
            END;
          EXCEPT
          | PkgErr.E(ec) =>
              IF ec.head = PkgErr.NoSuchDir THEN
                Output("No such repository: " &
                                  NetPath.ToText(pns[i].dir) & "\n");
              ELSIF ec.head = PkgErr.NoSuchPackage THEN
                Output("No such package: " &
                                  NetPath.PNToText(pns[i]) & "\n");
              ELSE RAISE PkgErr.E(ec);
              END;
          END;
        END;
      ELSE
        reps := LockOps.EnumerateDirs();
        IF reps # NIL THEN
          FOR i := 0 TO LAST(reps^) DO
            enum := LockOps.Enumerate(reps[i], NIL, FALSE, FALSE, FALSE);
            IF enum # NIL THEN
              FOR j := 0 TO LAST(enum^) DO
                ActOnOnePkg(PN{reps[i], enum[j].arc}, enum[j].e, kind);
              END;
            END;
          END;
        END;
      END;
    EXCEPT
    | PkgErr.E(lockEC) =>
        Output("Error: " & PkgErr.Msg(lockEC) & "\n");
    END;
    Process.Exit(1);
  END Run;

PROCEDURE ActOnOnePkg(pn: PN; VAR entry: LockOps.Entry; kind: Kind) =
  VAR
    header := NetPath.PNToText(pn) & "(" & Fmt.Int(entry.curVN) & "): ";
    src: PackageObj.Source;
    fp: Fingerprint.T;
    r: ReplicaEnum;
  BEGIN
    TRY
        (* check if package is to be fingerprinted *)
      CASE kind OF
      | Kind.Compare =>
          IF entry.fp = Fingerprint.Zero THEN
            Output(header & "not fingerprinted\n");
            RETURN;
          END;
      | Kind.Force =>
      | Kind.Normal =>
          IF entry.fp # Fingerprint.Zero THEN RETURN; END;
      | Kind.Print =>
          IF entry.fp # Fingerprint.Zero THEN
	    Output(header & FPToText(entry.fp) & "\n");
          ELSE
            Output(header & "not fingerprinted\n");
          END;
          RETURN;
      | Kind.Raz =>
          IF entry.fp # Fingerprint.Zero THEN
            LockOps.SetFingerprint(myAuth, pn,
               LockOps.Version{entry.instance, entry.curVN},
               Fingerprint.Zero);
          END;
          RETURN;
      END;

        (* checks if replica has current version *)
      VAR
        lastEC: AtomList.T := NIL;
        t: PackageObj.T;
        vnew: Version;
      BEGIN
        r := NewReplicaEnum();
        LOOP
          TRY
            src := NIL;
            t := r.next();
            IF t = NIL THEN EXIT; END;
            src := t.newSource(myAuth, pn, vnew);
            IF vnew = Version{entry.instance, entry.curVN} THEN EXIT; END;
          EXCEPT
          | PkgErr.E(ec) => lastEC := ec;
          | NetObj.Error(ec) => lastEC := ec;
          END;
        END;
        IF src = NIL THEN
          IF lastEC # NIL THEN
            RAISE PkgErr.E(lastEC);
          ELSE
            Output(header & "version not found\n");
            RETURN;
          END;
        END;
      END;

        (* fingerprints package *)
      IF verbose THEN
        Output(header & "starting fingerprint (" & r.last & ")\n");
      END;

      fp := FPFromSrc(src);
      IF fp = Fingerprint.Zero THEN
        (* null package ... fingerprint package name *)
        fp := FPFromText(fp, pn.arc);
      END;

        (* set fingerprint *)
      IF (kind = Kind.Force OR kind = Kind.Normal) AND (fp # entry.fp) THEN
        VAR v := Version{entry.instance, entry.curVN}; BEGIN
          LockOps.SetFingerprint(myAuth, pn, v, fp);
        END;
      END;

        (* see if the fingerprint changed *)
      IF entry.fp # Fingerprint.Zero AND fp # entry.fp THEN
        Output(header & "old=" & FPToText(entry.fp)
	      	               & " new=" & FPToText(fp)
                               & " (" & r.last & ")\n");
      ELSE
        IF kind # Kind.Compare THEN
          Output(header & FPToText(fp) & " (" & r.last & ")\n");
        END;
      END;

    EXCEPT
    | NetObj.Error(ec) =>
        Output(header & PkgErr.Msg(ec) & "\n");
    | PkgErr.E(ec) =>
        Output(header & PkgErr.Msg(ec) & "\n");
    END;
  END ActOnOnePkg;

PROCEDURE FPFromSrc(src: PackageObj.Source) : Fingerprint.T
             RAISES {NetObj.Error, PkgErr.E} =
  VAR e := src.enum();
      ex := src.links();
      fp := Fingerprint.OfEmpty;
  BEGIN
    IF ex # NIL THEN
      ExportSort.Sort(ex^, Export.Compare);
      FOR i := 0 TO LAST(ex^) DO
        fp := FPFromText(fp, NetPath.ToText(ex[i].link));
        fp := FPFromText(fp, NetPath.ToText(ex[i].referent));
      END;
    END;
    IF verbose THEN Output("   ... exports - " & FPToText(fp) & "\n"); END;
    RETURN FPFromDir(fp, src, e.dir, NIL);
  END FPFromSrc;

PROCEDURE FPFromDir(fp: Fingerprint.T;
                    src: PackageObj.Source; 
                    de: PackageObj.DirEnum; path: TEXT) : Fingerprint.T
             RAISES {NetObj.Error, PkgErr.E} =
  BEGIN
    WHILE de # NIL DO
      VAR e: PackageObj.DirElem := de.head; BEGIN
        fp := FPFromText(fp, e.arc);
        CASE e.info.type OF
        | PackageObj.FileType.Dir =>
            fp := FPFromDir(fp, src, e.children,
                      Pathname.Join(path, e.arc, NIL));
        | PackageObj.FileType.Normal =>
            fp := FPFromText(fp, Fmt.Int(ROUND(e.info.date) + e.info.perm));
            fp := FPFromFile(fp, src, Pathname.Join(path, e.arc, NIL));
        | PackageObj.FileType.SLink =>
            fp := FPFromText(fp, e.referent);
        ELSE
        END;
        IF verbose THEN
          Output("   ... " & Pathname.Join(path, e.arc, NIL)
                           & " - " & FPToText(fp) & "\n");
        END;
      END;
      de := de.tail;
    END;
    RETURN fp;
  END FPFromDir;

PROCEDURE FPFromFile(fp: Fingerprint.T;
                     src: PackageObj.Source; path: TEXT) : Fingerprint.T
             RAISES {NetObj.Error, PkgErr.E} =
  VAR rd: Rd.T;
      nch: CARDINAL;
      buff: ARRAY [0..1023] OF CHAR;
    <* FATAL NetPath.Invalid *>
  BEGIN
    rd := src.pullFile(NetPath.FromRelFN(path));
    TRY
      TRY
        LOOP
          nch := Rd.GetSub(rd, buff);
          IF nch = 0 THEN EXIT; END;
          fp := Fingerprint.FromChars(SUBARRAY(buff, 0, nch), fp);
        END;
      FINALLY
        TRY Rd.Close(rd); EXCEPT Rd.Failure => END;
      END;
    EXCEPT
    | Rd.Failure(ec) => RAISE PkgErr.E(ec);
    END;
    RETURN fp;
  END FPFromFile;

PROCEDURE FPFromText(fp: Fingerprint.T; t: TEXT) : Fingerprint.T =
  BEGIN
    RETURN Fingerprint.FromChars(t^, fp);
  END FPFromText;

TYPE
  ReplicaEnum = OBJECT
    done: BOOLEAN := FALSE;
    this, start: CARDINAL;
    last: TEXT := NIL;
  METHODS
    next(): PackageObj.T RAISES {PkgErr.E, NetObj.Error} := NextReplica;
  END;

PROCEDURE NewReplicaEnum() : ReplicaEnum =
  VAR n := ROUND(Time.Now()) MOD NUMBER(site.replicas^);
  BEGIN
    RETURN NEW(ReplicaEnum, this := n, start := n);
  END NewReplicaEnum;

PROCEDURE NextReplica(t: ReplicaEnum) : PackageObj.T
       RAISES {PkgErr.E, NetObj.Error} =
  VAR repl: TEXT;
  BEGIN
    IF t.done THEN RETURN NIL; END;
    IF replica # NIL THEN
      repl := replica;
      t.done := TRUE;
    ELSE
      repl := site.replicas^[t.this];
      INC(t.this);
      IF t.this = NUMBER(site.replicas^) THEN t.this := 0; END;
      IF t.this = t.start THEN t.done := TRUE; END;
    END;
    t.last := repl;
    RETURN PackageObj.New(repl);
  END NextReplica;

PROCEDURE FPToText(fp: Fingerprint.T) : TEXT =
  BEGIN
    RETURN Fmt.FN(
      "%02s%02s%02s%02s.%02s%02s%02s%02s",
       ARRAY OF TEXT{
          Fmt.Unsigned(fp.byte[0], 16),
          Fmt.Unsigned(fp.byte[1], 16),
          Fmt.Unsigned(fp.byte[2], 16),
          Fmt.Unsigned(fp.byte[3], 16),
          Fmt.Unsigned(fp.byte[4], 16),
          Fmt.Unsigned(fp.byte[5], 16),
          Fmt.Unsigned(fp.byte[6], 16),
          Fmt.Unsigned(fp.byte[7], 16)});
  END FPToText;

PROCEDURE Usage() =
  BEGIN
    Output("Usage:   pkgfp [-vfcpz] [-r replicaName] [packages ...]\n");
    Process.Exit(1);
  END Usage;

PROCEDURE ParseCommandLine(VAR (*out*) kind: Kind) : REF ARRAY OF PN  = 
  VAR narg := 1;
      arg: TEXT;
      res: REF ARRAY OF PN;
  BEGIN
    kind := Kind.Normal;
    WHILE narg < Params.Count DO
      arg := Params.Get(narg);
      IF Text.Empty(arg) OR Text.GetChar(arg, 0) # '-' THEN EXIT; END;
      INC(narg);
      FOR i := 1 TO Text.Length(arg) - 1 DO
	VAR nk := kind; argx: TEXT; BEGIN
          CASE Text.GetChar(arg, i) OF
	  | 'v' => verbose := TRUE;
          | 'c' => nk := Kind.Compare;
          | 'p' => nk := Kind.Print;
	  | 'f' => nk := Kind.Force;
	  | 'z' => nk := Kind.Raz;
          | 'r' =>
              IF narg = Params.Count OR replica # NIL THEN Usage(); END;
              argx := Params.Get(narg);
              INC(narg);
              FOR j := 0 TO LAST(site.replicas^) DO
                IF Text.Equal(argx, site.replicas[j]) OR
                     Text.Equal(argx, StripRepName(site.replicas[j])) THEN
                  replica := site.replicas[j];
                END;
              END;
              IF replica = NIL THEN
                Output("Replica \"" & argx & "\" is unknown\n");
                Process.Exit(1);
              END;
          ELSE
            Usage();
	  END;
          IF kind # Kind.Normal AND nk # kind THEN Usage(); END;
          kind := nk;
        END;
      END;
    END;
    IF narg = Params.Count THEN RETURN NIL; END;
    res := NEW(REF ARRAY OF PN, Params.Count-narg);
    FOR i := 0 TO LAST(res^) DO
      TRY
        res[i] := NetPath.PNFromText(Params.Get(narg));
      EXCEPT
      | NetPath.Invalid =>
          Output("Bad package argument: " & Params.Get(narg) & "\n");
          Process.Exit(1);
      END;
      IF res[i].dir = NIL THEN
        res[i].dir := site.defaultRepository;
      END;
      INC(narg);
    END;
    RETURN res;
  END ParseCommandLine;

PROCEDURE StripRepName(r: TEXT) : TEXT =
  VAR n := Text.FindChar(r, '.');
  BEGIN
    IF n <= 0 THEN RETURN r; END;
    RETURN Text.Sub(r, 0, n);
  END StripRepName;

PROCEDURE Output(t: TEXT) =
  BEGIN
    TRY
      Wr.PutText(stdout, t);
      Wr.Flush(stdout);
    EXCEPT
    | Wr.Failure =>
        Process.Exit(1);
    END;
  END Output;

BEGIN
  TRY
    site := Site.Init();
  EXCEPT
  | Site.Error(ec) =>
      Output("Site.Error(\"" & Site.ErrMsg(ec) & "\")\n");
      Process.Exit(1);
  END;
  Run();
END PkgFP.
