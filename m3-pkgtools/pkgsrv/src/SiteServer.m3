(* Copyright 1990 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* SiteServer.m3 *)
(* Last modified on Wed Apr 27 17:05:07 PDT 1994 by wobber *)

MODULE SiteServer;

IMPORT Atom, FileSys, Lex, OSError, RefList, IP, Rd, Text, TextRd, Sx;
IMPORT FloatMode, NetPath, Pathname, Thread, Site, NetPathRefTbl, TextList;

CONST
  DefaultConfigFN = "siphon.config";
  DefaultSiphonUser = "siphon";

VAR
  symSiteName: Atom.T;
  symLockServer: Atom.T;
  symSiphonServer: Atom.T;
  symReplicas: Atom.T;
  symBackups: Atom.T;
  symOwner: Atom.T;
  symDefaultRepository: Atom.T;
  symIPPort: Atom.T;
  symRemoteSites: Atom.T;
  symRoute: Atom.T;

  symRepositories: Atom.T;
  symExportDirs: Atom.T;

TYPE
  TextRefArray = REF ARRAY OF TEXT;
  TT = T OBJECT
    fn: TEXT;
    t: Site.T;
    r: RepInfo;
  OVERRIDES
    get := GetSite;
    repInfo := GetRepInfo;
  END;

PROCEDURE Init(fn: TEXT) : T RAISES {BadDB} =
  VAR t: TT;
  BEGIN
    IF fn = NIL THEN fn := DefaultConfigFN; END;
    t := NEW(TT, fn := fn);
    symSiteName := Atom.FromText ("site");
    symLockServer := Atom.FromText ("lockServer");
    symSiphonServer := Atom.FromText ("siphonServer");
    symReplicas := Atom.FromText ("replicaServers");
    symBackups := Atom.FromText ("backupServers");
    symOwner := Atom.FromText ("owner");
    symDefaultRepository := Atom.FromText ("defaultRepository");
    symIPPort := Atom.FromText ("ipPort");
    symRemoteSites := Atom.FromText ("remoteSites");
    symRoute := Atom.FromText ("route");
    symRepositories := Atom.FromText ("repositories");
    symExportDirs := Atom.FromText ("exports");
    t.t := GetSiteInner(t);
    t.r := NEW(RepInfo, mu := NEW(MUTEX),
          reps := NEW(NetPathRefTbl.Default).init(),
          exports := NEW(NetPathRefTbl.Default).init());
    EVAL GetRepInfoInner(t);
    RETURN t;
  END Init;

PROCEDURE GetSite (t: TT) : Site.T =
  BEGIN
    TRY
      RETURN GetSiteInner(t);
    EXCEPT
    | BadDB => RETURN t.t;
    END;
  END GetSite;

PROCEDURE GetSiteInner (t: TT) : Site.T RAISES {BadDB} =
  VAR l: RefList.T;
      site: Site.T;
      txt: TEXT;
  BEGIN
    l := DoReadFile(t.fn);
    site :=  NEW (Site.T,
          name := One (l, symSiteName, TRUE),
          lockserver := One (l, symLockServer, TRUE),
          siphonserver := One (l, symSiphonServer),
          replicas := Many (l, symReplicas),
          backupHosts := Many (l, symBackups),
          owner := One (l, symOwner),
          defaultRepository := NIL,
          ipPort := CheckPort(One (l, symIPPort)),
          foreignSites := ParseForeignSites(l));
    txt := One (l, symDefaultRepository);
    IF txt # NIL THEN
      TRY
        site.defaultRepository := NetPath.FromText(txt);
      EXCEPT
      | NetPath.Invalid => RAISE BadDB("bad default repository");
      END;
    END;
    IF site.replicas = NIL THEN
      site.replicas :=  NEW (TextRefArray, 1);
      site.replicas[0] := site.lockserver;
    END;
    IF site.owner = NIL THEN site.owner := DefaultSiphonUser;  END;
    IF site.siphonserver = NIL THEN
      site.siphonserver := site.lockserver;
    END;
    RETURN site;
  END GetSiteInner;

PROCEDURE GetRepInfo(t: TT) : RepInfo =
  BEGIN
    TRY
      RETURN GetRepInfoInner(t);
    EXCEPT
    | BadDB => RETURN t.r;
    END;
  END GetRepInfo;

EXCEPTION MissingRefList;
EXCEPTION MissingText;
EXCEPTION BadFN;

PROCEDURE GetRepInfoInner(t: TT) : RepInfo RAISES {BadDB} =
  VAR reps, exports, l, ll, lll: RefList.T;
      np: NetPath.T;
      r: RepData;
  BEGIN
    l := DoReadFile(t.fn);
    reps := Assoc (l, symRepositories);
    exports := Assoc (l, symExportDirs);
    TRY
      IF reps # NIL THEN
        reps := reps.tail;   (* skip past symbol *)
        WHILE reps # NIL DO
          ll := NextRefList(reps);
          np := NetPath.FromText(NextText(ll));
          r := NEW(RepData, fn := NextFN(ll), vols := NIL);
          lll := NextRefList(ll, TRUE);
          WHILE lll # NIL DO
            r.vols := TextList.Cons(NextFN(lll), r.vols);
          END;
          LOCK t.r.mu DO EVAL t.r.reps.put(np, r); END;
        END;
      END;
      IF exports # NIL THEN
        exports := exports.tail;   (* skip past symbol *)
        WHILE exports # NIL DO
          ll := NextRefList(exports);
          np := NetPath.FromText(NextText(ll));
          LOCK t.r.mu DO EVAL t.r.exports.put(np, NextFN(ll, TRUE)); END;
        END;
      END;
    EXCEPT
    | NetPath.Invalid =>
        RAISE BadDB("Invalid NetPath.T in repository or export spec.");
    | MissingRefList =>
        RAISE BadDB("Missing RefList.T in repository or export spec.");
    | MissingText =>
        RAISE BadDB("Missing TEXT in repository or export spec.");
    | BadFN =>
        RAISE BadDB("Bad file name in repository or export spec.");
    END;
    RETURN t.r;
  END GetRepInfoInner;

PROCEDURE NextRefList(VAR l: RefList.T; nilOK: BOOLEAN := FALSE) : RefList.T
     RAISES {MissingRefList} =
  VAR res: RefList.T;
  BEGIN
    IF (l = NIL) OR (l.head = NIL) THEN
      IF nilOK THEN RETURN NIL; END;
      RAISE MissingRefList;
    END;
    res := l.head;
    IF TYPECODE(res) # TYPECODE(RefList.T) THEN RAISE MissingRefList; END;
    l := l.tail;
    RETURN res;
  END NextRefList;

PROCEDURE NextFN(VAR l: RefList.T; nilOK: BOOLEAN := FALSE) : TEXT
    RAISES {BadFN, MissingText} =
  VAR res: TEXT;
  BEGIN
    res := NextText(l, nilOK);
    IF res # NIL THEN
      IF NOT (Pathname.Valid(res) AND Pathname.Absolute(res)) THEN
        RAISE BadFN;
      END;
    END;
    RETURN res;
  END NextFN;

PROCEDURE NextText(VAR l: RefList.T; nilOK: BOOLEAN := FALSE) : TEXT
    RAISES {MissingText} =
  VAR res: TEXT;
  BEGIN
    IF (l = NIL) OR (l.head = NIL) THEN
      IF nilOK THEN RETURN NIL; END;
      RAISE MissingText;
    END;
    res := l.head;
    IF TYPECODE(res) # TYPECODE(TEXT) THEN RAISE MissingText; END;
    l := l.tail;
    RETURN res;
  END NextText;

PROCEDURE ParseForeignSites(l: RefList.T) : REF ARRAY OF Site.Remote 
     RAISES {BadDB} =
  VAR fl, sl: RefList.T;
      r: REFANY; rems: REF ARRAY OF Site.Remote;
  BEGIN
    fl := Assoc (l, symRemoteSites);
    IF fl = NIL OR RefList.Length(fl) < 2 THEN RETURN NIL; END;
    fl := fl.tail;
    rems := NEW(REF ARRAY OF Site.Remote, RefList.Length(fl));
    FOR i := 0 TO LAST(rems^) DO
      r := fl.head; fl := fl.tail;
      IF r = NIL OR TYPECODE(r) # TYPECODE(RefList.T) THEN
        RAISE BadDB("Bad foreign site spec");
      END;
      sl := r;
      rems[i].name := One (sl, symSiteName, TRUE);
      rems[i].siphonserver := One (sl, symSiphonServer, TRUE);
      rems[i].ipPort := CheckPort(One (sl, symIPPort));
      rems[i].route := Many(sl, symRoute);
    END;
    RETURN rems;
  END ParseForeignSites;

PROCEDURE DoReadFile (fn: TEXT): RefList.T RAISES {BadDB}  =
  VAR rd: Rd.T; wrl: REFANY;
    <* FATAL Thread.Alerted *>
  BEGIN
    TRY
      rd := FileSys.OpenRead(fn);
      TRY
        wrl := Sx.Read (rd);
      FINALLY
        Rd.Close(rd);
      END;
      IF TYPECODE (wrl) # TYPECODE (RefList.T) THEN
        RAISE BadDB ("Configuration Sx is wrong type");
      END;
    EXCEPT
    | Sx.ReadError(eArg) =>
          RAISE  BadDB("Bad config file: " & eArg);
    | Rd.EndOfFile, Rd.Failure, OSError.E =>
          RAISE BadDB ("Problem reading configuration file");
    END;
    RETURN wrl;
  END DoReadFile;

PROCEDURE One(assoc: RefList.T; sym: Atom.T;
  insist: BOOLEAN := FALSE) : TEXT RAISES {BadDB} =
  VAR one: TEXT;
  VAR l: RefList.T;
  VAR r: REFANY;
  BEGIN
    l := Assoc (assoc, sym);
    IF (l = NIL) AND NOT insist THEN RETURN NIL;  END;
    IF RefList.Length (l) = 2 THEN
      r := l.tail.head;
      IF TYPECODE (r) = TYPECODE (TEXT) THEN
        one := r;
        IF NOT Text.Empty (one) THEN RETURN one; END;
      END;
    END;
    RAISE BadDB (SymName(assoc,sym) & " is missing or empty or multi-valued");
  END One;

PROCEDURE Many (
    assoc: RefList.T; sym: Atom.T;
    insist: BOOLEAN := FALSE) : TextRefArray RAISES {BadDB} =
  VAR many: TextRefArray := NIL;
  VAR len: INTEGER;
  VAR r: REFANY;
  VAR l: RefList.T;
  BEGIN
    l := Assoc (assoc, sym);
    IF (l = NIL) AND  NOT insist THEN RETURN NIL;  END;
    len := RefList.Length (l) - 1;
    IF len > 0 THEN
      l := l.tail;
      many :=  NEW (TextRefArray, len);
      FOR j := 0 TO len - 1 DO
        r := l.head; l := l.tail;
        IF TYPECODE (r) # TYPECODE (TEXT) THEN
          RAISE BadDB (SymName (assoc, sym) & " has wrong sx type");
        ELSE
          many[j] := r;
          IF Text.Empty (many[j]) THEN
            RAISE BadDB (SymName (assoc, sym) & " has an empty element");
          END;
        END;
      END;
      RETURN many;
    END;
    RAISE BadDB (SymName (assoc, sym) & " is missing or empty");
  END Many;

PROCEDURE SymName (assoc: RefList.T; qual: Atom.T := NIL): TEXT =
  VAR t: TEXT := assoc.head;
  BEGIN
    IF qual # NIL THEN t := t & "." & Atom.ToText(qual);  END;
    RETURN t;
  END SymName;

EXCEPTION BadPort;

PROCEDURE CheckPort(text: TEXT) : IP.Port RAISES {BadDB} =
  VAR
    rd: Rd.T;
    res: INTEGER;
    <* FATAL Rd.Failure, Thread.Alerted *>
  BEGIN
    IF text = NIL THEN RETURN IP.NullPort; END;
    rd := TextRd.New(text);
    TRY
      res := Lex.Int(rd);
      IF res < 0 OR res > LAST(IP.Port) THEN RAISE BadPort; END;
      RETURN res;
    EXCEPT
    | BadPort, Lex.Error, FloatMode.Trap =>
        RAISE BadDB ("Bad IP port: " & text);
    END;
  END CheckPort;
  
PROCEDURE Assoc(l: RefList.T; sym: Atom.T) : RefList.T =
  VAR ll: RefList.T;
  BEGIN
    WHILE l # NIL DO
      IF TYPECODE(l.head) = TYPECODE(RefList.T) THEN
        ll := l.head;
        IF TYPECODE(ll.head) = TYPECODE(Atom.T) THEN
          IF sym = NARROW(ll.head, Atom.T) THEN RETURN ll; END;
        END;
      END;
      l := l.tail;
    END;
    RETURN NIL;
  END Assoc;

BEGIN
END SiteServer.




