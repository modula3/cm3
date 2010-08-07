(*---------------------------------------------------------------------------*)
MODULE PkgBase;

IMPORT TextList, TextListSort, TextTextTbl;
IMPORT Thread, Text, Rd, Pathname, FileRd;
IMPORT PkgError, PkgKindData, PkgKindDataTbl, MsgX, MsgIF, FSUtils, FileInfo,
       PathRepr, TextUtils;
FROM TextReadingUtils IMPORT GetToken, GetTokenOrString;

(*---------------------------------------------------------------------------*)
REVEAL 
  T = Public BRANDED "PkgBase 0.1" OBJECT
    tab      : PkgKindDataTbl.T;
    kinds    : TextList.T; (* needed to keep the order of declarations *)
    hostType : TEXT;
    osType   : TEXT;
    cache    : FileInfo.T;
    msgif    : MsgIF.T;
    platformSuffix : TEXT := NIL;
  METHODS
    explicitKind(p : Pathname.T; VAR kind : Kind) : BOOLEAN := ExplicitKind;
  OVERRIDES
    oldInit := OldInit;
    init := Init;
    setCache := SetCache;
    addDefs := AddDefs;
    kindDefined := KindDefined;
    kindList := KindList;
    getAction := GetAction;
    isKind := IsKind;
    kindFound := KindFound;
    createEmptyPkg := CreateEmptyPkg;
    ensurePkgExists := EnsurePkgExists;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE OldInit(self : T; hosttype, ostype : TEXT; fc : FileInfo.T := NIL;
                  msgif : MsgIF.T := NIL) : T =
  BEGIN
    self.msgif := msgif;
    self.kinds := NIL;
    self.tab := NEW(PkgKindDataTbl.Default).init();
    self.hostType := hosttype;
    self.osType   := ostype;
    self.cache := fc;
    self.platformSuffix := NIL;
    RETURN self;
  END OldInit; 

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T; env : TextTextTbl.T; fc : FileInfo.T := NIL; 
               msgif : MsgIF.T := NIL) : T =
  BEGIN
    self.msgif := msgif;
    self.kinds := NIL;
    self.tab := NEW(PkgKindDataTbl.Default).init();
    IF NOT env.get("tpc-hosttype", self.hostType) AND
       NOT env.get("tpc-hosttype-default", self.hostType) THEN
      MsgX.Error2(msgif, "PkgBase.Init()", "tpc-hosttype undefined");
      self.hostType := "unknown";
    END;
    IF NOT env.get("tpc-ostype", self.osType) AND
       NOT env.get("tpc-ostype-default", self.osType) THEN
      MsgX.Error2(msgif, "PkgBase.Init()", "tpc-ostype undefined");
      self.osType := "unknown";
    END;
    MsgX.D2(msgif, "PkgBase.Init()", 
            "platform " & self.hostType & "-" & self.osType);
    IF env.get("platform-suffix", self.platformSuffix) OR 
       env.get("platform-suffix-default", self.platformSuffix) THEN
      MsgX.D2(msgif, "PkgBase.Init()", 
              "platform-suffix " & self.platformSuffix);
    ELSE
      (* platform suffix may be undefined *)
      MsgX.D2(msgif, "PkgBase.Init()", "no platform-suffix");
      self.platformSuffix := NIL;
    END;
    self.cache := fc;
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE SetCache(self : T; fc : FileInfo.T) =
  VAR 
    iter  : PkgKindDataTbl.Iterator := self.tab.iterate();
    atab  : PkgKindData.T;
    k     : Kind;
  BEGIN
    self.cache := fc;
    WHILE iter.next(k, atab) DO
      atab.setCache(fc);
    END;
  END SetCache;

(*---------------------------------------------------------------------------*)
PROCEDURE KindList (self : T) : TextList.T =
  VAR 
    l     : TextList.T := NIL;
  BEGIN
    IF self.kinds # NIL THEN
      l := TextListSort.Sort(self.kinds);
    END;
    RETURN l;
  END KindList; 

(*---------------------------------------------------------------------------*)
PROCEDURE AddDefs(self : T; rd : Rd.T) : BOOLEAN =
  VAR
    currentKind  :  TEXT := "undefined";
    currentTable :  PkgKindData.T := NIL;
    actToken :  TEXT;
    okay     := TRUE;

  (*-------------------------------------------------------------------------*)
  PROCEDURE ParsePkgKind() : BOOLEAN 
    RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} = 
    VAR
      okay  : BOOLEAN := TRUE;
      exit1 : BOOLEAN := FALSE;
      exit2 : BOOLEAN := FALSE;
      arg   : TEXT;
      pk    : PkgKindData.PredKind;
      actionName, actionCommands, kindName, tmp : TEXT;
    BEGIN
      currentKind := GetToken(rd);
      currentTable.setName(currentKind);
      IF NOT TextList.Member(self.kinds, currentKind) THEN
        self.kinds := TextList.Append(self.kinds, TextList.List1(currentKind));
      END;
      (* parse the predicate *)
      actToken := GetToken(rd);
      WHILE NOT exit1 DO
        IF Text.Equal(actToken, "action") THEN
          exit1 := TRUE;
        ELSIF Text.Equal(actToken, "pkgkind") THEN
          exit1 := TRUE;
        ELSIF Text.Equal(actToken, "inherit") THEN
          actionName := GetTokenOrString(rd);
          kindName := GetTokenOrString(rd);
          IF Text.Equal(actionName, "actions") THEN
            IF currentTable.getAction("inherit-actions", tmp) THEN
              EVAL currentTable.putAction("inherit-actions", tmp & "," & 
                kindName);
            ELSE
              EVAL currentTable.putAction("inherit-actions", kindName);
            END;
          ELSIF Text.Equal(actionName, "predicates") THEN
            EVAL currentTable.putAction("inherit-predicates", kindName);
          ELSE
            okay := FALSE;
            MsgX.Error2(self.msgif, "PkgBase.ParsePkgKind()",
                        "invalid inherit directive: " & actionName &
                        " " & kindName & " before " & actToken);
          END;
          actToken := GetToken(rd);
        ELSIF Text.Equal(actToken, "has") OR Text.Equal(actToken, "and") THEN
          VAR
            op := GetToken(rd);
            arg := GetTokenOrString(rd);
          BEGIN
            IF Text.Equal(op, "dir") THEN
              pk := PkgKindData.PredKind.Dir;
              arg := PathRepr.Native(arg);
            ELSIF Text.Equal(op, "file") THEN
              pk := PkgKindData.PredKind.File;
              arg := PathRepr.Native(arg);
            ELSIF Text.Equal(op, "match") THEN
              pk := PkgKindData.PredKind.Match;
            ELSIF Text.Equal(op, "notdir") OR Text.Equal(op, "nodir") THEN
              pk := PkgKindData.PredKind.NoDir;
              arg := PathRepr.Native(arg);
            ELSIF Text.Equal(op, "notfile") OR Text.Equal(op, "nofile") THEN
              pk := PkgKindData.PredKind.NoFile;
              arg := PathRepr.Native(arg);
            ELSIF Text.Equal(op, "notmatch") OR Text.Equal(op, "nomatch") THEN
              pk := PkgKindData.PredKind.NoMatch;
            ELSE
              okay := FALSE;
              MsgX.Error2(self.msgif, "PkgBase.ParsePkgKind()", 
                          "found " & actToken & 
                          " instead of dir|file|match|nodir|nofile|nomatch");
            END;
            currentTable.addCondition(pk, arg);
          END;
          actToken := GetToken(rd);
        ELSIF Text.Equal(actToken, "platform") THEN
          arg := GetTokenOrString(rd);
          pk := PkgKindData.PredKind.Platform;
          currentTable.addCondition(pk, arg);
          actToken := GetToken(rd);
        ELSIF Text.Equal(actToken, "hosttype") THEN
          arg := GetTokenOrString(rd);
          pk := PkgKindData.PredKind.HostType;
          currentTable.addCondition(pk, arg);
          actToken := GetToken(rd);
        ELSIF Text.Equal(actToken, "ostype") THEN
          arg := GetTokenOrString(rd);
          pk := PkgKindData.PredKind.OSType;
          currentTable.addCondition(pk, arg);
          actToken := GetToken(rd);
        ELSIF Text.Length(actToken) > 0 AND 
          Text.GetChar(actToken, 0) = '#' THEN
          (* skip until end of line *)
          EVAL Rd.GetLine(rd);
          actToken := GetToken(rd);
        ELSE
          okay := FALSE;
          MsgX.Error2(self.msgif, "PkgBase.ParsePkgKind()",
                      "found " & actToken & 
                      " instead of action|pkgkind|has|and");
          actToken := GetToken(rd);
        END;
      END;
      (* parse the actions *)
      WHILE NOT exit2 DO
        IF Text.Equal(actToken, "action") THEN
          actionName := GetTokenOrString(rd);
          actionCommands := GetTokenOrString(rd);
          EVAL currentTable.putAction(actionName, actionCommands);
          actToken := GetToken(rd);
        ELSIF Text.Equal(actToken, "pkgkind") THEN
          exit2 := TRUE;
        ELSIF Text.Length(actToken) > 0 AND 
          Text.GetChar(actToken, 0) = '#' THEN
          (* skip until end of line *)
          EVAL Rd.GetLine(rd);
          actToken := GetToken(rd);
        ELSE
          okay := FALSE;
          actToken := GetToken(rd);
          MsgX.Error2(self.msgif, "PkgBase.ParsePkgKind()",
                      "found " & actToken & 
                      "instead of action|pkgkind");
        END;
      END;
      RETURN okay;
    END ParsePkgKind;

  (*-------------------------------------------------------------------------*)
  BEGIN (* AddDefs *)
    TRY
      actToken := GetToken(rd);
      WHILE NOT Rd.EOF(rd) DO
        IF Text.Equal(actToken, "pkgkind") THEN
          IF currentTable # NIL THEN
            EVAL self.tab.put(currentKind, currentTable);
          END;
          currentTable := NEW(PkgKindData.T).init(self.cache);
          currentKind  := "undefined";
          (* read next package kind definition *)
          okay := ParsePkgKind();
        ELSIF Text.Length(actToken) > 0 AND 
          Text.GetChar(actToken, 0) = '#' THEN
          (* skip until end of line *)
          EVAL Rd.GetLine(rd);
          actToken := GetToken(rd);
        ELSE
          okay := FALSE;
          actToken := GetToken(rd);
          MsgX.Error2(self.msgif, "PkgBase.ParsePkgKind()", 
                      "found " & actToken & "instead of pkgkind");
        END;
      END;
      IF currentTable # NIL THEN
        EVAL self.tab.put(currentKind, currentTable);
      END;
    EXCEPT
      Rd.EndOfFile   => 
      IF currentTable # NIL THEN
        EVAL self.tab.put(currentKind, currentTable);
      END;
    | Rd.Failure     => RETURN FALSE;
    | Thread.Alerted => RETURN FALSE;
    END;
    RETURN okay;
  END AddDefs;

(*---------------------------------------------------------------------------*)
PROCEDURE KindDefined(self : T; k : Kind) : BOOLEAN =
  VAR atab : PkgKindData.T;
  BEGIN
    RETURN self.tab.get(k, atab);
    (*
    RETURN (self.platformSuffix # NIL AND
            self.tab.get(k & self.platformSuffix) OR 
            self.tab.get(k, atab);
    *)
  END KindDefined;

(*---------------------------------------------------------------------------*)
PROCEDURE GetAction(self : T; k : Kind; a : Action) : CmdSeq =
  VAR 
    atab : PkgKindData.T;
    cmds : CmdSeq;
  BEGIN
    MsgX.D(self.msgif, "PkgBase.GetAction(" & k & ", " & a & ")");
    IF NOT self.tab.get(k, atab) THEN
      MsgX.D(self.msgif, " -> not found");
      RETURN NIL;
    END;
    IF NOT atab.getAction(a, cmds) THEN
      IF atab.getAction("inherit-actions", k) THEN
        MsgX.D(self.msgif, " -> inherit-actions " & k);
        WITH kseq = TextUtils.Split(k, ",") DO
          FOR i := 0 TO kseq.size() - 1 DO
            WITH pkind = kseq.get(i) DO
              MsgX.D(self.msgif, " -> searching in " & pkind);
              cmds := GetAction(self, pkind, a);
              IF cmds # NIL THEN
                MsgX.D(self.msgif, " -> " & cmds);
                RETURN cmds;
              END;
            END;
          END;
        END;
      ELSE
        MsgX.D(self.msgif, " -> not found");
        RETURN NIL;
      END;
    END;
    IF cmds = NIL THEN
      MsgX.D(self.msgif, " -> not found");
    ELSE
      MsgX.D(self.msgif, " -> " & cmds);
    END;
    RETURN cmds;
  END GetAction;

(*---------------------------------------------------------------------------*)
PROCEDURE IsKind(self : T; p : Pathname.T; k : Kind) : BOOLEAN =
  BEGIN
    RETURN IsKindI(self, p, k);
  END IsKind;

(*---------------------------------------------------------------------------*)
PROCEDURE IsKindI(self : T; p : Pathname.T; k : Kind; 
                  strict := TRUE) : BOOLEAN =
  VAR 
    atab  : PkgKindData.T;
    ekind : Kind;
    pkind : Kind;
    res   := TRUE;
  BEGIN
    MsgX.D(self.msgif, "    PkgBase.IsKind(" & p & ", " & k & ")");
    IF self.explicitKind(p, ekind) THEN
      RETURN Text.Equal(k, ekind);
    END;
    IF NOT self.tab.get(k, atab) THEN
      IF strict THEN
        MsgX.Error2(self.msgif, "PkgBase.IsKind()", "kind undefined: " & k);
      END;
      (* FIXME : RAISE AN EXCEPTION *)
      RETURN FALSE;
    END;
    IF atab.getAction("inherit-predicates", pkind) THEN
      MsgX.D(self.msgif, "    -> inherit-predicates " & pkind);
      res := IsKind(self, p, pkind);
    END;
    res := res AND atab.evalCondition(p, self.hostType, self.osType);
    IF res THEN
      MsgX.D(self.msgif, "    -> TRUE");
    ELSE
      MsgX.D(self.msgif, "    -> FALSE");
    END;
    RETURN res;
  END IsKindI;

(*---------------------------------------------------------------------------*)
PROCEDURE KindFound(self : T; p : Pathname.T; VAR k : Kind) : BOOLEAN =
  VAR 
    ekind : Kind;
    kt    : TEXT;
    act   : TextList.T;
    res   := TRUE;

  PROCEDURE TestKind(VAR k : Kind) : BOOLEAN =
    (* check for kind k with and without platform suffix if defined *)
    VAR res := FALSE;
    BEGIN
      MsgX.D(self.msgif, "  PkgBase.KindFound.TestKind(" & k & ")");
      IF self.platformSuffix # NIL THEN
        res := IsKindI(self, p, k & self.platformSuffix, strict := FALSE);
      END;
      IF res THEN
        k := k & self.platformSuffix;
      ELSE
        res := IsKindI(self, p, k, strict := FALSE);
      END;
      IF res THEN
        MsgX.D(self.msgif, "  kind = " & k);
      END;
      RETURN res;
    END TestKind;

  BEGIN
    (* check if any explicit kind is given for the package *)
    IF k # NIL THEN kt := k ELSE kt := "NIL"; END;
    MsgX.D(self.msgif, "PkgBase.KindFound(" & p & ", " & kt & ")");
    IF self.explicitKind(p, ekind) THEN
      MsgX.D(self.msgif, "testing explicit kind " & ekind);
      IF self.platformSuffix # NIL AND
         self.kindDefined(ekind & self.platformSuffix) THEN
        ekind := ekind & self.platformSuffix;
      END;
      IF NOT self.kindDefined(ekind) THEN
        MsgX.Error2(self.msgif, "PkgBase.KindFound()", "explicit kind " & 
          ekind & " is not defined");
      END;
      k := ekind;
      MsgX.D(self.msgif, "kind = " & ekind);
      RETURN TRUE;
    END;
    IF k # NIL THEN
      (* test the preferred kind first *)
      MsgX.D(self.msgif, "test preferred kind " & k);
      res := TestKind(k);
      IF res THEN
        MsgX.D(self.msgif, "preferred kind found: " & k);
        RETURN TRUE;
      END;
    END;
    (* test systematically for all package kinds *)
    act := self.kinds;
    WHILE act # NIL DO
      k := act.head;
      MsgX.D(self.msgif, " -> testing for kind " & k);
      res := TestKind(k);
      IF res THEN
        MsgX.D(self.msgif, "kind = " & k);
        RETURN TRUE;
      END;
      act := act.tail;
    END;
    MsgX.D(self.msgif, "kind not found");
    RETURN FALSE;
  END KindFound; 

(*---------------------------------------------------------------------------*)
PROCEDURE CreateEmptyPkg(self : T; p : Pathname.T; k : Kind) 
  RAISES {PkgError.E} =
  VAR 
    atab  : PkgKindData.T;
    pkind : Kind;
  BEGIN
    IF NOT self.tab.get(k, atab) THEN
      MsgX.Fatal2(self.msgif, "PkgBase.CreateEmptyPkg()", "kind undefined");
      RAISE PkgError.E("PkgBase.CreateEmptyPkg(): kind undefined");
    END;
    IF atab.getAction("inherit-predicates", pkind) THEN
      CreateEmptyPkg(self, p, pkind);
    ELSIF FSUtils.Exists(p) THEN
      RAISE PkgError.E("package " & p & " already exists")
    END;
    atab.createStructure(p);
  END CreateEmptyPkg;

(*---------------------------------------------------------------------------*)
PROCEDURE EnsurePkgExists(self : T; p : Pathname.T; k : Kind) 
  RAISES {PkgError.E} =
  VAR 
    atab  : PkgKindData.T;
    pkind : Kind;
  BEGIN
    IF NOT self.tab.get(k, atab) THEN
      MsgX.Fatal2(self.msgif, "PkgBase.EnsurePkgExists()", "kind undefined");
      RAISE PkgError.E("PkgBase.EnsurePkgExists(): kind undefined");
    END;
    IF atab.getAction("inherit-predicates", pkind) THEN
      EnsurePkgExists(self, p, pkind);
    END;
    atab.ensureStructureExists(p);
  END EnsurePkgExists; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExplicitKind(<*UNUSED*> self : T; p : Pathname.T; 
                       VAR k : Kind) : BOOLEAN =
  VAR 
    pn : Pathname.T;
    rd : FileRd.T;
  BEGIN
    pn := Pathname.Join(p, "PkgKind", NIL);
    IF FSUtils.IsFile(pn) THEN
      TRY
        rd := FileRd.Open(pn);
        k := GetTokenOrString(rd);
        Rd.Close(rd);
      EXCEPT ELSE
        k := "undefined";
        RETURN FALSE;
      END;
      RETURN TRUE;
    END;
    RETURN FALSE;
  END ExplicitKind;

(*---------------------------------------------------------------------------*)
BEGIN
END PkgBase.
