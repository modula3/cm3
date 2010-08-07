(*--------------------------------------------------------------------------*)
MODULE FileInfo;

(*--------------------------------------------------------------------------*)
IMPORT 
  Fmt, FmtTime, Fingerprint, Text, TextSeq, Wr;
IMPORT 
  APN AS AbsPathn, APNSeq AS AbsPathnSeq, 
  FileStatus, APNStatusTbl AS AbsPathnStatTbl, 
  Dummy, TextHashTbl, APNHashTbl AS AbsPathnHashTbl,
  FileSystem, DirectoryTbl, SMsg AS Msg, MsgX, MsgIF, FindExpr;


(*--------------------------------------------------------------------------*)
REVEAL
  T = Public BRANDED Brand OBJECT
    base  : AbsPathn.T;
    cache : AbsPathnStatTbl.Default;
    dir   : DirectoryTbl.Default;
    msgif : MsgIF.T;
  OVERRIDES
    init := Init;
    getBase := GetBase;
    size := Size;
    getStatus := GetStatus;
    update := Update;
    updateRec := UpdateRec;
    updateFlat := UpdateFlat;
    getDir := GetDir;
    dirList := DirList;
    fingerprint := ComputeFingerprint;
    dump := Dump;
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE Init (sf : T; sizeHint : CARDINAL; base : AbsPathn.T;
                msgif : MsgIF.T := NIL) : T =
  BEGIN
    sf.msgif := msgif;
    sf.base := base;
    sf.cache := NEW(AbsPathnStatTbl.Default).init(sizeHint);
    sf.dir := NEW(DirectoryTbl.Default).init();
    RETURN sf
  END Init;

(*--------------------------------------------------------------------------*)
PROCEDURE GetBase (sf : T) : AbsPathn.T =
  BEGIN
    RETURN sf.base
  END GetBase;
  
(*--------------------------------------------------------------------------*)
PROCEDURE Size (sf : T) : CARDINAL =
  BEGIN
    RETURN sf.cache.size()
  END Size;
  
(*--------------------------------------------------------------------------*)
PROCEDURE GetStatus (sf : T; p : AbsPathn.T) : FileStatus.T =
  VAR res : FileStatus.T;
  BEGIN
    IF NOT sf.cache.get(p, res) THEN
      res := NEW(FileStatus.T);
      res.exists := FALSE
    END;
    RETURN res
  END GetStatus;

(*--------------------------------------------------------------------------*)
PROCEDURE Update (sf : T; READONLY p : AbsPathn.T) : FileStatus.T  =
  VAR res : FileStatus.T;
  BEGIN
    IF sf.base = NIL OR Text.Empty(sf.base.denotation()) OR
       p.isAbsolute() THEN
      res := FileSystem.Lookup(p);
    ELSE
      res := FileSystem.Lookup(AbsPathn.Join(sf.base, p, NIL));
    END;
    EVAL sf.cache.put(p, res);
    RETURN res  
  END Update;

(*--------------------------------------------------------------------------*)
PROCEDURE TextSeqToTextHashTbl(s : TextSeq.T) : TextHashTbl.T =
  VAR l : CARDINAL;
      res : TextHashTbl.T;
  
  BEGIN
    l := s.size();
    res := NEW(TextHashTbl.Default).init(l);
    FOR i := 0 TO l - 1 DO
      EVAL res.put(s.get(i), Dummy.dummyConst)
    END;
    RETURN res
  END TextSeqToTextHashTbl;


PROCEDURE AbsPathnSeqToAbsPathnHashTbl(s : AbsPathnSeq.T) : AbsPathnHashTbl.T =
  VAR l : CARDINAL;
      res :  AbsPathnHashTbl.T;
  
  BEGIN
    l := s.size();
    res := NEW(AbsPathnHashTbl.Default).init(l);
    FOR i := 0 TO l - 1 DO
      EVAL res.put(s.get(i), Dummy.dummyConst)
    END;
    RETURN res
  END AbsPathnSeqToAbsPathnHashTbl;

(*--------------------------------------------------------------------------*)
PROCEDURE UpdateRec 
  (sf : T; p : AbsPathn.T; ext : TextSeq.T; ignoreDirs : AbsPathnSeq.T;
   ignoreDirExpr : FindExpr.T := NIL; ignoreFileExpr : FindExpr.T := NIL) =

  VAR 
    e : TextHashTbl.T;
    iD : AbsPathnHashTbl.T; 
    pn : AbsPathn.T;
    dummy : Dummy.T;

  PROCEDURE UR (READONLY p : AbsPathn.T; READONLY stat : FileStatus.T) =
 
    VAR i : FileSystem.Iterator;
        n : AbsPathn.T;
        s : FileStatus.T;
        q : AbsPathn.T;
        l : AbsPathnSeq.T;
        b : TEXT;
        pn : AbsPathn.T;
        ignoreDir  : BOOLEAN;
        ignoreFile : BOOLEAN;
    BEGIN
      (* OutFileStatus(sf, p, stat); *)
      IF sf.base = NIL OR Text.Empty(AbsPathn.Denotation(sf.base)) OR
         p.isAbsolute() THEN
        pn := p;
      ELSE
        pn := AbsPathn.Join(sf.base, p, NIL);
      END;
      i := FileSystem.GetIterator(pn);
      l := NEW(AbsPathnSeq.T).init();
      WHILE FileSystem.NextWithStatus(i, n, s) DO 
        q := AbsPathn.Join(p, n, NIL);
        b := AbsPathn.Denotation(n);
        IF s.isDir THEN
          TRY
            ignoreDir := ignoreDirExpr # NIL AND ignoreDirExpr.test(b);
          EXCEPT ELSE
            ignoreDir := FALSE;
          END;
        ELSE
          ignoreDir := FALSE;
        END;
        IF s.isFile THEN
          TRY
            ignoreFile := ignoreFileExpr # NIL AND ignoreFileExpr.test(b);
          EXCEPT ELSE
            ignoreFile := FALSE;
          END;
        ELSE
          ignoreFile := FALSE;
        END;
        IF e = NIL OR e.size() = 0 OR e.get(AbsPathn.LastExt(n), dummy) OR 
          s.isDir THEN
          IF s.isDir AND NOT ignoreDir OR NOT ignoreFile THEN
            (* OutFileStatus(sf, q, s); *)
            EVAL sf.cache.put(q, s);
            l.addhi(q);
            (*
            IF sf.msgif = NIL OR sf.msgif.debugLevel > 2 THEN
              MsgX.D(sf.msgif, "FileInfo: updating " & q.denotation(), 
                     level :=3);
            END;
            *)
          END;
        END; 
        IF s.exists (*C*)AND s.isDir (*C*)AND NOT ignoreDir (*C*)AND 
           (iD = NIL OR NOT iD.get (n, dummy)) THEN
          UR(q, s)
        END
      END;
      EVAL sf.cache.put(p, stat);
      EVAL sf.dir.put(p, l);
      i.close()
    END UR;

  BEGIN
    (* Msg.T2("UpdateRec", p.denotation()); *)
    IF ext = NIL THEN
      e := NIL;
    ELSE
      IF extLast # NIL AND ext = extLast THEN
        e := eLast;
      ELSE
        e := TextSeqToTextHashTbl(ext);
        eLast := e;
        extLast := ext;
      END;
    END;
    IF ignoreDirs = NIL THEN
      iD := NIL;
    ELSE
      IF ignoreDirsLast # NIL AND ignoreDirs = ignoreDirsLast THEN
        iD := iDLast;
      ELSE
        iD := AbsPathnSeqToAbsPathnHashTbl(ignoreDirs);
        iDLast := iD;
        ignoreDirsLast := ignoreDirs;
      END;
    END;
    IF sf.base = NIL OR Text.Empty(AbsPathn.Denotation(sf.base)) OR
      p.isAbsolute() THEN
      pn := p;
    ELSE
      pn := AbsPathn.Join(sf.base, p, NIL);
    END;
    UR(p, FileSystem.Lookup(pn));
  END UpdateRec;

(*--------------------------------------------------------------------------*)
PROCEDURE UpdateFlat (sf : T; p : AbsPathn.T; ext : TextSeq.T;
                      ignoreDirExpr : FindExpr.T := NIL; 
                      ignoreFileExpr : FindExpr.T := NIL) : AbsPathnSeq.T = 
 
  VAR 
    e : TextHashTbl.T;
    i : FileSystem.Iterator;
    n : AbsPathn.T;
    dummy : Dummy.T;
    s : FileStatus.T;
    q : AbsPathn.T;
    res : AbsPathnSeq.T := NEW(AbsPathnSeq.T).init(10);
    ignoreFile : BOOLEAN;
  BEGIN
    IF extLast # NIL AND ext = extLast THEN
      e := eLast;
    ELSE
      IF ext = NIL THEN
        e := NIL;
      ELSE
        e := TextSeqToTextHashTbl(ext);
        extLast := ext;
        eLast := e;
      END;
    END;
    IF sf.base = NIL OR Text.Empty(sf.base.denotation()) OR
      p.isAbsolute() THEN
      i := FileSystem.GetIterator(p);
    ELSE
      i := FileSystem.GetIterator(AbsPathn.Join(sf.base, p, NIL));
    END;
    WHILE i.nextWithStatus(n, s) DO 
      q := AbsPathn.Join(p, n, NIL);
      TRY
        IF s.isFile THEN
          ignoreFile := ignoreFileExpr # NIL AND
          ignoreFileExpr.test(n.denotation());
        ELSIF s.isDir THEN
          ignoreFile := ignoreDirExpr # NIL AND
          ignoreDirExpr.test(n.denotation());
        ELSE
          ignoreFile := FALSE;
        END;
      EXCEPT ELSE
        ignoreFile := FALSE;
      END;
      IF (e = NIL OR e.size() = 0 OR e.get(AbsPathn.LastExt(n), dummy) OR 
          s.isDir) AND NOT ignoreFile THEN    
         EVAL sf.cache.put(q, s);
         res.addhi(q);
         (*
         IF sf.msgif = NIL OR sf.msgif.debugLevel > 2 THEN
           MsgX.D(sf.msgif, "FileInfo: updating " & q.denotation(), 
                  level := 3);
         END;
         *)
      END; 
    END;
    EVAL sf.cache.put(p, FileSystem.Lookup(p));
    EVAL sf.dir.put(p, res);
    i.close();
    RETURN res
  END UpdateFlat;

(*--------------------------------------------------------------------------*)
PROCEDURE GetDir(self : T; READONLY 
p : AbsPathn.T) : AbsPathnSeq.T =
  VAR res : AbsPathnSeq.T;
  BEGIN
    IF self.dir.get(p, res) THEN
      RETURN res;
    ELSE
      RETURN NIL;
    END;
  END GetDir;

(*--------------------------------------------------------------------------*)
PROCEDURE DirList(self : T; p : AbsPathn.T; long := TRUE) : TEXT =
  VAR 
    dir  := GetDir(self, p);
    fn   :  AbsPathn.T;
    s    :  FileStatus.T;
    type :  TEXT;
    time :  TEXT;
    size :  TEXT;
    line :  TEXT;
    res  := "";
  BEGIN
    IF dir = NIL THEN RETURN NIL END;
    IF long THEN
      FOR i := 0 TO dir.size() - 1 DO
        fn := dir.get(i);
        s  := GetStatus(self, fn);
        IF s.exists THEN
          IF s.isDir THEN
            type := "d";
          ELSIF s.isFile THEN
            type := "f";
          ELSE
            type := "?";
          END;
          time := FmtTime.Long(s.modified);
          size := Fmt.LongInt(s.size); 
          line := Fmt.F("%-32s %10s %1s %s\n", fn.denotation(), 
                        size, type, time);
          res := res & line;
        END;
      END;
    ELSE
      FOR i := 0 TO dir.size() - 1 DO
        fn := dir.get(i);
        res := res & fn.denotation() & "\n";
      END;
    END;
    RETURN res;
  END DirList;

(*--------------------------------------------------------------------------*)
<*NOWARN*> PROCEDURE OutFileStatus(self : T; p : AbsPathn.T;
                                   stat : FileStatus.T) =
  BEGIN
    IF NOT Msg.dFlag THEN RETURN END;
    VAR
      pn := p.denotation();
      s, t, k : TEXT;
    BEGIN
      IF stat = NIL THEN
        MsgX.D(self.msgif, pn & ": NIL");
      ELSE
        t  := FmtTime.Long(stat.modified);
        s  := Fmt.LongInt(stat.size);
        IF stat.isDir THEN
          k := " d ";
        ELSIF stat.isFile THEN
          k := " f ";
        ELSE
          k := " ? ";
        END;
        MsgX.D(self.msgif, pn & k & s & " " & t);
      END;
    END;
  END OutFileStatus;

(*--------------------------------------------------------------------------*)
PROCEDURE ComputeFingerprint(
    self : T; READONLY p : AbsPathn.T;
    READONLY ignoreDirExpr : FindExpr.T := NIL; 
    READONLY ignoreFileExpr : FindExpr.T := NIL) : REF Fingerprint.T =
  VAR
    dir  :  AbsPathnSeq.T;
    fn   :  AbsPathn.T;
    s    :  FileStatus.T;
    fp   := NEW(REF Fingerprint.T);
    res  :  REF Fingerprint.T := NIL;
    size :  TEXT;
    time :  TEXT;
    base :  TEXT;
    ignoreDir : BOOLEAN;
    ignoreFile : BOOLEAN;
    todo := NEW(AbsPathnSeq.T).init();
  BEGIN
    (* Msg.T2("ComputeFingerprint", p.denotation()); *)
    todo.addhi(p);
    WHILE todo.size() > 0 DO
      fn := todo.remlo();
      s  := GetStatus(self, fn);
      (* OutFileStatus(self, fn, s); *)
      IF s.exists THEN 
        base := AbsPathn.Last(fn).denotation();
        IF s.isDir THEN
          TRY
            ignoreDir := ignoreDirExpr # NIL AND ignoreDirExpr.test(base);
          EXCEPT ELSE
            ignoreDir := FALSE;
          END;
        ELSIF s.isFile THEN
          TRY
            ignoreFile := ignoreFileExpr # NIL AND ignoreFileExpr.test(base);
          EXCEPT ELSE
            ignoreFile := FALSE;
          END;
        END;
        IF s.isFile AND NOT ignoreFile THEN
          (* combine the fingerprints of the name, the size, and the
             modification time *)
          time := Fmt.LongReal(s.modified, Fmt.Style.Fix, 2);
          (* Msg.D(time); *)
          size := Fmt.LongInt(s.size);
          fp^  := Fingerprint.Combine(
                      fp^, 
                      Fingerprint.Combine(
                          Fingerprint.FromText(time),
                          Fingerprint.FromText(size)));
          (* fp^  := Fingerprint.FromText(fn.denotation() & time & size); *)
        ELSIF s.isDir AND NOT ignoreDir THEN
          (* combine the fingerprints of all directory entries *)
          fp^ := Fingerprint.FromText(fn.denotation());
          dir  := GetDir(self, fn);
          FOR i := 0 TO dir.size() - 1 DO
            fn := dir.get(i);
            todo.addhi(fn);
          END;
        ELSE
          (* use only the name *)
          fp^ := Fingerprint.FromText(fn.denotation());
        END;
        IF res = NIL THEN
          res := fp;
          fp  := NEW(REF Fingerprint.T);
        ELSE
          res^ := Fingerprint.Combine(res^, fp^)
        END;
      END;
    END;
    RETURN res;
  END ComputeFingerprint;

(*--------------------------------------------------------------------------*)
PROCEDURE ComputeFingerprintRec(
    self : T; READONLY p : AbsPathn.T;
    READONLY ignoreDirExpr : FindExpr.T := NIL; 
    READONLY ignoreFileExpr : FindExpr.T := NIL) : REF Fingerprint.T =
  VAR
    dir  :  AbsPathnSeq.T;
    fn   :  AbsPathn.T;
    s    :  FileStatus.T;
    fp   := NEW(REF Fingerprint.T);
    time :  TEXT;
    size :  TEXT;
    base :  TEXT;
    ignoreDir : BOOLEAN;
    ignoreFile : BOOLEAN;
  BEGIN
    s  := GetStatus(self, p);
    IF NOT s.exists THEN RETURN NIL END;
    fp^ := Fingerprint.FromText(p.denotation());
    base := AbsPathn.Last(p).denotation();
    IF s.isDir THEN
      TRY
        ignoreDir := ignoreDirExpr # NIL AND ignoreDirExpr.test(base);
      EXCEPT ELSE
        ignoreDir := FALSE;
      END;
    END;
    IF s.isFile THEN
      TRY
        ignoreFile := ignoreFileExpr # NIL AND ignoreFileExpr.test(base);
      EXCEPT ELSE
        ignoreFile := FALSE;
      END;
    END;
    IF s.isFile AND NOT ignoreFile THEN
      (* combine the fingerprints of the name, the size, and the
         modification time *)
      time := Fmt.LongReal(s.modified, Fmt.Style.Fix, 2);
      size := Fmt.LongInt(s.size);
      fp^  := Fingerprint.Combine(
                  fp^, 
                  Fingerprint.Combine(
                      Fingerprint.FromText(time),
                      Fingerprint.FromText(size)));
    ELSIF s.isDir AND NOT ignoreDir THEN
      (* combine the fingerprints of all directory entries *)
      dir  := GetDir(self, p);
      FOR i := 0 TO dir.size() - 1 DO
        fn := dir.get(i);
        WITH fpi = ComputeFingerprint(self, fn, 
                                      ignoreDirExpr, ignoreFileExpr) DO
          IF fpi # NIL AND NOT Fingerprint.Equal(fpi^, Fingerprint.Zero) THEN
            fp^ := Fingerprint.Combine(fp^, fpi^);
          END;
        END;
      END;
    ELSE
      (* use only the name *)
    END;
    RETURN fp;
  END ComputeFingerprintRec;

(*--------------------------------------------------------------------------*)
PROCEDURE Dump (sf : T; wr : Wr.T) = 
  VAR
    iter := sf.cache.iterate();
    pn   : AbsPathn.T;
    s    : FileStatus.T;
  BEGIN
    WHILE iter.next(pn, s) DO
      TRY
        Wr.PutText(wr, pn.denotation() & ": " & s.repr() & "\n");
      EXCEPT ELSE END;
    END;
  END Dump;

(*--------------------------------------------------------------------------*)
VAR
  extLast : TextSeq.T := NIL;
  ignoreDirsLast : AbsPathnSeq.T := NIL;
  eLast : TextHashTbl.T := NIL;
  iDLast : AbsPathnHashTbl.T := NIL; 
BEGIN
END FileInfo.
