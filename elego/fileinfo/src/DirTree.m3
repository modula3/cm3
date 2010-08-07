(*--------------------------------------------------------------------------*)
MODULE DirTree;

IMPORT Time, TextSeq;
IMPORT FileInfo, FileStatus, FindExpr, BoolSeq, RegEx;
IMPORT APN AS APN, APNSeq AS APNSeq;

(*--------------------------------------------------------------------------*)
PROCEDURE SimpleTextDirLayout(self : SimpleTextLayout; pn : APN.T;
                              time : Time.T; size : LONGINT;
                              <*UNUSED*>level : CARDINAL; 
                              lastPrefix : BoolSeq.T) : TEXT =
  VAR
    res := "";
    ilast := lastPrefix.size() - 1;
  BEGIN
    FOR i := 0 TO ilast DO
      WITH last = lastPrefix.get(i) DO
        IF i < ilast THEN
          IF last THEN
            res := res & "    ";
          ELSE
            res := res & "  | ";
          END;
        ELSE
          IF last THEN
            res := res & "  \\- ";
          ELSE
            res := res & "  |- ";
          END;
        END;
      END;
    END;
    RETURN res & self.fmtDir(pn, time, size) & "\n";
  END SimpleTextDirLayout;

(*--------------------------------------------------------------------------*)
PROCEDURE SimpleTextFileLayout(self : SimpleTextLayout; pn : APN.T; 
                               time : Time.T; size : LONGINT;
                               <*UNUSED*>level : CARDINAL; 
                               lastPrefix : BoolSeq.T) : TEXT =
  VAR
    res := "";
    ilast := lastPrefix.size() - 1;
  BEGIN
    FOR i := 0 TO ilast DO
      WITH last = lastPrefix.get(i) DO
        IF i < ilast THEN
          IF last THEN
            res := res & "    ";
          ELSE
            res := res & "  | ";
          END;
        ELSE
          IF last THEN
            res := res & "  \\- ";
          ELSE
            res := res & "  |- ";
          END;
        END;
      END;
    END;
    RETURN res & self.fmtFile(pn, time, size) & "\n";
  END SimpleTextFileLayout;

(*--------------------------------------------------------------------------*)
PROCEDURE SimpleTextFmtDir(<*UNUSED*>self : SimpleTextLayout; 
                           pn : APN.T; 
                           <*UNUSED*>time : Time.T; 
                           <*UNUSED*>size : LONGINT) : TEXT =
  BEGIN
    RETURN APN.Last(pn).denotation();
  END SimpleTextFmtDir;

(*--------------------------------------------------------------------------*)
PROCEDURE SimpleTextFmtFile(<*UNUSED*>self : SimpleTextLayout; 
                            pn : APN.T; 
                            <*UNUSED*>time : Time.T; 
                            <*UNUSED*>size : LONGINT) : TEXT =
  BEGIN
    RETURN APN.Last(pn).denotation();
  END SimpleTextFmtFile;

(*--------------------------------------------------------------------------*)
PROCEDURE SimpleTextNoSort(<*UNUSED*>self : SimpleTextLayout; 
                           list : APNSeq.T) : APNSeq.T =
  BEGIN
    RETURN list;
  END SimpleTextNoSort;

(*--------------------------------------------------------------------------*)
PROCEDURE Layout(cache   :  FileInfo.T;
                 root    :  APN.T;
                 lfuns   :  LayoutClosure := NIL;
                 ignDir  :  FindExpr.T := NIL; 
                 ignFile :  FindExpr.T := NIL;
                 sc      := SortCriterion.None;
                 update  := FALSE) : TEXT =

  PROCEDURE L(p : APN.T; level : CARDINAL; lastPrefix : BoolSeq.T) : TEXT =

    PROCEDURE LevelLayout(list : APNSeq.T) : TEXT =
      VAR 
        fn  :  APN.T;
        res := "";
      BEGIN
        list := lfuns.sort(list);
        FOR i := 0 TO list.size() - 1 DO
          fn := list.get(i);
          VAR lp := NEW(BoolSeq.T).init(); BEGIN
            lp := BoolSeq.Cat(lp, lastPrefix);
            lp.addhi(i = list.size() - 1);
            res := res & L(fn, level + 1, lp);
          END;
        END;
        RETURN res;
      END LevelLayout; 

    VAR
      dir        :  APNSeq.T;
      dirs       :  APNSeq.T;
      files      :  APNSeq.T;
      fn         :  APN.T;
      s, fstat   :  FileStatus.T;
      base       :  TEXT;
      base2      :  TEXT;
      ignoreDir  :  BOOLEAN;
      ignoreFile :  BOOLEAN;
      itemLayout :  TEXT := "";
      partLayout :  TEXT := "";
    BEGIN
      s  := cache.getStatus(p);
      IF NOT s.exists THEN RETURN "not found: " & p.denotation() END;
      base := APN.Last(p).denotation();
      TRY
        ignoreDir := ignDir # NIL AND ignDir.test(base);
      EXCEPT ELSE
        ignoreDir := FALSE;
      END;
      TRY
        ignoreFile := ignFile # NIL AND ignFile.test(base);
      EXCEPT ELSE
        ignoreFile := FALSE;
      END;
      IF s.isFile AND NOT ignoreFile THEN
        itemLayout := lfuns.file(p, s.modified, s.size, level, lastPrefix);
      ELSIF s.isDir AND NOT ignoreDir THEN
        itemLayout := lfuns.dir(p, s.modified, s.size, level, lastPrefix);
        dir  := cache.getDir(p);
        IF sc # SortCriterion.None THEN
          dirs := NEW(APNSeq.T).init();
          files := NEW(APNSeq.T).init();
          FOR i := 0 TO dir.size() - 1 DO
            fn := dir.get(i);
            base2 := APN.Last(fn).denotation();
            fstat := cache.getStatus(fn);
            TRY
              IF fstat.isDir AND 
                (ignDir = NIL OR NOT ignDir.test(base2)) THEN
                dirs.addhi(fn);
              ELSIF fstat.isFile AND 
                (ignFile = NIL OR NOT ignFile.test(base2)) THEN
                files.addhi(fn);
              END;
            EXCEPT
              RegEx.Error => 
              IF fstat.isDir THEN
                dirs.addhi(fn);
              ELSIF fstat.isFile THEN
                files.addhi(fn);
              END;
            END;
          END;
          IF sc = SortCriterion.DirsFirst THEN
            partLayout := LevelLayout(APNSeq.Cat(dirs, files));
          ELSIF sc = SortCriterion.FilesFirst THEN
            partLayout := LevelLayout(APNSeq.Cat(files, dirs));
          END;
        ELSE
          partLayout := LevelLayout(dir);
        END;
      ELSIF NOT ignoreDir AND NOT ignoreFile THEN
        itemLayout := lfuns.file(p, s.modified, s.size, level, lastPrefix) &
                          " (unknown file type)\n";
      END;
      RETURN itemLayout & partLayout;
    END L;

  VAR
    last : BoolSeq.T;
  BEGIN (* Layout *)
    IF cache = NIL THEN RETURN NIL END;
    IF update THEN
      cache.updateRec(root, emptyTextSeq, emptyAPNSeq, ignDir, ignFile);
    END;
    IF lfuns = NIL THEN
      lfuns := simpleTextLayout;
    END;
    last := NEW(BoolSeq.T).init();
    last.addhi(TRUE);
    RETURN L(root, 0, last);
  END Layout;

VAR 
  emptyTextSeq := NEW (TextSeq.T).init();
  emptyAPNSeq  := NEW (APNSeq.T).init();
BEGIN
  simpleTextLayout := NEW(SimpleTextLayout);
END DirTree.
