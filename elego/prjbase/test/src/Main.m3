(*---------------------------------------------------------------------------*)
MODULE Main;

IMPORT Text, TextRd, TextSeq, TextTextTbl, Params, Bundle;
IMPORT Msg, PkgBase, PkgBaseBundle, PrjDesc, FSUtils;


(*---------------------------------------------------------------------------*)
PROCEDURE OutTextSeq(header : TEXT; seq : TextSeq.T) =
  BEGIN
    Msg.T(header);
    FOR i := 0 TO seq.size() - 1 DO
      Msg.T("  " & seq.get(i));
    END;
    Msg.T("");
  END OutTextSeq;

(*---------------------------------------------------------------------------*)
PROCEDURE OutTextTable(header : TEXT; tbl : TextTextTbl.T) =
  VAR 
    iter := tbl.iterate();
    key, val : TEXT;
  BEGIN
    Msg.T(header);
    WHILE iter.next(key, val) DO
      Msg.T("  " & key & "	" & val);
    END;
    Msg.T("");
  END OutTextTable;

(*---------------------------------------------------------------------------*)
VAR (* Main *)
  cfg     := NEW(PkgBase.T).init();
  cfgData := Bundle.Get(PkgBaseBundle.Get(), "PkgBase.DefaultData");
  trd     := TextRd.New(cfgData);
  prjdesc :  PrjDesc.T;
  fn      :  TEXT;
  pkgpath :  TextSeq.T;
BEGIN (* Main *)
  Msg.tFlag := TRUE;
  IF NOT FSUtils.IsDir("../src") THEN
    Msg.Error("../src no dir");
  END;
  IF NOT FSUtils.IsFile("../src/m3makefile") THEN
    Msg.Error("../src/m3makefile no file");
  END;
  IF NOT cfg.addDefs(trd) THEN
    Msg.Fatal("error in PkgBase.DefaultData");
  END;

  IF Params.Count > 1 THEN
    fn := Params.Get(1);
  ELSE
    fn := "PrjDesc";
  END;

  TRY
    prjdesc := NEW(PrjDesc.T).init(fn, cfg, TRUE);
  EXCEPT
    PrjDesc.Error(e) => Msg.Fatal(e);
  END;
  (* file `fn' read in *)

  pkgpath := NEW(TextSeq.T).init();
  FOR i := 0 TO prjdesc.packages().size() - 1 DO
    WITH pkg = prjdesc.packages().get(i) DO
      pkgpath.addhi(pkg & " found at " & prjdesc.getPoolSet().pkgPath(pkg));
    END;
  END;
  OutTextSeq("All collections of this project:", prjdesc.collections());
  OutTextSeq("All packages of this project:", pkgpath);
  OutTextSeq("All snapshots of this project:", prjdesc.snapshots());
  OutTextSeq("All releases of this project:", prjdesc.releases());

  FOR i := 0 TO prjdesc.snapshots().size() - 1 DO
    WITH name = prjdesc.snapshots().get(i) DO
      WITH tbl = prjdesc.snapshot(name) DO
        OutTextTable("Snapshot " & name, tbl);
      END;
    END;
  END;

  FOR i := 0 TO prjdesc.releases().size() - 1 DO
    WITH name = prjdesc.releases().get(i) DO
      WITH tbl = prjdesc.release(name) DO
        OutTextTable("Release " & name, tbl);
      END;
    END;
  END;

  TRY 
    prjdesc.buildDepGraph();
  EXCEPT
    PrjDesc.Error(e) => Msg.Fatal(e);
  END;

  OutTextSeq("All ignored packages used by this project: ", 
             prjdesc.ignoredPackages());
  OutTextSeq("Package update sequence: ", prjdesc.packageUpdateSequence());

  OutTextSeq("All modified packages:", prjdesc.modifiedPackages());

  IF prjdesc.testAllPackagesReleased() THEN
    Msg.T("all packages of the project are checked out as released versions");
  END;
  IF prjdesc.testNoPackageModified() THEN
    Msg.T("no package of the project has been locally modified");
  END;
  TRY
    prjdesc.write("PrjDesc.tmp");
  EXCEPT
    PrjDesc.Error(e) => Msg.Fatal(e);
  END;
  (* file `fn' read in *)
END Main.
