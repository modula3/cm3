(*--------------------------------------------------------------------------*)
MODULE Main;

  IMPORT
    TextSeq, AbstractPathname, AbstractPathnameSeq, Time, Params, Process, 
    Fmt, Wr, Stdio, FileInfo, SimpleScanner, ScanToken, FingerprintFmt, Msg;

REVEAL
  SimpleScanner.Token = 
    ScanToken.T BRANDED "ScanToken prjm 0.0" OBJECT
    METHODS
    END;

  VAR cache : FileInfo.T;
      base, path, root : AbstractPathname.T; 
      ext: TextSeq.T := NIL;
      ignoreDirs : AbstractPathnameSeq.T := NIL;
      start, end : Time.T;

  BEGIN 
    Msg.dFlag := TRUE;
    IF Params.Count < 3 THEN
      <*NOWARN*> Wr.PutText(Stdio.stderr, 
                    "usage: FileInfoTest base path name [ext [ext [...]]]\n");
      (*base : cache base, for example /usr/contrib/lib
        path : starting point for (recursive) update relative to base, e.g. m3
        name : directory name to be ignored, for example hmtl
        ext  : file extension to be considered, for example i3*)
      Process.Exit(1);
    END;
    base := NEW(AbstractPathname.T).init(Params.Get(1));
    path := NEW(AbstractPathname.T).init(Params.Get(2));
    root := AbstractPathname.Join(base, path, NIL);
    IF Params.Count > 3 THEN
      ignoreDirs := NEW(AbstractPathnameSeq.T).init(1);
      ignoreDirs.addhi(NEW(AbstractPathname.T).init(Params.Get(3)));
    END;
    IF Params.Count > 4 THEN
      ext := NEW(TextSeq.T).init(10);
      FOR i := 4 TO Params.Count - 1 DO
        ext.addhi(Params.Get(i));
      END;
    END;
    TRY
      Wr.PutText(Stdio.stdout, "testing recursive update\n");
      start := Time.Now();
      cache := NEW(FileInfo.T).init(10000, base);
      cache.updateRec(path, ext, ignoreDirs);
      end := Time.Now();
      Wr.PutText(Stdio.stdout, 
                 "cache size   = " & Fmt.Int(cache.size()) & "\n");
      Wr.PutText(Stdio.stdout, "time elapsed = " & 
        Fmt.LongReal(end - start, Fmt.Style.Fix) & "\n");
      Wr.PutText(Stdio.stdout, "testing fingerprint\n");
      start := Time.Now();
      WITH fp = cache.fingerprint(path) DO
        end := Time.Now();
        IF fp = NIL THEN
          Wr.PutText(Stdio.stdout, "NIL\n");
        ELSE
          Wr.PutText(Stdio.stdout, FingerprintFmt.Hex(fp^) & "\n");
        END;
        Wr.PutText(Stdio.stdout, "time elapsed = " & 
          Fmt.LongReal(end - start, Fmt.Style.Fix) & "\n");
      END;
      Wr.PutText(Stdio.stdout, "testing flat update\n");
      start := Time.Now();
      cache := NEW(FileInfo.T).init(100, base);
      EVAL cache.updateFlat(path, ext);
      end := Time.Now();
      Wr.PutText(Stdio.stdout, 
                 "cache size   = " & Fmt.Int(cache.size()) & "\n");
      Wr.PutText(Stdio.stdout, "time elapsed = " & 
        Fmt.LongReal(end - start, Fmt.Style.Fix) & "\n");
    EXCEPT ELSE
    END;
  END Main.
