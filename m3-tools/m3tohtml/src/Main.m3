(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun  1 08:23:24 PDT 1995 by kalsow                   *)

MODULE Main;

IMPORT Text, Rd, Wr, Stdio, Thread, Fmt, Time, TextSeq;
IMPORT OSError, FileRd, FileWr, Pathname, FS, M3Config, ParseParams;
IMPORT MarkUp, M3DB, HTMLDir, FilePath, Process, FSUtils, Msg;
FROM Msg IMPORT M, V, F;
<*FATAL Thread.Alerted*>

CONST u = ARRAY OF TEXT {
  "",
  "SYNTAX",
  "",
  "  m3tohtml [<options>] <pkg>+",
  "or",
  "  m3tohtml [<options>]  <  <file-list>",
  "",
  "  options:",
  "",
  "    -force|-F                        overwrite existing HTML.index",
  "    -root|-pkgroot <package root>    defined package root directory",
  "                                     (default: PKG_USE from cm3.cfg)",
  "    -dir|-dest <outdir>              create output in directory outdir",
  "    -d|-debug                        display debug output",
  "    -v|-verbose                      be verbose",
  "",
  "SEMANTICS",
  "",
  "  m3tohtml reads one or more CM3 packages and creates an HTML tree of all",
  "  interfaces and modules together with a complete index structure.",
  "  All interface, module, procedure, and type names are converted into",
  "  hyperrefs pointing to the appropriate definition.",
  "",
  "  All output will be placed in the current directory (unless -d is used),",
  "  where also a file named m3db will be found. This file contains all", 
  "  symbol information from the parsed M3 sources needed for the hypertext.",
  "",
  "  As m3tohtml actually understands the complete Modula-3 syntax, it is",
  "  much more than a documentation generator based on comment extraction.",
  "  It is possible to navigate with a few clicks directly to the definition",
  "  or implementation of a given entity, which is a great help for",
  "  programmers.",
  "  ",
  "  The generated tree will have exactly the same structure as the package",
  "  sub-tree used as input; the suffix `.html' will be appended to all",
  "  file names. Additionally, a new `href' hierarchy may be created, which",
  "  contains partial index files for intermediate index levels.",
  "  If the first form with automatic package scanning is used, only",
  "  files with the extensions `.i3', `.m3', `.ig', `.mg', and `.tmpl'",
  "  will be used for HTML generation.",
  "",
  "HISTORY",
  "",
  "  The m3tohtml man page says that Bill Kalsow wrote it as part of his",
  "  HTML browser for /proj/m3. He didn't write a man page.",
  "  Later, part of the functionality of the program has been incorporated",
  "  into Reactor, the graphical CM3 frontend from Critical Mass.",
  "  The changes from CM3 4.1 to 5.1 broke this code in several ways.",
  "  It was made usable again at Elego GmbH, where an easier-to-use",
  "  interface was implemented, too. The second (original) form which",
  "  reads all the file and package names from standard input in a non-",
  "  documented format should still work, too.",
  "",
  "BUGS",
  "",
  "  The program is still somewhat peculiar about its environment. It tends",
  "  to crash in unexpected situations with obscure error messages (if all).",
  "  There are also still some issues with the generated HTML; parameters",
  "  of generic module instantiations contain wrong references, and pathname",
  "  normalization does not cover all possibilities (for example `./.').",
  ""
  };
    

PROCEDURE Usage() =
  BEGIN
    FOR i := FIRST(u) TO LAST(u) DO
      M(u[i]);
    END;
  END Usage;

PROCEDURE ProcessParameters() =
  BEGIN
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      TRY
        IF pp.keywordPresent("-h") OR pp.keywordPresent("-help") OR
           pp.keywordPresent("-?") THEN
          Usage();
          Process.Exit(0);
        END;
        force := pp.keywordPresent("-force") OR pp.keywordPresent("-F");
        Msg.debug := pp.keywordPresent("-d") OR pp.keywordPresent("-debug");
        Msg.verbose := pp.keywordPresent("-v") OR 
                           pp.keywordPresent("-verbose");
        IF pp.keywordPresent("-root") OR pp.keywordPresent("-pkgroot") THEN
          pkgRoot := pp.getNext();
        END;
        IF pp.keywordPresent("-dir") OR pp.keywordPresent("-dest") THEN
          outdir := pp.getNext();
        END;
	nTargets := NUMBER(pp.arg^) - pp.next;
        (* build parameters *)
	targets := NEW(TextSeq.T).init(nTargets);
	FOR i := 1 TO nTargets DO
	  VAR t := pp.getNext(); BEGIN
            targets.addhi(t);
	  END;
	END;
        pp.finish();
      EXCEPT
        ParseParams.Error => F("parameter error");
      END;
    END;
    (* all command line parameters handled *)
  END ProcessParameters;

TYPE
  Source = REF RECORD
    from : TEXT;
    to   : TEXT;
    kind : FilePath.Kind;
    next : Source;
  END;

VAR
  sources: Source := NIL;
  n_sources: INTEGER := 0;
  pkgRoot := M3Config.PKG_USE;
  targets : TextSeq.T;
  nTargets : INTEGER;
  force := FALSE;
  outdir : TEXT := NIL;

PROCEDURE ReadFileList () =
  <*FATAL Rd.EndOfFile, Rd.Failure, Thread.Alerted*>

  PROCEDURE AddFile(file, pkg, pkgpath: TEXT) =

    PROCEDURE Add() =
      BEGIN
        sources := NEW (Source, next := sources,
                        from := Pathname.Join(pkgpath, file, NIL),
                        to   := Pathname.Join(pkg, FixDerived (file), NIL),
                        kind := kind);
        V("  ", fk, ": ", sources.from, " -> ", sources.to);
      END Add;

    BEGIN
      INC (n_sources);
      kind := FilePath.Classify (file);
      CASE kind OF
        FilePath.Kind.I3 => fk := "I3"; Add();
      | FilePath.Kind.M3 => fk := "M3"; Add();
      | FilePath.Kind.IG => fk := "IG"; Add();
      | FilePath.Kind.MG => fk := "MG"; Add();
      | FilePath.Kind.TMPL => fk := "TMPL"; Add();
      ELSE
        fk := "??";
        V("  ", fk, ": ", file);
      END;
    END AddFile;

  PROCEDURE AddPkg(pkg: TEXT) =
    VAR
      root := Pathname.Join(pkgRoot, pkg, NIL);

    PROCEDURE AddRec(pref: TEXT) =
      VAR
        dir := root;
      BEGIN
        IF pref # NIL THEN
          dir := Pathname.Join(root, pref, NIL);
        END;
        VAR
          iter  :  FS.Iterator;
          name  :  TEXT;
          path  :  TEXT;
          rpath :  TEXT;
        BEGIN
          TRY
            iter  := FS.Iterate(dir);
          EXCEPT
            OSError.E => V("cannot read directory ", dir); RETURN;
          END;
          WHILE iter.next(name) DO
            path := Pathname.Join(dir, name, NIL);
            IF pref = NIL THEN
              rpath := name;
            ELSE
              rpath := Pathname.Join(pref, name, NIL);
            END;
            IF FSUtils.IsDir(path) THEN
              AddRec(rpath);
            ELSIF FSUtils.IsFile(path) THEN
              AddFile(rpath, pkg, root);
            ELSE
            END;
          END;
        END;
      END AddRec;

    BEGIN
      IF NOT FSUtils.IsDir(root) THEN
        M("package ", pkg, " not found");
        RETURN;
      END;
      V(pkg, " ==> ", root);
      AddRec(NIL);
    END AddPkg;

  VAR 
    pkg, proj_pkg, file, fk: TEXT;  
    a,b,c: Source;  rd := Stdio.stdin;
    kind: FilePath.Kind;
  BEGIN
    proj_pkg := "";
    pkg := "";
    IF nTargets = 0 THEN
      (* read the input file *)
      WHILE NOT Rd.EOF (rd) DO
        file := Rd.GetLine (rd);
        IF Text.GetChar (file, 0) = '$' THEN
          pkg := Text.Sub (file, 1);
          WITH i =Text.FindChar(pkg, '$') DO
            IF i > 0 THEN
              proj_pkg := Text.Sub(pkg, i + 1);
              pkg := Text.Sub(pkg, 0, i);
            ELSE
              proj_pkg := pkgRoot & M3Config.PATH_SEP & pkg;
            END;
          END;
          V(pkg, " ==> ", proj_pkg);
        ELSE
          AddFile(file, pkg, proj_pkg);
        END;
      END;
    ELSE
      FOR i := 0 TO nTargets - 1 DO
        WITH pkg = targets.get(i) DO
          AddPkg(pkg);
        END;
      END;
    END;

    (* reverse the list *)
    a := sources;  b := NIL;
    WHILE (a # NIL) DO
      c := a.next;
      a.next := b;
      b := a;
      a := c;
    END;
    sources := b;
  END ReadFileList;

VAR(*CONST*) Build_dir_len := Text.Length (M3Config.BUILD_DIR);

PROCEDURE FixDerived (filename: TEXT): TEXT =
  VAR i: INTEGER;
  BEGIN
    IF (filename = NIL) OR (Text.Length (filename) <= Build_dir_len) THEN
      RETURN filename;
    END;

    i := 0;
    WHILE (i < Build_dir_len) DO
      IF Text.GetChar (filename, i) # Text.GetChar (M3Config.BUILD_DIR, i) THEN
        RETURN filename;
      END;
      INC (i);
    END;

    IF Text.GetChar (filename, i) = Text.GetChar (M3Config.PATH_SEP, 0) THEN
      filename := "derived" & Text.Sub (filename, i);
    END;
    RETURN filename;
  END FixDerived;

PROCEDURE UpdateDB () =
  <*FATAL Thread.Alerted*>
  VAR s := sources;  rd: Rd.T;  n := 0;
  BEGIN  
    M3DB.Open ("m3db");
    WHILE (s # NIL) DO
      TRY
        rd := FileRd.Open (s.from);
        M3DB.AddUnit (rd, s.to);
        Rd.Close (rd);
        Tick (n);
      EXCEPT OSError.E, Rd.Failure => (*skip*)
        Out ("failed to parse: ", s.from);
      END;
      s := s.next;
    END;
    M3DB.Dump ("m3db");
  END UpdateDB;

PROCEDURE GenerateHTML () =
  CONST TmpFile = "/tmp/m3tohtml.tmp";
  VAR s := sources;  rd: Rd.T;  wr: Wr.T;  n := 0;  args: ARRAY [0..1] OF TEXT;
  BEGIN  
    WHILE (s # NIL) DO
      TRY
        MakeDir (Pathname.Prefix (s.to));

        args[0] := s.from;
        args[1] := TmpFile;
        IF Process.Wait (Process.Create ("PREPROCESS", args)) = 0 THEN
          rd := FileRd.Open (TmpFile);
          WITH dir = Pathname.Prefix(s.to) DO
            IF dir # NIL THEN
              IF NOT FSUtils.IsDir(dir) THEN
                FSUtils.MakeDir(dir);
              END;
            END;
          END;
          TRY
            wr := FileWr.Open (s.to & ".html");
          EXCEPT ELSE
            F("cannot open ", s.to & ".html");
          END;
          MarkUp.Annotate (rd, wr, s.to);
          Wr.Close (wr);
          Rd.Close (rd);
        ELSE
          Out (s.from, ": preprocess failed");
        END;

        Tick (n);
      EXCEPT OSError.E, Rd.Failure, Wr.Failure => (*skip*)
        Out ("failed to translate: ", s.from);
      END;
      s := s.next;
    END;
    TRY FS.DeleteFile (TmpFile);
    EXCEPT OSError.E => (*skip*)
    END;
  END GenerateHTML;

PROCEDURE MakeDir (dir: TEXT) =
  BEGIN
    IF Text.Length (dir) <= 0 THEN RETURN; END;

    TRY
      IF FS.Status (dir).type = FS.DirectoryFileType THEN RETURN; END;
    EXCEPT OSError.E =>
      (* skip *)
    END;

    (* build our parent *)
    MakeDir (Pathname.Prefix (dir));

    TRY
      FS.CreateDirectory (dir);
    EXCEPT OSError.E =>
      (* skip *)
    END;
  END MakeDir;

PROCEDURE GenerateIndex () =
  <*FATAL Wr.Failure, OSError.E, Thread.Alerted *>
  VAR names := NEW (REF ARRAY OF TEXT, n_sources);  wr: Wr.T;
  BEGIN
    wr := FileWr.Open ("INDEX.html");
    Wr.PutText (wr, "<HTML>\n<HEAD>\n<TITLE>Modula-3 sources</TITLE>\n");
    Wr.PutText (wr, "</HEAD>\n<BODY bgcolor=\"#eeeeee\">\n<H1>Modula-3 sources</H1>\n<P>\n");
    GenIndex (wr, "href/I3", FilePath.Kind.I3, "Interfaces", names^);
    GenIndex (wr, "href/IG", FilePath.Kind.IG, "Generic interfaces", names^);
    GenIndex (wr, "href/M3", FilePath.Kind.M3, "Modules", names^);
    GenIndex (wr, "href/MG", FilePath.Kind.MG, "Generic modules", names^);
    Wr.PutText (wr, "</UL>\n</BODY>\n</HTML>\n");
    Wr.Close (wr);
  END GenerateIndex;

PROCEDURE GenIndex (wr: Wr.T;  file: TEXT;  kind: FilePath.Kind;  title: TEXT;
                    VAR names: ARRAY OF TEXT) =
  <*FATAL Wr.Failure, Thread.Alerted *>
  VAR cnt := 0;  s := sources;
  BEGIN
    Wr.PutText (wr, "<H2>");
    Wr.PutText (wr, title);
    Wr.PutText (wr, "</H2>\n<P>\n");
    WHILE (s # NIL) DO
      IF s.kind = kind THEN
        names [cnt] := s.to;  INC (cnt);
      END;
      s := s.next;
    END;
    IF cnt > 0 THEN
      HTMLDir.GenDir (SUBARRAY (names, 0, cnt), wr, file,
                      "Critical Mass Modula-3: " & title, 70);
    END;
    Wr.PutText (wr, "<P>\n");
  END GenIndex;

PROCEDURE Tick (VAR i: INTEGER) =
  <*FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    INC (i);
    IF (i >= 20) THEN
      Wr.PutChar (Stdio.stdout, '.');
      Wr.Flush (Stdio.stdout);
      i := 0;
    END;
  END Tick;

PROCEDURE Out (a, b, c: TEXT := NIL) =
  <*FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    IF (a # NIL) THEN Wr.PutText (Stdio.stdout, a); END;
    IF (b # NIL) THEN Wr.PutText (Stdio.stdout, b); END;
    IF (c # NIL) THEN Wr.PutText (Stdio.stdout, c); END;
    Wr.PutText (Stdio.stdout, "\n");
    Wr.Flush (Stdio.stdout);
  END Out;

PROCEDURE RunPhase (p: PROCEDURE ();  name: TEXT) =
  VAR start, stop: Time.T;
  BEGIN
    start := Time.Now ();
    Out (name, "...");
    p ();
    stop := Time.Now ();
    Out ("  ", Fmt.LongReal (stop - start, Fmt.Style.Fix, 2), " seconds.");
  END RunPhase;

PROCEDURE Confirm(msg : TEXT) : BOOLEAN =
  VAR 
    answer : TEXT;
  BEGIN
    LOOP
      TRY
        Wr.PutText(Stdio.stdout, msg & "? [y(es)<cr>/n(o)<cr>] ");
        Wr.Flush(Stdio.stdout);
        answer := Rd.GetLine(Stdio.stdin);
      EXCEPT 
        Rd.Failure => M("reader failure on stdin"); RETURN FALSE;
      | Rd.EndOfFile => M("eof on stdin"); RETURN FALSE;
      | Wr.Failure => M("writer failure on stdout"); RETURN FALSE;
      ELSE
        M("exception while reading confirmation");
        RETURN FALSE; (* if anything is wrong we don't want to continue *)
      END;
      IF Text.Equal(answer, "y") OR Text.Equal(answer, "yes") OR
         Text.Equal(answer, "Y") OR Text.Equal(answer, "YES") THEN
        RETURN TRUE;
      ELSIF Text.Equal(answer, "n") OR Text.Equal(answer, "no") OR
            Text.Equal(answer, "N") OR Text.Equal(answer, "NO") THEN
        RETURN FALSE;
      END;
      TRY
        Wr.PutText(Stdio.stdout, "\nPlease answer `yes' or `no'\n");
        Wr.Flush(Stdio.stdout);
      EXCEPT
        Rd.Failure => M("reader failure on stdin"); RETURN FALSE;
      | Rd.EndOfFile => M("eof on stdin"); RETURN FALSE;
      | Wr.Failure => M("writer failure on stdout"); RETURN FALSE;
      ELSE
        M("exception while reading confirmation");
        RETURN FALSE; (* if anything is wrong we don't want to continue *)
      END;
    END;
  END Confirm; 

BEGIN
  ProcessParameters();
  IF outdir # NIL THEN
    IF NOT FSUtils.IsDir(outdir) THEN
      FSUtils.MakeDir(outdir);
    END;
    TRY
      Process.SetWorkingDirectory(outdir);
    EXCEPT
      OSError.E => F("cannot change directory to " & outdir);
    END;
  END;
  IF FSUtils.IsFile("INDEX.html") AND NOT force THEN
    IF NOT Confirm("Overwrite existing INDEX.html") THEN
      Process.Exit(1);
    END;
  END;
  IF nTargets > 0 THEN
    RunPhase (ReadFileList, "scanning packages");
  ELSE
    RunPhase (ReadFileList, "reading file list");
  END;
  RunPhase (UpdateDB, "building database");
  RunPhase (GenerateHTML, "generating html");
  RunPhase (GenerateIndex, "generating index");
END Main.
