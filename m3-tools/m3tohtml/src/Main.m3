(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun  1 08:23:24 PDT 1995 by kalsow                   *)

MODULE Main;

IMPORT Text, Rd, Wr, Stdio, Thread, Fmt, Time;
IMPORT OSError, FileRd, FileWr, Pathname, FS, M3Config;
IMPORT MarkUp, M3DB, HTMLDir, FilePath, Process;
<*FATAL Thread.Alerted*>

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

PROCEDURE ReadFileList () =
  <*FATAL Rd.EndOfFile, Rd.Failure, Thread.Alerted*>
  VAR pkg, proj_pkg, file: TEXT;  a,b,c: Source;  rd := Stdio.stdin;
  BEGIN
    (* read the input file *)
    WHILE NOT Rd.EOF (rd) DO
      file := Rd.GetLine (rd);
      IF Text.GetChar (file, 0) = '$' THEN
        pkg := Text.Sub (file, 1) & M3Config.PATH_SEP;
        proj_pkg := M3Config.PKG_USE & M3Config.PATH_SEP & pkg;
      ELSE
        INC (n_sources);
        sources := NEW (Source, next := sources,
                        from := proj_pkg & file,
                        to   := pkg & FixDerived (file),
                        kind := FilePath.Classify (file));
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
          wr := FileWr.Open (s.to & ".html");
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
    Wr.PutText (wr, "<HTML>\n<HEAD>\n<TITLE>SRC Modula-3 sources</TITLE>\n");
    Wr.PutText (wr, "</HEAD>\n<BODY>\n<H1>SRC Modula-3 sources</H1>\n<P>\n");
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
    HTMLDir.GenDir (SUBARRAY (names, 0, cnt), wr, file,
                     "SRC Modula-3: " & title, 70);
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

BEGIN
  RunPhase (ReadFileList, "reading file list");
  RunPhase (UpdateDB, "building database");
  RunPhase (GenerateHTML, "generating html");
  RunPhase (GenerateIndex, "generating index");
END Main.
