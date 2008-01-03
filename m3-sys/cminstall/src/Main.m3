MODULE Main;

IMPORT Bundle, (* CMKey, CMCurrent, *) CoffTime, Env, File, Fmt, FS, Glob;
IMPORT M3ID, Msg, OS, OSError, Params, Pathname, Pipe, Process;
IMPORT Quake, QScanner, QToken, RegEx, Registry, RegularFile, Setup;
IMPORT Text, Text2, TextSeq, TextWr, Thread, Wr;
FROM Msg IMPORT Out, OutS, Ask, AskBool, AskChoice;

CONST
  OnUnix = (CoffTime.EpochAdjust = 0.0d0);

  DefaultInstallDir = ARRAY BOOLEAN OF TEXT
    { "c:\\cm3", "/usr/local/cm3" } [OnUnix];

  MinDiskSpace = 75; (* megabytes *)

  (* until we get the permission...
  REACTOR_EXE = ARRAY BOOLEAN OF TEXT <*NOWARN*>
    { "reactor.exe", "reactor" } [OnUnix];
  *)

  CM3_EXE = ARRAY BOOLEAN OF TEXT
    { "cm3.exe", "cm3" } [OnUnix];

  GZIP_EXE = ARRAY BOOLEAN OF TEXT
    { "gzip.exe", "gzip" } [OnUnix];

  TAR_EXE = ARRAY BOOLEAN OF TEXT
    { "tar.exe", "tar" } [OnUnix];

  FIXUP_EXE = ARRAY BOOLEAN OF TEXT
    { "fixup.bat", "fixup" } [OnUnix];

  ARCHIVES = ARRAY [0..3] OF TEXT
    {
      "cm3-doc*",
      "cm3-misc*",
      "cm3-specials*",
      "cm3-fixes*"
    };

VAR
  install_passwd    : TEXT;
  install_root      : TEXT;
  install_log       : TEXT;  
  disk_space        : INTEGER;
  initial_cfg       : TEXT;
  cm3_cfg           : TEXT;
  cminstall_root    : TEXT := NIL;
  gzip              : TEXT (* = OS.MakePath (cminstall_root, GZIP_EXE) *);
  tar               : TEXT (* = OS.MakePath (cminstall_root, TAR_EXE) *);

PROCEDURE DoIt () =
  BEGIN
    ParseParams ();

    IF NOT UtilsFound() THEN
      RETURN;
    END;

    Out ();
    Out ("Thank you for using Critical Mass CM3.  This program");
    Out ("will configure and install the system.");
    Out ();
    Out ("cminstall_root is set to ", cminstall_root);
    Out ("If this is not correct, please restart the installer with");
    Out ("-root <directory-of-installer-and-system-archive>");
    Out ();
    Out ("The installer will ask you some questions about the locations",
         " of programs");
    Out ("and libraries. Usually it will display a default inside [],",
         " which can be");
    Out ("accepted with <Enter>.");
    Out ("If the installer has found several choices, you may cycle through",
         " them");
    Out ("with `+' or `.' for the next and `-' for the previous one.");
    Out ("You may of course also enter a completely different value.");
    Out ();

    (* disabled
    (* verify license *)
    Out ("The use of this software is subject to the license agreement");
    Out ("in the file COPYRIGHT-CMASS.  Please read it now.");
    Out ();
    IF NOT AskBool ("Do you agree to the terms of the license?", "Y") THEN
      RETURN;
    END;
    *)

    (* get the install key *)
    (* disabled
    LOOP
      Out ();
      install_passwd := Ask ("Please enter your installation key: ", NIL);
      IF KeyCheck (install_passwd) THEN EXIT; END;
      Out ();
      Out ("Sorry, that is an invalid installation key, please reenter it.");
    END;
    *)

    (* get the install directory *)
    LOOP
      Out ();
      install_root := Ask ("Where would you like the system installed?",
                            DefaultInstallDir);
      install_root := OS.CleanDirName (install_root);
      Msg.Debug ("install_root => ", install_root);

      (* make sure the install directory exists *)
      IF NOT OS.MakeDir (install_root) THEN
        Out ("Unable to create directory: ", install_root);
      ELSE
        install_root := OS.GetAbsolutePath (install_root);
        Msg.Debug ("install_root => ", install_root);

        (* verify the disk space *)
        disk_space := OS.GetDiskSpace (install_root);
        Msg.Debug ("available disk space: ", Fmt.Int (disk_space), " megabytes");
        IF disk_space >= MinDiskSpace THEN EXIT; END;
        Out ("It appears that there is only about ", Fmt.Int (disk_space),
             " megabytes of space");
        Out ("in that directory.  CM3 requires about ",
             Fmt.Int (MinDiskSpace), "MB of disk space.");
        IF AskBool ("Do you want to use this directory anyway?", "N") THEN
          EXIT;
        END;
      END;
    END;

    (* set the various paths that we'll be using *)
    cm3_cfg     := OS.MakePath (install_root, "bin", "cm3.cfg");
    install_log := OS.MakePath (install_root, "Install.log");
    Msg.AttachDrain (install_log);

    (* process the cfg prototype *)
    Msg.Debug ("processsing config script");
    initial_cfg := GenConfig ();

    (* install the new cm3.cfg file before unpacking in case it chokes *)
    EVAL OS.MakeDir (Pathname.Prefix (cm3_cfg));
    OS.WriteFile (cm3_cfg, initial_cfg);

    (* uncompress and copy the bits *)
    Out ();
    Out ("Installing CM3 in: ", install_root);
    Out ("This may take a few minutes...");
    Unpack ("system");
    VAR 
      iter: FS.Iterator;
      fn: TEXT;
      stat: File.Status;
      archive_name, archive_path1: TEXT;
    BEGIN
      TRY
        iter := FS.Iterate(".");
        FOR i := FIRST(ARCHIVES) TO LAST(ARCHIVES) DO
          archive_name := ARCHIVES[i];
          Msg.Debug ("looking for archive: ", archive_name);
          WHILE iter.nextWithStatus(fn, stat) DO
            Msg.Debug ("  file ", fn);
            IF stat.type = RegularFile.FileType AND
              Glob.Match(archive_name, fn) THEN
              Msg.Debug ("    matching regular file");
              archive_path1 := OS.MakePath(cminstall_root, fn);
              IF OS.IsExecutable(archive_path1) THEN
                Unpack(fn);
              ELSE
                Msg.Debug ("not found: ", archive_name);
              END;
            END;
          END;
        END;
      EXCEPT
        OSError.E(e) => Msg.Error(e, "cannot list current directory");
      | RegEx.Error => Msg.Error(NIL, "invalid regular expression");
      END;
    END;
    (* Unpack (OS.MakePath ("..", "DOCS")); *)

    (* reinstall the new cm3.cfg file to make sure we've got the right one *)
    OS.WriteFile (cm3_cfg, initial_cfg);

(*******
    (* check out networking *)
    Out ();
    Out ("CM3 uses TCP/IP to communicate between a World-Wide Web browser");
    Out ("and the server.  I'll test that networking is installed, now.  This");
    Out ("test may take up to 20 seconds, please wait.");
    TestTCP ();
********)

    (* see if there are any last minute patches... *)
    RunFixups ();

    (* reminders *)
    Out ();
    Out ("CM3 is now installed.");
    Out ();
    Out ("Before you begin, here's a few reminders:");
    Out ();
    Out ("  1) The CM3 compiler executable is in:");
    Out ("        ", OS.MakePath (install_root, "bin", CM3_EXE));
    Out ("     You may need to modify your PATH environment variable to find it.");
  IF OnUnix THEN
    Out ("     And on Unix, you may need to type \"rehash\" to your shell.");
  END;
    Out ();
  IF OnUnix THEN
    Out ("  2) CM3's shared libraries and any you create and ship are in:");
    Out ("        ", OS.MakePath (install_root, "lib"));
    Out ("     On most Unix systems you need to set the LD_LIBRARY_PATH");
    Out ("     (on Darwin / MacOS X it's called DYLD_LIBRARY_PATH)");
    Out ("     environment variable before running programs that use");
    Out ("     these shared libraries.");
  ELSE
    Out ("  2) CM3's shared libraries and any you create and ship are in:");
    Out ("        ", OS.MakePath (install_root, "bin"));
    Out ("     As long as that directory is on your PATH, Windows will be able");
    Out ("     to find and use these libraries.");
  END;
    Out ();
    Out ("  3) Your system configuration file is:");
    Out ("        ", cm3_cfg);
    Out ("     At any point in time, you may edit it to modify or update your");
    Out ("     installation.");
    Out ();
    (* disabled
    Out ("  4) CM3 will keep your personal configuration information");
    Out ("     and private packages in \"HOME/proj\".  Be sure to set your");
    Out ("     HOME environment variable before running CM3.");
    Out ();
    *)
    Out ("  4) A copy of this installation dialogue is in:");
    Out ("        ", install_log);
    Out ();
    Out ("  5) If you had trouble with this installation or need more assistance,");
    Out ("     please send us a transcript of this installation via e-mail at");
    Out ("     \"m3-support@elego.de\".");
    Out ();
    Out ("Thank you.");

    (* finally, flush the log file *)
    Msg.FinishLog (install_log);
  END DoIt;

PROCEDURE ParseParams () =
  VAR i := 1;  arg: TEXT;
  BEGIN
    WHILE (i < Params.Count) DO
      arg := Params.Get (i);  INC (i);
      IF Text.Equal (arg, "-debug") THEN
        Msg.Debugging := TRUE;
      ELSIF Text.Equal (arg, "-root") THEN
        IF (i >= Params.Count) THEN
          Msg.Error (NIL, "Missing directory for \"-root <dir>\" option");
        END;
        cminstall_root := Params.Get (i);  INC (i);
      ELSE
        Msg.Error (NIL, "Unrecognized option: ", arg);
      END;
    END;

    IF (cminstall_root = NIL) THEN
      (* use the directory containing this executable *)
      cminstall_root := Pathname.Prefix (Params.Get (0));
    END;
    cminstall_root := OS.GetAbsolutePath (cminstall_root);
  END ParseParams;

(*-------------------------------------------- initial cm3.cfg generation ---*)

TYPE
  TK = QToken.T;
  LibFile = REF RECORD file: TEXT;  next: LibFile; END;

PROCEDURE LibFilesToText(l : LibFile) : TEXT =
  VAR res : TEXT;
  BEGIN
    IF l = NIL THEN RETURN "(none)" END;
    res := l.file;
    l := l.next;
    WHILE l # NIL DO
      res := res & " " & l.file;
      l := l.next;
    END;
    RETURN res;
  END LibFilesToText;

PROCEDURE OutResult (res : TEXT) =
  BEGIN
    IF res = NIL THEN
      Out (" not found");
    ELSE
      Out (" found ");
    END;
  END OutResult;

PROCEDURE GenConfig (): TEXT =
  <*FATAL Wr.Failure, Thread.Alerted*>
  TYPE
    Kind = { Any, Dir, Exe, LibPath };
  VAR
    config     := Bundle.Get (Setup.Get (), "config");
    wr         := TextWr.New ();
    scan       := NewScan (config);
    done       := 0;
    len        := Text.Length (config);
    begin_cfg  := Txt2ID ("BEGIN_CONFIG");
    end_cfg    := Txt2ID ("END_CONFIG");
    result     : TEXT;
    title      : TEXT;
    v0, v1, v2 : TEXT;
    confirm    : BOOLEAN;
    rule       : INTEGER;
    kind       : Kind;
    lib_files  : LibFile;
    lib_dirs   : TextSeq.T := NEW(TextSeq.T);
    choices    : TextSeq.T := NEW(TextSeq.T);
  BEGIN
    scan.next (); (* prime the token stream *)

    WHILE (done < len) DO

      (* scan up to the next config section *)
      WHILE (scan.token # TK.EOF)
        AND ((scan.token # TK.Name) OR (scan.string # begin_cfg)) DO
        scan.next ();
      END;
      IF (scan.start > done) THEN
        (* copy what we skipped, verbatim *)
        Wr.PutText (wr, Text.Sub (config, done, scan.start - done));
        done := scan.start;
      END;
      scan.next (); (* BEGIN_CONFIG *)

      IF (done >= len) THEN EXIT; END;
      EVAL choices.init();
      EVAL lib_dirs.init();

      (* get the config item's title *)
      IF (scan.token # TK.String) THEN
        ConfigErr (scan, "missing title");
      END;
      title := ID2Txt (scan.string);
      scan.next ();

      Out (title);

      kind := Kind.Any;  lib_files := NIL;
      WHILE (scan.token = TK.Cardinal) DO
        confirm := TRUE;
        result := NIL;

        rule := scan.cardinal;
        Msg.Debug (" => ", Fmt.Int (rule));
        scan.next ();  (* rule # *)

        CASE rule OF
        | 0 => (* exe-name *)
            v0 := GetTxt (scan);
            OutS ("checking for executable " & v0 & "...");
            result := OS.FindExecutable (v0);
            kind := Kind.Exe;
            OutResult (result);

        | 1 => (* file-extension *)
            v0 := GetTxt (scan);
            OutS ("registry lookup by extension for " & v0 & "...");
            result := Registry.LookupByExtension (v0);
            IF result # NIL THEN choices.addhi(result) END;
            kind := Kind.Exe;
            OutResult (result);

        | 2, 3 => (* file-extension, dir-name *)
            v0 := GetTxt (scan);
            v1 := GetTxt (scan);
            v2  := Registry.LookupByExtension (v0);
            OutS ("registry lookup by extension: file " & v0 & " dir: " &
              v1 & "...");
            IF v2 # NIL THEN
              v2 := OS.GetAbsolutePath (Pathname.Prefix (v2), v1);
              IF (v2 # NIL) AND OS.IsDirectory (v2) THEN
                result := v2;
              END;
            END;
            kind := Kind.Dir;
            OutResult (result);

        | 4 =>  (* env-variable, exe-name *)
            v0 := GetTxt (scan);
            v1 := GetTxt (scan);
            OutS ("checking for " & v1 & " at value of environment variable " &
              v0 & "...");
            v2 := Env.Get (v0);
            IF v2 # NIL THEN
              result := OS.FindExecutable (OS.MakePath (v2, v1));
            END;
            kind := Kind.Exe;
            OutResult (result);

        | 5 => (* exe-name, dir-name *)
            v0 := GetTxt (scan);
            v1 := GetTxt (scan);
            OutS ("checking for directory " & v1 & " with executable " & 
              v0 & "...");
            v2 := OS.FindExecutable (v0);
            IF (v2 # NIL) THEN
              v2 := OS.GetAbsolutePath (Pathname.Prefix (v2), v1);
              IF (v2 # NIL) AND OS.IsDirectory (v2) THEN
                result := v2;
              END;
            END;
            kind := Kind.Dir;
            OutResult (result);

        | 6 => (* dir-name, exe-name *)
            v0 := GetTxt (scan);
            v1 := GetTxt (scan);
            OutS ("checking for executable " & v1 & " in directory " & 
              v0 & "...");
            result := OS.FindExecutable (OS.MakePath (v0, v1));
            kind := Kind.Exe;
            OutResult (result);

        | 7 => (* install root *)
            result := OS.FilenameWithoutSpaces (install_root);
            kind := Kind.Dir;
            confirm := FALSE;
            Out ("setting INSTALL_ROOT to " & result);

        | 8 => (* dir-name *)
            v0 := GetTxt (scan);
            OutS ("checking for directory " & v0 & "...");
            IF OS.IsDirectory (v0) THEN
              result := v0;
            END;
            kind := Kind.Dir;
            OutResult (result);

        | 9 =>  (* env-variable, dir-name *)
            v0 := GetTxt (scan);
            v1 := GetTxt (scan);
            v2 := Env.Get (v0);
            OutS ("checking for directory " & v1 & 
              " with environment variable " & v0 & "...");
            IF v2 # NIL THEN
              v2 := OS.GetAbsolutePath (v2, v1);
              IF (v2 # NIL) AND OS.IsDirectory (v2) THEN
                result := v2;
              END;
            END;
            kind := Kind.Dir;
            OutResult (result);

        | 10 => (* env-variable *)
            v0 := GetTxt (scan);
            v1 := Env.Get (v0);
            OutS ("checking for executable " & 
              " with environment variable " & v0 & "...");
            IF (v1 # NIL) THEN
              result := OS.FindExecutable (v1);
            END;
            kind := Kind.Exe;
            OutResult (result);

        | 11 => (* file-name *)
            lib_files := NEW (LibFile, next := lib_files, 
                              file := GetTxt (scan));
            Out ("looking for library file(s): " & LibFilesToText(lib_files));

        | 12 => (* dir-name *)
            v0 := GetTxt (scan);
            IF v0 # NIL THEN
              OutS ("checking for library files in directory " & v0 & "...");
              lib_dirs.addhi(v0);
            END;
            IF (v0 # NIL) AND OS.IsDirectory (v0)
              AND FilesPresent (v0, lib_files) THEN
              result := v0;
            END;
            kind := Kind.LibPath;
            OutResult (result);

        | 13 => (* dir-name *)
            v0 := GetTxt (scan);
            OutS ("checking for directory " & v0 & "...");
            IF OS.IsDirectory (v0) THEN
              result := v0;
              lib_dirs.addhi(result);
            END;
            kind := Kind.LibPath;
            OutResult (result);

        | 14 => (* install key *)
            result := install_passwd;
            kind := Kind.Any;
            confirm := FALSE;

        ELSE
            ConfigErr (scan, "unknown key: " & Fmt.Int (scan.cardinal));
        END; (* CASE *)
        IF result # NIL AND NOT MemberOfTextSeq(choices, result) THEN
          choices.addhi(result)
        END;
      END; (* WHILE *)

      (* confirm with the user and stick it into the config file *)
      IF confirm THEN
        LOOP
          Out ();
          v0 := AskChoice (title, choices);
          CASE kind OF
          | Kind.Any =>
              EXIT;
          | Kind.Exe =>
              IF OS.IsExecutable (v0) THEN EXIT; END;
              v0 := OS.FilenameWithoutSpaces (v0);
              Out ();
              Out ("Please enter the name of an executable program.");
          | Kind.Dir =>
              v0 := OS.CleanDirName (v0);
              v0 := OS.FilenameWithoutSpaces (v0);
              IF OS.IsDirectory (v0) THEN EXIT; END;
              Out ();
              Out ("Please enter the name of a directory.");
          | Kind.LibPath =>
              v0 := OS.CleanDirName (v0);
              IF v0 = NIL THEN v0 := "" END;
              IF OS.IsDirectory (v0) THEN
                v0 := OS.FilenameWithoutSpaces (v0);
                IF FilesPresent (v0, lib_files) THEN
                  v0 := "-L" & v0;
                  EXIT;
                ELSE
                  Out ("The libraries " & LibFilesToText(lib_files) &
                    " are not present in the chosen directory.");
                  IF AskBool("Would you like to change the library names?",
                             "yes") THEN
                    Out ("Warning: Changing the required library names is ",
                         "currently only partially");
                    Out ("supported by the installer. You will have to edit ",
                         OS.MakePath(install_root, "bin", "cm3.cfg"));
                    Out ("manually after the installation and adapt the ",
                         "-l suffixes in the SYSTEM_LIBS");
                    Out ("array.");
                    Out ("Sorry for the inconvenience.");
                    lib_files := NIL;
                    v0 := Ask("enter library file (or ENTER to " &
                              "continue): ", NIL);
                    WHILE v0 # NIL AND NOT Text.Empty(v0) DO
                      lib_files := NEW (LibFile, next := lib_files, 
                                        file := v0);
                      v0 := Ask("enter library file (or ENTER to " &
                                "continue): ", NIL);
                    END;
                    IF AskBool("Would you like to add library search paths?",
                               "yes") THEN
                      v0 := Ask("enter library directory (or ENTER to " &
                                "continue): ", NIL);
                      WHILE v0 # NIL AND NOT Text.Empty(v0) DO
                        lib_dirs.addhi(v0);
                        v0 := Ask("enter library directory (or ENTER to " &
                                  "continue): ", NIL);
                      END;
                    END;
                    Out ("looking for library file(s): " & 
                      LibFilesToText(lib_files));
                    EVAL choices.init();
                    FOR i := 0 TO lib_dirs.size() - 1 DO
                      WITH dir = lib_dirs.get(i) DO
                        OutS ("checking in directory ", dir, "...");
                        IF OS.IsDirectory (dir) AND lib_files # NIL AND
                          FilesPresent (dir, lib_files) THEN
                          OutResult (dir);
                          IF NOT MemberOfTextSeq(choices, dir) THEN
                            choices.addhi(dir);
                          END;
                        ELSE
                          OutResult (NIL);
                        END;
                      END;
                    END;
                  ELSE
                    IF AskBool("Would you like to continue nonetheless?",
                               "yes") THEN
                      v0 := "-L" & v0;
                      EXIT;
                    END;
                    Out ();
                    Out ("Please enter the name of a directory.");
                  END;
                END;
              ELSE
                Out ();
                Out ("Please enter the name of a directory.");
              END;
          END; (* CASE *)
        END; (* LOOP *)
        Out ();
        result := v0;
      END;
      Wr.PutText (wr, Text2.EscapeString (result));

      (* skip to the end of the config section *)
      WHILE (scan.token # TK.EOF)
        AND ((scan.token # TK.Name) OR (scan.string # end_cfg)) DO
        scan.next ();
      END;
      done := scan.start + scan.length;
      scan.next (); (* END_CONFIG *)
    END;

    RETURN TextWr.ToText (wr);
  END GenConfig;

PROCEDURE FilesPresent (dir: TEXT;   files: LibFile): BOOLEAN =
  BEGIN
    WHILE (files # NIL) DO
      IF NOT OS.IsExecutable (OS.MakePath (dir, files.file)) THEN
        RETURN FALSE;
      END;
      files := files.next;
    END;
    RETURN TRUE;
  END FilesPresent;

PROCEDURE MemberOfTextSeq(tl : TextSeq.T; elem : TEXT) : BOOLEAN =
  BEGIN
    FOR i := 0 TO tl.size() - 1 DO
      WITH act = tl.get(i) DO
        IF Text.Equal(act, elem) THEN
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END MemberOfTextSeq;

(*--------------------------------------------- low-level quake support ---- *)

VAR
  quake_id_map := Quake.NewIDMap (Str2ID, Txt2ID, ID2Txt);

PROCEDURE NewScan (file: TEXT): QScanner.T =
  BEGIN
    RETURN NEW (QScanner.T).initText (file, quake_id_map);
  END NewScan;

PROCEDURE GetTxt (scan: QScanner.T): TEXT =
  VAR txt: TEXT;
  BEGIN
    IF scan.token # TK.String THEN
      ConfigErr (scan, "missing string");
    END;
    txt := ID2Txt (scan.string);
    scan.next ();
    RETURN txt;
  END GetTxt;

PROCEDURE ConfigErr (scan: QScanner.T;  msg: TEXT) =
  BEGIN
    Msg.Error (NIL, "Bad config file! --- ", msg, " --- token = ",
               QToken.Name [scan.token]);
  END ConfigErr;

PROCEDURE Str2ID (READONLY x: ARRAY OF CHAR): Quake.ID =
  BEGIN
    RETURN M3ID.FromStr (x);
  END Str2ID;

PROCEDURE Txt2ID (t: TEXT): Quake.ID =
  BEGIN
    RETURN M3ID.Add (t);
  END Txt2ID;

PROCEDURE ID2Txt (i: Quake.ID): TEXT =
  BEGIN
    RETURN M3ID.ToText (i);
  END ID2Txt;

(*------------------------------------------- decompression and unpacking ---*)
PROCEDURE UtilsFound() : BOOLEAN =
  BEGIN
    gzip := OS.MakePath (cminstall_root, GZIP_EXE);
    tar  := OS.MakePath (cminstall_root, TAR_EXE);
    IF NOT OS.IsExecutable(gzip) THEN
      gzip := OS.FindExecutable(GZIP_EXE);
      IF gzip = NIL THEN
        Msg.Out("Cannot find gzip.");
        Msg.Out("A workable gzip (de)compression program must be installed",
                "and found via PATH.");
        RETURN FALSE;
      END;
    END;
    Msg.Debug ("GZIP_EXE = ", gzip);
    IF NOT OS.IsExecutable(tar) THEN
      tar := OS.FindExecutable(TAR_EXE);
      IF tar = NIL THEN
        Msg.Out("Cannot find tar.");
        Msg.Out("A workable tar archiving program must be installed",
                "and found via PATH.");
        RETURN FALSE;
      END;
    END;
    Msg.Debug ("TAR_EXE = ", tar);
    RETURN TRUE;
  END UtilsFound; 

CONST
  GZipArgs = ARRAY [0..0] OF TEXT { "-dc" };
  TarArgs  = ARRAY [0..1] OF TEXT { "-xf", "-" };

PROCEDURE Unpack (archive: TEXT) =
  VAR data: TEXT := OS.MakePath (cminstall_root, archive);
  BEGIN
    IF OS.IsExecutable (data) THEN
      TRY
        IF Glob.Match("*.tar", data) THEN
          UnpackTAR (data);
        ELSIF Glob.Match("*.tar.gz", data) THEN
          UnpackTGZ (data);
        ELSIF Glob.Match("*.tgz", data) THEN
          UnpackTGZ (data);
        ELSE
          Msg.Error(NIL, "unknown archive type: ", data);
        END;
      EXCEPT ELSE END; (* cannot happen :-) *)
    ELSE
      IF OS.IsExecutable (data & ".tar") THEN 
        UnpackTAR (data & ".tar");
      ELSE
        UnpackTGZ (data & ".tgz");
      END;
    END;
  END Unpack;

PROCEDURE UnpackTAR (data: TEXT) =
  VAR
    tar_process    : Process.T;
    input, stdin   : File.T;
    stdout, stderr : File.T;
  BEGIN
    Msg.Debug ("unpacking:  archive = ", data);

    (* get the default file handles *)
    Process.GetStandardFileHandles (stdin, stdout, stderr);

    (* open the tar file *)
    TRY
      input := FS.OpenFileReadonly (data);
    EXCEPT OSError.E (ec) =>
      Msg.Error (ec, "Unable to open archive: ", data);
    END;

    (* create the tar process *)
    TRY
      tar_process := Process.Create (tar, TarArgs, stdin := input,
                                      stdout := stdout, stderr := stderr,
                                      wd := install_root);
    EXCEPT OSError.E (ec) =>
      Msg.Error (ec, "Unable to create unpacking process: ", tar);
    END;

    (* close our copy of the input file *)
    TRY
      input.close ();
    EXCEPT OSError.E (ec) =>
      Msg.Error (ec, "Trouble closing archive: ", data);
    END;

    (* wait for everybody to finish *)
    Msg.Debug ("waiting for process completion...");
    EVAL Process.Wait (tar_process);
    Msg.Debug ("unpacking done.");
  END UnpackTAR;

<*UNUSED*> PROCEDURE UnpackTGZWin32 (data: TEXT) =
  CONST
    TarArgs  = ARRAY [0..1] OF TEXT { "-zxmf", "-" };
  VAR
    tar_process    : Process.T;
    input, stdin   : File.T;
    stdout, stderr : File.T;
  BEGIN
    Msg.Debug ("unpacking:  archive = ", data);

    (* get the default file handles *)
    Process.GetStandardFileHandles (stdin, stdout, stderr);

    (* open the tar file *)
    TRY
      input := FS.OpenFileReadonly (data);
    EXCEPT OSError.E (ec) =>
      Msg.Error (ec, "Unable to open archive: ", data);
    END;

    (* create the tar process *)
    TRY
      tar_process := Process.Create (tar, TarArgs, stdin := input,
                                      stdout := stdout, stderr := stderr,
                                      wd := install_root);
    EXCEPT OSError.E (ec) =>
      Msg.Error (ec, "Unable to create unpacking process: ", tar);
    END;

    (* close our copy of the input file *)
    TRY
      input.close ();
    EXCEPT OSError.E (ec) =>
      Msg.Error (ec, "Trouble closing archive: ", data);
    END;

    (* wait for everybody to finish *)
    Msg.Debug ("waiting for process completion...");
    EVAL Process.Wait (tar_process);
    Msg.Debug ("unpacking done.");
  END UnpackTGZWin32;

PROCEDURE UnpackTGZ (data: TEXT) =
  VAR
    gzip_process   : Process.T;
    tar_process    : Process.T;
    p_in, p_out    : Pipe.T;
    input, stdin   : File.T;
    stdout, stderr : File.T;
  BEGIN
    Msg.Debug ("unpacking:  archive = ", data);
      
    (* get the default file handles *)
    Process.GetStandardFileHandles (stdin, stdout, stderr);

    (* open the compressed tar file *)
    TRY
      input := FS.OpenFileReadonly (data);
    EXCEPT OSError.E (ec) =>
      Msg.Error (ec, "Unable to open compressed archive: ", data);
    END;

    (* create a pipe for the gzip->tar connection *)
    TRY
      Pipe.Open (hr := p_out, hw := p_in);
    EXCEPT OSError.E (ec) =>
      Msg.Error (ec, "Unable to open a pipe for the decompressor");
    END;

    (* create the gzip process reading from the tar file *)
    TRY
      gzip_process := Process.Create (gzip, GZipArgs, stdin := input,
                                      stdout := p_in, stderr := stderr);
      Msg.Debug (gzip, " ", GZipArgs[0]);
    EXCEPT OSError.E (ec) =>
      Msg.Error (ec, "Unable to create decompression process: ", gzip);
    END;

    (* create the tar process *)
    TRY
      tar_process := Process.Create (tar, TarArgs, stdin := p_out,
                                      stdout := stdout, stderr := stderr,
                                      wd := install_root);
      Msg.Debug (tar, " " & TarArgs[0] & " " & TarArgs[1] & " wd = ",
                 install_root);
    EXCEPT OSError.E (ec) =>
      Msg.Error (ec, "Unable to create unpacking process: ", tar);
    END;

    (* close our copies of the pipe ends *)
    TRY
      p_in.close ();   p_out.close ();
    EXCEPT OSError.E (ec) =>
      Msg.Error (ec, "Trouble closing decompression pipe");
    END;

    (* close our copy of the input file *)
    TRY
      input.close ();
    EXCEPT OSError.E (ec) =>
      Msg.Error (ec, "Trouble closing compressed archive: ", data);
    END;

    (* wait for everybody to finish *)
    Msg.Debug ("waiting for process completion...");
    EVAL Process.Wait (gzip_process);
    EVAL Process.Wait (tar_process);
    Msg.Debug ("unpacking done.");
  END UnpackTGZ;

(*---------------------------------------------------- last minute fixups ---*)

PROCEDURE RunFixups () =
  VAR
    fixup          : TEXT := OS.MakePath (cminstall_root, FIXUP_EXE);
    stdin          : File.T;
    stdout, stderr : File.T;
    fixup_process  : Process.T;
    fixup_args     : ARRAY [0..0] OF TEXT;
  BEGIN
    TRY
      EVAL FS.Status (fixup);
    EXCEPT OSError.E =>
      RETURN; (* no fixup file *)
    END;

    Msg.Debug ("starting fixup: ", fixup, "...");

    (* get the default file handles *)
    Process.GetStandardFileHandles (stdin, stdout, stderr);

    (* create the fixup process *)
    TRY
      fixup_args[0] := install_root;
      fixup_process := Process.Create (fixup, fixup_args, stdin := stdin,
                                       stdout := stdout, stderr := stderr,
                                       wd := install_root);
    EXCEPT OSError.E (ec) =>
      Msg.Error (ec, "Unable to create fixup process: ", fixup);
    END;

    (* wait for it to finish *)
    Msg.Debug ("waiting for process completion...");
    EVAL Process.Wait (fixup_process);
    Msg.Debug ("fixup done.");
  END RunFixups;

(*------------------------------------------------------------ decryption ---*)
(* disabled
PROCEDURE KeyCheck (passwd: TEXT): BOOLEAN =
  CONST Day = 24.0d0 * 3600.0d0;
  CONST FirstWarning = 7.0d0 * Day;
  VAR key := CMKey.Decode (passwd);  expire: Time.T;
  BEGIN
    IF (key.usage = CMKey.Usage.Invalid)
    OR (key.version # CMCurrent.Version)
    OR (key.platform # CMCurrent.Platform) THEN
      RETURN FALSE;
    END;

    Out ("CM3 Installation: ", key.banner);

    IF (key.usage = CMKey.Usage.Demo) THEN
      expire := key.expiration - Time.Now ();
      IF (expire > FirstWarning) THEN
        (* ok *)
      ELSIF (expire <= 0.0d0) THEN
        Out ("---");
        Out ("--- This preview copy of CM3 has already expired.");
        BuyIt ();
        Process.Exit (1);
      ELSE
        Out ("---");
        Out ("--- Warning: this preview copy of CM3 will expire in ",
                    Fmt.Int (ROUND (expire / Day)), " days.");
        BuyIt ();
      END;
    END;

    RETURN TRUE;
  END KeyCheck;

CONST
  BuyMsg = ARRAY OF TEXT {
    "To purchase a non-expiring copy of CM3, please contact:",
    "",
    "    Critical Mass, Inc.",
    "    1770 Massachusetts Ave.",
    "    Cambridge, MA 02140 USA",
    "",
    "    Telephone:  1 617 354 MASS    E-mail:  info@cmass.com",
    "    Fax:        1 617 354 5027    Web:     www.cmass.com",
    ""
  };

PROCEDURE BuyIt () =
  BEGIN
    FOR i := FIRST (BuyMsg) TO LAST (BuyMsg) DO
      Out ("--- ", BuyMsg[i]);
    END;
  END BuyIt;
*)

(*---------------------------------------------------------- network test ---*)

<* UNUSED *> PROCEDURE TestTCP () =
  BEGIN
    Msg.Debug ("Testing network connections");
    Msg.Debug ("Network test done.");
  END TestTCP;

BEGIN
  DoIt ();
END Main.
