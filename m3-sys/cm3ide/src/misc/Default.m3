
MODULE Default;

IMPORT Params, Text, ErrLog;
IMPORT Env, Process, Fmt, IP, Pathname, FS;
IMPORT Thread, IO, TextList, OSError, Wr;
IMPORT Quake, QCompiler, QIdent, QMachine, QValue;
IMPORT ConfigItem, ID, MxConfig, M3File, PkgRoot, UserState;

TYPE CI = ConfigItem.T;

(*-------------------------------------------------- initialization ---*)

VAR init_done := FALSE;

PROCEDURE Init () =
  CONST Slash   = ARRAY BOOLEAN OF TEXT { "\134", "/" };
  CONST PathSep = ARRAY BOOLEAN OF CHAR { ';',    ':' };
  VAR got_addr: BOOLEAN;
  BEGIN
    IF (init_done) THEN RETURN; END;
    init_done := TRUE;

    (* host dependent configuration *)
    on_unix  := Text.Equal ("a/b", Pathname.Join ("a", "b", NIL));
    slash    := Slash [on_unix];
    path_sep := PathSep [on_unix];

    (* cm3 configuration *)
    IF MxConfig.FindFile () = NIL THEN
      Die ("Unable to locate a cm3 configuration file.", Wr.EOL,
           "Apparently \"cm3\" is not on your search path.");
    END;

    build_dir       := MxConfig.Get ("BUILD_DIR");
    system_root     := MxConfig.Get ("PKG_USE"); (* note: system_root is the public package root *)
    doc_root        := MxConfig.Get ("DOC_INSTALL");
    initial_browser := MxConfig.Get ("INITIAL_CM3_IDE_BROWSER");
    initial_editor  := MxConfig.Get ("INITIAL_CM3_IDE_EDITOR");

(* Following code added by RCC to attempt to find/construct certain critical roots if they weren't found above *)
(* Note that the typical installation's pathname hierarchy (using POSIX-style path syntax) is:
|     somePath/cm3          = cm3 installation root
|     somePath/cm3/bin      = executables
|     somePath/cm3/pkg      = public pkg root
|     somePath/cm3/doc      = doc root
|     somePath/cm3/examples = examples root
*)
   IF (build_dir = NIL)
   THEN (* attempt to substitute 'TARGET' for 'BUILD_DIR' *)
      WITH p = MxConfig.Get ("TARGET") DO
         IF (p # NIL) THEN
            build_dir :=  p;
            ErrLog.Msg ("CAUTION:  BUILD_DIR not defined in cm3.cfg, setting it to TARGET:  ", build_dir);
         END;
      END;
   END;
   IF (system_root = NIL)
   THEN (* attempt to construct pkg root from 'INSTALL_ROOT' *)
      WITH p = MxConfig.Get ("INSTALL_ROOT") DO
         IF (p # NIL) THEN
            system_root := Pathname.Join (p, "pkg", NIL);
            IF M3File.IsDirectory(system_root) THEN
               ErrLog.Msg ("CAUTION:  PKG_USE not defined in cm3.cfg, constructed it from INSTALL_ROOT as:  ", system_root);
            ELSE
               system_root := NIL;
            END;
         END;
      END;
   END;
   IF (system_root = NIL)
   THEN (* attempt to construct pkg root using the path to the cm3.cfg *)
      TRY
         WITH p = MxConfig.FindFile (), (* earlier code checked that result is not NIL *)
              folder = Pathname.Prefix (FS.GetAbsolutePathname(p)),
              parent = Pathname.Prefix (folder),
              tryPkgRoot = Pathname.Join (parent, "pkg", NIL)
         DO
            IF M3File.IsDirectory(tryPkgRoot) THEN
               system_root := tryPkgRoot;
               ErrLog.Msg ("CAUTION:  PKG_USE not defined in cm3.cfg, constructed it from cm3.cfg path as:  ", system_root);
            END;
         END;
      EXCEPT
       | OSError.E => Die ("Unable to get absolute pathname to cm3.cfg file.");
      END;
   END;
   IF (system_root = NIL)
   THEN (* attempt to construct pkg root using the path to the currently executing program *)
      TRY
         WITH p = Params.Get (0),
              folder = Pathname.Prefix (FS.GetAbsolutePathname(p)),
              parent = Pathname.Prefix (folder),
              tryPkgRoot = Pathname.Join (parent, "pkg", NIL)
         DO
            IF M3File.IsDirectory(tryPkgRoot) THEN
               system_root := tryPkgRoot;
               ErrLog.Msg ("CAUTION:  PKG_USE not defined in cm3.cfg, constructed it from executable path as:  ", system_root);
            END;
         END;
      EXCEPT
       | OSError.E => Die ("Unable to get absolute pathname to currently executing program.");
      END;
   END;
   IF (doc_root = NIL) AND (system_root # NIL) THEN
      doc_root := Pathname.Join (Pathname.Prefix (system_root), "doc", NIL);
      ErrLog.Msg ("CAUTION:  DOC_INSTALL not defined in cm3.cfg, constructed it from package root as:  ", doc_root);
   END;
   IF (system_root # NIL)
   THEN
      example_root := Pathname.Join (Pathname.Prefix (system_root), "examples", NIL);
   ELSE
      example_root := NIL;
   END;
   IF (example_root = NIL) OR (NOT M3File.IsDirectory(example_root)) THEN
      example_root := NIL;
      ErrLog.Msg ("NOTICE:  Unable to locate 'examples' folder.");
   END;
(* End code by RCC *)

    IF (build_dir = NIL) THEN
      Die ("The build directory was not defined.  Either the configuration"
         & Wr.EOL & "file, ", MxConfig.FindFile (), ", contains a syntax error or"
         & Wr.EOL & "it doesn't define BUILD_DIR.");
    END;

    IF (system_root = NIL) THEN
      Die ("The system package root was not defined.  Either the configuration"
         & Wr.EOL & "file, ", MxConfig.FindFile (), ", contains a syntax error or"
         & Wr.EOL & "it doesn't define PKG_USE.");
    END;

    (* CM3-IDE configuration *)
    LocateCM3_IDEConfig ();
    ParseCmdLine ();
    ReadCM3_IDEConfig ();

    (* finally, fix up the IP address, machine name, and server URL *)
    WITH
      verbose = ConfigItem.X[CI.Verbose_log].bool,
      machine = ConfigItem.X[CI.Server_machine].text,
      ip_addr = ConfigItem.X[CI.IP_address].addr,
      socket  = ConfigItem.X[CI.Server_port].int
    DO
      IF (machine # NIL) AND Text.Length (machine) < 1 THEN machine := NIL; END;

      IF (machine = NIL) AND (ip_addr = IP.NullAddress) THEN
        IF verbose THEN
          ErrLog.Msg ("getting hostname and IP address from system...");
        END;
        TRY
          ip_addr := IP.GetHostAddr ();
          machine := IP.GetCanonicalByAddr (ip_addr);
        EXCEPT IP.Error =>
          machine := NIL;
        END;
        IF (machine = NIL) THEN
          Die ("CM3-IDE is unable to get host machine's name.",  Wr.EOL,
               "Perhaps networking is not enabled on your system?");
        END;
      ELSIF machine # NIL THEN
        IF verbose THEN ErrLog.Msg ("getting IP address from hostname..."); END;
        TRY
          got_addr := IP.GetHostByName (machine, ip_addr);
        EXCEPT IP.Error =>
          got_addr := FALSE;
        END;
        IF NOT got_addr THEN
          Die ("CM3-IDE is unable to get "& machine &"'s IP Address.", Wr.EOL,
               "Perhaps networking is not enabled on your system?");
        END;
      ELSE
        IF verbose THEN ErrLog.Msg ("getting hostname from IP address..."); END;
        TRY
          machine := IP.GetCanonicalByAddr (ip_addr);
        EXCEPT IP.Error =>
          machine := NIL;
        END;
        IF (machine = NIL) THEN
          Die ("CM3-IDE is unable to get host machine's name.", Wr.EOL,
               "Perhaps networking is not enabled on your system?");
        END;
      END;

      server_href := "http://" & machine & ":" & Fmt.Int (socket) & "/";
      IF verbose THEN ErrLog.Msg ("root URL:  ", server_href); END;
    END;
  END Init;

(*-------------------------------------------------------- config files ---*)

CONST
  ConfigDir  = "proj";

PROCEDURE LocateCM3_IDEConfig () =
  VAR home, rhome, dir, wd, t1, t2: TEXT;
  BEGIN
    dir   := NIL;
    home  := Env.Get ("HOME");
    rhome := Env.Get ("CM3_IDE_HOME");

    IF (rhome # NIL) THEN
      IF Pathname.Absolute (rhome) AND IsDir (rhome) THEN
        dir := rhome;
      ELSE (* see if we can use "." or $HOME for the root *)
        TRY wd := Process.GetWorkingDirectory ();
        EXCEPT OSError.E => wd := NIL;
        END;
        t1 := Pathname.Join (wd, rhome, NIL);
        t2 := Pathname.Join (home, rhome, NIL);
        IF    (wd   # NIL) AND IsDir (t1) THEN dir := t1;
        ELSIF (home # NIL) AND IsDir (t2) THEN dir := t2;
        END;
      END;
      IF (dir = NIL) THEN (* $CM3_IDE_HOME isn't useful *)
        ErrLog.Msg ("$CM3_IDE_HOME = \"", rhome, "\" is not a useable directory.");
      END;
    END;

    IF (dir = NIL) AND (home # NIL) THEN
      t1 := Pathname.Join (home, ConfigDir, NIL);
      IF IsDir (t1)
        THEN dir := t1;
        ELSE ErrLog.Msg ("\"", t1, "\" is not a useable directory.");
      END;
    END;

    IF (dir = NIL) THEN
      ErrLog.Msg (
        "Unable to find a working directory using $HOME or $CM3_IDE_HOME,");
      TRY
        t1 := Process.GetWorkingDirectory ();
      EXCEPT OSError.E =>
        t1 := ".";  (* ouch *)
      END;
      ErrLog.Msg ("trying the current directory, \"", t1, "\" ...");
      t1 := Pathname.Join (t1, ConfigDir, NIL);
      IF IsDir (t1) THEN dir := t1; END;
    END;

    IF (dir = NIL) THEN
      ErrLog.Msg (Wr.EOL, "Unable to find a suitable working directory.", Wr.EOL,
                  "Your personal CM3-IDE configuration will not be saved.");
      ErrLog.Msg (Wr.EOL);
    END;

    user_home := dir;
    UserState.Init (dir);
  END LocateCM3_IDEConfig;

PROCEDURE ReadCM3_IDEConfig () =
  VAR txt: TEXT;
  BEGIN
    (* inhale the predefined configuration settings *)
    FOR ci := FIRST (CI) TO LAST (CI) DO
      txt := UserState.Get (ConfigItem.Desc[ci].name);
      ConfigItem.Set (ci, txt);
    END;

    (* check for user-defined package roots *)
    VAR n_roots := 0;  key, name, path, b: TEXT;  build: BOOLEAN;  BEGIN
      LOOP
        key := "root-" & Fmt.Int (n_roots);
        name := UserState.Get (key & "-name");
        path := UserState.Get (key & "-path");
        b := UserState.Get (key & "-build");
        build := (b # NIL) AND Text.Equal (b, "TRUE");
        IF (name = NIL) OR (path = NIL) THEN EXIT; END;
        PkgRoot.Add (name, path, build);
        INC (n_roots);
      END;

      IF (n_roots <= 0) THEN
        (* no user roots => add the default system roots *)
        IF (user_home # NIL) THEN
          name := "proj";
          path := user_home;
          UserState.Put ("root-0-name", name);
          UserState.Put ("root-0-path", path);
          UserState.Put ("root-0-build", "TRUE");
          PkgRoot.Add (name, path, TRUE);
          name := "public";
          path := system_root;
          UserState.Put ("root-1-name", name);
          UserState.Put ("root-1-path", path);
          UserState.Put ("root-1-build", "FALSE");
          PkgRoot.Add (name, path, FALSE);
        ELSE
          name := "public";
          path := system_root;
          UserState.Put ("root-0-name", name);
          UserState.Put ("root-0-path", path);
          UserState.Put ("root-0-build", "FALSE");
          PkgRoot.Add (name, path, FALSE);
        END;
      END;
    END;
  END ReadCM3_IDEConfig;

PROCEDURE IsDir (dir: TEXT): BOOLEAN =
  BEGIN
    IF M3File.IsDirectory (dir) THEN RETURN TRUE; END;
    TRY
      FS.CreateDirectory (dir);
      RETURN TRUE;
    EXCEPT OSError.E =>
      RETURN FALSE;
    END;
  END IsDir;

(*-------------------------------------------------- command line parsing ---*)

PROCEDURE ParseCmdLine () =
  VAR args: TextList.T := NIL;
  BEGIN
    FOR i := 1 TO Params.Count - 1 DO
      args := TextList.Cons (Params.Get (i), args);
    END;
    ProcessArgs (TextList.ReverseD (args));
  END ParseCmdLine;

PROCEDURE ProcessArgs (args: TextList.T) =
  VAR parm: TEXT;
  BEGIN 
    WHILE (args # NIL) DO
      parm := PopArg (args);
      IF Text.Equal (parm, "-help") OR Text.Equal (parm, "-h") THEN
        PrintHelp();
        Process.Exit(0);
      ELSIF Text.Equal (parm, "-version") OR Text.Equal (parm, "-v") THEN
        PrintVersion();
        Process.Exit(0);
      ELSIF Text.Equal (parm, "-browser") THEN
        ConfigItem.SetExecutable (CI.Start_browser, PopArg (args));
      ELSIF Text.Equal (parm, "-editor") THEN
        ConfigItem.SetExecutable (CI.Edit_file, PopArg (args));
      ELSIF Text.Equal (parm, "-workers") THEN
        ConfigItem.Set (CI.Num_server_threads, PopArg (args));
      ELSIF Text.Equal (parm, "-port") THEN
        ConfigItem.Set (CI.Server_port, PopArg (args));
      ELSIF Text.Equal (parm, "-server") THEN
        ConfigItem.Set (CI.Server_machine, PopArg (args));
      ELSIF Text.Equal (parm, "-refresh") THEN
        ConfigItem.Set (CI.Refresh_interval, PopArg (args));
      ELSIF Text.Equal (parm, "-system") THEN
        system_root := PopArg (args);
      ELSIF Text.Equal (parm, "-verbose") THEN
        ConfigItem.Set (CI.Verbose_log, "TRUE");
      ELSIF Text.Equal (parm, "") THEN
        (* ignore blank lines *)
      ELSE
        PrintHelp();
        Die ("Unrecognized option: \"", parm, "\"");
      END;
    END;
  END ProcessArgs;

PROCEDURE PrintHelp () =
  VAR
    msg: TEXT;
  BEGIN
    msg := 
        "CM3-IDE [options]" & Wr.EOL &
        "  where options are" & Wr.EOL & 
        "  -help                this message" & Wr.EOL &
        "  -version             print CM3_IDE version number" & Wr.EOL &
        "  -browser <prog>      use web browser <prog>" & Wr.EOL &
        "  -editor <prog>       use text editor <prog>" & Wr.EOL &
        "  -port <num>          use HTTP socket <num>" & Wr.EOL &
        "  -refresh <seconds>   set refresh interval" & Wr.EOL & 
        "  -system <path>       set location of public packages" & Wr.EOL & 
        Wr.EOL;

    IO.Put (msg);
  END PrintHelp;

PROCEDURE PrintVersion() = 
  BEGIN
    IO.Put ("CM3_IDE (tm) by Critical Mass, Inc. " &
            "Version 4.1 for " & build_dir & Wr.EOL);
  END PrintVersion;

PROCEDURE PopArg (VAR args: TextList.T): TEXT =
  VAR txt: TEXT;
  BEGIN
    IF (args = NIL) THEN RETURN ""; END;
    txt := args.head;
    args := args.tail;
    RETURN txt;
  END PopArg;

(*----------------------------- user-defined config file procedures ---*)

VAR
  id_map := Quake.NewIDMap (Str2ID, Txt2ID, ID2Txt);

PROCEDURE GetConfigProc (ci   : CI;
              VAR(*OUT*) m    : Quake.Machine;
              VAR(*OUT*) proc : QValue.T)
  RAISES {Thread.Alerted} =
  CONST ConfigFile = "<CM3-IDE configuration>";
  VAR
    bind: QValue.Binding;
    nm   := ConfigItem.Desc[ci].name;
    body := ConfigItem.ToText (ci);
  BEGIN
    <*ASSERT ConfigItem.Desc[ci].kind = ConfigItem.Kind.Proc*>
    IF (body = NIL) THEN
      ErrLog.Msg ("The current configuration does not define \"", nm, "\"");
      m := NIL;
      RETURN;
    END;

    m := Quake.NewMachine (id_map);
    TRY
      m.evaluate (QCompiler.CompileText (ConfigFile, body, id_map));
    EXCEPT Quake.Error (msg) =>
      ErrLog.Msg ("Unable to evaluate configuration procedure \"", nm,
                  "\":" & Wr.EOL, msg);
      m := NIL;
      RETURN;
    END;

    bind := m.lookup (m.map.txt2id (nm));
    IF (bind = NIL) OR (bind.value.kind # QValue.Kind.Proc) THEN
      ErrLog.Msg ("Configuration does not define the required procedure: ", nm);
      m := NIL;
      RETURN;
    END;

    proc := bind.value;
  END GetConfigProc;

PROCEDURE Str2ID (READONLY x: ARRAY OF CHAR): Quake.ID =
  BEGIN
    RETURN ID.FromStr (x);
  END Str2ID;

PROCEDURE Txt2ID (t: TEXT): Quake.ID =
  BEGIN
    RETURN ID.Add (t);
  END Txt2ID;

PROCEDURE ID2Txt (i: Quake.ID): TEXT =
  BEGIN
    RETURN ID.ToText (i);
  END ID2Txt;


(*------------------------------------------------------------ misc ---*)

PROCEDURE Die (a, b, c, d: TEXT := NIL) =
  BEGIN
    ErrLog.Msg (a, b, c, d);
    Process.Exit (1);
  END Die;

BEGIN
  Init ();
END Default.
