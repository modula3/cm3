(*--------------------------------------------------------------------------*)
MODULE MiniEnv;

IMPORT ASCII, Env, MxConfig, Pathname, Process, Rd, Thread;
IMPORT SMsg AS Msg, TextUtils, FSUtils, PathRepr, System;
IMPORT CompactEnvName;
(* IMPORT IO; *)

PROCEDURE FindDir(READONLY guesses : ARRAY OF TEXT) : TEXT = 
  BEGIN
    FOR i := 0 TO NUMBER(guesses) - 1 DO
      WITH guess = PathRepr.Native(guesses[i]) DO
        IF FSUtils.IsDir(guess) THEN
          RETURN guess;
        END;
      END;
    END;
    RETURN NIL;
  END FindDir;

PROCEDURE UnameOutput(arg : TEXT) : TEXT = 
  VAR
    rd  : Rd.T;
    p   : Process.T;
    res : TEXT;
  BEGIN
    TRY
      p := System.RdExecute("uname " & arg, rd);
      res := Rd.GetLine(rd);
      EVAL Process.Wait(p);
    EXCEPT
      System.ExecuteError => RETURN NIL;
    | Thread.Alerted => RETURN NIL;
    | Rd.EndOfFile => RETURN NIL;
    | Rd.Failure => RETURN NIL;
    END;
    RETURN res;
  END UnameOutput;

VAR
  val : TEXT;
  compactRootGuesses := ARRAY [0..7] OF TEXT {
    "/usr/contrib/lib/compact",
    "/usr/local/lib/compact",
    "/usr/local/cm3/compact",
    "/lib/compact",
    "/opt/compact",
    "/opt/cm3/compact",
    "c:/compact",
    "d:/compact"
  };
  tmpDirGuesses := ARRAY [0..7] OF TEXT {
    "/tmp",
    "/var/tmp",
    "/usr/tmp",
    "c:/tmp",
    "d:/tmp",
    "c:/temp",
    "d:/temp",
    "/"
  };
BEGIN
  isPosix := MxConfig.HOST_OS_TYPE() = MxConfig.OS_TYPE.POSIX;
  isWin32 := MxConfig.HOST_OS_TYPE() = MxConfig.OS_TYPE.WIN32;

  tmpdir := Env.Get("TMPDIR");
  IF tmpdir = NIL THEN
    tmpdir := Env.Get("TMP");
    IF tmpdir = NIL THEN
      tmpdir := FindDir(tmpDirGuesses);
    END;
    IF tmpdir = NIL THEN
      Msg.Fatal("cannot find any tmp directory");
    END;
  END;
  tmpdir := PathRepr.Native(tmpdir);
  compactroot := Env.Get("COMPACTROOT");
  IF compactroot = NIL THEN
    compactroot := FindDir(compactRootGuesses);
    IF compactroot = NIL THEN
      Msg.Warning("COMPACTROOT is undefined and has not been found at " &
        "any default location.");
      compactroot := tmpdir;
      Msg.Warning("COMPACTROOT set to " & compactroot);
    END;
  END;
  compactroot := PathRepr.Native(compactroot);
  home := Env.Get("HOME");
  IF home = NIL THEN
    home := Env.Get("HOMEPATH");
    IF home = NIL THEN
      home := Pathname.Join(compactroot, "home", NIL);
      IF NOT FSUtils.IsDir(home) THEN
        home := Pathname.Join(compactroot, "demo", NIL);
        home := Pathname.Join(home, "workspace", NIL);
      END;
      IF NOT FSUtils.IsDir(home) THEN
        home := compactroot;
      END;
      Msg.Warning("HOME undefined, set to " & home);
    ELSE
      IF Env.Get("HOMEDRIVE") # NIL THEN
        home := Env.Get("HOMEDRIVE") & home;
      END;
    END;
  END;
  home := PathRepr.Native(home);
  user := Env.Get("USER");
  IF user = NIL THEN
    user := Env.Get("LOGNAME");
    IF user = NIL THEN
      user := Env.Get("USERNAME");
      IF user = NIL THEN
        user := ("nobody");
        Msg.Warning("USER undefined, set to " & user);
      END;
    END;
  END;

  editor := Env.Get("VISUAL");
  IF editor = NIL THEN
    editor := Env.Get("EDITOR");
    IF editor = NIL THEN
      IF isPosix THEN
        editor := "xterm -e vi";
      ELSIF isWin32 THEN
        editor := "notepad";
      ELSE
        editor := "edit";
      END;
    END;
  END;

  editorovr := Env.Get("COMPACTEDITOR");
  IF editorovr = NIL THEN
    editorovr := Env.Get("COMPACT_EDITOR");
    (* This may be used to override values in compactrc. *)
  END;

  httpdeditorovr := Env.Get("COMPACTHTTPDEDITOR");
  IF httpdeditorovr = NIL THEN
    httpdeditorovr := Env.Get("COMPACT_HTTPD_EDITOR");
    (* This may be used to override values in compactrc. *)
  END;

  val := Env.Get("HOSTTYPE");
  IF val # NIL THEN
    tpc_hosttype := TextUtils.Lower(val);
    (* IO.Put("tpc_hosttype (1) " & tpc_hosttype & "\n"); *)
  ELSIF isPosix THEN
    tpc_hosttype := UnameOutput("-m");
    (* IO.Put("tpc_hosttype (2) " & tpc_hosttype & "\n"); *)
  ELSIF isWin32 THEN
    (* On NT, we should have "Cpu" set to something like "i386". *)
    val := Env.Get("Cpu");
    IF val # NIL THEN
      tpc_hosttype := TextUtils.Lower(val);
    ELSE (* FIXME: just an assumption *)
      tpc_hosttype := "i486";
    END;
  END;
  IF tpc_hosttype = NIL THEN tpc_hosttype := "" END;
  tpc_hosttype := TextUtils.RemoveChars(tpc_hosttype, ASCII.Set{'-'});
  (* IO.Put("tpc_hosttype (3) " & tpc_hosttype & "\n"); *)

  val := Env.Get("OSTYPE");
  IF val # NIL THEN
    tpc_ostype := TextUtils.Lower(val);
  ELSIF isPosix THEN
    val := UnameOutput("-s");
    IF val = NIL THEN
      val := Env.Get("TARGET");
      IF val = NIL THEN
        val := Env.Get("CM3_TARGET");
      ELSE
        val := "unknown";
      END
    ELSE
    END;
    tpc_ostype := TextUtils.Lower(val);
  ELSIF isWin32 THEN
    (* On NT, we should have "OS" set to "Windows_NT", which ComPact
       calls "winnt". *)
    val := Env.Get("OS");
    IF val # NIL THEN
      tpc_ostype := TextUtils.Lower(val);
    ELSE (* FIXME: just an assumption *)
      tpc_ostype := "nt";
    END;
  END;
  tpc_ostype := TextUtils.RemoveChars(tpc_ostype, ASCII.Set{'-'});

  val := Env.Get("TPC_VARIANT");
  IF val # NIL THEN
    tpc_variant := TextUtils.Lower(val);
  ELSE
    IF isPosix THEN
      tpc_variant := "";
    ELSIF isWin32 THEN
      (* FIXME: just an assumption *)
      IF TextUtils.Contains(tpc_ostype, "cygwin") THEN
        tpc_variant := "cygwin32";
      ELSE
        tpc_variant := "win32";
      END;
    END;
  END;
  tpc_variant := TextUtils.RemoveChars(tpc_variant, ASCII.Set{'-'});

  val := Env.Get("TPC_COMPILER");
  IF val # NIL THEN
    tpc_compiler := TextUtils.Lower(val);
  ELSE
    IF isPosix THEN
      tpc_compiler := "gcc";
    ELSIF isWin32 THEN
      (* FIXME: just an assumption *)
      IF TextUtils.Contains(tpc_ostype, "cygwin") THEN
        tpc_compiler := "gcc";
      ELSE
        tpc_compiler := "cl";
      END;
    END;
  END;
  val := Env.Get("TPC_OPTIONS");
  IF val # NIL THEN
    tpc_options := TextUtils.Lower(val);
  END;
  tpc_compiler := TextUtils.RemoveChars(tpc_compiler, ASCII.Set{'-'});

  val := Env.Get(CompactEnvName.Passphrase);
  IF val # NIL THEN
    pass := val;
  END;

END MiniEnv.
