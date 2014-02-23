(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE Main;

IMPORT M3Timers, Pathname, Process, Quake;
IMPORT RTCollector, RTParams, RTutils, Thread, Wr;
IMPORT TextTextTbl;

IMPORT Builder, Dirs, M3Build, M3Options, Makefile, Msg, Utils, WebFile;
IMPORT MxConfig(*, M3Config, CMKey, CMCurrent *);
(* IMPORT Fmt, Time; only needed for key and expiration check *)
(* IMPORT Version; *)

VAR
  config    : TEXT          := NIL;
  makefile  : TEXT          := NIL;
  build_dir : TEXT          := NIL;
  mach      : Quake.Machine := NIL;

PROCEDURE DefineIfNotDefined (qmachine: Quake.Machine;
                              symbol, value: TEXT) RAISES {Quake.Error} =
BEGIN
    IF Quake.LookUp (qmachine, symbol) = NIL THEN
        Quake.Define (qmachine, symbol, value);
    END;
END DefineIfNotDefined;
        
PROCEDURE DoIt () RAISES {Wr.Failure} =
VAR defs: TextTextTbl.T;
  BEGIN
    IF RTParams.IsPresent ("verbose") THEN
      Msg.SetLevel (Msg.Level.Verbose);
      M3Timers.Start ();
    END;
    IF RTParams.IsPresent ("debug")   THEN
      Msg.SetLevel (Msg.Level.Debug);
      M3Timers.Start ();
    END;
    Process.RegisterExitor (CleanUp);

    mach := M3Build.NewMachine ();
    TRY
      TRY

        defs := Makefile.ScanCommandLine ();
    
        (* figure out what we're trying to do *)
        VAR
          name, val: TEXT;
          iter := defs.iterate();
        BEGIN
          WHILE iter.next(name, val) DO
            Quake.Define(mach, name, val);
          END;
        END;

        config := MxConfig.FindFile ();
        IF (config = NIL) THEN
          Msg.FatalError (NIL, "unable to locate configuration file, \"",
                          MxConfig.Filename, "\"");
        END;
    
        (* Default to a native build, so the config file can say less. *)
        
        (* DefineIfNotDefined: overridable from command line with -D *)

        DefineIfNotDefined (mach, "TARGET", MxConfig.HOST);
        DefineIfNotDefined (mach, "OS_TYPE", MxConfig.HOST_OS_TYPE);
        (* DefineIfNotDefined (mach, "BACKEND_MODE", Version.BackendMode); *)
        (* DefineIfNotDefined (mach, "C_COMPILER", Version.CCompiler); *)
        (* DefineIfNotDefined (mach, "LINKER", Version.Linker); *)
        (* DefineIfNotDefined (mach, "THREAD_LIBRARY", Version.ThreadLibrary); *)
        (* DefineIfNotDefined (mach, "WINDOW_LIBRARY", Version.WindowLibrary); *)
        DefineIfNotDefined (mach, "WORD_SIZE", MxConfig.HOST_WORD_SIZE);

        (* Even if the config file overrides the defaults, such as to do
           a cross build, the host characteristics are still available. *)

        (* Quake.Define vs. DefineIfNotDefined: These probably
        don't make sense to ever override from command line. *)

        Quake.Define(mach, "HOST", MxConfig.HOST);
        Quake.Define(mach, "HOST_OS_TYPE", MxConfig.HOST_OS_TYPE);
        (* Quake.Define(mach, "HOST_GNU_MAKE", Version.GNUMake); *)

        (* define the site configuration *)
        Msg.Verbose ("EVAL (\"", config, "\")");
        Quake.Run (mach, config);

        (* -- disabled
        CheckExpire (Quake.LookUp (mach, "INSTALL_KEY"));
        *)

        Builder.SetupNamingConventions (mach);

        (* figure out where we are and get where we want to be *)
        build_dir := Quake.LookUp (mach, "BUILD_DIR");
        IF (build_dir = NIL) THEN
          Msg.FatalError (NIL, "configuration file didn't specify BUILD_DIR");
        END;
        Dirs.SetUp (build_dir);

        (* define the "builtin" quake functions *)
        M3Build.SetUp (mach, Dirs.package, Dirs.to_package,
                       Pathname.Last (Dirs.derived));

        (* what does the user want us to do? *)
        makefile := Makefile.Build (Dirs.to_source);

        (* and finally, do it *)
        IF M3Options.major_mode = M3Options.Mode.RealClean THEN
          (* shortcut; don't call quake to remove everything, do it directly *)
          M3Build.RealClean();
        ELSIF (makefile # NIL) THEN
          Msg.Verbose ("EVAL (\"", makefile, "\")");
          M3Build.Run (mach, Pathname.Join (Dirs.derived, makefile, NIL));
        END;

      FINALLY
        (* free any temp files & garbage *)
        Quake.Done (mach);
        mach := NIL;
      END;

    EXCEPT
    | Quake.Error (msg) =>
        IF NOT M3Build.done THEN
          Msg.Error (NIL, msg);
          M3Options.exit_code := 2;
        END;
    | Thread.Alerted =>
        Msg.FatalError (NIL, "interrupted");
    END;

    IF M3Options.exit_code # 0 THEN
      Msg.Out("Fatal Error: package build failed", Wr.EOL);
    END;
    Process.Exit (M3Options.exit_code);
  END DoIt;

(* -- disabled --
PROCEDURE CheckExpire (passwd: TEXT) =
  CONST Day = 24.0d0 * 3600.0d0;
  CONST FirstWarning = 7.0d0 * Day;
  VAR key := CMKey.Decode (passwd);  expire: Time.T;
  BEGIN
    IF (key.usage = CMKey.Usage.Invalid)
    OR (key.version # CMCurrent.Version)
    OR (key.platform # CMCurrent.Platform) THEN
      Msg.Error (NIL, "---");
      Msg.Error (NIL, "--- This copy of cm3 does not have a valid license key.");
      BuyIt ();
      Process.Exit (1);
    END;

    IF (key.usage # CMKey.Usage.Commercial) THEN
      Msg.Out ("Critical Mass cm3: ", key.banner, Wr.EOL);
    END;

    IF (key.usage = CMKey.Usage.Demo) THEN
      expire := key.expiration - Time.Now ();
      IF (expire > FirstWarning) THEN
        (* ok *)
      ELSIF (expire <= 0.0d0) THEN
        Msg.Error (NIL, "---");
        Msg.Error (NIL, "--- This preview copy of cm3 has expired.");
        BuyIt ();
        Process.Exit (1);
      ELSE
        Msg.Error (NIL, "---");
        Msg.Error (NIL, "--- Warning: this preview copy of cm3 will expire in ",
                    Fmt.Int (ROUND (expire / Day)), " days.");
        BuyIt ();
      END;
    END;
  END CheckExpire;

CONST
  BuyMsg = ARRAY OF TEXT {
    "To purchase a non-expiring copy of cm3, please contact:",
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
      Msg.Error (NIL, "--- ", BuyMsg[i]);
    END;
  END BuyIt;
*)

(*------------------------------------------------- process shutdown ---*)

PROCEDURE CleanUp () =
  BEGIN
    IF (mach # NIL) THEN
      TRY
        Quake.Done (mach);
        mach := NIL;
      EXCEPT Quake.Error (msg) =>
        Msg.Error (NIL, msg);
      END;
    END;

    WebFile.Dump ();
    Builder.CleanUp ();
    M3Timers.Stop ();
    Utils.RemoveTempFiles ();
    IF M3Options.major_mode # M3Options.Mode.RealClean THEN
      Dirs.CleanUp ();
    END;

    IF (M3Options.heap_stats) THEN
      RTutils.Heap (suppressZeros := TRUE,
                    presentation := RTutils.HeapPresentation.ByNumber);
      RTCollector.Collect ();
      RTutils.Heap (suppressZeros := TRUE,
                    presentation := RTutils.HeapPresentation.ByNumber);
    END;
  END CleanUp;

BEGIN
  DoIt ();
END Main.
