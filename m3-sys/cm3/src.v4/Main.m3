(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE Main;

IMPORT Fmt, M3Timers, Pathname, Process, Quake, Time, Wr;
IMPORT RTCollector, RTCollectorSRC, RTParams, RTutils, Thread;

IMPORT Builder, Dirs, M3Build, M3Options, Makefile, Msg, Utils, WebFile;
IMPORT MxConfig AS M3Config, CMKey, CMCurrent;

VAR
  config    : TEXT          := NIL;
  makefile  : TEXT          := NIL;
  build_dir : TEXT          := NIL;
  mach      : Quake.Machine := NIL;

PROCEDURE DoIt () =
  BEGIN
    RTCollectorSRC.DisableVM ();
    IF RTParams.IsPresent ("verbose") THEN
      Msg.SetLevel (Msg.Level.Verbose);
      M3Timers.Start ();
    END;
    IF RTParams.IsPresent ("debug")   THEN
      Msg.SetLevel (Msg.Level.Debug);
      M3Timers.Start ();
    END;
    Process.RegisterExitor (CleanUp);

    config := M3Config.FindFile ();
    IF (config = NIL) THEN
      Msg.FatalError (NIL, "unable to locate configuration file, \"",
                      M3Config.Filename, "\"");
    END;

    mach := M3Build.NewMachine ();
    TRY
      TRY
        (* figure out what we're trying to do *)
        Makefile.ScanCommandLine ();

        (* define the site configuration *)
        Msg.Verbose ("EVAL (\"", config, "\")");
        Quake.Run (mach, config);

        CheckExpire (Quake.LookUp (mach, "INSTALL_KEY"));

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
        IF (makefile # NIL) THEN
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
        Msg.Error (NIL, msg);
    | Thread.Alerted =>
        Msg.FatalError (NIL, "interrupted");
    END;

    Process.Exit (M3Options.exit_code);
  END DoIt;

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
    Dirs.CleanUp ();

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
