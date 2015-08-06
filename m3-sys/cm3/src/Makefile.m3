(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE Makefile;

IMPORT FS, M3File, M3Timers, OSError, Params, Process, Text, Thread, Wr;
IMPORT Arg, M3Build, M3Options, M3Path, Msg, Utils, TextSeq, TextTextTbl;
IMPORT MxConfig, Dirs, Version;

TYPE
  NK = M3Path.Kind;
  MM = M3Options.Mode;

CONST
  ModeName = ARRAY MM OF TEXT { "-build", "-clean", "-ship", "-find",
                                "-depend", "-realclean" };
  ModeFlag = ARRAY MM OF TEXT { "_all",   "_clean", "_ship", "_find",
                                "_depend", "_realclean" };

TYPE
  State = RECORD
    args          : Arg.List := NIL;
    wr            : Wr.T     := NIL;
    keep_files    : BOOLEAN  := FALSE;
    use_overrides : BOOLEAN  := FALSE;
    mode_set      : BOOLEAN  := FALSE;
    found_work    : BOOLEAN  := FALSE;
    prepend_files : TextSeq.T;
    append_files  : TextSeq.T;
  END;

PROCEDURE Build (src_dir: TEXT): TEXT =
  CONST MakefileName = "m3make.args";
  VAR
    s: State;
    src_makefile := M3Path.New (src_dir, "m3makefile");
    src_overrides := M3Path.New (src_dir, "m3overrides");

  PROCEDURE Emit (wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN
      s.wr := wr;
      Out (wr, "set_config_options ()");
      Out (wr, "readonly ", ModeFlag [M3Options.major_mode],
           " = TRUE % cm3 ", ModeName [M3Options.major_mode]);
      CASE M3Options.major_mode OF
      | MM.Find => Out (wr, "M3_FIND_UNITS = []")
      | MM.Build, MM.Clean, MM.RealClean, MM.Ship, MM.Depend =>  (* skip *)
      END;

      ConvertArgList (s);

      CASE M3Options.major_mode OF
      | MM.Build  => Out (wr, "M3_MODE = ", "\"build\"");
      | MM.Clean  => Out (wr, "M3_MODE = ", "\"clean\"");
      | MM.Find   => Out (wr, "M3_MODE = ", "\"find\"");
      | MM.Depend => Out (wr, "M3_MODE = ", "\"depend\"");
      | MM.Ship   => Out (wr, "M3_MODE = ", "\"ship\"");
      | MM.RealClean  => Out (wr, "M3_MODE = ", "\"realclean\"");
      ELSE
        Out (wr, "M3_MODE = ", "\"other\"");
      END;

      CASE M3Options.major_mode OF
      | MM.Build, MM.Clean, MM.Find, MM.Depend =>
          IncludeOverrides (s, src_overrides);
          FOR i := 0 TO s.prepend_files.size() - 1 DO
            IncludeFile (s, s.prepend_files.get(i));
          END;
          IncludeMakefile (s, src_makefile, src_dir);
          FOR i := 0 TO s.append_files.size() - 1 DO
            IncludeFile (s, s.append_files.get(i));
          END;

      | MM.Ship =>
        IF M3File.IsReadable (".M3OVERRIDES") THEN
          Msg.Out ("package was built with overrides, not shipping.", Wr.EOL);
        ELSIF NOT M3File.IsReadable (".M3SHIP") THEN
          Msg.Out ("missing \".M3SHIP\" file, build the package first.",
                   Wr.EOL);
        ELSE
          Out (wr, "include (\".M3SHIP\")");
          s.found_work := TRUE;
        END;

      | MM.RealClean =>
        (* we don't bother to include anything since it will be erased soon *)
      END;
    END Emit;

  BEGIN
    Msg.Debug( "Build(", src_dir, ")", Wr.EOL );
    s.args := Arg.NewList ();
    s.prepend_files := NEW(TextSeq.T).init();
    s.append_files := NEW(TextSeq.T).init();
    FOR i := 1 TO Params.Count - 1 DO
      Arg.Append (s.args, Params.Get (i));
    END;

    Utils.WriteFile (MakefileName, Emit, append := FALSE);

    IF s.found_work THEN
      IF NOT s.keep_files AND M3Options.major_mode # MM.RealClean THEN
        Utils.NoteTempFile (MakefileName); END;
      RETURN MakefileName;
    ELSE
      Msg.Out ("cm3: found nothing to build.", Wr.EOL);
      IF NOT s.keep_files THEN Utils.Remove (MakefileName); END;
      RETURN NIL;
    END;
  END Build;

(*---------------------------------------------------------- internal ---*)

PROCEDURE ConvertArgList (VAR s: State)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR len: INTEGER;  arg: TEXT; noMoreOptions := FALSE;
  BEGIN
    WHILE (s.args.cnt > 0) DO
      arg := Arg.Pop (s.args);
      len := Text.Length (arg);
      IF (len < 1) THEN
        (* empty argument ignore *)
      ELSIF (Text.GetChar (arg, 0) # '-') OR (len < 2) OR noMoreOptions THEN
        NoteSourceFile (s, NIL, arg, cmd_line := TRUE);
      ELSIF Text.Equal (arg, "--") THEN
        noMoreOptions := TRUE;
      ELSIF NOT noMoreOptions THEN
        (* it's an option *)
        ConvertOption (s, arg, len);
      END;
    END;
  END ConvertArgList;

PROCEDURE ConvertOption (VAR s: State;  arg: TEXT;  arg_len: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR ok := FALSE;  wr := s.wr;
  BEGIN
    IF Text.GetChar (arg, 1) = '-' THEN
      arg := Text.Sub (arg, 1);
    END;
    CASE Text.GetChar (arg, 1) OF

    | '?' => IF (arg_len = 2) THEN
               ok := TRUE;  (* printed during the pre-scan *)
             END;

    | 'a' => IF (arg_len = 2) THEN
               Out (wr, "library (\"", GetArg (arg, s.args), "\")");
               ok := TRUE;
               s.found_work := TRUE;
             ELSIF Text.Equal(arg, "-append") THEN
               ok := TRUE;
               WITH fn = GetArg (arg, s.args) DO
                 s.append_files.addhi(fn);
               END;
             END;

    | 'A' => IF (arg_len = 2) THEN
               Out (wr, "M3_OPTIONS += \"-NoAsserts\"");  ok := TRUE;
             END;

    | 'b' => IF Text.Equal (arg, "-boot") THEN
               Out (wr, "M3_BOOTSTRAP = TRUE");  ok := TRUE;
             ELSIF Text.Equal(arg, "-build") THEN
               ok := TRUE; (* mode set during the pre-scan *)
             END;

    | 'c' => IF (arg_len = 2) THEN
               Out (wr, "m3_compile_only ()");  ok := TRUE;
               s.found_work := TRUE;
             ELSIF Text.Equal (arg, "-commands") THEN
               Msg.SetLevel (Msg.Level.Commands);  ok := TRUE;
               Out (wr, "m3_option (\"-commands\")");
             ELSIF Text.Equal (arg, "-config") THEN
               ok := TRUE; (* printed during the pre-scan *)
             ELSIF Text.Equal (arg, "-console") THEN
               Out (wr, "M3_WINDOWS_GUI = FALSE");  ok := TRUE;
             ELSIF Text.Equal(arg, "-clean") THEN
               ok := TRUE;  (* mode set during the pre-scan *)
               s.found_work := TRUE;
             END;

    | 'd' => IF Text.Equal(arg, "-debug") THEN
               Msg.SetLevel (Msg.Level.Debug);  ok := TRUE;
               Out (wr, "m3_option (\"-debug\")");
             ELSIF Text.Equal (arg, "-depend") THEN
               ok := TRUE;
             END;

    | 'D' => ProcessDefine (arg, wr);  ok := TRUE;

    | 'f' => IF Text.Equal(arg, "-find") THEN
               ok := TRUE;  (* mode set during the pre-scan *)
               s.found_work := TRUE;
             END;

    | 'F' => IF Text.Equal(arg, "-F") (* for backward compatibility *) OR
                Text.Equal(arg, "-FP") THEN
               ok := TRUE;
               WITH fn = GetArg (arg, s.args) DO
                 s.prepend_files.addhi(fn);
               END;
             ELSIF Text.Equal(arg, "-FA") THEN
               ok := TRUE;
               WITH fn = GetArg (arg, s.args) DO
                 s.append_files.addhi(fn);
               END;
             END;

    | 'g' => IF (arg_len = 2) THEN
               Out (wr, "m3_debug (TRUE)");  ok := TRUE;
             ELSIF Text.Equal(arg, "-gui") THEN
               Out (wr, "M3_WINDOWS_GUI = TRUE");  ok := TRUE;
             ELSIF Text.Equal(arg, "-gw") OR
                   Text.Equal(arg, "-group-writable") THEN
               M3Build.groupWritable := TRUE; ok := TRUE;
             END;

    | 'h' => IF Text.Equal (arg, "-heap_stats") THEN
               M3Options.heap_stats := TRUE;  ok := TRUE;
             ELSIF Text.Equal (arg, "-help") THEN
               ok := TRUE;  (* printed during the pre-scan *)
             END;

    | 'k' => IF (arg_len = 2) OR Text.Equal (arg, "-keep") THEN
               Out (wr, "M3_KEEP_FILES = TRUE");
               s.keep_files := TRUE;  ok := TRUE;
             END;

    | 'l' => IF Text.Equal (arg, "-lazy") THEN
               Out (wr, "M3_LAZY_MODULE_INIT = TRUE");  ok := TRUE;
             ELSIF Text.Equal (arg, "-linkall") THEN
               Out (wr, "M3_LAZY_MODULE_INIT = FALSE");  ok := TRUE;
             END;

    | 'n' => IF Text.Equal (arg, "-no-m3ship-resolution") THEN
               M3Build.noM3ShipResolution := TRUE; ok := TRUE;
             END;

    | 'o' => IF (arg_len = 2) THEN
               Out (wr, "program (\"", GetArg (arg, s.args), "\")");
               ok := TRUE;
               s.found_work := TRUE;
             ELSIF Text.Equal (arg, "-override") THEN
               s.use_overrides := TRUE;  ok := TRUE;
             ELSIF Text.Equal (arg, "-once") THEN
               Out (wr, "M3_COMPILE_ONCE = TRUE");  ok := TRUE;
             END;

    | 'p' => IF Text.Equal(arg, "-pretend") OR
                Text.Equal(arg, "-profile") THEN
               ok := TRUE;
             ELSIF Text.Equal(arg, "-prepend") THEN
               ok := TRUE;
               WITH fn = GetArg (arg, s.args) DO
                 s.prepend_files.addhi(fn);
               END;
             ELSIF Text.Equal (arg, "-pb") THEN
               Out (wr, "M3_PARALLEL_BACK = ", GetArg (arg, s.args));
               ok := TRUE;
             END;

    | 'O' => IF (arg_len = 2) THEN
               Out (wr, "m3_optimize (TRUE)");  ok := TRUE;
             END;

    | 'r' => IF Text.Equal(arg, "-realclean") THEN
               ok := TRUE;  (* mode set during the pre-scan *)
               s.found_work := TRUE;
             END;

    | 's' => IF Text.Equal (arg, "-silent") THEN
               Msg.SetLevel (Msg.Level.Silent);  ok := TRUE;
               Out (wr, "m3_option (\"-silent\")");
             ELSIF Text.Equal (arg, "-skiplink") THEN
               Out (wr, "M3_SKIP_LINK = TRUE");  ok := TRUE;
             ELSIF Text.Equal (arg, "-ship") THEN
               ok := TRUE;  (* mode set during the pre-scan *)
               s.found_work := TRUE;
             END;

    | 't' => IF Text.Equal (arg, "-times") THEN
               M3Timers.Start ();  ok := TRUE;
             ELSIF Text.Equal (arg, "-trace") THEN
               Out (wr, "TRACE_INSTR()");  ok := TRUE;
             END;

    | 'v' => IF Text.Equal (arg, "-verbose") THEN
               Msg.SetLevel (Msg.Level.Verbose);
               Out (wr, "m3_option (\"-verbose\")");
               M3Timers.Start ();
               ok := TRUE;
             ELSIF Text.Equal (arg, "-version") THEN
               ok := TRUE;  (* printed during the pre-scan *)
             END;

    | 'w' => IF Text.Equal (arg, "-why") THEN
               Msg.SetLevel (Msg.Level.Explain);  ok := TRUE;
               Out (wr, "m3_option (\"-why\")");
             ELSIF Text.Equal (arg, "-windows") THEN
               Out (wr, "M3_WINDOWS_GUI = TRUE");  ok := TRUE;
             ELSIF Text.Equal (arg, "-w0") THEN
               Out (wr, "M3_OPTIONS += \"-w0\"");  ok := TRUE;
             ELSIF Text.Equal (arg, "-w1") THEN
               Out (wr, "M3_OPTIONS += \"-w1\"");  ok := TRUE;
             ELSIF Text.Equal (arg, "-w2") THEN
               Out (wr, "M3_OPTIONS += \"-w2\"");  ok := TRUE;
             ELSIF Text.Equal (arg, "-w3") THEN
               Out (wr, "M3_OPTIONS += \"-w3\"");  ok := TRUE;
             ELSIF Text.Equal (arg, "-widechar16") THEN ok := TRUE; 
             ELSIF Text.Equal (arg, "-widecharuni") THEN ok := TRUE; 
             END;

    | 'x' => IF (arg_len = 2) THEN
               s.use_overrides := TRUE;  ok := TRUE;
             END;

    | 'Z' => IF (arg_len = 2) THEN
               Out (wr, "M3_COVERAGE = TRUE");  ok := TRUE;
             END;

    ELSE (* error *)
    END;

    IF (NOT ok) THEN Msg.UsageError ("unrecognized option \"", arg, "\"") END;
  END ConvertOption;

PROCEDURE GetArg (arg: TEXT;  rest: Arg.List): TEXT =
  BEGIN
    IF (rest.cnt <= 0) THEN
      Msg.UsageError ("missing argument to \"", arg, "\" option");
    END;
    RETURN Arg.Pop (rest);
  END GetArg;

PROCEDURE ProcessDefine (arg: TEXT;  wr: Wr.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR len := Text.Length (arg);  eq: INTEGER;  sym, val: TEXT;
  BEGIN
    IF (len <= 2) THEN
      Msg.UsageError ("missing argument to \"-D\" option");
      RETURN;
    END;

    eq := Text.FindChar (arg, '=');
    IF (eq < 0) THEN
      (* -Dsymbol ==>  symbol = TRUE *)
      EVAL defs.put(Text.Sub (arg, 2), "TRUE");
      IF wr # NIL THEN
        Out (wr, Text.Sub (arg, 2), " = TRUE");
      END;
      RETURN;
    END;

    sym := Text.Sub (arg, 2, eq-2);
    val := Text.Sub (arg, eq+1);
    len := Text.Length (val);

    EVAL defs.put(sym, val);
    
    (* CONSIDER: removing the rest of this function is reasonable.
     The upside is, m3make.args won't get errors redefining readonly values.
     The downside is that cm3 -keep leaves less evidence, in m3make.args.
     Since we are leaving the writes in, we change Quake to allow redefining
     readonly values, if they are set to the same thing they are already set to.
     e.g.:
       readonly foo = 1
       foo = 2 % same error as usual
       readonly foo = 1
       foo = 1 % ok
     *)
    IF (len = 0) THEN
      (* -Dsymbol=   ==> symbol = "" *)
      IF wr # NIL THEN
        Out (wr, sym, " = \"\"");
      END;

    ELSIF Text.GetChar (arg, 0) = '"'
      AND Text.GetChar (arg, len-1) = '"' THEN
      (* -Dsymbol="foo" ==> symbol = "foo" *)
      IF wr # NIL THEN
        Out (wr, sym, " = ", val);
      END;

    ELSIF Text.Equal (val, "TRUE") OR Text.Equal (val, "FALSE") THEN
      IF wr # NIL THEN
        Out (wr, sym, " = ", val);
      END;

    ELSE
      (* -Dsymbol=val  ==> symbol = "val" *)
      IF wr # NIL THEN
        Out (wr, sym, " = \"", val, "\"");
      END;
    END;
  END ProcessDefine;

CONST
  SourceTag = ARRAY NK OF TEXT {
    NIL,                                                   (* unknown *)
    "interface", NIL, NIL, NIL, "import_obj",              (* i3, ib, ic, is, io *)
    "implementation", NIL, NIL, NIL, "import_obj",         (* m3, mb, mc, ms, mo *)
    "generic_interface", "generic_implementation",         (* ig, mg *)
    "c_source", "h_source", NIL, "s_source", "import_obj", (* c, h, b, s, o *)
    "import_lib", "import_lib", NIL,                       (* m3lib, lib, m3x *)
    NIL, NIL, NIL                                          (* pgm, mx, tmpl *)
  };

PROCEDURE NoteSourceFile (VAR s: State;  dir, name: TEXT;  cmd_line: BOOLEAN)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    file := M3Path.New (dir, name);
    info := M3Path.Parse (file);
    tag  : TEXT;
  BEGIN
    Msg.Debug ("  file ", file, Wr.EOL);

    IF (M3Options.major_mode = MM.Find) AND (cmd_line) THEN
      Out (s.wr, "M3_FIND_UNITS += \"", M3Path.Join (NIL, info.base, info.kind), "\"");
      s.found_work := TRUE;
      RETURN;
    END;

    tag := SourceTag [info.kind];
    IF (tag # NIL) THEN
      file := M3Path.Convert (M3Path.New (info.dir, info.base));
      Out (s.wr, tag, " (\"", file, "\")");
      s.found_work := TRUE;
    ELSE
      VisitSourceDir (s, file, cmd_line);
    END;
  END NoteSourceFile;

VAR normalizedDerivedDir: TEXT := NIL;

PROCEDURE VisitSourceDir (VAR s: State;  dir: TEXT;  cmd_line: BOOLEAN)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR iter: FS.Iterator;  name: TEXT;
      normalizedDir: TEXT;
  BEGIN
    Msg.Debug ("--- dir ", dir, " ---", Wr.EOL);
    IF NOT M3File.IsDirectory (dir) THEN
      IF (cmd_line) THEN
        Msg.FatalError (NIL, "unsupported file type \"", dir, "\"");
      END;
      Msg.Verbose ("ignoring ", dir, " (not a directory)");
      RETURN;
    END;
    IF normalizedDerivedDir = NIL THEN
      TRY
        normalizedDerivedDir := FS.GetAbsolutePathname(Dirs.derived);
      EXCEPT OSError.E (args) =>
        Msg.FatalError (args, "unable to get absolute path for \"",
                        Dirs.derived, "\"");
      END;
    END;
    TRY
      normalizedDir := FS.GetAbsolutePathname(dir);
    EXCEPT OSError.E (args) =>
      Msg.FatalError (args, "unable to get absolute path for \"", dir, "\"");
    END;
    IF Text.Equal(normalizedDir, normalizedDerivedDir) THEN
      Msg.Verbose ("ignoring ", dir, " (derived object directory)");
      RETURN;
    END;
    TRY
      Msg.Verbose ("Looking in ", dir);
      iter := FS.Iterate (dir);
      TRY
        WHILE iter.next (name) DO
          NoteSourceFile (s, dir, name, cmd_line := FALSE);
        END;
      FINALLY
        iter.close();
      END;
    EXCEPT OSError.E (args) =>
      Msg.FatalError (args, "unable to scan directory \"", dir, "\"");
    END;
  END VisitSourceDir;

PROCEDURE IncludeOverrides (VAR s: State;  overrides: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF M3File.IsReadable (overrides) THEN
      IF (s.use_overrides) THEN
        Out (s.wr, "include (\"", M3Path.Convert (overrides), "\")");
        s.found_work := TRUE;
      ELSE
        IF (M3Options.major_mode = MM.Depend) THEN
          Msg.Verbose ("ignoring ", overrides, Wr.EOL);
        ELSE
          Msg.Info ("ignoring ", overrides, Wr.EOL);
        END;
      END;
    ELSE
      IF (s.use_overrides) THEN
        IF (M3Options.major_mode = MM.Depend) THEN
          Msg.Verbose ("unable to read ", overrides,
                       ", options \"-override\" and \"-x\" ignored.", Wr.EOL);
        ELSE
          Msg.Out ("unable to read ", overrides,
                   ", options \"-override\" and \"-x\" ignored.", Wr.EOL);
        END;
      END;
    END;
  END IncludeOverrides;

PROCEDURE IncludeFile (VAR s: State;  file: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF M3File.IsReadable (file) THEN
      Out (s.wr, "include (\"", M3Path.Convert (file), "\")");
      s.found_work := TRUE;
    ELSE
      IF (M3Options.major_mode = MM.Depend) THEN
        Msg.Verbose ("unable to read ", file, Wr.EOL);
      ELSE
        Msg.Out ("unable to read ", file, Wr.EOL);
      END;
    END;
  END IncludeFile;

PROCEDURE IncludeMakefile (VAR s: State;  makefile, dir: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF M3File.IsReadable (makefile) THEN
      Out (s.wr, "include_dir (\"", M3Path.Convert (dir), "\")");
      s.found_work := TRUE;
    ELSE
      Msg.Debug ("Dirs.derived   = ", Dirs.derived, Wr.EOL);
      Msg.Debug ("Dirs.to_source = ", Dirs.to_source, Wr.EOL);
      Msg.Debug ("dir            = ", dir, Wr.EOL);
      Out (s.wr, "import (\"libm3\")");
      VisitSourceDir (s, dir, cmd_line := FALSE);
      Out (s.wr, "program (\"prog\")");
    END;
  END IncludeMakefile;

(*----------------------------------------- pre-scan command line ---*)

PROCEDURE ScanCommandLine () : TextTextTbl.T RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    use_overrides := FALSE;
    got_mode := FALSE;  arg: TEXT;
  BEGIN

    FOR i := 1 TO Params.Count-1 DO
      arg := Params.Get (i);
      IF Text.Length(arg) > 1 AND Text.GetChar (arg, 1) = '-' THEN
        arg := Text.Sub (arg, 1);
      END;
      IF    Text.Equal (arg, "-?")         THEN  PrintHelp ();
      ELSIF Text.Equal (arg, "-help")      THEN  PrintHelp ();
      ELSIF Text.Equal (arg, "-config")    THEN  PrintVersion (TRUE);
      ELSIF Text.Equal (arg, "-version")   THEN  PrintVersion (TRUE);
      END;
    END;

    FOR i := 1 TO Params.Count-1 DO
      arg := Params.Get (i);
      IF Text.Length(arg) > 1 AND Text.GetChar (arg, 1) = '-' THEN
        arg := Text.Sub (arg, 1);
      END;
      IF    Text.Equal (arg, "-build")     THEN  SetMode (got_mode, MM.Build);
      ELSIF Text.Equal (arg, "-clean")     THEN  SetMode (got_mode, MM.Clean);
      ELSIF Text.Equal (arg, "-realclean") THEN  SetMode (got_mode, MM.RealClean);
      ELSIF Text.Equal (arg, "-find")      THEN  SetMode (got_mode, MM.Find);
      ELSIF Text.Equal (arg, "-ship")      THEN  SetMode (got_mode, MM.Ship);
      ELSIF Text.Equal (arg, "-depend")    THEN  SetMode (got_mode, MM.Depend);
      ELSIF Text.Equal (arg, "-silent") THEN
        Msg.SetLevel (Msg.Level.Silent);
      ELSIF Text.Equal (arg, "-why") THEN
        Msg.SetLevel (Msg.Level.Explain);
      ELSIF Text.Equal (arg, "-commands") THEN
        Msg.SetLevel (Msg.Level.Commands);
      ELSIF Text.Equal (arg, "-verbose") THEN
        Msg.SetLevel (Msg.Level.Verbose);
      ELSIF Text.Equal (arg, "-debug") THEN
        Msg.SetLevel (Msg.Level.Debug);
      ELSIF Text.Equal (arg, "-profile") THEN
        EVAL defs.put("M3_PROFILING", "TRUE");
      ELSIF Text.Equal (arg, "-widechar16") THEN
        Utils.NoteWidechar16 ();
      ELSIF Text.Equal (arg, "-widecharuni") THEN
        Utils.NoteWidecharUni ();
      ELSIF Text.Equal (arg, "-trace") THEN
        traceQuake := TRUE;
      ELSIF Text.Equal (arg, "-x") OR Text.Equal (arg, "-override") THEN
        use_overrides := TRUE;
      ELSIF Text.Equal (arg, "-parallelback") THEN
      ELSIF Text.Equal (arg, "-pretend") THEN
        IF i < Params.Count - 1 THEN
          EVAL defs.put("CM3_VERSION", Params.Get(i+1));
        ELSE
          Msg.Error(NIL, "missing argument for -pretend");
        END;
      ELSIF Text.GetChar(arg, 0) = '-' AND Text.GetChar(arg, 1) = 'D' THEN
        ProcessDefine(arg, NIL);
      END;
    END;
    IF got_mode = FALSE THEN SetMode (got_mode, MM.Build); END;
    EVAL defs.put("M3_USE_OVERRIDES", ARRAY BOOLEAN OF TEXT {"", "TRUE"}[use_overrides]);
    RETURN defs;
  END ScanCommandLine;

PROCEDURE SetMode (VAR got_mode: BOOLEAN;  mode: MM) =
  BEGIN
    IF got_mode THEN
      Msg.Error (NIL, "mode \"", ModeName [M3Options.major_mode],
                 "\" already set, \"", ModeName [mode] & "\" ignored.");
    ELSE
      M3Options.major_mode := mode;
      got_mode := TRUE;
    END;
  END SetMode;

PROCEDURE PrintVersion (exit: BOOLEAN) =
  BEGIN
    IF traceQuake THEN MxConfig.EnableQuakeTrace() END;
    Msg.Out ("Critical Mass Modula-3 version ", Val("CM3_RELEASE"), Wr.EOL);
    Msg.Out ("  last updated: ", Val("CM3_CHANGED"), Wr.EOL);
    Msg.Out ("  compiled: ", Val("CM3_COMPILED"), Wr.EOL);
    Msg.Out ("  configuration: ", MxConfig.FindFile(), Wr.EOL);
    Msg.Out ("  host: ", MxConfig.HOST, Wr.EOL);
    Msg.Out ("  target: ", MxConfig.Get("TARGET"), Wr.EOL);
    Msg.Out (Wr.EOL);
    IF exit THEN Process.Exit (0); END;
  END PrintVersion;

PROCEDURE PrintHelp () =
  BEGIN
    PrintVersion (FALSE);
    FOR i := FIRST (HelpMsg) TO LAST (HelpMsg) DO
      Msg.Out (HelpMsg[i], Wr.EOL);
    END;
    Process.Exit (0);
  END PrintHelp;

CONST
  HelpMsg = ARRAY OF TEXT {
    "command line options:",
    "",
    "modes:  (default: -build)",
    "  -build         compile and link",
    "  -ship          install package",
    "  -clean         delete derived files",
    "  -realclean     delete derived target directory",
    "  -find          locate source files",
    "  -depend        output package dependencies",
    "",
    "compile options:  (default: -g -w1)",
    "  -g             produce symbol table information for debugger",
    "  -O             optimize code",
    "  -A             disable code generation for assertions",
    "  -once          don't recompile to improve opaque object code",
    "  -w0 .. -w3     limit compiler warning messages",
    "  -Z             generate coverage analysis code",
    "  -profile       generate profiling code",
    "  -widechar16    give WIDECHAR a 16-bit range",
    "  -widecharuni   give WIDECHAR the full Unicode range",
    "  -lazy          generate lazy module initialization code",
    "                 (main module and imports only) This is the default.",
    "  -linkall       generate module initialization code for all modules,",
    "                 even those that are not imported directly or indirectly",
    "                 by Main. This is currently experimental and does not",
    "                 always work as expected.",
    "",
    "program and library options:  (default: -o prog)",
    "  -c             compile only, produce no program or library",
    "  -a <foo>       build library <foo>",
    "  -o <foo>       build program <foo>",
    "  -skiplink      skip the final link step",
    "",
    "messages:  (default: -why)",
    "  -silent        produce no diagnostic output",
    "  -why           explain why code is being recompiled",
    "  -commands      list system commands as they are performed",
    "  -verbose       list internal steps as they are performed",
    "  -debug         dump internal debugging information",
    "  -trace         trace quake code execution",
    "",
    "information and help:",
    "  -help          print this help message",
    "  -?             print this help message",
    "  -version       print the version number header",
    "  -config        print the version number header",
    "",
    "misc:",
    "  --             end of options",
    "  -keep          preserve intermediate and temporary files",
    "  -times         produce a dump of elapsed times",
    "  -override      include the \"m3overrides\" file",
    "  -x             include the \"m3overrides\" file",
    "  -D<symbol>     define <symbol> with the value TRUE",
    "  -D<sym>=<val>  define <sym> with the value <val>",
    "  -F <fn>        prepend the quake code of file <fn>",
    "  -FP <fn>        \"",
    "  -prepend <fn>   \"",
    "  -FA <fn>       append the quake code of file <fn>",
    "  -append <fn>    \"",
    "  -console       produce a Windows CONSOLE subsystem program",
    "  -gui           produce a Windows GUI subsystem program",
    "  -windows       produce a Windows GUI subsystem program",
    "  -pretend <val> pretend to run as CM3_Version <val>",
    "  -gw            install group writable files",
    "  -group-writable \"",
    "  -pb <n>        allow <n> parallelism in running back-end (experimental)",
    "  -no-m3ship-resolution use quake variables in .M3SHIP (experimental)",
    "",
    "environment variables:",
    "  M3CONFIG       platform dependent configuration file to use (cm3.cfg)",
    "                 used if no suitable file is found in the local package",
    "  QUAKE_SHELL    shell to be used by the quake interpreter for exec()",
    "  QUAKE_SHELL_OPTION command option for quake shell (-c, /c)",
    "  QUAKE_TMPDIR   directory for temporary files used by quake",
    "  ",
    "  CM3_INSTALL_PREFIX path prefix to prepend to filenames being installed,",
    "                 \"make DESTDIR=\" behaviour for cm3",
    "  "
  };

(*---------------------------------------------------------- misc ---*)

PROCEDURE Out (wr: Wr.T;  a, b, c, d, e: TEXT := NIL)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF (a # NIL) THEN Wr.PutText (wr, a); END;
    IF (b # NIL) THEN Wr.PutText (wr, b); END;
    IF (c # NIL) THEN Wr.PutText (wr, c); END;
    IF (d # NIL) THEN Wr.PutText (wr, d); END;
    IF (e # NIL) THEN Wr.PutText (wr, e); END;
    Wr.PutText (wr, Wr.EOL);
  END Out;

PROCEDURE Val(name: TEXT) : TEXT =
  VAR res: TEXT := "undefined";
  BEGIN
    EVAL defs.get(name, res);
    RETURN res;
  END Val;

VAR
  defs := NEW(TextTextTbl.Default).init();
  traceQuake := FALSE;
BEGIN
  EVAL defs.put("CM3_RELEASE", Version.Text);  (* readable release version *)
  EVAL defs.put("CM3_VERSION", Version.Number);(* version as number *)
  EVAL defs.put("CM3_CHANGED", Version.LastChanged); (* date of last change *)
  EVAL defs.put("CM3_CREATED", Version.LastChanged); (* backw. compatibility *)
  EVAL defs.put("CM3_COMPILED", Version.Created); (* date of compilation *)
  EVAL defs.put("M3_PROFILING", "");           (* no profiling by default *)
  EVAL defs.put("EOL", Wr.EOL);
END Makefile.
