(* Copyright 1996-2003 John D. Polstra.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id$ *)

MODULE Main;

IMPORT
  BackoffTimer, CVProto, Date, Env, ErrMsg, Fmt, FS, FSClient, IO,
  IP, LockFile, Logger, OSError, Params, Pathname, Process, Stdio,
  SupFile, SupFileRec, SupGUI, SupMisc, Text, Thread, Time, TokScan,
  Version, WrLogger;

CONST
  DefaultVerbosity = 1;
  ThreadStackSize = 16 * 1024;	(* In units of Word.T *)

EXCEPTION Error(TEXT);

PROCEDURE CheckPort(port: INTEGER) =
  BEGIN
    IF port = IP.NullPort
    OR NOT (FIRST(IP.Port) <= port AND port <= LAST(IP.Port)) THEN
      ErrMsg.Fatal("Invalid port " & Fmt.Int(port));
    END;
    IF port < 1024 THEN
      ErrMsg.Fatal("Reserved port " & Fmt.Int(port) & " not permitted");
    END;
  END CheckPort;

PROCEDURE FetchArg(VAR argNum, argPos: CARDINAL): TEXT =
  VAR
    arg := Params.Get(argNum);
    argLen := Text.Length(arg);
    value: TEXT;
  BEGIN
    IF argPos < argLen THEN  (* There was no whitespace after the flag. *)
      value := Text.Sub(arg, argPos);
      argPos := argLen;
    ELSE  (* The value is in a separate argument. *)
      INC(argNum);
      IF argNum >= Params.Count THEN
	Usage();
      END;
      value := Params.Get(argNum);
    END;
    RETURN value;
  END FetchArg;

PROCEDURE OverrideOff(config: FSClient.Configuration;
                      option: SupFileRec.Option) =
  BEGIN
    WITH options = SupFileRec.Options{option} DO
      config.override.options := config.override.options - options;
      config.overrideMask := config.overrideMask + options;
    END;
  END OverrideOff;

PROCEDURE OverrideOn(config: FSClient.Configuration;
                     option: SupFileRec.Option) =
  BEGIN
    WITH options = SupFileRec.Options{option} DO
      config.override.options := config.override.options + options;
      config.overrideMask := config.overrideMask + options;
    END;
  END OverrideOn;

PROCEDURE Usage(msg: TEXT := NIL) =
  VAR
    prog := Pathname.Last(Params.Get(0));
  BEGIN
    IF msg # NIL THEN 
      msg := msg & "\n";
    ELSE
      msg := "";
    END;
    ErrMsg.Fatal(msg
      & "Usage: " & prog & " [options] supfile [destDir]"
      & "\n  Options:"
      & "\n    -1           Don't retry automatically on failure "
                              & "(same as \"-r 0\")"
      & "\n    -a           Require server to authenticate itself to us"
      & "\n    -A addr      Bind local socket to a specific address"
      & "\n    -b base      Override supfile's \"base\" directory"
      & "\n    -c collDir   Subdirectory of \"base\" for collections"
			      & " (default \""
			      & SupMisc.DefaultClientCollDir & "\")"
      & "\n    -d delLimit  Allow at most \"delLimit\" file deletions"
			      & " (default unlimited)"
      & "\n    -D           Do file deletions only; skip all other updates"
      & "\n                   (not implemented for checkout mode)"
      & "\n    -e           Enable shell command execution for all collections"
      & "\n    -E           Disable shell command execution for all collections"
      & "\n    -g           Don't use the GUI (implied if $DISPLAY is not set)"
      & "\n    -h host      Override supfile's \"host\" name"
      & "\n    -i pattern   Include only files/directories matching pattern."
      & "\n                   May be repeated for an OR operation.  Default is"
      & "\n                   to include each entire collection."
      & "\n    -k           Keep bad temporary files when fixups are required"
      & "\n    -l lockfile  Lock file during update; fail if already locked"
      & "\n    -L n         Verbosity level for \"-g\" (" &
			      Fmt.Int(FIRST(Verbosity)) & ".." &
			      Fmt.Int(LAST(Verbosity)) &
			      ", default " &
			      Fmt.Int(DefaultVerbosity) & ")"
      & "\n    -p port      Alternate server port (default " &
		              Fmt.Int(SupMisc.Port) & ")"
      & "\n    -P range     Range for client data ports (default unrestricted)"
      & "\n                 Range can be a single port number or a range"
      & "\n                 \"lo-hi\".  A range of \"-\" selects passive mode."
      & "\n                 A range of \"a\" selects active mode.  " &
			      "A range of \"m\""
      & "\n                 selects multiplexed mode (the default)."
      & "\n    -r n         Maximum retries on transient errors (default " &
			      "unlimited)"
      & "\n    -s           Don't stat client files; trust the checkouts file"
      & "\n    -v           Print version and exit"
      & "\n    -x           Require detailed compare for all RCS files"
      & "\n    -z           Enable compression for all collections"
      & "\n    -Z           Disable compression for all collections"
      );
  END Usage;

TYPE
  Verbosity = [0..2];
VAR
  useGUI := SupGUI.Supported;
  justPrintVersion := FALSE;
  lockFile: Pathname.T := NIL;
  lock: LockFile.T := NIL;
  verbosity: Verbosity := DefaultVerbosity;
  config := NEW(FSClient.Configuration);
  supFile: TEXT := NIL;
  arg: TEXT;
  argVal: TEXT;
  pos: INTEGER;
  argNum: CARDINAL;
  argPos: CARDINAL;
  argLen: CARDINAL;
  option: CHAR;
  retryCount := 0;
  maxRetries := -1;  (* Infinity. *)
BEGIN
  config.override := NEW(SupFileRec.T).init();
  WITH m3socks = Env.Get("M3SOCKS") DO
    IF m3socks # NIL AND NOT Text.Empty(m3socks) THEN
      config.connectMode := FSClient.ConnectMode.Socks;
    END;
  END;
  TRY
    argNum := 1;
    WHILE argNum < Params.Count DO
      arg := Params.Get(argNum);
      argLen := Text.Length(arg);
      IF argLen > 0 AND Text.GetChar(arg, 0) = '-' THEN
	argPos := 1;
	WHILE argPos < argLen DO
	  option := Text.GetChar(arg, argPos);
	  INC(argPos);
	  CASE option OF
	  | '1' => maxRetries := 0;
	  | 'a' => config.authRequired := TRUE;
	  | 'A' =>
	      WITH t = FetchArg(argNum, argPos) DO
		TRY
		  IF NOT SupMisc.ParseHost(t, config.localEndpoint.addr) THEN
		    ErrMsg.Fatal(t & ": host unknown");
		  END;
		EXCEPT IP.Error(list) =>
		  ErrMsg.Fatal(t & ": " & ErrMsg.StrError(list));
		END;
	      END;
	  | 'b' => config.override.clientBase := FetchArg(argNum, argPos);
	  | 'c' => config.override.clientCollDir := FetchArg(argNum, argPos);
	  | 'd' =>
	      config.deleteLimit :=
		TokScan.AtoI(FetchArg(argNum, argPos), "deletion limit");
	  | 'D' => OverrideOn(config, SupFileRec.Option.DoDeletesOnly);
	  | 'e' => OverrideOn(config, SupFileRec.Option.Execute);
	  | 'E' => OverrideOff(config, SupFileRec.Option.Execute);
	  | 'g' => useGUI := FALSE;
	  | 'h' => config.override.serverHost := FetchArg(argNum, argPos);
	  | 'i' => config.override.accepts.addhi(FetchArg(argNum, argPos));
	  | 'k' => OverrideOn(config, SupFileRec.Option.KeepBadFiles);
	  | 'l' => lockFile := FetchArg(argNum, argPos);
	  | 'L' =>
	      WITH n = TokScan.AtoI(FetchArg(argNum, argPos), "verbosity") DO
		IF n > LAST(Verbosity) THEN
		  verbosity := LAST(Verbosity);
		ELSE
		  verbosity := n;
		END;
	      END;
	  | 'p' =>
	      config.port :=
		TokScan.AtoI(FetchArg(argNum, argPos), "server port");
	      CheckPort(config.port);
	  | 'P' =>
	      argVal := FetchArg(argNum, argPos);
	      IF Text.Equal(argVal, "-") THEN  (* Passive mode. *)
		IF config.connectMode = FSClient.ConnectMode.Socks THEN
		  ErrMsg.Fatal("\"-P -\" is incompatible with SOCKS mode");
		END;
		config.connectMode := FSClient.ConnectMode.Passive;
	      ELSIF Text.Equal(argVal, "a") THEN  (* Active mode. *)
		IF config.connectMode = FSClient.ConnectMode.Socks THEN
		  ErrMsg.Fatal("\"-P a\" is incompatible with SOCKS mode");
		END;
		config.connectMode := FSClient.ConnectMode.Active;
	      ELSIF Text.Equal(argVal, "m") THEN  (* Multiplexed mode. *)
		(* We purposely don't check for SOCKS mode here.  Multiplexed
		   mode should work fine without any special processing for
		   SOCKS. *)
		config.connectMode := FSClient.ConnectMode.Mux;
	      ELSE
		(* FIXME - Remove this once SOCKS can handle it. *)
		IF config.connectMode = FSClient.ConnectMode.Socks THEN
		  ErrMsg.Fatal("\"-P\" is not yet supported in SOCKS mode");
		END;
		(**)
		pos := Text.FindChar(argVal, '-');
		IF pos >= 0 THEN
		  config.loDataPort := TokScan.AtoI(Text.Sub(argVal, 0, pos),
		    "low data port");
		  CheckPort(config.loDataPort);
		  config.hiDataPort := TokScan.AtoI(Text.Sub(argVal, pos+1),
		    "high data port");
		  CheckPort(config.hiDataPort);
		  IF config.loDataPort > config.hiDataPort THEN
		    ErrMsg.Fatal("Invalid data port range " & argVal);
		  END;
		ELSE
		  config.loDataPort := TokScan.AtoI(argVal, "data port");
		  CheckPort(config.loDataPort);
		  config.hiDataPort := config.loDataPort;
		END;
		config.connectMode := FSClient.ConnectMode.Active;
	      END;
	  | 'r' =>
	      maxRetries :=
		TokScan.AtoI(FetchArg(argNum, argPos), "retry limit");
	  | 's' => OverrideOn(config, SupFileRec.Option.TrustStatusFile);
	  | 'v' => justPrintVersion := TRUE;
	  | 'x' => OverrideOn(config, SupFileRec.Option.DetailAllRCSFiles);
	  | 'z' => OverrideOn(config, SupFileRec.Option.Compress);
	  | 'Z' => OverrideOff(config, SupFileRec.Option.Compress);
	  ELSE
	    Usage("Invalid option \"-" & Text.FromChar(option) & "\"");
	  END;
	END;
      ELSE
	IF supFile = NIL THEN
	  supFile := arg;
	ELSIF config.destDir = NIL THEN
	  config.destDir := arg;
	ELSE
	  Usage();
	END;
      END;
      INC(argNum);
    END;
  EXCEPT TokScan.Error(msg) =>
    Usage(msg);
  END;

  IF justPrintVersion THEN
    IF SupGUI.Supported THEN
      IO.Put("CVSup client, GUI version\n");
    ELSE
      IO.Put("CVSup client, non-GUI version\n");
    END;
    IO.Put("Copyright 1996-2003 John D. Polstra\n");
    IO.Put("Software version: " & Version.Name & "\n");
    IO.Put("Protocol version: " & Fmt.Int(CVProto.Current.major) & "." &
      Fmt.Int(CVProto.Current.minor) & "\n");
    IO.Put("Operating system: " & Version.Target & "\n");
    IO.Put("http://www.cvsup.org/\n");
    IO.Put("Report problems to cvsup-bugs@polstra.com\n");
    IO.Put("CVSup is a registered trademark of John D. Polstra\n");
    Process.Exit(0);
  END;

  IF supFile = NIL THEN
    Usage();
  END;

  IF config.destDir # NIL THEN
    TRY
      IF FS.Status(config.destDir).type # FS.DirectoryFileType THEN
	ErrMsg.Fatal("\"" & config.destDir & "\" is not a directory");
      END;
    EXCEPT OSError.E =>
      ErrMsg.Fatal("Directory \"" & config.destDir & "\" does not exist");
    END;
  END;

  IF useGUI THEN
    WITH display = Env.Get("DISPLAY") DO
      IF display = NIL OR Text.Empty(display) THEN
	useGUI := FALSE;
      END;
    END;
  END;

  (* Increase the default stack size for all threads. *)
  Thread.MinDefaultStackSize(ThreadStackSize);

  TRY
    IF useGUI THEN
      TRY
	config.lockFile := lockFile;  (* Tell client to do its own locking. *)
	SupGUI.Run(supFile := supFile, config := config);
      EXCEPT
      | SupGUI.Error(msg) =>
	ErrMsg.Fatal(msg);
      END;
    ELSE
      VAR
	bt: BackoffTimer.T;
	clientStatus: SupMisc.ThreadStatus;
      BEGIN
	CASE verbosity OF
	| 0 =>  (* Errors only *)
	    config.trace := NEW(WrLogger.T).init(Stdio.stdout,
	      Logger.Priority.Warning);
	    config.updaterTrace := config.trace;
	    config.listerTrace := config.trace;
	    config.detailerTrace := config.trace;
	| 1 =>  (* Files updated *)
	    config.trace := NEW(WrLogger.T).init(Stdio.stdout,
	      Logger.Priority.Notice);
	    config.updaterTrace := config.trace;
	    config.listerTrace := config.trace;
	    config.detailerTrace := config.trace;
	| 2 =>  (* File update details *)
	    config.trace := NEW(WrLogger.T).init(Stdio.stdout,
	      Logger.Priority.Info);
	    config.updaterTrace := config.trace;
	    config.listerTrace := NEW(WrLogger.T).init(Stdio.stdout,
	      Logger.Priority.Notice);
	    config.detailerTrace := config.listerTrace;
	END;

	TRY
	  IF lockFile # NIL THEN
	    TRY
	      lock := LockFile.Lock(lockFile);
	    EXCEPT OSError.E(l) =>
	      RAISE Error("Error locking \"" & lockFile
		& "\": " & ErrMsg.StrError(l));
	    END;
	    IF lock = NIL THEN  (* Already locked. *)
	      RAISE Error("\"" & lockFile
		& "\" is already locked by another process");
	    END;
	  END;
	  TRY
	    TRY
	      Logger.Info(config.trace, "Parsing supfile \"" & supFile & "\"");
	      config.collections := SupFile.Parse(supFile,
		override := config.override, mask := config.overrideMask);
	    EXCEPT SupFile.Error(msg) =>
	      RAISE Error(msg);
	    END;

	    bt := BackoffTimer.New(
	      min := 300.0d0,	(* 5 minutes *)
	      max := 7200.0d0,	(* 2 hours *)
	      backoff := 2.0,
	      jitter := 0.1);

	    LOOP
	      clientStatus := NEW(FSClient.T).init(config).apply();

	      IF clientStatus.status # SupMisc.ExitCode.TransientFailure
	      OR maxRetries >= 0 AND retryCount >= maxRetries THEN
		EXIT;
	      END;

	      WITH nextTry = Date.FromTime(Time.Now()+BackoffTimer.Get(bt)) DO
		Logger.Warning(config.trace, "Will retry at"
		  & " " & Fmt.Pad(Fmt.Int(nextTry.hour), 2, '0')
		  & ":" & Fmt.Pad(Fmt.Int(nextTry.minute), 2, '0')
		  & ":" & Fmt.Pad(Fmt.Int(nextTry.second), 2, '0'));
	      END;
	      BackoffTimer.AlertPause(bt);
	      Logger.Notice(config.trace, "Retrying");
	      INC(retryCount);
	    END;
	  FINALLY
	    IF lock # NIL THEN
	      TRY
		LockFile.Unlock(lock);
	      EXCEPT OSError.E(l) =>
		RAISE Error("Error unlocking \"" & lockFile
		  & "\": " & ErrMsg.StrError(l));
	      END;
	    END;
	  END;
	  IF clientStatus.status # SupMisc.ExitCode.Success THEN
	    Process.Exit(1);
	  END;
	EXCEPT Error(msg) =>
	  Logger.Err(config.trace, msg);
	  Process.Exit(1);
	END;
      END;
    END;
  EXCEPT
  | Thread.Alerted =>
    ErrMsg.Fatal("Interrupted");
  END;
END Main.
