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
 *)

MODULE Main;

IMPORT
  ClientClass, CVProto, ErrMsg, File, FilePosix, FileWr, Fmt, FS, FSServer,
  IO, IP, Logger, OSError, OSErrorPosix, Params, Pathname, Process,
  RCSComp, RegularFile, RTProcess, Stdio, SupMisc, SysLogger, Text, Thread,
  TimeStampLogger, TokScan, TreeComp, Unix, UnixMisc, Version, WrLogger;

CONST
  ThreadStackSize = 16 * 1024;  (* In units of Word.T *)

VAR
  mainPid: Process.ID;
  config := NEW(FSServer.Configuration);

PROCEDURE BecomeDaemon()
  RAISES {OSError.E} =
  BEGIN
    WITH pid = RTProcess.Fork() DO
      IF pid = -1 THEN  (* Error. *)
	OSErrorPosix.Raise()
      ELSIF pid # 0 THEN  (* Parent process. *)
	Unix.underscore_exit(0);
      END;
    END;

    (* This is the child process. *)

    IF UnixMisc.SetSID() = -1 THEN
      OSErrorPosix.Raise();
    END;

    (* EVAL Unix.chdir(SupMisc.RootDir);  FIXME - Not yet. *)
  END BecomeDaemon;

PROCEDURE CheckPort(port: INTEGER): IP.Port =
  BEGIN
    IF port = IP.NullPort
    OR NOT (FIRST(IP.Port) <= port AND port <= LAST(IP.Port)) THEN
      ErrMsg.Fatal("Invalid port " & Fmt.Int(port));
    END;
    IF port < 1024 THEN
      ErrMsg.Fatal("Reserved port " & Fmt.Int(port) & " not permitted");
    END;
    RETURN port;
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

PROCEDURE GoingDown() =
(* Called (hopefully) when process is killed or crashes. *)
  BEGIN
    IF config.logger # NIL AND Process.GetMyID() = mainPid THEN
      Logger.Notice(config.logger, "Going down");
    END;
  END GoingDown;

PROCEDURE Redirect(in, out, err: File.T) =
  BEGIN
    IF in # NIL THEN EVAL Unix.dup2(in.fd, 0) END;
    IF out # NIL THEN EVAL Unix.dup2(out.fd, 1) END;
    IF err # NIL THEN EVAL Unix.dup2(err.fd, 2) END;
  END Redirect;

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
      & "Usage: " & prog & " [options]"
      & "\n  Options:"
      & "\n    -A addr         Listen for connections on a specific address"
      & "\n    -b base         Base configuration directory (default " &
                                  SupMisc.DefaultServerBase & ")"
      & "\n    -c collDirs     Colon-separated search path for collections,"
				  & " relative"
      & "\n                      to base (default \""
				  & SupMisc.DefaultServerCollDir & "\")"
      & "\n    -C maxClients   Max. simultaneous clients (default 1 then exit)"
      & "\n    -e              Don't close standard error and standard output"
				  & " files"
      & "\n    -f              Don't become a daemon, even if -C is specified"
      & "\n    -l log          Log file, or \"@facility\" (e.g., " &
				  "\"@local0\") to use syslog"
      & "\n    -p port         Alternate TCP/IP port (default " &
				  Fmt.Int(SupMisc.Port) & ")"
      & "\n    -P range        Range for server data port in passive mode"
      & "\n                    (default unrestricted).  Range can be a single"
      & "\n                    port number, or a range \"lo-hi\"."
      & "\n    -s scanDir      Directory where scan files can be found " &
				 "(default none)"
      & "\n    -v              Print version and exit"
      & "\n    -x              Require detailed compare for all RCS files"
      & "\n    -Z compLevel    Compression level (0..9, default " &
				  Fmt.Int(SupMisc.DefaultCompression) & ")"
      );
  END Usage;

VAR
  justPrintVersion := FALSE;
  logName: Pathname.T := NIL;
  logFile: File.T := NIL;
  devNull, outFile : File.T := NIL;
  noFork := FALSE;
  keepStderr := FALSE;
  argNum: CARDINAL;
  argPos: CARDINAL;
  argLen: CARDINAL;
  option: CHAR;
  arg: TEXT;
  argVal: TEXT;
  pos: INTEGER;
  server: FSServer.T;
BEGIN
  TRY
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
	    | 'A' =>
		WITH t = FetchArg(argNum, argPos) DO
		  TRY
		    IF NOT SupMisc.ParseHost(t, config.localEndpoint.addr) THEN
		      ErrMsg.Fatal(t & ": host unknown");
		    END
		  EXCEPT IP.Error(list) =>
		    ErrMsg.Fatal(t & ": " & ErrMsg.StrError(list));
		  END;
		END;
	    | 'b' =>
		config.serverBase := FetchArg(argNum, argPos);
	    | 'c' =>
		config.serverCollDirs := FetchArg(argNum, argPos);
	    | 'C' =>
		config.maxChildren := TokScan.AtoI(FetchArg(argNum, argPos));
	    | 'd' => 
                WITH level = TokScan.AtoI(FetchArg(argNum, argPos)) DO
                  ClientClass.debugLevel := level;
                  TreeComp.traceLevel := level;
                  RCSComp.traceLevel := level;
                END;
	    | 'f' => noFork := TRUE;
	    | 'e' => keepStderr := TRUE;
	    | 'l' => logName := FetchArg(argNum, argPos);
	    | 'p' =>
		config.localEndpoint.port :=
		  CheckPort(TokScan.AtoI(FetchArg(argNum, argPos)));
	    | 'P' =>
		argVal := FetchArg(argNum, argPos);
		pos := Text.FindChar(argVal, '-');
		IF pos >= 0 THEN
		  config.loDataPort :=
		    CheckPort(TokScan.AtoI(Text.Sub(argVal, 0, pos),
		    "low data port"));
		  config.hiDataPort :=
		    CheckPort(TokScan.AtoI(Text.Sub(argVal, pos+1),
		    "high data port"));
		  IF config.loDataPort > config.hiDataPort THEN
		    ErrMsg.Fatal("Invalid data port range " & argVal);
		  END;
		ELSE
		  config.loDataPort :=
		    CheckPort(TokScan.AtoI(argVal, "data port"));
		  config.hiDataPort := config.loDataPort;
		END;
	    | 's' =>
		config.serverScanDir := FetchArg(argNum, argPos);
	    | 't' => 
                WITH level = TokScan.AtoI(FetchArg(argNum, argPos)) DO
                  TreeComp.traceLevel := level;
                  RCSComp.traceLevel := level;
                END;
	    | 'v' => justPrintVersion := TRUE;
	    | 'x' => config.detailAllRCSFiles := TRUE;
	    | 'Z' =>
		WITH val = TokScan.AtoI(FetchArg(argNum, argPos)) DO
		  IF 0 <= val AND val <= 9 THEN
		    config.compLevel := val;
		  ELSE
		    ErrMsg.Fatal("Compression level out of range 0..9");
		  END;
		END;
	    ELSE
	      Usage("Invalid option \"-" & Text.FromChar(option) & "\"");
	    END;
	  END;
	ELSE
	  Usage();
	END;
	INC(argNum);
      END;
    EXCEPT TokScan.Error(msg) =>
      Usage(msg);
    END;

    IF justPrintVersion THEN
      IO.Put("CVSup server\n");
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

    IF logName # NIL THEN
      IF Text.Length(logName) > 0 AND Text.GetChar(logName, 0) = '@' THEN
	(* Log to a syslog facility. *)
	TRY
	  WITH facility = SysLogger.DecodeFacility(Text.Sub(logName, 1)) DO
	    config.logger := NEW(SysLogger.T).init("cvsupd", facility,
	      SysLogger.Options{SysLogger.Option.Pid});
	  END;
	EXCEPT SysLogger.Error(msg) =>
	  ErrMsg.Fatal(msg);
	END;
      ELSE  (* Log to a file. *)
	TRY
	  logFile := FS.OpenFile(logName, truncate := FALSE);
	  TYPECASE logFile OF
	  | RegularFile.T(reg) =>
	      EVAL reg.seek(RegularFile.Origin.End, 0);
	      UnixMisc.AppendAlways(logFile);
	  ELSE (* Ignore *) END;
	  WITH logWr = NEW(FileWr.T).init(logFile) DO
	    config.logger := NEW(WrLogger.T).init(logWr);
	    config.logger := NEW(TimeStampLogger.T).init(config.logger);
	  END;
	EXCEPT OSError.E(l) =>
	  ErrMsg.Fatal("Cannot create \"" & logName & "\"", l);
	END;
      END;
    ELSIF config.maxChildren < 0 THEN  (* Once-only mode. *)
      config.logger := NEW(WrLogger.T).init(Stdio.stdout);
      config.logger := NEW(TimeStampLogger.T).init(config.logger);
    END;

    (* Increase the default stack size for all threads. *)
    Thread.MinDefaultStackSize(ThreadStackSize);

    server := NEW(FSServer.T).init(config);

    IF config.maxChildren >= 0 AND NOT noFork THEN  (* Become a daemon. *)
      TRY
	BecomeDaemon();

	(* Redirect stdin, stdout, and stderr. *)
	devNull := FS.OpenFile(SupMisc.DevNull,
	  truncate := FALSE,
	  create := FS.CreateOption.Never);
	TRY
	  IF logFile # NIL THEN  (* Logging to a file *)
	    outFile := logFile;
	  ELSIF keepStderr THEN
	    outFile := NIL;
	  ELSE
	    outFile := devNull;
	  END;
	  Redirect(in := devNull, out := outFile, err := outFile);
	FINALLY
	  IF devNull.fd > 2 THEN
	    devNull.close();
	  END;
	END;
      EXCEPT OSError.E(l) =>
	ErrMsg.Fatal("Cannot become a daemon", l);
      END;
    END;

    mainPid := Process.GetMyID();
    Process.RegisterExitor(GoingDown);

    server.run();
  EXCEPT
  | FSServer.Error(msg) =>
      ErrMsg.Fatal(msg);
  | Thread.Alerted =>
      ErrMsg.Fatal("Interrupted");
  END;
END Main.
