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
 * $Id: FSClient.m3,v 1.1.1.1 2009-04-09 17:01:39 jkrell Exp $ *)

MODULE FSClient;

IMPORT
  Auth, AuthMD5, ChannelMux, CVProto, Detailer, Env, ErrMsg, FileAttr,
  Fmt, Glob, GlobTree, IOWatchDog, IP, LockFile, Logger, OSError,
  Pathname, Process, RCSKeyword, Rd, Reaper, SigHandler, StreamRd,
  StreamWr, SupFileRec, SupMisc, SyncFixupQueue, TCPMisc, Text,
  Thread, TokScan, TreeList, UnixMisc, Updater, Usignal, Version,
  WatchDog, Wr;

IMPORT SupConnRW AS ConnRW;
IMPORT SupTCP AS TCP;

EXCEPTION
  Error(TEXT);
  TransientFailure(TEXT);

CONST
  AuthFile = ".cvsup/auth";
  IdleTimeout = 15.0d0 * 60.0d0;
  S1GUrl = "http://www.cvsup.org/s1g/";

REVEAL
  T = Public BRANDED OBJECT
      config: Configuration;
      server: TEXT;
      peer: IP.Endpoint;
      proto: CVProto.T := NIL;
      tcp0, tcp1, tcp2, tcp3: TCP.T := NIL;
      mux: ChannelMux.T := NIL;
      rdA, rdB, oldRdA: StreamRd.T := NIL;
      wrA, wrB, oldWrA: StreamWr.T := NIL;
      watchDog: IOWatchDog.T := NIL;
      authPath: Pathname.T;
      authDB: Auth.DB := NIL;
    OVERRIDES
      init := Init;
      apply := Apply;
    END;

PROCEDURE Init(self: T; config: Configuration): T =
  BEGIN
    self.config := config;
    IF self.config.hiDataPort = IP.NullPort THEN
      self.config.hiDataPort := self.config.loDataPort;
    END;
    RETURN self;
  END Init;

PROCEDURE Apply(self: T): REFANY =
  VAR
    fixups: SyncFixupQueue.T;
    reaper: Reaper.T;
    lister: TreeList.T;
    detailer: Detailer.T;
    updater: Updater.T;
    listerThread: Thread.T;
    detailerThread: Thread.T;
    updaterThread: Thread.T;
    threadStatus: SupMisc.ThreadStatus;
    lock: LockFile.T := NIL;
    homeDir: Pathname.T;
    killer: Killer := NIL;
    retVal: REFANY;
    thread: Thread.T;
  BEGIN
    TRY
      IF self.config.lockFile # NIL THEN
	TRY
	  lock := LockFile.Lock(self.config.lockFile);
	EXCEPT OSError.E(l) =>
	  RAISE Error("Error locking \"" & self.config.lockFile
	    & "\": " & ErrMsg.StrError(l));
	END;
	IF lock = NIL THEN  (* Already locked. *)
	  RAISE Error("\"" & self.config.lockFile
	    & "\" is already locked by another process");
	END;
      END;
      TRY
	homeDir := Env.Get("HOME");
	IF homeDir = NIL THEN
	  RAISE Error("Environment variable $\"HOME\" is not set");
	END;
	self.authPath := SupMisc.ResolvePath(homeDir, AuthFile);
	TRY
	  self.authDB := Auth.Open(self.authPath);
	EXCEPT Auth.Error(msg) =>
	  IF self.config.authRequired THEN
	    RAISE Error(msg);
	  END;
	END;
	TRY
	  IF CheckCollections(self) = 0 THEN
	    RAISE Error("No collections selected");
	  END;

	  self.server := self.config.collections.get(0).serverHost;

	  TRY
	    EstablishControlConnection(self);

	    self.watchDog := NEW(IOWatchDog.T).init(timeout := IdleTimeout);
	    TRY
	      IOWatchDog.AddRd(self.watchDog, self.rdA);
	      IOWatchDog.AddWr(self.watchDog, self.wrA);

	      ShakeHands(self);
	      AdjustForProtocol(self);
	      Authorize(self);
	      ExchangeAttributeInfo(self);
	      ExchangeCollectionInfo(self);
	      EstablishDataConnection(self);

	      reaper := NEW(Reaper.T).init();
	      fixups := NEW(SyncFixupQueue.T).init();

	      lister := NEW(TreeList.T).init(
		proto := self.proto,
		wr := self.wrA,
		collections := self.config.collections,
		reaper := reaper,
		stats := self.config.listerStats,
		trace := self.config.listerTrace);

	      detailer := NEW(Detailer.T).init(
		proto := self.proto,
		rd := self.rdA,
		wr := self.wrB,
		collections := self.config.collections,
		fixups := fixups,
		reaper := reaper,
		stats := self.config.detailerStats,
		logger := self.config.detailerTrace);

	      updater := NEW(Updater.T).init(
		proto := self.proto,
		rd := self.rdB,
		collections := self.config.collections,
		fixups := fixups,
		deleteLimit := self.config.deleteLimit,
		reaper := reaper,
		destDir := self.config.destDir,
		stats := self.config.updaterStats,
		trace := self.config.updaterTrace);

	      TRY
		killer := NEW(Killer, thread := Thread.Self());
		IF NOT UnixMisc.SigIsIgnored(Usignal.SIGINT) THEN
		  SigHandler.Register(Usignal.SIGINT, killer);
		END;
		IF NOT UnixMisc.SigIsIgnored(Usignal.SIGHUP) THEN
		  SigHandler.Register(Usignal.SIGHUP, killer);
		END;
		IF NOT UnixMisc.SigIsIgnored(Usignal.SIGTERM) THEN
		  SigHandler.Register(Usignal.SIGTERM, killer);
		END;

		(* Start the worker threads. *)
		listerThread := Reaper.Fork(reaper, lister);
		detailerThread := Reaper.Fork(reaper, detailer);
		updaterThread := Reaper.Fork(reaper, updater);

		TRY
		  Trace(self, "Running");

		  (* Wait until all the threads have finished, or until an
		     error is returned from one of them. *)
		  TRY
		    threadStatus := NIL;
		    WHILE Reaper.AlertJoinNext(reaper, thread, retVal) DO
		      threadStatus := retVal;
		      IF threadStatus.status # SupMisc.ExitCode.Success THEN
			Err(self, threadStatus.message);
			Reaper.AlertAll(reaper);  (* Kill remaining threads. *)
			EXIT
		      END;
		    END;
		  EXCEPT Thread.Alerted =>
		    (* Interrupted from GUI or signal. *)
		    Reaper.AlertAll(reaper);
		    Trace(self, "Cleaning up ...");
		    RAISE Thread.Alerted;
		  END;
		FINALLY
		  (* Reap all the remaining threads. *)
		  WHILE Reaper.JoinNext(reaper, thread, retVal) DO
		    (* Nothing *)
		  END;
		END;
	      FINALLY
		IF NOT UnixMisc.SigIsIgnored(Usignal.SIGINT) THEN
		  SigHandler.Register(Usignal.SIGINT, NIL);
		END;
		IF NOT UnixMisc.SigIsIgnored(Usignal.SIGHUP) THEN
		  SigHandler.Register(Usignal.SIGHUP, NIL);
		END;
		IF NOT UnixMisc.SigIsIgnored(Usignal.SIGTERM) THEN
		  SigHandler.Register(Usignal.SIGTERM, NIL);
		END;
	      END;

	      IF threadStatus.status = SupMisc.ExitCode.Success THEN
		Trace(self, "Shutting down connection to server");
		Wr.Close(self.wrA);
		self.wrA := NIL;
		Wr.Close(self.wrB);
		self.wrB := NIL;
		IF NOT Rd.EOF(self.rdA) THEN
		  RAISE Error("Detailer protocol error: Expected EOF, " &
		      "didn't get it");
		END;
		IF NOT Rd.EOF(self.rdB) THEN
		  RAISE Error("Updater protocol error: Expected EOF, " &
		    "didn't get it");
		END;
		IF self.mux # NIL THEN
		  ChannelMux.Close(self.mux);
		  self.mux := NIL;
		END;
		IF self.oldWrA # NIL THEN
		  Wr.Close(self.oldWrA);
		  self.oldWrA := NIL;
		END;
		Notice(self, "Finished successfully");
	      END;
	    FINALLY
	      IOWatchDog.Cancel(self.watchDog);
	    END;
	  FINALLY
	    ShutdownConnections(self);
	  END;
	  threadStatus.message := NIL;  (* Already printed. *)
	  RETURN threadStatus;
	FINALLY
	  IF self.authDB # NIL THEN
	    TRY
	      Auth.Close(self.authDB);
	    EXCEPT ELSE END;
	    self.authDB := NIL;
	  END;
	END;
      FINALLY
	IF lock # NIL THEN
	  TRY
	    LockFile.Unlock(lock);
	  EXCEPT OSError.E(l) =>
	    RAISE Error("Error unlocking \"" & self.config.lockFile
	      & "\": " & ErrMsg.StrError(l));
	  END;
	END;
      END;
    EXCEPT
    | Error(msg) =>
	Err(self, msg);
	RETURN NEW(SupMisc.ThreadStatus,
	  status := SupMisc.ExitCode.Failure);
    | Rd.EndOfFile =>
	Err(self, "Premature EOF from server");
	RETURN NEW(SupMisc.ThreadStatus,
	  status := SupMisc.ExitCode.TransientFailure);
    | Rd.Failure(list) =>
	Err(self, "Network read failure: " & ErrMsg.StrError(list));
	RETURN NEW(SupMisc.ThreadStatus,
	  status := SupMisc.ExitCode.TransientFailure);
    | Thread.Alerted =>
	IF self.watchDog # NIL AND IOWatchDog.Expired(self.watchDog) THEN
	  Err(self, "Inactivity timeout");
	  RETURN NEW(SupMisc.ThreadStatus,
	    status := SupMisc.ExitCode.TransientFailure);
	ELSE
	  Err(self, "Interrupted");
	  IF killer # NIL AND killer.killedBySignal # -1 THEN
	    EVAL Usignal.kill(Process.GetMyID(), killer.killedBySignal);
	  END;
	  RETURN NEW(SupMisc.ThreadStatus,
	    status := SupMisc.ExitCode.Failure);
	END;
    | TransientFailure(msg) =>
	Err(self, msg);
	RETURN NEW(SupMisc.ThreadStatus,
	  status := SupMisc.ExitCode.TransientFailure);
    | Wr.Failure(list) =>
	Err(self, "Network write failure: " & ErrMsg.StrError(list));
	RETURN NEW(SupMisc.ThreadStatus,
	  status := SupMisc.ExitCode.TransientFailure);
    END;
  END Apply;

PROCEDURE Accept(self: T;
                 addr: IP.Address;
		 cmd: TEXT): TCP.T
  RAISES {Thread.Alerted, TransientFailure, Wr.Failure} =
(* Creates a connector, binds it to an acceptable port on the given
   address, sends its endpoint with the command "cmd", accepts one
   connection from the server, and closes the connector. *)
  VAR
    conn: TCP.Connector;
    wd: WatchDog.T;
    epToks: ARRAY [0..4] OF TEXT;
  BEGIN
    conn := NewConnector(addr, self.config.loDataPort, self.config.hiDataPort);
    TRY
      TokScan.EncodeEndpoint(TCP.GetEndPoint(conn), epToks);
      self.proto.putCmd(self.wrA, cmd,
	epToks[0], epToks[1], epToks[2], epToks[3], epToks[4]);
      Wr.Flush(self.wrA);
      TRY
	wd := WatchDog.New(SupMisc.ListenTimeout);
	TRY
	  RETURN TCP.Accept(conn);
	FINALLY
	  WatchDog.Cancel(wd);
	END;
      EXCEPT
      | IP.Error(list) =>
	  RAISE TransientFailure("Accept failed: "
	    & ErrMsg.StrError(list));
      | Thread.Alerted =>
	  IF WatchDog.Expired(wd) THEN
	    RAISE TransientFailure(
	      "Timed out waiting for connection from server.  Check your"
	      & " firewall setup or try the \"-P m\" option");
	  ELSE
	    RAISE Thread.Alerted;
	  END;
      END;
    FINALLY
      TCP.CloseConnector(conn);
    END;
  END Accept;

PROCEDURE ShutdownConnections(self: T)
  RAISES {Thread.Alerted} =
(* Closes all the network connections, being careful to get it all done
   even if exceptions occur along the way. *)
  BEGIN
    IF self.mux # NIL THEN ChannelMux.Close(self.mux) END;

    (* Close all of the TCP channels, being careful to avoid any delays.
       This will close the file descriptors right out from under the
       associated readers and writers.  Below, when we close the readers
       and writers themselves, we'll get some Rd.Failure and Wr.Failure
       exceptions.  But that is better than hanging up waiting for the
       buffer flushes to complete. *)

    IF self.tcp0 # NIL THEN
      TRY TurnOffLinger(self.tcp0) EXCEPT Error => (* Ignore *) END;
      TCP.Close(self.tcp0);
    END;
    IF self.tcp1 # NIL THEN
      TRY TurnOffLinger(self.tcp1) EXCEPT Error => (* Ignore *) END;
      TCP.Close(self.tcp1);
    END;
    IF self.tcp2 # NIL THEN
      TRY TurnOffLinger(self.tcp2) EXCEPT Error => (* Ignore *) END;
      TCP.Close(self.tcp2);
    END;
    IF self.tcp3 # NIL THEN
      TRY TurnOffLinger(self.tcp3) EXCEPT Error => (* Ignore *) END;
      TCP.Close(self.tcp3);
    END;

    (* Close the readers and writers. *)
    TRY
      IF self.oldRdA # NIL THEN
	TRY Rd.Close(self.oldRdA) EXCEPT Rd.Failure => (* Ignore *) END;
      END;
    FINALLY
      TRY
	IF self.rdA # NIL THEN
	  TRY Rd.Close(self.rdA) EXCEPT Rd.Failure => (* Ignore *) END;
	END;
      FINALLY
	TRY
	  IF self.rdB # NIL THEN
	    TRY Rd.Close(self.rdB) EXCEPT Rd.Failure => (* Ignore *) END;
	  END;
	FINALLY
	  TRY
	    IF self.oldWrA # NIL THEN
	      TRY Wr.Close(self.oldWrA) EXCEPT Wr.Failure => (* Ignore *) END;
	    END;
	  FINALLY
	    TRY
	      IF self.wrA # NIL THEN
		TRY Wr.Close(self.wrA) EXCEPT Wr.Failure => (* Ignore *) END;
	      END;
	    FINALLY
	      IF self.wrB # NIL THEN
		TRY Wr.Close(self.wrB) EXCEPT Wr.Failure => (* Ignore *) END;
	      END;
	    END;
	  END;
	END;
      END;
    END;
  END ShutdownConnections;

(*****************************************************************************)

PROCEDURE GetSockName(tcp: TCP.T): IP.Endpoint
  RAISES {Error} =
  BEGIN
    TRY
      RETURN TCPMisc.GetSockName(tcp);
    EXCEPT IP.Error(list) =>
      RAISE Error("GetSockName failed: " & ErrMsg.StrError(list));
    END;
  END GetSockName;

PROCEDURE NewConnector(addr: IP.Address;
                       loPort, hiPort: IP.Port): TCP.Connector
  RAISES {TransientFailure} =
  BEGIN
    TRY
      RETURN SupMisc.NewConnector(addr, loPort, hiPort);
    EXCEPT IP.Error(list) =>
      RAISE TransientFailure("Listen failed: " & ErrMsg.StrError(list));
    END;
  END NewConnector;

PROCEDURE TurnOffLinger(tcp: TCP.T)
  RAISES {Error} =
  BEGIN
    TRY
      TCPMisc.LingerOnClose(tcp, FALSE);
    EXCEPT IP.Error(list) =>
      RAISE Error("Cannot turn off SO_LINGER: " &
	ErrMsg.StrError(list));
    END;
  END TurnOffLinger;

PROCEDURE TurnOffNoDelay(tcp: TCP.T)
  RAISES {Error} =
  BEGIN
    TRY
      TCPMisc.CoalesceWrites(tcp, TRUE);
    EXCEPT IP.Error(list) =>
      RAISE Error("Cannot turn off TCP_NODELAY: " &
	ErrMsg.StrError(list));
    END;
  END TurnOffNoDelay;

(*****************************************************************************)

PROCEDURE Err(self: T; msg: TEXT) =
  BEGIN
    IF self.config.trace # NIL THEN
      Logger.Put(self.config.trace, Logger.Priority.Err, msg);
    END;
  END Err;

PROCEDURE Notice(self: T; msg: TEXT) =
  BEGIN
    IF self.config.trace # NIL THEN
      Logger.Put(self.config.trace, Logger.Priority.Notice, msg);
    END;
  END Notice;

PROCEDURE Trace(self: T; msg: TEXT) =
  BEGIN
    IF self.config.trace # NIL THEN
      Logger.Put(self.config.trace, Logger.Priority.Info, msg);
    END;
  END Trace;

PROCEDURE Warn(self: T; msg: TEXT) =
  BEGIN
    IF self.config.trace # NIL THEN
      Logger.Put(self.config.trace, Logger.Priority.Warning, msg);
    END;
  END Warn;

(*****************************************************************************)

PROCEDURE ShakeHands(self: T)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted, TransientFailure,
	  Wr.Failure} =
  VAR
    ts: TokScan.T;
    cmd: TEXT;
    serverMajor, serverMinor: CVProto.VersionNumber;
    actualMajor, actualMinor: CVProto.VersionNumber;
    serverSW: TEXT;
  BEGIN
    TRY
      ts := TokScan.New(SupMisc.GetCmdLine(self.rdA));
      cmd := ts.getToken("greeting");
      IF TokScan.EqualFolded(cmd, "OK") THEN
	serverMajor := ts.getInt("server protocol major version");
	serverMinor := ts.getInt("server protocol minor version");
	IF NOT ts.next(serverSW) THEN serverSW := "." END;
      ELSIF TokScan.EqualFolded(cmd, "!") THEN
	RAISE TransientFailure("Rejected by server: " & ts.getRest());
      ELSE
	RAISE TokScan.Error("Invalid greeting from server");
      END;

      SupMisc.PutCmd(self.wrA, "PROTO",
	Fmt.Int(CVProto.Current.major),
	Fmt.Int(CVProto.Current.minor),
	Version.Name);
      Wr.Flush(self.wrA);

      ts := TokScan.New(SupMisc.GetCmdLine(self.rdA));
      cmd := ts.getToken("PROTO");
      IF TokScan.EqualFolded(cmd, "PROTO") THEN
	actualMajor := ts.getInt("actual protocol major version");
	actualMinor := ts.getInt("actual protocol minor version");
      ELSIF TokScan.EqualFolded(cmd, "!") THEN
	RAISE Error("Protocol negotiation failed: " & ts.getRest());
      ELSE
	RAISE TokScan.Error("Invalid PROTO command from server");
      END;

      self.proto := CVProto.Lookup(actualMajor, actualMinor);

      IF self.proto.v.exchangesVersions THEN
	Trace(self, "Server software version: " & serverSW);
      END;

      IF CVProto.HasS1GBug(self.proto, serverSW) THEN
	Err(self, "Server " & self.server & " has the S1G bug");
	Err(self, "See " & S1GUrl & " for details");
	Err(self, "Please notify the maintainer of " & self.server);
	RAISE Error("Refusing update from server with S1G bug");
      END;

      IF self.proto.major # CVProto.Current.major
      OR self.proto.minor # CVProto.Current.minor THEN
	Notice(self, "Falling back to protocol version "
	  & Fmt.Int(self.proto.major) & "." & Fmt.Int(self.proto.minor));
      END;
    EXCEPT
    | TokScan.Error(msg) =>
	RAISE Error("Startup protocol error: " & msg);
    | CVProto.NotSupported =>
	RAISE Error("Server protocol version"
	  & " " & Fmt.Int(serverMajor) & "." & Fmt.Int(serverMinor)
	  & " " & "not supported by client");
    END;
  END ShakeHands;

PROCEDURE AdjustForProtocol(self: T)
  RAISES {Error} =
(* Make sure the client and server can work together, and adjust various
   options according to what has been negotiated. *)
  BEGIN
    CASE self.config.connectMode OF
    | ConnectMode.Default =>
	IF self.proto.v.hasMuxMode THEN
	  self.config.connectMode := ConnectMode.Mux;
	ELSE
	  self.config.connectMode := ConnectMode.Active;
	END;
    | ConnectMode.Active, ConnectMode.Passive, ConnectMode.Socks =>
	(* Nothing special *)
    | ConnectMode.Mux =>
	IF NOT self.proto.v.hasMuxMode THEN
	  RAISE Error("Server does not support multiplexed mode");
	END;
    END;

    FOR i := 0 TO self.config.collections.size()-1 DO
      WITH sfr = self.config.collections.get(i) DO
	IF NOT SupFileRec.Option.Skip IN sfr.options THEN
	  IF NOT self.proto.v.hasClientAccepts AND sfr.accepts.size() > 0 THEN
	    RAISE Error("Server does not support the \"-i pattern\" option");
	  END;
	END;
      END;
    END;

    IF self.config.authRequired AND NOT self.proto.v.hasMD5Auth THEN
      RAISE Error("Server does not support authentication");
    END;
  END AdjustForProtocol;

PROCEDURE Authorize(self: T)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted, Wr.Failure} =
  VAR
    user: TEXT;
    host: TEXT;
    ts: TokScan.T;
  BEGIN
    TRY
      user := UnixMisc.GetLogin();
      IF user = NIL THEN user := "?" END;
      TRY host := UnixMisc.GetHostName() EXCEPT OSError.E => host := "?" END;
      (* Early servers expected only the user name, and not the host name.
         But no servers ever checked for the end of the line, so even early
	 ones can handle the host name.  Therefore we don't need a new
	 protocol version to distinguish between when it is sent and when
	 it is not. *)
      self.proto.putCmd(self.wrA, "USER", user, host);
      Wr.Flush(self.wrA);
      IF self.proto.v.hasMD5Auth THEN
	DoMD5Auth(self);
      ELSE
	ts := self.proto.getCmd(self.rdA);
	ts.getFolded("OK");
      END;
    EXCEPT
    | TokScan.Error(msg) =>
	RAISE Error("Authorization protocol error: " & msg);
    END;
  END Authorize;

PROCEDURE DoMD5Auth(self: T)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted, TokScan.Error,
    Wr.Failure} =
  VAR
    ts: TokScan.T;
    realm: TEXT;
    serverChallenge: TEXT;
    serverResponse: TEXT;
    client := ".";
    clientResponse := ".";
    clientChallenge := ".";
    sharedSecret := "*";
    auth: Auth.T;
    cmd: TEXT;
  BEGIN
    ts := self.proto.getCmd(self.rdA);
    ts.getFolded("AUTHMD5");
    realm := ts.getToken("realm");
    serverChallenge := ts.getToken("server challenge");
    ts.getEnd("end of AUTHMD5 command");
    IF self.config.authRequired OR NOT Text.Equal(serverChallenge, ".") THEN
      IF self.authDB = NIL THEN
	RAISE Error("Authentication required, but could not open \"" &
	  self.authPath & "\"");
      END;
      IF Text.Equal(realm, ".") THEN
	RAISE Error("Authentication required, but not enabled on server");
      END;
      TRY
	IF NOT Auth.Lookup(self.authDB, realm, auth) THEN
	  RAISE Error("No record for server \"" & realm & "\" in \"" &
	    self.authPath & "\"");
	END;
      EXCEPT Auth.Error(msg) =>
	RAISE Error(msg);
      END;
      client := auth.client;
      sharedSecret := AuthMD5.MakeSecret(auth.server, auth.client,
	auth.password);
    END;
    IF NOT Text.Equal(serverChallenge, ".") THEN
      clientResponse := AuthMD5.GenResponse(serverChallenge, sharedSecret);
    END;
    IF self.config.authRequired THEN
      clientChallenge := AuthMD5.GenChallenge(self.peer.addr, ".");
    END;
    self.proto.putCmd(self.wrA, "AUTHMD5", client, clientResponse,
      clientChallenge);
    Wr.Flush(self.wrA);
    ts := self.proto.getCmd(self.rdA);
    cmd := ts.getToken("server reply to AUTHMD5");
    IF TokScan.EqualFolded(cmd, "OK") THEN
      serverResponse := ts.getToken("server auth response");
      IF self.config.authRequired
      AND NOT AuthMD5.CheckResponse(serverResponse, clientChallenge,
	sharedSecret)
      THEN
	RAISE Error("Server failed to authenticate itself to client");
      END;
    ELSIF Text.Equal(cmd, "!") THEN
      RAISE Error("Server error: " & ts.getRest());
    ELSE
      RAISE Error("Invalid server reply to AUTHMD5");
    END;
  END DoMD5Auth;

PROCEDURE ExchangeAttributeInfo(self: T)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted, Wr.Failure} =
  VAR
    ts: TokScan.T;
    count: INTEGER;
    tok: TEXT;
  BEGIN
    IF self.proto.v.hasFileAttrs THEN
      TRY
	Trace(self, "Negotiating file attribute support");
	self.proto.v.attrSupport := FileAttr.Supported;
	self.proto.putCmd(self.wrA, "ATTR",
	  Fmt.Int(NUMBER(self.proto.v.attrSupport)));
	FOR i := FIRST(self.proto.v.attrSupport) TO
	  LAST(self.proto.v.attrSupport)
	DO
	  Wr.PutText(self.wrA,
	    FileAttr.EncodeAttrTypes(self.proto.v.attrSupport[i]) & "\n");
	END;
	self.proto.putCmd(self.wrA, ".");
	Wr.Flush(self.wrA);

	ts := self.proto.getCmd(self.rdA);
	ts.getFolded("ATTR");
	count := ts.getInt("number of file types");
	ts.getEnd("end of \"ATTR\" command");

	IF count > NUMBER(self.proto.v.attrSupport) THEN
	  RAISE TokScan.Error("Invalid file type count");
	END;

	FOR i := 0 TO count-1 DO
	  ts := self.proto.getCmd(self.rdA);
	  tok := ts.getToken("attrTypes");
	  ts.getEnd("end of attrTypes");
	  WITH as = self.proto.v.attrSupport[VAL(i, FileAttr.FileType)] DO
	    as := as * FileAttr.DecodeAttrTypes(tok);
	  END;
	END;
	ts := self.proto.getCmd(self.rdA);
	ts.getLiteral(".");
	ts.getEnd("end of \".\"");

	FOR i := count TO NUMBER(self.proto.v.attrSupport)-1 DO
	  self.proto.v.attrSupport[VAL(i, FileAttr.FileType)] :=
	    FileAttr.AttrTypes{};
	END;
      EXCEPT TokScan.Error(msg) =>
	RAISE Error("Protocol error negotiating attribute support: " & msg);
      END;
    ELSE
      self.proto.v.attrSupport := FileAttr.Historical;
    END;
  END ExchangeAttributeInfo;

PROCEDURE ExchangeCollectionInfo(self: T)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted, Wr.Failure} =
(* All "Rd" and "Wr" exceptions relate to the network connection. *)
  CONST
    ServerMaySet = SupFileRec.Options{
      SupFileRec.Option.Skip,
      SupFileRec.Option.NoRsync,
      SupFileRec.Option.NoRCS};
    ServerMayClear = SupFileRec.Options{
      SupFileRec.Option.CheckRCS};
  VAR (* CONST *)
    NoWS              := GlobTree.Not(GlobTree.Match("*[ \t\r\n]*"));
  VAR
    sfr: SupFileRec.T;
    ts: TokScan.T;
    cmd: TEXT;
    dirAccept     := GlobTree.True;
    fileAccept    := GlobTree.True;
    dirRefuse     := GlobTree.False;
    fileRefuse    := GlobTree.False;
    pat: TEXT;
    aliasName: TEXT;
    keyName: TEXT;
  BEGIN
    Trace(self, "Exchanging collection information");
    TRY
      (* Send all the supfile information to the server. *)
      FOR i := 0 TO self.config.collections.size()-1 DO
	sfr := self.config.collections.get(i);
	IF NOT SupFileRec.Option.Skip IN sfr.options THEN
	  self.proto.putCmd(self.wrA, "COLL",
	    sfr.collection,
	    sfr.release,
	    more := TRUE);
	  IF self.proto.v.clientSendsUmask THEN
	    self.proto.putCmd(self.wrA, NIL,
	      Fmt.Unsigned(sfr.umask, base := 8),
	      more := TRUE);
	  END;
	  self.proto.putCmd(self.wrA, NIL,
	    SupFileRec.EncodeOptions(sfr.options));
	  IF self.proto.v.hasClientAccepts THEN
	    FOR j := 0 TO sfr.accepts.size()-1 DO
	      self.proto.putCmd(self.wrA, "ACC", sfr.accepts.get(j));
	    END;
	  END;
	  FOR j := 0 TO sfr.refusals.size()-1 DO
	    self.proto.putCmd(self.wrA, "REF", sfr.refusals.get(j));
	  END;
	  self.proto.putCmd(self.wrA, ".");
	END;
      END;
      self.proto.putCmd(self.wrA, ".");
      Wr.Flush(self.wrA);

      (* Read back the filtering details. *)
      FOR i := 0 TO self.config.collections.size()-1 DO
	sfr := self.config.collections.get(i);
	IF NOT SupFileRec.Option.Skip IN sfr.options THEN
	  ts := self.proto.getCmd(self.rdA);
	  ts.getFolded("COLL");
	  WITH collection = ts.getToken("collection name") DO
	    IF NOT Text.Equal(collection, sfr.collection) THEN
	      RAISE TokScan.Error("Expected collection \"" &
		sfr.collection & "\", got \"" & collection & "\"");
	    END;
	  END;
	  WITH release = ts.getToken("release name") DO
	    IF NOT Text.Equal(release, sfr.release) THEN
	      RAISE TokScan.Error("Expected release \"" &
		sfr.release & "\", got \"" & release & "\"");
	    END;
	  END;
	  WITH so = SupFileRec.DecodeOptions(ts.getToken("options")) DO
	    sfr.options := sfr.options +
	      ServerMaySet * so -
	      ServerMayClear * (SupFileRec.AllOptions - so);
	  END;

	  LOOP
	    ts := self.proto.getCmd(self.rdA);
	    cmd := ts.getToken("command");
	    IF Text.Equal(cmd, ".") THEN EXIT END;
	    IF TokScan.EqualFolded(cmd, "PRFX") THEN
	      sfr.keywordPrefix := ts.getToken("keyword prefix");
	    ELSIF TokScan.EqualFolded(cmd, "KEYALIAS") THEN
	      aliasName := ts.getToken("alias name");
	      keyName := ts.getToken("RCS keyword");
	      TRY
		sfr.expander.alias(aliasName, keyName);
	      EXCEPT RCSKeyword.Unknown =>
		Warn(self, "Attempt by server to alias unknown RCS keyword \""
		  & keyName & "\"");
	      END;
	    ELSIF TokScan.EqualFolded(cmd, "KEYON") THEN
	      keyName := ts.getToken("RCS keyword");
	      IF Text.Equal(keyName, ".") THEN
		sfr.expander.enableAll();
	      ELSE
		TRY
		  sfr.expander.enable(keyName);
		EXCEPT RCSKeyword.Unknown =>
		  Warn(self, "Attempt by server to enable unknown RCS"
		    & " keyword \"" & keyName & "\"");
		END;
	      END;
	    ELSIF TokScan.EqualFolded(cmd, "KEYOFF") THEN
	      keyName := ts.getToken("RCS keyword");
	      IF Text.Equal(keyName, ".") THEN
		sfr.expander.enableAll(enabled := FALSE);
	      ELSE
		TRY
		  sfr.expander.enable(keyName, enabled := FALSE);
		EXCEPT RCSKeyword.Unknown =>
		  Warn(self, "Attempt by server to disable unknown RCS"
		    & " keyword \"" & keyName & "\"");
		END;
	      END;
	    ELSIF TokScan.EqualFolded(cmd, "NORS") THEN
	      pat := ts.getToken("pattern");
	      sfr.noRsync := GlobTree.Or(sfr.noRsync,
		GlobTree.Match(pat, Glob.MatchOptions{
		  Glob.MatchOption.Pathname }));
	    ELSIF TokScan.EqualFolded(cmd, "RNORS") THEN
	      pat := ts.getToken("pattern");
	      sfr.noRsync := GlobTree.Or(sfr.noRsync,
		GlobTree.Match(pat, Glob.MatchOptions{
		  Glob.MatchOption.Pathname,
		  Glob.MatchOption.LeadingDir }));
	    ELSIF TokScan.EqualFolded(cmd, "!") THEN
	      sfr.options := sfr.options +
		SupFileRec.Options{SupFileRec.Option.Skip};
	      Warn(self, "Server message: " & ts.getRest());
	    ELSIF TokScan.EqualFolded(cmd, "UPGR")
	    OR TokScan.EqualFolded(cmd, "ALWS")
	    OR TokScan.EqualFolded(cmd, "OANY") THEN
	      EVAL ts.getToken("pattern");  (* Not used any more. *)
	    ELSE
	      RAISE TokScan.Error("Invalid command \"" & cmd & "\"");
	    END;
	  END;

	  IF sfr.accepts.size() > 0 THEN
	    dirAccept := GlobTree.False;
	    fileAccept := GlobTree.False;
	    FOR i := 0 TO sfr.accepts.size()-1 DO
	      pat := sfr.accepts.get(i);
	      dirAccept := GlobTree.Or(dirAccept,
		GlobTree.Match(pat, Glob.MatchOptions{
		  Glob.MatchOption.Pathname,
		  Glob.MatchOption.LeadingDir,
		  Glob.MatchOption.PrefixDirs }));
	      IF SupFileRec.Option.CheckoutMode IN sfr.options THEN
		(* We must modify the pattern so that it refers
		   to the RCS file, rather than the checked-out file. *)
		WITH len = Text.Length(pat) DO
		  IF len = 0 OR Text.GetChar(pat, len-1) # '*' THEN
		    pat := pat & SupMisc.RCSSuffix;
		  END;
		END;
	      END;
	      fileAccept := GlobTree.Or(fileAccept,
		GlobTree.Match(pat, Glob.MatchOptions{
		  Glob.MatchOption.Pathname,
		  Glob.MatchOption.LeadingDir }));
	    END;
	  END;

	  FOR j := 0 TO sfr.refusals.size()-1 DO
	    pat := sfr.refusals.get(j);
	    dirRefuse := GlobTree.Or(dirRefuse, GlobTree.Match(pat));
	    IF SupFileRec.Option.CheckoutMode IN sfr.options THEN
	      (* We must modify the pattern so that it refers
		 to the RCS file, rather than the checked-out file. *)
	      WITH len = Text.Length(pat) DO
		IF len = 0 OR Text.GetChar(pat, len-1) # '*' THEN
		  pat := pat & SupMisc.RCSSuffix;
		END;
	      END;
	    END;
	    fileRefuse := GlobTree.Or(fileRefuse, GlobTree.Match(pat));
	  END;

	  sfr.dirFilter := GlobTree.And(dirAccept, GlobTree.Not(dirRefuse));
	  sfr.fileFilter := GlobTree.And(fileAccept, GlobTree.Not(fileRefuse));

	  (* If the server can't handle white space in file names, make sure
	     we don't send him any. *)
	  IF NOT self.proto.v.handlesWhiteSpace THEN
	    sfr.dirFilter := GlobTree.And(sfr.dirFilter, NoWS);
	    sfr.fileFilter := GlobTree.And(sfr.fileFilter, NoWS);
	  END;

	  (* Set up a mask of file attribes that we don't want to sync
	     with the server. *)
	  IF NOT SupFileRec.Option.SetOwner IN sfr.options THEN
	    sfr.attrIgnore := sfr.attrIgnore + FileAttr.AttrTypes{
	      FileAttr.AttrType.Owner, FileAttr.AttrType.Group };
	  END;
	  IF NOT SupFileRec.Option.SetMode IN sfr.options THEN
	    sfr.attrIgnore := sfr.attrIgnore + FileAttr.AttrTypes{
	      FileAttr.AttrType.Mode };
	  END;
	  IF NOT SupFileRec.Option.SetFlags IN sfr.options THEN
	    sfr.attrIgnore := sfr.attrIgnore + FileAttr.AttrTypes{
	      FileAttr.AttrType.Flags };
	  END;
	END;
      END;
      ts := self.proto.getCmd(self.rdA);
      ts.getLiteral(".");
    EXCEPT TokScan.Error(msg) =>
      RAISE Error("Protocol error exchanging collection info: " & msg);
    END;
  END ExchangeCollectionInfo;

PROCEDURE EstablishControlConnection(self: T)
  RAISES {Error, Thread.Alerted, TransientFailure} =
  BEGIN
    TRY
      IF NOT SupMisc.ParseHost(self.server, self.peer.addr) THEN
	RAISE Error("Unknown host \"" & self.server & "\"");
      END;
    EXCEPT IP.Error(list) =>
      RAISE TransientFailure("Name lookup failure for \""
	& self.server & "\": " & ErrMsg.StrError(list));
    END;
    self.peer.port := self.config.port;

    Trace(self, "Connecting to " & self.server);
    TRY
      self.tcp0 := TCPMisc.ConnectFrom(self.peer, self.config.localEndpoint);
    EXCEPT IP.Error(list) =>
      RAISE TransientFailure("Cannot connect to " & self.server & ": " &
	ErrMsg.StrError(list));
    END;
    TurnOffNoDelay(self.tcp0);
    self.rdA := ConnRW.NewRd(self.tcp0);
    self.wrA := ConnRW.NewWr(self.tcp0);

    Notice(self, "Connected to " & self.server);
  END EstablishControlConnection;

PROCEDURE EstablishDataConnection(self: T)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted, TransientFailure,
	  Wr.Failure} =
  VAR
    ts: TokScan.T;
    addr: IP.Address;
    ep: IP.Endpoint;
    id: ChannelMux.ChannelID;
    chan0, chan1: ChannelMux.Channel;
  BEGIN
    TRY
      CASE self.config.connectMode OF
      | ConnectMode.Active =>
	  Trace(self, "Establishing active-mode data connection");
	  addr := GetSockName(self.tcp0).addr;
	  self.tcp1 := Accept(self, addr, "PORT");

	  TurnOffNoDelay(self.tcp1);
	  self.rdB := ConnRW.NewRd(self.tcp1);
	  IOWatchDog.AddRd(self.watchDog, self.rdB);
	  self.wrB := ConnRW.NewWr(self.tcp1);
	  IOWatchDog.AddWr(self.watchDog, self.wrB);
      | ConnectMode.Socks =>
	  Trace(self, "Establishing SOCKS-mode data connection");
	  addr := GetSockName(self.tcp0).addr;
	  self.tcp1 := Accept(self, addr, "SOCKS1");
	  self.tcp2 := Accept(self, addr, "SOCKS2");
	  self.tcp3 := Accept(self, addr, "SOCKS3");

	  (* We would like to do a "Rd.Close(self.rdA)" at this point, since
	     we aren't going to use that direction any more.  But that causes
	     SOCKS to drop the entire connection. *)
	  self.oldRdA := self.rdA;

	  TurnOffNoDelay(self.tcp1);
	  self.rdA := ConnRW.NewRd(self.tcp1);
	  IOWatchDog.AddRd(self.watchDog, self.rdA);
	  TurnOffNoDelay(self.tcp2);
	  self.wrB := ConnRW.NewWr(self.tcp2);
	  IOWatchDog.AddWr(self.watchDog, self.wrB);
	  TurnOffNoDelay(self.tcp3);
	  self.rdB := ConnRW.NewRd(self.tcp3);
	  IOWatchDog.AddRd(self.watchDog, self.rdB);
      | ConnectMode.Passive =>
	  Trace(self, "Establishing passive-mode data connection");
	  self.proto.putCmd(self.wrA, "PASV");
	  Wr.Flush(self.wrA);

	  ts := self.proto.getCmd(self.rdA);
	  ts.getFolded("PORT");
	  ep := ts.getEndpoint();
	  ts.getEnd();

	  TRY
	    self.tcp1 := TCPMisc.ConnectFrom(ep, self.config.localEndpoint);
	  EXCEPT IP.Error(list) =>
	    RAISE TransientFailure("Cannot connect to data port: "
	      & ErrMsg.StrError(list));
	  END;

	  TurnOffNoDelay(self.tcp1);
	  self.rdB := ConnRW.NewRd(self.tcp1);
	  IOWatchDog.AddRd(self.watchDog, self.rdB);
	  self.wrB := ConnRW.NewWr(self.tcp1);
	  IOWatchDog.AddWr(self.watchDog, self.wrB);
      | ConnectMode.Mux =>
	  Trace(self, "Establishing multiplexed-mode data connection");
	  self.proto.putCmd(self.wrA, "MUX");
	  Wr.Flush(self.wrA);
	  TRY
	    self.mux :=
	      ChannelMux.Open(self.rdA, self.wrA, chan0, active := TRUE);
	  EXCEPT IP.Error(l) =>
	    RAISE Error("ChannelMux.Open failed: " & ErrMsg.StrError(l));
	  END;

	  self.oldRdA := self.rdA;
	  self.oldWrA := self.wrA;

	  self.rdA := ConnRW.NewRd(chan0);
	  IOWatchDog.AddRd(self.watchDog, self.rdA);
	  self.wrA := ConnRW.NewWr(chan0);
	  IOWatchDog.AddWr(self.watchDog, self.wrA);

	  TRY
	    id := ChannelMux.Listen(self.mux);
	  EXCEPT IP.Error(l) =>
	    RAISE Error("ChannelMux.Listen failed: " & ErrMsg.StrError(l));
	  END;
	  self.proto.putCmd(self.wrA, "CHAN", Fmt.Int(id));
	  Wr.Flush(self.wrA);
	  TRY
	    chan1 := ChannelMux.Accept(self.mux, id);
	  EXCEPT IP.Error(l) =>
	    RAISE Error("ChannelMux.Accept failed: " & ErrMsg.StrError(l));
	  END;
	  self.rdB := ConnRW.NewRd(chan1);
	  IOWatchDog.AddRd(self.watchDog, self.rdB);
	  self.wrB := ConnRW.NewWr(chan1);
	  IOWatchDog.AddWr(self.watchDog, self.wrB);
      | ConnectMode.Default =>
	  (* It should have been set to a concrete value by now. *)
	  <* ASSERT FALSE *>	
      END;
    EXCEPT
    | TokScan.Error(msg) =>
	RAISE Error("Protocol error establishing data connection: " & msg);
    END;
  END EstablishDataConnection;

PROCEDURE CheckCollections(self: T): CARDINAL =
  VAR
    numValid: CARDINAL := 0;
    sfr: SupFileRec.T;
    link: TEXT;
  BEGIN
    FOR i := 0 TO self.config.collections.size()-1 DO
      sfr := self.config.collections.get(i);
      IF NOT SupMisc.IsDirectory(sfr.clientPrefix) THEN
	(* Skip this collection, and warn about it unless its prefix is
	   a symbolic link pointing to "SKIP". *)
	sfr.options := sfr.options + SupFileRec.Options{SupFileRec.Option.Skip};
	TRY
	  link := UnixMisc.ReadLink(sfr.clientPrefix)
	EXCEPT OSError.E =>
	  link := NIL;
	END;
	IF link = NIL OR NOT Text.Equal(link, "SKIP") THEN
	  Warn(self, "Nonexistent prefix \""
	    & sfr.clientPrefix & "\" for "
	    & sfr.collection & "/" & sfr.release);
	END;
      END;
      IF NOT SupFileRec.Option.Skip IN sfr.options THEN
	INC(numValid);
      END;
    END;
    RETURN numValid;
  END CheckCollections;

(*****************************************************************************)

TYPE
  Killer = SigHandler.T OBJECT
    thread: Thread.T;
    killedBySignal := -1;
  OVERRIDES
    apply := KillerApply;
  END;

PROCEDURE KillerApply(self: Killer; sig: INTEGER) =
  BEGIN
    self.killedBySignal := sig;
    Thread.Alert(self.thread);
  END KillerApply;

BEGIN
END FSClient.
