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

MODULE FSServer EXPORTS FSServer, FSServerRep;

IMPORT
  AccessRules, AtomList, AuthMD5, ChannelMux, ClassDB, ClientClass,
  CVProto, ErrMsg, ExecRec, FileAttr, FileRd, Fmt, FS, Glob, GlobTree,
  IOWatchDog, IP, Logger, OSError, OSErrorPosix, Passwd, Pathname,
  Process, ProcTitle, RCSComp, RCSKeyword, Rd, Reaper, RTProcess, SigHandler,
  StreamRd, StreamWr, SupFileRec, SupFileRecSeq, SupMisc, TCPMisc,
  Text, Thread, Time, TokScan, TreeComp, Uerror, Usignal,
  Utypes, Version, WatchDog, Wr;

IMPORT ConnRW;
IMPORT Cerrno;
IMPORT TCP;

REVEAL
  T = Rep BRANDED OBJECT
  OVERRIDES
    init := Init;
    log := MasterLog;
    run := Run;
  END;

CONST
  AccessFile = "cvsupd.access";
  ClassFile = "cvsupd.class";
  HaltFile = "cvsupd.HALT";
  IdleTimeout = 15.0d0 * 60.0d0;
  MaxAccessFileAge = 3.0d0 * 60.0d0 * 60.0d0;
  MaxClassFileAge = 3.0d0 * 60.0d0 * 60.0d0;
  PasswdFile = "cvsupd.passwd";
  S1GUrl = "http://www.cvsup.org/s1g/";

VAR (* CONST *)
  EagainAtom := OSErrorPosix.ErrnoAtom(Uerror.EAGAIN);

VAR
  TheT: T := NIL;  (* The one and only server object. *)
  Pid: Process.ID;
(* Initialize "Pid" as late as possible, i.e., in the "Run" procedure
   rather than in "Init".  The pid can change between "Init" and "Run",
   if the caller forks to become a daemon. *)

EXCEPTION ForkFailed(AtomList.T);

PROCEDURE Init(self: T;
               config: Configuration): T
  RAISES {Error} =
  BEGIN
    IF TheT # NIL THEN
      ErrMsg.Fatal("Cannot create multiple FSServer.Ts");
    END;
    TheT := self;
    self.config := config;

    self.numSlots := MAX(config.maxChildren, 1);
    self.childPids := NEW(REF ARRAY OF Process.ID, self.numSlots);
    self.childAddrs := NEW(REF ARRAY OF IP.Address, self.numSlots);
    FOR i := 0 TO self.numSlots-1 DO
      self.childPids[i] := Process.NullID;
      self.childAddrs[i] := IP.NullAddress;
    END;

    IF config.localEndpoint.port = IP.NullPort THEN
      config.localEndpoint.port := SupMisc.Port;
    END;
    IF config.serverBase = NIL THEN
      config.serverBase := SupMisc.DefaultServerBase;
    END;
    IF config.serverCollDirs = NIL THEN
      config.serverCollDirs := SupMisc.DefaultServerCollDir;
    END;
    IF config.hiDataPort = IP.NullPort THEN
      config.hiDataPort := config.loDataPort;
    END;
    IF config.compLevel = -1 THEN
      config.compLevel := SupMisc.DefaultCompression;
    END;

    IF NOT SupMisc.IsDirectory(config.serverBase) THEN
      RAISE Error("Base directory \"" & config.serverBase
	& "\" does not exist");
    END;

    TRY
      self.connector := TCP.NewConnector(config.localEndpoint);
    EXCEPT IP.Error(list) =>
      RAISE Error("Listen failed: " & ErrMsg.StrError(list));
    END;

    RETURN self;
  END Init;

PROCEDURE Run(self: T)
  RAISES {Error, Thread.Alerted} =
  VAR
    tcp0: TCP.T;
    id: CARDINAL := 0;
    childCl: SubProcess;
    accessPath := SupMisc.ResolvePath(self.config.serverBase, AccessFile);
    classDBPath := SupMisc.ResolvePath(self.config.serverBase, ClassFile);
    classDB: ClientClass.DB;
    accessRules: AccessRules.T;
    clientEndpoint: IP.Endpoint;
    errno: INTEGER;
  BEGIN
    self.startTime := Time.Now();
    Pid := Process.GetMyID();

    (* Install a signal handler to reap child processes that have finished. *)
    SigHandler.Register(Usignal.SIGCHLD,
      NEW(ChildHandler, server := self, apply := Reap));

    self.log("CVSup server started");
    self.log("Software version: " & Version.Name);
    self.log("Protocol version: " & Fmt.Int(CVProto.Current.major) &
      "." & Fmt.Int(CVProto.Current.minor));
    self.log("Ready to service requests");

    LOOP
      LOOP
	TRY
	  tcp0 := TCPMisc.AcceptFrom(self.connector, clientEndpoint);
	  EXIT;
	EXCEPT IP.Error(list) =>
	  IF ErrMsg.GetErrno(list, errno) THEN
        IF   (errno = Uerror.ENFILE)
          OR (errno = Uerror.ECONNABORTED)
          OR (errno = Uerror.ECONNRESET)
          OR (errno = Uerror.ENOBUFS) THEN
		(* Warn and discard the aborted connection. *)
		self.log("Accept failed: " & ErrMsg.StrError(list),
		  Logger.Priority.Warning);
	    ELSE
	      RAISE Error("Accept failed: " & ErrMsg.StrError(list));
	    END;
	  ELSE
	    RAISE Error("Accept failed: " & ErrMsg.StrError(list));
	  END;
	END;
      END;
      TRY
	TRY
	  (* FIXME - If there are DNS problems, this can hold us up for
	     a long time. *)
	  accessRules := AccessRules.Get(accessPath, MaxAccessFileAge,
	    self.config.logger);
	EXCEPT Rd.Failure(list) =>
          RAISE Error("Read failure on \"" & accessPath & "\": " & 
            ErrMsg.StrError(list));
        END;
	TRY
	  classDB := ClassDB.Get(classDBPath, MaxClassFileAge,
	    self.config.logger);
        EXCEPT Rd.Failure(list) =>
          RAISE Error("Read failure on \"" & classDBPath & "\": " &
            ErrMsg.StrError(list));
        END;

	TRY
	  IF self.config.maxChildren < 0 THEN
	    (* We are running in foreground.  We serve one client without
	       forking, then quit. *)
	    INC(self.numChildren);
	    self.childPids[0] := Process.GetMyID();
	    self.childAddrs[0] := clientEndpoint.addr;
	    childCl := NEW(SubProcess).init(self, self.numChildren, id,
	      tcp0, clientEndpoint.addr, accessRules, classDB);
	    EVAL childCl.apply();
	    EXIT;
	  ELSE  (* Normal daemon mode. *)
	    VAR
	      childPid: Utypes.pid_t;
	      isChild := FALSE;
	      slot := -1;
	    BEGIN
	      SigHandler.Block();
	      TRY
		(* Find a vacant slot for this child. *)
		FOR i := 0 TO self.numSlots-1 DO
		  IF self.childPids[i] = Process.NullID THEN
		    slot := i;
		    EXIT;
		  END;
		END;

		childPid := Fork();
		INC(self.numChildren);

		IF childPid = 0 THEN
		  isChild := TRUE;
		  Pid := Process.GetMyID();
		  childPid := Pid;
		END;
		IF slot >= 0 THEN
		  self.childPids[slot] := childPid;
		  self.childAddrs[slot] := clientEndpoint.addr;
		END;
	      FINALLY
		IF isChild THEN
		  SigHandler.ShutDown();
		ELSE
		  SigHandler.Unblock();
		END;
	      END;

	      IF isChild THEN
		TCP.CloseConnector(self.connector);
		childCl := NEW(SubProcess).init(self, self.numChildren, id,
		  tcp0, clientEndpoint.addr, accessRules, classDB);
		EVAL childCl.apply();
		Process.Exit(0);
	      END;

	      INC(id);
	    END;
	  END;
	EXCEPT
	| ForkFailed(l) =>
	    self.log("Could not fork: " & ErrMsg.StrError(l));
	END;
      FINALLY
	TCP.Close(tcp0);
      END;
    END;
  END Run;

PROCEDURE MasterLog(self: T; msg: TEXT; priority := Logger.Priority.Notice) =
  BEGIN
    IF self.config.logger # NIL THEN
      Logger.Put(self.config.logger, priority, msg);
    END;
  END MasterLog;

(*****************************************************************************)

TYPE
  SubProcess = OBJECT
      parent: T;
      numChildren: CARDINAL;
      id: CARDINAL;
      clientAddr: IP.Address;
      accessRules: AccessRules.T;
      tcp0, tcp1, tcp2, tcp3: TCP.T := NIL;
      mux: ChannelMux.T := NIL;
      rdA, rdB, oldRdA: StreamRd.T := NIL;
      wrA, wrB, oldWrA: StreamWr.T := NIL;
      collections: SupFileRecSeq.T := NIL;
      reaper: Reaper.T := NIL;
      idleKiller: IdleKiller := NIL;
      proto: CVProto.T := NIL;
      claimedUser := "?";       (* User name from USER command. *)
      addrHost := "?";          (* Host name from reverse DNS, or IP addr. *)
      clientVersion := ".";
      clientClass: ClientClass.T := NIL;
      statsMsg := "";
      doLogging := FALSE;
      authRequired := FALSE;
      passwdDB: Passwd.DB := NIL;
      passwd: Passwd.T := NIL;	(* Password entry from auth, or NIL *)
      classDB: ClientClass.DB := NIL;
    METHODS
      init(parent: T;
	   numChildren: CARDINAL;
	   id: CARDINAL;
	   tcp0: TCP.T;
	   clientAddr: IP.Address;
	   accessRules: AccessRules.T;
           classDB: ClientClass.DB): SubProcess := SubProcessInit;
      apply(): REFANY := ServeOne;
      log(msg: TEXT;
	  flag := '=';
	  priority := Logger.Priority.Notice) := SubProcessLog;
    END;

PROCEDURE ServeOne(self: SubProcess): REFANY =
  VAR
    treeComp: TreeComp.T;
    treeCompThread: Thread.T;
    rcsComp: RCSComp.T;
    rcsCompThread: Thread.T;
    thread: Thread.T;
    retVal: REFANY;
    threadMsg: TEXT;
  BEGIN
    TRY
      TRY
	TurnOffNoDelay(self.tcp0);
	self.rdA := ConnRW.NewRd(self.tcp0);
	self.wrA := ConnRW.NewWr(self.tcp0);

	self.idleKiller := NEW(IdleKiller).init(
	  sub := self, timeout := IdleTimeout);
	IOWatchDog.AddRd(self.idleKiller, self.rdA);
	IOWatchDog.AddWr(self.idleKiller, self.wrA);

	CheckShutdown(self);
	TRY
	  self.passwdDB := Passwd.Open(SupMisc.ResolvePath(
	    self.parent.config.serverBase, PasswdFile));
	EXCEPT
	| OSError.E => (* Ignore *)
	| Passwd.Error(msg) =>
	    self.parent.log(msg, Logger.Priority.Warning);
	END;
	TRY
	  CheckTooBusy(self);
	  ShakeHands(self);
	  Authorize(self);
	  SetClientClass(self);
          (* CheckClassAccessLimits(self); FIXME: not yet *)
	FINALLY
	  IF self.passwdDB # NIL THEN
	    TRY
	      Passwd.Close(self.passwdDB);
	    EXCEPT ELSE END;
	    self.passwdDB := NIL;
	  END;
	END;
	ExchangeAttributeInfo(self);
	ExchangeCollectionInfo(self);
	EstablishDataConnection(self);

	FindScanFiles(self);

	self.reaper := NEW(Reaper.T).init();

	treeComp := NEW(TreeComp.T).init(
	  proto := self.proto,
	  rd := self.rdA,
	  wr := self.wrA,
	  collections := self.collections,
          clientClass := self.clientClass,
	  compLevel := self.parent.config.compLevel,
	  reaper := self.reaper,
	  logger := self.parent.config.logger);
	rcsComp := NEW(RCSComp.T).init(
	  proto := self.proto,
	  rd := self.rdB,
	  wr := self.wrB,
	  collections := self.collections,
          clientClass := self.clientClass,
	  compLevel := self.parent.config.compLevel,
	  reaper := self.reaper,
	  logger := self.parent.config.logger);

	treeCompThread := Reaper.Fork(self.reaper, treeComp);
	rcsCompThread := Reaper.Fork(self.reaper, rcsComp);

	(* Wait until all the subthreads have finished, or until an
	   error is returned from one of them. *)
	retVal := NIL;
	WHILE Reaper.JoinNext(self.reaper, thread, retVal) AND retVal = NIL DO
	  (* Nothing *)
	END;

	GetStats(self);
	IF retVal # NIL THEN  (* There was an error. *)
	  threadMsg := retVal;
	  IF thread = treeCompThread THEN
	    Die(self, "TreeComp failed: " & threadMsg);
	  ELSE
	    <* ASSERT thread = rcsCompThread *>
	    Die(self, "RCSComp failed: " & threadMsg);
	  END;
	ELSE
	  IF NOT Rd.EOF(self.rdA) THEN
	    RAISE Error(
	      "TreeComp protocol error: Expected EOF, didn't get it");
	  END;
	  IF NOT Rd.EOF(self.rdB) THEN
	    RAISE
	      Error("RCSComp protocol error: Expected EOF, didn't get it");
	  END;
	  Wr.Close(self.wrA);
	  self.wrA := NIL;
	  Wr.Close(self.wrB);
	  self.wrB := NIL;
	  IF self.oldRdA # NIL THEN
	    IF NOT Rd.EOF(self.oldRdA) THEN
	      RAISE Error(
		"ChannelMux protocol error: Expected EOF, didn't get it");
	    END;
	  END;
	  self.log(self.statsMsg & "Finished successfully", '-');
	END;
      FINALLY
	ShutdownConnections(self);
      END;
    EXCEPT
    | Error(msg) =>
	IF msg # NIL AND NOT Text.Empty(msg) THEN
	  self.log(self.statsMsg & msg, '-');
	END;
    | Rd.EndOfFile =>
	self.log(self.statsMsg & "Premature EOF from client", '-');
    | Rd.Failure(list) =>
	self.log(self.statsMsg & "Network read failure: " &
	  ErrMsg.StrError(list), '-');
    | Thread.Alerted =>
	self.log(self.statsMsg & "Interrupted");
    | Wr.Failure(list) =>
	self.log(self.statsMsg & "Network write failure: " &
	  ErrMsg.StrError(list), '-');
    END;
    RETURN NIL;
  END ServeOne;

PROCEDURE SubProcessInit(self: SubProcess;
                         parent: T;
			 numChildren: CARDINAL;
			 id: CARDINAL;
			 tcp0: TCP.T;
			 clientAddr: IP.Address;
			 accessRules: AccessRules.T;
                         classDB: ClientClass.DB): SubProcess =
  BEGIN
    self.parent := parent;
    self.numChildren := numChildren;
    self.id := id;
    self.tcp0 := tcp0;
    self.clientAddr := clientAddr;
    self.accessRules := accessRules;
    self.classDB := classDB;
    RETURN self;
  END SubProcessInit;

PROCEDURE SubProcessLog(self: SubProcess;
                        msg: TEXT;
			flag := '=';
			priority := Logger.Priority.Notice) =
  BEGIN
    IF self.doLogging AND self.parent.config.logger # NIL THEN
      Logger.Put(self.parent.config.logger, priority,
	Text.FromChar(flag) & Fmt.Int(self.id) & " " & msg);
    END;
  END SubProcessLog;

PROCEDURE AllLingerOff(self: SubProcess) =
  BEGIN
    IF self.tcp0 # NIL THEN
      TRY TurnOffLinger(self.tcp0) EXCEPT Error => (* Ignore *) END;
    END;
    IF self.tcp1 # NIL THEN
      TRY TurnOffLinger(self.tcp1) EXCEPT Error => (* Ignore *) END;
    END;
    IF self.tcp2 # NIL THEN
      TRY TurnOffLinger(self.tcp2) EXCEPT Error => (* Ignore *) END;
    END;
    IF self.tcp3 # NIL THEN
      TRY TurnOffLinger(self.tcp3) EXCEPT Error => (* Ignore *) END;
    END;
  END AllLingerOff;

PROCEDURE Die(self: SubProcess;
              msg: TEXT) =
  BEGIN
    (* Alert the subthreads in an effort to keep them from holding the
       readers and writers locked.  Otherwise, the "GetStats" call could
       block. *)
    IF self.reaper # NIL THEN
      Reaper.AlertAll(self.reaper);
    END;
    AllLingerOff(self);
    GetStats(self);
    self.log(self.statsMsg & msg, '-');
    Process.Exit(1);
  END Die;

PROCEDURE GetStats(self: SubProcess) =
  VAR
    bytesIn, bytesOut: LONGREAL;
    sfr: SupFileRec.T;
    msg: TEXT;
  BEGIN
    IF Text.Empty(self.statsMsg) THEN  (* Not already done. *)
      (* Stats by collection. *)
      IF self.collections # NIL THEN
	FOR i := 0 TO self.collections.size()-1 DO
	  sfr := self.collections.get(i);
	  LOCK sfr DO
	    bytesIn := sfr.bytesIn;
	    bytesOut := sfr.bytesOut;
	  END;
	  IF bytesIn # 0.0d0 OR bytesOut # 0.0d0 THEN
	    msg := "[" &
	      Fmt.LongReal(bytesIn / 1024.0d0, Fmt.Style.Fix, 0) & "Kin+" &
	      Fmt.LongReal(bytesOut / 1024.0d0, Fmt.Style.Fix, 0) & "Kout]";
	    msg := msg & " " & sfr.collection & "/" & sfr.release;

(**** checkoutDate and checkoutTag aren't filled in, unfortunately.

	    IF SupFileRec.Option.CheckoutMode IN sfr.options THEN
	      IF NOT Text.Equal(sfr.checkoutTag, ".")
	      OR Text.Equal(sfr.checkoutDate, ".") THEN
		msg := msg & " tag=" & sfr.checkoutTag;
	      END;
	      IF NOT Text.Equal(sfr.checkoutDate, ".") THEN
		msg := msg & " date=" & sfr.checkoutDate;
	      END;
	    END;
****)
	    self.log(msg, priority := Logger.Priority.Info);
	  END;
	END;
      END;

      (* Totals. *)
      bytesIn := 0.0d0;
      bytesOut := 0.0d0;
      IF self.rdA # NIL THEN
	bytesIn := bytesIn + StreamRd.ByteCount(self.rdA);
      END;
      IF self.rdB # NIL THEN
	bytesIn := bytesIn + StreamRd.ByteCount(self.rdB);
      END;
      IF self.wrA # NIL THEN
	bytesOut := bytesOut + StreamWr.ByteCount(self.wrA);
      END;
      IF self.wrB # NIL THEN
	bytesOut := bytesOut + StreamWr.ByteCount(self.wrB);
      END;
      self.statsMsg := "[" &
	Fmt.LongReal(bytesIn / 1024.0d0, Fmt.Style.Fix, 0) & "Kin+" &
	Fmt.LongReal(bytesOut / 1024.0d0, Fmt.Style.Fix, 0) & "Kout] ";
    END;
  END GetStats;

PROCEDURE ShutdownConnections(self: SubProcess)
  RAISES {Thread.Alerted} =
(* Closes all the network connections, being careful to get it all done
   even if exceptions occur along the way. *)
  BEGIN
    IF self.mux # NIL THEN ChannelMux.Close(self.mux) END;
    AllLingerOff(self);
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
	TRY
	  IF self.wrB # NIL THEN
	    TRY Wr.Close(self.wrB) EXCEPT Wr.Failure => (* Ignore *) END;
	  END;
	FINALLY
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
		IF self.tcp0 # NIL THEN TCP.Close(self.tcp0) END;
		IF self.tcp1 # NIL THEN TCP.Close(self.tcp1) END;
		IF self.tcp2 # NIL THEN TCP.Close(self.tcp2) END;
		IF self.tcp3 # NIL THEN TCP.Close(self.tcp3) END;
	      END;
	    END;
	  END;
	END;
      END;
    END;
  END ShutdownConnections;

(*****************************************************************************)

PROCEDURE Authorize(self: SubProcess)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted, Wr.Failure} =
(* The exceptions Rd.EndOfFile, Rd.Failure, and Wr.Failure are raised
   only for the network connection. *)
  VAR
    ts: TokScan.T;
    claimedHost: TEXT;
    loginMsg: TEXT;
    verMsg: TEXT;
  BEGIN
    TRY
      ts := self.proto.getCmd(self.rdA);
      ts.getFolded("USER");
      self.claimedUser := ts.getToken("user ID");
      (* Early clients didn't send the host name, so we have to be prepared
	 for that. *)
      IF NOT ts.next(claimedHost) THEN claimedHost := NIL END;
      self.addrHost := GetHostName(self.clientAddr);

      IF self.proto.v.hasMD5Auth THEN
	DoMD5Auth(self);
      ELSE
	self.proto.putCmd(self.wrA, "OK");
	Wr.Flush(self.wrA);
      END;

      (* If we reach this point, the user is going to get service. *)
      loginMsg := self.claimedUser & "@" & self.addrHost;
      IF self.passwd # NIL THEN  (* We have an authenticated client. *)
	loginMsg := loginMsg & " <" & self.passwd.client & ">";
      ELSIF claimedHost # NIL
      AND NOT TokScan.EqualFolded(claimedHost, self.addrHost) THEN
	loginMsg := loginMsg & " (" & claimedHost & ")";
      END;
      verMsg := Fmt.Int(self.proto.major) & "." & Fmt.Int(self.proto.minor);
      IF self.proto.v.exchangesVersions THEN
	verMsg := self.clientVersion & "/" & verMsg;
      END;
      loginMsg := loginMsg & " [" & verMsg & "]";
      self.doLogging := TRUE;
      self.log(loginMsg, '+');
      IF TreeComp.traceLevel = 0 AND RCSComp.traceLevel = 0 THEN
        (* my m3gdb always crashes in setproctitle, so I don't do it
           if the -d or -t option are given (debug/trace) *)
        ProcTitle.Set(loginMsg);
      END;
    EXCEPT TokScan.Error(msg) =>
      RAISE Error("Protocol error authorizing user: " & msg);
    END;
  END Authorize;

PROCEDURE DoMD5Auth(self: SubProcess)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted, TokScan.Error,
    Wr.Failure} =
  VAR
    ts: TokScan.T;
    realm := ".";
    serverChallenge := ".";
    serverResponse := ".";
    sharedSecret := "*";
    client: TEXT;
    clientChallenge: TEXT;
    clientResponse: TEXT;
    errmsg: TEXT;
  BEGIN
    (* Since "self.doLogging" is still "FALSE" at this point, if we raise
       an exception it won't get logged.  So we log the important ones
       ourselves.  We log them as if they came from the master server,
       since an error at this point means the client session will not
       be established. *)
    TRY
      IF self.passwdDB # NIL THEN
	realm := Passwd.GetRealm(self.passwdDB);
	IF self.authRequired THEN
	  serverChallenge := AuthMD5.GenChallenge(self.clientAddr,
	    Passwd.GetPrivateKey(self.passwdDB));
	END;
      END;
      self.proto.putCmd(self.wrA, "AUTHMD5", realm, serverChallenge);
      Wr.Flush(self.wrA);
      ts := self.proto.getCmd(self.rdA);
      ts.getFolded("AUTHMD5");
      client := ts.getToken("client ID");
      clientResponse := ts.getToken("client auth response");
      clientChallenge := ts.getToken("client auth challenge");
      ts.getEnd("end of AUTHMD5 command");
      IF NOT Text.Equal(client, ".") AND self.passwdDB # NIL THEN
	self.passwd :=
	  Passwd.Lookup(self.passwdDB, client, self.parent.config.logger);
	IF self.passwd # NIL THEN
	  sharedSecret := self.passwd.sharedSecret;
	END;
      END;
      IF self.authRequired THEN
	IF Text.Equal(sharedSecret, "*")
	OR NOT AuthMD5.CheckResponse(clientResponse, serverChallenge,
	  sharedSecret)
	THEN
	  self.proto.putCmd(self.wrA, "!", "Authentication failed");
	  Wr.Flush(self.wrA);
	  errmsg := "Authentication failed: " &
	    self.claimedUser & "@" & self.addrHost & " <" & client & ">";
	  self.parent.log(errmsg, Logger.Priority.Notice);
	  RAISE Error(errmsg);
	END;
      END;
      IF NOT Text.Equal(clientChallenge, ".")
      AND NOT Text.Equal(sharedSecret, "*") THEN
	serverResponse := AuthMD5.GenResponse(clientChallenge, sharedSecret);
      END;
      self.proto.putCmd(self.wrA, "OK", serverResponse);
      Wr.Flush(self.wrA);
    EXCEPT Passwd.Error(msg) =>
      self.parent.log(msg, Logger.Priority.Warning);
      RAISE Error(msg);
    END;
  END DoMD5Auth;

PROCEDURE CheckShutdown(self: SubProcess)
  RAISES {Error, Thread.Alerted, Wr.Failure} =
  VAR
    haltFile := SupMisc.ResolvePath(self.parent.config.serverBase, HaltFile);
  BEGIN
    TRY
      IF FS.Status(haltFile).modificationTime >= self.parent.startTime THEN
	SupMisc.PutCmd(self.wrA, "!",
	  "Server is going down for maintenance");
	Wr.Flush(self.wrA);
	RAISE Error("Connection rejected: shutting down");
      END;
    EXCEPT OSError.E => (* Not shutting down. *) END;
  END CheckShutdown;

PROCEDURE CheckTooBusy(self: SubProcess)
  RAISES {Error, Thread.Alerted, Wr.Failure} =
  VAR
    checkResult: AccessRules.CheckResult;
  BEGIN
    IF self.parent.config.maxChildren >= 0
    AND self.numChildren > self.parent.config.maxChildren THEN
      checkResult := AccessRules.CheckResult.TooMany;
    ELSE
      checkResult := AccessRules.Check(self.accessRules, self.clientAddr,
                                       self.parent.childAddrs^);
    END;
    CASE checkResult OF
    | AccessRules.CheckResult.OK =>
	(* Do nothing. *)
    | AccessRules.CheckResult.AuthRequired =>
	IF self.passwdDB = NIL THEN
	  SupMisc.PutCmd(self.wrA, "!", "Access denied");
	  Wr.Flush(self.wrA);
	  RAISE Error("Connection rejected: access denied");
	END;
	self.authRequired := TRUE;
    | AccessRules.CheckResult.TooMany =>
	SupMisc.PutCmd(self.wrA, "!",
	  "Access limit exceeded; try again later");
	Wr.Flush(self.wrA);
	RAISE Error("Connection rejected: access limit exceeded");
    | AccessRules.CheckResult.Denied =>
	SupMisc.PutCmd(self.wrA, "!", "Access denied");
	Wr.Flush(self.wrA);
	RAISE Error("Connection rejected: access denied");
    END;
  END CheckTooBusy;

PROCEDURE SetClientClass(self: SubProcess) =
  BEGIN
    self.clientClass := NIL;
    IF self.passwd # NIL AND NOT Text.Empty(self.passwd.class) THEN
      self.clientClass := self.classDB.getClass(self.passwd.class);
      (* XXX Log it if the class is not found *)
    END;
    IF self.clientClass = NIL THEN
      self.clientClass := self.classDB.getClass("default");
      <* ASSERT self.clientClass # NIL *>
    END;
  END SetClientClass;

PROCEDURE ShakeHands(self: SubProcess)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted, Wr.Failure} =
  VAR
    ts: TokScan.T;
    clientMajor, clientMinor: CVProto.VersionNumber;
  BEGIN
    TRY
      SupMisc.PutCmd(self.wrA, "OK",
	Fmt.Int(CVProto.Current.major),
	Fmt.Int(CVProto.Current.minor),
	Version.Name,
	"CVSup server ready");
      Wr.Flush(self.wrA);

      ts := TokScan.New(SupMisc.GetCmdLine(self.rdA));
      ts.getFolded("PROTO");
      clientMajor := ts.getInt("client protocol major version");
      clientMinor := ts.getInt("client protocol minor version");
      IF NOT ts.next(self.clientVersion) THEN self.clientVersion := "." END;

      TRY
	self.proto := CVProto.Resolve(clientMajor, clientMinor);
	(* Reject old clients that have the S1G bug.  It causes them to
	   detail every file, creating a heavy load on the server. *)
	IF CVProto.HasS1GBug(self.proto, self.clientVersion) THEN
	  RAISE CVProto.NotSupported;
	END;
      EXCEPT CVProto.NotSupported =>
	SupMisc.PutCmd(self.wrA, "!",
	  "See " & S1GUrl & " for upgrading information");
	Wr.Flush(self.wrA);
	RAISE Error("Client has S1G bug");
      END;
      
      IF self.authRequired AND NOT self.proto.v.hasMD5Auth THEN
	SupMisc.PutCmd(self.wrA, "!",
	  "Client does not support required authentication; upgrade to a "
	  & "newer version");
	Wr.Flush(self.wrA);
	RAISE Error("Client does not support required authentication");
      END;
      SupMisc.PutCmd(self.wrA, "PROTO",
	Fmt.Int(self.proto.major),
	Fmt.Int(self.proto.minor));
      Wr.Flush(self.wrA);
    EXCEPT
    | TokScan.Error(msg) =>
	RAISE Error("Protocol error shaking hands: " & msg);
    END;
  END ShakeHands;

(*****************************************************************************)

PROCEDURE EstablishDataConnection(self: SubProcess)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted, Wr.Failure} =
  VAR
    ts: TokScan.T;
    cmd: TEXT;
    dataPeer: IP.Endpoint;
    here: IP.Endpoint;
    conn: TCP.Connector;
    id: ChannelMux.ChannelID;
    chan0, chan1: ChannelMux.Channel;
  BEGIN
    TRY
      here := GetSockName(self.tcp0);

      ts := self.proto.getCmd(self.rdA);
      cmd := ts.getToken("command");
      IF TokScan.EqualFolded(cmd, "PORT") THEN  (* Active mode *)
	dataPeer := ts.getEndpoint();
	ts.getEnd();

	(* We connect back from a specific port, namely, one less than
	   the well-known port that listens for connections from clients.
	   That makes it easier for firewalls to be set up to work with
	   this package. *)
	DEC(here.port);

	self.tcp1 := ConnectFrom(to := dataPeer, from := here);

	TurnOffNoDelay(self.tcp1);
	self.rdB := ConnRW.NewRd(self.tcp1);
	IOWatchDog.AddRd(self.idleKiller, self.rdB);
	self.wrB := ConnRW.NewWr(self.tcp1);
	IOWatchDog.AddWr(self.idleKiller, self.wrB);
      ELSIF TokScan.EqualFolded(cmd, "SOCKS1") THEN  (* Socks mode *)
	dataPeer := ts.getEndpoint();
	ts.getEnd();

	(* We establish three separate connections back to the client.  Each
	   will be used unidirectionally.  That causes the SOCKS server to
	   fork a new process for each connection, working around the fact
	   that it uses nonblocking I/O calls.  We initiate each connection
	   from the same port, one less than our original listening port. *)
	DEC(here.port);

	self.tcp1 := ConnectFrom(to := dataPeer, from := here);

	ts := self.proto.getCmd(self.rdA);
	ts.getFolded("SOCKS2");
	dataPeer := ts.getEndpoint();
	ts.getEnd();
	self.tcp2 := ConnectFrom(to := dataPeer, from := here);

	ts := self.proto.getCmd(self.rdA);
	ts.getFolded("SOCKS3");
	dataPeer := ts.getEndpoint();
	ts.getEnd();
	self.tcp3 := ConnectFrom(to := dataPeer, from := here);

	(* We would like to do a "Wr.Close(self.wrA)" at this point, since
	   we aren't going to use that direction any more.  But that causes
	   SOCKS to drop the entire connection. *)
	self.oldWrA := self.wrA;

	TurnOffNoDelay(self.tcp1);
	self.wrA := ConnRW.NewWr(self.tcp1);
	IOWatchDog.AddWr(self.idleKiller, self.wrA);

	TurnOffNoDelay(self.tcp2);
	self.rdB := ConnRW.NewRd(self.tcp2);
	IOWatchDog.AddRd(self.idleKiller, self.rdB);

	TurnOffNoDelay(self.tcp3);
	self.wrB := ConnRW.NewWr(self.tcp3);
	IOWatchDog.AddWr(self.idleKiller, self.wrB);
      ELSIF TokScan.EqualFolded(cmd, "PASV") THEN  (* Passive mode *)
	conn := NewConnector(here.addr, self.parent.config.loDataPort,
	  self.parent.config.hiDataPort);
	TRY
	  self.tcp1 := Accept(self, conn, "PORT");
	FINALLY
	  TCP.CloseConnector(conn);
	END;

	TurnOffNoDelay(self.tcp1);
	self.rdB := ConnRW.NewRd(self.tcp1);
	IOWatchDog.AddRd(self.idleKiller, self.rdB);
	self.wrB := ConnRW.NewWr(self.tcp1);
	IOWatchDog.AddWr(self.idleKiller, self.wrB);
      ELSIF TokScan.EqualFolded(cmd, "MUX") THEN  (* Multiplexed mode. *)
	TRY
	  self.mux :=
	    ChannelMux.Open(self.rdA, self.wrA, chan0, active := FALSE);
	EXCEPT IP.Error(l) =>
	  RAISE Error("ChannelMux.Open failed: " & ErrMsg.StrError(l));
	END;

	self.oldRdA := self.rdA;
	self.oldWrA := self.wrA;

	self.rdA := ConnRW.NewRd(chan0);
	IOWatchDog.AddRd(self.idleKiller, self.rdA);
	self.wrA := ConnRW.NewWr(chan0);
	IOWatchDog.AddWr(self.idleKiller, self.wrA);  

	ts := self.proto.getCmd(self.rdA);
	ts.getFolded("CHAN");
	id := ts.getInt("data channel ID");
	ts.getEnd();
	TRY
	  chan1 := ChannelMux.Connect(self.mux, id);
	EXCEPT IP.Error(l) =>
	  RAISE Error("ChannelMux.Connect failed: " & ErrMsg.StrError(l));
	END;
	self.rdB := ConnRW.NewRd(chan1);
	IOWatchDog.AddRd(self.idleKiller, self.rdB);
	self.wrB := ConnRW.NewWr(chan1);
	IOWatchDog.AddWr(self.idleKiller, self.wrB);  
      ELSE
	RAISE TokScan.Error("Invalid command \"" & cmd & "\"");
      END;
    EXCEPT
    | TokScan.Error(msg) =>
	RAISE Error("Protocol error establishing data connection: " & msg);
    END;
  END EstablishDataConnection;

(*****************************************************************************)

PROCEDURE ExchangeAttributeInfo(self: SubProcess)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted, Wr.Failure} =
(* The exceptions Rd.EndOfFile, Rd.Failure, and Wr.Failure are raised
   only for the network connection. *)
  VAR
    ts: TokScan.T;
    count: INTEGER;  
    tok: TEXT;
  BEGIN
    IF self.proto.v.hasFileAttrs THEN
      TRY
	self.proto.v.attrSupport := FileAttr.Supported;
	ts := self.proto.getCmd(self.rdA);
	ts.getFolded("ATTR");
	count := ts.getInt("number of file types");
	ts.getEnd("end of \"ATTR\" command");

	FOR i := 0 TO count-1 DO
	  ts := self.proto.getCmd(self.rdA);
	  tok := ts.getToken("attrTypes");
	  ts.getEnd("end of attrTypes");
	  IF i < NUMBER(self.proto.v.attrSupport) THEN
	    WITH as = self.proto.v.attrSupport[VAL(i, FileAttr.FileType)] DO
	      as := as * FileAttr.DecodeAttrTypes(tok);
	    END;
	  END;
	END;
	ts := self.proto.getCmd(self.rdA);
	ts.getLiteral(".");
	ts.getEnd("end of \".\"");

	FOR i := count TO NUMBER(self.proto.v.attrSupport)-1 DO
	  self.proto.v.attrSupport[VAL(i, FileAttr.FileType)] :=
	    FileAttr.AttrTypes{};
	END;

	count := MIN(count, NUMBER(self.proto.v.attrSupport));
	self.proto.putCmd(self.wrA, "ATTR", Fmt.Int(count));
	FOR i := 0 TO count-1 DO
	  WITH as = self.proto.v.attrSupport[VAL(i, FileAttr.FileType)] DO
	    Wr.PutText(self.wrA, FileAttr.EncodeAttrTypes(as) & "\n");
	  END;
	END;
	self.proto.putCmd(self.wrA, ".");
	Wr.Flush(self.wrA);
      EXCEPT TokScan.Error(msg) =>
	RAISE Error("Protocol error negotiating attribute support: " & msg);
      END;
    ELSE
      self.proto.v.attrSupport := FileAttr.Historical;
    END;
  END ExchangeAttributeInfo;

(*****************************************************************************)

TYPE 
  CollDirsPredClosure = SupMisc.TextPredicateClosure OBJECT
    cl: ClientClass.T;
  METHODS
  OVERRIDES
    matches := CollDirAllowed;
  END;

PROCEDURE CollDirAllowed(self: CollDirsPredClosure; t: TEXT): BOOLEAN =
  BEGIN
    RETURN self.cl.inAllowedCollectionDirs(t);
  END CollDirAllowed;

PROCEDURE ExchangeCollectionInfo(self: SubProcess)
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted, Wr.Failure} =
(* The exceptions Rd.EndOfFile, Rd.Failure, and Wr.Failure are raised
   only for the network connection. *)
  VAR
    ts: TokScan.T;
    cmd: TEXT;
    sfr: SupFileRec.T;
    textPredCl := NEW(CollDirsPredClosure, cl := self.clientClass);
  BEGIN
    self.collections := NEW(SupFileRecSeq.T).init();
    TRY
      (* Read all the supfile information from the client. *)
      LOOP
	ts := self.proto.getCmd(self.rdA);
	cmd := ts.getToken();
	IF Text.Equal(cmd, ".") THEN EXIT END;
	IF NOT TokScan.EqualFolded(cmd, "COLL") THEN
	  RAISE TokScan.Error("\"COLL\" expected");
	END;
	sfr := NEW(SupFileRec.T).init();
	sfr.collection := ts.getToken("collection");
	sfr.release := ts.getToken("release");
	IF self.proto.v.clientSendsUmask THEN
	  sfr.umask := ts.getInt("umask", radix := 8);
	ELSE
	  sfr.umask := 0;
	END;
	sfr.options := SupFileRec.DecodeOptions(ts.getToken("options"));
	LOOP
	  ts := self.proto.getCmd(self.rdA);
	  cmd := ts.getToken("command");
	  IF Text.Equal(cmd, ".") THEN EXIT END;
	  IF TokScan.EqualFolded(cmd, "REF") THEN
	    sfr.refusals.addhi(ts.getToken("refusal pattern"));
	  ELSIF TokScan.EqualFolded(cmd, "ACC") THEN
	    sfr.accepts.addhi(ts.getToken("acceptance pattern"));
	  ELSE
	    RAISE TokScan.Error(
	      "Invalid command while exchanging collection info");
	  END;
	END;
	sfr.serverBase := self.parent.config.serverBase;
	sfr.serverCollDirs := 
             SupMisc.FilterPathList(self.parent.config.serverCollDirs,
                                    textPredCl);
        IF TreeComp.traceLevel > 0 THEN
          self.log("collectionDirs: " & self.parent.config.serverCollDirs);
          self.log("filteredCollectionDirs: " & sfr.serverCollDirs);
        END;
	(* Set up a mask of file attribes that we don't want to sync
	   to the client. *)
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

        (* clear some options for restricted access to CVS repositories *)
        IF self.clientClass.collectionIsPartiallyHidden(sfr.collection) THEN
          self.log("clearing RCS options for partially hidden collection " &
            sfr.collection);
          sfr.options := sfr.options -
            SupFileRec.Options{SupFileRec.Option.CheckRCS,
                               SupFileRec.Option.NoRCS,
                               SupFileRec.Option.StrictCheckRCS};
        END;
        IF self.parent.config.detailAllRCSFiles THEN
          sfr.options := sfr.options + 
              SupFileRec.Options{SupFileRec.Option.DetailAllRCSFiles};
          self.log("detailing all RCS files of collection " &
            sfr.collection);
        END;
	self.collections.addhi(sfr);
      END;

      (* Send back the filtering details, and construct our own filter. *)
      FOR i := 0 TO self.collections.size()-1 DO
	SendCollectionInfo(self, self.collections.get(i));
      END;
      self.proto.putCmd(self.wrA, ".");
      Wr.Flush(self.wrA);
    EXCEPT TokScan.Error(msg) =>
      RAISE Error("Protocol error exchanging collection info: " & msg);
    END;
  END ExchangeCollectionInfo;

PROCEDURE SendCollectionInfo(self: SubProcess;
                             sfr: SupFileRec.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    releasesPath: Pathname.T;
    collCommandSent := FALSE;
  PROCEDURE PutCollCommand() RAISES {Thread.Alerted, Wr.Failure} =
    (* Yes, it is silly to use a nested procedure for this.  But
       it seems to be necessary in order to work around a compiler
       bug when optimization is turned on.  When I tried to use a
       simple flag, outside a nested procedure, the compiler
       apparently did some invalid constant lifting, and all use
       of the flag in the emitted code disappeared.  So please
       leave this the way it is.  I was using the SRC M3 compiler
       version 3.6, with the gcc-2.7.2 code generator when this
       problem occurred.  John Polstra <jdp@polstra.com> 7 January
       1997 *)
    BEGIN
      IF NOT collCommandSent THEN
	self.proto.putCmd(self.wrA, "COLL",
	  sfr.collection,
	  sfr.release,
	  SupFileRec.EncodeOptions(sfr.options));
	collCommandSent := TRUE;
      END;
    END PutCollCommand;
  BEGIN
    TRY
      TRY
	(* Validate the collection name.  This is a security measure.  The
	   collection name is used to form a pathname to the configuration
	   files, and we don't want any funny business involving ".." and
	   such things. *)
	IF NOT Pathname.Valid(sfr.collection)
	OR Text.Empty(sfr.collection)
	OR Text.FindChar(sfr.collection, '/') >= 0
	OR Text.FindChar(sfr.collection, '\\') >= 0
	OR Text.Equal(sfr.collection, Pathname.Current)
	OR Text.Equal(sfr.collection, Pathname.Parent) THEN
	  RAISE Error("Invalid collection \"" & sfr.collection & "\"");
	END;
	(* Parse the "releases" file. *)
	releasesPath := SupMisc.FindFile(sfr.serverBase, sfr.serverCollDirs,
	    SupMisc.CatPath(sfr.collection, "releases"));
	IF releasesPath = NIL OR NOT 
          (* we pretend not to know about collections that are hidden to
             specific clients here *)
          self.clientClass.inAllowedCollections(sfr.collection) THEN
	  RAISE Error("Unknown collection \"" & sfr.collection & "\"");
	END;

	(* Skip the collection if it is partially hidden and the client
	   has requested checkout mode.  We don't support checkout mode
	   for partially-hidden collections yet. *)
	IF SupFileRec.Option.CheckoutMode IN sfr.options AND
	self.clientClass.collectionIsPartiallyHidden(sfr.collection) THEN
	  RAISE Error("Checkout mode not supported for partially-hidden" &
	    " collection \"" & sfr.collection & "\"");
	END;

	(* Process the "releases" file. *)
	ParseReleasesFile(self, sfr, releasesPath);

	PutCollCommand();
	self.proto.putCmd(self.wrA, "PRFX", sfr.keywordPrefix);

	(* Process the "CVSROOT/options" file, if any. *)
	SendOptionsInfo(self, sfr);

	(* Process the "list" file. *)
	SendListInfo(self, sfr);
      EXCEPT Error(msg) =>  (* Tell the client about the error. *)
	sfr.options := sfr.options +
	  SupFileRec.Options{SupFileRec.Option.Skip};
	PutCollCommand();
	self.proto.putCmd(self.wrA, "!", msg);
	self.log(msg);
      END;
    FINALLY
      self.proto.putCmd(self.wrA, ".");
    END;
  END SendCollectionInfo;

PROCEDURE ParseReleasesFile(<*UNUSED*> self: SubProcess;
                                       sfr: SupFileRec.T;
			               path: Pathname.T)
  RAISES {Error, Thread.Alerted} =
  VAR
    rd: Rd.T;
    foundRelease: BOOLEAN;
    rel, line, field, name, value: TEXT;
    ts, ts2: TokScan.T;
  BEGIN
    TRY
      TRY
	rd := FileRd.Open(path);
      EXCEPT OSError.E =>  (* FIXME - Check to make sure it's ENOENT. *)
	RAISE Error("Unknown collection \"" & sfr.collection & "\"");
      END;
      TRY
	foundRelease := FALSE;
	sfr.serverPrefix := NIL;
	sfr.serverListFile := NIL;
	LOOP
	  TRY line := Rd.GetLine(rd) EXCEPT Rd.EndOfFile => EXIT END;
	  ts := TokScan.New(line);
	  rel := ts.getToken("release name");
	  IF Text.Equal(rel, sfr.release) THEN
	    foundRelease := TRUE;
	    WHILE ts.next(field) DO
	      IF Text.FindChar(field, '=') >= 0 THEN
		ts2 := TokScan.New(field, SET OF CHAR{'='});
		name := ts2.getToken("field name");
		value := ts2.getToken("field value");
		IF TokScan.EqualFolded(name, "list") THEN
		  sfr.serverListFile :=
		    SupMisc.CatPath(SupMisc.PathPrefix(path), value);
		ELSIF TokScan.EqualFolded(name, "prefix") THEN
		  sfr.serverPrefix :=
		    SupMisc.ResolvePath(sfr.serverBase, value);
		ELSIF TokScan.EqualFolded(name, "keywordprefix") THEN
		  sfr.keywordPrefix :=
		    SupMisc.ResolvePath(sfr.serverBase, value);
		ELSIF TokScan.EqualFolded(name, "super") THEN
		  sfr.superCollection := value;
		END;
	      ELSE
		IF TokScan.EqualFolded(field, "norsync") THEN
		  sfr.options := sfr.options +
		    SupFileRec.Options{SupFileRec.Option.NoRsync};
		ELSIF TokScan.EqualFolded(field, "nocheckrcs") THEN
		  sfr.options := sfr.options -
		    SupFileRec.Options{SupFileRec.Option.CheckRCS};
		ELSIF TokScan.EqualFolded(field, "norcs") THEN
		  sfr.options := sfr.options +
		    SupFileRec.Options{SupFileRec.Option.NoRCS};
		END;
	      END;
	    END;
	    EXIT;
	  END;
	END;
      FINALLY
	TRY
	  Rd.Close(rd);
	EXCEPT Rd.Failure(list) =>
	  RAISE Error("Cannot close \"" & path & "\": " &
	    ErrMsg.StrError(list));
	END;
      END;
      IF NOT foundRelease THEN
	RAISE Error("Unknown release \"" & sfr.release &
	  "\" for \"" & sfr.collection & "\"");
      END;
      IF sfr.serverListFile = NIL THEN
	RAISE TokScan.Error("Missing \"list\" specification for \""
	  & sfr.collection & "." & sfr.release & "\"");
      END;
      IF sfr.serverPrefix = NIL THEN
	sfr.serverPrefix := sfr.serverBase;
      END;
      (* If the prefix directory does not exist, just report that
	 the collection/release is not available. *)
      TRY
	IF FS.Status(sfr.serverPrefix).type # FS.DirectoryFileType THEN
	  OSErrorPosix.Raise0(Uerror.ENOTDIR);
	END;
      EXCEPT OSError.E(l) =>
	IF l.head = EagainAtom THEN
	  RAISE Error("Collection \"" & sfr.collection
	    & "\" release \"" & sfr.release & "\" is temporarily unavailable");
	ELSE
	  RAISE Error("Collection \"" & sfr.collection
	    & "\" release \"" & sfr.release & "\" is not available here");
	END;
      END;
      IF sfr.keywordPrefix = NIL THEN
	sfr.keywordPrefix := sfr.serverPrefix;
      END;
    EXCEPT
    | Rd.Failure(list) =>
	RAISE Error("Read error from \"" & path & "\": " &
	  ErrMsg.StrError(list));
    | TokScan.Error(msg) =>
	RAISE Error("Parse error in \"" & path & "\": " & msg);
    END;
  END ParseReleasesFile;

PROCEDURE SendOptionsInfo(self: SubProcess;
                          sfr: SupFileRec.T)
  RAISES {Error, Thread.Alerted, Wr.Failure} =
  VAR
    path := SupMisc.CatPath(sfr.serverPrefix, SupMisc.CVSOptions);
    rd: Rd.T;
    line: TEXT;
    lineNum := 0;
    ts: TokScan.T;
    cmd: TEXT;
    keyName: TEXT;
    aliasName: TEXT;
    args: TEXT;
    includeOnly: BOOLEAN;
  BEGIN
    TRY
      rd := FileRd.Open(path);
    EXCEPT OSError.E =>  (* No options file. *)
      RETURN;
    END;
    TRY
      TRY
	LOOP
	  TRY line := Rd.GetLine(rd) EXCEPT Rd.EndOfFile => EXIT END;
	  INC(lineNum);
	  WITH pos = Text.FindChar(line, '#') DO
	    IF pos >= 0 THEN line := Text.Sub(line, 0, pos) END;
	  END;
	  line := TokScan.Trim(line);
	  IF NOT Text.Empty(line) THEN
	    TRY
	      ts := TokScan.New(line, SET OF CHAR{'='});
	      cmd := TokScan.Trim(ts.getToken("command"));
	      IF TokScan.EqualFolded(cmd, "tag") THEN
		aliasName := TokScan.Trim(ts.getToken("alias name"));
		IF ts.next(keyName) THEN
		  WITH pos = Text.FindChar(keyName, ',') DO  (* Kludge. *)
		    IF pos >= 0 THEN keyName := Text.Sub(keyName, 0, pos) END;
		  END;
		  keyName := TokScan.Trim(keyName);
		ELSE
		  keyName := "Id";
		END;
		IF self.proto.v.hasKeywordControl THEN
		  TRY
		    sfr.expander.alias(aliasName, keyName);
		    self.proto.putCmd(self.wrA, "KEYALIAS",
		      aliasName, keyName);
		  EXCEPT RCSKeyword.Unknown =>
		    RAISE TokScan.Error("Unknown RCS keyword \""
		      & keyName & "\"");
		  END;
		END;
	      ELSIF TokScan.EqualFolded(cmd, "tagexpand") THEN
		args := TokScan.Trim(ts.getRest());
		IF Text.Empty(args) THEN
		  RAISE TokScan.Error("missing \"tagexpand\" arguments");
		END;
		CASE Text.GetChar(args, 0) OF
		| 'e' => includeOnly := FALSE;
		| 'i' => includeOnly := TRUE;
		ELSE
		  RAISE TokScan.Error(
		    "\"tagexpand\" argument must begin with \"e\" or \"i\"");
		END;
		IF self.proto.v.hasKeywordControl THEN
		  IF includeOnly THEN  (* First, disable all keywords. *)
		    sfr.expander.enableAll(enabled := FALSE);
		    self.proto.putCmd(self.wrA, "KEYOFF", ".");
		  ELSE  (* First, enable all keywords. *)
		    sfr.expander.enableAll(enabled := TRUE);
		    self.proto.putCmd(self.wrA, "KEYON", ".");
		  END;
		END;
		args := Text.Sub(args, 1);
		ts := TokScan.New(args, SET OF CHAR{','});
		WHILE ts.next(keyName) DO
		  keyName := TokScan.Trim(keyName);
		  IF self.proto.v.hasKeywordControl THEN
		    TRY
		      IF includeOnly THEN  (* Enable specific keyword. *)
			sfr.expander.enable(keyName, enabled := TRUE);
			self.proto.putCmd(self.wrA, "KEYON", keyName);
		      ELSE  (* Disable specific keyword. *)
			sfr.expander.enable(keyName, enabled := FALSE);
			self.proto.putCmd(self.wrA, "KEYOFF", keyName);
		      END;
		    EXCEPT RCSKeyword.Unknown =>
		      RAISE TokScan.Error("Unknown RCS keyword \""
			& keyName & "\"");
		    END;
		  END;
		END;
	      ELSE
		(* For now, we just ignore unrecognized commands. *)
	      END;
	    EXCEPT TokScan.Error(msg) =>
	      self.log(path & ":" & Fmt.Int(lineNum) & ": " & msg);
	    END;
	  END;
	END;
      FINALLY
	Rd.Close(rd);
      END;
    EXCEPT
    | Rd.Failure(l) =>
	RAISE Error("Read error from \"" & path & "\": " &
	  ErrMsg.StrError(l));
    END;
  END SendOptionsInfo;

PROCEDURE SendListInfo(self: SubProcess;
                       sfr: SupFileRec.T)
  RAISES {Error, Thread.Alerted, Wr.Failure} =
  VAR (* CONST *)
    NoWS              := GlobTree.Not(GlobTree.Match("*[ \t\r\n]*"));
  VAR
    rd: Rd.T;
    line: TEXT;
    ts: TokScan.T;
    cmd: TEXT;
    pat: TEXT;
    dirUpgrade        := GlobTree.False;
    fileUpgrade       := GlobTree.False;
    dirAlways         := GlobTree.False;
    fileAlways        := GlobTree.False;
    dirAccept         := GlobTree.True;
    fileAccept        := GlobTree.True;
    dirRefuse         := GlobTree.False;
    fileRefuse        := GlobTree.False;
    omit              := GlobTree.False;
    symlink           := GlobTree.False;
  BEGIN
    TRY
      TRY
	rd := FileRd.Open(sfr.serverListFile);
      EXCEPT OSError.E(list) =>
	RAISE Error("Cannot open \"" & sfr.serverListFile & "\": " &
	  ErrMsg.StrError(list));
      END;
      TRY
	LOOP
	  TRY line := Rd.GetLine(rd) EXCEPT Rd.EndOfFile => EXIT END;
	  ts := TokScan.New(line);
	  IF ts.next(cmd) AND Text.GetChar(cmd, 0) # '#' THEN
	    IF TokScan.EqualFolded(cmd, "upgrade") THEN
	      pat := ts.getToken("pattern");
	      REPEAT
		dirUpgrade := GlobTree.Or(dirUpgrade,
		  GlobTree.Match(pat, Glob.MatchOptions{
		    Glob.MatchOption.Pathname,
		    Glob.MatchOption.LeadingDir,
		    Glob.MatchOption.PrefixDirs }));
		fileUpgrade := GlobTree.Or(fileUpgrade,
		  GlobTree.Match(pat, Glob.MatchOptions{
		    Glob.MatchOption.Pathname,
		    Glob.MatchOption.LeadingDir }));
		IF self.proto.v.serverSendsFilter THEN
		  self.proto.putCmd(self.wrA, "UPGR", pat);
		END;
	      UNTIL NOT ts.next(pat);
	    ELSIF TokScan.EqualFolded(cmd, "always") THEN
	      pat := ts.getToken("pattern");
	      REPEAT
		dirAlways := GlobTree.Or(dirAlways,
		  GlobTree.Match(pat, Glob.MatchOptions{
		    Glob.MatchOption.Pathname,
		    Glob.MatchOption.LeadingDir,
		    Glob.MatchOption.PrefixDirs }));
		fileAlways := GlobTree.Or(fileAlways,
		  GlobTree.Match(pat, Glob.MatchOptions{
		    Glob.MatchOption.Pathname,
		    Glob.MatchOption.LeadingDir }));
		IF self.proto.v.serverSendsFilter THEN
		  self.proto.putCmd(self.wrA, "ALWS", pat);
		END;
	      UNTIL NOT ts.next(pat);
	    ELSIF TokScan.EqualFolded(cmd, "omitany") THEN
	      pat := ts.getToken("pattern");
	      REPEAT
		omit := GlobTree.Or(omit, GlobTree.Match(pat));
		IF self.proto.v.serverSendsFilter THEN
		  self.proto.putCmd(self.wrA, "OANY", pat);
		END;
	      UNTIL NOT ts.next(pat);
	    ELSIF TokScan.EqualFolded(cmd, "symlink") THEN
	      pat := ts.getToken("pattern");
	      REPEAT
		symlink := GlobTree.Or(symlink,
		  GlobTree.Match(pat, Glob.MatchOptions{
		    Glob.MatchOption.Pathname }));
	      UNTIL NOT ts.next(pat);
	    ELSIF TokScan.EqualFolded(cmd, "rsymlink") THEN
	      pat := ts.getToken("pattern");
	      REPEAT
		symlink := GlobTree.Or(symlink,
		  GlobTree.Match(pat, Glob.MatchOptions{
		    Glob.MatchOption.Pathname,
		    Glob.MatchOption.LeadingDir }));
	      UNTIL NOT ts.next(pat);
	    ELSIF TokScan.EqualFolded(cmd, "norsync") THEN
	      pat := ts.getToken("pattern");
	      REPEAT
		sfr.noRsync := GlobTree.Or(sfr.noRsync,
		  GlobTree.Match(pat, Glob.MatchOptions{
		    Glob.MatchOption.Pathname }));
		IF self.proto.v.hasRsyncFilter THEN
		  self.proto.putCmd(self.wrA, "NORS", pat);
		END;
	      UNTIL NOT ts.next(pat);
	    ELSIF TokScan.EqualFolded(cmd, "rnorsync") THEN
	      pat := ts.getToken("pattern");
	      REPEAT
		sfr.noRsync := GlobTree.Or(sfr.noRsync,
		  GlobTree.Match(pat, Glob.MatchOptions{
		    Glob.MatchOption.Pathname,
		    Glob.MatchOption.LeadingDir }));
		IF self.proto.v.hasRsyncFilter THEN
		  self.proto.putCmd(self.wrA, "RNORS", pat);
		END;
	      UNTIL NOT ts.next(pat);
	    ELSIF TokScan.EqualFolded(cmd, "execute") THEN
	      sfr.executes.addhi(ParseExec(ts.getRest()));
	    END;
	  END;
	END;

	IF FileAttr.AttrType.FileType IN
	self.proto.v.attrSupport[FileAttr.FileType.SymLink] THEN
	  sfr.symlink := symlink;  (* Symbolic links are supported. *)
	ELSE  (* Follow all symbolic links. *)
	  sfr.symlink := GlobTree.False;
	END;

	(* Build filters from the client's "accepts" and "refuses".
	   This is a little bit tricky, because in checkout mode
	   the client specifies the names of the checked-out files,
	   not the RCS files that we deal with here on the server.  *)

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
	    (* Regardless of whether we are in checkout mode, we want to
	       accept the file if the unaltered pattern matches a leading
	       directory of it. *)
	    fileAccept := GlobTree.Or(fileAccept,
	      GlobTree.Match(pat, Glob.MatchOptions{
		Glob.MatchOption.Pathname,
		Glob.MatchOption.LeadingDir }));
	    (* If we are in checkout mode, then we also want to accept the
	       file if "<pattern>,v" matches it. *)
	    IF SupFileRec.Option.CheckoutMode IN sfr.options THEN
	      WITH len = Text.Length(pat) DO
		IF len = 0 OR Text.GetChar(pat, len-1) # '*' THEN
		  (* Adding ",v" to the end would make a difference. *)
		  fileAccept := GlobTree.Or(fileAccept,
		    GlobTree.Match(pat & SupMisc.RCSSuffix,
		      Glob.MatchOptions{ Glob.MatchOption.Pathname }));
		END;
	      END;
	    END;
	  END;
	END;

	IF SupFileRec.Option.CheckoutMode IN sfr.options THEN
	  (* We don't want the "CVSROOT" directory. *)
	  dirRefuse := GlobTree.Or(dirRefuse,
	    GlobTree.Match(SupMisc.CVSAdmin));
	END;
	FOR i := 0 TO sfr.refusals.size()-1 DO
	  pat := sfr.refusals.get(i);
	  dirRefuse := GlobTree.Or(dirRefuse, GlobTree.Match(pat));
	  IF SupFileRec.Option.CheckoutMode IN sfr.options THEN
	    (* We must modify the client-specified pattern so that it refers
	       to the RCS file, rather than the checked-out file. *)
	    WITH len = Text.Length(pat) DO
	      IF len = 0 OR Text.GetChar(pat, len-1) # '*' THEN
		pat := pat & SupMisc.RCSSuffix;
	      END;
	    END;
	  END;
	  fileRefuse := GlobTree.Or(fileRefuse, GlobTree.Match(pat));
	END;

	(* Now combine the filter pieces. *)

	(* Start with the files from "upgrade" lines on the server. *)
	sfr.dirFilter := dirUpgrade;
	sfr.fileFilter := fileUpgrade;

	(* Take out the files from "omitany" lines on the server. *)
	sfr.dirFilter := GlobTree.And(sfr.dirFilter, GlobTree.Not(omit));
	sfr.fileFilter := GlobTree.And(sfr.fileFilter, GlobTree.Not(omit));

	(* Add back the files from "always lines on the server. *)
	sfr.dirFilter := GlobTree.Or(sfr.dirFilter, dirAlways);
	sfr.fileFilter := GlobTree.Or(sfr.fileFilter, fileAlways);

	(* Restrict the files to those specified in client "accepts". *)
	sfr.dirFilter := GlobTree.And(sfr.dirFilter, dirAccept);
	sfr.fileFilter := GlobTree.And(sfr.fileFilter, fileAccept);

	(* Take out the files from client "refuse" files. *)
	sfr.dirFilter := GlobTree.And(sfr.dirFilter, GlobTree.Not(dirRefuse));
	sfr.fileFilter :=
	  GlobTree.And(sfr.fileFilter, GlobTree.Not(fileRefuse));

	(* If the client can't handle white space in file names, make sure
	   we don't send him any. *)
	IF NOT self.proto.v.handlesWhiteSpace THEN
	  sfr.dirFilter := GlobTree.And(sfr.dirFilter, NoWS);
	  sfr.fileFilter := GlobTree.And(sfr.fileFilter, NoWS);
	END;
      FINALLY
	TRY
	  Rd.Close(rd);
	EXCEPT Rd.Failure(list) =>
	  RAISE Error("Cannot close \"" & sfr.serverListFile & "\": " &
	    ErrMsg.StrError(list));
	END;
      END;
    EXCEPT
    | Rd.Failure(list) =>
      RAISE Error("Read error from \"" & sfr.serverListFile & "\": " &
	ErrMsg.StrError(list));
    | TokScan.Error(msg) =>
      RAISE Error("Parse error in \"" & sfr.serverListFile & "\": " & msg);
    END;
  END SendListInfo;

PROCEDURE ParseExec(line: TEXT): ExecRec.T
  RAISES {TokScan.Error} =
    VAR
      cmdLim: INTEGER;
      patLim: INTEGER;
      patterns: TEXT;
      pattern: TEXT;
      command: TEXT;
      ts: TokScan.T;
      gt := GlobTree.False;
  BEGIN
    cmdLim := Text.FindChar(line, '(');
    IF cmdLim = -1 THEN RAISE TokScan.Error("\"(\" expected") END;
    command := TokScan.Trim(Text.Sub(line, 0, cmdLim));
    IF Text.Empty(command) THEN
      RAISE TokScan.Error("Missing command for \"execute\"");
    END;

    patLim := Text.FindChar(line, ')', cmdLim+1);
    IF patLim = -1 THEN RAISE TokScan.Error("\")\" expected") END;
    patterns := Text.Sub(line, cmdLim+1, patLim-(cmdLim+1));

    IF NOT Text.Empty(TokScan.Trim(Text.Sub(line, patLim+1))) THEN
      RAISE TokScan.Error("Unexpected garbage after \")\"");
    END;

    ts := TokScan.New(patterns);
    WHILE ts.next(pattern) DO
      gt := GlobTree.Or(gt,
	GlobTree.Match(pattern, Glob.MatchOptions{
	  Glob.MatchOption.Pathname }));
    END;

    RETURN NEW(ExecRec.T, pattern := gt, command := command);
  END ParseExec;

(*****************************************************************************)

PROCEDURE FindScanFiles(self: SubProcess)
  RAISES {Thread.Alerted} =
  CONST
    MaxNestedCollectionDepth = 100;
  VAR
    scanBase: Pathname.T;
    sfr, scanSFR: SupFileRec.T;
    newestTime: Time.T;
    scanPath, newestPath: Pathname.T;
    releasesPath: Pathname.T;
    super: TEXT;
  BEGIN
    IF self.parent.config.serverScanDir = NIL THEN  (* Not using scan files. *)
      RETURN;
    END;
    scanBase := SupMisc.ResolvePath(self.parent.config.serverBase,
      self.parent.config.serverScanDir);
    FOR i := 0 TO self.collections.size()-1 DO
      sfr := self.collections.get(i);
      IF NOT SupFileRec.Option.Skip IN sfr.options THEN
	newestTime := -1.0d0;
	newestPath := NIL;

	(* Clone enough of the original SupFileRec to do what we need to do,
	   but leave the checkout mode option turned off in the new copy. *)
	scanSFR := NEW(SupFileRec.T).init();
	scanSFR.collection := sfr.collection;
	scanSFR.release := sfr.release;
	scanSFR.serverBase := sfr.serverBase;
	scanSFR.serverCollDirs := sfr.serverCollDirs;
	scanSFR.options := SupFileRec.Options{SupFileRec.Option.UseRelSuffix};
	scanSFR.superCollection := sfr.superCollection;

	(* Find the newest scan file that covers this collection,
	   searching from the current collection up through successive
	   superset collections.  We limit the number of levels we will
	   search, to protect ourselves against loops in the superset
	   specifications. *)
	FOR i := 1 TO MaxNestedCollectionDepth DO
	  scanPath := SupMisc.CatPath(scanBase,
	    SupMisc.CatPath(scanSFR.collection,
	    SupMisc.StatusFileName(scanSFR)));
	  TRY
	    WITH t = FS.Status(scanPath).modificationTime DO
	      IF t > newestTime THEN
		newestTime := t;
		newestPath := scanPath;
	      END;
	    END;
	  EXCEPT OSError.E => (* Ignore. *) END;

	  super := scanSFR.superCollection;
	  IF super = NIL THEN EXIT END;

	  (* Go up to the next super-collection and repeat. *)
	  scanSFR := NEW(SupFileRec.T).init();
	  scanSFR.collection := super;
	  scanSFR.release := sfr.release;
	  scanSFR.serverBase := sfr.serverBase;
	  scanSFR.serverCollDirs := sfr.serverCollDirs;
	  scanSFR.options :=
	    SupFileRec.Options{SupFileRec.Option.UseRelSuffix};

	  releasesPath := SupMisc.FindFile(self.parent.config.serverBase,
	    self.parent.config.serverCollDirs,
	    SupMisc.CatPath(scanSFR.collection, "releases"));
	  IF releasesPath = NIL THEN  (* Super-collection doesn't exist. *)
	    EXIT;
	  END;
	  TRY
	    ParseReleasesFile(self, scanSFR, releasesPath);
	  EXCEPT Error =>  (* Cannot parse super-collection. *)
	    EXIT;
	  END;
	END;

	sfr.serverScanFile := newestPath;
      END;
    END;
  END FindScanFiles;

(*****************************************************************************)

PROCEDURE Accept(self: SubProcess;
                 conn: TCP.Connector;
		 cmd: TEXT): TCP.T
  RAISES {Error, Thread.Alerted, Wr.Failure} =
(* Sends the connector's endpoint with the command "cmd", and accepts
   one connection from the peer. *)
  VAR
    wd: WatchDog.T;
    epToks: ARRAY [0..4] OF TEXT;
  BEGIN
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
	RAISE Error("Accept failed: " & ErrMsg.StrError(list));
    | Thread.Alerted =>
	IF WatchDog.Expired(wd) THEN
	  RAISE Error("Timed out waiting for connection from client");
	ELSE
	  RAISE Thread.Alerted;
	END;
    END;
  END Accept;

PROCEDURE ConnectFrom(to, from: IP.Endpoint): TCP.T
  RAISES {Error, Thread.Alerted} =
  BEGIN
    TRY
      RETURN TCPMisc.ConnectFrom(to := to, from := from);
    EXCEPT IP.Error(list) =>
      RAISE Error("Connect failed: " & ErrMsg.StrError(list));
    END;
  END ConnectFrom;

PROCEDURE Fork(): Utypes.pid_t
  RAISES {ForkFailed} =
  VAR
    childPid := RTProcess.Fork();
  BEGIN
    IF childPid = -1 THEN
      RAISE ForkFailed(AtomList.List1(OSErrorPosix.ErrnoAtom(
	Cerrno.GetErrno())));
    END;
    RETURN childPid;
  END Fork;

PROCEDURE GetHostName(addr: IP.Address): TEXT =
  VAR
    hostName: TEXT;
  BEGIN
    TRY
      hostName := IP.GetCanonicalByAddr(addr);
    EXCEPT IP.Error =>
      hostName := NIL;
    END;
    IF hostName = NIL THEN
      hostName := Fmt.Int(addr.a[0])
	& "." & Fmt.Int(addr.a[1])
	& "." & Fmt.Int(addr.a[2])
	& "." & Fmt.Int(addr.a[3]);
    END;
    RETURN hostName;
  END GetHostName;

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
  RAISES {Error} =
  BEGIN
    TRY
      RETURN SupMisc.NewConnector(addr, loPort, hiPort);
    EXCEPT IP.Error(list) =>
      RAISE Error("Listen failed: " & ErrMsg.StrError(list));
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

TYPE
  IdleKiller = IOWatchDog.T OBJECT
    sub: SubProcess;
  METHODS
    init(sub: SubProcess;
	 timeout: Time.T;
	 pollInterval: Time.T := 60.0d0): IdleKiller := IdleKillerInit;
  OVERRIDES
    alert := IdleAlert;
  END;

PROCEDURE IdleKillerInit(ik: IdleKiller;
			 sub: SubProcess;
			 timeout: Time.T;
			 pollInterval: Time.T := 60.0d0): IdleKiller =
  BEGIN
    ik.sub := sub;
    EVAL IOWatchDog.T.init(ik, timeout, pollInterval);
    RETURN ik;
  END IdleKillerInit;

PROCEDURE IdleAlert(ik: IdleKiller) =
  BEGIN
    ik.sub.log("[0Kin+0Kout] Inactivity timeout", '-');
    EVAL Usignal.kill(Process.GetMyID(), Usignal.SIGALRM);
  END IdleAlert;

BEGIN
END FSServer.
