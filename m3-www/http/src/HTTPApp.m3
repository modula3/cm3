(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Created by steveg *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 14:50:46 1997
 *)

MODULE HTTPApp;

<* PRAGMA LL *>

IMPORT App, ConnRW, FloatMode, Fmt, HTTP, IP, Lex, Rd, RdUtils, TCP,
       TCPPeer, Text, TextExtras, TextRd, Thread, Wr;

(* ClientLog logs the message to the client (through "wr") and 
   to the application log (through "log")
 *)
TYPE
  ClientLog = App.Log OBJECT
    appLog: App.Log;
    wr: Wr.T;
    msg: TEXT;
    serverPush: BOOLEAN;
  OVERRIDES
    log := MessageLog;
  END;

PROCEDURE MessageLog (self: ClientLog; msg: TEXT; status: App.LogStatus)
  RAISES {App.Error} =
  BEGIN
    IF status = App.LogStatus.Error THEN
      IF self.serverPush THEN
        self.msg := Fmt.F("%s%s\n", self.msg, msg);
        ServerPushFrame(self.wr, "text/plain", self.msg, self.appLog);
      ELSE
        TRY
          Wr.PutText(self.wr, msg);
          Wr.PutText(self.wr, "\n");
        EXCEPT
        | Wr.Failure, Thread.Alerted =>
            self.appLog.log(
              "Error sending message to client", App.LogStatus.Error);
        END;
      END;
    END;
    self.appLog.log(msg, status);
  END MessageLog;

PROCEDURE WrLog (old: App.Log; wr: Wr.T; serverPush: BOOLEAN := FALSE):
  App.Log =
  BEGIN
    RETURN NEW(ClientLog, appLog := old, wr := wr,
               msg := "", serverPush := serverPush);
  END WrLog;

VAR
  readWriteMu := NEW(MUTEX);
  readWriteCV := NEW(Thread.Condition);
  readingCnt := 0;
  writingCnt := 0;
  (* single writer, multiple reader algorithm.

     if a thread is writing, then writingCnt # 0.
     if a thread is reading, then readingCnt # 0.

     a thread can read if another thread is reading.
     a thread cannot read if another thread is writing.
     a thread can write if no thread is reading or writing.

     readingCnt and writingCnt are protected by readWriteMu.
  *)
     
PROCEDURE ReadLock() =
  BEGIN
    LOCK readWriteMu DO
      WHILE writingCnt # 0 DO
        Thread.Wait(readWriteMu, readWriteCV);
      END;

      INC(readingCnt);
    END;
  END ReadLock;

PROCEDURE ReadUnlock() =
  BEGIN
    LOCK readWriteMu DO
      DEC(readingCnt);
    END;
  END ReadUnlock;

PROCEDURE WriteLock() =
  BEGIN
    LOCK readWriteMu DO
      WHILE readingCnt # 0 OR writingCnt # 0 DO
        Thread.Wait(readWriteMu, readWriteCV);
      END;

      INC(writingCnt);
    END;
  END WriteLock;

PROCEDURE WriteUnlock() =
  BEGIN
    LOCK readWriteMu DO
      DEC(writingCnt);
    END;
  END WriteUnlock;

REVEAL
  RequestHandler = RequestHandlerPublic BRANDED
  "HTTPApp.RequestHandler" OBJECT
    port: INTEGER;
  OVERRIDES
    accept := DefaultAccept;
    request := DefaultRequest;
  END;

PROCEDURE DefaultAccept (<* UNUSED *>     self       : RequestHandler;
                         <* UNUSED *>     request    : HTTP.Request;
                         <* UNUSED *>     serverData : REFANY;
                         <* UNUSED *> VAR acceptState: REFANY;
                         <* UNUSED *>     log        : App.Log         ):
  BOOLEAN =
  BEGIN
    <* ASSERT FALSE *>
  END DefaultAccept;

PROCEDURE DefaultRequest(<* UNUSED *> self: RequestHandler;
                         <* UNUSED *> request: HTTP.Request;
                         <* UNUSED *> serverData, acceptState: REFANY;
                         <* UNUSED *> rd: Rd.T;
                         <* UNUSED *> wr: Wr.T;
                         <* UNUSED *> log: App.Log) =
  BEGIN
    <* ASSERT FALSE *>
  END DefaultRequest;

REVEAL
  ReplyHandler = ReplyHandlerPublic BRANDED "HTTPApp.ReplyHandler" OBJECT
  OVERRIDES
    reply := DefaultReply;
  END;

PROCEDURE DefaultReply(<* UNUSED *> self: ReplyHandler;
                       <* UNUSED *> reply: HTTP.Reply;
                       <* UNUSED *> rd: Rd.T;
                       <* UNUSED *> wr: Wr.T;
                       <* UNUSED *> log: App.Log) =
  BEGIN
    <* ASSERT FALSE *>
  END DefaultReply;

REVEAL
  Proxy = ProxyPublic BRANDED "HTTPApp.Proxy" OBJECT
          OVERRIDES
            init := InitProxy;
            add  := AddProxy;
          END;

PROCEDURE InitProxy (self: Proxy): Proxy =
  BEGIN
    self.rules := NIL;
    self.tail := NIL;
    RETURN self;
  END InitProxy;

CONST
  NotComma = SET OF CHAR{'\000' .. '\377'} - SET OF CHAR{','};

PROCEDURE AddProxy (self: Proxy; ruleTxt: TEXT; log: App.Log)
  RAISES {App.Error} =
  VAR
    rd               := TextRd.New(ruleTxt);
    rule             := NEW(ProxyRules);
    prev: ServerList := NIL;
    host: TEXT;
  BEGIN
    IF log = NIL THEN log := App.defaultLog; END;
    TRY
      rule.hostPattern := Lex.Scan(rd);
      IF Text.Length(rule.hostPattern) = 0
           OR Text.GetChar(
                rule.hostPattern, Text.Length(rule.hostPattern) - 1) = '\\' THEN
        log.log(
          Fmt.F("Bad pattern (%s) in hostPattern proxy(s) value: \"%s\"",
                rule.hostPattern, ruleTxt), App.LogStatus.Error);
      END;

      TRY
        LOOP
          Lex.Skip(rd);
          host := Lex.Scan(rd, NotComma);
          IF Text.Length(host) > 0 THEN
            WITH server = NEW(ServerList,
                              head := NEW(Server).initParse(host, log),
                              tail := NIL) DO
              IF prev = NIL THEN
                prev := server;
                rule.proxy := prev;
              ELSE
                prev.tail := server;
                prev := prev.tail
              END;
            END;
          END;
          EVAL Rd.GetChar(rd);
        END;
      EXCEPT
      | Rd.EndOfFile =>
      END;
    EXCEPT
    | Rd.Failure, Thread.Alerted =>
        log.log(Fmt.F("Bad urlPattern:proxy value: \"%s\"", ruleTxt),
                App.LogStatus.Error);
    END;
    IF self.tail = NIL THEN
      self.rules := rule;
    ELSE
      self.tail.tail := rule;
    END;
    self.tail := rule;
  END AddProxy;

REVEAL
  Server = ServerPublic BRANDED "HTTPApp.Server" OBJECT
  OVERRIDES
    init := InitServer;
    initParse := InitParseServer;
  END;

PROCEDURE InitServer (self  : Server;
                      server: TEXT;
                      port  : INTEGER;
                      log   : App.Log  ): Server RAISES {App.Error} =
  VAR addr: IP.Address;
  BEGIN
    IF Text.Equal(server, "DIRECT") THEN
      self.server := NIL
    ELSE
      self.server := server;
      self.port := port;
      self.endpoint := IP.NullEndPoint;
      TRY
        IF NOT IP.GetHostByName(server, addr) THEN
          log.log(Fmt.F("Cannot find %s", server), App.LogStatus.Status);
          RETURN self;
        END;
      EXCEPT
      | IP.Error =>
          log.log(Fmt.F("Cannot find %s", server), App.LogStatus.Status);
          RETURN self;
      END;
      self.endpoint := IP.Endpoint{addr := addr, port := port};
    END;
    RETURN self;
  END InitServer;

CONST 
  NonColon = SET OF CHAR{'\000'..'\377'} - SET OF CHAR{':'};
  NonSlash = SET OF CHAR{'\000'..'\377'} - SET OF CHAR{'/'};

PROCEDURE InitParseServer (self: Server; serverAndPort: TEXT; log: App.Log):
  Server RAISES {App.Error} =
  VAR
    server: TEXT;
    port         := 0;
  BEGIN
    IF Text.Equal(serverAndPort, "DIRECT") THEN
      server := "DIRECT";
    ELSE
      WITH rd = TextRd.New(serverAndPort) DO
        TRY
          server := Lex.Scan(rd, NonColon);
          IF Text.Equal(server, "http") THEN
            WITH url = NEW(HTTP.URL).init(serverAndPort, log) DO
              server := url.host;
              port := url.port;
            END;
          ELSE
            IF NOT Rd.EOF(rd) THEN
              EVAL Rd.GetChar(rd);
              port := Lex.Int(rd);
            END;
          END;
        EXCEPT
        | Lex.Error, FloatMode.Trap, Rd.EndOfFile, Rd.Failure,
              Thread.Alerted =>
            log.log(
              Fmt.F("Bad server and port given: \"%s\" (need server:port)",
                    serverAndPort), App.LogStatus.Error);
        END;
      END;
    END;
    RETURN self.init(server, port, log);
  END InitParseServer;

TYPE
  RequestHandlerList = REF RECORD
    head: RequestHandler;
    tail: RequestHandlerList;
  END;

VAR
  requestHandlerList: RequestHandlerList := NIL;

PROCEDURE FindRequestHandler (              request      : HTTP.Request;
                                            serverData   : REFANY;
                              VAR (* OUT *) acceptState  : REFANY;
                                            port, service: INTEGER;
                                            log          : App.Log       ):
  RequestHandler RAISES {App.Error} =
  VAR list: RequestHandlerList;
  BEGIN
    ReadLock();
    TRY
      IF requestHandlerList = NIL THEN RETURN NIL END;
      FOR i := FIRST(RequestPriority) TO LAST(RequestPriority) DO
        list := requestHandlerList;
        REPEAT
          IF list.head.priority = i
               AND (list.head.port = service OR list.head.port = port
                      OR list.head.port = AnyPort)
               AND list.head.accept(request, serverData, acceptState, log) THEN
            RETURN list.head
          END;
          list := list.tail;
        UNTIL list = NIL;
      END;
    FINALLY
      ReadUnlock();
    END;
    RETURN NIL;
  END FindRequestHandler;

TYPE
  Closure = Thread.Closure OBJECT
              client: TCP.T;
              log   : App.Log;
              wrLog : BOOLEAN;
              data  : REFANY;
              port, service  : INTEGER;
            OVERRIDES
              apply := ServerHandler;
            END;

PROCEDURE GetNameDontCrash (host: TCP.T): TEXT RAISES {IP.Error} =
  VAR res := TCPPeer.GetName(host);
  BEGIN
    IF res = NIL THEN
      WITH ep = TCPPeer.Get(host) DO
        RETURN
          Fmt.F("%s.%s.%s.%s", Fmt.Int(ep.addr.a[0]), Fmt.Int(ep.addr.a[1]),
                Fmt.Int(ep.addr.a[2]), Fmt.Int(ep.addr.a[3]));
      END;
    END;
    RETURN res
  END GetNameDontCrash;

PROCEDURE ServerHandler (cl: Closure): REFANY =
  VAR
    rd            : Rd.T;
    wr            : Wr.T;
    request       : HTTP.Request;
    requestHandler: RequestHandler;
    acceptState   : REFANY;
  BEGIN
    TRY
      TRY
        rd := ConnRW.NewRd(cl.client);
        wr := ConnRW.NewWr(cl.client);
        IF cl.wrLog THEN cl.log := WrLog(cl.log, wr, FALSE); END;

        request := NEW(HTTP.Request).parse(rd, cl.log);
        IF Text.Length(request.url.host) = 0 THEN
          request.url.host := App.GetHostName();
          request.url.port := cl.port;
        END;

        IF App.Verbose() THEN
          cl.log.log(Fmt.F("INCOMING REQUEST: %s",
                           request.toText(NIL, TRUE, cl.log)),
                     App.LogStatus.Verbose);
        ELSIF App.Debug() THEN
          TRY
            cl.log.log(Fmt.F("%s %s", GetNameDontCrash(cl.client),
                             request.url.toText()), App.LogStatus.Debug);
          EXCEPT
          | IP.Error =>
          END;
        END;

        requestHandler := FindRequestHandler(request, cl.data, acceptState,
                                             cl.port, cl.service, cl.log);
        IF requestHandler # NIL THEN
          requestHandler.request(
            request, cl.data, acceptState, rd, wr, cl.log);
        ELSE
          TRY
            HTTP.WriteSimpleReplyHeader(
              wr, NIL, cl.log, HTTP.StatusCode[HTTP.StatusType.Not_Found],
              HTTP.StatusReason[HTTP.StatusType.Not_Found]);
            Wr.PutText(wr, "Content-type: text/plain\r\n\r\n");
            Wr.PutText(wr, Fmt.F("The requested item: %s was not found\n",
                                 request.url.toText()));
          EXCEPT
          | Wr.Failure, Thread.Alerted =>
          END;
        END;
      FINALLY
        TRY Wr.Close(wr); EXCEPT | Wr.Failure, Thread.Alerted => END;
        TRY Rd.Close(rd); EXCEPT | Rd.Failure, Thread.Alerted => END;
        Thread.Pause(30.0d0);
        TCP.Close(cl.client);
      END;
    EXCEPT
    | App.Error =>
    END;
    RETURN NIL;
  END ServerHandler;

TYPE
  ServerPorts = REF RECORD
    port, service: INTEGER;
    next: ServerPorts;
  END;

VAR
  serverPorts: ServerPorts := NIL; <* LL = readWriteMu *>

PROCEDURE ServerPort (port, service: INTEGER): BOOLEAN =
  VAR sp: ServerPorts;
  BEGIN
    LOCK readWriteMu DO
      sp := serverPorts;
      WHILE sp # NIL DO
        IF sp.port = port
             AND (sp.service = service OR sp.service = AnyService) THEN
          RETURN TRUE;
        END;
        sp := sp.next;
      END;
      RETURN FALSE;
    END;
  END ServerPort;

PROCEDURE Serve (port, serviceValue: INTEGER; log: App.Log; data: REFANY)
  RAISES {App.Error} =
  VAR
    conn     : TCP.T;
    connector: TCP.Connector;
    nilLog                   := log = NIL;
  BEGIN
    LOCK readWriteMu DO
      serverPorts := NEW(ServerPorts, port := port, service := serviceValue,
                                    next := serverPorts);
    END;

    IF log = NIL THEN log := App.defaultLog END;
    TRY
      connector := TCP.NewConnector(
                     IP.Endpoint{addr := IP.NullAddress, port := port});
    EXCEPT
    | IP.Error (cause) =>
        log.log("IP.Error making connector: " & RdUtils.FailureText(cause),
                App.LogStatus.Error);
        RETURN;
    END;
    log.log(
      Fmt.F("Listening on port: %s", Fmt.Int(port)), App.LogStatus.Status);

    LOOP
      TRY
        conn := TCP.Accept(connector);
        EVAL Thread.Fork(
               NEW(Closure, client := conn, log := log, wrLog := nilLog,
                   data := data, port := port, service := serviceValue));
      EXCEPT
      | IP.Error, Thread.Alerted => (* continue *)
      END;
    END;
  END Serve;

PROCEDURE Direct (request: HTTP.Request; log: App.Log): TCP.T
  RAISES {App.Error} =
  VAR server := NEW(Server).init(request.url.host, request.url.port, log);
  BEGIN
    TRY
      RETURN TCP.Connect(server.endpoint);
    EXCEPT
    | IP.Error, Thread.Alerted =>
        log.log(Fmt.F("Unable to connect to server: %s:%s", server.server,
                      Fmt.Int(server.port)), App.LogStatus.Error);
    END;
    <* ASSERT FALSE *>
  END Direct;

PROCEDURE Client (             request : HTTP.Request;
                               proxy   : Proxy;
                               style   : HTTP.Style;
                               rdClient: Rd.T;
                               wrClient: Wr.T;
                               handler : ReplyHandler;
                  <* UNUSED *> service : INTEGER;
                               log     : App.Log       )
  RAISES {App.Error} =
  VAR
    conn        : TCP.T;
    wrServer    : Wr.T;
    rdServer    : Rd.T;
    rules       : ProxyRules;
    proxyRequest: BOOLEAN    := FALSE;
    proxies     : ServerList;
    hp          : TEXT;
  BEGIN
    IF log = NIL THEN log := App.defaultLog END;
    IF style = NIL THEN style := HTTP.DefaultStyle() END;
    IF App.Verbose() THEN
      log.log(
        Fmt.F("OUTGOING request: %s", request.toText(style, TRUE, log)),
        App.LogStatus.Verbose);
    END;

    IF Text.Length(request.url.host) = 0 THEN
      log.log(Fmt.F("No server given in requested URL: %s",
                    request.url.toText()), App.LogStatus.Error);
    END;

    IF proxy = NIL THEN
      conn := Direct(request, log);
    ELSE
      rules := proxy.rules;
      hp := Fmt.F("%s:%s", request.url.host, Fmt.Int(request.url.port));
      WHILE rules # NIL AND conn = NIL DO
        IF TextExtras.PatternMatch(hp, rules.hostPattern) THEN
          IF App.Verbose() THEN
            IF rules.proxy.head.server = NIL THEN
              log.log(Fmt.F("proxy rule matched: %s %s DIRECT",
                            Fmt.F("%s:%s", request.url.host,
                                  Fmt.Int(request.url.port)),
                            rules.hostPattern), App.LogStatus.Verbose);
            ELSE
              log.log(
                Fmt.F("proxy rule matched: %s %s %s",
                      Fmt.F("%s:%s", request.url.host,
                            Fmt.Int(request.url.port)), rules.hostPattern,
                      rules.proxy.head.server), App.LogStatus.Verbose);
            END;
          END;
          proxies := rules.proxy;
          WHILE proxies # NIL DO
            WITH server = proxies.head DO
              IF server.server = NIL THEN
                conn := Direct(request, log);
                EXIT;
              ELSE
                TRY
                  IF server.endpoint = IP.NullEndPoint THEN
                    (* try to resolve another time *)
                    EVAL server.init(server.server, server.port, log);
                  END;
                  conn := TCP.Connect(server.endpoint);
                  proxyRequest := TRUE;
                  EXIT;
                EXCEPT
                | IP.Error, Thread.Alerted =>
                    log.log(
                      Fmt.F("Unable to connect to proxy server: %s:%s",
                            server.server, Fmt.Int(server.port)),
                      App.LogStatus.Status);
                END;
              END;
            END;
            proxies := proxies.tail;
          END;
        END;
        rules := rules.tail;
      END;

      IF conn = NIL THEN
        log.log(Fmt.F("proxy rule matched: %s DIRECT (default)",
                      Fmt.F("%s:%s", request.url.host,
                            Fmt.Int(request.url.port))),
                App.LogStatus.Verbose);
        conn := Direct(request, log);
      END;
    END;

    wrServer := ConnRW.NewWr(conn);
    rdServer := ConnRW.NewRd(conn);
    TRY
      TRY
        TRY
          request.write(wrServer, style, proxyRequest, log);
          IF rdClient # NIL AND request.method = HTTP.Method.Post
               AND request.postData # NIL THEN
            Wr.PutText(wrServer, request.postData);
          END;
        FINALLY
          (* SCG/MSM 2/22/96 A CERN proxy gets confused if we close the wr
             while still using the rd.

             Symptoms:

             Sending large files though the cern proxy fail partway
             through.  The reader blocks on a read and the socket is in
             FIN_WAIT_2. *)
          Wr.Flush(wrServer);
        END;
        WITH reply = NEW(HTTP.Reply).parse(rdServer, log) DO
          IF App.Verbose() THEN
            log.log(reply.toText(NIL, log), App.LogStatus.Verbose);
          END;
          handler.reply(reply, rdServer, wrClient, log);
        END;
      FINALLY
        Wr.Close(wrServer);
        Rd.Close(rdServer);
        TCP.Close(conn);
      END;
    EXCEPT
    | Rd.Failure, Thread.Alerted, Wr.Failure =>
        log.log(Fmt.F("error get url: %s", request.url.toText()),
                App.LogStatus.Error);
    END;
  END Client;

PROCEDURE RegisterRequestHandler(port: INTEGER; handler: RequestHandler) =
  BEGIN
    WriteLock();
    TRY
      handler.port := port;
      requestHandlerList := NEW(RequestHandlerList, head := handler,
                                tail := requestHandlerList);
    FINALLY
      WriteUnlock();
    END;
  END RegisterRequestHandler;

PROCEDURE ServerPushSupported(request: HTTP.Request): BOOLEAN =
  VAR
    field := request.lookupField("User-Agent");
    trd: TextRd.T;
    agent: TEXT;
  BEGIN
    TRY
      IF field # NIL THEN
        trd := TextRd.New(field.value);
        agent := Lex.Scan(trd, NonSlash);
        IF Text.Equal(agent, "Mozilla") THEN RETURN TRUE END;
      END;
    EXCEPT
    | Rd.Failure, Thread.Alerted =>
    END;
    RETURN FALSE;
  END ServerPushSupported;

PROCEDURE ServerPushFrame(wr: Wr.T; contentType, msg: TEXT; log: App.Log) RAISES {App.Error} =
  BEGIN
    TRY
      IF App.Verbose() THEN
        log.log(Fmt.F("Serverpush: %s", msg), App.LogStatus.Verbose);
      END;
      Wr.PutText(wr, ServerPushBoundaryStringStart);
      Wr.PutText(wr, Fmt.F("Content-type: %s\r\nContent-length: %s\r\n\r\n",
                           contentType, Fmt.Int(Text.Length(msg))));
      Wr.PutText(wr, msg);
      Wr.Flush(wr);
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        IF App.Debug() THEN
           log.log(Fmt.F("Failure writing \"%s\" server push frame", msg),
                   App.LogStatus.Debug);
        END;
    END;
  END ServerPushFrame;

VAR
  proxy: Proxy := NEW(Proxy).init();

PROCEDURE DefaultProxy(<* UNUSED *> log: App.Log): Proxy =
  BEGIN
    RETURN proxy;
  END DefaultProxy;

TYPE
  Arg = {ProxyServer};

  ArgHandler = App.ArgHandler OBJECT
  OVERRIDES
    set := SetArg;
  END;
  
PROCEDURE SetArg (            self : ArgHandler;
                  <*UNUSED *> src  : App.ArgSource;
                              value: TEXT;
                              log  : App.Log        ) RAISES {App.Error} =
  BEGIN
    CASE VAL(self.id, Arg) OF
    | Arg.ProxyServer =>
        IF Text.Length(value) > 0 THEN proxy.add(value, log); END;
    END;
  END SetArg;

BEGIN
  EVAL NEW(ArgHandler, id := ORD(Arg.ProxyServer), 
           paramName := "pattern ProxyServer:port|DIRECT",
           default := "").init(
           switchName := "proxy", envName := "HTTP_PROXY", 
           configName := "proxy");
END HTTPApp.
