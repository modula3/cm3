(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Sun Aug 18 20:17:44 PDT 1996 by steveg *)

MODULE HTTPApp;

<* PRAGMA LL *>

IMPORT App, ConnRW, FloatMode, Fmt, FmtTime, HTTP, 
       IntList, IO, IP, Lex, Rd, RdCopy, RdUtils, TCP, TCPPeer,
       Text, TextList, TextRd, Thread, Time, Wr;

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
        ServerPushFrame(
          self.wr, "text/plain", self.msg, self.appLog);
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
    RETURN NEW(ClientLog, appLog := old, wr := wr, msg := "",
               serverPush := serverPush);
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
  RequestHandler = RequestHandlerPublic BRANDED OBJECT
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
  ReplyHandler = ReplyHandlerPublic BRANDED OBJECT
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
  Proxy = ProxyPublic BRANDED OBJECT
  OVERRIDES
    init := InitProxy;
    addProxy := AddProxy;
  END;

PROCEDURE InitProxy(self: Proxy; noProxy: TEXT): Proxy =
  BEGIN
    self.noProxy := ParseNoProxy(noProxy);
    RETURN self;
  END InitProxy;

PROCEDURE AddProxy(self: Proxy; server: Server) =
  VAR
    new := NEW(ServerList, head := server);
    proxies: ServerList;
  BEGIN
    IF self.proxies = NIL THEN
      self.proxies := new;
    ELSE
      proxies := self.proxies;
      WHILE proxies.tail # NIL DO proxies := proxies.tail; END;
      proxies.tail := new;
    END;
  END AddProxy;

REVEAL
  Server = ServerPublic BRANDED OBJECT
  OVERRIDES
    init := InitServer;
    initParse := InitParseServer;
  END;

PROCEDURE InitServer(self: Server; 
                    server: TEXT; 
                    port: INTEGER; 
                    log: App.Log): Server RAISES {App.Error} =
  VAR
    addr: IP.Address;
  BEGIN
    self.server := server;
    self.port := port;

    TRY
      IF NOT IP.GetHostByName(server, addr) THEN
        log.log(Fmt.F("Cannot find %s", server), App.LogStatus.Error);
      END;
    EXCEPT
    | IP.Error => log.log(Fmt.F("Cannot find %s", server), App.LogStatus.Error);
    END;
    self.endpoint :=  IP.Endpoint{addr := addr, port := port};
    RETURN self;
  END InitServer;

CONST 
  NonColon = SET OF CHAR{'\000'..'\377'} - SET OF CHAR{':'};
  NonSlash = SET OF CHAR{'\000'..'\377'} - SET OF CHAR{'/'};

PROCEDURE InitParseServer(self: Server; 
                               serverAndPort: TEXT; 
                               log: App.Log): Server RAISES {App.Error} =
  VAR
    server: TEXT;
    port := 0;
  BEGIN
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
      | Lex.Error, FloatMode.Trap, Rd.EndOfFile, Rd.Failure, Thread.Alerted =>
          log.log(Fmt.F("Bad server given: %s (need server:port)", 
                      server), App.LogStatus.Error);
      END;
    END;
    RETURN self.init(server, port, log);
  END InitParseServer;

CONST 
  NonComma = SET OF CHAR{'\000'..'\377'} - SET OF CHAR{','};

PROCEDURE ParseNoProxy(t: TEXT): TextList.T =
  VAR
    rd := TextRd.New(t);
    res: TextList.T;
    item: TEXT;
  BEGIN
    TRY
      LOOP
        item := Lex.Scan(rd, NonComma);
        IF Text.Equal(item, "") THEN RETURN res; END;
        res := NEW(TextList.T, head := item, tail := res);
        IF Rd.EOF(rd) THEN EXIT END;
        EVAL(Rd.GetChar(rd));
      END;
    EXCEPT
    | Rd.Failure, Thread.Alerted =>
        <* ASSERT FALSE *>
    | Rd.EndOfFile => 
    END;
    RETURN res;
  END ParseNoProxy;

TYPE
  RequestHandlerList = REF RECORD
    head: RequestHandler;
    tail: RequestHandlerList;
  END;

VAR
  requestHandlerList: RequestHandlerList := NIL;

PROCEDURE FindRequestHandler(request: HTTP.Request; 
                             serverData: REFANY;
                             VAR (* OUT *) acceptState: REFANY;
                             log: App.Log): RequestHandler
  RAISES {App.Error} =
  VAR
    list: RequestHandlerList;
  BEGIN
    ReadLock();
    TRY
      FOR i := FIRST(RequestPriority) TO LAST(RequestPriority) DO
        list := requestHandlerList;
        WHILE list # NIL AND (list.head.priority # i OR
              NOT list.head.accept(request, serverData, acceptState, log)) DO
          list := list.tail;
        END;
        IF list # NIL THEN RETURN list.head END;
      END;
    FINALLY
      ReadUnlock();
    END;
    RETURN NIL;
  END FindRequestHandler;

TYPE
  Closure = Thread.Closure OBJECT
    client: TCP.T;
    log: App.Log;
    wrLog: BOOLEAN;
    data: REFANY;
  OVERRIDES
    apply := ServerHandler;
  END;

PROCEDURE ServerHandler (cl: Closure): REFANY =
  VAR
    rd            : Rd.T;
    wr            : Wr.T;
    request       : HTTP.Request;
    requestHandler: RequestHandler;
    acceptState   : REFANY;
    clientName    : TEXT := "";
    clientIPAddr  : IP.Endpoint;
  BEGIN
    TRY
      TRY
        rd := ConnRW.NewRd(cl.client);
        wr := ConnRW.NewWr(cl.client);

	(* Obtain a best representation of the client name.
           If we blow up with an IP.Error, then we do not
           wish to continue this connection, because we
           don't know who we are talking to.  More graceful
           failure would be nice, but I don't understand this
           failure mode anyway.  -- Dave Chase *) 

        clientIPAddr := TCPPeer.Get(cl.client);
        clientName := TCPPeer.GetName(cl.client);

        IF clientName = NIL THEN
	  (* No translation, format as "a.b.c.d:port" *)
          clientName := Fmt.Int(clientIPAddr.addr.a[0])
            & "." & Fmt.Int(clientIPAddr.addr.a[1])
            & "." & Fmt.Int(clientIPAddr.addr.a[2])
            & "." & Fmt.Int(clientIPAddr.addr.a[3])
            & ":" & Fmt.Int(clientIPAddr.port);
        ELSE
          (* Format as "this.that=(a.b.c.d:port)" *)
          clientName :=   
              clientName & "=(" & Fmt.Int(clientIPAddr.addr.a[0]) 
              & "." & Fmt.Int(clientIPAddr.addr.a[1])
              & "." & Fmt.Int(clientIPAddr.addr.a[2])
              & "." & Fmt.Int(clientIPAddr.addr.a[3])
              & ":" & Fmt.Int(clientIPAddr.port)
              & ")";
        END;

        IF cl.wrLog THEN cl.log := WrLog(cl.log, wr, FALSE); END;

        request := NEW(HTTP.Request, client := clientName).parse(rd, cl.log);

        IF App.Verbose() THEN
          cl.log.log(request.toText(NIL, TRUE, cl.log), App.LogStatus.Verbose);
        END;

        cl.log.log(
          Fmt.F("%s %s %s", FmtTime.Short(Time.Now()),
                clientName, request.url.toText()),
          App.LogStatus.Debug);

        requestHandler :=
          FindRequestHandler(request, cl.data, acceptState, cl.log);
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
        TCP.Close(cl.client);
      END;
    EXCEPT
    | App.Error =>
    | IP.Error =>
    END;
    RETURN NIL;
  END ServerHandler;

VAR
  serverPorts: IntList.T := NIL; <* LL = readWriteMu *>

PROCEDURE ServerPort (port: INTEGER): BOOLEAN =
  BEGIN
    LOCK readWriteMu DO RETURN IntList.Member(serverPorts, port); END;
  END ServerPort;

PROCEDURE Serve (port: INTEGER; log: App.Log; data: REFANY)
  RAISES {App.Error} =
  VAR
    conn     : TCP.T;
    connector: TCP.Connector;
    nilLog                   := log = NIL;
  BEGIN
    LOCK readWriteMu DO
      serverPorts := IntList.Cons(port, serverPorts);
    END;

    IF log = NIL THEN log := App.defaultLog END;
    TRY
      connector := TCP.NewConnector(
                     IP.Endpoint{addr := IP.GetHostAddr(), port := port});
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
        EVAL Thread.Fork(NEW(Closure, client := conn, log := log,
                             wrLog := nilLog, data := data));
      EXCEPT
      | IP.Error, Thread.Alerted => (* continue *)
      END;
    END;
  END Serve;

PROCEDURE NoProxyRequest (             request: HTTP.Request;
                                       proxy  : Proxy;
                          <* UNUSED *> log    : App.Log       ): BOOLEAN =
  VAR
    suffix   : TextList.T;
    len, sLen: INTEGER;
  BEGIN 
    len := Text.Length(request.url.host);
    IF len = 0 OR request.url.local() THEN RETURN FALSE END;

    suffix := proxy.noProxy;
    WHILE suffix # NIL DO
      sLen := Text.Length(suffix.head);
      IF len >= sLen
           AND Text.Equal(suffix.head,
                          Text.Sub(request.url.host, len - sLen, sLen)) THEN
        RETURN TRUE
      END;
      suffix := suffix.tail;
    END;
    RETURN FALSE;
  END NoProxyRequest;

PROCEDURE Client (request : HTTP.Request;
                  proxy   : Proxy;
                  style   : HTTP.Style;
                  rdClient: Rd.T;
                  wrClient: Wr.T;
                  handler : ReplyHandler;
                  log     : App.Log       ) RAISES {App.Error} =
  VAR
    server            : Server;
    conn              : TCP.T;
    wrServer          : Wr.T;
    rdServer          : Rd.T;
    proxyRequest      : BOOLEAN;
    contentLengthField: HTTP.Field;
  BEGIN
    IF log = NIL THEN log := App.defaultLog END;
    IF style = NIL THEN style := HTTP.DefaultStyle() END;
    IF App.Verbose() THEN
      log.log(request.toText(style, TRUE, log), App.LogStatus.Verbose);
    END;

    IF Text.Length(request.url.host) = 0 THEN
      log.log(Fmt.F("No server given in requested URL: %s",
                    request.url.toText()), App.LogStatus.Error);
    END;
    IF NoProxyRequest(request, proxy, log) THEN
      proxyRequest := FALSE;
      server :=
        NEW(Server).init(request.url.host, request.url.port, log);
      TRY
        conn := TCP.Connect(server.endpoint);
      EXCEPT
      | IP.Error, Thread.Alerted =>
          log.log(Fmt.F("Unable to connect to server: %s:%s",
                        server.server, Fmt.Int(server.port)), App.LogStatus.Error);
      END;
    ELSE
      proxyRequest := TRUE;
      VAR proxies := proxy.proxies;
      BEGIN
        WHILE proxies # NIL DO
          IF App.Debug() AND proxies # proxy.proxies THEN
            log.log(Fmt.F("rolling over to proxy server: %s",
                          proxies.head.server), App.LogStatus.Debug);
          END;
          TRY
            conn := TCP.Connect(proxies.head.endpoint);
            EXIT;
          EXCEPT
          | IP.Error, Thread.Alerted =>
              log.log(Fmt.F("Unable to connect to proxy server: %s",
                            proxies.head.server), App.LogStatus.Status);
          END;
          proxies := proxies.tail;
        END;
        IF proxies = NIL THEN
          log.log(
            "Unable to connect to any proxy server", App.LogStatus.Error);
        END;
      END;
    END;

    wrServer := ConnRW.NewWr(conn);
    rdServer := ConnRW.NewRd(conn);
    TRY
      TRY
        TRY
          request.write(wrServer, style, proxyRequest, log);
          IF rdClient # NIL AND request.method = HTTP.Method.Post THEN
            contentLengthField :=
              request.lookupField(HTTP.FieldName[HTTP.FieldType.Content_Length]);
            TRY
              IF contentLengthField # NIL THEN
                EVAL RdCopy.ToWriter(
                       rdClient, wrServer,
                       IO.GetInt(TextRd.New(contentLengthField.value)));
              END;
            EXCEPT
            | IO.Error =>
            END;
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

PROCEDURE RegisterRequestHandler(handler: RequestHandler) =
  BEGIN
    WriteLock();
    TRY
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

PROCEDURE PostRd (request: HTTP.Request; rd: Rd.T; log: App.Log): Rd.T
  RAISES {App.Error} =
  VAR
    field := request.lookupField(
               HTTP.FieldName[HTTP.FieldType.Content_Length]);
  BEGIN
    IF field = NIL THEN
      log.log(Fmt.F("No \"Content-Length\" field in request: %s",
                    request.toText(
                      HTTP.DefaultStyle(request.version), TRUE, log)),
              App.LogStatus.Error);
    END;
    TRY
      RETURN TextRd.New(Rd.GetText(rd, Lex.Int(TextRd.New(field.value))));
    EXCEPT
    | Rd.Failure, Thread.Alerted, Lex.Error, FloatMode.Trap =>
        log.log(Fmt.F("Error getting POST arguments for request: %s",
                      request.toText(
                        HTTP.DefaultStyle(request.version), TRUE, log)),
                App.LogStatus.Error);
        RETURN NIL;
    END;
  END PostRd;

VAR
  proxy: Proxy := NEW(Proxy);

PROCEDURE DefaultProxy(<* UNUSED *> log: App.Log): Proxy =
  BEGIN
    RETURN proxy;
  END DefaultProxy;

TYPE
  Arg = {ProxyServer, NoProxy};

  ArgHandler = App.ArgHandler OBJECT
  OVERRIDES
    set := SetArg;
  END;
  
PROCEDURE SetArg(self: ArgHandler; 
                 <*UNUSED *> src: App.ArgSource; 
                 value: TEXT; log: App.Log) RAISES {App.Error} =
  BEGIN
    CASE VAL(self.id, Arg) OF
    | Arg.ProxyServer =>
        IF NOT Text.Equal(value, "") THEN
          proxy.addProxy(NEW(Server).initParse(value, log));
        END;
    | Arg.NoProxy =>
         IF NOT Text.Equal (value, "") THEN
           proxy.noProxy := ParseNoProxy(value);
         END;
    END;
  END SetArg;

BEGIN

  (* default was "www-proxy.pa.dec.com:8080" *)
  EVAL NEW(ArgHandler, id := ORD(Arg.ProxyServer), 
           paramName := "ProxyServer:port",
           default := "").init(
           switchName := "proxy", envName := "http_proxy", 
           configName := "proxy");

  (* default was ".dec.com" *)
  EVAL NEW(ArgHandler, id := ORD(Arg.NoProxy), 
           paramName := "noProxy1[,noProxy2]*",
           default := "").init(
           switchName := "noProxy", envName := "no_proxy", 
           configName := "noProxy");
END HTTPApp.
