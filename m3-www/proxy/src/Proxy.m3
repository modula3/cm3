MODULE Proxy EXPORTS Main;

IMPORT App, FloatMode, Fmt, HTTP, HTTPApp, IO, Lex, ProxyBundle, Rd, 
       RdCopy, Rsrc, Text, TextRd, TextWr, Thread, Wr;

VAR
  port    : INTEGER;
  reject               := FALSE;
  rsrcPath: Rsrc.Path;

TYPE
  RequestHandler = HTTPApp.RequestHandler OBJECT
  OVERRIDES
    request := Request;
    accept := Accept;
  END;

PROCEDURE Accept (<* UNUSED *>     self       : RequestHandler;
                  <* UNUSED *>     request    : HTTP.Request;
                  <* UNUSED *>     serverData : REFANY;
                  <* UNUSED *> VAR acceptState: REFANY;
                  <* UNUSED *>     log        : App.Log         ):
  BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Accept;

PROCEDURE Request (<* UNUSED *> self                   : RequestHandler;
                                request                : HTTP.Request;
                   <* UNUSED *> serverData, acceptState: REFANY;
                                rd                     : Rd.T;
                                wr                     : Wr.T;
                                log                    : App.Log         )
  RAISES {App.Error} =
  BEGIN
    IF App.Verbose() THEN
      log.log(Fmt.F("Proxy request headers: %s",
                    request.toText(
                      HTTP.DefaultStyle(request.version), TRUE, log)),
              App.LogStatus.Verbose);
    ELSIF App.Debug() THEN
      log.log(
        Fmt.F("Proxy request: %s %s", HTTP.MethodText[request.method],
              request.url.toText()), App.LogStatus.Debug);
    END;
    IF reject THEN
      (* This is an MSM idea to have a proxy to reject add requests... *)
      HTTP.WriteSimpleReplyHeader(
        wr, NIL, log, HTTP.StatusCode[HTTP.StatusType.Forbidden],
        "Rejected by design");
      TRY
        Wr.PutText(wr, "Content-Type: image/gif\r\n\r\n");
        Wr.PutText(wr, Rsrc.Get("noads.gif", rsrcPath));
      EXCEPT
      | Rd.Failure, Rsrc.NotFound, Wr.Failure, Thread.Alerted =>
      END;
      RETURN;
    END;
    IF request.method = HTTP.Method.Post AND request.postData # NIL THEN
      IF App.Verbose() THEN
        log.log(request.postData, App.LogStatus.Verbose);
      END;
      rd := TextRd.New(request.postData);
    END;
    HTTPApp.Client(
      request, HTTPApp.DefaultProxy(), HTTP.DefaultStyle(request.version),
      rd, wr, NEW(ReplyHandler, request := request), port, log);
  END Request;

TYPE
  ReplyHandler = HTTPApp.ReplyHandler OBJECT
    request: HTTP.Request;
  OVERRIDES
    reply := Reply;
  END;

PROCEDURE Reply (<* UNUSED *> self : ReplyHandler;
                              reply: HTTP.Reply;
                              rd   : Rd.T;
                              wr   : Wr.T;
                              log  : App.Log       ) RAISES {App.Error} =
  VAR
    twr               : TextWr.T;
    rep               : TEXT;
    contentLengthField: HTTP.Field;
    length            : CARDINAL   := LAST(CARDINAL);
  BEGIN
    IF App.Verbose() THEN
      log.log(Fmt.F("Proxy reply headers: %s",
                    reply.toText(HTTP.DefaultStyle(reply.version), log)),
              App.LogStatus.Verbose);

    ELSIF App.Debug() THEN
      log.log(
        Fmt.F("Proxy reply: %s %s", Fmt.Int(reply.code), reply.reason),
        App.LogStatus.Debug);
    END;
    reply.write(wr, HTTP.DefaultStyle(reply.version), log);
    contentLengthField :=
      reply.lookupField(HTTP.FieldName[HTTP.FieldType.Content_Length]);
    TRY
      IF contentLengthField # NIL THEN
        TRY
          length := IO.GetInt(TextRd.New(contentLengthField.value));
        EXCEPT
        | IO.Error =>
            log.log("Proxy error copying request\n", App.LogStatus.Error);
        END;
      END;

      IF App.Verbose() THEN
        twr := TextWr.New();
        EVAL RdCopy.ToWriter(rd, twr, length);
        rep := TextWr.ToText(twr);
        log.log(rep, App.LogStatus.Verbose);
        Wr.PutText(wr, rep);
      ELSE
        EVAL RdCopy.ToWriter(rd, wr, length);
      END;
    EXCEPT
      Rd.Failure, Thread.Alerted, Wr.Failure =>
        log.log("Proxy error copying reply\n", App.LogStatus.Error);
    END;
  END Reply;

TYPE
  Arg = {Port, Pac, Reject};

TYPE
  ArgHandler = App.ArgHandler OBJECT
  OVERRIDES
    set := SetArg;
  END;

PROCEDURE SetArg (             self : ArgHandler;
                  <* UNUSED *> src  : App.ArgSource;
                               value: TEXT;
                               log  : App.Log        ) RAISES {App.Error} =
  BEGIN
    CASE VAL(self.id, Arg) OF
    | Arg.Port =>
        TRY
          port := Lex.Int(TextRd.New(value));
        EXCEPT
        | Rd.Failure, Thread.Alerted, Lex.Error, FloatMode.Trap =>
            log.log(Fmt.F("Bad port: %s", value), App.LogStatus.Error);
        END;
    | Arg.Reject => reject := Text.Equal(value, "TRUE");
    ELSE                         <* ASSERT FALSE *>
    END;
  END SetArg;

BEGIN
  rsrcPath :=  Rsrc.BuildPath("$PROXYPATH", ProxyBundle.Get());
  EVAL NEW(ArgHandler, id := ORD(Arg.Reject), hasParam:= FALSE).init(
                                     switchName := "reject");
  EVAL NEW(ArgHandler, id := ORD(Arg.Port), paramName := "port",
           default := "8888").init(switchName := "port", register := TRUE);
  TRY
    App.InitializeArguments(logConfiguration := FALSE);
    HTTPApp.RegisterRequestHandler(port, NEW(RequestHandler));
    HTTP.SetProgramInfo(
      HTTP.ProgramInfo{
        type := HTTP.ProgramType.Proxy, name := "Chatty_Client_Proxy/1.0"});
    HTTP.SetDefaultViaFieldValue(HTTP.CurrentVersion, port);

    HTTPApp.Serve(port, port);
  EXCEPT
  | App.Error =>
  END;
END Proxy.
