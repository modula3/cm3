(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Mon Aug 12 21:01:24 PDT 1996 by steveg *)

MODULE HTTPProxy;

IMPORT App, Fmt, HTTP, HTTPApp, Rd, RdCopy, Thread, Wr;


REVEAL
  RequestHandler = RequestHandlerPublic BRANDED OBJECT
  OVERRIDES
    request := Request;
    accept := Accept;
    replyHandlerGenerator := ReplyHandlerGenerator;
  END;

PROCEDURE ReplyHandlerGenerator(<* UNUSED *> self: RequestHandler;
                                     <* UNUSED *> request: HTTP.Request; 
                                     <* UNUSED *> serverData, acceptState: REFANY;
                                     <* UNUSED *> rd: Rd.T;  
                                     <* UNUSED *> wr: Wr.T;
                                     <* UNUSED *> log: App.Log): HTTPApp.ReplyHandler =
  BEGIN
    RETURN NEW(ReplyHandler);
  END ReplyHandlerGenerator;

PROCEDURE Accept(<* UNUSED *> self: RequestHandler;
                 <* UNUSED *> request: HTTP.Request;
                 <* UNUSED *> serverData: REFANY;
                 <* UNUSED *> VAR acceptState: REFANY;
                 <* UNUSED *> log: App.Log): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Accept;

PROCEDURE Request (self                   : RequestHandler;
                   request                : HTTP.Request;
                   serverData, acceptState: REFANY;
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
    HTTPApp.Client(
      request, HTTPApp.DefaultProxy(), HTTP.DefaultStyle(request.version),
      rd, wr, self.replyHandlerGenerator(
                request, serverData, acceptState, rd, wr, log), log);
  END Request;

REVEAL
  ReplyHandler = HTTPApp.ReplyHandler BRANDED OBJECT
  OVERRIDES
    reply := Reply;
  END;

PROCEDURE Reply (<* UNUSED *> self : HTTPApp.ReplyHandler;
                              reply: HTTP.Reply;
                              rd   : Rd.T;
                              wr   : Wr.T;
                              log  : App.Log               )
  RAISES {App.Error} =
  BEGIN
    reply.write(wr, HTTP.DefaultStyle(reply.version), log);
    TRY
      EVAL RdCopy.ToWriter(rd, wr);
    EXCEPT
      Rd.Failure, Thread.Alerted, Wr.Failure =>
        RAISE App.Error("Proxy error copying reply\n");
    END;
  END Reply;


BEGIN
END HTTPProxy.
