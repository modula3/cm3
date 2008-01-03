(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Created by steveg *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 14:49:57 1997
 *)

MODULE HTTPProxy;

IMPORT App, Fmt, HTTP, HTTPApp, IO, Rd, RdCopy, TextRd, TextWr, Thread, Wr;


REVEAL
  RequestHandler = RequestHandlerPublic BRANDED
  "HTTPProxy.RequestHandler" OBJECT
  OVERRIDES
    request := Request;
    accept := Accept;
    replyHandlerGenerator := ReplyHandlerGenerator;
  END;

PROCEDURE ReplyHandlerGenerator (             self   : RequestHandler;
                                 <* UNUSED *> request: HTTP.Request;
                                 <* UNUSED *> serverData, acceptState: REFANY;
                                 <* UNUSED *> rd : Rd.T;
                                 <* UNUSED *> wr : Wr.T;
                                 <* UNUSED *> log: App.Log):
  HTTPApp.ReplyHandler =
  BEGIN
    RETURN NEW(ReplyHandler, logReply := self.logReply);
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
    IF request.method = HTTP.Method.Post AND request.postData # NIL THEN
      IF self.logRequest THEN
        log.log(request.postData, App.LogStatus.Verbose);
      END;
      rd := TextRd.New(request.postData);
    END;
    HTTPApp.Client(
      request, HTTPApp.DefaultProxy(), HTTP.DefaultStyle(request.version),
      rd, wr, self.replyHandlerGenerator(
                request, serverData, acceptState, rd, wr, log),
      self.service, log);
  END Request;

REVEAL
  ReplyHandler = HTTPApp.ReplyHandler BRANDED "HTTPProxy.ReplyHandler" OBJECT
    logReply: BOOLEAN;
  OVERRIDES
    reply := Reply;
  END;

PROCEDURE Reply (self : ReplyHandler;
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
      log.log(
        Fmt.F("Proxy reply headers: %s",
              reply.toText(HTTP.DefaultStyle(reply.version), log)),
        App.LogStatus.Verbose);

    ELSIF App.Debug() THEN
      log.log(Fmt.F("Proxy reply: %s %s", Fmt.Int(reply.code), reply.reason),
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

      IF self.logReply THEN
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

BEGIN
END HTTPProxy.
