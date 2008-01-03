(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* created by steveg *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 14:50:11 1997
 *)

MODULE HTTPPayment;

IMPORT App, Fmt, HTTP, TextExtras;

TYPE
  PF = {PaymentProtocol, PaymentCash, PaymentAuthorization, PaymentBid,
        PaymentReceipt, PaymentOffer, PaymentError, NotAPaymentField};

PROCEDURE PaymentField(field: HTTP.Field): PF =
  BEGIN
    IF TextExtras.CIEqual(field.name, ProtocolField) THEN
      RETURN PF.PaymentProtocol
    ELSIF TextExtras.CIEqual(field.name, CashField) THEN
      RETURN PF.PaymentCash
    ELSIF TextExtras.CIEqual(field.name, AuthorizationField) THEN
      RETURN PF.PaymentAuthorization
    ELSIF TextExtras.CIEqual(field.name, BidField) THEN
      RETURN PF.PaymentBid
    ELSIF TextExtras.CIEqual(field.name, ReceiptField) THEN
      RETURN PF.PaymentReceipt
    ELSIF TextExtras.CIEqual(field.name, OfferField) THEN
      RETURN PF.PaymentOffer
    ELSIF TextExtras.CIEqual(field.name, ErrorField) THEN
      RETURN PF.PaymentError
    ELSE
      RETURN PF.NotAPaymentField
    END;
  END PaymentField;

PROCEDURE AppendValue(VAR (* in/out *) text: TEXT; value: TEXT) =
  BEGIN
    IF text = NIL THEN text := value ELSE text := text & ", " & value END;
  END AppendValue;

REVEAL
  Reply = ReplyPublic BRANDED "HTTPPayment.Reply" OBJECT
  OVERRIDES
    init := InitReply;
    toReply := ToReply;
  END;

PROCEDURE InitReply (self : Reply;
                     reply: HTTP.Reply;
                     url  : TEXT;
                     log  : App.Log     ): Reply RAISES {App.Error} =
  VAR
    iterator := reply.iterateFields();
    field    := iterator.next();
  BEGIN
    self.reply := reply;
    self.url := url;
    WHILE field # NIL DO
      CASE PaymentField(field) OF
      | PF.PaymentProtocol => AppendValue(self.protocols, field.value);
      | PF.PaymentCash => AppendValue(self.cash, field.value);
      | PF.PaymentAuthorization =>
          AppendValue(self.authorization, field.value);
      | PF.PaymentReceipt => AppendValue(self.receipt, field.value);
      | PF.PaymentOffer => AppendValue(self.offer, field.value);
      | PF.PaymentError => AppendValue(self.error, field.value);
      | PF.PaymentBid =>
          log.log(
            Fmt.F("Illegal payment (field %s: %s) in reply: %s",
                  field.name, field.value,
                  reply.toText(HTTP.DefaultStyle(reply.version), log)),
            App.LogStatus.Error);
      | PF.NotAPaymentField =>
      END;
      field := iterator.next();
    END;
    RETURN self;
  END InitReply;

PROCEDURE ToReply (self: Reply; <*UNUSED*>log: App.Log): HTTP.Reply
  RAISES {(*App.Error*)} =
  VAR hReply := NEW(HTTP.Reply, code := self.reply.code,
                    reason := self.reply.reason);
  BEGIN
    self.reply.copyFields(hReply);
    EVAL
      hReply.addField(NEW(HTTP.Field).init(ProtocolField, self.protocols));
    IF self.cash # NIL THEN
      EVAL hReply.addField(NEW(HTTP.Field).init(CashField, self.cash));
    END;
    IF self.authorization # NIL THEN
      EVAL hReply.addField(
             NEW(HTTP.Field).init(AuthorizationField, self.authorization));
    END;
    IF self.receipt # NIL THEN
      EVAL
        hReply.addField(NEW(HTTP.Field).init(ReceiptField, self.receipt));
    END;
    IF self.offer # NIL THEN
      EVAL hReply.addField(NEW(HTTP.Field).init(OfferField, self.offer));
    END;
    IF self.error # NIL THEN
      EVAL hReply.addField(NEW(HTTP.Field).init(ErrorField, self.error));
    END;
    self.reply := hReply;
    RETURN hReply;
  END ToReply; 

PROCEDURE IsPaymentReply(reply: HTTP.Reply): BOOLEAN =
  BEGIN
    RETURN reply.lookupField(ProtocolField) # NIL;
  END IsPaymentReply;

REVEAL
  Request = RequestPublic BRANDED "HTTPPayment.Request" OBJECT
  OVERRIDES
    init := InitRequest;
    toRequest := ToRequest;
  END;

PROCEDURE InitRequest (self: Request; request: HTTP.Request; log: App.Log):
  Request RAISES {App.Error} =
  VAR
    iterator := request.iterateFields();
    field    := iterator.next();
  BEGIN
    self.request := request;
    self.vendorName :=
      Fmt.F("%s:%s", request.url.host, Fmt.Int(request.url.port));
    WHILE field # NIL DO
      CASE PaymentField(field) OF
      | PF.PaymentProtocol => AppendValue(self.protocols, field.value);
      | PF.PaymentCash => AppendValue(self.cash, field.value);
      | PF.PaymentAuthorization =>
          AppendValue(self.authorization, field.value);
      | PF.PaymentBid => AppendValue(self.bid, field.value);
      | PF.PaymentReceipt, PF.PaymentOffer, PF.PaymentError =>
          log.log(Fmt.F("Illegal payment (field %s: %s) in request: %s",
                        field.name, field.value,
                        request.toText(
                          HTTP.DefaultStyle(request.version), TRUE, log)),
                  App.LogStatus.Error);
      | PF.NotAPaymentField =>
      END;
      field := iterator.next();
    END;
    RETURN self;
  END InitRequest;

PROCEDURE ToRequest (self: Request; <* UNUSED *> log: App.Log): HTTP.Request =
  VAR
    req := NEW(HTTP.Request, method := self.request.method,
               url := self.request.url, postData := self.request.postData);
  BEGIN
    self.request.copyFields(req);
    IF self.protocols # NIL THEN
      EVAL
        req.addField(NEW(HTTP.Field).init(ProtocolField, self.protocols));
    END;
    IF self.cash # NIL THEN
      EVAL req.addField(NEW(HTTP.Field).init(CashField, self.cash));
      EVAL req.addField(
             NEW(HTTP.Field).init(AuthorizationField, self.authorization));
    END;
    IF self.bid # NIL THEN
      EVAL req.addField(NEW(HTTP.Field).init(BidField, self.bid));
    END;
    RETURN req;
  END ToRequest;

TYPE
  ProtocolHandlerList = OBJECT
    head: ProtocolHandler;
    tail: ProtocolHandlerList;
  END;

VAR
  protocolHandlers: ProtocolHandlerList;
  mu := NEW(MUTEX);

PROCEDURE ParseRequest (request: HTTP.Request; log: App.Log): Request
  RAISES {App.Error} =
  VAR
    handlers: ProtocolHandlerList;
    field                         := request.lookupField(ProtocolField);
  BEGIN
    IF field = NIL THEN
      handlers := NIL;
      IF App.Verbose() THEN
        log.log(Fmt.F("Not a payment request: %s",
                      request.toText(
                        HTTP.DefaultStyle(request.version), TRUE, log)),
                App.LogStatus.Verbose);
      END;
    ELSE
      LOCK mu DO
        handlers := protocolHandlers;
        WHILE handlers # NIL AND NOT handlers.head.accept(field.value) DO
          handlers := handlers.tail;
        END;
      END;
    END;

    IF handlers # NIL THEN
      RETURN handlers.head.parseRequest(request, log);
    ELSE
      RETURN NEW(Request).init(request, log);
    END;
  END ParseRequest;

PROCEDURE ParseReply (reply: HTTP.Reply; request: Request; log: App.Log):
  Reply RAISES {App.Error} =
  VAR
    handlers: ProtocolHandlerList;
    field                         := reply.lookupField(ProtocolField);
  BEGIN
    IF field = NIL THEN
      log.log(Fmt.F("Not a payment reply: %s",
                    reply.toText(HTTP.DefaultStyle(reply.version), log)),
              App.LogStatus.Error);
    END;
    LOCK mu DO
      handlers := protocolHandlers;
      WHILE handlers # NIL AND NOT handlers.head.accept(field.value) DO
        handlers := handlers.tail;
      END;
    END;

    IF handlers # NIL THEN
      RETURN handlers.head.parseReply(reply, request, log);
    ELSE
      RETURN NEW(Reply).init(reply, request.request.url.toText(), log);
    END;
  END ParseReply;

PROCEDURE RegisterProtocolHandler(handler: ProtocolHandler) =
  BEGIN
    LOCK mu DO
      protocolHandlers := NEW(ProtocolHandlerList, head := handler, 
                              tail := protocolHandlers);
    END;
  END RegisterProtocolHandler;

BEGIN
END HTTPPayment.
