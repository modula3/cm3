(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Fri Feb 23 10:57:42 PST 1996 by steveg *)

INTERFACE HTTPPayment;

<* PRAGMA LL *>

IMPORT
  App, HTTP;

CONST
  ProtocolField = "Payment-Protocol";
  CashField = "Payment-Cash";
  AuthorizationField = "Payment-Authorization";
  BidField = "Payment-Bid";
  ReceiptField = "Payment-Receipt";
  OfferField = "Payment-Offer";
  ErrorField = "Payment-Error";

CONST
  InsufficientPaymentCode = 402;
  BadPaymentCode = 420;
  UnknownPaymentProtocolCode = 421;
  PaymentRejectedCode = 422;

  InsufficientPaymentReason = "InsufficientPayment";
  BadPaymentReason = "BadPayment";
  UnknownPaymentProtocolReason = "UnknownPaymentProtocol";
  PaymentRejectedReason = "PaymentRejected";

TYPE
  Request <: RequestPublic;
  RequestPublic = OBJECT
    vendorName: TEXT; (* server:port from "url" *)
    protocols, cash, authorization, bid: TEXT := NIL;
    (* value fields of the respective header fields in a payment
       request.  NIL if not present *)
    request: HTTP.Request;
  METHODS
    init(req: HTTP.Request; log: App.Log): Request RAISES {App.Error};
    toRequest(log: App.Log): HTTP.Request RAISES {App.Error}; 
    (* returns a request from current value of fields *)
  END;

TYPE
  Reply <: ReplyPublic;
  ReplyPublic = OBJECT
    url: TEXT;
    reply: HTTP.Reply;
    protocols, cash, authorization, receipt, offer, error: TEXT;
    (* value fields of the respective header fields in a payment
       request.  NIL if not present *)
  METHODS
    init(reply: HTTP.Reply; url: TEXT; log: App.Log): Reply RAISES {App.Error};
    (* initializes the reply from the payment fields in "reply".  *)

    toReply (log: App.Log): HTTP.Reply RAISES {App.Error};
    (* returns a reply from current value of fields *)
  END;

PROCEDURE ParseRequest(request: HTTP.Request; log: App.Log): Request
  RAISES {App.Error};
  <* LL < mu *>
  (* Extract the payment fields from an HTTP reply.  If the request
     corresponds to a supported payment protocol the result will be
     the appropriate subtype of a "Request". *)

PROCEDURE ParseReply(reply: HTTP.Reply; request: Request; 
                     log: App.Log): Reply 
  RAISES {App.Error};
  <* LL < mu *>
  (* Extract the payment fields from an HTTP reply.  If the reply 
     corresponds to a supported payment protocol the result will be
     the appropriate subtype of a "Reply". *)

PROCEDURE IsPaymentReply(reply: HTTP.Reply): BOOLEAN;
  (* returns TRUE is the reply is a payment reply. *)

TYPE
  ProtocolHandler = OBJECT
  METHODS
    accept(protocol: TEXT): BOOLEAN; <* LL = mu *>
    (* return TRUE if the handler accepts the given protocol.  *)
    parseRequest(request: HTTP.Request; log: App.Log): Request 
      RAISES {App.Error}; <* LL < mu *>
    parseReply(reply: HTTP.Reply; 
               request: Request; 
               log: App.Log): Reply 
      RAISES {App.Error}; <* LL < mu *>
  END;

PROCEDURE RegisterProtocolHandler(handler: ProtocolHandler); 
  <* LL < mu *>

END HTTPPayment.
