(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Mon Jul 15 14:55:28 PDT 1996 by steveg *)

INTERFACE HTTPApp;

(* 
    This interface provides a general structure for HTTP
    applications.  The client writes the handlers for 
    arguments, logging, request, and replies while the
    library handles the basic HTTP processing.
  *)

IMPORT
  App, HTTP, IP, Rd, TextList, Wr;

TYPE 
  RequestPriority = {High, Normal, Low};

  RequestHandler <: RequestHandlerPublic;
  RequestHandlerPublic = OBJECT
    priority: RequestPriority := RequestPriority.Normal;
  METHODS
    accept(request: HTTP.Request; serverData: REFANY;
           VAR (* OUT *) acceptState: REFANY;
           log: App.Log): BOOLEAN RAISES {App.Error};
    (* returns TRUE if the handler is prepared to accept the
       request.  

       "serverData" is data given to the "Serve" procedure by the application.
       "acceptState" is passed on to "request" if if the request is accepted.
     *)

    request(request: HTTP.Request; 
            serverData, acceptState: REFANY;
            rd: Rd.T; wr: Wr.T; log: App.Log) 
            RAISES {App.Error};
    (* "request" is invoked in its own thread for a request received 
       by the server.
       
       "serverData" is data passed to the "Serve" procedure
       "acceptState" is data created in the "accept" method
       "request" is a parsed HTTP header, 
       "rd" is a reader on the the remainder of the request.  
       "wr" is a writer for the reply.  
       "log" is for reporting conditions to the server's log.
          "status" and "error" messages to "log" are also written to
          "wr".
    *)
  END;

PROCEDURE PostRd(request: HTTP.Request; rd: Rd.T; log: App.Log): Rd.T 
  RAISES {App.Error};
  (* Use the "Content-Length" field of "request" to convert "rd" into
     a reader that returns EOF after "Content-Length" bytes *)

TYPE
  ReplyHandler <: ReplyHandlerPublic;
  ReplyHandlerPublic = OBJECT
  METHODS
    reply(reply: HTTP.Reply; rd: Rd.T; wr: Wr.T; log: App.Log) 
          RAISES {App.Error};
    (* reply is normally invoked to handle the reply of
       a client request or proxied request.

       "reply" is the parsed headers of the reply.  "rd" is a reader
       on the remainder of the reply.  

       "wr" is the writer given to the "Client" call.  

       "log" is for reporting conditions to the server's log.

       The reply handler filters the contents of "rd" and writes
       them to "wr".
    *)
  END;

  Server <: ServerPublic;
  ServerPublic = OBJECT
    server: TEXT;
    port: INTEGER;
    endpoint: IP.Endpoint;
  METHODS
    init(server: TEXT; port: INTEGER; log: App.Log): Server 
      RAISES {App.Error};
    (* initializes server, port and noProxy and sets the endpoint *)
    initParse(serverAndPort: TEXT; log: App.Log): Server
      RAISES {App.Error};
    (* "serverAndPort" is in <server>[:<port>] or
       http://<server>[:port]/" format (the latter is for compatibility
       with Mosaic environment variables).  If the port is
       not given, then "defaultPort" is used.  "noProxy" is a 
       no proxy list that will be parsed by ParseNoProxy.  *)
  END;

TYPE
  ServerList = REF RECORD
    head: Server;
    tail: ServerList := NIL;
  END;

  Proxy <: ProxyPublic;
  ProxyPublic = OBJECT
    proxies: ServerList;
    noProxy: TextList.T;
  METHODS
    init(noProxy: TEXT): Proxy;
    (* inititialize the proxy with the "noProxy" text - a comma separated
       list of server name suffixes. *)
    addProxy(server: Server);
    (* add a proxy server to the end of the list.  Proxy servers added
       first have priority. *)
  END;  

PROCEDURE ParseNoProxy(noProxy: TEXT): TextList.T;
(* "noProxy" is a comma separated list of server name suffixes.  The result
   is a text list where each server suffix is parsed into a separate
   list element *)

PROCEDURE Serve(port: INTEGER; log: App.Log := NIL; serverData: REFANY := NIL) 
  RAISES {App.Error};
(* "Listen" sits in a loop waiting for HTTP requests on "port".
   Messages written to "log" should be presented to the user
   of the application (as desired).

   "serverData" is passed to the "accept" and "request" methods of
   the RequestHandlers when requests are made.

   If "log" = NIL then the log handler will write to stdout
   and to the wr of the request
*)

PROCEDURE ServerPort(port: INTEGER): BOOLEAN;
(* Return TRUE if there has been a call on "Serve" for "port" *)

PROCEDURE Client(request: HTTP.Request;
                 proxy: Proxy;
                 style: HTTP.Style;
                 rd: Rd.T;
                 wr: Wr.T;
                 handler: ReplyHandler;
                 log: App.Log := NIL) RAISES {App.Error};
(* Make a client request or proxy a client request.  
   The request is made directly if there is no proxy given or
   if the destination server does not match against "proxy.noProxy").

   If "style" is NIL, then DefaultStyle() is used.

   Program information (user-agent, or via) header is added automatically
   to the request.

   Host: header is added automatically to the request

   If "rd" is not NIL,  read from "rd" and send it with the request.  

   After "request" is sent to the server, the header of the
   reply is parsed and "handler.reply" is called with "wr" for
   its output.

   If log = NIL then log := App.defaultLog 
 *)

PROCEDURE RegisterRequestHandler(handler: RequestHandler);
(*  Adds "handler" to the handlers known to the
    server.  When the server gets a request, it invokes the
    handlers until one services the request.
*)

CONST
  ServerPushBoundaryString = "ServerPushBoundary\n";
  ServerPushBoundaryStringStart = "--ServerPushBoundary\n";
  ServerPushBoundaryStringEnd = "--ServerPushBoundary--\n";

PROCEDURE ServerPushSupported(request: HTTP.Request): BOOLEAN;

PROCEDURE ServerPushFrame(wr: Wr.T; contentType: TEXT := "text/html"; 
                          msg: TEXT; log: App.Log) RAISES {App.Error};

PROCEDURE DefaultProxy(log: App.Log := NIL): Proxy RAISES {App.Error};
(* returns the proxy defined by the user arguments given (must be
   called AFTER App.InitializeArguments):

   switch   env          config   default
   -proxy   http_proxy   proxy:   "www-proxy.pa.dec.com:8080"
   -noProxy no_proxy     noProxy: ".dec.com"

   If log = NIL then DefaultLog will be used.
 *)

END HTTPApp.
