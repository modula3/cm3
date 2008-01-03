(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Tue Mar 18 15:47:53 PST 1997 by steveg *)

INTERFACE HTTPApp;

(* 
    This interface provides a general structure for HTTP
    applications.  The client writes the handlers for 
    arguments, logging, request, and replies while the
    library handles the basic HTTP processing.
  *)

IMPORT
  App, HTTP, IP, Rd, Wr;

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
    (* initializes server, port and sets the endpoint.  If
       "server" = "DIRECT" then the server will not
       proxy but will go directly to the true destination *)
    initParse(serverAndPort: TEXT; log: App.Log): Server
      RAISES {App.Error};
    (* "serverAndPort" is in <server>[:<port>] or
       http://<server>[:port]/" format (the latter is for compatibility
       with Mosaic environment variables).  If the port is
       not given, then "defaultPort" is used.  
       If serverAndPort = "DIRECT" then the server will not
       proxy but will go directly to the true destination
    *)
  END;

TYPE
  ServerList = REF RECORD
                     head: Server;
                     tail: ServerList := NIL;
                   END;

  ProxyRules = REF RECORD
                    hostPattern: TEXT;
                    proxy     : ServerList;
                    tail      : ProxyRules;
                  END;

  Proxy <: ProxyPublic;
  (* a "proxy" is a list of proxy rules: host patterns and proxies.
     When proxying, a host is matched against each pattern in order and
     the first matching pattern wins.  Each proxy in the proxy server
     list is tried in turn.  If the proxy server is "DIRECT" then a
     direct connection to the URL's server is tried.  If any
     connection fails, the next proxy server is tried.

     There is a "*:DIRECT" proxy rule as the last rule (set by "init").
  *)
  ProxyPublic =
    OBJECT
      rules, tail: ProxyRules;
    METHODS
      init (): Proxy;
      add (rule: TEXT; log: App.Log) RAISES {App.Error};
      (* "add" adds a new proxy rule to the end of the proxy rules

         "rule" is a text formatted:
         <hostPattern> <server>[,<server>]*

         "hostPattern" is a simple pattern including '?' to match
         any one character and '*' to match any sequence of 0 or more
         characters

         if any "server" is "DIRECT" then it corresponds to
         a non-proxy direct connection
      *)
    END;

PROCEDURE Serve (port, serviceValue: INTEGER;
                 log               : App.Log   := NIL;
                 serverData        : REFANY    := NIL  )
  RAISES {App.Error};
(* "Listen" sits in a loop waiting for HTTP requests on "port".
   Messages written to "log" should be presented to the user
   of the application (as desired).

   "serverData" is passed to the "accept" and "request" methods of
   the RequestHandlers when requests are made.  Any request handler
   that matches the port or the serviceValue may be called.

   If "log" = NIL then the log handler will write to stdout
   and to the wr of the request
*)

PROCEDURE ServerPort(port, service: INTEGER): BOOLEAN;
(* Return TRUE if there has been a call on "Serve" for "port" and "service" *)

PROCEDURE Client(request: HTTP.Request;
                 proxy: Proxy;
                 style: HTTP.Style;
                 rd: Rd.T;
                 wr: Wr.T;
                 handler: ReplyHandler;
                 service: INTEGER;
                 log: App.Log := NIL) RAISES {App.Error};
(* Make a client request or proxy a client request.  
   The request is made directly if there is no proxy given or
   the proxy rule matching the URL specifies "DIRECT".

   If "style" is NIL, then DefaultStyle() is used.

   Program information (user-agent, or via) header is added automatically
   to the request.

   Host: header is added automatically to the request

   If "rd" is not NIL,  read from "rd" and send it with the request.  

   After "request" is sent to the server, the header of the
   reply is parsed and "handler.reply" is called with "wr" for
   its output.

   "service" is the number of the service associated with a "Serve"
   call.  It is used to determine if the request is to a port
   associated with the current service or another.  If you didn't
   make a "Serve" call then use "AnyService".

   If log = NIL then log := App.defaultLog 
 *)

CONST
  AnyPort = 0;
  AnyService = 0;

PROCEDURE RegisterRequestHandler(port: INTEGER; handler: RequestHandler);
(*  Adds "handler" to the handlers known to the
    server.  When the server gets a request, it invokes the
    handlers until one services the request.  The handler will only
    get called if "port" matches the server's port or "port = AnyPort"
    or if "port" < 0 then "port" represents a service type, and 
    the request handler is invoked if "port" matches the server's 
    service type.  (Make sure the services have different values, :-)

    NOTE: a given request handler can only listen on a single port
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
   -pac     HTTP_PAC     pac:     "*: DIRECT"

   If log = NIL then DefaultLog will be used.
 *)

END HTTPApp.
