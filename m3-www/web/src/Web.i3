(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Tue Sep 26 10:54:12 PDT 1995 by mhb      *)
(*      modified on Wed Sep  6 11:01:49 PDT 1995 by glassman *)

(* The "Web" interface retrieves documents from the World Wide Web using an
   http proxy server.  Details about the HTTP protocol are in

|      http://info.cern.ch/hypertext/WWW/Protocols/HTTP/HTTP2.html

   *)

INTERFACE Web;

IMPORT Date, IP, Rd, Thread;

EXCEPTION Error(TEXT);

TYPE T <: ROOT;
(* A "Web.T" identifies an http proxy server.  The routines in this
   interface that take a "Web.T" as a parameter accept the value "NIL",
   which represents the default proxy server obtained by calling
   "Setup(NIL)". *)

CONST
  DefaultProxyHost   = NIL;
       (* At SRC, set it to "http://www-proxy.pa.dec.com:8080/" instead *)
  DefaultNoProxyList = "";
       (*  At SRC, set it to "src-www,.dec.com" instead *)
  
PROCEDURE Setup (proxyURL, noProxyList: TEXT := NIL): T
  RAISES {Error};
(* Return a data type representing an http proxy server. *)

(* "proxyURL" is the url for the proxy server; it should have the format:
|      http://hostname.blah.blah.blah:8080/
   If "proxyURL" is "NIL", it defaults to the environment variable
   "http_proxy".  If "http_proxy" is empty or undefined, "proxyURL"
   defaults to "DefaultProxyHost".  If "DefaultProxyHost" is NIL, 
   no proxy is used.

   "noProxyList" specifies a set of domains for which the proxy should not
   be consulted; the format is a comma-separated list of domain names, with
   optional port.  If "noProxyList" is NIL, it defaults to the environment
   variable "no_proxy".  If "no_proxy" is empty or undefined, "noProxyList"
   defaults to "DefaultNoProxyList".  If "DefaultNoProxyList" is the empty 
   string, the proxy will be consulted for every URL.

   Details about proxies are at:
|      http://info.cern.ch/hypertext/WWW/Daemon/User/Proxies/ProxyClients.html

   "Setup" raises "Error" if "proxyURL" is not in a valid format. *)


CONST 
  DefaultRequestFields = ARRAY [0 .. 0] OF TEXT{"Accept: */*"};

TYPE
  MIMEType = {Application, Audio, Image, Message, Multipart, Text, Video,
              Xperimental};

  HTMLDate = TEXT;

  Header = RECORD
             httpVersion   : TEXT;
             statusCode    : INTEGER;
             reason        : TEXT;
             contentType   : MIMEType;
             contentSubType: TEXT;
             (* optional fields: *)
             allowed      : TEXT     := NIL;
             public       : TEXT     := NIL;
             contentLength: INTEGER  := 0;
             encoding     : TEXT     := NIL;
             date         : HTMLDate := NIL;
             expires      : HTMLDate := NIL;
             lastModified : HTMLDate := NIL;
             server       : TEXT     := NIL;
             MIMEVersion  : TEXT     := NIL;
             title        : TEXT     := NIL;
             location     : TEXT     := NIL;
           END;

  Page = OBJECT
           header  : Header;
           contents: TEXT;
         END;

PROCEDURE Get (    url   : TEXT;
               VAR header: Header;
               READONLY requestFields: ARRAY OF TEXT := DefaultRequestFields;
               forceCache: BOOLEAN := FALSE;
               debug     : BOOLEAN := FALSE;
               server    : T       := NIL    ): Rd.T
  RAISES {Error, Thread.Alerted, IP.Error};
(* Do a "GET" request, passing in the "requestFields".  By default, the
   proxy server will grab pages from a local cache, if one is available and
   if the "url" is in the cache.  When "forceCache" is "TRUE", the proxy
   server will explicitly not use any cache.  The "Error" exception is
   raised if the header returned by the request is invalid in any way. *)

PROCEDURE GetHead (url: TEXT;
                   READONLY requestFields: ARRAY OF TEXT := DefaultRequestFields;
                   forceCache: BOOLEAN := FALSE;
                   server    : T       := NIL    ): Rd.T
  RAISES {Error, Thread.Alerted, IP.Error};
(* Do a "HEAD" request, passing in the "requestFields".  By default, the
   proxy server will grab pages from a local cache, if one is available and
   if the "url" is in the cache.  When "forceCache" is "TRUE", the proxy
   server will explicitly not use any cache.  Mostly for debugging use. *)

PROCEDURE Post (    url   : TEXT;
                    argString  : TEXT;
                VAR header: Header;
                READONLY requestFields: ARRAY OF TEXT := DefaultRequestFields;
                server    : T       := NIL    ): Rd.T
  RAISES {Error, Thread.Alerted, IP.Error};
(* Do a "POST" request.  Like GET, except for extra argument *)


PROCEDURE ParseHead (rd: Rd.T): Header RAISES {Error, Thread.Alerted};
(* Parses the information returned by "GetHead". *)

PROCEDURE ToDate (t: HTMLDate): Date.T RAISES {Error, Thread.Alerted};
(* Takes an HTML format date, such as that in the Date or Last-modified
   field, and parses it into a Date.T *)

PROCEDURE AbsoluteURL (url, base: TEXT): TEXT;
(* Returns an absolute URL constructed from "url" and "base", the URL of
   the document containing "url". *)

PROCEDURE EncodeURL (t: TEXT): TEXT RAISES {Thread.Alerted};
(* Encodes special characters in a text string, such as the argument of an
   ISINDEX query *)

END Web.


