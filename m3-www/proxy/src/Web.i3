(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Mon Oct 17 22:35:54 PDT 1994 by glassman *)
(* modified on Thu Sep 29 11:56:28 PDT 1994 by mhb *)

(* The "Web" interface retrieve documents from the World Wide Web using an
   http proxy server.  Details about the HTTP protocol are in
   http://info.cern.ch/hypertext/WWW/Protocols/HTTP/HTTP2.html *)

INTERFACE Web;

IMPORT IP, Rd, Thread;

EXCEPTION Error(TEXT);


TYPE T <: ROOT;
(* A "Web.T" identifies an http proxy server.  The routines in this
   interface that take a "Web.T" as a parameter accept the value "NIL",
   which represents the default proxy server obtained by calling
   "Setup(NIL)". *)


PROCEDURE Setup (host: TEXT := NIL): T RAISES {Error};
(* Return a identifier to an http proxy server. *)

(* If "host" is "NIL", it defaults to the value of environment
   variable "http_proxy".  If this variable is
   empty or undefined, it defaults to an implementation-specific value.  
   The exception is raised if "host" is not in a valid format. *)



CONST DefaultRequestFields = ARRAY [0 .. 0] OF TEXT{"Accept: */*"};
TYPE
  MIMEType = {Application, Audio, Image, Message, Multipart,
              Text, Video, Xperimental};

  Date = TEXT;

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
             date         : Date     := NIL;
             expires      : Date     := NIL;
             lastModified : Date     := NIL;
             server       : TEXT     := NIL;
             MIMEVersion  : TEXT     := NIL;
             title        : TEXT     := NIL;
           END;

PROCEDURE Get (    url   : TEXT;
               VAR header: Header;
               READONLY requestFields: ARRAY OF TEXT := DefaultRequestFields;
               forceCache: BOOLEAN := FALSE;
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
                   server: T := NIL): Rd.T
  RAISES {Error, Thread.Alerted, IP.Error};
(* Do a "HEAD" request, passing in the "requestFields".  By default, the
   proxy server will grab pages from a local cache, if one is available and
   if the "url" is in the cache.  When "forceCache" is "TRUE", the proxy
   server will explicitly not use any cache.  Mostly for debugging use *)


PROCEDURE AbsoluteURL (url, base: TEXT): TEXT;
(* Returns an absolute URL constructed from "url" and "base", the
   URL of the document containing "url". *)

END Web.
