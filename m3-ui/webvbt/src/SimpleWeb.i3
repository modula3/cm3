(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Aug 27 13:31:06 PDT 1996 by najork                   *)
(*      modified on Sat Sep 16 10:13:43 PDT 1995 by mhb                      *)

(* The "SimpleWeb" interface fetches the contents of a URL.  If there are
   any errors or if the URL is not of an acceptable type, an approrpriate
   message is returned as the contents of the page.  In addition, the
   "SimpleWeb" interface maintains a cache of pages. *)

INTERFACE SimpleWeb;

IMPORT Thread, Web;

TYPE
  ExtensionType = 
    RECORD 
      ext: TEXT;
      type: Web.MIMEType;
      subType: TEXT;
    END;

CONST 
  DefaultAccepts = ARRAY OF TEXT {(* no wild cards; sorry *)
                     "text/html",  
                     "text/plain", 
                     "image/x-xbitmap", 
                     "image/gif", 
                     "image/jpeg",
                     "image/ppm",
                     "image/pbm",
                     "image/pgm",
                     "image/pnm"};

PROCEDURE Setup(READONLY a: ARRAY OF ExtensionType);

PROCEDURE Fetch (         url    : TEXT;
                 READONLY accepts: ARRAY OF TEXT := DefaultAccepts;
                          reload : BOOLEAN       := FALSE;
                          server : Web.T         := NIL             ):
  Web.Page RAISES {Thread.Alerted};
(* Fetch the URL and return its contents.  By default, pages are grabbed
   from a local cache, if "url" is in the cache, or from the server's local
   cache, if the server maintains a cache.  When "reload" is "TRUE", the
   request always goes to the server; moreover, the server will explicitly
   not use its own cache, if it maintains one.

   If the "url" cannot be retreived for any reason, or if the requested
   "url" is not of type listed in the "accepts" parameter, then an object
   of type "text/plain" is returned whose "body" contains an appropriate
   message. *)

END SimpleWeb.
