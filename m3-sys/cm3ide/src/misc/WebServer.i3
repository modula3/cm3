(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

(* WebServer offers a basic HTTP service.  It parses and dispatches
   incoming requests and sends the corresponding replies. *)

INTERFACE WebServer;

IMPORT Node;

PROCEDURE RegisterRoot (tag: TEXT;  root: Node.T);
(* Registers "root" as the initial node for any URL that begins with "tag".
   If "tag" is "NIL", "r" is the initial node any URL that doesn't
   match any of the registered tags.  It is a checked runtime error
   to register more than one root for a tag. *)

PROCEDURE UnregisterRoot (tag: TEXT);
(* Remove the root named "tag" from the table of registered roots *)

PROCEDURE Run ();
(* Open the TCP channels and begin offering service.  "Run" returns when
   it is no longer able to offer service. *)

PROCEDURE Restart ();
(* Start a new server using the current communication configuration *)

END WebServer.
