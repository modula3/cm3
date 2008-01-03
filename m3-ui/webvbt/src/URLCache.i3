(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jan 11 11:33:35 PST 1996 by mhb                      *)
<* PRAGMA LL *>

(* A cache of URL information. *)

INTERFACE URLCache;

IMPORT HTML, Web;


PROCEDURE Put(url: TEXT; READONLY header: Web.Header; contents: TEXT);
(* Store "header" and "contents" into cache under the key "url". *)

PROCEDURE Get(url: TEXT; VAR header: Web.Header; VAR contents: TEXT): BOOLEAN;
(* Retrieve the most recent "header" and "contents" that have been stored
   under the key "url". *)

PROCEDURE Enable();
PROCEDURE Disable();
(* Enables or disable subseqent calls to Put *)

PROCEDURE Flush();
(* Flush the cache. *)


PROCEDURE PutHTML(url: TEXT; html: HTML.T);
(* Store "html" under the key "url". *)

PROCEDURE GetHTML(url: TEXT; VAR html: HTML.T): BOOLEAN;
(* Retrieve the most recent "html" that has been stored under "url". *)

PROCEDURE EnableHTML();
PROCEDURE DisableHTML();
(* Enables or disable subseqent calls to PutHTML *)

PROCEDURE FlushHTML();
(* Flush the HTML cache. *)

END URLCache.
