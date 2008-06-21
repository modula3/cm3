(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

INTERFACE BuildCache;

IMPORT IntRefTbl, Node, Pkg;

TYPE T <: Node.T;

PROCEDURE New (root: Pkg.T;  wd: TEXT): T;
(* Creates and returns a new cache entry *)

PROCEDURE AttachBody (t: T;  body: TEXT);
(* Attaches the HTML body to node 't' *)

PROCEDURE AddError (t: T;  file, line, msg: TEXT;  warning: BOOLEAN): Node.T;
(* Add and return an error node as a child of "t" *)

PROCEDURE LookUp (root: Pkg.T): T;
(* Returns the existing cache entry if one exists, "NIL" otherwise. *)

PROCEDURE Timestamp (t: T): INTEGER;
(* Returns the time "t" was built. *)

VAR (*READONLY*)
  cache: IntRefTbl.T;  (* entry name -> LIST(T) *)

END BuildCache.
