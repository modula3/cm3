(* Copyright (C) 2017 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

(* An implementation of the MD5 digest algorithm (RFC 1321) *)

INTERFACE MD5;

IMPORT Rd;

(* The returned text will always be 32 hex chars *)

PROCEDURE FromText(txt : TEXT) : TEXT;
PROCEDURE FromFile(rd : Rd.T) : TEXT;

END MD5.
