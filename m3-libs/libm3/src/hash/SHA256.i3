(* Copyright (C) 2021 Peter McKinna.  All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

INTERFACE SHA256;

(* An implementation of the SHA256 digest algorithm (FIPS PUB 180-4) *)

IMPORT Rd;

(* The returned text will always be 64 hex chars *)

PROCEDURE FromText(txt : TEXT) : TEXT;
PROCEDURE FromFile(rd : Rd.T) : TEXT;

END SHA256.
