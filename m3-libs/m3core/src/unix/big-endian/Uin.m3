(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Big-endian version. *)

MODULE Uin;

FROM Ctypes IMPORT unsigned_short, unsigned;

(* Big-endian versions; simply return the argument. *)

PROCEDURE ntohl(x: unsigned): unsigned = BEGIN RETURN x; END ntohl;
PROCEDURE ntohs(x: unsigned_short): unsigned_short = BEGIN RETURN x; END ntohs;
PROCEDURE htonl(x: unsigned): unsigned = BEGIN RETURN x; END htonl;
PROCEDURE htons(x: unsigned_short): unsigned_short = BEGIN RETURN x; END htons;

BEGIN
END Uin.
