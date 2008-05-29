(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Big-endian version. *)

MODULE Uin;

FROM Utypes IMPORT uint16_t, uint32_t;

(* Big-endian versions; simply return the argument. *)

PROCEDURE ntohl(x: uint32_t): uint32_t = BEGIN RETURN x; END ntohl;
PROCEDURE ntohs(x: uint16_t): uint16_t = BEGIN RETURN x; END ntohs;
PROCEDURE htonl(x: uint32_t): uint32_t = BEGIN RETURN x; END htonl;
PROCEDURE htons(x: uint16_t): uint16_t = BEGIN RETURN x; END htons;

BEGIN
END Uin.
