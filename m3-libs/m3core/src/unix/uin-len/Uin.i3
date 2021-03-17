(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

INTERFACE Uin;

FROM Ctypes IMPORT char, unsigned_char, unsigned_short, unsigned;

CONST
  IPPROTO_TCP = 6;

TYPE
  struct_in_addr = RECORD
    s_addr: unsigned;
  END;

  struct_sockaddr_in = RECORD
    sin_len: unsigned_char; (* This is absent on most platforms. *)
    sin_family: unsigned_char; (* This is 16 bits on most platforms. *)
    sin_port: unsigned_short;
    sin_addr: struct_in_addr;
    sin_zero: ARRAY [0..7] OF char;
  END;

  (* generic struct to get the family before casting *)
  struct_sockaddr = RECORD
    sin_len: unsigned_char; (* This is absent on other platforms. *)
    sin_family: unsigned_char; (* This is 16 bits on other platforms. *)
    data       : ARRAY [0..13] OF CHAR;
  END;

<*EXTERNAL "Uin__ntohl"*> PROCEDURE ntohl(x: unsigned): unsigned;
<*EXTERNAL "Uin__ntohs"*> PROCEDURE ntohs(x: unsigned_short): unsigned_short;
<*EXTERNAL "Uin__htonl"*> PROCEDURE htonl(x: unsigned): unsigned;
<*EXTERNAL "Uin__htons"*> PROCEDURE htons(x: unsigned_short): unsigned_short;

END Uin.
