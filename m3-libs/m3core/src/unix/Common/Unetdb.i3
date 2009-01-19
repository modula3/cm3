(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

<*EXTERNAL*> INTERFACE Unetdb;

FROM Ctypes IMPORT int, char_star, char_star_star, const_char_star;
FROM Utypes IMPORT socklen_t, hostent_addrtype_t, hostent_length_t;

TYPE
  struct_hostent = RECORD
    h_name:       char_star;
    h_aliases:    char_star_star;
    h_addrtype:   hostent_addrtype_t;
    h_length:     hostent_length_t;
    h_addr_list:  char_star_star;
  END;
  struct_hostent_star = UNTRACED REF struct_hostent;

(*CONST*)
<*EXTERNAL "Unetdb_TRY_AGAIN"*> VAR TRY_AGAIN: int;
<*EXTERNAL "Unetdb_NO_RECOVERY"*> VAR NO_RECOVERY: int;
<*EXTERNAL "Unetdb_NO_ADDRESS"*> VAR NO_ADDRESS: int;

PROCEDURE gethostbyname (name: const_char_star): struct_hostent_star;
PROCEDURE gethostbyaddr (addr: const_char_star; len: socklen_t; type: int): struct_hostent_star;

END Unetdb.
