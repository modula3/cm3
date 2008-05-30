(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Upwd;

FROM Ctypes IMPORT char_star, int;
FROM Utypes IMPORT uid_t;

TYPE
  struct_passwd = RECORD
    pw_name : char_star;
    pw_passwd : char_star;
    pw_uid : uid_t;
  END;
  struct_passwd_star = UNTRACED REF struct_passwd;

<*EXTERNAL*> PROCEDURE getpwuid (uid: int): struct_passwd_star;
<*EXTERNAL*> PROCEDURE getpwnam (name: char_star): struct_passwd_star;

END Upwd.
