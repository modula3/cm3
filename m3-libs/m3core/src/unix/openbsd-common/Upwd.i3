(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Mon Mar 09 19:02:29 PST 1992 by muller    *)

INTERFACE Upwd;

FROM Ctypes IMPORT char_star, int;
FROM Utypes IMPORT uid_t, gid_t, time_t;

TYPE
  struct_passwd = RECORD
    pw_name : char_star;
    pw_passwd : char_star;
    pw_uid : uid_t;
    pw_gid : gid_t;
    pw_class : char_star;
    pw_gecos : char_star;
    pw_dir : char_star;
    pw_shell : char_star;
    expire : time_t;
  END;

  struct_passwd_star = UNTRACED REF struct_passwd;

<*EXTERNAL*> PROCEDURE getpwent (): struct_passwd_star;
<*EXTERNAL*> PROCEDURE getpwuid (uid: int): struct_passwd_star;
<*EXTERNAL*> PROCEDURE getpwnam (name: char_star): struct_passwd_star;
<*EXTERNAL*> PROCEDURE setpwent(): int;
<*EXTERNAL*> PROCEDURE endpwent(): int;

END Upwd.
