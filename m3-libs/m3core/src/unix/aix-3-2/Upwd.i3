(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Sep 24 09:45:11 PDT 1993 by kalsow    *)
(*      modified on Tue Nov 20 23:04:01 1990 by muller        *)

INTERFACE Upwd;

FROM Ctypes IMPORT char_star, int;
FROM Utypes IMPORT uid_t;

(*** <pwd.h> ***)

TYPE
  struct_passwd = RECORD
    pw_name:     char_star;
    pw_passwd:   char_star;
    pw_spare:    uid_t;      (* Not in the declaration in pwd.h but needed. *)
    pw_uid:      uid_t;
    pw_gid:      uid_t;
    pw_gecos:    char_star;
    pw_dir:      char_star;
    pw_shell:    char_star;
  END;

  struct_passwd_star = UNTRACED REF struct_passwd;

(*** getpwent, getpwuid, getpwnam, setpwent, endpwent(2) - get
     password file entry ***)

<*EXTERNAL*> PROCEDURE getpwent (): struct_passwd_star;
<*EXTERNAL*> PROCEDURE getpwuid (uid: int): struct_passwd_star;
<*EXTERNAL*> PROCEDURE getpwnam (name: char_star): struct_passwd_star;
<*EXTERNAL*> PROCEDURE setpwent(): int;
<*EXTERNAL*> PROCEDURE endpwent(): int;

END Upwd.
