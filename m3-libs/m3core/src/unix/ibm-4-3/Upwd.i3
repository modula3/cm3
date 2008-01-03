(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Dec 17 11:20:03 PST 1993 by kalsow    *)
(*      modified on Fri May  4 09:01:08 1990 by muller        *)

INTERFACE Upwd;

FROM Ctypes IMPORT char_star, short, int;
FROM Utypes IMPORT uid_t;

(*** <pwd.h> ***)

TYPE
  struct_passwd = RECORD
    pw_name:     char_star;
    pw_passwd:   char_star;
    pw_uid:      uid_t;
    pad:         short;
    pw_gid:      uid_t;
    pad1:        short;
    pw_quota:    int;	(* ULTRIX, BSD-4.2 *)
    pw_comment:  char_star;
    pw_gecos:    char_star;
    pw_dir:      char_star;
    pw_shell:    char_star;
  END;

  struct_comment = RECORD
    c_dept:      char_star;
    c_name:      char_star;
    c_acct:      char_star;
    c_bin:       char_star;
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
