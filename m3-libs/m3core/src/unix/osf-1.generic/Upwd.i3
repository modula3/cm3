(* Copyright (C) 1993, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Dec 22 09:12:52 PST 1993 by kalsow    *)
(*      modified on Wed May 12 16:55:29 PDT 1993 by muller    *)

INTERFACE Upwd;

FROM Ctypes IMPORT char_star, int;
FROM Utypes IMPORT uid_t, gid_t;

(*** <pwd.h> ***)

TYPE
  struct_passwd = RECORD
    pw_name:     char_star;
    pw_passwd:   char_star;
    pw_uid:      uid_t;
    pw_gid:      gid_t;
    pw_quota:    int;
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
<*EXTERNAL*> PROCEDURE getpwuid (uid: uid_t): struct_passwd_star;
<*EXTERNAL*> PROCEDURE getpwnam (name: char_star): struct_passwd_star;
<*EXTERNAL*> PROCEDURE setpwent(): int;
<*EXTERNAL*> PROCEDURE endpwent(): int;

END Upwd.
