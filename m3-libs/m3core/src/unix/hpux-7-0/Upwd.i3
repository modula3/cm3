(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Jun 29 09:04:56 1990 by piet@cs.ruu.nl *)
(*      modified on Fri May  4 09:01:08 1990 by muller        *)

INTERFACE Upwd;

FROM Ctypes IMPORT char_star, long, int;
FROM Utypes IMPORT uid_t, gid_t;

(*** <pwd.h> ***)

CONST
  UID_NOBODY: uid_t = 16_fffe;

TYPE
  struct_passwd = RECORD
    pw_name:     char_star;
    pw_passwd:   char_star;
    pw_uid:      uid_t;
    pw_gid:      gid_t;
    pw_age:      char_star;
    pw_comment:  char_star;
    pw_gecos:    char_star;
    pw_dir:      char_star;
    pw_shell:    char_star;
    pw_audid:    long;
    pw_audflg:   int;
  END;

  struct_comment = RECORD
    c_dept:      char_star;
    c_name:      char_star;
    c_acct:      char_star;
    c_bin:       char_star;
  END;

  struct_spasswd = RECORD
    pw_name:     char_star;
    pw_passwd:   char_star;
    pw_age:      char_star;
    pw_audid:    long;
    pw_audflg:   int;
  END;

  struct_passwd_star = UNTRACED REF struct_passwd;

  struct_spasswd_star = UNTRACED REF struct_spasswd;

(*** getpwent, getpwuid, getpwnam, setpwent, endpwent(2) - get
     password file entry ***)

<*EXTERNAL*> PROCEDURE getpwent (): struct_passwd_star;
<*EXTERNAL*> PROCEDURE getpwuid (uid: int): struct_passwd_star;
<*EXTERNAL*> PROCEDURE getpwnam (name: char_star): struct_passwd_star;
<*EXTERNAL*> PROCEDURE setpwent(): int;
<*EXTERNAL*> PROCEDURE endpwent(): int;

<*EXTERNAL*> PROCEDURE getspwent (): struct_spasswd_star;
<*EXTERNAL*> PROCEDURE getspwuid (uid: int): struct_spasswd_star;
<*EXTERNAL*> PROCEDURE getspwaid (uid: int): struct_spasswd_star;
<*EXTERNAL*> PROCEDURE getspwnam (name: char_star): struct_spasswd_star;

END Upwd.
