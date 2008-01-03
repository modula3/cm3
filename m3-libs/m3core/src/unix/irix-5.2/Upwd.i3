(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Thu Oct  6 10:59:50 PDT 1994 by ericv         *)
(*      modified on Sat Jun 27 15:49:20 PDT 1992 by muller        *)

INTERFACE Upwd;

FROM Ctypes IMPORT char_star, int;
FROM Utypes IMPORT uid_t;

(*** <pwd.h> ***)

TYPE
  struct_passwd = RECORD
    pw_name:     char_star;
    pw_passwd:   char_star;
    pw_uid:      uid_t;
    pw_gid:      uid_t;
    pw_age:      char_star;
    pw_comment:  char_star;
    pw_gecos:    char_star;
    pw_dir:      char_star;
    pw_shell:    char_star;
    pw_origin:   int;            (* type of entry, defined below. *)
    pw_yp_passwd: char_star;	 (* if non-null, yp passwd value *)
    pw_yp_gecos: char_star;	 (* if non-null, yp gecos field *)
    pw_yp_dir: char_star;	 (* if non-null, yp home directory *)
    pw_yp_shell: char_star;	 (* if non-null, yp login shell *)
    pw_yp_netgroup: char_star;   (* if non-null, yp netgroup name *)
  END;

(* pw_origin values: *)
CONST
  PW_LOCAL	 = 0;
  PW_YP_USER	 = 1;
  PW_YP_NETGROUP	 = 2;
  PW_YP_ALL	 = 3;
  PW_YP_REMOTE	 = 4;

TYPE
  struct_comment = RECORD
    c_dept:      char_star;
    c_name:      char_star;
    c_acct:      char_star;
    c_bin:       char_star;
  END;

  struct_passwd_star = UNTRACED REF struct_passwd;

(*** getpwent, getpwuid, getpwnam, setpwent, endpwent(2) - get
     password file entry ***)

<*EXTERNAL*> PROCEDURE getpwuid (uid: uid_t): struct_passwd_star;
<*EXTERNAL*> PROCEDURE getpwnam (name: char_star): struct_passwd_star;
<*EXTERNAL*> PROCEDURE getpwent (): struct_passwd_star;
<*EXTERNAL*> PROCEDURE setpwent();
<*EXTERNAL*> PROCEDURE endpwent();

END Upwd.
