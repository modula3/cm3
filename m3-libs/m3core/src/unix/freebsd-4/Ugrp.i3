(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Aug 17 14:25:29 PDT 1994 by kalsow    *)
(*                                                            *)
(* Originally submitted on Fri, 22 Jul 1994 16:42:53 GMT      *)
(*   by jredford@lehman.com (John Redford)                    *)

INTERFACE Ugrp;

FROM Ctypes IMPORT char_star,char_star_star;
FROM Utypes IMPORT gid_t;

(*** <grp.h> ***)

TYPE
  struct_group = RECORD
    gr_name:    char_star;
    gr_passwd:  char_star;
    gr_gid:     gid_t;
    gr_mem:     char_star_star;
  END;

  struct_group_star = UNTRACED REF struct_group;

(*** getgrent, getgrgid, getgrnam, setgrent, endgrent
     - get group file entry ***)

<*EXTERNAL*> PROCEDURE getgrent(): struct_group_star;
<*EXTERNAL*> PROCEDURE getgrgid(gid: gid_t): struct_group_star;
<*EXTERNAL*> PROCEDURE getgrnam(name: char_star): struct_group_star;
<*EXTERNAL*> PROCEDURE setgrent();
<*EXTERNAL*> PROCEDURE endgrent();

END Ugrp.


