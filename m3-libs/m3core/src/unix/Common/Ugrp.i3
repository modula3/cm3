(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Aug 17 14:25:29 PDT 1994 by kalsow    *)
(*                                                            *)
(* Originally submitted on Fri, 22 Jul 1994 16:42:53 GMT      *)
(*   by jredford@lehman.com (John Redford)                    *)

INTERFACE Ugrp;

FROM Ctypes IMPORT const_char_star, char_star,char_star_star;
FROM Utypes IMPORT gid_t;

(*** <grp.h> ***)

TYPE
  struct_group = RECORD
  (* This MUST match Ugrp.c. *)
    gr_mem:     char_star_star;
    gr_name:    char_star;
    gr_gid:     gid_t;
  END;

  struct_group_star = UNTRACED REF struct_group;

(*** getgrent, getgrgid, getgrnam, setgrent, endgrent
     - get group file entry ***)

<*EXTERNAL Ugrp__getgrent*> PROCEDURE getgrent(a: struct_group_star): struct_group_star;
<*EXTERNAL Ugrp__getgrgid*> PROCEDURE getgrgid(a: struct_group_star; gid: gid_t): struct_group_star;
<*EXTERNAL Ugrp__getgrnam*> PROCEDURE getgrnam(a: struct_group_star; name: const_char_star): struct_group_star;
<*EXTERNAL Ugrp__setgrent*> PROCEDURE setgrent();
<*EXTERNAL Ugrp__endgrent*> PROCEDURE endgrent();

END Ugrp.
