(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Oct 31 13:22:56 PST 1994 by kalsow     *)
(*      modified on Sat Jun 27 15:27:28 PDT 1992 by muller     *)

MODULE Udir;

(* Emulation of System V readdir in terms of Berkeley readdir.  This can
   be eliminated when libucb is no longer linked into M3 programs. *)
  
VAR
  dirent := NEW(struct_dirent_star);    (* The System V structure. *)

PROCEDURE readdir(dirPtr: DIR_star): struct_dirent_star =
  VAR direct := UCB_readdir(dirPtr); (* The Berkeley structure. *)
  BEGIN
    IF direct = NIL THEN RETURN NIL END;
    dirent.d_ino    := direct.d_ino;
 (* dirent.d_offset := 0; --- no documented way to calculate d_offset. *)
    dirent.d_reclen := direct.d_reclen;
    dirent.d_name   := direct.d_name;
    RETURN dirent;
  END readdir;

BEGIN
END Udir.


