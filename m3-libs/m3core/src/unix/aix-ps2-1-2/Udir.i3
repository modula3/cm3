(*   Copyright (C) by IBM Corporation, 1990-91.                              *)

(* Last modified on Thu Sep 23 07:57:34 PDT 1993 by kalsow                   *)
(*      modified on Wed Mar  4 11:43:29 PST 1992 by muller                   *)
(*      modified on Tue Sep 18 11:38:53 1990 by Dick Orgass                  *)

INTERFACE Udir;     (* For AIX386. *)

IMPORT Utypes;
FROM Ctypes IMPORT char_star, int, long, unsigned_long, unsigned_short;

<*UNUSED*> CONST
  UdirCopyright = "Copyright (C) by IBM Corporation, 1990-91.";  
  UdirRCSHeader = "$Header$";
  UdirDate = "$Date$";
  UdirRevision = "$Revision$";

(* For documentation of this interface (except NameToText) see Calls and Subroutines Reference: Base Operating System, Volume 1, IBM Form number SC23-2198-00, pp 1-522 to 1-524. *)

CONST
  MAXNAMELEN = 255;     (* Maximum length of component of file path name. *)
  MAXPATHLEN = 1024;    (* Maximum length of file path name. *)

TYPE
  D_name = ARRAY [0..MAXNAMELEN] OF CHAR;
  DIR = RECORD
    dd_fd: int;
    dd_blksize: int;
    dd_buf: char_star;
    dd_size: long;
    dd_flag: long;
    dd_loc: long;
    dd_curoff: long
  END;
  DIR_star = UNTRACED REF DIR;

  struct_dirent = RECORD
    d_offset: unsigned_long;
    d_ino: Utypes.ino_t;
    d_reclen: unsigned_short;
    d_namelen: unsigned_short;
    d_name: D_name;
  END;

  struct_dirent_star = UNTRACED REF struct_dirent;

<*EXTERNAL*> PROCEDURE opendir (dir: char_star): DIR_star;
<*EXTERNAL*> PROCEDURE readdir (dirPtr: DIR_star): struct_dirent_star;
<*EXTERNAL*> PROCEDURE telldir (dirPtr: DIR_star): long;
<*EXTERNAL*> PROCEDURE seekdir (dirPtr: DIR_star; location: long);
<*EXTERNAL*> PROCEDURE rewinddir (dirPtr: DIR_star);
<*EXTERNAL*> PROCEDURE closedir (dirPtr: DIR_star): int;

PROCEDURE NameToText (READONLY name: D_name): TEXT RAISES {};
(* Converts the d_name field of a struct_dirent to a TEXT and returns it.  *)
  
END Udir.

(* Change Log

  $Log$
Revision 1.3  1991/03/18  22:38:18  muller
*** empty log message ***


*)
