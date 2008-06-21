(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Mar  7 11:10:39 PST 1995 by kalsow     *)

INTERFACE Buf;

IMPORT M3Scanner;

TYPE
  T = M3Scanner.Buf;  (* == REF ARRAY OF CHAR *)

PROCEDURE FromFile (path: TEXT;  pad: CARDINAL := 0): T;
(* Read and return the entire contents of the file named by "path"
   with "pad" bytes of zero appended.  If an error occurs, "NIL" is
   returned. *)

PROCEDURE FromText (txt: TEXT): T;
(* Return the contents of 'txt' as a buffer *)

END Buf.

