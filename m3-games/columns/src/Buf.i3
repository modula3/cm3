(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Jun 14 08:21:08 PDT 1995 by kalsow     *)

INTERFACE Buf;

IMPORT File, OSError;

TYPE
  T = REF ARRAY OF CHAR;

PROCEDURE FromFile (path: TEXT;  src: File.T): T  RAISES {OSError.E};
(* Read and return the entire contents of the file "src".  If "src"
   is "NIL", open and read the file named "path" and then close it. *)

END Buf.

