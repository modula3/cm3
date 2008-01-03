(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Jun 14 08:21:08 PDT 1995 by kalsow     *)

INTERFACE Buf;

IMPORT File, OSError;

TYPE
  T = REF ARRAY OF CHAR;

PROCEDURE FromFile (path: TEXT;  src: File.T;
                    pad: CARDINAL := 0): T  RAISES {OSError.E};
(* Read and return the entire contents of the file "src" with "pad" 
   bytes of zero appended.  If "src" is "NIL", open and read the file
   named "path" and then close it. *)

PROCEDURE FromText (txt: TEXT): T;
(* Return the contents of 'txt' as a buffer *)

END Buf.

