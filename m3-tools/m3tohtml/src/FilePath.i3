(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun  1 08:35:51 PDT 1995 by kalsow                   *)

INTERFACE FilePath;

PROCEDURE Normalize (dest, src: TEXT): TEXT;
(* return a relative path from file "src" to file "dest" *)

PROCEDURE Classify (path: TEXT): Kind;
(* classify the file based on its suffix *)

TYPE Kind = { I3, M3, IG, MG, other };

END FilePath.


