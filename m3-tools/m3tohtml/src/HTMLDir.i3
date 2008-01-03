(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Apr 27 14:05:24 PDT 1994 by kalsow                   *)

INTERFACE HTMLDir;

(* In principle an HTML front-end will do a good job rendering
   a list of names in <DIR></DIR> brackets.  In practice "xmosaic"
   doesn't.  The GenDir is intended to compensate. *)

IMPORT Wr;

PROCEDURE GenDir (READONLY names: ARRAY OF TEXT;
                  wr: Wr.T;  file, title: TEXT;  limit: INTEGER);

END HTMLDir.

