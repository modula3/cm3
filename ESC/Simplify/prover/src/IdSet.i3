(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Aug 17 12:31:54 PDT 2000 by rustan                   *)
(*      modified on Thu Apr 18 13:51:11 PDT 1996 by detlefs                  *)

INTERFACE IdSet;

CONST
  N = BITSIZE(INTEGER);
  Brand = "IdSet";

TYPE
  T = SET OF [0..N-1];

CONST
  Empty = T{};

END IdSet.
