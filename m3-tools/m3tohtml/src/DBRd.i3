(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Apr  7 13:21:06 PDT 1994 by kalsow                   *)

INTERFACE DBRd;

TYPE
  T <: T_; T_ = OBJECT
  METHODS
    init (path: TEXT): T;
    get_int (): INTEGER;
    get_line (): TEXT;
    close ();
  END;

END DBRd.


