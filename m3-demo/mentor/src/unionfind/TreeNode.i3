(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Aug 18 11:38:50 PDT 1994 by heydon                   *)
(*      modified on Fri Jun 10 15:37:40 PDT 1994 by shillner                 *)

INTERFACE TreeNode;

TYPE
  T = OBJECT
    id: CARDINAL;
    rank: CARDINAL := 0;
    up: T := NIL
  END;

END TreeNode.
