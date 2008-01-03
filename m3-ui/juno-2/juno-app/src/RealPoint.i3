(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Apr 29 17:56:38 PDT 1994 by heydon                   *)

INTERFACE RealPoint;

(* Real-valued points. *)

IMPORT JunoValue;

TYPE T = RECORD x, y: JunoValue.Real END;

END RealPoint.
