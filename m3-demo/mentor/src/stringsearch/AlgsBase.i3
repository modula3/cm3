(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue May 24 09:15:49 PDT 1994 by najork   *)
(*      modified on Sat Aug  1 18:25:26 PDT 1992 by broder   *)

INTERFACE AlgsBase;

IMPORT Thread, StringSearchAlgClass;

PROCEDURE GetData (alg: StringSearchAlgClass.T; VAR p, s: TEXT)
  RAISES {Thread.Alerted};

END AlgsBase.

