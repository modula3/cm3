(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Apr 24 11:44:03 PDT 1995 by najork                   *)
(*       Created on Mon Apr 24 11:37:08 PDT 1995 by najork                   *)

MODULE Test22 EXPORTS Main;

IMPORT TextVBT, Thread, Trestle;


PROCEDURE Apply (<*UNUSED*> self: Thread.Closure): REFANY =
  BEGIN
    LOOP
      EVAL NEW (REF INTEGER);
    END;
  END Apply;
    

VAR v := TextVBT.New ("Memory hog");

BEGIN
  EVAL Thread.Fork (NEW (Thread.Closure, apply := Apply));
  Trestle.Install (v);
  Trestle.AwaitDelete(v)
END Test22.
