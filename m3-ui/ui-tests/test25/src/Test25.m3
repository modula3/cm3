(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu May 18 20:27:35 PDT 1995 by najork                   *)
(*      modified on Thu Feb  2 16:57:25 PST 1995 by heydon                   *)
(*      modified on Tue Oct 27 17:58:35 PST 1992 by gnelson                  *)
(*      modified on Sat Aug 22 23:32:17 PDT 1992 by myers                    *)

MODULE Test25 EXPORTS Main;

IMPORT FormsVBT, Rsrc, TestBundle, Trestle;

<* FATAL ANY *>

VAR
  Path := Rsrc.BuildPath(TestBundle.Get());
  w    := NEW(FormsVBT.T).initFromRsrc("Test25.fv", Path);
BEGIN
  Trestle.Install (w);
  Trestle.AwaitDelete(w)
END Test25.
