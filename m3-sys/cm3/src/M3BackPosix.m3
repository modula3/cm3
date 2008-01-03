(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Oct 12 16:12:41 PDT 1994 by kalsow     *)

MODULE M3BackPosix EXPORTS M3Backend;

IMPORT Wr, M3CG, M3CG_BinWr;

PROCEDURE Open (target: Wr.T;  <*UNUSED*> target_name: TEXT): M3CG.T =
  BEGIN
    RETURN M3CG_BinWr.New (target);
  END Open;

PROCEDURE Close (<*UNUSED*> cg: M3CG.T) =
  BEGIN
  END Close;

BEGIN
END M3BackPosix.
