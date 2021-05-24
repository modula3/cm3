(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE M3Loc;

IMPORT M3ID;

TYPE
  T = REF RECORD
    pkg    := M3ID.NoID;
    subdir := M3ID.NoID;
    pkg_dir: TEXT   := NIL;  (* == pkg_root / pkg *)
    path   : TEXT   := NIL;  (* == pkg_dir / subdir *)
    next   : T      := NIL;
  END;

CONST
  noPkg = M3ID.NoID;

PROCEDURE New (pkg, subdir: M3ID.T;  pkg_dir: TEXT): T;

END M3Loc.
