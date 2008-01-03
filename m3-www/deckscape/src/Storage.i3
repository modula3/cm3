(* Copyright 1996 Digital Equipment Corporation.              *)
(* Distributed only by permission.                            *)
(*                                                            *)
(* Last modified on Mon Aug 19 22:13:28 PDT 1996 by mhb       *)

INTERFACE Storage;

IMPORT WorkspaceVBT;

PROCEDURE Restore(): WorkspaceVBT.T;

PROCEDURE Save(v: WorkspaceVBT.T);

END Storage.
