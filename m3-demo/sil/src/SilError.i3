(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Nov  4 09:58:25 PST 1994 by kalsow    *)

INTERFACE SilError;

IMPORT AtomList;

PROCEDURE Put (info: AtomList.T;  a, b, c: TEXT := NIL);

END SilError.


