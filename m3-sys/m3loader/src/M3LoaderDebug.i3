(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Nov  9 11:07:40 PST 1994 by isard      *)

INTERFACE M3LoaderDebug;

IMPORT M3ID;

PROCEDURE Txt (out: TEXT);
PROCEDURE Name (out: M3ID.T);
PROCEDURE Int (out: INTEGER);
PROCEDURE Address (out: INTEGER);
PROCEDURE NL ();

END M3LoaderDebug.
