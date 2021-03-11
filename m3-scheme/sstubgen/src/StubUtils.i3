(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Created by Susan Owicki                                     *)
(* Last modified on Wed Feb 10 09:12:31 PST 1993 by owicki     *)

INTERFACE StubUtils;

IMPORT Atom, Wr;

(** EXCEPTION Error(TEXT); **)
EXCEPTION Failure;

VAR perfMon: BOOLEAN;

VAR stubchatter: Wr.T;

PROCEDURE Message(text: TEXT);
(* Write "text" on  writer "stubchatter", preceeded by "stubgen: and
   followed by a newline  *)

PROCEDURE Die(text: TEXT);
(* Write "text" on  writer "stubchatter", preceeded by "stubgen: and
   followed by a newline, then die  *)

PROCEDURE SetPerfMon(flag: BOOLEAN);
(* Set flag to indicate whether stubs should include performance monitoring
   code   *)

PROCEDURE FileName(typeName: Atom.T): TEXT;

END StubUtils.
