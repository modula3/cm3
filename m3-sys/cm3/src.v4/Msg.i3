(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Jul  6 11:28:36 PDT 1994 by kalsow     *)

INTERFACE Msg;

IMPORT AtomList, Arg;

TYPE
  Level = {Silent, Explain, Commands, Verbose, Debug};

VAR (*READONLY*)
  level : Level := Level.Silent;

PROCEDURE SetLevel (new: Level);
(* level := MAX (level, new) *)


PROCEDURE UsageError (a, b, c: TEXT := NIL);
PROCEDURE FatalError (info: AtomList.T;  a, b, c, d: TEXT := NIL);
PROCEDURE Error      (info: AtomList.T;  a, b, c, d: TEXT := NIL);
PROCEDURE OSErr (info: AtomList.T): TEXT;

PROCEDURE Debug (a, b, c, d: TEXT := NIL);
PROCEDURE Verbose (a, b, c, d, e: TEXT := NIL);
PROCEDURE Commands (a, b, c, d, e, f: TEXT := NIL);
PROCEDURE Explain (a, b, c, d: TEXT := NIL);

PROCEDURE Out (a, b, c, d, e, f, g: TEXT := NIL);
PROCEDURE OutL (a, b: TEXT;  l: Arg.List);

END Msg.
