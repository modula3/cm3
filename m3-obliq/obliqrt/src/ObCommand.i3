(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 15:04:12 1997
 *)

INTERFACE ObCommand;
IMPORT ObErr, SynWr;

TYPE
  T =
    BRANDED "ObCommand.T" OBJECT
      name: TEXT;
      sortingName: TEXT;
      Exec: Proc;
    END;

  Set <: ROOT;

  Proc = 
    PROCEDURE(wr: SynWr.T; self: T; arg: TEXT:=NIL; 
              data: REFANY:=NIL) RAISES {ObErr.Fail};
      (* When arg is "!", the command should give a one-line description 
         of itself. If arg is "?", the command should give a full
         description. Otherwise, arg can be parsed to set options. *)

PROCEDURE Setup();

PROCEDURE NewSet(): Set;

PROCEDURE Register(set: Set; command: T);
PROCEDURE ReRegister(set: Set; name: TEXT; proc: Proc);

PROCEDURE Exec(wr: SynWr.T; name: TEXT; arg: TEXT:=NIL; 
               set: Set; data: REFANY:=NIL) 
  RAISES {ObErr.Fail};
(* When name="?" all the register commands are invoked
   with argument "!". Otherwise, the named command, if found,
   is invoked with argument arg. Data is passed along to the
   commands. *)

END ObCommand.
