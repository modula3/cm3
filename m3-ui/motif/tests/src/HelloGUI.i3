(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue May 16 09:04:03 PDT 1995 by kalsow   *)
(*                                                           *)
(* Contributed by Harry George (hgeorge@eskimo.com), 5/16/95 *)

INTERFACE HelloGUI;
(*
Abstract: Interface for Hello sample

11/16/94  Harry George
          Initial version.
*)

VAR
  verbosity:[0..3]:=0;  (*0 is quiet, 3 is verbose*)

(*-------------------*)
PROCEDURE Init(str:TEXT);
(* 
bring up a run button, which when
pushed makes "str" come up in a text
field.  
*)

END HelloGUI.
