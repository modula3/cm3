(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue May 16 09:07:12 PDT 1995 by kalsow   *)
(*                                                           *)
(* Contributed by Harry George (hgeorge@eskimo.com), 5/16/95 *)

MODULE Hello EXPORTS Main;
(*
Abstract: Hello, world sample

11/16/94  Harry George
          Initial version
*)


IMPORT IO, HelloGUI;

VAR
  verbosity:[0..3]:=1;

VAR
  Module:="Hello";

(*-----------------*)
PROCEDURE debug(level:INTEGER; ftn,str:TEXT) =
BEGIN
  IF verbosity >= level THEN
    (*debugging levels*)
    IO.Put (Module & ":" & ftn & ":" & str);
  END;
END debug;

(*---------------------*)
CONST ftn = "Main";
BEGIN
  debug(1,ftn,"begin\n");

  HelloGUI.verbosity:=2;
  HelloGUI.Init("Hello, world");

  debug(1,ftn,"end\n");
END Hello.
