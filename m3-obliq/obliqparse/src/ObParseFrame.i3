(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
INTERFACE ObParseFrame;
IMPORT MetaParser;

TYPE

PROCEDURE Setup();

PROCEDURE RegisterActions(actions: MetaParser.ActionTable);

END ObParseFrame.

