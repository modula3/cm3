(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Tue Jan 25 15:03:39 1994 by luca               *)
(*      modified on Tue Jun 23 20:16:54 1992 by knaff          *)

INTERFACE Constructor;
IMPORT SynParse, MetaParser;

VAR
  grammar : SynParse.NonTerminal ;
  metaParser: SynParse.T;
  (* The meta-grammar used to parse the client grammars. *)

PROCEDURE Setup() RAISES {SynParse.Fail};
(* To be called before any other use of this module. *)

PROCEDURE LinkInSelects(table: MetaParser.ActionTable) ;

END Constructor.

