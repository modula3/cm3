(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObParseTree;
IMPORT SynParse, MetaParser;
    
  PROCEDURE Setup();
  (* To be called before any other use of this module *)

  PROCEDURE RegisterActions(actions: MetaParser.ActionTable) 
    RAISES {SynParse.Fail};

END ObParseTree.
