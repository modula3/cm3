(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObPrintTree;
IMPORT ObTree, ObLib, SynWr;

  PROCEDURE Setup();
  (* To be called before any other use of this module *)

  PROCEDURE FmtIdeName(name: ObTree.IdeName; env: ObTree.Env): TEXT;

  PROCEDURE FmtIde(name: ObTree.IdeName; place: ObTree.IdePlace; env: ObTree.Env): TEXT;

  PROCEDURE PrintIdeName(swr: SynWr.T; name: ObTree.IdeName; env: ObTree.Env);
  PROCEDURE PrintIdeList(swr: SynWr.T; list: ObTree.IdeList; env: ObTree.Env): ObTree.Env;

  PROCEDURE PrintIde(swr: SynWr.T; name: ObTree.IdeName; place: ObTree.IdePlace; env: ObTree.Env);

  PROCEDURE PrintOk(swr: SynWr.T);
  PROCEDURE PrintBool(swr: SynWr.T; bool: BOOLEAN);
  PROCEDURE PrintChar(swr: SynWr.T; char: CHAR);
  PROCEDURE PrintText(swr: SynWr.T; text: TEXT);
  PROCEDURE PrintInt(swr: SynWr.T; int: INTEGER);
  PROCEDURE PrintReal(swr: SynWr.T; real: LONGREAL);

  PROCEDURE PrintTerm(swr: SynWr.T; term: ObTree.Term; libEnv: ObLib.Env; 
    env: ObTree.Env; depth: INTEGER);

  PROCEDURE PrintSignature(swr: SynWr.T; term: ObTree.Term; libEnv: ObLib.Env;
                      env: ObTree.Env);

END ObPrintTree.
