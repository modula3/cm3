(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: TokParams.i3,v 1.2 2001-09-19 15:03:34 wagner Exp $ *)

INTERFACE TokParams;
IMPORT TokSpec;
IMPORT ParseParams;
TYPE
  T = RECORD
    (* names of files *)
    source, out: TEXT;
    tokSource, tokOut: TEXT; (* same as first line if specifyTok=FALSE *)
    (* bases stripped of directory names and ".i3" *)
    outBase, tokOutBase: TEXT;
  END;
PROCEDURE Get(progName, inSuffix, outSuffix: TEXT;
              specifyTok: BOOLEAN := TRUE): T;

PROCEDURE GetPP(progName, inSuffix, outSuffix: TEXT;
                specifyTok: BOOLEAN := TRUE;
                VAR pp: ParseParams.T): T;
(* Like Get, but returns the ParseParams.T without calling finish().
   The caller can consume additional keywords before calling pp.finish(). *)

PROCEDURE ReadTokens(tp: T): TokSpec.T;

(* examples:
   tp := TokParams.Get("tok", ".t", "Tok.i3", FALSE);
   tp := TokParams.Get("lex", ".l", "Lex.i3");
   tp := TokParams.Get("yacc", ".y", "Parse.i3");
   tok := ReadTokens(tp);
*)
END TokParams.
