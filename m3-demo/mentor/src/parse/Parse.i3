(* Copyright 1992 Digital Equipment Corporation.       *)
(* Distributed only by permission.                     *)
(* Last modified on Sat Jul 25 00:45:11 1992 by kalsow *)

INTERFACE Parse;

IMPORT FormsVBT, Token;

TYPE
  State = REF RECORD
    states   : REF ARRAY OF Node;
    n_tokens : INTEGER;
    input    : REF ARRAY OF Token.T;
    tokens   : REF ARRAY OF TEXT;  (* tokens[i] == Token.Name [input[i]] *)
  END;

TYPE
  Node = REF RECORD
    tag  : TEXT;
    next : REF ARRAY OF Node;
    x,y  : REAL;
  END;

PROCEDURE Init (fv: FormsVBT.T): State;
PROCEDURE FmtState (s: State): TEXT;

TYPE IntList = REF ARRAY OF INTEGER;

PROCEDURE FmtIntList (x: IntList): TEXT;

END Parse.

