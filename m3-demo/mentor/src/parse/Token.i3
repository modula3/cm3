(* Copyright 1992 Digital Equipment Corporation.       *)
(* Distributed only by permission.                     *)
(* Last modified on Mon Aug  3 13:15:37 PDT 1992 by kalsow *)

INTERFACE Token;

TYPE
  T = { LParen, RParen, Plus, Star, Assign, Id, Semi, EOF };

CONST
  Name = ARRAY T OF TEXT {
    "(", ")", "+", "*", "=", "*ID*", ";", "$"
  };

PROCEDURE Scan (input: TEXT;  VAR cursor: INTEGER;
                 VAR t: T;  VAR name: TEXT);
(* scan 'input' beginning at character 'cursor'.  Return
   the new token, and its name. *)

END Token.

