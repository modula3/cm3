(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Sun Feb 18 07:22:28 PST 1996 by steveg    *)

INTERFACE FastLex;

IMPORT Lex, Rd, Thread;

PROCEDURE Scan(
    rd: Rd.T; READONLY cs: SET OF CHAR := Lex.NonBlanks): TEXT 
  RAISES {Rd.Failure, Thread.Alerted};

PROCEDURE Skip(
    rd: Rd.T; READONLY cs: SET OF CHAR := Lex.Blanks)
  RAISES {Rd.Failure, Thread.Alerted};

END FastLex.
