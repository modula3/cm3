(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Thu Jan  5 22:24:34 PST 1995 by najork       *)
(*      modified on Thu Jul 16 07:41:51 1992 by mhb          *)
(*      modified on Thu Jun  4 23:56:54 1992 by steveg       *)

MODULE Match;

IMPORT VBT;

CONST
  Names = ARRAY State OF TEXT{"Hide", "Clue", "Reveal"};

PROCEDURE StateName(state: State): TEXT =
  BEGIN RETURN Names[state] END StateName;

PROCEDURE ClueName(<* UNUSED *> READONLY clues: Clues): TEXT =
  BEGIN RETURN "" END ClueName;

PROCEDURE FmtMouseRec(<*UNUSED *> READONLY cd: VBT.MouseRec): TEXT =
  BEGIN
    RETURN "Match.m3: Too lazy to format a VBT.MouseRec";
  END FmtMouseRec;

BEGIN
END Match.
