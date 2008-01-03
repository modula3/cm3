(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Thu Feb  9 09:05:43 PST 1995 by kalsow     *)
(*      modified on Thu Jan  5 22:24:48 PST 1995 by najork     *)
(*      modified on Thu Jul 16 22:58:16 1992 by mhb            *)
(*      modified on Mon Jun  8 19:46:54 1992 by steveg         *)

INTERFACE Match;

IMPORT VBT;

CONST
  Last = 16;

TYPE
  State = {Hide, Clue, Reveal};

PROCEDURE StateName(state: State): TEXT;


TYPE 
  Clues = ARRAY [1..Last] OF TEXT;

PROCEDURE ClueName (READONLY clues: Clues): TEXT;

PROCEDURE FmtMouseRec(READONLY cd: VBT.MouseRec): TEXT;

END Match.
