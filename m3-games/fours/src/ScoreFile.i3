(* Copyright (C) 1995, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Jun 14 08:39:11 PDT 1995 by kalsow     *)

INTERFACE ScoreFile;

(* Maintains a file of scores indexed by user.

   ScoreFile maintains a file of scores indexed by user names.

   Index: games; scores *)

IMPORT Time;

TYPE
  FileName = TEXT;
  Player   = TEXT;
  EnumProc = PROCEDURE (p: Player; READONLY s: Score);

EXCEPTION
  Error (TEXT);

TYPE
  Score = RECORD
    best_date  : Time.T;
    best_level : INTEGER;
    best_rows  : INTEGER;
    best_score : INTEGER;
    n_games    : INTEGER;
    n_seconds  : Time.T;
  END;

PROCEDURE Get (f: FileName; p: Player; VAR(*OUT*) s: Score): BOOLEAN RAISES{Error};
(* returns the recorded score for player 'p' in file 'f'.  If no
   score is recorded, s is unchanged and Get returns FALSE. *)

PROCEDURE Put (f: FileName;  p: Player; READONLY s: Score)  RAISES{Error};
(* records score 's' for player 'p' in file 'f'. *)

PROCEDURE Enumerate (f: FileName;  e: EnumProc)  RAISES{Error};
(* calls 'e' with each player and score that is recorded in 'f'. *)

END ScoreFile.
