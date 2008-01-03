(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Wed Feb  8 16:40:40 PST 1995 by kalsow     *)
(*      modified on Thu Jan  5 22:32:50 PST 1995 by najork     *)
(*      modified on Fri Jul 17 09:15:22 1992 by mhb            *)
(*      modified on Tue Jun  9 22:44:06 1992 by steveg         *)

MODULE AlgMatch; 

IMPORT Algorithm, Match, MatchAlgClass, MatchIE, Thread, VBT, ZeusPanel;

TYPE T = MatchAlgClass.T BRANDED OBJECT
    ans: ARRAY[1..Last] OF INTEGER;
    state: ARRAY[1..Last] OF Match.State;
    first, second: INTEGER := 0;
  OVERRIDES
    run := Run;
    feSelected := Selected;
  END;

CONST
  Last = Match.Last;

CONST
  NewState = ARRAY BOOLEAN OF
               Match.State{Match.State.Hide, Match.State.Reveal};

PROCEDURE Selected (alg: T; i: INTEGER; READONLY cd: VBT.MouseRec) =
  <*FATAL Thread.Alerted*>
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      IF alg.second # 0 THEN
        WITH newState = NewState[alg.ans[alg.first] = alg.second] DO
          MatchIE.SetState(alg, alg.first, alg.second, newState);
          alg.state[alg.first] := newState;
          alg.state[alg.second] := newState;
          alg.first := 0;
          alg.second := 0;
        END
      ELSIF i # 0 THEN
        IF alg.state[i] = Match.State.Hide THEN
          MatchIE.SetState(alg, i, i, Match.State.Clue);
          alg.state[i] := Match.State.Clue;
          IF alg.first = 0 THEN
            alg.first := i;
          ELSE
            alg.second := i;
          END
        END
      END
    END
  END Selected;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  BEGIN
(*
      1 "douze",         "Paris",
      3 "Shi er",        "Beijing",
      5 "Dua belas",     "Kuala Lumpur",
      7 "T'jour Ney",    "Kyoto",
      9 "Shtaim Esrey",  "Jerusalem",
      11 "Doisprezece",  "Bucharest",
      13 "Dodici",       "Florence",
      15 "Zwolf",        "Munich"
      };
    MatchIE.Init (alg, Match.Clues{
      "Beijing",   "T'jour Ney",  "Kyoto",        "Bucharest",
      "Paris",     "Shi er",      "Kuala Lumpur", "Shtaim Esrey",
      "Dua belas", "douze",       "Doisprezece",  "Florence",
      "Dodici",    "Zwolf",       "Jerusalem",    "Munich"});
    alg.ans := ARRAY[1..16] OF INTEGER{
       6,  3,  2, 11,
      10,  1,  9, 15,
       7,  5,  4, 13,
      12, 16,  8, 14};
*)

    alg.ans := ARRAY[1..16] OF INTEGER{
      15, 12,  5, 13,
       3, 10, 14, 16,
      11,  6,  9,  2,
       4,  7,  1,  8};

    FOR i := 1 TO Last DO
      alg.state[i] := Match.State.Hide;
    END;

    MatchIE.Init (alg, Match.Clues{
      "ZEBRA",   "DUCK",  "CAT",    "BIRD",
      "CAT",     "COW",   "DOG",    "LLAMA",
      "TIGER",   "COW",   "TIGER",  "DUCK",
      "BIRD",    "DOG",   "ZEBRA",  "LLAMA"});

    WHILE NOT AllRevealed(alg) DO 
      IF Thread.TestAlert() THEN RAISE Thread.Alerted END;
      Thread.Pause (2.0d0);
    END;
  END Run;

PROCEDURE AllRevealed (alg: T): BOOLEAN =
  BEGIN
    FOR i := 1 TO 16 DO
      IF alg.state[i] # Match.State.Reveal THEN RETURN FALSE END;
    END;
    RETURN TRUE;
  END AllRevealed;

PROCEDURE MatchNew (): Algorithm.T =
  BEGIN
    RETURN NEW(T).init();
  END MatchNew;

BEGIN
  ZeusPanel.RegisterAlg(MatchNew, "Match", "Match");
END AlgMatch.













