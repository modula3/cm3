(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Jan  5 23:50:26 PST 1995 by najork  *)
(*      modified on Thu Sep 24 13:14:55 PDT 1992 by mhb     *)
(*      modified on Tue Aug  4 13:58:14 PDT 1992 by guarino *)

MODULE BM;

IMPORT Algorithm, AlgsBase, StringSearchAlgClass, StringSearchIE,
       Text, TextF, Thread, ZeusPanel;

TYPE T = StringSearchAlgClass.T BRANDED OBJECT OVERRIDES run := Run; END;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  VAR
    pattern, str: TEXT;                  (* pattern and string *)
    m, n        : CARDINAL;              (* their length *)
    i, j        : INTEGER               := 0;
    skip        : ARRAY CHAR OF INTEGER;
  BEGIN
    AlgsBase.GetData(alg, pattern, str);
    m := Text.Length(pattern);
    n := Text.Length(str);
    IF m = 0 OR n = 0 THEN RETURN; END;
    StringSearchIE.Setup(alg, pattern, str);
    skip := InitSkip(pattern);
    n := LAST(pattern^) - 1;
    m := LAST(str^) - 1;
    i := n;
    j := n;
    REPEAT
      StringSearchIE.Probe(alg, j, i);
      IF pattern[j] = str[i] THEN
        StringSearchIE.Result(alg, TRUE);
        StringSearchIE.PartialMatch(alg, j, i, n - j + 1);
        DEC(i);
        DEC(j);
      ELSE
        StringSearchIE.Result(alg, FALSE);
        StringSearchIE.PartialMatchClear(alg); 
        i := i + skip[str[i]];
        j := n;
        StringSearchIE.SlideTo(alg, i - n);
      END;
      IF j < 0 THEN
        StringSearchIE.CompleteMatch(alg, i + 1);
        INC(i, n + 2);
        j := n;
        StringSearchIE.SlideTo(alg, i - n);
      END;
    UNTIL i > m;
  END Run;

PROCEDURE InitSkip (pattern: TEXT): ARRAY CHAR OF INTEGER =
  VAR
    n             := NUMBER(pattern^) - 1;
    skip          :  ARRAY CHAR OF INTEGER ;
  BEGIN
    FOR c := FIRST(CHAR) TO LAST(CHAR) DO
      skip[c] := n;
    END;
    FOR j := FIRST(pattern^) TO LAST(pattern^) DO
      skip[pattern[j]] := n - j - 1;
    END;
    RETURN (skip);
  END InitSkip;

PROCEDURE New (): Algorithm.T =
  BEGIN
    RETURN
      NEW(
        T, data := ZeusPanel.NewForm("stringsearchinput.fv")).init();
  END New;

BEGIN
  ZeusPanel.RegisterAlg(New, "BoyerMoore", "StringSearch");
END BM.

