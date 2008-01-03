(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Jan  5 23:50:02 PST 1995 by najork  *)
(*      modified on Thu Sep 24 13:14:26 PDT 1992 by mhb     *)
(*      modified on Tue Aug  4 12:09:51 PDT 1992 by guarino *)
(*      modified on Sun Aug  2 04:23:17 PDT 1992 by broder *)

MODULE BruteForce;

IMPORT Algorithm, StringSearchAlgClass, StringSearchIE, Text,
       Thread, ZeusPanel, AlgsBase;

TYPE T = StringSearchAlgClass.T BRANDED OBJECT OVERRIDES run := Run; END;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  VAR
    p, s: TEXT;                 (* pattern and string *)
    m, n: CARDINAL;             (* their length *)
    i   : CARDINAL := 0;        (* index in pattern *)
  BEGIN
    AlgsBase.GetData(alg, p, s);
    m := Text.Length(p);
    n := Text.Length(s);
    IF m = 0 OR n = 0 THEN RETURN; END;
    StringSearchIE.Setup(alg, p, s);

    FOR j := 0 TO n - m DO
      StringSearchIE.SlideTo(alg, j);
      i := 0;
      REPEAT
        StringSearchIE.Probe(alg, i, j + i);
        IF Text.GetChar(p,i) = Text.GetChar(s,j + i) THEN
          StringSearchIE.Result(alg, TRUE);
          StringSearchIE.PartialMatch(alg, 0, j, i + 1);
          INC(i);
        ELSE
          StringSearchIE.Result(alg, FALSE);
          StringSearchIE.PartialMatchClear(alg); 
          EXIT;
        END;
      UNTIL i = m;
      IF i = m THEN StringSearchIE.CompleteMatch(alg, j); END;
    END;
  END Run;

PROCEDURE New (): Algorithm.T =
  BEGIN
    RETURN
      NEW(
        T, data := ZeusPanel.NewForm("stringsearchinput.fv")).init();
  END New;

BEGIN
  ZeusPanel.RegisterAlg(New, "BruteForce", "StringSearch");
END BruteForce.

