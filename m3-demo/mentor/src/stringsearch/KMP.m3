(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Jan  5 23:50:15 PST 1995 by najork  *)
(*      modified on Sat Oct 24 03:25:10 PDT 1992 by broder  *)
(*      modified on Thu Sep 24 13:14:29 PDT 1992 by mhb     *)
(*      modified on Fri Aug  7 16:28:58 PDT 1992 by guarino *)

MODULE KMP;

IMPORT Algorithm, StringSearchAlgClass, StringSearchIE, Text,
       Thread, ZeusPanel, AlgsBase;

REVEAL 
  T = StringSearchAlgClass.T BRANDED OBJECT OVERRIDES run := Run; END;


PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  VAR
    p, s: TEXT;                 (* pattern and string *)
    m, n: CARDINAL;             (* their length *)
    next: REF ARRAY OF INTEGER;
    i   : CARDINAL             := 0; (* index in pattern *)
  BEGIN
    AlgsBase.GetData(alg, p, s);
    m := Text.Length(p);
    n := Text.Length(s);
    IF m = 0 OR n = 0 THEN RETURN; END;
    next := InitNext(alg, p);
    ZeusPanel.Pause(alg, "KMP precomputing done");
    StringSearchIE.Setup(alg, p, s);

    (**
        This is the basic algorithm i := 0;
        FOR j := 0 TO n - m DO
        (* Invariant invalidated by INC(j) *)
          WHILE (i > 0) AND (p[i] # s[j]) DO i := next[i]; END;
          IF p[i] = s[j] THEN INC(i) END;
        (* Invariant: p[0..i-1] is the longest prefix of p that is a
          suffix of s[0..j] *)
          IF i = m THEN (*Match*) i := next[i] END;
        END;
    **)

    FOR j := 0 TO n - m DO
      WHILE (i > 0) AND (Text.GetChar(p, i) # Text.GetChar(s, j)) DO
        StringSearchIE.Probe(alg, i, j);
        StringSearchIE.Result(alg, FALSE);
        StringSearchIE.PartialMatchClear(alg);
        i := next[i];
        StringSearchIE.SlideTo(alg, j - i);
        StringSearchIE.PartialMatch(alg, 0, j - i, i);
      END;
      StringSearchIE.Probe(alg, i, j);
      IF Text.GetChar(p, i) = Text.GetChar(s, j) THEN
        StringSearchIE.Result(alg, TRUE);
        INC(i);
        StringSearchIE.PartialMatch(alg, 0, j - i + 1, i);
      ELSE                      (* i = 0 *)
        StringSearchIE.Result(alg, FALSE);
        StringSearchIE.PartialMatchClear(alg);
        StringSearchIE.SlideTo(alg, j + 1);
      END;
      IF i = m THEN
        StringSearchIE.CompleteMatch(alg, j - i + 1);
        i := next[i];
        StringSearchIE.SlideTo(alg, j - i + 1);
        StringSearchIE.PartialMatch(alg, 0, j - i + 1, i);
      END;
    END;
  END Run;

(* Compute the next function: next[i] == the length of the maximum proper
   suffix of p[0..i-1] that is a prefix of p *)


PROCEDURE InitNext (alg: T; p: TEXT): REF ARRAY OF INTEGER
  RAISES {Thread.Alerted} =
  VAR
    m    := Text.Length(p);
    next := NEW(REF ARRAY OF INTEGER, m + 1);
    i    := 0;
  BEGIN
    StringSearchIE.KMPSetup(alg, p); (* Must do before setup! *)
    StringSearchIE.Setup(alg, p, p);
    StringSearchIE.SlideTo(alg, 1);
    next[0] := 0;
    (* AddEdge(0,0) not needed *)
    next[1] := 0;
    StringSearchIE.AddEdge(alg, 1, 0);

    FOR j := 1 TO m - 1 DO
      WHILE (i > 0) AND (Text.GetChar(p, i) # Text.GetChar(p, j)) DO
        StringSearchIE.Probe(alg, i, j);
        StringSearchIE.Result(alg, FALSE);
        StringSearchIE.PartialMatchClear(alg);
        i := next[i];
        StringSearchIE.SlideTo(alg, j - i);
        StringSearchIE.PartialMatch(alg, 0, j - i, i);
      END;

      StringSearchIE.Probe(alg, i, j);
      IF Text.GetChar(p, i) = Text.GetChar(p, j) THEN
        StringSearchIE.Result(alg, TRUE);
        INC(i);
        StringSearchIE.PartialMatch(alg, 0, j - i + 1, i);
        next[j + 1] := i;
        StringSearchIE.AddEdge(alg, j + 1, i);
      ELSE                      (* i = 0 *)
        StringSearchIE.Result(alg, FALSE);
        StringSearchIE.PartialMatchClear(alg);
        next[j + 1] := i;
        StringSearchIE.AddEdge(alg, j + 1, i);
        StringSearchIE.SlideTo(alg, j + 1);
      END;
    END;
    StringSearchIE.PartialMatchClear(alg);
    RETURN (next);
  END InitNext;

PROCEDURE New (): Algorithm.T =
  BEGIN
    RETURN
      NEW(
        T, data := ZeusPanel.NewForm("stringsearchinput.fv")).init();
  END New;

BEGIN
  ZeusPanel.RegisterAlg(New, "KnuthMorrisPratt", "StringSearch");
END KMP.

