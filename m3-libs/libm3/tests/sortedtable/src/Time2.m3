(* Copyright 1990 Digital Equipment Corporation.            *)
(* Distributed only by permission.                          *)
(* Last modified on Wed Oct  7 16:19:53 PDT 1992 by johnh   *)
(*      modified on Wed Apr 11 10:50:38 PDT 1990 by mcjones *)

(* This module has not been fully converted to M3 *)

MODULE TimingComparison EXPORTS Main;
IMPORT Fmt;
IMPORT IntTable, List, OS, SortedIntTable, SortedTextTable, Stdio, Table,
  Text, TextExtra, TextTable, Time;


PROCEDURE PrintTimes (elapsed, cpu: Time.T) =
  VAR elapsedTenths: CARDINAL; cpuTenths: CARDINAL;
  BEGIN
    elapsedTenths := (elapsed.microseconds + 50000) DIV 100000;
    cpuTenths := (cpu.microseconds + 50000) DIV 100000;
    Wr.PutText (Stdio.stdout, Fmt.Pad (Fmt.Int (elapsed.seconds), 3) & "."
                             & Fmt.Int (elapsedTenths)
                             & " elapsed seconds, ");
    Wr.PutText (Stdio.stdout, Fmt.Pad (Fmt.Int (cpu.seconds), 3) & "."
                             & Fmt.Int (cpuTenths) & " cpu seconds");
    Wr.PutText (Stdio.stdout, ".\n")
  END PrintTimes;

PROCEDURE TimeInts (N: CARDINAL) =
  VAR
    elapsed0, elapsed1: Time.T;
    info: OS.ProcessInfo;
    cpu0, cpu1: Time.T;
    st: SortedIntTable.T;
    i, key: SortedIntTable.Key;
    s: SortedIntTable.Stream;
    v: SortedIntTable.Value;
    ht: IntTable.T;
    l: List.T;
  BEGIN
    st := SortedIntTable.New ();
    OS.GetProcessInfo (OS.NullPID, info);
    cpu0 := info.rUsage[OS.RWho.Self].userTime;
    elapsed0 := Time.Now ();
    FOR z_3 := 1 TO N DO
      i := z_3;
      IF SortedIntTable.Put (st, i, NIL) THEN END
    END;
    OS.GetProcessInfo (OS.NullPID, info);
    elapsed1 := Time.Subtract (Time.Now (), elapsed0);
    cpu1 := Time.Subtract (info.rUsage[OS.RWho.Self].userTime, cpu0);
    Wr.PutText (Stdio.stdout, "n=" & Fmt.Pad (Fmt.Int (N), 6)
                             & " SortedTable insertion   ");
    PrintTimes (elapsed1, cpu1);
    OS.GetProcessInfo (OS.NullPID, info);
    cpu0 := info.rUsage[OS.RWho.Self].userTime;
    elapsed0 := Time.Now ();
    s := SortedIntTable.NewStream (st, TRUE, 1);
    WHILE SortedIntTable.Next (s, key, v) DO END;
    OS.GetProcessInfo (OS.NullPID, info);
    elapsed1 := Time.Subtract (Time.Now (), elapsed0);
    cpu1 := Time.Subtract (info.rUsage[OS.RWho.Self].userTime, cpu0);
    Wr.PutText (Stdio.stdout, "n=" & Fmt.Pad (Fmt.Int (N), 6)
                             & " SortedTable enumeration ");
    PrintTimes (elapsed1, cpu1);
    ht := IntTable.New ();
    OS.GetProcessInfo (OS.NullPID, info);
    cpu0 := info.rUsage[OS.RWho.Self].userTime;
    elapsed0 := Time.Now ();
    FOR z_4 := 1 TO N DO i := z_4; IF IntTable.Put (ht, i, NIL) THEN END END;
    OS.GetProcessInfo (OS.NullPID, info);
    elapsed1 := Time.Subtract (Time.Now (), elapsed0);
    cpu1 := Time.Subtract (info.rUsage[OS.RWho.Self].userTime, cpu0);
    Wr.PutText (Stdio.stdout, "n=" & Fmt.Pad (Fmt.Int (N), 6)
                             & " Table       insertion   ");
    PrintTimes (elapsed1, cpu1);
    OS.GetProcessInfo (OS.NullPID, info);
    cpu0 := info.rUsage[OS.RWho.Self].userTime;
    elapsed0 := Time.Now ();
    l := List.Sort (IntTable.ToAssocList (ht));
    WHILE l # NIL DO
              (* Visit l^.first. *)
      l := l.tail
    END;
    OS.GetProcessInfo (OS.NullPID, info);
    elapsed1 := Time.Subtract (Time.Now (), elapsed0);
    cpu1 := Time.Subtract (info.rUsage[OS.RWho.Self].userTime, cpu0);
    Wr.PutText (Stdio.stdout, "n=" & Fmt.Pad (Fmt.Int (N), 6)
                             & " Table       enumeration ");
    PrintTimes (elapsed1, cpu1);

  END TimeInts;

PROCEDURE TimeTexts (N: CARDINAL) =
  VAR
    elapsed0, elapsed1: Time.T;
    info: OS.ProcessInfo;
    cpu0, cpu1: Time.T;
    st: SortedTextTable.T;
    i: INTEGER;
    key: SortedTextTable.Key;
    s: SortedTextTable.Stream;
    v: SortedTextTable.Value;
    ht: TextTable.T;
    l: List.T;
  BEGIN
    st := SortedTextTable.New ();
    OS.GetProcessInfo (OS.NullPID, info);
    cpu0 := info.rUsage[OS.RWho.Self].userTime;
    elapsed0 := Time.Now ();
    FOR z_5 := 1 TO N DO
      i := z_5;
      IF SortedTextTable.Put (st, TextExtra.FromInt (i), NIL) THEN END
    END;
    OS.GetProcessInfo (OS.NullPID, info);
    elapsed1 := Time.Subtract (Time.Now (), elapsed0);
    cpu1 := Time.Subtract (info.rUsage[OS.RWho.Self].userTime, cpu0);
    Wr.PutText (Stdio.stdout, "n=" & Fmt.Pad (Fmt.Int (N), 6)
                             & " SortedTable insertion   ");
    PrintTimes (elapsed1, cpu1);
    OS.GetProcessInfo (OS.NullPID, info);
    cpu0 := info.rUsage[OS.RWho.Self].userTime;
    elapsed0 := Time.Now ();
    s := SortedTextTable.NewStream (st, TRUE, "");
    WHILE SortedTextTable.Next (s, key, v) DO END;
    OS.GetProcessInfo (OS.NullPID, info);
    elapsed1 := Time.Subtract (Time.Now (), elapsed0);
    cpu1 := Time.Subtract (info.rUsage[OS.RWho.Self].userTime, cpu0);
    Wr.PutText (Stdio.stdout, "n=" & Fmt.Pad (Fmt.Int (N), 6)
                             & " SortedTable enumeration ");
    PrintTimes (elapsed1, cpu1);
    ht := TextTable.New ();
    OS.GetProcessInfo (OS.NullPID, info);
    cpu0 := info.rUsage[OS.RWho.Self].userTime;
    elapsed0 := Time.Now ();
    FOR z_6 := 1 TO N DO
      i := z_6;
      IF TextTable.Put (ht, TextExtra.FromInt (i), NIL) THEN END
    END;
    OS.GetProcessInfo (OS.NullPID, info);
    elapsed1 := Time.Subtract (Time.Now (), elapsed0);
    cpu1 := Time.Subtract (info.rUsage[OS.RWho.Self].userTime, cpu0);
    Wr.PutText (Stdio.stdout, "n=" & Fmt.Pad (Fmt.Int (N), 6)
                             & " Table       insertion   ");
    PrintTimes (elapsed1, cpu1);
    OS.GetProcessInfo (OS.NullPID, info);
    cpu0 := info.rUsage[OS.RWho.Self].userTime;
    elapsed0 := Time.Now ();
    l := List.Sort (TextTable.ToAssocList (ht));
    WHILE l # NIL DO
              (* Visit l^.first. *)
      l := l.tail
    END;
    OS.GetProcessInfo (OS.NullPID, info);
    elapsed1 := Time.Subtract (Time.Now (), elapsed0);
    cpu1 := Time.Subtract (info.rUsage[OS.RWho.Self].userTime, cpu0);
    Wr.PutText (Stdio.stdout, "n=" & Fmt.Pad (Fmt.Int (N), 6)
                             & " Table       enumeration ");
    PrintTimes (elapsed1, cpu1);

  END TimeTexts;

BEGIN
  TimeTexts (1000);
  TimeTexts (10000);
  TimeTexts (100000);
  (*
    TimeInts(1000);
    TimeInts(10000);
    TimeInts(100000)
  *)
END TimingComparison.
