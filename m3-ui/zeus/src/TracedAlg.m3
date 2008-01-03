(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Jul  2 19:15:32 PDT 1992 by johnh*)
(*      modified on Wed May 13 07:51:16 1992 by mhb   *)

MODULE TracedAlg;

IMPORT Algorithm, List, Stdio, Thread, VBT, Wr, Zeus, ZeusClass, ZeusPanel;

REVEAL 
  T = Algorithm.T BRANDED OBJECT 
  OVERRIDES
    install := Install;
    delete := Delete;
    snapshot := Snapshot;
    restore := Restore;
    config := Config;
    startrun := Startrun;
    endrun := Endrun;
    run := Run;
    data := Data;
  END;   

PROCEDURE Install (<* UNUSED *> alg: T) =
  BEGIN Trace ("install") END Install;

PROCEDURE Delete (<* UNUSED *> alg: T) =
  BEGIN Trace ("delete") END Delete;

PROCEDURE Snapshot (<* UNUSED *> alg: T; <* UNUSED *> wr: Wr.T) =
  BEGIN Trace ("snapshot") END Snapshot;

PROCEDURE Restore (<* UNUSED *> alg: T; <* UNUSED *> l: List.T) =
  BEGIN Trace ("restore") END Restore;

PROCEDURE Config (
    <* UNUSED *> alg: T; 
    <* UNUSED *> state: ZeusClass.StateChange; 
    <* UNUSED *> o: ZeusClass.T) =
  BEGIN Trace ("config") END Config;

PROCEDURE Startrun (<* UNUSED *> alg: T) =
  BEGIN Trace ("startrun") END Startrun;

PROCEDURE Endrun (<* UNUSED *> alg: T) =
  BEGIN Trace ("endrun") END Endrun;

PROCEDURE Run (<* UNUSED *> alg: T) RAISES {Thread.Alerted} =
  BEGIN Trace ("run") END Run;

PROCEDURE Data (<* UNUSED *> alg: T): VBT.T =
  BEGIN Trace ("data"); RETURN NIL END Data;

PROCEDURE Trace (t: TEXT) =
  <* FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    LOCK Zeus.stdoutMu DO
      Wr.PutText (Stdio.stdout, "alg: " & t & "\n");
      Wr.Flush (Stdio.stdout);
    END
  END Trace;
 

PROCEDURE New (): Algorithm.T = 
  BEGIN RETURN NEW(T) END New;

BEGIN
  ZeusPanel.RegisterAlg (New, "Traced Alg")  
END TracedAlg.
 
