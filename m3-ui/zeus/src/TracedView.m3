(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Jul  2 19:16:22 PDT 1992 by johnh*)
(*      modified on Wed May 13 06:21:18 1992 by mhb   *)

MODULE TracedView;

IMPORT Filter, List, Stdio, TextVBT, Thread, View, Wr, 
    Zeus, ZeusClass, ZeusPanel;

REVEAL 
  T = View.T BRANDED OBJECT 
  OVERRIDES
    install := Install;
    delete := Delete;
    snapshot := Snapshot;
    restore := Restore;
    config := Config;
    startrun := Startrun;
    endrun := Endrun;
  END;   

PROCEDURE New (): View.T =
  BEGIN RETURN NEW(T) END New;

PROCEDURE Install (view: T) =
  BEGIN 
    Trace ("install");  
    EVAL Filter.Replace(view, TextVBT.New("See stdout"));
    View.T.install(view);
  END Install;

PROCEDURE Delete (view: T) =
  BEGIN 
    Trace ("delete");
    View.T.delete (view);
   END Delete;

PROCEDURE Snapshot (view: T; wr: Wr.T) =
  BEGIN 
    Trace ("snapshot");
    View.T.snapshot (view, wr);
   END Snapshot;

PROCEDURE Restore (view: T; l: List.T) =
  BEGIN 
    Trace ("restore");
    View.T.restore (view, l);
   END Restore;

PROCEDURE Config (
    view: T; 
    state: ZeusClass.StateChange; 
    o: ZeusClass.T) =
  BEGIN 
    Trace ("config");
    View.T.config (view, state, o);
   END Config;

PROCEDURE Startrun (view: T) =
  BEGIN 
    Trace ("startrun");
    View.T.startrun (view);
   END Startrun;

PROCEDURE Endrun (view: T) =
  BEGIN 
    Trace ("endrun");
    View.T.endrun (view);
  END Endrun;

PROCEDURE Trace (t: TEXT) =
  BEGIN
    LOCK Zeus.stdoutMu DO
      TRY
        Wr.PutText(Stdio.stdout, "view: " & t & "\n");
        Wr.Flush(Stdio.stdout);
      EXCEPT
        Wr.Failure, Thread.Alerted =>
      END;
    END
  END Trace;
 
BEGIN
  ZeusPanel.RegisterView (New, "Traced View")  
END TracedView.
 
