(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Sep 23 16:05:38 PDT 1993 by mhb    *)
(*      modified on Wed Jun 23 15:05:40 PDT 1993 by steveg *)
(*      modified on Fri Feb  5 20:37:12 PST 1993 by johnh  *)
<* PRAGMA LL *>

(* This internal interface provides the ZeusPanel snapshot and restore
   facilities. *)

INTERFACE ZeusSnapshot;

IMPORT RefList, Rd, Wr, ZeusPanelPrivate;

CONST
  HomeDir = "HOME";
  StateDir = ".zeusState"; 
  FinalState = "Final_State";   (* filename for state at "Quit" *)

PROCEDURE SessionToStateDir (sess  : ZeusPanelPrivate.Session;
                             report: BOOLEAN                    := TRUE)
  RAISES {};
  <* LL = VBT.mu *>

PROCEDURE SessionFromStateDir (panel : ZeusPanelPrivate.T;
                               name  : TEXT;
                               report: BOOLEAN := TRUE): BOOLEAN RAISES {};
  <* LL = VBT.mu *>

PROCEDURE Snapshot (panel : ZeusPanelPrivate.T;
                    file  : TEXT;
                    report: BOOLEAN              := TRUE) RAISES {};
  <* LL = VBT.mu *>

PROCEDURE SnapshotToWr (panel : ZeusPanelPrivate.T;
                        wr  : Wr.T;
                        report: BOOLEAN          := TRUE) RAISES {};
  <* LL = VBT.mu *>

PROCEDURE FinalSnapshot (panel : ZeusPanelPrivate.T) RAISES {};
  <* LL = VBT.mu *>
  (* Snapshot to default place at Zeus exit. *)

PROCEDURE Restore (panel : ZeusPanelPrivate.T;
                   file  : TEXT;
                   report: BOOLEAN              := TRUE) RAISES {};
  <* LL = VBT.mu *>

PROCEDURE RestoreFromRd (panel : ZeusPanelPrivate.T;
                         rd    : Rd.T;
                         report: BOOLEAN              := TRUE;
                         file  : TEXT                 := NIL   ) RAISES {};
  <* LL = VBT.mu *>
  (* Restore(p, file) == RestoreFromRd(p, FileStream.OpenRead(file)) *)
  (* The file argument is just to be used in error messages. *)

PROCEDURE RestoreFromList (panel : ZeusPanelPrivate.T;
                           list  : RefList.T;
                           report: BOOLEAN      := TRUE) RAISES {};
  <* LL = VBT.mu *>
  (* RestoreFromRd(p, rd) == RestoreFromList(p, Sx.Read(rd)) *)

PROCEDURE InitialRestore (panel : ZeusPanelPrivate.T) RAISES {};
  <* LL = VBT.mu *>
  (* Restore from default place at Zeus startup. *)


(* Procedures to save and restore panel and algorithm data, ignoring
   views completely, under the assumption that the sessions at the time
   of snapshot can be mapped to those in panel.sessions at the time of
   restoration, in the same order. *)

PROCEDURE GrabDataList(panel: ZeusPanelPrivate.T): REFANY;
  (* Returns a list. *)

PROCEDURE RestoreData(panel: ZeusPanelPrivate.T; list: RefList.T);
  (* Restores from a list. *)


END ZeusSnapshot.
