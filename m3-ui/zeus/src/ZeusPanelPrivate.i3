(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Sun Jul 18 12:01:40 PDT 1993 by mhb    *)
(*      modified on Wed Jun 23 15:04:55 PDT 1993 by steveg *)
(*      modified on Wed Feb 17 17:22:48 PST 1993 by johnh *)
<* PRAGMA LL *>

INTERFACE ZeusPanelPrivate;

IMPORT FormsVBT, RefList, Rsrc, Thread, VBT, View, Zeus,  ZeusClass;

TYPE
  RunState = {Virgin, Running, Stepping, Paused, Done, Aborted};

(* CONST *)
VAR stateIdle := ARRAY RunState OF BOOLEAN
            {TRUE, FALSE, FALSE, FALSE, TRUE, TRUE};

CONST Log10: LONGREAL = 2.3025850930d0; 

TYPE
  T = ROOT OBJECT
        title   : TEXT;
        path    : Rsrc.Path;
        fv      : FormsVBT.T;
        scale   : REAL := 1.0;  (* scale factor for control panels *)
        fvpath  : Rsrc.Path;    (* my internal path *)
        sessions: RefList.T;       (* of Session *)

        (* Used by "speedometer": *)
        speedFactor   : REAL     := 10.0;   (* load value from form *)
        logSpeedFactor: LONGREAL := Log10;
        delayTime     : REAL     := 0.0;    (* load value from form *)
        minDelayFrac  : REAL     := 0.0;    (* ditto *)
        codeDelayFrac : REAL     := 0.0;    (* ditto *)

        (* Used by "interpreter": *)
        panelThread: Thread.T;
        priority   : INTEGER    := 1;    (* load value from form *)
        mu         : MUTEX;     <* LL(mu) > VBT.mu *>
            (* LL ==> a thread with mu locked must not acquire VBT.mu *)
        runCond   : Thread.Condition;
        algCond   : Thread.Condition;
        runState  : RunState;
        numActive : CARDINAL           := 0;
            (* numActive = number of sess IN sessions s.t. sess.active *)
        numRunning: CARDINAL           := 0;
            (* numRunning = number of sessions s.t. sess.running.  
               A bug is possible here, if a session gets deleted, but keeps
               running, while panelThread is awakened and assumes that no
               sessions are running. *)
        mustSynch : BOOLEAN            := FALSE;
        clock     : CARDINAL           := 0;
        subclock  : CARDINAL           := 0;
        quit      : BOOLEAN            := FALSE;

        (* Used by "photo" and "album" *)
        album: VBT.T;
        cntViews: CARDINAL;
      END;

  Session = Zeus.Session OBJECT
              name      : TEXT;
              viewsToAdd: RefList.T (* of View.T *) := NIL; <* LL = VBT.mu *>
              inTrestle : BOOLEAN;
              fv        : FormsVBT.T;
              algThread : Thread.T;
              runCond   : Thread.Condition;
              feedCond  : Thread.Condition;
              feedbackOn: BOOLEAN                := FALSE;
              algIsSet: BOOLEAN := FALSE; (* alg not defaulted *)
              active: BOOLEAN := FALSE; (* alg started, not yet stopped *)
              running  : BOOLEAN    := FALSE; (* alg not paused *)
              waitUntil: CARDINAL;  (* used for event weights *)
              quit     : BOOLEAN    := FALSE;
            OVERRIDES
              pre  := PreEventCallback;
              post := PostEventCallback;
            END;

PROCEDURE NewSession (name     : TEXT;
                      panel    : T;
                      inTrestle: BOOLEAN;
                      pickAlg  : BOOLEAN   := TRUE);
  <* LL = VBT.mu *>
  (* Create a new session, as a new top-level window if inTrestle, in the
     Zeus panel otherwise. If pickAlg, call PickedAlg on the first alg assoc
     with the new session. *)

PROCEDURE PickedAlg (sess: Session; which: TEXT);
  <* LL = VBT.mu *>
  (* Select a new algorithm. *)

PROCEDURE PickedView (sess: Session; which: TEXT): View.T;
  <* LL = VBT.mu *>
  (* Create a new view. *)

PROCEDURE IsCodeView (which: TEXT; sess: Session; VAR file: TEXT):
  BOOLEAN;
  <* LL = arbitrary *>
  (* Is which a code view? *)


PROCEDURE DestroyAllSessions (panel: T);
(* LL = VBT.mu *)

PROCEDURE DestroySession (sess: Session);
(* LL = VBT.mu *)

PROCEDURE LoadFromPanel (panel: T);
(* loads parameter values from the control panel FormsVBT *)

PROCEDURE PrepForSnapshot(panel: T);
  (* Call this from ZeusSnapshot.m3 before a snapshot. *)

PROCEDURE OverrideRestore(panel: T);
  (* Call this from ZeusSnapshot.m3 after a restore to reset things
     that the restore operation shouldn't have changed, but may have. *)

PROCEDURE GroupInfoExists (sessName: TEXT): BOOLEAN;
(* TRUE iff a session has been registered under sessName, i.e., a GroupInfo
   record exists under the given name. *)

PROCEDURE PreEventCallback (sess     : Session;
                            initiator: ZeusClass.T;
                            style    : Zeus.EventStyle;
                            priority : INTEGER;
                            eventName: TEXT             )
  RAISES {Thread.Alerted};


PROCEDURE PostEventCallback (sess     : Session;
                             initiator: ZeusClass.T;
                             style    : Zeus.EventStyle;
                             priority : INTEGER;
                             eventName: TEXT             )
  (* LL <= VBT.mu *)
  RAISES {Thread.Alerted};

END ZeusPanelPrivate.
