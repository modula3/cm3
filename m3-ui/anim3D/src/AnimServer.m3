(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Sep  4 15:24:03 PDT 1995 by najork                   *)
(*       Created on Tue May 24 11:27:39 PDT 1994 by najork                   *)


MODULE AnimServer;

IMPORT Anim3D, AnimHandle, AnimHandlePrivate, Axis, Fmt, GO, GOPrivate, 
       GraphicsBasePrivate, HVSplit, ParseParams, RootGO, RootGOPrivate, 
       Stdio, TextVBT, Thread, Time, Trestle, TrestleComm, WeakRef, Wr;

TYPE
  Closure = Thread.SizedClosure BRANDED OBJECT
  OVERRIDES
    apply := Apply;
  END;

  RootList = REF RECORD
    head : RootGO.T;
    tail : RootList;
  END;

  HandleList = REF RECORD
    head : AnimHandle.T;
    tail : HandleList;
  END;

VAR
  roots   : RootList   := NIL;
  handles : HandleList := NIL;
  handles_lock         := NEW (MUTEX);

  server_id: Thread.T := NIL;  (* for debugging purposes *)


(* For debugging purposes. Meant to be used in an ASSERT *)
PROCEDURE IsServer(): BOOLEAN =
  BEGIN
    RETURN Thread.Self () = server_id;
  END IsServer;


(* "Apply" is the main procedure of the animation server thread. 
   This thread terminates only when the program terminates. *)
PROCEDURE Apply (<* UNUSED *> self : Closure) : REFANY =

  PROCEDURE SignalExpiredHandles (VAR handles : HandleList; now : LONGREAL) =
    BEGIN
      IF handles # NIL THEN
        WITH ah = handles.head DO
          IF ah.endtime <= now THEN
            Thread.Signal (ah.cv);
            handles := handles.tail;
            SignalExpiredHandles (handles, now);
          ELSE
            SignalExpiredHandles (handles.tail, now);
          END;
        END;
      END;
    END SignalExpiredHandles;
    
  VAR
    now      : LONGREAL;
    damaged  : BOOLEAN;
    tmpRoots : RootList;
    timer    : Timer := NEW (Timer).init();
  BEGIN
    server_id := Thread.Self();  (* for debugging purposes *)
    LOOP
      now := Anim3D.Now ();

      tmpRoots := roots;
      WHILE tmpRoots # NIL DO
        tmpRoots.head.base.processEvents ();
        tmpRoots := tmpRoots.tail;
      END;

      LOCK externalLock DO
        LOCK internalLock DO

          tmpRoots := roots;
          WHILE tmpRoots # NIL DO
            WITH root = tmpRoots.head DO
              IF root.base.status = GraphicsBasePrivate.Status.Destroyed THEN
                root.base.unmap ();
                RemoveRootGO (root);
              END;
            END;
            tmpRoots := tmpRoots.tail;
          END;

          IF SolverHook # NIL THEN
            TRY 
              IF NOT SolverHook (now) THEN
                ReportError ("Could not solve constraint system");
              END;
            EXCEPT 
              SolverError (msg) => ReportError(msg);
            END;
          END;

          tmpRoots := roots;
          WHILE tmpRoots # NIL DO
            tmpRoots.head.adjust (now);
            tmpRoots := tmpRoots.tail;
          END;

          tmpRoots := roots;
          damaged := FALSE;
          WHILE tmpRoots # NIL DO
            tmpRoots.head.base.repair (damaged);
            tmpRoots := tmpRoots.tail;
          END;

          tmpRoots := roots;
          WHILE tmpRoots # NIL DO
            tmpRoots.head.undamage ();
            tmpRoots := tmpRoots.tail;
          END;

        END;
      END;

      LOCK handles_lock DO
        SignalExpiredHandles (handles, now);
      END;
      
      IF NOT damaged THEN
        Thread.Pause (0.1d0);
      END;

      IF timer.active THEN
        timer.click();
      END;
    END; (* This is an endless-loop! *)
  END Apply;


PROCEDURE RegisterRootGO (root : RootGO.T) =
  BEGIN
    (*** Must be protected from interference with the animation server ***)
    LOCK internalLock DO
      roots := NEW (RootList, head := root, tail := roots);
    END;
  END RegisterRootGO;


PROCEDURE RemoveRootGO (root : RootGO.T) =

  PROCEDURE RecursiveRemove (root : RootGO.T; VAR list : RootList) =
    BEGIN
      <* ASSERT list # NIL *>
      IF list.head = root THEN
        list := list.tail;
      ELSE
        RecursiveRemove (root, list.tail);
      END;
    END RecursiveRemove;

  BEGIN
    (*** Must be protected from interference with the animation server ***)
    RecursiveRemove (root, roots);
  END RemoveRootGO;


VAR
  nextdl := 0;


PROCEDURE NewDisplayList (go: GO.T): INTEGER =
  BEGIN
    INC (nextdl);
    EVAL WeakRef.FromRef (go, FreeDisplayList);
    RETURN nextdl;
  END NewDisplayList;


PROCEDURE FreeDisplayList (<*UNUSED*> READONLY wr: WeakRef.T; r: REFANY) =
  VAR
    tmpRoots : RootList;
  BEGIN
    WITH go = NARROW (r, GO.T) DO
      WHILE tmpRoots # NIL DO
        tmpRoots.head.base.freeDisplayList (go);
        tmpRoots := tmpRoots.tail;
      END;
    END;
  END FreeDisplayList;      


PROCEDURE PauseAnimHandle (ah : AnimHandle.T) =
  BEGIN
    LOCK handles_lock DO
      handles := NEW (HandleList, head := ah, tail := handles);
      Thread.Wait (handles_lock, ah.cv);
    END;
  END PauseAnimHandle;


PROCEDURE SetErrorWr (wr : Wr.T) =
  BEGIN
    animerr := wr;
  END SetErrorWr;


PROCEDURE ReportError (msg : TEXT) =
  <* FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    Wr.PutText (animerr, msg & "\n");
  END ReportError;


VAR 
  animerr : Wr.T;

TYPE
  Timer = OBJECT
    active : BOOLEAN := FALSE;
    text1  : TextVBT.T;
    text2  : TextVBT.T;
    framecounter : LONGREAL := 0.0d0;
    geomAvg      : LONGREAL := 0.0d0;
    serverstart  : LONGREAL;
    now          : LONGREAL;
  METHODS
    init (): Timer  := InitTimer;
    click ()        := ClickTimer;
  END;


PROCEDURE InitTimer (self : Timer) : Timer =
  BEGIN
    IF NEW(ParseParams.T).init(Stdio.stderr).keywordPresent("@O3Dshowtime") THEN
      self.active := TRUE;
      self.serverstart := Time.Now();
      self.now := self.serverstart;
      self.text1 := TextVBT.New (" ", hmargin := 5.0, vmargin := 5.0);
      self.text2 := TextVBT.New (" ", hmargin := 5.0, vmargin := 5.0);
      WITH vsplit = HVSplit.Cons (Axis.T.Ver, self.text1, self.text2) DO
        TRY
          Trestle.Install (vsplit);
        EXCEPT
          TrestleComm.Failure => self.active := FALSE;
        END;
      END;
    END;
    RETURN self;
  END InitTimer;


PROCEDURE ClickTimer (self : Timer) =
  BEGIN
    WITH fc    = self.framecounter, 
         geom  = self.geomAvg,
         now   = Time.Now(),
         total = (now - self.serverstart) / fc DO
      fc := fc + 1.0d0;
      geom := (geom + (now - self.now)) * 0.5d0;
      self.now := now;
      TextVBT.Put (self.text1, 
                   "Total Avg: " & Fmt.LongReal(total) & " sec/frame");
      TextVBT.Put (self.text2, 
                   "Geom. Avg: " & Fmt.LongReal(geom) & " sec/frame");
    END;
 END ClickTimer;    


BEGIN
  roots        := NIL;
  handles      := NIL;
  handles_lock := NEW (MUTEX);

  animerr := Stdio.stderr;

  internalLock := NEW (MUTEX);
  externalLock := NEW (MUTEX);

  EVAL Thread.Fork (NEW (Closure, stackSize := 20000));  
                                          (* start animation server thread *)
END AnimServer.
