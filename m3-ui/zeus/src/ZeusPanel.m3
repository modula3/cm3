(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Tue Jan 31 13:23:34 PST 1995 by kalsow   *)
(*      modified on Mon Jan  9 14:07:43 PST 1995 by mhb      *)
(*      modified on Mon Jul 19 17:07:14 PDT 1993 by steveg   *)
(*      modified on Thu Feb 18 16:15:36 PST 1993 by johnh    *)
(*      modified on Fri Aug  7 21:45:26 PDT 1992 by meehan   *)
(*      modified on Fri Jul 31  5:03:25 PDT 1992 by sclafani *)
(*      modified on Wed Jul  1 10:09:55 PDT 1992 by tt    *)

<* PRAGMA LL *>

MODULE ZeusPanel EXPORTS ZeusPanel, ZeusPanelFriends, ZeusPanelPrivate;

IMPORT AlbumVBT, Algorithm, AlgorithmClass, Animate, Atom, Axis, Classes,
       DataView, FileRd, FileWr, FlexVBT, FloatMode, Fmt, FormsVBT, RefList,
       RefListSort, RefListUtils, Lex, ListVBT, Math, MultiFilter, OSError,
       ScrollerVBT, Params, Rd, Rsrc, ScaleFilter, Stdio, Sx, Text,
       TextEditVBT, TextList, TextPort, TextRd, TextWr, Thread, Trestle,
       TrestleComm, VBT, View, ViewClass, ViewportVBT, Wr, Zeus, ZeusBundle,
       ZeusClass, ZeusCodeView, ZeusPanelFriends, ZeusPrivate, ZeusSnapshot;


VAR
  me: VBT.T; (* This is the VBT installed into Trestle *)

VAR ControlPanel: T;

<*FATAL FormsVBT.Error, FormsVBT.Unimplemented, 
        TrestleComm.Failure, 
        Zeus.Error, Zeus.Locked, 
        Thread.Alerted, 
        OSError.E, Wr.Failure, Rd.Failure *>


(* **************** Control Panel Form **************** *)

PROCEDURE NewPanel (): T =
  <* LL = VBT.mu *>
  VAR panel: T;

  PROCEDURE Attach (name: TEXT; proc: FormsVBT.Proc) =
    BEGIN
      FormsVBT.AttachProc(panel.fv, name, proc, panel);
    END Attach;

  BEGIN
    panel := NEW(T,
                 (* InitInterpreter *)
                 mu := NEW(MUTEX), runCond := NEW(Thread.Condition),
                 algCond := NEW(Thread.Condition));
    panel.fvpath := Rsrc.BuildPath("$ZEUSPATH", ZeusBundle.Get());
    panel.fv := NewForm("zeusPanel.fv", panel.fvpath);
    me := panel.fv;

    Attach("quit", QuitP);
    Attach("goBtn", GoP);
    Attach("stepBtn", StepP);
    Attach("abortBtn", AbortP);
    FormsVBT.MakeDormant(panel.fv, "goBtn");
    FormsVBT.MakeDormant(panel.fv, "stepBtn");
    FormsVBT.MakeDormant(panel.fv, "abortBtn");

    Attach("delay", SpeedP);
    Attach("minDelayFrac", MinDelayP);
    Attach("codeDelayFrac", CodeDelayP);
    Attach("maxSpeedFactor", SpeedFactorP);

    Attach("errClear", ErrClearP);
    Attach("errClearAndShut", ErrClearP);

    Attach("priority", PriorityP);

    Attach("snapshot", SnapshotP);
    Attach("restore", RestoreP);
    Attach("restoreShortcut", RestoreP);
    Attach("photoBtn", PhotoP);
    Attach("clearAlbum", ClearAlbumP);
    Attach("delViews", DelAllViewsP);

    Attach("recordBtn", RecordBtnP);
    Attach("record", RecordP);
    Attach("grabData", GrabDataP);
    Attach("futurePause", FuturePauseP);
    Attach("playbackBtn", PlaybackBtnP);
    Attach("playback", PlaybackP);

    LoadFromPanel(panel);

    VAR
      i           := 0;
      cnt         := Params.Count;
      param: TEXT;
    BEGIN
      WHILE i < cnt DO
        param := Params.Get(i);
        TRY
          IF Text.Equal(param, "-scale") THEN
            INC(i);
            IF i >= cnt THEN EXIT END;
            panel.scale := Lex.Real(TextRd.New (Params.Get(i)));
            ScaleFilter.Scale(
              FormsVBT.GetVBT(panel.fv, "scale"), panel.scale, panel.scale);
          ELSIF Text.Equal(param, "-xdrift") THEN
            INC(i);
            IF i >= cnt THEN EXIT END;
            XDRIFT := Lex.Int(TextRd.New (Params.Get(i)));
          ELSIF Text.Equal(param, "-ydrift") THEN
            INC(i);
            IF i >= cnt THEN EXIT END;
            YDRIFT := Lex.Int(TextRd.New (Params.Get(i)));
          ELSE
            INC(i);
          END;
        EXCEPT
          Lex.Error, FloatMode.Trap =>
        END;
      END;
    END;
    RETURN panel;
  END NewPanel;

PROCEDURE NewForm (name: TEXT; path: Rsrc.Path := NIL):
  FormsVBT.T =
  <* FATAL FormsVBT.Error, Rd.Failure, Rsrc.NotFound, Thread.Alerted *>
  BEGIN
    IF path = NIL THEN path := GetPath() END;
    RETURN NEW(FormsVBT.T).initFromRsrc(name, path)
  END NewForm;

PROCEDURE LoadFromPanel (panel: T) =
  <*LL = VBT.mu*>
  BEGIN
    FormsVBT.MakeEvent(panel.fv, "delay", 0);
    FormsVBT.MakeEvent(panel.fv, "minDelayFrac", 0);
    FormsVBT.MakeEvent(panel.fv, "codeDelayFrac", 0);
    FormsVBT.MakeEvent(panel.fv, "maxSpeedFactor", 0);
    FormsVBT.MakeEvent(panel.fv, "priority", 0);
  END LoadFromPanel;

<*UNUSED*> PROCEDURE NYI (msg: TEXT) =
  BEGIN                         (* LL = VBT.mu *)
    ReportError(msg & " not yet implemented.");
  END NYI;

PROCEDURE QuitP (<*UNUSED*>  fv : FormsVBT.T;
                 <*UNUSED*>  e  : TEXT;
                             arg: REFANY;
                 <*UNUSED *> t  : VBT.TimeStamp) =
  BEGIN (* LL = VBT.mu *)
    Trestle.Delete(NARROW(arg, T).fv);
  END QuitP;

PROCEDURE GoP (<*UNUSED*> fv : FormsVBT.T;
               <*UNUSED*> e  : TEXT;
                          arg: REFANY;
                          t  : VBT.TimeStamp) =
  BEGIN (* LL = VBT.mu *)
    (* ignored in playback, so don't generate it. *)
    (*    Script(ActionType.Go);*)
    ScriptMaybeStartFrame(arg);
    Go(NARROW(arg, T), t);
  END GoP;

PROCEDURE StepP (<*UNUSED*> fv : FormsVBT.T;
                 <*UNUSED*> e  : TEXT;
                            arg: REFANY;
                            t  : VBT.TimeStamp) =
  BEGIN (* LL = VBT.mu *)
    (* ignored in playback, so don't generate it. *)
    (*    Script(ActionType.Step);*)
    ScriptMaybeStartFrame(arg);
    Step(NARROW(arg, T), t);
  END StepP;

PROCEDURE AbortP (<*UNUSED*> fv : FormsVBT.T;
                  <*UNUSED*> e  : TEXT;
                             arg: REFANY;
                             t  : VBT.TimeStamp) =
  BEGIN (* LL = VBT.mu *)
    Script(ActionType.Abort);
    AbortInternal(NARROW(arg, T), t);
  END AbortP;

PROCEDURE SpeedP (<*UNUSED*> fv : FormsVBT.T;
                  <*UNUSED*> e  : TEXT;
                             arg: REFANY;
                  <*UNUSED*> t  : VBT.TimeStamp) =
  BEGIN (* LL = VBT.mu *)
    UpdateSpeed(NARROW(arg, T));
  END SpeedP;

PROCEDURE MinDelayP (<*UNUSED*> fv : FormsVBT.T;
                  <*UNUSED*> e  : TEXT;
                             arg: REFANY;
                  <*UNUSED*> t  : VBT.TimeStamp) =
  BEGIN (* LL = VBT.mu *)
    UpdateMinDelay(NARROW(arg, T));
  END MinDelayP;

PROCEDURE CodeDelayP (<*UNUSED*> fv : FormsVBT.T;
                  <*UNUSED*> e  : TEXT;
                             arg: REFANY;
                  <*UNUSED*> t  : VBT.TimeStamp) =
  BEGIN (* LL = VBT.mu *)
    UpdateCodeDelay(NARROW(arg, T));
  END CodeDelayP;

PROCEDURE SpeedFactorP (<*UNUSED*> fv : FormsVBT.T;
                  <*UNUSED*> e  : TEXT;
                             arg: REFANY;
                  <*UNUSED*> t  : VBT.TimeStamp) =
  BEGIN (* LL = VBT.mu *)
    UpdateSpeedFactor(NARROW(arg, T));
  END SpeedFactorP;

PROCEDURE PriorityP (           fv : FormsVBT.T;
                                e  : TEXT;
                                arg: REFANY;
                     <*UNUSED*> t  : VBT.TimeStamp) =
  BEGIN (* LL = VBT.mu *)
    Script(ActionType.Priority, Sx.FromInt(FormsVBT.GetInteger(fv, e)));
    SetPanelPriority(NARROW(arg, T), FormsVBT.GetInteger(fv, e));
  END PriorityP;

PROCEDURE ErrClearP (<*UNUSED*> fv : FormsVBT.T;
                     <*UNUSED*> e  : TEXT;
                                arg: REFANY;
                     <*UNUSED*> t  : VBT.TimeStamp) =
  BEGIN                          (* LL = VBT.mu *)
    (* Don't script.  Should we? *)
    ClearError(arg);
  END ErrClearP;

PROCEDURE SnapshotP (           fv : FormsVBT.T;
                     <*UNUSED*> e  : TEXT;
                                arg: REFANY;
                     <*UNUSED*> t  : VBT.TimeStamp) =
  BEGIN (* LL = VBT.mu *)
    Script(ActionType.Snapshot, FormsVBT.GetText(fv, "snapshot"));
    ZeusSnapshot.Snapshot(NARROW(arg, T), FormsVBT.GetText(fv, "snapshot"));
  END SnapshotP;

PROCEDURE RestoreP (           fv : FormsVBT.T;
                               e  : TEXT;
                               arg: REFANY;
                    <*UNUSED*> t  : VBT.TimeStamp) =
  BEGIN (* LL = VBT.mu *)
    ZeusSnapshot.Restore(NARROW(arg, T), FormsVBT.GetText(fv, e));
(* DON'T PUT Restore IN SCRIPT.  Leave it to the frame restore.
    (* put Script call afterward, so session deletions (part of Restore's
       operation) happen before the Restore in scriptOut. *)
    (* put snapshots in-line in scripts, rather than using filenames *)
    TRY
      WITH list = Sx.Read(FileRd.Read(FormsVBT.GetText(fv, e))) DO
        Script(ActionType.Restore, list);
        (* The following would hide information better: *)
        (* Script(ActionType.Restore, SnapshotToList()); *)
      END;
    EXCEPT
    ELSE
    END;
*)
(*    Script(ActionType.Restore, FormsVBT.GetText(fv, e)); *)
  END RestoreP;

PROCEDURE RecordBtnP (           fv : FormsVBT.T;
                      <*UNUSED*> e  : TEXT;
                      <*UNUSED*> arg: REFANY;
                      <*UNUSED*> t  : VBT.TimeStamp) =
  BEGIN                          (* LL = VBT.mu *)
    IF scripting = ScriptingState.Off THEN
      FormsVBT.PopUp(fv, "RecordDialog");
    ELSIF scripting = ScriptingState.Recording THEN
      StopScript();
    END (* IF *);
  END RecordBtnP;

PROCEDURE RecordP (           fv : FormsVBT.T;
                   <*UNUSED*> e  : TEXT;
                   <*UNUSED*> arg: REFANY;
                   <*UNUSED*> t  : VBT.TimeStamp) =
  BEGIN                          (* LL = VBT.mu *)
    IF scripting = ScriptingState.Off THEN
      StartScript(FormsVBT.GetText(fv, "record"));
    END (* IF *);
  END RecordP;

PROCEDURE PlaybackBtnP (           fv : FormsVBT.T;
                        <*UNUSED*> e  : TEXT;
                        <*UNUSED*> arg: REFANY;
                        <*UNUSED*> t  : VBT.TimeStamp) =
  BEGIN                          (* LL = VBT.mu *)
    IF scripting = ScriptingState.Off THEN
      FormsVBT.PopUp(fv, "PlaybackDialog");
    ELSIF scripting = ScriptingState.Playback THEN
      StopPlayback();
    END (* IF *);
  END PlaybackBtnP;

PROCEDURE PlaybackP (           fv : FormsVBT.T;
                     <*UNUSED*> e  : TEXT;
                     <*UNUSED*> arg: REFANY;
                     <*UNUSED*> t  : VBT.TimeStamp) =
  BEGIN                          (* LL = VBT.mu *)
    IF scripting = ScriptingState.Off THEN
      StartPlayback(FormsVBT.GetText(fv, "playback"));
    END (* IF *);
  END PlaybackP;

PROCEDURE FuturePauseP (<*UNUSED*> fv : FormsVBT.T;
                        <*UNUSED*> e  : TEXT;
                                   arg: REFANY;
                        <*UNUSED*> t  : VBT.TimeStamp) =
  BEGIN                          (* LL = VBT.mu *)
    IF NOT stateIdle[NARROW(arg, T).runState] THEN
      Script(ActionType.FuturePause);
    END;
  END FuturePauseP;

PROCEDURE GrabDataP (<*UNUSED*> fv : FormsVBT.T;
                     <*UNUSED*> e  : TEXT;
                                arg: REFANY;
                     <*UNUSED*> t  : VBT.TimeStamp) =
  BEGIN                          (* LL = VBT.mu *)
    IF NOT stateIdle[NARROW(arg, T).runState] THEN
      Script(ActionType.GrabData, ZeusSnapshot.GrabDataList(arg));
    END;
  END GrabDataP;

PROCEDURE SessionsP (           fv : FormsVBT.T;
                                e  : TEXT;
                                arg: REFANY;
                     <*UNUSED*> t  : VBT.TimeStamp) =
  (* This is called only when stateIdle[panel.runState], thanks
     to the dormancy of the session menu at other times.  See
     SetRunState. *)
  BEGIN                          (* LL = VBT.mu *)
    <*ASSERT Text.Equal("SESS", Text.Sub(e, 0, 4)) *>
    Script(
      ActionType.Sessions,
      RefList.List2(
        Text.Sub(e, 4, LAST(INTEGER)),
        Sx.FromBool(FormsVBT.GetBoolean(fv, "inTrestle"))));
    NewSessionDefault(
      Text.Sub(e, 4, LAST(INTEGER)), NARROW(arg, T));
  END SessionsP;

PROCEDURE PhotoP (<*UNUSED*> fv : FormsVBT.T;
                  <*UNUSED*> e  : TEXT;
                             arg: REFANY;
                  <*UNUSED*> t  : VBT.TimeStamp) =
  BEGIN                         (* LL = VBT.mu *)
    Script(ActionType.Photo);
    Photo(NARROW(arg, T));
  END PhotoP;

PROCEDURE ClearAlbumP (<*UNUSED*> fv : FormsVBT.T;
                       <*UNUSED*> e  : TEXT;
                                  arg: REFANY;
                       <*UNUSED*> t  : VBT.TimeStamp) =
  BEGIN                         (* LL = VBT.mu *)
    Script(ActionType.ClearAlbum);
    ClearAlbum(NARROW(arg, T));
  END ClearAlbumP;

PROCEDURE DelAllViewsP (<*UNUSED*> fv : FormsVBT.T;
                       <*UNUSED*> e  : TEXT;
                                  arg: REFANY;
                       <*UNUSED*> t  : VBT.TimeStamp) =
  BEGIN                         (* LL = VBT.mu *)
    (* Don't script; will be caught by frame restore. *)
    DeleteAllViews(arg);
  END DelAllViewsP;

(* **************** Session Form **************** *)

PROCEDURE AlgsP (           fv : FormsVBT.T;
                            e  : TEXT;
                            arg: REFANY;
                 <*UNUSED*> t  : VBT.TimeStamp) =
  (* This is called only when stateIdle[panel.runState], thanks to
     the dormancy of the algs menu at other times.  See SetRunState. *)
  VAR
    sess               := NARROW(arg, Session);
    tb  : ListVBT.T    := FormsVBT.GetVBT(fv, e);
    sel : ListVBT.Cell;
    st  : TEXT;
  BEGIN (* LL = VBT.mu *)
    IF tb.getFirstSelected(sel) THEN
      st := tb.getValue(sel);
      WITH name = sess.name & "." & st DO
        Script(ActionType.Algs, RefList.List2(SessListPos(sess), name));
        PickedAlg(sess, name);
        TRY
          IF sess.alg # NIL THEN sess.alg.restore(NIL); END;
        EXCEPT
          ZeusClass.Error =>
        END;
      END;
    END;
  END AlgsP;
  
PROCEDURE ViewsP (           fv : FormsVBT.T;
                             e  : TEXT;
                             arg: REFANY;
                  <*UNUSED*> t  : VBT.TimeStamp) =
  VAR
    sess               := NARROW(arg, Session);
    tb  : ListVBT.T    := FormsVBT.GetVBT(fv, e);
    sel : ListVBT.Cell;
  BEGIN                          (* LL = VBT.mu *)
    IF tb.getFirstSelected(sel) THEN
      WITH name = sess.name & "."
                    & NARROW(tb.getValue(sel), TEXT) DO
        Script(ActionType.Views,
               RefList.List2(SessListPos(sess), name));
        WITH view = PickedView(sess, name) DO
          TRY
            IF view # NIL THEN view.restore(NIL); END;
          EXCEPT
            ZeusClass.Error =>
          END;
        END;
      END;
      tb.selectNone();
    END;
  END ViewsP;

PROCEDURE AbortAlgP (<*UNUSED*> fv : FormsVBT.T;
                     <*UNUSED*> e  : TEXT;
                                arg: REFANY;
                     <*UNUSED*> t  : VBT.TimeStamp) =
  (* This should abort just the algorithm for this session *)
  BEGIN (* LL = VBT.mu *)
    Script(ActionType.AbortAlg, SessListPos(arg));
    AbortAlg(NARROW(arg, Session));
  END AbortAlgP;

PROCEDURE DestroyP (<*UNUSED*> fv : FormsVBT.T;
                    <*UNUSED*> e  : TEXT;
                               arg: REFANY;
                    <*UNUSED*> t  : VBT.TimeStamp) =
  VAR sess := NARROW(arg, Session);
  BEGIN (* LL = VBT.mu *)
    (* put Script() call in DestroySession() to catch WM deletes, too. *)
    IF sess.inTrestle THEN
      Trestle.Delete(sess.fv);
    ELSE
      DestroySession(sess);
    END;
  END DestroyP;


(* **************** Main Interaction **************** *)

PROCEDURE Interact (title: TEXT      := "ZEUS Control Panel";
                    path : Rsrc.Path := NIL                   ) =
  VAR panel := Resolve(NIL);
  BEGIN
    panel.title := title;
    panel.path := path;
    Start(panel);
    Trestle.Install(panel.fv, "Zeus", NIL, panel.title);
    (* LOCK VBT.mu DO Trestle.MoveNear(panel.fv, NIL); END;*)
    Trestle.AwaitDelete(panel.fv);
    Finish(panel);
  END Interact;

TYPE
  PanelClosure = Thread.SizedClosure OBJECT
                   panel: T;
                 OVERRIDES
                   apply := PanelThread
                 END;

  AlgClosure = Thread.SizedClosure OBJECT
                 panel: T;
                 sess : Session;
               OVERRIDES
                 apply := AlgThread
               END;

PROCEDURE Start (panel: T) =
  VAR pclosure: PanelClosure;
  BEGIN                         (* LL = {} *)
    LOCK VBT.mu DO
      ZeusSnapshot.InitialRestore(panel);
      IF (panel.sessions = NIL) AND (groupInfo # NIL) THEN
        NewSessionDefault(
          NARROW(groupInfo.head, AlgGroupInfo).groupName, panel);
      END;
    END;
    pclosure := NEW(PanelClosure, panel := panel, stackSize := 10000);
    panel.panelThread := Thread.Fork(pclosure);
  END Start;


PROCEDURE Finish (panel: T) =
  BEGIN                          (* LL = {} *)
    (* DebugFinish();*)
    StopScript();
    LOCK panel.mu DO
      panel.quit := TRUE;
      Thread.Broadcast(panel.runCond);
      AbortWithLock(panel, 0);
    END;
    EVAL Thread.Join(panel.panelThread);
    LOCK VBT.mu DO
      ZeusSnapshot.FinalSnapshot(panel);
      DestroyAllSessions(panel);
    END;
    LOCK VBT.mu DO VBT.Discard(panel.fv); END;
  END Finish;


(* **************** Miscellaneous Entries **************** *)

PROCEDURE GetAnimationTime (): REAL =
<* LL = VBT.mu *>
  VAR panel := Resolve(NIL);
  BEGIN
    RETURN panel.delayTime
  END GetAnimationTime;

PROCEDURE SetTitle (title: TEXT) =
  VAR panel := Resolve(NIL);
  BEGIN
    panel.title := title;
    LOCK VBT.mu DO RenameTrestleChassis(panel.fv, title); END;
  END SetTitle;

PROCEDURE GetPath (): Rsrc.Path =
  VAR panel := Resolve(NIL);
  BEGIN
    RETURN panel.path
  END GetPath;

PROCEDURE ReportErrorC (report: BOOLEAN; t: TEXT) =
  BEGIN (* LL = VBT.mu *)
    IF report THEN ReportError(t); END;
  END ReportErrorC;

PROCEDURE ReportError (text: TEXT) =
  VAR
    panel  : T;
    tlength: INTEGER;
  BEGIN                         (* LL = VBT.mu *)
    panel := Resolve(NIL);
    IF text = NIL THEN RETURN END;
    tlength := Text.Length(text);
    IF tlength = 0 THEN RETURN END;
    IF (Text.GetChar(text, tlength - 1) # '\n') THEN
      text := text & "\n";
    END;
    TextEditVBTAppend(FormsVBT.GetVBT(panel.fv, "error"), text);
    FormsVBT.PopUp(panel.fv, "ErrorDialog");
  END ReportError;

PROCEDURE Abort () =
  VAR panel := Resolve(NIL);
  BEGIN (* LL = VBT.mu *)
    Script(ActionType.Abort);
    AbortInternal(panel, 0);
  END Abort;


PROCEDURE ClearError(panel: T) =
  BEGIN
    TextEditVBTClear(FormsVBT.GetVBT(panel.fv, "error"))
  END ClearError;


PROCEDURE PrepForSnapshot (panel: T) =
  <* LL = VBT.mu *>
  BEGIN
    ClearError(panel);
  END PrepForSnapshot;

PROCEDURE OverrideRestore(panel: T) =
  <* LL = VBT.mu *>
  (* Call this from ZeusSnapshot.m3 after a restore to reset things
     that the restore operation shouldn't have changed, but may have. *)
  BEGIN
    ClearError(panel);
    SetRunState(panel, RunState.Virgin);
    ChangeScriptingState(scripting);
    ResetSessionMenu(panel);
  END OverrideRestore;


<*UNUSED*> 
PROCEDURE AlgReady (alg: Algorithm.T; ready: BOOLEAN) =
  (* Enable or disable the GO and STEP buttons.  The buttons are enabled
     whenever the user changes the algorithm.  This procedure is useful
     when it is known that the user has specified invalid data such that it
     is meaningless to run the algorithm with such data. *)
  (* This doesn't work. *)
  VAR fv: FormsVBT.T;
  BEGIN
    fv := Resolve(alg).fv;
    IF ready THEN
      FormsVBT.MakeActive(fv, "goBtn");
      FormsVBT.MakeActive(fv, "stepBtn");
    ELSE
      FormsVBT.MakeDormant(fv, "goBtn");
      FormsVBT.MakeDormant(fv, "stepBtn");
    END;
  END AlgReady;



(* **************** Registration **************** *)

TYPE
  AlgGroupInfo = REF RECORD
                       groupName: TEXT;
                       title    : TEXT;
                       vbt      : VBT.T;    (* menu entry *)
                       algs     : TextList.T := NIL;
                       views    : TextList.T := NIL;
                     END;

VAR
  groupInfo: RefList.T := NIL; (* of AlgGroupInfo *)

PROCEDURE GICompare (a1, a2: REFANY): [-1 .. 1] =
  VAR
    i1 := NARROW(a1, AlgGroupInfo);
    i2 := NARROW(a2, AlgGroupInfo);
  BEGIN
    IF i1 = NIL THEN
      RETURN -1
    ELSIF i2 = NIL THEN
      RETURN 1
    ELSE
      RETURN Text.Compare(i1.title, i2.title);
    END;
  END GICompare;

PROCEDURE GetGroupInfo (sessName: TEXT; inMenu: BOOLEAN := TRUE):
  AlgGroupInfo =
  <* LL = VBT.mu *>
  (* Look up the named algorithm group and return its AlgGroupInfo record.
     Create an AlgGroupInfo record if none exists.  In this case, and if
     inMenu is TRUE, then insert an entry into the menu in the Sessions
     menu in the control panel. *)
  VAR
    panel := Resolve(NIL);
    info  := GetExistingGI(sessName);
  BEGIN
    IF info # NIL THEN RETURN info END;
    info := NEW(AlgGroupInfo, groupName := sessName, title := sessName);
    IF inMenu THEN
      RefListUtils.Push(groupInfo, info);
      UpdateSessionMenu(panel);
    END;
    RETURN info;
  END GetGroupInfo;

PROCEDURE UpdateSessionMenu (panel: T) =
  <* LL = VBT.mu *>
  VAR
    l   : RefList.T;
    info: AlgGroupInfo;
  BEGIN
    groupInfo := RefListSort.SortD(groupInfo, GICompare);
    l := groupInfo;
    FormsVBT.Delete(panel.fv, "sessionMenu", 0, LAST(CARDINAL));
    WHILE l # NIL DO
      info := RefListUtils.Pop(l);
      (*
      IF info.vbt # NIL THEN
        FormsVBT.InsertVBT(panel.fv, "sessionMenu", info.vbt);
      ELSE
      *)
        info.vbt := FormsVBT.Insert(
                      panel.fv, "sessionMenu",
                      "(Shape (Width 100) (MButton %SESS" & info.groupName
                        & " (Text %TITLE" & info.groupName & " \""
                        & info.title & "\")))");
        FormsVBT.AttachProc(
          panel.fv, "SESS" & info.groupName, SessionsP, panel);
      (*
      END;
      *)
    END;
  END UpdateSessionMenu;

PROCEDURE GetExistingGI (sessName: TEXT): AlgGroupInfo =
  (* Look up the named algorithm group and return its AlgGroupInfo record.
     RETURN NIL if none exists. *)
  VAR l := groupInfo;
  BEGIN (* LL = VBT.mu *)
    WHILE l # NIL DO
      IF Text.Equal(sessName, NARROW(l.head, AlgGroupInfo).groupName) THEN
        RETURN l.head
      END;
      l := l.tail;
    END;
    RETURN NIL;
  END GetExistingGI;

PROCEDURE GroupInfoExists (sessName: TEXT): BOOLEAN =
  BEGIN (* LL = VBT.mu *)
    RETURN GetExistingGI(sessName) # NIL
  END GroupInfoExists;


PROCEDURE SetSessTitle (sessName, sessTitle: TEXT) =
  (* Change the title of session "sessName" to "sessTitle." Create a
     session named "sessName," if none existed previously. *)
  VAR
    info : AlgGroupInfo;
    panel               := Resolve(NIL);
  BEGIN (* LL = {} *)
    LOCK VBT.mu DO
      info := GetGroupInfo(sessName);
      info.title := sessTitle;
      FormsVBT.PutText(panel.fv, "TITLE" & sessName, sessTitle);
      UpdateSessionMenu(panel);
    END;
  END SetSessTitle;

PROCEDURE ResetSessionMenu (panel: T) =
  <* LL = VBT.mu *>
  (* Reset the titles of the sessions in the session menu to be equal to
     their real titles. *)
  VAR l := groupInfo;
  BEGIN
    WHILE l # NIL DO
      WITH info = NARROW(l.head, AlgGroupInfo) DO
        FormsVBT.PutText(panel.fv, "TITLE" & info.groupName, info.title);
      END;
      l := l.tail;
    END;
    UpdateSessionMenu(panel);
  END ResetSessionMenu;

EXCEPTION DuplicateName;
<* FATAL DuplicateName *>

PROCEDURE RegisterAlg (proc: NewAlgProc; name, sessName: TEXT) =
  (* LL = {} *)
  VAR info: AlgGroupInfo;
  BEGIN
    LOCK VBT.mu DO
      info := GetGroupInfo(sessName);
      IF NOT TextList.Member(info.algs, name) THEN
        Classes.RegisterAlg(proc, sessName & "." & name);
        info.algs := TextList.Cons(name, info.algs);
      ELSE
        RAISE DuplicateName;
      END;
    END;
  END RegisterAlg;


PROCEDURE RegisterView (proc          : NewViewProc;
                        name, sessName: TEXT;
                        alertable     : BOOLEAN       := FALSE;
                        sample        : View.T        := NIL    ) =
  (* LL = {} *)
  VAR info: AlgGroupInfo;
  BEGIN
    LOCK VBT.mu DO
      info := GetGroupInfo(sessName);
      IF NOT TextList.Member(info.views, name) THEN
        Classes.RegisterView(proc, sessName & "." & name, alertable, sample);
        info.views := TextList.Cons(name, info.views);
      ELSE
        RAISE DuplicateName;
      END;
    END;
  END RegisterView;


(* **************** Creating and Destroying Sessions **************** *)

TYPE
  SessionWatcherClosure = Thread.Closure OBJECT
                            sess: Session;
                          OVERRIDES
                            apply := SessionWatcher
                          END;


PROCEDURE NewSessionDefault (name: TEXT; panel: T) =
  (* Get the inTrestle parm from the FV before calling NewSession. *)
  BEGIN                         (* LL = VBT.mu *)
    IF NOT ZeusSnapshot.SessionFromStateDir(panel, name, FALSE) THEN
      NewSession(name, panel, FormsVBT.GetBoolean(panel.fv, "inTrestle"))
    END;
    LOCK panel.mu DO UpdateSessionButtons(panel); END;
  END NewSessionDefault;

PROCEDURE NewSession (name     : TEXT;
                      panel    : T;
                      inTrestle: BOOLEAN;
                      pickAlg  : BOOLEAN   := TRUE) =
  <* LL = VBT.mu *>
  (* if pickAlg, call PickedAlg on the first alg assoc with the new
     session. *)
  VAR
    sess := NEW(Session, name := name,
                fv := NewForm("zeusSession.fv", panel.fvpath),
                inTrestle := inTrestle,
                (*mu := NEW(MUTEX), *)
                runCond := NEW(Thread.Condition),
                feedCond := NEW(Thread.Condition), alg := NEW(Algorithm.T));
    info                 := GetGroupInfo(name, FALSE);
    l       : TextList.T;
    browser : ListVBT.T;
    aclosure: AlgClosure;

  PROCEDURE Attach (id: TEXT; proc: FormsVBT.Proc) =
    BEGIN
      FormsVBT.AttachProc(sess.fv, id, proc, sess);
    END Attach;

  BEGIN
    EVAL sess.init();
    Zeus.AttachAlg(sess, sess.alg);
    sess.alg.install();
    Attach("algs", AlgsP);
    Attach("views", ViewsP);
    Attach("abort", AbortAlgP);
    FormsVBT.MakeDormant(sess.fv, "abort");
    Attach("destroy", DestroyP);
    Attach("eventDataBool", ToggleTSplitP);
    Attach("algBool", ToggleTSplitP);
    Attach("dataFormBool", ToggleTSplitP);

    browser := FormsVBT.GetVBT(sess.fv, "algs");
    l := info.algs;
    WHILE l # NIL DO InsertToBrowser(browser, l.head);  l := l.tail END;
    browser := FormsVBT.GetVBT(sess.fv, "views");
    l := info.views;
    WHILE l # NIL DO InsertToBrowser(browser, l.head); l := l.tail END;

    aclosure :=
      NEW(AlgClosure, panel := panel, sess := sess, stackSize := 10000);
    sess.algThread := Thread.Fork(aclosure);
    LOCK panel.mu DO
      IF panel.sessions = NIL THEN
        FormsVBT.MakeActive(panel.fv, "goBtn");
        FormsVBT.MakeActive(panel.fv, "stepBtn");
      END;
      RefListUtils.Push(panel.sessions, sess);
      Animate.SetDuration(panel.delayTime);
    END;

    IF sess.inTrestle THEN
      ScaleFilter.Scale(
        FormsVBT.GetVBT(sess.fv, "scale"), panel.scale, panel.scale);
      Trestle.Attach(sess.fv);
      Trestle.Decorate(sess.fv, applName := "Zeus",
                       windowTitle := "Zeus " & info.title & " Session");
      MoveNear(sess.fv, panel.fv);
      (* Trestle.Install(sess.fv, "Zeus", NIL, "Zeus " & name & "
         Session");*)
      EVAL Thread.Fork(NEW(SessionWatcherClosure, sess := sess));
    ELSE
      DestroyFVOwner(panel, FormsVBT.GetGeneric(panel.fv, "sessionFV"));
      FormsVBT.PutText(panel.fv, "sessName", info.title);
      FormsVBT.PutGeneric(panel.fv, "sessionFV", sess.fv);
    END;
    IF pickAlg AND (info.algs # NIL) THEN
      PickedAlg(sess, sess.name & "." & NARROW(info.algs.head, TEXT));
    END;
    TRY
      IF sess.alg # NIL THEN sess.alg.restore(NIL); END;
    EXCEPT
      ZeusClass.Error =>
    END;
  END NewSession;

PROCEDURE SessionWatcher (cl: SessionWatcherClosure): REFANY =
  BEGIN                         (* LL = {} *)
    WITH sess = cl.sess DO
      Trestle.AwaitDelete(sess.fv);
      LOCK VBT.mu DO DestroySession(sess); END;
    END;
    RETURN NIL;
  END SessionWatcher;

PROCEDURE DestroyFVOwner (panel: T; fv: VBT.T) =
  VAR
    l     : RefList.T;
    tokill: Session := NIL;
  BEGIN                         (* LL = VBT.mu *)
    LOCK panel.mu DO
      l := panel.sessions;
      WHILE l # NIL DO
        WITH sess = NARROW(RefListUtils.Pop(l), Session) DO
          IF sess.fv = fv THEN tokill := sess END;
        END;
      END;
    END;
    IF tokill # NIL THEN DestroySession(tokill); END;
  END DestroyFVOwner;


PROCEDURE DestroySession (sess: Session) =
  VAR panel := Resolve(NIL);
      wasActive: BOOLEAN;
  BEGIN                         (* LL = VBT.mu *)
    IF NOT stateIdle[panel.runState] THEN
      (* frame restores will catch other destroys *)
      Script(ActionType.Destroy, SessListPos(sess));
    END;
    IF scripting # ScriptingState.Playback THEN (* no need o/w *)
      ZeusSnapshot.SessionToStateDir(sess);
    END;
    LOCK panel.mu DO
      sess.quit := TRUE;
      wasActive := sess.active;
      ChangeSessActive(sess, panel, FALSE);
      RefListUtils.Delete(panel.sessions, sess);
      UpdateSessionButtons(panel);
      IF (panel.sessions = NIL) AND (NOT panel.quit) THEN
        FormsVBT.MakeDormant(panel.fv, "goBtn");
        FormsVBT.MakeDormant(panel.fv, "stepBtn");
        FormsVBT.MakeDormant(panel.fv, "abortBtn");
      END
    END;
    IF wasActive THEN SetRunState(panel, RunState.Aborted) END;
    DeleteViews(sess);
    IF sess.alg # NIL THEN DeleteAlg(sess) END;
    Thread.Alert(sess.algThread);
    Thread.Broadcast(sess.runCond);
    (* I think this caused a deadlock, and it doesn't seem necessary: *)
    (*    EVAL Thread.Join(sess.algThread);*)
    IF (NOT sess.inTrestle)
         AND (sess.fv = FormsVBT.GetGeneric(panel.fv, "sessionFV")) THEN
      FormsVBT.PutGeneric(panel.fv, "sessionFV", NIL);
      FormsVBT.PutText(panel.fv, "sessName", "Null");
    END;
  END DestroySession;

PROCEDURE DestroyAllSessions (panel: T) =
  VAR
    l, rest: RefList.T;            (* of Session *)
    sess   : Session;
  BEGIN                         (* LL = VBT.mu *)
    LOCK panel.mu DO
      l := panel.sessions;
(*      panel.sessions := NIL;    (* is this a good idea? *)
                                  NO! Destroys the numActive invariant! *)
      WHILE l # NIL DO
        sess := RefListUtils.Pop(l);
        IF sess.inTrestle THEN
          sess.quit := TRUE;    (* so sess won't be made active *)
          Trestle.Delete(sess.fv);
        ELSE
          RefListUtils.Push(rest, sess); (* probably happens <= once *)
        END;
      END;
    END;
    WHILE rest # NIL DO DestroySession(RefListUtils.Pop(rest)) END;
  END DestroyAllSessions;

PROCEDURE UpdateSessionButtons (panel: T) =
  <* LL = {VBT.mu, panel.mu} *>
  (* Selectively show the "Abort Alg" and "Destroy Session" buttons. *)
  VAR
    l   : RefList.T;
    sel : CARDINAL;
    sess: Session;
  BEGIN
    l := panel.sessions;
    IF RefList.Length(l) > 1 THEN sel := 1 ELSE sel := 0 END;
    WHILE l # NIL DO
      sess := RefListUtils.Pop(l);
      FormsVBT.PutInteger(sess.fv, "showButtons", sel);
    END;
  END UpdateSessionButtons;


PROCEDURE ToggleTSplitP (             fv : FormsVBT.T;
                                      e  : TEXT;
                                      arg: REFANY;
                         <* UNUSED *> t  : VBT.TimeStamp) =
  <* LL = VBT.mu *>
  BEGIN
    Script(ActionType.ToggleTSplit, RefList.List2(SessListPos(arg), e));
    WITH tsplitName = Text.Sub(e, 0, Text.Length(e)
                                       - Text.Length("Bool"))
                        & "T" DO
      FormsVBT.PutInteger(
        fv, tsplitName, 1 - FormsVBT.GetInteger(fv, tsplitName))
    END
  END ToggleTSplitP;

PROCEDURE SessListPos(sess: Session): REF INTEGER =
  (* Return position of sess in panel.sessions as a REF INTEGER.
     If sess NOTIN panel.sessions,
     then return RefList.Length(panel.sessions), which is arguably wrong. *)
  VAR panel := Resolve(NIL);
      l: RefList.T;
      pos:= 0;
  BEGIN
    LOCK panel.mu DO
      l := panel.sessions;
      WHILE (l # NIL) AND (RefListUtils.Pop(l) # sess) DO INC(pos) END;
    END (* LOCK *);
    RETURN Sx.FromInt(pos);
  END SessListPos;


(* **************** Selecting Algorithms and Views **************** *)

PROCEDURE PickedAlg (sess: Session; which: TEXT) =
  (* LL = VBT.mu *)
  VAR
    alg   : Algorithm.T;
    suffix: TEXT;
  BEGIN
    TRY
      ZeusPanelFriends.whichAlg := which;
      alg := Classes.NewAlg(Classes.FindAlg(which));
    EXCEPT
      Classes.NotFound => RETURN
    END;
    Zeus.Acquire(sess);
    sess.viewsToAdd := RefList.Append(sess.viewsToAdd, sess.views);
    Zeus.Release(sess);
    IF sess.alg # NIL THEN DeleteAlg(sess) END;
    Zeus.AttachAlg(sess, alg);
    alg.install();
    sess.algIsSet := TRUE;
    IF CheckPrefix(which, sess.name & ".", suffix) THEN
      FormsVBT.PutText(sess.fv, "algName", suffix);
      SelectInBrowser(FormsVBT.GetVBT(sess.fv, "algs"), suffix);
    END;
    FormsVBT.PutGeneric(sess.fv, "dataForm", alg.data);
    FormsVBT.PutGeneric(sess.fv, "eventDataForm", alg.eventData);
    InitViewBrowser(sess, alg);
    InitCodeViewBrowser(sess, alg);
    SetAllViewTitles(sess);    (* also makes incompat views dormant *)
  END PickedAlg;


PROCEDURE PickedView (sess: Session; which: TEXT): View.T =
  (* LL = VBT.mu *)
  VAR view: View.T;
  BEGIN
    TRY
      ZeusPanelFriends.whichView := which;
      view := Classes.NewView(Classes.FindView(which));
    EXCEPT
      Classes.NotFound =>
        view := NewCodeView(sess, which);
    END;
    IF view = NIL THEN RETURN NIL END;
    view.install();
    SetViewTitle(sess, view);
    (*
        IF sess.inTrestle THEN
          MoveNear(view, sess.fv);
        ELSE
          MoveNear(view, Resolve(NIL).fv);
        END;
    *)
    RefListUtils.Push(sess.viewsToAdd, view);
    ZeusPrivate.Mark(sess, view);
    RETURN view
  END PickedView;


PROCEDURE DeleteAlg (sess: Session) =
  (* LL = VBT.mu *)
  BEGIN
    (*    DeleteCodeViews(sess);
          EmptyCodeViewBrowser(sess, sess.alg); *)
    sess.alg.delete();
  END DeleteAlg;

PROCEDURE AttachViews (sess: Session) =
  (* LL = VBT.mu *)
  VAR
    rest: RefList.T;
    view: View.T;
  BEGIN
    rest := sess.viewsToAdd;
    WHILE rest # NIL DO
      view := NARROW(rest.head, View.T);
      Zeus.AttachView(sess, view);
      rest := rest.tail;
    END;
    sess.viewsToAdd := NIL;
  END AttachViews;


PROCEDURE DetachView (view: View.T) =
  (* LL = VBT.mu *)
  VAR sess := NARROW(Zeus.Resolve(view), Session);
  BEGIN
    RefListUtils.Delete(sess.viewsToAdd, view);
    Zeus.DetachView(view);
  END DetachView;


PROCEDURE DeleteViews (sess: Session) =
  VAR
    rest: RefList.T;
    view: View.T;
  BEGIN                         (* LL = VBT.mu *)
    Zeus.Acquire(sess);
    rest := RefList.Append(sess.viewsToAdd, sess.views);
    Zeus.Release(sess);
    WHILE rest # NIL DO
      view := NARROW(rest.head, View.T);
      view.delete();
      rest := rest.tail;
    END;
    sess.viewsToAdd := NIL;
  END DeleteViews;

PROCEDURE DeleteAllViews (panel: T) =
  <* LL = VBT.mu *>
  VAR rest: RefList.T;
  BEGIN
    LOCK panel.mu DO
      rest := panel.sessions;
      WHILE rest # NIL DO DeleteViews(RefListUtils.Pop(rest)); END;
    END;
  END DeleteAllViews;

PROCEDURE SetAllViewTitles (sess: Session) =
  (* LL = VBT.mu *)
  (* This sets view titles, and also makes views that are incompatible with
     the current algorithm be Dormant. *)
  VAR rest: RefList.T;
  BEGIN
    rest := sess.viewsToAdd;
    WHILE rest # NIL DO
      WITH v = NARROW(RefListUtils.Pop(rest), View.T) DO
        IF v.isCompat(sess.alg) THEN
          SetViewTitle(sess, v);
          ViewClass.Activate(v, TRUE);
        ELSE
          ViewClass.Activate(v, FALSE);
        END;
      END;
    END;
    Zeus.Acquire(sess);
    rest := sess.views;
    Zeus.Release(sess);
    WHILE rest # NIL DO
      WITH v = NARROW(RefListUtils.Pop(rest), View.T) DO
        IF v.isCompat(sess.alg) THEN
          SetViewTitle(sess, v);
          ViewClass.Activate(v, TRUE);
        ELSE
          ViewClass.Activate(v, FALSE);
        END;
      END;
    END;
  END SetAllViewTitles;

PROCEDURE SetViewTitle (sess: Session; view: View.T) =
  (* LL = VBT.mu *)
  VAR asuffix, vsuffix: TEXT;
  BEGIN
    IF CheckPrefix(view.name, sess.name & ".", vsuffix)
         AND CheckPrefix(sess.alg.name, sess.name & ".", asuffix) THEN
      RenameTrestleChassis(view, asuffix & ": " & vsuffix);
    END;
  END SetViewTitle;

PROCEDURE InitViewBrowser (sess: Session; alg: Algorithm.T) =
  VAR
    tp  : ListVBT.T  := FormsVBT.GetVBT(sess.fv, "views");
    info             := GetGroupInfo(sess.name, FALSE);
    l   : TextList.T;
    view: View.T;
  BEGIN                          (* LL = VBT.mu *)
    tp.removeCells(0, LAST(INTEGER));
    l := info.views;
    WHILE l # NIL DO
      WITH t    = l.head,
           name = sess.name & "." & t DO
        TRY
          l := l.tail;
          view := Classes.SampleView(Classes.FindView(name));
          IF view.isCompat(alg) THEN InsertToBrowser(tp, t); END;
        EXCEPT
          Classes.NotFound =>
        END;
      END;
    END;
  END InitViewBrowser;


(* **************** Code Views **************** *)

<*UNUSED*>
PROCEDURE DeleteCodeViews (sess: Session) =
  VAR l: RefList.T;
  BEGIN                         (* LL = VBT.mu *)
    l := sess.viewsToAdd;
    WHILE l # NIL DO
      TYPECASE RefListUtils.Pop(l) OF
      | ZeusCodeView.T (v) =>
          v.delete();
          RefListUtils.Delete(sess.viewsToAdd, v);
      ELSE
      END;
    END;
    Zeus.Acquire(sess);
    l := sess.views;
    Zeus.Release(sess);
    WHILE l # NIL DO
      TYPECASE RefListUtils.Pop(l) OF
      | ZeusCodeView.T (v) =>
          v.delete();           (* Zeus.DetachView does the rest *)
      ELSE
      END;
    END;
  END DeleteCodeViews;

PROCEDURE IsCodeView (which: TEXT; sess: Session; VAR file: TEXT):
  BOOLEAN =
  (* LL = arbitrary *)
  VAR
    t   : TEXT;
    list: RefList.T;
  BEGIN
    IF NOT CheckPrefix(which, sess.name & ".", t) THEN RETURN FALSE END;
    list := RefListUtils.Assoc(sess.alg.codeViews, t);
    IF RefList.Length(list) # 2 THEN
      RETURN FALSE;
    ELSE
      TYPECASE list.tail.head OF
      | TEXT (txt) => file := txt; RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    END;
  END IsCodeView;

PROCEDURE NewCodeView (sess: Session; which: TEXT): ZeusCodeView.T =
  (* LL = VBT.mu *)
  VAR
    twr                   := TextWr.New();
    view : ZeusCodeView.T;
    t, fn: TEXT;
    path: Rsrc.Path;
  BEGIN
    IF NOT IsCodeView(which, sess, fn) THEN
      ReportError(which & " is not a code view");
      RETURN NIL
    END;
    path := sess.alg.codePath;
    IF path = NIL THEN path := GetPath() END;
    TRY
      view := ZeusCodeView.New(which, Rsrc.Open(fn, path), sess.alg, twr);
    EXCEPT
    Rsrc.NotFound => 
        ReportError("Cannot find file " & fn);
        RETURN NIL;
    END;
    t := TextWr.ToText(twr);
    IF NOT Text.Equal(t, "") THEN
      ReportError(t);
      RETURN NIL
    ELSE
      RETURN view
    END;
  END NewCodeView;

<*UNUSED*>
PROCEDURE EmptyCodeViewBrowser (sess: Session; alg: Algorithm.T) =
  VAR
    l       := alg.codeViews;
    browser := FormsVBT.GetVBT(sess.fv, "views");
  BEGIN                         (* LL = VBT.mu *)
    WHILE l # NIL DO
      DeleteFromBrowser(
        browser, NARROW(NARROW(RefListUtils.Pop(l), RefList.T).head, TEXT));
    END;
  END EmptyCodeViewBrowser;

PROCEDURE InitCodeViewBrowser (sess: Session; alg: Algorithm.T) =
  VAR
    l       := alg.codeViews;
    browser := FormsVBT.GetVBT(sess.fv, "views");
  BEGIN                         (* LL = VBT.mu *)
    WHILE l # NIL DO
      InsertToBrowser(
        browser, NARROW(NARROW(RefListUtils.Pop(l), RefList.T).head, TEXT));
    END;
  END InitCodeViewBrowser;

(* **************** Broadcasting to Zeus Routines **************** *)

PROCEDURE Startrun(sess: Session) =
  BEGIN                         (* LL = {} *)
    Zeus.Dispatch(sess.alg, Zeus.EventStyle.Broadcast, Zeus.MaxPriority,
                  "ZeusClass.Startrun", DispatchStartrun, NIL);
  END Startrun;
  
PROCEDURE DispatchStartrun (v: ZeusClass.T; <*UNUSED*> args: REFANY) =
  <* LL = {} *>
  (* Must test type of v, since Broadcast events go to both. *)
  BEGIN
    TYPECASE v OF
    | View.T (v) => v.startrun();
    ELSE
    END;
  END DispatchStartrun;


PROCEDURE Endrun(sess: Session) =
  BEGIN                         (* LL = {} *)
    Zeus.Dispatch(sess.alg, Zeus.EventStyle.Broadcast, Zeus.MaxPriority,
                  "ZeusClass.Endrun", DispatchEndrun, NIL);
  END Endrun;
  
PROCEDURE DispatchEndrun (v: ZeusClass.T; <*UNUSED*> args: REFANY) =
  <* LL = {} *>
  (* Must test type of v, since Broadcast events go to both. *)
  BEGIN
    TYPECASE v OF
    | View.T (v) => v.endrun();
    ELSE
    END;
  END DispatchEndrun;


(* **************** Interpreter **************** *)

PROCEDURE PanelThread (pc: PanelClosure): REFANY =
  (* LL = {} *)
  VAR
    l    : RefList.T;              (* of Session *)
    sess : Session;
    panel          := pc.panel;

  PROCEDURE OKToPause (): BOOLEAN =
    BEGIN
      RETURN (panel.runState = RunState.Paused)
               OR (panel.runState = RunState.Stepping);
(*      RETURN (panel.runState = RunState.Paused)
             OR ((scripting # ScriptingState.Playback)
                 AND (panel.runState = RunState.Stepping));*)
    END OKToPause;

  BEGIN                         (* LL = {} *)
(* DebugWrite("P-id = " & Fmt.Ref(Thread.Self()) & "\n");*)
    panel.panelThread := Thread.Self();
    WHILE TRUE DO
      <* ASSERT (panel.numActive = 0) *>
      LOCK panel.mu DO
(* IF debugP THEN DebugWrite("Pi "); END;*)
        panel.clock := 0;
        panel.subclock := 0;
        IF panel.quit THEN RETURN NIL; END;
        IF scripting = ScriptingState.Playback THEN
          PanelThreadPlayback(panel, TRUE);
        END;
        WHILE (panel.runState # RunState.Running)
          AND (panel.runState # RunState.Stepping)
          AND (NOT panel.quit) DO
          (* wait for a user-invoked Step or Go command... *)
(* IF debugP THEN DebugWrite("Pj "); END;*)
          Thread.Wait(panel.mu, panel.runCond);
        END;
        IF panel.quit THEN RETURN NIL; END;
        panel.clock := 1;    (* clock is 0 only when idle *)
      END;
      LOCK VBT.mu DO
        LOCK panel.mu DO
          l := panel.sessions;
          WHILE l # NIL DO
            sess := RefListUtils.Pop(l);
            IF NOT sess.quit THEN
              ChangeSessActive(sess, panel, TRUE);
              sess.waitUntil := 0;
              FormsVBT.MakeActive(sess.fv, "abort");
            END;
          END;
        END;
      END;
      LOCK panel.mu DO
        WHILE panel.numActive > 0 DO
(* IF debugP THEN DebugWrite("Pa "); END;*)
          panel.numRunning := 0;
          l := panel.sessions;
          WHILE l # NIL DO
            sess := l.head;
            IF sess.active AND (sess.waitUntil <= panel.clock) THEN 
	      sess.running := TRUE;
	      INC(panel.numRunning);
	      Thread.Broadcast(sess.runCond);
            ELSE
              sess.running := FALSE;
	    END;
            l := l.tail;
          END;
          IF panel.numRunning = 0 THEN 
(* IF debugP THEN DebugWrite("Pb "); END;*)
            INC(panel.clock);
            panel.subclock := 0;
          ELSE
(* IF debugP THEN DebugWrite("Pc "); END;*)
            Thread.Wait(panel.mu, panel.algCond);
            (* now panel.numRunning = 0 *)
(* IF debugP THEN DebugWrite("Pd "); END;*)
            IF scripting = ScriptingState.Playback THEN
              PanelThreadPlayback(panel, FALSE);
            END;
            IF OKToPause() THEN
              WaitForUser(panel);
            END;
            INC(panel.subclock);
          END;
        END;
      END;
    END;
    RETURN NIL;
  END PanelThread;

PROCEDURE PanelThreadPlayback(panel: T; frameStart: BOOLEAN) =
  <* LL = {panel.mu} *>
  (* but NOT VBT.mu *)
  (* No algorithm threads are running.  Release panel.mu, lock VBT.mu.
     If frameStart, flush playback records that aren't frame-starters.
     Call DoNextPlayback, release VBT.mu, reacquire panel.mu, and return. *)
  BEGIN
(* IF debugP THEN DebugWrite("ptp "); END;*)
    Thread.Release(panel.mu);
    TRY
      LOCK VBT.mu DO 
        IF frameStart THEN FlushFramePlayback() END;
        DoNextPlayback(panel);
      END;
    FINALLY
      Thread.Acquire(panel.mu);
    END;
  END PanelThreadPlayback;

PROCEDURE WaitForUser (panel: T) =
  <* LL = {panel.mu} *>
  (* but NOT VBT.mu *)
  (* panel.numRunning = 0, so no algorithm threads are running.  Lock
     ordering requires us to release panel.mu before we can lock VBT.mu.
     We need to lock VBT.mu to enable/disable feedback.  Sleeping unlocks
     panel.mu anyway, so it's probably no big deal to unlock it a little
     earlier. *)
  VAR
    l: RefList.T;
    sess: Session;
  BEGIN
(* IF debugP THEN DebugWrite("wfu "); END;*)
    Thread.Release(panel.mu);
    LOCK VBT.mu DO
      LOCK panel.mu DO
        l := panel.sessions;
        WHILE l # NIL DO
          sess := RefListUtils.Pop(l);
          IF sess.active THEN EnableFeedback (sess) END;
        END;
      END
    END;
    TRY
      LOCK panel.mu DO Thread.Wait(panel.mu, panel.runCond) END;
    FINALLY
      LOCK VBT.mu DO
        LOCK panel.mu DO
          l := panel.sessions;
          WHILE l # NIL DO
            sess := RefListUtils.Pop(l);
            DisableFeedback (sess);   (* not just for active sessions *)
          END;
        END
      END;
      Thread.Acquire(panel.mu);
    END;
  END WaitForUser;


VAR
  NullDataView := NEW(DataView.T);

PROCEDURE AlgThread (ac: AlgClosure): REFANY =
  VAR finalState: RunState;
  BEGIN                         (* LL = {} *)
    WITH panel = ac.panel,
         sess  = ac.sess,
         alg = sess.alg   
     DO
(* DebugWrite("A-id = " & Fmt.Ref(Thread.Self()) & "\n");*)
      sess.algThread := Thread.Self();
      WHILE TRUE DO
(* IF debugP THEN DebugWrite("Ak "); END;*)
        LOCK panel.mu DO
          IF sess.quit THEN RETURN NIL; END;
          (* wait for a user-invoked Step or Go command... *)
          Thread.Wait(panel.mu, sess.runCond);
(* IF debugP THEN DebugWrite("Al "); END;*)
          IF sess.quit THEN RETURN NIL; END;
        END;
(* IF debugP THEN DebugWrite("Am "); END;*)
        <* ASSERT (sess.active) *>
        LOCK VBT.mu DO AttachViews(sess); END;
        IF alg.varPath = NIL THEN alg.varPath := GetPath() END;
        alg.varView := NIL;
        Startrun(sess); 
        IF alg.varView = NIL THEN alg.varView := NullDataView END;
        finalState := RunState.Done;
        TRY
          IF sess.algIsSet THEN
            LOCK VBT.mu DO sess.alg.updateEventCounts(TRUE) END;
(* IF debugP THEN DebugWrite("An "); END;*)
            sess.alg.run();
(* IF debugP THEN DebugWrite("Ao "); END;*)
            LOCK VBT.mu DO sess.alg.updateEventCounts(FALSE) END;
          END
        EXCEPT
          Thread.Alerted => finalState := RunState.Aborted;
(* IF debugP THEN DebugWrite("Ap "); END;*)
        | FormsVBT.Error (errorText) =>
            ReportError("FormsVBT error in algorithm: " & errorText);
        ELSE
          ReportError("Unhandled exception raised in algorithm.");
        END;
  (* Endrun is broadcast (doesn't go through PostEventCallback),
     so we can now unregister from the panel's group of alg threads: *)
(* IF debugP THEN DebugWrite("Aq "); END;*)
        IF NOT sess.quit THEN (* test unnecessary? *)
          LOCK VBT.mu DO FormsVBT.MakeDormant(sess.fv, "abort"); END 
        END;
        LOCK panel.mu DO
          ChangeSessActive(sess, panel, FALSE);
        END;
        LOCK VBT.mu DO SetRunState(panel, finalState); END;
        Endrun(sess);
        LOCK panel.mu DO StopRunning(sess, panel) END;
      END;
      RETURN NIL;
    END;
  END AlgThread;

PROCEDURE StopRunning (sess: Session; panel: T) =
  <* LL.sup = panel.mu *>
  BEGIN
(* IF debugP THEN DebugWrite("sr "); END;*)
    IF sess.running THEN
      sess.running := FALSE;
      DEC(panel.numRunning);
      IF panel.numRunning = 0 THEN Thread.Signal(panel.algCond); END;
    END;
  END StopRunning;

PROCEDURE ChangeSessActive(sess: Session; panel: T; act: BOOLEAN) =
  <*LL = panel.mu*>
  BEGIN
    IF RefList.Member(panel.sessions, sess) THEN
      IF act THEN
        IF NOT sess.active THEN INC(panel.numActive) END;
      ELSE
        IF sess.active THEN DEC(panel.numActive) END;
      END;
      sess.active := act;
      panel.mustSynch := (panel.numActive > 1) OR
                             (scripting # ScriptingState.Off);
    END;
  END ChangeSessActive;
      

PROCEDURE Go (panel: T; eventTime: VBT.TimeStamp) =
  BEGIN                         (* LL = VBT.mu *)
    GrabFocus(panel, eventTime);
    CASE GetRunState(panel) OF

    | RunState.Virgin, RunState.Done, RunState.Aborted =>
        SetRunState(panel, RunState.Running);
        Thread.Broadcast(panel.runCond);

    | RunState.Stepping, RunState.Paused =>
        SetRunState(panel, RunState.Running);
        Thread.Broadcast(panel.runCond);

    | RunState.Running => SetRunState(panel, RunState.Paused);

    END;
  END Go;


PROCEDURE Step (panel: T; eventTime: VBT.TimeStamp) =
  BEGIN                         (* LL = VBT.mu *)
    GrabFocus(panel, eventTime);
    SetRunState(panel, RunState.Stepping);
    Thread.Broadcast(panel.runCond);
  END Step;

  

PROCEDURE AbortInternal (panel: T; eventTime: VBT.TimeStamp) =
  (* LL < panel.mu *)
  BEGIN
    LOCK panel.mu DO AbortWithLock(panel, eventTime) END;
  END AbortInternal;

PROCEDURE AbortWithLock (panel: T; eventTime: VBT.TimeStamp) =
  (* LL = panel.mu *)
  VAR
    l   : RefList.T;
    sess: Session;
  BEGIN
    (* DebugStart();*)
    (* DebugWrite("abort:" & Fmt.Ref(Thread.Self()) & "\n");*)
    IF NOT stateIdle[panel.runState] THEN
      Thread.Broadcast(panel.runCond);
      l := panel.sessions;
      WHILE l # NIL DO sess := RefListUtils.Pop(l); AbortAlg(sess); END;
    END;
    ReleaseFocus(panel, eventTime);
  END AbortWithLock;


PROCEDURE AbortAlg (sess: Session) =
  BEGIN                         (* LL = arbitrary *)
    DisableFeedback(sess);
    IF sess.active THEN
      Thread.Alert(sess.algThread);
      ZeusPrivate.AlertViews(sess);    (* abort any alertable views *)
    END;
  END AbortAlg;


PROCEDURE PreEventCallback (<*UNUSED*> sess     : Session;
                            <*UNUSED*> initiator: ZeusClass.T;
                            <*UNUSED*> style    : Zeus.EventStyle;
                            <*UNUSED*> priority : INTEGER;
                            <*UNUSED*> eventName: TEXT             ) 
  RAISES {Thread.Alerted} =
  BEGIN                         (* LL = arbitrary *)
    IF Thread.TestAlert() THEN RAISE Thread.Alerted END;
  END PreEventCallback;


PROCEDURE PostEventCallback (           sess     : Session;
                                        initiator: ZeusClass.T;
                                        style    : Zeus.EventStyle;
                                        priority : INTEGER;
                             <*UNUSED*> eventName: TEXT             )
  (* LL <= VBT.mu *)
  RAISES {Thread.Alerted} =
  VAR
    feedFg, pauseFg: BOOLEAN;
    alg            : Algorithm.T;
    panel                        := Resolve(NIL);
    now, delayFrac : REAL;

  PROCEDURE OKToPause (): BOOLEAN =
    (* LL = panel.mu *)
    BEGIN
      RETURN
        (panel.runState = RunState.Paused)
          OR ((panel.mustSynch OR (panel.runState = RunState.Stepping))
                AND (priority <= panel.priority) AND alg.stopAtEvent
                AND sess.evtWasHandled);
    END OKToPause;
  PROCEDURE FeedbackOK (): BOOLEAN =
    (* LL = panel.mu *)
    BEGIN
      RETURN (panel.runState = RunState.Paused)
               OR ((panel.runState = RunState.Stepping)
                     AND (priority <= panel.priority) AND alg.stopAtEvent
                     AND sess.evtWasHandled);
    END FeedbackOK;
  BEGIN
    IF (style = Zeus.EventStyle.Output) OR (style = Zeus.EventStyle.Code) THEN
      (* LL < VBT.mu *)
      alg := NARROW(initiator, Algorithm.T);
      LOCK panel.mu DO feedFg := FeedbackOK(); pauseFg := OKToPause(); END;
      IF (NOT feedFg) AND sess.evtWasHandled THEN
        IF style = Zeus.EventStyle.Output THEN
          delayFrac := panel.minDelayFrac;
        ELSIF style = Zeus.EventStyle.Code THEN
          delayFrac := panel.codeDelayFrac;
        ELSE
          delayFrac := 0.0;
        END;
        now := Animate.ATime();
        IF now < delayFrac THEN
          TRY
            Thread.AlertPause(MAX(0.0D0, FLOAT(panel.delayTime
                                           * (delayFrac - now), LONGREAL)));
          EXCEPT
            Thread.Alerted => Thread.Alert(Thread.Self());
          END;
        END;
      END;
      (* LOCK panel.mu DO feedFg := FeedbackOK(); END;*)
(* IF debugP THEN DebugWrite("pec "); END;*)
      LOCK panel.mu DO
        IF pauseFg (* OKToPause() *) THEN
          <* ASSERT NOT RefList.Member(panel.sessions, sess) OR sess.running *>
          StopRunning(sess, panel);
          sess.waitUntil := panel.clock + alg.waitAtEvent;
          Thread.AlertWait(panel.mu, sess.runCond);
        END;
      END;
    END;
    IF Thread.TestAlert() THEN RAISE Thread.Alerted END;
  END PostEventCallback;


PROCEDURE GetRunState (panel: T): RunState =
  BEGIN                         (* LL = arbitrary *)
    LOCK panel.mu DO RETURN panel.runState; END;
  END GetRunState;

PROCEDURE SetRunState (panel: T;
                       state: RunState;
                       msg  : TEXT       := NIL) =
  <* LL = VBT.mu *>
  BEGIN
    LOCK panel.mu DO SetRunStateWithLock(panel, state, msg) END;
  END SetRunState;

PROCEDURE SetRunStateWithLock (panel: T;
                               state: RunState;
                               msg  : TEXT       := NIL) =
  <* LL = {VBT.mu, panel.mu} *>
  PROCEDURE Set (btn: TEXT; status: TEXT) =
    VAR l: RefList.T;
        abortable := NOT stateIdle[state];
    BEGIN
      l := panel.sessions;
      WHILE l # NIL DO
        WITH sess = NARROW(RefListUtils.Pop(l), Session) DO
          IF abortable THEN
            FormsVBT.MakeDormant(sess.fv, "algs")
          ELSE
            FormsVBT.MakeActive(sess.fv, "algs")
          END
        END
      END;
      IF abortable THEN
        FormsVBT.MakeDormant(panel.fv, "restoreBtn");
        FormsVBT.MakeDormant(panel.fv, "restoreShortcut");
        FormsVBT.MakeDormant(panel.fv, "restoreContents");
        FormsVBT.MakeDormant(panel.fv, "sessionMenu");
        FormsVBT.MakeActive(panel.fv, "abortBtn");
      ELSE
        FormsVBT.MakeActive(panel.fv, "restoreBtn");
        FormsVBT.MakeActive(panel.fv, "restoreShortcut");
        FormsVBT.MakeActive(panel.fv, "restoreContents");
        FormsVBT.MakeActive(panel.fv, "sessionMenu");
        FormsVBT.MakeDormant(panel.fv, "abortBtn");
      END;
      ActivateScriptButtons(panel);
      FormsVBT.PutText(panel.fv, "goText", btn);
      IF msg # NIL THEN status := status & " - " & msg END;
      FormsVBT.PutText(panel.fv, "status", status);
    END Set;

  BEGIN
    IF (panel.numActive > 0) AND ((state = RunState.Aborted)
                                   OR (state = RunState.Done)) THEN
      RETURN;
    END;
    panel.runState := state;
    CASE state OF
    | RunState.Virgin => Set("GO", "Ready");
    | RunState.Running => Set("PAUSE", "Running");
    | RunState.Stepping => Set("RESUME", "Paused");
    | RunState.Paused => Set("RESUME", "Paused");
    | RunState.Done => Set("GO", "Completed");
    | RunState.Aborted => Set("GO", "Aborted");
    END;
  END SetRunStateWithLock;



(* **************** Reactivity / Feedback **************** *)

PROCEDURE EnableFeedback (sess: Session) =
  <* LL = VBT.mu *>
  BEGIN
    ControlSessionFeedback(sess, TRUE);
  END EnableFeedback;

PROCEDURE DisableFeedback (sess: Session) =
  <* LL = VBT.mu *>
  BEGIN
    ControlSessionFeedback(sess, FALSE);
  END DisableFeedback;

PROCEDURE ControlSessionFeedback (sess: Zeus.Session; on: BOOLEAN) =
  <* LL = VBT.mu *>
  VAR l := sess.views;
  BEGIN
    WITH alg = sess.alg DO
      alg.reactivity(on);
      WHILE l # NIL DO
        WITH view = NARROW(RefListUtils.Pop(l), View.T) DO
          IF view.isCompat(alg) THEN view.reactivity(on); END;
        END;
      END;
    END;
  END ControlSessionFeedback;

PROCEDURE StartFeedback (alg: Algorithm.T) RAISES {Thread.Alerted} =
<* LL = {}, S = Running *>
(* Suspend the algorithm and allow feedback events (as if the user had
   clicked Pause).  Return after "alg" has called EndFeedback.  This
   procedure is a noop if there already is a 'pending' StartFeedback for
   this alg. *)
  VAR sess := NARROW(Zeus.Resolve(alg), Session);
  BEGIN
    LOCK VBT.mu DO
      IF NOT sess.feedbackOn THEN
        sess.feedbackOn := TRUE;
        EnableFeedback(sess);
        TRY Thread.AlertWait(VBT.mu, sess.feedCond);
        FINALLY
          DisableFeedback(sess);
          sess.feedbackOn := FALSE;
        END;
      END;
    END;
  END StartFeedback;

PROCEDURE EndFeedback (alg: Algorithm.T) =
  <* LL = VBT.mu, S = Paused *>
  (* This procedure signals a previous call to StartFeedback to return.  It
     is typically called from an algorithm's Feedback method. *)
  VAR sess := NARROW(Zeus.Resolve(alg), Session);
  BEGIN
    IF NOT sess.feedbackOn THEN
      ReportError("EndFeedback called with feedback off")
    ELSE
      Thread.Broadcast(sess.feedCond);
    END;
  END EndFeedback;

PROCEDURE Pause (alg: Algorithm.T; msg: TEXT := NIL)
  RAISES {Thread.Alerted} =
  <* LL = 0, S = Running *>
  VAR
    sess  := NARROW(Zeus.Resolve(alg), Session);
    panel := Resolve(NIL);
  BEGIN
    LOCK VBT.mu DO SetRunState(panel, RunState.Paused, msg) END;
    LOCK panel.mu DO
      StopRunning(sess, panel);
      sess.waitUntil := panel.clock;
      Thread.AlertWait(panel.mu, sess.runCond)
    END
  END Pause;


(* **************** Event Priority **************** *)

(* PROCEDURE GetPriority (): INTEGER; *)
(* Report what priority the user has set in the control panel. *)
<*UNUSED*> PROCEDURE GetPriority (): INTEGER =
  (* LL = VBT.mu *)
  BEGIN
    RETURN GetPanelPriority(Resolve(NIL));
  END GetPriority;

(* PROCEDURE SetPriority (priority: INTEGER); *)
(* Change the priority.  Client algorithms can use this to cause events to
   be generated that are not included in the "Step" command.  To do so, the
   algorithm first retrieves the current priority, then lowers it (probably
   to 0), does some stuff, then restores the priority to its initial
   value. *)
<*UNUSED*> PROCEDURE SetPriority (priority: INTEGER) =
  (* LL = VBT.mu *)
  BEGIN
    SetPanelPriority(Resolve(NIL), priority);
  END SetPriority;

PROCEDURE SetPanelPriority (panel: T; priority: INTEGER) =
  BEGIN                         (* LL = VBT.mu *)
    LOCK panel.mu DO
      panel.priority := priority;
      FormsVBT.PutInteger(panel.fv, "priority", priority);
    END;
  END SetPanelPriority;

PROCEDURE GetPanelPriority (panel: T): INTEGER =
  BEGIN                         (* LL = arbitrary *)
    LOCK panel.mu DO RETURN panel.priority END;
  END GetPanelPriority;


(* **************** Speedometer **************** *)

(* M3 FormsVBT doesn't have a REAL-valued slider, so this is
   done another way. *)

PROCEDURE UpdateSpeed (panel: T) =
  (* LL = VBT.mu *)
  BEGIN
    panel.delayTime := FromFancySlider(panel);
    Script(ActionType.Speed, Sx.FromReal(panel.delayTime));
    Animate.SetDuration(panel.delayTime);
    FormsVBT.PutText(
      panel.fv, "delayText", Fmt.Real(panel.delayTime, Fmt.Style.Fix, 4));
  END UpdateSpeed;

PROCEDURE UpdateMinDelay (panel: T) =
  (* LL = VBT.mu *)
  BEGIN
    panel.minDelayFrac := FromSimpleSlider(panel, "minDelayFrac");
    Script(ActionType.MinDelay, Sx.FromReal(panel.minDelayFrac));
    FormsVBT.PutText(panel.fv, "minDelayText",
                     Fmt.Real(panel.minDelayFrac, Fmt.Style.Fix,  2));
  END UpdateMinDelay;

PROCEDURE UpdateCodeDelay (panel: T) =
  (* LL = VBT.mu *)
  BEGIN
    panel.codeDelayFrac := FromSimpleSlider(panel, "codeDelayFrac");
    Script(ActionType.CodeDelay, Sx.FromReal(panel.codeDelayFrac));
    FormsVBT.PutText(panel.fv, "codeDelayText",
                     Fmt.Real(panel.codeDelayFrac, Fmt.Style.Fix, 2));
  END UpdateCodeDelay;

PROCEDURE USFError (panel: T; t: TEXT) =
  (* LL = VBT.mu *)
  BEGIN
    FormsVBT.PutText(panel.fv, "maxSpeedFactor",
                     Fmt.Real(panel.speedFactor, Fmt.Style.Fix, 2));
    ReportError("Bad max speed factor value: " & t)
  END USFError;

PROCEDURE UpdateSpeedFactor (panel: T) =
  (* LL = VBT.mu *)
  VAR
    t       := FormsVBT.GetText(panel.fv, "maxSpeedFactor");
    r: REAL;
  BEGIN
    TRY
      r := Lex.Real(TextRd.New (t));
      IF r <= 1.0 THEN
        USFError(panel, t);
      ELSE
        panel.speedFactor := r;
        Script(ActionType.SpeedFactor, t);
        panel.logSpeedFactor :=
          Math.log(FLOAT(panel.speedFactor, LONGREAL));
        UpdateSpeed(panel)
      END;
    EXCEPT
      Lex.Error, FloatMode.Trap => USFError(panel, t);
    END;
  END UpdateSpeedFactor;


PROCEDURE SetupSliderConversion (    fv               : FormsVBT.T;
                                name: TEXT;
                                 VAR min, range, value: LONGREAL    ) =
  (* LL = VBT.mu *)
  (* range is set to the range of the slider, min is set to its min, and
     value is set to its value. *)
  VAR v := NARROW(FormsVBT.GetVBT(fv, name), ScrollerVBT.T);
  BEGIN
    min := FLOAT(ScrollerVBT.GetMin(v), LONGREAL);
    range := FLOAT(ScrollerVBT.GetMax(v), LONGREAL) - min;
    value := FLOAT(ScrollerVBT.Get(v), LONGREAL);
  END SetupSliderConversion;

PROCEDURE FromSimpleSlider(panel: T; name: TEXT): REAL =
  VAR min, range, value: LONGREAL;
  BEGIN
    SetupSliderConversion(panel.fv, name, min, range, value);
    RETURN FLOAT((value - min) / range);
  END FromSimpleSlider;

PROCEDURE ToSimpleSlider(panel: T; name: TEXT; r: REAL) =
  VAR min, range, value: LONGREAL;
  BEGIN
    SetupSliderConversion(panel.fv, name, min, range, value);
    WITH frac = FLOAT(MAX(0.0, MIN(1.0, r)), LONGREAL) DO
      FormsVBT.PutInteger(panel.fv, name, ROUND(frac * range + min));
    END;
  END ToSimpleSlider;


CONST
  SpeedoBreak: LONGREAL = 0.1d0;
  SpeedoRange: LONGREAL = (1.0d0 - SpeedoBreak);
  SpeedoMid: LONGREAL = (SpeedoBreak + 0.5d0 * SpeedoRange);

PROCEDURE FromFancySlider (panel: T): REAL =
  (* LL = VBT.mu *)
  (* Returns a delay value *)
  VAR min, range, value: LONGREAL;
  BEGIN
    SetupSliderConversion(panel.fv, "delay", min, range, value);
    value := (value - min) / range;
    IF value <= SpeedoBreak THEN
      RETURN FLOAT(value) / (panel.speedFactor * FLOAT(SpeedoBreak));
    ELSE
      RETURN FLOAT(Math.exp(panel.logSpeedFactor * 2.0d0
                              * (value - SpeedoMid) / SpeedoRange))
    END;
  END FromFancySlider;

PROCEDURE ToFancySlider (panel: T; delay: REAL) =
  (* LL = VBT.mu *)
  VAR min, range, value: LONGREAL;
  BEGIN
    SetupSliderConversion(panel.fv, "delay", min, range, value);
    IF delay <= (1.0 / panel.speedFactor) THEN
      FormsVBT.PutInteger(
        panel.fv, "delay",
        ROUND(SpeedoBreak * FLOAT(delay * panel.speedFactor, LONGREAL)
                * range + min));
    ELSE
      FormsVBT.PutInteger(
        panel.fv, "delay",
        ROUND(
          (SpeedoRange * Math.log(FLOAT(delay, LONGREAL))
             / (panel.logSpeedFactor * 2.0d0) + SpeedoMid) * range + min));
    END;
  END ToFancySlider;


(* **************** Keyboard Focus **************** *)

PROCEDURE GrabFocus (<*UNUSED*> panel: T; <*UNUSED*> time: VBT.TimeStamp) =
  BEGIN
  END GrabFocus;


PROCEDURE ReleaseFocus (<*UNUSED*> panel: T; <*UNUSED*> time: VBT.TimeStamp) =
  BEGIN
  END ReleaseFocus;



(* **************** Photo Album **************** *)

PROCEDURE CntViews (panel: T): CARDINAL =
  VAR
    rest, views: RefList.T;
    cnt        : CARDINAL := 0;
  BEGIN
    LOCK panel.mu DO
      rest := panel.sessions;
      WHILE rest # NIL DO
        views := NARROW(rest.head, Session).views;
        WHILE views # NIL DO INC(cnt); views := views.tail; END;
        rest := rest.tail;
      END;
    END;
    RETURN cnt
  END CntViews;

PROCEDURE TakePhotos (panel: T) =
  VAR rest, views: RefList.T;
  BEGIN
    LOCK panel.mu DO
      rest := panel.sessions;
      WHILE rest # NIL DO
        views := NARROW(rest.head, Session).views;
        WHILE views # NIL DO
          WITH view  = NARROW(views.head, View.T),
               flex  = NARROW(MultiFilter.Child(panel.album), FlexVBT.T),
               album = NARROW(MultiFilter.Child(flex), AlbumVBT.T)        DO
            album.add(view);
          END;
          views := views.tail;
        END;
        rest := rest.tail;
      END;
    END;
  END TakePhotos;

EXCEPTION Oops;

PROCEDURE GetReal (fv: FormsVBT.T; name: TEXT): REAL RAISES {Oops} =
  VAR
    t       := FormsVBT.GetText(fv, name);
    r: REAL;
  BEGIN
    TRY
      r := Lex.Real(TextRd.New (t));
      IF r <= 5.0 THEN
        ReportError("Bad value (too small) for " & name & ": " & t);
        RAISE Oops;
      ELSE
        RETURN r
      END;
    EXCEPT
      Lex.Error, FloatMode.Trap =>
        ReportError("Bad real value for " & name & ": " & t);
        RAISE Oops;
    END;
  END GetReal;

CONST
  AlbumAxis = Axis.T.Ver;
(*OBSOLETE  FixedShape = FlexShape.Shape{FlexShape.Fixed, FlexShape.Fixed};*)
  FixedShape = FlexVBT.Fixed;

PROCEDURE NewAlbum (fv: FormsVBT.T; cnt: CARDINAL): AlbumVBT.T RAISES {Oops} =
  BEGIN
    RETURN NEW(AlbumVBT.T).init(AlbumAxis, cnt, GetReal(fv, "photoWidth"),
                                GetReal(fv, "photoHeight"))
  END NewAlbum;

TYPE
  MyViewport = ViewportVBT.T OBJECT
    panel: T;
  OVERRIDES
    misc := MiscVP;
  END;

PROCEDURE MiscVP(t: MyViewport; READONLY cd: VBT.MiscRec) =
  BEGIN
    IF cd.type = VBT.Deleted THEN
      t.panel.album := NIL
    END;
    ViewportVBT.T.misc(t, cd);
  END MiscVP;

PROCEDURE SetAlbum (panel: T; cnt: CARDINAL) RAISES {Oops} =
  BEGIN
    IF panel.album = NIL THEN
      panel.album :=
        NEW(MyViewport, panel := panel).init(
          NEW(FlexVBT.T).init(
            NewAlbum(panel.fv, cnt), FixedShape),
          Axis.Other[AlbumAxis],
          shapeStyle := ViewportVBT.ShapeStyle.Unrelated,
          scrollStyle := ViewportVBT.ScrollStyle.HorAndVer);
      (* panel.album := NEW(Filter.T).init(NewAlbum(panel.fv,
         cnt)); *)
      Trestle.Attach(panel.album);
      Trestle.Decorate(
        panel.album, applName := "Zeus Photo Album");
      Trestle.MoveNear(panel.album, NIL);
    ELSE
      WITH flex = MultiFilter.Child(panel.album),
           oldAlbum = MultiFilter.Replace(
                        flex, NewAlbum(panel.fv, cnt)) DO
        VBT.Discard(oldAlbum)
      END
    END;
    panel.cntViews := cnt;
  END SetAlbum;

PROCEDURE Photo (panel: T) =
  VAR cnt := CntViews(panel);
  BEGIN                         (* LL = VBT.mu *)
    TRY
      IF panel.album = NIL OR panel.cntViews # cnt THEN
        SetAlbum(panel, cnt);
      END;
    EXCEPT
      Oops =>                   (* don't do anything *)
    END;
    TakePhotos(panel);
  END Photo;

PROCEDURE ClearAlbum (panel: T) =
  BEGIN                         (* LL = VBT.mu *)
    IF panel.album # NIL THEN
      WITH flex  = NARROW(MultiFilter.Child(panel.album), FlexVBT.T),
           album = NARROW(MultiFilter.Child(flex), AlbumVBT.T) DO
        album.clear()
      END
    END
  END ClearAlbum;


(* PROCEDURE PhotographViews(alg: Algorithm.T) RAISES {Thread.Alerted}; *)
<* LL=VBT.mu, s=Any *>
(* This procedure takes a "photograph" (captures a miniture pixmap)
   of all active views and enters them into an "photo album".  It
   creates the album if none exists.  All views will get redisplayed
   (and maybe reshaped) when the photograph is taken. *)
<* UNUSED *>
PROCEDURE PhotographViews (<* UNUSED *> alg: Algorithm.T) =
  VAR panel := Resolve(NIL);
  BEGIN                         (* LL = VBT.mu *)
    Photo(panel)
  END PhotographViews;


(*PROCEDURE ClearPhotoAlbum(alg: Algorithm.T) RAISES {Thread.Alerted}; *)
<* LL=VBT.mu, s=Any *>
(* This procedure removes any "photographs" from the "photo album"
   (see PhotographViews, above). *)
<* UNUSED *>
PROCEDURE ClearPhotoAlbum (<* UNUSED *> alg: Algorithm.T) =
  VAR panel := Resolve(NIL);
  BEGIN                         (* LL = VBT.mu *)
    ClearAlbum(panel)
  END ClearPhotoAlbum;



(* **************** Scripting **************** *)

TYPE
  ActionType = {Go, Step, Abort, Speed, MinDelay, CodeDelay, SpeedFactor,
                Priority, Snapshot, Restore, Sessions, Photo, ClearAlbum,
                Algs, Views, AbortAlg, Destroy, ToggleTSplit,
                FutureGo, FuturePause, GrabData};
  ScriptRec = REF RECORD
                    action: ActionType;
                    clock  : INTEGER;
                    subclock  : INTEGER;
                    args  : REFANY;
                  END;
  ScriptingState = {Off, Recording, Playback};

VAR scriptOut: RefList.T; (* of ScriptRec, in reverse order *)
    scriptOutFile: TEXT; (* name of file where script will be written *)
    scriptIn: RefList.T; (* of ScriptRec, in forward order *)
    scripting: ScriptingState := ScriptingState.Off;

VAR actName:= ARRAY ActionType OF TEXT
                  {"Go", "Step", "Abort", "Speed", "MinDelay", "CodeDelay",
                   "SpeedFactor", "Priority", "Snapshot", "Restore",
                   "Sessions", "Photo", "ClearAlbum", "Algs", "Views",
                   "AbortAlg", "Destroy", "ToggleTSplit",
                   "FutureGo", "FuturePause", "GrabData"};


PROCEDURE StartScript(file: TEXT) =
  <* LL=VBT.mu *>
  BEGIN
    IF scripting = ScriptingState.Off THEN
      scriptOutFile := file;
      scriptOut := NIL;
      ChangeScriptingState(ScriptingState.Recording);
      (* move the following to just after Go/Step has been pressed. *)
(*      Script(ActionType.Restore, SnapshotToList());*)
    END (* IF *);
  END StartScript;

PROCEDURE StopScript() =
  <* LL=VBT.mu *>
  BEGIN
    IF scripting = ScriptingState.Recording THEN
      WriteScript(scriptOutFile);
      ChangeScriptingState(ScriptingState.Off);
    END (* IF *);
  END StopScript;

PROCEDURE WriteScript(file: TEXT) =
  <* LL=VBT.mu *>
  (* write scriptOut to the named file in reverse order *)
  VAR
    wr:= FileWr.Open(file);
    rec: ScriptRec;
    list := RefList.ReverseD(scriptOut);
  BEGIN
    scriptOut := NIL;
    WHILE list # NIL DO
      rec := RefListUtils.Pop(list);
      TRY
        Wr.PutText(wr, "(" & actName[rec.action] & " " &
          "(" & Fmt.Int(rec.clock) & " " & Fmt.Int(rec.subclock) & ") ");
        Sx.Print(wr, rec.args);
        Wr.PutText(wr, ")\n" );
      EXCEPT
        Sx.PrintError =>
      END;
    END (* WHILE *);
    Wr.Close(wr);
  END WriteScript;


PROCEDURE Script (act: ActionType; argsIn: REFANY := NIL) =
(* To find the calling sequences for Script(), search for "ActionType.";
   collecting them here doesn't work, since they tend to get obsolete. *)
  <* LL=VBT.mu *>
  VAR panel := Resolve(NIL);
  BEGIN
    IF scripting = ScriptingState.Recording THEN
      RefListUtils.Push(scriptOut, NEW(ScriptRec, action := act,
                               clock := panel.clock,
                               subclock := panel.subclock,
                               args := argsIn));
    END (* IF *);
  END Script;

PROCEDURE ScriptMaybeStartFrame(panel: T) =
  BEGIN
    LOCK panel.mu DO
      IF stateIdle[panel.runState] AND
        (scripting = ScriptingState.Recording) THEN
        Script(ActionType.Restore, SnapshotToList());
        Script(ActionType.FutureGo);
      END;
    END;
  END ScriptMaybeStartFrame;




PROCEDURE StartPlayback(file: TEXT) =
  <* LL=VBT.mu *>
  BEGIN
    IF scripting = ScriptingState.Off THEN
      ReadScript(file);
      ChangeScriptingState(ScriptingState.Playback);
      DoNextPlayback(Resolve(NIL));
    END (* IF *);
  END StartPlayback;

PROCEDURE StopPlayback() =
  <* LL=VBT.mu *>
  BEGIN
    IF scripting = ScriptingState.Playback THEN
      scriptIn := NIL;
      ChangeScriptingState(ScriptingState.Off);
    END (* IF *);
  END StopPlayback;

PROCEDURE DoNextPlayback(panel: T) =
  <*LL = VBT.mu*>
  VAR rec: ScriptRec;
      b: BOOLEAN;
  BEGIN
    IF scripting = ScriptingState.Playback THEN
      LOOP
        IF scriptIn = NIL THEN StopPlayback(); EXIT; END;
        rec := scriptIn.head;
        LOCK panel.mu DO
          b := (stateIdle[panel.runState] AND
                 (rec.clock + rec.subclock + panel.clock + panel.subclock = 0))
               OR ((rec.clock + rec.subclock # 0)
                   AND ((panel.clock > rec.clock)
                        OR ((panel.clock = rec.clock)
                            AND (panel.subclock >= rec.subclock))));
        END;
        IF b THEN
          EVAL RefListUtils.Pop(scriptIn);
          IF NOT Playback(panel, rec) THEN EXIT END;
        ELSE
          EXIT;
        END;
      END (* LOOP *);
    END;
  END DoNextPlayback;

      
PROCEDURE FlushFramePlayback() =
  <* LL=VBT.mu *>
  (* Delete all ScriptRecs up to the next one for time (0,0) *)
  PROCEDURE NotAtFrameStart(rec: ScriptRec): BOOLEAN =
    BEGIN
      RETURN (rec.clock + rec.subclock # 0)
    END NotAtFrameStart;
  BEGIN
(* IF debugP THEN DebugWrite("ffp "); END;*)
    IF scripting = ScriptingState.Playback THEN
      WHILE (scriptIn # NIL) AND NotAtFrameStart(scriptIn.head) DO
        EVAL RefListUtils.Pop(scriptIn);
      END;
    END;
    IF scriptIn = NIL THEN StopPlayback(); END;
  END FlushFramePlayback;
    

PROCEDURE Playback(panel: T; rec: ScriptRec):  BOOLEAN =
  (* Return TRUE if playback may continue, FALSE if algorithm should
     execute at least one step now. *)
  <* LL=VBT.mu *>
  PROCEDURE SessFromPos(pos: REF INTEGER): Session =
    BEGIN
      LOCK panel.mu DO
        IF RefList.Length(panel.sessions) > pos^ THEN
          RETURN NARROW(RefList.Nth(panel.sessions, pos^), Session);
        ELSE
          ReportError("Playback error: not enough sessions");
          RETURN NIL;
        END;
      END;
    END SessFromPos;
  BEGIN
(* IF debugP THEN DebugWrite("play:" & Fmt.Int(ORD(rec.action)) & " "); END;*)
    CASE rec.action OF
    | ActionType.Go =>
        (*        Go(panel, 0);*)    (* see FutureGo *)
    | ActionType.Step =>
        (*        Step(panel, 0);*)    (* see FutureGo *)
    | ActionType.Abort =>
        AbortInternal(panel, 0);
    | ActionType.Speed =>
        ToFancySlider(panel, NARROW(rec.args, REF REAL)^);
        UpdateSpeed(panel);    (* works because scripting # Recording *)
    | ActionType.MinDelay =>
        ToSimpleSlider(panel, "minDelayFrac", NARROW(rec.args, REF REAL)^);
        UpdateMinDelay(panel);
    | ActionType.CodeDelay =>
        ToSimpleSlider(panel, "codeDelayFrac", NARROW(rec.args, REF REAL)^);
        UpdateCodeDelay(panel);
    | ActionType.SpeedFactor =>
        FormsVBT.PutText(panel.fv, "maxSpeedFactor", rec.args);
        UpdateSpeedFactor(panel);
    | ActionType.Priority =>
        SetPanelPriority(panel, NARROW(rec.args, REF INTEGER)^);
    | ActionType.Snapshot =>
        (* don't do snapshot during playback *)
        (*        ZeusSnapshot.Snapshot(panel, rec.args);*)
    | ActionType.Restore =>
        TYPECASE rec.args OF
        | TEXT (file) =>
            ZeusSnapshot.Restore(panel, file);
        | RefList.T (list) =>
            ZeusSnapshot.RestoreFromList(panel, list);
        ELSE (* do nothing if restore format is wrong *)
        END;
    | ActionType.Sessions =>
        (* do nothing; will be caught at next frame start *)
        (* NOTE: REF BOOLEAN is also wrong type, it is Sx.True or Sx.False (an Atom.T) *)
        (*        FormsVBT.PutBoolean(panel.fv, "inTrestle",
                            NARROW(rec.args.tail.head, REF BOOLEAN)^);
           NewSessionDefault(rec.args.head, panel);
        *)
    | ActionType.Photo =>
        Photo(panel);
    | ActionType.ClearAlbum =>
        ClearAlbum(panel);
    | ActionType.Algs =>
        (* do nothing; will be caught at next frame start *)
        (*        WITH sess = SessFromPos(rec.args.head) DO
          IF sess # NIL THEN
            PickedAlg(sess, rec.args.tail.head);
            TRY
              IF sess.alg # NIL THEN sess.alg.restore(NIL); END;
            EXCEPT
              ZeusClass.Error =>
            END;
          END;
        END;
        *)
    | ActionType.Views =>
        (* do nothing; will be caught at next frame start *)
        (*        WITH sess = SessFromPos(rec.args.head) DO
          IF sess # NIL THEN
            WITH view = PickedView(sess, rec.args.tail.head) DO
              TRY
                IF view # NIL THEN view.restore(NIL); END;
              EXCEPT
                ZeusClass.Error =>
              END;
            END;
          END;
        END;
        *)
    | ActionType.AbortAlg =>
        WITH sess = SessFromPos(rec.args) DO
          IF sess # NIL THEN AbortAlg(sess); END;
        END;
    | ActionType.Destroy =>
        WITH sess = SessFromPos(rec.args) DO
          (* This works because Script checks the "scripting" variable. *)
          IF sess # NIL THEN DestroyP(NIL, NIL, sess, 0); END;
        END;
    | ActionType.ToggleTSplit =>
        IF NOT stateIdle[panel.runState] THEN
          (* number of sessions not preserved during idle states. *)
          WITH sess = SessFromPos(NARROW(rec.args, RefList.T).head) DO
            (* This works because Script checks the "scripting" variable. *)
            IF sess # NIL THEN
              ToggleTSplitP(sess.fv, NARROW(rec.args, RefList.T).tail.head, sess, 0);
            END;
          END;
        END;
    | ActionType.FutureGo =>
        SetRunState(panel, RunState.Running, "Playback Mode");
        Thread.Broadcast(panel.runCond);
        RETURN FALSE;
    | ActionType.FuturePause =>
        SetRunState(panel, RunState.Paused, "Under playback control");
    | ActionType.GrabData =>
        ZeusSnapshot.RestoreData(panel, rec.args);
        ChangeScriptingState(scripting);
    END (* CASE *);
    RETURN TRUE;
  END Playback;

EXCEPTION BadScript;

PROCEDURE ReadScript(file: TEXT) =
  <* LL=VBT.mu *>
  (* read in scriptIn from the named file *)
  PROCEDURE ParseAct(a: REFANY): ActionType
      RAISES {BadScript} =
    BEGIN
      TYPECASE a OF
      | Atom.T (sxs) =>
          WITH name = Atom.ToText(sxs) DO
            FOR i := FIRST(ActionType) TO LAST(ActionType) DO
              IF Text.Equal(name, actName[i]) THEN RETURN i END;
            END;
            RAISE BadScript;
          END;
      ELSE RAISE BadScript;
      END;
    END ParseAct;
  VAR
    rd:= FileRd.Open(file);
    ref: REFANY := NIL;
  BEGIN
    scriptIn := NIL;
    TRY
      WHILE NOT Rd.EOF(rd) DO
        TYPECASE Sx.Read(rd) OF
        | RefList.T (l) =>
            IF RefList.Length(l) >= 3 THEN ref := l.tail.tail.head END;
            WITH l2 = l.tail.head DO
              IF ISTYPE(l2, RefList.T)
                    AND (RefList.Length(l2) = 2)
                    AND ISTYPE(RefList.Nth(l2, 0), REF INTEGER)
                    AND ISTYPE(RefList.Nth(l2, 1), REF INTEGER) THEN
                RefListUtils.Push(
                    scriptIn,
                    NEW(ScriptRec,
                        action := ParseAct(l.head),
                        clock := NARROW(RefList.Nth(l2, 0), REF INTEGER)^,
                        subclock := NARROW(RefList.Nth(l2, 1), REF INTEGER)^,
                        args := ref));
              ELSE
                RAISE BadScript;
              END;
            END;
        ELSE
            RAISE BadScript;
        END;
      END (* WHILE *);
    EXCEPT
    | BadScript, Sx.ReadError => ReportError("Bad script format");
    ELSE
    END;
    scriptIn := RefList.ReverseD(scriptIn);
    Rd.Close(rd);
  END ReadScript;

PROCEDURE ChangeScriptingState (newState: ScriptingState) =
(* Implement the ScriptingState finite state machine. *)
  VAR panel := Resolve(NIL);
      fv := panel.fv;
  BEGIN (* LL = VBT.mu *)
    scripting := newState;
    IF scripting = ScriptingState.Off THEN
      FormsVBT.PutText(fv, "recordBtnText", "Record ...");
      FormsVBT.PutText(fv, "playbackBtnText", "Playback ...");
      ActivateScriptButtons(panel);
    ELSIF scripting = ScriptingState.Recording THEN
      FormsVBT.PutText(fv, "recordBtnText", "Stop Recording");
      FormsVBT.PutText(fv, "playbackBtnText", "Playback ...");
      FormsVBT.MakeActive(fv, "recordBtn");
      FormsVBT.MakeDormant(fv, "playbackBtn");
    ELSIF scripting = ScriptingState.Playback THEN
      FormsVBT.PutText(fv, "recordBtnText", "Record ...");
      FormsVBT.PutText(fv, "playbackBtnText", "Stop Playback");
      FormsVBT.MakeDormant(fv, "recordBtn");
      FormsVBT.MakeActive(fv, "playbackBtn");
    END;
    IF scripting = ScriptingState.Recording THEN
      FormsVBT.MakeActive(fv, "futurePause");
      FormsVBT.MakeActive(fv, "grabData");
    ELSE
      FormsVBT.MakeDormant(fv, "futurePause");
      FormsVBT.MakeDormant(fv, "grabData");
    END;
    FormsVBT.PopDown(fv, "RecordDialog");
    FormsVBT.PopDown(fv, "PlaybackDialog");
  END ChangeScriptingState;

PROCEDURE ActivateScriptButtons(panel: T) =
  <* LL = VBT.mu *>
  BEGIN
    IF scripting = ScriptingState.Off THEN
      WITH fv = panel.fv DO
        IF stateIdle[panel.runState] THEN
          FormsVBT.MakeActive(fv, "playbackBtn");
          FormsVBT.MakeActive(fv, "recordBtn");
        ELSE
          FormsVBT.MakeDormant(fv, "playbackBtn");
          FormsVBT.MakeDormant(fv, "recordBtn");
        END;
      END;
    ELSIF scripting = ScriptingState.Recording THEN
      WITH fv = panel.fv DO
        IF stateIdle[panel.runState] THEN
          FormsVBT.MakeDormant(fv, "futurePause");
          FormsVBT.MakeDormant(fv, "grabData");
        ELSE
          FormsVBT.MakeActive(fv, "futurePause");
          FormsVBT.MakeActive(fv, "grabData");
        END;
      END;
    END;
  END ActivateScriptButtons;


(* **************** Utilities **************** *)

PROCEDURE Resolve (v: ZeusClass.T): T =
  (* LL = arbitrary *)
  (* This should never be called with any argument but NIL.  Probably
     should go away soon. *)
  BEGIN
    IF v = NIL THEN
      RETURN ControlPanel;
    ELSE
      <* ASSERT FALSE *>
(*      RETURN NARROW(VBT.GetProp(v, TYPECODE(T)), T);*)
    END;
  END Resolve;

<*UNUSED*> PROCEDURE Bound (val: INTEGER; min, max: INTEGER): INTEGER =
  BEGIN
    RETURN MAX(min, MIN(val, max))
  END Bound;


PROCEDURE TextEditVBTAppend (v: TextEditVBT.T; text: TEXT) =
  (* LL = VBT.mu *)
  BEGIN
    TextPort.PutText(v.tp, text);
  END TextEditVBTAppend;


PROCEDURE TextEditVBTClear (v: TextEditVBT.T) =
  BEGIN
    TextPort.SetText(v.tp, "")
  END TextEditVBTClear;


PROCEDURE InsertToBrowser (tp: ListVBT.T; name: TEXT) =
  (* LL = VBT.mu *)
  VAR len := tp.count();
  BEGIN
    FOR i := 0 TO len - 1 DO
      IF Text.Compare(name, tp.getValue(i)) = -1 THEN
        tp.insertCells(i, 1);
        tp.setValue(i, name);
        RETURN;
      END;
    END;
    tp.insertCells(len, 1);
    tp.setValue(len, name);
  END InsertToBrowser;

PROCEDURE DeleteFromBrowser (tp: ListVBT.T; name: TEXT) =
  (* LL = VBT.mu *)
  BEGIN
    FOR i := 0 TO tp.count() - 1 DO
      IF Text.Equal(name, tp.getValue(i)) THEN
        tp.removeCells(i, 1);
        RETURN;
      END;
    END;
  END DeleteFromBrowser;

PROCEDURE SelectInBrowser (tp: ListVBT.T; name: TEXT) =
  (* LL = VBT.mu *)
  BEGIN
    FOR i := 0 TO tp.count() DO
      IF Text.Equal(name, tp.getValue(i)) THEN
        tp.selectOnly(i);
        RETURN;
      END;
    END;
  END SelectInBrowser;

PROCEDURE RenameTrestleChassis (v: VBT.T; title: TEXT) =
  (* LL = VBT.mu *)
  BEGIN
    Trestle.Decorate(v, NIL, title);
  END RenameTrestleChassis;

PROCEDURE MoveNear (u, v: VBT.T) =
  (* LL = VBT.mu *)
  (* Replace Trestle.MoveNear(u, v).  No, revert to Trestle-style. *)
  BEGIN
    Trestle.MoveNear(u, v);
(*
    WITH dom = VBT.Domain(v),
         ne  = Trestle.ScreenOf(v, Rect.NorthEast(dom)) DO
      IF (ne.trsl # NIL) AND (ne.id # Trestle.NoScreen) THEN
        Trestle.Overlap(
          u, ne.id, Point.Add(ne.q, Point.FromCoords(-10, 30)));
      ELSE
        Trestle.MoveNear(u, v);
      END;
    END;
*)
  END MoveNear;

PROCEDURE CheckPrefix (t, pref: TEXT; VAR (*OUT*) res: TEXT): BOOLEAN =
  (* LL = arbitrary *)
  (* If pref is a prefix of t, set res := the suffix of t and return TRUE;
     else return FALSE, with res unspecified. *)
  VAR len := Text.Length(pref);
  BEGIN
    IF Text.Equal(pref, Text.Sub(t, 0, len)) THEN
      res := Text.Sub(t, len, LAST(CARDINAL));
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END CheckPrefix;

PROCEDURE SnapshotToList (): REFANY =
  VAR sx: REFANY;
  BEGIN
    WITH twr = TextWr.New() DO
      ZeusSnapshot.SnapshotToWr(Resolve(NIL), twr);
      TRY
        sx := Sx.Read(TextRd.New(TextWr.ToText(twr)))
      EXCEPT
        Rd.EndOfFile, Sx.ReadError =>
      END;
      RETURN sx;
    END;
  END SnapshotToList;

(* **************** Debugging **************** *)

VAR debugWr := TextWr.New();
    debugMu := NEW(MUTEX);
    debugP := FALSE;

<*UNUSED*>
PROCEDURE DebugWrite(t: TEXT) =
  BEGIN
    LOCK debugMu DO Wr.PutText(debugWr, t); END;
  END DebugWrite;

<*UNUSED*>
PROCEDURE DebugStart() =
  BEGIN
    LOCK debugMu DO debugP := TRUE; END;
  END DebugStart;

<*UNUSED*>
PROCEDURE DebugFinish() =
  BEGIN
    LOCK debugMu DO
      debugP := FALSE;
      Wr.PutText(Stdio.stderr, TextWr.ToText(debugWr));
    END;
  END DebugFinish;

(* **************** Mainline **************** *)

BEGIN
  LOCK VBT.mu DO ControlPanel := NewPanel(); END;
END ZeusPanel.
