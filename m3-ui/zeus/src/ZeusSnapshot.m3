(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* Last modified on Thu Feb  2 09:33:20 PST 1995 by kalsow   *)
(*      modified on Thu Oct 21 14:15:09 PDT 1993 by mhb      *)
(*      modified on Fri Jul  9 16:36:47 PDT 1993 by steveg   *)
(*      modified on Wed Feb 17 17:45:49 PST 1993 by johnh    *)
(*      modified on Fri Aug  7 21:45:26 PDT 1992 by meehan   *)
(*      modified on Fri Jul 31  5:03:25 PDT 1992 by sclafani *)
(*      modified on Wed Jul  1 10:09:55 PDT 1992 by tt       *)

<* PRAGMA LL *>

MODULE ZeusSnapshot;

(* A snapshot is an S-expression, written out by hand but normally read in
   using the Sx package.  Restore methods take an Rd.T as an argument.  A
   snapshot method writes its own data, then calls the snapshot method of
   its supertype.  A restore method reads its own data from the reader,
   then calls its supertype's restore method on the remaining list.
   Snapshot and restore methods must be exact inverses. *)

(* Some restore methods may find it convenient to read the input from the
   Rd.T into a List.T using Sx.Read.  This technique is used extensively
   below. *)


IMPORT Algorithm, AlgorithmClass, Atom, Classes, Env, Fmt, File,
       FileRd, FileWr, FormsVBT, FS, RefList, OSError,
       Pathname, Point, Rd, Rect, RefListUtils, RegularFile,
       StableVBT, Sx, Text, TextRd, TextWr, Thread, Trestle,
       TrestleComm, VBT, View, ViewClass, Wr, Zeus, ZeusClass,
       ZeusPanel, ZeusPanelFriends, ZeusPanelPrivate,
       ZeusPrivate, ZeusUtil;
IMPORT Process, Stdio;

<*FATAL FormsVBT.Error, FormsVBT.Unimplemented, 
        TrestleComm.Failure, Thread.Alerted, Wr.Failure *>



(* Convenience procedures for ZeusPanel init and exit. *)

PROCEDURE FinalSnapshot (panel : ZeusPanelPrivate.T) RAISES {} =
  <* LL = VBT.mu *>
  (* Snapshot to default file at Zeus exit. *)
  BEGIN
    Snapshot(panel, StateDirFile(FinalState), FALSE);
  END FinalSnapshot;

PROCEDURE InitialRestore (panel : ZeusPanelPrivate.T) RAISES {} =
  <* LL = VBT.mu *>
  (* Restore from default file at Zeus startup. *)
  BEGIN
    Restore(panel, StateDirFile(FinalState), FALSE);
  END InitialRestore;


(* ************ Session Snapshot / Restore ************ *)

(* Snapshot and restore sessions to the StateDir directory *)

PROCEDURE SessionToStateDir (sess  : ZeusPanelPrivate.Session;
                             report: BOOLEAN                    := TRUE)
  RAISES {} =
  VAR
    twr         := TextWr.New();
    fname       := StateDirFile(sess.name);
    wr   : Wr.T;
  BEGIN                          (* LL = VBT.mu *)
    TRY
      SessionToWr(sess, twr);
      wr := FileWr.Open(fname);
      Sx.Print(wr, Sx.Read(TextRd.New(TextWr.ToText(twr))));
      Wr.PutText(wr, "\n");
      Wr.Close(wr);
    EXCEPT
    | Rd.EndOfFile, Sx.ReadError, Sx.PrintError =>
        ZeusPanel.ReportErrorC(report, "Trouble with Sx in snapshot");
    | Wr.Failure, OSError.E =>
        ZeusPanel.ReportErrorC(report, "Cannot open file: " & fname);
    | FormsVBT.Error (msg) => ZeusPanel.ReportErrorC(report, msg);
    | ZeusClass.Error (msg) => ZeusPanel.ReportErrorC(report, msg);
    | Thread.Alerted =>
        ZeusPanel.ReportErrorC(
          report, "Snapshort alerted; incompletely recorded");
    END;
  END SessionToStateDir;

PROCEDURE SessionFromStateDir (panel : ZeusPanelPrivate.T;
                               name  : TEXT;
                               report: BOOLEAN              := TRUE):
  BOOLEAN RAISES {} =
  (* Return TRUE if successful *)
  VAR
    fname         := StateDirFile(name);
    rd   : Rd.T;
    list : RefList.T;
    msg  : TEXT;
  BEGIN                          (* LL = VBT.mu *)
    TRY
      rd := FileRd.Open(fname);
      list := Sx.Read(rd);
    EXCEPT
    | OSError.E =>
        (* no such file or directory *)
        RETURN FALSE;
    | Rd.EndOfFile =>
        ZeusPanel.ReportErrorC(
          report, "Unexpected end of file in " & fname);
        RETURN FALSE;
    | Sx.ReadError (msg) =>
        ZeusPanel.ReportErrorC(
          report, "Syntax error in " & fname & ": " & msg);
        RETURN FALSE;
    | Thread.Alerted =>
        ZeusPanel.ReportErrorC(report, "Alerted while reading " & fname);
        RETURN FALSE;
    END;
    TRY
      RestoreSession(panel, list, FALSE);
      RETURN TRUE;
    EXCEPT
    | BadSnapshot (err) => msg := err;
    | FormsVBT.Error (err) => msg := err;
    | ZeusClass.Error (err) => msg := err;
    | Thread.Alerted => msg := "interrupted";
    END;
    ZeusPanel.ReportErrorC(
      report, "Problems restoring file: " & fname & " - " & msg);
    RETURN FALSE;
  END SessionFromStateDir;


(* **************** Snapshot / Restore **************** *)

PROCEDURE Snapshot (panel: ZeusPanelPrivate.T;
                    file: TEXT;
                    report: BOOLEAN := TRUE)
  RAISES {} =
  (* LL = VBT.mu *)
  VAR
    wr : Wr.T;
  BEGIN
    TRY
      wr := FileWr.Open(file);
      SnapshotToWr(panel, wr, report);
      Wr.Close(wr);
      FormsVBT.PopDown(panel.fv, "SnapshotDialog");
    EXCEPT
    | Wr.Failure, OSError.E (*(ec)*) =>
        ZeusPanel.ReportErrorC(report, "Cannot open file: " & file
          (* & " (" & OS.errMessage[ec] & ")"*));
    END;
  END Snapshot;


PROCEDURE SnapshotToWr (panel: ZeusPanelPrivate.T;
                        wr: Wr.T;
                        report: BOOLEAN := TRUE)
  RAISES {} =
  (* LL = VBT.mu *)
  VAR
    twr       := TextWr.New();
  BEGIN
    TRY
      ZeusPanelPrivate.PrepForSnapshot(panel);
      SnapshotWr(panel, twr);
      Sx.Print(wr, Sx.Read(TextRd.New(TextWr.ToText(twr))));
      Wr.PutText(wr, "\n");
    EXCEPT
    | Rd.EndOfFile, Sx.ReadError, Sx.PrintError =>
        ZeusPanel.ReportErrorC(report, "Trouble with Sx in snapshot");
    | FormsVBT.Error (msg) => ZeusPanel.ReportErrorC(report, msg);
    | ZeusClass.Error (msg) => ZeusPanel.ReportErrorC(report, msg);
    | Thread.Alerted =>
        ZeusPanel.ReportErrorC(
            report, "Snapshort alerted; incompletely recorded");
    END;
  END SnapshotToWr;

PROCEDURE SnapshotWr (panel: ZeusPanelPrivate.T; wr: Wr.T)
  RAISES {FormsVBT.Error, Thread.Alerted, ZeusClass.Error} =
  (* LL = VBT.mu *)
  VAR
    l := RefList.Reverse(panel.sessions); (* reverse so order is same after
                                          restoration *)
  BEGIN
    Wr.PutText(wr, "(");
    panel.fv.snapshot(wr);
    WHILE l # NIL DO SessionToWr(RefListUtils.Pop(l), wr); END;
    Wr.PutText(wr, ")\n");
  END SnapshotWr;

PROCEDURE SessionToWr (sess: ZeusPanelPrivate.Session; wr: Wr.T)
  RAISES {FormsVBT.Error, Thread.Alerted, ZeusClass.Error} =
  (* LL = VBT.mu *)
  VAR
    dom := VBT.Domain(sess.fv);
    nw  := Trestle.ScreenOf(sess.fv, Rect.NorthWest(dom));
    se  := Trestle.ScreenOf(sess.fv, Rect.SouthEast(dom));
    vlist, addlist: RefList.T;
  BEGIN
    Wr.PutText(wr, "(");
    Wr.PutText(
      wr, "(InTrestle " & Fmt.Bool(sess.inTrestle) & ")\n");
    Wr.PutText(wr, "(Session \"" & sess.name & "\")\n");
    Wr.PutText(
      wr, "(ScreenPos " & Fmt.Int(nw.id) & " " & Fmt.Int(nw.q.h)
            & " " & Fmt.Int(nw.q.v) & " " & Fmt.Int(se.q.h) & " "
            & Fmt.Int(se.q.v) & ")\n");
    Wr.PutText(wr, "(FV ");
    sess.fv.snapshot(wr);
    Wr.PutText(wr, ")\n");
    AlgToWr(wr, sess.alg);
    Wr.PutText(wr, "(");
    Zeus.Acquire(sess);
    vlist := sess.views;
    addlist := sess.viewsToAdd;
    WHILE addlist # NIL DO
      WITH v = RefListUtils.Pop(addlist) DO
        IF NOT RefList.Member(vlist, v) THEN RefListUtils.Push(vlist, v); END;
      END;
    END;
    ViewsToWr(wr, sess, vlist);
    Zeus.Release(sess);
    Wr.PutText(wr, ")");
    Wr.PutText(wr, ")\n");
  END SessionToWr;

PROCEDURE AlgToWr (wr: Wr.T; alg: Algorithm.T)
  RAISES {Thread.Alerted, ZeusClass.Error} =
  (* LL = VBT.mu *)
  BEGIN
    Wr.PutText(wr, "(");
    IF (alg # NIL) AND NOT Text.Equal(alg.name, "") THEN
      Wr.PutText(wr, "(Alg \"" & alg.name & "\")\n");
      alg.snapshot(wr);
    ELSE
      Wr.PutText(wr, "(Alg \"NIL\")\n");
    END;
    Wr.PutText(wr, ")\n");
  END AlgToWr;

PROCEDURE ViewsToWr (           wr   : Wr.T;
                     <*UNUSED*> sess : ZeusPanelPrivate.Session;
                                views: RefList.T (* of View.T *))
  RAISES {ZeusClass.Error} =
  (* LL = VBT.mu *)
  VAR
    rest: RefList.T;
    view: View.T;
    scr : Trestle.ScreenOfRec;
  BEGIN
    rest := views;
    WHILE rest # NIL DO
      view := NARROW(rest.head, View.T);
      scr := Trestle.ScreenOf(view, Point.Origin);
      (* this test wouldn't be needed if deleting views got rid of them: *)
      IF scr.id # Trestle.NoScreen THEN
        Wr.PutText(wr, "(");
        Wr.PutText(wr, "(View \"" & view.name & "\")\n");
        view.snapshot(wr);
        Wr.PutText(wr, ")\n");
      END;
      rest := rest.tail;
    END;
  END ViewsToWr;

 
EXCEPTION
  BadSnapshot( TEXT );

PROCEDURE Restore (panel : ZeusPanelPrivate.T;
                   file  : TEXT;
                   report: BOOLEAN              := TRUE) RAISES {} =
  (* LL = VBT.mu *)
  VAR rd: Rd.T;
  BEGIN
    TRY
      rd := FileRd.Open(file);
    EXCEPT
    | OSError.E =>
        ZeusPanel.ReportErrorC(report, "Cannot open file: " & file);
        RETURN;
    END;
    RestoreFromRd(panel, rd, report, file);
    FormsVBT.PopDown(panel.fv, "RestoreDialog");
  END Restore;

PROCEDURE RestoreFromRd (panel : ZeusPanelPrivate.T;
                         rd    : Rd.T;
                         report: BOOLEAN              := TRUE;
                         file  : TEXT                 := NIL   )
  RAISES {} =
  <* LL = VBT.mu *>
  (* Restore(p, file) == RestoreFromRd(p, FileRd.Open(file)) *)
  VAR
    list: RefList.T;
    ra  : REFANY;
  BEGIN
    TRY
      ra := Sx.Read(rd);
      IF ISTYPE(ra, RefList.T) THEN
        list := ra
      ELSE
        RAISE Sx.ReadError("File does not contain a RefList.T")
      END;
    EXCEPT
    | Rd.EndOfFile =>
        ZeusPanel.ReportErrorC(report, "Unexpected end of file in " & file);
        RETURN;
    | Sx.ReadError (msg) =>
        ZeusPanel.ReportErrorC(
          report, "Syntax error in " & file & ": " & msg);
        RETURN;
    | Thread.Alerted =>
        ZeusPanel.ReportErrorC(report, "Alerted while reading " & file);
        RETURN;
    END;
    RestoreFromList(panel, list, report);
  END RestoreFromRd;


PROCEDURE RestoreFromList (panel: ZeusPanelPrivate.T;
                           list: RefList.T;
                           report: BOOLEAN := TRUE)
    RAISES {} =
  VAR msg: TEXT;
  BEGIN
    ZeusPanelPrivate.DestroyAllSessions(panel);
    TRY
      PrivateRestoreFromList(panel, list);
      ZeusPanelPrivate.OverrideRestore(panel);
      RETURN;
    EXCEPT
    | BadSnapshot (err) => msg := err;
    | FormsVBT.Error (err) => msg := err;
    | ZeusClass.Error (err) => msg := err;
    | Thread.Alerted => msg := "interrupted";
    END;
    ZeusPanel.ReportErrorC(
        report, "Problems restoring from snapshot: " & msg);
    ZeusPanelPrivate.DestroyAllSessions(panel);
  END RestoreFromList;


PROCEDURE PrivateRestoreFromList (panel: ZeusPanelPrivate.T; list: RefList.T)
  RAISES {BadSnapshot, FormsVBT.Error, Thread.Alerted, ZeusClass.Error} =
  (* LL = VBT.mu *)
  VAR
    l      : RefList.T;
    wr: TextWr.T;
  BEGIN
    IF list = NIL THEN RAISE BadSnapshot("Snapshot list is empty") END;
    l := RefListUtils.Pop(list); (* Snapshot brackets w/ parens *)
    TRY
      wr := TextWr.New();
      Sx.Print(wr, l);
      panel.fv.restore(TextRd.New(TextWr.ToText(wr)));
    EXCEPT
      Sx.PrintError, FormsVBT.Mismatch => 
    END;
    ZeusPanelPrivate.LoadFromPanel(panel);
    
    WHILE (list # NIL) DO
      IF (NOT ISTYPE(list.head, RefList.T)) OR (list.head = NIL) THEN
        RAISE BadSnapshot("Not a valid snapshot");
      END;
      l := RefListUtils.Pop(list);
      TYPECASE l.head OF
      | RefList.T => RestoreSession(panel, l, TRUE);
      ELSE
        RAISE BadSnapshot("Not a valid snapshot");
      END;
    END;
  END PrivateRestoreFromList;

PROCEDURE RestoreSession (panel     : ZeusPanelPrivate.T;
                          list      : RefList.T;
                          restoreIT : BOOLEAN)
  RAISES {BadSnapshot, FormsVBT.Error, Thread.Alerted, ZeusClass.Error} =
  (* LL = VBT.mu *)
  (* If restoreIT, put the session where it wants to be; o/w, put it in
     Trestle or not as dictated by the panel. *)
  VAR
    sess   : ZeusPanelPrivate.Session;
    bool   : BOOLEAN;
    l      : RefList.T;
    keyword: TEXT;
  BEGIN
    bool := FormsVBT.GetBoolean(panel.fv, "inTrestle");
    TRY
      WHILE (list # NIL) DO
        IF NOT ISTYPE(list.head, RefList.T) THEN
          RAISE BadSnapshot("Invalid session snapshot");
        END;
        l := RefListUtils.Pop(list);
        IF l # NIL THEN
          TYPECASE l.head OF
          | Atom.T (sxs) =>
              keyword := Atom.ToText(sxs);
              IF Text.Equal(keyword, "InTrestle") THEN
                IF restoreIT THEN bool := GetSessInTrestle(l) END;
              ELSIF Text.Equal(keyword, "Session") THEN
                sess := GetSession(panel, l, bool);
              ELSIF Text.Equal(keyword, "ScreenPos") THEN
                IF (sess # NIL) AND sess.inTrestle THEN
                  GetSessPosition(panel, sess, l);
                END;
              ELSIF Text.Equal(keyword, "FV") THEN
                IF sess # NIL THEN GetSessFV(sess, l) END;
              ELSE
                RAISE BadSnapshot("Unknown keyword");
              END;
          | RefList.T (lfirst) =>
              IF sess # NIL THEN
                TYPECASE lfirst.head OF
                | Atom.T => GetAlg(sess, l);
                | RefList.T => GetViews(sess, l);
                ELSE
                  RAISE BadSnapshot("Invalid session snapshot");
                END;
              END;
          ELSE
            RAISE BadSnapshot("Invalid session snapshot");
          END;
        END;
      END;
    EXCEPT
    | BadSnapshot (msg) =>
        IF sess # NIL THEN ZeusPanelPrivate.DestroySession(sess); END;
        RAISE BadSnapshot(msg);
    END;
  END RestoreSession;

PROCEDURE GetSession (panel: ZeusPanelPrivate.T; arg: REFANY; inTrestle: BOOLEAN): ZeusPanelPrivate.Session
  RAISES {BadSnapshot} =
  (* LL = VBT.mu *)
  VAR sess: ZeusPanelPrivate.Session;
  BEGIN
    KeywordCheck(arg, "Session");
    IF ISTYPE(arg, RefList.T) AND (RefList.Length(arg) = 2)
         AND ISTYPE(RefList.Nth(arg, 1), TEXT)
         AND ZeusPanelPrivate.GroupInfoExists(RefList.Nth(arg, 1)) THEN
      ZeusPanelPrivate.NewSession(RefList.Nth(arg, 1), panel, inTrestle, FALSE);
      sess := panel.sessions.head;
    ELSE
      RAISE BadSnapshot("Garbled session name");
    END;
    RETURN sess;
  END GetSession;

PROCEDURE GetSessInTrestle (arg: REFANY): BOOLEAN RAISES {BadSnapshot} =
  (* LL = VBT.mu *)
  BEGIN
    KeywordCheck(arg, "InTrestle");
    IF ISTYPE(arg, RefList.T) AND (RefList.Length(arg) = 2)
         AND ISTYPE(RefList.Nth(arg, 1), Atom.T) THEN
      IF RefList.Nth(arg, 1) = Sx.True THEN
        RETURN TRUE
      ELSIF RefList.Nth(arg, 1) = Sx.False THEN
        RETURN FALSE
      ELSE
        RAISE BadSnapshot("Garbled inTrestle parameter");
      END;
    ELSE
      RAISE BadSnapshot("Garbled inTrestle parameter");
    END;
  END GetSessInTrestle;

PROCEDURE GetSessPosition (panel: ZeusPanelPrivate.T; sess: ZeusPanelPrivate.Session; arg: REFANY)
  RAISES {BadSnapshot} =
  (* LL = VBT.mu *)
  VAR l: RefList.T;
  PROCEDURE NarrowToInt (r: REFANY): INTEGER RAISES {BadSnapshot} =
    BEGIN
      TYPECASE r OF
      | REF INTEGER (rint) => RETURN rint^;
      ELSE
        RAISE BadSnapshot("Integer arg expected in position");
      END;
    END NarrowToInt;
  BEGIN
    IF ISTYPE(arg, RefList.T) AND (RefList.Length(arg) = 6) THEN
      l := arg;
      KeywordCheck(l, "ScreenPos");
      SetSessPosition(
        panel, sess, NarrowToInt(RefList.Nth(l, 1)),
        NarrowToInt(RefList.Nth(l, 2)), NarrowToInt(RefList.Nth(l, 3)),
        NarrowToInt(RefList.Nth(l, 4)), NarrowToInt(RefList.Nth(l, 5)));
    ELSE
      RAISE BadSnapshot("Error in session position");
    END;
  END GetSessPosition;

PROCEDURE SetSessPosition (<*UNUSED*> panel: ZeusPanelPrivate.T;
                                      sess : ZeusPanelPrivate.Session;
                                      id   : INTEGER;
                           nwh, nwv, seh, sev: INTEGER) =
  (* LL = VBT.mu *)
  VAR
    nw := Point.FromCoords(nwh, nwv);
    se := Point.FromCoords(seh, sev);
    v  := sess.fv;
  BEGIN
    DEC(nw.h, ZeusPanelFriends.XDRIFT);
    DEC(nw.v, ZeusPanelFriends.YDRIFT);
    DEC(se.h, ZeusPanelFriends.XDRIFT);
    DEC(se.v, ZeusPanelFriends.YDRIFT);
    IF ZeusUtil.ScreenPosOK(id, nw) THEN
      StableVBT.SetShape(v, ABS(se.h - nw.h), ABS(se.v - nw.v));
      Trestle.Overlap(v, id, nw);
    ELSE
      (* leave alone; already installed *)
    END
  END SetSessPosition;

PROCEDURE GetSessFV (sess: ZeusPanelPrivate.Session; arg: REFANY)
  RAISES {BadSnapshot} =
  (* LL = VBT.mu *)
  VAR wr: TextWr.T;
  BEGIN
    KeywordCheck(arg, "FV");
    IF ISTYPE(arg, RefList.T) AND (RefList.Length(arg) = 2)
         AND ISTYPE(RefList.Nth(arg, 1), RefList.T) THEN
      TRY
        wr := TextWr.New();
        Sx.Print(wr, RefList.Nth(arg, 1));
        sess.fv.restore(TextRd.New(TextWr.ToText(wr)));
      EXCEPT
      | FormsVBT.Mismatch =>
      | Sx.PrintError, FormsVBT.Error =>
          RAISE BadSnapshot("Bad session FV snapshot");
      END;
    ELSE
      RAISE BadSnapshot("Bad session FV snapshot");
    END;
  END GetSessFV;

PROCEDURE GetAlg (sess: ZeusPanelPrivate.Session; arg: REFANY; new := TRUE)
  RAISES {BadSnapshot, Thread.Alerted, ZeusClass.Error} =
  (* LL = VBT.mu *)
  (* IF new THEN create a new alg object, ELSE re-use the old one. *)
  VAR list, l: RefList.T; wr: TextWr.T;
  BEGIN
    IF (arg # NIL) AND ISTYPE(arg, RefList.T) THEN
      list := arg;
    ELSE
      RAISE BadSnapshot("Bad alg snapshot");
    END;
    l := RefListUtils.Pop(list);
    IF (NOT ISTYPE(l, RefList.T)) OR (RefList.Length(l) # 2) THEN
      RAISE BadSnapshot("Bad alg snapshot");
    END;
    KeywordCheck(l, "Alg");
    TYPECASE l.tail.head OF
    | TEXT (text) =>
        TRY
          EVAL Classes.FindAlg(text);
        EXCEPT
          Classes.NotFound =>
            IF Text.Equal(text, "NIL") THEN
              RETURN
            ELSE
              RAISE BadSnapshot("Invalid alg name");
            END;
        END;
        IF new THEN ZeusPanelPrivate.PickedAlg(sess, text); END;
        TRY
          wr := TextWr.New();
          Sx.Print(wr, list.head);
          sess.alg.restore(TextRd.New(TextWr.ToText(wr)));
        EXCEPT
          Sx.PrintError => RAISE BadSnapshot("GetAlg error")
        END;
    ELSE
      RAISE BadSnapshot("Alg named not a string");
    END;
  END GetAlg;

PROCEDURE GetViews (sess: ZeusPanelPrivate.Session; arg: REFANY)
  RAISES {BadSnapshot, ZeusClass.Error} =
  (* LL = VBT.mu *)
  VAR list: RefList.T;
  BEGIN
    IF NOT ISTYPE(arg, RefList.T) THEN RAISE BadSnapshot("Bad views") END;
    list := arg;
    WHILE list # NIL DO GetView(sess, RefListUtils.Pop(list)); END;
  END GetViews;

PROCEDURE GetView (sess: ZeusPanelPrivate.Session; arg: REFANY)
  RAISES {BadSnapshot, ZeusClass.Error} =
  (* LL = VBT.mu *)
  VAR
    list, l: RefList.T;
    view   : View.T;
    discard: TEXT;
    wr: TextWr.T;
  BEGIN
    IF (arg = NIL) OR (NOT ISTYPE(arg, RefList.T)) THEN
      RAISE BadSnapshot("Bad view snapshot")
    END;
    list := arg;
    l := RefListUtils.Pop(list);
    IF (NOT ISTYPE(l, RefList.T)) OR (RefList.Length(l) # 2) THEN
      RAISE BadSnapshot("Bad view snapshot");
    END;
    KeywordCheck(l, "View");
    TYPECASE l.tail.head OF
    | TEXT (text) =>
        TRY
          EVAL Classes.FindView(text);
        EXCEPT
          Classes.NotFound =>
            IF NOT ZeusPanelPrivate.IsCodeView(text, sess, discard) THEN
              RETURN;
              (* Be more forgiving; ignore invalid view names, to allow
                 code views that don't belong to the current algorithm. *)
              (* RAISE BadSnapshot("Invalid view name");*)
            END;
        END;
        view := ZeusPanelPrivate.PickedView(sess, text);
        TRY
          wr := TextWr.New();
          Sx.Print(wr, list.head);
          view.restore(TextRd.New(TextWr.ToText(wr)));
        EXCEPT
          Sx.PrintError => RAISE BadSnapshot("GetView error")
        END;
    ELSE
      RAISE BadSnapshot("View named not a string");
    END;
  END GetView;


(* **************** Data-only Snapshot / Restore **************** *)

(* Procedures to save and restore panel and algorithm data, ignoring
   views completely, under the assumption that the sessions at the time
   of snapshot can be mapped to those in panel.sessions at the time of
   restoration, in the same order. *)

PROCEDURE GrabDataList(panel: ZeusPanelPrivate.T): REFANY =
  <* LL = VBT.mu *>
  VAR l := panel.sessions;
  BEGIN
    TRY
      WITH twr = TextWr.New() DO
        Wr.PutText(twr, "(");
        panel.fv.snapshot(twr);
        WHILE l # NIL DO
          WITH sess = NARROW(RefListUtils.Pop(l), ZeusPanelPrivate.Session) DO
            Wr.PutText(twr, "(");
            Wr.PutText(twr, "(Session \"" & sess.name & "\")\n");
            Wr.PutText(twr, "(FV ");
            sess.fv.snapshot(twr);
            Wr.PutText(twr, ")\n");
            AlgToWr(twr, sess.alg);
            Wr.PutText(twr, ")\n");
          END;
        END;
        Wr.PutText(twr, ")\n");
        RETURN Sx.Read(TextRd.New(TextWr.ToText(twr)));
      END;
    EXCEPT
    | ZeusClass.Error, Rd.EndOfFile, Sx.ReadError =>
        ZeusPanel.ReportError("Trouble writing data snapshot");
        RETURN NIL;
    END;
  END GrabDataList;

PROCEDURE RestoreData(panel: ZeusPanelPrivate.T; list: RefList.T) =
  <* LL = VBT.mu *>
  VAR l, sesslist: RefList.T;
      sess: ZeusPanelPrivate.Session; wr: TextWr.T;
  BEGIN
    TRY
      IF (list # NIL) AND ISTYPE(list.head, RefList.T) THEN
        l := RefListUtils.Pop(list);
      ELSE
        RAISE BadSnapshot("Bad format (1)");
      END;
      TRY
        wr := TextWr.New();
        Sx.Print(wr, l);
        panel.fv.restore(TextRd.New(TextWr.ToText(wr)));
      EXCEPT
        Sx.PrintError, FormsVBT.Mismatch => 
          RAISE BadSnapshot("Bad format (2)");
      END;
      ZeusPanelPrivate.LoadFromPanel(panel);
      sesslist := panel.sessions;
      WHILE (list # NIL) AND (sesslist # NIL) DO
        sess := RefListUtils.Pop(sesslist);
        l := RefListUtils.Pop(list);
        IF ISTYPE(l, RefList.T) THEN
          WHILE l # NIL DO
            WITH lfirst = RefListUtils.Pop(l) DO
              IF (lfirst # NIL) AND ISTYPE(lfirst, RefList.T) THEN
                TYPECASE NARROW(lfirst, RefList.T).head OF
                | Atom.T (sxs) =>
                    IF Text.Equal(Atom.ToText(sxs), "Session") THEN
                      IF NOT Text.Equal(RefList.Nth(lfirst, 1), sess.name) THEN
                        RAISE BadSnapshot("Session name mismatch");
                      END;
                    ELSIF Text.Equal(Atom.ToText(sxs), "FV") THEN
                      GetSessFV(sess, lfirst);
                    ELSE
                      RAISE BadSnapshot("Unknown keyword");
                    END;
                | RefList.T (ll) =>
                    IF ISTYPE(ll.head, Atom.T) THEN
                      GetAlg(sess, lfirst, FALSE);
                    ELSE
                      RAISE BadSnapshot("Bad format (3)");
                    END;
                ELSE RAISE BadSnapshot("Bad format (4)");
                END (* TYPECASE *);
              ELSE
                RAISE BadSnapshot("Bad format (5)");
              END;
            END (* WITH *);
          END (* WHILE *);   (* for each session sublist *)
        ELSE
          RAISE BadSnapshot("Bad format (6)");
        END;
      END (* WHILE *);    (* for each session *)
      IF (list # NIL) OR (sesslist # NIL) THEN
        RAISE BadSnapshot("Wrong number of sessions");
      END;
    EXCEPT
      BadSnapshot(msg) =>
        ZeusPanel.ReportError("Data snapshot error: " & msg);
    ELSE
        ZeusPanel.ReportError("Data snapshot error");
    END (* TRY *);
  END RestoreData;


(*********** Utilities ***********)

PROCEDURE KeywordCheck (arg: REFANY; t: TEXT) RAISES {BadSnapshot} =
  (* LL = arbitrary *)
  BEGIN
    TRY
      ZeusUtil.KeywordCheck(arg, t);
    EXCEPT
      ZeusUtil.BadSnapshot (msg) => RAISE BadSnapshot(msg);
    END;
  END KeywordCheck;


PROCEDURE StateDirFile (file: TEXT): Pathname.T =
  <* LL = arbitrary *>
  <* FATAL Pathname.Invalid *>
  VAR home := Env.Get (HomeDir);
  BEGIN
    IF home = NIL THEN
      Wr.PutText (Stdio.stderr, 
        "Error: the HOME environment variable is undefined.\n");
      Wr.PutText (Stdio.stderr, 
        "Please set it to the path of your home directory and try again.\n");
      Process.Exit (0);
    END;
    WITH path = Pathname.Decompose (home) DO
      path.addhi(StateDir);
      MakeStateDir(Pathname.Compose(path));
      path.addhi(file);
      RETURN Pathname.Compose(path)
    END;
  END StateDirFile;

PROCEDURE MakeStateDir (path: Pathname.T) =
  <* LL = arbitrary *>
  VAR
    status: File.Status;
    exists: BOOLEAN := TRUE;
  BEGIN
    TRY
      TRY status := FS.Status(path) EXCEPT
        OSError.E => exists := FALSE;
      END;
      IF NOT exists OR NOT status.type = FS.DirectoryFileType THEN
        IF exists AND status.type = RegularFile.FileType THEN
          FS.DeleteFile(path)
        END;
        FS.CreateDirectory(path);
      END;
    EXCEPT
    | OSError.E => ZeusPanel.ReportError("Can't create " & path);
    END;
  END MakeStateDir;


(* **************** Mainline **************** *)

BEGIN
END ZeusSnapshot.
