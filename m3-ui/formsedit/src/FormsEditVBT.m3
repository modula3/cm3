(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri May 17 11:43:43 PDT 1996 by mhb                      *)
(*      modified on Tue Jan 31 11:27:14 PST 1995 by kalsow                   *)
(*      modified on Wed Jun 29 16:17:48 PDT 1994 by bharat                   *)
(*      modified on Fri Jun  4 16:08:13 PDT 1993 by meehan                   *)
(*      modified on Tue Jun 16 21:55:39 PDT 1992 by muller                   *)

MODULE FormsEditVBT;

IMPORT AnyEvent, Axis, Cursor, FileBrowserVBT, FileRd, FileWr, Filter,
       Formatter, formseditBundle, FormsVBT, Fmt, Font, FS, FVRuntime,
       FVTypes, IntRefTbl, IO, KeyboardKey, KeyTrans, Manpage, MText,
       MTextRd, OSError, PaintOp, Palette, Pathname, Point, Process, Rd,
       RdUtils, Rect, RefList, RefListUtils, Rsrc, RTTypeSRC, ScreenType,
       StableVBT, Sx, Text, TextEditVBT, TextPort, TextPortClass, TextRd,
       TextWr, Thread, Trestle, TrestleComm, VBT, VBTClass, VTDef, VText, Wr,
       XParam, ZChassisVBT, ZChildVBT;

<* FATAL FormsVBT.Unimplemented *>(* Should never happen here. *)

<* PRAGMA LL *>

CONST
  DummyText = "(Rim (Pen 10) (Text (Name ignoreMe) "
                & "\"This space available for a small fee\"))";
  HelpFile  = "formsedit.txt";
  STACKSIZE = 10000;

REVEAL
  FormsVBT.T <: FVRuntime.SemiPublic; (* expose "formstack" field *)
  T = Public BRANDED OBJECT
        ed                     : Editor;
        number                                   := 0;
        fullPathname, shortname: TEXT            := "";
        display, geometry      : TEXT;
        rd                     : Rd.T;           (* For manpage *)
        prettyprintWidth       : CARDINAL        := 78;
        revertWidth            : CARDINAL        := 78;
        root                   : EditorRoot;
        mu                     : MUTEX;
        egrec                  : XParam.Geometry;
        path                   : Rsrc.Path;
      METHODS
        delete   ()                              := DeleteFrame;
        decorate () RAISES {TrestleComm.Failure} := DecorateFrame;
        spawn    ()                              := Spawn
      OVERRIDES
        editor       := GetEditor;
        init         := Init;
        initFromFile := InitFromFile;
      END;
  EditorRoot = PublicRoot BRANDED OBJECT
                 firstFrame: T;
                 mu        : MUTEX;
                 allClosed : NamedCondition;
                 frames    : RefList.T              := NIL; (* children *)
                 thread    : Thread.T;           (* our own thread *)
                 display   : TEXT;
                 drec      : XParam.Display;
                 trsl      : Trestle.T;
                 array     : Trestle.ScreenArray
               OVERRIDES
                 apply := EditorRootApply;
                 init  := EditorRootInit
               END;

TYPE                             (* in alphabetical order *)

  Attachment = FormsVBT.Closure OBJECT
                 frame: T;
                 proc : KeyProc
               OVERRIDES
                 apply := AttachmentApply
               END;

  Editor = FormsVBT.T OBJECT
             (* The components to which we need fast access *)
             buffer    : TextEditVBT.T;
             stderr    : TextEditVBT.T;
             errorPopup: ZChassisVBT.T;
             (* The internals of the buffer *)
             textport: EPort;
             vtext   : VText.T;
             mtext   : MText.T;
             (* Other things *)
             syntax      : Sx.Syntax;
             rangeTable  : IntRefTbl.Default;
             highlighter : VText.Interval;
             frame       : T;
             modelTsplits: RefList.T        := NIL;
             rd          : MTextRd.T
           METHODS
             init (Frame: T): Editor RAISES {FormsVBT.Error} := EditorInit;
             <* LL = VBT.mu *>
             decorate () RAISES {TrestleComm.Failure} := DecorateEditor;
           OVERRIDES
             realize := Realize
           END;

  EPort = FVTypes.Port OBJECT
            ed: Editor
          OVERRIDES
            modified := NoteModification;
            filter   := EPortFilter;
            error    := EPortError;
            notFound := EPortNotFound
          END;

  FinderClosure = FormsVBT.Closure OBJECT
                    first, next, prev, typein, textedit, close: TEXT
                  METHODS
                    init (fv: FormsVBT.T): FinderClosure := InitFCL
                  OVERRIDES
                    apply := ShowFindWindow
                  END;

  FrameClosure =
    Thread.Closure OBJECT frame: T OVERRIDES apply := FrameApply END;

  JustFVfileBrowser = FVTypes.FVFileBrowser OBJECT
                        ed: Editor
                      OVERRIDES
                        init  := FBinit;
                        error := FBerror
                      END;

  KeyProc = PROCEDURE (frame: T; time: VBT.TimeStamp); <* LL = VBT.mu *>

  Mover = FormsVBT.Closure OBJECT
            id : CARDINAL;
            vbt: VBT.T
          OVERRIDES
            apply := MoverApply
          END;

  NamedCondition = Thread.Condition OBJECT name: TEXT END; (* debugging *)

  ParseClosure = Thread.SizedClosure OBJECT
                   frame: T;
                 OVERRIDES
                   apply := ParseClosureApply
                 END;

  ReadMacro = Sx.ReadMacro OBJECT ed: Editor OVERRIDES read := ReadList END;

VAR HighlightOptions: VText.IntervalOptions; (* CONST *)

VAR
  FrameCountLock := NEW (MUTEX);
  FrameCount     := 0;
  formseditPath  := Rsrc.BuildPath ("$formseditPATH", formseditBundle.Get ());

PROCEDURE Init (frame: T; description: TEXT): T RAISES {FormsVBT.Error} =
  BEGIN
    <* LL = VBT.mu *>
    IF description = NIL THEN description := DummyText END;
    frame.fullPathname := "";
    frame.shortname := "";
    frame.ed := NEW (Editor).init (frame);
    FormsVBT.PutText (frame.ed, "openfile", "");
    FormsVBT.PutText (frame.ed, "shortname", "");
    TextPort.SetText (frame.ed.buffer.tp, description);
    TextPort.SetModified (frame.ed.textport, FALSE);
    SetModified (frame.ed, FALSE);
    frame.path := RefList.List1 (".");
    Parse (frame);
    RETURN frame
  END Init;

PROCEDURE InitFromFile (frame: T; filename: TEXT): T
  RAISES {FormsVBT.Error, Thread.Alerted} =
  <* LL = VBT.mu *>
  BEGIN
    IF Text.Empty (filename) THEN RAISE FormsVBT.Error ("No filename.") END;
    IF NOT Pathname.Absolute (filename) THEN
      TRY
        filename := Pathname.Join (Process.GetWorkingDirectory (), filename, NIL)
      EXCEPT
      | OSError.E (list) =>
          RAISE FormsVBT.Error (
                  "Can't get current directory: " & RdUtils.FailureText (list))
      END
    END;
    TRY
      frame.fullPathname := FS.GetAbsolutePathname (filename)
    EXCEPT
    | OSError.E (list) => RAISE FormsVBT.Error (RdUtils.FailureText (list))
    END;
    frame.shortname := Pathname.Last (filename);
    frame.ed := NEW (Editor).init (frame);
    frame.path := NIL;
    Read (frame);
    FormsVBT.PutText (frame.ed, "openfile", filename);
    FormsVBT.PutText (frame.ed, "shortname", frame.shortname);
    RETURN frame
  END InitFromFile;

PROCEDURE Read (frame: T) RAISES {FormsVBT.Error, Thread.Alerted} =
  <* LL = VBT.mu *>
  VAR
    ed        := frame.ed;
    rd : Rd.T;
    dir: Pathname.T;
  BEGIN
    ClearError (ed);
    TRY
      rd := FileRd.Open (frame.fullPathname);
      TRY
        dir := Pathname.Prefix (frame.fullPathname);
        IF NOT RefList.Member (frame.path, dir) THEN
          frame.path := RefList.Cons (dir, frame.path)
        END;
        TextPort.SetText (ed.textport, Rd.GetText (rd, Rd.Length (rd)));
        TextPort.SetModified (ed.textport, FALSE);
        SetModified (ed, FALSE);
        Parse (frame);
        frame.decorate ();
        ed.decorate ()
      FINALLY
        Rd.Close (rd)
      END
    EXCEPT
    | OSError.E (list) => RAISE FormsVBT.Error (RdUtils.FailureText (list))
    | Rd.Failure (f) =>
        RAISE
          FormsVBT.Error (Fmt.F ("Could not read file %s : %s",
                                 frame.fullPathname, RdUtils.FailureText (f)))
    | TrestleComm.Failure =>
        RAISE
          FormsVBT.Error (
            "TrestleComm.Failure while attempting to change the decoration")
    END
  END Read;

PROCEDURE EditorRootInit (root     : EditorRoot;
                          frame    : T;
                          Xdisplay                := ":0.0";
                          Xgeometry               := "+50+50"): EditorRoot
  RAISES {TrestleComm.Failure, XParam.Error} =
  BEGIN
    root.firstFrame := frame;
    frame.root := root;
    root.display := Xdisplay;
    root.drec := XParam.ParseDisplay (Xdisplay);
    root.trsl := Trestle.Connect (Xdisplay);
    root.array := Trestle.GetScreens (root.trsl);
    IF root.array = NIL OR NUMBER (root.array^) = 0 THEN
      RAISE TrestleComm.Failure
    END;
    Palette.Init (root.array [0].type);
    Palette.Init (root.array [0].type.bits);
    Install (frame, Xgeometry);
    root.mu := NEW (MUTEX);
    LOCK root.mu DO
      root.frames := NIL;
      root.allClosed := NEW (NamedCondition, name := "all editors closed")
    END;
    RETURN root
  END EditorRootInit;

PROCEDURE EditorRootApply (root: EditorRoot): REFANY =
  VAR frames: RefList.T;
  BEGIN
    root.thread := Thread.Self ();
    root.firstFrame.spawn ();
    TRY
      LOCK root.mu DO
        WHILE root.frames # NIL DO
          Thread.AlertWait (root.mu, root.allClosed)
        END
      END
    EXCEPT
    | Thread.Alerted =>
        Debug (Fmt.F ("EdRoot was alerted. There are %s frames.\n",
                      Fmt.Int (RefList.Length (root.frames))));
        (* Alert all the frames *)
        LOCK root.mu DO frames := root.frames END;
        WHILE frames # NIL DO AlertFrame (RefListUtils.Pop (frames)) END;
        LOCK root.mu DO
          WHILE root.frames # NIL DO Thread.Wait (root.mu, root.allClosed) END
        END
    END;
    RETURN NIL
  END EditorRootApply;

PROCEDURE Spawn (frame: T) =
  VAR fc := NEW (FrameClosure, frame := frame);
  BEGIN
    frame.mu := NEW (MUTEX);
    EVAL Thread.Fork (fc);
    LOCK frame.root.mu DO RefListUtils.Push (frame.root.frames, frame) END
  END Spawn;

PROCEDURE FrameApply (fc: FrameClosure): REFANY =
  <* LL = 0 *>
  BEGIN
    Trestle.AwaitDelete (fc.frame);
    RETURN NIL
  END FrameApply;

PROCEDURE AlertFrame (frame: T) =
  <* LL = 0 *>
  <* FATAL FormsVBT.Error *>
  VAR ed := frame.ed;
  BEGIN
    Debug (Fmt.F ("Frame %s is being alerted.\n", Fmt.Int (frame.number)));
    LOCK VBT.mu DO
      IF TextPort.IsModified (ed.textport) THEN
        FormsVBT.MakeDormant (ed, "dontquit");
        FormsVBT.MakeDormant (ed, "cancelsaveas");
        FormsVBT.PopUp (ed, "quitConfirmation")
      ELSE
        frame.delete ()
      END
    END
  END AlertFrame;


PROCEDURE Install (frame: T; editorGeo: TEXT)
  RAISES {TrestleComm.Failure, XParam.Error} =
  <* FATAL FormsVBT.Error *>(* In here, they're all our fault. *)
  VAR
    frameGeo := "+10+10";        (* NW corner *)
    ed       := frame.ed;
    drec     := frame.root.drec;
    trsl     := frame.root.trsl;
    array    := frame.root.array;
  VAR
    egrec, fgrec: XParam.Geometry;
    name        : TEXT;
  BEGIN
    frame.geometry := editorGeo;
    egrec := XParam.ParseGeometry (editorGeo);
    frame.egrec := egrec;
    fgrec := XParam.ParseGeometry (frameGeo);
    (* Set up Rescreen menu-items. *)
    IF NUMBER (array^) = 1 THEN
      FormsVBT.MakeDormant (ed, "rescreenFilter")
    ELSE
      FOR i := LAST (array^) TO FIRST (array^) BY -1 DO
        name := "Edit" & Fmt.Int (i);
        EVAL FormsVBT.Insert (
               ed, "rescreenMenu",
               Fmt.F ("(MButton %%s (Text RightAlign \"%s:%s.%s\"))", name,
                      drec.hostname, Fmt.Int (drec.display), Fmt.Int (i)), 0);
        FormsVBT.Attach (ed, name, NEW (Mover, id := i, vbt := ed))
      END;
      EVAL FormsVBT.Insert (ed, "rescreenMenu", "\"Move Editor to\"", 0);
      EVAL FormsVBT.Insert (ed, "rescreenMenu", "(Bar 1)", 0);
      FOR i := LAST (array^) TO FIRST (array^) BY -1 DO
        name := "Frame" & Fmt.Int (i);
        EVAL FormsVBT.Insert (
               ed, "rescreenMenu",
               Fmt.F ("(MButton %%s (Text RightAlign \"%s:%s.%s\"))", name,
                      drec.hostname, Fmt.Int (drec.display), Fmt.Int (i)), 0);
        FormsVBT.Attach (ed, name, NEW (Mover, id := i, vbt := frame))
      END;
      EVAL FormsVBT.Insert (ed, "rescreenMenu", "\"Move Result to\"", 0);
    END;

    PROCEDURE FixSize (v: VBT.T; VAR g: XParam.Geometry) =
      BEGIN
        VBTClass.Rescreen (v, array [0].type);
        IF g.size = XParam.Missing THEN
          WITH shapes = VBTClass.GetShapes (v) DO
            g.size.h := shapes [Axis.T.Hor].pref;
            g.size.v := shapes [Axis.T.Ver].pref;
          END
        END
      END FixSize;
    BEGIN
      FixSize (ed, egrec);
      FixSize (frame, fgrec)
    END;

    Trestle.Attach(ed, trsl);
    ed.decorate();
    StableVBT.SetShape(ed, egrec.size.h, egrec.size.v);
    Trestle.Overlap(
      ed, drec.screen, XParam.Position(trsl, drec.screen, egrec));

    Trestle.Attach(frame, trsl);
    frame.decorate();
    Trestle.Overlap(frame, drec.screen,
                    XParam.Position(trsl, drec.screen, fgrec))
  END Install;

PROCEDURE DecorateFrame (frame: T) RAISES {TrestleComm.Failure} =
  BEGIN
    Trestle.Decorate (frame,
                      windowTitle :=
                        Fmt.F ("FV Result %s: %s", Fmt.Int (frame.number),
                               Last40 (frame.fullPathname)),
                      iconTitle := Fmt.F ("R %s: %s", Fmt.Int (frame.number),
                                          frame.shortname),
                      applName := "FormsEdit Result View", bgColorR := 0.7,
                      bgColorG := 0.7, bgColorB := 1.0)
  END DecorateFrame;

PROCEDURE DecorateEditor (ed: Editor) RAISES {TrestleComm.Failure} =
  VAR frame := ed.frame;
  BEGIN
    Trestle.Decorate (ed, windowTitle :=
                            Fmt.F ("FV Editor %s: %s", Fmt.Int (frame.number),
                                   Last40 (frame.fullPathname)),
                      iconTitle := Fmt.F ("E %s: %s", Fmt.Int (frame.number),
                                          frame.shortname),
                      applName := "FormsEdit", bgColorR := 1.0,
                      bgColorG := 0.7, bgColorB := 0.7)
  END DecorateEditor;

PROCEDURE Last40 (t: TEXT): TEXT =
  VAR n := Text.Length (t);
  BEGIN
    IF n <= 40 THEN RETURN t ELSE RETURN "..." & Text.Sub (t, n - 40, n) END
  END Last40;

PROCEDURE GetEditor (frame: T): FormsVBT.T =
  BEGIN
    RETURN frame.ed
  END GetEditor;

PROCEDURE Realize (ed: Editor; type, name: TEXT): VBT.T
  RAISES {FormsVBT.Error} =
  BEGIN
    IF Text.Equal (name, "openfile") AND Text.Equal (type, "FileBrowser") THEN
      RETURN NEW (JustFVfileBrowser, ed := ed)
    END;
    IF Text.Equal (name, "buffer") AND Text.Equal (type, "TextEdit") THEN
      RETURN NEW (FVTypes.FVTextEdit, tp := NEW (EPort, ed := ed))
    END;
    IF Text.Length (name) > 6 AND Text.Equal (Text.Sub (name, 0, 6), "Model_") THEN
      RefListUtils.Push (ed.modelTsplits, name)
    END;
    RETURN FormsVBT.T.realize (ed, type, name)
  END Realize;

PROCEDURE FBinit (fb    : JustFVfileBrowser;
                  font  : Font.T              := Font.BuiltIn;
                  colors: PaintOp.ColorQuad   := NIL           ):
  FileBrowserVBT.T =
  BEGIN
    EVAL FVTypes.FVFileBrowser.init (fb, font, colors);
    FileBrowserVBT.SetSuffixes (fb, "fv");
    RETURN fb
  END FBinit;

PROCEDURE FBerror (fb: JustFVfileBrowser; err: FileBrowserVBT.E) =
  <* LL = VBT.mu *>
  BEGIN
    Gripe (fb.ed, "Error in %s: %s", err.path, err.text)
  END FBerror;

PROCEDURE ChangeSuffixes (<* UNUSED *> fbcl: FormsVBT.Closure;
                                       fv  : FormsVBT.T;
                                       name: TEXT;
                          <* UNUSED *> time: VBT.TimeStamp  ) =
  VAR fb: FileBrowserVBT.T;
  BEGIN
    TRY
      fb := FormsVBT.GetVBT (fv, "openfile");
      IF Text.Equal (name, "fvonly") THEN
        FileBrowserVBT.SetSuffixes (fb, "fv")
      ELSE
        FileBrowserVBT.SetSuffixes (fb, "")
      END
    EXCEPT
    | FormsVBT.Error (msg) => Gripe (fv, msg)
    END
  END ChangeSuffixes;

PROCEDURE EPortNotFound (eport: EPort) =
  VAR fv := eport.ed;
  BEGIN
    TRY
      FormsVBT.PopUp (fv, "notfound");
      EVAL Thread.Fork (NEW (PDNF, fv := fv))
    EXCEPT
    | FormsVBT.Error (msg) => Gripe (eport.ed, msg)
    END
  END EPortNotFound;

PROCEDURE InitFCL (cl: FinderClosure; fv: FormsVBT.T): FinderClosure =
  BEGIN
    TRY
      FormsVBT.Attach (fv, cl.first, cl);
      FormsVBT.Attach (fv, cl.next, cl);
      FormsVBT.Attach (fv, cl.prev, cl);
      FormsVBT.Attach (fv, cl.typein, cl)
    EXCEPT
    | FormsVBT.Error (msg) => Gripe (fv, msg)
    END;
    RETURN cl;
  END InitFCL;

PROCEDURE ShowFindWindow (cl  : FinderClosure;
                          fv  : FormsVBT.T;
                          name: TEXT;
                          time: VBT.TimeStamp  ) =
  VAR
    loc : TextPortClass.Loc;
    te  : TextEditVBT.T;
    text: TEXT;
  BEGIN
    IF Text.Equal (name, cl.first) THEN
      loc := TextPortClass.Loc.First
    ELSIF Text.Equal (name, cl.typein) OR Text.Equal (name, cl.next) THEN
      loc := TextPortClass.Loc.Next
    ELSE
      loc := TextPortClass.Loc.Prev
    END;
    TRY
      te := FormsVBT.GetVBT (fv, cl.textedit);
      text := FormsVBT.GetText (fv, cl.typein);
      LOCK te.tp.mu DO
        TextPortClass.FindAndSelect (te.tp, text, time, loc)
      END;
      IF Text.Equal (name, cl.typein) THEN
        FormsVBT.MakeEvent (fv, cl.close, time)
      END
    EXCEPT
    | FormsVBT.Error (msg) => Gripe (fv, msg)
    END
  END ShowFindWindow;

TYPE
  PDNF = Thread.Closure OBJECT
           fv: FormsVBT.T
         OVERRIDES
           apply := PopDownNotFound
         END;

PROCEDURE PopDownNotFound (cl: PDNF): REFANY =
  BEGIN
    Thread.Pause (2.0D0);
    LOCK VBT.mu DO
      TRY
        FormsVBT.PopDown (cl.fv, "notfound")
      EXCEPT
      | FormsVBT.Error (msg) => Gripe (cl.fv, msg)
      END
    END;
    RETURN NIL
  END PopDownNotFound;

TYPE
  ER = Manpage.ErrorReporter OBJECT
         ed: Editor
       OVERRIDES
         apply := CallGripe
       END;

PROCEDURE CallGripe (er: ER; msg: TEXT) =
  <* LL = VBT.mu *>
  BEGIN
    Gripe (er.ed, msg)
  END CallGripe;

PROCEDURE UpdateKeybindingLabels (ed: Editor) RAISES {FormsVBT.Error} =
  VAR
    index   := ORD (ed.textport.getModel()) - 1;
    tsplits := ed.modelTsplits;
  BEGIN
    WHILE tsplits # NIL DO
      FormsVBT.PutInteger (ed, RefListUtils.Pop (tsplits), index)
    END
  END UpdateKeybindingLabels;

VAR
  qcl   := NEW (FormsVBT.Closure, apply := DoQuit);
  ccl   := NEW (FormsVBT.Closure, apply := DoClose);
  fbcl  := NEW (FormsVBT.Closure, apply := ChangeSuffixes);
  scl   := NEW (FormsVBT.Closure, apply := SaveAs);
  ppcl  := NEW (FormsVBT.Closure, apply := ChangePPW);

PROCEDURE EditorInit (ed: Editor; frame: T): Editor RAISES {FormsVBT.Error} =
  <* LL = VBT.mu *>
  <* FATAL Rsrc.NotFound *>
  PROCEDURE attach (name: TEXT; proc: KeyProc) RAISES {FormsVBT.Error} =
    BEGIN
      FormsVBT.Attach (
        ed, name, NEW (Attachment, frame := frame, proc := proc))
    END attach;
  BEGIN
    ed.frame := frame;
    LOCK FrameCountLock DO INC (FrameCount); frame.number := FrameCount END;
    EVAL Filter.T.init (frame, NIL);
    TRY
      EVAL ed.initFromRsrc ("formseditvbt.fv", formseditPath);
      Manpage.Init (ed, HelpFile, NEW (ER, ed := ed), helpcase := NIL,
                    path := formseditPath);
      ed.buffer := FormsVBT.GetVBT (ed, "buffer");
      ed.stderr := FormsVBT.GetVBT (ed, "stderr");
      ed.errorPopup := FormsVBT.GetVBT (ed, "errorPopup");
      ed.textport := ed.buffer.tp;
      ed.vtext := TextPort.GetVText (ed.textport);
      ed.mtext := ed.vtext.mtext;
      ed.rd := NEW (MTextRd.T).init (ed.mtext);
      ed.rangeTable := NEW (IntRefTbl.Default).init ();
      ed.syntax := Sx.CopySyntax (FVRuntime.FVSyntax);
      Sx.SetReadMacro (ed.syntax, '(', NEW (ReadMacro, ed := ed));

      VBT.SetCursor (ed.textport, Cursor.TextPointer);
      FormsVBT.AttachEditOps (
        ed, "buffer", "cut", "copy", "paste", "clear", "selectAll", "undo",
        "redo", NIL, "findNext", "findPrev");
      FormsVBT.AttachEditOps (
        ed, "manpagetext", copy := "mpcopy", selectAll := "mpselectAll",
        findFirst := "helpfindfirst", findNext := "helpfindnext",
        findPrev := "helpfindprev");
      EVAL NEW (FinderClosure, first := "bhelpfindfirst",
                next := "bhelpfindnext", prev := "bhelpfindprev",
                typein := "bhelpfindtext", textedit := "buffer",
                close := "bhelpfindclose").init (ed);
      EVAL
        NEW (FinderClosure, first := "helpfindfirst", next := "helpfindnext",
             prev := "helpfindprev", typein := "helpfindtext",
             textedit := "manpagetext", close := "helpfindclose").init (ed);
      FormsVBT.Attach (ed, "close", ccl);
      FormsVBT.Attach (ed, "closeAnyway", ccl);
      attach ("errorPopup", Reset);
      attach ("dumpTable", DumpTheTable);
      FormsVBT.Attach (ed, "fvonly", fbcl);
      UpdateKeybindingLabels (ed);
      attach ("new", New);
      FormsVBT.Attach (ed, "notfvonly", fbcl);
      attach ("open", DoOpen);   (* the Open button in the dialog *)
      attach ("openfile", DoOpen); (* typing Return in the helper *)
      attach ("openMButton", OpenDialog); (* the Open...  menu item *)
      FormsVBT.Attach (ed, "overwrite", scl); (* the Yes button in the
                                                 overwrite confirmation *)
      attach ("parse", Parse);
      attach ("PPrint", PrettyPrint);
      FormsVBT.Attach (ed, "ppwidth", ppcl);
      FormsVBT.Attach (ed, "ppwidthPopMButton", ppcl);
      FormsVBT.Attach (ed, "ppwRevert", ppcl);
      FormsVBT.Attach (ed, "ppwApply", ppcl);
      FormsVBT.Attach (ed, "ppwOK", ppcl);
      FormsVBT.Attach (ed, "quit", qcl);
      FormsVBT.Attach (ed, "quit2", qcl);
      FormsVBT.Attach (ed, "quitAnyway", qcl);
      attach ("revert", Revert);
      attach ("save", Save);
      FormsVBT.Attach (ed, "saveandclose", ccl);
      FormsVBT.Attach (ed, "saveandquit", qcl);
      attach ("saveandswitch", SaveAndSwitch);
      FormsVBT.Attach (ed, "saveas", scl); (* the Save button in the dialog *)
      FormsVBT.Attach (ed, "saveasfile", scl); (* typing Return in the
                                                  helper *)
      attach ("snapshot", Snapshot);
      attach ("switchAnyway", SwitchAnyway);
      ed.highlighter :=
        VText.CreateInterval (ed.vtext, 0, 0, HighlightOptions)
    EXCEPT
    | VTDef.Error (code) => RAISE FormsVBT.Error (VTDef.ErrorCodeTexts [code])
    | Rd.Failure (ref) => RAISE FormsVBT.Error (RdUtils.FailureText (ref))
    | Thread.Alerted => RAISE FormsVBT.Error ("Alerted")
    END;
    RETURN ed
  END EditorInit;

PROCEDURE AttachmentApply (             cl  : Attachment;
                           <* UNUSED *> v   : FormsVBT.T;
                           <* UNUSED *> name: TEXT;
                                        time: VBT.TimeStamp) =
  BEGIN
    cl.proc (cl.frame, time)
  END AttachmentApply;

PROCEDURE EPortFilter (eport: EPort; cd: VBT.KeyRec) =
  <* LL = VBT.mu *>
  VAR
    frame := eport.ed.frame;
    time  := cd.time;
    handled := TRUE;             (* Did we recognize this
                                    key? *)
  BEGIN
    TRY
      IF cd.whatChanged = KeyboardKey.Menu (* "Do" key *)
           OR cd.whatChanged = KeyboardKey.KP_Enter THEN
        Parse (frame, time)
      ELSIF cd.whatChanged = KeyboardKey.Help THEN
        Help (frame, time)
      ELSIF VBT.Modifier.Option IN cd.modifiers
              OR VBT.Modifier.Control IN cd.modifiers
                   AND eport.getModel () = TextPort.Model.Mac THEN
        CASE KeyTrans.Latin1 (cd.whatChanged) OF
        | 'a' => SelectAll (frame, time)
        | 'f' =>
            IF eport.getModel () # TextPort.Model.Emacs THEN
              FormsVBT.MakeEvent (
                eport.ed, "findMButton", time)
            ELSE
              handled := FALSE
            END
        | 'h' => Help (frame, time)
        | 'n' => New (frame, time)
        | 'o' => OpenDialog (frame, time)
        | 'p' => PrettyPrint (frame, time)
        | 'q' => FormsVBT.MakeEvent (eport.ed, "quit", time)
        | 's' => Save (frame, time)
        ELSE
          handled := FALSE
        END
      ELSE
        handled := FALSE
      END;
      IF NOT handled THEN FVTypes.Port.filter (eport, cd) END
    EXCEPT
    | FormsVBT.Error (msg) => Gripe (frame.ed, msg)
    END
  END EPortFilter;

(*********************** Editing Commands **********************************)

PROCEDURE SelectAll (frame: T; time: VBT.TimeStamp) =
  <* LL = VBT.mu *>
  BEGIN
    TextPort.Select (
      frame.ed.textport, time, 0, LAST (CARDINAL), replaceMode := TRUE)
  END SelectAll;

(*********************** Control Commands **********************************)

PROCEDURE DoQuit (<* UNUSED *> cl  : FormsVBT.Closure;
                               fv  : FormsVBT.T;
                               name: TEXT;
                               time: VBT.TimeStamp     ) =
  <* LL = VBT.mu *>
  VAR
    ed   : Editor := fv;
    frame         := ed.frame;
  BEGIN
    TRY
      IF Text.Equal (name, "quit") OR Text.Equal (name, "quit2") THEN
        IF NOT TextPort.IsModified (ed.textport) THEN
          frame.delete ();
          Thread.Alert (frame.root.thread) (* Alert the EdRoot *)
        ELSE
          FormsVBT.PopUp (ed, "quitConfirmation")
        END
      ELSIF Text.Equal (name, "quitAnyway") THEN
        frame.delete ();
        Thread.Alert (frame.root.thread)
      ELSIF NOT Text.Equal (name, "saveandquit") THEN (* skip *)
      ELSIF NOT Text.Empty (frame.fullPathname) THEN
        Save (frame, time);
        frame.delete ();
        Thread.Alert (frame.root.thread)
      ELSE
        FormsVBT.PopUp (ed, "SaveAsDialog");
        FormsVBT.PopDown (ed, "quitConfirmation")
      END
    EXCEPT
    | FormsVBT.Error (msg) => Gripe (ed, msg)
    END;
  END DoQuit;

PROCEDURE DoClose (<* UNUSED *> cl  : FormsVBT.Closure;
                                fv  : FormsVBT.T;
                                name: TEXT;
                                time: VBT.TimeStamp     ) =
  <* LL = VBT.mu *>
  VAR
    ed   : Editor := fv;
    frame         := ed.frame;
  BEGIN
    TRY
      IF Text.Equal (name, "close") THEN
        IF NOT TextPort.IsModified (ed.textport) THEN
          frame.delete ()
        ELSE
          FormsVBT.PopUp (ed, "closeConfirmation")
        END
      ELSIF Text.Equal (name, "closeAnyway") THEN
        frame.delete ()
      ELSIF NOT Text.Equal (name, "saveandclose") THEN (* skip *)
      ELSIF Text.Empty (frame.fullPathname) THEN
        FormsVBT.PopUp (frame.ed, "SaveAsDialog");
        FormsVBT.PopDown (ed, "closeConfirmation")
      ELSE
        Save (frame, time);
        frame.delete ()
      END
    EXCEPT
    | FormsVBT.Error (msg) => Gripe (ed, msg)
    END
  END DoClose;

PROCEDURE DeleteFrame (frame: T) =
  <* LL = VBT.mu *>
  VAR root := frame.root;
  BEGIN
    LOCK frame.mu DO
      Trestle.Delete (frame);
      Trestle.Delete (frame.ed);
      LOCK root.mu DO
        RefListUtils.DeleteQ (root.frames, frame);
        IF root.frames = NIL THEN Thread.Signal (root.allClosed) END
      END
    END
  END DeleteFrame;

PROCEDURE New (frame: T; <* UNUSED *> time: VBT.TimeStamp) =
  <* LL = VBT.mu *>
  VAR newframe: T;
  BEGIN
    TRY
      newframe := NEW (T, root := frame.root).init ();
      Install (newframe, MoveGeometry (frame));
      newframe.spawn ()
    EXCEPT
    | TrestleComm.Failure, XParam.Error, FormsVBT.Error =>
        Gripe (frame.ed, "Couldn't install new window")
    END
  END New;

PROCEDURE MoveGeometry (frame: T): TEXT =
  <* LL = VBT.mu *>
  CONST
    Displacement = ARRAY Rect.Vertex OF
                     Point.T {Point.T {50, 50}, Point.T {-50, 50},
                              Point.T {50, -50}, Point.T {-50, -50}};
  VAR g := frame.egrec; d := VBT.Domain (frame.ed);
  BEGIN
    g.dp := Point.Add (g.dp, Displacement [g.vertex]);
    g.size := Point.T {Rect.HorSize (d), Rect.VerSize (d)}; 
    RETURN XParam.UnparseGeometry (g)
  END MoveGeometry;

(*********************** Help Command **********************************)

PROCEDURE Help (frame: T; <* UNUSED *> time: VBT.TimeStamp)
  RAISES {FormsVBT.Error} =
  <* LL = VBT.mu *>
  BEGIN
    FormsVBT.PopUp (frame.ed, "manpage")
  END Help;

PROCEDURE Revert (frame: T; <* UNUSED *> time: VBT.TimeStamp) =
  <* LL = VBT.mu *>
  BEGIN
    TRY
      Read (frame);
      FormsVBT.PopDown (frame.ed, "RevertDialog")
    EXCEPT
    | FormsVBT.Error (msg) => Gripe (frame.ed, msg)
    | Thread.Alerted =>
    END
  END Revert;

(****************** Snapshot/Restore Command *****************************)

PROCEDURE Snapshot (frame: T; <* UNUSED *> time: VBT.TimeStamp) =
  <* LL = VBT.mu *>
  <* FATAL Wr.Failure, Thread.Alerted *>(* Can't happen with TextWr *)
  BEGIN
    WITH ed = frame.ed,
         ch = NARROW (Filter.Child (frame), FormsVBT.T),
         wr = TextWr.New ()                              DO
      TRY
        TRY
          FormsVBT.PutText (ed, "SnapshotText", "");
          ch.snapshot (wr);
          FormsVBT.PutText (ed, "SnapshotText", TextWr.ToText (wr));
        EXCEPT
        | FormsVBT.Error (msg) => Gripe (ed, msg)
        END
      FINALLY
        Wr.Close (wr)
      END;
    END
  END Snapshot;

(*********************** Open Command **********************************)

PROCEDURE OpenDialog (<* UNUSED *> frame: T;
                      <* UNUSED *> time : VBT.TimeStamp) =
  <* LL = VBT.mu *>
  BEGIN
    (* do nothing; keep the popup just as it was 
       the last time it was displayed *)
  END OpenDialog;

PROCEDURE DoOpen (frame: T; <* UNUSED *> time: VBT.TimeStamp) =
  <* LL = VBT.mu *>
  VAR
    ed         := frame.ed;
    file: TEXT;
  BEGIN
    TRY
      file := FormsVBT.GetText (ed, "openfile");
      IF Text.Empty (file) THEN
        Gripe (ed, "No such file");
        RETURN
      ELSIF NOT FormsVBT.GetBoolean (ed, "reuse") THEN
        OpenNewWindow (frame, file)
      ELSIF TextPort.IsModified (ed.textport) THEN
        FormsVBT.PopUp (ed, "switchConfirmation")
      ELSE
        OpenInCurrentWindow (frame, file)
      END;
      FormsVBT.PopDown (ed, "OpenDialog")
    EXCEPT
    | FormsVBT.Error (text) => Gripe (ed, text)
    END
  END DoOpen;

PROCEDURE OpenNewWindow (frame: T; filename: TEXT) =
  BEGIN
    TRY
      WITH newframe = NEW (T, root := frame.root).initFromFile (filename) DO
        Install (newframe, MoveGeometry (frame));
        newframe.spawn ()
      END
    EXCEPT
    | TrestleComm.Failure, XParam.Error =>
        Gripe (frame.ed, "Couldn't install new window")
    | FormsVBT.Error (text) => Gripe (frame.ed, text)
    | Thread.Alerted =>
    END
  END OpenNewWindow;

PROCEDURE SwitchAnyway (frame: T; <* UNUSED *> time: VBT.TimeStamp) =
  <* LL = VBT.mu *>
  VAR ed := frame.ed;
  BEGIN
    TRY
      ClearError (ed);
      FormsVBT.PopDown (ed, "switchConfirmation");
      OpenInCurrentWindow (frame, FormsVBT.GetText (ed, "openfile"))
    EXCEPT
    | FormsVBT.Error (text) => Gripe (ed, text)
    END
  END SwitchAnyway;

PROCEDURE OpenInCurrentWindow (frame: T; filename: TEXT) =
  <* LL = VBT.mu *>
  BEGIN
    TRY
      frame.fullPathname := filename;
      frame.shortname := Pathname.Last (filename);
      FormsVBT.PutText (frame.ed, "shortname", frame.shortname);
      Read (frame)
    EXCEPT
    | FormsVBT.Error (text) => Gripe (frame.ed, text)
    | Thread.Alerted =>
    END
  END OpenInCurrentWindow;
  
PROCEDURE SaveAndSwitch (frame: T; time: VBT.TimeStamp) =
  <* LL = VBT.mu *>
  VAR ed := frame.ed;
  BEGIN
    TRY
      ClearError (ed);
      Save (frame, time);
      OpenInCurrentWindow (frame, FormsVBT.GetText (ed, "openfile"));
      FormsVBT.PopDown (ed, "switchConfirmation")
    EXCEPT
    | FormsVBT.Error (text) => Gripe (ed, text)
    END
  END SaveAndSwitch;


(*********************** Error-handling **********************************)

TYPE
  Edown = Thread.SizedClosure OBJECT
            ed: Editor
          OVERRIDES
            apply := RemoveErrorWindow
          END;

PROCEDURE EPortError (<* UNUSED *> p  : EPort;
                      <* UNUSED *> msg: TEXT   ) =
  <* LL = VBT.mu *>
  BEGIN
    (* most of the errors are about grabbing focus, so we won't
       bother the user -- mhb 9/22/93: Gripe (p.ed, msg) *)
  END EPortError;
   
PROCEDURE Gripe (ed: Editor; fmt: TEXT; a, b, c, d, e: TEXT := NIL) =
  <* LL = VBT.mu *>
  BEGIN
    IF a # NIL THEN fmt := Fmt.F (fmt, a, b, c, d, e) END;
    TextPort.SetText (ed.stderr.tp, fmt);
    ZChildVBT.Pop (ed.errorPopup);
    EVAL Thread.Fork (NEW (Edown, stackSize := 3000, ed := ed))
  END Gripe;

PROCEDURE RemoveErrorWindow (cl: Edown): REFANY =
  <* LL = {} *>
  <* FATAL FormsVBT.Error *>(* "errorPopup" exists. *)
  BEGIN
    Thread.Pause (5.0D0);
    LOCK VBT.mu DO FormsVBT.PopDown (cl.ed, "errorPopup") END;
    RETURN NIL
  END RemoveErrorWindow;
      
PROCEDURE LockNGripe (ed: Editor; fmt: TEXT; a, b, c, d, e: TEXT := NIL) =
  <* LL = 0 *>
  BEGIN
    LOCK VBT.mu DO Gripe (ed, fmt, a, b, c, d, e) END
  END LockNGripe; 

PROCEDURE ClearError (ed: Editor) =
  <* LL = VBT.mu *>
  <* FATAL FormsVBT.Error *>(* "errorPopup" exists. *)
  BEGIN
    FormsVBT.PopDown (ed, "errorPopup");
    TextPort.SetText (ed.stderr.tp, "")
  END ClearError;

PROCEDURE NoteModification (eport: EPort) =
  <* LL = VBT.mu *>
  BEGIN
    SetModified (eport.ed, TRUE)
  END NoteModification;
  
PROCEDURE SetModified (ed: Editor; value: BOOLEAN) =
  <* LL = VBT.mu *>
  <* FATAL FormsVBT.Error *>
  BEGIN
    FormsVBT.PutInteger (ed, "modified", ORD (value))
  END SetModified;

PROCEDURE Reset (frame: T; <* UNUSED *> time: VBT.TimeStamp := 0) =
  <* LL = VBT.mu *>
  VAR ed := frame.ed;
  BEGIN
    ClearError (ed);
    ed.rangeTable  := NEW (IntRefTbl.Default).init ();
    TRY
      VText.SwitchInterval (ed.highlighter, VText.OnOffState.Off);
      VBT.Mark (ed.textport)
    EXCEPT
    | VTDef.Error (code) => Gripe (ed, VTDef.ErrorCodeTexts [code])
    END
  END Reset;

(*********************** PPrint Command **********************************)

PROCEDURE ChangePPW (<* UNUSED *> ppcl: FormsVBT.Closure;
                                  fv  : FormsVBT.T;
                                  name: TEXT;
                                  time: VBT.TimeStamp     ) =
  <* LL = VBT.mu *>
  <* FATAL FormsVBT.Unimplemented *>
  VAR
    ed   : Editor := fv;
    frame         := ed.frame;
  BEGIN
    TRY
      IF Text.Equal (name, "ppwidth") THEN (* the Numeric *)
        frame.prettyprintWidth := FormsVBT.GetInteger (frame.ed, "ppwidth");
        TYPECASE FormsVBT.GetTheEvent (frame.ed) OF
        | AnyEvent.Key =>        (* User typed Return *)
            frame.revertWidth := frame.prettyprintWidth;
            FormsVBT.PopDown (frame.ed, "PPwidthNumeric");
            PrettyPrint (frame, time)
        | AnyEvent.Mouse =>      (* User clicked +/- button *)
        ELSE                     <* ASSERT FALSE *>
        END
      ELSIF Text.Equal (name, "ppwRevert") THEN
        FormsVBT.PutInteger (fv, "ppwidth", frame.revertWidth);
        frame.prettyprintWidth := frame.revertWidth;
        PrettyPrint (frame, time)
      ELSIF Text.Equal (name, "ppwApply") THEN
        frame.prettyprintWidth := FormsVBT.GetInteger (frame.ed, "ppwidth");
        PrettyPrint (frame, time)
      ELSIF Text.Equal (name, "ppwOK") THEN
        frame.prettyprintWidth := FormsVBT.GetInteger (frame.ed, "ppwidth");
        frame.revertWidth := frame.prettyprintWidth;
        FormsVBT.PopDown (frame.ed, "PPwidthNumeric");
        PrettyPrint (frame, time)
      ELSIF Text.Equal (name, "ppwidthPopMButton") THEN
        FormsVBT.PutInteger (fv, "ppwidth", frame.revertWidth);
        frame.prettyprintWidth := frame.revertWidth
      END
    EXCEPT
    | FormsVBT.Error (msg) => Gripe (frame.ed, msg)
    END
  END ChangePPW;

PROCEDURE PrettyPrint (frame: T; <* UNUSED *> time: VBT.TimeStamp) =
  <* LL = VBT.mu *>
  <* FATAL Thread.Alerted *>
  (* This is fast enough that we can do it in event-time. *)
  VAR ed := frame.ed;
  BEGIN
    VBT.SetCursor (frame.ed, Cursor.NotReady);
    TRY
      Reset (frame);
      TRY
        WITH oldtext     = TextPort.GetText (ed.textport),
             oldlength   = Text.Length (oldtext),
             oldposition = TextPort.Index (ed.textport),
             rd          = TextRd.New (oldtext),
             s           = Sx.Read (rd, syntax := FVRuntime.FVSyntax),
             wr          = TextWr.New (),
             fmt         = Formatter.New (wr, frame.prettyprintWidth)          DO
          PPrint (fmt, s);
          Formatter.Close (fmt);
          WITH newtext     = TextWr.ToText (wr),
               newlength   = Text.Length (newtext),
               newposition = (oldposition * newlength) DIV oldlength DO
            TextPort.SetText (ed.textport, newtext);
            TextPort.Normalize (ed.textport, newposition);
          END
        END
      EXCEPT
      | Sx.ReadError (msg) => Gripe (ed, "S-expression error: %s", msg)
      | Sx.PrintError (ref) => Gripe (ed, SxPrintErrorText (ref))
      | Rd.EndOfFile => Gripe (ed, "Premature end of file")
      | Wr.Failure (ref) => Gripe (ed, RdUtils.FailureText (ref))
      END
    FINALLY
      VBT.SetCursor (frame.ed, Cursor.TextPointer)
    END
  END PrettyPrint;

PROCEDURE PPrint (fmt: Formatter.T; s: Sx.T) RAISES {Wr.Failure, Sx.PrintError} =
  VAR
    wr              := TextWr.New ();
    car, cadr: Sx.T;
  PROCEDURE default (x: RefList.T) RAISES {Wr.Failure, Sx.PrintError} =
    BEGIN
      Formatter.Begin (fmt, 2);
      Formatter.PutChar (fmt, '(');
      LOOP
        pprint (x.head);
        x := x.tail;
        IF x = NIL THEN EXIT END;
        Formatter.PutChar (fmt, ' ');
        Formatter.UnitedBreak (fmt, 0)
      END;
      Formatter.PutChar (fmt, ')');
      Formatter.End (fmt)
    END default;
  PROCEDURE pprint (s: Sx.T) RAISES {Wr.Failure, Sx.PrintError} =
    BEGIN
      TYPECASE s OF
      | NULL => Formatter.PutText (fmt, "()", TRUE)
      | RefList.T (x) =>
          IF RefList.Length (x) # 2 THEN
            default (x)
          ELSE
            car := x.head;
            cadr := x.tail.head;
            IF car = FVRuntime.qName THEN
              Formatter.PutChar (fmt, '%');
              pprint (cadr);
            ELSIF car = FVRuntime.qValue THEN
              Formatter.PutChar (fmt, '=');
              pprint (cadr)
            ELSIF car = FVRuntime.qQuote THEN
              Formatter.PutChar (fmt, '\'');
              pprint (cadr)
            ELSIF car = FVRuntime.qBackquote THEN
              Formatter.PutChar (fmt, '`');
              pprint (cadr)
            ELSIF car = FVRuntime.qComma THEN
              Formatter.PutChar (fmt, ',');
              pprint (cadr)
            ELSIF car = FVRuntime.qCommaAtsign THEN
              Formatter.PutText (fmt, ",@", TRUE);
              pprint (cadr)
            ELSE
              default (x)
            END
          END
      ELSE
        TRY Sx.Print (wr, s) EXCEPT Thread.Alerted => <* ASSERT FALSE *> END;
        Formatter.PutText (fmt, TextWr.ToText (wr))
      END
    END pprint;
  BEGIN
    pprint (s)
  END PPrint;



PROCEDURE SxPrintErrorText (ref: REFANY): TEXT =
  BEGIN
    TYPECASE ref OF
    | TEXT (msg) => RETURN "S-expression print error: " & msg
    ELSE
      RETURN "Unknown Sx.PrintError"
    END
  END SxPrintErrorText;
  
(******************* Parse ("Do It") Command ******************************)

PROCEDURE Parse (frame: T; <* UNUSED *> time: VBT.TimeStamp := 0) =
  <* LL = VBT.mu *>
  <* FATAL FormsVBT.Error *>
  BEGIN
    Reset (frame);
    FormsVBT.MakePassive (frame.ed, "top");
    EVAL Thread.Fork (NEW (ParseClosure, stackSize := STACKSIZE,
                           frame := frame))
  END Parse;

PROCEDURE ParseClosureApply (cl: ParseClosure): REFANY =
  <* LL = 0 *>
  VAR
    frame         := cl.frame;
    ed            := frame.ed;
    new           := NEW (FormsVBT.T);
    old  : VBT.T;
    form : REFANY;
  BEGIN
    TRY                          (* EXCEPT *)
      TRY                        (* FINALLY *)
        form := Sx.Read (ed.rd.init (), syntax := ed.syntax);
        (* Now check for extra characters: *)
        TRY
          EVAL Sx.Read (ed.rd, syntax := ed.syntax);
          RAISE Sx.ReadError ("Extra characters on input")
        EXCEPT
        | Rd.EndOfFile =>
        END;
        (* As it reads, start/end intervals will be added to the table. *)
        LOCK VBT.mu DO
          EVAL new.initFromSx (form, path := frame.path);
          StableVBT.Disable (frame);
          old := Filter.Replace (frame, new);
          IF old # NIL THEN
            FVRuntime.SetAttachments (new, FVRuntime.GetAttachments (old));
            VBT.Discard (old)
          END;
          ClearError (ed)
        END
      FINALLY
        LOCK VBT.mu DO FormsVBT.MakeActive (ed, "top") END;
      END
    EXCEPT
    | FormsVBT.Error (msg) =>
        LOCK VBT.mu DO Gripe (ed, msg); HighlightError (new, frame) END
    | Sx.ReadError (msg) => LOCK VBT.mu DO Gripe (ed, msg) END
    | Rd.EndOfFile => LockNGripe (ed, "Premature end of file ")
    | Rd.Failure (ref) => LockNGripe (ed, RdUtils.FailureText (ref))
    | Thread.Alerted =>
    END;
    RETURN NIL
  END ParseClosureApply;

PROCEDURE ReadList (rm: ReadMacro; rd: Rd.T; s: Sx.Syntax): RefList.T
  RAISES {Sx.ReadError, Thread.Alerted} =
  (* Record the starting and ending positions of every list we read, so that
     we can highlight the list if there's an error. *)
  VAR
    start := Rd.Index (rd) - 1;
    form  := Sx.ReadDelimitedList (rd, ')', s);
    end   := Rd.Index (rd);
  BEGIN
    EVAL rm.ed.rangeTable.put (
           start, NEW (Range, start := start, end := end, form := form));
    RETURN RefList.List1 (form)
  END ReadList;

TYPE Range = REF RECORD start, end: INTEGER; form: Sx.T END;

PROCEDURE FindRange (t: IntRefTbl.T; form: Sx.T): Range =
  VAR
    iter           := t.iterate ();
    start: INTEGER;
    ref  : REFANY;
    r    : Range;
  BEGIN
    WHILE iter.next (start, ref) DO
      r := ref;
      IF r.form = form THEN RETURN r END
    END;
    RETURN NIL
  END FindRange;
  
PROCEDURE HighlightError (new: FormsVBT.T; frame: T) =
  <* LL = VBT.mu *>
  VAR
    ed           := frame.ed;
    stack        := new.formstack;
    r    : Range;
  BEGIN
    WHILE stack # NIL DO
      r := FindRange (ed.rangeTable, RefListUtils.Pop (stack));
      IF r # NIL THEN
        TRY
          TextPort.Normalize (ed.textport, r.start);
          VText.MoveInterval (ed.highlighter, r.start, r.end);
          VText.SwitchInterval (ed.highlighter, VText.OnOffState.On);
          VBT.Mark (ed.textport)
        EXCEPT
        | VTDef.Error =>         (* ignore *)
        END;
        RETURN
      END                        (* IF *)
    END                          (* WHILE *)
  END HighlightError;

(******************* Save and SaveAs Commands ******************************)

PROCEDURE Save (frame: T; <* UNUSED *> time: VBT.TimeStamp) =
  <* LL = VBT.mu *>
  VAR
    ed             := frame.ed;
    filename       := frame.fullPathname;
    wr      : Wr.T;
  BEGIN
    ClearError (ed);
    TRY
      IF Text.Empty (filename) THEN
        FormsVBT.PopUp (ed, "SaveAsDialog");
        RETURN
      END;
      wr := FileWr.Open (filename);
      TRY
        Wr.PutText (wr, TextPort.GetText (ed.textport))
      FINALLY
        Wr.Close (wr)
      END;
      TextPort.SetModified (ed.textport, FALSE);
      SetModified (ed, FALSE)
    EXCEPT
    | OSError.E (list) => Gripe (ed, RdUtils.FailureText (list))
    | FormsVBT.Error (msg) => Gripe (ed, msg)
    | Wr.Failure (refany) =>
        Gripe (
          ed, "Couldn't write %s: %s", filename, RdUtils.FailureText (refany))
    | Thread.Alerted =>
    END
  END Save;

PROCEDURE SaveAs (<* UNUSED *> cl  : FormsVBT.Closure;
                               fv  : FormsVBT.T;
                               name: TEXT;
                  <* UNUSED *> time: VBT.TimeStamp     ) =
  <* LL = VBT.mu *>
  VAR
    ed      : Editor := fv;
    frame            := ed.frame;
    filename: TEXT;
    wr      : Wr.T;
  BEGIN
    ClearError (ed);
    TRY
      filename := FormsVBT.GetText (ed, "saveasfile");
      IF Text.Empty (filename) THEN Gripe (ed, "No filename."); RETURN END;
      IF Text.Equal (name, "overwrite") THEN (* Don't ask *)
        FormsVBT.PopDown (ed, "overwriteConfirmation")
      ELSIF ProbeFile (filename) THEN
        FormsVBT.PopUp (ed, "overwriteConfirmation");
        FormsVBT.PopDown (ed, "SaveAsDialog");
        RETURN
      END;
      wr := FileWr.Open (filename);
      TRY
        Wr.PutText (wr, TextPort.GetText (ed.buffer.tp));
      FINALLY
        Wr.Close (wr)
      END;
      frame.fullPathname := FS.GetAbsolutePathname (filename);
      frame.shortname := Pathname.Prefix (filename);
      FormsVBT.PutText (ed, "shortname", frame.shortname);
      TextPort.SetModified (ed.textport, FALSE);
      SetModified (ed, FALSE);
      FormsVBT.PopDown (ed, "SaveAsDialog");
      frame.decorate ();
      ed.decorate ()
    EXCEPT
    | OSError.E (list) => Gripe (ed, RdUtils.FailureText (list))
    | FormsVBT.Error (msg) => Gripe (ed, msg)
    | Wr.Failure (refany) =>
        Gripe (
          ed, "Couldn't write %s: %s", filename, RdUtils.FailureText (refany))
    | Thread.Alerted =>
    | TrestleComm.Failure => Gripe (ed, "Couldn't change window labels")
    END
  END SaveAs;

PROCEDURE ProbeFile (pn: Pathname.T): BOOLEAN =
  BEGIN
    TRY
      Rd.Close (FileRd.Open (pn));
      RETURN TRUE
    EXCEPT
    | OSError.E, Rd.Failure, Thread.Alerted => RETURN FALSE
    END
  END ProbeFile;

PROCEDURE DumpTheTable (frame: T; <* UNUSED *> time: VBT.TimeStamp) =
  <* LL = VBT.mu *>
  <* FATAL Wr.Failure, Thread.Alerted *>(* all in-memory *)
  BEGIN
    VAR
      ed          := frame.ed;
      ch          := Filter.Child (frame);
      alist       := FVRuntime.NamedVBTs (ch);
      alist2      := alist;
      attachments := FVRuntime.GetAttachments (ch);
      maxlen      := 0;
    VAR
      value: REFANY;
      key  : TEXT;
      pair : RefList.T;
      vbt  : VBT.T;
      sr   : VBT.SizeRange;
      size : CARDINAL;
    BEGIN
      WHILE alist2 # NIL DO
        pair := RefListUtils.Pop (alist2);
        maxlen := MAX (maxlen, Text.Length (pair.head))
      END;
      WITH wr = TextWr.New () DO
        TRY
          Wr.PutText (
            wr,
            Fmt.Pad ("name", maxlen)
              & " : type  H: [lo, pref, hi] = size. V: [lo, pref, hi] = size.\n\n");
          WHILE alist # NIL DO
            pair := RefListUtils.Pop (alist);
            key := RefListUtils.Pop (pair);
            value := pair.head;
            Wr.PutText (wr, Fmt.F ("%s : %s", Fmt.Pad (key, maxlen),
                                   RTTypeSRC.TypeName (value)));
            IF RefListUtils.Assoc (attachments, key) # NIL THEN
              Wr.PutChar (wr, '*')
            END;
            vbt := value;
            WITH a = VBTClass.GetShapes (vbt, clearNewShape := FALSE),
                 d = VBT.Domain (vbt)                                  DO
              FOR ax := FIRST (Axis.T) TO LAST (Axis.T) DO
                sr := a [ax];
                IF ax = Axis.T.Hor THEN
                  size := Rect.HorSize (d)
                ELSE
                  size := Rect.VerSize (d)
                END;
                Wr.PutText (wr, Fmt.F (" %s: [%s, %s, %s] = %s.",
                                       ARRAY Axis.T OF TEXT {"H", "V"} [ax],
                                       Fmt.Int (sr.lo), Fmt.Int (sr.pref),
                                       Fmt.Int (sr.hi), Fmt.Int (size)))
              END
            END;
            Wr.PutChar (wr, '\n')
          END;
          TRY
            FormsVBT.PutText (ed, "VBTtable", TextWr.ToText (wr))
          EXCEPT
          | FormsVBT.Error (msg) => Gripe (ed, msg)
          END
        FINALLY
          Wr.Close (wr)
        END
      END
    END
  END DumpTheTable;

PROCEDURE MoverApply (             m         : Mover;
                                   ed        : FormsVBT.T;
                      <* UNUSED *> buttonName: TEXT;
                      <* UNUSED *> time      : VBT.TimeStamp) =
  <* LL = VBT.mu *>
  BEGIN
    TRY
      WITH nw  = Rect.NorthWest (VBT.Domain (m.vbt)),
           rec = Trestle.ScreenOf (m.vbt, nw)         DO
        Trestle.Overlap (m.vbt, m.id, rec.q)
      END
    EXCEPT
    | TrestleComm.Failure => Gripe (ed, "Can't move. Trestle.Overlap failed.")
    END
  END MoverApply;

VAR doDebug := FALSE;

PROCEDURE Debug (t: TEXT) =
  BEGIN
    IF doDebug THEN IO.Put (t) END
  END Debug;
  
BEGIN
  HighlightOptions :=
    VText.MakeIntervalOptions (
      VText.IntervalStyle.BoxStyle, PaintOp.bgFg, PaintOp.bgFg, PaintOp.Bg)
END FormsEditVBT.
