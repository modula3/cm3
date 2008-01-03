(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 31 11:45:16 PST 1995 by kalsow                   *)
(*      modified on Sun Jun  5 14:23:16 PDT 1994 by mhb                      *)

MODULE RehearseCode EXPORTS Main;

IMPORT AutoRepeat, Axis, CodeView, FileRd, Fmt, FormsVBT, HVBar,
       HVSplit, ListVBT, OSError, Params, Rd, RefList,
       RehearseCodeBundle, Rsrc, SortedIntRefTbl,
       SortedTextRefTbl, Split, Stdio, Text, TextEditVBT,
       TextPort, Thread, Trestle, TrestleComm, VBT, Wr, WrClass;

<* FATAL Rsrc.NotFound, Rd.Failure, Wr.Failure, Thread.Alerted *>
<* FATAL Split.NotAChild, TrestleComm.Failure *>
<* FATAL FormsVBT.Error *>

TYPE
  View = REF RECORD
               filename: TEXT         := NIL;
               codeview: CodeView.T;
             END;

  Writer = Wr.T OBJECT
             typescript: TextEditVBT.T;
           OVERRIDES
             seek  := Seek;
             flush := Flush;
           END;

  Repeater = AutoRepeat.T OBJECT OVERRIDES
               repeat := RepeatStep
             END;

VAR
  procNames   : RefList.T (* of TEXT *);
  regions     : RefList.T (* of REF INTEGER *);
  views       : RefList.T (* of View *);
  running                  := FALSE;
  currentProc : TEXT       := NIL;
  fv          : FormsVBT.T;
  typescriptWr: Writer;
  codeViews   : HVSplit.T;
  repeater                 := NEW (Repeater).init (0, 400);

PROCEDURE NewWriter (ts: TextEditVBT.T): Writer =
  CONST BufferSize = 100;
  BEGIN
    RETURN
      NEW (Writer, typescript := ts, lo := 0, cur := 0, hi := BufferSize,
           st := 0, buff := NEW (REF ARRAY OF CHAR, BufferSize),
           closed := FALSE, seekable := FALSE, buffered := FALSE);
  END NewWriter;

PROCEDURE Seek (wr: Writer; <* UNUSED *> n: CARDINAL)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    wr.flush ()
  END Seek;

PROCEDURE Flush (wr: Writer) RAISES {Thread.Alerted} =
  BEGIN
    TextPort.PutText (
      wr.typescript.tp,
      Text.FromChars (SUBARRAY (wr.buff^, 0, wr.cur - wr.lo)));
    wr.lo := wr.cur;
    wr.hi := wr.lo + NUMBER (wr.buff^);
    IF Thread.TestAlert () THEN RAISE Thread.Alerted END
  END Flush;

PROCEDURE PickAction (             fv  : FormsVBT.T;
                      <* UNUSED *> name: Text.T;
                      <* UNUSED *> cl  : REFANY;
                      <* UNUSED *> time: VBT.TimeStamp) =
  BEGIN Pick(fv)
  END PickAction;

PROCEDURE Pick (fv: FormsVBT.T) =
  VAR 
    list := views;
    browser : ListVBT.T := FormsVBT.GetVBT (fv, "procedures");
    cell: ListVBT.Cell;
  BEGIN
    IF running THEN AutoRepeat.Stop (repeater); running := FALSE; END;
    IF NOT browser.getFirstSelected (cell) THEN RETURN; END;
    WITH name = RefList.Nth (procNames, cell) DO
      WHILE list # NIL DO
        WITH view = NARROW (list.head, View) DO
          view.codeview.exitAll ();
          view.codeview.enter (name, 0);
        END;
        list := list.tail
      END;
      regions := UnionOfRegions (name, views).tail;
      currentProc := name;
    END;
  END Pick;

PROCEDURE ReparseAction (             fv  : FormsVBT.T;
                         <* UNUSED *> name: Text.T;
                         <* UNUSED *> cl  : REFANY;
                         <* UNUSED *> time: VBT.TimeStamp) =
  VAR list := views;
  BEGIN
    IF running THEN
      AutoRepeat.Stop(repeater);
      running := FALSE;
    END;
    WHILE list # NIL DO
      WITH view = NARROW(list.head, View) DO
        Wr.PutText(typescriptWr, Fmt.F("Reloading file %s ...\n",
                                       view.filename));
        TRY
          WITH new = CodeView.New(
                       FileRd.Open(view.filename), typescriptWr) DO
            Split.Replace(
              VBT.Parent(view.codeview), view.codeview, new);
            view.codeview := new;
          END;
        EXCEPT
          OSError.E =>
            Wr.PutText(
              typescriptWr,
              Fmt.F("*** OSError.E on file %s\n", view.filename));
        END;
      END;
      list := list.tail;
    END;
    WITH view = NARROW(views.head, View) DO
      procNames := view.codeview.listNames();
    END;
    StuffBrowser(fv, procNames);
    regions := NIL;
    currentProc := NIL;
  END ReparseAction;

PROCEDURE StepAction (<* UNUSED *> fv  : FormsVBT.T;
                      <* UNUSED *> name: Text.T;
                      <* UNUSED *> cl  : REFANY;
                      <* UNUSED *> time: VBT.TimeStamp) =
  BEGIN
    IF running THEN AutoRepeat.Stop (repeater); running := FALSE; END;
    IF (regions = NIL) AND (currentProc # NIL) THEN
      regions := UnionOfRegions (currentProc, views);
    END;
    IF regions # NIL THEN
      WITH region = NARROW (regions.head, REF INTEGER) DO
        At (region^, views);
      END;
      regions := regions.tail;
    END;
  END StepAction;

PROCEDURE RunAction (<* UNUSED *> fv  : FormsVBT.T;
                     <* UNUSED *> name: Text.T;
                     <* UNUSED *> cl  : REFANY;
                     <* UNUSED *> time: VBT.TimeStamp) =
  BEGIN
    IF running THEN AutoRepeat.Stop (repeater); running := FALSE; RETURN; END;
    IF (regions = NIL) AND (currentProc # NIL) THEN
      regions := UnionOfRegions (currentProc, views);
    END; 
    AutoRepeat.Start (repeater);
    running := TRUE;
  END RunAction;

PROCEDURE RepeatStep (repeater: Repeater) =
  BEGIN
    IF regions = NIL THEN
      AutoRepeat.Stop (repeater);
      running := FALSE;
    ELSE
      WITH region = NARROW (regions.head, REF INTEGER) DO
        LOCK VBT.mu DO At (region^, views); END;
      END;
      regions := regions.tail;
    END;
  END RepeatStep;

PROCEDURE ExitAction (             fv  : FormsVBT.T;
                      <* UNUSED *> name: Text.T;
                      <* UNUSED *> cl  : REFANY;
                      <* UNUSED *> time: VBT.TimeStamp) =
  BEGIN
    IF running THEN AutoRepeat.Stop (repeater); running := FALSE; END;
    Trestle.Delete (codeViews);
    Trestle.Delete (fv);
  END ExitAction;

PROCEDURE At (line: INTEGER; viewList: RefList.T) =
  BEGIN
    WHILE viewList # NIL DO
      WITH view = NARROW (viewList.head, View) DO
        view.codeview.at (line, 0);
      END;
      viewList := viewList.tail;
    END;
  END At;

PROCEDURE StuffBrowser (fv: FormsVBT.T; names: RefList.T) =
  VAR browser: ListVBT.T := FormsVBT.GetVBT (fv, "procedures");
      oldCount := browser.count();
      oldSelection := -1;
      newCount := RefList.Length(names);
  BEGIN
    EVAL browser.getFirstSelected(oldSelection);
    browser.selectNone();
    browser.removeCells (0, LAST(INTEGER));
    browser.insertCells (oldCount, newCount);
    IF newCount > 0 THEN
      FOR j := 0 TO newCount - 1 DO
        browser.setValue (j, NARROW (names.head, TEXT));
        names := names.tail;
      END;
      IF oldCount = newCount AND oldSelection # -1 THEN
        browser.selectOnly(oldSelection)
      ELSE
        browser.selectOnly(0)
      END;
      Pick(fv)
    END;
  END StuffBrowser;

PROCEDURE CheckNames (names: RefList.T; viewList: RefList.T) =
  VAR nameList: RefList.T;
  BEGIN
    WHILE viewList # NIL DO
      WITH view = NARROW(viewList.head, View) DO
        nameList := names;
        WHILE nameList # NIL DO
          WITH name = NARROW(nameList.head, TEXT) DO
            IF NOT TextListMember(
                     name, view.codeview.listNames()) THEN
              Wr.PutText(
                typescriptWr,
                Fmt.F("procedure annotation %s not in file %s\n",
                      name, view.filename));
            END;
          END;
          nameList := nameList.tail
        END
      END;
      viewList := viewList.tail
    END
  END CheckNames;

PROCEDURE TextListMember (x: TEXT; l: RefList.T): BOOLEAN =
  BEGIN
    WHILE l # NIL DO
      IF Text.Equal(NARROW(l.head, TEXT), x) THEN
        RETURN TRUE
      END;
      l := l.tail;
    END;
    RETURN FALSE;
  END TextListMember;


PROCEDURE UnionOfNames (viewList: RefList.T): RefList.T =
  VAR
    list : RefList.T;
    name : TEXT;
    value: REFANY;
    tbl  : SortedTextRefTbl.T;
  BEGIN
    WHILE viewList # NIL DO
      WITH view = NARROW(viewList.head, View) DO
        list := RefList.Append(list, view.codeview.listNames());
      END;
      viewList := viewList.tail;
    END;
    (* build a SortedTextRefTbl of unique keys: *)
    tbl := NEW(SortedTextRefTbl.Default).init();
    WHILE list # NIL DO
      name := list.head;
      EVAL tbl.put(name, NIL);
      list := list.tail;
    END;
    (* build a new list with items in sorted order: *)
    WITH iter = tbl.iterateOrdered(FALSE) DO
      WHILE iter.next(name, value) DO
        list := RefList.Cons(name, list)
      END
    END;
    RETURN list
  END UnionOfNames;

PROCEDURE UnionOfRegions (proc: TEXT; viewList: RefList.T):
  RefList.T =
  VAR
    list     : RefList.T;
    refRegion: REFANY;
    region   : INTEGER;
    tbl      : SortedIntRefTbl.T;
  BEGIN
    WHILE viewList # NIL DO
      WITH view = NARROW(viewList.head, View) DO
        list :=
          RefList.Append(list, view.codeview.listRegions(proc));
      END;
      viewList := viewList.tail;
    END;
    (* build a SortedIntRefTbl of unique keys: *)
    tbl := NEW(SortedIntRefTbl.Default).init();
    WHILE list # NIL DO
      refRegion := list.head;
      region := NARROW(refRegion, REF INTEGER)^;
      EVAL tbl.put(region, refRegion);
      list := list.tail;
    END;
    (* build a new list with items in sorted order: *)
    WITH iter = tbl.iterateOrdered(FALSE) DO
      WHILE iter.next(region, refRegion) DO
        list := RefList.Cons(refRegion, list)
      END
    END;
    RETURN list
  END UnionOfRegions;

PROCEDURE Main () =
  VAR hsplit, vsplit: HVSplit.T;
  BEGIN
    fv := NEW(FormsVBT.T).initFromRsrc (
            "RehearseCode.fv",
            Rsrc.BuildPath ("$REHEARSECODE",  RehearseCodeBundle.Get()));
    FormsVBT.AttachProc (fv, "reparse", ReparseAction);
    FormsVBT.AttachProc (fv, "step", StepAction);
    FormsVBT.AttachProc (fv, "run", RunAction);
    FormsVBT.AttachProc (fv, "exit", ExitAction);
    FormsVBT.AttachProc (fv, "procedures", PickAction);

    typescriptWr := NewWriter (FormsVBT.GetVBT (fv, "typescript"));

    IF (Params.Count < 2) OR (Params.Count > 5) THEN
      Wr.PutText (Stdio.stderr, 
        "usage: RehearseCode filename1 [... filename4]\n");
      RETURN
    END;

    FOR i := 1 TO Params.Count - 1 DO
      WITH source = Params.Get (i),
           view   = NEW (View)      DO
        TRY
          Wr.PutText (
            typescriptWr, Fmt.F ("Loading file %s ...\n", source));
          view.filename := source;
          view.codeview :=
            CodeView.New (FileRd.Open (source), typescriptWr);
          views := RefList.Cons(view, views);
          IF vsplit = NIL THEN
            vsplit := HVSplit.Cons (Axis.T.Ver, view.codeview);
          ELSE
            Split.AddChild (vsplit, HVBar.New (1.5), view.codeview);
          END;
        EXCEPT
        |  OSError.E =>
            Wr.PutText (
              Stdio.stderr,
              Fmt.F ("RehearseCode: Error trying to open file %s\n", source));
            Wr.PutText (
              typescriptWr, Fmt.F ("*** Error trying to open file %s\n", source));
        END;
      END;
    END;

    IF views = NIL THEN
      Wr.PutText (Stdio.stderr, "RehearseCode: no source files found\n");
      RETURN 
    END;

    IF hsplit = NIL THEN
      codeViews := vsplit;
    ELSE
      codeViews := hsplit;
    END;

    procNames := UnionOfNames (views);
    CheckNames (procNames, views);
    StuffBrowser (fv, procNames);
    Trestle.Install (
      codeViews, "RehearseCode", NIL, "RehearseCode Code Views");
    Trestle.Install (fv, "RehearseCode", NIL, "RehearseCode Controller");
    Trestle.AwaitDelete (fv);
  END Main;

BEGIN
  Main ();
END RehearseCode.
