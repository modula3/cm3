(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(* Last modified on Fri May 24 10:58:50 PDT 1996 by mcjones        *)

(* Edit an existing Lectern document via Lectern and BuildLectern. *)

MODULE EditLectern EXPORTS Main;

IMPORT Atom, EditLecternBundle, Env, FileRd, FileWr, Fmt, FormsVBT,
  LecternClient, LecternDoc, OSError, Pathname, Pipe, Process, Rd,
  Rsrc, SortedTextTextTbl, Stdio, Text, TextPort, TextSeq, Thread,
  Trestle, TrestleComm, VBT, VBTClass, Wr;

<*FATAL
  FormsVBT.Error, FormsVBT.Unimplemented, Rd.EndOfFile, Wr.Failure*>

PROCEDURE ToArray(s: TextSeq.T): REF ARRAY OF TEXT =
(* Return a new array containing "s.getlo()", ..., "s.gethi()". *)
  VAR r := NEW(REF ARRAY OF TEXT, s.size());
  BEGIN
    FOR i := 0 TO LAST(r^) DO
      r[i] := s.get(i)
    END;
    RETURN r
  END ToArray;

CONST LogFilePath = "EditLectern.log"; (* originally was ".EditLectern.log" *)

VAR
  deferPath: TEXT;
  main: FormsVBT.T;
  defer: BOOLEAN;
  path: TEXT := NIL;

  (* Following valid only when path # NIL: *)
  attrs: LecternDoc.Attributes := NIL;
  attrTbl, attrMod: SortedTextTextTbl.T := NIL;
  page1, image1: INTEGER; (* 1-origin page number ~ image number *)
  contents: INTEGER; (* 1-origin image number *)
  index: INTEGER; (* page1-origin page number *)
  gamma: TEXT;
  saveThread: Thread.T;

PROCEDURE Open(
  <*UNUSED*>cl: FormsVBT.Closure;
  fv: FormsVBT.T;
  <*UNUSED*> name: TEXT;
  <*UNUSED*>time: VBT.TimeStamp) =
  VAR
    p: TEXT;
    rd: Rd.T;
    dd: LecternDoc.Dir;
    g: REAL;
  BEGIN
    FormsVBT.PopDown(fv, "OpenDialog");
    TRY
      p := FormsVBT.GetText(fv, "openFile");
      rd := FileRd.Open(p);
      TRY LecternDoc.ReadDir(rd, dd) FINALLY Rd.Close(rd) END;
      path := p;

      (* Convert from origin of 0 to 1, and from page 0 to page 1. *)
      IF dd.origin >= -1 THEN
        page1 := 1;
        image1 := dd.origin + 1 + 1
      ELSE
        page1 := -dd.origin;
        image1 := 1
      END;
      FormsVBT.PutInteger(fv, "pagenumber", page1);
      FormsVBT.PutInteger(fv, "imagenumber", image1);

      (* Convert from origin of 0 to 1. *)
      contents := dd.contents + 1;
      FormsVBT.PutInteger(fv, "contents", contents);

      (* Convert from 0-origin image number to page number. *)
      FormsVBT.PutChoice(fv, "indexCh", "indexPage");
      index := dd.index - dd.origin;
      FormsVBT.PutInteger(fv, "index", index);

      (* If all the pages have the same gamma, set that as the current
         value in the dialog. Otherwise, use 1.0.  *)
      g := dd.gammas^[0];
      FOR i := 1 TO LAST(dd.gammas^) DO
        IF dd.gammas^[i] # dd.gammas^[0] THEN g := 1.0 END
      END;
      IF 0.9 <= g AND g <= 1.0 THEN
        FormsVBT.PutChoice(fv, "gammaCh", "gammaNormal")
      ELSIF 0.45 <= g AND g <= 0.46 THEN
        FormsVBT.PutChoice(fv, "gammaCh", "gammaDarker")
      ELSE
        FormsVBT.PutChoice(fv, "gammaCh", NIL)
      END;
      gamma := Fmt.Real(g, style := Fmt.Style.Auto, prec := 3);
      FormsVBT.PutText(fv, "gamma", gamma);

      attrs := dd.attributes;
      attrTbl := NEW(SortedTextTextTbl.Default).init();
      attrMod := NEW(SortedTextTextTbl.Default).init();
      FOR i := 0 TO LAST(attrs^) DO
        EVAL attrTbl.put(attrs[i].key, attrs[i].value)
      END;
      FormsVBT.PutTextProperty(
        fv, "attributes", "Items", Keys(attrTbl));
        FormsVBT.PutText(fv, "key", "");
        FormsVBT.PutText(fv, "value", "");
      FormsVBT.MakeActive(fv, "main");
      Trestle.Decorate(
        main, windowTitle := "EditLectern: " & Pathname.Last(path));
      LecternClient.Send(ARRAY OF TEXT{path})
    EXCEPT
    | OSError.E(code) => Error(Atom.ToText(code.head))
    | LecternDoc.NotLectern => Error("Not Lectern document")
    | LecternClient.Error(msg) => Error(msg)
    END
  END Open;

PROCEDURE Gamma(
    <*UNUSED*> cl: FormsVBT.Closure;
    fv: FormsVBT.T;
    name: TEXT;
    <*UNUSED*> time: VBT.TimeStamp) =
  VAR ch := FormsVBT.GetChoice(fv, name);
  BEGIN
    IF Text.Equal(ch, "gammaNormal") THEN
      FormsVBT.PutText(fv, "gamma", "1.0")
    ELSIF Text.Equal(ch, "gammaDarker") THEN
      FormsVBT.PutText(fv, "gamma", "0.454")
    END
  END Gamma;

PROCEDURE Keys(attrTbl: SortedTextTextTbl.T): TEXT =
  VAR t, k, v: TEXT;
  BEGIN
    t := "";
    WITH i = attrTbl.iterateOrdered() DO
      WHILE i.next(k, v) DO t := t & k & "\n" END
    END;
    RETURN t    
  END Keys;

PROCEDURE SelectAttribute(
    <*UNUSED*> cl: FormsVBT.Closure;
    fv: FormsVBT.T;
    <*UNUSED*> name: TEXT;
    time: VBT.TimeStamp) =
  VAR k, v: TEXT;
  BEGIN
    IF attrs # NIL THEN
      WITH n = FormsVBT.GetInteger(fv, "attributes") DO
        <*ASSERT n >= 0*>
        WITH i = attrTbl.iterateOrdered() DO
          FOR j := 0 TO n DO EVAL i.next(k, v) END
        END;
        FormsVBT.PutText(fv, "key", k);
        FormsVBT.PutText(fv, "value", v);
        TextPort.Select(
          FormsVBT.GetVBT(fv, "value"), time, replaceMode := TRUE)
      END
    END
  END SelectAttribute;

PROCEDURE SetAttribute(
    <*UNUSED*> cl: FormsVBT.Closure;
    fv: FormsVBT.T;
    <*UNUSED*> name: TEXT;
    <*UNUSED*> time: VBT.TimeStamp) =
  BEGIN
    IF attrs # NIL THEN
      WITH k = FormsVBT.GetText(fv, "key"),
           v = FormsVBT.GetText(fv, "value") DO
        EVAL attrTbl.put(k, v);
        EVAL attrMod.put(k, v);
        FormsVBT.PutTextProperty(fv, "attributes", "Items", Keys(attrTbl));
        FormsVBT.PutTextProperty(fv, "attributes", "Select", k)
      END;
    END
  END SetAttribute;

PROCEDURE DeleteAttribute(
    <*UNUSED*> cl: FormsVBT.Closure;
    fv: FormsVBT.T;
    <*UNUSED*> name: TEXT;
    <*UNUSED*> time: VBT.TimeStamp) =
  VAR v: TEXT;
  BEGIN
    IF attrs # NIL THEN
      WITH k = FormsVBT.GetText(fv, "key") DO
        EVAL attrTbl.delete(k, v);
        EVAL attrMod.delete(k, v)
      END;
      FormsVBT.PutTextProperty(fv, "attributes", "Items", Keys(attrTbl))
    END
  END DeleteAttribute;

PROCEDURE PutTs(t: TEXT) =
(* Append "t" to interactor named "typescript". *)
  BEGIN
    FormsVBT.PutText(main, "typescript", t, append := TRUE);
    VBTClass.Redisplay(FormsVBT.GetVBT(main, "typescript"))
  END PutTs;

EXCEPTION ProcessError(TEXT);

PROCEDURE LogProcess(wr: Wr.T; cmd: TEXT; READONLY params: ARRAY OF TEXT) =
  BEGIN
    Wr.PutText(wr, cmd);
    FOR i := 0 TO LAST(params) DO
      Wr.PutChar(wr, ' ');
      IF Text.FindChar(params[i], ' ') >= 0 THEN
        Wr.PutChar(wr, '\"');
        Wr.PutText(wr, params[i]);
        Wr.PutChar(wr, '\"')
      ELSE
        Wr.PutText(wr, params[i])
      END
    END;
    Wr.PutText(wr, Wr.EOL)
  END LogProcess;

PROCEDURE RunProcess(cmd: TEXT; READONLY params: ARRAY OF TEXT): BOOLEAN
  RAISES {ProcessError} =
(* Run "cmd" with "params", appending its merged stdout/stder to the
   "typescript" field.  Return "TRUE" if the process exits with a zero
   status, otherwise display an error message and return "FALSE". *)
  VAR
    hrSelf, hwChild: Pipe.T;
    p: Process.T;
    code: Process.ExitCode;
    rd: Rd.T;
  BEGIN
    IF Thread.TestAlert() THEN RETURN FALSE END;
    TRY
      (* IF debug THEN *)
      PutTs("> " & cmd);
      FOR i := 0 TO LAST(params) DO
        PutTs(" " & params[i])
      END;
      PutTs("\n");
      (* END *)
      Pipe.Open(hr := hrSelf, hw := hwChild);
      p := Process.Create(
        cmd := cmd,
        params := params,
        stdout := hwChild, stderr := hwChild);
      TRY
        TRY hwChild.close() EXCEPT OSError.E => (*SKIP*) END;
        rd := NEW(FileRd.T).init(hrSelf);
        WHILE NOT Rd.EOF(rd) DO
          PutTs(Rd.GetLine(rd) & "\n")
        END
      FINALLY
        code := Process.Wait(p);
        IF code # 0 THEN
          RAISE ProcessError(cmd & " returned status " & Fmt.Int(code))
        END
      END;
      RETURN TRUE
    EXCEPT OSError.E(code) => RAISE ProcessError(Atom.ToText(code.head))
    END
  END RunProcess;

PROCEDURE SaveMB(
    <*UNUSED*> cl: FormsVBT.Closure;
    fv: FormsVBT.T;
    <*UNUSED*> name: TEXT;
    time: VBT.TimeStamp) =
  BEGIN
    DoSave(fv, time)
  END SaveMB;

PROCEDURE PopSaveAs(
    <*UNUSED*> cl: FormsVBT.Closure;
    fv: FormsVBT.T;
    <*UNUSED*> name: TEXT;
    time: VBT.TimeStamp) =
  VAR pathDest := FormsVBT.GetText(fv, "saveAsFile");
  BEGIN
    IF path = NIL THEN RETURN END;
    IF pathDest # NIL THEN
      pathDest := Pathname.Join(
        Pathname.Prefix(pathDest), Pathname.Last(path), NIL)
    ELSE
      pathDest := path
    END;
    TRY
      FormsVBT.PutText(fv, "saveAsFile", pathDest)
    EXCEPT FormsVBT.Error =>
    END;
    FormsVBT.PopUp(fv, "SaveAsDialog", time := time);
  END PopSaveAs;

PROCEDURE SaveAs(
    <*UNUSED*> cl: FormsVBT.Closure;
    fv: FormsVBT.T;
    <*UNUSED*> name: TEXT;
    time: VBT.TimeStamp) =
  BEGIN
    FormsVBT.PopDown(fv, "SaveAsDialog");
    DoSave(fv, time, FormsVBT.GetText(fv, "saveAsFile"))
  END SaveAs;

TYPE SaveThread = Thread.Closure OBJECT
    params: TextSeq.T;
  METHODS
    init(): SaveThread := SaveInit;
    p(t: TEXT) := SaveP
  OVERRIDES
    apply := SaveApply
  END;

PROCEDURE DoSave(
    fv: FormsVBT.T;
    time: VBT.TimeStamp;
    pathNew: TEXT := NIL) =
(* Save to "pathNew", or to "path" if "pathNew = NIL". *)
(* LL.sup = VBT.mu *)
  VAR
    g, k, v: TEXT;
    i, j: INTEGER;
    th := NEW(SaveThread).init();
  BEGIN
    (* *** Make Save/Save As passive when no document open? *)
    IF path = NIL THEN RETURN END;

    defer := Text.Equal(FormsVBT.GetChoice(fv, "defer"), "deferTRUE");

    (* Construct BuildLectern parameter list in th.params: *)

    g := FormsVBT.GetText(fv, "gamma");
    IF NOT Text.Equal(g, gamma) THEN
      th.p("-gamma"); th.p(g);
      th.p("-rescale")
    ELSE
      th.p("-include")
    END;
    th.p(path); th.p("1"); th.p("9999");

    WITH i = attrMod.iterateOrdered() DO
      WHILE i.next(k, v) DO
        th.p("-" & k & ":"); th.p(v)
      END
    END;

    FOR i := 0 TO LAST(attrs^) DO
      IF NOT attrTbl.get(attrs[i].key, v) THEN
        th.p("-noattribute"); th.p(attrs[i].key)
      END
    END;

    i := FormsVBT.GetInteger(fv, "pagenumber");
    j := FormsVBT.GetInteger(fv, "imagenumber");
    IF i # page1 OR j # image1 THEN
      page1 := i;
      image1 := j;
      th.p("-page1"); th.p(Fmt.Int(image1 - page1 + 1));
    END;

    i := FormsVBT.GetInteger(fv, "contents");
    IF i # contents THEN th.p("-contents"); th.p(Fmt.Int(i)) END;

    (* Issue "-index" after "-page1", since it is relative to
       origin. But treat value of 0 specially, forcing BuildLectern
       to set dd.origin to -1. *)
    i := FormsVBT.GetInteger(fv, "index");
    IF Text.Equal(FormsVBT.GetChoice(fv, "indexCh"), "indexImage") THEN
      i := i + page1 - image1
    END;
    IF i # index THEN
      th.p("-index");
      IF i # 0 THEN th.p(Fmt.Int(i))
      ELSE th.p(Fmt.Int(i - (image1 - page1 + 1) + 1 + 1 - 1))
      END
    END;

    IF pathNew = NIL THEN pathNew := path END;
    th.p(pathNew); (* destination *)

    DoClose(fv);
 
    FormsVBT.MakePassive(fv, "zbackground");
    saveThread := Thread.Fork(th);
    FormsVBT.PopUp(fv, "CancelDialog", time := time);
  END DoSave;

PROCEDURE SaveInit(self: SaveThread): SaveThread =
  BEGIN
    self.params := NEW(TextSeq.T).init();
    RETURN self
  END SaveInit;

PROCEDURE SaveP(self: SaveThread; t: TEXT) =
  BEGIN
    self.params.addhi(t)
  END SaveP;

PROCEDURE SaveApply(self: SaveThread): REFANY =
(* LL.sup < VBT.mu *)
  VAR wr: Wr.T; params: REF ARRAY OF TEXT;
  BEGIN
    TRY
      TRY
        wr := FileWr.OpenAppend(deferPath);
        params := ToArray(self.params);
        LogProcess(wr, "BuildLectern", params^);
        IF NOT defer THEN
          TRY
            EVAL RunProcess("BuildLectern", params^)
          EXCEPT
           ProcessError(msg) => LOCK VBT.mu DO Error(msg) END
          END
        END;
        Wr.Flush(wr)
      EXCEPT OSError.E(code) =>
        LOCK VBT.mu DO Error(Atom.ToText(code.head)) END
      END
    FINALLY
      LOCK VBT.mu DO
        FormsVBT.PopDown(main, "CancelDialog");
        FormsVBT.MakeActive(main, "zbackground");
        saveThread := NIL
      END
    END;
    RETURN NIL
  END SaveApply;

PROCEDURE Cancel(
    <*UNUSED*> cl: FormsVBT.Closure;
    <*UNUSED*>fv: FormsVBT.T;
    <*UNUSED*> name: TEXT;
    <*UNUSED*> time: VBT.TimeStamp) =
  BEGIN
    IF saveThread # NIL THEN
      (* Signal running process, if any! *)
      Thread.Alert(saveThread)
    END
  END Cancel;

PROCEDURE DoClose(fv: FormsVBT.T) =
  (* LL.sup = VBT.mu *)
  BEGIN
    (* *** Check for pending edits? *)
    path := NIL;
    Trestle.Decorate(main, windowTitle := "EditLectern");
    FormsVBT.MakeVanish(fv, "main")
  END DoClose;

PROCEDURE CloseMB(
    <*UNUSED*> cl: FormsVBT.Closure;
    fv: FormsVBT.T;
    <*UNUSED*> name: TEXT;
    <*UNUSED*> time: VBT.TimeStamp) =
  BEGIN
    DoClose(fv)
  END CloseMB;

PROCEDURE QuitMB(
    <*UNUSED*> cl: FormsVBT.Closure;
    fv: FormsVBT.T;
    <*UNUSED*> name: TEXT;
    <*UNUSED*> time: VBT.TimeStamp) =
  BEGIN
    Trestle.Delete(fv)
  END QuitMB;

PROCEDURE Error(msg: TEXT) =
  (* LL.sup = VBT.mu *)
  BEGIN
    FormsVBT.PutText(main, "errorMsg", msg);
    FormsVBT.PopUp(main, "ErrorDialog")
  END Error;

<*FATAL Rd.Failure, Thread.Alerted, TrestleComm.Failure*>
BEGIN
  TRY
    deferPath := Env.Get("HOME");
    IF deferPath = NIL THEN
      deferPath := Process.GetWorkingDirectory();
      Wr.PutText(Stdio.stderr,
        "EditLectern: $HOME not defined; assuming " & deferPath & "\n")
    END;
    deferPath := Pathname.Join(deferPath, LogFilePath, NIL);
    main := NEW(FormsVBT.T).initFromRsrc(
      "EditLectern.fv",
      Rsrc.BuildPath("$EditLecternPATH", EditLecternBundle.Get()));

    defer := FALSE;

    WITH cl = NEW(FormsVBT.Closure, apply := Open) DO
      FormsVBT.Attach(main, "open", cl);
      FormsVBT.Attach(main, "openFile", cl)
    END;

    FormsVBT.Attach(main, "gammaCh", NEW(FormsVBT.Closure, apply := Gamma));

    FormsVBT.Attach(
      main, "attributes", NEW(FormsVBT.Closure, apply := SelectAttribute));
    FormsVBT.AttachEditOps(main, "key");
    FormsVBT.AttachEditOps(main, "value");
    FormsVBT.Attach(
      main, "attrSet", NEW(FormsVBT.Closure, apply := SetAttribute));
    FormsVBT.Attach(
      main, "attrDelete", NEW(FormsVBT.Closure, apply := DeleteAttribute));

    FormsVBT.Attach(main, "closeMB", NEW(FormsVBT.Closure, apply := CloseMB));
    FormsVBT.Attach(main, "quitMB", NEW(FormsVBT.Closure, apply := QuitMB));
    FormsVBT.Attach(main, "saveMB", NEW(FormsVBT.Closure, apply := SaveMB));
    FormsVBT.Attach(
      main, "saveAsMB", NEW(FormsVBT.Closure, apply := PopSaveAs));
    WITH cl = NEW(FormsVBT.Closure, apply := SaveAs) DO
      FormsVBT.Attach(main, "saveAs", cl);
      FormsVBT.Attach(main, "saveAsFile", cl)
    END;

    FormsVBT.Attach(main, "cancel", NEW(FormsVBT.Closure, apply := Cancel));

    Trestle.Install(main, windowTitle := "EditLectern");
    Trestle.AwaitDelete(main)
  EXCEPT
  | OSError.E(code) =>
      Wr.PutText(
        Stdio.stderr,
        "EditLectern: OS error: " & Atom.ToText(code.head) & "\n");
      Process.Exit(1)
  | FormsVBT.Error(msg) =>
      Wr.PutText(Stdio.stderr, "EditLectern: FormsVBT error: " & msg & "\n");
      Process.Exit(1)
  | Rsrc.NotFound =>
      Wr.PutText(Stdio.stderr, "EditLectern: Rsrc.NotFound\n");
      Process.Exit(1)
  END
END EditLectern.
