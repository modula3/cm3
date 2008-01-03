(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb  4 11:42:43 PST 2000 by heydon                   *)
(*      modified on Wed Aug 27 17:38:48 PDT 1997 by leifer                   *)
(*      modified on Thu Nov 30 13:56:40 PST 1995 by gnelson                  *)
(*      modified on Sat Aug 22 23:32:17 PDT 1992 by myers                    *)
<* PRAGMA LL *>

MODULE Juno EXPORTS Main;

IMPORT Source, Drawing, Editor, EditorUI, SaveState, PublicView, CurrCmd;
IMPORT   Marquee, View, JunoError, JunoWM, ToolBox, DblBufferVBT, Drag;
IMPORT   JunoUIImpl, PrintImpl, PSImpl, RandomImpl, TextImpl, TimeImpl;
IMPORT   UnitImpl;
IMPORT   FVFilter, JunoConfig, JunoRsrc, JunoVersion, JunoZeus;
IMPORT BuiltInSlots, JunoAST, JunoASTUtils, JunoCompile, JunoCompileErr;
IMPORT   JunoScope, JunoUnparse, JunoLex, JunoParse;
IMPORT JunoRT;
IMPORT FormsVBT, TextEditVBT, TextPort, ListVBT;
IMPORT Trestle, TrestleComm, VBT, VBTClass, Filter, Split, HVSplit, ZSplit;
IMPORT   Axis, PackSplit, PaintOp, TextVBT, Rect, Point, Font;
IMPORT FS, RegularFile, Pathname, FileRd, FileWr, Pipe, Process, OSError, Lex;
IMPORT   Rd, Wr, Stdio, Text, TextRd, TextWr, Thread, Atom, Rsrc, Fmt, Params;
IMPORT   NetObj, Pickle, TextList, TextListSort, Env;

<* FATAL Wr.Failure, Rd.Failure, Thread.Alerted *>
<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

TYPE
  SyncStat = { NotDone, Save, Discard, Cancel };

  Window = FormsVBT.T OBJECT
    root: View.Root;
    scope: JunoScope.T;
    toolbox: ToolBox.T;
    builtInSize: CARDINAL;
    fileName: TEXT;
    fileStale: BOOLEAN;
    checkpointStale: BOOLEAN;
    saveAsFName: TEXT;

    mu: MUTEX;
    untilDone: Thread.Condition;
    stat: SyncStat
  METHODS
    init(d: Drawing.T): Window := NewForm
    <* LL.sup < VBT.mu *>
    (* Initialize the window on drawing "d". *)
  OVERRIDES
    misc := Misc
  END;

(* If "w: Window", then "w.fileName" is the complete absolute pathname 
   of the current module file, and "w.fileStale" iff the disk file 
   is out-of-date with respect to the contents of the current module 
   editor.  The value of "checkPointStale" is "TRUE" if the checkpoint file 
   differs from the state of the editor and source view. *) 

EXCEPTION OpenFailure;
(* An attempt was made to open something other than a file *)

(* ========================= Compilation Procedures ======================== *)

PROCEDURE MakeCurrMod(w: Window; time: VBT.TimeStamp): BOOLEAN =
(* If the current module is out-of-date, recompile it and install its
   definitions in the top-level scope. If it changed, update the toolbox. On
   return, "w.root.eTrue" will be "TRUE" iff the module compiled successfully.
   Return "TRUE" iff the current module is up-to-date. *)
  <* LL.sup < w.root.editor *>
  VAR dummy: JunoAST.Id; mod: JunoScope.Mod; BEGIN
    IF w.root.eTrue THEN RETURN TRUE END;
    IF CompileEditor(w.root, w.root.editor, time, w.scope, dummy, mod) THEN
      w.root.eTrue := TRUE;
      CurrCmd.SetScope(w.root.ccmd, mod.public_scp);
      ToolBox.Update(Split.Succ(w.toolbox, NIL), w.root.editor,
    	w.root, w.builtInSize, anon := TRUE)
    END;
    RETURN w.root.eTrue
  END MakeCurrMod;

PROCEDURE MakeCurrCmd(w: Window; time: VBT.TimeStamp; skipify: BOOLEAN) =
(* Parse, compile, run, and pretty-print the current command. Also, if the
   current command compiled successfully, disable the "Solve" button. Requires
   that the current module is up-to-date, i.e., that "w.root.eTrue". *)
  BEGIN
    <* ASSERT w.root.eTrue OR CurrCmd.GetAST(w.root.ccmd) = JunoAST.SkipVal *>
    IF Source.Make(w.root.source, time, skipify) THEN
      (* disable the "Solve" button *)
      FormsVBT.MakeDormant(w, SolveButton)
    END
  END MakeCurrCmd;

PROCEDURE CompileModAndCmd(w: Window; ts: VBT.TimeStamp): BOOLEAN =
(* Compile the current module (if necessary) and then the current command (if
   necessary), unparsing the current command if successful. If either of the
   compilations fails, highlight the error using timestamp "ts" in the
   appropriate window. If compiling the current module was successful, update
   the toolbox if necessary. Return "TRUE" iff both compilations were
   successful. *)
  BEGIN
    RETURN MakeCurrMod(w, ts) AND
      Source.Compile(w.root.source, ts, skipify := FALSE)
  END CompileModAndCmd;

PROCEDURE MakeModAndCmd(w: Window; ts: VBT.TimeStamp; skipify: BOOLEAN) =
(* Compile the current module and the current command, and run the current
   command, replacing its body by "SKIP" if "skipify" is TRUE. If the current
   module changed, update the toolbox. If there was an error compiling the
   current module, don't update the toolbox or compile and run the current
   command. *)
  BEGIN
    IF NOT MakeCurrMod(w, ts) THEN RETURN END;
    MakeCurrCmd(w, ts, skipify)
  END MakeModAndCmd;

(* ======================== Module Editor Procedures ======================= *)

CONST NoTimeStamp: VBT.TimeStamp = 0;

PROCEDURE CompileEditor(
    rt: View.Root; 
    ed: Editor.T; 
    time: VBT.TimeStamp;
    scp: JunoScope.T; 
    VAR (*OUT*) modName: JunoAST.Id;
    VAR (*OUT*) entity: JunoScope.Mod;
    uniqueModName := TRUE): BOOLEAN =
(* Parse (if necessary), compile, and unparse the the editor "ed", installing
   buttons for the UI declarations in "rt", setting "modName" to the name
   of the current module (if any, or to "NIL" if none), and setting "entity"
   to a module entity whose "public" and "scp" scopes have parent scope "scp"
   and that contain bindings for public and all declarations, respectively.

   Returns "TRUE" if there was no error in the compilation. If there was an
   error, it is displayed using time "time". *)
  BEGIN
    IF
      Editor.Compile(ed, time, scp, modName, entity, uniqueModName) AND
      EditorUI.CompileUI(rt, ed, time, entity.scp)
    THEN
      Editor.Unparse(ed);
      RETURN TRUE
    END;
    RETURN FALSE
  END CompileEditor;

PROCEDURE CompileModule(w: Window; mod: TEXT; rd: Rd.T; augment := FALSE):
  BOOLEAN =
(* Create a new "Editor.T" on the module named "mod" whose contents are read
   from "rd", compile it, bind its module scope into "w.scope", and install it
   in the root list of modules, and (if there are no errors) return "TRUE".
   If there are errors, return "FALSE".

   Let "scp" denote the scope containing the definitions in "mod". If "augment
   = FALSE", then "mod" must be unbound in "w.scope". In this case, "mod" is
   bound to "scp" in "w.scope". Otherwise, "mod" is bound to some scope
   "oldScp" in "w.scope". In this case, any binding "(name, entity)" in "scp"
   for which "name" is *not* bound in "oldScp" is added to "oldScp". As the
   name implies, this is used to augment the scope for a built-in module
   like "PS" with the names of constants, global variables, etc. defined in a
   ".juno" file. *)
  <* LL.sup < VBT.mu *>
  <* FATAL JunoScope.NameClash *>
  VAR
    src := Rd.GetText(rd, LAST(CARDINAL));
    ed := NEW(Editor.T).init(src, readOnly := TRUE);
    modNm: JunoAST.Id; modScp: JunoScope.Mod;
    success: BOOLEAN;
    ent: JunoScope.Mod;
  BEGIN
    (* Compile the new module *)
    success := CompileEditor(w.root, ed, NoTimeStamp, w.scope, modNm, modScp,
      uniqueModName := NOT augment);
    IF NOT success THEN RETURN FALSE END;
    IF modNm = NIL THEN
      LOCK VBT.mu DO
        JunoError.Display(startup, "Missing MODULE header in module \""
          & mod & "\"")
        (* Must use "startup" since "w" is not yet connected to the
           window system. *)
      END;
      RETURN FALSE
    END;
    ent := JunoScope.Lookup(w.scope, modNm, localOnly := TRUE);
    <* ASSERT augment = (ent # NIL) *>
    IF ent = NIL THEN
      JunoScope.Bind(w.scope, modNm, modScp)
    ELSE
      (* augment the existing scope with the public names *)
      VAR names := JunoScope.Names(modScp.public_scp); BEGIN
        FOR i := 0 TO LAST(names^) DO
          IF JunoScope.Lookup(ent.scp, names[i]) = NIL THEN
            VAR e := JunoScope.Lookup(modScp.public_scp, names[i]); BEGIN
              <* ASSERT e # NIL AND ent.public_scp = ent.scp *>
              JunoScope.Bind(ent.public_scp, names[i], e)
            END
          END
        END
      END
    END;
    (* Add the module to the list of modules *)
    w.root.modules := NEW(View.EditorList, view := ed,
      form := NIL, mod := mod, next := w.root.modules);
    RETURN TRUE
  END CompileModule;

PROCEDURE CompileModules(
    w: Window;
    rd: Rd.T;
    VAR (*INOUT*) modList: TextList.T;
    fromRsrc := FALSE) =
(* Read the modules named on each line of the stream "rd", and compile each
   one via a call to "CompileModule". Blank lines and lines beginning with "%"
   in "rd" are ignored. A line containing "EOF" terminates reading.

   Each other line should contain the filename of a Juno module file. For
   each, a module name is formed from the filename by stripping the extension
   from the last component of the filename. These module names are prepended
   to the list "modList". If the filename is followed by the text
   "(builtin)", then the scope formed from the module is used to *augment* an
   existing built-in scope. By default, the module files are read from the
   file system. If "fromRsrc" is true, then the files are opened as resources
   via calls to "RsrcOpen".

   For each named file, the corresponding module name is shown in the
   "initModule" text field of the "startup" screen.

   If an error occurs in any file, then "w.fileName" is changed to the 
   name of the file containing the error, and the rest of the files 
   are ignored. In these error cases, the name of the offending module
   is not appended to "modList".
*)
  <* FATAL Rd.EndOfFile *>
  CONST CommentChar = '%'; BuiltinText = "(builtin)";
  VAR
    ln, fileName, modName: TEXT;
    fileRd: Rd.T;
    lnRd := NEW(TextRd.T);
    builtin: BOOLEAN; (* module name followed by BuiltinText? *)
  BEGIN
    TRY
      WHILE NOT Rd.EOF(rd) DO
    	ln := Rd.GetLine(rd);
        IF Text.Length(ln) > 0 AND Text.GetChar(ln, 0) # CommentChar THEN
          IF Text.Equal(ln, "EOF") THEN EXIT END;
          EVAL lnRd.init(ln);
          Lex.Skip(lnRd);
          fileName := Lex.Scan(lnRd);
          modName := Pathname.LastBase(fileName);
          builtin := FALSE;
          IF NOT Rd.EOF(lnRd) THEN
            Lex.Skip(lnRd);
            IF Text.Equal(Lex.Scan(lnRd), BuiltinText) THEN
              builtin := TRUE
            END
          END;
          FormsVBT.PutText(startup, "initModule", modName);
          IF fromRsrc THEN
            TRY 
              fileRd := RsrcOpen(fileName);
              IF NOT CompileModule(w, modName, fileRd, augment := builtin) THEN
                Wr.PutText(Stdio.stderr, "Compiler error in bundled module "
                  & fileName & "\n");
                Wr.Flush(Stdio.stderr);
                RETURN
              END
            EXCEPT Rsrc.NotFound =>
              Wr.PutText(Stdio.stderr, "Error: unable to find \"");
              Wr.PutText(Stdio.stderr, fileName & "\" resource\n");
              Wr.Flush(Stdio.stderr);
              RETURN
            END
          ELSE
            TRY 
              fileRd := FileRd.Open(fileName);
              IF NOT CompileModule(w, modName, fileRd, augment := builtin) THEN
                w.fileName := fileName;
                RETURN
              END
            EXCEPT OSError.E =>
                Wr.PutText(Stdio.stderr, "Error: unable to open file \"");
                Wr.PutText(Stdio.stderr, fileName & "\"\n");
                Wr.Flush(Stdio.stderr);
                RETURN
            END
          END;
          (* only add "modName" to the list of modulese *after*
             it has been successfully compiled *)
          modList := TextList.Cons(modName, modList);
        END
      END
    EXCEPT
      Rd.Failure =>
        Wr.PutText(Stdio.stderr, "Fatal error: Rd.Failure on config file\n");
        Wr.Flush(Stdio.stderr)
    END
  END CompileModules;

PROCEDURE OpenModule(w: Window; mod: TEXT; hasPrivate := TRUE; show := TRUE) =
  <* LL.sup = VBT.mu *>
  VAR p := FindModule(w.root.modules, mod); BEGIN
    <* ASSERT p # NIL *>
    IF p.form = NIL THEN
      p.form := NewToolbox(w, p.view, hasPrivate := hasPrivate)
    END;
    IF show AND VBT.Parent(p.form) = NIL THEN
      Split.AddChild(w.toolbox, p.form)
    END
  END OpenModule;

PROCEDURE FindModule(curr: View.EditorList; mod: TEXT): View.EditorList =
(* Search for the module named "mod" in the list "curr"; return "NIL" if no
   such module appears in the list. *)
  BEGIN
    WHILE curr # NIL AND NOT Text.Equal(curr.mod, mod) DO
      curr := curr.next
    END;
    RETURN curr
  END FindModule;

TYPE ToolType = { Point, Templ, None };

PROCEDURE CodeDeclName(decl: JunoAST.Decl): JunoAST.Id =
(* Return the name of the predicate, function, or procedure declaration
   "decl". It is a checked run-time error if "decl" is not one of these
   three types. *)
  VAR hdr: JunoAST.Header; BEGIN
    TYPECASE decl OF <* NOWARN *>
      JunoAST.PredDecl (pd) => hdr := pd.header
    | JunoAST.FuncDecl (fd) => hdr := fd.header
    | JunoAST.ProcDecl (pd) => hdr := pd.header
    END;
    RETURN hdr.name
  END CodeDeclName;

PROCEDURE NewUIDecl(toolType: ToolType; nm: JunoAST.Id): JunoAST.UIDecl =
(* Return a new PointTool or Template UI declaration whose type is determined
   from "toolType" and the name of whose argument is "nm". It is a checked
   run-time error for "toolType" to be other than "Point" or "Templ". *)
  VAR res := NEW(JunoAST.UIDecl, bp := JunoAST.End); BEGIN
    res.args := JunoASTUtils.NewExprList(
      JunoASTUtils.QIdFromId(nm), bp := JunoAST.End);
    CASE toolType OF <* NOWARN *>
      ToolType.Point => res.name := Editor.PointToolSym
    | ToolType.Templ => res.name := Editor.TemplToolSym
    END;
    RETURN res
  END NewUIDecl;

PROCEDURE InstallDecl(w: Window; decl: JunoAST.Decl; toolType: ToolType):
  BOOLEAN =
(* Install the top-level procedure, predicate, or function declaration "decl"
   in "w.root.editor", and add a PointTool or Template UI declaration as well
   according to "toolType". The new declaration is compiled and installed in
   the current-command scope. The booleans in "w.root" are also set
   appropriately.

   If a declaration with the same name as "decl" is already defined in the
   current-command scope, and error message is displayed, and no action is
   taken. Returns TRUE iff the declaration was installed successfully. *)
  <* LL.sup = VBT.mu *>
  VAR
    declName := CodeDeclName(decl);
    scp := CurrCmd.GetScope(w.root.ccmd);
  BEGIN
    IF JunoScope.Lookup(scp, declName) # NIL THEN
      JunoError.Display(w.root.editor,
        "\"" & Atom.ToText(declName) & "\" is already defined");
      RETURN FALSE
    END;
    BindDecl(scp, declName, decl);
    WITH ed = w.root.editor DO
      Editor.AddTree(ed, decl);
      IF toolType # ToolType.None THEN
        Editor.AddTree(ed, NewUIDecl(toolType, declName));
        ToolBox.Update(Split.Succ(w.toolbox, NIL), ed, w.root,
          w.builtInSize, anon := TRUE)
      END
    END;
    w.root.astTrue := TRUE;
    w.root.sTrue := FALSE;
    w.root.dTrue := FALSE;
    SetModuleState(w, ModState.Stale);
    RETURN TRUE
  END InstallDecl;

PROCEDURE BadFoldArg(w: Window; id: JunoAST.Id) =
(* Display an error to "w" as a result of the "CurrCmd.BadFoldArg" exception
   with argument "id". *)
  BEGIN
    JunoError.Display(w.root.source,
      "The current command\nhas no local variable\nnamed \""
      & Atom.ToText(id) & "\"")
  END BadFoldArg;

PROCEDURE DoFold(
    w: Window;
    ts: VBT.TimeStamp;
    hdr: JunoAST.PredHeader;
    foldKind := CurrCmd.FoldKind.Proc;
    toolType := ToolType.None) =
(* Requires that the current module and current command are both
   up-to-date. *)
  <* LL.sup = VBT.mu *>
  VAR decl: JunoAST.Decl; BEGIN
    TRY
      decl := CurrCmd.FoldByHeader(w.root.ccmd, hdr, foldKind);
      IF InstallDecl(w, decl, toolType) THEN
    	CurrCmd.ChangeAST(w.root.ccmd, JunoAST.SkipVal);
    	MakeCurrCmd(w, ts, skipify := FALSE)
      END
    EXCEPT
      CurrCmd.BadFoldArg (id) => BadFoldArg(w, id)
    END
  END DoFold;

PROCEDURE DoFoldAsAnim(w: Window; ts: VBT.TimeStamp;
  hdr: JunoAST.PredHeader; sliderPts: JunoAST.IdList) =
(* Requires that the current module and current command are both
   up-to-date. *)
  <* LL.sup = VBT.mu *>
  VAR
    ccmd := w.root.ccmd;
    noError: BOOLEAN;
    procDecl, frameDecl, animDecl: JunoAST.ProcDecl;
    animCmd: JunoAST.Cmd;
  BEGIN
    TRY
      procDecl := CurrCmd.FoldAnim(ccmd, hdr, sliderPts);
      frameDecl := CurrCmd.FoldAnimFrame(ccmd, hdr, sliderPts);
      animDecl := CurrCmd.FoldAnimCreator(ccmd, hdr, frameDecl.header.name);
      animCmd := CurrCmd.FoldAnimCmd(ccmd, hdr.ins, animDecl.header.name);
      IF InstallDecl(w, procDecl, ToolType.Point) THEN
    	noError := InstallDecl(w, frameDecl, ToolType.None);
    	noError := noError AND InstallDecl(w, animDecl, ToolType.None);
    	<* ASSERT noError *>
    	CurrCmd.ChangeAST(ccmd, animCmd);
    	MakeCurrCmd(w, ts, skipify := TRUE)
      END
    EXCEPT
      CurrCmd.BadFoldArg (id) => BadFoldArg(w, id)
    END
  END DoFoldAsAnim;

PROCEDURE BindDecl(scp: JunoScope.T; nm: JunoAST.Id; decl: JunoAST.Decl) =
(* Bind "nm" to a scope entity formed from the predicate or procedure
   declaration "decl" in "scp", and then compile "decl" and install the
   resulting byte-code in the global code table. Requires that "nm" is not
   already bound in "scp", and that no two formals in "decl" have the same
   name. *)
  <* FATAL JunoCompileErr.Error, JunoScope.NameClash *>
  VAR codeEnt: JunoScope.Code; BEGIN
    TYPECASE decl OF <* NOWARN *>
      JunoAST.PredDecl (p) =>
        VAR predEnt := JunoScope.NewPred(p, mod := NIL); BEGIN
          JunoCompile.PredDecl(p.header.name, predEnt, scp);
          codeEnt := predEnt
        END
    | JunoAST.ProcDecl (p) =>
        VAR procEnt := JunoScope.NewProc(p, mod := NIL); BEGIN
          EVAL JunoCompile.ProcDecl(p.header.name, procEnt, scp);
          codeEnt := procEnt
        END
    END;
    JunoScope.Bind(scp, nm, codeEnt)
  END BindDecl;

(* =========================== I/O Procedures ============================== *)

PROCEDURE WritePostScriptToFile(w: Window; fname: TEXT; showPage := TRUE) =
<* LL.sup < w *>
(* Write PostScript for the current command associated with "w" to the file
   named "fname". If there is an error, display an error message in "w". *)
  BEGIN
    TRY 
      WritePostScriptToWr(w, FileWr.Open(fname), showPage)
    EXCEPT OSError.E, Wr.Failure =>
      JunoError.Display(w, "Error opening/writing file:\n\""
        & fname & "\"")
    END
  END WritePostScriptToFile;

PROCEDURE PipePostScriptToCmd(
    w: Window;
    cmd: TEXT;
    READONLY args: ARRAY OF TEXT;
    showPage := TRUE) =
(* Run the command "cmd" with arguments "args", piping the PostScript code
   produced by running the current command of the window "fv" to the standard
   input of the new process. If "showPage" is "TRUE", then a final "showpage"
   command is appended to the PostScript; this is usually necessary for
   printing, but not for previewing. *)
  VAR hwSelf, hrChild: Pipe.T; p: Process.T; wr: Wr.T; BEGIN
    TRY
      Pipe.Open((*OUT*) hrChild, (*OUT*) hwSelf);
      p := Process.Create(cmd, args, stdin := hrChild);
      TRY hrChild.close() EXCEPT OSError.E => (*SKIP*) END;
      wr := NEW(FileWr.T).init(hwSelf);
      WritePostScriptToWr(w, wr, showPage);
      EVAL Process.Wait(p)
    EXCEPT OSError.E, Wr.Failure =>
      VAR cmdLine := cmd; BEGIN
        FOR i := 0 TO LAST(args) DO cmdLine := cmdLine & " " & args[i] END;
        JunoError.Display(w, "Error forking process:\n\"" & cmdLine & "\"")
      END
    END
  END PipePostScriptToCmd;

PROCEDURE WritePostScriptToWr(w: Window; wr: Wr.T; showPage: BOOLEAN)
  RAISES { Wr.Failure } =
(* Write the PostScript for "w.root.ccmd" to "wr"; send a final "showpage"
   command iff "showPage" is TRUE. Display any error messages on "w". Close
   "wr" when done. *)
  BEGIN
    psImpl.startToFile(wr);
    TRY
      psImpl.prologue();
      PSImpl.Reset(w.root.drawing, inExec := FALSE);
      TRY EVAL CurrCmd.Run(w.root.ccmd, skipify := FALSE) EXCEPT
        CurrCmd.CompileError (errorMsg) =>
          JunoError.Display(w, errorMsg)
      | CurrCmd.RuntimeError (errorRec) =>
          JunoError.Display(w, errorRec.errorMsg);
          JunoError.DisplayPS(wr, errorRec.errorMsg)
      END;
      psImpl.epilogue(showPage := showPage)
    FINALLY
      psImpl.endToFile();
      Wr.Close(wr)
    END
  END WritePostScriptToWr;

PROCEDURE GetFileName(fv: FormsVBT.T; browserName: TEXT): TEXT =
(* Returns the name of the selected file in the FileBrowser named
   "browserName" in the form "w". If this this file names a directory, the
   directory name is set back into the FileBrowser and "NIL" is returned. *)
  <* LL.sup = VBT.mu *>
  VAR fname := FormsVBT.GetText(fv, browserName); BEGIN
    TRY
      IF FS.Status(fname).type = FS.DirectoryFileType THEN
    	FormsVBT.PutText(fv, browserName, fname); RETURN NIL
      END
    EXCEPT
      OSError.E => (* SKIP - file does not exist; it will be created *)
    END;
    RETURN fname
  END GetFileName;

PROCEDURE SaveCurrMod(w: Window; ts: VBT.TimeStamp): BOOLEAN =
(* If "w.fileStale", then pop up a dialogue asking the user if the current
   file should be saved. If the user chooses "Cancel", return FALSE
   immediately; if the user chooses "Discard", then return TRUE immediately.
   If the user chooses "Save", then attempt to save the current module under
   the name "w.fileName"; return TRUE iff the save was successful. *)
  VAR stat: SyncStat; BEGIN
    IF w.fileStale THEN
      LOCK w.mu DO w.stat := SyncStat.NotDone END;
      FormsVBT.PutText(w, "saveName", Pathname.Last(w.fileName));
      FormsVBT.PopUp(w, "saveChangesWindow", time := ts);
      LOCK w.mu DO
        WHILE w.stat = SyncStat.NotDone DO
          Thread.Wait(w.mu, w.untilDone)
        END;
        stat := w.stat
      END;
      FormsVBT.PopDown(w, "saveChangesWindow");
      IF stat = SyncStat.Cancel THEN 
        RETURN FALSE 
      ELSIF stat = SyncStat.Discard THEN
        TRY FS.DeleteFile(CheckpointName(w.fileName)) EXCEPT
          OSError.E => (*SKIP*)
        END;
        RETURN TRUE
      ELSIF stat = SyncStat.Save THEN
        RETURN PutFile(w, w.fileName, ts)
      END
    END;
    RETURN TRUE
  END SaveCurrMod;

PROCEDURE GetFile(w: Window; name: TEXT; time: VBT.TimeStamp)
  RAISES {OSError.E, OpenFailure} = 
(* Opens the file named "name", or creates it if the file does not exist.
   Then reads the contents of this file into "w"'s editor, and makes
   those new contents. Raises "OSError.E" if the file cannot be opened for
   reading or created. This procedure requires that "name # NIL" and that
   "name" does not name a directory. *)
  <* LL.sup = VBT.mu *>
  VAR txt: TEXT; type: Atom.T; create := FALSE; BEGIN
    <* ASSERT name # NIL *>
    (* Read the text from the file *)
    TRY
      type := FS.Status(name).type;
      IF type # RegularFile.FileType THEN RAISE OpenFailure END
    EXCEPT OSError.E => create := TRUE
    END;
    IF create THEN
      (* file does not exist -- create a new empty file *)
      FS.OpenFile(name, truncate := FALSE).close();
      txt := ""
    ELSE
      VAR rd: FileRd.T; BEGIN
        rd := NEW(FileRd.T).init(FS.OpenFileReadonly(name));
        txt := Rd.GetText(rd, LAST(CARDINAL));
        Rd.Close(rd) (* this closes underlying File.T *)
      END
    END;

    (* Install the text and compile the current module *)
    FormsVBT.PutText(w, "currFile", Pathname.Last(name));
    TextPort.SetText(w.root.editor, txt);
    w.root.eTrue := FALSE;
    VAR cmd: JunoAST.Cmd := NIL; ed := w.root.editor; nm: JunoAST.Id; BEGIN
      IF MakeCurrMod(w, time) THEN
        (* Pop the current command *)
        cmd := Editor.PopCurrCmd(ed, nm)
      END;
      IF cmd # NIL THEN
        UnbindDecl(CurrCmd.GetScope(w.root.ccmd), nm);
        ToolBox.Update(Split.Succ(w.toolbox, NIL), ed, w.root,
    	  w.builtInSize, anon := TRUE)
      ELSE
        (* Make or Pop failed; set curr cmd to "SKIP" *)
        cmd := JunoAST.SkipVal
      END;
      CurrCmd.ChangeAST(w.root.ccmd, cmd)
    END;
    w.root.astTrue := TRUE;
    w.root.sTrue := FALSE;
    w.root.dTrue := FALSE;
    MakeCurrCmd(w, time, w.root.skipify);
    SetModuleState(w, ModState.UpToDate)
  END GetFile;

PROCEDURE PutFile(w: Window; name: TEXT; ts: VBT.TimeStamp): BOOLEAN =
(* Write the contents of "w"'s editor window and a pushed version of the
   current command to the file named "name". Before pushing, both the current
   module and command are compiled.

   Returns TRUE only in the event of success; in this case, the background of
   the name is set to light grey, and "w.fileStale" is set to FALSE. *)
  <* LL.sup = VBT.mu *>
  VAR wr: Wr.T; proc: JunoAST.Decl := NIL; BEGIN
    IF NOT CompileModAndCmd(w, ts) THEN RETURN FALSE END;
    w.root.source.update(); (* unparse compiled current command *)
    IF CurrCmd.GetAST(w.root.ccmd) # JunoAST.SkipVal
       OR Editor.NextCmdNum(w.root.editor) > 0 THEN
      proc := CurrCmd.FoldByName(w.root.ccmd,
        Editor.NextCmdName(w.root.editor),
        kind := CurrCmd.FoldKind.ProcNoArgs)
    END;
    TRY
      wr := FileWr.Open(name);
      IF wr # NIL THEN 
	Wr.PutText(wr, TextPort.GetText(w.root.editor));
        IF proc # NIL THEN
          JunoUnparse.Block(wr, proc, tokens := LAST(INTEGER),
            prec := JunoConfig.realPrec);
          Wr.PutChar(wr, '\n')
        END;
	Wr.Close(wr);
      END
    EXCEPT
      OSError.E =>
        JunoError.Display(w, "Could not open file for writing:\n" & name);
        RETURN FALSE
    | Wr.Failure =>
        JunoError.Display(w, "Error writing file:\n" & name);
        RETURN FALSE
    END;
    SetModuleState(w, ModState.UpToDate);
    TextPort.SetModified(w.root.editor, FALSE);
    VAR te := NARROW(Filter.Child(w.root.source), TextEditVBT.T); BEGIN
      TextPort.SetModified(te.tp, FALSE)
    END;
    TRY FS.DeleteFile(CheckpointName(name)) EXCEPT OSError.E => (*SKIP*) END;
    RETURN TRUE
  END PutFile;

(* ====================== Top-Level Window Procedures ====================== *)

CONST SolveButton = "solve";

PROCEDURE NewForm(w: Window; d: Drawing.T): Window =
(* Initialize "w" by reading its description from the form "Juno.fv". Attach
   procedures to the buttons in "w"'s interface, and initialize its toolbox. *)
  <* LL.sup < VBT.mu *>
  <* FATAL Rsrc.NotFound *>
  BEGIN
    TRY
      EVAL w.initFromRsrc("Juno.fv", JunoRsrc.Path, raw := TRUE);
      LOCK VBT.mu DO JunoConfig.SetFonts(w) END;
      FormsVBT.MakeDormant(w, SolveButton);
      FormsVBT.AttachProc(w, "config",           DoMakePassive);
      FormsVBT.AttachProc(w, "configOk",         DoConfigure);
      FormsVBT.AttachProc(w, "configCancel",     DoMakeActive);
      FormsVBT.AttachProc(w, "quit",           	 DoQuit);
      FormsVBT.AttachProc(w, "load",           	 DoLoad);
(* NOT YET IMPLEMENTED:
      FormsVBT.AttachProc(w, "savejava",         DoSaveJava);
*)
      FormsVBT.AttachProc(w, "reload",         	 DoReload);
      FormsVBT.AttachProc(w, "clearall",       	 DoClearAll);
      FormsVBT.AttachProc(w, "loadBrowser",    	 DoLoadFile);
      FormsVBT.AttachProc(w, "loadCancel",     	 DoMakeActive);
      FormsVBT.AttachProc(w, "save",           	 DoSave);
      FormsVBT.AttachProc(w, "saveAs",         	 DoMakePassive);
      FormsVBT.AttachProc(w, "saveBrowser",    	 DoSaveAs);
      FormsVBT.AttachProc(w, "saveAsCancel",   	 DoMakeActive);
      FormsVBT.AttachProc(w, "saveAsOverwrite",	 DoSaveAsOverwrite);
      FormsVBT.AttachProc(w, "overwriteCancel",	 DoMakeActive);
      FormsVBT.AttachProc(w, "moduleBrowser",  	 DoModule);
      FormsVBT.AttachProc(w, "print",          	 DoPrint);
      FormsVBT.AttachProc(w, "preview",        	 DoPreview);
      FormsVBT.AttachProc(w, "savePS",         	 DoSavePS);
      FormsVBT.AttachProc(w, "savePSas",       	 DoMakePassive);
      FormsVBT.AttachProc(w, "savePSBrowser",  	 DoSavePSAs);
      FormsVBT.AttachProc(w, "savePSCancel",   	 DoMakeActive);
      FormsVBT.AttachProc(w, "labels",         	 DoLabels);
      FormsVBT.AttachProc(w, "autoUpdate",     	 DoAutoUpdate);
      FormsVBT.AttachProc(w, "run",            	 DoRun);
      FormsVBT.AttachProc(w, SolveButton,      	 DoSolve);
      FormsVBT.AttachProc(w, "stop",           	 DoStop);
      FormsVBT.AttachProc(w, "fold",           	 DoFoldDialog);
      FormsVBT.AttachProc(w, "foldOk",         	 DoFoldWork);
      FormsVBT.AttachProc(w, "declName",       	 DoFoldWork);
      FormsVBT.AttachProc(w, "foldCancel",     	 DoMakeActive);
      FormsVBT.AttachProc(w, "foldAsAnim",       DoFoldAsAnimDialog);
      FormsVBT.AttachProc(w, "foldAsAnimOk",     DoFoldAsAnimWork);
      FormsVBT.AttachProc(w, "animName",         DoFoldAsAnimWork);
      FormsVBT.AttachProc(w, "sliderPts",        DoFoldAsAnimWork);
      FormsVBT.AttachProc(w, "foldAsAnimCancel", DoMakeActive);
      FormsVBT.AttachProc(w, "saveChanges",    	 DoSaveChanges);
      FormsVBT.AttachProc(w, "discardChanges", 	 DoDiscardChanges);
      FormsVBT.AttachProc(w, "cancelChanges",  	 DoCancelChanges);
      FormsVBT.AttachProc(w, "pushCurrCmd",    	 DoPushCurrCmd);
      FormsVBT.AttachProc(w, "popCurrCmd",     	 DoPopCurrCmd)
    EXCEPT
      FormsVBT.Error (msg) =>
        Wr.PutText(Stdio.stderr, "FormsVBT error: " & msg & "\n");
        Wr.Flush(Stdio.stderr);
        <* ASSERT FALSE *>
    END;
    RETURN InitToolbox(w, d)
  END NewForm;

VAR (* READONLY *)
  LoadInitialModule := VBT.GetMiscCodeType("LoadInitialModule");

PROCEDURE FindSavedState(VAR (*OUT*) st: SaveState.T; nm: Pathname.T):
  BOOLEAN =
  VAR rd: Rd.T; BEGIN
    TRY 
      rd := FileRd.Open(CheckpointName(nm));
      RETURN SaveState.Restore(st, rd)
    EXCEPT
      OSError.E => RETURN FALSE
    END
  END FindSavedState;

PROCEDURE Misc(w: Window; READONLY cd: VBT.MiscRec) =
(* If "cd.type # LoadInitialModule", then forward this event to the parent's
   "misc" method. Otherwise, call "GetFile" to load the current module; if an
   error occurs opening this file, report it to the standard error output and
   crash. *)
  <* LL.sup = VBT.mu *>
  VAR st: SaveState.T; BEGIN
    IF cd.type # LoadInitialModule THEN
      FormsVBT.T.misc(w, cd)
    ELSE
      IF FindSavedState(st, w.fileName) THEN
        (* recover from checkpoint *)
        w.fileName := st.file;
        w.fileStale := TRUE;
        w.checkpointStale := FALSE;
        FormsVBT.PutText(w, "currFile", Pathname.Last(st.file));
        TextPort.SetText(w.root.editor, st.editor);
        Source.SetText(w.root.source, st.source);
        w.root.eTrue := FALSE;
        w.root.astTrue := FALSE;
        w.root.sTrue := TRUE;
        w.root.dTrue := TRUE;
        SetModuleState(w, ModState.Stale);
        DoRun(w, NIL, NIL, cd.time);
        JunoError.Display(w, "Restoring from checkpoint.\n" &
          "To discard checkpoint,\nopen file again.");
      ELSE
        TRY GetFile(w, w.fileName, cd.time) EXCEPT
          OSError.E =>
	    Wr.PutText(Stdio.stderr, "Fatal error: cannot open or create \"");
	    Wr.PutText(Stdio.stderr, w.fileName);
            Wr.PutText(Stdio.stderr, "\" in the current directory\n");
            Wr.Flush(Stdio.stderr);
	    Process.Exit(1)
        | OpenFailure =>
	    Wr.PutText(Stdio.stderr, "Fatal error: \"");
	    Wr.PutText(Stdio.stderr, w.fileName);
            Wr.PutText(Stdio.stderr, "\" is not a file\n");
            Wr.Flush(Stdio.stderr);
            Process.Exit(1)
        END
      END;
      LOCK w.mu DO w.stat := SyncStat.Save END; (* anything except "NotDone" *)
      Thread.Signal(w.untilDone)
    END
  END Misc;

PROCEDURE InitToolbox(w: Window; d: Drawing.T): Window =
(* Initialize "w.toolbox" to contain buttons for the built-in tools, and set
   "w.builtInSize". Return "w". *)
  VAR tb1 := HVSplit.New(Axis.T.Ver); BEGIN
    Split.AddChildArray(tb1, ARRAY OF VBT.T{
      NEW(ToolBox.Button).init(w.root, "Create", Drawing.NewCreateTool()),
      NEW(ToolBox.Button).init(w.root, "Freeze", Drawing.NewFreezeTool()),
      NEW(ToolBox.Button).init(w.root, "Drag",   Drag.NewTool()),
      NEW(ToolBox.Button).init(w.root, "Adjust", Drawing.NewAdjustTool()),
      NEW(ToolBox.Button).init(w.root, "Grid On",Drawing.NewGridTools(tb1, d)),
      NEW(ToolBox.Button).init(w.root, "Rel",    Drawing.NewRelTool()),
      NEW(ToolBox.Button).init(w.root, "Rel1",   Drawing.NewRel1Tool()),
      NEW(ToolBox.Button).init(w.root, "Hor",
        Drawing.NewPredTool(JunoASTUtils.QIdFromId(Drawing.HorToolSym),  2)),
      NEW(ToolBox.Button).init(w.root, "Ver",
        Drawing.NewPredTool(JunoASTUtils.QIdFromId(Drawing.VerToolSym),  2)),
      NEW(ToolBox.Button).init(w.root, "Cong",
        Drawing.NewPredTool(JunoASTUtils.QIdFromId(Drawing.CongToolSym), 4)),
      NEW(ToolBox.Button).init(w.root, "Para",
        Drawing.NewPredTool(JunoASTUtils.QIdFromId(Drawing.ParaToolSym), 4))});
    w.builtInSize := Split.NumChildren(tb1);
    w.toolbox := PackSplit.New(Axis.T.Ver,
      op := PaintOp.FromRGB(0.8, 0.8, 0.8));
    Split.Insert(w.toolbox, pred := NIL, new := tb1);
    RETURN w
  END InitToolbox;

(* ============================= UI Callbacks ============================== *)

PROCEDURE DoMakePassive(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    FVFilter.MakePassive(fv, "background")
  END DoMakePassive;

PROCEDURE DoMakeActive(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    FVFilter.MakeActive(fv, "background")
  END DoMakeActive;

TYPE
  MenuClosure = Thread.Closure BRANDED "Juno.MenuClosure" OBJECT
    fv: FormsVBT.T;
    ts: VBT.TimeStamp
  END;
  MenuRec = BRANDED "Juno.MenuRec" OBJECT
    fv: FormsVBT.T;
    ts: VBT.TimeStamp
  END;

(* "Configure" ------------------------------------------------------------- *)

PROCEDURE ParseConfigWindow(fv: FormsVBT.T; nm: TEXT)
  RAISES {JunoConfig.Error}=
  <* LL.sup = VBT.mu *>
  VAR rd := TextRd.New(FormsVBT.GetText(fv, nm)); BEGIN
    JunoConfig.ParseConfigFile(rd)
  END ParseConfigWindow;

PROCEDURE DoConfigure(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
(* Callback for "Ok" menu button in "Configure..." dialogue. *)
  <* LL.sup = VBT.mu *>
  BEGIN
    TRY
      (* parse new configuration settings *)
      ParseConfigWindow(fv, "configDefault");
      ParseConfigWindow(fv, "configOverrides");

      (* change the fonts of the top-level window *)
      JunoConfig.SetFonts(fv);

      (* mark the top-level window for redisplay and set its origin *)
      VAR
        w: Window := fv;
        ch: Drawing.Child := Filter.Child(w.root.currView);
      BEGIN
        VBT.Mark(w);
        ch.setOrigin(JunoConfig.origin)
      END;

      (* change the fonts of any installed module view (help) windows *)
      VAR curr: View.EditorList := w.root.modules; BEGIN
        WHILE curr # NIL DO
          IF curr.form # NIL THEN
            VAR helpWindow := NARROW(curr.form, Toolbox).helpWindow; BEGIN
              IF helpWindow # NIL THEN
                JunoConfig.SetFonts(helpWindow);
                VBT.Mark(helpWindow)
              END
            END
          END;
          curr := curr.next
        END
      END;

      (* clean up *)
      FormsVBT.PopDown(fv, "configWindow");
      FVFilter.MakeActive(fv, "background")
    EXCEPT
      JunoConfig.Error (msg) =>
        JunoError.Display(w, "Error parsing configuration file:\n" & msg)
    END
  END DoConfigure;

(* "Quit" ------------------------------------------------------------------ *)

PROCEDURE DoQuit(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
(* Callback for "Quit" menu button. *)
  <* LL.sup = VBT.mu *>
  BEGIN
    FVFilter.MakePassive(fv, "background");
    EVAL Thread.Fork(NEW(MenuClosure, fv := fv, ts := ts,
      apply := DoQuitWork))
  END DoQuit;

PROCEDURE DoQuitWork(cl: MenuClosure): REFANY =
  BEGIN
    IF SaveCurrMod(cl.fv, cl.ts) THEN
      VAR w: Window := cl.fv; BEGIN
        Trestle.Delete(w);
        IF zeusOption THEN
          Trestle.Delete(animObj.w)
        END
      END
    ELSE
      FVFilter.MakeActive(cl.fv, "background")
    END;
    RETURN NIL
  END DoQuitWork;

PROCEDURE DoLoad(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
  <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
(* Callback for "New/Open..." menu button. *)
  <* LL.sup = VBT.mu *>
  BEGIN
    FVFilter.MakePassive(fv, "background");
    EVAL Thread.Fork(NEW(MenuClosure, fv := fv, ts := ts,
      apply := DoLoadWork))
  END DoLoad;

(* "Open/New..." ----------------------------------------------------------- *)

PROCEDURE DoLoadWork(cl: MenuClosure): REFANY =
(* First try to save the current module (if necessary). If that succeeds,
   pop-up the load file dialogue, which will be processed by the "DoLoadFile"
   callback. *)
  BEGIN
    IF SaveCurrMod(cl.fv, cl.ts)
      THEN FormsVBT.PopUp(cl.fv, "loadWindow", time := cl.ts)
      ELSE FVFilter.MakeActive(cl.fv, "background")
    END;
    RETURN NIL
  END DoLoadWork;

PROCEDURE DoReload(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
  <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
(* Callback for "Reload" menu button. *)
  <* LL.sup = VBT.mu *>
  VAR win:=NARROW(fv,Window); BEGIN
    FVFilter.MakePassive(fv, "background");
    EVAL DoLoadFileWork(NEW(FileIORec,
      fv := fv, ts := ts, fname := win.fileName));
  END DoReload;

(* NOT YET IMPLEMENTED:

<* LL.sup = VBT.mu *>
PROCEDURE DoSaveJava(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
(* Callback for "SaveJava" menu button. *)
  VAR
    win := NARROW(fv,Window);
    fn := win.fileName;
    java_path := Pathname.Prefix(fn);
    java_package := Pathname.LastBase(fn);
    java_package_path := Pathname.Join(pn1, pn2, NIL);
  BEGIN
    FVFilter.MakePassive(fv, "background");
    JunoError.Display(fv, "DoSaveJava: not implemented yet");
    FVFilter.MakeActive(fv, "background");
  END DoSaveJava;
*)

<* LL.sup = VBT.mu *>
PROCEDURE DoClearAll(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; time: VBT.TimeStamp) =
  VAR name := "Untitled.juno"; w := NARROW(fv,Window); BEGIN
    <* ASSERT name # NIL *>
    (* Read the text from the file *)
    FormsVBT.PutText(w, "currFile", Pathname.Last(name));
    TextPort.SetText(w.root.editor, "");
    w.root.eTrue := FALSE;
    EVAL MakeCurrMod(w, time);
    CurrCmd.ChangeAST(w.root.ccmd, JunoAST.SkipVal);
    w.root.astTrue := TRUE;
    w.root.sTrue := FALSE;
    w.root.dTrue := FALSE;
    MakeCurrCmd(w, time, w.root.skipify);
    SetModuleState(w, ModState.UpToDate)
  END DoClearAll;

TYPE
  FileIOClosure = MenuClosure BRANDED "Juno.FileIOClosure" OBJECT
    fname: TEXT
  END;
  FileIORec = MenuRec OBJECT
    fname: TEXT
  END;

PROCEDURE DoLoadFile(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
  <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
(* Load the file requested by the user in the "loadBrowser" field of "fv".
   Display the name of the new file with a light gray background in the
   "currFile" field of "fv". *)
  <* LL.sup = VBT.mu *>
  VAR fname: TEXT; BEGIN
    fname := GetFileName(fv, "loadBrowser");
    IF fname = NIL THEN RETURN END;
    FormsVBT.PopDown(fv, "loadWindow");
    FVFilter.MakePassive(fv, "background", FVFilter.CursorKind.Working);
(*
    EVAL Thread.Fork(NEW(FileIOClosure, fv := fv, ts := ts,
      fname := fname, apply := DoLoadFileWork))
*)
    EVAL DoLoadFileWork(NEW(FileIORec, fv := fv, ts := ts, fname := fname))
  END DoLoadFile;

PROCEDURE DoLoadFileWork(cl: FileIORec): REFANY =
  <* FATAL OpenFailure *>
  VAR w: Window := cl.fv; BEGIN
    TRY
      TRY
    	GetFile(w, cl.fname, cl.ts);
    	w.fileName := cl.fname;
    	w.checkpointStale := TRUE
      EXCEPT
    	OSError.E =>
    	  JunoError.Display(w, "Could not open file for reading:\n\""
    	    & cl.fname & "\"")
      END
    FINALLY
      FVFilter.MakeActive(cl.fv, "background")
    END;
    RETURN NIL
  END DoLoadFileWork;

(* "Save..." --------------------------------------------------------------- *)

PROCEDURE DoSave(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
  <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
(* Save the contents of the editor to "fv.filename". Change the background
   color of the "currFile" field to light gray, and set the editor's
   "modified" attribute to false. *)
  <* LL.sup = VBT.mu *>
  BEGIN
    FVFilter.MakePassive(fv, "background", FVFilter.CursorKind.Working);
(*
    EVAL Thread.Fork(NEW(MenuClosure, fv := fv, ts := ts, apply := DoSaveWork))
*)
    EVAL DoSaveWork(NEW(MenuRec, fv := fv, ts := ts))
  END DoSave;

PROCEDURE DoSaveWork(cl: MenuRec): REFANY =
(* Save the current file; make the background active when done. *)
  VAR w: Window := cl.fv; BEGIN
    EVAL PutFile(w, w.fileName, cl.ts);
    FVFilter.MakeActive(cl.fv, "background");
    RETURN NIL
  END DoSaveWork;

(* "Save As..." ------------------------------------------------------------ *)

PROCEDURE DoSaveAs(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
  <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
(* Save the contents of the editor to the filename specified by the user in
   the "saveBrowser" component of "fv". Set "fv.filename" and the "currFile"
   component to contain this new name, change the color of the "currFile"
   field to light gray, and set the editor's "modified" attribute to false. *)
  <* LL.sup = VBT.mu *>
  VAR fname: TEXT; BEGIN
    FormsVBT.PopDown(fv, "saveAsWindow");
    fname := GetFileName(fv, "saveBrowser");
    IF fname = NIL THEN RETURN END;
    FVFilter.MakePassive(fv, "background", FVFilter.CursorKind.Working);
    TRY
      EVAL FS.Status(fname);
      (* file already exists; check that the user wants to overwrite it *)
      NARROW(fv, Window).saveAsFName := fname;
      FormsVBT.PutText(fv, "overwriteFileName", Pathname.Last(fname));
      FormsVBT.PopUp(fv, "saveAsConfirmWindow")
    EXCEPT OSError.E =>
      (* file does not exist -- so save it directly *)
(*
      EVAL Thread.Fork(NEW(FileIOClosure, fv := fv, ts := ts, 
	fname := fname, apply := DoSaveAsWork))
*)
      EVAL DoSaveAsWork(NEW(FileIORec, fv := fv, ts := ts, 
	fname := fname))
    END;
  END DoSaveAs;

PROCEDURE DoSaveAsOverwrite(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
  <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
(* This callback procedure is invoked after the user has chosen "Save As...",
   specified a file that already exists, and confirmed that he/she would
   indeed like to overwrite the file. *)
  VAR fname := NARROW(fv, Window).saveAsFName; BEGIN
(*
      EVAL Thread.Fork(NEW(FileIOClosure, fv := fv, ts := ts, 
	fname := fname, apply := DoSaveAsWork))
*)
      EVAL DoSaveAsWork(NEW(FileIORec, fv := fv, ts := ts, 
	fname := fname))
  END DoSaveAsOverwrite;

PROCEDURE DoSaveAsWork(cl: FileIORec): REFANY =
  VAR w: Window := cl.fv; BEGIN
    TRY
      IF PutFile(w, cl.fname, cl.ts) THEN
    	FormsVBT.PutText(cl.fv, "currFile", Pathname.Last(cl.fname));
    	TRY FS.DeleteFile(CheckpointName(w.fileName)) EXCEPT
          OSError.E => (*SKIP*)
        END;
    	w.fileName := cl.fname;
    	w.checkpointStale := TRUE
      END
    FINALLY
      FVFilter.MakeActive(cl.fv, "background")
    END;
    RETURN NIL
  END DoSaveAsWork;

(* "Open Module..." -------------------------------------------------------- *)

PROCEDURE DoModule(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
  <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
  <* LL.sup = VBT.mu *>
  BEGIN
    FVFilter.MakePassive(fv, "background", FVFilter.CursorKind.Working);
(*
    EVAL Thread.Fork(NEW(MenuClosure, fv := fv, ts := ts,
      apply := DoModuleWork))
*)
    EVAL DoModuleWork(NEW(MenuRec, fv := fv, ts := ts))
  END DoModule;

PROCEDURE DoModuleWork(cl: MenuRec): REFANY =
  VAR w: Window := cl.fv; BEGIN
    TRY
      OpenModule(w, FormsVBT.GetText(cl.fv, "moduleBrowser"),
    	hasPrivate := TRUE)
    FINALLY
      FVFilter.MakeActive(cl.fv, "background")
    END;
    RETURN NIL
  END DoModuleWork;

(* "Print" ----------------------------------------------------------------- *)

PROCEDURE DoPrint(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
  <* LL.sup = VBT.mu *>
  BEGIN
    FVFilter.MakePassive(fv, "background", FVFilter.CursorKind.Working);
    EVAL Thread.Fork(NEW(MenuClosure, fv := fv, ts := ts,
      apply := DoPrintWork))
(*
    EVAL DoPrintWork(NEW(MenuRec, fv := fv, ts := ts))
*)
  END DoPrint;

PROCEDURE DoPrintWork(cl: MenuClosure): REFANY =
  VAR w: Window := cl.fv; BEGIN
    TRY
      TRY
      	VAR cmd: TEXT; args: REF ARRAY OF TEXT; BEGIN
      	  JunoConfig.ParseCmd(JunoConfig.printCmd, (*OUT*) cmd, (*OUT*) args,
            titleVal := Pathname.Last(Pathname.ReplaceExt(w.fileName, "ps")));
      	  PipePostScriptToCmd(cl.fv, cmd, args^)
      	END
      EXCEPT JunoConfig.Error (msg) =>
        JunoError.Display(w, "Error in external print command:\n" & msg)
      END
    FINALLY
      FVFilter.MakeActive(cl.fv, "background")
    END;
    RETURN NIL
  END DoPrintWork;

(* "Preview" --------------------------------------------------------------- *)

PROCEDURE DoPreview(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
  <* LL.sup = VBT.mu *>
  BEGIN
    FVFilter.MakePassive(fv, "background", FVFilter.CursorKind.Working);
    EVAL Thread.Fork(NEW(MenuClosure, fv := fv, ts := ts,
      apply := DoPreviewWork))
(*
    EVAL DoPreviewWork(NEW(MenuRec, fv := fv, ts := ts))
*)
  END DoPreview;

VAR previewNum := 1;
(* counter of the number of the next preview document *)

TYPE
  PreviewClosure = Thread.Closure OBJECT
    fileName: TEXT;
    w: Window;
  OVERRIDES
    apply := ForkPreviewer
  END;

PROCEDURE DoPreviewWork(cl: MenuClosure): REFANY =
(* Write the PostScript output to a temporary file, then fork a thread to run
   the PostScript previewer named by "JunoConfig.previewCmd" to view that
   file, deleting the file when the forked preview process exits. *)
  VAR nm := "/tmp/juno-" & Fmt.Int(Process.GetMyID()) & "-"; BEGIN
    nm := nm & Fmt.Int(previewNum) & ".ps"; INC(previewNum);
    WritePostScriptToFile(cl.fv, nm, showPage := FALSE);
    EVAL Thread.Fork(NEW(PreviewClosure, fileName := nm, w := cl.fv));
    FVFilter.MakeActive(cl.fv, "background");
    RETURN NIL
  END DoPreviewWork;

PROCEDURE ForkPreviewer(self: PreviewClosure): REFANY =
(* Fork a PostScript preview process running on the file "self.fileName", wait
   for the process to exit, and then delete the temporary file. *)
  BEGIN
    TRY
      TRY
        (* fork a PostScript preview process *)
        VAR cmd: TEXT; args: REF ARRAY OF TEXT; display := disp; BEGIN
          IF display = NIL THEN
            display := Env.Get("DISPLAY");
            IF display = NIL THEN display := ":0.0" END
          END;
          TRY
            JunoConfig.ParseCmd(JunoConfig.previewCmd, (*OUT*)cmd, (*OUT*)args,
              titleVal := Pathname.Last(Pathname.ReplaceExt(w.fileName, "ps")),
              displayVal := display, filenameVal := self.fileName);
            EVAL Process.Wait(Process.Create(cmd, args^))
          EXCEPT JunoConfig.Error (msg) =>
            JunoError.Display(self.w,
              "Error in external preview command:\n" & msg)
          END
        END
      FINALLY
        (* delete the temporary file in any event *)
        FS.DeleteFile(self.fileName)
      END
    EXCEPT
      OSError.E => (* SKIP *)
    END;
    RETURN NIL
  END ForkPreviewer;

(* "Save PostScript As..." ------------------------------------------------- *)

PROCEDURE DoSavePS(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  <* LL.sup = VBT.mu *>
  VAR w: Window := fv; BEGIN
    FVFilter.MakePassive(fv, "background", FVFilter.CursorKind.Working);
    WritePostScriptToFile(w, Pathname.ReplaceExt(w.fileName, "ps"));
    FVFilter.MakeActive(fv, "background")
  END DoSavePS;

PROCEDURE DoSavePSAs(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
  <* LL.sup = VBT.mu *>
  VAR fname: TEXT; BEGIN
    fname := GetFileName(fv, "savePSBrowser");
    IF fname = NIL THEN RETURN END;
    FormsVBT.PopDown(fv, "savePSWindow");
    FVFilter.MakePassive(fv, "background", FVFilter.CursorKind.Working);
    EVAL Thread.Fork(NEW(FileIOClosure, fv := fv, ts := ts,
      fname := fname, apply := DoSavePSAsWork))
(*
    EVAL DoSavePSAsWork(NEW(FileIORec, fv := fv, ts := ts,
      fname := fname))
*)
  END DoSavePSAs;

PROCEDURE DoSavePSAsWork(cl: FileIOClosure): REFANY =
  BEGIN
    WritePostScriptToFile(cl.fv, cl.fname);
    FVFilter.MakeActive(cl.fv, "background");
    RETURN NIL
  END DoSavePSAsWork;

(* "Labels" Menu ----------------------------------------------------------- *)

PROCEDURE DoLabels(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  <* LL.sup = VBT.mu *>
  VAR w: Window := fv; option: TEXT; style: [0..2]; BEGIN
    option := FormsVBT.GetChoice(fv, "labels");
    IF Text.Equal(option, "letters") THEN style := 2
    ELSIF Text.Equal(option, "crosses") THEN style := 1
    ELSIF Text.Equal(option, "nothing") THEN style := 0
    ELSE <* ASSERT FALSE *>
    END;
    Drawing.SetLabelStyle(w.root.drawing, style)
  END DoLabels;

(* "Redisplay" Menu -------------------------------------------------------- *)

PROCEDURE DoAutoUpdate(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  <* LL.sup = VBT.mu *>
  VAR w: Window := fv; option: TEXT; BEGIN
    option := FormsVBT.GetChoice(fv, "autoUpdate");
    IF Text.Equal(option, "autoRun") THEN w.root.skipify := FALSE
    ELSIF Text.Equal(option, "autoSolve") THEN w.root.skipify := TRUE
    ELSE <* ASSERT FALSE *>
    END
  END DoAutoUpdate;

(* "Run"/"Solve"/"Stop" Buttons -------------------------------------------- *)

TYPE
  RunClosure = MenuClosure OBJECT
    skipify: BOOLEAN
  END;

PROCEDURE DoRun(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
  <* LL.sup = VBT.mu *>
  BEGIN
    FVFilter.MakePassive(fv, "background", FVFilter.CursorKind.Working);
(*
    EVAL Thread.Fork(NEW(RunClosure, fv := fv, ts := ts,
      skipify := FALSE, apply := DoRunWork));
*)
    EVAL DoRunWork(NEW(RunClosure, fv := fv, ts := ts,
      skipify := FALSE))
  END DoRun;

PROCEDURE DoSolve(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
  <* LL.sup = VBT.mu *>
  BEGIN
    FVFilter.MakePassive(fv, "background", FVFilter.CursorKind.Working);
(*
    EVAL Thread.Fork(NEW(RunClosure, fv := fv, ts := ts,
      skipify := TRUE, apply := DoRunWork))
*)
    EVAL DoRunWork(NEW(RunClosure, fv := fv, ts := ts,
      skipify := TRUE))
  END DoSolve;

PROCEDURE DoRunWork(cl: RunClosure): REFANY =
  VAR w: Window := cl.fv; BEGIN
    EVAL Drawing.FinishTextTool(w.root.drawing);
    TRY MakeModAndCmd(cl.fv, cl.ts, cl.skipify) FINALLY
      FVFilter.MakeActive(cl.fv, "background")
    END;
    RETURN NIL
  END DoRunWork;

PROCEDURE DoStop(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
  <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  <* LL.sup = VBT.mu *>
  BEGIN
    TRY
      JunoRT.Interrupt()
    FINALLY
      FVFilter.MakeActive(fv, "background")
    END
  END DoStop;

(* "Fold..." --------------------------------------------------------------- *)

PROCEDURE DoFoldDialog(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
  <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
(* This procedure is called when the user chooses "Fold...". *)
  <* LL.sup = VBT.mu *>
  BEGIN
    FVFilter.MakePassive(fv, "background", FVFilter.CursorKind.Working);
(*
    EVAL Thread.Fork(NEW(MenuClosure, fv := fv, ts := ts,
      apply := DoFoldDialogWork))
*)
    EVAL DoFoldDialogWork(NEW(MenuRec, fv := fv, ts := ts))
  END DoFoldDialog;

PROCEDURE DoFoldDialogWork(cl: MenuRec): REFANY =
(* This procedure fills in the default values for the dialog and pops up the
   window. The "DoFoldWork" procedure is invoked when the user clicks the "OK"
   button. *)
  VAR w: Window := cl.fv; BEGIN
    IF NOT CompileModAndCmd(w, cl.ts) THEN
      FVFilter.MakeActive(cl.fv, "background");
      RETURN NIL
    END;
    VAR name: TEXT; BEGIN
      IF ISTYPE(CurrCmd.GetCmd(CurrCmd.GetAST(w.root.ccmd)), JunoAST.Skip) THEN
    	name := CurrCmd.NewDeclName(w.root.ccmd, "Pred");
    	FormsVBT.MakeSelected(cl.fv, "predType");
      ELSE
    	name := CurrCmd.NewDeclName(w.root.ccmd, "Proc");
    	FormsVBT.MakeSelected(cl.fv, "procType")
      END;
      FormsVBT.PutText(cl.fv, "declName", name, append := FALSE);
    END;
    FormsVBT.MakeSelected(cl.fv, "pointTool");
    FVFilter.MakePassive(cl.fv, "background", FVFilter.CursorKind.Passive);
    FormsVBT.PopUp(cl.fv, "foldWindow", time := cl.ts);
    RETURN NIL
  END DoFoldDialogWork;

PROCEDURE ToolTypeFromName(nm: TEXT): ToolType =
  BEGIN
    IF Text.Equal(nm, "pointTool") THEN 
      RETURN ToolType.Point
    ELSIF Text.Equal(nm, "noTool") THEN 
      RETURN ToolType.None
    ELSE 
      <* ASSERT FALSE *>
    END
  END ToolTypeFromName;

PROCEDURE ParseProcName(w: Window; nm: TEXT;
  VAR (*OUT*) hdr: JunoAST.PredHeader): BOOLEAN =
(* Parse the field named "nm" of the form "w" as a predicate header, set
   "hdr" to the AST of the parsed result if successful, and return TRUE.
   Otherwise, pop up an error message over the window "w" and return FALSE. *)
  <* FATAL Rd.Failure *>
  BEGIN
    TRY 
      VAR junk: CARDINAL; BEGIN
        JunoParse.FoldHeader(TextRd.New(FormsVBT.GetText(w, nm)), hdr, junk)
      END;
      IF hdr.ins = NIL THEN
        hdr.ins := CurrCmd.GetFoldArgs(w.root.ccmd)
      END
    EXCEPT
      JunoParse.Error, JunoLex.Error =>
    	JunoError.Display(w, "Fold name syntax is Id or Id(IdList)");
        RETURN FALSE
    END;
    RETURN TRUE
  END ParseProcName;

PROCEDURE ParseVarList(fv: FormsVBT.T; nm: TEXT;
  VAR (*OUT*) pts: JunoAST.IdList): BOOLEAN =
(* Parse the field named "nm" of the form "fv" as a list of identifiers
   (variable names), set "pts" to the parsed result, and return TRUE.
   Otherwise, pop up an error message over the window "fv" and return FALSE.
*)
  BEGIN
    TRY 
      VAR junk: CARDINAL; BEGIN
        JunoParse.IdList(TextRd.New(FormsVBT.GetText(fv, nm)), pts, junk)
      END
    EXCEPT
      JunoParse.Error, JunoLex.Error =>
    	JunoError.Display(fv, "Fold points syntax is \"id, id, ...\"");
        RETURN FALSE
    END;
    RETURN TRUE
  END ParseVarList;

PROCEDURE DoFoldWork(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
  <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
(* This procedure is called when the user has set the values in the "Fold..."
   dialogue and clicked "OK" (or hit <Enter> in the type-in field). It
   extracts the values from the dialogue and folds the current command
   appropriately. *)
  <* LL.sup = VBT.mu *>
  VAR hdr: JunoAST.PredHeader; BEGIN
    FormsVBT.PopDown(fv, "foldWindow");
    IF ParseProcName(fv, "declName", (*OUT*) hdr) THEN
      VAR
    	foldKind: CurrCmd.FoldKind; 
    	kind := FormsVBT.GetChoice(fv, "typeRadio");
    	toolKind := ToolTypeFromName(FormsVBT.GetChoice(fv, "toolRadio"));
      BEGIN
    	IF Text.Equal(kind, "predType") THEN 
    	  foldKind := CurrCmd.FoldKind.Pred
    	ELSIF Text.Equal(kind, "procType") THEN
    	  foldKind := CurrCmd.FoldKind.Proc
    	ELSIF Text.Equal(kind, "templType") THEN
    	  foldKind := CurrCmd.FoldKind.ProcNoArgs;
    	  toolKind := ToolType.Templ
    	ELSE
    	  <* ASSERT FALSE *>
    	END;
    	DoFold(fv, ts, hdr, foldKind, toolType := toolKind)
      END
    END;
    FVFilter.MakeActive(fv, "background")
  END DoFoldWork;

(* "Fold As Animation..." -------------------------------------------------- *)

PROCEDURE DoFoldAsAnimDialog(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
  <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
(* This procedure is called when the user chooses "Fold As Animation...". *)
  <* LL.sup = VBT.mu *>
  BEGIN
    FVFilter.MakePassive(fv, "background", FVFilter.CursorKind.Working);
(*
    EVAL Thread.Fork(NEW(MenuClosure, fv := fv, ts := ts,
      apply := DoFoldAsAnimDialogWork))
*)
    EVAL DoFoldAsAnimDialogWork(NEW(MenuRec, fv := fv, ts := ts))
  END DoFoldAsAnimDialog;

VAR SliderDrawName := JunoASTUtils.QIdFromTexts("Slider", "Draw");

PROCEDURE SliderArgs(ccmd: CurrCmd.T): TEXT =
(* Search the body of "ccmd" for a procedure call statement to "Slider.Draw",
   and return an unparsing of the arguments to that call. If no such call can
   be found, return an empty text. *)
  VAR
    res: TEXT;
    body := CurrCmd.GetCmd(CurrCmd.GetAST(ccmd));
    call := JunoASTUtils.FirstProcCall(body, SliderDrawName);
  BEGIN
    IF call = NIL THEN res := "" ELSE
      <* FATAL Wr.Failure *>
      VAR wr := TextWr.New(); BEGIN
        JunoUnparse.P(wr, call.ins, width := LAST(INTEGER),
          prec := JunoConfig.realPrec);
        res := TextWr.ToText(wr)
      END
    END;
    RETURN res
  END SliderArgs;

PROCEDURE DoFoldAsAnimDialogWork(cl: MenuRec): REFANY =
(* This procedure fills in the default values for the dialog and pops up the
   window. The "DoFoldAsAnimWork" procedure is invoked when the user clicks
   the "OK" button. *)
  VAR w: Window := cl.fv; BEGIN
    IF NOT CompileModAndCmd(w, cl.ts) THEN
      FVFilter.MakeActive(cl.fv, "background");
      RETURN NIL
    END;
    FormsVBT.PutText(cl.fv, "animName",
      CurrCmd.NewDeclName(w.root.ccmd, "Anim"));
    FormsVBT.PutText(cl.fv, "sliderPts", SliderArgs(w.root.ccmd));
    FVFilter.MakePassive(cl.fv, "background", FVFilter.CursorKind.Passive);
    FormsVBT.PopUp(cl.fv, "foldAsAnimWindow", time := cl.ts);
    RETURN NIL
  END DoFoldAsAnimDialogWork;

PROCEDURE DoFoldAsAnimWork(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
  <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
(* This procedure is called when the user has set the values in the "Fold As
   Animation..." dialogue and clicked "OK" (or hit <Enter> in one of the
   type-in fields. It extracts the values from the dialogue and folds the
   current command appropriately. *)
  <* LL.sup = VBT.mu *>
  VAR hdr: JunoAST.PredHeader; pts: JunoAST.IdList; BEGIN
    FormsVBT.PopDown(fv, "foldAsAnimWindow");
    IF ParseProcName(fv, "animName", (*OUT*) hdr) AND
       ParseVarList(fv, "sliderPts", (*OUT*) pts) THEN
      IF pts.size # 3
        THEN JunoError.Display(fv, "You must specify exactly 3 slider points")
        ELSE DoFoldAsAnim(fv, ts, hdr, sliderPts := pts)
      END
    END;
    FVFilter.MakeActive(fv, "background")
  END DoFoldAsAnimWork;

(* "Push Current Command" -------------------------------------------------- *)

PROCEDURE DoPushCurrCmd(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
  <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
  <* LL.sup = VBT.mu *>
  BEGIN
    FVFilter.MakePassive(fv, "background", FVFilter.CursorKind.Working);
(*
    EVAL Thread.Fork(NEW(MenuClosure, fv := fv, ts := ts,
      apply := DoPushCurrCmdWork))
*)
    EVAL DoPushCurrCmdWork(NEW(MenuRec, fv := fv, ts := ts))
  END DoPushCurrCmd;

PROCEDURE DoPushCurrCmdWork(cl: MenuRec): REFANY =
  VAR w: Window := cl.fv; BEGIN
    TRY
      (* Build current module and command; return if compilation error *)
      IF NOT CompileModAndCmd(w, cl.ts) THEN RETURN NIL END;
      (* Otherwise, push current command *)
      VAR nm := Editor.NextCmdName(w.root.editor); BEGIN
        DoFold(w, cl.ts, NEW(JunoAST.PredHeader, name := nm),
          CurrCmd.FoldKind.ProcNoArgs, ToolType.None)
      END
    FINALLY
      FVFilter.MakeActive(cl.fv, "background")
    END;
    RETURN NIL
  END DoPushCurrCmdWork;

(* "Pop Current Command" --------------------------------------------------- *)

PROCEDURE DoPopCurrCmd(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
  <* LL.sup = VBT.mu *>
  BEGIN
    FVFilter.MakePassive(fv, "background", FVFilter.CursorKind.Working);
(*
    EVAL Thread.Fork(NEW(MenuClosure, fv := fv, ts := ts,
      apply := DoPopCurrCmdWork))
*)
    EVAL DoPopCurrCmdWork(NEW(MenuRec, fv := fv, ts := ts))
  END DoPopCurrCmd;

PROCEDURE DoPopCurrCmdWork(cl: MenuRec): REFANY =
  VAR w: Window := cl.fv; BEGIN
    TRY
      IF NOT CompileModAndCmd(w, cl.ts) THEN RETURN NIL END;
      IF ISTYPE(CurrCmd.GetAST(w.root.ccmd), JunoAST.Skip) THEN
    	VAR ed := w.root.editor; nm: JunoAST.Id; cmd: JunoAST.Cmd; BEGIN
    	  cmd := Editor.PopCurrCmd(ed, nm);
    	  IF cmd # NIL THEN
    	    UnbindDecl(CurrCmd.GetScope(w.root.ccmd), nm);
    	    ToolBox.Update(Split.Succ(w.toolbox, NIL), ed, w.root,
    	      w.builtInSize, anon := TRUE);
    	    CurrCmd.ChangeAST(w.root.ccmd, cmd);
    	    w.root.astTrue := TRUE;
    	    w.root.sTrue := FALSE;
    	    w.root.dTrue := FALSE;
            SetModuleState(w, ModState.Stale);
    	    MakeCurrCmd(w, cl.ts, w.root.skipify)
    	  END
    	END
      END
    FINALLY
      FVFilter.MakeActive(cl.fv, "background")
    END;
    RETURN NIL
  END DoPopCurrCmdWork;

PROCEDURE UnbindDecl(scp: JunoScope.T; nm: JunoAST.Id) =
(* Remove "nm"'s binding from "scp". Requires that "nm" is bound in "scp". *)
  <* FATAL JunoScope.NotFound *>
  BEGIN
    EVAL JunoScope.Unbind(scp, nm)
  END UnbindDecl;

(* Save/Discard/Cancel Changes --------------------------------------------- *)

PROCEDURE DoSaveChanges(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  <* LL.sup = VBT.mu *>
  VAR w: Window := fv; BEGIN
    LOCK w.mu DO w.stat := SyncStat.Save END;
    Thread.Signal(w.untilDone)
  END DoSaveChanges;

PROCEDURE DoDiscardChanges(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  <* LL.sup = VBT.mu *>
  VAR w: Window := fv; BEGIN
    LOCK w.mu DO w.stat := SyncStat.Discard END;
    Thread.Signal(w.untilDone)
  END DoDiscardChanges;

PROCEDURE DoCancelChanges(fv: FormsVBT.T; <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  <* LL.sup = VBT.mu *>
  VAR w: Window := fv; BEGIN
    LOCK w.mu DO w.stat := SyncStat.Cancel END;
    Thread.Signal(w.untilDone)
  END DoCancelChanges;

(* ========================= ToolBox Procedures ============================ *)

TYPE
  Toolbox = FormsVBT.T OBJECT
    w: Window;
    ed: Editor.T;
    helpWindow: VBT.T := NIL; (* top-level installed window (if any) *)
    hasPrivate: BOOLEAN
  END;

(* A "Toolbox" is a window that represents the tools for a module. It is
   created from a FormsVBT form. The form has a menu for displaying help on
   the module and for performing other operations on the toolbox. The toolbox
   itself contains point, text, and set tools. *)

PROCEDURE NewToolbox(w: Window; v: Editor.T; hasPrivate: BOOLEAN): Toolbox =
(* Create and return a toolbox form for "v". *)
  <* FATAL Rsrc.NotFound *>
  <* LL.sup = VBT.mu *>
  VAR
    res: Toolbox := NEW(Toolbox, w := w, ed := v,
      hasPrivate := hasPrivate).initFromRsrc(
        "Toolbox.fv", JunoRsrc.Path, raw := TRUE);
    moduleName := Atom.ToText(Editor.ModuleName(v));
  BEGIN
    JunoConfig.SetFonts(res);
    IF moduleName = NIL THEN moduleName := "<Untitled>" END;
    TRY
      FormsVBT.PutText(res, "toolboxName", moduleName);
      FormsVBT.PutGeneric(res, "toolboxButtons", ButtonView(w.root, v));
      FormsVBT.AttachProc(res, "help", DoHelp);
      FormsVBT.AttachProc(res, "close", DoCloseToolbox);
    EXCEPT
      FormsVBT.Error (msg) =>
        Wr.PutText(Stdio.stderr, "FormsVBT error: " & msg & "\n");
        Wr.Flush(Stdio.stderr);
        <* ASSERT FALSE *>
    END;
    RETURN res
  END NewToolbox;

PROCEDURE ButtonView(rt: View.Root; ed: Editor.T): VBT.T =
(* Return the button view on the module view "ed". *)
  VAR res := HVSplit.New(hv := Axis.T.Ver, adjustable := FALSE); BEGIN
    ToolBox.Update(res, ed, rt, anon := FALSE);
    RETURN res
  END ButtonView;

PROCEDURE DoHelp(
    fv: FormsVBT.T;
    <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY;
    <*UNUSED*> ts: VBT.TimeStamp) =
  <* LL.sup = VBT.mu *>
  VAR tb: Toolbox := fv; BEGIN
    IF tb.helpWindow = NIL THEN
      <* FATAL Rsrc.NotFound *>
      VAR public, private: VBT.T; BEGIN
        IF tb.hasPrivate
          THEN private := NEW(TextEditVBT.T, tp := tb.ed).init()
          ELSE private := TextVBT.New("No private view")
        END;
        public := NEW(TextEditVBT.T,
          tp := NEW(PublicView.T).init(tb.ed)).init();
        tb.helpWindow := NEW(FormsVBT.T).initFromRsrc(
          "Editor2.fv", JunoRsrc.Path, raw := TRUE);
        FormsVBT.PutGeneric(tb.helpWindow, "publicView", public);
        FormsVBT.PutGeneric(tb.helpWindow, "privateView", private);
        JunoConfig.SetFonts(tb.helpWindow);
      END
    ELSE
      (* force help window into "public" view *)
      FormsVBT.PutChoice(tb.helpWindow, "view", "public");
      FormsVBT.PutInteger(tb.helpWindow, "body", 0);
    END;
    <* FATAL TrestleComm.Failure *>
    VAR
      wScrn := Trestle.ScreenOf(tb.w, Rect.NorthEast(VBT.Domain(w)));
      wST := ScreenType(wScrn.trsl, wScrn.id);
      tbScrn := Trestle.ScreenOf(tb.helpWindow, Point.Origin);
    BEGIN
      (* delete help window if it is on the wrong Trestle *)
      IF tbScrn.trsl # NIL AND tbScrn.trsl # wScrn.trsl THEN
        Trestle.Delete(tb.helpWindow);
        tbScrn.trsl := NIL
      END;
      (* guarantee that the help window is attached to the right Trestle *)
      IF tbScrn.trsl = NIL THEN
        Trestle.Attach(tb.helpWindow, wScrn.trsl);
        VAR mod := FormsVBT.GetText(tb, "toolboxName"); BEGIN
          Trestle.Decorate(tb.helpWindow, instance := mod, windowTitle := mod)
        END
      END;
      (* guarantee that the help window has the right screen type so we can
         measure it *)
      IF VBT.ScreenTypeOf(tb.helpWindow) # wST THEN
        VBTClass.Rescreen(tb.helpWindow, wST)
      END;
      (* intall the help window so its northeast corner is just inside the
         northeast corner of the main Juno window *)
      VAR s := VBTClass.GetShape(tb.helpWindow, Axis.T.Hor,
        n := 0, clearNewShape := FALSE);
      BEGIN
        Trestle.Overlap(tb.helpWindow, wScrn.id,
          Point.Add(wScrn.q, Point.T{-(s.pref+24), 24}))
      END
    END
  END DoHelp;

PROCEDURE ScreenType(trsl: Trestle.T; id: Trestle.ScreenID): VBT.ScreenType
    RAISES {TrestleComm.Failure} =
  VAR scrns := Trestle.GetScreens(trsl); i := 0; BEGIN
    WHILE scrns[i].id # id DO INC(i) END;
    RETURN scrns[i].type
  END ScreenType; 

PROCEDURE DoCloseToolbox(
    fv: FormsVBT.T;
    <*UNUSED*> event: TEXT;
    <*UNUSED*> data: REFANY;
    <*UNUSED*> ts: VBT.TimeStamp) =
  <* LL.sup = VBT.mu *>
  BEGIN
    Split.Delete(VBT.Parent(fv), fv)
  END DoCloseToolbox;

(* ================================ Main Body ============================== *)

TYPE
  SourceView = Source.T OBJECT
    w: Window
  OVERRIDES
    modified := SourceModified
  END;

  EditorView = Editor.T OBJECT
    w: Window
  OVERRIDES
    txtModified := EditorModified
  END;

PROCEDURE SourceModified(sv: SourceView; how: View.ModKind) =
(* Callback procedure when text in the source window (current command) is
   modified. *)
  <* LL.sup < sv *>
  VAR w := sv.w; BEGIN
    w.checkpointStale := TRUE;
    w.root.sTrue := FALSE;
    SetModuleState(w, ModState.Stale);
    IF how # View.ModKind.ImplicitConsistent THEN
      FormsVBT.MakeActive(w, SolveButton);
      IF how = View.ModKind.Explicit THEN
    	ToolBox.Unselect(w.root);
    	w.root.dTrue := FALSE;
    	w.root.astTrue := FALSE
      END
    END
  END SourceModified;

PROCEDURE EditorModified(ev: EditorView) =
(* Callback procedure when text in the editor window is modified. *)
  <* LL.sup < ev *>
  VAR w := ev.w; BEGIN
    w.checkpointStale := TRUE;
    w.root.eTrue := FALSE;
    SetModuleState(w, ModState.Stale);
    FormsVBT.MakeActive(w, SolveButton);
    ToolBox.Unselect(w.root);
    w.root.dTrue := FALSE;
    (* If a public part of the module changed, we have to recompile the
       current command. Since we have no way of telling (for now) if the
       change was to the public or private part of the module, we have to
       recompile the current command in all cases. *)
    w.root.ccmd.codeValid := FALSE
  END EditorModified;

TYPE ModState = { Stale, UpToDate };
  
PROCEDURE SetModuleState(w: Window; modState: ModState) =
  CONST ModStateColor = ARRAY ModState OF TEXT{"LightRed", "LightGray"}; BEGIN
    w.fileStale := (modState = ModState.Stale);
    FormsVBT.PutTextProperty(w, "currFile", "BgColor", ModStateColor[modState])
  END SetModuleState;

PROCEDURE PrintUsage(msg: TEXT) =
(* Print a usage message and exit the program. *)
  BEGIN
    Wr.PutText(Stdio.stderr, "Fatal error: ");
    Wr.PutText(Stdio.stderr, msg);
    Wr.PutChar(Stdio.stderr, '\n');
    Wr.PutText(Stdio.stderr, "Usage: Juno [-zeus] [-display <Xdisplay>] ");
    Wr.PutText(Stdio.stderr, "[-geometry <Xgeometry>] [filename]\n");
    Wr.Flush(Stdio.stderr);
    Process.Exit(1)
  END PrintUsage;

PROCEDURE RsrcOpen(nm: TEXT): Rd.T RAISES {Rsrc.NotFound} =
  BEGIN RETURN Rsrc.Open(nm, JunoRsrc.Path) END RsrcOpen;

PROCEDURE CompileFile(root: View.Root; fileName: TEXT; parent: JunoScope.T):
    JunoScope.Mod =
(* Build the module in the resource named "fileName" under the scope "parent",
   and return the resulting module entity. Both the public and private scope
   in the resulting entity are created with a parent scope of "parent".
   Requires that there are no compilation errors in the module. *)
  <* FATAL Rsrc.NotFound *>
  VAR
    rd: Rd.T := RsrcOpen(fileName);
    te := NEW(Editor.T).init(Rd.GetText(rd, LAST(CARDINAL)));
    modNm: JunoAST.Id; mod: JunoScope.Mod;
  BEGIN
    IF NOT CompileEditor(root, te, NoTimeStamp, parent, modNm, mod) THEN
      Process.Crash("compilation error in \"" & fileName & "\"")
    END;
    Rd.Close(rd);
    RETURN mod
  END CompileFile;

PROCEDURE FillBrowser(v: ListVBT.T; modList: TextList.T) =
(* Prepend the names in "modList" to "v". *)
  VAR i := 0; BEGIN
    v.insertCells(0, TextList.Length(modList));
    WHILE modList # NIL DO
      v.setValue(i, modList.head);
      modList := modList.tail; INC(i)
    END
  END FillBrowser;

TYPE CheckpointClosure = Thread.Closure OBJECT
    w: Window
  OVERRIDES
    apply := MakeCheckpoint
  END;

VAR 
  checkpointThread: Thread.T;

PROCEDURE CheckpointName(nm: Pathname.T): Pathname.T =
  BEGIN
    RETURN Pathname.ReplaceExt(nm, "bak")
  END CheckpointName;

PROCEDURE MakeCheckpoint(self:CheckpointClosure): REFANY =
  VAR w := self.w; BEGIN
    LOOP
      Thread.Pause(FLOAT(JunoConfig.chkptIntv, LONGREAL));
      LOCK VBT.mu DO
        IF w.fileStale AND w.checkpointStale THEN
          TRY
            VAR 
              wr := FileWr.Open(CheckpointName(w.fileName)); 
              st: SaveState.T;
            BEGIN
              st.file := w.fileName;
              st.editor := TextPort.GetText(w.root.editor);
              st.source := Source.GetText(w.root.source);
              SaveState.Save(st, wr);
              w.checkpointStale := FALSE;
              Wr.Close(wr)
            END
          EXCEPT
            OSError.E, Wr.Failure, Thread.Alerted => (*SKIP*)
          END
        END
      END
    END
  END MakeCheckpoint;

PROCEDURE SetTEFont(te: TextEditVBT.T; f: Font.T) =
  <* LL.sup = VBT.mu *>
  BEGIN te.tp.setFont(f) END SetTEFont;

<* FATAL Split.NotAChild, JunoScope.NameClash *>
VAR
  startup: FormsVBT.T;
  builtInScope, modScope: JunoScope.T;
  root: View.Root;
  drawing: Drawing.T;
  w: Window;
  source: SourceView;
  editor: EditorView;
  psImpl: PSImpl.Impl;
  zeusOption := FALSE;
  animObj: JunoZeus.T;
  disp, geom: TEXT := NIL;
  fileName := "Untitled.juno";
  writepkl := FALSE;
  readpkl  := TRUE;
  configFile: Pathname.T := NIL;
BEGIN
  (* bump minimum stack size to twice the default *)
  Thread.MinDefaultStackSize(6000);

  (* process command-line arguments *)
  VAR
    i := 1; arg: TEXT;
    origin := -1;      (* ordinal of "JunoConfig.Origin" type *)
    orientation := -1; (* ordinal of "JunoConfig.Orientation" type *)
  BEGIN
    WHILE i < Params.Count DO
      arg := Params.Get(i);
      IF Text.Length(arg) = 0 THEN
        PrintUsage("empty argument #" & Fmt.Int(i))
      END;
      IF Text.GetChar(arg, 0) = '-' THEN
        IF Text.Equal(arg, "-zeus") THEN
          zeusOption := TRUE; INC(i)
        ELSIF Text.Equal(arg, "-southwest") THEN
          origin := ORD(JunoConfig.Origin.SW); INC(i)
        ELSIF Text.Equal(arg, "-center") THEN
          origin := ORD(JunoConfig.Origin.Center); INC(i)
        ELSIF Text.Equal(arg, "-portrait") THEN
          orientation := ORD(JunoConfig.Orientation.Portrait); INC(i)
        ELSIF Text.Equal(arg, "-landscape") THEN
          orientation := ORD(JunoConfig.Orientation.Landscape); INC(i)
        ELSIF Text.Equal(arg, "-config") THEN
          INC(i);
          IF i >= Params.Count THEN
            PrintUsage(arg & " option requires an argument")
          END;
          configFile := Params.Get(i); INC(i)
        ELSIF Text.Equal(arg, "-display") THEN
          INC(i);
          IF i >= Params.Count THEN
            PrintUsage(arg & " option requires an argument")
          END;
          disp := Params.Get(i); INC(i)
        ELSIF Text.Equal(arg, "-geometry") THEN
          INC(i);
          IF i >= Params.Count THEN
            PrintUsage(arg & " option requires an argument")
          END;
          geom := Params.Get(i); INC(i);
        ELSIF Text.Equal(arg, "-writepkl") THEN
          writepkl := TRUE
        ELSIF Text.Equal(arg, "-nopkl") THEN
          readpkl := FALSE
        ELSE
          PrintUsage("unrecognized flag: \"" & arg & "\"")
        END
      ELSE
        fileName := arg; INC(i);
        EXIT (* filename only allowed as last argument *)
      END
    END;
    IF i < Params.Count THEN PrintUsage("too many arguments") END;

    TRY
      (* initialize values from configuration file *)
      configFile := JunoConfig.Init(configFile);
  
      (* change overrides from the command-line *)
      IF origin # -1 THEN
  	JunoConfig.origin := VAL(origin, JunoConfig.Origin)
      END;
      IF orientation # -1 THEN
  	JunoConfig.orientation := VAL(orientation, JunoConfig.Orientation)
      END;
    EXCEPT
      OSError.E =>
  	Wr.PutText(Stdio.stderr, "Error: unable to open configuration file \""
  	  & configFile & "\"\n");
  	Process.Exit(1);
    | JunoConfig.Error (msg) =>
  	Wr.PutText(Stdio.stderr, "Error reading configuration file:\n");
  	Wr.PutText(Stdio.stderr, "  " & msg & "\n");
  	Process.Exit(1);
    END
  END;

  TRY
    (* install top-level window *)
    <* FATAL Rsrc.NotFound *>
    BEGIN
      startup := NEW(FormsVBT.T).initFromRsrc(
    	"Startup.fv", JunoRsrc.Path, raw := TRUE)
    END;
    IF NOT writepkl THEN
      JunoWM.Install(startup, disp, geom, applName := "Juno")
    END;

    (* initialize globals *)
    builtInScope := CompileFile(root, "BuiltIn.juno", parent:=NIL).public_scp;
    BuiltInSlots.Init(builtInScope);
    modScope := JunoScope.New(p := builtInScope, size := 50);
    root := NEW(View.Root, ccmd := NIL, marquee := NIL, 
      dTrue := TRUE, astTrue := TRUE, sTrue := FALSE, eTrue := FALSE);
    root.ccmd := CurrCmd.New(ast := JunoAST.SkipVal,
      scp := JunoScope.New(modScope));
    root.marquee := NEW(Marquee.T).init(font := JunoConfig.codeFont);
    drawing := NEW(Drawing.T).init(
      NEW(Drawing.Child).init(JunoConfig.origin), root);
    w := NEW(Window, root := root, fileName := fileName, scope := modScope,
      mu := NEW(MUTEX), untilDone := NEW(Thread.Condition)).init(drawing);
    source := NEW(SourceView, w := w).init(root);
    editor := NEW(EditorView, w := w).init("");
    psImpl := PSImpl.New(root);
    root.source := source;
    root.drawing := drawing;
    root.currView := drawing;
    root.editor := editor;
  
    (* fill in generic windows in top-level form *)
    FormsVBT.PutGeneric(w, "toolbox", w.toolbox);
    FormsVBT.PutGeneric(w, "drawing", NEW(DblBufferVBT.T).init(drawing));
    FormsVBT.PutGeneric(w, "source", ZSplit.New(source));
    FormsVBT.PutGeneric(w, "editor", NEW(TextEditVBT.T, tp := editor).init());

    (* fill in configuration overrides (if any) *)
    LOCK VBT.mu DO
      SetTEFont(FormsVBT.GetVBT(w, "configDefault"),   JunoConfig.codeFont);
      SetTEFont(FormsVBT.GetVBT(w, "configOverrides"), JunoConfig.codeFont);
    END;
    IF configFile # NIL THEN
      VAR wr := TextWr.New(); BEGIN
        TRY
          <* FATAL Wr.Failure *> (* TextWr.T's never fail *)
          VAR rd := FileRd.Open(configFile); BEGIN
            (* copy "rd" to "wr" *)
            WHILE NOT Rd.EOF(rd) DO
              <* FATAL Rd.EndOfFile *>
              BEGIN Wr.PutChar(wr, Rd.GetChar(rd)) END
            END
          END
        EXCEPT
          OSError.E, Rd.Failure =>
            Wr.PutText(wr, "; Error reading configuration file:\n");
            Wr.PutText(wr, "; \"" & configFile & "\"");
        END;
        (* put the text in the "Configure" dialog box *)
        LOCK VBT.mu DO
          FormsVBT.PutText(w, "configOverrides", TextWr.ToText(wr))
        END
      END
    END;

    (* Bind modules with external procedures *)
    JunoScope.Bind(modScope, Atom.FromText("PS"), psImpl);
    JunoScope.Bind(modScope, Atom.FromText("Print"), PrintImpl.New());
    JunoScope.Bind(modScope, Atom.FromText("Random"), RandomImpl.New());
    JunoScope.Bind(modScope, Atom.FromText("Text"), TextImpl.New());
    JunoScope.Bind(modScope, Atom.FromText("Time"), TimeImpl.New());
    JunoScope.Bind(modScope, Atom.FromText("Unit"), UnitImpl.New(root));
    JunoScope.Bind(modScope, JunoUIImpl.ModSym, JunoUIImpl.New(root));

    (* Compile modules specified in "Bundled.modlist" and "Juno.modlist"
       (if any) configuration files. *)
    <* FATAL Rsrc.NotFound *>
    VAR modList: TextList.T := NIL; BEGIN
      CompileModules(w, RsrcOpen("Bundled.modlist"), modList, fromRsrc :=TRUE);
      TRY CompileModules(w, FileRd.Open("Juno.modlist"), modList) EXCEPT
        OSError.E => (* SKIP *)
      END;
      FormsVBT.PutText(startup, "initModule", "");
      FillBrowser(FormsVBT.GetVBT(w, "moduleBrowser"),
        TextListSort.SortD(modList));
    END;
  
    (* Open the built-in modules *)
    LOCK VBT.mu DO
      OpenModule(w, "PS",     hasPrivate := FALSE);
      OpenModule(w, "Print",  hasPrivate := FALSE, show := FALSE);
      OpenModule(w, "Random", hasPrivate := FALSE, show := FALSE);
      OpenModule(w, "Text",   hasPrivate := FALSE, show := FALSE);
      OpenModule(w, "Time",   hasPrivate := FALSE, show := FALSE);
      OpenModule(w, "Unit",   hasPrivate := FALSE, show := FALSE);
      OpenModule(w, "JunoUI", hasPrivate := FALSE, show := FALSE)
    END;
  
    (* Switch TSplit to its first child (i.e, the Juno window) *)
    FormsVBT.PutText(w, "version", JunoVersion.Name);
    FormsVBT.PutGeneric(startup, "mainChild", w);
    FormsVBT.PutInteger(startup, "tsplit", 0);

    (* Load the initial module *)
    LOCK w.mu DO
      w.stat := SyncStat.NotDone;
      <* FATAL VBT.Error *> BEGIN
        VBT.Forge(w, LoadInitialModule)
      END;
      WHILE w.stat = SyncStat.NotDone DO
        Thread.Wait(w.mu, w.untilDone)
      END
    END;
    
    (* Install animation window *)
    IF zeusOption THEN
      animObj := NEW(JunoZeus.T).init(w, w.root, JunoConfig.origin);
      Trestle.Install(animObj.w, windowTitle := "Juno Animation");
      <* FATAL Thread.Alerted *> BEGIN
        NetObj.Export("JunoZeus", animObj)
      END
    END;
  
    FVFilter.MakeActive(w, "background");
    IF NOT writepkl THEN
      checkpointThread := Thread.Fork(NEW(CheckpointClosure, w := w));
      Trestle.AwaitDelete(startup)
    ELSE
      <* FATAL OSError.E, Pickle.Error *>
      VAR wr := FileWr.Open("big.pkl"); BEGIN
        Editor.SaveSlots(wr);
        JunoCompile.SaveSlots(wr);
        Pickle.Write(wr, w);
        Wr.Flush(wr);
        Wr.Close(wr)
      END 
    END
  EXCEPT
  | JunoWM.Error (txt) =>
      PrintUsage(txt)
  | TrestleComm.Failure =>
      Wr.PutText(Stdio.stderr, "Fatal error: ");
      Wr.PutText(Stdio.stderr, "unable to connect to window system.\n");
      Wr.Flush(Stdio.stderr)
  | NetObj.Error =>
      Wr.PutText(Stdio.stderr, "Fatal error: ");
      Wr.PutText(Stdio.stderr, "unable to export JunoZeus object.\n");
      Wr.Flush(Stdio.stderr)
  END
END Juno.
