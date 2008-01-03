(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Mar  1 00:36:12 PST 1997 by heydon                   *)
(*      modified on Mon Jun 12 14:03:54 PDT 1995 by gnelson                  *)
(*      modified on Fri Aug  7 21:51:56 PDT 1992 by myers                    *)
<* PRAGMA LL *>

MODULE Editor EXPORTS Editor, JunoHandleLexErr, EditorUI;

IMPORT JunoError, View, Drawing, ToolBox, EditorXtra, JunoConfig;
IMPORT JunoParse, JunoLex, JunoAST, JunoUnparse, JunoToken;
IMPORT   JunoScope, JunoCompile;
FROM JunoCompileErr IMPORT Error, Raise;
IMPORT JunoRT;
IMPORT TextPort;
IMPORT VBT, Rect, Axis, TextVBT, HVSplit, Split;
IMPORT Rd, Wr, Formatter, TextRd, TextWr, Text, Atom, Lex, Fmt, FloatMode;
IMPORT   AtomAtomTbl, AtomRefTbl;

FROM Thread IMPORT Alerted;

<* FATAL Rd.Failure, Wr.Failure, Alerted *>

CONST
  MinUnparseWidth = 15;
  CmdPrefix = "Cmd";

REVEAL
  T = Public BRANDED "Editor.T" OBJECT
    trees, lastTree: Forest := NIL;
    currentTree: Forest;
    treesValid, textPretty := FALSE;
    width: INTEGER := -1;
    maxCurrCmd: INTEGER := -1;
    toolTypes: AtomAtomTbl.T;
    setMenus: AtomRefTbl.T;
  OVERRIDES
    init     := Init;
    reshape  := Reshape;
    shape    := Shape;
    modified := Modified;
    txtModified := NoOp;
    getToolType := GetToolType;
    getMenu := GetMenu;
  END;

  (* An "Editor.T" is an editor for a Juno module. If "t: T", then "t.trees"
     is the list of parse trees for the top-level blocks of the module.
     "t.trees" holds the truth iff "t.treesValid".  
     
     The value of "currentTree" points at the tree containing the 
     top line of the textport; it is set by Parse and maintained 
     by Unparse, and is valid only if "treesValid" is true.  It
     is used to prevent the textport from scrolling undesirably
     when the user reshapes the editor or remakes it by clicking "Run". 
     
     The boolean "t.textPretty" is TRUE iff
     the editor contains the result of unparsing "t.trees"; note that
     "t.textPretty => t.treesValid". Hence there are 3 combinations for the
     two booleans:

|    Valid  Pretty  Meaning
|      F      F     the editor contains the truth, and is not pretty-printed
|      T      F     "t.trees" is the result of successfully parsing the source
|      T      T     the editor contains the unparsed version of "t.trees"

     If "t.textPretty", then "t.width" is the width at which the trees were
     unparsed. If the trees were unparsed into an empty window, then
     "t.textPretty" is TRUE, and "t.width = -1".

     The editor also implements an abstract "current command stack". The
     procedures declared in the editor with names of the form "CmdPrefix &
     Fmt.Int(X)", where "CmdPrefix" is a global constant and "X" is a
     non-negative integer, are on the stack. The value "X" is called the index
     of the current command. "t.maxCurrCmd" is the value of the maximum
     current command index; this is the current command on the top of the
     stack. The stack is empty iff "t.maxCurrCmd = -1". *)

  Forest = ForestPublic BRANDED "Editor.Forest" OBJECT
    start, end: CARDINAL;
  END;

  (* For each tree "t" in the list of trees, "t.start" and "t.end" are the
     indices in the module editor of the first and last character of the
     unparsed version of "t". This interval includes the whitespace charcters
     following "t". *)

PROCEDURE NoOp(<*UNUSED*> tp: T) =
  BEGIN END NoOp;

PROCEDURE Init(tp: T; src: TEXT; readOnly := FALSE): T =
  BEGIN
    EVAL TextPort.T.init(tp, font := JunoConfig.codeFont,
      wrap := FALSE, readOnly := readOnly);
    TextPort.SetModified(tp, TRUE);
    TextPort.SetText(tp, src);
    TextPort.SetModified(tp, FALSE);
    tp.toolTypes := NEW(AtomAtomTbl.Default).init();
    tp.setMenus := NEW(AtomRefTbl.Default).init();
    RETURN tp
  END Init;

PROCEDURE ScrollToCurrentTree(tp: T) =
  VAR pos: INTEGER; BEGIN
    IF tp.currentTree = NIL THEN
      pos := 0
    ELSE
      pos := tp.currentTree.start
    END;
    EditorXtra.IndexToTop(tp, pos)
  END ScrollToCurrentTree;

PROCEDURE SetCurrentTree(tp: T) =
  VAR 
    cpos := EditorXtra.TopLineIndex(tp);
    f := tp.trees;
  BEGIN
    WHILE f # NIL AND f.end <= cpos DO
      f := f.next
    END;
    tp.currentTree := f
  END SetCurrentTree;

PROCEDURE Reshape(tp: T; READONLY cd: VBT.ReshapeRec) =
  <* LL.sup = VBT.mu.tp *>
  BEGIN
    IF Rect.IsEmpty(cd.new) THEN
      tp.width := -1
    ELSE
      VAR width := Width(tp); BEGIN
        IF tp.treesValid AND (NOT tp.textPretty OR width # tp.width) THEN
          SetCurrentTree(tp);
          Unparse2(tp, width);
          ScrollToCurrentTree(tp)
        END
      END
    END;
    TextPort.T.reshape(tp, cd)
  END Reshape;

PROCEDURE Shape(tp: T; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
  VAR res := TextPort.T.shape(tp, ax, n); BEGIN
    res.lo := 0;
    res.hi := VBT.DefaultShape.hi;
    RETURN res
  END Shape;

PROCEDURE Modified (tp: T) =
  <* LL.sup < VBT.mu *>
  BEGIN
    TextPort.T.modified(tp);
    tp.treesValid := FALSE;
    tp.textPretty := FALSE;
    tp.txtModified()
  END Modified;

PROCEDURE GetToolType(ed: T; nm: Atom.T; VAR (*OUT*) type: Atom.T): BOOLEAN =
  BEGIN
    RETURN ed.toolTypes.get(nm, type)
  END GetToolType;

PROCEDURE GetMenu(ed: T; nm: Atom.T): VBT.T =
  VAR menuRef: REFANY; BEGIN
    IF ed.setMenus.get(nm, menuRef) THEN
      RETURN menuRef
    ELSE
      RETURN TextVBT.New("No parameters have been defined")
    END
  END GetMenu;

PROCEDURE Trees(tp: T): Forest =
  BEGIN
    RETURN tp.trees
  END Trees;

PROCEDURE Valid (tp: T): BOOLEAN =
  BEGIN
    RETURN tp.treesValid
  END Valid;

PROCEDURE HandleLexErr(
    err: JunoLex.ErrorRec;
    rd: Rd.T;
    wr: Wr.T;
    VAR (*OUT*) start, finish: INTEGER) =
  BEGIN
    Wr.PutText(wr, "\n");
    CASE err.kind OF <* NOWARN *>
    | JunoLex.ErrorKind.UnclosedComment, JunoLex.ErrorKind.UnclosedText =>
        start := Wr.Index(wr);
        Wr.PutText(wr, err.initialChars);
        Wr.PutText(wr, Rd.GetText(rd, LAST(CARDINAL)));
        finish := Wr.Index(wr)
    | JunoLex.ErrorKind.BadInitialChar, JunoLex.ErrorKind.BadEscapeChar,
        JunoLex.ErrorKind.BadReal =>
      Wr.PutText(wr, err.initialChars);
      start := Wr.Index(wr);
      finish := start + 1;
      Wr.PutText(wr, Rd.GetText(rd, LAST(CARDINAL)));
      IF start = Wr.Index(wr) THEN Wr.PutChar(wr, ' ') END
    END
  END HandleLexErr;

PROCEDURE CurrCmdIndex(ast: JunoAST.T): INTEGER =
(* If "ast" is a procedure declaration for a procedure whose name has the
   value of the global constant "CmdPrefix" as a prefix, then return the value
   of the suffix; otherwise, return -1. *)
  BEGIN
    TYPECASE ast OF
      JunoAST.ProcDecl (pd) =>
        VAR
          procName := Atom.ToText(pd.header.name);
          prefixLen := Text.Length(CmdPrefix);
          res: INTEGER;
        BEGIN
          IF Text.Equal(CmdPrefix, Text.Sub(procName, 0, prefixLen)) THEN
            TRY res := Lex.Int(TextRd.New(Text.Sub(procName, prefixLen)))
            EXCEPT Lex.Error, FloatMode.Trap, Rd.Failure => res := -1
            END;
            RETURN res
          END
        END
    ELSE (* SKIP *)
    END;
    RETURN -1
  END CurrCmdIndex;

PROCEDURE Parse(tp: T; time: VBT.TimeStamp): BOOLEAN =
  BEGIN
    IF tp.treesValid THEN RETURN TRUE END;
    RETURN Parse2(tp, time)
  END Parse;

PROCEDURE Parse2(tp: T; time: VBT.TimeStamp): BOOLEAN =
  VAR
    errmsg: TEXT;
    rd := TextRd.New(TextPort.GetText(tp));
    wr: TextWr.T;
    w := Width(tp);
    start, finish := -1;
    cpos := EditorXtra.TopLineIndex(tp);
    ip: JunoParse.IterativeParse;
  <*FATAL Rd.Failure, Wr.Failure *>
  BEGIN
    TRY ip := JunoParse.StartIterativeParse(rd) EXCEPT
      JunoLex.Error (err) =>
      	wr := TextWr.New();
      	errmsg := JunoLex.ErrorText(err.kind);
      	HandleLexErr(err, rd, wr, start, finish);
      	ip := NIL
    END;
    tp.trees := NIL; 
    tp.lastTree := NIL;
    tp.currentTree := NIL;
    tp.maxCurrCmd := -1;
    IF ip # NIL THEN
  	LOOP
  	  VAR ast: JunoAST.Block; tokens: CARDINAL; BEGIN
  	    TRY
  	      JunoParse.Block(ip, ast, tokens)
  	    EXCEPT
  	      JunoLex.Error (err) =>
              	wr := TextWr.New();
              	UnparseTrees(tp.trees, wr, w);
  		IF ast # NIL THEN
  		  JunoUnparse.Block(wr, ast, tokens,
                    indent := 0, width := w, prec := JunoConfig.realPrec)
  		END;
              	errmsg := JunoLex.ErrorText(err.kind);
  		  HandleLexErr(err, rd, wr, start, finish);
              	EXIT
  	    | JunoParse.Error(err) =>
                wr := TextWr.New();
                UnparseTrees(tp.trees, wr, w);
  		IF ast # NIL THEN
  		  JunoUnparse.Block(wr, ast, tokens,
                    indent := 0, width := w, prec := JunoConfig.realPrec)
  		END;
  		Wr.PutChar(wr, '\n');
  		errmsg := "Parse error";
  		IF err.expected # JunoToken.Kind.Unknown THEN
  		  errmsg := errmsg & " (expected "
  		    & JunoToken.KindName[err.expected] & ")"
  		END;
  		start := Wr.Index(wr);
  		Wr.PutText(wr, JunoToken.ToText(err.found));
  		finish := Wr.Index(wr);
  		Wr.PutChar(wr, ' ');
  		Wr.PutText(wr, err.additional);
  		Wr.PutText(wr, Rd.GetText(rd, LAST(CARDINAL)));
  		EXIT
  	    END;
  	    IF ast = NIL THEN EXIT END;
            (* Next, append "ast" to the list ending at "tp.lastTree".  Also,
               if it is the first parsed item that ends past the top line of
               the textport, record it in "currentTree". *)
            VAR f := NEW(Forest, tree := ast, next := NIL); BEGIN
              AppendTree(tp, f);
  	      IF tp.currentTree = NIL AND JunoParse.GetIndex(ip) > cpos THEN
  	        tp.currentTree := f
  	      END
  	    END;
            tp.maxCurrCmd := MAX(tp.maxCurrCmd, CurrCmdIndex(ast))
  	  END
  	END;                      (* loop *)
  	JunoParse.FinishIterativeParse(ip)
    END;                        (* if *)
    IF start # finish THEN
      <* ASSERT start # -1 AND finish # -1 *>
      TextPort.SetModified(tp, TRUE);
      TextPort.SetText(tp, TextWr.ToText(wr));
      TextPort.SetModified(tp, FALSE);
      Wr.Close(wr);
      JunoError.P(tp, errmsg, start, finish, time);
      RETURN FALSE
    ELSE
      TextPort.SetModified(tp, FALSE);
      TextPort.Normalize(tp, cpos);
      tp.treesValid := TRUE;
      RETURN TRUE
    END
  END Parse2;

PROCEDURE Unparse(tp: T; errast: JunoAST.T := NIL;
  msg: TEXT := NIL; time: VBT.TimeStamp := 0) =
  <* LL.sup < tp *>
  BEGIN
    <* ASSERT tp.treesValid *>
    <* ASSERT (errast # NIL) = (msg # NIL) *>
    IF Rect.IsEmpty(VBT.Domain(tp)) THEN
      tp.width := -1
    ELSE
      VAR width := Width(tp); BEGIN
        IF NOT tp.textPretty OR width # tp.width THEN
          Unparse2(tp, width, errast, msg, time)
        END
      END;
      IF errast = NIL THEN ScrollToCurrentTree(tp) END
    END
  END Unparse;

PROCEDURE Unparse2(tp: T; width: CARDINAL;
  errast: JunoAST.T := NIL; msg: TEXT := NIL; time: VBT.TimeStamp := 0) =
  <* FATAL Wr.Failure *>
  VAR wr := TextWr.New(); BEGIN
    UnparseTrees(tp.trees, wr, width, errast := errast);
    VAR txt := TextWr.ToText(wr); start, finish: INTEGER; BEGIN
      IF errast # NIL THEN
  	start := Text.FindChar(txt, '\001');
        finish := Text.FindChar(txt, '\002');
        <* ASSERT start # -1 AND finish # -1 *>
  	txt := Text.Sub(txt, 0, start)
  	  & Text.Sub(txt, start + 1, finish - start - 1)
  	  & Text.Sub(txt, finish + 1)
      END;
      TextPort.SetModified(tp, TRUE);
      TextPort.SetText(tp, txt);
      TextPort.SetModified(tp, FALSE);
      IF errast # NIL THEN
        VAR t := tp.trees; BEGIN
          (* update the "start" and "end" values for blocks appearing after
             the erroneous tree "errast" *)
          WHILE t # NIL DO
            IF t.start > finish THEN
              DEC(t.start, 2); DEC(t.end, 2)
            ELSIF t.end > finish THEN
              DEC(t.end, 2)
            END;
            t := t.next
          END
        END;
        IF time # 0 THEN
          JunoError.P(tp, msg, start, finish - 1, time)
        END
      END;
      Wr.Close(wr)
    END;
    tp.textPretty := TRUE;
    tp.width := width
  END Unparse2;

PROCEDURE UnparseTrees(f: Forest; wr: Wr.T;
  width: CARDINAL; errast: JunoAST.T  := NIL) =
(* Unparse the list of trees "f" to "wr" at the width "width". If "errast #
   NIL", then bracket the unparsing of "errast" by the characters '\001' and
   '\002' when unparsing. This procedure also sets the "start" and "end"
   fields of each tree in "f". *)
  VAR fmt := Formatter.New(wr, width); BEGIN
    WHILE f # NIL DO
      f.start := Wr.Index(wr);
      JunoUnparse.ToFmt(fmt, f.tree, indent := 0,
        prec := JunoConfig.realPrec, errast := errast);
      Formatter.NewLine(fmt, freshLine := FALSE);
      (* Print a second newline so long as it would not separate two
         consecutive UI declarations. *)
      IF NOT (ISTYPE(f.tree, JunoAST.UIDecl) AND f.next # NIL AND
              ISTYPE(f.next.tree, JunoAST.UIDecl)) THEN
        Formatter.NewLine(fmt, freshLine := FALSE)
      END;
      Formatter.Flush(fmt);
      f.end := Wr.Index(wr);
      f := f.next
    END;
    Formatter.Close(fmt)
  END UnparseTrees;

PROCEDURE AppendTree(tp: T; t: Forest) =
(* Append "t" to "tp"'s list of trees. *)
  BEGIN
    IF tp.trees = NIL
      THEN tp.trees := t
      ELSE tp.lastTree.next := t
    END;
    tp.lastTree := t
  END AppendTree;

PROCEDURE AddTree(ed: T; ast: JunoAST.T) =
  VAR t := NEW(Forest, tree := ast); BEGIN
    AppendTree(ed, t);
    ed.maxCurrCmd := MAX(ed.maxCurrCmd, CurrCmdIndex(ast));
    (* unparse the new tree to the end of the editor *)
    VAR wr := TextWr.New(); BEGIN
      t.start := TextPort.Length(ed);
      JunoUnparse.P(wr, ast, 0, Width(ed),
        prec := JunoConfig.realPrec, errast := NIL);
      Wr.PutText(wr, "\n\n");
      VAR wasMod := TextPort.IsModified(ed); BEGIN
      	TextPort.SetModified(ed, TRUE);
      	TextPort.PutText(ed, TextWr.ToText(wr));
      	t.end := TextPort.Length(ed);
      	TextPort.Normalize(ed, t.start);
      	TextPort.SetModified(ed, wasMod)
      END;
      Wr.Close(wr)
    END;
    (* update the editor's tables for a UIDecl *)
    TYPECASE ast OF JunoAST.UIDecl (ui) =>
      <* FATAL Error *> BEGIN
      	IF ed.toolTypes.put(FirstName(ui, 1).id1, ui.name) THEN
      	  <* ASSERT FALSE *>
        END
      END
    ELSE (* SKIP *)
    END
  END AddTree;

PROCEDURE NextCmdNum(ed: T): CARDINAL =
  BEGIN RETURN ed.maxCurrCmd + 1 END NextCmdNum;

PROCEDURE NextCmdName(ed: T): Atom.T =
  BEGIN
    RETURN Atom.FromText(CmdPrefix & Fmt.Int(NextCmdNum(ed)))
  END NextCmdName;

PROCEDURE PopCurrCmd(ed: T; VAR (*OUT*) nm: JunoAST.Id): JunoAST.Cmd =
  <* LL.sup <= VBT.mu *>
  BEGIN
    IF NOT ed.treesValid OR ed.maxCurrCmd < 0 THEN RETURN NIL END;
    Unparse(ed);
    VAR t: Forest := NIL; BEGIN
      VAR curr := ed.trees; prev: Forest := NIL; max := -1; BEGIN
      	(* Set "t" to the tree to delete, set "max" to the new current
           command maximum, and set "prev" to the tree before "t" (or
           "NIL" if "t" is the first tree in the list). *)
        WHILE curr # NIL DO
          VAR ix := CurrCmdIndex(curr.tree); BEGIN
            IF ix = ed.maxCurrCmd
              THEN t := curr
              ELSE max := MAX(max, ix)
            END
          END;
          IF t = NIL THEN prev := curr END;
          curr := curr.next
      	END;
        ed.maxCurrCmd := max;
      	(* remove "t" from "trees[ed]" *)
        IF ed.lastTree = t THEN
          ed.lastTree := prev
        END;
      	IF prev = NIL
      	  THEN ed.trees := t.next
      	  ELSE prev.next := t.next
      	END;
      END;
      (* delete the text for "t" from "src[ed]" *)
      VAR wasMod := TextPort.IsModified(ed); BEGIN
        TextPort.SetModified(ed, TRUE);
        TextPort.Replace(ed, t.start, t.end, "");
        TextPort.SetModified(ed, wasMod)
      END;
      (* return the procedure body *)
      VAR decl := NARROW(t.tree, JunoAST.ProcDecl); body := decl.body; BEGIN
        nm := decl.header.name;
        TYPECASE body OF JunoAST.If (if) =>
          IF ISTYPE(if.body, JunoAST.Proj)
            THEN RETURN if.body
            ELSE RETURN body
          END
        ELSE RETURN body
        END
      END
    END
  END PopCurrCmd;

PROCEDURE Width(ed: TextPort.T): CARDINAL =
  VAR res := VBT.TextWidth(ed, "m", ed.getFont()); BEGIN
    IF res # 0 THEN
      res := Rect.HorSize(VBT.Domain(ed)) DIV res - 2;
    END;
    RETURN MAX(MinUnparseWidth, res)
  END Width;

PROCEDURE ModuleName(ed: T): Atom.T =
  VAR first := ed.trees; BEGIN
    WHILE first # NIL DO
      TYPECASE first.tree OF
        JunoAST.Module (m) => RETURN m.name
      | JunoAST.Comment => first := first.next
      ELSE EXIT
      END
    END;
    RETURN NIL
  END ModuleName;

VAR (* CONST *)
  global_mod := Atom.FromText("_GLOBAL_MOD");
  global_cmd := Atom.FromText("_GLOBAL_CMD");
  global_slot := JunoRT.GetCodeIndex(JunoRT.ProcAttr{
    global_mod, global_cmd, JunoRT.Sig{0,0,0}});

PROCEDURE ProcessExecRes(READONLY res: JunoRT.ExecRes; error_ast: JunoAST.T)
    RAISES {Error} =
(* Raises "Error" if "res.trapCode # JunoRT.TrapCode.NormalHalt", with a
   message constructed from "res.errorCode", and with error AST "error_ast". *)
  BEGIN
    IF res.trapCode # JunoRT.TrapCode.NormalHalt THEN
      Raise(JunoRT.TrapMessage(res), error_ast)
    END
  END ProcessExecRes;

PROCEDURE Pass0(
    VAR forest: Forest;
    scp: JunoScope.T;
    uniqueModName: BOOLEAN;
    VAR (*OUT*) mod: JunoAST.Id)
  : JunoScope.T RAISES {Error} =
(* Process the "MODULE" and "IMPORT" declarations in "forest", set "mod" to
   the name of the initial module declaration (or NIL if there is none), and
   set "forest" to point to the first declaration after the longest prefix
   of the form "<comment>* <module> (<comment> | <import>)*".

   Returns a restricted version of "scp" as determined by any IMPORT
   statements, or "scp" itself if there were no IMPORT statements. This
   implementation assumes that all bundled modules are defined in "scp", and
   that identifiers for "BuiltIn.juno" are defined in proper ancestor scopes
   of "scp".

   If "uniqueModName = TRUE", then any specified module name must not appear
   in "scp"; if it does, "Error" is raised. Similarly, any modules specified
   in an "IMPORT" statement must appear in "scp"; if they do not, "Error" is
   raised. *)
  VAR res: JunoScope.T := NIL; BEGIN
    mod := NIL;
    WHILE forest # NIL DO
      TYPECASE forest.tree OF
      | JunoAST.Comment => (* SKIP *)
      | JunoAST.Module (md) =>
          IF mod # NIL THEN EXIT END;
          IF uniqueModName AND JunoScope.Lookup(scp, md.name) # NIL THEN
            Raise("A \"" & Atom.ToText(md.name) &
              "\" module is already defined", md)
          END;
          mod := md.name
      ELSE EXIT
      END;
      forest := forest.next
    END;
    WHILE forest # NIL DO
      TYPECASE forest.tree OF
        JunoAST.Comment => (* SKIP *)
      | JunoAST.Import (imp) =>
          (* form new scope if necessary *)
          IF res = NIL THEN res := JunoScope.New(JunoScope.Parent(scp)) END;

          (* copy imported modules bound in "scp" to "res" *)
          VAR curr := imp.idList.head; ent: JunoScope.Entity; BEGIN
            WHILE curr # NIL DO
              ent := JunoScope.Lookup(scp, curr.id, localOnly := TRUE);
              IF ent = NIL THEN
            	Raise("\""& Atom.ToText(curr.id) &"\" is not a bundled module",
            	  imp.idList)
              END;
              TRY JunoScope.Bind(res, curr.id, ent) EXCEPT
                JunoScope.NameClash =>
                  Raise("\""& Atom.ToText(curr.id) &"\" repeated in IMPORTs",
            	    imp.idList)
              END;
              curr := curr.next
            END
          END
      ELSE EXIT
      END;
      forest := forest.next
    END;
    IF res = NIL
      THEN RETURN scp
      ELSE RETURN res
    END
  END Pass0;

PROCEDURE Pass1(
    forest: Forest;
    public, scp: JunoScope.T;
    mod: JunoAST.Id)
    RAISES {Error} =
(* Pass1 processes the top-level declarations in "forest" for the module named
   "mod". "Forest" is assumed to have been produced by Pass0, so it does not
   contain the MODULE and IMPORT declarations at the start of the module.
   Pass1 treats each type of top-level declaration as follows:
|
|    (* Comment *)      Skip.
|    MODULE, IMPORT     Raise Error.
|    CONST, VAR, PROC   Only install entries in "scp" (and "public").
|    PRED, FUNC         Install entries in "scp" (and "public") and compile
|                         bodies in order of occurrence.
|    UI                 Skip.
|
   Entries are only installed in the "public" scope if the declaration is not
   PRIVATE. *)
  <* FATAL JunoScope.NameClash *>
  BEGIN
    WHILE forest # NIL DO
      TYPECASE forest.tree OF <*NOWARN*>
      | JunoAST.Module (md) =>
          IF mod = NIL
            THEN Raise("MODULE header not at start of file", md)
            ELSE Raise("Only one MODULE header is allowed", md)
          END
      | JunoAST.Import (import) =>
          Raise("IMPORT may only be preceded by MODULE header", import)
      | JunoAST.Comment => (* SKIP *)
      | JunoAST.ConstDecl (cd) =>
          VAR curr := cd.head; BEGIN
            WHILE curr # NIL DO
              IF JunoScope.Lookup(scp, curr.name) # NIL THEN
                Raise("\""&Atom.ToText(curr.name)&"\" is already declared", cd)
              END;
              VAR
                c := NEW(JunoScope.Const, init := curr.value,
                  index := JunoRT.GetVarIndex(mod, curr.name));
              BEGIN
                JunoScope.Bind(scp, curr.name, c);
                IF NOT cd.private THEN
                  JunoScope.Bind(public, curr.name, c)
                END
              END;
              curr := curr.next
            END
          END
      | JunoAST.VarDecl (vd) =>
          VAR curr := vd.head; BEGIN
            WHILE curr # NIL DO
              IF JunoScope.Lookup(scp, curr.name) # NIL THEN
                Raise("\""&Atom.ToText(curr.name)&"\" is already declared", vd)
              END;
              VAR
                v := NEW(JunoScope.Var, init := curr.value,
                  index := JunoRT.GetVarIndex(mod, curr.name));
              BEGIN
                JunoScope.Bind(scp, curr.name, v);
                IF NOT vd.private THEN
                  JunoScope.Bind(public, curr.name, v)
                END
              END;
              curr := curr.next
            END
          END
      | JunoAST.ProcDecl (proc) =>
          WITH pnm = proc.header.name DO
            IF JunoScope.Lookup(scp, pnm) # NIL THEN
              Raise("\"" & Atom.ToText(pnm) & "\" is already declared",
                proc.header)
            END;
            VAR p := JunoScope.NewProc(proc, mod); BEGIN
              JunoScope.Bind(scp, pnm, p);
              IF NOT proc.private THEN JunoScope.Bind(public, pnm, p) END
            END
          END
      | JunoAST.PredDecl (pred) =>
          WITH pnm = pred.header.name DO
            IF JunoScope.Lookup(scp, pnm) # NIL THEN
              Raise("\"" & Atom.ToText(pnm) & "\" is already declared",
                pred.header)
            END;
            VAR p := JunoScope.NewPred(pred, mod); BEGIN
              JunoCompile.PredDecl(pnm, p, scp);
              JunoScope.Bind(scp, pnm, p);
              IF NOT pred.private THEN JunoScope.Bind(public, pnm, p) END
            END
          END
      | JunoAST.FuncDecl (func) =>
          WITH fnm = func.header.name DO
            IF JunoScope.Lookup(scp, fnm) # NIL THEN
              Raise("\"" & Atom.ToText(fnm) & "\" is already declared",
                func.header)
            END;
            VAR f := JunoScope.NewFunc(func, mod); BEGIN
              JunoCompile.FuncDecl(fnm, f, scp);
              JunoScope.Bind(scp, fnm, f);
              IF NOT func.private THEN JunoScope.Bind(public, fnm, f) END
            END
          END
      | JunoAST.UIDecl => (* SKIP *)
      END;
      forest := forest.next
    END
  END Pass1;

PROCEDURE Pass2(forest: Forest; scp: JunoScope.T) RAISES {Error} =
(* Compile procedure bodies. *)
  BEGIN
    WHILE forest # NIL DO
      TYPECASE forest.tree OF
      | JunoAST.ProcDecl(proc) =>
          VAR p: JunoScope.Proc :=
            JunoScope.Lookup(scp, proc.header.name, localOnly := TRUE);
          BEGIN
            EVAL JunoCompile.ProcDecl(proc.header.name, p, scp)
          END
      ELSE (* SKIP *)
      END;
      forest := forest.next
    END
  END Pass2;

PROCEDURE Pass3(forest: Forest; scp: JunoScope.T) RAISES {Error} =
(* Compile and run constant and global variable initializers. *)
  BEGIN
    WHILE forest # NIL DO
      TYPECASE forest.tree OF
      | JunoAST.ConstDecl (cd) =>
          VAR curr := cd.head; BEGIN
            WHILE curr # NIL DO
              VAR
                c: JunoScope.Const := JunoScope.Lookup(
                  scp, curr.name, localOnly := TRUE);
                res_slot: CARDINAL;
              BEGIN
                JunoRT.code_tbl[global_slot] := JunoCompile.Expr(
                  c.init, scp, curr.name, (*OUT*) res_slot, pure := FALSE);
                ProcessExecRes(JunoRT.ExecFromSlot(global_slot), cd);
                JunoRT.value_tbl[c.index] := JunoRT.value_tbl[res_slot]
              END;
              curr := curr.next
            END
          END
      | JunoAST.VarDecl (vd) =>
          VAR curr := vd.head; BEGIN
            WHILE curr # NIL DO
              VAR
                v: JunoScope.Var := JunoScope.Lookup(
                  scp, curr.name, localOnly := TRUE);
                res_slot: CARDINAL;
                init: JunoAST.Expr := v.init;
              BEGIN
                IF init = JunoAST.NilExpr THEN init := JunoAST.NilVal END;
                JunoRT.code_tbl[global_slot] := JunoCompile.Expr(
                  init, scp, curr.name, res_slot, pure := FALSE);
                ProcessExecRes(JunoRT.ExecFromSlot(global_slot), vd);
                JunoRT.value_tbl[v.index] := JunoRT.value_tbl[res_slot]
              END;
              curr := curr.next
            END
          END
      ELSE (* SKIP *)
      END;
      forest := forest.next
    END
  END Pass3;
    
PROCEDURE Compile(
    te: T;
    time: VBT.TimeStamp;
    scp: JunoScope.T;
    VAR (*OUT*) nm: JunoAST.Id;
    VAR (*OUT*) entity: JunoScope.Mod;
    uniqueModName := TRUE): BOOLEAN =
  <* LL.sup < te *>
  BEGIN
    IF NOT Parse(te, time) THEN RETURN FALSE END;
    RETURN Compile2(te, time, scp, uniqueModName, nm, entity)
  END Compile;

PROCEDURE Compile2(
    te: T;
    time: VBT.TimeStamp;
    parent: JunoScope.T;
    uniqueModName: BOOLEAN;
    VAR (*OUT*) nm: JunoAST.Id;
    VAR (*OUT*) entity: JunoScope.Mod)
  : BOOLEAN =
  <* LL.sup < te *>
  VAR forest := te.trees; restrict, public, scp: JunoScope.T; BEGIN
    TRY
      restrict := Pass0(forest, parent, uniqueModName, nm);
      (* Initialize "public", "scp" so module is compiled under restricted
         scope. *)
      public := JunoScope.New(restrict);
      scp := JunoScope.New(restrict);
      Pass1(forest, public, scp, nm);
      Pass2(forest, scp);
      Pass3(forest, scp);
      (* Make "parent" the parent scope of "public" and "scp" *)
      IF restrict # parent THEN
      	JunoScope.SetParent(public, parent);
      	JunoScope.SetParent(scp, parent)
      END
    EXCEPT
      Error (err) =>
        <* ASSERT err.ast # NIL *>
        te.textPretty := FALSE; (* for error to be unparsed *)
        Unparse(te, err.ast, err.msg, time);
        RETURN FALSE
    END;
    entity := NEW(JunoScope.Mod, public_scp := public, scp := scp);
    RETURN TRUE
  END Compile2;

TYPE InCnt = { EqualsZero, EqualsOne, AtLeastOne, Any };

PROCEDURE Pass4(rt: View.Root; ed: T; scp: JunoScope.T) RAISES {Error} =
(* Compile and process UI declarations. *)
  VAR forest := ed.trees; BEGIN
    (* clear the "UI" tables *)
    EVAL NARROW(ed.toolTypes, AtomAtomTbl.Default).init(
      sizeHint := ed.toolTypes.size());
    EVAL NARROW(ed.setMenus, AtomRefTbl.Default).init(
      sizeHint := ed.setMenus.size());

    WHILE forest # NIL DO
      TYPECASE forest.tree OF
      | JunoAST.UIDecl (ui) =>
          IF ui.name = PointToolSym OR ui.name = TextToolSym
             OR ui.name = SetToolSym OR ui.name = TemplToolSym THEN
            VAR nm: JunoAST.QId; ent: JunoScope.Entity; BEGIN
              nm := FirstName(ui, argCnt := 1);
              ent := CheckEnt(nm, scp);
              IF ui.name = PointToolSym THEN
                IF NOT ISTYPE(ent, JunoScope.Code) THEN
                  (* not a predicate, function, or procedure *)
                  Raise("Must be a predicate, function, or procedure", nm)
                END;
                IF ISTYPE(ent, JunoScope.Proc) THEN
                  CheckProc(ent, nm)	 (* check for no OUT or INOUT args *)
                END
              ELSIF ui.name = TextToolSym THEN
                CheckProc(ent, nm, InCnt.AtLeastOne)
              ELSIF ui.name = SetToolSym THEN
                CheckProc(ent, nm, InCnt.EqualsOne)
              ELSIF ui.name = TemplToolSym THEN
                CheckProc(ent, nm, InCnt.EqualsZero)
              END;
              IF ed.toolTypes.put(nm.id1, ui.name) THEN
                Raise("Duplicate UI declaration", nm)
              END
            END
          ELSIF ui.name = ParamSym THEN
            VAR 
              nm := FirstName(ui, argCnt := 2);
              ent := CheckEnt(nm, scp);
              valueAST := ui.args.head.next.expr;
              mod := ModuleName(ed);
              buttonName: TEXT;
              button: VBT.T;
              menu: VBT.T;
              menuRef: REFANY;
            BEGIN
              CheckProc(ent, nm, InCnt.EqualsOne);
              TYPECASE valueAST OF
                JunoAST.LitValue => (*SKIP*)
              | JunoAST.QId (qid) =>
                  (* Check that "qid" names a legal term *)
                  VAR res, unit: JunoScope.Entity; BEGIN
                    res := JunoScope.LookupQId(scp, qid, unit);
                    TYPECASE res OF
                      NULL =>
                        Raise("Unknown identifier", qid)
                    | JunoScope.Const, JunoScope.Var, JunoScope.Proc =>
                        (* SKIP - these are legal terms *)
                    ELSE
                        Raise("Parameter value must be\n"
                          & "a CONST, VAR, or PROC", qid)
                    END
                  END;
                  valueAST := Qualify(qid, mod)
              ELSE
                Raise("Parameter value must be a\n"
                  & "(qualified) identifier or literal", valueAST)
              END;
              <* FATAL Wr.Failure *>
              VAR twr := NEW(TextWr.T).init(); BEGIN
                JunoUnparse.Expr(twr, valueAST, tokens := LAST(INTEGER),
                  width := LAST(INTEGER), prec := JunoConfig.realPrec);
                buttonName := TextWr.ToText(twr)
              END;
              button :=
                NEW(ToolBox.SetButton).init(rt, buttonName, 
                  Drawing.NewSetTool(Qualify(nm, mod), valueAST));
              IF NOT ed.setMenus.get(nm.id1, menuRef) THEN
                menu := NEW(HVSplit.T).init(Axis.T.Ver);
                EVAL ed.setMenus.put(nm.id1, menu)
              ELSE
                menu := menuRef
              END;
              Split.AddChild(menu, button)
            END
          ELSE
            Raise("Unknown UI declaration", ui)
          END
      ELSE (* SKIP *)
      END;
      forest := forest.next
    END
  END Pass4;
  
PROCEDURE Qualify(qid: JunoAST.QId; mod: Atom.T): JunoAST.QId =
(* If "qid" is unqualified and "mod # NIL", return "mod . qid.id1", else
   return "qid". *)
  BEGIN
    IF qid.id0 = JunoAST.NilId AND mod # NIL
      THEN RETURN NEW(JunoAST.QId, bp := qid, id0 := mod, id1 := qid.id1)
      ELSE RETURN qid
    END
  END Qualify;

PROCEDURE FirstName(ui: JunoAST.UIDecl; argCnt: CARDINAL): JunoAST.QId
    RAISES {Error} =
(* Checks that "ui" has "argCnt" arguments, which is required to be non-zero.
   If so, returns the unqualified identifier that is the first argument.
   Raises "Error" with the appropriate error message if the first argument is
   not an unqualified identifier. *)
  BEGIN
    <* ASSERT argCnt > 0 *>
    IF ui.args.size # argCnt THEN
      VAR errAST: JunoAST.T; BEGIN
        IF ui.args.size = 0 THEN errAST := ui ELSE errAST := ui.args END;
        Raise("Wrong number of arguments", errAST)
      END
    END;
    TYPECASE ui.args.head.expr OF
      JunoAST.QId (qid) =>
        IF qid.id0 # JunoAST.NilId THEN
          Raise("Expecting unqualified identifier", qid)
        END;
        RETURN qid
    ELSE
      Raise("Expecting an identifier", ui.args.head.expr);
      RETURN NIL (* not reached -- just to surpress compiler warning *)
    END
  END FirstName;

PROCEDURE CheckEnt(qid: JunoAST.QId; scp: JunoScope.T): JunoScope.Entity
    RAISES {Error} =
(* Returns the entity bound to "qid" in "scp". Requires that "qid" is
   unqualified. Raises "Error" with an appropriate error message if "qid" is
   not bound in "scp". *)
  VAR res := JunoScope.Lookup(scp, qid.id1); BEGIN
    IF res = NIL THEN
      Raise("Undefined", qid)
    END;
    RETURN res
  END CheckEnt;

PROCEDURE CheckProc(ent: JunoScope.Entity; ast: JunoAST.T; inCnt := InCnt.Any)
    RAISES {Error} =
(* Check that "ent" is a procedure with no OUT or INOUT parameters. If "inCnt"
   is "EqualsOne", then the procedure must have exactly one IN argument; if it
   is "AtLeastOne", then it must have at least one IN argument. Raises "Error"
   so that "ast" will be highlighted if any of these checks fail; otherwise,
   this procedure is a no-op. *)
  BEGIN
    TYPECASE ent OF JunoScope.Proc (p) =>
      IF p.out_cnt # 0 OR p.inout_cnt # 0 THEN
        Raise("Procedure may not have any\nOUT or INOUT arguments", ast)
      END;
      IF inCnt = InCnt.EqualsOne AND p.in_cnt # 1 THEN
        Raise("Procedure must have\nexactly one IN argument", ast)
      ELSIF inCnt = InCnt.AtLeastOne AND p.in_cnt = 0 THEN
        Raise("Procedure must have\nat least one IN argument", ast)
      ELSIF inCnt = InCnt.EqualsZero AND p.in_cnt # 0 THEN
        Raise("Procedure must have\nno IN arguments", ast)
      END
    ELSE Raise("Must be a procedure", ast)
    END
  END CheckProc;

PROCEDURE CompileUI(
    rt: View.Root;
    te: T;
    time: VBT.TimeStamp;
    scp: JunoScope.T): BOOLEAN =
BEGIN
    <* ASSERT te.treesValid *>
    TRY
      Pass4(rt, te, scp)
    EXCEPT
      Error (err) =>
        <* ASSERT err.ast # NIL *>
        Unparse(te, err.ast, err.msg, time);
        RETURN FALSE
    END;
    RETURN TRUE
END CompileUI;

PROCEDURE SaveSlots(wr: Wr.T) =
  BEGIN
    Wr.PutText(wr, Fmt.Int(global_slot) & "\n")
  END SaveSlots;

PROCEDURE RestoreSlots(rd: Rd.T) =
  <* FATAL FloatMode.Trap, Lex.Error, Rd.Failure, Rd.EndOfFile *>
  BEGIN
    global_slot := Lex.Int(rd);
    IF Rd.GetChar(rd) # 'n' THEN <* ASSERT FALSE *> END
  END RestoreSlots;

BEGIN
  PointToolSym := Atom.FromText("PointTool");
  TextToolSym  := Atom.FromText("TextTool");
  SetToolSym   := Atom.FromText("SetTool");
  ParamSym     := Atom.FromText("Param");
  TemplToolSym := Atom.FromText("Template");
END Editor.
