(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Dec 15 10:43:46 PST 1997 by heydon                   *)
(*      modified on Tue Feb  7 11:43:41 PST 1995 by gnelson                  *)
<* PRAGMA LL *>
<* PRAGMA SPEC *>

MODULE ToolBox;

IMPORT View, Drawing, Editor, Source, CurrCmd, JunoConfig;
IMPORT JunoAST, JunoASTUtils;
IMPORT VBT, TextVBT, Filter, PaintOp, Split, BorderedVBT;
IMPORT   ButtonVBT, MenuBtnVBT, AnchorBtnVBT;
IMPORT Atom;

REVEAL
  Button = ButtonPublic BRANDED "ToolBox.Button" OBJECT
    root: View.Root;
    tool: Drawing.ArgTool;
  OVERRIDES
    init := ButtonInit;
  END;

REVEAL
  SetButton = SetButtonPublic BRANDED "ToolBox.SetButton" OBJECT
    root: View.Root;
    tool: Drawing.SetTool;
  OVERRIDES
    init := SetButtonInit
  END;

TYPE
  PointButton = Button BRANDED "ToolBox.PointButton" OBJECT END;
  TextButton = Button BRANDED "ToolBox.TextButton" OBJECT END;
  SetAnchorBtn = AnchorBtnVBT.T BRANDED "ToolBox.SetAnchorBtn" OBJECT
    ed: Editor.T;
    nm: Atom.T;
  OVERRIDES
    pre := SetAnchorBtnPre
  END;
  TemplButton = Button BRANDED "TemplButton" OBJECT cmd: JunoAST.Cmd END;

(* These are the types for the three kinds of buttons appearing in a
   toolbox. The first two are normal buttons for point tools and text tools,
   and the third is an anchor for a set tool menu. That menu will be a VSplit
   of "SetButton"'s.

   If "btn: SetAnchorBtn", then "btn.ed" is the editor in which the procedure
   for this button appears, and "nm" is the name of the button's procedure.*)

VAR (* CONST *)
  toolColor := ARRAY BOOLEAN OF PaintOp.ColorQuad{
    PaintOp.bgFg, PaintOp.MakeColorQuad(PaintOp.Fg, PaintOp.Bg)};

PROCEDURE ButtonText(name: TEXT): TextVBT.T =
(* Return a "TextVBT.T" with the name "name" left-justified in the current
   tool font "JunoConfig.TextFont". *)
  BEGIN
    RETURN TextVBT.New(name, halign := 0.0, fnt := JunoConfig.textFont)
  END ButtonText;

PROCEDURE ButtonInit(
    self: Button;
    root: View.Root;
    name: TEXT;
    tl: Drawing.ArgTool): Button =
(* This is the implementation of the "Button.init" method. *)
  BEGIN
    self.root := root;
    self.tool := tl;
    tl.label := name;
    EVAL ButtonVBT.T.init(self, ButtonText(name), DoButton);
    RETURN self
  END ButtonInit;

PROCEDURE DoButton(self: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
(* This is the call-back procedure for a "Button". It selects the tool
   associated with the button. If the tool has any arguments, this procedure
   also permanently highlights the button (and unhighlights the button of the
   current tool if one was selected). *)
  <* LL.sup = VBT.mu *>
  VAR tb: Button := self; BEGIN
    TYPECASE self OF
      TemplButton (b) =>
        CurrCmd.ChangeAST(b.root.ccmd, b.cmd);
        b.root.astTrue := TRUE;
        b.root.source.modified(View.ModKind.ImplicitConsistent);
        b.root.ccmd.codeValid := FALSE;
        b.root.dTrue := FALSE;
        IF NOT Source.Make(b.root.source, cd.time, b.root.skipify) THEN
          b.root.astTrue := FALSE;
          EVAL Source.Make(b.root.source, cd.time, b.root.skipify)
        END;
        RETURN
      ELSE (* SKIP *)
    END;
    Drawing.SelectTool(tb.root.drawing, tb.tool, cd.time);
    IF NUMBER(tb.tool.argType^) # 0 THEN
      HiliCurrButton(tb.root, hili := FALSE);
      tb.root.currButton := tb;
      HiliCurrButton(tb.root, hili := TRUE)
    END
  END DoButton;

PROCEDURE SetButtonInit(
    self: SetButton;
    root: View.Root;
    name: TEXT;
    tl: Drawing.SetTool): SetButton =
(* This is the implementation of the "SetButton.init" method. *)
  BEGIN
    self.root := root;
    self.tool := tl;
    tl.label := name;
    EVAL MenuBtnVBT.T.init(self, ButtonText(name), DoSetButton);
    RETURN self
  END SetButtonInit;

PROCEDURE DoSetButton(self: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
(* This is the callback procedure for a "SetButton". It selects the tool
   associated with the button when it was created. *)
  <* LL.sup = VBT.mu *>
  VAR tb: SetButton := self; BEGIN
    Drawing.SelectTool(tb.root.drawing, tb.tool, cd.time)
  END DoSetButton;

PROCEDURE SetAnchorBtnPre(btn: SetAnchorBtn) =
(* This is the implementation of the "pre" method for the "AnchorBtnVBT.T" of
   a set tool. It dynamically updates its menu from the menu stored in the
   button's editor "btn.ed" under the name "btn.nm". *)
  <* LL.sup = VBT.mu *>
  VAR menu := btn.ed.getMenu(btn.nm); BEGIN
    IF btn.menu = NIL THEN
      btn.menu := BorderedVBT.New(menu)
    ELSIF menu # Filter.Child(btn.menu) THEN
      EVAL Filter.Replace(btn.menu, menu) 
    END;
    AnchorBtnVBT.T.pre(btn)
  END SetAnchorBtnPre;

PROCEDURE Update(t: T; ed: Editor.T; rt: View.Root; n:=0; anon:=TRUE) =
  <* LL.sup = VBT.mu *>
  <* FATAL Split.NotAChild *>
  VAR
    ch: ButtonVBT.T := Split.Nth(t, n);
    mod := JunoAST.NilId;
    tr := Editor.Trees(ed);
  BEGIN
    (* The buttons in the toolbox preceeding "ch" correspond to the first "n"
       buttons plus the buttons for the trees preceeding "tr". *)
    WHILE tr # NIL DO
      TYPECASE tr.tree OF
        JunoAST.Module(m) =>
          IF NOT anon THEN mod := m.name END
      | JunoAST.PredDecl (p) =>
          IF NOT p.private THEN
	    VAR hd := p.header; type: Atom.T; BEGIN
              IF ed.getToolType(hd.name, type) THEN
                <* ASSERT type = Editor.PointToolSym *>
                VAR nmMatch: BOOLEAN; BEGIN
                  IF PredMatch(mod, ch, hd, nmMatch)
                    THEN ch := Split.Succ(t, ch)
                    ELSE InsertBtn(t, ch, NewPredBtn(rt, mod, hd), nmMatch)
                  END
                END
              END
	    END
          END
      | JunoAST.FuncDecl (f) =>
          IF NOT f.private THEN
	    VAR hd := f.header; type: Atom.T; BEGIN
              IF ed.getToolType(hd.name, type) THEN
                <* ASSERT type = Editor.PointToolSym *>
                VAR nmMatch: BOOLEAN; BEGIN
                  IF FuncMatch(mod, ch, hd, nmMatch)
                    THEN ch := Split.Succ(t, ch)
                    ELSE InsertBtn(t, ch, NewFuncBtn(rt, mod, hd), nmMatch)
                  END
                END
              END
	    END
          END
      | JunoAST.ProcDecl (p) =>
          IF NOT p.private THEN
	    VAR hd := p.header; type: Atom.T; BEGIN
              IF ed.getToolType(hd.name, type) THEN
                VAR nmMatch: BOOLEAN; BEGIN
                  IF type # Editor.TemplToolSym AND
                     ProcMatch(mod, ch, hd, nmMatch) AND
                     BtnTypeMatches(ch, type) THEN
                    ch := Split.Succ(t, ch)
                  ELSE
                    InsertBtn(t, ch, NewProcBtn(rt, ed, mod, p, type),
                      nmMatch)
                  END
                END
	      END
	    END
          END
      ELSE (* SKIP *)
      END;
      tr := tr.next
    END;
    WHILE ch # NIL DO
      VAR nextCh := Split.Succ(t, ch); BEGIN
        Split.Delete(t, ch);
        ch := nextCh
      END
    END
  END Update;

PROCEDURE PredMatch(mod: JunoAST.Id; btn: ButtonVBT.T; sig: JunoAST.PredHeader;
    VAR (*OUT*) nameMatch: BOOLEAN): BOOLEAN =
(* Returns "TRUE" iff both the "name" and "signature" of "btn" and "sig"
   match. If "FALSE" is returned, then "nameMatch" is set "TRUE" iff the
   "name" of "btn" matches that of "sig". Otherwise, the value of "nameMatch"
   is undefined. *)
  BEGIN
    TYPECASE btn OF
      NULL => (* SKIP *)
    | PointButton (b) =>
      	TYPECASE b.tool OF
          Drawing.PredTool (tool) =>
            nameMatch := (tool.name.id0 = mod AND tool.name.id1 = sig.name);
    	    RETURN nameMatch AND (tool.in_cnt = sig.ins.size)
        | Drawing.ArgTool (tool) =>
            nameMatch := (tool.name.id0 = mod AND tool.name.id1 = sig.name);
            RETURN FALSE
      	END
    ELSE (* SKIP *)
    END;
    nameMatch := FALSE;
    RETURN FALSE
  END PredMatch;

PROCEDURE FuncMatch(mod: JunoAST.Id; btn: ButtonVBT.T; sig: JunoAST.FuncHeader;
    VAR (*OUT*) nameMatch: BOOLEAN): BOOLEAN =
(* Returns "TRUE" iff both the "name" and "signature" of "btn" and "sig"
   match. If "FALSE" is returned, then "nameMatch" is set "TRUE" iff the
   "name" of "btn" matches that of "sig". Otherwise, the value of "nameMatch"
   is undefined. *)
  BEGIN
    TYPECASE btn OF
      NULL => (* SKIP *)
    | PointButton (b) =>
      	TYPECASE b.tool OF
          Drawing.FuncTool (tool) =>
            nameMatch := (tool.name.id0 = mod AND tool.name.id1 = sig.name);
    	    RETURN nameMatch AND (tool.in_cnt = sig.ins.size)
        | Drawing.ArgTool (tool) =>
            nameMatch := (tool.name.id0 = mod AND tool.name.id1 = sig.name);
            RETURN FALSE
      	END
    ELSE (* SKIP *)
    END;
    nameMatch := FALSE;
    RETURN FALSE
  END FuncMatch;

PROCEDURE ProcMatch(mod: JunoAST.Id; btn: ButtonVBT.T; sig: JunoAST.ProcHeader;
    VAR (*OUT*) nameMatch: BOOLEAN): BOOLEAN =
(* Returns "TRUE" iff both the "name" and "signature" of "btn" and "sig"
   match. If "FALSE" is returned, then "nameMatch" is set "TRUE" iff the
   "name" of "btn" matches that of "sig". Otherwise, the value of "nameMatch"
   is undefined.

   In the case that "btn" is a "SetAnchorBtn", which has no signature, the
   signatures of "btn" and "sig" are always considered to match. Otherwise,
   the signatures match if the signature of the tool associated with "btn"
   matches that of "sig". *)
  BEGIN
    TYPECASE btn OF <* NOWARN *>
      NULL => (* SKIP *)
    | SetAnchorBtn (b) => nameMatch := FALSE; RETURN b.nm = sig.name
    | Button (b) =>
        TYPECASE b.tool OF
          Drawing.ProcTool (tool) =>
            nameMatch := (tool.name.id0 = mod AND tool.name.id1 = sig.name);
            RETURN nameMatch AND (tool.in_cnt = sig.ins.size AND
              tool.out_cnt = sig.outs.size AND tool.inout_cnt=sig.inouts.size)
        | Drawing.ArgTool (tool) =>
            nameMatch := (tool.name.id0 = mod AND tool.name.id1 = sig.name);
            RETURN FALSE
        END
    END;
    nameMatch := FALSE;
    RETURN FALSE
  END ProcMatch;

PROCEDURE BtnTypeMatches(btn: ButtonVBT.T; type: Atom.T): BOOLEAN =
(* Return "TRUE" iff the type of "btn" matches the UI declaration type name
   "type", which is one of "Editor.PointToolSym", "Editor.TextToolSym", or
   "Editor.SetToolSym". *)
  BEGIN
    TYPECASE btn OF <* NOWARN *>
      PointButton   => RETURN type = Editor.PointToolSym
    | TemplButton   => RETURN type = Editor.TemplToolSym
    | TextButton    => RETURN type = Editor.TextToolSym
    | SetAnchorBtn  => RETURN type = Editor.SetToolSym
    END
  END BtnTypeMatches;

PROCEDURE InsertBtn(t: T; VAR (*INOUT*) ch: ButtonVBT.T; new: ButtonVBT.T;
    nameMatch: BOOLEAN) =
(* If "nameMatch", then replace "ch" by "new" in "t", and set "ch" to the
   successor child of this new child. In this case, "ch" must be non-NIL.
   Otherwise, insert "new" before "ch" in "t", and leave "ch" unchanged. *)
  <* FATAL Split.NotAChild *>
  BEGIN
    IF nameMatch THEN
      <* ASSERT ch # NIL *>
      VAR curr := ch; BEGIN
        ch := Split.Succ(t, ch);
        Split.Delete(t, curr)
      END
    END;
    Split.Insert(t, Split.Pred(t, ch), new)
  END InsertBtn;

PROCEDURE NewPredBtn(
    rt: View.Root;
    mod: JunoAST.Id;
    hd: JunoAST.PredHeader): Button =
  BEGIN
    RETURN NEW(PointButton).init(rt, Atom.ToText(hd.name),
      Drawing.NewPredTool(JunoASTUtils.QIdFromIds(mod, hd.name), hd.ins.size))
  END NewPredBtn;

PROCEDURE NewFuncBtn(
    rt: View.Root;
    mod: JunoAST.Id;
    hd: JunoAST.FuncHeader): Button =
  BEGIN
    RETURN NEW(PointButton).init(rt, Atom.ToText(hd.name),
      Drawing.NewFuncTool(JunoASTUtils.QIdFromIds(mod, hd.name), hd.ins.size))
  END NewFuncBtn;

PROCEDURE NewProcBtn(
    rt: View.Root;
    ed: Editor.T;
    mod: Atom.T;
    p: JunoAST.ProcDecl;
    type: Atom.T): ButtonVBT.T =
  VAR hd := p.header; txtName := Atom.ToText(hd.name);  BEGIN
    IF type = Editor.SetToolSym THEN
      RETURN NEW(SetAnchorBtn, ed := ed, nm := hd.name).init(
        ButtonText(txtName), menu := NIL, hfudge := 10.0, n := 999)
    ELSE
      VAR res: Button; BEGIN
        IF type = Editor.TextToolSym THEN 
          res := NEW(TextButton)
        ELSIF type = Editor.PointToolSym THEN
          res := NEW(PointButton)
        ELSIF type = Editor.TemplToolSym THEN
          res := NEW(TemplButton, cmd := StripIFProj(p.body))
        ELSE
          <* ASSERT FALSE *>
        END;        
        RETURN res.init(rt, txtName,
          Drawing.NewProcTool(JunoASTUtils.QIdFromIds(mod, hd.name),
            hd.ins.size, hd.outs.size, hd.inouts.size,
            isText := type = Editor.TextToolSym))
      END
    END
  END NewProcBtn;

PROCEDURE StripIFProj(body: JunoAST.Cmd): JunoAST.Cmd =
  BEGIN
    TYPECASE body OF JunoAST.If (if) =>
      IF ISTYPE(if.body, JunoAST.Proj)
        THEN RETURN if.body
        ELSE RETURN body
      END
    ELSE RETURN body
    END
  END StripIFProj;

<* SPEC Unselect REQUIRES sup(LL) = VBT.mu *>

PROCEDURE Unselect(rt: View.Root) =
  BEGIN
    IF rt.drawing # NIL THEN
      Drawing.SelectTool(rt.drawing, NIL, time := 0)
    END;
    HiliCurrButton(rt, hili := FALSE);
    rt.currButton := NIL
  END Unselect;

PROCEDURE HiliCurrButton(rt: View.Root; hili: BOOLEAN) =
(* If the current button associated with "rt" is non-NIL, hilight or unhilight
   it as "hili" is "TRUE" OR "FALSE". *)
  BEGIN
    IF rt.currButton # NIL THEN
      TextVBT.SetFont(Filter.Child(rt.currButton),
        JunoConfig.textFont, toolColor[hili])
    END
  END HiliCurrButton;

PROCEDURE SwapButton(t: T; curr, new: Drawing.ArgTool; newLabel: TEXT) =
  <* LL.sup = VBT.mu *>
  <* FATAL Split.NotAChild *>
  VAR ch: Button := Split.Succ(t, NIL); BEGIN
    WHILE ch # NIL AND ch.tool # curr DO
      ch := Split.Succ(t, ch)
    END;
    <* ASSERT ch # NIL *>
    Split.Replace(t, ch, NEW(Button).init(ch.root, newLabel, new))
  END SwapButton;

BEGIN
END ToolBox.
