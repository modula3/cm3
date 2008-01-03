(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Thu Feb 29 12:35:27 PST 1996 by mhb       *)
(*      modified on Tue Jan 31 11:16:01 PST 1995 by kalsow    *)
(*      modified on Tue Oct  5 14:13:11 PDT 1993 by muller    *)
(*      modified on Wed Jun  9 21:09:21 PDT 1993 by meehan    *)
(*      modified on Wed Oct 28 11:11:50 PST 1992 by steveg    *)
(*      modified on Mon Jan 15 16:48:10 PST 1990 by brooks    *)
(*      modified on Sun May 21 17:17:10 PDT 1989 by gidi      *)
<* PRAGMA LL *>

MODULE FormsVBT EXPORTS FVRuntime, FormsVBT, FVTypes;

(* This module contains the code to construct (parse) FV-expressions. *)

IMPORT FVRuntime;

IMPORT Atom, AtomIntTbl, Axis, BiFeedbackVBT, BooleanVBT,
       ButtonVBT, ChoiceVBT, Color, ColorName, Cursor,
       FeedbackVBT, FileBrowserVBT, Filter, FlexVBT, Fmt,
       HVSplit, Image, Jva, JVSink, ListVBT, Macro,
       MarginFeedbackVBT, MenuSwitchVBT, MultiSplit, MultiFilter,
       NumericVBT, OSError, PaintOp, Pixmap, Pts, Rd, RdUtils,
       ReactivityVBT, RefList, Rsrc, ScaleFilter, ScrollerVBT,
       Shadow, ShadowedFeedbackVBT, ShadowedVBT, SourceVBT,
       Split, SplitterVBT, SwitchVBT, Sx, Text, TextEditVBT,
       TextPort, TextPortClass, TextureVBT, TextVBT, TextWr,
       Thread, TSplit, TypescriptVBT, VBT, ViewportVBT, Wr,
       ZChildVBT, ZSplit;

(*IMPORT StubImageRd AS ImageRd;*)

FROM RefListUtils IMPORT Pop, Push, AssocQ;

<* FATAL Thread.Alerted *>

TYPE
  ParseClosure = Thread.SizedClosure OBJECT
                   description: Sx.T;
                   fv         : T;
                   fixupList  : FixupLink := NIL;
                   state      : State
                 OVERRIDES
                   apply := Apply
                 END;
  FixupLink = REF RECORD
                    targetName: TEXT;
                    sourceVBT : VBT.T;
                    next      : FixupLink
                  END;

VAR                              (* CONST *)
  qBar    := Atom.FromText ("Bar");
  qChisel := Atom.FromText ("Chisel");
  qFill   := Atom.FromText ("Fill");
  qGlue   := Atom.FromText ("Glue");
  qInsert := Atom.FromText ("Insert");
  qMain   := Atom.FromText ("Main");
  qMinus  := Atom.FromText ("-");
  qPlus   := Atom.FromText ("+");
  qReset  := Atom.FromText ("Reset");
  qRidge  := Atom.FromText ("Ridge");

PROCEDURE Parse (t: T; description: Sx.T; READONLY state: State): VBT.T
  RAISES {Error} =
  BEGIN
    TYPECASE
        Thread.Join (Thread.Fork (NEW (ParseClosure, stackSize := 10000,
                                       description := description, fv := t,
                                       state := state))) OF
    | TEXT (msg) => RAISE Error (msg)
    | VBT.T (ch) => RETURN ch
    ELSE <* ASSERT FALSE *>
    END
  END Parse;

PROCEDURE Apply (cl: ParseClosure): REFANY =
  <* LL = 0 *>
  VAR ch: VBT.T;
  BEGIN
    TRY
      ch := Item (cl, cl.description, cl.state);
      Pass2 (cl);
      RETURN ch
    EXCEPT
    | Error (msg) => RETURN msg
    END
  END Apply;

EXCEPTION Narrow;
(* NARROW-faults are checked runtime errors, but implementations
   are not required to map them into exceptions, so you can't
   catch them with TRY EXCEPT ELSE. That would have been handy
   in the following procedure.  (So would multi-methods!) *)

PROCEDURE Pass2 (cl: ParseClosure) RAISES {Error} =
  (* Find targets of (For xxx) and (TabTo xxx) forms. *)
  BEGIN
    WHILE cl.fixupList # NIL DO
      TRY
        WITH target = GetVBT (cl.fv, cl.fixupList.targetName),
             source = cl.fixupList.sourceVBT                   DO
          TYPECASE source OF
          | FVHelper (fbh) =>
              TYPECASE target OF
              | FVFileBrowser (x) => FileBrowserVBT.SetHelper (x, fbh)
              | TextPort.T (target) => fbh.tabNext := target
              | NumericVBT.T (target) => fbh.tabNext := target.typein
              | TextEditVBT.T (target) => fbh.tabNext := target.tp
              ELSE
                RAISE Narrow
              END
          | FVDirMenu (dm) =>
              TYPECASE target OF
              | FVFileBrowser (x) => FileBrowserVBT.SetDirMenu (x, dm)
              ELSE
                RAISE Narrow
              END
          | FVPageButton, FVPageMButton =>
              TYPECASE target OF
              | FVTSplit (x) => FVRuntime.SetPageTarget (source, x)
              ELSE
                RAISE Narrow
              END
          | FVLinkButton, FVLinkMButton =>
              FVRuntime.SetLinkTarget (
                source, FindTChild (target, cl.fixupList.targetName))
          | FVCloseButton (cb) =>
              TYPECASE target OF
              | ZChildVBT.T (x) => cb.target := x
              ELSE
                RAISE Narrow
              END
          | FVPopButton, FVPopMButton =>
              TYPECASE target OF
              | ZChildVBT.T (x) => FVRuntime.SetPopTarget (source, x)
              ELSE
                RAISE Narrow
              END
          | FVTypeIn (source) =>
              TYPECASE target OF
              | TextPort.T (target) => source.tabNext := target
              | NumericVBT.T (target) => source.tabNext := target.typein
              | TextEditVBT.T (target) => source.tabNext := target.tp
              ELSE
                RAISE Narrow
              END
          | FVNumeric (source) =>
              TYPECASE target OF
              | TextPort.T (target) => source.typein.tabNext := target
              | NumericVBT.T (target) =>
                  source.typein.tabNext := target.typein
              | TextEditVBT.T (target) => source.typein.tabNext := target.tp
              ELSE
                RAISE Narrow
              END
          ELSE
            Gripe ("Internal error [Pass2]: ", source)
          END                    (* TYPECASE source *)
        END                      (* WITH *)
      EXCEPT
      | FileBrowserVBT.Error (e) =>
          Gripe (Fmt.F ("Error in FileBrowser %s: %s %s",
                        cl.fixupList.targetName, e.path, e.text))
      | Error (msg) => RAISE Error (msg)
      ELSE                       (* NARROW fault, NIL, etc. *)
        Gripe (Fmt.F ("The form named %s is of the wrong type",
                      cl.fixupList.targetName))
      END;
      cl.fixupList := cl.fixupList.next
    END                          (* WHILE *)
  END Pass2;

PROCEDURE FindTChild (vbt: VBT.T; vbtName: TEXT): VBT.T RAISES {Error} =
  BEGIN
    (* "The named component must be either a TSplit child, or a descendant of
       something that is.  In the latter case the TSplit child is the true
       target." *)
    LOOP
      TYPECASE VBT.Parent (vbt) OF
      | NULL => RAISE Error (vbtName & " is not in a TSplit")
      | FVTSplit => RETURN vbt
      | VBT.Split (parent) => vbt := parent
      END
    END
  END FindTChild;

(*************************** Parser *******************************)

TYPE
  ComponentProc =
    PROCEDURE (cl: ParseClosure; VAR list: RefList.T; READONLY s: State): VBT.T
      RAISES {Error};
  RealizeProc = PROCEDURE (): VBT.T RAISES {Error};
  StateProc = PROCEDURE (list: RefList.T; VAR state: State) RAISES {Error};
  MetricsProc =
    PROCEDURE (sym: Atom.T; arglist: RefList.T; VAR metrics: RefList.T)
      RAISES {Error};

(* The "state" data structure, about 132 bytes, could be made
   much more efficient. Currently, a copy of the data structure is
   made each time that a component is encountered. (Further, a copy from
   the heap is made on each component that has a name, so that the 
   inheritable props can be changed at runtime.) 

   For instance, a copy of the entire data structure isn't needed, 
   just those variables that have actually changed. Other (more) 
   efficient schemes are possible. --mhb 1/24/94 *)


PROCEDURE Item (cl: ParseClosure; exp: Sx.T; READONLY state: State):
  VBT.T RAISES {Error} =
  (*
    This routine interprets an S-expression as a component (VBT).
     - NIL is illegal.
     - The symbol Fill is parsed as (Fill).
     - The symbol Bar is parsed as (Bar 2).
     - The symbol Chisel is parsed as (Chisel 2).
     - The symbol Ridge is parsed as (Ridge 2).
     - The symbol Glue is parsed as (Glue 2).
     - A text "abc" is parsed as (Text "abc").
     - Lists whose first element names a component are parsed by
       specific routines, stored in "componentProcTable".
    Nothing else is legal. *)
  VAR
    list: RefList.T;
    res : VBT.T;
  BEGIN
    Push (cl.fv.formstack, exp); (* For debugging *)
    TYPECASE exp OF
    | NULL =>
    | Atom.T (s) =>
        res := ParseSymbolComponent (cl, s, state);
        cl.fv.formstack := cl.fv.formstack.tail;
        RETURN res
    | TEXT (text) =>
        list := RefList.List1 (text);
        res := pText (cl, list, state);
        cl.fv.formstack := cl.fv.formstack.tail;
        RETURN res
    | RefList.T (list) =>
        TYPECASE list.head OF
        | NULL =>
        | Atom.T (sym) =>
            list := list.tail;
            WITH p = FindComponentProc (sym) DO
              IF p # NIL THEN
                res := p (cl, list, state)
              ELSE
                WITH m = MacroFunction (sym, state) DO
                  IF m # NIL THEN
                    res := Item (cl, m.apply (list), state)
                  ELSIF sym = qInsert THEN
                    res :=
                      OneChild (
                        cl, InsertFile (OneText (list), cl.fv.path, cl.fv.baseURL), state)
                  ELSE
                    Gripe ("Unknown component: ", sym)
                  END
                END
              END
            END;
            cl.fv.formstack := cl.fv.formstack.tail;
            RETURN res
        ELSE
        END
    ELSE
    END;
    Gripe ("Syntax error: ", exp); <* ASSERT FALSE *>
  END Item;

PROCEDURE MacroFunction (sym: Atom.T; READONLY state: State): Macro.T =
  BEGIN
    WITH pair = AssocQ (state.macros, sym) DO
      IF pair # NIL THEN RETURN pair.tail.head ELSE RETURN NIL END
    END
  END MacroFunction;
      
PROCEDURE ParseSymbolComponent (cl: ParseClosure; sym: Atom.T; READONLY state: State):
  VBT.T RAISES {Error} =
  BEGIN
    IF sym = qBar OR sym = qGlue OR sym = qRidge OR sym = qChisel
         OR sym = qFill THEN
      RETURN Item (cl, RefList.List1 (sym), state)
    ELSE
      Gripe ("Unknown Symbol-component: ", sym); <* ASSERT FALSE *>
    END
  END ParseSymbolComponent;

PROCEDURE Gripe (msg: TEXT; form: REFANY := NIL) RAISES {Error} =
  BEGIN
    IF form # NIL THEN msg := msg & ToText (form) END;
    RAISE Error (msg)
  END Gripe;

(* ====================================================================== *)
(* Parsing routines for components *)
(* ====================================================================== *)

VAR Unnamed := Atom.FromText ("");

PROCEDURE NamePP (): SymbolPP =
  BEGIN
    RETURN NEW (SymbolPP, val := Unnamed, valname := "", name := "Name")
  END NamePP;

PROCEDURE Named (n: SymbolPP): BOOLEAN =
  BEGIN
    RETURN n.val # Unnamed
  END Named;

(* ======================= Realizing VBTs ========================== *)

REVEAL
  T <: Private;
  Private = SemiPublic BRANDED OBJECT OVERRIDES realize := Realize END;

PROCEDURE Realize (<* UNUSED *> fv  : Private;
                                type: TEXT;
                   <* UNUSED *> name: TEXT     ): VBT.T RAISES {Error} =
  BEGIN
    RETURN FindRealizeProc (Atom.FromText (type)) ()
  END Realize;


PROCEDURE rAny (): VBT.T =
  BEGIN
    RETURN NEW(FVAny)
  END rAny;

PROCEDURE rAnyFilter (): VBT.T =
  BEGIN
    RETURN NEW(FVAnyFilter)
  END rAnyFilter;

PROCEDURE rAnySplit (): VBT.T =
  BEGIN
    RETURN NEW(FVAnySplit)
  END rAnySplit;

PROCEDURE rAudio (): VBT.T =
  BEGIN
    RETURN NEW(FVAudio)
  END rAudio;

PROCEDURE rBar (): VBT.T =
  BEGIN
    RETURN NEW (FVBar)
  END rBar;

PROCEDURE rBoolean (): VBT.T =
  BEGIN
    RETURN NEW (FVBoolean)
  END rBoolean;

PROCEDURE rBorder (): VBT.T =
  BEGIN
    RETURN NEW (FVBorder)
  END rBorder;

PROCEDURE rBrowser (): VBT.T =
  BEGIN
    RETURN NEW (FVBrowser)
  END rBrowser;

PROCEDURE rButton (): VBT.T =
  BEGIN
    RETURN NEW (FVButton)
  END rButton;

PROCEDURE rChisel (): VBT.T =
  BEGIN
    RETURN NEW (FVChisel)
  END rChisel;

PROCEDURE rChoice (): VBT.T =
  BEGIN
    RETURN NEW (FVChoice)
  END rChoice;

PROCEDURE rCloseButton (): VBT.T =
  BEGIN
    RETURN NEW (FVCloseButton)
  END rCloseButton;

PROCEDURE rDirMenu (): VBT.T =
  BEGIN
    RETURN NEW (FVDirMenu)
  END rDirMenu;

PROCEDURE rFileBrowser (): VBT.T =
  BEGIN
    RETURN NEW (FVFileBrowser)
  END rFileBrowser;

PROCEDURE rFill (): VBT.T =
  BEGIN
    RETURN NEW (FVFill)
  END rFill;

PROCEDURE rFilter (): VBT.T =
  BEGIN
    RETURN NEW (FVFilter)
  END rFilter;

PROCEDURE rFrame (): VBT.T =
  BEGIN
    RETURN NEW (FVFrame)
  END rFrame;

PROCEDURE rGeneric (): VBT.T =
  BEGIN
    RETURN NEW (FVGeneric)
  END rGeneric;

PROCEDURE rGlue (): VBT.T =
  BEGIN
    RETURN NEW (FVGlue)
  END rGlue;

PROCEDURE rGuard (): VBT.T =
  BEGIN
    RETURN NEW (FVGuard)
  END rGuard;

PROCEDURE rHBox (): VBT.T =
  BEGIN
    RETURN NEW (FVHBox)
  END rHBox;

PROCEDURE rHPackSplit (): VBT.T =
  BEGIN
    RETURN NEW (FVHPackSplit)
  END rHPackSplit;

PROCEDURE rHTile (): VBT.T =
  BEGIN
    RETURN NEW (FVHTile)
  END rHTile;

PROCEDURE rHelp (): VBT.T =
  BEGIN
    RETURN NEW (FVHelp)
  END rHelp;

PROCEDURE rHelper (): VBT.T =
  BEGIN
    RETURN NEW (FVHelper)
  END rHelper;

PROCEDURE rImage (): VBT.T =
  BEGIN
    RETURN NEW (FVImage)
  END rImage;

PROCEDURE rIntApply (): VBT.T =
  BEGIN
    RETURN NEW(FVIntApply)
  END rIntApply;

PROCEDURE rLinkButton (): VBT.T =
  BEGIN
    RETURN NEW (FVLinkButton)
  END rLinkButton;

PROCEDURE rLinkMButton (): VBT.T =
  BEGIN
    RETURN NEW (FVLinkMButton)
  END rLinkMButton;

PROCEDURE rMButton (): VBT.T =
  BEGIN
    RETURN NEW (FVMButton)
  END rMButton;

PROCEDURE rMenu (): VBT.T =
  BEGIN
    RETURN NEW (FVMenu)
  END rMenu;

PROCEDURE rMultiBrowser (): VBT.T =
  BEGIN
    RETURN NEW (FVMultiBrowser)
  END rMultiBrowser;

PROCEDURE rNumeric (): VBT.T =
  BEGIN
    RETURN NEW (FVNumeric)
  END rNumeric;

PROCEDURE rPageButton (): VBT.T =
  BEGIN
    RETURN NEW (FVPageButton)
  END rPageButton;

PROCEDURE rPageMButton (): VBT.T =
  BEGIN
    RETURN NEW (FVPageMButton)
  END rPageMButton;

PROCEDURE rPixmap (): VBT.T =
  BEGIN
    RETURN NEW (FVPixmap)
  END rPixmap;

PROCEDURE rPopButton (): VBT.T =
  BEGIN
    RETURN NEW (FVPopButton)
  END rPopButton;

PROCEDURE rPopMButton (): VBT.T =
  BEGIN
    RETURN NEW (FVPopMButton)
  END rPopMButton;

PROCEDURE rRadio (): VBT.T =
  BEGIN
    RETURN NEW (FVRadio)
  END rRadio;

PROCEDURE rRidge (): VBT.T =
  BEGIN
    RETURN NEW (FVRidge)
  END rRidge;

PROCEDURE rRim (): VBT.T =
  BEGIN
    RETURN NEW (FVRim)
  END rRim;

PROCEDURE rScale (): VBT.T =
  BEGIN
    RETURN NEW (FVScale)
  END rScale;

PROCEDURE rScroller (): VBT.T =
  BEGIN
    RETURN NEW (FVScroller)
  END rScroller;

PROCEDURE rShape (): VBT.T =
  BEGIN
    RETURN NEW (FVShape)
  END rShape;

PROCEDURE rSource (): VBT.T =
  BEGIN
    RETURN NEW (FVSource)
  END rSource;

PROCEDURE rStable (): VBT.T =
  BEGIN
    RETURN NEW (FVStable)
  END rStable;

PROCEDURE rTSplit (): VBT.T =
  BEGIN
    RETURN NEW (FVTSplit)
  END rTSplit;

PROCEDURE rTarget (): VBT.T =
  BEGIN
    RETURN NEW (FVTarget)
  END rTarget;

PROCEDURE rText (): VBT.T =
  BEGIN
    RETURN NEW (FVText)
  END rText;

PROCEDURE rTextEdit (): VBT.T =
  BEGIN
    RETURN NEW (FVTextEdit)
  END rTextEdit;

PROCEDURE rTexture (): VBT.T =
  BEGIN
    RETURN NEW (FVTexture)
  END rTexture;

PROCEDURE rTrillButton (): VBT.T =
  BEGIN
    RETURN NEW (FVTrillButton)
  END rTrillButton;

PROCEDURE rTypeIn (): VBT.T =
  BEGIN
    RETURN NEW (FVTypeIn)
  END rTypeIn;

PROCEDURE rTypescript (): VBT.T =
  BEGIN
    RETURN NEW (FVTypescript)
  END rTypescript;

PROCEDURE rVBox (): VBT.T =
  BEGIN
    RETURN NEW (FVVBox)
  END rVBox;

PROCEDURE rVTile (): VBT.T =
  BEGIN
    RETURN NEW (FVVTile)
  END rVTile;

PROCEDURE rVideo (): VBT.T =
  BEGIN
    RETURN NEW(FVVideo)
  END rVideo;

PROCEDURE rViewport (): VBT.T =
  BEGIN
    RETURN NEW (FVViewport)
  END rViewport;

PROCEDURE rVPackSplit (): VBT.T =
  BEGIN
    RETURN NEW (FVHPackSplit)
  END rVPackSplit;

PROCEDURE rZBackground (): VBT.T =
  BEGIN
    RETURN NEW (FVZBackground)
  END rZBackground;

PROCEDURE rZChassis (): VBT.T =
  BEGIN
    RETURN NEW (FVZChassis)
  END rZChassis;

PROCEDURE rZChild (): VBT.T =
  BEGIN
    RETURN NEW (FVZChild)
  END rZChild;

PROCEDURE rZGrow (): VBT.T =
  BEGIN
    RETURN NEW (FVZGrow)
  END rZGrow;

PROCEDURE rZMove (): VBT.T =
  BEGIN
    RETURN NEW (FVZMove)
  END rZMove;

PROCEDURE rZSplit (): VBT.T =
  BEGIN
    RETURN NEW (FVZSplit)
  END rZSplit;


(* ========================= Any, AnyFilter, AnySplit ============================= *)

PROCEDURE pAny (         cl  : ParseClosure;
                VAR      list: RefList.T;
                READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    name         := NamePP();
    state        := s;
    res  : FVAny;
  BEGIN
    ParseProps(cl, list, state, PP1{name});
    res := cl.fv.realize("Any", name.valname);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pAny;

PROCEDURE pAnyFilter (
    cl:ParseClosure; VAR list:RefList.T; READONLY s:State): VBT.T
  RAISES {Error} =
  VAR
    name  := NamePP();
    state := s;
  VAR
    res: FVAnyFilter;
  BEGIN
    ParseProps(cl, list, state, PP1{name});
    res := cl.fv.realize("AnyFilter", name.valname);
    res := res.init(OneChild(cl, list, state));
    AddNameProp(cl, res, name, state);
    RETURN res
  END pAnyFilter;

PROCEDURE pAnySplit (
    cl:ParseClosure; VAR list:RefList.T; READONLY s:State): VBT.T
  RAISES {Error} =
  VAR
    name  := NamePP();
    state := s;
  VAR res: FVAnySplit;
  BEGIN
    ParseProps(cl, list, state, PP1{name});
    res := cl.fv.realize("AnySplit", name.valname);
    AddChildren(cl, res, list, state);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pAnySplit;


(* ========================= Bar & Glue ============================= *)

(* Bar uses the current Color.  Glue uses the current BgColor. *)

CONST
  FlexOne = FlexVBT.SizeRange{
              1.0 * Pts.MMPerInch / Pts.PtsPerInch, 0.0, 0.0};
(*
  FlexOnePointFive = FlexVBT.SizeRange{
              1.5 * Pts.MMPerInch / Pts.PtsPerInch, 0.0, 0.0};
*)

PROCEDURE pBar (         cl  : ParseClosure;
                VAR      list: RefList.T;
                READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    name  := NamePP();
    main  := NEW(SizeRangePP, val := FlexOne, found := TRUE);
    state := s;
    res: FVBar;
  BEGIN
    IF state.hvsplit = NIL THEN
      RAISE Error("Bar must appear inside an HBox or VBox.")
    END;
    ParseProps(cl, list, state, PP2{name, main}, main := main);
    res := cl.fv.realize("Bar", name.valname);
    res := res.init(TextureVBT.New(state.fgOp),
                    ShapefromSpec(main.val, state));
    AddNameProp(cl, res, name, state);
    RETURN res
  END pBar;

PROCEDURE pGlue (         cl  : ParseClosure;
                 VAR      list: RefList.T;
                 READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    name  := NamePP();
    main  := NEW(SizeRangePP, val := FlexOne, found := TRUE);
    state := s;
    res: FVGlue;
  BEGIN
    IF state.hvsplit = NIL THEN
      RAISE Error("Glue must appear inside an HBox or VBox.")
    END;
    ParseProps(cl, list, state, PP2{name, main}, main := main);
    res := cl.fv.realize("Glue", name.valname);
    res := res.init(TextureVBT.New(state.bgOp),
                    ShapefromSpec(main.val, state));
    AddNameProp(cl, res, name, state);
    RETURN res
  END pGlue;

PROCEDURE ShapefromSpec (         f    : FlexVBT.SizeRange;
                         READONLY state: State             ): FlexVBT.Shape =
  VAR sh := FlexVBT.Default;
  BEGIN
    sh [state.glueAxis] := f;
    RETURN sh
  END ShapefromSpec;

(* ========================= Border & Rim ============================= *)

PROCEDURE pBorder (         cl  : ParseClosure;
                   VAR      list: RefList.T;
                   READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state   := s;
    name    := NamePP();
    pen     := NEW(RealPP, name := "Pen", val := 1.0);
    texture := NEW(TextPP, name := "Pattern");
    txt     := Pixmap.Solid;
  VAR
    res: FVBorder;
    ch : VBT.T;
  BEGIN
    ParseProps(cl, list, state, PP3{name, pen, texture});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("Border", name.valname);
    IF texture.val # NIL THEN
      txt := GetPixmap(texture.val, cl.fv.path, cl.fv.baseURL)
    END;
    res :=
      res.init(ch, Pts.ToMM(pen.val), state.shadow.bgFg, txt);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pBorder;

PROCEDURE pRim (cl: ParseClosure; VAR list: RefList.T; READONLY s: State): VBT.T
  RAISES {Error} =
  VAR
    state   := s;
    name    := NamePP ();
    pen     := NEW (RealPP, name := "Pen", val := 1.0);
    texture := NEW (TextPP, name := "Pattern");
    txt     := Pixmap.Solid;
  VAR
    res: FVRim;
    ch : VBT.T;
  BEGIN
    ParseProps (cl, list, state, PP3 {name, pen, texture});
    ch := OneChild (cl, list, state);
    res := cl.fv.realize ("Rim", name.valname);
    IF texture.val # NIL THEN txt := GetPixmap (texture.val, cl.fv.path, cl.fv.baseURL) END;
    res := res.init (ch, Pts.ToMM (pen.val), state.shadow.fgBg, txt);
    AddNameProp (cl, res, name, state);
    RETURN res
  END pRim;

PROCEDURE GetPixmap (name: TEXT; path: Rsrc.Path; baseURL: TEXT): Pixmap.T
  RAISES {Error} =
  BEGIN
    WITH image = GetRawImage(name, path, baseURL) DO
      RETURN Image.Scaled(image)
    END
  END GetPixmap;

PROCEDURE GetRawImage (name: TEXT; path: Rsrc.Path; baseURL: TEXT):
  Image.Raw RAISES {Error} =
  VAR rd: Rd.T;
  BEGIN
    TRY
      rd := Open(name, path, baseURL);
      TRY RETURN Image.FromRd(rd) FINALLY Rd.Close(rd) END;
    EXCEPT
    | Image.Error => RAISE Error("Format error in pixmap for " & name)
    | Rd.Failure (ref) => RAISE Error(RdUtils.FailureText(ref))
    END
  END GetRawImage;


(* ========================= Frame & Ridge ============================= *)

PROCEDURE pFrame (         cl  : ParseClosure;
                  VAR      list: RefList.T;
                  READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state       := s;
    name        := NamePP();
    shadowStyle := NewShadowStyle(Shadow.Style.Raised);
  VAR
    res: FVFrame;
    ch : VBT.T;
  BEGIN
    ParseProps(
      cl, list, state, PP1{name}, enums := EP1{shadowStyle});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("Frame", name.valname);
    res := res.init(ch, state.shadow,
                    VAL(shadowStyle.chosen, Shadow.Style));
    AddNameProp(cl, res, name, state);
    RETURN res
  END pFrame;

PROCEDURE pRidge (         cl  : ParseClosure;
                  VAR      list: RefList.T;
                  READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state := s;
    name  := NamePP();
    main := NEW(
              RealPP, val := DefaultShadowSizePts, found := TRUE);
  VAR
    res   : FVRidge;
    shadow: Shadow.T;
  BEGIN
    ParseProps(cl, list, state, PP2{name, main}, main := main);
    res := cl.fv.realize("Ridge", name.valname);
    shadow :=
      Shadow.New(Pts.ToMM(main.val), state.bgOp, state.fgOp,
                 state.lightOp, state.darkOp);
    res := res.init(state.glueAxis, shadow, Shadow.Style.Ridged);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pRidge;

PROCEDURE pChisel (         cl  : ParseClosure;
                   VAR      list: RefList.T;
                   READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state := s;
    name  := NamePP();
    main := NEW(
              RealPP, val := DefaultShadowSizePts, found := TRUE);
  VAR
    res   : FVChisel;
    shadow: Shadow.T;
  BEGIN
    ParseProps(cl, list, state, PP2{name, main}, main := main);
    res := cl.fv.realize("Chisel", name.valname);
    shadow :=
      Shadow.New(Pts.ToMM(main.val), state.bgOp, state.fgOp,
                 state.lightOp, state.darkOp);
    res :=
      res.init(state.glueAxis, shadow, Shadow.Style.Chiseled);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pChisel;

(* =========================== Fill & Shape =========================== *)

PROCEDURE pFill (         cl  : ParseClosure;
                 VAR      list: RefList.T;
                 READONLY s   : State         ): VBT.T
  RAISES {Error} =
  CONST
    INFINITESTRETCH = FlexVBT.SizeRange{
                        natural := 0.0, shrink := 0.0, stretch :=
                        FlexVBT.Infinity};
  VAR
    state         := s;
    name          := NamePP();
    shape         := FlexVBT.Default;
    res  : FVFill;
  BEGIN
    IF state.hvsplit = NIL THEN
      RAISE Error("Fill must appear inside an HBox or VBox.")
    END;
    ParseProps(cl, list, state, PP1{name});
    AssertEmpty(list);
    shape[state.glueAxis] := INFINITESTRETCH;
    res := cl.fv.realize("Fill", name.valname);
    res := res.init(TextureVBT.New(state.bgOp), shape);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pFill;

PROCEDURE pShape (         cl  : ParseClosure;
                  VAR      list: RefList.T;
                  READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state           := s;
    name            := NamePP();
    height          := NEW(SizeRangePP, name := "Height");
    width           := NEW(SizeRangePP, name := "Width");
    res   : FVShape;
    ch    : VBT.T;
  BEGIN
    ParseProps(cl, list, state, PP3{name, height, width});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("Shape", name.valname);
    res := res.init(ch, FlexVBT.Shape{width.val, height.val});
    AddNameProp(cl, res, name, state);
    RETURN res
  END pShape;


(* =========================== Buttons =============================== *)

PROCEDURE pButton (         cl  : ParseClosure;
                   VAR      list: RefList.T;
                   READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state           := s;
    name            := NamePP();
    res  : FVButton;
    ch   : VBT.T;
  BEGIN
    ParseProps(cl, list, state, PP1{name});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("Button", name.valname);
    res := res.init(
             NEW(ShadowedFeedbackVBT.T).init(ch, state.shadow));
    AddNameProp(cl, res, name, state);
    RETURN res
  END pButton;

PROCEDURE pMButton (         cl  : ParseClosure;
                    VAR      list: RefList.T;
                    READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state            := s;
    name             := NamePP();
    res  : FVMButton;
    ch   : VBT.T;
  BEGIN
    ParseProps(cl, list, state, PP1{name});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("MButton", name.valname);
    res :=
      res.init(ShadowedFeedbackVBT.NewMenu(ch, state.shadow));
    AddNameProp(cl, res, name, state);
    RETURN res
  END pMButton;

PROCEDURE pPopButton (         cl  : ParseClosure;
                      VAR      list: RefList.T;
                      READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state                := s;
    name                 := NamePP();
    forName              := NEW(SymbolPP, name := "For");
    res    : FVPopButton;
    ch     : VBT.T;
  BEGIN
    ParseProps(cl, list, state, PP2{name, forName});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("PopButton", name.valname);
    res := res.init(
             NEW(ShadowedFeedbackVBT.T).init(ch, state.shadow));
    AddForProp(cl, res, forName);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pPopButton;

PROCEDURE pPopMButton (         cl  : ParseClosure;
                       VAR      list: RefList.T;
                       READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state                 := s;
    name                  := NamePP();
    forName               := NEW(SymbolPP, name := "For");
    res    : FVPopMButton;
    ch     : VBT.T;
  BEGIN
    ParseProps(cl, list, state, PP2{name, forName});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("PopMButton", name.valname);
    res :=
      res.init(ShadowedFeedbackVBT.NewMenu(ch, state.shadow));
    AddForProp(cl, res, forName);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pPopMButton;

PROCEDURE pGuard (         cl  : ParseClosure;
                  VAR      list: RefList.T;
                  READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state          := s;
    name           := NamePP();
    res  : FVGuard;
    ch   : VBT.T;
  BEGIN
    ParseProps(cl, list, state, PP1{name});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("Guard", name.valname);
    res := res.init(ch, state.shadow);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pGuard;

PROCEDURE pTrillButton (         cl  : ParseClosure;
                        VAR      list: RefList.T;
                        READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state                := s;
    name                 := NamePP();
    res  : FVTrillButton;
    ch   : VBT.T;
  BEGIN
    ParseProps(cl, list, state, PP1{name});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("TrillButton", name.valname);
    res := res.init(
             NEW(ShadowedFeedbackVBT.T).init(ch, state.shadow));
    AddNameProp(cl, res, name, state);
    RETURN res
  END pTrillButton;

PROCEDURE pPageButton (         cl  : ParseClosure;
                       VAR      list: RefList.T;
                       READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state                   := s;
    name                    := NamePP();
    forName                 := NEW(SymbolPP, name := "For");
    backwards               := NEW(BooleanPP, name := "Back");
    res      : FVPageButton;
    ch       : VBT.T;
  BEGIN
    ParseProps(
      cl, list, state, PP2{name, forName}, KP1{backwards});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("PageButton", name.valname);
    IF forName.val # NIL THEN
      AddForProp(cl, res, forName)
    ELSIF state.tsplit = NIL THEN
      RAISE
        Error("This PageButton is not included in a TSplit and "
                & "it has no (For ...) property.")
    END;
    res :=
      res.init(ch, state.shadow, backwards.val, state.tsplit);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pPageButton;

PROCEDURE pPageMButton (         cl  : ParseClosure;
                        VAR      list: RefList.T;
                        READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state                    := s;
    name                     := NamePP();
    forName                  := NEW(SymbolPP, name := "For");
    backwards                := NEW(BooleanPP, name := "Back");
    res      : FVPageMButton;
    ch       : VBT.T;
  BEGIN
    ParseProps(
      cl, list, state, PP2{name, forName}, KP1{backwards});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("PageMButton", name.valname);
    IF forName.val # NIL THEN
      AddForProp(cl, res, forName)
    ELSIF state.tsplit = NIL THEN
      RAISE
        Error("This PageMButton is not included in a TSplit and "
                & "it has no (For ...) property.")
    END;
    res :=
      res.init(ch, state.shadow, backwards.val, state.tsplit);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pPageMButton;

PROCEDURE pLinkButton (         cl  : ParseClosure;
                       VAR      list: RefList.T;
                       READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state                := s;
    name                 := NamePP();
    toName               := NEW(SymbolPP, name := "For");
    res   : FVLinkButton;
    ch    : VBT.T;
  BEGIN
    ParseProps(cl, list, state, PP2{name, toName});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("LinkButton", name.valname);
    IF toName.val = NIL THEN
      RAISE Error("LinkButton must include (To <name>)")
    END;
    AddForProp(cl, res, toName);
    res := res.init(
             NEW(ShadowedFeedbackVBT.T).init(ch, state.shadow));
    AddNameProp(cl, res, name, state);
    RETURN res
  END pLinkButton;

PROCEDURE pLinkMButton (         cl  : ParseClosure;
                        VAR      list: RefList.T;
                        READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state                 := s;
    name                  := NamePP();
    toName                := NEW(SymbolPP, name := "For");
    res   : FVLinkMButton;
    ch    : VBT.T;
  BEGIN
    ParseProps(cl, list, state, PP2{name, toName});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("LinkMButton", name.valname);
    IF toName.val = NIL THEN
      RAISE Error("LinkMButton must include (To <name>)")
    END;
    AddForProp(cl, res, toName);
    res :=
      res.init(ShadowedFeedbackVBT.NewMenu(ch, state.shadow));
    AddNameProp(cl, res, name, state);
    RETURN res
  END pLinkMButton;

PROCEDURE pCloseButton (         cl  : ParseClosure;
                        VAR      list: RefList.T;
                        READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state                  := s;
    name                   := NamePP();
    forName                := NEW(SymbolPP, name := "For");
    res    : FVCloseButton;
    ch     : VBT.T;
  BEGIN
    ParseProps(cl, list, state, PP2{name, forName});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("CloseButton", name.valname);
    IF forName.val # NIL THEN
      AddForProp(cl, res, forName)
    ELSIF state.zchild # NIL THEN
      res.target := state.zchild
    ELSE
      RAISE
        Error(
          "This CloseButton is not included in a ZChild or ZChassis "
            & "and it has no (For ...) property.")
    END;
    res := res.init(ch, state.shadow);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pCloseButton;


(* ====================== Boolean, Choice, Radio ======================= *)

PROCEDURE pBoolean (         cl  : ParseClosure;
                    VAR      list: RefList.T;
                    READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state     := s;
    name      := NamePP();
    value     := NEW(BooleanPP, name := "Value");
    checkbox  := NEW(BooleanPP, name := "CheckBox");
    checkmark := NEW(BooleanPP, name := "CheckMark");
    inverting := NEW(BooleanPP, name := "Inverting");
    menustyle := NEW(BooleanPP, name := "MenuStyle");
    enum := NEW(EnumPP).init(
              KP3{checkbox, checkmark, inverting}, 0);
    child, feedback: VBT.T;
    switch         : ButtonVBT.T;
    res            : FVBoolean;
  BEGIN
    ParseProps(cl, list, state, PP2{name, value}, KP1{menustyle},
               enums := EP1{enum});
    child := OneChild(cl, list, state);
    IF inverting.val THEN
      feedback :=
        NEW(ShadowedFeedbackVBT.T).init(child, state.shadow)
    ELSIF checkmark.val THEN
      feedback := MarginFeedbackVBT.NewCheck(child, state.shadow)
    ELSE
      feedback := MarginFeedbackVBT.NewBox(child, state.shadow)
    END;
    IF menustyle.val THEN
      switch := NEW(MenuSwitchVBT.T).init(
                  MenuStyle(feedback, state.shadow))
    ELSE
      switch := NEW(SwitchVBT.T).init(feedback)
    END;
    res := cl.fv.realize("Boolean", name.valname);
    res := res.init(switch);
    BooleanVBT.Put(res, value.val);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pBoolean;

PROCEDURE pChoice (         cl  : ParseClosure;
                   VAR      list: RefList.T;
                   READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state     := s;
    name      := NamePP();
    value     := NEW(BooleanPP, name := "Value");
    checkmark := NEW(BooleanPP, name := "CheckMark");
    checkbox  := NEW(BooleanPP, name := "CheckBox");
    inverting := NEW(BooleanPP, name := "Inverting");
    enum := NEW(EnumPP).init(
              KP3{checkbox, checkmark, inverting}, 0);
    menustyle := NEW(BooleanPP, name := "MenuStyle");
    child, feedback: VBT.T;
    switch         : ButtonVBT.T;
    res            : FVChoice;
  BEGIN
    IF state.radio = NIL THEN
      RAISE Error("Choice must be contained within Radio")
    END;
    ParseProps(cl, list, state, PP2{name, value}, KP1{menustyle},
               enums := EP1{enum});
    IF name.val = NIL THEN
      RAISE Error("Choices must be named.")
    END;
    child := OneChild(cl, list, state);
    IF inverting.val THEN
      feedback :=
        NEW(ShadowedFeedbackVBT.T).init(child, state.shadow)
    ELSIF checkmark.val THEN
      feedback := MarginFeedbackVBT.NewCheck(child, state.shadow)
    ELSE
      feedback :=
        MarginFeedbackVBT.NewBullet(child, state.shadow)
    END;
    IF menustyle.val THEN
      switch := NEW(MenuSwitchVBT.T).init(
                  MenuStyle(feedback, state.shadow))
    ELSE
      switch := NEW(SwitchVBT.T).init(feedback)
    END;
    res := cl.fv.realize("Choice", name.valname);
    res := res.init(switch, state.radio.radio);
    res.radio := state.radio;
    res.name := name.valname;
    IF value.val THEN ChoiceVBT.Put(res) END;
    AddNameProp(cl, res, name, state);
    RETURN res
  END pChoice;

PROCEDURE pRadio (         cl  : ParseClosure;
                  VAR      list: RefList.T;
                  READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state          := s;
    name           := NamePP();
    value          := NEW(SymbolPP, name := "Value");
    res  : FVRadio;
  BEGIN
    ParseProps(cl, list, state, PP2{name, value});
    res := cl.fv.realize("Radio", name.valname);
    res.radio := NEW(ChoiceVBT.Group);
    state.radio := res;
    EVAL Filter.T.init(res, OneChild(cl, list, state));
    (* Did the client select a choice via (Radio ...  =<symbol>
       ...)? *)
    IF value.val # NIL THEN
      ChoiceVBT.Put(GetVBT(cl.fv, value.valname))
    END;
    AddNameProp(cl, res, name, state);
    RETURN res
  END pRadio;

PROCEDURE MenuStyle (feedback: FeedbackVBT.T; shadow: Shadow.T):
  FeedbackVBT.T =
  BEGIN
    WITH ch = MultiFilter.Replace(feedback, NIL),
         sh = ShadowedFeedbackVBT.NewMenu(NIL, shadow) DO
      RETURN NEW(BiFeedbackVBT.T).init(sh, feedback, ch)
    END
  END MenuStyle;


(* =========================== Splits =============================== *)

PROCEDURE pHBox (cl: ParseClosure; VAR list : RefList.T; READONLY s: State):
    VBT.T
  RAISES {Error} =
  BEGIN
    RETURN pHVBox(cl, list, s, Axis.T.Hor)
  END pHBox;

PROCEDURE pVBox (cl: ParseClosure; VAR list : RefList.T; READONLY s: State):
    VBT.T
  RAISES {Error} =
  BEGIN
    RETURN pHVBox(cl, list, s, Axis.T.Ver)
  END pVBox;

PROCEDURE pHVBox (         cl  : ParseClosure;
                  VAR      list: RefList.T;
                  READONLY s   : State;
                           axis: Axis.T        ): VBT.T
  RAISES {Error} =
  CONST TypeNames = ARRAY Axis.T OF TEXT{"HBox", "VBox"};
  VAR
    state            := s;
    name             := NamePP();
    res  : HVSplit.T;
  BEGIN
    ParseProps(cl, list, state, PP1{name});
    res := cl.fv.realize(TypeNames[axis], name.valname);
    res := res.init(axis, adjustable := FALSE);
    state.glueAxis := axis;
    state.hvsplit := res;
    AddChildren(cl, res, list, state);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pHVBox;

PROCEDURE pHTile (cl: ParseClosure; VAR list: RefList.T; READONLY s: State):
    VBT.T
  RAISES {Error} =
  BEGIN
    RETURN pHVTile (cl, list, s, Axis.T.Hor)
  END pHTile;

PROCEDURE pVTile (cl: ParseClosure; VAR list: RefList.T; READONLY s: State):
    VBT.T
  RAISES {Error} =
  BEGIN
    RETURN pHVTile (cl, list, s, Axis.T.Ver)
  END pVTile;

PROCEDURE pHVTile (         cl  : ParseClosure;
                   VAR      list: RefList.T;
                   READONLY s   : State;
                            axis: Axis.T        ): VBT.T
  RAISES {Error} =
  CONST TypeNames = ARRAY Axis.T OF TEXT{"HTile", "VTile"};
  VAR
    state := s;
    name  := NamePP();
    (* asTargets := NEW (BooleanPP, name := "Targets"); *)
    res: SplitterVBT.T;
  BEGIN
    ParseProps(
      cl, list, state, PP1{name} (* , KP1 {asTargets} *));
    res := cl.fv.realize(TypeNames[axis], name.valname);
    res := res.init(axis, op := state.shadow.bgFg);
    state.glueAxis := axis;
    AddChildren(cl, res, list, state);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pHVTile;

PROCEDURE pHPackSplit (cl: ParseClosure; 
    VAR list: RefList.T; READONLY state: State): VBT.T RAISES {Error} =
  BEGIN
    RETURN pHVPackSplit (cl, list, state, Axis.T.Hor)
  END pHPackSplit;

PROCEDURE pVPackSplit (cl: ParseClosure; 
    VAR list: RefList.T; READONLY state: State): VBT.T RAISES {Error} =
  BEGIN
    RETURN pHVPackSplit (cl, list, state, Axis.T.Ver)
  END pVPackSplit;

PROCEDURE pHVPackSplit (         cl  : ParseClosure;
                        VAR      list: RefList.T;
                        READONLY s   : State;
                                 axis: Axis.T        ): VBT.T
  RAISES {Error} =
  CONST
    TypeNames = ARRAY Axis.T OF TEXT{"HPackSplit", "VPackSplit"};
  VAR
    state      := s;
    name       := NamePP();
    hgap       := NEW(RealPP, name := "HGap", val := 2.0);
    vgap       := NEW(RealPP, name := "VGap", val := 2.0);
    background := NEW(TextPP, name := "Background");
    txt        := Pixmap.Solid;
  VAR res: FVHPackSplit;
  BEGIN
    ParseProps(
      cl, list, state, PP4{name, hgap, vgap, background});
    res := cl.fv.realize(TypeNames[axis], name.valname);
    IF background.val # NIL THEN
      txt := GetPixmap(background.val, cl.fv.path, cl.fv.baseURL)
    END;
    res := res.init(hv := axis, hgap := Pts.ToMM(hgap.val),
                    vgap := Pts.ToMM(vgap.val), txt := txt,
                    op := state.bgOp);
    AddChildren(cl, res, list, state);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pHVPackSplit;


(* ========================== TSplits ============================ *)

PROCEDURE pTSplit (         cl  : ParseClosure;
                   VAR      list: RefList.T;
                   READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state := s;
    name  := NamePP();
    value := NEW(CardinalPP, name := "Value",
                 val := LAST(CARDINAL));
    which              := NEW(SymbolPP, name := "Which");
    circular           := NEW(BooleanPP, name := "Circular");
    flexible           := NEW(BooleanPP, name := "Flex");
    res     : FVTSplit;
    n       : CARDINAL;
    namedChild, numberedChild: VBT.T := NIL;
  BEGIN
    ParseProps(cl, list, state, PP3{name, value, which},
               KP2{circular, flexible});
    res := cl.fv.realize("TSplit", name.valname);
    res := res.init(fickle := flexible.val);
    res.circular := circular.val;
    state.tsplit := res;
    AddChildren(cl, res, list, state);

    (* Check validity and consistency of (Which n) and (Value
       name). *)
    n := Split.NumChildren(res);

    IF which.val # NIL THEN
      namedChild := GetVBT(cl.fv, which.valname)
    END;

    TRY
      IF value.val = LAST(CARDINAL) THEN
        IF namedChild # NIL THEN
          TSplit.SetCurrent(res, namedChild)
        ELSE
          TSplit.SetCurrent(res, Split.Nth(res, 0))
        END
      ELSIF value.val < n THEN
        numberedChild := Split.Nth(res, value.val);
        IF namedChild = NIL OR namedChild = numberedChild THEN
          TSplit.SetCurrent(res, numberedChild)
        ELSE
          RAISE
            Error(
              Fmt.F(
                "(Which %s) is not the same child as (Value %s)",
                Atom.ToText(which.val), Fmt.Int(value.val)))
        END
      ELSIF value.val = 1 THEN
        RAISE Error("TSplit has no children.")
      ELSE
        RAISE
          Error(
            Fmt.F("TSplit has only %s children.", Fmt.Int(n)))
      END
    EXCEPT
      Split.NotAChild =>
        RAISE
          Error(
            Atom.ToText(which.val)
              & " is not the name of a child of this TSplit.")
    END;
    AddNameProp(cl, res, name, state);
    RETURN res
  END pTSplit;


(* ===================== FileBrowser & Helper ==================== *)

PROCEDURE pFileBrowser (         cl  : ParseClosure;
                        VAR      list: RefList.T;
                        READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state    := s;
    name     := NamePP();
    value    := NEW(TextPP, name := "Value", val := ".");
    suffixes := NEW(TextListPP, name := "Suffixes");
    readOnly := NEW(BooleanPP, name := "ReadOnly");
    res: FVFileBrowser;
  BEGIN
    ParseProps(
      cl, list, state, PP3{name, value, suffixes}, KP1{readOnly});
    AssertEmpty(list);
    res := cl.fv.realize("FileBrowser", name.valname);
    res := res.init(state.font, state.shadow);
    TRY
      IF value.found THEN FileBrowserVBT.Set(res, value.val) END;
      FileBrowserVBT.SetReadOnly(res, readOnly.val);
      IF suffixes.val # NIL THEN
        FileBrowserVBT.SetSuffixes(
          res, SuffixesFromList(suffixes.val))
      END
    EXCEPT
    | FileBrowserVBT.Error (e) =>
        RAISE Error(Fmt.F("Error for %s: %s", e.path, e.text))
    END;
    AddNameProp(cl, res, name, state);
    RETURN res
  END pFileBrowser;

PROCEDURE SuffixesFromList (list: RefList.T): TEXT =
  VAR wr := TextWr.New ();
  <* FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    LOOP
      IF Text.Empty (list.head) THEN
        Wr.PutChar (wr, '$')
      ELSE
        Wr.PutText (wr, list.head)
      END;
      list := list.tail;
      IF list = NIL THEN RETURN TextWr.ToText (wr) END;
      Wr.PutChar (wr, ' ')
    END
  END SuffixesFromList;
      
PROCEDURE pHelper (         cl  : ParseClosure;
                   VAR      list: RefList.T;
                   READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state          := s;
    name           := NamePP();
    forName        := NEW(SymbolPP, name := "For");
    firstFocus     := NEW(BooleanPP, name := "FirstFocus");
    expandOnDemand := NEW(BooleanPP, name := "ExpandOnDemand");
    tabTo          := NEW(SymbolPP, name := "TabTo");
  VAR res: FVHelper;
  BEGIN
    ParseProps(cl, list, state, PP3{name, forName, tabTo},
               KP2{firstFocus, expandOnDemand});
    IF forName.val = NIL THEN
      RAISE Error("Helper must include (For <name>)")
    END;
    AssertEmpty(list);
    res := cl.fv.realize("Helper", name.valname);
    res := res.init(expandOnDemand.val, font := state.font,
                    colorScheme := state.shadow);
    AddForProp(cl, res, forName);
    IF tabTo.val # NIL THEN AddForProp(cl, res, tabTo) END;
    CheckFirstFocus(firstFocus, res);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pHelper;

PROCEDURE CheckFirstFocus (firstFocus: BooleanPP; widget: VBT.T) =
  BEGIN
    IF firstFocus.val THEN FVRuntime.SetFirstFocus(widget) END
  END CheckFirstFocus;

PROCEDURE pDirMenu (         cl  : ParseClosure;
                    VAR      list: RefList.T;
                    READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state              := s;
    name               := NamePP();
    forName            := NEW(SymbolPP, name := "For");
    res    : FVDirMenu;
  BEGIN
    ParseProps(cl, list, state, PP2{name, forName});
    IF forName.val = NIL THEN
      RAISE Error("DirMenu must include (For <name>)")
    END;
    AssertEmpty(list);
    res := cl.fv.realize("DirMenu", name.valname);
    res := res.init(font := state.font, shadow := state.shadow);
    AddForProp(cl, res, forName);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pDirMenu;

PROCEDURE pBrowser (         cl  : ParseClosure;
                    VAR      list: RefList.T;
                    READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state  := s;
    name   := NamePP();
    value  := NEW(IntegerPP, name := "Value", val := -1);
    select := NEW(TextPP, name := "Select");
    items  := NEW(TextListPP, name := "Items");
    from   := NEW(TextPP, name := "From");
    quick  := NEW(BooleanPP, name := "Quick");
    colors: Shadow.T;
    res   : FVBrowser;
    u     : UniSelector;
  BEGIN
    ParseProps(cl, list, state,
               PP5{name, value, select, items, from}, KP1{quick});
    AssertEmpty(list);
    res := cl.fv.realize("Browser", name.valname);
    colors := state.shadow;
    TYPECASE res.painter OF
    | NULL =>
        res.painter := NEW(ListVBT.TextPainter).init(
                         colors.bg, colors.fg, colors.fg,
                         colors.bg, state.font)
    | ListVBT.TextPainter (tp) =>
        res.painter := tp.init(colors.bg, colors.fg, colors.fg,
                               colors.bg, state.font)
    ELSE
    END;
    TYPECASE res.selector OF
    | NULL => u := NEW(UniSelector).init(res); res.selector := u
    | UniSelector (sel) => u := sel.init(res)
    ELSE
      RAISE
        Error(
          "Browser has a selector that is not a subtype of FVTypes.UniSelector")
    END;
    u.browser := res;
    u.quick := quick.val;
    res := res.init(colors := state.shadow);
    IF items.val # NIL THEN
      SetValues(res, items.val)
    ELSIF from.val # NIL THEN
      SetValues(res, ItemsFromFile(from.val, cl))
    END;
    IF value.val # -1 THEN
      res.selectOnly(value.val)
    ELSIF select.val # NIL THEN
      res.selectOnly(ListVBTPosition(res, select.val))
    END;
    AddNameProp(cl, res, name, state);
    RETURN res
  END pBrowser;

PROCEDURE pMultiBrowser (         cl  : ParseClosure;
                         VAR      list: RefList.T;
                         READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state  := s;
    name   := NamePP();
    value  := NEW(CardinalListPP, name := "Value");
    select := NEW(TextListPP, name := "Select");
    items  := NEW(TextListPP, name := "Items");
    from   := NEW(TextPP, name := "From");
    quick  := NEW(BooleanPP, name := "Quick");
    res   : FVMultiBrowser;
    m     : MultiSelector;
    colors: Shadow.T;
  BEGIN
    ParseProps(cl, list, state,
               PP5{name, value, select, items, from}, KP1{quick});
    AssertEmpty(list);
    res := cl.fv.realize("MultiBrowser", name.valname);
    colors := state.shadow;
    TYPECASE res.painter OF
    | NULL =>
        res.painter := NEW(ListVBT.TextPainter).init(
                         colors.bg, colors.fg, colors.fg,
                         colors.bg, state.font)
    | ListVBT.TextPainter (tp) =>
        res.painter := tp.init(colors.bg, colors.fg, colors.fg,
                               colors.bg, state.font)
    ELSE
    END;
    TYPECASE res.selector OF
    | NULL =>
        m := NEW(MultiSelector).init(res);
        res.selector := m
    | MultiSelector (sel) => m := sel.init(res)
    ELSE
      RAISE
        Error(
          "MultiBrowser has a selector that is not a subtype "
            & "of FVTypes.MultiSelector")
    END;
    m.quick := quick.val;
    m.browser := res;
    res := res.init(colors := state.shadow);
    IF items.val # NIL THEN
      SetValues(res, items.val)
    ELSIF from.val # NIL THEN
      SetValues(res, ItemsFromFile(from.val, cl))
    END;
    IF value.val # NIL THEN
      REPEAT
        res.select(NARROW(Pop(value.val), REF INTEGER)^, TRUE)
      UNTIL value.val = NIL
    ELSIF select.val # NIL THEN
      REPEAT
        res.select(ListVBTPosition(res, Pop(select.val)), TRUE)
      UNTIL select.val = NIL
    END;
    AddNameProp(cl, res, name, state);
    RETURN res
  END pMultiBrowser;

PROCEDURE SetValues (v: ListVBT.T; new: RefList.T) =
  VAR
    oldCount := v.count ();
    newCount := RefList.Length (new);
    delta    := oldCount - newCount;
  BEGIN
    IF delta < 0 THEN
      v.insertCells (oldCount, -delta)
    ELSIF delta > 0 THEN
      v.removeCells (newCount, delta)
    END;
    FOR j := 0 TO newCount - 1 DO v.setValue (j, Pop (new)) END
  END SetValues;
  
PROCEDURE ListVBTPosition (v: ListVBT.T; item: TEXT):
  [-1 .. LAST (CARDINAL)] =
  BEGIN
    FOR i := v.count () - 1 TO 0 BY -1 DO
      IF Text.Equal (v.getValue (i), item) THEN RETURN i END
    END;
    RETURN -1
  END ListVBTPosition;
  
PROCEDURE ItemsFromFile (name: TEXT; cl: ParseClosure): RefList.T
  RAISES {Error} =
  VAR tl: RefList.T := NIL;
  BEGIN
    TRY                          (* EXCEPT *)
      WITH in = Open(name, cl.fv.path, cl.fv.baseURL) DO
        TRY                      (* FINALLY *)
          TRY                    (* EXCEPT *)
            LOOP Push(tl, Rd.GetLine(in)) END
          EXCEPT
          | Rd.EndOfFile => RETURN RefList.ReverseD(tl)
          END                    (* TRY *)
        FINALLY
          Rd.Close(in)
        END                      (* TRY *)
      END                        (* WITH *)
    EXCEPT
    | Rd.Failure (ref) => RAISE Error(RdUtils.FailureText(ref))
    END                          (* TRY *)
  END ItemsFromFile;


(* =========================== Insert =========================== *)

PROCEDURE InsertFile (pathname: TEXT; path: Rsrc.Path; baseURL: TEXT):
  RefList.T RAISES {Error} =
  VAR
    res: RefList.T := NIL;
    rd : Rd.T;
  BEGIN
    TRY
      rd := Open(pathname, path, baseURL);
      TRY
        LOOP Push(res, Sx.Read(rd, syntax := FVSyntax)) END
      FINALLY
        Rd.Close(rd)
      END
    EXCEPT
    | Sx.ReadError (txt) => RAISE Error("Sx.ReadError: " & txt)
    | Rd.EndOfFile => RETURN RefList.ReverseD(res)
    | Rd.Failure (ref) => RAISE Error(RdUtils.FailureText(ref))
    END
  END InsertFile;

(* =========================== Menus =============================== *)

PROCEDURE pMenu (         cl  : ParseClosure;
                 VAR      list: RefList.T;
                 READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state           := s;
    name            := NamePP();
    local           := NEW(BooleanPP, name := "NotInTrestle");
    res  : FVMenu;
    count: CARDINAL;
  BEGIN
    ParseProps(cl, list, state, PP1{name}, KP1{local});
    WITH feedback = NEW(ShadowedFeedbackVBT.T).init(
                      NIL, state.shadow),
         menuFrame = NEW(ShadowedVBT.T).init(
                       NIL, state.shadow, Shadow.Style.Raised) DO
      res := cl.fv.realize("Menu", name.valname);
      IF local.val THEN
        count := 0
      ELSE
        count := LAST(CARDINAL)
      END;
      res := res.init(feedback, menuFrame, count, state.menubar);
      AddChildren(cl, res, list, state);
      AddNameProp(cl, res, name, state);
      RETURN res
    END
  END pMenu;


(* =========================== Help =============================== *)

PROCEDURE pHelp (cl: ParseClosure; VAR list: RefList.T; READONLY s: State):
  VBT.T RAISES {Error} =
  VAR
    state           := s;
    name            := NamePP();
    local           := NEW(BooleanPP, name := "NotInTrestle");
    res  : FVHelp;
    count: CARDINAL;
  BEGIN
    ParseProps(cl, list, state, PP1{name}, KP1{local});
    res := cl.fv.realize("Help", name.valname);
    IF local.val THEN count := 0 ELSE count := LAST(CARDINAL) END;
    res := res.init(NIL, NIL, count);
    AddChildren(cl, res, list, state);
    AddNameProp(cl, res, name, state);
    RETURN res;
  END pHelp;


(* =========================== Numeric =============================== *)

PROCEDURE pNumeric (         cl  : ParseClosure;
                    VAR      list: RefList.T;
                    READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state       := s;
    name        := NamePP();
    allowEmpty  := NEW(BooleanPP, name := "AllowEmpty");
    hideButtons := NEW(BooleanPP, name := "HideButtons");
    value       := NEW(IntegerPP, name := "Value");
    min := NEW(IntegerPP, name := "Min", val := FIRST(INTEGER));
    max := NEW(IntegerPP, name := "Max", val := LAST(INTEGER));
    forName    := NEW(SymbolPP, name := "TabTo");
    firstFocus := NEW(BooleanPP, name := "FirstFocus");
    res: FVNumeric;
  BEGIN
    ParseProps(
      cl, list, state, PP5{min, max, value, name, forName},
      KP3{allowEmpty, hideButtons, firstFocus});
    AssertEmpty(list);
    IF max.val < min.val THEN
      RAISE Error(Fmt.F("Numeric max (%s) is less than min (%s)",
                        Fmt.Int(max.val), Fmt.Int(min.val)))
    ELSIF NOT value.found THEN
      value.val := MIN(MAX(0, min.val), max.val)
    ELSIF min.val <= value.val AND value.val <= max.val THEN (* skip *)
    ELSE
      RAISE
        Error(
          Fmt.F(
            "Initial Numeric value (%s) is not between %s and %s",
            Fmt.Int(value.val), Fmt.Int(min.val),
            Fmt.Int(max.val)))
    END;
    res := cl.fv.realize("Numeric", name.valname);
    res := res.init(min.val, max.val, allowEmpty.val,
                    hideButtons.val, state.font, state.shadow);
    NumericVBT.Put(res, value.val);
    IF forName.val # NIL THEN AddForProp(cl, res, forName) END;
    CheckFirstFocus(firstFocus, res);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pNumeric;


(* ======================= Texture =========================== *)

PROCEDURE pTexture (         cl  : ParseClosure;
                    VAR      list: RefList.T;
                    READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state      := s;
    name       := NamePP();
    main       := NEW(TextPP, found := TRUE);
    localalign := NEW(BooleanPP, name := "LocalAlign");
    res: FVTexture;
    txt            := Pixmap.Solid;
  BEGIN
    ParseProps(
      cl, list, state, PP2{name, main}, KP1{localalign}, main);
    res := cl.fv.realize("Texture", name.valname);
    IF main.val # NIL THEN
      txt := GetPixmap(main.val, cl.fv.path, cl.fv.baseURL)
    END;
    res := res.init(state.shadow.bgFg, txt, localalign.val);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pTexture;


(* ======================= Pixmap & Image =========================== *)

PROCEDURE pImage (<*UNUSED*>          cl  : ParseClosure;
                  <*UNUSED*> VAR      list: RefList.T;
                  <*UNUSED*> READONLY s   : State         ): VBT.T
  RAISES {Error} =
(*******************
  VAR
    state               := s;
    name                := NamePP();
    main                := NEW(TextPP);
    accurate            := NEW(BooleanPP, name := "Accurate");
    gamma               := NEW(BooleanPP, name := "NeedsGamma");
    res     : FVImage;
    len: INTEGER; 
*****************)
  BEGIN
    RAISE Error ("Image not currently supported.");
(*************
    ParseProps(cl, list, state, PP2{name, main},
               KP2{accurate, gamma}, main := main);
    res := cl.fv.realize("Image", name.valname);
    res.bg := state.shadow.bg;
    res.op := state.shadow.bgFg;
    IF gamma.val THEN res.gamma := 2.4
    ELSE res.gamma := 1.0 END;
    res.rd := Open(main.val, cl.fv.path, cl.fv.baseURL);
    TRY len := Rd.Length(res.rd) EXCEPT
    | Thread.Alerted => <* ASSERT FALSE *>
    | Rd.Failure (ref) => RAISE Error(RdUtils.FailureText(ref))
    END;
    WITH pm = NEW(ImageRd.T).init(res.rd, 0, len, res.op, NIL, res.gamma) DO
      res := res.init(pm, res.bg)
    END;
    AddNameProp(cl, res, name, state);
    RETURN res
***************)
  END pImage;

PROCEDURE pPixmap (         cl  : ParseClosure;
                   VAR      list: RefList.T;
                   READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state               := s;
    name                := NamePP();
    main                := NEW(TextPP);
    accurate            := NEW(BooleanPP, name := "Accurate");
    gamma               := NEW(BooleanPP, name := "NeedsGamma");
    res     : FVPixmap;
    image   : Image.Raw;
    op      : PaintOp.T;
  BEGIN
    ParseProps(cl, list, state, PP2{name, main},
               KP2{accurate, gamma}, main := main);
    res := cl.fv.realize("Pixmap", name.valname);
    image := GetRawImage(main.val, cl.fv.path, cl.fv.baseURL);
    TYPECASE image OF
    | Image.RawBitmap => op := state.shadow.bgFg
    | Image.RawPixmap (im) =>
        op := PaintOp.Copy;
        im.needsGamma := gamma.val;
        IF accurate.val THEN
          im.colorMode := Image.Mode.Accurate
        ELSE
          im.colorMode := Image.Mode.Normal
        END
    ELSE                         <* ASSERT FALSE *>
    END;
    res := res.init(Image.Scaled(image), op:=op, bg:=state.shadow.bg);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pPixmap;


(* =========================== Scroller =============================== *)

PROCEDURE pScroller (         cl  : ParseClosure;
                     VAR      list: RefList.T;
                     READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state := s;
    name  := NamePP();
    value := NEW(IntegerPP, name := "Value", val := 50);
    min   := NEW(IntegerPP, name := "Min", val := 0);
    max   := NEW(IntegerPP, name := "Max", val := 100);
    v     := NEW(BooleanPP, name := "Vertical");
    thumb := NEW(CardinalPP, name := "Thumb", val := 0);
    step  := NEW(CardinalPP, name := "Step", val := 1);
    axis  := Axis.T.Hor;
    res: FVScroller;
  BEGIN
    ParseProps(cl, list, state,
               PP6{name, value, min, max, thumb, step}, KP1{v});
    AssertEmpty(list);
    IF v.val THEN axis := Axis.T.Ver END;
    thumb.val := MIN(thumb.val, max.val - min.val);
    res := cl.fv.realize("Scroller", name.valname);
    res := res.init(axis, min.val, max.val, state.shadow,
                    step.val, thumb.val);
    ScrollerVBT.Put(res, value.val);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pScroller;


(* ======================== Source & Target ======================= *)

PROCEDURE pSource (         cl  : ParseClosure;
                   VAR      list: RefList.T;
                   READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state           := s;
    name            := NamePP();
    res  : FVSource;
    ch   : VBT.T;
  BEGIN
    ParseProps(cl, list, state, PP1{name});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("Source", name.valname);
    res := res.init(
             NEW(ShadowedFeedbackVBT.T).init(ch, state.shadow));
    AddNameProp(cl, res, name, state);
    RETURN res
  END pSource;

PROCEDURE pTarget (         cl  : ParseClosure;
                   VAR      list: RefList.T;
                   READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state           := s;
    name            := NamePP();
    res  : FVTarget;
    ch   : VBT.T;
  BEGIN
    ParseProps(cl, list, state, PP1{name});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("Target", name.valname);
    res := res.init(ch);
    SourceVBT.BeTarget(res, SourceVBT.NewTarget());
    AddNameProp(cl, res, name, state);
    RETURN res
  END pTarget;

(* ==================== Stable ===================== *)

PROCEDURE pStable (         cl  : ParseClosure;
                   VAR      list: RefList.T;
                   READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state           := s;
    name            := NamePP();
    res  : FVStable;
    ch   : VBT.T;
  BEGIN
    ParseProps(cl, list, state, PP1{name});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("Stable", name.valname);
    res := res.init(ch);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pStable;

(* ==================== Filter, Generic, Viewport ===================== *)

PROCEDURE pFilter (         cl  : ParseClosure;
                   VAR      list: RefList.T;
                   READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state   := s;
    name    := NamePP();
    active  := NEW(BooleanPP, name := "Active");
    passive := NEW(BooleanPP, name := "Passive");
    dormant := NEW(BooleanPP, name := "Dormant");
    vanish  := NEW(BooleanPP, name := "Vanish");
    enum := NEW(EnumPP).init(
              KP4{active, passive, dormant, vanish}, 0);
    cursor           := NEW(TextPP, name := "Cursor", val := "");
    curs  : Cursor.T;
    res   : FVFilter;
    ch    : VBT.T;
  BEGIN
    ParseProps(
      cl, list, state, PP2{name, cursor}, enums := EP1{enum});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("Filter", name.valname);
    res := res.init(ch, state.shadow);
    IF Text.Empty(cursor.val) THEN
      curs := Cursor.DontCare
    ELSE
      curs := Cursor.FromName(ARRAY OF TEXT{cursor.val})
    END;
    ReactivityVBT.Set(
      res, VAL(enum.chosen, ReactivityVBT.State), curs);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pFilter;

PROCEDURE pScale (         cl  : ParseClosure;
                  VAR      list: RefList.T;
                  READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state     := s;
    name      := NamePP();
    hscale    := NEW(RealPP, name := "HScale", val := 1.0);
    vscale    := NEW(RealPP, name := "VScale", val := 1.0);
    auto      := NEW(BooleanPP, name := "Auto");
    autoFixed := NEW(BooleanPP, name := "AutoFixed");
  VAR
    res: FVScale;
    ch : VBT.T;
  BEGIN
    ParseProps(cl, list, state, PP3{name, hscale, vscale},
               KP2{auto, autoFixed});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("Scale", name.valname);
    res := res.init(ch);
    IF auto.val THEN
      ScaleFilter.AutoScale(res, keepAspectRatio := FALSE)
    ELSIF autoFixed.val THEN
      ScaleFilter.AutoScale(res, keepAspectRatio := TRUE)
    ELSE
      IF hscale.val < 1.0E-6 THEN
        RAISE Error("HScale is too small")
      END;
      IF vscale.val < 1.0E-6 THEN
        RAISE Error("VScale is too small")
      END;
      ScaleFilter.Scale(res, hscale.val, vscale.val)
    END;
    AddNameProp(cl, res, name, state);
    RETURN res
  END pScale;

PROCEDURE pGeneric (         cl  : ParseClosure;
                    VAR      list: RefList.T;
                    READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state            := s;
    name             := NamePP();
    res  : FVGeneric;
  BEGIN
    ParseProps(cl, list, state, PP1{name});
    AssertEmpty(list);
    res := cl.fv.realize("Generic", name.valname);
    res := res.init(NEW(TextureVBT.T).init(txt := Pixmap.Gray),
                    FVRuntime.EMPTYSHAPE);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pGeneric;

PROCEDURE pViewport (         cl  : ParseClosure;
                     VAR      list: RefList.T;
                     READONLY s   : State         ): VBT.T RAISES {Error} =
  VAR
    state     := s;
    name      := NamePP();
    step      := NEW(CardinalPP, name := "Step", val := 10);
    horandver := NEW(BooleanPP, name := "HorAndVer");
    horonly   := NEW(BooleanPP, name := "HorOnly");
    veronly   := NEW(BooleanPP, name := "VerOnly");
    enum      := NEW(EnumPP).init(KP3{horandver, horonly, veronly}, 0);
    res       : FVViewport;
    ch        : VBT.T;
    axis      : Axis.T;
    shapeStyle: ViewportVBT.ShapeStyle;
  BEGIN
    ParseProps(cl, list, state, PP2{name, step}, enums := EP1{enum});
    ch := OneChild(cl, list, state);
    IF horandver.val OR horonly.val THEN
      axis := Axis.T.Ver
    ELSE
      axis := Axis.T.Hor
    END;
    IF horandver.val THEN
      shapeStyle := ViewportVBT.ShapeStyle.Unrelated
    ELSE
      shapeStyle := ViewportVBT.ShapeStyle.Related
    END;
    res := cl.fv.realize("Viewport", name.valname);
    res :=
      res.init(ch := ch, axis := axis, shadow := state.shadow,
               step := step.val, shapeStyle := shapeStyle,
               scrollStyle := VAL(enum.chosen, ViewportVBT.ScrollStyle));
    AddNameProp(cl, res, name, state);
    RETURN res
  END pViewport;


(* ============================= Text ================================= *)

PROCEDURE pText (         cl  : ParseClosure;
                 VAR      list: RefList.T;
                 READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state       := s;
    name        := NamePP();
    main        := NEW(TextPP, found := TRUE);
    margin      := NEW(RealPP, name := "Margin", val := 2.0);
    vmargin     := NEW(RealPP, name := "VMargin", val := 0.0);
    leftalign   := NEW(BooleanPP, name := "LeftAlign");
    centeralign := NEW(BooleanPP, name := "Center");
    rightalign  := NEW(BooleanPP, name := "RightAlign");
    enum := NEW(EnumPP).init(
              KP3{leftalign, centeralign, rightalign}, 1);
    from         := NEW(TextPP, name := "From");
    res : FVText;
  BEGIN
    ParseProps(cl, list, state, PP5{name, main, margin, vmargin, from},
               main := main, enums := EP1{enum});
    IF main.val # NIL THEN       (* skip *)
    ELSIF from.val # NIL THEN
      main.val := TextFromFile(from.val, cl)
    ELSE
      RAISE Error("Main property is missing")
    END;
    res := cl.fv.realize("Text", name.valname);
    res := res.init(main.val, bgFg := state.shadow,
                    fnt := state.labelFont,
                    halign := FLOAT(enum.chosen) * 0.5,
                    vmargin := Pts.ToMM(vmargin.val),
                    hmargin := Pts.ToMM(margin.val));
    AddNameProp(cl, res, name, state);
    RETURN res
  END pText;


(* ========================== Text editors ========================= *)

PROCEDURE pTypeIn (         cl  : ParseClosure;
                   VAR      list: RefList.T;
                   READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state          := s;
    name           := NamePP();
    value          := NEW(TextPP, name := "Value", val := "");
    readOnly       := NEW(BooleanPP, name := "ReadOnly");
    expandOnDemand := NEW(BooleanPP, name := "ExpandOnDemand");
    forName        := NEW(SymbolPP, name := "TabTo");
    turnMargin := NEW(RealPP, name := "TurnMargin", val := 2.0);
    firstFocus := NEW(BooleanPP, name := "FirstFocus");
    from       := NEW(TextPP, name := "From");
  VAR res: FVTypeIn;
  BEGIN
    ParseProps(cl, list, state,
               PP5{name, value, forName, turnMargin, from},
               KP3{readOnly, expandOnDemand, firstFocus});
    AssertEmpty(list);
    res := cl.fv.realize("TypeIn", name.valname);
    res := res.init(expandOnDemand.val, font := state.font,
                    colorScheme := state.shadow,
                    readOnly := readOnly.val,
                    turnMargin := Pts.ToMM(turnMargin.val));
    IF value.found OR from.val = NIL THEN
      TextPort.SetText(res, value.val)
    ELSE
      TextPort.SetText(res, TextFromFile(from.val, cl))
    END;
    VBT.SetCursor(res, Cursor.TextPointer);
    IF forName.val # NIL THEN AddForProp(cl, res, forName) END;
    CheckFirstFocus(firstFocus, res);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pTypeIn;

PROCEDURE pTextEdit (         cl  : ParseClosure;
                     VAR      list: RefList.T;
                     READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state       := s;
    name        := NamePP();
    value       := NEW(TextPP, name := "Value", val := "");
    readOnly    := NEW(BooleanPP, name := "ReadOnly");
    clip        := NEW(BooleanPP, name := "Clip");
    turnMargin  := NEW(RealPP, name := "TurnMargin", val := 2.0);
    from        := NEW(TextPP, name := "From");
    noScrollbar := NEW(BooleanPP, name := "NoScrollbar");
    firstFocus  := NEW(BooleanPP, name := "FirstFocus");
    reportKeys  := NEW(BooleanPP, name := "ReportKeys");
  VAR res: FVTextEdit;
  BEGIN
    ParseProps(
      cl, list, state, PP4{name, value, from, turnMargin},
      KP5{readOnly, clip, noScrollbar, firstFocus, reportKeys});
    AssertEmpty(list);
    res := cl.fv.realize("TextEdit", name.valname);
    IF res.tp = NIL THEN res.tp := NEW(Port) END;
    res.tp :=
      NARROW(res.tp, Port).init(
        textedit := res, reportKeys := TRUE (* reportKeys.val *),
        font := state.font, colorScheme := state.shadow,
        readOnly := readOnly.val, wrap := NOT clip.val,
        turnMargin := Pts.ToMM(turnMargin.val));
    IF value.found OR from.val = NIL THEN
      TextPort.SetText(res.tp, value.val)
    ELSE
      TextPort.SetText(res.tp, TextFromFile(from.val, cl))
    END;
    IF res.sb # NIL THEN
      res.sb := res.sb.init(Axis.T.Ver, state.shadow)
    ELSIF NOT noScrollbar.val THEN
      res.sb :=
        NEW(TextEditVBT.Scrollbar).init(Axis.T.Ver, state.shadow)
    END;
    res := res.init(NOT noScrollbar.val);
    VBT.SetCursor(res, Cursor.TextPointer);
    CheckFirstFocus(firstFocus, res);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pTextEdit;

PROCEDURE pTypescript (         cl  : ParseClosure;
                       VAR      list: RefList.T;
                       READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state      := s;
    name       := NamePP();
    readOnly   := NEW(BooleanPP, name := "ReadOnly");
    clip       := NEW(BooleanPP, name := "Clip");
    turnMargin := NEW(RealPP, name := "TurnMargin", val := 2.0);
    firstFocus := NEW(BooleanPP, name := "FirstFocus");
  VAR res: FVTypescript;
  BEGIN
    ParseProps(cl, list, state, PP2{name, turnMargin},
               KP2{readOnly, clip});
    AssertEmpty(list);
    res := cl.fv.realize("Typescript", name.valname);
    TYPECASE res.tp OF
    | NULL => res.tp := NEW(TypescriptVBT.Port)
    | TypescriptVBT.Port =>
    ELSE
      RAISE Error("The .tp field of the Typescript must be "
                    & "a subtype of TypescriptVBT.Port")
    END;
    res.tp := res.tp.init(
                font := state.font, colorScheme := state.shadow,
                readOnly := readOnly.val, wrap := NOT clip.val,
                turnMargin := Pts.ToMM(turnMargin.val));
    IF res.sb = NIL THEN
      res.sb := NEW(TextEditVBT.Scrollbar)
    END;
    res.sb := res.sb.init(Axis.T.Ver, state.shadow);
    res := res.init();
    VBT.SetCursor(res, Cursor.TextPointer);
    CheckFirstFocus(firstFocus, res);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pTypescript;

PROCEDURE TextFromFile (filename: TEXT; cl: ParseClosure): TEXT
  RAISES {Error} =
  BEGIN
    TRY
      IF cl.fv.baseURL = NIL THEN
        RETURN Rsrc.Get(filename, cl.fv.path);
      ELSE
        WITH rd = Open(filename, cl.fv.path, cl.fv.baseURL) DO
          TRY
            RETURN Rd.GetText(rd, LAST(CARDINAL))
          FINALLY
            Rd.Close(rd)
          END
        END
      END
    EXCEPT
    | Rd.Failure (ref) => RAISE Error(RdUtils.FailureText(ref))
    | Thread.Alerted => RAISE Error("interrupted (Thread.Alerted)")
    | Rsrc.NotFound => RAISE Error("No such resource: " & filename)
    END
  END TextFromFile;

PROCEDURE NewShadowStyle (default := Shadow.Style.Flat): EnumPP =
  VAR
    flat     := NEW (BooleanPP, name := "Flat");
    raised   := NEW (BooleanPP, name := "Raised");
    lowered  := NEW (BooleanPP, name := "Lowered");
    ridged   := NEW (BooleanPP, name := "Ridged");
    chiseled := NEW (BooleanPP, name := "Chiseled");
  BEGIN
    RETURN NEW (EnumPP).init (
             KP5 {flat, raised, lowered, ridged, chiseled}, ORD (default))
  END NewShadowStyle;

(* ======================== ZSplits & ZChildren ===================== *)

PROCEDURE pZSplit (         cl  : ParseClosure;
                   VAR      list: RefList.T;
                   READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state           := s;
    name            := NamePP();
    res  : FVZSplit;
  BEGIN
    ParseProps(cl, list, state, PP1{name});
    res := cl.fv.realize("ZSplit", name.valname);
    res := res.init();
    state.zsplit := res;
    AddChildren(cl, res, list, state);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pZSplit;

PROCEDURE pZBackground (         cl  : ParseClosure;
                        VAR      list: RefList.T;
                        READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state                := s;
    name                 := NamePP();
    res  : FVZBackground;
    ch   : VBT.T;
  BEGIN
    (* it's OK, because we may be inserting the form dynamically
       IF state.zsplit = NIL THEN RAISE Error ("ZBackground must
       be inside a ZSplit.") END; *)
    ParseProps(cl, list, state, PP1{name});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("ZBackground", name.valname);
    res := res.init(ch);
    AddNameProp(cl, res, name, state);
    RETURN res
  END pZBackground;

PROCEDURE pZChassis (         cl  : ParseClosure;
                     VAR      list: RefList.T;
                     READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state   := s;
    name    := NamePP();
    open    := NEW(BooleanPP, name := "Open");
    noClose := NEW(BooleanPP, name := "NoClose");
    title   := NEW(VBTPP, name := "Title");
    at      := NEW(AtSpecPP, name := "At");
    chain   := NEW(ChainsPP, name := "Chain");
    scaled  := NEW(BooleanPP, name := "Scaled");
    fixedH  := NEW(BooleanPP, name := "FixedH");
    fixedV  := NEW(BooleanPP, name := "FixedV");
    fixedHV := NEW(BooleanPP, name := "FixedHV");
  VAR
    res           : FVZChassis;
    titleChild, ch: VBT.T;
    shaper        : ZSplit.ReshapeControl;
  BEGIN
    (* it's OK, because we may be inserting the form dynamically
       IF state.zsplit = NIL THEN RAISE Error ("ZChassis must be
       inside a ZSplit.") END; *)
    ParseProps(
      cl, list, state, PP4{name, title, at, chain},
      KP6{open, noClose, scaled, fixedH, fixedV, fixedHV});
    IF title.val = NIL THEN
      titleChild :=
        TextVBT.New("<Untitled>", fnt := state.labelFont,
                    bgFg := state.shadow)
    ELSE
      titleChild := OneChild(cl, title.val, state)
    END;
    res := cl.fv.realize("ZChassis", name.valname);
    state.zchild := res;
    ch := OneChild(cl, list, state);
    IF chain.shaper # NIL THEN
      shaper := chain.shaper
    ELSIF scaled.val THEN
      shaper := ZChildVBT.Scaled
    ELSIF fixedH.val THEN
      shaper := ZChildVBT.ScaledHFixed
    ELSIF fixedV.val THEN
      shaper := ZChildVBT.ScaledVFixed
    ELSIF fixedHV.val THEN
      shaper := ZChildVBT.ScaledHVFixed
    ELSE
      shaper := NIL
    END;
    IF at.val.edges THEN
      IF shaper = NIL THEN shaper := ZChildVBT.Scaled END;
      res := res.initFromEdges(
               ch, titleChild, at.val.w, at.val.e, at.val.n,
               at.val.s, state.shadow, NOT noClose.val, open.val,
               at.val.type, shaper)
    ELSE
      IF shaper = NIL THEN shaper := ZChildVBT.ScaledHVFixed END;
      res := res.init(ch, titleChild, state.shadow,
                      NOT noClose.val, open.val, at.val.h,
                      at.val.v, at.val.loc, at.val.type, shaper)
    END;
    AddNameProp(cl, res, name, state);
    RETURN res
  END pZChassis;

PROCEDURE pZChild (         cl  : ParseClosure;
                   VAR      list: RefList.T;
                   READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state   := s;
    name    := NamePP();
    open    := NEW(BooleanPP, name := "Open");
    at      := NEW(AtSpecPP, name := "At");
    chain   := NEW(ChainsPP, name := "Chain");
    scaled  := NEW(BooleanPP, name := "Scaled");
    fixedH  := NEW(BooleanPP, name := "FixedH");
    fixedV  := NEW(BooleanPP, name := "FixedV");
    fixedHV := NEW(BooleanPP, name := "FixedHV");
  VAR
    res   : FVZChild;
    ch    : VBT.T;
    shaper: ZSplit.ReshapeControl;
  BEGIN
    (* it's OK, because we may be inserting the form dynamically
       IF state.zsplit = NIL THEN RAISE Error ("ZChild must be
       inside a ZSplit.") END; *)
    ParseProps(cl, list, state, PP3{name, at, chain},
               KP5{open, scaled, fixedH, fixedV, fixedHV});
    res := cl.fv.realize("ZChild", name.valname);
    state.zchild := res;
    ch := OneChild(cl, list, state);
    IF chain.shaper # NIL THEN
      shaper := chain.shaper
    ELSIF scaled.val THEN
      shaper := ZChildVBT.Scaled
    ELSIF fixedH.val THEN
      shaper := ZChildVBT.ScaledHFixed
    ELSIF fixedV.val THEN
      shaper := ZChildVBT.ScaledVFixed
    ELSIF fixedHV.val THEN
      shaper := ZChildVBT.ScaledHVFixed
    ELSE
      shaper := NIL
    END;
    IF at.val.edges THEN
      IF shaper = NIL THEN shaper := ZChildVBT.Scaled END;
      res := res.initFromEdges(
               ch, at.val.w, at.val.e, at.val.n, at.val.s,
               at.val.type, shaper, open.val)
    ELSE
      IF shaper = NIL THEN shaper := ZChildVBT.ScaledHVFixed END;
      res := res.init(ch, at.val.h, at.val.v, at.val.loc,
                      at.val.type, shaper, open.val)
    END;
    AddNameProp(cl, res, name, state);
    RETURN res
  END pZChild;

PROCEDURE pZGrow (         cl  : ParseClosure;
                  VAR      list: RefList.T;
                  READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state          := s;
    name           := NamePP();
    res  : FVZGrow;
    ch   : VBT.T;
  BEGIN
    ParseProps(cl, list, state, PP1{name});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("ZGrow", name.valname);
    res := res.init(
             NEW(ShadowedFeedbackVBT.T).init(ch, state.shadow));
    AddNameProp(cl, res, name, state);
    RETURN res
  END pZGrow;

PROCEDURE pZMove (         cl  : ParseClosure;
                  VAR      list: RefList.T;
                  READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state          := s;
    name           := NamePP();
    res  : FVZMove;
    ch   : VBT.T;
  BEGIN
    ParseProps(cl, list, state, PP1{name});
    ch := OneChild(cl, list, state);
    res := cl.fv.realize("ZMove", name.valname);
    res := res.init(
             NEW(ShadowedFeedbackVBT.T).init(ch, state.shadow));
    AddNameProp(cl, res, name, state);
    RETURN res
  END pZMove;

(* ============================ Video and Audio  =========================== *)

PROCEDURE pVideo (         cl  : ParseClosure;
                  VAR      list: RefList.T;
                  READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state       := s;
    name        := NamePP();
    source      := NEW(TextPP, found := TRUE);
    quality     := NEW(CardinalPP, name := "Quality", val := 8);
    colors      := NEW(CardinalPP, name := "Colors", val := 50);
    width       := NEW(CardinalPP, name := "Width", val := 640);
    height      := NEW(CardinalPP, name := "Height", val := 480);
    synchronous := NEW(BooleanPP, name := "Synchronous");
    msecs       := NEW(CardinalPP, name := "MSecs");
    paused      := NEW(BooleanPP, name := "Paused");
    fixed       := NEW(BooleanPP, name := "FixedSize");

    res: FVVideo;
  BEGIN
    ParseProps(
      cl, list, state,
      PP7{name, source, quality, colors, width, height, msecs},
      KP3{synchronous, paused, fixed}, main := source);

    IF source.val = NIL OR Text.Empty(source.val) THEN
      RAISE Error("Video: must specify a source host name");
    END;
    IF quality.val < FIRST(JVSink.Quality)
         OR LAST(JVSink.Quality) < quality.val THEN
      RAISE Error("Video quality must be between 0 and 15");
    END;
    res := cl.fv.realize("Video", name.valname);
    res := res.init(
             source.val, quality.val, colors.val, width.val,
             height.val, synchronous.val, fixed.val, msecs.val);

    IF paused.val THEN res.setPaused(TRUE); END;

    AddNameProp(cl, res, name, state);
    RETURN res
  END pVideo;

PROCEDURE pAudio (         cl  : ParseClosure;
                  VAR      list: RefList.T;
                  READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state         := s;
    name          := NamePP();
    source        := NEW(TextPP, name := "Value");
    volume        := NEW(IntegerPP, name := "Volume");
    mute          := NEW(BooleanPP, name := "Mute");
    ignoreMapping := NEW(BooleanPP, name := "IgnoreMapping");
    res: FVAudio;
  BEGIN
    ParseProps(cl, list, state, PP3{name, source, volume},
               KP2{mute, ignoreMapping});

    IF source.val = NIL OR Text.Empty(source.val) THEN
      RAISE Error("Audio: must specify a source host name");
    END;

    IF volume.val < FIRST(Jva.Volume)
         OR LAST(Jva.Volume) < volume.val THEN
      RAISE Error(Fmt.F("Audio: value must be in range [%s..%s]",
                        Fmt.Int(FIRST(Jva.Volume)),
                        Fmt.Int(LAST(Jva.Volume))));
    END;

    res := cl.fv.realize("Audio", name.valname);
    TRY
      EVAL res.init(OneChild(cl, list, state), source.val,
                    mute.val, ignoreMapping.val, volume.val);
    EXCEPT
    | OSError.E (e) =>
        VAR etext := "";
        BEGIN
          IF e # NIL AND e.head # NIL THEN
            etext := RdUtils.FailureText(e);
          END;
          RAISE Error("Audio: initialising " & etext);
        END;
    | Thread.Alerted => RAISE Error("Audio: Thread alerted");
    END;

    AddNameProp(cl, res, name, state);
    RETURN res
  END pAudio;

(* ============================= IntApply  =============================== *)

PROCEDURE pIntApply (         cl  : ParseClosure;
                     VAR      list: RefList.T;
                     READONLY s   : State         ): VBT.T
  RAISES {Error} =
  VAR
    state                    := s;
    name                     := NamePP();
    forName                  := NEW(SymbolPP, name := "For");
    propertyName             := NEW(TextPP, name := "Property");
    res         : FVIntApply;
  BEGIN
    ParseProps(cl, list, state, PP3{name, forName, propertyName});
    IF forName = NIL OR Text.Empty(forName.valname) THEN
      RAISE Error("IntApply: must specify (For ...) property");
    END;
    res := cl.fv.realize("IntApply", name.valname);
    EVAL res.init(cl.fv, OneChild(cl, list, state),
                  forName.valname, propertyName.val);
    AddNameProp(cl, res, name, state);
    RETURN res;
  END pIntApply;


(* ====================================================================== *)
(* Parsing routines for inherited properties ("states") *)
(* ====================================================================== *)

PROCEDURE pMacro (list: RefList.T; VAR state: State) RAISES {Error} =
  (* (Macro name [BOA] (formals) bq-expr) *)
  BEGIN
    WITH m    = Macro.Parse (list),
         pair = AssocQ (state.macros, list.head) DO
      IF pair # NIL THEN
        pair.tail.head := m
      ELSE
        Push (state.macros, RefList.List2 (list.head, m))
      END
    END
  END pMacro;

(* Follow the guidelines in Kobara's book on Motif.  Nice background colors
   have RGB components that are each between 155 and 175 on a scale of 0-255.
   If the color is in that range, then the LightShadow should be computed "by
   multiplying the background color R, G, and B numbers each by 1.50".  Well,
   that arithmetic isn't quite right; 175 * 1.50 > 255.  So we just scale
   linearly so that 175 comes out at 0.95 ("not quite hitting white").
   Likewise, the DarkShadow should be computed by multiplying the BgColor
   values by 0.5.  The values in an RGB will be "gamma-corrected"
   by Trestle, so we use "true RGB" values here. *)
   
CONST
  rgb155      = 155.0 / 255.0; 
  rgb175      = 175.0 / 255.0;
  scaleLight  = 0.95 / rgb175;
  scaleDark   = 0.5;
    
PROCEDURE pBgColor (list: RefList.T; VAR state: State)
  RAISES {Error} =
  VAR
    r     := ColorRGB(list, PaintOp.BW.UseBg);
    red   := r.rgb.r;
    green := r.rgb.g;
    blue  := r.rgb.b;
    nice  := TRUE;
  BEGIN
    state.bgRGB := r.rgb;
    state.bgOp := r.op;
    nice := nice AND rgb155 <= red AND red <= rgb175;
    nice := nice AND rgb155 <= green AND green <= rgb175;
    nice := nice AND rgb155 <= blue AND blue <= rgb175;
    IF nice THEN
      state.darkRGB.r := red * scaleDark;
      state.darkRGB.g := green * scaleDark;
      state.darkRGB.b := blue * scaleDark;
      state.lightRGB.r := red * scaleLight;
      state.lightRGB.g := green * scaleLight;
      state.lightRGB.b := blue * scaleLight;
      state.lightOp :=
        PaintOp.FromRGB(
          state.lightRGB.r, state.lightRGB.g, state.lightRGB.b,
          PaintOp.Mode.Accurate, -1.0, PaintOp.BW.UseFg);
      state.darkOp :=
        PaintOp.FromRGB(
          state.darkRGB.r, state.darkRGB.g, state.darkRGB.b,
          PaintOp.Mode.Accurate, -1.0, PaintOp.BW.UseFg)
    END;
    state.shadow :=
      Shadow.New(state.shadowSz, state.bgOp, state.fgOp,
                 state.lightOp, state.darkOp)
  END pBgColor;

PROCEDURE pColor (list: RefList.T; VAR state: State) RAISES {Error} =
  BEGIN
    WITH r = ColorRGB (list) DO state.fgRGB := r.rgb; state.fgOp := r.op END;
    state.shadow := Shadow.New (state.shadowSz, state.bgOp, state.fgOp,
                                state.lightOp, state.darkOp)
  END pColor;

PROCEDURE pLightShadow (list: RefList.T; VAR state: State) RAISES {Error} =
  BEGIN
    WITH r = ColorRGB (list) DO
      state.lightRGB := r.rgb;
      state.lightOp := r.op
    END;
    state.shadow := Shadow.New (state.shadowSz, state.bgOp, state.fgOp,
                                state.lightOp, state.darkOp)
  END pLightShadow;

PROCEDURE pDarkShadow (list: RefList.T; VAR state: State) RAISES {Error} =
  BEGIN
    WITH r = ColorRGB (list) DO
      state.darkRGB := r.rgb;
      state.darkOp := r.op
    END;
    state.shadow := Shadow.New (state.shadowSz, state.bgOp, state.fgOp,
                                state.lightOp, state.darkOp)
  END pDarkShadow;

EXCEPTION BadColorSpec;         (* internal *)

TYPE RgbOp = RECORD rgb: Color.T; op: PaintOp.T END;

VAR
  qRGB := Atom.FromText ("RGB");
  qHSV := Atom.FromText ("HSV");
  
PROCEDURE ColorRGB (list: RefList.T; bw := PaintOp.BW.UseFg): RgbOp
  RAISES {Error} =
  VAR
    original                         := list;
    res     : RgbOp;
    rep                              := qRGB;
    vals    : ARRAY [0 .. 2] OF REAL;
  BEGIN
    TRY
      IF list = NIL THEN RAISE BadColorSpec END;
      TYPECASE list.head OF
      | NULL => RAISE BadColorSpec
      | TEXT (t) =>
          IF list.tail # NIL THEN RAISE BadColorSpec END;
          res.rgb := ColorName.ToRGB (t)
      | REFANY =>
          IF RefList.Length (list) = 4 THEN
            TYPECASE Pop (list) OF
            | NULL => RAISE BadColorSpec
            | Atom.T (s) =>
                IF s = qRGB OR s = qHSV THEN
                  rep := s
                ELSE
                  RAISE BadColorSpec
                END
            ELSE
              RAISE BadColorSpec
            END
          END;
          IF RefList.Length (list) # 3 THEN RAISE BadColorSpec END;
          FOR i := 0 TO 2 DO
            TYPECASE Pop (list) OF
            | NULL => RAISE BadColorSpec
            | REF INTEGER (ri) =>
                IF ri^ = 0 THEN
                  vals [i] := 0.0
                ELSIF ri^ = 1 THEN
                  vals [i] := 1.0
                ELSE
                  RAISE BadColorSpec
                END
            | REF REAL (rr) => vals [i] := rr^
            ELSE
              RAISE BadColorSpec
            END
          END;
          IF rep = qHSV THEN
            res.rgb :=
              Color.FromHSV (Color.HSV {vals [0], vals [1], vals [2]})
          ELSE
            res.rgb := Color.T {vals [0], vals [1], vals [2]};
          END;
      END;
      res.op := PaintOp.FromRGB (res.rgb.r, res.rgb.g, res.rgb.b,
                                 PaintOp.Mode.Accurate, -1.0, bw)
    EXCEPT
    | BadColorSpec =>
        Gripe ("Illegal color-spec: ", original); <* ASSERT FALSE *>
    | ColorName.NotFound =>
        Gripe ("No such color: ", original); <* ASSERT FALSE *>
    END;
    RETURN res
  END ColorRGB;

PROCEDURE pFont (list: RefList.T; VAR state: State) RAISES {Error} =
  BEGIN
    IF RefList.Length (list) = 1 AND ISTYPE (list.head, TEXT) THEN
      state.fontName := OneText (list)
    ELSE
      state.fontMetrics :=
        ParseFont (list, state.fontMetrics, DefaultFontMetrics);
      state.fontName := MetricsToName (state.fontMetrics)
    END;
    state.font := FVRuntime.FindFont (state.fontName)
  END pFont;

PROCEDURE pLabelFont (list: RefList.T; VAR state: State) RAISES {Error} =
  BEGIN
    IF RefList.Length (list) = 1 AND ISTYPE (list.head, TEXT) THEN
      state.labelFontName := OneText (list)
    ELSE
      state.labelFontMetrics :=
        ParseFont (list, state.labelFontMetrics, DefaultLabelFontMetrics);
      state.labelFontName := MetricsToName (state.labelFontMetrics)
    END;
    state.labelFont := FindFont (state.labelFontName)
  END pLabelFont;

PROCEDURE MetricsToName (metrics: RefList.T): TEXT =
  VAR
    wr           := TextWr.New ();
    pair: RefList.T;
  <* FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    FOR i := 0 TO LAST (MetricsProcs) DO
      Wr.PutChar (wr, '-');
      pair := AssocQ (metrics, MetricsProcs [i].symname);
      IF pair = NIL THEN
        Wr.PutChar (wr, '*')
      ELSE
        Wr.PutText (wr, pair.tail.head)
      END
    END;
    RETURN TextWr.ToText (wr)
  END MetricsToName;
    
PROCEDURE ParseFont (alist, metrics, default: RefList.T): RefList.T
  RAISES {Error} =
  VAR n: INTEGER;
  PROCEDURE gripe (x: REFANY) RAISES {Error} =
    BEGIN
      Gripe ("Bad font-spec: ", x)
    END gripe;
  BEGIN
    WHILE alist # NIL DO
      TYPECASE Pop (alist) OF
      | NULL => gripe (NIL)
      | Atom.T (sym) =>
          IF sym = qReset THEN
            metrics := RefList.Append (default, metrics)
          ELSE
            gripe (sym)
          END
      | RefList.T (pair) =>
          TYPECASE pair.head OF
          | NULL => gripe (pair)
          | Atom.T (sym) =>
              IF MetricsNameTable.get (sym, n) THEN
                MetricsProcs [n].proc (sym, pair.tail, metrics)
              ELSE
                gripe (pair)
              END
          | REFANY => gripe (pair)
          END
      | REFANY (r) => gripe (r)
      END
    END;
    RETURN metrics
  END ParseFont;

PROCEDURE mText (sym: Atom.T; arglist: RefList.T; VAR metrics: RefList.T)
  RAISES {Error} =
  BEGIN
    Push (metrics, RefList.List2 (sym, OneText (arglist)))
  END mText;

PROCEDURE mCardinal (sym: Atom.T; arglist: RefList.T; VAR metrics: RefList.T)
  RAISES {Error} =
  BEGIN
    IF RefList.Length (arglist) = 1 THEN (* gripe *)
      TYPECASE arglist.head OF
      | NULL =>                 (* gripe *)
      | TEXT (t) =>
          IF Text.Equal (t, "*") THEN
            Push (metrics, RefList.List2 (sym, t));
            RETURN
          END
      | REF INTEGER (ri) =>
          IF ri^ >= 0 THEN
            Push (metrics, RefList.List2 (sym, Fmt.Int (ri^)));
            RETURN
          END
      ELSE                      (* gripe *)
      END
    END;
    Gripe ("Bad font-spec: ", arglist); <* ASSERT FALSE *>
  END mCardinal;

PROCEDURE pShadowSize (list: RefList.T; VAR state: State) RAISES {Error} =
  BEGIN
    state.shadowSz := Pts.ToMM(OneReal (list));
    state.shadow := Shadow.New (state.shadowSz, state.bgOp, state.fgOp,
                                state.lightOp, state.darkOp)
  END pShadowSize;


(* ====================================================================== *)
(* Parsing routines for local properties *)
(* ====================================================================== *)

TYPE
  PP = OBJECT                   (* Property pair *)
         name  := "Main";
         found := FALSE
       METHODS
         set (form: RefList.T) RAISES {Error}
       END;
  KP0 = ARRAY [0 .. -1] OF BooleanPP;
  KP1 = ARRAY [0 .. 0] OF BooleanPP;
  KP2 = ARRAY [0 .. 1] OF BooleanPP;
  KP3 = ARRAY [0 .. 2] OF BooleanPP;
  KP4 = ARRAY [0 .. 3] OF BooleanPP;
  KP5 = ARRAY [0 .. 4] OF BooleanPP;
  KP6 = ARRAY [0 .. 5] OF BooleanPP;

  PP0 = ARRAY [0 .. -1] OF PP;
  PP1 = ARRAY [0 .. 0] OF PP;
  PP2 = ARRAY [0 .. 1] OF PP;
  PP3 = ARRAY [0 .. 2] OF PP;
  PP4 = ARRAY [0 .. 3] OF PP;
  PP5 = ARRAY [0 .. 4] OF PP;
  PP6 = ARRAY [0 .. 5] OF PP;
  PP7 = ARRAY [0 .. 6] OF PP;

  EP0 = ARRAY [0 .. -1] OF EnumPP;
  EP1 = ARRAY [0 .. 0] OF EnumPP;

PROCEDURE ParseProps (         cl   : ParseClosure;
                      VAR      list : RefList.T;
                      VAR      state: State;
                      READONLY props: ARRAY OF PP        := PP0 {};
                      READONLY keys : ARRAY OF BooleanPP := KP0 {};
                               main : PP                 := NIL;
                      READONLY enums: ARRAY OF EnumPP    := EP0 {}  )
  RAISES {Error} =
  (* This is where we parse the properties in a component-list.  We keep
     scanning items until we reach something that isn't a known property.  The
     component-parser that called us is responsible for parsing all the
     remaining items on the list. *)
  VAR copy := list;
  BEGIN
    WHILE list # NIL DO
      copy := list;
      list := ParseProp (cl, list, state, props, keys, main, enums);
      IF list = copy THEN EXIT END
    END;
    IF main = NIL THEN          (* skip *)
    ELSIF list # NIL THEN
      main.set (list);
      list := NIL
    ELSIF NOT main.found THEN
      RAISE Error ("Missing Main property")
    END;
    (* Make sure they picked one in each enumeration. *)
    FOR i := FIRST (enums) TO LAST (enums) DO
      IF enums [i].chosen # -1 THEN (* skip *)
      ELSIF NOT enums [i].choices [enums [i].default].found THEN
        enums [i].choices [enums [i].default].val := TRUE;
        enums [i].chosen := enums [i].default
      ELSE
        Gripe ("Default marked #False, but no alternative was selected: ",
               enums [i].choices [enums [i].default].name); <* ASSERT FALSE *>
      END
    END
  END ParseProps;

PROCEDURE ParseProp (         cl   : ParseClosure;
                     VAR      list : RefList.T;
                     VAR      state: State;
                     READONLY props: ARRAY OF PP;
                     READONLY keys : ARRAY OF BooleanPP;
                              main : PP;
                     READONLY enums: ARRAY OF EnumPP     ): RefList.T
  RAISES {Error} =
  VAR sProc: StateProc; symname: TEXT;
  BEGIN
    TYPECASE list.head OF
    | NULL =>
    | Atom.T (sym) =>            (* Is it a "keyword", like MenuStyle? *)
        symname := Atom.ToText (sym);
        FOR i := FIRST (keys) TO LAST (keys) DO
          IF Text.Equal (symname, keys [i].name) THEN
            keys [i].val := TRUE;
            keys [i].found := TRUE;
            RETURN list.tail
          END
        END;
        (* It might be an enumeration keyword. *)
        FOR i := FIRST (enums) TO LAST (enums) DO
          FOR j := FIRST (enums [i].choices^) TO LAST (enums [i].choices^) DO
            IF Text.Equal (symname, enums [i].choices [j].name) THEN
              IF enums [i].chosen # -1 THEN
                Gripe ("Contradictory choices: ",
                       enums [i].choices [j].name & " "
                         & enums [i].choices [enums [i].chosen].name);
                <* ASSERT FALSE *>
              ELSE
                enums [i].choices [j].val := TRUE;
                enums [i].choices [j].found := TRUE;
                enums [i].chosen := j;
                RETURN list.tail
              END
            END
          END
        END
      (* If it's not a keyword, it might be a symbol-component (Bar or
         Fill) *)
    | RefList.T (form) =>
        TYPECASE Pop (form) OF
        | NULL =>
        | Atom.T (sym) =>
            (* Is it specific to this component?  E.g., (Height ...) *)
            symname := Atom.ToText (sym);
            FOR i := FIRST (props) TO LAST (props) DO
              IF Text.Equal (symname, props [i].name) THEN
                props [i].set (form); (* parse and set *)
                props [i].found := TRUE;
                RETURN list.tail
              END
            END;
            (* Is it a state like (BgColor ...)? *)
            sProc := FindStateProc (sym);
            IF sProc # NIL THEN sProc (form, state); RETURN list.tail END;
            (* Is it a macro?  Expand and re-test. *)
            WITH m = MacroFunction (sym, state) DO
              IF m # NIL THEN
                RETURN RefList.Cons (m.apply (form), list.tail)
              END
            END;
            (* Is it a Boolean for this component?  E.g., (Flex #True ...) *)
            FOR i := FIRST (keys) TO LAST (keys) DO
              IF Text.Equal (symname, keys [i].name) THEN
                keys [i].val := OneBoolean (form);
                keys [i].found := TRUE;
                RETURN list.tail
              END
            END;
            (* Is it an enumeration keyword? *)
            FOR i := FIRST (enums) TO LAST (enums) DO
              FOR j := FIRST (enums [i].choices^)
                  TO LAST (enums [i].choices^) DO
                IF Text.Equal (symname, enums [i].choices [j].name) THEN
                  enums [i].choices [j].found := TRUE;
                  IF OneBoolean (form) THEN
                    IF enums [i].chosen # -1 THEN
                      Gripe ("Contradictory choices: ",
                             enums [i].choices [j].name & " "
                               & enums [i].choices [enums [i].chosen].name);
                      <* ASSERT FALSE *>
                    ELSE
                      enums [i].choices [j].val := TRUE;
                      enums [i].chosen := j;
                      RETURN list.tail
                    END
                  ELSIF enums [i].chosen = j THEN
                    enums [i].choices [j].val := FALSE;
                    enums [i].chosen := -1;
                    RETURN list.tail
                  ELSE
                    RETURN list.tail
                  END
                END
              END
            END;
            (* Is it (Main ...)? *)
            IF main # NIL AND sym = qMain THEN
              main.set (form);
              main.found := TRUE;
              RETURN list.tail
            END;
            (* Is it Insert? *)
            IF sym = qInsert THEN
              RETURN RefList.AppendD (
                       InsertFile (OneText (form), cl.fv.path, cl.fv.baseURL), list.tail)
            END
          (* It must a component like (HBox ...). *)
        ELSE
        END
    ELSE
    END;
    RETURN list
  END ParseProp;


TYPE
  AtSpecPP = PP OBJECT
               val: RECORD
                      h, v, w, e, n, s := 0.5;
                      loc              := ZChildVBT.Location.Center;
                      type             := ZChildVBT.CoordType.Scaled;
                      edges            := FALSE
                    END
             OVERRIDES
               set := SetAtSpecPP
             END;
  BooleanPP = PP OBJECT val := FALSE OVERRIDES set := SetBooleanPP END;
  CardinalPP =
    PP OBJECT val: CARDINAL := 0 OVERRIDES set := SetCardinalPP END;
  CardinalListPP =
    PP OBJECT val: RefList.T := NIL OVERRIDES set := SetCardinalListPP END;
  ChainsPP = 
    PP OBJECT 
      shaper: ZSplit.ReshapeControl;
    OVERRIDES
      set := SetChainsPP
    END;
  EnumPP =
    PP OBJECT
      choices: REF ARRAY OF BooleanPP;
      chosen : [-1 .. LAST (CARDINAL)]  := -1;
      default: CARDINAL                 := 0
    METHODS
      init (READONLY a: ARRAY OF BooleanPP; default: CARDINAL): EnumPP
        := InitEnumPP
    END;
  IntegerPP = PP OBJECT val := 0 OVERRIDES set := SetIntegerPP END;
  RealPP = PP OBJECT val := 0.0 OVERRIDES set := SetRealPP END;
  SizeRangePP =
    PP OBJECT val := FlexVBT.DefaultRange OVERRIDES set := SetSizeRangePP END;
  SymbolPP = PP OBJECT
               val    : Atom.T := NIL;
               valname: TEXT   := ""
             OVERRIDES
               set := SetSymbolPP
             END;
  TextPP = PP OBJECT val: TEXT := NIL OVERRIDES set := SetTextPP END;
  TextListPP =
    PP OBJECT val: RefList.T := NIL OVERRIDES set := SetTextListPP END;
  VBTPP = PP OBJECT val: RefList.T := NIL;  OVERRIDES set := SetVBTPP END;

PROCEDURE InitEnumPP (         pp     : EnumPP;
                      READONLY a      : ARRAY OF BooleanPP;
                               default: CARDINAL            ): EnumPP =
  BEGIN
    pp.choices := NEW (REF ARRAY OF BooleanPP, NUMBER (a));
    pp.choices^ := a;
    pp.default := default;
    RETURN pp
  END InitEnumPP;

PROCEDURE SetSymbolPP (pp: SymbolPP; form: RefList.T) RAISES {Error} =
  BEGIN
    pp.val := OneSymbol (form);
    pp.valname := Atom.ToText (pp.val)
  END SetSymbolPP;

PROCEDURE SetBooleanPP (pp: BooleanPP; form: RefList.T) RAISES {Error} =
  BEGIN
    pp.val := OneBoolean (form)
  END SetBooleanPP;

PROCEDURE SetIntegerPP (pp: IntegerPP; form: RefList.T) RAISES {Error} =
  BEGIN
    pp.val := OneInteger (form)
  END SetIntegerPP;

PROCEDURE SetRealPP (pp: RealPP; form: RefList.T) RAISES {Error} =
  BEGIN
    pp.val := OneReal (form)
  END SetRealPP;

PROCEDURE SetCardinalPP (pp: CardinalPP; form: RefList.T) RAISES {Error} =
  BEGIN
    pp.val := OneCardinal (form)
  END SetCardinalPP;

PROCEDURE SetCardinalListPP (pp: CardinalListPP; form: RefList.T)
  RAISES {Error} =
  PROCEDURE cardinalp (ref: REFANY): BOOLEAN =
    BEGIN
      TYPECASE ref OF
      | NULL => RETURN FALSE
      | REF INTEGER (ri) => RETURN ri^ >= 0
      ELSE
        RETURN FALSE
      END
    END cardinalp;
  BEGIN
    pp.val := ListOfType (form, cardinalp, "cardinals ")
  END SetCardinalListPP;

PROCEDURE SetTextListPP (pp: TextListPP; form: RefList.T) RAISES {Error} =
  PROCEDURE textp (ref: REFANY): BOOLEAN =
    BEGIN
      RETURN ISTYPE (ref, TEXT)
    END textp;
  BEGIN
    pp.val := ListOfType (form, textp, "texts ")
  END SetTextListPP;

PROCEDURE ListOfType (form: RefList.T;
                      p   : (PROCEDURE (ref: REFANY): BOOLEAN);
                      name: TEXT                                ): RefList.T
  RAISES {Error} =
  PROCEDURE every (l: RefList.T): BOOLEAN =
    BEGIN
      WHILE l # NIL DO IF NOT p (Pop (l)) THEN RETURN FALSE END END;
      RETURN TRUE
    END every;
  BEGIN
    (** Allow form to be (1 2 3 ...) or ((1 2 3 ...)),
        since =(1 2 3) is read as (Value (1 2 3)), which is
        the same as (Value 1 2 3). *)
    IF every (form) THEN RETURN form END;
    TYPECASE form.head OF
    | RefList.T (l) => IF form.tail = NIL AND every (l) THEN RETURN l END
    ELSE
    END;
    Gripe ("Bad list of " & name, form); <* ASSERT FALSE *>
  END ListOfType;

EXCEPTION BadAtSpec;

PROCEDURE SetAtSpecPP (pp: AtSpecPP; form: RefList.T)
  RAISES {Error} =
  VAR
    n       : ARRAY [0 .. 1] OF REAL;
    original := form;
    len   := RefList.Length (form);
    gotCoordType := FALSE;
  PROCEDURE pct (x: REAL) RAISES {BadAtSpec} =
    BEGIN IF x < 0.0 OR 1.0 < x THEN RAISE BadAtSpec END END pct;
  PROCEDURE ispct (x: REAL): BOOLEAN =
    BEGIN RETURN x >= 0.0 AND x <= 1.0 END ispct;
  PROCEDURE check () RAISES {BadAtSpec} =
    BEGIN
      IF form # NIL THEN RAISE BadAtSpec END;
      IF NOT gotCoordType THEN
        pp.val.type := ZChildVBT.CoordType.Absolute;
        IF pp.val.edges THEN
           IF ispct(pp.val.w) AND ispct(pp.val.e) AND
              ispct(pp.val.n) AND ispct(pp.val.s) THEN 
                pp.val.type := ZChildVBT.CoordType.Scaled
           END
        ELSE
           IF ispct(pp.val.h) AND ispct(pp.val.v) THEN
                pp.val.type := ZChildVBT.CoordType.Scaled
           END
        END
      END;
      IF pp.val.type = ZChildVBT.CoordType.Absolute THEN
        IF pp.val.edges THEN
          pp.val.w := Pts.ToMM (pp.val.w);
          pp.val.e := Pts.ToMM (pp.val.e);
          pp.val.n := Pts.ToMM (pp.val.n);
          pp.val.s := Pts.ToMM (pp.val.s)
        ELSE
          pp.val.h := Pts.ToMM (pp.val.h);
          pp.val.v := Pts.ToMM (pp.val.v)
        END
      ELSE
        IF pp.val.edges THEN
          pct (pp.val.w);
          pct (pp.val.e);
          pct (pp.val.n);
          pct (pp.val.s)
        ELSE
          pct (pp.val.h);
          pct (pp.val.v)
        END
      END
    END check;
  BEGIN
    TRY
      pp.val.type := ZChildVBT.CoordType.Absolute;
      IF len < 2 OR len > 5 THEN RAISE BadAtSpec END;
      FOR i := 0 TO 1 DO
        TYPECASE Pop (form) OF
        | NULL => RAISE BadAtSpec
        | REF INTEGER (ri) => n [i] := FLOAT (ri^)
        | REF REAL (rr) => n [i] := rr^
        ELSE
          RAISE BadAtSpec
        END
      END;
      pp.val.h := n [0];
      pp.val.v := n [1];
      IF form = NIL THEN check (); RETURN END;
      TYPECASE Pop (form) OF
      | NULL => RAISE BadAtSpec
      | Atom.T (s) =>
          IF GetLocation (s, pp.val.loc) THEN
            IF form # NIL THEN
              IF GetCoordType (Pop (form), pp.val.type) THEN
                gotCoordType := TRUE
              ELSE RAISE BadAtSpec END
            END
          ELSIF GetCoordType (s, pp.val.type) THEN gotCoordType := TRUE
          ELSE RAISE BadAtSpec END;
          check ();
          RETURN
      | REF INTEGER (ri) => pp.val.n := FLOAT (ri^)
      | REF REAL (rr) => pp.val.n := rr^
      ELSE
        RAISE BadAtSpec
      END;
      IF form = NIL THEN RAISE BadAtSpec END;
      pp.val.edges := TRUE;
      pp.val.w := n [0];
      pp.val.e := n [1];
      TYPECASE Pop (form) OF
      | NULL => RAISE BadAtSpec
      | REF INTEGER (ri) => pp.val.s := FLOAT (ri^)
      | REF REAL (rr) => pp.val.s := rr^
      ELSE RAISE BadAtSpec
      END;
      IF form # NIL THEN
        IF GetCoordType (Pop (form), pp.val.type) THEN gotCoordType := TRUE
        ELSE RAISE BadAtSpec END;
      END;
      check ()
    EXCEPT
    | BadAtSpec =>
        Gripe ("Bad 'At' spec: ", original); <* ASSERT FALSE *>
    END
  END SetAtSpecPP;

VAR
  Locations := ARRAY ZChildVBT.Location OF
                 Atom.T {Atom.FromText ("NW"),
                             Atom.FromText ("NE"),
                             Atom.FromText ("SW"),
                             Atom.FromText ("SE"),
                             Atom.FromText ("Center")};
                                      
PROCEDURE GetLocation (s: Atom.T; VAR loc: ZChildVBT.Location):
  BOOLEAN =
  BEGIN
    FOR i := FIRST (Locations) TO LAST (Locations) DO
      IF s = Locations [i] THEN loc := i; RETURN TRUE END
    END;
    RETURN FALSE
  END GetLocation;

VAR
  CoordTypes := ARRAY ZChildVBT.CoordType OF Atom.T {
                  Atom.FromText ("Absolute"),
                  Atom.FromText ("Relative")};


PROCEDURE GetCoordType (x: REFANY; VAR type: ZChildVBT.CoordType):
  BOOLEAN =
  BEGIN
    TYPECASE x OF
    | NULL =>
    | Atom.T (s) =>
        FOR i := FIRST (CoordTypes) TO LAST (CoordTypes) DO
          IF s = CoordTypes [i] THEN type := i; RETURN TRUE END
        END
    ELSE
    END;
    RETURN FALSE
  END GetCoordType;

PROCEDURE SetChainsPP (pp: ChainsPP; form: RefList.T) RAISES {Error} =
  BEGIN
    pp.shaper := NEW(ZSplit.ChainReshapeControl, chains := ChainSet (form)); 
  END SetChainsPP;

PROCEDURE ChainSet (VAR list: RefList.T): ZSplit.ChainSet
    RAISES {Error} =
  VAR chain: ZSplit.Ch; 
    chainSet := ZSplit.ChainSet{};
  BEGIN
    WHILE RefList.Length (list) # 0 DO
      IF GetChain (Pop(list), chain) THEN
        chainSet := chainSet + ZSplit.ChainSet{chain};
      ELSE
        Gripe ("Unknown side for chaining", list)
      END
    END;
    RETURN chainSet
  END ChainSet;

VAR
  Chains := ARRAY ZSplit.Ch OF
                 Atom.T {Atom.FromText ("W"),
                             Atom.FromText ("E"),
                             Atom.FromText ("N"),
                             Atom.FromText ("S")};
                                      
PROCEDURE GetChain (s: Atom.T; VAR ch: ZSplit.Ch):
  BOOLEAN =
  BEGIN
    FOR i := FIRST (Chains) TO LAST (Chains) DO
      IF s = Chains [i] THEN ch := i; RETURN TRUE END
    END;
    RETURN FALSE
  END GetChain;

PROCEDURE SetSizeRangePP (pp: SizeRangePP; form: RefList.T) RAISES {Error} =
  BEGIN
    pp.val := SizeRange (form)
  END SetSizeRangePP;

EXCEPTION BadSize;

PROCEDURE SizeRange (VAR list: RefList.T): FlexVBT.SizeRange
  RAISES {Error} =
  VAR
    size     := FlexVBT.DefaultRange;
    original := list;
  BEGIN
    TRY
      IF list = NIL THEN RAISE BadSize END;
      GetNatural (list, size);
      IF RefList.Length (list) = 4 THEN GetStretchOrShrink (list, size); END;
      IF RefList.Length (list) = 2 THEN GetStretchOrShrink (list, size); END;
      IF RefList.Length (list) # 0 THEN RAISE BadSize END;
      RETURN size;
    EXCEPT
    | BadSize => Gripe ("Illegal size", original); <* ASSERT FALSE *>
    END;
  END SizeRange;

PROCEDURE GetNatural (VAR list: RefList.T;
                      VAR size: FlexVBT.SizeRange)
  RAISES {BadSize} =
  BEGIN
    TYPECASE list.head OF
    | NULL => RAISE BadSize
    | REF REAL, REF INTEGER =>
        size.natural := Pts.ToMM(GetNum(list));
    ELSE
      (* no leading number *)
    END;
  END GetNatural;

PROCEDURE GetStretchOrShrink (VAR list: RefList.T;
                              VAR size: FlexVBT.SizeRange)
  RAISES {BadSize} =
  BEGIN
    TYPECASE Pop(list) OF
    | NULL => RAISE BadSize
    | Atom.T (sym) =>
        IF sym = qPlus THEN
          size.stretch := Pts.ToMM(GetNum(list, TRUE));
        ELSIF sym = qMinus THEN
          size.shrink := Pts.ToMM(GetNum(list, TRUE));
        ELSE
          RAISE BadSize
        END
    ELSE
      RAISE BadSize
    END
  END GetStretchOrShrink;

VAR
  Infinities := ARRAY [0 .. 5] OF
                  Atom.T {
                  Atom.FromText ("Inf"), Atom.FromText ("inf"),
                  Atom.FromText ("INF"), Atom.FromText ("Infinity"),
                  Atom.FromText ("infinity"), Atom.FromText ("INFINITY")};

PROCEDURE GetNum (VAR list        : RefList.T;
                      infOK       : BOOLEAN     := FALSE;
                      positiveOnly: BOOLEAN     := TRUE   ): REAL
  RAISES {BadSize} =
  BEGIN
    TYPECASE Pop(list) OF
    | NULL =>
    | REF REAL (rr) =>
        IF positiveOnly AND rr^ < 0.0 THEN RAISE BadSize END;
        RETURN rr^
    | REF INTEGER (ri) =>
        IF positiveOnly AND ri^ < 0 THEN RAISE BadSize END;
        RETURN FLOAT(ri^)
    | Atom.T (sym) =>
        IF NOT infOK THEN RAISE BadSize END;
        FOR i := FIRST(Infinities) TO LAST(Infinities) DO
          IF sym = Infinities[i] THEN RETURN FlexVBT.Infinity END
        END
    ELSE
    END;
    RAISE BadSize
  END GetNum;

PROCEDURE SetVBTPP (pp: VBTPP; form: RefList.T) =
  BEGIN
    pp.val := form
  END SetVBTPP;

(*
  VAR
    state := pp.state;
    name  := NamePP ();
  BEGIN
    ParseProps (form, state, PP1 {name});
    pp.val := OneChild (pp.cl, form, state);
    AddNameProp (pp.cl, pp.val, name, state)
  END SetVBTPP;
*)

PROCEDURE OneChild (         cl   : ParseClosure;
                             list : RefList.T;
                    READONLY state: State         ): VBT.T
  RAISES {Error} =
  BEGIN
    IF list = NIL THEN
      Gripe("A component is required here", ""); <* ASSERT FALSE *>
    ELSIF list.tail # NIL THEN
      Gripe(Fmt.F("A single component is required here: %s",
                  ToText(list, maxDepth := 3, maxLength := 4)));
      <* ASSERT FALSE *>
    ELSE
      RETURN Item(cl, Pop(list), state)
    END
  END OneChild;

PROCEDURE SetTextPP (pp: TextPP; form: RefList.T) RAISES {Error} =
  BEGIN
    pp.val := OneText (form)
  END SetTextPP;

PROCEDURE AddChildren (         cl   : ParseClosure;
                                v    : MultiSplit.T;
                                list : RefList.T;
                       READONLY state: State         )
  RAISES {Error} =
  BEGIN
    WHILE list # NIL DO
      TYPECASE Pop(list) OF
      | NULL =>
          Gripe("NIL is an illegal form"); <* ASSERT FALSE *>
      | RefList.T (a) =>
          TYPECASE a.head OF
          | NULL =>
              Gripe("(NIL ...) is an illegal form"); <* ASSERT FALSE *>
          | Atom.T (sym) =>
              IF sym = qInsert THEN
                list := RefList.Append(InsertFile(
                                         OneText(a.tail),
                                         cl.fv.path, cl.fv.baseURL), list)
              ELSE
                MultiSplit.AddChild(v, Item(cl, a, state))
              END
          ELSE
            MultiSplit.AddChild(v, Item(cl, a, state))
          END
      | REFANY (ra) =>
          MultiSplit.AddChild(v, Item(cl, ra, state))
      END
    END
  END AddChildren;

PROCEDURE OneText (list: RefList.T): TEXT RAISES {Error} =
  BEGIN
    IF list # NIL THEN
      TYPECASE list.head OF
      | NULL =>
          (* Technically, this is illegal, but the FormsVBT prettyprinter
             in Ivy converts "" to (), and there's still some of that code
             around. *)
          IF list.tail = NIL THEN RETURN "" END
      | TEXT (t) => IF list.tail = NIL THEN RETURN t END
      ELSE
      END
    END;
    Gripe ("Bad text-form: ", list); <* ASSERT FALSE *>
  END OneText;

PROCEDURE OneCardinal (list: RefList.T): CARDINAL RAISES {Error} =
  BEGIN
    IF list # NIL THEN
      TYPECASE list.head OF
      | NULL =>
      | REF INTEGER (ri) =>
          IF ri^ >= 0 AND list.tail = NIL THEN RETURN ri^ END
      ELSE
      END
    END;
    Gripe ("Expected a cardinal integer: ", list); <* ASSERT FALSE *>
  END OneCardinal;

PROCEDURE OneInteger (list: RefList.T): INTEGER RAISES {Error} =
  BEGIN
    IF list # NIL THEN
      TYPECASE list.head OF
      | NULL =>
      | REF INTEGER (ri) => IF list.tail = NIL THEN RETURN ri^ END
      ELSE
      END
    END;
    Gripe ("Expected an integer: ", list); <* ASSERT FALSE *>
  END OneInteger;

PROCEDURE OneReal (list: RefList.T): REAL RAISES {Error} =
  BEGIN
    IF list # NIL THEN
      TYPECASE list.head OF
      | NULL =>
      | REF INTEGER (ri) => IF list.tail = NIL THEN RETURN FLOAT (ri^) END
      | REF REAL (rr) => IF list.tail = NIL THEN RETURN rr^ END
      ELSE
      END
    END;
    Gripe ("Expected a real number: ", list); <* ASSERT FALSE *>
  END OneReal;

PROCEDURE OneBoolean (form: RefList.T): BOOLEAN RAISES {Error} =
  BEGIN
    IF form # NIL AND form.tail = NIL THEN
      TYPECASE form.head OF
      | NULL =>
      | Atom.T (sym) =>
          IF sym = Sx.True THEN
            RETURN TRUE
          ELSIF sym = Sx.False THEN
            RETURN FALSE
          END
      ELSE
      END
    END;
    Gripe ("Not a BOOLEAN: ", form); <* ASSERT FALSE *>
  END OneBoolean;

PROCEDURE OneSymbol (form: RefList.T): Atom.T RAISES {Error} =
  BEGIN
    IF form # NIL AND form.tail = NIL THEN
      TYPECASE form.head OF
      | NULL =>
      | Atom.T (sym) => RETURN sym
      ELSE
      END
    END;
    Gripe ("Not a symbol: ", form); <* ASSERT FALSE *>
  END OneSymbol;

PROCEDURE AssertEmpty (list: RefList.T) RAISES {Error} =
  BEGIN
    IF list # NIL THEN Gripe ("Extra junk in form: ", list) END
  END AssertEmpty;

(* ====================== Runtime Utilities ========================= *)

PROCEDURE AddNameProp (         cl   : ParseClosure;
                                v    : VBT.T;
                                pp   : SymbolPP;
                       READONLY state: State         ) RAISES {Error} =
  VAR stateRef: REF State;
  BEGIN
    IF Named (pp) THEN
      FVRuntime.SetVBT (cl.fv, pp.valname, v);
      stateRef := NEW (REF State);
      stateRef^ := state;
      stateRef^.name := pp.valname;
      VBT.PutProp (v, stateRef);
    END
  END AddNameProp;

PROCEDURE AddForProp (cl: ParseClosure; v: VBT.T; pp: SymbolPP)
  RAISES {Error} =
  BEGIN
    IF pp.val = NIL THEN RAISE Error ("A name is required here.") END;
    cl.fixupList := NEW (FixupLink, targetName := pp.valname,
                         sourceVBT := v, next := cl.fixupList)
  END AddForProp;


(* ========================== Table Lookup =========================== *)

PROCEDURE FindComponentProc (sym: Atom.T): ComponentProc =
  VAR n: INTEGER;
  BEGIN
    IF ComponentNameTable.get (sym, n) THEN
      RETURN ComponentProcs [n]
    ELSE
      RETURN NIL
    END
  END FindComponentProc;

PROCEDURE FindRealizeProc (sym: Atom.T): RealizeProc RAISES {Error} =
  VAR n: INTEGER;
  BEGIN
    IF ComponentNameTable.get (sym, n) THEN
      RETURN RealizeProcs [n]
    ELSE
      Gripe ("Unknown component: ", sym); <* ASSERT FALSE *>
    END
  END FindRealizeProc;

PROCEDURE FindStateProc (sym: Atom.T): StateProc =
  VAR n: INTEGER;
  BEGIN
    IF StateNameTable.get (sym, n) THEN
      RETURN StateProcs [n]
    ELSE
      RETURN NIL
    END
  END FindStateProc;

CONST
  StateNames = ARRAY OF
                 TEXT {"BgColor", "Color", "DarkShadow", "Font",
                       "LabelFont", "LightShadow", "Macro", "ShadowSize"};

CONST
  StateProcs = ARRAY [0 .. LAST (StateNames)] OF
                 StateProc {pBgColor, pColor, pDarkShadow, pFont,
                            pLabelFont, pLightShadow, pMacro, pShadowSize};

(* NOTE: FVTypes contains type declarations corresponding to each
   component. When a new component is added, be sure to add an entry to
    Also, if the VBT class for a component changes (unlikely, but
   possible), be sure to modify the component's entry in FVTypes
   appropriately. *)

CONST
  ComponentNames = ARRAY OF
                     TEXT{
                     "Any", "AnyFilter", "AnySplit",
                     "Audio", "Bar", "Boolean", "Border", "Browser",
                     "Button", "Chisel", "Choice", "CloseButton",
                     "DirMenu", "FileBrowser", "Fill", "Filter", "Frame",
                     "Generic", "Glue", "Guard", "HBox", "HPackSplit",
                     "HTile", "Help", "Helper", "Image", "IntApply", "LinkButton",
                     "LinkMButton", "MButton", "Menu", "MultiBrowser",
                     "Numeric", "PageButton", "PageMButton", "Pixmap",
                     "PopButton", "PopMButton", "Radio", "Ridge", "Rim",
                     "Scale", "Scroller", "Shape", "Source", "Stable", "TSplit",
                     "Target", "Text", "TextEdit", "Texture",
                     "TrillButton", "TypeIn", "Typescript", "VBox",
                     "VPackSplit", "VTile", "Video", "Viewport",
                     "ZBackground", "ZChassis", "ZChild", "ZGrow", "ZMove",
                     "ZSplit"};

CONST
  ComponentProcs = ARRAY [0 .. LAST(ComponentNames)] OF
                     ComponentProc{
                     pAny, pAnyFilter, pAnySplit,
                     pAudio, pBar, pBoolean, pBorder, pBrowser, pButton,
                     pChisel, pChoice, pCloseButton, pDirMenu,
                     pFileBrowser, pFill, pFilter, pFrame, pGeneric, pGlue,
                     pGuard, pHBox, pHPackSplit, pHTile, pHelp, pHelper, pImage,
                     pIntApply, pLinkButton, pLinkMButton, pMButton, pMenu,
                     pMultiBrowser, pNumeric, pPageButton, pPageMButton,
                     pPixmap, pPopButton, pPopMButton, pRadio, pRidge,
                     pRim, pScale, pScroller, pShape, pSource, pStable, pTSplit,
                     pTarget, pText, pTextEdit, pTexture, pTrillButton,
                     pTypeIn, pTypescript, pVBox, pVPackSplit, pVTile,
                     pVideo, pViewport, pZBackground, pZChassis, pZChild,
                     pZGrow, pZMove, pZSplit};

CONST
  RealizeProcs = ARRAY [0 .. LAST(ComponentNames)] OF
                   RealizeProc{
                   rAny, rAnyFilter, rAnySplit,
                   rAudio, rBar, rBoolean, rBorder, rBrowser, rButton,
                   rChisel, rChoice, rCloseButton, rDirMenu, rFileBrowser,
                   rFill, rFilter, rFrame, rGeneric, rGlue, rGuard, rHBox,
                   rHPackSplit, rHTile, rHelp, rHelper, rImage, rIntApply, rLinkButton,
                   rLinkMButton, rMButton, rMenu, rMultiBrowser, rNumeric,
                   rPageButton, rPageMButton, rPixmap, rPopButton,
                   rPopMButton, rRadio, rRidge, rRim, rScale, rScroller,
                   rShape, rSource, rStable, rTSplit, rTarget, rText, rTextEdit,
                   rTexture, rTrillButton, rTypeIn, rTypescript, rVBox,
                   rVPackSplit, rVTile, rVideo, rViewport, rZBackground,
                   rZChassis, rZChild, rZGrow, rZMove, rZSplit};

TYPE
  mp = RECORD
         name                         : TEXT;
         proc                         : MetricsProc;
         fontDefault, labelFontDefault: TEXT;
         symname                      : Atom.T
       END;

(* In the following table, we use "impossible" names to prevent the client
   from specifying AdStyle and PixelSize, so these will always be "*" in
   the font name. *)
VAR
  MetricsProcs := ARRAY [0 .. 13] OF
                    mp {mp {"Foundry", mText, "*", "*", NIL},
                        mp {"Family", mText, "fixed", "helvetica", NIL},
                        mp {"WeightName", mText, "medium", "bold", NIL},
                        mp {"Slant", mText, "r", "r", NIL},
                        mp {"Width", mText, "semicondensed", "*", NIL},
                        mp {" -AdStyle- ", mText, "*", "*", NIL},
                        mp {" -PixelSize- ", mCardinal, "*", "*", NIL},
                        mp {"PointSize", mCardinal, "100", "100", NIL},
                        mp {"HRes", mCardinal, "*", "*", NIL},
                        mp {"VRes", mCardinal, "*", "*", NIL},
                        mp {"Spacing", mText, "*", "*", NIL},
                        mp {"AvgWidth", mCardinal, "*", "*", NIL},
                        mp {"Registry", mText, "iso8859", "iso8859", NIL},
                        mp {"Encoding", mText, "1", "1", NIL}};
(* The 14 metrics-components must be in this order, so that we can generate
   the strings easily.  I have no idea what "AdStyle" is. *)
   
VAR StateNameTable, ComponentNameTable, MetricsNameTable: AtomIntTbl.T;

PROCEDURE InitParser () =
  BEGIN
    StateNameTable := NEW (AtomIntTbl.Default).init (NUMBER (StateNames));
    ComponentNameTable :=
      NEW (AtomIntTbl.Default).init (NUMBER (ComponentNames));
    MetricsNameTable := NEW (AtomIntTbl.Default).init (NUMBER (MetricsProcs));
    FOR i := FIRST (StateNames) TO LAST (StateNames) DO
      EVAL StateNameTable.put (Atom.FromText (StateNames [i]), i)
    END;
    FOR i := FIRST (ComponentNames) TO LAST (ComponentNames) DO
      EVAL ComponentNameTable.put (Atom.FromText (ComponentNames [i]), i)
    END;
    FOR i := FIRST (MetricsProcs) TO LAST (MetricsProcs) DO
      WITH s = Atom.FromText (MetricsProcs [i].name) DO
        EVAL MetricsNameTable.put (s, i);
        MetricsProcs [i].symname := s
      END
    END;
    DefaultFontMetrics := NIL;
    DefaultLabelFontMetrics := NIL;
    FOR i := 0 TO 13 DO
      WITH mp = MetricsProcs [i] DO
        Push (DefaultFontMetrics, RefList.List2 (mp.symname, mp.fontDefault));
        Push (DefaultLabelFontMetrics,
              RefList.List2 (mp.symname, mp.labelFontDefault))
      END
    END
  END InitParser;

BEGIN 
END FormsVBT.

