(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Tue Jan 31 14:53:43 PST 1995 by kalsow  *)
(*      modified on Fri Jan  6 00:14:48 PST 1995 by najork  *)
(*      modified on Thu Apr 22 12:38:22 PDT 1993 by mhb     *)
(*      modified on Wed Jan  6 16:43:43 PST 1993 by steveg  *)
(*      modified on Wed Aug  5 17:44:27 PDT 1992 by guarino *)
(*      modified on Sun Aug  2 02:13:47 PDT 1992 by broder *)

MODULE TextView;
IMPORT ColorName, FormsVBT, StringSearchViewClass, PaintOp, 
       Rd, TextEditVBT, TextPort, Thread, VBT, Rsrc,
       View, VTDef, VText, ZeusPanel, ViewsBase, Text;


TYPE
  T = StringSearchViewClass.T OBJECT
        form             : FormsVBT.T;
        probe, match, pat, inpat: VText.Interval;
        lenPat           : CARDINAL;
        port             : TextPort.T;
        boxStart, boxLast: CARDINAL := 0;
      OVERRIDES
        oeSetup             := Setup;
        oeProbe             := Probe;
        oeResult            := Result;
        oePartialMatch      := PartialMatch;
        oePartialMatchClear := PartialMatchClear;
        oeCompleteMatch     := CompleteMatch;
        oeSlideTo           := SlideTo;
      END;


VAR
  (* Interval options; actually constants *)
  TrueIO := MakeHighlight(
              fg := PaintOp.Fg, bg := Color(ViewsBase.TrueC));
  FalseIO := MakeHighlight(
               fg := PaintOp.Fg, bg := Color(ViewsBase.FalseC));
  PartialIO := MakeHighlight(
                 fg := PaintOp.Fg,
                 bg := Color(ViewsBase.PartialC));
  CompleteIO := MakeHighlight(
                  fg := PaintOp.Fg,
                  bg := Color(ViewsBase.CompleteC));
  ProbeIO := MakeInverse(fg:= PaintOp.Fg, bg:=PaintOp.Bg);

  PatternIO := MakeBoxed();

PROCEDURE Color(color: TEXT): PaintOp.T =
  <* FATAL ColorName.NotFound *>
  VAR rgb := ColorName.ToRGB(color);
  BEGIN
    RETURN PaintOp.FromRGB(rgb.r, rgb.g, rgb.b);
  END Color;

PROCEDURE VTError (code: VTDef.ErrorCode): TEXT =
  BEGIN
    CASE code OF
    | VTDef.ErrorCode.IsNil => RETURN ("NIL vtext");
    | VTDef.ErrorCode.IllegalIndex => RETURN ("Illegal index ");
    | VTDef.ErrorCode.IllegalRegion => RETURN ("Illegal region");
    | VTDef.ErrorCode.IllegalCoord => RETURN ("Illegal coordinate");
    | VTDef.ErrorCode.IllegalDividers => RETURN ("Illegal dividers");
    | VTDef.ErrorCode.IllegalFont => RETURN ("Illegal font");
    | VTDef.ErrorCode.Closed => RETURN ("vtext already closed");
    ELSE
      RETURN ("unknown VTDef.ErrorCode")
    END;
  END VTError;

PROCEDURE MakeInverse (fg, bg: PaintOp.T): VText.IntervalOptions =
  BEGIN
    WITH cs = PaintOp.MakeColorScheme(fg := fg, bg := bg) DO
      RETURN VText.MakeIntervalOptions(
               VText.IntervalStyle.InverseStyle, cs, cs, cs.bg)
    END
  END MakeInverse;

PROCEDURE MakeHighlight (fg, bg: PaintOp.T): VText.IntervalOptions =
  BEGIN
    WITH cs = PaintOp.MakeColorScheme(fg := fg, bg := bg) DO
      RETURN VText.MakeIntervalOptions(
               VText.IntervalStyle.HighlightStyle, cs, cs, cs.bg)
    END
  END MakeHighlight;

PROCEDURE MakeBoxed (): VText.IntervalOptions =
  BEGIN
    WITH cs = PaintOp.MakeColorScheme(
                fg := PaintOp.Fg, bg := PaintOp.Bg) DO
      RETURN VText.MakeIntervalOptions(
               VText.IntervalStyle.BoxStyle, cs, cs, cs.bg)
    END
  END MakeBoxed;

PROCEDURE Setup (view: T; pattern, target: TEXT) =
  <* FATAL VTDef.Error *>
  BEGIN
    LOCK VBT.mu DO
      TRY
        FormsVBT.PutText(view.form, "text", target);
        FormsVBT.PutText(view.form, "pattern", pattern);
        FormsVBT.PutInteger(view.form, "probeCount", 0);
        view.lenPat := Text.Length(pattern);
        VText.MoveInterval(view.pat, 0, view.lenPat);
        view.boxStart := 0;
        view.boxLast := view.lenPat;
        VText.SwitchInterval(view.pat, VTDef.OnOffState.On);
        VBT.Mark(view.form);
      EXCEPT
      | FormsVBT.Error (msg) => ZeusPanel.ReportError(msg);
      | FormsVBT.Unimplemented =>
          ZeusPanel.ReportError(
            "FormsVBT.Unimplemented in SSTextView.Setup");
      END;
    END;
  END Setup;

PROCEDURE Probe (view: T; <* UNUSED *> i: CARDINAL; j: CARDINAL) =
  VAR
    indexL, indexR: VText.Index;
    options       : VText.IntervalOptions;
    state         : VText.OnOffState;
  BEGIN
    TRY
      FormsVBT.PutInteger(view.form, "probeCount",
                          FormsVBT.GetInteger(view.form, "probeCount") + 1);
      VText.ExplodeInterval(view.pat, indexL, indexR, options, state);
      IF j = indexL THEN
        VText.MoveInterval(view.pat, indexL + 1, indexR);
      ELSE
        VText.MoveInterval(view.pat, indexL, indexR - 1);
      END;
      VText.ChangeIntervalOptions(view.probe, ProbeIO);
      VText.MoveInterval(view.probe, j, j + 1);
      VText.SwitchInterval(view.probe, VTDef.OnOffState.On);
      (* TextPort.Normalize(view.port, j); *)
      VBT.Mark(view.form);
    EXCEPT
    | FormsVBT.Error (msg) => ZeusPanel.ReportError(msg);
    | FormsVBT.Unimplemented =>
        ZeusPanel.ReportError("FormsVBT.Unimplemented in SSTextView.Probe");
    | VTDef.Error (code) => ZeusPanel.ReportError(VTError(code));
    END;
  END Probe;

PROCEDURE Result (view: T; r: BOOLEAN) =
  BEGIN
    TRY
      IF r THEN
        VText.ChangeIntervalOptions(view.probe, TrueIO)
      ELSE
        VText.ChangeIntervalOptions(view.probe, FalseIO)
      END;
      VBT.Mark(view.form);
    EXCEPT
    | VTDef.Error (code) => ZeusPanel.ReportError(VTError(code));
    END;
  END Result;


PROCEDURE PartialMatch (view: T; i, j, len: CARDINAL) =
  BEGIN
    TRY
      IF j = view.boxStart THEN
        VText.MoveInterval(view.pat, j + len, view.boxLast);
      ELSE
        VText.MoveInterval(view.pat, view.boxStart, j);
      END;
      VText.MoveInterval(view.match, j, j + len);
      VText.SwitchInterval(view.probe, VTDef.OnOffState.Off);
      VText.SwitchInterval(view.match, VTDef.OnOffState.On);
      VText.MoveInterval(view.inpat, i, i + len);
      VText.SwitchInterval(view.inpat, VTDef.OnOffState.On);

      (* TextPort.Normalize(view.port, j); *)
      VBT.Mark(view.form);
    EXCEPT
    | VTDef.Error (code) => ZeusPanel.ReportError(VTError(code));
    END;
  END PartialMatch;

PROCEDURE PartialMatchClear (view: T) =
  BEGIN
    TRY
      VText.SwitchInterval(view.pat, VTDef.OnOffState.Off);
      VText.SwitchInterval(view.probe, VTDef.OnOffState.Off);
      VText.SwitchInterval(view.match, VTDef.OnOffState.Off);
      VText.SwitchInterval(view.inpat, VTDef.OnOffState.Off);
      VBT.Mark(view.form);
    EXCEPT
    | VTDef.Error (code) => ZeusPanel.ReportError(VTError(code));
    END;
  END PartialMatchClear;

PROCEDURE CompleteMatch (view: T; j: CARDINAL) =
  VAR interval: VText.Interval;
  BEGIN
    TRY
      interval :=
        VText.CreateInterval(TextPort.GetVText(view.port), j, j + 1, CompleteIO);
      VText.SwitchInterval(interval, VTDef.OnOffState.On);
      VText.SwitchInterval(view.inpat, VTDef.OnOffState.Off);
      VText.SwitchInterval(view.match, VTDef.OnOffState.Off);
      VBT.Mark(view.form);
    EXCEPT
    | VTDef.Error (code) => ZeusPanel.ReportError(VTError(code));
    END;
  END CompleteMatch;


PROCEDURE SlideTo (view: T; j: CARDINAL) =
  BEGIN
    TRY
      VText.MoveInterval(view.pat, j, j + view.lenPat);
      VText.SwitchInterval(view.pat, VTDef.OnOffState.On);
      view.boxStart := j;
      view.boxLast := j + view.lenPat;
(*    TextPort.Normalize(view.port, j); *)
      VBT.Mark(view.port);
    EXCEPT
    | VTDef.Error (code) => ZeusPanel.ReportError(VTError(code));
    END;
  END SlideTo;


PROCEDURE New (): View.T =
  <* FATAL Rsrc.NotFound *>
  VAR
    f            : FormsVBT.T;
    p, m, pt, ipt: VText.Interval;
    textedit     : TextEditVBT.T;
  BEGIN
    TRY
      f := NEW(FormsVBT.T).initFromRsrc(
             "stringsearchtextview.fv", ZeusPanel.GetPath());
      textedit := FormsVBT.GetVBT(f, "pattern");
      ipt := VText.CreateInterval(
               TextPort.GetVText(textedit.tp), 0, 0, PartialIO);
      textedit := FormsVBT.GetVBT(f, "text");
      p := VText.CreateInterval(
             TextPort.GetVText(textedit.tp), 0, 0, ProbeIO);
      m := VText.CreateInterval(
             TextPort.GetVText(textedit.tp), 0, 0, PartialIO);
      pt := VText.CreateInterval(
              TextPort.GetVText(textedit.tp), 0, 0, PatternIO);

      RETURN NEW(T, form := f, probe := p, match := m, pat := pt,
                 inpat := ipt, port := textedit.tp).init(f);
    EXCEPT
    | Rd.Failure => ZeusPanel.ReportError("Rd.Failure in SSTextView.New");
    | Thread.Alerted =>
        ZeusPanel.ReportError("Thread.Alerted in SSTextView.New");
    | FormsVBT.Error (msg) => ZeusPanel.ReportError(msg);
    | VTDef.Error (code) => ZeusPanel.ReportError(VTError(code));
    END;
    RETURN (NIL);
  END New;


BEGIN
  ZeusPanel.RegisterView (New, "Text", "StringSearch");
END TextView.
