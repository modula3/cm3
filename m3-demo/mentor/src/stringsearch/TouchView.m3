(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Tue Jan 31 14:53:44 PST 1995 by kalsow *)
(*      modified on Fri Jan  6 00:05:29 PST 1995 by najork *)
(*      modified on Fri Apr 23 15:49:11 PDT 1993 by mhb    *)
(*      modified on Wed Jan  6 16:25:13 PST 1993 by steveg *)
(*      modified on Tue Aug  4 14:09:04 PDT 1992 by guarino *)

MODULE TouchView;
IMPORT ColorName, FormsVBT, PaintOp, Rd, Rsrc,
       StringSearchViewClass, TextEditVBT, TextPort,
       Thread, VBT, View, VTDef, VText, ZeusPanel;


TYPE
  T = StringSearchViewClass.T OBJECT
        form : FormsVBT.T;
        port : TextPort.T;
        interval: VText.Interval := NIL;
        l, r: CARDINAL := 0;
      OVERRIDES
        oeSetup         := Setup;
        oeProbe         := Probe;
      END;

VAR
  Black : PaintOp.T;
  Yellow : PaintOp.T;
  present: VText.IntervalOptions;


PROCEDURE VTError(code: VTDef.ErrorCode):TEXT =
BEGIN
  CASE code OF
  | VTDef.ErrorCode.IsNil => RETURN("NIL vtext");
  | VTDef.ErrorCode.IllegalIndex => RETURN("Illegal index ");
  | VTDef.ErrorCode.IllegalRegion => RETURN("Illegal region");
  | VTDef.ErrorCode.IllegalCoord => RETURN("Illegal coordinate");
  | VTDef.ErrorCode.IllegalDividers => RETURN("Illegal dividers");
  | VTDef.ErrorCode.IllegalFont => RETURN("Illegal font");
  | VTDef.ErrorCode.Closed => RETURN("vtext already closed");
  ELSE RETURN("unknown VTDef.ErrorCode")
  END;
END VTError;

PROCEDURE MakeHighlight (fg, bg: PaintOp.T): VText.IntervalOptions =
  BEGIN
    WITH cs = PaintOp.MakeColorScheme (fg := fg, bg:= bg) DO
      RETURN VText.MakeIntervalOptions (
               VText.IntervalStyle.HighlightStyle, cs, cs, cs.bg)
    END
  END MakeHighlight;

PROCEDURE Setup (view: T; pattern, target: TEXT) =
  BEGIN
    LOCK VBT.mu DO
      TRY
        FormsVBT.PutText(view.form, "text", target);
        FormsVBT.PutText(view.form, "pattern", pattern);
        FormsVBT.PutInteger(view.form, "probeCount", 0);
        view.interval := NIL;
        VBT.Mark(view.port);
      EXCEPT
      | FormsVBT.Error (msg) => ZeusPanel.ReportError(msg);
      | FormsVBT.Unimplemented =>
          ZeusPanel.ReportError("FormsVBT.Unimplemented in MGRdView.Setup");
      END;
    END;
  END Setup;

PROCEDURE Probe (view: T; 
    <* UNUSED *> i:    CARDINAL;
                 n:    CARDINAL) =
  BEGIN
    TRY
      FormsVBT.PutInteger(view.form, "probeCount",
                          FormsVBT.GetInteger(view.form, "probeCount") + 1);
     (* this scheme is not foolproof for avoiding overlapping intervals *)
      IF view.interval # NIL AND n = view.l - 1 THEN
        VText.MoveInterval(view.interval, n, view.r);
        DEC(view.l);
      ELSIF view.interval # NIL AND n = view.r THEN
        VText.MoveInterval(view.interval, view.l, n + 1);
        INC(view.r);
      ELSIF view.interval = NIL OR n < view.l OR n > view.r THEN
        view.interval :=
          VText.CreateInterval(TextPort.GetVText(view.port), n, n + 1, present);
        VText.SwitchInterval(view.interval, VTDef.OnOffState.On);
        view.l := n;
        view.r := n + 1;
      END;
      VBT.Mark(view.form);
    EXCEPT
    | FormsVBT.Error (msg) => ZeusPanel.ReportError(msg);
    | FormsVBT.Unimplemented =>
        ZeusPanel.ReportError("FormsVBT.Unimplemented in MGRdView.Probe");
    | VTDef.Error (code) => ZeusPanel.ReportError(VTError(code));
    END;
  END Probe;

PROCEDURE New (): View.T =
  <* FATAL Rsrc.NotFound *>
  VAR
    f       : FormsVBT.T;
    textedit: TextEditVBT.T;
  BEGIN
    TRY
      f := NEW(FormsVBT.T).initFromRsrc(
             "stringsearchtextview.fv", ZeusPanel.GetPath());
      textedit := FormsVBT.GetVBT(f, "text");
      RETURN NEW(T, form := f, port := textedit.tp).init(f);
    EXCEPT
    | Rd.Failure =>
        ZeusPanel.ReportError("Rd.Failure in MGRdView.New");
    | Thread.Alerted =>
        ZeusPanel.ReportError("Thread.Alerted in MGRdView.New");
    | FormsVBT.Error (msg) => ZeusPanel.ReportError(msg);
    END;
    RETURN (NIL);
  END New;

<* FATAL ColorName.NotFound *>
BEGIN
  WITH rgb = ColorName.ToRGB("Black") DO
    Black     := PaintOp.FromRGB(rgb.r, rgb.g, rgb.b);
  END;
  WITH rgb = ColorName.ToRGB("Yellow") DO
    Yellow     := PaintOp.FromRGB(rgb.r, rgb.g, rgb.b);
  END;
  present := MakeHighlight(Black, Yellow);

  ZeusPanel.RegisterView (New, "Touched", "StringSearch");
END TouchView.
