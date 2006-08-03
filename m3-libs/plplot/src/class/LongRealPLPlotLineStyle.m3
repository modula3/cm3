MODULE LongRealPLPlotLineStyle;

IMPORT LongRealPLPlot AS PL;

REVEAL
  Default =
    DefaultPublic BRANDED OBJECT OVERRIDES apply := DefaultApply; END;
  Custom = CustomPublic BRANDED OBJECT OVERRIDES apply := CustomApply; END;

PROCEDURE DefaultApply (SELF: Default; ) =
  BEGIN
    PL.SetLineStyle(SELF.style);
  END DefaultApply;

PROCEDURE CustomApply (SELF: Custom; ) =
  BEGIN
    PL.SetCustomLineStyle(SELF.mark^, SELF.space^);
  END CustomApply;

BEGIN
  Continuous := NEW(Default, style := PL.LineStyle.Continuous);
END LongRealPLPlotLineStyle.
