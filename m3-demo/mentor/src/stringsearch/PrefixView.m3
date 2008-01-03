(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Fri Jan  6 00:00:41 PST 1995 by najork   *)
(*      modified on Thu Feb 11 13:50:34 PST 1993 by steveg   *)
(*      modified on Wed Aug  5 12:55:07 PDT 1992 by broder   *)
(*      modified on Tue Aug  4 20:22:22 PDT 1992 by guarino  *)
(*      modified on Tue Jul 21 06:43:42 PDT 1992 by mhb      *)

MODULE PrefixView;

IMPORT StringSearchViewClass, View, ZeusPanel, Grid, Text, MG, MGV, Font,
       ScaleFilter, ViewsBase, PaintOp, ColorName, VBT, Filter, Thread;


CONST
  RectSize = 20.0;
  InitSize = 12;

TYPE
  T = StringSearchViewClass.T OBJECT
        grid: Grid.V;
        pat : TEXT;
        m   : CARDINAL;         (* Length of pattern *)
      OVERRIDES
        oeKMPSetup := KMPSetup;
        oeAddEdge  := AddEdge;
      END;

PROCEDURE Color(color: TEXT): PaintOp.T =
  <* FATAL ColorName.NotFound *>
  VAR rgb := ColorName.ToRGB(color);
  BEGIN
    RETURN PaintOp.FromRGB(rgb.r, rgb.g, rgb.b);
  END Color;

VAR 
  PartialCS := PaintOp.MakeColorScheme(
                 fg := PaintOp.Fg,
                 bg := Color(ViewsBase.PartialC));

PROCEDURE KMPSetup (self: T; p: TEXT) RAISES {Thread.Alerted} =
  VAR
    m := Text.Length(p);
    g := NEW(Grid.V).init(m, m, RectSize, RectSize);
  BEGIN
    LOCK VBT.mu DO
      WITH scale = NEW(ScaleFilter.T).init(g) DO
        ScaleFilter.AutoScale(scale);
        EVAL Filter.Replace(self, scale);
      END;
    END;
    self.pat := p;
    self.m := m;
    self.grid := g;
    LOCK g.mu DO
      FOR i := 0 TO m - 1 DO
        FOR j := 0 TO m - 1 DO g.a[i, j].setVisible(g, 0.0); END;
      END;
      FOR i := 0 TO m - 1 DO
        FOR j := 0 TO i DO
          g.a[i, j].setVisible(g, 1.0);
          g.a[i, j].setLabel(g, Text.Sub(p, j, 1));
          g.a[i, j].setFont(
            g, Font.FromName(
                 ARRAY OF TEXT{"-*-courier-bold-r-*-*-*-180-*-*-*-*-*-*"}));
        END;
      END;
    END;
    MGV.Animation(g);
  END KMPSetup;

PROCEDURE AddEdge (self: T; f, t: CARDINAL) RAISES {Thread.Alerted} =
  BEGIN
    FOR i := 1 TO t DO
      self.grid.a[f - 1, f - i].setColor(self.grid, PartialCS)
    END;
    MGV.Animation(self.grid);
  END AddEdge;

PROCEDURE New (): View.T =
  VAR g := NEW(Grid.V).init(InitSize, InitSize, RectSize, RectSize);
  BEGIN
    FOR i := 0 TO InitSize - 1 DO
      FOR j := 0 TO InitSize - 1 DO g.a[i, j].setVisible(g, 0.0); END;
    END;
    WITH scale = NEW(ScaleFilter.T).init(g) DO
      ScaleFilter.AutoScale(scale);
      RETURN NEW(T, grid := g).init(scale);
    END;
  END New;

BEGIN
  ZeusPanel.RegisterView(New, "Prefix", "StringSearch");
END PrefixView.

