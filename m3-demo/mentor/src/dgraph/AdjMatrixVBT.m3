(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE AdjMatrixVBT;

IMPORT PaintOp, Text, GridMJJ, MGPublic, MG;

REVEAL
  T = Public BRANDED OBJECT
    n: INTEGER;
    do_redisplay := TRUE;
  OVERRIDES
    init := Init;
    getColor := GetColor;
    setColor := SetColor;
    getText := GetText;
    setText := SetText;
    getHighlight := GetHighlight;
    setHighlight := SetHighlight;
    getRowLabel := GetRowLabel;
    setRowLabel := SetRowLabel;
    getColumnLabel := GetColumnLabel;
    setColumnLabel := SetColumnLabel;
    display := Display;
  END;

  Prefix = GridMJJ.V BRANDED OBJECT END;

PROCEDURE Init(self: T; n: INTEGER; <*UNUSED*> grid : BOOLEAN): T =
  VAR 
    label: TEXT;
  BEGIN
    self.n := n;
    EVAL GridMJJ.V.init(self, n+1, n+1, 20.0, 20.0, 4.0, 4.0);

    (* Add an extra column and row for the label *)
    INC(n);

    FOR i := 0 TO n-1 DO
      FOR j := 0 TO n-1 DO
        label := "";
        IF i = 0 OR j = 0 THEN
          IF i = 0 AND j = 0 THEN (* nothing *)
          ELSIF i = 0 THEN label := Text.FromChar(VAL(ORD('A') + j-1, CHAR));
          ELSE label := Text.FromChar(VAL(ORD('A') + i-1, CHAR));
          END;
        END; (* if *)

          IF (i = 0 OR j = 0) THEN
            (* label vertex *)
            IF i = 0 AND j = 0 THEN (* nothing *)
            ELSIF i = 0 THEN MGPublic.SetLabel(self.a[0, j], self, label)
            ELSE MGPublic.SetLabel(self.a[i, 0], self, label);
            END;
          ELSE
          END;

      END; (* for *)
    END; (* for *)

    RETURN self;
  END Init;


PROCEDURE Display(<*UNUSED*> t: T; <*UNUSED*> state: BOOLEAN) =
  BEGIN
  END Display;

PROCEDURE GetColor(self: T; n, m: INTEGER): PaintOp.T =
  BEGIN
    LOCK self.mu DO
      RETURN self.a[n+1, m+1].color.bg;
    END; (* lock *)
  END GetColor;

PROCEDURE SetColor(self: T; n, m: INTEGER; color: PaintOp.T) RAISES {}=
  BEGIN
    MGPublic.SetColor(self.a[n+1, m+1], self,
                      PaintOp.MakeColorScheme(color, PaintOp.Fg));
  END SetColor;

PROCEDURE GetText(self: T; n, m: INTEGER): TEXT RAISES {}=
  BEGIN
    LOCK self.mu DO
      RETURN self.a[n+1, m+1].label;
    END; (* lock *)
  END GetText;

PROCEDURE SetText(self: T; n, m: INTEGER; t: TEXT) RAISES {}=
  BEGIN
    MGPublic.SetLabel(self.a[n+1, m+1], self, t);
  END SetText;

PROCEDURE GetHighlight(self: T; n, m: INTEGER): PaintOp.T RAISES {}=
  BEGIN
    LOCK self.mu DO
      RETURN self.a[n+1, m+1].color.fg;
    END; (* lock *)
  END GetHighlight;

PROCEDURE SetHighlight(self: T; n, m: INTEGER; color: PaintOp.T) RAISES {}=
  VAR bgColor: PaintOp.T;
  BEGIN
    WITH rect = self.a[n+1, m+1] DO 
      LOCK self.mu DO bgColor := rect.color.bg; END;
      MGPublic.SetColor(rect, self,
                        PaintOp.MakeColorScheme(bgColor, color));
    END;
  END SetHighlight;

PROCEDURE SetRowLabel(self: T; n: INTEGER; color: PaintOp.T)=
  BEGIN
    MGPublic.SetColor(self.a[n+1, 0], self,
                      PaintOp.MakeColorScheme(color, PaintOp.Fg));
  END SetRowLabel;

PROCEDURE GetRowLabel(self: T; n: INTEGER): PaintOp.T =
  BEGIN
    LOCK self.mu DO
      RETURN self.a[n+1, 0].color.bg;
    END; (* lock *)
  END GetRowLabel;

PROCEDURE SetColumnLabel(self: T; n: INTEGER; color: PaintOp.T) RAISES {}=
  BEGIN
    MGPublic.SetColor(self.a[0, n+1], self,
                      PaintOp.MakeColorScheme(color, PaintOp.Fg));
  END SetColumnLabel;

PROCEDURE GetColumnLabel(self: T; n: INTEGER): PaintOp.T =
  BEGIN
    LOCK self.mu DO
      RETURN self.a[0, n+1].color.bg;
    END; (* lock *)
  END GetColumnLabel;

BEGIN

END AdjMatrixVBT.

