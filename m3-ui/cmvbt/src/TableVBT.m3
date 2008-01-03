
(* Copyright 1996-2000 Critical Mass, Inc. All Rights Reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE TableVBT;

IMPORT GridSplit, VBT, TextVBT;
IMPORT HVSplit, Axis, Font;
IMPORT Pixmap, ShadowedFeedbackVBT, PaintOp, Shadow, SwitchVBT;
IMPORT Rect, Point, Cursor;
IMPORT Split, ShadowedBarVBT, HighlightVBT, VBTClass;

REVEAL 
  Private = HVSplit.T BRANDED OBJECT END;

  T = Public BRANDED OBJECT
    hl: HighlightVBT.T;
    grid: Content;
    hdg: Heading;
  OVERRIDES
    init := Init;
    contents := Contents;
    heading := DefaultHeading;
    content := DefaultContent;
    insert  := DefaultInsert;
    delete  := DefaultDelete;
    numrows := NumRows;
(*    reshape := Reshape; *)
  END;

TYPE
  Heading = GridSplit.T BRANDED OBJECT
    grid: GridSplit.T;
    state: State;
    which: INTEGER := -1;
    lmp: Point.T; (* last mouse position *)
    resizing: BOOLEAN := FALSE;
  OVERRIDES
    mouse    := Mouse;
    position := Position;
  END;

  Content = GridSplit.T OBJECT
    parent: T;
    loc: GridSplit.Location;
  OVERRIDES
    mouse := ContentMouse;
    reshape := ContentReshape;
  END;

  State = {None, RowResize, Select};

PROCEDURE DefaultHeading (<*UNUSED*>v: T; 
                           <*UNUSED*>r: CARDINAL;
                           <*UNUSED*>READONLY cd: VBT.MouseRec) =
  BEGIN
    (* Empty body *)
  END DefaultHeading;

PROCEDURE DefaultContent (<*UNUSED*>v: T; 
                           <*UNUSED*>r: CARDINAL;
                           <*UNUSED*>READONLY cd: VBT.MouseRec) =
  BEGIN
    (* Empty body *)
  END DefaultContent;

VAR
  litegray := PaintOp.FromRGB (0.75, 0.75, 0.75);
  darkgray := PaintOp.FromRGB (0.25, 0.25, 0.25);
  litemedigray := PaintOp.FromRGB(0.675, 0.675, 0.675);
  white    := PaintOp.FromRGB (1.00, 1.00, 1.00);
  black    := PaintOp.FromRGB (0.00, 0.00, 0.00);

TYPE
  Header = SwitchVBT.T OBJECT
    which: CARDINAL;
    owner: T;
  OVERRIDES
    callback := Callback;    
  END;


PROCEDURE Callback (self: Header; READONLY cd: VBT.MouseRec) =
BEGIN
  self.owner.heading(self.which, cd);
END Callback;

PROCEDURE Contents(v: T): GridSplit.T = 
  BEGIN
    RETURN v.grid;
  END Contents;

PROCEDURE ContentReshape(v: Content; READONLY cd: VBT.ReshapeRec) RAISES {} = 
  VAR
    r: Rect.T;
  BEGIN
    IF v.loc.row >= 0 AND v.loc.pl = GridSplit.Placement.InsideCell THEN 
      r := GridSplit.GetCoord (v, top := v.loc.row, bottom := v.loc.row);
      HighlightVBT.SetRect(v, r, inset := LAST(CARDINAL));
    END;
    GridSplit.T.reshape(v, cd);
  END ContentReshape;

PROCEDURE Position (v: Heading; READONLY cd: VBT.PositionRec) =  
  VAR
    loc: GridSplit.Location;
  BEGIN
    IF v.state = State.None THEN
      loc := GridSplit.Locate (v, cd.cp.pt);
      VBT.SetCage (v, VBT.CageFromPosition(cd.cp));
      IF loc.pl = GridSplit.Placement.VerMargin THEN
        VBT.SetCursor (v, hresize); 
      ELSE
        VBT.SetCursor (v, Cursor.DontCare);
      END;
    ELSIF cd.cp.gone THEN
      VBT.SetCursor (v, hresize);
      VBT.SetCage (v, VBT.GoneCage); 
    ELSE
      VBT.SetCage (v, VBT.CageFromPosition(cd.cp));
    END;
  END Position;

PROCEDURE Mouse(v: Heading; READONLY cd: VBT.MouseRec) RAISES {} =
  VAR
    loc := GridSplit.Locate (v, cd.cp.pt);
  BEGIN
    CASE cd.clickType OF 
    | VBT.ClickType.FirstDown =>
      VBT.SetCage (v, VBT.CageFromPosition(cd.cp));
      IF loc.row > 0 THEN
        v.which := loc.row;
      ELSIF loc.pl = GridSplit.Placement.VerMargin AND loc.row = 0 THEN
        v.state := State.RowResize;
        v.which := loc.col;
        v.lmp := cd.cp.pt;
        VBT.SetCursor (v, hresize);
      ELSE
        GridSplit.T.mouse (v, cd);
      END;
    | VBT.ClickType.LastUp =>
      VBT.SetCursor (v, Cursor.DontCare); 
      IF loc.col > -1 AND loc.row = 0 AND v.state = State.RowResize THEN
        VAR
          delta := cd.cp.pt.h - v.lmp.h;
          which := v.which;
          newsize: INTEGER;
        BEGIN
          IF which < 0 THEN 
            which := 0;
            delta := -delta;
          END;

          newsize := GridSplit.GetColWidth(v, which) + delta; 

          IF newsize > 0 THEN
            GridSplit.SetColWidth (v, which, newsize);
            GridSplit.SetColWidth (v.grid, which, newsize);
          END;
        END;
      ELSE 
        GridSplit.T.mouse(v, cd);
      END;
      v.state := State.None;
    ELSE
      v.state := State.None;
    END;
  END Mouse;

PROCEDURE ContentMouse(v: Content; READONLY cd: VBT.MouseRec) RAISES {} =
  VAR
(*    first: BOOLEAN := TRUE; *)
    r: Rect.T;
  BEGIN
    v.loc := GridSplit.Locate (v, cd.cp.pt);
    CASE cd.clickType OF 
    | VBT.ClickType.FirstDown =>
      IF v.loc.row >= 0 AND v.loc.pl = GridSplit.Placement.InsideCell THEN 
        r := GridSplit.GetCoord (v, top := v.loc.row, bottom := v.loc.row);
        HighlightVBT.Invert (v, r, inset := LAST(CARDINAL));
        v.parent.content(v.loc.row, cd);
      END;
    ELSE
    END;
    GridSplit.T.mouse(v, cd);
  END ContentMouse;


PROCEDURE Init(v: T;
               READONLY names: ARRAY OF TEXT;
               READONLY sizes: ARRAY OF CARDINAL;
               font: Font.T;
               nrows: CARDINAL := 0;
               rowsize, colsize: CARDINAL;
               op: PaintOp.T := PaintOp.BgFg;
               txt: Pixmap.T := Pixmap.Solid): T =
VAR
  ncols: CARDINAL := NUMBER(names);

  shadow := Shadow.New(size := 0.5, bg := PaintOp.Bg,
                                    fg := op,
                                    light := litegray,
                                    dark  := darkgray);

  cq := PaintOp.MakeColorQuad (litemedigray, black);

BEGIN

  <* ASSERT NUMBER(names) = NUMBER(sizes) *>

  v.grid := NEW(Content, parent := v).init (ncols := ncols,
                                       nrows := nrows,
                                       rowsize := rowsize,
                                       colsize := colsize,
                                       margin := 1,
                                       bg := darkgray,
                                       fg := white,
                                       txt := txt);

  (* We should use a better method for setting the size of col headings. *)

  v.hdg := NEW(Heading, grid := v.grid).init (ncols := ncols,
                                      nrows := 1,
                                      rowsize := 30,
                                      colsize := colsize,
                                      margin := 1,
                                      bg := darkgray,
                                      fg := white,
                                      txt := txt);


  EVAL HVSplit.T.init(v, Axis.T.Ver);
  Split.AddChild (v, v.hdg);
  Split.AddChild (v, NEW(ShadowedBarVBT.T)
                        .init(Axis.T.Ver, shadow,
                         Shadow.Style.Chiseled));
  v.hl := HighlightVBT.New(v.grid);
  Split.AddChild (v, v.hl);

  FOR i := FIRST(names) TO LAST(names) DO
    WITH inside = NEW(ShadowedFeedbackVBT.T)
                           .init(TextVBT.New(names[i],
                                 fnt := font, bgFg := cq,
                                 halign := 0.0),
                                 shadow := shadow),
         btn = NEW(Header, 
                   owner := v,
                   which := i).init (inside) DO

       GridSplit.Put (v.hdg, 0, i, btn);
       GridSplit.SetColWidth (v.hdg, i, sizes[i]);
       GridSplit.SetColWidth (v.grid,    i, sizes[i]);
    END;
  END;

  FOR row := 0 TO nrows - 1 DO
    FOR col := 0 TO ncols - 1 DO
      GridSplit.Put (v.grid, row, col, TextVBT.New(""));
    END;
  END;

  RETURN v;
END Init;

PROCEDURE NumRows(v: T): CARDINAL = 
  BEGIN
    RETURN GridSplit.NumRows(v.grid);
  END NumRows;

PROCEDURE DefaultInsert(v: T; row: CARDINAL; READONLY data: ARRAY OF VBT.T) =  
  VAR
    numrows := GridSplit.NumRows(v.grid);
  BEGIN
    IF row > numrows THEN
      row := numrows;
    END;
    GridSplit.InsertRow(v.grid, row, data);
  END DefaultInsert;

PROCEDURE DefaultDelete(v: T; row: CARDINAL; VAR data: ARRAY OF VBT.T) =
  BEGIN
    GridSplit.RemoveRow(v.grid, row, data);
  END DefaultDelete;

VAR
  hresize :=  Cursor.FromName (ARRAY OF TEXT {"XC_sb_h_double_arrow"});

<*UNUSED*> VAR  
  vresize :=  Cursor.FromName (ARRAY OF TEXT {"XC_sb_v_double_arrow"});

BEGIN
END TableVBT.

