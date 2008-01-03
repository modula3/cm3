(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Nov  4 13:12:04 PST 1994 by kalsow    *)

MODULE SilObject;

IMPORT Rect;
IMPORT SilRd, SilWr, SilFont, SilWindow;
  
REVEAL
  T = T_ BRANDED "SilObject" OBJECT  OVERRIDES
    select         := Select;
    deleteSelected := DeleteSelected;
    takeChar       := TakeChar;
    setFont        := SetFont;
    setWidth       := SetWidth;
  END;
  
PROCEDURE WriteBBox (t: T;  wr: SilWr.T) =
  BEGIN
    wr.putInt (t.bbox.west);
    wr.putInt (t.bbox.east);
    wr.putInt (t.bbox.north);
    wr.putInt (t.bbox.south);
  END WriteBBox;
  
PROCEDURE ReadBBox (t: T;  rd: SilRd.T) =
  BEGIN
    <*ASSERT rd.n_ints >= 4*>
    t.bbox.west  := rd.ints[0];
    t.bbox.east  := rd.ints[1];
    t.bbox.north := rd.ints[2];
    t.bbox.south := rd.ints[3];
  END ReadBBox;

PROCEDURE Select (t          : T;
        READONLY  clip       : Rect.T;
                  pointMode  : BOOLEAN;
                  dropOthers : BOOLEAN;
       VAR(*OUT*) bbox       : Rect.T) : BOOLEAN =
  (*This is the basic method for selection, which may be
    overridden.  If pointMode is TRUE, THEN a 'hit' means
    the point is within the bounding box for the object.  If
    pointMode is FALSE, THEN a hit occurs only the bounding box
    for the object falls entirely within 'clip'.
  
    If a hit occurs, the object is marked selected, and is redrawn in red.
    If no hit occurs and dropOthers is TRUE, the object is DeSelected.
  *)
  VAR hit := FALSE;
  BEGIN
    IF t.state < Visible THEN RETURN FALSE; END;

    IF pointMode
      THEN hit := Rect.Member (Rect.NorthWest (clip), t.bbox);
      ELSE hit := Rect.Subset (t.bbox, clip);
    END;
  
    IF hit THEN
      t.state := Selected;
      bbox := t.bbox;
      RETURN TRUE
    ELSIF dropOthers AND (t.state = Selected) THEN
      t.state := Visible;
      bbox := t.bbox;
      RETURN TRUE
    ELSE
      RETURN FALSE;
    END;
  END Select;
  
PROCEDURE DeleteSelected (t: T;  VAR(*OUT*) bbox: Rect.T): BOOLEAN =
  (*delete selected objects*)
  BEGIN
    IF t.state < Visible THEN
      DEC (t.state) (*for undelete*)
    ELSIF t.state = Selected THEN
      t.state := Selected-1; (*deleted*)
      bbox := t.bbox;
      RETURN TRUE;
    END;
    RETURN FALSE;
  END DeleteSelected;

PROCEDURE TakeChar(<*UNUSED*> t    : T;
                   <*UNUSED*> ch   : CHAR;
                  <*UNUSED*>  w    : SilWindow.T;
        <*UNUSED*> VAR(*OUT*) bbox : Rect.T): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END TakeChar;
  
PROCEDURE SetFont (<*UNUSED*> t   : T;
                   <*UNUSED*> f   : SilFont.T;
                   <*UNUSED*> sel : BOOLEAN;
                   <*UNUSED*> w   : SilWindow.T): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END SetFont;
  
PROCEDURE SetWidth (<*UNUSED*> t   : T;
                    <*UNUSED*> wid : INTEGER;
                    <*UNUSED*> sel : BOOLEAN): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END SetWidth;

BEGIN
END SilObject.
