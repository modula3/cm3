(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Jun 27 14:42:49 PDT 1995 by najork                   *)
(*       Created on Tue Jan 17 16:14:18 PST 1995 by najork                   *)


UNSAFE MODULE WinScrnCursor;

IMPORT Cursor, Point, Rect, ScrnCursor, ScrnPixmap, Text, TrestleComm, 
       TrestleImpl, VBTClass, WinDef, WinGDI, WinNT, WinScreenType, 
       WinScrnPixmap, WinUser;

CONST
  False = 0;
  True  = 1;


TYPE
  T = ScrnCursor.T BRANDED OBJECT
  OVERRIDES
    localize := Localize;
    unload   := Unload;
  END;


(*-----------------------------------------------------------------------------
   The spec in ScrnCursor.i3 states:

       If "cs" is a "ScrnCursor.T", then "cs.id" is an identifier whose
       interpretation depends on the screentype that owns "cs".  The method
       call "cs.localize()" returns a raw cursor equal to the one on which
       "cs" is a handle, and the method call "cs.unload()" causes "cs"
       to become anonymous.  

   However, the X version of "localize" (XScrnCrsr.CursorLocalize) always 
   raises "Failure", and the X version of "unload" (XScrnCrsr.CursorUnregister)
   is a no-op. For now, we do the same ...
-----------------------------------------------------------------------------*)


PROCEDURE Localize (<*UNUSED*> self: T): ScrnCursor.Raw 
    RAISES {ScrnCursor.Failure} =
  BEGIN
    RAISE ScrnCursor.Failure;
  END Localize;


PROCEDURE Unload (<*UNUSED*> self: T) =
  BEGIN
  END Unload;


(*****************************************************************************)

TYPE
  Oracle = ScrnCursor.Oracle BRANDED OBJECT
    st: WinScreenType.T;
  OVERRIDES
    load    := Load;
    list    := List;
    lookup  := Lookup;
    builtIn := BuiltIn;
  END;


PROCEDURE NewOracle (st: WinScreenType.T): ScrnCursor.Oracle =
  BEGIN
    RETURN NEW (Oracle, st := st);
  END NewOracle;


PROCEDURE Load (                      self: Oracle;
                             READONLY c   : ScrnCursor.Raw;
                <* UNUSED *>          nm  : TEXT): ScrnCursor.T =
  VAR
    status  : WinDef.BOOL;
    hcursor : WinDef.HCURSOR;
    iconInfo: WinUser.ICONINFO;
    col1    : WinDef.COLORREF;
    col2    : WinDef.COLORREF;
    col3    : WinDef.COLORREF;
    andRaw  : ScrnPixmap.Raw;
    colRaw  : ScrnPixmap.Raw;
  BEGIN
    IF c.plane1 = NIL OR c.plane2 = NIL OR 
       c.plane1.depth # 1 OR c.plane2.depth # 1 OR 
       Rect.IsEmpty (c.plane1.bounds) OR c.plane1.bounds # c.plane2.bounds THEN
      RETURN ScrnCursor.DontCare;
    END;

    WITH st = self.st DO
      LOCK st.trsl DO
        (* I assume that we have a color screen, and don't deal with
           with the "gray" and "bw" fields of "c.color1", ..., "c.color3" *)

        col1 := WinGDI.RGB (ROUND (255.0 * c.color1.r),
                            ROUND (255.0 * c.color1.g),
                            ROUND (255.0 * c.color1.b));
        col2 := WinGDI.RGB (ROUND (255.0 * c.color2.r),
                            ROUND (255.0 * c.color2.g),
                            ROUND (255.0 * c.color2.b));
        col3 := WinGDI.RGB (ROUND (255.0 * c.color3.r),
                            ROUND (255.0 * c.color3.g),
                            ROUND (255.0 * c.color3.b));

        (* Construct the AND and XOR bitmaps *)
        andRaw := ScrnPixmap.NewRaw (1, c.plane1.bounds);
        colRaw := ScrnPixmap.NewRaw (32, c.plane1.bounds);

        FOR y := c.plane1.bounds.north TO c.plane1.bounds.south - 1 DO
          FOR x := c.plane1.bounds.west TO c.plane1.bounds.east - 1 DO
            WITH pt = Point.T {x, y},
                 p1 = c.plane1.get (pt),
                 p2 = c.plane2.get (pt) DO
              IF    p1 = 0 AND p2 = 0 THEN
                andRaw.set (pt, 1);
                colRaw.set (pt, 0);
              ELSIF p1 = 0 AND p2 = 1 THEN
                andRaw.set (pt, 0);
                colRaw.set (pt, col1);
              ELSIF p1 = 1 AND p2 = 0 THEN
                andRaw.set (pt, 0);
                colRaw.set (pt, col2);
              ELSIF p1 = 1 AND p2 = 1 THEN
                andRaw.set (pt, 0);
                colRaw.set (pt, col3);
              ELSE
                <* ASSERT FALSE *>
              END;
            END;
          END;
        END;

        WITH hbmMask  = WinScrnPixmap.PixmapFromRaw(st, andRaw),
             hbmColor = WinScrnPixmap.PixmapFromRaw(st, colRaw) DO

          iconInfo := WinUser.ICONINFO {
                          fIcon    := False,
                          xHotspot := c.hotspot.h,
                          yHotspot := c.hotspot.v,
                          hbmMask  := hbmMask,
                          hbmColor := hbmColor};
            
          hcursor := WinUser.CreateIconIndirect (ADR (iconInfo));
          <* ASSERT hcursor # NIL *>
          
          status := WinGDI.DeleteObject (hbmMask);
          <* ASSERT status = True *>
          status := WinGDI.DeleteObject (hbmColor);
          <* ASSERT status = True *>

          RETURN NEW (T, id := LOOPHOLE (hcursor, INTEGER));
        END;
      END;
    END;
  END Load;


(*-----------------------------------------------------------------------------
   The spec in ScrnCursor.i3 states:

       The method call "st.cursor.list(pat, maxResults)" returns the names 
       of all cursors owned by "st" that match the pattern "pat".  The list 
       of results may be truncated to length "maxResults". A "*" matches 
       any number of characters and a "?" matches a single character.

   However, the X version (XScrnCrsr.CursorList) always returns NIL.
   Since this seems to be adequate, we do the same ...
-----------------------------------------------------------------------------*)

PROCEDURE List (<* UNUSED *> self      : Oracle;
                <* UNUSED *> pat       : TEXT;
                <* UNUSED *> maxResults: CARDINAL): REF ARRAY OF TEXT =
  BEGIN
    RETURN NIL
  END List;


TYPE 
  NamedCursor = RECORD 
    name: TEXT; 
    crsr: WinNT.LPCTSTR;
  END;

VAR
  cursors := ARRAY [1 .. 14] OF NamedCursor {
                       NamedCursor {"ARROW",       WinUser.IDC_ARROW},
                       NamedCursor {"IBEAM",       WinUser.IDC_IBEAM},
                       NamedCursor {"WAIT",        WinUser.IDC_WAIT},
                       NamedCursor {"CROSS",       WinUser.IDC_CROSS},
                       NamedCursor {"UPARROW",     WinUser.IDC_UPARROW},
                       NamedCursor {"SIZE",        WinUser.IDC_SIZE},
                       NamedCursor {"ICON",        WinUser.IDC_ICON},
                       NamedCursor {"SIZENWSE",    WinUser.IDC_SIZENWSE},
                       NamedCursor {"SIZENESW",    WinUser.IDC_SIZENESW},
                       NamedCursor {"SIZEWE",      WinUser.IDC_SIZEWE},
                       NamedCursor {"SIZENS",      WinUser.IDC_SIZENS},
                       NamedCursor {"SIZEALL",     WinUser.IDC_SIZEALL},
                       NamedCursor {"NO",          WinUser.IDC_NO},
                       NamedCursor {"APPSTARTING", WinUser.IDC_APPSTARTING}};


PROCEDURE Lookup (self: Oracle; name: TEXT): ScrnCursor.T
    RAISES {TrestleComm.Failure} =
  BEGIN
    FOR i := FIRST (cursors) TO LAST (cursors) DO
      IF Text.Equal (name, cursors[i].name) THEN
        LOCK self.st.trsl DO
          RETURN LoadCursor (cursors[i].crsr);
        END;
      END;
    END;
    RETURN NIL;
  END Lookup;


PROCEDURE BuiltIn (self: Oracle; cs: Cursor.Predefined): ScrnCursor.T =
  BEGIN
    TRY
      LOCK self.st.trsl DO
        CASE cs OF
        | Cursor.DontCare.cs =>
          RETURN ScrnCursor.DontCare;
        | Cursor.TextPointer.cs =>
          RETURN LoadCursor (WinUser.IDC_ARROW);
        | Cursor.NotReady.cs =>
          RETURN LoadCursor (WinUser.IDC_WAIT);
        END;
      END;
    EXCEPT 
      TrestleComm.Failure =>  RETURN ScrnCursor.DontCare;
    END;
  END BuiltIn;


PROCEDURE LoadCursor (lpszCursor: WinNT.LPCTSTR): ScrnCursor.T 
    RAISES {TrestleComm.Failure} =
  BEGIN
    WITH hcursor = WinUser.LoadCursor (NIL, lpszCursor) DO
      IF hcursor # NIL THEN
        RETURN NEW (T, id := LOOPHOLE(hcursor, INTEGER));
      ELSE
        RAISE TrestleComm.Failure;
      END;
    END;
  END LoadCursor;


PROCEDURE SetCursor (cs: ScrnCursor.T) =
  VAR
    hcursor: WinDef.HCURSOR;
  BEGIN
    (* In xvbt, a value of "ScrnCursor.DontCare" means that the cursor is 
       "undefined", that is, its appearance is the same as that of the parent 
       window (which is always the root window, since xvbt does not build up 
       trees of X windows.
             
       For simplicity, I assume that the cursor of the desktop window 
       is always "IDC_ARROW". So, undefining the cursor allociated with
       a "Child" means setting it to "IDC_ARROW". *)

    IF cs = ScrnCursor.DontCare THEN
      hcursor := WinUser.LoadCursor (NIL, WinUser.IDC_ARROW);
    ELSE
      hcursor := LOOPHOLE (cs.id, ADDRESS);
    END;
    EVAL WinUser.SetCursor (hcursor);
  END SetCursor;


BEGIN
END WinScrnCursor.
