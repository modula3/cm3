(* Copyright (C) 1992-1993, Digital Equipment Corporation                    *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Mar 21 16:29:20 PST 1993 by meehan *)
(*      modified on Tue Jun 16 13:12:40 PDT 1992 by muller *)
(*      modified on Thu Mar 19 11:22:13 PST 1992 by jdd    *)
(*      modified on Tue May 15 17:25:24 PDT 1990 by mcjones *)

(* VTPounce contains the "Pounce" function, which maps a point in the text
   image into a position in the text. A selection mode is given: Char, Word,
   Line, Paragraph, or All. Pounce returns a pair of indices defining the
   matched item, an indication of which index is closer in the image, and a
   cage around the coordinates.

   A number of performance enhancements could be made to this code if it were
   found necessary. *)

MODULE VTPounce;

IMPORT ISOChar, Point, Rd, Rect, Thread;
IMPORT VTDef, VTVirtual, VTRd, VTBase;

TYPE
  VirtualStart = VTDef.VirtualStart;
  WordCode = VTDef.WordCode;

(* "FarWest", "FarEast", "FarNorth", and "FarSouth" are coordinates
   adequately "far" in the given direction. *)
CONST
  FarWest =  -1024;
  FarEast = 1024 + 1024;
  FarNorth =  -1024;
  FarSouth = 1024 + 1024;

PROCEDURE Locate (              view  : View;
                                p     : Point.T;
                  VAR (* OUT *) iL, iR: I;
                  VAR (* OUT *) lineNo: LineNo;
                  VAR (* OUT *) c     : CHAR     )
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR w0, w: Pixels;
  BEGIN
    IF view.virtual.dirty THEN VTVirtual.UpdateView (view) END;
    (* take care of empty view first *)
    lineNo := 0;
    c := '\000';
    IF MIN (view.virtual.line [view.virtual.lines].virtualLine.from,
            view.vt.length) = view.virtual.start.at THEN
      iL := view.virtual.start.at;
      iR := view.virtual.start.at;
      RETURN
    END;
    (* find the character pointed to, if any; iL and iR will bound it *)
    IF p.v < view.rect.text.north THEN
      p.v := view.rect.text.north
    ELSIF p.v >= view.rect.text.south THEN
      p.v := view.rect.text.south - 1
    END;
    IF p.h < view.rect.text.west THEN
      p.h := view.rect.text.west
    ELSIF p.h >= view.rect.text.east THEN
      p.h := view.rect.text.east - 1
    END;
    lineNo := (p.v - view.rect.text.north) DIV view.lineSpacing;
    IF lineNo >= view.virtual.lines THEN
      (* below the text; empty selection *)
      iL := MIN (view.virtual.line [view.virtual.lines].virtualLine.from,
                 view.vt.length);
      iR := iL;
      (* iL := MAX(iL - 1, virtual.start.at); (* experiment *) *)
      (* lineNo := virtual.lines - 1; (* experiment *) *)
    ELSE
      iL := MIN (view.virtual.line [lineNo].virtualLine.from, view.vt.length);
      w := p.h - view.rect.text.west;
      iR := MIN (view.virtual.line [lineNo].virtualLine.to, view.vt.length);
      IF iL < iR THEN
        (* non-empty line, so potentially non-empty selection *)
        <* ASSERT iL <= view.vt.length *>
        VTRd.InitReaderIx (view.vt, iL);
        w0 := 0;
        LOOP
          IF iL = iR THEN EXIT END;
          c := Rd.GetChar (view.vt.rd);
          CASE c OF
          | '\n' => iR := iL + 1; EXIT
          | '\t' =>
              IF '\t' IN view.vScreenFont.vScreenFont.vFont.vFont.printable THEN
                w0 := w0 + view.vScreenFont.vScreenFont.width [' ']
                        + view.vScreenFont.vScreenFont.width ['\t'] - 1;
                w0 := w0 - w0 MOD view.vScreenFont.vScreenFont.width ['\t']
              ELSE
                w0 := w0 + view.vScreenFont.vScreenFont.width [c]
              END
          ELSE
            w0 := w0 + view.vScreenFont.vScreenFont.width [c]
          END;
          IF w0 > w THEN iR := iL + 1; EXIT END;
          iL := iL + 1
        END;
        (* ELSE iL := MAX(iL - 1, virtual.start.at); (* experiment *) *)
      END
    END
  END Locate;


PROCEDURE Extend (                view  : View;
                  VAR (* INOUT *) iL, iR: I;
                                  lineNo: LineNo;
                                  c     : CHAR;
                                  mode  : SelectionMode)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR code: WordCode;
  BEGIN
    IF iL = iR THEN RETURN END;
    IF view.virtual.dirty THEN VTVirtual.UpdateView (view) END;
    CASE mode OF
    | SelectionMode.CharSelection =>
      (* done *)
    | SelectionMode.WordSelection =>
        (* two cases *)
        code := wordCodes [c];
        CASE code OF
        | WordCode.Special =>
          (* if the cursor is at a special character, done *)
        | WordCode.WhiteSpace, WordCode.AlphaNumeric =>
            (* if the cursor is at a white-space character or an alphanumeric
               character, extend selection to match the largest enclosing set
               of such. *)
            (* get a reverse reader *)
            VTRd.InitReaderIx (view.vt, iL);
            VTRd.Rev (view.vt);
            (* read backwards until we find a different character *)
            WHILE (iL > 0) AND (wordCodes [Rd.GetChar (view.vt.rrd)] = code) DO
              iL := iL - 1
            END;
            (* read forward *)
            <* ASSERT iR <= view.vt.length *>
            Rd.Seek (view.vt.rd, iR);
            (* read forward until we find a different character *)
            WHILE (iR < view.vt.length)
                    AND (wordCodes [Rd.GetChar (view.vt.rd)] = code) DO
              iR := iR + 1
            END;
        END
    | SelectionMode.LineSelection =>
        (* easy *)
        iL := view.virtual.line [lineNo].virtualLine.from;
        iR := MIN (view.virtual.line [lineNo].virtualLine.to, view.vt.length)
    | SelectionMode.ParagraphSelection =>
        (* Get a reverse reader *)
        <* ASSERT iL <= view.vt.length *>
        VTRd.InitReaderIx (view.vt, iL);
        VTRd.Rev (view.vt);
        (* read until we see two new-lines *)
        LOOP
          IF iL = 0 THEN EXIT END;
          c := Rd.GetChar (view.vt.rrd);
          IF c = '\n' THEN
            IF iL = 1 THEN EXIT END;
            c := Rd.GetChar (view.vt.rrd);
            IF c = '\n' THEN EXIT ELSE iL := iL - 2 END
          ELSE
            iL := iL - 1
          END
        END;
        (* read forward *)
        iR := MAX (iR - 2, iL);
        <* ASSERT iR <= view.vt.length *>
        Rd.Seek (view.vt.rd, iR);
        (* read until we see two new-lines *)
        LOOP
          IF iR = view.vt.length THEN EXIT END;
          c := Rd.GetChar (view.vt.rd);
          iR := iR + 1;
          IF c = '\n' THEN
            IF iR = view.vt.length THEN EXIT END;
            c := Rd.GetChar (view.vt.rd);
            iR := iR + 1;
            IF c = '\n' THEN EXIT END
          END
        END
    | SelectionMode.AllSelection =>
        (* easy *)
        iL := 0;
        iR := view.vt.length
    END
  END Extend;


PROCEDURE Encage (             view: View;
                               p   : Point.T;
                               iL  : I;
                  VAR (* OUT*) iM  : I;
                               iR  : I;
                  VAR (* OUT*) cage: Rect.T   ): WhichEnd
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    pL, pR                      : Point.T;
    whichEnd                    : WhichEnd;
    cwest, ceast, cnorth, csouth: Pixels;
    start                       : VirtualStart;
  BEGIN
    IF view.virtual.dirty THEN VTVirtual.UpdateView (view) END;
    (* find the boundaries of the selection in the image *)
    VTBase.UnsafeLocatePoint (view, iL, pL);
    IF pL.v = -1 THEN
      pL.h := FarWest;
      pL.v := FarNorth
    ELSIF pL.v = -2 THEN
      pL.h := FarEast;
      pL.v := FarSouth
    END;
    IF iL < iR THEN
      VTBase.UnsafeLocatePoint (view, iR - 1, pR, 0);
      IF pR.v = -1 THEN
        pR.h := FarWest;
        pR.v := FarNorth
      ELSIF pR.v = -2 THEN
        pR.h := FarEast;
        pR.v := FarSouth
      END
    ELSE
      pR := pL
    END;
    IF (pR.v > pL.v) OR (p.v < pL.v) OR (p.v >= pR.v + view.lineSpacing) THEN
      (* more than one line *)
      pR.v := pR.v + view.lineSpacing; (* bottom of line *)
      IF (p.v - pL.v < pR.v - p.v) THEN
        (* closer to top *)
        whichEnd := WhichEnd.Left;
        (* find a rectangular cage *)
        IF p.v < pL.v + view.lineSpacing THEN
          cwest := pL.h;
          ceast := FarEast;
          cnorth := pL.v;
          csouth := pL.v + view.lineSpacing
        ELSE
          cwest := FarWest;
          ceast := FarEast;
          cnorth := pL.v + view.lineSpacing;
          csouth := pL.v + (pR.v - pL.v + 1) DIV 2
        END
      ELSE
        (* closer to bottom *)
        whichEnd := WhichEnd.Right;
        (* find a rectangular cage *)
        IF p.v >= pR.v - view.lineSpacing THEN
          cwest := FarWest;
          ceast := pR.h;
          cnorth := pR.v - view.lineSpacing;
          csouth := pR.v
        ELSE
          cwest := FarWest;
          ceast := FarEast;
          cnorth := pL.v + (pR.v - pL.v + 1) DIV 2;
          csouth := pR.v - view.lineSpacing
        END
      END
    ELSE
      (* one line *)
      IF (p.h - pL.h < pR.h - p.h) THEN
        (* closer to left *)
        whichEnd := WhichEnd.Left;
        cwest := pL.h;
        ceast := pL.h + (pR.h - pL.h + 1) DIV 2
      ELSE
        (* closer to right *)
        whichEnd := WhichEnd.Right;
        cwest := pL.h + (pR.h - pL.h + 1) DIV 2;
        ceast := pR.h
      END;
      cnorth := pL.v;
      csouth := pR.v + view.lineSpacing
    END;
    (* If the original point is not in the cage, that's because it was outside
       the text image.  Coordinates in that direction should be treated
       equally.  The mapping algorithm must have good properties for this to
       work. *)
    IF p.h < cwest THEN
      ceast := cwest;
      cwest := FarWest
    ELSIF p.h >= ceast THEN
      cwest := ceast;
      ceast := FarEast
    END;
    IF p.v < cnorth THEN
      csouth := cnorth;
      cnorth := FarNorth
    ELSIF p.v >= csouth THEN
      cnorth := csouth;
      csouth := FarSouth
    END;
    cage := Rect.Meet (
              Rect.FromEdges (cwest, ceast, cnorth, csouth), view.rect.full);
    (* decide which end *)
    CASE whichEnd OF
    | WhichEnd.Left => iM := iL
    | WhichEnd.Right =>
        iM := iR;
        IF iL < iR THEN
          VTBase.Up (view, view.lineWidth, iM, 0, start);
          IF start.at = iM THEN iM := iM - 1 END
        END
    END;
    RETURN whichEnd
  END Encage;
  
VAR wordCodes: ARRAY CHAR OF WordCode;

BEGIN
  FOR c := FIRST (CHAR) TO LAST (CHAR) DO
    IF c IN ISOChar.AlphaNumerics THEN
      wordCodes [c] := WordCode.AlphaNumeric
    ELSIF c IN ISOChar.Spaces THEN
      wordCodes [c] := WordCode.WhiteSpace
    ELSE
      wordCodes [c] := WordCode.Special
    END
  END;
  wordCodes ['\n'] := WordCode.Special (* I guess.  \r?  \f? *)
END VTPounce.
