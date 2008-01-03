(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri May 17 11:53:04 PDT 1996 by mhb                          *)
(*      modified on Mon Jan 30 14:32:28 PST 1995 by kalsow                       *)
(*      modified on Sun May  9 14:22:25 1993 by meehan                       *)
(*      modified on Tue Jun 16 13:08:10 PDT 1992 by muller                   *)
(*      modified on Thu Feb  7 14:12:23 PST 1991 by chan                     *)
(*      modified on Wed Feb  6 15:30:37 PST 1991 by brooks                   *)
(*      modified on Fri Sep 28 13:48:06 PDT 1990 by birrell                  *)
(*      modified on Tue Jun 26 15:15:58     1990 by jdd                      *)
(*      modified on Thu May 17  9:41:31 PDT 1990 by mcjones                  *)
(*      modified on Fri May 11 12:54:07 PDT 1990 by steveg                   *)
(*      modified on Wed May 17 16:19:47 PDT 1989 by gidi                     *)
<* PRAGMA LL *>

MODULE TextEditVBT;

IMPORT Axis, HVSplit, PaintOp, Pixmap, Pts, Rd,
       ScrollerVBTClass, Split, TextPort, TextPortClass,
       TextureVBT, Thread, VBT, VBTKitEnv, VTDef, VText;

REVEAL
  T = Public BRANDED OBJECT OVERRIDES init := Init END;
  Private = HVSplit.T BRANDED OBJECT END;

REVEAL
  Scrollbar = TextPortClass.Scrollbar BRANDED OBJECT
              OVERRIDES
                scroll     := Scroll;
                autoScroll := AutoScroll;
                thumb      := Thumb;
                update     := Update
              END;

PROCEDURE Init (v: T; scrollable := TRUE): T =
  VAR 
    colors: PaintOp.ColorScheme; 
    pred, texture: VBT.T := NIL;
  BEGIN
    TRY
      v := HVSplit.T.init (v, Axis.T.Hor);
      IF v.tp = NIL THEN v.tp := NEW (TextPort.T).init () END;
      Split.Insert (v, NIL, v.tp);
      colors := v.tp.getColorScheme ();
      IF scrollable THEN
        IF v.sb = NIL THEN
          v.sb := NEW (Scrollbar).init (Axis.T.Ver, colors)
        END;
        v.tp.scrollbar := v.sb;
        v.sb.textport := v.tp;
        IF NOT VBTKitEnv.ScrollbarWest THEN pred := v.tp; END;
        texture := NEW(TextureVBT.T, shape := Shape).init(colors.fg, Pixmap.Solid);
        Split.Insert (v, pred, texture);
        IF NOT VBTKitEnv.ScrollbarWest THEN pred := texture; END;
        Split.Insert (v, pred, v.sb)
      END
    EXCEPT
    | Split.NotAChild =>         <* ASSERT FALSE *>
    END;
    RETURN v
  END Init;

PROCEDURE Shape (v: TextureVBT.T; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
  VAR sr: VBT.SizeRange;
  BEGIN
    IF ax = Axis.T.Hor THEN
      sr.lo := Pts.ToScreenPixels (v, 1.0, Axis.T.Hor);
      sr.pref := sr.lo;
      sr.hi := sr.lo + 1;
      RETURN sr
    ELSE
      RETURN TextureVBT.T.shape (v, ax, n)
    END
  END Shape;

PROCEDURE Update (s: Scrollbar) =
  <* LL = v.mu *>
  CONST name = "Update Scrollbar";
  VAR
    v     := s.textport;
    vtext := v.vtext;
    start: CARDINAL;
  BEGIN
    TRY
      start := VText.StartIndex (vtext, 0);
      ScrollerVBTClass.Update (
        s, start, start + VText.CharsInRegion (vtext, 0),
        v.length ())
    EXCEPT
    | VTDef.Error (ec) => v.vterror (name, ec)
    | Rd.EndOfFile => v.rdeoferror (name)
    | Rd.Failure (ref) => v.rdfailure (name, ref)
    | Thread.Alerted =>
    END
  END Update;

CONST NearEdge = 13;
(* Thumbing closer than this to top/bottom of scroll bar is treated as
       being exactly at the top/bottom. *)

PROCEDURE Scroll (                      s     : Scrollbar;
                  <* UNUSED *> READONLY cd    : VBT.MouseRec;
                                        part  : INTEGER;
                  <* UNUSED *>          height: INTEGER;
                  towardsEOF: BOOLEAN) =
  <* LL= VBT.mu *>
  CONST name = "Scroll";
  VAR
    v                 := s.textport;
    vtext             := v.vtext;
    distance: INTEGER;
  BEGIN
    LOCK v.mu DO
      TRY
        distance := MAX (1, VText.WhichLine (vtext, 0, part));
        IF NOT towardsEOF THEN distance := -distance END;
        VText.Scroll (vtext, 0, distance);
        VText.Update (vtext);
        s.update ()
      EXCEPT
      | VTDef.Error (ec) => v.vterror (name, ec)
      | Rd.EndOfFile => v.rdeoferror (name)
      | Rd.Failure (ref) => v.rdfailure (name, ref)
      | Thread.Alerted =>
      END
    END
  END Scroll;

PROCEDURE AutoScroll (                      s : Scrollbar;
                      <* UNUSED *> READONLY cd: VBT.MouseRec;
                      linesToScroll: CARDINAL;
                      towardsEOF   : BOOLEAN   ) =
  <* LL = VBT.mu *>
  CONST name = "AutoScroll";
  VAR
    distance: INTEGER := linesToScroll;
    v                 := s.textport;
    vtext             := v.vtext;
  BEGIN
    LOCK v.mu DO
      IF NOT towardsEOF THEN distance := -distance END;
      TRY
        VText.Scroll (vtext, 0, distance);
        VText.Update (vtext);
        s.update ()
      EXCEPT
      | VTDef.Error (ec) => v.vterror (name, ec)
      | Rd.EndOfFile => v.rdeoferror (name)
      | Rd.Failure (ref) => v.rdfailure (name, ref)
      | Thread.Alerted =>
      END
    END
  END AutoScroll;

PROCEDURE Thumb (                      s     : Scrollbar;
                 <* UNUSED *> READONLY cd    : VBT.MouseRec;
                                       part  : INTEGER;
                                       height: INTEGER       ) =
  <* LL = VBT.mu *>
  CONST name = "Thumb";
  VAR
    position: CARDINAL;
    v                  := s.textport;
    vtext              := v.vtext;
    length             := v.length ();
  BEGIN
    LOCK v.mu DO
      TRY
        IF length = 0 OR part < NearEdge THEN
          position := 0
        ELSIF part + NearEdge > height THEN
          position := length - 1
        ELSE
          position :=
            MAX (0, ROUND (FLOAT (length)
                             * (FLOAT (part) / FLOAT (height))))
        END;
        VText.SetStart (vtext, 0, position);
        VText.Update (vtext);
        s.update ()
      EXCEPT
      | VTDef.Error (ec) => v.vterror (name, ec)
      | Rd.EndOfFile => v.rdeoferror (name)
      | Rd.Failure (ref) => v.rdfailure (name, ref)
      | Thread.Alerted =>
      END
    END
  END Thumb;

BEGIN
END TextEditVBT.
