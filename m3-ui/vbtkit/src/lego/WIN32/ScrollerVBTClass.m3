(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 30 15:22:54 PST 1995 by kalsow *)
(*      modified on Wed Jun  2 16:53:21 PDT 1993 by meehan *)
(*      modified on Sat Jan 30 02:11:47 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:08:20 PDT 1992 by muller *)
(*      modified on Fri Mar 27 02:55:05 1992 by steveg*)
<* PRAGMA LL *>

MODULE ScrollerVBTClass;

IMPORT AutoRepeat, Axis, (* Cursor, *) PaintOp, Pixmap, Point, Rect,
         Region, VBT, VBTKitResources;
(*Debugging> IMPORT IO, Fmt; <Debugging*)

TYPE
    Dim = RECORD
          pixels: INTEGER;
          millimeters: REAL;
        END;

    State = {Listen, Thumb, ScrollEOF, ScrollBOF};

REVEAL
  T =
    Public BRANDED OBJECT
      mu   : MUTEX;
      <* LL = mu *>
      start, end, length: CARDINAL;
      state: State;
      event: VBT.MouseRec;
      (* The current event, as given to the client. The event starts out as
         the FirstDown mouse event that initiated scrolling. Auto-scroll
         causes the time to be set to zero, since auto-scroll does not
         happen in event time. Thumbing and proportional
         scrolling cause the time, cursor position, and modifier fields to
         be updated (these are Trestle position events). *)

      eventLoc: Point.T;
      (* The coordinate of mouse's FirstDown. Used to implement dead area
         for proportional scrolling. That is, as long as mouse is with a
         few pixels of where it first went down, then autoscrolling starts.
         Once the mouse moves a sufficient amount, any autoscrolling in
         progress is stopped. *)

      axis          : Axis.T;
      scrollTextureP: ARRAY Axis.T OF PaintOp.T;
      scrollTexture : ARRAY Axis.T OF Pixmap.T;

      stripeBorderP : PaintOp.T;
      stripeBorder  : Pixmap.T;
      stripeTextureP: ARRAY Axis.T OF PaintOp.T;
      stripeTexture : ARRAY Axis.T OF Pixmap.T;

      stripe: Rect.T;

      needToComputePixels: BOOLEAN := FALSE; (* set true when MM passed in via SetAttributes() *)

      stripeW, stripeE, stripeN, stripeS: Dim;

      scrollMargin, stripeWidth, minStripeLen: Dim;

      repeater: AutoRepeater;

    OVERRIDES
      scroll     := ScrollProc;
      autoScroll := AutoProc;
      thumb      := ThumbProc;
      repaint    := Repaint;
      reshape    := Reshape;
      redisplay  := Redisplay;
      rescreen   := Rescreen;
      mouse      := Mouse;
      position   := Position;
      discard    := Discard;
      shape      := Shape;
      init       := Init;
    END;


TYPE
  ActionType = {Scroll, AutoScroll, Thumb};
  Action = RECORD
             type: ActionType;    (* Scroll, AutoScroll, Thumb *)
             cd  : VBT.MouseRec;  (* Scroll, AutoScroll, Thumb *)
             linesToScroll: CARDINAL;  (* AutoScroll *)
             towardsEOF   : BOOLEAN;   (* Scroll, AutoScroll *)
             part         : INTEGER;   (* Scroll, Thumb *)
             height       : INTEGER;   (* Scroll, Thumb *)
           END;
  AutoRepeater =
    AutoRepeat.T OBJECT v: T OVERRIDES repeat := Repeat END;

PROCEDURE Init (v: T; axis := Axis.T.Ver; colors: PaintOp.ColorQuad := NIL): T =
  BEGIN
    IF colors = NIL THEN colors := PaintOp.bgFg END;
    InitGraphics ();
    v.mu := NEW (MUTEX);
    v.axis := axis;
    v.start := 0;
    v.end := 0;
    v.length := 0;
    FOR a := FIRST (Axis.T) TO LAST (Axis.T) DO
      v.scrollTextureP [a] := PaintOp.FromRGB (0.8, 0.8, 0.8);
      v.scrollTexture [a] := ScrollPixmap [a];
      v.stripeTextureP [a] := PaintOp.FromRGB (0.6, 0.6, 0.6);
      v.stripeTexture [a] := Pixmap.Solid;
    END;
    v.stripeBorderP := colors.bgFg;
    v.stripeBorder := Pixmap.Solid;
    WITH quarterMargin = ROUND(FLOAT(DefaultScrollMargin) * 0.25)
    DO
       v.stripeW.pixels := quarterMargin;
       v.stripeE.pixels := quarterMargin;
       v.stripeN.pixels := quarterMargin;
       v.stripeS.pixels := quarterMargin;
    END; (* with *)
    v.scrollMargin.pixels := DefaultScrollMargin;
    v.stripeWidth.pixels  := DefaultStripeWidth;
    v.minStripeLen.millimeters := DefaultMinStripeLen;
    v.repeater := NEW (AutoRepeater, v := v).init ();
    RETURN v
  END Init;

PROCEDURE Update (v: T; start, end, length: CARDINAL) =
  BEGIN
    LOCK v.mu DO
      IF start = v.start AND end = v.end AND length = v.length THEN
        RETURN
      ELSE
        v.start := start;
        v.end := end;
        v.length := length
      END
    END;
    VBT.Mark(v);
  END Update;


(************)
(* Graphics *)
(************)

PROCEDURE GetAttributes (v: T): Attributes =
  VAR a: Attributes;
  BEGIN
(*Debugging> IO.Put("===+++===!!! GetAttributes !!!===+++==="); <Debugging*)
    a.axis := v.axis;
    a.margin := v.scrollMargin.millimeters;
    a.scrollPaintOps := v.scrollTextureP;
    a.scrollPixmaps := v.scrollTexture;
    a.minStripeLen := v.minStripeLen.millimeters;
    a.stripeWidth := v.stripeWidth.millimeters;
    a.stripePaintOps := v.stripeTextureP;
    a.stripePixmaps := v.stripeTexture;
    a.stripeBorder := v.stripeW.millimeters;
    a.stripeBorderPaintOp := v.stripeBorderP;
    a.stripeBorderPixmap := v.stripeBorder;
    IF v # NIL THEN
      LOCK v.mu DO
        IF NOT v.needToComputePixels THEN
          WITH st = VBT.ScreenTypeOf(v) DO
            IF st # NIL THEN
              a.margin := FLOAT(v.scrollMargin.pixels) / st.res[Axis.Other[v.axis]];
              a.stripeWidth := FLOAT(v.stripeWidth.pixels) / st.res[Axis.Other[v.axis]];
              a.stripeBorder := FLOAT(v.stripeW.pixels) / st.res[Axis.Other[v.axis]];
            END;
          END;
        END;
      END;
    END;
    RETURN a;
  END GetAttributes;

PROCEDURE SetAttributes (v: T; READONLY a: Attributes) =
  <* LL.sup = VBT.mu *>
  BEGIN
(*Debugging> IO.Put("===+++===!!! SetAttributes !!!===+++==="); <Debugging*)
    v.axis := a.axis;
    v.scrollMargin.millimeters := a.margin;
    v.scrollTextureP := a.scrollPaintOps;
    v.scrollTexture := a.scrollPixmaps;
    v.minStripeLen.millimeters := a.minStripeLen;
    v.stripeWidth.millimeters := a.stripeWidth;
    v.stripeTextureP := a.stripePaintOps;
    v.stripeTexture := a.stripePixmaps;
    v.stripeW.millimeters := a.stripeBorder;
    v.stripeE.millimeters := a.stripeBorder;
    v.stripeS.millimeters := a.stripeBorder;
    v.stripeN.millimeters := a.stripeBorder;
    v.stripeBorderP := a.stripeBorderPaintOp;
    v.stripeBorder := a.stripeBorderPixmap;
    v.needToComputePixels := TRUE;

    VBT.NewShape(v);
    VBT.Mark(v);
  END SetAttributes;

PROCEDURE Colorize (v: T; colors: PaintOp.ColorQuad) =
  <* LL.sup = VBT.mu *>
  BEGIN
    LOCK v.mu DO
      v.scrollTextureP[v.axis] := colors.bgFg;
      v.stripeTextureP[v.axis] := colors.bgFg;
      v.stripeBorderP := colors.bgFg;
    END;
    VBT.Mark(v);
  END Colorize;


(*********)
(* Paint *)
(*********)

PROCEDURE PaintView (v: T) =
  (* LL = v.mu. *)
  VAR
    dom:    Rect.T;
    stripe: Rect.T;
    parts:  Rect.Partition;
    hi, low: Rect.T;
  BEGIN
    dom    := VBT.Domain(v);
    dom    := AdjustForScrollButtons (v, dom, low, hi);
    ComputeStripePosition(v, dom);

      (* paint scroll buttons *)
    VBT.PaintPixmap(v, low, PaintOp.Copy,
                       ScrollButtonPixmap[v.axis, ScrollButton.Low],
                       Rect.NorthWest(low));
    VBT.PaintPixmap(v, hi, PaintOp.Copy,
                       ScrollButtonPixmap[v.axis, ScrollButton.High],
                       Rect.NorthWest(hi));

    stripe := v.stripe;

      (* paint stripe *)
    INC(stripe.west,  v.stripeW.pixels);
    DEC(stripe.east,  v.stripeE.pixels);
    INC(stripe.north, v.stripeN.pixels);
    DEC(stripe.south, v.stripeS.pixels);
    VBT.PaintTexture(
      v, stripe, v.stripeTextureP[v.axis], v.stripeTexture[v.axis]);

      (* paint border around stripe *)
    DEC(stripe.west,  v.stripeW.pixels);
    INC(stripe.east,  v.stripeE.pixels);
    DEC(stripe.north, v.stripeN.pixels);
    INC(stripe.south, v.stripeS.pixels);
    PaintBorder(
      v, stripe, v.stripeBorderP, v.stripeBorder, v.stripeW.pixels,
      v.stripeE.pixels, v.stripeN.pixels, v.stripeS.pixels);

    Rect.Factor(dom, stripe, parts, 0, 0);

      (* paint scroll texture in four pieces *)
    WITH textureP = v.scrollTextureP[v.axis],
         texture  = v.scrollTexture [v.axis] DO
      VBT.PaintTexture(v, parts[0], textureP, texture);
      VBT.PaintTexture(v, parts[1], textureP, texture);
      VBT.PaintTexture(v, parts[3], textureP, texture);
      VBT.PaintTexture(v, parts[4], textureP, texture);
    END;

  END PaintView;

PROCEDURE AdjustForScrollButtons (v: T; r: Rect.T; VAR low, high: Rect.T): Rect.T =
  (* LL = mu. Sets the domain of low and high 'arrow' buttons,
              and returns the subset of the domain which is
              to be used for drawing. *)
  VAR
    res : Rect.T := r;
    scrollButtonSize := Rect.Size(v.axis, VBT.PixmapDomain(v, ScrollButtonPixmap[v.axis, ScrollButton.Low]));
  BEGIN
    low := r; high := r;
    IF v.axis = Axis.T.Ver THEN
      INC(res.north, scrollButtonSize);
      DEC(res.south, scrollButtonSize);
      high.north := res.south;
      low.south := res.north;
    ELSE
      INC(res.west, scrollButtonSize);
      DEC(res.east, scrollButtonSize);
      high.west := res.east;
      low.east := res.west;
    END;
    RETURN res;
  END AdjustForScrollButtons;

PROCEDURE ComputeStripePosition (v: T; r: Rect.T) =
  (* LL = mu. Returns the domain of the white part of the stripe. *)
  VAR
    lo, hi             : INTEGER;
    top, bottom, height: INTEGER;
    factor             : REAL;
  BEGIN
    IF v.axis = Axis.T.Hor THEN
      r.north := r.north + v.scrollMargin.pixels;
      r.south := r.south - v.scrollMargin.pixels;
      IF r.south - r.north < v.stripeWidth.pixels THEN
        r.south := r.north + v.stripeWidth.pixels;
      END;
      lo := r.west;
      hi := r.east;
    ELSE
      r.west := r.west + v.scrollMargin.pixels;
      r.east := r.east - v.scrollMargin.pixels;
      IF r.east - r.west < v.stripeWidth.pixels THEN
        r.east := r.west + v.stripeWidth.pixels;
      END;
      lo := r.north;
      hi := r.south;
    END;
    IF v.end - v.start < v.length THEN
      height := hi - lo;
      factor := FLOAT (height) / FLOAT (v.length);
      top := lo + TRUNC (FLOAT (v.start) * factor);
      bottom := MIN (hi, lo + TRUNC (FLOAT (v.end) * factor));
      hi := MIN (hi, MAX (bottom, top + v.minStripeLen.pixels));
      lo := MAX (lo, MIN (top, hi - v.minStripeLen.pixels));
      IF v.axis = Axis.T.Hor THEN
        r.west := lo;
        r.east := hi;
      ELSE
        r.north := lo;
        r.south := hi;
      END;
    END;
    v.stripe := r;
  END ComputeStripePosition;

VAR
  white := PaintOp.FromRGB (1.0, 1.0, 1.0);
  black := PaintOp.FromRGB (0.0, 0.0, 0.0);

PROCEDURE PaintBorder (         v         : VBT.T;
                       READONLY r         : Rect.T;
                     <*UNUSED*> textureP  : PaintOp.T;
                                texture   : Pixmap.T;
                                w, e, n, s: INTEGER    ) =
  (* LL = mu. *)
  VAR sr: Rect.T;
  BEGIN
    sr.west := r.west + w;
    sr.east := r.east - e;
    sr.north := r.north;
    sr.south := r.north + n;
    VBT.PaintTexture(v, sr, white, texture, Point.Origin); (* top *)
    sr.north := r.south - s;
    sr.south := r.south;
    VBT.PaintTexture(v, sr, black, texture, Point.Origin); (* bottom *)
    sr.north := r.north;
    sr.west := r.west;
    sr.east := sr.west + w;
    VBT.PaintTexture(v, sr, white, texture, Point.Origin); (* left *)
    sr.west := r.east - e;
    sr.east := r.east;
    VBT.PaintTexture(v, sr, black, texture, Point.Origin); (* right *)
  END PaintBorder;


(********************)
(* Scroller methods *)
(********************)

PROCEDURE Mouse (v: T; READONLY cd: VBT.MouseRec) RAISES {} =
  VAR
    action: Action;
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      IF Gesture (v, cd, action) THEN
        VBT.SetCage(v, VBT.CageFromPosition(cd.cp, TRUE));
        GetPartHeight (v, action.part, action.height);
        PerformAction (v, action);
        AutoRepeat.Start (v.repeater);
      END;
    ELSIF v.state # State.Listen THEN
      AutoRepeat.Stop(v.repeater);
      VBT.SetCage (v, VBT.EverywhereCage);
      v.state := State.Listen;
      v.state := State.Listen;
    END;
  END Mouse;

PROCEDURE WithinButtons (v: T; READONLY cp: VBT.CursorPosition;
                    VAR step: INTEGER): BOOLEAN =
(* LL = v.mu *)
  CONST
    Slop = 1; (* One 'units' movement. *)
  VAR
    dom := VBT.Domain(v);
    low, hi: Rect.T;
    factor: INTEGER := 0;
  <*UNUSED*>  VAR
    logical_min, logical_max, logical_step: INTEGER;
  BEGIN
    EVAL AdjustForScrollButtons (v, dom, low, hi);
    IF Rect.Member (cp.pt, low) THEN
      factor := -1;
    ELSIF Rect.Member (cp.pt, hi) THEN
      factor := +1;
    END;
(******
    logical_min := ScrollerVBT.GetMin(v);
    logical_max := ScrollerVBT.GetMax(v);
    <* ASSERT logical_max > logical_min *>
    step := (factor * v.length * logical_step) DIV (logical_max - logical_min);
*****)
    step := factor * Slop;
    RETURN factor # 0;
  END WithinButtons;

PROCEDURE BeforeThumb (v: T; READONLY cp: VBT.CursorPosition): BOOLEAN =
(* LL = v.mu *)
  BEGIN
    RETURN Rect.Member (cp.pt, VBT.Domain(v)) AND
           DeltaPoints (v.axis, cp.pt, Rect.NorthWest (v.stripe)) < 0;
  END BeforeThumb;

PROCEDURE AfterThumb (v: T; READONLY cp: VBT.CursorPosition): BOOLEAN =
(* LL = v.mu *)
  BEGIN
    RETURN Rect.Member (cp.pt, VBT.Domain(v)) AND
           DeltaPoints (v.axis, cp.pt, Rect.SouthEast(v.stripe)) > 0;
  END AfterThumb;

PROCEDURE WithinThumb (v: T; READONLY cp: VBT.CursorPosition): BOOLEAN =
(* LL = v.mu *)
  BEGIN
    RETURN Rect.Member (cp.pt, VBT.Domain (v)) AND
      DeltaPoints (v.axis, cp.pt, Rect.NorthWest (v.stripe)) > 0 AND
      DeltaPoints (v.axis, cp.pt, Rect.SouthEast (v.stripe)) < 0;
  END WithinThumb;

PROCEDURE Gesture (v: T; READONLY cd: VBT.MouseRec; VAR action: Action): BOOLEAN =
  VAR
    step: INTEGER;
  BEGIN
    LOCK v.mu DO
      CASE cd.whatChanged OF
      | VBT.Modifier.MouseL,
        VBT.Modifier.MouseR,
        VBT.Modifier.MouseM =>
          (*Debugging> IO.Put("Mouse L, R, or M\n"); <Debugging*)
          CASE v.state OF
          | State.Listen =>
            IF WithinButtons (v, cd.cp, step) THEN
              action.type := ActionType.AutoScroll;
              IF step < 0 THEN
                (*Debugging> IO.Put("Top Button\n"); <Debugging*)
                v.state := State.ScrollBOF;
              ELSIF step > 0 THEN
                (*Debugging> IO.Put("Bottom Button\n"); <Debugging*)
                v.state := State.ScrollEOF;
              ELSE
                <* ASSERT FALSE *> (* step must be either negative or positive *)
              END;
              action.towardsEOF := step > 0;
              action.linesToScroll := ABS(step); (* this line added by RCC to make auto scroll work *)
            ELSIF BeforeThumb (v, cd.cp) THEN
              action.type := ActionType.Scroll;
              v.state := State.ScrollBOF;
              action.towardsEOF := FALSE;
            ELSIF AfterThumb (v, cd.cp) THEN
              action.type := ActionType.Scroll;
              action.towardsEOF := TRUE;
              v.state := State.ScrollEOF;
            ELSIF WithinThumb (v, cd.cp) THEN
              (* Don't do anything when the thumb is down,
                 unless there is some mouse movement. *)
              VBT.SetCage(v, VBT.CageFromPosition(cd.cp, TRUE));
              v.state := State.Thumb;
              RETURN FALSE;
            END;
          | State.ScrollEOF =>
            action.towardsEOF := TRUE;
            IF AfterThumb (v, cd.cp) THEN
              action.type := ActionType.Scroll;
            ELSIF WithinButtons (v, cd.cp, step) THEN
              IF step > 0 THEN
                action.type := ActionType.AutoScroll;
                action.linesToScroll := step; (* this line added by RCC to make auto scroll work *)
              END;
            ELSE RETURN FALSE;
            END;
          | State.ScrollBOF =>
            action.towardsEOF := FALSE;
            IF BeforeThumb (v, cd.cp) THEN
              action.type := ActionType.Scroll;
            ELSIF WithinButtons (v, cd.cp, step) THEN
              IF step > 0 THEN
                action.type := ActionType.AutoScroll;
                action.linesToScroll := step; (* this line added by RCC to make auto scroll work *)
                v.state := State.ScrollBOF;
              END;
            ELSE RETURN FALSE;
            END;
          | State.Thumb =>
            action.type := ActionType.Thumb;
          ELSE
            RETURN FALSE;
          END;
(*Debugging>
      | VBT.Modifier.Mouse0 => IO.Put("Mouse 0\n"); RETURN FALSE;
      | VBT.Modifier.Mouse1 => IO.Put("Mouse 1\n"); RETURN FALSE;
      | VBT.Modifier.Mouse2 => IO.Put("Mouse 2\n"); RETURN FALSE;
      | VBT.Modifier.Mouse3 => IO.Put("Mouse 3\n"); RETURN FALSE;
      | VBT.Modifier.Mouse4 => IO.Put("Mouse 4\n"); RETURN FALSE;
(* ---
      | VBT.Modifier.Mod0 => IO.Put("Mod 0\n"); RETURN FALSE;
      | VBT.Modifier.Mod1 => IO.Put("Mod 1\n"); RETURN FALSE;
      | VBT.Modifier.Mod2 => IO.Put("Mod 2\n"); RETURN FALSE;
      | VBT.Modifier.Mod3 => IO.Put("Mod 3\n"); RETURN FALSE;
      | VBT.Modifier.Shift => IO.Put("Shift\n"); RETURN FALSE;
      | VBT.Modifier.Lock => IO.Put("Lock\n"); RETURN FALSE;
      | VBT.Modifier.Control => IO.Put("Control\n"); RETURN FALSE;
      | VBT.Modifier.Option => IO.Put("Option\n"); RETURN FALSE;
*)
<Debugging*)
      ELSE
         RETURN FALSE;
      END;
      v.event := cd;
      action.cd := cd;
    END (* LOCK v.mu *);
    RETURN TRUE;
  END Gesture;

PROCEDURE Position (v: T; READONLY cd: VBT.PositionRec) RAISES {} =
  VAR
    action: Action;
    perform: BOOLEAN := FALSE;
  BEGIN
    IF v.state = State.Listen THEN RETURN END;
    VBT.SetCage(v, VBT.CageFromPosition(cd.cp, TRUE));
    IF cd.cp.offScreen THEN AutoRepeat.Stop(v.repeater); RETURN END;
    AutoRepeat.Start(v.repeater);
    LOCK v.mu DO
      v.event.cp := cd.cp;
      IF Rect.Member (cd.cp.pt, VBT.Domain(v)) THEN
         GetPartHeight (v, action.part, action.height);
         perform := TRUE;
      ELSE
        AutoRepeat.Stop (v.repeater);
      END;
    END;
    IF perform THEN
      IF Gesture (v, v.event, action) THEN
        PerformAction (v, action);
      END;
    END;
  END Position;

PROCEDURE GetPartHeight (v: T; VAR (* out*) part, height: INTEGER) =
  (* LL = mu *)
  VAR dom := VBT.Domain(v);
  BEGIN
    height := Rect.Size(v.axis, dom);
    part := DeltaPoints(v.axis, v.event.cp.pt, Rect.NorthWest(dom));
  END GetPartHeight;

PROCEDURE DeltaPoints (axis: Axis.T; READONLY p1, p2: Point.T): INTEGER =
  BEGIN
    IF axis = Axis.T.Hor THEN
      RETURN p1.h - p2.h;
   ELSE
      RETURN p1.v - p2.v;
    END;
  END DeltaPoints;

(* CONST ActionNames = ARRAY ActionType OF TEXT {"Scroll", "AutoScroll", "Thumb"}; *)

PROCEDURE PerformAction (v: T; READONLY action: Action) =
  BEGIN
    CASE action.type OF
    | ActionType.Scroll =>
        v.scroll(action.cd, (*action.part,*)action.height, action.height, action.towardsEOF);
           (* In the call above, I changed "part" to "height" for Windows to get one screen
           at a time instead of the proportional effects specified in the interface.--R.Coleburn*)
    | ActionType.AutoScroll =>
        v.autoScroll(action.cd, action.linesToScroll, action.towardsEOF);
    | ActionType.Thumb => v.thumb(action.cd, action.part, action.height);
    END;
  END PerformAction;

(***********

PROCEDURE Where (t: TEXT; cp: VBT.CursorPosition) =
  BEGIN
    IO.Put (Fmt.F(" (%s %s,%s) ", t, Fmt.Int(cp.pt.h), Fmt.Int(cp.pt.v)));
  END Where;

***********)

PROCEDURE Repeat (ar: AutoRepeater) =
  VAR
    action: Action;
    v              := ar.v;
  BEGIN
    IF Gesture (v, v.event, action) THEN
      LOCK v.mu DO
        GetPartHeight (v, action.part, action.height);
      END;
      PerformAction (v, action);
    END;
  END Repeat;

PROCEDURE Repaint (v: T; <* UNUSED *> READONLY rgn: Region.T) RAISES {} =
  BEGIN
    LOCK v.mu DO PaintView(v); END;
  END Repaint;

PROCEDURE Reshape (v: T; <* UNUSED *> READONLY cd: VBT.ReshapeRec) RAISES {} =
  BEGIN
    VBT.Mark(v);
    (* cause the repaint to happen *after* all reshaping takes place. This
       strategy is needed because another VBT's Reshape method may call
       Update to change the length of the stripe. *)
  END Reshape;

PROCEDURE Redisplay (v: T) RAISES {} =
  BEGIN
    Repaint(v, Region.FromRect(VBT.Domain(v)));
  END Redisplay;

PROCEDURE Rescreen (                      v : T;
                    <* UNUSED *> READONLY cd: VBT.RescreenRec)
  RAISES {} =
  BEGIN
   LOCK v.mu DO
    IF v.needToComputePixels
    THEN
       v.stripeW.pixels :=
         ROUND(VBT.MMToPixels(v, v.stripeW.millimeters, Axis.T.Hor));
       v.stripeE.pixels :=
         ROUND(VBT.MMToPixels(v, v.stripeE.millimeters, Axis.T.Hor));
       v.stripeN.pixels :=
         ROUND(VBT.MMToPixels(v, v.stripeN.millimeters, Axis.T.Ver));
       v.stripeS.pixels :=
         ROUND(VBT.MMToPixels(v, v.stripeS.millimeters, Axis.T.Ver));

       v.scrollMargin.pixels :=
         ROUND(VBT.MMToPixels(
           v, v.scrollMargin.millimeters, Axis.Other[v.axis]));

       v.stripeWidth.pixels :=
         ROUND(VBT.MMToPixels(
           v, v.stripeWidth.millimeters, Axis.Other[v.axis]));
       v.needToComputePixels := (v.stripeWidth.pixels = 0) AND (v.stripeWidth.millimeters # 0.0);
    ELSE
       WITH dom = VBT.PixmapDomain(v, ScrollButtonPixmap[v.axis, ScrollButton.Low]),
            multiplier = Rect.Size(v.axis, dom) DIV PixmapSize,
            margin = multiplier * DefaultScrollMargin,
            quarterMargin = ROUND(FLOAT(margin) * 0.25),
            width = multiplier * DefaultStripeWidth
       DO
          v.stripeW.pixels := quarterMargin;
          v.stripeE.pixels := quarterMargin;
          v.stripeN.pixels := quarterMargin;
          v.stripeS.pixels := quarterMargin;
          v.scrollMargin.pixels := margin;
          v.stripeWidth.pixels := width;
       END; (* with *)
    END; (* if *)
(*Debugging> IO.Put("=== === RescreenScrollBar === === qm=" & Fmt.Int(v.stripeW.pixels) & ", margin=" & Fmt.Int(v.scrollMargin.pixels) & ", width=" & Fmt.Int(v.stripeWidth.pixels) & " === ===\n"); <Debugging*)
    v.minStripeLen.pixels :=
      ROUND(VBT.MMToPixels(v, v.minStripeLen.millimeters, v.axis));
    VBT.NewShape(v);
   END;
  END Rescreen;

PROCEDURE Shape (v: T; ax: Axis.T; <* UNUSED *> n: CARDINAL):
  VBT.SizeRange =
  VAR shape: ARRAY Axis.T OF VBT.SizeRange;
  BEGIN
    WITH otherAxis = Axis.Other[v.axis] DO
      shape[v.axis].lo := v.minStripeLen.pixels;
      shape[otherAxis].lo :=
        v.stripeWidth.pixels + 2 * v.scrollMargin.pixels;

      shape[v.axis].pref := shape[v.axis].lo;
      shape[v.axis].hi :=
        MAX(shape[v.axis].pref + 1, VBT.DefaultShape.hi);

      shape[otherAxis].pref := shape[otherAxis].lo;
      shape[otherAxis].hi := shape[otherAxis].lo + 1;
    END;
    RETURN shape[ax]
  END Shape;

PROCEDURE Discard (v: T) =
  BEGIN
    LOCK v.mu DO AutoRepeat.Stop(v.repeater) END
  END Discard;


(******************************  Default methods  ********************)

PROCEDURE ScrollProc ( <* UNUSED *> v: T;
                       <* UNUSED *> READONLY cd: VBT.MouseRec;
                       <* UNUSED *> part: INTEGER;
                       <* UNUSED *> height: INTEGER;
                       <* UNUSED *> towardsEOF: BOOLEAN) =
  BEGIN
  END ScrollProc;

PROCEDURE AutoProc ( <* UNUSED *> v: T;
                     <* UNUSED *> READONLY cd: VBT.MouseRec;
                     <* UNUSED *> linesToScroll: CARDINAL;
                     <* UNUSED *> towardsEOF: BOOLEAN) =
  BEGIN
  END AutoProc;

PROCEDURE ThumbProc ( <* UNUSED *> v: T;
                      <* UNUSED *> READONLY cd: VBT.MouseRec;
                      <* UNUSED *> part: INTEGER;
                      <* UNUSED *> height: INTEGER) =
  BEGIN
  END ThumbProc;


(************)
(* Graphics *)
(************)

TYPE
  ScrollButton = {Low, High};

VAR
  ScrollButtonPixmap: ARRAY Axis.T OF ARRAY ScrollButton OF Pixmap.T ;
  ScrollButtonPixmapNames := ARRAY Axis.T OF ARRAY ScrollButton OF TEXT
               {ARRAY ScrollButton OF TEXT {"left.ppm", "right.ppm"},
                ARRAY ScrollButton OF TEXT {"up.ppm", "down.ppm"}};
(* Note that for the above pixmaps, the raw size is 16 pixels square.
   If the pixmaps change, you must change the values of PixmapSize,
   DefaultScrollMargin, and DefaultStripeWidth below as per the formula. *)

CONST
  PixmapSize          = 16; (* # pixels on each side, pixmap must be square *)
  DefaultScrollMargin =  3; (* in pixels *) (* ((2 x Margin) + StripeWidth) = PixmapSize *)
  DefaultStripeWidth  = 10; (* in pixels *) (* ((2 x Margin) + StripeWidth) = PixmapSize *)
  DefaultMinStripeLen =  8.0; (* in millimeters *)

VAR
  globalLock: MUTEX;
  graphicsInited: BOOLEAN;
(*  Cursors: ARRAY State, Axis.T OF Cursor.T; *)
  ScrollPixmap: ARRAY Axis.T OF Pixmap.T;

PROCEDURE InitGraphics () =
  BEGIN
    LOCK globalLock DO
      IF graphicsInited THEN RETURN END; graphicsInited := TRUE;

      WITH a = Axis.T.Hor DO
        ScrollPixmap[a] :=
          VBTKitResources.GetPixmap("ScrollBg");
      END;

      WITH a = Axis.T.Ver DO
        ScrollPixmap[a] :=
          VBTKitResources.GetPixmap("ScrollBg");
      END;

      FOR i := FIRST(Axis.T) TO LAST(Axis.T) DO
        FOR j := FIRST(ScrollButton) TO LAST(ScrollButton) DO
          ScrollButtonPixmap[i,j] :=
              VBTKitResources.GetPixmap (ScrollButtonPixmapNames[i,j]);
        END;
      END;

    END;
  END InitGraphics;

BEGIN
  graphicsInited := FALSE;
  globalLock := NEW(MUTEX);
END ScrollerVBTClass.

