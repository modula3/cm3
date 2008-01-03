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

IMPORT AutoRepeat, Axis, Cursor, PaintOp, Palette, Pixmap, Point,
         Rect, Region, ScreenType, ScrnColorMap, VBT, VBTKitResources,
         Shadow, ShadowPaint, TrestleComm;

TYPE
  State = {ListenState, JumpEOFState, JumpSOFState, ContEOFState,
            ContSOFState, PropState, ThumbState};

  Dim = RECORD
          pixels: INTEGER;
          millimeters: REAL
        END;

CONST
  Shadows = TRUE;

REVEAL
  T =
    Public BRANDED OBJECT
      ready: BOOLEAN;
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

      colors        : PaintOp.ColorQuad;
      shadow        : Shadow.T;      (* NIL <==> don't use shadows *)
      troughColor   : PaintOp.T;
      shadowPixels  : CARDINAL;

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
      v.scrollTextureP [a] := colors.bgFg;
      v.scrollTexture [a] := ScrollPixmap [a];
      v.stripeTextureP [a] := colors.bgFg;
      v.stripeTexture [a] := Pixmap.Empty;
    END;
    v.colors := colors;
    v.shadow := NIL;
    v.stripeBorderP := colors.bgFg;
    v.stripeBorder := Pixmap.Solid;
    v.stripeW.millimeters := 0.25;
    v.stripeE.millimeters := 0.25;
    v.stripeN.millimeters := 0.25;
    v.stripeS.millimeters := 0.25;
    v.scrollMargin.millimeters := DefaultScrollMargin;
    v.stripeWidth.millimeters := DefaultStripeWidth;
    v.minStripeLen.millimeters := DefaultMinStripeLen;
    v.repeater := NEW (AutoRepeater, v := v).init ();
    VBT.SetCursor (v, Cursors [State.ListenState, v.axis]);
    RETURN v
  END Init;

(* Initializes the parameters for drawing shadows.  Sets "v.shadow" to
   "NIL" if shadows cannot or should not be used. *)
PROCEDURE InitShadow(v: T; st: VBT.ScreenType) =
  VAR
    light, dark: PaintOp.T;
    bgRGB, darkRGB: ScrnColorMap.RGB;
  BEGIN
    (* Make sure it is possible to paint shadows. *)
    IF NOT Shadows OR st = NIL OR st.depth <= 1 OR st.cmap = NIL THEN
      v.shadow := NIL;
      RETURN;
    END;

    TRY
      bgRGB := GetRGB(st, v.colors.bg);

      (* If we were given shadow colors, we use them.  Otherwise we calculate
         some reasonable ones.  The algorithm is borrowed from Tk. *)
      TYPECASE v.colors OF
      | Shadow.T(s) =>
          v.shadow := s;
          darkRGB := GetRGB(st, s.dark);
      ELSE
        (* For the dark shadow, reduce each of the background color
           components by 40%. *)
        darkRGB.r := 0.6 * bgRGB.r;
        darkRGB.g := 0.6 * bgRGB.g;
        darkRGB.b := 0.6 * bgRGB.b;
        dark := PaintOp.FromRGB(darkRGB.r, darkRGB.g, darkRGB.b,
          mode := PaintOp.Mode.Accurate, bw := PaintOp.BW.UseFg);

        (* For the light shadow, boost each background color component
           by 40% or halfway to white, whichever is greater. *)
        light := PaintOp.FromRGB(
          MAX(MIN(1.4 * bgRGB.r, 1.0), (bgRGB.r + 1.0) / 2.0),
          MAX(MIN(1.4 * bgRGB.g, 1.0), (bgRGB.g + 1.0) / 2.0),
          MAX(MIN(1.4 * bgRGB.b, 1.0), (bgRGB.b + 1.0) / 2.0),
          mode := PaintOp.Mode.Accurate, bw := PaintOp.BW.UseFg);

        v.shadow := Shadow.New(
          bg := v.colors.bg,
          fg := v.colors.fg,
          light := light,
          dark := dark);
      END;
    EXCEPT TrestleComm.Failure =>
      v.shadow := NIL;
      RETURN;
    END;

    IF NOT Shadow.Supported(v.shadow, v) THEN
      v.shadow := NIL;
      RETURN;
    END;

    v.shadowPixels := ROUND(VBT.MMToPixels(v, v.shadow.size, v.axis));
    IF v.shadowPixels <= 0 THEN
      v.shadow := NIL;
      RETURN;
    END;

    (* Make the trough color components slightly darker than the
       background -- 25% of the way from the background color
       to the dark shadow. *)
    v.troughColor := PaintOp.FromRGB(
      (3.0 * bgRGB.r + darkRGB.r) / 4.0,
      (3.0 * bgRGB.g + darkRGB.g) / 4.0,
      (3.0 * bgRGB.b + darkRGB.b) / 4.0,
      mode := PaintOp.Mode.Accurate, bw := PaintOp.BW.UseBg);
  END InitShadow;

(* Get the RGB components of a tint. *)
PROCEDURE GetRGB(st: VBT.ScreenType; op: PaintOp.T): ScrnColorMap.RGB
  RAISES {TrestleComm.Failure} =
  VAR
    ent: ARRAY [0..0] OF ScrnColorMap.Entry;
  BEGIN
    ent[0].pix := Palette.ResolveOp(st, op).pix;
    st.cmap.standard().read(ent);
    RETURN ent[0].rgb;
  END GetRGB;

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
    RETURN a;
  END GetAttributes;

PROCEDURE SetAttributes (v: T; READONLY a: Attributes) =
  <* LL.sup = VBT.mu *>
  BEGIN
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
  (* LL = mu. *)
  BEGIN
    IF v.shadow # NIL THEN
      PaintViewWithShadows(v);
    ELSE
      PaintViewAsBefore(v);
    END;
  END PaintView;

PROCEDURE PaintViewWithShadows (v: T) =
  VAR
    dom   : Rect.T;
    stripe: Rect.T;
    r     : Rect.T;
    f     : Rect.Partition;
  BEGIN
    dom := VBT.Domain(v);
    stripe := ComputeStripe(v, dom);

    (* Paint the scroll.  We are careful not to draw the area of the
       trough that will be covered by the stripe.  This helps reduce
       the flicker. *)
    r := Rect.Inset(dom, v.shadowPixels);
    ShadowPaint.Border(v, Region.Full, v.shadow, Shadow.Style.Lowered,
      r, dom);
    Rect.Factor(r, stripe, f, 0, 0);
    FOR i := FIRST(f) TO LAST(f) DO
      IF i # 2 AND NOT Rect.IsEmpty(f[i]) THEN
        VBT.PaintTint(v, f[i], v.troughColor);
      END;
    END;

    (* Paint the stripe. *)
    r := Rect.Inset(stripe, v.shadowPixels);
    ShadowPaint.Border(v, Region.Full, v.shadow, Shadow.Style.Raised,
      r, stripe);
    VBT.PaintTint(v, r, v.shadow.bg);
  END PaintViewWithShadows;

(*  
PROCEDURE PaintViewWithShadows (v: T) =
  VAR
    dom   : Rect.T;
    stripe: Rect.T;
  BEGIN
    dom := VBT.Domain(v);
    stripe := ComputeStripe(v, dom);
    
    (* paint scroll *)
    PaintShadow(v, dom, inwards := TRUE, raised := FALSE);
    
    (* paint stripe *)
    PaintShadow(v, stripe, inwards := FALSE, raised := TRUE);
  END PaintViewWithShadows;

PROCEDURE PaintShadow (         v                : T;
                       READONLY rect             : Rect.T;
                                inwards          : BOOLEAN;
                                raised           : BOOLEAN ) =
  VAR
    top, bottom, front: PaintOp.T;
    in, out: Rect.T;
  BEGIN
    IF (inwards) THEN
      out := rect;
      in := rect;
      INC(in.west, ShadowSize);
      DEC(in.east, ShadowSize);
      INC(in.north, ShadowSize);
      DEC(in.south, ShadowSize);
    ELSE
      in := rect;
      out := rect;
      DEC(out.west, ShadowSize);
      INC(out.east, ShadowSize);
      DEC(out.north, ShadowSize);
      INC(out.south, ShadowSize);
    END;
    IF (raised) THEN
      top := ScrollShadow.light;
      bottom := ScrollShadow.dark;
      front := ScrollShadow.fg;
    ELSE
      top := ScrollShadow.dark;
      bottom := ScrollShadow.light;
      front := ScrollShadow.bg;
    END;
    VBT.PaintTint(v,
                  Rect.FromEdges(out.west, in.west, out.north, out.south), top);
    VBT.PaintTint(v,
                  Rect.FromEdges(in.west, out.east, out.north, in.north), top);
    VBT.PaintTrapezoid(v, Rect.Full, Trapezoid.FromTriangle(
                                         Rect.NorthEast(in), Rect.NorthEast(out),
                                         Point.T{out.east, in.north}), bottom);
    VBT.PaintTint(v,
                  Rect.FromEdges(in.east, out.east, in.north, out.south), bottom);
    VBT.PaintTrapezoid(v, Rect.Full, Trapezoid.FromEdges(
                                         in.south, in.west, in.east, out.south,
                                         out.west, in.east), bottom);
    VBT.PaintTexture(v, in, front, Pixmap.Solid);
  END PaintShadow;
*)  
  
PROCEDURE PaintViewAsBefore (v: T) =
  VAR
    dom:    Rect.T;
    stripe: Rect.T;
    parts:  Rect.Partition;
  BEGIN
    dom    := VBT.Domain(v);
    stripe := ComputeStripe(v, dom);
    Rect.Factor(dom, stripe, parts, 0, 0);

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

      (* paint scroll texture in four pieces *)
    WITH textureP = v.scrollTextureP[v.axis],
	 texture  = v.scrollTexture [v.axis] DO
      VBT.PaintTexture(v, parts[0], textureP, texture);
      VBT.PaintTexture(v, parts[1], textureP, texture);
      VBT.PaintTexture(v, parts[3], textureP, texture);
      VBT.PaintTexture(v, parts[4], textureP, texture);
    END;

  END PaintViewAsBefore;


PROCEDURE ComputeStripe (v: T; r: Rect.T): Rect.T =
  (* LL = mu. Returns the domain of the white part of the stripe,
     including its shadows, if any. *)
  VAR
    lo, hi             : INTEGER;
    top, bottom, height: INTEGER;
    factor             : REAL;
  BEGIN
    (* Make the axis vertical for simplicity.  We'll change it back
       at the end. *)
    r := Rect.Transpose(r, Axis.Other[v.axis]);

    IF v.shadow # NIL THEN  (* Adjust for the trough shadows. *)
      INC(r.west, v.shadowPixels);
      DEC(r.east, v.shadowPixels);
      r.east := MIN(r.east, r.west + v.stripeWidth.pixels + 2*v.shadowPixels);
      INC(r.north, v.shadowPixels);
      DEC(r.south, v.shadowPixels);
    ELSE                    (* Adjust for the scrollMargin *)
      INC(r.west, v.scrollMargin.pixels);
      DEC(r.east, v.scrollMargin.pixels);
      r.east := MIN(r.east, r.west + v.stripeWidth.pixels);
    END;

    IF v.end - v.start < v.length THEN
      lo := r.north;
      hi := r.south;
      height := hi - lo;
      factor := FLOAT (height) / FLOAT (v.length);
      top := lo + ROUND (FLOAT (v.start) * factor);
      IF v.end >= v.length THEN
        bottom := hi;
      ELSE
        bottom := MIN (hi, top + ROUND (FLOAT (v.end - v.start) * factor));
      END;
      hi := MIN (hi, MAX (bottom, top + v.minStripeLen.pixels));
      lo := MAX (lo, MIN (top, hi - v.minStripeLen.pixels));
      r.north := lo;
      r.south := hi;
    END;

    r := Rect.Transpose(r, Axis.Other[v.axis]);
    RETURN r;
  END ComputeStripe;

PROCEDURE PaintBorder (         v         : VBT.T;
                       READONLY r         : Rect.T;
                                textureP  : PaintOp.T;
                                texture   : Pixmap.T;
                                w, e, n, s: INTEGER    ) =
  (* LL = mu. *)
  VAR sr: Rect.T;
  BEGIN
    sr.west := r.west + w;
    sr.east := r.east - e;
    sr.north := r.north;
    sr.south := r.north + n;
    VBT.PaintTexture(v, sr, textureP, texture, Point.Origin); (* top *)
    sr.north := r.south - s;
    sr.south := r.south;
    VBT.PaintTexture(v, sr, textureP, texture, Point.Origin); (* bottom *)
    sr.north := r.north;
    sr.west := r.west;
    sr.east := sr.west + w;
    VBT.PaintTexture(v, sr, textureP, texture, Point.Origin); (* left *)
    sr.west := r.east - e;
    sr.east := r.east;
    VBT.PaintTexture(v, sr, textureP, texture, Point.Origin); (* right *)
  END PaintBorder;


(********************)
(* Scroller methods *)
(********************)

PROCEDURE Mouse (v: T; READONLY cd: VBT.MouseRec) RAISES {} =
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      v.ready := TRUE;
      FirstDown(v, cd);
      VBT.SetCage(v, VBT.CageFromPosition(cd.cp, TRUE));
    ELSIF v.ready THEN
      AutoRepeat.Stop(v.repeater);
      v.ready := FALSE;
      IF (cd.clickType = VBT.ClickType.LastUp) AND (NOT cd.cp.offScreen)
        THEN
        FirstUp(v, cd);
      ELSE
        CancelPendingMouse(v, cd);
      END;
      VBT.SetCage(v, VBT.EverywhereCage);
    END;
  END Mouse;

PROCEDURE FirstDown (v: T; READONLY cd: VBT.MouseRec) =
  VAR
    perform: BOOLEAN;
    action : Action;
  BEGIN
    perform := FALSE;
    LOCK v.mu DO
      v.event := cd;
      v.eventLoc := cd.cp.pt;
      CASE cd.whatChanged OF

      | VBT.Modifier.MouseM =>
          perform := TRUE;
          action.type := ActionType.Thumb;
          action.cd := cd;
          GetPartHeight(v, action.part, action.height);
          v.state := State.ThumbState;

      | VBT.Modifier.MouseR, VBT.Modifier.MouseL =>
          IF cd.whatChanged = VBT.Modifier.MouseL THEN
            v.state := State.JumpEOFState;
          ELSE
            v.state := State.JumpSOFState
          END;
          AutoRepeat.Start(v.repeater);

      | VBT.Modifier.Mouse0..VBT.Modifier.Mouse4 =>
          (* The wheels on scroller mice generate these events.  We
             ignore them for now. *)

      ELSE <* ASSERT FALSE *>
      END;
      VBT.SetCursor(v, Cursors[v.state, v.axis]);
    END (* LOCK v.mu *);
    IF perform THEN PerformAction(v, action) END;
  END FirstDown;

PROCEDURE FirstUp (v: T; READONLY cd: VBT.MouseRec) =
  VAR perform: BOOLEAN; action: Action;
  BEGIN
    perform := FALSE;
    LOCK v.mu DO
      v.event := cd;
      IF (v.state = State.JumpSOFState) OR (v.state = State.JumpEOFState)
        THEN
        perform := TRUE;
        action.type := ActionType.Scroll;
        action.cd := cd;
        GetPartHeight (v, action.part, action.height);
        action.towardsEOF := (v.state = State.JumpEOFState);
      END;
      v.state := State.ListenState;
      VBT.SetCursor (v, Cursors[State.ListenState, v.axis]);
    END(* LOCK v.mu *);
    IF perform THEN PerformAction (v, action) END;
  END FirstUp;

PROCEDURE CancelPendingMouse (v: T; <* UNUSED *> READONLY cd: VBT.MouseRec) =
  BEGIN
    LOCK v.mu DO
      v.state := State.ListenState;
      VBT.SetCursor (v, Cursors[State.ListenState, v.axis]);
    END;
  END CancelPendingMouse;

PROCEDURE Position (v: T; READONLY cd: VBT.PositionRec) RAISES {} =
  BEGIN
    IF NOT v.ready THEN RETURN END;
    VBT.SetCage(v, VBT.CageFromPosition(cd.cp, TRUE));
    IF cd.cp.offScreen THEN
      (* VBT.SetCursor(v, Cursor.DontCare); *)
      RETURN
    END;
    VAR perform := FALSE; action: Action; prev := v.event.cp; BEGIN
      LOCK v.mu DO
        v.event.cp := cd.cp;
        v.event.time := cd.time;
        v.event.modifiers := cd.modifiers;
        CASE v.state OF

        | State.JumpEOFState, State.JumpSOFState, State.ContEOFState,
            State.ContSOFState =>
            IF ABS(DeltaPoints(v.axis, v.eventLoc, cd.cp.pt)) > 3 THEN
              v.state := State.PropState;
              AutoRepeat.Stop(v.repeater);
            END;

        | State.PropState =>
            WITH deltaV = DeltaPoints(v.axis, cd.cp.pt, prev.pt) DO
              IF deltaV # 0 THEN
                perform := TRUE;
                action.type := ActionType.AutoScroll;
                action.towardsEOF := (deltaV > 0);
                action.linesToScroll := ABS(deltaV);
              END
            END;

        | State.ThumbState =>
            perform := TRUE;
            action.type := ActionType.Thumb;
            action.cd := v.event;
            GetPartHeight(v, action.part, action.height);

        ELSE <* ASSERT FALSE *>
        
        END;

        VBT.SetCursor(v, Cursors[v.state, v.axis])
      END (* LOCK v.mu *);
      IF perform THEN PerformAction(v, action) END
    END
  END Position;

PROCEDURE GetPartHeight (v: T; VAR (* out*) part, height: INTEGER) =
  (* LL = mu *)
  VAR dom := VBT.Domain(v);
  BEGIN
    part := DeltaPoints(v.axis, v.event.cp.pt, Rect.NorthWest(dom));
    height := Rect.Size(v.axis, dom);
  END GetPartHeight;

PROCEDURE DeltaPoints (axis: Axis.T; READONLY p1, p2: Point.T): INTEGER =
  BEGIN
    IF axis = Axis.T.Hor THEN
      RETURN p1.h - p2.h;
    ELSE
      RETURN p1.v - p2.v;
    END;
  END DeltaPoints;

PROCEDURE PerformAction (v: T; READONLY action: Action) =
  BEGIN
    CASE action.type OF
    | ActionType.Scroll =>
        v.scroll(action.cd, action.part, action.height, action.towardsEOF);
    | ActionType.AutoScroll =>
        v.autoScroll(action.cd, action.linesToScroll, action.towardsEOF);
    | ActionType.Thumb => v.thumb(action.cd, action.part, action.height);
    END;
  END PerformAction;

PROCEDURE Repeat (ar: AutoRepeater) =
  VAR
    action: Action;
    v              := ar.v;
  BEGIN
    LOCK v.mu DO
      IF v.state = State.JumpEOFState THEN
        v.state := State.ContEOFState
      ELSIF v.state = State.JumpSOFState THEN
        v.state := State.ContSOFState
      END;
      IF v.state # State.ContEOFState
           AND v.state # State.ContSOFState THEN
        AutoRepeat.Stop(ar);
        RETURN
      END;
      v.event.time := 0;
      action.type := ActionType.AutoScroll;
      action.cd := v.event;
      action.linesToScroll := 1;
      action.towardsEOF := (v.state = State.ContEOFState);
    END;
    LOCK VBT.mu DO PerformAction (v, action) END;
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

PROCEDURE Rescreen (         v : T;
                    READONLY cd: VBT.RescreenRec)
  RAISES {} =
  BEGIN
    InitShadow(v, cd.st);

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

    v.minStripeLen.pixels :=
      ROUND(VBT.MMToPixels(v, v.minStripeLen.millimeters, v.axis));
    VBT.NewShape(v);
  END Rescreen;

PROCEDURE Shape (v: T; ax: Axis.T; <* UNUSED *> n: CARDINAL):
  VBT.SizeRange =
  VAR shape: ARRAY Axis.T OF VBT.SizeRange;
  BEGIN
    WITH otherAxis = Axis.Other[v.axis] DO
      shape[v.axis].lo := v.minStripeLen.pixels;
      shape[otherAxis].lo := v.stripeWidth.pixels;
      IF v.shadow # NIL THEN
        INC(shape[v.axis].lo, 4 * v.shadowPixels);
        INC(shape[otherAxis].lo, 4 * v.shadowPixels);
      ELSE
        INC(shape[otherAxis].lo, 2 * v.scrollMargin.pixels);
      END;

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

CONST
  DefaultScrollMargin = 1.0;
  DefaultStripeWidth  = 2.0;
  DefaultMinStripeLen = 4.0;

VAR
  globalLock: MUTEX := NEW (MUTEX);
  graphicsInited: BOOLEAN := FALSE;
  Cursors: ARRAY State, Axis.T OF Cursor.T;
  ScrollPixmap: ARRAY Axis.T OF Pixmap.T;
  
PROCEDURE InitGraphics () =
  BEGIN
    LOCK globalLock DO
      IF graphicsInited THEN RETURN END;
      graphicsInited := TRUE;

      WITH a = Axis.T.Hor DO
        XCLoad(State.ThumbState, a, "XC_sb_up_arrow");
        XCLoad(State.ListenState, a, "XC_sb_h_double_arrow");
        XCLoad(State.JumpEOFState, a, "XC_sb_left_arrow");
        XCLoad(State.JumpSOFState, a, "XC_sb_right_arrow");
        Cursors[State.ContEOFState, a] :=
          Cursors[State.JumpEOFState, a];
        Cursors[State.ContSOFState, a] :=
          Cursors[State.JumpSOFState, a];
        Cursors[State.PropState, a] :=
          Cursors[State.ListenState, a];
        ScrollPixmap[a] :=
          VBTKitResources.GetPixmap("IvyScrollH");
      END;

      WITH a = Axis.T.Ver DO
        XCLoad(State.ThumbState, a, "XC_sb_right_arrow");
        XCLoad(State.ListenState, a, "XC_sb_v_double_arrow");
        XCLoad(State.ListenState, a, "XC_sb_v_double_arrow");
        XCLoad(State.JumpEOFState, a, "XC_sb_up_arrow");
        XCLoad(State.JumpSOFState, a, "XC_sb_down_arrow");
        Cursors[State.ContEOFState, a] :=
          Cursors[State.JumpEOFState, a];
        Cursors[State.ContSOFState, a] :=
          Cursors[State.JumpSOFState, a];
        Cursors[State.PropState, a] :=
          Cursors[State.ListenState, a];
        ScrollPixmap[a] :=
          VBTKitResources.GetPixmap("IvyScrollV");
      END;

    END;
  END InitGraphics;

PROCEDURE XCLoad (state: State; axis: Axis.T; cursor: TEXT) =
  BEGIN
    Cursors[state, axis] := Cursor.FromName(ARRAY OF TEXT{cursor});
  END XCLoad;

BEGIN
END ScrollerVBTClass.

