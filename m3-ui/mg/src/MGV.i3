(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Tue Jun 22 09:45:04 PDT 1993 by steveg   *)
(*      modified on Tue Jul 21 16:31:36 PDT 1992 by harrison *)
(*      modified on Thu Jul  9 18:40:17 1992 by mhb      *)

INTERFACE MGV;

<* PRAGMA LL *>

IMPORT Axis, IntRefTbl, R2, Rect, Region, Thread, VBT;
  
TYPE
  V <: VPublic;
  VPublic =
    VBT.Leaf OBJECT
      mu: MUTEX;                <* LL: VBT.mu < Z.mu < v.mu < v *>
      (* At the MG level, VBT.mu may or may not be held, Z.mu is normally
         held, v.mu is held or is acquired, v is not held.

         v.mu protects all fields listed below, and all elements associated
         with v *)

      <* LL = self.mu *>

      border := ARRAY Axis.T OF REAL{10.0, 10.0};
      (* a border around the displayed graphics *)

      animations: AnimateGroup := NIL;
      (* the list of pending animations.  Use self.animations.add to
         add a pending animation.  Operations that produce animations
         must call Animate at some point to activate the animations.
       *)

      nonOverlappingElements: BOOLEAN := FALSE;
      (* nonOverlappingElements is an efficiency hint to procedures which
         alter the visual state of MG elements.

         If there are overlapping elements, then the region
         corresponding to the changed element is added to the
         dirty region and the window is marked for repainting.

         If there are no overlapping elements, then the changed
         element can be repainted immediately.
      *)

      view: VBT.T; (* View.T *)
      (* the view containing self.  Needed for feedback events *)

      selector: Selector := NIL;
      (* self.selector.select is called when a mouse click is received by
         self.mouse.  A NIL selector defaults to closestSelector. *)

      selectee: Selectee := NIL;
      (* self.selectee.select is called with the result of
         self.selector.select when a mouse click is received.  A NIL
         selectee defaults to a selectee that does nothing. *)

      (* The mouse/selector/selectee methods combine to define the
         selection actions on a V.  The mouse method normally just
         translates the mouse point from screen coordinates to MG
         coordinates.

         selector.select does low-level, element oriented selection.  It is
         called with v.mu locked and finds the element(s) that are
         selected.  The default selector returns the closest element, but
         other selectors could return the closest group, a group of the
         elements within some distance, the group containing the closest
         ellipse, etc.

         selectee.select handles the higher-level actions of the selection.
         It is called with whatever elements the selector returns.  It is
         normally view specific and handles the highlight actions
         associated with the selection and the input events to the
         algorithm.  The default selectee does nothing. *)

      doubleBuffer: BOOLEAN := TRUE;

      displayList: MGGroup;
      (* all elements in the displayList will paint on V's screen *)

      lookup: IntRefTbl.T;
      (* The table mapping element IDs provided by the algorithm to
         elements in the view. *)

      (* READONLY except at initialization or through methods *)
      nw := R2.Origin;
      (* nw is the offset of the north west corner of the window relative
         to the MG origin *)

      dirtyRegion := Rect.Empty;
      (* Internal bookkeeping field

         Any action on an element in the display list, that could leave a
         portion of the screen out of date, should add that area to the
         dirtyRegion.  During Repaint and Redisplay that area will get
         repainted. *)

      paintVBT: VBT.Leaf;
      (* this is the VBT that painting actions are done on (for double
         buffering) *)

      shapeVBT: VBT.Leaf;
      (* this is the VBT that shape painting is done on (for path
         rendering) *)
    METHODS
      <* LL < self.mu *>
      init (): V;               (* allocates empty displayList *)

      <* LL = VBT.mu *>
      setDoubleBuffer (yes: BOOLEAN);

      <* LL <=VBT.mu *>
      mgRedisplay(READONLY br: Region.T);
      (* Similar to the VBT redisplay method, except that the
         LL is more liberal - can be called without VBT.mu *)

      <* LL = self.mu *>
      setNW       (nw := R2.Origin);
      setSelector (selector: Selector := NIL);
      (* sets self.selector to selectorClosest if "selector" = NIL *)
      setSelectee (selectee: Selectee := NIL);
      (* sets self.selectee to a default selectee if "selectee" = NIL *)
    END;

TYPE
  Selector <: SelectorPublic;
  SelectorPublic = OBJECT
    METHODS
      <* LL = {VBT.mu, Z.mu, v.mu *>
      select(v: V; READONLY pos: R2.T; READONLY cd: VBT.MouseRec): MGT;
      (* v.selector.select is called synchronously in VBT event time when a
         mouse event is received by v.  It can implement any policy it
         wishes to select and return an element (or none) *)
    END;

VAR
  selectorClosest: Selector;
  (* returns the first element (in top down visual order) which contains
     "pos" in its bounding box or the element whose bounding box is closest
     to "pos" *)

(* ------------------------------------------------------------------
    See the TypeSelector interface for generating some types of selectors 
------------------------------------------------------------------ *)

TYPE
  Selectee <: SelecteePublic;
  SelecteePublic = OBJECT
    METHODS
      <* LL = {VBT.mu, Z.mu} *>
      select(v: V; t: MGT; READONLY cd: VBT.MouseRec);
      (*  v.selectee.select is called synchronously in VBT event time with the
         result of calling v.selector.select.  It may perform any action
         on t or any other element.  
      *)
    END;

(*| Here are 2 potential Shape methods.  ShapeStretchy is the default 
    for an MGV.T

   ShapeFixed returns a size range of {pref, pref, pref + 1}

   ShapeStretchy returns a size range of 
     {0, pref, MAX(pref + 1, VBT.DefaultShape.hi)}
 *)
PROCEDURE ShapeFixed(v: V; ax: Axis.T; n: CARDINAL): VBT.SizeRange;
PROCEDURE ShapeStretchy(v: V; ax: Axis.T; n: CARDINAL): VBT.SizeRange;

(* Here are 2 potential Reshape methods.  ReshapeSWOrigin is the 
   default for an MGV.T.

   ReshapeSWOrigin moves the MG graphical origin to the south west
   corner of "v".

   ReshapeLeaveOrigin leaves the MG graphical origin alone when
   "v" is reshaped.  It leaves the v.nw value unchanged so
   that views that display themselves relative to the NW corner
   of the window or which set v.nw should use this reshape 
   method.
*)
PROCEDURE ReshapeSWOrigin(v: V; READONLY cd: VBT.ReshapeRec);
PROCEDURE ReshapeLeaveOrigin(v: V; READONLY cd: VBT.ReshapeRec);

PROCEDURE AddAnimation (v: V; animation: AnimateT; mg: MGT);
<* LL < v.mu *>
(* Add "animation" applied to "mg" to the group of animations
   pending on "v".  The animation may be "played" by calling
   Animation(v). *)

PROCEDURE AddAnimationLocked (v: V; animation: AnimateT; mg: MGT);
<* LL.sup = v.mu *>
(* Like AddAnimation, but locked. *)

PROCEDURE Animation(v: V; duration := 1.0) RAISES {Thread.Alerted};
<* LL < v.mu *>
(* Play the pending animations on "v" in duration seconds. If 
   "duration" = 0.0 then the animation will occur in a single
   update.  *)

(* the following definitions are for interface structural reasons *)
TYPE
  MGT <: REFANY; (* A MGT is actually an MG.T *)
  MGGroup <: REFANY; (* A MGGroup is actually an MG.Group *)
  AnimateT <: REFANY; (* An AnimateT is actually an Animate.T *)
  AnimateGroup <: REFANY; (* An AnimateGroup is actually an Animate.Group *)

END MGV.
