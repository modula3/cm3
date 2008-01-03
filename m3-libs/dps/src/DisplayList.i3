(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:18:30 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 18:02:43 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

INTERFACE DisplayList; 

(*  A DisplayList.T is a subclass of a Linked2Tree.T.  It defines
    a node in a tree of window-displayable entities, adding a suite
    of display-related methods to the basic Linked2Tree methods.

    A DisplayList.E is identical to a DisplayList.T, but is meant
    to define a "leaf" node in the tree.  [One could imagine that
    a DisplayList.E defined a basic node, and a DisplayList.T was an 
    E augmented with additional fields, but there are technical reasons
    involving the Modula-3 language that make it convenient to 
    actually equate them.]

    Typically, a DispalyList.T is subclassed to define particular 
    displayable entities: text regions, menus, command buttons, ...

    A DisplayList.T contains three data fields:

      The "box" field circumscribes the screen area that the 
      entity occupies.  The box does not have to narrowly define
      the area: DPS.EverywhereBOx is a valid value.  But if an
      implementation can tightly bound its area, that is a boon, 
      as it can minimize repainting.  Some subclasses
      use the box field to, e.g., arrange boxes in a column.
      Such subclasses require that their children supply accurate 
      box values.

      The "fixedPoint" field defines what point of the box
      doesn't move when the content is transformed. It is akin 
      to an X11 "gravity" declaration.

      The "childWithInputFocus" field is a convenience that
      allows clients to use a default input focus mechanism.  If
      non-NIL, it indicates the immediate child that has (or contains)
      the current input focus.

    A DisplayList.T has several methods:

      NewBox alters the value of the "box" field.  This is of potential
      interest to some parent nodes, as they may geometrically arrange
      their children.  So the default NewBox procedure calls the 
      method ..

      NewBoxOfChild if there is a parent.  Nodes that arrange their
      children can override this method to discover changes in their
      children. The default NewBoxOfChild just changes the "box" field
      and then marks the union of old and new boxes as being "dirty". 

        [NewBoxToParent is identical to NewBox.  It is an artifact.]

      Repaint is a standard repaint arrangement. It supplies a box, and
      demands that the client return a TEXT that is the PostScript 
      that will repaint the screen area. Nodes that have children 
      must themselves code the recursion: this allows nodes to affect
      the repainting of their children.

      Dirty allows a client to declare that part of the screen that
      he paints should be repainted.  It is typically called when the
      client's data changes.  Note that the standard way to repaint
      yourself is to call Dirty, knowing that yor Repaint method will
      be calle shortly.

        An important detail: the system promises that Repaint methods
        are *not* called from within a Dirty call.  So a client *can*
        call Dirty while holding important mutexes.

      ImmediatePostScript allows the client to directly paint the
      screen, outside of a Repaint call.  (Even here, the call is
      passed up through the display list hierarchy.)  Use of this
      method is discouraged (Dirty & Repaint are preferred), but 
      it can be valuable for things like blinking cursors.

      Mouse passes X11 mouse events to the client.  Mouse events
      are passed "up the tree": if the client doesn't hanle the event,
      but has children that might, he must call the Mouse methods of 
      the children.  (The default method, of course, does this.)
      The method returns a boolean saying whether the event was 
      handled or not.  The return value is important in calling children.

      Char passes X11 keyboard events to the client.  It is quite 
      similar to the Mose method.

      Key passes X11 raw keyboard events to the client.  Normally, 
      key events are converted to character events and the Char method 
      called.

      GetInputFocus acquires the input focus on behalf of this node,
      or on behalf of the indicated (argument) child.

      LoseInputFocus removes the input focus from this node if it
      has it.

      KillInputFocus removes the input focus regardless of who has it. *)


IMPORT DPS, DPSWindow, Fifo, Linked2Tree, Thread;

TYPE T = Linked2Tree.T OBJECT 
  box: DPS.Box := DPS.Box { DPS.Place{0.0,0.0}, DPS.Place{0.0,0.0} }; 
  fixedPoint: DPS.FixedPoint := DPS.FixedPoint.sw;
  childWithInputFocus: T := NIL;
 METHODS
  NewBox (box: DPS.Box) := NewBox;
    NewBoxToParent (box: DPS.Box) := NewBoxToParent;
    NewBoxOfChild (e: E; box: DPS.Box) := NewBoxOfChild;
  Repaint (box: DPS.Box; only: REFANY := NIL): TEXT := Repaint;
  Dirty (box: DPS.Box; only: T := NIL) := DirtyToParent;
  ImmediatePostScript (script: TEXT) := PostScriptToParent;
  Mouse (window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN := Mouse;
  Char (window: DPSWindow.T; char: CHAR): BOOLEAN := CharT;
  Key (window: DPSWindow.T; event: DPS.KeyEvent):= Key;
  GetInputFocus (e: T := NIL):= GetInputFocus;
  LoseInputFocus ():= LoseInputFocus;
  KillInputFocus ():= KillInputFocus;
  END;
TYPE E = T;
TYPE Dirt = DPS.Box;
TYPE R = T OBJECT (* "R" for "Root" *)
  window: DPSWindow.T;
  cleanThread: Thread.T := NIL;
  dirtyFifo: Fifo.T := NIL;
 OVERRIDES
  Char := CharR;
  Dirty := DirtyToWindow;
  ImmediatePostScript := PostScriptToWindow;
  END;

(* The first element in the list is the furthest from the user. *)
(* The list is painted in normal order and mouse-scanned in reverse order. *)

CONST BackgroundGray = 1.00;

PROCEDURE DirtyToParent (e: E; box: DPS.Box; only: T := NIL);
PROCEDURE DirtyToWindow (r: R; box: DPS.Box; only: T := NIL);

PROCEDURE PostScriptToParent (e: E; script: TEXT);
PROCEDURE PostScriptToWindow (r: R; script: TEXT);

PROCEDURE NewBox (t: T; box: DPS.Box);
PROCEDURE NewBoxToParent (e: E; box: DPS.Box);
PROCEDURE NewBoxOfChild (t: T; e: E; box: DPS.Box);

PROCEDURE Repaint (t: T; box: DPS.Box; only: REFANY := NIL): TEXT;
PROCEDURE Mouse (t: T; window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN;
PROCEDURE CharT (t: T; window: DPSWindow.T; char: CHAR): BOOLEAN;
PROCEDURE CharR (r: R; window: DPSWindow.T; char: CHAR): BOOLEAN;

PROCEDURE Key (t: T; window: DPSWindow.T; event: DPS.KeyEvent);

PROCEDURE GetInputFocus (t: T; e: E := NIL);
PROCEDURE LoseInputFocus (t: T);
PROCEDURE KillInputFocus (t: T); (* Should be applied to root dlist. *)

  END DisplayList.



