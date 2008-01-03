(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "Win" interface provides the data types and routines for a
   client window to carry out changes in focus, and the manipulation
   of specific kinds of items.
*)

INTERFACE Win;

IMPORT VBT, Color,
       View;

TYPE T <: Public;
  Public = View.T;

(* A "Win.T" is a "View.T" equipped with routines that trigger
   {\em activities} in the display.
   Some activities merely change the window's focus on the board,
   while others manipulate items.
   (The implementation of "Win.T" crafts its "position" and "key"
   methods as required by these activities.
   However, it leaves the "mouse" method unchanged.
   Thus, a client program is free to develop a user interface of its choice
   to trigger the activities.
   Typically, "Win.T" is further subtyped to override its "mouse"
   method with one that invokes different routines in response to
   different kinds of mouse clicks.) 

   There are two kinds of activities: {\em actions} and {\em
   events}. Actions have a distinct beginning and an end, such as
   typing into the window, or panning the window's focus on the board.
   On the other hand, events are single-stroke happenings, such as
   deleting an item, or pasting some text. When the client initiates 
   a new action, the current action, if any, is automatically terminated. 

   \subsection{Actions}
   At any time, the {\em status} of the window describes the current
   action (or the absence of any action). 
*)

TYPE Status = {Nothing, Selecting, Moving, Typing, Ruling,
               Panning, Dragging, Magnifying, Reducing};

PROCEDURE GetStatus (wn: T): Status;
(* Returns the status of the window. *)

(* The actions are expected to be triggered on mouse clicks, and most
   routines implementing actions take a "VBT.MouseRec" as a parameter.
*)

PROCEDURE Nothing (wn: T);
(* Terminates current action. *)

PROCEDURE Typing (wn: T; READONLY cd: VBT.MouseRec);
(* Invites the user to type text into the window. *)

(* A cursor appears at the position of the pointer (given in "cd").
   Graphic characters typed by the user appear on the window. 
   The Backspace/Delete key may be used to backspace characters upto
   the beginning of the line.
   Upon a carriage return, the cursor moves to the left margin, a line
   below. The left margin is determined by the horizontal coordinate
   of the the pointer position when the action began.
   Each line begins a new item. 

   If the item being typed is deleted by another client, the next
   keystroke will begin a new item. The cursor position and the left
   margin remain unchanged.
*)

PROCEDURE Ruling (wn: T; READONLY cd: VBT.MouseRec);
(* Invites the user to draw a rule. *)

(* A highlighted rectangle marks the boundary of the rule.
   The given pointer position makes one corner, and the current
   pointer position make the other. When the action ends, the
   rectangle is frozen and painted.
*)


PROCEDURE Selecting (wn: T; READONLY cd: VBT.MouseRec);
(* Invites the user to select items. *)

(* The given pointer position makes one corner
   of a rectangle that is used to select the items. While the action
   is in progress, the current pointer position makes the other corner
   of the rectangle. All items whose bounding boxes lie within the
   rectangle are selected. The rectangle is visible as a highlighted
   boundary, and the selected items are highlighted. The set of
   selected items changes as the user changes the rectangle. At the
   end of the action, all selected items remain so.

   If there were selected items when the action began, they are unselected. 
*)

PROCEDURE Dragging (wn: T; READONLY cd: VBT.MouseRec);
(* Shifts the window's focus on the board. *)

(* The window's focus shifts such that the focus-point below the pointer
   remains fixed below it even as the pointer is moved. For example,
   if the pointer is moved towards the top-right, the items visible in
   the window seem to move the same distance in the same direction.
*)

PROCEDURE Panning (wn: T; READONLY cd: VBT.MouseRec);
(* Pans  the window's focus on the board. *)

(* Same as "Dragging" except that the items seem to move in the
   opposite direction as the pointer. This gives the impression that
   the window is being panned over the board.
*)


PROCEDURE Moving (wn: T; READONLY cd: VBT.MouseRec);
(* Moves the selected Items. *)

(* A highlighted rectangle marks the bounding box of all  selected
   items, and it moves in the same direction and amount as the pointer.
   When the action terminates, all selected items are moved to the new
   position marked by the rectangle.
*)

PROCEDURE Magnifying (wn: T; READONLY cd: VBT.MouseRec); 
(* Magnifies  the window's focus on the board. *)

(* The magnification occurs such that the focus-point below the initial
   pointer position remains fixed. Further movement of the pointer is
   not effectual. *)

PROCEDURE Reducing (wn: T; READONLY cd: VBT.MouseRec);
(* Reduces the window's focus on the board. *)

(* \subsection{Events} *)

PROCEDURE ChangeFont (wn: T; fontName: TEXT);
(* Changes the current font assocaited with the window. *)

(* Any text item created later acquires this font. 
   If this event is invoked when the "Typing" action is in progress,
   the current item is terminated, so that additional keystrokes will
   create a new item. The position of the curosr and the left margin
   remain unchanged by the event.
*)

PROCEDURE ChangeColor (wn: T; op: Color.T);
(* Changes the current color assocaited with the window. *)

(* Any item created later acquires this color. 
   If this event is invoked when the "Typing" action is in progress,
   the current item is terminated, so that additional keystrokes will
   create a new item. 
*)

PROCEDURE DiscardSelection  (wn: T);
(* Unselects the selected items. *)

PROCEDURE DeleteSelection (wn: T);
(* Deletes the selected items. *)

PROCEDURE PasteSource (wn: T; READONLY cd: VBT.MouseRec);
(* Pastes the text read from the source selection. *)

(* Text read from the source selection is painted with the reference
   point of the first line at the pointed position. The text may comprise
   multiple lines.
   The font and the color of the text is acquired from the current
   font and color associated with the window.
*)



PROCEDURE ChangeZoomRate (wn: T; rate: REAL);
(* Sets the zoom factor to "factor".*)

(* When magnifying, the scale is multiplied by "1+rate" every second. 
   step.
   When reducing, the scale is divided by "1+rate" every second. 
*)
 
PROCEDURE SelectItem (wn: T; READONLY cd: VBT.MouseRec);
(* Selects the item under the pointer, if any. *)

PROCEDURE Undo (wn: T);
(* Undoes the effect of last action that created or deleted items. *)

END Win.
