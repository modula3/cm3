(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "View" interface provides a rudimentary window to a shared board.
*)

INTERFACE View;

IMPORT VBT, Rect, Point,
       NetObj,
       Board, Item, ItemList, Focus, RectR, PointR;

TYPE T <: Public;
     Public = VBT.Leaf OBJECT
       reportFocus: PROCEDURE (READONLY focus: Focus.T) := NIL;
       reportError: PROCEDURE (msg: TEXT) := NIL;
     METHODS
       init (bd: Board.T): VBT.T RAISES {NetObj.Error};
       refresh (READONLY rect: Rect.T);
       quit ();
     END;

(* A "View.T", or a {\em view}, is a  window owned by a board client
   that displays the items in a rectangular portion of the board even 
   as the items are created, modified, or deleted by other clients. 
   A view also provides  rudimentary support to its owner client to
   create, modify or delete items. 

   The rectangular portion displayed by the view is called its {\em
   focus}. The view may have cached a larger portion of the board, so
   that changing the focus does not require communication with the
   board. The rectangular portion that the view has cached is called
   its {\em scope}, and the list of cached items is called its {\em
   display list}. 

   The method call "init (bd)" method connects the view to the board "bd".
   The "refresh" method is there so that it may be extended by
   subtypes of "View.T" to paint images in addition to the 
   view's display. (The "refresh" method of the subtype must call 
   "View.T.refresh" at some point.) For example, the "Win" interface uses it to
   paint the cursor. 

   It is good form to call the "quit" method before deleting a view.
   This breaks the connection with the board.

   The routines in this interface are divided into two groups. The
   first is for use by the owner client: to access the focus of the view,
   to `select' items, and to indicate that an item be created,
   modified, or deleted. However, a view does not provide the tools to
   carry out the selection, creation, modification, or deletion. 
   (That is left to "Win.T", a subtype of "View.T".) Thus, this
   interface is free from knowing the specific subtypes of "Item.T"
   being manipulated. 

   The second set of routines is for use by the callback object
   registered with the board. The callback object invokes the routines
   to manipulate the items in response to the notifications
   received from the board. (An alternative was to reuse the routines
   provided to the client for the same purpose, but that would
   complicate the interface as the two set of routines differ
   in their semantics and signatures.)

   All routines must be called with "VBT.mu locked. 
*)


<*INLINE*> PROCEDURE GetFocus (v: T): Focus.T;
(* Returns the attributes of the focus of the view. *)

PROCEDURE ChangeFocus (v: T; focus: Focus.T);
(* Sets the attributes of the focus of the view. *)

PROCEDURE ChangeOffset (v: T; offset: PointR.T);
(* Sets the attributes of the focus of the view. *)

PROCEDURE GetSelection (v: T): ItemList.T;
(* Returns the list of selected items. The list is read-only: it must
   not be modified. *)

PROCEDURE SelectItems (v: T; its: ItemList.T);
(* Causes the items "its" to be selected. Items selected
   earlier are unselected first.
*)

PROCEDURE SelectOne (v: T; pt: Point.T);
(* Causes an item whose bounding box covers point "pt" to be selected,
   if any. Items selected earlier are unselected first.
*)

PROCEDURE SetSelectionBox (v: T; box: Rect.T);
(* Causes items whose bouding boxes lie within "box" to be selected.
   The "box" is specified in the window coordinates. Items selected
   earlier are unselected first.
*)

PROCEDURE CreateItems  (v: T; its: Item.TArray);
(* The view informs the board server of the creation of "its",
   installs "its" in its display list, and updates the display.
*)

PROCEDURE ModifyItems (v: T; its: Item.TArray; additive: BOOLEAN; 
                      oldBox: RectR.T);
(* The view informs the board server, and updates its  display. 
*)

(* The view makes use of the last two arguments to update its display
   efficiently. 
   If "additive" is true, the modification is {\em additive} in that the new
   image of the item can be simply painted over the old one.
   For example, "additive" is true  when characters are added to a
   text item, or when its color changes, but false when its font changes.
   If "additive" is false, the view must unpaint the old image, and
   restore the image of any other item overlapping it. It restricts
   this restoration to "oldBox", which should be set to the join of
   the old bouding boxes  of the modified items. 
   However, if "additive" is true, "oldBox" is not used.

   If an item in "its" is not found in the display list, say
   because it was recently deleted by a callback notification, the
   slot in "its" is set to "NIL". This indicates to the calling
   procedure that the item it is trying to modify has been deleted.
*)

PROCEDURE DeleteItems (v: T; ids: Item.IDArray);
(* The view informs the board server of the deletion,
   removes the items from its display list, and updates the display.
*)


(* The following routines are invoked by the callback object. *)

PROCEDURE ItemsCreated (v: T; its: Item.TArray);
(* The view installs "its" in its display list, and updates the
   display. 
*)

PROCEDURE ItemsModified (v: T; its: Item.TArray; additive: BOOLEAN);
(* The view overwrites the item installed in its display list with the
   value provided. 
*)

(* The view uses "additive" and the old bouding box of
   the item (from the existing copy in the display list) in the same way
   as for the "ModifyItem" routine. If the item is not indisplay list,
   the call is ignored.
*)

PROCEDURE ItemsDeleted (v: T; ids: Item.IDArray);
(* The view removes the item from its display list, and updates the display.
*)


END View.
