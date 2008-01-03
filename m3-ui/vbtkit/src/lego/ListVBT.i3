(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri May 17 10:56:05 PDT 1996 by mhb                          *)
(*      modified on Thu Jul  8 23:45:54 1993 by gnelson                      *)
(*      modified on Mon Jun 14 20:59:37 PDT 1993 by meehan                   *)
(*      modified on Tue Jun 16 13:08:44 PDT 1992 by muller                   *)
(*      modified on Wed Apr 22 16:38:03 PDT 1992 by birrell                  *)
<* PRAGMA LL                                                                 *>

(* A "ListVBT" defines a VBT class for displaying a list (or
   table) of items.  Each item is in a {\em cell}.  All cells are
   the same size.  They are displayed in a single vertical
   column, with a scrollbar. The location of the scrollbar is
   governed by the environment variable "SCROLLBARLOC", described
   in the "VBTKitEnv" interface.
 
   The "ListVBT" itself deals with the details of being a VBT,
   maintains a table that maps a cell-number to a cell-value, and
   maintains the {\em selection}, a distinguished subset of the
   cells.  It uses subsidiary objects to handle the details of
   what cells look like on the screen ("Painter"), and how the
   list responds to mouse clicks ("Selector").

   This interface contains basic versions of each of the
   subsidiary objects:

   \begin{itemize}

   \item "TextPainter", which treats cells' values as "TEXT" and
   paints them.

   \item "UniSelector", which maintains at most one selected
   cell, adjusted by mouse clicks.

   \item "MultiSelector", which uses mouse clicks for selection,
   but permits multiple cells to be selected.

   \end{itemize}

   The client can subclass these, or provide entirely different ones.
   A client that wishes to take actions in response to mouse clicks
   should subclass a "Selector". Similarly, a client that wishes to
   display objects other than text strings should subclass "Painter".

   \subsubsection{Locking levels}

   "ListVBT" is internally synchronized; it can safely be called
   from multiple threads.  All "ListVBT.T" methods have "LL.sup <
   list".  In addition, "VBT.mu < list" for any "list" of type
   "ListVBT.T".

   VBT methods call "Selector" methods with "LL.sup = VBT.mu".
   "Selector" methods are permitted to call "ListVBT.T" methods.

   "ListVBT.T" methods call "Painter" methods with the "ListVBT.T"'s
   internal mutex held.  "Painter" methods must not call any of the
   "ListVBT.T" methods; their locking level is such that "LL.sup =
   list".
   
   The "TextPainter" class uses its own internal lock for font
   information; \linebreak "TextPainter.setFont(v,font)" has "LL.sup < v".

   \subsubsection{The type {ListVBT.T}}
   *)


INTERFACE ListVBT;

IMPORT Font, PaintOp, Rect, VBT;

TYPE Cell = INTEGER;
(* The number of a cell; the first cell-number is 0. *)

TYPE
  T <: Public;
  Private <: VBT.Split;
  Public = Private OBJECT
             painter : Painter  := NIL;
             selector: Selector := NIL;
           METHODS
             init             (colors: PaintOp.ColorQuad): T;
             setValue         (this: Cell; value: REFANY);
             getValue         (this: Cell): REFANY;
             count            (): CARDINAL;
             insertCells      (at: Cell; n: CARDINAL);
             removeCells      (at: Cell; n: CARDINAL);
             selectNone       ();
             selectOnly       (this: Cell);
             select           (this: Cell; selected: BOOLEAN);
             isSelected       (this: Cell): BOOLEAN;
             getAllSelected   (): REF ARRAY OF Cell;
             getFirstSelected (VAR this: Cell): BOOLEAN;
             scrollTo         (this: Cell);
             scrollToShow     (this: Cell);
             reportVisible    (first: Cell; num: CARDINAL); 
           END;

(* In the following descriptions, "v" is an object of type
   "ListVBT.T", and a value "n" is said to be {\it in range} if

   \medskip {\display {\tt 0 $\leq$ n < v.count() }}

   \medskip "v.painter" is the list's painter; the client may read but not
   assign to this field, although the client may provide a value
   at allocation time.  If the actual painter has methods
   allowing it to be modified, the client is welcome to call
   them, although the client and painter are then responsible for
   provoking any necessary repaints.

   "v.selector" is the list's selector; client may read but not
   assign to this field, although the client may provide a value
   at allocation time.  If the actual selector has methods
   allowing it to be modified, the client is welcome to call
   them, although the client and selector are then responsible
   for any necessary adjustments to the set of selected cells.

   The call "v.init(colors)" initializes "v" as a "ListVBT" and
   returns "v".  It must be called before any other method.  "colors"
   is passed intact to the scroller; "colors.fg" is used for a bar
   that separates the cells from the scroller.  If "v.painter = NIL"
   when this method is called, "init" will allocate and initialize a
   "TextPainter".  If "v.selector = NIL", "init" will allocate and
   initialize a "UniSelector".  Neither the painter nor the selector
   need have been initialized before this method is called.  The list
   initially has no cells (and no selection).

   In the call "v.setValue(this,value)", if "this" is in range,
   then record "value" as the value of the cell "this"; otherwise
   do nothing.

   In the call "v.getValue(this)", if "this" is in range,
   then return the previously recorded value of the cell "this";
   otherwise return "NIL".

   The call "v.count()" returns the number of cells.

   The call "v.insertCells(at,n)" inserts "n" cells, starting at
| MAX (0, MIN (at, v.count()))
   Previously existing cells at and beyond "at" are renumbered
   appropriately, and selections are relocated appropriately.
   The VBT will be repainted in due course.  The new cells'
   values are all "NIL", and they are not selected.

   The call "v.removeCells(at, n)" removes all cells in the range
| [MAX (0, MIN (at, v.count ())) ..
|   -1 + MIN (at + n, v.count ())]
   Subsequent cells are renumbered appropriately.  The VBT will
   be repainted in due course.

   The call "v.selectNone()" makes the set of selected cells be
   empty.

   In the call "v.selectOnly(this)", if "this" is in range, make
   the set of selected cells be exactly "this"; otherwise make
   the list of selected cells be empty.  Equivalent to
| v.selectNone(); v.select(this,TRUE)

   In the call "v.select(this,selected)", if "this" is in range
   and "selected" is "TRUE", add "this" to the set of selected
   cells (without complaint if it's already selected); otherwise
   if "this" is in range and "selected" is "FALSE", remove it
   from the set of selected cells (again without complaint).
   The VBT will be repainted as necessary in due course.

   The call "v.isSelected(this)" returns "TRUE" if "this" is
   in range and is a selected cell; otherwise it returns "FALSE".

   The call "v.getAllSelected()" returns the set of selected cells.
   If there are none, it returns a non-"NIL" "REF" to an array of length
   0.

   The call "v.getFirstSelected(this)" assigns to "this" the
   lowest-numbered selected cell and returns "TRUE"; if there are
   no selected cells, it returns "FALSE".

   The call "v.scrollTo(this)" adjusts the list's scrolling
   position to place
| MAX (0, MIN (this, v.count () - 1) )
   at the top of "v"'s domain.

   The call "v.scrollToShow(this)" adjusts the list's scrolling
   position to make "this" visible. 
   
   The "ListVBT" will call "v.reportVisible(first, num)" whenever
   the set of visible cells changes (either because of scrolling
   or because of reshaping).  (A cell is ``visible'' if it
   is within the domain of the "ListVBT"; it may not be visible
   to the user if other windows obscure the "ListVBT".)  The
   argument "first" is the index of the first visible cell, and
   "num" is the number of visible cells.  The default for this
   method is a no-op; override it if you need the information
   it provides.  The locking level of the method is "LL.sup = v"
   (that is, the "ListVBT" itself is locked when the method is called,
   so the method mustn't operate on "v").
*)

(* \subsubsection{The Painter} *)

(* Here is the definition of a "Painter".  In the comments about
   its methods, "v" is the VBT in which the painting is to take
   place; it is the "ListVBT.T" or a subtype of it.  Recall
   that "LL.sup = list" for all methods, other than "init". *)

TYPE
  Painter = OBJECT
            METHODS
              init   (): Painter;
              height (v: VBT.T): INTEGER;
              paint (v       : VBT.T;
                     r       : Rect.T;
                     value   : REFANY;
                     index    : CARDINAL;
                     selected: BOOLEAN;
                     bad     : Rect.T   );
              select (v       : VBT.T;
                      r       : Rect.T;
                      value   : REFANY;
                      index   : CARDINAL;
                      selected: BOOLEAN );
              erase (v: VBT.T; r: Rect.T);
            END;

(* The call "p.init()" initializes "p" as a "Painter" and returns
   "p".

   The call "p.height(v)" returns the pixel height of each cell
   if painted in "v".  The list caches the result of this call,
   so it needn't be very efficient.  It is called only when the
   list has a non-empty domain.  It gets re-evaluated whenever
   the list's screen changes.

   The call "p.paint(v, r, value, index, select, bad)" paints the cell
   with the given index and value in the given rectangle (whose height 
   will equal that returned  by "p.height()", and some part of which will be 
   visible).  If "selected" is "TRUE", highlight the painted cell to indicate
   that it is in the set of selected cells.  "bad" is the subset
   of "r" that actually needs to be painted; "bad" is wholly
   contained in "r".

   The call "p.select(v, r, value, index, selected)" changes the 
   highlight of the cell with the given index and value,
   according to "selected", to show whether it is
   in the set of selected cells.  The cell has previously been
   painted; its selection state has indeed changed.  It's OK for
   this method to be identical to "paint", but it might be more
   efficient or cause less flicker, e.g.  by just inverting "r".

   The call "p.erase(v, r)" paints the given rectangle to show
   that it contains no cells.  Typically, this just fills it with
   the background color used when painting cells. *)

(* \subsubsection{TextPainter} *)

(* Perhaps the most common type of "Painter" is a "TextPainter".
   It displays cells whose values are text strings.  Here is its
   public definition: *)

TYPE
  TextPainter <: TextPainterPublic;

  TextPainterPublic =
    Painter OBJECT
    METHODS
      init (bg       := PaintOp.Bg;
            fg       := PaintOp.Fg;
            hiliteBg := PaintOp.Fg;
            hiliteFg := PaintOp.Bg;
            font     := Font.BuiltIn): TextPainter;
      setFont (v: VBT.T; font: Font.T); <* LL.sup < v *>
    END;

(* The call "p.init(...)" initializes "p" as a "TextPainter" and
   returns "p".  Unselected cells are painted with "fg" text on
   "bg"; selected cells are painted with "hiliteFg" text on
   "hiliteBg"; erased areas are painted with "bg".  Text is drawn
   using "font".

   After the call "p.setFont(v, font)", the "TextPainter" uses
   "font" for subsequent painting of values; the call also marks
   "v" for redisplay.  "v" should be the relevant "ListVBT.T". *)


(* \subsubsection{The Selector} *)

(* Here is the definition of "Selector".  Recall that "LL.sup =
   VBT.mu" for all methods other than "init". *)

TYPE
  Selector =
    OBJECT
    METHODS
      init         (v: T): Selector;
      insideClick  (READONLY cd: VBT.MouseRec; this: Cell);
      outsideClick (READONLY cd: VBT.MouseRec);
      insideDrag   (READONLY cd: VBT.PositionRec; this: Cell);
      outsideDrag  (READONLY cd: VBT.PositionRec);
    END;

(* The call "s.init(v)" initializes "s" as a "Selector" and
   returns "s".  The "ListVBT" "v" need not have been initialized
   before this method is called.

   The call "s.insideClick(cd, this)" is called on a "FirstDown" mouse
   click inside the cell, or on any mouse click inside the cell while
   we have the mouse focus.  On any click other than "LastUp", the
   list itself has set a cage so that it receives position reports
   during subsequent drags.

   The call "s.outsideClick(cd)" is called when there is a "FirstDown"
   click in the "ListVBT" that is not in a cell, or on any mouse click
   not in a cell while we have the mouse focus.  On any click other
   than "LastUp", the list itself has set a cage so that it receives
   position reports during subsequent drags.

   The call "s.insideDrag(cd)" is called if the list has received a
   "FirstDown" click and a subsequent position report with the mouse
   not in any cell.  The list itself has set a cage so that it
   receives further position reports.

   The call "s.outsideDrag(cd)" is called if the list has the mouse
   focus and receives a subsequent position report with the mouse in
   this cell.  The list itself has set a cage so that it receives
   further position reports. *)

(* \subsubsection{UniSelector and MultiSelector} *)

(* One common class of "Selector" is a "UniSelector".  It
   maintains the invariant that there is at most one selected
   cell.  On an "insideClick" firstDown, or an "insideDrag", it
   removes any previous selection and then selects this cell.  Its
   other methods do nothing.  Here is its declaration: *)

TYPE 
  UniSelector <: Selector;

(* The other common class of "Selector" is "MultiSelector".  It
   permits multiple cells to be selected.  On an "insideClick"
   firstDown, it remembers this cell as the {\em anchor}; if this is
   not a shift-click, it calls "selectNone" and inverts the
   selection state of this cell.  On an "insideDrag", it makes the
   selection state of all cells between this cell and the anchor
   be the same as that of the anchor.  Here is its
   declaration: *)

TYPE
  MultiSelector <: Selector;

END ListVBT.


