(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* ZSplit.i3, coded Fri Oct 31 11:24:53 1986 by cgn *)
(* Last modified on Fri Mar  5 19:59:33 PST 1993 by msm     *)
(*      modified on Mon Feb 24 13:55:25 PST 1992 by muller  *)
(*      modified on Wed Dec 11 18:29:58 PST 1991 by gnelson *)
(*      modified on Fri Feb  2 14:08:01 PST 1990 by glassman *)
<*PRAGMA LL*>

(* A "ZSplit.T" is a parent window with overlapping child windows. 
   
   Each child has a stacking order given (conceptually) by a "z"
   coordinate.  A pixel of the parent's screen that is in the domain
   of more than one child is controlled by whichever of these children
   is highest in the "z" coordinate.  The portions of the domains of
   the children that extend outside the parent domain will be clipped.
   
   "Split.Succ" enumerates the children from top to bottom.

   The bottom child is called the {\it background}.  An initial
   background can be specified when the "ZSplit" is created; usually
   it remains the background throughout the life of the "ZSplit".
   Usually the background has the same domain as the parent, and
   therefore controls all pixels that are not controlled by any other
   child.  In the unusual case that the background child has a domain
   different from the parent domain, there may be some parent pixels
   that are not controlled by any child.  The "ZSplit" will ignore these
   pixels when asked to repaint.
   
   The shape of a "ZSplit" is the shape of its background child (if
   it has no children its shape is the default shape for a "VBT").  When
   the preferred shape of a non-background child changes, the "ZSplit"
   reshapes the child to its new preferred shape, preserving its 
   {\it offset}, which is the vector between the northwest corners 
   of the parent and child.  *)
   
INTERFACE ZSplit;

IMPORT VBT, Rect, Split, Point;

TYPE 
  T <: Public;
  Private <: Split.T;
  Public = Private OBJECT METHODS
    <* LL <= VBT.mu *>
    init(bg: VBT.T := NIL;
      saveBits := FALSE;
      parlim: INTEGER := -1): T
  END;

(* The call "v.init(...)" initializes "v" as a "ZSplit". *)

(* It is only legal to call the "init" method for a newly-allocated
   "ZSplit" (as in the definition of the procedure "New" below) or from
   the "init" method of a subclass.  This restriction applies to all
   the "init" methods in Trestle, although it will not be repeated for
   each one. \index{init method, rules for calling}

  The "ZSplit" will be given the initial background child "bg" if
   "bg#NIL"; it will be given no children if "bg=NIL".  If "bg" is
   non-"NIL" it will be mapped initially.  If "saveBits" is "TRUE",
   the split will try to save the children's old bits when reformatting;
   if the children don't use them anyway, it is faster to let "saveBits"
   default to "FALSE".  The value of "parlim" is the minimum area of
   a child for which a separate thread will be forked to reshape or
   repaint it; if it is "-1", it is set to an appropriate default (see
   the "VBTTuning" interface).  *)


PROCEDURE New(
    bg: VBT.T := NIL;
    saveBits := FALSE;
    parlim: INTEGER := -1)
    : T; <* LL <= VBT.mu *>
(* "New(...)" is equivalent to "NEW(T).init(...)". *)

(* \subsubsection{Inserting children} *)

(* The default "Split.Insert" call is rarely useful for a "ZSplit": it
   inserts the new child at the parent's northwest corner, unmapped.
   "Split.AddChild" is even less useful, since it adds children as the
   background, which is almost certainly not what you want.  The 
   following procedures are more useful for inserting children into a 
   "ZSplit": *)

PROCEDURE InsertAfter(
    v: T;
    pred, ch: VBT.T;
    READONLY dom: Rect.T;
    alsoMap: BOOLEAN := TRUE) RAISES {Split.NotAChild};
<* LL.sup = VBT.mu *>
(* Insert "ch" as a new child of "v" with domain "dom", and mark "v"
   for redisplay.  *)
   
(* The new child is inserted immediately after (that is, below) "pred";
   if "pred=NIL" the new child is inserted first (that is, on top).
   If the height or width of "dom" does not satisfy "ch"'s size
   contraints, then the height and width of the child are projected
   into range; its offset is preserved.  This is a checked runtime error
   if "ch" is not detached.  If "alsoMap" is "TRUE", "ch" is mapped,
   otherwise it is unmapped. 
   
   It is occasionally useful to insert a new child below all existing 
   children except the background, in which case the following 
   procedure is handy:  *)

TYPE Altitude = {Top, Bot};

PROCEDURE Insert(
    v: T;
    ch: VBT.T;
    READONLY dom: Rect.T;
    alt := Altitude.Top;
    alsoMap: BOOLEAN := TRUE); <* LL.sup = VBT.mu *>
(* Insert "ch" at the top if "alt = Altitude.Top"; insert "ch" just
   above the background if "alt = Altitude.Bot".  *)
   
(* That is, "Insert" is equivalent to

    | IF alt = Altitude.Top THEN
    |   pred := NIL
    | ELSE
    |   pred := Split.Pred(v, Split.Pred(v, NIL))
    | END;
    | InsertAfter(v, pred, ch, dom, alsoMap)


   Finally, instead of providing the new child's domain it can be useful
   to provide only the northwest corner and let the child's domain be
   determined by its shape constraints: *)


PROCEDURE InsertAt(
    v: T;
    ch: VBT.T;
    nw: Point.T;
    alt := Altitude.Top;
    alsoMap: BOOLEAN := TRUE); <* LL.sup = VBT.mu *>
(*  Insert "ch" with its preferred shape and its northwest corner 
    at "nw".  The "alt" and "alsoMap" parameters are interpreted as 
    in "Insert". *)


(* \subsubsection{Moving, lifting, and lowering children} *)

PROCEDURE Move(ch: VBT.T; READONLY dom: Rect.T);
<* LL.sup = VBT.mu *>
(* Change the domain of "ch" to be "dom" and mark "ch"'s parent for
   redisplay.  *)
   
(* If the height or width of "dom" do not satisfy "ch"'s size
   constraints, then they are projected into range, preserving the
   northwest corner of "dom".  The stacking order of "ch" is not
   changed.  "Move" is a checked runtime error if "ch"'s parent is not
   a "ZSplit".  Note that this has nothing to do with "Split.Move", 
   unlike the next procedure. *)


PROCEDURE Lift(ch: VBT.T; alt := Altitude.Top); 
<* LL.sup = VBT.mu *>
(* Lift "ch" to the top or lower it to be just above the background,
   depending on "alt".  "Lift" is equivalent to:

| v := VBT.Parent(ch);
| IF alt = Altitude.Top THEN
|   pred := NIL
| ELSE
|   pred := Split.Pred(v, Split.Pred(v, NIL))
| END;
| Split.Move(v, pred, ch)

*)

(* \subsubsection{Mapping and unmapping children} *)

(* You can {\it unmap} a child of a "ZSplit", which reshapes the child
   to be empty after recording the child's shape and offset.  When you
   later {\it map} the child, the recorded shape and offset are
   restored.  An unmapped child is rescreened when the parent is
   rescreened, and its recorded shape and offset are updated when the
   parent is reshaped, just like the domains of the mapped children.
*)

PROCEDURE Unmap(ch: VBT.T); <* LL.sup = VBT.mu *>
(* If "ch" is mapped, unmap it and mark its parent for redisplay. *)

PROCEDURE Map(ch: VBT.T); <* LL.sup = VBT.mu *>
(* If "ch" is unmapped, map it and mark its parent for redisplay. *)

PROCEDURE IsMapped(ch: VBT.T): BOOLEAN;
<* LL.sup = VBT.mu *>
(* Return "TRUE" if ch is "mapped" and "FALSE" if "ch" is unmapped. *)

(* "Map", "Unmap", and "IsMapped" are checked runtime errors if "ch"'s
   parent is not a "ZSplit".  *)


(* \subsubsection{Getting domains} *)


PROCEDURE GetDomain(ch: VBT.T): Rect.T;
<* LL.sup = VBT.mu *>
(* Return the effective domain of "ch". *)

(* The effective domain is the same as the normal domain, except (1)
   if the parent has been marked for redisplay, "GetDomain" returns
   the domain that "ch" will receive when the redisplay happens, or
   (2) if the domain of the parent is "Rect.Empty", "GetDomain" returns
   the domain "ch" would receive if the parent were reshaped to its
   last non-empty domain, or (3) if the child is unmapped, "GetDomain" 
   returns the domain the child would have if it were mapped.
   
   "GetDomain" is a checked runtime error if the parent of "ch" is not
   a "ZSplit".  *)


PROCEDURE GetParentDomain(v: T): Rect.T;
<* LL.sup = VBT.mu *>
(* Return the last non-empty value of "v.domain", or "Rect.Empty" if
   "v.domain" has always been empty.  *)

(* \subsubsection{Moving children when the parent is reshaped} *)

(* You can supply procedures to control what happens to the children
   when a "ZSplit" is reshaped.  If you don't supply a procedure, the
   default behavior is as follows: the initial background child is
   always reshaped to have the same domain as the parent.  The other
   children are reshaped so as to preserve their shape and their offsets
   (even if this makes them extend outside the parent domain).  The rule
   is different if the parent is reshaped to "Rect.Empty": in this case
   the "ZSplit" records its children's shapes and offsets and reshapes
   them all to "Rect.Empty".  When the "ZSplit" is later reshaped to
   a non-empty domain, it reshapes the initial background child to have
   the same domain as the parent, and restores the saved dimensions
   and offsets of the other children.

   In the unusual case that the initial background child is deleted,
   subsequent background children do not automatically inherit the 
   special reshaping behavior of the initial background child.  
   
   To override the default behavior, use "SetReshapeControl":  *)
   

PROCEDURE SetReshapeControl(
    ch: VBT.T;
    rc: ReshapeControl); <* LL.sup = VBT.mu *>
(* Set the reshape control object for the child "ch" to be "rc".  *)


TYPE ReshapeControl = OBJECT METHODS
  apply(ch:VBT.T; READONLY old, new, prev: Rect.T)
  : Rect.T <* LL.sup = VBT.mu.ch *>
END;

(* "SetReshapeControl" arranges that whenever the "ZSplit" parent "v"
   of "ch" is reshaped from domain "old" to domain "new", then if the
   previous domain of "ch" is "prev", the new domain of "ch" will become
   "rc.apply(ch, old, new, prev)" (if this rectangle doesn't satisfy
   "ch"'s size constraints, its height and width will be projected into
   range, preserving its offset).
   
   These methods of the "ReshapeControl" objects may be called concurrently
   for different children.  (This is why the apply method has only a 
   share of "VBT.mu".) The stacking order is not changed by
   reshaping.

   When a "ZSplit" child is replaced by "Split.Replace", the new child
   inherits the old child's reshape control object.  

   "SetReshapeControl" is a checked runtime error if the parent of "ch"
   is not a "ZSplit".

   If the "ZSplit" is reshaped to "Rect.Empty", it will reshape its
   children to "Rect.Empty" without calling their reshape control
   methods.  Similarly, if the parent is subsequently reshaped to its
   original rectangle, it will restore the children's previous domains
   without calling the methods.  

   By default, the background is chained absolutely to the parent
   domain, using "Background": *)

VAR (*CONST*)
  Background: ReshapeControl;
   
   (* One useful reshape control method provided by this interface is
   "ChainReshape", in which some set of the child's west, east, north,
   and south edges are ``chained'' to the corresponding edges of the
   parent.  Chaining an edge means that the distance between the child
   edge and the corresponding parent edge will be preserved.  For
   example, if both the west and east edges are chained, then the child's
   horizontal extent will be inset into the parent's horizontal extent
   by fixed amounts on both sides.  For another example, suppose that
   the the east edge is chained and the west edge is not.  In this case
   the distance between the east edges of the child and parent will
   be preserved, but the west edge of the child will move so as to
   preserve the width of the child.  The north and south edges control
   the vertical extent in a similar manner.  *)

TYPE
  Ch = {W, E, N, S};
  ChainSet = SET OF Ch;
  ChainReshapeControl = ReshapeControl OBJECT 
    chains: ChainSet 
  OVERRIDES
    apply := ChainedReshape
  END;

VAR (*CONST*)
  NoChains, WChains, EChains, WEChains, NChains, 
  WNChains, ENChains, WENChains, SChains, 
  WSChains, ESChains, WESChains, NSChains, 
  WNSChains, ENSChains, WENSChains: ChainReshapeControl;

(* The ``variables'' above are constants for the following reshape 
   control objects: 

    | NEW(ChainReshapeControl, chains := ChainSet{}), 
    | NEW(ChainReshapeControl, chains := ChainSet{Ch.W}),
    |
    | ...
    |
    | NEW(ChainReshapeControl, 
    |     chains := ChainSet{Ch.W,Ch.E,Ch.N,Ch.S})

 *)


PROCEDURE ChainedReshape(
  self: ChainReshapeControl;
  ch: VBT.T;
  READONLY oldParentDomain, newParentDomain, 
    oldChildDomain: Rect.T): Rect.T;
(* Return the rectangle that results from chaining each edge in
   "self.chains" to the corresponding edge of the parent domain, and
   leaving the other edges unconstrained.  *)
   
(* If both edges in a dimension are chained, the offset and extent of
   the child will both vary to satisfy the chain constraints; if one edge
   is chained, the offset will vary and the extent will be fixed; if
   both edges are unchained, the offset and the extent will both be
   fixed.  *)

(* The default behavior for the initial background child
   is "Background", and the default behavior for all other children
   is "WNChains". 

   One final reshape control method is sometimes useful: *)


PROCEDURE ScaledReshape(
  self: ReshapeControl;
  ch: VBT.T;
  READONLY oldParentDomain, newParentDomain, 
    oldChildDomain: Rect.T) : Rect.T;  
(* Return the integer approximation to the rectangle that results from
   scaling the old child domain to occupy the same relative position
   of the changing parent domain.  *)


VAR (*CONST*) Scaled: ReshapeControl;

(* This ``variable'' is really a constant for the following reshape 
   control object:

| NEW(ReshapeControl, apply := ScaledReshape)

*)

END ZSplit.
