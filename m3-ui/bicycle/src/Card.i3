(* Copyright (C) 1991, Digital Equipment Corporation                    *)
(* All rights reserved.                                                 *)
(* See the file COPYRIGHT for a full description.                       *)

(* Last modified on Thu Jul  1 16:55:48 PDT 1993 by msm     *)

INTERFACE Card;

(* This interfaces paints and moves playing cards, in a way suitable for
   many kinds of solitaire games, and maybe other things.  The cards are
   all children of a provided ZSplit.

   The card layout is adjusted automatically when a card is detached from
   the top of a stack.  Attaching one card to the top of another stack of
   cards makes the new card the top of the stack, and positions the new
   card.  If the bottom-most card in the stack has value Min, the cards
   are stacked directly on top of one another.  Otherwise, the cards are
   stacked with a vertical spacing of Overlap if the card immediately
   underneath is face up, and OverlapDown if the card below is face down.
   A stack whose only elements have value Min, Max, or Talon paints as
   an empty card.  Almost always, you should have a Min, Max, or Talon
   at the bottom of a stack of cards.

   Game play proceeds when the user clicks on a card, or drags a card to
   another card.  The five procedures attachable, play, obvious, trivial,
   and stupid control the behavior.  The attachable procedure should return
   TRUE if the first card can be legally played on the second.  The play
   procedure should return TRUE, and set the out parameters, if there is
   a forced play.  The obvious, trivial, and stupid procedures are bound
   to clicking on the left, middle, and right buttons.  By convention, the
   obvious procedures offers a move for the selected card which, whether
   or not it is currently legal, would be progress towards solving the game.
   The trivial procedure suggests a legal move which isn't necessarily
   on the shortest path towards a solution, but may be a useful move if you
   can't think of anything else to do.  The stupid procedure suggests a
   move which is generally illegal, because it shows you what you might
   move onto this card to make progress.
   
   This interface always keeps a log of the moves that have been made.
   No standard user interface for undo and redo is provided, since in
   some games it might be considered cheating.

   In addition, the interface supports creating cards without stacking 
   semantics, which can be used when rectangular cards are good enough.
   This is the type "Card".

*)
     
IMPORT TwoTone, VBT, ZSplit, Point;

CONST
  Width = 79; (* width of a card, in pixels *)
  Height = 123; (* height of card, in pixels *)
  OverlapPercentage = 25;
  OverlapDownPercentage = 10;
  Overlap = (OverlapPercentage * Height + 50) DIV 100;
  OverlapDown = (OverlapDownPercentage * Height + 50) DIV 100;

VAR (* READONLY *)
  felt: TwoTone.T; (* color of the background; you have to put the
  TextureVBT in the background of your ZSplit for everything to work. *)

TYPE
  Value =
    {Min, Ace, Deuce, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack,
     Queen, King, Max, Talon};
  Rank = [Value.Ace..Value.King];
  Family = {Any, Spades, Hearts, Diamonds, Clubs};
  Suit = [Family.Spades..Family.Clubs];
  Private <: VBT.T;
  Public = Private OBJECT
    value: Value;
    family: Family;
    faceUp := TRUE;
    above, below: T := NIL
  END;
  T <: Public;
  StandardDeck = ARRAY [0 .. 51] OF T;
  CardPublic = VBT.Leaf OBJECT
    value: Value;
    family: Family;
    faceUp := TRUE;
  END;
  Card <: CardPublic;

(* All procedures in this interface have LL = VBT.mu *)

PROCEDURE InitializeStandardDeck(VAR deck: StandardDeck; zSplit: ZSplit.T);
(* Create a full deck of cards, all face up at the origin. *)

PROCEDURE Shuffle(VAR deck: ARRAY OF T);
(* Randomly reorder the elements of the deck *)

PROCEDURE New(
    value: Value;
    suit: Family;
    READONLY loc: Point.T;
    zSplit: ZSplit.T;
    faceUp: BOOLEAN := TRUE)
    : T;
(* Create a single card. *)

PROCEDURE NewCard (value: Value; suit: Family; faceUp := TRUE): Card;
(* Create a single rectangular card; equivalent to NEW(Card, value :=
   value, family := suit, faceUp := faceUp). *)

PROCEDURE Detach(c: T);
(* Remove c from its stack; the cards above c are not altered.
   If c is not at the top of its stack, you won't much like the
   results, unless you're detaching all the cards in order
   to reset the game. *)

EXCEPTION BadDeal;

PROCEDURE Attach(c, p: T) RAISES {BadDeal};
(* c is placed on top of p.  If p is in a stack with a Min at the base, c is
placed directly on top of p. Otherwise, c is shifted down by Overlap,
unless p is itself the unreal card at the base of the stack.  Cards
played on top of down-facing cards are offset by OverlapDown instead.
All cards above c are moved as well.  If p is in the same stack as c, 
BadDeal is raised, unless c is atop p. If p has any real card above it 
other than c, BadDeal is raised. *)

PROCEDURE StartUndoLog();
(* Clear the undo and redo logs *)

PROCEDURE Undo(): BOOLEAN;
(* Undo a move (including all forced moves that followed it) and return TRUE;
   if at beginning of log, return FALSE *)

PROCEDURE Redo(slowly := FALSE): BOOLEAN;
(* Redo a move and return TRUE; if at end of log, return FALSE.  By
   setting slowly, you can make the redo pause briefly after each
   forced card is moved, so that the user can see what is going on. *)

PROCEDURE Flip(c: T; up: BOOLEAN);
(* Make c face "up", and mark "c" for redisplay *)

PROCEDURE FlipCard(c: Card; up: BOOLEAN);
(* Make c face "up", and mark "c" for redisplay *)

PROCEDURE Real(c: T): BOOLEAN;
(* Return TRUE if c's value is a Rank and c's family is a Suit. *)

PROCEDURE RealCard(c: Card): BOOLEAN;
(* Return TRUE if c's value is a Rank and c's family is a Suit. *)

PROCEDURE Top(c: T): T;
(* Return the card on top of c's stack *)

PROCEDURE Bottom(c: T): T;
(* Return the (generally unreal) card at the bottom of c's stack *)

TYPE
  AttachProc = PROCEDURE(a, b: T): BOOLEAN;
  (* Can first card can be played on second *)
  PlayProc = PROCEDURE(VAR a, b: T): BOOLEAN;
  (* Returns a forced play and TRUE, else FALSE.  Playing a card
     on itself flips the card. *)
  MoveProc = PROCEDURE(a: T): T;
  (* Returns a card on which to play the argument, else NIL *)

VAR
  
  attachable: AttachProc;
  play: PlayProc;
  obvious, trivial, stupid: MoveProc;
  (* return the obvious move, if it exists (button 1 action); 
     return the trivial move, if it exists (button 2 action);
     return a stupid but informative move  (button 3 action) *)

PROCEDURE EnableTracking(enable: BOOLEAN);
  (* When Tracking is turned on, Card automatically highlights
     the position returned by obvious.  Tracking defaults to FALSE. *)

PROCEDURE EnableHighlight(enable: BOOLEAN; chain: INTEGER);
  (* When true, pressing a mouse button highlights the selected
     target card.  If chain is non-negative, every chain milliseconds, 
     the next card is also highlighted.  These default to TRUE and 1000 *)

END Card.
