(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* File TypeInVBT.def *)
(* Last modified on Sun Feb 14 18:55:44 PST 1993 by msm    *)
(*      modified on Tue Mar 10 19:01:25 1992 by steveg *)
(*      modified on Mon Feb 24 13:55:20 PST 1992 by muller *)
(*      modified on Wed Oct 23  0:58:46 PDT 1991 by gnelson*)
(*      modified on Mon Feb 15 16:54:41 1988 by birrell    *)
<*PRAGMA LL*>

INTERFACE TypeInVBT;

(* A "TypeInVBT.T" is a "VBT" into which the user can type a one-line
   text string using ISO-Latin 1 keycodes.   

   A "TypeInVBT" acquires the keyboard focus when the user clicks on it
   with the mouse, or whenever the program directs it to by
   calling TakeFocus.  Whenever it acquires the keyboard focus, it also
   acquires the primary selection, and primary-selects the text string in
   the "VBT".

   When it has the keyboard focus, the user can append to the text by 
   typing graphic characters, and can backspace by typing the key
   labelled "<X]".  Associated with every TypeInVBT is an action 
   procedure that is called whenever the user types
   a non-graphic character (such as "RETURN").

   When a "TypeInVBT" it has the keyboard focus, it responds to 
   the following editing commands:
     control-q deletes the entire text (primary selection);
     control-w copies the secondary selection to the end of the text;
     control-e moves the secondary selection to the end of the text;
     control-r swaps the text (primary selection) with the secondary selection.

   You can make the entire text string be the secondary selection by
   control-clicking with the mouse.

   The TypeInVBT filters keystrokes through a ComposeKey.T.

*)


IMPORT VBT, TextVBT, PaintOp, Font, ComposeKey;

TYPE T <: Public; Public = TextVBT.T OBJECT
  METHODS (* OVERRIDES *)
    init(
      txt: TEXT := "";
      halign, valign: REAL := 0.5;
      hmargin: REAL := 0.5;
      vmargin: REAL := 0.5;
      fnt: Font.T := Font.BuiltIn;
      op: PaintOp.ColorQuad := NIL;
      action: Proc := NIL;
      ref: REFANY := NIL;
      composer: ComposeKey.T := NIL): T
  END; 

(* The call "v.init(...)" initializes "v" as a "TypeInVBT".  This
   includes initializing "v" as a "TextVBT", using the first seven
   arguments.  The "action" argument specifies "v"'s action procedure;
   if it is non-NIL, it is called for any non-printing characters that
   the user types, except for <X] and control characters.  If "ref"
   is non-"NIL", it is added to "v" property set.  The composer's filter
   method is invoked on every keystroke; if composer is NIL, TypeInVBT
   will allocate (and initialize) a new Composer.  If you pass a non-NIL
   value, you must take care to initialize the composer yourself. *)

TYPE
  Composer <: CPublic;
  CPublic = ComposeKey.T OBJECT METHODS init (v: VBT.T) END;
(* A Composer overrides the feedback method to change the cursor
   during composing. *)

TYPE Proc = PROCEDURE(v: T; READONLY cd: VBT.KeyRec);

PROCEDURE SetAction(v: T; action: Proc); <* LL.sup = VBT.mu *>
(* Change the action for non-printing, non-control characters. *)

PROCEDURE New(
  txt: TEXT := "";
  halign, valign: REAL := 0.5;
  hmargin: REAL := 0.5;
  vmargin: REAL := 0.5;
  fnt: Font.T := Font.BuiltIn;
  op: PaintOp.ColorQuad := NIL;
  action: Proc := NIL;
  ref: REFANY := NIL;
  composer: ComposeKey.T := NIL): T; 
(* "New(args)" is short for "NEW(T).init(args)". *)

PROCEDURE TakeFocus(v: T; t: VBT.TimeStamp): BOOLEAN;
(* TakeFocus(v, t) causes "v" to acquire the keyboard focus and
   ownership of the primary selection and returns "TRUE", or returns
   "FALSE" if one or both of these acquisitions failed.
   *)

PROCEDURE HasFocus(v: T): BOOLEAN;
(* Does v have the keyboard focus? LL = VBT.mu *)

END TypeInVBT.
