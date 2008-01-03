(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Nov 12 16:07:57 PST 1994 by heydon                   *)
<* PRAGMA SPEC *>

(* A "ToolBox.T" is a collection of buttons that activate drawing tools. *)

INTERFACE ToolBox;

IMPORT Drawing, View, Editor;
IMPORT VBT, ButtonVBT, MenuBtnVBT;

TYPE
  T = VBT.Split;

  Button <: ButtonPublic;
  ButtonPublic = ButtonVBT.T OBJECT METHODS
    init(root: View.Root; name: TEXT; tl: Drawing.ArgTool): Button
  END;

  SetButton <: SetButtonPublic;
  SetButtonPublic = MenuBtnVBT.T OBJECT METHODS
    init(root: View.Root; name: TEXT; tl: Drawing.SetTool): SetButton
  END;

(* "NEW(Button).init(rt, nm, tl)" returns a button displaying the name "nm"
   that applies the drawing tool "tl" to the drawing view of "root".

   The call "NEW(SetButton).init(rt, nm, tl)" does the same thing, but is
   activated by selecting it while dragging through a menu. *)

<* SPEC Update REQUIRES sup(LL) = VBT.mu *>
PROCEDURE Update(t: T; ed: Editor.T; rt: View.Root; n := 0; anon := TRUE);
(* Replace all children in "t" after the first "n" with buttons for the
   public predicates, (functions), and procedures defined in the editor "ed".
   All of these buttons apply to the root "rt". If "anon = TRUE", then the
   "MODULE" declaration (if any) is ignored, so all the button names will be
   unqualified. *)

PROCEDURE Unselect(rt: View.Root);
(* Unhighlight and unselect (in the drawing view) the currently selected tool
   "rt.currButton". *)

<* SPEC SwapButton REQUIRES sup(LL) = VBT.mu *>
PROCEDURE SwapButton(t: T; curr, new: Drawing.ArgTool; newLabel: TEXT);
(* Let "b" be the button in "T" with associated tool "curr". Install a new
   button in "b"'s place with the new label "newLabel" and associated tool
   "new". Requires "curr" to be a tool in "T". *)

END ToolBox.
