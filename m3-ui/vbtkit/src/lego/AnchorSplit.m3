(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 15 15:55:36 PDT 1993 by meehan *)
(*      modified on Fri Jan 29 15:21:35 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 12:59:14 PDT 1992 by muller *)

MODULE AnchorSplit;

IMPORT AnchorBtnVBT, FeedbackVBT, Filter, MultiClass,
       MultiFilter, SwitchVBT, VBT;

(* The first child (anchor) is MultiFilter.Child (Filter.Child (v)).
   The second child (menu)  is MultiFilter.Child (v.menu).
*)

REVEAL
  T = Public BRANDED OBJECT
      METHODS
        getAnchorChild (): VBT.T   := GetAnchorChild;
        setAnchorChild (ch: VBT.T) := SetAnchorChild;
        getMenuChild   (): VBT.T   := GetMenuChild;
        setMenuChild   (ch: VBT.T) := SetMenuChild;
      OVERRIDES
        init   := Init;
        pre    := SwitchVBT.Pre;
        post   := SwitchVBT.Post;
        cancel := SwitchVBT.Cancel;
      END;

TYPE 
  MC = MultiClass.Split OBJECT
    OVERRIDES
      replace := Replace;
      insert  := Insert;
      succ    := Succ;
    END;

PROCEDURE Init (v             : T;
                feedback      : FeedbackVBT.T;
                menuFrame     : MultiFilter.T;
                n             : CARDINAL        := 0;
                anchorParent  : VBT.T           := NIL;
                hfudge, vfudge                  := 0.0  ): T =
  BEGIN
    <* ASSERT MultiFilter.Child (feedback) = NIL *>
    <* ASSERT MultiFilter.Child (menuFrame) = NIL *>
    EVAL AnchorBtnVBT.T.init (
           v, feedback, menuFrame, n, anchorParent, hfudge, vfudge);
    MultiClass.Be (v, NEW (MC));
    RETURN v
  END Init;

PROCEDURE GetAnchorChild (v: T): VBT.T =
  BEGIN
    RETURN MultiFilter.Child (Filter.Child (v))
  END GetAnchorChild;

PROCEDURE SetAnchorChild (v: T; ch: VBT.T) =
  BEGIN
    EVAL MultiFilter.Replace (Filter.Child (v), ch)
  END SetAnchorChild;

PROCEDURE GetMenuChild (v: T): VBT.T =
  BEGIN
    RETURN MultiFilter.Child (v.menu)
  END GetMenuChild;

PROCEDURE SetMenuChild (v: T; ch: VBT.T) =
  BEGIN
    EVAL MultiFilter.Replace (v.menu, ch)
  END SetMenuChild;

PROCEDURE Insert (m: MC; pred, ch: VBT.T) =
  VAR v: T := m.vbt;
  BEGIN
    IF pred = NIL THEN
      v.setAnchorChild (ch)
    ELSIF pred = v.getAnchorChild () THEN
      IF v.getMenuChild () = NIL THEN
        v.setMenuChild (ch)
      ELSE
        v.setAnchorChild (ch)    (* See Trestle Ref Man, p.  46 *)
      END
    ELSE
      v.setMenuChild (ch)        (* See Trestle Ref Man, p.  46 *)
    END
  END Insert;

PROCEDURE Replace (m: MC; ch, new: VBT.T) =
  VAR v: T := m.vbt;
  BEGIN
    IF ch = NIL THEN             <* ASSERT FALSE *>
    ELSIF ch = v.getAnchorChild () THEN
      v.setAnchorChild (new)
    ELSIF ch = v.getMenuChild () THEN
      v.setMenuChild (new)
    ELSE                         <* ASSERT FALSE *>
    END
  END Replace;

PROCEDURE Succ (m: MC; ch: VBT.T): VBT.T =
  VAR v: T := m.vbt;
  BEGIN
    IF ch = NIL THEN
      RETURN v.getAnchorChild ()
    ELSIF ch = v.getAnchorChild () THEN
      RETURN v.getMenuChild ()
    ELSIF ch = v.getMenuChild () THEN
      RETURN NIL
    ELSE                         <* ASSERT FALSE *>
    END
  END Succ;

BEGIN
END AnchorSplit.



