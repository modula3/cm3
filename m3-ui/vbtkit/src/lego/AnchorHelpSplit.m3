(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Sep 25 10:46:08 EDT 1995 by dagenais *)
(*      modified on Tue Jun 15 15:55:36 PDT 1993 by meehan *)
(*      modified on Fri Jan 29 15:21:35 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 12:59:14 PDT 1992 by muller *)

MODULE AnchorHelpSplit;

IMPORT AnchorHelpVBT, Filter, MultiClass, VBT;

(* The first child (anchor) is Filter.Child (v).
   The second child (help)  is v.help.
*)

REVEAL
  T = Public BRANDED OBJECT
      METHODS
        getAnchorChild (): VBT.T   := GetAnchorChild;
        setAnchorChild (ch: VBT.T) := SetAnchorChild;
        getHelpChild   (): VBT.T   := GetHelpChild;
        setHelpChild   (ch: VBT.T) := SetHelpChild;
      OVERRIDES
        init   := Init;
      END;

TYPE 
  MC = MultiClass.Split OBJECT
    OVERRIDES
      replace := Replace;
      insert  := Insert;
      succ    := Succ;
    END;

PROCEDURE Init (v             : T;
                ch            : VBT.T;
                help          : VBT.T;
                n             : CARDINAL        := 0;
                hfudge := 0.0;
                vfudge := 1.0  ): T =
  BEGIN
    EVAL AnchorHelpVBT.T.init (
           v, ch, help, n, hfudge, vfudge);
    MultiClass.Be (v, NEW (MC));
    RETURN v
  END Init;

PROCEDURE GetAnchorChild (v: T): VBT.T =
  BEGIN
    RETURN Filter.Child (v)
  END GetAnchorChild;

PROCEDURE SetAnchorChild (v: T; ch: VBT.T) =
  BEGIN
    EVAL Filter.Replace (v, ch)
  END SetAnchorChild;

PROCEDURE GetHelpChild (v: T): VBT.T =
  BEGIN
    RETURN v.help;
  END GetHelpChild;

PROCEDURE SetHelpChild (v: T; ch: VBT.T) =
  BEGIN
    v.help := ch;
  END SetHelpChild;

PROCEDURE Insert (m: MC; pred, ch: VBT.T) =
  VAR v: T := m.vbt;
  BEGIN
    IF pred = NIL THEN
      v.setAnchorChild (ch)
    ELSIF pred = v.getAnchorChild () THEN
      IF v.getHelpChild () = NIL THEN
        v.setHelpChild (ch)
      ELSE
        v.setAnchorChild (ch)    (* See Trestle Ref Man, p.  46 *)
      END
    ELSE
      v.setHelpChild (ch)        (* See Trestle Ref Man, p.  46 *)
    END
  END Insert;

PROCEDURE Replace (m: MC; ch, new: VBT.T) =
  VAR v: T := m.vbt;
  BEGIN
    IF ch = NIL THEN             <* ASSERT FALSE *>
    ELSIF ch = v.getAnchorChild () THEN
      v.setAnchorChild (new)
    ELSIF ch = v.getHelpChild () THEN
      v.setHelpChild (new)
    ELSE                         <* ASSERT FALSE *>
    END
  END Replace;

PROCEDURE Succ (m: MC; ch: VBT.T): VBT.T =
  VAR v: T := m.vbt;
  BEGIN
    IF ch = NIL THEN
      RETURN v.getAnchorChild ()
    ELSIF ch = v.getAnchorChild () THEN
      RETURN v.getHelpChild ()
    ELSIF ch = v.getHelpChild () THEN
      RETURN NIL
    ELSE                         <* ASSERT FALSE *>
    END
  END Succ;

BEGIN
END AnchorHelpSplit.



