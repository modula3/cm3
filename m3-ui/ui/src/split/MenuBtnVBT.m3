(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Tue Mar 10 19:06:47 1992 by steveg   *)
(*      modified on Mon Feb 24 13:53:48 PST 1992 by muller   *)
(*      modified on Sun Nov 10 19:41:16 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:32:21 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE MenuBtnVBT;

IMPORT VBT, Filter, ButtonVBT, BtnVBTClass, TextVBT;

FROM VBT IMPORT ClickType;

REVEAL 
  T = ButtonVBT.T BRANDED OBJECT
  OVERRIDES 
    mouse := Mouse;
    init := Be
  END;

PROCEDURE Be(v: T; ch: VBT.T; p: ButtonVBT.Proc; 
  ref: REFANY := NIL): ButtonVBT.T =
  BEGIN
    EVAL ButtonVBT.T.init(v, ch, p, ref);
    v.armed := TRUE;
    RETURN v
  END Be;

PROCEDURE New(
    ch: VBT.T; 
    action: ButtonVBT.Proc; 
    ref: REFANY := NIL): T RAISES {} =
  BEGIN
    RETURN Be(NEW(T), ch, action, ref)
  END New;
  
PROCEDURE Mouse(v: T; READONLY cd: VBT.MouseRec) RAISES {} =
  BEGIN
    Filter.T.mouse(v, cd);
    IF cd.clickType # ClickType.FirstDown THEN
      IF (cd.clickType = ClickType.LastUp) AND NOT cd.cp.gone 
      THEN
        IF NOT v.ready THEN v.pre() END;
        v.action(v, cd);
        v.post()
      ELSIF v.ready THEN
        v.cancel()
      END;
      v.ready := FALSE
    END
  END Mouse;

PROCEDURE TextItem(name: TEXT; 
  action: ButtonVBT.Proc; 
  ref: REFANY := NIL): T RAISES {} =
  BEGIN
    RETURN
    New(TextVBT.New(name, 0.0, 0.5, 3.0, 0.5),
        action := action,
        ref := ref)
  END TextItem;

BEGIN 
END MenuBtnVBT.
    
