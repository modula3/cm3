(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:54:10 PST 1992 by muller   *)
(*      modified on Sun Nov 10 18:21:17 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:33:09 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE QuickBtnVBT;

IMPORT ButtonVBT, BtnVBTClass, VBT, Filter;

REVEAL T = ButtonVBT.T BRANDED OBJECT 
  OVERRIDES
    mouse := Mouse;
    pre := Pre;
    post := Pre
  END;

PROCEDURE New(
  ch: VBT.T; 
  action: ButtonVBT.Proc; 
  ref: REFANY := NIL): T RAISES {} =
  BEGIN
    RETURN NEW(T).init(ch, action, ref)
  END New;

PROCEDURE Mouse(v: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Filter.T.mouse(v, cd);
    IF VBT.ClickType.FirstDown = cd.clickType THEN
      v.pre();
      v.action(v, cd);
      v.post()
    END
  END Mouse;

PROCEDURE Pre(<*UNUSED*> v: T) = BEGIN END Pre;
  
BEGIN END QuickBtnVBT.
