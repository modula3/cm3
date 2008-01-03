(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:54:23 PST 1992 by muller   *)
(*      modified on Sun Nov 10 16:02:29 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:33:34 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE RootVBT;

IMPORT VBT, Split, Filter, HighlightVBT, VBTClass, Palette, VBTRep;

TYPE
  GrandChild = HighlightVBT.T OBJECT
    proc: DeleteProc
  METHODS
    rescreen := Rescreen;
    misc := Misc;
  END;

REVEAL Child = ETAgent.T BRANDED OBJECT END;

PROCEDURE NewChild(ch: VBT.T; p: DeleteProc): Child = 
  VAR grandChild := NEW(GrandChild, proc := p); res := NEW(Child); BEGIN
    EVAL HighlightVBT.T.init(grandChild, ch);
    EVAL ETAgent.T.init(res, grandChild);
    LOCK res DO 
      res.props := res.props + VBTRep.Props{VBTRep.Prop.Combiner}
    END;
    RETURN res
  END NewChild;

PROCEDURE Rescreen(v: Child; READONLY cd: VBT.RescreenRec) =
  BEGIN
    Palette.Init(cd.st);
    HighlightVBT.T.rescreen(v, cd) 
  END Rescreen;

PROCEDURE Misc(v: GrandChild; READONLY cd: VBT.MiscRec) =
  VAR ch := v.ch; button: VBT.Button := LAST(VBT.Button);
  CONST gone = VBT.CursorPosition{Point.Origin, NoScreen, TRUE, TRUE};
  BEGIN
    IF ch = NIL THEN RETURN END;
    IF cd.type = VBT.Deleted OR cd.type = VBT.Disconnected THEN
      VBTClass.Position(v, VBT.PositionRec{gone, 0, VBT.Modifiers{}});
      VBTClass.Mouse(v, 
        VBT.MouseRec{button, 0, gone, VBT.Modifiers{},
          VBT.ClickType.LastUp, 0});
      IF v.proc # NIL THEN v.proc(ch) END;
      EVAL Filter.Replace(v, NIL)
    END;
    VBTClass.Misc(ch, cd);
  END Misc;

END RootVBT.
