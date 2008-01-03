(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon May  8 16:17:47 PDT 1995 by najork                   *)
(*       Created on Mon Apr 24 11:37:08 PDT 1995 by najork                   *)

MODULE Test23 EXPORTS Main;

IMPORT Fmt, IO, Text, TextVBT, Trestle, VBT;


PROCEDURE ModifierToText (m: VBT.Modifier): TEXT =
  BEGIN
    CASE m OF
    | VBT.Modifier.Shift   => RETURN "Shift";
    | VBT.Modifier.Lock    => RETURN "Lock";
    | VBT.Modifier.Control => RETURN "Control";
    | VBT.Modifier.Option  => RETURN "Option";
    | VBT.Modifier.Mod0    => RETURN "Mod0";
    | VBT.Modifier.Mod1    => RETURN "Mod1";
    | VBT.Modifier.Mod2    => RETURN "Mod2";
    | VBT.Modifier.Mod3    => RETURN "Mod3";
    | VBT.Modifier.MouseL  => RETURN "MouseL";
    | VBT.Modifier.MouseM  => RETURN "MouseM";
    | VBT.Modifier.MouseR  => RETURN "MouseR";
    | VBT.Modifier.Mouse0  => RETURN "Mouse0";
    | VBT.Modifier.Mouse1  => RETURN "Mouse1";
    | VBT.Modifier.Mouse2  => RETURN "Mouse2";
    | VBT.Modifier.Mouse3  => RETURN "Mouse3";
    | VBT.Modifier.Mouse4  => RETURN "Mouse4";
    END;
  END ModifierToText;


PROCEDURE ModifiersToText (mods: VBT.Modifiers): TEXT =
  VAR 
    res := "";
  BEGIN
    FOR m := FIRST (VBT.Modifier) TO LAST (VBT.Modifier) DO
      IF m IN mods THEN
        IF NOT Text.Equal (res, "") THEN res := res & "," END;
        res := res & ModifierToText(m);
      END;
    END;
    RETURN res;
  END ModifiersToText;


PROCEDURE Key (v: VBT.T; READONLY cd: VBT.KeyRec) =
  VAR 
    msg : TEXT;
  BEGIN
    msg := "whatChanged = "  & Fmt.Int (cd.whatChanged) &
           "  wentDown = "   & Fmt.Bool (cd.wentDown) & 
           "  modifiers = {" & ModifiersToText(cd.modifiers) & "}";
    TextVBT.Put (v, msg);
  END Key;


PROCEDURE Misc (v: VBT.T; READONLY cd: VBT.MiscRec) = 
  BEGIN    
    IF cd.type = VBT.TakeSelection AND cd.selection = VBT.KBFocus THEN
      VBT.Acquire (v, VBT.KBFocus, cd.time);
    END;
  END Misc;
    

VAR v := NEW (TextVBT.T, key := Key, misc := Misc).init ("");

BEGIN
  Trestle.Install (v);
  Trestle.AwaitDelete(v)
END Test23.
