(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun 20 16:31:58 PDT 1996 by heydon                   *)
(*      modified on Fri May 17 11:08:49 PDT 1996 by mhb                      *)
(*      modified on Tue Jun 16 12:54:32 PDT 1992 by muller                   *)
(*      modified on Fri Mar 27 02:15:23 PST 1992 by steveg		     *)
(*      modified on Mon Feb 11 16:06:44 PST 1991 by brooks		     *)

MODULE AnyEvent;

IMPORT Fmt, KeyTrans, Thread, VBT, Wr;

<* FATAL Thread.Alerted, Wr.Failure *>

PROCEDURE FromKey (READONLY event: VBT.KeyRec): Key =
  BEGIN
    RETURN NEW(Key, key := event);
  END FromKey;

PROCEDURE FromMouse (READONLY event: VBT.MouseRec): Mouse =
  BEGIN
    RETURN NEW(Mouse, mouse := event);
  END FromMouse;

PROCEDURE FromPosition (READONLY event: VBT.PositionRec):
  Position =
  BEGIN
    RETURN NEW(Position, position := event);
  END FromPosition;

PROCEDURE FromMisc (READONLY event: VBT.MiscRec): Misc =
  BEGIN
    RETURN NEW(Misc, misc := event);
  END FromMisc;


PROCEDURE TimeStamp (anyevent: T): VBT.TimeStamp =
  BEGIN
    TYPECASE anyevent OF
    | Key (key) => RETURN key.key.time;
    | Mouse (mouse) => RETURN mouse.mouse.time;
    | Position (position) => RETURN position.position.time;
    | Misc (misc) => RETURN misc.misc.time;
    ELSE <* ASSERT(FALSE) *>
    END;
  END TimeStamp;


PROCEDURE ToWr (anyevent: T; wr: Wr.T) =
  BEGIN
    TYPECASE anyevent OF
    | Key (key) => WriteKeyRec(wr, key.key);
    | Mouse (mouse) => WriteMouseRec(wr, mouse.mouse);
    | Position (position) => WritePositionRec(wr, position.position);
    | Misc (misc) => WriteMiscRec(wr, misc.misc);
    ELSE <* ASSERT(FALSE) *>
    END;
  END ToWr;


PROCEDURE WriteKeyRec (wr: Wr.T; READONLY kr: VBT.KeyRec) =
  BEGIN
    Wr.PutText (wr, Fmt.F ("{KeyRec whatChanged=%s (%s) wentDown=%s ",
                           Fmt.Int (kr.whatChanged),
                           Fmt.Char (KeyTrans.Latin1 (kr.whatChanged)),
                           Fmt.Bool (kr.wentDown)));
    WriteModifiers (wr, kr.modifiers);
    Wr.PutChar (wr, ' ');
    WriteTimeStamp (wr, kr.time);
    Wr.PutChar (wr, '}')
  END WriteKeyRec;
  
PROCEDURE WriteMouseRec (wr: Wr.T; READONLY cd: VBT.MouseRec) =
  CONST
    ButtonNames = ARRAY VBT.Button OF
                    TEXT {"MouseL", "MouseM", "MouseR", "Mouse0", "Mouse1",
                          "Mouse2", "Mouse3", "Mouse4"};
    ClickNames = ARRAY VBT.ClickType OF
                   TEXT {"FirstDown", "OtherDown", "OtherUp", "LastUp"};
  BEGIN
    Wr.PutText (
      wr, Fmt.F ("{MouseRec whatChanged=%s clickType=%s clickCount=%s ",
                 ButtonNames [cd.whatChanged], ClickNames [cd.clickType],
                 Fmt.Int (cd.clickCount)));
    WriteCursorPosition (wr, cd.cp);
    Wr.PutChar (wr, ' ');
    WriteTimeStamp (wr, cd.time);
    Wr.PutChar (wr, ' ');
    WriteModifiers (wr, cd.modifiers);
    Wr.PutText (wr, "}")
  END WriteMouseRec;

PROCEDURE WritePositionRec (wr: Wr.T; READONLY pr: VBT.PositionRec) =
  <* FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    Wr.PutText (wr, "{PositionRec ");
    WriteCursorPosition (wr, pr.cp);
    Wr.PutChar (wr, ' ');
    WriteModifiers (wr, pr.modifiers);
    Wr.PutChar (wr, ' ');
    WriteTimeStamp (wr, pr.time);
    Wr.PutChar (wr, '}')
  END WritePositionRec;

PROCEDURE WriteMiscRec (wr: Wr.T; READONLY cd: VBT.MiscRec) =
  BEGIN
    Wr.PutText (wr,
                Fmt.F ("{MiscRec type=%s detail=<%s,%s> selection=%s ",
                       VBT.MiscCodeTypeName (cd.type),
                       Fmt.Int (cd.detail [0]), Fmt.Int (cd.detail [1]),
                       VBT.SelectionName (cd.selection)));
    WriteTimeStamp (wr, cd.time);
    Wr.PutChar (wr, '}')
  END WriteMiscRec;

PROCEDURE WriteModifiers (wr: Wr.T; READONLY mods: VBT.Modifiers) =
  CONST
    ModifierNames = ARRAY VBT.Modifier OF
                      TEXT {
                      "Shift", "Lock", "Control", "Option", "Mod0", "Mod1",
                      "Mod2", "Mod3", "MouseL", "MouseM", "MouseR",
                      "Mouse0", "Mouse1", "Mouse2", "Mouse3", "Mouse4"};
  BEGIN
    Wr.PutText (wr, "{Modifiers");
    FOR i := FIRST (VBT.Modifier) TO LAST (VBT.Modifier) DO
      IF i IN mods THEN
        Wr.PutChar (wr, ' ');
        Wr.PutText (wr, ModifierNames [i])
      END
    END;
    Wr.PutChar (wr, '}')
  END WriteModifiers;

PROCEDURE WriteCursorPosition (wr: Wr.T; READONLY cp: VBT.CursorPosition) =
  CONST Not = ARRAY BOOLEAN OF TEXT {"not ", ""};
  BEGIN
    Wr.PutText (
      wr, Fmt.F ("{CursorPos pt=[%s,%s] screen=%s %sgone %soffScreen}",
                 Fmt.Int (cp.pt.h), Fmt.Int (cp.pt.v), Fmt.Int (cp.screen),
                 Not [cp.gone], Not [cp.offScreen]))
  END WriteCursorPosition;
  
PROCEDURE WriteTimeStamp (wr: Wr.T; t: VBT.TimeStamp) =
  BEGIN
    Wr.PutText (wr, Fmt.F ("{time %s}", Fmt.Unsigned (t, 10)))
  END WriteTimeStamp;

BEGIN
END AnyEvent.


