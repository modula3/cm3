(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 13:08:44 PDT 1992 by muller *)
(*      modified on Sat Jun 13 12:32:44 PDT 1992 by meehan *)
(* modified on Thu Jul 11 9:14:59 PDT 1991 by mhb *)
(* modified on Fri Feb 15 11:12:44 PST 1991 by brooks *)

MODULE KeyTrans;

IMPORT VBT, Key;


PROCEDURE Latin1 (key: VBT.KeySym): CHAR =
  BEGIN
    IF key >= 32 AND key <= 255 THEN (* ISO-Latin-1 printable *)
      RETURN VAL (key, CHAR)
    ELSE
      CASE key OF
      | Key.Backspace => RETURN '\010'
      | Key.Tab => RETURN '\t'
      | Key.Return => RETURN '\n'
      | Key.Escape => RETURN '\033'
      | Key.Delete => RETURN '\177'
      ELSE
        RETURN NullKey
      END
    END
  END Latin1;

PROCEDURE TTY (READONLY cd: VBT.KeyRec): CHAR =
  VAR c := Latin1 (cd.whatChanged);
  BEGIN
    IF VBT.Modifier.Control IN cd.modifiers THEN
      IF c >= '@' AND c <= '_' THEN (* ctrl-uppers *)
        c := VAL (ORD (c) - 8_100, CHAR)
      ELSIF c >= '`' AND c <= '~' THEN (* ctrl-lowers *)
        c := VAL (ORD (c) - 8_140, CHAR)
      ELSIF c = '?' THEN        (* ctrl-? *)
        c := '\177'
      END
    END;
    RETURN c
  END TTY;

BEGIN
END KeyTrans.
