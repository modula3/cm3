(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 13:08:45 PDT 1992 by muller     *)
(*      modified on Sat Jun 13 12:32:56 PDT 1992 by meehan     *)
(*      modified on Tue Feb  4 16:42:03 PST 1992 by mhb        *)
(*      modified on Wed Nov 14 16:18:34 PST 1990 by brooks     *)


INTERFACE KeyTrans;

(* "KeyTrans" is a utility interface that provides some standard
   mapping between keyboard keys and ASCII characters. *)

IMPORT VBT;

CONST
  NullKey = '\000';


PROCEDURE Latin1 (key: VBT.KeySym): CHAR;
(* Return the ISO-Latin-1 character corresponding to "key".

   \medskip\bulletitem If "32 <= key <= 255", then return
   "VAL(key, CHAR)".

   \medskip\bulletitem If "key" is "Key.Backspace", "Key.Tab",
   "Key.Return", "Key.Escape", or "Key.Delete", then return the
   corresponding character.

   \medskip\bulletitem Otherwise return "NullKey" *)
   
PROCEDURE TTY (READONLY cd: VBT.KeyRec): CHAR;
(* Similar to "Latin1", but if this is a control key, then clear
   the ``high-order bits'' from the character code.  For example,
   for both control-a and control-A, we return "VAL(1, CHAR)". *)

END KeyTrans.
