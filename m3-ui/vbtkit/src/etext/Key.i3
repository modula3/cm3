(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 13:08:46 PDT 1992 by muller     *)
(*      modified on Sat Jun 13 12:01:07 PDT 1992 by meehan     *)
(*      modified on Thu Nov  8 15:10:55 PST 1990 by brooks     *)

INTERFACE Key;

(* Constants for the "KeySym"s of common non-graphic keys. *)

CONST
  KeyboardSetBase = 255 * 256;
  Backspace = 8 + KeyboardSetBase;
  Tab = 9 + KeyboardSetBase;
  Linefeed = 10 + KeyboardSetBase;
  Return = 13 + KeyboardSetBase;
  Escape = 27 + KeyboardSetBase;
  Delete = 255 + KeyboardSetBase;
  LeftShift = 225 + KeyboardSetBase;
  RightShift = 226 + KeyboardSetBase;
  LeftControl = 227 + KeyboardSetBase;
  RightControl = 228 + KeyboardSetBase;
  LeftAlt = 232 + KeyboardSetBase;
  RightAlt = 233 + KeyboardSetBase;
  CapsLock = 229 + KeyboardSetBase;
    (* SRC keyboard names *)
  LeftOption = LeftAlt;
  RightOption = RightAlt;

END Key.
