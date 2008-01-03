(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jul  4 10:00:08 PDT 1995 by mhb                      *)

INTERFACE TextPortButton;

IMPORT VText, VBT;

CONST
  Brand = "TextPortButton";

TYPE
  Interval = VText.Interval;
  T = OBJECT
    label: TEXT;
    interval: Interval; (* R/O *)
  METHODS
    callback (READONLY cd: VBT.MouseRec);
  END;

(* For a TextPortButton.T "b", the field "b.interval" is set after
   "b" is inserted into some TextPortWithButtons.T "t" 
   by calling "t.insertButton(b)". *)

PROCEDURE GetExtents(b: T; VAR (*out*) left, right: CARDINAL);

END TextPortButton.
