(* Copyright (C) 1996, Digital Equipment Corporation            *)
(* All rights reserved.                                         *)
(* See the file COPYRIGHT for a full description.               *)
(*                                                              *)
(* Last modified on Fri May 17 10:38:18 PDT 1996 by mhb         *)

(* The "VBTKitEnv" interface provides a way to collect in one
   place the various environment variables that widgets in the
   vbtkit use for controlling their appearance. *)

INTERFACE VBTKitEnv;

(* "SCROLLBARLOC" specifies where scrollbars should be placed.  The
   possible values are "north", "northeast", "northwest", "south",
   "southeast", "southwest", "east", and "west".  The default location for
   a vertical scrollbar is "west"; and for a horizontal scrollbar, the
   default location is "south". *)

VAR (* read-only after initialization *)
  ScrollbarWest:  BOOLEAN := TRUE;
  ScrollbarSouth: BOOLEAN := TRUE;


(* "TEXTPORTMODEL" specifies the editing model that text-editing widgets
   should use.  The possible values are "emacs", "ivy", "mac", and "xterm";
   "emacs" is the default value.  See the "TextPort" interface for more
   information. *)

VAR (* read-only after initialization *)
  TextPortModel: TEXT := "emacs";

END VBTKitEnv.









