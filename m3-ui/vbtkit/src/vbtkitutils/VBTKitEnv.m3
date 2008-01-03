(* Copyright (C) 1996, Digital Equipment Corporation            *)
(* All rights reserved.                                         *)
(* See the file COPYRIGHT for a full description.               *)
(*                                                              *)
(* Last modified on Fri May 17 11:18:09 PDT 1996 by mhb         *)

MODULE VBTKitEnv;

IMPORT Env, Text;

BEGIN
  WITH s = Env.Get ("SCROLLBARLOC") DO
    IF s # NIL THEN
      IF Text.Equal (s, "north") OR 
         Text.Equal (s, "northeast") OR 
         Text.Equal (s, "northwest")THEN ScrollbarSouth := FALSE
      END;
      IF Text.Equal (s, "east") OR 
         Text.Equal (s, "northeast") OR 
         Text.Equal (s, "southeast")THEN ScrollbarWest := FALSE
      END
    END
  END;

  WITH s = Env.Get ("TEXTPORTMODEL") DO
    IF s # NIL THEN
      IF Text.Equal (s, "ivy") THEN TextPortModel := "ivy" 
      ELSIF Text.Equal (s, "mac") THEN TextPortModel := "mac" 
      ELSIF Text.Equal (s, "xterm") THEN TextPortModel := "xterm"
      END
    END
  END;

END VBTKitEnv.

