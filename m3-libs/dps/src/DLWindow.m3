(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Wed Aug 17 16:30:36 PDT 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:32 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

MODULE DLWindow;  

IMPORT DisplayList, DPS, Fmt;

PROCEDURE Paint (t: T; box: DPS.Box; only: REFANY) =
  <*FATAL DPS.BadPostScript*>
 VAR him, ret: TEXT := "";
 VAR dl: DisplayList.T;
  BEGIN
  IF only = NIL THEN
    ret := " gsave " & Fmt.Real(BackgroundGray) & " setgray " 
     & DPS.NewPathBox (box) & " fill grestore ";
    END;
  dl := t.displayList;
  IF dl # NIL THEN 
    him :=  dl.Repaint (box, only); 
    IF him # NIL THEN ret := ret & him; END; 
    END;
  IF ret # NIL THEN t.Send ( ret ); END;
  END Paint;

PROCEDURE Mouse (t: T; event: DPS.MouseEvent): BOOLEAN =
 VAR dl: DisplayList.T;
  BEGIN
  dl := t.displayList;
  IF dl = NIL THEN RETURN FALSE; END;
  IF dl.Mouse (t, event) THEN RETURN TRUE; END; 
  (* If no-one processes mouse, kill any input focus. *)
  dl.KillInputFocus();
  RETURN FALSE;
  END Mouse;

PROCEDURE Char (t: T; char: CHAR): BOOLEAN =
 VAR dl: DisplayList.T;
  BEGIN
  dl := t.displayList;
  IF dl = NIL THEN RETURN FALSE; END;
  RETURN dl.Char (t, char);
  END Char;

PROCEDURE Key (t: T; event: DPS.KeyEvent) =
 VAR got: CHAR;
  BEGIN
  got := DPS.CharFromKey (event.key, event.modifiers);
  IF got = '\000' THEN RETURN; END;
  EVAL Char (t, got);
  END Key;

PROCEDURE KillInputFocus (t: T) =
 VAR dl: DisplayList.T;
  BEGIN
  dl := t.displayList;
  IF dl = NIL THEN RETURN; END;
  dl.KillInputFocus();
  END KillInputFocus;

  BEGIN
  END DLWindow.

