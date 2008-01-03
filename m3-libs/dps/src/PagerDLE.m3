(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Wed Aug 17 16:27:19 PDT 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:23 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

MODULE PagerDLE;

IMPORT DisplayList, DPS, DPSWindow;

PROCEDURE Init (t: T; pages: REF ARRAY OF DisplayList.T := NIL) =
  BEGIN
  IF t.initialized THEN RETURN; END;
  t.pages := pages; (* NIL or not ... *)
  IF pages=NIL THEN t.pageCount := 0;
   ELSE t.pageCount := NUMBER(pages^);
    END;
  t.page := -1; (* None displayed. *)
  t.box := DPS.EverywhereBox; (* Don't know how big children are. *)
  t.initialized := TRUE;
  IF pages#NIL THEN NthPage (t, 0); END;
  END Init;

PROCEDURE AppendPage ( t: T; page: DisplayList.T ) =
 VAR new: REF ARRAY OF DisplayList.T;
  BEGIN
  Init (t, NIL);
  IF t.pages = NIL THEN 
    t.pages := NEW (REF ARRAY OF DisplayList.T, 10);
    END;
  IF t.pageCount >= NUMBER(t.pages^) THEN
    new := NEW (REF ARRAY OF DisplayList.T, NUMBER(t.pages^) + 10);
    FOR j := 0 TO NUMBER(t.pages^)-1 DO
      new[j] := t.pages[j];
      END;
    t.pages := new;
    END;
  t.pages[t.pageCount] := page;
  t.pageCount := t.pageCount + 1;
  IF t.First()=NIL THEN NthPage (t, 0); END;
  END AppendPage;
 
PROCEDURE NthPage (t: T; page: INTEGER) =
 VAR first: DisplayList.T;
 VAR haveFocus: BOOLEAN;
  BEGIN
  haveFocus := t.childWithInputFocus # NIL; (* Crude. *)
  IF page < 0 THEN RETURN; END;
  IF page > t.pageCount-1 THEN RETURN; END;
  IF page = t.page THEN RETURN; END;
  first := t.First();
  IF first#NIL THEN first.Remove(); END;
  t.Append (t.pages^[page]);
  t.page := page;
  IF haveFocus THEN t.GetInputFocus (t.First()); END;
  t.Dirty (DPS.EverywhereBox);
  END NthPage;
 
PROCEDURE NextPage (t: T) =
  BEGIN
  NthPage (t, t.page + 1);
  END NextPage;
 
PROCEDURE Mouse (t: T; window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN =
 VAR e: DisplayList.E;
  BEGIN
  Init (t);

  IF t.canMouseChildren THEN
    e := t.Last();
    WHILE e # NIL DO 
      IF e.Mouse (window, event) THEN RETURN TRUE; END; 
      e := e.Previous(); 
      END;
    END;

  IF event.clickType # DPS.ClickType.FirstDown THEN RETURN FALSE; END;
  RETURN FALSE;

  END Mouse;

PROCEDURE Char (e: E; window: DPSWindow.T; char: CHAR): BOOLEAN =
 VAR ee: DisplayList.E;
  BEGIN
  Init (e);
  ee := e.childWithInputFocus;
  IF ee = NIL THEN RETURN FALSE; END;
  IF ee.Char (window, char) THEN RETURN TRUE; END;
  CASE char OF
  | ' ' =>  NextPage (e); RETURN TRUE;
  | '!' =>  NthPage (e, 0); RETURN TRUE;
  | '@' =>  NthPage (e, 1); RETURN TRUE;
  | '#' =>  NthPage (e, 2); RETURN TRUE;
  | '$' =>  NthPage (e, 3); RETURN TRUE;
  | '%' =>  NthPage (e, 4); RETURN TRUE;
  | '^' =>  NthPage (e, 5); RETURN TRUE;
  | '&' =>  NthPage (e, 6); RETURN TRUE;
  | '*' =>  NthPage (e, 7); RETURN TRUE;
  | '(' =>  NthPage (e, 8); RETURN TRUE;
   ELSE RETURN FALSE; 
    END; (* of CASE *)
  END Char;

BEGIN
END PagerDLE.

