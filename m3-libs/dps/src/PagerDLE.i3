(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:15:51 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 18:03:39 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)



INTERFACE PagerDLE;

(*  A PagerDLE.T is a subclass of a DisplayList.T. (And similarly
    for a PagerDLE.E.)

    The PagerDLE implementation allows the client to define
    a sequence of logical "child" display list elements, and to have
    those elements "paged" as real children of the PagerDLE.T, one
    at a time.

    Methods allow the client to append a logical page to the
    (initially empty) sequence, to display the next page, and to 
    display the nth page. *)

IMPORT DisplayList, DPS, DPSWindow, PopupMenuDLE;

TYPE T = DisplayList.T OBJECT 
  initialized: BOOLEAN := FALSE;
  canMouseChildren: BOOLEAN := TRUE;
  popup: PopupMenuDLE.T;
  pages: REF ARRAY OF DisplayList.T;
  page: INTEGER := -1;
  pageCount: INTEGER := 0;
 METHODS
  AppendPage (page: DisplayList.T) := AppendPage;
  NthPage (page: INTEGER) := NthPage;
  NextPage () := NextPage;
 OVERRIDES
  Mouse := Mouse;
  Char := Char;
  END;
TYPE E = T;

PROCEDURE Init ( t: T; pages: REF ARRAY OF DisplayList.T := NIL );

PROCEDURE AppendPage ( t: T; page: DisplayList.T );

PROCEDURE NthPage (t: T; page: INTEGER);
PROCEDURE NextPage (t: T);

PROCEDURE Mouse (e: E; t: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN;
PROCEDURE Char (e: E; t: DPSWindow.T; char: CHAR): BOOLEAN;
 
  END PagerDLE.



