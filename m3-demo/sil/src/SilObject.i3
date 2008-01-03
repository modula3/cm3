(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Nov  4 13:11:51 PST 1994 by kalsow    *)

(* generic graphical objects *)

INTERFACE SilObject;

IMPORT Point, Rect;
IMPORT SilWindow, SilFont, SilRd, SilWr;

CONST
  Selected = 1;
  Visible  = 0;
(*Deleted  < 0*)
  
TYPE
  T <: T_; T_ = OBJECT
    next  : T       := NIL;
    state : INTEGER := Visible;
    bbox  : Rect.T  := Rect.Empty;
  METHODS
    draw (READONLY origin: Point.T;  selected: BOOLEAN;  w: SilWindow.T);

    select (READONLY clip       : Rect.T;
                     pointMode  : BOOLEAN;
                     dropOthers : BOOLEAN;
          VAR(*OUT*) bbox       : Rect.T): BOOLEAN;

    deleteSelected (VAR(*OUT*) bbox: Rect.T): BOOLEAN;

    takeChar (ch: CHAR;  w: SilWindow.T;  VAR(*OUT*) bbox: Rect.T): BOOLEAN;

    clone (): T;

    write (wr: SilWr.T);

    setFont (f: SilFont.T;  selected: BOOLEAN;  w: SilWindow.T): BOOLEAN;

    setWidth (width: INTEGER;  selected: BOOLEAN): BOOLEAN;
  END;

PROCEDURE WriteBBox (t: T;  wr: SilWr.T);
PROCEDURE ReadBBox  (t: T;  rd: SilRd.T);

END SilObject.
