(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Mon Feb 10 17:52:10 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)



INTERFACE cDPS;

IMPORT DPS;

  <*EXTERNAL*> PROCEDURE doinitialize ();

  <*EXTERNAL*> PROCEDURE docreatesimplewindow (ctx: DPS.T; width, height: INTEGER := 600);

  <*EXTERNAL*> PROCEDURE dosendps (ctx: DPS.T; string: UNTRACED REF CHAR);
  <*EXTERNAL*> PROCEDURE dosendpsnervously (ctx: DPS.T; string: UNTRACED REF CHAR): INTEGER;

  <*EXTERNAL*> PROCEDURE stufftransforms ( win: DPS.T );
  <*EXTERNAL*> PROCEDURE transformtodps ( win: DPS.T;
     x, y: INTEGER; ux, uy: UNTRACED REF REAL );

  <*EXTERNAL*> PROCEDURE xyupathhit (ctx: INTEGER; x, y: REAL; upath: UNTRACED REF CHAR): INTEGER;

  <*EXTERNAL*> PROCEDURE doprocessinputs ( dpy: INTEGER; 
     win, event: UNTRACED REF INTEGER;
     but, modifiers: UNTRACED REF INTEGER;
     x, y, w, h: UNTRACED REF INTEGER );

  <*EXTERNAL*> PROCEDURE doflush (ctx: DPS.T);
  <*EXTERNAL*> PROCEDURE dowait (ctx: DPS.T);

  <*EXTERNAL*> PROCEDURE noticeCursor (win: DPS.T);

  END cDPS.

