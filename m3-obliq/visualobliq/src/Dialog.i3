(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Sat Oct 23 23:25:22 PDT 1993 by mhb    *)
(*      modified on Fri Sep 10 11:51:47 PDT 1993 by bharat *)
<* PRAGMA LL *>

(* Here we define the screen object *)

INTERFACE Dialog;

IMPORT Attributes, FormsVBT, Rsrc, ZHandleVBT;

VAR
  screens   := 0;
  screen    : ARRAY [1 .. 10] OF T;
  PixelsPerPtHor, PixelsPerPtVer : REAL;
  attributes     : Attributes.T;
  rsrcPath : Rsrc.Path;
 
TYPE
  T <: Public;
  Public = FormsVBT.T OBJECT
    selection : ZHandleVBT.Selection;
    TestMode  : BOOLEAN := FALSE;
    screenindex : CARDINAL;
    grid := 5;
    <* LL <= VBT.mu *>
  END;



PROCEDURE message(fv: FormsVBT.T; txt: TEXT); 
    
(* You need to initialize Dialog.T the FormsVBT.T way and then call *)
(* initSelection to create a new selection                          *)

(* PROCEDURE SetGlobalGrid(n : INTEGER); *)

PROCEDURE SetGlobalBg(n : TEXT);
PROCEDURE SetGlobalFg(n : TEXT);
PROCEDURE SetGlobalFont(n : TEXT);

END Dialog.









