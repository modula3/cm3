(* Copyright (C) 1989, 1990 Digital Equipment Corporation	*)
(* All rights reserved.						*)
(* See the file COPYRIGHT for a full description.		*)

(* File: XtE.i3							*)
(* Last modified on Fri May  7 15:09:23 PDT 1993 by mjordan     *)  
(*      modified on Fri Apr 13 14:09:03 1990 by jerome		*)
(*      modified on Mon Feb 26 21:51:24 1990 by muller		*)


INTERFACE XtE;

(*==============================================================*)
(*	The X11 R4 Interface for Modula 3			*)
(*								*)
(*	Representation Types  corresponding to:			*)
(*			X11 R4 Intrinsic			*)
(*			X11 R4 Athena Widget Set		*)
(*==============================================================*)


FROM Ctypes IMPORT char_star;

VAR

  Ellipse                        : char_star;                        
  Oval                           : char_star;                     
  Rectangle                      : char_star;                          
  RoundedRectangle               : char_star;                                 
  always                         : char_star;                       
  center                         : char_star;                       
  default                        : char_star;                        
  file                           : char_star;
  left                           : char_star;                     
  notUseful                      : char_star;                          
  right                          : char_star;                      
  string                         : char_star;                       
  textScrollNever                : char_star;
  textScrollWhenNeeded           : char_star;
  textScrollAlways               : char_star;
  textWrapNever                  : char_star;
  textWrapLine                   : char_star;
  textWrapWord                   : char_star;
  textResizeNever                : char_star;
  textResizeWidth                : char_star;
  textResizeHeight               : char_star;
  textResizeBoth                 : char_star;
  whenMapped                     : char_star;


PROCEDURE ForceToLoadAnImplementation ();

END XtE.
