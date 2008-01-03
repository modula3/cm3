(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Oct  3 16:33:09 PDT 1995 by najork                   *)
(*       Created on Tue Jul 11 22:37:39 PDT 1995 by najork                   *)


MODULE Win_OpenGL_Base EXPORTS Win_OpenGL_Base, Win_OpenGL_BaseProxy;


IMPORT GraphicsBase;

REVEAL
  T = Public BRANDED OBJECT
  OVERRIDES
    init := Init;
  END;


PROCEDURE Init (<*UNUSED*> self      : T; 
                <*UNUSED*> title     : TEXT; 
                <*UNUSED*> x, y, w, h: INTEGER): T 
    RAISES {GraphicsBase.Failure} =
  BEGIN
    RAISE GraphicsBase.Failure;
  END Init;


PROCEDURE Available () : BOOLEAN =
  BEGIN
    RETURN FALSE;
  END Available;


BEGIN
END Win_OpenGL_Base.
