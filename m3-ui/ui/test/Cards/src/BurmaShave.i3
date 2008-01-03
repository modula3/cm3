(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:48:05 PST 1992 by muller   *)
(*      modified on Sun Nov 10 17:47:10 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:54:07 PDT 1991 by msm      *)
<*PRAGMA LL*>

INTERFACE BurmaShave;

IMPORT VBT;

TYPE
  T <: Public; 
  Private <: VBT.Leaf; 
  Public =  Private OBJECT METHODS init(): T END;
  
PROCEDURE New(): T RAISES {};

END BurmaShave.
