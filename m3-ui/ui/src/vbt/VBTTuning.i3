(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* VBTTuning.i3, coded Fri Oct 31 11:24:53 1986 by cgn *)
(* Last modified on Mon Feb 24 13:59:03 PST 1992 by muller  *)
(*      modified on Sat Dec 21 16:39:35 PST 1991 by gnelson *)
(*      modified on Fri Feb  2 14:08:01 PST 1990 by glassman *)
(*      modified on Mon Jan 12 17:12:01 1987 by msm *)
<*PRAGMA LL*>

(* This interface defines values that can be changed to 
   maximize Trestle's performance on particular systems. *)
   
INTERFACE VBTTuning;

IMPORT Word;

CONST
  BatchSize: CARDINAL = 325; 
  BatchLatency: CARDINAL = 50000;
  HVParlim: CARDINAL = 100000;
  ZParlim: CARDINAL = 100000;
  ResumeLength: CARDINAL = 1;
  CombineLimit: CARDINAL 
    = (BatchSize * ADRSIZE(Word.T)) DIV 2;
   
(* The value "BatchSize" is the number of "Word.T"'s in a standard
   painting batch.

   The value "BatchLatency" is the number of microseconds before a 
   paint batch is automatically forced.

   The values "HVParlim" and "ZParlim" are the default
   minimum child areas (in pixels) for which "ZSplit" and "HVSplit" 
   will fork separate repaint or reshaping threads. 

   "ResumeLength" is the size that a queue of paint batches must
   shrink to before a cross-address space filter will unblock a
   thread that painted into an overfull queue.  It must
   be at least 1. 
   
   The value "CombineLimit" is the number of addressable 
   units (e.g., bytes) in a batch beyond which Trestle will
   not consider combining another batch into it. *)

END VBTTuning.
