(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Wed May 12 16:36:27 PDT 1993 by swart          *)
(*      modified on Fri Nov  3 11:55:47 1989 by muller         *)
(*      modified on Thu Nov  2 18:18:07 1989 by gnelson        *)
(*      modified on Thu Nov  2 18:18:07 1989 by manasse        *)

INTERFACE AutoFlushWr;

(* Writers that are automatically flushed; useful for writing to
   terminals. *)

IMPORT Wr, Time;

TYPE
  T <: Public;
  Public = Wr.T OBJECT METHODS init (ch: Wr.T; p: Time.T := -1.0D0): T END;

(* After wr := NEW(T).init(ch), everything written to wr will be forwarded 
   to ch (that is, target(wr) = target(ch)).  Furthermore, wr will 
   be flushed automatically by a background thread every p seconds 
   (approximately).  If p is less than 0, it defaults to a value that is 
   reasonable for writing to terminals.  The child ch must be unlocked 
   when init is called. After ch has been passed to init, you must not 
   operate on ch (except implicitly through wr).  Flushing wr flushes 
   ch; closing wr closes ch. *)

END AutoFlushWr.
