(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:52:53 PST 1992 by muller   *)
(*      modified on Wed Sep 11 15:28:15 PDT 1991 by msm      *)
<*PRAGMA LL*>

INTERFACE BtnVBTClass;

IMPORT ButtonVBT;

TYPE T = ButtonVBT.Public BRANDED OBJECT 
  ready, armed := FALSE 
END;

(* The "ready" boolean is set if the "pre" method has been called.
   The "armed" boolean is set if an up transition over the button
   would cause the action procedure to be called. *)

REVEAL ButtonVBT.T <: T;
  
END BtnVBTClass.

