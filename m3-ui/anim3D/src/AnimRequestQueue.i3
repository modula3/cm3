(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Tue Jul 26 16:24:08 PDT 1994 by najork                   *)


INTERFACE AnimRequestQueue;

IMPORT AnimHandle, Prop;

TYPE 
  T <: Public;
  Public = OBJECT
  METHODS 
    init (ah : AnimHandle.T) : T;
    insert (req : Prop.Request) RAISES {Prop.BadInterval};
    duration () : REAL;
    flush ();
  END;

END AnimRequestQueue.
