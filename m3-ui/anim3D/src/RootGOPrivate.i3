(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Jul 29 13:41:18 PDT 1994 by najork                   *)
(*       Created on Wed Feb 16 18:24:35 PST 1994 by najork                   *)


INTERFACE RootGOPrivate;

IMPORT CameraGO, GraphicsBase;

FROM RootGO IMPORT T, Public;

REVEAL T <: Private;

TYPE
  Private = Public OBJECT
    cam  : CameraGO.T;
    base : GraphicsBase.T;
  END;

(* "root.repair(damaged)" redraws the scene rooted at "root". Only those
   nodes that were marked as damaged are re-rendered, for the other nodes,
   cached values are used. 

   Nodes can be damaged in two ways: 
   \begin{enumerate}
   \item By operations that change the scene DAG (i.e.\ the group operations
         "add", "remove", and "flush").
   \item By changes in the value of an attached property. 
   \end{enumerate}

   Calling "root.adjust(time)" serves two purposes: It reevaluates all the 
   properties attached to all descendants of "root", and damages those nodes 
   whose properties have changed since the last round of rendering. It also
   propagates damages up the scene DAGs.
       "
   "root.repair(damaged)" is called after all roots have been adjusted. The 
   VAR parameter "damaged" is set to TRUE if there were any damages in the 
   scene, otherwise, it remains unchanged. *)

END RootGOPrivate.
