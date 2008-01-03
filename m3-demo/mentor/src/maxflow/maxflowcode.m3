(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

@Maxflow
PROCEDURE Maxflow =@

  BEGIN
@1  WHILE there is an augmenting path DO @
@2    p := FindPath (flowChange); @
@3    currentVertex := source; @
@4    WHILE p # NIL DO  @
@5      edge := List.Pop(p); @
@6      IF edge.from = currentVertex (* forward edge *) THEN @
@7        edge.flow := edge.flow + flowChange; @
@8        currentVertex := edge.to; @
@9      ELSE (* back edge *) @
@10       edge.flow := edge.flow - flowChange; @
@11       currentVertex := edge.from; @
        END;
      END;
    END;
  END Maxflow;


PROCEDURE FindPath (VAR flowChange: REAL) : List.T ;

(* returns a list of edges which when undirected form a 
   path from the source to the sink. Every forward edge
   on this path has nonzero residual capacity 
   (i.e. capacity > flow),
   and every back edge on this path has positive flow.
   Such a path is called an augmenting path.
   flowChange :=
      minimun ( minimum residual capacity on forward edges,
                minimum flow on back edges )
*)

@Maxflow
