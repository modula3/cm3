(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Thu May 25 15:49:27 PDT 1995 by najork                   *)

(* A "GroupGO.T" is a geometric object that contains a collection of other
   geometric objects.

   Groups serve two purposes: They allow the user to group indiviual
   geometric objects together, and they provide a mechanism for property
   inheritance. 

   Groups allow the user to organize individual geometric objects into 
   a graph structure. A {\em scene} is described by a {\em root node} 
   (a \type{RootGO}{T}, which is a subtype of "GroupGO.T"), and contains 
   all the geometric objects reacheable from the root. Usually, this 
   structure forms a tree. However, geometric objects may occur in more 
   than one group, so in general, the objects in a scene form a DAG 
   (the rendering process does not terminate for cyclic graphs). 

   During rendering, the DAG is travered in a depth-first fashion. Whenever 
   a node "o" is visited, there is a unique traversal path between "o" and 
   the root. We call the nodes on this path the {\em ancestors} of "o". 

   Associated with each geometric object "o" is a property mapping $M_o$,
   a partial function from property names $n$ to property values $n$
   (see the \interface{GO} interface for details). 
   When the scene is traversed, the property mappings of the nodes on the
   traversal path are composed together. Composition of property mappings 
   is defined as follows:
   \[ (M_i \circ M_{i+1})(n) = 
     \left\{
      \begin{array}{ll}
       M_i(n) & \mbox{if $M_{i+1}(n)$ is undefined} \\
       M_{i+1}(n) & \mbox{if $M_i(n)$ is undefined} \\
       M_i(n) \oplus M_{i+1}(n) & \mbox{otherwise}
      \end{array}
     \right.
   \]
   $\oplus$ is the {\em property value composition operator}. Its semantics
   depends of the type of the property values. At the moment, for all 
   properties except transformation properties, $v \oplus v' = v'$.
   A transformation property value $v$ is internally described by a $4\times4$
   matrix $A_v$. For transformation property values, 
   $v \oplus v' = v''$ where $A_{v''} = A_v A_{v'}$.
   
   Here is an example of how this property inheritance mechanism works:
   \begin{center}
   \begin{tabular}{c}
   \psfig{figure=images/PropInheritance.ps,width=4in,silent=} 
   \end{tabular}
   \end{center}
   The actual scene contains three objects, a sphere, a box, and a cone.
   The sphere and the box are red, while the cone is blue.

   % (This is a latex-comment)
   % Add (or provide a hyperlink to) the actual program and the actual image.
*)

INTERFACE GroupGO;

IMPORT GO;

EXCEPTION BadElement;

TYPE
  T <: Public;
  Public = GO.T OBJECT
  METHODS
    init (initSize := 5) : T;
    add (o : GO.T);
    remove (o : GO.T) RAISES {BadElement};
    flush ();
    content () : REF ARRAY OF GO.T;
  END;
(* "g.init(size)" initializes and returns a new group "g". Initially, "g"
   has room for "size" elements; whenever "g" fills up, its size doubles.

   "g.add(o)" adds a new geometric object "o" to the group "g".

   "g.remove(o)" removes the geometric object "o" from the group "g". If "o" 
   is not contained in "g", the exception "BadElement" is raised.

   "g.flush()" removes all geometric objects from "g". 

   "g.content()" returns an array containing all the elements of "g".
*)

PROCEDURE New (initSize := 5) : T;

END GroupGO.
