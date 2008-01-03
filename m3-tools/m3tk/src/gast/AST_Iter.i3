(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* "AST_Iter" provides an another way to systematically visit
the nodes of an AST. *)

INTERFACE AST_Iter;

IMPORT AST;
IMPORT AST_Name AS Previous_View;

TYPE 
  NODE = Previous_View.NODE OBJECT
    METHODS
      newIter(): T := Null;
  END;

REVEAL AST.NODE <: NODE;

TYPE
  T <: T_public;
  T_public = OBJECT
  METHODS
    next(VAR (*out*) n: AST.NODE): BOOLEAN RAISES {};

    update(nn: AST.NODE);
    (* If "SELF.next(r)" would return "TRUE", replace the child
        with "nn", else a checked
       run-time error. No actual call of "next" takes place. *)
  END;

PROCEDURE Null(n: NODE): T RAISES {}; 
(* Returns an iterator that always returns "FALSE" on a call of "next". *)

END AST_Iter.

(* The "newIter" method returns an iterator that will return the
children of the node in some order.  A default procedure that returns
none of the children is provided for "newIter". The designer of a
given AST is responsible for providing a specific procedure for each
subtype of an "AST.NODE". The order in which the children are returned
will be the same as that provided by the "walk" method described in
the preceding section.

The "next" method return "FALSE" if there are no more children of this
node else it sets "n" to the next child, steps the iterator, and
returns "TRUE".

If a call of "self.next(r)" would return "TRUE", a call of
"self.update(nn)" will replace the child ((which currently has value
"r") with the value "nn", otherwise a checked runtime error will
occur. *)

(* \subsubsection{Example} *)

(* Using the iterator is straightforward and most procedures that
   visit nodes in a tree will take the following form:

| PROCEDURE Visit(n: AST.NODE)=
|   BEGIN
|     (* Processing based on "n". *)
|     (* Now visit the children of this node. *)
|     VAR iter := n.newIter(n); child: AST.NODE;
|     BEGIN
|       WHILE iter.next(child) DO
|         IF child # NIL THEN Visit(child) END;
|       END;
|     END;
|  END Visit;

   Using the "update" method is more subtle. First note that updating
a direct or indirect child of a node "n" does not require "update",
but typically can be accomplished by direct assignment, e.g.
"n.child.attribute := value".  The "update" method is needed when you
want to update a node of your parent and there is no way to know
statically which attribute to modify. For example, you might want to
replace all nodes of type "X" with nodes of type "Y", assuming that "Y
<: X", and "X" nodes occur in many different contexts. In this case
the notion you need is update the "ith" child of node "n".  This is
essentially what "update" does, using the fact that the iterator
provides an abstraction for the location of an attribute.  One
consequence of this generality is the lack of compile-time type
checking on the "update" operation.  If an inappropriate value is
supplied for the attribute at the index encoded by the iterator, the
program will cause a checked run-time error ("NARROW" fault).

The following variant of "Visit" passes a reference, using an
iterator, to the location of node "n" in it's parent node. This allows
the processing code to replace the value of "n" in the parent with a
new value.

| PROCEDURE Visit(n: AST.NODE; n_loc: AST_Iter.T)=
|   BEGIN
|     (* processing that leads to an update *)
|     n_loc.update(new_value)
|
|     VAR iter, iter_me := n.newIter(n);
|         child: AST.NODE;
|     BEGIN
|       WHILE iter.next(child) DO
|         IF child # NIL THEN Visit(child, iter_me) END;
|         EVAL iter_me.next(child);
|       END;
|     END;
|  END Visit; 
*)
