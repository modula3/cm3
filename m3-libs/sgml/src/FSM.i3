(*  SGML parser library                                                    *)
(*  Copyright (C) 1997 Michel Dagenais                                     *)
(*                                                                         *)
(*  This library is free software; you can redistribute it and/or          *)
(*  modify it under the terms of the GNU Library General Public            *)
(*  License as published by the Free Software Foundation; either           *)
(*  version 2 of the License, or (at your option) any later version.       *)
(*                                                                         *)
(*  This library is distributed in the hope that it will be useful,        *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of         *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      *)
(*  Library General Public License for more details.                       *)
(*                                                                         *)
(*  You should have received a copy of the GNU Library General Public      *)
(*  License along with this library; if not, write to the Free             *)
(*  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.     *)
(*                                                                         *)
(*  For more information on this program, contact Michel Dagenais at       *)
(*  dagenais@vlsi.polymtl.ca or Electrical Eng. Dept., Ecole Polytechnique *)
(*  P.O. Box 6079, Station A, Montreal, Quebec, Canada, H3C 3A7.           *)

INTERFACE FSM;

(* This interface defines deterministic finite state machines (FSM). The
   FSM is incrementally built from primitive operations (or, and, optional,
   repeat), usually during the parsing of the expression representing the
   FSM behavior. The FSM may then be wrapped up before being used.

   The states in the FSM are represented by Nodes, and the events triggering
   the transitions between states are represented by Atom.T objects.
   Starting from the current state (initially StartNode), the FSM determines
   the next state given a specified event. It is possible
   to determine if the FSM final state is reachable, or if only one type
   of event is acceptable from the current state.

   Building a FSM has a linear time complexity with respect to its size.
   Simulating the FSM has a linear time complexity with respect to the
   number of simulated events. *)

IMPORT Atom;

EXCEPTION
  Error(TEXT);

TYPE
  Node <: REFANY;

  T <: REFANY;

  Edge = RECORD 
      label: Atom.T; 
      destination: Node;
    END;

(* A FSM is constructed by nested groupings of smaller unwrapped FSM, using
   the Or, Sequence, Repeat, and Optional operations. *)

PROCEDURE New(VAR m: T; type: Atom.T);

(* Create a new FSM in "m" accepting the event "type". A NIL "type"
   allows a direct transition without receiving any event, to be used
   for empty FSM ready to exit. *)

PROCEDURE NewElse(VAR m: T);

(* Create a new FSM in "m" accepting any event, to be used as "ELSE"
   within a Or construct. *)

PROCEDURE Sequence(VAR m1, m2, result: T);

PROCEDURE Or(VAR m1, m2, result: T);

(* Create a new FSM "result" by destructively using the input FSM "m1" and
   "m2". The "result" FSM represents respectively a Sequence, or an Or
   combination of the input FSM. *)

PROCEDURE Optional(VAR m, result: T);

PROCEDURE Repeat(VAR m, result: T);

(* Create a new FSM "result" by destructively using the input FSM "m".
   The result FSM represents respectively the optional content of "m",
   or a possibly empty Repetition of the content of "m". *)

PROCEDURE Copy(READONLY m: T; VAR result: T);

(* Since FSM are destructed when used as input to combined FSM, their
   reference should not be assigned to several variables, or reused.
   If the same FSM is needed twice, for example to have "one or more X"
   as "X followed by zero or more X", the Copy procedure must be used. 
   In that case, "m" is preserved while creating "result". *)

(* Once the FSM is fully built, it needs some pre-processing before
   being used. *)

PROCEDURE Wrap(VAR m: T) RAISES {Error};

(* Wrap prepares the the FSM structure to efficiently perform the
   Enter, Exit, and Expect procedures. The following checks are also
   performed, and an error is raised in case of failure. The FSM should
   be deterministic (each event type is specified only once at each node),
   and there should be no ambiguity for optional (nested optional or repeat
   constructs) or "ELSE" (consecutive NIL "type") constructs. *)

(* The FSM may be used to determine if an event satisfies the FSM and
   find the next state. *)

PROCEDURE StartNode(READONLY m: T): Node;

(* Get the initial state for the FSM "m". *)

PROCEDURE Enter(currNode: Node; type: Atom.T; VAR nextNode: Node): BOOLEAN;

(* Given a current state "currNode" and an event "type", determine the
   next state "nextNode". The procedure returns "TRUE" if the event is
   acceptable from the current state. *)

PROCEDURE Exit(currNode: Node): BOOLEAN;

(* The procedure returns "TRUE" if the final state of the FSM is reachable.
   It is possible to have the final state reachable while more events
   could be accepted, for instance if the FSM ends with a Repeat construct. *)

PROCEDURE Expect(currNode: Node): Atom.T;

(* The procedure returns an event type if only one is acceptable from the
   current state "currNode". It returns NIL if several event types are
   acceptable. *)

(* When the event received does not satisfy the FSM, is may be useful
   to analyze or print the FSM content, to help understand which events
   would be accepted. *)

PROCEDURE GetNodes(m: T; VAR first, last: Node): REF ARRAY OF Node;

(* Return an array of the nodes contained in the FSM. The parameters
   "first" and "last" are filled with the begin and end of the FSM graph. *)

PROCEDURE NodeId(m: T; n: Node): CARDINAL;

(* Return a unique identifier within the FSM for a node. *)

PROCEDURE NodeContent(m: T; n: Node; VAR id: CARDINAL; 
    VAR next: REF ARRAY OF Edge; VAR else, skip: Node);

(* Return the content of a node. The "id" argument is useful to uniquely
   identify each node. The "next" array specifies all the accepted events
   and the associated next node for each. Some nodes have an "else" edge
   to cover all other events. Some nodes have a "skip" edge representing
   a transition without event. *)

END FSM.
