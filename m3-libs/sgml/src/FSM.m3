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

MODULE FSM;

IMPORT RefSeq, Atom, AtomRefTbl, DeepCopy, TextRd, TextWr;

(* IMPORT AtomPkl; <*NOWARN*>  It is used for its side effect *)

REVEAL
  T = BRANDED REF RECORD
      firstNode, lastNode: Node;
      nodes: RefSeq.T;
    END;

  Node = BRANDED REF RECORD
      replacedBy: Node;
      replaces: RefSeq.T;
      next: AtomRefTbl.T;
      else: Node;
      skip: Node;
      id: CARDINAL;
    END;

(* As FSM are combined, nodes are merged together (one replacing the other).
   The links to merged nodes are not updated until the end to insure a linear
   time complexity. Each node remembers the nodes it replaces and
   the node it is replaced by. The "next" table stores the next state
   for each accepted event. The "else" at each node handles all events 
   not otherwise specified in the "next" state table. The "skip" points
   to states which may be entered directly without any event. *)

PROCEDURE NewNode(): Node =
  BEGIN
    RETURN NEW(Node, else := NIL, skip := NIL, replacedBy := NIL, replaces := 
        NEW(RefSeq.T).init(), next := NEW(AtomRefTbl.Default).init());
  END NewNode;

PROCEDURE ReplaceNode(n2, n1: Node) =
  BEGIN
    <* ASSERT n2.replacedBy = NIL *>
    n2.replacedBy := n1;
    n1.replaces.addhi(n2);
  END ReplaceNode;

(* A NIL type is used to set the "skip" transition used for optional
   or repeat constructs. *)

PROCEDURE New(VAR m: T; type: Atom.T) =
  BEGIN
    m := NEW(T, firstNode := NewNode(), lastNode := NewNode());
    IF type = NIL THEN 
      m.firstNode.skip := m.lastNode;
    ELSE
      EVAL m.firstNode.next.put(type,m.lastNode);
    END;
  END New;

(* An edge which accepts any event, used for ELSE transitions in Or. *)

PROCEDURE NewElse(VAR m: T) =
  BEGIN
    m := NEW(T, firstNode := NewNode(), lastNode := NewNode());
    m.firstNode.else := m.lastNode;
  END NewElse;

(* expression for "m1" followed by expression for "m2" *)

PROCEDURE Sequence(VAR m1, m2, result: T) =
  BEGIN
    ReplaceNode(m2.firstNode, m1.lastNode);
    result := m1;
    result.lastNode := m2.lastNode;
    m1 := NIL;
    m2 := NIL;
  END Sequence;

(* expresion for "m1" or expression for "m2" *)

PROCEDURE Or(VAR m1, m2, result: T) =
  BEGIN
    ReplaceNode(m2.firstNode, m1.firstNode);
    ReplaceNode(m2.lastNode, m1.lastNode);
    result := m1;
    m1 := NIL;
    m2 := NIL;
  END Or;

(* Zero or one "m" *)

PROCEDURE Optional(VAR m, result: T) =
  BEGIN
    New(result,NIL);
    ReplaceNode(m.firstNode, result.firstNode);
    ReplaceNode(m.lastNode, result.lastNode);
    m := NIL;
  END Optional;

(* Zero or more "m" *)

PROCEDURE Repeat(VAR m, result: T) =
  BEGIN
    New(result,NIL);
    ReplaceNode(m.firstNode, result.firstNode);
    ReplaceNode(m.lastNode, result.firstNode);
    m := NIL;
  END Repeat;


PROCEDURE Copy(READONLY m: T; VAR result: T) =
BEGIN
  result := DeepCopy.Copy(m);
END Copy;

(* Relink all the links in "next", "else", and "skip" transitions to account
   for merged (replaced) nodes. *)

PROCEDURE Wrap(VAR m: T) RAISES {Error} =
  BEGIN
    m.nodes := NEW(RefSeq.T).init();
    RelinkNode(m,m.firstNode);
  END Wrap;

PROCEDURE RelinkNode(m: T; n: Node) RAISES {Error} =
  VAR
    node, destNode, nextNode: Node;
    iter := n.next.iterate();
    type: Atom.T;
    replaces := n.replaces;
    tmp: REFANY;
  BEGIN
    destNode := n;

    (* Store all the non replaced nodes in a sequence for easier acces later.*)

    IF n.replacedBy = NIL THEN
      n.id := m.nodes.size();
      m.nodes.addhi(n);
    END;

    (* Determine the chain end, the node which really replaces this one
       and all the nodes in between in the "replacedBy" chain. *)

    WHILE destNode.replacedBy # NIL DO destNode := destNode.replacedBy; END;

    (* Make all nodes in the chain point directly to the chain end as
       the real "replacedBy". *)

    node := n;
    WHILE node.replacedBy # NIL DO
      nextNode := node.replacedBy;
      node.replacedBy := destNode;
      node := nextNode;
    END;

    (* Mark the node as processed, as far as "replacedBy" is concerned. *)

    n.replaces := NIL;

    (* Prepare for the updated "next" table as links will be inserted from
       all replaced nodes. *)

    n.next := NEW(AtomRefTbl.Default).init();

    (* Process all the replaced nodes *)

    FOR i := 0 TO replaces.size() - 1 DO
      node := replaces.get(i);
      IF node.replaces # NIL THEN RelinkNode(m,node); END;
    END;

    (* Process the else node and update the else link *)

    IF n.else # NIL THEN
      IF n.else.replaces # NIL THEN RelinkNode(m,n.else); END;
      IF n.else.replacedBy # NIL THEN n.else := n.else.replacedBy; END;
    END;

    (* Process the skip node and update the skip link *)

    IF n.skip # NIL THEN
      IF n.skip.replaces # NIL THEN RelinkNode(m,n.skip); END;
      IF n.skip.replacedBy # NIL THEN n.skip := n.skip.replacedBy; END;
    END;

    (* Use the replacing node, if any, as new origin for the links.
       Process the replacedBy node, and merge the "else" and "skip"
       transitions. *)

    IF n.replacedBy # NIL THEN
      nextNode := n.replacedBy;
      IF nextNode.replaces # NIL THEN RelinkNode(m,nextNode); END; 

      (* If two merged nodes both specify "else" or "skip" transitions,
         there is ambiguity. Raise an error. *)

      IF n.else # NIL THEN
        IF nextNode.else # NIL THEN
          RAISE Error("Too many ANY applicable");
        ELSE
          nextNode.else := n.else;
        END;
      END;

      IF n.skip # NIL THEN
        IF nextNode.skip # NIL THEN
          RAISE Error("Too many optional/repeat applicable");
        ELSE
          nextNode.skip := n.skip;
        END;
      END;

    ELSE 
      nextNode := n; 
    END;

    (* Insert all the transitions from the "next" table into the new
       "next" table with the next node properly updated. *)

    WHILE iter.next(type,tmp) DO
      node := tmp;
      IF node.replaces # NIL THEN RelinkNode(m,node); END;
      IF node.replacedBy # NIL THEN node := node.replacedBy; END;

      (* Two merged nodes both specified a transition for the same event.
         Raise an error. *)

      IF nextNode.next.get(type,tmp) THEN 
        RAISE Error("Duplicate " & Atom.ToText(type));
      END;
      EVAL nextNode.next.put(type,node);
    END;
  END RelinkNode;

PROCEDURE StartNode(READONLY m: T): Node =
  BEGIN
    RETURN m.firstNode;
  END StartNode;

PROCEDURE Enter(currNode: Node; type: Atom.T; VAR nextNode: Node): BOOLEAN =
  VAR
    tmp: REFANY;
  BEGIN
    LOOP
      (* This is an acceptable transition from the current state. *)

      IF currNode.next.get(type,tmp) THEN
        nextNode := NARROW(tmp,Node);
        RETURN TRUE;
      END;

      (* The else clause accepts anything. *)

      IF currNode.else # NIL THEN
        nextNode := currNode.else;
        RETURN TRUE;
      END;

      (* Exit the current "optional" or "repeat" construct which cannot
         accept the specified event. Perhaps the event is acceptable
         further down in the FSM. *)

      IF currNode.skip # NIL THEN
        currNode := currNode.skip;
      ELSE
        RETURN FALSE;
      END;
    END;
  END Enter;

PROCEDURE Exit(currNode: Node): BOOLEAN =
  BEGIN
    LOOP
      (* The final FSM exit node was reached *)

      IF currNode.next.size() = 0 AND currNode.else = NIL AND
         currNode.skip = NIL THEN 
        RETURN TRUE; 
      END;

      (* Exit the current Optional or Repeat construct to see if the final
         FSM exit node may be reached. *)

      IF currNode.skip # NIL THEN
        currNode := currNode.skip;
      ELSE
        RETURN FALSE;
      END;
    END;
  END Exit;

PROCEDURE Expect(currNode: Node): Atom.T =
  VAR
    type: Atom.T;
    tmp: REFANY;
  BEGIN
    (* There is only one type of event accepted at this node. *)

    IF currNode.next.size() = 1 AND currNode.else = NIL THEN
      EVAL currNode.next.iterate().next(type,tmp);
      RETURN type;
    ELSE
      RETURN NIL;
    END;
  END Expect;

PROCEDURE GetNodes(m: T; VAR first, last: Node): REF ARRAY OF Node =
  VAR
    a := NEW(REF ARRAY OF Node,m.nodes.size());
  BEGIN
    FOR i := 0 TO m.nodes.size() - 1 DO a[i] := m.nodes.get(i); END;
    first := m.firstNode;
    last := m.lastNode;
    RETURN a;
  END GetNodes;

PROCEDURE NodeId(<*UNUSED*>m: T; n: Node): CARDINAL =
  BEGIN
    RETURN n.id;
  END NodeId;

PROCEDURE NodeContent(<*UNUSED*>m: T; n: Node; VAR id: CARDINAL; 
    VAR next: REF ARRAY OF Edge; VAR else, skip: Node) =
  VAR
    label: Atom.T;
    destination: REFANY;
  BEGIN
    id := n.id;
    else := n.else;
    skip := n.skip;
    next := NEW(REF ARRAY OF Edge,n.next.size());
    WITH iter = n.next.iterate() DO
      FOR i := 0 TO n.next.size() - 1 DO
        EVAL iter.next(label,destination);
        next[i] := Edge{label,destination};
      END;
    END;
  END NodeContent;

BEGIN
END FSM.
