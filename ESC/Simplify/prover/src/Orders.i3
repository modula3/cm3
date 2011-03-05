(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright 95 Digital Equipment Corporation.
   Digital Internal Use Only
   Last modified on Sat Apr 29 13:41:28 PDT 2000 by saxe   
        modified on Wed Dec 13 16:31:24 PST 1995 by detlefs
*)

(* The "Orders" interface implements a set of partial orders over
   enodes; this is a "theory" in the sense of the "Context" interface.
   Each partial order is associated with a pair of function symbols,
   representing the strict and non-strict part of the the partial
   order.  Operations on the partial orders are undoable; that is,
   this interface actually implements a stack of partial order states.
*)

INTERFACE Orders;

IMPORT Enode, OrdNode, AF, Atom, RefList, Prover;

REVEAL
  Enode.OrdersMisc = Enode.SimplexMisc BRANDED OBJECT
    ordNode: OrdNode.T;
  END (* OBJECT *);

(* If "e" is an enode that is represented in one or more partial
   orders, then "e.ordNode" is the representative for the that enode
   in the partial orders.
*)

PROCEDURE DclRelLit(gtSym, geSym: Atom.T): AF.Lit RAISES { Prover.Error };
(* Returns a literal that, when asserted in the positive sense, declares
   "gtSym" and "geSym" to be the strict and non-strict parts,
   respectively, of a new partial order (which is added to the set
   represented by this interface).  Raises "Prover.Error" if either
   function symbol is already part of a partial order, or if "gtSym"
   and "geSym" are the same symbol.  The effect of denying such a
   literal are undefined.
*)

PROCEDURE IsRel(sym: Atom.T): BOOLEAN;
(* Returns "TRUE" iff "sym" is a function symbol in a declared partial
   order. 
*)

PROCEDURE NewEdgeLit(fsym: Atom.T;
                     e1, e2: Enode.T;
                     sense := TRUE): AF.Lit RAISES { Prover.Error };
(* Returns a literal that asserts or denies (according to "sense")
   that "e1" and "e2" are related by "fsym", which must be a declared
   function symbol of a partial order (or else "Prover.Error" is raised.)
   The assertion of such a literal in the positive sense will return
   "TRUE" iff the addition of the pair "e1, e2" permits the partial
   order of which "fsym" is one of the function symbols to remain a
   partial order.
*)

PROCEDURE PropEqs(ord1, ord2: OrdNode.T);
(* The enodes represented by "ord1" and "ord2" have been merged.
   Propogate this equality to all partial orders (if any) containing
   both "ord1" and "ord2".
*)

PROCEDURE Push();
(* Store the state of the partial orders, so that it will be recovered
   by a subsequent "Pop". *)

PROCEDURE Pop();
(* Return to the last state saved by "Push"; requires that such a
   state exists. *)

PROCEDURE Top(): RefList.T;
(* Returns an S-expression representation of the partial orders. *)

PROCEDURE Init();
(* Initializes the state to contain no partial orders. *)

PROCEDURE Init0();
(* Must be called before any other procedures. *)

PROCEDURE PrintAll();
(* Prints the state of all ordering theories currently in use.
   For tracing. *)

END Orders.






