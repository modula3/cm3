(* Created on Tue Nov 18 17:23:12 PST 1997 by heydon       *)
(* Last modified on Sat Nov 22 13:22:27 PST 1997 by heydon *)
(* Copyright (C) 1997, Digital Equipment Corporation       *)

GENERIC MODULE RedBlackTbl(Key, Value, Tbl, SortedTbl);

TYPE
  Color = { Red, Black };
  Node = REF RECORD
    k: Key.T;
    v: Value.T;
    color := Color.Red;
    p, l, r: Node := NIL; (* parent, left child, right child *)
  END;

REVEAL
  T = Public BRANDED Brand OBJECT
    nil: Node := NIL;   (* sentinal at leaves of tree *)
    root: Node;         (* tree root node *)
    num: CARDINAL := 0; (* number of elements in the table *)
  OVERRIDES
    keyCompare := KeyCompare;
    init := Init;
    size := Size;
    get := Get;
    put := Put;
    delete := Delete;
    iterate := Iterate;
    iterateOrdered := IterateOrdered;
  END;

CONST
  IterBrand = "(Iterator " & Brand & ")";

REVEAL
  Iterator = IteratorPublic BRANDED IterBrand OBJECT
    t: T;       (* corresponding tree *)
    curr: Node; (* current node in iteration *)
  END;

TYPE
  IteratorUp = Iterator OBJECT OVERRIDES
    reset := ResetUp;
    next := NextUp;
    seek := SeekUp;
  END;
  IteratorDown = Iterator OBJECT OVERRIDES
    reset := ResetDown;
    next := NextDown;
    seek := SeekDown;
  END;

(* Utility procedures ------------------------------------------------------ *)

PROCEDURE Min(t: T; curr: Node): Node =
(* Return the minimum element of the tree rooted at "curr". Requires
   "curr # t.nil". *)
  VAR prev: Node; BEGIN
    <* ASSERT curr # t.nil *>
    REPEAT
      prev := curr;
      curr := curr.l
    UNTIL curr = t.nil;
    RETURN prev
  END Min;

PROCEDURE Max(t: T; curr: Node): Node =
(* Return the maximum element of the tree rooted at "curr". Requires
   "curr # t.nil". *)
  VAR prev: Node; BEGIN
    <* ASSERT curr # t.nil *>
    REPEAT
      prev := curr;
      curr := curr.r
    UNTIL curr = t.nil;
    RETURN prev
  END Max;

PROCEDURE Successor(t: T; n: Node): Node =
  VAR res: Node; BEGIN
    IF n.r # t.nil THEN
      res := Min(t, n.r)
    ELSE
      WHILE n.p # t.nil AND n.p.r = n DO
        n := n.p
      END;
      res := n.p
    END;
    RETURN res
  END Successor;

PROCEDURE Predecessor(t: T; n: Node): Node =
  VAR res: Node; BEGIN
    IF n.l # t.nil THEN
      res := Max(t, n.l)
    ELSE
      WHILE n.p # t.nil AND n.p.l = n DO
        n := n.p
      END;
      res := n.p
    END;
    RETURN res
  END Predecessor;

PROCEDURE LeftRotate(t: T; p, ch: Node) =
(* Requires that "ch" is the right child of "p". Do a left-rotation
   about those two nodes. *)
  BEGIN
    <* ASSERT ch # t.nil AND ch = p.r *>
    (* make "p"'s right child "ch"'s left child *)
    p.r := ch.l;
    p.r.p := p;

    (* adjust "ch"'s parent *)
    ch.p := p.p;
    IF p.p = t.nil THEN
      (* "p" was the tree root; make "ch" the new root *)
      t.root := ch
    ELSE
      IF p = p.p.l
        THEN p.p.l := ch
        ELSE p.p.r := ch
      END
    END;

    (* make "p" "ch"'s new left child *)
    ch.l := p;
    p.p := ch;
  END LeftRotate;

PROCEDURE RightRotate(t: T; p, ch: Node) =
(* Requires that "ch" is the left child of "p". Do a right-rotation
   about those two nodes. *)
  BEGIN
    <* ASSERT ch # t.nil AND ch = p.l *>
    (* make "p"'s left child "ch"'s right child *)
    p.l := ch.r;
    p.l.p := p;

    (* adjust "ch"'s parent *)
    ch.p := p.p;
    IF p.p = t.nil THEN
      (* "p" was the tree root; make "ch" the new root *)
      t.root := ch
    ELSE
      IF p = p.p.l
        THEN p.p.l := ch
        ELSE p.p.r := ch
      END
    END;

    (* make "p" "ch"'s new right child *)
    ch.r := p;
    p.p := ch;
  END RightRotate;

(* Tree method implementations --------------------------------------------- *)

PROCEDURE KeyCompare(<*UNUSED*> t: T; READONLY k1, k2: Key.T): [-1..1] =
  BEGIN RETURN Key.Compare(k1, k2) END KeyCompare;

PROCEDURE Init(t: T): T =
  BEGIN
    IF t.nil = NIL THEN
      (* initialization *)
      t.nil := NEW(Node, color := Color.Black)
    ELSE
      (* clear existing table *)
      t.num := 0
    END;
    t.root := t.nil;
    RETURN t
  END Init;

PROCEDURE Size(t: T): CARDINAL =
  BEGIN RETURN t.num END Size;

PROCEDURE Get(t: T; READONLY k: Key.T; VAR (*OUT*) v: Value.T): BOOLEAN =
  VAR curr := t.root; BEGIN
    WHILE curr # t.nil DO
      CASE t.keyCompare(k, curr.k) OF
      | -1 => curr := curr.l
      |  1 => curr := curr.r
      |  0 => v := curr.v; RETURN TRUE
      END
    END;
    RETURN FALSE
  END Get;

PROCEDURE Put(t: T; READONLY k: Key.T; READONLY v: Value.T): BOOLEAN =
  BEGIN
    IF t.root = t.nil THEN
      (* empty tree *)
      t.root := NEW(Node, k := k, v := v, p := t.nil,
        l := t.nil, r := t.nil, color := Color.Black);
    ELSE
      VAR prev: Node; cmp: [-1..1]; curr := t.root; BEGIN
        (* insert new element or return if "k" already in table *)
        REPEAT
          prev := curr;
          cmp := t.keyCompare(k, curr.k);
          CASE cmp OF
          | -1 => curr := curr.l
          |  1 => curr := curr.r
          |  0 => curr.v := v; RETURN TRUE
          END
        UNTIL curr = t.nil;
        curr := NEW(Node, k := k, v := v, color := Color.Red,
          p := prev, l := t.nil, r := t.nil);
        IF cmp < 0
          THEN prev.l := curr
          ELSE prev.r := curr
        END;

        (* rebalance if necessary *)
        WHILE prev # t.root AND prev.color = Color.Red DO
          (* Note: The root of the tree is always colored black,
             so "prev.p" is guaranteed to be non-NIL. *)
          <* ASSERT prev.p # t.nil *>
          IF prev = prev.p.l THEN
            (* "prev" is a left child *)
            VAR uncle := prev.p.r; BEGIN
              IF uncle.color = Color.Red THEN
                prev.color := Color.Black;
                uncle.color := Color.Black;
                curr := prev.p;
                prev := curr.p;
                curr.color := Color.Red;
              ELSE
                IF curr = prev.r THEN
                  LeftRotate(t, prev, curr);
                  curr := prev; prev := curr.p;
                END;
                prev.color := Color.Black;
                prev.p.color := Color.Red;
                RightRotate(t, prev.p, prev);
              END
            END
          ELSE
            (* "prev" is a right child *)
            VAR uncle := prev.p.l; BEGIN
              IF uncle.color = Color.Red THEN
                prev.color := Color.Black;
                uncle.color := Color.Black;
                curr := prev.p;
                prev := curr.p;
                curr.color := Color.Red;
              ELSE
                IF curr = prev.l THEN
                  RightRotate(t, prev, curr);
                  curr := prev; prev := curr.p;
                END;
                prev.color := Color.Black;
                prev.p.color := Color.Red;
                LeftRotate(t, prev.p, prev);
              END
            END
          END
        END
      END;
      t.root.color := Color.Black
    END;
    INC(t.num);
    RETURN FALSE;
  END Put;

PROCEDURE Delete(t: T; READONLY k: Key.T; VAR (*OUT*) v: Value.T): BOOLEAN =
  VAR curr := t.root; rep, repCh: Node; BEGIN
    (* find the node to delete (if any *)
    WHILE curr # t.nil DO
      CASE t.keyCompare(k, curr.k) OF
      | -1 => curr := curr.l
      |  1 => curr := curr.r
      |  0 => EXIT
      END
    END;
    IF curr = t.nil THEN RETURN FALSE END;

    (* locate replacement node and one of its children *)
    IF curr.l = t.nil OR curr.r = t.nil
      THEN rep := curr
      ELSE rep := Successor(t, curr)
    END;
    IF rep.l # t.nil
      THEN repCh := rep.l
      ELSE repCh := rep.r
    END;

    (* splice out "rep" node *)
    repCh.p := rep.p;
    IF rep.p = t.nil THEN
      t.root := repCh
    ELSE
      IF rep = rep.p.l
        THEN repCh.p.l := repCh
        ELSE repCh.p.r := repCh
      END
    END;

    (* save value of node to be deleted *)
    v := curr.v;

    (* copy "rep" fields into "curr" if they are different *)
    IF rep # curr THEN
      curr.k := rep.k;
      curr.v := rep.v
    END;

    (* rebalance tree if necessary *)
    IF rep.color = Color.Black THEN
      DeleteFixup(t, repCh)
    END;
    DEC(t.num);
    RETURN TRUE
  END Delete;

PROCEDURE DeleteFixup(t: T; ch: Node) =
  VAR p, sib: Node; BEGIN
    WHILE ch # t.root AND ch.color = Color.Black DO
      p := ch.p;
      IF ch = p.l THEN
        (* "ch" is the left child of "p" *)
        sib := p.r;
        <* ASSERT sib # t.nil *>
        IF sib.color = Color.Red THEN
          (* case 1 *)
          sib.color := Color.Black;
          p.color := Color.Red;
          LeftRotate(t, p, sib);
          sib := p.r
        END;
        <* ASSERT sib.color = Color.Black AND sib # t.nil *>
        IF sib.l.color = Color.Black AND sib.r.color = Color.Black THEN
          (* case 2 *)
          sib.color := Color.Red;
          ch := p
        ELSE
          IF sib.r.color = Color.Black THEN
            (* case 3 *)
            <* ASSERT sib.l.color = Color.Red *>
            sib.l.color := Color.Black;
            sib.color := Color.Red;
            RightRotate(t, sib, sib.l);
            sib := p.r
          END;
          <* ASSERT sib.r.color = Color.Red *>
          (* case 4 *)
          sib.color := p.color;
          p.color := Color.Black;
          sib.r.color := Color.Black;
          LeftRotate(t, p, sib);
          ch := t.root
        END
      ELSE
        (* "ch" is the right child of "p" *)
        sib := p.l;
        <* ASSERT sib # t.nil *>
        IF sib.color = Color.Red THEN
          (* case 1 *)
          sib.color := Color.Black;
          p.color := Color.Red;
          RightRotate(t, p, sib);
          sib := p.l
        END;
        <* ASSERT sib.color = Color.Black AND sib # t.nil *>
        IF sib.r.color = Color.Black AND sib.l.color = Color.Black THEN
          (* case 2 *)
          sib.color := Color.Red;
          ch := p
        ELSE
          IF sib.l.color = Color.Black THEN
            (* case 3 *)
            <* ASSERT sib.r.color = Color.Red *>
            sib.r.color := Color.Black;
            sib.color := Color.Red;
            LeftRotate(t, sib, sib.r);
            sib := p.l
          END;
          <* ASSERT sib.l.color = Color.Red *>
          (* case 4 *)
          sib.color := p.color;
          p.color := Color.Black;
          sib.l.color := Color.Black;
          RightRotate(t, p, sib);
          ch := t.root
        END
      END
    END;
    ch.color := Color.Black
  END DeleteFixup;

PROCEDURE Iterate(t: T): Tbl.Iterator =
  BEGIN RETURN IterateOrdered(t, TRUE) END Iterate;

PROCEDURE IterateOrdered(t: T; up: BOOLEAN): SortedTbl.Iterator =
  VAR res: Iterator; BEGIN
    IF up
      THEN res := NEW(IteratorUp);
      ELSE res := NEW(IteratorDown);
    END;
    res.t := t;
    res.reset();
    RETURN res
  END IterateOrdered;
 
(* Iterator method implementations ---------------------------------------- *)

PROCEDURE ResetUp(it: Iterator) =
  VAR t := it.t; BEGIN
    IF t.root = t.nil
      THEN it.curr := t.nil
      ELSE it.curr := Min(t, t.root)
    END
  END ResetUp;

PROCEDURE ResetDown(it: Iterator) =
  VAR t := it.t; BEGIN
    IF t.root = t.nil
      THEN it.curr := t.nil
      ELSE it.curr := Max(t, t.root)
    END
  END ResetDown;

PROCEDURE NextUp(it: Iterator; VAR (*OUT*) k: Key.T; VAR (*OUT*) v: Value.T):
    BOOLEAN =
  VAR curr := it.curr; BEGIN
    (* handle empty iterator *)
    IF curr = it.t.nil THEN RETURN FALSE END;

    (* save key and value in current node *)
    k := curr.k; v := curr.v;

    (* advance "it.curr" to next node in order *)
    it.curr := Successor(it.t, curr);
    RETURN TRUE
  END NextUp;

PROCEDURE NextDown(it: Iterator; VAR (*OUT*) k: Key.T; VAR (*OUT*) v: Value.T):
    BOOLEAN =
  VAR curr := it.curr; BEGIN
    (* handle empty iterator *)
    IF curr = it.t.nil THEN RETURN FALSE END;

    (* save key and value in current node *)
    k := curr.k; v := curr.v;

    (* advance "it.curr" to next node in order *)
    it.curr := Predecessor(it.t, curr);
    RETURN TRUE
  END NextDown;

PROCEDURE SeekUp(it: Iterator; READONLY key: Key.T) =
  VAR t := it.t; curr := t.root; prev, anc := t.nil; cmp: [-1..1] := -1; BEGIN
    WHILE curr # t.nil DO
      prev := curr;
      cmp := t.keyCompare(key, curr.k);
      CASE cmp OF
      | -1 => anc := curr; curr := curr.l
      |  1 => curr := curr.r
      |  0 => it.curr := curr; RETURN
      END
    END;
    IF cmp < 0
      THEN it.curr := prev
      ELSE it.curr := anc
    END
  END SeekUp;

PROCEDURE SeekDown(it: Iterator; READONLY key: Key.T) =
  VAR t := it.t; curr := t.root; prev, anc := t.nil; cmp: [-1..1] := 1; BEGIN
    WHILE curr # t.nil DO
      prev := curr;
      cmp := t.keyCompare(key, curr.k);
      CASE cmp OF
      | -1 => curr := curr.l
      |  1 => anc := curr; curr := curr.r
      |  0 => it.curr := curr; RETURN
      END
    END;
    IF cmp > 0
      THEN it.curr := prev
      ELSE it.curr := anc
    END
  END SeekDown;

BEGIN
END RedBlackTbl.
