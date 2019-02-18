(* $Id$ *)

GENERIC MODULE Tree(Elem);

REVEAL
  Default = PublicDefault BRANDED Brand OBJECT
    root : Node := NIL;
  OVERRIDES
    init := InitDummy;
    insert := Insert;
    iterateOrdered := IterateOrdered;
    iterate := IterateOrdered;  (* for now *)
    lowerBound := LowerBound;
    successor := Successor;
    delete := Delete;
    copy := Copy;
  END;

TYPE 
  Node = OBJECT
    this : Elem.T;
    l, r, u : Node := NIL;
  END;

PROCEDURE InitDummy(t : Default) : T = 
  BEGIN RETURN t END InitDummy;

PROCEDURE Insert(t : Default; e : Elem.T) =
  BEGIN
    IF t.root = NIL THEN
      t.root := NEW(Node, this := e)
    ELSE
      InsertNode(t.root, NEW(Node, this := e))
    END
  END Insert;

PROCEDURE InsertNode(into : Node; what : Node) =
  VAR
    p := into;
    q : Node;
  BEGIN
    <* ASSERT p # NIL *>
    LOOP
      q := p;
      IF Elem.Compare(what.this, p.this) <= 0 THEN
        p := p.l;
        IF p = NIL THEN
          q.l := what;
          what.u := q;
          RETURN
        END
      ELSE
        p := p.r;
        IF p = NIL THEN
          q.r := what;
          what.u := q;
          RETURN
        END
      END
    END
  END InsertNode;

PROCEDURE Delete(t : Default; e : Elem.T) : BOOLEAN =
  VAR
    p := NodeLowerBound(t.root, e);
    newp : Node;
  BEGIN
    WHILE p # NIL AND Elem.Compare(p.this, e) = 0 AND NOT Elem.Equal(p.this, e) DO
      p := Next(p)
    END;

    IF p # NIL AND Elem.Equal(p.this, e) THEN

      (* first make sure we have one nil child *)

      IF p.l # NIL AND p.r # NIL THEN
        p.l.u := NIL;
        InsertNode(p.r, what := p.l);
        p.l := NIL
      END;

      <* ASSERT p.l = NIL OR p.r = NIL *>

      (* pick non-NIL child *)
      IF p.l = NIL THEN 
        newp := p.r
      ELSE
        newp := p.l
      END;

      (* make parent of non-NIL child the old parent of p *)
      IF newp # NIL THEN
        newp.u := p.u
      END;

      (* make child of parent of p the new child *)
      IF p.u = NIL THEN 
        <* ASSERT p = t.root *>
        t.root := newp;
      ELSE
        IF p.u.r = p THEN
          p.u.r := newp
        ELSIF p.u.l = p THEN
          p.u.l := newp
        ELSE
          <* ASSERT FALSE *>
        END
      END;
      RETURN TRUE
    END;
    (* not found *)
    RETURN FALSE
  END Delete;

PROCEDURE IterateOrdered(t : Default) : Iterator =
  BEGIN
    RETURN NEW(OrderedIterator, p := First(t.root))
  END IterateOrdered;

(* HMMM *)
PROCEDURE Next(p : Node) : Node =
  BEGIN
    (* make assertions on tree structure *)
    <* ASSERT p # NIL *>
    <* ASSERT p.u # p *>
    <* ASSERT p.u = NIL OR p.u.r = p OR p.u.l = p *>
    <* ASSERT p.r = NIL OR p.r.u = p *>
    <* ASSERT p.l = NIL OR p.l.u = p *>
    <* ASSERT p.l = NIL OR Elem.Compare(p.l.this, p.this) <= 0 *>
    <* ASSERT p.r = NIL OR Elem.Compare(p.this, p.r.this) <= 0 *>
    <* ASSERT p.l = NIL OR p.r = NIL OR Elem.Compare(p.l.this, p.r.this)<= 0 *>

    (* painful and probably incorrect *)
    IF p.r # NIL THEN
      RETURN First(p.r)
    END;

    LOOP
      IF p.u = NIL THEN 
        RETURN NIL 
      END;
      IF p.u.r = p THEN
        p := p.u;
      ELSIF p.u.l = p THEN 
        RETURN p.u
      END
    END
  END Next;

PROCEDURE First(p : Node) : Node =
  VAR
    q : Node;
  BEGIN
    WHILE p # NIL DO
      q := p;
      p := p.l
    END;
    RETURN q
  END First;

REVEAL
  Iterator = PublicIterator BRANDED Brand & " Iterator" OBJECT END;

TYPE
  OrderedIterator = Iterator OBJECT
    p : Node;
  OVERRIDES
    next := INext;
  END;


PROCEDURE INext(i : OrderedIterator; VAR next : Elem.T) : BOOLEAN =
  BEGIN
    IF i.p # NIL THEN next := i.p.this; i.p := Next(i.p); RETURN TRUE END;
    RETURN FALSE
  END INext;

PROCEDURE LowerBound(t : Default; e : Elem.T; VAR lb : Elem.T) : BOOLEAN =
  VAR
    n := NodeLowerBound(t.root, e);
  BEGIN
    IF n = NIL THEN
      RETURN FALSE
    ELSE
      lb := n.this;
      RETURN TRUE
    END
  END LowerBound;

PROCEDURE NodeLowerBound(n : Node; e : Elem.T) : Node =
  VAR
    res : Node;
  BEGIN
    (* argh this code sucks *)

    IF n = NIL THEN
      res := NIL
    ELSIF Elem.Compare(n.this, e) < 0 THEN
      res := NodeLowerBound(n.r, e);
      IF res = NIL THEN
        res := n
      END;
    ELSIF Elem.Compare(n.this, e) > 0 OR Elem.Compare(n.this, e) = 0 AND n.l # NIL AND Elem.Compare(n.l.this, e) = 0 THEN
      res := NodeLowerBound(n.l, e)
    ELSE
      res := n 
    END;

    (* lower bound must be .le. e *)
    <* ASSERT res = NIL OR Elem.Compare(res.this, e) <= 0 *>

    (* lower bound must exist if first element is .le. e *)
    IF res = NIL THEN
      <* ASSERT n = NIL OR Elem.Compare(First(n).this, e) > 0 *>
    END;
    RETURN res
  END NodeLowerBound;

PROCEDURE Successor(t : Default; e : Elem.T; VAR successor : Elem.T) : BOOLEAN =
  VAR
    p := NodeLowerBound(t.root, e);
  BEGIN
    WHILE p # NIL AND Elem.Compare(e, p.this) = 0 DO
      p := Next(p)
    END;
    IF p = NIL THEN RETURN FALSE ELSE successor := p.this; RETURN TRUE END
  END Successor;

PROCEDURE Copy(t : Default) : T =
  VAR 
    res := NEW(Default).init();
    iter := t.iterate();
    e : Elem.T;
  BEGIN
    WHILE iter.next(e) DO res.insert(e) END;
    RETURN res
  END Copy;

BEGIN END Tree.
