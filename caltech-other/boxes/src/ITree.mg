GENERIC MODULE ITree(Elem);
IMPORT Debug, Fmt;

REVEAL 
  T = BRANDED Brand REF RECORD
    l, r, dn, up : T := NIL;
    min : INTEGER;
    this : Elem.T := NIL; (* stuff contained in interval to the RIGHT 
                                  (increasing numbers) *)
  END;

PROCEDURE FmtInterval(interval : Interval) : TEXT =
  BEGIN 
    RETURN "{ " & Fmt.Int(interval.lo) & ", " & Fmt.Int(interval.hi) & " }"
  END FmtInterval;

(* create a new "number line" *)
PROCEDURE New() : T =
  VAR
    max := NEW(T, min := LAST(INTEGER));
    min := NEW(T, min := FIRST(INTEGER));
  BEGIN
    max.dn := min;
    min.up := max;
    max.l := min;
    RETURN max
  END New;

(* guaranteed to terminate at one of the endpoints of 
   the interval containing for *)
PROCEDURE IntSearch(x : T; for : INTEGER; VAR lo, hi : T) =
  VAR
    res : T;
  BEGIN
    WHILE for # x.min DO
      res := x;
      IF for < x.min THEN 
        x := x.l 
      ELSE
        x := x.r 
      END;
      IF x = NIL THEN EXIT END;
      IF for = x.min THEN res := x END
    END;

    (* each interval goes from this.min to this.up.min - 1 *)
    IF for >= res.min THEN 
      lo := res; hi := res.up
    ELSE
      hi := res; lo := res.dn
    END;

    RETURN
  END IntSearch;

(* this is a no-op if at already exists in the tree *)
PROCEDURE Insert(x : T; at : INTEGER) : T =
  VAR
    lo, hi : T;
  BEGIN
    IntSearch(x,at,lo,hi);
    <* ASSERT at >= lo.min AND at < hi.min *>
    IF at # lo.min AND at # hi.min THEN
      VAR
        new := NEW(T, min := at, this := lo.this, dn := lo, up := hi);
      BEGIN
        IF lo.this # NIL THEN
          Debug.Error("Attempting to split non-NIL interval " &
            Fmt.Int(lo.min) & "; " & Fmt.Int(lo.up.min))
        END;
        lo.up := new;
        hi.dn := new;
        IF lo.r = NIL THEN 
          lo.r := new 
        ELSIF hi.l = NIL THEN
          hi.l := new
        ELSE
          <* ASSERT FALSE *>
        END;
        RETURN new
      END
    ELSE
      RETURN lo (* interval containing at *)
    END
  END Insert;

PROCEDURE MarkInterval(x : T; interval : Interval; with : Elem.T) =
  VAR
    rep := Insert(x, interval.lo);
  BEGIN
    EVAL Insert(x, interval.hi);

    (* check for errors *)
    IF rep.this # NIL THEN
      Debug.Error("Already have something: attempting to mark " & 
        FmtInterval(interval) & 
        " ran into non-NIL in interval " & 
        Fmt.Int(rep.min) & "; " & Fmt.Int(rep.up.min))
    END;
    rep.this := with
  END MarkInterval;

PROCEDURE Search(x : T; for : INTEGER) : Elem.T =
  VAR lo, hi : T; BEGIN IntSearch(x,for,lo,hi); RETURN lo.this END Search;

(* XXX untested code follows *)
PROCEDURE UnMarkInterval( x : T; interval : Interval) =
  VAR
    lo, hi : T;
  BEGIN
    (* find interval --- needs to be a single, unbroken interval ---
       and delete it, fixing up the tree links as necessary *)
    IntSearch(x, interval.lo, lo, hi);

    <* ASSERT lo.min = interval.lo *>
    <* ASSERT hi.min = interval.hi *>
    <* ASSERT lo.up = hi *>
    
    (* mark interval as NIL and try to merge with neighbors *)
    lo.this := NIL;

    IF lo.up.this = NIL THEN
      (* delete node lo.up *)
      DeleteNode(x, lo.up)
    END;

    IF lo.dn.this = NIL THEN
      (* delete node lo *)
      DeleteNode(x, lo)
    END;
  END UnMarkInterval;

PROCEDURE DeleteNode(tree, x : T) =
  VAR 
    p := FindParent(tree,x);
  BEGIN
    (* first tree links *)
    IF x.r = NIL THEN
      IF p.r = x THEN p.r := x.l ELSE <* ASSERT p.l = x *> p.l := x.l END;

      (* sideways links *)
      x.up.dn := x.dn;
      x.dn.up := x.up;

      <* ASSERT x.up.min # x.up.dn.min *>
      (* OLD(x) is free *)
    ELSIF x.l = NIL THEN
      IF p.r = x THEN p.r := x.r ELSE <* ASSERT p.l = x *> p.l := x.r END;

      (* sideways links *)
      x.up.dn := x.dn;
      x.dn.up := x.up;

      <* ASSERT x.up.min # x.up.dn.min *>
      (* OLD(x) is free *)
    ELSE
      (* both non-NIL, successor must have NIL left child *)
      (* copy successor data to x and splice out successor *)
      p := FindParent(tree,x.up);
      <* ASSERT x.up.l = NIL *>
      x.this := x.up.this;
      x.min := x.up.min;
      IF p.r = x.up THEN 
        p.r := x.up.r 
      ELSE 
        <* ASSERT p.l = x.up *> 
        p.l := x.up.r 
      END;

      (* sideways links *)
      x.up.up.dn := x;
      x.up := x.up.up;

      <* ASSERT x.up.min # x.min *>

      (* OLD(x.up) is free *)
    END;
  END DeleteNode;

PROCEDURE FindParent(tree : T; n : T) : T =
  VAR 
    for := n.min;
    x := tree;
    res : T;
  BEGIN
    WHILE for # x.min DO
      res := x;
      IF for < x.min THEN
        x := x.l
      ELSIF for > x.min THEN
        x := x.r
      ELSE
        <* ASSERT FALSE *>
      END;
      IF for = x.min THEN 
        <* ASSERT (res.l = NIL OR res.r = NIL) OR res.l.min # res.r.min *>
        <* ASSERT res.l = n OR res.r = n *>
        RETURN res 
      END
    END;
    <* ASSERT FALSE *>
  END FindParent;

<*NOWARN*>PROCEDURE CheckInvariants(tree : T) = 

  PROCEDURE Recurse(x : T) =
    BEGIN
      IF x = NIL THEN RETURN END;
      <* ASSERT x.r = NIL OR x.r.min > x.min *>
      <* ASSERT x.l = NIL OR x.l.min < x.min *>
      Recurse(x.r);
      Recurse(x.l)
    END Recurse;

  BEGIN
    Recurse(tree);
    VAR
      x := Insert(tree, FIRST(INTEGER));
    BEGIN
      WHILE x.up # NIL DO
        <* ASSERT x.min # x.up.min *>
        x := x.up
      END
    END
  END CheckInvariants;

PROCEDURE CheckExists(tree, x : T) : BOOLEAN = 
  BEGIN
    IF tree = NIL THEN
      RETURN FALSE
    ELSIF x = tree THEN 
      RETURN TRUE 
    ELSE 
      RETURN CheckExists(tree.r,x) OR CheckExists(tree.l,x) 
    END
  END CheckExists;

BEGIN END ITree.
