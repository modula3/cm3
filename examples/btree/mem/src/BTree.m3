UNSAFE MODULE BTree;

IMPORT IO;
IMPORT IntIntTbl;

VAR
  pageGlob : INTEGER := 0;

PROCEDURE Search(root : Page; x: INTEGER; VAR p: Page; VAR k: INTEGER) : BOOLEAN =
  VAR
    m, l, r: INTEGER;
    found: BOOLEAN;
    a: Page;
  BEGIN
    a := root; found := FALSE;
    WHILE a # NIL AND NOT found DO
      l := 0; r := a.m; (*binary search*)
      WHILE l < r DO
        m := (l+r) DIV 2;
        IF x <= a.e[m].key THEN r := m ELSE l := m+1 END
      END;
      IF r < a.m AND a.e[r].key = x THEN
        found := TRUE
      ELSIF r = 0 THEN a := a.p0 ELSE a := a.e[r-1].p
      END
    END ;
    p := a; k := r;
    RETURN found;
  END Search;

(* c is ancestor page a is current page b is sibling page*)
PROCEDURE Overflow (c,a : Page; VAR u : Entry; r,s : INTEGER): BOOLEAN  =
  VAR
    b,tmp : Page;
    right : BOOLEAN;

    PROCEDURE Balance() =
      VAR
        k,m,n : INTEGER;
      BEGIN
        INC(r);
        IF right THEN
          k := 2 + PageSize + b.m;
          m := (k DIV 2) + 1;
          k := k - m - b.m;
          tmp := c.e[s].p;
          c.e[s].p := b.p0;

          FOR i := b.m-1 TO 0 BY -1 DO b.e[i+k] := b.e[i]; END;
          b.e[k-1] := c.e[s];
          IF m = r THEN
            FOR i := r-1 TO PageSize-1 DO b.e[i-r+1] := a.e[i]; END;
            c.e[s] := u;
          ELSE
            IF m > r THEN
              FOR i := m-1 TO PageSize-1 DO b.e[i-m+1] := a.e[i]; END;
              c.e[s] := a.e[m-2];
              FOR i := m-3 TO r-1 BY -1 DO a.e[i+1] := a.e[i]; END;
              a.e[r-1] := u;
            ELSE (* m < r *)
              FOR i := r-1 TO PageSize-1 DO b.e[i-m+1] := a.e[i]; END;
              b.e[r-m-1] := u;
              FOR i := m TO r-2 DO b.e[i-m] := a.e[i]; END;
              c.e[s] := a.e[m-1];
            END;
          END;

          b.p0 := c.e[s].p;
          c.e[s].p := tmp;
          a.m := m - 1;
          INC(b.m, k);
        ELSE                   (* left *)
          n := b.m + 1;
          m := (2 + PageSize + n) DIV 2;
          k := m - n;

          tmp := c.e[s].p;
          c.e[s].p := a.p0;
          b.e[n-1] := c.e[s];
          IF k = r THEN
            FOR i := 0 TO k-2 DO b.e[i+n] := a.e[i]; END;
            FOR i := r-1 TO PageSize  - 1 DO a.e[i-r+1] := a.e[i]; END;
            c.e[s] := u;
          ELSE
            IF k < r THEN
              FOR i := 0 TO k-2 DO b.e[i+n] := a.e[i]; END;
              c.e[s] := a.e[k-1];
              FOR i := k TO r-2 DO a.e[i-k] := a.e[i]; END;
              a.e[r-k-1] := u;
              FOR i := r-1 TO PageSize-1 DO a.e[i-k+1] := a.e[i]; END;
            ELSE (* k > r*)
              FOR i := 0 TO r-2 DO b.e[i+n] := a.e[i]; END;
              b.e[r+n-1] := u;
              FOR i := r-1 TO k-3 DO b.e[i+n+1] := a.e[i]; END;
              c.e[s] := a.e[k-2];
              FOR i := k-1 TO PageSize-1 DO a.e[i-k+1] := a.e[i]; END;
            END;
          END;

          a.p0 := c.e[s].p;
          c.e[s].p := tmp;
          a.m := PageSize - (k - 1);
          INC(b.m, k);
        END;
    END Balance;

  BEGIN
    IF c = NIL THEN RETURN FALSE; END;

    right := s < c.m;
    IF right THEN
      b := c.e[s].p;
    ELSE
      DEC(s);
      IF s = 0 THEN b := c.p0 ELSE b := c.e[s-1].p END;
    END;

    IF b.m = PageSize THEN RETURN FALSE; END;
    Balance();
    RETURN TRUE;
  END Overflow;

PROCEDURE Insert(x: INTEGER; c,a: Page; VAR h: BOOLEAN; VAR v: Entry;
                 s : INTEGER) =
(*a # NIL. Search key x in B-tree with root a;
insert new item with key x. If an entry is to be passed up,
assign it to v. h := "tree has become higher"*)
  VAR
    m, L, R: INTEGER;
    b: Page;
    u: Entry;
  BEGIN (* ~h *)
    IF a = NIL THEN
      v.key := x;
      v.p := NIL;
      h := TRUE;
    ELSE
      L := 0; R := a.m;
      (*binary search*)
      WHILE L < R DO
        m := (L+R) DIV 2;
        IF x <= a.e[m].key THEN R := m ELSE L := m+1 END;
      END;
      IF R < a.m AND a.e[R].key = x THEN (*found, do nothing*)
        IO.Put("found on insert\n");
      ELSE (*item not on this page*)
        IF R = 0 THEN b := a.p0 ELSE b := a.e[R-1].p END;
        Insert(x, a, b, h, u, R);
        IF h THEN (*insert u to the left of a.e[R]*)
          IF a.m < PageSize THEN
            h := FALSE;
            FOR i := a.m TO R+1 BY -1 DO a.e[i] := a.e[i-1]; END;
            a.e[R] := u;
            INC(a.m)
          ELSE
            IF Overflow(c,a,u,R,s) THEN
              h := FALSE;
            ELSE
              b := NEW(Page); (*overflow; split a into a,b and assign the middle entry to v*)
              b.pageNum := pageGlob;
              INC(pageGlob);

              IF R < N THEN (*insert in left page a*)
                v := a.e[N-1];
                FOR i := N-1 TO R+1 BY -1 DO a.e[i] := a.e[i-1]; END;
                a.e[R] := u;
                FOR i := 0 TO N-1 DO b.e[i] := a.e[i+N]; END;
              ELSE (*insert in right page b*)
                DEC(R, N);
                IF R = 0 THEN
                  v := u
                ELSE
                  v := a.e[N];
                  FOR i := 0 TO R-2 DO b.e[i] := a.e[i+N+1] END;
                  b.e[R-1] := u;
                END;
                FOR i := R TO N-1 DO b.e[i] := a.e[i+N]; END;
              END;
              a.m := N;
              b.m := N;
              b.p0 := v.p;
              v.p := b;
            END;
          END;
        END;
      END;
    END;
  END Insert;

PROCEDURE InsertKey(VAR p : Page; k: INTEGER) =
VAR
  q : Page;
  u : Entry;
  h : BOOLEAN;
  BEGIN
    Insert(k, NIL, p, h, u, -1);
    IF h THEN
      q := p;
      p := NEW(Page);
      p.m := 1;
      p.p0 := q;
      p.e[0] := u;
      p.pageNum := pageGlob;
      INC(pageGlob);
    END;
  END InsertKey;

PROCEDURE Underflow(c, a: Page; s: INTEGER; VAR h: BOOLEAN) =
(*a = underflowing page, c = ancestor page,
s = index of deleted entry in c*)
  VAR
    b: Page;
    k: INTEGER;
  BEGIN (*h & (a.m = N-1) & (c.e[s-1].p = a) *)
    IF s < c.m THEN (*b := page to the right of a*)
      b := c.e[s].p;
      k := (b.m-N+1) DIV 2; (*k = nof items available on page b*)
      a.e[N-1] := c.e[s];
      a.e[N-1].p := b.p0;
      IF k > 0 THEN (*balance by moving k-1 items from b to a*)
        FOR i := 0 TO k-2 DO a.e[i+N] := b.e[i] END;
        c.e[s] := b.e[k-1];
        b.p0 := c.e[s].p;
        c.e[s].p := b;
        DEC(b.m, k);
        FOR i := 0 TO b.m-1 DO b.e[i] := b.e[i+k] END;
        a.m := N-1+k;
        h := FALSE;
      ELSE (*merge pages a and b, discard b*)
        <*ASSERT k = 0*>
        FOR i := 0 TO N-1 DO a.e[i+N] := b.e[i] END;
        DEC(c.m);
        FOR i := s TO c.m-1 DO c.e[i] := c.e[i+1] END;
        a.m := 2*N;
        h := c.m < N;
        b := NIL;
      END;
    ELSE (*b := page to the left of a*)
      <*ASSERT s = c.m *>
      DEC(s);
      IF s = 0 THEN b := c.p0 ELSE b := c.e[s-1].p END;
      k := (b.m-N+1) DIV 2; (*k = nof items available on page b*)
      IF k > 0 THEN
        FOR i := N-2 TO 0 BY -1 DO a.e[i+k] := a.e[i]; END;
        a.e[k-1] := c.e[s];
        a.e[k-1].p := a.p0;
        (*move k-1 items from b to a, one to c*)
        DEC(b.m, k);
        FOR i := k-2 TO 0 BY -1 DO a.e[i] := b.e[i+b.m+1] END;
        c.e[s] := b.e[b.m];
        a.p0 := c.e[s].p;
        c.e[s].p := a;
        a.m := N-1+k;
        h := FALSE
      ELSE (*merge pages a and b, discard a*)
        <*ASSERT k = 0*>
        c.e[s].p := a.p0;
        b.e[N] := c.e[s];
        FOR i := 0 TO N-2 DO b.e[i+N+1] := a.e[i] END;
        b.m := 2*N;
        DEC(c.m);
        h := c.m < N;
        a := NIL;
      END;
    END;
  END Underflow;

PROCEDURE Delete(x: INTEGER; a: Page; VAR h: BOOLEAN) =
(*search and delete key x in B-tree a; if a page underflow arises,
balance with adjacent page or merge; h := "page a is undersize"*)
  VAR
    m, L, R: INTEGER;
    q: Page;

  PROCEDURE Del(p: Page; VAR h: BOOLEAN) =
    VAR
      k: INTEGER;
      q: Page; (*global a, R*)
    BEGIN
      k := p.m - 1;
      q := p.e[k].p;
      IF q # NIL THEN
        Del(q, h);
        IF h THEN
          Underflow(p, q, p.m, h)
        END;
      ELSE
        p.e[k].p := a.e[R].p;
        a.e[R] := p.e[k];
        DEC(p.m);
        h := p.m < N
      END;
  END Del;

  BEGIN
    IF a # NIL THEN
      L := 0; R := a.m; (*binary search*)
      WHILE L < R DO
        m := (L+R) DIV 2;
        IF x <= a.e[m].key THEN R := m; ELSE L := m+1; END;
      END;
      IF R = 0 THEN q := a.p0; ELSE q := a.e[R-1].p; END;
      IF (R < a.m) AND (a.e[R].key = x) THEN (*found*)
        IF q = NIL THEN (*a is leaf page*)
          DEC(a.m);
          h := a.m < N;
          FOR i := R TO a.m-1 DO a.e[i] := a.e[i+1] END;
        ELSE
          Del(q, h);
          IF h THEN
            Underflow(a, q, R, h)
          END;
        END;
      ELSE
        Delete(x, q, h);
        IF h THEN
          Underflow(a, q, R, h)
        END;
      END;
    END;
  END Delete;

PROCEDURE DeleteKey(VAR p : Page; k: INTEGER) =
VAR
  q : Page;
  h : BOOLEAN;
  BEGIN
    Delete(k, p, h);
    IF h AND p^.m = 0 THEN
      q := p;
      p := q^.p0;
    END;
  END DeleteKey;

PROCEDURE ShowTree(p: Page; level: INTEGER) =
  BEGIN
    IF p # NIL THEN
      FOR i := 1 TO level DO IO.Put("  "); END;
      IO.Put("("); IO.PutInt(p.pageNum); IO.Put(")");
      FOR i := 0 TO p.m-1 DO
        IO.PutInt(p.e[i].key); IO.Put("  "); (* fmt to 4 wide *)
      END;
      IO.Put("\n");
      ShowTree(p.p0, level+1);
      FOR i := 0 TO p.m-1 DO
        ShowTree(p.e[i].p, level+1);
      END;
    END;
  END ShowTree;

PROCEDURE CheckSeq(a : Page; VAR max, min : INTEGER) =
  BEGIN
    max := -1;
    min := LAST(INTEGER);
    FOR i := 0 TO a.m-2 DO
      IF a.e[i].key >= a.e[i+1].key THEN
        IO.Put("Err seq - page "); IO.PutInt(a.pageNum);
        IO.Put(" key "); IO.PutInt(a.e[i].key); IO.Put("\n");
      END;
    END;
    FOR i := 0 TO a.m-1 DO
      IF max < a.e[i].key THEN max := a.e[i].key; END;
      IF min > a.e[i].key THEN min := a.e[i].key; END;
    END;
  END CheckSeq;

PROCEDURE Check(a,b : Page; k : INTEGER) =
  VAR amax,amin, bmax,bmin : INTEGER;
  BEGIN
    IF a # NIL AND b # NIL THEN

      CheckSeq(a, amax,amin);
      CheckSeq(b, bmax,bmin);

      IF amax >= bmin THEN
        IO.Put("Err max ");
        IO.PutInt(a.pageNum);
        IO.Put("\n");
      END;

      IF NOT (( amax < k) AND (k < bmin)) THEN
        IO.Put("Err order\n");
      END;
    END;
  END Check;

PROCEDURE DoValidTree (p: Page; u : IntIntTbl.T) =
  VAR pmax,pmin : INTEGER;
    valid : BOOLEAN;
  BEGIN
    IF p # NIL THEN

      CheckSeq(p,pmax,pmin);

      FOR i := 0 TO p.m-1 DO
        valid := u.put(p.e[i].key,i);
        IF valid THEN
          IO.Put("Err not unique\n");
        END;
      END;

      DoValidTree(p.p0,u);

      FOR i := 0 TO p.m - 1 DO
        IF i < p.m - 1 THEN
          Check(p.e[i].p, p.e[i+1].p, p.e[i+1].key);
        END;
        DoValidTree(p.e[i].p,u);
      END;
    END
  END DoValidTree;

PROCEDURE ValidTree (p: Page) =
  VAR u := NEW(IntIntTbl.Default).init();
  BEGIN
    DoValidTree(p,u);
  END ValidTree;

BEGIN
END BTree.
