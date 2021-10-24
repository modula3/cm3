MODULE BTree;

(* Construct and manage a B-tree of page size 2N.*)

IMPORT IsamIO, IntRefTbl, SortedIntIntTbl, IntPQ, OSError;
IMPORT Text, Fmt, IO;

CONST
  DiskBlockSize = 4096;
  MaxCache      = 100;
  idxExt        = ".idx";
  datExt        = ".dat";

  N  = (DiskBlockSize DIV BYTESIZE(Item) - 1) DIV 2;
  (* Try smaller values of N to test paging overflow and underflow
     functions say N = 4; *)
 
  PageSize = 2 * N;
  InfoSize = BYTESIZE(Info);
  PageLen  = BYTESIZE(PageRec);

TYPE
  PageRef = INTEGER;

  Item = RECORD
           key: KeyType;
           p  : PageRef;
           dp : DataRef;
         END;

  Page = REF PageRec;
  PageRec = RECORD
              m : [0 .. PageSize];                    (* # of items *)
              p0: PageRef;
              e : ARRAY [0 .. PageSize - 1] OF Item;
            END;

  InfoRef = REF Info;
  Info = RECORD
           pageCount    : INTEGER;
           blockLen     : CARDINAL;
           numKeys      : KeyRange;
           firstFreeRec : INTEGER;
           firstFreePage: INTEGER;
           numFreeRecs  : INTEGER;
           numFreePages : INTEGER;
           rootRef      : ARRAY KeyRange OF PageRef;
         END;

  KeyMapRef = REF KeyMap;
  KeyMap = RECORD
             p  : Page;
             pri: IntPQ.Elt;
             dirty: BOOLEAN;     (* use this if turn cache into
                                    write-back *)
           END;

  Elt = IntPQ.Elt BRANDED OBJECT pRef: PageRef;  END;

  Cache = OBJECT
            map     : IntRefTbl.T;
            pq      : IntPQ.T;
            elt     : Elt;
            next    : INTEGER       := 0;
            useCache: BOOLEAN;
          METHODS
            init (use: BOOLEAN): Cache                       := InitCache;
            in   (pRef: PageRef; VAR km: KeyMapRef): BOOLEAN := InCache;
            add  (pRef: PageRef; a: Page)                    := AddCache;
            del  (pRef: PageRef)                             := DelCache;
            get  (km: KeyMapRef): Page                       := GetCache;
            used (): BOOLEAN                                 := CacheUsed;
          END;

REVEAL
  T =
    Public BRANDED "BTree.T" OBJECT
      idx          : IsamIO.FileRef;
      dat          : IsamIO.FileRef;
      info         : InfoRef;
      cache        : Cache;
      (* to avoid allocations in addrec/delrec we keep a ref here *)
      firstFreePage: REF INTEGER;
      firstFreeRec : REF INTEGER;
      infoDirty    : BOOLEAN;
      opened       : BOOLEAN          := FALSE;
      numNodes     : INTEGER;
      free         : SortedIntIntTbl.T;
    METHODS
      flushInfo  ()                        := FlushInfo;
      flushPages ()                        := FlushPages;
      deleteFile ()                        := DeleteFile;
      newPage    (VAR bRef: PageRef): Page := NewPage;
      getPage    (pRef: PageRef): Page     := GetPage;
      savePage   (pRef: PageRef; a: Page)  := SavePage;
      delPage    (pRef: PageRef)           := DelPage;
      find (root: PageRef; READONLY x: KeyType; VAR p: Page; VAR k: INTEGER):
            BOOLEAN := Find;
      insert (READONLY x         : KeyType;
                       cRef, aRef: PageRef;
                       dRef      : DataRef;
                       s         : INTEGER;
              VAR      h         : BOOLEAN;
              VAR      v         : Item     ) : BOOLEAN := Insert;
      underflow (cRef, aRef: PageRef; s: INTEGER; VAR h: BOOLEAN) := Underflow;
      overflow (cRef, aRef: PageRef; READONLY u: Item; r, s: INTEGER):
                BOOLEAN := Overflow;
      delete (READONLY x: KeyType; pRef: PageRef; VAR h: BOOLEAN) : BOOLEAN := Delete;
      validateKey (keyNum: KeyRange) RAISES {Error} := ValidateKey;
      printTree   (pRef: PageRef; l: INTEGER)       := PrintTree;
      dumpInfo() := DumpInfo;
      freeRecs() := FreeRecs;
    OVERRIDES
      create    := Create;
      open      := Open;
      close     := Close;
      findKey   := FindKey;
      insertKey := InsertKey;
      deleteKey := DeleteKey;
      addRec    := AddRec;
      delRec    := DelRec;
      putRec    := PutRec;
      getRec    := GetRec;
      iterate   := Iterate;
      print     := Print;
      printInfo := PrintInfo;
    END;

TYPE
  DefaultIterator = Iterator BRANDED OBJECT
    tree : T;
    freeIter : SortedIntIntTbl.Iterator;
    ofs : INTEGER := 0;
    free : INTEGER := -1;
  OVERRIDES
    next := Next;
  END;

VAR
  debug  := FALSE;

<* FATAL OSError.E, IntPQ.Empty, IntPQ.NotInQueue *>

PROCEDURE PK (lab: TEXT; READONLY k: KeyType) =
  VAR t: TEXT;
  BEGIN
    IF NOT debug THEN RETURN; END;
    IO.Put(lab);
    t := Text.FromChars(k);
    IF Text.Length(t) >= 0 THEN IO.Put(t); END;
    IO.Put("\n");
  END PK;

PROCEDURE D (lab: TEXT; n: INTEGER) =
  BEGIN
    IF NOT debug THEN RETURN; END;
    IO.Put(lab);
    IF n >= 0 THEN IO.PutInt(n); END;
    IO.Put("\n");
  END D;

PROCEDURE InitCache (self: Cache; use: BOOLEAN): Cache =
  BEGIN
    self.map := NEW(IntRefTbl.Default).init(MaxCache);
    self.pq := NEW(IntPQ.Default).init(sizeHint := MaxCache);
    self.useCache := use;
    RETURN self;
  END InitCache;

PROCEDURE InCache (self: Cache; pRef: PageRef; VAR km: KeyMapRef):
  BOOLEAN =
  VAR
    b  : REFANY;
    res: BOOLEAN;
  BEGIN
    IF NOT self.useCache THEN RETURN FALSE; END;
    res := self.map.get(pRef, b);
    km := NARROW(b, KeyMapRef);
    RETURN res;
  END InCache;

(* Called if pRef not in cache and then adds it.  Checks size of pq and
   deletes lru item.  TODO: check for wraparound on priority and reset
   cache*)
PROCEDURE AddCache (self: Cache; pRef: PageRef; a: Page) =
  VAR
    b : REFANY;
    km: KeyMapRef;
  BEGIN
    (* not in cache *)
    IF NOT self.useCache THEN RETURN END;
    IF self.pq.size() = MaxCache THEN
      self.elt := self.pq.deleteMin();
      EVAL self.map.delete(self.elt.pRef, b);
    END;
    self.elt := NEW(Elt, priority := self.next, pRef := pRef);
    self.pq.insert(self.elt);
    INC(self.next);

    km := NEW(KeyMapRef);
    km.p := a;
    km.dirty := TRUE;
    km.pri := self.elt;
    EVAL self.map.put(pRef, km);
  END AddCache;

PROCEDURE DelCache (self: Cache; pRef: PageRef) =
  VAR
    b : REFANY;
    km: KeyMapRef;
  BEGIN
    IF NOT self.useCache THEN RETURN END;
    IF self.in(pRef, km) THEN
      self.pq.delete(km.pri);
      EVAL self.map.delete(pRef, b);
    END
  END DelCache;

(* Called if pref is in cache, updates priority and returns page *)
PROCEDURE GetCache (self: Cache; km: KeyMapRef): Page =
  BEGIN
    IF NOT self.useCache THEN RETURN NIL; END;
    (* update cache by resetting priority *)
    self.pq.change(km.pri, self.next);
    INC(self.next);
    RETURN km.p;
  END GetCache;

PROCEDURE CacheUsed (self: Cache): BOOLEAN =
  BEGIN
    RETURN self.useCache;
  END CacheUsed;

PROCEDURE AddRec (self: T; a: REFANY): INTEGER =
  VAR dRef: INTEGER;
  BEGIN
    IF self.info.firstFreeRec # -1 THEN
      dRef := self.info.firstFreeRec;
      TRY
        IsamIO.GetBlock(self.dat, dRef, BYTESIZE(INTEGER), self.firstFreeRec);
      EXCEPT
      | IsamIO.Eof => <*ASSERT FALSE *>
      END;
      self.info.firstFreeRec := self.firstFreeRec^;
      DEC(self.info.numFreeRecs);
      self.infoDirty := TRUE;
    ELSE
      dRef := IsamIO.SeekEOF(self.dat);
    END;
    IsamIO.PutBlock(self.dat, dRef, self.info.blockLen, a);
    RETURN dRef;
  END AddRec;

PROCEDURE DelRec (self: T; dRef: DataRef) =
  BEGIN
    (* if key stored without data rec nothing to delete - fix this
      since dRef = 0 is first datarec need another way to detect
      key stored and no data 
    IF dRef = 0 THEN RETURN; END;
    *)
    self.firstFreeRec^ := self.info.firstFreeRec;
    IsamIO.PutBlock(self.dat, dRef, BYTESIZE(INTEGER), self.firstFreeRec);
    self.info.firstFreeRec := dRef;
    INC(self.info.numFreeRecs);
    self.infoDirty := TRUE;
  END DelRec;

PROCEDURE PutRec (self: T; dRef: DataRef; a: REFANY) =
  BEGIN
    IsamIO.PutBlock(self.dat, dRef, self.info.blockLen, a);
  END PutRec;

PROCEDURE GetRec (self: T; dRef: DataRef; a: REFANY) RAISES {EOF} =
  BEGIN
    TRY
      IsamIO.GetBlock(self.dat, dRef, self.info.blockLen, a);
    EXCEPT
    | IsamIO.Eof => RAISE EOF;
    END;
  END GetRec;

PROCEDURE NewPage (self: T; VAR pRef: PageRef): Page =
  VAR a: Page;
  BEGIN
    IF self.info.firstFreePage # -1 THEN
      pRef := self.info.firstFreePage;
      TRY
        IsamIO.GetBlock(self.idx, (pRef - 1) * PageLen + InfoSize,
                        BYTESIZE(INTEGER), self.firstFreePage);
      EXCEPT
      | IsamIO.Eof => <*ASSERT FALSE, "Eof in free page list" *>
      END;
      self.info.firstFreePage := self.firstFreePage^;
      DEC(self.info.numFreePages);
    ELSE
      INC(self.info.pageCount);
      pRef := self.info.pageCount;
    END;

    D("NewPage ", pRef);
    a := NEW(Page);
    self.infoDirty := TRUE;
    self.flushInfo();
    RETURN a;
  END NewPage;

PROCEDURE GetPage (self: T; pRef: PageRef): Page =
  VAR
    km: KeyMapRef;
    a : Page;
  BEGIN
    D("GetPage ",pRef);
    IF pRef = 0 THEN RETURN NIL; END;
    IF self.cache.in(pRef, km) THEN
      a := self.cache.get(km);
    ELSE
      a := NEW(Page);
      TRY
        IsamIO.GetBlock(self.idx, (pRef - 1) * PageLen + InfoSize, PageLen, a);
      EXCEPT
      | IsamIO.Eof => <*ASSERT FALSE, "Eof in getpage" *>
      END;
      self.cache.add(pRef, a);
    END;
    RETURN a;
  END GetPage;

PROCEDURE SavePage (self: T; pRef: PageRef; a: Page) =
  BEGIN
    D("SavePage ", pRef);
    (* force re-read of this page *)
    self.cache.del(pRef);
    IsamIO.PutBlock(self.idx, (pRef - 1) * PageLen + InfoSize, PageLen, a);
  END SavePage;

PROCEDURE DelPage (self: T; pRef: PageRef) =
  BEGIN
    D("DelPage ", pRef);
    self.cache.del(pRef);
    self.firstFreePage^ := self.info.firstFreePage;
    IsamIO.PutBlock(self.idx, (pRef - 1) * PageLen + InfoSize,
                    BYTESIZE(INTEGER), self.firstFreePage);
    self.info.firstFreePage := pRef;
    INC(self.info.numFreePages);
    self.infoDirty := TRUE;
    self.flushInfo();
  END DelPage;

PROCEDURE FlushPages (self: T) =
  VAR
    iter : IntRefTbl.Iterator;
    pRef : PageRef;
    val  : REFANY;
    km   : KeyMapRef;
    count: INTEGER;
  BEGIN
    IF self.cache.useCache THEN
      iter := self.cache.map.iterate();
      count := 0;
      WHILE iter.next(pRef, val) DO
        km := NARROW(val, KeyMapRef);
        IF km.dirty THEN
          IsamIO.PutBlock(
            self.idx, (pRef - 1) * PageLen + InfoSize, PageLen, km.p);
          km.dirty := FALSE;
          INC(count);
        END;
      END;
    END;
  END FlushPages;

PROCEDURE FlushInfo (self: T) =
  BEGIN
    IF self.infoDirty THEN
      IsamIO.PutBlock(self.idx, 0, BYTESIZE(Info), self.info);
      (* serious slow down if flush here IsamIO.Flush(self.idx); *)
      self.infoDirty := FALSE;
    END;
  END FlushInfo;

PROCEDURE ValidateKey (self: T; keyNum: KeyRange) RAISES {Error} =
  BEGIN
    IF NOT self.opened THEN
      RAISE Error("Error File Block not open\n");
    END;
    IF keyNum < 1 OR keyNum > self.info.numKeys THEN
      RAISE Error("Error Invalid keynum\n");
    END;
  END ValidateKey;

PROCEDURE Comp (a, b: KeyType): BOOLEAN =
  VAR ret: [-1 .. 1];
  BEGIN
    (* ret := a <= b; for keytype integer *)
    ret := Text.Compare(Text.FromChars(a), Text.FromChars(b));
    RETURN ret <= 0;
  END Comp;

(* Search for a key *)
PROCEDURE Find (         self: T;
                         root: PageRef;
                READONLY x   : KeyType;
                VAR      p   : Page;
                VAR      k   : INTEGER  ): BOOLEAN =
  VAR
    m, l, r: INTEGER;
    found  : BOOLEAN;
    q      : PageRef;
    a      : Page;
  BEGIN
    a := self.getPage(root);
    found := FALSE;
    WHILE a # NIL AND NOT found DO
      l := 0;
      r := a.m;                  (*binary search*)
      WHILE l < r DO
        m := l + (r - l) DIV 2;
        IF Comp(x, a.e[m].key) THEN r := m ELSE l := m + 1 END
      END;
      IF r < a.m AND a.e[r].key = x THEN
        found := TRUE
      ELSE
        IF r = 0 THEN q := a.p0 ELSE q := a.e[r - 1].p END;
        a := self.getPage(q);
      END;
    END;
    p := a;
    k := r;
    RETURN found;
  END Find;

(* Find a key and return data ref if found *)
PROCEDURE FindKey
  (self: T; READONLY k: KeyType; keyNum: KeyRange; VAR dr: DataRef):
  BOOLEAN RAISES {Error} =
  VAR
    root : PageRef;
    p    : Page;
    index: INTEGER;
  BEGIN
    PK("Find Key ", k);
    self.validateKey(keyNum);
    root := self.info.rootRef[keyNum];
    IF root = -1 THEN RETURN FALSE END; (* empty tree *)
    IF Find(self, root, k, p, index) THEN
      dr := p.e[index].dp;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END FindKey;

PROCEDURE Overflow
  (self: T; cRef, aRef: PageRef; READONLY u: Item; r, s: INTEGER):
  BOOLEAN =
  (* a = overflowing page, c = ancestor page, s = index of a in c, r =
     index of u in a *)
  VAR
    a, b, c  : Page;
    bRef, tmp: PageRef;
    right    : BOOLEAN;

  PROCEDURE Balance () =
    VAR k, m, n: INTEGER;
    BEGIN
      INC(r);
      IF right THEN
        k := 2 + PageSize + b.m;
        m := (k DIV 2) + 1;
        k := k - m - b.m;
        tmp := c.e[s].p;
        c.e[s].p := b.p0;

        FOR i := b.m - 1 TO 0 BY -1 DO b.e[i + k] := b.e[i]; END;
        b.e[k - 1] := c.e[s];
        IF m = r THEN
          FOR i := r - 1 TO PageSize - 1 DO b.e[i - r + 1] := a.e[i]; END;
          c.e[s] := u;
        ELSE
          IF m > r THEN
            FOR i := m - 1 TO PageSize - 1 DO
              b.e[i - m + 1] := a.e[i];
            END;
            c.e[s] := a.e[m - 2];
            FOR i := m - 3 TO r - 1 BY -1 DO a.e[i + 1] := a.e[i]; END;
            a.e[r - 1] := u;
          ELSE                   (* m < r *)
            FOR i := r - 1 TO PageSize - 1 DO
              b.e[i - m + 1] := a.e[i];
            END;
            b.e[r - m - 1] := u;
            FOR i := m TO r - 2 DO b.e[i - m] := a.e[i]; END;
            c.e[s] := a.e[m - 1];
          END;
        END;

        b.p0 := c.e[s].p;
        c.e[s].p := tmp;
        a.m := m - 1;
        INC(b.m, k);
      ELSE                       (* left *)
        n := b.m + 1;
        m := (2 + PageSize + n) DIV 2;
        k := m - n;

        tmp := c.e[s].p;
        c.e[s].p := a.p0;
        b.e[n - 1] := c.e[s];
        IF k = r THEN
          FOR i := 0 TO k - 2 DO b.e[i + n] := a.e[i]; END;
          FOR i := r - 1 TO PageSize - 1 DO a.e[i - r + 1] := a.e[i]; END;
          c.e[s] := u;
        ELSE
          IF k < r THEN
            FOR i := 0 TO k - 2 DO b.e[i + n] := a.e[i]; END;
            c.e[s] := a.e[k - 1];
            FOR i := k TO r - 2 DO a.e[i - k] := a.e[i]; END;
            a.e[r - k - 1] := u;
            FOR i := r - 1 TO PageSize - 1 DO
              a.e[i - k + 1] := a.e[i];
            END;
          ELSE                   (* k > r*)
            FOR i := 0 TO r - 2 DO b.e[i + n] := a.e[i]; END;
            b.e[r + n - 1] := u;
            FOR i := r - 1 TO k - 3 DO b.e[i + n + 1] := a.e[i]; END;
            c.e[s] := a.e[k - 2];
            FOR i := k - 1 TO PageSize - 1 DO
              a.e[i - k + 1] := a.e[i];
            END;
          END;
        END;
        a.p0 := c.e[s].p;
        c.e[s].p := tmp;
        a.m := PageSize - (k - 1);
        INC(b.m, k);
      END;
    END Balance;

  BEGIN
    IF s = -1 THEN RETURN FALSE; END;
    c := self.getPage(cRef);
    a := self.getPage(aRef);

    right := s < c.m;
    IF right THEN
      bRef := c.e[s].p;
    ELSE
      DEC(s);
      IF s = 0 THEN bRef := c.p0 ELSE bRef := c.e[s - 1].p END;
    END;

    b := self.getPage(bRef);
    IF b.m = PageSize THEN RETURN FALSE; END;

    Balance();

    self.savePage(bRef, b);
    self.savePage(cRef, c);
    RETURN TRUE;
  END Overflow;

PROCEDURE Insert (         self      : T;
                  READONLY x         : KeyType;
                           cRef, aRef: PageRef;
                           dRef      : DataRef;
                           s         : INTEGER;
                  VAR      h         : BOOLEAN;
                  VAR      v         : Item     ) : BOOLEAN =

  (* a # NIL.  Search key x in B-tree with root a; insert new item with key
     x.  If an entry is to be passed up, assign it to v.  h := "tree has
     become higher"*)
  VAR
    m, l, r: INTEGER;
    a, b   : Page;
    bRef   : PageRef;
    u      : Item;
    res    : BOOLEAN := TRUE;
  BEGIN                          (* NOT h *)
    IF aRef = 0 THEN
      h := TRUE;
      (* item with key x is not in tree *)
      v.key := x;
      v.p := 0;
      v.dp := dRef;
    ELSE
      a := self.getPage(aRef);
      l := 0;
      r := a.m;
      (*binary search*)
      WHILE l < r DO
        m := l + (r - l) DIV 2;
        IF Comp(x, a.e[m].key) THEN r := m ELSE l := m + 1 END
      END;
      IF r < a.m AND a.e[r].key = x THEN (*found, do nothing*)
        (* maybe raise exception on found *)
        PK("\nSearch - key found on insert ", x);
        RETURN FALSE;
      ELSE                       (*item not on this page*)
        IF r = 0 THEN bRef := a.p0 ELSE bRef := a.e[r - 1].p END;
        res := self.insert(x, aRef, bRef, dRef, r, h, u);
        IF NOT res THEN RETURN res; END;
        IF h THEN                (*insert u to the left of a.e[r]*)
          IF a.m < PageSize THEN
            h := FALSE;
            FOR i := a.m TO r + 1 BY -1 DO a.e[i] := a.e[i - 1]; END;
            a.e[r] := u;
            INC(a.m)
          ELSE
            (* check if space in a parent or sibling page and balance if so *)
            IF self.overflow(cRef, aRef, u, r, s) THEN
              PK("Overflow\n", x);
              h := FALSE;
            ELSE
              b := self.newPage(bRef);
              (*overflow; split a into a,b and assign the middle entry to v*)
              IF r < N THEN      (*insert in left page a*)
                v := a.e[N - 1];
                FOR i := N - 1 TO r + 1 BY -1 DO a.e[i] := a.e[i - 1]; END;
                a.e[r] := u;
                FOR i := 0 TO N - 1 DO b.e[i] := a.e[i + N]; END;
              ELSE               (*insert in right page b*)
                DEC(r, N);
                IF r = 0 THEN
                  v := u
                ELSE
                  v := a.e[N];
                  FOR i := 0 TO r - 2 DO b.e[i] := a.e[i + N + 1] END;
                  b.e[r - 1] := u;
                END;
                FOR i := r TO N - 1 DO b.e[i] := a.e[i + N]; END;
              END;
              a.m := N;
              b.m := N;
              b.p0 := v.p;
              v.p := bRef;
              self.savePage(bRef, b);
            END;                 (* not overflow *)
          END;
          self.savePage(aRef, a);
        END;
      END;
    END;
    RETURN res;
  END Insert;

PROCEDURE InsertKey
  (self: T; READONLY key: KeyType; dRef: DataRef; keyNum: KeyRange) : BOOLEAN
  RAISES {Error} =
  VAR
    p, q      : Page;
    pRef, qRef: PageRef;
    u         : Item;
    h,res     : BOOLEAN;
  BEGIN
    PK("\nInsertKey ", key);
    self.validateKey(keyNum);

    res := TRUE;
    pRef := self.info.rootRef[keyNum];
    IF pRef = -1 THEN
      (* empty tree create new root ref *)
      p := self.newPage(qRef);
      p.m := 1;
      p.p0 := 0;
      p.e[0].key := key;
      p.e[0].dp := dRef;
      (* assume all page refs are 0 on alloc *)
      pRef := qRef;
      self.infoDirty := TRUE;
      self.savePage(qRef, p);
    ELSE
      res := self.insert(key, -1, pRef, dRef, -1, h, u);
      IF NOT res THEN RETURN res; END;
      IF h THEN
        (* tree has grown *)
        q := self.getPage(pRef);
        IF q.m = 0 THEN
          p := q;
          qRef := pRef;
        ELSE
          p := self.newPage(qRef);
        END;
        p.m := 1;
        p.p0 := pRef;
        p.e[0] := u;
        pRef := qRef;            (* got a new rootRef *)
        self.infoDirty := TRUE;
        self.savePage(pRef, p);
      END;
    END;
    self.info.rootRef[keyNum] := pRef;
    self.flushPages();
    self.flushInfo();
    RETURN res;
  END InsertKey;

PROCEDURE Underflow
  (self: T; cRef, aRef: PageRef; s: INTEGER; VAR h: BOOLEAN) =
  (* a = underflowing page, c = ancestor page, s = index of deleted entry
     in c *)
  VAR
    a, b, c: Page;
    bRef   : PageRef;
    k      : INTEGER;
  BEGIN                      (* h and (a.m = N-1) and (c.e[s-1].p = a) *)
    D("\nUnderflow aref ", aRef);
    D("\nUnderflow cref ", cRef);
    <* ASSERT h *>

    c := self.getPage(cRef);
    a := self.getPage(aRef);

    <* ASSERT a.m = N - 1 *>
    IF s > 0 THEN <* ASSERT c.e[s - 1].p = aRef *> END;

    IF s < c.m THEN              (*b := page to the right of a*)
      bRef := c.e[s].p;
      b := self.getPage(bRef);

      k := (b.m - N + 1) DIV 2;  (*k = no of items available on page b*)
      a.e[N - 1] := c.e[s];
      a.e[N - 1].p := b.p0;
      IF k > 0 THEN              (*balance by moving k-1 items from b to
                                    a*)
        FOR i := 0 TO k - 2 DO a.e[i + N] := b.e[i] END;
        c.e[s] := b.e[k - 1];
        b.p0 := c.e[s].p;
        c.e[s].p := bRef;
        DEC(b.m, k);
        FOR i := 0 TO b.m - 1 DO b.e[i] := b.e[i + k] END;
        a.m := N - 1 + k;
        h := FALSE;
        self.savePage(bRef, b);
        self.savePage(cRef, c);
      ELSE                       (*merge pages a and b, discard b*)
        FOR i := 0 TO N - 1 DO a.e[i + N] := b.e[i] END;
        DEC(c.m);
        FOR i := s TO c.m - 1 DO c.e[i] := c.e[i + 1] END;
        a.m := PageSize;
        h := c.m < N;
        self.savePage(cRef, c);
        self.delPage(bRef);
        b := NIL;
      END;
      self.savePage(aRef, a);
    ELSE                         (*b := page to the left of a*)
      <* ASSERT s = c.m *>

      DEC(s);
      IF s = 0 THEN bRef := c.p0 ELSE bRef := c.e[s - 1].p END;
      b := self.getPage(bRef);
      k := (b.m - N + 1) DIV 2;  (*k = no of items available on page b*)
      IF k > 0 THEN
        FOR i := N - 2 TO 0 BY -1 DO a.e[i + k] := a.e[i]; END;
        a.e[k - 1] := c.e[s];
        a.e[k - 1].p := a.p0;
        (*move k-1 items from b to a, one to c*)
        DEC(b.m, k);
        FOR i := k - 2 TO 0 BY -1 DO a.e[i] := b.e[i + b.m + 1] END;
        c.e[s] := b.e[b.m];
        a.p0 := c.e[s].p;
        c.e[s].p := aRef;
        a.m := N - 1 + k;
        h := FALSE;
        self.savePage(aRef, a);
        self.savePage(bRef, b);
        self.savePage(cRef, c);
      ELSE                       (*merge pages a and b, discard a*)
        c.e[s].p := a.p0;
        b.e[N] := c.e[s];
        FOR i := 0 TO N - 2 DO b.e[i + N + 1] := a.e[i] END;
        b.m := PageSize;
        DEC(c.m);
        h := c.m < N;
        self.savePage(bRef, b);
        self.savePage(cRef, c);
        self.delPage(aRef);
        a := NIL;
      END;
    END;
  END Underflow;

PROCEDURE Delete
  (self: T; READONLY x: KeyType; aRef: PageRef; VAR h: BOOLEAN) : BOOLEAN =
  (* search and delete key x in B-tree a; if a page underflow arises,
     balance with adjacent page or merge; h := "page a is undersize"*)
  VAR
    m, l, r: INTEGER;
    a      : Page;
    qRef   : PageRef;
    res    : BOOLEAN := TRUE;

  PROCEDURE Del (pRef: PageRef; VAR h: BOOLEAN) =
    VAR
      k   : INTEGER;
      p   : Page;                (* global a, r *)
      qRef: PageRef;
    BEGIN
      p := self.getPage(pRef);
      k := p.m - 1;
      qRef := p.e[k].p;
      IF qRef # 0 THEN
        Del(qRef, h);
        IF h THEN self.underflow(pRef, qRef, p.m, h) END;
      ELSE
        p.e[k].p := a.e[r].p;
        a.e[r] := p.e[k];
        DEC(p.m);
        h := p.m < N;
        self.savePage(pRef, p);
        self.savePage(aRef, a);
      END;
    END Del;

  BEGIN
    D("\nDelete - aref ", aRef);
    IF aRef = 0 THEN
      (* key not found *)
      PK("\nDelete - key not found ", x);
      RETURN FALSE;
    END;

    a := self.getPage(aRef);

    IF a # NIL THEN
      l := 0;
      r := a.m;                  (*binary search*)
      WHILE l < r DO
        m := l + (r - l) DIV 2;
        IF Comp(x, a.e[m].key) THEN r := m ELSE l := m + 1 END
      END;
      IF r = 0 THEN qRef := a.p0; ELSE qRef := a.e[r - 1].p; END;
      IF r < a.m AND a.e[r].key = x THEN (*found*)
        IF qRef = 0 THEN         (*a is leaf page*)
          DEC(a.m);
          h := a.m < N;
          FOR i := r TO a.m - 1 DO a.e[i] := a.e[i + 1] END;
          self.savePage(aRef, a);
        ELSE
          Del(qRef, h);
          IF h THEN self.underflow(aRef, qRef, r, h) END;
        END;
      ELSE
        res := self.delete(x, qRef, h);
        IF h THEN self.underflow(aRef, qRef, r, h) END;
      END;
    END;
    RETURN res;
  END Delete;

PROCEDURE DeleteKey (self: T; READONLY k: KeyType; keyNum: KeyRange) : BOOLEAN
  RAISES {Error} =
  VAR
    root, pRef: PageRef;
    p         : Page;
    h,res     : BOOLEAN;
  BEGIN
    PK("Delete Key ", k);
    self.validateKey(keyNum);

    res := TRUE;
    root := self.info.rootRef[keyNum];
    IF root = -1 THEN
      (* empty tree *)
      RETURN FALSE;
    END;
    res := self.delete(k, root, h);
    IF NOT res THEN RETURN res; END;

    p := self.getPage(root);
    IF h AND p.m = 0 THEN
      pRef := root;
      root := p.p0;
      self.delPage(pRef);
      p := NIL;
      IF root > 0 THEN
        self.info.rootRef[keyNum] := root;
      ELSE
        (* deleted all keys *)
        self.info.rootRef[keyNum] := -1;
      END;
      self.infoDirty := TRUE;
    END;
    self.flushPages();
    self.flushInfo();
    RETURN res;
  END DeleteKey;

(* primitive print tree. not very readable *)
PROCEDURE PrintTree (self: T; pRef: PageRef; level: INTEGER) =
  VAR
    p  : Page;
    t  : TEXT;
    len: INTEGER;
  BEGIN
    IF pRef > 0 THEN
      p := self.getPage(pRef);

      FOR i := 1 TO level DO IO.Put("  "); END;
      FOR i := 0 TO p.m - 1 DO
        (* only create a text which is length of the string without
           trailing 0 *)
        FOR j := 0 TO MaxKeyLen - 1 DO
          IF p.e[i].key[j] = '\000' THEN len := j; EXIT; END;
        END;
        t := Text.FromChars(SUBARRAY(p.e[i].key, 0, len));
        IO.Put(t);
        IO.Put("("); IO.PutInt(p.e[i].dp);IO.Put(")");
        IO.Put("  ");            (* fmt to 4 wide *)
        INC(self.numNodes);
      END;
      IO.Put("\n");
      self.printTree(p.p0, level + 1);
      FOR i := 0 TO p.m - 1 DO
        self.printTree(p.e[i].p, level + 1);
      END;
    END;
  END PrintTree;

PROCEDURE Print (self: T; keyNum: KeyRange) RAISES {Error} =
  BEGIN
    D("Print Tree\n", 1);
    self.numNodes := 0;
    self.validateKey(keyNum);
    self.printTree(self.info.rootRef[keyNum],1);
    IO.Put("Num nodes ");
    IO.PutInt(self.numNodes);
    IO.Put("\n");
  END Print;

PROCEDURE DumpInfo (self: T) =
  BEGIN
    IO.Put("File Block Info\n");
    IO.Put("PageSize " & Fmt.Int(PageSize) & "\n");
    IO.Put("PageRecSize " & Fmt.Int(BYTESIZE(PageRec)) & "\n");
    IO.Put("ItemSize " & Fmt.Int(BYTESIZE(Item)) & "\n");
    IO.Put("Root refs ");
    FOR i := 1 TO MaxKeys DO
      IO.PutInt(self.info.rootRef[i]);
      IO.Put(" ");
    END;
    IO.Put("\n");
    IO.Put("Num Keys " & Fmt.Int(self.info.numKeys) & "\n");
    IO.Put("Page count " & Fmt.Int(self.info.pageCount) & "\n");
    IO.Put("Block Length " & Fmt.Int(self.info.blockLen) & "\n");
    IO.Put("Free Pages " & Fmt.Int(self.info.numFreePages) & "\n");
    IO.Put("First Free Page " & Fmt.Int(self.info.firstFreePage) & "\n");
    IO.Put("Free Recs " & Fmt.Int(self.info.numFreeRecs) & "\n");
    IO.Put("First Free Rec " & Fmt.Int(self.info.firstFreeRec) & "\n");
  END DumpInfo;

PROCEDURE PrintInfo(self: T) =
  BEGIN
    self.dumpInfo();
  END PrintInfo;

PROCEDURE Init (self: T; fname: TEXT; create: BOOLEAN := FALSE) =
  VAR idxName, datName: TEXT;
  BEGIN
    idxName := fname & idxExt;
    datName := fname & datExt;
    IF create THEN
      self.idx := IsamIO.Create(idxName);
      self.dat := IsamIO.Create(datName);
    ELSE
      self.idx := IsamIO.Open(idxName);
      self.dat := IsamIO.Open(datName);
    END;
    self.firstFreeRec := NEW(REF INTEGER);
    self.firstFreePage := NEW(REF INTEGER);
    self.cache := NEW(Cache).init(use := TRUE);
    self.info := NEW(InfoRef);
    self.infoDirty := FALSE;
    self.opened := TRUE;
  END Init;

(* create the file if not exist, otherwise truncate it and write the info
   rec *)
PROCEDURE Create
  (self: T; fname: TEXT; numKeys: KeyRange; blockLen: CARDINAL): T =
  BEGIN
    D("\nCreate file " & fname & "\n\n", 0);
    Init(self, fname, create := TRUE);
    (* initialise info *)
    self.info.pageCount := 0;
    self.info.blockLen := blockLen;
    self.info.numKeys := numKeys;
    self.info.firstFreeRec := -1;
    self.info.firstFreePage := -1;
    self.info.numFreePages := 0;
    self.info.numFreeRecs := 0;
    FOR i := 1 TO MaxKeys DO self.info.rootRef[i] := -1; END;
    self.infoDirty := TRUE;
    self.flushInfo();
    RETURN self;
  END Create;

(* Open file and read info rec *)
PROCEDURE Open (self: T; fname: TEXT): T RAISES {Error} =
  BEGIN
    D("\nOpen file " & fname & "\n\n", 0);
    IF NOT (IsamIO.FileExists(fname & idxExt)
              AND IsamIO.FileExists(fname & datExt)) THEN
      RAISE Error("file " & fname & " does not exist");
    END;
    Init(self, fname);
    TRY
      IsamIO.GetBlock(self.idx, 0, BYTESIZE(Info), self.info);
    EXCEPT
    | IsamIO.Eof => RAISE Error("Eof reading info");
    END;
    RETURN self;
  END Open;

PROCEDURE Close (self: T) =
  BEGIN
    IF NOT self.opened THEN RETURN; END;
    self.infoDirty := TRUE;
    self.flushPages();
    self.flushInfo();
    self.opened := FALSE;
    IsamIO.Close(self.idx);
  END Close;

PROCEDURE DeleteFile (self: T) =
  BEGIN
    IsamIO.Delete(self.idx);
  END DeleteFile;

(* create a sorted table of the free blocks to be used in iterating
   over the entire data file *)
PROCEDURE FreeRecs (self: T) =
  VAR
    fr : REF INTEGER;
    dRef : DataRef;
  BEGIN
    self.free := NEW(SortedIntIntTbl.Default).init();
    fr := NEW(REF INTEGER);
    fr^ := self.info.firstFreeRec;
    IF fr^ > -1 THEN
      REPEAT
        dRef := fr^;
        EVAL self.free.put(dRef,dRef);
        TRY
          IsamIO.GetBlock(self.dat, dRef, BYTESIZE(INTEGER), fr);
        EXCEPT
        | IsamIO.Eof => <*ASSERT FALSE, "Eof in freerecs" *>
        END;
      UNTIL fr^ = -1;
    END;
  END FreeRecs;

PROCEDURE Iterate(self : T) : Iterator =
  VAR 
    iter : DefaultIterator;
    s : SortedIntIntTbl.Iterator;
    xx : INTEGER;
  BEGIN
    self.freeRecs();
    s := self.free.iterateOrdered(TRUE);
    iter := NEW(DefaultIterator, tree := self, freeIter := s);
    EVAL iter.freeIter.next(iter.free,xx);
    RETURN iter;
  END Iterate;

PROCEDURE Next(i : DefaultIterator; data : REFANY) : BOOLEAN =
  VAR xx : INTEGER;
  BEGIN
    TRY
      IF i.ofs = i.free THEN
        REPEAT
          INC(i.ofs,i.tree.info.blockLen);
          EVAL i.freeIter.next(i.free,xx);
        UNTIL i.ofs # i.free;
      END;
      IsamIO.GetBlock(i.tree.dat, i.ofs, i.tree.info.blockLen, data);
      INC(i.ofs,i.tree.info.blockLen);
    EXCEPT
    | IsamIO.Eof => RETURN FALSE
    END;
    RETURN TRUE;
  END Next;

BEGIN
END BTree.
