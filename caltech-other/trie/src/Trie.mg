(* $Id$ *)

GENERIC MODULE Trie(Key, Value);

TYPE Data = RECORD value : Value.T; next : INTEGER := 0 END;
TYPE Tab = ARRAY Key.T OF Data;
TYPE TabArr = REF ARRAY OF Tab;

REVEAL
  T = Public BRANDED Brand OBJECT
    tab : TabArr;
    defValue : Value.T;
    last : CARDINAL := 0;
  OVERRIDES
    get := Get;
    put := Put;
    init := Init;
    iterate := Iterate;
  END;

PROCEDURE Init(t : T; READONLY defValue : Value.T) : T =
  BEGIN
    t.defValue := defValue;
    t.tab := NEW(TabArr,1);
    t.last := 0;
    FOR i := FIRST(t.tab[0]) TO LAST(t.tab[0]) DO
      t.tab[0][i] := Data { t.defValue, 0 }
    END;
    RETURN t
  END Init;

PROCEDURE Get(t : T; READONLY k : ARRAY OF Key.T) : Value.T =
  VAR
    p := 0;
  BEGIN
    FOR i := FIRST(k) TO LAST(k) DO
      WITH next = t.tab[p][k[i]].next DO
        IF next = 0 THEN 
          RETURN t.defValue 
        ELSE
          p := next
        END
      END
    END;
    RETURN t.tab[p][k[LAST(k)]].value
  END Get;

PROCEDURE Expand(VAR tab : TabArr) =
  VAR
    n := 3 * NUMBER(tab^) DIV 2 + 1;
    new := NEW(TabArr, n);
  BEGIN
    SUBARRAY(new^,0,NUMBER(tab^)) := tab^;
    tab := new
  END Expand;

PROCEDURE Put(t : T; READONLY k : ARRAY OF Key.T; 
              READONLY v : Value.T) : Value.T =
  VAR
    p := 0;
  BEGIN
    FOR i := FIRST(k) TO LAST(k) DO
      IF t.tab[p][k[i]].next = 0 THEN 
        (* extend trie *)
        INC(t.last);
        IF t.last > LAST(t.tab^) THEN Expand(t.tab) END;
        
        t.tab[p][k[i]].next := t.last;
        FOR i := FIRST(Key.T) TO LAST(Key.T) DO
          t.tab[t.last][i] := Data { t.defValue, 0 }
        END
      END;
      p := t.tab[p][k[i]].next
    END;
    TRY
      RETURN t.tab[p][k[LAST(k)]].value
    FINALLY
      t.tab[p][k[LAST(k)]].value := v
    END
  END Put;

PROCEDURE Iterate(t : T) : Iterator =
  BEGIN RETURN NEW(Iterator, t := t) END Iterate;

REVEAL
  Iterator = PublicIterator BRANDED Brand & "Iterator" OBJECT
    t : T;
    p : INTEGER := 0;
    k := FIRST(Key.T);
  OVERRIDES
    next := INext;
  END;

PROCEDURE INext(i : Iterator; VAR v : Value.T) : BOOLEAN =

  PROCEDURE Inc() =
    BEGIN
      IF i.k = LAST(Key.T) THEN
        i.k := FIRST(Key.T); INC(i.p)
      ELSE
        INC(i.k)
      END
    END Inc;

  BEGIN
    LOOP
      IF i.p > i.t.last THEN 
        RETURN FALSE
      ELSIF i.t.tab[i.p][i.k].value # i.t.defValue THEN 
        v := i.t.tab[i.p][i.k].value; Inc(); RETURN TRUE
      ELSE
        Inc()
      END
    END
  END INext;

BEGIN END Trie.
