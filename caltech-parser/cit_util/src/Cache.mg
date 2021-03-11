(* $Id$ *)

GENERIC MODULE Cache(Key, Value, KeyRefTbl);
IMPORT Thread;

TYPE
  LRU = OBJECT
    which : Key.T;
    prev : LRU;
    next : LRU;
  END;

  S = REF RECORD
    lru : LRU;
    value : Value.T;
  END;

  KeyList = OBJECT
    key : Key.T;
    next : KeyList;
  END;

REVEAL
  T = Public BRANDED Brand OBJECT
    maxCache : CARDINAL;
    data : KeyRefTbl.T;
    lru : LRU;
    volatile : KeyList;
  OVERRIDES
    init := Init;
    get := Get;
    haveCachedData := HaveCachedData;
    purge := Purge;
    noCache := NoCache;
    flushMatching := FlushMatching;
  END;

PROCEDURE FlushMatching(t : T; key : Key.T) =
  VAR
    old : REFANY;
  BEGIN
    IF t.data.delete(key,old) THEN
      WITH rec = NARROW(old,S) DO
        (* update LRU *)
        VAR
          l := rec.lru;
        BEGIN
          (* delete l from its old position *)
          l.prev.next := l.next;
          l.next.prev := l.prev
        END
      END
    END
  END FlushMatching;

PROCEDURE Init(t : T; cacheSize : CARDINAL) : T =
  BEGIN
    t.maxCache := cacheSize;
    t.data := NEW(KeyRefTbl.Default).init(cacheSize);

    (* make sentinel *)
    t.lru := NEW(LRU);
    t.lru.prev := t.lru;
    t.lru.next := t.lru;

    t.volatile := NIL;

    RETURN t
  END Init;

PROCEDURE Purge(t : T) =
  BEGIN EVAL t.init(t.maxCache) END Purge;

PROCEDURE NoCache(t : T; idx : Key.T) =
  VAR 
    p := t.volatile;
  BEGIN
    WHILE p # NIL DO
      IF Key.Equal(idx,p.key) THEN RETURN END;
      p := p.next
    END;
    t.volatile := NEW(KeyList, key := idx, next := t.volatile)
  END NoCache;

PROCEDURE Get(t : T; idx : Key.T) : Value.T =
  VAR
    res : REFANY;
  BEGIN

    (* check if key value is volatile; if so, don't cache *)
    VAR
      p := t.volatile;
    BEGIN
      WHILE p # NIL DO
        IF Key.Equal(idx,p.key) THEN
          RETURN t.compute(idx, NEW(S).value) 
        END;
        p := p.next
      END
    END;       

    IF t.data.get(idx,res) THEN 
      LOCK mu DO
        hitRate := decay*1.0d0 + (1.0d0-decay) * hitRate
      END;
      WITH rec = NARROW(res,S) DO
        (* update LRU *)

        VAR
          l := rec.lru;
        BEGIN
          (* delete l from its old position *)
          l.prev.next := l.next;
          l.next.prev := l.prev;
          
          (* put it at head *)
          l.next := t.lru.next;
          l.prev := t.lru;
          t.lru.next.prev := l;
          t.lru.next := l;
        END;

        RETURN rec.value
      END
    ELSE
      LOCK mu DO
        hitRate := decay*0.0d0 + (1.0d0-decay) * hitRate
      END;
    END;

    (* figure out which one to evict, if any *)

    VAR
      l : LRU;
      x : BOOLEAN;
      s : S;
      maxCache : CARDINAL;
    BEGIN
      LOCK mu DO
        adjusted := TRUE;
        IF adaptive = -1 THEN
          maxCache := t.maxCache
        ELSE
          maxCache := maxSize
        END;
        IF t.data.size() >= maxCache THEN
          REPEAT
            (* evict record at tail *)
            l := t.lru.prev;
            t.lru.prev := l.prev;
            l.prev.next := t.lru;
            
            VAR r : REFANY; BEGIN x := t.data.delete(l.which,r); s:=r END;
            
            <* ASSERT x *>
          UNTIL t.data.size() < maxCache
        ELSE
          l := NEW(LRU);
          s := NEW(S);
        END
      END;
      
      s.value := t.compute(idx,s.value);
      EVAL t.data.put(idx, s);

      (* update l accordingly *)
      l.which := idx;
      
      (* insert l at head *)
      l.next := t.lru.next;
      l.prev := t.lru;
      
      l.prev.next := l;
      l.next.prev := l;
      
      (* make new record *)
      s.lru := l;
      
      RETURN s.value
    END
  END Get;

PROCEDURE HaveCachedData(t : T; idx : Key.T) : BOOLEAN =
  VAR dummy : REFANY; BEGIN RETURN t.data.get(idx,dummy) END HaveCachedData;

VAR
  adaptive : [-1..LAST(CARDINAL)] := -1;
  adaptiveMin : [1..LAST(CARDINAL)] := 1;
  maxSize : CARDINAL;
  mu := NEW(MUTEX);
  hitRate := 1.0d0;
  decay := 0.001d0;
  target : LONGREAL;
  pause := 1.0d0;
  running := FALSE;
  adjusted := TRUE;
  
PROCEDURE EnableAdaptiveCaching(maxSizeS : [1..LAST(CARDINAL)]; 
                                targetHitRate : LONGREAL;
                                startRatio : LONGREAL;
                                minSizeS : [1..LAST(CARDINAL)]) =
  BEGIN 
    <* ASSERT maxSizeS >= minSizeS *>
    LOCK mu DO
      IF NOT running THEN 
        running := TRUE;
        EVAL Thread.Fork(NEW(AdaptiveClosure))
      END;
      target := targetHitRate;
      adaptive := maxSizeS;
      adaptiveMin := minSizeS;
      maxSize := MAX(1,
                     ROUND(FLOAT(maxSizeS,LONGREAL)*startRatio))
    END
  END EnableAdaptiveCaching;

PROCEDURE DisableAdaptiveCaching() = 
  BEGIN 
    LOCK mu DO
      adaptive := -1 
    END
  END DisableAdaptiveCaching;

TYPE
  AdaptiveClosure = Thread.Closure OBJECT OVERRIDES
    apply := ACApply
  END;

PROCEDURE ACApply(<*UNUSED*>cl : AdaptiveClosure) : REFANY =
  BEGIN
    LOOP
      LOCK mu DO
        IF adjusted THEN
          IF hitRate >= target THEN
            maxSize := MAX(adaptiveMin,
                           MIN(maxSize-1,
                               ROUND(FLOAT(maxSize,LONGREAL)*0.99d0)))
          ELSE
            maxSize := MIN(adaptive,
                           MAX(maxSize+1, 3*maxSize DIV 2))
          END;
          adjusted := FALSE
        END
      END;
      Thread.Pause(pause)
    END
  END ACApply;
    
BEGIN 
END Cache.



